/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(MMdevel_drj_arena_hysteresis.11) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 *
 * PURPOSE
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.  This module
 * implements an Arena Class: The VM Arena Class.
 *
 * DESIGN
 *
 * See design.mps.arena.vm, and design.mps.arena.coop-vm:
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.zone-shift: The VM arena pokes around with arena->zoneShift.
 * In fact, the arena implementation really owns this field.
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC. (but it would change how VMFindFreeInRefSet avoids allocating
 * over the tables, see .alloc.skip)@@@@
 */


#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(MMdevel_drj_arena_hysteresis.11) $");


/* @@@@ Arbitrary calculation for the maximum number of distinct */
/* object sets for generations. */
/* .gencount.const: Must be a constant suitable for use as an */
/* array size. */
#define VMArenaGenCount ((Count)(MPS_WORD_WIDTH/2))

/* Structure Pointer types */
typedef struct PageStruct *Page;
typedef struct VMArenaChunkCacheEntryStruct *VMArenaChunkCacheEntry;
typedef struct VMArenaChunkStruct *VMArenaChunk;
typedef struct VMArenaStruct *VMArena;


/* PageStruct -- page structure
 *
 * .page-table: The page table (defined as a PageStruct array)
 * is central to the design of the arena.
 * See design.mps.arena.vm:table.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union, so that it shares a common prefix with the
 * SegStruct.  See design.mps.seg.field.pool.
 *
 * Pages (hence PageStructs that describe them) can be in one of
 * 4 states:
 * allocated to initial page of segment.
 *   allocated pages are mapped
 *   BTGet(allocTable, i) == 1
 *   PageRest()->pool == pool
 * allocated to non-initial page of segment.
 *   allocated pages are mapped
 *   BTGet(allocTable, i) == 1
 *   PageRest()->pool == NULL
 *   PageRest()->type == PageTypeTail
 * latent (free and in hysteresis fund).
 *   these pages are mapped
 *   BTGet(allocTable, i) == 0
 *   PageRest()->pool == NULL
 *   PageRest()->type == PageTypeLatent
 * free and not in the hysteresis fund.
 *   these pages are not mapped
 *   BTGet(allocTable, i) == 0
 *   PTE may itself be unmapped, but when it is (use pageTableMapped
 *     to determine whether page occupied by page table is mapped):
 *   PageRest()->pool == NULL
 *   PageRest()->type == PageTypeFree
 * 
 */

/* .page.disc: PageStruct disciminator values, */
/* see .page.is below */
enum {PageTypeTail=1, PageTypeLatent, PageTypeFree};

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct segStruct;        /* segment */
    struct {
      Pool pool;                /* NULL, must be first field (.page) */
      int type;                 /* discriminator, see .page.disc */
      union {
        struct {                /* use tail iff type == PageTypeTail */
	  Seg seg;              /* segment at base page of run */
	  Addr limit;           /* limit of segment */
	} tail;
	struct {                /* use latent iff type == PageTypeLatent */
	  RingStruct arenaRing;
	} latent;
      } the;
    } rest;                     /* other (non initial segment) page */
  } the;
} PageStruct;


/* VMArenaChunkStruct */

#define VMArenaChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMArenaChunkStruct {
  Sig sig;                      /* design.mps.sig */
  VM vm;                        /* virtual memory handle */
  Size pageSize;                /* size of pages */
  Shift pageShift;              /* log2 of page size, for shifts */
  VMArena vmArena;              /* parent VMarena */
  RingStruct arenaRing;         /* ring of all chunks in arena */
  Bool primary;                 /* primary chunk contains vmArena */
  Bool inBoot;                  /* TRUE in boot (used in checking) */
  Addr base;                    /* base address of chunk */
  Addr limit;                   /* limit address of chunk */
  Count pages;                  /* number of pages in chunk */
  Page pageTable;               /* the page table */
  Count pageTablePages;         /* number of pages occupied by page table */
  BT pageTableMapped;           /* indicates mapped state of page table */
  BT noLatentPages;             /* 1 bit per page of pageTable */
  BT allocTable;                /* page allocation table */
  Size ullageSize;              /* size unusable for segments */
  Count ullagePages;            /* number of pages occupied by ullage */
} VMArenaChunkStruct;


/* addrPageBase -- the base of the page this address is on */

#define addrPageBase(chunk, addr) \
  AddrAlignDown((addr), (chunk)->pageSize)

/* Address of the page descriptor (as an Addr) */

#define addrOfPageDesc(chunk, index) \
  ((Addr)&(chunk)->pageTable[index])


/* VMArenaChunkCacheEntryStruct */
/* SIGnature Arena VM Chunk Cache */
#define VMArenaChunkCacheEntrySig       ((Sig)0x519AF3CC) 
typedef struct VMArenaChunkCacheEntryStruct {
  Sig sig;
  VMArenaChunk chunk;
  Addr base;
  Addr limit;
  Page pageTableBase;
  Page pageTableLimit;
} VMArenaChunkCacheEntryStruct;


/* VMArenaStruct
 * See design.mps.arena.coop-vm:struct.vmarena for description.
 */

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */
typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VMArenaChunk primary;
  RingStruct chunkRing;
  VMArenaChunkCacheEntryStruct chunkCache; /* just one entry */
  RingStruct latentRing;	/* ring of all latent pages */
  Size latentSize;              /* total size of latent pages */
  Size committed;               /* amount of committed RAM */
  RefSet blacklist;             /* zones to use last */
  RefSet genRefSet[VMArenaGenCount]; /* .gencount.const */
  RefSet freeSet;               /* unassigned zones */
  Size extendBy;
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;

/* Forward declarations */
static void VMArenaPageFree(VMArenaChunk chunk, Index pi);
static void VMArenaPageInit(VMArenaChunk chunk, Index pi);
static Addr VMSegLimit(Seg seg);
static void VMArenaPurgeLatentPages(VMArena vmArena);


static Bool VMArenaChunkCheck(VMArenaChunk chunk)
{
  CHECKS(VMArenaChunk, chunk);
  CHECKL(VMCheck(chunk->vm));
  CHECKL(VMAlign(chunk->vm) == chunk->pageSize);
  CHECKL(ShiftCheck(chunk->pageShift));
  CHECKL(BoolCheck(chunk->primary));
  CHECKL(BoolCheck(chunk->inBoot));
  /* inBoot => primary */
  CHECKL(!chunk->inBoot || chunk->primary);
  if(!chunk->inBoot) {
    CHECKU(VMArena, chunk->vmArena);
    CHECKL(chunk->primary == (chunk == chunk->vmArena->primary));
  }
  CHECKL(RingCheck(&chunk->arenaRing));
  CHECKL(1uL << chunk->pageShift == chunk->pageSize);
  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check chunk is in itself */
  CHECKL(chunk->base <= (Addr)chunk);
  CHECKL((Addr)(chunk+1) <= chunk->limit);
  /* check that the tables fit in the chunk */
  CHECKL(chunk->ullagePages <= chunk->pages);
  /* check that the two notions of ullage size are consistent */
  CHECKL(chunk->ullageSize == chunk->ullagePages << chunk->pageShift);
  CHECKL(chunk->pageTable != NULL);
  /* check that pageTable is in the chunk ... */
  CHECKL((Addr)chunk->pageTable >= chunk->base);
  CHECKL(addrOfPageDesc(chunk, chunk->pageTablePages) <=
         AddrAdd(chunk->base, chunk->ullageSize));
  /* check pageTableMapped table */
  CHECKL(chunk->pageTableMapped != NULL);
  CHECKL((Addr)chunk->pageTableMapped >= chunk->base);
  CHECKL(AddrAdd((Addr)chunk->pageTableMapped,
                 BTSize(chunk->pageTablePages)) <=
         AddrAdd(chunk->base, chunk->ullageSize));
  /* check noLatentPages table */
  CHECKL(chunk->noLatentPages != NULL);
  CHECKL((Addr)chunk->noLatentPages >= chunk->base);
  CHECKL(AddrAdd((Addr)chunk->noLatentPages,
                 BTSize(chunk->pageTablePages)) <=
         AddrAdd(chunk->base, chunk->ullageSize));
  /* check allocTable */
  CHECKL(chunk->allocTable != NULL);
  CHECKL((Addr)chunk->allocTable >= chunk->base);
  CHECKL(AddrAdd((Addr)chunk->allocTable, BTSize(chunk->pages)) <= 
         AddrAdd(chunk->base, chunk->ullageSize));
  /* .improve.check-table: Could check the consistency of the tables. */
  CHECKL(chunk->pages == 
         AddrOffset(chunk->base, chunk->limit) >> 
         chunk->pageShift);
  return TRUE;
}


/* ArenaVMArena -- find the VMArena pointer given a generic Arena */

#define ArenaVMArena(arena) PARENT(VMArenaStruct, arenaStruct, (arena))


/* VMArenaArena -- find the generic Arena pointer given a VMArena */
/* .arena.check-free: Must not call (indirectly) VMArenaCheck */

#define VMArenaArena(VMArena) (&(VMArena)->arenaStruct)


/* SegVMArena -- find the VMArena given a segment */

#define SegVMArena(seg) ArenaVMArena(PoolArena(SegPool(seg)))


/* indexOfAddr -- return the index of the page containing an address
 *
 * .index.addr: The address passed may be equal to the limit of the
 * arena, in which case the last page index plus one is returned.  (It
 * is, in a sense, the limit index of the page table.)
 */

#define INDEX_OF_ADDR(chunk, addr) \
  (AddrOffset((chunk)->base, (addr)) >> (chunk)->pageShift)
static Index indexOfAddr(VMArenaChunk chunk, Addr addr)
{
  AVERT(VMArenaChunk, chunk);
  AVER(chunk->base <= addr);
  AVER(addr <= chunk->limit);   /* .index.addr */

  return INDEX_OF_ADDR(chunk, addr);
}


/* PageIndexBase -- map page index to base address of page
 *
 * See design.mps.arena.vm:table.linear
 */

#define PageIndexBase(chunk, i) \
  AddrAdd((chunk)->base, ((i) << (chunk)->pageShift))


/* PageTablePageIndex
 *
 * Maps from a page base address for a page occupied by the page table
 * to the index of that page in the range of pages occupied by the
 * page table.
 * So that PageTablePageIndex(chunk, (Addr)chunk->pageTable) == 0
 * and PageTablePageIndex(chunk,
 *   AddrAlignUp((Addr)&chunk->pageTable[chunk->pages], chunk->pageSize) ==
 *   chunk->pageTablePages
 */
#define PageTablePageIndex(chunk, pageAddr) \
  (AddrOffset((Addr)(chunk)->pageTable, pageAddr) >> (chunk)->pageShift)

/* TablePageIndexBase
 *
 * Takes a page table page index (ie the index of a page occupied
 * by the page table, where the page occupied by chunk->pageTable is
 * index 0) and returns the base address of that page.
 *
 * (reverse of mapping defined by PageTablePageIndex)
 */
#define TablePageIndexBase(chunk, index) \
  (AddrAdd((Addr)(chunk)->pageTable, ((index) << (chunk)->pageShift)))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)

/* PageRest -- descriptor for non initial segment pages */

#define PageRest(page)          (&(page)->the.rest)


/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&PageRest((page))->the.tail)


/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, (seg))


/* PageIsHead -- is a page a head (contains segment descriptor)?
 *
 * See design.mps.arena.vm:table.disc.
 */

#define PageIsHead(page)        (PageRest((page))->pool != NULL)

/* PageIsLatent -- is page latent (free and mapped)?
 *
 * @@@@ Uses argument multiple times. */
#define PageIsLatent(page)	(PageRest((page))->pool == NULL && \
                                 PageRest((page))->type == PageTypeLatent)


static Bool VMArenaChunkCacheEntryCheck(VMArenaChunkCacheEntry entry)
{
  CHECKS(VMArenaChunkCacheEntry, entry);
  CHECKD(VMArenaChunk, entry->chunk);
  CHECKL(entry->base == entry->chunk->base);
  CHECKL(entry->limit == entry->chunk->limit);
  CHECKL(entry->pageTableBase == &entry->chunk->pageTable[0]);
  CHECKL(entry->pageTableLimit ==
    &entry->chunk->pageTable[entry->chunk->pages]);

  return TRUE;
}


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  Index gen;
  RefSet allocSet;

  CHECKS(VMArena, vmArena);
  CHECKD(Arena, VMArenaArena(vmArena)); /* .arena.check-free */
  CHECKD(VMArenaChunk, vmArena->primary);
  CHECKD(VMArenaChunkCacheEntry, &vmArena->chunkCache);
  CHECKL(RingCheck(&vmArena->chunkRing));
  CHECKL(RingCheck(&vmArena->latentRing));
  /* latent pages are committed, so must be less latent than committed */
  CHECKL(vmArena->latentSize <= vmArena->committed);
  /* we could iterate over all chunks accumulating an accurate */
  /* count of committed, but we don't have all day */
  CHECKL(VMMapped(vmArena->primary->vm) <= vmArena->committed);
  CHECKL(RefSetCheck(vmArena->blacklist));

  allocSet = RefSetEMPTY;
  for(gen = (Index)0; gen < VMArenaGenCount; ++gen) {
    CHECKL(RefSetCheck(vmArena->genRefSet[gen]));
    allocSet = RefSetUnion(allocSet, vmArena->genRefSet[gen]);
  }
  CHECKL(RefSetCheck(vmArena->freeSet));
  CHECKL(RefSetInter(allocSet, vmArena->freeSet) == RefSetEMPTY);
  CHECKL(vmArena->extendBy > 0);

  return TRUE;
}

/* Chunk Cache
 *
 * Functions for manipulating the chunk cache in the VM arena.
 */

static void VMArenaChunkCacheEntryInit(VMArenaChunkCacheEntry entry)
{
  static VMArenaChunkCacheEntryStruct initEntryStruct =
    {VMArenaChunkCacheEntrySig, 0};

  *entry = initEntryStruct;

  return;
}

static void VMArenaChunkEncache(VMArena vmArena, VMArenaChunk chunk)
{
  /* static function called internally, hence no checking */

  /* check chunk already in cache first */
  if(vmArena->chunkCache.chunk == chunk) {
    return;
  }

  vmArena->chunkCache.chunk = chunk;
  vmArena->chunkCache.base = chunk->base;
  vmArena->chunkCache.limit = chunk->limit;
  vmArena->chunkCache.pageTableBase = &chunk->pageTable[0];
  vmArena->chunkCache.pageTableLimit = &chunk->pageTable[chunk->pages];

  AVERT(VMArenaChunkCacheEntry, &vmArena->chunkCache);

  return;
}


/* VM indirect functions
 *
 * These functions should be the only ones in this module that call
 * VMMap or VMUnmap directly (there is an annoying exception to this
 * during boot).  These functions are responsible for
 * maintaining * vmArena->committed, and for checking that the
 * commit limit does not get exceeded.
 */

static Res VMArenaMap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Arena arena;
  Size size;
  Res res;

  /* no checking as function is local to module */

  arena = &vmArena->arenaStruct;
  size = AddrOffset(base, limit);
  /* committed can't overflow (since we can't commit more memory than */
  /* address space), but we're paranoid. */
  AVER(vmArena->committed < vmArena->committed + size);
  /* check against commit limit */
  if(arena->commitLimit < vmArena->committed + size)
    return ResCOMMIT_LIMIT;
  
  res = VMMap(vm, base, limit);
  if(res != ResOK)
    return res;
    
  vmArena->committed += size;
  return ResOK;
}

static void VMArenaUnmap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Size size;

  /* no checking as function is local to module */

  size = AddrOffset(base, limit);

  VMUnmap(vm, base, limit);
  AVER(size <= vmArena->committed);
  vmArena->committed -= size;

  return;
}


/* Boot Allocator
 *
 * A structure and associated protocols for allocating memory
 * during the boot sequence.
 *
 * .boot.c: The Boot Allocator is used to allocate C structures
 * for use in this implementation, not client objects.  Therefore
 * we use "C types" (void *, size_t) not "client types" (Addr, Size).
 *
 * .improve.module: Split into separate module? @@@@
 */

#define VMArenaBootSig ((Sig)0x519A6B3B) /* SIGnature ARena VM Boot */

typedef struct VMArenaBootStruct
{
  Sig sig;
  void *base;
  void *alloc;
  void *limit;
} VMArenaBootStruct;
typedef VMArenaBootStruct *VMArenaBoot;

static Bool VMArenaBootCheck(VMArenaBoot boot)
{
  CHECKS(VMArenaBoot, boot);
  CHECKL(boot->base != NULL);
  CHECKL(boot->alloc != NULL);
  CHECKL(boot->limit != NULL);
  CHECKL(boot->base <= boot->alloc);
  CHECKL(boot->alloc <= boot->limit);
  CHECKL(boot->alloc < boot->limit);

  return TRUE;
}

/* VMArenaBootInit 
 *
 * Initializes the BootStruct for later use with BootAlloc.
 *
 * .bootinit.arg.boot: a pointer to the structure to be initialized
 *   (must have been allocated by the caller, probably on the stack).
 * .bootinit.arg.base: a pointer to the base of the memory to be
 *   allocated from (the memory need not be committed) (see .boot.c).
 * .bootinit.arg.limit: a pointer to the limit of the memory to be
 *   allocated from (this is used to check for over-allocation) (see
 *   .boot.c).
 */
static Res VMArenaBootInit(VMArenaBootStruct *boot,
                           void *base, void *limit)
{
  /* Can't check boot as we are supposed to be initializing it */
  AVER(boot != NULL);
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  boot->base = base;
  boot->alloc = base;
  boot->limit = limit;
  boot->sig = VMArenaBootSig;

  AVERT(VMArenaBoot, boot);
  return ResOK;
}

static Res VMArenaBootFinish(VMArenaBoot boot)
{
  AVERT(VMArenaBoot, boot);

  boot->base = boot->alloc = boot->limit = NULL;
  boot->sig = SigInvalid;

  return ResOK;
}


/* VMArenaBootAllocated
 *
 * Returns the total amount allocated using this descriptor
 */
static size_t VMArenaBootAllocated(VMArenaBoot boot)
{
  AVERT(VMArenaBoot, boot);

  return PointerOffset(boot->base, boot->alloc);
}

/* VMArenaBootAlloc
 *
 * A simple allocator used in the Chunk and Arena boot sequences.
 * (It basically just increments a single pointer)
 *
 * Arguments
 *
 * .bootalloc.arg.preturn: The returned pointer, see .boot.c.
 * .bootalloc.arg.boot: must have been initialized with
 * VMArenaBootInit()
 * .bootalloc.arg.size: size of requested object, see .boot.c.
 * .bootalloc.arg.align: required alignment of object, see .boot.c.
 */
static Res VMArenaBootAlloc(void **pReturn, VMArenaBoot boot,
                            size_t size, size_t align)
{
  void *blockBase, *blockLimit;  /* base, limit of candidate block */

  AVER(pReturn != NULL);
  AVERT(VMArenaBoot, boot);
  AVER(size > 0);
  AVER(AlignCheck((Align)align));

  /* Align alloc pointer up and bounds check. */
  /* There's no PointerAlignUp, so we use AddrAlignUp @@@@ */
  /* .vm.addr-is-star: In this file, Addr is compatible with C */
  /* pointers. */
  blockBase = PointerAlignUp(boot->alloc, align);
  if(boot->limit <= blockBase || blockBase < boot->alloc) {
    return ResMEMORY;
  }
  blockLimit = PointerAdd(blockBase, size);
  /* Following checks that the ordering constraint holds: */
  /* boot->alloc <= blockBase < blockLimit <= boot->limit */
  /* (if it doesn't hold then something overallocated/wrapped round) */
  if(blockBase < boot->alloc ||
     blockLimit <= blockBase ||
     boot->limit < blockLimit) {
    return ResMEMORY;
  }

  /* Fits!  So allocate it */
  boot->alloc = blockLimit;
  *pReturn = blockBase;
  return ResOK;
}


/* VMArenaChunkCreate
 *
 * Arguments
 *
 * Creates a chunk and also allocates a "spare" block of memory (which
 * can be trivial, i.e. 0 size).
 *
 * The spare block feature is used by the VMArenaCreate to allocate
 * space for the VMArenaStruct during the boot sequence.
 *
 * .chunkcreate.arg.chunkreturn: chunkReturn, the obvious return
 *   parameter for the created chunk.
 * .chunkcreate.arg.spareReturn: spareReturn, the return parameter for
 *   a spare block of memory of at least spareSize (see
 *   .chunkcreate.arg.sparesize below) bytes.
 * .chunkcreate.arg.primary: TRUE iff primary.  Primary chunk contains
 *   arena.
 * .chunkcreate.arg.vmarena: vmArena, the parent VMArena.
 * .chunkcreate.arg.size: size, approximate amount of virtual address
 * that the chunk should reserve.  In practice this will be rounded up
 * to a page size (by the VM).
 * .chunkcreate.arg.sparesize: spareSize, the requested size of a block
 * which should be allocated in the chunk to be used by the caller.
 * Can be specified as 0 to indicate no block desired.
 */
static Res VMArenaChunkCreate(VMArenaChunk *chunkReturn,
                              void **spareReturn,
                              Bool primary, VMArena vmArena,
                              Size size, size_t spareSize)
{
  BT allocTable;
  BT noLatentPages;
  BT pageTableMapped;
  Count pages;
  Count pageTablePages;
  PageStruct *pageTable;
  Res res;
  Shift pageShift;
  Size pageSize;        /* cache of VMAlign() */
  Size pageTableSize;
  Size ullageSize;
  Size vmSize;
  VM vm;
  VMArenaBootStruct bootStruct;
  VMArenaBoot boot = &bootStruct;
  VMArenaChunk chunk;
  void *p;
  void *spare = NULL;

  AVER(chunkReturn != NULL);
  AVER(BoolCheck(primary));
  if(primary) {
    AVER(vmArena == NULL);
    AVER(spareReturn != NULL);
    AVER(spareSize > 0);
  } else {
    AVERT(VMArena, vmArena);
    AVER(spareReturn == NULL);
    AVER(spareSize == 0);
  }
  AVER(size > 0);

  res = VMCreate(&vm, size);
  if(res != ResOK)
    goto failVMCreate;

  pageSize = VMAlign(vm);
  pageShift = SizeLog2(pageSize);
  /* the VM will have aligned the userSize; pick up the actual size */
  vmSize = AddrOffset(VMBase(vm), VMLimit(vm));
  pages = vmSize >> pageShift;

  pageTableSize = SizeAlignUp(pages * sizeof(PageStruct),
                              pageSize);
  pageTablePages = pageTableSize >> pageShift;

  /* Allocate and map the descriptor and tables (comprising the ullage) */
  /* See design.mps.arena.coop-vm:chunk.create.tables */

  res = VMArenaBootInit(boot,
                        (void *)VMBase(vm),
                        (void *)VMLimit(vm));
  if(res != ResOK)
    goto failBootInit;

  res = VMArenaBootAlloc(&p, boot,
                         sizeof(VMArenaChunkStruct), MPS_PF_ALIGN);
  if(res != ResOK)
    goto failAllocChunk;
  chunk = p;  /* chunk now allocated, but not initialised */
  if(spareSize != 0) {
    res = VMArenaBootAlloc(&p, boot, spareSize, MPS_PF_ALIGN);
    if(res != ResOK)
      goto failAllocSpare;
    spare = p;
  }
  res = VMArenaBootAlloc(&p, boot,
                         (size_t)BTSize(pages), MPS_PF_ALIGN);
  if(res != ResOK)
    goto failAllocTable;
  allocTable = p;
  res = VMArenaBootAlloc(&p, boot,
                         (size_t)BTSize(pageTablePages), MPS_PF_ALIGN);
  if(res != ResOK)
    goto failPageTableMapped;
  pageTableMapped = p;
  res = VMArenaBootAlloc(&p, boot,
                         (size_t)BTSize(pageTablePages), MPS_PF_ALIGN);
  if(res != ResOK)
    goto failNoLatentPages;
  noLatentPages = p;
  res = VMArenaBootAlloc(&p, boot,
                         (size_t)pageTableSize, (size_t)pageSize);
  if(res != ResOK)
    goto failAllocPageTable;
  pageTable = p;
  ullageSize = VMArenaBootAllocated(boot);
  res = VMArenaBootFinish(boot);
  if(res != ResOK)
    goto failBootFinish;

  /* Actually commit the necessary addresses. */
  /* design.mps.arena.coop-vm:chunk.create.tables.map */
  if(primary) {
    /* no vmArena, so must call VMMap directly */
    res = VMMap(vm, VMBase(vm), (Addr)pageTable);
  } else {
    res = VMArenaMap(vmArena, vm, VMBase(vm), (Addr)pageTable);
  }
  if(res != ResOK)
    goto failTableMap;

  chunk->vm = vm;
  chunk->pageSize = pageSize;
  chunk->pageShift = pageShift;
  chunk->vmArena = vmArena;
  RingInit(&chunk->arenaRing);
  chunk->primary = primary;
  chunk->inBoot = primary;
  chunk->base = VMBase(vm);
  chunk->limit = VMLimit(vm);
  chunk->pages = pages;
  chunk->pageTable = pageTable;
  chunk->pageTablePages = pageTablePages;
  chunk->pageTableMapped = pageTableMapped;
  chunk->noLatentPages = noLatentPages;
  chunk->allocTable = allocTable;
  chunk->ullageSize = ullageSize;
  chunk->ullagePages = ullageSize >> chunk->pageShift;

  /* .ullagepages: pages whose page index is < ullagePages are */
  /* recorded as free but never allocated as alloc starts */
  /* searching after the tables (see .alloc.skip).  SegOfAddr */
  /* uses the fact that these pages are marked as free in order */
  /* to detect "references" to these pages as  being bogus see */
  /* .addr.free. */

  /* chunk->ullageSize is expected to be page aligned */
  AVER((chunk->ullageSize >> chunk->pageShift) << chunk->pageShift ==
       chunk->ullageSize);
  BTResRange(chunk->pageTableMapped, 0, chunk->pageTablePages);
  BTSetRange(chunk->noLatentPages, 0, chunk->pageTablePages);
  BTResRange(chunk->allocTable, 0, chunk->pages);

  chunk->sig = VMArenaChunkSig;
  AVERT(VMArenaChunk, chunk);
  *chunkReturn = chunk;
  if(spareSize != 0) {
    AVER(spare != NULL);
    *spareReturn = spare;
  }
  return ResOK;

failTableMap:
failBootFinish:
failAllocPageTable:
failNoLatentPages:
failPageTableMapped:
failAllocTable:
failAllocSpare:
failAllocChunk:
failBootInit:
  VMDestroy(vm);
failVMCreate:
  AVER(res != ResOK);
  return res;
}

static void VMArenaChunkDestroy(VMArenaChunk chunk)
{
  VM vm;
  VMArena vmArena;

  /* Can't check chunk during destroy as parent vmArena is invalid */
  AVER(chunk->sig == VMArenaChunkSig);

  chunk->sig = SigInvalid;
  RingFinish(&chunk->arenaRing);
  vm = chunk->vm;
  vmArena = chunk->vmArena;
  /* unmap the permanently mapped tables */
  if(chunk->primary) {
    /* must call VMUnmap directly */
    VMUnmap(vm, chunk->base, (Addr)chunk->pageTable);
  } else {
    VMArenaUnmap(vmArena, vm, chunk->base, (Addr)chunk->pageTable);
  }
  VMDestroy(vm);
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

static Res VMArenaInit(Arena *arenaReturn, ArenaClass class,
                       va_list args)
{
  Size userSize;        /* size requested by user */
  Size chunkSize;       /* size actually created */
  Res res;
  VMArena vmArena;
  Arena arena;
  Index gen;
  void *spare;
  VMArenaChunk chunk;

  userSize = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER((ArenaClass)mps_arena_class_vm() == class ||
       (ArenaClass)mps_arena_class_vmnz() == class);
  AVER(userSize > 0);

  res = VMArenaChunkCreate(&chunk, &spare, TRUE /* is primary */,
                           NULL, userSize, sizeof(VMArenaStruct));
  if(res != ResOK)
    goto failChunkCreate;
  
  vmArena = spare;
  vmArena->primary = chunk;
  RingInit(&vmArena->chunkRing);
  RingAppend(&vmArena->chunkRing, &chunk->arenaRing);
  /* This chunk is special in that it hasn't got a proper vmArena */
  /* field yet. */
  chunk->vmArena = vmArena;
  chunk->inBoot = FALSE;
  vmArena->committed = VMMapped(vmArena->primary->vm);
  RingInit(&vmArena->latentRing);
  vmArena->latentSize = 0;

  arena = VMArenaArena(vmArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, class);

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of zones as will fit into a reference set (the number of */
  /* bits in a word).  Note that some zones are discontiguous in the */
  /* chunk if the size is not a power of 2.  See */
  /* design.mps.arena.class.fields. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);
  arena->alignment = chunk->pageSize;

  /* .blacklist: We blacklist the first and last zones because */
  /* they commonly correspond to low integers. */
  /* .improve.blacklist.dynamic: @@@@ This should be dynamic. */
  vmArena->blacklist = 
    RefSetAdd(arena, RefSetAdd(arena, RefSetEMPTY, (Addr)1), (Addr)-1);

  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genRefSet[gen] = RefSetEMPTY;
  }

  vmArena->freeSet = RefSetUNIV; /* includes blacklist */
  /* design.mps.arena.coop-vm:struct.vmarena.extendby.init */
  vmArena->extendBy = userSize;

  /* initialize and load cache */
  VMArenaChunkCacheEntryInit(&vmArena->chunkCache);
  VMArenaChunkEncache(vmArena, vmArena->primary);

  /* Sign and check the arena. */
  vmArena->sig = VMArenaSig;
  AVERT(VMArena, vmArena);

  EVENT_PP(ArenaCreate, vmArena, chunk);

  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
  return res;
}


/* VMArenaFinish -- finish the arena */

static void VMArenaFinish(Arena arena)
{
  VMArena vmArena;
  Ring node, next;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  VMArenaPurgeLatentPages(vmArena);
  
  vmArena->sig = SigInvalid;

  ArenaFinish(arena); /* impl.c.arena.finish.caller */
  /* destroy all chunks except primary (destroy primary last) */
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    /* Can't check chunk as vmArena is invalid during Destroy */

    RingRemove(node);
    if(chunk != vmArena->primary) {
      VMArenaChunkDestroy(chunk);
    }
  }
  RingFinish(&vmArena->chunkRing);
  VMArenaChunkDestroy(vmArena->primary); /* destroys arena too */

  EVENT_P(ArenaDestroy, vmArena);
}


/* VMArenaReserved -- return the amount of reserved address space
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaReserved(Arena arena)
{
  Size reserved;
  Ring node, next;
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  reserved = 0;
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    reserved += VMReserved(chunk->vm);
  }

  return reserved;
}


/* VMArenaCommitted -- return the amount of committed virtual memory
 */

static Size VMArenaCommitted(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return vmArena->committed;
}

/* VMArenaSpareCommitExceeded
 *
 * Simply calls through to Purge
 */

static void VMArenaSpareCommitExceeded(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  VMArenaPurgeLatentPages(vmArena);

  return;
}


/* Page Table Partial Mapping
 *
 * Some helper functions */


/* tablePageBaseIndex -- index of the first page descriptor falling
 *                       (at least partially) on this table page
 *
 * .repr.table-page: Table pages are passed as the page's base address.
 *
 * .division: We calculate it by dividing the offset from the beginning
 * of the page table by the size of a table element.  This relies on
 * .vm.addr-is-star.
 */

#define tablePageBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
   / sizeof(PageStruct))

/* tablePageWholeBaseIndex
 *
 * Index of the first page descriptor wholly on this table page.
 * Table page specified by address (not index).
 */
#define tablePageWholeBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, \
              AddrAdd((tablePage), sizeof(PageStruct)-1)) \
   / sizeof(PageStruct))


/* tablePageLimitIndex -- index of the first page descriptor falling
 *                        (wholly) on the next table page
 *
 * Similar to tablePageBaseIndex, see .repr.table-page and .division.
 */

#define tablePageLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + (chunk)->pageSize - 1) \
   / sizeof(PageStruct) \
   + 1)

/* tablePageWholeLimitIndex
 *
 * Index of the first page descriptor falling partially on the next
 * table page.
 */
#define tablePageWholeLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage))+(chunk)->pageSize) \
   / sizeof(PageStruct))


/* tablePageInUse
 *
 * Check whether a given page of the page table is in use.
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if
 * any of the page descriptors falling on it (even partially) are being
 * used.  Relies on .repr.table-page and .vm.addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're
 * (de)allocating.
 */

static Bool tablePageInUse(VMArenaChunk chunk, Addr tablePage)
{
  Index limitIndex;

  AVERT(VMArenaChunk, chunk);
  /* Check it's in the page table. */
  AVER((Addr)&chunk->pageTable[0] <= tablePage);
  AVER(tablePage < addrOfPageDesc(chunk, chunk->pages));

  if(tablePage == addrPageBase(chunk, addrOfPageDesc(chunk, chunk->pages))) {
    limitIndex = chunk->pages;
  } else {
    limitIndex = tablePageLimitIndex(chunk, tablePage);
  }
  AVER(limitIndex <= chunk->pages);

  return !BTIsResRange(chunk->allocTable,
                       tablePageBaseIndex(chunk, tablePage),
                       limitIndex);
}


/* Table Pages Used
 *
 * Takes a range of pages identified by [pageBase, pageLimit), and
 * returns the pages occupied by the page table which store the
 * PageStruct descriptors for those pages.
 */
static void VMArenaTablePagesUsed(Index *tableBaseReturn,
                                  Index *tableLimitReturn,
				  VMArenaChunk chunk,
				  Index pageBase, Index pageLimit) {
  /* static used only internally, so minimal checking */
  *tableBaseReturn =
    PageTablePageIndex(chunk,
                       addrPageBase(chunk, addrOfPageDesc(chunk, pageBase)));
  *tableLimitReturn = 
    PageTablePageIndex(chunk,
                       AddrAlignUp(addrOfPageDesc(chunk, pageLimit),
		                   chunk->pageSize));
  
  return;
}


/* Pages from baseIndex to limitIndex are about to be allocated.
 * Ensure that the relevant pages occupied by the page table are
 * mapped. */
static Res VMArenaEnsurePageTableMapped(VMArenaChunk chunk,
                                Index baseIndex, Index limitIndex)
{
  /* tableBaseIndex, tableLimitIndex, tableCursorIndex, */
  /* unmappedBase, unmappedLimit are all indexes of pages occupied */
  /* by the page table. */
  Index i;
  Index tableBaseIndex, tableLimitIndex;
  Index tableCursorIndex;
  Index unmappedBaseIndex, unmappedLimitIndex;
  Res res;

  VMArenaTablePagesUsed(&tableBaseIndex, &tableLimitIndex,
                        chunk, baseIndex, limitIndex);

  tableCursorIndex = tableBaseIndex;
  
  while(BTFindLongResRange(&unmappedBaseIndex, &unmappedLimitIndex,
                           chunk->pageTableMapped,
		           tableCursorIndex, tableLimitIndex,
		           1)) {
    Addr unmappedBase = TablePageIndexBase(chunk, unmappedBaseIndex);
    Addr unmappedLimit = TablePageIndexBase(chunk, unmappedLimitIndex);
    /* There might be a page descriptor overlapping the beginning */
    /* of the range of table pages we are about to map. */
    /* We need to work out whether we should touch it. */
    if(unmappedBaseIndex == tableBaseIndex &&
       unmappedBaseIndex > 0 &&
       !BTGet(chunk->pageTableMapped, unmappedBaseIndex - 1))
    {
      /* Start with first descriptor wholly on page */
      baseIndex = tablePageWholeBaseIndex(chunk, unmappedBase);
    } else {
      /* start with first descriptor partially on page */
      baseIndex = tablePageBaseIndex(chunk, unmappedBase);
    }
    /* Similarly for the potentially overlapping page descriptor */
    /* at the end. */
    if(unmappedLimitIndex == tableLimitIndex &&
       unmappedLimitIndex < chunk->pageTablePages &&
       !BTGet(chunk->pageTableMapped, unmappedLimitIndex))
    {
      /* Finish with last descriptor wholly on page */
      limitIndex = tablePageBaseIndex(chunk, unmappedLimit);
    } else if(unmappedLimitIndex == chunk->pageTablePages) {
      /* Finish with last descriptor in chunk */
      limitIndex = chunk->pages;
    } else {
      /* Finish with last descriptor partially on page */
      limitIndex = tablePageWholeBaseIndex(chunk, unmappedLimit);
    }
    res = VMArenaMap(chunk->vmArena, chunk->vm, 
		     unmappedBase, unmappedLimit);
    if(res != ResOK) {
      return res;
    }
    BTSetRange(chunk->pageTableMapped, unmappedBaseIndex, unmappedLimitIndex);
    for(i = baseIndex; i < limitIndex; ++i) {
      VMArenaPageInit(chunk, i);
    }
    tableCursorIndex = unmappedLimitIndex;
    if(tableCursorIndex == tableLimitIndex)
      break;
  }

  return ResOK;
}

/* Of the pages occupied by the page table from tablePageBase to
 * tablePageLimit find those which are wholly unused and unmap them.
 */
static void VMArenaUnmapUnusedTablePages(VMArenaChunk chunk,
                                         Addr tablePageBase,
					 Addr tablePageLimit)
{
  Addr cursor;
  Size pageSize;

  pageSize = chunk->pageSize;

  /* minimal checking as static function only called locally */
  AVER(AddrIsAligned(tablePageBase, pageSize));
  AVER(AddrIsAligned(tablePageLimit, pageSize));


  /* for loop indexes over base addresses of pages occupied by page table */
  for(cursor = tablePageBase;
      cursor < tablePageLimit;
      cursor = AddrAdd(cursor, pageSize)) {
    if(!tablePageInUse(chunk, cursor)) {
      VMArenaUnmap(chunk->vmArena, chunk->vm,
                   cursor,
		   AddrAdd(cursor, pageSize));
      AVER(BTGet(chunk->noLatentPages, PageTablePageIndex(chunk, cursor)));
      AVER(BTGet(chunk->pageTableMapped, PageTablePageIndex(chunk, cursor)));
      BTRes(chunk->pageTableMapped, PageTablePageIndex(chunk, cursor));
    }
  }
  AVER(cursor == tablePageLimit);

  return;
}
      

/* findFreeInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 *
 * .findfreeinarea.arg.downwards: downwards basically governs whether
 * we use BTFindShortResRange (if downwards is FALSE) or
 * BTFindShortResRangeHigh (if downwards is TRUE).
 * .findfreeinarea.arg.downwards.justify: This _roughly_
 * corresponds to allocating segments from top down (when downwards is
 * TRUE), at least within an interval.  It is used for implementing
 * SegPrefHigh.
 */

static Bool findFreeInArea(Index *baseReturn,
                           VMArenaChunk chunk, Size size,
                           Addr base, Addr limit,
                           Bool downwards)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;    /* Index equiv. to base and limit */
  Index start, end;             /* base and limit of free run */

  AVER(baseReturn != NULL);
  AVERT(VMArenaChunk, chunk);
  AVER(AddrIsAligned(base, chunk->pageSize));
  AVER(AddrIsAligned(limit, chunk->pageSize));
  AVER(chunk->base <= base);
  AVER(base < limit);
  AVER(limit <= chunk->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, chunk->pageSize));
  AVER(BoolCheck(downwards));

  basePage = indexOfAddr(chunk, base);
  limitPage = indexOfAddr(chunk, limit);
  pages = size >> chunk->pageShift;

  if(downwards) {
    if(!BTFindShortResRangeHigh(&start, &end,
                                chunk->allocTable,
                                basePage, limitPage,
                                pages)) {
      return FALSE;
    }
  } else {
    if(!BTFindShortResRange(&start, &end,
                            chunk->allocTable,
                            basePage, limitPage,
                            pages)) {
      return FALSE;
    }
  }

  *baseReturn = start;
  return TRUE;
}


/* VMFindFreeInRefSet -- try to allocate a segment with a RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a segment whose RefSet (see
 * RefSetOfSeg) will be a subset of the specified RefSet.
 *
 * For meaning of downwards arg see findFreeInArea.
 * .improve.findfreeinrefset.downwards: This
 * should be improved so that it allocates segments from top down
 * globally, as opposed to (currently) just within an interval.
 */

static Bool VMFindFreeInRefSet(Index *baseReturn,
                             VMArenaChunk *chunkReturn,
                             VMArena vmArena, Size size, RefSet refSet,
                             Bool downwards)
{
  Arena arena;
  Addr chunkBase, base, limit;
  Size zoneSize;
  Ring node, next;

  AVER(baseReturn != NULL);
  AVER(chunkReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);
  AVER(RefSetCheck(refSet));
  AVER(BoolCheck(downwards));

  arena = VMArenaArena(vmArena);
  zoneSize = (Size)1 << arena->zoneShift;

  /* .improve.alloc.chunk.cache: check (non-existant) chunk cache */
  /* first? */
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    AVERT(VMArenaChunk, chunk);

    /* .alloc.skip: The first address available for segments, */
    /* is just after the arena tables. */
    chunkBase = PageIndexBase(chunk, chunk->ullagePages);

    base = chunkBase;
    while(base < chunk->limit) {
      if(RefSetIsMember(arena, refSet, base)) {
        /* Search for a run of zone stripes which are in the RefSet */
        /* and the arena.  Adding the zoneSize might wrap round (to */
        /* zero, because limit is aligned to zoneSize, which is a */
        /* power of two). */
        limit = base;
        do {
          /* advance limit to next higher zone stripe boundary */
          limit = AddrAlignUp(AddrAdd(limit, 1), zoneSize);

          AVER(limit > base || limit == (Addr)0);

          if(limit >= chunk->limit || limit < base) {
            limit = chunk->limit;
            break;
          }

          AVER(base < limit && limit < chunk->limit);
        } while(RefSetIsMember(arena, refSet, limit));

        /* If the RefSet was universal, then the area found ought to */
        /* be the whole chunk. */
        AVER(refSet != RefSetUNIV ||
             (base == chunkBase && limit == chunk->limit));

        /* Try to allocate a segment in the area. */
        if(AddrOffset(base, limit) >= size &&
           findFreeInArea(baseReturn,
                          chunk, size, base, limit, downwards)) {
          *chunkReturn = chunk;
          return TRUE;
        }
        
        base = limit;
      } else {
        /* Adding the zoneSize might wrap round (to zero, because */
        /* base is aligned to zoneSize, which is a power of two). */
        base = AddrAlignUp(AddrAdd(base, 1), zoneSize);
        AVER(base > chunkBase || base == (Addr)0);
        if(base >= chunk->limit || base < chunkBase) {
          base = chunk->limit;
          break;
        }
      }
    }

    AVER(base == chunk->limit);
  }

  return FALSE;
}


static Serial vmGenOfSegPref(VMArena vmArena, SegPref pref)
{
  Serial gen;

  AVERT(VMArena, vmArena);
  AVERT(SegPref, pref);
  AVER(pref->isGen);

  gen = pref->gen;
  if(gen >= VMArenaGenCount) {
    gen = VMArenaGenCount - 1;
  }
  return gen;
}

/* VMSegFind
 *
 * Finds space for a segment (note it does not create or allocate a
 * segment).
 *
 * .vmsegfind.arg.basereturn: return parameter for the index in the
 *   chunk's page table of the base of the free area found.
 * .vmsegfind.arg.chunkreturn: return parameter for the chunk in which
 *   the free space has been found.
 * .vmsegfind.arg.vmarena:
 * .vmsegfind.arg.pref: the SegPref object to be used when considering
 *   which zones to try.
 * .vmsegfind.arg.size: Size of segment to find space for.
 * .vmsegfind.arg.barge: TRUE iff stealing space in zones used
 *   by other SegPrefs should be considered (if it's FALSE then only
 * zones already used by this segpref or free zones will be used).
 */

static Bool VMSegFind(Index *baseReturn, VMArenaChunk *chunkReturn,
                      VMArena vmArena, SegPref pref, Size size,
                      Bool barge)
{
  RefSet refSet;

  /* This function is local to VMSegAlloc, so */
  /* no checking required */

  if(pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    refSet = vmArena->genRefSet[gen];
  } else {
    refSet = pref->refSet;
  }

  /* @@@@ Some of these tests might be duplicates.  If we're about */
  /* to run out of virtual address space, then slow allocation is */
  /* probably the least of our worries. */

  /* .segalloc.improve.map: Define a function that takes a list */
  /* (say 4 long) of RefSets and tries VMFindFreeInRefSet on */
  /* each one in turn.  Extra RefSet args that weren't needed */
  /* could be RefSetEMPTY */

  if(pref->isCollected) { /* GC'd segment */
    /* We look for space in the following places (in order) */
    /*   - Zones already allocated to me (refSet) but are not */
    /*     blacklisted; */
    /*   - Zones that are either allocated to me, or are unallocated */
    /*     but not blacklisted; */
    /*   - Any non-blacklisted zone; */
    /*   - Any zone; */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated (or the default */
    /* is used). */
    if(VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                        RefSetDiff(refSet, vmArena->blacklist),
                        pref->high) ||
       VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                        RefSetUnion(refSet,
                                    RefSetDiff(vmArena->freeSet, 
                                               vmArena->blacklist)),
                                               pref->high))
    {
      /* found */
      return TRUE;
    }
    if(!barge) {
      /* do not barge into other zones, give up now */
      return FALSE;
    }
    if(VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                        RefSetDiff(RefSetUNIV, vmArena->blacklist),
                        pref->high) ||
       VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                        RefSetUNIV, pref->high))
    {
      /* found */
      return TRUE;
    }
  } else { /* non-GC'd segment */
    /* We look for space in the following places (in order) */
    /*   - Zones preferred (refSet) and blacklisted; */
    /*   - Zones preferred; */
    /*   - Zones preferred or blacklisted zone; */
    /*   - Any zone. */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated. */
    if(VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                         RefSetInter(refSet, vmArena->blacklist),
                         pref->high) ||
       VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                         refSet, pref->high) ||
       VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                         RefSetUnion(refSet, vmArena->blacklist),
                         pref->high) ||
       VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                         RefSetUNIV, pref->high)) {
      return TRUE;
    }
  }
  return FALSE;
}

/* VMExtend -- Extend the arena by making a new chunk
 *
 * .extend.arg.vmarena: vmArena.  The VMArena obviously.
 * .extend.arg.size: size.  size of segment that we wish
 *   to allocate after the extension.
 */
static Res VMExtend(VMArena vmArena, Size size)
{
  VMArenaChunk newChunk;
  Size chunkSize;
  Res res;

  /* Internal static function, so no checking. */

  /* @@@@ .improve.debug: chunkSize (calculated below) won't */
  /* be big enough if the tables of the new chunk are */
  /* more than vmArena->extendBy (because there will be few than */
  /* size bytes free in the new chunk).  Fix this. */
  chunkSize = vmArena->extendBy + size;
  res = VMArenaChunkCreate(&newChunk, NULL /* spare */,
                           FALSE /* primary */, vmArena,
                           chunkSize, 0 /* spareSize */);
  /* .improve.chunk-create.fail: If we fail we could try again */
  /* (with a smaller size say).  We don't do this. */
  if(ResOK == res) {
    RingAppend(&vmArena->chunkRing, &newChunk->arenaRing);
  }

  return res;
}

/* Used in abstracting allocation policy between VM and VMNZ */
typedef Res (*VMSegAllocPolicyMethod)(Index *, VMArenaChunk *,
                                      VMArena, SegPref, Size);

static Res VMSegAllocPolicy(Index *baseIndexReturn,
                            VMArenaChunk *chunkReturn,
                            VMArena vmArena,
                            SegPref pref,
                            Size size)
{
  /* internal and static, no checking */

  if(!VMSegFind(baseIndexReturn, chunkReturn,
                vmArena, pref, size, FALSE)) {
    /* try and extend, but don't worry if we can't */
    (void)VMExtend(vmArena, size);

    /* We may or may not have a new chunk at this point */
    /* we proceed to try the allocation again anyway. */
    /* We specify barging, but if we have got a new chunk */
    /* then hopefully we won't need to barge. */
    if(!VMSegFind(baseIndexReturn, chunkReturn,
                  vmArena, pref, size, TRUE)) {
      /* .improve.alloc-fail: This could be because the request was */
      /* too large, or perhaps the arena is fragmented.  We could */
      /* return a more meaningful code. */
      return ResRESOURCE;
    }
  }
  return ResOK;
}

static Res VMNZSegAllocPolicy(Index *baseIndexReturn,
                              VMArenaChunk *chunkReturn,
                              VMArena vmArena,
                              SegPref pref,
                              Size size)
{
  /* internal and static, no checking */

  if(VMFindFreeInRefSet(baseIndexReturn, chunkReturn, vmArena, size, 
                      RefSetUNIV,
                      pref->high)) {
    return ResOK;
  }
  return ResRESOURCE;
}

/* Checks whether a free page is mapped or not. */
static Bool VMArenaPageIsMapped(VMArenaChunk chunk, Index pi)
{
  Index pageTableBaseIndex;
  Index pageTableLimitIndex;
  int pageType;

  /* Note that unless the pi'th PageStruct crosses a page boundary */
  /* Base and Limit will differ by exactly 1. */
  /* They will differ by at most 2 assuming that */
  /* sizeof(PageStruct) <= chunk->pageSize (!) */

  VMArenaTablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex,
                        chunk, pi, pi+1);
  /* using unsigned arithmetic overflow to use just one comparison */
  AVER(pageTableLimitIndex - pageTableBaseIndex - 1 < 2);

  /* We can examine the PageStruct descriptor iff both table pages */
  /* are mapped. */
  if(BTGet(chunk->pageTableMapped, pageTableBaseIndex) &&
     BTGet(chunk->pageTableMapped, pageTableLimitIndex - 1)) {
    pageType = PageRest(&chunk->pageTable[pi])->type;
    if(PageTypeLatent == pageType) {
      return TRUE;
    }
    AVER(PageTypeFree == pageType);
  }
  return FALSE;
}


/* Used internally by VMSegAllocComm.  Sets up the PageStruct descriptors
 * for allocated page. There are a lot of redundant arguments to
 * avoid recomputation. */
static void VMArenaPageAlloc(VMArenaChunk chunk, Index pi,
                             Index piBase, Count pages,
			     Addr segLimit, Seg seg, Pool pool)
{
  /* static called only locally, so minimal checking */
  AVER(!BTGet(chunk->allocTable, pi));
  BTSet(chunk->allocTable, pi);
  if(pi == piBase) {
    SegInit(seg, pool);
    if(pages == 1) {
      SegSetSingle(seg, TRUE);
    } else {
      SegSetSingle(seg, FALSE);
    }
    return;
  }
  PageRest(&chunk->pageTable[pi])->pool = NULL;
  PageRest(&chunk->pageTable[pi])->type = PageTypeTail;
  PageTail(&chunk->pageTable[pi])->seg = seg;
  PageTail(&chunk->pageTable[pi])->limit = segLimit;

  return;
}

static void VMArenaPageInit(VMArenaChunk chunk, Index pi)
{
  BTRes(chunk->allocTable, pi);
  PageRest(&chunk->pageTable[pi])->pool = NULL;
  PageRest(&chunk->pageTable[pi])->type = PageTypeFree;

  return;
}

static void VMArenaPageFree(VMArenaChunk chunk, Index pi)
{
  AVER(BTGet(chunk->allocTable, pi));
  VMArenaPageInit(chunk, pi);

  return;
}


/* Removes the PageStruct descriptor from the Hysteresis fund. */
/* Temporarily leaves it in an inconsistent state. */
static void VMArenaHysteresisRemovePage(VMArenaChunk chunk, Index pi)
{
  Arena arena = VMArenaArena(chunk->vmArena);
  /* minimal checking as it's a static used only locally */
  AVER(PageTypeLatent == PageRest(&chunk->pageTable[pi])->type);
  RingRemove(&PageRest(&chunk->pageTable[pi])->the.latent.arenaRing);
  AVER(arena->spareCommitted >= chunk->pageSize);
  arena->spareCommitted -= chunk->pageSize;

  return;
}


/* VMSegAllocComm -- allocate a segment from the arena
 *
 * Common code used by mps_arena_class_vm and
 * mps_arena_class_vmnz. */

static Res VMSegAllocComm(Seg *segReturn,
                          VMSegAllocPolicyMethod policy,
                          SegPref pref,
                          Size size,
                          Pool pool)
{
  Addr segBase, segLimit;
  Arena arena;
  Count pages;
  Index i;
  Index baseIndex, limitIndex;
  Index mappedBase, mappedLimit;
  Index pageTableBaseIndex, pageTableLimitIndex;
  Index unmappedBase, unmappedLimit;
  RefSet segRefSet;
  Res res;
  Seg seg;
  VMArena vmArena;
  VMArenaChunk chunk;

  AVER(segReturn != NULL);
  AVER(FunCheck((Fun)policy));
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  /* Assume all chunks have same pageSize (see */
  /* design.mps.arena.coop-vm:struct.chunk.pagesize.assume) */
  AVER(SizeIsAligned(size, vmArena->primary->pageSize));
  
  /* NULL is used as a discriminator */
  /* (see design.mps.arena.vm:table.disc) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  /* Early check on commit limit. */
  if(VMArenaArena(vmArena)->spareCommitted < size) {
    Size necessaryCommitIncrease =
      size - VMArenaArena(vmArena)->spareCommitted;
    if(vmArena->committed + necessaryCommitIncrease > arena->commitLimit ||
       vmArena->committed + necessaryCommitIncrease < vmArena->committed) {
      return ResCOMMIT_LIMIT;
    }
  }

  res = (*policy)(&baseIndex, &chunk, vmArena, pref, size);
  if(res != ResOK) {
    return res;
  }

  /* chunk (and baseIndex) should be initialised by policy */
  AVERT(VMArenaChunk, chunk);

  segBase = PageIndexBase(chunk, baseIndex);
  segLimit = AddrAdd(segBase, size);
  seg = PageSeg(&chunk->pageTable[baseIndex]);

  /* Compute number of pages to be allocated. */
  pages = size >> chunk->pageShift;

  /* Ensure that the page descriptors we need are on mapped pages. */
  res = VMArenaEnsurePageTableMapped(chunk, baseIndex, baseIndex + pages);
  if(res != ResOK)
    goto failTableMap;

  mappedBase = baseIndex;
  mappedLimit = mappedBase;
  limitIndex = baseIndex + pages;

  do {
    while(VMArenaPageIsMapped(chunk, mappedLimit)) {
      ++mappedLimit;
      if(mappedLimit >= limitIndex) {
	break;
      }
    }
    AVER(mappedLimit <= limitIndex);
    /* NB for loop will loop 0 times iff first page is not mapped */
    for(i = mappedBase; i < mappedLimit; ++i) {
      VMArenaHysteresisRemovePage(chunk, i);
      VMArenaPageAlloc(chunk, i, baseIndex, pages, segLimit, seg, pool);
    }
    if(mappedLimit >= limitIndex)
      break;
    unmappedBase = mappedLimit;
    unmappedLimit = unmappedBase;
    while(!VMArenaPageIsMapped(chunk, unmappedLimit)) {
      ++unmappedLimit;
      if(unmappedLimit >= limitIndex) {
        break;
      }
    }
    AVER(unmappedLimit <= limitIndex);
    res = VMArenaMap(vmArena, chunk->vm, 
		     PageIndexBase(chunk, unmappedBase),
		     PageIndexBase(chunk, unmappedLimit));
    if(res != ResOK) {
      goto failPagesMap;
    }
    for(i = unmappedBase; i < unmappedLimit; ++i) {
      VMArenaPageAlloc(chunk, i, baseIndex, pages, segLimit, seg, pool);
    }
    mappedBase = unmappedLimit;
    mappedLimit = mappedBase;
  } while(mappedLimit < limitIndex);
  AVER(mappedLimit == limitIndex);

  segRefSet = RefSetOfSeg(arena, seg);

  if(pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    vmArena->genRefSet[gen] = 
      RefSetUnion(vmArena->genRefSet[gen], segRefSet);
  }

  vmArena->freeSet = RefSetDiff(vmArena->freeSet, segRefSet);
  
  AVERT(Seg, seg);
  
  *segReturn = seg;
  return ResOK;

failPagesMap:
  /* region from baseIndex to mappedLimit needs unmapping */
  if(baseIndex < mappedLimit) {
    VMArenaUnmap(vmArena, chunk->vm,
		 PageIndexBase(chunk, baseIndex),
		 PageIndexBase(chunk, mappedLimit));
    /* mark pages as free */
    SegFinish(PageSeg(&chunk->pageTable[baseIndex]));
    for(i = baseIndex; i < mappedLimit; ++i) {
      VMArenaPageFree(chunk, i);
    }
  }
  /* find which pages of page table were affected */
  VMArenaTablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex,
                        chunk, baseIndex, limitIndex);
  /* Resetting the noLatentPages bits is lazy, it means that */
  /* we don't have to bother trying to unmap unused portions */
  /* of the pageTable. */
  BTResRange(chunk->noLatentPages,
             pageTableBaseIndex, pageTableLimitIndex);
failTableMap:
  return res;
}


static Res VMSegAlloc(Seg *segReturn, SegPref pref, Size size,
                      Pool pool)
{
  /* All checks performed in common VMSegAllocComm */
  return VMSegAllocComm(segReturn,
                        VMSegAllocPolicy, pref, size, pool);
}

static Res VMNZSegAlloc(Seg *segReturn, SegPref pref, Size size,
                        Pool pool)
{
  /* All checks performed in common VMSegAllocComm */
  return VMSegAllocComm(segReturn,
                        VMNZSegAllocPolicy, pref, size, pool);
}

static VMArenaChunk VMArenaChunkOfSeg(VMArena vmArena, Seg seg)
{
  Ring node, next;
  /* critical because used from critical functions */
  AVERT_CRITICAL(VMArena, vmArena);
  AVERT_CRITICAL(Seg, seg);

  /* check cache first */
  if(vmArena->chunkCache.pageTableBase <= (Page)seg &&
     (Page)seg < vmArena->chunkCache.pageTableLimit) {
    return vmArena->chunkCache.chunk;
  }

  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    Page page = PageOfSeg(seg);

    if(&chunk->pageTable[0] <= page &&
       page < &chunk->pageTable[chunk->pages]) {
      /* Gotcha! */
      VMArenaChunkEncache(vmArena, chunk);
      return chunk;
    }
  }
  NOTREACHED;
  return NULL;
}


/* The function f is called on the ranges of latent pages which are
 * within the range of pages from base to limit.  PageStruct descriptors
 * from base to limit should be mapped in the page table before calling
 * this function. */
static void VMArenaFindLatentRanges(
  VMArenaChunk chunk, Index base, Index limit,
  void (*f)(VMArenaChunk, Index, Index, void *, size_t),
  void *p, size_t s)
{
  Index latentBase, latentLimit;

  /* Minimal checking as static used only internally. */
  AVER(base < limit);

  latentBase = base;
  do {
    while(!PageIsLatent(&chunk->pageTable[latentBase])) {
      ++latentBase;
      if(latentBase >= limit)
	goto done;
    }
    latentLimit = latentBase;
    while(PageIsLatent(&chunk->pageTable[latentLimit])) {
      ++latentLimit;
      if(latentLimit >= limit)
	break;
    }
    f(chunk, latentBase, latentLimit, p, s);
    latentBase = latentLimit;
  } while(latentBase < limit);
done:
  AVER(latentBase == limit);

  return;
}


/* Takes a range of pages which are latent pages (in the hysteresis fund), */
/* unmaps them and removes them from the hysteresis fund. */
static void VMArenaUnmapLatentRange(VMArenaChunk chunk,
                                    Index rangeBase, Index rangeLimit,
				    void *p, size_t s)
{
  Index i;

  /* The closure variable are not used */
  UNUSED(p);
  UNUSED(s);

  for(i = rangeBase; i < rangeLimit; ++i) {
    VMArenaHysteresisRemovePage(chunk, i);
    VMArenaPageInit(chunk, i);
  }
  VMArenaUnmap(chunk->vmArena, chunk->vm,
               PageIndexBase(chunk, rangeBase),
	       PageIndexBase(chunk, rangeLimit));
  
  return;
}
  

/* PurgeLatentPages
 *
 * All latent pages are found and removed from the hysteresis fund
 * (ie they are unmapped).  Pages occupied by the page table are
 * potentially unmapped.  This is currently the only way the hysteresis
 * fund is prevented from growing.
 *
 * It uses the noLatentPages bits to determine which areas of the
 * pageTable to examine.
 */
static void VMArenaPurgeLatentPages(VMArena vmArena)
{
  Ring node, next;

  AVERT(VMArena, vmArena);

  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    Index latentBaseIndex, latentLimitIndex;
    Index tablePageCursor = 0;
    while(BTFindLongResRange(&latentBaseIndex, &latentLimitIndex,
                             chunk->noLatentPages,
	           	     tablePageCursor, chunk->pageTablePages,
	      	             1)) {
      Addr latentTableBase, latentTableLimit;
      Index pageBase, pageLimit;
      Index tablePage;

      latentTableBase = TablePageIndexBase(chunk, latentBaseIndex);
      latentTableLimit = TablePageIndexBase(chunk, latentLimitIndex);
      /* Determine whether to use initial overlapping PageStruct. */
      if(latentBaseIndex > 0 &&
         !BTGet(chunk->pageTableMapped, latentBaseIndex - 1)) {
	pageBase = tablePageWholeBaseIndex(chunk, latentTableBase);
      } else {
        pageBase = tablePageBaseIndex(chunk, latentTableBase);
      }
      for(tablePage = latentBaseIndex;
          tablePage < latentLimitIndex;
	  ++tablePage) {
	/* Determine whether to use final overlapping PageStruct. */
        if(tablePage == latentLimitIndex - 1 &&
	   latentLimitIndex < chunk->pageTablePages &&
	   !BTGet(chunk->pageTableMapped, latentLimitIndex)) {
	  pageLimit =
	    tablePageWholeLimitIndex(chunk,
	                             TablePageIndexBase(chunk, tablePage));
	} else if(tablePage == chunk->pageTablePages - 1) {
	  pageLimit = chunk->pages;
	} else {
	  pageLimit =
	    tablePageLimitIndex(chunk,
	                        TablePageIndexBase(chunk, tablePage));
	}
	VMArenaFindLatentRanges(chunk, pageBase, pageLimit,
	                        VMArenaUnmapLatentRange, NULL, 0);
	BTSet(chunk->noLatentPages, tablePage);
	pageBase = pageLimit;
      }
      VMArenaUnmapUnusedTablePages(chunk,
                                   latentTableBase, latentTableLimit);
      tablePageCursor = latentLimitIndex;
      if(tablePageCursor >= chunk->pageTablePages) {
        AVER(tablePageCursor == chunk->pageTablePages);
	break;
      }
    }

  }

  AVER(VMArenaArena(vmArena)->spareCommitted == 0);
  
  return;
}


/* Add Pages to Hysteresis fund.
 *
 * Pages with indices from pageBase up to but not including pageLimit
 * are converted to latent pages and added to the hysteresis fund.
 */

static void VMArenaHysteresisAddPages(VMArena vmArena, VMArenaChunk chunk,
                                      Index piBase, Index piLimit)
{
  Arena arena;
  Index pi;
  Index pageTableBase;
  Index pageTableLimit;

  AVERT(VMArena, vmArena);
  AVERT(VMArenaChunk, chunk);
  AVER(piBase < piLimit);
  AVER(piLimit <= chunk->pages);

  arena = VMArenaArena(vmArena);

  /* loop from pageBase to pageLimit-1 inclusive */
  for(pi = piBase; pi < piLimit; ++pi) {
    PageRest(&chunk->pageTable[pi])->pool = NULL;
    PageRest(&chunk->pageTable[pi])->type = PageTypeLatent;
    RingInit(&PageRest(&chunk->pageTable[pi])->the.latent.arenaRing);
    RingAppend(&vmArena->latentRing,
               &PageRest(&chunk->pageTable[pi])->the.latent.arenaRing);
  }
  arena->spareCommitted += (piLimit - piBase) << chunk->pageShift;
  BTResRange(chunk->allocTable, piBase, piLimit);

  VMArenaTablePagesUsed(&pageTableBase, &pageTableLimit,
                        chunk, piBase, piLimit);
  BTResRange(chunk->noLatentPages, pageTableBase, pageTableLimit);

  if(arena->spareCommitted > arena->spareCommitLimit) {
    VMArenaPurgeLatentPages(vmArena);
  }

  return;
}


/* VMSegFree - free a segment in the arena
 */

static void VMSegFree(Seg seg)
{
  VMArena vmArena;
  VMArenaChunk chunk;
  Page page;
  Count pages;
  Index basePage;
  Addr base, limit;

  AVERT(Seg, seg);
  vmArena = SegVMArena(seg);
  AVERT(VMArena, vmArena);

  chunk = VMArenaChunkOfSeg(vmArena, seg);

  page = PageOfSeg(seg);
  limit = VMSegLimit(seg);
  basePage = page - chunk->pageTable;
  AVER(basePage < chunk->pages);

  SegFinish(seg);

  base = PageIndexBase(chunk, basePage);
  /* Calculate the number of pages in the segment */
  pages = AddrOffset(base, limit) >> chunk->pageShift;
  VMArenaHysteresisAddPages(vmArena, chunk, basePage, basePage + pages);

  return;
}


/* .seg.critical: These Seg functions are low-level and are on 
 * the critical path in various ways.  The more common therefore 
 * use AVER_CRITICAL
 */

/* VMSegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then returning the
 * base address of that page.
 */

static Addr VMSegBase(Seg seg)
{
  VMArena vmArena;
  VMArenaChunk chunk;
  Page page;
  Index i;
  
  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);
  chunk = VMArenaChunkOfSeg(vmArena, seg);

  page = PageOfSeg(seg);
  /* .improve.segbase.subtract: This subtractions is redundant */
  /* with the one in VMArenaChunkOfSeg.  CSE by hand? */

  i = page - chunk->pageTable;

  return PageIndexBase(chunk, i);
}


/* VMSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr VMSegLimit(Seg seg)
{
  VMArena vmArena;
  VMArenaChunk chunk;
  Page page;

  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);
  chunk = VMArenaChunkOfSeg(vmArena, seg);

  if(SegSingle(seg)) {
    return AddrAdd(VMSegBase(seg), chunk->pageSize);
  } else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* VMSegSize -- return the size (limit - base) of a segment
 */

static Size VMSegSize(Seg seg)
{
  VMArena vmArena;
  VMArenaChunk chunk;
  Page page;
  Index i;
  Addr base, limit;

  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);
  chunk = VMArenaChunkOfSeg(vmArena, seg);

  page = PageOfSeg(seg);
  i = page - chunk->pageTable;
  base = PageIndexBase(chunk, i);

  if(SegSingle(seg)) {
    limit = AddrAdd(VMSegBase(seg), chunk->pageSize);
  } else {
    page = PageOfSeg(seg);
    limit = PageTail(page+1)->limit;
  }

  return AddrOffset(base, limit);
}

static Bool VMArenaChunkOfAddr(VMArenaChunk *chunkReturn,
                               VMArena vmArena, Addr addr)

{
  Ring node, next;
  /* No checks because critical and internal */

  /* check cache first */
  if(vmArena->chunkCache.base <= addr &&
     addr < vmArena->chunkCache.limit) {
    *chunkReturn = vmArena->chunkCache.chunk;
    return TRUE;
  }
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      /* Gotcha! */
      VMArenaChunkEncache(vmArena, chunk);
      *chunkReturn = chunk;
      return TRUE;
    }
  }
  return FALSE;
}

/* VMSegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool VMSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  Bool b;
  Index i;
  VMArena vmArena;
  VMArenaChunk chunk;
  
  /* design.mps.trace.fix.noaver */
  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  vmArena = ArenaVMArena(arena);
  AVERT_CRITICAL(VMArena, vmArena);
  
  b = VMArenaChunkOfAddr(&chunk, vmArena, addr);
  if(!b)
    return FALSE;
  /* design.mps.trace.fix.segofaddr */
  i = INDEX_OF_ADDR(chunk, addr);
  /* .addr.free: If the page is recorded as being free then */
  /* either the page is free or it is */
  /* part of the arena tables (see .ullagepages) */
  if(BTGet(chunk->allocTable, i)) {
    Page page = &chunk->pageTable[i];

    if(PageIsHead(page)) {
      *segReturn = PageSeg(page);
    } else {
      AVER_CRITICAL(PageRest(page)->type == PageTypeTail);
      *segReturn = PageTail(page)->seg;
    }
    return TRUE;
  }
  
  return FALSE;
}

static Bool VMIsReservedAddr(Arena arena, Addr addr)
{
  VMArena vmArena;
  VMArenaChunk dummy;

  AVERT(Arena, arena);
  /* addr is arbitrary */

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return VMArenaChunkOfAddr(&dummy, vmArena, addr);
}


/* segSearchInChunk -- search for a segment
 *
 * .seg-search: Searches for a segment in the chunk starting at page
 * index i, return NULL if there is none.  A page is the first page
 * of a segment if it is marked allocated in the allocTable, and
 * its pool is not NULL.
 *
 * .seg-search.private: This function is private to this module and
 * is used in the segment iteration protocol (SegFirst and SegNext).
 */

static Bool segSearchInChunk(Seg *segReturn,
                             VMArenaChunk chunk, Index i)
{
  AVER(segReturn != NULL);
  AVERT(VMArenaChunk, chunk);
  AVER(chunk->ullagePages <= i);
  AVER(i <= chunk->pages);

  while(i < chunk->pages &&
        !(BTGet(chunk->allocTable, i) &&
          PageIsHead(&chunk->pageTable[i]))) {
    ++i;
  }

  if(i == chunk->pages)
    return FALSE;
  
  AVER(i < chunk->pages);
  
  *segReturn = PageSeg(&chunk->pageTable[i]);
  return TRUE;
}



/* VMNextChunkOfAddr
 *
 * Returns the next higher chunk in memory which does _not_
 * contain addr.
 *
 * IE the chunks are partitioned into 3 sets (some of which are empty):
 * The set of chunks < addr; the set of chunks containing addr (empty or
 * singleton), the set of chunk > addr.
 * Of the latter set, the chunk with least address is chosen.  FALSE
 * is returned if this set is empty.
 */
static Bool VMNextChunkOfAddr(VMArenaChunk *chunkReturn,
                              VMArena vmArena, Addr addr)
{
  Addr leastBase;
  VMArenaChunk leastChunk;
  Ring node, next;

  leastBase = (Addr)(Word)-1;
  leastChunk = NULL;
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    if(addr < chunk->base && chunk->base < leastBase) {
      leastBase = chunk->base;
      leastChunk = chunk;
    }
  }
  if(leastChunk != NULL) {
    *chunkReturn = leastChunk;
    return TRUE;
  }
  return FALSE;
}


/* segSearch (internal to impl.c.arenavm)
 *
 * Searches for the next segment in increasing address order.
 * The segment returned is the next one along from addr (ie
 * it has a base address bigger than addr and no other segment
 * with a base address bigger than addr has a smaller base address).
 *
 * Returns FALSE if there is no segment to find (end of the arena).
 */
static Bool segSearch(Seg *segReturn, VMArena vmArena, Addr addr)
{
  Bool b;
  VMArenaChunk chunk;

  b = VMArenaChunkOfAddr(&chunk, vmArena, addr);
  if(b) {
    Index i;

    i = indexOfAddr(chunk, addr);

    /* There are fewer pages than addresses, therefore the */
    /* page index can never wrap around */
    AVER_CRITICAL(i+1 != 0);

    if(segSearchInChunk(segReturn, chunk, i+1)) {
      return TRUE;
    }
  }
  while(VMNextChunkOfAddr(&chunk, vmArena, addr)) {
    addr = chunk->base;
    /* We start from ullagePages, as the ullage can't be a segment. */
    /* See .ullagepages */
    if(segSearchInChunk(segReturn, chunk, chunk->ullagePages)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* VMSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Bool VMSegFirst(Seg *segReturn, Arena arena)
{
  Bool b;
  VMArena vmArena;

  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  /* .segfirst.assume.nozero: We assume that there is no segment */
  /* with base address (Addr)0.  Happily this assumption is sound */
  /* for a number of reasons. */
  b = segSearch(segReturn, vmArena, (Addr)0);
  if(b) {
    return TRUE;
  }
  return FALSE;
}


/* VMSegNext -- return the "next" segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 *
 * VMSegNext finds the segment with the lowest base address which is
 * greater than a specified address.  The address must be (or must once
 * have been) the base address of a segment.
 */

static Bool VMSegNext(Seg *segReturn, Arena arena, Addr addr)
{
  Bool b;
  VMArena vmArena;

  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  vmArena = ArenaVMArena(arena);
  AVERT_CRITICAL(VMArena, vmArena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  b = segSearch(segReturn, vmArena, addr);
  if(b) {
    return TRUE;
  }
  return FALSE;
}


/* mps_arena_class_vm -- return the arena class VM */

static ArenaClassStruct ArenaClassVMStruct = {
  ArenaClassSig,
  "VM",                                 /* name */
  sizeof(VMArenaStruct),                /* size */
  offsetof(VMArenaStruct, arenaStruct), /* offset */
  VMArenaInit,                          /* init */
  VMArenaFinish,                        /* finish */
  VMArenaReserved,                      /* reserved */
  VMArenaCommitted,                     /* committed */
  VMArenaSpareCommitExceeded,
  ArenaNoExtend,                        /* extend */
  ArenaNoRetract,                       /* retract */
  VMIsReservedAddr,                     /* isReserved */
  VMSegAlloc,                           /* segAlloc */
  VMSegFree,                            /* segFree */
  VMSegBase,                            /* segBase */
  VMSegLimit,                           /* segLimit */
  VMSegSize,                            /* segSize */
  VMSegOfAddr,                          /* segOfAddr */
  VMSegFirst,                           /* segFirst */
  VMSegNext,                            /* segNext */
  ArenaTrivDescribe,                    /* describe */
  ArenaClassSig
};

static ArenaClassStruct ArenaClassVMNZStruct = {
  ArenaClassSig,
  "VMNZ",                               /* name */
  sizeof(VMArenaStruct),                /* size */
  offsetof(VMArenaStruct, arenaStruct), /* offset */
  VMArenaInit,                          /* init */
  VMArenaFinish,                        /* finish */
  VMArenaReserved,                      /* reserved */
  VMArenaCommitted,                     /* committed */
  VMArenaSpareCommitExceeded,
  ArenaNoExtend,                        /* extend */
  ArenaNoRetract,                       /* retract */
  VMIsReservedAddr,                     /* isReserved */
  VMNZSegAlloc,                         /* segAlloc */
  VMSegFree,                            /* segFree */
  VMSegBase,                            /* segBase */
  VMSegLimit,                           /* segLimit */
  VMSegSize,                            /* segSize */
  VMSegOfAddr,                          /* segOfAddr */
  VMSegFirst,                           /* segFirst */
  VMSegNext,                            /* segNext */
  ArenaTrivDescribe,                    /* describe */
  ArenaClassSig
};

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)&ArenaClassVMStruct;
}

/* VMNZ is just VMArena with a different segment allocation policy */
mps_arena_class_t mps_arena_class_vmnz(void)
{
  return (mps_arena_class_t)&ArenaClassVMNZStruct;
}
