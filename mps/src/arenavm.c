/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.48) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 *
 * See design.mps.arena.vm.
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
 * GC. (but it would change how findFreeInRefSet avoids allocating
 * over the tables, see .alloc.skip)@@@@
 */


#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.48) $");


typedef struct VMArenaStruct *VMArena;
typedef struct PageStruct *Page;

/* @@@@ Arbitrary calculation for the maximum number of distinct */
/* object sets for generations. */
/* .gencount.const: Must be a constant suitable for use as an */
/* array size. */
#define VMArenaGenCount ((Count)(MPS_WORD_WIDTH/2))


/* VMArenaStruct -- VM Arena Structure */

#define VMArenaChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMArenaChunkStruct {
  Sig sig;			/* design.mps.sig */
  VM vm;                        /* virtual memory handle */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  VMArena vmArena;		/* parent VMarena */
  RingStruct arenaRing;         /* ring of all chunks in arena */
  Bool primary;                 /* primary chunk contains vmArena */
  Bool inBoot;                  /* TRUE when in boot (used in checking) */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  BT allocTable;                /* page allocation table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
} VMArenaChunkStruct;

typedef struct VMArenaChunkStruct *VMArenaChunk;

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

typedef struct VMArenaChunkCacheEntryStruct *VMArenaChunkCacheEntry;

/* VMArenaStruct
 * See design.mps.arena.coop-vm.struct.vmarena for description of fields.
 */

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */
typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VMArenaChunk primary;
  RingStruct chunkRing;
  VMArenaChunkCacheEntryStruct chunkCache; /* just one entry */
  RefSet blacklist;             /* zones to use last */
  RefSet genRefSet[VMArenaGenCount]; /* .gencount.const */
  RefSet freeSet;               /* unassigned zones */
  Size extendBy;
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;


/* ArenaVMArena -- find the VMArena pointer given a generic Arena */

#define ArenaVMArena(arena) PARENT(VMArenaStruct, arenaStruct, (arena))


/* VMArenaArena -- find the generic Arena pointer given a VMArena */
/* .arena.check-free: Must not call (indirectly) VMArenaCheck */

#define VMArenaArena(VMArena) (&(VMArena)->arenaStruct)


/* SegVMArena -- find the VMArena given a segment */

#define SegVMArena(seg) ArenaVMArena(PoolArena(SegPool(seg)))


/* PageStruct -- page structure
 *
 * .page-table: The page table (defined as a PageStruct array)
 * is central to the design of the arena.
 * See design.mps.arena.vm.table.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union, so that it shares a common prefix with the
 * SegStruct.  See impl.h.mpmst.seg.pool.
 */

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct segStruct;         /* segment */
    struct {
      Pool pool;                 /* NULL, must be first field (.page) */
      Seg seg;                   /* segment at base page of run */
      Addr limit;                /* limit of segment */
    } tail;                      /* tail page */
  } the;
} PageStruct;


/* PageIndexBase -- map page index to base address of page
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageIndexBase(chunk, i) \
  AddrAdd((chunk)->base, ((i) << (chunk)->pageShift))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)


/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)


/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, (seg))


/* PageIsHead -- is a page a head (contains segment descriptor)?
 *
 * See design.mps.arena.vm.table.disc.
 */

#define PageIsHead(page)        (PageTail((page))->pool != NULL)


/* addrPageBase -- the base of the page this address is on */

#define addrPageBase(chunk, addr) \
  AddrAlignDown((addr), (chunk)->pageSize)

#define addrOfPageDesc(chunk, index) \
  ((Addr)&(chunk)->pageTable[index])


static Addr VMSegLimit(Seg seg);

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
  }
  CHECKL(RingCheck(&chunk->arenaRing));
  CHECKL(1uL << chunk->pageShift == chunk->pageSize);
  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check that the tables fit in the chunk */
  CHECKL(chunk->tablePages <= chunk->pages);
  /* check that the two notions of table size are consistent */
  CHECKL(chunk->tablesSize == chunk->tablePages << chunk->pageShift);
  CHECKL(chunk->pageTable != NULL);
  /* check that pageTable is in the chunk ... */
  CHECKL((Addr)chunk->pageTable >= chunk->base);
  /* and is in fact no bigger than we said it would be */
  CHECKL((Addr)&chunk->pageTable[chunk->pages] <=
         AddrAdd(chunk->base, chunk->tablesSize));
  /* check allocTable */
  CHECKL(chunk->allocTable != NULL);
  CHECKL((Addr)chunk->allocTable >= chunk->base);
  CHECKL(AddrAdd((Addr)chunk->allocTable, BTSize(chunk->pages)) <= 
	 AddrAdd(chunk->base, chunk->tablesSize));
  /* .improve.check-table: Could check the consistency of the tables. */
  CHECKL(chunk->pages == 
         AddrOffset(chunk->base, chunk->limit) >> 
         chunk->pageShift);
  return TRUE;
}

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
  /* Can't check the structure as we are supposed to be initializing it */
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
  void *blockbase, *blocklimit;  /* base, limit of candidate block */

  AVER(pReturn != NULL);
  AVERT(VMArenaBoot, boot);
  AVER(size > 0);
  AVER(AlignCheck((Align)align));

  /* Align alloc pointer up and bounds check. */
  /* There's no PointerAlignUp, so we use AddrAlignUp @@@@ */
  /* .vm.addr-is-star: In this file, Addr is compatible with C */
  /* pointers. */
  blockbase = (void *)AddrAlignUp((Addr)boot->alloc, (Align)align);
  if(boot->limit <= blockbase || blockbase < boot->alloc) {
    return ResMEMORY;
  }
  blocklimit = PointerAdd(blockbase, size);
  if(boot->limit < blocklimit || blocklimit <= boot->alloc) {
    return ResMEMORY;
  }

  /* Fits!  So allocate it */
  boot->alloc = blocklimit;
  *pReturn = blockbase;
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
 * .chunkcreate.arg.vmarena: vmArena, the VMArena which will use this chunk.
 * .chunkcreate.arg.size: size, approximate amount of virtual address
 * that the chunk should reserve.  In practice this will be rounded up
 * to a page size (by the VM).
 * .chunkcreate.arg.sparesize: spareSize, the requested size of a block
 * which should be allocated in the chunk to be used by the caller.
 * Can be specified as 0 to indicate no block desired.
 */
static Res VMArenaChunkCreate(VMArenaChunk *chunkReturn, void **spareReturn,
                              Bool primary, VMArena vmArena,
			      Size size, size_t spareSize)
{
  Res res;
  Size pageTableSize;
  Size vmSize;
  VM vm;
  VMArenaBootStruct bootStruct;
  VMArenaBoot boot = &bootStruct;
  VMArenaChunk chunk;
  VMArenaChunkStruct initChunkStruct;
  VMArenaChunk initChunk = &initChunkStruct;
  void *p;
  void *spare = NULL;

  AVER(chunkReturn != NULL);
  AVER((spareReturn == NULL) == (spareSize == 0));
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

  initChunk->vm = vm;
  initChunk->pageSize = VMAlign(vm);
  initChunk->pageShift = SizeLog2(initChunk->pageSize);
  initChunk->primary = primary;
  initChunk->inBoot = primary;
  initChunk->vmArena = vmArena;
  /* Can't initialise ring yet as it will create pointers to the */
  /* wrong structure (the current copy on the stack, rather than */
  /* the to the ultimate copy) */
  initChunk->base = VMBase(vm);
  initChunk->limit = VMLimit(vm);
  /* the VM will have aligned the userSize, so pick up the actual size */
  vmSize = AddrOffset(initChunk->base, initChunk->limit);
  initChunk->pages = vmSize >> initChunk->pageShift;

  /* Allocate and map the descriptor and tables.  See */
  /* design.mps.arena.coop-vm.chunk.create.tables */

  res = VMArenaBootInit(boot,
                        (void *)initChunk->base,
                        (void *)initChunk->limit);
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
			 (size_t)BTSize(initChunk->pages), MPS_PF_ALIGN);
  if(res != ResOK)
    goto failAllocTable;
  initChunk->allocTable = p;
  pageTableSize = SizeAlignUp(initChunk->pages * sizeof(PageStruct),
                              initChunk->pageSize);
  res = VMArenaBootAlloc(&p, boot,
                         (size_t)pageTableSize, (size_t)initChunk->pageSize);
  if(res != ResOK)
    goto failAllocPageTable;
  initChunk->pageTable = p;
  initChunk->tablesSize = VMArenaBootAllocated(boot);
  res = VMArenaBootFinish(boot);
  if(res != ResOK)
    goto failBootFinish;

  /* Actually commit the necessary addresses. */
  /* design.mps.arena.coop-vm.chunk.create.tables.map */
  res = VMMap(vm, initChunk->base, (Addr)initChunk->pageTable);
  if(res != ResOK)
    goto failTableMap;

  /* Copy the structure to its ultimate destination. */
  /* design.mps.arena.coop-vm.chunk.create.copy */
  *chunk = *initChunk;
  RingInit(&chunk->arenaRing);

  /* .tablepages: pages whose page index is < tablePages are */
  /* recorded as free but never allocated as alloc starts */
  /* searching after the tables (see .alloc.skip).  SegOfAddr */
  /* uses the fact that these pages are marked as free in order */
  /* to detect "references" to these pages as  being bogus see */
  /* .addr.free. */

  /* chunk->tablesSize is expected to be page aligned */
  AVER((chunk->tablesSize >> chunk->pageShift) << chunk->pageShift ==
       chunk->tablesSize);
  chunk->tablePages = chunk->tablesSize >> chunk->pageShift;
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
failAllocTable:
failAllocSpare:
failAllocChunk:
failBootInit:
  VMDestroy(vm);
failVMCreate:
  return res;
}

static void VMArenaChunkDestroy(VMArenaChunk chunk)
{
  VM vm;

  /* Can't check chunk during destroy as its parent vmArena is invalid */
  AVER(chunk->sig == VMArenaChunkSig);

  chunk->sig = SigInvalid;
  RingFinish(&chunk->arenaRing);
  vm = chunk->vm;
  /* unmap the permanently mapped tables */
  VMUnmap(vm, chunk->base, (Addr)chunk->pageTable);
  VMDestroy(vm);
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

static Res VMArenaInit(Arena *arenaReturn, va_list args)
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

  arena = VMArenaArena(vmArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, (ArenaClass)mps_arena_class_vm());

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
  /* design.mps.arena.coop-vm.struct.vmarena.extendby.init */
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
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaCommitted(Arena arena)
{
  Size committed;
  Ring node, next;
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  committed = 0;
  RING_FOR(node, &vmArena->chunkRing, next) {
    VMArenaChunk chunk = RING_ELT(VMArenaChunk, arenaRing, node);
    committed += VMMapped(chunk->vm);
  }

  return committed;
}


/* Page Table Partial Mapping
 *
 * Some helper functions */


/* tablePageBaseIndex -- index of the first page descriptor falling
 *                       (at least partially) on this table page
 *
 * .repr.table-page: Table pages are passed as the base address of the page.
 *
 * .division: We calculate it by dividing the offset from the beginning
 * of the page table by the size of a table element.  This relies on
 * .vm.addr-is-star.
 */

#define tablePageBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
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


/* tablePageInUse -- check whether a given page of the page table is in use
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if any
 * of the page descriptors falling on it (even partially) are being used.
 * Relies on .repr.table-page and .vm.addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're (de)allocating.
 */

static Bool tablePageInUse(VMArenaChunk chunk, Addr tablePage)
{
  AVERT(VMArenaChunk, chunk);
  /* Check it's in the page table. */
  AVER((Addr)&chunk->pageTable[0] <= tablePage);
  AVER(tablePage < (Addr)&chunk->pageTable[chunk->pages]);

  return !BTIsResRange(chunk->allocTable,
		       tablePageBaseIndex(chunk, tablePage),
		       tablePageLimitIndex(chunk, tablePage));
}


/* unusedTablePages -- find any unused pages occupied by the descriptors given
 *
 * .unused: The caller guarantees the pages between baseIndex and
 * limitIndex are free, so those descriptors aren't being used.
 * .used.first-and-last: Since the descriptors given are not being used
 * at the moment, only the first page and the last page could be
 * partially used, the rest (if any) can be assumed to be unused.
 */

static Bool unusedTablePages(Addr *pagesBaseReturn, Addr *pagesLimitReturn,
			     VMArenaChunk chunk,
			     Index baseIndex, Index limitIndex)
{
  Addr firstPageBase, lastPageBase, pagesBase, pagesLimit;

  AVERT(VMArenaChunk, chunk);
  AVER(baseIndex < limitIndex && limitIndex <= chunk->pages);
  AVER(BTIsResRange(chunk->allocTable, baseIndex, limitIndex));
  AVER(pagesBaseReturn != NULL);
  AVER(pagesLimitReturn != NULL);

  /* firstPageBase is the base address of the table page that contains the */
  /* (first byte of the) page descriptor for baseIndex. */
  firstPageBase = addrPageBase(chunk, addrOfPageDesc(chunk, baseIndex));

  /* lastPageBase is the base address of the table page that contains the */
  /* (last byte of the) page descriptor for the page before limitIndex. */
  lastPageBase = addrPageBase(chunk,
			      AddrAdd(addrOfPageDesc(chunk, limitIndex-1),
				      sizeof(PageStruct) - 1));

  /* If there is only one page involved, just check whether it is */
  /* used.  This is the common case, since it's unlikely that */
  /* many page descriptors will be allocated or freed at once. */
  if(firstPageBase == lastPageBase) {
    if(tablePageInUse(chunk, firstPageBase)) {
      return FALSE;
    } else {
      *pagesBaseReturn = firstPageBase;
      *pagesLimitReturn = AddrAdd(firstPageBase, chunk->pageSize);
      return TRUE;
    }
  }

  /* If the page containing the page descriptor for baseIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(chunk, firstPageBase)) {
    pagesBase = AddrAdd(firstPageBase, chunk->pageSize);
  } else {
    pagesBase = firstPageBase;
  }

  /* If the page containing the page descriptor for limitIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(chunk, lastPageBase)) {
    pagesLimit = lastPageBase;
  } else {
    pagesLimit = AddrAdd(lastPageBase, chunk->pageSize);
  }

  /* If the pages were adjacent, and both excluded, then there */
  /* is nothing left. */
  if (pagesBase == pagesLimit) {
    return FALSE;
  } else {
    *pagesBaseReturn = pagesBase;
    *pagesLimitReturn = pagesLimit;
    return TRUE;
  }
}


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
  Index basePage, limitPage;	/* Index equiv. to base and limit */
  Index start, end;		/* base and limit of free run */

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


/* findFreeInRefSet -- try to allocate a segment with a RefSet
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

static Bool findFreeInRefSet(Index *baseReturn, VMArenaChunk *chunkReturn,
			     VMArena vmArena, Size size, RefSet refSet,
			     Bool downwards)
{
  Arena arena;
  Addr chunkBase, base, limit;
  Size zoneSize;
  Ring node, next;

  AVER(baseReturn != NULL);
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
    chunkBase = PageIndexBase(chunk, chunk->tablePages);

    base = chunkBase;
    while(base < chunk->limit) {
      if(RefSetIsMember(arena, refSet, base)) {
	/* Search for a run of zone stripes which are in the RefSet and */
	/* the arena.  Adding the zoneSize might wrap round (to zero, */
	/* because limit is aligned to zoneSize, which is a power of two). */
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
	/* Adding the zoneSize might wrap round (to zero, because base */
	/* is aligned to zoneSize, which is a power of two). */
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
                      VMArena vmArena, SegPref pref, Size size, Bool barge)
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
  /* (say 4 long) of RefSets and tries findFreeInRefSet on */
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
    /* Note that each is a superset of the previous, unless blacklisted */
    /* zones have been allocated (or the default is used). */
    if(findFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
		        RefSetDiff(refSet, vmArena->blacklist),
		        pref->high) ||
       findFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
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
    if(findFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
		        RefSetDiff(RefSetUNIV, vmArena->blacklist),
		        pref->high) ||
       findFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
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
    /* Note that each is a superset of the previous, unless blacklisted */
    /* zones have been allocated. */
    if(findFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
			 RefSetInter(refSet, vmArena->blacklist),
			 pref->high) ||
       findFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
			 refSet, pref->high) ||
       findFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
			 RefSetUnion(refSet, vmArena->blacklist),
			 pref->high) ||
       findFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
			 RefSetUNIV, pref->high)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* VMSegAlloc -- allocate a segment from the arena */

static Res VMSegAlloc(Seg *segReturn, SegPref pref, Size size,
		      Pool pool)
{
  Addr addr, unmappedPagesBase, unmappedPagesLimit;
  Arena arena;
  Index i, pages, base;
  RefSet segRefSet;
  Res res;
  Seg seg;
  VMArena vmArena;
  VMArenaChunk chunk;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  /* Assume all chunks have same pageSize */
  AVER(SizeIsAligned(size, vmArena->primary->pageSize));
  
  /* NULL is used as a discriminator */
  /* (see design.mps.arena.vm.table.disc) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  if(!VMSegFind(&base, &chunk, vmArena, pref, size, FALSE)) {
    VMArenaChunk newChunk;
    Size chunkSize;
    chunkSize = vmArena->extendBy + size;
    res = VMArenaChunkCreate(&newChunk, NULL /* spare */,
			     FALSE /* primary */, vmArena,
			     chunkSize, 0 /* spareSize */);
    if(res != ResOK) {
      /* We could trim chunkSize down to size and try again (but */
      /* don't). */
      return res;
    }
    RingAppend(&vmArena->chunkRing, &newChunk->arenaRing);
    if(!VMSegFind(&base, &chunk, vmArena, pref, size, TRUE)) {
      /* even with new chunk didn't work... */
      /* @@@@ .improve.debug: If the tables of the new chunk */
      /* were more than vmArena->extendBy then we will have failed */
      /* to allocate the seg anyway. */
      /* .improve.alloc-fail: This could be because the request was */
      /* too large, or perhaps the arena is fragmented.  We could return a */
      /* more meaningful code. */
      return ResRESOURCE;
    }
  }

  /* chunk (and base) should be initialised by VMSegFind */
  AVERT(VMArenaChunk, chunk);

  /* Test commit limit */
  /* Assumes VMArenaCommitted will increase by size after the call */
  /* to VMMap. */
  if(VMMapped(chunk->vm) + size > arena->commitLimit) {
    return ResCOMMIT_LIMIT;
  }

  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, so that we can exit straight away if */
  /* we fail. */
  addr = PageIndexBase(chunk, base);
  res = VMMap(chunk->vm, addr, AddrAdd(addr, size));
  if(res != ResOK)
    goto failSegMap;

  /* Compute number of pages to be allocated. */
  pages = size >> chunk->pageShift;

  /* Ensure that the page descriptors we need are on mapped pages. */
  if(unusedTablePages(&unmappedPagesBase, &unmappedPagesLimit,
		      chunk, base, base + pages)) {
    /* test commit limit */
    /* Assumes VMArenaCommitted will increase by unmappedPagesLimit - */
    /* unmappedPagesBase after the call to VMMap */
    if(VMMapped(chunk->vm) +
       AddrOffset(unmappedPagesBase, unmappedPagesLimit) >
       arena->commitLimit) {
      res = ResCOMMIT_LIMIT;
      goto failTableMap;
    }
    res = VMMap(chunk->vm, unmappedPagesBase, unmappedPagesLimit);
    if(res != ResOK)
      goto failTableMap;
  }

  /* Initialize the generic segment structure. */
  seg = PageSeg(&chunk->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(!BTGet(chunk->allocTable, base));
  BTSet(chunk->allocTable, base);
  if(pages > 1) {
    Addr limit = PageIndexBase(chunk, base + pages);

    SegSetSingle(seg, FALSE);
    for(i = base + 1; i < base + pages; ++i) {
      AVER(!BTGet(chunk->allocTable, i));
      BTSet(chunk->allocTable, i);
      PageTail(&chunk->pageTable[i])->pool = NULL;
      PageTail(&chunk->pageTable[i])->seg = seg;
      PageTail(&chunk->pageTable[i])->limit = limit;
    }
  } else {
    SegSetSingle(seg, TRUE);
  }

  segRefSet = RefSetOfSeg(arena, seg);

  if(pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    vmArena->genRefSet[gen] = 
      RefSetUnion(vmArena->genRefSet[gen], segRefSet);
  }

  vmArena->freeSet = RefSetDiff(vmArena->freeSet, segRefSet);
  
  AVERT(Seg, seg);
  
  EVENT_PPAWP(SegAlloc, vmArena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;

failTableMap:
  VMUnmap(chunk->vm, addr, AddrAdd(addr, size));
failSegMap:
  return res;
}

static VMArenaChunk VMArenaChunkOfSeg(VMArena vmArena, Seg seg)
{
  Ring node, next;
  /* critcal because used from critical functions */
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


/* VMSegFree - free a segment in the arena
 */

static void VMSegFree(Seg seg)
{
  VMArena vmArena;
  VMArenaChunk chunk;
  Page page;
  Count pages;
  Index basePage;
  Addr base, limit, unusedPagesBase, unusedPagesLimit;

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
  VMUnmap(chunk->vm, base, limit);

  /* Calculate the number of pages in the segment */
  pages = AddrOffset(base, limit) >> chunk->pageShift;

  /* There shouldn't be any pages marked free within the segment's */
  /* area of the alloc table. */
  AVER(BTIsSetRange(chunk->allocTable, basePage, basePage + pages));
  BTResRange(chunk->allocTable, basePage, basePage + pages);

  /* Unmap any pages that became unused in the page table */
  if(unusedTablePages(&unusedPagesBase, &unusedPagesLimit,
		      chunk, basePage, basePage + pages))
    VMUnmap(chunk->vm, unusedPagesBase, unusedPagesLimit);

  EVENT_PP(SegFree, vmArena, seg);
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
  /* part of the arena tables (see .tablepages) */
  if(BTGet(chunk->allocTable, i)) {
    Page page = &chunk->pageTable[i];

    if(PageIsHead(page))
      *segReturn = PageSeg(page);
    else
      *segReturn = PageTail(page)->seg;
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


/* segSearch -- search for a segment
 *
 * .seg-search: Searches for a segment in the arena starting at page
 * index i, return NULL if there is none.  A page is the first page
 * of a segment if it is marked allocated in the allocTable, and
 * its pool is not NULL.
 *
 * .seg-search.private: This function is private to this module and
 * is used in the segment iteration protocol (SegFirst and SegNext).
 */

static Bool segSearch(Seg *segReturn, VMArenaChunk chunk, Index i)
{
  AVER(segReturn != NULL);
  AVERT(VMArenaChunk, chunk);
  AVER(chunk->tablePages <= i);
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


/* VMSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Bool VMSegFirst(Seg *segReturn, Arena arena)
{
  Bool b;
  VMArena vmArena;
  VMArenaChunk chunk;

  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  b = VMNextChunkOfAddr(&chunk, vmArena, (Addr)0);
  /* There's at least one chunk, so we must have found something */
  AVER(b);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return segSearch(segReturn, chunk, chunk->tablePages);
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
  VMArena vmArena;
  VMArenaChunk chunk;
  Bool b;
  Seg seg;

  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  vmArena = ArenaVMArena(arena);
  AVERT_CRITICAL(VMArena, vmArena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  b = VMArenaChunkOfAddr(&chunk, vmArena, addr);
  if(b) {
    Index i;

    i = indexOfAddr(chunk, addr);

    /* There are fewer pages than addresses, therefore the */
    /* page index can never wrap around */
    AVER_CRITICAL(i+1 != 0);

    if(segSearch(&seg, chunk, i+1)) {
      *segReturn = seg;
      return TRUE;
    }
  }
  while(VMNextChunkOfAddr(&chunk, vmArena, addr)) {
    addr = chunk->base;
    /* We start from tablePages, as the tables can't be a segment. */
    /* See .tablepages */
    if(segSearch(&seg, chunk, chunk->tablePages)) {
      *segReturn = seg;
      return TRUE;
    }
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

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)&ArenaClassVMStruct;
}
