/* arenavm.c: VIRTUAL MEMORY ARENA CLASS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * .design: See <design/arenavm/>, and <design/arena/#coop-vm>
 *
 * .vm.addr-is-star: In this file, Addr is compatible with C
 * pointers, and Count with size_t (Index), because all refer to the
 * virtual address space.
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC.  (But it would change how pagesFindFreeInZones avoids allocating
 * over the tables, see .alloc.skip.)
 */

#include "boot.h"
#include "tract.h"
#include "bt.h"
#include "sa.h"
#include "mpm.h"
#include "mpsavm.h"
#include "cbs.h"
#include "poolmfs.h"

SRCID(arenavm, "$Id$");


/* VMChunk -- chunks for VM arenas */

typedef struct VMChunkStruct *VMChunk;

#define VMChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMChunkStruct {
  ChunkStruct chunkStruct;      /* generic chunk */
  VM vm;                        /* virtual memory handle */
  Addr overheadMappedLimit;     /* limit of pages mapped for overhead */
  SparseArrayStruct pages;      /* to manage backing store of page table */
  Sig sig;                      /* <design/sig/> */
} VMChunkStruct;

#define VMChunk2Chunk(vmchunk) (&(vmchunk)->chunkStruct)
#define Chunk2VMChunk(chunk) PARENT(VMChunkStruct, chunkStruct, chunk)


/* VMChunkVMArena -- get the VM arena from a VM chunk */

#define VMChunkVMArena(vmchunk) \
  Arena2VMArena(ChunkArena(VMChunk2Chunk(vmchunk)))


/* VMArena
 *
 * See <design/arena/#coop-vm.struct.vmarena> for description.
 */

typedef struct VMArenaStruct *VMArena;

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VM vm;                        /* VM where the arena itself is stored */
  char vmParams[VMParamSize];   /* VM parameter block */
  Size spareSize;              /* total size of spare pages */
  Size extendBy;                /* desired arena increment */
  Size extendMin;               /* minimum arena increment */
  ArenaVMExtendedCallback extended;
  ArenaVMContractedCallback contracted;
  RingStruct spareRing;         /* spare (free but mapped) tracts */
  Sig sig;                      /* <design/sig/> */
} VMArenaStruct;

#define Arena2VMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)
#define VMArena2Arena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static Size VMPurgeSpare(Arena arena, Size size);
static void chunkUnmapSpare(Chunk chunk);
extern ArenaClass VMArenaClassGet(void);
static void VMCompact(Arena arena, Trace trace);


/* VMChunkCheck -- check the consistency of a VM chunk */

static Bool VMChunkCheck(VMChunk vmchunk)
{
  Chunk chunk;

  CHECKS(VMChunk, vmchunk);
  chunk = VMChunk2Chunk(vmchunk);
  CHECKL(ChunkCheck(chunk));
  CHECKL(VMCheck(vmchunk->vm));
  CHECKL(VMAlign(vmchunk->vm) == ChunkPageSize(chunk));
  CHECKL(vmchunk->overheadMappedLimit <= (Addr)chunk->pageTable);
  CHECKD(SparseArray, &vmchunk->pages);
  /* SparseArrayCheck is agnostic about where the BTs live, so VMChunkCheck
     makes sure they're where they're expected to be (in the chunk). */
  CHECKL(chunk->base < (Addr)vmchunk->pages.mapped);
  CHECKL(AddrAdd(vmchunk->pages.mapped, BTSize(chunk->pages)) <=
         vmchunk->overheadMappedLimit);
  CHECKL(chunk->base < (Addr)vmchunk->pages.pages);
  CHECKL(AddrAdd(vmchunk->pages.pages, BTSize(chunk->pageTablePages)) <=
         vmchunk->overheadMappedLimit);
  /* .improve.check-table: Could check the consistency of the tables. */
  
  return TRUE;
}


/* addrOfPageDesc -- address of the page descriptor (as an Addr) */

#define addrOfPageDesc(chunk, index) \
  ((Addr)&(chunk)->pageTable[index])


/* PageTablePageIndex
 *
 * Maps from a page base address for a page occupied by the page table
 * to the index of that page in the range of pages occupied by the
 * page table.  So that
 *   PageTablePageIndex(chunk, (Addr)chunk->pageTable) == 0
 * and
 *   PageTablePageIndex(chunk,
 *                      AddrAlignUp(addrOfPageDesc(chunk->pages), pageSize)
 *     == chunk->pageTablePages
 */
#define PageTablePageIndex(chunk, pageAddr) \
  ChunkSizeToPages(chunk, AddrOffset((Addr)(chunk)->pageTable, pageAddr))


/* TablePageIndexBase
 *
 * Takes a page table page index (i.e., the index of a page occupied
 * by the page table, where the page occupied by chunk->pageTable is
 * index 0) and returns the base address of that page.
 * (Reverse of mapping defined by PageTablePageIndex.)
 */
#define TablePageIndexBase(chunk, index) \
  AddrAdd((Addr)(chunk)->pageTable, ChunkPagesToSize(chunk, index))


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  Arena arena;
  VMChunk primary;

  CHECKS(VMArena, vmArena);
  arena = VMArena2Arena(vmArena);
  CHECKD(Arena, arena);
  /* spare pages are committed, so must be less spare than committed. */
  CHECKL(vmArena->spareSize <= arena->committed);

  CHECKL(vmArena->extendBy > 0);
  CHECKL(vmArena->extendMin <= vmArena->extendBy);

  if (arena->primary != NULL) {
    primary = Chunk2VMChunk(arena->primary);
    CHECKD(VMChunk, primary);
    /* We could iterate over all chunks accumulating an accurate */
    /* count of committed, but we don't have all day. */
    CHECKL(VMMapped(primary->vm) <= arena->committed);
  }
  
  CHECKL(RingCheck(&vmArena->spareRing));

  /* FIXME: Can't check VMParams */

  return TRUE;
}


/* VMArenaDescribe -- describe the VMArena
 */
static Res VMArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  VMArena vmArena;

  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  vmArena = Arena2VMArena(arena);
  if (!TESTT(VMArena, vmArena)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  /* ...but the next method is ArenaTrivDescribe, so don't call it;
   * see impl.c.arena#describe.triv.dont-upcall.
   *
  super = ARENA_SUPERCLASS(VMArenaClass);
  res = super->describe(arena, stream);
  if (res != ResOK) return res;
   *
  */

  res = WriteF(stream,
               "  spareSize:     $U\n", (WriteFU)vmArena->spareSize,
               NULL);
  if(res != ResOK)
    return res;

  /* (incomplete: some fields are not Described) */

  return ResOK;
}



/* VM indirect functions
 *
 * These functions should be used to map and unmap within the arena.
 * They are responsible for maintaining vmArena->committed, and for
 * checking that the commit limit does not get exceeded.
 */
static Res vmArenaMap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Arena arena;
  Size size;
  Res res;

  /* no checking as function is local to module */

  arena = VMArena2Arena(vmArena);
  size = AddrOffset(base, limit);
  /* committed can't overflow (since we can't commit more memory than */
  /* address space), but we're paranoid. */
  AVER(arena->committed < arena->committed + size);
  /* check against commit limit */
  if (arena->commitLimit < arena->committed + size)
    return ResCOMMIT_LIMIT;

  res = VMMap(vm, base, limit);
  if (res != ResOK)
    return res;
  arena->committed += size;
  return ResOK;
}


static void vmArenaUnmap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Arena arena;
  Size size;

  /* no checking as function is local to module */

  arena = VMArena2Arena(vmArena);
  size = AddrOffset(base, limit);
  AVER(size <= arena->committed);

  VMUnmap(vm, base, limit);
  arena->committed -= size;
  return;
}


/* VMChunkCreate -- create a chunk
 *
 * chunkReturn, return parameter for the created chunk.
 * vmArena, the parent VMArena.
 * size, approximate amount of virtual address that the chunk should reserve.
 */
static Res VMChunkCreate(Chunk *chunkReturn, VMArena vmArena, Size size)
{
  Res res;
  Addr base, limit, chunkStructLimit;
  Align pageSize;
  VM vm;
  BootBlockStruct bootStruct;
  BootBlock boot = &bootStruct;
  VMChunk vmChunk;
  void *p;

  AVER(chunkReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);

  res = VMCreate(&vm, size, vmArena->vmParams);
  if (res != ResOK)
    goto failVMCreate;

  pageSize = VMAlign(vm);
  /* The VM will have aligned the userSize; pick up the actual size. */
  base = VMBase(vm);
  limit = VMLimit(vm);

  res = BootBlockInit(boot, (void *)base, (void *)limit);
  if (res != ResOK)
    goto failBootInit;

  /* Allocate and map the descriptor. */
  /* See <design/arena/>.@@@@ */
  res = BootAlloc(&p, boot, sizeof(VMChunkStruct), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failChunkAlloc;
  vmChunk = p;
  /* Calculate the limit of the page where the chunkStruct resides. */
  chunkStructLimit = AddrAlignUp((Addr)(vmChunk + 1), pageSize);
  res = vmArenaMap(vmArena, vm, base, chunkStructLimit);
  if (res != ResOK)
    goto failChunkMap;
  vmChunk->overheadMappedLimit = chunkStructLimit;

  vmChunk->vm = vm;
  res = ChunkInit(VMChunk2Chunk(vmChunk), VMArena2Arena(vmArena),
                  base, limit, pageSize, boot);
  if (res != ResOK)
    goto failChunkInit;

  BootBlockFinish(boot);

  vmChunk->sig = VMChunkSig;
  AVERT(VMChunk, vmChunk);

  *chunkReturn = VMChunk2Chunk(vmChunk);
  return ResOK;

failChunkInit:
  /* No need to unmap, as we're destroying the VM. */
failChunkMap:
failChunkAlloc:
failBootInit:
  VMDestroy(vm);
failVMCreate:
  return res;
}


/* VMChunkInit -- initialize a VMChunk */

static Res VMChunkInit(Chunk chunk, BootBlock boot)
{
  VMChunk vmChunk;
  Addr overheadLimit;
  void *p;
  Res res;
  BT saMapped, saPages;

  /* chunk is supposed to be uninitialized, so don't check it. */
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(BootBlock, boot);
  
  res = BootAlloc(&p, boot, BTSize(chunk->pages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failSaMapped;
  saMapped = p;
  
  res = BootAlloc(&p, boot, BTSize(chunk->pageTablePages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failSaPages;
  saPages = p;
  
  overheadLimit = AddrAdd(chunk->base, (Size)BootAllocated(boot));

  /* Put the page table as late as possible, as in VM systems we don't want */
  /* to map it. */
  res = BootAlloc(&p, boot, chunk->pageTablePages << chunk->pageShift, chunk->pageSize);
  if (res != ResOK)
    goto failAllocPageTable;
  chunk->pageTable = p;

  /* Map memory for the bit tables. */
  if (vmChunk->overheadMappedLimit < overheadLimit) {
    overheadLimit = AddrAlignUp(overheadLimit, ChunkPageSize(chunk));
    res = vmArenaMap(VMChunkVMArena(vmChunk), vmChunk->vm,
                     vmChunk->overheadMappedLimit, overheadLimit);
    if (res != ResOK)
      goto failTableMap;
    vmChunk->overheadMappedLimit = overheadLimit;
  }

  SparseArrayInit(&vmChunk->pages,
                  chunk->pageTable,
                  sizeof(PageUnion),
                  chunk->pages,
                  saMapped, saPages, vmChunk->vm);

  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failSaPages:
failAllocPageTable:
failSaMapped:
  return res;
}


/* vmChunkDestroy -- destroy a VMChunk */

static void vmChunkDestroy(Chunk chunk)
{
  VM vm;
  VMChunk vmChunk;

  AVERT(Chunk, chunk);
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(VMChunk, vmChunk);
  
  chunkUnmapSpare(chunk);
  
  SparseArrayFinish(&vmChunk->pages);
  
  vmChunk->sig = SigInvalid;
  vm = vmChunk->vm;
  ChunkFinish(chunk);
  VMDestroy(vm);
}


/* VMChunkFinish -- finish a VMChunk */

static void VMChunkFinish(Chunk chunk)
{
  VMChunk vmChunk = Chunk2VMChunk(chunk);

  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               VMBase(vmChunk->vm), vmChunk->overheadMappedLimit);
  /* No point in finishing the other fields, since they are unmapped. */
}


/* VMArenaVarargs -- parse obsolete varargs */

static void VMArenaVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_ARENA_SIZE;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_ARGS_END;
  AVER(ArgListCheck(args));
}


/* VMArenaTrivExtended -- trivial callback for VM arena extension */

static void vmArenaTrivExtended(Arena arena, Addr base, Size size)
{
  AVERT(Arena, arena);
  AVER(base != 0);
  AVER(size > 0);
  UNUSED(arena);
  UNUSED(base);
  UNUSED(size);
}

/* VMArenaTrivContracted -- trivial callback for VM arena contraction */

static void vmArenaTrivContracted(Arena arena, Addr base, Size size)
{
  AVERT(Arena, arena);
  AVER(base != 0);
  AVER(size > 0);
  UNUSED(arena);
  UNUSED(base);
  UNUSED(size);
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

ARG_DEFINE_KEY(arena_extended, Fun);
#define vmKeyArenaExtended (&_mps_key_arena_extended)
ARG_DEFINE_KEY(arena_contracted, Fun);
#define vmKeyArenaContracted (&_mps_key_arena_contracted)

static Res VMArenaInit(Arena *arenaReturn, ArenaClass class, ArgList args)
{
  Size userSize;        /* size requested by user */
  Size chunkSize;       /* size actually created */
  Size vmArenaSize; /* aligned size of VMArenaStruct */
  Res res;
  VMArena vmArena;
  Arena arena;
  VM arenaVM;
  Chunk chunk;
  mps_arg_s arg;
  char vmParams[VMParamSize];
  
  AVER(arenaReturn != NULL);
  AVER(class == VMArenaClassGet());
  AVER(ArgListCheck(args));

  ArgRequire(&arg, args, MPS_KEY_ARENA_SIZE);
  userSize = arg.val.size;

  AVER(userSize > 0);
  
  /* Parse the arguments into VM parameters, if any.  We must do this into
     some stack-allocated memory for the moment, since we don't have anywhere
     else to put it.  It gets copied later. */
  res = VMParamFromArgs(vmParams, sizeof(vmParams), args);
  if (res != ResOK)
    goto failVMCreate;

  /* Create a VM to hold the arena and map it. */
  vmArenaSize = SizeAlignUp(sizeof(VMArenaStruct), MPS_PF_ALIGN);
  res = VMCreate(&arenaVM, vmArenaSize, vmParams);
  if (res != ResOK)
    goto failVMCreate;
  res = VMMap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  if (res != ResOK)
    goto failVMMap;
  vmArena = (VMArena)VMBase(arenaVM);

  arena = VMArena2Arena(vmArena);
  /* <code/arena.c#init.caller> */
  res = ArenaInit(arena, class, VMAlign(arenaVM), args);
  if (res != ResOK)
    goto failArenaInit;
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->spareSize = 0;
  RingInit(&vmArena->spareRing);

  /* Copy the stack-allocated VM parameters into their home in the VMArena. */
  AVER(sizeof(vmArena->vmParams) == sizeof(vmParams));
  mps_lib_memcpy(vmArena->vmParams, vmParams, sizeof(vmArena->vmParams));

  /* <design/arena/#coop-vm.struct.vmarena.extendby.init> */
  vmArena->extendBy = userSize;
  vmArena->extendMin = 0;

  vmArena->extended = vmArenaTrivExtended;
  if (ArgPick(&arg, args, vmKeyArenaExtended))
    vmArena->extended = (ArenaVMExtendedCallback)arg.val.fun;

  vmArena->contracted = vmArenaTrivContracted;
  if (ArgPick(&arg, args, vmKeyArenaContracted))
    vmArena->contracted = (ArenaVMContractedCallback)arg.val.fun;

  /* have to have a valid arena before calling ChunkCreate */
  vmArena->sig = VMArenaSig;
  res = VMChunkCreate(&chunk, vmArena, userSize);
  if (res != ResOK)
    goto failChunkCreate;

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of stripes as will fit into a reference set (the number of */
  /* bits in a word).  Fail if the chunk is so small stripes are smaller */
  /* than pages.  Note that some zones are discontiguous in the chunk if */
  /* the size is not a power of 2.  See <design/arena/#class.fields>. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);
  AVER(chunk->pageSize == arena->alignment);

  AVERT(VMArena, vmArena);
  if ((ArenaClass)mps_arena_class_vm() == class)
    EVENT3(ArenaCreateVM, arena, userSize, chunkSize);
  else
    EVENT3(ArenaCreateVMNZ, arena, userSize, chunkSize);

  vmArena->extended(arena, chunk->base, chunkSize);
  
  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
  ArenaFinish(arena);
failArenaInit:
  VMUnmap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
failVMMap:
  VMDestroy(arenaVM);
failVMCreate:
  return res;
}


/* VMArenaFinish -- finish the arena */

static void VMArenaFinish(Arena arena)
{
  VMArena vmArena;
  Ring node, next;
  VM arenaVM;

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  arenaVM = vmArena->vm;

  /* destroy all chunks, including the primary */
  arena->primary = NULL;
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    vmChunkDestroy(chunk);
  }
  
  /* Destroying the chunks should have purged and removed all spare pages. */
  RingFinish(&vmArena->spareRing);

  /* Destroying the chunks should leave only the arena's own VM. */
  AVER(arena->committed == VMMapped(arenaVM));

  vmArena->sig = SigInvalid;

  ArenaFinish(arena); /* <code/global.c#finish.caller> */

  VMUnmap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  VMDestroy(arenaVM);
  EVENT1(ArenaDestroy, vmArena);
}


/* VMArenaReserved -- return the amount of reserved address space
 *
 * Add up the reserved space from all the chunks.
 */
static Size VMArenaReserved(Arena arena)
{
  Size reserved;
  Ring node, next;

  reserved = 0;
  RING_FOR(node, &arena->chunkRing, next) {
    VMChunk vmChunk = Chunk2VMChunk(RING_ELT(Chunk, chunkRing, node));
    reserved += VMReserved(vmChunk->vm);
  }
  return reserved;
}


/* vmArenaChunkSize -- choose chunk size for arena extension
 *
 * .vmchunk.overhead: This code still lacks a proper estimate of
 * the overhead required by a vmChunk for chunkStruct, page tables
 * etc.  For now, estimate it as 10%.  RHSK 2007-12-21
 */
static Size vmArenaChunkSize(VMArena vmArena, Size size)
{
  Size fraction = 10;  /* 10% -- see .vmchunk.overhead */
  Size chunkSize;
  Size chunkOverhead;

  /* 1: use extendBy, if it is big enough for size + overhead */
  chunkSize = vmArena->extendBy;
  chunkOverhead = chunkSize / fraction;
  if(chunkSize > size && (chunkSize - size) >= chunkOverhead)
    return chunkSize;

  /* 2: use size + overhead (unless it overflows SizeMAX) */
  chunkOverhead = size / (fraction - 1);
  if((SizeMAX - size) >= chunkOverhead)
    return size + chunkOverhead;

  /* 3: use SizeMAX */
  return SizeMAX;
}


/* VMArenaGrow -- Extend the arena by making a new chunk
 *
 * The size arg specifies how much we wish to allocate after the extension.
 */
static Res VMArenaGrow(Arena arena, SegPref pref, Size size)
{
  Chunk newChunk;
  Size chunkSize;
  Res res;
  VMArena vmArena;
  
  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  
  /* TODO: Ensure that extended arena will be able to satisfy pref. */
  AVERT(SegPref, pref);
  UNUSED(pref);

  chunkSize = vmArenaChunkSize(vmArena, size);

  EVENT3(vmArenaExtendStart, size, chunkSize,
         VMArenaReserved(VMArena2Arena(vmArena)));

  /* .chunk-create.fail: If we fail, try again with a smaller size */
  {
    unsigned fidelity = 8;  /* max fraction of addr-space we may 'waste' */
    Size chunkHalf;
    Size chunkMin = 4 * 1024;  /* typical single page */
    Size sliceSize;
    
    if (vmArena->extendMin > chunkMin)
      chunkMin = vmArena->extendMin;
    if (chunkSize < chunkMin)
      chunkSize = chunkMin;
    
    for(;; chunkSize = chunkHalf) {
      chunkHalf = chunkSize / 2;
      sliceSize = chunkHalf / fidelity;
      AVER(sliceSize > 0);
      
      /* remove slices, down to chunkHalf but no further */
      res = ResRESOURCE;
      for(; chunkSize > chunkHalf; chunkSize -= sliceSize) {
        if(chunkSize < chunkMin) {
          EVENT2(vmArenaExtendFail, chunkMin,
                 VMArenaReserved(VMArena2Arena(vmArena)));
          return res;
        }
        res = VMChunkCreate(&newChunk, vmArena, chunkSize);
        if(res == ResOK)
          goto vmArenaGrow_Done;
      }
    }
  }
  
vmArenaGrow_Done:
  EVENT2(vmArenaExtendDone, chunkSize, VMArenaReserved(VMArena2Arena(vmArena)));
  vmArena->extended(VMArena2Arena(vmArena),
		    newChunk->base,
		    AddrOffset(newChunk->base, newChunk->limit));
      
  return res;
}


/* pageState -- determine page state, even if unmapped
 *
 * Parts of the page table may be unmapped if their corresponding pages are
 * free.
 */

static unsigned pageState(VMChunk vmChunk, Index pi)
{
  Chunk chunk = VMChunk2Chunk(vmChunk);
  if (SparseArrayIsMapped(&vmChunk->pages, pi))
    return PageState(ChunkPage(chunk, pi));
  return PageStateFREE;
}


/* sparePageRelease -- releases a spare page
 *
 * Either to allocate it or to purge it.
 * Temporarily leaves it in an inconsistent state.
 */
static void sparePageRelease(VMChunk vmChunk, Index pi)
{
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Arena arena = ChunkArena(chunk);
  Page page = ChunkPage(chunk, pi);

  AVER(PageState(page) == PageStateSPARE);
  AVER(arena->spareCommitted >= ChunkPageSize(chunk));

  arena->spareCommitted -= ChunkPageSize(chunk);
  RingRemove(PageSpareRing(page));
}


static Res pageDescMap(VMChunk vmChunk, Index basePI, Index limitPI)
{
  Size before = VMMapped(vmChunk->vm);
  Arena arena = VMArena2Arena(VMChunkVMArena(vmChunk));
  Res res = SparseArrayMap(&vmChunk->pages, basePI, limitPI);
  arena->committed += VMMapped(vmChunk->vm) - before;
  return res;
}

static void pageDescUnmap(VMChunk vmChunk, Index basePI, Index limitPI)
{
  Size before = VMMapped(vmChunk->vm);
  Arena arena = VMArena2Arena(VMChunkVMArena(vmChunk));
  SparseArrayUnmap(&vmChunk->pages, basePI, limitPI);
  arena->committed += VMMapped(vmChunk->vm) - before;
}


/* pagesMarkAllocated -- Mark the pages allocated */

static Res pagesMarkAllocated(VMArena vmArena, VMChunk vmChunk,
                              Index basePI, Count pages, Pool pool)
{
  Index cursor, i, j, k;
  Index limitPI;
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Res res;
  
  limitPI = basePI + pages;
  AVER(limitPI <= chunk->pages);

  /* NOTE: We could find a reset bit range in vmChunk->pages.pages in order
     to skip across hundreds of pages at once.  That could speed up really
     big block allocations (hundreds of pages long). */

  cursor = basePI;
  while (BTFindLongResRange(&j, &k, vmChunk->pages.mapped, cursor, limitPI, 1)) {
    for (i = cursor; i < j; ++i) {
      sparePageRelease(vmChunk, i);
      PageAlloc(chunk, i, pool);
    }
    res = pageDescMap(vmChunk, j, k);
    if (res != ResOK)
      goto failSAMap;
    res = vmArenaMap(vmArena, vmChunk->vm,
                     PageIndexBase(chunk, j), PageIndexBase(chunk, k));
    if (res != ResOK)
      goto failVMMap;
    for (i = j; i < k; ++i) {
      PageInit(chunk, i);
      PageAlloc(chunk, i, pool);
    }
    cursor = k;
    if (cursor == limitPI)
      return ResOK;
  }
  for (i = cursor; i < limitPI; ++i) {
    sparePageRelease(vmChunk, i);
    PageAlloc(chunk, i, pool);
  }
  return ResOK;

failVMMap:
  pageDescUnmap(vmChunk, j, k);
failSAMap:
  /* region from basePI to j needs deallocating */
  /* TODO: Consider making pages spare instead, then purging. */
  if (basePI < j) {
    vmArenaUnmap(vmArena, vmChunk->vm,
                 PageIndexBase(chunk, basePI),
                 PageIndexBase(chunk, j));
    for (i = basePI; i < j; ++i)
      PageFree(chunk, i);
    pageDescUnmap(vmChunk, basePI, j);
  }
  return res;
}

static Res VMPagesMarkAllocated(Arena arena, Chunk chunk,
                                Index baseIndex, Count pages, Pool pool)
{
  Res res;

  AVERT(Arena, arena);
  AVERT(Chunk, chunk);
  AVER(chunk->allocBase <= baseIndex);
  AVER(pages > 0);
  AVER(baseIndex + pages <= chunk->pages);
  AVERT(Pool, pool);

  res = pagesMarkAllocated(Arena2VMArena(arena),
                           Chunk2VMChunk(chunk),
                           baseIndex,
                           pages,
                           pool);
  /* TODO: Could this loop be pushed down into vmArenaMap? */
  while (res != ResOK) {
    /* Try purging spare pages in the hope that the OS will give them back
       at the new address.  Will eventually run out of spare pages, so this
       loop will terminate. */
    /* TODO: Investigate implementing VMRemap so that we can guarantee
       success if we have enough spare pages. */
    if (VMPurgeSpare(arena, pages * ChunkPageSize(chunk)) == 0)
      break;
    res = pagesMarkAllocated(Arena2VMArena(arena),
                             Chunk2VMChunk(chunk),
                             baseIndex,
                             pages,
                             pool);
  }
  return res;
}


/* chunkUnmapAroundPage -- unmap spare pages in a chunk including this one
 *
 * Unmap the spare page passed, and possibly other pages in the chunk,
 * unmapping at least the size passed if available.  The amount unmapped
 * may exceed the size by up to one page.  Returns the amount of memory
 * unmapped.
 *
 * To minimse unmapping calls, the page passed is coalesced with spare
 * pages above and below, even though these may have been more recently
 * made spare.
 */

static Size chunkUnmapAroundPage(Chunk chunk, Size size, Page page)
{
  VMChunk vmChunk;
  Size purged = 0;
  Size pageSize;
  Index basePI, limitPI;

  AVERT(Chunk, chunk);
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(VMChunk, vmChunk);
  AVER(PageState(page) == PageStateSPARE);
  /* size is arbitrary */

  pageSize = ChunkPageSize(chunk);

  basePI = (Index)(page - chunk->pageTable);
  AVER(basePI < chunk->pages); /* page is within chunk's page table */
  limitPI = basePI;

  do {
    sparePageRelease(vmChunk, limitPI);
    ++limitPI;
    purged += pageSize;
  } while (purged < size &&
           limitPI < chunk->pages &&
           pageState(vmChunk, limitPI) == PageStateSPARE);
  while (purged < size &&
         basePI > 0 &&
         pageState(vmChunk, basePI - 1) == PageStateSPARE) {
    --basePI;
    sparePageRelease(vmChunk, basePI);
    purged += pageSize;
  }

  vmArenaUnmap(VMChunkVMArena(vmChunk),
               vmChunk->vm,
               PageIndexBase(chunk, basePI),
               PageIndexBase(chunk, limitPI));

  pageDescUnmap(vmChunk, basePI, limitPI);

  return purged;
}


/* arenaUnmapSpare -- return spare pages to the OS
 *
 * The size is the desired amount to purge, and the amount that was purged is
 * returned.  If filter is not NULL, then only pages within that chunk are
 * unmapped.
 */

#define ArenaChunkRing(arena) (&(arena)->chunkRing)

static Size arenaUnmapSpare(Arena arena, Size size, Chunk filter)
{
  Ring node;
  Size purged = 0;
  VMArena vmArena;

  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  if (filter != NULL)
    AVERT(Chunk, filter);

  /* Start by looking at the oldest page on the spare ring, to try to
     get some LRU behaviour from the spare pages cache. */
  /* RING_FOR won't work here, because chunkUnmapAroundPage deletes many
     entries from the spareRing, often including the "next" entry.  However,
     it doesn't delete entries from other chunks, so we can use them to step
     around the ring. */
  node = &vmArena->spareRing;
  while (RingNext(node) != &vmArena->spareRing && purged < size) {
    Ring next = RingNext(node);
    Page page = PageOfSpareRing(next);
    Chunk chunk;
    Bool b;
    /* Use the fact that the page table resides in the chunk to find the
       chunk that owns the page. */
    b = ChunkOfAddr(&chunk, arena, (Addr)page);
    AVER(b);
    if (filter == NULL || chunk == filter) {
      purged += chunkUnmapAroundPage(chunk, size - purged, page);
      /* chunkUnmapAroundPage must delete the page it's passed from the ring,
         or we can't make progress and there will be an infinite loop */
      AVER(RingNext(node) != next);
    } else
      node = next;
  }

  return purged;
}

static Size VMPurgeSpare(Arena arena, Size size)
{
  return arenaUnmapSpare(arena, size, NULL);
}


/* chunkUnmapSpare -- unmap all spare pages in a chunk */

static void chunkUnmapSpare(Chunk chunk)
{
  AVERT(Chunk, chunk);
  (void)arenaUnmapSpare(ChunkArena(chunk),
                        AddrOffset(chunk->base, chunk->limit),
                        chunk);
}


/* VMFree -- free a region in the arena */

static void VMFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  VMArena vmArena;
  Chunk chunk = NULL;           /* suppress "may be used uninitialized" */
  Count pages;
  Index pi, piBase, piLimit;
  Bool foundChunk;

  AVER(base != NULL);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);

  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
  AVER(AddrIsAligned(base, ChunkPageSize(arena->primary)));

  foundChunk = ChunkOfAddr(&chunk, arena, base);
  AVER(foundChunk);

  /* Calculate the number of pages in the region */
  pages = ChunkSizeToPages(chunk, size);
  piBase = INDEX_OF_ADDR(chunk, base);
  piLimit = piBase + pages;
  AVER(piBase < piLimit);
  AVER(piLimit <= chunk->pages);

  /* loop from pageBase to pageLimit-1 inclusive */
  /* Finish each Tract found, then convert them to spare pages. */
  for(pi = piBase; pi < piLimit; ++pi) {
    Page page = ChunkPage(chunk, pi);
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);

    TractFinish(tract);
    PageSetPool(page, NULL);
    PageSetType(page, PageStateSPARE);
    /* We must init the page's rings because it is a union with the
       tract and will contain junk. */
    RingInit(PageSpareRing(page));
    RingAppend(&vmArena->spareRing, PageSpareRing(page));
  }
  arena->spareCommitted += ChunkPagesToSize(chunk, piLimit - piBase);
  BTResRange(chunk->allocTable, piBase, piLimit);

  /* Consider returning memory to the OS. */
  /* TODO: Chunks are only destroyed when ArenaCompact is called, and that is
     only called from TraceReclaim.  Should consider destroying chunks here. */
  if (arena->spareCommitted > arena->spareCommitLimit) {
    /* Purge half of the spare memory, not just the extra sliver, so
       that we return a reasonable amount of memory in one go, and avoid
       lots of small unmappings, each of which has an overhead. */
    /* TODO: Consider making this time-based. */
    /* TODO: Consider making this smarter about the overheads tradeoff. */
    Size toPurge = arena->spareCommitted - arena->spareCommitLimit / 2;
    (void)VMPurgeSpare(arena, toPurge);
  }
}


static void VMCompact(Arena arena, Trace trace)
{
  VMArena vmArena;
  Ring node, next;
  Size vmem1;

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  AVERT(Trace, trace);

  vmem1 = VMArenaReserved(arena);

  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    if(chunk != arena->primary
       && BTIsResRange(chunk->allocTable, 0, chunk->pages)) {
      Addr base = chunk->base;
      Size size = AddrOffset(chunk->base, chunk->limit);

      /* Ensure there are no spare (mapped) pages left in the chunk.
         This could be short-cut if we're about to destroy the chunk,
         provided we can do the correct accounting in the arena. */
      chunkUnmapSpare(chunk);

      vmChunkDestroy(chunk);

      vmArena->contracted(arena, base, size);
    }
  }

  {
    Size vmem0 = trace->preTraceArenaReserved;
    Size vmem2 = VMArenaReserved(arena);

    /* VMCompact event: emit for all client-requested collections, */
    /* plus any others where chunks were gained or lost during the */
    /* collection.  */
    if(trace->why == TraceStartWhyCLIENTFULL_INCREMENTAL
       || trace->why == TraceStartWhyCLIENTFULL_BLOCK
       || vmem0 != vmem1
       || vmem1 != vmem2)
      EVENT3(VMCompact, vmem0, vmem1, vmem2);
  }
}

mps_res_t mps_arena_vm_growth(mps_arena_t mps_arena,
                              size_t mps_desired, size_t mps_minimum)
{
  Arena arena = (Arena)mps_arena;
  Size desired = (Size)mps_desired;
  Size minimum = (Size)mps_minimum;
  VMArena vmArena;
  
  ArenaEnter(arena);
  
  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  
  if(desired < minimum) {
    /* May not desire an increment smaller than the minimum! */
    ArenaLeave(arena);
    return MPS_RES_PARAM;
  }
  
  vmArena->extendBy = desired;
  vmArena->extendMin = minimum;
  
  ArenaLeave(arena);
  
  return MPS_RES_OK;
}


/* VMArenaClass  -- The VM arena class definition */

DEFINE_ARENA_CLASS(VMArenaClass, this)
{
  INHERIT_CLASS(this, AbstractArenaClass);
  this->name = "VM";
  this->size = sizeof(VMArenaStruct);
  this->offset = offsetof(VMArenaStruct, arenaStruct);
  this->varargs = VMArenaVarargs;
  this->init = VMArenaInit;
  this->finish = VMArenaFinish;
  this->reserved = VMArenaReserved;
  this->purgeSpare = VMPurgeSpare;
  this->grow = VMArenaGrow;
  this->free = VMFree;
  this->chunkInit = VMChunkInit;
  this->chunkFinish = VMChunkFinish;
  this->compact = VMCompact;
  this->describe = VMArenaDescribe;
  this->pagesMarkAllocated = VMPagesMarkAllocated;
}


/* mps_arena_class_vm -- return the arena class VM */

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)VMArenaClassGet();
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
