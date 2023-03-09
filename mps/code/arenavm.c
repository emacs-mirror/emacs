/* arenavm.c: VIRTUAL MEMORY ARENA CLASS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * .design: <design/arenavm>, and <design/arena#.coop-vm>
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
#include "bt.h"
#include "cbs.h"
#include "mpm.h"
#include "mpsavm.h"
#include "poolmfs.h"
#include "sa.h"
#include "tract.h"
#include "vm.h"

SRCID(arenavm, "$Id$");


/* VMChunk -- chunks for VM arenas */

typedef struct VMChunkStruct *VMChunk;

#define VMChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMChunkStruct {
  ChunkStruct chunkStruct;      /* generic chunk */
  VMStruct vmStruct;            /* virtual memory descriptor */
  Addr overheadMappedLimit;     /* limit of pages mapped for overhead */
  SparseArrayStruct pages;      /* to manage backing store of page table */
  Sig sig;                      /* design.mps.sig.field.end.outer */
} VMChunkStruct;

#define VMChunk2Chunk(vmchunk) (&(vmchunk)->chunkStruct)
#define VMChunkVM(vmchunk) (&(vmchunk)->vmStruct)
#define Chunk2VMChunk(chunk) PARENT(VMChunkStruct, chunkStruct, chunk)


/* VMChunkVMArena -- get the VM arena from a VM chunk */

#define VMChunkVMArena(vmchunk) \
  MustBeA(VMArena, ChunkArena(VMChunk2Chunk(vmchunk)))


/* VMArena
 *
 * <design/arena#.coop-vm.struct.vmarena> for description.
 */

typedef struct VMArenaStruct *VMArena;

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VMStruct vmStruct;            /* VM descriptor for VM containing arena */
  char vmParams[VMParamSize];   /* VM parameter block */
  Size extendBy;                /* desired arena increment */
  Size extendMin;               /* minimum arena increment */
  ArenaVMExtendedCallback extended;
  ArenaVMContractedCallback contracted;
  MFSStruct cbsBlockPoolStruct; /* stores blocks for CBSs */
  CBSStruct spareLandStruct;    /* spare memory */
  Sig sig;                      /* design.mps.sig.field.end.outer */
} VMArenaStruct;

#define VMArenaVM(vmarena) (&(vmarena)->vmStruct)
#define VMArenaCBSBlockPool(vmarena) MFSPool(&(vmarena)->cbsBlockPoolStruct)
#define VMArenaSpareLand(vmarena) CBSLand(&(vmarena)->spareLandStruct)


/* Forward declarations */

static void VMFree(Addr base, Size size, Pool pool);
static Size VMPurgeSpare(Arena arena, Size size);
static Size vmArenaUnmapSpare(Arena arena, Size size, Chunk filter);
DECLARE_CLASS(Arena, VMArena, AbstractArena);
static void VMCompact(Arena arena, Trace trace);
static void pageDescUnmap(VMChunk vmChunk, Index basePI, Index limitPI);


/* VMChunkCheck -- check the consistency of a VM chunk */

ATTRIBUTE_UNUSED
static Bool VMChunkCheck(VMChunk vmchunk)
{
  Chunk chunk;

  CHECKS(VMChunk, vmchunk);
  chunk = VMChunk2Chunk(vmchunk);
  CHECKD(Chunk, chunk);
  CHECKD(VM, VMChunkVM(vmchunk));
  CHECKL(SizeIsAligned(ChunkPageSize(chunk), VMPageSize(VMChunkVM(vmchunk))));
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

ATTRIBUTE_UNUSED
static Bool VMArenaCheck(VMArena vmArena)
{
  Arena arena;
  VMChunk primary;

  CHECKS(VMArena, vmArena);
  arena = MustBeA(AbstractArena, vmArena);
  CHECKD(Arena, arena);

  CHECKL(vmArena->extendBy > 0);
  CHECKL(vmArena->extendMin <= vmArena->extendBy);

  if (arena->primary != NULL) {
    primary = Chunk2VMChunk(arena->primary);
    CHECKD(VMChunk, primary);
    /* We could iterate over all chunks accumulating an accurate */
    /* count of committed, but we don't have all day. */
    CHECKL(VMMapped(VMChunkVM(primary)) <= arena->committed);
  }

  CHECKD(Pool, VMArenaCBSBlockPool(vmArena));
  CHECKD(Land, VMArenaSpareLand(vmArena));
  CHECKL((LandSize)(VMArenaSpareLand(vmArena)) == arena->spareCommitted);

  /* TODO: Add sig to VMParamsStruct so it can be checked. */

  return TRUE;
}


/* VMArenaDescribe -- describe the VMArena
 */
static Res VMArenaDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Arena arena = CouldBeA(AbstractArena, inst);
  VMArena vmArena = CouldBeA(VMArena, arena);
  Res res;

  if (!TESTC(VMArena, vmArena))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, VMArena, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "extendBy: $U\n", (WriteFU)vmArena->extendBy,
               "extendMin: $U\n", (WriteFU)vmArena->extendMin,
               NULL);
  if(res != ResOK)
    return res;

  res = LandDescribe(VMArenaSpareLand(vmArena), stream, depth + 2);
  if (res != ResOK)
    return res;

  /* TODO: incomplete -- some fields are not Described */

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
  Arena arena = MustBeA(AbstractArena, vmArena);
  Size size = AddrOffset(base, limit);
  Res res;

  /* no checking as function is local to module */

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
  Arena arena = MustBeA(AbstractArena, vmArena);
  Size size = AddrOffset(base, limit);

  /* no checking as function is local to module */
  AVER(size <= arena->committed);

  VMUnmap(vm, base, limit);
  arena->committed -= size;
}


/* chunkUnmapRange -- unmap range of addresses in a chunk */

static void chunkUnmapRange(Chunk chunk, Addr base, Addr limit)
{
  VMArena vmArena;
  VMChunk vmChunk;
  Index basePI, limitPI, i;

  AVERT(Chunk, chunk);
  AVER(base < limit);

  vmArena = MustBeA(VMArena, ChunkArena(chunk));
  vmChunk = Chunk2VMChunk(chunk);
  basePI = INDEX_OF_ADDR(chunk, base);
  limitPI = INDEX_OF_ADDR(chunk, limit);

  for (i = basePI; i < limitPI; ++i)
    PageInit(chunk, i);
  vmArenaUnmap(vmArena, VMChunkVM(vmChunk), base, limit);
  pageDescUnmap(vmChunk, basePI, limitPI);
}


/* VMChunkCreate -- create a chunk
 *
 * chunkReturn, return parameter for the created chunk.
 * vmArena, the parent VMArena.
 * size, approximate amount of virtual address that the chunk should reserve.
 */
static Res VMChunkCreate(Chunk *chunkReturn, VMArena vmArena, Size size)
{
  Arena arena = MustBeA(AbstractArena, vmArena);
  Res res;
  Addr base, limit, chunkStructLimit;
  VMStruct vmStruct;
  VM vm = &vmStruct;
  BootBlockStruct bootStruct;
  BootBlock boot = &bootStruct;
  VMChunk vmChunk;
  void *p;

  AVER(chunkReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);

  res = VMInit(vm, size, ArenaGrainSize(arena), vmArena->vmParams);
  if (res != ResOK)
    goto failVMInit;

  base = VMBase(vm);
  limit = VMLimit(vm);

  res = BootBlockInit(boot, (void *)base, (void *)limit);
  if (res != ResOK)
    goto failBootInit;

  /* .overhead.chunk-struct: Allocate and map the chunk structure. */
  res = BootAlloc(&p, boot, sizeof(VMChunkStruct), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failChunkAlloc;
  vmChunk = p;
  /* Calculate the limit of the grain where the chunkStruct resides. */
  chunkStructLimit = AddrAlignUp((Addr)(vmChunk + 1), ArenaGrainSize(arena));
  res = vmArenaMap(vmArena, vm, base, chunkStructLimit);
  if (res != ResOK)
    goto failChunkMap;
  vmChunk->overheadMappedLimit = chunkStructLimit;

  /* Copy VM descriptor into its place in the chunk. */
  VMCopy(VMChunkVM(vmChunk), vm);
  res = ChunkInit(VMChunk2Chunk(vmChunk), arena, base, limit,
                  VMReserved(VMChunkVM(vmChunk)), boot);
  if (res != ResOK)
    goto failChunkInit;

  BootBlockFinish(boot);

  vmChunk->sig = VMChunkSig;
  AVERT(VMChunk, vmChunk);

  *chunkReturn = VMChunk2Chunk(vmChunk);
  return ResOK;

failChunkInit:
  VMUnmap(vm, VMBase(vm), chunkStructLimit);
failChunkMap:
failChunkAlloc:
failBootInit:
  VMFinish(vm);
failVMInit:
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

  /* .overhead.sa-mapped: Chunk overhead for sparse array 'mapped' table. */
  res = BootAlloc(&p, boot, BTSize(chunk->pages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failSaMapped;
  saMapped = p;

  /* .overhead.sa-pages: Chunk overhead for sparse array 'pages' table. */
  res = BootAlloc(&p, boot, BTSize(chunk->pageTablePages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failSaPages;
  saPages = p;

  overheadLimit = AddrAdd(chunk->base, (Size)BootAllocated(boot));

  /* .overhead.page-table: Put the page table as late as possible, as
   * in VM systems we don't want to map it. */
  res = BootAlloc(&p, boot, chunk->pageTablePages << chunk->pageShift, chunk->pageSize);
  if (res != ResOK)
    goto failAllocPageTable;
  chunk->pageTable = p;

  /* Map memory for the bit tables. */
  if (vmChunk->overheadMappedLimit < overheadLimit) {
    overheadLimit = AddrAlignUp(overheadLimit, ChunkPageSize(chunk));
    res = vmArenaMap(VMChunkVMArena(vmChunk), VMChunkVM(vmChunk),
                     vmChunk->overheadMappedLimit, overheadLimit);
    if (res != ResOK)
      goto failTableMap;
    vmChunk->overheadMappedLimit = overheadLimit;
  }

  SparseArrayInit(&vmChunk->pages,
                  chunk->pageTable,
                  sizeof(PageUnion),
                  chunk->pages,
                  saMapped, saPages, VMChunkVM(vmChunk));

  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failSaPages:
failAllocPageTable:
failSaMapped:
  return res;
}


/* vmChunkDestroy -- destroy a VMChunk */

static Bool vmChunkDestroy(Tree tree, void *closure)
{
  Chunk chunk;
  VMChunk vmChunk;

  AVERT(Tree, tree);
  AVER(closure == UNUSED_POINTER);
  UNUSED(closure);

  chunk = ChunkOfTree(tree);
  AVERT(Chunk, chunk);
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(VMChunk, vmChunk);

  (void)vmArenaUnmapSpare(ChunkArena(chunk), ChunkSize(chunk), chunk);

  SparseArrayFinish(&vmChunk->pages);

  vmChunk->sig = SigInvalid;
  ChunkFinish(chunk);

  return TRUE;
}


/* VMChunkFinish -- finish a VMChunk */

static void VMChunkFinish(Chunk chunk)
{
  VMStruct vmStruct;
  VM vm = &vmStruct;
  VMChunk vmChunk = Chunk2VMChunk(chunk);

  /* Copy VM descriptor to stack-local storage so that we can continue
   * using the descriptor after the VM has been unmapped. */
  VMCopy(vm, VMChunkVM(vmChunk));

  vmArenaUnmap(VMChunkVMArena(vmChunk), vm,
               VMBase(vm), vmChunk->overheadMappedLimit);

  /* No point in finishing the other fields, since they are unmapped. */

  VMFinish(vm);
}


/* VMArenaVarargs -- parse obsolete varargs */

static void VMArenaVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_ARENA_SIZE;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
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


/* vmArenaChunkSize -- compute chunk size
 *
 * Compute the size of the smallest chunk that has size bytes of usable
 * address space (that is, after all overheads are accounted for).
 *
 * If successful, update *chunkSizeReturn with the computed chunk size
 * and return ResOK. If size is too large for a chunk, leave
 * *chunkSizeReturn unchanged and return ResRESOURCE.
 */
static Res vmArenaChunkSize(Size *chunkSizeReturn, VMArena vmArena, Size size)
{
  Size grainSize;               /* Arena grain size. */
  Shift grainShift;             /* The corresponding Shift. */
  Count pages;                  /* Number of usable pages in chunk. */
  Size pageTableSize;           /* Size of the page table. */
  Count pageTablePages;         /* Number of pages in the page table. */
  Size chunkSize;               /* Size of the chunk. */
  Size overhead;                /* Total overheads for the chunk. */

  AVER(chunkSizeReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);

  grainSize = ArenaGrainSize(MustBeA(AbstractArena, vmArena));
  grainShift = SizeLog2(grainSize);

  overhead = 0;
  do {
    chunkSize = size + overhead;
    AVER(SizeIsAligned(chunkSize, grainSize));

    /* See .overhead.chunk-struct. */
    overhead = SizeAlignUp(sizeof(VMChunkStruct), MPS_PF_ALIGN);

    /* See <code/tract.c#overhead.pages>, */
    pages = chunkSize >> grainShift;
    overhead += SizeAlignUp(BTSize(pages), MPS_PF_ALIGN);

    /* See .overhead.sa-mapped. */
    overhead += SizeAlignUp(BTSize(pages), MPS_PF_ALIGN);

    /* See .overhead.sa-pages. */
    pageTableSize = SizeAlignUp(pages * sizeof(PageUnion), grainSize);
    pageTablePages = pageTableSize >> grainShift;
    overhead += SizeAlignUp(BTSize(pageTablePages), MPS_PF_ALIGN);

    /* See .overhead.page-table. */
    overhead = SizeAlignUp(overhead, grainSize);
    overhead += SizeAlignUp(pageTableSize, grainSize);

    if (SizeMAX - overhead < size)
      return ResRESOURCE;
  } while (chunkSize < size + overhead);

  *chunkSizeReturn = chunkSize;
  return ResOK;
}


/* VMArenaCreate -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

ARG_DEFINE_KEY(arena_extended, Fun);
#define vmKeyArenaExtended (&_mps_key_arena_extended)
ARG_DEFINE_KEY(arena_contracted, Fun);
#define vmKeyArenaContracted (&_mps_key_arena_contracted)

static Res VMArenaCreate(Arena *arenaReturn, ArgList args)
{
  Size size = VM_ARENA_SIZE_DEFAULT; /* initial arena size */
  Align grainSize = MPS_PF_ALIGN; /* arena grain size */
  Size pageSize = PageSize(); /* operating system page size */
  Size chunkSize; /* size actually created */
  Size vmArenaSize; /* aligned size of VMArenaStruct */
  Res res;
  VMArena vmArena;
  Arena arena;
  VMStruct vmStruct;
  VM vm = &vmStruct;
  Chunk chunk;
  mps_arg_s arg;
  char vmParams[VMParamSize];

  AVER(arenaReturn != NULL);
  AVERT(ArgList, args);

  if (ArgPick(&arg, args, MPS_KEY_ARENA_GRAIN_SIZE))
    grainSize = arg.val.size;
  if (grainSize < pageSize)
    /* Make it easier to write portable programs by rounding up. */
    grainSize = pageSize;
  AVERT(ArenaGrainSize, grainSize);

  if (ArgPick(&arg, args, MPS_KEY_ARENA_SIZE))
    size = arg.val.size;
  if (size < grainSize * MPS_WORD_WIDTH)
    /* There has to be enough room in the chunk for a full complement of
       zones. Make it easier to write portable programs by rounding up. */
    size = grainSize * MPS_WORD_WIDTH;

  /* Parse remaining arguments, if any, into VM parameters. We must do
     this into some stack-allocated memory for the moment, since we
     don't have anywhere else to put it. It gets copied later. */
  res = VMParamFromArgs(vmParams, sizeof(vmParams), args);
  if (res != ResOK)
    goto failVMInit;

  /* Create a VM to hold the arena and map it. Store descriptor on the
     stack until we have the arena to put it in. */
  vmArenaSize = SizeAlignUp(sizeof(VMArenaStruct), MPS_PF_ALIGN);
  res = VMInit(vm, vmArenaSize, grainSize, vmParams);
  if (res != ResOK)
    goto failVMInit;
  res = VMMap(vm, VMBase(vm), VMLimit(vm));
  if (res != ResOK)
    goto failVMMap;
  vmArena = (VMArena)VMBase(vm);

  arena = CouldBeA(AbstractArena, vmArena);

  res = NextMethod(Arena, VMArena, init)(arena, grainSize, args);
  if (res != ResOK)
    goto failArenaInit;
  SetClassOfPoly(arena, CLASS(VMArena));
  AVER(vmArena == MustBeA(VMArena, arena));

  arena->reserved = VMReserved(vm);
  arena->committed = VMMapped(vm);

  /* Initialize a pool to hold the CBS blocks for the spare memory
     land. This pool can't be allowed to extend itself using
     ArenaAlloc because it is needed to implement ArenaAlloc (in the
     case where allocation hits the commit limit and so spare memory
     needs to be purged), so MFSExtendSelf is set to FALSE. Failures
     to extend are handled where the spare memory land is used. */
  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(RangeTreeStruct));
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, grainSize);
    MPS_ARGS_ADD(piArgs, MFSExtendSelf, FALSE);
    res = PoolInit(VMArenaCBSBlockPool(vmArena), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failMFSInit;

  /* Initialise spare land. */
  MPS_ARGS_BEGIN(liArgs) {
    MPS_ARGS_ADD(liArgs, CBSBlockPool, VMArenaCBSBlockPool(vmArena));
    res = LandInit(VMArenaSpareLand(vmArena), CLASS(CBS), arena,
                   grainSize, arena, liArgs);
  } MPS_ARGS_END(liArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failLandInit;
  ++ ArenaGlobals(arena)->systemPools;

  /* Copy VM descriptor into its place in the arena. */
  VMCopy(VMArenaVM(vmArena), vm);

  /* Copy the stack-allocated VM parameters into their home in the VMArena. */
  AVER(sizeof(vmArena->vmParams) == sizeof(vmParams));
  (void)mps_lib_memcpy(vmArena->vmParams, vmParams, sizeof(vmArena->vmParams));

  /* <design/arena#.coop-vm.struct.vmarena.extendby.init> */
  vmArena->extendBy = size;
  vmArena->extendMin = 0;

  vmArena->extended = vmArenaTrivExtended;
  if (ArgPick(&arg, args, vmKeyArenaExtended))
    vmArena->extended = (ArenaVMExtendedCallback)arg.val.fun;

  vmArena->contracted = vmArenaTrivContracted;
  if (ArgPick(&arg, args, vmKeyArenaContracted))
    vmArena->contracted = (ArenaVMContractedCallback)arg.val.fun;

  /* have to have a valid arena before calling ChunkCreate */
  vmArena->sig = VMArenaSig;
  res = VMChunkCreate(&chunk, vmArena, size);
  if (res != ResOK)
    goto failChunkCreate;

#if defined(AVER_AND_CHECK_ALL)
  /* Check the computation of the chunk size in vmArenaChunkSize, now
   * that we have the actual chunk for comparison. Note that
   * vmArenaChunkSize computes the smallest size with a given number
   * of usable bytes -- the actual chunk may be one grain larger. */
  {
    Size usableSize, computedChunkSize;
    usableSize = AddrOffset(PageIndexBase(chunk, chunk->allocBase),
                            chunk->limit);
    res = vmArenaChunkSize(&computedChunkSize, vmArena, usableSize);
    AVER(res == ResOK);
    AVER(computedChunkSize == ChunkSize(chunk)
         || computedChunkSize + grainSize == ChunkSize(chunk));
  }
#endif

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of stripes as will fit into a reference set (the number of */
  /* bits in a word).  Fail if the chunk is so small stripes are smaller */
  /* than pages.  Note that some zones are discontiguous in the chunk if */
  /* the size is not a power of 2.  <design/arena#.class.fields>. */
  chunkSize = ChunkSize(chunk);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);
  AVER(ChunkPageSize(chunk) == ArenaGrainSize(arena));

  AVERT(VMArena, vmArena);
  EVENT7(ArenaCreateVM, arena, size, chunkSize, grainSize,
         ClassOfPoly(Arena, arena), ArenaGlobals(arena)->systemPools,
         arena->serial);

  vmArena->extended(arena, chunk->base, chunkSize);

  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
  LandFinish(VMArenaSpareLand(vmArena));
failLandInit:
  PoolFinish(VMArenaCBSBlockPool(vmArena));
failMFSInit:
  NextMethod(Inst, VMArena, finish)(MustBeA(Inst, arena));
failArenaInit:
  VMUnmap(vm, VMBase(vm), VMLimit(vm));
failVMMap:
  VMFinish(vm);
failVMInit:
  return res;
}


static void vmArenaMFSFreeExtent(Pool pool, Addr base, Size size, void *closure)
{
  Chunk chunk = NULL;       /* suppress "may be used uninitialized" */
  Bool foundChunk;

  AVERT(Pool, pool);
  AVER(closure == UNUSED_POINTER);
  UNUSED(closure);

  foundChunk = ChunkOfAddr(&chunk, PoolArena(pool), base);
  AVER(foundChunk);
  chunkUnmapRange(chunk, base, AddrAdd(base, size));
}


static void VMArenaDestroy(Arena arena)
{
  VMArena vmArena = MustBeA(VMArena, arena);
  Land spareLand = VMArenaSpareLand(vmArena);
  VMStruct vmStruct;
  VM vm = &vmStruct;

  /* Unmap all remaining spare memory. */
  VMPurgeSpare(arena, LandSize(spareLand));
  AVER(LandSize(spareLand) == 0);
  AVER(arena->spareCommitted == 0);

  /* The CBS block pool can't free its own memory via ArenaFree
     because that would attempt to insert the freed memory into the
     spare memory land, which uses blocks from the block pool. */
  MFSFinishExtents(VMArenaCBSBlockPool(vmArena), vmArenaMFSFreeExtent,
                   UNUSED_POINTER);
  PoolFinish(VMArenaCBSBlockPool(vmArena));

  /* Destroy all chunks, including the primary. See
   * <design/arena#.chunk.delete> */
  arena->primary = NULL;
  TreeTraverseAndDelete(&arena->chunkTree, vmChunkDestroy, UNUSED_POINTER);

  /* Must wait until the chunks are destroyed, since vmChunkDestroy
     calls vmArenaUnmapSpare which uses the spare land. */
  LandFinish(VMArenaSpareLand(vmArena));

  /* Destroying the chunks must leave only the arena's own VM. */
  AVER(arena->reserved == VMReserved(VMArenaVM(vmArena)));
  AVER(arena->committed == VMMapped(VMArenaVM(vmArena)));

  vmArena->sig = SigInvalid;

  NextMethod(Inst, VMArena, finish)(MustBeA(Inst, arena));

  /* Copy VM descriptor to stack-local storage so that we can continue
   * using the descriptor after the VM has been unmapped. */
  VMCopy(vm, VMArenaVM(vmArena));
  VMUnmap(vm, VMBase(vm), VMLimit(vm));
  VMFinish(vm);

  EVENT1(ArenaDestroy, vmArena);
}


/* VMArenaGrow -- Extend the arena by making a new chunk
 *
 * size specifies how much we wish to allocate after the extension.
 * pref specifies the preference for the location of the allocation.
 */
static Res VMArenaGrow(Arena arena, LocusPref pref, Size size)
{
  VMArena vmArena = MustBeA(VMArena, arena);
  Chunk newChunk;
  Size chunkSize;
  Size chunkMin;
  Res res;

  /* TODO: Ensure that extended arena will be able to satisfy pref. */
  AVERT(LocusPref, pref);
  UNUSED(pref);

  res = vmArenaChunkSize(&chunkMin, vmArena, size);
  if (res != ResOK)
    return res;
  chunkSize = vmArena->extendBy;

  EVENT3(VMArenaExtendStart, size, chunkSize, ArenaReserved(arena));

  /* .chunk-create.fail: If we fail, try again with a smaller size */
  {
    unsigned fidelity = 8;  /* max fraction of addr-space we may 'waste' */
    Size chunkHalf;
    Size sliceSize;

    if (vmArena->extendMin > chunkMin)
      chunkMin = vmArena->extendMin;
    if (chunkSize < chunkMin)
      chunkSize = chunkMin;

    res = ResRESOURCE;
    for(;; chunkSize = chunkHalf) {
      chunkHalf = chunkSize / 2;
      sliceSize = chunkHalf / fidelity;
      AVER(sliceSize > 0);

      /* remove slices, down to chunkHalf but no further */
      for(; chunkSize > chunkHalf; chunkSize -= sliceSize) {
        if(chunkSize < chunkMin) {
          EVENT2(VMArenaExtendFail, chunkMin, ArenaReserved(arena));
          return res;
        }
        res = VMChunkCreate(&newChunk, vmArena, chunkSize);
        if(res == ResOK)
          goto vmArenaGrow_Done;
      }
    }
  }

vmArenaGrow_Done:
  EVENT2(VMArenaExtendDone, chunkSize, ArenaReserved(arena));
  vmArena->extended(arena,
                    newChunk->base,
                    AddrOffset(newChunk->base, newChunk->limit));

  return res;
}


/* spareRangeRelease -- release a range of spare memory in a chunk
 *
 * Temporarily leaves data structures in an inconsistent state (the
 * spare memory is still marked as SPARE in the chunk's page table,
 * but it is no longer in the spare memory land). The caller must
 * either allocate the memory or unmap it.
 */

static void spareRangeRelease(VMChunk vmChunk, Index piBase, Index piLimit)
{
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Arena arena = ChunkArena(chunk);
  VMArena vmArena = VMChunkVMArena(vmChunk);
  Land spareLand = VMArenaSpareLand(vmArena);
  RangeStruct range, containingRange;
  Res res;

  AVER(piBase < piLimit);
  RangeInit(&range, PageIndexBase(chunk, piBase),
            PageIndexBase(chunk, piLimit));

  res = LandDelete(&containingRange, spareLand, &range);
  if (res != ResOK) {
    /* Range could not be deleted from the spare memory land because
       it splits the containing range and so needs to allocate a
       block but the block pool is full. Use the first grain of the
       containing range to extend the block pool. */
    Addr extendBase = RangeBase(&containingRange);
    Index extendBasePI = INDEX_OF_ADDR(chunk, extendBase);
    Addr extendLimit = AddrAdd(extendBase, ArenaGrainSize(arena));
    RangeStruct extendRange;
    AVER(res == ResLIMIT);
    RangeInit(&extendRange, extendBase, extendLimit);
    AVER(!RangesOverlap(&extendRange, &range));
    res = LandDelete(&containingRange, spareLand, &extendRange);
    AVER(res == ResOK);
    AVER(arena->spareCommitted >= RangeSize(&extendRange));
    arena->spareCommitted -= RangeSize(&extendRange);
    PageAlloc(chunk, extendBasePI, VMArenaCBSBlockPool(vmArena));
    MFSExtend(VMArenaCBSBlockPool(vmArena), extendBase, extendLimit);
    res = LandDelete(&containingRange, spareLand, &range);
    AVER(res == ResOK);
  }
  AVER(arena->spareCommitted >= RangeSize(&range));
  arena->spareCommitted -= RangeSize(&range);
}


static Res pageDescMap(VMChunk vmChunk, Index basePI, Index limitPI)
{
  Size before = VMMapped(VMChunkVM(vmChunk));
  Arena arena = MustBeA(AbstractArena, VMChunkVMArena(vmChunk));
  Res res = SparseArrayMap(&vmChunk->pages, basePI, limitPI);
  Size after = VMMapped(VMChunkVM(vmChunk));
  AVER(before <= after);
  arena->committed += after - before;
  return res;
}

static void pageDescUnmap(VMChunk vmChunk, Index basePI, Index limitPI)
{
  Size size, after;
  Size before = VMMapped(VMChunkVM(vmChunk));
  Arena arena = MustBeA(AbstractArena, VMChunkVMArena(vmChunk));
  SparseArrayUnmap(&vmChunk->pages, basePI, limitPI);
  after = VMMapped(VMChunkVM(vmChunk));
  AVER(after <= before);
  size = before - after;
  AVER(arena->committed >= size);
  arena->committed -= size;
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
    if (cursor < j)
      spareRangeRelease(vmChunk, cursor, j);
    for (i = cursor; i < j; ++i)
      PageAlloc(chunk, i, pool);
    res = pageDescMap(vmChunk, j, k);
    if (res != ResOK)
      goto failSAMap;
    res = vmArenaMap(vmArena, VMChunkVM(vmChunk),
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
  if (cursor < limitPI)
    spareRangeRelease(vmChunk, cursor, limitPI);
  for (i = cursor; i < limitPI; ++i)
    PageAlloc(chunk, i, pool);
  return ResOK;

failVMMap:
  pageDescUnmap(vmChunk, j, k);
failSAMap:
  /* Region from basePI to j was allocated but can't be used */
  if (basePI < j) {
    VMFree(PageIndexBase(chunk, basePI),
           ChunkPagesToSize(chunk, j - basePI),
           pool);
  }
  return res;
}

static Res VMPagesMarkAllocated(Arena arena, Chunk chunk,
                                Index baseIndex, Count pages, Pool pool)
{
  Res res;
  VMArena vmArena = MustBeA(VMArena, arena);

  AVERT(Arena, arena);
  AVERT(Chunk, chunk);
  AVER(chunk->allocBase <= baseIndex);
  AVER(pages > 0);
  AVER(baseIndex + pages <= chunk->pages);
  AVERT(Pool, pool);

  res = pagesMarkAllocated(vmArena,
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
    res = pagesMarkAllocated(vmArena,
                             Chunk2VMChunk(chunk),
                             baseIndex,
                             pages,
                             pool);
  }
  return res;
}


static Bool VMChunkPageMapped(Chunk chunk, Index index)
{
  VMChunk vmChunk;
  AVERT(Chunk, chunk);
  AVER(index < chunk->pages);
  vmChunk = Chunk2VMChunk(chunk);
  return BTGet(vmChunk->pages.mapped, index);
}


/* vmArenaUnmapSpare -- unmap spare memory
 *
 * The size is the desired amount to unmap, and the amount that was
 * unmapped is returned. If filter is not NULL, then only memory
 * within that chunk is unmapped.
 */

typedef struct VMArenaUnmapSpareClosureStruct {
  Arena arena;           /* arena owning the spare memory */
  Size size;             /* desired amount of spare memory to unmap */
  Chunk filter;          /* NULL or chunk to unmap from */
  Size unmapped;         /* actual amount unmapped */
} VMArenaUnmapSpareClosureStruct, *VMArenaUnmapSpareClosure;

static Bool vmArenaUnmapSpareRange(Bool *deleteReturn, Land land, Range range,
                                   void *p)
{
  VMArenaUnmapSpareClosure closure = p;
  Arena arena;
  Chunk chunk = NULL;       /* suppress "may be used uninitialized" */
  Bool foundChunk;

  AVER(deleteReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(p != NULL);

  arena = closure->arena;
  foundChunk = ChunkOfAddr(&chunk, arena, RangeBase(range));
  AVER(foundChunk);

  if (closure->filter == NULL || closure->filter == chunk) {
    Size size = RangeSize(range);
    chunkUnmapRange(chunk, RangeBase(range), RangeLimit(range));
    AVER(arena->spareCommitted >= size);
    arena->spareCommitted -= size;
    closure->unmapped += size;
    *deleteReturn = TRUE;
  }

  return closure->unmapped < closure->size;
}

static Size vmArenaUnmapSpare(Arena arena, Size size, Chunk filter)
{
  VMArena vmArena = MustBeA(VMArena, arena);
  Land spareLand = VMArenaSpareLand(vmArena);
  VMArenaUnmapSpareClosureStruct closure;

  if (filter != NULL)
    AVERT(Chunk, filter);

  closure.arena = arena;
  closure.size = size;
  closure.filter = filter;
  closure.unmapped = 0;
  (void)LandIterateAndDelete(spareLand, vmArenaUnmapSpareRange, &closure);

  AVER(LandSize(spareLand) == arena->spareCommitted);

  return closure.unmapped;
}

static Size VMPurgeSpare(Arena arena, Size size)
{
  return vmArenaUnmapSpare(arena, size, NULL);
}


/* VMFree -- free a region in the arena */

static void VMFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  VMArena vmArena;
  Land spareLand;
  Chunk chunk = NULL;           /* suppress "may be used uninitialized" */
  Count pages;
  Index pi, piBase, piLimit;
  Bool foundChunk;
  Size spareCommitted;
  RangeStruct range, containingRange;
  Res res;

  AVER(base != NULL);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  vmArena = MustBeA(VMArena, arena);
  spareLand = VMArenaSpareLand(vmArena);

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

  /* Finish each Tract in the region. */
  for(pi = piBase; pi < piLimit; ++pi) {
    Page page = ChunkPage(chunk, pi);
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);

    TractFinish(tract);
  }
  BTResRange(chunk->allocTable, piBase, piLimit);

  /* Freed range is now spare memory, so add it to spare memory land. */
  RangeInitSize(&range, base, size);
  res = LandInsert(&containingRange, spareLand, &range);
  if (res != ResOK) {
    /* The freed range could not be inserted into the spare memory
       land because the block pool is full. Allocate the first grain
       of the freed range and use it to extend the block pool. */
    Addr extendLimit = AddrAdd(base, ArenaGrainSize(arena));
    res = ArenaFreeLandDelete(arena, base, extendLimit);
    if (res != ResOK) {
      /* Give up and unmap the memory immediately. */
      chunkUnmapRange(chunk, RangeBase(&range), RangeLimit(&range));
      return;
    }
    PageAlloc(chunk, INDEX_OF_ADDR(chunk, base), VMArenaCBSBlockPool(vmArena));
    MFSExtend(VMArenaCBSBlockPool(vmArena), base, extendLimit);

    /* Adjust the freed range and try again. This time the insertion
       must succeed since we just extended the block pool. */
    RangeSetBase(&range, extendLimit);
    AVERT(Range, &range);
    if (!RangeIsEmpty(&range)) {
      res = LandInsert(&containingRange, spareLand, &range);
      AVER(res == ResOK);
    }
  }
  arena->spareCommitted += RangeSize(&range);

  /* Consider returning memory to the OS. */
  /* Purging spare memory can cause page descriptors to be unmapped,
     causing ArenaCommitted to fall, so we can't be sure to unmap
     enough in one pass. This somewhat contradicts the goal of having
     spare committed memory, which is to reduce the amount of mapping
     and unmapping, but we need to do this in order to be able to
     check the spare committed invariant. */
  spareCommitted = ArenaSpareCommitted(arena);
  while (spareCommitted > ArenaSpareCommitLimit(arena)) {
    Size toPurge = spareCommitted - ArenaSpareCommitLimit(arena);
    /* Purge at least half of the spare memory, not just the extra
       sliver, so that we return a reasonable amount of memory in one
       go, and avoid lots of small unmappings, each of which has an
       overhead. */
    /* TODO: Consider making this time-based. */
    /* TODO: Consider making this smarter about the overheads tradeoff. */
    Size minPurge = ArenaSpareCommitted(arena) / 2;
    Size newSpareCommitted;
    if (toPurge < minPurge)
      toPurge = minPurge;
    VMPurgeSpare(arena, toPurge);
    newSpareCommitted = ArenaSpareCommitted(arena);
    AVER(newSpareCommitted < spareCommitted);
    spareCommitted = newSpareCommitted;
  }
  AVER(ArenaCurrentSpare(arena) <= ArenaSpare(arena));

  /* TODO: Chunks are only destroyed when ArenaCompact is called, and
     that is only called from traceReclaim. Should consider destroying
     chunks here. See job003815. */
}


/* vmChunkCompact -- delete chunk if empty and not primary */

static Bool vmChunkCompact(Tree tree, void *closure)
{
  Chunk chunk;
  Arena arena = closure;
  VMArena vmArena = MustBeA(VMArena, arena);

  AVERT(Tree, tree);

  chunk = ChunkOfTree(tree);
  AVERT(Chunk, chunk);
  if(chunk != arena->primary
     && BTIsResRange(chunk->allocTable, 0, chunk->pages))
  {
    Addr base = chunk->base;
    Size size = ChunkSize(chunk);
    /* Callback before destroying the chunk, as the arena is (briefly)
       invalid afterwards. See job003893. */
    (*vmArena->contracted)(arena, base, size);
    vmChunkDestroy(tree, UNUSED_POINTER);
    return TRUE;
  } else {
    /* Keep this chunk. */
    return FALSE;
  }
}


static void VMCompact(Arena arena, Trace trace)
{
  STATISTIC_DECL(Size vmem1)

  AVERT(Trace, trace);

  STATISTIC(vmem1 = ArenaReserved(arena));

  /* Destroy chunks that are completely free, but not the primary
   * chunk. <design/arena#.chunk.delete>
   * TODO: add hysteresis here. See job003815. */
  TreeTraverseAndDelete(&arena->chunkTree, vmChunkCompact, arena);

  STATISTIC({
    Size vmem0 = trace->preTraceArenaReserved;
    Size vmem2 = ArenaReserved(arena);

    /* VMCompact event: emit for collections where chunks were gained
     * or lost during the collection. */
    if (vmem0 != vmem1 || vmem1 != vmem2)
      EVENT3(VMCompact, vmem0, vmem1, vmem2);
  });
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
  vmArena = MustBeA(VMArena, arena);

  /* Must desire at least the minimum increment! */
  AVER(desired >= minimum);

  vmArena->extendBy = desired;
  vmArena->extendMin = minimum;

  ArenaLeave(arena);

  return MPS_RES_OK;
}


/* VMArenaClass  -- The VM arena class definition */

DEFINE_CLASS(Arena, VMArena, klass)
{
  INHERIT_CLASS(klass, VMArena, AbstractArena);
  klass->instClassStruct.describe = VMArenaDescribe;
  klass->size = sizeof(VMArenaStruct);
  klass->varargs = VMArenaVarargs;
  klass->create = VMArenaCreate;
  klass->destroy = VMArenaDestroy;
  klass->purgeSpare = VMPurgeSpare;
  klass->grow = VMArenaGrow;
  klass->free = VMFree;
  klass->chunkInit = VMChunkInit;
  klass->chunkFinish = VMChunkFinish;
  klass->compact = VMCompact;
  klass->pagesMarkAllocated = VMPagesMarkAllocated;
  klass->chunkPageMapped = VMChunkPageMapped;
  AVERT(ArenaClass, klass);
}


/* mps_arena_class_vm -- return the arena class VM */

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)CLASS(VMArena);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
