/* arenavm.c: VIRTUAL MEMORY ARENA CLASS
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
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
#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$Id$");


/* @@@@ Arbitrary calculation for the maximum number of distinct */
/* object sets for generations.  Should be in config.h. */
/* .gencount.const: Must be a constant suitable for use as an */
/* array size. */
#define VMArenaGenCount ((Count)(MPS_WORD_WIDTH/2))

/* VMChunk -- chunks for VM arenas */

typedef struct VMChunkStruct *VMChunk;

#define VMChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMChunkStruct {
  ChunkStruct chunkStruct;      /* generic chunk */
  VM vm;                        /* virtual memory handle */
  Addr overheadMappedLimit;           /* limit of pages mapped for overhead */
  BT pageTableMapped;           /* indicates mapped state of page table */
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
  ZoneSet blacklist;             /* zones to use last */
  ZoneSet genZoneSet[VMArenaGenCount]; /* .gencount.const */
  ZoneSet freeSet;               /* unassigned zones */
  Size extendBy;                /* desired arena increment */
  Size extendMin;               /* minimum arena increment */
  ArenaVMExtendedCallback extended;
  ArenaVMContractedCallback contracted;
  RingStruct spareRing;         /* spare (free but mapped) tracts */
  RingStruct freeRing[MPS_WORD_WIDTH]; /* free page caches, per zone */
  Sig sig;                      /* <design/sig/> */
} VMArenaStruct;

#define Arena2VMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)
#define VMArena2Arena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static Size VMPurgeSpare(Arena arena, Size size);
static void chunkUnmapSpare(Chunk chunk);
extern ArenaClass VMArenaClassGet(void);
extern ArenaClass VMNZArenaClassGet(void);
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
  /* check pageTableMapped table */
  CHECKL(vmchunk->pageTableMapped != NULL);
  CHECKL((Addr)vmchunk->pageTableMapped >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->pageTableMapped, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadMappedLimit);
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
  Index gen, i;
  ZoneSet allocSet;
  Arena arena;
  VMChunk primary;

  CHECKS(VMArena, vmArena);
  arena = VMArena2Arena(vmArena);
  CHECKD(Arena, arena);
  /* spare pages are committed, so must be less spare than committed. */
  CHECKL(vmArena->spareSize <= arena->committed);
  CHECKL(vmArena->blacklist != ZoneSetUNIV);

  allocSet = ZoneSetEMPTY;
  for(gen = (Index)0; gen < VMArenaGenCount; ++gen) {
    allocSet = ZoneSetUnion(allocSet, vmArena->genZoneSet[gen]);
  }
  CHECKL(ZoneSetInter(allocSet, vmArena->freeSet) == ZoneSetEMPTY);
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
  for (i = 0; i < NELEMS(vmArena->freeRing); ++i)
    CHECKL(RingCheck(&vmArena->freeRing[i]));

  /* FIXME: Can't check VMParams */

  return TRUE;
}


/* VMArenaDescribe -- describe the VMArena
 */
static Res VMArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  VMArena vmArena;
  Index gen;

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

  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    if(vmArena->genZoneSet[gen] != ZoneSetEMPTY) {
      res = WriteF(stream,
                   "  genZoneSet[$U]: $B\n",
                   (WriteFU)gen, (WriteFB)vmArena->genZoneSet[gen],
                   NULL);
      if(res != ResOK)
        return res;
    }
  }
  
  res = WriteF(stream,
               "  freeSet:       $B\n", (WriteFB)vmArena->freeSet,
               "  blacklist:     $B\n", (WriteFB)vmArena->blacklist,
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
  size_t btSize;
  VMChunk vmChunk;
  Addr overheadLimit;
  void *p;
  Res res;

  /* chunk is supposed to be uninitialized, so don't check it. */
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(BootBlock, boot);

  btSize = (size_t)BTSize(chunk->pageTablePages);
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failPageTableMapped;
  vmChunk->pageTableMapped = p;

  /* Actually commit all the tables. <design/arenavm/>.@@@@ */
  overheadLimit = AddrAdd(chunk->base, (Size)BootAllocated(boot));
  if (vmChunk->overheadMappedLimit < overheadLimit) {
    overheadLimit = AddrAlignUp(overheadLimit, ChunkPageSize(chunk));
    res = vmArenaMap(VMChunkVMArena(vmChunk), vmChunk->vm,
                     vmChunk->overheadMappedLimit, overheadLimit);
    if (res != ResOK)
      goto failTableMap;
    vmChunk->overheadMappedLimit = overheadLimit;
  }

  BTResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages);

  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failPageTableMapped:
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
  
  /* This check will also ensure that there are no non-free pages in the
     chunk, because those pages would require mapped page table entries. */
  AVER(BTIsResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages));

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
  Index gen;
  VM arenaVM;
  Chunk chunk;
  mps_arg_s arg;
  char vmParams[VMParamSize];
  Index i;
  
  AVER(arenaReturn != NULL);
  AVER(class == VMArenaClassGet() || class == VMNZArenaClassGet());
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
  res = ArenaInit(arena, class);
  if (res != ResOK)
    goto failArenaInit;
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->spareSize = 0;
  RingInit(&vmArena->spareRing);
  for (i = 0; i < NELEMS(vmArena->freeRing); ++i)
    RingInit(&vmArena->freeRing[i]);

  /* Copy the stack-allocated VM parameters into their home in the VMArena. */
  AVER(sizeof(vmArena->vmParams) == sizeof(vmParams));
  mps_lib_memcpy(vmArena->vmParams, vmParams, sizeof(vmArena->vmParams));

  /* .blacklist: We blacklist the zones that could be referenced by small
     integers misinterpreted as references.  This isn't a perfect simulation,
     but it should catch the common cases. */
  {
    union {
      mps_word_t word;
      mps_addr_t addr;
      int i;
      long l;
    } nono;
    vmArena->blacklist = ZoneSetEMPTY;
    nono.word = 0;
    nono.i = 1;
    vmArena->blacklist = ZoneSetAddAddr(arena, vmArena->blacklist, nono.addr);
    nono.i = -1;
    vmArena->blacklist = ZoneSetAddAddr(arena, vmArena->blacklist, nono.addr);
    nono.l = 1;
    vmArena->blacklist = ZoneSetAddAddr(arena, vmArena->blacklist, nono.addr);
    nono.l = -1;
    vmArena->blacklist = ZoneSetAddAddr(arena, vmArena->blacklist, nono.addr);
  }
  EVENT2(ArenaBlacklistZone, vmArena, vmArena->blacklist);
  
  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genZoneSet[gen] = ZoneSetEMPTY;
  }
  vmArena->freeSet = ZoneSetUNIV; /* includes blacklist */
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
  arena->primary = chunk;

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of stripes as will fit into a reference set (the number of */
  /* bits in a word).  Fail if the chunk is so small stripes are smaller */
  /* than pages.  Note that some zones are discontiguous in the chunk if */
  /* the size is not a power of 2.  See <design/arena/#class.fields>. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);
  arena->alignment = chunk->pageSize;

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
  Index i;

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
  for (i = 0; i < NELEMS(vmArena->freeRing); ++i)
    RingFinish(&vmArena->freeRing[i]);

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


/* Page Table Partial Mapping
 *
 * Some helper functions
 */


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
   / sizeof(PageUnion))


/* tablePageWholeBaseIndex
 *
 * Index of the first page descriptor wholly on this table page.
 * Table page specified by address (not index).
 */
#define tablePageWholeBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, \
              AddrAdd((tablePage), sizeof(PageUnion)-1)) \
   / sizeof(PageUnion))


/* tablePageLimitIndex -- index of the first page descriptor falling
 *                        (wholly) on the next table page
 *
 * Similar to tablePageBaseIndex, see .repr.table-page and .division.
 */
#define tablePageLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + ChunkPageSize(chunk) - 1) \
   / sizeof(PageUnion) \
   + 1)

/* tablePageWholeLimitIndex
 *
 * Index of the first page descriptor falling partially on the next
 * table page.
 */
#define tablePageWholeLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + ChunkPageSize(chunk)) \
   / sizeof(PageUnion))


/* tablePagesUsed
 *
 * Takes a range of pages identified by [pageBase, pageLimit), and
 * returns the pages occupied by the page table which store the
 * PageUnion descriptors for those pages.
 */
static void tablePagesUsed(Index *tableBaseReturn, Index *tableLimitReturn,
                           Chunk chunk, Index pageBase, Index pageLimit)
{
  *tableBaseReturn =
    PageTablePageIndex(chunk,
                       AddrPageBase(chunk, addrOfPageDesc(chunk, pageBase)));
  *tableLimitReturn =
    PageTablePageIndex(chunk,
                       AddrAlignUp(addrOfPageDesc(chunk, pageLimit),
                                   ChunkPageSize(chunk)));
  return;
}


/* tablePagesEnsureMapped -- ensure needed part of page table is mapped
 *
 * Pages from baseIndex to limitIndex are about to be allocated.
 * Ensure that the relevant pages occupied by the page table are mapped.
 */
static Res tablePagesEnsureMapped(VMChunk vmChunk,
                                  Index baseIndex, Index limitIndex)
{
  /* tableBaseIndex, tableLimitIndex, tableCursorIndex, */
  /* unmappedBase, unmappedLimit are all indexes of pages occupied */
  /* by the page table. */
  Index tableBaseIndex, tableLimitIndex;
  Index tableCursorIndex;
  Index unmappedBaseIndex, unmappedLimitIndex;
  Index i;
  Chunk chunk;
  Res res;

  chunk = VMChunk2Chunk(vmChunk);

  tablePagesUsed(&tableBaseIndex, &tableLimitIndex,
                 chunk, baseIndex, limitIndex);

  tableCursorIndex = tableBaseIndex;
 
  while(BTFindLongResRange(&unmappedBaseIndex, &unmappedLimitIndex,
                           vmChunk->pageTableMapped,
                           tableCursorIndex, tableLimitIndex,
                           1)) {
    Addr unmappedBase = TablePageIndexBase(chunk, unmappedBaseIndex);
    Addr unmappedLimit = TablePageIndexBase(chunk, unmappedLimitIndex);
    /* There might be a page descriptor overlapping the beginning */
    /* of the range of table pages we are about to map. */
    /* We need to work out whether we should touch it. */
    if (unmappedBaseIndex == tableBaseIndex
        && unmappedBaseIndex > 0
        && !BTGet(vmChunk->pageTableMapped, unmappedBaseIndex - 1)) {
      /* Start with first descriptor wholly on page */
      baseIndex = tablePageWholeBaseIndex(chunk, unmappedBase);
    } else {
      /* start with first descriptor partially on page */
      baseIndex = tablePageBaseIndex(chunk, unmappedBase);
    }
    /* Similarly for the potentially overlapping page descriptor */
    /* at the end. */
    if (unmappedLimitIndex == tableLimitIndex
        && unmappedLimitIndex < chunk->pageTablePages
        && !BTGet(vmChunk->pageTableMapped, unmappedLimitIndex)) {
      /* Finish with last descriptor wholly on page */
      limitIndex = tablePageBaseIndex(chunk, unmappedLimit);
    } else if (unmappedLimitIndex == chunk->pageTablePages) {
      /* Finish with last descriptor in chunk */
      limitIndex = chunk->pages;
    } else {
      /* Finish with last descriptor partially on page */
      limitIndex = tablePageWholeBaseIndex(chunk, unmappedLimit);
    }
    res = vmArenaMap(VMChunkVMArena(vmChunk),
                     vmChunk->vm, unmappedBase, unmappedLimit);
    if (res != ResOK)
      return res;
    BTSetRange(vmChunk->pageTableMapped, unmappedBaseIndex, unmappedLimitIndex);
    for(i = baseIndex; i < limitIndex; ++i) {
      PageInit(chunk, i);
    }
    tableCursorIndex = unmappedLimitIndex;
    if (tableCursorIndex == tableLimitIndex)
      break;
  }

  return ResOK;
}


/* pagesFindFreeInArea -- find a range of free pages in a given address range
 *
 * Search for a free run of pages in the free table, between the given
 * base and limit.
 *
 * The downwards arg governs whether we use BTFindShortResRange (if
 * downwards is FALSE) or BTFindShortResRangeHigh (if downwards is
 * TRUE).  This _roughly_ corresponds to allocating pages from top down
 * (when downwards is TRUE), at least within an interval.  It is used
 * for implementing SegPrefHigh.
 */
static Bool pagesFindFreeInArea(Index *baseReturn, Chunk chunk, Size size,
                                Addr base, Addr limit, Bool downwards)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;    /* Index equiv. to base and limit */
  Index start, end;             /* base and limit of free run */

  AVER(AddrIsAligned(base, ChunkPageSize(chunk)));
  AVER(AddrIsAligned(limit, ChunkPageSize(chunk)));
  AVER(chunk->base <= base);
  AVER(base < limit);
  AVER(limit <= chunk->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, ChunkPageSize(chunk)));

  basePage = INDEX_OF_ADDR(chunk, base);
  limitPage = INDEX_OF_ADDR(chunk, limit);
  pages = ChunkSizeToPages(chunk, size);

  if (downwards) {
    if (!BTFindShortResRangeHigh(&start, &end, chunk->allocTable,
                                 basePage, limitPage, pages))
      return FALSE;
  } else {
    if(!BTFindShortResRange(&start, &end, chunk->allocTable,
                            basePage, limitPage, pages))
      return FALSE;
  }

  *baseReturn = start;
  return TRUE;
}


/* pagesFindFreeInZones -- find a range of free pages with a ZoneSet
 *
 * This function finds the intersection of ZoneSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a page whose ZoneSet (see
 * ZoneSetOfPage) will be a subset of the specified ZoneSet.
 *
 * For meaning of downwards arg see pagesFindFreeInArea.
 * .improve.findfree.downwards: This should be improved so that it
 * allocates pages from top down globally, as opposed to (currently)
 * just within an interval.
 */
static Bool pagesFindFreeInZones(Index *baseReturn, VMChunk *chunkReturn,
                                 VMArena vmArena, Size size, ZoneSet zones,
                                 Bool downwards)
{
  Arena arena;
  Addr chunkBase, base, limit;
  Size zoneSize;
  Ring node, next;

  arena = VMArena2Arena(vmArena);
  zoneSize = (Size)1 << arena->zoneShift;

  /* Try to reuse single pages from the already-mapped spare pages list */
  if (size == ArenaAlign(arena)) {
    Index i;
    for (i = 0; i < NELEMS(vmArena->freeRing); ++i) {
      Ring ring = &vmArena->freeRing[i];
      if (ZoneSetIsMember(zones, i) && !RingIsSingle(ring)) {
        Page page = PageOfFreeRing(RingNext(ring));
        Chunk chunk;
        Bool b = ChunkOfAddr(&chunk, arena, (Addr)page);
        AVER(b);
        *baseReturn = (Index)(page - chunk->pageTable);
        *chunkReturn = Chunk2VMChunk(chunk);
        return TRUE;
      }
    }
  }

  /* Should we check chunk cache first? */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    AVERT(Chunk, chunk);

    /* .alloc.skip: The first address available for arena allocation, */
    /* is just after the arena tables. */
    chunkBase = PageIndexBase(chunk, chunk->allocBase);

    base = chunkBase;
    while(base < chunk->limit) {
      if (ZoneSetHasAddr(arena, zones, base)) {
        /* Search for a run of zone stripes which are in the ZoneSet */
        /* and the arena.  Adding the zoneSize might wrap round (to */
        /* zero, because limit is aligned to zoneSize, which is a */
        /* power of two). */
        limit = base;
        do {
          /* advance limit to next higher zone stripe boundary */
          limit = AddrAlignUp(AddrAdd(limit, 1), zoneSize);

          AVER(limit > base || limit == (Addr)0);

          if (limit >= chunk->limit || limit < base) {
            limit = chunk->limit;
            break;
          }

          AVER(base < limit);
          AVER(limit < chunk->limit);
        } while(ZoneSetHasAddr(arena, zones, limit));

        /* If the ZoneSet was universal, then the area found ought to */
        /* be the whole chunk. */
        AVER(zones != ZoneSetUNIV
             || (base == chunkBase && limit == chunk->limit));

        /* Try to allocate a page in the area. */
        if (AddrOffset(base, limit) >= size
            && pagesFindFreeInArea(baseReturn, chunk, size, base, limit,
                                   downwards)) {
          *chunkReturn = Chunk2VMChunk(chunk);
          return TRUE;
        }
       
        base = limit;
      } else {
        /* Adding the zoneSize might wrap round (to zero, because */
        /* base is aligned to zoneSize, which is a power of two). */
        base = AddrAlignUp(AddrAdd(base, 1), zoneSize);
        AVER(base > chunkBase || base == (Addr)0);
        if (base >= chunk->limit || base < chunkBase) {
          base = chunk->limit;
          break;
        }
      }
    }

    AVER(base == chunk->limit);
  }

  return FALSE;
}


/* vmGenOfSegPref -- return generation specified by a segment preference */

static Serial vmGenOfSegPref(VMArena vmArena, SegPref pref)
{
  Serial gen;

  AVER(pref->isGen);
  UNUSED(vmArena);

  gen = pref->gen;
  if (gen >= VMArenaGenCount) {
    gen = VMArenaGenCount - 1;
  }
  return gen;
}


/* pagesFindFreeWithSegPref -- find a range of free pages with given preferences
 *
 * Note this does not create or allocate any pages.
 *
 * basereturn: return parameter for the index in the
 *   chunk's page table of the base of the free area found.
 * chunkreturn: return parameter for the chunk in which
 *   the free space has been found.
 * pref: the SegPref object to be used when considering
 *   which zones to try.
 * size: Size to find space for.
 * barge: TRUE iff stealing space in zones used
 *   by other SegPrefs should be considered (if it's FALSE then only
 *   zones already used by this segpref or free zones will be used).
 */
static Bool pagesFindFreeWithSegPref(Index *baseReturn, VMChunk *chunkReturn,
                                     VMArena vmArena, SegPref pref, Size size,
                                     Bool barge)
{
  ZoneSet preferred;

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    preferred = vmArena->genZoneSet[gen];
  } else {
    preferred = pref->zones;
  }

  /* @@@@ Some of these tests might be duplicates.  If we're about */
  /* to run out of virtual address space, then slow allocation is */
  /* probably the least of our worries. */

  /* .alloc.improve.map: Define a function that takes a list */
  /* (say 4 long) of ZoneSets and tries pagesFindFreeInZones on */
  /* each one in turn.  Extra ZoneSet args that weren't needed */
  /* could be ZoneSetEMPTY */

  if (pref->isCollected) { /* GC'd memory */
    /* We look for space in the following places (in order) */
    /*   - Zones already allocated to me (preferred) but are not */
    /*     blacklisted; */
    /*   - Zones that are either allocated to me, or are unallocated */
    /*     but not blacklisted; */
    /*   - Any non-blacklisted zone; */
    /*   - Any zone; */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated (or the default */
    /* is used). */
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetDiff(preferred, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUnion(preferred,
                                             ZoneSetDiff(vmArena->freeSet,
                                                         vmArena->blacklist)),
                                pref->high)) {
      return TRUE; /* found */
    }
    if (!barge)
      /* do not barge into other zones, give up now */
      return FALSE;
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetDiff(ZoneSetUNIV, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUNIV, pref->high)) {
      return TRUE; /* found */
    }
  } else { /* non-GC'd memory */
    /* We look for space in the following places (in order) */
    /*   - Zones preferred (preferred) and blacklisted; */
    /*   - Zones preferred; */
    /*   - Zones preferred or blacklisted zone; */
    /*   - Any zone. */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated. */
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetInter(preferred, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                preferred, pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUnion(preferred, vmArena->blacklist),
                                pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUNIV, pref->high)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* vmArenaExtend -- Extend the arena by making a new chunk
 *
 * The size arg specifies how much we wish to allocate after the extension.
 */
static Res vmArenaExtend(VMArena vmArena, Size size)
{
  Chunk newChunk;
  Size chunkSize;
  Res res;

  /* Choose chunk size. */
  /* .vmchunk.overhead: This code still lacks a proper estimate of */
  /* the overhead required by a vmChunk for chunkStruct, page tables */
  /* etc.  For now, estimate it as 10%.  RHSK 2007-12-21 */
  do {
    Size fraction = 10;  /* 10% -- see .vmchunk.overhead */
    Size chunkOverhead;
    
    /* 1: use extendBy, if it is big enough for size + overhead */
    chunkSize = vmArena->extendBy;
    chunkOverhead = chunkSize / fraction;
    if(chunkSize > size && (chunkSize - size) >= chunkOverhead)
      break;
    
    /* 2: use size + overhead (unless it overflows SizeMAX) */
    chunkOverhead = size / (fraction - 1);
    if((SizeMAX - size) >= chunkOverhead) {
      chunkSize = size + chunkOverhead;
      break;
    }
    
    /* 3: use SizeMAX */
    chunkSize = SizeMAX;
    break;
  } while(0);

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
      for(; chunkSize > chunkHalf; chunkSize -= sliceSize) {
        if(chunkSize < chunkMin) {
          EVENT2(vmArenaExtendFail, chunkMin,
                 VMArenaReserved(VMArena2Arena(vmArena)));
          return ResRESOURCE;
        }
        res = VMChunkCreate(&newChunk, vmArena, chunkSize);
        if(res == ResOK)
          goto vmArenaExtend_Done;
      }
    }
  }

vmArenaExtend_Done:
  EVENT2(vmArenaExtendDone, chunkSize, VMArenaReserved(VMArena2Arena(vmArena)));
  vmArena->extended(VMArena2Arena(vmArena),
		    newChunk->base,
		    AddrOffset(newChunk->base, newChunk->limit));

  return res;
}


/* VM*AllocPolicy -- allocation policy methods */


/* Used in abstracting allocation policy between VM and VMNZ */
typedef Res (*VMAllocPolicyMethod)(Index *, VMChunk *, VMArena, SegPref, Size);

static Res VMAllocPolicy(Index *baseIndexReturn, VMChunk *chunkReturn,
                         VMArena vmArena, SegPref pref, Size size)
{
  if (!pagesFindFreeWithSegPref(baseIndexReturn, chunkReturn,
                                vmArena, pref, size, FALSE)) {
    /* try and extend, but don't worry if we can't */
    (void)vmArenaExtend(vmArena, size);

    /* We may or may not have a new chunk at this point */
    /* we proceed to try the allocation again anyway. */
    /* We specify barging, but if we have got a new chunk */
    /* then hopefully we won't need to barge. */
    if (!pagesFindFreeWithSegPref(baseIndexReturn, chunkReturn,
                                  vmArena, pref, size, TRUE)) {
      /* .improve.alloc-fail: This could be because the request was */
      /* too large, or perhaps the arena is fragmented.  We could */
      /* return a more meaningful code. */
      return ResRESOURCE;
    }
  }
  return ResOK;
}

static Res VMNZAllocPolicy(Index *baseIndexReturn, VMChunk *chunkReturn,
                           VMArena vmArena, SegPref pref, Size size)
{
  if (pagesFindFreeInZones(baseIndexReturn, chunkReturn, vmArena, size,
                           ZoneSetUNIV, pref->high)) {
    return ResOK;
  }
  return ResRESOURCE;
}


/* pageDescIsMapped -- is the page descriptor for a page mapped? */

static Bool pageDescIsMapped(VMChunk vmChunk, Index pi)
{
  Index pageTableBaseIndex;
  Index pageTableLimitIndex;
  Chunk chunk = VMChunk2Chunk(vmChunk);
  
  AVER(pi < chunk->pages);

  /* Note that unless the pi'th PageUnion crosses a page boundary */
  /* Base and Limit will differ by exactly 1. */
  /* They will differ by at most 2 assuming that */
  /* sizeof(PageUnion) <= ChunkPageSize(chunk) (!) */
  tablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex, chunk, pi, pi+1);
  /* using unsigned arithmetic overflow to use just one comparison */
  AVER(pageTableLimitIndex - pageTableBaseIndex - 1 < 2);

  /* We can examine the page descriptor iff both table pages */
  /* are mapped. */
  return BTGet(vmChunk->pageTableMapped, pageTableBaseIndex) &&
         BTGet(vmChunk->pageTableMapped, pageTableLimitIndex - 1);
}


/* pageState -- determine page state, even if unmapped
 *
 * Parts of the page table may be unmapped if their corresponding pages are
 * free.
 */

static unsigned pageState(VMChunk vmChunk, Index pi)
{
  Chunk chunk = VMChunk2Chunk(vmChunk);
  if (pageDescIsMapped(vmChunk, pi))
    return PageState(&chunk->pageTable[pi]);
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
  Page page = &chunk->pageTable[pi];

  AVER(PageState(page) == PageStateSPARE);
  AVER(arena->spareCommitted >= ChunkPageSize(chunk));

  arena->spareCommitted -= ChunkPageSize(chunk);
  RingRemove(PageSpareRing(page));
  RingRemove(PageFreeRing(page));
}


/* tablePagesUnmap -- unmap page table pages describing a page range
 *
 * The pages in the range [basePage, limitPage) have been freed, and this
 * function then attempts to unmap the corresponding part of the page
 * table.  This may not be possible because other parts of those pages may
 * be in use.  This function extends the range as far as possible across
 * free pages, so that such cases will be cleaned up eventually.
 */

static void tablePagesUnmap(VMChunk vmChunk, Index basePage, Index limitPage)
{
  Index basePTI, limitPTI;
  Addr base, limit;
  Chunk chunk;
  
  chunk = VMChunk2Chunk(vmChunk);
  AVER(basePage < chunk->pages);
  AVER(limitPage <= chunk->pages);
  AVER(basePage < limitPage);

  /* Now attempt to unmap the part of the page table that's no longer
     in use because we've made a run of pages free.  This scan will
     also catch any adjacent unused pages, though they ought to have
     been caught by previous scans. */

  /* Lower basePage until we reach a desciptor we can't unmap, or the
     beginning of the table. */
  while (basePage > 0 &&
         pageDescIsMapped(vmChunk, basePage) &&
         PageState(&chunk->pageTable[basePage]) == PageStateFREE)
    --basePage;

  /* Raise limitPage until we reach a descriptor we can't unmap, or the end
     of the table. */
  while (limitPage < chunk->pages &&
         pageDescIsMapped(vmChunk, limitPage) &&
         PageState(&chunk->pageTable[limitPage]) == PageStateFREE)
    ++limitPage;

  /* Calculate the range of pages in the page table. */
  tablePagesUsed(&basePTI, &limitPTI, chunk, basePage, limitPage);
  base = TablePageIndexBase(chunk, basePTI);
  limit = TablePageIndexBase(chunk, limitPTI);

  /* If we can't unmap the base page, step up. */
  if (!pageDescIsMapped(vmChunk, basePage) ||
      PageState(&chunk->pageTable[basePage]) != PageStateFREE)
    base = AddrAdd(base, chunk->pageSize);
  /* If that leaves any pages, then if the limit page contains a desciptor
     we can't unmap, step down.  Note, limit is the base of the page table
     page *after* the one containing the desc for limitPage. */
  if (base < limit) {
    if (limitPage < chunk->pages &&
        pageState(vmChunk, limitPage) != PageStateFREE)
      limit = AddrSub(limit, chunk->pageSize);
    /* If that leaves any pages, unmap them. */
    if (base < limit) {
      vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm, base, limit);
      BTResRange(vmChunk->pageTableMapped,
                 PageTablePageIndex(chunk, base),
                 PageTablePageIndex(chunk, limit));
    }
  }
}


/* pagesMarkAllocated -- Mark the pages allocated */

static Res pagesMarkAllocated(VMArena vmArena, VMChunk vmChunk,
                              Index baseIndex, Count pages, Pool pool)
{
  Index i, mappedLimit, limitIndex;
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Res res;

  /* Ensure that the page descriptors we need are on mapped pages. */
  limitIndex = baseIndex + pages;
  AVER(limitIndex <= chunk->pages);
  res = tablePagesEnsureMapped(vmChunk, baseIndex, limitIndex);
  if (res != ResOK)
    goto failTableMap;

  /* We're not expecting zero-sized allocations. */
  AVER(baseIndex < limitIndex);

  i = baseIndex;
  mappedLimit = baseIndex;
  while (i < limitIndex) {
    Addr freeBase;

    /* Allocate a run of spare pages. */
    while(i < limitIndex && PageState(&chunk->pageTable[i]) == PageStateSPARE) {
      sparePageRelease(vmChunk, i);
      PageAlloc(chunk, i, pool);
      ++i;
    }
    
    if (i >= limitIndex)
      return ResOK;

    /* Allocate a run of free pages. */
    freeBase = PageIndexBase(chunk, i);
    AVER(PageState(&chunk->pageTable[i]) == PageStateFREE);
    while (i < limitIndex && PageState(&chunk->pageTable[i]) == PageStateFREE) {
      PageAlloc(chunk, i, pool);
      ++i;
    }

    /* Map the memory for those free pages. */
    res = vmArenaMap(vmArena, vmChunk->vm, freeBase, PageIndexBase(chunk, i));
    if (res != ResOK)
      goto failPagesMap;
    mappedLimit = i;
  }

  return ResOK;

failPagesMap:
  /* region from baseIndex to mappedLimit needs unmapping */
  /* TODO: Consider making them spare instead, then purging. */
  if (baseIndex < mappedLimit) {
    vmArenaUnmap(vmArena, vmChunk->vm,
                 PageIndexBase(chunk, baseIndex),
                 PageIndexBase(chunk, mappedLimit));
  }
  while (i > baseIndex) {
    --i;
    TractFinish(PageTract(&chunk->pageTable[i]));
    PageFree(chunk, i);
  }
  tablePagesUnmap(vmChunk, baseIndex, limitIndex);
failTableMap:
  return res;
}


/* vmAllocComm -- allocate a region from the arena
 *
 * Common code used by mps_arena_class_vm and
 * mps_arena_class_vmnz.
 */
static Res vmAllocComm(Addr *baseReturn, Tract *baseTractReturn,
                       VMAllocPolicyMethod policy,
                       SegPref pref, Size size, Pool pool)
{
  Addr base, limit;
  Tract baseTract;
  Arena arena;
  Count pages;
  Index baseIndex;
  ZoneSet zones;
  Res res;
  VMArena vmArena;
  VMChunk vmChunk;
  Chunk chunk;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVER(FunCheck((Fun)policy));
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
 
  /* NULL is used as a discriminator */
  /* (see <design/arenavm/table.disc>) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  /* Early check on commit limit. */
  if (arena->spareCommitted < size) {
    Size necessaryCommitIncrease = size - arena->spareCommitted;
    if (arena->committed + necessaryCommitIncrease > arena->commitLimit
        || arena->committed + necessaryCommitIncrease < arena->committed) {
      return ResCOMMIT_LIMIT;
    }
  }

  res = (*policy)(&baseIndex, &vmChunk, vmArena, pref, size);
  if (res != ResOK)
    return res;

  /* chunk (and baseIndex) should be initialised by policy */
  AVERT(VMChunk, vmChunk);
  chunk = VMChunk2Chunk(vmChunk);

  /* Compute number of pages to be allocated. */
  pages = ChunkSizeToPages(chunk, size);

  res = pagesMarkAllocated(vmArena, vmChunk, baseIndex, pages, pool);
  while (res != ResOK) {
    /* Try purging spare pages in the hope that the OS will give them back
       at the new address.  Will eventually run out of spare pages, so this
       loop will terminate. */
    /* TODO: Investigate implementing VMRemap so that we can guarantee
       success if we have enough spare pages. */
    if (VMPurgeSpare(arena, size) == 0)
      goto failPagesMap;
    res = pagesMarkAllocated(vmArena, vmChunk, baseIndex, pages, pool);
  }

  base = PageIndexBase(chunk, baseIndex);
  baseTract = PageTract(&chunk->pageTable[baseIndex]);
  limit = AddrAdd(base, size);
  zones = ZoneSetOfRange(arena, base, limit);

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    if (!ZoneSetSuper(vmArena->genZoneSet[gen], zones)) {
      /* Tracking the whole zoneset for each generation number gives
       * more understandable telemetry than just reporting the added
       * zones. */
      EVENT3(ArenaGenZoneAdd, arena, gen, ZoneSetUnion(vmArena->genZoneSet[gen], zones));
    }
            
    vmArena->genZoneSet[gen] = ZoneSetUnion(vmArena->genZoneSet[gen], zones);
  }

  if (ZoneSetInter(vmArena->freeSet, zones) != ZoneSetEMPTY) {
      EVENT2(ArenaUseFreeZone, arena, ZoneSetInter(vmArena->freeSet, zones));
  }
  vmArena->freeSet = ZoneSetDiff(vmArena->freeSet, zones);
 
  *baseReturn = base;
  *baseTractReturn = baseTract;
  return ResOK;

failPagesMap:
  return res;
}


static Res VMAlloc(Addr *baseReturn, Tract *baseTractReturn,
                   SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common vmAllocComm */
  return vmAllocComm(baseReturn, baseTractReturn,
                     VMAllocPolicy, pref, size, pool);
}

static Res VMNZAlloc(Addr *baseReturn, Tract *baseTractReturn,
                     SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common vmAllocComm */
  return vmAllocComm(baseReturn, baseTractReturn,
                     VMNZAllocPolicy, pref, size, pool);
}


/* chunkUnmapAroundPage -- unmap spare pages in a chunk including this one
 *
 * Unmap the spare page passed, and possibly other pages in the chunk,
 * attempting to stop when the amount of unmapped memory reaches the size
 * passed.  May exceed expectation.  Returns the amount of memory unmapped.
 *
 * To minimse calls to the OS, the page passed is coalesced with
 * spare pages above and below, even though these may have been more recently
 * made spare.
 */

static Size chunkUnmapAroundPage(Chunk chunk, Size size, Page page)
{
  VMChunk vmChunk;
  Size purged = 0;
  Size pageSize;
  Index basePage, limitPage;

  AVERT(Chunk, chunk);
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(VMChunk, vmChunk);
  AVER(PageState(page) == PageStateSPARE);
  /* size is arbitrary */

  pageSize = ChunkPageSize(chunk);

  /* Not required by this code, but expected. */
  AVER(SizeIsAligned(size, pageSize));

  
  basePage = (Index)(page - chunk->pageTable);
  limitPage = basePage;

  do {
    sparePageRelease(vmChunk, limitPage);
    PageInit(chunk, limitPage);
    ++limitPage;
    purged += pageSize;
  } while (purged < size &&
           limitPage < chunk->pages &&
           pageState(vmChunk, limitPage) == PageStateSPARE);
  while (purged < size &&
         basePage > 0 &&
         pageState(vmChunk, basePage - 1) == PageStateSPARE) {
    --basePage;
    sparePageRelease(vmChunk, basePage);
    PageInit(chunk, basePage);
    purged += pageSize;
  }

  vmArenaUnmap(VMChunkVMArena(vmChunk),
               vmChunk->vm,
               PageIndexBase(chunk, basePage),
               PageIndexBase(chunk, limitPage));

  tablePagesUnmap(vmChunk, basePage, limitPage);

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
  VMChunk vmChunk;
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
  vmChunk = Chunk2VMChunk(chunk);

  /* Calculate the number of pages in the region */
  pages = ChunkSizeToPages(chunk, size);
  piBase = INDEX_OF_ADDR(chunk, base);
  piLimit = piBase + pages;
  AVER(piBase < piLimit);
  AVER(piLimit <= chunk->pages);

  /* loop from pageBase to pageLimit-1 inclusive */
  /* Finish each Tract found, then convert them to spare pages. */
  for(pi = piBase; pi < piLimit; ++pi) {
    Page page = &chunk->pageTable[pi];
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);

    TractFinish(tract);
    PageSetPool(page, NULL);
    PageSetType(page, PageStateSPARE);
    /* We must init the page's rings because it is a union with the
       tract and will contain junk. */
    RingInit(PageSpareRing(page));
    RingAppend(&vmArena->spareRing, PageSpareRing(page));
    RingInit(PageFreeRing(page));
    RingInsert(&vmArena->freeRing[AddrZone(arena, PageIndexBase(chunk, pi))],
               PageFreeRing(page));
  }
  arena->spareCommitted += ChunkPagesToSize(chunk, piLimit - piBase);
  BTResRange(chunk->allocTable, piBase, piLimit);

  /* Consider returning memory to the OS. */
  /* TODO: Chunks are only destroyed when ArenaCompact is called, and that is
     only called from TraceReclaim.  Should consider destroying chunks here. */
  if (arena->spareCommitted > arena->spareCommitLimit)
    (void)VMPurgeSpare(arena, arena->spareCommitted - arena->spareCommitLimit);
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

  /* Destroy any empty chunks (except the primary). */
  /* TODO: Avoid a scan of the allocTable by keeping a count of allocated
     pages in a chunk. */
  /* TODO: Avoid oscillations in chunk creation by adding some hysteresis. */
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
  this->alloc = VMAlloc;
  this->free = VMFree;
  this->chunkInit = VMChunkInit;
  this->chunkFinish = VMChunkFinish;
  this->compact = VMCompact;
  this->describe = VMArenaDescribe;
}


/* VMNZArenaClass  -- The VMNZ arena class definition
 *
 * VMNZ is just VMArena with a different allocation policy.
 */
DEFINE_ARENA_CLASS(VMNZArenaClass, this)
{
  INHERIT_CLASS(this, VMArenaClass);
  this->name = "VMNZ";
  this->alloc = VMNZAlloc;
}


/* mps_arena_class_vm -- return the arena class VM */

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)VMArenaClassGet();
}


/* mps_arena_class_vmnz -- return the arena class VMNZ */

mps_arena_class_t mps_arena_class_vmnz(void)
{
  return (mps_arena_class_t)VMNZArenaClassGet();
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
