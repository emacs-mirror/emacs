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
#include "cbs.h"
#include "poolmfs.h"

SRCID(arenavm, "$Id$");


/* VMChunk -- chunks for VM arenas */

typedef struct VMChunkStruct *VMChunk;

#define VMChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMChunkStruct {
  ChunkStruct chunkStruct;      /* generic chunk */
  VM vm;                        /* virtual memory handle */
  Addr overheadMappedLimit;           /* limit of pages mapped for overhead */
  BT pageTableMapped;           /* indicates mapped state of page table */
  BT noSparePages;             /* 1 bit per page of pageTable */
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
  ZoneSet freeSet;               /* unassigned zones */
  Size extendBy;                /* desired arena increment */
  Size extendMin;               /* minimum arena increment */
  ArenaVMExtendedCallback extended;
  ArenaVMContractedCallback contracted;
  Sig sig;                      /* <design/sig/> */
} VMArenaStruct;

#define Arena2VMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)
#define VMArena2Arena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static void sparePagesPurge(VMArena vmArena);
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
  /* check pageTableMapped table */
  CHECKL(vmchunk->pageTableMapped != NULL);
  CHECKL((Addr)vmchunk->pageTableMapped >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->pageTableMapped, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadMappedLimit);
  /* check noSparePages table */
  CHECKL(vmchunk->noSparePages != NULL);
  CHECKL((Addr)vmchunk->noSparePages >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->noSparePages, BTSize(chunk->pageTablePages))
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


/* pageIsSpare -- is page spare (free and mapped)? */

#define pageIsSpare(page) \
  ((page)->the.rest.pool == NULL && (page)->the.rest.type == PageTypeSpare)


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
  CHECKL(vmArena->blacklist != ZoneSetUNIV);

  /* FIXME: Test for vmArena->freeSet? */
  CHECKL(vmArena->extendBy > 0);
  CHECKL(vmArena->extendMin <= vmArena->extendBy);

  if (arena->primary != NULL) {
    primary = Chunk2VMChunk(arena->primary);
    CHECKD(VMChunk, primary);
    /* We could iterate over all chunks accumulating an accurate */
    /* count of committed, but we don't have all day. */
    CHECKL(VMMapped(primary->vm) <= arena->committed);
  }
  
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

  /* Add the chunk's free address space to the arena's freeCBS, so that
     we can allocate from it. */
  /* FIXME: Should be in generic ChunkInit so other arenas get the effect. */
  {
    Arena arena = VMArena2Arena(vmArena);
    Chunk chunk = VMChunk2Chunk(vmChunk);
    res = ArenaFreeCBSInsert(arena,
                             PageIndexBase(chunk, chunk->allocBase),
                             chunk->limit);
    if (res != ResOK)
        goto failCBSInsert;
  }

  *chunkReturn = VMChunk2Chunk(vmChunk);
  return ResOK;

failCBSInsert:
  ChunkFinish(VMChunk2Chunk(vmChunk));
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
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failnoSparePages;
  vmChunk->noSparePages = p;

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
  BTSetRange(vmChunk->noSparePages, 0, chunk->pageTablePages);
  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failnoSparePages:
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
  AVER(BTIsSetRange(vmChunk->noSparePages, 0, chunk->pageTablePages));
  AVER(BTIsResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages));

  /* FIXME: Should be in generic ChunkFinish so other arenas get the effect. */
  ArenaFreeCBSDelete(ChunkArena(chunk),
                     PageIndexBase(chunk, chunk->allocBase),
                     chunk->limit);

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
  res = ArenaInit(arena, class, VMAlign(arenaVM));
  if (res != ResOK)
    goto failArenaInit;
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->spareSize = 0;

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
    vmArena->blacklist = ZoneSetAdd(arena, vmArena->blacklist, nono.addr);
    nono.i = -1;
    vmArena->blacklist = ZoneSetAdd(arena, vmArena->blacklist, nono.addr);
    nono.l = 1;
    vmArena->blacklist = ZoneSetAdd(arena, vmArena->blacklist, nono.addr);
    nono.l = -1;
    vmArena->blacklist = ZoneSetAdd(arena, vmArena->blacklist, nono.addr);
  }
  EVENT2(ArenaBlacklistZone, vmArena, vmArena->blacklist);
  
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

  sparePagesPurge(vmArena);
  /* destroy all chunks, including the primary */
  arena->primary = NULL;
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    vmChunkDestroy(chunk);
  }
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


/* VMArenaSpareCommitExceeded -- handle excess spare pages */

static void VMArenaSpareCommitExceeded(Arena arena)
{
  VMArena vmArena;

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);

  sparePagesPurge(vmArena);
  return;
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
    + ChunkPageSize(chunk) - 1) \
   / sizeof(PageStruct) \
   + 1)

/* tablePageWholeLimitIndex
 *
 * Index of the first page descriptor falling partially on the next
 * table page.
 */
#define tablePageWholeLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + ChunkPageSize(chunk)) \
   / sizeof(PageStruct))


/* tablePageInUse -- Check whether a given page of the page table is in use
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if
 * any of the page descriptors falling on it (even partially) are being
 * used.  Relies on .repr.table-page and .vm.addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're (de)allocating.
 */
static Bool tablePageInUse(Chunk chunk, Addr tablePage)
{
  Index limitIndex;

  /* Check it's in the page table. */
  AVER((Addr)&chunk->pageTable[0] <= tablePage);
  AVER(tablePage < addrOfPageDesc(chunk, chunk->pages));

  if (tablePage == AddrPageBase(chunk, addrOfPageDesc(chunk, chunk->pages))) {
    limitIndex = chunk->pages;
  } else {
    limitIndex = tablePageLimitIndex(chunk, tablePage);
  }
  AVER(limitIndex <= chunk->pages);

  return !BTIsResRange(chunk->allocTable,
                       tablePageBaseIndex(chunk, tablePage), limitIndex);
}


/* tablePagesUsed
 *
 * Takes a range of pages identified by [pageBase, pageLimit), and
 * returns the pages occupied by the page table which store the
 * PageStruct descriptors for those pages.
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


/* tablePagesUnmapUnused
 *
 * Of the pages occupied by the page table from tablePageBase to
 * tablePageLimit find those which are wholly unused and unmap them.
 */
static void tablePagesUnmapUnused(VMChunk vmChunk,
                                  Addr tablePageBase, Addr tablePageLimit)
{
  Chunk chunk;
  Addr cursor;
  Size pageSize;

  chunk = VMChunk2Chunk(vmChunk);
  pageSize = ChunkPageSize(chunk);
  AVER(AddrIsAligned(tablePageBase, pageSize));
  AVER(AddrIsAligned(tablePageLimit, pageSize));

  /* for loop indexes over base addresses of pages occupied by page table */
  for(cursor = tablePageBase;
      cursor < tablePageLimit;
      cursor = AddrAdd(cursor, pageSize)) {
    if (!tablePageInUse(chunk, cursor)) {
      vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
                   cursor, AddrAdd(cursor, pageSize));
      AVER(BTGet(vmChunk->noSparePages, PageTablePageIndex(chunk, cursor)));
      AVER(BTGet(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor)));
      BTRes(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor));
    }
  }
  AVER(cursor == tablePageLimit);

  return;
}


/* vmArenaGrow -- Extend the arena by making a new chunk
 *
 * The size arg specifies how much we wish to allocate after the extension.
 */
static Res vmArenaGrow(Arena arena, SegPref pref, Size size)
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


/* pageIsMapped -- checks whether a free page is mapped or not. */

static Bool pageIsMapped(VMChunk vmChunk, Index pi)
{
  Index pageTableBaseIndex;
  Index pageTableLimitIndex;
  int pageType;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  /* Note that unless the pi'th PageStruct crosses a page boundary */
  /* Base and Limit will differ by exactly 1. */
  /* They will differ by at most 2 assuming that */
  /* sizeof(PageStruct) <= ChunkPageSize(chunk) (!) */
  tablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex, chunk, pi, pi+1);
  /* using unsigned arithmetic overflow to use just one comparison */
  AVER(pageTableLimitIndex - pageTableBaseIndex - 1 < 2);

  /* We can examine the PageStruct descriptor iff both table pages */
  /* are mapped. */
  if (BTGet(vmChunk->pageTableMapped, pageTableBaseIndex)
      && BTGet(vmChunk->pageTableMapped, pageTableLimitIndex - 1)) {
    pageType = PageType(&chunk->pageTable[pi]);
    if (pageType == PageTypeSpare)
      return TRUE;
    AVER(pageType == PageTypeFree);
  }
  return FALSE;
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

  AVER(PageType(&chunk->pageTable[pi]) == PageTypeSpare);
  AVER(arena->spareCommitted >= ChunkPageSize(chunk));
  arena->spareCommitted -= ChunkPageSize(chunk);
  return;
}


/* pagesMarkAllocated -- Mark the pages allocated */

static Res pagesMarkAllocated(VMArena vmArena, VMChunk vmChunk,
                              Index baseIndex, Count pages, Pool pool)
{
  Index i;
  Index limitIndex;
  Index mappedBase, mappedLimit;
  Index unmappedBase, unmappedLimit;
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Res res;

  /* Ensure that the page descriptors we need are on mapped pages. */
  limitIndex = baseIndex + pages;
  res = tablePagesEnsureMapped(vmChunk, baseIndex, limitIndex);
  if (res != ResOK)
    goto failTableMap;

  mappedBase = baseIndex;
  mappedLimit = mappedBase;

  do {
    while(pageIsMapped(vmChunk, mappedLimit)) {
      ++mappedLimit;
      if (mappedLimit >= limitIndex)
        break;
    }
    AVER(mappedLimit <= limitIndex);
    /* NB for loop will loop 0 times iff first page is not mapped */
    for(i = mappedBase; i < mappedLimit; ++i) {
      sparePageRelease(vmChunk, i);
      PageAlloc(chunk, i, pool);
    }
    if (mappedLimit >= limitIndex)
      break;
    unmappedBase = mappedLimit;
    unmappedLimit = unmappedBase;
    while(!pageIsMapped(vmChunk, unmappedLimit)) {
      ++unmappedLimit;
      if (unmappedLimit >= limitIndex)
        break;
    }
    AVER(unmappedLimit <= limitIndex);
    res = vmArenaMap(vmArena, vmChunk->vm,
                     PageIndexBase(chunk, unmappedBase),
                     PageIndexBase(chunk, unmappedLimit));
    if (res != ResOK)
      goto failPagesMap;
    for(i = unmappedBase; i < unmappedLimit; ++i) {
      PageAlloc(chunk, i, pool);
    }
    mappedBase = unmappedLimit;
    mappedLimit = mappedBase;
  } while(mappedLimit < limitIndex);
  AVER(mappedLimit == limitIndex);

  return ResOK;

failPagesMap:
  /* region from baseIndex to mappedLimit needs unmapping */
  if (baseIndex < mappedLimit) {
    vmArenaUnmap(vmArena, vmChunk->vm,
                 PageIndexBase(chunk, baseIndex),
                 PageIndexBase(chunk, mappedLimit));
    /* mark pages as free */
    for(i = baseIndex; i < mappedLimit; ++i) {
      TractFinish(PageTract(&chunk->pageTable[i]));
      PageFree(chunk, i);
    }
  }
  {
    Index pageTableBaseIndex, pageTableLimitIndex;
    /* find which pages of page table were affected */
    tablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex,
                   chunk, baseIndex, limitIndex);
    /* Resetting the noSparePages bits is lazy, it means that */
    /* we don't have to bother trying to unmap unused portions */
    /* of the pageTable. */
    BTResRange(vmChunk->noSparePages, pageTableBaseIndex, pageTableLimitIndex);
  }
failTableMap:
  return res;
}

static Res VMPagesMarkAllocated(Arena arena, Chunk chunk,
                                Index baseIndex, Count pages, Pool pool)
{
  return pagesMarkAllocated(Arena2VMArena(arena),
                            Chunk2VMChunk(chunk),
                            baseIndex,
                            pages,
                            pool);
}


/* spareRangesMap -- map a function over spare ranges
 *
 * The function f is called on the ranges of spare pages which are
 * within the range of pages from base to limit.  PageStruct descriptors
 * from base to limit should be mapped in the page table before calling
 * this function.
 */
typedef void (*spareRangesFn)(VMChunk, Index, Index, void *);

static void spareRangesMap(VMChunk vmChunk, Index base, Index limit,
                           spareRangesFn f, void *p)
{
  Index spareBase, spareLimit;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  AVER(base < limit);

  spareBase = base;
  do {
    while(!pageIsSpare(&chunk->pageTable[spareBase])) {
      ++spareBase;
      if (spareBase >= limit)
        goto done;
    }
    spareLimit = spareBase;
    while(pageIsSpare(&chunk->pageTable[spareLimit])) {
      ++spareLimit;
      if (spareLimit >= limit)
        break;
    }
    f(vmChunk, spareBase, spareLimit, p);
    spareBase = spareLimit;
  } while(spareBase < limit);
done:
  AVER(spareBase == limit);

  return;
}


/* vmArenaUnmapSpareRange
 *
 * Takes a range of spare pages and unmaps them, turning them into free pages.
 */
static void vmArenaUnmapSpareRange(VMChunk vmChunk,
                                   Index rangeBase, Index rangeLimit, void *p)
{
  Index i;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  UNUSED(p);
  for(i = rangeBase; i < rangeLimit; ++i) {
    sparePageRelease(vmChunk, i);
    PageInit(chunk, i);
  }
  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               PageIndexBase(chunk, rangeBase),
               PageIndexBase(chunk, rangeLimit));

  return;
}
 

/* sparePagesPurge -- all spare pages are found and purged (unmapped)
 *
 * This is currently the only way the spare pages are reduced.
 *
 * It uses the noSparePages bits to determine which areas of the
 * pageTable to examine.
 */
static void sparePagesPurge(VMArena vmArena)
{
  Ring node, next;
  Arena arena = VMArena2Arena(vmArena);

  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    VMChunk vmChunk = Chunk2VMChunk(chunk);
    Index spareBaseIndex, spareLimitIndex;
    Index tablePageCursor = 0;

    while(BTFindLongResRange(&spareBaseIndex, &spareLimitIndex,
                             vmChunk->noSparePages,
                             tablePageCursor, chunk->pageTablePages,
                             1)) {
      Addr spareTableBase, spareTableLimit;
      Index pageBase, pageLimit;
      Index tablePage;

      spareTableBase = TablePageIndexBase(chunk, spareBaseIndex);
      spareTableLimit = TablePageIndexBase(chunk, spareLimitIndex);
      /* Determine whether to use initial overlapping PageStruct. */
      if (spareBaseIndex > 0
          && !BTGet(vmChunk->pageTableMapped, spareBaseIndex - 1)) {
        pageBase = tablePageWholeBaseIndex(chunk, spareTableBase);
      } else {
        pageBase = tablePageBaseIndex(chunk, spareTableBase);
      }
      for(tablePage = spareBaseIndex; tablePage < spareLimitIndex;
          ++tablePage) {
        /* Determine whether to use final overlapping PageStruct. */
        if (tablePage == spareLimitIndex - 1
            && spareLimitIndex < chunk->pageTablePages
            && !BTGet(vmChunk->pageTableMapped, spareLimitIndex)) {
          pageLimit =
            tablePageWholeLimitIndex(chunk,
                                     TablePageIndexBase(chunk, tablePage));
        } else if (tablePage == chunk->pageTablePages - 1) {
          pageLimit = chunk->pages;
        } else {
          pageLimit =
            tablePageLimitIndex(chunk, TablePageIndexBase(chunk, tablePage));
        }
        if (pageBase < pageLimit) {
          spareRangesMap(vmChunk, pageBase, pageLimit,
                         vmArenaUnmapSpareRange, NULL);
        } else {
          /* Only happens for last page occupied by the page table */
          /* and only then when that last page has just the tail end */
          /* part of the last page descriptor and nothing more. */
          AVER(pageBase == pageLimit);
          AVER(tablePage == chunk->pageTablePages - 1);
        }
        BTSet(vmChunk->noSparePages, tablePage);
        pageBase = pageLimit;
      }
      tablePagesUnmapUnused(vmChunk, spareTableBase, spareTableLimit);
      tablePageCursor = spareLimitIndex;
      if (tablePageCursor >= chunk->pageTablePages) {
        AVER(tablePageCursor == chunk->pageTablePages);
        break;
      }
    }

  }

  AVER(arena->spareCommitted == 0);
  return;
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
  Index pageTableBase;
  Index pageTableLimit;
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
    PagePool(page) = NULL;
    PageType(page) = PageTypeSpare;
  }
  arena->spareCommitted += ChunkPagesToSize(chunk, piLimit - piBase);
  BTResRange(chunk->allocTable, piBase, piLimit);

  tablePagesUsed(&pageTableBase, &pageTableLimit, chunk, piBase, piLimit);
  BTResRange(vmChunk->noSparePages, pageTableBase, pageTableLimit);

  if (arena->spareCommitted > arena->spareCommitLimit) {
    sparePagesPurge(vmArena);
  }

  /* Chunks are only freed when ArenaCompact is called. */

  return;
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

      sparePagesPurge(vmArena);
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
  this->spareCommitExceeded = VMArenaSpareCommitExceeded;
  this->grow = vmArenaGrow;
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
