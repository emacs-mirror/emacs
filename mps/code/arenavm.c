/* impl.c.arenavm: VIRTUAL MEMORY ARENA CLASS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 *
 * DESIGN
 *
 * .design: See design.mps.arena.vm, and design.mps.arena.coop-vm
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
  BT noSparePages;             /* 1 bit per page of pageTable */
  Sig sig;                      /* design.mps.sig */
} VMChunkStruct;

#define VMChunk2Chunk(vmchunk) (&(vmchunk)->chunkStruct)
#define Chunk2VMChunk(chunk) PARENT(VMChunkStruct, chunkStruct, chunk)


/* VMChunkVMArena -- get the VM arena from a VM chunk */

#define VMChunkVMArena(vmchunk) \
  Arena2VMArena(ChunkArena(VMChunk2Chunk(vmchunk)))


/* VMArena
 *
 * See design.mps.arena.coop-vm.struct.vmarena for description.
 */

typedef struct VMArenaStruct *VMArena;

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VM vm;                        /* VM where the arena itself is stored */
  Size spareSize;              /* total size of spare pages */
  ZoneSet blacklist;             /* zones to use last */
  ZoneSet genZoneSet[VMArenaGenCount]; /* .gencount.const */
  ZoneSet freeSet;               /* unassigned zones */
  Size extendBy;
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;

#define Arena2VMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)
#define VMArena2Arena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static void sparePagesPurge(VMArena vmArena);
static ArenaClass VMArenaClassGet(void);
static ArenaClass VMNZArenaClassGet(void);


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
  Index gen;
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

  if (arena->primary != NULL) {
    primary = Chunk2VMChunk(arena->primary);
    CHECKD(VMChunk, primary);
    /* We could iterate over all chunks accumulating an accurate */
    /* count of committed, but we don't have all day. */
    CHECKL(VMMapped(primary->vm) <= arena->committed);
  }
  return TRUE;
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

  res = VMCreate(&vm, size);
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
  /* See design.mps.arena.@@@@ */
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
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failnoSparePages;
  vmChunk->noSparePages = p;

  /* Actually commit all the tables. design.mps.arena.vm.@@@@ */
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


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */
static Res VMArenaInit(Arena *arenaReturn, ArenaClass class, va_list args)
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

  userSize = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER(class == VMArenaClassGet() || class == VMNZArenaClassGet());
  AVER(userSize > 0);

  /* Create a VM to hold the arena and map it. */
  vmArenaSize = SizeAlignUp(sizeof(VMArenaStruct), MPS_PF_ALIGN);
  res = VMCreate(&arenaVM, vmArenaSize);
  if (res != ResOK)
    goto failVMCreate;
  res = VMMap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  if (res != ResOK)
    goto failVMMap;
  vmArena = (VMArena)VMBase(arenaVM);

  arena = VMArena2Arena(vmArena);
  /* impl.c.arena.init.caller */
  res = ArenaInit(arena, class);
  if (res != ResOK)
    goto failArenaInit;
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->spareSize = 0;

  /* .blacklist: We blacklist the zones corresponding to small integers. */
  vmArena->blacklist =
    ZoneSetAdd(arena, ZoneSetAdd(arena, ZoneSetEMPTY, (Addr)1), (Addr)-1);

  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genZoneSet[gen] = ZoneSetEMPTY;
  }
  vmArena->freeSet = ZoneSetUNIV; /* includes blacklist */
  /* design.mps.arena.coop-vm.struct.vmarena.extendby.init */
  vmArena->extendBy = userSize;

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
  /* the size is not a power of 2.  See design.mps.arena.class.fields. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);

  AVERT(VMArena, vmArena);
  if ((ArenaClass)mps_arena_class_vm() == class)
    EVENT_PWW(ArenaCreateVM, arena, userSize, chunkSize);
  else
    EVENT_PWW(ArenaCreateVMNZ, arena, userSize, chunkSize);
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
  /* destroy all chunks */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    vmChunkDestroy(chunk);
  }
  AVER(arena->committed == VMMapped(arenaVM));

  vmArena->sig = SigInvalid;

  ArenaFinish(arena); /* impl.c.global.finish.caller */

  VMUnmap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  VMDestroy(arenaVM);
  EVENT_P(ArenaDestroy, vmArena);
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

  /* Should we check chunk cache first? */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    AVERT(Chunk, chunk);

    /* .alloc.skip: The first address available for arena allocation, */
    /* is just after the arena tables. */
    chunkBase = PageIndexBase(chunk, chunk->allocBase);

    base = chunkBase;
    while(base < chunk->limit) {
      if (ZoneSetIsMember(arena, zones, base)) {
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

          AVER(base < limit && limit < chunk->limit);
        } while(ZoneSetIsMember(arena, zones, limit));

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

  /* .improve.debug: @@@@ chunkSize (calculated below) won't */
  /* be big enough if the tables of the new chunk are */
  /* more than vmArena->extendBy (because there will be fewer than */
  /* size bytes free in the new chunk).  Fix this. */
  chunkSize = vmArena->extendBy + size;
  res = VMChunkCreate(&newChunk, vmArena, chunkSize);
  /* .improve.chunk-create.fail: If we fail we could try again */
  /* (with a smaller size, say).  We don't do this. */
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
  /* (see design.mps.arena.vm.table.disc) therefore the real pool */
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
  if (res != ResOK) {
    if (arena->spareCommitted > 0) {
      sparePagesPurge(vmArena);
      res = pagesMarkAllocated(vmArena, vmChunk, baseIndex, pages, pool);
      if (res != ResOK)
	goto failPagesMap;
      /* win! */
    } else {
      goto failPagesMap;
    }
  }

  base = PageIndexBase(chunk, baseIndex);
  baseTract = PageTract(&chunk->pageTable[baseIndex]);
  limit = AddrAdd(base, size);
  zones = ZoneSetOfRange(arena, base, limit);

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    vmArena->genZoneSet[gen] = ZoneSetUnion(vmArena->genZoneSet[gen], zones);
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
  Chunk chunk;
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
  /* @@@@ Chunks are never freed. */

  return;
}


/* VMArenaClass  -- The VM arena class definition */

DEFINE_ARENA_CLASS(VMArenaClass, this)
{
  INHERIT_CLASS(this, AbstractArenaClass);
  this->name = "VM";
  this->size = sizeof(VMArenaStruct);
  this->offset = offsetof(VMArenaStruct, arenaStruct);
  this->init = VMArenaInit;
  this->finish = VMArenaFinish;
  this->reserved = VMArenaReserved;
  this->spareCommitExceeded = VMArenaSpareCommitExceeded;
  this->alloc = VMAlloc;
  this->free = VMFree;
  this->chunkInit = VMChunkInit;
  this->chunkFinish = VMChunkFinish;
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
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
