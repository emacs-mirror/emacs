/* impl.c.arenavm: VIRTUAL MEMORY ARENA CLASS
 *
 * $HopeName: MMsrc!arenavm.c(trunk.65) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
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
 * GC.  (But it would change how VMFindFreeInRefSet avoids allocating
 * over the tables, see .alloc.skip.)
 */


#include "boot.h"
#include "tract.h"
#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.65) $");


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
  Addr overheadLimit;           /* limit of pages mapped for overhead */
  BT pageTableMapped;           /* indicates mapped state of page table */
  BT noLatentPages;             /* 1 bit per page of pageTable */
  Sig sig;                      /* design.mps.sig */
} VMChunkStruct;

#define VMChunkChunk(vmchunk) (&(vmchunk)->chunkStruct)
#define ChunkVMChunk(chunk) PARENT(VMChunkStruct, chunkStruct, chunk)


/* VMChunkVMArena -- get the VM arena from a VM chunk */

#define VMChunkVMArena(vmchunk) \
  ArenaVMArena(ChunkArena(VMChunkChunk(vmchunk)))


/* VMArena
 *
 * See design.mps.arena.coop-vm.struct.vmarena for description.
 */

typedef struct VMArenaStruct *VMArena;

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VM vm;                        /* VM where the arena itself is stored */
  Size latentSize;              /* total size of latent pages */
  RefSet blacklist;             /* zones to use last */
  RefSet genRefSet[VMArenaGenCount]; /* .gencount.const */
  RefSet freeSet;               /* unassigned zones */
  Size extendBy;
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;


/* ArenaVMArena -- find the VMArena pointer given a generic Arena */

#define ArenaVMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)


/* VMArenaArena -- find the generic Arena pointer given a VMArena */

#define VMArenaArena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static void VMArenaPurgeLatentPages(VMArena vmArena);


/* VMChunkCheck -- check the consistency of a VM chunk */

static Bool VMChunkCheck(VMChunk vmchunk)
{
  Chunk chunk;

  CHECKS(VMChunk, vmchunk);
  chunk = VMChunkChunk(vmchunk);
  CHECKL(ChunkCheck(chunk));
  CHECKL(VMCheck(vmchunk->vm));
  CHECKL(VMAlign(vmchunk->vm) == ChunkPageSize(chunk));
  CHECKL(vmchunk->overheadLimit <= (Addr)chunk->pageTable);
  /* check pageTableMapped table */
  CHECKL(vmchunk->pageTableMapped != NULL);
  CHECKL((Addr)vmchunk->pageTableMapped >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->pageTableMapped, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadLimit);
  /* check noLatentPages table */
  CHECKL(vmchunk->noLatentPages != NULL);
  CHECKL((Addr)vmchunk->noLatentPages >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->noLatentPages, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadLimit);
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


/* PageIsLatent -- is page latent (free and mapped)? */

#define PageIsLatent(page) \
  ((page)->the.rest.pool == NULL && (page)->the.rest.type == PageTypeLatent)


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  Index gen;
  RefSet allocSet;
  Arena arena;
  VMChunk primary;

  CHECKS(VMArena, vmArena);
  arena = VMArenaArena(vmArena);
  CHECKD(Arena, arena);
  /* latent pages are committed, so must be less latent than committed. */
  CHECKL(vmArena->latentSize <= arena->committed);
  CHECKL(RefSetCheck(vmArena->blacklist));

  allocSet = RefSetEMPTY;
  for(gen = (Index)0; gen < VMArenaGenCount; ++gen) {
    CHECKL(RefSetCheck(vmArena->genRefSet[gen]));
    allocSet = RefSetUnion(allocSet, vmArena->genRefSet[gen]);
  }
  CHECKL(RefSetCheck(vmArena->freeSet));
  CHECKL(RefSetInter(allocSet, vmArena->freeSet) == RefSetEMPTY);
  CHECKL(vmArena->extendBy > 0);

  if (arena->primary != NULL) {
    primary = ChunkVMChunk(arena->primary);
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

  arena = VMArenaArena(vmArena);
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

  arena = VMArenaArena(vmArena);
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
  vmChunk->overheadLimit = chunkStructLimit;

  vmChunk->vm = vm;
  res = ChunkInit(VMChunkChunk(vmChunk), VMArenaArena(vmArena),
                  base, limit, pageSize, boot);
  if (res != ResOK)
    goto failChunkInit;

  BootBlockFinish(boot);

  vmChunk->sig = VMChunkSig;
  AVERT(VMChunk, vmChunk);
  *chunkReturn = VMChunkChunk(vmChunk);
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
  Addr ullageLimit;
  void *p;
  Res res;

  /* chunk is supposed to be uninitialized, so don't check it. */
  vmChunk = ChunkVMChunk(chunk);
  AVERT(BootBlock, boot);

  btSize = (size_t)BTSize(chunk->pageTablePages);
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failPageTableMapped;
  vmChunk->pageTableMapped = p;
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failNoLatentPages;
  vmChunk->noLatentPages = p;

  /* Actually commit all the tables. design.mps.arena.@@@@ */
  ullageLimit = AddrAdd(chunk->base, (Size)BootAllocated(boot));
  if (vmChunk->overheadLimit < ullageLimit) {
    ullageLimit = AddrAlignUp(ullageLimit, ChunkPageSize(chunk));
    res = vmArenaMap(VMChunkVMArena(vmChunk), vmChunk->vm,
                     vmChunk->overheadLimit, ullageLimit);
    if (res != ResOK)
      goto failTableMap;
    vmChunk->overheadLimit = ullageLimit;
  }

  BTResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages);
  BTSetRange(vmChunk->noLatentPages, 0, chunk->pageTablePages);
  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failNoLatentPages:
failPageTableMapped:
  return res;
}


/* vmChunkDestroy -- destroy a VMChunk */

static void vmChunkDestroy(Chunk chunk)
{
  VM vm;
  VMChunk vmChunk;

  AVERT(Chunk, chunk);
  vmChunk = ChunkVMChunk(chunk);
  AVERT(VMChunk, vmChunk);
  AVER(BTIsSetRange(vmChunk->noLatentPages, 0, chunk->pageTablePages));
  AVER(BTIsResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages));

  vmChunk->sig = SigInvalid;
  vm = vmChunk->vm;
  ChunkFinish(chunk);
  VMDestroy(vm);
}


/* VMChunkFinish -- finish a VMChunk */

static void VMChunkFinish(Chunk chunk)
{
  VMChunk vmChunk = ChunkVMChunk(chunk);

  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               VMBase(vmChunk->vm), vmChunk->overheadLimit);
  /* No point in finishing the other fields, since they are unmapped. */
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
  Size vmArenaSize; /* aligned size of VMArenaStruct */
  Res res;
  VMArena vmArena;
  Arena arena;
  Lock lock;
  Index gen;
  VM arenaVM;
  Chunk chunk;

  userSize = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER((ArenaClass)mps_arena_class_vm() == class ||
       (ArenaClass)mps_arena_class_vmnz() == class);
  AVER(userSize > 0);

  /* Create a VM to hold the arena and the lock, and map it. */
  vmArenaSize = SizeAlignUp(sizeof(VMArenaStruct), MPS_PF_ALIGN); 
  res = VMCreate(&arenaVM, vmArenaSize + LockSize());
  if (res != ResOK)
    goto failVMCreate;
  res = VMMap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  if (res != ResOK)
    goto failVMMap;
  vmArena = (VMArena)VMBase(arenaVM);
  lock = (Lock)PointerAdd((void *)vmArena, vmArenaSize);

  arena = VMArenaArena(vmArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, lock, class);
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->latentSize = 0;

  /* .blacklist: We blacklist the first and last zones because */
  /* they commonly correspond to low integers. */
  vmArena->blacklist = 
    RefSetAdd(arena, RefSetAdd(arena, RefSetEMPTY, (Addr)1), (Addr)-1);

  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genRefSet[gen] = RefSetEMPTY;
  }
  vmArena->freeSet = RefSetUNIV; /* includes blacklist */
  /* design.mps.arena.coop-vm.struct.vmarena.extendby.init */
  vmArena->extendBy = userSize;

  /* have to have a valid arena before calling ChunkCreate */
  vmArena->sig = VMArenaSig;

  if ((ArenaClass)mps_arena_class_vm() == class)
    EVENT_PWW(ArenaCreateVM, arena, userSize, chunkSize);
  else
    EVENT_PWW(ArenaCreateVMNZ, arena, userSize, chunkSize);

  res = VMChunkCreate(&chunk, vmArena, userSize);
  if (res != ResOK)
    goto failChunkCreate;
  arena->primary = chunk;

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of stripes as will fit into a reference set (the number of */
  /* bits in a word).  Fail if the chunk is so small stripes are smaller */
  /* than pages.  Note that some zones are discontiguous in the */
  /* chunk if the size is not a power of 2.  See */
  /* design.mps.arena.class.fields. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);

  AVERT(VMArena, vmArena);
  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
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

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  arenaVM = vmArena->vm;

  VMArenaPurgeLatentPages(vmArena);
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
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaReserved(Arena arena)
{
  Size reserved;
  Ring node, next;

  reserved = 0;
  RING_FOR(node, &arena->chunkRing, next) {
    VMChunk vmChunk = ChunkVMChunk(RING_ELT(Chunk, chunkRing, node));
    reserved += VMReserved(vmChunk->vm);
  }
  return reserved;
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

  /* static used only internally, so minimal checking */
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
  /* static used only internally, so minimal checking */
  *tableBaseReturn =
    PageTablePageIndex(chunk,
                       AddrPageBase(chunk, addrOfPageDesc(chunk, pageBase)));
  *tableLimitReturn = 
    PageTablePageIndex(chunk,
                       AddrAlignUp(addrOfPageDesc(chunk, pageLimit),
		                   ChunkPageSize(chunk)));
  return;
}


/* VMArenaEnsurePageTableMapped -- ensure needed part of page table is mapped
 *
 * Pages from baseIndex to limitIndex are about to be allocated.
 * Ensure that the relevant pages occupied by the page table are mapped. 
 */

static Res VMArenaEnsurePageTableMapped(VMChunk vmChunk,
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

  /* static used only internally, so minimal checking */
  chunk = VMChunkChunk(vmChunk);

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


/* unmapUnusedTablePages
 *
 * Of the pages occupied by the page table from tablePageBase to
 * tablePageLimit find those which are wholly unused and unmap them.
 */

static void unmapUnusedTablePages(VMChunk vmChunk,
                                  Addr tablePageBase, Addr tablePageLimit)
{
  Chunk chunk;
  Addr cursor;
  Size pageSize;

  /* static used only internally, so minimal checking */
  chunk = VMChunkChunk(vmChunk);
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
      AVER(BTGet(vmChunk->noLatentPages, PageTablePageIndex(chunk, cursor)));
      AVER(BTGet(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor)));
      BTRes(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor));
    }
  }
  AVER(cursor == tablePageLimit);

  return;
}
      

/* findFreeInArea -- try to allocate pages in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 *
 * .findfreeinarea.arg.downwards: downwards basically governs whether
 * we use BTFindShortResRange (if downwards is FALSE) or
 * BTFindShortResRangeHigh (if downwards is TRUE).
 * .findfreeinarea.arg.downwards.justify: This _roughly_
 * corresponds to allocating pages from top down (when downwards is
 * TRUE), at least within an interval.  It is used for implementing
 * SegPrefHigh.
 */

static Bool findFreeInArea(Index *baseReturn, Chunk chunk, Size size,
                           Addr base, Addr limit, Bool downwards)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;    /* Index equiv. to base and limit */
  Index start, end;             /* base and limit of free run */

  /* static used only internally, so minimal checking */
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


/* VMFindFreeInRefSet -- try to allocate a range of pages with a RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a page whose RefSet (see
 * RefSetOfPage) will be a subset of the specified RefSet.
 *
 * For meaning of downwards arg see findFreeInArea.
 * .improve.findfreeinrefset.downwards: This
 * should be improved so that it allocates pages from top down
 * globally, as opposed to (currently) just within an interval.
 */

static Bool VMFindFreeInRefSet(Index *baseReturn, VMChunk *chunkReturn,
                               VMArena vmArena, Size size, RefSet refSet,
                               Bool downwards)
{
  Arena arena;
  Addr chunkBase, base, limit;
  Size zoneSize;
  Ring node, next;

  /* static used only internally, so minimal checking */
  AVER(RefSetCheck(refSet));

  arena = VMArenaArena(vmArena);
  zoneSize = (Size)1 << arena->zoneShift;

  /* Should we check chunk cache first? */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    AVERT(Chunk, chunk);

    /* .alloc.skip: The first address available for arena allocation, */
    /* is just after the arena tables. */
    chunkBase = PageIndexBase(chunk, chunk->ullagePages);

    base = chunkBase;
    while(base < chunk->limit) {
      if (RefSetIsMember(arena, refSet, base)) {
        /* Search for a run of zone stripes which are in the RefSet */
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
        } while(RefSetIsMember(arena, refSet, limit));

        /* If the RefSet was universal, then the area found ought to */
        /* be the whole chunk. */
        AVER(refSet != RefSetUNIV
             || (base == chunkBase && limit == chunk->limit));

        /* Try to allocate a page in the area. */
        if (AddrOffset(base, limit) >= size
            && findFreeInArea(baseReturn, chunk, size, base, limit, downwards)) {
          *chunkReturn = ChunkVMChunk(chunk);
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


static Serial vmGenOfSegPref(VMArena vmArena, SegPref pref)
{
  Serial gen;

  AVERT(VMArena, vmArena);
  AVERT(SegPref, pref);
  AVER(pref->isGen);

  gen = pref->gen;
  if (gen >= VMArenaGenCount) {
    gen = VMArenaGenCount - 1;
  }
  return gen;
}


/* VMRegionFind
 *
 * Finds space for the pages (note it does not create or allocate any
 * pages).
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
 * zones already used by this segpref or free zones will be used).
 */

static Bool VMRegionFind(Index *baseReturn, VMChunk *chunkReturn,
                         VMArena vmArena, SegPref pref, Size size,
                         Bool barge)
{
  RefSet refSet;

  /* This function is local to VMAlloc, so */
  /* no checking required */

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    refSet = vmArena->genRefSet[gen];
  } else {
    refSet = pref->refSet;
  }

  /* @@@@ Some of these tests might be duplicates.  If we're about */
  /* to run out of virtual address space, then slow allocation is */
  /* probably the least of our worries. */

  /* .alloc.improve.map: Define a function that takes a list */
  /* (say 4 long) of RefSets and tries VMFindFreeInRefSet on */
  /* each one in turn.  Extra RefSet args that weren't needed */
  /* could be RefSetEMPTY */

  if (pref->isCollected) { /* GC'd memory */
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
    if (VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                           RefSetDiff(refSet, vmArena->blacklist),
                           pref->high)
        || VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                              RefSetUnion(refSet,
                                          RefSetDiff(vmArena->freeSet, 
                                                     vmArena->blacklist)),
                              pref->high)) {
      return TRUE; /* found */
    }
    if (!barge)
      /* do not barge into other zones, give up now */
      return FALSE;
    if (VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                           RefSetDiff(RefSetUNIV, vmArena->blacklist),
                           pref->high)
        || VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                              RefSetUNIV, pref->high)) {
      return TRUE; /* found */
    }
  } else { /* non-GC'd memory */
    /* We look for space in the following places (in order) */
    /*   - Zones preferred (refSet) and blacklisted; */
    /*   - Zones preferred; */
    /*   - Zones preferred or blacklisted zone; */
    /*   - Any zone. */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated. */
    if (VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                           RefSetInter(refSet, vmArena->blacklist),
                           pref->high)
        || VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                              refSet, pref->high)
        || VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size, 
                              RefSetUnion(refSet, vmArena->blacklist),
                              pref->high)
        || VMFindFreeInRefSet(baseReturn, chunkReturn, vmArena, size,
                              RefSetUNIV, pref->high)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* VMExtend -- Extend the arena by making a new chunk
 *
 * The size arg specifies how much we wish to allocate after the extension.
 */

static Res VMExtend(VMArena vmArena, Size size)
{
  Chunk newChunk;
  Size chunkSize;
  Res res;

  /* Internal static function, so no checking. */

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


/* Allocation policy methods */


/* Used in abstracting allocation policy between VM and VMNZ */
typedef Res (*VMAllocPolicyMethod)(Index *, VMChunk *,
                                   VMArena, SegPref, Size);

static Res VMZoneAllocPolicy(Index *baseIndexReturn, VMChunk *chunkReturn,
                             VMArena vmArena, SegPref pref, Size size)
{
  /* internal and static, no checking */

  if (!VMRegionFind(baseIndexReturn, chunkReturn,
                    vmArena, pref, size, FALSE)) {
    /* try and extend, but don't worry if we can't */
    (void)VMExtend(vmArena, size);

    /* We may or may not have a new chunk at this point */
    /* we proceed to try the allocation again anyway. */
    /* We specify barging, but if we have got a new chunk */
    /* then hopefully we won't need to barge. */
    if (!VMRegionFind(baseIndexReturn, chunkReturn,
                      vmArena, pref, size, TRUE)) {
      /* .improve.alloc-fail: This could be because the request was */
      /* too large, or perhaps the arena is fragmented.  We could */
      /* return a more meaningful code. */
      return ResRESOURCE;
    }
  }
  return ResOK;
}

static Res VMNZAllocPolicy(Index *baseIndexReturn,
                           VMChunk *chunkReturn,
                           VMArena vmArena,
                           SegPref pref,
                           Size size)
{
  /* internal and static, no checking */

  if (VMFindFreeInRefSet(baseIndexReturn, chunkReturn, vmArena, size, 
                         RefSetUNIV, pref->high)) {
    return ResOK;
  }
  return ResRESOURCE;
}


/* VMArenaPageIsMapped -- checks whether a free page is mapped or not. */

static Bool VMArenaPageIsMapped(VMChunk vmChunk, Index pi)
{
  Index pageTableBaseIndex;
  Index pageTableLimitIndex;
  int pageType;
  Chunk chunk = VMChunkChunk(vmChunk);

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
    if (pageType == PageTypeLatent)
      return TRUE;
    AVER(pageType == PageTypeFree);
  }
  return FALSE;
}


/* VMArenaHysteresisRemovePage
 *
 * Removes the page descriptor from the hysteresis fund.
 * Temporarily leaves it in an inconsistent state.
 */

static void VMArenaHysteresisRemovePage(VMChunk vmChunk, Index pi)
{
  Chunk chunk = VMChunkChunk(vmChunk);
  Arena arena = ChunkArena(chunk);

  /* minimal checking as it's a static used only locally */
  AVER(PageTypeLatent == PageType(&chunk->pageTable[pi]));
  AVER(arena->spareCommitted >= ChunkPageSize(chunk));
  arena->spareCommitted -= ChunkPageSize(chunk);
  return;
}


static Res VMArenaPagesMap(VMArena vmArena, VMChunk vmChunk,
                           Index baseIndex, Count pages, Pool pool)
{
  Index i;
  Index limitIndex;
  Index mappedBase, mappedLimit;
  Index unmappedBase, unmappedLimit;
  Chunk chunk = VMChunkChunk(vmChunk);
  Res res;

  /* Ensure that the page descriptors we need are on mapped pages. */
  limitIndex = baseIndex + pages;
  res = VMArenaEnsurePageTableMapped(vmChunk, baseIndex, limitIndex);
  if (res != ResOK)
    goto failTableMap;

  mappedBase = baseIndex;
  mappedLimit = mappedBase;

  do {
    while(VMArenaPageIsMapped(vmChunk, mappedLimit)) {
      ++mappedLimit;
      if (mappedLimit >= limitIndex)
	break;
    }
    AVER(mappedLimit <= limitIndex);
    /* NB for loop will loop 0 times iff first page is not mapped */
    for(i = mappedBase; i < mappedLimit; ++i) {
      VMArenaHysteresisRemovePage(vmChunk, i);
      PageAlloc(chunk, i, pool);
    }
    if (mappedLimit >= limitIndex)
      break;
    unmappedBase = mappedLimit;
    unmappedLimit = unmappedBase;
    while(!VMArenaPageIsMapped(vmChunk, unmappedLimit)) {
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
    /* Resetting the noLatentPages bits is lazy, it means that */
    /* we don't have to bother trying to unmap unused portions */
    /* of the pageTable. */
    BTResRange(vmChunk->noLatentPages, pageTableBaseIndex, pageTableLimitIndex);
  }
failTableMap:
  return res;
}


/* VMAllocComm -- allocate a region from the arena
 *
 * Common code used by mps_arena_class_vm and
 * mps_arena_class_vmnz. 
 */

static Res VMAllocComm(Addr *baseReturn, Tract *baseTractReturn,
                       VMAllocPolicyMethod policy,
                       SegPref pref, Size size, Pool pool)
{
  Addr base, limit;
  Tract baseTract;
  Arena arena;
  Count pages;
  Index baseIndex;
  RefSet refSet;
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
  vmArena = ArenaVMArena(arena);
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
  chunk = VMChunkChunk(vmChunk);

  /* Compute number of pages to be allocated. */
  pages = ChunkSizeToPages(chunk, size);

  res = VMArenaPagesMap(vmArena, vmChunk, baseIndex, pages, pool);
  if (res != ResOK) {
    if (arena->spareCommitted > 0) {
      VMArenaPurgeLatentPages(vmArena);
      res = VMArenaPagesMap(vmArena, vmChunk, baseIndex, pages, pool);
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
  refSet = RefSetOfRange(arena, base, limit);

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    vmArena->genRefSet[gen] = 
      RefSetUnion(vmArena->genRefSet[gen], refSet);
  }

  vmArena->freeSet = RefSetDiff(vmArena->freeSet, refSet);
  
  *baseReturn = base;
  *baseTractReturn = baseTract;
  return ResOK;

failPagesMap:
  return res;
}


static Res VMAlloc(Addr *baseReturn, Tract *baseTractReturn,
                   SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common VMAllocComm */
  return VMAllocComm(baseReturn, baseTractReturn,
                     VMZoneAllocPolicy, pref, size, pool);
}

static Res VMNZAlloc(Addr *baseReturn, Tract *baseTractReturn,
                     SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common VMAllocComm */
  return VMAllocComm(baseReturn, baseTractReturn,
                     VMNZAllocPolicy, pref, size, pool);
}


/* VMArenaFindLatentRanges -- map a function over latent ranges
 *
 * The function f is called on the ranges of latent pages which are
 * within the range of pages from base to limit.  PageStruct descriptors
 * from base to limit should be mapped in the page table before calling
 * this function. 
 */

typedef void (*latentRangesFn)(VMChunk, Index, Index, void *);

static void VMArenaFindLatentRanges(VMChunk vmChunk, Index base, Index limit,
                                    latentRangesFn f, void *p)
{
  Index latentBase, latentLimit;
  Chunk chunk = VMChunkChunk(vmChunk);

  AVER(base < limit);

  latentBase = base;
  do {
    while(!PageIsLatent(&chunk->pageTable[latentBase])) {
      ++latentBase;
      if (latentBase >= limit)
	goto done;
    }
    latentLimit = latentBase;
    while(PageIsLatent(&chunk->pageTable[latentLimit])) {
      ++latentLimit;
      if (latentLimit >= limit)
	break;
    }
    f(vmChunk, latentBase, latentLimit, p);
    latentBase = latentLimit;
  } while(latentBase < limit);
done:
  AVER(latentBase == limit);

  return;
}


/* vmArenaUnmapLatentRange
 *
 * Takes a range of pages which are latent pages (in the hysteresis fund),
 * unmaps them and removes them from the hysteresis fund.
 */

static void vmArenaUnmapLatentRange(VMChunk vmChunk,
                                    Index rangeBase, Index rangeLimit,
				    void *p)
{
  Index i;
  Chunk chunk = VMChunkChunk(vmChunk);

  UNUSED(p);
  for(i = rangeBase; i < rangeLimit; ++i) {
    VMArenaHysteresisRemovePage(vmChunk, i);
    PageInit(chunk, i);
  }
  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               PageIndexBase(chunk, rangeBase),
	       PageIndexBase(chunk, rangeLimit));
  
  return;
}
  

/* PurgeLatentPages
 *
 * All latent pages are found and removed from the hysteresis fund
 * (i.e., they are unmapped).  Pages occupied by the page table are
 * potentially unmapped.  This is currently the only way the hysteresis
 * fund is prevented from growing.
 *
 * It uses the noLatentPages bits to determine which areas of the
 * pageTable to examine.
 */

static void VMArenaPurgeLatentPages(VMArena vmArena)
{
  Ring node, next;
  Arena arena = VMArenaArena(vmArena);

  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    VMChunk vmChunk = ChunkVMChunk(chunk);
    Index latentBaseIndex, latentLimitIndex;
    Index tablePageCursor = 0;

    while(BTFindLongResRange(&latentBaseIndex, &latentLimitIndex,
                             vmChunk->noLatentPages,
	           	     tablePageCursor, chunk->pageTablePages,
	      	             1)) {
      Addr latentTableBase, latentTableLimit;
      Index pageBase, pageLimit;
      Index tablePage;

      latentTableBase = TablePageIndexBase(chunk, latentBaseIndex);
      latentTableLimit = TablePageIndexBase(chunk, latentLimitIndex);
      /* Determine whether to use initial overlapping PageStruct. */
      if (latentBaseIndex > 0
          && !BTGet(vmChunk->pageTableMapped, latentBaseIndex - 1)) {
	pageBase = tablePageWholeBaseIndex(chunk, latentTableBase);
      } else {
        pageBase = tablePageBaseIndex(chunk, latentTableBase);
      }
      for(tablePage = latentBaseIndex;
          tablePage < latentLimitIndex;
	  ++tablePage) {
	/* Determine whether to use final overlapping PageStruct. */
        if (tablePage == latentLimitIndex - 1
            && latentLimitIndex < chunk->pageTablePages
            && !BTGet(vmChunk->pageTableMapped, latentLimitIndex)) {
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
	  VMArenaFindLatentRanges(vmChunk, pageBase, pageLimit,
				  vmArenaUnmapLatentRange, NULL);
	} else {
	  /* Only happens for last page occupied by the page table */
	  /* and only then when that last page has just the tail end */
	  /* part of the last page descriptor and nothing more. */
	  AVER(pageBase == pageLimit);
	  AVER(tablePage == chunk->pageTablePages - 1);
	}
	BTSet(vmChunk->noLatentPages, tablePage);
	pageBase = pageLimit;
      }
      unmapUnusedTablePages(vmChunk, latentTableBase, latentTableLimit);
      tablePageCursor = latentLimitIndex;
      if (tablePageCursor >= chunk->pageTablePages) {
        AVER(tablePageCursor == chunk->pageTablePages);
	break;
      }
    }

  }

  AVER(VMArenaArena(vmArena)->spareCommitted == 0);
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
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
  AVER(AddrIsAligned(base, ChunkPageSize(arena->primary)));

  foundChunk = ChunkOfAddr(&chunk, arena, base);
  AVER(foundChunk);
  vmChunk = ChunkVMChunk(chunk);

  /* Calculate the number of pages in the region */
  pages = ChunkSizeToPages(chunk, size);
  piBase = INDEX_OF_ADDR(chunk, base);
  piLimit = piBase + pages;
  AVER(piBase < piLimit);
  AVER(piLimit <= chunk->pages);

  /* loop from pageBase to pageLimit-1 inclusive */
  /* Finish each Tract found, then convert them to latent pages and  */
  /* add them to the hysteresis fund */
  for(pi = piBase; pi < piLimit; ++pi) {
    Page page = &chunk->pageTable[pi];
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);

    TractFinish(PageTract(page));
    PagePool(page) = NULL;
    PageType(page) = PageTypeLatent;
  }
  arena->spareCommitted += ChunkPagesToSize(chunk, piLimit - piBase);
  BTResRange(chunk->allocTable, piBase, piLimit);

  tablePagesUsed(&pageTableBase, &pageTableLimit, chunk, piBase, piLimit);
  BTResRange(vmChunk->noLatentPages, pageTableBase, pageTableLimit);

  if (arena->spareCommitted > arena->spareCommitLimit) {
    VMArenaPurgeLatentPages(vmArena);
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
  return (mps_arena_class_t)EnsureVMArenaClass();
}


/* mps_arena_class_vmnz -- return the arena class VMNZ */

mps_arena_class_t mps_arena_class_vmnz(void)
{
  return (mps_arena_class_t)EnsureVMNZArenaClass();
}
