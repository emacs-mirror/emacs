/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.27) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
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
 * .trans.space: Most of the functions in this code break
 * guide.impl.c.naming.method.prefix, because they take a Space as
 * a first parameter, rather than an Arena.  This is because of the
 * way that the Space and Arena are nested.  It will go away if and
 * when arena classes are implemented.  richard 1997-07-16
 *
 * .trans.zone-shift: The arena pokes around with space->zoneShift.
 * In fact, the arena implementation really owns this field.  The
 * relationship should become clearer when arena classes are
 * implemented.  richard 1997-07-16
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC. (but it would change how SegAllocWithRefSet avoids allocating
 * over the tables, see .alloc.skip)
 */


#include "mpm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.27) $");


/* Space Arena Projection
 * 
 * .space-arena: SpaceArena is applied to space in several places in
 * this module without first checking the validity of the space.  This
 * is "safe" in that SpaceArena is really just an addition, and the
 * subsequent ArenaCheck will fail if there is a missing signature on
 * the arena.  This nastiness will go away if arena classes are
 * implemented.  richard 1997-06-25
  *
 * .space-arena.private: Only the arena module needs to discuss the
 * arena object, hence, this method is private to this module.
 */

#define SpaceArena(space)       (&(space)->arenaStruct)


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

#define PageIndexBase(arena, i) \
  AddrAdd((arena)->base, ((i) << (arena)->pageShift))


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

#define PageIsHead(page)        (PageTail(page)->pool != NULL)


/* addrPageBase -- the base of the page this address is on */

#define addrPageBase(arena, addr)  AddrAlignDown((addr), (arena)->pageSize)

#define addrOfPageDesc(arena, index)  ((Addr)&(arena)->pageTable[index])


/* ArenaCreate -- create the arena
 *
 * .arena-create.space: In fact, this creates the space structure 
 * and initializes the arena part.  The space structure is created
 * by calling VMCreate.
 */

Res ArenaCreate(Space *spaceReturn, Size size, Addr base)
{
  Res res;
  Space space;
  Size allocTableSize;
  Size pageTableSize;
  Arena arena;
  
  AVER(spaceReturn != NULL);
  AVER(size > 0);
  /* no restrictions on base, it's simply passed through to VMCreate */

  /* VMCreate requires aligned size */
  size = SizeAlignUp(size, VMAlign());

  /* .vm.create: Create the space structure, initialize the VM part */
  res = VMCreate(&space, size, base);
  if(res != ResOK)
    goto failVMCreate;

  arena = SpaceArena(space);
  /* see design.mps.space.arena */
  arena->base = VMBase(space);
  arena->limit = VMLimit(space);
  AVER(AddrOffset(arena->base, arena->limit) == size);
  arena->pageSize = VMAlign();
  arena->pageShift = SizeLog2(arena->pageSize);
  arena->pages = size >> arena->pageShift;

  /* We generally assume that a page is aligned enough for any */
  /* normal object. */
  AVER(arena->pageSize >= MPS_PF_ALIGN);

  /* .init-tables: Allocate the page tables at the base of the arena. */
  /* There are two tables, the free table which is a bool array, and the */
  /* page table which is a PageStruct array.  Both tables are allocated */
  /* contiguously in one chunk, but only the free table is mapped now. */
  allocTableSize = SizeAlignUp(BTSize(arena->pages), arena->pageSize);
  pageTableSize = SizeAlignUp(arena->pages * sizeof(PageStruct),
                              arena->pageSize);
  arena->tablesSize = allocTableSize + pageTableSize;
  arena->allocTable = (BT)arena->base;
  /* .vm.Addr-is-star: In this file, Addr is compatible with C pointers. */
  arena->pageTable = (Page)AddrAdd(arena->base, allocTableSize);
  res = VMMap(space, arena->base, (Addr)arena->pageTable);
  if(res != ResOK)
    goto failTableMap;

  /* .tablepages: pages whose page index is < tablePages are recorded as */
  /* free but never allocated as alloc starts searching after the tables */
  /* (see .alloc.skip).  SegOfAddr uses the fact that these pages are */
  /* marked as free in order to detect "references" to these pages as */
  /* being bogus see .addr.free. */
  arena->tablePages = arena->tablesSize >> arena->pageShift;
  BTResRange(arena->allocTable, 0, arena->pages);

  /* Set the zone shift to divide the arena into the same number of */
  /* zones as will fit into a reference set (the number of bits in a */
  /* word).  Note that some zones are discontiguous in the arena if the */
  /* size is not a power of 2. See design.mps.space.arena. */
  space->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);

  /* Sign and check the arena. */
  arena->sig = ArenaSig;
  AVERT(Arena, arena);
  
  EVENT_PP(ArenaCreate, arena, space);

  *spaceReturn = space;
  return ResOK;

failTableMap:
  VMDestroy(space);
failVMCreate:
  return res;
}


/* ArenaDestroy -- finish the arena and destroy the space structure */

void ArenaDestroy(Space space)
{
  Arena arena;

  AVERT(Arena, SpaceArena(space));
  
  arena = SpaceArena(space);
  arena->sig = SigInvalid;
  VMUnmap(space, arena->base, (Addr)arena->pageTable);
  VMDestroy(space);     /* .vm.create */

  EVENT_P(ArenaDestroy, arena);
}


/* ArenaExtend -- extend the arena
 *
 * This function is here for completeness only.  It is specific
 * to the Client Arena, impl.c.arenacl.
 */

Res ArenaExtend(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}


/* ArenaRetract -- retract an extension of the arena
 *
 * This function is here for completeness only.  It is specific
 * to the Client Arena, impl.c.arenacl.
 */

Res ArenaRetract(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}


/* ArenaReserved -- return the amount of reserved address space
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

Size ArenaReserved(Space space)
{
  AVERT(Arena, SpaceArena(space));
  return VMReserved(space);
}


/* ArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

Size ArenaCommitted(Space space)
{
  AVERT(Arena, SpaceArena(space));
  return VMMapped(space);
}


/* ArenaCheck -- check of the consistency of the arena structure */

Bool ArenaCheck(Arena arena)
{
  CHECKS(Arena, arena);
  CHECKD(VM, &arena->vmStruct);
  CHECKL(arena->base != (Addr)0);
  CHECKL(arena->base < arena->limit);
  CHECKL(ShiftCheck(arena->pageShift));
  CHECKL(arena->pageSize == 1uL << arena->pageShift);
  CHECKL(VMAlign() == arena->pageSize);
  CHECKL(arena->pages == 
           AddrOffset(arena->base, arena->limit) >> arena->pageShift);
  CHECKL(arena->tablePages <= arena->pages);
  CHECKL(arena->tablesSize == arena->tablePages << arena->pageShift);
  CHECKL(arena->pageTable != NULL);
  CHECKL((Addr)arena->pageTable >= arena->base);
  CHECKL((Addr)&arena->pageTable[arena->pages] <=
           AddrAdd(arena->base, arena->tablesSize));
  CHECKL(arena->allocTable != NULL);
  CHECKL((Addr)arena->allocTable >= arena->base);
  CHECKL(AddrAdd((Addr)arena->allocTable, BTSize(arena->pages)) <=
         arena->limit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* refSet can't be checked because it's an arbitrary bit pattern */
  return TRUE;
}

static SegPrefStruct segPrefDefault = {
  SegPrefSig,                           /* sig */
  FALSE,                                /* high */
  RefSetUNIV,                           /* refSet */
};

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}

Res SegPrefExpress(SegPref pref, SegPrefKind kind, void *p)
{
  AVERT(SegPref, pref);
  AVER(pref != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    pref->high = TRUE;
    break;

  case SegPrefLow:
    AVER(p == NULL);
    pref->high = FALSE;
    break;

  case SegPrefRefSet:
    AVER(p != NULL);
    pref->refSet = *(RefSet *)p;
    break;

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }

  return ResOK;
}


/* indexOfAddr -- return the index of the page containing an address
 *
 * .index.addr: The address passed may be equal to the limit of the
 * arena, in which case the last page index plus one is returned.  (It
 * is, in a sense, the limit index of the page table.)
 */

static Index indexOfAddr(Arena arena, Addr addr)
{
  AVERT(Arena, arena);
  AVER(arena->base <= addr);
  AVER(addr <= arena->limit);   /* .index.addr */
  return AddrOffset(arena->base, addr) >> arena->pageShift;
}


/* findFreeInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 */

static Bool findFreeInArea(Index *baseReturn,
                           Space space, Size size,
                           Addr base, Addr limit)
{
  Arena arena;
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;	/* Index equiv. to base and limit */
  Index start, end;		/* base and limit of free run */

  AVER(baseReturn != NULL);
  AVERT(Space, space);  
  arena = SpaceArena(space);
  AVERT(Arena, arena);
  AVER(AddrIsAligned(base, arena->pageSize));
  AVER(AddrIsAligned(limit, arena->pageSize));
  AVER(arena->base <= base);
  AVER(base < limit);
  AVER(limit <= arena->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, arena->pageSize));

  basePage = indexOfAddr(arena, base);
  limitPage = indexOfAddr(arena, limit);
  pages = size >> arena->pageShift;

  if(!BTFindShortResRange(&start, &end,
                          arena->allocTable,
                          basePage, limitPage,
                          pages))
    return FALSE;

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
 */

static Bool findFreeInRefSet(Index *baseReturn,
			     Space space, Size size, RefSet refSet)
{
  Arena arena = SpaceArena(space);
  Addr arenaBase, base, limit;
  Size zoneSize = (Size)1 << space->zoneShift;

  AVER(baseReturn != NULL);
  AVERT(Arena, arena);
  AVER(size > 0);
  /* Can't check refSet */

  /* .alloc.skip: The first address available for segments, */
  /* is just after the arena tables. */
  arenaBase = PageIndexBase(arena, arena->tablePages);

  base = arenaBase;
  while(base < arena->limit) {
  
    if(RefSetIsMember(space, refSet, base)) {
      /* Search for a run of zone stripes which are in the RefSet and */
      /* the arena.  Adding the zoneSize might wrap round (to zero, */
      /* because limit is aligned to zoneSize, which is a power of two). */
      limit = base;
      do {
        limit = AddrAlignDown(AddrAdd(limit, zoneSize), zoneSize);

        AVER(limit > base || limit == (Addr)0);

        if(limit >= arena->limit || limit < base) {
          limit = arena->limit;
          break;
        }

        AVER(base < limit && limit < arena->limit);
      } while(RefSetIsMember(space, refSet, limit));

      /* If the RefSet was universal, then the area found ought to */
      /* be the whole arena. */
      AVER(refSet != RefSetUNIV ||
           (base == arenaBase && limit == arena->limit));

      /* Try to allocate a segment in the area. */
      if(AddrOffset(base, limit) >= size &&
         findFreeInArea(baseReturn, space, size, base, limit))
        return TRUE;
      
      base = limit;
    } else {
      /* Adding the zoneSize might wrap round (to zero, because base */
      /* is aligned to zoneSize, which is a power of two). */
      base = AddrAlignDown(AddrAdd(base, zoneSize), zoneSize);
      AVER(base > arenaBase || base == (Addr)0);
      if(base >= arena->limit || base < arenaBase) {
        base = arena->limit;
        break;
      }
    }
  }

  AVER(base == arena->limit);

  return FALSE;
}


/* tablePageBaseIndex -- index of the first page descriptor falling
 *                       (at least partially) on this table page
 *
 * .repr.table-page: Table pages are passed as the base address of the page.
 *
 * .division: We calculate it by dividing the offset from the beginning
 * of the page table by the size of a table element.  This relies on
 * .vm.Addr-is-star.
 */

#define tablePageBaseIndex(arena, tablePage) \
  (AddrOffset((Addr)(arena)->pageTable, (tablePage)) \
   / sizeof(PageStruct))


/* tablePageLimitIndex -- index of the first page descriptor falling
 *                        (wholly) the next table page
 *
 * Similar to tablePageBaseIndex, see .repr.table-page and .division.
 */

#define tablePageLimitIndex(arena, tablePage) \
  ((AddrOffset((Addr)(arena)->pageTable, (tablePage)) + (arena)->pageSize - 1) \
   / sizeof(PageStruct) \
   + 1)


/* tablePageInUse -- check whether a given page of the page table is in use
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if any
 * of the page descriptors falling on it (even partially) are being used.
 * Relies on .repr.table-page and .vm.Addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're (de)allocating.
 */

static Bool tablePageInUse(Arena arena, Addr tablePage)
{
  AVERT(Arena, arena);
  /* Check it's in the page table. */
  AVER((Addr)&arena->pageTable[0] <= tablePage);
  AVER(tablePage < (Addr)&arena->pageTable[arena->pages]);

  return !BTIsResRange(arena->allocTable,
		       tablePageBaseIndex(arena, tablePage),
		       tablePageLimitIndex(arena, tablePage));
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
			     Arena arena, Index baseIndex, Index limitIndex)
{
  Addr firstPageBase, lastPageBase, pagesBase, pagesLimit;

  AVERT(Arena, arena);
  AVER(baseIndex < limitIndex && limitIndex <= arena->pages);
  AVER(BTIsResRange(arena->allocTable, baseIndex, limitIndex));
  AVER(pagesBaseReturn != NULL);
  AVER(pagesLimitReturn != NULL);

  /* firstPageBase is the base address of the table page that contains the */
  /* (first byte of the) page descriptor for baseIndex. */
  firstPageBase = addrPageBase(arena, addrOfPageDesc(arena, baseIndex));

  /* lastPageBase is the base address of the table page that contains the */
  /* (last byte of the) page descriptor for the page before limitIndex. */
  lastPageBase = addrPageBase(arena,
			      AddrAdd(addrOfPageDesc(arena, limitIndex-1),
				      sizeof(PageStruct) - 1));

  /* If there is only one page involved, just check whether it is */
  /* used.  This is the common case, since it's unlikely that */
  /* many page descriptors will be allocated or freed at once. */
  if(firstPageBase == lastPageBase) {
    if(tablePageInUse(arena, firstPageBase)) {
      return FALSE;
    } else {
      *pagesBaseReturn = firstPageBase;
      *pagesLimitReturn = AddrAdd(firstPageBase, arena->pageSize);
      return TRUE;
    }
  }

  /* If the page containing the page descriptor for baseIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(arena, firstPageBase)) {
    pagesBase = AddrAdd(firstPageBase, arena->pageSize);
  } else {
    pagesBase = firstPageBase;
  }

  /* If the page containing the page descriptor for limitIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(arena, lastPageBase)) {
    pagesLimit = lastPageBase;
  } else {
    pagesLimit = AddrAdd(lastPageBase, arena->pageSize);
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


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegPref pref, Space space, Size size, Pool pool)
{
  Arena arena = SpaceArena(space);
  Index i, pages, base;
  Addr addr, unmappedPagesBase, unmappedPagesLimit;
  Seg seg;
  Res res;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVERT(Arena, SpaceArena(space));
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arena->pageSize));
  
  /* NULL is used as a discriminator */
  /* (see design.mps.arena.vm.table.disc) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  if(!findFreeInRefSet(&base, space, size, pref->refSet) &&
     (pref->refSet == RefSetUNIV ||
      !findFreeInRefSet(&base, space, size, RefSetUNIV))) {
    /* .improve.alloc-fail: This could be because the request was */
    /* too large, or perhaps the arena is fragmented.  We could return a */
    /* more meaningful code. */
    return ResRESOURCE;
  }
  
  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, so that we can exit straight away if */
  /* we fail. */
  addr = PageIndexBase(arena, base);
  res = VMMap(space, addr, AddrAdd(addr, size));
  if(res) goto failSegMap;

  /* Compute number of pages to be allocated. */
  pages = size >> arena->pageShift;

  /* Ensure that the page descriptors we need are on mapped pages. */
  if(unusedTablePages(&unmappedPagesBase, &unmappedPagesLimit,
		      arena, base, base + pages)) {
    res = VMMap(space, unmappedPagesBase, unmappedPagesLimit);
    if(res) goto failTableMap;
  }

  /* Initialize the generic segment structure. */
  seg = PageSeg(&arena->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(!BTGet(arena->allocTable, base));
  BTSet(arena->allocTable, base);
  if(pages > 1) {
    Addr limit = PageIndexBase(arena, base + pages);
    SegSetSingle(seg, FALSE);
    for(i = base + 1; i < base + pages; ++i) {
      AVER(!BTGet(arena->allocTable, i));
      BTSet(arena->allocTable, i);
      PageTail(&arena->pageTable[i])->pool = NULL;
      PageTail(&arena->pageTable[i])->seg = seg;
      PageTail(&arena->pageTable[i])->limit = limit;
    }
  } else
    SegSetSingle(seg, TRUE);
  
  AVERT(Seg, seg);
  
  EVENT_PPAWP(SegAlloc, arena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;

failTableMap:
  VMUnmap(space, addr, AddrAdd(addr, size));
failSegMap:
  return res;
}


/* SegFree - free a segment in the arena
 *
 * .seg-free.alt: It is possible to re-implement this function without
 * making a call to SegLimit, by searching for the end of the segment
 * in .free.loop.
 */

void SegFree(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Count pages;
  Index basePage;
  Addr base, limit, unusedPagesBase, unusedPagesLimit;

  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  page = PageOfSeg(seg);
  limit = SegLimit(space, seg);
  basePage = page - arena->pageTable;
  AVER(basePage <= arena->pages);

  SegFinish(seg);

  base = PageIndexBase(arena, basePage);
  VMUnmap(space, base, limit);

  /* Calculate the number of pages in the segment */
  pages = AddrOffset(base, limit) >> arena->pageShift;

  /* There shouldn't be any pages marked free within the segment's */
  /* area of the alloc table. */
  AVER(BTIsSetRange(arena->allocTable, basePage, basePage + pages));
  BTResRange(arena->allocTable, basePage, basePage + pages);

  /* Unmap any pages that became unused in the page table */
  if(unusedTablePages(&unusedPagesBase, &unusedPagesLimit,
		      arena, basePage, basePage + pages))
    VMUnmap(space, unusedPagesBase, unusedPagesLimit);

  EVENT_PP(SegFree, arena, seg);
}


/* ArenaAlign -- return the alignment of segments */

Align ArenaAlign(Space space)
{
  Arena arena;
  AVERT(Arena, SpaceArena(space));
  arena = SpaceArena(space);
  return arena->pageSize;
}


/* SegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then returning the
 * base address of that page.
 */

Addr SegBase(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Index i;
  
  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  page = PageOfSeg(seg);
  i = page - arena->pageTable;

  return PageIndexBase(arena, i);
}


/* SegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

Addr SegLimit(Space space, Seg seg)
{
  Arena arena;
  Page page;

  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  if(SegSingle(seg))
    return AddrAdd(SegBase(space, seg), arena->pageSize);
  else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* SegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls do roughly the same thing twice.
 */

Size SegSize(Space space, Seg seg)
{
  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);
  return AddrOffset(SegBase(space, seg), SegLimit(space, seg));
}


/* SegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

Bool SegOfAddr(Seg *segReturn, Space space, Addr addr)
{
  Arena arena;
  
  AVER(segReturn != NULL);
  AVERT(Arena, SpaceArena(space));
  
  arena = SpaceArena(space);
  if(arena->base <= addr && addr < arena->limit) {
    Index i = indexOfAddr(arena, addr);
    /* .addr.free: If the page is recorded as being free then */
    /* either the page is free or it is */
    /* part of the arena tables (see .tablepages) */
    if(BTGet(arena->allocTable, i)) {
      Page page = &arena->pageTable[i];

      if(PageIsHead(page))
        *segReturn = PageSeg(page);
      else
        *segReturn = PageTail(page)->seg;
      return TRUE;
    }
  }
  
  return FALSE;
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

static Bool segSearch(Seg *segReturn, Arena arena, Index i)
{
  AVER(segReturn != NULL);
  AVERT(Arena, arena);
  AVER(arena->tablePages <= i);
  AVER(i <= arena->pages);

  while(i < arena->pages &&
        !(BTGet(arena->allocTable, i) &&
          PageIsHead(&arena->pageTable[i])))
    ++i;

  if(i == arena->pages)
    return FALSE;
  
  AVER(i < arena->pages);
  
  *segReturn = PageSeg(&arena->pageTable[i]);
  return TRUE;
}


/* SegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

Bool SegFirst(Seg *segReturn, Space space)
{
  Arena arena;

  AVER(segReturn != NULL);
  AVERT(Space, space);
  arena = SpaceArena(space);
  AVERT(Arena, arena);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return segSearch(segReturn, arena, (Index)arena->tablePages);
}


/* SegNext -- return the "next" segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 *
 * SegNext find the segment with the lowest base address which is
 * greater than a specified address.  The address must be (or once
 * have been) the base address of a segment.
 */

Bool SegNext(Seg *segReturn, Space space, Addr addr)
{
  Arena arena;
  Index i;

  AVER(segReturn != NULL);
  AVERT(Space, space);
  AVER(AddrIsAligned(addr, ArenaAlign(space)));
  arena = SpaceArena(space);
  AVERT(Arena, arena);

  i = indexOfAddr(arena, addr);

  /* There are fewer pages than addresses, therefore the */
  /* page index can never wrap around */
  AVER(i+1 != 0);

  return segSearch(segReturn, arena, i + 1);
}
