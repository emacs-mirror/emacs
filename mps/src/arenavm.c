/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.19) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 * design.mps.arena.vm
 */


#include "mpm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.19) $");


/* Space Arena Projection
 * 
 * Only the arena module needs to discuss the arena object, hence, this
 * method is private to this module.
 *
 * .space-arena: SpaceArena is applied to space in several places in
 * this module without first checking the validity of the space.  This
 * is "safe" in that SpaceArena is really just an addition, and the
 * subsequent ArenaCheck will fail if there is a missing signature on
 * the arena.  This nastiness will go away if arena classes are
 * implemented.  richard 1997-06-25
 */

#define SpaceArena(space)       (&(space)->arenaStruct)


/* PageStruct -- page structure
 *
 * The page table (defined as a PageStruct array) is central to the
 * design of the arena.  See design.mps.arena.vm.table.*.
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


/* Page Index to Base address mapping
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageBase(arena, i) \
  AddrAdd((arena)->base, ((i) << arena->pageShift))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)


/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)


/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, seg)



/* ArenaCreate -- create the arena
 *
 * In fact, this creates the space structure and initializes the
 * arena part.
 *
 * In fact, the space structure is created by calling VMCreate.
 */

Res ArenaCreate(Space *spaceReturn, Size size, Addr base)
{
  Res res;
  Space space;
  Size f_words, f_size, p_size; /* see .init-tables */
  Arena arena;
  
  AVER(spaceReturn != NULL);
  AVER(size > 0);
  /* no restrictions on base, it's simply passed through to VMCreate */

  /* VMCreate requires aligned size */
  size = SizeAlignUp(size, VMAlign());

  /* .vm.create: Create the space structure, initialize the VM part */
  res = VMCreate(&space, size, base);
  if(res) return res;

  arena = SpaceArena(space);
  /* see design.mps.space.arena */
  arena->base = VMBase(space);
  arena->limit = VMLimit(space);
  AVER(AddrOffset(arena->base, arena->limit) == size);
  arena->pageSize = VMAlign();
  arena->pageShift = SizeLog2(arena->pageSize);
  arena->pages = size >> arena->pageShift;

  /* .init-tables: Allocate the page tables at the base of the arena.
   *
   * .improve.table.zone-zero: It would be better to make sure that the
   * page tables are in zone zero, since that zone is least useful for
   * GC. (but it would change how SegAllocWithRefSet avoids allocating
   * over the tables, see .alloc.skip)
   *
   * There are two tables, the free table which is a bool array, and the
   * page table which is a PageStruct array.  Both tables are allocated
   * contiguously in one chunk.
   *
   * f_words is the number of words required for the free table.
   * 
   * f_size is the page-aligned size of the free table.
   * 
   * p_size is the page-aligned size of the page table.
   */
  f_words = SizeAlignUp(arena->pages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT;
  f_size = SizeAlignUp(f_words * sizeof(Word), arena->pageSize);
  p_size = SizeAlignUp(arena->pages * sizeof(PageStruct), arena->pageSize);
  arena->tablesSize = f_size + p_size;
  res = VMMap(space, arena->base, AddrAdd(arena->base, arena->tablesSize));
  if(res) {
    VMDestroy(space);
    return res;
  }
  arena->freeTable = (BT)arena->base;
  arena->pageTable = (Page)AddrAdd(arena->base, f_size);

  /* .tablepages: pages whose page index is < tablePages are recorded as
   * free but never allocated as alloc starts searching after the tables
   * (see .alloc.skip).  SegOfAddr uses the fact that these pages are
   * marked as free in order to detect "references" to these pages as
   * being bogus see .addr.free.
   */
  arena->tablePages = arena->tablesSize >> arena->pageShift;
  BTResRange(arena->freeTable, 0, arena->pages);

  /* Set the zone shift to divide the arena into the same number of
   * zones as will fit into a reference set (the number of bits in a
   * word).  Note that some zones are discontiguous in the arena if the
   * size is not a power of 2. See design.mps.space.arena.
   */
  space->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);

  /* Sign the arena. */
  arena->sig = ArenaSig;
  
  AVERT(Arena, arena);
  
  EVENT2(ArenaCreate, arena, space);

  *spaceReturn = space;
  return ResOK;
}

Res ArenaExtend(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}

Res ArenaRetract(Space space, Addr base, Size size)
{
  return ResUNIMPL;
}

/* ArenaDestroy -- finish the arena and destroy the space structure */

void ArenaDestroy(Space space)
{
  Arena arena;

  AVERT(Arena, SpaceArena(space));
  
  arena = SpaceArena(space);
  arena->sig = SigInvalid;
  VMUnmap(space, arena->base, AddrAdd(arena->base, arena->tablesSize));
  VMDestroy(space);     /* .vm.create */

  EVENT1(ArenaDestroy, arena);
}


/* ArenaReserved -- return the amount of reserved address space
 * ArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

Size ArenaReserved(Space space)
{
  AVERT(Arena, SpaceArena(space));
  return VMReserved(space);
}

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
  CHECKL(arena->pageShift <= MPS_WORD_WIDTH);
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
  CHECKL(arena->freeTable != NULL);
  CHECKL((Addr)arena->freeTable >= arena->base);
  CHECKL((Addr)&arena->freeTable[(arena->pages + MPS_WORD_WIDTH-1)>>MPS_WORD_SHIFT] <=
           arena->limit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* nothing else to check */
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

Res SegPrefExpress (SegPref sp, SegPrefKind kind, void *p)
{
  AVERT(SegPref,sp);
  AVER(sp != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    sp->high = TRUE;
    return ResOK;

  case SegPrefLow:
    AVER(p == NULL);
    sp->high = FALSE;
    return ResOK;

  case SegPrefRefSet:
    AVER(p != NULL);
    sp->refSet = *(RefSet *)p;
    return ResOK;

  default:
    /* see design.mps.pref.default */
    return ResOK;
  }
}


/* IndexOfAddr -- return the page index of the page containing an address */

static Index IndexOfAddr(Arena arena, Addr addr)
{
  AVERT(Arena, arena);
  AVER(arena->base <= addr);
  AVER(addr <= arena->limit);
  return AddrOffset(arena->base, addr) >> arena->pageShift;
}


/* SegAllocInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 *
 * .improve.bit-twiddle:  This code can probably be seriously
 * optimised by twiddling the bit table.
 */

static Bool SegAllocInArea(Index *baseReturn,
			   Space space, Size size, Addr base, Addr limit)
{
  Arena arena;
  Word pages;				/* number of pages equiv. to size */
  Index basePage, limitPage;		/* Index equiv. to base and limit */
  Index start, end;			/* base and limit of free run */

  AVER(baseReturn != NULL);
  AVERT(Space, space);  
  arena = SpaceArena(space);
  AVERT(Arena, arena);
  AVER(arena->base <= base);
  AVER(base < limit);
  AVER(limit <= arena->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, arena->pageSize));

  basePage = IndexOfAddr(arena, base);
  limitPage = IndexOfAddr(arena, limit);
  pages = size >> arena->pageShift;

  if(!BTFindResRange(&start, &end,
                     arena->freeTable,
                     basePage, limitPage,
                     pages))
    return FALSE;

  *baseReturn = start;
  return TRUE;
}


/* SegAllocWithRefSet
 *   -- try to allocate a segment with a particular RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to allocate a segment in the resulting set of
 * areas.
 */

static Bool SegAllocWithRefSet(Index *baseReturn,
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
  arenaBase = PageBase(arena, arena->tablePages);

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

      AVER(refSet != RefSetUNIV ||
           (base == arenaBase && limit == arena->limit));

      /* Try to allocate a segment in the area. */
      if(AddrOffset(base, limit) >= size &&
         SegAllocInArea(baseReturn, space, size, base, limit))
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


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegPref pref, Space space, Size size, Pool pool)
{
  Arena arena = SpaceArena(space);
  Index i, pages, base;
  Addr addr;
  Seg seg;
  Res res;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVERT(Arena, SpaceArena(space));
  AVER(size > 0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arena->pageSize));
  
  /* NULL is used as a discriminator (see design.mps.arena.vm.table.disc) */
  /* therefore the real pool must be non-NULL. */
  AVER(pool != NULL);

  if(!SegAllocWithRefSet(&base, space, size, pref->refSet) &&
     (pref->refSet == RefSetUNIV ||
      !SegAllocWithRefSet(&base, space, size, RefSetUNIV))) {
    /* .improve.alloc-fail: This could be because the request was */
    /* too large, or perhaps the arena is fragmented.  We could return a */
    /* more meaningful code. */
    return ResRESOURCE;
  }
  
  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, because the unwind (in case of failure) */
  /* is simpler. */
  addr = PageBase(arena, base);
  res = VMMap(space, addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = PageSeg(&arena->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(!BTGet(arena->freeTable, base));
  BTSet(arena->freeTable, base);
  pages = size >> arena->pageShift;
  if(pages > 1) {
    Addr limit = PageBase(arena, base + pages);
    SegSetSingle(seg, FALSE);
    for(i = base + 1; i < base + pages; ++i) {
      AVER(!BTGet(arena->freeTable, i));
      BTSet(arena->freeTable, i);
      PageTail(&arena->pageTable[i])->pool = NULL;
      PageTail(&arena->pageTable[i])->seg = seg;
      PageTail(&arena->pageTable[i])->limit = limit;
    }
  } else
    SegSetSingle(seg, TRUE);
  
  AVERT(Seg, seg);
  
  EVENT5(SegAlloc, arena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;
}


/* SegFree - free a segment in the arena */

void SegFree(Space space, Seg seg)
{
  Arena arena;
  Page page;
  Index i, pl, pn;
  Addr base, limit; 

  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  page = PageOfSeg(seg);
  limit = SegLimit(space, seg);
  i = page - arena->pageTable;
  AVER(i <= arena->pages);

  SegFinish(seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(arena, i);

  /* Calculate the number of pages in the segment, and hence the
   * limit for .free.loop */
  pn = AddrOffset(base, limit) >> arena->pageShift;
  pl = i + pn;
  /* .free.loop: */
  while(i < pl) {
    AVER(BTGet(arena->freeTable, i));
    BTRes(arena->freeTable, i);
    ++i;
  }

  /* .free.unmap: Unmap the segment memory. */
  VMUnmap(space, base, PageBase(arena, i));

  /* Double check that .free.loop takes us to the limit page of the
   * segment.
   */
  AVER(PageBase(arena, i) == limit);

  EVENT2(SegFree, arena, seg);
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
 * segment structure in the page table and then multiplying that
 * by the page size and adding it to the arena base address.
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

  return PageBase(arena, i);
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
    Index i = IndexOfAddr(arena, addr);
    /* .addr.free: If the page is recorded as being free then */
    /* either the page is free or it is */
    /* part of the arena tables (see .tablepages) */
    if(BTGet(arena->freeTable, i)) {
      Page page = &arena->pageTable[i];

      if(SegPool(PageSeg(page)) != NULL)
        *segReturn = PageSeg(page);
      else
        *segReturn = PageTail(page)->seg;
      return TRUE;
    }
  }
  
  return FALSE;
}


/* SegSearch -- search for a segment
 *
 * Searches for a segment in the arena starting at page index i,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Bool SegSearch(Seg *segReturn, Arena arena, Index i)
{
  AVER(segReturn != NULL);
  AVERT(Arena, arena);
  AVER(arena->tablePages <= i);
  AVER(i <= arena->pages);

  /* static function called with checked arguments, */
  /* so we don't bother checking them here as well */

  while(i < arena->pages &&
        (!BTGet(arena->freeTable, i) ||
         SegPool(PageSeg(&arena->pageTable[i])) == NULL)) {
    ++i;
  }
  
  if(i < arena->pages) {
    *segReturn = PageSeg(&arena->pageTable[i]);
    return TRUE;
  }
  
  AVER(i == arena->pages);
  return FALSE;
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
  return SegSearch(segReturn, arena, (Index)arena->tablePages);
}


/* SegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 */

Bool SegNext(Seg *segReturn, Space space, Addr addr)
{
  Arena arena;
  Index i;

  AVER(segReturn != NULL);
  AVERT(Space, space);
  /* There are further restrictions on addr, but they are much */
  /* harder to check */
  AVER(AddrIsAligned((addr), ArenaAlign(space)));

  arena = SpaceArena(space);
  AVERT(Arena, arena);
  i = IndexOfAddr(arena, addr);
  /* There are fewer pages than addresses, therefore the */
  /* page index can never wrap around */
  AVER(i+1 != 0);
  return SegSearch(segReturn, arena, i + 1);
}


