/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.9) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 * design.mps.arenavm (beware, design.mps.arena is obsolete)
 */


#include "mpm.h"


SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.9) $");


/* Space Arena Projection
 * 
 * Only the arena module needs to discuss the arena object, hence, this
 * method is private to this module.
 */

#define SpaceArena(space)       (&(space)->arenaStruct)

/* Page Index to Base address mapping
 *
 * See design.mps.arenavm.table.linear
 */

#define PageBase(arena, pi) \
  AddrAdd((arena)->base, ((pi) << arena->pageShift))


/* Index Types
 * 
 * PI is the type of a value used to index into the page table.
 * 
 * BI is the type of a value used to index into a bool table (See ABTGet
 * and ABTSet in this module).
 */

typedef Size PI;
typedef Size BI;


/* PageStruct -- page structure
 *
 * The page table (defined as a PageStruct array) is central to the
 * design of the arena.  See design.mps.arenavm.table.*
 */

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct head;             /* segment */
    struct {
      Pool pool;                /* .page: NULL, must be first field
                                 * see impl.h.mpmst.seg.pool */
      Seg seg;                  /* segment at base page of run */
      Addr limit;               /* limit of segment */
    } tail;                     /* tail page */
  } the;
} PageStruct;


/* ABTGet -- get a bool from a bool table
 *
 * Note: The function version of ABTGet isn't used anywhere in
 * this source, but is left here in case we want to revert from
 * the macro.
 */

#if 0
static Bool (ABTGet)(ABT bt, BI i)
{
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  return (bt[wi] >> bi) & 1;
}
#endif /* 0 */

#define ABTGet(bt, i) \
  (((bt)[(i)>>MPS_WORD_SHIFT] >> ((i)&(MPS_WORD_WIDTH-1))) & 1)


/* ABTSet -- set a bool in a bool table */

static void ABTSet(ABT bt, BI i, Bool b)
{
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  Word mask = ~((Word)1 << bi);
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  bt[wi] = (bt[wi] & mask) | ((Word)b << bi);
}


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
  PI i;
  
  AVER(spaceReturn != NULL);
  AVER(size > 0);
  /* no restrictions on base, it's simply passed through to VMCreate */

  /* VMCreate requires aligned size */
  size = SizeAlignUp(size, VMAlign());

  /* .vm.create: Create the space structure, initialize the VM part */
  res = VMCreate(&space, size, base);
  if(res) return res;

  arena = SpaceArena(space);
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
   * GC. (but it would change how SegAlloc avoids allocating over the
   * tables, see .alloc.skip)
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
  arena->freeTable = (ABT)arena->base;
  arena->pageTable = (Page)AddrAdd(arena->base, f_size);

  /* .tablepages: pages whose page index is < tablePages are recorded as
   * free but never allocated as alloc starts searching after the tables
   * (see .alloc.skip)
   */
  arena->tablePages = arena->tablesSize >> arena->pageShift;
  for(i = 0; i < arena->pages; ++i)
    ABTSet(arena->freeTable, i, TRUE);

  /* Set the zone shift to divide the arena into the same number of
   * zones as will fit into a reference set (the number of bits in a
   * word).  Note that some zones are discontiguous in the arena if the
   * size is not a power of 2.
   */
  space->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);

  /* Sign the arena. */
  arena->sig = ArenaSig;
  
  AVERT(Arena, arena);

  *spaceReturn = space;
  return ResOK;
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


/* SegCheck -- check the consistency of a segment structure */  

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, seg->pool);
  /* .seg.check-little: all other fields can't be checked */
  return TRUE;
}


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, Space space, Size size, Pool pool)
{
  Arena arena = SpaceArena(space);
  PI pi, count, pages, base = 0;        /* whinge stopper */
  Addr addr;
  Seg seg;
  Res res;

  AVER(segReturn != NULL);
  AVERT(Arena, SpaceArena(space));
  AVER(size > 0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arena->pageSize));
  
  /* NULL is used as a discriminator (see
   * design.mps.arenavm.table.disc), therefore the real pool must be
   * non-NULL.
   */
  AVER(pool != NULL);

  /* Search for a free run of pages in the free table.
   * .alloc.skip: Start from arena->tablePages (.tablepages).
   * .improve.bit-twiddle:  This code can probably be seriously
   * optimised by twiddling the bit table.
   */
  pages = size >> arena->pageShift;
  count = 0;
  for(pi = arena->tablePages; pi < arena->pages; ++pi) {
    if(ABTGet(arena->freeTable, pi)) {
      if(count == 0)
        base = pi;
      ++count;
      if(count == pages)
        goto found;
    } else
      count = 0;
  }
  
  /* No space was found.
   * .improve.alloc-fail: This could be because the request was
   * too large, or perhaps the arena is fragmented.  We could return a
   * more meaningful code.
   */
  return ResRESOURCE;

found:
  /* .alloc.early-map: Map in the segment memory before actually
   * allocating the pages, because the unwind (in case of failure)
   * is simpler. */
  addr = PageBase(arena, base);
  res = VMMap(space, addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = &arena->pageTable[base].the.head;
  seg->pool = pool;
  seg->p = NULL;
  seg->rank = RankEXACT;    /*  exact by default */
  seg->condemned = TraceIdNONE;
  seg->grey = TraceSetEMPTY;
  seg->buffer = NULL;
  RingInit(&seg->poolRing);

  seg->pm = AccessSetEMPTY; /* see impl.c.shield */
  seg->sm = AccessSetEMPTY;
  seg->depth = 0;

  /* Allocate the first page, and, if there is more than one page,
   * allocate the rest of the pages and store the multi-page information
   * in the page table.
   */
  AVER(ABTGet(arena->freeTable, base));
  ABTSet(arena->freeTable, base, FALSE);
  if(pages > 1) {
    Addr limit = PageBase(arena, base + pages);
    seg->single = FALSE;
    for(pi = base + 1; pi < base + pages; ++pi) {
      AVER(ABTGet(arena->freeTable, pi));
      ABTSet(arena->freeTable, pi, FALSE);
      arena->pageTable[pi].the.tail.pool = NULL;
      arena->pageTable[pi].the.tail.seg = seg;
      arena->pageTable[pi].the.tail.limit = limit;
    }
  } else {
    seg->single = TRUE;
  }
  
  AVERT(Seg, seg);

  *segReturn = seg;
  return ResOK;
}


/* SegFree - free a segment in the arena */

void SegFree(Space space, Seg seg)
{
  Arena arena;
  Page page;
  PI pi, pl, pn;
  Addr base, limit; 

  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  page = PARENT(PageStruct, the.head, seg);
  limit = SegLimit(space, seg);
  pi = page - arena->pageTable;
  AVER(pi <= arena->pages);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(arena, pi);

  /* Calculate the number of pages in the segment, and hence the
   * limit for .free.loop */
  pn = AddrOffset(base, limit) >> arena->pageShift;
  pl = pi + pn;
  /* .free.loop: */
  for( ; pi < pl; ++pi) {
    AVER(ABTGet(arena->freeTable, pi) == FALSE);
    ABTSet(arena->freeTable, pi, TRUE);
  }

  /* .free.unmap: Unmap the segment memory. */
  VMUnmap(space, base, PageBase(arena, pi));

  /* Double check that .free.loop takes us to the limit page of the
   * segment.
   */
  AVER(PageBase(arena, pi) == limit);
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
  PI pi;
  
  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  arena = SpaceArena(space);
  page = PARENT(PageStruct, the.head, seg);
  pi = page - arena->pageTable;

  return PageBase(arena, pi);
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
  if(seg->single)
    return AddrAdd(SegBase(space, seg), arena->pageSize);
  else {
    page = PARENT(PageStruct, the.head, seg);
    return page[1].the.tail.limit;
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
    PI pi = AddrOffset(arena->base, addr) >> arena->pageShift;
    if(!ABTGet(arena->freeTable, pi)) {
      Page page = &arena->pageTable[pi];
      if(page->the.head.pool != NULL)
        *segReturn = &page->the.head;
      else
        *segReturn = page->the.tail.seg;
      return TRUE;
    }
  }
  
  return FALSE;
}


/* SegSearch -- search for a segment
 *
 * Searches for a segment in the arena starting at page index pi,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg SegSearch(Arena arena, PI pi)
{
  while(pi < arena->pages &&
        (ABTGet(arena->freeTable, pi) ||
         arena->pageTable[pi].the.head.pool == NULL))
    ++pi;
  
  if(pi < arena->pages)
    return &arena->pageTable[pi].the.head;
  
  AVER(pi == arena->pages);
  return NULL;
}


/* SegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 * See SEG_FOR (impl.h.mpm).
 */

Seg SegFirst(Space space)
{
  Arena arena;

  AVERT(Arena, SpaceArena(space));
  arena = SpaceArena(space);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return SegSearch(arena, (PI)arena->tablePages);
}


/* SegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.  See SEG_FOR (impl.h.mpm).
 */

Seg SegNext(Space space, Seg seg)
{
  Arena arena;
  Page page;
  PI pi;
  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);
  page = PARENT(PageStruct, the.head, seg);
  arena = SpaceArena(space);
  pi = page - arena->pageTable;
  return SegSearch(arena, pi + 1);
}


