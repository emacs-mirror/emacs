/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.4) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#include "mpm.h"

SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.4) $");

#define SpaceArena(space)       (&(space)->arenaStruct)

typedef Size PI;                /* page index type */
typedef Size BI;                /* bool index type */


/* PageStruct -- page structure
 *
 * The page table is an array of page structures.
 * The structure can contain a segment (head) or a tail struct.
 * A single page is allocated a single entry (of course).
 * When a multi-page segment is allocated, the pool (first) field
 * contains the pool in the head page, and NULL in the tail pages.
 * The tail pages also store a pointer to the segment structure,
 * so that lookup is fast, and the limit of the segment in memory.
 */

typedef struct PageStruct {             /* page structure */
  union
  {
    SegStruct head;             /* segment */
    struct
    {
      Pool pool;                /* NULL -- MUST BE FIRST FIELD */
      Seg seg;                  /* segment at base page of run */
      Addr limit;               /* limit of segment */
    } tail;                     /* tail page */
  } the;
} PageStruct;


/* BTGet -- get a bool from the bool table
 *
 * Note: The function version of BTGet isn't used anywhere in
 * this source, but is left here in case we want to revert from
 * the macro BTGET.
 */

#if 0
static Bool (BTGet)(BT bt, BI i)
{
  Size wi = i >> WORD_SHIFT;            /* word index */
  Size bi = i & (WORD_WIDTH - 1);       /* bit index */
  return (bt[wi] >> bi) & 1;
}
#endif /* 0 */

#define BTGET(bt, i) \
  (1 & ( (bt)[(i)>>WORD_SHIFT] >> ((WORD_WIDTH-1)&(i)) ) )


/* BTSet -- set a bool in a bool table */

static void BTSet(BT bt, BI i, Bool b)
{
  Size bi = i & (WORD_WIDTH - 1);       /* bit index */
  Word mask = ~((Word)1 << bi);
  Size wi = i >> WORD_SHIFT;            /* word index */
  bt[wi] = (bt[wi] & mask) | ((Word)b << bi);
}


#define PageBase(arena, pi) \
  AddrAdd((arena)->base, ((pi) << arena->pageShift))


/* ArenaCreate -- create the arena
 *
 * In fact, this creates the space structure and initialize the
 * arena part.
 */

Res ArenaCreate(Space *spaceReturn, Size size, Addr base)
{
  Res res;
  Space space;
  Size f_words, f_size, p_size;
  Arena arena;
  PI i;
  
  AVER(spaceReturn != NULL);
  AVER(size > 0);

  /* Create the space structure, initialize the VM part, and */
  /* the current attribute structure. */
  res = VMCreate(&space, size, base);
  if(res) return res;

  arena = SpaceArena(space);
  arena->base = VMBase(space);
  arena->limit = VMLimit(space);
  size = AddrOffset(arena->base, arena->limit);
  arena->pageSize = VMAlign();
  arena->pageShift = SizeLog2(arena->pageSize);
  arena->pages = size >> arena->pageShift;

  /* Allocate the page tables at the base of the arena. */
  /* @@@@ It would be better to make sure that the page tables */
  /* are in zone zero, since that zone is least useful for GC. */
  f_words = SizeAlignUp(arena->pages, WORD_WIDTH) >> WORD_SHIFT;
  f_size = SizeAlignUp(f_words * sizeof(Word), arena->pageSize);
  p_size = SizeAlignUp(arena->pages * sizeof(PageStruct), arena->pageSize);
  arena->tablesSize = f_size + p_size;
  res = VMMap(space, arena->base, AddrAdd(arena->base, arena->tablesSize));
  if(res) {
    VMDestroy(space);
    return res;
  }
  arena->freeTable = (Word *)arena->base;
  arena->pageTable = (Page)AddrAdd(arena->base, f_size);

  /* .tablePages: pages numbered < tablePages are recorded as free
   * but never allocated as segments as they contain the arena's
   * tables, but have no associated pool.
   */
  arena->tablePages = (f_size + p_size) >> arena->pageShift;
  for(i = 0; i < arena->pages; ++i)
    BTSet(arena->freeTable, i, TRUE);

  /* Set the zone shift to divide the arena into the same number of */
  /* zones as will fit into a reference set (the number of bits in */
  /* a word). */
/* space->zoneShift = SizeLog2(size >> WORD_SHIFT); */
  space->zoneShift = 20; /* @@@@ */
  

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
  VMDestroy(space);
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
  return SpaceArena(space)->vmStruct.reserved;
}

Size ArenaCommitted(Space space)
{
  AVERT(Arena, SpaceArena(space));
  return SpaceArena(space)->vmStruct.mapped;
}


/* ArenaCheck -- check of the consistency of the arena structure */

Bool ArenaCheck(Arena arena)
{
  CHECKS(Arena, arena);
  CHECKD(VM, &arena->vmStruct);
  CHECKL(arena->base != (Addr)0);
  CHECKL(arena->base < arena->limit);
  CHECKL(SizeIsP2(arena->pageSize));
  CHECKL(arena->pageShift == SizeLog2(arena->pageSize));
  CHECKL(arena->pages == AddrOffset(arena->base, arena->limit) >> arena->pageShift);
  CHECKL(arena->pageTable != NULL);
  CHECKL((Addr)arena->pageTable >= arena->base);
  CHECKL((Addr)&arena->pageTable[arena->pages] <= arena->limit);
  CHECKL((Addr)arena->freeTable >= arena->base);
  CHECKL((Addr)&arena->freeTable[(arena->pages + WORD_WIDTH - 1) >> WORD_SHIFT] <= arena->limit);
  CHECKL(arena->tablesSize > 0);
  /* @@@@ Could check the consistency of the tables. */
  return TRUE;
}


/* SegCheck -- check the consistency of a segment structure */  

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, seg->pool);
  return TRUE;
}


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, Space space, Size size, Pool pool)
{
  Arena arena = SpaceArena(space);
  PI pi, count, pages, base = 0; 
  Addr addr;
  Seg seg;
  Res res;

  AVER(segReturn != NULL);
  AVERT(Arena, SpaceArena(space));
  AVER(size > 0);
  AVERT(Pool, pool);
  AVER(SizeIsAligned(size, arena->pageSize));
  
  /* The page table distinguishes allocated head pages (at the */
  /* base of segments) from others by storing NULL in the pool */
  /* field of the segment structure.  The real pool must */
  /* therefore be non-NULL. */
  AVER(pool != NULL);

  /* Search for a free run of pages in the free table. */
  /* Start from arena->tablePages (.tablePages). */
  /* @@@@ This code can probably be seriously optimised by */
  /* twiddling the bit table. */  
  pages = size >> arena->pageShift;
  count = 0;
  for(pi = arena->tablePages; pi < arena->pages; ++pi) {
    if(BTGET(arena->freeTable, pi)) {
      if(count == 0)
        base = pi;
      ++count;
      if(count == pages)
        goto found;
    } else
      count = 0;
  }
  
  /* No space was found.  This could be because the request was */
  /* too large, or perhaps the arena is fragmented.  Perhaps we */
  /* should return a more meaningful code. */
  return ResRESOURCE;

found:
  /* Map in the segment memory before actually allocating the pages */
  addr = PageBase(arena, base);
  res = VMMap(space, addr, AddrAdd(addr, size));
  if(res) return res;

  /* Initialize the generic segment structure. */
  seg = &arena->pageTable[base].the.head;
  seg->pool = pool;
  seg->p = NULL;
  seg->single = TRUE;
  seg->rank = RankEXACT;    /*  exact by default */
  seg->condemned = TraceIdNONE;

  seg->pm = AccessSetEMPTY; /* see impl.c.shield */
  seg->sm = AccessSetEMPTY;
  seg->depth = 0;

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page */
  /* information in the page table. */
  BTSet(arena->freeTable, base, FALSE);
  if(pages > 1) {
    Addr limit = PageBase(arena, base + pages);
    seg->single = FALSE;
    for(pi = base + 1; pi < base + pages; ++pi) {
      AVER(BTGET(arena->freeTable, pi));
      BTSet(arena->freeTable, pi, FALSE);
      arena->pageTable[pi].the.tail.pool = NULL;
      arena->pageTable[pi].the.tail.seg = seg;
      arena->pageTable[pi].the.tail.limit = limit;
    }
  }
  
  AVERT(Seg, seg);

  *segReturn = seg;
  return ResOK;
}


/* SegFree - free a segment in the arena */

void SegFree(Space space, Seg seg)
{
  Arena arena = SpaceArena(space);
  Page page = PARENT(PageStruct, the.head, seg);
  PI pi = page - arena->pageTable;
  Size size;
  Addr base; 
  Addr limit = SegLimit(space, seg);

  AVERT(Arena, SpaceArena(space));
  AVERT(Seg, seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped later. */
  base = PageBase(arena, pi);

  /* Set the free bit on the first page, and all subsequent */
  /* pages which are allocated and have a NULL pool field -- */
  /* they must be the rest of the segment. */
  /* @@@@ This is probably not the best way, as it involves */
  /* multiple probes to the page table.  Better to calculate */
  /* the number of pages in the segment and just update the */
  /* free table. */
  size = 0;
  do {
    BTSet(arena->freeTable, pi, TRUE);
    ++pi;
    size += arena->pageSize;
  } while(!BTGET(arena->freeTable, pi) &&
          arena->pageTable[pi].the.tail.pool == NULL);

  /* Unmap the segment memory. */
  VMUnmap(space, base, PageBase(arena, pi));

  /* Double check that the loop above takes us to the limit page */
  /* of the segment. */
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
 * There is scope for optimizing this, because both base
 * and limit calls do roughly the same thing twice.
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
    if(!BTGET(arena->freeTable, pi)) {
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
 */

static Seg SegSearch(Arena arena, PI pi)
{
  while(pi < arena->pages &&
        (BTGET(arena->freeTable, pi) ||
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
 * See SEG_FOR.
 */

Seg SegFirst(Space space)
{
  AVERT(Arena, SpaceArena(space));
  return SegSearch(SpaceArena(space), (PI)0);
}


/* SegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.  See SEG_FOR.
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


