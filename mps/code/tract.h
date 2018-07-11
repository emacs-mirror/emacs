/* tract.h: PAGE TABLE INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
 */


#ifndef tract_h
#define tract_h

#include "mpmtypes.h"
#include "bt.h"
#include "ring.h"
#include "tree.h"


/* Page states
 *
 * .states: The first word of the page descriptor contains a pointer to
 * the page's owning pool if the page is allocated.  The bottom two bits
 * indicate the page state.  Note that the page descriptor itself may
 * not be mapped since it is stored in a SparseArray.
 */

#define PageStateALLOC 0    /* allocated to a pool as a tract */
#define PageStateSPARE 1    /* free but mapped to backing store */
#define PageStateFREE  2    /* free and unmapped (address space only) */
#define PageStateWIDTH 2    /* bitfield width */

typedef union PagePoolUnion {
  unsigned state : PageStateWIDTH; /* see .states */
  Pool pool;
} PagePoolUnion;



/* TractStruct -- tract structure
 *
 * .tract: Tracts represent the grains of memory allocation from
 * the arena.  See <design/arena/>.
 */

typedef struct TractStruct { /* Tract structure */
  PagePoolUnion pool; /* MUST BE FIRST (<design/arena/#tract.field> pool) */
  Seg seg;                     /* NULL or segment containing tract */
  Addr base;                   /* Base address of the tract */
  TraceSet white : TraceLIMIT; /* traces for which tract is white */
} TractStruct;


extern Addr (TractBase)(Tract tract);
#define TractBase(tract)         ((tract)->base)
extern Addr TractLimit(Tract tract, Arena arena);

#define TractHasPool(tract) \
  ((tract)->pool.state == PageStateALLOC && TractPool(tract))
#define TractPool(tract)         ((tract)->pool.pool)
#define TractHasSeg(tract)       ((tract)->seg != NULL)
#define TractSeg(tract)          ((tract)->seg)
#define TractWhite(tract)        ((tract)->white)
#define TractSetWhite(tract, w)  ((void)((tract)->white = (w)))

extern Bool TractCheck(Tract tract);
extern void TractInit(Tract tract, Pool pool, Addr base);
extern void TractFinish(Tract tract);


/* TRACT_*SEG -- Test / set / unset seg->tract associations
 *
 * These macros all multiply evaluate the tract parameter
 */

#define TRACT_SEG(segReturn, tract) \
  (TractHasSeg(tract) && ((*(segReturn) = (tract)->seg), TRUE))

#define TRACT_SET_SEG(tract, _seg) \
  BEGIN (tract)->seg = (_seg); END

#define TRACT_UNSET_SEG(tract) \
  BEGIN (tract)->seg = NULL; END


/* PageUnion -- page descriptor
 *
 * .page-table: The page table (defined as a PageUnion array)
 * is central to the design of the arena.
 * See <design/arenavm/#table>.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union.  See <design/arena/#tract.field.pool>.
 */

typedef struct PageSpareStruct {
  PagePoolUnion pool;         /* spare tract, pool.state == PoolStateSPARE */
  RingStruct spareRing;       /* link in arena spare ring, LRU order */
} PageSpareStruct;

typedef union PageUnion {     /* page structure */
  PagePoolUnion pool;         /* pool.state is the discriminator */
  TractStruct alloc;          /* allocated tract, pool.state == PoolStateALLOC */
  PageSpareStruct spare;      /* spare page, pool.state == PoolStateSPARE */
} PageUnion;


#define PageTract(page)       (&(page)->alloc)
#define PageOfTract(tract)    PARENT(PageUnion, alloc, tract)
#define PagePool(page)        RVALUE((page)->pool.pool)
#define PageIsAllocated(page) RVALUE(PagePool(page) != NULL)
#define PageState(page)       RVALUE((page)->pool.state)
#define PageSpareRing(page)   (&(page)->spare.spareRing)
#define PageOfSpareRing(node) PARENT(PageUnion, spare, RING_ELT(PageSpare, spareRing, node))

#define PageSetPool(page, _pool) \
  BEGIN \
    Page _page = (page); \
    _page->pool.pool = (_pool); \
    AVER(PageState(_page) == PageStateALLOC); \
  END

#define PageSetType(page, _state) \
  BEGIN \
    Page _page = (page); \
    AVER(PagePool(_page) == NULL); \
    _page->pool.state = (_state); \
  END


/* Chunks */


#define ChunkSig ((Sig)0x519C804C) /* SIGnature CHUNK */

typedef struct ChunkStruct {
  Sig sig;              /* <design/sig/> */
  Serial serial;        /* serial within the arena */
  Arena arena;          /* parent arena */
  RingStruct arenaRing; /* node in ring of all chunks in arena */
  TreeStruct chunkTree; /* node in tree of all chunks in arena */
  Size pageSize;        /* size of pages */
  Shift pageShift;      /* log2 of page size, for shifts */
  Addr base;            /* base address of chunk */
  Addr limit;           /* limit address of chunk */
  Index allocBase;      /* index of first page allocatable to clients */
  Index pages;          /* index of the page after the last allocatable page */
  BT allocTable;        /* page allocation table */
  Page pageTable;       /* the page table */
  Count pageTablePages; /* number of pages occupied by page table */
  Size reserved;        /* reserved address space for chunk (including overhead
                           such as losses due to alignment): must not change
                           (or arena reserved calculation will break) */
} ChunkStruct;


#define ChunkArena(chunk) RVALUE((chunk)->arena)
#define ChunkSize(chunk) AddrOffset((chunk)->base, (chunk)->limit)
#define ChunkPageSize(chunk) RVALUE((chunk)->pageSize)
#define ChunkPageShift(chunk) RVALUE((chunk)->pageShift)
#define ChunkPagesToSize(chunk, pages) ((Size)(pages) << (chunk)->pageShift)
#define ChunkSizeToPages(chunk, size) ((Count)((size) >> (chunk)->pageShift))
#define ChunkPage(chunk, pi) (&(chunk)->pageTable[pi])
#define ChunkOfTree(tree) PARENT(ChunkStruct, chunkTree, tree)
#define ChunkReserved(chunk) RVALUE((chunk)->reserved)

extern Bool ChunkCheck(Chunk chunk);
extern Res ChunkInit(Chunk chunk, Arena arena, Addr base, Addr limit,
                     Size reserved, BootBlock boot);
extern void ChunkFinish(Chunk chunk);
extern Compare ChunkCompare(Tree tree, TreeKey key);
extern TreeKey ChunkKey(Tree tree);
extern Bool ChunkCacheEntryCheck(ChunkCacheEntry entry);
extern void ChunkCacheEntryInit(ChunkCacheEntry entry);
extern Bool ChunkOfAddr(Chunk *chunkReturn, Arena arena, Addr addr);
extern Res ChunkNodeDescribe(Tree node, mps_lib_FILE *stream);


/* AddrPageBase -- the base of the page this address is on */

#define AddrPageBase(chunk, addr) \
  AddrAlignDown((addr), ChunkPageSize(chunk))


/* Page table functions */

extern Tract TractOfBaseAddr(Arena arena, Addr addr);
extern Bool TractOfAddr(Tract *tractReturn, Arena arena, Addr addr);


/* INDEX_OF_ADDR -- return the index of the page containing an address
 *
 * .index.addr: The address passed may be equal to the limit of the
 * arena, in which case the last page index plus one is returned.  (It
 * is, in a sense, the limit index of the page table.)
 */

#define INDEX_OF_ADDR(chunk, addr) \
  ((Index)ChunkSizeToPages(chunk, AddrOffset((chunk)->base, addr)))

extern Index IndexOfAddr(Chunk chunk, Addr addr);


/* PageIndexBase -- map page index to base address of page
 *
 * See <design/arenavm/#table.linear>
 */

#define PageIndexBase(chunk, i) \
  AddrAdd((chunk)->base, ChunkPagesToSize(chunk, i))


/* TractAverContiguousRange -- verify that range is contiguous */

#define TractAverContiguousRange(arena, rangeBase, rangeLimit) \
  BEGIN \
    Chunk _ch = NULL; \
    \
    UNUSED(_ch); \
    AVER(ChunkOfAddr(&_ch, arena, rangeBase)); \
    AVER((rangeLimit) <= _ch->limit); \
  END


/* TRACT_TRACT_FOR -- iterate over a range of tracts in a chunk
 *
 * See <design/arena-tract-iter/#if.macro>.
 * Parameters arena & limit are evaluated multiple times.
 * Check first tract & last tract lie with the same chunk.
 */

#define TRACT_TRACT_FOR(tract, addr, arena, firstTract, limit) \
  tract = (firstTract); addr = TractBase(tract); \
  TractAverContiguousRange(arena, addr, limit); \
  for(; tract != NULL; \
      (addr = AddrAdd(addr, ArenaGrainSize(arena))), \
      (addr < (limit) ? \
        (tract = PageTract(PageOfTract(tract) + 1)) : \
        (tract = NULL) /* terminate loop */))


/* TRACT_FOR -- iterate over a range of tracts in a chunk
 *
 * See <design/arena/#tract.for>.
 * Parameters arena & limit are evaluated multiple times.
 */

#define TRACT_FOR(tract, addr, arena, base, limit) \
  TRACT_TRACT_FOR(tract, addr, arena, TractOfBaseAddr(arena, base), limit)


extern void PageAlloc(Chunk chunk, Index pi, Pool pool);
extern void PageInit(Chunk chunk, Index pi);
extern void PageFree(Chunk chunk, Index pi);


#endif /* tract_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
