/* tract.h: PAGE TABLE INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */


#ifndef tract_h
#define tract_h

#include "mpmtypes.h"
#include "ring.h"
#include "bt.h"


/* Page states
 *
 * .states: Pages (hence PageStructs that describe them) can be in
 * one of 3 states:
 *  allocated (to a pool as tracts)
 *   allocated pages are mapped
 *   BTGet(allocTable, i) == 1
 *   PageType() == PageStateALLOC
 *   PagePool()->pool == pool
 *  spare
 *   these pages are mapped
 *   BTGet(allocTable, i) == 0
 *   PageType() == PageStateSPARE
 *   PagePool() == NULL
 *  free
 *   these pages are not mapped
 *   BTGet(allocTable, i) == 0
 *   PTE may itself be unmapped, but when it is (use pageTableMapped
 *     to determine whether page occupied by page table is mapped):
 *   PagePool() == NULL
 *   PageType() == PageStateFREE
 */

#define PageStateALLOC 0
#define PageStateSPARE 1
#define PageStateFREE  2
#define PageStateWIDTH 2         /* bitfield width */


/* TractStruct -- tract structure
 *
 * .tract: Tracts represent the grains of memory allocation from
 * the arena.  See <design/arena/>.
 *
 * .bool: The hasSeg field is a boolean, but can't be represented
 * as type Bool. See <design/arena/#tract.field.hasSeg>.
 */

typedef union TractPoolUnion {
  Pool pool;
  unsigned state : PageStateWIDTH; /* see .states */
} TractPoolUnion;

typedef struct TractStruct { /* Tract structure */
  TractPoolUnion pool; /* MUST BE FIRST (<design/arena/#tract.field> pool) */
  void *p;                     /* pointer for use of owning pool */
  Addr base;                   /* Base address of the tract */
  TraceSet white : TraceLIMIT; /* traces for which tract is white */
  unsigned int hasSeg : 1;     /* does tract have a seg in p? See .bool */
} TractStruct;


extern Addr (TractBase)(Tract tract);
#define TractBase(tract)         ((tract)->base)
extern Addr TractLimit(Tract tract);

#define TractPool(tract)         ((tract)->pool.pool)
#define TractP(tract)            ((tract)->p)
#define TractSetP(tract, pp)     ((void)((tract)->p = (pp)))
#define TractHasSeg(tract)       ((Bool)(tract)->hasSeg)
#define TractSetHasSeg(tract, b) ((void)((tract)->hasSeg = (b)))
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
  (TractHasSeg(tract) && ((*(segReturn) = (Seg)TractP(tract)), TRUE))

#define TRACT_SET_SEG(tract, seg) \
  (TractSetHasSeg(tract, TRUE), TractSetP(tract, seg))

#define TRACT_UNSET_SEG(tract) \
  (TractSetHasSeg(tract, FALSE), TractSetP(tract, NULL))


/* PageStruct -- Page structure
 *
 * .page-table: The page table (defined as a PageStruct array)
 * is central to the design of the arena.
 * See <design/arenavm/#table>.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union.  See <design/arena/#tract.field.pool>.
 */

typedef struct PageStruct {     /* page structure */
  union PageStructUnion {
    TractPoolUnion pool;
    TractStruct tractStruct;    /* allocated tract */
    struct {
      TractPoolUnion pool;      /* MUST BE FIRST (<design/arena/#tract.field> pool) */
      RingStruct spareRing;
    } rest;                     /* other (non-allocated) page */
  } the;
} PageStruct;


#define PageTract(page)       (&(page)->the.tractStruct)
#define PageOfTract(tract) \
  PARENT(PageStruct, the, PARENT(union PageStructUnion, tractStruct, (tract)))
#define PagePool(page)        RVALUE((page)->the.pool.pool)
#define PageIsAllocated(page) RVALUE(PagePool(page) != NULL)
#define PageType(page)        RVALUE((page)->the.pool.state)
#define PageSpareRing(page)   RVALUE(&(page)->the.rest.spareRing)
#define PageOfSpareRing(node) RING_ELT(Page, the.rest.spareRing, node)

#define PageSetPool(page, _pool) \
  BEGIN \
    PageStruct *_page = (page); \
    _page->the.pool.pool = (_pool); \
    AVER(PageType(_page) == PageStateALLOC); \
  END

#define PageSetType(page, _state) \
  BEGIN \
    PageStruct *_page = (page); \
    AVER(PagePool(_page) == NULL); \
    _page->the.pool.state = (_state); \
  END


/* Chunks */


#define ChunkSig ((Sig)0x519C804C) /* SIGnature CHUNK */

typedef struct ChunkStruct {
  Sig sig;              /* <design/sig/> */
  Serial serial;        /* serial within the arena */
  Arena arena;          /* parent arena */
  RingStruct chunkRing; /* ring of all chunks in arena */
  Size pageSize;        /* size of pages */
  Shift pageShift;      /* log2 of page size, for shifts */
  Addr base;            /* base address of chunk */
  Addr limit;           /* limit address of chunk */
  Index allocBase;      /* index of first page allocatable to clients */
  Index pages;          /* index of the page after the last allocatable page */
  BT allocTable;        /* page allocation table */
  PageStruct* pageTable; /* the page table */
  Count pageTablePages; /* number of pages occupied by page table */
} ChunkStruct;


#define ChunkArena(chunk) ((chunk)->arena)
#define ChunkPageSize(chunk) ((chunk)->pageSize)
#define ChunkPageShift(chunk) ((chunk)->pageShift)
#define ChunkPagesToSize(chunk, pages) ((Size)(pages) << (chunk)->pageShift)
#define ChunkSizeToPages(chunk, size) ((Count)((size) >> (chunk)->pageShift))

extern Bool ChunkCheck(Chunk chunk);
extern Res ChunkInit(Chunk chunk, Arena arena,
                     Addr base, Addr limit, Align pageSize, BootBlock boot);
extern void ChunkFinish(Chunk chunk);

extern Bool ChunkCacheEntryCheck(ChunkCacheEntry entry);
extern void ChunkCacheEntryInit(ChunkCacheEntry entry);

extern Bool ChunkOfAddr(Chunk *chunkReturn, Arena arena, Addr addr);

/* CHUNK_OF_ADDR -- return the chunk containing an address
 *
 * arena and addr are evaluated multiple times.
 */

#define CHUNK_OF_ADDR(chunkReturn, arena, addr) \
  (((arena)->chunkCache.base <= (addr) && (addr) < (arena)->chunkCache.limit) \
   ? (*(chunkReturn) = (arena)->chunkCache.chunk, TRUE) \
   : ChunkOfAddr(chunkReturn, arena, addr))


/* AddrPageBase -- the base of the page this address is on */

#define AddrPageBase(chunk, addr) \
  AddrAlignDown((addr), ChunkPageSize(chunk))


/* Page table functions */

extern Tract TractOfBaseAddr(Arena arena, Addr addr);
extern Bool TractOfAddr(Tract *tractReturn, Arena arena, Addr addr);

/* TRACT_OF_ADDR -- return the tract containing an address */

#define TRACT_OF_ADDR(tractReturn, arena, addr) \
  BEGIN \
    Arena _arena = (arena); \
    Addr _addr = (addr); \
    Chunk _chunk; \
    Index _i; \
    \
    if (CHUNK_OF_ADDR(&_chunk, _arena, _addr)) { \
      _i = INDEX_OF_ADDR(_chunk, _addr); \
      if (BTGet(_chunk->allocTable, _i)) \
        *(tractReturn) = PageTract(&_chunk->pageTable[_i]); \
      else \
        *(tractReturn) = NULL; \
    } else \
        *(tractReturn) = NULL; \
  END


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
    AVER(ChunkOfAddr(&_ch, arena, rangeBase) && (rangeLimit) <= _ch->limit); \
  END


extern Bool TractFirst(Tract *tractReturn, Arena arena);
extern Bool TractNext(Tract *tractReturn, Arena arena, Addr addr);


/* TRACT_TRACT_FOR -- iterate over a range of tracts
 *
 * See <design/arena-tract-iter/#if.macro>.
 * Parameters arena & limit are evaluated multiple times.
 * Check first tract & last tract lie with the same chunk.
 */

#define TRACT_TRACT_FOR(tract, addr, arena, firstTract, limit) \
  tract = (firstTract); addr = TractBase(tract); \
  TractAverContiguousRange(arena, addr, limit); \
  for(; tract != NULL; \
      (addr = AddrAdd(addr, (arena)->alignment)), \
      (addr < (limit) ? \
        (tract = PageTract(PageOfTract(tract) + 1)) : \
        (tract = NULL) /* terminate loop */))


/* TRACT_FOR -- iterate over a range of tracts
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
