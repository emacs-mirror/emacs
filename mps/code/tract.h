/* impl.h.tract: PAGE TABLE INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */


#ifndef tract_h
#define tract_h

#include "mpmtypes.h"
#include "ring.h"


/* TractStruct -- tract structure
 *
 * .tract: Tracts represent the grains of memory allocation from
 * the arena.  See <design/arena/>.
 *
 * .bool: The hasSeg field is a boolean, but can't be represented
 * as type Bool. See <design/arena/#tract.field.hasSeg>.
 */

typedef struct TractStruct { /* Tract structure */
  Pool pool;      /* MUST BE FIRST (<design/arena/#tract.field> pool) */
  void *p;                     /* pointer for use of owning pool */
  Addr base;                   /* Base address of the tract */
  TraceSet white : TraceLIMIT; /* traces for which tract is white */
  unsigned int hasSeg : 1;     /* does tract have a seg in p? See .bool */
} TractStruct;


extern Addr (TractBase)(Tract tract);
#define TractBase(tract)         ((tract)->base)
extern Addr TractLimit(Tract tract);

#define TractPool(tract)         ((tract)->pool)
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
 *
 * .states: Pages (hence PageStructs that describe them) can be in
 * one of 3 states:
 *  allocated (to a pool as tracts)
 *   allocated pages are mapped
 *   BTGet(allocTable, i) == 1
 *   PageRest()->pool == pool
 *  spare
 *   these pages are mapped
 *   BTGet(allocTable, i) == 0
 *   PageRest()->pool == NULL
 *   PageRest()->type == PageTypeSpare
 *  free
 *   these pages are not mapped
 *   BTGet(allocTable, i) == 0
 *   PTE may itself be unmapped, but when it is (use pageTableMapped
 *     to determine whether page occupied by page table is mapped):
 *   PageRest()->pool == NULL
 *   PageRest()->type == PageTypeFree
 */

enum {PageTypeSpare=1, PageTypeFree};

typedef struct PageStruct {     /* page structure */
  union {
    TractStruct tractStruct;    /* allocated tract */
    struct {
      Pool pool;                /* NULL, must be first field (.page) */
      int type;                 /* see .states */
    } rest;                     /* other (non-allocated) page */
  } the;
} PageStruct;


/* PageTract -- tract descriptor of an allocated page */

#define PageTract(page) (&(page)->the.tractStruct)

/* PageOfTract -- VM page descriptor from arena tract */

#define PageOfTract(tract) PARENT(PageStruct, the.tractStruct, (tract))

/* PagePool -- pool field of a page */

#define PagePool(page) ((page)->the.rest.pool)

/* PageIsAllocated -- is a page allocated?
 *
 * See <design/arenavm/#table.disc>.
 */

#define PageIsAllocated(page) ((page)->the.rest.pool != NULL)

/* PageType -- type of page */

#define PageType(page) ((page)->the.rest.type)


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
extern void ChunkEncache(Arena arena, Chunk chunk);

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


/* TractIsContiguousRange -- do base and limit define a contiguous range */

#define AverTractIsContiguousRange(arena, rangeBase, rangeLimit) \
  BEGIN \
    Chunk _ch; \
    \
    UNUSED(_ch); \
    AVER(ChunkOfAddr(&_ch, arena, rangeBase) && (rangeLimit) <= _ch->limit); \
  END


extern Bool TractFirst(Tract *tractReturn, Arena arena);
extern Bool TractNext(Tract *tractReturn, Arena arena, Addr addr);


/* TRACT_TRACT_FOR -- iterate over a range of tracts
 *
 * See <design/arena/#tract-iter.if.macro>.
 * Parameters arena & limit are evaluated multiple times.
 * Check first tract & last tract lie with the same chunk.
 */

#define TRACT_TRACT_FOR(tract, addr, arena, firstTract, limit) \
  tract = (firstTract); addr = TractBase(tract); \
  AverTractIsContiguousRange(arena, addr, limit); \
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
