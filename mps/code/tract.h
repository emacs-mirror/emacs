/* tract.h: PAGE TABLE INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */


#ifndef tract_h
#define tract_h

#include "mpmtypes.h"
#include "bt.h"
#include "ring.h"
#include "tree.h"


/* TractStruct -- tract structure
 *
 * .tract: Tracts represent the grains of memory allocation from
 * the arena.  <design/arena>.
 */

typedef struct TractStruct { /* Tract structure */
  Pool pool;      /* MUST BE FIRST <design/arena#.tract.field.pool> */
  Seg seg;                     /* NULL or segment containing tract */
  Addr base;                   /* Base address of the tract */
} TractStruct;


extern Addr (TractBase)(Tract tract);
#define TractBase(tract)         ((tract)->base)
extern Addr TractLimit(Tract tract, Arena arena);

#define TractHasPool(tract) (TractPool(tract) != NULL)
#define TractPool(tract)         RVALUE((tract)->pool)
#define TractHasSeg(tract)       ((tract)->seg != NULL)
#define TractSeg(tract)          RVALUE((tract)->seg)

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
 * is central to the design of the arena. <design/arenavm#.table>
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union.  <design/arena#.tract.field.pool>.
 */

typedef union PageUnion {     /* page structure */
  Pool pool;                  /* discriminator */
  TractStruct alloc;          /* allocated tract, pool != NULL */
} PageUnion;


#define PageTract(page)       (&(page)->alloc)
#define PageOfTract(tract)    PARENT(PageUnion, alloc, tract)
#define PagePool(page)        RVALUE((page)->pool)
#define PageIsAllocated(page) (PagePool(page) != NULL)


/* Chunks */


#define ChunkSig ((Sig)0x519C804C) /* SIGnature CHUNK */

typedef struct ChunkStruct {
  Sig sig;              /* design.mps.sig.field */
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
 * <design/arenavm#.table.linear>
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
 * <design/arena-tract-iter#.if.macro>.
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
 * <design/arena#.tract.for>.
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
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
