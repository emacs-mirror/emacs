/* tract.c: PAGE TABLES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .ullagepages: Pages whose page index is < allocBase are recorded as
 * free but never allocated as alloc starts searching after the tables.
 * TractOfAddr uses the fact that these pages are marked as free in order
 * to detect "references" to these pages as being bogus.
 */

#include "tract.h"
#include "boot.h"
#include "mpm.h"

SRCID(tract, "$Id$");


static void ChunkDecache(Arena arena, Chunk chunk);


/* TractArena -- get the arena of a tract */

#define TractArena(seg) PoolArena(TractPool(tract))


/* TractCheck -- check the integrity of a tract */

Bool TractCheck(Tract tract)
{
  CHECKU(Pool, TractPool(tract));
  CHECKL(AddrIsAligned(TractBase(tract), ArenaAlign(TractArena(tract))));
  if (TractHasSeg(tract)) {
    CHECKL(TraceSetCheck(TractWhite(tract)));
    CHECKU(Seg, (Seg)TractP(tract));
  } else {
    CHECKL(TractWhite(tract) == TraceSetEMPTY);
  }
  return TRUE;
}


/* TractInit -- initialize a tract */

void TractInit(Tract tract, Pool pool, Addr base)
{
  AVER(tract != NULL);
  AVERT(Pool, pool);

  tract->pool = pool;
  tract->base = base;
  tract->p = NULL;
  tract->white = TraceSetEMPTY;
  tract->hasSeg = FALSE;

  AVERT(Tract, tract);

}


/* TractFinish -- finish a tract */

void TractFinish(Tract tract)
{
  AVERT(Tract, tract);

  /* Check that there's no segment - and hence no shielding. */
  AVER(!TractHasSeg(tract));
  tract->pool = NULL;
}



/* .tract.critical: These tract functions are low-level and used
 * throughout. They are therefore on the critical path and their
 * AVERs are so-marked.
 */


/* TractBase -- return the base address of a tract */

Addr (TractBase)(Tract tract)
{
  Addr base;
  AVERT_CRITICAL(Tract, tract); /* .tract.critical */

  base = tract->base;
  return base;
}


/* TractLimit -- return the limit address of a segment */

Addr TractLimit(Tract tract)
{
  Arena arena;
  AVERT_CRITICAL(Tract, tract); /* .tract.critical */
  arena = TractArena(tract);
  AVERT_CRITICAL(Arena, arena);
  return AddrAdd(TractBase(tract), arena->alignment);
}


/* Chunk functions */


/* ChunkCheck -- check a chunk */

Bool ChunkCheck(Chunk chunk)
{
  CHECKS(Chunk, chunk);
  CHECKU(Arena, chunk->arena);
  CHECKL(chunk->serial < chunk->arena->chunkSerial);
  CHECKL(RingCheck(&chunk->chunkRing));
  CHECKL(ChunkPagesToSize(chunk, 1) == ChunkPageSize(chunk));
  CHECKL(ShiftCheck(ChunkPageShift(chunk)));

  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check chunk is in itself */
  CHECKL(chunk->base <= (Addr)chunk);
  CHECKL((Addr)(chunk+1) <= chunk->limit);
  CHECKL(ChunkSizeToPages(chunk, AddrOffset(chunk->base, chunk->limit))
         == chunk->pages);
  /* check that the tables fit in the chunk */
  CHECKL(chunk->allocBase <= chunk->pages);
  CHECKL(chunk->allocBase >= chunk->pageTablePages);

  CHECKL(chunk->allocTable != NULL);
  /* check that allocTable is in the chunk overhead */
  CHECKL((Addr)chunk->allocTable >= chunk->base);
  CHECKL(AddrAdd((Addr)chunk->allocTable, BTSize(chunk->pages))
         <= PageIndexBase(chunk, chunk->allocBase));

  /* check they don't overlap (knowing the order) */
  CHECKL(AddrAdd((Addr)chunk->allocTable, BTSize(chunk->pages))
         <= (Addr)chunk->pageTable);

  CHECKL(chunk->pageTable != NULL);
  CHECKL((Addr)chunk->pageTable >= chunk->base);
  CHECKL((Addr)&chunk->pageTable[chunk->pageTablePages]
         <= PageIndexBase(chunk, chunk->allocBase));
  /* check there's enough space in the page table */
  CHECKL(INDEX_OF_ADDR(chunk, (Addr)chunk->pageTable) >= 0);
  CHECKL(INDEX_OF_ADDR(chunk, AddrSub(chunk->limit, 1)) < chunk->pages);
  CHECKL(chunk->pageTablePages < chunk->pages);

  /* Could check the consistency of the tables, but not O(1). */
  return TRUE;
}


/* ChunkInit -- initialize generic part of chunk */

Res ChunkInit(Chunk chunk, Arena arena,
              Addr base, Addr limit, Align pageSize, BootBlock boot)
{
  Size size;
  Count pages;
  PageStruct *pageTable;
  Shift pageShift;
  Size pageTableSize;
  void *p;
  Res res;

  /* chunk is supposed to be uninitialized, so don't check it. */
  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(AddrIsAligned(base, pageSize));
  AVER(base < limit);
  AVER(AddrIsAligned(limit, pageSize));
  AVERT(Align, pageSize);
  AVER(pageSize > MPS_PF_ALIGN);
  AVERT(BootBlock, boot);

  chunk->serial = (arena->chunkSerial)++;
  chunk->arena = arena;
  RingInit(&chunk->chunkRing);
  RingAppend(&arena->chunkRing, &chunk->chunkRing);

  chunk->pageSize = pageSize;
  chunk->pageShift = pageShift = SizeLog2(pageSize);
  chunk->base = base;
  chunk->limit = limit;
  size = AddrOffset(base, limit);

  chunk->pages = pages = size >> pageShift;
  res = BootAlloc(&p, boot, (size_t)BTSize(pages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failAllocTable;
  chunk->allocTable = p;

  pageTableSize = SizeAlignUp(pages * sizeof(PageStruct), pageSize);
  chunk->pageTablePages = pageTableSize >> pageShift;

  res = (arena->class->chunkInit)(chunk, boot);
  if (res != ResOK)
    goto failClassInit;

  /* Put the page table as late as possible, as in VM systems we don't want */
  /* to map it. */
  res = BootAlloc(&p, boot, (size_t)pageTableSize, (size_t)pageSize);
  if (res != ResOK)
    goto failAllocPageTable;
  chunk->pageTable = pageTable = p;

  /* @@@@ Is BootAllocated always right? */
  chunk->allocBase = (Index)(BootAllocated(boot) >> pageShift);

  /* Init allocTable after class init, because it might be mapped there. */
  BTResRange(chunk->allocTable, 0, pages);

  chunk->sig = ChunkSig;
  AVERT(Chunk, chunk);
  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failAllocPageTable:
  (arena->class->chunkFinish)(chunk);
failClassInit:
failAllocTable:
  return res;
}


/* ChunkFinish -- finish the generic fields of a chunk */

void ChunkFinish(Chunk chunk)
{
  AVERT(Chunk, chunk);
  AVER(BTIsResRange(chunk->allocTable, 0, chunk->pages));
  ChunkDecache(chunk->arena, chunk);
  chunk->sig = SigInvalid;
  RingRemove(&chunk->chunkRing);
  /* Finish all other fields before class finish, because they might be */
  /* unmapped there. */
  (chunk->arena->class->chunkFinish)(chunk);
}


/* Chunk Cache
 *
 * Functions for manipulating the chunk cache in the arena.
 */


/* ChunkCacheEntryCheck -- check a chunk cache entry */

Bool ChunkCacheEntryCheck(ChunkCacheEntry entry)
{
  CHECKS(ChunkCacheEntry, entry);
  if (entry->chunk != NULL) {
    CHECKD(Chunk, entry->chunk);
    CHECKL(entry->base == entry->chunk->base);
    CHECKL(entry->limit == entry->chunk->limit);
    CHECKL(entry->pageTableBase == &entry->chunk->pageTable[0]);
    CHECKL(entry->pageTableLimit
           == &entry->chunk->pageTable[entry->chunk->pages]);
  }
  return TRUE;
}


/* ChunkCacheEntryInit -- initialize a chunk cache entry */

void ChunkCacheEntryInit(ChunkCacheEntry entry)
{
  entry->chunk = NULL;
  /* No need to init other fields. */
  entry->sig = ChunkCacheEntrySig;
  return;
}


/* ChunkEncache -- cache a chunk */

void ChunkEncache(Arena arena, Chunk chunk)
{
  AVERT(Arena, arena);
  AVERT(Chunk, chunk);
  AVER(arena == chunk->arena);

  /* check chunk already in cache first */
  if (arena->chunkCache.chunk == chunk) {
    return;
  }

  arena->chunkCache.chunk = chunk;
  arena->chunkCache.base = chunk->base;
  arena->chunkCache.limit = chunk->limit;
  arena->chunkCache.pageTableBase = &chunk->pageTable[0];
  arena->chunkCache.pageTableLimit = &chunk->pageTable[chunk->pages];

  AVERT(ChunkCacheEntry, &arena->chunkCache);
  return;
}


/* ChunkDecache -- make sure a chunk is not in the cache */

static void ChunkDecache(Arena arena, Chunk chunk)
{
  if (arena->chunkCache.chunk == chunk) {
    arena->chunkCache.chunk = NULL;
  }
}


/* ChunkOfAddr -- return the chunk which encloses an address */

Bool ChunkOfAddr(Chunk *chunkReturn, Arena arena, Addr addr)
{
  Ring node, next;

  AVER_CRITICAL(chunkReturn != NULL);
  AVERT_CRITICAL(Arena, arena);
  /* addr is arbitrary */

  /* check cache first */
  if (arena->chunkCache.base <= addr && addr < arena->chunkCache.limit) {
    *chunkReturn = arena->chunkCache.chunk;
    return TRUE;
  }
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    if (chunk->base <= addr && addr < chunk->limit) {
      /* Gotcha! */
      ChunkEncache(arena, chunk);
      *chunkReturn = chunk;
      return TRUE;
    }
  }
  return FALSE;
}


/* ChunkOfNextAddr
 *
 * Finds the next higher chunk in memory which does _not_ contain addr.
 * Returns FALSE if there is none.
 */

static Bool ChunkOfNextAddr(Chunk *chunkReturn, Arena arena, Addr addr)
{
  Addr leastBase;
  Chunk leastChunk;
  Ring node, next;

  leastBase = (Addr)(Word)-1;
  leastChunk = NULL;
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    if (addr < chunk->base && chunk->base < leastBase) {
      leastBase = chunk->base;
      leastChunk = chunk;
    }
  }
  if (leastChunk != NULL) {
    *chunkReturn = leastChunk;
    return TRUE;
  }
  return FALSE;
}


/* ArenaIsReservedAddr -- is address managed by this arena? */

Bool ArenaIsReservedAddr(Arena arena, Addr addr)
{
  Chunk dummy;

  AVERT(Arena, arena);
  /* addr is arbitrary */

  return ChunkOfAddr(&dummy, arena, addr);
}


/* IndexOfAddr -- return the index of the page containing an address
 *
 * Function version of INDEX_OF_ADDR, for debugging purposes.
 */

Index IndexOfAddr(Chunk chunk, Addr addr)
{
  AVERT(Chunk, chunk);
  /* addr is arbitrary */

  return INDEX_OF_ADDR(chunk, addr);
}


/* Page table functions */

/* .tract.critical: These Tract functions are low-level and are on
 * the critical path in various ways.  The more common therefore
 * use AVER_CRITICAL.
 */


/* TractOfAddr -- return the tract the given address is in, if any
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If so, return it.
 */

Bool TractOfAddr(Tract *tractReturn, Arena arena, Addr addr)
{
  Bool b;
  Index i;
  Chunk chunk;
 
  /* <design/trace/#fix.noaver> */
  AVER_CRITICAL(tractReturn != NULL); /* .tract.critical */
  AVERT_CRITICAL(Arena, arena);

  b = ChunkOfAddr(&chunk, arena, addr);
  if (!b)
    return FALSE;
  /* <design/trace/#fix.tractofaddr> */
  i = INDEX_OF_ADDR(chunk, addr);
  /* .addr.free: If the page is recorded as being free then */
  /* either the page is free or it is */
  /* part of the arena tables (see .ullagepages). */
  if (BTGet(chunk->allocTable, i)) {
    Page page = &chunk->pageTable[i];
    *tractReturn = PageTract(page);
    return TRUE;
  }

  return FALSE;
}


/* TractOfBaseAddr -- return a tract given a base address
 *
 * The address must have been allocated to some pool.
 */

Tract TractOfBaseAddr(Arena arena, Addr addr)
{
  Tract tract;
  Bool found;

  AVERT_CRITICAL(Arena, arena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  /* Check first in the cache, see <design/arena/#tract.cache>. */
  if (arena->lastTractBase == addr) {
    tract = arena->lastTract;
  } else {
    found = TractOfAddr(&tract, arena, addr);
    AVER_CRITICAL(found);
  }

  AVER_CRITICAL(TractBase(tract) == addr);
  return tract;
}


/* tractSearchInChunk -- search for a tract
 *
 * .tract-search: Searches for a tract in the chunk starting at page
 * index i, return NULL if there is none.  .tract-search.private: This
 * function is private to this module and is used in the tract iteration
 * protocol (TractFirst and TractNext).
 */

static Bool tractSearchInChunk(Tract *tractReturn, Chunk chunk, Index i)
{
  AVER_CRITICAL(chunk->allocBase <= i);
  AVER_CRITICAL(i <= chunk->pages);

  while(i < chunk->pages
        && !(BTGet(chunk->allocTable, i)
             && PageIsAllocated(&chunk->pageTable[i]))) {
    ++i;
  }
  if (i == chunk->pages)
    return FALSE;
  AVER(i < chunk->pages);
  *tractReturn = PageTract(&chunk->pageTable[i]);
  return TRUE;
}


/* tractSearch
 *
 * Searches for the next tract in increasing address order.
 * The tract returned is the next one along from addr (i.e.,
 * it has a base address bigger than addr and no other tract
 * with a base address bigger than addr has a smaller base address).
 *
 * Returns FALSE if there is no tract to find (end of the arena).
 */

static Bool tractSearch(Tract *tractReturn, Arena arena, Addr addr)
{
  Bool b;
  Chunk chunk;

  b = ChunkOfAddr(&chunk, arena, addr);
  if (b) {
    Index i;

    i = INDEX_OF_ADDR(chunk, addr);
    /* There are fewer pages than addresses, therefore the */
    /* page index can never wrap around */
    AVER_CRITICAL(i+1 != 0);

    if (tractSearchInChunk(tractReturn, chunk, i+1)) {
      return TRUE;
    }
  }
  while (ChunkOfNextAddr(&chunk, arena, addr)) {
    /* If the ring was kept in address order, this could be improved. */
    addr = chunk->base;
    /* Start from allocBase to skip the tables. */
    if (tractSearchInChunk(tractReturn, chunk, chunk->allocBase)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* TractFirst -- return the first tract in the arena
 *
 * This is used to start an iteration over all tracts in the arena, not
 * including the ones used for page tables and other arena structures.
 */

Bool TractFirst(Tract *tractReturn, Arena arena)
{
  AVER(tractReturn != NULL);
  AVERT(Arena, arena);

  /* .tractfirst.assume.nozero: We assume that there is no tract */
  /* with base address (Addr)0.  Happily this assumption is sound */
  /* for a number of reasons. */
  return tractSearch(tractReturn, arena, (Addr)0);
}


/* TractNext -- return the "next" tract in the arena
 *
 * TractNext finds the tract with the lowest base address which is
 * greater than a specified address.  The address must be (or once
 * have been) the base address of a tract.
 *
 * This is used as the iteration step when iterating over all
 * tracts in the arena.
 */

Bool TractNext(Tract *tractReturn, Arena arena, Addr addr)
{
  AVER_CRITICAL(tractReturn != NULL); /* .tract.critical */
  AVERT_CRITICAL(Arena, arena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  return tractSearch(tractReturn, arena, addr);
}


/* PageAlloc
 *
 * Sets up the PageStruct for an allocated page to turn it into a Tract.
 */

void PageAlloc(Chunk chunk, Index pi, Pool pool)
{
  Tract tract;
  Addr base;

  AVERT(Chunk, chunk);
  AVER(pi >= chunk->allocBase);
  AVER(pi < chunk->pages);
  AVER(!BTGet(chunk->allocTable, pi));
  AVERT(Pool, pool);

  tract = PageTract(&chunk->pageTable[pi]);
  base = PageIndexBase(chunk, pi);
  BTSet(chunk->allocTable, pi);
  TractInit(tract, pool, base);
  return;
}


/* PageInit -- initialize a page (as free) */

void PageInit(Chunk chunk, Index pi)
{
  AVERT(Chunk, chunk);
  AVER(pi < chunk->pages);

  BTRes(chunk->allocTable, pi);
  PagePool(&chunk->pageTable[pi]) = NULL;
  PageType(&chunk->pageTable[pi]) = PageTypeFree;
  return;
}


/* PageFree -- free an allocated page */

void PageFree(Chunk chunk, Index pi)
{
  AVERT(Chunk, chunk);
  AVER(pi >= chunk->allocBase);
  AVER(pi < chunk->pages);
  AVER(BTGet(chunk->allocTable, pi));

  PageInit(chunk, pi);
  return;
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
