/* tract.c: PAGE TABLES
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .ullagepages: Pages whose page index is < allocBase are recorded as
 * free but never allocated as alloc starts searching after the tables.
 * TractOfAddr uses the fact that these pages are marked as free in order
 * to detect "references" to these pages as being bogus.
 *
 * .chunk.at.base: The chunks are stored in a balanced binary tree.
 * Looking up an address in this tree is on the critical path, and
 * therefore vital that it runs quickly. It is an implementation
 * detail of chunks that they are always stored at the base of the
 * region of address space they represent. Thus chunk happens to
 * always be the same as chunk->base. We take advantage of this in the
 * tree search by using chunk as its own key (instead of looking up
 * chunk->base): this saves a dereference and perhaps a cache miss.
 * See ChunkKey and ChunkCompare for this optimization. The necessary
 * property is asserted in ChunkCheck.
 */

#include "tract.h"
#include "boot.h"
#include "bt.h"
#include "mpm.h"

SRCID(tract, "$Id$");


/* TractArena -- get the arena of a tract */

#define TractArena(tract) PoolArena(TractPool(tract))


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

  tract->pool.pool = pool;
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
  tract->pool.pool = NULL;
}



/* .tract.critical: These tract functions are low-level and used
 * throughout. They are therefore on the
 * [critical path](../design/critical-path.txt) and their
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
  /* Can't use CHECKD_NOSIG because TreeEMPTY is NULL. */
  CHECKL(TreeCheck(&chunk->chunkTree));
  CHECKL(ChunkPagesToSize(chunk, 1) == ChunkPageSize(chunk));
  CHECKL(ShiftCheck(ChunkPageShift(chunk)));

  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check chunk structure is at its own base: see .chunk.at.base. */
  CHECKL(chunk->base == (Addr)chunk);
  CHECKL((Addr)(chunk+1) <= chunk->limit);
  CHECKL(ChunkSizeToPages(chunk, ChunkSize(chunk)) == chunk->pages);
  /* check that the tables fit in the chunk */
  CHECKL(chunk->allocBase <= chunk->pages);
  CHECKL(chunk->allocBase >= chunk->pageTablePages);

  CHECKD_NOSIG(BT, chunk->allocTable);
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
  CHECKL(NONNEGATIVE(INDEX_OF_ADDR(chunk, (Addr)chunk->pageTable)));
  /* check there's enough space in the page table */
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

  chunk->pageSize = pageSize;
  chunk->pageShift = pageShift = SizeLog2(pageSize);
  chunk->base = base;
  chunk->limit = limit;
  size = ChunkSize(chunk);

  chunk->pages = pages = size >> pageShift;
  res = BootAlloc(&p, boot, (size_t)BTSize(pages), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failAllocTable;
  chunk->allocTable = p;

  pageTableSize = SizeAlignUp(pages * sizeof(PageUnion), pageSize);
  chunk->pageTablePages = pageTableSize >> pageShift;

  res = (arena->class->chunkInit)(chunk, boot);
  if (res != ResOK)
    goto failClassInit;

  /* @@@@ Is BootAllocated always right? */
  /* Last thing we BootAlloc'd is pageTable.  We requested pageSize */
  /* alignment, and pageTableSize is itself pageSize aligned, so */
  /* BootAllocated should also be pageSize aligned. */
  AVER(AddrIsAligned(BootAllocated(boot), pageSize));
  chunk->allocBase = (Index)(BootAllocated(boot) >> pageShift);

  /* Init allocTable after class init, because it might be mapped there. */
  BTResRange(chunk->allocTable, 0, pages);

  /* Add the chunk's free address space to the arena's freeCBS, so that
     we can allocate from it. */
  if (arena->hasFreeCBS) {
    res = ArenaFreeCBSInsert(arena,
                             PageIndexBase(chunk, chunk->allocBase),
                             chunk->limit);
    if (res != ResOK)
      goto failCBSInsert;
  }

  TreeInit(&chunk->chunkTree);

  chunk->sig = ChunkSig;
  AVERT(Chunk, chunk);

  ArenaChunkInsert(arena, &chunk->chunkTree);

  /* As part of the bootstrap, the first created chunk becomes the primary
     chunk.  This step allows AreaFreeCBSInsert to allocate pages. */
  if (arena->primary == NULL)
    arena->primary = chunk;

  return ResOK;

failCBSInsert:
  (arena->class->chunkFinish)(chunk);
  /* .no-clean: No clean-ups needed past this point for boot, as we will
     discard the chunk. */
failClassInit:
failAllocTable:
  return res;
}


/* ChunkFinish -- finish the generic fields of a chunk */

void ChunkFinish(Chunk chunk)
{
  Arena arena;

  AVERT(Chunk, chunk);

  AVER(BTIsResRange(chunk->allocTable, 0, chunk->pages));
  arena = ChunkArena(chunk);

  if (arena->hasFreeCBS)
    ArenaFreeCBSDelete(arena,
                       PageIndexBase(chunk, chunk->allocBase),
                       chunk->limit);

  chunk->sig = SigInvalid;

  TreeFinish(&chunk->chunkTree);

  if (chunk->arena->primary == chunk)
    chunk->arena->primary = NULL;

  /* Finish all other fields before class finish, because they might be */
  /* unmapped there. */
  (chunk->arena->class->chunkFinish)(chunk);
}


/* ChunkCompare -- Compare key to [base,limit) */

Compare ChunkCompare(Tree tree, TreeKey key)
{
  Addr base1, base2, limit2;
  Chunk chunk;

  AVERT_CRITICAL(Tree, tree);
  AVER_CRITICAL(tree != TreeEMPTY);

  /* See .chunk.at.base. */
  chunk = ChunkOfTree(tree);
  AVERT_CRITICAL(Chunk, chunk);

  base1 = AddrOfTreeKey(key);
  base2 = chunk->base;
  limit2 = chunk->limit;

  if (base1 < base2)
    return CompareLESS;
  else if (base1 >= limit2)
    return CompareGREATER;
  else
    return CompareEQUAL;
}


/* ChunkKey -- Return the key corresponding to a chunk */

TreeKey ChunkKey(Tree tree)
{
  /* See .chunk.at.base. */
  Chunk chunk = ChunkOfTree(tree);
  return TreeKeyOfAddrVar(chunk);
}


/* ChunkOfAddr -- return the chunk which encloses an address */

Bool ChunkOfAddr(Chunk *chunkReturn, Arena arena, Addr addr)
{
  Tree tree;

  AVER_CRITICAL(chunkReturn != NULL);
  AVERT_CRITICAL(Arena, arena);
  /* addr is arbitrary */

  if (TreeFind(&tree, ArenaChunkTree(arena), TreeKeyOfAddrVar(addr),
               ChunkCompare)
      == CompareEQUAL)
  {
    Chunk chunk = ChunkOfTree(tree);
    AVER_CRITICAL(chunk->base <= addr && addr < chunk->limit);
    *chunkReturn = chunk;
    return TRUE;
  }
  return FALSE;
}


/* chunkAboveAddr
 *
 * Finds the next higher chunk in memory which does _not_ contain
 * addr. If there is such a chunk, update *chunkReturn and return
 * TRUE, otherwise return FALSE.
 */

static Bool chunkAboveAddr(Chunk *chunkReturn, Arena arena, Addr addr)
{
  Tree tree;
  Chunk chunk;

  AVER_CRITICAL(chunkReturn != NULL);
  AVERT_CRITICAL(Arena, arena);
  /* addr is arbitrary */

  if (TreeFindNext(&tree, ArenaChunkTree(arena), TreeKeyOfAddrVar(addr),
                   ChunkCompare))
  {
    chunk = ChunkOfTree(tree);
    AVER_CRITICAL(addr < chunk->base);
    *chunkReturn = chunk;
    return TRUE;
  }
  return FALSE;
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


/* ChunkNodeDescribe -- describe a single node in the tree of chunks,
 * for SplayTreeDescribe
 */

Res ChunkNodeDescribe(Tree node, mps_lib_FILE *stream)
{
  Chunk chunk;

  if (!TreeCheck(node)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  chunk = ChunkOfTree(node);
  if (!TESTT(Chunk, chunk)) return ResFAIL;

  return WriteF(stream, "[$P,$P)", (WriteFP)chunk->base,
                (WriteFP)chunk->limit, NULL);
}


/* Page table functions */

/* .tract.critical: These Tract functions are low-level and are on
 * the [critical path](../design/critical-path.txt) in various ways.  The
 * more common therefore use AVER_CRITICAL.
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
    *tractReturn = PageTract(ChunkPage(chunk, i));
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
  Tract tract = NULL;
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

  while (i < chunk->pages
         && !(BTGet(chunk->allocTable, i)
              && PageIsAllocated(ChunkPage(chunk, i)))) {
    ++i;
  }
  if (i == chunk->pages)
    return FALSE;
  AVER(i < chunk->pages);
  *tractReturn = PageTract(ChunkPage(chunk, i));
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
  while (chunkAboveAddr(&chunk, arena, addr)) {
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
 * Sets up the page descriptor for an allocated page to turn it into a Tract.
 */

void PageAlloc(Chunk chunk, Index pi, Pool pool)
{
  Tract tract;
  Addr base;
  Page page;

  AVERT(Chunk, chunk);
  AVER(pi >= chunk->allocBase);
  AVER(pi < chunk->pages);
  AVER(!BTGet(chunk->allocTable, pi));
  AVERT(Pool, pool);

  page = ChunkPage(chunk, pi);
  tract = PageTract(page);
  base = PageIndexBase(chunk, pi);
  BTSet(chunk->allocTable, pi);
  TractInit(tract, pool, base);
}


/* PageInit -- initialize a page (as free) */

void PageInit(Chunk chunk, Index pi)
{
  Page page;

  AVERT(Chunk, chunk);
  AVER(pi < chunk->pages);
  
  page = ChunkPage(chunk, pi);

  BTRes(chunk->allocTable, pi);
  PageSetPool(page, NULL);
  PageSetType(page, PageStateFREE);
  RingInit(PageSpareRing(page));
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
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
