/* arenacl.c: ARENA CLASS USING CLIENT MEMORY
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/arena/#client>.
 *
 * .improve.remember: One possible performance improvement is to
 * remember (a conservative approximation to) the indices of the first
 * and last free pages in each chunk, and start searching from these
 * in ChunkAlloc.  See request.epcore.170534_.
 *
 * .. _request.epcore.170534: https://info.ravenbrook.com/project/mps/import/2001-11-05/mmprevol/request/epcore/170534
 */

#include "boot.h"
#include "tract.h"
#include "bt.h"
#include "mpm.h"
#include "mpsacl.h"

SRCID(arenacl, "$Id$");


/* ClientArenaStruct -- Client Arena Structure */

#define ClientArenaSig ((Sig)0x519A6EC7) /* SIGnature AREna CLient */

typedef struct ClientArenaStruct {
  ArenaStruct arenaStruct; /* generic arena structure */
  Sig sig;                 /* <design/sig/> */
} ClientArenaStruct;
typedef struct ClientArenaStruct *ClientArena;

#define Arena2ClientArena(arena) PARENT(ClientArenaStruct, arenaStruct, arena)
#define ClientArena2Arena(clArena) (&(clArena)->arenaStruct)


/* CLChunk -- chunk structure */

typedef struct ClientChunkStruct *ClientChunk;

#define ClientChunkSig ((Sig)0x519A6C2C) /* SIGnature ARena CLient Chunk */

typedef struct ClientChunkStruct {
  ChunkStruct chunkStruct;     /* generic chunk */
  Size freePages;              /* number of free pages in chunk */
  Addr pageBase;               /* base of first managed page in chunk */
  Sig sig;                     /* <design/sig/> */
} ClientChunkStruct;

#define ClientChunk2Chunk(clchunk) (&(clchunk)->chunkStruct)
#define Chunk2ClientChunk(chunk) PARENT(ClientChunkStruct, chunkStruct, chunk)


/* ClientChunkClientArena -- get the client arena from a client chunk */

#define ClientChunkClientArena(clchunk) \
  Arena2ClientArena(ChunkArena(ClientChunk2Chunk(clchunk)))


/* ClientChunkCheck -- check the consistency of a client chunk */

static Bool ClientChunkCheck(ClientChunk clChunk)
{
  Chunk chunk;

  CHECKS(ClientChunk, clChunk);
  chunk = ClientChunk2Chunk(clChunk);
  CHECKL(ChunkCheck(chunk));
  CHECKL(clChunk->freePages <= chunk->pages);
  /* check they don't overlap (knowing the order) */
  CHECKL((Addr)(chunk + 1) < (Addr)chunk->allocTable);
  return TRUE;
}
 

/* ClientArenaCheck -- check the consistency of a client arena */

static Bool ClientArenaCheck(ClientArena clientArena)
{
  CHECKS(ClientArena, clientArena);
  CHECKD(Arena, ClientArena2Arena(clientArena));
  return TRUE;
}


/* clientChunkCreate -- create a ClientChunk */

static Res clientChunkCreate(Chunk *chunkReturn, Addr base, Addr limit,
                             ClientArena clientArena)
{
  ClientChunk clChunk;
  Chunk chunk;
  Addr alignedBase;
  BootBlockStruct bootStruct;
  BootBlock boot = &bootStruct;
  Res res;
  void *p;

  AVER(chunkReturn != NULL);
  AVER(base != (Addr)0);
  /* @@@@ Should refuse on small chunks, instead of AVERring. */
  AVER(limit != (Addr)0);
  AVER(limit > base);

  /* Initialize boot block. */
  /* Chunk has to be page-aligned, and the boot allocs must be within it. */
  alignedBase = AddrAlignUp(base, ARENA_CLIENT_PAGE_SIZE);
  AVER(alignedBase < limit);
  res = BootBlockInit(boot, (void *)alignedBase, (void *)limit);
  if (res != ResOK)
    goto failBootInit;

  /* Allocate the chunk. */
  /* See <design/arena/>.@@@@ */
  res = BootAlloc(&p, boot, sizeof(ClientChunkStruct), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failChunkAlloc;
  clChunk = p;  chunk = ClientChunk2Chunk(clChunk);

  res = ChunkInit(chunk, ClientArena2Arena(clientArena),
                  alignedBase, AddrAlignDown(limit, ARENA_CLIENT_PAGE_SIZE),
                  ARENA_CLIENT_PAGE_SIZE, boot);
  if (res != ResOK)
    goto failChunkInit;

  ClientArena2Arena(clientArena)->committed +=
    AddrOffset(base, PageIndexBase(chunk, chunk->allocBase));
  BootBlockFinish(boot);

  clChunk->sig = ClientChunkSig;
  AVERT(ClientChunk, clChunk);
  *chunkReturn = chunk;
  return ResOK;

failChunkInit:
failChunkAlloc:
failBootInit:
  return res;
}


/* ClientChunkInit -- initialize a ClientChunk */

static Res ClientChunkInit(Chunk chunk, BootBlock boot)
{
  ClientChunk clChunk;

  /* chunk is supposed to be uninitialized, so don't check it. */
  clChunk = Chunk2ClientChunk(chunk);
  AVERT(BootBlock, boot);
  UNUSED(boot);

  clChunk->freePages = chunk->pages; /* too large @@@@ */

  return ResOK;
}


/* clientChunkDestroy -- destroy a ClientChunk */

static void clientChunkDestroy(Chunk chunk)
{
  ClientChunk clChunk;

  clChunk = Chunk2ClientChunk(chunk);
  AVERT(ClientChunk, clChunk);

  clChunk->sig = SigInvalid;
  ChunkFinish(chunk);
}


/* ClientChunkFinish -- finish a ClientChunk */

static void ClientChunkFinish(Chunk chunk)
{
  /* Can't check chunk as it's not valid anymore. */
  UNUSED(chunk); NOOP;
}


/* ClientArenaVarargs -- parse obsolete varargs */

static void ClientArenaVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_ARENA_SIZE;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_ARENA_CL_BASE;
  args[1].val.addr = va_arg(varargs, Addr);
  args[2].key = MPS_KEY_ARGS_END;
  AVER(ArgListCheck(args));
}


/* ClientArenaInit -- create and initialize the client arena
 *
 * .init.memory: Creates the arena structure in the chuck given, and
 * makes the first chunk from the memory left over.
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

ARG_DEFINE_KEY(arena_cl_addr, Addr);

static Res ClientArenaInit(Arena *arenaReturn, ArenaClass class, ArgList args)
{
  Arena arena;
  ClientArena clientArena;
  Size size;
  Size clArenaSize;   /* aligned size of ClientArenaStruct */
  Addr base, limit, chunkBase;
  Res res;
  Chunk chunk;
  mps_arg_s arg;
  
  AVER(arenaReturn != NULL);
  AVER((ArenaClass)mps_arena_class_cl() == class);
  AVER(ArgListCheck(args));
  
  ArgRequire(&arg, args, MPS_KEY_ARENA_SIZE);
  size = arg.val.size;
  ArgRequire(&arg, args, MPS_KEY_ARENA_CL_BASE);
  base = arg.val.addr;

  AVER(base != (Addr)0);

  clArenaSize = SizeAlignUp(sizeof(ClientArenaStruct), MPS_PF_ALIGN);
  if (size < clArenaSize)
    return ResMEMORY;

  limit = AddrAdd(base, size);

  /* allocate the arena */
  base = AddrAlignUp(base, MPS_PF_ALIGN);
  clientArena = (ClientArena)base;
  chunkBase = AddrAlignUp(AddrAdd(base, clArenaSize), MPS_PF_ALIGN);
  if (chunkBase > limit)
    return ResMEMORY;

  arena = ClientArena2Arena(clientArena);
  /* <code/arena.c#init.caller> */
  res = ArenaInit(arena, class, ARENA_CLIENT_PAGE_SIZE);
  if (res != ResOK)
    return res;

  /* have to have a valid arena before calling ChunkCreate */
  clientArena->sig = ClientArenaSig;

  res = clientChunkCreate(&chunk, chunkBase, limit, clientArena);
  if (res != ResOK)
    goto failChunkCreate;
  arena->primary = chunk;

  /* Set the zone shift to divide the initial chunk into the same */
  /* number of zones as will fit into a reference set (the number of */
  /* bits in a word). Note that some zones are discontiguous in the */
  /* arena if the size is not a power of 2. */
  arena->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);
  AVER(arena->alignment == ChunkPageSize(arena->primary));

  EVENT3(ArenaCreateCL, arena, size, base);
  AVERT(ClientArena, clientArena);
  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
  ArenaFinish(arena);
  AVER(res != ResOK);
  return res;
}


/* ClientArenaFinish -- finish the arena */

static void ClientArenaFinish(Arena arena)
{
  ClientArena clientArena;
  Ring node, next;

  clientArena = Arena2ClientArena(arena);
  AVERT(ClientArena, clientArena);

  /* destroy all chunks */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    clientChunkDestroy(chunk);
  }

  clientArena->sig = SigInvalid;

  ArenaFinish(arena); /* <code/arena.c#finish.caller> */
}


/* ClientArenaExtend -- extend the arena */

static Res ClientArenaExtend(Arena arena, Addr base, Size size)
{
  ClientArena clientArena;
  Chunk chunk;
  Res res;
  Addr limit;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);
  limit = AddrAdd(base, size);
 
  clientArena = Arena2ClientArena(arena);
  res = clientChunkCreate(&chunk, base, limit, clientArena);
  return res;
}


/* ClientArenaReserved -- return the amount of reserved address space */

static Size ClientArenaReserved(Arena arena)
{
  Size size;
  Ring node, nextNode;

  AVERT(Arena, arena);

  size = 0;
  /* .req.extend.slow */
  RING_FOR(node, &arena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    AVERT(Chunk, chunk);
    size += AddrOffset(chunk->base, chunk->limit);
  }

  return size;
}


/* chunkAlloc -- allocate some tracts in a chunk */

static Res chunkAlloc(Addr *baseReturn, Tract *baseTractReturn,
                      SegPref pref, Size pages, Pool pool, Chunk chunk)
{
  Index baseIndex, limitIndex, indx;
  Bool b;
  Arena arena;
  ClientChunk clChunk;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  clChunk = Chunk2ClientChunk(chunk);

  if (pages > clChunk->freePages)
    return ResRESOURCE;

  arena = chunk->arena;

  if (pref->high)
    b = BTFindShortResRangeHigh(&baseIndex, &limitIndex, chunk->allocTable,
                                chunk->allocBase, chunk->pages, pages);
  else
    b = BTFindShortResRange(&baseIndex, &limitIndex, chunk->allocTable,
                            chunk->allocBase, chunk->pages, pages);

  if (!b)
    return ResRESOURCE;

  /* Check commit limit.  Note that if there are multiple reasons */
  /* for failing the allocation we attempt to return other result codes */
  /* in preference to ResCOMMIT_LIMIT.  See <design/arena/#commit-limit> */
  if (ArenaCommitted(arena) + pages * ChunkPageSize(chunk)
      > arena->commitLimit) {
    return ResCOMMIT_LIMIT;
  }

  /* Initialize the generic tract structures. */
  AVER(limitIndex > baseIndex);
  for(indx = baseIndex; indx < limitIndex; ++indx) {
    PageAlloc(chunk, indx, pool);
  }

  clChunk->freePages -= pages;

  *baseReturn = PageIndexBase(chunk, baseIndex);
  *baseTractReturn = PageTract(&chunk->pageTable[baseIndex]);

  return ResOK;
}


/* ClientAlloc -- allocate a region from the arena */

static Res ClientAlloc(Addr *baseReturn, Tract *baseTractReturn,
                       SegPref pref, Size size, Pool pool)
{
  Arena arena;
  Res res;
  Ring node, nextNode;
  Size pages;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > 0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
  /* NULL is used as a discriminator (see */
  /* <design/arenavm/#table.disc>), therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  pages = ChunkSizeToPages(arena->primary, size);

  /* .req.extend.slow */
  RING_FOR(node, &arena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    res = chunkAlloc(baseReturn, baseTractReturn, pref, pages, pool, chunk);
    if (res == ResOK || res == ResCOMMIT_LIMIT) {
      return res;
    }
  }
  return ResRESOURCE;
}


/* ClientFree - free a region in the arena */

static void ClientFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  Chunk chunk = NULL;           /* suppress "may be used uninitialized" */
  Size pages;
  ClientArena clientArena;
  Index pi, baseIndex, limitIndex;
  Bool foundChunk;
  ClientChunk clChunk;

  AVER(base != NULL);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  clientArena = Arena2ClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
  AVER(AddrIsAligned(base, ChunkPageSize(arena->primary)));

  foundChunk = ChunkOfAddr(&chunk, arena, base);
  AVER(foundChunk);
  clChunk = Chunk2ClientChunk(chunk);
  AVERT(ClientChunk, clChunk);

  pages = ChunkSizeToPages(chunk, size);
  baseIndex = INDEX_OF_ADDR(chunk, base);
  limitIndex = baseIndex + pages;
  AVER(baseIndex < limitIndex);
  AVER(limitIndex <= chunk->pages);

  for(pi = baseIndex; pi < limitIndex; pi++) {
    Page page = &chunk->pageTable[pi];
    Tract tract = PageTract(page);

    AVER(TractPool(tract) == pool);
    TractFinish(tract);
  }

  AVER(BTIsSetRange(chunk->allocTable, baseIndex, limitIndex));
  BTResRange(chunk->allocTable, baseIndex, limitIndex);

  clChunk->freePages += pages;
}


/* ClientArenaClass  -- The Client arena class definition */

DEFINE_ARENA_CLASS(ClientArenaClass, this)
{
  INHERIT_CLASS(this, AbstractArenaClass);
  this->name = "CL";
  this->size = sizeof(ClientArenaStruct);
  this->offset = offsetof(ClientArenaStruct, arenaStruct);
  this->varargs = ClientArenaVarargs;
  this->init = ClientArenaInit;
  this->finish = ClientArenaFinish;
  this->reserved = ClientArenaReserved;
  this->extend = ClientArenaExtend;
  this->alloc = ClientAlloc;
  this->free = ClientFree;
  this->chunkInit = ClientChunkInit;
  this->chunkFinish = ClientChunkFinish;
}


/* mps_arena_class_cl -- return the arena class CL */

mps_arena_class_t mps_arena_class_cl(void)
{
  return (mps_arena_class_t)EnsureClientArenaClass();
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
