/* impl.c.arenacl: ARENA IMPLEMENTATION USING CLIENT MEMORY
 *
 * $HopeName: !arenacl.c(trunk.18) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .readership: MM developers
 *
 * .design: See design.mps.arena.client.
 *
 * .improve.remember: One possible performance improvement is to
 * remember (a conservative approximation to) the indices of the first
 * and last free pages in each chunk, and start searching from these
 * in ChunkAlloc. See request.epcore.170534.
 */

#include "mpm.h"
#include "mpsacl.h"


SRCID(arenacl, "$HopeName: !arenacl.c(trunk.18) $");


typedef struct ClientArenaStruct *ClientArena;
typedef struct ChunkStruct *Chunk;
typedef struct PageStruct *Page;


/* ClientArenaStruct -- Client Arena Structure */

#define ClientArenaSig ((Sig)0x519A6EC7) /* SIGnature AREna CLient */

typedef struct ClientArenaStruct {
  ArenaStruct arenaStruct; /* generic arena structure */
  RingStruct chunkRing;    /* all the chunks */
  Serial chunkSerial;      /* next chunk number */
  Shift pageShift;         /* log2(pageSize), for shifts */
  Size pageSize;           /* size of block managed by PageStruct */
  Sig sig;                 /* design.mps.sig */
} ClientArenaStruct;


/* ArenaClientArena -- find the ClientArena given a generic Arena */

#define ArenaClientArena(arena) \
  PARENT(ClientArenaStruct, arenaStruct, (arena))


/* ClientArenaArena -- find the generic Arena given a ClientArena */

#define ClientArenaArena(ClientArena) (&(ClientArena)->arenaStruct)


/* TractClientArena -- find the client arena given a tract */

#define TractClientArena(tract) ArenaClientArena(PoolArena(TractPool(tract)))

/* ChunkStruct -- chunk structure */

#define ChunkSig        ((Sig)0x519C804c) /* SIGnature CHUNK */

typedef struct ChunkStruct { /* chunk structure */
  Sig sig;                   /* impl.h.misc.sig */
  ClientArena arena;         /* the arena */
  RingStruct arenaRing;      /* ring of chunks within the arena */
  Serial serial;             /* serial within the arena */
  Size pages;                /* number of pages in chunk */
  Size freePages;            /* number of free pages in chunk */
  Addr base;                 /* base address of chunk */
  Addr limit;                /* limit address of chunk */
  Addr pageBase;             /* base of first managed page in chunk */
  Page pageTable;            /* the page table */
  BT allocTable;             /* page allocated table */
} ChunkStruct;


/* PageStruct -- page structure
 *
 * The page table is lifted entirely from arenavm. See
 * design.mps.arenavm.table.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union.  See design.mps.arena.tract.field.pool.
 */

typedef struct PageStruct {   /* page structure */
  union {
    TractStruct tractStruct;  /* tract */
    struct {
      Pool pool;              /* NULL, must be first field (.page) */
    } tail;
  } the;
} PageStruct;


static Bool ClientTractNext(Tract *tractReturn, Arena arena, Addr addr);


/* ChunkCheck -- check the consistency of a chunk */

static Bool ChunkCheck(Chunk chunk)
{
  CHECKS(Chunk, chunk);
  CHECKU(ClientArena, chunk->arena);
  CHECKL(RingCheck(&chunk->arenaRing));
  CHECKL(chunk->serial <= chunk->arena->chunkSerial);
  CHECKL(chunk->freePages <= chunk->pages);
  /* check base and limit: */
  CHECKL(chunk->base != (Addr)0);
  CHECKL(chunk->limit != (Addr)0);
  CHECKL(chunk->base < chunk->limit);
  /* check the control structures are not NULL: */
  CHECKL(chunk->pageBase != (Addr)0);
  CHECKL(chunk->pageTable != NULL);
  CHECKL(chunk->allocTable != NULL);
  /* check the control structures are between base and limit */
  /* (allowing for the case in which the chunk manages no pages): */
  CHECKL((Addr)chunk >= chunk->base);
  CHECKL((Addr)chunk < chunk->limit);
  CHECKL(chunk->pageBase > chunk->base);
  CHECKL(chunk->pageBase <= chunk->limit);
  CHECKL((Addr)chunk->pageTable > chunk->base);
  CHECKL((Addr)chunk->pageTable <= chunk->limit);
  CHECKL((Addr)chunk->allocTable > chunk->base);
  CHECKL((Addr)chunk->allocTable <= chunk->limit);
  /* check order of control structures within chunk: */
  CHECKL((Addr)chunk < (Addr)chunk->pageTable);
  CHECKL((Addr)chunk->pageTable <= (Addr)chunk->allocTable);
  CHECKL((Addr)chunk->allocTable <= (Addr)chunk->pageBase);
  /* check size of control structures within chunk: */
        /* enough size for chunk struct: */
  CHECKL(AddrOffset(chunk, chunk->pageTable) >= sizeof(ChunkStruct));
        /* enough space for page table: */
  CHECKL(AddrOffset(chunk->pageTable, chunk->allocTable) /
    sizeof(PageStruct) >= chunk->pages);
        /* enough space for alloc table: */
  CHECKL(AddrOffset(chunk->allocTable, chunk->pageBase) / sizeof(Word)
         >= SizeAlignUp(chunk->pages, MPS_WORD_WIDTH)
            >> MPS_WORD_SHIFT);
        /* enough space for pages: */
  CHECKL((AddrOffset(chunk->pageBase, chunk->limit) >>
         chunk->arena->pageShift)
         == chunk->pages);
  /* .check.tables: could check the consistency of the tables, */
  /* but not O(1) */
  return TRUE;
}


/* would like to be able to write a PageCheck, but Pages don't even
 * have a signature */


/* PageBase -- Page Index to Base address mapping
 *
 * See design.mps.arenavm.table.linear
 */

#define PageBase(chunk, index) \
  AddrAdd((chunk)->pageBase, ((index) << (chunk)->arena->pageShift))

/* PageTract -- tract descriptor of a page */

#define PageTract(page)         (&(page)->the.tractStruct)

/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)

/* PagePool -- pool field of a page */

#define PagePool(page)          ((page)->the.tail.pool)

/* PageOfTract -- page descriptor from arena tract */

#define PageOfTract(tract)      PARENT(PageStruct, the.tractStruct, tract)


/* ChunkPageIndexOfAddr -- base address to page index (within a chunk)
 *                         mapping
 */

static Index ChunkPageIndexOfAddr(Chunk chunk, Addr addr)
{
  AVERT(Chunk, chunk);
  AVER(chunk->pageBase <= addr);
  AVER(addr < AddrAdd(chunk->pageBase,
                      (Size)(chunk->pages << chunk->arena->pageShift)));

  return (Index)(AddrOffset(chunk->pageBase, addr) >>
              chunk->arena->pageShift);
}
 

/* ClientArenaCheck -- check the consistency of a client arena */

static Bool ClientArenaCheck(ClientArena clientArena)
{
  CHECKS(ClientArena, clientArena);
  CHECKD(Arena, ClientArenaArena(clientArena));
  CHECKL(RingCheck(&clientArena->chunkRing));
  /* no possible check on clientArena->chunkSerial */
  CHECKL(ShiftCheck(clientArena->pageShift));
  CHECKL(clientArena->pageSize == 1uL << clientArena->pageShift);
  return TRUE;
}


/* ChunkCreate -- create a chunk */

static Res ChunkCreate(Chunk *chunkReturn, Addr base, Addr limit,
                       ClientArena clientArena)
{
  Chunk chunk;
  Addr a;
  Size tablePages;

  AVERT(ClientArena, clientArena);
  AVER(chunkReturn != NULL);
  AVER(base != (Addr)0);
  AVER(limit != (Addr)0);
  AVER(limit > base);

  /* allocate the chunk */
  a = AddrAlignUp(base, MPS_PF_ALIGN);
  chunk = (Chunk)a;

  /* figure out the sizes and locations of the control structures */
  a = AddrAlignUp(AddrAdd(a, sizeof(ChunkStruct)), MPS_PF_ALIGN);

  if (a > limit) /* the chunk is too small */
    return ResMEMORY;

  tablePages = AddrOffset(a, limit) >> clientArena->pageShift;

  chunk->pageTable = (Page)a;
  a = AddrAlignUp(AddrAdd(a, sizeof(PageStruct) * tablePages),
    MPS_PF_ALIGN);

  chunk->allocTable = (BT)a;
  a = AddrAlignUp(AddrAdd(a, BTSize(tablePages)), ARENA_CLIENT_PAGE_SIZE);

  /* the rest is in managed pages; there may be some wastage at */
  /* the end */
  chunk->pageBase = a;

  /* initialize the remaining slots */
  chunk->base = base;
  chunk->limit = limit;
  chunk->pages = AddrOffset(chunk->pageBase, chunk->limit)
                 >> clientArena->pageShift;
  chunk->freePages = chunk->pages;
  chunk->arena = clientArena;

  BTResRange(chunk->allocTable, 0, tablePages);

  /* link to the arena (in address order) */
  RingInit(&chunk->arenaRing);
  {
    Ring node, nextNode;

    RING_FOR(node, &clientArena->chunkRing, nextNode) {
      Chunk cchunk = RING_ELT(Chunk, arenaRing, node);
      if(chunk->base < cchunk->base) {
        goto found;
      }
    }
  found:
    RingAppend(node, &chunk->arenaRing);
  }

  chunk->serial = clientArena->chunkSerial;
  ++clientArena->chunkSerial;

  /* sign it, check it, return it */
  chunk->sig = ChunkSig;
  AVERT(Chunk, chunk);

  *chunkReturn = chunk;
  return ResOK;
}


/* ClientArenaInit -- create and initialize the client arena
 *
 * .init.memory: Creates the arena structure in the chuck given, and
 * makes the first chunk from the memory left over.
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

static Res ClientArenaInit(Arena *arenaReturn, ArenaClass class,
                           va_list args)
{
  Arena arena;
  ClientArena clientArena;
  Lock lock;
  Size size;
  size_t clArenaSize;   /* aligned size of ClientArenaStruct */
  Addr base, limit, chunkBase;
  Res res;
  Chunk chunk;
 
  size = va_arg(args, Size);
  base = va_arg(args, Addr);
  AVER(arenaReturn != NULL);
  AVER((ArenaClass)mps_arena_class_cl() == class);
  AVER(base != (Addr)0);

  clArenaSize = SizeAlignUp(sizeof(ClientArenaStruct), MPS_PF_ALIGN);
  if (size < (clArenaSize + LockSize()))
    return ResMEMORY;

  limit = AddrAdd(base, size);

  /* allocate the arena */
  base = AddrAlignUp(base, MPS_PF_ALIGN);
  clientArena = (ClientArena)base;
  chunkBase = AddrAlignUp(AddrAdd(base, clArenaSize + LockSize()),
                          MPS_PF_ALIGN);
  if (chunkBase > limit) return ResMEMORY;

  lock = (Lock)PointerAdd(base, clArenaSize);
  arena = ClientArenaArena(clientArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, lock, class);

  clientArena->pageSize = ARENA_CLIENT_PAGE_SIZE;
  clientArena->pageShift = SizeLog2(clientArena->pageSize);
  RingInit(&clientArena->chunkRing);
  clientArena->chunkSerial = (Serial)0;

  /* have to have a valid arena before calling ChunkCreate */
  clientArena->sig = ClientArenaSig;
 
  AVERT(ClientArena, clientArena);

  res = ChunkCreate(&chunk, chunkBase, limit, clientArena);
  if (res) return res;
 
  /* Set the zone shift to divide the initial chunk into the same */
  /* number of zones as will fit into a reference set (the number of */
  /* bits in a word). Note that some zones are discontiguous in the */
  /* arena if the size is not a power of 2. */
  arena->zoneShift = SizeFloorLog2(size >> MPS_WORD_SHIFT);
  arena->alignment = clientArena->pageSize;
 
  EVENT_PWA(ArenaCreateCL, arena, size, base);

  *arenaReturn = arena;
  return ResOK;
}


/* ClientArenaFinish -- finish the arena */

static void ClientArenaFinish(Arena arena)
{
  ClientArena clientArena;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  clientArena->sig = SigInvalid;
  ArenaFinish(arena); /* impl.c.arena.finish.caller */
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
 
  clientArena = ArenaClientArena(arena);
  res = ChunkCreate(&chunk, base, limit, clientArena);
  return res;
}


/* ClientArenaRetract -- retract a chunk from the arena
 *
 * Returns ResFAIL if there is no such chunk, or if it exists but is
 * not fully free.
 */

static Res ClientArenaRetract(Arena arena, Addr base, Size size)
{
  ClientArena clientArena;
  Ring node, nextNode;
  Addr limit;
 
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  limit = AddrAdd(base, size);

  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    AVERT(Chunk, chunk);
    if ((chunk->base == base) && (chunk->limit == limit)) {
      /* check that it's empty */
      if (!BTIsResRange(chunk->allocTable, 0, chunk->pages))
          return ResFAIL;
      return ResOK;
    }
  }
  return ResFAIL;       /* no such chunk */
}


/* ClientArenaReserved -- return the amount of reserved address space
 * ClientArenaCommitted -- return the amount of committed virtual memory
 *
 * (actually for the client arena, ArenaCommitted returns the amount
 * allocated in tracts).
 */

static Size ClientArenaReserved(Arena arena)
{
  ClientArena clientArena;
  Size size;
  Ring node, nextNode;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  size = 0;
  /* .req.extend.slow */
  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    AVERT(Chunk, chunk);
    size += AddrOffset(chunk->base, chunk->limit);
  }

  return size;
}

static Size ClientArenaCommitted(Arena arena)
{
  ClientArena clientArena;
  Size size = 0;
  Ring node, nextNode;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  /* .req.extend.slow */
  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    AVERT(Chunk, chunk);
    size += ((chunk->pages - chunk->freePages) * clientArena->pageSize);
  }

  return size;
}


/* ChunkAlloc: allocate some tracts in a chunk */

static Res ChunkAlloc(Addr *baseReturn, Tract *baseTractReturn,
                      SegPref pref, Size pages, Pool pool, Chunk chunk)
{
  Index baseIndex, limitIndex, index;
  Bool b;
  ClientArena clientArena;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVERT(Chunk, chunk);

  if (pages > chunk->freePages)
    return ResRESOURCE;

  clientArena = chunk->arena;

  if (pref->high)
    b = BTFindShortResRangeHigh(&baseIndex, &limitIndex,
				chunk->allocTable,
				0, chunk->pages,
				pages);
  else
    b = BTFindShortResRange(&baseIndex, &limitIndex,
			    chunk->allocTable,
			    0, chunk->pages,
			    pages);
 
  if (!b)
    return ResRESOURCE;
 
  /* check commit limit, note that if there are multiple reasons */
  /* for failing the allocation we attempt to return other result codes */
  /* in preference to ResCOMMIT_LIMIT.  See design.mps.arena.commit-limit */
  if(ClientArenaCommitted(PoolArena(pool)) + pages*clientArena->pageSize >
     PoolArena(pool)->commitLimit) {
    return ResCOMMIT_LIMIT;
  }

  /* Initialize the generic tract structures. */
  AVER(limitIndex > baseIndex);
  for(index = baseIndex; index < limitIndex; ++index) {
    Tract tract;
    Addr base;
    AVER(!BTGet(chunk->allocTable, index));
    tract = PageTract(&chunk->pageTable[index]);
    base = PageBase(chunk, index);
    BTSet(chunk->allocTable, index);
    TractInit(tract, pool, base);
  }

  chunk->freePages -= pages;
 
  *baseReturn = PageBase(chunk, baseIndex);
  *baseTractReturn = PageTract(&chunk->pageTable[baseIndex]);
   
  return ResOK;
}


/* ClientAlloc -- allocate a region from the arena */

static Res ClientAlloc(Addr *baseReturn, Tract *baseTractReturn,
                       SegPref pref, Size size, Pool pool)
{
  ClientArena clientArena;
  Res res;
  Ring node, nextNode;
  Size pages;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > 0);
  AVERT(Pool, pool);

  clientArena = ArenaClientArena(PoolArena(pool));
  AVERT(ClientArena, clientArena);
  AVER(SizeIsAligned(size, clientArena->pageSize));
  /* NULL is used as a discriminator (see */
  /* design.mps.arenavm.table.disc), therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  pages = size >> clientArena->pageShift;

  /* .req.extend.slow */
  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    res = ChunkAlloc(baseReturn, baseTractReturn, pref, pages, pool, chunk);
    if(res == ResOK || res == ResCOMMIT_LIMIT) {
      return res;
    }
  }
  return ResRESOURCE;
}


/* ClientChunkOfAddr -- return the chunk which encloses an address
 *
 */

static Bool ClientChunkOfAddr(Chunk *chunkReturn,
                              ClientArena clientArena, Addr addr)
{
  Ring node, nextNode;
  /* No checks because critical and internal */

  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      *chunkReturn = chunk;
      return TRUE;
    }
  }
  return FALSE;
}


/* ClientFree - free a region in the arena */

static void ClientFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  Chunk chunk;
  Size pages;
  ClientArena clientArena;
  Index pi, baseIndex, limitIndex;
  Bool foundChunk;

  AVER(base != NULL);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(SizeIsAligned(size, clientArena->pageSize));
  AVER(AddrIsAligned(base, clientArena->pageSize));

  foundChunk = ClientChunkOfAddr(&chunk, clientArena, base);
  AVER(foundChunk);

  pages = size >> clientArena->pageShift;
  baseIndex = AddrOffset(chunk->pageBase, base) >>
                clientArena->pageShift;
  limitIndex = baseIndex + pages;
  AVER(baseIndex < limitIndex);
  AVER(limitIndex <= chunk->pages);

  for(pi = baseIndex; pi < limitIndex; pi++) {
    Page page = &chunk->pageTable[pi];
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);
    TractFinish(PageTract(page));
  }

  AVER(BTIsSetRange(chunk->allocTable, baseIndex, limitIndex));
  BTResRange(chunk->allocTable, baseIndex, limitIndex);

  chunk->freePages += pages;
}


/* .tract.critical: These Tract functions are low-level and are on
 * the critical path in various ways.  The more common therefore
 * use AVER_CRITICAL.
 */


/* ClientTractOfAddr -- return the tract which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If so, return it.
 */

static Bool ClientTractOfAddr(Tract *tractReturn, Arena arena, Addr addr)
{
  Bool b;
  Chunk chunk;
  Index index;
  ClientArena clientArena;
 
  AVER_CRITICAL(tractReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT_CRITICAL(ClientArena, clientArena);

  b = ClientChunkOfAddr(&chunk, clientArena, addr);
  if(!b)
    return FALSE;
  index = AddrOffset(chunk->pageBase, addr) >> clientArena->pageShift;
  if(BTGet(chunk->allocTable, index)) {
    Page page = &chunk->pageTable[index];
    *tractReturn = PageTract(page);
    return TRUE;
  }
  return FALSE;
}

static Bool ClientIsReserved(Arena arena, Addr addr)
{
  ClientArena clientArena;
  Chunk dummy;

  AVERT(Arena, arena);
  /* addr is arbitrary */

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  return ClientChunkOfAddr(&dummy, clientArena, addr);
}


/* tractSearchChunk -- search for a tract in a given chunk
 *
 * .tract-search: Searches for a tract in the chunk starting at page
 * 'index', return NULL if there is none.  A tract is present if its
 * page is not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the tract
 * iteration protocol (TractFirst and TractNext).
 */

static Tract tractSearchChunk(Chunk chunk, Index index)
{
  AVERT_CRITICAL(Chunk, chunk);
  AVER_CRITICAL(index <= chunk->pages);

  while(index < chunk->pages && !BTGet(chunk->allocTable, index)) {
    /* can't check that the pool is NULL because it won't have */
    /* been initialized before the first allocation of the page */
    /* AVER_CRITICAL(PagePool(&chunk->pageTable[index]) == NULL); */
    ++index;
  }
   
  if(index < chunk->pages) {
    Tract tract = PageTract(&chunk->pageTable[index]);
    AVER_CRITICAL(TractPool(tract) != NULL);
    return tract;
  }
 
  AVER_CRITICAL(index == chunk->pages);
  return NULL;
}


/* ClientTractFirst -- return the first tract in the arena
 *
 * This is used to start an iteration over all tracts in the arena.
 */

static Bool ClientTractFirst(Tract *tractReturn, Arena arena)
{
  return ClientTractNext(tractReturn, arena, (Addr)0);
}


/* ClientTractNext -- return the next tract in the arena
 *
 * This is used as the iteration step when iterating over all
 * tracts in the arena.
 */

static Bool ClientTractNext(Tract *tractReturn, Arena arena, Addr addr)
{
  ClientArena clientArena;
  Ring node, nextNode;

  AVER_CRITICAL(tractReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT_CRITICAL(ClientArena, clientArena);
  AVER_CRITICAL(AddrIsAligned(addr, ArenaAlign(arena)));

  /* For each chunk, search for a tract whose base is bigger */
  /* than  addr.  Chunks whose limit is less than addr are not */
  /* considered.  Chunks are on the ring in address order, so */
  /* this finds the first */
  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    Index index;

    if(addr < AddrAdd(chunk->pageBase,
               (Size)(chunk->pages << clientArena->pageShift))) {
      Tract tract;

      if(addr < chunk->pageBase) {
        /* The address is not in this chunk, so we want */
        /* to start looking at the beginning of the chunk. */
        index = 0;
      } else {
        /* The address is in this chunk, so we want to start */
        /* looking just after the page at this address. */
        index = ChunkPageIndexOfAddr(chunk, addr);
        /* There are fewer pages than addresses so the page index will */
        /* not wrap. */
        AVER(index + 1 != 0);
        ++index;
      }
      tract = tractSearchChunk(chunk, index);
      if(tract != NULL) {
        AVER_CRITICAL(addr < TractBase(tract));
        *tractReturn = tract;
        return TRUE;
      }
    }
    /* This chunk didn't have any more tracts, so try the next one. */
  }
  /* If there are no more chunks, then we are done. */
  return FALSE;
}


/* ClientTractNextContig -- return the next contiguous tract
 *
 * This is used as the iteration step when iterating over all
 * a contiguous range of tracts owned by a pool. Both current
 * and next tracts must be allocated.
 */

static Tract ClientTractNextContig(Arena arena, Tract tract)
{
  ClientArena clArena;
  Page page, next;
  Tract tnext;

  clArena = ArenaClientArena(arena);
  AVERT_CRITICAL(ClientArena, clArena);
  AVERT_CRITICAL(Tract, tract);

  /* check both this tract & next tract lie with the same chunk */
  {
    Chunk ch1, ch2;
    UNUSED(ch1);
    UNUSED(ch2);
    AVER_CRITICAL(ClientChunkOfAddr(&ch1, clArena, TractBase(tract)) &&
                  ClientChunkOfAddr(&ch2, clArena,
                                    AddrAdd(TractBase(tract),
                                            arena->alignment)) &&
                  (ch1 == ch2));
  }

  /* the next contiguous tract is contiguous in the page table */
  page = PageOfTract(tract);
  next = page + 1; 
  tnext = PageTract(next);
  AVERT_CRITICAL(Tract, tnext);
  AVER_CRITICAL(PagePool(next) != NULL);
  return tnext;
}


/* ClientArenaClass  -- The Client arena class definition */

DEFINE_ARENA_CLASS(ClientArenaClass, this)
{
  INHERIT_CLASS(this, AbstractArenaClass);
  this->name = "CL";
  this->size = sizeof(ClientArenaStruct);
  this->offset = offsetof(ClientArenaStruct, arenaStruct);
  this->init = ClientArenaInit;
  this->finish = ClientArenaFinish;
  this->reserved = ClientArenaReserved;
  this->committed = ClientArenaCommitted;
  this->extend = ClientArenaExtend;
  this->retract = ClientArenaRetract;
  this->isReserved = ClientIsReserved;
  this->alloc = ClientAlloc;
  this->free = ClientFree;
  this->tractOfAddr = ClientTractOfAddr;
  this->tractFirst = ClientTractFirst;
  this->tractNext = ClientTractNext;
  this->tractNextContig = ClientTractNextContig;
}


/* mps_arena_class_cl -- return the arena class CL */

mps_arena_class_t mps_arena_class_cl(void)
{
  return (mps_arena_class_t)EnsureClientArenaClass();
}
