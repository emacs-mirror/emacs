/* impl.c.arenacl: ARENA IMPLEMENTATION USING CLIENT MEMORY
 *
 * $HopeName: MMsrc!arenacl.c(trunk.15) $
 * Copyright (C) 1997. Harlequin Group plc. All rights reserved.
 *
 * .readership: MM developers
 * 
 * .design: See design.mps.arena.client.
 * 
 * .improve.remember: One possible performance improvement is to
 * remember (a conservative approximation to) the indices of the first
 * and last free pages in each chunk, and start searching from these
 * in ChunkSegAlloc. See request.epcore.170534.
 */

#include "mpm.h"
#include "mpsacl.h"


SRCID(arenacl, "$HopeName: MMsrc!arenacl.c(trunk.15) $");


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


/* SegClientArena -- find the client arena given a segment */

#define SegClientArena(seg) ArenaClientArena(PoolArena(SegPool(seg)))

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
 * field of this union, so that it shares a common prefix with the
 * SegStruct.  See impl.h.mpmst.seg.pool.
 */

typedef struct PageStruct {   /* page structure */
  union {
    SegStruct segStruct;      /* segment */
    struct {
      Pool pool;              /* NULL, must be first field (.page) */
      Seg seg;                /* segment at base page of run */
      Addr limit;             /* limit of segment */
    } tail;
  } the;
} PageStruct;


static Addr ClientSegLimit(Seg seg);
static Bool ClientSegNext(Seg *segReturn, Arena arena, Addr addr);


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

/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)

/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)

/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, seg)


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
 * allocated in segments).
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


/* ChunkSegAlloc: allocate a segment in a chunk */

static Res ChunkSegAlloc(Seg *segReturn, SegPref pref, Size pages, 
                         Pool pool, Chunk chunk)
{
  Index baseIndex, limitIndex, index;
  Bool b;
  Seg seg;
  ClientArena clientArena;

  AVER(segReturn != NULL);
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

  /* Initialize the generic segment structure. */
  seg = PageSeg(&chunk->pageTable[baseIndex]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page */
  /* information in the page table. */
  AVER(!BTGet(chunk->allocTable, baseIndex));
  BTSet(chunk->allocTable, baseIndex);
  if(pages > 1) {
    Addr limit = PageBase(chunk, limitIndex);
    SegSetSingle(seg, FALSE);
    for(index = baseIndex + 1; index < limitIndex; ++index) {
      AVER(!BTGet(chunk->allocTable, index));
      BTSet(chunk->allocTable, index);
      PageTail(&chunk->pageTable[index])->pool = NULL;
      PageTail(&chunk->pageTable[index])->seg = seg;
      PageTail(&chunk->pageTable[index])->limit = limit;
    }
  } else {
    SegSetSingle(seg, TRUE);
  }
  chunk->freePages -= pages;
  
  AVERT(Seg, seg);

  *segReturn = seg;
  return ResOK;
}


/* ClientSegAlloc -- allocate a segment from the arena */

static Res ClientSegAlloc(Seg *segReturn, SegPref pref,
                          Size size, Pool pool)
{
  ClientArena clientArena;
  Res res;
  Ring node, nextNode;
  Size pages;

  AVER(segReturn != NULL);
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
    res = ChunkSegAlloc(segReturn, pref, pages, pool, chunk);
    if(res == ResOK || res == ResCOMMIT_LIMIT) {
      return res;
    }
  }
  return ResRESOURCE;
}


/* SegChunk -- identify the chunk (and index) in which a segment 
 *             resides 
 */

static Res SegChunk(Chunk *chunkReturn, Index *indexReturn, Seg seg)
{
  Page page;
  Ring node, nextNode;
  ClientArena clientArena;
  
  AVER(chunkReturn != NULL);
  AVERT(Seg, seg);

  clientArena = SegClientArena(seg);
  AVERT(ClientArena, clientArena);

  page = PageOfSeg(seg);

  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if ((page >= chunk->pageTable) &&
        (page < (chunk->pageTable + chunk->pages))) {
      *indexReturn = page - chunk->pageTable;
      *chunkReturn = chunk;
      return ResOK;
    }
  }
  return ResFAIL;
}


/* ClientSegFree - free a segment in the arena */

static void ClientSegFree(Seg seg)
{
  ClientArena clientArena;
  Chunk chunk;
  Index baseIndex, limitIndex;
  Size pages;
  Addr base, limit; 
  Res res;

  AVERT(Seg, seg);

  clientArena = SegClientArena(seg);
  AVERT(ClientArena, clientArena);
  res = SegChunk(&chunk, &baseIndex, seg);
  AVER(res == ResOK);

  base = PageBase(chunk, baseIndex);
  limit = ClientSegLimit(seg);

  SegFinish(seg);

  pages = AddrOffset(base, limit) >> clientArena->pageShift;
  limitIndex = baseIndex + pages;
  AVER(BTIsSetRange(chunk->allocTable, baseIndex, limitIndex));
  BTResRange(chunk->allocTable, baseIndex, limitIndex);

  chunk->freePages += pages;
}


/* .seg.critical: These Seg functions are low-level and are on 
 * the critical path in various ways.  The more common therefore 
 * use AVER_CRITICAL.
 */


/* ClientSegBase -- return the base address of a segment
 *
 * The segment base is calculated by identifying the chunk and page
 * index, then multiplying that by the page size and adding it to
 * the chunk base address.
 */

static Addr ClientSegBase(Seg seg)
{
  ClientArena clientArena;
  Index index;
  Chunk chunk;
  Res res;
  
  AVERT_CRITICAL(Seg, seg);
  clientArena = SegClientArena(seg);
  AVERT_CRITICAL(ClientArena, clientArena);

  res = SegChunk(&chunk, &index, seg);
  AVER(res == ResOK);

  return PageBase(chunk, index);
}


/* ClientSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr ClientSegLimit(Seg seg)
{
  ClientArena clientArena;
  Page page;

  AVERT_CRITICAL(Seg, seg);
  clientArena = SegClientArena(seg);
  AVERT_CRITICAL(ClientArena, clientArena);

  if(SegSingle(seg)) {
    return AddrAdd(ClientSegBase(seg), clientArena->pageSize);
  } else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* ClientSegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls do roughly the same thing twice.
 */

static Size ClientSegSize(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return AddrOffset(ClientSegBase(seg), ClientSegLimit(seg));
}


/* ClientSegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool ClientSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  ClientArena clientArena;
  Ring node, nextNode;
  
  AVER_CRITICAL(segReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT_CRITICAL(ClientArena, clientArena);

  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      Index index = AddrOffset(chunk->pageBase, addr) >> 
              clientArena->pageShift;
      if(BTGet(chunk->allocTable, index)) {
        Page page = &chunk->pageTable[index];
        if(SegPool(PageSeg(page)) != NULL)
          *segReturn = PageSeg(page);
        else
          *segReturn = PageTail(page)->seg;
        return TRUE;
      }
    }
  }
  return FALSE;
}

static Bool ClientIsReserved(Arena arena, Addr addr)
{
  ClientArena clientArena;
  Ring node, nextNode;

  AVERT(Arena, arena);
  /* addr is arbitrary */

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      return TRUE;
    }
  }
  return FALSE;
}


/* SegSearchChunk -- search for a segment in a given chunk
 *
 * Searches for a segment in the chunk starting at page 'index',
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg SegSearchChunk(Chunk chunk, Index index)
{
  AVERT_CRITICAL(Chunk, chunk);
  AVER_CRITICAL(index <= chunk->pages);

  while(index < chunk->pages &&
        (!BTGet(chunk->allocTable, index) ||
         SegPool(PageSeg(&chunk->pageTable[index])) == NULL))
    ++index;
  
  if(index < chunk->pages)
    return PageSeg(&chunk->pageTable[index]);
  
  AVER_CRITICAL(index == chunk->pages);
  return NULL;
}


/* ClientSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Bool ClientSegFirst(Seg *segReturn, Arena arena)
{
  return ClientSegNext(segReturn, arena, (Addr)0);
}


/* ClientSegNext -- return the next segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 */

static Bool ClientSegNext(Seg *segReturn, Arena arena, Addr addr)
{
  ClientArena clientArena;
  Ring node, nextNode;

  AVER_CRITICAL(segReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT_CRITICAL(ClientArena, clientArena);
  AVER_CRITICAL(AddrIsAligned(addr, ArenaAlign(arena)));

  /* For each chunk, search for a segment whose base is bigger */
  /* than  addr.  Chunks whose limit is less than addr are not */
  /* considered.  Chunks are on the ring in address order, so */
  /* this finds the first */
  RING_FOR(node, &clientArena->chunkRing, nextNode) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    Index index;

    if(addr < AddrAdd(chunk->pageBase,
               (Size)(chunk->pages << clientArena->pageShift))) {
      Seg seg;

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
      seg = SegSearchChunk(chunk, index);
      if(seg != NULL) {
        AVER_CRITICAL(addr < ClientSegBase(seg));
        *segReturn = seg;
        return TRUE;
      }
    }
    /* This chunk didn't have any more segs, so try the next one. */
  }
  /* If there are no more chunks, then we are done. */
  return FALSE;
}


/* mps_arena_class_cl -- return the arena class CL */

static ArenaClassStruct ArenaClassCLStruct = {
  ArenaClassSig,
  "CL",                                     /* name */
  sizeof(ClientArenaStruct),                /* size */
  offsetof(ClientArenaStruct, arenaStruct), /* offset */
  ClientArenaInit,                          /* init */
  ClientArenaFinish,                        /* finish */
  ClientArenaReserved,                      /* reserved */
  ClientArenaCommitted,                     /* committed */
  ArenaNoSpareCommitExceeded,
  ClientArenaExtend,                        /* extend */
  ClientArenaRetract,                       /* retract */
  ClientIsReserved,                         /* isReserved */
  ClientSegAlloc,                           /* segAlloc */
  ClientSegFree,                            /* segFree */
  ClientSegBase,                            /* segBase */
  ClientSegLimit,                           /* segLimit */
  ClientSegSize,                            /* segSize */
  ClientSegOfAddr,                          /* segOfAddr */
  ClientSegFirst,                           /* segFirst */
  ClientSegNext,                            /* segNext */
  ArenaTrivDescribe,                        /* describe */
  ArenaClassSig
};

mps_arena_class_t mps_arena_class_cl(void)
{
  return (mps_arena_class_t)&ArenaClassCLStruct;
}
