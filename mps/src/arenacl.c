/* impl.c.arenacl: ARENA IMPLEMENTATION USING CLIENT MEMORY
 *
 * $HopeName: MMsrc!arenacl.c(trunk.7) $
 * 
 * Copyright (C) 1996,1997 Harlequin Group, all rights reserved.
 *
 * .readership: MM developers
 * 
 * .design: See design.mps.arena.client.
 *
 * .req: The requirements are not fully known. They are inherited from
 * req.epcore, and from other requirements on the MPS. They include:
 * 
 * .req.client: Manage memory provided by the client.
 * 
 * .req.client.only: Operate entirely within client-provided memory.
 * 
 * .req.extend: Allow extension by the client providing additional chunks
 *   of memory.
 * 
 * .req.extend.few: Normal operation will be in one chunk; "a few" chunks is
 *   unusual; "many" chunks is very unlikely.
 * 
 * .req.extend.slow: Speed may degrade proportionally to the number of 
 *   chunks of memory. 
 *
 * .req.place: Allow preferential placement of segments.
 * 
 * .improve.twiddle: There are several places in which many bits in a bit
 * table are checked or set in sequence. These could all be made much faster
 * with suitable bit-twiddling code.
 *
 */

#include "mpm.h"
#include "mpsacl.h"


SRCID(arenacl, "$HopeName: MMsrc!arenacl.c(MMdevel_config_thread.3) $");


typedef struct ClientArenaStruct *ClientArena;
typedef struct ChunkStruct *Chunk;
typedef struct PageStruct *Page;
typedef Word *ABT;                      /* bool table type */


/* ClientArenaStruct -- Client Arena Structure */

#define ClientArenaSig      ((Sig)0x519A6EC7) /* SIGnature AREna CLient */

typedef struct ClientArenaStruct {
  ArenaStruct arenaStruct;	/* generic arena structure */
  RingStruct chunkRing;         /* all the chunks */
  Serial chunkSerial;           /* next chunk number */
  Shift pageShift;              /* log2(pageSize), for shifts */
  Size pageSize;                /* size of block managed by PageStruct */
  Sig sig;                      /* design.mps.sig */
} ClientArenaStruct;


/* ArenaClientArena -- find the ClientArena pointer given a generic Arena */

#define ArenaClientArena(arena) PARENT(ClientArenaStruct, arenaStruct, (arena))


/* ClientArenaArena -- find the generic Arena pointer given a ClientArena */

#define ClientArenaArena(ClientArena) (&(ClientArena)->arenaStruct)


/* ChunkStruct -- chunk structure */

#define ChunkSig        ((Sig)0x519C804c) /* SIGnature CHUNK */

typedef struct ChunkStruct {    /* chunk structure */
  Sig sig;                      /* impl.h.misc.sig */
  ClientArena arena;            /* the arena */
  RingStruct arenaRing;         /* ring of chunks within the arena */
  Serial serial;                /* serial within the arena */
  Size pages;                   /* number of pages in chunk */
  Size freePages;               /* number of free pages in chunk */
  Addr base;                    /* base address of chunk */
  Addr limit;                   /* limit address of chunk */
  Addr pageBase;                /* base of first managed page in chunk */
  Page pageTable;               /* the page table */
  ABT freeTable;                /* page free table */
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

typedef struct PageStruct {      /* page structure */
  union {
    SegStruct segStruct;          /* segment */
    struct {
      Pool pool;                  /* NULL, must be first field (.page) */
      Seg seg;                    /* segment at base page of run */
      Addr limit;                 /* limit of segment */
    } tail;
  } the;
} PageStruct;


static Addr ClientSegLimit(Arena arena, Seg seg);
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
  CHECKL(chunk->freeTable != NULL);
  /* check the control structures are between base and limit */
  /* (allowing for the case in which the chunk manages no pages): */
  CHECKL((Addr)chunk >= chunk->base);
  CHECKL((Addr)chunk < chunk->limit);
  CHECKL(chunk->pageBase > chunk->base);
  CHECKL(chunk->pageBase <= chunk->limit);
  CHECKL((Addr)chunk->pageTable > chunk->base);
  CHECKL((Addr)chunk->pageTable <= chunk->limit);
  CHECKL((Addr)chunk->freeTable > chunk->base);
  CHECKL((Addr)chunk->freeTable <= chunk->limit);
  /* check order of control structures within chunk: */
  CHECKL((Addr)chunk < (Addr)chunk->pageTable);
  CHECKL((Addr)chunk->pageTable <= (Addr)chunk->freeTable);
  CHECKL((Addr)chunk->freeTable <= (Addr)chunk->pageBase);
  /* check size of control structures within chunk: */
        /* enough size for chunk struct: */
  CHECKL(AddrOffset(chunk, chunk->pageTable) >= sizeof(ChunkStruct));
        /* enough space for page table: */
  CHECKL(AddrOffset(chunk->pageTable, chunk->freeTable) / sizeof(PageStruct)
         >= chunk->pages);
        /* enough space for free table: */
  CHECKL(AddrOffset(chunk->freeTable, chunk->pageBase) / sizeof(Word)
         >= SizeAlignUp(chunk->pages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT);
        /* enough space for pages: */
  CHECKL((AddrOffset(chunk->pageBase, chunk->limit) >> chunk->arena->pageShift)
         == chunk->pages);
  /* .check.tables: could check the consistency of the tables, but not O(1) */
  return TRUE;
}


/* would like to be able to write a PageCheck, but Pages don't even
 * have a signature */


/* PageBase -- Page Index to Base address mapping
 *
 * See design.mps.arenavm.table.linear
 */

#define PageBase(chunk, pi) \
  AddrAdd((chunk)->pageBase, ((pi) << (chunk)->arena->pageShift))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)

/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)

/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, seg)


/* Index Types
 * 
 * PI is the type of a value used to index into a page table.
 * 
 * BI is the type of a value used to index into a bool table (See ABTGet
 * and ABTSet in this module).
 */

typedef Size PI;
typedef Size BI;


/* ChunkPageIndexOfAddr -- base address to page index (within a chunk) mapping */

static PI ChunkPageIndexOfAddr(Chunk chunk, Addr addr)
{
  AVERT(Chunk, chunk);
  AVER(chunk->pageBase <= addr);
  AVER(addr < AddrAdd(chunk->pageBase,
                      (Size)(chunk->pages << chunk->arena->pageShift)));

  return (PI)(AddrOffset(chunk->pageBase, addr) >>
	      chunk->arena->pageShift);
}
  

/* ABTGet -- get a bool from a bool table
 *
 * Note: The function version of ABTGet isn't used anywhere in
 * this source, but is left here in case we want to revert from
 * the macro.
 */

#if 0
static Bool (ABTGet)(ABT bt, BI i)
{
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  return (bt[wi] >> bi) & 1;
}
#endif /* 0 */

#define ABTGet(bt, i) \
  (((bt)[(i)>>MPS_WORD_SHIFT] >> ((i)&(MPS_WORD_WIDTH-1))) & 1)


/* ABTSet -- set a bool in a bool table */

static void ABTSet(ABT bt, BI i, Bool b)
{
  Size bi = i & (MPS_WORD_WIDTH - 1);       /* bit index */
  Word mask = ~((Word)1 << bi);
  Size wi = i >> MPS_WORD_SHIFT;            /* word index */
  bt[wi] = (bt[wi] & mask) | ((Word)b << bi);
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
  Size freeTableWords;
  PI i;

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
  a = AddrAlignUp(AddrAdd(a, sizeof(PageStruct) * tablePages), MPS_PF_ALIGN);

  chunk->freeTable = (ABT)a;
  freeTableWords = SizeAlignUp(tablePages, MPS_WORD_WIDTH) >> MPS_WORD_SHIFT;
  a = AddrAlignUp(AddrAdd(a, freeTableWords * sizeof(Word)),
		  ARENA_CLIENT_PAGE_SIZE);

  /* the rest is in managed pages; there may be some wastage at the end */
  chunk->pageBase = a;

  /* initialize the remaining slots */
  chunk->base = base;
  chunk->limit = limit;
  chunk->pages = AddrOffset(chunk->pageBase, chunk->limit)
                 >> clientArena->pageShift;
  chunk->freePages = chunk->pages;
  chunk->arena = clientArena;

  /* initialize the freeTable */
  /* .improve.twiddle.init: Could go a lot faster with bit twiddling */
  for(i = 0; i < tablePages; ++i)
    ABTSet(chunk->freeTable, i, TRUE);

  /* link to the arena (in address order) */
  RingInit(&chunk->arenaRing);
  {
    Ring node;

    node = RingNext(&clientArena->chunkRing);
    RING_FOR(node, &clientArena->chunkRing) {
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

static Res ClientArenaInit(Arena *arenaReturn, va_list args)
{
  Arena arena;
  ClientArena clientArena;
  Size size;
  Addr base, limit, chunkBase;
  Res res;
  Chunk chunk;
  
  size = va_arg(args, Size);
  base = va_arg(args, Addr);
  AVER(arenaReturn != NULL);
  AVER(base != (Addr)0);

  if (size < sizeof(ClientArenaStruct)) return ResMEMORY;

  limit = AddrAdd(base, size);

  /* allocate the arena */
  base = AddrAlignUp(base, MPS_PF_ALIGN);
  clientArena = (ClientArena)base;
  chunkBase = AddrAlignUp(AddrAdd(base, sizeof(ClientArenaStruct)),
			  MPS_PF_ALIGN);
  if (chunkBase > limit) return ResMEMORY;

  arena = ClientArenaArena(clientArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, (ArenaClass)mps_arena_class_cl());

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
  Ring node;
  Addr limit;
  
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  limit = AddrAdd(base, size);

  RING_FOR(node, &clientArena->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    AVERT(Chunk, chunk);
    if ((chunk->base == base) && (chunk->limit == limit)) {
      /* check that it's empty */
      PI pi;
      for (pi = 0; pi < chunk->pages; pi++) {
        if (ABTGet(chunk->freeTable, pi) == FALSE)
          return ResFAIL;
      }
      return ResOK;
    }
  }
  return ResFAIL;       /* no such chunk */
}


/* ClientArenaReserved -- return the amount of reserved address space
 * ClientArenaCommitted -- return the amount of committed virtual memory
 * 
 * (actually for the client arena, ArenaCommitted returns the amount allocated
 *  in segments).
 */

static Size ClientArenaReserved(Arena arena)
{
  ClientArena clientArena;
  Size size;
  Ring node;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  size = 0;
  RING_FOR(node, &clientArena->chunkRing) { /* .req.extend.slow */
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
  Ring node;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  RING_FOR(node, &clientArena->chunkRing) { /* .req.extend.slow */
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    AVERT(Chunk, chunk);
    size += ((chunk->pages - chunk->freePages) * clientArena->pageSize);
  }

  return size;
}


/* ChunkSegAlloc: allocate a segment in a chunk */

static Res ChunkSegAlloc(Seg *segReturn, SegPref pref, Size pages, Pool pool,
                         Chunk chunk)
{
  PI pi, count, base = 0;
  Seg seg;
  ClientArena clientArena;

  AVER(segReturn != NULL);
  AVERT(Chunk, chunk);

  if (pages > chunk->freePages)
    return ResRESOURCE;

  clientArena = chunk->arena;
  
  /* Search the free table for a sufficiently-long run of free pages. */
  /* If we succeed, we go to "found:" with the lowest page number in */
  /* the run in 'base'. */
  /* .improve.twiddle.search: This code could go a lot faster with */
  /* twiddling the bit table. */
  /* .improve.clear: I have tried to make this code clear, with */
  /* comments &c, but there's room for further clarification. */

  count = 0; /* the number of free pages found in the current run */
  if (pref->high) { /* search down from the top of the chunk */
    pi = chunk->pages;
    while (pi != 0) {
      pi--;
      if (ABTGet(chunk->freeTable, pi)) {
        ++count;
        if (count == pages) { /* then we're done, take the base of this run */
          base = pi;
          goto found;
        }
      } else
        count = 0;
    }
  } else { /* search up from the bottom of the chunk */
    pi = 0;
    while (pi != chunk->pages) {
      if(ABTGet(chunk->freeTable, pi)) {
        if(count == 0)
          base = pi; /* remember the base of this run */
        ++count;
        if(count == pages) /* now we're done */
          goto found;
      } else
        count = 0;
      pi++;
    }
  }
  
  /* No adequate run was found. */
  /* .improve.alloc-fail: This could be because the request was */
  /* too large, or perhaps because of fragmentation.  We could return a */
  /* more meaningful code. */
  return ResRESOURCE;

found:
  /* Initialize the generic segment structure. */
  seg = PageSeg(&chunk->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(ABTGet(chunk->freeTable, base));
  ABTSet(chunk->freeTable, base, FALSE);
  if(pages > 1) {
    Addr limit = PageBase(chunk, base + pages);
    SegSetSingle(seg, FALSE);
    for(pi = base + 1; pi < base + pages; ++pi) {
      AVER(ABTGet(chunk->freeTable, pi));
      ABTSet(chunk->freeTable, pi, FALSE);
      PageTail(&chunk->pageTable[pi])->pool = NULL;
      PageTail(&chunk->pageTable[pi])->seg = seg;
      PageTail(&chunk->pageTable[pi])->limit = limit;
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

static Res ClientSegAlloc(Seg *segReturn, SegPref pref, Arena arena,
			Size size, Pool pool)
{
  ClientArena clientArena;
  Res res;
  Ring node;
  Size pages;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(SizeIsAligned(size, clientArena->pageSize));
  AVER(size > 0);
  AVERT(Pool, pool);
  /* NULL is used as a discriminator (see design.mps.arenavm.table.disc), */
  /* therefore the real pool must be non-NULL. */
  AVER(pool != NULL);

  pages = size >> clientArena->pageShift;

  RING_FOR(node, &clientArena->chunkRing) { /* .req.extend.slow */
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    res = ChunkSegAlloc(segReturn, pref, pages, pool, chunk);
    if (res == ResOK)
      return res;
  }
  return ResRESOURCE;
}


/* SegChunk: identify the chunk (and index) in which a segment resides */

static Res SegChunk(Chunk *chunkReturn, PI *piReturn, Seg seg,
		    ClientArena clientArena)
{
  Page page;
  Ring node;
  
  AVER(chunkReturn != NULL);
  AVERT(Seg, seg);
  AVERT(ClientArena, clientArena);

  page = PageOfSeg(seg);

  RING_FOR(node, &clientArena->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if ((page >= chunk->pageTable) &&
        (page < (chunk->pageTable + chunk->pages))) {
      *piReturn = page - chunk->pageTable;
      *chunkReturn = chunk;
      return ResOK;
    }
  }
  return ResFAIL;
}


/* ClientSegFree - free a segment in the arena */

static void ClientSegFree(Arena arena, Seg seg)
{
  ClientArena clientArena;
  Chunk chunk;
  PI pi, pl, pn;
  Addr base, limit; 
  Res res;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVERT(Seg, seg);

  res = SegChunk(&chunk, &pi, seg, clientArena);
  AVER(res == ResOK);

  limit = ClientSegLimit(arena, seg);

  SegFinish(seg);

  /* Remember the base address of the segment so it can be */
  /* unmapped .free.unmap */
  base = PageBase(chunk, pi);

  /* Calculate the number of pages in the segment, and hence the */
  /* limit for .free.loop */
  pn = AddrOffset(base, limit) >> clientArena->pageShift;
  pl = pi + pn;
  /* .free.loop: */
  for( ; pi < pl; ++pi) {
    AVER(ABTGet(chunk->freeTable, pi) == FALSE);
    ABTSet(chunk->freeTable, pi, TRUE);
  }

  chunk->freePages += pn;

  /* Double check that .free.loop takes us to the limit page of the */
  /* segment. */
  AVER(PageBase(chunk, pi) == limit);
}


/* ClientSegBase -- return the base address of a segment
 *
 * The segment base is calculated by identifying the chunk and page
 * index, then multiplying that by the page size and adding it to
 * the chunk base address.
 */

static Addr ClientSegBase(Arena arena, Seg seg)
{
  ClientArena clientArena;
  PI pi;
  Chunk chunk;
  Res res;
  
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVERT(Seg, seg);

  res = SegChunk(&chunk, &pi, seg, clientArena);
  AVER(res == ResOK);

  return PageBase(chunk, pi);
}


/* ClientSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr ClientSegLimit(Arena arena, Seg seg)
{
  ClientArena clientArena;
  Page page;

  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVERT(Seg, seg);

  if(SegSingle(seg))
    return AddrAdd(ClientSegBase(arena, seg), clientArena->pageSize);
  else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* ClientSegSize -- return the size (limit - base) of a segment
 *
 * .improve.redundant-calc: There is scope for optimizing this,
 * because both base and limit calls do roughly the same thing twice.
 */

static Size ClientSegSize(Arena arena, Seg seg)
{
  AVERT(ClientArena, ArenaClientArena(arena));
  AVERT(Seg, seg);
  return AddrOffset(ClientSegBase(arena, seg), ClientSegLimit(arena, seg));
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
  Ring node;
  
  AVER(segReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);

  RING_FOR(node, &clientArena->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    if(chunk->base <= addr && addr < chunk->limit) {
      PI pi = AddrOffset(chunk->pageBase, addr) >> clientArena->pageShift;
      if(!ABTGet(chunk->freeTable, pi)) {
        Page page = &chunk->pageTable[pi];
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


/* SegSearchChunk -- search for a segment in a given chunk
 *
 * Searches for a segment in the chunk starting at page index pi,
 * return NULL if there is none.  A segment is present if it is
 * not free, and its pool is not NULL.
 *
 * This function is private to this module and is used in the segment
 * iteration protocol (SegFirst and SegNext).
 */
static Seg SegSearchChunk(Chunk chunk, PI pi)
{
  AVERT(Chunk, chunk);
  AVER(pi <= chunk->pages);

  while(pi < chunk->pages &&
        (ABTGet(chunk->freeTable, pi) ||
         SegPool(PageSeg(&chunk->pageTable[pi])) == NULL))
    ++pi;
  
  if(pi < chunk->pages)
    return PageSeg(&chunk->pageTable[pi]);
  
  AVER(pi == chunk->pages);
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
  Ring node;
  ClientArena clientArena;

  AVER(segReturn != NULL);
  clientArena = ArenaClientArena(arena);
  AVERT(ClientArena, clientArena);
  AVER(AddrIsAligned(addr, ArenaAlign(arena)));

  /* For each chunk, search for a segment whose base is bigger than */
  /* addr.  Chunks whose limit is less than addr are not considered. */
  /* Chunks are on the ring in address order, so this finds the first */
  /* segment whose base is bigger than addr. */
  RING_FOR(node, &clientArena->chunkRing) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    PI pi;

    if(addr < AddrAdd(chunk->pageBase,
		      (Size)(chunk->pages << clientArena->pageShift))) {
      Seg seg;

      if(addr < chunk->pageBase) {
	/* The address is not in this chunk, so we want */
	/* to start looking at the beginning of the chunk. */
	pi = 0;
      } else {
	/* The address is in this chunk, so we want to start */
	/* looking just after the page at this address. */
	pi = ChunkPageIndexOfAddr(chunk, addr);
	/* There are fewer pages than addresses so the page index will */
	/* not wrap. */
	AVER(pi + 1 != 0);
	++pi;
      }
      seg = SegSearchChunk(chunk, pi);
      if(seg != NULL) {
	AVER(addr < ClientSegBase(arena, seg));
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
  ClientArenaExtend,                        /* extend */
  ClientArenaRetract,                       /* retract */
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
