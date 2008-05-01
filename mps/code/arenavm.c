/* arenavm.c: VIRTUAL MEMORY ARENA CLASS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * .design: See <design/arenavm/>, and <design/arena/#coop-vm>
 *
 * .vm.addr-is-star: In this file, Addr is compatible with C
 * pointers, and Count with size_t (Index), because all refer to the
 * virtual address space.
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC.  (But it would change how pagesFindFreeInZones avoids allocating
 * over the tables, see .alloc.skip.)
 */

#include "boot.h"
#include "tract.h"
#include "bt.h"
#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$Id$");

/* @@@@ Arbitrary calculations (these should be in config.h) */
/* ArenaGangCount = the maximum number of distinct gangs (ie. */
/*  distinct object sets, be they generations or some other group. */
/* VMArenaGenCount = the maximum number of distinct generations. */
enum {
  VMArenaGenCount = 3 /* @@@@ was MPS_WORD_WIDTH/2 @@@@@@@@@@@@@@@ */,
  GangIndexDefault = VMArenaGenCount + 0,
  GangIndexAway,
  ArenaGangCount
};

typedef struct GangStruct *Gang;

typedef struct GangsetStruct *Gangset;

extern void ArenaTractsInZones(Count *tractsUsed, Count *tractsFree,
  Arena arena, ZoneSet zones);

extern void SegPrefZonesOpen(Arena arena, SegPref pref);
extern Bool SegPrefZonesNext(ZoneSet *zonesReturn, Arena arena, SegPref pref, 
  Bool pregrow);
extern void SegPrefZonesClose(Arena arena, SegPref pref, Bool allocated, 
  Addr base, Addr limit);

extern Bool GangCheck(Gang gang);
extern Res GangDescribe(Gang gang, mps_lib_FILE *stream);
extern Res GangFullDescribe(Gang gang, Gang gangNew, ZoneSet zonesNew,
  Arena arenaForUsage, mps_lib_FILE *stream);

extern Bool GangsetCheck(Gangset gangset);
extern Res GangsetDescribe(Gangset gangset, mps_lib_FILE *stream);
extern Res GangsetFullDescribe(Gangset gangset, Gang gangNew, ZoneSet zonesNew,
  Arena arenaForUsage, mps_lib_FILE *stream);
static void GangsetInit(Arena arena, Gangset gangset);

enum {
  GANGNAMELEN = 4
};

#define GangSig ((Sig)0x5199A499) /* SIGnature GANG */

typedef struct GangStruct {
  Index index;                  /* index in gangset */
  char name[GANGNAMELEN];
  ZoneSet preferred;
  ZoneSet in;
  ZoneSet claimed;
  ZoneSet unique;
  Bool hasCollected;
  Bool hasUncollected;
  Sig sig;                      /* <design/sig/> */
} GangStruct;


/* Gangset -- set of gangs all trying to keep out of each others' way
 *
 * A gangset keeps track of the space being used by different gangs of 
 * objects, with the aim of keeping each gang where it wants to be, 
 * and away from other gangs.
 */

#define GangsetSig ((Sig)0x5199A495) /* SIGnature GANGSet */

typedef struct GangsetStruct {
  GangStruct gangs[ArenaGangCount];
  Count gangCount;
  Count gangGenCount;
  Index gangIndexDefault;  /* index of the default gang */
  Index gangIndexAway;     /* index of the away gang */
  ZoneSet blacklist;       /* zones that are bad for GC segs */
  ZoneSet unpreferred;     /* zones not preferred by any gang */
  ZoneSet freezones;       /* unassigned zones */
  Bool trying;             /* is a search for zones in progress? */
  Index tryingGangIndex;
  ZoneSet tryingZones;
  char *tryingWhat;
  Sig sig;                 /* <design/sig/> */
} GangsetStruct;


/* VMChunk -- chunks for VM arenas */

typedef struct VMChunkStruct *VMChunk;

#define VMChunkSig ((Sig)0x519A6B3C) /* SIGnature ARena VM Chunk */

typedef struct VMChunkStruct {
  ChunkStruct chunkStruct;      /* generic chunk */
  VM vm;                        /* virtual memory handle */
  Addr overheadMappedLimit;           /* limit of pages mapped for overhead */
  BT pageTableMapped;           /* indicates mapped state of page table */
  BT noSparePages;             /* 1 bit per page of pageTable */
  Sig sig;                      /* <design/sig/> */
} VMChunkStruct;

#define VMChunk2Chunk(vmchunk) (&(vmchunk)->chunkStruct)
#define Chunk2VMChunk(chunk) PARENT(VMChunkStruct, chunkStruct, chunk)


/* VMChunkVMArena -- get the VM arena from a VM chunk */

#define VMChunkVMArena(vmchunk) \
  Arena2VMArena(ChunkArena(VMChunk2Chunk(vmchunk)))


/* VMArena
 *
 * See <design/arena/#coop-vm.struct.vmarena> for description.
 */

typedef struct VMArenaStruct *VMArena;

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;
  VM vm;                        /* VM where the arena itself is stored */
  Size spareSize;              /* total size of spare pages */
  GangsetStruct gangset;
  ZoneSet blacklist;             /* zones to use last */
  ZoneSet uncolZoneSet;          /* ! pref->isCollected */
  ZoneSet nogenZoneSet;
  ZoneSet genZoneSet[VMArenaGenCount]; /* .gencount.const */
  ZoneSet freeSet;               /* unassigned zones */
  Size extendBy;                /* desired arena increment */
  Size extendMin;               /* minimum arena increment */
  Sig sig;                      /* <design/sig/> */
} VMArenaStruct;

#define Arena2VMArena(arena) PARENT(VMArenaStruct, arenaStruct, arena)
#define VMArena2Arena(vmarena) (&(vmarena)->arenaStruct)


/* Forward declarations */

static void sparePagesPurge(VMArena vmArena);
static ArenaClass VMArenaClassGet(void);
static ArenaClass VMNZArenaClassGet(void);


/* VMChunkCheck -- check the consistency of a VM chunk */

static Bool VMChunkCheck(VMChunk vmchunk)
{
  Chunk chunk;

  CHECKS(VMChunk, vmchunk);
  chunk = VMChunk2Chunk(vmchunk);
  CHECKL(ChunkCheck(chunk));
  CHECKL(VMCheck(vmchunk->vm));
  CHECKL(VMAlign(vmchunk->vm) == ChunkPageSize(chunk));
  CHECKL(vmchunk->overheadMappedLimit <= (Addr)chunk->pageTable);
  /* check pageTableMapped table */
  CHECKL(vmchunk->pageTableMapped != NULL);
  CHECKL((Addr)vmchunk->pageTableMapped >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->pageTableMapped, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadMappedLimit);
  /* check noSparePages table */
  CHECKL(vmchunk->noSparePages != NULL);
  CHECKL((Addr)vmchunk->noSparePages >= chunk->base);
  CHECKL(AddrAdd((Addr)vmchunk->noSparePages, BTSize(chunk->pageTablePages))
         <= vmchunk->overheadMappedLimit);
  /* .improve.check-table: Could check the consistency of the tables. */
  return TRUE;
}


/* addrOfPageDesc -- address of the page descriptor (as an Addr) */

#define addrOfPageDesc(chunk, index) \
  ((Addr)&(chunk)->pageTable[index])


/* PageTablePageIndex
 *
 * Maps from a page base address for a page occupied by the page table
 * to the index of that page in the range of pages occupied by the
 * page table.  So that
 *   PageTablePageIndex(chunk, (Addr)chunk->pageTable) == 0
 * and
 *   PageTablePageIndex(chunk,
 *                      AddrAlignUp(addrOfPageDesc(chunk->pages), pageSize)
 *     == chunk->pageTablePages
 */
#define PageTablePageIndex(chunk, pageAddr) \
  ChunkSizeToPages(chunk, AddrOffset((Addr)(chunk)->pageTable, pageAddr))


/* TablePageIndexBase
 *
 * Takes a page table page index (i.e., the index of a page occupied
 * by the page table, where the page occupied by chunk->pageTable is
 * index 0) and returns the base address of that page.
 * (Reverse of mapping defined by PageTablePageIndex.)
 */
#define TablePageIndexBase(chunk, index) \
  AddrAdd((Addr)(chunk)->pageTable, ChunkPagesToSize(chunk, index))


/* pageIsSpare -- is page spare (free and mapped)? */

#define pageIsSpare(page) \
  ((page)->the.rest.pool == NULL && (page)->the.rest.type == PageTypeSpare)


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  Index gen;
  ZoneSet allocSet;
  Arena arena;
  VMChunk primary;

  CHECKS(VMArena, vmArena);
  arena = VMArena2Arena(vmArena);
  CHECKD(Arena, arena);
  /* spare pages are committed, so must be less spare than committed. */
  CHECKL(vmArena->spareSize <= arena->committed);
  CHECKL(vmArena->blacklist != ZoneSetUNIV);
  
  CHECKD(Gangset, &vmArena->gangset);

  allocSet = ZoneSetEMPTY;
  allocSet = ZoneSetUnion(allocSet, vmArena->uncolZoneSet);
  allocSet = ZoneSetUnion(allocSet, vmArena->nogenZoneSet);
  for(gen = (Index)0; gen < VMArenaGenCount; ++gen) {
    allocSet = ZoneSetUnion(allocSet, vmArena->genZoneSet[gen]);
  }
  CHECKL(ZoneSetInter(allocSet, vmArena->freeSet) == ZoneSetEMPTY);
  CHECKL(vmArena->extendBy > 0);
  CHECKL(vmArena->extendMin <= vmArena->extendBy);

  if (arena->primary != NULL) {
    primary = Chunk2VMChunk(arena->primary);
    CHECKD(VMChunk, primary);
    /* We could iterate over all chunks accumulating an accurate */
    /* count of committed, but we don't have all day. */
    CHECKL(VMMapped(primary->vm) <= arena->committed);
  }
  return TRUE;
}


/* VMArenaDescribe -- describe the VMArena
 */
static Res VMArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  VMArena vmArena;

  if (!CHECKT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  vmArena = Arena2VMArena(arena);
  if (!CHECKT(VMArena, vmArena)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  /* ...but the next method is ArenaTrivDescribe, so don't call it;
   * see impl.c.arena#describe.triv.dont-upcall.
   *
  super = ARENA_SUPERCLASS(VMArenaClass);
  res = super->describe(arena, stream);
  if (res != ResOK) return res;
   *
  */

#if 0
{
  Index gen;
  res = WriteF(stream,
               "   uncolZoneSet: $B\n",
               (WriteFB)vmArena->uncolZoneSet,
               "   nogenZoneSet: $B\n",
               (WriteFB)vmArena->nogenZoneSet,
               NULL);
  if(res != ResOK)
    return res;
  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    if(vmArena->genZoneSet[gen] != ZoneSetEMPTY) {
      res = WriteF(stream,
                   "  genZoneSet[$U]: $B\n",
                   (WriteFU)gen, (WriteFB)vmArena->genZoneSet[gen],
                   NULL);
      if(res != ResOK)
        return res;
    }
  }
  
  res = WriteF(stream,
               "  freeSet:       $B\n", (WriteFB)vmArena->freeSet,
               "  blacklist:     $B\n", (WriteFB)vmArena->blacklist,
               NULL);
  if(res != ResOK)
    return res;
}
#endif

  res = GangsetFullDescribe(&vmArena->gangset, NULL, ZoneSetEMPTY, 
                            arena, stream);
  if(res != ResOK)
    return res;
  
  /* (incomplete: some fields are not Described) */

  return ResOK;
}



/* VM indirect functions
 *
 * These functions should be used to map and unmap within the arena.
 * They are responsible for maintaining vmArena->committed, and for
 * checking that the commit limit does not get exceeded.
 */
static Res vmArenaMap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Arena arena;
  Size size;
  Res res;

  /* no checking as function is local to module */

  arena = VMArena2Arena(vmArena);
  size = AddrOffset(base, limit);
  /* committed can't overflow (since we can't commit more memory than */
  /* address space), but we're paranoid. */
  AVER(arena->committed < arena->committed + size);
  /* check against commit limit */
  if (arena->commitLimit < arena->committed + size)
    return ResCOMMIT_LIMIT;

  res = VMMap(vm, base, limit);
  if (res != ResOK)
    return res;
  arena->committed += size;
  return ResOK;
}


static void vmArenaUnmap(VMArena vmArena, VM vm, Addr base, Addr limit)
{
  Arena arena;
  Size size;

  /* no checking as function is local to module */

  arena = VMArena2Arena(vmArena);
  size = AddrOffset(base, limit);
  AVER(size <= arena->committed);

  VMUnmap(vm, base, limit);
  arena->committed -= size;
  return;
}


/* VMChunkCreate -- create a chunk
 *
 * chunkReturn, return parameter for the created chunk.
 * vmArena, the parent VMArena.
 * size, approximate amount of virtual address that the chunk should reserve.
 */
static Res VMChunkCreate(Chunk *chunkReturn, VMArena vmArena, Size size)
{
  Res res;
  Addr base, limit, chunkStructLimit;
  Align pageSize;
  VM vm;
  BootBlockStruct bootStruct;
  BootBlock boot = &bootStruct;
  VMChunk vmChunk;
  void *p;

  AVER(chunkReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);

  res = VMCreate(&vm, size);
  if (res != ResOK)
    goto failVMCreate;

  pageSize = VMAlign(vm);
  /* The VM will have aligned the userSize; pick up the actual size. */
  base = VMBase(vm);
  limit = VMLimit(vm);

  res = BootBlockInit(boot, (void *)base, (void *)limit);
  if (res != ResOK)
    goto failBootInit;

  /* Allocate and map the descriptor. */
  /* See <design/arena/>.@@@@ */
  res = BootAlloc(&p, boot, sizeof(VMChunkStruct), MPS_PF_ALIGN);
  if (res != ResOK)
    goto failChunkAlloc;
  vmChunk = p;
  /* Calculate the limit of the page where the chunkStruct resides. */
  chunkStructLimit = AddrAlignUp((Addr)(vmChunk + 1), pageSize);
  res = vmArenaMap(vmArena, vm, base, chunkStructLimit);
  if (res != ResOK)
    goto failChunkMap;
  vmChunk->overheadMappedLimit = chunkStructLimit;

  vmChunk->vm = vm;
  res = ChunkInit(VMChunk2Chunk(vmChunk), VMArena2Arena(vmArena),
                  base, limit, pageSize, boot);
  if (res != ResOK)
    goto failChunkInit;

  BootBlockFinish(boot);

  vmChunk->sig = VMChunkSig;
  AVERT(VMChunk, vmChunk);
  *chunkReturn = VMChunk2Chunk(vmChunk);
  return ResOK;

failChunkInit:
  /* No need to unmap, as we're destroying the VM. */
failChunkMap:
failChunkAlloc:
failBootInit:
  VMDestroy(vm);
failVMCreate:
  return res;
}


/* VMChunkInit -- initialize a VMChunk */

static Res VMChunkInit(Chunk chunk, BootBlock boot)
{
  size_t btSize;
  VMChunk vmChunk;
  Addr overheadLimit;
  void *p;
  Res res;

  /* chunk is supposed to be uninitialized, so don't check it. */
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(BootBlock, boot);

  btSize = (size_t)BTSize(chunk->pageTablePages);
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failPageTableMapped;
  vmChunk->pageTableMapped = p;
  res = BootAlloc(&p, boot, btSize, MPS_PF_ALIGN);
  if (res != ResOK)
    goto failnoSparePages;
  vmChunk->noSparePages = p;

  /* Actually commit all the tables. <design/arenavm/>.@@@@ */
  overheadLimit = AddrAdd(chunk->base, (Size)BootAllocated(boot));
  if (vmChunk->overheadMappedLimit < overheadLimit) {
    overheadLimit = AddrAlignUp(overheadLimit, ChunkPageSize(chunk));
    res = vmArenaMap(VMChunkVMArena(vmChunk), vmChunk->vm,
                     vmChunk->overheadMappedLimit, overheadLimit);
    if (res != ResOK)
      goto failTableMap;
    vmChunk->overheadMappedLimit = overheadLimit;
  }

  BTResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages);
  BTSetRange(vmChunk->noSparePages, 0, chunk->pageTablePages);
  return ResOK;

  /* .no-clean: No clean-ups needed for boot, as we will discard the chunk. */
failTableMap:
failnoSparePages:
failPageTableMapped:
  return res;
}


/* vmChunkDestroy -- destroy a VMChunk */

static void vmChunkDestroy(Chunk chunk)
{
  VM vm;
  VMChunk vmChunk;

  AVERT(Chunk, chunk);
  vmChunk = Chunk2VMChunk(chunk);
  AVERT(VMChunk, vmChunk);
  AVER(BTIsSetRange(vmChunk->noSparePages, 0, chunk->pageTablePages));
  AVER(BTIsResRange(vmChunk->pageTableMapped, 0, chunk->pageTablePages));

  vmChunk->sig = SigInvalid;
  vm = vmChunk->vm;
  ChunkFinish(chunk);
  VMDestroy(vm);
}


/* VMChunkFinish -- finish a VMChunk */

static void VMChunkFinish(Chunk chunk)
{
  VMChunk vmChunk = Chunk2VMChunk(chunk);

  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               VMBase(vmChunk->vm), vmChunk->overheadMappedLimit);
  /* No point in finishing the other fields, since they are unmapped. */
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */
static Res VMArenaInit(Arena *arenaReturn, ArenaClass class, va_list args)
{
  Size userSize;        /* size requested by user */
  Size chunkSize;       /* size actually created */
  Size vmArenaSize; /* aligned size of VMArenaStruct */
  Res res;
  VMArena vmArena;
  Arena arena;
  Index gen;
  VM arenaVM;
  Chunk chunk;

  userSize = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER(class == VMArenaClassGet() || class == VMNZArenaClassGet());
  AVER(userSize > 0);

  /* Create a VM to hold the arena and map it. */
  vmArenaSize = SizeAlignUp(sizeof(VMArenaStruct), MPS_PF_ALIGN);
  res = VMCreate(&arenaVM, vmArenaSize);
  if (res != ResOK)
    goto failVMCreate;
  res = VMMap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  if (res != ResOK)
    goto failVMMap;
  vmArena = (VMArena)VMBase(arenaVM);

  arena = VMArena2Arena(vmArena);
  /* <code/arena.c#init.caller> */
  res = ArenaInit(arena, class);
  if (res != ResOK)
    goto failArenaInit;
  arena->committed = VMMapped(arenaVM);

  vmArena->vm = arenaVM;
  vmArena->spareSize = 0;

  GangsetInit(arena, &vmArena->gangset);
  
  /* .blacklist: We blacklist the zones corresponding to small integers. */
  vmArena->blacklist =
    ZoneSetAdd(arena, ZoneSetAdd(arena, ZoneSetEMPTY, (Addr)1), (Addr)-1);

  vmArena->uncolZoneSet = ZoneSetEMPTY;
  vmArena->nogenZoneSet = ZoneSetEMPTY;
  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genZoneSet[gen] = ZoneSetEMPTY;
  }
  vmArena->freeSet = ZoneSetUNIV; /* includes blacklist */
  /* <design/arena/#coop-vm.struct.vmarena.extendby.init> */
  vmArena->extendBy = userSize;
  vmArena->extendMin = 0;

  /* have to have a valid arena before calling ChunkCreate */
  vmArena->sig = VMArenaSig;
  res = VMChunkCreate(&chunk, vmArena, userSize);
  if (res != ResOK)
    goto failChunkCreate;
  arena->primary = chunk;

  /* .zoneshift: Set the zone shift to divide the chunk into the same */
  /* number of stripes as will fit into a reference set (the number */
  /* of bits in a word).  Note that some zones are discontiguous in */
  /* the chunk if the size is not a power of 2.  */
  /* See <design/arena/#class.fields>. */
  chunkSize = AddrOffset(chunk->base, chunk->limit);
  arena->zoneShift = SizeFloorLog2(chunkSize >> MPS_WORD_SHIFT);

  AVERT(VMArena, vmArena);
  if ((ArenaClass)mps_arena_class_vm() == class)
    EVENT_PWW(ArenaCreateVM, arena, userSize, chunkSize);
  else
    EVENT_PWW(ArenaCreateVMNZ, arena, userSize, chunkSize);
  *arenaReturn = arena;
  return ResOK;

failChunkCreate:
  ArenaFinish(arena);
failArenaInit:
  VMUnmap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
failVMMap:
  VMDestroy(arenaVM);
failVMCreate:
  return res;
}


/* VMArenaFinish -- finish the arena */

static void VMArenaFinish(Arena arena)
{
  VMArena vmArena;
  Ring node, next;
  VM arenaVM;

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  arenaVM = vmArena->vm;

  sparePagesPurge(vmArena);
  /* destroy all chunks */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    vmChunkDestroy(chunk);
  }
  AVER(arena->committed == VMMapped(arenaVM));

  vmArena->sig = SigInvalid;

  ArenaFinish(arena); /* <code/global.c#finish.caller> */

  VMUnmap(arenaVM, VMBase(arenaVM), VMLimit(arenaVM));
  VMDestroy(arenaVM);
  EVENT_P(ArenaDestroy, vmArena);
}


/* VMArenaReserved -- return the amount of reserved address space
 *
 * Add up the reserved space from all the chunks.
 */
static Size VMArenaReserved(Arena arena)
{
  Size reserved;
  Ring node, next;

  reserved = 0;
  RING_FOR(node, &arena->chunkRing, next) {
    VMChunk vmChunk = Chunk2VMChunk(RING_ELT(Chunk, chunkRing, node));
    reserved += VMReserved(vmChunk->vm);
  }
  return reserved;
}


/* VMArenaSpareCommitExceeded -- handle excess spare pages */

static void VMArenaSpareCommitExceeded(Arena arena)
{
  VMArena vmArena;

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);

  sparePagesPurge(vmArena);
  return;
}


/* Page Table Partial Mapping
 *
 * Some helper functions
 */


/* tablePageBaseIndex -- index of the first page descriptor falling
 *                       (at least partially) on this table page
 *
 * .repr.table-page: Table pages are passed as the page's base address.
 *
 * .division: We calculate it by dividing the offset from the beginning
 * of the page table by the size of a table element.  This relies on
 * .vm.addr-is-star.
 */
#define tablePageBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
   / sizeof(PageStruct))


/* tablePageWholeBaseIndex
 *
 * Index of the first page descriptor wholly on this table page.
 * Table page specified by address (not index).
 */
#define tablePageWholeBaseIndex(chunk, tablePage) \
  (AddrOffset((Addr)(chunk)->pageTable, \
              AddrAdd((tablePage), sizeof(PageStruct)-1)) \
   / sizeof(PageStruct))


/* tablePageLimitIndex -- index of the first page descriptor falling
 *                        (wholly) on the next table page
 *
 * Similar to tablePageBaseIndex, see .repr.table-page and .division.
 */
#define tablePageLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + ChunkPageSize(chunk) - 1) \
   / sizeof(PageStruct) \
   + 1)

/* tablePageWholeLimitIndex
 *
 * Index of the first page descriptor falling partially on the next
 * table page.
 */
#define tablePageWholeLimitIndex(chunk, tablePage) \
  ((AddrOffset((Addr)(chunk)->pageTable, (tablePage)) \
    + ChunkPageSize(chunk)) \
   / sizeof(PageStruct))


/* tablePageInUse -- Check whether a given page of the page table is in use
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if
 * any of the page descriptors falling on it (even partially) are being
 * used.  Relies on .repr.table-page and .vm.addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're (de)allocating.
 */
static Bool tablePageInUse(Chunk chunk, Addr tablePage)
{
  Index limitIndex;

  /* Check it's in the page table. */
  AVER((Addr)&chunk->pageTable[0] <= tablePage);
  AVER(tablePage < addrOfPageDesc(chunk, chunk->pages));

  if (tablePage == AddrPageBase(chunk, addrOfPageDesc(chunk, chunk->pages))) {
    limitIndex = chunk->pages;
  } else {
    limitIndex = tablePageLimitIndex(chunk, tablePage);
  }
  AVER(limitIndex <= chunk->pages);

  return !BTIsResRange(chunk->allocTable,
                       tablePageBaseIndex(chunk, tablePage), limitIndex);
}


/* tablePagesUsed
 *
 * Takes a range of pages identified by [pageBase, pageLimit), and
 * returns the pages occupied by the page table which store the
 * PageStruct descriptors for those pages.
 */
static void tablePagesUsed(Index *tableBaseReturn, Index *tableLimitReturn,
                           Chunk chunk, Index pageBase, Index pageLimit)
{
  *tableBaseReturn =
    PageTablePageIndex(chunk,
                       AddrPageBase(chunk, addrOfPageDesc(chunk, pageBase)));
  *tableLimitReturn =
    PageTablePageIndex(chunk,
                       AddrAlignUp(addrOfPageDesc(chunk, pageLimit),
                                   ChunkPageSize(chunk)));
  return;
}


/* tablePagesEnsureMapped -- ensure needed part of page table is mapped
 *
 * Pages from baseIndex to limitIndex are about to be allocated.
 * Ensure that the relevant pages occupied by the page table are mapped.
 */
static Res tablePagesEnsureMapped(VMChunk vmChunk,
                                  Index baseIndex, Index limitIndex)
{
  /* tableBaseIndex, tableLimitIndex, tableCursorIndex, */
  /* unmappedBase, unmappedLimit are all indexes of pages occupied */
  /* by the page table. */
  Index tableBaseIndex, tableLimitIndex;
  Index tableCursorIndex;
  Index unmappedBaseIndex, unmappedLimitIndex;
  Index i;
  Chunk chunk;
  Res res;

  chunk = VMChunk2Chunk(vmChunk);

  tablePagesUsed(&tableBaseIndex, &tableLimitIndex,
                 chunk, baseIndex, limitIndex);

  tableCursorIndex = tableBaseIndex;
 
  while(BTFindLongResRange(&unmappedBaseIndex, &unmappedLimitIndex,
                           vmChunk->pageTableMapped,
                           tableCursorIndex, tableLimitIndex,
                           1)) {
    Addr unmappedBase = TablePageIndexBase(chunk, unmappedBaseIndex);
    Addr unmappedLimit = TablePageIndexBase(chunk, unmappedLimitIndex);
    /* There might be a page descriptor overlapping the beginning */
    /* of the range of table pages we are about to map. */
    /* We need to work out whether we should touch it. */
    if (unmappedBaseIndex == tableBaseIndex
        && unmappedBaseIndex > 0
        && !BTGet(vmChunk->pageTableMapped, unmappedBaseIndex - 1)) {
      /* Start with first descriptor wholly on page */
      baseIndex = tablePageWholeBaseIndex(chunk, unmappedBase);
    } else {
      /* start with first descriptor partially on page */
      baseIndex = tablePageBaseIndex(chunk, unmappedBase);
    }
    /* Similarly for the potentially overlapping page descriptor */
    /* at the end. */
    if (unmappedLimitIndex == tableLimitIndex
        && unmappedLimitIndex < chunk->pageTablePages
        && !BTGet(vmChunk->pageTableMapped, unmappedLimitIndex)) {
      /* Finish with last descriptor wholly on page */
      limitIndex = tablePageBaseIndex(chunk, unmappedLimit);
    } else if (unmappedLimitIndex == chunk->pageTablePages) {
      /* Finish with last descriptor in chunk */
      limitIndex = chunk->pages;
    } else {
      /* Finish with last descriptor partially on page */
      limitIndex = tablePageWholeBaseIndex(chunk, unmappedLimit);
    }
    res = vmArenaMap(VMChunkVMArena(vmChunk),
                     vmChunk->vm, unmappedBase, unmappedLimit);
    if (res != ResOK)
      return res;
    BTSetRange(vmChunk->pageTableMapped, unmappedBaseIndex, unmappedLimitIndex);
    for(i = baseIndex; i < limitIndex; ++i) {
      PageInit(chunk, i);
    }
    tableCursorIndex = unmappedLimitIndex;
    if (tableCursorIndex == tableLimitIndex)
      break;
  }

  return ResOK;
}


/* tablePagesUnmapUnused
 *
 * Of the pages occupied by the page table from tablePageBase to
 * tablePageLimit find those which are wholly unused and unmap them.
 */
static void tablePagesUnmapUnused(VMChunk vmChunk,
                                  Addr tablePageBase, Addr tablePageLimit)
{
  Chunk chunk;
  Addr cursor;
  Size pageSize;

  chunk = VMChunk2Chunk(vmChunk);
  pageSize = ChunkPageSize(chunk);
  AVER(AddrIsAligned(tablePageBase, pageSize));
  AVER(AddrIsAligned(tablePageLimit, pageSize));

  /* for loop indexes over base addresses of pages occupied by page table */
  for(cursor = tablePageBase;
      cursor < tablePageLimit;
      cursor = AddrAdd(cursor, pageSize)) {
    if (!tablePageInUse(chunk, cursor)) {
      vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
                   cursor, AddrAdd(cursor, pageSize));
      AVER(BTGet(vmChunk->noSparePages, PageTablePageIndex(chunk, cursor)));
      AVER(BTGet(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor)));
      BTRes(vmChunk->pageTableMapped, PageTablePageIndex(chunk, cursor));
    }
  }
  AVER(cursor == tablePageLimit);

  return;
}


/* pagesFindFreeInArea -- find a range of free pages in a given address range
 *
 * Search for a free run of pages in the free table, between the given
 * base and limit.
 *
 * The downwards arg governs whether we use BTFindShortResRange (if
 * downwards is FALSE) or BTFindShortResRangeHigh (if downwards is
 * TRUE).  This _roughly_ corresponds to allocating pages from top down
 * (when downwards is TRUE), at least within an interval.  It is used
 * for implementing SegPrefHigh.
 */
static Bool pagesFindFreeInArea(Index *baseReturn, Chunk chunk, Size size,
                                Addr base, Addr limit, Bool downwards)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;    /* Index equiv. to base and limit */
  Index start, end;             /* base and limit of free run */

  AVER(AddrIsAligned(base, ChunkPageSize(chunk)));
  AVER(AddrIsAligned(limit, ChunkPageSize(chunk)));
  AVER(chunk->base <= base);
  AVER(base < limit);
  AVER(limit <= chunk->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, ChunkPageSize(chunk)));

  basePage = INDEX_OF_ADDR(chunk, base);
  limitPage = INDEX_OF_ADDR(chunk, limit);
  pages = ChunkSizeToPages(chunk, size);

  if (downwards) {
    if (!BTFindShortResRangeHigh(&start, &end, chunk->allocTable,
                                 basePage, limitPage, pages))
      return FALSE;
  } else {
    if(!BTFindShortResRange(&start, &end, chunk->allocTable,
                            basePage, limitPage, pages))
      return FALSE;
  }

  *baseReturn = start;
  return TRUE;
}


/* pagesFindFreeInZones -- find a range of free pages with a ZoneSet
 *
 * This function finds the intersection of ZoneSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a page whose ZoneSet (see
 * ZoneSetOfPage) will be a subset of the specified ZoneSet.
 *
 * For meaning of downwards arg see pagesFindFreeInArea.
 * .improve.findfree.downwards: This should be improved so that it
 * allocates pages from top down globally, as opposed to (currently)
 * just within an interval.
 */
static Bool pagesFindFreeInZones(Index *baseReturn, VMChunk *chunkReturn,
                                 VMArena vmArena, Size size, ZoneSet zones,
                                 Bool downwards)
{
  Arena arena;
  Addr chunkAllocBase, base, limit;
  Ring node, next;

  arena = VMArena2Arena(vmArena);

  /* Should we check chunk cache first? */
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    AVERT(Chunk, chunk);

    chunkAllocBase = PageIndexBase(chunk, chunk->allocBase);

    for(base = chunkAllocBase;
        ChunkZonesNextArea(&base, &limit, chunk, zones, base);
        base = limit) {
      /* Try to allocate a page in the area. */
      if (AddrOffset(base, limit) >= size
          && pagesFindFreeInArea(baseReturn, chunk, size, base, limit,
                                 downwards)) {
        *chunkReturn = Chunk2VMChunk(chunk);
        return TRUE;
      }
    }
  }

  return FALSE;
}


/* vmGenOfSegPref -- return generation specified by a segment preference */

static Serial vmGenOfSegPref(VMArena vmArena, SegPref pref)
{
  Serial gen;

  AVER(pref->isGen);
  UNUSED(vmArena);

  gen = pref->gen;
  if (gen >= VMArenaGenCount) {
    gen = VMArenaGenCount - 1;
  }
  return gen;
}


/* pagesFindFreeWithSegPref -- find a range of free pages with given preferences
 *
 * Note this does not create or allocate any pages.
 *
 * basereturn: return parameter for the index in the
 *   chunk's page table of the base of the free area found.
 * chunkreturn: return parameter for the chunk in which
 *   the free space has been found.
 * pref: the SegPref object to be used when considering
 *   which zones to try.
 * size: Size to find space for.
 * barge: TRUE iff stealing space in zones used
 *   by other SegPrefs should be considered (if it's FALSE then only
 *   zones already used by this segpref or free zones will be used).
 */
extern Bool pagesFindFreeWithSegPref(Index *baseReturn, VMChunk *chunkReturn,
                                     VMArena vmArena, SegPref pref, Size size,
                                     Bool barge);
Bool pagesFindFreeWithSegPref(Index *baseReturn, VMChunk *chunkReturn,
                                     VMArena vmArena, SegPref pref, Size size,
                                     Bool barge)
{
  ZoneSet preferred;

  if (pref->isGen) {
    Serial gen = vmGenOfSegPref(vmArena, pref);
    preferred = vmArena->genZoneSet[gen];
  } else {
    preferred = pref->zones;
    /* Note: for now, vmArena->nogenZoneSet is recorded, but not used */
    /* to colocate further allocation.  If you want colocation, use */
    /* a 'generation' number.  RHSK 2008-02-15. */
  }

  /* @@@@ Some of these tests might be duplicates.  If we're about */
  /* to run out of virtual address space, then slow allocation is */
  /* probably the least of our worries. */

  /* .alloc.improve.map: Define a function that takes a list */
  /* (say 4 long) of ZoneSets and tries pagesFindFreeInZones on */
  /* each one in turn.  Extra ZoneSet args that weren't needed */
  /* could be ZoneSetEMPTY */

  if (pref->isCollected) { /* GC'd memory */
    /* We look for space in the following places (in order) */
    /*   - Zones already allocated to me (preferred) but are not */
    /*     blacklisted; */
    /*   - Zones that are either allocated to me, or are unallocated */
    /*     but not blacklisted; */
    /*   - Any non-blacklisted zone; */
    /*   - Any zone; */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated (or the default */
    /* is used). */
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetDiff(preferred, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUnion(preferred,
                                             ZoneSetDiff(vmArena->freeSet,
                                                         vmArena->blacklist)),
                                pref->high)) {
      return TRUE; /* found */
    }
    if (!barge)
      /* do not barge into other zones, give up now */
      return FALSE;
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetDiff(ZoneSetUNIV, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUNIV, pref->high)) {
      return TRUE; /* found */
    }
  } else { /* non-GC'd memory */
    /* We look for space in the following places (in order) */
    /*   - Zones preferred (preferred) and blacklisted; */
    /*   - Zones preferred; */
    /*   - Zones preferred or blacklisted zone; */
    /*   - Any zone. */
    /* Note that each is a superset of the previous, unless */
    /* blacklisted zones have been allocated. */
    if (pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                             ZoneSetInter(preferred, vmArena->blacklist),
                             pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                preferred, pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUnion(preferred, vmArena->blacklist),
                                pref->high)
        || pagesFindFreeInZones(baseReturn, chunkReturn, vmArena, size,
                                ZoneSetUNIV, pref->high)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* vmArenaExtend -- Extend the arena by making a new chunk
 *
 * The size arg specifies how much we wish to allocate after the extension.
 */
static Res vmArenaExtend(VMArena vmArena, Size size)
{
  Chunk newChunk;
  Size chunkSize;
  Res res;

  /* Choose chunk size. */
  /* .vmchunk.overhead: This code still lacks a proper estimate of */
  /* the overhead required by a vmChunk for chunkStruct, page tables */
  /* etc.  For now, estimate it as 10%.  RHSK 2007-12-21 */
  do {
    Size fraction = 10;  /* 10% -- see .vmchunk.overhead */
    Size chunkOverhead;
    
    /* 1: use extendBy, if it is big enough for size + overhead */
    chunkSize = vmArena->extendBy;
    chunkOverhead = chunkSize / fraction;
    if(chunkSize > size && (chunkSize - size) >= chunkOverhead)
      break;
    
    /* 2: use size + overhead (unless it overflows SizeMAX) */
    chunkOverhead = size / (fraction - 1);
    if((SizeMAX - size) >= chunkOverhead) {
      chunkSize = size + chunkOverhead;
      break;
    }
    
    /* 3: use SizeMAX */
    chunkSize = SizeMAX;
    break;
  } while(0);


  DIAG_FIRSTF(( "vmArenaExtend_Start", 
    "to accommodate size $W, try chunkSize $W", size, chunkSize,
    " (VMArenaReserved currently $W bytes)\n",
    VMArenaReserved(VMArena2Arena(vmArena)), NULL ));

  DIAG( ArenaDescribe(VMArena2Arena(vmArena), DIAG_STREAM); );

  DIAG_END("vmArenaExtend_Start");

  /* .chunk-create.fail: If we fail, try again with a smaller size */
  {
    int fidelity = 8;  /* max fraction of addr-space we may 'waste' */
    Size chunkHalf;
    Size chunkMin = 4 * 1024;  /* typical single page */
    Size sliceSize;
    
    if (vmArena->extendMin > chunkMin)
      chunkMin = vmArena->extendMin;
    if (chunkSize < chunkMin)
      chunkSize = chunkMin;
    
    for(;; chunkSize = chunkHalf) {
      chunkHalf = chunkSize / 2;
      sliceSize = chunkHalf / fidelity;
      AVER(sliceSize > 0);
      
      /* remove slices, down to chunkHalf but no further */
      for(; chunkSize > chunkHalf; chunkSize -= sliceSize) {
        if(chunkSize < chunkMin) {
          DIAG_SINGLEF(( "vmArenaExtend_FailMin", 
            "no remaining address-space chunk >= min($W)", chunkMin,
            " (so VMArenaReserved remains $W bytes)\n",
            VMArenaReserved(VMArena2Arena(vmArena)), NULL ));
          return ResRESOURCE;
        }
        res = VMChunkCreate(&newChunk, vmArena, chunkSize);
        if(res == ResOK)
          goto vmArenaExtend_Done;
      }
    }
  }

vmArenaExtend_Done:

  DIAG_SINGLEF(( "vmArenaExtend_Done",
    "Request for new chunk of VM $W bytes succeeded", chunkSize,
    " (VMArenaReserved now $W bytes)\n", 
    VMArenaReserved(VMArena2Arena(vmArena)), NULL ));

  return res;
}


/* VM*AllocPolicy -- allocation policy methods */


/* Used in abstracting allocation policy between VM and VMNZ */
typedef Res (*VMAllocPolicyMethod)(Index *, VMChunk *, VMArena, SegPref, Size);

static Res VMAllocPolicy(Index *baseIndexReturn, VMChunk *chunkReturn,
                         VMArena vmArena, SegPref pref, Size size)
{
  Arena arena;
  ZoneSet zones;
  
  arena = VMArena2Arena(vmArena);

  for(SegPrefZonesOpen(arena, pref);
      SegPrefZonesNext(&zones, arena, pref, TRUE);) {
    if(pagesFindFreeInZones(baseIndexReturn, chunkReturn, vmArena, size,
                            zones, pref->high)) {
      /* Range found, but allocation may still fail; therefore */
      /* leave the SegPrefZones search unclosed. */
      return ResOK;
    }
  }
  /* end this search, without having allocated */
  SegPrefZonesClose(arena, pref, FALSE, NULL, NULL);
  
  /* try and extend, but don't worry if we can't */
  (void)vmArenaExtend(vmArena, size);

  /* We may or may not have a new chunk at this point */
  /* we proceed to try the allocation again anyway. */
  /* We specify barging, but if we have got a new chunk */
  /* then hopefully we won't need to barge. */
  for(SegPrefZonesOpen(arena, pref);
      SegPrefZonesNext(&zones, arena, pref, FALSE);) {
    if(pagesFindFreeInZones(baseIndexReturn, chunkReturn, vmArena, size,
                            zones, pref->high)) {
      /* Range found, but allocation may still fail; therefore */
      /* leave the SegPrefZones search unclosed. */
      return ResOK;
    }
  }
  /* end this search, without having allocated */
  SegPrefZonesClose(arena, pref, FALSE, NULL, NULL);

  /* .improve.alloc-fail: This could be because the request was */
  /* too large, or perhaps the arena is fragmented.  We could */
  /* return a more meaningful code. */
  return ResRESOURCE;
}

static Res VMNZAllocPolicy(Index *baseIndexReturn, VMChunk *chunkReturn,
                           VMArena vmArena, SegPref pref, Size size)
{
  Arena arena = VMArena2Arena(vmArena);
  SegPrefZonesOpen(arena, pref);
  if (pagesFindFreeInZones(baseIndexReturn, chunkReturn, vmArena, size,
                           ZoneSetUNIV, pref->high)) {
    return ResOK;
  }
  SegPrefZonesClose(arena, pref, FALSE, NULL, NULL);
  return ResRESOURCE;
}


/* pageIsMapped -- checks whether a free page is mapped or not. */

static Bool pageIsMapped(VMChunk vmChunk, Index pi)
{
  Index pageTableBaseIndex;
  Index pageTableLimitIndex;
  int pageType;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  /* Note that unless the pi'th PageStruct crosses a page boundary */
  /* Base and Limit will differ by exactly 1. */
  /* They will differ by at most 2 assuming that */
  /* sizeof(PageStruct) <= ChunkPageSize(chunk) (!) */
  tablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex, chunk, pi, pi+1);
  /* using unsigned arithmetic overflow to use just one comparison */
  AVER(pageTableLimitIndex - pageTableBaseIndex - 1 < 2);

  /* We can examine the PageStruct descriptor iff both table pages */
  /* are mapped. */
  if (BTGet(vmChunk->pageTableMapped, pageTableBaseIndex)
      && BTGet(vmChunk->pageTableMapped, pageTableLimitIndex - 1)) {
    pageType = PageType(&chunk->pageTable[pi]);
    if (pageType == PageTypeSpare)
      return TRUE;
    AVER(pageType == PageTypeFree);
  }
  return FALSE;
}


/* sparePageRelease -- releases a spare page
 *
 * Either to allocate it or to purge it.
 * Temporarily leaves it in an inconsistent state.
 */
static void sparePageRelease(VMChunk vmChunk, Index pi)
{
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Arena arena = ChunkArena(chunk);

  AVER(PageType(&chunk->pageTable[pi]) == PageTypeSpare);
  AVER(arena->spareCommitted >= ChunkPageSize(chunk));
  arena->spareCommitted -= ChunkPageSize(chunk);
  return;
}


/* pagesMarkAllocated -- Mark the pages allocated */

static Res pagesMarkAllocated(VMArena vmArena, VMChunk vmChunk,
                              Index baseIndex, Count pages, Pool pool)
{
  Index i;
  Index limitIndex;
  Index mappedBase, mappedLimit;
  Index unmappedBase, unmappedLimit;
  Chunk chunk = VMChunk2Chunk(vmChunk);
  Res res;

  /* Ensure that the page descriptors we need are on mapped pages. */
  limitIndex = baseIndex + pages;
  res = tablePagesEnsureMapped(vmChunk, baseIndex, limitIndex);
  if (res != ResOK)
    goto failTableMap;

  mappedBase = baseIndex;
  mappedLimit = mappedBase;

  do {
    while(pageIsMapped(vmChunk, mappedLimit)) {
      ++mappedLimit;
      if (mappedLimit >= limitIndex)
        break;
    }
    AVER(mappedLimit <= limitIndex);
    /* NB for loop will loop 0 times iff first page is not mapped */
    for(i = mappedBase; i < mappedLimit; ++i) {
      sparePageRelease(vmChunk, i);
      PageAlloc(chunk, i, pool);
    }
    if (mappedLimit >= limitIndex)
      break;
    unmappedBase = mappedLimit;
    unmappedLimit = unmappedBase;
    while(!pageIsMapped(vmChunk, unmappedLimit)) {
      ++unmappedLimit;
      if (unmappedLimit >= limitIndex)
        break;
    }
    AVER(unmappedLimit <= limitIndex);
    res = vmArenaMap(vmArena, vmChunk->vm,
                     PageIndexBase(chunk, unmappedBase),
                     PageIndexBase(chunk, unmappedLimit));
    if (res != ResOK)
      goto failPagesMap;
    for(i = unmappedBase; i < unmappedLimit; ++i) {
      PageAlloc(chunk, i, pool);
    }
    mappedBase = unmappedLimit;
    mappedLimit = mappedBase;
  } while(mappedLimit < limitIndex);
  AVER(mappedLimit == limitIndex);

  return ResOK;

failPagesMap:
  /* region from baseIndex to mappedLimit needs unmapping */
  if (baseIndex < mappedLimit) {
    vmArenaUnmap(vmArena, vmChunk->vm,
                 PageIndexBase(chunk, baseIndex),
                 PageIndexBase(chunk, mappedLimit));
    /* mark pages as free */
    for(i = baseIndex; i < mappedLimit; ++i) {
      TractFinish(PageTract(&chunk->pageTable[i]));
      PageFree(chunk, i);
    }
  }
  {
    Index pageTableBaseIndex, pageTableLimitIndex;
    /* find which pages of page table were affected */
    tablePagesUsed(&pageTableBaseIndex, &pageTableLimitIndex,
                   chunk, baseIndex, limitIndex);
    /* Resetting the noSparePages bits is lazy, it means that */
    /* we don't have to bother trying to unmap unused portions */
    /* of the pageTable. */
    BTResRange(vmChunk->noSparePages, pageTableBaseIndex, pageTableLimitIndex);
  }
failTableMap:
  return res;
}


/* vmAllocComm -- allocate a region from the arena
 *
 * Common code used by mps_arena_class_vm and
 * mps_arena_class_vmnz.
 */
static Res vmAllocComm(Addr *baseReturn, Tract *baseTractReturn,
                       VMAllocPolicyMethod policy,
                       SegPref pref, Size size, Pool pool)
{
  Addr base, limit;
  Tract baseTract;
  Arena arena;
  Count pages;
  Index baseIndex;
  Res res;
  VMArena vmArena;
  VMChunk vmChunk;
  Chunk chunk;

  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVER(FunCheck((Fun)policy));
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
 
  /* NULL is used as a discriminator */
  /* (see <design/arenavm/table.disc>) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  /* Early check on commit limit. */
  if (arena->spareCommitted < size) {
    Size necessaryCommitIncrease = size - arena->spareCommitted;
    if (arena->committed + necessaryCommitIncrease > arena->commitLimit
        || arena->committed + necessaryCommitIncrease < arena->committed) {
      return ResCOMMIT_LIMIT;
    }
  }

  res = (*policy)(&baseIndex, &vmChunk, vmArena, pref, size);
  if (res != ResOK)
    return res;

  /* chunk (and baseIndex) should be initialised by policy */
  AVERT(VMChunk, vmChunk);
  chunk = VMChunk2Chunk(vmChunk);

  /* Compute number of pages to be allocated. */
  pages = ChunkSizeToPages(chunk, size);

  res = pagesMarkAllocated(vmArena, vmChunk, baseIndex, pages, pool);
  if (res != ResOK) {
    if (arena->spareCommitted > 0) {
      sparePagesPurge(vmArena);
      res = pagesMarkAllocated(vmArena, vmChunk, baseIndex, pages, pool);
      if (res != ResOK)
        goto failPagesMap;
      /* win! */
    } else {
      goto failPagesMap;
    }
  }

  base = PageIndexBase(chunk, baseIndex);
  baseTract = PageTract(&chunk->pageTable[baseIndex]);
  limit = AddrAdd(base, size);
  
  SegPrefZonesClose(arena, pref, TRUE, base, limit);
 
  *baseReturn = base;
  *baseTractReturn = baseTract;
  return ResOK;

failPagesMap:
  SegPrefZonesClose(arena, pref, FALSE, NULL, NULL);
  return res;
}


static Res VMAlloc(Addr *baseReturn, Tract *baseTractReturn,
                   SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common vmAllocComm */
  return vmAllocComm(baseReturn, baseTractReturn,
                     VMAllocPolicy, pref, size, pool);
}

static Res VMNZAlloc(Addr *baseReturn, Tract *baseTractReturn,
                     SegPref pref, Size size, Pool pool)
{
  /* All checks performed in common vmAllocComm */
  return vmAllocComm(baseReturn, baseTractReturn,
                     VMNZAllocPolicy, pref, size, pool);
}


/* spareRangesMap -- map a function over spare ranges
 *
 * The function f is called on the ranges of spare pages which are
 * within the range of pages from base to limit.  PageStruct descriptors
 * from base to limit should be mapped in the page table before calling
 * this function.
 */
typedef void (*spareRangesFn)(VMChunk, Index, Index, void *);

static void spareRangesMap(VMChunk vmChunk, Index base, Index limit,
                           spareRangesFn f, void *p)
{
  Index spareBase, spareLimit;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  AVER(base < limit);

  spareBase = base;
  do {
    while(!pageIsSpare(&chunk->pageTable[spareBase])) {
      ++spareBase;
      if (spareBase >= limit)
        goto done;
    }
    spareLimit = spareBase;
    while(pageIsSpare(&chunk->pageTable[spareLimit])) {
      ++spareLimit;
      if (spareLimit >= limit)
        break;
    }
    f(vmChunk, spareBase, spareLimit, p);
    spareBase = spareLimit;
  } while(spareBase < limit);
done:
  AVER(spareBase == limit);

  return;
}


/* vmArenaUnmapSpareRange
 *
 * Takes a range of spare pages and unmaps them, turning them into free pages.
 */
static void vmArenaUnmapSpareRange(VMChunk vmChunk,
                                   Index rangeBase, Index rangeLimit, void *p)
{
  Index i;
  Chunk chunk = VMChunk2Chunk(vmChunk);

  UNUSED(p);
  for(i = rangeBase; i < rangeLimit; ++i) {
    sparePageRelease(vmChunk, i);
    PageInit(chunk, i);
  }
  vmArenaUnmap(VMChunkVMArena(vmChunk), vmChunk->vm,
               PageIndexBase(chunk, rangeBase),
               PageIndexBase(chunk, rangeLimit));

  return;
}
 

/* sparePagesPurge -- all spare pages are found and purged (unmapped)
 *
 * This is currently the only way the spare pages are reduced.
 *
 * It uses the noSparePages bits to determine which areas of the
 * pageTable to examine.
 */
static void sparePagesPurge(VMArena vmArena)
{
  Ring node, next;
  Arena arena = VMArena2Arena(vmArena);

  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    VMChunk vmChunk = Chunk2VMChunk(chunk);
    Index spareBaseIndex, spareLimitIndex;
    Index tablePageCursor = 0;

    while(BTFindLongResRange(&spareBaseIndex, &spareLimitIndex,
                             vmChunk->noSparePages,
                             tablePageCursor, chunk->pageTablePages,
                             1)) {
      Addr spareTableBase, spareTableLimit;
      Index pageBase, pageLimit;
      Index tablePage;

      spareTableBase = TablePageIndexBase(chunk, spareBaseIndex);
      spareTableLimit = TablePageIndexBase(chunk, spareLimitIndex);
      /* Determine whether to use initial overlapping PageStruct. */
      if (spareBaseIndex > 0
          && !BTGet(vmChunk->pageTableMapped, spareBaseIndex - 1)) {
        pageBase = tablePageWholeBaseIndex(chunk, spareTableBase);
      } else {
        pageBase = tablePageBaseIndex(chunk, spareTableBase);
      }
      for(tablePage = spareBaseIndex; tablePage < spareLimitIndex;
          ++tablePage) {
        /* Determine whether to use final overlapping PageStruct. */
        if (tablePage == spareLimitIndex - 1
            && spareLimitIndex < chunk->pageTablePages
            && !BTGet(vmChunk->pageTableMapped, spareLimitIndex)) {
          pageLimit =
            tablePageWholeLimitIndex(chunk,
                                     TablePageIndexBase(chunk, tablePage));
        } else if (tablePage == chunk->pageTablePages - 1) {
          pageLimit = chunk->pages;
        } else {
          pageLimit =
            tablePageLimitIndex(chunk, TablePageIndexBase(chunk, tablePage));
        }
        if (pageBase < pageLimit) {
          spareRangesMap(vmChunk, pageBase, pageLimit,
                         vmArenaUnmapSpareRange, NULL);
        } else {
          /* Only happens for last page occupied by the page table */
          /* and only then when that last page has just the tail end */
          /* part of the last page descriptor and nothing more. */
          AVER(pageBase == pageLimit);
          AVER(tablePage == chunk->pageTablePages - 1);
        }
        BTSet(vmChunk->noSparePages, tablePage);
        pageBase = pageLimit;
      }
      tablePagesUnmapUnused(vmChunk, spareTableBase, spareTableLimit);
      tablePageCursor = spareLimitIndex;
      if (tablePageCursor >= chunk->pageTablePages) {
        AVER(tablePageCursor == chunk->pageTablePages);
        break;
      }
    }

  }

  AVER(arena->spareCommitted == 0);
  return;
}


/* VMFree -- free a region in the arena */

static void VMFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  VMArena vmArena;
  VMChunk vmChunk;
  Chunk chunk;
  Count pages;
  Index pi, piBase, piLimit;
  Index pageTableBase;
  Index pageTableLimit;
  Bool foundChunk;

  AVER(base != NULL);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);

  /* All chunks have same pageSize. */
  AVER(SizeIsAligned(size, ChunkPageSize(arena->primary)));
  AVER(AddrIsAligned(base, ChunkPageSize(arena->primary)));

  foundChunk = ChunkOfAddr(&chunk, arena, base);
  AVER(foundChunk);
  vmChunk = Chunk2VMChunk(chunk);

  /* Calculate the number of pages in the region */
  pages = ChunkSizeToPages(chunk, size);
  piBase = INDEX_OF_ADDR(chunk, base);
  piLimit = piBase + pages;
  AVER(piBase < piLimit);
  AVER(piLimit <= chunk->pages);

  /* loop from pageBase to pageLimit-1 inclusive */
  /* Finish each Tract found, then convert them to spare pages. */
  for(pi = piBase; pi < piLimit; ++pi) {
    Page page = &chunk->pageTable[pi];
    Tract tract = PageTract(page);
    AVER(TractPool(tract) == pool);

    TractFinish(tract);
    PagePool(page) = NULL;
    PageType(page) = PageTypeSpare;
  }
  arena->spareCommitted += ChunkPagesToSize(chunk, piLimit - piBase);
  BTResRange(chunk->allocTable, piBase, piLimit);

  tablePagesUsed(&pageTableBase, &pageTableLimit, chunk, piBase, piLimit);
  BTResRange(vmChunk->noSparePages, pageTableBase, pageTableLimit);

  if (arena->spareCommitted > arena->spareCommitLimit) {
    sparePagesPurge(vmArena);
  }
  /* @@@@ Chunks are never freed. */

  return;
}


mps_res_t mps_arena_vm_growth(mps_arena_t mps_arena,
                              size_t mps_desired, size_t mps_minimum)
{
  Arena arena = (Arena)mps_arena;
  Size desired = (Size)mps_desired;
  Size minimum = (Size)mps_minimum;
  VMArena vmArena;
  
  ArenaEnter(arena);
  
  AVERT(Arena, arena);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  
  if(desired < minimum) {
    /* May not desire an increment smaller than the minimum! */
    ArenaLeave(arena);
    return MPS_RES_PARAM;
  }
  
  vmArena->extendBy = desired;
  vmArena->extendMin = minimum;
  
  ArenaLeave(arena);
  
  return MPS_RES_OK;
}


/* VMArenaClass  -- The VM arena class definition */

DEFINE_ARENA_CLASS(VMArenaClass, this)
{
  INHERIT_CLASS(this, AbstractArenaClass);
  this->name = "VM";
  this->size = sizeof(VMArenaStruct);
  this->offset = offsetof(VMArenaStruct, arenaStruct);
  this->init = VMArenaInit;
  this->finish = VMArenaFinish;
  this->reserved = VMArenaReserved;
  this->spareCommitExceeded = VMArenaSpareCommitExceeded;
  this->alloc = VMAlloc;
  this->free = VMFree;
  this->chunkInit = VMChunkInit;
  this->chunkFinish = VMChunkFinish;
  this->describe = VMArenaDescribe;
}


/* VMNZArenaClass  -- The VMNZ arena class definition
 *
 * VMNZ is just VMArena with a different allocation policy.
 */
DEFINE_ARENA_CLASS(VMNZArenaClass, this)
{
  INHERIT_CLASS(this, VMArenaClass);
  this->name = "VMNZ";
  this->alloc = VMNZAlloc;
}


/* mps_arena_class_vm -- return the arena class VM */

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)VMArenaClassGet();
}


/* mps_arena_class_vmnz -- return the arena class VMNZ */

mps_arena_class_t mps_arena_class_vmnz(void)
{
  return (mps_arena_class_t)VMNZArenaClassGet();
}


void ArenaTractsInZones(Count *tractsUsed, Count *tractsFree,
                        Arena arena, ZoneSet zones)
{
  Count tTot = 0, tUsed, tFree = 0;  /* count tracts */
  Ring node, next;
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, chunkRing, node);
    Addr base, limit;
    for(base = PageIndexBase(chunk, chunk->allocBase);
        ChunkZonesNextArea(&base, &limit, chunk, zones, base);
        base = limit) {
      Index bi, li;  /* index equiv. to base and limit */
      Index biFound, liFound;  /* free range */

      AVER(AddrIsAligned(base, ChunkPageSize(chunk)));
      AVER(AddrIsAligned(limit, ChunkPageSize(chunk)));
      AVER(chunk->base <= base);
      AVER(base < limit);
      AVER(limit <= chunk->limit);

      bi = INDEX_OF_ADDR(chunk, base);
      li = INDEX_OF_ADDR(chunk, limit);
      tTot += li - bi;
      
      /* count all free ranges */
      /* @@@@ more efficient methods exist; */
      /* @@@@ see "popcount" in the literature */
      while((bi < li)
            && BTFindLongResRange(&biFound, &liFound, chunk->allocTable,
                                  bi, li, 1)) {
        AVER(biFound < liFound);
        tFree += liFound - biFound;
        bi = liFound;
      }
    }
  }
  AVER(tFree < (1 << 30));
  AVER(tFree <= tTot);
  tUsed = tTot - tFree;
  *tractsUsed = tUsed;
  *tractsFree = tFree;
}

/* GANGS */

/* GangCheck */

Bool GangCheck(Gang gang)
{
  CHECKS(Gang, gang);
  CHECKL(gang->name[GANGNAMELEN - 1] == '\0');
  CHECKL(gang->preferred != ZoneSetUNIV);
  CHECKL(ZoneSetSub(gang->claimed, gang->in));
  CHECKL(BoolCheck(gang->hasCollected));
  CHECKL(BoolCheck(gang->hasUncollected));
  return TRUE;
}


/* GangDescribe */
 
Res GangDescribe(Gang gang, mps_lib_FILE *stream)
{
  return GangFullDescribe(gang, NULL, ZoneSetEMPTY, NULL, stream);
}
 
Res GangFullDescribe(Gang gang, Gang gangNew, ZoneSet zonesNew,
                     Arena arenaForUsage, mps_lib_FILE *stream)
{
  Res res;
  Count zones = MPS_WORD_WIDTH;
  Index zone;
  /* Count tracts: total, Used, Free; percentage used */
  Count t1 = 0, t1U = 0, t1F = 0;  /* unique */
  Count tc = 0, tcU = 0, tcF = 0;  /* claimed */
  Count th = 0, thU = 0, thF = 0, phU = 0;  /* home = unique + claimed */
  Count tb = 0, tbU = 0, tbF = 0, pbU = 0;  /* borrowed */

  if(!CHECKT(Gang, gang))
    return ResFAIL;
  if(gangNew && !CHECKT(Gang, gangNew))
    return ResFAIL;
  /* no check for zonesNew */
  if(arenaForUsage && !CHECKT(Arena, arenaForUsage))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
    "Gang \"$S\" ",
    (WriteFS)gang->name, NULL);
  if(res != ResOK)
    return res;

  if(arenaForUsage) {
    for(zone = zones; zone-- > 0;) {
      ZoneSet z = 1 << zone;
      Count tractsUsed = 0, tractsFree = 0;
      Bool in = (ZoneSetInter(z, gang->in) != ZoneSetEMPTY);
      Bool claimed = (ZoneSetInter(z, gang->claimed) != ZoneSetEMPTY);
      Bool unique = (ZoneSetInter(z, gang->unique) != ZoneSetEMPTY);
      Bool borrowed = in && !claimed;
      ArenaTractsInZones(&tractsUsed, &tractsFree, arenaForUsage, z);
      if(unique) {
        t1U += tractsUsed;
        t1F += tractsFree;
      } else if(claimed) {
        tcU += tractsUsed;
        tcF += tractsFree;
      } else if(borrowed) {
        tbU += tractsUsed;
        tbF += tractsFree;
      } else {
        AVER(!in);
      }
    }
    /* home = unique + claimed */
    thU = t1U + tcU;
    thF = t1F + tcF;

    /* total = Used + Free */
    t1 = t1U + t1F;
    tc = tcU + tcF;
    th = thU + thF;
    tb = tbU + tbF;

    if(th != 0)
      phU = (100 * thU) / th;
    if(tb != 0)
      pbU = (100 * tbU) / tb;

    /* home */
    if(th == 0) {
      res = WriteF(stream,
        "            ",
        NULL);
    } else {
      res = WriteF(stream,
        "$ut ($u%) ",
        thU, phU,
        NULL);
    }
    if(res != ResOK)
      return res;

    /* borrowed */
    if(tb == 0) {
      res = WriteF(stream,
        "            ",
        NULL);
    } else {
      res = WriteF(stream,
        "$ut ($u%) ",
        tbU, pbU,
        NULL);
    }
    if(res != ResOK)
      return res;
  }

  for(zone = zones; zone-- > 0;) {
    ZoneSet z = 1 << zone;
    Bool in = (ZoneSetInter(z, gang->in) != ZoneSetEMPTY);
    Bool claimed = (ZoneSetInter(z, gang->claimed) != ZoneSetEMPTY);
    Bool unique = (ZoneSetInter(z, gang->unique) != ZoneSetEMPTY);
    res = WriteF(stream,
      "$S", unique ? (WriteFS)"1"
            : claimed ? (WriteFS)"c"
            : in ? (WriteFS)"b"
            : (WriteFS)".",
      NULL);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream,
    " ",
    (gang->hasCollected
      ? (gang->hasUncollected ? "GC+man " : "GC  ")
      : (gang->hasUncollected ?    "man " : "    ")),
    /* not interesting: " (preferred $B)", (WriteFB)gang->preferred, */
    NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream,
    "\n",
    NULL);
  if(res != ResOK)
    return res;

  if(gang == gangNew) {
    res = WriteF(stream,
      "           ",
      "            ",
      "            ",
      NULL);
    if(res != ResOK)
      return res;
    for(zone = zones; zone-- > 0;) {
      ZoneSet z = 1 << zone;
      Bool newzone = (ZoneSetInter(z, zonesNew) != ZoneSetEMPTY);
      res = WriteF(stream,
        "$S", newzone ? (WriteFS)"^"
              : (WriteFS)" ",
        NULL);
      if(res != ResOK)
        return res;
    }
    res = WriteF(stream,
      " (new)\n",
      NULL);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}


/* GANGSET */

Bool GangsetCheck(Gangset gangset)
{
  Index i;
  ZoneSet preferred = ZoneSetEMPTY;
  ZoneSet in = ZoneSetEMPTY;
  ZoneSet shared = ZoneSetEMPTY;
  ZoneSet claimed = ZoneSetEMPTY;
  ZoneSet unique = ZoneSetEMPTY;

  CHECKS(Gangset, gangset);

  CHECKL(gangset->gangCount > 0);
  for(i = 0; i < gangset->gangCount; i += 1) {
    Gang gang = &gangset->gangs[i];
    CHECKD(Gang, gang);
    CHECKL(gang->index == i);

    CHECKL(ZoneSetInter(preferred, gang->preferred) == ZoneSetEMPTY);
    preferred = ZoneSetUnion(preferred, gang->preferred);

    shared = ZoneSetUnion(shared, ZoneSetInter(in, gang->in));
    in = ZoneSetUnion(in, gang->in);
    
    CHECKL(ZoneSetInter(claimed, gang->claimed) == ZoneSetEMPTY);
    claimed = ZoneSetUnion(claimed, gang->claimed);
    CHECKL(ZoneSetInter(unique, gang->unique) == ZoneSetEMPTY);
    unique = ZoneSetUnion(unique, gang->unique);
  }
  CHECKL(ZoneSetInter(shared, unique) == ZoneSetEMPTY);
  CHECKL(ZoneSetUnion(shared, unique) == in);
  CHECKL(claimed == in);
  
  CHECKL(gangset->gangGenCount < gangset->gangCount);
  CHECKL(gangset->gangIndexDefault >= gangset->gangGenCount);
  CHECKL(gangset->gangIndexDefault < gangset->gangCount);
  CHECKL(gangset->gangIndexAway >= gangset->gangGenCount);
  CHECKL(gangset->gangIndexAway != gangset->gangIndexDefault);
  CHECKL(gangset->gangIndexAway < gangset->gangCount);
  CHECKL(gangset->blacklist != ZoneSetUNIV);
  CHECKL(gangset->unpreferred == ZoneSetComp(preferred));
  CHECKL(gangset->freezones == ZoneSetComp(in));

  CHECKL(BoolCheck(gangset->trying));
  if(gangset->trying) {
    CHECKL(gangset->tryingGangIndex < gangset->gangCount);
    /* no check for tryingZones */
    /* no check for tryingWhat */
  }
  
  return TRUE;
}

Res GangsetDescribe(Gangset gangset, mps_lib_FILE *stream)
{
  return GangsetFullDescribe(gangset, NULL, ZoneSetEMPTY, NULL, stream);
}

Res GangsetFullDescribe(Gangset gangset, Gang gangNew, ZoneSet zonesNew,
                        Arena arenaForUsage, mps_lib_FILE *stream)
{
  Res res;
  Index i;

  if(!CHECKT(Gangset, gangset))
    return ResFAIL;
  if(gangNew && !CHECKT(Gang, gangNew))
    return ResFAIL;
  /* no check for zonesNew */
  if(arenaForUsage && !CHECKT(Arena, arenaForUsage))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
    "Gangset $P {\n", (WriteFP)gangset,
    "[KEY]  zones are...   1:unique to this gang  c:claimed by this gang (but now shared)  b:borrowed (barged into)\n",
    "[COLS]      HOME-ZONES   BORROWED\n",
    "[COLS]   TRACTS (%USED) TRACTS (%USED)\n",
    NULL);
  if(res != ResOK)
    return res;

  for(i = 0; i < gangset->gangCount; i += 1) {
    res = GangFullDescribe(&gangset->gangs[i], gangNew, zonesNew, 
                          arenaForUsage, stream);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream,
    "  blacklist:     $B\n", (WriteFB)gangset->blacklist,
    "unpreferred:     $B\n", (WriteFB)gangset->unpreferred,
    "  freezones:     $B\n", (WriteFB)gangset->freezones,
    NULL);
  if(res != ResOK)
    return res;

  if(gangset->trying) {
    res = WriteF(stream,
      "  Trying: GangIndex $U  Zones $B  $S\n",
      (WriteFU)gangset->tryingGangIndex,
      (WriteFB)gangset->tryingZones,
      (WriteFS)(gangset->tryingWhat == NULL ? "" : gangset->tryingWhat),
      NULL);
  } else {
    res = WriteF(stream,
      "  (not trying)\n",
      NULL);
  }
  if(res != ResOK)
    return res;

  res = WriteF(stream,
    "}\n",
    NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


static void GangsetInit(Arena arena, Gangset gangset)
{
  ZoneSet preferred = ZoneSetEMPTY;
  Index i;

  DIAG_FIRSTF(( "GangsetInit", NULL ));
  for(i = 0; i < ArenaGangCount; i += 1) {
    Gang gang;
    gang = &gangset->gangs[i];
    gang->index = i;
    gang->name[0] = '\0';
    gang->name[1] = '\0';
    gang->name[2] = '\0';
    gang->name[3] = '\0';
    AVER(4 == GANGNAMELEN);
    gang->preferred = ZoneSetEMPTY;
    gang->in = ZoneSetEMPTY;
    gang->claimed = ZoneSetEMPTY;
    gang->hasCollected = FALSE;
    gang->hasUncollected = FALSE;
    if(i < VMArenaGenCount) {
      /* pre-made gangs for generations */
      gang->name[0] = 'g';
      gang->name[1] = 'e';
      gang->name[2] = '0' + i;
    } else if(i == GangIndexDefault) {
      /* nogen default-zoneset: Gang Default */
      gang->name[0] = 'd';
      gang->name[1] = 'e';
      gang->name[2] = 'f';
      /* note: we do NOT set preferred to ArenaDefaultZONESET */
      gangset->gangIndexDefault = GangIndexDefault;
    } else if(i == GangIndexAway) {
      /* nogen non-default-zoneset: Gang Away */
      gang->name[0] = 'a';
      gang->name[1] = 'w';
      gang->name[2] = 'y';
      gang->preferred = ZoneSetComp(ArenaDefaultZONESET);
      gangset->gangIndexAway = GangIndexAway;
    } else {
      /* unused gang */
      gang->name[0] = 'x';
    }
    gang->sig = GangSig;
    AVERT(Gang, gang);
    DIAG( GangDescribe(gang, DIAG_STREAM); );
    preferred = ZoneSetUnion(preferred, gang->preferred);
  }
  AVER(i == ArenaGangCount);
  gangset->gangCount = i;
  gangset->gangGenCount = VMArenaGenCount;

  /* .blacklist: We blacklist the zones corresponding to small integers. */
  gangset->blacklist =
    ZoneSetAdd(arena, ZoneSetAdd(arena, ZoneSetEMPTY, (Addr)1), (Addr)-1);
  
  gangset->freezones = ZoneSetUNIV;
  gangset->unpreferred = ZoneSetComp(preferred);
  gangset->trying = FALSE;
  
  gangset->sig = GangsetSig;
  AVERT(Gangset, gangset);
  DIAG_END("GangsetInit");
}


/* SegPrefZonesOpen -- start a search for zones for this SegPref
 *
 * Usage:
 *   - for(SegPrefZonesOpen; SegPrefZonesNext(&zones); ) {
 *   -   attempt to allocate in zones;
 *   - SegPrefZonesClose, with allocated = True or False.
 *
 * Every search started with SegPrefZonesOpen must be ended with a 
 * call to SegPrefZonesClose (once it is known whether or not a 
 * range of memory has been successfully allocated).
 *
 * Currently, only one such search may be in progress at a time.
 * This is sufficient and simple.  (It would be easy to allow 
 * multiple simultaneous searches if that becomes necessary).
 *
 * The state of the search is recorded in the gangset.  This gives 
 * us a place to store arbitrary state, and lets us verify that 
 * each SegPrefZonesOpen is followed by a SegPrefZonesClose.
 */

void SegPrefZonesOpen(Arena arena, SegPref pref)
{
  VMArena vmArena;
  Gangset gangset;
  Index gangIndex;

  AVERT(Arena, arena);
  AVERT(SegPref, pref);
  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  gangset = &vmArena->gangset;
  /* gangset = &arena->gangset; */
  AVERT(Gangset, gangset);

  AVER(!gangset->trying);

  if(pref->isGen) {
    gangIndex = pref->gen;
    if(gangIndex > VMArenaGenCount) {
      gangIndex = VMArenaGenCount;
    }
  } else if(pref->zones == ArenaDefaultZONESET) {
    gangIndex = GangIndexDefault;
  } else {
    /* for now, all other preferences are lumped together in */
    /* the "Away" gang */
    gangIndex = GangIndexAway;
  }
  AVERT(Gang, &gangset->gangs[gangIndex]);
  
  gangset->trying = TRUE;
  gangset->tryingGangIndex = gangIndex;
  gangset->tryingZones = ZoneSetEMPTY;
  gangset->tryingWhat = NULL;
}


Bool SegPrefZonesNext(ZoneSet *zonesReturn, Arena arena, SegPref pref, 
                      Bool pregrow)
{
  VMArena vmArena;
  Gangset gangset;
  Gang gang;
  ZoneSet bl;
  ZoneSet extra;
  ZoneSet zones;
  char *t;
  Bool newzones;

  AVERT(Arena, arena);
  AVERT(SegPref, pref);
  AVER(BoolCheck(pregrow));

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  gangset = &vmArena->gangset;
  /* gangset = &arena->gangset; */
  AVERT(Gangset, gangset);

  AVER(gangset->trying);
  AVER(gangset->tryingGangIndex < gangset->gangCount);
  gang = &gangset->gangs[gangset->tryingGangIndex];
  AVERT(Gang, gang);

  bl = gangset->blacklist;
  zones = gangset->tryingZones;
  newzones = TRUE;

  /* For simplicity, we insist that each successive set of zones to 
   * try must be a superset of the previous set.  This allows us to 
   * remember what what we've tried so far in a simple ZoneSet.
   *
   * This is a compromise.  A more complex mechanism would make it 
   * possible, after adding a new category (such as shared-claimed), 
   * to still favour the gang's preferred zones, thereby encouraging
   * merged gangs to separate out again.  This idea might be worth 
   * re-considering when/if gang's "preferred" zoneset becomes 
   * widely used by clients.
   */

  if(pref->isCollected) {
    do {
      /* Find a strategy that enlarges the zones to try.
       *
       * Collected Criteria:
       * Most importantly: not blacklist.
       * Then these categories of zone, according to who's using it:
       *     - uniquely me,
       *     - shared(claimed by me),
       *     - free: preferred by me, unpreferred, some other gang's
       *     - shared(borrowed, ie. not claimed by me),
       *     - other owner: pick the zone with most free grains.
       * Then whether it's one of our preferred zones.
       *
       * Attempt Phase-II Growth: before other.
       */
      
      /* Unique */
      t = "in my current unique zones (& my preferred)";
      extra = ZoneSetInter(gang->unique, gang->preferred);
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "in my current unique zones";
      extra = gang->unique;
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Shared(claimed) */
      t = "use a shared claimed zone (& my preferred)";
      extra = ZoneSetInter(gang->claimed, gang->preferred);
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared claimed zone";
      extra = gang->claimed;
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Free zone -- these can be slow */
      t = "add a free zone (my preferred)";
      extra = ZoneSetInter(gangset->freezones, gang->preferred);
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (unpreferred)";
      extra = ZoneSetInter(gangset->freezones, gangset->unpreferred);
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (even other gang's preferred)";
      extra = gangset->freezones;
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Shared(borrowed) */
      /* in but not claimed => borrowed */
      t = "use a shared borrowed zone (& my preferred)";
      extra = ZoneSetInter(gang->in, gang->preferred);
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared borrowed zone";
      extra = gang->in;
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "grow";
      if(pregrow) {
        /* At this point, caller prefers to try to grow the arena, */
        /* rather than barge into another gang's zone.  Ie. we are */
        /* in phase II, not yet in phase III. */
        newzones = FALSE;
        break;
      }

      t = "barge into another gang's zone";
      /* @@@@ should first try the zone with most free grains */
      extra = ZoneSetUNIV;
      zones = ZoneSetUnion(zones, ZoneSetDiff(extra, bl));
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;
      
      t = "allow blacklist";
      zones = ZoneSetUNIV;
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;
      
      t = "no more zones to try";
      AVER(zones == ZoneSetUNIV);
      newzones = FALSE;

    } while(FALSE);

  } else {
    do {
      /* Find a strategy that enlarges the zones to try.
       *
       * Uncollected Criteria:
       * Most importantly, categories of zone, according to who's using it:
       *     - uniquely me,
       *     - shared(claimed by me),
       *     - free: preferred by me, unpreferred, some other gang's
       *     - shared(borrowed, ie. not claimed by me),
       *     - other owner: pick the zone with most free grains.
       * Then whether it's one of our preferred zones.
       * Lastly, using blacklist is good, because GC segs don't want it.
       *
       * Attempt Phase-II Growth: before other.
       */
       
      /* Unique */
      t = "in my current unique zones (& my preferred & blacklist)";
      extra = ZoneSetInter(gang->unique, gang->preferred);
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "in my current unique zones (& my preferred)";
      extra = ZoneSetInter(gang->unique, gang->preferred);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "in my current unique zones";
      extra = gang->unique;
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Shared(claimed) */
      t = "use a shared claimed zone (& my preferred & blacklist)";
      extra = ZoneSetInter(gang->claimed, gang->preferred);
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared claimed zone (& my preferred)";
      extra = ZoneSetInter(gang->claimed, gang->preferred);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared claimed zone";
      extra = gang->claimed;
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Free zone -- these can be slow */
      t = "add a free zone (my preferred & blacklist)";
      extra = ZoneSetInter(gangset->freezones, gang->preferred);
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (my preferred)";
      extra = ZoneSetInter(gangset->freezones, gang->preferred);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (unpreferred & blacklist)";
      extra = ZoneSetInter(gangset->freezones, gangset->unpreferred);
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (unpreferred)";
      extra = ZoneSetInter(gangset->freezones, gangset->unpreferred);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (even other gang's preferred, & blacklist)";
      extra = gangset->freezones;
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "add a free zone (even other gang's preferred)";
      extra = gangset->freezones;
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      /* Shared(borrowed) */
      /* in but not claimed => borrowed */
      t = "use a shared borrowed zone (& my preferred & blacklist)";
      extra = ZoneSetInter(gang->in, gang->preferred);
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared borrowed zone (& my preferred)";
      extra = ZoneSetInter(gang->in, gang->preferred);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared borrowed zone (& blacklist)";
      extra = gang->in;
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "use a shared borrowed zone";
      extra = gang->in;
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;

      t = "time to grow arena, to avoid barging";
      if(pregrow) {
        newzones = FALSE;
        break;
      }

      t = "barge into another gang's zone (& blacklist)";
      extra = ZoneSetUNIV;
      extra = ZoneSetInter(extra, bl);
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;
      
      t = "barge into another gang's zone";
      /* @@@@ should first try the zone with most free grains */
      extra = ZoneSetUNIV;
      zones = ZoneSetUnion(zones, extra);
      if(!ZoneSetSub(zones, gangset->tryingZones))
        break;
      
      t = "no more zones to try";
      AVER(zones == ZoneSetUNIV);
      newzones = FALSE;

    } while(FALSE);

  }
  
  gangset->tryingWhat = t;
  if(newzones) {
    /* AVER(zones > gangset->tryingZones) */
    AVER(ZoneSetSuper(zones, gangset->tryingZones));
    AVER(zones != gangset->tryingZones);
    gangset->tryingZones = zones;
    *zonesReturn = zones;

    DIAG_SINGLEF(( "SegPrefZonesNext",
      "gang \"$S\" ", (WriteFS)gang->name,
      "$S: zones = $B", (WriteFS)t, (WriteFB)zones, NULL ));
  } else {
    DIAG_SINGLEF(( "SegPrefZonesNext",
      "gang \"$S\" ", (WriteFS)gang->name,
      "$S (newzones = Empty)\n", (WriteFS)t, NULL ));
  }
  
  return newzones;
}


/* SegPrefZonesClose -- end the search for zones for this SegPref
 *
 * For each search started with SegPrefZonesOpen, there must be one
 * matching call to SegPrefZonesClose, before the next call to
 * SegPrefZonesOpen.
 *
 * If an allocation was made, pass allocated = TRUE, and the base and 
 * limit of the range of memory that was allocated using this SegPref.  
 * This function records that the zones in the range are in use for 
 * pages allocated under this SegPref.
 *
 * If an allocation was not made, pass allocated = FALSE; pass NULL 
 * for base and limit.  Typically, an allocation was not made because:
 *   - search was abandoned because the caller chose to grow the arena
 *     (and will then start a new search);
 *   - search was abandoned because the caller chose to fail the 
 *     allocation (commit-limit exceeded, phase IV, etc);
 *   - an error occurred when attempting to allocate the found range
 *     (eg. OS refuses to map more pages).
 */

void SegPrefZonesClose(Arena arena, SegPref pref, Bool allocated, 
                       Addr base, Addr limit)
{
  VMArena vmArena;
  ZoneSet zones;
  ZoneSet zonesNew;
  Gangset gangset;
  Gang gang;
  
  AVERT(Arena, arena);
  AVERT(SegPref, pref);
  AVER(BoolCheck(allocated));
  /* base & limit checked later */

  vmArena = Arena2VMArena(arena);
  AVERT(VMArena, vmArena);
  gangset = &vmArena->gangset;
  /* gangset = &arena->gangset; */
  AVERT(Gangset, gangset);

  AVER(gangset->trying);
  AVER(gangset->tryingGangIndex < gangset->gangCount);
  gang = &gangset->gangs[gangset->tryingGangIndex];
  AVERT(Gang, gang);
  
  if(!allocated) {
    /* this search was abandoned */
    AVER(base == NULL);
    AVER(limit == NULL);
    gangset->trying = FALSE;
    return;
  }

  AVER(base < limit);
  zones = ZoneSetOfRange(arena, base, limit);

  zonesNew = ZoneSetDiff(zones, gang->in);
  if(zonesNew != ZoneSetEMPTY) {
    Index i;

    /* Using a new zone. */
    gang->in = ZoneSetUnion(gang->in, zonesNew);
    /* Are zonesNew claimed and unique for this gang?  Assume yes... */
    gang->claimed = ZoneSetUnion(gang->claimed, zonesNew);
    gang->unique = ZoneSetUnion(gang->unique, zonesNew);
    for(i = 0; i < ArenaGangCount; i += 1) {
      /* ... unless we find some other gang already using them. */
      Gang other;
      ZoneSet newclash;
      other = &gangset->gangs[i];
      if(other == gang)
        continue;
      newclash = ZoneSetInter(zonesNew, other->in);
      if(newclash != ZoneSetEMPTY) {
        gang->claimed = ZoneSetDiff(gang->claimed, newclash);
        gang->unique = ZoneSetDiff(gang->unique, newclash);
        other->unique = ZoneSetDiff(other->unique, newclash);
      }
    }
    gangset->freezones = ZoneSetDiff(gangset->freezones, zonesNew);

    DIAG_FIRSTF(( "SegPrefZonesClose_newzone", NULL));
    DIAG( SegPrefDescribe(pref, DIAG_STREAM); );
    DIAG( GangsetFullDescribe(gangset, gang, zonesNew, arena, DIAG_STREAM); );
    DIAG_END("SegPrefZonesClose_newzone");

    AVERT(Gangset, gangset);
  }
  
  if(pref->isCollected) {
    gang->hasCollected = TRUE;
  } else {
    gang->hasUncollected = TRUE;
  }

  gangset->trying = FALSE;
  return;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
