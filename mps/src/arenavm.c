/* impl.c.arenavm: VIRTUAL MEMORY BASED ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arenavm.c(trunk.38) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * This is the implementation of the Segment abstraction from the VM
 * abstraction.  Use of this arena implies use of a VM.
 *
 * DESIGN
 *
 * See design.mps.arena.vm.
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.zone-shift: The VM arena pokes around with arena->zoneShift.
 * In fact, the arena implementation really owns this field.
 *
 *
 * IMPROVEMENTS
 *
 * .improve.table.zone-zero: It would be better to make sure that the
 * page tables are in zone zero, since that zone is least useful for
 * GC. (but it would change how findFreeInRefSet avoids allocating
 * over the tables, see .alloc.skip)@@@@
 */


#include "mpm.h"
#include "mpsavm.h"

SRCID(arenavm, "$HopeName: MMsrc!arenavm.c(trunk.38) $");


typedef struct VMArenaStruct *VMArena;
typedef struct PageStruct *Page;


/* VMArenaStruct -- VM Arena Structure */

#define VMArenaSig      ((Sig)0x519A6EB3) /* SIGnature AREna VM */
/* @@@@ Arbitrary calculation for the maximum number of distinct */
/* object sets for generations. */
#define VMArenaGenCount ((Count)(MPS_WORD_WIDTH/2))

typedef struct VMArenaStruct {  /* VM arena structure */
  ArenaStruct arenaStruct;      /* generic arena structure */
  VM vm;                        /* virtual memory handle */
  Addr base;                    /* base address of arena area */
  Addr limit;                   /* limit address of arena area */
  Size pageSize;                /* size of block managed by PageStruct */
  Shift pageShift;              /* log2 of page size, for shifts */
  Index pages;                  /* number of pages in table */
  Page pageTable;               /* the page table */
  BT allocTable;                /* page allocation table */
  Size tablesSize;              /* size of area occupied by tables */
  Index tablePages;             /* number of pages occupied by tables */
  RefSet blacklist;             /* zones to use last */
  RefSet genRefSet[VMArenaGenCount];  /* zones assigned to generations */
  RefSet freeSet;               /* unassigned zones */
  Sig sig;                      /* design.mps.sig */
} VMArenaStruct;


/* ArenaVMArena -- find the VMArena pointer given a generic Arena */

#define ArenaVMArena(arena) PARENT(VMArenaStruct, arenaStruct, (arena))


/* VMArenaArena -- find the generic Arena pointer given a VMArena */

#define VMArenaArena(VMArena) (&(VMArena)->arenaStruct)


/* SegVMArena -- find the VMArena given a segment */

#define SegVMArena(seg) ArenaVMArena(PoolArena(SegPool(seg)))


/* PageStruct -- page structure
 *
 * .page-table: The page table (defined as a PageStruct array)
 * is central to the design of the arena.
 * See design.mps.arena.vm.table.*.
 *
 * .page: The "pool" field must be the first field of the "tail"
 * field of this union, so that it shares a common prefix with the
 * SegStruct.  See impl.h.mpmst.seg.pool.
 */

typedef struct PageStruct {     /* page structure */
  union {
    SegStruct segStruct;         /* segment */
    struct {
      Pool pool;                 /* NULL, must be first field (.page) */
      Seg seg;                   /* segment at base page of run */
      Addr limit;                /* limit of segment */
    } tail;                      /* tail page */
  } the;
} PageStruct;


/* PageIndexBase -- map page index to base address of page
 *
 * See design.mps.arena.vm.table.linear
 */

#define PageIndexBase(vmArena, i) \
  AddrAdd((vmArena)->base, ((i) << (vmArena)->pageShift))


/* PageSeg -- segment descriptor of a page */

#define PageSeg(page)           (&(page)->the.segStruct)


/* PageTail -- tail descriptor of a page */

#define PageTail(page)          (&(page)->the.tail)


/* PageOfSeg -- page descriptor from segment */

#define PageOfSeg(seg)          PARENT(PageStruct, the.segStruct, (seg))


/* PageIsHead -- is a page a head (contains segment descriptor)?
 *
 * See design.mps.arena.vm.table.disc.
 */

#define PageIsHead(page)        (PageTail(page)->pool != NULL)


/* addrPageBase -- the base of the page this address is on */

#define addrPageBase(vmArena, addr) \
  AddrAlignDown((addr), (vmArena)->pageSize)

#define addrOfPageDesc(vmArena, index) \
  ((Addr)&(vmArena)->pageTable[index])


static Addr VMSegLimit(Seg seg);


/* VMArenaCheck -- check the consistency of an arena structure */

static Bool VMArenaCheck(VMArena vmArena)
{
  Index gen;
  RefSet allocSet;

  CHECKS(VMArena, vmArena);
  CHECKD(Arena, VMArenaArena(vmArena));
  CHECKL(VMCheck(vmArena->vm));
  CHECKL(vmArena->base != (Addr)0);
  CHECKL(vmArena->base < vmArena->limit);
  CHECKL(ShiftCheck(vmArena->pageShift));
  CHECKL(vmArena->pageSize == 1uL << vmArena->pageShift);
  CHECKL(VMAlign(vmArena->vm) == vmArena->pageSize);
  CHECKL(vmArena->pages == 
         AddrOffset(vmArena->base, vmArena->limit) >> 
         vmArena->pageShift);
  CHECKL(vmArena->tablePages <= vmArena->pages);
  CHECKL(vmArena->tablesSize == vmArena->tablePages << 
         vmArena->pageShift);
  CHECKL(vmArena->pageTable != NULL);
  CHECKL((Addr)vmArena->pageTable >= vmArena->base);
  CHECKL((Addr)&vmArena->pageTable[vmArena->pages] <=
           AddrAdd(vmArena->base, vmArena->tablesSize));
  CHECKL(vmArena->allocTable != NULL);
  CHECKL((Addr)vmArena->allocTable >= vmArena->base);
  CHECKL(AddrAdd((Addr)vmArena->allocTable, BTSize(vmArena->pages))
         <= vmArena->limit);
  /* .improve.check-table: Could check the consistency of the tables. */

  allocSet = RefSetEMPTY;
  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    allocSet = RefSetUnion(allocSet, vmArena->genRefSet[gen]);
  }
  /* Might have allocated zones to non-generational pools */
  AVER(RefSetInter(allocSet, vmArena->freeSet) == RefSetEMPTY);

  return TRUE;
}


/* VMArenaInit -- create and initialize the VM arena
 *
 * .arena.alloc: The arena descriptor will be allocated at the base of
 * the arena, before the tables.
 * .arena.init: Once the arena has been allocated, we call ArenaInit
 * to do the generic part of init.
 */

static Res VMArenaInit(Arena *arenaReturn, va_list args)
{
  Size userSize;        /* size requested by user */
  Size arenaSize;       /* size actually created, as determined by VM */
  Res res;
  Size pageTableSize;
  VM vm;
  Addr base;
  VMArenaStruct initialArenaStruct;
  VMArena initArena = &initialArenaStruct;
  VMArena vmArena;
  Arena arena;
  Index gen;

  userSize = va_arg(args, Size);
  AVER(arenaReturn != NULL);
  AVER(userSize > 0);

  res = VMCreate(&vm, userSize);
  if(res != ResOK)
    goto failVMCreate;

  /* .arena.alloc */
  base = VMBase(vm);
  initArena->vm = vm;
  initArena->base = base;
  initArena->limit = VMLimit(vm);
  /* the VM will have aligned the userSize, so pick up the actual size */
  arenaSize = AddrOffset(initArena->base, initArena->limit);
  initArena->pageSize = VMAlign(vm);
  initArena->pageShift = SizeLog2(initArena->pageSize);
  initArena->pages = arenaSize >> initArena->pageShift;
  /* We generally assume that a page is aligned enough for any */
  /* normal object. */
  AVER(initArena->pageSize >= MPS_PF_ALIGN);

  /* .arena.tables: Place the tables at the base of the arena.  First*/
  /* the arena descriptor, immediately followed by the alloc table */
  /* (a bit table), and then at the next page boundary, the page */
  /* table (a PageStruct array).  Only the free table is mapped now. */
  /* .vm.addr-is-star: In this file, Addr is compatible with C */
  /* pointers. */
  initArena->allocTable =
    (BT)AddrAlignUp(AddrAdd(base, (Size)sizeof(VMArenaStruct)),
                    MPS_PF_ALIGN);
  initArena->pageTable = 
    (Page)AddrAlignUp(AddrAdd((Addr)initArena->allocTable,
    BTSize(initArena->pages)), initArena->pageSize);
  pageTableSize = SizeAlignUp(initArena->pages * sizeof(PageStruct),
                              initArena->pageSize);
  initArena->tablesSize = AddrOffset(base,
                                     AddrAdd((Addr)initArena->pageTable,
                                             pageTableSize));
  res = VMMap(vm, base, (Addr)initArena->pageTable);
  if(res != ResOK)
    goto failTableMap;

  /* Now that we've mapped the tables, copy in the stuff already */
  /* computed. */
  vmArena = (VMArena)base;
  *vmArena = *initArena;

  arena = VMArenaArena(vmArena);
  /* impl.c.arena.init.caller */
  ArenaInit(arena, (ArenaClass)mps_arena_class_vm());

  /* .tablepages: pages whose page index is < tablePages are */
  /* recorded as free but never allocated as alloc starts */
  /* searching after the tables (see .alloc.skip).  SegOfAddr */
  /* uses the fact that these pages are marked as free in order */
  /* to detect "references" to these pages as  being bogus see */
  /* .addr.free. */
  vmArena->tablePages = vmArena->tablesSize >> vmArena->pageShift;
  BTResRange(vmArena->allocTable, 0, vmArena->pages);

  /* Set the zone shift to divide the arena into the same number of */
  /* zones as will fit into a reference set (the number of bits in a */
  /* word).  Note that some zones are discontiguous in the arena if the */
  /* size is not a power of 2.  See design.mps.arena.class.fields. */
  arena->zoneShift = SizeFloorLog2(arenaSize >> MPS_WORD_SHIFT);
  arena->alignment = vmArena->pageSize;

  /* We blacklist the first and last zones because they commonly */
  /* correspond to low integers.  */
  /* @@@@ This should be dynamic. */
  vmArena->blacklist = 
    RefSetAdd(arena, RefSetAdd(arena, RefSetEMPTY, (Addr)1), (Addr)-1);

  for(gen = (Index)0; gen < VMArenaGenCount; gen++) {
    vmArena->genRefSet[gen] = RefSetEMPTY;
  }

  vmArena->freeSet = RefSetUNIV; /* includes blacklist */

  /* Sign and check the arena. */
  vmArena->sig = VMArenaSig;
  AVERT(VMArena, vmArena);

  EVENT_PP(ArenaCreate, vmArena, vm);

  *arenaReturn = arena;
  return ResOK;

failTableMap:
  VMDestroy(vm);
failVMCreate:
  return res;
}


/* VMArenaFinish -- finish the arena and destroy the VM */

static void VMArenaFinish(Arena arena)
{
  VMArena vmArena;
  VM vm;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  
  vmArena->sig = SigInvalid;
  vm = vmArena->vm;
  ArenaFinish(arena); /* impl.c.arena.finish.caller */
  VMUnmap(vm, vmArena->base, (Addr)vmArena->pageTable);
  VMDestroy(vm);

  EVENT_P(ArenaDestroy, vmArena);
}


/* VMArenaReserved -- return the amount of reserved address space
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaReserved(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return VMReserved(vmArena->vm);
}


/* VMArenaCommitted -- return the amount of committed virtual memory
 *
 * Since this is a VM-based arena, this information is retrieved from
 * the VM.
 */

static Size VMArenaCommitted(Arena arena)
{
  VMArena vmArena;

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return VMMapped(vmArena->vm);
}


/* indexOfAddr -- return the index of the page containing an address
 *
 * .index.addr: The address passed may be equal to the limit of the
 * arena, in which case the last page index plus one is returned.  (It
 * is, in a sense, the limit index of the page table.)
 */

#define INDEX_OF_ADDR(vmArena, addr) \
  (AddrOffset((vmArena)->base, (addr)) >> (vmArena)->pageShift)
static Index indexOfAddr(VMArena vmArena, Addr addr)
{
  AVERT(VMArena, vmArena);
  AVER(vmArena->base <= addr);
  AVER(addr <= vmArena->limit);   /* .index.addr */

  return INDEX_OF_ADDR(vmArena, addr);
}


/* findFreeInArea -- try to allocate a segment in an area
 *
 * Search for a free run of pages in the free table, but between
 * base and limit.
 */

static Bool findFreeInArea(Index *baseReturn,
                           VMArena vmArena, Size size,
                           Addr base, Addr limit)
{
  Word pages;                   /* number of pages equiv. to size */
  Index basePage, limitPage;	/* Index equiv. to base and limit */
  Index start, end;		/* base and limit of free run */

  AVER(baseReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(AddrIsAligned(base, vmArena->pageSize));
  AVER(AddrIsAligned(limit, vmArena->pageSize));
  AVER(vmArena->base <= base);
  AVER(base < limit);
  AVER(limit <= vmArena->limit);
  AVER(size <= AddrOffset(base, limit));
  AVER(size > (Size)0);
  AVER(SizeIsAligned(size, vmArena->pageSize));

  basePage = indexOfAddr(vmArena, base);
  limitPage = indexOfAddr(vmArena, limit);
  pages = size >> vmArena->pageShift;

  if(!BTFindShortResRange(&start, &end,
                          vmArena->allocTable,
                          basePage, limitPage,
                          pages))
    return FALSE;

  *baseReturn = start;
  return TRUE;
}


/* findFreeInRefSet -- try to allocate a segment with a RefSet
 * 
 * This function finds the intersection of refSet and the set of free
 * pages and tries to find a free run of pages in the resulting set of
 * areas.
 *
 * In other words, it finds space for a segment whose RefSet (see
 * RefSetOfSeg) will be a subset of the specified RefSet.
 */

static Bool findFreeInRefSet(Index *baseReturn,
			     VMArena vmArena, Size size, RefSet refSet)
{
  Arena arena;
  Addr arenaBase, base, limit;
  Size zoneSize;

  AVER(baseReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(size > 0);
  /* Can't check refSet */

  arena = VMArenaArena(vmArena);
  zoneSize = (Size)1 << arena->zoneShift;
  /* .alloc.skip: The first address available for segments, */
  /* is just after the arena tables. */
  arenaBase = PageIndexBase(vmArena, vmArena->tablePages);

  base = arenaBase;
  while(base < vmArena->limit) {
  
    if(RefSetIsMember(arena, refSet, base)) {
      /* Search for a run of zone stripes which are in the RefSet and */
      /* the arena.  Adding the zoneSize might wrap round (to zero, */
      /* because limit is aligned to zoneSize, which is a power of two). */
      limit = base;
      do {
        limit = AddrAlignDown(AddrAdd(limit, zoneSize), zoneSize);

        AVER(limit > base || limit == (Addr)0);

        if(limit >= vmArena->limit || limit < base) {
          limit = vmArena->limit;
          break;
        }

        AVER(base < limit && limit < vmArena->limit);
      } while(RefSetIsMember(arena, refSet, limit));

      /* If the RefSet was universal, then the area found ought to */
      /* be the whole arena. */
      AVER(refSet != RefSetUNIV ||
           (base == arenaBase && limit == vmArena->limit));

      /* Try to allocate a segment in the area. */
      if(AddrOffset(base, limit) >= size &&
         findFreeInArea(baseReturn, vmArena, size, base, limit))
        return TRUE;
      
      base = limit;
    } else {
      /* Adding the zoneSize might wrap round (to zero, because base */
      /* is aligned to zoneSize, which is a power of two). */
      base = AddrAlignDown(AddrAdd(base, zoneSize), zoneSize);
      AVER(base > arenaBase || base == (Addr)0);
      if(base >= vmArena->limit || base < arenaBase) {
        base = vmArena->limit;
        break;
      }
    }
  }

  AVER(base == vmArena->limit);

  return FALSE;
}


/* tablePageBaseIndex -- index of the first page descriptor falling
 *                       (at least partially) on this table page
 *
 * .repr.table-page: Table pages are passed as the base address of the page.
 *
 * .division: We calculate it by dividing the offset from the beginning
 * of the page table by the size of a table element.  This relies on
 * .vm.addr-is-star.
 */

#define tablePageBaseIndex(vmArena, tablePage) \
  (AddrOffset((Addr)(vmArena)->pageTable, (tablePage)) \
   / sizeof(PageStruct))


/* tablePageLimitIndex -- index of the first page descriptor falling
 *                        (wholly) the next table page
 *
 * Similar to tablePageBaseIndex, see .repr.table-page and .division.
 */

#define tablePageLimitIndex(vmArena, tablePage) \
  ((AddrOffset((Addr)(vmArena)->pageTable, (tablePage)) \
    + (vmArena)->pageSize - 1) \
   / sizeof(PageStruct) \
   + 1)


/* tablePageInUse -- check whether a given page of the page table is in use
 *
 * Returns TRUE if and only if the table page given is in use, i.e., if any
 * of the page descriptors falling on it (even partially) are being used.
 * Relies on .repr.table-page and .vm.addr-is-star.
 *
 * .improve.limits: We don't need to check the parts we're (de)allocating.
 */

static Bool tablePageInUse(VMArena vmArena, Addr tablePage)
{
  AVERT(VMArena, vmArena);
  /* Check it's in the page table. */
  AVER((Addr)&vmArena->pageTable[0] <= tablePage);
  AVER(tablePage < (Addr)&vmArena->pageTable[vmArena->pages]);

  return !BTIsResRange(vmArena->allocTable,
		       tablePageBaseIndex(vmArena, tablePage),
		       tablePageLimitIndex(vmArena, tablePage));
}


/* unusedTablePages -- find any unused pages occupied by the descriptors given
 *
 * .unused: The caller guarantees the pages between baseIndex and
 * limitIndex are free, so those descriptors aren't being used.
 * .used.first-and-last: Since the descriptors given are not being used
 * at the moment, only the first page and the last page could be
 * partially used, the rest (if any) can be assumed to be unused.
 */

static Bool unusedTablePages(Addr *pagesBaseReturn, Addr *pagesLimitReturn,
			     VMArena vmArena, Index baseIndex, Index limitIndex)
{
  Addr firstPageBase, lastPageBase, pagesBase, pagesLimit;

  AVERT(VMArena, vmArena);
  AVER(baseIndex < limitIndex && limitIndex <= vmArena->pages);
  AVER(BTIsResRange(vmArena->allocTable, baseIndex, limitIndex));
  AVER(pagesBaseReturn != NULL);
  AVER(pagesLimitReturn != NULL);

  /* firstPageBase is the base address of the table page that contains the */
  /* (first byte of the) page descriptor for baseIndex. */
  firstPageBase = addrPageBase(vmArena, addrOfPageDesc(vmArena, baseIndex));

  /* lastPageBase is the base address of the table page that contains the */
  /* (last byte of the) page descriptor for the page before limitIndex. */
  lastPageBase = addrPageBase(vmArena,
			      AddrAdd(addrOfPageDesc(vmArena, limitIndex-1),
				      sizeof(PageStruct) - 1));

  /* If there is only one page involved, just check whether it is */
  /* used.  This is the common case, since it's unlikely that */
  /* many page descriptors will be allocated or freed at once. */
  if(firstPageBase == lastPageBase) {
    if(tablePageInUse(vmArena, firstPageBase)) {
      return FALSE;
    } else {
      *pagesBaseReturn = firstPageBase;
      *pagesLimitReturn = AddrAdd(firstPageBase, vmArena->pageSize);
      return TRUE;
    }
  }

  /* If the page containing the page descriptor for baseIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(vmArena, firstPageBase)) {
    pagesBase = AddrAdd(firstPageBase, vmArena->pageSize);
  } else {
    pagesBase = firstPageBase;
  }

  /* If the page containing the page descriptor for limitIndex */
  /* is in use, exclude it. */
  if(tablePageInUse(vmArena, lastPageBase)) {
    pagesLimit = lastPageBase;
  } else {
    pagesLimit = AddrAdd(lastPageBase, vmArena->pageSize);
  }

  /* If the pages were adjacent, and both excluded, then there */
  /* is nothing left. */
  if (pagesBase == pagesLimit) {
    return FALSE;
  } else {
    *pagesBaseReturn = pagesBase;
    *pagesLimitReturn = pagesLimit;
    return TRUE;
  }
}


/* VMSegAlloc -- allocate a segment from the arena */

static Res VMSegAlloc(Seg *segReturn, SegPref pref, Size size,
		      Pool pool)
{
  VMArena vmArena;
  Arena arena;
  Index i, pages, base;
  Addr addr, unmappedPagesBase, unmappedPagesLimit;
  Seg seg;
  Res res;
  RefSet refSet, segRefSet;
  Serial gen = (Serial)0; /* avoids incorrect warning */

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);
  AVER(SizeIsAligned(size, vmArena->pageSize));
  
  /* NULL is used as a discriminator */
  /* (see design.mps.arena.vm.table.disc) therefore the real pool */
  /* must be non-NULL. */
  AVER(pool != NULL);

  if(pref->isGen) {
    gen = pref->gen;
    if(gen >= VMArenaGenCount)
      gen = VMArenaGenCount - 1;
    refSet = vmArena->genRefSet[gen];
  } else {
    refSet = pref->refSet;
  }
   
  /* @@@@ Some of these tests might be duplicates.  If we're about */
  /* to run out of virtual address space, then slow allocation is */
  /* probably the least of our worries. */

  if(pref->isCollected) { /* GC'd segment */
    /* We look for space in the following places (in order) */
    /*   - Zones already allocated to me (refSet) but are not */
    /*     blacklisted; */
    /*   - Zones that are either allocated to me, or are unallocated */
    /*     but not blacklisted; */
    /*   - Any non-blacklisted zone; */
    /*   - Any zone; */
    /* Note that each is a superset of the previous, unless blacklisted */
    /* zones have been allocated (or the default is used). */
    if(!findFreeInRefSet(&base, vmArena, size, 
			 RefSetDiff(refSet, vmArena->blacklist)) &&
       !findFreeInRefSet(&base, vmArena, size, 
                         RefSetUnion(refSet,
				     RefSetDiff(vmArena->freeSet, 
						vmArena->blacklist))) && 
       !findFreeInRefSet(&base, vmArena, size, 
			 RefSetDiff(RefSetUNIV, vmArena->blacklist)) && 
       !findFreeInRefSet(&base, vmArena, size, RefSetUNIV)) {
      /* .improve.alloc-fail: This could be because the request was */
      /* too large, or perhaps the arena is fragmented.  We could return a */
      /* more meaningful code. */
      return ResRESOURCE;
    }
  } else { /* non-GC'd segment */
    /* We look for space in the following places (in order) */
    /*   - Zones preferred (refSet) and blacklisted; */
    /*   - Zones preferred; */
    /*   - Zones preferred or blacklisted zone; */
    /*   - Any zone. */
    /* Note that each is a superset of the previous, unless blacklisted */
    /* zones have been allocated. */
    if(!findFreeInRefSet(&base, vmArena, size, 
			 RefSetInter(refSet, vmArena->blacklist)) &&
       !findFreeInRefSet(&base, vmArena, size, refSet) && 
       !findFreeInRefSet(&base, vmArena, size, 
			 RefSetUnion(refSet, vmArena->blacklist)) && 
       !findFreeInRefSet(&base, vmArena, size, RefSetUNIV)) {
      return ResRESOURCE;
    }
  }

  /* .alloc.early-map: Map in the segment memory before actually */
  /* allocating the pages, so that we can exit straight away if */
  /* we fail. */
  addr = PageIndexBase(vmArena, base);
  res = VMMap(vmArena->vm, addr, AddrAdd(addr, size));
  if(res != ResOK)
    goto failSegMap;

  /* Compute number of pages to be allocated. */
  pages = size >> vmArena->pageShift;

  /* Ensure that the page descriptors we need are on mapped pages. */
  if(unusedTablePages(&unmappedPagesBase, &unmappedPagesLimit,
		      vmArena, base, base + pages)) {
    res = VMMap(vmArena->vm, unmappedPagesBase, unmappedPagesLimit);
    if(res != ResOK)
      goto failTableMap;
  }

  /* Initialize the generic segment structure. */
  seg = PageSeg(&vmArena->pageTable[base]);
  SegInit(seg, pool);

  /* Allocate the first page, and, if there is more than one page, */
  /* allocate the rest of the pages and store the multi-page information */
  /* in the page table. */
  AVER(!BTGet(vmArena->allocTable, base));
  BTSet(vmArena->allocTable, base);
  if(pages > 1) {
    Addr limit = PageIndexBase(vmArena, base + pages);

    SegSetSingle(seg, FALSE);
    for(i = base + 1; i < base + pages; ++i) {
      AVER(!BTGet(vmArena->allocTable, i));
      BTSet(vmArena->allocTable, i);
      PageTail(&vmArena->pageTable[i])->pool = NULL;
      PageTail(&vmArena->pageTable[i])->seg = seg;
      PageTail(&vmArena->pageTable[i])->limit = limit;
    }
  } else
    SegSetSingle(seg, TRUE);

  segRefSet = RefSetOfSeg(arena, seg);

  if(pref->isGen)
    vmArena->genRefSet[gen] = 
      RefSetUnion(vmArena->genRefSet[gen], segRefSet);

  vmArena->freeSet = RefSetDiff(vmArena->freeSet, segRefSet);
  
  AVERT(Seg, seg);
  
  EVENT_PPAWP(SegAlloc, vmArena, seg, addr, size, pool);

  *segReturn = seg;
  return ResOK;

failTableMap:
  VMUnmap(vmArena->vm, addr, AddrAdd(addr, size));
failSegMap:
  return res;
}


/* VMSegFree - free a segment in the arena
 *
 * .seg-free.alt: It is possible to re-implement this function without
 * making a call to VMSegLimit, by searching for the end of the segment
 * in .free.loop.
 */

static void VMSegFree(Seg seg)
{
  VMArena vmArena;
  Page page;
  Count pages;
  Index basePage;
  Addr base, limit, unusedPagesBase, unusedPagesLimit;

  AVERT(Seg, seg);
  vmArena = SegVMArena(seg);
  AVERT(VMArena, vmArena);

  page = PageOfSeg(seg);
  limit = VMSegLimit(seg);
  basePage = page - vmArena->pageTable;
  AVER(basePage <= vmArena->pages);

  SegFinish(seg);

  base = PageIndexBase(vmArena, basePage);
  VMUnmap(vmArena->vm, base, limit);

  /* Calculate the number of pages in the segment */
  pages = AddrOffset(base, limit) >> vmArena->pageShift;

  /* There shouldn't be any pages marked free within the segment's */
  /* area of the alloc table. */
  AVER(BTIsSetRange(vmArena->allocTable, basePage, basePage + pages));
  BTResRange(vmArena->allocTable, basePage, basePage + pages);

  /* Unmap any pages that became unused in the page table */
  if(unusedTablePages(&unusedPagesBase, &unusedPagesLimit,
		      vmArena, basePage, basePage + pages))
    VMUnmap(vmArena->vm, unusedPagesBase, unusedPagesLimit);

  EVENT_PP(SegFree, vmArena, seg);
}

/* .seg.critical: These Seg functions are low-level and are on 
 * the critical path in various ways.  The more common therefore 
 * use AVER_CRITICAL
 */

/* VMSegBase -- return the base address of a segment
 *
 * The segment base is calculated by working out the index of the
 * segment structure in the page table and then returning the
 * base address of that page.
 */

static Addr VMSegBase(Seg seg)
{
  VMArena vmArena;
  Page page;
  Index i;
  
  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);

  page = PageOfSeg(seg);
  i = page - vmArena->pageTable;

  return PageIndexBase(vmArena, i);
}


/* VMSegLimit -- return the limit address (end+1) of a segment
 *
 * If the segment is a single page, then the limit is just
 * the next page, otherwise it is stored on the next page
 * table entry.
 */

static Addr VMSegLimit(Seg seg)
{
  VMArena vmArena;
  Page page;

  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);

  if(SegSingle(seg)) {
    return AddrAdd(VMSegBase(seg), vmArena->pageSize);
  } else {
    page = PageOfSeg(seg);
    return PageTail(page+1)->limit;
  }
}


/* VMSegSize -- return the size (limit - base) of a segment
 *
 */

static Size VMSegSize(Seg seg)
{
  VMArena vmArena;
  Page page;
  Index i;
  Addr base, limit;

  AVERT_CRITICAL(Seg, seg); /* .seg.critical */

  vmArena = SegVMArena(seg);
  AVERT_CRITICAL(VMArena, vmArena);

  page = PageOfSeg(seg);
  i = page - vmArena->pageTable;
  base = PageIndexBase(vmArena, i);

  if(SegSingle(seg)) {
    limit = AddrAdd(VMSegBase(seg), vmArena->pageSize);
  } else {
    page = PageOfSeg(seg);
    limit = PageTail(page+1)->limit;
  }

  return AddrOffset(base, limit);
}


/* VMSegOfAddr -- return the segment which encloses an address
 *
 * If the address is within the bounds of the arena, calculate the
 * page table index from the address and see if the page is allocated.
 * If it is a head page, return the segment, otherwise follow the
 * tail's pointer back to the segment in the head page.
 */

static Bool VMSegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  VMArena vmArena;
  
  /* design.mps.trace.fix.noaver */
  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  vmArena = ArenaVMArena(arena);
  AVERT_CRITICAL(VMArena, vmArena);
  
  if(vmArena->base <= addr && addr < vmArena->limit) {
    /* design.mps.trace.fix.segofaddr */
    Index i = INDEX_OF_ADDR(vmArena, addr);
    /* .addr.free: If the page is recorded as being free then */
    /* either the page is free or it is */
    /* part of the arena tables (see .tablepages) */
    if(BTGet(vmArena->allocTable, i)) {
      Page page = &vmArena->pageTable[i];

      if(PageIsHead(page))
        *segReturn = PageSeg(page);
      else
        *segReturn = PageTail(page)->seg;
      return TRUE;
    }
  }
  
  return FALSE;
}

static Bool VMIsReservedAddr(Arena arena, Addr addr)
{
  VMArena vmArena;

  AVERT(Arena, arena);
  /* addr is arbitrary */

  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  return vmArena->base <= addr && addr < vmArena->limit;
}


/* segSearch -- search for a segment
 *
 * .seg-search: Searches for a segment in the arena starting at page
 * index i, return NULL if there is none.  A page is the first page
 * of a segment if it is marked allocated in the allocTable, and
 * its pool is not NULL.
 *
 * .seg-search.private: This function is private to this module and
 * is used in the segment iteration protocol (SegFirst and SegNext).
 */

static Bool segSearch(Seg *segReturn, VMArena vmArena, Index i)
{
  AVER(segReturn != NULL);
  AVERT(VMArena, vmArena);
  AVER(vmArena->tablePages <= i);
  AVER(i <= vmArena->pages);

  while(i < vmArena->pages &&
        !(BTGet(vmArena->allocTable, i) &&
          PageIsHead(&vmArena->pageTable[i])))
    ++i;

  if(i == vmArena->pages)
    return FALSE;
  
  AVER(i < vmArena->pages);
  
  *segReturn = PageSeg(&vmArena->pageTable[i]);
  return TRUE;
}


/* VMSegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

static Bool VMSegFirst(Seg *segReturn, Arena arena)
{
  VMArena vmArena;

  AVER(segReturn != NULL);
  vmArena = ArenaVMArena(arena);
  AVERT(VMArena, vmArena);

  /* We start from tablePages, as the tables can't be a segment.
   * See .tablepages */
  return segSearch(segReturn, vmArena, vmArena->tablePages);
}


/* VMSegNext -- return the "next" segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 *
 * VMSegNext finds the segment with the lowest base address which is
 * greater than a specified address.  The address must be (or must once
 * have been) the base address of a segment.
 */

static Bool VMSegNext(Seg *segReturn, Arena arena, Addr addr)
{
  VMArena vmArena;
  Index i;

  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  vmArena = ArenaVMArena(arena);
  AVERT_CRITICAL(VMArena, vmArena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  i = indexOfAddr(vmArena, addr);

  /* There are fewer pages than addresses, therefore the */
  /* page index can never wrap around */
  AVER_CRITICAL(i+1 != 0);

  return segSearch(segReturn, vmArena, i + 1);
}


/* mps_arena_class_vm -- return the arena class VM */

static ArenaClassStruct ArenaClassVMStruct = {
  ArenaClassSig,
  "VM",                                 /* name */
  sizeof(VMArenaStruct),                /* size */
  offsetof(VMArenaStruct, arenaStruct), /* offset */
  VMArenaInit,                          /* init */
  VMArenaFinish,                        /* finish */
  VMArenaReserved,                      /* reserved */
  VMArenaCommitted,                     /* committed */
  ArenaNoExtend,                        /* extend */
  ArenaNoRetract,                       /* retract */
  VMIsReservedAddr,                     /* isReserved */
  VMSegAlloc,                           /* segAlloc */
  VMSegFree,                            /* segFree */
  VMSegBase,                            /* segBase */
  VMSegLimit,                           /* segLimit */
  VMSegSize,                            /* segSize */
  VMSegOfAddr,                          /* segOfAddr */
  VMSegFirst,                           /* segFirst */
  VMSegNext,                            /* segNext */
  ArenaTrivDescribe,                    /* describe */
  ArenaClassSig
};

mps_arena_class_t mps_arena_class_vm(void)
{
  return (mps_arena_class_t)&ArenaClassVMStruct;
}
