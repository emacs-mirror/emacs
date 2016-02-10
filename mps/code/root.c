/* root.c: ROOT IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This is the implementation of the root datatype.
 *
 * .design: For design, see <design/root/> and
 * design.mps.root-interface. */

#include "mpm.h"

SRCID(root, "$Id$");


/* RootStruct -- tracing root structure */

#define RootSig         ((Sig)0x51960029) /* SIGnature ROOT */

typedef struct RootStruct {
  Sig sig;
  Serial serial;                /* from arena->rootSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attachment to arena */
  Rank rank;                    /* rank of references in this root */
  TraceSet grey;                /* traces for which root is grey */
  RefSet summary;               /* summary of references in root */
  RootMode mode;                /* mode */
  Bool protectable;             /* Can protect root? */
  Addr protBase;                /* base of protectable area */
  Addr protLimit;               /* limit of protectable area */
  AccessSet pm;                 /* Protection Mode */
  RootVar var;                  /* union discriminator */
  union RootUnion {
    struct {
      mps_root_scan_t scan;     /* the function which does the scanning */
      void *p;                  /* environment for scan */
      size_t s;                 /* environment for scan */
    } fun;
    struct {
      Word *base;               /* beginning of table */
      Word *limit;              /* one off end of table */
    } table;
    struct {
      Word *base;               /* beginning of table */
      Word *limit;              /* one off end of table */
      Word mask;                /* tag mask for scanning */
      Word pattern;             /* tag pattern for scanning */
    } tableMasked;
    struct {
      mps_reg_scan_t scan;      /* function for scanning registers */
      Thread thread;            /* passed to scan */
      void *p;                  /* passed to scan */
      size_t s;                 /* passed to scan */
    } reg;
    struct {
      Thread thread;            /* passed to scan */
      Word mask;                /* tag mask for scanning */
      Word pattern;             /* tag pattern for scanning */
      Word *stackBot;           /* bottom of stack */
    } regMasked;
    struct {
      mps_fmt_scan_t scan;      /* format-like scanner */
      Addr base, limit;         /* passed to scan */
    } fmt;
  } the;
} RootStruct;


/* RootVarCheck -- check a Root union discriminator
 *
 * .rootvarcheck: Synchronize with <code/mpmtypes.h#rootvar> */

Bool RootVarCheck(RootVar rootVar)
{
  CHECKL(rootVar == RootTABLE || rootVar == RootTABLE_MASKED
         || rootVar == RootFUN || rootVar == RootFMT || rootVar == RootREG
         || rootVar == RootREG_MASKED);
  UNUSED(rootVar);
  return TRUE;
}


/* RootModeCheck */

Bool RootModeCheck(RootMode mode)
{
  CHECKL((mode & (RootModeCONSTANT | RootModePROTECTABLE
                  | RootModePROTECTABLE_INNER))
         == mode);
  /* RootModePROTECTABLE_INNER implies RootModePROTECTABLE */
  CHECKL((mode & RootModePROTECTABLE_INNER) == 0
         || (mode & RootModePROTECTABLE));
  UNUSED(mode);

  return TRUE;
}


/* RootCheck -- check the consistency of a root structure
 *
 * .rootcheck: Keep synchonized with <code/mpmst.h#root>. */

Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Arena, root->arena);
  CHECKL(root->serial < ArenaGlobals(root->arena)->rootSerial);
  CHECKD_NOSIG(Ring, &root->arenaRing);
  CHECKL(RankCheck(root->rank));
  CHECKL(TraceSetCheck(root->grey));
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(root->the.table.base != 0);
    CHECKL(root->the.table.base < root->the.table.limit);
    break;

    case RootTABLE_MASKED:
    CHECKL(root->the.tableMasked.base != 0);
    CHECKL(root->the.tableMasked.base < root->the.tableMasked.limit);
    CHECKL((~root->the.tableMasked.mask & root->the.tableMasked.pattern) == 0);
    break;

    case RootFUN:
    CHECKL(root->the.fun.scan != NULL);
    break;

    case RootREG:
    CHECKL(root->the.reg.scan != NULL);
    CHECKD_NOSIG(Thread, root->the.reg.thread); /* <design/check/#hidden-type> */
    /* Can't check anything about p or s. */
    break;

    case RootREG_MASKED:
    CHECKD_NOSIG(Thread, root->the.regMasked.thread); /* <design/check/#hidden-type> */
    CHECKL((~root->the.regMasked.mask & root->the.regMasked.pattern) == 0);
    /* Can't check anything about stackBot. */
    break;

    case RootFMT:
    CHECKL(root->the.fmt.scan != NULL);
    CHECKL(root->the.fmt.base != 0);
    CHECKL(root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED;
  }
  CHECKL(RootModeCheck(root->mode));
  CHECKL(BoolCheck(root->protectable));
  if (root->protectable) {
    CHECKL(root->protBase != (Addr)0);
    CHECKL(root->protLimit != (Addr)0);
    CHECKL(root->protBase < root->protLimit);
    CHECKL(AccessSetCheck(root->pm));
  } else {
    CHECKL(root->protBase == (Addr)0);
    CHECKL(root->protLimit == (Addr)0);
    CHECKL(root->pm == (AccessSet)0);
  }
  return TRUE;
}


/* rootCreate, RootCreateTable, RootCreateReg, RootCreateFmt, RootCreateFun
 *
 * RootCreate* set up the appropriate union member, and call the generic
 * create function to do the actual creation
 *
 * See <design/root/#init> for initial value. */

static Res rootCreate(Root *rootReturn, Arena arena,
                      Rank rank, RootMode mode, RootVar type,
                      union RootUnion *theUnionP)
{
  Root root;
  Res res;
  void *p;
  Globals globals;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVERT(RootVar, type);
  globals = ArenaGlobals(arena);

  res = ControlAlloc(&p, arena, sizeof(RootStruct), FALSE);
  if (res != ResOK)
    return res;
  root = (Root)p; /* Avoid pun */

  root->arena = arena;
  root->rank = rank;
  root->var = type;
  root->the  = *theUnionP;
  root->grey = TraceSetEMPTY;
  root->summary = RefSetUNIV;
  root->mode = mode;
  root->pm = AccessSetEMPTY;
  root->protectable = FALSE;
  root->protBase = (Addr)0;
  root->protLimit = (Addr)0;

  /* See <design/arena/#root-ring> */
  RingInit(&root->arenaRing);

  root->serial = globals->rootSerial;
  ++globals->rootSerial;
  root->sig = RootSig;

  AVERT(Root, root);

  RingAppend(&globals->rootRing, &root->arenaRing);

  *rootReturn = root;
  return ResOK;
}

static Res rootCreateProtectable(Root *rootReturn, Arena arena,
                                 Rank rank, RootMode mode, RootVar var,
                                 Addr base, Addr limit,
                                 union RootUnion *theUnion)
{
  Res res;
  Root root;
  Ring node, next;

  res = rootCreate(&root, arena, rank, mode, var, theUnion);
  if (res != ResOK)
    return res;
  if (mode & RootModePROTECTABLE) {
    root->protectable = TRUE;
    if (mode & RootModePROTECTABLE_INNER) {
      root->protBase = AddrArenaGrainUp(base, arena);
      root->protLimit = AddrArenaGrainDown(limit, arena);
      if (!(root->protBase < root->protLimit)) {
        /* root had no inner pages */
        root->protectable = FALSE;
        root->mode &=~ (RootModePROTECTABLE|RootModePROTECTABLE_INNER);
      }
    } else {
      root->protBase = AddrArenaGrainDown(base, arena);
      root->protLimit = AddrArenaGrainUp(limit, arena);
    }
  }

  /* Check that this root doesn't intersect with any other root */
  RING_FOR(node, &ArenaGlobals(arena)->rootRing, next) {
    Root trial = RING_ELT(Root, arenaRing, node);
    if (trial != root) {
      /* (trial->protLimit <= root->protBase */
      /*  || root->protLimit <= trial->protBase) */
      /* is the "okay" state.  The negation of this is: */
      if (root->protBase < trial->protLimit
          && trial->protBase < root->protLimit) {
        NOTREACHED;
        RootDestroy(root);
        return ResFAIL;
      }
    }
  }

  AVERT(Root, root);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Arena arena,
                    Rank rank, RootMode mode, Word *base, Word *limit)
{
  Res res;
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVER(base != 0);
  AVER(AddrIsAligned(base, sizeof(Word)));
  AVER(base < limit);
  AVER(AddrIsAligned(limit, sizeof(Word)));

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  res = rootCreateProtectable(rootReturn, arena, rank, mode,
                              RootTABLE, (Addr)base, (Addr)limit, &theUnion);
  return res;
}

Res RootCreateTableMasked(Root *rootReturn, Arena arena,
                          Rank rank, RootMode mode, Word *base, Word *limit,
                          Word mask)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVER(base != 0);
  AVER(base < limit);
  /* Can't check anything about mask. */

  theUnion.tableMasked.base = base;
  theUnion.tableMasked.limit = limit;
  theUnion.tableMasked.mask = mask;
  theUnion.tableMasked.pattern = 0;

  return rootCreateProtectable(rootReturn, arena, rank, mode, RootTABLE_MASKED,
                               (Addr)base, (Addr)limit, &theUnion);
}

Res RootCreateReg(Root *rootReturn, Arena arena,
                  Rank rank, Thread thread,
                  mps_reg_scan_t scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVERT(Thread, thread);
  AVER(ThreadArena(thread) == arena);
  AVER(scan != NULL);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;
  theUnion.reg.s = s;

  return rootCreate(rootReturn, arena, rank, (RootMode)0, RootREG, &theUnion);
}

Res RootCreateRegMasked(Root *rootReturn, Arena arena,
                        Rank rank, Thread thread,
                        Word mask, Word pattern, Word *stackBot)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVERT(Thread, thread);
  AVER(ThreadArena(thread) == arena);
  AVER((~mask & pattern) == 0);

  theUnion.regMasked.thread = thread;
  theUnion.regMasked.mask = mask;
  theUnion.regMasked.pattern = pattern;
  theUnion.regMasked.stackBot = stackBot;

  return rootCreate(rootReturn, arena, rank, (RootMode)0, RootREG_MASKED,
                    &theUnion);
}

/* RootCreateFmt -- create root from block of formatted objects
 *
 * .fmt.no-align-check: Note that we don't check the alignment of base
 * and limit. That's because we're only given the scan function, so we
 * don't know the format's alignment requirements.
 */

Res RootCreateFmt(Root *rootReturn, Arena arena,
                  Rank rank, RootMode mode, mps_fmt_scan_t scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return rootCreateProtectable(rootReturn, arena, rank, mode,
                               RootFMT, base, limit, &theUnion);
}

Res RootCreateFun(Root *rootReturn, Arena arena, Rank rank,
                  mps_root_scan_t scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVER(FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return rootCreate(rootReturn, arena, rank, (RootMode)0, RootFUN, &theUnion);
}


/* RootDestroy -- destroy a root */

void RootDestroy(Root root)
{
  Arena arena;

  AVERT(Root, root);

  arena = RootArena(root);

  AVERT(Arena, arena);

  RingRemove(&root->arenaRing);
  RingFinish(&root->arenaRing);

  root->sig = SigInvalid;

  ControlFree(arena, root, sizeof(RootStruct));
}


/* RootArena -- return the arena of a root
 *
 * Must be thread-safe. See <design/interface-c/#check.testt> */

Arena RootArena(Root root)
{
  AVER(TESTT(Root, root));
  return root->arena;
}


/* RootRank -- return the rank of a root */

Rank RootRank(Root root)
{
  AVERT(Root, root);
  return root->rank;
}


/* RootPM -- return the protection mode of a root */

AccessSet RootPM(Root root)
{
  AVERT(Root, root);
  return root->pm;
}


/* RootSummary -- return the summary of a root */

RefSet RootSummary(Root root)
{
  AVERT(Root, root);
  return root->summary;
}


/* RootGrey -- mark root grey */

void RootGrey(Root root, Trace trace)
{
  AVERT(Root, root);
  AVERT(Trace, trace);
 
  root->grey = TraceSetAdd(root->grey, trace);
}


static void rootSetSummary(Root root, RefSet summary)
{
  AVERT(Root, root);
  /* Can't check summary */
  if (root->protectable) {
    if (summary == RefSetUNIV) {
      root->summary = summary;
      root->pm &= ~AccessWRITE;
    } else {
      root->pm |= AccessWRITE;
      root->summary = summary;
    }
  } else
    AVER(root->summary == RefSetUNIV);
}


/* RootScan -- scan root */

Res RootScan(ScanState ss, Root root)
{
  Res res;

  AVERT(Root, root);
  AVERT(ScanState, ss);
  AVER(root->rank == ss->rank);

  if (TraceSetInter(root->grey, ss->traces) == TraceSetEMPTY)
    return ResOK;

  AVER(ScanStateSummary(ss) == RefSetEMPTY);

  if (root->pm != AccessSetEMPTY) {
    ProtSet(root->protBase, root->protLimit, AccessSetEMPTY);
  }

  switch(root->var) {
    case RootTABLE:
    res = TraceScanArea(ss, root->the.table.base, root->the.table.limit);
    ss->scannedSize += AddrOffset(root->the.table.base, root->the.table.limit);
    if (res != ResOK)
      goto failScan;
    break;

    case RootTABLE_MASKED:
    res = TraceScanAreaMasked(ss,
                              root->the.tableMasked.base,
                              root->the.tableMasked.limit,
                              root->the.tableMasked.mask,
                              root->the.tableMasked.pattern);
    ss->scannedSize += AddrOffset(root->the.table.base, root->the.table.limit);
    if (res != ResOK)
      goto failScan;
    break;

    case RootFUN:
    res = (*root->the.fun.scan)(&ss->ss_s, root->the.fun.p, root->the.fun.s);
    if (res != ResOK)
      goto failScan;
    break;

    case RootREG:
    res = (*root->the.reg.scan)(&ss->ss_s, root->the.reg.thread,
                                root->the.reg.p, root->the.reg.s);
    if (res != ResOK)
      goto failScan;
    break;

    case RootREG_MASKED:
    res = ThreadScan(ss, root->the.regMasked.thread,
                     root->the.regMasked.stackBot,
                     root->the.regMasked.mask,
                     root->the.regMasked.pattern);
    if (res != ResOK)
      goto failScan;
    break;
    
    case RootFMT:
    res = (*root->the.fmt.scan)(&ss->ss_s, root->the.fmt.base, root->the.fmt.limit);
    ss->scannedSize += AddrOffset(root->the.fmt.base, root->the.fmt.limit);
    if (res != ResOK)
      goto failScan;
    break;

    default:
    NOTREACHED;
    res = ResUNIMPL;
    goto failScan;
  }

  AVER(res == ResOK);
  root->grey = TraceSetDiff(root->grey, ss->traces);
  rootSetSummary(root, ScanStateSummary(ss));
  EVENT3(RootScan, root, ss->traces, ScanStateSummary(ss));

failScan:
  if (root->pm != AccessSetEMPTY) {
    ProtSet(root->protBase, root->protLimit, root->pm);
  }

  return res;
}


/* RootOfAddr -- return the root at addr
 *
 * Returns TRUE if the addr is in a root (and returns the root in
 * *rootReturn) otherwise returns FALSE.  Cf. SegOfAddr.  */

Bool RootOfAddr(Root *rootReturn, Arena arena, Addr addr)
{
  Ring node, next;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  /* addr is arbitrary and can't be checked */

  RING_FOR(node, &ArenaGlobals(arena)->rootRing, next) {
    Root root = RING_ELT(Root, arenaRing, node);

    if (root->protectable && root->protBase <= addr && addr < root->protLimit) {
      *rootReturn = root;
      return TRUE;
    }
  }

  return FALSE;
}


/* RootAccess -- handle barrier hit on root */

void RootAccess(Root root, AccessSet mode)
{
  AVERT(Root, root);
  AVERT(AccessSet, mode);
  AVER((root->pm & mode) != AccessSetEMPTY);
  AVER(mode == AccessWRITE); /* only write protection supported */

  rootSetSummary(root, RefSetUNIV);

  /* Access must now be allowed. */
  AVER((root->pm & mode) == AccessSetEMPTY);
  ProtSet(root->protBase, root->protLimit, root->pm);
}


/* RootsIterate -- iterate over all the roots in the arena */

Res RootsIterate(Globals arena, RootIterateFn f, void *p)
{
  Res res = ResOK;
  Ring node, next;

  RING_FOR(node, &arena->rootRing, next) {
    Root root = RING_ELT(Root, arenaRing, node);

    res = (*f)(root, p);
    if (res != ResOK)
      return res;
  }
  return res;
}


/* RootDescribe -- describe a root */

Res RootDescribe(Root root, mps_lib_FILE *stream, Count depth)
{
  Res res;

  if (!TESTT(Root, root))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, depth,
               "Root $P ($U) {\n", (WriteFP)root, (WriteFU)root->serial,
               "  arena $P ($U)\n", (WriteFP)root->arena,
               (WriteFU)root->arena->serial,
               "  rank $U\n", (WriteFU)root->rank,
               "  grey $B\n", (WriteFB)root->grey,
               "  summary $B\n", (WriteFB)root->summary,
               "  mode",
               root->mode == 0 ? " NONE" : "",
               root->mode & RootModeCONSTANT ? " CONSTANT" : "",
               root->mode & RootModePROTECTABLE ? " PROTECTABLE" : "",
               root->mode & RootModePROTECTABLE_INNER ? " INNER" : "",
               "\n",
               "  protectable $S", WriteFYesNo(root->protectable),
               "  protBase $A", (WriteFA)root->protBase,
               "  protLimit $A", (WriteFA)root->protLimit,
               "  pm",
               root->pm == AccessSetEMPTY ? " EMPTY" : "",
               root->pm & AccessREAD ? " READ" : "",
               root->pm & AccessWRITE ? " WRITE" : "",
               NULL);
  if (res != ResOK)
    return res;

  switch(root->var) {
  case RootTABLE:
    res = WriteF(stream, depth + 2,
                 "table base $A limit $A\n",
                 (WriteFA)root->the.table.base,
                 (WriteFA)root->the.table.limit,
                 NULL);
    if (res != ResOK)
      return res;
    break;

  case RootTABLE_MASKED:
    res = WriteF(stream, depth + 2,
                 "table base $A limit $A mask $B\n",
                 (WriteFA)root->the.tableMasked.base,
                 (WriteFA)root->the.tableMasked.limit,
                 (WriteFB)root->the.tableMasked.mask,
                 NULL);
    if (res != ResOK)
      return res;
    break;

  case RootFUN:
    res = WriteF(stream, depth + 2,
                 "scan function $F\n", (WriteFF)root->the.fun.scan,
                 "environment p $P s $W\n",
                 (WriteFP)root->the.fun.p, (WriteFW)root->the.fun.s,
                 NULL);
    if (res != ResOK)
      return res;
    break;

    case RootREG:
    res = WriteF(stream, depth + 2,
                 "thread $P\n", (WriteFP)root->the.reg.thread,
                 "environment p $P", (WriteFP)root->the.reg.p,
                 NULL);
    if (res != ResOK)
      return res;
    break;

  case RootREG_MASKED:
    res = WriteF(stream, depth + 2,
                 "thread $P\n", (WriteFP)root->the.regMasked.thread,
		 "mask $B\n", (WriteFB)root->the.regMasked.mask,
		 "pattern $B\n", (WriteFB)root->the.regMasked.pattern,
		 "stackBot $P\n", (WriteFP)root->the.regMasked.stackBot,
                 NULL);
    if (res != ResOK)
      return res;
    break;

  case RootFMT:
    res = WriteF(stream, depth + 2,
                 "scan function $F\n", (WriteFF)root->the.fmt.scan,
                 "format base $A limit $A\n",
                 (WriteFA)root->the.fmt.base, (WriteFA)root->the.fmt.limit,
                 NULL);
    if (res != ResOK)
      return res;
    break;
          
  default:
    NOTREACHED;
  }

  res = WriteF(stream, depth,
               "} Root $P ($U)\n", (WriteFP)root, (WriteFU)root->serial,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


/* RootsDescribe -- describe all roots */

Res RootsDescribe(Globals arenaGlobals, mps_lib_FILE *stream, Count depth)
{
  Res res = ResOK;
  Ring node, next;

  RING_FOR(node, &arenaGlobals->rootRing, next) {
    Root root = RING_ELT(Root, arenaRing, node);
    res = RootDescribe(root, stream, depth);
    if (res != ResOK)
      return res;
  }
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
