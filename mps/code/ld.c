/* ld.c: LOCATION DEPENDENCY IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
 *
 * .def: A location dependency records the fact that the bit-patterns
 * of some references will be used directly (most likely for
 * hashing), and provides a protocol for finding out whether that
 * dependency has become stale because a reference has been changed (by
 * a moving memory manager).
 *
 * .rationale: The client may build hash-tables using pointer hashing.
 * The collector may change the values of the pointers transparently,
 * by fixing them and moving the objects.  The hash function will no
 * longer return the same value, and the object can't be found in
 * the expected bucket.  When the client can't find an object in a
 * hashtable it must check to see if any of the references in the table
 * have moved, and rehash if they have.  Location dependency provides
 * a reasonably accurate way of determining whether this has happened.
 *
 * .impl: A location dependency consists of an epoch (monotonically
 * increasing notion of time) and a reference set.  The epoch records
 * when the location dependency started, and the reference set
 * accumulates an approximation to the set of references which are
 * depended on.  The client can check to see if any of these
 * references have moved since the epoch.
 *
 * .history: The current epoch, and a history of object movement
 * are recorded in the arena.  Each slot in the history contains a
 * summary of all the movement since an earlier epoch (maintained by
 * LDAge).  To see if a dependency has become stale all that
 * is needed is to see whether its reference set intersects with the
 * movement since its epoch.
 *
 * .mod: LDHistoryLENGTH is used as a modulus to calculate the offset
 * of an epoch in the history, so it's best if this is a power of two.
 * <code/mpmconf.h>
 *
 * .epoch-size: The epoch should probably be a longer integer to avoid
 * the possibility of overflow.
 * (32 bits only gives 50 days at 1ms frequency)
 *
 * .ld.access: Accesses (reads and writes) to the ld structure must be
 * "wrapped" with an ShieldExpose/Cover pair if and only if the access
 * is taking place inside the arena.  Currently this is only the case for
 * LDReset.
 */

#include "mpm.h"

SRCID(ld, "$Id$");


void HistoryInit(History history)
{
  Index i;
  
  AVER(history != NULL);
  
  history->epoch = 0;
  history->prehistory = RefSetEMPTY;
  for (i = 0; i < LDHistoryLENGTH; ++i)
    history->history[i] = RefSetEMPTY;

  history->sig = HistorySig;
  AVERT(History, history);
}

Bool HistoryCheck(History history)
{
  Index i;
  RefSet rs;

  CHECKS(History, history);
  
  /* check that each history entry is a subset of the next oldest */
  rs = RefSetEMPTY;
  /* note this loop starts from 1; there is no history age 0 */
  for (i = 1; i <= LDHistoryLENGTH; ++i) {
    /* check history age 'i'; 'j' is the history index. */
    Index j = (history->epoch + LDHistoryLENGTH - i) % LDHistoryLENGTH;
    CHECKL(RefSetSub(rs, history->history[j]));
    rs = history->history[j];
  }
  /* the oldest history entry must be a subset of the prehistory */
  CHECKL(RefSetSub(rs, history->prehistory));

  return TRUE;
}

void HistoryFinish(History history)
{
  AVERT(History, history);
  history->sig = SigInvalid;
}

Res HistoryDescribe(History history, mps_lib_FILE *stream, Count depth)
{
  Res res;
  Index i;

  if (!TESTT(History, history))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = WriteF(stream, depth,
               "History $P {\n",      (WriteFP)history,
               "  epoch      = $U\n", (WriteFU)history->epoch,
               "  prehistory = $B\n", (WriteFB)history->prehistory,
               "  history {\n",
               "    [note: indices are raw, not rotated]\n",
               NULL);
  if (res != ResOK)
    return res;

  for (i = 0; i < LDHistoryLENGTH; ++i) {
    res = WriteF(stream, depth + 4,
                 "[$U] = $B\n", (WriteFU)i, (WriteFB)history->history[i],
                 NULL);
    if (res != ResOK)
      return res;
  }

  res = WriteF(stream, depth,
               "  }\n",
               "} History $P\n", (WriteFP)history,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


/* LDReset -- reset a dependency to empty
 *
 * .reset.sync: This does not need to be synchronized with LDAge
 * because if the epoch advances after it is read the dependency
 * will simply include movement for more time than necessary.
 */
void LDReset(mps_ld_t ld, Arena arena)
{
  Bool b;
  Seg seg;

  AVER(ld != NULL);
  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, (Addr)ld);
  if (b)
    ShieldExpose(arena, seg);   /* .ld.access */
  ld->_epoch = ArenaHistory(arena)->epoch;
  ld->_rs = RefSetEMPTY;
  if (b)
    ShieldCover(arena, seg);
}


/* LDAdd -- add a reference to a dependency
 *
 * .add.lock-free:  This function is thread safe with respect to the
 * (rest of the) mps.  It is unnecessary to claim locks before calling
 * this function.
 *
 * .add.user-serial:
 * However, this function is _not_ thread safe with respect to itself.
 * Users should ensure that calls to LDAdd operating on the same LD are
 * serialized.
 *
 * .add.sync: Add must take place _before_ the location of the reference
 * is depended on.  If the reference changes between adding and
 * depending it will show up as moved because the movement will have
 * occured since the epoch recorded in the dependency.  If the location
 * were used first only the new location of the reference would end up
 * in the set.
 *
 * .add.no-arena-check: Add does not check that the address belongs to
 * the arena because this would require taking the arena lock. We
 * would rather that this function be lock-free even if some errors
 * are not detected.
 *
 * .add.no-align-check: Add does not check that the address is
 * aligned, for the same reason as .add.check: it can't find out which
 * pool the address belongs to without taking the lock.
 */
void LDAdd(mps_ld_t ld, Arena arena, Addr addr)
{
  AVER(ld != NULL);
  AVER(TESTT(Arena, arena)); /* see .add.lock-free */
  AVER(ld->_epoch <= ArenaHistory(arena)->epoch);

  ld->_rs = RefSetAdd(arena, ld->_rs, addr);
}


/* LDIsStaleAny -- check whether any dependency is stale
 *
 * .stale.thread-safe: This function is thread safe.  It will return a
 * correct (but possibly conservative) answer regardless of the number
 * of calls to LDAge anywhere during the function. Update with care.
 *
 * .stale.current: If the dependency's epoch is the current epoch,
 * nothing can have moved since it was initialized.
 *
 * .stale.recent: If the dependency is recent, see if it intersects
 * with everything which has moved since it was initialized.
 *
 * .stale.recent.conservative: The refset from the history table is
 * loaded before we check whether ld->_epoch is "recent" with respect to
 * the current epoch.  This means that we may (conservatively) decide
 * to use the prehistory instead.
 *
 * .stale.old: Otherwise, if the dependency is older than the length
 * of the history, check it against all movement that has ever occured.
 */
Bool LDIsStaleAny(mps_ld_t ld, Arena arena)
{
  History history;
  RefSet rs;

  AVER(ld != NULL);
  AVER(TESTT(Arena, arena)); /* .stale.thread-safe */
  history = ArenaHistory(arena);
  AVER(ld->_epoch <= history->epoch);

  if (history->epoch == ld->_epoch) /* .stale.current */
    return FALSE;

  /* Load the history refset, _then_ check to see if it's recent.
   * This may in fact load an okay refset, which we decide to throw
   * away and use the pre-history instead. */
  rs = history->history[ld->_epoch % LDHistoryLENGTH];
  /* .stale.recent */
  /* .stale.recent.conservative */
  if (history->epoch - ld->_epoch > LDHistoryLENGTH) {
    rs = history->prehistory;     /* .stale.old */
  }

  return RefSetInter(ld->_rs, rs) != RefSetEMPTY;
}


/* LDIsStale -- check whether a particular dependency is stale
 *
 * .stale.conservative: In fact we just ignore the address and test if
 * any dependency is stale. This is conservatively correct (no false
 * negatives) but provides a hook for future improvement.
 *
 * .stale.no-arena-check: See .add.no-arena-check.
 *
 * .stale.no-align-check: See .add.no-align-check.
 */
Bool LDIsStale(mps_ld_t ld, Arena arena, Addr addr)
{
  UNUSED(addr);
  return LDIsStaleAny(ld, arena);
}


/* LDAge -- age the arena by adding a moved set
 *
 * This stores the fact that a set of references has changed in
 * the history in the arena structure, and increments the epoch.
 *
 * This is only called during a 'flip', because it must be atomic
 * w.r.t. the mutator (and therefore w.r.t. LdIsStale). This is
 * because it updates the notion of the 'current' and 'oldest' history
 * entries.
 */
void LDAge(Arena arena, RefSet rs)
{
  History history;
  Size i;

  AVERT(Arena, arena);
  history = ArenaHistory(arena);
  AVER(rs != RefSetEMPTY);

  /* Replace the entry for epoch - LDHistoryLENGTH by an empty */
  /* set which will become the set which has moved since the */
  /* current epoch. */
  history->history[history->epoch % LDHistoryLENGTH] = RefSetEMPTY;

  /* Record the fact that the moved set has moved, by adding it */
  /* to all the sets in the history, including the set for the */
  /* current epoch. */
  for(i = 0; i < LDHistoryLENGTH; ++i)
    history->history[i] = RefSetUnion(history->history[i], rs);

  /* This is the union of all movement since time zero. */
  history->prehistory = RefSetUnion(history->prehistory, rs);

  /* Advance the epoch by one. */
  ++history->epoch;
  AVER(history->epoch != 0);      /* .epoch-size */
}


/* LDMerge -- merge two location dependencies
 *
 * .merge.lock-free:  This function is thread-safe with respect to the
 * (rest of the) MPS.  It is unnecessary to claim locks before calling
 * this function.
 */
void LDMerge(mps_ld_t ld, Arena arena, mps_ld_t from)
{
  AVER(ld != NULL);
  AVER(TESTT(Arena, arena)); /* .merge.lock-free */
  AVER(ld->_epoch <= ArenaHistory(arena)->epoch);
  AVER(from != NULL);
  AVER(from->_epoch <= ArenaHistory(arena)->epoch);

  /* If a reference has been added since epoch e1 then I've */
  /* certainly added since epoch e0 where e0 < e1.  Therefore */
  /* the epoch of the merged ld is the minimum. */
  if (from->_epoch < ld->_epoch)
    ld->_epoch = from->_epoch;

  /* The set of references added is the union of the two. */
  ld->_rs = RefSetUnion(ld->_rs, from->_rs);
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
