/* impl.c.locus: LOCUS MANAGER
 *
 * $HopeName: MMsrc!locus.c(MMdevel_pekka_locus.1) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * DESIGN
 *
 * See design.mps.arena.vm, and design.mps.locus.
 */


#include "mpm.h"


SRCID(locus, "$HopeName: MMsrc!locus.c(MMdevel_pekka_locus.1) $");



/* SegPrefCheck -- check the consistency of a segment preference */

Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* refSet can't be checked because it's an arbitrary bit pattern. */
  CHECKL(BoolCheck(pref->isGen));
  CHECKL(BoolCheck(pref->isCollected));
  /* gen is an arbitrary serial */
  return TRUE;
}


/* SegPrefDefault -- return a segment preference representing the 
 *                   defaults
 */

static SegPrefStruct segPrefDefault = {
  SegPrefSig,                           /* sig */
  ARENA_DEFAULT_SEG_HIGH,               /* high */
  ARENA_DEFAULT_REFSET,                 /* refSet */
  FALSE,                                /* isCollected */
  FALSE,                                /* isGen */
  (Serial)0,                            /* gen */
};

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}


/* SegPrefExpress -- express a segment preference */

Res SegPrefExpress(SegPref pref, SegPrefKind kind, void *p)
{
  AVERT(SegPref, pref);
  AVER(pref != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    pref->high = TRUE;
    break;

  case SegPrefLow:
    AVER(p == NULL);
    pref->high = FALSE;
    break;

  case SegPrefRefSet:
    AVER(p != NULL);
    pref->refSet = *(RefSet *)p;
    break;

  case SegPrefCollected:
    AVER(p == NULL);
    pref->isCollected = TRUE;
    break;

  case SegPrefGen:
    AVER(p != NULL);
    pref->isGen = TRUE;
    pref->gen = *(Serial *)p;
    break;

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }

  return ResOK;
}
