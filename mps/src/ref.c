/*  impl.c.ref
 *
 *                    REFERENCES
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "std.h"
#include "ref.h"


#ifdef DEBUG_ASSERT

Bool RefRankIsValid(RefRank rank, ValidationType validParam)
{
  AVER(rank >= 0);
  AVER(rank < RefRankMAX);
  return TRUE;
}

#endif /* DEBUG_ASSERT */
