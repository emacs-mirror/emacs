/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName$
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * This source provides the AssertFail function which is
 * invoked by the assertion macros (see impl.h.assert).
 * It also provides for user-installed assertion failure handlers.
 */

#include "check.h"
#include "mpm.h"

SRCID(assert, "$HopeName: MMsrc!assert.c(trunk.11) $");


/* CheckLevel -- Control check level 
 *
 * This controls the behaviour of Check methods unless MPS_HOT_RED 
 * is defined, when it is effectively stuck at "CheckNONE".
 */

unsigned CheckLevel = CheckSHALLOW;


static void AssertLib(const char *cond, const char *id,
                      const char *file, unsigned line)
{
  WriteF(mps_lib_stderr,
         "\n"
         "MPS ASSERTION FAILURE\n"
         "\n"
         "Id:        $S\n", id,
         "File:      $S\n", file,
         "Line:      $U\n", (WriteFU)line,
         "Condition: $S\n", cond,
         "\n",
         NULL);

  mps_lib_abort();
}


static AssertHandler handler = &AssertLib;


AssertHandler AssertDefault(void)
{
  return &AssertLib;
}


AssertHandler AssertInstall(AssertHandler new)
{
  AssertHandler prev = handler;
  handler = new;
  return prev;
}


/* AssertFail -- fail an assertion
 *
 * This function is called when an ASSERT macro fails a test.  It
 * calls the installed assertion handler, if it is not NULL.  If
 * handler returns the progam continues.
 */
void AssertFail1(const char *s)
{
  if (handler != NULL)
    (*handler)(s, "", "", 0);
}
