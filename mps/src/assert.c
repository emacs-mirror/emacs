/*  ==== ASSERTION ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 *  When DEBUG_ASSERT is defined (see debug.h) this source provides
 *  the AssertFail function which is invoked by the ASSERT macros
 *  (see assert.h).  It produces no code otherwise.
 *
 *  Notes
 *
 *  3. To be really solid, assert should write the information into a
 *  buffer before reaching the handler, so that it can be recovered
 *  even if printing fails.  richard 1994-11-15
 *
 *  4. This file declares a static object.  We said we wouldn't do
 *  that.  richard 1994-11-15
 */

#include "std.h"
#include "assert.h"
#include "asrtos.h"
#include "lib.h"
#include <stdarg.h>


static AssertHandler handler = AssertOS;


AssertHandler AssertDefault(void)
{
  return(AssertOS);
}


AssertHandler AssertInstall(AssertHandler new)
{
  AssertHandler prev = handler;
  handler = new;
  return(prev);
}


#ifdef DEBUG_ASSERT


/*  === FAIL ASSERTION ===
 *
 *  This function is called when an ASSERT macro fails a test.  It
 *  calls the installed assertion handler, if it is not NULL, then
 *  exits the program.
 */

void AssertFail(const char *who, const char *cond,
		const char *file, unsigned line,
		const char *format, ...)
{
  va_list arg;

  if(handler != NULL) {
    va_start(arg, format);
    (*handler)(who, cond, file, line, format, arg);
    va_end(arg);
    LibAbort();
    /* not reached, but we can't assert that */
  }
}


/*  === UNREACHABLE STATEMENT ===
 *
 *  This function is called by the NOTREACHED macro.
 */

void AssertReach(const char *file, unsigned line)
{
  AssertFail(NULL, "unreachable statement", file, line, NULL);
}


#endif
