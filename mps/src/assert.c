/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName: MMsrc!assert.c(trunk.2) $
 *
 * When DEBUG is defined (see debug.h) this source provides
 * the AssertFail function which is invoked by the assertion macros
 * (see impl.h.assert).  It also provides for user-installed
 * assertion failure handlers.
 *
 * Notes
 *
 * 3. To be really solid, assert should write the information into a
 * buffer before reaching the handler, so that it can be recovered
 * even if printing fails.  richard 1994-11-15
 */

#include "std.h"
#include "assert.h"
#include "lib.h"
#include <stdarg.h>


static void AssertLib(const char *cond, const char *id,
                      const char *file, unsigned line)
{
  LibFormat(LibStreamErr(),
            "\nMPS ASSERTION FAILURE\n\n"
            "Id:        %s\n"
            "File:      %s\n"
            "Line:      %u\n"
            "Condition: %s\n\n",
            id, file, line, cond);
  LibAbort();
}

static AssertHandler handler = AssertLib;

AssertHandler AssertDefault(void)
{
  return AssertLib;
}

AssertHandler AssertInstall(AssertHandler new)
{
  AssertHandler prev = handler;
  handler = new;
  return prev;
}


#ifdef DEBUG


/*  === FAIL ASSERTION ===
 *
 *  This function is called when an ASSERT macro fails a test.  It
 *  calls the installed assertion handler, if it is not NULL, then
 *  exits the program.
 */

void AssertFail(const char *cond, const char *id, 
  const char *file, unsigned line)
{
  if(handler != NULL) {
    (*handler)(cond, id, file, line);
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
  AssertFail("unreachable statement", "no id", file, line);
}


#endif
