/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName: MMsrc!assert.c(MMdevel_restr.2) $
 *
 * This source provides the AssertFail function which is
 * invoked by the assertion macros (see impl.h.assert).
 * It also provides for user-installed assertion failure handlers.
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


static void AssertLib(const char *cond, const char *id,
                      const char *file, unsigned line)
{
  Lib_fprintf(Lib_stderr,
              "\nMPS ASSERTION FAILURE\n\n"
              "Id:        %s\n"
              "File:      %s\n"
              "Line:      %u\n"
              "Condition: %s\n\n",
              id, file, line, cond);
  Lib_abort();
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


/* AssertFail -- fail an assertion
 *
 * This function is called when an ASSERT macro fails a test.  It
 * calls the installed assertion handler, if it is not NULL.  If
 * handler returns the progam continues.
 */

void AssertFail(const char *cond, const char *id,
                const char *file, unsigned line)
{
  if(handler != NULL)
    (*handler)(cond, id, file, line);
}
