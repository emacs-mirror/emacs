/* impl.c.assert: ASSERTION IMPLEMENTATION
 *
 * $HopeName: !assert.c(trunk.10) $
 *
 * This source provides the AssertFail function which is
 * invoked by the assertion macros (see impl.h.assert).
 * It also provides for user-installed assertion failure handlers.
 *
 *
 * NOTES
 *
 * .note.assert-buffer: The Assertion Buffer is not used.  In the future
 * we probably ought to use it. (see .buffer.* below)
 */

#include "mpm.h"


/* CheckLevel -- Control check level
 * This controls the behaviour of Check methods unless MPS_HOT_RED
 * is defined, when it is effectively stuck at "CheckNONE".
 * See impl.h.mpm for extern declaration.
 */

Word CheckLevel = CheckSHALLOW;


/* Assertion buffer
 *
 * .buffer.purpose: the assertion buffer serves two purposes:
 * .buffer.purpose.modify: assertion strings are picked apart and
 * modified before being passed to assertion handlers, this
 * buffer is used to do that.
 * .buffer.purpose.store: Debuggers, post-mortem dumps, etc, can retrieve the
 * assertion text even if for some reason printing the text out failed.
 * [note this is not used, see .note.buffer]
 */

char AssertBuffer[ASSERT_BUFFER_SIZE];

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

void AssertFail1(const char *s)
{
  if(handler != NULL)
    (*handler)(s, "", "", 0);
}
