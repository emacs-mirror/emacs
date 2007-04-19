/*$Header$
  Implementation of diagnostic interface.
*/

#include <stdarg.h>

#include "mpm.h"
#include "mpslib.h" /* for mps_lib_stdout */

Bool DiagEnabledGlobal = TRUE;

Bool DiagIsOn(void)
{
  return DiagEnabledGlobal;
}


mps_lib_FILE *DiagStream(void)
{
  return mps_lib_stdout;
}


