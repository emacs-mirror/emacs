/* impl.c.testlib: Test library
 *
 * $HopeName: MMsrc!testlib.c(trunk.10) $
 * Copyright (C) 1995, 1998 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "testlib.h"
#include "mps.h"
#include "mpm.h"
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


/* rnd -- a random number generator
 *
 * I nabbed it from "ML for the Working Programmer"
 * Originally from:
 * Stephen K Park & Keith W Miller (1988). Random number generators:
 * good one are to find.  Communications of the ACM, 31:1192-1201
 */
unsigned long rnd(void)
{
  static unsigned long seed = 1;
  double s;
  s = seed;
  s *= 16807.0;
  s = fmod(s, 2147483647.0);  /* 2^31 - 1 */
  seed = (unsigned long)s;
  return seed;
}


/* die -- Test a return code, and exit on error */
void die(mps_res_t res, const char *s)
{
  if(res != MPS_RES_OK)
  {
    fflush(stdout); /* synchronize */
    fprintf(stderr, "\n%s: %d\n", s, res);
    exit(1);
  }
}


/* adjust_collection_freq -- multiply all collection frequencies by
 *                           a given factor
 *
 * If frequencies are adjusted too low, they are corrected so that all are
 * non-zero and larger than the ones for lower generations.
 */

#define multFREQ(freq, mult) ((freq) = (unsigned long)((freq) * (mult)))

void adjust_collection_freq(double multiplier)
{
  multFREQ(AMCGen0Frequency, multiplier);
  if(AMCGen0Frequency == 0)
    AMCGen0Frequency = 1;
  multFREQ(AMCGen1Frequency, multiplier);
  if(AMCGen1Frequency <= AMCGen0Frequency)
    AMCGen1Frequency = AMCGen0Frequency + 1;
  multFREQ(AMCGen2Frequency, multiplier);
  if(AMCGen2Frequency <= AMCGen1Frequency)
    AMCGen2Frequency = AMCGen1Frequency + 1;
  multFREQ(AMCGen2plusFrequencyMultiplier, multiplier);
  if(AMCGen2plusFrequencyMultiplier == 0)
    AMCGen2plusFrequencyMultiplier = 1;
  multFREQ(AMCGen0RampmodeFrequency, multiplier);
  if(AMCGen0RampmodeFrequency == 0)
    AMCGen0RampmodeFrequency = 1;
  multFREQ(AMCGen1RampmodeFrequency, multiplier);
  if(AMCGen1RampmodeFrequency == AMCGen0RampmodeFrequency)
    AMCGen1RampmodeFrequency = AMCGen0RampmodeFrequency + 1;
  multFREQ(AMCRampGenFrequency, multiplier);
  assert(AMCRampGenFollows == 1);
  if(AMCRampGenFrequency <= AMCGen1RampmodeFrequency)
    AMCRampGenFrequency = AMCGen1RampmodeFrequency + 1;
  multFREQ(AMCGen2RampmodeFrequency, multiplier);
  if(AMCGen2RampmodeFrequency <= AMCRampGenFrequency)
    AMCGen2RampmodeFrequency = AMCRampGenFrequency * 2;
  multFREQ(AMCGen2plusRampmodeFrequencyMultiplier, multiplier);
  if(AMCGen2plusRampmodeFrequencyMultiplier == 0)
    AMCGen2plusRampmodeFrequencyMultiplier = 1;
}
