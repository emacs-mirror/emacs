/* impl.c.meter: METERS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * TRANSGRESSIONS
 *
 * .trans.label: We label meters with EventLabelAddr, but of course that's
 * meant for labelling Addr's.  We get away with it as long as the type
 * Meter is compatible with Addr.
 */

#include "meter.h"
#include "mpm.h"

SRCID(meter, "$Id$");


/* MeterInit -- initialize a meter */

void MeterInit(Meter meter, char *name, void *owner)
{
  Word sym;

  meter->name = name;
  meter->count = 0;
  meter->total = 0.0;
  meter->meanSquared = 0.0;
  meter->max = 0;
  meter->min = (Size)-1;

  sym = EventInternString(name);
  EventLabelAddr((Addr)meter, sym); /* see .trans.label */
  EVENT_PP(MeterInit, meter, owner);
  UNUSED(owner); /* @@@@ hack */
}


/* MeterAccumulate -- accumulate another data point in the meter */

void MeterAccumulate(Meter meter, Size amount)
{
  Count count = meter->count + 1;
  double total = meter->total;
  double meanSquared = meter->meanSquared;
  double dcount = (double)count;

  /* .limitation.variance: This computation accumulates a running
   * mean^2, minimizing overflow, but sacrificing numerical stablity
   * for small variances.  For more accuracy, the data set should be
   * emitted using a telemetry stream and analyzed off-line.
    .stddev: stddev = sqrt(meanSquared - mean^2).
   */
  meter->count = count;
  meter->total = total + amount;
  meter->meanSquared =
    meanSquared / dcount * (dcount - 1.0)
    + amount / dcount * amount;
  if (amount > meter->max)
    meter->max = amount;
  if (amount < meter->min)
    meter->min = amount;
}


/* MeterWrite -- describe method for meters */

Res MeterWrite(Meter meter, mps_lib_FILE *stream)
{
  Res res = ResOK;

  res = WriteF(stream,
               "meter $S {", meter->name,
               "count: $U", meter->count,
               NULL);
  if (res != ResOK)
    return res;
  if (meter->count > 0) {
    double mean = meter->total / (double)meter->count;
   
    res = WriteF(stream,
                 ", total: $D", meter->total,
                 ", max: $U", meter->max,
                 ", min: $U", meter->min,
                 ", mean: $D", mean,
                 ", mean^2: $D", meter->meanSquared,
                 NULL);
    if (res != ResOK)
      return res;
  }
  res = WriteF(stream, "}\n", NULL);

  return res;
}


/* MeterEmit -- emit an evnet with the current data from the meter */

void MeterEmit(Meter meter)
{
  EVENT_PDDWWW(MeterValues, meter, meter->total, meter->meanSquared,
               meter->count, meter->max, meter->min);
  UNUSED(meter); /* @@@@ hack */
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
