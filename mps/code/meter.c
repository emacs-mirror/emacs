/* meter.c: METERS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "meter.h"
#include "mpm.h"

SRCID(meter, "$Id$");


/* MeterInit -- initialize a meter */

void MeterInit(Meter meter, const char *name, void *owner)
{
  Word sym;

  meter->name = name;
  meter->count = 0;
  meter->total = 0.0;
  meter->meanSquared = 0.0;
  meter->max = 0;
  meter->min = (Size)-1;

  sym = EventInternString(name);
  EventLabelPointer(meter, sym);
  EVENT2(MeterInit, meter, owner);
}


/* MeterAccumulate -- accumulate another data point in the meter */

void MeterAccumulate(Meter meter, Size amount)
{
  Count count = meter->count + 1;
  double total = meter->total;
  double meanSquared = meter->meanSquared;
  double dcount = (double)count;

  /* .limitation.variance: This computation accumulates a running
   * mean^2, minimizing overflow, but sacrificing numerical stability
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

Res MeterWrite(Meter meter, mps_lib_FILE *stream, Count depth)
{
  Res res = ResOK;

  res = WriteF(stream, depth,
               "meter \"$S\" {", (WriteFS)meter->name,
               "count: $U", (WriteFU)meter->count,
               NULL);
  if (res != ResOK)
    return res;
  if (meter->count > 0) {
    double mean = meter->total / (double)meter->count;

    res = WriteF(stream, 0,
                 ", total $D", (WriteFD)meter->total,
                 ", max $U", (WriteFU)meter->max,
                 ", min $U", (WriteFU)meter->min,
                 ", mean $D", (WriteFD)mean,
                 ", meanSquared $D", (WriteFD)meter->meanSquared,
                 NULL);
    if (res != ResOK)
      return res;
  }
  res = WriteF(stream, 0, "}\n", NULL);

  return res;
}


/* MeterEmit -- emit an event with the current data from the meter */

void MeterEmit(Meter meter)
{
  EVENT6(MeterValues, meter, meter->total, meter->meanSquared,
         meter->count, meter->max, meter->min);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
