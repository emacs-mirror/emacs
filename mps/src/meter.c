/* impl.c.meter: METERS
 *
 * $HopeName: MMsrc!meter.c(MMdevel_gavinm_splay.4) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 */

#include "mpm.h"
#include "meter.h"
#include "math.h"

void MeterInit(Meter meter, char *name) 
{
  meter->name = name;
  meter->count = 0;
  meter->total = 0.0;
  meter->meanSquared = 0.0;
  meter->max = 0;
  meter->min = (Size)-1;
}


void MeterAccumulate(Meter meter, Size amount)
{
  Count count = meter->count + 1;
  double total = meter->total;
  double meanSquared = meter->meanSquared;
  double dcount = (double)count;

  /* .limitation.variance: This computation accumulates a running
   * mean^2, minimizing overflow, but sacrificing numerical stablity
   * for small variances.  For more accuracy, the data set should be
   * emitted using a telemetry stream and analyzed off line.
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


Res MeterWrite(Meter meter, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "meter $S {", meter->name,
               "count: $U", meter->count,
               NULL);
  if (res != ResOK)
    return res;
  if (meter->count > 0) {
    double mean = meter->total / (double)meter->count;
    /* --- stddev = sqrt(meanSquared - mean^2), but see
     * .limitation.variance above
     */
    double stddev = sqrt(fabs(meter->meanSquared - (mean * mean)));
    
    res = WriteF(stream,
                 ", total: $D", meter->total,
                 ", max: $U", meter->max,
                 ", min: $U", meter->min,
                 ", mean: $D", mean,
                 ", stddev: $D", stddev,
                 NULL);
    if (res != ResOK)
      return res;
  }
  res = WriteF(stream,
               "}\n",
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}
