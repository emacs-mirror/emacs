/* impl.h.meter: METER INTERFACE
 *
 * $HopeName: MMsrc!meter.h(trunk.5) $
 * Copyright (C) 1998, 1999 Harlequin Group plc.  All rights reserved.
 *
 * .sources: mps.design.metrics.
 *
 * .purpose: Defines an interface for creating "meters" that accumulate
 * the number, total and mean^2 of a set of data points.  These
 * accumulators can be used to report on the number, total, average, and
 * variance of the data set.
 */

#ifndef meter_h
#define meter_h

#include "mpmtypes.h"
#include "config.h"
#include "misc.h"
#include "mpslib.h"


typedef struct MeterStruct *Meter;

typedef struct MeterStruct 
{
  char *name;
  Count count;
  double total;
  double meanSquared;
  Size min;
  Size max;
} MeterStruct;


extern void MeterInit(Meter meter, char* name, void *owner);
extern void MeterAccumulate(Meter meter, Size amount);
extern Res MeterWrite(Meter meter, mps_lib_FILE *stream);
extern void MeterEmit(Meter meter);

#define METER_DECL(meter) STATISTIC_DECL(struct MeterStruct meter)
#define METER_INIT(meter, init, owner) \
  BEGIN STATISTIC(MeterInit(&(meter), init, owner)); UNUSED(owner); END
/* Hack: owner is typically only used for MeterInit */
#define METER_ACC(meter, delta) \
  STATISTIC(MeterAccumulate(&(meter), delta))
#if defined(DIAGNOSTICS)
#define METER_WRITE(meter, stream) MeterWrite(&(meter), stream)
#elif defined(DIAGNOSTICS_NONE)
#define METER_WRITE(meter, stream) (ResOK)
#else
#error "Diagnostics not configured."
#endif
#define METER_EMIT(meter) STATISTIC(MeterEmit(meter))


#endif /* meter_h */
