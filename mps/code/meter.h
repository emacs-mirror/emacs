/* meter.h: METER INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
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
  const char *name;
  Count count;
  double total;
  double meanSquared;
  Size min;
  Size max;
} MeterStruct;


extern void MeterInit(Meter meter, const char *name, void *owner);
extern void MeterAccumulate(Meter meter, Size amount);
extern Res MeterWrite(Meter meter, mps_lib_FILE *stream, Count depth);
extern void MeterEmit(Meter meter);

#define METER_DECL(meter) STATISTIC_DECL(struct MeterStruct meter)
#define METER_INIT(meter, init, owner) \
  BEGIN STATISTIC(MeterInit(&(meter), init, owner)); UNUSED(owner); END
/* Hack: owner is typically only used for MeterInit */
#define METER_ACC(meter, delta) \
  STATISTIC(MeterAccumulate(&(meter), delta))
#if defined(STATISTICS)
#define METER_WRITE(meter, stream, depth) BEGIN \
    Res _res = MeterWrite(&(meter), (stream), (depth)); \
    if (_res != ResOK) return _res; \
  END
#elif defined(STATISTICS_NONE)
#define METER_WRITE(meter, stream, depth) NOOP
#else
#error "No statistics configured."
#endif
#define METER_EMIT(meter) STATISTIC(MeterEmit(meter))


#endif /* meter_h */


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
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
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
