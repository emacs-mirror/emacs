/*  ==== METERING ====
 *
 *  $HopeName: MMsrc!meter.c(trunk.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the metering module.
 *
 *  Provided here are a simple observer, the Enable and Disable
 *  methods, and validation methods.
 *
 *  Notes
 *  3. This module has static data in violation of
 *  rule.impl.c.no-static
 *  1995-08-15 drj
 */

#include "std.h"
#include "stdconf.h"
#include "lib.h"
#include "meter.h"

SRCID("$HopeName$");


#ifdef DEBUG
Bool MeterIsValid(Meter meter, ValidationType validParam)
{
  unsigned i;

  AVER(meter->name != NULL);
  AVER(meter->enabled == TRUE || meter->enabled == FALSE);
  AVER(meter->format != NULL);
  AVER(meter->observers <= METER_OBS_MAX);
  for(i=0; i<meter->observers; ++i) {
    AVER(ISVALIDNESTED(MeterObserver, &meter->observer[i]));
  }
  return TRUE;
}
#endif /* DEBUG */

#ifdef DEBUG
Bool MeterObserverIsValid(MeterObserver observer,
                          ValidationType validParam)
{
  AVER(observer->name != NULL);
  AVER(observer->enabled == TRUE || observer->enabled == FALSE);
  /* period */
  /* tick */
  AVER(observer->f != NULL);
  /* p */
  /* i */
  return TRUE;
}
#endif /* DEBUG */

void MeterEnable(Meter meter)
{
  AVER(ISVALID(Meter, meter));

  meter->enabled = TRUE;
}

void MeterDisable(Meter meter)
{
  AVER(ISVALID(Meter, meter));

  meter->enabled = FALSE;
}

Error MeterStream(LibStream *streamReturn)
{
  return LibStreamMeter(streamReturn);
}

void MeterObserverPrint(Meter meter, int index,
                        const char *file, unsigned line, ...)
{
  LibStream s;
  MeterObserver obs = &meter->observer[index];
  va_list arg;
  /* AVER(ISVALID(Meter, meter)) circularity problems */

  ++(obs->tick);
  if(obs->tick >= obs->period)
  {
    if(MeterStream(&s) == ErrSUCCESS) {
      LibFormat(s, "%s %s %u %s ", meter->name, file, line, obs->name);
      va_start(arg, line);
      LibVFormat(s, (char *)meter->format, arg);
      va_end(arg);
      LibPutChar(s, '\n');
    }
    obs->tick = 0;
  }
}

