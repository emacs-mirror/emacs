/*  ==== METERING ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the interface to the metering module.
 *
 *  Metering (globally) is enabled by defining the METERING preprocessor
 *  symbol, if this is not defined, then there will be no metering (see
 *  target.h).
 *
 *  The aim is to have a simple and reasonably efficient method for
 *  metering certain values and events, by "annotating" the source
 *  code.
 *  
 *  Abstractly a meter is a channel that can accept data of a certain
 *  format.  The channel is connected to a number of observers, one of
 *  the observers is distinguised and is called the primary observer
 *  (it is the first observer).  Data can be registered on the channel,
 *  and any data so registered will be transmitted to the observers.
 *  Observers are the sole entity responsible for doing something
 *  useful with the data.  A meter can be enabled and disabled
 *  dynamically; a disabled meter does not transmit data registered
 *  with it to the observers.
 *  
 *  Concretely a meter is represented by a MeterStruct, and an observer
 *  by a MeterOberverStruct.  A meter may have no more than
 *  METER_OBS_MAX observers connected to it.
 *  
 *  Data is registered on a meter using the METERx or MULTIMETERx
 *  call.  The difference being that METERx transmits the data only to
 *  the primary observer, and MULTIMETERx transmits the data to all the
 *  observers.
 *  
 *  MeterEnable (MeterDisable) can be used to enable (disable) a
 *  meter.
 *  
 *  To actually use a meter, you need to declare a meter (with its
 *  associated observers), and use it.  The code will look something
 *  like this:
 *  
 *  #include "meter.h"
 *  
 *  MeterStruct fooMeterStruct = {"foo", TRUE, "%d", 1,
 *                                  {{"print", TRUE, 1, 0,
 *  				  MeterObserverPrint, NULL, 0}}
 *                                };
 *  Meter fooMeter = &fooMeterStruct;
 *  
 *  :
 *  
 *  void foo(int fooFactor) {
 *    METER1(foo, fooFactor);
 *    {perform foo at FooFactor fooFactor}
 *  }
 *  
 *
 *  Notes
 *   1. MULTIMETERx is a pollution of the namespace (should be
 *    METERMULTIx), but it's a good pun.
 *  1995-07-31 drj
 *   2. Why is METER_OBS_MAX 4?
 *  1995-07-31 drj
 */

#ifndef meter_h
#define meter_h

#include "std.h"
#include "stdconf.h"
#include "lib.h"

typedef struct MeterStruct *Meter;
typedef struct MeterObserverStruct *MeterObserver;

/*  == Observer Struct ==
 *
 *  The period and tick fields are intended to be used by the Observer
 *  to if the observer wants to exhibit some sort of periodic
 *  behaviour.  It is intended that any observer that wishes to exhibit
 *  periodic behaviour should increment the tick field every time it is
 *  called, and if the tick field is greater than or equal to the
 *  period field it should reset the tick field (to 0) and perform some
 *  special action.  This means that the interpretation of the period
 *  field can be made regardless of the observer being used.  The code
 *  should look something like this:
 *  
 *  Observer observer = &meter->observer[index];
 *  ++(observer->tick);
 *  if(observer->tick >= observer->period) {
 *    observer->tick = 0;
 *    do_something_intensive;
 *  } else {
 *    do_something_light;
 *  }
 *  
 *  The f field is the address of the observer method that is called
 *  when observations are to be taken.  f is invoked as (*f)(Meter
 *  meter, int index, const char *file, unsigned line, ...), where
 *  meter is the meter that the data has been sent to, index is the
 *  index of this observer in the meter->observer array, file and line
 *  are the name of the file and the number of the line where the
 *  actual metering is taking place.  The remaining arguments are the
 *  data that is being metered, and can be interpreted according to the
 *  meter->format field.
 *
 *  The p and i fields are intended to be used to store any closure
 *  information that the observer needs.
 */

typedef struct MeterObserverStruct
{
  const char *name;                             /* string name */
  Bool enabled;
  unsigned period;                              /* output rate, or zero */
  unsigned tick;                                /* since last output */
  void (*f)(Meter meter, int index,             /* identity */
            const char *file, unsigned line,    /* static location */
            ...);                               /* the data (see format) */
  void *p;                                      /* closure pointer */
  int i;                                        /* closure int */
} MeterObserverStruct;

/*  == Meter Struct ==
 *
 *  This is the concrete representation of a meter.
 *
 *  The name field is just a string name of the meter.
 *
 *  The enabled flag is TRUE if the meter is enabled and FALSE if
 *  disabled.  A disabled meter will not do anything with data that is
 *  registered with it.
 *  
 *  The format field is a string representing the format of the data
 *  such that printf(meter->format, ...) works, where the remaining
 *  arguments are the data registered on the meter.
 *  
 *  observer is an array of the observers attached to this meter.  The
 *  first observer in the array (observer[0]) is the primary observer.
 *  The primary observer is the only one that the data will be
 *  transmitted to if using a METER call (if using a MULTIMETER call
 *  then all observers will be transmitted the data).
 */

typedef struct MeterStruct
{
  const char *name;
  Bool enabled;
  char *format;
  unsigned observers;           /* number of observers in observer[] */
  MeterObserverStruct observer[METER_OBS_MAX];
} MeterStruct;

/* == Observers ==
 *
 * This is a default observer.  It prints data to the default output
 * stream every meter->period ticks.
 */

void MeterObserverPrint(Meter meter, int index,
                        const char *file, unsigned line, ...);

/* == Validation ==
 *
 */
Bool MeterIsValid(Meter meter, ValidationType validParam);
Bool MeterObserverIsValid(MeterObserver observer,
                          ValidationType validParam);

/* == Control ==
 *
 * A disabled meter will not transmit any data to its observers.
 */
void MeterEnable(Meter meter);
void MeterDisable(Meter meter);

/*  == Stream ==
 *
 *  Returns (through streamReturn) a LibStream for default
 *  observer output.  If it fails then *streamReturn will not be
 *  updated and an Error value will be returned. (see std.include.h.lib
 *  for details of LibStream)
 */
Error MeterStream(LibStream *streamReturn);

/*  == Registering ==
 *
 *  To register data with a meter use METERx or MULTIMETERx, where x is
 *  the number of data to register (or use METER or MULTIMETER if not
 *  registering any data).  The macros take a meter as their first
 *  argument, and the data values as the remaining arguments.  The
 *  METER macro only transmits the data to the principal observer (the
 *  first one).  The MULTIMETER macro transmits data to all observers.
 *
 *  The following code was generated using the UNIX program:
 *
#!/bin/sh
nawk '
BEGIN {
printf("#ifdef METERING\n\n")

print "#define METER(meter) \\"
print "  M_BEGIN \\"
print "    if((meter)->enabled == TRUE) \\"
print "      if((meter)->observer[0].enabled == TRUE) { \\"
print "        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__); \\"
print "      } \\"
print "  M_END"
print ""

for (i=0; i<6; ++i) {
 printf("#define METER%d(meter", i+1)
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf(") \\\n")
 print "  M_BEGIN \\"
 print "    if((meter)->enabled == TRUE) \\"
 print "      if((meter)->observer[0].enabled == TRUE) { \\"
 printf("        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__")
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf("); \\\n      } \\\n")
 print "  M_END"
 print ""
}

print "#define MULTIMETER(meter) \\"
print "  M_BEGIN { \\"
print "    int _metermulti_loop_i; \\"
print "    if((meter)->enabled == TRUE) \\"
printf ("      for(_metermulti_loop_i = 0; _metermulti_loop_i")
printf (" < METER_OBS_MAX; ++_metermulti_loop_i) \\\n")
print "        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \\"
printf("          (*(meter)->observer[_metermulti_loop_i].f)")
printf("(meter, _metermulti_loop_i, __FILE__, __LINE__); \\\n")
print "        } \\"
print "  } \\"
print "  M_END"
print ""

for (i=0; i<6; ++i) {
 printf("#define MULTIMETER%d(meter", i+1)
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf("); \\\n")
 print "  M_BEGIN { \\"
 print "    int _metermulti_loop_i; \\"
 print "    if((meter)->enabled == TRUE) \\"
 printf ("      for(_metermulti_loop_i = 0; _metermulti_loop_i")
 printf (" < METER_OBS_MAX; ++_metermulti_loop_i) \\\n")
 print "        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \\"
 printf("          (*(meter)->observer[_metermulti_loop_i].f)")
 printf("(meter, _metermulti_loop_i, __FILE__, __LINE__")
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf("); \\\n        } \\\n")
 print "  } \\"
 print "  M_END"
 print ""
}

printf("\n#else\n\n")
print "#define METER(meter)  NOOP"
for(i=0; i<6; ++i) {
 printf("#define METER%d(meter", i+1)
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf(")  NOOP\n")
}
print "#define MULTIMETER(meter)  NOOP"
for(i=0; i<6; ++i) {
 printf("#define MULTIMETER%d(meter", i+1)
 for(j=0; j<=i; ++j) printf(", p%d", j)
 printf(")  NOOP\n")
}
printf("\n#endif\n")
}
' </dev/null
*
*/


#ifdef METERING

#define METER(meter) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__); \
      } \
  M_END

#define METER1(meter, p0) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0); \
      } \
  M_END

#define METER2(meter, p0, p1) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0, p1); \
      } \
  M_END

#define METER3(meter, p0, p1, p2) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0, p1, p2); \
      } \
  M_END

#define METER4(meter, p0, p1, p2, p3) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0, p1, p2, p3); \
      } \
  M_END

#define METER5(meter, p0, p1, p2, p3, p4) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0, p1, p2, p3, p4); \
      } \
  M_END

#define METER6(meter, p0, p1, p2, p3, p4, p5) \
  M_BEGIN \
    if((meter)->enabled == TRUE) \
      if((meter)->observer[0].enabled == TRUE) { \
        (*(meter)->observer[0].f)(meter, 0, __FILE__, __LINE__, p0, p1, p2, p3, p4, p5); \
      } \
  M_END

#define MULTIMETER(meter) \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__); \
        } \
  } \
  M_END

#define MULTIMETER1(meter, p0); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0); \
        } \
  } \
  M_END

#define MULTIMETER2(meter, p0, p1); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0, p1); \
        } \
  } \
  M_END

#define MULTIMETER3(meter, p0, p1, p2); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0, p1, p2); \
        } \
  } \
  M_END

#define MULTIMETER4(meter, p0, p1, p2, p3); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0, p1, p2, p3); \
        } \
  } \
  M_END

#define MULTIMETER5(meter, p0, p1, p2, p3, p4); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0, p1, p2, p3, p4); \
        } \
  } \
  M_END

#define MULTIMETER6(meter, p0, p1, p2, p3, p4, p5); \
  M_BEGIN { \
    int _metermulti_loop_i; \
    if((meter)->enabled == TRUE) \
      for(_metermulti_loop_i = 0; _metermulti_loop_i < METER_OBS_MAX; ++_metermulti_loop_i) \
        if((meter)->observer[_metermulti_loop_i].enabled == TRUE) { \
          (*(meter)->observer[_metermulti_loop_i].f)(meter, _metermulti_loop_i, __FILE__, __LINE__, p0, p1, p2, p3, p4, p5); \
        } \
  } \
  M_END


#else

#define METER(meter)  NOOP
#define METER1(meter, p0)  NOOP
#define METER2(meter, p0, p1)  NOOP
#define METER3(meter, p0, p1, p2)  NOOP
#define METER4(meter, p0, p1, p2, p3)  NOOP
#define METER5(meter, p0, p1, p2, p3, p4)  NOOP
#define METER6(meter, p0, p1, p2, p3, p4, p5)  NOOP
#define MULTIMETER(meter)  NOOP
#define MULTIMETER1(meter, p0)  NOOP
#define MULTIMETER2(meter, p0, p1)  NOOP
#define MULTIMETER3(meter, p0, p1, p2)  NOOP
#define MULTIMETER4(meter, p0, p1, p2, p3)  NOOP
#define MULTIMETER5(meter, p0, p1, p2, p3, p4)  NOOP
#define MULTIMETER6(meter, p0, p1, p2, p3, p4, p5)  NOOP

#endif /* METERING */

#endif /* meter_h */
