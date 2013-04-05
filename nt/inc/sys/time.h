#ifndef SYS_TIME_H_INCLUDED
#define SYS_TIME_H_INCLUDED

/*
 * sys/time.h either doesn't exist on Windows, or doesn't necessarily
 * have the below stuff.
 */

/* The guards are for MinGW64, which defines these structs on its
   system headers which are included by ms-w32.h.  */
#ifndef _W64
struct timeval
{
  long		tv_sec;		/* seconds */
  long		tv_usec;	/* microseconds */
};
#endif

#ifndef _TIMEZONE_DEFINED
struct timezone
{
  int		tz_minuteswest;	/* minutes west of Greenwich */
  int		tz_dsttime;	/* type of dst correction */
};
#endif

void gettimeofday (struct timeval *, struct timezone *);

#define ITIMER_REAL      0
#define ITIMER_PROF      1

struct itimerval
{
  struct  timeval it_interval;	/* timer interval */
  struct  timeval it_value;	/* current value */
};

int getitimer (int, struct itimerval *);
int setitimer (int, struct itimerval *, struct itimerval *);

#endif /* SYS_TIME_H_INCLUDED */

/* end of sys/time.h */

