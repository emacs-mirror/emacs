/*
 * sys/time.h doesn't exist on NT
 */

struct timeval 
  {
    long tv_sec;	/* seconds */
    long tv_usec;	/* microseconds */
  };
struct timezone 
  {
    int	tz_minuteswest;	/* minutes west of Greenwich */
    int	tz_dsttime;	/* type of dst correction */
  };

void gettimeofday (struct timeval *, struct timezone *);

/* end of sys/time.h */
