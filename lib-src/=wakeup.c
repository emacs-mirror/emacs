/* Program to produce output at regular intervals.  */

#include <config.h>

#include <stdio.h>
#include <sys/types.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

struct tm *localtime ();

main (argc, argv)
     int argc;
     char **argv;
{
  int period = 60;
  time_t when;
  struct tm *tp;

  if (argc > 1)
    period = atoi (argv[1]);

  while (1)
    {
      /* Make sure wakeup stops when Emacs goes away.  */
      if (getppid () == 1)
	exit (0);
      printf ("Wake up!\n");
      fflush (stdout);
      /* If using a period of 60, produce the output when the minute
	 changes. */
      if (period == 60)
	{
	  time (&when);
	  tp = localtime (&when);
	  sleep (60 - tp->tm_sec);
	}
      else
	sleep (period);
    }
}
