/* timer.c --- daemon to provide a tagged interval timer service

   This little daemon runs forever waiting for commands to schedule events.
   SIGALRM causes
   it to check its queue for events attached to the current second; if
   one is found, its label is written to stdout.  SIGTERM causes it to
   terminate, printing a list of pending events.

   This program is intended to be used with the lisp package called
   timer.el.  The first such program was written anonymously in 1990.
   This version was documented and rewritten for portability by
   esr@snark.thyrsus.com, Aug 7 1992.  */

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>  /* time_t */

#include <../src/config.h>
#undef read

#ifdef LINUX
/* Perhaps this is correct unconditionally.  */
#undef signal
#endif
#ifdef _CX_UX
/* I agree with the comment above, this probably should be unconditional (it
 * is already unconditional in a couple of other files in this directory),
 * but in the spirit of minimizing the effects of my port, I am making it
 * conditional on _CX_UX.
 */
#undef signal
#endif


extern int errno;
extern char *strerror ();
extern time_t time ();

/*
 * The field separator for input.  This character shouldn't occur in dates,
 * and should be printable so event strings are readable by people.
 */
#define FS '@'

struct event
  {
    char *token;
    time_t reply_at;
  };
int events_size;		/* How many slots have we allocated?  */
int num_events;			/* How many are actually scheduled?  */
struct event *events;		/* events[0 .. num_events-1] are the
				   valid events.  */

char *pname;      /* program name for error messages */

/* This buffer is used for reading commands.
   We make it longer when necessary, but we never free it.  */
char *buf;
/* This is the allocated size of buf.  */
int buf_size;

/* Non-zero means don't handle an alarm now;
   instead, just set alarm_deferred if an alarm happens.
   We set this around parts of the program that call malloc and free.  */
int defer_alarms;

/* Non-zero if an alarm came in during the reading of a command.  */
int alarm_deferred;

/* Schedule one event, and arrange an alarm for it.
   STR is a string of two fields separated by FS.
   First field is string for get_date, saying when to wake-up.
   Second field is a token to identify the request.  */

void
schedule (str)
     char *str;
{
  extern time_t get_date ();
  extern char *strcpy ();
  time_t now;
  register char *p;
  static struct event *ep;
  
  /* check entry format */
  for (p = str; *p && *p != FS; p++)
    continue;
  if (!*p)
    {
      fprintf (stderr, "%s: bad input format: %s\n", pname, str);
      return;
    }
  *p++ = 0;
  
  /* allocate an event slot */
  ep = events + num_events;

  /* If the event array is full, stretch it.  After stretching, we know
     that ep will be pointing to an available event spot.  */
  if (ep == events + events_size)
    {
      int old_size = events_size;

      events_size *= 2;
      events = ((struct event *)
		realloc (events, events_size * sizeof (struct event)));
      if (! events)
	{
	  fprintf (stderr, "%s: virtual memory exhausted.\n", pname);
	  /* Since there is so much virtual memory, and running out
	     almost surely means something is very very wrong,
	     it is best to exit rather than continue.  */
	  exit (1);
	}

      while (old_size < events_size)
	events[old_size++].token = NULL;
    }

  /* Don't allow users to schedule events in past time.  */
  ep->reply_at = get_date (str, NULL);
  if (ep->reply_at - time (&now) < 0)
    {
      fprintf (stderr, "%s: bad time spec: %s%c%s\n", pname, str, FS, p);
      return;
    }

  /* save the event description */
  ep->token = (char *) malloc ((unsigned) strlen (p) + 1);
  if (! ep->token)
    {
      fprintf (stderr, "%s: malloc %s: %s%c%s\n",
	       pname, strerror (errno), str, FS, p);
      return;
    }

  strcpy (ep->token, p);
  num_events++;
}

/* Print the notification for the alarmed event just arrived if any,
   and schedule an alarm for the next event if any.  */

void
notify ()
{
  time_t now, tdiff, waitfor = -1;
  register struct event *ep;

  /* Inhibit interference with alarms while changing global vars.  */
  defer_alarms = 1;
  alarm_deferred = 0;

  now = time ((time_t *) NULL);

  for (ep = events; ep < events + num_events; ep++)
    /* Are any events ready to fire?  */
    if (ep->reply_at <= now)
      {
	fputs (ep->token, stdout);
	putc ('\n', stdout);
	fflush (stdout);
	free (ep->token);

	/* We now have a hole in the event array; fill it with the last
	   event.  */
	ep->token = events[num_events - 1].token;
	ep->reply_at = events[num_events - 1].reply_at;
	num_events--;

	/* We ought to scan this event again.  */
	ep--;
      }
    else
      {
	/* next timeout should be the soonest of any remaining */
	if ((tdiff = ep->reply_at - now) < waitfor || waitfor < 0)
	  waitfor = (long)tdiff;
      }

  /* If there are no more events, we needn't bother setting an alarm.  */
  if (num_events > 0)
    alarm (waitfor);

  /* Now check if there was another alarm
     while we were handling an explicit request.  */
  defer_alarms = 0;
  if (alarm_deferred)
    notify ();
  alarm_deferred = 0;
}

/* Read one command from command from standard input
   and schedule the event for it.  */

void
getevent ()
{
  int i;
  int n_events;

  /* In principle the itimer should be disabled on entry to this
     function, but it really doesn't make any important difference
     if it isn't.  */

  if (buf == 0)
    {
      buf_size = 80;
      buf = (char *) malloc (buf_size);
    }

  /* Read a line from standard input, expanding buf if it is too short
     to hold the line.  */
  for (i = 0; ; i++)
    {
      char c;
      int nread;

      if (i >= buf_size)
	{
	  buf_size *= 2;
	  alarm_deferred = 0;
	  defer_alarms = 1;
	  buf = (char *) realloc (buf, buf_size);
	  defer_alarms = 0;
	  if (alarm_deferred)
	    notify ();
	  alarm_deferred = 0;
	}

      /* Read one character into c.  */
      while (1)
	{
	  nread = read (fileno (stdin), &c, 1);

	  /* Retry after transient error.  */
	  if (nread < 0
	      && (1
#ifdef EINTR
		  || errno == EINTR
#endif
#ifdef EAGAIN
		  || errno == EAGAIN
#endif
		  ))
	    continue;

	  /* Report serious errors.  */
	  if (nread < 0)
	    {
	      perror ("read");
	      exit (1);
	    }

	  /* On eof, exit.  */
	  if (nread == 0)
	    exit (0);

	  break;
	}

      if (c == '\n')
	{
	  buf[i] = '\0';
	  break;
	}

      buf[i] = c;
    }

  /* Register the event.  */
  alarm_deferred = 0;
  defer_alarms = 1;
  schedule (buf);
  defer_alarms = 0;
  notify ();
  alarm_deferred = 0;
}

/* Handle incoming signal SIG.  */

SIGTYPE
sigcatch (sig)
     int sig;
{
  struct event *ep;

  /* required on older UNIXes; harmless on newer ones */
  signal (sig, sigcatch);

  switch (sig)
    {
    case SIGALRM:
      if (defer_alarms)
	alarm_deferred = 1;
      else
	notify ();
      break;
    case SIGTERM:
      fprintf (stderr, "Events still queued:\n");
      for (ep = events; ep < events + num_events; ep++)
	fprintf (stderr, "%d = %ld @ %s\n",
		 ep - events, ep->reply_at, ep->token);
      exit (0);
      break;
    }
}

/*ARGSUSED*/
int
main (argc, argv)
     int argc;
     char **argv;
{
  for (pname = argv[0] + strlen (argv[0]);
       *pname != '/' && pname != argv[0];
       pname--);
  if (*pname == '/')
    pname++;

  events_size = 16;
  events = ((struct event *) malloc (events_size * sizeof (*events)));
  num_events = 0;

  signal (SIGALRM, sigcatch);
  signal (SIGTERM, sigcatch);

  /* Loop reading commands from standard input
     and scheduling alarms accordingly.
     The alarms are handled asynchronously, while we wait for commands.  */
  while (1)
    getevent ();
}

#ifndef HAVE_STRERROR
char *
strerror (errnum)
     int errnum;
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}

#endif /* ! HAVE_STRERROR */

long *
xmalloc (size)
     int size;
{
  register long *val;

  val = (long *) malloc (size);

  if (!val && size)
    {
      fprintf (stderr, "timer: virtual memory exceeded\n");
      exit (1);
    }
    
  return val;
}

/* timer.c ends here */
