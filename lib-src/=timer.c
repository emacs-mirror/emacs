/* timer.c --- daemon to provide a tagged interval timer service

   This little daemon runs forever waiting for signals.  SIGIO (or
   SIGUSR1) causes it to read an event spec from stdin; that is, a
   date followed by colon followed by an event label.  SIGALRM causes
   it to check its queue for events attached to the current second; if
   one is found, its label is written to stdout.  SIGTERM causes it to
   terminate, printing a list of pending events.

   This program is intended to be used with the lisp package called
   timer.el.  It was written anonymously in 1990.  This version was
   documented and rewritten for portability by esr@snark.thyrsus.com,
   Aug 7 1992.  */

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>      /* FASYNC */
#include <sys/types.h>  /* time_t */

#include <../src/config.h>
#ifdef USG
#undef SIGIO
#define SIGIO	SIGUSR1
#endif

#ifdef LINUX
/* Perhaps this is correct unconditionally.  */
#undef signal
#endif


extern int errno;
extern char *sys_errlist[], *malloc ();
extern time_t time ();

/*
 * The field separator for input.  This character shouldn't be legal in a date,
 * and should be printable so event strings are readable by people.  Was
 * originally ';', then got changed to bogus `\001'.
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

char *pname;      /* programme name for error messages */

/* Accepts a string of two fields separated by FS.
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

	  /* Should timer exit now?  Well, we've still got other
	     events in the queue, and more memory might become
	     available in the future, so we'll just toss this event.
	     This will screw up whoever scheduled the event, but
	     maybe someone else will survive.  */
	  return;
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
	       pname, sys_errlist[errno], str, FS, p);
      return;
    }

  strcpy (ep->token, p);
  num_events++;
}

void
notify ()
{
  time_t now, tdiff, waitfor = -1;
  register struct event *ep;

  /* If an alarm timer runs out while this function is executing,
     it could get called recursively.  This would be bad, because
     it's not re-entrant.  So we must try to suspend the signal. */
#if 0   /* This function isn't right for BSD.  Fix it later.  */
  sighold(SIGIO);
#endif

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

#if 0  /* This function isn't right for BSD.  */
  sigrelse(SIGIO);
#endif
}

void
getevent ()
{
  int i;
  char *buf;
  int buf_size;

  /* In principle the itimer should be disabled on entry to this
     function, but it really doesn't make any important difference
     if it isn't.  */

  buf_size = 80;
  buf = (char *) malloc (buf_size);

  /* Read a line from standard input, expanding buf if it is too short
     to hold the line.  */
  for (i = 0; ; i++)
    {
      int c;

      if (i >= buf_size)
	{
	  buf_size *= 2;
	  buf = (char *) realloc (buf, buf_size);

	  /* If we're out of memory, toss this event.  */
	  do
	    {
	      c = getchar ();
	    }
	  while (c != '\n' && c != EOF);
	  
	  return;
	}

      c = getchar ();

      if (c == EOF)
	exit (0);

      if (c == '\n')
	{
	  buf[i] = '\0';
	  break;
	}

      buf[i] = c;
    }

  /* Register the event.  */
  schedule (buf);
  free (buf);

  /* Who knows what this interrupted, or if it said "now"? */
  notify ();
}

SIGTYPE
sigcatch (sig)
     int sig;
/* dispatch on incoming signal, then restore it */
{
  struct event *ep;

  switch (sig)
    {
    case SIGALRM:
      notify ();
      break;
    case SIGIO:
      getevent ();
      break;
    case SIGTERM:
      fprintf (stderr, "Events still queued:\n");
      for (ep = events; ep < events + num_events; ep++)
	fprintf (stderr, "%d = %ld @ %s\n",
		 ep - events, ep->reply_at, ep->token);
      exit (0);
      break;
    }

  /* required on older UNIXes; harmless on newer ones */
  signal (sig, sigcatch);
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

  signal (SIGIO, sigcatch);
  signal (SIGALRM, sigcatch);
  signal (SIGTERM, sigcatch);

#ifndef USG
  if (fcntl (0, F_SETOWN, getpid ()) == -1)
    {
      fprintf (stderr, "%s: can't set ownership of stdin\n", pname);
      fprintf (stderr, "%s\n", sys_errlist[errno]);
      exit (1);
    }
  if (fcntl (0, F_SETFL, fcntl (0, F_GETFL, 0) | FASYNC) == -1)
    {
      fprintf (stderr, "%s: can't request asynchronous I/O on stdin\n", pname);
      fprintf (stderr, "%s\n", sys_errlist[errno]);
      exit (1);
    }
#endif /* USG */

  /* In case Emacs sent some input before we set up
     the handling of SIGIO, read it now.  */
  kill (0, SIGIO);

  for (;;)
    pause ();
}

/* timer.c ends here */
