/*
 * timer.c --- daemon to provide a tagged interval timer service
 *
 * This little daemon runs forever waiting for signals.  SIGIO (or SIGUSR1)
 * causes it to read an event spec from stdin; that is, a date followed by
 * colon followed by an event label.  SIGALRM causes it to check its queue
 * for events attached to the current second; if one is found, its label
 * is written to stdout.  SIGTERM causes it to terminate, printing a list
 * of pending events.
 *
 * This program is intended to be used with the lisp package called timer.el.
 * It was written anonymously in 1990.  This version was documented and
 * rewritten for portability by esr@snark,thyrsus.com, Aug 7 1992. 
 */
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>      /* FASYNC */
#include <sys/types.h>  /* time_t */

#include "../src/config.h"
#ifdef USG
#undef SIGIO
#define SIGIO	SIGUSR1
#endif

extern int errno;
extern char *sys_errlist[], *malloc();
extern time_t time();

#define MAXEVENTS 256

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
}
events[MAXEVENTS];

char *pname;      /* programme name for error messages */

/* Accepts a string of two fields seperated by FS.
 * First field is string for getdate, saying when to wake-up.
 * Second field is a token to identify the request.
 */
void schedule(str)
	char *str;
{
    extern time_t getdate();
    extern char *strcpy();
    time_t now;
    register char *p;
    static struct event *ep;

#ifdef DEBUG
    (void) fprintf(stderr, "Timer sees: %s", str);
#endif /* DEBUG */

    /* check entry format */
    for(p = str; *p && *p != FS; p++)
	continue;
    if (!*p)
    {
	(void)fprintf(stderr, "%s: bad input format: %s", pname, str);
	return;
    }
    *p++ = 0;
  
    /* allocate an event slot */
    for(ep = events; ep < events + MAXEVENTS; ep++)
	if (ep->token == (char *)NULL)
	    break;
    if (ep == events + MAXEVENTS)
	(void) fprintf(stderr, "%s: too many events: %s", pname, str);

    /* don't allow users to schedule events in past time */
    else if ((ep->reply_at = get_date(str, NULL)) - time(&now) < 0)
	(void)fprintf(stderr, "%s: bad time spec: %s%c%s", pname, str, FS, p);

    /* save the event description */
    else if ((ep->token = malloc((unsigned)strlen(p) + 1)) == NULL)
	(void)fprintf(stderr, "%s: malloc %s: %s%c%s",
		      pname, sys_errlist[errno], str, FS, p);
    else
    {
	(void)strcpy(ep->token, p);

#ifdef DEBUG
	(void) fprintf(stderr,
		       "New event: %ld: %s", ep->reply_at, ep->token);
#endif /* DEBUG */
    }
}

void
notify()
{
    time_t now, tdiff, waitfor = -1;
    register struct event *ep;

    now = time((time_t *)NULL);

    for(ep = events; ep < events + MAXEVENTS; ep++)
	if (ep->token)
	{
	    /* any events ready to fire? */
	    if (ep->reply_at <= now)
	    {
#ifdef DEBUG
		(void) fprintf(stderr,
			       "Event %d firing: %ld @ %s",
			       (ep - events), ep->reply_at, ep->token);
#endif /* DEBUG */
		(void)fputs(ep->token, stdout);
		free(ep->token);
		ep->token = (char *)NULL;
	    }
	    else
	    {
#ifdef DEBUG
		(void) fprintf(stderr,
			   "Event %d still waiting: %ld @ %s",
			   (ep - events), ep->reply_at, ep->token);
#endif /* DEBUG */

		/* next timeout should be the soonest of any remaining */
		if ((tdiff = ep->reply_at - now) < waitfor || waitfor < 0)
		    waitfor = (long)tdiff;
	    }
	}

    /* If there's no more events, SIGIO should be next wake-up */
    if (waitfor != -1)
    {
#ifdef DEBUG
	(void) fprintf(stderr,
		       "Setting %d-second alarm\n", waitfor);
#endif /* DEBUG */
	(void)alarm(waitfor);
    }
}

void
getevent()
{
    extern char *fgets();
    struct event *ep;
    char buf[BUFSIZ];

    /* in principle the itimer should be disabled on entry to this function,
       but it really doesn't make any important difference if it isn't */

    if (fgets(buf, sizeof(buf), stdin) == NULL)
	exit(0);

    /* register the event */
    schedule(buf);

    /* Who knows what this interrupted, or if it said "now"? */
    notify();
}

void
sigcatch(sig)
/* dispatch on incoming signal, then restore it */
{
    struct event *ep;

    switch(sig)
    {
    case SIGALRM:
#ifdef DEBUG
	(void) fprintf(stderr, "Alarm signal received\n");
#endif /* DEBUG */
	notify();
	break;
    case SIGIO:
	getevent();
	break;
    case SIGTERM:
	(void) fprintf(stderr, "Events still queued:\n");
	for (ep = events; ep < events + MAXEVENTS; ep++)
	    if (ep->token)
		(void) fprintf(stderr, "%d = %ld @ %s",
			       ep - events, ep->reply_at, ep->token);
	exit(0);
	break;
    }

    /* required on older UNIXes; harmless on newer ones */
    (void) signal(sig, sigcatch);
}

/*ARGSUSED*/
int
main(argc, argv)
     int argc;
     char **argv;
{
  for (pname = argv[0] + strlen(argv[0]); *pname != '/' && pname != argv[0];
       pname--);
  if (*pname == '/') pname++;

  (void)signal(SIGIO, sigcatch);
  (void)signal(SIGALRM, sigcatch);
  (void)signal(SIGTERM, sigcatch);

#ifndef USG
  (void)fcntl(0, F_SETFL, FASYNC);
#endif /* USG */

  while (1) pause();
}

/* timer.c ends here */
