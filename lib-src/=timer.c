#include <stdio.h>
#include <signal.h>
#include <fcntl.h>      /* FASYNC */
#ifdef USG              /* FASYNC for SysV */
#include <sys/file.h>
#endif
#include <sys/time.h>   /* itimer */
#include <sys/types.h>  /* time_t */

extern int errno;
extern char *sys_errlist[], *malloc();
extern time_t time();

#define MAXEVENTS 256
#define FS 1           /* field seperator for input */

struct event {
  char *token;
  time_t reply_at;
} *events[MAXEVENTS];

int slot;         /* The next open place in the events array */
int mevent = 0;   /* 1+ the highest event number */
char *pname;      /* programme name for error messages */

/* Accepts a string of two fields seperated by a ';'
 * First field is string for getdate, saying when to wake-up.
 * Second field is a token to identify the request.
 */
struct event *
schedule(str)
     char *str;

{
  extern time_t getdate();
  extern char *strcpy();
  time_t now;
  register char *p;
  static struct event e;

  for(p = str; *p && *p != FS; p++);
  if (!*p) {
    (void)fprintf(stderr, "%s: bad input format: %s", pname, str);
    return((struct event *)NULL);
  }
  *p++ = 0;
  
  if ((e.reply_at = get_date(str, NULL)) - time(&now) < 0) {
    (void)fprintf(stderr, "%s: bad time spec: %s%c%s", pname, str, FS, p);
    return((struct event *)NULL);
  }

  if ((e.token = malloc((unsigned)strlen(p) + 1)) == NULL) {
    (void)fprintf(stderr, "%s: malloc %s: %s%c%s",
                  pname, sys_errlist[errno], str, FS, p);
    return((struct event *)NULL);
  }
  (void)strcpy(e.token,p);

  return(&e);
}

void
notify()

{
  time_t now, tdiff;
  register int i, newmax = 0;
  /* I prefer using the interval timer rather than alarm(); the latter
     could be substituted if portability requires it. */
  struct itimerval itimer;

  now = time((time_t *)NULL);
  slot = mevent;
  itimer.it_interval.tv_sec = itimer.it_interval.tv_usec = 0;
  itimer.it_value.tv_usec = 0;
  itimer.it_value.tv_sec = -1;

  for(i=0; i < mevent; i++) {
    while (events[i] && events[i]->reply_at <= now) {
      (void)fputs(events[i]->token, stdout);
      free(events[i]->token);
      free((char *)events[i]);
      events[i] = 0;
    }

    if (events[i]) {
      newmax = i+1;
      if ((tdiff = events[i]->reply_at - now) < (time_t)itimer.it_value.tv_sec
          || itimer.it_value.tv_sec < 0)
        /* next timeout */
        itimer.it_value.tv_sec = (long)tdiff;
    } else {
      /* Keep slot as the lowest unused events element */
      if (i < slot) slot = i;
    }
  }
  /* if the array is full to mevent, slot should be the next available spot */
  if (slot > (mevent = newmax)) slot = mevent;
  /* If there's no more events, SIGIO should be next wake-up */
  if (mevent) (void)setitimer(ITIMER_REAL, &itimer, (struct itimerval *)NULL);
}

void
getevent()

{
  extern char *memcpy(), *fgets();
  struct event *ep;
  char buf[256];

  /* in principle the itimer should be disabled on entry to this function,
     but it really doesn't make any important difference if it isn't */

  if (fgets(buf, sizeof(buf), stdin) == NULL) exit(0);

  if (slot == MAXEVENTS)
    (void)fprintf(stderr, "%s: too many events: %s", pname, buf);

  else {
    if ((events[slot] = (struct event *)malloc((sizeof(struct event))))
        == NULL)
      (void)fprintf(stderr,"%s: malloc %s: %s", pname, sys_errlist[errno],buf);

    else {
      if ((ep = schedule(buf)) == NULL)
        free((char *)events[slot]), events[slot] = 0;

      else {
        (void)memcpy((char *)events[slot],(char *)ep,sizeof(struct event));
        if (slot == mevent) mevent++;
      } /* schedule */
    } /* malloc */
  } /* limit events */
  /* timing, timing.  Who knows what this interrupted, or if it said "now"? */
  notify();
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

  (void)signal(SIGIO, getevent);
  (void)signal(SIGALRM, notify);
  (void)fcntl(0, F_SETFL, FASYNC);

  while (1) pause();
}
