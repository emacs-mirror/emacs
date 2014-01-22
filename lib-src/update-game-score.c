/* update-game-score.c --- Update a score file

Copyright (C) 2002-2014 Free Software Foundation, Inc.

Author: Colin Walters <walters@debian.org>

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* This program allows a game to securely and atomically update a
   score file.  It should be installed setuid, owned by an appropriate
   user like `games'.

   Alternatively, it can be compiled without HAVE_SHARED_GAME_DIR
   defined, and in that case it will store scores in the user's home
   directory (it should NOT be setuid).

   Created 2002/03/22.
*/

#include <config.h>

#include <unistd.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <pwd.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <getopt.h>

#ifdef WINDOWSNT
#include "ntlib.h"
#endif

#ifndef min
# define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#define MAX_ATTEMPTS 5
#define MAX_DATA_LEN 1024

static _Noreturn void
usage (int err)
{
  fprintf (stdout, "Usage: update-game-score [-m MAX] [-r] [-d DIR] game/scorefile SCORE DATA\n");
  fprintf (stdout, "       update-game-score -h\n");
  fprintf (stdout, " -h\t\tDisplay this help.\n");
  fprintf (stdout, " -m MAX\t\tLimit the maximum number of scores to MAX.\n");
  fprintf (stdout, " -r\t\tSort the scores in increasing order.\n");
  fprintf (stdout, " -d DIR\t\tStore scores in DIR (only if not setuid).\n");
  exit (err);
}

static int lock_file (const char *filename, void **state);
static int unlock_file (const char *filename, void *state);

struct score_entry
{
  intmax_t score;
  char *username;
  char *data;
};

#define MAX_SCORES min (PTRDIFF_MAX, SIZE_MAX / sizeof (struct score_entry))

static int read_scores (const char *filename, struct score_entry **scores,
			ptrdiff_t *count, ptrdiff_t *alloc);
static int push_score (struct score_entry **scores, ptrdiff_t *count,
		       ptrdiff_t *size, struct score_entry const *newscore);
static void sort_scores (struct score_entry *scores, ptrdiff_t count,
			 bool reverse);
static int write_scores (const char *filename,
			 const struct score_entry *scores, ptrdiff_t count);

static _Noreturn void
lose (const char *msg)
{
  fprintf (stderr, "%s\n", msg);
  exit (EXIT_FAILURE);
}

static _Noreturn void
lose_syserr (const char *msg)
{
  fprintf (stderr, "%s: %s\n", msg, strerror (errno));
  exit (EXIT_FAILURE);
}

static char *
get_user_id (void)
{
  struct passwd *buf = getpwuid (getuid ());
  if (!buf || strchr (buf->pw_name, ' ') || strchr (buf->pw_name, '\n'))
    {
      intmax_t uid = getuid ();
      char *name = malloc (sizeof uid * CHAR_BIT / 3 + 4);
      if (name)
	sprintf (name, "%"PRIdMAX, uid);
      return name;
    }
  return buf->pw_name;
}

static const char *
get_prefix (bool running_suid, const char *user_prefix)
{
  if (!running_suid && user_prefix == NULL)
    lose ("Not using a shared game directory, and no prefix given.");
  if (running_suid)
    {
#ifdef HAVE_SHARED_GAME_DIR
      return HAVE_SHARED_GAME_DIR;
#else
      lose ("This program was compiled without HAVE_SHARED_GAME_DIR,\n and should not be suid.");
#endif
    }
  return user_prefix;
}

int
main (int argc, char **argv)
{
  int c;
  bool running_suid;
  void *lockstate;
  char *scorefile;
  char *nl;
  const char *prefix, *user_prefix = NULL;
  struct stat buf;
  struct score_entry *scores;
  struct score_entry newscore;
  bool reverse = false;
  ptrdiff_t scorecount, scorealloc;
  ptrdiff_t max_scores = MAX_SCORES;

  srand (time (0));

  while ((c = getopt (argc, argv, "hrm:d:")) != -1)
    switch (c)
      {
      case 'h':
	usage (EXIT_SUCCESS);
	break;
      case 'd':
	user_prefix = optarg;
	break;
      case 'r':
	reverse = 1;
	break;
      case 'm':
	{
	  intmax_t m = strtoimax (optarg, 0, 10);
	  if (m < 0)
	    usage (EXIT_FAILURE);
	  max_scores = min (m, MAX_SCORES);
	}
	break;
      default:
	usage (EXIT_FAILURE);
      }

  if (argc - optind != 3)
    usage (EXIT_FAILURE);

  running_suid = (getuid () != geteuid ());

  prefix = get_prefix (running_suid, user_prefix);

  scorefile = malloc (strlen (prefix) + strlen (argv[optind]) + 2);
  if (!scorefile)
    lose_syserr ("Couldn't allocate score file");

  strcpy (scorefile, prefix);
  strcat (scorefile, "/");
  strcat (scorefile, argv[optind]);

  newscore.score = strtoimax (argv[optind + 1], 0, 10);

  newscore.data = argv[optind + 2];
  if (strlen (newscore.data) > MAX_DATA_LEN)
    newscore.data[MAX_DATA_LEN] = '\0';
  nl = strchr (newscore.data, '\n');
  if (nl)
    *nl = '\0';

  newscore.username = get_user_id ();
  if (! newscore.username)
    lose_syserr ("Couldn't determine user id");

  if (stat (scorefile, &buf) < 0)
    lose_syserr ("Failed to access scores file");

  if (lock_file (scorefile, &lockstate) < 0)
    lose_syserr ("Failed to lock scores file");

  if (read_scores (scorefile, &scores, &scorecount, &scorealloc) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to read scores file");
    }
  if (push_score (&scores, &scorecount, &scorealloc, &newscore) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to add score");
    }
  sort_scores (scores, scorecount, reverse);
  /* Limit the number of scores.  If we're using reverse sorting, then
     also increment the beginning of the array, to skip over the
     *smallest* scores.  Otherwise, just decrementing the number of
     scores suffices, since the smallest is at the end. */
  if (scorecount > max_scores)
    {
      if (reverse)
	scores += scorecount - max_scores;
      scorecount = max_scores;
    }
  if (write_scores (scorefile, scores, scorecount) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to write scores file");
    }
  if (unlock_file (scorefile, lockstate) < 0)
    lose_syserr ("Failed to unlock scores file");
  exit (EXIT_SUCCESS);
}

static int
read_score (FILE *f, struct score_entry *score)
{
  int c;
  if ((c = getc (f)) != EOF)
    ungetc (c, f);
  if (feof (f))
    return 1;
  for (score->score = 0; (c = getc (f)) != EOF && isdigit (c); )
    {
      if (INTMAX_MAX / 10 < score->score)
	return -1;
      score->score *= 10;
      if (INTMAX_MAX - (c - '0') < score->score)
	return -1;
      score->score += c - '0';
    }
  while ((c = getc (f)) != EOF
	 && isspace (c))
    ;
  if (c == EOF)
    return -1;
  ungetc (c, f);
#ifdef HAVE_GETDELIM
  {
    size_t count = 0;
    score->username = 0;
    if (getdelim (&score->username, &count, ' ', f) < 1
	|| score->username == NULL)
      return -1;
    /* Trim the space */
    score->username[strlen (score->username)-1] = '\0';
  }
#else
  {
    ptrdiff_t unameread = 0;
    ptrdiff_t unamelen = 30;
    char *username = malloc (unamelen);
    if (!username)
      return -1;

    while ((c = getc (f)) != EOF && c != ' ')
      {
	if (unameread >= unamelen - 1)
	  {
	    ptrdiff_t unamelen_max = min (PTRDIFF_MAX, SIZE_MAX);
	    if (unamelen <= unamelen_max / 2)
	      unamelen *= 2;
	    else if (unamelen < unamelen_max)
	      unamelen = unamelen_max;
	    else
	      {
		errno = ENOMEM;
		return -1;
	      }
	    username = realloc (username, unamelen);
	    if (!username)
	      return -1;
	  }
	username[unameread] = c;
	unameread++;
      }
    if (c == EOF)
      return -1;
    username[unameread] = '\0';
    score->username = username;
  }
#endif
#ifdef HAVE_GETLINE
  score->data = NULL;
  errno = 0;
  {
    size_t len;
    if (getline (&score->data, &len, f) < 0)
      return -1;
    score->data[strlen (score->data)-1] = '\0';
  }
#else
  {
    ptrdiff_t cur = 0;
    ptrdiff_t len = 16;
    char *buf = malloc (len);
    if (!buf)
      return -1;
    while ((c = getc (f)) != EOF
	   && c != '\n')
      {
	if (cur >= len-1)
	  {
	    if (min (PTRDIFF_MAX, SIZE_MAX) / 2 < len)
	      {
		errno = ENOMEM;
		return -1;
	      }
	    if (!(buf = realloc (buf, len *= 2)))
	      return -1;
	  }
	buf[cur] = c;
	cur++;
      }
    score->data = buf;
    score->data[cur] = '\0';
  }
#endif
  return 0;
}

static int
read_scores (const char *filename, struct score_entry **scores,
	     ptrdiff_t *count, ptrdiff_t *alloc)
{
  int readval = -1;
  ptrdiff_t scorecount = 0;
  ptrdiff_t cursize = 0;
  struct score_entry *ret = 0;
  struct score_entry entry;
  FILE *f = fopen (filename, "r");
  int retval = -1;
  if (!f)
    return -1;
  while ((readval = read_score (f, &entry)) == 0)
    if (push_score (&ret, &scorecount, &cursize, &entry) < 0)
      return -1;
  if (readval > 0 && fclose (f) == 0)
    {
      *count = scorecount;
      *alloc = cursize;
      *scores = ret;
      retval = 0;
    }
  return retval;
}

static int
score_compare (const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  return (sb->score > sa->score) - (sb->score < sa->score);
}

static int
score_compare_reverse (const void *a, const void *b)
{
  return score_compare (b, a);
}

int
push_score (struct score_entry **scores, ptrdiff_t *count, ptrdiff_t *size,
	    struct score_entry const *newscore)
{
  struct score_entry *newscores = *scores;
  if (*count == *size)
    {
      ptrdiff_t newsize = *size;
      if (newsize <= 0)
	newsize = 1;
      else if (newsize <= MAX_SCORES / 2)
	newsize *= 2;
      else if (newsize < MAX_SCORES)
	newsize = MAX_SCORES;
      else
	{
	  errno = ENOMEM;
	  return -1;
	}
      newscores = realloc (newscores, sizeof *newscores * newsize);
      if (!newscores)
	return -1;
      *scores = newscores;
      *size = newsize;
    }
  newscores[*count] = *newscore;
  (*count) += 1;
  return 0;
}

static void
sort_scores (struct score_entry *scores, ptrdiff_t count, bool reverse)
{
  qsort (scores, count, sizeof *scores,
	 reverse ? score_compare_reverse : score_compare);
}

static int
write_scores (const char *filename, const struct score_entry *scores,
	      ptrdiff_t count)
{
  int fd;
  FILE *f;
  ptrdiff_t i;
  char *tempfile = malloc (strlen (filename) + strlen (".tempXXXXXX") + 1);
  if (!tempfile)
    return -1;
  strcpy (tempfile, filename);
  strcat (tempfile, ".tempXXXXXX");
  fd = mkostemp (tempfile, 0);
  if (fd < 0)
    return -1;
#ifndef WINDOWSNT
  if (fchmod (fd, 0644) != 0)
    return -1;
#endif
  f = fdopen (fd, "w");
  if (! f)
    return -1;
  for (i = 0; i < count; i++)
    if (fprintf (f, "%"PRIdMAX" %s %s\n",
		 scores[i].score, scores[i].username, scores[i].data)
	< 0)
      return -1;
  if (fclose (f) != 0)
    return -1;
  if (rename (tempfile, filename) != 0)
    return -1;
#ifdef WINDOWSNT
  if (chmod (filename, 0644) < 0)
    return -1;
#endif
  return 0;
}

static int
lock_file (const char *filename, void **state)
{
  int fd;
  struct stat buf;
  int attempts = 0;
  const char *lockext = ".lockfile";
  char *lockpath = malloc (strlen (filename) + strlen (lockext) + 60);
  if (!lockpath)
    return -1;
  strcpy (lockpath, filename);
  strcat (lockpath, lockext);
  *state = lockpath;
 trylock:
  attempts++;
  /* If the lock is over an hour old, delete it.  */
  if (stat (lockpath, &buf) == 0
      && 60 * 60 < time (0) - buf.st_ctime)
    unlink (lockpath);
  fd = open (lockpath, O_CREAT | O_EXCL, 0600);
  if (fd < 0)
    {
      if (errno == EEXIST)
	{
	  /* Break the lock; we won't corrupt the file, but we might
	     lose some scores. */
	  if (attempts > MAX_ATTEMPTS)
	    {
	      unlink (lockpath);
	      attempts = 0;
	    }
	  sleep ((rand () % 2)+1);
	  goto trylock;
	}
      else
	return -1;
    }
  close (fd);
  return 0;
}

static int
unlock_file (const char *filename, void *state)
{
  char *lockpath = (char *) state;
  int saved_errno = errno;
  int ret = unlink (lockpath);
  int unlink_errno = errno;
  free (lockpath);
  errno = ret < 0 ? unlink_errno : saved_errno;
  return ret;
}

/* update-game-score.c ends here */
