/* update-game-score.c --- Update a score file

Copyright (C) 2002-2020 Free Software Foundation, Inc.

Author: Colin Walters <walters@debian.org>

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


/* This program allows a game to securely and atomically update a
   score file.  It should be installed either setuid or setgid, owned
   by an appropriate user or group like `games'.

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
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <pwd.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <getopt.h>

#include <unlocked-io.h>

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
  char *score;
  char *user_data;
};

#define MAX_SCORES min (PTRDIFF_MAX, SIZE_MAX / sizeof (struct score_entry))

static int read_scores (const char *filename, struct score_entry **scores,
			ptrdiff_t *count, ptrdiff_t *alloc);
static int push_score (struct score_entry **scores, ptrdiff_t *count,
		       ptrdiff_t *size, struct score_entry const *newscore);
static void sort_scores (struct score_entry *scores, ptrdiff_t count,
			 bool reverse);
static int write_scores (const char *filename, mode_t mode,
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
  fprintf (stderr, "%s: %s\n", msg,
	   errno ? strerror (errno) : "Invalid data in score file");
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
get_prefix (bool privileged, const char *user_prefix)
{
  if (privileged)
    {
#ifdef HAVE_SHARED_GAME_DIR
      return HAVE_SHARED_GAME_DIR;
#else
      lose ("This program was compiled without HAVE_SHARED_GAME_DIR,\n"
	    "and should not run with elevated privileges.");
#endif
    }
  if (user_prefix == NULL)
    lose ("Not using a shared game directory, and no prefix given.");
  return user_prefix;
}

static char *
normalize_integer (char *num)
{
  bool neg;
  char *p;
  while (*num != '\n' && isspace (*num))
    num++;
  neg = *num == '-';
  num += neg || *num == '-';

  if (*num == '0')
    {
      while (*++num == '0')
	continue;
      neg &= !!*num;
      num -= !*num;
    }

  for (p = num; '0' <= *p && *p <= '9'; p++)
    continue;

  if (*p || p == num)
    {
      errno = 0;
      return 0;
    }

  if (neg)
    *--num = '-';
  return num;
}

int
main (int argc, char **argv)
{
  int c;
  bool running_suid, running_sgid;
  void *lockstate;
  char *scorefile;
  char *end, *nl, *user, *data;
  const char *prefix, *user_prefix = NULL;
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
	  intmax_t m = strtoimax (optarg, &end, 10);
	  if (optarg == end || *end || m < 0)
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
  running_sgid = (getgid () != getegid ());
  if (running_suid && running_sgid)
    lose ("This program can run either suid or sgid, but not both.");

  prefix = get_prefix (running_suid || running_sgid, user_prefix);

  scorefile = malloc (strlen (prefix) + strlen (argv[optind]) + 2);
  if (!scorefile)
    lose_syserr ("Couldn't allocate score file");

  char *z = stpcpy (scorefile, prefix);
  *z++ = '/';
  strcpy (z, argv[optind]);

  newscore.score = normalize_integer (argv[optind + 1]);
  if (! newscore.score)
    {
      fprintf (stderr, "%s: Invalid score\n", argv[optind + 1]);
      return EXIT_FAILURE;
    }

  user = get_user_id ();
  if (! user)
    lose_syserr ("Couldn't determine user id");
  data = argv[optind + 2];
  if (strnlen (data, MAX_DATA_LEN + 1) == MAX_DATA_LEN + 1)
    data[MAX_DATA_LEN] = '\0';
  nl = strchr (data, '\n');
  if (nl)
    *nl = '\0';
  newscore.user_data = malloc (strlen (user) + 1 + strlen (data) + 1);
  if (! newscore.user_data
      || sprintf (newscore.user_data, "%s %s", user, data) < 0)
    lose_syserr ("Memory exhausted");

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
  if (write_scores (scorefile, running_sgid ? 0664 : 0644,
		    scores, scorecount) < 0)
    {
      unlock_file (scorefile, lockstate);
      lose_syserr ("Failed to write scores file");
    }
  if (unlock_file (scorefile, lockstate) < 0)
    lose_syserr ("Failed to unlock scores file");
  exit (EXIT_SUCCESS);
}

static char *
read_score (char *p, struct score_entry *score)
{
  score->score = p;
  p = strchr (p, ' ');
  if (!p)
    return p;
  *p++ = 0;
  score->user_data = p;
  p = strchr (p, '\n');
  if (!p)
    return p;
  *p++ = 0;
  return p;
}

static int
read_scores (const char *filename, struct score_entry **scores,
	     ptrdiff_t *count, ptrdiff_t *alloc)
{
  char *p, *filedata;
  ptrdiff_t filesize, nread;
  struct stat st;
  FILE *f = fopen (filename, "r");
  if (!f)
    return -1;
  if (fstat (fileno (f), &st) != 0)
    return -1;
  if (! (0 <= st.st_size && st.st_size < min (PTRDIFF_MAX, SIZE_MAX)))
    {
      errno = EOVERFLOW;
      return -1;
    }
  filesize = st.st_size;
  filedata = malloc (filesize + 1);
  if (! filedata)
    return -1;
  nread = fread (filedata, 1, filesize + 1, f);
  if (filesize < nread)
    {
      errno = 0;
      return -1;
    }
  if (nread < filesize)
    filesize = nread;
  if (ferror (f) || fclose (f) != 0)
    return -1;
  filedata[filesize] = 0;
  if (strlen (filedata) != filesize)
    {
      errno = 0;
      return -1;
    }

  *scores = 0;
  *count = *alloc = 0;
  for (p = filedata; p < filedata + filesize; )
    {
      struct score_entry entry;
      p = read_score (p, &entry);
      if (!p)
	{
	  errno = 0;
	  return -1;
	}
      if (push_score (scores, count, alloc, &entry) < 0)
	return -1;
    }
  return 0;
}

static int
score_compare (const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  char *sca = sa->score;
  char *scb = sb->score;
  size_t lena, lenb;
  bool nega = *sca == '-';
  bool negb = *scb == '-';
  int diff = nega - negb;
  if (diff)
    return diff;
  if (nega)
    {
      char *tmp = sca;
      sca = scb + 1;
      scb = tmp + 1;
    }
  lena = strlen (sca);
  lenb = strlen (scb);
  if (lena != lenb)
    return lenb < lena ? -1 : 1;
  return strcmp (scb, sca);
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
write_scores (const char *filename, mode_t mode,
	      const struct score_entry *scores, ptrdiff_t count)
{
  int fd;
  FILE *f;
  ptrdiff_t i;
  char *tempfile = malloc (strlen (filename) + strlen (".tempXXXXXX") + 1);
  if (!tempfile)
    return -1;
  strcpy (stpcpy (tempfile, filename), ".tempXXXXXX");
  fd = mkostemp (tempfile, 0);
  if (fd < 0)
    return -1;
#ifndef DOS_NT
  if (fchmod (fd, mode) != 0)
    return -1;
#endif
  f = fdopen (fd, "w");
  if (! f)
    return -1;
  for (i = 0; i < count; i++)
    if (fprintf (f, "%s %s\n", scores[i].score, scores[i].user_data) < 0)
      return -1;
  if (fclose (f) != 0)
    return -1;
  if (rename (tempfile, filename) != 0)
    return -1;
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
  strcpy (stpcpy (lockpath, filename), lockext);
  *state = lockpath;

  while ((fd = open (lockpath, O_CREAT | O_EXCL, 0600)) < 0)
    {
      if (errno != EEXIST)
	return -1;
      attempts++;

      /* Break the lock if it is over an hour old, or if we've tried
	 more than MAX_ATTEMPTS times.  We won't corrupt the file, but
	 we might lose some scores. */
      if (MAX_ATTEMPTS < attempts
	  || (stat (lockpath, &buf) == 0 && 60 * 60 < time (0) - buf.st_ctime))
	{
	  if (unlink (lockpath) != 0 && errno != ENOENT)
	    return -1;
	  attempts = 0;
	}

      sleep ((rand () & 1) + 1);
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
