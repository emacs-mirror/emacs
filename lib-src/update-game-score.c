/* update-game-score.c --- Update a score file
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This program is allows a game to securely and atomically update a
   score file.  It should be installed setuid, owned by an appropriate
   user like `games'.

   Alternatively, it can be compiled without HAVE_SHARED_GAME_DIR
   defined, and in that case it will store scores in the user's home
   directory (it should NOT be setuid).

   Created 2002/03/22, by Colin Walters <walters@debian.org>
*/

#define _GNU_SOURCE

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <pwd.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <config.h>

#define MAX_ATTEMPTS 5
#define MAX_SCORES 200
#define MAX_DATA_LEN 1024

#ifdef HAVE_SHARED_GAME_DIR
#define SCORE_FILE_PREFIX HAVE_SHARED_GAME_DIR
#else
#define SCORE_FILE_PREFIX "~/.emacs.d/games"
#endif

#if !defined (__GNUC__) || __GNUC__ < 2
#define __attribute__(x) 
#endif

int
usage(int err)
{
  fprintf(stdout, "Usage: update-game-score [-m MAX ] [ -r ] game/scorefile SCORE DATA\n");
  fprintf(stdout, "       update-game-score -h\n");
  fprintf(stdout, " -h\t\tDisplay this help.\n");
  fprintf(stdout, " -m MAX\t\tLimit the maximum number of scores to MAX.\n");
  fprintf(stdout, " -r\t\tSort the scores in increasing order.\n");
  exit(err);
}

int
lock_file(const char *filename, void **state);
int
unlock_file(const char *filename, void *state);

struct score_entry
{
  long score;
  char *username;
  char *data;
};

int
read_scores(const char *filename, struct score_entry **scores,
	    int *count);
int
push_score(struct score_entry **scores, int *count,
	   int newscore, char *username, char *newdata);
void
sort_scores(struct score_entry *scores, int count, int reverse);
int
write_scores(const char *filename, const struct score_entry *scores,
	     int count);

char *
get_user_id(struct passwd *buf)
{
  char *name;
  if (!buf)
    {
      int count = 1;
      int uid = (int) getuid();
      int tuid = uid;
      while (tuid /= 10)
	count++;
      name = malloc(count+1);
      sprintf(name, "%d", uid);
      return name;
    }
  return buf->pw_name;
}

char *
get_home_dir(struct passwd *buf)
{
  if (!buf)
    return NULL;
  return buf->pw_dir;
}

void lose(const char *msg, ...)
     __attribute__ ((format (printf,1,0), noreturn));

void lose(const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    exit(1);
}

int
main(int argc, char **argv)
{
  int c;
  void *lockstate;
  char *scorefile, *prefix;
  struct stat buf;
  struct score_entry *scores;
  int newscore, scorecount, reverse = 0, max = MAX_SCORES;
  char *newdata;
  struct passwd *passwdbuf;

  srand(time(0));

  while ((c = getopt(argc, argv, "hrm:")) != -1)
    switch (c)
      {
      case 'h':
	usage(0);
	break;
      case 'r':
	reverse = 1;
	break;
      case 'm':
	max = atoi(optarg);
	if (max > MAX_SCORES)
	  max = MAX_SCORES;
	break;
      default:
	usage(1);
      }

  if (optind+3 != argc)
    usage(1);

  passwdbuf = getpwuid(getuid());

  if (!strncmp(SCORE_FILE_PREFIX, "~", 1))
    {
      char *homedir = get_home_dir(passwdbuf);
      if (!homedir)
	lose("Unable to determine home directory\n");
      prefix = malloc(strlen(homedir) + strlen(SCORE_FILE_PREFIX) + 1);
      strcpy(prefix, homedir);
      /* Skip over the '~'. */
      strcat(prefix, SCORE_FILE_PREFIX+1);
    }
  else
    prefix = strdup(SCORE_FILE_PREFIX);

  if (!prefix)
    lose("Couldn't create score file name: %s\n", strerror(errno));

  scorefile = malloc(strlen(prefix) + strlen(argv[optind]) + 2);
  if (!scorefile)
    lose("Couldn't create score file name: %s\n", strerror(errno));

  strcpy(scorefile, prefix);
  free(prefix);
  strcat(scorefile, "/");
  strcat(scorefile, argv[optind]);
  newscore = atoi(argv[optind+1]);
  newdata = argv[optind+2];
  if (strlen(newdata) > MAX_DATA_LEN)
    newdata[MAX_DATA_LEN] = '\0';
  
  if (stat(scorefile, &buf) < 0)
    lose("Failed to access scores file \"%s\": %s\n", scorefile,
	 strerror(errno));
  if (lock_file(scorefile, &lockstate) < 0)
      lose("Failed to lock scores file \"%s\": %s\n",
	   scorefile, strerror(errno));
  if (read_scores(scorefile, &scores, &scorecount) < 0)
    {
      unlock_file(scorefile, lockstate);
      lose("Failed to read scores file \"%s\": %s\n", scorefile,
	   strerror(errno));
    }
  push_score(&scores, &scorecount, newscore, get_user_id(passwdbuf), newdata);
  /* Limit the number of scores.  If we're using reverse sorting, then
     we should increment the beginning of the array, to skip over the
     *smallest* scores.  Otherwise, we just decrement the number of
     scores, since the smallest will be at the end. */
  if (scorecount > MAX_SCORES)
    scorecount -= (scorecount - MAX_SCORES);
    if (reverse)
      scores += (scorecount - MAX_SCORES);
  sort_scores(scores, scorecount, reverse);
  if (write_scores(scorefile, scores, scorecount) < 0)
    {
      unlock_file(scorefile, lockstate);
      lose("Failed to write scores file \"%s\": %s\n", scorefile,
	   strerror(errno));
    }
  unlock_file(scorefile, lockstate);
  exit(0);
}

int
read_score(FILE *f, struct score_entry *score)
{
  int c;
  if (feof(f))
    return 1;
  while ((c = getc(f)) != EOF
	 && isdigit(c))
    {
      score->score *= 10;
      score->score += (c-48);
    }
  while ((c = getc(f)) != EOF
	 && isspace(c))
    ;
  if (c == EOF)
    return -1;
  ungetc(c, f);
#ifdef HAVE_GETDELIM
  {
    size_t count = 0;
    if (getdelim(&score->username, &count, ' ', f) < 1
	|| score->username == NULL)
      return -1;
  }
#else
  {
    int unameread = 0;
    int unamelen = 30;
    char *username = malloc(unamelen);
    if (!username)
      return -1;
    
    while ((c = getc(f)) != EOF
	   && !isspace(c))
      {
	if (unameread >= unamelen-1)
	  if (!(username = realloc(username, unamelen *= 2)))
	    return -1;
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
    if (getline(&score->data, &len, f) < 0)
      return -1;
    score->data[strlen(score->data)-1] = '\0';
  }
#else
  {
    int cur = 0;
    int len = 16;
    char *buf = malloc(len);
    if (!buf)
      return -1;
    while ((c = getc(f)) != EOF
	   && c != '\n')
      {
	if (cur >= len-1)
	  {
	    if (!(buf = realloc(buf, len *= 2)))
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

int
read_scores(const char *filename, struct score_entry **scores,
	    int *count)
{
  int readval, scorecount, cursize;
  struct score_entry *ret;
  FILE *f = fopen(filename, "r");
  if (!f) 
    return -1;
  scorecount = 0;
  cursize = 16;
  ret = malloc(sizeof(struct score_entry) * cursize);
  if (!ret) 
    return -1;
  while ((readval = read_score(f, &ret[scorecount])) == 0)
    {
      /* We encoutered an error */
      if (readval < 0)
	return -1;
      scorecount++;
      if (scorecount >= cursize)
	{
	  ret = realloc(ret, cursize *= 2);
	  if (!ret)
	    return -1;
	}
    }
  *count = scorecount;
  *scores = ret;
  return 0;
}

int
score_compare(const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  return (sb->score > sa->score) - (sb->score < sa->score);
}

int
score_compare_reverse(const void *a, const void *b)
{
  const struct score_entry *sa = (const struct score_entry *) a;
  const struct score_entry *sb = (const struct score_entry *) b;
  return (sa->score > sb->score) - (sa->score < sb->score);
}

int
push_score(struct score_entry **scores, int *count,
	   int newscore, char *username, char *newdata) 
{
 struct score_entry *newscores = realloc(*scores,
					 sizeof(struct score_entry) * ((*count) + 1));
  if (!newscores)
    return -1;
  newscores[*count].score = newscore;
  newscores[*count].username = username;
  newscores[*count].data = newdata;
  (*count) += 1;
  *scores = newscores;
  return 0;
}
  
void
sort_scores(struct score_entry *scores, int count, int reverse)
{
  qsort(scores, count, sizeof(struct score_entry),
	reverse ? score_compare_reverse : score_compare);
}

int
write_scores(const char *filename, const struct score_entry *scores,
	     int count)
{
  FILE *f;  
  int i;
  char *tempfile = malloc(strlen(filename) + strlen(".tempXXXXXX") + 1);
  if (!tempfile)
    return -1;
  strcpy(tempfile, filename);
  strcat(tempfile, ".tempXXXXXX");
#ifdef HAVE_MKSTEMP
  if (mkstemp(tempfile) < 0
#else
  if (mktemp(tempfile) != tempfile
#endif
      || !(f = fopen(tempfile, "w")))
    return -1;
  for (i = 0; i < count; i++)
    if (fprintf(f, "%ld %s %s\n", scores[i].score, scores[i].username,
		scores[i].data) < 0)
      return -1;
  fclose(f);
  if (rename(tempfile, filename) < 0)
    return -1;
  if (chmod(filename, 0644) < 0)
    return -1;
  return 0;
}

int
lock_file(const char *filename, void **state)
{
  int fd;
  struct stat buf;
  int attempts = 0;
  char *lockext = ".lockfile";
  char *lockpath = malloc(strlen(filename) + strlen(lockext) + 60);
  if (!lockpath)
    return -1;
  strcpy(lockpath, filename);
  strcat(lockpath, lockext);
  *state = lockpath;
 trylock:
  attempts++;
  /* If the lock is over an hour old, delete it. */
  if (stat(lockpath, &buf) == 0
      && (difftime(buf.st_ctime, time(NULL) > 60*60)))
    unlink(lockpath);
  if ((fd = open(lockpath, O_CREAT | O_EXCL, 0600)) < 0)
    {
      if (errno == EEXIST)
	{
	  /* Break the lock; we won't corrupt the file, but we might
	     lose some scores. */
	  if (attempts > MAX_ATTEMPTS)
	    {
	      unlink(lockpath);
	      attempts = 0;
	    }
	  sleep((rand() % 2)+1);
	  goto trylock;
	}
      else
	return -1;
    }
  close(fd);
  return 0;
}

int
unlock_file(const char *filename, void *state)
{
  char *lockpath = (char *) state;
  int ret = unlink(lockpath);
  int saved_errno = errno;
  free(lockpath);
  errno = saved_errno;
  return ret;
}
