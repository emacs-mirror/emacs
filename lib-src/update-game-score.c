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
   score file.  It should be installed setgid, owned by an appropriate
   group like `games'.

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
#include <sys/stat.h>
#include <config.h>

#define MAX_ATTEMPTS 5
#define SCORE_FILE_PREFIX "/var/games/emacs/"

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
get_user_id()
{
  char *name;
  struct passwd *buf = getpwuid(getuid());
  if (!buf)
    {
      int count = 1;
      int uid = (int) getuid();
      while (uid /= 10)
	count++;
      name = malloc(count+1);
      sprintf(name, "%d", uid);
      return name;
    }
  return buf->pw_name;
}

int
main(int argc, char **argv)
{
  int c;
  void *lockstate;
  char *scorefile;
  struct stat buf;
  struct score_entry *scores;
  int newscore, scorecount, reverse = 0, max = -1;
  char *newdata;

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
	break;
      default:
	usage(1);
      }

  if (optind+3 != argc)
    usage(1);
  scorefile = malloc(strlen(SCORE_FILE_PREFIX) + strlen(argv[optind]) + 1);
  if (!scorefile)
    {
      fprintf(stderr, "Couldn't create score file name: %s\n",
	      strerror(errno));
      goto fail;
    }
  strcpy(scorefile, SCORE_FILE_PREFIX);
  strcat(scorefile, argv[optind]);
  newscore = atoi(argv[optind+1]);
  newdata = argv[optind+2];
  
  if (stat(scorefile, &buf) < 0)
    {
      fprintf(stderr, "Failed to access scores file \"%s\": %s\n",
	      scorefile, strerror(errno));
      goto fail;
    }
  if (lock_file(scorefile, &lockstate) < 0)
    {
      fprintf(stderr, "Failed to lock scores file \"%s\": %s\n",
	      scorefile, strerror(errno));
      goto fail;
    }
  if (read_scores(scorefile, &scores, &scorecount) < 0)
    {
      fprintf(stderr, "Failed to read scores file \"%s\": %s\n",
	      scorefile, strerror(errno));
      goto fail_unlock;
    }
  push_score(&scores, &scorecount, newscore, get_user_id(), newdata);
  sort_scores(scores, scorecount, reverse);
  if (write_scores(scorefile, scores, scorecount) < 0)
    {
      fprintf(stderr, "Failed to write scores file \"%s\": %s\n",
	      scorefile, strerror(errno));
      goto fail_unlock;
    }
  unlock_file(scorefile, lockstate);
  exit(0);
 fail_unlock:
  unlock_file(scorefile, lockstate);
 fail:
  exit(1);
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
#ifdef HAVE_GETDELIM
  {
    int count = 0;
    if (getdelim(&score->username, &count, ' ', f) < 1
	|| score->username == NULL)
      return -1;
  }
#else
  {
    int unameread = 0;
    int unamelen = 30;
    char *username;
    
    while ((c = getc(f)) != EOF
	   && !isspace(c))
      {
	if (unameread == unamelen)
	  {
	    if (!(username = realloc(username, unamelen *= 2)))
	      return -1;
	  }
	username[unameread] = c;
	unameread++;
      }
    score->username = username;
  }
#endif
#ifdef HAVE_GETLINE
  score->data = NULL;
  errno = ESUCCES;
  {
    int len;
    if (getline(&score->data, &len, f) < 0)
      return -1;
  }
#else
  {
    int cur = 0;
    int len = 16;
    char *buf = malloc(len);
    if (!buf)
      return -1;
    while ((c = getc(f)) != EOF)
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
    score->data[cur+1] = '\0';
  }
#endif
  /* Trim the newline */
  score->data[strlen(score->data)-1] = '\0';
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
  if (mkstemp(tempfile) < 0
      || !(f = fopen(tempfile, "w")))
    return -1;
  for (i = 0; i < count; i++)
    if (fprintf(f, "%ld %s %s\n", scores[i].score, scores[i].username,
		scores[i].data) < 0)
      return -1;
  fclose(f);
  rename(tempfile, filename);
  return 0;
}

#if 0
int
lock_file(const char *filename, void **state)
{
  struct flock lock;
  int *istate;
  int fd, attempts = 0, ret = 0;
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 0;
  istate = malloc(sizeof(int));
  if (!istate)
    return -1;
  if ((fd = open(filename, O_RDWR, 0600)) < 0)
    return -1;
  *istate = fd;
 trylock:
  attempts++;
  if ((ret = fcntl(fd, F_GETLK, &lock)) == -1)
    {
      if (ret == EACCES || ret == EAGAIN)
	if (attempts > MAX_ATTEMPTS)
	  exit(1);
	else
	  {
	    sleep((rand() % 3)+1);
	    goto trylock;
	  }
      else
	ret = 0 ;
    }
  else
    ret = 0;
  *state = istate;
  return ret;
}

int
unlock_file(const char *filename, void *state)
{
  int fd, ret;
  fd = *((int *) state);
  free(state);
  ret = close(fd);
  return ret;
}

#else

int
lock_file(const char *filename, void **state)
{
  int fd;
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
  if ((fd = open(lockpath, O_CREAT | O_EXCL, 0600)) < 0)
    {
      if (errno == EEXIST)
	{
	  /* Break the lock; we won't corrupt the file, but we might
	     lose some scores. */
	  if (attempts > MAX_ATTEMPTS)
	    unlink(lockpath);
	  else
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

#endif
