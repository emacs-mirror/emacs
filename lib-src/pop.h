/* pop.h: Header file for the "pop.c" client POP3 protocol.
   Copyright (C) 1991, 1993, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

Author:  Jonathan Kamens <jik@security.ov.com>

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


#include <stdio.h>

#define GETLINE_MIN 1024	/* the getline buffer starts out this */
				/* size */
#define GETLINE_INCR 1024	/* the getline buffer is grown by this */
				/* size when it needs to grow */

extern char pop_error[];
extern int pop_debug;

struct _popserver
{
  int file, data;
  char *buffer;
  int buffer_size, buffer_index;
  int in_multi;
  int trash_started;
};

typedef struct _popserver *popserver;

/*
 * Valid flags for the pop_open function.
 */

#define POP_NO_KERBEROS	(1<<0)
#define POP_NO_HESIOD	(1<<1)
#define POP_NO_GETPASS 	(1<<2)

#ifdef __STDC__
#define _ARGS(a) a
#else
#define _ARGS(a) ()
#endif

extern popserver pop_open _ARGS((char *host, char *username, char *password,
				 int flags));
extern int pop_stat _ARGS((popserver server, int *count, int *size));
extern int pop_list _ARGS((popserver server, int message, int **IDs,
			   int **size));
extern int pop_retrieve _ARGS((popserver server, int message, int markfrom,
			       char **));
extern int pop_retrieve_first _ARGS((popserver server, int message,
				     char **response));
extern int pop_retrieve_next _ARGS((popserver server, char **line));
extern int pop_retrieve_flush _ARGS((popserver server));
extern int pop_top_first _ARGS((popserver server, int message, int lines,
				char **response));
extern int pop_top_next _ARGS((popserver server, char **line));
extern int pop_top_flush _ARGS((popserver server));
extern int pop_multi_first _ARGS((popserver server, char *command,
				  char **response));
extern int pop_multi_next _ARGS((popserver server, char **line));
extern int pop_multi_flush _ARGS((popserver server));
extern int pop_delete _ARGS((popserver server, int message));
extern int pop_noop _ARGS((popserver server));
extern int pop_last _ARGS((popserver server));
extern int pop_reset _ARGS((popserver server));
extern int pop_quit _ARGS((popserver server));
extern void pop_close _ARGS((popserver));

#undef _ARGS

/* arch-tag: 76cc5f58-8e86-48fa-bc72-a7c6cb1c4f1c
   (do not change this comment) */
