/* pop.h: Header file for the "pop.c" client POP3 protocol.
   Copyright (c) 1991,1993 Free Software Foundation, Inc.
   Written by Jonathan Kamens, jik@security.ov.com.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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
extern char *pop_retrieve _ARGS((popserver server, int message, int markfrom));
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
