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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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
  void *extra;
};

typedef struct _popserver *popserver;

/*
 * Valid flags for the pop_open function.
 */

#define POP_NO_KERBEROS	(1<<0)
#define POP_NO_HESIOD	(1<<1)
#define POP_NO_GETPASS 	(1<<2)
#define POP_NO_GSSAPI	(1<<3)	/* don't use the GSSAPI */
#define POP_NO_NOPROT	(1<<4)	/* prohibit no protection; this *only* */
				/* makes sense if you use GSSAPI */
#define POP_NO_INTEG	(1<<5)	/* don't use plain integrity */
#define POP_NO_ENCRYPT	(1<<6)	/* don't use encryption */

/*
 * GSSAPI documentation
 *
 * This version will attempt to perform a GSSAPI handshake first; if this
 * fails, then it will attempt standard POP authentication.  Note that
 * library conflicts may prevent the use of this with the Kerberos
 * kpop hack.
 *
 * If you specify POP_NO_NOPROT and this library is unable to provide either
 * integrity protection or encryption, pop_open() will fail.  The pop_open()
 * call will attempt the highest level protection available; i.e., if both
 * server and client support encryption (and you do not provide the
 * POP_NO_ENCRYPT flag), that will be used; if both server and client support
 * integrity protection (and you do not provide the POP_NO_INTEG flag), that
 * will be used.  If neither of these are available, and you have not
 * specified the POP_NO_NOPROT flag, then this will be a normal, unprotected
 * connection.
 */

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
