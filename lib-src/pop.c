/* pop.c: client routines for talking to a POP3-protocol post-office server
   Copyright (c) 1991, 1993, 1996, 1997 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
#define NO_SHORTNAMES	/* Tell config not to load remap.h */
#include <../src/config.h>
#else
#define MAIL_USE_POP
#endif

#ifdef MAIL_USE_POP

#ifdef HAVE_CONFIG_H
/* Cancel these substitutions made in config.h */
#undef open
#undef read
#undef write
#undef close
#endif

#include <sys/types.h>
#ifdef WINDOWSNT
#include "ntlib.h"
#include <winsock.h>
#undef SOCKET_ERROR
#define RECV(s,buf,len,flags) recv(s,buf,len,flags)
#define SEND(s,buf,len,flags) send(s,buf,len,flags)
#define CLOSESOCKET(s) closesocket(s)
#else
#include <netinet/in.h>
#include <sys/socket.h>
#define RECV(s,buf,len,flags) read(s,buf,len)
#define SEND(s,buf,len,flags) write(s,buf,len)
#define CLOSESOCKET(s) close(s)
#endif
#include <pop.h>

#ifdef sun
#include <malloc.h>
#endif /* sun */

#ifdef HESIOD
#include <hesiod.h>
/*
 * It really shouldn't be necessary to put this declaration here, but
 * the version of hesiod.h that Athena has installed in release 7.2
 * doesn't declare this function; I don't know if the 7.3 version of
 * hesiod.h does.
 */
extern struct servent *hes_getservbyname (/* char *, char * */);
#endif

#include <pwd.h>
#include <netdb.h>
#include <errno.h>
#include <stdio.h>
#ifdef STDC_HEADERS
#include <string.h>
#define index strchr
#endif
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef KERBEROS
# ifdef HAVE_KRB5_H
#  include <krb5.h>
# endif
# ifdef HAVE_DES_H
#  include <des.h>
# else
#  ifdef HAVE_KERBEROSIV_DES_H
#   include <kerberosIV/des.h>
#  else
#   ifdef HAVE_KERBEROS_DES_H
#    include <kerberos/des.h>
#   endif
#  endif
# endif
# ifdef HAVE_KRB_H
#  include <krb.h>
# else
#  ifdef HAVE_KERBEROSIV_KRB_H
#   include <kerberosIV/krb.h>
#  else
#   ifdef HAVE_KERBEROS_KRB_H
#    include <kerberos/krb.h>
#   endif
#  endif
# endif
# ifdef HAVE_COM_ERR_H
#  include <com_err.h>
# endif
#endif /* KERBEROS */

#ifdef KERBEROS
#ifndef KERBEROS5
extern int krb_sendauth (/* long, int, KTEXT, char *, char *, char *,
			    u_long, MSG_DAT *, CREDENTIALS *, Key_schedule,
			    struct sockaddr_in *, struct sockaddr_in *,
			    char * */);
extern char *krb_realmofhost (/* char * */);
#endif /* ! KERBEROS5 */
#endif /* KERBEROS */

#ifndef WINDOWSNT
#if !defined(HAVE_H_ERRNO) || !defined(HAVE_CONFIG_H)
extern int h_errno;
#endif
#endif

#ifndef _P
# ifdef __STDC__
#  define _P(a) a
# else
#  define _P(a) ()
# endif /* __STDC__ */
#endif /* ! __P */

static int socket_connection _P((char *, int));
static int pop_getline _P((popserver, char **));
static int sendline _P((popserver, char *));
static int fullwrite _P((int, char *, int));
static int getok _P((popserver));
#if 0
static int gettermination _P((popserver));
#endif
static void pop_trash _P((popserver));
static char *find_crlf _P((char *, int));

#define ERROR_MAX 80		/* a pretty arbitrary size */
#define POP_PORT 110
#define KPOP_PORT 1109
#ifdef WINDOWSNT
#define POP_SERVICE "pop3"	/* we don't want the POP2 port! */
#else
#define POP_SERVICE "pop"
#endif
#ifdef KERBEROS
#define KPOP_SERVICE "kpop"
#endif

#ifdef GSSAPI
# ifdef HAVE_GSSAPI_H
#  include <gssapi.h>
# else
#  include <gssapi/gssapi.h>
# endif
#define GSSAPI_SERVICE "pop"
static int pop_auth (/* popserver server, char *user,
			char *host, int flags */);
static void gen_gss_error (/* char *msg, OM_uint32 major, OM_uint32 minor */);
struct _pop_gssapi
{
  int gss_flags;		/* encryption?  integrity protection? */
  OM_uint32 max_size;		/* max size we can send the server */
  gss_ctx_id_t gss_context;	/* the security context */
};
#define GSSAPI_NOPROT		0x01
#define GSSAPI_INTEGRITY	0x02
#define GSSAPI_PRIVACY		0x04
#define GSSAPI_NEEDWRAP		(GSSAPI_INTEGRITY|GSSAPI_PRIVACY)
#define GSSAPI_PROTECTION	(GSSAPI_NOPROT|GSSAPI_INTEGRITY|GSSAPI_PRIVACY)
#define GSSAPI_RCVBUF		1024
#define GSSAPI_SVC_TYPE	{10, "\052\206\110\206\367\022\001\002\001\004"}
#define Gssapi(data)		((struct _pop_gssapi *) (data))

static int b64_decode (/* char *enc, gss_buffer_t dec */);
static int b64_encode (/* gss_buffer_t dec, char **enc */);
#define B64_SUCCESS	0
#define B64_BADPARAM	1
#define B64_BADCHAR	2
#define B64_BADPAD	3
#define B64_BADLEN	4
#define B64_NOMEM	5
static char *b64_error[] =
{
  "Success",
  "Bad parameters",
  "Bad characters in encoding",
  "Bad padding in encoding",
  "Bad length",
  "Out of memory"
};

/*
 * This function is only needed if you are using the GSSAPI protection
 * mechanisms; it keeps trying until it has read the requested number
 * bytes from the passed-in fd.
 */
static int fullread (/* int fd, char *buf, int nbytes */);
#endif /* GSSAPI */

char pop_error[ERROR_MAX];
int pop_debug = 0;

#ifndef min
#define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

/*
 * Function: pop_open (char *host, char *username, char *password,
 * 		       int flags)
 *
 * Purpose: Establishes a connection with a post-office server, and
 * 	completes the authorization portion of the session.
 *
 * Arguments:
 * 	host	The server host with which the connection should be
 * 		established.  Optional.  If omitted, internal
 * 		heuristics will be used to determine the server host,
 * 		if possible.
 * 	username
 * 		The username of the mail-drop to access.  Optional.
 * 		If omitted, internal heuristics will be used to
 * 		determine the username, if possible.
 * 	password
 * 		The password to use for authorization.  If omitted,
 * 		internal heuristics will be used to determine the
 * 		password, if possible.
 * 	flags	A bit mask containing flags controlling certain
 * 		functions of the routine.  Valid flags are defined in
 * 		the file pop.h
 *
 * Return value: Upon successful establishment of a connection, a
 * 	non-null popserver will be returned.  Otherwise, null will be
 * 	returned, and the string variable pop_error will contain an
 * 	explanation of the error.
 */
popserver
pop_open (host, username, password, flags)
     char *host;
     char *username;
     char *password;
     int flags;
{
  int sock;
  popserver server;

  /* Determine the user name */
  if (! username)
    {
      username = getenv ("USER");
      if (! (username && *username))
	{
	  username = getlogin ();
	  if (! (username && *username))
	    {
	      struct passwd *passwd;
	      passwd = getpwuid (getuid ());
	      if (passwd && passwd->pw_name && *passwd->pw_name)
		{
		  username = passwd->pw_name;
		}
	      else
		{
		  strcpy (pop_error, "Could not determine username");
		  return (0);
		}
	    }
	}
    }

  /*
   *  Determine the mail host.
   */

  if (! host)
    {
      host = getenv ("MAILHOST");
    }

#ifdef HESIOD
  if ((! host) && (! (flags & POP_NO_HESIOD)))
    {
      struct hes_postoffice *office;
      office = hes_getmailhost (username);
      if (office && office->po_type && (! strcmp (office->po_type, "POP"))
	  && office->po_name && *office->po_name && office->po_host
	  && *office->po_host)
	{
	  host = office->po_host;
	  username = office->po_name;
	}
    }
#endif

#ifdef MAILHOST
  if (! host)
    {
      host = MAILHOST;
    }
#endif

  if (! host)
    {
      strcpy (pop_error, "Could not determine POP server");
      return (0);
    }

  /* Determine the password */
#if defined(KERBEROS) || defined(GSSAPI)
# ifdef KERBEROS
#  define NO_KERBEROS	POP_NO_KERBEROS
# else
#  define NO_KERBEROS	0
# endif /* KERBEROS */

# ifdef GSSAPI
#  define NO_GSSAPI	POP_NO_GSSAPI
# else
#  define NO_GSSAPI	0
# endif /* GSSAPI */

# define DONT_NEED_PASSWORD (! (flags & (NO_KERBEROS | NO_GSSAPI)))
#else
# define DONT_NEED_PASSWORD 0
#endif
 
  if ((! password) && (! DONT_NEED_PASSWORD))
    {
      if (! (flags & POP_NO_GETPASS))
	{
	  password = getpass ("Enter POP password:");
	}
      if (! password)
	{
	  strcpy (pop_error, "Could not determine POP password");
	  return (0);
	}
    }
  if (password)
    flags |= POP_NO_KERBEROS | (!(flags & POP_NO_NOPROT) ? POP_NO_GSSAPI : 0);
  else
    password = username;

  sock = socket_connection (host, flags);
  if (sock == -1)
    return (0);

  server = (popserver) malloc (sizeof (struct _popserver));
  if (! server)
    {
      strcpy (pop_error, "Out of memory in pop_open");
      return (0);
    }
  server->buffer = (char *) malloc (GETLINE_MIN);
  if (! server->buffer)
    {
      strcpy (pop_error, "Out of memory in pop_open");
      free ((char *) server);
      return (0);
    }
	  
  server->file = sock;
  server->data = 0;
  server->buffer_index = 0;
  server->buffer_size = GETLINE_MIN;
  server->in_multi = 0;
  server->trash_started = 0;
  server->extra = 0;

  if (getok (server))
    return (0);

#ifdef GSSAPI
  /*
   * unless forbidden to use GSSAPI, try the GSSAPI AUTH mechanism..first.
   */
  pop_error[0] = '\0'; /* so we can detect errors later... */
  if (! (flags & POP_NO_GSSAPI))
    {
      int ret;

      ret = pop_auth (server, username, host, flags);
      if (ret == 0)
	{
	  return (server);
	}
      else if (ret == -2)
	{
	  pop_close (server);
	  return (0);
	}
    }
#endif /* GSSAPI */
  /*
   * POP_NO_NOPROT is used in the case that we want protection; if
   * the authentication negotiation failed, then we want to fail now.
   */
  if ((flags & POP_NO_NOPROT))
    {
      pop_close (server);
#ifdef GSSAPI
      if (pop_error[0] == '\0')
#endif
	strcpy (pop_error, "Unable to provide protection");
      return (0);
    }

  /*
   * I really shouldn't use the pop_error variable like this, but....
   */
  if (strlen (username) > ERROR_MAX - 6)
    {
      pop_close (server);
      strcpy (pop_error,
	      "Username too long; recompile pop.c with larger ERROR_MAX");
      return (0);
    }
  sprintf (pop_error, "USER %s", username);

  if (sendline (server, pop_error) || getok (server))
    {
      return (0);
    }

  if (strlen (password) > ERROR_MAX - 6)
    {
      pop_close (server);
      strcpy (pop_error,
	      "Password too long; recompile pop.c with larger ERROR_MAX");
      return (0);
    }
  sprintf (pop_error, "PASS %s", password);

  if (sendline (server, pop_error) || getok (server))
    {
      return (0);
    }

  return (server);
}

/*
 * Function: pop_stat
 *
 * Purpose: Issue the STAT command to the server and return (in the
 * 	value parameters) the number of messages in the maildrop and
 * 	the total size of the maildrop.
 *
 * Return value: 0 on success, or non-zero with an error in pop_error
 * 	in failure.
 *
 * Side effects: On failure, may make further operations on the
 * 	connection impossible.
 */
int
pop_stat (server, count, size)
     popserver server;
     int *count;
     int *size;
{
  char *fromserver;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_stat");
      return (-1);
    }
     
  if (sendline (server, "STAT") || (pop_getline (server, &fromserver) < 0))
    return (-1);

  if (strncmp (fromserver, "+OK ", 4))
    {
      if (0 == strncmp (fromserver, "-ERR", 4))
	{
	  strncpy (pop_error, fromserver, ERROR_MAX);
	}
      else
	{
	  strcpy (pop_error,
		  "Unexpected response from POP server in pop_stat");
	  pop_trash (server);
	}
      return (-1);
    }

  *count = atoi (&fromserver[4]);
     
  fromserver = index (&fromserver[4], ' ');
  if (! fromserver)
    {
      strcpy (pop_error,
	      "Badly formatted response from server in pop_stat");
      pop_trash (server);
      return (-1);
    }

  *size = atoi (fromserver + 1);

  return (0);
}

/*
 * Function: pop_list
 *
 * Purpose: Performs the POP "list" command and returns (in value
 * 	parameters) two malloc'd zero-terminated arrays -- one of
 * 	message IDs, and a parallel one of sizes.
 *
 * Arguments:
 * 	server	The pop connection to talk to.
 * 	message	The number of the one message about which to get
 * 		information, or 0 to get information about all
 * 		messages.
 *
 * Return value: 0 on success, non-zero with error in pop_error on
 * 	failure.
 *
 * Side effects: On failure, may make further operations on the
 * 	connection impossible.
 */
int
pop_list (server, message, IDs, sizes)
     popserver server;
     int message;
     int **IDs;
     int **sizes;
{
  int how_many, i;
  char *fromserver;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_list");
      return (-1);
    }

  if (message)
    how_many = 1;
  else
    {
      int count, size;
      if (pop_stat (server, &count, &size))
	return (-1);
      how_many = count;
    }

  *IDs = (int *) malloc ((how_many + 1) * sizeof (int));
  *sizes = (int *) malloc ((how_many + 1) * sizeof (int));
  if (! (*IDs && *sizes))
    {
      strcpy (pop_error, "Out of memory in pop_list");
      return (-1);
    }

  if (message)
    {
      sprintf (pop_error, "LIST %d", message);
      if (sendline (server, pop_error))
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      if (pop_getline (server, &fromserver) < 0)
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      if (strncmp (fromserver, "+OK ", 4))
	{
	  if (! strncmp (fromserver, "-ERR", 4))
	    strncpy (pop_error, fromserver, ERROR_MAX);
	  else
	    {
	      strcpy (pop_error,
		      "Unexpected response from server in pop_list");
	      pop_trash (server);
	    }
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*IDs)[0] = atoi (&fromserver[4]);
      fromserver = index (&fromserver[4], ' ');
      if (! fromserver)
	{
	  strcpy (pop_error,
		  "Badly formatted response from server in pop_list");
	  pop_trash (server);
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*sizes)[0] = atoi (fromserver);
      (*IDs)[1] = (*sizes)[1] = 0;
      return (0);
    }
  else
    {
      if (pop_multi_first (server, "LIST", &fromserver))
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      for (i = 0; i < how_many; i++)
	{
	  if (pop_multi_next (server, &fromserver) <= 0)
	    {
	      free ((char *) *IDs);
	      free ((char *) *sizes);
	      return (-1);
	    }
	  (*IDs)[i] = atoi (fromserver);
	  fromserver = index (fromserver, ' ');
	  if (! fromserver)
	    {
	      strcpy (pop_error,
		      "Badly formatted response from server in pop_list");
	      free ((char *) *IDs);
	      free ((char *) *sizes);
	      pop_trash (server);
	      return (-1);
	    }
	  (*sizes)[i] = atoi (fromserver);
	}
      if (pop_multi_next (server, &fromserver) < 0)
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      else if (fromserver)
	{
	  strcpy (pop_error,
		  "Too many response lines from server in pop_list");
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*IDs)[i] = (*sizes)[i] = 0;
      return (0);
    }
}

/*
 * Function: pop_retrieve
 *
 * Purpose: Retrieve a specified message from the maildrop.
 *
 * Arguments:
 * 	server	The server to retrieve from.
 * 	message	The message number to retrieve.
 *	markfrom
 * 		If true, then mark the string "From " at the beginning
 * 		of lines with '>'.
 *	msg_buf	Output parameter to which a buffer containing the
 * 		message is assigned.
 * 
 * Return value: The number of bytes in msg_buf, which may contain
 * 	embedded nulls, not including its final null, or -1 on error
 * 	with pop_error set.
 *
 * Side effects: May kill connection on error.
 */
int
pop_retrieve (server, message, markfrom, msg_buf)
     popserver server;
     int message;
     int markfrom;
     char **msg_buf;
{
  int *IDs, *sizes, bufsize, fromcount = 0, cp = 0;
  char *ptr, *fromserver;
  int ret;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_retrieve");
      return (-1);
    }

  if (pop_list (server, message, &IDs, &sizes))
    return (-1);

  if (pop_retrieve_first (server, message, &fromserver))
    {
      return (-1);
    }

  /*
   * The "5" below is an arbitrary constant -- I assume that if
   * there are "From" lines in the text to be marked, there
   * probably won't be more than 5 of them.  If there are, I
   * allocate more space for them below.
   */
  bufsize = sizes[0] + (markfrom ? 5 : 0);
  ptr = (char *)malloc (bufsize);
  free ((char *) IDs);
  free ((char *) sizes);

  if (! ptr)
    {
      strcpy (pop_error, "Out of memory in pop_retrieve");
      pop_retrieve_flush (server);
      return (-1);
    }

  while ((ret = pop_retrieve_next (server, &fromserver)) >= 0)
    {
      if (! fromserver)
	{
	  ptr[cp] = '\0';
	  *msg_buf = ptr;
	  return (cp);
	}
      if (markfrom && fromserver[0] == 'F' && fromserver[1] == 'r' &&
	  fromserver[2] == 'o' && fromserver[3] == 'm' &&
	  fromserver[4] == ' ')
	{
	  if (++fromcount == 5)
	    {
	      bufsize += 5;
	      ptr = (char *)realloc (ptr, bufsize);
	      if (! ptr)
		{
		  strcpy (pop_error, "Out of memory in pop_retrieve");
		  pop_retrieve_flush (server);
		  return (-1);
		}
	      fromcount = 0;
	    }
	  ptr[cp++] = '>';
	}
      bcopy (fromserver, &ptr[cp], ret);
      cp += ret;
      ptr[cp++] = '\n';
    }

  free (ptr);
  return (-1);
}     

int
pop_retrieve_first (server, message, response)
     popserver server;
     int message;
     char **response;
{
  sprintf (pop_error, "RETR %d", message);
  return (pop_multi_first (server, pop_error, response));
}

/*
  Returns a negative number on error, 0 to indicate that the data has
  all been read (i.e., the server has returned a "." termination
  line), or a positive number indicating the number of bytes in the
  returned buffer (which is null-terminated and may contain embedded
  nulls, but the returned bytecount doesn't include the final null).
  */

int
pop_retrieve_next (server, line)
     popserver server;
     char **line;
{
  return (pop_multi_next (server, line));
}

int
pop_retrieve_flush (server)
     popserver server;
{
  return (pop_multi_flush (server));
}

int
pop_top_first (server, message, lines, response)
     popserver server;
     int message, lines;
     char **response;
{
  sprintf (pop_error, "TOP %d %d", message, lines);
  return (pop_multi_first (server, pop_error, response));
}

/*
  Returns a negative number on error, 0 to indicate that the data has
  all been read (i.e., the server has returned a "." termination
  line), or a positive number indicating the number of bytes in the
  returned buffer (which is null-terminated and may contain embedded
  nulls, but the returned bytecount doesn't include the final null).
  */

int
pop_top_next (server, line)
     popserver server;
     char **line;
{
  return (pop_multi_next (server, line));
}

int
pop_top_flush (server)
     popserver server;
{
  return (pop_multi_flush (server));
}

int
pop_multi_first (server, command, response)
     popserver server;
     char *command;
     char **response;
{
  if (server->in_multi)
    {
      strcpy (pop_error,
	      "Already in multi-line query in pop_multi_first");
      return (-1);
    }

  if (sendline (server, command) || (pop_getline (server, response) < 0))
    {
      return (-1);
    }

  if (0 == strncmp (*response, "-ERR", 4))
    {
      strncpy (pop_error, *response, ERROR_MAX);
      return (-1);
    }
  else if (0 == strncmp (*response, "+OK", 3))
    {
      for (*response += 3; **response == ' '; (*response)++) /* empty */;
      server->in_multi = 1;
      return (0);
    }
  else
    {
      strcpy (pop_error,
	      "Unexpected response from server in pop_multi_first");
      return (-1);
    }
}

/*
  Read the next line of data from SERVER and place a pointer to it
  into LINE.  Return -1 on error, 0 if there are no more lines to read
  (i.e., the server has returned a line containing only "."), or a
  positive number indicating the number of bytes in the LINE buffer
  (not including the final null).  The data in that buffer may contain
  embedded nulls, but does not contain the final CRLF. When returning
  0, LINE is set to null. */

int
pop_multi_next (server, line)
     popserver server;
     char **line;
{
  char *fromserver;
  int ret;

  if (! server->in_multi)
    {
      strcpy (pop_error, "Not in multi-line query in pop_multi_next");
      return (-1);
    }

  if ((ret = pop_getline (server, &fromserver)) < 0)
    {
      return (-1);
    }

  if (fromserver[0] == '.')
    {
      if (! fromserver[1])
	{
	  *line = 0;
	  server->in_multi = 0;
	  return (0);
	}
      else
	{
	  *line = fromserver + 1;
	  return (ret - 1);
	}
    }
  else
    {
      *line = fromserver;
      return (ret);
    }
}

int
pop_multi_flush (server)
     popserver server;
{
  char *line;
  int ret;

  if (! server->in_multi)
    {
      return (0);
    }

  while ((ret = pop_multi_next (server, &line)))
    {
      if (ret < 0)
	return (-1);
    }

  return (0);
}

/* Function: pop_delete
 *
 * Purpose: Delete a specified message.
 *
 * Arguments:
 * 	server	Server from which to delete the message.
 * 	message	Message to delete.
 *
 * Return value: 0 on success, non-zero with error in pop_error
 * 	otherwise.
 */
int
pop_delete (server, message)
     popserver server;
     int message;
{
  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_delete");
      return (-1);
    }

  sprintf (pop_error, "DELE %d", message);

  if (sendline (server, pop_error) || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_noop
 *
 * Purpose: Send a noop command to the server.
 *
 * Argument:
 * 	server	The server to send to.
 *
 * Return value: 0 on success, non-zero with error in pop_error
 * 	otherwise.
 *
 * Side effects: Closes connection on error.
 */
int
pop_noop (server)
     popserver server;
{
  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_noop");
      return (-1);
    }

  if (sendline (server, "NOOP") || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_last
 *
 * Purpose: Find out the highest seen message from the server.
 *
 * Arguments:
 * 	server	The server.
 *
 * Return value: If successful, the highest seen message, which is
 * 	greater than or equal to 0.  Otherwise, a negative number with
 * 	the error explained in pop_error.
 *
 * Side effects: Closes the connection on error.
 */
int
pop_last (server)
     popserver server;
{
  char *fromserver;
     
  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_last");
      return (-1);
    }

  if (sendline (server, "LAST"))
    return (-1);

  if (pop_getline (server, &fromserver) < 0)
    return (-1);

  if (! strncmp (fromserver, "-ERR", 4))
    {
      strncpy (pop_error, fromserver, ERROR_MAX);
      return (-1);
    }
  else if (strncmp (fromserver, "+OK ", 4))
    {
      strcpy (pop_error, "Unexpected response from server in pop_last");
      pop_trash (server);
      return (-1);
    }
  else
    {
      return (atoi (&fromserver[4]));
    }
}

/*
 * Function: pop_reset
 *
 * Purpose: Reset the server to its initial connect state
 *
 * Arguments:
 * 	server	The server.
 *
 * Return value: 0 for success, non-0 with error in pop_error
 * 	otherwise.
 *
 * Side effects: Closes the connection on error.
 */
int
pop_reset (server)
     popserver server;
{
  if (pop_retrieve_flush (server))
    {
      return (-1);
    }

  if (sendline (server, "RSET") || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_quit
 *
 * Purpose: Quit the connection to the server,
 *
 * Arguments:
 * 	server	The server to quit.
 *
 * Return value: 0 for success, non-zero otherwise with error in
 * 	pop_error.
 *
 * Side Effects: The popserver passed in is unusable after this
 * 	function is called, even if an error occurs.
 */
int
pop_quit (server)
     popserver server;
{
  int ret = 0;

  if (server->file >= 0)
    {
      if (pop_retrieve_flush (server))
	{
	  ret = -1;
	}

      if (sendline (server, "QUIT") || getok (server))
	{
	  ret = -1;
	}

      close (server->file);
    }

  if (server->buffer)
    free (server->buffer);
#ifdef GSSAPI
  if (server->extra)
    {
      OM_uint32 minor;

      if (Gssapi (server->extra)->gss_context != GSS_C_NO_CONTEXT)
	gss_delete_sec_context (&minor, &(Gssapi (server->extra)->gss_context),
				GSS_C_NO_BUFFER);
      free ((char *) server->extra);
    }
#endif /* GSSAPI */
  free ((char *) server);

  return (ret);
}

#ifdef WINDOWSNT
static int have_winsock = 0;
#endif

/*
 * Function: socket_connection
 *
 * Purpose: Opens the network connection with the mail host, without
 * 	doing any sort of I/O with it or anything.
 *
 * Arguments:
 * 	host	The host to which to connect.
 *	flags	Option flags.
 * 	
 * Return value: A file descriptor indicating the connection, or -1
 * 	indicating failure, in which case an error has been copied
 * 	into pop_error.
 */
static int
socket_connection (host, flags)
     char *host;
     int flags;
{
  struct hostent *hostent;
  struct servent *servent;
  struct sockaddr_in addr;
  char found_port = 0;
  char *service;
  int sock;
#ifdef KERBEROS
#ifdef KERBEROS5
  krb5_error_code rem;
  krb5_context kcontext = 0;
  krb5_auth_context auth_context = 0;
  krb5_ccache ccdef;
  krb5_principal client, server;
  krb5_error *err_ret;
  register char *cp;
#else
  KTEXT ticket;
  MSG_DAT msg_data;
  CREDENTIALS cred;
  Key_schedule schedule;
  int rem;
  char *realhost;
#endif /* KERBEROS5 */
#endif /* KERBEROS */

  int try_count = 0;

#ifdef WINDOWSNT
  {
    WSADATA winsockData;
    if (WSAStartup (0x101, &winsockData) == 0)
      have_winsock = 1;
  }
#endif

  do
    {
      hostent = gethostbyname (host);
      try_count++;
      if ((! hostent) && ((h_errno != TRY_AGAIN) || (try_count == 5)))
	{
	  strcpy (pop_error, "Could not determine POP server's address");
	  return (-1);
	}
    } while (! hostent);

  bzero ((char *) &addr, sizeof (addr));
  addr.sin_family = AF_INET;

#ifdef KERBEROS
  service = (flags & POP_NO_KERBEROS) ? POP_SERVICE : KPOP_SERVICE;
#else
  service = POP_SERVICE;
#endif

#ifdef HESIOD
  if (! (flags & POP_NO_HESIOD))
    {
      servent = hes_getservbyname (service, "tcp");
      if (servent)
	{
	  addr.sin_port = servent->s_port;
	  found_port = 1;
	}
    }
#endif
  if (! found_port)
    {
      servent = getservbyname (service, "tcp");
      if (servent)
	{
	  addr.sin_port = servent->s_port;
	}
      else
	{
#ifdef KERBEROS
	  addr.sin_port = htons ((flags & POP_NO_KERBEROS) ?
				POP_PORT : KPOP_PORT);
#else
	  addr.sin_port = htons (POP_PORT);
#endif
	}
    }

#define POP_SOCKET_ERROR "Could not create socket for POP connection: "

  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    {
      strcpy (pop_error, POP_SOCKET_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (POP_SOCKET_ERROR));
      return (-1);
	  
    }

  while (*hostent->h_addr_list)
    {
      bcopy (*hostent->h_addr_list, (char *) &addr.sin_addr,
	     hostent->h_length);
      if (! connect (sock, (struct sockaddr *) &addr, sizeof (addr)))
	break;
      hostent->h_addr_list++;
    }

#define CONNECT_ERROR "Could not connect to POP server: "
     
  if (! *hostent->h_addr_list)
    {
      CLOSESOCKET (sock);
      strcpy (pop_error, CONNECT_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (CONNECT_ERROR));
      return (-1);
	  
    }

#ifdef KERBEROS
#define KRB_ERROR "Kerberos error connecting to POP server: "
  if (! (flags & POP_NO_KERBEROS))
    {
#ifdef KERBEROS5
      if ((rem = krb5_init_context (&kcontext)))
	{
	krb5error:
	  if (auth_context)
	    krb5_auth_con_free (kcontext, auth_context);
	  if (kcontext)
	    krb5_free_context (kcontext);
	  strcpy (pop_error, KRB_ERROR);
	  strncat (pop_error, error_message (rem),
		   ERROR_MAX - sizeof(KRB_ERROR));
	  CLOSESOCKET (sock);
	  return (-1);
	}

      if ((rem = krb5_auth_con_init (kcontext, &auth_context)))
	goto krb5error;
      
      if (rem = krb5_cc_default (kcontext, &ccdef))
	goto krb5error;

      if (rem = krb5_cc_get_principal (kcontext, ccdef, &client))
	goto krb5error;

      for (cp = hostent->h_name; *cp; cp++)
	{
	  if (isupper (*cp))
	    {
	      *cp = tolower (*cp);
	    }
	}

      if (rem = krb5_sname_to_principal (kcontext, hostent->h_name,
					 POP_SERVICE, FALSE, &server))
	goto krb5error;

      rem = krb5_sendauth (kcontext, &auth_context,
			   (krb5_pointer) &sock, "KPOPV1.0", client, server,
			  AP_OPTS_MUTUAL_REQUIRED,
			  0,	/* no checksum */
			  0,	/* no creds, use ccache instead */
			  ccdef,
			  &err_ret,
			  0,	/* don't need subsession key */
			  0);	/* don't need reply */
      krb5_free_principal (kcontext, server);
      if (rem)
	{
	  if (err_ret && err_ret->text.length)
	    {
	      strcpy (pop_error, KRB_ERROR);
	      strncat (pop_error, error_message (rem),
		       ERROR_MAX - sizeof (KRB_ERROR));
	      strncat (pop_error, " [server says '",
		       ERROR_MAX - strlen (pop_error) - 1);
	      strncat (pop_error, err_ret->text.data,
		       min (ERROR_MAX - strlen (pop_error) - 1,
			    err_ret->text.length));
	      strncat (pop_error, "']",
		       ERROR_MAX - strlen (pop_error) - 1);
	    }
	  else
	    {
	      strcpy (pop_error, KRB_ERROR);
	      strncat (pop_error, error_message (rem),
		       ERROR_MAX - sizeof (KRB_ERROR));
	    }
	  if (err_ret)
	    krb5_free_error (kcontext, err_ret);
	  krb5_auth_con_free (kcontext, auth_context);
	  krb5_free_context (kcontext);

	  CLOSESOCKET (sock);
	  return (-1);
	}
#else  /* ! KERBEROS5 */	  
      ticket = (KTEXT) malloc (sizeof (KTEXT_ST));
      realhost = strdup (hostent->h_name);
      rem = krb_sendauth (0L, sock, ticket, "pop", realhost,
			  (char *) krb_realmofhost (realhost),
			  (unsigned long) 0, &msg_data, &cred, schedule,
			  (struct sockaddr_in *) 0,
			  (struct sockaddr_in *) 0,
			  "KPOPV0.1");
      free ((char *) ticket);
      free (realhost);
      if (rem != KSUCCESS)
	{
	  strcpy (pop_error, KRB_ERROR);
	  strncat (pop_error, krb_err_txt[rem],
		   ERROR_MAX - sizeof (KRB_ERROR));
	  CLOSESOCKET (sock);
	  return (-1);
	}
#endif /* KERBEROS5 */
    }
#endif /* KERBEROS */

  return (sock);
} /* socket_connection */

/*
 * Function: pop_getline
 *
 * Purpose: Get a line of text from the connection and return a
 * 	pointer to it.  The carriage return and linefeed at the end of
 * 	the line are stripped, but periods at the beginnings of lines
 * 	are NOT dealt with in any special way.
 *
 * Arguments:
 * 	server	The server from which to get the line of text.
 *
 * Returns: The number of characters in the line, which is returned in
 * 	LINE, not including the final null.  A return value of 0
 * 	indicates a blank line.  A negative return value indicates an
 * 	error (in which case the contents of LINE are undefined.  In
 * 	case of error, an error message is copied into pop_error.
 *
 * Notes: The line returned is overwritten with each call to pop_getline.
 *
 * Side effects: Closes the connection on error.
 *
 * THE RETURNED LINE MAY CONTAIN EMBEDDED NULLS!
 */
static int
pop_getline (server, line)
     popserver server;
     char **line;
{
#define GETLINE_ERROR "Error reading from server: "

  int ret;
  int search_offset = 0;

  if (server->data)
    {
      char *cp = find_crlf (server->buffer + server->buffer_index,
			    server->data);
      if (cp)
	{
	  int found;
	  int data_used;

	  found = server->buffer_index;
	  data_used = (cp + 2) - server->buffer - found;
	       
	  *cp = '\0';		/* terminate the string to be returned */
	  server->data -= data_used;
	  server->buffer_index += data_used;

	  if (pop_debug)
	    /* Embedded nulls will truncate this output prematurely,
	       but that's OK because it's just for debugging anyway. */
	    fprintf (stderr, "<<< %s\n", server->buffer + found);
	  *line = server->buffer + found;
	  return (data_used - 2);
	}
      else
	{
	  bcopy (server->buffer + server->buffer_index,
		 server->buffer, server->data);
	  /* Record the fact that we've searched the data already in
             the buffer for a CRLF, so that when we search below, we
             don't have to search the same data twice.  There's a "-
             1" here to account for the fact that the last character
             of the data we have may be the CR of a CRLF pair, of
             which we haven't read the second half yet, so we may have
             to search it again when we read more data. */
	  search_offset = server->data - 1;
	  server->buffer_index = 0;
	}
    }
  else
    {
      server->buffer_index = 0;
    }

  while (1)
    {
#ifdef GSSAPI
      /*
       * We might be playing with a protected connection.  If we are, then
       * we need to first read a chunk of ciphertext from the server,
       * unwrap it, and stuff it into the buffer.
       */
      if (server->extra &&
	  ((Gssapi (server->extra)->gss_flags) & GSSAPI_NEEDWRAP))
	{
	  char rcvbuf[GSSAPI_RCVBUF];
	  OM_uint32 major, minor, length;
	  gss_buffer_desc in_tok, out_tok;
	  struct _pop_gssapi *gss_data = Gssapi (server->extra);

	  ret = fullread (server->file, (char *) &length, sizeof (length));

	  if (ret == sizeof (length))
	    {
	      in_tok.length = ntohl (length);

	      if (in_tok.length <= GSSAPI_RCVBUF)
		{
		  ret = fullread (server->file, rcvbuf, in_tok.length);

		  if (ret == in_tok.length)
		    {
		      in_tok.value = (void *) rcvbuf;

		      major = gss_unwrap (&minor, gss_data->gss_context,
					  &in_tok, &out_tok, 0, 0);

		      if (major != GSS_S_COMPLETE)
			{
			  pop_trash (server);
			  gen_gss_error ("unwrapping", major, minor);
			  return (-1);
			}

		      while (server->data + out_tok.length >=
			     server->buffer_size - 1)
			server->buffer_size += GETLINE_INCR;

		      server->buffer = (char *)realloc (server->buffer,
							server->buffer_size);

		      if (! server->buffer)
			{
			  gss_release_buffer (&minor, &out_tok);
			  pop_trash (server);
			  strcpy (pop_error, "Out of memory in pop_getline");
			  return (-1);
			}

		      bcopy (out_tok.value, server->buffer + server->data,
			     out_tok.length);

		      ret = out_tok.length;

		      gss_release_buffer (&minor, &out_tok);
		    }
		  else
		    ret = 0;	/* force detection of unexpected EOF */
		}
	      else
		{
		  pop_trash (server);
		  strcpy (pop_error, "Token from server too long in pop_getline");
		  return (-1);
		}
	    }
	  else
	    ret = 0;		/* force detection of unexpected EOF */
	}
      else
	{
#endif /* GSSAPI */
	  /* There's a "- 1" here to leave room for the null that we put
	     at the end of the read data below.  We put the null there so
	     that find_crlf knows where to stop when we call it. */
	  if (server->data == server->buffer_size - 1)
	    {
	      server->buffer_size += GETLINE_INCR;
	      server->buffer = (char *)realloc (server->buffer,
						server->buffer_size);
	      if (! server->buffer)
		{
		  strcpy (pop_error, "Out of memory in pop_getline");
		  pop_trash (server);
		  return (-1);
		}
	    }
	  ret = RECV (server->file, server->buffer + server->data,
		      server->buffer_size - server->data - 1, 0);
#ifdef GSSAPI
	}
#endif /* GSSAPI */
      if (ret < 0)
	{
	  strcpy (pop_error, GETLINE_ERROR);
	  strncat (pop_error, strerror (errno),
		   ERROR_MAX - sizeof (GETLINE_ERROR));
	  pop_trash (server);
	  return (-1);
	}
      else if (ret == 0)
	{
	  strcpy (pop_error, "Unexpected EOF from server in pop_getline");
	  pop_trash (server);
	  return (-1);
	}
      else
	{
	  char *cp;
	  server->data += ret;
	  server->buffer[server->data] = '\0';
	       
	  cp = find_crlf (server->buffer + search_offset,
			  server->data - search_offset);
	  if (cp)
	    {
	      int data_used = (cp + 2) - server->buffer;
	      *cp = '\0';
	      server->data -= data_used;
	      server->buffer_index = data_used;

	      if (pop_debug)
		fprintf (stderr, "<<< %s\n", server->buffer);
	      *line = server->buffer;
	      return (data_used - 2);
	    }
	  /* As above, the "- 1" here is to account for the fact that
	     we may have read a CR without its accompanying LF. */
	  search_offset += ret - 1;
	}
    }

  /* NOTREACHED */
}

#ifdef GSSAPI
/*
 * Function: fullread
 *
 * Purpose: Just like read, but keeps trying until the specified number
 * 	number of bytes has been read into the buffer.  This function is
 * 	only needed if you are using the GSSAPI protection mechanisms.
 *
 * Return value: Same as read.  Pop_error is not set.
 */
static int
fullread (fd, buf, nbytes)
     int fd;
     char *buf;
     int nbytes;
{
  char *cp;
  int ret;

  cp = buf;

  while (nbytes > 0 && (ret = RECV (fd, cp, nbytes, 0)) > 0)
    {
      cp += ret;
      nbytes -= ret;
    }

  return (ret);
}
#endif /* GSSAPI */

/*
 * Function: sendline
 *
 * Purpose: Sends a line of text to the POP server.  The line of text
 * 	passed into this function should NOT have the carriage return
 * 	and linefeed on the end of it.  Periods at beginnings of lines
 * 	will NOT be treated specially by this function.
 *
 * Arguments:
 * 	server	The server to which to send the text.
 * 	line	The line of text to send.
 *
 * Return value: Upon successful completion, a value of 0 will be
 * 	returned.  Otherwise, a non-zero value will be returned, and
 * 	an error will be copied into pop_error.
 *
 * Side effects: Closes the connection on error.
 */
static int
sendline (server, line)
     popserver server;
     char *line;
{
#define SENDLINE_ERROR "Error writing to POP server: "
  int ret;

#ifdef GSSAPI
  /*
   * We might be playing with a protected connection.  If we are, then we
   * need to build our full plaintext, parse it into chunks small enough
   * for the server to swallow, wrap each one, and send it over the net as
   * specified by the RFC.
   */
  if (server->extra && ((Gssapi (server->extra)->gss_flags) & GSSAPI_NEEDWRAP))
    {
      char *sendbuf, *ptr;
      OM_uint32 major, minor, length;
      gss_buffer_desc in_tok, out_tok;
      int len = 0, tot_len;
      struct _pop_gssapi *gss_data = Gssapi (server->extra);

      sendbuf = malloc (strlen (line) + 3);

      if (! sendbuf)
	{
	  pop_trash (server);
	  strcpy (pop_error, "Out of memory in sendline");
	  return (-1);
	}

      tot_len = sprintf (sendbuf, "%s\r\n", line);

      for (ptr = sendbuf; tot_len > 0; tot_len -= len, ptr += len)
	{
	  len = ((tot_len > gss_data->max_size) ?
		 gss_data->max_size : tot_len);

	  in_tok.value = (void *) ptr;
	  in_tok.length = len;

	  major = gss_wrap (&minor, gss_data->gss_context,
			    (gss_data->gss_flags & GSSAPI_PRIVACY) ? 1 : 0,
			    GSS_C_QOP_DEFAULT, &in_tok, 0, &out_tok);

	  if (major != GSS_S_COMPLETE)
	    {
	      free (sendbuf);
	      pop_trash (server);
	      gen_gss_error ("wrapping", major, minor);
	      return (-1);
	    }

	  /*
	   * "Once the protection mechanism is in effect, the stream of
	   *  command and response octets is processed into buffers of
	   *  ciphertext.  Each buffer is transferred over the connection
	   *  as a stream of octets prepended with a four octet field in
	   *  network byte order that represents the length of the
	   *  following data." - RFC 1734, section 2
	   */
	  length = htonl (out_tok.length);
	  ret = fullwrite (server->file, (char *) &length, sizeof (length));
	  if (ret == sizeof (length))
	    {
	      ret = fullwrite (server->file, (char *) out_tok.value,
			       out_tok.length);
	    }

	  gss_release_buffer (&minor, &out_tok);

	  if (ret < 0)
	    break;
	}

      free (sendbuf);
    }
  else
    {
#endif /* GSSAPI */
      ret = fullwrite (server->file, line, strlen (line));
      if (ret >= 0)
	{			/* 0 indicates that a blank line was written */
	  ret = fullwrite (server->file, "\r\n", 2);
	}
#ifdef GSSAPI
    }
#endif /* GSSAPI */

  if (ret < 0)
    {
      pop_trash (server);
      strcpy (pop_error, SENDLINE_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (SENDLINE_ERROR));
      return (ret);
    }

  if (pop_debug)
    fprintf (stderr, ">>> %s\n", line);

  return (0);
}

/*
 * Procedure: fullwrite
 *
 * Purpose: Just like write, but keeps trying until the entire string
 * 	has been written.
 *
 * Return value: Same as write.  Pop_error is not set.
 */
static int
fullwrite (fd, buf, nbytes)
     int fd;
     char *buf;
     int nbytes;
{
  char *cp;
  int ret = 0;

  cp = buf;
  while (nbytes && ((ret = SEND (fd, cp, nbytes, 0)) > 0))
    {
      cp += ret;
      nbytes -= ret;
    }

  return (ret);
}

/*
 * Procedure getok
 *
 * Purpose: Reads a line from the server.  If the return indicator is
 * 	positive, return with a zero exit status.  If not, return with
 * 	a negative exit status.
 *
 * Arguments:
 * 	server	The server to read from.
 * 
 * Returns: 0 for success, else for failure and puts error in pop_error.
 *
 * Side effects: On failure, may make the connection unusable.
 */
static int
getok (server)
     popserver server;
{
  char *fromline;

  if (pop_getline (server, &fromline) < 0)
    {
      return (-1);
    }

  if (! strncmp (fromline, "+OK", 3))
    return (0);
  else if (! strncmp (fromline, "-ERR", 4))
    {
      strncpy (pop_error, fromline, ERROR_MAX);
      pop_error[ERROR_MAX-1] = '\0';
      return (-1);
    }
  else
    {
      strcpy (pop_error,
	      "Unexpected response from server; expecting +OK or -ERR");
      pop_trash (server);
      return (-1);
    }
}	  

#if 0
/*
 * Function: gettermination
 *
 * Purpose: Gets the next line and verifies that it is a termination
 * 	line (nothing but a dot).
 *
 * Return value: 0 on success, non-zero with pop_error set on error.
 *
 * Side effects: Closes the connection on error.
 */
static int
gettermination (server)
     popserver server;
{
  char *fromserver;

  if (pop_getline (server, &fromserver) < 0)
    return (-1);

  if (strcmp (fromserver, "."))
    {
      strcpy (pop_error,
	      "Unexpected response from server in gettermination");
      pop_trash (server);
      return (-1);
    }

  return (0);
}
#endif

/*
 * Function pop_close
 *
 * Purpose: Close a pop connection, sending a "RSET" command to try to
 * 	preserve any changes that were made and a "QUIT" command to
 * 	try to get the server to quit, but ignoring any responses that
 * 	are received.
 *
 * Side effects: The server is unusable after this function returns.
 * 	Changes made to the maildrop since the session was started (or
 * 	since the last pop_reset) may be lost.
 */
void 
pop_close (server)
     popserver server;
{
  pop_trash (server);
  free ((char *) server);

  return;
}

/*
 * Function: pop_trash
 *
 * Purpose: Like pop_close or pop_quit, but doesn't deallocate the
 * 	memory associated with the server.  It is legal to call
 * 	pop_close or pop_quit after this function has been called.
 */
static void
pop_trash (server)
     popserver server;
{
  if (server->file >= 0)
    {
      /* avoid recursion; sendline can call pop_trash */
      if (server->trash_started)
	return;
      server->trash_started = 1;

      sendline (server, "RSET");
      sendline (server, "QUIT");

      CLOSESOCKET (server->file);
      server->file = -1;
      if (server->buffer)
	{
	  free (server->buffer);
	  server->buffer = 0;
	}
#ifdef GSSAPI
      if (server->extra)
	{
	  OM_uint32 minor;

	  if (Gssapi (server->extra)->gss_context != GSS_C_NO_CONTEXT)
	    gss_delete_sec_context (&minor,
				    &(Gssapi (server->extra)->gss_context),
				   GSS_C_NO_BUFFER);
	  free ((char *) server->extra);
	  server->extra = 0;
	}
#endif /* GSSAPI */
    }

#ifdef WINDOWSNT
  if (have_winsock)
    WSACleanup ();
#endif
}

#ifdef GSSAPI
/*
 * Function: pop_auth
 *
 * Purpose: To perform a GSSAPI authentication handshake with a POP server.
 * 	If the negotiation is successful, it will return 0; otherwise, it
 * 	will fill in pop_error with the error message and return either -1,
 * 	indicating a potentially recoverable error, or -2, indicating an
 * 	unrecoverable error.
 *
 * Side effects: The server may choose to close the connection if the
 * 	handshake fails.  The connection will be trashed if the error is
 * 	unrecoverable.
 */
static int
pop_auth (server, username, host, flags)
     popserver server;
     char *username, *host;
     int flags;
{
  int gss_flags, ret;
  char *fromserver;
  OM_uint32 max_size, t_flags;
  gss_ctx_id_t gss_context = GSS_C_NO_CONTEXT;
  gss_buffer_desc in_tok, out_tok;
  gss_name_t svc_name;
  OM_uint32 major, minor, t_minor;

  /* calculate usable protection mechanisms */
  gss_flags = (GSSAPI_PROTECTION &
	       ~(((flags & POP_NO_NOPROT) ? GSSAPI_NOPROT : 0) |
		 ((flags & POP_NO_INTEG) ? GSSAPI_INTEGRITY : 0) |
		 ((flags & POP_NO_ENCRYPT) ? GSSAPI_PRIVACY : 0)));

  if (gss_flags == 0)
    {
      strcpy (pop_error, "Unable to provide selected protection level");
      return (-1);
    }

  /* import service name of pop server */
  in_tok.value = (void *) malloc (strlen (host) + sizeof (GSSAPI_SERVICE) + 2);

  if (! in_tok.value)
    {
      strcpy (pop_error, "Out of memory in pop_auth");
      return (-1);
    }

  sprintf ((char *) in_tok.value, "%s@%s", GSSAPI_SERVICE, host);
  in_tok.length = strlen ((char *) in_tok.value);

  {
    gss_OID_desc svc_name_oid = GSSAPI_SVC_TYPE;

    major = gss_import_name (&minor, &in_tok, &svc_name_oid, &svc_name);
  }

  free ((char *) in_tok.value);

  if (major != GSS_S_COMPLETE)
    {
      gen_gss_error ("parsing name", major, minor);
      return (-1);
    }

  /* begin GSSAPI authentication handshake */
  if (sendline (server, "AUTH GSSAPI") || (pop_getline (server, &fromserver) < 0))
    {
      gss_release_name (&t_minor, &svc_name);
      return (-1);
    }

  do
    {
      /* sanity-check server response */
      if (strncmp (fromserver, "+ ", 2))
	{
	  gss_release_name (&t_minor, &svc_name);
	  if (gss_context != GSS_C_NO_CONTEXT)
	    gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
	  if (0 == strncmp (fromserver, "-ERR", 4))
	    {
	      strncpy (pop_error, fromserver, ERROR_MAX);
	      return (-1);
	    }
	  else
	    {
	      pop_trash (server);
	      strcpy (pop_error,
		      "Unexpected response from POP server in pop_auth");
	      return (-2);
	    }
	}

      if (strlen (fromserver) > 2)
	{
	  /* base 64 decode the response... */
	  ret = b64_decode (fromserver + 2, &in_tok);
	  if (ret != B64_SUCCESS)
	    {
	      gss_release_name (&t_minor, &svc_name);
	      if (gss_context != GSS_C_NO_CONTEXT)
		gss_delete_sec_context (&t_minor, &gss_context,
					GSS_C_NO_BUFFER);
	      sendline (server, "*");
	      strcpy (pop_error, b64_error[ret]);
	      return (-1);
	    }
	}
      else
	{
	  in_tok.length = 0;
	  in_tok.value = 0;
	}

      /* call init_sec_context */
      major = gss_init_sec_context (&minor, GSS_C_NO_CREDENTIAL, &gss_context,
				    svc_name, GSS_C_NULL_OID,
				    GSS_C_MUTUAL_FLAG, 0,
				    GSS_C_NO_CHANNEL_BINDINGS,
				    in_tok.length ? & in_tok : GSS_C_NO_BUFFER,
				    0, &out_tok, 0, 0);

      if (in_tok.length != 0)
	free ((char *) in_tok.value);

      /* check for error */
      if (GSS_ERROR (major))
	{
	  gss_release_name (&t_minor, &svc_name);
	  if (gss_context != GSS_C_NO_CONTEXT)
	    gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
	  sendline (server, "*");
	  gen_gss_error ("in init_sec_context", major, minor);
	  return (-1);
	}

      if (out_tok.length != 0)
	{
	  /* base 64 encode output token, if any */
	  ret = b64_encode (&out_tok, &fromserver);

	  gss_release_buffer (&t_minor, &out_tok);

	  if (ret != B64_SUCCESS)
	    {
	      gss_release_name (&t_minor, &svc_name);
	      if (gss_context != GSS_C_NO_CONTEXT)
		gss_delete_sec_context (&t_minor, &gss_context,
					GSS_C_NO_BUFFER);
	      sendline (server, "*");
	      strcpy (pop_error, b64_error[ret]);
	      return (-1);
	    }

	  /* send output token... */
	  ret = sendline (server, fromserver);

	  free (fromserver);
	}
      else
	/* empty output token... */
	ret = sendline (server, "");

      /* get next token from server */
      if (ret || (pop_getline (server, &fromserver) < 0))
	{
	  gss_release_name (&t_minor, &svc_name);
	  if (gss_context != GSS_C_NO_CONTEXT)
	    gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
	  return (-1);
	}
    } while ((major & GSS_S_CONTINUE_NEEDED));

  /* release name... */
  gss_release_name (&t_minor, &svc_name);

  /* get final response from server */
  if (strncmp (fromserver, "+ ", 2))
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      if (0 == strncmp (fromserver, "-ERR", 4))
	{
	  strncpy (pop_error, fromserver, ERROR_MAX);
	  return (-1);
	}
      else
	{
	  pop_trash (server);
	  strcpy (pop_error,
		  "Unexpected response from POP server in pop_auth");
	  return (-2);
	}
    }

  /* base 64 decode... */
  ret = b64_decode (fromserver + 2, &in_tok);
  if (ret != B64_SUCCESS)
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      strcpy (pop_error, b64_error[ret]);
      return (-1);
    }

  /* unwrap... */
  major = gss_unwrap (&minor, gss_context, &in_tok, &out_tok, 0, 0);

  free ((char *) in_tok.value);

  if (major != GSS_S_COMPLETE || out_tok.length != sizeof (t_flags))
    {
      if (out_tok.length != 0)
	gss_release_buffer (&t_minor, &out_tok);
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      gen_gss_error ("in gss_unwrap", major, minor);
      return (-1);
    }

  /* get and check flags/size */
  bcopy ((void *) out_tok.value, (void *) &t_flags, sizeof (t_flags));

  gss_release_buffer (&t_minor, &out_tok);

  max_size = ntohl (t_flags);

  t_flags = ((max_size & 0xFF000000) >> 24) & gss_flags;
  max_size &= 0x00FFFFFF;

  if ((t_flags & GSSAPI_PRIVACY))
    gss_flags = GSSAPI_PRIVACY;

  else if ((t_flags & GSSAPI_INTEGRITY))
    gss_flags = GSSAPI_INTEGRITY;

  else if ((t_flags & GSSAPI_NOPROT))
    gss_flags = GSSAPI_NOPROT;

  else
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      strcpy (pop_error, "Server does not provide selected protection level");
      return (-1);
    }

  if (max_size == 0)
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      strcpy (pop_error, "Bad server max length");
      return (-1);
    }

  if ((gss_flags & GSSAPI_NEEDWRAP))
    {
      major = gss_wrap_size_limit (&t_minor, gss_context,
				   (gss_flags & GSSAPI_PRIVACY) ? 1 : 0,
				   GSS_C_QOP_DEFAULT,
				   (max_size < GSSAPI_RCVBUF) ? max_size :
				   GSSAPI_RCVBUF, &max_size);
      if (major != GSS_S_COMPLETE)
	{
	  gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
	  sendline (server, "*");
	  gen_gss_error ("getting max size", major, minor);
	  return (-1);
	}
    }

  /* generate return flags */
  {
    OM_uint32 tmp;

    tmp = (((gss_flags << 24) & 0xFF000000) | (GSSAPI_RCVBUF & 0x00FFFFFF));
    t_flags = ntohl (tmp);
  }

  in_tok.length = sizeof (t_flags) + strlen (username);
  in_tok.value = (void *) malloc (in_tok.length);

  if (! in_tok.value)
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      strcpy (pop_error, "Out of memory in pop_auth");
      return (-1);
    }

  bcopy ((void *) &t_flags, in_tok.value, sizeof (t_flags));
  bcopy ((void *) username,
	 (void *) (((char *) in_tok.value) + sizeof (t_flags)),
	 in_tok.length - sizeof (t_flags));

  /* wrap result */
  major = gss_wrap (&minor, gss_context, 0, GSS_C_QOP_DEFAULT,
		    &in_tok, 0, &out_tok);

  free ((char *) in_tok.value);

  if (major != GSS_S_COMPLETE || out_tok.length == 0)
    {
      if (out_tok.length != 0)
	gss_release_buffer (&t_minor, &out_tok);
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      gen_gss_error ("in gss_wrap", major, minor);
      return (-1);
    }

  /* base 64 encode... */
  ret = b64_encode (&out_tok, &fromserver);

  gss_release_buffer (&t_minor, &out_tok);

  if (ret != B64_SUCCESS)
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      sendline (server, "*");
      strcpy (pop_error, b64_error[ret]);
      return (-1);
    }

  /* send to server */
  ret = sendline (server, fromserver);

  free (fromserver);

  /* see if the server likes me... */
  if (ret || getok (server))
    {
      gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
      return (-1);
    }

  /* stash context */
  {
    struct _pop_gssapi *gss_data;

    gss_data = (struct _pop_gssapi *) malloc (sizeof (struct _pop_gssapi));

    if (! gss_data)
      {
	pop_trash (server);
	gss_delete_sec_context (&t_minor, &gss_context, GSS_C_NO_BUFFER);
	strcpy (pop_error, "Out of memory in pop_auth");
	return (-2);
      }

    gss_data->gss_flags = gss_flags;
    gss_data->max_size = max_size;
    gss_data->gss_context = gss_context;

    server->extra = gss_data;
  }

  return (0);
}

/*
 * Add as much error text to pop_error as will fit, but only put complete
 * messages
 */
static void
gen_gss_error (msg, major, minor)
     char *msg;
     OM_uint32 major, minor;
{
  char *p = pop_error, *t, *saved;
  int max = ERROR_MAX - 1; /* for \0 */
  OM_uint32 t_minor, msg_ctx = 0;
  gss_buffer_desc gss_msg;

  while (*msg && max)
    {
      *p++ = *msg++;
      max--;
    }

  if (max >= 2)
    {
      saved = p;
      *p++ = ':';
      *p++ = ' ';
      max -= 2;
    }
  else
    {
      *p = '\0';
      return;
    }

  do
    {
      gss_display_status (&t_minor, major, GSS_C_GSS_CODE, GSS_C_NO_OID,
			  &msg_ctx, &gss_msg);
      for (t = (char *) gss_msg.value; *t && max; max--)
	{
	  *p++ = *t++;
	}
      gss_release_buffer (&t_minor, &gss_msg);
      if (max == 0)
	{
	  *saved = '\0';
	  return;
	}
    } while (msg_ctx);

  saved = p;

  do
    {
      gss_display_status (&t_minor, minor, GSS_C_MECH_CODE, GSS_C_NO_OID,
			  &msg_ctx, &gss_msg);
      for (t = (char *) gss_msg.value; *t && max; max--)
	{
	  *p++ = *t++;
	}
      gss_release_buffer (&t_minor, &gss_msg);
      if (max == 0)
	{
	  *saved = '\0';
	  return;
	}
    } while (msg_ctx);

  *p = '\0';
  return;
}

/*
 * table-based base64 decoding function; takes 4 characters from in and
 * writes from 1 to 3 bytes to out, storing the amount written in len
 */
static int
b64_d (in, out, len)
     char *in, *out;
     int *len;
{
  int decodearray[] =
  {
    0x3e,   -1,   -1,   -1, 0x3f, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a,
    0x3b, 0x3c, 0x3d,   -1,   -1,   -1,   -1,   -1,   -1,   -1, 0x00, 0x01,
    0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
    0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
      -1,   -1,   -1,   -1,   -1,   -1, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b,
    0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33
  };

  int d;

  if (!in || !out || !len)
    return (B64_BADPARAM);

  if (*in < '+' || *in > 'z')
    return (B64_BADCHAR);

  d = decodearray[*(in++) - '+'];
  if (d == -1)
    return (B64_BADCHAR);
  *out = d << 2;

  if (*in < '+' || *in > 'z')
    return (B64_BADCHAR);

  d = decodearray[*(in++) - '+'];
  if (d == -1)
    return (B64_BADCHAR);
  *(out++) |= d >> 4;
  *out = (d & 15) << 4;

  if (*in < '+' || *in > 'z')
    return (B64_BADCHAR);
  else if (*in == '=')
    if (*(in + 1) != '=')
      return (B64_BADPAD);
    else
      {
	*len = 1;
	return (B64_SUCCESS);
      }

  d = decodearray[*(in++) - '+'];
  if (d == -1)
    return (B64_BADCHAR);
  *(out++) |= d >> 2;
  *out = (d & 3) << 6;

  if (*in < '+' || *in > 'z')
    return (B64_BADCHAR);
  else if (*in == '=')
    {
      *len = 2;
      return (B64_SUCCESS);
    }

  d = decodearray[*in - '+'];
  if (d == -1)
    return (B64_BADCHAR);
  *out |= d;

  *len = 3;
  return (B64_SUCCESS);
}

/*
 * simple base64 encoding function that takes from 0 to 3 bytes and
 * outputs 4 encoded characters, with appropriate padding
 */
static int
b64_e (in, out, len)
     unsigned char *in, *out;
     int len;
{
  unsigned char codearray[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  if (!in || !out || len <= 0 || len > 3)
    return (B64_BADPARAM);

  *(out++) = codearray[((*in) >> 2)];

  if (--len == 0)
    {
      *(out++) = codearray[(((*in) & 3) << 4)];
      *(out++) = '=';
      *out = '=';
      return (B64_SUCCESS);
    }

  *(out++) = codearray[(((*in) & 3) << 4) | ((*(in + 1)) >> 4)];
  in++;

  if (--len == 0)
    {
      *(out++) = codearray[(((*in) & 15) << 2)];
      *out = '=';
      return (B64_SUCCESS);
    }

  *(out++) = codearray[(((*in) & 15) << 2) | ((*(in + 1)) >> 6)];
  *out = codearray[((*(in + 1)) & 63)];

  return (B64_SUCCESS);
}

/*
 * given an input string, generate an output gss_buffer_t containing the
 * decoded data and correct length; works by repeatedly driving b64_d ()
 * over the input string
 */
static int
b64_decode (enc, dec)
     char *enc;
     gss_buffer_t dec;
{
  char *tmp;
  int inlen, outlen = 0, t_len, ret;

  if (!enc || !dec)
    return (B64_BADPARAM);

  dec->value = 0;
  dec->length = 0;

  inlen = strlen (enc);
  if ((inlen % 4))
    return (B64_BADLEN);

  dec->value = (void *) (tmp = (char *) malloc ((inlen / 4) * 3));

  if (! tmp)
    return (B64_NOMEM);

  for (; inlen; inlen -= 4)
    {
      ret = b64_d (enc, tmp, &t_len);
      if (ret != B64_SUCCESS)
	{
	  free ((char *) dec->value);
	  dec->value = 0;
	  return (ret);
	}
      else if (t_len != 3)
	{
	  dec->length = outlen + t_len;
	  return (B64_SUCCESS);
	}
      else
	{
	  enc += 4;
	  tmp += t_len;
	  outlen += t_len;
	}
    }

  dec->length = outlen;
  return (B64_SUCCESS);
}

/*
 * given a gss_buffer_t, generate an encoded string containing the data.
 * works by repeatedly driving b64_e () over the contents of the buffer_t
 */
static int
b64_encode (dec, enc)
     gss_buffer_t dec;
     char **enc;
{
  unsigned char *tmp, *in;
  int ret, len;

  if (!dec || !enc)
    return (B64_BADPARAM);

  in = (unsigned char *) dec->value;
  len = dec->length;
  *enc = (char *) (tmp = (unsigned char *) malloc (((len * 4) / 3) + 5));

  if (! tmp)
    return (B64_NOMEM);

  do
    {
      ret = b64_e (in, tmp, len >= 3 ? 3 : len);
      if (ret != B64_SUCCESS)
	{
	  free (*enc);
	  *enc = 0;
	  return (ret);
	}
      else
	{
	  in += 3;
	  tmp += 4;
	}
    } while ((len -= 3) > 0);

  *tmp = '\0';

  return (B64_SUCCESS);
}

#endif /* GSSAPI */

/* Return a pointer to the first CRLF in IN_STRING, which can contain
   embedded nulls and has LEN characters in it not including the final
   null, or 0 if it does not contain one.  */

static char *
find_crlf (in_string, len)
     char *in_string;
     int len;
{
  while (len--)
    {
      if (*in_string == '\r')
	{
	  if (*++in_string == '\n')
	    return (in_string - 1);
	}
      else
	in_string++;
    }
  return (0);
}

#endif /* MAIL_USE_POP */
