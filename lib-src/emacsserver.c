/* Communication subprocess for GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1992 Free Software Foundation, Inc.

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


/* The GNU Emacs edit server process is run as a subprocess of Emacs
   under control of the file lisp/server.el.
   This program accepts communication from client (program emacsclient.c)
   and passes their commands (consisting of keyboard characters)
   up to the Emacs which then executes them.  */

#define NO_SHORTNAMES
#include <../src/config.h>
#undef read
#undef write
#undef open
#undef close
#undef signal


#if !defined(HAVE_SOCKETS) && !defined(HAVE_SYSVIPC)
#include <stdio.h>

main ()
{
  fprintf (stderr, "Sorry, the Emacs server is supported only on systems\n");
  fprintf (stderr, "with Berkeley sockets or System V IPC.\n");
  exit (1);
}

#else /* HAVE_SOCKETS or HAVE_SYSVIPC */

#if ! defined (HAVE_SYSVIPC)
/* BSD code is very different from SYSV IPC code */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <sys/un.h>
#include <stdio.h>
#include <errno.h>

extern int errno;

main ()
{
  char system_name[32];
  int s, infd, fromlen;
  struct sockaddr_un server, fromunix;
  char *homedir;
  char *str, string[BUFSIZ], code[BUFSIZ];
  FILE *infile;
  FILE **openfiles;
  int openfiles_size;

  int geteuid ();
  char *getenv ();

  openfiles_size = 20;
  openfiles = (FILE **) malloc (openfiles_size * sizeof (FILE *));
  if (openfiles == 0)
    abort ();

  /* 
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      perror ("socket");
      exit (1);
    }
  server.sun_family = AF_UNIX;
#ifndef SERVER_HOME_DIR
  gethostname (system_name, sizeof (system_name));
  sprintf (server.sun_path, "/tmp/esrv%d-%s", geteuid (), system_name);

  if (unlink (server.sun_path) == -1 && errno != ENOENT)
    {
      perror ("unlink");
      exit (1);
    }
#else  
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr,"No home directory\n");
      exit (1);
    }
  strcpy (server.sun_path, homedir);
  strcat (server.sun_path, "/.emacs_server");
  /* Delete anyone else's old server.  */
  unlink (server.sun_path);
#endif

  if (bind (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2) < 0)
    {
      perror ("bind");
      exit (1);
    }
  /* Only this user can send commands to this Emacs.  */
  chmod (server.sun_path, 0600);
  /*
   * Now, just wait for everything to come in..
   */
  if (listen (s, 5) < 0)
    {
      perror ("listen");
      exit (1);
    }

  /* Disable sigpipes in case luser kills client... */
  signal (SIGPIPE, SIG_IGN);
  for (;;)
    {
      int rmask = (1 << s) + 1;
      if (select (s + 1, &rmask, 0, 0, 0) < 0)
	perror ("select");
      if (rmask & (1 << s))	/* client sends list of filenames */
	{
	  fromlen = sizeof (fromunix);
	  fromunix.sun_family = AF_UNIX;
	  infd = accept (s, (struct sockaddr *) &fromunix, &fromlen); /* open socket fd */
	  if (infd < 0)
	    {
	      if (errno == EMFILE || errno == ENFILE)
		printf ("Too many clients.\n");
	      else
		perror ("accept");
	      continue;
	    }

	  if (infd >= openfiles_size)
	    {
	      openfiles_size *= 2;
	      openfiles = (FILE **) realloc (openfiles,
					     openfiles_size * sizeof (FILE *));
	      if (openfiles == 0)
		abort ();
	    }

	  infile = fdopen (infd, "r+"); /* open stream */
	  if (infile == NULL)
	    {
	      printf ("Too many clients.\n");
	      write (infd, "Too many clients.\n", 18);
	      close (infd);		/* Prevent descriptor leak.. */
	      continue;
	    }
	  str = fgets (string, BUFSIZ, infile);
	  if (str == NULL)
	    {
	      perror ("fgets");
	      close (infd);		/* Prevent descriptor leak.. */
	      continue;
	    }
	  openfiles[infd] = infile;
	  printf ("Client: %d %s", infd, string);
	  /* If what we read did not end in a newline,
	     it means there is more.  Keep reading from the socket
	     and outputting to Emacs, until we get the newline.  */
	  while (string[strlen (string) - 1] != '\n')
	    {
	      if (fgets (string, BUFSIZ, infile) == 0)
		break;
	      printf ("%s", string);
	    }
	  fflush (stdout);
	  fflush (infile);
	  continue;
	}
      else if (rmask & 1) /* emacs sends codeword, fd, and string message */
	{
	  /* Read command codeword and fd */
	  clearerr (stdin);
	  scanf ("%s %d%*c", code, &infd);
	  if (ferror (stdin) || feof (stdin))
	    {
	      fprintf (stderr, "server: error reading from standard input\n");
	      exit (1);
	    }

	  /* Transfer text from Emacs to the client, up to a newline.  */
	  infile = openfiles[infd];
	  while (1)
	    {
	      if (fgets (string, BUFSIZ, stdin) == 0)
		break;
	      fprintf (infile, "%s", string);
	      if (string[strlen (string) - 1] == '\n')
		break;
	    }
	  fflush (infile);

	  /* If command is close, close connection to client.  */
	  if (strncmp (code, "Close:", 6) == 0) 
	    if (infd > 2) 
	      {
		fclose (infile);
		close (infd);
	      }
	  continue;
	} 
    }
}

#else  /* This is the SYSV IPC section */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <setjmp.h>

jmp_buf msgenv;

SIGTYPE
msgcatch ()
{
  longjmp (msgenv, 1);
}


/* "THIS has to be fixed.  Remember, stderr may not exist...-rlk."
   Incorrect.  This program runs as an inferior of Emacs.
   Its stderr always exists--rms.  */
#include <stdio.h>

main ()
{
  int s, infd, fromlen;
  key_t key;
  struct msgbuf * msgp =
    (struct msgbuf *) malloc (sizeof *msgp + BUFSIZ);
  struct msqid_ds msg_st;
  int p;
  char *homedir, *getenv ();
  char string[BUFSIZ];
  FILE *infile;

  /*
   * Create a message queue using ~/.emacs_server as the path for ftok
   */
  if ((homedir = getenv ("HOME")) == NULL)
    {
      fprintf (stderr,"No home directory\n");
      exit (1);
    }
  strcpy (string, homedir);
  strcat (string, "/.emacs_server");
  creat (string, 0600);
  key = ftok (string, 1);	/* unlikely to be anyone else using it */
  s = msgget (key, 0600 | IPC_CREAT);
  if (s == -1)
    {
      perror ("msgget");
      exit (1);
    }

  /* Fork so we can close connection even if parent dies */
  p = fork ();
  if (setjmp (msgenv))
    {
      msgctl (s, IPC_RMID, 0);
      kill (p, SIGKILL);
      exit (0);
    }
  signal (SIGTERM, msgcatch);
  signal (SIGINT, msgcatch);
  /* If parent goes away, remove message box and exit */
  if (p == 0)
    {
      p = getppid ();
      setpgrp ();		/* Gnu kills process group on exit */
      while (1)
	{
	  if (kill (p, 0) < 0)
	    {
	      msgctl (s, IPC_RMID, 0);
	      exit (0);
	    }
	  sleep (10);
	}
    }

  while (1)
    {
      if ((fromlen = msgrcv (s, msgp, BUFSIZ - 1, 1, 0)) < 0)
        {
	  perror ("msgrcv");
	  exit (1);
        }
      else
        {
	  msgctl (s, IPC_STAT, &msg_st);
	  strncpy (string, msgp->mtext, fromlen);
	  string[fromlen] = 0;	/* make sure */
	  /* Newline is part of string.. */
	  printf ("Client: %d %s", s, string); 
	  fflush (stdout);
	  /* Now, wait for a wakeup */
	  fgets (msgp->mtext, BUFSIZ, stdin);
	  msgp->mtext[strlen (msgp->mtext)-1] = 0;
	  /*	  strcpy (msgp->mtext, "done");*/
	  msgp->mtype = msg_st.msg_lspid;
	  msgsnd (s, msgp, strlen (msgp->mtext)+1, 0);
	}
    }
}

#endif /* HAVE_SYSVIPC */

#endif /* HAVE_SOCKETS or HAVE_SYSVIPC */
