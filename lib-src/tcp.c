/*
 * TCP/IP stream emulation for GNU Emacs.
 * Copyright (C) 1988, 1989, 1992, 1993 Free Software Foundation, Inc.

 * Author: Masanobu Umeda
 * Maintainer: umerin@mse.kyutech.ac.jp

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 *
 * Yasunari, Itoh at PFU limited contributed for Fujitsu UTS and SX/A.
 *
 * Thu Apr  6 13:47:37 JST 1989
 * USG fixes by Sakaeda <saka@mickey.trad.pf.fujitsu.junet>
 *
 * For Fujitsu UTS compile with:
 *	cc -O -o tcp tcp.c -DFUJITSU_UTS -lu -lsocket
 */

#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/types.h>

#ifdef FUJITSU_UTS
#define USG
#include <sys/ucbtypes.h>
#include <sys/tisp/socket.h>
#include <netdb.h>
#include <sys/tisp/in.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#endif

#ifdef USG
#include <sys/stat.h>
#include <signal.h>
#endif

#ifdef FUJITSU_UTS
#define bcopy(f, t, n)    memcpy (t, f, n)
#define bcmp(b1, b2, n)   (memcmp (b1, b2, n)!=0)
#define bzero(b, n)      memset (b, 0, n)
#endif

#ifdef USG
int selectable = 1;

sigout ()
{
  fcntl (fileno (stdin), F_SETFL, 0);
  exit (-1);
}
#endif

main (argc, argv)
     int argc;
     char *argv[];
{
  struct hostent	*host;
  struct sockaddr_in	sockin, sockme;
  struct servent	*serv;
  char	*hostname = NULL;
  char	*service = "nntp";
  int	port;
  int	readfds;
  int   writefds;
  int	server;			/* NNTP Server */
  int	emacsIn = fileno (stdin); /* Emacs intput */
  int	emacsOut = fileno (stdout); /* Emacs output */
  char	buffer[1024];
  int	nbuffer;		/* Number of bytes in buffer */
  int   wret;
  char  *retry;			/* retry bufferp */
  int   false = 0;		/* FALSE flag for setsockopt () */

  if (argc < 2)
    {
      fprintf (stderr, "Usage: %s HOST [SERVICE]\n", argv[0]);
      exit (1);
    }
  if (argc >= 2)
    hostname = argv[1];
  if (argc >= 3)
    service = argv[2];

  if ((host = gethostbyname (hostname)) == NULL)
    {
      perror ("gethostbyname");
      exit (1);
    }
  if (isdigit (service[0]))
    port = atoi (service);
  else
    {
      serv = getservbyname (service, "tcp");
      if (serv == NULL)
	{
	  perror ("getservbyname");
	  exit (1);
	}
      port = serv->s_port;
    }

  bzero (&sockin, sizeof (sockin));
  sockin.sin_family = host->h_addrtype;
  bcopy (host->h_addr, &sockin.sin_addr, host->h_length);
  sockin.sin_port = port;
  if ((server = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
      perror ("socket");
      exit (1);
    }
  if (setsockopt (server, SOL_SOCKET, SO_REUSEADDR, &false, sizeof (false)))
    {
      perror ("setsockopt");
      exit (1);
    }
  bzero (&sockme, sizeof (sockme));
  sockme.sin_family = sockin.sin_family;
  sockme.sin_addr.s_addr = INADDR_ANY;
  if (bind (server, &sockme, sizeof (sockme)) < 0)
    {
      perror ("bind");
      exit (1);
    }
  if (connect (server, &sockin, sizeof (sockin)) < 0)
    {
      perror ("connect");
      close (server);
      exit (1);
    }

#ifdef O_NDELAY
  fcntl (server, F_SETFL, O_NDELAY);

#ifdef USG
  /* USG pipe cannot not select emacsIn */
  {
    struct stat statbuf;
    fstat (emacsIn, &statbuf);
    if (statbuf.st_mode & 010000)
      selectable = 0;
    if (!selectable)
      {
	signal (SIGINT, sigout);
	fcntl (emacsIn, F_SETFL, O_NDELAY);
      }
  }
#endif
#endif

  /* Connection established. */
  while (1)
    {
      readfds = (1 << server) | (1 << emacsIn);
      if (select (32, &readfds, NULL, NULL, (struct timeval *)NULL) == -1)
	{
	  perror ("select");
	  exit (1);
	}
      if (readfds & (1 << emacsIn))
	{
	  /* From Emacs */
	  nbuffer = read (emacsIn, buffer, sizeof buffer -1);

#ifdef USG
	  if (selectable && nbuffer == 0)
	    {
	      goto finish;
	    }
	  else if (!(readfds & (1 << server)) && nbuffer == 0)
	    {
	      sleep (1);
	    }
	  else 
#else
	    if (nbuffer == 0)
	      goto finish;
#endif
	  for (retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret)
	    {
	      writefds = 1 << server;
	      if (select (server+1, NULL, &writefds, NULL, (struct timeval*)NULL) == -1)
		{
		  perror ("select");
		  exit (1);
		}
	      wret = write (server, retry, nbuffer);
	      if (wret < 0) goto finish;
	    }
	}
      if (readfds & (1 << server))
	{
	  /* From NNTP server */
	  nbuffer = read (server, buffer, sizeof buffer -1);
	  if (nbuffer == 0)
	    goto finish;
	  for (retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret)
	    {
	      writefds = 1 << emacsOut;
#ifdef USG
	      if (selectable)
#endif
		if (select (emacsOut+1, NULL, &writefds, NULL, (struct timeval*)NULL) == -1)
		  {
		    perror ("select");
		    exit (1);
		  }
	      wret = write (emacsOut, retry, nbuffer);
	      if (wret < 0) goto finish;
	    }
	}
    }

  /* End of communication. */
 finish:
  close (server);
#ifdef USG
  if (!selectable) fcntl (emacsIn, F_SETFL, 0);
#endif
  close (emacsIn);
  close (emacsOut);
  exit (0);
}
