/* System description file for SCO 3.2v5.
   Copyright (C) 1996, 2002  Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Mark Diekhans <markd@grizzly.com>.  */

/* SCO is sort of like SVR3.  */
#include "usg5-3.h"
#define SCO_R5

#if 0 /* Turned off rather than make the Lisp code check for this. -- rms.
	 I am assuming that (at least most of) the tests for usg-unix-v
	 do the right thing for sco3.2v4 also.  Things that *might* be wrong
	 as a result of turning off these lines include the values of
	 ange-ftp-remote-shell-file-name (now remsh)
	 dired-chown-program (now just chown)
	 lpr-command (now lp)
	 nntp-buggy-select (now t)
	 rmail-spool-directory (now /usr/mail?)
	 and the actions of the function print-region-1.  */

/* SYSTEM_TYPE should indicate the kind of system you are using.  */
#undef SYSTEM_TYPE
#define SYSTEM_TYPE "SCO 3.2v4"
#endif

/* SCO supports job control.  */
#undef NOMULTIPLEJOBS

/* SCO has termios.  */
#define HAVE_TERMIOS

/* SCO has ptys with unusual names.  */
#define HAVE_PTYS

#define PTY_ITERATION \
   for (i = 0; ; i++)
#define PTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ptyp%d", i);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ttyp%d", i);

/* Sockets are an option on SCO.  If you have X, you have them.
   They also exist if you have TCP, but we don't know how to test
   for that.  */
#ifdef HAVE_X_WINDOWS
#define HAVE_SOCKETS
#endif

#ifndef __GNUC__
#define LINKER ld
#endif

/* This is safe since we already assumed HAVE_SOCKET
   if using X windows.  */
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM_COMMON -lpt -lnls -lnsl -lc -lsocket
#ifdef MOTIF
# define LIBX11_SYSTEM -lgen LIBX11_SYSTEM_COMMON
#else /* ndef MOTIF */
# define LIBX11_SYSTEM LIBX11_SYSTEM_COMMON
#endif /* ndef MOTIF */

#undef LIB_X11_LIB
#define LIB_X11_LIB -lX11

#ifdef HAVE_INET_SOCKETS /* This comes from autoconf.  */
#define HAVE_SOCKETS
#endif

#ifdef HAVE_SOCKETS
#define LIBS_SYSTEM -lsocket -lPW
#endif

#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif

/* This enables configure to tell that we have alloca.  */
#ifndef LIBS_SYSTEM
#define LIBS_SYSTEM -lPW
#endif

#ifdef HAVE_X11R5
/* configure can't get this right linking fails unless -lsocket is used.  */
#undef HAVE_XSCREENNUMBEROFSCREEN
#define HAVE_XSCREENNUMBEROFSCREEN
#endif

/* We don't have -loldX, and we don't need it.  */
#define LIB_XMENU_LIB

/* SCO does have TIOCGWINSZ.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_PTEM_H

/* We need to link with crt1.o and crtn.o.  */
#define START_FILES pre-crt0.o /lib/crt1.o
#define LIB_STANDARD -lc /lib/crtn.o

/* Send signals to subprocesses by "typing" signal chars at them.  */
#define SIGNALS_VIA_CHARACTERS

/* Specify program for etc/fakemail to run.  Define SMAIL if you are
   using smail, don't for MMDF.  */

#ifdef SMAIL
#define MAIL_PROGRAM_NAME "/bin/smail -q0"
#else
#define MAIL_PROGRAM_NAME "/usr/lib/mail/execmail"
#endif

/* Tell process_send_signal to use VSUSP instead of VSWTCH.  */
#define PREFER_VSUSP

/* SCO Unix has Posix signals, but in 3.2.5 something broken that causes
 * all keyboard-quit signals to be lost after the first one. */
#undef POSIX_SIGNALS

#define SIGMASKTYPE long

#ifndef NOT_C_CODE
extern SIGMASKTYPE sigprocmask_set;
#endif /* not NOT_C_CODE */

#define sigblock(sig)					\
     (sigprocmask_set = SIGEMPTYMASK | (sig),		\
      sigprocmask (SIG_BLOCK, &sigprocmask_set, NULL))
#define sigunblock(sig)						\
     (sigprocmask_set = SIGFULLMASK & ~(sig),			\
      sigprocmask (SIG_SETMASK, &sigprocmask_set, NULL))

#ifndef PENDING_OUTPUT_COUNT
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->__ptr - (FILE)->__base)
#endif

/* Use ELF and get real shared libraries */ 

#undef COFF
#define ELF

#define UNEXEC unexelf.o

#ifndef __GNUC__
#define C_SWITCH_SYSTEM -belf
#define LD_SWITCH_SYSTEM -belf
#endif

/* Don't disable static function, as SCO's header files have some.*/
#undef static

#undef START_FILES
#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/values-Xt.o
#undef LIB_STANDARD
#define LIB_STANDARD -lc /usr/ccs/lib/crtn.o

#define NARROWPROTO 1
