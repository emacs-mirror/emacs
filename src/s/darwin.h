/* System description header file for Darwin (Mac OS X).
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010 Free Software Foundation, Inc.

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


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define BSD4_2
/* BSD4_3 and BSD4_4 are already defined in sys/param.h */
#define BSD_SYSTEM

/* More specific than the above two.  We cannot use __APPLE__ as this
   may not be defined on non-OSX Darwin, and we cannot define DARWIN
   here because Panther and lower CoreFoundation.h uses DARWIN to
   distinguish OS X from pure Darwin. */

#define DARWIN_OS


/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "darwin"

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.

   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   Emacs uses the presence or absence of the SIGIO and BROKEN_SIGIO macros
   to indicate whether or not signal-driven I/O is possible.  It uses
   INTERRUPT_INPUT to decide whether to use it by default.

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvantages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe. */

#define INTERRUPT_INPUT

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_TERMIOS if the system provides POSIX-style
 *	functions and macros for terminal control.
 *
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 *
 *	Do not define both.  HAVE_TERMIOS is preferred, if it is
 *	supported on your system.
 */

#define HAVE_TERMIOS

#define NO_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 *      Note: PTYs are broken on darwin <6.  Use at your own risk.
 */

#define HAVE_PTYS

/**
 * PTYs only work correctly on Darwin 7 or higher.  So make the
 * default for process-connection-type dependent on the kernel
 * version.
 */
#define MIN_PTY_KERNEL_VERSION '7'

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* Avoid the use of the name init_process (process.c) because it is
   also the name of a Mach system call.  */
#define init_process emacs_init_process

/* Used in dispnew.c.  Copied from freebsd.h. */
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* System uses OXTABS instead of the expected TAB3.  (Copied from
   bsd386.h.)  */
#define TAB3 OXTABS

/* Darwin ld insists on the use of malloc routines in the System
   framework.  */
#define SYSTEM_MALLOC

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* Definitions for how to dump.  Copied from nextstep.h.  */

#define UNEXEC unexmacosx.o

#define START_FILES pre-crt0.o

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START (0)

/* Definitions for how to compile & link.  */

#ifdef HAVE_NS
#define LIBS_NSGUI -framework AppKit
#define SYSTEM_PURESIZE_EXTRA 200000
#define HEADERPAD_EXTRA 6C8
#else /* !HAVE_NS */
#define LIBS_NSGUI
#define HEADERPAD_EXTRA 690
#endif /* !HAVE_NS */

/* On Darwin, res_init appears not to be useful: see bug#562 and
   http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01467.html  */

#undef HAVE_RES_INIT
#undef HAVE_LIBRESOLV

/* The -headerpad option tells ld (see man page) to leave room at the
   end of the header for adding load commands.  Needed for dumping.
   0x690 is the total size of 30 segment load commands (at 56
   each); under Cocoa 31 commands are required.  */
#define LD_SWITCH_SYSTEM_TEMACS -prebind LIBS_NSGUI -Xlinker -headerpad -Xlinker HEADERPAD_EXTRA

#define C_SWITCH_SYSTEM_TEMACS -Dtemacs

#ifdef temacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
/* Don't use posix_memalign because it is not compatible with
   unexmacosx.c.  */
#undef HAVE_POSIX_MEMALIGN
#endif

/* The ncurses library has been moved out of the System framework in
   Mac OS X 10.2.  So if ./configure detects it, set the command-line
   option to use it.  */
#ifdef HAVE_LIBNCURSES
#define LIBS_TERMCAP -lncurses
/* This prevents crashes when running Emacs in Terminal.app under
   10.2.  */
#define TERMINFO
#endif

/* Link this program just by running cc.  */
#define ORDINARY_LINK

/* Adding -lm confuses the dynamic linker, so omit it.  */
#define LIB_MATH

/* Define the following so emacs symbols will not conflict with those
   in the System framework.  Otherwise -prebind will not work.  */

/* Do not define abort in emacs.c.  */
#define NO_ABORT

/* Do not define matherr in floatfns.c.  */
#define NO_MATHERR

/* The following solves the problem that Emacs hangs when evaluating
   (make-comint "test0" "/nodir/nofile" nil "") when /nodir/nofile
   does not exist.  Also, setsid is not allowed in the vfork child's
   context as of Darwin 9/Mac OS X 10.5.  */
#undef HAVE_WORKING_VFORK
#define vfork fork

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

/* This makes create_process in process.c save and restore signal
   handlers correctly.  Suggested by Nozomu Ando.*/
#define POSIX_SIGNALS

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method for marking the
   stack.  */
#define GC_MARK_STACK   GC_MAKE_GCPROS_NOOPS

/* arch-tag: 481d443d-4f89-43ea-b5fb-49706d95fa41
   (do not change this comment) */
