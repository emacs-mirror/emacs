/* System description file for MS-DOS

   Copyright (C) 1993, 1996, 1997, 2001 Free Software Foundation, Inc.

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

/* Note: lots of stuff here was taken from s-msdos.h in demacs. */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
/* #define USG */
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
/* #define BSD4_2 */
/* #define BSD4_3 */
/* #define BSD_SYSTEM */
/* #define VMS */
#ifndef MSDOS
#define MSDOS
#endif

#ifdef __GO32__
#ifndef __DJGPP__
#define __DJGPP__ 1	/* V2 defines __DJGPP__ == 2 */
#endif
#else
You lose; /* Emacs for DOS must be compiled with DJGPP */
#endif

#define DOS_NT	/* MSDOS or WINDOWSNT */
#undef BSD_SYSTEM
#undef VMS

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "ms-dos"

#define SYMS_SYSTEM syms_of_dosfns();syms_of_msdos();syms_of_win16select()

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.
   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvantages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe.
*/

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

/* #define FIRST_PTY_LETTER 'a' */

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

#define SYSV_SYSTEM_DIR

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* Define this is the compiler understands `volatile'.  */
#define HAVE_VOLATILE


/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#undef subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Avoid incompatibilities between gmalloc.c and system header files
   in how to declare valloc.  */
#define GMALLOC_INHIBIT_VALLOC

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

#if __DJGPP__ < 2

#define NO_MODE_T

/* New chdir () routine.
   DJGPP v2.0 and later doesn't need it because its chdir() does
   set the drive itself. */
#ifdef chdir
#undef chdir
#endif
#define chdir sys_chdir

#define LIBS_SYSTEM -lpc  /* isn't required in DJGPP v2.0, either */

#endif /* __DJGPP__ < 2 */

#if __DJGPP__ > 1

#define DATA_START  (&etext + 1)
#define TEXT_START  &start
#define TEXT_END    &etext

#define _NAIVE_DOS_REGS

#else /* not __DJGPP__ > 1 */

/* This somehow needs to be defined even though we use COFF.  */
#define TEXT_START -1

#endif /* not __DJGPP__ > 1 */

#define ORDINARY_LINK

/* command.com does not understand `...` so we define this.  */
#define LIB_GCC -Lgcc
#define DONT_NEED_ENVIRON
#define SEPCHAR ';'

#define NULL_DEVICE "nul"

#if __DJGPP__ < 2
#define O_RDONLY        0x0001
#define O_WRONLY        0x0002
#define O_RDWR          0x0004
#define O_CREAT         0x0100
#define O_TRUNC         0x0200
#define O_EXCL          0x0400
#define O_APPEND        0x0800
#define O_TEXT          0x4000
#define O_BINARY        0x8000
#define NO_MATHERR
#endif

#define HAVE_INVERSE_HYPERBOLIC
#define FLOAT_CHECK_DOMAIN

/* When $TERM is "internal" then this is substituted:  */
#define INTERNAL_TERMINAL "pc|bios|IBM PC with colour display:\
:co#80:li#25:Co#16:pa#256:km:ms:cm=<CM>:cl=<CL>:ce=<CE>:\
:se=</SO>:so=<SO>:us=<UL>:ue=</UL>:md=<BD>:mh=<DIM>:mb=<BL>:mr=<RV>:me=<NV>:\
:AB=<BG %d>:AF=<FG %d>:op=<DefC>:"

/* Define this to a function (Fdowncase, Fupcase) if your file system
   likes that */
#define FILE_SYSTEM_CASE Fmsdos_downcase_filename

/* Define this to be the separator between devices and paths */
#define DEVICE_SEP ':'

/* We'll support either convention on MSDOG.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))

/* Call init_gettimeofday when TZ changes.  */
#if __DJGPP__ < 2
#define LOCALTIME_CACHE
#define tzset init_gettimeofday
#endif

/* bcopy under djgpp is quite safe */
#define GAP_USE_BCOPY
#define BCOPY_UPWARD_SAFE 1
#define BCOPY_DOWNWARD_SAFE 1

/* Mode line description of a buffer's type.  */
#define MODE_LINE_BINARY_TEXT(buf) (NILP(buf->buffer_file_type) ? "T" : "B")

/* Do we have POSIX signals?  */
#if __DJGPP__ > 1
#define POSIX_SIGNALS
#endif

/* We have (the code to control) a mouse.  */
#define HAVE_MOUSE

/* We canuse mouse menus.  */
#define HAVE_MENUS

/* We have support for faces.  */
#define HAVE_FACES

/* Define one of these for easier conditionals.  */
#ifdef HAVE_X_WINDOWS
/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 15000
#define HAVE_X11R5
#define LIBX11_SYSTEM -lxext -lsys
#else
/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 50000
#endif

/* Tell the garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the
   jmp_buf.  */

#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
