/* System description file for Windows NT.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

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

/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
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
#ifndef WINDOWSNT
#define WINDOWSNT
#endif
#ifndef DOS_NT
#define DOS_NT 	/* MSDOS or WINDOWSNT */
#endif

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here */
#define _VARARGS_ __cdecl

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal, or main) its calling
   convention must be whatever standard the libraries expect */
#define _CALLBACK_ __cdecl

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "windows-nt"
#define SYMS_SYSTEM syms_of_ntterm ()

#define NO_MATHERR 1

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS 1 */

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.

   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   Emacs uses the presence or absence of the SIGIO macro to indicate
   whether or not signal-driven I/O is possible.  It uses
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

/* #define INTERRUPT_INPUT 1 */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

/*
 *      Define HAVE_TERMIOS if the system provides POSIX-style
 *      functions and macros for terminal control.
 *
 *      Define HAVE_TERMIO if the system provides sysV-style ioctls
 *      for terminal control.
 *
 *      Do not define both.  HAVE_TERMIOS is preferred, if it is
 *      supported on your system.
 */

/* #define HAVE_TERMIOS 1 */
/* #define HAVE_TERMIO 1 */

/*
 *      Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *      Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL 1

/*
 *      Define HAVE_SELECT if the system supports the `select' system call.
 */

/* #define HAVE_SELECT 1 */

/*
 *      Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS 1 */

/*
 *      Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* NT supports Winsock which is close enough (with some hacks) */

#define HAVE_SOCKETS 1

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING
#define bzero(b, l) memset(b, 0, l)
#define bcopy(s, d, l) memmove(d, s, l)
#define bcmp(a, b, l) memcmp(a, b, l)

/* bcopy (aka memmove aka memcpy at least on x86) under MSVC is quite safe */
#define GAP_USE_BCOPY 1
#define BCOPY_UPWARD_SAFE 1
#define BCOPY_DOWNWARD_SAFE 1

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses 1

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF 1

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */
#define MAIL_USE_POP 1
#define MAIL_USE_SYSTEM_LOCK 1

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION 1 */

/* Define this if your operating system declares signal handlers to
   have a type other than the usual.  `The usual' is `void' for ANSI C
   systems (i.e. when the __STDC__ macro is defined), and `int' for
   pre-ANSI systems.  If you're using GCC on an older system, __STDC__
   will be defined, but the system's include files will still say that
   signal returns int or whatever; in situations like that, define
   this to be what the system's include files want.  */
/* #define SIGTYPE int */

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
#define SEPCHAR ';'

#define ORDINARY_LINK 1

/* ============================================================ */

/* Here, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Define this to be the separator between path elements */
#define DIRECTORY_SEP XINT (Vdirectory_sep_char)

/* Define this to be the separator between devices and paths */
#define DEVICE_SEP ':'

/* We'll support either convention on NT.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))

/* The null device on Windows NT. */
#define NULL_DEVICE     "NUL:"

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

#define LISP_FLOAT_TYPE 1

#undef  HAVE_SYS_SELECT_H
#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#undef  HAVE_UTIME_H
#undef  HAVE_LINUX_VERSION_H
#undef  HAVE_SYS_SYSTEMINFO_H
#undef  HAVE_TERMIOS_H
#define HAVE_LIMITS_H 1
#define HAVE_STRING_H 1
#define STDC_HEADERS 1
#define TIME_WITH_SYS_TIME 1

#define HAVE_GETTIMEOFDAY 1
#define HAVE_GETHOSTNAME 1
#undef  HAVE_GETDOMAINNAME
#define HAVE_DUP2 1
#define HAVE_RENAME 1
#define HAVE_CLOSEDIR 1
#define HAVE_FSYNC 1		/* fsync is called _commit in MSVC.  */

#undef  TM_IN_SYS_TIME
#undef  HAVE_TM_ZONE
#define HAVE_TZNAME 1

#define HAVE_LONG_FILE_NAMES 1

#define HAVE_MKDIR 1
#define HAVE_RMDIR 1
#define HAVE_RANDOM 1
#undef  HAVE_SYSINFO
#undef  HAVE_LRAND48
#define HAVE_BCOPY 1
#define HAVE_BCMP 1
#define HAVE_LOGB 1
#define HAVE_FREXP 1
#define HAVE_FMOD 1
#undef  HAVE_RINT
#undef  HAVE_CBRT
#define HAVE_FTIME 1
#undef  HAVE_RES_INIT /* For -lresolv on Suns.  */
#undef  HAVE_SETSID
#undef  HAVE_FPATHCONF
#define HAVE_SELECT 1
#define HAVE_MKTIME 1
#undef  HAVE_EUIDACCESS
#define HAVE_GETPAGESIZE 1
#define HAVE_TZSET 1
#define HAVE_SETLOCALE 1
#undef  HAVE_UTIMES
#undef  HAVE_SETRLIMIT
#undef  HAVE_SETPGID
#undef  HAVE_GETCWD
#define HAVE_SHUTDOWN 1
#define HAVE_STRFTIME 1

#define LOCALTIME_CACHE
#undef  HAVE_INET_SOCKETS

#undef  HAVE_AIX_SMT_EXP

/* Define if you have the ANSI `strerror' function.
   Otherwise you must have the variable `char *sys_errlist[]'.  */
#define HAVE_STRERROR 1

/* Define if `struct utimbuf' is declared by <utime.h>.  */
#undef  HAVE_STRUCT_UTIMBUF

#define HAVE_MOUSE 1
#define HAVE_H_ERRNO 1

#ifdef HAVE_NTGUI
#define HAVE_WINDOW_SYSTEM 1
#define HAVE_FACES 1
#define HAVE_MENUS 1
#endif

#define MODE_LINE_BINARY_TEXT(_b_) (NILP ((_b_)->buffer_file_type) ? "T" : "B")

/* get some redefinitions in place */

#ifdef emacs

/* calls that are emulated or shadowed */
#undef access
#define access  sys_access
#undef chdir
#define chdir   sys_chdir
#undef chmod
#define chmod   sys_chmod
#undef close
#define close   sys_close
#undef creat
#define creat   sys_creat
#define ctime	sys_ctime
#undef dup
#define dup     sys_dup
#undef dup2
#define dup2    sys_dup2
#define fopen   sys_fopen
#define link    sys_link
#define mkdir   sys_mkdir
#undef mktemp
#define mktemp  sys_mktemp
#undef open
#define open    sys_open
#define pipe    sys_pipe
#undef read
#define read    sys_read
#define rename  sys_rename
#define rmdir   sys_rmdir
#define select  sys_select
#define sleep   sys_sleep
#define strerror sys_strerror
#undef unlink
#define unlink  sys_unlink
#undef write
#define write   sys_write

/* subprocess calls that are emulated */
#define spawnve sys_spawnve
#define wait    sys_wait
#define kill    sys_kill
#define signal  sys_signal

#endif /* emacs */

/* map to MSVC names */
#define execlp    _execlp
#define execvp    _execvp
#define fcloseall _fcloseall
#define fdopen	  _fdopen
#define fgetchar  _fgetchar
#define fileno	  _fileno
#define flushall  _flushall
#define fputchar  _fputchar
#define fsync	  _commit
#define ftruncate _chsize
#define getw	  _getw
#define getpid    _getpid
#define isatty    _isatty
#define logb      _logb
#define _longjmp  longjmp
#define lseek     _lseek
#define popen     _popen
#define pclose    _pclose
#define putw	  _putw
#define umask	  _umask
#define utime	  _utime
#define utimbuf	  _utimbuf
#define index     strchr
#define rindex    strrchr
#define strdup    _strdup
#define strupr    _strupr
#define strnicmp  _strnicmp
#define stricmp   _stricmp
#define tzset     _tzset
#define tzname    _tzname

#ifdef HAVE_NTGUI
#define abort	w32_abort
#endif

/* this is hacky, but is necessary to avoid warnings about macro
   redefinitions using the SDK compilers */
#ifndef __STDC__
#define __STDC__ 1
#define MUST_UNDEF__STDC__
#endif
#include <direct.h>
#include <io.h>
#include <stdio.h>
#ifdef MUST_UNDEF__STDC__
#undef __STDC__
#undef MUST_UNDEF__STDC__
#endif

/* Defines that we need that aren't in the standard signal.h  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGTRAP 5               /* Trace trap */
#define SIGKILL 9               /* Die, die die */
#define SIGPIPE 13              /* Write on pipe with no readers */
#define SIGALRM 14              /* Alarm */
#define SIGCHLD 18              /* Death of child */

#ifndef NSIG
#define NSIG 23
#endif

/* For integration with MSDOS support.  */
#define getdisk()               (_getdrive () - 1)
#ifdef emacs
#define getdefdir(_drv, _buf)   ((_buf[0] = (_drv + 'A' - 1), _buf[1] = ':', _buf[2] = '/', _buf[3] = 0), 1)
#else
#define getdefdir(_drv, _buf)   _getdcwd (_drv, _buf, MAXPATHLEN)
#endif

extern char *get_emacs_configuration (void);
extern char *get_emacs_configuration_options (void);
#define EMACS_CONFIGURATION 	get_emacs_configuration ()
#define EMACS_CONFIG_OPTIONS	get_emacs_configuration_options ()

/* Define this so that winsock.h definitions don't get included with
   windows.h.  For this to have proper effect, config.h must always be
   included before windows.h.  */
#define _WINSOCKAPI_    1
#define _WINSOCK_H

/* Defines size_t and alloca ().  */
#ifdef USE_CRT_DLL
#define malloc e_malloc
#define free   e_free
#define realloc e_realloc
#define calloc e_calloc
#endif
#include <malloc.h>

#include <sys/stat.h>

/* Define for those source files that do not include enough NT 
   system files.  */
#ifndef NULL
#ifdef __cplusplus
#define NULL	0
#else
#define NULL	((void *)0)
#endif
#endif

/* For proper declaration of environ.  */
#include <stdlib.h>
#ifndef sys_nerr
#define sys_nerr _sys_nerr
#endif
#include <string.h>

/* We need a little extra space, see ../../lisp/loadup.el */
#define SYSTEM_PURESIZE_EXTRA 137500

/* For unexec to work on Alpha systems, we need to put Emacs'
   initialized data into a separate section from the CRT initialized
   data (because the Alpha linker freely reorders data variables, even
   across libraries, so our data and the CRT data get intermingled).

   Starting with MSVC 5.0, we must also place the uninitialized data
   into its own section.  VC5 intermingles uninitialized data from the CRT
   between Emacs' static uninitialized data and its public uninitialized
   data.  A separate .bss section for Emacs groups both static and
   public uninitalized together.

   Note that unexnt.c relies on this fact, and must be modified
   accordingly if this section name is changed, or if this pragma is
   removed.  Also, obviously, all files that define initialized data
   must include config.h to pick up this pragma.  */

/* Names must be < 8 bytes */
#pragma data_seg("EMDATA")
#pragma bss_seg("EMBSS")

/* #define FULL_DEBUG */
/* #define EMACSDEBUG */

#ifdef EMACSDEBUG
extern void _DebPrint (const char *fmt, ...);
#define DebPrint(stuff) _DebPrint stuff
#else
#define DebPrint(stuff)
#endif


/* ============================================================ */
