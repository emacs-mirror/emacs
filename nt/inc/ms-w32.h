/* System description file for Windows NT.

Copyright (C) 1993-1995, 2001-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */

#ifndef WINDOWSNT
#define WINDOWSNT
#endif

#include <mingw_time.h>

/* MinGW-w64 gcc does not automotically define a macro for
   differentiating it from MinGW gcc. We need to test the presence of
   __MINGW64_VERSION_MAJOR in _mingw.h: */
#ifdef __MINGW32__
# include <_mingw.h>
# ifdef __MINGW64_VERSION_MAJOR
#  define MINGW_W64
# endif
# if defined __MINGW32_VERSION && __MINGW32_VERSION >= 5001000L
/* Avoid warnings about gettimeofday being deprecated.  */
#  undef __POSIX_2008_DEPRECATED
#  define __POSIX_2008_DEPRECATED
# endif
/* Old versions of MinGW don't have these in the w32api headers, and
   Gnulib uses them in some files.  */
# ifndef _WIN32_WINNT_WIN2K
#  define _WIN32_WINNT_WIN2K	0x0500
# endif
# ifndef _WIN32_WINNT_WINXP
#  define _WIN32_WINNT_WINXP	0x0501
# endif
# ifndef _WIN32_WINNT_WS03
#  define _WIN32_WINNT_WS03	0x0502
# endif
# ifndef _WIN32_WINNT_VISTA
#  define _WIN32_WINNT_VISTA	0x0600
# endif
# ifndef _WIN32_WINNT_WIN7
#  define _WIN32_WINNT_WIN7	0x0601
# endif
# ifndef _WIN32_WINNT_WIN8
#  define _WIN32_WINNT_WIN8	0x0602
# endif
# ifndef _WIN32_WINNT_WINBLUE
#  define _WIN32_WINNT_WINBLUE	0x0603
# endif
# ifndef _WIN32_WINNT_WIN10
#  define _WIN32_WINNT_WIN10	0x0A00
# endif
#endif

/* #undef const */

/* Number of chars of output in the buffer of a stdio stream. */
#ifdef __GNU_LIBRARY__
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->__bufp - (FILE)->__buffer)
#else
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here.  */
#define _VARARGS_ __cdecl

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal, or main) its calling
   convention must be whatever standard the libraries expect.  */
#define _CALLBACK_ __cdecl

/* Define HAVE_TIMEVAL if the system supports the BSD style clock values.
   Look in <sys/time.h> for a timeval structure.  */
#define HAVE_TIMEVAL 1

/* Our select emulation does 1-byte read-ahead waiting for received
   packets, so datagrams are broken.  */
#define BROKEN_DATAGRAM_SOCKETS 1

#define MAIL_USE_SYSTEM_LOCK 1

/* Define to 1 if GCC-style __attribute__ ((__aligned__ (expr))) works. */
#ifdef __GNUC__
#define HAVE_ATTRIBUTE_ALIGNED 1
#endif

/* Define to 1 if strtold conforms to C99. */
#ifdef __GNUC__
#define HAVE_C99_STRTOLD 1
#endif

#if (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 8))
# ifndef HAVE___BUILTIN_UNWIND_INIT
#  define HAVE___BUILTIN_UNWIND_INIT 1
# endif
#endif

/* ============================================================ */

/* Here, add any special hacks needed to make Emacs work on this
   system.  For example, you might define certain system call names
   that don't exist on your system, or that do different things on
   your system and must be used only through an encapsulation (which
   you should place, by convention, in sysdep.c).  */

#ifdef __GNUC__
# ifndef __cplusplus
#  undef inline
/* config.h may have defined already.  */
#  ifndef restrict
#   define restrict __restrict__
#  endif
# endif
#endif

#ifndef WINDOWSNT
/* Some of the files of Emacs which are intended for use with other
   programs assume that if you have a config.h file, you must declare
   the type of getenv.  */
extern char *getenv ();
#endif

/* Prevent accidental use of features unavailable in older Windows
   versions we still support.  MinGW64 defines this to a higher value
   in its system headers, and is not really compatible with values
   lower than 0x0500, so leave it alone.  */
#ifndef MINGW_W64
# undef _WIN32_WINNT
# define _WIN32_WINNT 0x0400
#endif

/* Make a leaner executable.  */
#define WIN32_LEAN_AND_MEAN

#include <sys/types.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

/* This is used to hold UTF-8 encoded file names.  */
#define MAX_UTF8_PATH   (MAXPATHLEN * 4)

#ifdef HAVE_NTGUI
# ifndef HAVE_WINDOW_SYSTEM
#  define HAVE_WINDOW_SYSTEM 1
#  define POLL_FOR_INPUT 1
# endif
#endif

/* Get some redefinitions in place.  */

#ifdef emacs

#ifdef MINGW_W64
/* MinGW64 specific stuff.  */
/* Make sure 'struct timespec' and 'struct timezone' are defined.  */
#include <sys/types.h>
#include <time.h>
/* This prototype avoids MinGW64 compiler warnings due to the fact
   that time.h is included before localtime is redirected to
   sys_localtime below.  */
extern struct tm * sys_localtime (const time_t *);
/* MinGW64 uses a 2-argument _setjmp, and setjmp is a macro defined to
   supply the 2nd arg correctly, so don't use _setjmp directly in that
   case.  */
#undef HAVE__SETJMP

/* Unlike MS and mingw.org, MinGW64 doesn't define gai_strerror as an
   inline function in a system header file, and instead seems to
   require linking against ws2_32.a.  But we don't want to link with
   -lws2_32, as that would make Emacs dependent on the respective DLL.
   So MinGW64 is amply punished here by the following:  */
#undef HAVE_GAI_STRERROR
#endif

/* The following is needed for recovery from C stack overflows.  */
#include <setjmp.h>
typedef jmp_buf sigjmp_buf;
#ifdef MINGW_W64
/* Evidently, MinGW64's longjmp crashes when invoked from an exception
   handler, see https://sourceforge.net/p/mingw-w64/mailman/message/32421953/.
   This seems to be an unsolved problem in the MinGW64 runtime.  So we
   use the GCC intrinsics instead.  FIXME.  */
#define sigsetjmp(j,m) __builtin_setjmp(j)
#else
#define sigsetjmp(j,m) setjmp(j)
#endif
extern void w32_reset_stack_overflow_guard (void);

/* Calls that are emulated or shadowed.  */
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
#define localtime sys_localtime
/* We redirect 'read' below, after including io.h, see bug#73444.  */
#define rename  sys_rename
#define rmdir   sys_rmdir
#define select  sys_select
#define pselect sys_select
#define sleep   sys_sleep
#undef unlink
#define unlink  sys_unlink
#undef opendir
#define opendir sys_opendir
#undef closedir
#define closedir sys_closedir
#undef readdir
#define readdir sys_readdir
#undef seekdir
#define seekdir sys_seekdir
/* This prototype is needed because some files include config.h
   _after_ the standard headers, so sys_unlink gets no prototype from
   stdio.h or io.h.  */
extern int sys_unlink (const char *);
#undef write
#define write   sys_write
#undef umask
#define umask   sys_umask
extern int sys_umask (int);
#define clock   sys_clock

/* Subprocess calls that are emulated.  */
#define spawnve sys_spawnve
#define kill    sys_kill
#define signal  sys_signal

/* Internal signals.  */
#define emacs_raise(sig) emacs_abort()

/* termcap.c calls that are emulated.  */
#define tputs   sys_tputs
#define tgetstr sys_tgetstr

/* cm.c calls that are emulated.  */
#define chcheckmagic sys_chcheckmagic
#define cmcostinit   sys_cmcostinit
#define cmgoto       sys_cmgoto
#define cmputc       sys_cmputc
#define Wcm_clear    sys_Wcm_clear

/* MinGW64 system headers include string.h too early, causing the
   compiler to emit a warning about sys_strerror having no
   prototype, or the linker fail to link.  */
#include <string.h>
#define strerror sys_strerror
char *sys_strerror (int);

#endif /* emacs */

/* Used both in Emacs, in lib-src, and in Gnulib.  */
#undef open
#define open    sys_open

/* Map to MSVC names.  */
#define tcdrain _commit
#define fdopen	  _fdopen
#define fsync	  _commit
#define ftruncate _chsize
#define getpid    _getpid
#define isatty    _isatty
#define _longjmp  longjmp
#define execvp    _execvp
#include <stdint.h>		/* for intptr_t */
extern intptr_t _execvp (const char *, char **);

/* We cannot include system header process.h, since there's src/process.h.  */
int _getpid (void);

/* Include time.h before redirecting tzname, since MSVC's time.h
   defines _tzname to call a function, but also declares tzname a
   2-element array.  Having the redirection before including the
   header thus has the effect of declaring a function that returns an
   array, and triggers an error message.  */
#include <time.h>
#define tzname    _tzname

/* Required for functions in lib/time_r.c, since we don't use lib/time.h.  */
extern struct tm *gmtime_r (time_t const * restrict, struct tm * restrict);
extern struct tm *localtime_r (time_t const * restrict, struct tm * restrict);

#include <direct.h>
#include <io.h>
#include <stdio.h>
#ifndef fileno
#define fileno	  _fileno
#endif

/* Here we redirect CRT's 'read' to our own implementation, see bug#73444.  */
#undef read
#define read    sys_read
int sys_read (int, char *, unsigned int);

/* Defines that we need that aren't in the standard signal.h.  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGTRAP 5               /* Trace trap */
#define SIGKILL 9               /* Die, die die */
#define SIGPIPE 13              /* Write on pipe with no readers */
#define SIGALRM 14              /* Alarm */
#define SIGCHLD 18              /* Death of child */
#define SIGPROF 19              /* Profiling */

#ifndef NSIG
#define NSIG 23
#endif

#ifndef ENOTSUP
#define ENOTSUP ENOSYS
#endif

/* In case lib/errno.h is not used.  */
#ifndef EOPNOTSUPP
#define EOPNOTSUPP 130
#endif

#ifdef MINGW_W64
#ifndef _POSIX
typedef _sigset_t sigset_t;
#endif
#endif

typedef void (_CALLBACK_ *signal_handler) (int);
extern signal_handler sys_signal (int, signal_handler);

struct sigaction {
  int sa_flags;
  void (_CALLBACK_ *sa_handler)(int);
  sigset_t sa_mask;
};
#define SA_RESTART      0
#define SIG_BLOCK       1
#define SIG_SETMASK     2
#define SIG_UNBLOCK     3

extern int sigemptyset (sigset_t *);
extern int sigaddset (sigset_t *, int);
extern int sigfillset (sigset_t *);
extern int sigprocmask (int, const sigset_t *, sigset_t *);
/* MinGW64 defines pthread_sigmask as zero in its pthread_signal.h
   header, but we have an implementation for that function in w32proc.c.  */
#ifdef pthread_sigmask
#undef pthread_sigmask
#endif
extern int pthread_sigmask (int, const sigset_t *, sigset_t *);
extern int sigismember (const sigset_t *, int);
extern int setpgrp (int, int);
extern int sigaction (int, const struct sigaction *, struct sigaction *);
extern int alarm (int);

extern int sys_kill (pid_t, int);

extern void explicit_bzero (void *, size_t);

/* For integration with MSDOS support.  */
#define getdisk()               (_getdrive () - 1)
#ifdef emacs
#define getdefdir(_drv, _buf)   ((_buf[0] = (_drv + 'A' - 1), _buf[1] = ':', _buf[2] = '/', _buf[3] = 0), 1)
#else
#define getdefdir(_drv, _buf)   _getdcwd (_drv, _buf, MAXPATHLEN)
#endif

#ifndef EMACS_CONFIGURATION
extern char *get_emacs_configuration (void);
extern char *get_emacs_configuration_options (void);
#define EMACS_CONFIGURATION 	get_emacs_configuration ()
#define EMACS_CONFIG_OPTIONS	get_emacs_configuration_options ()
#endif

/* Define this so that winsock.h definitions don't get included with
   windows.h.  For this to have proper effect, config.h must always be
   included before windows.h.  */
#define _WINSOCKAPI_    1
#if !(defined __MINGW32_VERSION && __MINGW32_VERSION >= 5000002L)
/* mingw.org's MinGW 5.x changed how it includes winsock.h and time.h,
   and now defining _WINSOCK_H skips the definition of struct timeval,
   which we don't want.  */
# define _WINSOCK_H
#endif

/* Defines size_t and alloca ().  */
#include <stdlib.h>
#include <sys/stat.h>
#include <malloc.h>

/* Needed in Emacs and in Gnulib.  */
/* This must be after including sys/stat.h, because we need mode_t.  */
#undef mkdir
#define mkdir(d,f)   sys_mkdir(d,f)
int sys_mkdir (const char *, mode_t);

#ifdef emacs

typedef void * (* malloc_fn)(size_t);
typedef void * (* realloc_fn)(void *, size_t);
typedef void (* free_fn)(void *);

extern void *malloc_before_dump(size_t);
extern void *realloc_before_dump(void *, size_t);
extern void free_before_dump(void *);
extern void *malloc_after_dump(size_t);
extern void *realloc_after_dump(void *, size_t);
extern void free_after_dump(void *);

extern void *malloc_after_dump_9x(size_t);
extern void *realloc_after_dump_9x(void *, size_t);
extern void free_after_dump_9x(void *);

extern void *sys_calloc(size_t, size_t);

extern malloc_fn the_malloc_fn;
extern realloc_fn the_realloc_fn;
extern free_fn the_free_fn;

#define malloc(size) (*the_malloc_fn)(size)
#define free(ptr)   (*the_free_fn)(ptr)
#define realloc(ptr, size) (*the_realloc_fn)(ptr, size)
#define calloc(num, size) sys_calloc(num, size)

#endif

/* Define for those source files that do not include enough NT system files.  */
#ifndef NULL
#ifdef __cplusplus
#define NULL	0
#else
#define NULL	((void *)0)
#endif
#endif

/* For proper declaration of environ.  */
#ifndef sys_nerr
#define sys_nerr _sys_nerr
#endif

/* This must be after including stdlib.h, which defines putenv on MinGW.  */
#ifdef putenv
# undef putenv
#endif
#define putenv    sys_putenv
extern int sys_putenv (char *);

extern int getloadavg (double *, int);
extern int getpagesize (void);

extern void * memrchr (void const *, int, size_t);

/* Declared here, since we don't use Gnulib's stdlib.h.  */
extern int mkostemp (char *, int);

#if defined (__MINGW32__)

/* Define to 1 if the system has the type `long long int'. */
# ifndef HAVE_LONG_LONG_INT
#  define HAVE_LONG_LONG_INT 1
# endif

/* Define to 1 if the system has the type `unsigned long long int'. */
# ifndef HAVE_UNSIGNED_LONG_LONG_INT
#  define HAVE_UNSIGNED_LONG_LONG_INT 1
# endif

#endif

#define DATA_START 	get_data_start ()

/* #define FULL_DEBUG */
/* #define EMACSDEBUG */

/* Event name for when emacsclient starts the Emacs daemon on Windows.  */
#define W32_DAEMON_EVENT "EmacsServerEvent"

/* ============================================================ */
