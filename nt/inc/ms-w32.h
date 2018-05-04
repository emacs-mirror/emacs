/* System description file for Windows NT.

Copyright (C) 1993-1995, 2001-2018 Free Software Foundation, Inc.

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
   differentiating it fom MinGW gcc. We need to test the presence of
   __MINGW64_VERSION_MAJOR in _mingw.h: */
#ifdef __MINGW32__
# include <_mingw.h>
# ifdef __MINGW64_VERSION_MAJOR
#  define MINGW_W64
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

/* And the select implementation does 1-byte read-ahead waiting
   for received packets, so datagrams are broken too.  */
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

/* This isn't perfect, as some systems might have the page file in
   another place.  Also, I suspect that the time stamp of that file
   might also change when Windows enlarges the file due to
   insufficient VM.  Still, this seems to be the most reliable way;
   the alternative (of using GetSystemTimes) won't work on laptops
   that hibernate, because the system clock is stopped then.  Other
   possibility would be to run "net statistics workstation" and parse
   the output, but that's gross.  So this should do; if the file is
   not there, the boot time will be returned as zero, and filelock.c
   already handles that.  */
#define BOOT_TIME_FILE "C:/pagefile.sys"

/* ============================================================ */

/* Here, add any special hacks needed to make Emacs work on this
   system.  For example, you might define certain system call names
   that don't exist on your system, or that do different things on
   your system and must be used only through an encapsulation (which
   you should place, by convention, in sysdep.c).  */

#ifdef __GNUC__
#ifndef __cplusplus
#undef inline
#endif
#else  /* MSVC */
#define inline __inline
#endif

#ifdef __GNUC__
/* config.h may have defined already.  */
# ifndef restrict
#  define restrict __restrict__
# endif
#else
  /* FIXME: should we define to __restrict, which MSVC supports? */
# define restrict
#endif

/* `mode_t' is not defined for MSVC. Define. */
#ifdef _MSC_VER
typedef unsigned short mode_t;
#endif

/* A va_copy replacement for MSVC.  */
#ifdef _MSC_VER
# ifdef _WIN64
#  ifndef va_copy               /* Need to be checked (?) */
#   define va_copy(d,s) ((d) = (s))
#  endif
# else	/* not _WIN64 */
#  define va_copy(d,s) ((d) = (s))
# endif	 /* not _WIN64 */
#endif	 /* _MSC_VER */

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
   require to link against ws2_32.a.  But we don't want to link with
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

#ifdef _MSC_VER
#include <sys/timeb.h>
#include <sys/stat.h>
#include <signal.h>

/* MSVC gets link-time errors without these redirections.  */
#define fstat(a, b) sys_fstat(a, b)
#define stat(a, b)  sys_stat(a, b)
#define utime       sys_utime
#endif

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
#undef read
#define read    sys_read
#define rename  sys_rename
#define rmdir   sys_rmdir
#define select  sys_select
#define pselect sys_select
#define sleep   sys_sleep
#define strerror sys_strerror
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

#endif /* emacs */

/* Used both in Emacs, in lib-src, and in Gnulib.  */
#undef open
#define open    sys_open

/* Map to MSVC names.  */
#define execlp    _execlp
#define execvp    _execvp
#include <stdint.h>		/* for intptr_t */
extern intptr_t _execvp (const char *, char **);
#ifdef MINGW_W64
/* GCC 6 has a builtin execve with the prototype shown below.  MinGW64
   changed the prototype in its process.h to match that, although the
   library function still calls _execve, which still returns intptr_t.
   However, using the prototype with intptr_t causes GCC to emit
   warnings.  Fortunately, execve is not used in the MinGW build, but
   the code that references it is still compiled.  */
extern int execve (const char *, char * const *, char * const *);
#else
extern intptr_t execve (const char *, char * const *, char * const *);
#endif
#define fdatasync _commit
#define fdopen	  _fdopen
#define fsync	  _commit
#define ftruncate _chsize
#define getpid    _getpid
#ifdef _MSC_VER
typedef int pid_t;
#define snprintf  _snprintf
#define strtoll   _strtoi64
#define copysign  _copysign
#endif
#define isatty    _isatty
#define _longjmp  longjmp
/* MinGW64 defines lseek to invoke lseek64.  */
#ifndef lseek
#define lseek     _lseek
#endif
#define popen     _popen
#define pclose    _pclose
#define strdup    _strdup
#define strupr    _strupr
#define strnicmp  _strnicmp
#define stricmp   _stricmp
#define tzset     _tzset

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

#ifdef _MSC_VER
/* This is hacky, but is necessary to avoid warnings about macro
   redefinitions using the MSVC compilers, since, when __STDC__ is
   undefined or zero, those compilers declare functions like fileno,
   lseek, and chdir, for which we defined macros above.  */
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
#else  /* !_MSC_VER */
#include <direct.h>
#include <io.h>
#include <stdio.h>
#endif	/* !_MSC_VER */
#ifndef fileno
#define fileno	  _fileno
#endif

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

#ifdef _MSC_VER
typedef int sigset_t;
typedef int ssize_t;
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
#define _WINSOCK_H

/* Defines size_t and alloca ().  */
#include <stdlib.h>
#include <sys/stat.h>
#ifdef _MSC_VER
#define alloca _alloca
#else
#include <malloc.h>
#endif

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

extern malloc_fn the_malloc_fn;
extern realloc_fn the_realloc_fn;
extern free_fn the_free_fn;

#define malloc(size) (*the_malloc_fn)(size)
#define free(ptr)   (*the_free_fn)(ptr)
#define realloc(ptr, size) (*the_realloc_fn)(ptr, size)

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

#ifdef _MSC_VER
# if defined(_WIN64)
typedef __int64 EMACS_INT;
typedef unsigned __int64 EMACS_UINT;
#  define EMACS_INT_MAX	          LLONG_MAX
#  define PRIuMAX                 "llu"
#  define pI			  "ll"
/* Fix a bug in MSVC headers : stdint.h */
#  define _INTPTR 2
# elif defined(_WIN32)
/* Temporarily disable wider-than-pointer integers until they're tested more.
   Build with CFLAGS='-DWIDE_EMACS_INT' to try them out.  */

#  ifdef WIDE_EMACS_INT

/* Use pre-C99-style 64-bit integers.  */
typedef __int64 EMACS_INT;
typedef unsigned __int64 EMACS_UINT;
#   define EMACS_INT_MAX           LLONG_MAX
#   define PRIuMAX                 "llu"
#   define pI			  "I64"
#  else
typedef int EMACS_INT;
typedef unsigned int EMACS_UINT;
#   define EMACS_INT_MAX           LONG_MAX
#   define PRIuMAX                 "lu"
#   define pI			  "l"
#  endif
# endif
#endif

/* We need a little extra space, see ../../lisp/loadup.el.  */
#define SYSTEM_PURESIZE_EXTRA 50000

#define DATA_START 	get_data_start ()

/* For unexec to work on Alpha systems, we need to put Emacs'
   initialized data into a separate section from the CRT initialized
   data (because the Alpha linker freely reorders data variables, even
   across libraries, so our data and the CRT data get intermingled).

   Starting with MSVC 5.0, we must also place the uninitialized data
   into its own section.  VC5 intermingles uninitialized data from the CRT
   between Emacs' static uninitialized data and its public uninitialized
   data.  A separate .bss section for Emacs groups both static and
   public uninitialized together.

   Note that unexw32.c relies on this fact, and must be modified
   accordingly if this section name is changed, or if this pragma is
   removed.  Also, obviously, all files that define initialized data
   must include config.h to pick up this pragma.  */

/* Names must be < 8 bytes.  */
#ifdef _MSC_VER
#pragma data_seg("EMDATA")
#pragma bss_seg("EMBSS")
#endif

/* #define FULL_DEBUG */
/* #define EMACSDEBUG */

#ifdef _MSC_VER
#if _MSC_VER >= 800 && !defined(__cplusplus)
/* Unnamed type definition in parentheses.
   A structure, union, or enumerated type with no name is defined in a
   parenthetical expression.  The type definition is meaningless.  */
#pragma warning(disable:4116)
/* 'argument' : conversion from 'type1' to 'type2', possible loss of
   data A floating point type was converted to an integer type.  A
   possible loss of data may have occurred.  */
#pragma warning(disable:4244)
/* Negative integral constant converted to unsigned type.
   An expression converts a negative integer constant to an unsigned type.
   The result of the expression is probably meaningless.  */
#pragma warning(disable:4308)
#endif
#endif

/* Event name for when emacsclient starts the Emacs daemon on Windows.  */
#define W32_DAEMON_EVENT "EmacsServerEvent"

/* ============================================================ */
