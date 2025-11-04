/* Declarations for GNU's read utmp module.

   Copyright (C) 1992-2007, 2009-2025 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by jla; revised by djm */

#ifndef __READUTMP_H__
#define __READUTMP_H__

/* This file uses _GL_ATTRIBUTE_MALLOC, _GL_ATTRIBUTE_RETURNS_NONNULL,
   HAVE_UTMP_H, HAVE_UTMPX_H, HAVE_STRUCT_UTMP_*, HAVE_STRUCT_UTMPX_*,
   HAVE_UTMPNAME, HAVE_UTMPXNAME.  */
#if !_GL_CONFIG_H_INCLUDED
# error "Please include config.h first."
#endif

#include "idx.h"

#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

/* AIX 4.3.3 has both utmp.h and utmpx.h, but only struct utmp
   has the ut_exit member.  */
#if (HAVE_UTMPX_H && HAVE_UTMP_H && HAVE_STRUCT_UTMP_UT_EXIT \
     && ! HAVE_STRUCT_UTMPX_UT_EXIT)
# undef HAVE_UTMPX_H
#endif

/* HPUX 10.20 needs utmp.h, for the definition of e.g., UTMP_FILE.  */
#if HAVE_UTMP_H
# include <utmp.h>
#endif

/* Needed for BOOT_TIME, USER_PROCESS, LOGIN_PROCESS.  */
#if HAVE_UTMPX_H
# if defined _THREAD_SAFE && defined UTMP_DATA_INIT
    /* When including both utmp.h and utmpx.h on AIX 4.3, with _THREAD_SAFE
       defined, work around the duplicate struct utmp_data declaration.  */
#  define utmp_data gl_aix_4_3_workaround_utmp_data
# endif
# include <utmpx.h>
#endif


#ifdef __cplusplus
extern "C" {
#endif


/* Type of entries returned by read_utmp on all platforms.  */
struct gl_utmp
{
  /* All 'char *' here are of arbitrary length and point to storage
     with lifetime equal to that of this struct.  */
  char *ut_user;                /* User name */
  char *ut_id;                  /* Session ID */
  char *ut_line;                /* seat / device */
  char *ut_host;                /* for remote sessions: user@host or host,
                                   for local sessions: the X11 display :N */
  struct timespec ut_ts;        /* time */
  pid_t ut_pid;                 /* process ID of ? */
  pid_t ut_session;             /* process ID of session leader */
  short ut_type;                /* BOOT_TIME, USER_PROCESS, LOGIN_PROCESS,
                                   or other */
  struct { int e_termination; int e_exit; } ut_exit;
};

/* The following types, macros, and constants describe the 'struct gl_utmp'.  */
#define UT_USER(UT) ((UT)->ut_user)
#define UT_TIME_MEMBER(UT) ((UT)->ut_ts.tv_sec)
#define UT_PID(UT) ((UT)->ut_pid)
#define UT_TYPE_EQ(UT, V) ((UT)->ut_type == (V))
#define UT_TYPE_NOT_DEFINED 0
#define UT_EXIT_E_TERMINATION(UT) ((UT)->ut_exit.e_termination)
#define UT_EXIT_E_EXIT(UT) ((UT)->ut_exit.e_exit)

/* Type of entry returned by read_utmp().  */
typedef struct gl_utmp STRUCT_UTMP;

/* Size of the UT_USER (ut) member, or -1 if unbounded.  */
enum { UT_USER_SIZE = -1 };

/* Size of the ut->ut_id member, or -1 if unbounded.  */
enum { UT_ID_SIZE = -1 };

/* Size of the ut->ut_line member, or -1 if unbounded.  */
enum { UT_LINE_SIZE = -1 };

/* Size of the ut->ut_host member, or -1 if unbounded.  */
enum { UT_HOST_SIZE = -1 };


/* When read_utmp accesses a file (as opposed to fetching the information
   from systemd), it uses the following low-level types and macros.
   Keep them here, rather than moving them into readutmp.c, for backward
   compatibility.  */

#if HAVE_UTMPX_H

/* <utmpx.h> defines 'struct utmpx' with the following fields:

     Field        Type                       Platforms
     ----------   ------                     ---------
   ⎡ ut_user      char[]                     glibc, musl, macOS, FreeBSD, AIX, HP-UX, Solaris, Cygwin, Android
   ⎣ ut_name      char[]                     NetBSD, Minix
     ut_id        char[]                     glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
     ut_line      char[]                     glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
     ut_pid       pid_t                      glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
     ut_type      short                      glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
   ⎡ ut_tv        struct                     glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
   ⎢              { tv_sec; tv_usec; }
   ⎣ ut_time      time_t                     Cygwin
     ut_host      char[]                     glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
     ut_exit      struct                     glibc, musl, NetBSD, Minix, HP-UX, Solaris, Android
                  { e_termination; e_exit; }
     ut_session   [long] int                 glibc, musl, NetBSD, Minix, Solaris, Android
   ⎡ ut_addr      [long] int                 HP-UX, Cygwin
   ⎢ ut_addr_v6   [u]int[4]                  glibc, musl, Android
   ⎣ ut_ss        struct sockaddr_storage    NetBSD, Minix
 */

# if __GLIBC__ && _TIME_BITS == 64
/* This is a near-copy of glibc's struct utmpx, which stops working
   after the year 2038.  Unlike the glibc version, struct utmpx32
   describes the file format even if time_t is 64 bits.  */
#define _GL_UT_USER_SIZE  sizeof (((struct utmpx *) 0)->ut_user)
#define _GL_UT_ID_SIZE    sizeof (((struct utmpx *) 0)->ut_id)
#define _GL_UT_LINE_SIZE  sizeof (((struct utmpx *) 0)->ut_line)
#define _GL_UT_HOST_SIZE  sizeof (((struct utmpx *) 0)->ut_host)
struct utmpx32
{
  short int ut_type;               /* Type of login.  */
  pid_t ut_pid;                    /* Process ID of login process.  */
  char ut_line[_GL_UT_LINE_SIZE];  /* Devicename.  */
  char ut_id[_GL_UT_ID_SIZE];      /* Inittab ID.  */
  char ut_user[_GL_UT_USER_SIZE];  /* Username.  */
  char ut_host[_GL_UT_HOST_SIZE];  /* Hostname for remote login. */
  struct __exit_status ut_exit;    /* Exit status of a process marked
                                      as DEAD_PROCESS.  */
  /* The fields ut_session and ut_tv must be the same size when compiled
     32- and 64-bit.  This allows files and shared memory to be shared
     between 32- and 64-bit applications.  */
  int ut_session;                  /* Session ID, used for windowing.  */
  struct
  {
    /* Seconds.  Unsigned not signed, as glibc did not exist before 1970,
       and if the format is still in use after 2038 its timestamps
       will surely have the sign bit on.  This hack stops working
       at 2106-02-07 06:28:16 UTC.  */
    unsigned int tv_sec;
    int tv_usec;                   /* Microseconds.  */
  } ut_tv;                         /* Time entry was made.  */
  int ut_addr_v6[4];               /* Internet address of remote host.  */
  char ut_reserved[20];            /* Reserved for future use.  */
};
#  define UTMP_STRUCT_NAME utmpx32
# else
#  define UTMP_STRUCT_NAME utmpx
# endif
# define SET_UTMP_ENT setutxent
# define GET_UTMP_ENT getutxent
# define END_UTMP_ENT endutxent
# ifdef HAVE_UTMPXNAME /* glibc, musl, macOS, NetBSD, Minix, Solaris, Cygwin */
#  define UTMP_NAME_FUNCTION utmpxname
# elif defined UTXDB_ACTIVE /* FreeBSD */
#  define UTMP_NAME_FUNCTION(x) setutxdb (UTXDB_ACTIVE, x)
# elif defined __ANDROID__ /* Android */
/* As of Android NDK r26, the getutxent, setutxent functions are no-ops.
   Therefore we can ignore the file name.  */
#  define UTMP_NAME_FUNCTION(x) ((void) (x))
# endif

#elif HAVE_UTMP_H

/* <utmp.h> defines 'struct utmp' with the following fields:

     Field        Type                       Platforms
     ----------   ------                     ---------
   ⎡ ut_user      char[]                     glibc, musl, AIX, HP-UX, Solaris, Cygwin, Android
   ⎣ ut_name      char[]                     macOS, old FreeBSD, NetBSD, OpenBSD, Minix
     ut_id        char[]                     glibc, musl, AIX, HP-UX, Solaris, Cygwin, Android
     ut_line      char[]                     glibc, musl, macOS, old FreeBSD, NetBSD, OpenBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android
     ut_pid       pid_t                      glibc, musl, AIX, HP-UX, Solaris, Cygwin, Android
     ut_type      short                      glibc, musl, AIX, HP-UX, Solaris, Cygwin, Android
   ⎡ ut_tv        struct                     glibc, musl, Android
   ⎢              { tv_sec; tv_usec; }
   ⎣ ut_time      time_t                     macOS, old FreeBSD, NetBSD, OpenBSD, Minix, AIX, HP-UX, Solaris, Cygwin
     ut_host      char[]                     glibc, musl, macOS, old FreeBSD, NetBSD, OpenBSD, Minix, AIX, HP-UX, Cygwin, Android
     ut_exit      struct                     glibc, musl, AIX, HP-UX, Solaris, Android
                  { e_termination; e_exit; }
     ut_session   [long] int                 glibc, musl, Android
   ⎡ ut_addr      [long] int                 HP-UX, Cygwin
   ⎣ ut_addr_v6   [u]int[4]                  glibc, musl, Android
 */

# define UTMP_STRUCT_NAME utmp
# define SET_UTMP_ENT setutent
# define GET_UTMP_ENT getutent
# define END_UTMP_ENT endutent
# ifdef HAVE_UTMPNAME /* glibc, musl, NetBSD, Minix, AIX, HP-UX, Solaris, Cygwin, Android */
#  define UTMP_NAME_FUNCTION utmpname
# endif

#endif

/* Evaluates to 1 if gl_utmp's ut_id field may ever have a non-zero value.  */
#define HAVE_STRUCT_XTMP_UT_ID \
  (READUTMP_USE_SYSTEMD \
   || (HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_ID : HAVE_STRUCT_UTMP_UT_ID))

/* Evaluates to 1 if gl_utmp's ut_pid field may ever have a non-zero value.  */
#define HAVE_STRUCT_XTMP_UT_PID \
  (READUTMP_USE_SYSTEMD \
   || (HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_PID : HAVE_STRUCT_UTMP_UT_PID))

/* Evaluates to 1 if gl_utmp's ut_host field may ever be non-empty.  */
#define HAVE_STRUCT_XTMP_UT_HOST \
  (READUTMP_USE_SYSTEMD \
   || (HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_HOST : HAVE_STRUCT_UTMP_UT_HOST))

/* Definition of UTMP_FILE.
   On glibc systems, UTMP_FILE is "/var/run/utmp".  */
#if !defined UTMP_FILE && defined _PATH_UTMP
# define UTMP_FILE _PATH_UTMP
#endif
#ifdef UTMPX_FILE /* Solaris, SysVr4 */
# undef UTMP_FILE
# define UTMP_FILE UTMPX_FILE
#endif
#ifndef UTMP_FILE
# define UTMP_FILE "/etc/utmp"
#endif

/* Definition of WTMP_FILE.
   On glibc systems, UTMP_FILE is "/var/log/wtmp".  */
#if !defined WTMP_FILE && defined _PATH_WTMP
# define WTMP_FILE _PATH_WTMP
#endif
#ifdef WTMPX_FILE /* Solaris, SysVr4 */
# undef WTMP_FILE
# define WTMP_FILE WTMPX_FILE
#endif
#ifndef WTMP_FILE
# define WTMP_FILE "/etc/wtmp"
#endif

/* In early versions of Android, <utmp.h> did not define BOOT_TIME or
   LOGIN_PROCESS, only USER_PROCESS.  We need to use the value that is defined
   in newer versions of Android.  */
#if defined __ANDROID__ && !defined BOOT_TIME
# define BOOT_TIME 2
# define LOGIN_PROCESS 6
#endif

/* Some platforms, such as OpenBSD, don't have an ut_type field and don't have
   the BOOT_TIME, USER_PROCESS, and LOGIN_PROCESS macros.  But we want to
   support them in 'struct gl_utmp'.  */
#if !(HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_TYPE : HAVE_STRUCT_UTMP_UT_TYPE)
# define BOOT_TIME 2
# define USER_PROCESS 0
# define LOGIN_PROCESS 6
#endif

/* Macros that test (UT)->ut_type.  */
#ifdef BOOT_TIME
# define UT_TYPE_BOOT_TIME(UT) ((UT)->ut_type == BOOT_TIME)
#else
# define UT_TYPE_BOOT_TIME(UT) 0
#endif
#ifdef USER_PROCESS
# define UT_TYPE_USER_PROCESS(UT) ((UT)->ut_type == USER_PROCESS)
#else
# define UT_TYPE_USER_PROCESS(UT) 0
#endif
#ifdef LOGIN_PROCESS
# define UT_TYPE_LOGIN_PROCESS(UT) ((UT)->ut_type == LOGIN_PROCESS)
#else
# define UT_TYPE_LOGIN_PROCESS(UT) 0
#endif

/* Determines whether an entry *UT corresponds to a user process.  */
#define IS_USER_PROCESS(UT)                                    \
  ((UT)->ut_user[0] && UT_TYPE_USER_PROCESS (UT))

/* Define if read_utmp is not just a dummy.  */
#if READUTMP_USE_SYSTEMD || HAVE_UTMPX_H || HAVE_UTMP_H || defined __CYGWIN__ || defined _WIN32
# define READ_UTMP_SUPPORTED 1
#endif

/* Options for read_utmp.  */
enum
  {
    READ_UTMP_CHECK_PIDS   = 1,
    READ_UTMP_USER_PROCESS = 2,
    READ_UTMP_BOOT_TIME    = 4,
    READ_UTMP_NO_BOOT_TIME = 8
  };

/* Return a copy of (UT)->ut_user, without trailing spaces,
   as a freshly allocated string.  */
char *extract_trimmed_name (const STRUCT_UTMP *ut)
  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_DEALLOC_FREE
  _GL_ATTRIBUTE_RETURNS_NONNULL;

/* Read the utmp entries corresponding to file FILE into freshly-
   malloc'd storage, set *UTMP_BUF to that pointer, set *N_ENTRIES to
   the number of entries, and return zero.  If there is any error,
   return -1, setting errno, and don't modify the parameters.
   A good candidate for FILE is UTMP_FILE.
   If OPTIONS & READ_UTMP_CHECK_PIDS is nonzero, omit entries whose
   process-IDs do not currently exist.
   If OPTIONS & READ_UTMP_USER_PROCESS is nonzero, omit entries which
   do not correspond to a user process.
   If OPTIONS & READ_UTMP_BOOT_TIME is nonzero, omit all entries except
   the one that contains the boot time.
   If OPTIONS & READ_UTMP_NO_BOOT_TIME is nonzero, omit the boot time
   entries.

   This function is not multithread-safe, since on many platforms it
   invokes the functions setutxent, getutxent, endutxent.  These
   functions are needed because they may lock FILE (so that we don't
   read garbage when a concurrent process writes to FILE), but their
   drawback is that they have a common global state.  */
int read_utmp (char const *file, idx_t *n_entries, STRUCT_UTMP **utmp_buf,
               int options);


#ifdef __cplusplus
}
#endif

#endif /* __READUTMP_H__ */
