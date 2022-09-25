/* English descriptions of signals.
   Copyright (C) 2020-2022 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Bruno Haible <bruno@clisp.org>, 2020.  */

#include <config.h>

/* Specification.  */
#include <string.h>

#include <signal.h>

const char *
sigdescr_np (int sig)
{
  /* Note: Some platforms (glibc, FreeBSD, NetBSD, OpenBSD, AIX, IRIX, Haiku,
     Android) have an array 'sys_siglist'.  (On AIX, you need to declare it
     yourself, and it has fewer than NSIG elements.)  Its contents varies
     depending on the OS.
     On other OSes, you can invoke strsignal (sig) in the C locale.
     In the code below, we show the differences.
     You can see how cryptic some of these strings are.  We try to pick more
     understandable wordings.  */

  switch (sig)
    {
    /* Signals specified by ISO C.  */
    case SIGABRT:
      /* glibc: "Aborted".  *BSD: "Abort trap".  Solaris: "Abort".  */
      return "Aborted";
    case SIGFPE:
      /* glibc, *BSD: "Floating point exception".  Solaris: "Arithmetic exception".
         The latter is more correct, because of integer division by 0 or -1.  */
      return "Arithmetic exception";
    case SIGILL:
      return "Illegal instruction";
    case SIGINT:
      return "Interrupt";
    case SIGSEGV:
      return "Segmentation fault";
    case SIGTERM:
      return "Terminated";

    /* Signals specified by POSIX.
       <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html>  */
    #if defined SIGALRM
    case SIGALRM:
      return "Alarm clock";
    #endif
    #if defined SIGBUS
    case SIGBUS:
      return "Bus error";
    #endif
    #if defined SIGCHLD
    case SIGCHLD:
      /* glibc, *BSD: "Child exited".  Solaris: "Child status changed".  */
      return "Child stopped or exited";
    #endif
    #if defined SIGCONT
    case SIGCONT:
      return "Continued";
    #endif
    #if defined SIGHUP
    case SIGHUP:
      return "Hangup";
    #endif
    #if defined SIGKILL
    case SIGKILL:
      return "Killed";
    #endif
    #if defined SIGPIPE
    case SIGPIPE:
      return "Broken pipe";
    #endif
    #if defined SIGQUIT
    case SIGQUIT:
      return "Quit";
    #endif
    #if defined SIGSTOP
    case SIGSTOP:
      /* glibc, Solaris: "Stopped (signal)".  *BSD: "Suspended (signal)".  */
      return "Stopped (signal)";
    #endif
    #if defined SIGTSTP
    case SIGTSTP:
      /* glibc: "Stopped".  *BSD: "Suspended".  Solaris: "Stopped (user)".  */
      return "Stopped";
    #endif
    #if defined SIGTTIN
    case SIGTTIN:
      return "Stopped (tty input)";
    #endif
    #if defined SIGTTOU
    case SIGTTOU:
      return "Stopped (tty output)";
    #endif
    #if defined SIGUSR1
    case SIGUSR1:
      /* glibc, *BSD: "User defined signal 1".  Solaris: "User signal 1".  */
      return "User defined signal 1";
    #endif
    #if defined SIGUSR2
    case SIGUSR2:
      /* glibc, *BSD: "User defined signal 2".  Solaris: "User signal 2".  */
      return "User defined signal 2";
    #endif
    #if defined SIGPOLL
    case SIGPOLL:
      /* glibc: "I/O possible".  Solaris: "Pollable event".  */
      return "I/O possible";
    #endif
    #if defined SIGPROF
    case SIGPROF:
      return "Profiling timer expired";
    #endif
    #if defined SIGSYS
    case SIGSYS:
      return "Bad system call";
    #endif
    #if defined SIGTRAP
    case SIGTRAP:
      /* glibc, Solaris: "Trace/breakpoint trap".  *BSD: "Trace/BPT trap".  */
      return "Trace/breakpoint trap";
    #endif
    #if defined SIGURG
    case SIGURG:
      /* glibc, *BSD: "Urgent I/O condition".  Solaris: "Urgent socket condition".  */
      return "Urgent I/O condition";
    #endif
    #if defined SIGVTALRM
    case SIGVTALRM:
      return "Virtual timer expired";
    #endif
    #if defined SIGXCPU
    case SIGXCPU:
      /* glibc, *BSD: "CPU time limit exceeded".  Solaris: "Cpu limit exceeded".  */
      return "CPU time limit exceeded";
    #endif
    #if defined SIGXFSZ
    case SIGXFSZ:
      return "File size limit exceeded";
    #endif

    /* Other signals on other systems.  */
    /* native Windows */
    #if defined SIGBREAK
    case SIGBREAK:
      return "Ctrl-Break";
    #endif
    /* IRIX */
    #if defined SIGCKPT
    case SIGCKPT:
      return "Checkpoint"; /* See man 1 cpr, man 3C atcheckpoint */
    #endif
    /* Linux, IRIX, Cygwin */
    #if defined SIGCLD && SIGCLD != SIGCHLD
    case SIGCLD:
      return "Child stopped or exited";
    #endif
    /* AIX */
    #if defined SIGCPUFAIL
    case SIGCPUFAIL:
      /* AIX: "CPU failure predicted".  */
      return "CPU going down"; /* See man bindprocessor */
    #endif
    /* AIX */
    #if defined SIGDANGER
    case SIGDANGER:
      /* AIX: "Paging space low".  */
      return "Swap space nearly exhausted";
    #endif
    /* Mac OS X, FreeBSD, NetBSD, OpenBSD, Minix, AIX, IRIX, Cygwin, mingw */
    #if defined SIGEMT
    case SIGEMT:
      /* glibc/Hurd, *BSD: "EMT trap".  Solaris: "Emulation trap".  */
      return "Instruction emulation needed";
    #endif
    /* Mac OS X, FreeBSD, NetBSD, OpenBSD, Minix */
    #if defined SIGINFO && SIGINFO != SIGPWR
    case SIGINFO:
      return "Information request";
    #endif
    /* Linux, Mac OS X, FreeBSD, NetBSD, OpenBSD, Minix, AIX, IRIX, Cygwin */
    #if defined SIGIO && SIGIO != SIGPOLL
    case SIGIO:
      return "I/O possible";
    #endif
    /* Linux, IRIX, Cygwin, mingw */
    #if defined SIGIOT && SIGIOT != SIGABRT
    case SIGIOT:
      return "IOT instruction"; /* a PDP-11 instruction */
    #endif
    /* AIX */
    #if defined SIGKAP
    case SIGKAP:
      /* Process must issue a KSKAPACK ioctl, or will be killed in 30 seconds.  */
      /* AIX: "Monitor mode granted".  */
      return "Keep Alive Poll";
    #endif
    /* Haiku */
    #if defined SIGKILLTHR
    case SIGKILLTHR:
      return "Kill thread";
    #endif
    /* Minix */
    #if defined SIGKMEM
    case SIGKMEM:
      return "Kernel memory request";
    #endif
    /* Minix */
    #if defined SIGKMESS
    case SIGKMESS:
      return "Kernel message";
    #endif
    /* Minix */
    #if defined SIGKSIG
    case SIGKSIG:
      return "Kernel signal";
    #endif
    /* Minix */
    #if defined SIGKSIGSM
    case SIGKSIGSM:
      return "Kernel signal for signal manager";
    #endif
    /* FreeBSD */
    #if defined SIGLIBRT
    case SIGLIBRT:
      return "Real-time library interrupt";
    #endif
    /* Cygwin */
    #if defined SIGLOST && SIGLOST != SIGABRT && SIGLOST != SIGPWR
    case SIGLOST:
      /* Solaris: "Resource lost".  */
      return "File lock lost";
    #endif
    /* AIX */
    #if defined SIGMIGRATE
    case SIGMIGRATE:
      return "Process migration";
    #endif
    /* AIX */
    #if defined SIGMSG
    case SIGMSG:
      /* AIX: "Input device data".  */
      return "Message in the ring";
    #endif
    /* ACM */
    #if defined SIGPLAN
    case SIGPLAN:
      return "Programming language anomaly";
    #endif
    /* AIX */
    #if defined SIGPRE
    case SIGPRE:
      return "Programmed exception";
    #endif
    /* IRIX */
    #if defined SIGPTINTR
    case SIGPTINTR:
      return "Pthread interrupt";
    #endif
    /* IRIX */
    #if defined SIGPTRESCHED
    case SIGPTRESCHED:
      return "Pthread rescheduling";
    #endif
    /* Linux, NetBSD, Minix, AIX, IRIX, Cygwin */
    #if defined SIGPWR
    case SIGPWR:
      /* glibc: "Power failure".  NetBSD: "Power fail/restart".  */
      return "Power failure";
    #endif
    /* AIX */
    #if defined SIGRECONFIG
    case SIGRECONFIG:
      return "Dynamic logical partitioning changed";
    #endif
    /* AIX */
    #if defined SIGRECOVERY
    case SIGRECOVERY:
      return "Kernel recovery";
    #endif
    /* IRIX */
    #if defined SIGRESTART
    case SIGRESTART:
      return "Checkpoint restart"; /* See man 1 cpr, man 3C atrestart */
    #endif
    /* AIX */
    #if defined SIGRETRACT
    case SIGRETRACT:
      /* AIX: "Monitor mode retracted".  */
      return "Retracting Keep Alive Poll";
    #endif
    /* AIX */
    #if defined SIGSAK
    case SIGSAK:
      /* AIX: "Secure attention".  */
      return "Secure Attention Key";
    #endif
    /* ACM */
    #if defined SIGSAM
    case SIGSAM:
      return "Symbolic computation failed";
    #endif
    /* Minix */
    #if defined SIGSNDELAY
    case SIGSNDELAY:
      return "Done sending message";
    #endif
    /* AIX */
    #if defined SIGSOUND
    case SIGSOUND:
      /* AIX: "Sound completed".  */
      return "Sound configuration changed";
    #endif
    /* Linux */
    #if defined SIGSTKFLT
    case SIGSTKFLT:
      return "Stack fault";
    #endif
    /* AIX */
    #if defined SIGSYSERROR
    case SIGSYSERROR:
      return "Kernel error";
    #endif
    /* AIX */
    #if defined SIGTALRM
    case SIGTALRM:
      return "Thread alarm clock";
    #endif
    /* FreeBSD, OpenBSD */
    #if defined SIGTHR
    case SIGTHR:
      /* OpenBSD: "Thread AST".  */
      return "Thread library interrupt";
    #endif
    /* IRIX */
    #if defined SIGUME
    case SIGUME:
      return "Uncorrectable memory error";
    #endif
    /* AIX */
    #if defined SIGVIRT
    case SIGVIRT:
      return "Virtual time alarm clock";
    #endif
    /* AIX */
    #if defined SIGWAITING
    case SIGWAITING:
      /* AIX: "No runnable lwp".  */
      return "Thread waiting";
    #endif
    /* Linux, Mac OS X, FreeBSD, NetBSD, OpenBSD, Minix, AIX, IRIX, Cygwin, Haiku */
    #if defined SIGWINCH
    case SIGWINCH:
      /* glibc: "Window changed".  *BSD: "Window size changed" or "Window size changes".  */
      return "Window size changed";
    #endif

    default:
      return NULL;
    }
}
