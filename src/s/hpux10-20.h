#include "hpux10.h"

#define HPUX10_20

/* otherwise sigunblock wont be defined */
#define POSIX_SIGNALS

/* Polling problems (interrupted system call) reported for HP-UX 10.10
   don't exist from 10.20 on (see process.c) */
#undef POLL_INTERRUPTED_SYS_CALL

/* arch-tag: 8d8dcbf1-ca9b-48a1-94be-b750de18a5c6
   (do not change this comment) */
