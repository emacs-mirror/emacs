/* Fake unistd.h: config.h already provides most of the relevant things. */

#ifndef _UNISTD_H
#define _UNISTD_H

/* On Microsoft platforms, <stdlib.h> declares 'environ'; on POSIX
   platforms, <unistd.h> does.  Every file in Emacs that includes
   <unistd.h> also includes <stdlib.h>, so there's no need to declare
   'environ' here.  */

/* Provide prototypes of library functions that are emulated on w32
   and whose prototypes are usually found in unistd.h on POSIX
   platforms.  */
extern ssize_t readlink (const char *, char *, size_t);
extern int symlink (char const *, char const *);
extern int setpgid (pid_t, pid_t);
extern pid_t getpgrp (void);
extern pid_t setsid (void);
extern pid_t tcgetpgrp (int);

extern int faccessat (int, char const *, int, int);

/* These are normally on fcntl.h, but we don't override that header.  */
/* Use values compatible with gnulib, as there's no reason to differ.  */
#define AT_FDCWD (-3041965)
#define AT_EACCESS 4
#define AT_SYMLINK_NOFOLLOW 4096

/* Here are some more fcntl.h macros that default to gnulib-compatible
   values.  Include <fcntl.h> first, to make sure we don't override
   its values if any.  FIXME: If we know <fcntl.h> does not define
   O_NOCTTY and O_RDWR, this can be replaced with a simple "#define
   O_NOCTTY 0" and "#define O_RDWR 2".  */
#include <fcntl.h>
#ifndef O_NOCTTY
#define O_NOCTTY 0
#endif
#ifndef O_RDWR
#define O_RDWR 2
#endif

#endif	/* _UNISTD_H */
