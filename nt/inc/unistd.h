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

#endif	/* _UNISTD_H */
