#include "usg5-4.h"

#define SOLARIS2

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  */
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */

/* Compile in non-ansi fashion to work around bugs in system header files.  */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Xs
#else /* GCC */
#define C_SWITCH_SYSTEM -traditional
#endif /* GCC */
#define const

/* Karl Berry writes:
If you have the misfortune to be running Solaris 2.1, you may have
noticed that the access system call does not check the readonlyness of
the filesystem the path refers to.  This is a bug, according to
access(2), but in the meantime, some of us need the right behavior.  */

/* Well, we released Emacs with this change, and fixed a typo, but
   people keep saying that it doesn't work, and that the patch is easy
   to install.  Patch number is 100947-02.  */
#undef SOLARIS_BROKEN_ACCESS
