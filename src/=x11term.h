#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <X11/Xutil.h>
#include <X11/X10.h>

#define XMOUSEBUFSIZE 64

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#define BLOCK_INPUT_DECLARE() int BLOCK_INPUT_mask
#ifdef SIGIO
#define BLOCK_INPUT() EMACS_SIGBLOCKX (SIGIO, BLOCK_INPUT_mask)
#define UNBLOCK_INPUT() \
  do { int _dummy; EMACS_SIGSETMASK (BLOCK_INPUT_mask, _dummy); } while (0)
#else /* not SIGIO */
#define BLOCK_INPUT()
#define UNBLOCK_INPUT()
#endif /* SIGIO */

#define CLASS  "Emacs"	/* class id for GNU Emacs, used in .Xdefaults, etc. */
