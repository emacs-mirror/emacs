/* the default search path for Lisp function "load" */
#define PATH_LOADSEARCH "EMACS_LIBRARY:[LISP]"

/* the extra search path for programs to invoke.
  This is appended to whatever the PATH environment variable says. */
#define PATH_EXEC "EMACS_LIBRARY:[ETC]"

/* the name of the directory that contains lock files
  with which we record what files are being modified in Emacs.
  This directory should be writable by everyone.  */
#define PATH_LOCK "EMACS_LIBRARY:[LOCK]"

/* the name of the file !!!SuperLock!!! in the directory
  specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "EMACS_LIBRARY:[LOCK]$$$SUPERLOCK$$$."
