/* Hey Emacs, this is -*- C -*- code!  */

/* The default search path for Lisp function "load".
   This sets load-path.  */
#define PATH_LOADSEARCH "EMACS_LIBRARY:[LOCAL-LISP],EMACS_LIBRARY:[LISP]"

/* Like PATH_LOADSEARCH, but used only when Emacs is dumping.  This
   path is usually identical to PATH_LOADSEARCH except that the entry
   for the directory containing the installed lisp files has been
   replaced with ../lisp.  */
#define PATH_DUMPLOADSEARCH "[-.LISP]"

/* The extra search path for programs to invoke.  This is appended to
   whatever the PATH environment variable says to set the Lisp
   variable exec-path and the first file name in it sets the Lisp
   variable exec-directory.  exec-directory is used for finding
   executables and other architecture-dependent files.  */
#define PATH_EXEC "EMACS_LIBRARY:[LIB-SRC]"

/* Where Emacs should look for its architecture-independent data
   files, like the docstring file.  The lisp variable data-directory
   is set to this value.  */
#define PATH_DATA "EMACS_LIBRARY:[ETC]"

/* the name of the directory that contains lock files
  with which we record what files are being modified in Emacs.
  This directory should be writable by everyone.  */
#define PATH_LOCK "EMACS_LIBRARY:[LOCK]"

/* the name of the file !!!SuperLock!!! in the directory
  specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "EMACS_LIBRARY:[LOCK]$$$SUPERLOCK$$$."
