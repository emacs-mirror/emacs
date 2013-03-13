/* paths.h file for MS Windows

Copyright (C) 1993, 1995, 1997, 1999, 2001-2013 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* Relative file names in this file that begin with "%emacs_dir%/" are
   treated specially by decode_env_path: they are expanded relative to
   the value of the emacs_dir environment variable, which points to
   the root of the Emacs tree.  */

/* The default search path for Lisp function "load".
   Together with PATH_SITELOADSEARCH, this sets load-path.  */
/* #define PATH_LOADSEARCH "/usr/local/lib/emacs/lisp" */
#define PATH_LOADSEARCH "%emacs_dir%/lisp;%emacs_dir%/leim"

/* Like PATH_LOADSEARCH, but contains the non-standard pieces.  */
#define PATH_SITELOADSEARCH "%emacs_dir%/site-lisp;%emacs_dir%/../site-lisp"

/* Like PATH_LOADSEARCH, but used only when Emacs is dumping.  This
   path is usually identical to PATH_LOADSEARCH except that the entry
   for the directory containing the installed lisp files has been
   replaced with ../lisp.  */
#define PATH_DUMPLOADSEARCH "../lisp"

/* The extra search path for programs to invoke.  This is appended to
   whatever the PATH environment variable says to set the Lisp
   variable exec-path and the first file name in it sets the Lisp
   variable exec-directory.  exec-directory is used for finding
   executables and other architecture-dependent files.  */
/* #define PATH_EXEC "/usr/local/lib/emacs/etc" */
#define PATH_EXEC "%emacs_dir%/bin;%emacs_dir%/lib-src/oo-spd/i386;%emacs_dir%/lib-src/oo/i386"

/* Where Emacs should look for its architecture-independent data
   files, like the NEWS file.  The lisp variable data-directory
   is set to this value.  */
/* #define PATH_DATA "/usr/local/lib/emacs/data" */
#define PATH_DATA "%emacs_dir%/etc"

/* Where Emacs should look for X bitmap files.
   The lisp variable x-bitmap-file-path is set based on this value.  */
#define PATH_BITMAPS ""

/* Where Emacs should look for its docstring file.  The lisp variable
   doc-directory is set to this value.  */
#define PATH_DOC "%emacs_dir%/etc"

/* Where the configuration process believes the info tree lives.  The
   lisp variable configure-info-directory gets its value from this
   macro, and is then used to set the Info-default-directory-list.  */
/* #define PATH_INFO "/usr/local/info" */
#define PATH_INFO "C:/emacs/info"
