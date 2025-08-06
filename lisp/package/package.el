;;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>
;; Created: 10 Mar 2007
;; Version: 1.1.0
;; Keywords: tools
;; Package-Requires: ((tabulated-list "1.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea behind package.el is to be able to download packages and
;; install them.  Packages are versioned and have versioned
;; dependencies.  Furthermore, this supports built-in packages which
;; may or may not be newer than user-specified packages.  This makes
;; it possible to upgrade Emacs and automatically disable packages
;; which have moved from external to core.  (Note though that we don't
;; currently register any of these, so this feature does not actually
;; work.)

;; A package is described by its name and version.  The distribution
;; format is either a tar file or a single .el file.

;; A tar file should be named "NAME-VERSION.tar".  The tar file must
;; unpack into a directory named after the package and version:
;; "NAME-VERSION".  It must contain a file named "PACKAGE-pkg.el"
;; which consists of a call to define-package.  It may also contain a
;; "dir" file and the info files it references.

;; A .el file is named "NAME-VERSION.el" in the remote archive, but is
;; installed as simply "NAME.el" in a directory named "NAME-VERSION".

;; The downloader downloads all dependent packages.  By default,
;; packages come from the official GNU sources, but others may be
;; added by customizing the `package-archives' alist.  Packages get
;; byte-compiled at install time.

;; At activation time we will set up the load-path and the info path,
;; and we will load the package's autoloads.  If a package's
;; dependencies are not available, we will not activate that package.

;; Conceptually a package has multiple state transitions:
;;
;; * Download.  Fetching the package from ELPA.
;; * Install.  Untar the package, or write the .el file, into
;;   ~/.emacs.d/elpa/ directory.
;; * Autoload generation.
;; * Byte compile.  Currently this phase is done during install,
;;   but we may change this.
;; * Activate.  Evaluate the autoloads for the package to make it
;;   available to the user.
;; * Load.  Actually load the package and run some code from it.

;; Other external functions you may want to use:
;;
;; M-x list-packages
;;    Enters a mode similar to buffer-menu which lets you manage
;;    packages.  You can choose packages for install (mark with "i",
;;    then "x" to execute) or deletion, and you can see what packages
;;    are available.  This will automatically fetch the latest list of
;;    packages from ELPA.
;;
;; M-x package-install-from-buffer
;;    Install a package consisting of a single .el file that appears
;;    in the current buffer.  This only works for packages which
;;    define a Version header properly; package.el also supports the
;;    extension headers Package-Version (in case Version is an RCS id
;;    or similar), and Package-Requires (if the package requires other
;;    packages).
;;
;; M-x package-install-file
;;    Install a package from the indicated file.  The package can be
;;    either a tar file or a .el file.  A tar file must contain an
;;    appropriately-named "-pkg.el" file; a .el file must be properly
;;    formatted as with `package-install-from-buffer'.

;;; Thanks:
;;; (sorted by sort-lines):

;; Jim Blandy <jimb@red-bean.com>
;; Karl Fogel <kfogel@red-bean.com>
;; Kevin Ryde <user42@zip.com.au>
;; Lawrence Mitchell
;; Michael Olson <mwolson@member.fsf.org>
;; Sebastian Tennant <sebyte@smolny.plus.com>
;; Stefan Monnier <monnier@iro.umontreal.ca>
;; Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Phil Hagelberg <phil@hagelb.org>

;;; ToDo:

;; - putting info dirs at the start of the info path means
;;   users see a weird ordering of categories.  OTOH we want to
;;   override later entries.  maybe emacs needs to enforce
;;   the standard layout?
;; - put bytecode in a separate directory tree
;; - perhaps give users a way to recompile their bytecode
;;   or do it automatically when emacs changes
;; - give users a way to know whether a package is installed ok
;; - give users a way to view a package's documentation when it
;;   only appears in the .el
;; - use/extend checkdoc so people can tell if their package will work
;; - "installed" instead of a blank in the status column
;; - tramp needs its files to be compiled in a certain order.
;;   how to handle this?  fix tramp?
;; - maybe we need separate .elc directories for various emacs
;;   versions.  That way conditional compilation can work.  But would
;;   this break anything?
;; - William Xu suggests being able to open a package file without
;;   installing it
;; - Interface with desktop.el so that restarting after an install
;;   works properly
;; - Use hierarchical layout.  PKG/etc PKG/lisp PKG/info
;;   ... except maybe lisp?
;; - It may be nice to have a macro that expands to the package's
;;   private data dir, aka ".../etc".  Or, maybe data-directory
;;   needs to be a list (though this would be less nice)
;;   a few packages want this, eg sokoban
;; - Allow multiple versions on the server, so that if a user doesn't
;;   meet the requirements for the most recent version they can still
;;   install an older one.
;; - Allow optional package dependencies
;;   then if we require 'bbdb', bbdb-specific lisp in lisp/bbdb
;;   and just don't compile to add to load path ...?
;; - Our treatment of the info path is somewhat bogus

;;; Code:

(require 'package-install)
(require 'package-menu)
(require 'package-describe)

(provide 'package)
;;; package.el ends here
