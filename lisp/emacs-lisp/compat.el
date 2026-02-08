;;; compat.el --- Stub of the Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>,
;;	   Daniel Mendler <mail@daniel-mendler.de>
;; Maintainers: Daniel Mendler <mail@daniel-mendler.de>,
;;		Compat Development <~pkal/compat-devel@lists.sr.ht>,
;;		emacs-devel@gnu.org
;; URL: https://github.com/emacs-compat/compat
;; Keywords: lisp, maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Compat package on ELPA provides forward-compatibility
;; definitions for other packages.  While mostly transparent, a
;; minimal API is necessary whenever core definitions change calling
;; conventions (e.g. `plist-get' can be invoked with a predicate from
;; Emacs 29.1 onward).  For core packages on ELPA to be able to take
;; advantage of this functionality, the macros `compat-function' and
;; `compat-call' have to be available in the core, usable even if
;; users do not have the Compat package installed, which this file
;; ensures.

;; A basic introduction to Compat is given in the Info node `(elisp)
;; Forwards Compatibility'.  Further details on Compat are documented
;; in the Info node `(compat) Top' (installed along with the Compat
;; package) or read the same manual online:
;; https://elpa.gnu.org/packages/doc/compat.html.

;;; Code:

(defmacro compat-function (fun)
  "Return compatibility function symbol for FUN.
This is a pseudo-compatibility stub for core packages on ELPA,
that depend on the Compat package, whenever the user doesn't have
the package installed on their current system."
  `#',fun)

(defmacro compat-call (fun &rest args)
  "Call compatibility function or macro FUN with ARGS.
This is a pseudo-compatibility stub for core packages on ELPA,
that depend on the Compat package, whenever the user doesn't have
the package installed on their current system."
  (cons fun args))

;;;; Clever trick to avoid installing Compat if not necessary

;; The versioning scheme of the Compat package follows that of Emacs,
;; to indicate the version of Emacs, that functionality is being
;; provided for.  For example, the Compat version number 29.2.3.9
;; would attempt to provide compatibility definitions up to Emacs
;; 29.2, while also designating that this is the third major release
;; and ninth minor release of Compat, for the specific Emacs release.

;; The package version of this file is specified programmatically,
;; instead of giving a fixed version in the header of this file.  This
;; is done to ensure that the version of compat.el provided by Emacs
;; always corresponds to the current version of Emacs.  In addition to
;; the major-minor version, a large "major release" makes sure that
;; the built-in version of Compat is always preferred over an external
;; installation.  This means that if a package specifies a dependency
;; on Compat which matches the current or an older version of Emacs
;; that is being used, no additional dependencies have to be
;; downloaded.
;;
;; Further details and background on this file can be found in the
;; bug#66554 discussion.

;;;###autoload (push (list 'compat
;;;###autoload             emacs-major-version
;;;###autoload             emacs-minor-version
;;;###autoload             9999)
;;;###autoload       package--builtin-versions)

(provide 'compat)
;;; compat.el ends here
