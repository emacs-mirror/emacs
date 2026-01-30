;;; package-activate.el --- Core of the Emacs Package Manager -*- lexical-binding:t -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

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

;; This file contains the core definitions of package.el used to
;; activate packages at startup, as well as other functions that are
;; useful without having to load the entirety of package.el.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defcustom package-load-list '(all)
  "List of packages for `package-activate-all' to make available.
Each element in this list should be a list (NAME VERSION), or the
symbol `all'.  The symbol `all' says to make available the latest
installed versions of all packages not specified by other
elements.

For an element (NAME VERSION), NAME is a package name (a symbol).
VERSION should be t, a string, or nil.
If VERSION is t, the most recent version is made available.
If VERSION is a string, only that version is ever made available.
 Any other version, even if newer, is silently ignored.
 Hence, the package is \"held\" at that version.
If VERSION is nil, the package is not made available (it is \"disabled\")."
  :type '(repeat (choice (const all)
                         (list :tag "Specific package"
                               (symbol :tag "Package name")
                               (choice :tag "Version"
                                (const :tag "disable" nil)
                                (const :tag "most recent" t)
                                (string :tag "specific version")))))
  :risky t
  :version "24.1"
  :group 'package)

(defvar package--default-summary "No description available.")

(define-inline package-vc-p (pkg-desc)
  "Return non-nil if PKG-DESC is a VC package."
  (inline-letevals (pkg-desc)
    (inline-quote (eq (package-desc-kind ,pkg-desc) 'vc))))

(cl-defstruct (package-desc
               ;; Rename the default constructor from `make-package-desc'.
               (:constructor package-desc-create)
               ;; Has the same interface as the old `define-package',
               ;; which is still used in the "foo-pkg.el" files. Extra
               ;; options can be supported by adding additional keys.
               (:constructor
                package-desc-from-define
                (name-string version-string &optional summary requirements
                 &rest rest-plist
                 &aux
                 (name (intern name-string))
                 (version (if (eq (car-safe version-string) 'vc)
                              (version-to-list (cdr version-string))
                            (version-to-list version-string)))
                 (reqs (mapcar (lambda (elt)
                                 (list (car elt)
                                       (version-to-list (cadr elt))))
                               (if (eq 'quote (car requirements))
                                   (nth 1 requirements)
                                 requirements)))
                 (kind (plist-get rest-plist :kind))
                 (archive (plist-get rest-plist :archive))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (let ((value (cadr rest-plist)))
                                 (when value
                                   (push (cons (car rest-plist)
                                               (if (eq (car-safe value) 'quote)
                                                   (cadr value)
                                                 value))
                                         alist))))
                             (setq rest-plist (cddr rest-plist)))
                           alist)))))
  "Structure containing information about an individual package.
Slots:

`name'	Name of the package, as a symbol.

`version' Version of the package, as a version list.

`summary' Short description of the package, typically taken from
        the first line of the file.

`reqs'	Requirements of the package.  A list of (PACKAGE
        VERSION-LIST) naming the dependent package and the minimum
        required version.

`kind'	The distribution format of the package.  Currently, it is
        either `single', `tar', or (temporarily only) `dir'.  In
        addition, there is distribution format `vc', which is handled
        by package-vc.el.

`archive' The name of the archive (as a string) whence this
        package came.

`dir'	The directory where the package is installed (if installed),
        `builtin' if it is built-in, or nil otherwise.

`extras' Optional alist of additional keyword-value pairs.

`signed' Flag to indicate that the package is signed by provider."
  name
  version
  (summary package--default-summary)
  reqs
  kind
  archive
  dir
  extras
  signed)

;; Pseudo fields.
(defun package-version-join (vlist)
  "Return the version string corresponding to the list VLIST.
This is, approximately, the inverse of `version-to-list'.
\(Actually, it returns only one of the possible inverses, since
`version-to-list' is a many-to-one operation.)"
  (if (null vlist)
      ""
    (let ((str-list (list "." (int-to-string (car vlist)))))
      (dolist (num (cdr vlist))
        (cond
         ((>= num 0)
          (push (int-to-string num) str-list)
          (push "." str-list))
         ((< num -4)
          (error "Invalid version list `%s'" vlist))
         (t
          ;; pre, or beta, or alpha
          (cond ((equal "." (car str-list))
                 (pop str-list))
                ((not (string-match "[0-9]+" (car str-list)))
                 (error "Invalid version list `%s'" vlist)))
          (push (cond ((= num -1) "pre")
                      ((= num -2) "beta")
                      ((= num -3) "alpha")
                      ((= num -4) "snapshot"))
                str-list))))
      (if (equal "." (car str-list))
          (pop str-list))
      (apply #'concat (nreverse str-list)))))

(defun package-desc-full-name (pkg-desc)
  "Return full name of package-desc object PKG-DESC.
This is the name of the package with its version appended."
  (if (package-vc-p pkg-desc)
      (symbol-name (package-desc-name pkg-desc))
    (format "%s-%s"
            (package-desc-name pkg-desc)
            (package-version-join (package-desc-version pkg-desc)))))


;;; Installed packages
;; The following variables store information about packages present in
;; the system.  The most important of these is `package-alist'.  The
;; command `package-activate-all' is also closely related to this
;; section.

(defvar package--builtins nil
  "Alist of built-in packages.
The actual value is initialized by loading the library
`finder-inf'; this is not done until it is needed, e.g. by the
function `package-built-in-p'.

Each element has the form (PKG . PACKAGE-BI-DESC), where PKG is a package
name (a symbol) and DESC is a `package--bi-desc' structure.")
(put 'package--builtins 'risky-local-variable t)

(defvar package-alist nil
  "Alist of all packages available for activation.
Each element has the form (PKG . DESCS), where PKG is a package
name (a symbol) and DESCS is a non-empty list of `package-desc'
structures, sorted by decreasing versions.

This variable is set automatically by `package-load-descriptor',
called via `package-activate-all'.  To change which packages are
loaded and/or activated, customize `package-load-list'.")
(put 'package-alist 'risky-local-variable t)

;;;; Public interfaces for accessing built-in package info

;;;###autoload
(defvar package-activated-list nil
  ;; FIXME: This should implicitly include all builtin packages.
  "List of the names of currently activated packages.")
(put 'package-activated-list 'risky-local-variable t)

;;;; Populating `package-alist'.

;; The following functions are called on each installed package by
;; `package-load-all-descriptors', which ultimately populates the
;; `package-alist' variable.

(defun package-process-define-package (exp)
  "Process define-package expression EXP and push it to `package-alist'.
EXP should be a form read from a foo-pkg.el file.
Convert EXP into a `package-desc' object using the
`package-desc-from-define' constructor before pushing it to
`package-alist'.

If there already exists a package by the same name in
`package-alist', insert this object there such that the packages
are sorted with the highest version first."
  (when (eq (car-safe exp) 'define-package)
    (let* ((new-pkg-desc (apply #'package-desc-from-define (cdr exp)))
           (name (package-desc-name new-pkg-desc))
           (version (package-desc-version new-pkg-desc))
           (old-pkgs (assq name package-alist)))
      (if (null old-pkgs)
          ;; If there's no old package, just add this to `package-alist'.
          (push (list name new-pkg-desc) package-alist)
        ;; If there is, insert the new package at the right place in the list.
        (while
            (if (and (cdr old-pkgs)
                     (version-list-< version
                                     (package-desc-version (cadr old-pkgs))))
                (setq old-pkgs (cdr old-pkgs))
              (push new-pkg-desc (cdr old-pkgs))
              nil)))
      new-pkg-desc)))

(defun package-load-descriptor (pkg-dir)
  "Load the package description file in directory PKG-DIR.
Create a new `package-desc' object, add it to `package-alist' and
return it."
  (let ((pkg-file (expand-file-name (package--description-file pkg-dir)
                                    pkg-dir))
        (signed-file (concat pkg-dir ".signed")))
    (when (file-exists-p pkg-file)
      (with-temp-buffer
        (insert-file-contents pkg-file)
        (goto-char (point-min))
        (let ((pkg-desc (or (package-process-define-package
                             (read (current-buffer)))
                            (error "Can't find define-package in %s" pkg-file))))
          (setf (package-desc-dir pkg-desc) pkg-dir)
          (if (file-exists-p signed-file)
              (setf (package-desc-signed pkg-desc) t))
          pkg-desc)))))

(defun package-load-all-descriptors ()
  "Load descriptors for installed Emacs Lisp packages.
This looks for package subdirectories in `package-user-dir' and
`package-directory-list'.  The variable `package-load-list'
controls which package subdirectories may be loaded.

In each valid package subdirectory, this function loads the
description file containing a call to `define-package', which
updates `package-alist'."
  (dolist (dir (cons package-user-dir package-directory-list))
    (when (file-directory-p dir)
      (dolist (pkg-dir (directory-files dir t "\\`[^.]"))
        (when (file-directory-p pkg-dir)
          (package-load-descriptor pkg-dir))))))

(defun package--alist ()
  "Return `package-alist', after computing it if needed."
  (or package-alist
      (progn (package-load-all-descriptors)
             package-alist)))


;;; Package activation
;; Section for functions used by `package-activate', which see.

(defun package-disabled-p (pkg-name version)
  "Return whether PKG-NAME at VERSION can be activated.
The decision is made according to `package-load-list'.
Return nil if the package can be activated.
Return t if the package is completely disabled.
Return the max version (as a string) if the package is held at a lower version."
  (let ((force (assq pkg-name package-load-list)))
    (cond ((null force) (not (memq 'all package-load-list)))
          ((null (setq force (cadr force))) t) ; disabled
          ((eq force t) nil)
          ((stringp force)              ; held
           (unless (version-list-= version (version-to-list force))
             force))
          (t (error "Invalid element in `package-load-list'")))))

(defun package-built-in-p (package &optional min-version)
  "Return non-nil if PACKAGE is built-in to Emacs.
Optional arg MIN-VERSION, if non-nil, should be a version list
specifying the minimum acceptable version."
  (if (package-desc-p package) ;; was built-in and then was converted
      (eq 'builtin (package-desc-dir package))
    (let ((bi (assq package package--builtin-versions)))
      (cond
       (bi (version-list-<= min-version (cdr bi)))
       ((remove 0 min-version) nil)
       (t
        (require 'finder-inf nil t) ; For `package--builtins'.
        (assq package package--builtins))))))

(defun package--autoloads-file-name (pkg-desc)
  "Return the absolute name of the autoloads file, sans extension.
PKG-DESC is a `package-desc' object."
  (expand-file-name
   (format "%s-autoloads" (package-desc-name pkg-desc))
   (package-desc-dir pkg-desc)))

(declare-function info-initialize "info" ())

(defvar package--quickstart-pkgs t
  "If set to a list, we're computing the set of pkgs to activate.")

(defun package--add-info-node (pkg-dir)
  "Add info node located in PKG-DIR."
  (when (file-exists-p (expand-file-name "dir" pkg-dir))
    ;; FIXME: not the friendliest, but simple.
    (require 'info)
    (defvar Info-directory-list)
    (info-initialize)
    (add-to-list 'Info-directory-list pkg-dir)))

(defun package-activate-1 (pkg-desc &optional reload deps)
  "Activate package given by PKG-DESC, even if it was already active.
If DEPS is non-nil, also activate its dependencies (unless they
are already activated).
If RELOAD is non-nil, also `load' any files inside the package which
correspond to previously loaded files."
  (let* ((name (package-desc-name pkg-desc))
         (pkg-dir (package-desc-dir pkg-desc)))
    (unless pkg-dir
      (error "Internal error: unable to find directory for `%s'"
             (package-desc-full-name pkg-desc)))
    (catch 'exit
      ;; Activate its dependencies recursively.
      ;; FIXME: This doesn't check whether the activated version is the
      ;; required version.
      (when deps
        (dolist (req (package-desc-reqs pkg-desc))
          (unless (package-activate (car req))
            (message "Unable to activate package `%s'.\nRequired package `%s-%s' is unavailable"
                     name (car req) (package-version-join (cadr req)))
            (throw 'exit nil))))
      (if (listp package--quickstart-pkgs)
          ;; We're only collecting the set of packages to activate!
          (push pkg-desc package--quickstart-pkgs)
        (when (or reload (assq name package--builtin-versions))
          (require 'package)
          (declare-function package--reload-previously-loaded
                            "package" (pkg-desc &optional warn))

          (package--reload-previously-loaded
           pkg-desc (unless reload
                      "Package %S is activated too late.
The following files have already been loaded: %S")))
        (with-demoted-errors "Error loading autoloads: %s"
          (load (package--autoloads-file-name pkg-desc) nil t)))
      (package--add-info-node pkg-dir)
      (push name package-activated-list)
      ;; Don't return nil.
      t)))

;;;; `package-activate'

(defun package--get-activatable-pkg (pkg-name)
  ;; Is "activatable" a word?
  (let ((pkg-descs (cdr (assq pkg-name package-alist))))
    ;; Check if PACKAGE is available in `package-alist'.
    (while
        (when pkg-descs
          (let ((available-version (package-desc-version (car pkg-descs))))
            (or (package-disabled-p pkg-name available-version)
                ;; Prefer a builtin package.
                (package-built-in-p pkg-name available-version))))
      (setq pkg-descs (cdr pkg-descs)))
    (car pkg-descs)))

;; This function activates a newer version of a package if an older
;; one was already activated.  It also loads a features of this
;; package which were already loaded.
(defun package-activate (package &optional force)
  "Activate the package named PACKAGE.
If FORCE is true, (re-)activate it if it's already activated.
Newer versions are always activated, regardless of FORCE."
  (let ((pkg-desc (package--get-activatable-pkg package)))
    (cond
     ;; If no such package is found, maybe it's built-in.
     ((null pkg-desc)
      (package-built-in-p package))
     ;; If the package is already activated, just return t.
     ((and (memq package package-activated-list) (not force))
      t)
     ;; Otherwise, proceed with activation.
     (t (package-activate-1 pkg-desc nil 'deps)))))


;;; Installation -- Local operations
;; This section contains a variety of features regarding installing a
;; package to/from disk.  This includes autoload generation,
;; unpacking, compiling, as well as defining a package from the
;; current buffer.

;;;; Unpacking

;;;###autoload
(defvar package--activated nil
  "Non-nil if `package-activate-all' has been run.")

;;;###autoload
(progn ;; Make the function usable without loading `package.el'.
(defun package-activate-all ()
  "Activate all installed packages.
The variable `package-load-list' controls which packages to load."
  (setq package--activated t)
  (let* ((elc (concat package-quickstart-file "c"))
         (qs (if (file-readable-p elc) elc
               (if (file-readable-p package-quickstart-file)
                   package-quickstart-file))))
    ;; The quickstart file presumes that it has a blank slate,
    ;; so don't use it if we already activated some packages.
    (or (and qs (not (bound-and-true-p package-activated-list))
             ;; Skip `load-source-file-function' which would slow us down by
             ;; a factor 2 when loading the .el file (this assumes we were
             ;; careful to save this file so it doesn't need any decoding).
             (with-demoted-errors "Error during quickstart: %S"
               (let ((load-source-file-function nil))
                 (unless (boundp 'package-activated-list)
                   (setq package-activated-list nil))
                 (load qs nil 'nomessage)
                 t)))
        (progn
          (require 'package)
          ;; Silence the "unknown function" warning when this is compiled
          ;; inside `loaddefs.el'.
          ;; FIXME: We use `with-no-warnings' because the effect of
          ;; `declare-function' is currently not scoped, so if we use
          ;; it here, we end up with a redefinition warning instead :-)
          (with-no-warnings
            (package--activate-all)))))))

(defun package--activate-all ()
  (dolist (elt (package--alist))
    (condition-case err
        (package-activate (car elt))
      ;; Don't let failure of activation of a package arbitrarily stop
      ;; activation of further packages.
      (error (message "%s" (error-message-string err))))))

;;;; Inferring package from current buffer

(declare-function lm-package-version "lisp-mnt" (&optional file))

;;;###autoload
(defun package-installed-p (package &optional min-version)
  "Return non-nil if PACKAGE, of MIN-VERSION or newer, is installed.
If PACKAGE is a symbol, it is the package name and MIN-VERSION
should be a version list.

If PACKAGE is a `package-desc' object, MIN-VERSION is ignored."
  (cond
   ((package-desc-p package)
    (let ((dir (package-desc-dir package)))
        (and (stringp dir)
             (file-exists-p dir))))
   ((and (not (bound-and-true-p package--initialized))
         (null min-version)
         package-activated-list)
    ;; We used the quickstart: make it possible to use package-installed-p
    ;; even before package is fully initialized.
    (or
     (memq package package-activated-list)
     ;; Also check built-in packages.
     (package-built-in-p package min-version)))
   (t
    (or
     (let ((pkg-descs (cdr (assq package (package--alist)))))
       (and pkg-descs
            (version-list-<= min-version
                             (package-desc-version (car pkg-descs)))))
     ;; Also check built-in packages.
     (package-built-in-p package min-version)))))

;;;###autoload
(defun package-get-version ()
  "Return the version number of the package in which this is used.
Assumes it is used from an Elisp file placed inside the top-level directory
of an installed ELPA package.
The return value is a string (or nil in case we can't find it).
It works in more cases if the call is in the file which contains
the `Version:' header."
  ;; In a sense, this is a lie, but it does just what we want: precomputes
  ;; the version at compile time and hardcodes it into the .elc file!
  (declare (pure t))
  ;; Hack alert!
  (let ((file (or (macroexp-file-name) buffer-file-name)))
    (cond
     ((null file) nil)
     ;; Packages are normally installed into directories named "<pkg>-<vers>",
     ;; so get the version number from there.
     ((string-match "/[^/]+-\\([0-9]\\(?:[0-9.]\\|pre\\|beta\\|alpha\\|snapshot\\)+\\)/[^/]+\\'" file)
      (match-string 1 file))
     ;; For packages run straight from the an elpa.git clone, there's no
     ;; "-<vers>" in the directory name, so we have to fetch the version
     ;; the hard way.
     (t
      (let* ((pkgdir (file-name-directory file))
             (pkgname (file-name-nondirectory (directory-file-name pkgdir)))
             (mainfile (expand-file-name (concat pkgname ".el") pkgdir)))
        (unless (file-readable-p mainfile) (setq mainfile file))
        (when (file-readable-p mainfile)
          (require 'lisp-mnt)
          (lm-package-version mainfile)))))))

(provide 'package-activate)
;;; package-activate.el ends here
