;;; package-core.el --- Core of the Emacs Package Manager -*- lexical-binding:t -*-

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

;; This file contains the core definitions of package.el, including
;; package descriptors, general user options and the package activation
;; logic.

;;; Code:

(eval-and-compile (require 'cl-lib))
(eval-when-compile (require 'epg))      ;For setf accessors.
(eval-when-compile (require 'inline))   ;For `define-inline'

(defvar package--default-summary "No description available.")

(defvar package-list-unversioned nil
  "If non-nil, include packages that don't have a version in `list-packages'.")

(defvar package-list-unsigned nil
  "If non-nil, mention in the list which packages were installed without signature.")

(defvar package--emacs-version-list (version-to-list emacs-version)
  "The value of variable `emacs-version' as a list.")

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
        either `single' or `tar'.

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

(cl-defstruct (package--bi-desc
               (:constructor package-make-builtin (version summary))
               (:type vector))
  "Package descriptor format used in finder-inf.el and package--builtins."
  version
  reqs
  summary)

(defun package--from-builtin (bi-desc)
  "Create a `package-desc' object from BI-DESC.
BI-DESC should be a `package--bi-desc' object."
  (package-desc-create :name (pop bi-desc)
                       :version (package--bi-desc-version bi-desc)
                       :summary (package--bi-desc-summary bi-desc)
                       :dir 'builtin))

(defun package-desc--keywords (pkg-desc)
  "Return keywords of package-desc object PKG-DESC.
These keywords come from the foo-pkg.el file, and in general
corresponds to the keywords in the \"Keywords\" header of the
package."
  (let ((keywords (cdr (assoc :keywords (package-desc-extras pkg-desc)))))
    (if (eq (car-safe keywords) 'quote)
        (nth 1 keywords)
      keywords)))

(defun package--read-pkg-desc (kind)
  "Read a `define-package' form in current buffer.
Return the pkg-desc, with desc-kind set to KIND."
  (goto-char (point-min))
  (let* ((pkg-def-parsed (read (current-buffer)))
         (pkg-desc
          (when (eq (car pkg-def-parsed) 'define-package)
            (apply #'package-desc-from-define
                   (append (cdr pkg-def-parsed))))))
    (when pkg-desc
      (setf (package-desc-kind pkg-desc) kind)
      pkg-desc)))


;;; Customization options

(defgroup package nil
  "Manager for Emacs Lisp packages."
  :group 'applications
  :version "24.1")

;;;###autoload
(defcustom package-enable-at-startup t
  "Whether to make installed packages available when Emacs starts.
If non-nil, packages are made available before reading the init
file (but after reading the early init file).  This means that if
you wish to set this variable, you must do so in the early init
file.  Regardless of the value of this variable, packages are not
made available if `user-init-file' is nil (e.g. Emacs was started
with \"-q\").

Even if the value is nil, you can type \\[package-initialize] to
make installed packages available at any time, or you can
call (package-activate-all) in your init-file.

Note that this variable must be set to a non-default value in
your early-init file, as the variable's value is used before
loading the regular init file.  Therefore, if you customize it
via Customize, you should save your customized setting into
your `early-init-file'."
  :type 'boolean
  :version "24.1")

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
  :version "24.1")

(defcustom package-pinned-packages nil
  "An alist of packages that are pinned to specific archives.
This can be useful if you have multiple package archives enabled,
and want to control which archive a given package gets installed from.

Each element of the alist has the form (PACKAGE . ARCHIVE), where:
 PACKAGE is a symbol representing a package
 ARCHIVE is a string representing an archive (it should be the car of
an element in `package-archives', e.g. \"gnu\").

Adding an entry to this variable means that only ARCHIVE will be
considered as a source for PACKAGE.  If other archives provide PACKAGE,
they are ignored (for this package).  If ARCHIVE does not contain PACKAGE,
the package will be unavailable."
  :type '(alist :key-type (symbol :tag "Package")
                :value-type (string :tag "Archive name"))
  ;; This could prevent you from receiving updates for a package,
  ;; via an entry (PACKAGE . NON-EXISTING).  Which could be an issue
  ;; if PACKAGE has a known vulnerability that is fixed in newer versions.
  :risky t
  :version "24.4")

;;;###autoload
(defcustom package-user-dir (locate-user-emacs-file "elpa")
  "Directory containing the user's Emacs Lisp packages.
The directory name should be absolute.
Apart from this directory, Emacs also looks for system-wide
packages in `package-directory-list'."
  :type 'directory
  :initialize #'custom-initialize-delay
  :risky t
  :group 'package
  :version "24.1")

;;;###autoload
(defcustom package-directory-list
  ;; Defaults are subdirs named "elpa" in the site-lisp dirs.
  (let (result)
    (dolist (f load-path)
      (and (stringp f)
           (equal (file-name-nondirectory f) "site-lisp")
           (push (expand-file-name "elpa" f) result)))
    (nreverse result))
  "List of additional directories containing Emacs Lisp packages.
Each directory name should be absolute.

These directories contain packages intended for system-wide; in
contrast, `package-user-dir' contains packages for personal use."
  :type '(repeat directory)
  :initialize #'custom-initialize-delay
  :group 'package
  :risky t
  :version "24.1")

(defcustom package-selected-packages nil
  "Store here packages installed explicitly by user.
This variable is fed automatically by Emacs when installing a new package.
This variable is used by `package-autoremove' to decide
which packages are no longer needed.
You can use it to (re)install packages on other machines
by running `package-install-selected-packages'.

To check if a package is contained in this list here, use
`package--user-selected-p', as it may populate the variable with
a sane initial value."
  :version "25.1"
  :type '(repeat symbol))

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

(defun package-versioned-builtin-packages ()
  "Return a list of all the versioned built-in packages.
The return value is a list of names of built-in packages represented as
symbols."
  (mapcar #'car package--builtin-versions))

(defun package-builtin-package-version (package)
  "Return the version of a built-in PACKAGE given by its symbol.
The return value is a list of integers representing the version of
PACKAGE, in the format returned by `version-to-list', or nil if the
package is built-in but has no version or is not a built-in package."
  (alist-get package package--builtin-versions))

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

(defun package--active-built-in-p (package)
  "Return non-nil if the built-in version of PACKAGE is used.
If the built-in version of PACKAGE is used and PACKAGE is
also available for installation from an archive, it is an
indication that PACKAGE was never upgraded to any newer
version from the archive."
  (and (not (assq (cond
                   ((package-desc-p package)
                    (package-desc-name package))
                   ((stringp package) (intern package))
                   ((symbolp package) package)
                   ((error "Unknown package format: %S" package)))
                  (package--alist)))
       (package-built-in-p package)))

(defun package--autoloads-file-name (pkg-desc)
  "Return the absolute name of the autoloads file, sans extension.
PKG-DESC is a `package-desc' object."
  (expand-file-name
   (format "%s-autoloads" (package-desc-name pkg-desc))
   (package-desc-dir pkg-desc)))

(defvar Info-directory-list)
(declare-function info-initialize "info" ())

(defvar package--quickstart-pkgs t
  "If set to a list, we're computing the set of pkgs to activate.")

(defsubst package--library-stem (file)
  (catch 'done
    (let (result)
      (dolist (suffix (get-load-suffixes) file)
        (setq result (string-trim file nil suffix))
        (unless (equal file result)
          (throw 'done result))))))

(defun package--reload-previously-loaded (pkg-desc &optional warn)
  "Force reimportation of files in PKG-DESC already present in `load-history'.
New editions of files contain macro definitions and
redefinitions, the overlooking of which would cause
byte-compilation of the new package to fail.
If WARN is a string, display a warning (using WARN as a format string)
before reloading the files.  WARN must have two %-sequences
corresponding to package name (a symbol) and a list of files loaded (as
sexps)."
  (with-demoted-errors "Error in package--load-files-for-activation: %s"
    (let* (result
           (dir (package-desc-dir pkg-desc))
           ;; A previous implementation would skip `dir' itself.
           ;; However, in normal use reloading from the same directory
           ;; never happens anyway, while in certain cases external to
           ;; Emacs a package in the same directory not necessary
           ;; stays byte-identical, e.g.  during development.  Just
           ;; don't special-case `dir'.
           (effective-path (or (bound-and-true-p find-library-source-path)
                               load-path))
           (files (directory-files-recursively dir "\\`[^\\.].*\\.el\\'"))
           (history (mapcar #'file-truename
                            (cl-remove-if-not #'stringp
                                              (mapcar #'car load-history)))))
      (dolist (file files)
        (when-let* ((library (package--library-stem
                              (file-relative-name file dir)))
                    (canonical (locate-library library nil effective-path))
                    (truename (file-truename canonical))
                    ;; Normally, all files in a package are compiled by
                    ;; now, but don't assume that.  E.g. different
                    ;; versions can add or remove `no-byte-compile'.
                    (altname (if (string-suffix-p ".el" truename)
                                 (replace-regexp-in-string
                                  "\\.el\\'" ".elc" truename t)
                               (replace-regexp-in-string
                                "\\.elc\\'" ".el" truename t)))
                    (found (or (member truename history)
                               (and (not (string= altname truename))
                                    (member altname history))))
                    (recent-index (length found)))
          (unless (equal (file-name-base library)
                         (format "%s-autoloads" (package-desc-name pkg-desc)))
            (push (cons (expand-file-name library dir) recent-index) result))))
      (when (and result warn)
        (display-warning 'package
                         (format warn (package-desc-name pkg-desc)
                                 (mapcar #'car result))))
      (mapc (lambda (c) (load (car c) nil t))
            (sort result (lambda (x y) (< (cdr x) (cdr y))))))))

(defun package-desc-full-name (pkg-desc)
  "Return full name of package-desc object PKG-DESC.
This is the name of the package with its version appended."
  (if (package-vc-p pkg-desc)
      (symbol-name (package-desc-name pkg-desc))
    (format "%s-%s"
            (package-desc-name pkg-desc)
            (package-version-join (package-desc-version pkg-desc)))))

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
          (package--reload-previously-loaded
           pkg-desc (unless reload
                      "Package %S is activated too late.
The following files have already been loaded: %S")))
        (with-demoted-errors "Error loading autoloads: %s"
          (load (package--autoloads-file-name pkg-desc) nil t)))
      ;; Add info node.
      (when (file-exists-p (expand-file-name "dir" pkg-dir))
        ;; FIXME: not the friendliest, but simple.
        (require 'info)
        (info-initialize)
        (add-to-list 'Info-directory-list pkg-dir))
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

(defvar package--initialized nil
  "Non-nil if `package-initialize' has been run.")

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

(declare-function package-read-all-archive-contents "package-elpa" ())

(defvar package--compatibility-table nil
  "Hash table connecting package names to their compatibility.
Each key is a symbol, the name of a package.

The value is either nil, representing an incompatible package, or
a version list, representing the highest compatible version of
that package which is available.

A package is considered incompatible if it requires an Emacs
version higher than the one being used.  To check for package
\(in)compatibility, don't read this table directly, use
`package--incompatible-p' which also checks dependencies.")

(defun package--add-to-compatibility-table (pkg)
  "If PKG is compatible (without dependencies), add to the compatibility table.
PKG is a package-desc object.
Only adds if its version is higher than what's already stored in
the table."
  (unless (package--incompatible-p pkg 'shallow)
    (let* ((name (package-desc-name pkg))
           (version (or (package-desc-version pkg) '(0)))
           (table-version (gethash name package--compatibility-table)))
      (when (or (not table-version)
                (version-list-< table-version version))
        (puthash name version package--compatibility-table)))))

(defun package--mapc (function &optional packages)
  "Call FUNCTION for all known PACKAGES.
PACKAGES can be nil or t, which means to display all known
packages, or a list of packages.

Built-in packages are converted with `package--from-builtin'."
  (unless packages (setq packages t))
  (let (name)
    ;; Installed packages:
    (dolist (elt package-alist)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (mapc function (cdr elt))))

    ;; Built-in packages:
    (dolist (elt package--builtins)
      (setq name (car elt))
      (when (and (not (eq name 'emacs)) ; Hide the `emacs' package.
                 (or package-list-unversioned
                     (package--bi-desc-version (cdr elt)))
                 (or (eq packages t) (memq name packages)))
        (funcall function (package--from-builtin elt))))

    ;; Available and disabled packages:
    (dolist (elt (bound-and-true-p package-archive-contents))
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (dolist (pkg (cdr elt))
          ;; Hide obsolete packages.
          (unless (package-installed-p (package-desc-name pkg)
                                       (package-desc-version pkg))
        (funcall function pkg)))))))

(defun package--build-compatibility-table ()
  "Build `package--compatibility-table' with `package--mapc'."
  ;; Initialize the list of built-ins.
  (require 'finder-inf nil t)
  ;; Build compat table.
  (setq package--compatibility-table (make-hash-table :test 'eq))
  (package--mapc #'package--add-to-compatibility-table))

;;;###autoload
(defun package-initialize (&optional no-activate)
  "Load Emacs Lisp packages, and activate them.
The variable `package-load-list' controls which packages to load.
If optional arg NO-ACTIVATE is non-nil, don't activate packages.

It is not necessary to adjust `load-path' or `require' the
individual packages after calling `package-initialize' -- this is
taken care of by `package-initialize'.

If `package-initialize' is called twice during Emacs startup,
signal a warning, since this is a bad idea except in highly
advanced use cases.  To suppress the warning, remove the
superfluous call to `package-initialize' from your init-file.  If
you have code which must run before `package-initialize', put
that code in the early init-file."
  (interactive)
  (when (and package--initialized (not after-init-time))
    (lwarn '(package reinitialization) :warning
           "Unnecessary call to `package-initialize' in init file"))
  (setq package-alist nil)
  (package-load-all-descriptors)
  (require 'package)
  (package-read-all-archive-contents)
  (setq package--initialized t)
  (unless no-activate
    (package-activate-all))
  ;; This uses `package--mapc' so it must be called after
  ;; `package--initialized' is t.
  (package--build-compatibility-table))

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

(defun package-strip-rcs-id (str)
  "Strip RCS version ID from the version string STR.
If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (when str
    (when (string-match "\\`[ \t]*[$]Revision:[ \t]+" str)
      (setq str (substring str (match-end 0))))
    (let ((l (version-to-list str)))
      ;; Don't return `str' but (package-version-join (version-to-list str))
      ;; to make sure we use a "canonical name"!
      (if l (package-version-join l)))))

(defun package--incompatible-p (pkg &optional shallow)
  "Return non-nil if PKG has no chance of being installable.
PKG is a `package-desc' object.

If SHALLOW is non-nil, this only checks if PKG depends on a
higher `emacs-version' than the one being used.  Otherwise, also
checks the viability of dependencies, according to
`package--compatibility-table'.

If PKG requires an incompatible Emacs version, the return value
is this version (as a string).
If PKG requires incompatible packages, the return value is a list
of these dependencies, similar to the list returned by
`package-desc-reqs'."
  (let* ((reqs    (package-desc-reqs pkg))
         (version (cadr (assq 'emacs reqs))))
    (if (and version (version-list-< package--emacs-version-list version))
        (package-version-join version)
      (unless shallow
        (let (out)
          (dolist (dep (package-desc-reqs pkg) out)
            (let ((dep-name (car dep)))
              (unless (eq 'emacs dep-name)
                (let ((cv (gethash dep-name package--compatibility-table)))
                  (when (version-list-< (or cv '(0)) (or (cadr dep) '(0)))
                    (push dep out)))))))))))

(defun package--find-non-dependencies ()
  "Return a list of installed packages which are not dependencies.
Finds all packages in `package-alist' which are not dependencies
of any other packages.
Used to populate `package-selected-packages'."
  (let ((dep-list
         (delete-dups
          (apply #'append
            (mapcar (lambda (p) (mapcar #'car (package-desc-reqs (cadr p))))
                    package-alist)))))
    (cl-loop for p in package-alist
             for name = (car p)
             unless (memq name dep-list)
             collect name)))

(defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when (or value after-init-time)
    ;; It is valid to set it to nil, for example when the last package
    ;; is uninstalled.  But it shouldn't be done at init time, to
    ;; avoid overwriting configurations that haven't yet been loaded.
    (setq package-selected-packages (sort value #'string<)))
  (if after-init-time
      (customize-save-variable 'package-selected-packages package-selected-packages)
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(defun package--user-selected-p (pkg)
  "Return non-nil if PKG is a package was installed by the user.
PKG is a package name.
This looks into `package-selected-packages', populating it first
if it is still empty."
  (unless (consp package-selected-packages)
    (package--save-selected-packages (package--find-non-dependencies)))
  (memq pkg package-selected-packages))

(defun package-desc-status (pkg-desc)
  "Return the status of `package-desc' object PKG-DESC."
  (let* ((name (package-desc-name pkg-desc))
         (dir (package-desc-dir pkg-desc))
         (lle (assq name package-load-list))
         (held (cadr lle))
         (version (package-desc-version pkg-desc))
         (signed (or (not package-list-unsigned)
                     (package-desc-signed pkg-desc))))
    (cond
     ((package-vc-p pkg-desc) "source")
     ((eq dir 'builtin) "built-in")
     ((and lle (null held)) "disabled")
     ((stringp held)
      (let ((hv (if (stringp held) (version-to-list held))))
        (cond
         ((version-list-= version hv) "held")
         ((version-list-< version hv) "obsolete")
         (t "disabled"))))
     (dir                               ;One of the installed packages.
      (cond
       ((not (file-exists-p dir)) "deleted")
       ;; Not inside `package-user-dir'.
       ((not (file-in-directory-p dir package-user-dir)) "external")
       ((eq pkg-desc (cadr (assq name package-alist)))
        (if (not signed) "unsigned"
          (if (package--user-selected-p name)
              "installed" "dependency")))
       (t "obsolete")))
     ((package--incompatible-p pkg-desc) "incompat")
     (t
      (let* ((ins (cadr (assq name package-alist)))
             (ins-v (if ins (package-desc-version ins))))
        (cond
         ;; Installed obsolete packages are handled in the `dir'
         ;; clause above.  Here we handle available obsolete, which
         ;; are displayed depending on `package-menu--hide-packages'.
         ((and ins (version-list-<= version ins-v)) "avail-obso")
         (t
          (if (memq name (bound-and-true-p package-menu--new-package-list))
              "new" "available"))))))))

(defun package--print-email-button (recipient)
  "Insert a button whose action will send an email to RECIPIENT.
NAME should have the form (FULLNAME . EMAIL) where FULLNAME is
either a full name or nil, and EMAIL is a valid email address."
  (when (car recipient)
    (insert (car recipient)))
  (when (and (car recipient) (cdr recipient))
    (insert " "))
  (when (cdr recipient)
    (insert "<")
    (insert-text-button (cdr recipient)
                        'follow-link t
                        'action (lambda (_)
                                  (compose-mail
                                   (format "%s <%s>" (car recipient) (cdr recipient)))))
    (insert ">"))
  (insert "\n"))

(defun package-maintainers (pkg-desc &optional no-error)
  "Return an email address for the maintainers of PKG-DESC.
The email address may contain commas, if there are multiple
maintainers.  If no maintainers are found, an error will be
signaled.  If the optional argument NO-ERROR is non-nil no error
will be signaled in that case."
  (unless (package-desc-p pkg-desc)
    (error "Invalid package description: %S" pkg-desc))
  (let* ((name (package-desc-name pkg-desc))
         (extras (package-desc-extras pkg-desc))
         (maint (alist-get :maintainer extras)))
    (unless (listp (cdr maint))
      (setq maint (list maint)))
    (cond
     ((and (null maint) (null no-error))
      (user-error "Package `%s' has no explicit maintainer" name))
     ((and (not (progn
                  (require 'ietf-drums)
                  (ietf-drums-parse-address (cdar maint))))
           (null no-error))
      (user-error "Package `%s' has no maintainer address" name))
     (t
      (with-temp-buffer
        (mapc #'package--print-email-button maint)
        (replace-regexp-in-string
         "\n" ", " (string-trim
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))))

(declare-function ietf-drums-parse-address "ietf-drums"
                  (string &optional decode))

;;;###autoload
(defun package-report-bug (desc)
  "Prepare a message to send to the maintainers of a package.
DESC must be a `package-desc' object.

Of interest to package maintainers: By default, the command will use
`reporter-submit-bug-report' to generate a message buffer.  If your
package has specific needs, you can set the symbol property
`package-report-bug-function' of the symbol designating your package
name.
"
  (interactive (list (package--query-desc package-alist))
               package-menu-mode)
  (let ((maint (package-maintainers desc))
        (name (symbol-name (package-desc-name desc)))
        (pkgdir (package-desc-dir desc))
        vars)
    (when pkgdir
      (dolist-with-progress-reporter (group custom-current-group-alist)
          "Scanning for modified user options..."
        (when (and (car group)
                   (file-in-directory-p (car group) pkgdir))
          (dolist (ent (get (cdr group) 'custom-group))
            (when (and (custom-variable-p (car ent))
                       (boundp (car ent))
                       (not (eq (custom--standard-value (car ent))
                                (default-toplevel-value (car ent)))))
              (push (car ent) vars))))))
    (dlet ((reporter-prompt-for-summary-p t))
      (funcall (or (get name 'package-report-bug-function)
                   #'reporter-submit-bug-report)
               maint name vars))))

;;;; Inferring package from current buffer
(defun package-read-from-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (pcase-let ((`(,expr . ,offset) (read-from-string str)))
    (condition-case ()
        ;; The call to `ignore' suppresses a compiler warning.
        (progn (ignore (read-from-string str offset))
               (error "Can't read whole string"))
      (end-of-file expr))))

(defun package--query-desc (&optional alist)
  "Query the user for a package or return the package at point.
The optional argument ALIST must consist of elements with the
form (PKG-NAME PKG-DESC).  If not specified, it will default to
`package-alist'."
  (or (and (fboundp 'tabulated-list-get-id)
           (tabulated-list-get-id))
      (let ((alist (or alist package-alist)))
        (cadr (assoc (completing-read "Package: " alist nil t)
                     alist #'string=)))))

(defun package--alist-to-plist-args (alist)
  (mapcar #'macroexp-quote
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(provide 'package-core)
;;; package-core.el ends here
