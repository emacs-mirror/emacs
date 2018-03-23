;;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2007-2018 Free Software Foundation, Inc.

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
;; format is either  a tar file or a single .el file.

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
;;    then "x" to execute) or deletion (not implemented yet), and you
;;    can see what packages are available.  This will automatically
;;    fetch the latest list of packages from ELPA.
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
;;    formatted as with package-install-from-buffer.

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
;; - maybe we need separate .elc directories for various emacs versions
;;   and also emacs-vs-xemacs.  That way conditional compilation can
;;   work.  But would this break anything?
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

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'epg))      ;For setf accessors.
(require 'seq)

(require 'tabulated-list)
(require 'macroexp)
(require 'url-handlers)

(defgroup package nil
  "Manager for Emacs Lisp packages."
  :group 'applications
  :version "24.1")


;;; Customization options
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
call (package-initialize) in your init-file."
  :type 'boolean
  :version "24.1")

(defcustom package-load-list '(all)
  "List of packages for `package-initialize' to make available.
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

(defcustom package-archives `(("gnu" .
                               ,(format "http%s://elpa.gnu.org/packages/"
                                        (if (gnutls-available-p) "s" ""))))
  "An alist of archives from which to fetch.
The default value points to the GNU Emacs package repository.

Each element has the form (ID . LOCATION).
 ID is an archive name, as a string.
 LOCATION specifies the base location for the archive.
  If it starts with \"http(s):\", it is treated as an HTTP(S) URL;
  otherwise it should be an absolute directory name.
  (Other types of URL are currently not supported.)

Only add locations that you trust, since fetching and installing
a package can run arbitrary code."
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (string :tag "URL or directory name"))
  :risky t
  :version "26.1")                      ; gnutls test

(defcustom package-menu-hide-low-priority 'archive
  "If non-nil, hide low priority packages from the packages menu.
A package is considered low priority if there's another version
of it available such that:
    (a) the archive of the other package is higher priority than
    this one, as per `package-archive-priorities';
  or
    (b) they both have the same archive priority but the other
    package has a higher version number.

This variable has three possible values:
    nil: no packages are hidden;
    `archive': only criterion (a) is used;
    t: both criteria are used.

This variable has no effect if `package-menu--hide-packages' is
nil, so it can be toggled with \\<package-menu-mode-map> \\[package-menu-toggle-hiding]."
  :type '(choice (const :tag "Don't hide anything" nil)
                 (const :tag "Hide per package-archive-priorities"
                        archive)
                 (const :tag "Hide per archive and version number" t))
  :version "25.1")

(defcustom package-archive-priorities nil
  "An alist of priorities for packages.

Each element has the form (ARCHIVE-ID . PRIORITY).

When installing packages, the package with the highest version
number from the archive with the highest priority is
selected.  When higher versions are available from archives with
lower priorities, the user has to select those manually.

Archives not in this list have the priority 0.

See also `package-menu-hide-low-priority'."
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (integer :tag "Priority (default is 0)"))
  :risky t
  :version "25.1")

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
  ;; I don't really see why this is risky...
  ;; I suppose it could prevent you receiving updates for a package,
  ;; via an entry (PACKAGE . NON-EXISTING).  Which could be an issue
  ;; if PACKAGE has a known vulnerability that is fixed in newer versions.
  :risky t
  :version "24.4")

(defcustom package-user-dir (locate-user-emacs-file "elpa")
  "Directory containing the user's Emacs Lisp packages.
The directory name should be absolute.
Apart from this directory, Emacs also looks for system-wide
packages in `package-directory-list'."
  :type 'directory
  :risky t
  :version "24.1")

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
  :risky t
  :version "24.1")

(declare-function epg-find-configuration "epg-config"
                  (protocol &optional no-cache program-alist))

(defcustom package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
  "Directory containing GnuPG keyring or nil.
This variable specifies the GnuPG home directory used by package.
That directory is passed via the option \"--homedir\" to GnuPG.
If nil, do not use the option \"--homedir\", but stick with GnuPG's
default directory."
  :type `(choice
          (const
           :tag "Default Emacs package management GnuPG home directory"
           ,(expand-file-name "gnupg" package-user-dir))
          (const
           :tag "Default GnuPG directory (GnuPG option --homedir not used)"
           nil)
          (directory :tag "A specific GnuPG --homedir"))
  :risky t
  :version "26.1")

(defcustom package-check-signature
  (if (and (require 'epg-config)
           (epg-find-configuration 'OpenPGP))
      'allow-unsigned)
  "Non-nil means to check package signatures when installing.
The value `allow-unsigned' means to still install a package even if
it is unsigned.

This also applies to the \"archive-contents\" file that lists the
contents of the archive."
  :type '(choice (const nil :tag "Never")
                 (const allow-unsigned :tag "Allow unsigned")
                 (const t :tag "Check always"))
  :risky t
  :version "24.4")

(defcustom package-unsigned-archives nil
  "List of archives where we do not check for package signatures."
  :type '(repeat (string :tag "Archive name"))
  :risky t
  :version "24.4")

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

(defcustom package-menu-async t
  "If non-nil, package-menu will use async operations when possible.
Currently, only the refreshing of archive contents supports
asynchronous operations.  Package transactions are still done
synchronously."
  :type 'boolean
  :version "25.1")


;;; `package-desc' object definition
;; This is the struct used internally to represent packages.
;; Functions that deal with packages should generally take this object
;; as an argument.  In some situations (e.g. commands that query the
;; user) it makes sense to take the package name as a symbol instead,
;; but keep in mind there could be multiple `package-desc's with the
;; same name.
(defvar package--default-summary "No description available.")

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
                 (version (version-to-list version-string))
                 (reqs (mapcar #'(lambda (elt)
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

`reqs'	Requirements of the package. A list of (PACKAGE
        VERSION-LIST) naming the dependent package and the minimum
        required version.

`kind'	The distribution format of the package. Currently, it is
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

(defun package--from-builtin (bi-desc)
  (package-desc-create :name (pop bi-desc)
                       :version (package--bi-desc-version bi-desc)
                       :summary (package--bi-desc-summary bi-desc)
                       :dir 'builtin))

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
      (apply 'concat (nreverse str-list)))))

(defun package-desc-full-name (pkg-desc)
  (format "%s-%s"
          (package-desc-name pkg-desc)
          (package-version-join (package-desc-version pkg-desc))))

(defun package-desc-suffix (pkg-desc)
  (pcase (package-desc-kind pkg-desc)
    (`single ".el")
    (`tar ".tar")
    (`dir "")
    (kind (error "Unknown package kind: %s" kind))))

(defun package-desc--keywords (pkg-desc)
  (let ((keywords (cdr (assoc :keywords (package-desc-extras pkg-desc)))))
    (if (eq (car-safe keywords) 'quote)
        (nth 1 keywords)
      keywords)))

(defun package-desc-priority (p)
  "Return the priority of the archive of package-desc object P."
  (package-archive-priority (package-desc-archive p)))

;; Package descriptor format used in finder-inf.el and package--builtins.
(cl-defstruct (package--bi-desc
               (:constructor package-make-builtin (version summary))
               (:type vector))
  version
  reqs
  summary)


;;; Installed packages
;; The following variables store information about packages present in
;; the system.  The most important of these is `package-alist'.  The
;; command `package-initialize' is also closely related to this
;; section, but it is left for a later section because it also affects
;; other stuff.
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
name (a symbol) and DESCS is a non-empty list of `package-desc' structure,
sorted by decreasing versions.

This variable is set automatically by `package-load-descriptor',
called via `package-initialize'.  To change which packages are
loaded and/or activated, customize `package-load-list'.")
(put 'package-alist 'risky-local-variable t)

(defvar package-activated-list nil
  ;; FIXME: This should implicitly include all builtin packages.
  "List of the names of currently activated packages.")
(put 'package-activated-list 'risky-local-variable t)

;;;; Populating `package-alist'.
;; The following functions are called on each installed package by
;; `package-load-all-descriptors', which ultimately populates the
;; `package-alist' variable.
(defun package-process-define-package (exp)
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
  "Load the description file in directory PKG-DIR."
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
      (dolist (subdir (directory-files dir))
        (unless (equal subdir "..")
          (let ((pkg-dir (expand-file-name subdir dir)))
            (when (file-directory-p pkg-dir)
              (package-load-descriptor pkg-dir))))))))

(defun define-package (_name-string _version-string
                                    &optional _docstring _requirements
                                    &rest _extra-properties)
  "Define a new package.
NAME-STRING is the name of the package, as a string.
VERSION-STRING is the version of the package, as a string.
DOCSTRING is a short description of the package, a string.
REQUIREMENTS is a list of dependencies on other packages.
 Each requirement is of the form (OTHER-PACKAGE OTHER-VERSION),
 where OTHER-VERSION is a string.

EXTRA-PROPERTIES is currently unused."
  ;; FIXME: Placeholder!  Should we keep it?
  (error "Don't call me!"))


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

(defun package--activate-autoloads-and-load-path (pkg-desc)
  "Load the autoloads file and add package dir to `load-path'.
PKG-DESC is a `package-desc' object."
  (let* ((old-lp load-path)
         (pkg-dir (package-desc-dir pkg-desc))
         (pkg-dir-dir (file-name-as-directory pkg-dir)))
    (with-demoted-errors "Error loading autoloads: %s"
      (load (package--autoloads-file-name pkg-desc) nil t))
    (when (and (eq old-lp load-path)
               (not (or (member pkg-dir load-path)
                        (member pkg-dir-dir load-path))))
      ;; Old packages don't add themselves to the `load-path', so we have to
      ;; do it ourselves.
      (push pkg-dir load-path))))

(defvar Info-directory-list)
(declare-function info-initialize "info" ())

(defun package--load-files-for-activation (pkg-desc reload)
  "Load files for activating a package given by PKG-DESC.
Load the autoloads file, and ensure `load-path' is setup.  If
RELOAD is non-nil, also load all files in the package that
correspond to previously loaded files."
  (let* ((loaded-files-list (when reload
                              (package--list-loaded-files (package-desc-dir pkg-desc)))))
    ;; Add to load path, add autoloads, and activate the package.
    (package--activate-autoloads-and-load-path pkg-desc)
    ;; Call `load' on all files in `package-desc-dir' already present in
    ;; `load-history'.  This is done so that macros in these files are updated
    ;; to their new definitions.  If another package is being installed which
    ;; depends on this new definition, not doing this update would cause
    ;; compilation errors and break the installation.
    (with-demoted-errors "Error in package--load-files-for-activation: %s"
      (mapc (lambda (feature) (load feature nil t))
            ;; Skip autoloads file since we already evaluated it above.
            (remove (file-truename (package--autoloads-file-name pkg-desc))
                    loaded-files-list)))))

(defun package-activate-1 (pkg-desc &optional reload deps)
  "Activate package given by PKG-DESC, even if it was already active.
If DEPS is non-nil, also activate its dependencies (unless they
are already activated).
If RELOAD is non-nil, also `load' any files inside the package which
correspond to previously loaded files (those returned by
`package--list-loaded-files')."
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
      (package--load-files-for-activation pkg-desc reload)
      ;; Add info node.
      (when (file-exists-p (expand-file-name "dir" pkg-dir))
        ;; FIXME: not the friendliest, but simple.
        (require 'info)
        (info-initialize)
        (push pkg-dir Info-directory-list))
      (push name package-activated-list)
      ;; Don't return nil.
      t)))

(declare-function find-library-name "find-func" (library))

(defun package--list-loaded-files (dir)
  "Recursively list all files in DIR which correspond to loaded features.
Returns the `file-name-sans-extension' of each file, relative to
DIR, sorted by most recently loaded last."
  (let* ((history (delq nil
                        (mapcar (lambda (x)
                                  (let ((f (car x)))
                                    (and f (file-name-sans-extension f))))
                                load-history)))
         (dir (file-truename dir))
         ;; List all files that have already been loaded.
         (list-of-conflicts
          (delq
           nil
           (mapcar
               (lambda (x) (let* ((file (file-relative-name x dir))
                             ;; Previously loaded file, if any.
                             (previous
                              (ignore-errors
                                (file-name-sans-extension
                                 (file-truename (find-library-name file)))))
                             (pos (when previous (member previous history))))
                        ;; Return (RELATIVE-FILENAME . HISTORY-POSITION)
                        (when pos
                          (cons (file-name-sans-extension file) (length pos)))))
             (directory-files-recursively dir "\\`[^\\.].*\\.el\\'")))))
    ;; Turn the list of (FILENAME . POS) back into a list of features.  Files in
    ;; subdirectories are returned relative to DIR (so not actually features).
    (let ((default-directory (file-name-as-directory dir)))
      (mapcar (lambda (x) (file-truename (car x)))
        (sort list-of-conflicts
              ;; Sort the files by ascending HISTORY-POSITION.
              (lambda (x y) (< (cdr x) (cdr y))))))))

;;;; `package-activate'
;; This function activates a newer version of a package if an older
;; one was already activated.  It also loads a features of this
;; package which were already loaded.
(defun package-activate (package &optional force)
  "Activate the package named PACKAGE.
If FORCE is true, (re-)activate it if it's already activated.
Newer versions are always activated, regardless of FORCE."
  (let ((pkg-descs (cdr (assq package package-alist))))
    ;; Check if PACKAGE is available in `package-alist'.
    (while
        (when pkg-descs
          (let ((available-version (package-desc-version (car pkg-descs))))
            (or (package-disabled-p package available-version)
                ;; Prefer a builtin package.
                (package-built-in-p package available-version))))
      (setq pkg-descs (cdr pkg-descs)))
    (cond
     ;; If no such package is found, maybe it's built-in.
     ((null pkg-descs)
      (package-built-in-p package))
     ;; If the package is already activated, just return t.
     ((and (memq package package-activated-list) (not force))
      t)
     ;; Otherwise, proceed with activation.
     (t (package-activate-1 (car pkg-descs) nil 'deps)))))


;;; Installation -- Local operations
;; This section contains a variety of features regarding installing a
;; package to/from disk.  This includes autoload generation,
;; unpacking, compiling, as well as defining a package from the
;; current buffer.

;;;; Unpacking
(defvar tar-parse-info)
(declare-function tar-untar-buffer "tar-mode" ())
(declare-function tar-header-name "tar-mode" (tar-header) t)
(declare-function tar-header-link-type "tar-mode" (tar-header) t)

(defun package-untar-buffer (dir)
  "Untar the current buffer.
This uses `tar-untar-buffer' from Tar mode.  All files should
untar into a directory named DIR; otherwise, signal an error."
  (require 'tar-mode)
  (tar-mode)
  ;; Make sure everything extracts into DIR.
  (let ((regexp (concat "\\`" (regexp-quote (expand-file-name dir)) "/"))
        (case-fold-search (file-name-case-insensitive-p dir)))
    (dolist (tar-data tar-parse-info)
      (let ((name (expand-file-name (tar-header-name tar-data))))
        (or (string-match regexp name)
            ;; Tarballs created by some utilities don't list
            ;; directories with a trailing slash (Bug#13136).
            (and (string-equal dir name)
                 (eq (tar-header-link-type tar-data) 5))
            (error "Package does not untar cleanly into directory %s/" dir)))))
  (tar-untar-buffer))

(defun package--alist-to-plist-args (alist)
  (mapcar 'macroexp-quote
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))
(defun package-unpack (pkg-desc)
  "Install the contents of the current buffer as a package."
  (let* ((name (package-desc-name pkg-desc))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (expand-file-name dirname package-user-dir)))
    (pcase (package-desc-kind pkg-desc)
      (`dir
       (make-directory pkg-dir t)
       (let ((file-list
              (directory-files
               default-directory 'full "\\`[^.].*\\.el\\'" 'nosort)))
         (dolist (source-file file-list)
           (let ((target-el-file
                  (expand-file-name (file-name-nondirectory source-file) pkg-dir)))
             (copy-file source-file target-el-file t)))
         ;; Now that the files have been installed, this package is
         ;; indistinguishable from a `tar' or a `single'. Let's make
         ;; things simple by ensuring we're one of them.
         (setf (package-desc-kind pkg-desc)
               (if (> (length file-list) 1) 'tar 'single))))
      (`tar
       (make-directory package-user-dir t)
       ;; FIXME: should we delete PKG-DIR if it exists?
       (let* ((default-directory (file-name-as-directory package-user-dir)))
         (package-untar-buffer dirname)))
      (`single
       (let ((el-file (expand-file-name (format "%s.el" name) pkg-dir)))
         (make-directory pkg-dir t)
         (package--write-file-no-coding el-file)))
      (kind (error "Unknown package kind: %S" kind)))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)
    ;; Update package-alist.
    (let ((new-desc (package-load-descriptor pkg-dir)))
      (unless (equal (package-desc-full-name new-desc)
                     (package-desc-full-name pkg-desc))
        (error "The retrieved package (`%s') doesn't match what the archive offered (`%s')"
               (package-desc-full-name new-desc) (package-desc-full-name pkg-desc)))
      ;; Activation has to be done before compilation, so that if we're
      ;; upgrading and macros have changed we load the new definitions
      ;; before compiling.
      (when (package-activate-1 new-desc :reload :deps)
        ;; FIXME: Compilation should be done as a separate, optional, step.
        ;; E.g. for multi-package installs, we should first install all packages
        ;; and then compile them.
        (package--compile new-desc)
        ;; After compilation, load again any files loaded by
        ;; `activate-1', so that we use the byte-compiled definitions.
        (package--load-files-for-activation new-desc :reload)))
    pkg-dir))

(defun package-generate-description-file (pkg-desc pkg-file)
  "Create the foo-pkg.el file for single-file packages."
  (let* ((name (package-desc-name pkg-desc)))
    (let ((print-level nil)
          (print-quoted t)
          (print-length nil))
      (write-region
       (concat
        ";;; -*- no-byte-compile: t -*-\n"
        (prin1-to-string
         (nconc
          (list 'define-package
                (symbol-name name)
                (package-version-join (package-desc-version pkg-desc))
                (package-desc-summary pkg-desc)
                (let ((requires (package-desc-reqs pkg-desc)))
                  (list 'quote
                        ;; Turn version lists into string form.
                        (mapcar
                         (lambda (elt)
                           (list (car elt)
                                 (package-version-join (cadr elt))))
                         requires))))
          (package--alist-to-plist-args
           (package-desc-extras pkg-desc))))
        "\n")
       nil pkg-file nil 'silent))))

;;;; Autoload
(declare-function autoload-rubric "autoload" (file &optional type feature))

(defun package-autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (require 'autoload)
    (write-region (autoload-rubric file "package" nil) nil file nil 'silent))
  file)

(defvar generated-autoload-file)
(defvar autoload-timestamps)
(defvar version-control)

(defun package-generate-autoloads (name pkg-dir)
  (let* ((auto-name (format "%s-autoloads.el" name))
         ;;(ignore-name (concat name "-pkg.el"))
         (generated-autoload-file (expand-file-name auto-name pkg-dir))
         ;; We don't need 'em, and this makes the output reproducible.
         (autoload-timestamps nil)
         ;; Silence `autoload-generate-file-autoloads'.
         (noninteractive inhibit-message)
         (backup-inhibited t)
         (version-control 'never))
    (package-autoload-ensure-default-file generated-autoload-file)
    (update-directory-autoloads pkg-dir)
    (let ((buf (find-buffer-visiting generated-autoload-file)))
      (when buf (kill-buffer buf)))
    auto-name))

(defun package--make-autoloads-and-stuff (pkg-desc pkg-dir)
  "Generate autoloads, description file, etc.. for PKG-DESC installed at PKG-DIR."
  (package-generate-autoloads (package-desc-name pkg-desc) pkg-dir)
  (let ((desc-file (expand-file-name (package--description-file pkg-dir)
                                     pkg-dir)))
    (unless (file-exists-p desc-file)
      (package-generate-description-file pkg-desc desc-file)))
  ;; FIXME: Create foo.info and dir file from foo.texi?
  )

;;;; Compilation
(defvar warning-minimum-level)
(defun package--compile (pkg-desc)
  "Byte-compile installed package PKG-DESC.
This assumes that `pkg-desc' has already been activated with
`package-activate-1'."
  (let ((warning-minimum-level :error)
        (save-silently inhibit-message)
        (load-path load-path))
    (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))

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

(defun package--prepare-dependencies (deps)
  "Turn DEPS into an acceptable list of dependencies.

Any parts missing a version string get a default version string
of \"0\" (meaning any version) and an appropriate level of lists
is wrapped around any parts requiring it."
  (cond
   ((not (listp deps))
    (error "Invalid requirement specifier: %S" deps))
   (t (mapcar (lambda (dep)
                (cond
                 ((symbolp dep) `(,dep "0"))
                 ((stringp dep)
                  (error "Invalid requirement specifier: %S" dep))
                 ((and (listp dep) (null (cdr dep)))
                  (list (car dep) "0"))
                 (t dep)))
              deps))))

(declare-function lm-header "lisp-mnt" (header))
(declare-function lm-homepage "lisp-mnt" (&optional file))
(declare-function lm-maintainer "lisp-mnt" (&optional file))
(declare-function lm-authors "lisp-mnt" (&optional file))

(defun package-buffer-info ()
  "Return a `package-desc' describing the package in the current buffer.

If the buffer does not contain a conforming package, signal an
error.  If there is a package, narrow the buffer to the file's
boundaries."
  (goto-char (point-min))
  (unless (re-search-forward "^;;; \\([^ ]*\\)\\.el ---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$" nil t)
    (error "Package lacks a file header"))
  (let ((file-name (match-string-no-properties 1))
        (desc      (match-string-no-properties 2))
        (start     (line-beginning-position)))
    (unless (search-forward (concat ";;; " file-name ".el ends here"))
      (error "Package lacks a terminating comment"))
    ;; Try to include a trailing newline.
    (forward-line)
    (narrow-to-region start (point))
    (require 'lisp-mnt)
    ;; Use some headers we've invented to drive the process.
    (let* ((requires-str (lm-header "package-requires"))
           ;; Prefer Package-Version; if defined, the package author
           ;; probably wants us to use it.  Otherwise try Version.
           (pkg-version
            (or (package-strip-rcs-id (lm-header "package-version"))
                (package-strip-rcs-id (lm-header "version"))))
           (homepage (lm-homepage)))
      (unless pkg-version
        (error
            "Package lacks a \"Version\" or \"Package-Version\" header"))
      (package-desc-from-define
       file-name pkg-version desc
       (if requires-str
           (package--prepare-dependencies
            (package-read-from-string requires-str)))
       :kind 'single
       :url homepage
       :maintainer (lm-maintainer)
       :authors (lm-authors)))))

(defun package--read-pkg-desc (kind)
  "Read a `define-package' form in current buffer.
Return the pkg-desc, with desc-kind set to KIND."
  (goto-char (point-min))
  (unwind-protect
      (let* ((pkg-def-parsed (read (current-buffer)))
             (pkg-desc
              (when (eq (car pkg-def-parsed) 'define-package)
                (apply #'package-desc-from-define
                  (append (cdr pkg-def-parsed))))))
        (when pkg-desc
          (setf (package-desc-kind pkg-desc) kind)
          pkg-desc))))

(declare-function tar-get-file-descriptor "tar-mode" (file))
(declare-function tar--extract "tar-mode" (descriptor))

(defun package-tar-file-info ()
  "Find package information for a tar file.
The return result is a `package-desc'."
  (cl-assert (derived-mode-p 'tar-mode))
  (let* ((dir-name (file-name-directory
                    (tar-header-name (car tar-parse-info))))
         (desc-file (package--description-file dir-name))
         (tar-desc (tar-get-file-descriptor (concat dir-name desc-file))))
    (unless tar-desc
      (error "No package descriptor file found"))
    (with-current-buffer (tar--extract tar-desc)
      (unwind-protect
          (or (package--read-pkg-desc 'tar)
              (error "Can't find define-package in %s"
                (tar-header-name tar-desc)))
        (kill-buffer (current-buffer))))))

(defun package-dir-info ()
  "Find package information for a directory.
The return result is a `package-desc'."
  (cl-assert (derived-mode-p 'dired-mode))
  (let* ((desc-file (package--description-file default-directory)))
    (if (file-readable-p desc-file)
        (with-temp-buffer
          (insert-file-contents desc-file)
          (package--read-pkg-desc 'dir))
      (let ((files (directory-files default-directory t "\\.el\\'" t))
            info)
        (while files
          (with-temp-buffer
            (insert-file-contents (pop files))
            ;; When we find the file with the data,
            (when (setq info (ignore-errors (package-buffer-info)))
              ;; stop looping,
              (setq files nil)
              ;; set the 'dir kind,
              (setf (package-desc-kind info) 'dir))))
        (unless info
          (error "No .el files with package headers in `%s'" default-directory))
        ;; and return the info.
        info))))


;;; Communicating with Archives
;; Set of low-level functions for communicating with archives and
;; signature checking.
(defun package--write-file-no-coding (file-name)
  (let ((buffer-file-coding-system 'no-conversion))
    (write-region (point-min) (point-max) file-name nil 'silent)))

(declare-function url-http-file-exists-p "url-http" (url))

(defun package--archive-file-exists-p (location file)
  (let ((http (string-match "\\`https?:" location)))
    (if http
        (progn
          (require 'url-http)
          (url-http-file-exists-p (concat location file)))
      (file-exists-p (expand-file-name file location)))))

(declare-function epg-make-context "epg"
                  (&optional protocol armor textmode include-certs
                             cipher-algorithm
                             digest-algorithm
                             compress-algorithm))
(declare-function epg-verify-string "epg" (context signature
                                                   &optional signed-text))
(declare-function epg-context-result-for "epg" (context name))
(declare-function epg-signature-status "epg" (signature) t)
(declare-function epg-signature-to-string "epg" (signature))

(defun package--display-verify-error (context sig-file)
  (unless (equal (epg-context-error-output context) "")
    (with-output-to-temp-buffer "*Error*"
      (with-current-buffer standard-output
        (if (epg-context-result-for context 'verify)
            (insert (format "Failed to verify signature %s:\n" sig-file)
                    (mapconcat #'epg-signature-to-string
                               (epg-context-result-for context 'verify)
                               "\n"))
          (insert (format "Error while verifying signature %s:\n" sig-file)))
        (insert "\nCommand output:\n" (epg-context-error-output context))))))

(defmacro package--with-work-buffer (location file &rest body)
  "Run BODY in a buffer containing the contents of FILE at LOCATION.
LOCATION is the base location of a package archive, and should be
one of the URLs (or file names) specified in `package-archives'.
FILE is the name of a file relative to that base location.

This macro retrieves FILE from LOCATION into a temporary buffer,
and evaluates BODY while that buffer is current.  This work
buffer is killed afterwards.  Return the last value in BODY."
  (declare (indent 2) (debug t)
           (obsolete package--with-response-buffer "25.1"))
  `(with-temp-buffer
     (if (string-match-p "\\`https?:" ,location)
         (url-insert-file-contents (concat ,location ,file))
       (unless (file-name-absolute-p ,location)
         (error "Archive location %s is not an absolute file name"
           ,location))
       (insert-file-contents (expand-file-name ,file ,location)))
     ,@body))

(cl-defmacro package--with-response-buffer (url &rest body &key async file error-form noerror &allow-other-keys)
  "Access URL and run BODY in a buffer containing the response.
Point is after the headers when BODY runs.
FILE, if provided, is added to URL.
URL can be a local file name, which must be absolute.
ASYNC, if non-nil, runs the request asynchronously.
ERROR-FORM is run only if a connection error occurs.  If NOERROR
is non-nil, don't propagate connection errors (does not apply to
errors signaled by ERROR-FORM or by BODY).

\(fn URL &key ASYNC FILE ERROR-FORM NOERROR &rest BODY)"
  (declare (indent defun) (debug t))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (macroexp-let2* nil ((url-1 url)
                       (noerror-1 noerror))
    (let ((url-sym (make-symbol "url"))
          (b-sym (make-symbol "b-sym")))
      `(cl-macrolet ((unless-error (body-2 &rest before-body)
                                   (let ((err (make-symbol "err")))
                                     `(with-temp-buffer
                                        (when (condition-case ,err
                                                  (progn ,@before-body t)
                                                ,(list 'error ',error-form
                                                       (list 'unless ',noerror-1
                                                             `(signal (car ,err) (cdr ,err)))))
                                          ,@body-2)))))
         (if (string-match-p "\\`https?:" ,url-1)
             (let ((,url-sym (concat ,url-1 ,file)))
               (if ,async
                   (unless-error nil
                                 (url-retrieve ,url-sym
                                               (lambda (status)
                                                 (let ((,b-sym (current-buffer)))
                                                   (require 'url-handlers)
                                                   (unless-error ,body
                                                                 (when-let* ((er (plist-get status :error)))
                                                                   (error "Error retrieving: %s %S" ,url-sym er))
                                                                 (with-current-buffer ,b-sym
                                                                   (goto-char (point-min))
                                                                   (unless (search-forward-regexp "^\r?\n\r?" nil 'noerror)
                                                                     (error "Error retrieving: %s %S" ,url-sym "incomprehensible buffer")))
                                                                 (url-insert-buffer-contents ,b-sym ,url-sym)
                                                                 (kill-buffer ,b-sym)
                                                                 (goto-char (point-min)))))
                                               nil
                                               'silent))
                 (unless-error ,body (url-insert-file-contents ,url-sym))))
           (unless-error ,body
                         (let ((url (expand-file-name ,file ,url-1)))
                           (unless (file-name-absolute-p url)
                             (error "Location %s is not a url nor an absolute file name" url))
                           (insert-file-contents url))))))))

(define-error 'bad-signature "Failed to verify signature")

(defun package--check-signature-content (content string &optional sig-file)
  "Check signature CONTENT against STRING.
SIG-FILE is the name of the signature file, used when signaling
errors."
  (let ((context (epg-make-context 'OpenPGP)))
    (when package-gnupghome-dir
      (setf (epg-context-home-directory context) package-gnupghome-dir))
    (condition-case error
        (epg-verify-string context content string)
      (error (package--display-verify-error context sig-file)
             (signal 'bad-signature error)))
    (let (good-signatures had-fatal-error)
      ;; The .sig file may contain multiple signatures.  Success if one
      ;; of the signatures is good.
      (dolist (sig (epg-context-result-for context 'verify))
        (if (eq (epg-signature-status sig) 'good)
            (push sig good-signatures)
          ;; If package-check-signature is allow-unsigned, don't
          ;; signal error when we can't verify signature because of
          ;; missing public key.  Other errors are still treated as
          ;; fatal (bug#17625).
          (unless (and (eq package-check-signature 'allow-unsigned)
                       (eq (epg-signature-status sig) 'no-pubkey))
            (setq had-fatal-error t))))
      (when (or (null good-signatures) had-fatal-error)
        (package--display-verify-error context sig-file)
        (signal 'bad-signature (list sig-file)))
      good-signatures)))

(defun package--check-signature (location file &optional string async callback unwind)
  "Check signature of the current buffer.
Download the signature file from LOCATION by appending \".sig\"
to FILE.
GnuPG keyring location depends on `package-gnupghome-dir'.
STRING is the string to verify, it defaults to `buffer-string'.
If ASYNC is non-nil, the download of the signature file is
done asynchronously.

If the signature does not verify, signal an error.
If the signature is verified and CALLBACK was provided, `funcall'
CALLBACK with the list of good signatures as argument (the list
can be empty).
If no signatures file is found, and `package-check-signature' is
`allow-unsigned', call CALLBACK with a nil argument.
Otherwise, an error is signaled.

UNWIND, if provided, is a function to be called after everything
else, even if an error is signaled."
  (let ((sig-file (concat file ".sig"))
        (string (or string (buffer-string))))
    (package--with-response-buffer location :file sig-file
      :async async :noerror t
      ;; Connection error is assumed to mean "no sig-file".
      :error-form (let ((allow-unsigned (eq package-check-signature 'allow-unsigned)))
                    (when (and callback allow-unsigned)
                      (funcall callback nil))
                    (when unwind (funcall unwind))
                    (unless allow-unsigned
                      (error "Unsigned file `%s' at %s" file location)))
      ;; OTOH, an error here means "bad signature", which we never
      ;; suppress.  (Bug#22089)
      (unwind-protect
          (let ((sig (package--check-signature-content (buffer-substring (point) (point-max))
                                                       string sig-file)))
            (when callback (funcall callback sig))
            sig)
        (when unwind (funcall unwind))))))

;;; Packages on Archives
;; The following variables store information about packages available
;; from archives.  The most important of these is
;; `package-archive-contents' which is initially populated by the
;; function `package-read-all-archive-contents' from a cache on disk.
;; The `package-initialize' command is also closely related to this
;; section, but it has its own section.
(defconst package-archive-version 1
  "Version number of the package archive understood by this file.
Lower version numbers than this will probably be understood as well.")

;; We don't prime the cache since it tends to get out of date.
(defvar package-archive-contents nil
  "Cache of the contents of the Emacs Lisp Package Archive.
This is an alist mapping package names (symbols) to
non-empty lists of `package-desc' structures.")
(put 'package-archive-contents 'risky-local-variable t)

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

(defun package--build-compatibility-table ()
  "Build `package--compatibility-table' with `package--mapc'."
  ;; Initialize the list of built-ins.
  (require 'finder-inf nil t)
  ;; Build compat table.
  (setq package--compatibility-table (make-hash-table :test 'eq))
  (package--mapc #'package--add-to-compatibility-table))

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

;; Package descriptor objects used inside the "archive-contents" file.
;; Changing this defstruct implies changing the format of the
;; "archive-contents" files.
(cl-defstruct (package--ac-desc
               (:constructor package-make-ac-desc (version reqs summary kind extras))
               (:copier nil)
               (:type vector))
  version reqs summary kind extras)

(defun package--append-to-alist (pkg-desc alist)
  "Append an entry for PKG-DESC to the start of ALIST and return it.
This entry takes the form (`package-desc-name' PKG-DESC).

If ALIST already has an entry with this name, destructively add
PKG-DESC to the cdr of this entry instead, sorted by version
number."
  (let* ((name (package-desc-name pkg-desc))
         (priority-version (package-desc-priority-version pkg-desc))
         (existing-packages (assq name alist)))
    (if (not existing-packages)
        (cons (list name pkg-desc)
              alist)
      (while (if (and (cdr existing-packages)
                      (version-list-< priority-version
                                      (package-desc-priority-version
                                       (cadr existing-packages))))
                 (setq existing-packages (cdr existing-packages))
               (push pkg-desc (cdr existing-packages))
               nil))
      alist)))

(defun package--add-to-archive-contents (package archive)
  "Add the PACKAGE from the given ARCHIVE if necessary.
PACKAGE should have the form (NAME . PACKAGE--AC-DESC).
Also, add the originating archive to the `package-desc' structure."
  (let* ((name (car package))
         (version (package--ac-desc-version (cdr package)))
         (pkg-desc
          (package-desc-create
           :name name
           :version version
           :reqs (package--ac-desc-reqs (cdr package))
           :summary (package--ac-desc-summary (cdr package))
           :kind (package--ac-desc-kind (cdr package))
           :archive archive
           :extras (and (> (length (cdr package)) 4)
                        ;; Older archive-contents files have only 4
                        ;; elements here.
                        (package--ac-desc-extras (cdr package)))))
         (pinned-to-archive (assoc name package-pinned-packages)))
    ;; Skip entirely if pinned to another archive.
    (when (not (and pinned-to-archive
                    (not (equal (cdr pinned-to-archive) archive))))
      (setq package-archive-contents
            (package--append-to-alist pkg-desc package-archive-contents)))))

(defun package--read-archive-file (file)
  "Re-read archive file FILE, if it exists.
Will return the data from the file, or nil if the file does not exist.
Will throw an error if the archive version is too new."
  (let ((filename (expand-file-name file package-user-dir)))
    (when (file-exists-p filename)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents filename))
        (let ((contents (read (current-buffer))))
          (if (> (car contents) package-archive-version)
              (error "Package archive version %d is higher than %d"
                (car contents) package-archive-version))
          (cdr contents))))))

(defun package-read-archive-contents (archive)
  "Re-read archive contents for ARCHIVE.
If successful, set the variable `package-archive-contents'.
If the archive version is too new, signal an error."
  ;; Version 1 of 'archive-contents' is identical to our internal
  ;; representation.
  (let* ((contents-file (format "archives/%s/archive-contents" archive))
         (contents (package--read-archive-file contents-file)))
    (when contents
      (dolist (package contents)
        (package--add-to-archive-contents package archive)))))

(defvar package--old-archive-priorities nil
  "Store currently used `package-archive-priorities'.
This is the value of `package-archive-priorities' last time
`package-read-all-archive-contents' was called.  It can be used
by arbitrary functions to decide whether it is necessary to call
it again.")

(defun package-read-all-archive-contents ()
  "Re-read `archive-contents', if it exists.
If successful, set `package-archive-contents'."
  (setq package-archive-contents nil)
  (setq package--old-archive-priorities package-archive-priorities)
  (dolist (archive package-archives)
    (package-read-archive-contents (car archive))))

;;;; Package Initialize
;; A bit of a milestone.  This brings together some of the above
;; sections and populates all relevant lists of packages from contents
;; available on disk.
(defvar package--initialized nil)

;;;###autoload
(defun package-initialize (&optional no-activate)
  "Load Emacs Lisp packages, and activate them.
The variable `package-load-list' controls which packages to load.
If optional arg NO-ACTIVATE is non-nil, don't activate packages.
If called as part of loading `user-init-file', set
`package-enable-at-startup' to nil, to prevent accidentally
loading packages twice.

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
  (setq package-enable-at-startup nil)
  (package-load-all-descriptors)
  (package-read-all-archive-contents)
  (unless no-activate
    (dolist (elt package-alist)
      (condition-case err
          (package-activate (car elt))
        ;; Don't let failure of activation of a package arbitrarily stop
        ;; activation of further packages.
        (error (message "%s" (error-message-string err))))))
  (setq package--initialized t)
  ;; This uses `package--mapc' so it must be called after
  ;; `package--initialized' is t.
  (package--build-compatibility-table))


;;;; Populating `package-archive-contents' from archives
;; This subsection populates the variables listed above from the
;; actual archives, instead of from a local cache.
(defvar package--downloads-in-progress nil
  "List of in-progress asynchronous downloads.")

(declare-function epg-import-keys-from-file "epg" (context keys))

;;;###autoload
(defun package-import-keyring (&optional file)
  "Import keys from FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let ((context (epg-make-context 'OpenPGP)))
    (when package-gnupghome-dir
      (with-file-modes 448
        (make-directory package-gnupghome-dir t))
      (setf (epg-context-home-directory context) package-gnupghome-dir))
    (message "Importing %s..." (file-name-nondirectory file))
    (epg-import-keys-from-file context file)
    (message "Importing %s...done" (file-name-nondirectory file))))

(defvar package--post-download-archives-hook nil
  "Hook run after the archive contents are downloaded.
Don't run this hook directly.  It is meant to be run as part of
`package--update-downloads-in-progress'.")
(put 'package--post-download-archives-hook 'risky-local-variable t)

(defun package--update-downloads-in-progress (entry)
  "Remove ENTRY from `package--downloads-in-progress'.
Once it's empty, run `package--post-download-archives-hook'."
  ;; Keep track of the downloading progress.
  (setq package--downloads-in-progress
        (remove entry package--downloads-in-progress))
  ;; If this was the last download, run the hook.
  (unless package--downloads-in-progress
    (package-read-all-archive-contents)
    (package--build-compatibility-table)
    ;; We message before running the hook, so the hook can give
    ;; messages as well.
    (message "Package refresh done")
    (run-hooks 'package--post-download-archives-hook)))

(defun package--download-one-archive (archive file &optional async)
  "Retrieve an archive file FILE from ARCHIVE, and cache it.
ARCHIVE should be a cons cell of the form (NAME . LOCATION),
similar to an entry in `package-alist'.  Save the cached copy to
\"archives/NAME/FILE\" in `package-user-dir'."
  (package--with-response-buffer (cdr archive) :file file
    :async async
    :error-form (package--update-downloads-in-progress archive)
    (let* ((location (cdr archive))
           (name (car archive))
           (content (buffer-string))
           (dir (expand-file-name (format "archives/%s" name) package-user-dir))
           (local-file (expand-file-name file dir)))
      (when (listp (read-from-string content))
        (make-directory dir t)
        (if (or (not package-check-signature)
                (member name package-unsigned-archives))
            ;; If we don't care about the signature, save the file and
            ;; we're done.
            (progn (write-region content nil local-file nil 'silent)
                   (package--update-downloads-in-progress archive))
          ;; If we care, check it (perhaps async) and *then* write the file.
          (package--check-signature
           location file content async
           ;; This function will be called after signature checking.
           (lambda (&optional good-sigs)
             (write-region content nil local-file nil 'silent)
             ;; Write out good signatures into archive-contents.signed file.
             (when good-sigs
               (write-region (mapconcat #'epg-signature-to-string good-sigs "\n")
                             nil (concat local-file ".signed") nil 'silent)))
           (lambda () (package--update-downloads-in-progress archive))))))))

(defun package--download-and-read-archives (&optional async)
  "Download descriptions of all `package-archives' and read them.
This populates `package-archive-contents'.  If ASYNC is non-nil,
perform the downloads asynchronously."
  ;; The downloaded archive contents will be read as part of
  ;; `package--update-downloads-in-progress'.
  (dolist (archive package-archives)
    (cl-pushnew archive package--downloads-in-progress
                :test #'equal))
  (dolist (archive package-archives)
    (condition-case-unless-debug nil
        (package--download-one-archive archive "archive-contents" async)
      (error (message "Failed to download `%s' archive."
               (car archive))))))

;;;###autoload
(defun package-refresh-contents (&optional async)
  "Download descriptions of all configured ELPA packages.
For each archive configured in the variable `package-archives',
inform Emacs about the latest versions of all packages it offers,
and make them available for download.
Optional argument ASYNC specifies whether to perform the
downloads in the background."
  (interactive)
  (unless (file-exists-p package-user-dir)
    (make-directory package-user-dir t))
  (let ((default-keyring (expand-file-name "package-keyring.gpg"
                                           data-directory))
        (inhibit-message async))
    (when (and package-check-signature (file-exists-p default-keyring))
      (condition-case-unless-debug error
          (package-import-keyring default-keyring)
        (error (message "Cannot import default keyring: %S" (cdr error))))))
  (package--download-and-read-archives async))


;;; Dependency Management
;; Calculating the full transaction necessary for an installation,
;; keeping track of which packages were installed strictly as
;; dependencies, and determining which packages cannot be removed
;; because they are dependencies.
(defun package-compute-transaction (packages requirements &optional seen)
  "Return a list of packages to be installed, including PACKAGES.
PACKAGES should be a list of `package-desc'.

REQUIREMENTS should be a list of additional requirements; each
element in this list should have the form (PACKAGE VERSION-LIST),
where PACKAGE is a package name and VERSION-LIST is the required
version of that package.

This function recursively computes the requirements of the
packages in REQUIREMENTS, and returns a list of all the packages
that must be installed.  Packages that are already installed are
not included in this list.

SEEN is used internally to detect infinite recursion."
  ;; FIXME: We really should use backtracking to explore the whole
  ;; search space (e.g. if foo require bar-1.3, and bar-1.4 requires toto-1.1
  ;; whereas bar-1.3 requires toto-1.0 and the user has put a hold on toto-1.0:
  ;; the current code might fail to see that it could install foo by using the
  ;; older bar-1.3).
  (dolist (elt requirements)
    (let* ((next-pkg (car elt))
           (next-version (cadr elt))
           (already ()))
      (dolist (pkg packages)
        (if (eq next-pkg (package-desc-name pkg))
            (setq already pkg)))
      (when already
        (if (version-list-<= next-version (package-desc-version already))
            ;; `next-pkg' is already in `packages', but its position there
            ;; means it might be installed too late: remove it from there, so
            ;; we re-add it (along with its dependencies) at an earlier place
            ;; below (bug#16994).
            (if (memq already seen)     ;Avoid inf-loop on dependency cycles.
                (message "Dependency cycle going through %S"
                         (package-desc-full-name already))
              (setq packages (delq already packages))
              (setq already nil))
          (error "Need package `%s-%s', but only %s is being installed"
                 next-pkg (package-version-join next-version)
                 (package-version-join (package-desc-version already)))))
      (cond
       (already nil)
       ((package-installed-p next-pkg next-version) nil)

       (t
        ;; A package is required, but not installed.  It might also be
        ;; blocked via `package-load-list'.
        (let ((pkg-descs (cdr (assq next-pkg package-archive-contents)))
              (found nil)
              (found-something nil)
              (problem nil))
          (while (and pkg-descs (not found))
            (let* ((pkg-desc (pop pkg-descs))
                   (version (package-desc-version pkg-desc))
                   (disabled (package-disabled-p next-pkg version)))
              (cond
               ((version-list-< version next-version)
                ;; pkg-descs is sorted by priority, not version, so
                ;; don't error just yet.
                (unless found-something
                  (setq found-something (package-version-join version))))
               (disabled
                (unless problem
                  (setq problem
                        (if (stringp disabled)
                            (format-message
                             "Package `%s' held at version %s, but version %s required"
                             next-pkg disabled
                             (package-version-join next-version))
                          (format-message "Required package `%s' is disabled"
                                          next-pkg)))))
               (t (setq found pkg-desc)))))
          (unless found
            (cond
             (problem (error "%s" problem))
             (found-something
              (error "Need package `%s-%s', but only %s is available"
                     next-pkg (package-version-join next-version)
                     found-something))
             (t (error "Package `%s-%s' is unavailable"
                       next-pkg (package-version-join next-version)))))
          (setq packages
                (package-compute-transaction (cons found packages)
                                             (package-desc-reqs found)
                                             (cons found seen))))))))
  packages)

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
  (when value
    (setq package-selected-packages value))
  (if after-init-time
      (let ((save-silently inhibit-message))
        (customize-save-variable 'package-selected-packages package-selected-packages))
    (add-hook 'after-init-hook #'package--save-selected-packages)))

(defun package--user-selected-p (pkg)
  "Return non-nil if PKG is a package was installed by the user.
PKG is a package name.
This looks into `package-selected-packages', populating it first
if it is still empty."
  (unless (consp package-selected-packages)
    (package--save-selected-packages (package--find-non-dependencies)))
  (memq pkg package-selected-packages))

(defun package--get-deps (pkg &optional only)
  (let* ((pkg-desc (cadr (assq pkg package-alist)))
         (direct-deps (cl-loop for p in (package-desc-reqs pkg-desc)
                               for name = (car p)
                               when (assq name package-alist)
                               collect name))
         (indirect-deps (unless (eq only 'direct)
                          (delete-dups
                           (cl-loop for p in direct-deps
                                    append (package--get-deps p))))))
    (cl-case only
      (direct   direct-deps)
      (separate (list direct-deps indirect-deps))
      (indirect indirect-deps)
      (t        (delete-dups (append direct-deps indirect-deps))))))

(defun package--removable-packages ()
  "Return a list of names of packages no longer needed.
These are packages which are neither contained in
`package-selected-packages' nor a dependency of one that is."
  (let ((needed (cl-loop for p in package-selected-packages
                         if (assq p package-alist)
                         ;; `p' and its dependencies are needed.
                         append (cons p (package--get-deps p)))))
    (cl-loop for p in (mapcar #'car package-alist)
             unless (memq p needed)
             collect p)))

(defun package--used-elsewhere-p (pkg-desc &optional pkg-list all)
  "Non-nil if PKG-DESC is a dependency of a package in PKG-LIST.
Return the first package found in PKG-LIST of which PKG is a
dependency.  If ALL is non-nil, return all such packages instead.

When not specified, PKG-LIST defaults to `package-alist'
with PKG-DESC entry removed."
  (unless (string= (package-desc-status pkg-desc) "obsolete")
    (let* ((pkg (package-desc-name pkg-desc))
           (alist (or pkg-list
                      (remove (assq pkg package-alist)
                              package-alist))))
      (if all
          (cl-loop for p in alist
                   if (assq pkg (package-desc-reqs (cadr p)))
                   collect (cadr p))
        (cl-loop for p in alist thereis
                 (and (assq pkg (package-desc-reqs (cadr p)))
                      (cadr p)))))))

(defun package--sort-deps-in-alist (package only)
  "Return a list of dependencies for PACKAGE sorted by dependency.
PACKAGE is included as the first element of the returned list.
ONLY is an alist associating package names to package objects.
Only these packages will be in the return value and their cdrs are
destructively set to nil in ONLY."
  (let ((out))
    (dolist (dep (package-desc-reqs package))
      (when-let* ((cell (assq (car dep) only))
                  (dep-package (cdr-safe cell)))
        (setcdr cell nil)
        (setq out (append (package--sort-deps-in-alist dep-package only)
                          out))))
    (cons package out)))

(defun package--sort-by-dependence (package-list)
  "Return PACKAGE-LIST sorted by dependence.
That is, any element of the returned list is guaranteed to not
directly depend on any elements that come before it.

PACKAGE-LIST is a list of `package-desc' objects.
Indirect dependencies are guaranteed to be returned in order only
if all the in-between dependencies are also in PACKAGE-LIST."
  (let ((alist (mapcar (lambda (p) (cons (package-desc-name p) p)) package-list))
        out-list)
    (dolist (cell alist out-list)
      ;; `package--sort-deps-in-alist' destructively changes alist, so
      ;; some cells might already be empty.  We check this here.
      (when-let* ((pkg-desc (cdr cell)))
        (setcdr cell nil)
        (setq out-list
              (append (package--sort-deps-in-alist pkg-desc alist)
                      out-list))))))


;;; Installation Functions
;; As opposed to the previous section (which listed some underlying
;; functions necessary for installation), this one contains the actual
;; functions that install packages.  The package itself can be
;; installed in a variety of ways (archives, buffer, file), but
;; requirements (dependencies) are always satisfied by looking in
;; `package-archive-contents'.
(defun package-archive-base (desc)
  "Return the archive containing the package NAME."
  (cdr (assoc (package-desc-archive desc) package-archives)))

(defun package-install-from-archive (pkg-desc)
  "Download and install a tar package."
  ;; This won't happen, unless the archive is doing something wrong.
  (when (eq (package-desc-kind pkg-desc) 'dir)
    (error "Can't install directory package from archive"))
  (let* ((location (package-archive-base pkg-desc))
         (file (concat (package-desc-full-name pkg-desc)
                       (package-desc-suffix pkg-desc))))
    (package--with-response-buffer location :file file
      (if (or (not package-check-signature)
              (member (package-desc-archive pkg-desc)
                      package-unsigned-archives))
          ;; If we don't care about the signature, unpack and we're
          ;; done.
          (let ((save-silently t))
            (package-unpack pkg-desc))
        ;; If we care, check it and *then* write the file.
        (let ((content (buffer-string)))
          (package--check-signature
           location file content nil
           ;; This function will be called after signature checking.
           (lambda (&optional good-sigs)
             ;; Signature checked, unpack now.
             (with-temp-buffer (insert content)
                               (let ((save-silently t))
                                 (package-unpack pkg-desc)))
             ;; Here the package has been installed successfully, mark it as
             ;; signed if appropriate.
             (when good-sigs
               ;; Write out good signatures into NAME-VERSION.signed file.
               (write-region (mapconcat #'epg-signature-to-string good-sigs "\n")
                             nil
                             (expand-file-name
                              (concat (package-desc-full-name pkg-desc) ".signed")
                              package-user-dir)
                             nil 'silent)
               ;; Update the old pkg-desc which will be shown on the description buffer.
               (setf (package-desc-signed pkg-desc) t)
               ;; Update the new (activated) pkg-desc as well.
               (when-let* ((pkg-descs (cdr (assq (package-desc-name pkg-desc) package-alist))))
                 (setf (package-desc-signed (car pkg-descs)) t))))))))))

(defun package-installed-p (package &optional min-version)
  "Return non-nil if PACKAGE, of MIN-VERSION or newer, is installed.
If PACKAGE is a symbol, it is the package name and MIN-VERSION
should be a version list.

If PACKAGE is a `package-desc' object, MIN-VERSION is ignored."
  (unless package--initialized (error "package.el is not yet initialized!"))
  (if (package-desc-p package)
      (let ((dir (package-desc-dir package)))
        (and (stringp dir)
             (file-exists-p dir)))
    (or
     (let ((pkg-descs (cdr (assq package package-alist))))
       (and pkg-descs
            (version-list-<= min-version
                             (package-desc-version (car pkg-descs)))))
     ;; Also check built-in packages.
     (package-built-in-p package min-version))))

(defun package-download-transaction (packages)
  "Download and install all the packages in PACKAGES.
PACKAGES should be a list of `package-desc'.
This function assumes that all package requirements in
PACKAGES are satisfied, i.e. that PACKAGES is computed
using `package-compute-transaction'."
  (mapc #'package-install-from-archive packages))

;;;###autoload
(defun package-install (pkg &optional dont-select)
  "Install the package PKG.
PKG can be a `package-desc' or a symbol naming one of the available packages
in an archive in `package-archives'.  Interactively, prompt for its name.

If called interactively or if DONT-SELECT nil, add PKG to
`package-selected-packages'.

If PKG is a `package-desc' and it is already installed, don't try
to install it but still mark it as selected."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (unless (package-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  package-archive-contents))
                    nil t))
           nil)))
  (add-hook 'post-command-hook #'package-menu--post-refresh)
  (let ((name (if (package-desc-p pkg)
                  (package-desc-name pkg)
                pkg)))
    (unless (or dont-select (package--user-selected-p name))
      (package--save-selected-packages
       (cons name package-selected-packages)))
    (if-let* ((transaction
               (if (package-desc-p pkg)
                   (unless (package-installed-p pkg)
                     (package-compute-transaction (list pkg)
                                                  (package-desc-reqs pkg)))
                 (package-compute-transaction () (list (list pkg))))))
        (package-download-transaction transaction)
      (message "`%s' is already installed" name))))

(defun package-strip-rcs-id (str)
  "Strip RCS version ID from the version string STR.
If the result looks like a dotted numeric version, return it.
Otherwise return nil."
  (when str
    (when (string-match "\\`[ \t]*[$]Revision:[ \t]+" str)
      (setq str (substring str (match-end 0))))
    (ignore-errors
      (if (version-to-list str) str))))

(declare-function lm-homepage "lisp-mnt" (&optional file))

;;;###autoload
(defun package-install-from-buffer ()
  "Install a package from the current buffer.
The current buffer is assumed to be a single .el or .tar file or
a directory.  These must follow the packaging guidelines (see
info node `(elisp)Packaging').

Specially, if current buffer is a directory, the -pkg.el
description file is not mandatory, in which case the information
is derived from the main .el file in the directory.

Downloads and installs required packages as needed."
  (interactive)
  (let* ((pkg-desc
          (cond
            ((derived-mode-p 'dired-mode)
             ;; This is the only way a package-desc object with a `dir'
             ;; desc-kind can be created.  Such packages can't be
             ;; uploaded or installed from archives, they can only be
             ;; installed from local buffers or directories.
             (package-dir-info))
            ((derived-mode-p 'tar-mode)
             (package-tar-file-info))
            (t
             (save-excursion
              (package-buffer-info)))))
         (name (package-desc-name pkg-desc)))
    ;; Download and install the dependencies.
    (let* ((requires (package-desc-reqs pkg-desc))
           (transaction (package-compute-transaction nil requires)))
      (package-download-transaction transaction))
    ;; Install the package itself.
    (package-unpack pkg-desc)
    (unless (package--user-selected-p name)
      (package--save-selected-packages
       (cons name package-selected-packages)))
    pkg-desc))

;;;###autoload
(defun package-install-file (file)
  "Install a package from a file.
The file can either be a tar file, an Emacs Lisp file, or a
directory."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (if (file-directory-p file)
        (progn
          (setq default-directory file)
          (dired-mode))
      (insert-file-contents-literally file)
      (when (string-match "\\.tar\\'" file) (tar-mode)))
    (package-install-from-buffer)))

;;;###autoload
(defun package-install-selected-packages ()
  "Ensure packages in `package-selected-packages' are installed.
If some packages are not installed propose to install them."
  (interactive)
  ;; We don't need to populate `package-selected-packages' before
  ;; using here, because the outcome is the same either way (nothing
  ;; gets installed).
  (if (not package-selected-packages)
      (message "`package-selected-packages' is empty, nothing to install")
    (let* ((not-installed (seq-remove #'package-installed-p package-selected-packages))
           (available (seq-filter (lambda (p) (assq p package-archive-contents)) not-installed))
           (difference (- (length not-installed) (length available))))
      (cond
       (available
        (when (y-or-n-p
               (format "%s packages will be installed:\n%s, proceed?"
                       (length available)
                       (mapconcat #'symbol-name available ", ")))
          (mapc (lambda (p) (package-install p 'dont-select)) available)))
       ((> difference 0)
        (message "%s packages are not available (the rest already installed), maybe you need to `M-x package-refresh-contents'"
                 difference))
       (t
        (message "All your packages are already installed"))))))


;;; Package Deletion
(defun package--newest-p (pkg)
  "Return non-nil if PKG is the newest package with its name."
  (equal (cadr (assq (package-desc-name pkg) package-alist))
         pkg))

(defun package-delete (pkg-desc &optional force nosave)
  "Delete package PKG-DESC.

Argument PKG-DESC is a full description of package as vector.
Interactively, prompt the user for the package name and version.

When package is used elsewhere as dependency of another package,
refuse deleting it and return an error.
If prefix argument FORCE is non-nil, package will be deleted even
if it is used elsewhere.
If NOSAVE is non-nil, the package is not removed from
`package-selected-packages'."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (let* ((package-table
             (mapcar
              (lambda (p) (cons (package-desc-full-name p) p))
              (delq nil
                    (mapcar (lambda (p) (unless (package-built-in-p p) p))
                            (apply #'append (mapcar #'cdr package-alist))))))
            (package-name (completing-read "Delete package: "
                                           (mapcar #'car package-table)
                                           nil t)))
       (list (cdr (assoc package-name package-table))
             current-prefix-arg nil))))
  (let ((dir (package-desc-dir pkg-desc))
        (name (package-desc-name pkg-desc))
        pkg-used-elsewhere-by)
    ;; If the user is trying to delete this package, they definitely
    ;; don't want it marked as selected, so we remove it from
    ;; `package-selected-packages' even if it can't be deleted.
    (when (and (null nosave)
               (package--user-selected-p name)
               ;; Don't deselect if this is an older version of an
               ;; upgraded package.
               (package--newest-p pkg-desc))
      (package--save-selected-packages (remove name package-selected-packages)))
    (cond ((not (string-prefix-p (file-name-as-directory
                                  (expand-file-name package-user-dir))
                                 (expand-file-name dir)))
           ;; Don't delete "system" packages.
           (error "Package `%s' is a system package, not deleting"
                  (package-desc-full-name pkg-desc)))
          ((and (null force)
                (setq pkg-used-elsewhere-by
                      (package--used-elsewhere-p pkg-desc)))
           ;; Don't delete packages used as dependency elsewhere.
           (error "Package `%s' is used by `%s' as dependency, not deleting"
                  (package-desc-full-name pkg-desc)
                  (package-desc-name pkg-used-elsewhere-by)))
          (t
           (add-hook 'post-command-hook #'package-menu--post-refresh)
           (delete-directory dir t)
           ;; Remove NAME-VERSION.signed and NAME-readme.txt files.
           (dolist (suffix '(".signed" "readme.txt"))
             (let* ((version (package-version-join (package-desc-version pkg-desc)))
                    (file (concat (if (string= suffix ".signed")
                                      dir
                                    (substring dir 0 (- (length version))))
                                  suffix)))
               (when (file-exists-p file)
                 (delete-file file))))
           ;; Update package-alist.
           (let ((pkgs (assq name package-alist)))
             (delete pkg-desc pkgs)
             (unless (cdr pkgs)
               (setq package-alist (delq pkgs package-alist))))
           (message "Package `%s' deleted." (package-desc-full-name pkg-desc))))))

;;;###autoload
(defun package-reinstall (pkg)
  "Reinstall package PKG.
PKG should be either a symbol, the package name, or a `package-desc'
object."
  (interactive (list (intern (completing-read
                              "Reinstall package: "
                              (mapcar #'symbol-name
                                      (mapcar #'car package-alist))))))
  (package-delete
   (if (package-desc-p pkg) pkg (cadr (assq pkg package-alist)))
   'force 'nosave)
  (package-install pkg 'dont-select))

;;;###autoload
(defun package-autoremove ()
  "Remove packages that are no more needed.

Packages that are no more needed by other packages in
`package-selected-packages' and their dependencies
will be deleted."
  (interactive)
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or package-selected-packages
            (yes-or-no-p
             (format-message
              "`package-selected-packages' is empty! Really remove ALL packages? ")))
    (let ((removable (package--removable-packages)))
      (if removable
          (when (y-or-n-p
                 (format "%s packages will be deleted:\n%s, proceed? "
                   (length removable)
                   (mapconcat #'symbol-name removable ", ")))
            (mapc (lambda (p)
                    (package-delete (cadr (assq p package-alist)) t))
                  removable))
        (message "Nothing to autoremove")))))


;;;; Package description buffer.

;;;###autoload
(defun describe-package (package)
  "Display the full documentation of PACKAGE (a symbol)."
  (interactive
   (let* ((guess (or (function-called-at-point)
                     (symbol-at-point))))
     (require 'finder-inf nil t)
     ;; Load the package list if necessary (but don't activate them).
     (unless package--initialized
       (package-initialize t))
     (let ((packages (append (mapcar 'car package-alist)
                             (mapcar 'car package-archive-contents)
                             (mapcar 'car package--builtins))))
       (unless (memq guess packages)
         (setq guess nil))
       (setq packages (mapcar 'symbol-name packages))
       (let ((val
              (completing-read (if guess
                                   (format "Describe package (default %s): "
                                           guess)
                                 "Describe package: ")
                               packages nil t nil nil (when guess
                                                        (symbol-name guess)))))
         (list (intern val))))))
  (if (not (or (package-desc-p package) (and package (symbolp package))))
      (message "No package specified")
    (help-setup-xref (list #'describe-package package)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (describe-package-1 package)))))

(defface package-help-section-name
  '((t :inherit (bold font-lock-function-name-face)))
  "Face used on section names in package description buffers."
  :version "25.1")

(defun package--print-help-section (name &rest strings)
  "Print \"NAME: \", right aligned to the 13th column.
If more STRINGS are provided, insert them followed by a newline.
Otherwise no newline is inserted."
  (declare (indent 1))
  (insert (make-string (max 0 (- 11 (string-width name))) ?\s)
          (propertize (concat name ": ") 'font-lock-face 'package-help-section-name))
  (when strings
    (apply #'insert strings)
    (insert "\n")))

(declare-function lm-commentary "lisp-mnt" (&optional file))

(defun describe-package-1 (pkg)
  (require 'lisp-mnt)
  (let* ((desc (or
                (if (package-desc-p pkg) pkg)
                (cadr (assq pkg package-alist))
                (let ((built-in (assq pkg package--builtins)))
                  (if built-in
                      (package--from-builtin built-in)
                    (cadr (assq pkg package-archive-contents))))))
         (name (if desc (package-desc-name desc) pkg))
         (pkg-dir (if desc (package-desc-dir desc)))
         (reqs (if desc (package-desc-reqs desc)))
         (required-by (if desc (package--used-elsewhere-p desc nil 'all)))
         (version (if desc (package-desc-version desc)))
         (archive (if desc (package-desc-archive desc)))
         (extras (and desc (package-desc-extras desc)))
         (homepage (cdr (assoc :url extras)))
         (commit (cdr (assoc :commit extras)))
         (keywords (if desc (package-desc--keywords desc)))
         (built-in (eq pkg-dir 'builtin))
         (installable (and archive (not built-in)))
         (status (if desc (package-desc-status desc) "orphan"))
         (incompatible-reason (package--incompatible-p desc))
         (signed (if desc (package-desc-signed desc))))
    (when (string= status "avail-obso")
      (setq status "available obsolete"))
    (when incompatible-reason
      (setq status "incompatible"))
    (prin1 name)
    (princ " is ")
    (princ (if (memq (aref status 0) '(?a ?e ?i ?o ?u)) "an " "a "))
    (princ status)
    (princ " package.\n\n")

    (package--print-help-section "Status")
    (cond (built-in
           (insert (propertize (capitalize status)
                               'font-lock-face 'package-status-built-in)
                   "."))
          (pkg-dir
           (insert (propertize (if (member status '("unsigned" "dependency"))
                                   "Installed"
                                 (capitalize status))
                               'font-lock-face 'package-status-built-in))
           (insert (substitute-command-keys " in `"))
           (let ((dir (abbreviate-file-name
                       (file-name-as-directory
                        (if (file-in-directory-p pkg-dir package-user-dir)
                            (file-relative-name pkg-dir package-user-dir)
                          pkg-dir)))))
             (help-insert-xref-button dir 'help-package-def pkg-dir))
           (if (and (package-built-in-p name)
                    (not (package-built-in-p name version)))
               (insert (substitute-command-keys
                        "',\n             shadowing a ")
                       (propertize "built-in package"
                                   'font-lock-face 'package-status-built-in))
             (insert (substitute-command-keys "'")))
           (if signed
               (insert ".")
             (insert " (unsigned)."))
           (when (and (package-desc-p desc)
                      (not required-by)
                      (member status '("unsigned" "installed")))
             (insert " ")
             (package-make-button "Delete"
                                  'action #'package-delete-button-action
                                  'package-desc desc)))
          (incompatible-reason
           (insert (propertize "Incompatible" 'font-lock-face font-lock-warning-face)
                   " because it depends on ")
           (if (stringp incompatible-reason)
               (insert "Emacs " incompatible-reason ".")
             (insert "uninstallable packages.")))
          (installable
           (insert (capitalize status))
           (insert " from " (format "%s" archive))
           (insert " -- ")
           (package-make-button
            "Install"
            'action 'package-install-button-action
            'package-desc desc))
          (t (insert (capitalize status) ".")))
    (insert "\n")
    (unless (and pkg-dir (not archive)) ; Installed pkgs don't have archive.
      (package--print-help-section "Archive"
        (or archive "n/a")))
    (and version
         (package--print-help-section "Version"
           (package-version-join version)))
    (when commit
      (package--print-help-section "Commit" commit))
    (when desc
      (package--print-help-section "Summary"
        (package-desc-summary desc)))

    (setq reqs (if desc (package-desc-reqs desc)))
    (when reqs
      (package--print-help-section "Requires")
      (let ((first t))
        (dolist (req reqs)
          (let* ((name (car req))
                 (vers (cadr req))
                 (text (format "%s-%s" (symbol-name name)
                               (package-version-join vers)))
                 (reason (if (and (listp incompatible-reason)
                                  (assq name incompatible-reason))
                             " (not available)" "")))
            (cond (first (setq first nil))
                  ((>= (+ 2 (current-column) (length text) (length reason))
                       (window-width))
                   (insert ",\n               "))
                  (t (insert ", ")))
            (help-insert-xref-button text 'help-package name)
            (insert reason)))
        (insert "\n")))
    (when required-by
      (package--print-help-section "Required by")
      (let ((first t))
        (dolist (pkg required-by)
          (let ((text (package-desc-full-name pkg)))
            (cond (first (setq first nil))
                  ((>= (+ 2 (current-column) (length text))
                       (window-width))
                   (insert ",\n               "))
                  (t (insert ", ")))
            (help-insert-xref-button text 'help-package
                                     (package-desc-name pkg))))
        (insert "\n")))
    (when homepage
      ;; Prefer https for the homepage of packages on gnu.org.
      (if (string-match-p "^http://\\(elpa\\|www\\)\\.gnu\\.org/" homepage)
          (let ((gnu (cdr (assoc "gnu" package-archives))))
            (and gnu (string-match-p "^https" gnu)
                 (setq homepage
                       (replace-regexp-in-string "^http" "https" homepage)))))
      (package--print-help-section "Homepage")
      (help-insert-xref-button homepage 'help-url homepage)
      (insert "\n"))
    (when keywords
      (package--print-help-section "Keywords")
      (dolist (k keywords)
        (package-make-button
         k
         'package-keyword k
         'action 'package-keyword-button-action)
        (insert " "))
      (insert "\n"))
    (let* ((all-pkgs (append (cdr (assq name package-alist))
                             (cdr (assq name package-archive-contents))
                             (let ((bi (assq name package--builtins)))
                               (if bi (list (package--from-builtin bi))))))
           (other-pkgs (delete desc all-pkgs)))
      (when other-pkgs
        (package--print-help-section "Other versions"
          (mapconcat (lambda (opkg)
                       (let* ((ov (package-desc-version opkg))
                              (dir (package-desc-dir opkg))
                              (from (or (package-desc-archive opkg)
                                        (if (stringp dir) "installed" dir))))
                         (if (not ov) (format "%s" from)
                           (format "%s (%s)"
                                   (make-text-button (package-version-join ov) nil
                                                     'font-lock-face 'link
                                                     'follow-link t
                                                     'action
                                                     (lambda (_button)
                                                       (describe-package opkg)))
                                   from))))
                     other-pkgs ", ")
          ".")))

    (insert "\n")

    (if built-in
        ;; For built-in packages, insert the commentary.
        (let ((fn (locate-file (format "%s.el" name) load-path
                               load-file-rep-suffixes))
              (opoint (point)))
          (insert (or (lm-commentary fn) ""))
          (save-excursion
            (goto-char opoint)
            (when (re-search-forward "^;;; Commentary:\n" nil t)
              (replace-match ""))
            (while (re-search-forward "^\\(;+ ?\\)" nil t)
              (replace-match ""))))
      (let* ((basename (format "%s-readme.txt" name))
             (readme (expand-file-name basename package-user-dir))
             readme-string)
        ;; For elpa packages, try downloading the commentary.  If that
        ;; fails, try an existing readme file in `package-user-dir'.
        (cond ((and (package-desc-archive desc)
                    (package--with-response-buffer (package-archive-base desc)
                      :file basename :noerror t
                      (save-excursion
                        (goto-char (point-max))
                        (unless (bolp)
                          (insert ?\n)))
                      (write-region nil nil
                                    (expand-file-name readme package-user-dir)
                                    nil 'silent)
                      (setq readme-string (buffer-string))
                      t))
               (insert readme-string))
              ((file-readable-p readme)
               (insert-file-contents readme)
               (goto-char (point-max))))))))

(defun package-install-button-action (button)
  (let ((pkg-desc (button-get button 'package-desc)))
    (when (y-or-n-p (format-message "Install package `%s'? "
                                    (package-desc-full-name pkg-desc)))
      (package-install pkg-desc nil)
      (revert-buffer nil t)
      (goto-char (point-min)))))

(defun package-delete-button-action (button)
  (let ((pkg-desc (button-get button 'package-desc)))
    (when (y-or-n-p (format-message "Delete package `%s'? "
                                    (package-desc-full-name pkg-desc)))
      (package-delete pkg-desc)
      (revert-buffer nil t)
      (goto-char (point-min)))))

(defun package-keyword-button-action (button)
  (let ((pkg-keyword (button-get button 'package-keyword)))
    (package-show-package-list t (list pkg-keyword))))

(defun package-make-button (text &rest props)
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box (:line-width 2 :color "dark grey")
                                :background "light grey"
                                :foreground "black")
                       'link)))
    (apply 'insert-text-button button-text 'face button-face 'follow-link t
           props)))


;;;; Package menu mode.

(defvar package-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'package-menu-describe-package)
    (define-key map "u" 'package-menu-mark-unmark)
    (define-key map "\177" 'package-menu-backup-unmark)
    (define-key map "d" 'package-menu-mark-delete)
    (define-key map "i" 'package-menu-mark-install)
    (define-key map "U" 'package-menu-mark-upgrades)
    (define-key map "r" 'package-menu-refresh)
    (define-key map "f" 'package-menu-filter)
    (define-key map "~" 'package-menu-mark-obsolete-for-deletion)
    (define-key map "x" 'package-menu-execute)
    (define-key map "h" 'package-menu-quick-help)
    (define-key map "H" #'package-menu-hide-package)
    (define-key map "?" 'package-menu-describe-package)
    (define-key map "(" #'package-menu-toggle-hiding)
    map)
  "Local keymap for `package-menu-mode' buffers.")

(easy-menu-define package-menu-mode-menu package-menu-mode-map
  "Menu for `package-menu-mode'."
  `("Package"
    ["Describe Package" package-menu-describe-package :help "Display information about this package"]
    ["Help" package-menu-quick-help :help "Show short key binding help for package-menu-mode"]
    "--"
    ["Refresh Package List" package-menu-refresh
     :help "Redownload the ELPA archive"
     :active (not package--downloads-in-progress)]
    ["Redisplay buffer" revert-buffer :help "Update the buffer with current list of packages"]
    ["Execute Marked Actions" package-menu-execute :help "Perform all the marked actions"]

    "--"
    ["Mark All Available Upgrades" package-menu-mark-upgrades
     :help "Mark packages that have a newer version for upgrading"
     :active (not package--downloads-in-progress)]
    ["Mark All Obsolete for Deletion" package-menu-mark-obsolete-for-deletion :help "Mark all obsolete packages for deletion"]
    ["Mark for Install" package-menu-mark-install :help "Mark a package for installation and move to the next line"]
    ["Mark for Deletion" package-menu-mark-delete :help "Mark a package for deletion and move to the next line"]
    ["Unmark" package-menu-mark-unmark :help "Clear any marks on a package and move to the next line"]

    "--"
    ["Filter Package List" package-menu-filter :help "Filter package selection (q to go back)"]
    ["Hide by Regexp" package-menu-hide-package :help "Permanently hide all packages matching a regexp"]
    ["Display Older Versions" package-menu-toggle-hiding
     :style toggle :selected (not package-menu--hide-packages)
     :help "Display package even if a newer version is already installed"]

    "--"
    ["Quit" quit-window :help "Quit package selection"]
    ["Customize" (customize-group 'package)]))

(defvar package-menu--new-package-list nil
  "List of newly-available packages since `list-packages' was last called.")

(defvar package-menu--transaction-status nil
  "Mode-line status of ongoing package transaction.")

(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq mode-line-process '((package--downloads-in-progress ":Loading")
                            (package-menu--transaction-status
                             package-menu--transaction-status)))
  (setq tabulated-list-format
        `[("Package" 18 package-menu--name-predicate)
          ("Version" 13 nil)
          ("Status"  10 package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 10 package-menu--archive-predicate)))
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (tabulated-list-init-header))

(defmacro package--push (pkg-desc status listname)
  "Convenience macro for `package-menu--generate'.
If the alist stored in the symbol LISTNAME lacks an entry for a
package PKG-DESC, add one.  The alist is keyed with PKG-DESC."
  `(unless (assoc ,pkg-desc ,listname)
     ;; FIXME: Should we move status into pkg-desc?
     (push (cons ,pkg-desc ,status) ,listname)))

(defvar package-list-unversioned nil
  "If non-nil include packages that don't have a version in `list-package'.")

(defvar package-list-unsigned nil
  "If non-nil, mention in the list which packages were installed w/o signature.")

(defvar package--emacs-version-list (version-to-list emacs-version)
  "`emacs-version', as a list.")

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

(defun package-desc-status (pkg-desc)
  (let* ((name (package-desc-name pkg-desc))
         (dir (package-desc-dir pkg-desc))
         (lle (assq name package-load-list))
         (held (cadr lle))
         (version (package-desc-version pkg-desc))
         (signed (or (not package-list-unsigned)
                     (package-desc-signed pkg-desc))))
    (cond
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
          (if (memq name package-menu--new-package-list)
              "new" "available"))))))))

(defvar package-menu--hide-packages t
  "Whether available obsolete packages should be hidden.
Can be toggled with \\<package-menu-mode-map> \\[package-menu-toggle-hiding].
Installed obsolete packages are always displayed.")

(defun package-menu-toggle-hiding ()
  "Toggle visibility of obsolete available packages."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (user-error "The current buffer is not a Package Menu"))
  (setq package-menu--hide-packages
        (not package-menu--hide-packages))
  (message "%s packages" (if package-menu--hide-packages
                             "Hiding obsolete or unwanted"
                           "Displaying all"))
  (revert-buffer nil 'no-confirm))

(defun package--remove-hidden (pkg-list)
  "Filter PKG-LIST according to `package-archive-priorities'.
PKG-LIST must be a list of `package-desc' objects, all with the
same name, sorted by decreasing `package-desc-priority-version'.
Return a list of packages tied for the highest priority according
to their archives."
  (when pkg-list
    ;; Variable toggled with `package-menu-toggle-hiding'.
    (if (not package-menu--hide-packages)
        pkg-list
      (let ((installed (cadr (assq (package-desc-name (car pkg-list))
                                   package-alist))))
        (when installed
          (setq pkg-list
                (let ((ins-version (package-desc-version installed)))
                  (cl-remove-if (lambda (p) (version-list-< (package-desc-version p)
                                                       ins-version))
                                pkg-list))))
        (let ((filtered-by-priority
               (cond
                ((not package-menu-hide-low-priority)
                 pkg-list)
                ((eq package-menu-hide-low-priority 'archive)
                 (let* ((max-priority most-negative-fixnum)
                        (out))
                   (while pkg-list
                     (let ((p (pop pkg-list)))
                       (let ((priority (package-desc-priority p)))
                         (if (< priority max-priority)
                             (setq pkg-list nil)
                           (push p out)
                           (setq max-priority priority)))))
                   (nreverse out)))
                (pkg-list
                 (list (car pkg-list))))))
          (if (not installed)
              filtered-by-priority
            (let ((ins-version (package-desc-version installed)))
              (cl-remove-if (lambda (p) (version-list-= (package-desc-version p)
                                                   ins-version))
                            filtered-by-priority))))))))

(defcustom package-hidden-regexps nil
  "List of regexps matching the name of packages to hide.
If the name of a package matches any of these regexps it is
omitted from the package menu.  To toggle this, type \\[package-menu-toggle-hiding].

Values can be interactively added to this list by typing
\\[package-menu-hide-package] on a package"
  :version "25.1"
  :type '(repeat (regexp :tag "Hide packages with name matching")))

(defun package-menu--refresh (&optional packages keywords)
  "Re-populate the `tabulated-list-entries'.
PACKAGES should be nil or t, which means to display all known packages.
KEYWORDS should be nil or a list of keywords."
  ;; Construct list of (PKG-DESC . STATUS).
  (unless packages (setq packages t))
  (let ((hidden-names (mapconcat #'identity package-hidden-regexps "\\|"))
        info-list)
    ;; Installed packages:
    (dolist (elt package-alist)
      (let ((name (car elt)))
        (when (or (eq packages t) (memq name packages))
          (dolist (pkg (cdr elt))
            (when (package--has-keyword-p pkg keywords)
              (push pkg info-list))))))

    ;; Built-in packages:
    (dolist (elt package--builtins)
      (let ((pkg  (package--from-builtin elt))
            (name (car elt)))
        (when (not (eq name 'emacs)) ; Hide the `emacs' package.
          (when (and (package--has-keyword-p pkg keywords)
                     (or package-list-unversioned
                         (package--bi-desc-version (cdr elt)))
                     (or (eq packages t) (memq name packages)))
            (push pkg info-list)))))

    ;; Available and disabled packages:
    (unless (equal package--old-archive-priorities package-archive-priorities)
      (package-read-all-archive-contents))
    (dolist (elt package-archive-contents)
      (let ((name (car elt)))
        ;; To be displayed it must be in PACKAGES;
        (when (and (or (eq packages t) (memq name packages))
                   ;; and we must either not be hiding anything,
                   (or (not package-menu--hide-packages)
                       (not package-hidden-regexps)
                       ;; or just not hiding this specific package.
                       (not (string-match hidden-names (symbol-name name)))))
          ;; Hide available-obsolete or low-priority packages.
          (dolist (pkg (package--remove-hidden (cdr elt)))
            (when (package--has-keyword-p pkg keywords)
              (push pkg info-list))))))

    ;; Print the result.
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar #'package-menu--print-info-simple info-list))))

(defun package-all-keywords ()
  "Collect all package keywords"
  (let ((key-list))
    (package--mapc (lambda (desc)
                     (setq key-list (append (package-desc--keywords desc)
                                            key-list))))
    key-list))

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
    (dolist (elt package-archive-contents)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (dolist (pkg (cdr elt))
          ;; Hide obsolete packages.
          (unless (package-installed-p (package-desc-name pkg)
                                       (package-desc-version pkg))
        (funcall function pkg)))))))

(defun package--has-keyword-p (desc &optional keywords)
  "Test if package DESC has any of the given KEYWORDS.
When none are given, the package matches."
  (if keywords
      (let ((desc-keywords (and desc (package-desc--keywords desc)))
            found)
        (while (and (not found) keywords)
          (let ((k (pop keywords)))
            (setq found
                  (or (string= k (concat "arc:" (package-desc-archive desc)))
                      (string= k (concat "status:" (package-desc-status desc)))
                      (member k desc-keywords)))))
        found)
    t))

(defun package-menu--generate (remember-pos packages &optional keywords)
  "Populate the Package Menu.
 If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (package-menu--refresh packages keywords)
  (setf (car (aref tabulated-list-format 0))
        (if keywords
            (let ((filters (mapconcat 'identity keywords ",")))
              (concat "Package[" filters "]"))
          "Package"))
  (if keywords
      (define-key package-menu-mode-map "q" 'package-show-package-list)
    (define-key package-menu-mode-map "q" 'quit-window))
  (tabulated-list-init-header)
  (tabulated-list-print remember-pos))

(defun package-menu--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (package-menu--print-info-simple (car pkg)))
(make-obsolete 'package-menu--print-info
               'package-menu--print-info-simple "25.1")


;;; Package menu faces
(defface package-name
  '((t :inherit link))
  "Face used on package names in the package menu."
  :version "25.1")

(defface package-description
  '((t :inherit default))
  "Face used on package description summaries in the package menu."
  :version "25.1")

;; Shame this hyphenates "built-in", when "font-lock-builtin-face" doesn't.
(defface package-status-built-in
  '((t :inherit font-lock-builtin-face))
  "Face used on the status and version of built-in packages."
  :version "25.1")

(defface package-status-external
  '((t :inherit package-status-built-in))
  "Face used on the status and version of external packages."
  :version "25.1")

(defface package-status-available
  '((t :inherit default))
  "Face used on the status and version of available packages."
  :version "25.1")

(defface package-status-new
  '((t :inherit (bold package-status-available)))
  "Face used on the status and version of new packages."
  :version "25.1")

(defface package-status-held
  '((t :inherit font-lock-constant-face))
  "Face used on the status and version of held packages."
  :version "25.1")

(defface package-status-disabled
  '((t :inherit font-lock-warning-face))
  "Face used on the status and version of disabled packages."
  :version "25.1")

(defface package-status-installed
  '((t :inherit font-lock-comment-face))
  "Face used on the status and version of installed packages."
  :version "25.1")

(defface package-status-dependency
  '((t :inherit package-status-installed))
  "Face used on the status and version of dependency packages."
  :version "25.1")

(defface package-status-unsigned
  '((t :inherit font-lock-warning-face))
  "Face used on the status and version of unsigned packages."
  :version "25.1")

(defface package-status-incompat
  '((t :inherit error))
  "Face used on the status and version of incompat packages."
  :version "25.1")

(defface package-status-avail-obso
  '((t :inherit package-status-incompat))
  "Face used on the status and version of avail-obso packages."
  :version "25.1")


;;; Package menu printing
(defun package-menu--print-info-simple (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG is a `package-desc' object.
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (let* ((status  (package-desc-status pkg))
         (face (pcase status
                 (`"built-in"  'package-status-built-in)
                 (`"external"  'package-status-external)
                 (`"available" 'package-status-available)
                 (`"avail-obso" 'package-status-avail-obso)
                 (`"new"       'package-status-new)
                 (`"held"      'package-status-held)
                 (`"disabled"  'package-status-disabled)
                 (`"installed" 'package-status-installed)
                 (`"dependency" 'package-status-dependency)
                 (`"unsigned"  'package-status-unsigned)
                 (`"incompat"  'package-status-incompat)
                 (_            'font-lock-warning-face)))) ; obsolete.
    (list pkg
          `[(,(symbol-name (package-desc-name pkg))
             face package-name
             font-lock-face package-name
             follow-link t
             package-desc ,pkg
             action package-menu-describe-package)
            ,(propertize (package-version-join
                          (package-desc-version pkg))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg) "")
                                    'font-lock-face face)))
            ,(propertize (package-desc-summary pkg)
                         'font-lock-face 'package-description)])))

(defvar package-menu--old-archive-contents nil
  "`package-archive-contents' before the latest refresh.")

(defun package-menu-refresh ()
  "Download the Emacs Lisp package archive.
This fetches the contents of each archive specified in
`package-archives', and then refreshes the package menu."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (user-error "The current buffer is not a Package Menu"))
  (setq package-menu--old-archive-contents package-archive-contents)
  (setq package-menu--new-package-list nil)
  (package-refresh-contents package-menu-async))

(defun package-menu-hide-package ()
  "Hide a package under point.
If optional arg BUTTON is non-nil, describe its associated package."
  (interactive)
  (declare (interactive-only "change `package-hidden-regexps' instead."))
  (let* ((name (when (derived-mode-p 'package-menu-mode)
                 (concat "\\`" (regexp-quote (symbol-name (package-desc-name
                                                           (tabulated-list-get-id)))))))
         (re (read-string "Hide packages matching regexp: " name)))
    ;; Test if it is valid.
    (string-match re "")
    (push re package-hidden-regexps)
    (customize-save-variable 'package-hidden-regexps package-hidden-regexps)
    (package-menu--post-refresh)
    (let ((hidden
           (cl-remove-if-not (lambda (e) (string-match re (symbol-name (car e))))
                             package-archive-contents)))
      (message (substitute-command-keys
                (concat "Hiding %s packages, type `\\[package-menu-toggle-hiding]'"
                        " to toggle or `\\[customize-variable] RET package-hidden-regexps'"
                        " to customize it"))
        (length hidden)))))

(defun package-menu-describe-package (&optional button)
  "Describe the current package.
If optional arg BUTTON is non-nil, describe its associated package."
  (interactive)
  (let ((pkg-desc (if button (button-get button 'package-desc)
                    (tabulated-list-get-id))))
    (if pkg-desc
        (describe-package pkg-desc)
      (user-error "No package here"))))

;; fixme numeric argument
(defun package-menu-mark-delete (&optional _num)
  "Mark a package for deletion and move to the next line."
  (interactive "p")
  (if (member (package-menu-get-status)
              '("installed" "dependency" "obsolete" "unsigned"))
      (tabulated-list-put-tag "D" t)
    (forward-line)))

(defun package-menu-mark-install (&optional _num)
  "Mark a package for installation and move to the next line."
  (interactive "p")
  (if (member (package-menu-get-status) '("available" "avail-obso" "new" "dependency"))
      (tabulated-list-put-tag "I" t)
    (forward-line)))

(defun package-menu-mark-unmark (&optional _num)
  "Clear any marks on a package and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag " " t))

(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (tabulated-list-put-tag " "))

(defun package-menu-mark-obsolete-for-deletion ()
  "Mark all obsolete packages for deletion."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (equal (package-menu-get-status) "obsolete")
          (tabulated-list-put-tag "D" t)
        (forward-line 1)))))

(defvar package--quick-help-keys
  '(("install," "delete," "unmark," ("execute" . 1))
    ("next," "previous")
    ("Hide-package," "(-toggle-hidden")
    ("refresh-contents," "g-redisplay," "filter," "help")))

(defun package--prettify-quick-help-key (desc)
  "Prettify DESC to be displayed as a help menu."
  (if (listp desc)
      (if (listp (cdr desc))
          (mapconcat #'package--prettify-quick-help-key desc "   ")
        (let ((place (cdr desc))
              (out (car desc)))
          (add-text-properties place (1+ place)
                               '(face (bold font-lock-warning-face))
                               out)
          out))
    (package--prettify-quick-help-key (cons desc 0))))

(defun package-menu-quick-help ()
  "Show short key binding help for `package-menu-mode'.
The full list of keys can be viewed with \\[describe-mode]."
  (interactive)
  (message (mapconcat #'package--prettify-quick-help-key
                      package--quick-help-keys "\n")))

(define-obsolete-function-alias
  'package-menu-view-commentary 'package-menu-describe-package "24.1")

(defun package-menu-get-status ()
  (let* ((id (tabulated-list-get-id))
         (entry (and id (assoc id tabulated-list-entries))))
    (if entry
        (aref (cadr entry) 2)
      "")))

(defun package-archive-priority (archive)
  "Return the priority of ARCHIVE.

The archive priorities are specified in
`package-archive-priorities'. If not given there, the priority
defaults to 0."
  (or (cdr (assoc archive package-archive-priorities))
      0))

(defun package-desc-priority-version (pkg-desc)
  "Return the version PKG-DESC with the archive priority prepended.

This allows for easy comparison of package versions from
different archives if archive priorities are meant to be taken in
consideration."
  (cons (package-desc-priority pkg-desc)
        (package-desc-version pkg-desc)))

(defun package-menu--find-upgrades ()
  (let (installed available upgrades)
    ;; Build list of installed/available packages in this buffer.
    (dolist (entry tabulated-list-entries)
      ;; ENTRY is (PKG-DESC [NAME VERSION STATUS DOC])
      (let ((pkg-desc (car entry))
            (status (aref (cadr entry) 2)))
        (cond ((member status '("installed" "dependency" "unsigned"))
               (push pkg-desc installed))
              ((member status '("available" "new"))
               (setq available (package--append-to-alist pkg-desc available))))))
    ;; Loop through list of installed packages, finding upgrades.
    (dolist (pkg-desc installed)
      (let* ((name (package-desc-name pkg-desc))
             (avail-pkg (cadr (assq name available))))
        (and avail-pkg
             (version-list-< (package-desc-priority-version pkg-desc)
                             (package-desc-priority-version avail-pkg))
             (push (cons name avail-pkg) upgrades))))
    upgrades))

(defvar package-menu--mark-upgrades-pending nil
  "Whether mark-upgrades is waiting for a refresh to finish.")

(defun package-menu--mark-upgrades-1 ()
  "Mark all upgradable packages in the Package Menu.
Implementation of `package-menu-mark-upgrades'."
  (unless (derived-mode-p 'package-menu-mode)
    (error "The current buffer is not a Package Menu"))
  (setq package-menu--mark-upgrades-pending nil)
  (let ((upgrades (package-menu--find-upgrades)))
    (if (null upgrades)
        (message "No packages to upgrade.")
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((pkg-desc (tabulated-list-get-id))
                 (upgrade (cdr (assq (package-desc-name pkg-desc) upgrades))))
            (cond ((null upgrade)
                   (forward-line 1))
                  ((equal pkg-desc upgrade)
                   (package-menu-mark-install))
                  (t
                   (package-menu-mark-delete))))))
      (message "%d package%s marked for upgrading."
        (length upgrades)
        (if (= (length upgrades) 1) "" "s")))))

(defun package-menu-mark-upgrades ()
  "Mark all upgradable packages in the Package Menu.
For each installed package with a newer version available, place
an (I)nstall flag on the available version and a (D)elete flag on
the installed version.  A subsequent \\[package-menu-execute]
call will upgrade the package.

If there's an async refresh operation in progress, the flags will
be placed as part of `package-menu--post-refresh' instead of
immediately."
  (interactive)
  (if (not package--downloads-in-progress)
      (package-menu--mark-upgrades-1)
    (setq package-menu--mark-upgrades-pending t)
    (message "Waiting for refresh to finish...")))

(defun package-menu--list-to-prompt (packages)
  "Return a string listing PACKAGES that's usable in a prompt.
PACKAGES is a list of `package-desc' objects.
Formats the returned string to be usable in a minibuffer
prompt (see `package-menu--prompt-transaction-p')."
  (cond
   ;; None
   ((not packages) "")
   ;; More than 1
   ((cdr packages)
    (format "these %d packages (%s)"
      (length packages)
      (mapconcat #'package-desc-full-name packages ", ")))
   ;; Exactly 1
   (t (format-message "package `%s'"
                      (package-desc-full-name (car packages))))))

(defun package-menu--prompt-transaction-p (delete install upgrade)
  "Prompt the user about DELETE, INSTALL, and UPGRADE.
DELETE, INSTALL, and UPGRADE are lists of `package-desc' objects.
Either may be nil, but not all."
  (y-or-n-p
   (concat
    (when delete "Delete ")
    (package-menu--list-to-prompt delete)
    (when (and delete install)
      (if upgrade "; " "; and "))
    (when install "Install ")
    (package-menu--list-to-prompt install)
    (when (and upgrade (or install delete)) "; and ")
    (when upgrade "Upgrade ")
    (package-menu--list-to-prompt upgrade)
    "? ")))

(defun package-menu--partition-transaction (install delete)
  "Return an alist describing an INSTALL DELETE transaction.
Alist contains three entries, upgrade, delete, and install, each
with a list of package names.

The upgrade entry contains any `package-desc' objects in INSTALL
whose name coincides with an object in DELETE.  The delete and
the install entries are the same as DELETE and INSTALL with such
objects removed."
  (let* ((upg (cl-intersection install delete :key #'package-desc-name))
         (ins (cl-set-difference install upg :key #'package-desc-name))
         (del (cl-set-difference delete upg :key #'package-desc-name)))
    `((delete . ,del) (install . ,ins) (upgrade . ,upg))))

(defun package-menu--perform-transaction (install-list delete-list)
  "Install packages in INSTALL-LIST and delete DELETE-LIST."
  (if install-list
      (let ((status-format (format ":Installing %%d/%d"
                             (length install-list)))
            (i 0)
            (package-menu--transaction-status))
        (dolist (pkg install-list)
          (setq package-menu--transaction-status
                (format status-format (cl-incf i)))
          (force-mode-line-update)
          (redisplay 'force)
          ;; Don't mark as selected, `package-menu-execute' already
          ;; does that.
          (package-install pkg 'dont-select))))
  (let ((package-menu--transaction-status ":Deleting"))
    (force-mode-line-update)
    (redisplay 'force)
    (dolist (elt (package--sort-by-dependence delete-list))
      (condition-case-unless-debug err
          (let ((inhibit-message package-menu-async))
            (package-delete elt nil 'nosave))
        (error (message "Error trying to delete `%s': %S"
                 (package-desc-full-name elt)
                 err))))))

(defun package--update-selected-packages (add remove)
  "Update the `package-selected-packages' list according to ADD and REMOVE.
ADD and REMOVE must be disjoint lists of package names (or
`package-desc' objects) to be added and removed to the selected
packages list, respectively."
  (dolist (p add)
    (cl-pushnew (if (package-desc-p p) (package-desc-name p) p)
                package-selected-packages))
  (dolist (p remove)
    (setq package-selected-packages
          (remove (if (package-desc-p p) (package-desc-name p) p)
                  package-selected-packages)))
  (when (or add remove)
    (package--save-selected-packages package-selected-packages)))

(defun package-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.
Optional argument NOQUERY non-nil means do not ask the user to confirm."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (error "The current buffer is not in Package Menu mode"))
  (let (install-list delete-list cmd pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          ;; This is the key PKG-DESC.
          (setq pkg-desc (tabulated-list-get-id))
          (cond ((eq cmd ?D)
                 (push pkg-desc delete-list))
                ((eq cmd ?I)
                 (push pkg-desc install-list))))
        (forward-line)))
    (unless (or delete-list install-list)
      (user-error "No operations specified"))
    (let-alist (package-menu--partition-transaction install-list delete-list)
      (when (or noquery
                (package-menu--prompt-transaction-p .delete .install .upgrade))
        (let ((message-template
               (concat "Package menu: Operation %s ["
                       (when .delete  (format "Delet__ %s" (length .delete)))
                       (when (and .delete .install) "; ")
                       (when .install (format "Install__ %s" (length .install)))
                       (when (and .upgrade (or .install .delete)) "; ")
                       (when .upgrade (format "Upgrad__ %s" (length .upgrade)))
                       "]")))
          (message (replace-regexp-in-string "__" "ing" message-template) "started")
          ;; Packages being upgraded are not marked as selected.
          (package--update-selected-packages .install .delete)
          (package-menu--perform-transaction install-list delete-list)
          (when package-selected-packages
            (if-let* ((removable (package--removable-packages)))
                (message "Package menu: Operation finished.  %d packages %s"
                  (length removable)
                  (substitute-command-keys
                   "are no longer needed, type `\\[package-autoremove]' to remove them"))
              (message (replace-regexp-in-string "__" "ed" message-template)
                "finished"))))))))

(defun package-menu--version-predicate (A B)
  (let ((vA (or (aref (cadr A) 1)  '(0)))
        (vB (or (aref (cadr B) 1) '(0))))
    (if (version-list-= vA vB)
        (package-menu--name-predicate A B)
      (version-list-< vA vB))))

(defun package-menu--status-predicate (A B)
  (let ((sA (aref (cadr A) 2))
        (sB (aref (cadr B) 2)))
    (cond ((string= sA sB)
           (package-menu--name-predicate A B))
          ((string= sA "new") t)
          ((string= sB "new") nil)
          ((string-prefix-p "avail" sA)
           (if (string-prefix-p "avail" sB)
               (package-menu--name-predicate A B)
             t))
          ((string-prefix-p "avail" sB) nil)
          ((string= sA "installed") t)
          ((string= sB "installed") nil)
          ((string= sA "dependency") t)
          ((string= sB "dependency") nil)
          ((string= sA "unsigned") t)
          ((string= sB "unsigned") nil)
          ((string= sA "held") t)
          ((string= sB "held") nil)
          ((string= sA "external") t)
          ((string= sB "external") nil)
          ((string= sA "built-in") t)
          ((string= sB "built-in") nil)
          ((string= sA "obsolete") t)
          ((string= sB "obsolete") nil)
          ((string= sA "incompat") t)
          ((string= sB "incompat") nil)
          (t (string< sA sB)))))

(defun package-menu--description-predicate (A B)
  (let ((dA (aref (cadr A) 3))
        (dB (aref (cadr B) 3)))
    (if (string= dA dB)
        (package-menu--name-predicate A B)
      (string< dA dB))))

(defun package-menu--name-predicate (A B)
  (string< (symbol-name (package-desc-name (car A)))
           (symbol-name (package-desc-name (car B)))))

(defun package-menu--archive-predicate (A B)
  (string< (or (package-desc-archive (car A)) "")
           (or (package-desc-archive (car B)) "")))

(defun package-menu--populate-new-package-list ()
  "Decide which packages are new in `package-archives-contents'.
Store this list in `package-menu--new-package-list'."
  ;; Find which packages are new.
  (when package-menu--old-archive-contents
    (dolist (elt package-archive-contents)
      (unless (assq (car elt) package-menu--old-archive-contents)
        (push (car elt) package-menu--new-package-list)))
    (setq package-menu--old-archive-contents nil)))

(defun package-menu--find-and-notify-upgrades ()
  "Notify the user of upgradable packages."
  (when-let* ((upgrades (package-menu--find-upgrades)))
    (message "%d package%s can be upgraded; type `%s' to mark %s for upgrading."
      (length upgrades)
      (if (= (length upgrades) 1) "" "s")
      (substitute-command-keys "\\[package-menu-mark-upgrades]")
      (if (= (length upgrades) 1) "it" "them"))))

(defun package-menu--post-refresh ()
  "If there's a *Packages* buffer, revert it and check for new packages and upgrades.
Do nothing if there's no *Packages* buffer.

This function is called after `package-refresh-contents' and it
is added to `post-command-hook' by any function which alters the
package database (`package-install' and `package-delete').  When
run, it removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'package-menu--post-refresh)
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (package-menu--populate-new-package-list)
        (run-hooks 'tabulated-list-revert-hook)
        (tabulated-list-print 'remember 'update)))))

(defun package-menu--mark-or-notify-upgrades ()
  "If there's a *Packages* buffer, check for upgrades and possibly mark them.
Do nothing if there's no *Packages* buffer.  If there are
upgrades, mark them if `package-menu--mark-upgrades-pending' is
non-nil, otherwise just notify the user that there are upgrades.
This function is called after `package-refresh-contents'."
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if package-menu--mark-upgrades-pending
            (package-menu--mark-upgrades-1)
          (package-menu--find-and-notify-upgrades))))))

;;;###autoload
(defun list-packages (&optional no-fetch)
  "Display a list of packages.
This first fetches the updated list of packages before
displaying, unless a prefix argument NO-FETCH is specified.
The list is displayed in a buffer named `*Packages*', and
includes the package's version, availability status, and a
short description."
  (interactive "P")
  (require 'finder-inf nil t)
  ;; Initialize the package system if necessary.
  (unless package--initialized
    (package-initialize t))
  ;; Integrate the package-menu with updating the archives.
  (add-hook 'package--post-download-archives-hook
            #'package-menu--post-refresh)
  (add-hook 'package--post-download-archives-hook
            #'package-menu--mark-or-notify-upgrades 'append)

  ;; Generate the Package Menu.
  (let ((buf (get-buffer-create "*Packages*")))
    (with-current-buffer buf
      (package-menu-mode)

      ;; Fetch the remote list of packages.
      (unless no-fetch (package-menu-refresh))

      ;; If we're not async, this would be redundant.
      (when package-menu-async
        (package-menu--generate nil t)))
    ;; The package menu buffer has keybindings.  If the user types
    ;; `M-x list-packages', that suggests it should become current.
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'package-list-packages 'list-packages)

;; Used in finder.el
(defun package-show-package-list (&optional packages keywords)
  "Display PACKAGES in a *Packages* buffer.
This is similar to `list-packages', but it does not fetch the
updated list of packages, and it only displays packages with
names in PACKAGES (which should be a list of symbols).

When KEYWORDS are given, only packages with those KEYWORDS are
shown."
  (interactive)
  (require 'finder-inf nil t)
  (let* ((buf (get-buffer-create "*Packages*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (package-menu-mode)
      (package-menu--generate nil packages keywords))
    (if win
        (select-window win)
      (switch-to-buffer buf))))

;; package-menu--generate rebinds "q" on the fly, so we have to
;; hard-code the binding in the doc-string here.
(defun package-menu-filter (keyword)
  "Filter the *Packages* buffer.
Show only those items that relate to the specified KEYWORD.
KEYWORD can be a string or a list of strings.  If it is a list, a
package will be displayed if it matches any of the keywords.
Interactively, it is a list of strings separated by commas.

To restore the full package list, type `q'."
  (interactive
   (list (completing-read-multiple
          "Keywords (comma separated): " (package-all-keywords))))
  (package-show-package-list t (if (stringp keyword)
                                   (list keyword)
                                 keyword)))

(defun package-list-packages-no-fetch ()
  "Display a list of packages.
Does not fetch the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (list-packages t))

(provide 'package)

;;; package.el ends here
