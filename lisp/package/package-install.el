;;; package-install.el --- Physical Package Management  -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>

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

;; This file implements the logic for installing and updating a
;; tarball-based package.

;;; Code:

(require 'package-core)
(require 'package-elpa)
(require 'package-quickstart)

(require 'epg)
(require 'tar-mode)
(require 'lisp-mnt)

(defcustom package-install-upgrade-built-in nil
  "Non-nil means that built-in packages can be upgraded via a package archive.
If disabled, then `package-install' will not suggest to replace a
built-in package with a (possibly newer) version from a package archive."
  :type 'boolean
  :version "29.1"
  :group 'package)

(defcustom package-native-compile nil
  "Non-nil means to natively compile packages as part of their installation.
This controls ahead-of-time compilation of packages when they are
installed.  If this option is nil, packages will be natively
compiled when they are loaded for the first time.

This option does not have any effect if Emacs was not built with
native compilation support."
  :type '(boolean)
  :risky t
  :version "28.1"
  :group 'package)

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
             (t
              (if (eq next-pkg 'emacs)
                  (error "This package requires Emacs version %s"
                         (package-version-join next-version))
                (error (if (not next-version)
                           (format "Package `%s' is unavailable" next-pkg)
                         (format "Package `%s' (version %s) is unavailable"
                                 next-pkg (package-version-join next-version))))))))
          (setq packages
                (package-compute-transaction (cons found packages)
                                             (package-desc-reqs found)
                                             (cons found seen))))))))
  packages)

(defun package--get-deps (pkgs)
  (let ((seen '()))
    (while pkgs
      (let ((pkg (pop pkgs)))
        (if (memq pkg seen)
            nil ;; Done already!
          (let ((pkg-desc (cadr (assq pkg package-alist))))
            (when pkg-desc
              (push pkg seen)
              (setq pkgs (append (mapcar #'car (package-desc-reqs pkg-desc))
                                 pkgs)))))))
    seen))

(defun package--user-installed-p (package)
  "Return non-nil if PACKAGE is a user-installed package.
PACKAGE is the package name, a symbol.  Check whether the package
was installed into `package-user-dir' where we assume to have
control over."
  (let* ((pkg-desc (cadr (assq package package-alist)))
         (dir (package-desc-dir pkg-desc)))
    (file-in-directory-p dir package-user-dir)))

(defun package--removable-packages ()
  "Return a list of names of packages no longer needed.
These are packages which are neither contained in
`package-selected-packages' nor a dependency of one that is."
  (let ((needed (package--get-deps package-selected-packages)))
    (cl-loop for p in (mapcar #'car package-alist)
             unless (or (memq p needed)
                        ;; Do not auto-remove external packages.
                        (not (package--user-installed-p p)))
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
  "Return the package described by DESC."
  (cdr (assoc (package-desc-archive desc) package-archives)))

(defun package-desc-suffix (pkg-desc)
  "Return file-name extension of package-desc object PKG-DESC.
Depending on the `package-desc-kind' of PKG-DESC, this is one of:

   \\='single - \".el\"
   \\='tar    - \".tar\"
   \\='dir    - \"\"

Signal an error if the kind is none of the above."
  (pcase (package-desc-kind pkg-desc)
    ('single ".el")
    ('tar ".tar")
    ('dir "")
    (kind (error "Unknown package kind: %s" kind))))

(defun package-install-from-archive (pkg-desc)
  "Download and install a package defined by PKG-DESC."
  ;; This won't happen, unless the archive is doing something wrong.
  (when (eq (package-desc-kind pkg-desc) 'dir)
    (error "Can't install directory package from archive"))
  (let* ((location (package-archive-base pkg-desc))
         (file (concat (package-desc-full-name pkg-desc)
                       (package-desc-suffix pkg-desc))))
    (package--with-response-buffer location :file file
      (if (or (not (package-check-signature))
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
             (with-temp-buffer ;FIXME: Just use the previous current-buffer.
               (set-buffer-multibyte nil)
               (cl-assert (not (multibyte-string-p content)))
               (insert content)
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
               (when-let* ((pkg-descs (cdr (assq (package-desc-name pkg-desc)
                                                 package-alist))))
                 (setf (package-desc-signed (car pkg-descs)) t))))))))))

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
   ((and (not package--initialized)
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

PKG can be a `package-desc', or a symbol naming one of the available
packages in an archive in `package-archives'.

Mark the installed package as selected by adding it to
`package-selected-packages'.

When called from Lisp and optional argument DONT-SELECT is
non-nil, install the package but do not add it to
`package-selected-packages'.

If PKG is a `package-desc' and it is already installed, don't try
to install it but still mark it as selected.

If the command is invoked with a prefix argument, it will allow
upgrading of built-in packages, as if `package-install-upgrade-built-in'
had been enabled."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package--archives-initialize)
     (list (intern (completing-read
                    "Install package: "
                    (mapcan
                     (lambda (elt)
                       (and (or (and (or current-prefix-arg
                                         package-install-upgrade-built-in)
                                     (package--active-built-in-p (car elt)))
                                (not (package-installed-p (car elt))))
                            (list (symbol-name (car elt)))))
                     package-archive-contents)
                    nil t))
           nil)))
  (cl-check-type pkg (or symbol package-desc))
  (package--archives-initialize)
  (when (fboundp 'package-menu--post-refresh)
    (add-hook 'post-command-hook #'package-menu--post-refresh))
  (let ((name (if (package-desc-p pkg)
                  (package-desc-name pkg)
                pkg)))
    (unless (or dont-select (package--user-selected-p name))
      (package--save-selected-packages
       (cons name package-selected-packages)))
    (when (and (or current-prefix-arg package-install-upgrade-built-in)
               (package--active-built-in-p pkg))
      (setq pkg (or (cadr (assq name package-archive-contents)) pkg)))
    (if-let* ((transaction
               (if (package-desc-p pkg)
                   (unless (package-installed-p pkg)
                     (package-compute-transaction (list pkg)
                                                  (package-desc-reqs pkg)))
                 (package-compute-transaction () (list (list pkg))))))
        (progn
          (package-download-transaction transaction)
          (package--quickstart-maybe-refresh)
          (message  "Package `%s' installed." name))
      (message "`%s' is already installed" name))))

(declare-function package-vc-upgrade "package-vc" (pkg))

;;;###autoload
(defun package-upgrade (name)
  "Upgrade package NAME if a newer version exists.

NAME should be a symbol."
  (interactive
   (list (intern (completing-read
                  "Upgrade package: "
                  (package--upgradeable-packages t) nil t))))
  (cl-check-type name symbol)
  (let* ((pkg-desc (cadr (assq name package-alist)))
         (package-install-upgrade-built-in (not pkg-desc)))
    ;; `pkg-desc' will be nil when the package is an "active built-in".
    (if (and pkg-desc (package-vc-p pkg-desc))
        (package-vc-upgrade pkg-desc)
      (when pkg-desc
        (package-delete pkg-desc 'force 'dont-unselect))
      (package-install name
                       ;; An active built-in has never been "selected"
                       ;; before.  Mark it as installed explicitly.
                       (and pkg-desc 'dont-select)))))

(defun package--upgradeable-packages (&optional include-builtins)
  ;; Initialize the package system to get the list of package
  ;; symbols for completion.
  (package--archives-initialize)
  (mapcar
   #'car
   (seq-filter
    (lambda (elt)
      (or (let ((available
                 (assq (car elt) package-archive-contents)))
            (and available
                 (or (and
                      include-builtins
                      (not (package-desc-version (cadr elt))))
                     (version-list-<
                      (package-desc-version (cadr elt))
                      (package-desc-version (cadr available))))))
          (package-vc-p (cadr elt))))
    (if include-builtins
        (append package-alist
                (mapcan
                 (lambda (elt)
                   (when (not (assq (car elt) package-alist))
                     (list (list (car elt) (package--from-builtin elt)))))
                 package--builtins))
      package-alist))))

;;;###autoload
(defun package-upgrade-all (&optional query)
  "Refresh package list and upgrade all packages.
If QUERY, ask the user before upgrading packages.  When called
interactively, QUERY is always true.

Currently, packages which are part of the Emacs distribution are
not upgraded by this command.  To enable upgrading such a package
using this command, first upgrade the package to a newer version
from ELPA by either using `\\[package-upgrade]' or
`\\<package-menu-mode-map>\\[package-menu-mark-install]' after `\\[list-packages]'."
  (interactive (list (not noninteractive)))
  (package-refresh-contents)
  (let ((upgradeable (package--upgradeable-packages)))
    (if (not upgradeable)
        (message "No packages to upgrade")
      (when (and query
                 (not (yes-or-no-p
                       (if (length= upgradeable 1)
                           "One package to upgrade.  Do it? "
                         (format "%s packages to upgrade.  Do it?"
                                 (length upgradeable))))))
        (user-error "Upgrade aborted"))
      (mapc #'package-upgrade upgradeable))))

(defun package--dependencies (pkg)
  "Return a list of all transitive dependencies of PKG.
If PKG is a package descriptor, the return value is a list of
package descriptors.  If PKG is a symbol designating a package,
the return value is a list of symbols designating packages."
  (when-let* ((desc (if (package-desc-p pkg) pkg
                      (cadr (assq pkg package-archive-contents)))))
    ;; Can we have circular dependencies?  Assume "nope".
    (let ((all (named-let more ((pkg-desc desc))
                 (let (deps)
                   (dolist (req (package-desc-reqs pkg-desc))
                     (setq deps (nconc
                                 (catch 'found
                                   (dolist (p (apply #'append (mapcar #'cdr (package--alist))))
                                     (when (and (string= (car req) (package-desc-name p))
                                                (version-list-<= (cadr req) (package-desc-version p)))
                                       (throw 'found (more p)))))
                                 deps)))
                   (delete-dups (cons pkg-desc deps))))))
      (remq pkg (mapcar (if (package-desc-p pkg) #'identity #'package-desc-name) all)))))

(defun package-buffer-info ()
  "Return a `package-desc' describing the package in the current buffer.

If the buffer does not contain a conforming package, signal an
error.  If there is a package, narrow the buffer to the file's
boundaries."
  (goto-char (point-min))
  (unless (re-search-forward "^;;; \\([^ ]*\\)\\.el ---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$" nil t)
    (error "Package lacks a file header"))
  (let ((file-name (match-string-no-properties 1))
        (desc      (match-string-no-properties 2)))
    (require 'lisp-mnt)
    (let* ((version-info (lm-package-version))
           (pkg-version (package-strip-rcs-id version-info))
           (keywords (lm-keywords-list))
           (website (lm-website)))
      (unless pkg-version
        (if version-info
            (error "Unrecognized package version: %s" version-info)
          (error "Package lacks a \"Version\" or \"Package-Version\" header")))
      (package-desc-from-define
       file-name pkg-version desc
       (lm-package-requires)
       :kind 'single
       :url website
       :keywords keywords
       :maintainer
       ;; For backward compatibility, use a single cons-cell if
       ;; there's only one maintainer (the most common case).
       (let ((maints (lm-maintainers))) (if (cdr maints) maints (car maints)))
       :authors (lm-authors)))))

(defun package-dir-info ()
  "Find package information for a directory.
The return result is a `package-desc'."
  (cl-assert (derived-mode-p 'dired-mode))
  (let* ((desc-file (package--description-file default-directory)))
    (if (file-readable-p desc-file)
        (with-temp-buffer
          (insert-file-contents desc-file)
          (package--read-pkg-desc 'dir))
      (catch 'found
        (let ((files (or (and (derived-mode-p 'dired-mode)
                              (dired-get-marked-files))
                         (directory-files-recursively default-directory "\\.el\\'"))))
          ;; We sort the file names in lexicographical order, to ensure
          ;; that we check shorter file names first (ie. those further
          ;; up in the directory structure).
          (dolist (file (sort files))
            ;; The file may be a link to a nonexistent file; e.g., a
            ;; lock file.
            (when (file-exists-p file)
              (with-temp-buffer
                (insert-file-contents file)
                ;; When we find the file with the data,
                (when-let* ((info (ignore-errors (package-buffer-info))))
                  (setf (package-desc-kind info) 'dir)
                  (throw 'found info))))))
        (error "No .el files with package headers in `%s'" default-directory)))))

;;;###autoload
(defun package-install-from-buffer ()
  "Install a package from the current buffer.
The current buffer is assumed to be a single .el or .tar file or
a directory.  These must follow the packaging guidelines (see
info node `(elisp)Packaging').

Specially, if current buffer is a directory, the -pkg.el
description file is not mandatory, in which case the information
is derived from the main .el file in the directory.  Using Dired,
you can restrict what files to install by marking specific files.

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
             ;; Package headers should be parsed from decoded text
             ;; (see Bug#48137) where possible.
             (if (and (eq buffer-file-coding-system 'no-conversion)
                      buffer-file-name)
                 (let* ((package-buffer (current-buffer))
                        (decoding-system
                         (car (find-operation-coding-system
                               'insert-file-contents
                               (cons buffer-file-name
                                     package-buffer)))))
                   (with-temp-buffer
                     (insert-buffer-substring package-buffer)
                     (decode-coding-region (point-min) (point-max)
                                           decoding-system)
                     (package-buffer-info)))

               (save-excursion
                 (package-buffer-info))))))
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
    (package--quickstart-maybe-refresh)
    pkg-desc))

;;;###autoload
(defun package-install-file (file)
  "Install a package from FILE.
The file can either be a tar file, an Emacs Lisp file, or a
directory."
  (interactive "fPackage file name: ")
  (with-temp-buffer
    (if (file-directory-p file)
        (progn
          (setq default-directory file)
          (dired-mode))
      (insert-file-contents-literally file)
      (set-visited-file-name file)
      (set-buffer-modified-p nil)
      (when (string-match "\\.tar\\'" file) (tar-mode)))
    (package-install-from-buffer)))



;;;###autoload
(defun package-install-selected-packages (&optional noconfirm)
  "Ensure packages in `package-selected-packages' are installed.
If some packages are not installed, propose to install them.

If optional argument NOCONFIRM is non-nil, or when invoked with a prefix
argument, don't ask for confirmation to install packages."
  (interactive "P")
  (package--archives-initialize)
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
        (when (or noconfirm
                  (y-or-n-p
                   (format "Packages to install: %d (%s), proceed? "
                           (length available)
                           (mapconcat #'symbol-name available " "))))
          (mapc (lambda (p) (package-install p 'dont-select)) available)))
       ((> difference 0)
        (message (substitute-command-keys
                  "Packages that are not available: %d (the rest is already \
installed), maybe you need to \\[package-refresh-contents]")
                 difference))
       (t
        (message "All your packages are already installed"))))))

(defun package--newest-p (pkg)
  "Return non-nil if PKG is the newest package with its name."
  (equal (cadr (assq (package-desc-name pkg) package-alist))
         pkg))

(declare-function comp-el-to-eln-filename "comp.c")
(defvar package-vc-repository-store)
(defun package--delete-directory (dir)
  "Delete PKG-DESC directory DIR recursively.
Clean-up the corresponding .eln files if Emacs is native
compiled."
  (when (featurep 'native-compile)
    (cl-loop
     for file in (directory-files-recursively dir
                                              ;; Exclude lockfiles
                                              (rx bos (or (and "." (not "#")) (not ".")) (* nonl) ".el" eos))
     do (comp-clean-up-stale-eln (comp-el-to-eln-filename file))))
  (if (file-symlink-p (directory-file-name dir))
      (delete-file (directory-file-name dir))
    (delete-directory dir t)))

(defun package-delete (pkg-desc &optional force nosave)
  "Delete package PKG-DESC.

Argument PKG-DESC is the full description of the package, for example as
obtained by `package-get-descriptor'.  Interactively, prompt the user
for the package name and version.

When package is used elsewhere as dependency of another package,
refuse deleting it and return an error.
If prefix argument FORCE is non-nil, package will be deleted even
if it is used elsewhere.
If NOSAVE is non-nil, the package is not removed from
`package-selected-packages'."
  (interactive
   (progn
     (let* ((package-table
             (mapcar
              (lambda (p) (cons (package-desc-full-name p) p))
              (delq nil
                    (mapcar (lambda (p) (unless (package-built-in-p p) p))
                            (apply #'append (mapcar #'cdr (package--alist)))))))
            (package-name (completing-read "Delete package: "
                                           (mapcar #'car package-table)
                                           nil t)))
       (list (cdr (assoc package-name package-table))
             current-prefix-arg nil))))
  (let* ((dir (package-desc-dir pkg-desc))
         (name (package-desc-name pkg-desc))
         (new-package-alist (let ((pkgs (assq name package-alist)))
                              (if (null (remove pkg-desc (cdr pkgs)))
                                  (remq pkgs package-alist)
                                package-alist)))
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
                      (let ((package-alist new-package-alist))
                        (package--used-elsewhere-p pkg-desc)))) ;See bug#65475
           ;; Don't delete packages used as dependency elsewhere.
           (error "Package `%s' is used by `%s' as dependency, not deleting"
                  (package-desc-full-name pkg-desc)
                  (package-desc-name pkg-used-elsewhere-by)))
          (t
           (add-hook 'post-command-hook 'package-menu--post-refresh)
           (package--delete-directory dir)
           ;; Remove NAME-VERSION.signed and NAME-readme.txt files.
           ;;
           ;; NAME-readme.txt files are no longer created, but they
           ;; may be left around from an earlier install.
           (dolist (suffix '(".signed" "readme.txt"))
             (let* ((version (package-version-join (package-desc-version pkg-desc)))
                    (file (concat (if (string= suffix ".signed")
                                      dir
                                    (substring dir 0 (- (length version))))
                                  suffix)))
               (when (file-exists-p file)
                 (delete-file file))))
           ;; Update package-alist.
           (setq package-alist new-package-alist)
           (package--quickstart-maybe-refresh)
           (message "Package `%s' deleted."
                    (package-desc-full-name pkg-desc))))))

;;;###autoload
(defun package-reinstall (pkg)
  "Reinstall package PKG.
PKG should be either a symbol, the package name, or a `package-desc'
object."
  (interactive
   (progn
     (package--archives-initialize)
     (list (intern (completing-read
                    "Reinstall package: "
                    (mapcar #'symbol-name
                            (mapcar #'car package-alist)))))))
  (package--archives-initialize)
  (package-delete
   (if (package-desc-p pkg) pkg (cadr (assq pkg package-alist)))
   'force 'nosave)
  (package-install pkg 'dont-select))

;;;###autoload
(defun package-autoremove (&optional noconfirm)
  "Remove packages that are no longer needed.

Packages that are no more needed by other packages in
`package-selected-packages' and their dependencies
will be deleted.

If optional argument NOCONFIRM is non-nil, or when invoked with a prefix
argument, don't ask for confirmation to install packages."
  (interactive "P")
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or noconfirm
            package-selected-packages
            (yes-or-no-p
             (format-message
              "`package-selected-packages' is empty! Really remove ALL packages? ")))
    (let ((removable (package--removable-packages)))
      (if removable
          (when (or noconfirm
                    (y-or-n-p
                     (format "Packages to delete: %d (%s), proceed? "
                             (length removable)
                             (mapconcat #'symbol-name removable " "))))
            (mapc (lambda (p)
                    (package-delete (cadr (assq p package-alist)) t))
                  removable))
        (message "Nothing to autoremove")))))


;;;; Autoload
(declare-function autoload-rubric "autoload" (file &optional type feature))

(defun package-autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (declare (obsolete nil "29.1"))
  (unless (file-exists-p file)
    (require 'autoload)
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (with-suppressed-warnings ((obsolete autoload-rubric))
        (write-region (autoload-rubric file "package" nil)
                      nil file nil 'silent))))
  file)

(defvar autoload-timestamps)
(defvar version-control)

(defun package-generate-autoloads (name pkg-dir)
  "Generate autoloads in PKG-DIR for package named NAME."
  (let* ((auto-name (format "%s-autoloads.el" name))
         ;;(ignore-name (concat name "-pkg.el"))
         (output-file (expand-file-name auto-name pkg-dir))
         ;; We don't need 'em, and this makes the output reproducible.
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (loaddefs-generate
     pkg-dir output-file nil
     (prin1-to-string
      '(add-to-list
        'load-path
        ;; Add the directory that will contain the autoload file to
        ;; the load path.  We don't hard-code `pkg-dir', to avoid
        ;; issues if the package directory is moved around.
        ;; `loaddefs-generate' has code to do this for us, but it's
        ;; not currently exposed.  (Bug#63625)
        (or (and load-file-name
                 (directory-file-name
                  (file-name-directory load-file-name)))
             (car load-path)))))
    (let ((buf (find-buffer-visiting output-file)))
      (when buf (kill-buffer buf)))
    auto-name))

(defun package--make-autoloads-and-stuff (pkg-desc pkg-dir)
  "Generate autoloads, description file, etc., for PKG-DESC installed at PKG-DIR."
  (package-generate-autoloads (package-desc-name pkg-desc) pkg-dir)
  (let ((desc-file (expand-file-name (package--description-file pkg-dir)
                                     pkg-dir)))
    (unless (file-exists-p desc-file)
      (package-generate-description-file pkg-desc desc-file)))
  ;; FIXME: Create foo.info and dir file from foo.texi?
  )

(defun package-tar-file-info ()
  "Find package information for a tar file.
The return result is a `package-desc'."
  (cl-assert (derived-mode-p 'tar-mode))
  (let* ((dir-name (named-let loop
                       ((filename (tar-header-name (car tar-parse-info))))
                     (let ((dirname (file-name-directory filename)))
                       ;; The first file can be in a subdir: look for the top.
                       (if dirname (loop (directory-file-name dirname))
                         (file-name-as-directory filename)))))
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

(defun package-untar-buffer (dir)
  "Untar the current buffer.
This uses `tar-untar-buffer' from Tar mode.  All files should
untar into a directory named DIR; otherwise, signal an error."
  (tar-mode)
  ;; Make sure everything extracts into DIR.
  (let ((regexp (concat "\\`" (regexp-quote (expand-file-name dir)) "/"))
        (case-fold-search (file-name-case-insensitive-p dir)))
    (dolist (tar-data tar-parse-info)
      (let ((name (expand-file-name (tar-header-name tar-data))))
        (or (string-match regexp name)
            ;; Tarballs created by some utilities don't list
            ;; directories with a trailing slash (Bug#13136).
            (and (string-equal (expand-file-name dir) name)
                 (eq (tar-header-link-type tar-data) 5))
            (error "Package does not untar cleanly into directory %s/" dir)))))
  (tar-untar-buffer))

(declare-function dired-get-marked-files "dired")

(defun package-unpack (pkg-desc)
  "Install the contents of the current buffer as a package."
  (let* ((name (package-desc-name pkg-desc))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (expand-file-name dirname package-user-dir)))
    (pcase (package-desc-kind pkg-desc)
      ('dir
       (make-directory pkg-dir t)
       (let ((file-list
              (or (and (derived-mode-p 'dired-mode)
                       (dired-get-marked-files))
                  (directory-files-recursively default-directory "" nil))))
         (dolist (source-file file-list)
           (let ((target (expand-file-name
                          (file-relative-name source-file default-directory)
                          pkg-dir)))
             (make-directory (file-name-directory target) t)
             (copy-file source-file target t)))
         ;; Now that the files have been installed, this package is
         ;; indistinguishable from a `tar' or a `single'. Let's make
         ;; things simple by ensuring we're one of them.
         (setf (package-desc-kind pkg-desc)
               (if (length> file-list 1) 'tar 'single))))
      ('tar
       (make-directory package-user-dir t)
       (let* ((default-directory (file-name-as-directory package-user-dir)))
         (package-untar-buffer dirname)))
      ('single
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
        (when package-native-compile
          (package--native-compile-async new-desc))
        ;; After compilation, load again any files loaded by
        ;; `activate-1', so that we use the byte-compiled definitions.
        (package--reload-previously-loaded new-desc)))
    pkg-dir))

(defun package-generate-description-file (pkg-desc pkg-file)
  "Create the foo-pkg.el file PKG-FILE for single-file package PKG-DESC."
  (let* ((name (package-desc-name pkg-desc)))
    (let ((print-level nil)
          (print-quoted t)
          (print-length nil))
      (write-region
       (concat
        ";;; Generated package description from "
        (replace-regexp-in-string "-pkg\\.el\\'" ".el"
                                  (file-name-nondirectory pkg-file))
        "  -*- no-byte-compile: t -*-\n"
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

;;;###autoload
(defun package-isolate (packages &optional temp-init)
  "Start an uncustomized Emacs and only load a set of PACKAGES.
Interactively, prompt for PACKAGES to load, which should be specified
separated by commas.
If called from Lisp, PACKAGES should be a list of packages to load.
If TEMP-INIT is non-nil, or when invoked with a prefix argument,
the Emacs user directory is set to a temporary directory.
This command is intended for testing Emacs and/or the packages
in a clean environment."
  (interactive
   (cl-loop for p in (cl-loop for p in (package--alist) append (cdr p))
	    unless (package-built-in-p p)
	    collect (cons (package-desc-full-name p) p) into table
	    finally return
	    (list
             (cl-loop for c in
                      (completing-read-multiple
                       "Packages to isolate: " table
                       nil t)
		           collect (alist-get c table nil nil #'string=))
                  current-prefix-arg)))
  (let* ((name (concat "package-isolate-"
                       (mapconcat #'package-desc-full-name packages ",")))
         (all-packages (delete-consecutive-dups
                        (sort (append packages (mapcan #'package--dependencies packages))
                              (lambda (p0 p1)
                                (string< (package-desc-name p0) (package-desc-name p1))))))
         initial-scratch-message package-load-list)
    (with-temp-buffer
      (insert ";; This is an isolated testing environment, with these packages enabled:\n\n")
      (dolist (package all-packages)
        (push (list (package-desc-name package)
                    (package-version-join (package-desc-version package)))
              package-load-list)
        (insert ";; - " (package-desc-full-name package))
        (unless (memq package packages)
          (insert " (dependency)"))
        (insert "\n"))
      (insert "\n")
      (setq initial-scratch-message (buffer-string)))
    (apply #'start-process (concat "*" name "*") nil
           (list (expand-file-name invocation-name invocation-directory)
                 "--quick" "--debug-init"
                 "--init-directory" (if temp-init
                                        (make-temp-file name t)
                                      user-emacs-directory)
                 (format "--eval=%S"
                         `(progn
                            (setq initial-scratch-message ,initial-scratch-message)

                            (require 'package)
                            ,@(mapcar
                               (lambda (dir)
                                 `(add-to-list 'package-directory-list ,dir))
                               (cons package-user-dir package-directory-list))
                            (setq package-load-list ',package-load-list)
                            (package-activate-all)))))))



(defun package--parse-elpaignore (pkg-desc)
  "Return a list of regular expressions to match files ignored by PKG-DESC."
  (let* ((pkg-dir (file-name-as-directory (package-desc-dir pkg-desc)))
         (ignore (expand-file-name ".elpaignore" pkg-dir))
         files)
    (when (file-exists-p ignore)
      (with-temp-buffer
        (insert-file-contents ignore)
        (goto-char (point-min))
        (while (not (eobp))
          (push (wildcard-to-regexp
                 (let ((line (buffer-substring
                              (line-beginning-position)
                              (line-end-position))))
                   (file-name-concat pkg-dir (string-trim-left line "/"))))
                files)
          (forward-line)))
      files)))

(defvar warning-minimum-level)
(defvar byte-compile-ignore-files)

(defun package--compile (pkg-desc)
  "Byte-compile installed package PKG-DESC.
This assumes that `pkg-desc' has already been activated with
`package-activate-1'."
  (let ((byte-compile-ignore-files (package--parse-elpaignore pkg-desc))
        (warning-minimum-level :error)
        (load-path load-path))
    (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))

(defun package--native-compile-async (pkg-desc)
  "Native compile installed package PKG-DESC asynchronously.
This assumes that `pkg-desc' has already been activated with
`package-activate-1'."
  (when (native-comp-available-p)
    (let ((warning-minimum-level :error))
      (native-compile-async (package-desc-dir pkg-desc) t))))

;;;###autoload
(defun package-recompile (pkg)
  "Byte-compile package PKG again.
PKG should be either a symbol, the package name, or a `package-desc'
object."
  (interactive (list (intern (completing-read
                              "Recompile package: "
                              (mapcar #'symbol-name
                                      (mapcar #'car package-alist))))))
  (let ((pkg-desc (if (package-desc-p pkg)
                      pkg
                    (cadr (assq pkg package-alist)))))
    ;; Delete the old .elc files to ensure that we don't inadvertently
    ;; load them (in case they contain byte code/macros that are now
    ;; invalid).
    (dolist (elc (directory-files-recursively
                  (package-desc-dir pkg-desc) "\\.elc\\'"))
      (delete-file elc))
    (package--compile pkg-desc)))

;;;###autoload
(defun package-recompile-all ()
  "Byte-compile all installed packages.
This is meant to be used only in the case the byte-compiled files
are invalid due to changed byte-code, macros or the like."
  (interactive)
  (pcase-dolist (`(_ ,pkg-desc) package-alist)
    (with-demoted-errors "Error while recompiling: %S"
      (package-recompile pkg-desc))))

(provide 'package-install)
;;; package-install.el ends here
