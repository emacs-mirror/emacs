;;; package-vc.el --- Manage packages from VC checkouts     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Keywords: tools

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

;; While packages managed by package.el use tarballs for distributing
;; the source code, this extension allows for packages to be fetched
;; and upgraded directly from a version control system.
;;
;; To install a package from source use `package-vc-install'.  If you
;; aren't interested in activating a package, you can use
;; `package-vc-checkout' instead, which will prompt you for a target
;; directory.  If you wish to reuse an existing checkout, the command
;; `package-vc-install-from-checkout' will prepare the package.
;;
;; If you make local changes that you wish to share with an upstream
;; maintainer, the command `package-vc-prepare-patch' can prepare
;; these as patches to send via Email.

;;; TODO:

;; - Allow maintaining patches that are ported back onto regular
;;   packages and maintained between versions.

;;; Code:

(eval-when-compile (require 'rx))
(eval-when-compile (require 'map))
(eval-when-compile (require 'cl-lib))
(require 'package)
(require 'lisp-mnt)
(require 'vc)
(require 'seq)

(defgroup package-vc nil
  "Manage packages from VC checkouts."
  :group 'package
  :link '(custom-manual "(emacs) Fetching Package Sources")
  :prefix "package-vc-"
  :version "29.1")

(defconst package-vc--elpa-packages-version 1
  "Version number of the package specification format understood by package-vc.")

(define-obsolete-variable-alias
  'package-vc-heuristic-alist
  'vc-clone-heuristic-alist "31.1")

(defcustom package-vc-default-backend 'Git
  "Default VC backend to use for cloning package repositories.
`package-vc-install' uses this backend when you specify neither
the backend nor a repository URL that's recognized via
`vc-clone-heuristic-alist'.

The value must be a member of `vc-handled-backends' that supports
the `clone' VC function."
  :type vc-clonable-backends-custom-type
  :version "29.1")

(defcustom package-vc-register-as-project t
  "Non-nil means that packages should be registered as projects."
  :type 'boolean
  :version "30.1")

(defvar package-vc-selected-packages) ; pacify byte-compiler

;;;###autoload
(defun package-vc-install-selected-packages ()
  "Ensure packages specified in `package-vc-selected-packages' are installed."
  (interactive)
  (pcase-dolist (`(,name . ,spec) package-vc-selected-packages)
    (when (stringp name)
      (setq name (intern name)))
    (let ((pkg-descs (assoc name package-alist #'string=)))
      (unless (seq-some #'package-vc-p (cdr pkg-descs))
        (cond
         ((null spec)
          (package-vc-install name))
         ((stringp spec)
          (package-vc-install name spec))
         ((listp spec)
          (package-vc--archives-initialize)
          (package-vc--unpack
           (or (cadr (assoc name package-archive-contents))
               (package-desc-create :name name :kind 'vc))
           spec)))))))


(defcustom package-vc-selected-packages nil
  "List of packages to install from their VCS repositories.
Each element is of the form (NAME . SPEC), where NAME is a symbol
designating the package and SPEC is one of:

- nil, if any package version can be installed;
- a version string, if that specific revision is to be installed;
- a property list, describing a package specification.  For possible
  values, see the subsection \"Specifying Package Sources\" in the
  Info node `(emacs)Fetching Package Sources'.

The command `package-vc-install' updates the value of this user
option to store package specifications for packages that are not
specified in any archive."
  :type '(alist :tag "List of packages you want to be installed"
                :key-type (symbol :tag "Package")
                :value-type
                (choice (const :tag "Any revision" nil)
                        (string :tag "Specific revision")
                        (plist :options ((:url string)
                                         (:branch string)
                                         (:lisp-dir string)
                                         (:main-file string)
                                         (:doc string)
                                         (:vc-backend symbol)))))
  :version "29.1")

(defvar package-vc--archive-spec-alists nil
  "List of package specifications for each archive.
The list maps each package name, as a string, to a plist as
specified in `package-vc-selected-packages'.")

(defvar package-vc--archive-data-alist nil
  "List of package specification metadata for archives.
Each element of the list has the form (ARCHIVE . PLIST), where
PLIST keys are one of:

 `:version' (integer)
   Indicates the version of the file formatting, to be compared
   with `package-vc--elpa-packages-version'.

 `:vc-backend' (symbol)
   A symbol of the default VC backend to use if a package specification
   does not indicate a backend.  The value ought to be a member of
   `vc-handled-backends'.  If omitted, `vc-clone' will fall back on
   `package-vc-default-backend'.

All other values are ignored.")

(defun package-vc--desc->spec (pkg-desc &optional name)
  "Retrieve the package specification for PKG-DESC.
The optional argument NAME can be used to override the default
name for PKG-DESC."
  (alist-get
   (setq name (or name (package-desc-name pkg-desc)))
   (if (and (package-desc-archive pkg-desc)
            (not (alist-get name package-vc-selected-packages
                            nil nil #'string=)))
       (alist-get (intern (package-desc-archive pkg-desc))
                  package-vc--archive-spec-alists)
     ;; Consult both our local list of package specifications, as well
     ;; as the lists provided by the archives.
     (apply #'append (cons package-vc-selected-packages
                           (mapcar #'cdr package-vc--archive-spec-alists))))
   '() nil #'string=))

(defun package-vc--checkout-dir (pkg-desc &optional lisp-dir)
  "Return the directory of the actual VC checkout for PKG-DESC.
For most packages this is the same as `package-desc-dir', unless the
package has been installed via `package-vc-install-from-checkout'.  In
that case the package redirects to the actual VC checkout.  If the
optional LISP-DIR argument is non-nil, then check if a related package
specification has a `:lisp-dir' field to indicate that Lisp files are
located in a sub directory of the checkout, or the checkout has a sub
directory named \"lisp\" or \"src\" that contains .el files and return
that instead."
  (let* ((pkg-spec (package-vc--desc->spec pkg-desc))
         (pkg-dir (or (alist-get :vc-dir (package-desc-extras pkg-desc))
                      (package-desc-dir pkg-desc))))
    (expand-file-name
     (or (and lisp-dir
              (or (plist-get pkg-spec :lisp-dir)
                  ;; When nothing is specified about a `lisp-dir', then
                  ;; should heuristically check if there is a
                  ;; sub-directory with lisp files.  These are
                  ;; conventionally just called "lisp" or "src".  If
                  ;; this directory exists and contains non-zero number
                  ;; of lisp files, we will use that instead of
                  ;; `pkg-dir'.
                  (catch 'done
                    (dolist (name '("lisp" "src"))
                      (when-let* ((dir (expand-file-name name pkg-dir))
                                  ((file-directory-p dir))
                                  ((directory-files
                                    dir nil "\\`[^.].+\\.el\\'" t 1)))
                        ;; We won't use `dir', since dir is an absolute
                        ;; path and we don't want `lisp-dir' to depend
                        ;; on the current location of the package
                        ;; installation, ie. to break if moved around
                        ;; the file system or between installations.
                        (throw 'done name))))))
         ".")
     pkg-dir)))

(defun package-vc--read-archive-data (archive)
  "Update `package-vc--archive-spec-alists' for ARCHIVE.
This function is meant to be used as a hook for `package-read-archive-hook'."
  (let ((contents-file (expand-file-name
                        (format "archives/%s/elpa-packages.eld" archive)
                        package-user-dir)))
    (when (file-exists-p contents-file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents contents-file)
          ;; The response from the server is expected to have the form
          ;;
          ;;    ((("foo" :url "..." ...) ...)
          ;;     :version 1
          ;;     :default-vc Git)
          (let ((spec (read (current-buffer))))
            (when (eq package-vc--elpa-packages-version
                      (plist-get (cdr spec) :version))
              (setf (alist-get (intern archive) package-vc--archive-spec-alists)
                    (car spec)))
            (setf (alist-get (intern archive) package-vc--archive-data-alist)
                  (cdr spec))
            (when-let* ((default-vc (plist-get (cdr spec) :default-vc))
                        ((not (memq default-vc vc-handled-backends))))
              (warn "Archive `%S' expects missing VC backend %S"
                    archive (plist-get (cdr spec) :default-vc)))))))))

(defun package-vc--download-and-read-archives (&optional async)
  "Download specifications of all `package-archives' and read them.
Populate `package-vc--archive-spec-alists' with the result.

If optional argument ASYNC is non-nil, perform the downloads
asynchronously."
  (dolist (archive package-archives)
    (condition-case err
        (package--download-one-archive archive "elpa-packages.eld" async)
      (error (message "Failed to download `%s' archive: %S" (car archive) err)))))

(add-hook 'package-read-archive-hook     #'package-vc--read-archive-data 20)

(defun package-vc-commit (pkg-desc)
  "Return the last commit of a development package PKG-DESC."
  (cl-assert (package-vc-p pkg-desc))
  ;; FIXME: vc should be extended to allow querying the commit of a
  ;; directory (as is possible when dealing with git repositories).
  ;; This should be a fallback option.
  (cl-loop with dir = (package-vc--checkout-dir pkg-desc 'lisp-dir)
           for file in (directory-files dir t "\\.el\\'" t)
           when (vc-working-revision file) return it
           finally return "unknown"))

(defun package-vc--version (pkg)
  "Return the version number for the VC package PKG."
  (cl-assert (package-vc-p pkg))
  (if-let* ((main-file (package-vc--main-file pkg)))
      (with-temp-buffer
        (insert-file-contents main-file)
        (package-strip-rcs-id
         (or (lm-header "package-version")
             (lm-header "version")
             "0")))
    "0"))

(defun package-vc--main-file (pkg-desc)
  "Return the name of the main file for PKG-DESC."
  (cl-assert (package-vc-p pkg-desc))
  (let* ((pkg-spec (package-vc--desc->spec pkg-desc))
         (name (symbol-name (package-desc-name pkg-desc)))
         (directory (package-vc--checkout-dir pkg-desc 'lisp-dir))
         (file (expand-file-name
                (or (plist-get pkg-spec :main-file)
                    (concat name ".el"))
                directory)))
    (if (file-exists-p file) file
      ;; The following heuristic is only necessary when fetching a
      ;; repository with URL that would break the above assumptions.
      ;; Concrete example: https://github.com/sachac/waveform-el does
      ;; not have a file waveform-el.el, but a file waveform.el, so we
      ;; try and find the closest match.
      (let ((distance most-positive-fixnum) (best nil))
        (dolist (alt (directory-files directory t "\\.el\\'" t))
          (let ((sd (string-distance file alt)))
            (when (and (not (string-match-p (rx (or (: "-autoloads.el")
                                                    (: "-pkg.el"))
                                                eos)
                                            alt))
                       (< sd distance))
              (when (< sd distance)
                (setq distance (string-distance file alt)
                      best alt)))))
        best))))

(defun package-vc--generate-description-file (pkg-desc pkg-file)
  "Generate a package description file for PKG-DESC and write it to PKG-FILE."
  (let ((name (package-desc-name pkg-desc))
        (main-file (let ((file (package-vc--main-file pkg-desc)))
                     (and (file-exists-p file) file))))
    (when (equal (package-desc-summary pkg-desc) package--default-summary)
      ;; We unset the package description if it is just the default
      ;; summary, so that the following heuristic can take effect.
      (setf (package-desc-summary pkg-desc) nil))
    ;; Infer the package description if missing.
    (unless (package-desc-summary pkg-desc)
      (setf (package-desc-summary pkg-desc)
            (or (package-desc-summary pkg-desc)
                (and-let* ((pkg (cadr (assq name package-archive-contents))))
                  (package-desc-summary pkg))
                (and main-file
                     (lm-summary main-file))
                package--default-summary)))
    (let ((print-level nil)
          (print-quoted t)
          (print-length nil))
      (write-region
       (concat
        ";;; Generated package description from "
        (replace-regexp-in-string
         "-pkg\\.el\\'" ".el"
         (file-name-nondirectory pkg-file))
        "  -*- no-byte-compile: t; lexical-binding: t -*-\n"
        (prin1-to-string
         (nconc
          (list 'define-package
                (symbol-name name)
                (package-vc--version pkg-desc)
                (package-desc-summary pkg-desc)
                (let ((requires (package-desc-reqs pkg-desc)))
                  (list 'quote
                        ;; Turn version lists into string form.
                        (mapcar
                         (lambda (elt)
                           (list (car elt)
                                 (package-version-join (cadr elt))))
                         requires))))
          (list :kind 'vc)
          (package--alist-to-plist-args
           (let ((extras (copy-alist (package-desc-extras pkg-desc))))
             (setf (alist-get :commit extras)
                   (package-vc-commit pkg-desc))
             (when-let* (((null (alist-get :maintainer extras)))
                         (main-file)
                         (maintainers (lm-maintainers main-file)))
               ;; Like in `package-buffer-info', for backward
               ;; compatibility, use a single cons-cell if there's
               ;; only one maintainer.
               (setf (alist-get :maintainer extras)
                     (if (cdr maintainers)
                         maintainers
                       (car maintainers))))
             extras)
           )))
        "\n")
       nil pkg-file nil 'silent))))

(defcustom package-vc-make-program nil
  "Name of the GNU \"make\" executable on the system.

If the name of the GNU \"make\" executable on the current system is
neither \"make\" nor \"gmake\" then you will need to customize this
variable in order to build some VC packages."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Name of GNU 'make' on current system"))
  :version "31.1")

(defcustom package-vc-allow-build-commands nil
  "Whether to run extra build commands when installing VC packages.

Some packages specify \"make\" targets or other shell commands
that should run prior to building the package, by including the
:make or :shell-command keywords in their specification.  By
default, Emacs ignores these keywords when installing and
upgrading VC packages, but if the value is a list of package
names (symbols), the build commands will be run for those
packages.  If the value is t, always respect :make and
:shell-command keywords.

It may be necessary to run :make and :shell-command arguments in
order to initialize a package or build its documentation, but
please be careful when changing this option, as installing and
updating a package can run potentially harmful code.

This applies to package specifications that come from your
configured package archives, as well as from entries in
`package-vc-selected-packages' and specifications that you give
to `package-vc-install' directly."
  :type '(choice (const :tag "Run for all packages" t)
                 (repeat :tag "Run only for selected packages" (symbol :tag "Package name"))
                 (const :tag "Never run" nil))
  :version "30.1")

(defun package-vc--make (pkg-spec pkg-desc)
  "Process :make and :shell-command in PKG-SPEC.
PKG-DESC is the package descriptor for the package that is being
prepared."
  (let ((default-directory (package-vc--checkout-dir pkg-desc))
        (target (plist-get pkg-spec :make))
        (cmd (plist-get pkg-spec :shell-command))
        (buf (format " *package-vc make: %s*" (package-desc-name pkg-desc)))
        (makexe (or package-vc-make-program
                    (seq-find #'executable-find '("gmake" "make")))))
    (when (or cmd target)
      (with-current-buffer (get-buffer-create buf)
        (erase-buffer)
        (when (and cmd (/= 0 (call-process shell-file-name nil t nil shell-command-switch cmd)))
          (warn "Failed to run %s, see buffer %S" cmd (buffer-name)))
        (when (and target (/= 0 (apply #'call-process makexe nil t nil (ensure-list target))))
          (warn "Failed to make %s, see buffer %S" target (buffer-name)))))))

(declare-function org-export-to-file "ox" (backend file))

(defun package-vc--build-documentation (pkg-desc file)
  "Build documentation for package PKG-DESC from documentation source in FILE.
FILE can be an Org file, indicated by its \".org\" extension,
otherwise it's assumed to be an Info file."
  (let* ((pkg-name (package-desc-name pkg-desc))
         (default-directory (package-vc--checkout-dir pkg-desc))
         (docs-directory (file-name-directory (expand-file-name file)))
         (output (expand-file-name (format "%s.info" (file-name-base file))))
         (log-buffer (get-buffer-create (format " *package-vc doc: %s*" pkg-name)))
         clean-up)
    (with-current-buffer log-buffer
      (erase-buffer))
    (condition-case err
        (progn
          (when (string-match-p "\\.org\\'" file)
            (require 'ox)
            (require 'ox-texinfo)
            (with-temp-buffer
              (insert-file-contents file)
              (setq file (make-temp-file "ox-texinfo-"))
              (let ((default-directory docs-directory))
                (org-export-to-file 'texinfo file))
              (setq clean-up t)))
          (cond
           ((/= 0 (call-process "makeinfo" nil log-buffer nil
                                "-I" docs-directory
                                "--no-split" file
                                "-o" output))
            (message "Failed to build manual %s, see buffer %S"
                     file (buffer-name log-buffer)))
           ((/= 0 (call-process "install-info" nil log-buffer nil
                                output (expand-file-name "dir")))
            (message "Failed to install manual %s, see buffer %S"
                     output (buffer-name log-buffer)))
           ((kill-buffer log-buffer))))
      (error (with-current-buffer log-buffer
               (insert (error-message-string err)))
             (message "Failed to export org manual for %s, see buffer %S"
                      pkg-name (buffer-name log-buffer))))
    (when clean-up
      (delete-file file))))

(defun package-vc-install-dependencies (deps)
  "Install missing dependencies according to DEPS.

DEPS is a list of elements (PACKAGE VERSION-LIST), where
PACKAGE is a package name and VERSION-LIST is the required
version of that package.

Return a list of dependencies that couldn't be met (or nil, when
this function successfully installs all given dependencies)."
  (let ((to-install '()) (missing '()))
    (cl-labels ((search (pkg)
                  "Attempt to find all dependencies for PKG."
                  (cond
                   ((assq (car pkg) to-install)) ;inhibit cycles
                   ((package-installed-p (car pkg) (cadr pkg)))
                   ((let* ((pac package-archive-contents)
                           (desc (cadr (assoc (car pkg) pac))))
                      (if desc
                          (let ((reqs (package-desc-reqs desc)))
                            (push desc to-install)
                            (mapc #'search reqs))
                        (push pkg missing))))))
                (version-order (a b)
                  "Predicate to sort packages in order."
                  (version-list-<
                   (package-desc-version b)
                   (package-desc-version a)))
                (duplicate-p (a b)
                  "Are A and B the same package?"
                  (eq (package-desc-name a) (package-desc-name b)))
                (depends-on-p (target package)
                  "Does PACKAGE depend on TARGET?"
                  (or (eq target package)
                      (let* ((pac package-archive-contents)
                             (desc (cadr (assoc package pac))))
                        (and desc (seq-some
                                   (apply-partially #'depends-on-p target)
                                   (mapcar #'car (package-desc-reqs desc)))))))
                (dependent-order (a b)
                  (let ((desc-a (package-desc-name a))
                        (desc-b (package-desc-name b)))
                    (depends-on-p desc-a desc-b))))
      (mapc #'search deps)
      (cl-callf sort to-install #'version-order)
      (cl-callf seq-uniq to-install #'duplicate-p)
      (cl-callf sort to-install #'dependent-order))
    (mapc #'package-install-from-archive to-install)
    missing))

(defun package-vc--unpack-1 (pkg-desc)
  "Prepare PKG-DESC that is already checked-out.
When there's a relevant pkg-spec it is used for checkout directory.
Otherwise `dir' slot of PKG-SPEC is used.  This includes downloading
missing dependencies, generating autoloads, generating a package
description file (used to identify a package as a VC package later on),
building documentation and marking the package as installed."
  (let* (;; Main package directory, under `package-user-dir'.  This is
         ;; the same `checkout-dir' when package has been installed with
         ;; `package-vc-install'.
         (pkg-dir (package-desc-dir pkg-desc))
         (pkg-spec (package-vc--desc->spec pkg-desc))
         ;; Directory where the package repository has been checked out.
         ;; This is the `dir' argument of
         ;; `package-vc-install-from-checkout'.
         (checkout-dir (package-vc--checkout-dir pkg-desc))
         ;; Directory where package's Lisp code resides.  It may be
         ;; equal to `checkout-dir' or be a subdirectory of it.
         (lisp-dir (package-vc--checkout-dir pkg-desc 'lisp-dir))
         missing)

    ;; In case the package was installed directly from source, the
    ;; dependency list wasn't know beforehand, and they might have
    ;; to be installed explicitly.
    (let ((ignored-files
           (if (plist-get pkg-spec :ignored-files)
               (mapconcat
                (lambda (ignore)
                  (wildcard-to-regexp
                   (if (string-match-p "\\`/" ignore)
                       (concat checkout-dir ignore)
                     (concat "*/" ignore))))
                (plist-get pkg-spec :ignored-files)
                "\\|")
             regexp-unmatchable))
          (deps '()))
      (dolist (file (directory-files lisp-dir t "\\.el\\'" t))
        (unless (string-match-p ignored-files file)
          (with-temp-buffer
            (insert-file-contents file)
            (when-let* ((require-lines (lm-header-multiline "package-requires")))
              (thread-last
                (mapconcat #'identity require-lines " ")
                package-read-from-string
                lm--prepare-package-dependencies
                (nconc deps)
                (setq deps))))))
      (dolist (dep deps)
        (cl-callf version-to-list (cadr dep)))
      (setf (package-desc-reqs pkg-desc) deps)
      (setf missing (package-vc-install-dependencies (delete-dups deps)))
      (setf missing (delq (assq (package-desc-name pkg-desc)
                                missing)
                          missing)))

    ;; Generate autoloads
    (let* ((name (package-desc-name pkg-desc))
           (auto-name (format "%s-autoloads.el" name)))
      (package-generate-autoloads name lisp-dir)
      ;; There are two cases when we wish to "indirect" the loading of
      ;; autoload files:
      ;;
      ;; 1. a package specification has a `:lisp-dir' entry listing
      ;; indicting that the actual Lisp code is located in a
      ;; subdirectory of the checkout,
      ;;
      ;; 2. the package has been installed using
      ;; `package-vc-install-from-checkout' and we want to load the
      ;; other directory instead -- which is outside of the checkout.
      ;; We can therefore take file inequality as a sign that we have to
      ;; set up an indirection.
      (unless (file-equal-p lisp-dir pkg-dir)
        (write-region
         (concat
          ";; Autoload indirection for package-vc -*- lexical-binding: t -*-\n\n"
          (prin1-to-string
           ;; The indirection is just a single load statement to the
           ;; actual file (we don't want to use symbolic links due to
           ;; portability reasons).  Detecting which of the two cases
           ;; mentioned above we are setting up can be done by checking
           ;; if the directory with the lisp code is a subdirectory of
           ;; the package directory.
           `(load ,(if (file-in-directory-p lisp-dir pkg-dir)
                       `(expand-file-name
                         ,(file-relative-name
                           (expand-file-name auto-name lisp-dir)
                           pkg-dir)
                         (or (and load-file-name
                                  (file-name-directory load-file-name))
                             (car load-path)))
                     (expand-file-name auto-name lisp-dir)))))
         nil (expand-file-name auto-name pkg-dir))))

    ;; Generate package file
    (let ((pkg-file (expand-file-name (package--description-file pkg-dir) pkg-dir)))
      (package-vc--generate-description-file pkg-desc pkg-file))

    ;; Process :make and :shell-command arguments before building documentation
    (when (or (eq package-vc-allow-build-commands t)
              (memq (package-desc-name pkg-desc)
                    package-vc-allow-build-commands))
      (package-vc--make pkg-spec pkg-desc))

    ;; Detect a manual
    (when (executable-find "install-info")
      (dolist (doc-file (ensure-list (plist-get pkg-spec :doc)))
        (package-vc--build-documentation pkg-desc doc-file)))

    ;; Remove any previous instance of PKG-DESC from `package-alist'
    (let ((pkgs (assq (package-desc-name pkg-desc) package-alist)))
      (when pkgs
        (setf (cdr pkgs) (seq-remove #'package-vc-p (cdr pkgs)))))

    ;; Remove all compiled files to allow for macros to be used from
    ;; source files, regardless of order of source files compilation and
    ;; load ordering.  As a side effect there are no compiled files for
    ;; source files that no longer exist.
    (dolist (elc-file (directory-files-recursively
                       lisp-dir
                       (rx string-start
                           (not ".") (zero-or-more anychar) ".elc"
                           string-end)
                       nil
                       (lambda (dir)
                         (and (file-accessible-directory-p dir)
                              (not (string-prefix-p "." dir))))))
        (delete-file elc-file))

    ;; Update package-alist.
    (let* ((new-desc (package-load-descriptor pkg-dir))
           (compile-desc (package-desc-create :name (package-desc-name new-desc)
                                              :dir lisp-dir)))
      ;; Activation has to be done before compilation, so that if we're
      ;; upgrading and macros have changed we load the new definitions
      ;; before compiling.
      (when (package-activate-1 new-desc :reload :deps)
        ;; `package-activate-1' will reload all necessary package files
        ;; as long as their stems are relative to of `pkg-dir'.  If
        ;; that's not the case (for example for packages with different
        ;; `checkout-dir' or with source files in a sub directory of
        ;; `pkg-dir'), we want to reload package files  from the
        ;; `lisp-dir' before compilation.
        (unless (file-equal-p lisp-dir pkg-dir)
          (package--reload-previously-loaded compile-desc))
        ;; `package-activate-1' will add info node as long as dir file
        ;; exists in `pkg-dir'.  We need to manually add it when
        ;; `checkout-dir' is in different location.
        (unless (file-equal-p checkout-dir pkg-dir)
          (package--add-info-node checkout-dir))
        ;; FIXME: Compilation should be done as a separate, optional, step.
        ;; E.g. for multi-package installs, we should first install all packages
        ;; and then compile them.
        (package--compile compile-desc)
        (when package-native-compile
          (package--native-compile-async compile-desc))
        ;; After compilation, load again any files loaded by
        ;; `package-activate-1', so that we use the byte-compiled
        ;; definitions.  This time we'll use `compile-desc' straight
        ;; away.
        (package--reload-previously-loaded compile-desc)))

    ;; Mark package as selected
    (let ((name (package-desc-name pkg-desc)))
      (unless (memq name package-selected-packages)
        (package--save-selected-packages
         (cons name package-selected-packages))))

    (package--quickstart-maybe-refresh)

    ;; Confirm that the installation was successful
    (let ((main-file (package-vc--main-file pkg-desc)))
      (message "VC package `%s' installed (Version %s, Revision %S).%s"
               (package-desc-name pkg-desc)
               (lm-with-file main-file
                 (package-strip-rcs-id
                  (or (lm-header "package-version")
                      (lm-header "version"))))
               (vc-working-revision main-file)
               (if missing
                   (format
                    " Failed to install the following dependencies: %s"
                    (mapconcat
                     (lambda (p)
                       (format "%s (%s)" (car p) (cadr p)))
                     missing ", "))
                 "")))
    t))

(declare-function project-remember-projects-under "project" (dir &optional recursive))

(defun package-vc--clone (pkg-desc pkg-spec dir rev)
  "Clone the package PKG-DESC whose spec is PKG-SPEC into the directory DIR.
REV specifies a specific revision to checkout.  This overrides the `:branch'
attribute in PKG-SPEC."
  (pcase-let* ((name (package-desc-name pkg-desc))
               ((map :url :branch) pkg-spec))

    ;; Clone the repository into `repo-dir' if necessary
    (unless (file-exists-p dir)
      (make-directory (file-name-directory dir) t)
      (let ((backend (or (plist-get pkg-spec :vc-backend)
                         (vc-guess-url-backend url)
                         (plist-get (alist-get (package-desc-archive pkg-desc)
                                               package-vc--archive-data-alist
                                               nil nil #'string=)
                                    :vc-backend)
                         package-vc-default-backend)))
        (unless (vc-clone url backend dir
                          (or (and (not (eq rev :last-release)) rev) branch))
          (error "Failed to clone %s from %s" name url))))

    (when package-vc-register-as-project
      (let ((default-directory dir))
        (require 'project)
        (project-remember-projects-under dir)))

    ;; Check out the latest release if requested
    (when (eq rev :last-release)
      (if-let* ((release-rev (package-vc--release-rev pkg-desc)))
          (vc-retrieve-tag dir release-rev)
        (message "No release revision was found, continuing...")))))

(defvar package-vc-non-code-file-names
  '(".dir-locals.el" ".dir-locals-2.el")
  "List of file names that do not contain Emacs Lisp code.
This list is used by `package-vc--unpack' to better check if the
user is fetching code from a repository that does not contain any
Emacs Lisp files.")

(defun package-vc--unpack (pkg-desc pkg-spec &optional rev)
  "Install the package described by PKG-DESC.
PKG-SPEC is a package specification, a property list describing
how to fetch and build the package.  See `package-vc--archive-spec-alists'
for details.  The optional argument REV specifies a specific revision to
checkout.  This overrides the `:branch' attribute in PKG-SPEC."
  (unless (eq (package-desc-kind pkg-desc) 'vc)
    (let ((copy (copy-package-desc pkg-desc)))
      (setf (package-desc-kind copy) 'vc
            pkg-desc copy)))
  (let* ((name (package-desc-name pkg-desc))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (file-name-as-directory (expand-file-name dirname package-user-dir))))
    (when (string-empty-p name)
      (user-error "Empty package name"))
    (setf (package-desc-dir pkg-desc) pkg-dir)
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p (format "Overwrite previous checkout for package `%s'?" name))
          (package--delete-directory pkg-dir)
        (error "There already exists a checkout for %s" name)))
    (package-vc--clone pkg-desc pkg-spec pkg-dir rev)
    (when (directory-empty-p pkg-dir)
      (delete-directory pkg-dir)
      (error "Empty checkout for %s" name))
    (unless (seq-remove
             (lambda (file)
               (member (file-name-nondirectory file) package-vc-non-code-file-names))
             (directory-files-recursively pkg-dir "\\.el\\'" nil))
      (when (yes-or-no-p (format "No Emacs Lisp files found when fetching \"%s\", \
abort installation?" name))
        (delete-directory pkg-dir t)
        (user-error "Installation aborted")))

    ;; Ensure we have a copy of the package specification
    (when (null (package-vc--desc->spec pkg-desc name))
      (customize-save-variable
       'package-vc-selected-packages
       (cons (cons name pkg-spec)
             (seq-remove (lambda (spec) (string= name (car spec)))
                         package-vc-selected-packages))))

    (package-vc--unpack-1 pkg-desc)))

(defun package-vc--read-package-name (prompt &optional allow-url installed)
  "Query the user for a VC package and return a name with PROMPT.
If the optional argument ALLOW-URL is non-nil, the user is also
allowed to specify a non-package name.  If the optional argument
INSTALLED is non-nil, the selection will be filtered down to
VC packages that have already been installed."
  (package-vc--archives-initialize)
  (completing-read prompt (if installed package-alist package-archive-contents)
                   (if installed
                       (lambda (pkg) (package-vc-p (cadr pkg)))
                     (lambda (pkg)
                       (or (package-vc--desc->spec (cadr pkg))
                           ;; If we have no explicit VC data, we can try a kind of
                           ;; heuristic and use the URL header, that might already be
                           ;; pointing towards a repository, and use that as a backup
                           (and-let* ((extras (package-desc-extras (cadr pkg)))
                                      (url (alist-get :url extras))
                                      ((vc-guess-url-backend url)))))))
                   (not allow-url)))

(defun package-vc--read-package-desc (prompt &optional installed)
  "Query the user for a VC package and return a description with PROMPT.
If the optional argument INSTALLED is non-nil, the selection will
be filtered down to VC packages that have already been
installed, and the package description will be that of an
installed package."
  (cadr (assoc (package-vc--read-package-name prompt nil installed)
               (if installed package-alist package-archive-contents)
               #'string=)))

;;;###autoload
(defun package-vc-upgrade-all ()
  "Upgrade all installed VC packages.

This may fail if the local VCS state of one of the packages
conflicts with its remote repository state."
  (interactive)
  (dolist (package package-alist)
    (dolist (pkg-desc (cdr package))
      (when (package-vc-p pkg-desc)
        (package-vc-upgrade pkg-desc))))
  (message "Done upgrading packages."))

(declare-function vc-dir-prepare-status-buffer "vc-dir"
                  (bname dir backend &optional create-new))

;;;###autoload
(defun package-vc-upgrade (pkg-desc)
  "Upgrade the package described by PKG-DESC from package's VC repository.

This may fail if the local VCS state of the package conflicts
with the remote repository state."
  (interactive (list (package-vc--read-package-desc "Upgrade VC package: " t)))
  ;; HACK: To run `package-vc--unpack-1' after checking out the new
  ;; revision, we insert a hook into `vc-post-command-functions', and
  ;; remove it right after it ran.  To avoid running the hook multiple
  ;; times or even for the wrong repository (as `vc-pull' is often
  ;; asynchronous), we extract the relevant arguments using a pseudo
  ;; filter for `vc-filter-command-function', executed only for the
  ;; side effect, and store them in the lexical scope.  When the hook
  ;; is run, we check if the arguments are the same (`eq') as the ones
  ;; previously extracted, and only in that case will be call
  ;; `package-vc--unpack-1'.  Ugh...
  ;;
  ;; If there is a better way to do this, it should be done.
  (cl-assert (package-vc-p pkg-desc))
  (letrec ((checkout-dir (package-vc--checkout-dir pkg-desc))
           (vc-flags)
           (vc-filter-command-function
            (lambda (command file-or-list flags)
              (setq vc-flags flags)
              (list command file-or-list flags)))
           (post-upgrade
            (lambda (_command _file-or-list flags)
              (when (and (file-equal-p checkout-dir default-directory)
                         (eq flags vc-flags))
                (unwind-protect
                    (with-demoted-errors "Failed to activate: %S"
                      (package-vc--unpack-1 pkg-desc))
                  (remove-hook 'vc-post-command-functions post-upgrade))))))
    (add-hook 'vc-post-command-functions post-upgrade)
    (with-demoted-errors "Failed to fetch: %S"
      (require 'vc-dir)
      (with-current-buffer (vc-dir-prepare-status-buffer
                            (format " *package-vc-dir: %s*" checkout-dir)
                            checkout-dir
                            (vc-responsible-backend checkout-dir))
        (vc-pull)))))

(defun package-vc--archives-initialize ()
  "Initialize package.el and fetch package specifications."
  (package--archives-initialize)
  (unless package-vc--archive-data-alist
    (package-vc--download-and-read-archives)))

(defun package-vc--release-rev (pkg-desc)
  "Return the latest revision that bumps the \"Version\" tag for PKG-DESC.
If no such revision can be found, return nil."
  (with-current-buffer (find-file-noselect (package-vc--main-file pkg-desc))
    (vc-buffer-sync)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (cond
               ((re-search-forward
                 (concat (lm-get-header-re "package-version") ".*$")
                 (lm-code-start) t))
               ((re-search-forward
                 (concat (lm-get-header-re "version") ".*$")
                 (lm-code-start) t)))
          (ignore-error vc-not-supported
            (vc-call-backend (vc-backend (buffer-file-name))
                             'last-change
                             (buffer-file-name)
                             (line-number-at-pos nil t))))))))

;;;###autoload
(defun package-vc-install (package &optional rev backend name)
  "Fetch a package described by PACKAGE and set it up for use with Emacs.

PACKAGE specifies which package to install, where to find its
source repository and how to build it.

If PACKAGE is a symbol, install the package with that name
according to metadata that package archives provide for it.  This
is the simplest way to call this function, but it only works if
the package you want to install is listed in a package archive
you have configured.

If PACKAGE is a string, it specifies the URL of the package
repository.  In this case, optional argument BACKEND specifies
the VC backend to use for cloning the repository; if it's nil,
this function tries to infer which backend to use according to
the value of `vc-clone-heuristic-alist' and if that fails it
uses `package-vc-default-backend'.  Optional argument NAME
specifies the package name in this case; if it's nil, this
package uses `file-name-base' on the URL to obtain the package
name, otherwise NAME is the package name as a symbol.

PACKAGE can also be a cons cell (PNAME . SPEC) where PNAME is the
package name as a symbol, and SPEC is a plist that specifies how
to fetch and build the package.  For possible values, see the
subsection \"Specifying Package Sources\" in the Info
node `(emacs)Fetching Package Sources'.

By default, this function installs the last revision of the
package available from its repository.  If REV is a string, it
describes the revision to install, as interpreted by the relevant
VC backend.  The special value `:last-release' (interactively,
the prefix argument), says to use the commit of the latest
release, if it exists.  The last release is the latest revision
which changed the \"Version:\" header of the package's main Lisp
file.

If you use this function to install a package that you also have
installed from a package archive, the version this function
installs takes precedence."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package-vc--archives-initialize)
     (let* ((name-or-url (package-vc--read-package-name
                          "Fetch and install package: " t))
            (name (file-name-base (directory-file-name name-or-url))))
       (when (string-empty-p name)
         (user-error "Empty package name"))
       (list name-or-url
             (and current-prefix-arg :last-release)
             nil
             (intern (string-remove-prefix "emacs-" name))))))
  (package-vc--archives-initialize)
  (cond
   ((null package)
    (signal 'wrong-type-argument nil))
   ((consp package)
    (package-vc--unpack
     (package-desc-create :name (car package)
                          :kind 'vc)
     (cdr package)
     rev))
   ((and-let* (((stringp package))
               (backend (or backend (vc-guess-url-backend package))))
      (package-vc--unpack
       (package-desc-create
        :name (or name (intern (file-name-base package)))
        :kind 'vc)
       (list :vc-backend backend :url package)
       rev)))
   ((and-let* ((desc (assoc package package-archive-contents #'string=)))
      (package-vc--unpack
       (cadr desc)
       (or (package-vc--desc->spec (cadr desc))
           (and-let* ((extras (package-desc-extras (cadr desc)))
                      (url (alist-get :url extras))
                      (backend (vc-guess-url-backend url)))
             (list :vc-backend backend :url url))
           (user-error "Package `%s' has no VC data" package))
       rev)))
   ((user-error "Unknown package to fetch: %s" package))))

;;;###autoload
(defun package-vc-checkout (pkg-desc directory &optional rev)
  "Clone the sources for PKG-DESC into DIRECTORY and visit that directory.
Unlike `package-vc-install', this does not yet set up the package
for use with Emacs; use `package-vc-install-from-checkout' for
setting the package up after this function finishes.  Optional
argument REV means to clone a specific version of the package; it
defaults to the last version available from the package's
repository.  If REV has the special value
`:last-release' (interactively, the prefix argument), that stands
for the last released version of the package."
  (interactive
   (let* ((name (package-vc--read-package-name "Fetch package source: ")))
     (list (cadr (assoc name package-archive-contents #'string=))
           (read-directory-name "Clone into new or empty directory: " nil nil
                                (lambda (dir) (or (not (file-exists-p dir))
                                             (directory-empty-p dir))))
           (and current-prefix-arg :last-release))))
  (package-vc--archives-initialize)
  (let ((pkg-spec (or (package-vc--desc->spec pkg-desc)
                      (and-let* ((extras (package-desc-extras pkg-desc))
                                 (url (alist-get :url extras))
                                 (backend (vc-guess-url-backend url)))
                        (list :vc-backend backend :url url))
                      (user-error "Package `%s' has no VC data"
                                  (package-desc-name pkg-desc)))))
    (package-vc--clone pkg-desc pkg-spec directory rev)
    (find-file directory)))

;;;###autoload
(defun package-vc-install-from-checkout (dir &optional name interactive)
  "Install the package NAME from its source directory DIR.
NAME defaults to the base name of DIR.  Interactively, prompt the user
for DIR, which should be a directory under version control, typically
one created by `package-vc-checkout'.  If invoked interactively with a
prefix argument, prompt the user for the NAME of the package to set up.
If the optional argument INTERACTIVE is non-nil (as happens
interactively), DIR must be an absolute file name."
  (declare (obsolete "use the User Lisp directory instead." "31.1"))
  (interactive (let ((dir (expand-file-name (read-directory-name "Directory: "))))
                 (list dir (and current-prefix-arg
                                (let ((base (file-name-base
                                             (directory-file-name
                                              dir))))
                                  (read-string
                                   (format-prompt "Package name" base)
                                   nil nil base)))
                       :interactive)))
  (package-vc--archives-initialize)
  (let* ((dir (if interactive dir (expand-file-name dir))) ;avoid double expansion
         (name (or name (file-name-base (directory-file-name dir))))
         (pkg-dir (file-name-concat package-user-dir name)))
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p (format "Overwrite previous checkout for package `%s'?" name))
          (package--delete-directory pkg-dir)
        (error "There already exists a checkout for %s" name)))
    (make-directory pkg-dir t)
    ;; We store a custom package specification so that it is available
    ;; for `package-vc--unpack-1' as well as `package-vc--checkout-dir'
    ;; can later retrieve the actual checkout.
    (package-vc--unpack-1
     (package-desc-create
      :name (intern name)
      :extras (and (not (file-equal-p pkg-dir dir))
                   `((:vc-dir . ,dir)))
      :dir pkg-dir
      :kind 'vc))))

;;;###autoload
(defun package-vc-rebuild (pkg-desc)
  "Rebuild the installation for package given by PKG-DESC.
Rebuilding an installation means scraping for new autoload
cookies, re-compiling Emacs Lisp files, building and installing
any documentation, downloading any missing dependencies.  This
command does not fetch new revisions from a remote server.  That
is the responsibility of `package-vc-upgrade'.  Interactively,
prompt for the name of the package to rebuild."
  (interactive (list (package-vc--read-package-desc "Rebuild package: " t)))
  (package-vc--unpack-1 pkg-desc))

;;;###autoload
(defun package-vc-prepare-patch (pkg-desc subject revisions)
  "Email patches for REVISIONS to maintainer of package PKG-DESC using SUBJECT.

PKG-DESC is a package descriptor and SUBJECT is the subject of
the message.

Interactively, prompt for PKG-DESC, SUBJECT, and REVISIONS.  When
invoked with a numerical prefix argument, use the last N
revisions.  When invoked interactively in a Log View buffer with
marked revisions, use those.

See also `vc-prepare-patch'."
  (interactive
   (list (package-vc--read-package-desc "Package to prepare a patch for: " t)
         (and (not vc-prepare-patches-separately)
              (read-string "Subject: " "[PATCH] " nil nil t))
         (vc-prepare-patch-prompt-revisions)))
  (let ((default-directory (package-vc--checkout-dir pkg-desc)))
    (vc-prepare-patch (package-maintainers pkg-desc t)
                      subject revisions)))

(defun package-vc-root-log-incoming (pkg-desc)
  "Call `vc-root-log-incoming' for the package PKG-DESC."
  (interactive
   (list (package-vc--read-package-desc "Incoming log for package: " t)))
  (let ((default-directory (package-vc--checkout-dir pkg-desc))
        (vc-deduce-backend-nonvc-modes t))
    (call-interactively #'vc-root-log-incoming)))
(define-obsolete-function-alias
  'package-vc-log-incoming
  #'package-vc-root-log-incoming
  "31.1")

(provide 'package-vc)
;;; package-vc.el ends here
