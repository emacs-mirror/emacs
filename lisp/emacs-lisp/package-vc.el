;;; package-vc.el --- Manage packages from VC checkouts     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
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
;; `package-vc-install-from-checkout' will create a symbolic link and
;; prepare the package.
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

(defcustom package-vc-heuristic-alist
  `((,(rx bos "http" (? "s") "://"
          (or (: (? "www.") "github.com"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "codeberg.org"
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: (? "www.") "gitlab" (+ "." (+ alnum))
                 "/" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "git." (or "savannah" "sv") "." (? "non") "gnu.org/"
                 (or "r" "git") "/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          (or (? "/") ".git") eos)
     . Git)
    (,(rx bos "http" (? "s") "://"
          (or (: "hg.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "hg." (or "savannah" "sv") "." (? "non") "gnu.org/hgweb/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Hg)
    (,(rx bos "http" (? "s") "://"
          (or (: "bzr." (or "savannah" "sv") "." (? "non") "gnu.org/r/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Bzr))
  "Heuristic mapping URL regular expressions to VC backends."
  :type `(alist :key-type (regexp :tag "Regular expression matching URLs")
                :value-type (choice :tag "VC Backend"
                                    ,@(mapcar (lambda (b) `(const ,b))
                                              vc-handled-backends)))
  :version "29.1")

(defcustom package-vc-default-backend 'Git
  "Default VC backend used when cloning a package repository.
If no repository type was specified or could be guessed by
`package-vc-heuristic-alist', this is the default VC backend
used as fallback.  The value must be a member of
`vc-handled-backends' and the named backend must implement
the `clone' function."
  :type `(choice ,@(mapcar (lambda (b) (list 'const b))
                           vc-handled-backends))
  :version "29.1")

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

(defcustom package-vc-selected-packages '()
  "List of packages that must be installed.
Each member of the list is of the form (NAME . SPEC), where NAME
is a symbol designating the package and SPEC is one of:

- nil, if any package version can be installed;
- a version string, if that specific revision is to be installed;
- a property list, describing a package specification.  For more
  details, please consult the subsection \"Specifying Package
  Sources\" in the Info node `(emacs)Fetching Package Sources'.

This user option will be automatically updated to store package
specifications for packages that are not specified in any
archive."
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
            (when-let ((default-vc (plist-get (cdr spec) :default-vc))
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

(defun package-vc-commit (pkg)
  "Return the last commit of a development package PKG."
  (cl-assert (package-vc-p pkg))
  ;; FIXME: vc should be extended to allow querying the commit of a
  ;; directory (as is possible when dealing with git repositories).
  ;; This should be a fallback option.
  (cl-loop with dir = (package-desc-dir pkg)
           for file in (directory-files dir t "\\.el\\'" t)
           when (vc-working-revision file) return it
           finally return "unknown"))

(defun package-vc--version (pkg)
  "Return the version number for the VC package PKG."
  (cl-assert (package-vc-p pkg))
  (if-let ((main-file (package-vc--main-file pkg)))
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
         (directory (file-name-concat
                     (or (package-desc-dir pkg-desc)
                         (expand-file-name name package-user-dir))
                     (plist-get pkg-spec :lisp-dir)))
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
  (let ((name (package-desc-name pkg-desc)))
    ;; Infer the subject if missing.
    (unless (package-desc-summary pkg-desc)
      (setf (package-desc-summary pkg-desc)
            (let ((main-file (package-vc--main-file pkg-desc)))
              (or (package-desc-summary pkg-desc)
                  (and-let* ((pkg (cadr (assq name package-archive-contents))))
                    (package-desc-summary pkg))
                  (and main-file (file-exists-p main-file)
                       (lm-summary main-file))
                  package--default-summary))))
    (let ((print-level nil)
          (print-quoted t)
          (print-length nil))
      (write-region
       (concat
        ";;; Generated package description from "
        (replace-regexp-in-string
         "-pkg\\.el\\'" ".el"
         (file-name-nondirectory pkg-file))
        "  -*- no-byte-compile: t -*-\n"
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
           (package-desc-extras pkg-desc))))
        "\n")
       nil pkg-file nil 'silent))))

(declare-function org-export-to-file "ox" (backend file))

(defun package-vc--build-documentation (pkg-desc file)
  "Build documentation for package PKG-DESC from documentation source in FILE.
FILE can be an Org file, indicated by its \".org\" extension,
otherwise it's assumed to be an Info file."
  (let* ((pkg-name (package-desc-name pkg-desc))
         (default-directory (package-desc-dir pkg-desc))
         (docs-directory (file-name-directory (expand-file-name file)))
         (output (expand-file-name (format "%s.info" pkg-name)))
         clean-up)
    (when (string-match-p "\\.org\\'" file)
      (require 'ox)
      (require 'ox-texinfo)
      (with-temp-buffer
        (insert-file-contents file)
        (setq file (make-temp-file "ox-texinfo-"))
        (let ((default-directory docs-directory))
          (org-export-to-file 'texinfo file))
        (setq clean-up t)))
    (with-current-buffer (get-buffer-create " *package-vc doc*")
      (erase-buffer)
      (cond
       ((/= 0 (call-process "makeinfo" nil t nil
                            "-I" docs-directory
                            "--no-split" file
                            "-o" output))
        (message "Failed to build manual %s, see buffer %S"
                 file (buffer-name)))
       ((/= 0 (call-process "install-info" nil t nil
                            output (expand-file-name "dir")))
        (message "Failed to install manual %s, see buffer %S"
                 output (buffer-name)))
       ((kill-buffer))))
    (when clean-up
      (delete-file file))))

(defun package-vc-install-dependencies (requirements)
  "Install missing dependencies, and return missing ones.
The return value will be nil if everything was found, or a list
of (NAME VERSION) pairs of all packages that couldn't be found.

REQUIREMENTS should be a list of additional requirements; each
element in this list should have the form (PACKAGE VERSION-LIST),
where PACKAGE is a package name and VERSION-LIST is the required
version of that package."
  (let ((to-install '()) (missing '()))
    (cl-labels ((search (pkg)
                  "Attempt to find all dependencies for PKG."
                  (cond
                   ((assq (car pkg) to-install)) ;inhibit cycles
                   ((package-installed-p (car pkg)))
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
      (mapc #'search requirements)
      (cl-callf sort to-install #'version-order)
      (cl-callf seq-uniq to-install #'duplicate-p)
      (cl-callf sort to-install #'dependent-order))
    (mapc #'package-install-from-archive to-install)
    missing))

(defun package-vc--unpack-1 (pkg-desc pkg-dir)
  "Prepare PKG-DESC that is already checked-out in PKG-DIR.
This includes downloading missing dependencies, generating
autoloads, generating a package description file (used to
identify a package as a VC package later on), building
documentation and marking the package as installed."
  (let (missing)
    ;; Remove any previous instance of PKG-DESC from `package-alist'
    (let ((pkgs (assq (package-desc-name pkg-desc) package-alist)))
      (when pkgs
        (setf (cdr pkgs) (seq-remove #'package-vc-p (cdr pkgs)))))

    ;; In case the package was installed directly from source, the
    ;; dependency list wasn't know beforehand, and they might have
    ;; to be installed explicitly.
    (let ((deps '()))
      (dolist (file (directory-files pkg-dir t "\\.el\\'" t))
        (with-temp-buffer
          (insert-file-contents file)
          (when-let* ((require-lines (lm-header-multiline "package-requires")))
            (thread-last
              (mapconcat #'identity require-lines " ")
              package-read-from-string
              package--prepare-dependencies
              (nconc deps)
              (setq deps)))))
      (dolist (dep deps)
        (cl-callf version-to-list (cadr dep)))
      (setf missing (package-vc-install-dependencies (delete-dups deps)))
      (setf missing (delq (assq (package-desc-name pkg-desc)
                                missing)
                          missing)))

    (let ((default-directory (file-name-as-directory pkg-dir))
          (pkg-file (expand-file-name (package--description-file pkg-dir) pkg-dir))
          (pkg-spec (package-vc--desc->spec pkg-desc)))
      ;; Generate autoloads
      (let* ((name (package-desc-name pkg-desc))
             (auto-name (format "%s-autoloads.el" name))
             (lisp-dir (plist-get pkg-spec :lisp-dir)))
        (package-generate-autoloads
         name (file-name-concat pkg-dir lisp-dir))
        (when lisp-dir
          (write-region
           (with-temp-buffer
             (insert ";; Autoload indirection for package-vc\n\n")
             (prin1 `(load (expand-file-name
                            ,(file-name-concat lisp-dir auto-name)
                            (or (and load-file-name
                                     (file-name-directory load-file-name))
                                (car load-path))))
                    (current-buffer))
             (buffer-string))
           nil (expand-file-name auto-name pkg-dir))))

      ;; Generate package file
      (package-vc--generate-description-file pkg-desc pkg-file)

      ;; Detect a manual
      (when (executable-find "install-info")
        (dolist (doc-file (ensure-list (plist-get pkg-spec :doc)))
          (package-vc--build-documentation pkg-desc doc-file))))

    ;; Update package-alist.
    (let ((new-desc (package-load-descriptor pkg-dir)))
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

(defun package-vc--guess-backend (url)
  "Guess the VC backend for URL.
This function will internally query `package-vc-heuristic-alist'
and return nil if it cannot reasonably guess."
  (and url (alist-get url package-vc-heuristic-alist
                      nil nil #'string-match-p)))

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
                         (package-vc--guess-backend url)
                         (plist-get (alist-get (package-desc-archive pkg-desc)
                                               package-vc--archive-data-alist
                                               nil nil #'string=)
                                    :vc-backend)
                         package-vc-default-backend)))
        (unless (vc-clone url backend dir
                          (or (and (not (eq rev :last-release)) rev) branch))
          (error "Failed to clone %s from %s" name url))))

    ;; Check out the latest release if requested
    (when (eq rev :last-release)
      (if-let ((release-rev (package-vc--release-rev pkg-desc)))
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
  (pcase-let* (((map :lisp-dir) pkg-spec)
               (name (package-desc-name pkg-desc))
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

    ;; When nothing is specified about a `lisp-dir', then should
    ;; heuristically check if there is a sub-directory with lisp
    ;; files.  These are conventionally just called "lisp" or "src".
    ;; If this directory exists and contains non-zero number of lisp
    ;; files, we will use that instead of `pkg-dir'.
    (catch 'done
      (dolist (name '("lisp" "src"))
        (when-let* (((null lisp-dir))
                    (dir (expand-file-name name pkg-dir))
                    ((file-directory-p dir))
                    ((directory-files dir nil "\\`[^.].+\\.el\\'" t 1)))
          ;; We won't use `dir', since dir is an absolute path and we
          ;; don't want `lisp-dir' to depend on the current location of
          ;; the package installation, ie. to break if moved around the
          ;; file system or between installations.
          (throw 'done (setq lisp-dir name)))))

    ;; Ensure we have a copy of the package specification
    (unless (seq-some (lambda (alist) (equal (alist-get name (cdr alist)) pkg-spec))
                      package-vc--archive-spec-alists)
      (customize-save-variable
       'package-vc-selected-packages
       (cons (cons name pkg-spec)
             (seq-remove (lambda (spec) (string= name (car spec)))
                         package-vc-selected-packages))))

    (package-vc--unpack-1 pkg-desc pkg-dir)))

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
                                      ((package-vc--guess-backend url)))))))
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
  "Attempt to upgrade all installed VC packages."
  (interactive)
  (dolist (package package-alist)
    (dolist (pkg-desc (cdr package))
      (when (package-vc-p pkg-desc)
        (package-vc-upgrade pkg-desc))))
  (message "Done upgrading packages."))

;;;###autoload
(defun package-vc-upgrade (pkg-desc)
  "Attempt to upgrade the package PKG-DESC."
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
  (letrec ((pkg-dir (package-desc-dir pkg-desc))
           (vc-flags)
           (vc-filter-command-function
            (lambda (command file-or-list flags)
              (setq vc-flags flags)
              (list command file-or-list flags)))
           (post-upgrade
            (lambda (_command _file-or-list flags)
              (when (and (file-equal-p pkg-dir default-directory)
                         (eq flags vc-flags))
                (unwind-protect
                    (with-demoted-errors "Failed to activate: %S"
                      (package-vc--unpack-1 pkg-desc pkg-dir))
                  (remove-hook 'vc-post-command-functions post-upgrade))))))
    (add-hook 'vc-post-command-functions post-upgrade)
    (with-demoted-errors "Failed to fetch: %S"
      (let ((default-directory pkg-dir))
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
  "Fetch a PACKAGE and set it up for using with Emacs.

If PACKAGE is a string containing an URL, download the package
from the repository at that URL; the function will try to guess
the name of the package from the URL.  This can be overridden by
passing the optional argument NAME.  If PACKAGE is a cons-cell,
it should have the form (NAME . SPEC), where NAME is a symbol
indicating the package name and SPEC is a plist as described in
`package-vc-selected-packages'.  Otherwise PACKAGE should be a
symbol whose name is the package name, and the URL for the
package will be taken from the package's metadata.

By default, this function installs the last revision of the
package available from its repository.  If REV is a string, it
describes the revision to install, as interpreted by the VC
backend.  The special value `:last-release' (interactively, the
prefix argument), will use the commit of the latest release, if
it exists.  The last release is the latest revision which changed
the \"Version:\" header of the package's main Lisp file.

Optional argument BACKEND specifies the VC backend to use for cloning
the package's repository; this is only possible if NAME-OR-URL is a URL,
a string.  If BACKEND is omitted or nil, the function
uses `package-vc-heuristic-alist' to guess the backend.
Note that by default, a VC package will be prioritized over a
regular package, but it will not remove a VC package.

\(fn PACKAGE &optional REV BACKEND)"
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
               (backend (or backend (package-vc--guess-backend package))))
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
                      (backend (package-vc--guess-backend url)))
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
  (setf directory (expand-file-name directory))
  (package-vc--archives-initialize)
  (let ((pkg-spec (or (package-vc--desc->spec pkg-desc)
                      (and-let* ((extras (package-desc-extras pkg-desc))
                                 (url (alist-get :url extras))
                                 (backend (package-vc--guess-backend url)))
                        (list :vc-backend backend :url url))
                      (user-error "Package `%s' has no VC data"
                                  (package-desc-name pkg-desc)))))
    (package-vc--clone pkg-desc pkg-spec directory rev)
    (find-file directory)))

;;;###autoload
(defun package-vc-install-from-checkout (dir name)
  "Set up the package NAME in DIR by linking it into the ELPA directory.
Interactively, prompt the user for DIR, which should be a directory
under version control, typically one created by `package-vc-checkout'.
If invoked interactively with a prefix argument, prompt the user
for the NAME of the package to set up.  Otherwise infer the package
name from the base name of DIR."
  (interactive (let ((dir (read-directory-name "Directory: ")))
                 (list dir
                       (if current-prefix-arg
                           (read-string "Package name: ")
                         (file-name-base (directory-file-name dir))))))
  (unless (vc-responsible-backend dir)
    (user-error "Directory %S is not under version control" dir))
  (package-vc--archives-initialize)
  (let* ((name (or name (file-name-base (directory-file-name dir))))
         (pkg-dir (expand-file-name name package-user-dir)))
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p (format "Overwrite previous checkout for package `%s'?" name))
          (package--delete-directory pkg-dir)
        (error "There already exists a checkout for %s" name)))
    (make-symbolic-link (expand-file-name dir) pkg-dir)
    (package-vc--unpack-1
     (package-desc-create
      :name (intern name)
      :dir pkg-dir
      :kind 'vc)
     (file-name-as-directory pkg-dir))))

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
  (package-vc--unpack-1 pkg-desc (package-desc-dir pkg-desc)))

;;;###autoload
(defun package-vc-prepare-patch (pkg-desc subject revisions)
  "Send patch for REVISIONS to maintainer of the package PKG using SUBJECT.
The function uses `vc-prepare-patch', passing SUBJECT and
REVISIONS directly.  PKG-DESC must be a package description.
Interactively, prompt for PKG-DESC, SUBJECT, and REVISIONS.  When
invoked with a numerical prefix argument, use the last N
revisions.  When invoked interactively in a Log View buffer with
marked revisions, use those."
  (interactive
   (list (package-vc--read-package-desc "Package to prepare a patch for: " t)
         (and (not vc-prepare-patches-separately)
              (read-string "Subject: " "[PATCH] " nil nil t))
         (vc-prepare-patch-prompt-revisions)))
  (let ((default-directory (package-desc-dir pkg-desc)))
    (vc-prepare-patch (package-maintainers pkg-desc t)
                      subject revisions)))

(provide 'package-vc)
;;; package-vc.el ends here
