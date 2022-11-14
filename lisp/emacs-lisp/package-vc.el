;;; package-vc.el --- Manage packages from VC checkouts     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Keywords: tools

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

;; While packages managed by package.el use tarballs for distributing
;; the source code, this extension allows for packages to be fetched
;; and updated directly from a version control system.
;;
;; To install a package from source use `package-vc-install'.  If you
;; aren't interested in activating a package, you can use
;; `package-vc-checkout' instead, which will prompt you for a target
;; directory.  If you wish to re-use an existing checkout, the command
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
(eval-when-compile (require 'inline))
(eval-when-compile (require 'map))
(require 'package)
(require 'lisp-mnt)
(require 'vc)
(require 'seq)
(require 'xdg)

(defgroup package-vc nil
  "Manage packages from VC checkouts."
  :group 'package
  :link '(custom-manual "(emacs) Package from Source")
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

(defcustom package-vc-repository-store
  (expand-file-name "emacs/vc-packages" (xdg-data-home))
  "Directory used by `package-vc--unpack' to store repositories."
  :type 'directory
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
(defun package-vc-ensure-packages ()
  "Ensure packages specified in `package-vc-selected-packages' are installed."
  (pcase-dolist (`(,(and (pred symbolp) name) . ,spec)
                 package-vc-selected-packages)
    (let ((pkg-desc (cadr (assoc name package-alist #'string=))))
      (unless (and name (package-installed-p name)
                   (package-vc-p pkg-desc))
        (cond
         ((null spec)
          (package-vc-install name))
         ((stringp spec)
          (package-vc-install name nil spec))
         ((listp spec)
          (package-vc--archives-initialize)
          (package-vc--unpack pkg-desc spec)))))))

;;;###autoload
(defcustom package-vc-selected-packages '()
  "List of packages that must be installed.
Each member of the list is of the form (NAME . SPEC), where NAME
is a symbol designating the package and SPEC is one of:

- nil, if any package version can be installed;
- a version string, if that specific revision is to be installed;
- a property list of the form described in
  `package-vc-archive-spec-alist', giving a package
  specification.

This user option differs from `package-selected-packages' in that
it is meant to be specified manually.  You can also use the
function `package-vc-selected-packages' to apply the changes."
  :type '(alist :tag "List of packages you want to be installed"
                :key-type (symbol :tag "Package")
                :value-type
                (choice (const :tag "Any revision" nil)
                        (string :tag "Specific revision")
                        (plist :options ((:url string)
                                         (:branch string)
                                         (:lisp-dir string)
                                         (:main-file string)
                                         (:vc-backend symbol)))))
  :set (lambda (sym val)
         (custom-set-default sym val)
         (package-vc-ensure-packages))
  :version "29.1")

(defvar package-vc--archive-spec-alist nil
  "List of package specifications for each archive.
The list maps each package name, as a string, to a plist.
Valid keys and the corresponding value types are:

 `:url' (string)
    The URL of the repository used to fetch the package source.

 `:branch' (string)
    If given, the name of the branch to checkout after cloning the directory.

 `:lisp-dir' (string)
    The repository-relative name of the directory to use for loading the Lisp
    sources.  If not given, the value defaults to the root directory
    of the repository.

 `:main-file' (string)
    The main file of the project, relevant to gather package metadata.
    If not given, the assumed default is the package name with \".el\"
    appended to it.

 `:vc-backend' (symbol)
    A symbol of the VC backend to use for cloning the package.  The
    value ought to be a member of `vc-handled-backends'.  If omitted,
    `vc-clone' will fall back onto the archive default or on
    `package-vc-default-backend'.

All other values are ignored.")

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
   (or name (package-desc-name pkg-desc))
   (if (package-desc-archive pkg-desc)
       (alist-get (intern (package-desc-archive pkg-desc))
                  package-vc--archive-spec-alist)
     (mapcan #'append (mapcar #'cdr package-vc--archive-spec-alist)))
   nil nil #'string=))

(define-inline package-vc--query-spec (pkg-desc prop)
  "Query the property PROP for the package specification of PKG-DESC.
If no package specification can be determined, the function will
return nil."
  (inline-letevals (pkg-desc prop)
    (inline-quote (plist-get (package-vc--desc->spec ,pkg-desc) ,prop))))

(defun package-vc--read-archive-data (archive)
  "Update `package-vc--archive-spec-alist' for ARCHIVE.
This function is meant to be used as a hook for `package--read-archive-hook'."
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
              (setf (alist-get (intern archive) package-vc--archive-spec-alist)
                    (car spec)))
            (setf (alist-get (intern archive) package-vc--archive-data-alist)
                  (cdr spec))
            (when-let ((default-vc (plist-get (cdr spec) :default-vc))
                       ((not (memq default-vc vc-handled-backends))))
              (warn "Archive `%S' expects missing VC backend %S"
                    archive (plist-get (cdr spec) :default-vc)))))))))

(defun package-vc--download-and-read-archives (&optional async)
  "Download specifications of all `package-archives' and read them.
Populate `package-vc--archive-spec-alist' with the result.

If optional argument ASYNC is non-nil, perform the downloads
asynchronously."
  (dolist (archive package-archives)
    (condition-case-unless-debug nil
        (package--download-one-archive archive "elpa-packages.eld" async)
      (error (message "Failed to download `%s' archive." (car archive))))))

(add-hook 'package-read-archive-hook     #'package-vc--read-archive-data 20)
(add-hook 'package-refresh-contents-hook #'package-vc--download-and-read-archives 20)

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
  "Extract the commit of a development package PKG."
  (cl-assert (package-vc-p pkg))
  (if-let ((main-file (package-vc--main-file pkg)))
      (with-temp-buffer
        (insert-file-contents main-file)
        (package-strip-rcs-id
         (or (lm-header "package-version")
             (lm-header "version"))))
    "0"))

(defun package-vc--main-file (pkg-desc)
  "Return the name of the main file for PKG-DESC."
  (cl-assert (package-vc-p pkg-desc))
  (let ((pkg-spec (package-vc--desc->spec pkg-desc)))
    (or (plist-get pkg-spec :main-file)
        (expand-file-name
         (format "%s.el" (package-desc-name pkg-desc))
         (file-name-concat
          (or (package-desc-dir pkg-desc)
              (expand-file-name
               (package-desc-name pkg-desc)
               package-user-dir))
          (plist-get pkg-spec :lisp-dir))))))

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
                (cons 'vc (package-vc--version pkg-desc))
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

(declare-function org-export-to-file "ox" (backend file))

(defun package-vc--build-documentation (pkg-desc file)
  "Build documentation FILE for PKG-DESC.
FILE can be an Org file, indicated by its \".org\" extension,
otherwise it's assumed to be an Info file."
  (let ((pkg-dir (package-desc-dir pkg-desc)))
    (when (string-match-p "\\.org\\'" file)
      (require 'ox)
      (require 'ox-texinfo)
      (with-temp-buffer
        (insert-file-contents file)
        (setq file (make-temp-file "ox-texinfo-"))
        (org-export-to-file 'texinfo file)))
    (call-process "install-info" nil nil nil
                  file pkg-dir)))

(defun package-vc--unpack-1 (pkg-desc pkg-dir)
  "Install PKG-DESC that is already checked-out in PKG-DIR."
  ;; In case the package was installed directly from source, the
  ;; dependency list wasn't know beforehand, and they might have
  ;; to be installed explicitly.
  (let (deps)
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
    (package-download-transaction
     (package-compute-transaction nil (delete-dups deps))))

  (let ((default-directory (file-name-as-directory pkg-dir))
        (name (package-desc-name pkg-desc))
        (pkg-file (expand-file-name (package--description-file pkg-dir) pkg-dir)))
    ;; Generate autoloads
    (package-generate-autoloads name pkg-dir)

    ;; Generate package file
    (package-vc--generate-description-file pkg-desc pkg-file)

    ;; Detect a manual
    (when-let ((pkg-spec (package-vc--desc->spec pkg-desc))
               ((executable-find "install-info")))
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
  (package--save-selected-packages
   (cons (package-desc-name pkg-desc)
         package-selected-packages))

  ;; Confirm that the installation was successful
  (let ((main-file (package-vc--main-file pkg-desc)))
    (message "Source package `%s' installed (Version %s, Revision %S)."
             (package-desc-name pkg-desc)
             (lm-with-file main-file
               (package-strip-rcs-id
                (or (lm-header "package-version")
                    (lm-header "version"))))
             (vc-working-revision main-file)))
  t)

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
                         (package-vc--query-spec pkg-desc :vc-backend)
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

(defun package-vc--unpack (pkg-desc pkg-spec &optional rev)
  "Install the package described by PKG-DESC.
PKG-SPEC is a package specification, a property list describing
how to fetch and build the package.  See `package-vc--archive-spec-alist'
for details.  The optional argument REV specifies a specific revision to
checkout.  This overrides the `:branch' attribute in PKG-SPEC."
  (pcase-let* (((map :url :lisp-dir) pkg-spec)
               (name (package-desc-name pkg-desc))
               (dirname (package-desc-full-name pkg-desc))
               (pkg-dir (expand-file-name dirname package-user-dir))
               (real-dir (if (null lisp-dir)
                             pkg-dir
                           (unless (file-exists-p package-vc-repository-store)
                             (make-directory package-vc-repository-store t))
                           (file-name-concat
                            package-vc-repository-store
                            ;; FIXME: We aren't sure this directory
                            ;; will be unique, but we can try other
                            ;; names to avoid an unnecessary error.
                            (file-name-base url)))))
    (setf (package-desc-dir pkg-desc) pkg-dir)
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p "Overwrite previous checkout?")
          (package--delete-directory pkg-dir pkg-desc)
        (error "There already exists a checkout for %s" name)))
    (package-vc--clone pkg-desc pkg-spec real-dir rev)
    (unless (eq pkg-dir real-dir)
      ;; Link from the right position in `repo-dir' to the package
      ;; directory in the ELPA store.
      (make-symbolic-link (file-name-concat real-dir lisp-dir) pkg-dir))

    (package-vc--unpack-1 pkg-desc pkg-dir)))

(defun package-vc--sourced-packages-list ()
  "Generate a list of packages with VC data."
  (seq-filter
   (lambda (pkg)
     (or (package-vc--desc->spec (cadr pkg))
         ;; If we have no explicit VC data, we can try a kind of
         ;; heuristic and use the URL header, that might already be
         ;; pointing towards a repository, and use that as a backup
         (and-let* ((extras (package-desc-extras (cadr pkg)))
                    (url (alist-get :url extras))
                    ((package-vc--guess-backend url))))))
   package-archive-contents))

(defun package-vc-update (pkg-desc)
  "Attempt to update the package PKG-DESC."
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
  (letrec ((pkg-dir (package-desc-dir pkg-desc))
           (empty (make-symbol empty))
           (args (list empty empty empty))
           (vc-filter-command-function
            (lambda (command file-or-list flags)
              (setf (nth 0 args) command
                    (nth 1 args) file-or-list
                    (nth 2 args) flags)
              (list command file-or-list flags)))
           (post-upgrade
            (lambda (command file-or-list flags)
              (when (and (memq (nth 0 args) (list command empty))
                         (memq (nth 1 args) (list file-or-list empty))
                         (memq (nth 2 args) (list flags empty)))
                (with-demoted-errors "Failed to activate: %S"
                  (package-vc--unpack-1 pkg-desc pkg-dir))
                (remove-hook 'vc-post-command-functions post-upgrade)))))
    (add-hook 'vc-post-command-functions post-upgrade)
    (with-demoted-errors "Failed to fetch: %S"
      (vc-pull))))

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
(defun package-vc-install (name-or-url &optional name rev backend)
  "Fetch a package NAME-OR-URL and set it up for using with Emacs.
If NAME-OR-URL is a URL, download the package from the repository
at that URL; the function will try to guess the name of the package
from the URL.  Otherwise NAME-OR-URL should be a symbol whose name
is the package name, and the URL for the package will be taken from
the package's metadata.
By default, this function installs the last version of the package
available from its repository, but if REV is given and non-nil, it
specifies the revision to install.  If REV has the special value
`:last-release' (interactively, the prefix argument), that stands
for the last released version of the package.
When calling from Lisp, optional argument NAME overrides the package
name as deduced from NAME-OR-URL.
Optional argument BACKEND specifies the VC backend to use for cloning
the package's repository; this is only possible if NAME-OR-URL is a URL,
a string.  If BACKEND is omitted or nil, the function
uses `package-vc--guess-backend' to guess the backend."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package-vc--archives-initialize)
     (let* ((packages (package-vc--sourced-packages-list))
            (input (completing-read
                    "Fetch package source (name or URL): " packages))
            (name (file-name-base input)))
       (list input (intern (string-remove-prefix "emacs-" name))
             (and current-prefix-arg :last-release)))))
  (package-vc--archives-initialize)
  (cond
   ((and-let* (((stringp name-or-url))
               (backend (or backend (package-vc--guess-backend name-or-url))))
      (package-vc--unpack
       (package-desc-create
        :name (or name (intern (file-name-base name-or-url)))
        :kind 'vc)
       (list :vc-backend backend :url name-or-url)
       rev)))
   ((and-let* ((desc (assoc name-or-url package-archive-contents #'string=)))
      (package-vc--unpack
       (let ((copy (copy-package-desc (cadr desc))))
         (setf (package-desc-kind copy) 'vc)
         copy)
       (or (package-vc--desc->spec (cadr desc))
           (and-let* ((extras (package-desc-extras (cadr desc)))
                      (url (alist-get :url extras))
                      (backend (package-vc--guess-backend url)))
             (list :vc-backend backend :url url))
           (user-error "Package has no VC data"))
       rev)))
   ((user-error "Unknown package to fetch: %s" name-or-url))))

;;;###autoload
(defun package-vc-checkout (pkg-desc directory &optional rev)
  "Clone the sources for PKG-DESC into DIRECTORY and visit that directory.
Unlike `package-vc-install', this does not yet set up the package
for use with Emacs; use `package-vc-link-directory' for setting
the package up after this function finishes.
Optional argument REV means to clone a specific version of the
package; it defaults to the last version available from the
package's repository.  If REV has the special value
`:last-release' (interactively, the prefix argument), that stands
for the last released version of the package."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package-vc--archives-initialize)
     (let* ((packages (package-vc--sourced-packages-list))
            (input (completing-read
                    "Fetch package source (name or URL): " packages)))
       (list (cadr (assoc input package-archive-contents #'string=))
             (read-file-name "Clone into new or empty directory: " nil nil t nil
                             (lambda (dir) (or (not (file-exists-p dir))
                                               (directory-empty-p dir))))
             (and current-prefix-arg :last-release)))))
  (package-vc--archives-initialize)
  (let ((pkg-spec (or (package-vc--desc->spec pkg-desc)
                      (and-let* ((extras (package-desc-extras pkg-desc))
                                 (url (alist-get :url extras))
                                 (backend (package-vc--guess-backend url)))
                        (list :vc-backend backend :url url))
                      (user-error "Package has no VC data"))))
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
    (make-symbolic-link dir pkg-dir)
    (package-vc--unpack-1 (package-desc-create
                          :name (intern name)
                          :kind 'vc)
                         pkg-dir)))

;;;###autoload
(defun package-vc-refresh (pkg-desc)
  "Refresh the installation for package given by PKG-DESC.
Interactively, prompt for the name of the package to refresh."
  (interactive (package-vc--read-pkg "Refresh package: "))
  (package-vc--unpack-1 pkg-desc (package-desc-dir pkg-desc)))

(defun package-vc--read-pkg (prompt)
  "Query for a source package description with PROMPT."
  (cadr (assoc (completing-read
                prompt
                package-alist
                (lambda (pkg) (package-vc-p (cadr pkg)))
                t)
               package-alist
               #'string=)))

;;;###autoload
(defun package-vc-prepare-patch (pkg subject revisions)
  "Send patch for REVISIONS to maintainer of the package PKG using SUBJECT.
SUBJECT and REVISIONS are passed on to `vc-prepare-patch', which see.
PKG must be a package description.
Interactively, prompt for PKG, SUBJECT, and REVISIONS.  However,
if the current buffer has marked commit log entries, REVISIONS
are the tags of the marked entries, see `log-view-get-marked'."
  (interactive
   (list (package-vc--read-pkg "Package to prepare a patch for: ")
         (and (not vc-prepare-patches-separately)
              (read-string "Subject: " "[PATCH] " nil nil t))
         (or (log-view-get-marked)
             (vc-read-multiple-revisions "Revisions: "))))
  (vc-prepare-patch (package-maintainers pkg t)
                    subject revisions))

(provide 'package-vc)
;;; package-vc.el ends here
