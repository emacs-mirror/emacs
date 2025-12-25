;;; package-elpa.el --- ELPA integration -*- lexical-binding: t; -*-

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

;; This file contains the logic for contacting a ELPA (Emacs Lisp
;; Package Archive) and processing their response, as well as fetching
;; individual package tarballs.

;;; Code:

(require 'package-core)

(require 'epg)
(require 'url-http)
(require 'url-handlers)

(defgroup package-archive nil
  "Archive configuration of the package manager."
  :group 'package
  :version "31.1")

(defcustom package-archives `(("gnu" .
                               ,(format "http%s://elpa.gnu.org/packages/"
                                        (if (gnutls-available-p) "s" "")))
                              ("nongnu" .
                               ,(format "http%s://elpa.nongnu.org/nongnu/"
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
a package can run arbitrary code.

HTTPS URLs should be used where possible, as they offer superior
security."
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (string :tag "URL or directory name"))
  :risky t
  :version "28.1")

(defcustom package-archive-priorities nil
  "An alist of priorities for packages.

Each element has the form (ARCHIVE-ID . PRIORITY).

When installing packages, the package with the highest version
number from the archive with the highest priority is
selected.  When higher versions are available from archives with
lower priorities, the user has to select those manually.

Archives not in this list have the priority 0, as have packages
that are already installed.  If you use negative priorities for
the archives, they will not be upgraded automatically.

See also `package-menu-hide-low-priority'."
  :type '(alist :key-type (string :tag "Archive name")
                :value-type (integer :tag "Priority (default is 0)"))
  :risky t
  :version "25.1")

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

(defcustom package-check-signature 'allow-unsigned
  "Non-nil means to check package signatures when installing.

This also applies to the \"archive-contents\" file that lists the
contents of the archive.

The value can be one of:

  t                  Accept a package only if it comes with at least
                     one verified signature.

  `all'              Same as t, but verify all signatures if there
                     are more than one.

  `allow-unsigned'   Install a package even if it is unsigned,
                     but verify the signature if possible (that
                     is, if it is signed, we have the key for it,
                     and GnuPG is installed).

  nil                Package signatures are ignored."
  :type '(choice (const :value nil            :tag "Never")
                 (const :value allow-unsigned :tag "Allow unsigned")
                 (const :value t              :tag "Check always")
                 (const :value all            :tag "Check always (all signatures)"))
  :risky t
  :version "27.1")

(defun package-check-signature ()
  "Check whether we have a usable OpenPGP configuration.
If so, and variable `package-check-signature' is
`allow-unsigned', return `allow-unsigned', otherwise return the
value of variable `package-check-signature'."
  (if (eq package-check-signature 'allow-unsigned)
      (and (epg-find-configuration 'OpenPGP)
           'allow-unsigned)
    package-check-signature))

(defcustom package-unsigned-archives nil
  "List of archives where we do not check for package signatures.
This should be a list of strings matching the names of package
archives in the variable `package-archives'."
  :type '(repeat (string :tag "Archive name"))
  :risky t
  :version "24.4")

(defun package--write-file-no-coding (file-name)
  "Write file FILE-NAME without encoding using coding system."
  (let ((buffer-file-coding-system 'no-conversion))
    (write-region (point-min) (point-max) file-name nil 'silent)))

(defun package--archive-file-exists-p (location file)
  "Return t if FILE exists in remote LOCATION."
  (let ((http (string-match "\\`https?:" location)))
    (if http
        (progn
          (require 'url-http)
          (url-http-file-exists-p (concat location file)))
      (file-exists-p (expand-file-name file location)))))

(defun package--display-verify-error (context sig-file)
  "Show error details with CONTEXT for failed verification of SIG-FILE.
The details are shown in a new buffer called \"*Error\"."
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
  (declare (indent defun) (debug (sexp body)))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(package--with-response-buffer-1 ,url (lambda () ,@body)
                                    :file ,file
                                    :async ,async
                                    :error-function (lambda () ,error-form)
                                    :noerror ,noerror))

(defmacro package--unless-error (body &rest before-body)
  (declare (debug t) (indent 1))
  (let ((err (make-symbol "err")))
    `(with-temp-buffer
       (set-buffer-multibyte nil)
       (when (condition-case ,err
                 (progn ,@before-body t)
               (error (funcall error-function)
                      (unless noerror
                        (signal (car ,err) (cdr ,err)))))
         (funcall ,body)))))

(cl-defun package--with-response-buffer-1 (url body &key async file error-function noerror &allow-other-keys)
  (if (string-match-p "\\`https?:" url)
        (let ((url (url-expand-file-name file url)))
          (if async
              (package--unless-error #'ignore
                (url-retrieve
                 url
                 (lambda (status)
                   (let ((b (current-buffer)))
                     (package--unless-error body
                       (when-let* ((er (plist-get status :error)))
                         (error "Error retrieving: %s %S" url er))
                       (with-current-buffer b
                         (goto-char (point-min))
                         (unless (search-forward-regexp "^\r?\n\r?" nil t)
                           (error "Error retrieving: %s %S"
                                  url "incomprehensible buffer")))
                       (url-insert b)
                       (kill-buffer b)
                       (goto-char (point-min)))))
                 nil
                 'silent))
            (package--unless-error body
              ;; Copy&pasted from url-insert-file-contents,
              ;; except it calls `url-insert' because we want the contents
              ;; literally (but there's no url-insert-file-contents-literally).
              (let ((buffer (url-retrieve-synchronously url)))
                (unless buffer (signal 'file-error (list url "No Data")))
                (when (fboundp 'url-http--insert-file-helper)
                  ;; XXX: This is HTTP/S specific and should be moved
                  ;; to url-http instead.  See bug#17549.
                  (url-http--insert-file-helper buffer url))
                (url-insert buffer)
                (kill-buffer buffer)
                (goto-char (point-min))))))
      (package--unless-error body
        (unless (file-name-absolute-p url)
          (error "Location %s is not a url nor an absolute file name" url))
        (insert-file-contents-literally (expand-file-name file url)))))

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
          ;; If `package-check-signature' is allow-unsigned, don't
          ;; signal error when we can't verify signature because of
          ;; missing public key.  Other errors are still treated as
          ;; fatal (bug#17625).
          (unless (and (eq (package-check-signature) 'allow-unsigned)
                       (eq (epg-signature-status sig) 'no-pubkey))
            (setq had-fatal-error t))))
      (when (or (null good-signatures)
                (and (eq (package-check-signature) 'all)
                     had-fatal-error))
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
      :error-form (let ((allow-unsigned
                         (eq (package-check-signature) 'allow-unsigned)))
                    (when (and callback allow-unsigned)
                      (funcall callback nil))
                    (when unwind (funcall unwind))
                    (unless allow-unsigned
                      (error "Unsigned file `%s' at %s" file location)))
      ;; OTOH, an error here means "bad signature", which we never
      ;; suppress.  (Bug#22089)
      (unwind-protect
          (let ((sig (package--check-signature-content
                      (buffer-substring (point) (point-max))
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
  "Version number of the package archive understood by package.el.
Lower version numbers than this will probably be understood as well.")

;; We don't prime the cache since it tends to get out of date.
(defvar package-archive-contents nil
  "Cache of the contents of all archives in `package-archives'.
This is an alist mapping package names (symbols) to
non-empty lists of `package-desc' structures.")
(put 'package-archive-contents 'risky-local-variable t)

;; Package descriptor objects used inside the "archive-contents" file.
;; Changing this defstruct implies changing the format of the
;; "archive-contents" files.
(cl-defstruct (package--ac-desc
               (:constructor package-make-ac-desc (version reqs summary kind extras))
               (:copier nil)
               (:type vector))
  version reqs summary kind extras)

(defun package-get-descriptor (pkg-name)
  "Return the `package-desc' of PKG-NAME."
  (unless package--initialized (package-initialize 'no-activate))
  (or (package--get-activatable-pkg pkg-name)
      (cadr (assq pkg-name package-alist))
      (cadr (assq pkg-name package-archive-contents))))

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
  "Read cached archive FILE data, if it exists.
Return the data from the file, or nil if the file does not exist.
If the archive version is too new, signal an error."
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
  "Read cached archive file for ARCHIVE.
If successful, set or update the variable `package-archive-contents'.
ARCHIVE should be a string matching the name of a package archive
in the variable `package-archives'.
If the archive version is too new, signal an error."
  ;; Version 1 of 'archive-contents' is identical to our internal
  ;; representation.
  (let* ((contents-file (format "archives/%s/archive-contents" archive))
         (contents (package--read-archive-file contents-file)))
    (when contents
      (dolist (package contents)
        (if package
            (package--add-to-archive-contents package archive)
          (lwarn '(package refresh) :warning
                 "Ignoring nil package on `%s' package archive" archive))))))

(defvar package--old-archive-priorities nil
  "Store currently used `package-archive-priorities'.
This is the value of `package-archive-priorities' last time
`package-read-all-archive-contents' was called.  It can be used
by arbitrary functions to decide whether it is necessary to call
it again.")

(defvar package-read-archive-hook (list #'package-read-archive-contents)
  "List of functions to call to read the archive contents.
Each function must take an optional argument, a symbol indicating
what archive to read in.  The symbol ought to be a key in
`package-archives'.")

(defun package-read-all-archive-contents ()
  "Read cached archive file for all archives in `package-archives'.
If successful, set or update `package-archive-contents'."
  (setq package-archive-contents nil)
  (setq package--old-archive-priorities package-archive-priorities)
  (dolist (archive package-archives)
    (run-hook-with-args 'package-read-archive-hook (car archive))))

(defvar package--downloads-in-progress nil
  "List of in-progress asynchronous downloads.")

;;;###autoload
(defun package-import-keyring (&optional file)
  "Import keys from FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let ((context (epg-make-context 'OpenPGP)))
    (when package-gnupghome-dir
      (with-file-modes #o700
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
  ;; The downloaded archive contents will be read as part of
  ;; `package--update-downloads-in-progress'.
  (when async
    (cl-pushnew (cons archive file) package--downloads-in-progress
                :test #'equal))
  (package--with-response-buffer (cdr archive) :file file
    :async async
    :error-form (package--update-downloads-in-progress (cons archive file))
    (let* ((location (cdr archive))
           (name (car archive))
           (content (buffer-string))
           (dir (expand-file-name (concat "archives/" name) package-user-dir))
           (local-file (expand-file-name file dir)))
      (when (listp (read content))
        (make-directory dir t)
        (if (or (not (package-check-signature))
                (member name package-unsigned-archives))
            ;; If we don't care about the signature, save the file and
            ;; we're done.
            (progn
              (cl-assert (not enable-multibyte-characters))
              (let ((coding-system-for-write 'binary))
                (write-region content nil local-file nil 'silent))
              (package--update-downloads-in-progress (cons archive file)))
          ;; If we care, check it (perhaps async) and *then* write the file.
          (package--check-signature
           location file content async
           ;; This function will be called after signature checking.
           (lambda (&optional good-sigs)
             (cl-assert (not enable-multibyte-characters))
             (let ((coding-system-for-write 'binary))
               (write-region content nil local-file nil 'silent))
             ;; Write out good signatures into archive-contents.signed file.
             (when good-sigs
               (write-region (mapconcat #'epg-signature-to-string good-sigs "\n")
                             nil (concat local-file ".signed") nil 'silent)))
           (lambda () (package--update-downloads-in-progress (cons archive file)))))))))

(defun package--download-and-read-archives (&optional async)
  "Download descriptions of all `package-archives' and read them.
Populate `package-archive-contents' with the result.

If optional argument ASYNC is non-nil, perform the downloads
asynchronously."
  (dolist (archive package-archives)
    (condition-case-unless-debug err
        (package--download-one-archive archive "archive-contents" async)
      (error (message "Failed to download `%s' archive: %s"
                      (car archive)
                      (error-message-string err))))))

(defvar package-refresh-contents-hook (list #'package--download-and-read-archives)
  "List of functions to call to refresh the package archive.
Each function may take an optional argument indicating that the
operation ought to be executed asynchronously.")

;;;###autoload
(defun package-refresh-contents (&optional async)
  "Download descriptions of all configured ELPA packages.
For each archive configured in the variable `package-archives',
inform Emacs about the latest versions of all packages it offers,
and make them available for download.
Optional argument ASYNC specifies whether to perform the
downloads in the background.  This is always the case when the command
is invoked interactively."
  (interactive (list t))
  (when async
    (message "Refreshing package contents..."))
  (unless (file-exists-p package-user-dir)
    (make-directory package-user-dir t))
  (let ((default-keyring (expand-file-name "package-keyring.gpg"
                                           data-directory))
        (inhibit-message (or inhibit-message async)))
    (when (and (package-check-signature) (file-exists-p default-keyring))
      (condition-case-unless-debug error
          (package-import-keyring default-keyring)
        (error (message "Cannot import default keyring: %s"
                        (error-message-string error))))))

  (run-hook-with-args 'package-refresh-contents-hook async))

(defun package--archives-initialize ()
  "Make sure the list of installed and remote packages are initialized."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents)))

(defun package-archive-priority (archive)
  "Return the priority of ARCHIVE.

The archive priorities are specified in
`package-archive-priorities'.  If not given there, the priority
defaults to 0."
  (or (cdr (assoc archive package-archive-priorities))
      0))

(defun package-desc-priority (pkg-desc)
  "Return the priority of the archive of package-desc object PKG-DESC."
  (package-archive-priority (package-desc-archive pkg-desc)))

(defun package-desc-priority-version (pkg-desc)
  "Return the version PKG-DESC with the archive priority prepended.

This allows for easy comparison of package versions from
different archives if archive priorities are meant to be taken in
consideration."
  (cons (package-desc-priority pkg-desc)
        (package-desc-version pkg-desc)))

(provide 'package-elpa)
;;; package-elpa.el ends here
