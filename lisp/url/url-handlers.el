;;; url-handlers.el --- file-name-handler stuff for URL loading  -*- lexical-binding:t -*-

;; Copyright (C) 1996-1999, 2004-2020 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;;; Code:

(require 'url-parse)
(eval-when-compile (require 'mm-decode))
(eval-when-compile (require 'subr-x))
;; The following are autoloaded instead of `require'd to avoid eagerly
;; loading all of URL when turning on url-handler-mode in the .emacs.
(autoload 'url-expand-file-name "url-expand"
  "Convert URL to a fully specified URL, and canonicalize it.")
(autoload 'mm-dissect-buffer "mm-decode"
  "Dissect the current buffer and return a list of MIME handles.")
(autoload 'url-scheme-get-property "url-methods"
  "Get PROPERTY of a URL SCHEME.")

;; Always used after mm-dissect-buffer and defined in the same file.
(declare-function mm-save-part-to-file "mm-decode" (handle file))
(declare-function mm-destroy-parts "mm-decode" (handles))
;; mm-decode loads mm-bodies.
(declare-function mm-decode-string "mm-bodies" (string charset))
;; mm-decode loads mail-parse.
(declare-function mail-content-type-get "mail-parse" (ct attribute))
;; mm-decode loads mm-bodies, which loads mm-util.
(declare-function mm-charset-to-coding-system "mm-util"
                  (charset &optional lbt allow-override silent))

;; Implementation status
;; ---------------------
;; Function				Status
;; ------------------------------------------------------------
;; add-name-to-file			Needs DAV Bindings
;; copy-file				Broken (assumes 1st item is URL)
;; delete-directory			Finished (DAV)
;; delete-file				Finished (DAV)
;; diff-latest-backup-file
;; directory-file-name			unnecessary
;; directory-files			Finished (DAV)
;; dired-call-process
;; dired-compress-file
;; dired-uncache
;; expand-file-name			Finished
;; file-accessible-directory-p
;; file-attributes			Finished, better with DAV
;; file-directory-p			Needs DAV, finished
;; file-executable-p			Finished
;; file-exists-p			Finished
;; file-local-copy
;; file-modes
;; file-name-all-completions		Finished (DAV)
;; file-name-as-directory
;; file-name-completion			Finished (DAV)
;; file-name-directory
;; file-name-nondirectory
;; file-name-sans-versions		why?
;; file-newer-than-file-p
;; file-ownership-preserved-p		No way to know
;; file-readable-p			Finished
;; file-regular-p			!directory_p
;; file-remote-p			Finished
;; file-symlink-p			Needs DAV bindings
;; file-truename			Needs DAV bindings
;; file-writable-p			Check for LOCK?
;; find-backup-file-name		why?
;; get-file-buffer			why?
;; insert-directory			Use DAV
;; insert-file-contents			Finished
;; load
;; make-directory			Finished (DAV)
;; make-symbolic-link			Needs DAV bindings
;; rename-file				Finished (DAV)
;; set-file-modes			Use mod_dav specific executable flag?
;; set-visited-file-modtime		Impossible?
;; shell-command			Impossible?
;; unhandled-file-name-directory
;; vc-registered			Finished (DAV)
;; verify-visited-file-modtime
;; write-region

(defvar url-handler-regexp) ; defined below to avoid recursive load (revno:108572)

;;;###autoload
(define-minor-mode url-handler-mode
  "Toggle using `url' library for URL filenames (URL Handler mode)."
  :global t :group 'url
  ;; Remove old entry, if any.
  (setq file-name-handler-alist
	(delq (rassq 'url-file-handler file-name-handler-alist)
	      file-name-handler-alist))
  (if url-handler-mode
      (push (cons url-handler-regexp 'url-file-handler)
	    file-name-handler-alist)))

(defcustom url-handler-regexp
  "\\`\\(?:https?\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://"
  "Regular expression for URLs handled by `url-handler-mode'.
When URL Handler mode is enabled, this regular expression is
added to `file-name-handler-alist'.

Some valid URL protocols just do not make sense to visit
interactively (about, data, info, irc, mailto, etc.).  This
regular expression avoids conflicts with local files that look
like URLs (Gnus is particularly bad at this)."
  :group 'url
  :type 'regexp
  :version "25.1"
  :set (lambda (symbol value)
	 (let ((enable url-handler-mode))
	   (url-handler-mode 0)
	   (set-default symbol value)
	   (if enable
	       (url-handler-mode)))))

(defun url-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers (cons 'url-file-handler
					  (if (eq operation inhibit-file-name-operation)
					      inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defvar url-file-handler-load-in-progress nil
  "Check for recursive load.")

;;;###autoload
(defun url-file-handler (operation &rest args)
  "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc.).
ARGS are the arguments that would have been passed to OPERATION."
  ;; Avoid recursive load.
  (if (and load-in-progress url-file-handler-load-in-progress)
      (url-run-real-handler operation args)
    (let ((url-file-handler-load-in-progress load-in-progress))
      ;; Check, whether there are arguments we want pass to Tramp.
      (if (catch :do
            (dolist (url (cons default-directory args))
              (and (stringp url)
                   (member (url-type (url-generic-parse-url url))
                           url-tramp-protocols)
                   (throw :do t))))
          (apply #'url-tramp-file-handler operation args)
        ;; Otherwise, let's do the job.
        (let ((fn (get operation 'url-file-handlers))
              val)
          (if (and (not fn)
                   (fboundp (intern-soft (format "url-%s" operation))))
              (error "Missing URL handler mapping for %s" operation))
          (setq val (if fn (save-match-data (apply fn args))
                      (url-run-real-handler operation args)))
          (url-debug 'handlers "%s %S%S => %S" (if fn "Hooked" "Real")
                     operation args val)
          val)))))

(defun url-file-handler-identity (arg &rest _ignored)
  ;; Identity function.
  arg)

;; These are operations that we can fully support.
(put 'file-readable-p 'url-file-handlers #'url-file-exists-p)
(put 'substitute-in-file-name 'url-file-handlers #'url-file-handler-identity)
(put 'file-name-absolute-p 'url-file-handlers (lambda (&rest _ignored) t))
(put 'expand-file-name 'url-file-handlers #'url-handler-expand-file-name)
(put 'directory-file-name 'url-file-handlers #'url-handler-directory-file-name)
(put 'file-name-directory 'url-file-handlers #'url-handler-file-name-directory)
(put 'unhandled-file-name-directory 'url-file-handlers
     #'url-handler-unhandled-file-name-directory)
(put 'file-remote-p 'url-file-handlers #'url-handler-file-remote-p)
;; (put 'file-name-as-directory 'url-file-handlers
;;      #'url-handler-file-name-as-directory)

;; These are operations that we do not support yet (DAV!!!)
(put 'file-writable-p 'url-file-handlers #'ignore)
(put 'file-symlink-p 'url-file-handlers #'ignore)
;; Just like for ange-ftp: let's not waste time trying to look for RCS/foo,v
;; files and such since we can't do anything clever with them anyway.
(put 'vc-registered 'url-file-handlers #'ignore)

(defun url-handler-expand-file-name (file &optional base)
  ;; When we see "/foo/bar" in a file whose working dir is "http://bla/bla",
  ;; there are two interpretations possible: either it's a local "/foo/bar"
  ;; or it's "http:/bla/foo/bar".  When working with URLs, the second
  ;; interpretation is the right one, but when working with Emacs file
  ;; names, the first is preferred.
  (if (file-name-absolute-p file)
      (expand-file-name file "/")
    (url-expand-file-name file base)))

;; directory-file-name and file-name-as-directory are kind of hard to
;; implement really right for URLs since URLs can have repeated / chars.
;; We'd want the following behavior:
;; idempotence: (d-f-n (d-f-n X) == (d-f-n X)
;; idempotence: (f-n-a-d (f-n-a-d X) == (f-n-a-d X)
;; reversible:  (d-f-n (f-n-a-d (d-f-n X))) == (d-f-n X)
;; reversible:  (f-n-a-d (d-f-n (f-n-a-d X))) == (f-n-a-d X)
(defun url-handler-directory-file-name (dir)
  ;; When there's more than a single /, just don't touch the slashes at all.
  (if (string-suffix-p "//" dir) dir
    (url-run-real-handler 'directory-file-name (list dir))))

(defun url-handler-unhandled-file-name-directory (filename)
  (let ((url (url-generic-parse-url filename)))
    (if (equal (url-type url) "file")
        ;; `file' URLs are actually local.  The filename part may be ""
        ;; which really stands for "/".
        ;; FIXME: maybe we should check that the host part is "" or "localhost"
        ;; or some name that represents the local host?
        (or (file-name-as-directory (url-filename url)) "/")
      ;; All other URLs are not expected to be directly accessible from
      ;; a local process.
      nil)))

(defun url-handler-file-name-directory (dir)
  (let ((url (url-generic-parse-url dir)))
    ;; Do not attempt to handle `file' URLs which are local.
    (if (and (not (equal (url-type url) "file"))
	     (string-empty-p (url-filename url)))
	(url-handler-file-name-directory (concat dir "/"))
      (url-run-real-handler 'file-name-directory (list dir)))))

(defun url-handler-file-remote-p (filename &optional identification _connected)
  (let ((url (url-generic-parse-url filename)))
    (if (and (url-type url) (not (equal (url-type url) "file")))
	;; Maybe we can find a suitable check for CONNECTED.  For now,
	;; we ignore it.
	(cond
	 ((eq identification 'method) (url-type url))
	 ((eq identification 'user) (url-user url))
	 ((eq identification 'host) (url-host url))
	 ((eq identification 'localname) (url-filename url))
	 (t (url-recreate-url
	     (url-parse-make-urlobj (url-type url) (url-user url) nil
				    (url-host url) (url-port url)))))
      ;; If there is no URL type, or it is a "file://" URL, the
      ;; filename is expected to be non remote.  A more subtle check
      ;; for "file://" URLs could be applied, as said in
      ;; `url-handler-unhandled-file-name-directory'.
      nil)))

;; The actual implementation.
;;;###autoload
(defun url-copy-file (url newname &optional ok-if-already-exists &rest _ignored)
  "Copy URL to NEWNAME.  Both arguments must be strings.
Signal a `file-already-exists' error if file NEWNAME already
exists, unless a third argument OK-IF-ALREADY-EXISTS is supplied
and non-nil.  An integer as third argument means request
confirmation if NEWNAME already exists."
  (and (file-exists-p newname)
       (or (not ok-if-already-exists)
           (and (integerp ok-if-already-exists)
                (not (yes-or-no-p
                      (format "File %s already exists; copy to it anyway? "
                              newname)))))
       (signal 'file-already-exists (list "File already exists" newname)))
  (let* ((buffer (or (url-retrieve-synchronously url)
                     (signal 'file-missing
                             (list "Opening URL"
                                   "No such file or directory" url))))
         (handle (with-current-buffer buffer
                   (mm-dissect-buffer t))))
    (let ((mm-attachment-file-modes (default-file-modes)))
      (mm-save-part-to-file handle newname))
    (kill-buffer buffer)
    (mm-destroy-parts handle)))
(put 'copy-file 'url-file-handlers #'url-copy-file)

;;;###autoload
(defun url-file-local-copy (url &rest _ignored)
  "Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  (let ((filename (make-temp-file "url")))
    (url-copy-file url filename 'ok-if-already-exists)
    filename))
(put 'file-local-copy 'url-file-handlers #'url-file-local-copy)

(defun url-insert (buffer &optional beg end)
  "Insert the body of a URL object.
BUFFER should be a complete URL buffer as returned by `url-retrieve'.
If the headers specify a coding-system (and current buffer is multibyte),
it is applied to the body before it is inserted.
Returns a list of the form (SIZE CHARSET), where SIZE is the size in bytes
of the inserted text and CHARSET is the charset that was specified in the header,
or nil if none was found.
BEG and END can be used to only insert a subpart of the body.
They count bytes from the beginning of the body."
  (let* ((handle (with-current-buffer buffer (mm-dissect-buffer t)))
         (data (with-current-buffer (mm-handle-buffer handle)
                 (if beg
                     (buffer-substring (+ (point-min) beg)
                                       (if end (+ (point-min) end) (point-max)))
		   (buffer-string))))
         (charset (if enable-multibyte-characters
                      (mail-content-type-get (mm-handle-type handle)
                                             'charset))))
    (mm-destroy-parts handle)
    (insert (if charset
                (mm-decode-string data (mm-charset-to-coding-system charset))
              data))
    (list (length data) charset)))

(defvar url-http-codes)

;;;###autoload
(defun url-insert-buffer-contents (buffer url &optional visit beg end replace)
  "Insert the contents of BUFFER into current buffer.
This is like `url-insert', but also decodes the current buffer as
if it had been inserted from a file named URL."
  (if visit (setq buffer-file-name url))
  (save-excursion
    (let ((start (point))
          (size-and-charset (url-insert buffer beg end)))
      (kill-buffer buffer)
      (when replace
        (delete-region (point-min) start)
        (delete-region (point) (point-max)))
      (unless (cadr size-and-charset)
        ;; If the headers don't specify any particular charset, use the
        ;; usual heuristic/rules that we apply to files.
        (decode-coding-inserted-region (point-min) (point) url
                                       visit beg end replace))
      (let ((inserted (car size-and-charset)))
        (list url (or (after-insert-file-set-coding inserted visit)
                      inserted))))))

;;;###autoload
(defun url-insert-file-contents (url &optional visit beg end replace)
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer (signal 'file-error (list url "No Data")))
    (when (fboundp 'url-http--insert-file-helper)
      ;; XXX: This is HTTP/S specific and should be moved to url-http
      ;; instead.  See bug#17549.
      (url-http--insert-file-helper buffer url visit))
    (url-insert-buffer-contents buffer url visit beg end replace)))
(put 'insert-file-contents 'url-file-handlers #'url-insert-file-contents)

(defun url-file-name-completion (url _directory &optional _predicate)
  ;; Even if it's not implemented, it's not an error to ask for completion,
  ;; in case it's available (bug#14806).
  ;; (error "Unimplemented")
  url)
(put 'file-name-completion 'url-file-handlers #'url-file-name-completion)

(defun url-file-name-all-completions (_file _directory)
  ;; Even if it's not implemented, it's not an error to ask for completion,
  ;; in case it's available (bug#14806).
  ;; (error "Unimplemented")
  nil)
(put 'file-name-all-completions
     'url-file-handlers #'url-file-name-all-completions)

;; All other handlers map onto their respective backends.
(defmacro url-handlers-create-wrapper (method args)
  `(progn
     (defun ,(intern (format "url-%s" method)) ,args
       ,(format "URL file-name-handler wrapper for `%s' call.\n---\n%s" method
                (or (documentation method t) "No original documentation."))
       (setq url (url-generic-parse-url url))
       (when (url-type url)
         (funcall (url-scheme-get-property (url-type url) ',method)
                  ,@(remq '&rest (remq '&optional args)))))
     (unless (get ',method 'url-file-handlers)
       (put ',method 'url-file-handlers #',(intern (format "url-%s" method))))))

(url-handlers-create-wrapper file-exists-p (url))
(url-handlers-create-wrapper file-attributes (url &optional id-format))
(url-handlers-create-wrapper file-symlink-p (url))
(url-handlers-create-wrapper file-writable-p (url))
(url-handlers-create-wrapper file-directory-p (url))
(url-handlers-create-wrapper file-executable-p (url))
(url-handlers-create-wrapper directory-files (url &optional full match nosort))
(url-handlers-create-wrapper file-truename (url &optional counter prev-dirs))

(add-hook 'find-file-hook #'url-handlers-set-buffer-mode)

(defun url-handlers-set-buffer-mode ()
  "Set correct modes for the current buffer if visiting a remote file."
  (and buffer-file-name
       (string-match-p url-handler-regexp buffer-file-name)
       (auto-save-mode 0)))

(provide 'url-handlers)

;;; url-handlers.el ends here
