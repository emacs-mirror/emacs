;;; erc-d-u.el --- Helpers for ERC test server -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

;; The utilities here are kept separate from those in `erc-d' so that
;; tests running the server in a subprocess can use them without
;; having to require the main lib.  If migrating outside of test/lisp,
;; there may be no reason to continue this.
;;
;; Another (perhaps misguided) goal here is to avoid having ERC itself
;; as a dependency.
;;
;; FIXME this ^ is no longer the case (ERC is not a dependency)

;;; Code:
(require 'rx)
(require 'subr-x)
(eval-when-compile (require 'ert))

(defvar erc-d-u--canned-buffers nil
  "List of canned dialog buffers currently open for reading.")

(cl-defstruct (erc-d-u-scan-d) ; dialog scanner
  (buf nil :type buffer)
  (done nil :type boolean)
  (last nil :type integer)
  (hunks nil :type (list-of marker))
  (f #'erc-d-u--read-exchange-default :type function))

(cl-defstruct (erc-d-u-scan-e) ; exchange scanner
  (sd nil :type erc-d-u-scan-d)
  (pos nil :type marker))

(defun erc-d-u--read-dialog (info)
  "Read dialog file and stash relevant state in `erc-d-u-scan-d' INFO."
  (if (and (buffer-live-p (erc-d-u-scan-d-buf info))
           (with-current-buffer (erc-d-u-scan-d-buf info)
             (condition-case _err
                 (progn
                   (when (erc-d-u-scan-d-last info)
                     (goto-char (erc-d-u-scan-d-last info))
                     (forward-list))
                   (setf (erc-d-u-scan-d-last info) (point))
                   (down-list)
                   (push (set-marker (make-marker) (point))
                         (erc-d-u-scan-d-hunks info)))
               ((end-of-buffer scan-error)
                (setf (erc-d-u-scan-d-done info) t)
                nil))))
      (make-erc-d-u-scan-e :sd info :pos (car (erc-d-u-scan-d-hunks info)))
    (unless (erc-d-u-scan-d-hunks info)
      (kill-buffer (erc-d-u-scan-d-buf info))
      nil)))

(defun erc-d-u--read-exchange-default (info)
  "Read from marker in exchange `erc-d-u-scan-e' object INFO."
  (let ((hunks (erc-d-u-scan-e-sd info))
        (pos (erc-d-u-scan-e-pos info)))
    (or (and (erc-d-u-scan-d-hunks hunks)
             (buffer-live-p (erc-d-u-scan-d-buf hunks))
             (with-current-buffer (erc-d-u-scan-d-buf hunks)
               (goto-char pos)
               (condition-case _err
                   (read pos)
                 ;; Raised unless malformed
                 (invalid-read-syntax
                  nil))))
        (unless (or (cl-callf (lambda (s) (delq pos s)) ; flip
                        (erc-d-u-scan-d-hunks hunks))
                    (not (erc-d-u-scan-d-done hunks)))
          (kill-buffer (erc-d-u-scan-d-buf hunks))
          nil))))

(defun erc-d-u--read-exchange (info)
  "Call exchange reader assigned in `erc-d-u-scan-e' object INFO."
  (funcall (erc-d-u-scan-d-f (erc-d-u-scan-e-sd info)) info))

(defun erc-d-u--canned-read (file)
  "Dispense a reader for each exchange in dialog FILE."
  (let ((buf (generate-new-buffer (file-name-nondirectory file))))
    (push buf erc-d-u--canned-buffers)
    (with-current-buffer buf
      (setq-local parse-sexp-ignore-comments t
                  coding-system-for-read 'utf-8)
      (add-hook 'kill-buffer-hook
                (lambda () (setq erc-d-u--canned-buffers
                                 (delq buf erc-d-u--canned-buffers)))
                nil 'local)
      (insert-file-contents-literally file)
      (lisp-data-mode))
    (make-erc-d-u-scan-d :buf buf)))

(defvar erc-d-u--library-directory (file-name-directory load-file-name))
(defvar erc-d-u-canned-dialog-dir
  (file-name-as-directory (expand-file-name "resources"
                                            erc-d-u--library-directory)))

(defun erc-d-u--normalize-canned-name (dialog)
  "Return DIALOG name as a symbol without validating it."
  (if (symbolp dialog)
      dialog
    (intern (file-name-base dialog))))

(defvar erc-d-u-canned-file-name-extension ".eld")

(defun erc-d-u--expand-dialog-symbol (dialog)
  "Return filename based on symbol DIALOG."
  (let ((name (symbol-name dialog)))
    (unless (equal (file-name-extension name)
                   erc-d-u-canned-file-name-extension)
      (setq name (concat name erc-d-u-canned-file-name-extension)))
    (expand-file-name name erc-d-u-canned-dialog-dir)))

(defun erc-d-u--massage-canned-name (dialog)
  "Return DIALOG in a form acceptable to `erc-d-run'."
  (if (or (symbolp dialog) (file-exists-p dialog))
      dialog
    (erc-d-u--expand-dialog-symbol (intern dialog))))

(defun erc-d-u--canned-load-dialog (dialog)
  "Load dispensing exchanges from DIALOG.
If DIALOG is a string, consider it a filename.  Otherwise find a file
in `erc-d-u-canned-dialog-dir' with a base name matching the symbol's
name.

Return an iterator that yields exchanges, each one an iterator of spec
forms.  The first is a so-called request spec and the rest are composed
of zero or more response specs."
  (when (symbolp dialog)
    (setq dialog (erc-d-u--expand-dialog-symbol dialog)))
  (unless (file-exists-p dialog)
    (error "File not found: %s" dialog))
  (erc-d-u--canned-read dialog))

(defun erc-d-u--read-exchange-slowly (num orig info)
  (when-let* ((spec (funcall orig info)))
    (when (symbolp (car spec))
      (setf spec (copy-sequence spec)
            (nth 1 spec) (cond ((functionp num) (funcall num (nth 1 spec)))
                               ((< num 0) (max (nth 1 spec) (- num)))
                               (t (+ (nth 1 spec) num)))))
    spec))

(defun erc-d-u--rewrite-for-slow-mo (num read-info)
  "Return READ-INFO with a modified reader.
When NUM is a positive number, delay incoming requests by NUM more
seconds.  If NUM is negative, raise insufficient incoming delays to at
least -NUM seconds.  If NUM is a function, set each delay to whatever it
returns when called with the existing value."
  (let ((orig (erc-d-u-scan-d-f read-info)))
    (setf (erc-d-u-scan-d-f read-info)
          (apply-partially #'erc-d-u--read-exchange-slowly num orig))
    read-info))

(defun erc-d-u--get-remote-port (process)
  "Return peer TCP port for client PROCESS.
When absent, just generate an id."
  (let ((remote (plist-get (process-contact process t) :remote)))
    (if (vectorp remote)
        (aref remote (1- (length remote)))
      (format "%s:%d" (process-contact process :local)
              (logand 1023 (time-convert nil 'integer))))))

(defun erc-d-u--format-bind-address (process)
  "Return string or (STRING . INT) for bind address of network PROCESS."
  (let ((local (process-contact process :local)))
    (if (vectorp local) ; inet
        (cons (mapconcat #'number-to-string (seq-subseq local 0 -1) ".")
              (aref local (1- (length local))))
      local)))

(defun erc-d-u--unkeyword (plist)
  "Return a copy of PLIST with keywords keys converted to non-keywords."
  (cl-loop for (key value) on plist by #'cddr
           when (keywordp key)
           do (setq key (intern (substring (symbol-name key) 1)))
           append (list key value)))

(defun erc-d-u--massage-rx-args (key val)
  " Massage val so it's suitable for an `rx-let' binding.
Handle cases in which VAL is ([ARGLIST] RX-FORM) rather than just
RX-FORM.  KEY becomes the binding name."
  (if (and (listp val)
           (cdr val)
           (not (cddr val))
           (consp (car val)))
      (cons key val)
    (list key val)))

(defvar-local erc-d-u--process-buffer nil
  "Beacon for erc-d process buffers.
The server process is usually deleted first, but we may want to examine
the buffer afterward.")

(provide 'erc-d-u)
;;; erc-d-u.el ends here
