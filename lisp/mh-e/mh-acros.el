;;; mh-acros.el --- macros used in MH-E  -*- lexical-binding: t; -*-

;; Copyright (C) 2004, 2006-2026 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; This file contains all macros that are used in more than one file.
;; If you run "make recompile" in Bazaar Emacs and see the message
;; "Source is newer than compiled," it is a sign that macro probably
;; needs to be moved here.

;; Historically, it was so named with a silent "m" so that it would be
;; compiled first. Otherwise, "make recompile" in Bazaar Emacs would use
;; compiled files with stale macro definitions. Later, no-byte-compile
;; was added to the Local Variables section to avoid this problem and
;; because it's pointless to compile a file full of macros. But we
;; kept the name.

;;; Code:

(require 'cl-lib)



;;; Miscellaneous

;;;###mh-autoload
(defmacro with-mh-folder-updating (save-modification-flag &rest body)
  "Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG) &body BODY).
Execute BODY, which can modify the folder buffer without having to
worry about file locking or the read-only flag, and return its result.
If SAVE-MODIFICATION-FLAG is non-nil, the buffer's modification flag
is unchanged, otherwise it is cleared."
  (declare (debug t) (indent defun))
  (setq save-modification-flag (car save-modification-flag)) ; CL style
  `(prog1
       (let ((mh-folder-updating-mod-flag (buffer-modified-p))
             (buffer-read-only nil)
             (buffer-file-name nil))    ;don't let the buffer get locked
         (prog1
             (progn
               ,@body)
           (mh-set-folder-modified-p mh-folder-updating-mod-flag)))
     ,@(if (not save-modification-flag)
           '((mh-set-folder-modified-p nil)))))

;;;###mh-autoload
(defmacro mh-in-show-buffer (show-buffer &rest body)
  "Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
Display buffer SHOW-BUFFER in other window and execute BODY in it.
Stronger than `save-excursion', weaker than `save-window-excursion'."
  (declare (debug t) (indent defun))
  (setq show-buffer (car show-buffer))  ; CL style
  `(let ((mh-in-show-buffer-saved-window (selected-window)))
     (switch-to-buffer-other-window ,show-buffer)
     (if mh-bury-show-buffer-flag (bury-buffer (current-buffer)))
     (unwind-protect
         (progn
           ,@body)
       (select-window mh-in-show-buffer-saved-window))))

;;;###mh-autoload
(defmacro mh-do-at-event-location (event &rest body)
  "Switch to the location of EVENT and execute BODY.
After BODY has been executed return to original window.
The modification flag of the buffer in the event window is
preserved."
  (declare (debug t) (indent defun))
  (let ((event-window (make-symbol "event-window"))
        (event-position (make-symbol "event-position"))
        (original-window (make-symbol "original-window"))
        (original-position (make-symbol "original-position"))
        (modified-flag (make-symbol "modified-flag")))
    `(save-excursion
       (let* ((,event-window (posn-window (event-start ,event)))
              (,event-position (posn-point (event-start ,event)))
              (,original-window (selected-window))
              (,original-position (progn
                                   (set-buffer (window-buffer ,event-window))
                                   (point-marker)))
              (,modified-flag (buffer-modified-p))
              (buffer-read-only nil))
         (unwind-protect (progn
                           (select-window ,event-window)
                           (goto-char ,event-position)
                           ,@body)
           (set-buffer-modified-p ,modified-flag)
           (goto-char ,original-position)
           (set-marker ,original-position nil)
           (select-window ,original-window))))))



;;; Sequences and Ranges

;;;###mh-autoload
(defsubst mh-seq-msgs (sequence)
  "Extract messages from the given SEQUENCE."
  (cdr sequence))

;;;###mh-autoload
(defmacro mh-iterate-on-messages-in-region (var begin end &rest body)
  "Iterate over region.

VAR is bound to the message on the current line as we loop
starting from BEGIN till END.  In each step BODY is executed.

If VAR is nil then the loop is executed without any binding."
  (declare (debug (symbolp body)) (indent defun))
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var))
    `(save-excursion
       (goto-char ,begin)
       (beginning-of-line)
       (while (and (<= (point) ,end) (not (eobp)))
         (when (looking-at mh-scan-valid-regexp)
           (let ,(if binding-needed-flag `((,var (mh-get-msg-num t))) ())
             ,@body))
         (forward-line 1)))))

;;;###mh-autoload
(defmacro mh-iterate-on-range (var range &rest body)
  "Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over RANGE, which
can be a message number, a list of message numbers, a sequence, a
region in a cons cell, or a MH range (something like last:20) in
a string.  In each iteration, BODY is executed.

The parameter RANGE is usually created with
`mh-interactive-range' in order to provide a uniform interface to
MH-E functions."
  (declare (debug (symbolp body)) (indent defun))
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var)
        (msgs (make-symbol "msgs"))
        (seq-hash-table (make-symbol "seq-hash-table")))
    `(cond ((numberp ,range)
            (when (mh-goto-msg ,range t t)
              (let ,(if binding-needed-flag `((,var ,range)) ())
                ,@body)))
           ((and (consp ,range)
                 (numberp (car ,range)) (numberp (cdr ,range)))
            (mh-iterate-on-messages-in-region ,var
              (car ,range) (cdr ,range)
              ,@body))
           (t (let ((,msgs (cond ((and ,range (symbolp ,range))
                                  (mh-seq-to-msgs ,range))
                                 ((stringp ,range)
                                  (mh-translate-range mh-current-folder
                                                      ,range))
                                 (t ,range)))
                    (,seq-hash-table (make-hash-table)))
                (dolist (msg ,msgs)
                  (setf (gethash msg ,seq-hash-table) t))
                (mh-iterate-on-messages-in-region v (point-min) (point-max)
                  (when (gethash v ,seq-hash-table)
                    (let ,(if binding-needed-flag `((,var v)) ())
                      ,@body))))))))

(defmacro mh-dlet* (binders &rest body)
  "Like `let*' but always dynamically scoped."
  (declare (debug let) (indent 1))
  ;; Works in both lexical and non-lexical mode.
  `(progn
     (with-suppressed-warnings ((lexical
                                 ,@(mapcar (lambda (binder)
                                             (if (consp binder)
                                                 (car binder)
                                               binder))
                                           binders)))
       ,@(mapcar (lambda (binder)
                   `(defvar ,(if (consp binder) (car binder) binder)))
                 binders)
       (let* ,binders ,@body))))

;; Emacs 24 made flet obsolete and suggested either cl-flet or
;; cl-letf. This macro is based upon gmm-flet from Gnus.
(defmacro mh-flet (bindings &rest body)
  "Make temporary overriding function definitions.
That is, temporarily rebind the functions listed in BINDINGS and then
execute BODY.  BINDINGS is a list containing one or more lists of the
form (FUNCNAME ARGLIST BODY...), similar to defun."
  (declare (indent 1) (debug ((&rest (sexp sexp &rest form)) &rest form)))
  (if (fboundp 'cl-letf)
      `(cl-letf ,(mapcar (lambda (binding)
                           `((symbol-function ',(car binding))
                             (lambda ,@(cdr binding))))
                         bindings)
         ,@body)
    `(flet ,bindings ,@body)))

(provide 'mh-acros)

;; Local Variables:
;; sentence-end-double-space: nil
;; End:

;;; mh-acros.el ends here
