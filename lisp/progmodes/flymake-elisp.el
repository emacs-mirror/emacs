;;; flymake-elisp.el --- Flymake backends for emacs-lisp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: languages tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backends for elisp work.

;;; Code:
(require 'flymake)
(require 'checkdoc)
(eval-when-compile (require 'cl-lib))
(require 'bytecomp)

(defun flymake-elisp--checkdoc-1 ()
  "Do actual work for `flymake-elisp-checkdoc'."
  (let (collected)
    (cl-letf (((symbol-function 'checkdoc-create-error)
               (lambda (text start end &optional unfixable)
                 (push (list text start end unfixable) collected)
                 nil)))
      (let* ((checkdoc-autofix-flag nil)
             (checkdoc-generate-compile-warnings-flag nil)
             (buf (generate-new-buffer " *checkdoc-temp*"))
             (checkdoc-diagnostic-buffer buf))
        (unwind-protect
            (save-excursion
              (checkdoc-current-buffer t))
          (kill-buffer buf))))
    collected))

(defun flymake-elisp-checkdoc (report-fn)
  "A flymake backend for `checkdoc'.
Calls REPORT-FN directly."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (error "Can only work on `emacs-lisp-mode' buffers"))
  (funcall report-fn
           (cl-loop for (text start end _unfixable) in
                    (flymake-elisp--checkdoc-1)
                    collect
                    (flymake-make-diagnostic
                     (current-buffer)
                     start end :note text))))

(defun flymake-elisp--byte-compile-done (report-fn
                                         origin-buffer
                                         output-buffer
                                         temp-file)
  (unwind-protect
      (with-current-buffer
          origin-buffer
        (save-excursion
          (save-restriction
            (widen)
            (funcall
             report-fn
             (ignore-errors
               (cl-loop with data =
                        (with-current-buffer output-buffer
                          (goto-char (point-min))
                          (search-forward ":flymake-elisp-output-start")
                          (read (point-marker)))
                        for (string pos _fill level) in data
                        do (goto-char pos)
                        for beg = (if (< (point) (point-max))
                                      (point)
                                    (line-beginning-position))
                        for end = (min
                                   (line-end-position)
                                   (or (cdr
                                        (bounds-of-thing-at-point 'sexp))
                                       (point-max)))
                        collect (flymake-make-diagnostic
                                 (current-buffer)
                                 (if (= beg end) (1- beg) beg)
                                 end
                                 level
                                 string)))))))
    (kill-buffer output-buffer)
    (ignore-errors (delete-file temp-file))))

(defvar-local flymake-elisp--byte-compile-process nil
  "Buffer-local process started for byte-compiling the buffer.")

(defun flymake-elisp-byte-compile (report-fn)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (interactive (list (lambda (stuff)
                       (message "aha %s" stuff))))
  (unless (derived-mode-p 'emacs-lisp-mode)
    (error "Can only work on `emacs-lisp-mode' buffers"))
  (when flymake-elisp--byte-compile-process
    (process-put flymake-elisp--byte-compile-process 'flymake-elisp--obsolete t)
    (when (process-live-p flymake-elisp--byte-compile-process)
      (kill-process flymake-elisp--byte-compile-process)))
  (let ((temp-file (make-temp-file "flymake-elisp-byte-compile"))
        (origin-buffer (current-buffer)))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *flymake-elisp-byte-compile*")))
      (setq
       flymake-elisp--byte-compile-process
       (make-process
        :name "flymake-elisp-byte-compile"
        :buffer output-buffer
        :command (list (expand-file-name invocation-name invocation-directory)
                       "-Q"
                       "--batch"
                       ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                       "-L" default-directory
                       "-l" "flymake-elisp"
                       "-f" "flymake-elisp--batch-byte-compile"
                       temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((zerop (process-exit-status proc))
                  (flymake-elisp--byte-compile-done report-fn
                                                    origin-buffer
                                                    output-buffer
                                                    temp-file))
                 ((process-get proc 'flymake-elisp--obsolete)
                  (flymake-log 3 "proc %s considered obsolete" proc))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation (format "proc %s died violently" proc)))))))))
      :stderr null-device
      :noquery t)))

(defun flymake-elisp--batch-byte-compile (&optional file)
  "Helper for `flymake-elisp-byte-compile'.
Runs in a batch-mode Emacs.  Interactively use variable
`buffer-file-name' for FILE."
  (interactive (list buffer-file-name))
  (let* ((file (or file
                   (car command-line-args-left)))
         (dummy-elc-file)
         (byte-compile-log-buffer
          (generate-new-buffer " *dummy-byte-compile-log-buffer*"))
         (byte-compile-dest-file-function
          (lambda (source)
            (setq dummy-elc-file (make-temp-file (file-name-nondirectory source)))))
         (collected))
    (unwind-protect
        (cl-letf (((symbol-function 'byte-compile-log-warning)
                   (lambda (string &optional fill level)
                     (push (list string byte-compile-last-position fill level)
                           collected)
                     t)))
          (byte-compile-file file))
      (ignore-errors
        (delete-file dummy-elc-file)
        (kill-buffer byte-compile-log-buffer)))
    (prin1 :flymake-elisp-output-start)
    (terpri)
    (pp collected)))

(defun flymake-elisp-setup-backends ()
  "Setup flymake for elisp work."
  (add-hook 'flymake-diagnostic-functions 'flymake-elisp-checkdoc t t)
  (add-hook 'flymake-diagnostic-functions 'flymake-elisp-byte-compile t t))

(add-hook 'emacs-lisp-mode-hook
          'flymake-elisp-setup-backends)

(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'emacs-lisp-mode)
      (flymake-elisp-setup-backends))))

(provide 'flymake-elisp)
;;; flymake-elisp.el ends here
