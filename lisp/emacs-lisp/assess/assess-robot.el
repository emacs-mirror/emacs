;;; assess-robot.el --- Test support functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.2

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2016, Phillip Lord

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(defmacro assess-robot-with-switched-buffer (buffer &rest body)
  "With BUFFER, evaluate BODY.

This macro is rather like `with-current-buffer', except that it
uses `switch-to-buffer'. This is generally a bad idea when used
programmatically. But, it is necessary, for example, when using
keyboard macros."
  (declare (indent 1) (debug t))
  (let ((before-buffer (make-symbol "before-buffer")))
    `(let ((,before-buffer (current-buffer)))
       (unwind-protect
           (progn
             (switch-to-buffer ,buffer)
             ,@body)
         (switch-to-buffer ,before-buffer)))))

(defmacro assess-robot-with-temp-switched-buffer (&rest body)
  "Evaluate BODY in temporary buffer.

As with `assess-robot-with-switched-buffer', `switch-to-buffer'
is used."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       (assess-robot-with-switched-buffer ,temp-buffer
         (unwind-protect
             (progn
               ;; Enable the undo list because we want it for most robot
               ;; situations.
               (setq buffer-undo-list nil)
               ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

(defmacro assess-robot-with-switched-buffer-string (&rest body)
  "Evalate BODY in a temporary buffer and return buffer string.

See also `assess-robot-with-temp-switched-buffer'."
  (declare (debug t))
  `(assess-robot-with-temp-switched-buffer
     (progn
       ,@body
       (buffer-substring-no-properties
        (point-min) (point-max)))))

(defun assess-robot-execute-kmacro (macro)
  "Execute the MACRO.

In this case, MACRO is the \"long form\" accepted by
`edit-kdb-macro'."
  (let ((macro (read-kbd-macro macro)))
    ;; I wanted to add a nice way to edit the macro, but sadly
    ;; edit-kdb-macro provides no nice entry point. So, we take the nasty step
    ;; of setting the last-kbd-macro instead.
    (setq last-kbd-macro macro)
    (execute-kbd-macro
     (read-kbd-macro macro))))

(defun assess-robot-copy-and-finish ()
  "Copy the macro in edmacro to the kill-ring."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "Macro:")
    (forward-line)
    (let ((string
           (buffer-substring-no-properties
            (point)
            (point-max))))
      (with-temp-buffer
        (insert "\"")
        (insert string)
        (insert "\"")
        (kill-ring-save (point-min)
                        (point-max))))
    (edmacro-finish-edit)))

(eval-after-load
    'edmacro
  '(define-key edmacro-mode-map (kbd "C-c C-k") 'assess-robot-copy-and-finish))

(provide 'assess-robot)
;;; assess-robot.el ends here
