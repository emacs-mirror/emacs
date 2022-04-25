;;; string-edit.el --- editing long strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

;;; Code:

(require 'cl-lib)

(defface string-edit-help-text
  '((t (:inherit font-lock-comment-face)))
  "Face used on `string-edit' help text."
  :group 'text
  :version "29.1")

(defvar string-edit--success-callback)
(defvar string-edit--abort-callback)

(cl-defun string-edit (string success-callback
                              &key abort-callback help-text)
  "Switch to a new buffer to edit STRING.
When the user finishes editing (with \\<string-edit-mode-map>\\[string-edit-done]), SUCCESS-CALLBACK
is called with the resulting string.

If the user aborts (with \\<string-edit-mode-map>\\[string-edit-abort]), ABORT-CALLBACK (if any) is
called with no parameters.

If present, HELP-TEXT will be inserted at the start of the
buffer, but won't be included in the resulting string."
  (pop-to-buffer-same-window (generate-new-buffer "*edit string*"))
  (when help-text
    (let ((inhibit-read-only t))
      (insert help-text)
      (ensure-empty-lines 0)
      (add-text-properties (point-min) (point)
                           (list 'intangible t
                                 'face 'string-edit-help-text
                                 'read-only t))
      (insert (propertize (make-separator-line) 'rear-nonsticky t))
      (add-text-properties (point-min) (point)
                           (list 'string-edit--help-text t))))
  (let ((start (point)))
    (insert string)
    (goto-char start))
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (string-edit-mode)
  (setq-local string-edit--success-callback success-callback)
  (when abort-callback
    (setq-local string-edit--abort-callback abort-callback))
  (message "%s" (substitute-command-keys
                 "Type \\<string-edit-mode-map>\\[string-edit-done] when you've finished editing")))

(defun read-string-from-buffer (string &optional help-text)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<string-edit-mode-map>\\[string-edit-done], or aborts with \\<string-edit-mode-map>\\[string-edit-abort]).

If present, HELP-TEXT will be inserted at the start of the
buffer, but won't be included in the resulting string."
  (string-edit
   string
   (lambda (edited)
     (setq string edited)
     (exit-recursive-edit))
   :help-text help-text
   :abort-callback (lambda ()
                     (exit-recursive-edit)
                     (error "Aborted edit")))
  (recursive-edit)
  string)

(defvar-keymap string-edit-mode-map
  "C-c C-c" #'string-edit-done
  "C-c C-k" #'string-edit-abort)

(define-derived-mode string-edit-mode text-mode "String"
  "Mode for editing strings."
  :interactive nil)

(defun string-edit-done ()
  "Finish editing the string and call the callback function.
This will kill the current buffer."
  (interactive)
  (goto-char (point-min))
  ;; Skip past the help text.
  (when-let ((match (text-property-search-forward
                     'string-edit--help-text nil t)))
    (goto-char (prop-match-beginning match)))
  (let ((string (buffer-substring (point) (point-max)))
        (callback string-edit--success-callback))
    (kill-buffer (current-buffer))
    (funcall callback string)))

(defun string-edit-abort ()
  "Abort editing the current string."
  (interactive)
  (let ((callback string-edit--abort-callback))
    (kill-buffer (current-buffer))
    (when callback
      (funcall callback))))

(provide 'string-edit)

;;; string-edit.el ends here
