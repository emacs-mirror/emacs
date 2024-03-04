;;; string-edit.el --- editing long strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(defface string-edit-prompt
  '((t (:inherit font-lock-comment-face)))
  "Face used on `string-edit' help text."
  :group 'text
  :version "29.1")

(defvar string-edit--success-callback)
(defvar string-edit--abort-callback)

;;;###autoload
(cl-defun string-edit (prompt string success-callback
                              &key abort-callback)
  "Switch to a new buffer to edit STRING.
When the user finishes editing (with \\<string-edit-mode-map>\\[string-edit-done]), SUCCESS-CALLBACK
is called with the resulting string.

If the user aborts (with \\<string-edit-mode-map>\\[string-edit-abort]), ABORT-CALLBACK (if any) is
called with no parameters.

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If PROMPT is nil, no help text
will be inserted.

Also see `read-string-from-buffer'."
  (with-current-buffer (generate-new-buffer "*edit string*")
    (when prompt
      (let ((inhibit-read-only t))
        (insert prompt)
        (ensure-empty-lines 0)
        (add-text-properties (point-min) (point)
                             (list 'intangible t
                                   'face 'string-edit-prompt
                                   'read-only t))
        (insert (propertize (make-separator-line) 'rear-nonsticky t))
        (add-text-properties (point-min) (point)
                             (list 'string-edit--prompt t))))
    (let ((start (point)))
      (insert string)
      (goto-char start))

    ;; Use `fit-window-to-buffer' after the buffer is filled with text.
    (pop-to-buffer (current-buffer)
                   '(display-buffer-below-selected
                     (window-height . (lambda (window)
                                        (fit-window-to-buffer window nil 10)))))

    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (string-edit-mode)
    (setq-local string-edit--success-callback success-callback)
    (when abort-callback
      (setq-local string-edit--abort-callback abort-callback))
    (setq-local header-line-format
                (substitute-command-keys
                 "Type \\<string-edit-mode-map>\\[string-edit-done] when you've finished editing or \\[string-edit-abort] to abort"))
    (message "%s" (substitute-command-keys
                   "Type \\<string-edit-mode-map>\\[string-edit-done] when you've finished editing"))))

;;;###autoload
(defun read-string-from-buffer (prompt string)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<string-edit-mode-map>\\[string-edit-done], or aborts with \\<string-edit-mode-map>\\[string-edit-abort]).

PROMPT will be inserted at the start of the buffer, but won't be
included in the resulting string.  If nil, no prompt will be
inserted in the buffer.

Also see `string-edit'."
  (string-edit
   prompt
   string
   (lambda (edited)
     (setq string edited)
     (exit-recursive-edit))
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
  (text-property-search-forward 'string-edit--prompt)
  (let ((string (buffer-substring (point) (point-max)))
        (callback string-edit--success-callback))
    (quit-window 'kill)
    (funcall callback string)))

(defun string-edit-abort ()
  "Abort editing the current string."
  (interactive)
  (let ((callback string-edit--abort-callback))
    (quit-window 'kill)
    (when callback
      (funcall callback))))

(provide 'string-edit)

;;; string-edit.el ends here
