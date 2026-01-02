;;; string-edit.el --- editing long strings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(defvar string-edit--read)

;;;###autoload
(cl-defun string-edit (prompt string success-callback
                              &key abort-callback major-mode-sym read)
  "Switch to a new buffer to edit STRING.

Call MAJOR-MODE-SYM (defaulting to `string-edit-mode') to set up the new
buffer, and insert PROMPT (defaulting to nothing) at the start of the
buffer.

When the user finishes editing (with \\<string-edit-minor-mode-map>\\[string-edit-done]), call
READ (defaulting to `identity') on the resulting string, omitting PROMPT if any.

If READ returns without an error, quit the buffer and call
SUCCESS-CALLBACK on the result.

If the user aborts (with \\<string-edit-minor-mode-map>\\[string-edit-abort]),
call ABORT-CALLBACK (if any) with no parameters.

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
        (insert (propertize (make-separator-line)
                            'read-only t 'rear-nonsticky t))
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
    (funcall (or major-mode-sym #'string-edit-mode))
    (string-edit-minor-mode)
    (setq-local string-edit--success-callback success-callback)
    (setq-local string-edit--abort-callback abort-callback)
    (setq-local string-edit--read read)
    (setq-local header-line-format
                (substitute-command-keys
                 "Type \\<string-edit-minor-mode-map>\\[string-edit-done] when you've finished editing or \\[string-edit-abort] to abort"))
    (message "%s" (substitute-command-keys
                   "Type \\<string-edit-minor-mode-map>\\[string-edit-done] when you've finished editing"))))

;;;###autoload
(defun read-string-from-buffer (prompt string)
  "Switch to a new buffer to edit STRING in a recursive edit.
The user finishes editing with \\<string-edit-mode-map>\\[string-edit-done], or aborts with \\<string-edit-mode-map>\\[string-edit-abort].

Insert PROMPT at the start of the buffer.  If nil, no prompt is
inserted.

When the user exits recursive edit, return the contents of the
buffer (without including PROMPT).

Also see `string-edit'."
  (string-edit
   prompt
   string
   (lambda (edited)
     (setq string edited)
     (exit-recursive-edit))
   :abort-callback (lambda () (throw 'exit "Aborted edit")))
  (recursive-edit)
  string)

(defvar-keymap string-edit-minor-mode-map
  "C-c C-c" #'string-edit-done
  "C-c C-k" #'string-edit-abort)

(define-minor-mode string-edit-minor-mode
  "Minor mode for editing strings"
  :lighter "String"
  :interactive nil)

(define-derived-mode string-edit-mode text-mode "Text"
  "Mode for editing strings."
  :interactive nil)

(defun string-edit-done ()
  "Finish editing the string and call the callback function.
This will kill the current buffer."
  (interactive)
  (let* ((string
          (save-excursion
            (goto-char (point-min))
            ;; Skip past the help text.
            (text-property-search-forward 'string-edit--prompt)
            (buffer-substring (point) (point-max))))
         (valid (funcall (or string-edit--read #'identity) string))
         (callback string-edit--success-callback))
    (quit-window 'kill)
    (funcall callback valid)))

(defun string-edit-abort ()
  "Abort editing the current string."
  (interactive)
  (let ((callback string-edit--abort-callback))
    (quit-window 'kill)
    (when callback
      (funcall callback))))

(provide 'string-edit)

;;; string-edit.el ends here
