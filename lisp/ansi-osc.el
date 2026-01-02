;;; ansi-osc.el --- Support for OSC escape sequences      -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;;         Matthias Meulien <orontee@gmail.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: processes, terminals, services

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

;; Interpretation of OSC (Operating System Commands) escape sequences.
;; Handlers for OSC 2, 7 and 8 (for window title, current directory
;; and hyperlinks respectively) are provided.

;; The function `ansi-osc-compilation-filter' can be added to
;; `compilation-filter-hook' to collect OSC sequences in compilation
;; buffers.  The variable `ansi-osc-for-compilation-buffer' tells what
;; to do with collected sequences.

;;; Code:

;; According to ECMA 48, section 8.3.89 "OSC - OPERATING SYSTEM COMMAND"
;; OSC control sequences match:
;; "\e\\][\x08-\x0D]*[\x20-\x7E]*\\(\a\\|\e\\\\\\)"

(defvar-local ansi-osc--marker nil
  "Marker pointing to the start of an escape sequence.
Used by `ansi-osc-filter-region' and `ansi-osc-apply-on-region' to store
position of an unfinished escape sequence, for the complete sequence to
be handled in next call.")

(defun ansi-osc-filter-region (begin end)
  "Filter out all OSC control sequences from region between BEGIN and END.
When an unfinished escape sequence is found, the start position is saved
to `ansi-osc--marker'.  Later call will override BEGIN with the position
pointed by `ansi-osc--marker'."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char (or ansi-osc--marker begin))
      (when (eq (char-before) ?\e) (backward-char))
      (while (re-search-forward "\e]" end-marker t)
        (let ((pos0 (match-beginning 0)))
          (if (re-search-forward
               "\\=[\x08-\x0D]*[\x20-\x7E]*\\(\a\\|\e\\\\\\)"
               end-marker t)
              (delete-region pos0 (point))
            (setq ansi-osc--marker (copy-marker pos0))))))))

(defvar-local ansi-osc-handlers '(("0" . ansi-osc-window-title-handler)
                                  ("2" . ansi-osc-window-title-handler)
                                  ("7" . ansi-osc-directory-tracker)
                                  ("8" . ansi-osc-hyperlink-handler))
  "Alist of handlers for OSC escape sequences.
See `ansi-osc-apply-on-region' for details.")

(defun ansi-osc-apply-on-region (begin end)
  "Interpret OSC escape sequences in region between BEGIN and END.
This function searches for escape sequences of the forms

    ESC ] command ; text BEL
    ESC ] command ; text ESC \\

Every occurrence of such escape sequences is removed from the buffer.
Then, if `command' is a key in the alist that is the value of the local
variable `ansi-osc-handlers', that key's value, which should be a
function, is called with `command' and `text' as arguments, with point
where the escape sequence was located.  When an unfinished escape
sequence is identified, it's hidden and the start position is saved to
`ansi-osc--marker'.  Later call will override BEGIN with the position
pointed by `ansi-osc--marker'."
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char (or ansi-osc--marker begin))
      (when (eq (char-before) ?\e) (backward-char))
      (while (re-search-forward "\e]" end-marker t)
        (let ((pos0 (match-beginning 0))
              (code (and
                     (re-search-forward "\\=\\([0-9A-Za-z]*\\);" end-marker t)
                     (match-string 1)))
              (pos1 (point)))
          (if (re-search-forward "\a\\|\e\\\\" end-marker t)
              (let ((text (buffer-substring-no-properties
                           pos1 (match-beginning 0))))
                (setq ansi-osc--marker nil)
                (delete-region pos0 (point))
                (when-let* ((fun (cdr (assoc-string code ansi-osc-handlers))))
                  (funcall fun code text)))
            (put-text-property pos0 end-marker 'invisible t)
            (setq ansi-osc--marker (copy-marker pos0))))))))

;; Window title handling (OSC 2)

(defvar-local ansi-osc-window-title nil)
(defun ansi-osc-window-title-handler (_ text)
  "Set value of `ansi-osc-window-title' from an OSC 2 escape sequence.
The variable `ansi-osc-window-title' can then be referenced in
`frame-title-format' to dynamically set the frame title.

This function is intended to be included as an element of the
list that is the value of `ansi-osc-handlers'."
  (setq ansi-osc-window-title text))

;; Current directory tracking (OSC 7)

(declare-function url-host "url/url-parse.el")
(declare-function url-type "url/url-parse.el")
(declare-function url-filename "url/url-parse.el")
(defun ansi-osc-directory-tracker (_ text)
  "Update `default-directory' from OSC 7 escape sequences.

This function is intended to be included as an element of the
list that is the value of `ansi-osc-handlers'.  You should arrange
for your shell to print the appropriate escape sequence at each prompt,
such as with the following command:

    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

A remote `default-directory' is maintained.

This functionality serves as an alternative to `dirtrack-mode'
and `shell-dirtrack-mode'."
  (when-let* ((url (url-generic-parse-url text))
              ((string= (url-type url) "file")))
    (ignore-errors
      (cd-absolute
       (concat (file-remote-p default-directory)
               (url-unhex-string (url-filename url)))))))

;; Hyperlink handling (OSC 8)

(defvar-keymap ansi-osc-hyperlink-map
  :doc "Keymap used by OSC 8 hyperlink buttons."
  "RET"           (keymap-read-only-bind #'browse-url-button-open)
  "C-c RET"       #'browse-url-button-open
  "<mouse-2>"     #'browse-url-button-open
  "<follow-link>" 'mouse-face)

(define-button-type 'ansi-osc-hyperlink
  'keymap ansi-osc-hyperlink-map
  'help-echo (lambda (_ buffer pos)
               (when-let* ((url (get-text-property pos 'browse-url-data buffer)))
                 (format "mouse-2, C-c RET: Open %s" url))))

(defvar-local ansi-osc-hyperlink--state nil)

(defun ansi-osc-hyperlink-handler (_ text)
  "Create a hyperlink from an OSC 8 escape sequence.
This function is intended to be included as an element of the list
that is the value of `ansi-osc-handlers'."
  (when ansi-osc-hyperlink--state
    (let ((start (car ansi-osc-hyperlink--state))
          (url (cdr ansi-osc-hyperlink--state)))
      (make-text-button start (point)
                        'type 'ansi-osc-hyperlink
                        'browse-url-data url)))
  (setq ansi-osc-hyperlink--state
        (and (string-match ";\\(.+\\)" text)
             (cons (point-marker) (match-string-no-properties 1 text)))))

(defgroup ansi-osc nil
  "Interpretation of OSC escape sequences.
Handlers for OSC 2, 7 and 8 (for window title, current directory
and hyperlinks respectively) are provided.  OSC (Operating System
Commands) control sequences are defined in section 8.3.89 of the
ECMA-48 standard is freely available at
<URL:https://www.ecma-international.org/publications/standards/Ecma-048.htm>
as a PDF file."
  :version "29.1"
  :group 'processes)

(defcustom ansi-osc-for-compilation-buffer 'filter
  "What to do with OSC escape sequences in compilation output.

If nil, do nothing.

If the symbol `filter', then filter out all OSC control sequences.

If any other non-nil value, then collect OSC control sequences
and call the appropriate handlers as described in `ansi-osc-handlers'.

In order for this to have any effect, `ansi-osc-compilation-filter'
must be in `compilation-filter-hook'."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Filter out OSC" filter)
                 (other :tag "Translate OSC" t))
  :group 'ansi-osc
  :version "29.1")

(defvar compilation-filter-start)

;;;###autoload
(defun ansi-osc-compilation-filter ()
  "Maybe collect OSC control sequences.
This function depends on the variable `ansi-osc-for-compilation-buffer',
and is meant to be used in `compilation-filter-hook'."
  (let ((inhibit-read-only t))
    (pcase ansi-osc-for-compilation-buffer
      ('nil nil)
      ('filter
       (ansi-osc-filter-region compilation-filter-start (point)))
      (_
       (ansi-osc-apply-on-region compilation-filter-start (point))))))

(provide 'ansi-osc)
;;; ansi-osc.el ends here
