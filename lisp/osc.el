;;; osc.el --- Support for OSC escape sequences      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;;         Matthias Meulien <orontee@gmail.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: processes, terminals, services

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

;; Interpretation of OSC (Operating System Commands) escape
;; sequences. Handlers for OSC 7 and 8 (for current directory and
;; hyperlinks respectively) are provided.

;;; Code:

(defvar-local osc-handlers '(("7" . osc-directory-tracker)
                             ("8" . osc-hyperlink-handler))
  "Alist of handlers for OSC escape sequences.
See `osc-apply-on-region' for details.")

(defvar-local osc--marker nil)
;; The function `osc-apply-on-region' can set `osc--marker' to the start
;; position of an escape sequence without termination.

(defun osc-apply-on-region (begin end)
  "Interpret OSC escape sequences in region.
This function search for escape sequences of the forms

    ESC ] command ; text BEL
    ESC ] command ; text ESC \\

Every occurrence of such escape sequences is removed from the
buffer.  Then, if `command' is a key of the local variable
`osc-handlers' alist, the corresponding value, which should be a
function, is called with `command' and `text' as arguments, with
point where the escape sequence was located."
  (save-excursion
    (goto-char (or osc--marker begin))
    (when (eq (char-before) ?\e) (backward-char))
    (while (re-search-forward "\e]" end t)
      (let ((pos0 (match-beginning 0))
            (code (and (re-search-forward "\\=\\([0-9A-Za-z]*\\);" end t)
                       (match-string 1)))
            (pos1 (point)))
        (if (re-search-forward "\a\\|\e\\\\" end t)
            (let ((text (buffer-substring-no-properties
                         pos1 (match-beginning 0))))
              (setq osc--marker nil)
              (delete-region pos0 (point))
              (when-let ((fun (cdr (assoc-string code osc-handlers))))
                (funcall fun code text)))
          (put-text-property pos0 end 'invisible t)
          (setq osc--marker (copy-marker pos0)))))))

;; Current directory tracking (OSC 7)

(declare-function url-host "url/url-parse.el")
(declare-function url-type "url/url-parse.el")
(declare-function url-filename "url/url-parse.el")
(defun osc-directory-tracker (_ text)
  "Update `default-directory' from OSC 7 escape sequences.

This function is intended to be included as an entry of
`osc-handlers'.  You should moreover arrange for your shell to
print the appropriate escape sequence at each prompt, say with
the following command:

    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

This functionality serves as an alternative to `dirtrack-mode'
and `shell-dirtrack-mode'."
  (let ((url (url-generic-parse-url text)))
    (when (and (string= (url-type url) "file")
               (or (null (url-host url))
                   (string= (url-host url) (system-name))))
      (ignore-errors
        (cd-absolute (url-unhex-string (url-filename url)))))))

;; Hyperlink handling (OSC 8)

(defvar osc-hyperlink-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\r" 'browse-url-button-open)
    (define-key map [mouse-2] 'browse-url-button-open)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap used by OSC 8 hyperlink buttons.")

(define-button-type 'osc-hyperlink
  'keymap osc-hyperlink-map
  'help-echo (lambda (_ buffer pos)
               (when-let ((url (get-text-property pos 'browse-url-data buffer)))
                 (format "mouse-2, C-c RET: Open %s" url))))

(defvar-local osc-hyperlink--state nil)

(defun osc-hyperlink-handler (_ text)
  "Create a hyperlink from an OSC 8 escape sequence.
This function is intended to be included as an entry of
`osc-handlers'."
  (when osc-hyperlink--state
    (let ((start (car osc-hyperlink--state))
          (url (cdr osc-hyperlink--state)))
      (make-text-button start (point)
                        'type 'osc-hyperlink
                        'browse-url-data url)))
  (setq osc-hyperlink--state
        (and (string-match ";\\(.+\\)" text)
             (cons (point-marker) (match-string-no-properties 1 text)))))

(provide 'osc)
;;; osc.el ends here
