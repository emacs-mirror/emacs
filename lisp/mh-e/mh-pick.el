;;; mh-pick.el --- make a search pattern and search for a message in MH-E

;; Copyright (C) 1993, 1995, 2001, 2003 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Internal support for MH-E package.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'easymenu)
(require 'gnus-util)

;;; Internal variables:

(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")

(defvar mh-searching-folder nil)        ;Folder this pick is searching.
(defvar mh-searching-function nil)

;;;###mh-autoload
(defun mh-search-folder (folder window-config)
  "Search FOLDER for messages matching a pattern.
This function uses the MH command `pick' to do the work.
Add the messages found to the sequence named `search'.
Argument WINDOW-CONFIG is the current window configuration and is used when
the search folder is dismissed."
  (interactive (list (mh-prompt-for-folder "Search" mh-current-folder nil nil t)
                     (current-window-configuration)))
  (let ((pick-folder (if (equal folder "+") mh-current-folder folder)))
    (switch-to-buffer-other-window "search-pattern")
    (if (or (zerop (buffer-size))
            (not (y-or-n-p "Reuse pattern? ")))
        (mh-make-pick-template)
      (message ""))
    (setq mh-searching-function 'mh-pick-do-search
          mh-searching-folder pick-folder)
    (mh-make-local-vars 'mh-current-folder folder
                        'mh-previous-window-config window-config)
    (message "%s" (substitute-command-keys
                   (concat "Type \\[mh-do-search] to search messages, "
                           "\\[mh-help] for help.")))))

(defun mh-make-pick-template ()
  "Initialize the current buffer with a template for a pick pattern."
  (let ((inhibit-read-only t)) (erase-buffer))
  (insert "From: \n"
          "To: \n"
          "Cc: \n"
          "Date: \n"
          "Subject: \n"
          "---------\n")
  (mh-pick-mode)
  (goto-char (point-min))
  (dotimes (i 5)
    (add-text-properties (point) (1+ (point)) '(front-sticky t))
    (add-text-properties (- (line-end-position) 2) (1- (line-end-position))
                         '(rear-nonsticky t))
    (add-text-properties (point) (1- (line-end-position)) '(read-only t))
    (forward-line))
  (add-text-properties (point) (1+ (point)) '(front-sticky t))
  (add-text-properties (point) (1- (line-end-position)) '(read-only t))
  (goto-char (point-max)))

;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
(easy-menu-define
  mh-pick-menu mh-pick-mode-map "Menu for MH-E pick-mode"
  '("Pick"
    ["Execute the Search"       mh-pick-do-search t]))


;;; Help Messages
;;; Group messages logically, more or less.
(defvar mh-pick-mode-help-messages
  '((nil
     "Search messages using pick:  \\[mh-pick-do-search]\n"
     "Search messages using index:  \\[mh-index-do-search]\n"
     "Move to a field by typing C-c C-f C-<field>\n"
     "where <field> is the first letter of the desired field."))
  "Key binding cheat sheet.

This is an associative array which is used to show the most common commands.
The key is a prefix char. The value is one or more strings which are
concatenated together and displayed in the minibuffer if ? is pressed after
the prefix character. The special key nil is used to display the
non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.")

(put 'mh-pick-mode 'mode-class 'special)

(define-derived-mode mh-pick-mode fundamental-mode "MH-Pick"
  "Mode for creating search templates in MH-E.\\<mh-pick-mode-map>

After each field name, enter the pattern to search for.  If a field's
value does not matter for the search, leave it empty.  To search the
entire message, supply the pattern in the \"body\" of the template.
Each non-empty field must be matched for a message to be selected.
To effect a logical \"or\", use \\[mh-search-folder] multiple times.
When you have finished, type  \\[mh-pick-do-search]  to do the search.

The value of `mh-pick-mode-hook' is a list of functions to be called,
with no arguments, upon entry to this mode.

\\{mh-pick-mode-map}"

  (make-local-variable 'mh-searching-folder)
  (make-local-variable 'mh-searching-function)
  (make-local-variable 'mh-help-messages)
  (easy-menu-add mh-pick-menu)
  (setq mh-help-messages mh-pick-mode-help-messages)
  (run-hooks 'mh-pick-mode-hook))

;;;###mh-autoload
(defun mh-do-pick-search ()
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in `mh-searching-folder'.
Add the messages found to the sequence named `search'.

This is a deprecated function and `mh-pick-do-search' should be used instead."
  (interactive)
  (mh-pick-do-search))

;;;###mh-autoload
(defun mh-pick-do-search ()
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in `mh-searching-folder'.
Add the messages found to the sequence named `search'."
  (interactive)
  (let ((pattern-list (mh-pick-parse-search-buffer))
        (folder mh-searching-folder)
        (new-buffer-flag nil)
        (window-config mh-previous-window-config)
        range pick-args msgs)
    (unless pattern-list
      (error "No search pattern specified"))
    (save-excursion
      (cond ((get-buffer folder)
             (set-buffer folder)
             (setq range (if (and mh-first-msg-num mh-last-msg-num)
                             (format "%d-%d" mh-first-msg-num mh-last-msg-num)
                           "all")))
            (t
             (mh-make-folder folder)
             (setq range "all")
             (setq new-buffer-flag t))))
    (setq pick-args (mh-pick-regexp-builder pattern-list))
    (when pick-args
      (setq msgs (mh-seq-from-command folder 'search
                                      `("pick" ,folder ,range ,@pick-args))))
    (message "Searching...done")
    (if (not new-buffer-flag)
        (switch-to-buffer folder)
      (mh-scan-folder folder msgs)
      (setq mh-previous-window-config window-config))
    (mh-add-msgs-to-seq msgs 'search)
    (delete-other-windows)))

;;;###mh-autoload
(defun mh-do-search ()
  "Use the default searching function.
If \\[mh-search-folder] was used to create the search pattern then pick is used
to search the folder. Otherwise if \\[mh-index-search] was used then the
indexing program specified in `mh-index-program' is used."
  (interactive)
  (if (symbolp mh-searching-function)
      (funcall mh-searching-function)
    (error "No searching function defined")))

(defun mh-seq-from-command (folder seq command)
  "In FOLDER, make a sequence named SEQ by executing COMMAND.
COMMAND is a list.  The first element is a program name
and the subsequent elements are its arguments, all strings."
  (let ((msg)
        (msgs ())
        (case-fold-search t))
    (save-excursion
      (save-window-excursion
        (if (eq 0 (apply 'mh-exec-cmd-quiet nil command))
            ;; "pick" outputs one number per line
            (while (setq msg (car (mh-read-msg-list)))
              (setq msgs (cons msg msgs))
              (forward-line 1))))
      (set-buffer folder)
      (setq msgs (nreverse msgs))       ;put in ascending order
      msgs)))

(defun mh-pick-parse-search-buffer ()
  "Parse the search buffer contents.
The function returns a alist. The car of each element is either the header name
to search in or nil to search the whole message. The cdr of the element is the
pattern to search."
  (save-excursion
    (let ((pattern-list ())
          (in-body-flag nil)
          start begin)
      (goto-char (point-min))
      (while (not (eobp))
        (if (search-forward "--------" (line-end-position) t)
            (setq in-body-flag t)
          (beginning-of-line)
          (setq begin (point))
          (setq start (if in-body-flag
                          (point)
                        (search-forward ":" (line-end-position) t)
                        (point)))
          (push (cons (and (not in-body-flag)
                           (intern (downcase
                                    (buffer-substring-no-properties
                                     begin (1- start)))))
                      (mh-index-parse-search-regexp
                       (buffer-substring-no-properties
                        start (line-end-position))))
                pattern-list))
        (forward-line))
      pattern-list)))



;; Functions specific to how pick works...
(defun mh-pick-construct-regexp (expr component)
  "Construct pick compatible expression corresponding to EXPR.
COMPONENT is the component to search."
  (cond ((atom expr) (list component expr))
        ((eq (car expr) 'and)
         `("-lbrace" ,@(mh-pick-construct-regexp (cadr expr) component) "-and"
           ,@(mh-pick-construct-regexp (caddr expr) component) "-rbrace"))
        ((eq (car expr) 'or)
         `("-lbrace" ,@(mh-pick-construct-regexp (cadr expr) component) "-or"
           ,@(mh-pick-construct-regexp (caddr expr) component) "-rbrace"))
        ((eq (car expr) 'not)
         `("-lbrace" "-not" ,@(mh-pick-construct-regexp (cadr expr) component)
           "-rbrace"))
        (t (error "Unknown operator '%s' seen" (car expr)))))

(defun mh-pick-regexp-builder (pattern-list)
  "Generate pick search expression from PATTERN-LIST."
  (let ((result ()))
    (dolist (pattern pattern-list)
      (when (cdr pattern)
        (setq result `(,@result "-and" "-lbrace"
                       ,@(mh-pick-construct-regexp
                          (cdr pattern) (if (car pattern)
                                            (format "-%s" (car pattern))
                                          "-search"))
                       "-rbrace"))))
    (cdr result)))



;;; Build the pick-mode keymap:
;;; If this changes, modify mh-pick-mode-help-messages accordingly, above.
(gnus-define-keys  mh-pick-mode-map
  "\C-c?"               mh-help
  "\C-c\C-i"            mh-index-do-search
  "\C-c\C-p"            mh-pick-do-search
  "\C-c\C-c"            mh-do-search
  "\C-c\C-f\C-b"        mh-to-field
  "\C-c\C-f\C-c"        mh-to-field
  "\C-c\C-f\C-d"        mh-to-field
  "\C-c\C-f\C-f"        mh-to-field
  "\C-c\C-f\C-r"        mh-to-field
  "\C-c\C-f\C-s"        mh-to-field
  "\C-c\C-f\C-t"        mh-to-field
  "\C-c\C-fb"           mh-to-field
  "\C-c\C-fc"           mh-to-field
  "\C-c\C-fd"           mh-to-field
  "\C-c\C-ff"           mh-to-field
  "\C-c\C-fr"           mh-to-field
  "\C-c\C-fs"           mh-to-field
  "\C-c\C-ft"           mh-to-field)

(provide 'mh-pick)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: aef2b271-7768-42bd-a782-9a14ba9f83f7
;;; mh-pick.el ends here
