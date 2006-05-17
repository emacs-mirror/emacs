;;; mh-pick.el --- make a search pattern and search for a message in MH-E

;; Copyright (C) 1993, 1995,
;;  2001, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Internal support for MH-E package.

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-e)
(require 'easymenu)
(require 'gnus-util)

;;; Internal variables:

(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")

(defvar mh-searching-folder nil)        ;Folder this pick is searching.
(defvar mh-searching-function nil)

(defconst mh-pick-single-dash  '(cc date from subject to)
  "Search components that are supported by single-dash option in pick.")

;;;###mh-autoload
(defun mh-search-folder (folder window-config)
  "Search FOLDER for messages matching a pattern.

With this command, you can search a folder for messages to or from a
particular person or about a particular subject. In fact, you can also search
for messages containing selected strings in any arbitrary header field or any
string found within the messages.

You are first prompted for the name of the folder to search and then placed in
the following buffer in MH-Pick mode:

     From:
     To:
     Cc:
     Date:
     Subject:
     --------

Edit this template by entering your search criteria in an appropriate header
field that is already there, or create a new field yourself. If the string
you're looking for could be anywhere in a message, then place the string
underneath the row of dashes. The \\[mh-search-folder] command uses the MH
command \"pick\" to do the real work.

There are no semantics associated with the search criteria--they are simply
treated as strings. Case is ignored when all lowercase is used, and regular
expressions (a la \"ed\") are available. It is all right to specify several
search criteria. What happens then is that a logical _and_ of the various
fields is performed. If you prefer a logical _or_ operation, run
\\[mh-search-folder] multiple times.

As an example, let's say that we want to find messages from Ginnean about
horseback riding in the Kosciusko National Park (Australia) during January,
1994. Normally we would start with a broad search and narrow it down if
necessary to produce a manageable amount of data, but we'll cut to the chase
and create a fairly restrictive set of criteria as follows:

     From: ginnean
     To:
     Cc:
     Date: Jan 1994
     Subject: horse.*kosciusko
     --------

As with MH-Letter mode, MH-Pick provides commands like
\\<mh-pick-mode-map>\\[mh-to-field] to help you fill in the blanks.

To perform the search, type \\[mh-do-search]. The selected messages are placed
in the \"search\" sequence, which you can use later in forwarding, printing,
or narrowing your field of view. Subsequent searches are appended to the
\"search\" sequence. If, however, you wish to start with a clean slate, first
delete the \"search\" sequence.

If you're searching in a folder that is already displayed in an MH-Folder
buffer, only those messages contained in the buffer are used for the search.
Therefore, if you want to search in all messages, first kill the folder's
buffer with \\<mh-folder-mode-map>\\[kill-buffer] or scan the entire folder
with \\[mh-rescan-folder].

If you find that you do the same thing over and over when editing the search
template, you may wish to bind some shortcuts to keys. This can be done with
the variable `mh-pick-mode-hook', which is called when \\[mh-search-folder] is
run on a new pattern.

If you have run the \\[mh-index-search] command, but change your mind while
entering the search criteria and actually want to run a regular search, then
you can use the \\<mh-pick-mode-map>\\[mh-pick-do-search] command.

In a program, argument WINDOW-CONFIG is the current window configuration and
is used when the search folder is dismissed."
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
  (setq mh-help-messages mh-pick-mode-help-messages))

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

;; All implementations of pick have special options -cc, -date, -from and
;; -subject that allow to search for corresponding components. Any other
;; component is searched using option --COMPNAME, for example: `pick
;; --x-mailer mh-e'. Mailutils `pick' supports this option using a certain
;; kludge, but it prefers the following syntax for this purpose:
;; `--component=COMPNAME --pattern=PATTERN'.
;;                                           -- Sergey Poznyakoff, Aug 2003
(defun mh-pick-regexp-builder (pattern-list)
  "Generate pick search expression from PATTERN-LIST."
  (let ((result ()))
    (dolist (pattern pattern-list)
      (when (cdr pattern)
        (setq result `(,@result "-and" "-lbrace"
                       ,@(mh-pick-construct-regexp
                          (if (and (mh-variant-p 'mu-mh) (car pattern))
                              (format "--pattern=%s" (cdr pattern))
                            (cdr pattern))
                          (if (car pattern)
                              (cond
                               ((mh-variant-p 'mu-mh)
                                (format "--component=%s" (car pattern)))
                               ((member (car pattern) mh-pick-single-dash)
                                (format "-%s" (car pattern)))
                               (t
                                (format "--%s" (car pattern))))
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
