;;; mh-pick.el --- make a search pattern and search for a message in MH-E

;; Copyright (C) 1993, 1995, 2001 Free Software Foundation, Inc.

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

;; $Id: mh-pick.el,v 1.10 2003/01/08 23:21:16 wohler Exp $

;;; Code:

(require 'mh-e)
(require 'easymenu)
(require 'gnus-util)

;;; Internal variables:

(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")

(defvar mh-searching-folder nil)        ;Folder this pick is searching.

;;;###mh-autoload
(defun mh-search-folder (folder)
  "Search FOLDER for messages matching a pattern.
This function uses the MH command `pick' to do the work.
Add the messages found to the sequence named `search'."
  (interactive (list (mh-prompt-for-folder "Search"
                                           mh-current-folder
                                           t)))
  (switch-to-buffer-other-window "pick-pattern")
  (if (or (zerop (buffer-size))
          (not (y-or-n-p "Reuse pattern? ")))
      (mh-make-pick-template)
    (message ""))
  (setq mh-searching-folder folder)
  (message "%s" (substitute-command-keys
                 (concat "Type \\[mh-do-pick-search] to search messages, "
                         "\\[mh-help] for help."))))

(defun mh-make-pick-template ()
  "Initialize the current buffer with a template for a pick pattern."
  (erase-buffer)
  (insert "From: \n"
          "To: \n"
          "Cc: \n"
          "Date: \n"
          "Subject: \n"
          "---------\n")
  (mh-pick-mode)
  (goto-char (point-min))
  (end-of-line))

;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
(easy-menu-define
  mh-pick-menu mh-pick-mode-map "Menu for MH-E pick-mode"
  '("Pick"
    ["Execute the Search"       mh-do-pick-search t]))


;;; Help Messages
;;; Group messages logically, more or less.
(defvar mh-pick-mode-help-messages
  '((nil
     "Search messages:  \\[mh-do-pick-search]\n"
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
When you have finished, type  \\[mh-do-pick-search]  to do the search.

The value of `mh-pick-mode-hook' is a list of functions to be called,
with no arguments, upon entry to this mode.

\\{mh-pick-mode-map}"

  (make-local-variable 'mh-searching-folder)
  (easy-menu-add mh-pick-menu)
  (make-local-variable 'mh-help-messages)
  (setq mh-help-messages mh-pick-mode-help-messages)
  (run-hooks 'mh-pick-mode-hook))

;;;###mh-autoload
(defun mh-do-pick-search ()
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in `mh-searching-folder'.
Add the messages found to the sequence named `search'."
  (interactive)
  (let ((pattern-buffer (buffer-name))
        (searching-buffer mh-searching-folder)
        range
        msgs
        (pattern nil)
        (new-buffer nil))
    (save-excursion
      (cond ((get-buffer searching-buffer)
             (set-buffer searching-buffer)
             (setq range (list (format "%d-%d"
                                       mh-first-msg-num mh-last-msg-num))))
            (t
             (mh-make-folder searching-buffer)
             (setq range '("all"))
             (setq new-buffer t))))
    (message "Searching...")
    (goto-char (point-min))
    (while (and range
                (setq pattern (mh-next-pick-field pattern-buffer)))
      (setq msgs (mh-seq-from-command searching-buffer
                                      'search
                                      (mh-list-to-string
                                       (list "pick" pattern searching-buffer
                                             "-list"
                                             (mh-coalesce-msg-list range)))))
      (setq range msgs))                ;restrict the pick range for next pass
    (message "Searching...done")
    (if new-buffer
        (mh-scan-folder searching-buffer msgs)
      (switch-to-buffer searching-buffer))
    (mh-add-msgs-to-seq msgs 'search)
    (delete-other-windows)))

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

(defun mh-next-pick-field (buffer)
  "Return the next piece of a pick argument extracted from BUFFER.
Return a list like (\"--fieldname\" \"pattern\") or (\"-search\" \"bodypat\")
or nil if no pieces remain."
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
           nil)
          ((re-search-forward "^\\([a-z][^: \t\n]*\\):[ \t]*\\([a-z0-9].*\\)$"
                              nil t)
           (let* ((component
                   (format "--%s"
                           (downcase (buffer-substring (match-beginning 1)
                                                       (match-end 1)))))
                  (pat (buffer-substring (match-beginning 2) (match-end 2))))
             (forward-line 1)
             (list component pat)))
          ((re-search-forward "^-*$" nil t)
           (forward-char 1)
           (let ((body (buffer-substring (point) (point-max))))
             (if (and (> (length body) 0) (not (equal body "\n")))
                 (list "-search" body)
               nil)))
          (t
           nil))))



;;; Build the pick-mode keymap:
;;; If this changes, modify mh-pick-mode-help-messages accordingly, above.
(gnus-define-keys  mh-pick-mode-map
  "\C-c?"               mh-help
  "\C-c\C-c"            mh-do-pick-search
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

;;; mh-pick.el ends here
