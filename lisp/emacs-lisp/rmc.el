;;; rmc.el --- read from a multiple choice question -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Free Software Foundation, Inc.

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

(require 'seq)

;;;###autoload
(defun read-multiple-choice (prompt choices &optional help-string)
  "Ask user to select an entry from CHOICES, promting with PROMPT.
This function allows to ask the user a multiple-choice question.

CHOICES should be a list of the form (KEY NAME [DESCRIPTION]).
KEY is a character the user should type to select the entry.
NAME is a short name for the entry to be displayed while prompting
\(if there's no room, it might be shortened).
DESCRIPTION is an optional longer description of the entry; it will
be displayed in a help buffer if the user requests more help.  This
help description has a fixed format in columns.  For greater
flexibility, instead of passing a DESCRIPTION, the caller can pass
the optional argument HELP-STRING.  This argument is a string that
should contain a more detailed description of all of the possible
choices.  `read-multiple-choice' will display that description in a
help buffer if the user requests that.

This function translates user input into responses by consulting
the bindings in `query-replace-map'; see the documentation of
that variable for more information.  The relevant bindings for the
purposes of this function are `recenter', `scroll-up', `scroll-down',
and `edit'.
If the user types the `recenter', `scroll-up', or `scroll-down'
responses, the function performs the requested window recentering or
scrolling, and then asks the question again.  If the user enters `edit',
the function starts a recursive edit.  When the user exit the recursive
edit, the multiple-choice prompt gains focus again.

When `use-dialog-box' is t (the default), and the command using this
function was invoked via the mouse, this function pops up a GUI dialog
to collect the user input, but only if Emacs is capable of using GUI
dialogs.  Otherwise, the function will always use text-mode dialogs.

The return value is the matching entry from the CHOICES list.

Usage example:

\(read-multiple-choice \"Continue connecting?\"
                      \\='((?a \"always\")
                        (?s \"session only\")
                        (?n \"no\")))"
  (let* ((altered-names nil)
         (full-prompt
          (format
           "%s (%s): "
           prompt
           (mapconcat
            (lambda (elem)
              (let* ((name (cadr elem))
                     (pos (seq-position name (car elem)))
                     (altered-name
                      (cond
                       ;; Not in the name string.
                       ((not pos)
                        (format "[%c] %s" (car elem) name))
                       ;; The prompt character is in the name, so highlight
                       ;; it on graphical terminals...
                       ((display-supports-face-attributes-p
                         '(:underline t) (window-frame))
                        (setq name (copy-sequence name))
                        (put-text-property pos (1+ pos)
                                           'face 'read-multiple-choice-face
                                           name)
                        name)
                       ;; And put it in [bracket] on non-graphical terminals.
                       (t
                        (concat
                         (substring name 0 pos)
                         "["
                         (upcase (substring name pos (1+ pos)))
                         "]"
                         (substring name (1+ pos)))))))
                (push (cons (car elem) altered-name)
                      altered-names)
                altered-name))
            (append choices '((?? "?")))
            ", ")))
         tchar buf wrong-char answer)
    (save-window-excursion
      (save-excursion
	(while (not tchar)
	  (message "%s%s"
                   (if wrong-char
                       "Invalid choice.  "
                     "")
                   full-prompt)
          (setq tchar
                (if (and (display-popup-menus-p)
                         last-input-event ; not during startup
                         (consp last-nonmenu-event)
                         use-dialog-box)
                    (x-popup-dialog
                     t
                     (cons prompt
                           (mapcar
                            (lambda (elem)
                              (cons (capitalize (cadr elem))
                                    (car elem)))
                            choices)))
                  (condition-case nil
                      (let ((cursor-in-echo-area t))
                        (read-event))
                    (error nil))))
          (setq answer (lookup-key query-replace-map (vector tchar) t))
          (setq tchar
                (cond
                 ((eq answer 'recenter)
                  (recenter) t)
                 ((eq answer 'scroll-up)
                  (ignore-errors (scroll-up-command)) t)
                 ((eq answer 'scroll-down)
                  (ignore-errors (scroll-down-command)) t)
                 ((eq answer 'scroll-other-window)
                  (ignore-errors (scroll-other-window)) t)
                 ((eq answer 'scroll-other-window-down)
                  (ignore-errors (scroll-other-window-down)) t)
                 ((eq answer 'edit)
                  (save-match-data
                    (save-excursion
                      (message "%s"
                               (substitute-command-keys
                                "Recursive edit; type \\[exit-recursive-edit] to return to help screen"))
                      (recursive-edit))))
                 (t tchar)))
          (when (eq tchar t)
            (setq wrong-char nil
                  tchar nil))
          ;; The user has entered an invalid choice, so display the
          ;; help messages.
          (when (and (not (eq tchar nil))
                     (not (assq tchar choices)))
	    (setq wrong-char (not (memq tchar `(?? ,help-char)))
                  tchar nil)
            (when wrong-char
              (ding))
            (setq buf (get-buffer-create "*Multiple Choice Help*"))
            (if (stringp help-string)
                (with-help-window buf
                  (with-current-buffer buf
                    (insert help-string)))
              (with-help-window buf
                (with-current-buffer buf
                  (erase-buffer)
                  (pop-to-buffer buf)
                  (insert prompt "\n\n")
                  (let* ((columns (/ (window-width) 25))
                         (fill-column 21)
                         (times 0)
                         (start (point)))
                    (dolist (elem choices)
                      (goto-char start)
                      (unless (zerop times)
                        (if (zerop (mod times columns))
                            ;; Go to the next "line".
                            (goto-char (setq start (point-max)))
                          ;; Add padding.
                          (while (not (eobp))
                            (end-of-line)
                            (insert (make-string (max (- (* (mod times columns)
                                                            (+ fill-column 4))
                                                         (current-column))
                                                      0)
                                                 ?\s))
                            (forward-line 1))))
                      (setq times (1+ times))
                      (let ((text
                             (with-temp-buffer
                               (insert (format
                                        "%c: %s\n"
                                        (car elem)
                                        (cdr (assq (car elem) altered-names))))
                               (fill-region (point-min) (point-max))
                               (when (nth 2 elem)
                                 (let ((start (point)))
                                   (insert (nth 2 elem))
                                   (unless (bolp)
                                     (insert "\n"))
                                   (fill-region start (point-max))))
                               (buffer-string))))
                        (goto-char start)
                        (dolist (line (split-string text "\n"))
                          (end-of-line)
                          (if (bolp)
                              (insert line "\n")
                            (insert line))
                          (forward-line 1))))))))))))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (assq tchar choices)))

(provide 'rmc)

;;; rmc.el ends here
