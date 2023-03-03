;;; helper.el --- utility help package supporting help in electric modes  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-2023 Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: emacs-devel@gnu.org
;; Keywords: help
;; Package: emacs

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

;; hey, here's a helping hand.

;; Bind this to a string for <blank> in "... Other keys <blank>".
;; Helper-help uses this to construct help string when scrolling.
;; Defaults to "return"
(defvar Helper-return-blurb nil)

;; Keymap implementation doesn't work too well for non-standard loops.
;; But define it anyway for those who can use it.  Non-standard loops
;; will probably have to use Helper-help.  You can't autoload the
;; keymap either.


(defvar-keymap Helper-help-map
  "m" #'Helper-describe-mode
  "b" #'Helper-describe-bindings
  "c" #'Helper-describe-key-briefly
  "k" #'Helper-describe-key
  ;;"f" #'Helper-describe-function
  ;;"v" #'Helper-describe-variable
  "?" #'Helper-help-options
  (key-description (char-to-string help-char)) #'Helper-help-options)
(fset 'Helper-help-map Helper-help-map)

(defun Helper-help-scroller ()
  (let ((blurb (or (and (boundp 'Helper-return-blurb)
			Helper-return-blurb)
		   "return")))
    (save-window-excursion
      (goto-char (window-start))
      (if (get-buffer-window "*Help*")
	  (pop-to-buffer "*Help*")
	(switch-to-buffer "*Help*"))
      (goto-char (point-min))
      (let ((continue t) state)
	(while continue
	  (setq state (+ (* 2 (if (pos-visible-in-window-p (point-max)) 1 0))
			 (if (pos-visible-in-window-p (point-min)) 1 0)))
	  (message
            (nth state
                 (mapcar
                  #'substitute-command-keys
                  '("\\`SPC' forward, \\`DEL' back.  Other keys %s"
                    "\\`SPC' scrolls forward.  Other keys %s"
                    "\\`DEL' scrolls back.  Other keys %s"
                    "Type anything to %s")))
	    blurb)
	  (setq continue (read-event))
	  (cond ((and (memq continue '(?\s ?\C-v)) (< state 2))
		 (scroll-up))
                ((eq continue ?\C-l)
		 (recenter))
                ((and (or (eq continue 'backspace)
                          (eq continue ?\177))
                      (zerop (% state 2)))
		 (scroll-down))
		(t (setq continue nil))))))))

(defun Helper-help-options ()
  "Describe help options."
  (interactive)
  (message (substitute-command-keys
            "\\`c' (key briefly), \\`m' (mode), \\`k' (key), \\`b' (bindings)"))
  (sit-for 4))

(defun Helper-describe-key-briefly (key)
  "Briefly describe binding of KEY."
  (interactive "kDescribe key briefly: ")
  (describe-key-briefly key)
  (sit-for 4))

(defun Helper-describe-key (key)
  "Describe binding of KEY."
  (interactive "kDescribe key: ")
  (save-window-excursion (describe-key key))
  (Helper-help-scroller))

(defun Helper-describe-function ()
  "Describe a function.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-function))
  (Helper-help-scroller))

(defun Helper-describe-variable ()
  "Describe a variable.  Name read interactively."
  (interactive)
  (save-window-excursion (call-interactively 'describe-variable))
  (Helper-help-scroller))

(defun Helper-describe-mode ()
  "Describe the current mode."
  (interactive)
  (let ((name (format-mode-line mode-name))
	(documentation (documentation major-mode)))
    (with-current-buffer (get-buffer-create "*Help*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert name " Mode\n" documentation)
      (help-mode)))
  (Helper-help-scroller))

;;;###autoload
(defun Helper-describe-bindings ()
  "Describe local key bindings of current mode."
  (interactive)
  (save-window-excursion (describe-bindings))
  (Helper-help-scroller))

;;;###autoload
(defun Helper-help ()
  "Provide help for current mode."
  (interactive)
  (let ((continue t) c)
    (while continue
      (message (substitute-command-keys
                "Help (Type \\`?' for further options)"))
      (setq c (read-key-sequence nil))
      (setq c (lookup-key Helper-help-map c))
      (cond ((eq c 'Helper-help-options)
	     (Helper-help-options))
	    ((commandp c)
	     (call-interactively c)
	     (setq continue nil))
	    (t
	     (ding)
	     (setq continue nil))))))

(provide 'helper)

;;; helper.el ends here
