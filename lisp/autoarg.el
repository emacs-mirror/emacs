;;; autoarg.el --- make digit keys supply prefix args

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author:  Dave Love <fx@gnu.org>
;; Created: 1998-09-04
;; Keywords: abbrev, emulations

;; Autoarg Mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Autoarg Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides `autoarg-mode', a global minor mode meant to emulate
;; a facility reported from Twenex Emacs whereby digit keys supplied
;; prefix args rather than self inserting, with a digit sequence
;; terminated by space acting to insert the digits.

;; The bindings of DIGIT and C-DIGIT are swapped and a command bound
;; to SPC deals with a numeric prefix arg or acts normally without
;; such an arg.  (In the absence of a suitable terminal, you'd
;; probably want to swap DIGIT and M-DIGIT.)  See the mode doc.

;; You probably don't really want to use this.

;;; Code:

;;;###autoload
(defcustom autoarg-mode nil
  "Toggle Autoarg mode.

You must modify via \\[customize] for this variable to have an effect."
  :set (lambda (symbol vaautoarg-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'editing
  :require 'autoarg)
;; If you wanted a local mode:
;; (make-variable-buffer-local 'autoarg-mode)

(defvar autoarg-mode-map (make-sparse-keymap)
  "Keymap for Autoarg Mode.")

;; Loop over digit characters to set up keymap.
(let ((i ?0))
  (while (<= i ?9)
    (define-key autoarg-mode-map `[,i] 'digit-argument)
    (define-key autoarg-mode-map `[(control ,i)] 'self-insert-command)
    (setq i (1+ i))))
(define-key autoarg-mode-map " " 'autoarg-terminate)
;; Logical additions:
;; (define-key autoarg-mode-map [?-] 'negative-argument)
;; (define-key autoarg-mode-map [(control ?-)] 'self-insert-command)
;; A sensible/addition?
;; (define-key autoarg-mode-map [?\r] 'autoarg-terminate)

;;;###autoload
(defun autoarg-mode (&optional arg)
  "Toggle Autoarg mode minor mode globally.
With ARG, turn Autoarg mode on if ARG is positive, off otherwise.
\\<autoarg-mode-map>
In Autoarg mode digits are bound to `digit-argument' -- i.e. they
supply prefix arguments as C-DIGIT and M-DIGIT normally do -- and
C-DIGIT inserts DIGIT.  \\[autoarg-terminate] terminates the prefix sequence
and inserts the digits of the autoarg sequence into the buffer.
Without a numeric prefix arg the normal binding of \\[autoarg-terminate] is
invoked, i.e. what it would be with Autoarg mode off.

For example:
`6 9 \\[autoarg-terminate]' inserts `69' into the buffer, as does `C-6 C-9'.
`6 9 a' inserts 69 `a's into the buffer.
`6 9 \\[autoarg-terminate] \\[autoarg-terminate]' inserts `69' into the buffer and
then invokes the normal binding of \\[autoarg-terminate].
`C-u \\[autoarg-terminate]' invokes the normal binding of \\[autoarg-terminate] four times.

\\{autoarg-mode-map}"
  (interactive "P")
  (let ((old-mode autoarg-mode))
    (setq autoarg-mode (if (null arg)
			   (not autoarg-mode)
			 (> (prefix-numeric-value arg) 0))))
  (if (interactive-p)
      (message "Autoarg mode %sabled" (if autoarg-mode "en" "dis"))))

(add-to-list 'minor-mode-alist '(autoarg-mode " Aarg"))
(add-to-list 'minor-mode-map-alist (cons 'autoarg-mode autoarg-mode-map))

(defun autoarg-terminate (n)
  "Maybe terminate a digit prefix sequence.

With a non-negative numeric prefix arg, insert the digits comprising
the arg into the current buffer.  Otherwise use the binding of the key
which invoked this function, excluding the Autoarg keymap."
  (interactive "P")
  (if (numberp n)
      (insert (number-to-string n))
    (let* ((autoarg-mode nil)		; hide the bindings
	   (binding (key-binding (this-single-command-keys))))
      (if binding (call-interactively binding)))))

(provide 'autoarg)

;;; autoarg.el ends here
