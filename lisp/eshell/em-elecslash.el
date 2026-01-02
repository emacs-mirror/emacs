;;; em-elecslash.el --- electric forward slashes  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

;; Electric forward slash in remote Eshells.

;;; Code:

(require 'tramp)
(require 'thingatpt)
(require 'esh-cmd)
(require 'esh-ext)
(require 'esh-mode)

;; This makes us an option when customizing `eshell-modules-list'.
;;;###esh-module-autoload
(progn
(defgroup eshell-elecslash nil
  "Electric forward slash in remote Eshells.

This module helps with supplying absolute file name arguments to
remote commands.  After enabling it, typing a forward slash as
the first character of a command line argument will automatically
insert the Tramp prefix, /method:host:.  The automatic insertion
applies only when `default-directory' is remote and the command
is a Lisp function.

The result is that in most cases of supplying absolute file name
arguments to commands you should see the Tramp prefix inserted
automatically only when that's what you'd reasonably expect.
This frees you from having to keep track of whether commands are
Lisp functions or external when typing command line arguments."
  :tag "Electric forward slash"
  :group 'eshell-module))

;;; Functions:

(defun eshell-elecslash-initialize () ;Called from `eshell-mode' via intern-soft!
  "Initialize remote Eshell electric forward slash support."
  (add-hook 'post-self-insert-hook
            #'eshell-electric-forward-slash nil t))

(defun eshell-electric-forward-slash ()
  "Implementation of electric forward slash in remote Eshells.

Initializing the `eshell-elecslash' module adds this function to
`post-self-insert-hook'.  Typing / or ~/ as the first character
of a command line argument automatically inserts the Tramp prefix
in the case that `default-directory' is remote and the command is
a Lisp function.  Typing a second forward slash undoes the
insertion."
  (when (eq ?/ (char-before))
    (delete-char -1)
    (let ((tilde-before (eq ?~ (char-before)))
          (command (save-excursion
                     (beginning-of-line)
                     (skip-syntax-forward " ")
                     (thing-at-point 'sexp)))
          (prefix (file-remote-p default-directory)))
      (if (and prefix
               ;; We can't formally parse the input.  But if there is
               ;; one of these operators behind us, then looking at
               ;; the first command would not be sensible.  So be
               ;; conservative: don't insert the Tramp prefix if there
               ;; are any of these operators behind us.
               (not (looking-back (regexp-opt '("&&" "|" ";"))
                                  eshell-last-output-end))
	       (or (= (point) eshell-last-output-end)
		   (and tilde-before
                        (= (1- (point)) eshell-last-output-end))
		   (and (or tilde-before
                            (eq ?\s (char-syntax (char-before))))
		        (or (eshell-find-alias-function command)
			    (and (fboundp (intern-soft command))
			         (or eshell-prefer-lisp-functions
				     (not (eshell-search-path command))))))))
	  (let ((map (make-sparse-keymap))
	        (start (if tilde-before (1- (point)) (point))))
	    (when tilde-before (delete-char -1))
	    (insert prefix)
	    (unless tilde-before (insert "/"))
	    ;; Typing a second slash undoes the insertion, for when
	    ;; you really do want to type a local absolute file name.
	    (define-key map "/" (lambda ()
				  (interactive)
				  (delete-region start (point))
				  (insert (if tilde-before "~/" "/"))))
	    (set-transient-map map))
        (insert "/")))))

(provide 'em-elecslash)
;;; em-elecslash.el ends here
