;;; refill.el --- `auto-fill' by refilling paragraphs on changes

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides a mode where paragraphs are refilled after changes in them
;; (using `after-change-hooks').  This gives something akin to typical
;; word processor-style filling.  We restrict refilling due to
;; self-insertion to the characters which trigger auto-fill.

;; It partly satisfies a todo item in enriched.el for some value of
;; `without slowing down editing too much'.  It doesn't attempt to do
;; anything (using `window-size-change-functions'?) about resizing
;; windows -- who cares?

;; This implementation is probably fragile and missing some special
;; cases -- not extensively tested.  Yanking paragraph breaks, for
;; instance, won't DTRT by refilling all the relevant paragraphs.

;; You could do it a bit more efficiently (and robustly?) with just an
;; auto-fill function, but that doesn't cope with changes other than
;; through self-insertion.  (Using auto-fill and after-change
;; functions together didn't seem winning.)  This could probably
;; benefit from a less-general and faster `fill-paragraph-function',
;; ideally as a primitive.

;; The work is done in a local post-command hook but only if
;; `refill-doit' has been set by the after-change function.  Using
;; `post-command-hooks' ensures simply that refilling only happens
;; once per command.

;; [Per Abrahamsen's maniac.el does a similar thing, but operates from
;; post-command-hook.  I don't understand the statement in it that
;; after-change-hooks don't work for this purpose; perhaps there was
;; some Emacs bug at the time.  ISTR maniac has problems with
;; whitespace at the end of paragraphs.]

;;; Code:

(defun refill-fill-paragraph (arg)
  "Like `fill-paragraph' but don't delete whitespace at paragraph end."
  ;; Should probably use a text property indicating previously-filled
  ;; stuff to avoid filling before the point of change.
  (let ((before (point)))
    (save-excursion
      (forward-paragraph)
      (skip-syntax-backward "-")
      (let ((end (point))
	    (beg (progn (backward-paragraph) (point))))
	(goto-char before)
	(save-restriction
	  (if use-hard-newlines
	      (fill-region beg end arg)
	    (fill-region-as-paragraph beg end arg)))))))

(defvar refill-doit nil
  "Non-nil means that `refill-post-command-function' does its processing.
Set by `refill-after-change-function' in `after-change-hooks' and
unset by `refill-post-command-function' in `post-command-hooks'.  This
ensures refilling is only done once per command that causes a change,
regardless of the number of after-change calls from commands doing
complex processing.")
(make-variable-buffer-local 'refill-doit)

(defun refill-after-change-function (beg end len)
  "Function for `after-change-functions' which just sets `refill-doit'."
  (unless undo-in-progress
    (setq refill-doit t)))

(defun refill-post-command-function ()
  "Post-command function to do refilling (conditionally)."
  (when refill-doit		; there was a change
    ;; There's probably scope for more special cases here...
    (cond
     ((eq this-command 'self-insert-command)
      ;; Respond to the same characters as auto-fill (other than
      ;; newline, covered below).
      (if (aref auto-fill-chars (char-before))
	  (refill-fill-paragraph nil)))
     ((or (eq this-command 'quoted-insert)
	  (eq this-command 'fill-paragraph)
	  (eq this-command 'fill-region))
      nil)
     ((or (eq this-command 'newline)
	  (eq this-command 'newline-and-indent)
	  (eq this-command 'open-line))
      ;; Don't zap what was just inserted.
      (save-excursion
	(beginning-of-line)	 ; for newline-and-indent
	(skip-chars-backward "\n")
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (refill-fill-paragraph nil)))
      (widen)
      (save-excursion
	(skip-chars-forward "\n")
	(save-restriction
	  (narrow-to-region (line-beginning-position) (point-max))
	  (refill-fill-paragraph nil))))
     (t (refill-fill-paragraph nil)))
    (setq refill-doit nil)))

(defvar refill-mode nil
  "Non-nil if Refill mode is active.  Use `refill-mode' to toggle it.")
(make-variable-buffer-local 'refill-mode)

(defvar refill-mode-hook nil
  "Normal hook run by function `refill-mode'.")

(add-to-list 'minor-mode-alist '(refill-mode " Refill"))

;;;###autoload
(define-minor-mode refill-mode
  "Toggle Refill minor mode.
With prefix arg, turn Refill mode on iff arg is positive.

When Refill mode is on, the current paragraph will be formatted when
changes are made within it.  Self-inserting characters only cause
refilling if they would cause auto-filling."
  nil " Refill" nil
  ;; This provides the test for recursive paragraph filling.
  (make-local-variable 'fill-paragraph-function)
  (if refill-mode
      (progn (add-hook (make-local-hook 'after-change-functions)
		       'refill-after-change-function nil t)
	     (add-hook (make-local-hook 'post-command-hook)
		       'refill-post-command-function nil t)
	     (set (make-local-variable 'fill-paragraph-function)
		  'refill-fill-paragraph)
	     (auto-fill-mode 0))
    (remove-hook 'after-change-functions 'refill-after-change-function t)
    (remove-hook 'post-command-hook 'refill-post-command-function t)
    (setq fill-paragraph-function nil)))

(provide 'refill)

;;; refill.el ends here
