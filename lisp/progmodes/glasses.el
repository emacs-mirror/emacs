;;; glasses.el --- make cantReadThis readable

;; Copyright (C) 1999, 2000, 2001, 2005 Free Software Foundation, Inc.

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Maintainer: Milan Zamazal <pdm@zamazal.org>
;; Keywords: tools

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

;; This file defines a minor mode for making unreadableIdentifiersLikeThis
;; readable.  In some environments, for instance Java, it is common to use such
;; unreadable identifiers.  It is not good to use underscores in identifiers of
;; your own project in such an environment to make your sources more readable,
;; since it introduces undesirable confusion, which is worse than the
;; unreadability.  Fortunately, you use Emacs for the subproject, so the
;; problem can be solved some way.
;;
;; This file defines the `glasses-mode' minor mode, which displays underscores
;; between all the pairs of lower and upper English letters.  (This only
;; displays underscores, the text is not changed actually.)  Alternatively, you
;; can say you want the capitals in some given face (e.g. bold).
;;
;; The mode does something usable, though not perfect.  Improvement suggestions
;; from Emacs experts are welcome.
;;
;; If you like in-identifier separators different from underscores, change the
;; value of the variable `glasses-separator' appropriately.  See also the
;; variables `glasses-face' and `glasses-convert-on-write-p'.  You can also use
;; the command `M-x customize-group RET glasses RET'.
;;
;; If you set any of the variables `glasses-separator' or `glasses-face' after
;; glasses.el is loaded in a different way than through customize, you
;; should call the function `glasses-set-overlay-properties' afterwards.

;;; Code:


(eval-when-compile
  (require 'cl))


;;; User variables


(defgroup glasses nil
  "Make unreadable code likeThis(one) readable."
  :version "21.1"
  :group 'tools)


(defcustom glasses-separator "_"
  "*String to be displayed as a visual separator in unreadable identifiers."
  :group 'glasses
  :type 'string
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-face nil
  "*Face to be put on capitals of an identifier looked through glasses.
If it is nil, no face is placed at the capitalized letter.

For example, you can set `glasses-separator' to an empty string and
`glasses-face' to `bold'.  Then unreadable identifiers will have no separators,
but will have their capitals in bold."
  :group 'glasses
  :type 'symbol
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-separate-parentheses-p t
  "*If non-nil, ensure space between an identifier and an opening parenthesis."
  :group 'glasses
  :type 'boolean)


(defcustom glasses-uncapitalize-p nil
  "*If non-nil, downcase embedded capital letters in identifiers.
Only identifiers starting with lower case letters are affected, letters inside
other identifiers are unchanged."
  :group 'glasses
  :type 'boolean
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-uncapitalize-regexp "[a-z]"
  "*Regexp matching beginnings of words to be uncapitalized.
Only words starting with this regexp are uncapitalized.
The regexp is case sensitive.
It has any effect only when `glasses-uncapitalize-p' is non-nil."
  :group 'glasses
  :type 'regexp
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-convert-on-write-p nil
  "*If non-nil, remove separators when writing glasses buffer to a file.
If you are confused by glasses so much, that you write the separators into code
during coding, set this variable to t.  The separators will be removed on each
file write then.

Note the removal action does not try to be much clever, so it can remove real
separators too."
  :group 'glasses
  :type 'boolean)


(defun glasses-custom-set (symbol value)
  "Set value of the variable SYMBOL to VALUE and update overlay categories.
Used in :set parameter of some customized glasses variables."
  (set-default symbol value)
  (glasses-set-overlay-properties))


;;; Utility functions


(defun glasses-set-overlay-properties ()
  "Set properties of glasses overlays.
Consider current setting of user variables."
  ;; In-identifier overlay
  (put 'glasses 'evaporate t)
  (put 'glasses 'before-string glasses-separator)
  (put 'glasses 'face glasses-face)
  ;; Beg-identifier overlay
  (put 'glasses-init 'evaporate t)
  (put 'glasses-init 'face glasses-face)
  ;; Parenthesis overlay
  (put 'glasses-parenthesis 'evaporate t)
  (put 'glasses-parenthesis 'before-string " "))

(glasses-set-overlay-properties)


(defun glasses-overlay-p (overlay)
  "Return whether OVERLAY is an overlay of glasses mode."
  (memq (overlay-get overlay 'category)
	'(glasses glasses-init glasses-parenthesis)))


(defun glasses-make-overlay (beg end &optional category)
  "Create and return readability overlay over the region from BEG to END.
CATEGORY is the overlay category.  If it is nil, use the `glasses' category."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category (or category 'glasses))
    overlay))


(defun glasses-make-readable (beg end)
  "Make identifiers in the region from BEG to END readable."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
	;; Face only
	(goto-char beg)
	(while (re-search-forward
		"\\<\\([A-Z]\\)[a-zA-Z]*\\([a-z][A-Z]\\|[A-Z][a-z]\\)"
		end t)
	  (glasses-make-overlay (match-beginning 1) (match-end 1)
				'glasses-init))
	;; Face + separator
	(goto-char beg)
	(while (re-search-forward "[a-z]\\([A-Z]\\)\\|[A-Z]\\([A-Z]\\)[a-z]"
				  end t)
	  (let* ((n (if (match-string 1) 1 2))
		 (o (glasses-make-overlay (match-beginning n) (match-end n))))
	    (goto-char (match-beginning n))
	    (when (and glasses-uncapitalize-p
		       (save-match-data
			 (looking-at "[A-Z]\\($\\|[^A-Z]\\)"))
		       (save-excursion
			 (save-match-data
			   (re-search-backward "\\<.")
			   (looking-at glasses-uncapitalize-regexp))))
	      (overlay-put o 'invisible t)
	      (overlay-put o 'after-string (downcase (match-string n))))))
        ;; Separator change
        (unless (string= glasses-separator "_")
          (goto-char beg)
          (while (re-search-forward "[a-zA-Z0-9]\\(_+\\)[a-zA-Z0-9]" end t)
            (goto-char (match-beginning 1))
            (while (eql (char-after) ?\_)
              (let ((o (glasses-make-overlay (point) (1+ (point)))))
                ;; `concat' ensures the character properties won't merge
                (overlay-put o 'display (concat glasses-separator)))
              (forward-char))))
	;; Parentheses
	(when glasses-separate-parentheses-p
	  (goto-char beg)
	  (while (re-search-forward "[a-zA-Z]_*\\(\(\\)" end t)
	    (glasses-make-overlay (match-beginning 1) (match-end 1)
				  'glasses-parenthesis)))))))


(defun glasses-make-unreadable (beg end)
  "Return identifiers in the region from BEG to END to their unreadable state."
  (dolist (o (overlays-in beg end))
    (when (glasses-overlay-p o)
      (delete-overlay o))))


(defun glasses-convert-to-unreadable ()
  "Convert current buffer to unreadable identifiers and return nil.
This function modifies buffer contents, it removes all the separators,
recognized according to the current value of the variable `glasses-separator'."
  (when (and glasses-convert-on-write-p
	     (not (string= glasses-separator "")))
    (let ((case-fold-search nil)
	  (separator (regexp-quote glasses-separator)))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		(format "[a-z]\\(%s\\)[A-Z]\\|[A-Z]\\(%s\\)[A-Z][a-z]"
			separator separator)
		nil t)
	  (let ((n (if (match-string 1) 1 2)))
	    (replace-match "" t nil nil n)
	    (goto-char (match-end n))))
        (unless (string= glasses-separator "_")
          (goto-char (point-min))
          (while (re-search-forward (format "[a-zA-Z0-9]\\(%s+\\)[a-zA-Z0-9]"
                                            separator)
                                    nil t)
            (replace-match "_" nil nil nil 1)
            (goto-char (match-beginning 1))))
	(when glasses-separate-parentheses-p
	  (goto-char (point-min))
	  (while (re-search-forward "[a-zA-Z]_*\\( \\)\(" nil t)
	    (replace-match "" t nil nil 1))))))
  ;; nil must be returned to allow use in write file hooks
  nil)


(defun glasses-change (beg end &optional old-len)
  "After-change function updating glass overlays."
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
	(end-line (save-excursion (goto-char end) (line-end-position))))
    (glasses-make-unreadable beg-line end-line)
    (glasses-make-readable beg-line end-line)))


;;; Minor mode definition


;;;###autoload
(define-minor-mode glasses-mode
  "Minor mode for making identifiers likeThis readable.
When this mode is active, it tries to add virtual separators (like underscores)
at places they belong to."
  :group 'glasses :lighter " o^o"
  (save-excursion
    (save-restriction
      (widen)
      ;; We erase all the overlays anyway, to avoid dual sight in some
      ;; circumstances
      (glasses-make-unreadable (point-min) (point-max))
      (if glasses-mode
	  (progn
	    (jit-lock-register 'glasses-change)
	    (add-hook 'local-write-file-hooks
		      'glasses-convert-to-unreadable nil t))
	(jit-lock-unregister 'glasses-change)
	(remove-hook 'local-write-file-hooks
		     'glasses-convert-to-unreadable t)))))


;;; Announce

(provide 'glasses)


;;; arch-tag: a3515167-c89e-484f-90a1-d85143e52b12
;;; glasses.el ends here
