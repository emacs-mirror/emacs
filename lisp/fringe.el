;;; fringe.el --- change fringes appearance in various ways

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Maintainer: FSF
;; Keywords: frames

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

;; This file contains helpful functions for customizing the appearance
;; of the fringe.

;; The code is influenced by scroll-bar.el and avoid.el.  The author
;; gratefully acknowledge comments and suggestions made by Miles
;; Bader, Eli Zaretski, Richard Stallman, Pavel Janík and others which
;; improved this package.

;;; Code:

(defvar fringe-mode)

(defun set-fringe-mode-1 (ignore value)
  "Call `set-fringe-mode' with VALUE.
See `fringe-mode' for valid values and their effect.
This is usually invoked when setting `fringe-mode' via customize."
  (set-fringe-mode value))

(defun set-fringe-mode (value)
  "Set `fringe-mode' to VALUE and put the new value into effect.
See `fringe-mode' for possible values and their effect."
  (setq fringe-mode value)

  ;; Apply it to default-frame-alist.
  (let ((parameter (assq 'left-fringe default-frame-alist)))
    (if (consp parameter)
	(setcdr parameter fringe-mode)
      (setq default-frame-alist
	    (cons (cons 'left-fringe (if (consp fringe-mode)
					 (car fringe-mode)
				       fringe-mode))
		  default-frame-alist))))
  (let ((parameter (assq 'right-fringe default-frame-alist)))
    (if (consp parameter)
	(setcdr parameter fringe-mode)
      (setq default-frame-alist
	    (cons (cons 'right-fringe (if (consp fringe-mode)
					  (cdr fringe-mode)
					fringe-mode))
		  default-frame-alist))))

  ;; Apply it to existing frames.
  (let ((frames (frame-list)))
    (while frames
      (modify-frame-parameters
       (car frames)
       (list (cons 'left-fringe (if (consp fringe-mode)
				    (car fringe-mode)
				  fringe-mode))
	     (cons 'right-fringe (if (consp fringe-mode)
				     (cdr fringe-mode)
				   fringe-mode))))
      (setq frames (cdr frames)))))

(defcustom fringe-mode nil
  "*Specify appearance of fringes on all frames.
This variable can be nil (the default) meaning the fringes should have
the default width (8 pixels), it can be an integer value specifying
the width of both left and right fringe (where 0 means no fringe), or
a cons cell where car indicates width of left fringe and cdr indicates
width of right fringe (where again 0 can be used to indicate no
fringe).
To set this variable in a Lisp program, use `set-fringe-mode' to make
it take real effect.
Setting the variable with a customization buffer also takes effect.
If you only want to modify the appearance of the fringe in one frame,
you can use the interactive function `toggle-fringe'"
  :type '(choice (const :tag "Default width" nil)
		 (const :tag "No fringes" 0)
		 (const :tag "Only right" (0 . nil))
		 (const :tag "Only left" (nil . 0))
		 (const :tag "Half width" (5 . 5))
		 (integer :tag "Specific width")
		 (cons :tag "Different left/right sizes"
		       (integer :tag "Left width")
		       (integer :tag "Right width")))
  :group 'frames
  :require 'fringe
  :set 'set-fringe-mode-1)

(defun fringe-query-style (&optional all-frames)
  "Query user for fringe style.
Returns values suitable for left-fringe and right-fringe frame parameters.
If ALL-FRAMES, the negation of the fringe values in
`default-frame-alist' is used when user enters the empty string.
Otherwise the negation of the fringe value in the currently selected
frame parameter is used."
  (let ((mode (intern (completing-read
		       "Select fringe mode for all frames (SPACE for list): "
		       '(("none") ("default") ("left-only")
			 ("right-only") ("half"))
		       nil t))))
    (cond ((eq mode 'none) 0)
	  ((eq mode 'default) nil)
	  ((eq mode 'left-only) '(nil . 0))
	  ((eq mode 'right-only) '(0 . nil))
	  ((eq mode 'half) '(5 . 5))
	  ((eq mode (intern ""))
	   (if (eq 0 (cdr (assq 'left-fringe
				(if all-frames
				    default-frame-alist
				  (frame-parameters (selected-frame))))))
	       nil
	     0)))))

;;;###autoload
(defun fringe-mode (&optional mode)
  "Toggle appearance of fringes on all frames.
Valid values for MODE include `none', `default', `left-only',
`right-only' and `half'.  MODE can also be a cons cell where the
integer in car will be used as left fringe width and the integer in
cdr will be used as right fringe width. If MODE is not specified, the
user is queried.
It applies to all frames that exist and frames to be created in the
future.
If you want to set appearance of fringes on the selected frame only,
see `set-fringe-style'."
  (interactive (list (fringe-query-style 'all-frames)))
  (set-fringe-mode mode))

;;;###autoload
(defun set-fringe-style (&optional mode)
  "Set appearance of fringes on selected frame.
Valid values for MODE include `none', `default', `left-only',
`right-only' and `half'.  MODE can also be a cons cell where the
integer in car will be used as left fringe width and the integer in
cdr will be used as right fringe width. If MODE is not specified, the
user is queried.
If you want to set appearance of fringes on all frames, see `fringe-mode'."
  (interactive (list (fringe-query-style)))
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'left-fringe (if (consp mode) (car mode) mode))
	 (cons 'right-fringe (if (consp mode) (cdr mode) mode)))))

(provide 'fringe)

;;; fringe.el ends here
