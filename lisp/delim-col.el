;;; delim-col.el --- Prettify all columns in a region or rectangle.

;; Copyright (C) 1999 Vinicius Jose Latorre <vinicius@cpqd.com.br>

;; Author: Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Time-stamp:	<99/08/21 19:51:13 vinicius>
;; Version: 1.3
;; Keywords: internal

;; This file is part of GNU Emacs.

;; This GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; delim-col helps to prettify columns in a text region or rectangle.
;;
;; To use it, make sure that this file is in load-path and insert in your
;; .emacs:
;;
;;    (require 'delim-col)
;;
;; If you have, for example, the following columns:
;;
;;	a	b	c	d
;;	aaaa	bb	ccc	ddddd
;;	aaa	bbb	cccc   	dddd
;;	aa	bb	ccccccc	ddd
;;
;; And the following settings:
;;
;;    (setq delimit-columns-str-before "[ ")
;;    (setq delimit-columns-str-after " ]")
;;    (setq delimit-columns-str-separator ", ")
;;    (setq delimit-columns-separator "\t")
;;
;; If you select the lines above and type:
;;
;;    M-x delimit-columns-region RET
;;
;; You obtain the following result:
;;
;;	[ a   , b  , c      , d     ]
;;	[ aaaa, bb , ccc    , ddddd ]
;;	[ aaa , bbb, cccc   , dddd  ]
;;	[ aa  , bb , ccccccc, ddd   ]
;;
;; But if you select start from the very first b and the very last c and type:
;;
;;    M-x delimit-columns-rectangle RET
;;
;; You obtain the following result:
;;
;;	a	[ b  , c       ]	d
;;	aaaa	[ bb , ccc     ]	ddddd
;;	aaa	[ bbb, cccc    ]	dddd
;;	aa	[ bb , ccccccc ]	ddd
;;
;; Note that `delimit-columns-region' operates over all text region
;; selected, extending the region start to the beginning of line and the
;; region end to the end of line.  While `delimit-columns-rectangle'
;; operates over the text rectangle selected which rectangle diagonal is
;; given by the region start and end.
;;
;; `delimit-columns-region' is useful when you have columns of text that
;; are not well aligned, like:
;;
;;	horse	apple	bus
;;	dog	pineapple	car
;;	porcupine	strawberry	airplane
;;
;; `delimit-columns-region' and `delimit-columns-rectangle' handle lines
;; with different number of columns, like:
;;
;;	horse	apple	bus
;;	dog	pineapple	car	EXTRA
;;	porcupine	strawberry	airplane

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:

(defvar delimit-columns-str-before ""
  "*Specify a string to be inserted before all columns.")

(defvar delimit-columns-str-separator ", "
  "*Specify a string to be inserted between each column.")

(defvar delimit-columns-str-after ""
  "*Specify a string to be inserted after all columns.")

(defvar delimit-columns-separator "\t"
  "*Specify a regexp which separates each column.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands:


;;;###autoload
(defun delimit-columns-region (start end)
  "Prettify all columns in a text region.

START and END delimits the text region."
  (interactive "*r")
  (let ((delimit-columns-str-before
	 (if (stringp delimit-columns-str-before)
	     delimit-columns-str-before
	   ""))
	(delimit-columns-str-separator
	 (if (stringp delimit-columns-str-separator)
	     delimit-columns-str-separator
	   " "))
	(delimit-columns-str-after
	 (if (stringp delimit-columns-str-after)
	     delimit-columns-str-after
	   ""))
	(delimit-columns-limit (make-marker))
	(the-end (copy-marker end))
	delimit-columns-max)
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      ;; get maximum length for each column
      (save-excursion
	(while (< (point) the-end)
	  (delimit-columns-rectangle-max
	   (prog1
	       (point)
	     (end-of-line)))
	  (forward-char 1)))
      ;; prettify columns
      (while (< (point) the-end)
	(delimit-columns-rectangle-line
	 (prog1
	     (point)
	   (end-of-line)))
	(forward-char 1))
      ;; nullify markers
      (set-marker delimit-columns-limit nil)
      (set-marker the-end nil))))


(require 'rect)


;;;###autoload
(defun delimit-columns-rectangle (start end)
  "Prettify all columns in a text rectangle.

START and END delimits the corners of text rectangle."
  (interactive "*r")
  (let ((delimit-columns-str-before
	 (if (stringp delimit-columns-str-before)
	     delimit-columns-str-before
	   ""))
	(delimit-columns-str-separator
	 (if (stringp delimit-columns-str-separator)
	     delimit-columns-str-separator
	   " "))
	(delimit-columns-str-after
	 (if (stringp delimit-columns-str-after)
	     delimit-columns-str-after
	   ""))
	(delimit-columns-limit (make-marker))
	(the-end (copy-marker end))
	delimit-columns-max)
    ;; get maximum length for each column
    (save-excursion
      (operate-on-rectangle 'delimit-columns-rectangle-max
			    start the-end t))
    ;; prettify columns
    (save-excursion
      (operate-on-rectangle 'delimit-columns-rectangle-line
			    start the-end t))
    ;; nullify markers
    (set-marker delimit-columns-limit nil)
    (set-marker the-end nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables and Functions:


;; to avoid compilation gripes
(defvar delimit-columns-max nil)
(defvar delimit-columns-limit nil)


(defun delimit-columns-rectangle-max (startpos &optional ignore ignore)
  (set-marker delimit-columns-limit (point))
  (goto-char startpos)
  (let ((ncol 1)
	origin values)
    ;; get current column length
    (while (progn
	     (setq origin (current-column))
	     (re-search-forward delimit-columns-separator
				delimit-columns-limit 'move))
      (save-excursion
	(goto-char (match-beginning 0))
	(setq values (cons (- (current-column) origin)
			   values)))
      (setq ncol (1+ ncol)))
    (setq values (cons (- (current-column) origin)
		       values))
    ;; extend delimit-columns-max, if needed
    (let ((index (length delimit-columns-max)))
      (and (> ncol index)
	   (let ((extend (make-vector ncol 0)))
	     (while (> index 0)
	       (setq index (1- index))
	       (aset extend index (aref delimit-columns-max index)))
	     (setq delimit-columns-max extend))))
    ;; get maximum column length
    (while values
      (setq ncol (1- ncol))
      (aset delimit-columns-max ncol (max (aref delimit-columns-max ncol)
					  (car values)))
      (setq values (cdr values)))))


(defun delimit-columns-rectangle-line (startpos &optional ignore ignore)
  (let ((ncol 0)
	(len (length delimit-columns-max))
	origin)
    (set-marker delimit-columns-limit (point))
    (goto-char startpos)
    (insert delimit-columns-str-before)
    ;; Adjust all columns but last one
    (while (progn
	     (setq origin (current-column))
	     (and (< (point) delimit-columns-limit)
		  (re-search-forward delimit-columns-separator
				     delimit-columns-limit 'move)))
      (delete-region (match-beginning 0) (point))
      (insert (make-string (- (aref delimit-columns-max ncol)
			      (- (current-column) origin))
			   ?\ )
	      delimit-columns-str-separator)
      (setq ncol (1+ ncol)))
    ;; Adjust last column
    (insert (make-string (- (aref delimit-columns-max ncol)
			    (- (current-column) origin))
			 ?\ ))
    ;; Adjust extra columns, if needed
    (while (< (setq ncol (1+ ncol)) len)
      (insert delimit-columns-str-separator
	      (make-string (aref delimit-columns-max ncol) ?\ )))
    (insert delimit-columns-str-after)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'delim-col)


;;; delim-col.el ends here
