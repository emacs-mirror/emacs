;;; studly.el --- StudlyCaps (tm)(r)(c)(xxx)

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun studlify-region (begin end)
  "Studlify-case the region"
  (interactive "*r")
  (save-excursion
    (goto-char begin)
    (setq begin (point))
    (while (and (<= (point) end)
		(not (looking-at "\\W*\\'")))
      (forward-word 1)
      (backward-word 1)
      (setq begin (max (point) begin))
      (forward-word 1)
      (let ((offset 0)
	    (word-end (min (point) end))
	    c)
	(goto-char begin)
	(while (< (point) word-end)
	  (setq offset (+ offset (following-char)))
	  (forward-char 1))
	(setq offset (+ offset (following-char)))
	(goto-char begin)
	(while (< (point) word-end)
	  (setq c (following-char))
	  (if (and (= (% (+ c offset) 4) 2)
		   (let ((ch (following-char)))
		     (or (and (>= ch ?a) (<= ch ?z))
			 (and (>= ch ?A) (<= ch ?Z)))))
	      (progn
		(delete-char 1)
		(insert (logxor c ? ))))
	  (forward-char 1))
	(setq begin (point))))))

(defun studlify-word (count)
  "Studlify-case the current word, or COUNT words if given an argument"
  (interactive "*p")
  (let ((begin (point)) end rb re)
    (forward-word count)
    (setq end (point))
    (setq rb (min begin end) re (max begin end))
    (studlify-region rb re)))

(defun studlify-buffer ()
  "Studlify-case the current buffer"
  (interactive "*")
  (studlify-region (point-min) (point-max)))

;;; studly.el ends here
