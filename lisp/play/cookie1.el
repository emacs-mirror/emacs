;;; cookie.el --- retrieve random phrases from fortune cookie files

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Keywords: games
;; Created: Mon Mar 22 17:06:26 1993

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Support for random cookie fetches from phrase files, used for such
;; critical applications as emulating Zippy the Pinhead and confounding
;; the NSA Trunk Trawler.
;;
;; The two entry points are `cookie' and `cookie-insert'.  The helper
;; functions `pick-random' and `shuffle-vector' may be of interest to
;; programmers.
;;
;; The code expects phrase files to be in ITS-style LINS format
;; (strings terminated by ASCII 0 characters, leading whitespace
;; ignored).  Everything up to the first delimiter is treated as a
;; comment.  Other formats (notably UNIX fortune-file format) could
;; easily be supported by changing the `cookie-delimiter' constant.
;;
;; This code derives from Steve Strassman's 1987 spook.el package, but
;; has been generalized so that it supports multiple simultaneous cookie
;; databases.  It is intended to be called from other packages such as
;; yow.el and spook.el.
;;
;; TO DO: teach cookie-snarf to auto-detect ITS PINS or UNIX fortune(6)
;; format and do the right thing.

;;; Code:

; Randomize the seed in the random number generator.
(random t)

(defconst cookie-delimiter "\0"
  "Delimiter used to separate cookie file entries.")

(defun cookie (phrase-file startmsg endmsg)
  "Return a random phrase from PHRASE-FILE.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end."
  (let ((cookie-vector (cookie-snarf phrase-file startmsg endmsg)))
    (shuffle-vector cookie-vector)
    (aref cookie-vector 1)))

(defun cookie-insert (phrase-file &optional count startmsg endmsg)
  "Insert random phrases from PHRASE-FILE; COUNT of them.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end."
  (let ((cookie-vector (cookie-snarf phrase-file startmsg endmsg)))
    (shuffle-vector cookie-vector)
    (let ((start (point)))
      (insert ?\n)
      (cookie1 (min (- (length cookie-vector) 1) (or count 1)) cookie-vector)
      (insert ?\n)
      (fill-region-as-paragraph start (point) nil))))

(defun cookie1 (arg cookie-vec)
  "Inserts a cookie phrase ARG times."
  (cond ((zerop arg) t)
	(t (insert (aref cookie-vec arg))
	   (insert " ")
	   (cookie1 (1- arg) cookie-vec))))

(defun cookie-snarf (phrase-file startmsg endmsg)
  "Reads in the PHRASE-FILE, returns it as a vector of strings.  Emit
STARTMSG and ENDMSG before and after.  Caches the result; second and
subsequent calls on the same file won't go to disk."
  (if (boundp (intern phrase-file))
      (eval (intern phrase-file))
    (message startmsg)
    (save-excursion
      (let ((buf (generate-new-buffer "*cookie*"))
	    (result nil))
	(set-buffer buf)
	(insert-file-contents (expand-file-name phrase-file))
	(search-forward cookie-delimiter)
	(while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
	  (let ((beg (point)))
	    (search-forward "\0")
	    (setq result (cons (buffer-substring beg (1- (point)))
			       result))))
	(kill-buffer buf)
	(message endmsg)
	(set (intern phrase-file) (apply 'vector result))))))

(defun pick-random (n)
  "Returns a random number from 0 to N-1 inclusive."
  (% (logand 0777777 (random)) n))

; Thanks to Ian G Batten <BattenIG@CS.BHAM.AC.UK>
; [of the University of Birmingham Computer Science Department]
; for the iterative version of this shuffle.
;
(defun shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)"
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (pick-random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

(provide 'cookie)

;;; cookie.el ends here
