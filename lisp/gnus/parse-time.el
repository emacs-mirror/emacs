;;; parse-time.el --- Parsing time strings

;; Copyright (C) 1996 by Free Software Foundation, Inc.

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: util

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; With the introduction of the `encode-time', `decode-time', and
;; `format-time-string' functions, dealing with time became simpler in
;; Emacs.  However, parsing time strings is still largely a matter of
;; heuristics and no common interface has been designed.

;; `parse-time-string' parses a time in a string and returns a list of 9
;; values, just like `decode-time', where unspecified elements in the
;; string are returned as nil.  `encode-time' may be applied on these
;; valuse to obtain an internal time value.

;;; Code:

(eval-when-compile (require 'cl))		;and ah ain't kiddin' 'bout it

(put 'parse-time-syntax 'char-table-extra-slots 0)

(defvar parse-time-syntax (make-char-table 'parse-time-syntax))
(defvar parse-time-digits (make-char-table 'parse-time-syntax))

;; Byte-compiler warnings
(defvar elt)
(defvar val)

(unless (aref parse-time-digits ?0)
  (loop for i from ?0 to ?9
	do (set-char-table-range parse-time-digits i (- i ?0))))

(unless (aref parse-time-syntax ?0)
  (loop for i from ?0 to ?9
	do (set-char-table-range parse-time-syntax i ?0))
  (loop for i from ?A to ?Z
	do (set-char-table-range parse-time-syntax i ?A))
  (loop for i from ?a to ?z
	do (set-char-table-range parse-time-syntax i ?a))
  (set-char-table-range parse-time-syntax ?+ 1)
  (set-char-table-range parse-time-syntax ?- -1)
  (set-char-table-range parse-time-syntax ?: ?d)
  )

(defsubst digit-char-p (char)
  (aref parse-time-digits char))

(defsubst parse-time-string-chars (char)
  (aref parse-time-syntax char))

(put 'parse-error 'error-conditions '(parse-error error))
(put 'parse-error 'error-message "Parsing error")

(defsubst parse-integer (string &optional start end)
  "[CL] Parse and return the integer in STRING, or nil if none."
  (let ((integer 0)
	(digit 0)
	(index (or start 0))
	(end (or end (length string))))
    (when (< index end)
      (let ((sign (aref string index)))
	(if (or (eq sign ?+) (eq sign ?-))
	    (setq sign (parse-time-string-chars sign)
		  index (1+ index))
	  (setq sign 1))
	(while (and (< index end)
		    (setq digit (digit-char-p (aref string index))))
	  (setq integer (+ (* integer 10) digit)
		index (1+ index)))
	(if (/= index end)
	    (signal 'parse-error `("not an integer" ,(substring string (or start 0) end)))
	  (* sign integer))))))

(defun parse-time-tokenize (string)
  "Tokenize STRING into substrings."
  (let ((start nil)
	(end (length string))
	(all-digits nil)
	(list ())
	(index 0)
	(c nil))
    (while (< index end)
      (while (and (< index end)		;skip invalid characters
		  (not (setq c (parse-time-string-chars (aref string index)))))
	(incf index))
      (setq start index all-digits (eq c ?0))
      (while (and (< (incf index) end)	;scan valid characters
		  (setq c (parse-time-string-chars (aref string index))))
	(setq all-digits (and all-digits (eq c ?0))))
      (if (<= index end)
	  (push (if all-digits (parse-integer string start index)
		  (substring string start index))
		list)))
    (nreverse list)))

(defvar parse-time-months '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
			    ("Apr" . 4) ("May" . 5) ("Jun" . 6)
			    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
			    ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))
(defvar parse-time-weekdays '(("Sun" . 0) ("Mon" . 1) ("Tue" . 2)
			      ("Wed" . 3) ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))
(defvar parse-time-zoneinfo `(("Z" 0) ("UT" 0) ("GMT" 0)
			      ("PST" ,(* -8 3600)) ("PDT" ,(* -7 3600) t)
			      ("MST" ,(* -7 3600)) ("MDT" ,(* -6 3600) t)
			      ("CST" ,(* -6 3600)) ("CDT" ,(* -5 3600) t)
			      ("EST" ,(* -5 3600)) ("EDT" ,(* -4 3600) t))
  "(zoneinfo seconds-off daylight-savings-time-p)")

(defvar parse-time-rules
  `(((6) parse-time-weekdays)
    ((3) (1 31))
    ((4) parse-time-months)
    ((5) (1970 2038))
    ((2 1 0)
     ,#'(lambda () (and (stringp elt)
			(= (length elt) 8)
			(= (aref elt 2) ?:)
			(= (aref elt 5) ?:)))
     [0 2] [3 5] [6 8])
    ((8 7) parse-time-zoneinfo
     ,#'(lambda () (car val))
     ,#'(lambda () (cadr val)))
    ((8)
     ,#'(lambda ()
	  (and (stringp elt)
	       (= 5 (length elt))
	       (or (= (aref elt 0) ?+) (= (aref elt 0) ?-))))
     ,#'(lambda () (* 60 (+ (parse-integer elt 3 5)
			    (* 60 (parse-integer elt 1 3)))
		      (if (= (aref elt 0) ?-) -1 1))))
    ((5 4 3)
     ,#'(lambda () (and (stringp elt) (= (length elt) 10) (= (aref elt 4) ?-) (= (aref elt 7) ?-)))
     [0 4] [5 7] [8 10])
    ((2 1)
     ,#'(lambda () (and (stringp elt) (= (length elt) 5) (= (aref elt 2) ?:)))
     [0 2] [3 5])
    ((5) (70 99) ,#'(lambda () (+ 1900 elt))))
  "(slots predicate extractor...)")

(defun parse-time-string (string)
  "Parse the time-string STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil."
  (let ((time (list nil nil nil nil nil nil nil nil nil nil))
	(temp (parse-time-tokenize string)))
    (while temp
      (let ((elt (pop temp))
	    (rules parse-time-rules)
	    (exit nil))
	(while (and (not (null rules)) (not exit))
	  (let* ((rule (pop rules))
		 (slots (pop rule))
		 (predicate (pop rule))
		 (val))
	    (if (and (not (nth (car slots) time)) ;not already set
		     (setq val (cond ((and (consp predicate)
					   (not (eq (car predicate) 'lambda)))
				      (and (numberp elt)
					   (<= (car predicate) elt)
					   (<= elt (cadr predicate))
					   elt))
				     ((symbolp predicate)
				      (cdr (assoc elt (symbol-value predicate))))
				     ((funcall predicate)))))
		(progn
		  (setq exit t)
		  (while slots
		    (let ((new-val (and rule
					(let ((this (pop rule)))
					  (if (vectorp this)
					      (parse-integer elt (aref this 0) (aref this 1))
					    (funcall this))))))
		      (rplaca (nthcdr (pop slots) time) (or new-val val))))))))))
    time))

(provide 'parse-time)

;;; parse-time.el ends here
