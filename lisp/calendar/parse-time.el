;;; parse-time.el --- parsing time strings -*- lexical-binding: t -*-

;; Copyright (C) 1996, 2000-2022 Free Software Foundation, Inc.

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: util

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

;; With the introduction of the `encode-time', `decode-time', and
;; `format-time-string' functions, dealing with time became simpler in
;; Emacs.  However, parsing time strings is still largely a matter of
;; heuristics and no common interface has been designed.

;; `parse-time-string' parses a time in a string and returns a list of
;; values, just like `decode-time', where unspecified elements in the
;; string are returned as nil (except unspecified DST is returned as -1).
;; `encode-time' may be applied on these values to obtain an internal
;; time value.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(eval-when-compile (require 'subr-x))

;; Byte-compiler warnings
(defvar parse-time-elt)
(defvar parse-time-val)

(defsubst parse-time-string-chars (char)
  (cond ((<= ?a char ?z) ?a)
        ((<= ?0 char ?9) ?0)
        ((eq char ?+) 1)
        ((eq char ?-) -1)
        ((eq char ?:) ?d)))

(defun parse-time-tokenize (string)
  "Tokenize STRING into substrings.
Each substring is a run of \"valid\" characters, i.e., lowercase
letters, digits, plus or minus signs or colons."
  (let ((start nil)
	(end (length string))
	(all-digits nil)
	(list ())
	(index 0)
	(c nil))
    (while (< index end)
      (while (and (< index end)		;Skip invalid characters.
		  (not (setq c (parse-time-string-chars (aref string index)))))
	(cl-incf index))
      (setq start index
            all-digits (eq c ?0))
      (while (and (< (cl-incf index) end)	;Scan valid characters.
		  (setq c (parse-time-string-chars (aref string index))))
	(setq all-digits (and all-digits (eq c ?0))))
      (if (<= index end)
	  (push (if all-digits (cl-parse-integer string :start start :end index)
		  (substring string start index))
		list)))
    (nreverse list)))

(defvar parse-time-months '(("jan" . 1) ("feb" . 2) ("mar" . 3)
			    ("apr" . 4) ("may" . 5) ("jun" . 6)
			    ("jul" . 7) ("aug" . 8) ("sep" . 9)
			    ("oct" . 10) ("nov" . 11) ("dec" . 12)
			    ("january" . 1) ("february" . 2)
			    ("march" . 3) ("april" . 4) ("june" . 6)
			    ("july" . 7) ("august" . 8)
			    ("september" . 9) ("october" . 10)
			    ("november" . 11) ("december" . 12)))
(defvar parse-time-weekdays '(("sun" . 0) ("mon" . 1) ("tue" . 2)
			      ("wed" . 3) ("thu" . 4) ("fri" . 5)
			      ("sat" . 6) ("sunday" . 0) ("monday" . 1)
			      ("tuesday" . 2) ("wednesday" . 3)
			      ("thursday" . 4) ("friday" . 5)
			      ("saturday" . 6)))
(defvar parse-time-zoneinfo `(("z" 0) ("ut" 0) ("gmt" 0)
			      ("pst" ,(* -8 3600)) ("pdt" ,(* -7 3600) t)
			      ("mst" ,(* -7 3600)) ("mdt" ,(* -6 3600) t)
			      ("cst" ,(* -6 3600)) ("cdt" ,(* -5 3600) t)
			      ("est" ,(* -5 3600)) ("edt" ,(* -4 3600) t))
  "(zoneinfo seconds-off daylight-savings-time-p)")

(defvar parse-time-rules
  `(((6) parse-time-weekdays)
    ((3) (1 31))
    ((4) parse-time-months)
    ((5) (100))
    ((2 1 0)
     ,(lambda () (and (stringp parse-time-elt)
                      (= (length parse-time-elt) 8)
                      (= (aref parse-time-elt 2) ?:)
                      (= (aref parse-time-elt 5) ?:)))
     [0 2] [3 5] [6 8])
    ((8 7) parse-time-zoneinfo
     ,(lambda () (car parse-time-val))
     ,(lambda () (cadr parse-time-val)))
    ((8)
     ,(lambda ()
        (and (stringp parse-time-elt)
             (= 5 (length parse-time-elt))
             (or (= (aref parse-time-elt 0) ?+)
                 (= (aref parse-time-elt 0) ?-))))
     ,(lambda () (* 60 (+ (cl-parse-integer parse-time-elt :start 3 :end 5)
                          (* 60 (cl-parse-integer parse-time-elt :start 1 :end 3)))
                    (if (= (aref parse-time-elt 0) ?-) -1 1))))
    ((5 4 3)
     ,(lambda () (and (stringp parse-time-elt)
                      (= (length parse-time-elt) 10)
                      (= (aref parse-time-elt 4) ?-)
                      (= (aref parse-time-elt 7) ?-)))
     [0 4] [5 7] [8 10])
    ((2 1 0)
     ,(lambda () (and (stringp parse-time-elt)
                      (= (length parse-time-elt) 5)
                      (= (aref parse-time-elt 2) ?:)))
     [0 2] [3 5] ,(lambda () 0))
    ((2 1 0)
     ,(lambda () (and (stringp parse-time-elt)
                      (= (length parse-time-elt) 4)
                      (= (aref parse-time-elt 1) ?:)))
     [0 1] [2 4] ,(lambda () 0))
    ((2 1 0)
     ,(lambda () (and (stringp parse-time-elt)
                      (= (length parse-time-elt) 7)
                      (= (aref parse-time-elt 1) ?:)))
     [0 1] [2 4] [5 7])
    ((5) (50 110) ,(lambda () (+ 1900 parse-time-elt)))
    ((5) (0 49) ,(lambda () (+ 2000 parse-time-elt))))
  "(slots predicate extractor...)")
;;;###autoload(put 'parse-time-rules 'risky-local-variable t)

;;;###autoload
(defun parse-time-string (string)
  "Parse the time in STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
STRING should be an ISO 8601 time string, e.g., \"2020-01-15T16:12:21-08:00\",
or something resembling an RFC 822 (or later) date-time, e.g.,
\"Wed, 15 Jan 2020 16:12:21 -0800\".  This function is
somewhat liberal in what format it accepts, and will attempt to
return a \"likely\" value even for somewhat malformed strings.
The values returned are identical to those of `decode-time', but
any unknown values other than DST are returned as nil, and an
unknown DST value is returned as -1."
  (condition-case ()
      (iso8601-parse string)
    (wrong-type-argument
     (let ((time (list nil nil nil nil nil nil nil -1 nil))
	   (temp (parse-time-tokenize (downcase string))))
       (while temp
	 (let ((parse-time-elt (pop temp))
	       (rules parse-time-rules)
	       (exit nil))
	   (while (and rules (not exit))
	     (let* ((rule (pop rules))
		    (slots (pop rule))
		    (predicate (pop rule))
		    (parse-time-val))
	       (when (and (not (nth (car slots) time)) ;not already set
			  (setq parse-time-val
				(cond ((and (consp predicate)
					    (not (functionp predicate)))
				       (and (numberp parse-time-elt)
					    (<= (car predicate) parse-time-elt)
					    (or (not (cdr predicate))
						(<= parse-time-elt
						    (cadr predicate)))
					    parse-time-elt))
				      ((symbolp predicate)
				       (cdr (assoc parse-time-elt
						   (symbol-value predicate))))
				      ((funcall predicate)))))
		 (setq exit t)
		 (while slots
		   (let ((new-val (if rule
				      (let ((this (pop rule)))
					(if (vectorp this)
					    (cl-parse-integer
					     parse-time-elt
					     :start (aref this 0)
					     :end (aref this 1))
					  (funcall this)))
				    parse-time-val)))
		     (setf (nth (pop slots) time) new-val))))))))
       time))))

(defun parse-iso8601-time-string (date-string)
  "Parse an ISO 8601 time string, such as \"2020-01-15T16:12:21-08:00\".
Fall back on parsing something resembling an RFC 822 (or later) date-time.
This function is like `parse-time-string' except that it returns
a Lisp timestamp when successful."
  (when-let ((time (parse-time-string date-string)))
    (encode-time time)))

(provide 'parse-time)

;;; parse-time.el ends here
