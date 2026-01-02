;;; ietf-drums-date.el --- parse time/date for ietf-drums.el -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Bob Rogers <rogers@rgrjr.com>
;; Package: ietf-drums
;; Keywords: mail, util

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

;; 'ietf-drums-parse-date-string' parses a time and/or date in a
;; string and returns a list of values, just like `decode-time', where
;; unspecified elements in the string are returned as nil (except
;; unspecified DST is returned as -1).  `encode-time' may be applied
;; on these values to obtain an internal time value.

;; Historically, `parse-time-string' was used for this purpose, but it
;; was gradually but imperfectly extended to handle other date
;; formats.  'ietf-drums-parse-date-string' is compatible in that it
;; uses the same return value format and parses the same email date
;; formats by default, but can be made stricter if desired.

;;; Code:

(require 'cl-lib)
(require 'parse-time)

(define-error 'date-parse-error "Date/time parse error" 'error)

(defconst ietf-drums-date--slot-names
  '(second minute hour day month year weekday dst zone)
  "Names of return value slots, for better error messages
See the decoded-time defstruct.")

(defconst ietf-drums-date--slot-ranges
  '((0 60) (0 59) (0 23) (1 31) (1 12) (1 9999))
  "Numeric slot ranges, for bounds checking.
Note that RFC5322 explicitly requires that seconds go up to 60,
to allow for leap seconds (see Mills, D., \"Network Time
Protocol\", STD 12, RFC 1119, September 1989).")

(defsubst ietf-drums-date--ignore-char-p (char)
  ;; Ignore whitespace and commas.
  (memq char '(?\s ?\t ?\r ?\n ?,)))

(defun ietf-drums-date--tokenize-string (string &optional comment-eof)
  "Turn STRING into tokens, separated only by whitespace and commas.
Multiple commas are ignored.  Pure digit sequences are turned
into integers.  If COMMENT-EOF is true, then a comment as
defined by RFC5322 (strictly, the CFWS production that also
accepts comments) is treated as an end-of-file, and no further
tokens are recognized, otherwise we strip out all comments and
treat them as whitespace (per RFC822)."
  (let ((index 0)
	(end (length string))
	(list ()))
    (cl-flet ((skip-ignored ()
                ;; Skip ignored characters at index (the scan
                ;; position).  Skip RFC822 comments in matched parens,
                ;; but do not complain about unterminated comments.
                (let ((char nil)
                      (nest 0))
                  (while (and (< index end)
                              (setq char (aref string index))
                              (or (> nest 0)
                                  (ietf-drums-date--ignore-char-p char)
                                  (and (not comment-eof) (eql char ?\())))
                    (incf index)
                    ;; FWS bookkeeping.
                    (cond ((and (eq char ?\\)
                                (< (1+ index) end))
	                    ;; Move to the next char but don't check
	                    ;; it to see if it might be a paren.
                            (incf index))
                          ((eq char ?\() (incf nest))
                          ((eq char ?\)) (decf nest)))))))
      (skip-ignored)		;; Skip leading whitespace.
      (while (and (< index end)
                  (not (and comment-eof
                            (eq (aref string index) ?\())))
        (let* ((start index)
               (char (aref string index))
               (all-digits (<= ?0 char ?9)))
          ;; char is valid; look for more valid characters.
          (when (and (eq char ?\\)
                     (< (1+ index) end))
            ;; Escaped character, which might be a "(".  If so, we are
            ;; correct to include it in the token, even though the
            ;; caller is sure to barf.  If not, we violate RFC2?822 by
            ;; not removing the backslash, but no characters in valid
            ;; RFC2?822 dates need escaping anyway, so it shouldn't
            ;; matter that this is not done strictly correctly.  --
            ;; rgr, 24-Dec-21.
            (incf index))
          (while (and (< (incf index) end)
                      (setq char (aref string index))
                      (not (or (ietf-drums-date--ignore-char-p char)
                               (eq char ?\())))
            (unless (<= ?0 char ?9)
              (setq all-digits nil))
            (when (and (eq char ?\\)
                       (< (1+ index) end))
              ;; Escaped character, see above.
              (incf index)))
          (push (if all-digits
                    (cl-parse-integer string :start start :end index)
                  (substring string start index))
                list)
          (skip-ignored)))
      (nreverse list))))

(defun ietf-drums-parse-date-string (time-string &optional error no-822)
  "Parse an RFC5322 or RFC822 date, passed as TIME-STRING.
The optional ERROR parameter causes syntax errors to be flagged
by signaling an instance of the date-parse-error condition.  The
optional NO-822 parameter disables the more lax RFC822 syntax,
which is permitted by default.

The result is a list of (SEC MIN HOUR DAY MON YEAR DOW DST TZ),
which can be accessed as a decoded-time defstruct (q.v.),
e.g. `decoded-time-year' to extract the year, and turned into an
Emacs timestamp by `encode-time'.

The strict syntax for RFC5322 is as follows:

   [ day-of-week \",\" ] day FWS month-name FWS year FWS time [CFWS]

where the \"time\" production is:

   2DIGIT \":\" 2DIGIT [ \":\" 2DIGIT ] FWS ( \"+\" / \"-\" ) 4DIGIT

and FWS is \"folding white space,\" and CFWS is \"comments and/or
folding white space\", where comments are included in nesting
parentheses and are equivalent to white space.  RFC822 also
accepts comments in random places (all of which is handled by
ietf-drums-date--tokenize-string) and two-digit years.  For
two-digit years, 50 and up are interpreted as 1950 through 1999
and 00 through 49 as 200 through 2049.

We are somewhat more lax in what we accept (specifically, the
hours don't have to be two digits, and the TZ and the comma after
the DOW are optional), but we do insist that the items that are
present do appear in this order.  Unspecified/unrecognized
elements in the string are returned as nil (except unspecified
DST is returned as -1)."
  (let ((tokens (ietf-drums-date--tokenize-string (downcase time-string)
                                                  no-822))
        (time (list nil nil nil nil nil nil nil -1 nil)))
    (cl-labels ((set-matched-slot (slot index token)
                  ;; Assign a slot value from match data if index is
                  ;; non-nil, else from token, signaling an error if
                  ;; enabled and it's out of range.
                  (let ((value (if index
                                   (cl-parse-integer (match-string index token))
                                 token)))
                    (when error
                      (let ((range (nth slot ietf-drums-date--slot-ranges)))
                        (when (and range
                                   (not (<= (car range) value (cadr range))))
                          (signal 'date-parse-error
                                  (list "Slot out of range"
                                        (nth slot ietf-drums-date--slot-names)
                                        token (car range) (cadr range))))))
                    (setf (nth slot time) value)))
                (set-numeric (slot token)
                  ;; Only assign the slot if the token is a number.
                  (cond ((natnump token)
                          (set-matched-slot slot nil token))
                        (error
                          (signal 'date-parse-error
                                  (list "Not a number"
                                        (nth slot ietf-drums-date--slot-names)
                                        token))))))
      ;; Check for weekday.
      (let ((dow (assoc (car tokens) parse-time-weekdays)))
        (when dow
          ;; Day of the week.
          (set-matched-slot 6 nil (cdr dow))
          (pop tokens)))
      ;; Day.
      (set-numeric 3 (pop tokens))
      ;; Alphabetic month.
      (let* ((month (pop tokens))
             (match (assoc month parse-time-months)))
        (cond (match
                (set-matched-slot 4 nil (cdr match)))
              (error
                (signal 'date-parse-error
                        (list "Expected an alphabetic month" month)))
              (t
                (push month tokens))))
      ;; Year.
      (let ((year (pop tokens)))
        ;; Check the year for the right number of digits.
        (cond ((not (natnump year))
                (when error
                  (signal 'date-parse-error
                          (list "Expected a year" year)))
                (push year tokens))
              ((>= year 1000)
                (set-numeric 5 year))
              ((or no-822
                   (>= year 100))
                (when error
                  (signal 'date-parse-error
                          (list "Four-digit years are required" year)))
                (push year tokens))
              ((>= year 50)
                ;; second half of the 20th century.
                (set-numeric 5 (+ 1900 year)))
              (t
                ;; first half of the 21st century.
                (set-numeric 5 (+ 2000 year)))))
      ;; Time.
      (let ((time (pop tokens)))
        (cond ((or (null time) (natnump time))
                (when error
                  (signal 'date-parse-error
                          (list "Expected a time" time)))
                (push time tokens))
              ((string-match
                "^\\([0-9][0-9]?\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$"
                time)
                (set-matched-slot 2 1 time)
                (set-matched-slot 1 2 time)
                (set-matched-slot 0 3 time))
              ((string-match "^\\([0-9][0-9]?\\):\\([0-9][0-9]\\)$" time)
                ;; Time without seconds.
                (set-matched-slot 2 1 time)
                (set-matched-slot 1 2 time)
                (set-matched-slot 0 nil 0))
              (error
                (signal 'date-parse-error
                        (list "Expected a time" time)))))
      ;; Timezone.
      (let* ((zone (pop tokens))
             (match (assoc zone parse-time-zoneinfo)))
        (cond (match
                (set-matched-slot 8 nil (cadr match))
                (set-matched-slot 7 nil (caddr match)))
              ((and (stringp zone)
                    (string-match "^[-+][0-9][0-9][0-9][0-9]$" zone))
                ;; Numeric time zone.
                (set-matched-slot
                  8 nil
                  (* 60
                     (+ (cl-parse-integer zone :start 3 :end 5)
                        (* 60 (cl-parse-integer zone :start 1 :end 3)))
                     (if (= (aref zone 0) ?-) -1 1))))
              ((and zone error)
                (signal 'date-parse-error
                        (list "Expected a timezone" zone)))))
      (when (and tokens error)
        (signal 'date-parse-error
                (list "Extra token(s)" (car tokens)))))
    time))

(provide 'ietf-drums-date)

;;; ietf-drums-date.el ends here
