;;; format-spec.el --- functions for formatting arbitrary formatting strings

;; Copyright (C) 1999-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: tools

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

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defun format-spec (format specification &optional only-present)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"su - %u %k\".
SPECIFICATION is an alist mapping format specification characters
to their substitutions.

For instance:

  (format-spec \"su - %u %l\"
               \\=`((?u . ,(user-login-name))
                 (?l . \"ls\")))

Each %-spec may contain optional flag and width modifiers, as
follows:

  %<flags><width>character

The following flags are allowed:

* 0: Pad to the width, if given, with zeros instead of spaces.
* -: Pad to the width, if given, on the right instead of the left.
* <: Truncate to the width, if given, on the left.
* >: Truncate to the width, if given, on the right.
* ^: Convert to upper case.
* _: Convert to lower case.

The width modifier behaves like the corresponding one in `format'
when applied to %s.

For example, \"%<010b\" means \"substitute into the output the
value associated with ?b in SPECIFICATION, either padding it with
leading zeros or truncating leading characters until it's ten
characters wide\".

Any text properties of FORMAT are copied to the result, with any
text properties of a %-spec itself copied to its substitution.

ONLY-PRESENT indicates how to handle %-spec characters not
present in SPECIFICATION.  If it is nil or omitted, emit an
error; otherwise leave those %-specs and any occurrences of
\"%%\" in FORMAT verbatim in the result, including their text
properties, if any."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
        ;; Quoted percent sign.
        ((eq (char-after) ?%)
         (unless only-present
	   (delete-char 1)))
        ;; Valid format spec.
        ((looking-at "\\([-0 _^<>]*\\)\\([0-9.]*\\)\\([a-zA-Z]\\)")
	 (let* ((modifiers (match-string 1))
                (num (match-string 2))
	        (spec (string-to-char (match-string 3)))
	        (val (assq spec specification)))
	   (if (not val)
               (unless only-present
	         (error "Invalid format character: `%%%c'" spec))
	     (setq val (cdr val)
                   modifiers (format-spec--parse-modifiers modifiers))
	     ;; Pad result to desired length.
	     (let ((text (format "%s" val)))
               (when num
                 (setq num (string-to-number num))
                 (setq text (format-spec--pad text num modifiers))
                 (when (> (length text) num)
                   (cond
                    ((memq :chop-left modifiers)
                     (setq text (substring text (- (length text) num))))
                    ((memq :chop-right modifiers)
                     (setq text (substring text 0 num))))))
               (when (memq :uppercase modifiers)
                 (setq text (upcase text)))
               (when (memq :lowercase modifiers)
                 (setq text (downcase text)))
	       ;; Insert first, to preserve text properties.
	       (insert-and-inherit text)
	       ;; Delete the specifier body.
               (delete-region (+ (match-beginning 0) (length text))
                              (+ (match-end 0) (length text)))
               ;; Delete the percent sign.
               (delete-region (1- (match-beginning 0)) (match-beginning 0))))))
        ;; Signal an error on bogus format strings.
        (t
          (unless only-present
	    (error "Invalid format string")))))
    (buffer-string)))

(defun format-spec--pad (text total-length modifiers)
  (if (> (length text) total-length)
      ;; The text is longer than the specified length; do nothing.
      text
    (let ((padding (make-string (- total-length (length text))
                                (if (memq :zero-pad modifiers)
                                    ?0
                                  ?\s))))
      (if (memq :right-pad modifiers)
          (concat text padding)
        (concat padding text)))))

(defun format-spec--parse-modifiers (modifiers)
  (mapcan (lambda (char)
            (when-let ((modifier
                        (pcase char
                          (?0 :zero-pad)
                          (?\s :space-pad)
                          (?^ :uppercase)
                          (?_ :lowercase)
                          (?- :right-pad)
                          (?< :chop-left)
                          (?> :chop-right))))
              (list modifier)))
          modifiers))

(defun format-spec-make (&rest pairs)
  "Return an alist suitable for use in `format-spec' based on PAIRS.
PAIRS is a list where every other element is a character and a value,
starting with a character."
  (let (alist)
    (while pairs
      (unless (cdr pairs)
	(error "Invalid list of pairs"))
      (push (cons (car pairs) (cadr pairs)) alist)
      (setq pairs (cddr pairs)))
    (nreverse alist)))

(provide 'format-spec)

;;; format-spec.el ends here
