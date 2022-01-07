;;; format-spec.el --- format arbitrary formatting strings -*- lexical-binding: t -*-

;; Copyright (C) 1999-2022 Free Software Foundation, Inc.

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

;;;###autoload
(defun format-spec (format specification &optional ignore-missing split)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"su - %u %k\".
SPECIFICATION is an alist mapping format specification characters
to their substitutions.

For instance:

  (format-spec \"su - %u %l\"
               \\=`((?u . ,(user-login-name))
                 (?l . \"ls\")))

Each %-spec may contain optional flag, width, and precision
modifiers, as follows:

  %<flags><width><precision>character

The following flags are allowed:

* 0: Pad to the width, if given, with zeros instead of spaces.
* -: Pad to the width, if given, on the right instead of the left.
* <: Truncate to the width and precision, if given, on the left.
* >: Truncate to the width and precision, if given, on the right.
* ^: Convert to upper case.
* _: Convert to lower case.

The width and truncation modifiers behave like the corresponding
ones in `format' when applied to %s.

For example, \"%<010b\" means \"substitute into the output the
value associated with ?b in SPECIFICATION, either padding it with
leading zeros or truncating leading characters until it's ten
characters wide\".

Any text properties of FORMAT are copied to the result, with any
text properties of a %-spec itself copied to its substitution.

IGNORE-MISSING indicates how to handle %-spec characters not
present in SPECIFICATION.  If it is nil or omitted, emit an
error; if it is the symbol `ignore', leave those %-specs verbatim
in the result, including their text properties, if any; if it is
the symbol `delete', remove those %-specs from the result;
otherwise do the same as for the symbol `ignore', but also leave
any occurrences of \"%%\" in FORMAT verbatim in the result.

If SPLIT, instead of returning a single string, a list of strings
is returned, where each format spec is its own element."
  (with-temp-buffer
    (let ((split-start (point-min))
          (split-result nil))
      (insert format)
      (goto-char (point-min))
      (while (search-forward "%" nil t)
        (cond
         ;; Quoted percent sign.
         ((= (following-char) ?%)
          (when (memq ignore-missing '(nil ignore delete))
            (delete-char 1)))
         ;; Valid format spec.
         ((looking-at (rx (? (group (+ (in " 0<>^_-"))))
                          (? (group (+ digit)))
                          (? (group ?. (+ digit)))
                          (group alpha)))
          (let* ((beg (point))
                 (end (match-end 0))
                 (flags (match-string 1))
                 (width (match-string 2))
                 (trunc (match-string 3))
                 (char (string-to-char (match-string 4)))
                 (text (assq char specification)))
            (when (and split
                       (not (= (1- beg) split-start)))
              (push (buffer-substring split-start (1- beg)) split-result))
            (cond (text
                   ;; Handle flags.
                   (setq text (format-spec--do-flags
                               (format "%s" (cdr text))
                               (format-spec--parse-flags flags)
                               (and width (string-to-number width))
                               (and trunc (car (read-from-string trunc 1)))))
                   ;; Insert first, to preserve text properties.
                   (insert-and-inherit text)
                   ;; Delete the specifier body.
                   (delete-region (point) (+ end (length text)))
                   ;; Delete the percent sign.
                   (delete-region (1- beg) beg))
                  ((eq ignore-missing 'delete)
                   ;; Delete the whole format spec.
                   (delete-region (1- beg) end))
                  ((not ignore-missing)
                   (error "Invalid format character: `%%%c'" char)))
            (when split
              (push (buffer-substring (1- beg) (point)) split-result)
              (setq split-start (point)))))
         ;; Signal an error on bogus format strings.
         ((not ignore-missing)
          (error "Invalid format string"))))
      (if (not split)
          (buffer-string)
        (unless (= split-start (point-max))
          (push (buffer-substring split-start (point-max)) split-result))
        (nreverse split-result)))))

(defun format-spec--do-flags (str flags width trunc)
  "Return STR formatted according to FLAGS, WIDTH, and TRUNC.
FLAGS is a list of keywords as returned by
`format-spec--parse-flags'.  WIDTH and TRUNC are either nil or
string widths corresponding to `format-spec' modifiers."
  (let (diff str-width)
    ;; Truncate original string first, like `format' does.
    (when trunc
      (setq str-width (string-width str))
      (when (> (setq diff (- str-width trunc)) 0)
        (setq str (if (memq :chop-left flags)
                      (truncate-string-to-width str str-width diff)
                    (format (format "%%.%ds" trunc) str))
              ;; We know the new width so save it for later.
              str-width trunc)))
    ;; Pad or chop to width.
    (when width
      (setq str-width (or str-width (string-width str))
            diff (- width str-width))
      (cond ((zerop diff))
            ((> diff 0)
             (let ((pad (make-string diff (if (memq :pad-zero flags) ?0 ?\s))))
               (setq str (if (memq :pad-right flags)
                             (concat str pad)
                           (concat pad str)))))
            ((memq :chop-left flags)
             (setq str (truncate-string-to-width str str-width (- diff))))
            ((memq :chop-right flags)
             (setq str (format (format "%%.%ds" width) str))))))
  ;; Fiddle case.
  (cond ((memq :upcase flags)
         (upcase str))
        ((memq :downcase flags)
         (downcase str))
        (str)))

(defun format-spec--parse-flags (flags)
  "Convert sequence of FLAGS to list of human-readable keywords."
  (mapcan (lambda (char)
            (pcase char
              (?0 (list :pad-zero))
              (?- (list :pad-right))
              (?< (list :chop-left))
              (?> (list :chop-right))
              (?^ (list :upcase))
              (?_ (list :downcase))))
          flags))

(defun format-spec-make (&rest pairs)
  "Return an alist suitable for use in `format-spec' based on PAIRS.
PAIRS is a property list with characters as keys."
  (let (alist)
    (while pairs
      (unless (cdr pairs)
	(error "Invalid list of pairs"))
      (push (cons (car pairs) (cadr pairs)) alist)
      (setq pairs (cddr pairs)))
    (nreverse alist)))

(provide 'format-spec)

;;; format-spec.el ends here
