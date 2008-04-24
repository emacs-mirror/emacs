;;; format-spec.el --- functions for formatting arbitrary formatting strings

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; Code:

(eval-when-compile (require 'cl))

(defun format-spec (format specification)
  "Return a string based on FORMAT and SPECIFICATION.
FORMAT is a string containing `format'-like specs like \"bash %u %k\",
while SPECIFICATION is an alist mapping from format spec characters
to values.  Any text properties on a %-spec itself are propagated to
the text that it generates."
  (with-temp-buffer
    (insert format)
    (goto-char (point-min))
    (while (search-forward "%" nil t)
      (cond
       ;; Quoted percent sign.
       ((eq (char-after) ?%)
	(delete-char 1))
       ;; Valid format spec.
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
	(let* ((num (match-string 1))
	       (spec (string-to-char (match-string 2)))
	       (val (cdr (assq spec specification))))
	  (unless val
	    (error "Invalid format character: `%%%c'" spec))
	  ;; Pad result to desired length.
          (let ((text (format (concat "%" num "s") val)))
	    ;; Insert first, to preserve text properties.
            (insert-and-inherit text)
            ;; Delete the specifier body.
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            ;; Delete the percent sign.
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))
       ;; Signal an error on bogus format strings.
       (t
	(error "Invalid format string"))))
    (buffer-string)))

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

;; arch-tag: c22d49cf-d167-445d-b7f1-2504d4173f53
;;; format-spec.el ends here
