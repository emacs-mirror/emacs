;;; page.el --- page motion commands for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1985, 2001-2023 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: wp convenience
;; Package: emacs

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

;; This code provides the page-oriented movement and selection commands
;; documented in the Emacs manual.

;;; Code:

(defun forward-page (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp
`page-delimiter'."
  (interactive "p")
  (or count (setq count 1))
  (while (and (> count 0) (not (eobp)))
    (if (and (looking-at page-delimiter)
             (> (match-end 0) (point)))
        ;; If we're standing at the page delimiter, then just skip to
        ;; the end of it.  (But only if it's not a zero-length
        ;; delimiter, because then we wouldn't have forward progress.)
        (goto-char (match-end 0))
      ;; In case the page-delimiter matches the null string,
      ;; don't find a match without moving.
      (when (bolp)
        (forward-char 1))
      (unless (re-search-forward page-delimiter nil t)
        (goto-char (point-max))))
    (setq count (1- count)))
  (while (and (< count 0) (not (bobp)))
    ;; In case the page-delimiter matches the null string,
    ;; don't find a match without moving.
    (and (save-excursion (re-search-backward page-delimiter nil t))
	 (= (match-end 0) (point))
	 (goto-char (match-beginning 0)))
    (unless (bobp)
      (forward-char -1)
      (if (re-search-backward page-delimiter nil t)
	  ;; We found one--move to the end of it.
	  (goto-char (match-end 0))
	;; We found nothing--go to beg of buffer.
	(goto-char (point-min))))
    (setq count (1+ count))))

(defun backward-page (&optional count)
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp
`page-delimiter'."
  (interactive "p")
  (or count (setq count 1))
  (forward-page (- count)))

(defun mark-page (&optional arg)
  "Put mark at end of page, point at beginning.
A numeric arg specifies to move forward or backward by that many pages,
thus marking a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (if (> arg 0)
      (forward-page arg)
    (if (< arg 0)
        (forward-page (1- arg))))
  (forward-page)
  (push-mark nil t t)
  (forward-page -1))

(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (widen)
    (if (> arg 0)
	(forward-page arg)
      (if (< arg 0)
	  (let ((adjust 0)
		(opoint (point)))
	    ;; If we are not now at the beginning of a page,
	    ;; move back one extra time, to get to the start of this page.
	    (save-excursion
	      (beginning-of-line)
	      (or (and (looking-at page-delimiter)
		       (eq (match-end 0) opoint))
		  (setq adjust 1)))
	    (forward-page (- arg adjust)))))
    ;; Find the end of the page.
    (set-match-data nil)
    (forward-page)
    ;; If we stopped due to end of buffer, stay there.
    ;; If we stopped after a page delimiter, put end of restriction
    ;; at the beginning of that line.
    ;; Before checking the match that was found,
    ;; verify that forward-page actually set the match data.
    (if (and (match-beginning 0)
	     (save-excursion
	       (goto-char (match-beginning 0)) ; was (beginning-of-line)
	       (looking-at page-delimiter)))
	(goto-char (match-beginning 0))) ; was (beginning-of-line)
    (narrow-to-region (point)
		      (progn
			;; Find the top of the page.
			(forward-page -1)
			;; If we found beginning of buffer, stay there.
			;; If extra text follows page delimiter on same line,
			;; include it.
			;; Otherwise, show text starting with following line.
			(if (and (eolp) (not (bobp)))
			    (forward-line 1))
			(point)))))
(put 'narrow-to-page 'disabled t)

(defun page--count-lines-page ()
  "Return a list of line counts on the current page.
The list is on the form (TOTAL BEFORE AFTER), where TOTAL is the
total number of lines on the current page, while BEFORE and AFTER
are the number of lines on the current page before and after
point, respectively."
  (save-excursion
    (let ((opoint (point)))
      (forward-page)
      (beginning-of-line)
      (unless (looking-at page-delimiter)
        (end-of-line))
      (let ((end (point)))
        (backward-page)
        (list (count-lines (point) end)
              (count-lines (point) opoint)
              (count-lines opoint end))))))

(defun count-lines-page ()
  "Report number of lines on current page, and how many are before or after point."
  (interactive)
  (pcase-let ((`(,total ,before ,after) (page--count-lines-page)))
    (message (ngettext "Page has %d line (%d + %d)"
                       "Page has %d lines (%d + %d)" total)
             total before after)))

(defun page--what-page ()
  "Return a list of the page and line number of point."
  (save-restriction
    (widen)
    (save-excursion
      (let ((count 1)
            (opoint (point)))
        (goto-char (point-min))
        (while (re-search-forward page-delimiter opoint t)
          (when (= (match-beginning 0) (match-end 0))
            (forward-char))
          (setq count (1+ count)))
        (list count (line-number-at-pos opoint))))))

(defun what-page ()
  "Print page and line number of point."
  (interactive)
  (apply #'message (cons "Page %d, line %d" (page--what-page))))

(provide 'page)

;;; page.el ends here
