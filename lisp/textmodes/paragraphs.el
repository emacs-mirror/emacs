;;; paragraphs.el --- paragraph and sentence parsing.

;; Copyright (C) 1985, 86, 87, 91, 94 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp

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

;; This package provides the paragraph-oriented commands documented in the
;; Emacs manual.

;;; Code:

(defconst paragraph-start "^[ \t\n\f]" "\
*Regexp for beginning of a line that starts OR separates paragraphs.
This regexp should match lines that separate paragraphs
and should also match lines that start a paragraph
\(and are part of that paragraph).

The variable `paragraph-separate' specifies how to distinguish
lines that start paragraphs from lines that separate them.

If the variable `use-hard-newlines' is nonnil, then only lines following a
hard newline are considered to match.")

(defconst paragraph-separate "^[ \t\f]*$" "\
*Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change paragraph-start also.

If the variable `use-hard-newlines' is nonnil, then only lines following a
hard newline are considered to match.")

(defconst sentence-end (purecopy "[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*") "\
*Regexp describing the end of a sentence.
All paragraph boundaries also end sentences, regardless.

In order to be recognized as the end of a sentence, the ending period,
question mark, or exclamation point must be followed by two spaces,
unless it's inside some sort of quotes or parenthesis.")

(defconst page-delimiter "^\014" "\
*Regexp describing line-beginnings that separate pages.")

(defvar paragraph-ignore-fill-prefix nil "\
Non-nil means the paragraph commands are not affected by `fill-prefix'.
This is desirable in modes where blank lines are the paragraph delimiters.")

(defsubst looking-at-hard (re)
  ;; Just for convenience in writing the function below.
  (and (or (null use-hard-newlines)
	   (bobp)
	   (get-text-property (1- (point)) 'hard))
       (looking-at re)))

(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (let* ((fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 (paragraph-separate
	  (if fill-prefix-regexp
	      (concat paragraph-separate "\\|^"
		      fill-prefix-regexp "[ \t]*$")
	    paragraph-separate)))
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at-hard paragraph-separate))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
	       (looking-at-hard paragraph-separate))
	  nil
	;; Move back over paragraph-separating lines.
	(forward-char -1) (beginning-of-line)
	(while (and (not (bobp))
		    (looking-at-hard paragraph-separate))
	  (forward-line -1))
	(if (bobp)
	    nil
	  ;; Go to end of the previous (non-separating) line.
	  (end-of-line)
	  ;; Search back for line that starts or separates paragraphs.
	  (if (if fill-prefix-regexp
		  ;; There is a fill prefix; it overrides paragraph-start.
		  (progn
		   (while (progn (beginning-of-line)
				 (and (not (bobp))
				      (not (looking-at-hard 
					    paragraph-separate))
				      (looking-at fill-prefix-regexp)))
		     (forward-line -1))
		   (not (bobp)))
		(while (and (re-search-backward paragraph-start nil 1)
			    use-hard-newlines
			    (not (bobp))
			    (null (get-text-property (1- (point)) 'hard)))
		  (if (not (bobp)) (backward-char 1)))
		(> (point) (point-min)))
	      ;; Found one.
	      (progn
		;; Move forward over paragraph separators.
		;; We know this cannot reach the place we started
		;; because we know we moved back over a non-separator.
		(while (and (not (eobp)) (looking-at-hard paragraph-separate))
		  (forward-line 1))
		(if (eq (char-after (- (point) 2)) ?\n)
		    (forward-line -1)))
	    ;; No starter or separator line => use buffer beg.
	    (goto-char (point-min)))))
      (setq arg (1+ arg)))
    (while (and (> arg 0) (not (eobp)))
      (beginning-of-line)
      (while (prog1 (and (not (eobp))
			 (looking-at-hard paragraph-separate))
	       (forward-line 1)))
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides paragraph-start.
	  (while (and (not (eobp))
		      (not (looking-at-hard paragraph-separate))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(while (and (re-search-forward paragraph-start nil 1)
		    (not (eobp))
		    use-hard-newlines
		    (null (get-text-property (1- (match-beginning 0)) 'hard)))
	  (forward-char 1))
	(if (< (point) (point-max))
	    (goto-char (match-beginning 0))))
      (setq arg (1- arg)))))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point."
  (interactive)
  (forward-paragraph 1)
  (push-mark nil t t)
  (backward-paragraph 1))

(defun kill-paragraph (arg)
  "Kill forward to end of paragraph.
With arg N, kill forward to Nth end of paragraph;
negative arg -N means kill backward to Nth start of paragraph."
  (interactive "p")
  (kill-region (point) (progn (forward-paragraph arg) (point))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph.
With arg N, kill back to Nth start of paragraph;
negative arg -N means kill forward to Nth end of paragraph."
  (interactive "p")
  (kill-region (point) (progn (backward-paragraph arg) (point))))

(defun transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((opoint (point)) npoint)
    (forward-paragraph -1)
    (setq npoint (point))
    (skip-chars-forward " \t\n")
    ;; If the range of blank lines found spans the original start point,
    ;; try again from the beginning of it.
    ;; Must be careful to avoid infinite loop
    ;; when following a single return at start of buffer.
    (if (and (>= (point) opoint) (< npoint opoint))
	(progn
	  (goto-char npoint)
	  (if (> npoint (point-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((opoint (point)))
    (forward-paragraph 1)
    (if (eq (preceding-char) ?\n) (forward-char -1))
    (if (<= (point) opoint)
	(progn
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next `sentence-end'.  With argument, repeat.
With negative argument, move backward repeatedly to `sentence-beginning'.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  Also, every paragraph boundary terminates sentences as well."
  (interactive "p")
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((par-beg (save-excursion (start-of-paragraph-text) (point))))
      (if (re-search-backward (concat sentence-end "[^ \t\n]") par-beg t)
	  (goto-char (1- (match-end 0)))
	(goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
      (if (re-search-forward sentence-end par-end t)
	  (skip-chars-backward " \t\n")
	(goto-char par-end)))
    (setq arg (1- arg))))

(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See `forward-sentence' for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from point to end of sentence.
With arg, repeat; negative arg -N means kill back to Nth start of sentence."
  (interactive "p")
  (kill-region (point) (progn (forward-sentence arg) (point))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from point to start of sentence.
With arg, repeat, or kill forward to Nth end of sentence if negative arg -N."
  (interactive "p")
  (kill-region (point) (progn (backward-sentence arg) (point))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in `forward-sentence'."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-sentence arg)
      (point))
    nil t))

(defun transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'forward-sentence arg))

;;; paragraphs.el ends here
