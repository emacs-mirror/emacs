;;; fill.el --- fill commands for Emacs

;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

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

;; All the commands for filling text.  These are documented in the Emacs
;; manual.

;;; Code:

(defconst fill-individual-varying-indent nil
  "*Controls criterion for a new paragraph in `fill-individual-paragraphs'.
Non-nil means changing indent doesn't end a paragraph.
That mode can handle paragraphs with extra indentation on the first line,
but it requires separator lines between paragraphs.
A value of nil means that any change in indentation starts a new paragraph.")

(defconst sentence-end-double-space t
  "*Non-nil means a single space does not end a sentence.")

(defun set-fill-prefix ()
  "Set the fill prefix to the current line up to point.
Filling expects lines to start with the fill prefix and
reinserts the fill prefix in each resulting line."
  (interactive)
  (setq fill-prefix (buffer-substring
		     (save-excursion (beginning-of-line) (point))
		     (point)))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix cancelled")))

(defconst adaptive-fill-mode t
  "*Non-nil means determine a paragraph's fill prefix from its text.")

(defconst adaptive-fill-regexp "[ \t]*\\([>*] +\\)?"
  "*Regexp to match text at start of line that constitutes indentation.
If Adaptive Fill mode is enabled, whatever text matches this pattern
on the second line of a paragraph is used as the standard indentation
for the paragraph.")

(defun fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit `fill-column'.
Any paragraph breaks in the region will be removed.
Prefix arg means justify too.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
  ;; Arrange for undoing the fill to restore point.
  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
      (setq buffer-undo-list (cons (point) buffer-undo-list)))
  ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
  (let ((fill-prefix fill-prefix))
    ;; Figure out how this paragraph is indented, if desired.
    (if (and adaptive-fill-mode
	     (or (null fill-prefix) (string= fill-prefix "")))
	(save-excursion
	  (goto-char (min from to))
	  (if (eolp) (forward-line 1))
	  (forward-line 1)
	  (if (< (point) (max from to))
	      (let ((start (point)))
		(re-search-forward adaptive-fill-regexp)
		(setq fill-prefix (buffer-substring start (point))))
	    (goto-char (min from to))
	    (if (eolp) (forward-line 1))
	    ;; If paragraph has only one line, don't assume in general
	    ;; that additional lines would have the same starting
	    ;; decoration.  Assume no indentation.
	    ;; But if left-margin is nonzero, we can assume ordinary
	    ;; lines do have indentation.
	    (if (> left-margin 0)
		(progn
		  (re-search-forward adaptive-fill-regexp)
		  (setq fill-prefix (make-string (current-column) ?\ ))))
	    )))

    (save-restriction
      (let (beg)
	(goto-char (min from to))
	(skip-chars-forward "\n")
	(setq beg (point))
	(goto-char (max from to))
	(skip-chars-backward "\n")
	(setq to (point)
	      from beg)
	(goto-char from)
	(beginning-of-line)
	(narrow-to-region (point) to))
      (if use-hard-newlines
	  (remove-text-properties from to '(hard nil)))
      (if (> from (point))
	  (goto-char from))
      (let ((fpre (and fill-prefix (not (equal fill-prefix ""))
		       (regexp-quote fill-prefix))))
	;; Delete the fill prefix from every line except the first.
	;; The first line may not even have a fill prefix.
	(and fpre
	     (progn
	       (if (>= (length fill-prefix) fill-column)
		   (error "fill-prefix too long for specified width"))
	       (goto-char from)
	       (forward-line 1)
	       (while (not (eobp))
		 (if (looking-at fpre)
		     (delete-region (point) (match-end 0)))
		 (forward-line 1))
	       (goto-char from)
	       (and (looking-at fpre) (forward-char (length fill-prefix)))
	       (setq from (point)))))
      ;; from is now before the text to fill,
      ;; but after any fill prefix on the first line.

      ;; Make sure sentences ending at end of line get an extra space.
      ;; loses on split abbrevs ("Mr.\nSmith")
      (goto-char from)
      (while (re-search-forward "[.?!][])}\"']*$" nil t)
	(insert-and-inherit ? ))

      ;; Then change all newlines to spaces.
      (subst-char-in-region from (point-max) ?\n ?\ )

      ;; Flush excess spaces, except in the paragraph indentation.
      (goto-char from)
      (skip-chars-forward " \t")
      ;; Nuke tabs while we're at it; they get screwed up in a fill.
      ;; This is quick, but loses when a tab follows the end of a sentence.
      ;; Actually, it is difficult to tell that from "Mr.\tSmith".
      ;; Blame the typist.
      (subst-char-in-region (point) (point-max) ?\t ?\ )
      (while (re-search-forward "   *" nil t)
	(delete-region
	 (+ (match-beginning 0)
	    (if (and sentence-end-double-space
		     (save-excursion
		       (skip-chars-backward " ]})\"'")
		       (memq (preceding-char) '(?. ?? ?!))))
		2 1))
	 (match-end 0)))
      (goto-char (point-max))
      (delete-horizontal-space)
      (insert-and-inherit " ")
      (goto-char (point-min))

      ;; This is the actual filling loop.
      (let ((prefixcol 0) linebeg)
	(while (not (eobp))
	  (setq linebeg (point))
	  (move-to-column (1+ fill-column))
	  (if (eobp)
	      (delete-horizontal-space)
	    ;; Move back to start of word.
	    (skip-chars-backward "^ \n" linebeg)
	    ;; Don't break after a period followed by just one space.
	    ;; Move back to the previous place to break.
	    ;; The reason is that if a period ends up at the end of a line,
	    ;; further fills will assume it ends a sentence.
	    ;; If we now know it does not end a sentence,
	    ;; avoid putting it at the end of the line.
	    (if sentence-end-double-space
		(while (and (> (point) (+ linebeg 2))
			    (eq (preceding-char) ?\ )
			    (not (eq (following-char) ?\ ))
			    (eq (char-after (- (point) 2)) ?\.))
		  (forward-char -2)
		  (skip-chars-backward "^ \n" linebeg)))
	    (if (if (zerop prefixcol)
		    (save-excursion
		      (skip-chars-backward " " linebeg)
		      (bolp))
		  (>= prefixcol (current-column)))
		;; Keep at least one word even if fill prefix exceeds margin.
		;; This handles all but the first line of the paragraph.
		;; Meanwhile, don't stop at a period followed by one space.
		(let ((first t))
		  (move-to-column prefixcol)
		  (while (and (not (eobp))
			      (or first
				  (and (not (bobp))
				       sentence-end-double-space
				       (save-excursion (forward-char -1)
						       (and (looking-at "\\. ")
							    (not (looking-at "\\.  ")))))))
		    (skip-chars-forward " ")
		    (skip-chars-forward "^ \n")
		    (setq first nil)))
	      ;; Normally, move back over the single space between the words.
	      (forward-char -1))
	    (if (and fill-prefix (zerop prefixcol)
		     (< (- (point) (point-min)) (length fill-prefix))
		     (string= (buffer-substring (point-min) (point))
			      (substring fill-prefix 0 (- (point) (point-min)))))
		;; Keep at least one word even if fill prefix exceeds margin.
		;; This handles the first line of the paragraph.
		;; Don't stop at a period followed by just one space.
		(let ((first t))
		  (while (and (not (eobp))
			      (or first
				  (and (not (bobp))
				       sentence-end-double-space
				       (save-excursion (forward-char -1)
						       (and (looking-at "\\. ")
							    (not (looking-at "\\.  ")))))))
		    (skip-chars-forward " ")
		    (skip-chars-forward "^ \n")
		    (setq first nil))))
	    ;; Replace all whitespace here with one newline.
	    ;; Insert before deleting, so we don't forget which side of
	    ;; the whitespace point or markers used to be on.
	    (skip-chars-backward " ")
	    (insert ?\n)
	    ;; Give newline the properties of the space(s) it replaces
	    (set-text-properties (1- (point)) (point)
				 (text-properties-at (point)))
	    (delete-horizontal-space)
	    ;; Insert the fill prefix at start of each line.
	    ;; Set prefixcol so whitespace in the prefix won't get lost.
	    (and fill-prefix (not (equal fill-prefix ""))
		 (progn
		   (insert-and-inherit fill-prefix)
		   (setq prefixcol (current-column)))))
	  ;; Justify the line just ended, if desired.
	  (and justify-flag (not (eobp))
	       (progn
		 (forward-line -1)
		 (justify-current-line)
		 (forward-line 1))))))))

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.  Prefix arg means justify as well.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "P")
  (let ((before (point)))
    (save-excursion
      (forward-paragraph)
      (or (bolp) (newline 1))
      (let ((end (point))
	    (beg (progn (backward-paragraph) (point))))
	(goto-char before)
	(if use-hard-newlines
	    ;; Can't use fill-region-as-paragraph, since this paragraph may
	    ;; still contain hard newlines.  See fill-region.
	    (fill-region beg end arg)
	  (fill-region-as-paragraph beg end arg))))))

(defun fill-region (from to &optional justify-flag)
  "Fill each of the paragraphs in the region.
Prefix arg (non-nil third arg, if called from program) means justify as well.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "r\nP")
  ;; If using hard newlines, break at every one for filling purposes rather
  ;; than breaking at normal paragraph breaks.
  (let ((paragraph-start (if use-hard-newlines "^" paragraph-start))
 	end beg)
    (save-restriction
      (goto-char (max from to))
      (setq end (point))
      (goto-char (setq beg (min from to)))
      (beginning-of-line)
      (narrow-to-region (point) end)
      (while (not (eobp))
	(let ((initial (point))
	      (end (progn
		     (forward-paragraph 1) (point))))
	  (forward-paragraph -1)
	  (if (< (point) beg)
	      (goto-char beg))
	  (if (>= (point) initial)
	      (fill-region-as-paragraph (point) end justify-flag)
	    (goto-char end)))))))

(defun justify-current-line ()
  "Add spaces to line point is in, so it ends at `fill-column'."
  (interactive)
  (save-excursion
   (save-restriction
    (let (ncols beg indent end)
      (beginning-of-line)
      (forward-char (length fill-prefix))
      (skip-chars-forward " \t")
      (setq indent (current-column))
      (setq beg (point))
      (end-of-line)
      (narrow-to-region beg (point))
      (setq end (point))
      (skip-chars-backward " \t")
      (delete-char (- end (point)))
      (goto-char beg)
      (while (re-search-forward "   *" nil t)
	(delete-region
	 (+ (match-beginning 0)
	    (if (save-excursion
		 (skip-chars-backward " ])\"'")
		 (memq (preceding-char) '(?. ?? ?!)))
		2 1))
	 (match-end 0)))
      (goto-char beg)
      (while (re-search-forward "[.?!][])\"']*\n" nil t)
	(forward-char -1)
	(insert-and-inherit ? ))
      (goto-char (point-max))
      ;; Note that the buffer bounds start after the indentation,
      ;; so the columns counted by INDENT don't appear in (current-column).
      (setq ncols (- fill-column (current-column) indent))
      (if (search-backward " " nil t)
	  (while (> ncols 0)
	    (let ((nmove (+ 3 (random 3))))
	      (while (> nmove 0)
		(or (search-backward " " nil t)
		    (progn
		     (goto-char (point-max))
		     (search-backward " ")))
		(skip-chars-backward " ")
		(setq nmove (1- nmove))))
	    (insert-and-inherit " ")
	    (skip-chars-backward " ")
	    (setq ncols (1- ncols)))))))
  nil)


(defun fill-nonuniform-paragraphs (min max &optional justifyp mailp)
  "Fill paragraphs within the region, allowing varying indentation within each.
This command divides the region into \"paragraphs\",
only at paragraph-separator lines, then fills each paragraph
using as the fill prefix the smallest indentation of any line
in the paragraph.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY-FLAG and MAIL-FLAG:
JUSTIFY-FLAG to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive "r\nP")
  (let ((fill-individual-varying-indent t))
    (fill-individual-paragraphs min max justifyp mailp)))

(defun fill-individual-paragraphs (min max &optional justifyp mailp)
  "Fill paragraphs of uniform indentation within the region.
This command divides the region into \"paragraphs\", 
treating every change in indentation level as a paragraph boundary,
then fills each paragraph using its indentation level as the fill prefix.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY-FLAG and MAIL-FLAG:
JUSTIFY-FLAG to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive "r\nP")
  (save-restriction
    (save-excursion
      (goto-char min)
      (beginning-of-line)
      (narrow-to-region (point) max)
      (if mailp 
	  (while (and (not (eobp))
		      (or (looking-at "[ \t]*[^ \t\n]*:")
			  (looking-at "[ \t]*$")))
	    (if (looking-at "[ \t]*[^ \t\n]*:")
		(search-forward "\n\n" nil 'move)
	      (forward-line 1))))
      (narrow-to-region (point) max)
      ;; Loop over paragraphs.
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
	(beginning-of-line)
	(let ((start (point))
	      fill-prefix fill-prefix-regexp)
	  ;; Find end of paragraph, and compute the smallest fill-prefix
	  ;; that fits all the lines in this paragraph.
	  (while (progn
		   ;; Update the fill-prefix on the first line
		   ;; and whenever the prefix good so far is too long.
		   (if (not (and fill-prefix
				 (looking-at fill-prefix-regexp)))
		       (setq fill-prefix
			     (buffer-substring (point)
					       (save-excursion (skip-chars-forward " \t") (point)))
			     fill-prefix-regexp
			     (regexp-quote fill-prefix)))
		   (forward-line 1)
		   ;; Now stop the loop if end of paragraph.
		   (and (not (eobp))
			(if fill-individual-varying-indent
			    ;; If this line is a separator line, with or
			    ;; without prefix, end the paragraph.
			    (and 
			(not (looking-at paragraph-separate))
			(save-excursion
			  (not (and (looking-at fill-prefix-regexp)
				    (progn (forward-char (length fill-prefix))
						(looking-at paragraph-separate))))))
			  ;; If this line has more or less indent
			  ;; than the fill prefix wants, end the paragraph.
			  (and (looking-at fill-prefix-regexp)
			       (save-excursion
				 (not (progn (forward-char (length fill-prefix))
					     (or (looking-at paragraph-separate)
						 (looking-at paragraph-start))))))))))
	  ;; Fill this paragraph, but don't add a newline at the end.
	  (let ((had-newline (bolp)))
	    (fill-region-as-paragraph start (point) justifyp)
	    (or had-newline (delete-char -1))))))))

;;; fill.el ends here
