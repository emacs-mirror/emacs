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

(defun current-fill-column ()
  "Return the fill-column to use for this line.
The fill-column to use for a buffer is stored in the variable `fill-column',
but can be locally modified by the `right-margin' text property, which is
subtracted from `fill-column'.

The fill column to use for a line is the first column at which the column
number equals or exceeds the local fill-column - right-margin difference."
  (save-excursion
    (let* ((here (progn (beginning-of-line) (point)))
	   (here-col 0)
	   (eol (progn (end-of-line) (point)))
	   margin fill-col change col)
      ;; Look separately at each region of line with a different right-margin
      (while (and (setq margin (get-text-property here 'right-margin)
			fill-col (- fill-column (or margin 0))
			change (text-property-not-all here eol 
						      'right-margin margin))
		  (progn (goto-char (1- change))
			 (setq col (current-column))
			 (< col fill-col)))
	(setq here change
	      here-col col))
      (max here-col fill-col))))

(defun canonically-space-region (beg end)
  "Remove extra spaces between words in region.
Puts one space between words in region; two between sentences.
Remove indenation from each line."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    ;; Nuke tabs; they get screwed up in a fill.
    ;; This is quick, but loses when a tab follows the end of a sentence.
    ;; Actually, it is difficult to tell that from "Mr.\tSmith".
    ;; Blame the typist.
    (subst-char-in-region beg end ?\t ?\ )
    (while (and (< (point) end)
		(re-search-forward "   *" end t))
      (delete-region
       (+ (match-beginning 0)
	  ;; Determine number of spaces to leave:
	  (save-excursion
	    (skip-chars-backward " ]})\"'")
	    (cond ((and sentence-end-double-space
			(memq (preceding-char) '(?. ?? ?!)))  2)
		  ((char-equal (preceding-char) ?\n)  0)
		  (t 1))))
       (match-end 0)))
    ;; Make sure sentences ending at end of line get an extra space.
    ;; loses on split abbrevs ("Mr.\nSmith")
    (goto-char beg)
    (while (and (< (point) end)
		(re-search-forward "[.?!][])}\"']*$" end t))
      (insert-and-inherit ? ))))

(defun fill-region-as-paragraph (from to &optional justify nosqueeze)
  "Fill region as one paragraph: break lines to fit `fill-column'.
This removes any paragraph breaks in the region.
It performs justification according to the `justification' text-property,
but a prefix arg can be used to override this and request full justification.

Optional fourth arg NOSQUEEZE non-nil means to leave whitespace other than line
breaks untouched.  Normally it is made canonical before filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "r\nP")
  ;; Arrange for undoing the fill to restore point.
  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
      (setq buffer-undo-list (cons (point) buffer-undo-list)))
  (or justify (setq justify (current-justification)))

  ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
  (let ((fill-prefix fill-prefix)
	(skip-after 0))
    ;; Figure out how this paragraph is indented, if desired.
    (if (and adaptive-fill-mode
	     (or (null fill-prefix) (string= fill-prefix "")))
	(save-excursion
	  (goto-char (min from to))
	  (if (eolp) (forward-line 1))
	  (forward-line 1)
	  (move-to-left-margin)
	  (if (< (point) (max from to))
	      (let ((start (point)))
		(re-search-forward adaptive-fill-regexp)
		(setq fill-prefix (buffer-substring start (point)))
		(set-text-properties 0 (length fill-prefix) nil fill-prefix))
	    (goto-char (min from to))
	    (if (eolp) (forward-line 1))
	    ;; If paragraph has only one line, don't assume in general
	    ;; that additional lines would have the same starting
	    ;; decoration.  Assume no indentation.
	    )))

    (if (not justify)  ; filling disabled: just check indentation
	(progn
	  (goto-char (min from to))
	  (setq to (max from to))
	  (while (< (point) to)
	    (if (not (eolp))
		(if (< (current-indentation) (current-left-margin))
		    (indent-to-left-margin)))
	    (forward-line 1)))

      (save-restriction
	(let (beg)
	  (goto-char (min from to))
	  (skip-chars-forward "\n")
	  (setq beg (point))
	  (goto-char (max from to))
	  (skip-chars-backward "\n")
	  (setq skip-after (- to (point)))
	  ;; If we omit some final newlines from the end of the narrowing,
	  ;; arrange to advance past them at the end.
	  (setq to (point)
		from beg)
	  (goto-char from)
	  (beginning-of-line)
	  (narrow-to-region (point) to))
	(if use-hard-newlines
	    (remove-text-properties from to '(hard nil)))
	;; Make sure first line is indented (at least) to left margin...
	(if (or (memq justify '(right center))
		(< (current-indentation) (current-left-margin)))
	    (indent-to-left-margin))
	;; and remove indentation from other lines.
	(beginning-of-line 2)
	(indent-region (point) (point-max) 0)
	;; Delete the fill prefix from every line except the first.
	;; The first line may not even have a fill prefix.
	(goto-char from)
	(let ((fpre (and fill-prefix (not (equal fill-prefix ""))
			 (concat "[ \t]*"
				 (regexp-quote fill-prefix)))))
	  (and fpre
	       (progn
		 (if (>= (+ (current-left-margin) (length fill-prefix))
			 (current-fill-column))
		     (error "fill-prefix too long for specified width"))
		 (goto-char from)
		 (forward-line 1)
		 (while (not (eobp))
		   (if (looking-at fpre)
		       (delete-region (point) (match-end 0)))
		   (forward-line 1))
		 (goto-char from)
		 (and (looking-at fpre) (goto-char (match-end 0)))
		 (setq from (point)))))
	;; "from" is now before the text to fill,
	;; but after any fill prefix on the first line.

	;; Make sure sentences ending at end of line get an extra space.
	;; loses on split abbrevs ("Mr.\nSmith")
	(while (re-search-forward "[.?!][])}\"']*$" nil t)
	  (insert-and-inherit ? ))
	(goto-char from)
	(skip-chars-forward " \t")
	;; Then change all newlines to spaces.
	(subst-char-in-region from (point-max) ?\n ?\ )
	(if (and nosqueeze (not (eq justify 'full)))
	    nil
	  (canonically-space-region (point) (point-max))
	  (goto-char (point-max))
	  (delete-horizontal-space)
	  (insert-and-inherit " "))
	(goto-char (point-min))

	;; This is the actual filling loop.
	(let ((prefixcol 0) linebeg)
	  (while (not (eobp))
	    (setq linebeg (point))
	    (move-to-column (1+ (current-fill-column)))
	    (if (eobp)
		(or nosqueeze (delete-horizontal-space))
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
	      ;; Replace whitespace here with one newline, then indent to left
	      ;; margin.
	      (skip-chars-backward " ")
	      (insert ?\n)
	      ;; Give newline the properties of the space(s) it replaces
	      (set-text-properties (1- (point)) (point)
				   (text-properties-at (point)))
	      (indent-to-left-margin)
	      ;; Insert the fill prefix after indentation.
	      ;; Set prefixcol so whitespace in the prefix won't get lost.
	      (and fill-prefix (not (equal fill-prefix ""))
		   (progn
		     (insert-and-inherit fill-prefix)
		     (setq prefixcol (current-column)))))
	    ;; Justify the line just ended, if desired.
	    (if justify
		(if (eobp)
		    (justify-current-line justify t t)
		  (forward-line -1)
		  (justify-current-line justify nil t)
		  (forward-line 1))))))
      (forward-char skip-after))))

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

(defun fill-region (from to &optional justify nosqueeze to-eop)
  "Fill each of the paragraphs in the region.
Prefix arg (non-nil third arg, if called from program) means justify as well.

Noninteractively, fourth arg NOSQUEEZE non-nil means to leave
whitespace other than line breaks untouched, and fifth arg TO-EOP
non-nil means to keep filling to the end of the paragraph (or next
hard newline, if `use-hard-newlines' is on).

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "r\nP")
  ;; If using hard newlines, break at every one for filling purposes rather
  ;; than breaking at normal paragraph breaks.
  (let ((paragraph-start (if use-hard-newlines "^" paragraph-start))
 	end beg)
    (save-restriction
      (goto-char (max from to))
      (if to-eop
	  (progn (skip-chars-backward "\n")
		 (forward-paragraph)))
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
	      (fill-region-as-paragraph (point) end justify nosqueeze)
	    (goto-char end)))))))


(defconst default-justification 'left
  "*Method of justifying text not otherwise specified.
Possible values are `left', `right', `full', `center', or `none'.
The requested kind of justification is done whenever lines are filled.
The `justification' text-property  can locally override this variable.
This variable automatically becomes buffer-local when set in any fashion.")
(make-variable-buffer-local 'default-justification)

(defun current-justification ()
  "How should we justify this line?
This returns the value of the text-property `justification',
or the variable `default-justification' if there is no text-property.
However, it returns nil rather than `none' to mean \"don't justify\"."
  (let ((j (or (get-text-property 
		;; Make sure we're looking at paragraph body.
		(save-excursion (skip-chars-forward " \t") (point))
		'justification)
	       default-justification)))
    (if (eq 'none j)
	nil
      j)))

(defun set-justification (begin end value)
  "Set the region's justification style.
If the mark is not active, this operates on the current line.
In interactive use, if the BEGIN and END points are
not at line breaks, they are moved outward to the next line break.
If `use-hard-newlines' is true, they are moved to the next hard line breaks.
Noninteractively, the values of BEGIN, END and VALUE are not modified."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))
		     (let ((s (completing-read 
			       "Set justification to: "
			       '(("left") ("right") ("full") ("center") 
				 ("none"))
			       nil t)))
		       (if (equal s "")
			   (error "")
			 (intern s)))))
  (let* ((paragraph-start (if use-hard-newlines "^" paragraph-start)))
    (save-excursion
      (goto-char begin)
      (while (bolp) (forward-char 1))
      (backward-paragraph)
      (setq begin (point))

      (goto-char end)
      (skip-chars-backward " \t\n" begin)
      (forward-paragraph)
      (setq end (point))
      (set-mark begin)
      (goto-char end)
      (y-or-n-p "set-just")))
  (put-text-property begin end 'justification value)
  (fill-region begin end nil t))

(defun set-justification-none (b e)
  "Disable automatic filling for paragraphs in the region.
If the mark is not active, this applies to the current paragraph."
  (interactive "r")
  (set-justification b e 'none))

(defun set-justification-left (b e)
  "Make paragraphs in the region left-justified.
This is usually the default, but see `enriched-default-justification'.
If the mark is not active, this applies to the current paragraph."
  (interactive "r")
  (set-justification b e 'left))

(defun set-justification-right (b e)
  "Make paragraphs in the region right-justified:
Flush at the right margin and ragged on the left.
If the mark is not active, this applies to the current paragraph."
  (interactive "r")
  (set-justification b e 'right))

(defun set-justification-full (b e)
  "Make paragraphs in the region fully justified:
Flush on both margins.
If the mark is not active, this applies to the current paragraph."
  (interactive "r")
  (set-justification b e 'both))

(defun set-justification-center (b e)
  "Make paragraphs in the region centered.
If the mark is not active, this applies to the current paragraph."
  (interactive "r")
  (set-justification b e 'center))

(defun justify-current-line (&optional how eop nosqueeze)
  "Add spaces to line point is in, so it ends at `fill-column'.
Optional first argument HOW specifies alternate type of justification:
it can be `left', `right', `full', `center', or `none'.  
If HOW is t, will justify however the `justification' function says.
Any other value, including nil, is taken to mean `full'.
Second arg EOP non-nil means that this is the last line of the paragraph, so
it will not be stretched by full justification.
Third arg NOSQUEEZE non-nil means to leave interior whitespace unchanged,
otherwise it is made canonical."
  (interactive (list 'full nil nil))
  (if (eq t how) (setq how (or (current-justification) 'none)))
  (save-excursion
   (save-restriction
    (let ((fc (current-fill-column))
	  ncols beg indent end)
      (end-of-line)
      (if (and use-hard-newlines (null eop) 
	       (get-text-property (point) 'hard))
	  (setq eop t))
      (skip-chars-backward " \t")
      (if (= (current-column) fc)
	  nil ;; Quick exit if it appears to be properly justified already.
	(setq end (point))
	(beginning-of-line)
	(skip-chars-forward " \t")
	(if (and fill-prefix 
		 (equal fill-prefix 
			(buffer-substring (point) 
					  (+ (point) (length fill-prefix)))))
	    (forward-char (length fill-prefix)))
	(setq indent (current-column))
	(setq beg (point))
	(goto-char end)
	(cond ((or (eq 'none how) (eq 'left how))
	       nil)
	      ((eq 'right how)
	       (setq ncols (- (+ indent (current-fill-column))
			      (current-column)))
	       (if (> ncols 0)
		   (indent-line-to ncols)))
	      ((eq 'center how)
	       (setq ncols
		     (/ (- (+ indent (current-fill-column)) (current-column))
			2))
	       (if (>= ncols 0)
		   (indent-line-to ncols)
		 (message "Line to long to center")))
	      (t ;; full
	       (narrow-to-region beg end)
	       (or nosqueeze
		   (canonically-space-region beg end))
	       (goto-char (point-max))
	       (setq ncols (- (current-fill-column) indent (current-column)))
	       (if (< ncols 0)
		   (message "Line to long to justify")
		 (if (and (not eop)
			  (search-backward " " nil t))
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
		       (setq ncols (1- ncols)))))))))))
  nil)


(defun fill-nonuniform-paragraphs (min max &optional justifyp mailp)
  "Fill paragraphs within the region, allowing varying indentation within each.
This command divides the region into \"paragraphs\",
only at paragraph-separator lines, then fills each paragraph
using as the fill prefix the smallest indentation of any line
in the paragraph.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY and MAIL-FLAG:
JUSTIFY to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive "r\nP")
  (let ((fill-individual-varying-indent t))
    (fill-individual-paragraphs min max justifyp mailp)))

(defun fill-individual-paragraphs (min max &optional justify mailp)
  "Fill paragraphs of uniform indentation within the region.
This command divides the region into \"paragraphs\", 
treating every change in indentation level as a paragraph boundary,
then fills each paragraph using its indentation level as the fill prefix.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFY and MAIL-FLAG:
JUSTIFY to justify paragraphs (prefix arg),
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
	    (fill-region-as-paragraph start (point) justify)
	    (or had-newline (delete-char -1))))))))

;;; fill.el ends here
