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

(defvar fill-paragraph-function nil
  "Mode-specific function to fill a paragraph.")

(defun set-fill-prefix ()
  "Set the fill prefix to the current line up to point.
Filling expects lines to start with the fill prefix and
reinserts the fill prefix in each resulting line."
  (interactive)
  (setq fill-prefix (buffer-substring
		     (save-excursion (move-to-left-margin) (point))
		     (point)))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix cancelled")))

(defconst adaptive-fill-mode t
  "*Non-nil means determine a paragraph's fill prefix from its text.")

(defconst adaptive-fill-regexp "[ \t]*\\([#;>*]+ +\\)?"
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
  "Fill the region as one paragraph.
Removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification, and fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive "r\nP")
  ;; Arrange for undoing the fill to restore point.
  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
      (setq buffer-undo-list (cons (point) buffer-undo-list)))

  ;; Make sure "to" is the endpoint.  Make sure that we end up there.
  (goto-char (min from to))
  (setq to   (max from to))
  (setq from (point))

  ;; Delete all but one soft newline at end of region.
  (goto-char to)
  (let ((oneleft nil))
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
	       (not (and use-hard-newlines
			 (get-text-property (1- (point)) 'hard))))
	  (delete-backward-char 1)
	(backward-char 1)
	(setq oneleft t)))
    ;; If there was no newline, create one.
    (if (and (not oneleft) (> (point) from))
	(save-excursion (newline))))
  (setq to (point))

  ;; Ignore blank lines at beginning of region.
  (goto-char from)
  (skip-chars-forward " \t\n")
  (beginning-of-line)
  (setq from (point))
  
  (if (>= from to)
      nil ; There is no paragraph at all.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      (if (and adaptive-fill-mode
	       (or (null fill-prefix) (string= fill-prefix "")))
	  (save-excursion
	    (goto-char from)
	    (if (eolp) (forward-line 1))
	    (forward-line 1)
	    (move-to-left-margin)
	    (if (< (point) to)
		(let ((start (point)))
		  (re-search-forward adaptive-fill-regexp)
		  (setq fill-prefix (buffer-substring start (point)))
		  (set-text-properties 0 (length fill-prefix) nil
				       fill-prefix)))
	    ;; If paragraph has only one line, don't assume in general
	    ;; that additional lines would have the same starting
	    ;; decoration.  Assume no indentation.
	    ))

      (save-restriction
	(goto-char from)
	(beginning-of-line)
	(narrow-to-region (point) to)

	(if (not justify)	    ; filling disabled: just check indentation
	    (progn
	      (goto-char from)
	      (while (not (eobp))
		(if (and (not (eolp))
			 (< (current-indentation) (current-left-margin)))
		    (indent-to-left-margin))
		(forward-line 1)))

	  (if use-hard-newlines
	      (remove-text-properties from (point-max) '(hard nil)))
	  ;; Make sure first line is indented (at least) to left margin...
	  (if (or (memq justify '(right center))
		  (< (current-indentation) (current-left-margin)))
	      (indent-to-left-margin))
	  ;; Delete the fill prefix from every line except the first.
	  ;; The first line may not even have a fill prefix.
	  (goto-char from)
	  (let ((fpre (and fill-prefix (not (equal fill-prefix ""))
			   (concat "[ \t]*"
				   (regexp-quote fill-prefix)
				   "[ \t]*"))))
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
	  ;; Remove indentation from lines other than the first.
	  (beginning-of-line 2)
	  (indent-region (point) (point-max) 0)
	  (goto-char from)

	  ;; FROM, and point, are now before the text to fill,
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
	;; Leave point after final newline.
	(goto-char (point-max)))
    (forward-char 1))))

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.  Prefix arg means justify as well.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.

If `fill-paragraph-function' is non-nil, we call it (passing our
argument to it), and if it returns non-nil, we simply return its value."
  (interactive "P")
  (or (and fill-paragraph-function
	   (let ((function fill-paragraph-function)
		 fill-paragraph-function)
	     (funcall function arg)))
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
	      (fill-region-as-paragraph beg end arg)))))))

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
  (let (end beg)
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
	      end)
	  ;; If using hard newlines, break at every one for filling
	  ;; purposes rather than using paragraph breaks. 
	  (if use-hard-newlines
	      (progn 
		(while (and (setq end (text-property-any (point) (point-max)
							 'hard t))
			    (not (= ?\n (char-after end)))
			    (not (= end (point-max))))
		  (goto-char (1+ end)))
		(setq end (min (point-max) (1+ end)))
		(goto-char initial))
	    (forward-paragraph 1)
	    (setq end (point))
	    (forward-paragraph -1))
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
		(save-excursion (skip-chars-forward " \t") 
				(if (and (eobp) (not (bobp)))
				    (1- (point)) (point)))
		'justification)
	       default-justification)))
    (if (eq 'none j)
	nil
      j)))

(defun set-justification (begin end value &optional whole-par)
  "Set the region's justification style.
The kind of justification to use is prompted for.
If the mark is not active, this command operates on the current paragraph.
If the mark is active, the region is used.  However, if the beginning and end
of the region are not at paragraph breaks, they are moved to the beginning and
end of the paragraphs they are in.
If `use-hard-newlines' is true, all hard newlines are taken to be paragraph
breaks.

When calling from a program, operates just on region between BEGIN and END,
unless optional fourth arg WHOLE-PAR is non-nil.  In that case bounds are
extended to include entire paragraphs as in the interactive command."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))
		     (let ((s (completing-read
			       "Set justification to: "
			       '(("left") ("right") ("full")
				 ("center") ("none"))
			       nil t)))
		       (if (equal s "") (error ""))
		       (intern s))
		     t))
  (save-excursion
    (save-restriction
      (if whole-par
	  (let ((paragraph-start (if use-hard-newlines "." paragraph-start))
		(paragraph-ignore-fill-prefix (if use-hard-newlines t 
						paragraph-ignore-fill-prefix)))
	    (goto-char begin)
	    (while (and (bolp) (not (eobp))) (forward-char 1))
	    (backward-paragraph)
	    (setq begin (point))
	    (goto-char end)
	    (skip-chars-backward " \t\n" begin)
	    (forward-paragraph)
	    (setq end (point))))

      (narrow-to-region (point-min) end)
      (unjustify-region begin (point-max))
      (put-text-property begin (point-max) 'justification value)
      (fill-region begin (point-max) nil t))))

(defun set-justification-none (b e)
  "Disable automatic filling for paragraphs in the region.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'none t))

(defun set-justification-left (b e)
  "Make paragraphs in the region left-justified.
This is usually the default, but see the variable `default-justification'.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'left t))

(defun set-justification-right (b e)
  "Make paragraphs in the region right-justified:
Flush at the right margin and ragged on the left.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'right t))

(defun set-justification-full (b e)
  "Make paragraphs in the region fully justified:
This makes lines flush on both margins by inserting spaces between words.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'full t))

(defun set-justification-center (b e)
  "Make paragraphs in the region centered.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'center t))

;; A line has up to six parts:
;;
;;           >>>                    hello.  		       
;; [Indent-1][FP][    Indent-2     ][text][trailing whitespace][newline]
;;
;; "Indent-1" is the left-margin indentation; normally it ends at column
;;     given by the `current-left-margin' function.
;; "FP" is the fill-prefix.  It can be any string, including whitespace.
;; "Indent-2" is added to justify a line if the `current-justification' is
;;     `center' or `right'.  In `left' and `full' justification regions, any
;;     whitespace there is part of the line's text, and should not be changed.
;; Trailing whitespace is not counted as part of the line length when
;; center- or right-justifying.
;;
;; All parts of the line are optional, although the final newline can 
;;     only be missing on the last line of the buffer.

(defun justify-current-line (&optional how eop nosqueeze)
  "Do some kind of justification on this line.
Normally does full justification: adds spaces to the line to make it end at
the column given by `current-fill-column'.
Optional first argument HOW specifies alternate type of justification:
it can be `left', `right', `full', `center', or `none'.  
If HOW is t, will justify however the `current-justification' function says to.
If HOW is nil or missing, full justification is done by default.
Second arg EOP non-nil means that this is the last line of the paragraph, so
it will not be stretched by full justification.
Third arg NOSQUEEZE non-nil means to leave interior whitespace unchanged,
otherwise it is made canonical."
  (interactive)
  (if (eq t how) (setq how (or (current-justification) 'none)))
  (if (null how) (setq how 'full))
  (or (memq how '(none left))  ; No action required for these.
      (let ((fc (current-fill-column))
	    (pos (point-marker))
	    fp-end			; point at end of fill prefix
	    beg				; point at beginning of line's text
	    end				; point at end of line's text
	    indent			; column of `beg'
	    endcol			; column of `end'
	    ncols)			; new indent point or offset
	(end-of-line)
	;; Check if this is the last line of the paragraph.
	(if (and use-hard-newlines (null eop) 
		 (get-text-property (point) 'hard))
	    (setq eop t))
	(skip-chars-backward " \t")
	;; Quick exit if it appears to be properly justified already
	;; or there is no text.
	(if (or (bolp)
		(and (memq how '(full right))
		     (= (current-column) fc)))
	    nil
	  (setq end (point))
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  ;; Skip over fill-prefix.
	  (if (and fill-prefix 
		   (not (string-equal fill-prefix ""))
		   (equal fill-prefix
			  (buffer-substring 
			   (point) (min (point-max) (+ (length fill-prefix)
						       (point))))))
	      (forward-char (length fill-prefix))
	    (if (and adaptive-fill-mode 
		     (looking-at adaptive-fill-regexp))
		(goto-char (match-end 0))))
	  (setq fp-end (point))
	  (skip-chars-forward " \t")
	  ;; This is beginning of the line's text.
	  (setq indent (current-column))
	  (setq beg (point))
	  (goto-char end)
	  (setq endcol (current-column))

	  ;; HOW can't be null or left--we would have exited already
	  (cond ((eq 'right how) 
		 (setq ncols (- fc endcol))
		 (if (< ncols 0)
		     ;; Need to remove some indentation
		     (delete-region 
		      (progn (goto-char fp-end)
			     (if (< (current-column) (+ indent ncols))
				 (move-to-column (+ indent ncols) t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Need to add some
		   (goto-char beg)
		   (indent-to (+ indent ncols))
		   ;; If point was at beginning of text, keep it there.
		   (if (= beg pos) 
		       (move-marker pos (point)))))

		((eq 'center how)
		 ;; Figure out how much indentation is needed
		 (setq ncols (+ (current-left-margin)
				(/ (- fc (current-left-margin) ;avail. space
				      (- endcol indent)) ;text width
				   2)))
		 (if (< ncols indent)
		     ;; Have too much indentation - remove some
		     (delete-region
		      (progn (goto-char fp-end)
			     (if (< (current-column) ncols)
				 (move-to-column ncols t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Have too little - add some
		   (goto-char beg)
		   (indent-to ncols)
		   ;; If point was at beginning of text, keep it there.
		   (if (= beg pos)
		       (move-marker pos (point)))))

		((eq 'full how)
		 ;; Insert extra spaces between words to justify line
		 (save-restriction
		   (narrow-to-region beg end)
		   (or nosqueeze
		       (canonically-space-region beg end))
		   (goto-char (point-max))
		   (setq ncols (- fc endcol))
		   ;; Ncols is number of additional spaces needed
		   (if (> ncols 0)
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
			     (setq ncols (1- ncols)))))))
		(t (error "Unknown justification value"))))
	(goto-char pos)
	(move-marker pos nil)))
  nil)

(defun unjustify-current-line ()
  "Remove justification whitespace from current line.
If the line is centered or right-justified, this function removes any
indentation past the left margin.  If the line is full-jusitified, it removes
extra spaces between words.  It does nothing in other justification modes."
  (let ((justify (current-justification)))
    (cond ((eq 'left justify) nil)
	  ((eq  nil  justify) nil)
	  ((eq 'full justify)		; full justify: remove extra spaces
	   (beginning-of-line-text)
	   (canonically-space-region
	    (point) (save-excursion (end-of-line) (point))))
	  ((memq justify '(center right))
	   (save-excursion
	     (move-to-left-margin nil t)
	     ;; Position ourselves after any fill-prefix.
	     (if (and fill-prefix 
		      (not (string-equal fill-prefix ""))
		      (equal fill-prefix
			     (buffer-substring 
			      (point) (min (point-max) (+ (length fill-prefix)
							  (point))))))
		 (forward-char (length fill-prefix)))
	     (delete-region (point) (progn (skip-chars-forward " \t")
					   (point))))))))

(defun unjustify-region (&optional begin end)
  "Remove justification whitespace from region.
For centered or right-justified regions, this function removes any indentation
past the left margin from each line.  For full-jusitified lines, it removes 
extra spaces between words.  It does nothing in other justification modes.
Arguments BEGIN and END are optional; default is the whole buffer."
  (save-excursion
    (save-restriction
      (if end (narrow-to-region (point-min) end))
      (goto-char (or begin (point-min)))
      (while (not (eobp))
	(unjustify-current-line)
	(forward-line 1)))))


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
