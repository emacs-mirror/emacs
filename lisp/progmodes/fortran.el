;;; fortran.el --- Fortran mode for GNU Emacs

;;; Copyright (c) 1992 Free Software Foundation, Inc.

;; Author: Michael D. Prange <prange@erl.mit.edu>
;; Maintainer: bug-fortran-mode@erl.mit.edu
;; Version 1.28.8
;; Keywords: languages

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

;; fortran.el version 1.28.8, November 5,1992
;; Many contributions and valuable suggestions by
;; Lawrence R. Dodd, Ralf Fassel, Ralph Finch, Stephen Gildea,
;; Dr. Anil Gokhale, Ulrich Mueller, Mark Neale, Eric Prestemon, 
;; Gary Sabot and Richard Stallman.

;; Maintained (as of version 1.28) by Stephen A. Wood (saw@cebaf.gov)

;;; This version is an update of version 1.21 (Oct 1, 1985).
;;;  Updated by Stephen A. Wood (saw@cebaf.gov) to use tab format
;;;  continuation control and indentation.  (Digit after TAB to signify 
;;;  continuation line.

;;; Notes to fortran-mode version 1.28
;;;  1. Fortran mode can support either fixed format or tab format.  Fixed
;;;     format is where statements start in column 6 (first column is 0)
;;;     and continuation lines are denoted by a character in column 5.
;;;     In tab mode, statements follow a tab character.  Continuation lines
;;;     are where the first character on a line is a tab and the second is
;;;     a digit from 1 to 9.
;;;  2. When fortran mode is called, the buffer is analyzed to determine what
;;;     kind of formating is used.  Starting at the top of the file, lines
;;;     are scanned until a line is found that begins with a tab or 6 spaces.
;;;     The mode for that buffer is then set to either tab or fixed format
;;;     based on that line.  If no line starting with a tab or 6 spaces is
;;;     found before the end  of the buffer or in the first 100 lines, the
;;;     mode is set from the variable `fortran-tab-mode-default'.  t is tab
;;;     mode, nil is fixed format mode.  By default, fixed format mode is used.
;;;     To use tabbing mode as the default, put the following line in .emacs
;;;           (setq fortran-tab-mode-default t)
;;;     This line should not be in the hook since the hook is called only
;;;     after the file is analyzed.
;;;     To force a particular mode independent of the analysis, attach
;;;           (fortran-tab-mode t) or (fortran-tab-mode nil)
;;;     to fortran-mode-hook.
;;;  3. The command `fortran-tab-mode' will toggle between fixed and tab
;;;     formatting modes.  The file will not automatically be reformatted,
;;;     but either `indent-region' or `fortran-indent-subprogram' can be
;;;     used to reformat portions of the file.
;;;  4. Several abbreviations have been added.  Abbreviation mode is turned
;;;     on by default.
;;;  5. The routine fortran-blink-matching if has been incorporated (from
;;;     experimental version 1.27).  If the variable of the same name is set
;;;     to t, the the matching if statement is blinked whenever an endif
;;;     line is indented.
;;;  6. C-c C-w is now bound to fortran-window-create-momentarily (from
;;;     experimental version 1.27.)
;;;  7. LFD is now bound to fortran-reindent-then-newline-and-indent.
;;;  8. fortran-continuation-string (was fortran-continuation-char) is now
;;;     a string rather than a character.
;;;  9. Fixed a bug from 1.21 that gave max-lisp-eval-depth exceeded when
;;;     Comments lines had !'s in them.
;;; 10. DEC fortran recognizes a & in the first column as a continuation.
;;;     character.  This mode does not recognize the & as a continuation
;;;     character.
;;; 11. fortran-blink-matching-if still is in effect when indenting a region.
;;;     Is this a desirable effect?  (It looks kind of neat)
;;; 12. If you strike a digit and there are exactly 5 characters, all spaces
;;;     to the left of the point, the digit will be inserted in place to
;;;     serve as a continuation line marker.  Similarly, if the only thing to
;;;     the left of the point is a single tab, and the last command issued
;;;     was neither fortran-indent-line (TAB) or fortran-reindent-then-newline-
;;;     and-indent (LFD), the digit is inserted as a tab format style
;;;     continuation character.
;;; 13. Both modes should usually work with tab-width set to other than 8.
;;;     However, in tab-mode, if tab-width is less than 6, the column number
;;;     for the minimum indentation is set to 6 so that all line numbers will
;;;     have have a tab after them.  This may be a bit ugly, but why would
;;;     you want to use a tab-width other than 8 anyway?
;;; 14. When in tab mode, the fortran column ruler will not be correct if
;;;     tab-width is not 8.
;;; 15. Fortran-electic-line-number will work properly in overwrite-mode.
;;;     Thanks to Mark Neale (mjn@jet.uk)
;;; 16. Fixed bug in fortran-previous-statement that gives "Incomplete
;;;     continuation statement." when used on the first statement which
;;;     happens to be a comment that begins with the same character as
;;;     `fortran-continuation-string'
;;; 17. If `comment-start-skip' is found in a fortran string, no indenting is
;;;     done.  Thanks to Ralf Fassel (ralf@up3aud1.gwdg.de) for patches.
;;;     This awaits a hopeful future multimode solution in which
;;;     indentation/spacing inside of constants doesn't get touched when
;;;     comment delimeter characters happen to be inside the constant.
;;; 18. Changed meaning of `fortran-comment-line-column'.  If
;;;     If `fortran-comment-indent-style' is 'fixed, then, comments are
;;;     indented to `fortran-minimum-statement-indent' plus
;;;     `fortran-comment-line-column'.  If the style is `relative', the
;;;     meaning remains the same in that the line-column value is added to
;;;     the current indentation level.  The default value is now zero.
;;;     (Thanks to Ulrich Mueller (ulm@vsnhd1.cern.ch).
;;; 19. Fixed infinite loop in fortran-next-statement that occurs with emacs
;;;     versions 18.55 and before because of a difference in the behavior
;;;     of (forward-line 1) on a line that is the last in the buffer which
;;;     doesn't have a newline.  (Thanks to Ulrich Mueller)
;;; 20. Added indentation for structure, union and map blocks (Fortran 90
;;;     and other post f77 fortrans.) at the suggestion of Dr. Anil Gokhale
;;;     (avg@dynsim1.litwin.com).
;;; 21. The command fortran-auto-fill-mode toggles on and off fortran-auto-fill
;;;     mode.  By default it is off, and the fill column is 72.  (Thanks to
;;;     (Mark Neale for the code for auto fill.)  Will split line before
;;;     whitespace, commas, or operators.  Won't break stuff betweek quotes,
;;;     unless it is a comment line.  Put (fortran-auto-fill-mode 1) into
;;;     fortran-mode-hook to have auto fill mode automatically on.
;;; 22. If auto-fill-mode is on, fortran-indent-line will call the auto fill
;;;     code to make sure that lines are not to long after indentation.  This
;;;     suggestion and improvements to auto filling provided by Eric Prestemon
;;;     (ecprest@pocorvares.er.usgs.gov.
;;; 23. comment-line-start-skip treats cpp directives (beginning with #) as
;;;     unindentable comments.
;;; 24. where, elsewhere indenting now supported for F90.
;;; 

;;; Bugs to bug-fortran-mode@erl.mit.edu

(defconst fortran-mode-version "version 1.28.8")

;;; Code:

;;;###autoload
(defvar fortran-tab-mode-default nil
  "*Default tabbing/carriage control style for empty files in Fortran mode.
A value of t specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6.")

(defvar fortran-tab-mode nil
  "*tabbing/carriage control style for Fortran mode.
A value of t specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6.")

;; Buffer local, used to display mode line.
(defvar fortran-tab-mode-string)

(defvar fortran-do-indent 3
  "*Extra indentation applied to DO blocks.")

(defvar fortran-if-indent 3
  "*Extra indentation applied to IF blocks.")

(defvar fortran-structure-indent 3
  "*Extra indentation applied to STRUCTURE, UNION and MAP blocks.")

(defvar fortran-continuation-indent 5
  "*Extra indentation applied to Fortran continuation lines.")

(defvar fortran-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed produces fixed comment indentation to `fortran-comment-line-column'
beyond `fortran-minimum-statement-indent', and 'relative indents to current
Fortran indentation plus `fortran-comment-line-column'.")

(defvar fortran-comment-line-column 0
  "*Amount of extra indentation for text within full-line comments.")

(defvar comment-line-start nil
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")

(defvar fortran-minimum-statement-indent 6
  "*Minimum indentation for Fortran statements.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar fortran-comment-indent-char " "
  "*Single-character string inserted for Fortran comment indentation.
Normally a space.")

(defvar fortran-line-number-indent 1
  "*Maximum indentation for Fortran line numbers.
5 means right-justify them within their five-column field.")

(defvar fortran-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible DO loop ends.")

(defvar fortran-blink-matching-if nil
  "*From a Fortran ENDIF statement, blink the matching IF statement.")

(defvar fortran-continuation-string "$"
  "*Single-character string used for Fortran continuation lines.
In fixed format continuation style, this character is inserted in
column 6 by \\[fortran-split-line] to begin a continuation line.
Also, if \\[fortran-indent-line] finds this at the beginning of a line, it will
convert the line into a continuation line of the appropriate style.
Normally $.")

(defvar fortran-comment-region "c$$$"
  "*String inserted by \\[fortran-comment-region]\
 at start of each line in region.")

(defvar fortran-electric-line-number t
  "*Non-nil causes line number digits to be moved to the correct column as\
 typed.")

(defvar fortran-startup-message t
  "*Non-nil displays a startup message when Fortran mode is first called.")

(defvar fortran-column-ruler " "
  "*String displayed above current line by \\[fortran-column-ruler].")

(defconst bug-fortran-mode "bug-fortran-mode@erl.mit.edu"
  "Address of mailing list for Fortran mode bugs.")

(defvar fortran-mode-syntax-table nil
  "Syntax table in use in Fortran mode buffers.")

(defvar fortran-analyze-depth 100
  "Number of lines to scan to determine whether to use fixed or tab format\
 style.")

(defvar fortran-break-before-delimiters t
  "*Non-nil causes `fortran-do-auto-fill' to break lines before delimeters.")

(if fortran-mode-syntax-table
    ()
  (setq fortran-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" fortran-mode-syntax-table)
  (modify-syntax-entry ?\r " " fortran-mode-syntax-table)
  (modify-syntax-entry ?+ "." fortran-mode-syntax-table)
  (modify-syntax-entry ?- "." fortran-mode-syntax-table)
  (modify-syntax-entry ?= "." fortran-mode-syntax-table)
  (modify-syntax-entry ?* "." fortran-mode-syntax-table)
  (modify-syntax-entry ?/ "." fortran-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" fortran-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" fortran-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" fortran-mode-syntax-table)
  (modify-syntax-entry ?. "w" fortran-mode-syntax-table)
  (modify-syntax-entry ?_ "w" fortran-mode-syntax-table)
  (modify-syntax-entry ?\n ">" fortran-mode-syntax-table))

(defvar fortran-mode-map () 
  "Keymap used in Fortran mode.")
(if fortran-mode-map
    ()
  (setq fortran-mode-map (make-sparse-keymap))
  (define-key fortran-mode-map ";" 'fortran-abbrev-start)
  (define-key fortran-mode-map "\C-c;" 'fortran-comment-region)
  (define-key fortran-mode-map "\e\C-a" 'beginning-of-fortran-subprogram)
  (define-key fortran-mode-map "\e\C-e" 'end-of-fortran-subprogram)
  (define-key fortran-mode-map "\e;" 'fortran-indent-comment)
  (define-key fortran-mode-map "\e\C-h" 'mark-fortran-subprogram)
  (define-key fortran-mode-map "\e\n" 'fortran-split-line)
  (define-key fortran-mode-map "\n" 'fortran-reindent-then-newline-and-indent)
  (define-key fortran-mode-map "\e\C-q" 'fortran-indent-subprogram)
  (define-key fortran-mode-map "\C-c\C-w" 'fortran-window-create-momentarily)
  (define-key fortran-mode-map "\C-c\C-r" 'fortran-column-ruler)
  (define-key fortran-mode-map "\C-c\C-p" 'fortran-previous-statement)
  (define-key fortran-mode-map "\C-c\C-n" 'fortran-next-statement)
  (define-key fortran-mode-map "\t" 'fortran-indent-line)
  (define-key fortran-mode-map "0" 'fortran-electric-line-number)
  (define-key fortran-mode-map "1" 'fortran-electric-line-number)
  (define-key fortran-mode-map "2" 'fortran-electric-line-number)
  (define-key fortran-mode-map "3" 'fortran-electric-line-number)
  (define-key fortran-mode-map "4" 'fortran-electric-line-number)
  (define-key fortran-mode-map "5" 'fortran-electric-line-number)
  (define-key fortran-mode-map "6" 'fortran-electric-line-number)
  (define-key fortran-mode-map "7" 'fortran-electric-line-number)
  (define-key fortran-mode-map "8" 'fortran-electric-line-number)
  (define-key fortran-mode-map "9" 'fortran-electric-line-number))

(defvar fortran-mode-abbrev-table nil)
(if fortran-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'fortran-mode-abbrev-table ())
    (define-abbrev fortran-mode-abbrev-table  ";au"  "automatic" nil)
    (define-abbrev fortran-mode-abbrev-table  ";b"   "byte" nil)
    (define-abbrev fortran-mode-abbrev-table  ";bd"  "block data" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ch"  "character" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev fortran-mode-abbrev-table  ";c"   "continue" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cm"  "common" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cx"  "complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";df"  "define" nil)
    (define-abbrev fortran-mode-abbrev-table  ";di"  "dimension" nil)
    (define-abbrev fortran-mode-abbrev-table  ";do"  "double" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dc"  "double complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dp"  "double precision" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev fortran-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev fortran-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev fortran-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev fortran-mode-abbrev-table  ";eq"  "equivalence" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ew"  "endwhere" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ex"  "external" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ey"  "entry" nil)
    (define-abbrev fortran-mode-abbrev-table  ";f"   "format" nil)
    (define-abbrev fortran-mode-abbrev-table  ";fa"  ".false." nil)
    (define-abbrev fortran-mode-abbrev-table  ";fu"  "function" nil)
    (define-abbrev fortran-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev fortran-mode-abbrev-table  ";im"  "implicit" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ib"  "implicit byte" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ic"  "implicit complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ich" "implicit character" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ii"  "implicit integer" nil)
    (define-abbrev fortran-mode-abbrev-table  ";il"  "implicit logical" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ir"  "implicit real" nil)
    (define-abbrev fortran-mode-abbrev-table  ";inc" "include" nil)
    (define-abbrev fortran-mode-abbrev-table  ";in"  "integer" nil)
    (define-abbrev fortran-mode-abbrev-table  ";intr" "intrinsic" nil)
    (define-abbrev fortran-mode-abbrev-table  ";l"   "logical" nil)
    (define-abbrev fortran-mode-abbrev-table  ";n"   "namelist" nil)
    (define-abbrev fortran-mode-abbrev-table  ";o"   "open" nil) ; was ;op
    (define-abbrev fortran-mode-abbrev-table  ";pa"  "parameter" nil)
    (define-abbrev fortran-mode-abbrev-table  ";pr"  "program" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ps"  "pause" nil)
    (define-abbrev fortran-mode-abbrev-table  ";p"   "print" nil)
    (define-abbrev fortran-mode-abbrev-table  ";rc"  "record" nil)
    (define-abbrev fortran-mode-abbrev-table  ";re"  "real" nil)
    (define-abbrev fortran-mode-abbrev-table  ";r"   "read" nil)
    (define-abbrev fortran-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev fortran-mode-abbrev-table  ";rw"  "rewind" nil)
    (define-abbrev fortran-mode-abbrev-table  ";s"   "stop" nil)
    (define-abbrev fortran-mode-abbrev-table  ";sa"  "save" nil)
    (define-abbrev fortran-mode-abbrev-table  ";st"  "structure" nil)
    (define-abbrev fortran-mode-abbrev-table  ";sc"  "static" nil)
    (define-abbrev fortran-mode-abbrev-table  ";su"  "subroutine" nil)
    (define-abbrev fortran-mode-abbrev-table  ";tr"  ".true." nil)
    (define-abbrev fortran-mode-abbrev-table  ";ty"  "type" nil)
    (define-abbrev fortran-mode-abbrev-table  ";vo"  "volatile" nil)
    (define-abbrev fortran-mode-abbrev-table  ";w"   "write" nil)
    (define-abbrev fortran-mode-abbrev-table  ";wh"  "where" nil)
    (setq abbrevs-changed ac)))

;;;###autoload
(defun fortran-mode ()
  "Major mode for editing Fortran code.
\\[fortran-indent-line] indents the current Fortran line correctly. 
DO statements must not share a common CONTINUE.

Type ;? or ;\\[help-command] to display a list of built-in\
 abbrevs for Fortran keywords.

Key definitions:
\\{fortran-mode-map}

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Fortran mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 fortran-do-indent
    Extra indentation within do blocks.  (default 3)
 fortran-if-indent
    Extra indentation within if blocks.  (default 3)
 fortran-structure-indent
    Extra indentation within structure, union and map blocks.  (default 3)
 fortran-continuation-indent
    Extra indentation applied to continuation statements.  (default 5)
 fortran-comment-line-column
    Amount of extra indentation for text within full-line comments. (default 0)
 fortran-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at `fortran-comment-line-column' beyond
           the value of `fortran-minimum-statement-indent',
    relative  means indent at `fortran-comment-line-column' beyond the
 	      indentation for a line of code.
    (default 'fixed)
 fortran-comment-indent-char
    Single-character string to be inserted instead of space for
    full-line comment indentation.  (default \" \")
 fortran-minimum-statement-indent
    Minimum indentation for Fortran statements. (default 6)
 fortran-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 fortran-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible \"continue\"
    statements.  (default nil)
 fortran-blink-matching-if 
    From a Fortran ENDIF statement, blink the matching IF statement.
    (default nil)
 fortran-continuation-string
    Single-character string to be inserted in column 5 of a continuation
    line.  (default \"$\")
 fortran-comment-region
    String inserted by \\[fortran-comment-region] at start of each line in 
    region.  (default \"c$$$\")
 fortran-electric-line-number
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 fortran-break-before-delimiters
    Non-nil causes `fortran-do-auto-fill' breaks lines before delimeters.
    (default t)
 fortran-startup-message
    Set to nil to inhibit message first time Fortran mode is used.

Turning on Fortran mode calls the value of the variable `fortran-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if fortran-startup-message
      (message "Emacs Fortran mode %s.  Bugs to %s"
	       fortran-mode-version bug-fortran-mode))
  (setq fortran-startup-message nil)
  (setq local-abbrev-table fortran-mode-abbrev-table)
  (set-syntax-table fortran-mode-syntax-table)
  (make-local-variable 'fortran-break-before-delimiters)
  (setq fortran-break-before-delimiters t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'fortran-indent-line)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'fortran-comment-hook)
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip
	"^[Cc*]\\(\\([^ \t\n]\\)\\2\\2*\\)?[ \t]*\\|^#.*")
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "c")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "![ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start nil)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (setq abbrev-mode t) ; ?? (abbrev-mode 1) instead??
  (setq fill-column 72) ; Already local?
  (use-local-map fortran-mode-map)
  (setq mode-name "Fortran")
  (setq major-mode 'fortran-mode)
  (make-local-variable 'fortran-tab-mode)
  (make-local-variable 'fortran-comment-line-column)
  (make-local-variable 'fortran-minimum-statement-indent)
  (make-local-variable 'fortran-column-ruler)
  (make-local-variable 'fortran-tab-mode-string)
  (fortran-tab-mode (fortran-analyze-file-format))
  (run-hooks 'fortran-mode-hook))

(defun fortran-comment-hook ()
  (save-excursion
    (skip-chars-backward " \t")
    (max (+ 1 (current-column))
	 comment-column)))

(defun fortran-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (fortran-indent-line))
	((find-comment-start-skip)      ; this catches any inline comment and
					; leaves point after comment-start-skip
	 (if comment-start-skip
	     (progn (goto-char (match-beginning 0))
		    (if (not (= (current-column) (fortran-comment-hook)))
			(progn (delete-horizontal-space)
			       (indent-to (fortran-comment-hook)))))
	   (end-of-line)))        ; otherwise goto end of line or sth else?
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (fortran-comment-hook))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp fortran-comment-indent-char)
			  (aref fortran-comment-indent-char 0)
			fortran-comment-indent-char)
		      (- (calculate-fortran-indent) (current-column))))))

(defun fortran-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts fortran-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert fortran-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert fortran-comment-region)))
      (let ((com (regexp-quote fortran-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun fortran-abbrev-start ()
  "Typing ;\\[help-command] or ;? lists all the Fortran abbrevs. 
Any other key combination is executed normally."
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (= (setq c (read-char)) ??)	;insert char if not equal to `?'
	    (= c help-char))
	(fortran-abbrev-help)
      (setq unread-command-event c))))

(defun fortran-abbrev-help ()
  "List the currently defined abbrevs in Fortran mode."
  (interactive)
  (message "Listing abbrev table...")
  (require 'abbrevlist)
  (display-buffer (fortran-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun fortran-prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (insert-abbrev-table-description fortran-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun fortran-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of `fortran-column-ruler'.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display 
   fortran-column-ruler (save-excursion
			  (beginning-of-line) 
			  (if (eq (window-start (selected-window))
				  (window-point (selected-window)))
			      (progn (forward-line) (point))
			    (point)))
   nil "Type SPC or any command to erase ruler."))

(defun fortran-window-create ()
  "Makes the window 72 columns wide.
See also `fortran-window-create-momentarily'."
  (interactive)
  (condition-case error
      (progn
	(let ((window-min-width 2))
	  (if (< (window-width) (screen-width))
	      (enlarge-window-horizontally (- (screen-width)
					      (window-width) 1)))
	  (split-window-horizontally 73)
	  (other-window 1)
	  (switch-to-buffer " fortran-window-extra" t)
	  (select-window (previous-window))))
    (error (message "No room for Fortran window.")
	   'error)))

(defun fortran-window-create-momentarily (&optional arg)
  "Momentarily makes the window 72 columns wide.
Optional ARG non-nil and non-unity disables the momentary feature.
See also `fortran-window-create'."
  (interactive "p")
  (if (or (not arg)
	  (= arg 1))
      (save-window-excursion
	(if (not (equal (fortran-window-create) 'error))
	    (progn (message "Type SPC to continue editing.")
		   (let ((char (read-char)))
		     (or (equal char (string-to-char " "))
			 (setq unread-command-event char))))))
    (fortran-window-create)))

(defun fortran-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert "\n" comment-line-start " ")
    (if fortran-tab-mode
	(progn 
	  (insert "\n\t")
	  (insert-char (fortran-numerical-continuation-char) 1))
      (insert "\n " fortran-continuation-string)));Space after \n important
  (fortran-indent-line))		;when the cont string is C, c or *.

(defun fortran-numerical-continuation-char ()
  "Return a digit for tab-digit style of continution lines.
If, previous line is a tab-digit continuation line, returns that digit
plus one.  Otherwise return 1.  Zero not allowed."
  (save-excursion
    (forward-line -1)
    (if (looking-at "\t[1-9]")
	(+ ?1 (% (- (char-after (+ (point) 1)) ?0) 9))
      ?1)))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun fortran-electric-line-number (arg)
  "Self insert, but if part of a Fortran line number indent it automatically.
Auto-indent does not happen if a numeric arg is used."
  (interactive "P")
  (if (or arg (not fortran-electric-line-number))
      (if arg 
	  (self-insert-command (prefix-numeric-value arg))
	(self-insert-command 1))
    (if (or (and (= 5 (current-column))
		 (save-excursion
		   (beginning-of-line)
		   (looking-at "     ")));In col 5 with only spaces to left.
	    (and (= fortran-minimum-statement-indent (current-column))
		 (save-excursion
		   (beginning-of-line)
		   (looking-at "\t"));In col 8 with a single tab to the left.
		 (not (or (eq last-command 'fortran-indent-line)
			  (eq last-command
			      'fortran-reindent-then-newline-and-indent))))
	    (save-excursion
	      (re-search-backward "[^ \t0-9]"
				  (save-excursion
				    (beginning-of-line)
				    (point))
				  t)) ;not a line number
	    (looking-at "[0-9]")	;within a line number
	    )
	(self-insert-command (prefix-numeric-value arg))
      (skip-chars-backward " \t")
      (insert last-command-char)
      (fortran-indent-line))))

(defun beginning-of-fortran-subprogram ()
  "Moves point to the beginning of the current Fortran subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (if (looking-at "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")
	(forward-line 1))))

(defun end-of-fortran-subprogram ()
  "Moves point to the end of the current Fortran subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-fortran-subprogram ()
  "Put mark at end of Fortran subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-fortran-subprogram)
  (push-mark (point))
  (beginning-of-fortran-subprogram))

(defun fortran-previous-statement ()
  "Moves point to beginning of the previous Fortran statement.
Returns `first-statement' if that statement is the first
non-comment Fortran statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (and
	   (not (looking-at comment-line-start-skip))
	   (or (looking-at
	        (concat "[ \t]*" (regexp-quote fortran-continuation-string)))
	       (or (looking-at "     [^ 0\n]")
		   (looking-at "\t[1-9]")))))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at "     [^ 0\n]")
		    (looking-at "\t[1-9]")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test	
	   (fortran-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun fortran-next-statement ()
  "Moves point to beginning of the next Fortran statement.
Returns `last-statement' if that statement is the last
non-comment Fortran statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
		      (and (= (forward-line 1) 0)
			   (not (eobp))))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
 		    (looking-at "     [^ 0\n]")
 		    (looking-at "\t[1-9]")
 		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun fortran-blink-matching-if ()
  "From a Fortran ENDIF statement, blink the matching IF statement."
  (let ((count 1) (top-of-window (window-start)) matching-if
	(endif-point (point)) message)
    (if (save-excursion (beginning-of-line)
			(skip-chars-forward " \t0-9")
			(looking-at "end[ \t]*if\\b"))
	(progn
	  (save-excursion
	    (while (and (not (= count 0))
			(not (eq (fortran-previous-statement)
				 'first-statement))
			(not (looking-at
			      "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")))
					; Keep local to subprogram
	      (skip-chars-forward " \t0-9")
	      (cond ((looking-at "if[ \t]*(")
		     (save-excursion
		       (if (or
			    (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
			    (let (then-test);multi-line if-then
			      (while
				  (and (= (forward-line 1) 0)
					;search forward for then
				       (or (looking-at "     [^ 0\n]")
					   (looking-at "\t[1-9]"))
				       (not
					(setq
					 then-test
					 (looking-at
					  ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
			      then-test))
			   (setq count (- count 1)))))
		    ((looking-at "end[ \t]*if\\b")
		     (setq count (+ count 1)))))
	    (if (not (= count 0))
		(setq message "No matching if.")
	      (if (< (point) top-of-window)
		  (setq message (concat "Matches " (buffer-substring
						    (progn (beginning-of-line)
							   (point))
						    (progn (end-of-line)
							   (point)))))
		(setq matching-if (point)))))
	  (if message
	      (message "%s" message)
	    (goto-char matching-if)
	    (sit-for 1)
	    (goto-char endif-point))))))

(defun fortran-indent-line ()
  "Indents current Fortran line based on its contents and on previous lines."
  (interactive)
  (let ((cfi (calculate-fortran-indent)))
    (save-excursion
      (beginning-of-line)
      (if (or (not (= cfi (fortran-current-line-indentation)))
	      (and (re-search-forward "^[ \t]*[0-9]+" (+ (point) 4) t)
		   (not (fortran-line-number-indented-correctly-p))))
	  (fortran-indent-to-column cfi)
	(beginning-of-line)
	(if (and (not (looking-at comment-line-start-skip))
		 (find-comment-start-skip))
	    (fortran-indent-comment))))
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
	(move-to-column cfi))
    (if (and auto-fill-hook
	     (> (save-excursion (end-of-line) (current-column)) fill-column))
	(save-excursion
	  (end-of-line)
	  (fortran-do-auto-fill)))
    (if fortran-blink-matching-if
	(fortran-blink-matching-if))))

(defun fortran-reindent-then-newline-and-indent ()
  "Reindent the current Fortran line, insert a newline and indent the newline.
An abbrev before point is expanded if `abbrev-mode' is non-nil."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (or (looking-at "[0-9]")	;Reindent only where it is most
	    (looking-at "end")		;likely to be necessary
	    (looking-at "else")
	    (looking-at (regexp-quote fortran-continuation-string)))
	(fortran-indent-line)))
  (newline)
  (fortran-indent-line))

(defun fortran-indent-subprogram ()
  "Properly indents the Fortran subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-fortran-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-fortran-indent ()
  "Calculates the Fortran indent column based on previous lines."
  (let (icol first-statement (case-fold-search t)
	     (fortran-minimum-statement-indent
	      fortran-minimum-statement-indent))
    (save-excursion
      (setq first-statement (fortran-previous-statement))
      (if first-statement
	  (setq icol fortran-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol fortran-minimum-statement-indent)
	    (setq icol (fortran-current-line-indentation)))
	  (skip-chars-forward " \t0-9")
	  (cond ((looking-at "if[ \t]*(")
		 (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t_$(=a-z0-9]")
			 (let (then-test)	;multi-line if-then
			   (while (and (= (forward-line 1) 0)
				       ;;search forward for then
				       (or (looking-at "     [^ 0\n]")
					   (looking-at "\t[1-9]"))
				       (not (setq then-test (looking-at
							     ".*then\\b[ \t]\
*[^ \t_$(=a-z0-9]")))))
			   then-test))
		     (setq icol (+ icol fortran-if-indent))))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol fortran-if-indent)))
		((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
		 (setq icol (+ icol fortran-if-indent)))
		((looking-at "where.*(.*)[ \t]*\n")
		 (setq icol (+ icol fortran-if-indent)))
		((looking-at "do\\b")
		 (setq icol (+ icol fortran-do-indent)))
		((looking-at
		  "\\(structure\\|union\\|map\\)\\b[ \t]*[^ \t=(a-z]")
		 (setq icol (+ icol fortran-structure-indent)))
		((looking-at "end\\b[ \t]*[^ \t=(a-z]")
		 ;; Previous END resets indent to minimum
		 (setq icol fortran-minimum-statement-indent))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
	    ((looking-at comment-line-start-skip)
	     (cond ((eq fortran-comment-indent-style 'relative)
		    (setq icol (+ icol fortran-comment-line-column)))
		   ((eq fortran-comment-indent-style 'fixed)
		    (setq icol (+ fortran-minimum-statement-indent
				  fortran-comment-line-column))))
	     (setq fortran-minimum-statement-indent 0))
	    ((or (looking-at (concat "[ \t]*"
				     (regexp-quote
				      fortran-continuation-string)))
		 (looking-at "     [^ 0\n]")
		 (looking-at "\t[1-9]"))
	     (setq icol (+ icol fortran-continuation-indent)))
	    (first-statement)
	    ((and fortran-check-all-num-for-matching-do
		  (looking-at "[ \t]*[0-9]+")
		  (fortran-check-for-matching-do))
	     (setq icol (- icol fortran-do-indent)))
	    (t
	     (skip-chars-forward " \t0-9")
	     (cond ((looking-at "end[ \t]*if\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((looking-at "\\(else\\|elseif\\)\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((looking-at "\\(otherwise\\|else[ \t]*where\\)\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((looking-at "end[ \t]*where\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((and (looking-at "continue\\b")
			 (fortran-check-for-matching-do))
		    (setq icol (- icol fortran-do-indent)))
		   ((looking-at "end[ \t]*do\\b")
		    (setq icol (- icol fortran-do-indent)))
		   ((looking-at
		     "end[ \t]*\
\\(structure\\|union\\|map\\)\\b[ \t]*[^ \t=(a-z]")
		    (setq icol (- icol fortran-structure-indent)))
		   ((and (looking-at "end\\b[ \t]*[^ \t=(a-z]")
			 (not (= icol fortran-minimum-statement-indent)))
 		    (message "Warning: `end' not in column %d.  Probably\
 an unclosed block." fortran-minimum-statement-indent))))))
    (max fortran-minimum-statement-indent icol)))

(defun fortran-current-line-indentation ()
  "Indentation of current line, ignoring Fortran line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5/8 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at comment-line-start-skip)
	   (goto-char (match-end 0))
	   (skip-chars-forward
	    (if (stringp fortran-comment-indent-char)
		fortran-comment-indent-char
	      (char-to-string fortran-comment-indent-char))))
	  ((or (looking-at "     [^ 0\n]")
	       (looking-at "\t[1-9]"))
	   (goto-char (match-end 0)))
	  (t
	   ;; Move past line number.
	   (skip-chars-forward "[ \t0-9]");From Uli
	   ))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun fortran-indent-to-column (col)
  "Indents current line with spaces to column COL.
notes: 1) A non-zero/non-blank character in column 5 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If `fortran-continuation-string' is the first non-whitespace
          character, this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line.
       4) A TAB followed by a digit indicates a continuation line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if fortran-comment-indent-style
	    (let ((char (if (stringp fortran-comment-indent-char)
			    (aref fortran-comment-indent-char 0)
			  fortran-comment-indent-char)))
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " \t" (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      (if (looking-at "\t[1-9]")
	  (if fortran-tab-mode
	      (goto-char (match-end 0))
	    (delete-char 2)
	    (insert "     ")
	    (insert fortran-continuation-string))
	(if (looking-at "     [^ 0\n]")
	    (if fortran-tab-mode
		(progn (delete-char 6)
		       (insert "\t")
		       (insert-char (fortran-numerical-continuation-char) 1))
	      (forward-char 6))
	  (delete-horizontal-space)
	  ;; Put line number in columns 0-4
	  ;; or put continuation character in column 5.
	  (cond ((eobp))
		((looking-at (regexp-quote fortran-continuation-string))
		 (if fortran-tab-mode
		     (progn
		       (indent-to fortran-minimum-statement-indent)
		       (delete-char 1)
		       (insert-char (fortran-numerical-continuation-char) 1))
		   (indent-to 5)
		   (forward-char 1)))
		((looking-at "[0-9]+")
		 (let ((extra-space (- 5 (- (match-end 0) (point)))))
		   (if (< extra-space 0)
		       (message "Warning: line number exceeds 5-digit limit.")
		     (indent-to (min fortran-line-number-indent extra-space))))
		 (skip-chars-forward "0-9")))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (if (and comment-start-skip
	       (find-comment-start-skip))
	  (progn (goto-char (match-beginning 0))
		 (if (not (= (current-column) (fortran-comment-hook)))
		     (progn (delete-horizontal-space)
			    (indent-to (fortran-comment-hook)))))))))

(defun fortran-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indented.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) fortran-line-number-indent)
	 (or (= (current-column) fortran-line-number-indent)
	     (progn (skip-chars-forward "0-9")
		    (= (current-column) 5))))))

(defun fortran-check-for-matching-do ()
  "When called from a numbered statement, returns t if matching DO is found.
Otherwise return a nil."
  (let (charnum
	(case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]*[0-9]+")
	  (progn
	    (skip-chars-forward " \t")
	    (skip-chars-forward "0") ;skip past leading zeros
	    (setq charnum (buffer-substring (point)
					    (progn (skip-chars-forward "0-9")
						   (point))))
	    (beginning-of-line)
	    (and (re-search-backward
		  (concat
		   "\\(^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]\\)\\|\\(^[ \t0-9]*do\
[ \t]*0*"
		   charnum "\\b\\)\\|\\(^[ \t]*0*" charnum "\\b\\)")
		  nil t)
		 (looking-at (concat "^[ \t0-9]*do[ \t]*0*" charnum))))))))

(defun fortran-analyze-file-format ()
  "Return 0 if Fixed format is used, 1 if Tab formatting is used.
Use `fortran-tab-mode-default' if no non-comment statements are found in the
file before the end or the first `fortran-analyze-depth' lines."
  (let ((i 0))
    (save-excursion
      (goto-char (point-min))
      (setq i 0)
      (while (not (or
		   (eobp)
		   (looking-at "\t")
		   (looking-at "      ")
		   (> i fortran-analyze-depth)))
	(forward-line)
	(setq i (1+ i)))
      (cond
       ((looking-at "\t") 1)
       ((looking-at "      ") 0)
       (fortran-tab-mode-default 1)
       (t 0)))))

(defun fortran-tab-mode (arg)
  "Toggle `fortran-tab-mode' which indicates style of continuation lines.
With no argument, toggle on/off the tabbing mode of continuation lines.
If argument is a positive number, or non-nil if not a number,
`fortran-tab-mode' is turned on.  Otherwise `fortran-tab-mode' is false"
  (interactive "P")
  (setq fortran-tab-mode
	(if (null arg) (not fortran-tab-mode)
	  (if (numberp arg)
	      (> (prefix-numeric-value arg) 0)
	    arg)))
  (if fortran-tab-mode
      (fortran-setup-tab-format-style)
    (fortran-setup-fixed-format-style))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

(defun fortran-setup-tab-format-style ()
  "Set up Fortran mode to use the TAB-digit mode of continuation lines.
Use the command `fortran-tab-mode' to toggle between this and fixed
format style."
  (setq fortran-minimum-statement-indent (max tab-width 6))
  (setq indent-tabs-mode t)
  (setq fortran-column-ruler
	(concat
	 "0       810        20        30        40        5\
0        60        70\n"
	 "[   ]|  { |    |    |    |    |    |    |    |    \
|    |    |    |    |}\n"))
  (setq fortran-tab-mode-string " TAB-format")
  (set-buffer-modified-p (buffer-modified-p)))

(defun fortran-setup-fixed-format-style ()
  "Set up Fortran mode to use the column 6 mode of continuation lines.
Use the command `fortran-tab-mode' to toggle between this and tab
character format style."
  (setq fortran-minimum-statement-indent 6)
  (setq indent-tabs-mode nil)
  (setq fortran-column-ruler
	(concat
	 "0   4 6  10        20        30        40        5\
0        60        70\n"
	 "[   ]|{   |    |    |    |    |    |    |    |    \
|    |    |    |    |}\n"))
  (setq fortran-tab-mode-string " Fixed-format")
  (set-buffer-modified-p (buffer-modified-p)))

(or (assq 'fortran-tab-mode-string minor-mode-alist)
    (setq minor-mode-alist (cons
			    '(fortran-tab-mode-string fortran-tab-mode-string)
			    minor-mode-alist)))

(defun find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (let ((save-match-beginning) (save-match-end))
    (if (save-excursion 
	  (re-search-forward comment-start-skip
			     (save-excursion (end-of-line) (point)) t))
	(progn
	  (setq save-match-beginning (match-beginning 0))
	  (setq save-match-end (match-end 0))
	  (if (is-in-fortran-string-p (match-beginning 0))
	      (progn
		(save-excursion
		  (goto-char save-match-end)
		  (find-comment-start-skip))	; recurse for rest of line
		)
	    (goto-char save-match-beginning)
	    (re-search-forward comment-start-skip
			       (save-excursion (end-of-line) (point)) t)
	    (goto-char (match-end 0))
	    t))
      nil)))

(defun is-in-fortran-string-p (pos)
  "Return t if POS (a buffer position) is inside a standard Fortran string.
Fortran strings are delimeted by apostrophes (\').  Quote-Escape-sequences
(\\'), strings delimited by \" and detection of syntax-errors
(unbalanced quotes) are NOT supported."
;;; The algorithm is simple: start at point with value nil
;;; and toggle value at each quote found until end of line.
;;; The quote skip is hard-coded, maybe it's possible to change this
;;; and use something like 'string-constant-delimiter' (which
;;; doesn't exist yet) so this function can be used by other modes,
;;; but then one must pay attention to escape sequences, multi-line-constants
;;; and such things.
  (let ((is-in-fortran-string nil))
    (save-excursion   
      (goto-char pos)
      (fortran-previous-statement)
      (fortran-next-statement)
      (while (< (point) pos)
	;; Make sure we don't count quotes in continuation column.
	(if (looking-at "^     ")
	    (goto-char (+ 1 (match-end 0)))
	  (if (and (not is-in-fortran-string)
		   (looking-at comment-start-skip))
	      (beginning-of-line 2)
	    (if (looking-at "'")
		(setq is-in-fortran-string (not is-in-fortran-string)))
	    (forward-char 1)))))
    is-in-fortran-string))

(defun fortran-auto-fill-mode (arg)
  "Toggle fortran-auto-fill mode.
With ARG, turn `fortran-auto-fill' mode on iff ARG is positive.
In `fortran-auto-fill' mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-hook
	       (if (if (null arg)
		       (not auto-fill-hook)
		     (> (prefix-numeric-value arg) 0))
		   'fortran-indent-line
		 nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

(defun fortran-do-auto-fill ()
  (interactive)
  (let* ((opoint (point))
	 (bol (save-excursion (beginning-of-line) (point)))
	 (eol (save-excursion (end-of-line) (point)))
	 (bos (min eol (+ bol (fortran-current-line-indentation))))
	 (quote
	  (save-excursion
	    (goto-char bol)
	    (if (looking-at comment-line-start-skip)
		nil			; OK to break quotes on comment lines.
	      (move-to-column fill-column)
	      (cond ((is-in-fortran-string-p (point))
		     (save-excursion (re-search-backward "[^']'[^']" bol t)
				     (if fortran-break-before-delimiters
					 (point)
				       (1+ (point)))))
		    (t nil)))))
	 ;;
	 ;; decide where to split the line. If a position for a quoted
	 ;; string was found above then use that, else break the line
	 ;; before the last delimiter.
	 ;; Delimeters are whitespace, commas, and operators.
	 ;; Will break before a pair of *'s.
	 ;;
	 (fill-point
	  (or quote
	      (save-excursion
		(move-to-column (1+ fill-column))
		(skip-chars-backward "^ \t\n,'+-/*=)"
;;;		 (if fortran-break-before-delimiters
;;;		     "^ \t\n,'+-/*=" "^ \t\n,'+-/*=)")
		 )
		(if (<= (point) (1+ bos))
		    (progn
		      (move-to-column (1+ fill-column))
		      (if (not (re-search-forward "[\t\n,'+-/*)=]" eol t))
			  (goto-char bol))))
		(if (bolp)
		    (re-search-forward "[ \t]" opoint t)
		  (forward-char -1)
		  (if (looking-at "'")
		      (forward-char 1)
		    (skip-chars-backward " \t\*")))
		(if fortran-break-before-delimiters
		    (point)
		  (1+ (point))))))
	 )
    ;; if we are in an in-line comment, don't break unless the
    ;; line of code is longer than it should be. Otherwise
    ;; break the line at the column computed above.
    ;;
    ;; Need to use find-comment-start-skip to make sure that quoted !'s
    ;; don't prevent a break.
    (if (not (or (save-excursion
		   (if (and (re-search-backward comment-start-skip bol t)
			    (not (is-in-fortran-string-p (point))))
		       (progn
			 (skip-chars-backward " \t")
			 (< (current-column) (1+ fill-column)))))
		 (save-excursion
		   (goto-char fill-point)
		   (bolp))))
	(if (> (save-excursion
		 (goto-char fill-point) (current-column))
	       (1+ fill-column))
	    (progn (goto-char fill-point)
		   (fortran-break-line))
	  (save-excursion
	    (if (> (save-excursion
		     (goto-char fill-point) 
		     (current-column))
		   (+ (calculate-fortran-indent) fortran-continuation-indent))
		(progn
		  (goto-char fill-point)
		  (fortran-break-line))))))
    ))
(defun fortran-break-line ()
  (let ((opoint (point))
	(bol (save-excursion (beginning-of-line) (point)))
	(eol (save-excursion (end-of-line) (point)))
	(comment-string nil))
    
    (save-excursion
      (if (and comment-start-skip (find-comment-start-skip))
	  (progn
	    (re-search-backward comment-start-skip bol t)
	    (setq comment-string (buffer-substring (point) eol))
	    (delete-region (point) eol))))
;;; Forward line 1 really needs to go to next non white line
    (if (save-excursion (forward-line 1)
			(or (looking-at "     [^ 0\n]")
			    (looking-at "\t[1-9]")))
	(progn
	  (forward-line 1)
	  (delete-indentation)
	  (delete-char 2)
	  (delete-horizontal-space)
	  (fortran-do-auto-fill))
      (fortran-split-line))
    (if comment-string
	(save-excursion
	  (goto-char bol)
	  (end-of-line)
	  (delete-horizontal-space)
	  (indent-to (fortran-comment-hook))
	  (insert comment-string)))))

(provide 'fortran)

;;; fortran.el ends here
