;;; ediff-init.el --- Macros, variables and defsubsts used by Ediff
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>

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


;; Is it XEmacs?
(defconst ediff-xemacs-p (string-match "\\(Lucid\\|Xemacs\\)" emacs-version))
;; Is it Emacs?
(defconst ediff-emacs-p (not ediff-xemacs-p))
;; Are we running as a window application or on a TTY?
(defsubst ediff-device-type ()
  (if ediff-emacs-p
      window-system
    (device-type (selected-device))))
;; in XEmacs: device-type is tty on tty and stream in batch.
(defsubst ediff-window-display-p ()
  (and (ediff-device-type) (not (memq (ediff-device-type) '(tty stream)))))

;;; Macros
(defmacro ediff-odd-p (arg)
  (` (eq (logand (, arg) 1) 1)))

(defmacro ediff-buffer-live-p (buf)
  (` (and (, buf) (get-buffer (, buf)) (buffer-name (get-buffer (, buf))))))

(defmacro ediff-get-buffer (arg)
  (` (cond ((eq (, arg) 'A) ediff-buffer-A)
	   ((eq (, arg) 'B) ediff-buffer-B)
	   ((eq (, arg) 'C) ediff-buffer-C)
	   )
  ))
  
(defmacro ediff-get-value-according-to-buffer-type (buf-type list)
  (` (cond ((eq (, buf-type) 'A) (nth 0 (, list)))
	   ((eq (, buf-type) 'B) (nth 1 (, list)))
	   ((eq (, buf-type) 'C) (nth 2 (, list))))))
	   
(defmacro ediff-char-to-buftype (arg)
  (` (cond ((memq (, arg) '(?a ?A)) 'A)
	   ((memq (, arg) '(?b ?B)) 'B)
	   ((memq (, arg) '(?c ?C)) 'C)
	   )
  ))
  
(defmacro ediff-get-difference (n buf-type)
  (` (aref
      (symbol-value
       (intern (format "ediff-difference-vector-%S" (, buf-type)))) (, n))))
  
;; tell if it has been previously determined that the region has
;; no diffs other than the white space and newlines
;; The argument, N, is the diff region number used by Ediff to index the
;; diff vector. It is 1 less than the number seen by the user.
;;
;; A difference vector has the form:
;; [diff diff diff ...]
;; where each diff has the form:
;; [overlay fine-diff-vector no-fine-diffs-flag]
;; fine-diff-vector is a vector [fine-diff fine-diff fine-diff ...]
(defmacro ediff-no-fine-diffs-p (n)
  (` (aref (ediff-get-difference (, n) 'A) 2)))
  
(defmacro ediff-get-diff-overlay-from-diff-record (diff-rec)
  (` (aref (, diff-rec) 0)))
  
(defmacro ediff-get-diff-overlay (n buf-type)  
  (` (ediff-get-diff-overlay-from-diff-record
      (ediff-get-difference (, n) (, buf-type)))))

(defmacro ediff-get-fine-diff-vector-from-diff-record (diff-rec)
  (` (aref (, diff-rec) 1)))
      
(defmacro ediff-set-fine-diff-vector (n buf-type fine-vec)
  (` (aset (ediff-get-difference (, n) (, buf-type)) 1 (, fine-vec))))
  
(defmacro ediff-get-state-of-diff (n buf-type)
  (` (if (ediff-buffer-live-p ediff-buffer-C)
	 (aref (ediff-get-difference (, n) (, buf-type)) 3))))
(defmacro ediff-set-state-of-diff (n buf-type val)
  (` (aset (ediff-get-difference (, n) (, buf-type)) 3 (, val))))
(defmacro ediff-get-state-of-merge (n)
  (` (if (ediff-buffer-live-p ediff-buffer-C)
	 (aref (ediff-get-difference (, n) 'C) 4))))
(defmacro ediff-set-state-of-merge (n val)
  (` (aset (ediff-get-difference (, n) 'C) 4 (, val))))

;; if flag is t, puts a mark on diff region saying that 
;; the differences are in white space only. If flag is nil,
;; the region is marked as essential (i.e., differences are
;; not just in the white space and newlines.)
(defmacro ediff-mark-diff-as-space-only (n flag)
  (` (aset (ediff-get-difference (, n) 'A) 2 (, flag))))
  
(defmacro ediff-get-fine-diff-vector (n buf-type)
  (` (ediff-get-fine-diff-vector-from-diff-record
      (ediff-get-difference (, n) (, buf-type)))))
  
  
;; Defines SYMBOL as an advertised local variable.  
;; Performs a defvar, then executes `make-variable-buffer-local' on
;; the variable.  Also sets the `permanent-local' property,
;; so that `kill-all-local-variables' (called by major-mode setting
;; commands) won't destroy Ediff control variables.
;; 
;; Plagiarised from `emerge-defvar-local' for XEmacs.
(defmacro ediff-defvar-local (var value doc) 
  (` (progn
       (defvar (, var) (, value) (, doc))
       (make-variable-buffer-local '(, var))
       (put '(, var) 'permanent-local t))))
    
;; Macro to switch to BUFFER, evaluate FORMS, returns to original buffer.
;; Differs from `save-excursion' in that it doesn't save the point and mark.
;; This is essentially `emerge-eval-in-buffer' with the test for live buffers."
(defmacro ediff-eval-in-buffer (buffer &rest forms)
  (` (let ((StartBuffer (current-buffer)))
       (if (ediff-buffer-live-p (, buffer))
	   (unwind-protect
	       (progn
		 (set-buffer (, buffer))
		 (,@ forms))
	     (set-buffer StartBuffer))
	 (or (eq this-command 'ediff-quit)
	     (error
	      "You've killed an essential Ediff buffer---Please quit Ediff"))
	 ))))
	 

(defmacro ediff-multiframe-setup-p ()
  (` (and (ediff-window-display-p) ediff-multiframe)))
			 
(defmacro ediff-narrow-control-frame-p ()
  (` (and (ediff-multiframe-setup-p)
	  (equal ediff-help-message ediff-brief-message-string))))
	  
(defmacro ediff-3way-comparison-job ()
  (` (memq
      ediff-job-name
      '(ediff-files3 ediff-buffers3))))
(ediff-defvar-local ediff-3way-comparison-job nil "")
      
(defmacro ediff-merge-job ()
  (` (memq
      ediff-job-name
      '(ediff-merge-files
	ediff-merge-buffers
	ediff-merge-files-with-ancestor
	ediff-merge-buffers-with-ancestor
	ediff-merge-revisions
	ediff-merge-revisions-with-ancestor))))
(ediff-defvar-local ediff-merge-job nil "")

(defmacro ediff-merge-with-ancestor-job ()
  (` (memq
      ediff-job-name
      '(ediff-merge-files-with-ancestor
	ediff-merge-buffers-with-ancestor
	ediff-merge-revisions-with-ancestor))))
(ediff-defvar-local ediff-merge-with-ancestor-job nil "")

(defmacro ediff-3way-job ()
  (` (or ediff-3way-comparison-job ediff-merge-job)))
(ediff-defvar-local ediff-3way-job nil "")

;; A diff3 job is like a 3way job, but ediff-merge doesn't require the use
;; of diff3.
(defmacro ediff-diff3-job ()
  (` (or ediff-3way-comparison-job
	 ediff-merge-with-ancestor-job)))
(ediff-defvar-local ediff-diff3-job nil "")
	 
(defmacro ediff-windows-job ()
  (` (memq ediff-job-name '(ediff-windows-wordwise ediff-windows-linewise))))
(ediff-defvar-local ediff-windows-job nil "")

(defmacro ediff-word-mode-job ()
  (` (memq ediff-job-name '(ediff-windows-wordwise  ediff-regions-wordwise))))
(ediff-defvar-local ediff-word-mode-job nil "")

(defmacro ediff-narrow-job ()
  (` (memq ediff-job-name '(ediff-windows-wordwise
			    ediff-regions-wordwise
			    ediff-windows-linewise
			    ediff-regions-linewise))))
(ediff-defvar-local ediff-narrow-job nil "")


;; Hook variables

(defvar ediff-before-setup-windows-hooks nil
  "*Hooks to run before Ediff sets its own window config. 
This can be used to save the previous window config, which can be restored
on ediff-quit or ediff-suspend.") 
(defvar ediff-after-setup-windows-hooks nil
  "*Hooks to run after Ediff sets its own window config. 
This can be used to set up control window or icon in a desired place.")
(defvar ediff-before-setup-control-frame-hooks nil
  "*Hooks run before setting up the frame to display Ediff Control Panel.
Can be used to change control frame parameters to position it where it
is desirable.")
(defvar ediff-after-setup-control-frame-hooks nil
  "*Hooks run after setting up the frame to display Ediff Control Panel.
Can be used to move the frame where it is desired.")
(defvar ediff-startup-hooks nil
  "*Hooks to run in the control buffer after Ediff has been set up.")
(defvar ediff-select-hooks nil
  "*Hooks to run after a difference has been selected.")
(defvar ediff-unselect-hooks nil
  "*Hooks to run after a difference has been unselected.")
(defvar ediff-prepare-buffer-hooks  nil
  "*Hooks called after buffers A, B, and C are set up.")
(defvar ediff-load-hooks nil
  "*Hook run after Ediff is loaded.  Can be used to change defaults.")
  
(defvar ediff-mode-hooks nil
  "*Hook run just after ediff-mode is set up in the control buffer. 
This is done before any windows or frames are created. One can use it to
set local variables that determine how the display looks like.")
(defvar ediff-keymap-setup-hooks nil
  "*Hook run just after the default bindings in Ediff keymap are set up.")
  
(defvar ediff-display-help-hooks nil
  "*Hooks run after preparing the help message.")

(defvar ediff-suspend-hooks (list 'ediff-default-suspend-hook)
  "*Hooks to run in the Ediff control buffer when Ediff is suspended.")
(defvar ediff-quit-hooks (list 'ediff-cleanup-mess)
  "*Hooks to run in the Ediff control buffer after finishing Ediff.") 
(defvar ediff-cleanup-hooks nil
  "*Hooks to run on exiting Ediff but before killing the control buffer.
This is a place to do various cleanups, such as deleting the variant buffers.
Ediff provides a function, `ediff-janitor', as one such possible hook.")


;; Help messages

(defconst ediff-long-help-message-head
  "   Moving around     |     Toggling features     |       Manipulations
=====================|===========================|============================="
  "The head of the full help message.")
(defconst ediff-long-help-message-tail
  "=====================|===========================|=============================
                     |     D -on-line manual     |
    i -status info   |     ? -help off           |  G -send bug report   
-------------------------------------------------------------------------------
X,Y (x,y)  on the left are meta-symbols for the keys  A,B,C (a,b,c).
X,Y on the right are meta-symbols for buffers A,B,C.
A,B,C on the right denote the working buffers A,B,C, respectively."
  "The tail of the full-help message.")

(defconst ediff-long-help-message-compare3
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -hiliting           | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|                           |  ! -update diff regions
  C-l -recenter      |    ## -ignore whitespace  |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
  z/q -suspend/quit  |     m -wide display       |  ~ -rotate buffers
"
  "Help message usually used for 3-way comparison.
Normally, not a user option. See `ediff-help-message' for details.")
  
(defconst ediff-long-help-message-compare2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -hiliting           | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|                           |  ! -update diff regions
  C-l -recenter      |    ## -ignore whitespace  |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
  z/q -suspend/quit  |     m -wide display       |  ~ -swap buffers
"
  "Help message usually used for 2-way comparison.
Normally, not a user option. See `ediff-help-message' for details.")
  
(defconst ediff-long-help-message-narrow2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -hiliting           | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|     % -narrow/widen buffs |  ! -update diff regions
  C-l -recenter      |    ## -ignore whitespace  |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
  z/q -suspend/quit  |     m -wide display       |  ~ -swap buffers
"
  "Help message when comparing windows or regions line-by-line.
Normally, not a user option. See `ediff-help-message' for details.")
  
(defconst ediff-long-help-message-word-mode
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -hiliting           | rx -restore buf X's old diff
    j -jump to diff  |                           |                   
   gx -goto X's point|     % -narrow/widen buffs |  ! -recompute diffs
  C-l -recenter      |                           |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
  z/q -suspend/quit  |     m -wide display       |  ~ -swap buffers
"
  "Help message when comparing windows or regions word-by-word.
Normally, not a user option. See `ediff-help-message' for details.")
  
(defconst ediff-long-help-message-merge
  "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -hiliting           |  r -restore buf C's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      | #f/#h -focus/hide regions |  + -combine diff regions
  v/V -scroll up/dn  |     X -read-only in buf X | wx -save buf X
  </> -scroll lt/rt  |     m -wide display       | wd -save diff output
  z/q -suspend/quit  |     s -shrink window C    |  ~ -swap buffers
                     |     $ -show clashes only  |  & -merge w/new default
"
  "Help message during merging.
Normally, not a user option. See `ediff-help-message' for details.")

;; The actual long help message.
(ediff-defvar-local ediff-long-help-message ""
  "Normally, not a user option. See `ediff-help-message' for details.")
  
(defconst ediff-brief-message-string
  "  ? - help  "
  "Contents of the brief help message.")
;; The actual brief help message
(ediff-defvar-local ediff-brief-help-message ""
  "Normally, not a user option. See `ediff-help-message' for details.")
  
(ediff-defvar-local ediff-brief-help-message-custom nil
  "The brief help message that the user can customize.
If the user sets this to a parameter-less function, Ediff will use it to
produce the brief help message. This function must return a string.")
(ediff-defvar-local ediff-long-help-message-custom nil
  "The long help message that the user can customize.
See `ediff-brief-help-message-custom' for more.")

(defvar ediff-prefer-long-help-message nil
  "*If t, Ediff starts with a long help message. Short help msg otherwise.")

;; The actual help message.
(ediff-defvar-local ediff-help-message ""
  "The actual help message.
Normally, the user shouldn't touch this. However, if you want Ediff to
start up with different help messages for different jobs, you can change
the value of this variable and the variables `ediff-help-message-*' in
`ediff-startup-hooks'.") 
 
;; Selective browsing

(ediff-defvar-local ediff-skip-diff-region-function 'ediff-show-all-diffs
  "Function that determines the next/previous diff region to show.
Should return t for regions to be ignored and nil otherwise.
This function gets a region number as an argument. The region number
is the one used internally by Ediff. It is 1 less than the number seen
by the user.")

;; Regexp that determines buf A regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-A "" "")
;; Regexp that determines buf B regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-B "" "")
;; Regexp that determines buf C regions to focus on when skipping to diff
(ediff-defvar-local ediff-regexp-focus-C "" "")
;; connective that determines whether to focus regions that match both or
;; one of the regexps
(ediff-defvar-local ediff-focus-regexp-connective 'and "")
  
;; Regexp that determines buf A regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-A "" "")
;; Regexp that determines buf B regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-B "" "")
;; Regexp that determines buf C regions to ignore when skipping to diff
(ediff-defvar-local ediff-regexp-hide-C "" "")
;; connective that determines whether to hide regions that match both or
;; one of the regexps
(ediff-defvar-local ediff-hide-regexp-connective 'and "")
  
  
(defvar ediff-ange-ftp-ftp-name (if ediff-xemacs-p
				    'ange-ftp-ftp-path
				  'ange-ftp-ftp-name)
  "Function ange-ftp uses to find out if file is remote.")
  

;; Copying difference regions between buffers.    
(ediff-defvar-local ediff-killed-diffs-alist nil
  "A list of killed diffs. 
A diff is saved here if it is replaced by a diff
from another buffer.  This alist has the form:
\((num (buff-object . diff) (buff-object . diff) (buff-object . diff)) ...),
where some buffer-objects may be missing.")


;; Highlighting
;;(defvar ediff-before-flag-bol (if ediff-emacs-p "->>\n" (make-glyph "->>\n"))
(defvar ediff-before-flag-bol (if ediff-xemacs-p (make-glyph "->>") "->>")
  "*Flag placed above the highlighted block of differences. 
Must end with newline.")
;;(defvar ediff-after-flag-eol  (if ediff-emacs-p "<<-\n" (make-glyph "<<-"))
(defvar ediff-after-flag-eol  (if ediff-xemacs-p (make-glyph "<<-") "<<-")
  "*Flag placed below the highlighted block of differences.
Must end with newline.")

(defvar ediff-before-flag-mol (if ediff-xemacs-p (make-glyph "->>") "->>")
  "*Like ediff-before-flag, used when a difference starts in mid-line.")
(defvar ediff-after-flag-mol  (if ediff-xemacs-p (make-glyph "<<-") "<<-")
  "*Like ediff-after-flag, used when a difference starts in mid-line.")

  
(ediff-defvar-local ediff-use-faces t 
  "If t, differences are highlighted using faces on a window system.
If nil, they are highlighted using ASCII flags, ediff-before-flag
and ediff-after-flag.  On a non-window system, differences are always
highlighted using ASCII flags.
This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")


;; this indicates that diff regions are word-size, so fine diffs are
;; permanently nixed; used in ediff-windows-wordwise and ediff-regions-wordwise
(ediff-defvar-local ediff-word-mode nil "")
;; Name of the job (ediff-files, ediff-windows, etc.
(ediff-defvar-local ediff-job-name nil "")

;; Narrowing and ediff-region/windows support
;; This is a list (overlay-A overlay-B overlay-C)
;; If set, Ediff compares only those parts of buffers A/B/C that lie within
;; the bounds of these overlays.
(ediff-defvar-local ediff-narrow-bounds nil "")

;; List (overlay-A overlay-B overlay-C), where each overlay spans the
;; entire corresponding buffer.
(ediff-defvar-local ediff-wide-bounds nil "")

;; Current visibility boundaries in buffers A, B, and C.
;; This is also a list of overlays. When the user toggles narrow/widen,
;; this list changes from ediff-wide-bounds to ediff-narrow-bounds.
;; and back.
(ediff-defvar-local ediff-visible-bounds nil "")

(ediff-defvar-local ediff-start-narrowed t
  "Non-nil means start narrowed, if doing ediff-windows-* or ediff-regions-*")
(ediff-defvar-local ediff-quit-widened t
  "*Non-nil means: when finished, Ediff widens buffers A/B.
Actually, Ediff restores the scope of visibility that existed at startup.")

(ediff-defvar-local ediff-highlight-all-diffs t
  "If nil, only the selected differences are highlighted.
This variable can be set either in .emacs or toggled interactively, using
ediff-toggle-hilit. Use `setq-default' to set it.") 

;; A var local to each control panel buffer.  Indicates highlighting style
;; in effect for this buffer: `face', `ascii', nil -- temporarily
;; unhighlighted, `off' -- turned off \(on a dumb terminal only\).
(ediff-defvar-local ediff-highlighting-style nil "")


;; Variables that control each Ediff session.  They are local to the
;; control buffer. 

;; Mode variables
;; The buffer in which the A variant is stored.
(ediff-defvar-local ediff-buffer-A nil "")
;; The buffer in which the B variant is stored.
(ediff-defvar-local ediff-buffer-B nil "")
;; The buffer in which the C variant is stored.
(ediff-defvar-local ediff-buffer-C nil "")
;; Ancestor buffer
(ediff-defvar-local ediff-ancestor-buffer nil "")
;; The control buffer of ediff.
(ediff-defvar-local ediff-control-buffer nil "")

  
;; The suffix of the control buffer name.
(ediff-defvar-local ediff-control-buffer-suffix nil "")
;; Same as ediff-control-buffer-suffix, but without <,>. 
;; It's a number rather than string.
(ediff-defvar-local ediff-control-buffer-number nil "")


;; The original values of ediff-protected-variables for buffer A
(ediff-defvar-local ediff-buffer-values-orig-A nil "")
;; The original values of ediff-protected-variables for buffer B
(ediff-defvar-local ediff-buffer-values-orig-B nil "")
;; The original values of ediff-protected-variables for buffer C
(ediff-defvar-local ediff-buffer-values-orig-C nil "")
;; Buffer-local variables to be saved then restored during Ediff sessions
(defconst ediff-protected-variables '(buffer-read-only 
				      mode-line-format))

;; Vector of differences between the variants.  Each difference is
;; represented by a vector of two overlays plus a vector of fine diffs,
;; plus a no-fine-diffs flag.  The first overlay spans the
;; difference region in the A buffer and the second overlays the diff in
;; the B buffer. If a difference section is empty, the corresponding
;; overlay's endpoints coincide.
;;
;; The precise form of a difference vector for one buffer is:
;; [diff diff diff ...]
;; where each diff has the form:
;; [diff-overlay fine-diff-vector no-fine-diffs-flag state-of-difference
;; state-of-merge]
;; fine-diff-vector is a vector [fine-diff-overlay fine-diff-overlay ...]
;; no-fine-diffs-flag says if there are fine differences.
;; state-of-difference is A, B, C, or nil, indicating which buffer is
;; different from the other two (used only in 3-way jobs.
;; state-of-merge is default-A, default-B, prefer-A, or prefer-B. It
;; indicates the way a diff region was created in buffer C.
(ediff-defvar-local ediff-difference-vector-A nil "")
(ediff-defvar-local ediff-difference-vector-B nil "")
(ediff-defvar-local ediff-difference-vector-C nil "")

;; The difference that is currently selected.
(ediff-defvar-local ediff-current-difference -1 "")
;; Number of differences found.
(ediff-defvar-local ediff-number-of-differences nil "")
  
;; Buffer containing the output of diff, which is used by Ediff to step
;; through files.
(ediff-defvar-local ediff-diff-buffer nil "")
;; Like ediff-diff-buffer, but contains context diff. It is not used by
;; Ediff, but it is saved in a file, if user requests so.
(ediff-defvar-local ediff-custom-diff-buffer nil "")
;; Buffer used for diff-style fine differences between regions.
(ediff-defvar-local ediff-fine-diff-buffer nil "")
;; Temporary buffer used for computing fine differences.
(defconst ediff-tmp-buffer " *ediff-tmp*" "")
;; Buffer used for messages
(defconst ediff-msg-buffer " *ediff-message*" "")
;; Buffer containing the output of diff when diff returns errors.
(ediff-defvar-local ediff-error-buffer nil "")
;; Buffer to display debug info
(ediff-defvar-local ediff-debug-buffer "*ediff-debug*" "")

;; List of ediff control panels associated with each buffer A/B.
;; Not used any more, but may be needed in the future
(ediff-defvar-local ediff-this-buffer-control-sessions  nil "")

;; to be deleted in due time
;; List of difference overlays disturbed by working with the current diff.
(defvar ediff-disturbed-overlays nil "")
  
;; Priority of non-selected overlays.
(defvar ediff-shadow-overlay-priority  100 "")


(if ediff-xemacs-p
    (progn
      (fset 'ediff-read-event (symbol-function 'next-command-event))
      (fset 'ediff-overlayp (symbol-function 'extentp))
      (fset 'ediff-make-overlay (symbol-function 'make-extent))
      (fset 'ediff-delete-overlay (symbol-function 'delete-extent))
      (fset 'ediff-overlay-buffer (symbol-function 'extent-buffer))
      (fset 'ediff-overlay-get (symbol-function 'extent-property)))
  (fset 'ediff-read-event (symbol-function 'read-event))
  (fset 'ediff-overlayp (symbol-function 'overlayp))
  (fset 'ediff-overlayp (symbol-function 'overlayp))
  (fset 'ediff-make-overlay (symbol-function 'make-overlay))
  (fset 'ediff-delete-overlay (symbol-function 'delete-overlay))
  (fset 'ediff-overlay-buffer (symbol-function 'overlay-buffer))
  (fset 'ediff-overlay-get (symbol-function 'overlay-get)))
  
;; Check the current version against the major and minor version numbers
;; using op: cur-vers op major.minor If emacs-major-version or
;; emacs-minor-version are not defined, we assume that the current version
;; is hopelessly outdated.  We assume that emacs-major-version and
;; emacs-minor-version are defined.  Otherwise, for Emacs/XEmacs 19, if the
;; current minor version is < 10 (xemacs) or < 23 (emacs) the return value
;; will be nil (when op is =, >, or >=) and t (when op is <, <=), which may be
;; incorrect. However, this gives correct result in our cases, since we are
;; testing for sufficiently high Emacs versions.
(defun ediff-check-version (op major minor &optional type-of-emacs)
  (if (and (boundp 'emacs-major-version) (boundp 'emacs-minor-version))
      (and (cond ((eq type-of-emacs 'xemacs) ediff-xemacs-p)
		 ((eq type-of-emacs 'emacs) ediff-emacs-p)
		 (t t))
	   (cond ((eq op '=) (and (= emacs-minor-version minor)
				  (= emacs-major-version major)))
		 ((memq op '(> >= < <=))
		  (and (or (funcall op emacs-major-version major)
			   (= emacs-major-version major))
		       (if (= emacs-major-version major)
			   (funcall op emacs-minor-version minor)
			 t)))
		 (t
		  (error "%S: Invalid op in ediff-check-version" op))))
    (cond ((memq op '(= > >=)) nil)
	  ((memq op '(< <=)) t))))
  
  
;; warn if it is a wrong emacs
(if (or (ediff-check-version '< 19 29 'emacs)
	(ediff-check-version '< 19 12 'xemacs))
    (progn
      (with-output-to-temp-buffer ediff-msg-buffer
	(switch-to-buffer ediff-msg-buffer)
	(insert
	 (format "

This version of Ediff requires 

\t Emacs 19.29 and higher
\t OR
\t XEmacs 19.12 and higher

It is unlikely to work under Emacs version %s
that you are using...

Type any key to continue..." emacs-version))
	(beep 1)
	(beep 1)
	(ediff-read-event))
      (kill-buffer ediff-msg-buffer)))

;; A fix for NeXT Step
;; Should probably be eliminated in later versions.
(if (and (ediff-window-display-p) (eq (ediff-device-type) 'ns))
    (progn
      (fset 'x-display-color-p (symbol-function 'ns-display-color-p))
      (fset 'x-color-defined-p (symbol-function 'ns-color-defined-p))
      (fset 'x-display-pixel-height (symbol-function 'ns-display-pixel-height))
      (fset 'x-display-pixel-width (symbol-function 'ns-display-pixel-width))
      ))

(defsubst ediff-color-display-p ()
  (if ediff-emacs-p
      (x-display-color-p)
    (eq (device-class (selected-device)) 'color)))
  
(if (ediff-window-display-p)
    (if ediff-xemacs-p
	(progn
	  (fset 'ediff-display-pixel-width
		(symbol-function 'device-pixel-width))
	  (fset 'ediff-display-pixel-height
		(symbol-function 'device-pixel-height))
	  (fset 'ediff-valid-color-p (symbol-function 'valid-color-name-p))
	  (fset 'ediff-get-face (symbol-function 'get-face)))
      ;; Temporary fix for OS/2 port of Emacs
      ;; pm-win.el in PM-Emacs should be fixed.
      (fset 'ediff-display-pixel-width 
	    (symbol-function 'x-display-pixel-width))
      (fset 'ediff-display-pixel-height
	    (symbol-function 'x-display-pixel-height))
      (if (eq (ediff-device-type) 'pm)
	  (fset 'ediff-valid-color-p 
		(function (lambda (color) (assoc color pm-color-alist))))
	(fset 'ediff-valid-color-p (symbol-function 'x-color-defined-p)))
      (fset 'ediff-get-face (symbol-function 'internal-get-face))))
      

(if (not (ediff-window-display-p))
    (fset 'ediff-make-current-diff-overlay (function (lambda (type) nil)))
  ;; window system
  (defun ediff-set-face (ground face color)
    "Sets face foreground/background."
    (if (ediff-valid-color-p color)
	(if (eq ground 'foreground)
	    (set-face-foreground face color)
	  (set-face-background face color))
      (cond ((memq face
		   '(ediff-current-diff-face-A
		     ediff-current-diff-face-B
		     ediff-current-diff-face-C))
	     (copy-face 'highlight face))
	    ((memq face
		   '(ediff-fine-diff-face-A
		     ediff-fine-diff-face-B
		     ediff-fine-diff-face-C))
	     (copy-face 'secondary-selection face)
	     (set-face-underline-p face t))
	    ((memq face
		   '(ediff-even-diff-face-A ediff-odd-diff-face-A 
		     ediff-even-diff-face-B ediff-odd-diff-face-B
		     ediff-even-diff-face-C ediff-odd-diff-face-C))
	     (copy-face 'secondary-selection face)))))
      
  (defvar ediff-current-diff-face-A
    (progn
      (make-face 'ediff-current-diff-face-A)
      (or (face-differs-from-default-p 'ediff-current-diff-face-A)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-current-diff-face-A "firebrick")
		 (ediff-set-face
		  'background 'ediff-current-diff-face-A "pale green"))
		(t
		 (if ediff-xemacs-p
		     (copy-face 'modeline 'ediff-current-diff-face-A)
		   (copy-face 'highlight 'ediff-current-diff-face-A))
		 )))
      'ediff-current-diff-face-A)
    "Face for highlighting the selected difference in buffer A.")

  (defvar ediff-current-diff-face-B
    (progn
      (make-face 'ediff-current-diff-face-B)
      (or (face-differs-from-default-p 'ediff-current-diff-face-B)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-current-diff-face-B "DarkOrchid")
		 (ediff-set-face
		  'background 'ediff-current-diff-face-B "Yellow"))
		(t 
		 (if ediff-xemacs-p
		     (copy-face 'modeline 'ediff-current-diff-face-B)
		   (copy-face 'highlight 'ediff-current-diff-face-B))
		 )))
      'ediff-current-diff-face-B)
    "Face for highlighting the selected difference in buffer B.")
    

  (defvar ediff-current-diff-face-C
    (progn
      (make-face 'ediff-current-diff-face-C)
      (or (face-differs-from-default-p 'ediff-current-diff-face-C)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-current-diff-face-C "Navy")
		 (ediff-set-face
		  'background 'ediff-current-diff-face-C "Pink"))
		(t 
		 (if ediff-xemacs-p
		     (copy-face 'modeline 'ediff-current-diff-face-C)
		   (copy-face 'highlight 'ediff-current-diff-face-C))
		 )))
      'ediff-current-diff-face-C)
    "Face for highlighting the selected difference in buffer C.")

  (defvar ediff-fine-diff-face-A
    (progn
      (make-face 'ediff-fine-diff-face-A)
      (or (face-differs-from-default-p 'ediff-fine-diff-face-A)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face 'foreground 'ediff-fine-diff-face-A
				 "Navy")
		 (ediff-set-face 'background 'ediff-fine-diff-face-A
				 "sky blue"))
		(t (set-face-underline-p 'ediff-fine-diff-face-A t))))
      'ediff-fine-diff-face-A)
    "Face for highlighting the refinement of the selected diff in buffer A.")

  (defvar ediff-fine-diff-face-B
    (progn
      (make-face 'ediff-fine-diff-face-B)
      (or (face-differs-from-default-p 'ediff-fine-diff-face-B)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face 'foreground 'ediff-fine-diff-face-B "Black")
		 (ediff-set-face 'background 'ediff-fine-diff-face-B "cyan"))
		(t (set-face-underline-p 'ediff-fine-diff-face-B t))))
      'ediff-fine-diff-face-B)
    "Face for highlighting the refinement of the selected diff in buffer B.")
    
  (defvar ediff-fine-diff-face-C
    (progn
      (make-face 'ediff-fine-diff-face-C)
      (or (face-differs-from-default-p 'ediff-fine-diff-face-C)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face 'foreground 'ediff-fine-diff-face-C "black")
		 (ediff-set-face
		  'background 'ediff-fine-diff-face-C "Turquoise"))
		(t (set-face-underline-p 'ediff-fine-diff-face-C t))))
      'ediff-fine-diff-face-C)
    "Face for highlighting the refinement of the selected diff in buffer C.")
    
  (defvar ediff-even-diff-face-A
    (progn
      (make-face 'ediff-even-diff-face-A)
      (or (face-differs-from-default-p 'ediff-even-diff-face-A)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-even-diff-face-A "black")
		 (ediff-set-face
		  'background 'ediff-even-diff-face-A "light grey"))
		(t 
		 (copy-face 'italic 'ediff-even-diff-face-A))))
      'ediff-even-diff-face-A)
    "Face used to highlight even-numbered differences in buffer A.")
      
  (defvar ediff-even-diff-face-B
    (progn
      (make-face 'ediff-even-diff-face-B)
      (or (face-differs-from-default-p 'ediff-even-diff-face-B)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-even-diff-face-B "White")
		 (ediff-set-face
		  'background 'ediff-even-diff-face-B "Gray"))
		(t 
		 (copy-face 'italic 'ediff-even-diff-face-B))))
      'ediff-even-diff-face-B)
    "Face used to highlight even-numbered differences in buffer B.")
    
  (defvar ediff-even-diff-face-C
    (progn
      (make-face 'ediff-even-diff-face-C)
      (copy-face 'ediff-even-diff-face-A 'ediff-even-diff-face-C)
      'ediff-even-diff-face-C)
    "Face used to highlight even-numbered differences in buffer C.")
  
  (defvar ediff-odd-diff-face-A
    (progn
      (make-face 'ediff-odd-diff-face-A)
      (or (face-differs-from-default-p 'ediff-odd-diff-face-A)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-odd-diff-face-A "White")
		 (ediff-set-face
		  'background 'ediff-odd-diff-face-A "Gray"))
		(t 
		 (copy-face 'italic 'ediff-odd-diff-face-A))))
      'ediff-odd-diff-face-A)
    "Face used to highlight odd-numbered differences in buffer A.")
      
  (defvar ediff-odd-diff-face-B
    (progn
      (make-face 'ediff-odd-diff-face-B)
      (or (face-differs-from-default-p 'ediff-odd-diff-face-B)
	  (cond ((ediff-color-display-p)
		 (ediff-set-face
		  'foreground 'ediff-odd-diff-face-B "Black")
		 (ediff-set-face
		  'background 'ediff-odd-diff-face-B "light grey"))
		(t 
		 (copy-face 'italic 'ediff-odd-diff-face-B))))
      'ediff-odd-diff-face-B)
    "Face used to highlight odd-numbered differences in buffer B.")
    
  (defvar ediff-odd-diff-face-C
    (progn
      (make-face 'ediff-odd-diff-face-C)
      (copy-face 'ediff-odd-diff-face-A 'ediff-odd-diff-face-C)
      'ediff-odd-diff-face-C)
    "Face used to highlight odd-numbered differences in buffer C.")

  ;; Create *-var faces. These are the actual faces used to highlight
  ;; odd-numbered difference regions.
  ;; They are used as follows: when highlighting is turned on,
  ;; ediff-odd/even-diff-face-A/B/C are copied
  ;; into ediff-odd/even-diff-face-A/B/C-var, and all odd/even overlays become
  ;; highlighted. When highlighting is turned off, then the face 'default is
  ;; copied into ediff-odd/even-diff-face-A/B/C-var, thereby unhighlighting all
  ;; difference regions.
  (make-face 'ediff-even-diff-face-A-var)
  (make-face 'ediff-even-diff-face-B-var)
  (make-face 'ediff-even-diff-face-C-var)
  (make-face 'ediff-odd-diff-face-A-var)
  (make-face 'ediff-odd-diff-face-B-var)
  (make-face 'ediff-odd-diff-face-C-var)
  
  ;; initialize *-var faces
  (defun ediff-init-var-faces ()
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-even-diff-face-A 'default)
	       'ediff-even-diff-face-A-var)
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-even-diff-face-B 'default)
	       'ediff-even-diff-face-B-var)
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-even-diff-face-C 'default)
	       'ediff-even-diff-face-C-var)
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-odd-diff-face-A 'default)
	       'ediff-odd-diff-face-A-var)
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-odd-diff-face-B 'default)
	       'ediff-odd-diff-face-B-var)
    (copy-face (if (and ediff-use-faces ediff-highlight-all-diffs)
		   ediff-odd-diff-face-C 'default)
	       'ediff-odd-diff-face-C-var)
    )
      

;;; Overlays

  (ediff-defvar-local ediff-current-diff-overlay-A nil
    "Overlay for the current difference region in buffer A.")
  (ediff-defvar-local ediff-current-diff-overlay-B nil
    "Overlay for the current difference region in buffer B.")
  (ediff-defvar-local ediff-current-diff-overlay-C nil
    "Overlay for the current difference region in buffer C.")
  
  (defun ediff-make-current-diff-overlay (type)
    (let ((overlay (intern (format "ediff-current-diff-overlay-%S" type)))
	  (buffer (ediff-get-buffer type))
	  (face (face-name
		 (symbol-value
		  (intern (format "ediff-current-diff-face-%S" type))))))
      (set overlay
	   (ediff-make-bullet-proof-overlay (point-max) (point-max) buffer))
      (ediff-overlay-put (symbol-value overlay) 'face face)
      (ediff-overlay-put (symbol-value overlay) 'ediff ediff-control-buffer)
      ))
	
  ;; Compute priority of ediff overlay.
  (defun ediff-highest-priority (start end buffer)
    (let ((pos (max 1 (1- start)))
	  ovr-list)
      (if ediff-xemacs-p
	  (1+ ediff-shadow-overlay-priority)
	(ediff-eval-in-buffer buffer
	  (while (< pos (min (point-max) (1+ end)))
	    (setq ovr-list (append (overlays-at pos) ovr-list))
	    (setq pos (next-overlay-change pos)))
	  (1+ (apply '+
		     (mapcar (function
			      (lambda (ovr)
				(if ovr
				    (or (ediff-overlay-get ovr 'priority) 0)
				  0)))
			     ovr-list)
		     ))
	  ))))
  )
  
    
(defvar ediff-toggle-read-only-function nil
  "*Specifies the function to be used to toggle read-only.
If nil, Ediff tries to deduce the function from the binding of C-x C-q.
Normally, this is the `toggle-read-only' function, but, if version
control is used, it could be `vc-toggle-read-only' or `rcs-toggle-read-only'.")


;;; Misc

;; if nil, this silences some messages
(defconst ediff-verbose-p t)
  
(defvar ediff-no-emacs-help-in-control-buffer nil
  "*Non-nil means C-h should not invoke Emacs help in control buffer.
Instead, C-h jumps to previous difference.")
  
(defvar ediff-temp-file-prefix
  (let ((env (or (getenv "TMPDIR")
		 (getenv "TMP")
		 (getenv "TEMP")))
	d)
    (setq d (if (and env (> (length env) 0))
		env
	      (if (memq system-type '(vax-vms axp-vms))
		  "SYS$SCRATCH:"
		"/tmp")))
    ;; The following is to make sure we get something to which we can
    ;; add directory levels on VMS.
    (setq d (file-name-as-directory (directory-file-name d)))
    )
  "*Prefix to put on Ediff temporary file names.
Do not start with `~/' or `~user-name/'.")  

(defvar ediff-temp-file-mode 384	; u=rw only
  "*Mode for Ediff temporary files.")
  
;; Metacharacters that have to be protected from the shell when executing
;; a diff/diff3 command.
(defvar ediff-metachars "[ \t\n!\"#$&'()*;<=>?[\\^`{|~]"
  "Characters that must be quoted with \\ when used in a shell command line.
More precisely, a regexp to match any one such character.")

  
(ediff-defvar-local ediff-temp-file-A nil
  "Temporary file used for refining difference regions in buffer A.")
(ediff-defvar-local ediff-temp-file-B nil
  "Temporary file used for refining difference regions in buffer B.")
(ediff-defvar-local ediff-temp-file-C nil
  "Temporary file used for refining difference regions in buffer C.")

;;; In-line functions

(defsubst ediff-file-remote-p (file-name)
  (if (fboundp ediff-ange-ftp-ftp-name)
      (funcall ediff-ange-ftp-ftp-name file-name)))

;;(defun ediff-frame-has-menubar ()
;;  (and (ediff-window-display-p)
;;       (if ediff-xemacs-p
;;	   current-menubar
;;	 (let ((lines (cdr (assq 'menu-bar-lines
;;				 (frame-parameters (selected-frame))))))
;;	   (or (eq lines t) (and (numberp lines) (< 0 lines))))
;;	 )))
    
(defsubst ediff-frame-unsplittable-p (frame)
  (cdr (assq 'unsplittable (frame-parameters frame))))

(defsubst ediff-get-next-window (wind prev-wind)
  (or (window-live-p wind)
      (setq wind (if prev-wind
		     (next-window wind)
		   (selected-window)))))

(defsubst ediff-convert-diffs-to-overlays (diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'A diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'B diff-list)
  (if ediff-3way-job
      (ediff-set-diff-overlays-in-one-buffer 'C diff-list))
  (message "Processing difference regions ... done"))
  

(defsubst ediff-kill-buffer-carefully (buf)
  "Kill buffer BUF if it exists."
  (if (ediff-buffer-live-p buf)
      (kill-buffer (get-buffer buf))))
      

(defsubst ediff-highlight-diff (n)
  "Put face on diff N.  Invoked for X displays only."
  (ediff-highlight-diff-in-one-buffer n 'A)
  (ediff-highlight-diff-in-one-buffer n 'B)
  (ediff-highlight-diff-in-one-buffer n 'C))


(defsubst ediff-unhighlight-diff ()
  "Remove overlays from buffers A, B, and C."
  (ediff-unhighlight-diff-in-one-buffer 'A)
  (ediff-unhighlight-diff-in-one-buffer 'B)
  (ediff-unhighlight-diff-in-one-buffer 'C))
    
;; delete highlighting overlays, restore faces to their original form
(defsubst ediff-unhighlight-diffs-totally ()
  (ediff-unhighlight-diffs-totally-in-one-buffer 'A)
  (ediff-unhighlight-diffs-totally-in-one-buffer 'B)
  (ediff-unhighlight-diffs-totally-in-one-buffer 'C))
    
      
;; arg is a record for a given diff in a difference vector
;; this record is itself a vector
(defsubst ediff-clear-fine-diff-vector (diff-record)
  (if diff-record
      (mapcar 'ediff-delete-overlay
	      (ediff-get-fine-diff-vector-from-diff-record diff-record))))
	      
(defsubst ediff-clear-fine-differences-in-one-buffer (n buf-type)
  (ediff-clear-fine-diff-vector (ediff-get-difference n buf-type))
  (ediff-set-fine-diff-vector n buf-type nil))
  
(defsubst ediff-clear-fine-differences (n)
  (ediff-clear-fine-differences-in-one-buffer n 'A)
  (ediff-clear-fine-differences-in-one-buffer n 'B)
  (if ediff-3way-job
      (ediff-clear-fine-differences-in-one-buffer n 'C)))
	  

(defsubst ediff-convert-fine-diffs-to-overlays (diff-list region-num)
  (ediff-set-fine-overlays-in-one-buffer 'A diff-list region-num)
  (ediff-set-fine-overlays-in-one-buffer 'B diff-list region-num)
  (if ediff-3way-job
      (ediff-set-fine-overlays-in-one-buffer 'C diff-list region-num)
    ))
  
(defsubst ediff-mouse-event-p (event)
  (if ediff-xemacs-p
      (button-event-p event)
    (string-match "mouse" (format "%S" (event-basic-type event)))
    ))
    
(defsubst ediff-frame-iconified-p (frame)
  (if (and (ediff-window-display-p) (frame-live-p frame))
      (if ediff-xemacs-p
	  (frame-iconified-p frame)
	(eq (frame-visible-p frame) 'icon))))
    
(defsubst ediff-window-visible-p (wind)
  ;; under TTY, window-live-p also means window is visible
  (and (window-live-p wind)
       (or (not (ediff-window-display-p))
	   (frame-visible-p (window-frame wind)))))
  

(defsubst ediff-frame-char-width (frame)
  (if ediff-xemacs-p
      (/ (frame-pixel-width frame) (frame-width frame))
    (frame-char-width frame)))
    
(defsubst ediff-reset-mouse (&optional frame)
  (setq frame 
	(if ediff-xemacs-p
	    (frame-selected-window (or frame (selected-frame)))
	  (or frame (selected-frame))))
  (if (ediff-window-display-p)
      (set-mouse-position frame 1 0)))
    
(defsubst ediff-frame-char-height (frame)
  (if ediff-xemacs-p
      (/ (frame-pixel-height frame) (frame-height frame))
    (frame-char-height frame)))
    

(defsubst ediff-empty-overlay-p (overl)
  (= (ediff-overlay-start overl) (ediff-overlay-end overl)))
  
;; t if diff region is empty.
;; In case of buffer C, t also if it is not a 3way
;; comparison job (merging jobs return t as well).
(defun ediff-empty-diff-region-p (n buf-type)
  (if (eq buf-type 'C)
      (or (not ediff-3way-comparison-job)
	  (= (ediff-get-diff-posn 'C 'beg n)
	     (ediff-get-diff-posn 'C 'end n)))
    (= (ediff-get-diff-posn buf-type 'beg n)
       (ediff-get-diff-posn buf-type 'end n))))
       
;; Test if diff region is white space only.
;; If 2-way job and buf-type = C, then returns t.
(defun ediff-whitespace-diff-region-p (n buf-type)
  (or (and (eq buf-type 'C) (not ediff-3way-job))
      (let ((beg (ediff-get-diff-posn buf-type 'beg n))
	    (end (ediff-get-diff-posn buf-type 'end n)))
	(ediff-eval-in-buffer (ediff-get-buffer buf-type)
	  (save-excursion
	    (goto-char beg)
	    (skip-chars-forward ediff-whitespace)
	    (>= (point) end))))))
	    
;; temporarily uses DIR to abbreviate file name
;; if DIR is nil, use default-directory
(defsubst ediff-abbreviate-filename (file dir)
  (let* ((dir (or dir default-directory))
	 (directory-abbrev-alist (list (cons dir ""))))
    (abbreviate-file-name file)))
    
(defsubst ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  (ediff-eval-in-buffer 
      (ediff-eval-in-buffer ctrl-buf (ediff-get-buffer buf-type))
    (buffer-substring
     (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
     (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf)))))
  
;; If ediff modified mode line, strip the modification
(defsubst ediff-strip-mode-line-format ()
  (if (member (car mode-line-format) '(" A: " " B: " " C: "))
      (setq mode-line-format (nth 2 mode-line-format))))

;; Verify that we have a difference selected.
(defsubst ediff-valid-difference-p (&optional n)
  (or n (setq n ediff-current-difference))
  (and (>= n 0) (< n ediff-number-of-differences)))
  
(defsubst ediff-show-all-diffs (n)
  "Don't skip difference regions."
  nil)
     
(provide 'ediff-init)


;;; ediff-init.el  ends here
