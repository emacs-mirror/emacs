;; hilit19.el, Beta 1.9 -- customizable highlighting for Emacs19.
;; Copyright (c) 1993 Free Software Foundation, Inc.
;;
;; Author:  Jonathan Stigelman <Stig@netcom.com>
;; Keywords: faces
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:

;; hilit19.el, Beta 1.9 -- customizable highlighting for Emacs19.
;; Supports not only source code highlighting, but also rmail, VM, and gnus.

;; WHERE TO GET THE LATEST VERSION OF HILIT19.EL (possibly beta), 
;; PLUS LOTS OF OTHER *WAY COOL* STUFF VIA ANONYMOUS FTP:
;;
;;      netcom.com:/pub/stig/src/hilit19.el.gz
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TO SUBMIT BUG REPORTS (or feedback of any sort)...
;;
;;    M-x hilit-submit-feedback RET
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $Id: hilit19.el,v 1.34 1993/07/23 05:18:37 stig Exp stig $
;;
;; LCD Archive Entry:
;; emacs19/hilit19.el|Jonathan Stigelman|Stig@netcom.com
;; |Comprehensive (and comparatively fast) regex-based highlighting for Emacs 19
;; Thu Jul 22 21:03:46 1993|Beta 1.9||
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GENERAL OVERVIEW
;;
;;      This package installs numerous hooks to colorfully highlight your
;;      source code buffers as well as mail and news buffers.  Most 
;;      programming languages have predefined highlighting patterns.
;;	Just load hilit19 and files will be automatically highlighted as
;;      they're loaded.
;;
;;      Rehighlight a buffer by typing C-S-l (control-shift-lowercase-L).
;;
;;      If, when you edit the buffer, the coloring gets messed up, just
;;      redraw and the coloring will be adjusted.  If automatic highlighting
;;      in the current buffer has been turned off, then typing C-u C-S-l will
;;	force a rehighlight of the entire buffer.
;;
;;      Hilit19 can build faces by examining the names that you give to them
;;	For example, green/black-bold-italic-underline would be created as
;;	a face with a green foreground, and a black background, using a 
;;	bold-italic font...with underlining for good measure.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- In your .emacs:
;;
;;      (require 'hilit19)		; not intended to be autoloaded
;;
;;      (setq hilit-mode-enable-list '(not text-mode))
;;
;;      (hilit-translate type     'RoyalBlue   ; enable highlighting in C/C++
;;			 string	  nil)         ; disable string highlighting
;;
;; To get 100% of the utility of hilit19, you may also have to apply the
;; patches below for info.el and vm5.33L_19/vm-summary.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SETUP -- Are you using the right font for Emacs?
;;
;; Emacs cannot properly find bold and italic fonts unless you specify a
;; verbose X11 font name.  Here's a good font menu:
;;
;; (setq
;;  x-fixed-font-alist
;;  '("Font Menu"
;;    ("Fonts"
;;     ("6x12" "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-*-1")
;;     ("6x13" "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*-1")
;;     ("lucida 13"
;;      "-b&h-lucidatypewriter-medium-r-normal-sans-0-0-0-0-m-0-*-1")
;;     ("7x13" "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-1")
;;     ("7x14" "-misc-fixed-medium-r-normal--14-130-75-75-c-70-*-1")
;;     ("9x15" "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
;;     ("")
;;     ("clean 8x8" "-schumacher-clean-medium-r-normal--*-80-*-*-c-*-*-1")
;;     ("clean 8x14" "-schumacher-clean-medium-r-normal--*-140-*-*-c-*-*-1")
;;     ("clean 8x10" "-schumacher-clean-medium-r-normal--*-100-*-*-c-*-*-1")
;;     ("clean 8x16" "-schumacher-clean-medium-r-normal--*-160-*-*-c-*-*-1")
;;     ("")
;;     ("sony 8x16" "-sony-fixed-medium-r-normal--16-120-100-100-c-80-*-1")
;;     ("")
;;     ("-- Courier --")
;;     ("Courier 10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-*-1")
;;     ("Courier 12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-*-1")
;;     ("Courier 14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-*-1")
;;     ("Courier 18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-*-1")
;;     ("Courier 18-b" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-*-1")
;;     )))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KNOWN BUGS/TO DO LIST/HELP WANTED/APPLY WITHIN
;;
;; * unbalanced double quote characters can confuse hilit19.  This will be
;;   fixed, so don't bug me about it.
;;
;; * ALTHOUGH HILIT19 IS FASTER THAN FONT-LOCK-MODE, for various reasons,
;;   the speed of the package could still stand to be improved.  If you care
;;   to do a little profiling and make things tighter...
;;
;; * hilit-toggle-highlight is flaky in large buffers where auto-rehighlight
;;   is numeric after toggling twice, it loses it's numeric value
;;
;; PROJECTS THAT YOU CAN TAKE OVER BECAUSE I DON'T MUCH CARE ABOUT THEM...
;;
;; * Moved hilit-wysiwyg-replace here from my version of man.el, this is not
;;   a bug.  The bug is that I don't have a reverse operation yet...just a
;;   stub Wysiwyg-anything really belongs in a package of it's own.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Thanks to the following people for their input:
;;	ebert@enpc.enpc.fr (Rolf EBERT), ada, LaTeX & bibtex highlights
;;	Vivek Khera <khera@cs.duke.edu>, gnus hooks + random advice & patches
;;	brian@athe.WUstl.EDU (Brian Dunford-Shore), prolog highlights
;;	John Ladwig <jladwig@soils.umn.edu>, 1st pass nroff highlights
;;	campo@sunthpi3.difi.unipi.it (Massimo Campostrini), fortran highlights
;;	jayb@laplace.MATH.ColoState.EDU (Jay Bourland), 1st pass dired
;;	Yoshio Turner <yoshio@CS.UCLA.EDU>, modula 2 highlights
;;	Fritz Knabe <knabe@ecrc.de>, advice & patches
;;	Alon Albert <alon@milcse.rtsg.mot.com>, advice & patches
;;	dana@thumper.bellcore.com (Dana A. Chee), for breaking it...
;;      derway@ndc.com (Don Erway), for breaking it...
;;
;; With suggestions and minor regex patches from numerous others...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HISTORY 
;;
;; V1.9  21-July-1993		Stig@netcom.com
;;   better documentation and added the function hilit-submit-feedback.
;;   no longer rebind ^L, now C-S-l (control shift l) repaints the buffer
;;   multi-line highlights no longer cause problems when
;;      hilit-auto-rehighlight is 'visible
;;   added hilit-predefined-face-list...
;;   changed name of hilit-mode-alist to hilit-patterns-alist
;;   added hilit-message-quietly to mail-setup-hook
;;   added hilit-parser-alist which can be used to apply different patterns to
;;      different parts of a buffer.  This could be integrated in a far more
;;      elegant manner, but it presently serves the purpose of not applying
;;      message header patterns to message bodies in mail-mode and it's kin.
;;   hilit-set-mode-patterns now takes a list of modes and an optional parse-fn
;; V1.8  19-July-1993		Stig@netcom.com
;;   changed hilit-translate to be a macro so that now it mirrors setq
;;   now permit multiple layers of face-translation...
;;   hilit-lookup-face-create now parses background colors
;;   added code to check for face changes and recopy the fonts from 'default
;;      when necessary.  this can be disabled if you never change fonts.
;;      you should be able to change fonts, redraw, and have all of your
;;      bold & italic faces back to normal.  Should work in new frames as well.
;;   fixed typo for one of the vm hooks and included the magic patch to
;;      vm5.33 that keeps the summary window up to date.
;;   got rid of the annoying dings and delays when colors aren't available
;;   set case-fold-search to nil in highlighting-region function
;;   fixed minor bug in hilit-rehighlight-message-quietly
;;   patches to Info, LaTeX, fortran, nroff, & c++ patterns
;;   modula-2-mode support
;;   improved gnus-mark-article-hook
;;   moved timecard-mode highlights to timecard-mode itself
;; V1.7  12-July-1993		Stig@netcom.com
;;   fix to dired patterns
;;   punted on the dual functionality in hilit-auto-highlight and added
;;     hilit-mode-enable-list, which permits users to specifically lock out
;;     modes by preventing them from being added into the hilit-mode-list
;;   incorporated defaults for dark backgrounds (see hilit-background-mode)
;;   incorporated fortran highlighting patterns
;;   patches to ada-mode and msg-header regexes
;;   added msg-separator pattern
;;   changed dired-backup to dired ignored which (which is derived from the
;;     variable completion-ignored-extensions)
;; V1.6  5-July-1993		Stig@netcom.com
;;   added dired patterns
;;   fixed minor typo bug in mail patterns
;;   added profiling hook
;; V1.5  5-July-1993		Stig@netcom.com
;;   changed behavior of hilit-recenter to more closely match that of recenter
;;   hilit-auto-highlight can now be a list of major-modes to highlight on find
;;   reverted to using overlays...the cost of text-properties is too high, IMHO
;;   added 'visible option to hilit-auto-rehighlight variable
;;   now highlighting support for info pages (see patch below)
;;   added hilit-yank and hilit-yank-pop which replace their analogues
;;   wrote special parsing function for strings...bug squished...faster too
;;   tuned the texinfo patterns for better performance
;;   nroff support
;; V1.4  2-July-1993		Stig@netcom.com
;;   more efficient highlighting for news and mail
;;   switched to text properties (this may be temporary)
;;   changed regular expressions for c*mode to accomodate syntax tables
;;   minor mod to Ada parameter regexp
;;   now catch regex stack overflows and print an error
;;   string matching now uses start and end expressions to prevent overflows
;; V1.3 28-June-1993		Stig@netcom.com
;;   added support for hexadecimal color specification under X
;;   added hilit-translate for simple color translations
;;   changed coverage of hilit-quietly...when it's quiet, it's always quiet.
;;   removed extra call to unhighlight-region in rehighlight-buffer
;;   automatically installs hooks, unless hilit-inhibit-hooks set before load
;;   installed fixes for latex
;; V1.2 28-June-1993		Stig@netcom.com
;;   partially fixed bug in hilit-toggle-highlight
;;   added string highlighting
;;   fixed bug in hilit-lookup-face-create
;;   additions for Ada, Tex, LaTeX, and Texinfo (is scribe next? =)
;;   now highlight template decls in C++
;;   added reverse-* intelligence to hilit-lookup-face-create
;;   imported wysiwyg (overstrike replacement) stuff from my hacks to man.el
;;   sketched out a stub of a wysiwyg write file hook, care to finish it?
;; V1.1	25-June-1993		Stig@netcom.com
;;   replaced last vestiges of original hilit.el
;;   now map default modes to major-mode values
;;   reworked face allocation so that colors don't get tied up
;;   rewrote some comments that I'd put in earlier but somehow managed to nuke
;; V1.0 22-June-1993		Stig@netcom.com
;;   incrementally replaced just about everything...simpler, cleaner, & faster
;;   extended highlight coverage for C/C++ modes (highlight more things)
;;   added layer of indirection to face selection

;;;;;; THIS WILL ALLOW INFO PAGES TO BE HILIGHTED:
;;
;; *** 19.15/info.el       Sat Jun 19 14:47:06 1993
;; --- 19/info.el  Sun Jul  4 03:33:12 1993
;; ***************
;; *** 475,481 ****
;;                                   (setq active-expression
;;                                         (read (current-buffer))))))
;;                          (point-max)))
;; !      (if Info-enable-active-nodes (eval active-expression)))))
;;   
;;   (defun Info-set-mode-line ()
;;     (setq mode-line-buffer-identification
;; --- 475,482 ----
;;                                   (setq active-expression
;;                                         (read (current-buffer))))))
;;                          (point-max)))
;; !      (if Info-enable-active-nodes (eval active-expression)))
;; !    (run-hooks 'Info-select-hook)))
;;   
;;   (defun Info-set-mode-line ()
;;     (setq mode-line-buffer-identification
;;
;;;;;; AND THIS CAN BE APPLIED TO VM 5.33L_19
;; 
;; *** ../site/vm5.33L_19/vm-summary.el    Fri Jun  4 22:17:11 1993
;; --- ./vm-summary.el     Tue Jun 22 16:39:30 1993
;; ***************
;; *** 152,158 ****
;;                   (insert "->")
;;                   (delete-char 2)
;;                   (forward-char -2)
;; !                 (and w vm-auto-center-summary (vm-auto-center-summary))))
;;             (and old-window (select-window old-window)))))))
;;   
;;   (defun vm-mark-for-display-update (message)
;; --- 152,159 ----
;;                   (insert "->")
;;                   (delete-char 2)
;;                   (forward-char -2)
;; !                 (and w vm-auto-center-summary (vm-auto-center-summary))
;; !                 (run-hooks 'vm-summary-pointer-hook)))
;;             (and old-window (select-window old-window)))))))
;;   
;;   (defun vm-mark-for-display-update (message)
;; 
;;;;;;

;;; Code:

;; User Options:

(defvar hilit-quietly nil
  "* If non-nil, this inhibits progress indicators during highlighting")

(defvar hilit-inhibit-hooks nil
  "* If non-nil, this inhibits installation of hooks for Info, gnus, & vm.")

(defvar hilit-background-mode 'light
  "* 'mono inhibits color, 'dark or 'light indicate the background brightness.")

(defvar hilit-mode-enable-list nil
  "* If a list of modes to exclusively enable or specifically disable.
The sense of the list is negated if it begins with the symbol 'not'.
Set this variable before you load hilit19.

Ex:  (perl-mode jargon-mode c-mode)	; just perl, C, and jargon modes
     (not text-mode)			; all modes except text mode")

(defvar hilit-auto-highlight t
  "* T if we should highlight all buffers as we find 'em, nil to disable
  automatic highlighting by the find-file hook.")

(defvar hilit-auto-highlight-maxout 57000
  "* auto-highlight is disabled in buffers larger than this")

(defvar hilit-auto-rehighlight t
  "* If this is non-nil, then hilit-redraw and hilit-recenter will also
  rehighlight part or all of the current buffer.  T will rehighlights the
  whole buffer, a NUMBER will rehighlight that many lines before and
  after the cursor, or the symbol 'visible' will rehighlight only the visible
  portion of the current buffer.")

(make-variable-buffer-local 'hilit-auto-rehighlight)
(setq-default hilit-auto-rehighlight t)

(defvar hilit-auto-rehighlight-fallback '(20000 . 100)
  "* Cons of the form (THRESHOLD . FALLBACK), where FALLBACK is assigned to
hilit-auto-rehighlight if the size of a newly opened buffer is larger than
THRESHOLD.")

(defvar hilit-face-check t
  "* T slows down highlighting but permits the user to change fonts without
losing bold and italic faces...  T causes hilit-lookup-face-create to dig
through the frame parameters for the current window every time it's called.
If you never change fonts in emacs, set this to NIL.")

;; Variables that are not generally modified directly

(defvar hilit-parser-alist nil
  "alist of major-mode values and parsers called by hilit-rehighlight-buffer.

Parsers for a given mode are IGNORED for partial rehighlights...maybe you'd
like to make this more universal?")

(defvar hilit-patterns-alist nil
  "alist of major-mode values and default highlighting patterns

A hilighting pattern is a list of the form (start end face), where
start is a regex, end is a regex (or nil if it's not needed) and face
is the name of an entry in hilit-face-translation-table, the name of a face,
or nil (which disables the pattern).

See the hilit-lookup-face-create documentation for valid face names.")

(defvar hilit-predefined-face-list (face-list)
  "List of faces which with hilit-lookup-face-create will NOT tamper.

If hilit19 is dumped into emacs at your site, you may have to set this in
your init file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use this to report bugs:

(defun hilit-submit-feeback ()
  "Submit via mail a bug report on stig-paren"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on hilit19? ")
       (reporter-submit-bug-report
	"Jonathan Stigelman <Stig@netcom.com>"
	"hilit19.el Beta 1.9 ($Revision: 1.34 $)"
	(and (y-or-n-p "Do you need to include a dump hilit variables? ")
	     (append
	      '(
		hilit-quietly    		hilit-inhibit-hooks
		hilit-background-mode		hilit-mode-enable-list
		hilit-auto-highlight		hilit-auto-highlight-maxout
		hilit-auto-rehighlight		hilit-auto-rehighlight-fallback
		hilit-face-check
		)
	      (and (y-or-n-p "Have you modified the standard patterns? ")
		   (yes-or-no-p "Are your patterns *REALLY* relevant? ")
		   '(hilit-parser-alist
		     hilit-patterns-alist
		     hilit-predefined-face-list
		     ))))
	 (function
	  (lambda ()
	    (insert "\nFrame Configuration:\n====================\n"
		    (prin1-to-string (frame-configuration-to-register ?F))
		    "\n"
		    )))
	 nil
	 (concat
	  "This is (check all that apply, or delete those that don't):\n"
	  "  [ ] a _MASSIVE_THANK_YOU_ for writing hilit19.el\n"
	  "  [ ] my DONATION to your vacation fund (prototype digital cash)\n"
	  "  [ ] You're a RIGHTEOUS HACKER, what are your rates?\n"
	  "  [ ] I've used the force and read the source, but I'M CONFUSED\n"
	  "  [ ] a PATCH (diff -cw oldversion newversion) to fix a problem\n"
	  "  [ ] a REPRODUCABLE BUG that I do not believe to be an EMACS bug\n"
	  "     - I *swear* that it's not already mentioned in the KNOWN BUGS\n"
	  "     - Also, I have checked netcom.com:/pub/stig/src/hilit19.el.gz\n"
	  "       for a newer release that fixes the problem.\n"
	  "  [ ] ADVICE -- or an unfulfilled desire that I suspect you share\n"
	  "\n"
	  "Hey Stig, do you do anything besides hack emacs?\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These faces are either a valid face name, or nil
;; if you want to change them, you must do so AFTER hilit19 is loaded

(defconst hilit-face-translation-table
  (cond ((and (eq hilit-background-mode 'light) (x-display-color-p))
	 ;; COLOR DEFAULTS for LIGHT backgrounds
	 '(
	   ;; used for C/C++ and elisp and perl
	   (comment	. firebrick-italic)
	   (include	. purple)
	   (define	. ForestGreen-bold)
	   (defun	. blue-bold)
	   (decl	. RoyalBlue)
	   (type	. nil)
	   (keyword	. RoyalBlue)
	   (label	. red-bold)
	   (string	. grey40)

	   ;; some further faces for Ada
	   (struct	. black-bold)
	   (glob-struct	. magenta)
	   (named-param	. DarkGoldenrod)
	
	   ;; and anotherone for LaTeX
	   (crossref	. DarkGoldenrod)
 
	   (wysiwyg-bold   . bold)
	   (wysiwyg-underline . underline)

	   ;; compilation buffers
	   (error	. red-bold)
	   (warning	. firebrick)

	   ;; Makefiles (some faces borrowed from C/C++ too)
	   (rule	. blue-bold)

	   ;; VM, GNUS and Text mode
	   (msg-subject	. blue-bold)
	   (msg-from	. purple-bold) 
	   (msg-header	. firebrick-bold)
	   (msg-separator  . black/tan-bold)
	   (msg-quote	. ForestGreen)

	   (summary-seen	. grey40)
	   (summary-killed	. grey50)
	   (summary-Xed		. OliveDrab2)
	   (summary-current	. default/skyblue-bold)
	   (summary-deleted	. firebrick)
	   (summary-unread	. RoyalBlue)
	   (summary-new		. blue-bold)

	   (gnus-group-unsubscribed . grey50)
	   (gnus-group-empty	. nil)
	   (gnus-group-full	. ForestGreen)
	   (gnus-group-overflowing	. firebrick)

	   ;; dired mode
	   (dired-directory 	. blue-bold)
	   (dired-link		. firebrick-italic)
	   (dired-ignored	. ForestGreen)
	   (dired-deleted	. red-bold-italic)
	   (dired-marked	. purple)
	
	   ;; see jargon-mode.el and prep.ai.mit.edu:/pub/gnu/jargon*.txt
	   (jargon-entry	. blue-bold)
	   (jargon-xref		. purple-bold)
	   ;; really used for Info-mode
	   (jargon-keyword	. firebrick-underline)
	   ))
	((and (eq hilit-background-mode 'dark) (x-display-color-p))
	 ;; COLOR DEFAULTS for DARK backgrounds
	 '(
	   ;; used for C/C++ and elisp and perl
	   (comment	. moccasin)
	   (include	. Plum1)
	   (define	. green)
	   (defun	. cyan-bold)
	   (decl	. cyan)
	   (type	. yellow)
	   (keyword	. cyan)
	   (label	. orange-underlined)
	   (string	. orange)
	
	   ;; some further faces for Ada
	   (struct	. white-bold)
	   (glob-struct	. Plum1)
	   (named-param	. Goldenrod)
	
	   ;; and anotherone for LaTeX
	   (crossref	. Goldenrod)
	
	   (wysiwyg-bold   . bold)
	   (wysiwyg-underline . underline)
	
	   ;; compilation buffers
	   (error	. yellow)
	   (warning	. green)
	
	   ;; Makefiles (some faces borrowed from C/C++ too)
	   (rule		. cyan)
	
	   ;; VM, GNUS and Text mode
	   (msg-subject		. yellow)
	   (msg-from		. SeaGreen2)
	   (msg-header		. cyan)
	   (msg-separator	. lightblue)
	   (msg-quote		. green)
	
	   (summary-seen	. white)
	   (summary-killed	. white)
	   (summary-Xed		. green)
	   (summary-current	. green-bold)
	   (summary-deleted	. white)
	   (summary-unread	. yellow)
	   (summary-new		. yellow-bold)
	
	   (gnus-group-unsubscribed . white)
	   (gnus-group-empty	. yellow)
	   (gnus-group-full	. green)
	   (gnus-group-overflowing . orange)

	   ;; dired mode
	   (dired-directory	. cyan)
	   (dired-link		. green)
	   (dired-ignored	. moccasin)
	   (dired-deleted	. orange)
	   (dired-marked	. Plum1)

	   ;; see jargon-mode.el and prep.ai.mit.edu:/pub/gnu/jargon*.txt
	   (jargon-entry	. cyan)
	   (jargon-xref		. Plum1)
	   ;; really used for Info-mode
	   (jargon-keyword	. yellow)
	   ))
	(t
	 ;; MONO DEFAULTS -- you lose
	 '(
	   ;; used for C/C++ and elisp and perl
	   (comment       . italic)
	   (include       . default-bold-italic)
	   (define        . bold)
	   (defun         . default-bold-italic)
	   (decl          . bold)
	   (type          . nil)
	   (keyword       . default-bold-italic)
	   (label         . underline)
	   (string        . underline)

	   ;; some further faces for Ada
	   (struct	  . bold)
	   (named-param   . underline)
	   (glob-struct   . default-bold-underline)
      
	   ;; and another one for LaTeX
	   (crossref	  . underline)

	   (wysiwyg-bold  . bold)
	   (wysiwyg-underline . underline)

	   ;; compilation buffers
	   (error         . bold)
	   (warning       . italic)

	   ;; Makefiles (some faces borrowed from C/C++ too)
	   (rule          . bold)

	   ;; VM, GNUS and Text mode
	   (msg-subject   . bold)
	   (msg-from	  . bold)
	   (msg-header    . italic)
	   (msg-separator . nil)
	   (msg-quote     . italic)

	   (summary-seen	. nil)
	   (summary-killed	. nil)
	   (summary-Xed		. nil)
	   (summary-current	. reverse-default)
	   (summary-unread	. bold)
	   (summary-deleted	. italic)
	   (summary-new		. default-bold-italic)

	   (gnus-group-unsubscribed	. nil)
	   (gnus-group-empty		. nil)
	   (gnus-group-full		. italic)
	   (gnus-group-overflowing	. default-bold-italic)

	   ;; dired mode
	   (dired-directory 	. bold)
	   (dired-link		. italic)
	   (dired-ignored	. nil)
	   (dired-marked	. nil)
	   (dired-deleted	. default-bold-italic)
	
	   ;; see jargon-mode.el and prep.ai.mit.edu:/pub/gnu/jargon*.txt
	   (jargon-entry	. bold)
	   (jargon-xref		. italic)
	   ;; really used for Info-mode
	   (jargon-keyword	. underline)
	   ))
	)
    "alist that maps symbolic face-names to real face names")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To translate one face to another...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hilit-translate (&rest args)
  "(hilit-translate FROM TO FROM TO ...): translate each face FROM to the
value of its TO face.  This is like setq for faces.

The function hilit-lookup-face-create will repeatedly translate until no more
translations for the face exist in the translation table.

See the documentation for hilit-lookup-face-create for names of valid faces.
"
;; can't have an interactive macro
;;  (interactive "SFace translate from: \nSFace translate to: ")
  (or (zerop (% (length args) 2))
      (error "wrong number of args"))
  (let (cmdl from to)
    (while args
      (setq from (car args) to (nth 1 args) args (nthcdr 2 args)
	    cmdl (cons (list 'hilit-associate ''hilit-face-translation-table
			     ;; this is for reverse compatibility...
			     (if (and (consp from) (eq 'quote (car from)))
				 from
			       (list 'quote from)) to)
		       cmdl)))
    (cons 'progn cmdl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This function actually translates and then creates the faces...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun hilit-lookup-face-create (face &optional force)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the followwing naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]
Example: (hilit-lookup-face-create 'comment-face) might create and return 'red

Each color is either the name of an X color (see .../X11/lib/X11/rgb.txt),
a hexadecimal specification of the form \"hex-[0-9A-Fa-f]+\", or \"default\".

An optional argument, FORCE, will cause the face to be recopied from the
default...which is probably of use only if you've changed fonts.

See the documentation for hilit-translate and hilit-face-translation-table."

;; translate the face ...
  (let ((trec t) visited)
    (while trec
      (cond ((memq face visited) (error "face translation loop: %S" visited))
	    (t (setq visited (cons face visited)
		     trec (assq face hilit-face-translation-table))
	       (and trec (setq face (cdr trec)))))))

  ;; make the face if we need to...
  (let* ((fn (symbol-name face))
	 (frame (selected-frame))
	 (basefont (cdr (assq 'font (frame-parameters frame))))
	 error fgcolor bgcolor)
    (cond
     ((or (null face)			
	  (memq face hilit-predefined-face-list))
      ;; do nothing if the face is nil or if it's predefined.
      )
     ((or force
	  (not (memq face (face-list)))
	  (and hilit-face-check
	       (not (string= (get face 'basefont) basefont))))
      (copy-face 'default 'scratch-face)
      (if (string-match "^reverse-?" fn)
	  (progn (invert-face 'scratch-face)
		 (setq fn (substring fn (match-end 0)))))

      ;; parse foreground color
      (if (string-match "^\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq fgcolor (concat
			 (if (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0)))
	(error "bad face name %S" face))

      ;; parse background color
      (if (string-match "^/\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq bgcolor (concat
			 (and (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0))))
      
      (and (string= "default" fgcolor) (setq fgcolor nil))
      (and (string= "default" bgcolor) (setq bgcolor nil))
      
      ;; catch errors if we can't allocate the color(s)
      (condition-case nil
	  (progn (set-face-foreground 'scratch-face fgcolor)
		 (set-face-background 'scratch-face bgcolor)
		 (copy-face 'scratch-face face)
		 (put face 'basefont basefont))
	(error (message "couldn't allocate color for '%s'"
			(symbol-name face))
	       (setq face 'default)
	       (setq error t)))
      (or error
	  ;; don't bother w/ bold or italic if we didn't get the color
	  ;; we wanted, but ignore errors making the face bold or italic
	  ;; if the font isn't available, there's nothing to do about it...
	  (progn
	    (set-face-font face nil frame)
	    (set-face-underline-p face (string-match "underline" fn))
	    (if (string-match ".*bold" fn)
		(make-face-bold face frame  'noerr))
	    (if (string-match ".*italic" fn)
		(make-face-italic face frame 'noerr))
	    ))
      )))
  face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region Highlight/Unhighlight code (Both overlay and text-property versions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst hilit-region-set-face (start end face-name &optional prio prop)
  "Highlight region from START to END using FACE and, optionally, PRIO.
The optional 5th arg, PROP is a property to set instead of 'hilit."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face-name)
    (overlay-put overlay (or prop 'hilit) t)
    (and prio (overlay-put overlay 'priority prio))))

(defun hilit-unhighlight-region (start end &optional quietly)
  "Unhighlights the region from START to END, optionally in a QUIET way"
  (interactive "r")
  (or quietly hilit-quietly (message "Unhighlighting"))
  (while (< start end)
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr 'hilit) (delete-overlay ovr))))
	    (overlays-at start))
    (setq start (next-overlay-change start)))
  (or quietly hilit-quietly (message "Done unhighlighting")))

;;;; These functions use text properties instead of overlays.  Text properties
;;;; are copied through kill and yank...which might be convenient, but is not
;;;; terribly efficient as of 19.12, ERGO it's been disabled
;;
;;(defsubst hilit-region-set-face (start end face-name &optional prio prop)
;;  "Highlight region from START to END using FACE and, optionally, PRIO.
;;The optional 5th arg, PROP is a property to set instead of 'hilit."
;;    (put-text-property start end 'face face-name)
;;    )
;;
;;(defun hilit-unhighlight-region (start end &optional quietly)
;;  "Unhighlights the region from START to END, optionally in a QUIET way"
;;  (interactive "r")
;;  (let ((buffer-read-only nil)
;;	(bm (buffer-modified-p)))
;;    (remove-text-properties start end '(face))
;;    (set-buffer-modified-p bm)))
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Application code and user functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-highlight-region (start end &optional patterns quietly)
  "Highlights the area of the buffer between START and END (the region when
interactive).  Without the optional PATTERNS argument, the pattern for
major-mode is used.  If PATTERNS is a symbol, then the patterns associated
with that symbol are used.  QUIETLY suppresses progress messages if
non-nil."
  (interactive "r")
  (cond ((null patterns)
	 (setq patterns (cdr (assq major-mode hilit-patterns-alist))))
	((symbolp patterns)
	 (setq patterns (cdr (assq patterns hilit-patterns-alist)))))  
  ;; txt prop: (setq patterns (reverse patterns))
  (let ((prio (length patterns))
	(case-fold-search nil)
	;; txt prop: (buffer-read-only nil)
	;; txt prop: (bm (buffer-modified-p))
	p pstart pend face mstart)
    ;; txt prop: (unwind-protect
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while patterns
	  (setq p (car patterns))
	  (setq pstart (car p)
		pend (nth 1 p)
		face (hilit-lookup-face-create (nth 2 p)))
	  (if (not face)		; skipped if nil
	      nil
	    (or quietly hilit-quietly
		(message "highlighting %d: %s%s" prio pstart 
			 (if pend (concat " ... " pend) "")))
	    (goto-char (point-min))
	    (condition-case nil
		(cond 
		      ((symbolp pstart)
		       ;; inner loop -- special function to find pattern
		       (let (region)
			 (while (setq region (funcall pstart pend))
			   (hilit-region-set-face (car region) (cdr region)
						  face prio))))
		      (pend
		       ;; inner loop -- regex-start ... regex-end
		       (while (re-search-forward pstart nil t nil)
			 (goto-char (setq mstart (match-beginning 0)))
			 (if (re-search-forward pend nil t nil)
			     (hilit-region-set-face mstart (match-end 0)
						    face prio)
			   (forward-char 1))))
		      (t
		       ;; inner loop -- just one regex to match whole pattern
		       (while (re-search-forward pstart nil t nil)
			 (hilit-region-set-face  (match-beginning 0)
						 (match-end 0) face prio))))
	      (error (message "Unbalanced delimiters?  Barfed on '%s'"
			      pstart)
		     (ding) (sit-for 4))))
	  (setq prio (1- prio)
		patterns (cdr patterns)))
	))				
    (or quietly hilit-quietly (message "")) ; "Done highlighting"
    ;; txt prop: (set-buffer-modified-p bm)) ; unwind protection
    ))

(defun hilit-rehighlight-region (start end &optional quietly)
  "Re-highlights the region, optionally in a QUIET way"
  (interactive "r")
  (setq start (apply 'min start (mapcar 'overlay-start (overlays-at start)))
	end (apply 'max end (mapcar 'overlay-end (overlays-at end))))
  (hilit-unhighlight-region start end quietly)
  (hilit-highlight-region   start end nil quietly))

(defun hilit-rehighlight-buffer (&optional quietly)
  "Re-highlights the buffer, optionally in a QUIET way"
  (interactive "")
  (let ((parse-fn (cdr (assq major-mode hilit-parser-alist))))
    (if parse-fn
	(funcall parse-fn quietly)
      (hilit-rehighlight-region (point-min) (point-max) quietly)))
  nil)

(defun hilit-rehighlight-buffer-quietly ()
  (hilit-rehighlight-buffer t))

(defun hilit-rehighlight-message (quietly)
  "Highlight a buffer containing a news article or mail message."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$" nil 'noerr)
    (hilit-unhighlight-region (point-min) (point-max) quietly)
    (hilit-highlight-region (point-min) (point) 'msg-header quietly)
    (hilit-highlight-region (point) (point-max) 'msg-body quietly)))

(defalias 'hilit-highlight-buffer 'hilit-rehighlight-buffer)

(defun hilit-toggle-highlight (arg)
  "Locally toggle highlighting.  With arg, forces highlighting off."
  (interactive "P")
  ;; FIXME -- this loses numeric information in hilit-auto-rehighlight
  (setq hilit-auto-rehighlight
	(and (not arg) (not hilit-auto-rehighlight)))
  (if hilit-auto-rehighlight
      (hilit-rehighlight-buffer)
    (hilit-unhighlight-region (point-min) (point-max)))
  (message "Rehighlighting is set to %s" hilit-auto-rehighlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit-find-file-hook ()
  "Find-file hook for hilit package.  See the variable hilit-auto-highlight."
  (cond ((and hilit-auto-highlight
	      (assq major-mode hilit-patterns-alist))
	 (if (> buffer-saved-size (car hilit-auto-rehighlight-fallback))
	     (setq hilit-auto-rehighlight
		   (cdr hilit-auto-rehighlight-fallback)))
	 (if (> buffer-saved-size hilit-auto-highlight-maxout) nil
	   (hilit-rehighlight-buffer)
	   (set-buffer-modified-p nil)))))

(defun hilit-repaint-command (arg)
  "Rehighlights according to the value of hilit-auto-rehighlight, or the
prefix argument if that is specified.
\t\\[hilit-repaint-command]\t\trepaint according to hilit-auto-rehighlight
\t^U \\[hilit-repaint-command]\trepaint entire buffer
\t^U - \\[hilit-repaint-command]\trepaint visible portion of buffer
\t^U n \\[hilit-repaint-command]\trepaint n lines to either side of point"
  (interactive "P") 
  (let (st en quietly)
    (or arg (setq arg hilit-auto-rehighlight))
    (cond ((or (eq  arg 'visible) (eq arg '-))
	   (setq st (window-start) en (window-end) quietly t))
	  ((numberp arg)
	   (setq st (save-excursion (forward-line (- arg)) (point))
		 en (save-excursion (forward-line arg) (point))))
	  (arg
	   (hilit-rehighlight-buffer)))
    (if st
	  (hilit-rehighlight-region st en quietly))))

;; (defun hilit-rehighlight-yank-region ()
;;   "Rehighlights from the beginning of the line where the region starts to
;; the end of the line where the region ends.  This could flake out on
;; multi-line highlights (like C comments and lisp strings.)"
;;   (if hilit-auto-rehighlight
;;       (hilit-rehighlight-region
;;        (save-excursion (goto-char (region-beginning))
;; 		       (beginning-of-line) (point))
;;        (save-excursion (goto-char (region-end))
;; 		       (end-of-line) (point))
;;        t)))

(defun hilit-recenter (arg)
  "Recenter, then rehighlight according to hilit-auto-rehighlight.  If called
with an unspecified prefix argument (^U but no number), then a rehighlight of
the entire buffer is forced."
  (interactive "P")
  (recenter arg)
  ;; force display update
  (sit-for 0)
  (hilit-repaint-command (consp arg)))

;; (defun hilit-redraw-display (arg)
;;   "Rehighlights according to the value of hilit-auto-rehighlight, a prefix
;; arg forces a rehighlight of the whole buffer.  Otherwise just like
;; redraw-display."
;;   (interactive "P")
;;   (hilit-redraw-internal arg)
;;   (redraw-display))

(defun hilit-yank (arg)
  "Yank with rehighlighting"
  (interactive "*P")
  (let ((transient-mark-mode nil))
    (yank arg)
    (hilit-rehighlight-region (region-beginning) (region-end) t)
    (setq this-command 'yank)))

(defun hilit-yank-pop (arg)
  "Yank-pop with rehighlighting"
  (interactive "*p")
  (let ((transient-mark-mode nil))
    (yank-pop arg)
    (hilit-rehighlight-region (region-beginning) (region-end) t)
    (setq this-command 'yank)))

;;; this line highlighting stuff is untested.  play with it only if you feel
;;; adventurous...don't ask me to fix it...though you're welcome to.  -- Stig
;; 
;; (defun hilit-rehighlight-line-quietly (&rest args)
;;   "Quietly rehighlight just this line.
;; Useful as an after change hook in VM/gnus summary buffers and dired buffers.
;; If only there were an after-change-function, that is..."
;;   (save-excursion
;;     (push-mark nil t)
;;     (hilit-rehighlight-yank-region)
;;     (and orig-achange-function (apply orig-achange-function args))))
;; 
;; (defun hilit-install-line-hooks ()
;;   (make-variable-buffer-local 'after-change-function)
;;   (make-local-variable 'orig-achange-function)
;;   (setq orig-achange-function after-change-function)
;;   (setq after-change-function 'hilit-rehighlight-line-quietly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wysiwyg Stuff...  take it away and build a whole package around it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ; For the Jargon-impaired, WYSIWYG === What You See Is What You Get
;; ; Sure, it sucks to type.  Oh, well.
;; (defun hilit-wysiwyg-replace ()
;;   "Replace overstruck text with normal text that's been overlayed with the 
;; appropriate text attribute.  Suitable for a find-file hook."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((wysb (hilit-lookup-face-create 'wysiwyg-bold))
;; 	  (wysu (hilit-lookup-face-create 'wysiwyg-underline))
;; 	  (bmod (buffer-modified-p)))
;;       (while (re-search-forward "\\(.\b.\\)+" nil t)
;; 	(let ((st (match-beginning 0)) (en (match-end 0)))
;; 	  (goto-char st)
;; 	  (if (looking-at "_")
;; 	      (hilit-region-set-face st en wysu 100 'wysiwyg)
;; 	    (hilit-region-set-face st en wysb 100 'wysiwyg))
;; 	  (while (and (< (point) en) (looking-at ".\b"))
;; 	    (replace-match "") (forward-char))
;; 	  ))
;;       (set-buffer-modified-p bmod))))
;; 
;; ; is this more appropriate as a write-file-hook or a write-contents-hook?
;; (defun hilit-wysiwyg-write-repair ()
;;   "Replace wysiwyg overlays with overstrike text."
;;   (message "*sigh* hilit-wysiwyg-write-repair not implemented yet")
;;
;; For efficiency, this hook should copy the current buffer to a scratch
;; buffer and do it's overstriking there.  Overlays are not copied, so it'll
;; be necessary to hop back and forth.  This is OK since you're not fiddling
;; with--making or deleting--any overlays.  THEN write the new buffer,
;; delete it, and RETURN T. << important
;;
;; Just so you know...there is already an emacs function called
;; underline-region that does underlining.  I think that the thing to do is
;; extend that to do overstriking as well.
;;
;;  (while (< start end)
;;    (mapcar (function (lambda (ovr)
;;			  (and (overlay-get ovr 'hilit) (delete-overlay ovr))))
;;	    (overlays-at start))
;;    (setq start (next-overlay-change start)))
;;  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(substitute-key-definition 'yank     'hilit-yank     (current-global-map))
(substitute-key-definition 'yank-pop 'hilit-yank-pop (current-global-map))

;; (substitute-key-definition 'recenter 'hilit-recenter (current-global-map))
;; (substitute-key-definition 'redraw-display 'hilit-redraw-display
;;			      (current-global-map))

(global-set-key [?\C-\S-l] 'hilit-repaint-command)

(and window-system
     (add-hook 'find-file-hooks 'hilit-find-file-hook t))

(and (not hilit-inhibit-hooks)
     window-system
     (condition-case c
	 (progn

	   ;; BUFFER highlights...
	   (mapcar (function
		    (lambda (hook)
		      (add-hook hook 'hilit-rehighlight-buffer-quietly)))
		   '(
		     Info-select-hook
		     vm-summary-mode-hooks
		     vm-summary-pointer-hook
		     gnus-summary-prepare-hook
		     gnus-group-prepare-hook

		     vm-preview-message-hook
		     vm-show-message-hook
		     gnus-article-prepare-hook
		     rmail-show-message-hook
		     mail-setup-hook 
		     ))

	   ;; rehilight only the visible part of the summary buffer for speed.
	   (add-hook 'gnus-mark-article-hook
		     (function
		      (lambda ()
			(or (memq gnus-current-article gnus-newsgroup-marked)
			    (gnus-summary-mark-as-read gnus-current-article))
			(gnus-summary-set-current-mark)
			(save-excursion
			  (set-buffer gnus-summary-buffer)
			  (hilit-rehighlight-region (window-start)
						    (window-end) t)
			  ))))
	   ;; only need prepare article hook
	   ;;
	   ;;	(add-hook 'gnus-select-article-hook
	   ;;		  '(lambda () (save-excursion
	   ;;				(set-buffer gnus-article-buffer)
	   ;;				(hilit-rehighlight-buffer))))
	   )
       (error (message "Error loading highlight hooks: %s" c)
	      (ding) (sit-for 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default patterns for various modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; do I need this?  I changed the defconst to a defvar because defconst is
;;; inappropriate, but I don't know why I wanted hilit-patterns-alist to be
;;; reset on every reload...

(setq hilit-patterns-alist nil) 

(defun hilit-associate (alist key val)
  "creates, or destructively replaces, the pair (key . val) in alist"
  (let ((oldentry (assq key (eval alist))))
    (if oldentry
	(setcdr oldentry val)
      (set alist (cons (cons key val) (eval alist))))))
  
(defun hilit-set-mode-patterns (modelist patterns &optional parse-fn)
  "Sets the default hilighting patterns for MODE to PATTERNS.
See the variable hilit-mode-enable-list."
  (or (consp modelist) (setq modelist (list modelist)))
  (let (ok (flip (eq (car hilit-mode-enable-list) 'not)))
    (mapcar (function
	     (lambda (m)
	       (setq ok (or (null hilit-mode-enable-list)
			    (memq m hilit-mode-enable-list)))
	       (and flip (setq ok (not ok)))
	       (and ok
		   (progn
		     (and parse-fn
			  (hilit-associate 'hilit-parser-alist m parse-fn))
		     (hilit-associate 'hilit-patterns-alist m patterns)))))
	    modelist)))

(defun hilit-string-find (qchar)
  "looks for a string and returns (start . end) or NIL.  The argument QCHAR
is the character that would precede a character constant double quote.
Finds  [^QCHAR]\" ... [^\\]\""
  (let (st en)
    (while (and (search-forward "\"" nil t)
		(eq qchar (char-after (1- (setq st (match-beginning 0)))))))
    (while (and (search-forward "\"" nil t)
		(eq ?\\ (char-after (- (setq en (point)) 2)))))
    (and en (cons st en))))    

(hilit-set-mode-patterns
 '(c-mode c++-c-mode elec-c-mode)
 '(("/\\*" "\\*/" comment)
					;	("\"" "[^\\]\"" string)
   (hilit-string-find ?' string)
   ;; declaration
   ("^#[ \t]*\\(undef\\|define\\).*$" nil define)
   ("^#.*$" nil include)
   ;; function decls are expected to have types on the previous line
   ("^\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
   ("^\\(typedef\\|struct\\|union\\|enum\\).*$" nil decl)
   ;; datatype -- black magic regular expression
   ("[ \n\t({]\\(\\(register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
   ;; key words
   ("\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\)\\>" nil keyword)
   ))

(hilit-set-mode-patterns
 'c++-mode
 '(("/\\*" "\\*/" comment)
   ("//.*$" nil comment)
   ("^/.*$" nil comment)
;   ("\"" "[^\\]\"" string)
   (hilit-string-find ?' string)
   ;; declaration	
   ("^#[ \t]*\\(undef\\|define\\).*$" nil define)
   ("^#.*$" nil include)
   ;; function decls are expected to have types on the previous line
   ("^\\(\\(\\w\\|[$_]\\)+::\\)?\\(\\w\\|[$_]\\)+\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
   ("^\\(\\(\\w\\|[$_]\\)+[ \t]*::[ \t]*\\)?\\(\\(\\w\\|[$_]\\)+\\|operator.*\\)\\s *\\(\\(\\w\\|[$_]\\)+\\s *((\\|(\\)[^)]*)+" nil defun)
   ("^\\(template\\|typedef\\|struct\\|union\\|class\\|enum\\|public\\|private\\|protected\\).*$" nil decl)
   ;; datatype -- black magic regular expression
   ("[ \n\t({]\\(\\(register\\|volatile\\|unsigned\\|extern\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|char\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil type)
   ;; key words
   ("\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|while\\|do\\|for\\|public\\|protected\\|private\\|delete\\|new\\)\\>"
    nil keyword)))

(hilit-set-mode-patterns
 'perl-mode
 '(("\\s #.*$" nil comment)
   ("^#.*$" nil comment)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("^\\(__....?__\\|\\s *\\sw+:\\)" nil label)
   ("^require.*$" nil include)
   ("^package.*$" nil decl)
   ("^\\s *sub\\s +\\(\\w\\|[_']\\)+" nil defun)
   ("\\b\\(do\\|if\\|unless\\|while\\|until\\|else\\|elsif\\|for\\|foreach\\|continue\\|next\\|redo\\|last\\|goto\\|return\\|die\\|exit\\)\\b" nil keyword)))

(hilit-set-mode-patterns
 'ada-mode
 '(;; comments
   ("--.*$" nil comment)
   ;; main structure
   ("[ \t\n]procedure[ \t]" "\\([ \t]\\(is\\|renames\\)\\|);\\)" glob-struct)
   ("[ \t\n]task[ \t]" "[ \t]is" glob-struct)
   ("[ \t\n]function[ \t]" "return[ \t]+[A-Za-z_0-9]+[ \t]*\\(is\\|;\\|renames\\)" glob-struct)
   ("[ \t\n]package[ \t]" "[ \t]\\(is\\|renames\\)" glob-struct)
   ;; if there is nothing before "private", it is part of the structure
   ("^[ \t]*private[ \t\n]" nil glob-struct)
   ;; if there is no indentation before the "end", then it is most
   ;; probably the end of the package
   ("^end.*$" ";" glob-struct)
   ;; program structure -- "null", "delay" and "terminate" omitted
   ("[ \n\t]\\(in\\|out\\|select\\|if\\|else\\|case\\|when\\|and\\|or\\|not\\|accept\\|loop\\|do\\|then\\|elsif\\|else\\|for\\|while\\|exit\\)[ \n\t;]" nil struct)
   ;; block structure
   ("[ \n\t]\\(begin\\|end\\|declare\\|exception\\|generic\\|raise\\|return\\|package\\|body\\)[ \n\t;]" nil struct)
   ;; type declaration
   ("^[ \t]*\\(type\\|subtype\\).*$" ";" decl)
   ("[ \t]+is record.*$" "end record;" decl)
   ;; "pragma", "with", and "use" are close to C cpp directives
   ("^[ \t]*\\(with\\|pragma\\|use\\)" ";" include)
   ;; nice for named parameters, but not so beautiful in case statements
   ("[A-Za-z_0-9.]+[ \t]*=>"   nil named-param)
   ;; string constants probably not everybody likes this one
   ("\"" ".*\"" string)))

(hilit-set-mode-patterns
 'fortran-mode
 '(("^[*Cc].*$" nil comment)
   ("[ \t]\\(call\\|program\\|subroutine\\|function\\|stop\\|return\\|end\\|include\\)[ \t\n]" nil include)
   ("\\(^[ \t]*[0-9]+\\|[ \t]continue[ \t\n]\\|format\\)" nil define)
   ("[ \t]\\(do\\|do[ \t]*[0-9]+\\|go[ \t]*to[ \t]*[0-9]+\\|end[ \t]*do\\|if\\|else[ \t]*if\\|then\\|else\\|end[ \t]*if\\)[ \t\n(]" nil define)
   ("[ \t]\\(parameter[\t\n ]*([^)]*)\\|data\\|save\\|common[ \t\n]*/[^/]*/\\)" 
    nil decl)
   ("^     ." nil type)
   ("implicit[ \t]*none" nil decl)
   ("\\([ \t]\\|implicit[ \t]*\\)\\(dimension\\|integer\\|real\\|double[ \t]*precision\\|character\\|logical\\|complex\\|double[ \t]*complex\\)\\([*][0-9]*\\|[ \t\n]\\)" nil keyword)
   ("'[^'\n]*'" nil string)
   ))

(hilit-set-mode-patterns
 '(m2-mode modula-2-mode)
 '(("(\\*" "\\*)" comment)
   (hilit-string-find ?\\ string)
   ("^[ \t]*PROCEDURE[ \t]+\\w+[^ \t(;]*" nil defun)
   ("\\<\\(RECORD\\|ARRAY\\|OF\\|POINTER\\|TO\\|BEGIN\\|END\\|FOR\\|IF\\|THEN\\|ELSE\\|ELSIF\\|CASE\\|WHILE\\|DO\\|MODULE\\|FROM\\|RETURN\\|IMPORT\\|EXPORT\\|VAR\\|LOOP\\|UNTIL\\|\\DEFINITION\\|IMPLEMENTATION\\|AND\\|OR\\|NOT\\|CONST\\|TYPE\\|QUALIFIED\\)\\>" nil keyword)
   ))

(hilit-set-mode-patterns 'prolog-mode
 '(("/\\*" "\\*/" comment)
   ("%.*$" nil comment)
   (":-" nil defun)
   ("!" nil label)
   ("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" nil string)
   ("\\b\\(is\\|mod\\)\\b" nil keyword)
   ("\\(->\\|-->\\|;\\|==\\|\\\\==\\|=<\\|>=\\|<\\|>\\|=\\|\\\\=\\|=:=\\|=\\\.\\\.\\|\\\\\\\+\\)" nil decl)
   ("\\(\\\[\\||\\|\\\]\\)" nil include)))

(hilit-set-mode-patterns
 '(
   LaTeX-mode japanese-LaTeX-mode SliTeX-mode
   japanese-SliTeX-mode FoilTeX-mode latex-mode
   )
 '(
   ;; comments
   ("[^\\]%.*$" nil comment)

   ;; the following two match \foo[xx]{xx} or \foo*{xx} or \foo{xx}
   ("\\\\\\(sub\\)*\\(paragraph\\|section\\)\\(\*\\|\\[.*\\]\\)?{" "}"
    keyword)
   ("\\\\\\(chapter\\|part\\)\\(\*\\|\\[.*\\]\\)?{" "}" keyword)
   ("\\\\footnote\\(mark\\|text\\)?{" "}" keyword)
   ("\\\\[a-z]+box" nil keyword)
   ("\\\\\\(v\\|h\\)space\\(\*\\)?{" "}" keyword)

   ;; (re-)define new commands/environments/counters
   ("\\\\\\(re\\)?new\\(environment\\|command\\){" "}" defun)
   ("\\\\new\\(length\\|theorem\\|counter\\){" "}" defun)

   ;; various declarations/definitions
   ("\\\\\\(setlength\\|settowidth\\|addtolength\\|setcounter\\|addtocounter\\)" nil define)
   ("\\\\\\(\\|title\\|author\\|date\\|thanks\\){" "}" define)

   ("\\\\documentstyle\\(\\[.*\\]\\)?{" "}" decl)
   ("\\\\\\(begin\\|end\\|nofiles\\|includeonly\\){" "}" decl)
   ("\\\\\\(raggedright\\|makeindex\\|makeglossary\\|maketitle\\)\\b" nil
    decl)
   ("\\\\\\(pagestyle\\|thispagestyle\\|pagenumbering\\){" "}" decl)
   ("\\\\\\(normalsize\\|small\\|footnotesize\\|scriptsize\\|tiny\\|large\\|Large\\|LARGE\\|huge\\|Huge\\)\\b" nil decl)
   ("\\\\\\(appendix\\|tableofcontents\\|listoffigures\\|listoftables\\)\\b"
    nil decl)
   ("\\\\\\(bf\\|em\\|it\\|rm\\|sf\\|sl\\|ss\\|tt\\)\\b" nil decl)

   ;; label-like things
   ("\\\\item\\[" "\\]" label)
   ("\\\\item\\b" nil label)
   ("\\\\caption\\(\\[.*\\]\\)?{" "}" label)

   ;; things that bring in external files
   ("\\\\\\(include\\|input\\|bibliography\\){" "}" include)

   ;; "wysiwyg" emphasis
   ("{\\\\\\(em\\|it\\|sl\\)" "}" italic)
   ("{\\\\bf" "}" bold)

   ("``" "''" string)

   ;; things that do some sort of cross-reference
   ("\\\\\\(\\(no\\)?cite\\|\\(page\\)?ref\\|label\\|index\\|glossary\\){" "}" crossref)
   ))

(hilit-set-mode-patterns
 'bibtex-mode
 '(;;(";.*$"			nil	comment)
   ("%.*$"			nil	comment)
   ("@[a-zA-Z]+"		nil	keyword)
   ("{[ \t]*[-a-z:_A-Z0-9]+,"	nil	label) ; is wrong sometimes
   ("^[ \t]*[a-zA-Z]+[ \t]*="	nil	define)))

(hilit-set-mode-patterns
 'compilation-mode
 '(("^[^ \t]*:[0-9]+:.*$" nil error)
   ("^[^ \t]*:[0-9]+: warning:.*$" nil warning)))

(hilit-set-mode-patterns
 'makefile-mode
 '(("^#.*$" nil comment)
   ("[^$]#.*$" nil comment)
   ;; rules
   ("^%.*$" nil rule)
   ("^[.][A-Za-z][A-Za-z]?\..*$" nil rule)
   ;; variable definition
   ("^[_A-Za-z0-9]+ *\+?=" nil define)
   ("\\( \\|:=\\)[_A-Za-z0-9]+ *\\+=" nil define)
   ;; variable references
   ("\$[_A-Za-z0-9]" nil type)
   ("\${[_A-Za-z0-9]+}" nil type)
   ("\$\([_A-Za-z0-9]+\)" nil type)
   ("^include " nil include)))

(let* ((header-patterns '(("^Subject:.*$" nil msg-subject)
			  ("^From:.*$" nil msg-from)
			  ("^--text follows this line--$" nil msg-separator)
			  ("^[A-Za-z][A-Za-z0-9-]+:" nil msg-header)))
       (body-patterns '(("^\\(In article\\|[ \t]*\\w*[]>}|]\\).*$"
			 nil msg-quote)))
       (message-patterns (append header-patterns body-patterns)))
  (hilit-set-mode-patterns 'msg-header header-patterns)
  (hilit-set-mode-patterns 'msg-body body-patterns)
  (hilit-set-mode-patterns
   '(vm-mode text-mode mail-mode rmail-mode gnus-article-mode news-reply-mode)
   message-patterns
   'hilit-rehighlight-message))

(hilit-set-mode-patterns
 'gnus-group-mode
 '(("^U.*$" nil gnus-group-unsubscribed)
   ("^ +[01]?[0-9]:.*$" nil gnus-group-empty)
   ("^ +[2-9][0-9]:.*$" nil gnus-group-full)
   ("^ +[0-9][0-9][0-9]+:.*$" nil gnus-group-overflowing)))

(hilit-set-mode-patterns
 'gnus-summary-mode
 '(("^D +[0-9]+: \\[.*$" nil summary-seen)
   ("^K +[0-9]+: \\[.*$" nil summary-killed)
   ("^X +[0-9]+: \\[.*$" nil summary-Xed)
   ("^- +[0-9]+: \\[.*$" nil summary-unread)
   ("^. +[0-9]+:\\+\\[.*$" nil summary-current)
   ("^  +[0-9]+: \\[.*$" nil summary-new)
   ))

(hilit-set-mode-patterns
 'vm-summary-mode
 '(("^   .*$" nil summary-seen)
   ("^->.*$" nil  summary-current)
   ("^  D.*$" nil summary-deleted)
   ("^  U.*$" nil summary-unread)
   ("^  N.*$" nil summary-new)))


(hilit-set-mode-patterns
 '(emacs-lisp-mode lisp-mode)
 '(
   (";.*" nil comment)
;;;	 ("^;.*$" nil comment)
;;;	 ("\\s ;+[ ;].*$" nil comment)
   (hilit-string-find ?\\ string)
   ("^\\s *(def\\(un\\|macro\\|advice\\|subst\\)\\s " "\\()\\|nil\\)" defun)
   ("^\\s *(defvar\\s +\\S +" nil decl)
   ("^\\s *(defconst\\s +\\S +" nil define)
   ("^\\s *(\\(provide\\|require\\|\\(auto\\)?load\\).*$" nil include)
   ))


(hilit-set-mode-patterns
 'plain-tex-mode
 '(("^%%.*$" nil comment)
   ("{\\\\em\\([^}]+\\)}" nil comment)
   ("\\(\\\\\\w+\\)" nil keyword)
   ("{\\\\bf\\([^}]+\\)}" nil keyword)
   ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" nil defun)
   ("\\\\\\(begin\\|end\\){\\([A-Za-z0-9\\*]+\\)}" nil defun)
;   ("[^\\\\]\\$\\([^$]*\\)\\$" nil string)
   ("\\$\\([^$]*\\)\\$" nil string)
   ))

;; Reasonable extensions would include smarter parameter handling for such
;; things as the .IX and .I macros, which alternate the handling of following
;; arguments.

(hilit-set-mode-patterns
 'nroff-mode
 '(("^\\.[\\\][\\\"].*$" nil comment)
   ("^\\.so .*$" nil include)
   ("^\\.[ST]H.*$" nil defun)
;;   ("^[^\\.].*\"[^\\\"]*\\(\\\\\\(.\\)[^\\\"]*\\)*\"" nil string)
   ("\"" "[^\\]\"" string)
   ("^\\.[A-Za-z12\\\\].*$" nil define)
   ("\\([\\\][^ ]*\\)" nil keyword)
   ("^\\.[a-zA-Z].*$" nil keyword)))

(hilit-set-mode-patterns
 'texinfo-mode
 '(("^\\(@c\\|@comment\\)\\>.*$" nil comment)
   ("@\\(emph\\|strong\\|b\\|i\\){[^}]+}" nil comment)
; seems broken
;   ("\\$[^$]*\\$" nil string)
   ("@\\(file\\|kbd\\|key\\){[^}]+}" nil string)
   ("^\\*.*$" nil defun)
   ("@\\(if\\w+\\|format\\|item\\)\\b.*$" nil defun)
   ("@end +[A-Za-z0-9]+[ \t]*$" nil defun)
   ("@\\(samp\\|code\\|var\\){[^}]+}" nil defun)
   ("@\\w+\\({[^}]+}\\)?" nil keyword)
   ))

(hilit-set-mode-patterns
 'dired-mode
 (append
  '(("^D.*$"  nil dired-deleted)
   ("^\\*.*$" nil dired-marked)
   ("^  d.*$" nil dired-directory)
   ("^  l.*$" nil dired-link)
   ("^  -.*#.*#$" nil dired-ignored))
  (list (cons
	 (concat "^  .*\\("
		 (mapconcat 'regexp-quote completion-ignored-extensions "\\|")
		 "\\)$")
	 '(nil dired-ignored)))))

(hilit-set-mode-patterns
 'jargon-mode
 '(("^:[^:]*:" nil jargon-entry)
   ("{[^}]*}+" nil jargon-xref)))

(hilit-set-mode-patterns
 'Info-mode
 '(("^\\* [^:]+:+" nil jargon-entry)
   ("\\*[Nn]ote\\b[^:]+:+" nil jargon-xref)
   ("  \\(Next\\|Prev\\|Up\\):" nil jargon-xref)
   ("- \\(Variable\\|Function\\|Macro\\|Command\\|Special Form\\|User Option\\):.*$"
    nil jargon-keyword)))	; lisp manual

(provide 'hilit19)

;;; hilit19 ends here.
