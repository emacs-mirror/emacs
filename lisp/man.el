;;; man.el --- browse UNIX manual pages

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author:		Barry A. Warsaw <bwarsaw@cen.com>
;; Keywords:		help
;; Adapted-By:		ESR, pot

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

;; This code provides a function, `man', with which you can browse
;; UNIX manual pages.  Formatting is done in background so that you
;; can continue to use your Emacs while processing is going on.
;;
;; The mode also supports hypertext-like following of manual page SEE
;; ALSO references, and other features.  See below or do `?' in a
;; manual page buffer for details.

;; ========== Credits and History ========== 
;; In mid 1991, several people posted some interesting improvements to
;; man.el from the standard emacs 18.57 distribution.  I liked many of
;; these, but wanted everthing in one single package, so I decided
;; to incorporate them into a single manual browsing mode.  While
;; much of the code here has been rewritten, and some features added,
;; these folks deserve lots of credit for providing the initial
;; excellent packages on which this one is based.

;; Nick Duffek <duffek@chaos.cs.brandeis.edu>, posted a very nice
;; improvement which retrieved and cleaned the manpages in a
;; background process, and which correctly deciphered such options as
;; man -k.

;; Eric Rose <erose@jessica.stanford.edu>, submitted manual.el which
;; provided a very nice manual browsing mode.

;; This package was available as `superman.el' from the LCD package
;; for some time before it was accepted into Emacs 19.  The entry
;; point and some other names have been changed to make it a drop-in
;; replacement for the old man.el package.

;; Francesco Potorti` <pot@cnuce.cnr.it> cleaned it up thoroughly,
;; making it faster, more robust and more tolerant of different
;; systems' man idiosynchrasies.

;; ========== Features ==========
;; + Runs "man" in the background and pipes the results through a
;;   series of sed and awk scripts so that all retrieving and cleaning
;;   is done in the background. The cleaning commands are configurable.
;; + Syntax is the same as Un*x man
;; + Functionality is the same as Un*x man, including "man -k" and
;;   "man <section>", etc.
;; + Provides a manual browsing mode with keybindings for traversing
;;   the sections of a manpage, following references in the SEE ALSO
;;   section, and more.
;; + Multiple manpages created with the same man command are put into
;;   a narrowed buffer circular list.

;; ============= TODO ===========
;; - Add a command for printing.
;; - The awk script deletes multiple blank lines.  This behaviour does
;;   not allow to understand if there was indeed a blank line at the
;;   end or beginning of a page (after the header, or before the
;;   footer).  A different algorithm should be used.  It is easy to
;;   compute how many blank lines there are before and after the page
;;   headers, and after the page footer.  But it is possible to compute
;;   the number of blank lines before the page footer by euristhics
;;   only.  Is it worth doing?
;; - Allow a user option to mean that all the manpages should go in
;;   the same buffer, where they can be browsed with M-n and M-p.
;; - Allow completion on the manpage name when calling man.  This
;;   requires a reliable list of places where manpages can be found.  The
;;   drawback would be that if the list is not complete, the user might
;;   be led to believe that the manpages in the missing directories do
;;   not exist.


;;; Code:

(require 'assoc)

;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; empty defvars (keep the compiler quiet)

(defvar Man-notify)
(defvar Man-current-page)
(defvar Man-page-list)
(defvar Man-filter-list)
(defvar Man-original-frame)
(defvar Man-arguments)
(defvar Man-sections-alist)
(defvar Man-refpages-alist)
(defvar Man-uses-untabify-flag)
(defvar Man-page-mode-string)
(defvar Man-sed-script)

;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; user variables

(defvar Man-fontify-manpage-flag t
  "*Make up the manpage with fonts.")

(defvar Man-overstrike-face 'bold
  "*Face to use when fontifying overstrike.")

(defvar Man-underline-face 'underline
  "*Face to use when fontifying underlinining.")

;; Use the value of the obsolete user option Man-notify, if set.
(defvar Man-notify-method (if (boundp 'Man-notify) Man-notify 'friendly)
  "*Selects the behavior when manpage is ready.
This variable may have one of the following values, where (sf) means
that the frames are switched, so the manpage is displayed in the frame
where the man command was called from:

newframe   -- put the manpage in its own frame (see `Man-frame-parameters')
pushy      -- make the manpage the current buffer in the current window
bully      -- make the manpage the current buffer and only window (sf)
aggressive -- make the manpage the current buffer in the other window (sf)
friendly   -- display manpage in the other window but don't make current (sf)
polite     -- don't display manpage, but prints message and beep when ready
quiet      -- like `polite', but don't beep
meek       -- make no indication that the manpage is ready

Any other value of `Man-notify-method' is equivalent to `meek'.")

(defvar Man-frame-parameters nil
  "*Frame parameter list for creating a new frame for a manual page.")

(defvar Man-downcase-section-letters-flag t
  "*Letters in sections are converted to lower case.
Some Un*x man commands can't handle uppercase letters in sections, for
example \"man 2V chmod\", but they are often displayed in the manpage
with the upper case letter.  When this variable is t, the section
letter (e.g., \"2V\") is converted to lowercase (e.g., \"2v\") before
being sent to the man background process.")

(defvar Man-circular-pages-flag t
  "*If t, the manpage list is treated as circular for traversal.")

(defvar Man-section-translations-alist
  (list
   '("3C++" . "3")
   ;; Some systems have a real 3x man section, so let's comment this.
   ;; '("3X" . "3")                        ; Xlib man pages
   '("3X11" . "3")
   '("1-UCB" . ""))
  "*Association list of bogus sections to real section numbers.
Some manpages (e.g. the Sun C++ 2.1 manpages) have section numbers in
their references which Un*x `man' does not recognize.  This
association list is used to translate those sections, when found, to
the associated section number.")

(defvar manual-program "man"
  "The name of the program that produces man pages.")

(defvar Man-untabify-command "pr"
  "Command used for untabifying.")

(defvar Man-untabify-command-args (list "-t" "-e")
  "List of arguments to be passed to Man-untabify-command (which see).")

(defvar Man-sed-command "sed"
  "Command used for processing sed scripts.")

(defvar Man-awk-command "awk"
  "Command used for processing awk scripts.")

(defvar Man-mode-line-format
  '("" mode-line-modified
       mode-line-buffer-identification "  "
       global-mode-string
       " " Man-page-mode-string
       "  %[(" mode-name mode-line-process minor-mode-alist ")%]----"
       (-3 . "%p") "-%-")
  "Mode line format for manual mode buffer.")

(defvar Man-mode-map nil
  "Keymap for Man mode.")

(defvar Man-mode-hook nil
  "Hook run when Man mode is enabled.")

(defvar Man-cooked-hook nil
  "Hook run after removing backspaces but before Man-mode processing.")

(defvar Man-name-regexp "[-a-zA-Z0-9_][-a-zA-Z0-9_.]*"
  "Regular expression describing the name of a manpage (without section).")

(defvar Man-section-regexp "[0-9][a-zA-Z+]*\\|[LNln]"
  "Regular expression describing a manpage section within parentheses.")

(defvar Man-page-header-regexp
  (concat "^[ \t]*\\(" Man-name-regexp
	  "(\\(" Man-section-regexp "\\))\\).*\\1")
  "Regular expression describing the heading of a page.")

(defvar Man-heading-regexp "^\\([A-Z][A-Z ]+\\)$"
  "Regular expression describing a manpage heading entry.")

(defvar Man-see-also-regexp "SEE ALSO"
  "Regular expression for SEE ALSO heading (or your equivalent).
This regexp should not start with a `^' character.")

(defvar Man-first-heading-regexp "^[ \t]*NAME$\\|^[ \t]*No manual entry fo.*$"
  "Regular expression describing first heading on a manpage.
This regular expression should start with a `^' character.")

(defvar Man-reference-regexp
  (concat "\\(" Man-name-regexp "\\)(\\(" Man-section-regexp "\\))")
  "Regular expression describing a reference in the SEE ALSO section.")

(defvar Man-switches ""
  "Switches passed to the man command, as a single string.")

(defvar Man-specified-section-option
  (if (string-match "-solaris[0-9.]*$" system-configuration)
      "-s"
    "")
  "Option that indicates a specified a manual section name.")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end user variables

;; other variables and keymap initializations
(make-variable-buffer-local 'Man-sections-alist)
(make-variable-buffer-local 'Man-refpages-alist)
(make-variable-buffer-local 'Man-page-list)
(make-variable-buffer-local 'Man-current-page)
(make-variable-buffer-local 'Man-page-mode-string)
(make-variable-buffer-local 'Man-original-frame)
(make-variable-buffer-local 'Man-arguments)

(setq-default Man-sections-alist nil)
(setq-default Man-refpages-alist nil)
(setq-default Man-page-list nil)
(setq-default Man-current-page 0)
(setq-default Man-page-mode-string "1 of 1")

(defconst Man-sysv-sed-script "\
/\b/ {	s/_\b//g
	s/\b_//g
        s/o\b+/o/g
        s/+\bo/o/g
	:ovstrk
	s/\\(.\\)\b\\1/\\1/g
	t ovstrk
	}
/\e\\[[0-9][0-9]*m/ s///g"
  "Script for sysV-like sed to nuke backspaces and ANSI codes from manpages.")

(defconst Man-berkeley-sed-script "\
/\b/ {	s/_\b//g\\
	s/\b_//g\\
        s/o\b+/o/g\\
        s/+\bo/o/g\\
	:ovstrk\\
	s/\\(.\\)\b\\1/\\1/g\\
	t ovstrk\\
	}\\
/\e\\[[0-9][0-9]*m/ s///g"
  "Script for berkeley-like sed to nuke backspaces and ANSI codes from manpages.")

(if Man-mode-map
    nil
  (setq Man-mode-map (make-keymap))
  (suppress-keymap Man-mode-map)
  (define-key Man-mode-map " "    'scroll-up)
  (define-key Man-mode-map "\177" 'scroll-down)
  (define-key Man-mode-map "n"    'Man-next-section)
  (define-key Man-mode-map "p"    'Man-previous-section)
  (define-key Man-mode-map "\en"  'Man-next-manpage)
  (define-key Man-mode-map "\ep"  'Man-previous-manpage)
  (define-key Man-mode-map ">"    'end-of-buffer)
  (define-key Man-mode-map "<"    'beginning-of-buffer)
  (define-key Man-mode-map "."    'beginning-of-buffer)
  (define-key Man-mode-map "r"    'Man-follow-manual-reference)
  (define-key Man-mode-map "t"    'toggle-truncate-lines)
  (define-key Man-mode-map "g"    'Man-goto-section)
  (define-key Man-mode-map "s"    'Man-goto-see-also-section)
  (define-key Man-mode-map "k"    'Man-kill)
  (define-key Man-mode-map "q"    'Man-quit)
  (define-key Man-mode-map "m"    'man)
  (define-key Man-mode-map "?"    'describe-mode)
  )


;; ======================================================================
;; utilities

(defsubst Man-init-defvars ()
  "Used for initialising variables based on the value of window-system.
This is necessary if one wants to dump man.el with emacs."

  ;; The following is necessary until fonts are implemented on
  ;; terminals.
  (setq Man-fontify-manpage-flag (and Man-fontify-manpage-flag
				      window-system))

  (defconst Man-uses-untabify-flag t
    ;; don't use pr: it is buggy
    ;; (or (not (file-readable-p "/etc/passwd"))
    ;;   (/= 0 (apply 'call-process
    ;;                Man-untabify-command nil nil nil
    ;;                (append Man-untabify-command-args
    ;;                        (list "/etc/passwd")))))
    "When non-nil use `untabify' instead of Man-untabify-command.")

  (defconst Man-sed-script
    (cond
     (Man-fontify-manpage-flag
      nil)
     ((= 0 (call-process Man-sed-command nil nil nil Man-sysv-sed-script))
      Man-sysv-sed-script)
     ((= 0 (call-process Man-sed-command nil nil nil Man-berkeley-sed-script))
      Man-berkeley-sed-script)
     (t
      nil))
    "Script for sed to nuke backspaces and ANSI codes from manpages.")

  (defvar Man-filter-list
    (list
     (cons
      Man-sed-command
      (list
       (if Man-sed-script
	   (concat "-e '" Man-sed-script "'")
	 "")
       "-e '/^[\001-\032][\001-\032]*$/d'"
       "-e '/\e[789]/s///g'"
       "-e '/Reformatting page.  Wait/d'"
       "-e '/Reformatting entry.  Wait/d'"
       "-e '/^[ \t]*Hewlett-Packard[ \t]Company[ \t]*-[ \t][0-9]*[ \t]-/d'"
       "-e '/^[ \t]*Hewlett-Packard[ \t]*-[ \t][0-9]*[ \t]-.*$/d'"
       "-e '/^[ \t][ \t]*-[ \t][0-9]*[ \t]-[ \t]*Formatted:.*[0-9]$/d'"
       "-e '/^[ \t]*Page[ \t][0-9]*.*(printed[ \t][0-9\\/]*)$/d'"
       "-e '/^Printed[ \t][0-9].*[0-9]$/d'"
       "-e '/^[ \t]*X[ \t]Version[ \t]1[01].*Release[ \t][0-9]/d'"
       "-e '/^[A-za-z].*Last[ \t]change:/d'"
       "-e '/^Sun[ \t]Release[ \t][0-9].*[0-9]$/d'"
       "-e '/[ \t]*Copyright [0-9]* UNIX System Laboratories, Inc.$/d'"
       ))
     (cons
      Man-awk-command
      (list
       "'\n"
       "BEGIN { blankline=0; anonblank=0; }\n"
       "/^$/ { if (anonblank==0) next; }\n"
       "{ anonblank=1; }\n"
       "/^$/ { blankline++; next; }\n"
       "{ if (blankline>0) { print \"\"; blankline=0; } print $0; }\n"
       "'"
       ))
     (if (not Man-uses-untabify-flag)
	 (cons
	  Man-untabify-command
	  Man-untabify-command-args)
       ))
    "*Manpage cleaning filter command phrases.
This variable contains a list of the following form:

'((command-string phrase-string*)*)

Each phrase-string is concatenated onto the command-string to form a
command filter.  The (standard) output (and standard error) of the Un*x
man command is piped through each command filter in the order the
commands appear in the association list.  The final output is placed in
the manpage buffer.")
)

(defsubst Man-match-substring (&optional n string)
  "Return the substring matched by the last search.
Optional arg N means return the substring matched by the Nth paren
grouping.  Optional second arg STRING means return a substring from
that string instead of from the current buffer."
  (if (null n) (setq n 0))
  (if string
      (substring string (match-beginning n) (match-end n))
    (buffer-substring (match-beginning n) (match-end n))))

(defsubst Man-make-page-mode-string ()
  "Formats part of the mode line for Man mode."
  (format "%s page %d of %d"
	  (or (nth 2 (nth (1- Man-current-page) Man-page-list))
	      "")
	  Man-current-page
	  (length Man-page-list)))

(defsubst Man-build-man-command ()
  "Builds the entire background manpage and cleaning command."
  (let ((command (concat manual-program " " Man-switches " %s 2>/dev/null"))
	(flist Man-filter-list))
    (while (and flist (car flist))
      (let ((pcom (car (car flist)))
	    (pargs (cdr (car flist))))
	(setq command
	      (concat command " | " pcom " "
		      (mapconcat '(lambda (phrase)
				    (if (not (stringp phrase))
					(error "Malformed Man-filter-list"))
				    phrase)
				 pargs " ")))
	(setq flist (cdr flist))))
    command))

(defun Man-translate-references (ref)
  "Translates REF from \"chmod(2V)\" to \"2v chmod\" style.
Leave it as is if already in that style.  Possibly downcase and
translate the section (see the Man-downcase-section-letters-flag
and the Man-section-translations-alist variables)." 
  (let ((name "")
	(section "")
	(slist Man-section-translations-alist))
    (cond
     ;; "chmod(2V)" case ?
     ((string-match (concat Man-reference-regexp "$") ref)
      (setq name (Man-match-substring 1 ref)
	    section (Man-match-substring 2 ref)))
     ;; "2v chmod" case ?
     ((string-match (concat "\\(" Man-section-regexp
			    "\\) +\\(" Man-name-regexp "\\)$") ref)
      (setq name (Man-match-substring 2 ref)
	    section (Man-match-substring 1 ref))))
    (if (string= name "")
	ref				; Return the reference as is
      (if Man-downcase-section-letters-flag
	  (setq section (downcase section)))
      (while slist
	(let ((s1 (car (car slist)))
	      (s2 (cdr (car slist))))
	  (setq slist (cdr slist))
	  (if Man-downcase-section-letters-flag
	      (setq s1 (downcase s1)))
	  (if (not (string= s1 section)) nil
	    (setq section (if Man-downcase-section-letters-flag
			      (downcase s2)
			    s2)
		  slist nil))))
      (concat Man-specified-section-option section " " name))))


;; ======================================================================
;; default man entry: get word under point

(defsubst Man-default-man-entry ()
  "Make a guess at a default manual entry.
This guess is based on the text surrounding the cursor, and the
default section number is selected from `Man-auto-section-alist'."
  (let (default-title)
    (save-excursion
      
      ;; Default man entry title is any word the cursor is on, or if
      ;; cursor not on a word, then nearest preceding word.  Cannot
      ;; use the current-word function because it skips the dots.
      (if (not (looking-at "[-a-zA-Z_.]"))
	  (skip-chars-backward "^a-zA-Z"))
      (skip-chars-backward "-(a-zA-Z_0-9_.")
      (if (looking-at "(") (forward-char 1))
      (setq default-title
	    (buffer-substring
	     (point)
	     (progn (skip-chars-forward "-a-zA-Z0-9_.") (point))))
      
      ;; If looking at something like ioctl(2) or brc(1M), include the
      ;; section number in the returned value.  Remove text properties.
      (let ((result (concat
		     default-title
		     (if (looking-at
			  (concat "[ \t]*([ \t]*\\("
				  Man-section-regexp "\\)[ \t]*)"))
			 (format "(%s)" (Man-match-substring 1))))))
	(set-text-properties 0 (length result) nil result)
	result))))


;; ======================================================================
;; Top level command and background process sentinel

;; For compatibility with older versions.
;;;###autoload
(defalias 'manual-entry 'man)

;;;###autoload
(defun man (man-args)
  "Get a Un*x manual page and put it in a buffer.
This command is the top-level command in the man package.  It runs a Un*x
command to retrieve and clean a manpage in the background and places the
results in a Man mode (manpage browsing) buffer.  See variable
`Man-notify-method' for what happens when the buffer is ready.
If a buffer already exists for this man page, it will display immediately."
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
		(input (read-string
			(format "Manual entry%s: "
				(if (string= default-entry "")
				    ""
				  (format " (default %s)" default-entry))))))
	   (if (string= input "")
	       (if (string= default-entry "")
		   (error "No man args given")
		 default-entry)
	     input))))

  ;; Init the man package variables, if not already done.
  (Man-init-defvars)

  ;; Possibly translate the "subject(section)" syntax into the
  ;; "section subject" syntax and possibly downcase the section.
  (setq man-args (Man-translate-references man-args))

  (Man-getpage-in-background man-args))


(defun Man-getpage-in-background (topic)
  "Uses TOPIC to build and fire off the manpage and cleaning command."
  (let* ((man-args topic)
	 (bufname (concat "*Man " man-args "*"))
	 (buffer  (get-buffer bufname)))
    (if buffer
	(Man-notify-when-ready buffer)
      (require 'env)
      (message "Invoking %s %s in the background" manual-program man-args)
      (setq buffer (generate-new-buffer bufname))
      (save-excursion
	(set-buffer buffer)
	(setq Man-original-frame (selected-frame))
	(setq Man-arguments man-args))
      (let ((process-environment (copy-sequence process-environment)))
	;; Prevent any attempt to use display terminal fanciness.
	(setenv "TERM" "dumb")
	(set-process-sentinel
	 (start-process manual-program buffer "sh" "-c"
			(format (Man-build-man-command) man-args))
	 'Man-bgproc-sentinel)))))

(defun Man-notify-when-ready (man-buffer)
  "Notify the user when MAN-BUFFER is ready.
See the variable `Man-notify-method' for the different notification behaviors."
  (let ((saved-frame (save-excursion
		       (set-buffer man-buffer)
		       Man-original-frame)))
    (cond
     ((eq Man-notify-method 'newframe)
      ;; Since we run asynchronously, perhaps while Emacs is waiting
      ;; for input, we must not leave a different buffer current.  We
      ;; can't rely on the editor command loop to reselect the
      ;; selected window's buffer.
      (save-excursion
	(set-buffer man-buffer)
	(make-frame Man-frame-parameters)))
     ((eq Man-notify-method 'pushy)
      (switch-to-buffer man-buffer))
     ((eq Man-notify-method 'bully)
      (and window-system
	   (frame-live-p saved-frame)
	   (select-frame saved-frame))
      (pop-to-buffer man-buffer)
      (delete-other-windows))
     ((eq Man-notify-method 'aggressive)
      (and window-system
	   (frame-live-p saved-frame)
	   (select-frame saved-frame))
      (pop-to-buffer man-buffer))
     ((eq Man-notify-method 'friendly)
      (and window-system
	   (frame-live-p saved-frame)
	   (select-frame saved-frame))
      (display-buffer man-buffer 'not-this-window))
     ((eq Man-notify-method 'polite)
      (beep)
      (message "Manual buffer %s is ready" (buffer-name man-buffer)))
     ((eq Man-notify-method 'quiet)
      (message "Manual buffer %s is ready" (buffer-name man-buffer)))
     ((or (eq Man-notify-method 'meek)
	  t)
      (message ""))
     )))

(defun Man-fontify-manpage ()
  "Convert overstriking and underlining to the correct fonts.
Same for the ANSI bold and normal escape sequences."
  (interactive)
  (message "Please wait: making up the %s man page..." Man-arguments)
  (goto-char (point-min))
  (while (search-forward "\e[1m" nil t)
    (delete-backward-char 4)
    (put-text-property (point)
		       (progn (if (search-forward "\e[0m" nil 'move)
				  (delete-backward-char 4))
			      (point))
		       'face Man-overstrike-face))
  (goto-char (point-min))
  (while (search-forward "_\b" nil t)
    (backward-delete-char 2)
    (put-text-property (point) (1+ (point)) 'face Man-underline-face))
  (goto-char (point-min))
  (while (search-forward "\b_" nil t)
    (backward-delete-char 2)
    (put-text-property (1- (point)) (point) 'face Man-underline-face))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\)\\(\b\\1\\)+" nil t)
    (replace-match "\\1")
    (put-text-property (1- (point)) (point) 'face Man-overstrike-face))
  (goto-char (point-min))
  (while (re-search-forward "o\b\\+\\|\\+\bo" nil t)
    (replace-match "o")
    (put-text-property (1- (point)) (point) 'face 'bold))
  (goto-char (point-min))
  (while (re-search-forward "[-|]\\(\b[-|]\\)+" nil t)
    (replace-match "+")
    (put-text-property (1- (point)) (point) 'face 'bold))
  (message "%s man page made up" Man-arguments))

(defun Man-cleanup-manpage ()
  "Remove overstriking and underlining from the current buffer."
  (interactive)
  (message "Please wait: cleaning up the %s man page..."
	   Man-arguments)
  (if (or (interactive-p) (not Man-sed-script))
      (progn
	(goto-char (point-min))
	(while (search-forward "_\b" nil t) (backward-delete-char 2))
	(goto-char (point-min))
	(while (search-forward "\b_" nil t) (backward-delete-char 2))
	(goto-char (point-min))
	(while (re-search-forward "\\(.\\)\\(\b\\1\\)+" nil t)
	  (replace-match "\\1"))
	(goto-char (point-min))
	(while (re-search-forward "\e\\[[0-9]+m" nil t) (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "o\b\\+\\|\\+\bo" nil t) (replace-match "o"))
	))
  (goto-char (point-min))
  (while (re-search-forward "[-|]\\(\b[-|]\\)+" nil t) (replace-match "+"))
  (message "%s man page cleaned up" Man-arguments))

(defun Man-bgproc-sentinel (process msg)
  "Manpage background process sentinel."
  (let ((Man-buffer (process-buffer process))
	(delete-buff nil)
	(err-mess nil))

    (if (null (buffer-name Man-buffer)) ;; deleted buffer
	(set-process-buffer process nil)

      (save-excursion
	(set-buffer Man-buffer)
	(let ((case-fold-search nil))
	  (goto-char (point-min))
	  (cond ((or (looking-at "No \\(manual \\)*entry for")
		     (looking-at "[^\n]*: nothing appropriate$"))
		 (setq err-mess (buffer-substring (point)
						  (progn
						    (end-of-line) (point)))
		       delete-buff t))
		((not (and (eq (process-status process) 'exit)
			   (= (process-exit-status process) 0)))
		 (setq err-mess
		       (concat (buffer-name Man-buffer)
			       ": process "
			       (let ((eos (1- (length msg))))
				 (if (= (aref msg eos) ?\n)
				     (substring msg 0 eos) msg))))
		 (goto-char (point-max))
		 (insert (format "\nprocess %s" msg))
		 ))
	  (if delete-buff
	      (kill-buffer Man-buffer)
	    (if Man-fontify-manpage-flag
		(Man-fontify-manpage)
	      (Man-cleanup-manpage))
	    (run-hooks 'Man-cooked-hook)
	    (Man-mode)
	    (set-buffer-modified-p nil)
	    ))
	;; Restore case-fold-search before calling
	;; Man-notify-when-ready because it may switch buffers.

	(if (not delete-buff)
	    (Man-notify-when-ready Man-buffer))

	(if err-mess
	    (error err-mess))
	))))


;; ======================================================================
;; set up manual mode in buffer and build alists

(defun Man-mode ()
  "A mode for browsing Un*x manual pages.

The following man commands are available in the buffer. Try
\"\\[describe-key] <key> RET\" for more information:

\\[man]       Prompt to retrieve a new manpage.
\\[Man-follow-manual-reference]       Retrieve reference in SEE ALSO section.
\\[Man-next-manpage]   Jump to next manpage in circular list.
\\[Man-previous-manpage]   Jump to previous manpage in circular list.
\\[Man-next-section]       Jump to next manpage section.
\\[Man-previous-section]       Jump to previous manpage section.
\\[Man-goto-section]       Go to a manpage section.
\\[Man-goto-see-also-section]       Jumps to the SEE ALSO manpage section.
\\[Man-quit]       Deletes the manpage window, bury its buffer.
\\[Man-kill]       Deletes the manpage window, kill its buffer.
\\[describe-mode]       Prints this help text.

The following variables may be of some use. Try
\"\\[describe-variable] <variable-name> RET\" for more information:

Man-notify-method               What happens when manpage formatting is done.
Man-downcase-section-letters-flag  Force section letters to lower case.
Man-circular-pages-flag         Treat multiple manpage list as circular.
Man-auto-section-alist          List of major modes and their section numbers.
Man-section-translations-alist  List of section numbers and their Un*x equiv.
Man-filter-list                 Background manpage filter command.
Man-mode-line-format            Mode line format for Man mode buffers.
Man-mode-map                    Keymap bindings for Man mode buffers.
Man-mode-hook                   Normal hook run on entry to Man mode.
Man-section-regexp              Regexp describing manpage section letters.
Man-heading-regexp              Regexp describing section headers.
Man-see-also-regexp             Regexp for SEE ALSO section (or your equiv).
Man-first-heading-regexp        Regexp for first heading on a manpage.
Man-reference-regexp            Regexp matching a references in SEE ALSO.
Man-switches			Background `man' command switches.

The following key bindings are currently in effect in the buffer:
\\{Man-mode-map}"
  (interactive)
  (setq major-mode 'Man-mode
	mode-name "Man"
	buffer-auto-save-file-name nil
	mode-line-format Man-mode-line-format
	truncate-lines t
	buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (auto-fill-mode -1)
  (use-local-map Man-mode-map)
  (Man-build-page-list)
  (Man-strip-page-headers)
  (Man-unindent)
  (Man-goto-page 1)
  (run-hooks 'Man-mode-hook))

(defsubst Man-build-section-alist ()
  "Build the association list of manpage sections."
  (setq Man-sections-alist nil)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward Man-heading-regexp (point-max) t)
      (aput 'Man-sections-alist (Man-match-substring 1))
      (forward-line 1))))

(defsubst Man-build-references-alist ()
  "Build the association list of references (in the SEE ALSO section)."
  (setq Man-refpages-alist nil)
  (save-excursion
    (if (Man-find-section Man-see-also-regexp)
	(let ((start (progn (forward-line 1) (point)))
	      (end (progn
		     (Man-next-section 1)
		     (point)))
	      hyphenated
	      (runningpoint -1))
	  (save-restriction
	    (narrow-to-region start end)
	    (goto-char (point-min))
	    (back-to-indentation)
	    (while (and (not (eobp)) (/= (point) runningpoint))
	      (setq runningpoint (point))
	      (if (re-search-forward Man-reference-regexp end t)
		  (let* ((word (Man-match-substring 0))
			 (len (1- (length word))))
		    (if hyphenated
			(setq word (concat hyphenated word)
			      hyphenated nil))
		    (if (= (aref word len) ?-)
			(setq hyphenated (substring word 0 len))
		      (aput 'Man-refpages-alist word))))
	      (skip-chars-forward " \t\n,")))))))

(defun Man-build-page-list ()
  "Build the list of separate manpages in the buffer."
  (setq Man-page-list nil)
  (let ((page-start (point-min))
	(page-end (point-max))
	(header ""))
    (goto-char page-start)
    ;; (switch-to-buffer (current-buffer))(debug)
    (while (not (eobp))
      (setq header
	    (if (looking-at Man-page-header-regexp)
		(Man-match-substring 1)
	      nil))
      ;; Go past both the current and the next Man-first-heading-regexp
      (if (re-search-forward Man-first-heading-regexp nil 'move 2)
	  (let ((p (progn (beginning-of-line) (point))))
	    ;; We assume that the page header is delimited by blank
	    ;; lines and that it contains at most one blank line.  So
	    ;; if we back by three blank lines we will be sure to be
	    ;; before the page header but not before the possible
	    ;; previous page header.
	    (search-backward "\n\n" nil t 3)
	    (if (re-search-forward Man-page-header-regexp p 'move)
		(beginning-of-line))))
      (setq page-end (point))
      (setq Man-page-list (append Man-page-list
				  (list (list (copy-marker page-start)
					      (copy-marker page-end)
					      header))))
      (setq page-start page-end)
      )))

(defun Man-strip-page-headers ()
  "Strip all the page headers but the first from the manpage."
  (let ((buffer-read-only nil)
	(case-fold-search nil)
	(page-list Man-page-list)
	(page ())
	(header ""))
    (while page-list
      (setq page (car page-list))
      (and (nth 2 page)
	   (goto-char (car page))
	   (re-search-forward Man-first-heading-regexp nil t)
	   (setq header (buffer-substring (car page) (match-beginning 0)))
	   ;; Since the awk script collapses all successive blank
	   ;; lines into one, and since we don't want to get rid of
	   ;; the fast awk script, one must choose between adding
	   ;; spare blank lines between pages when there were none and
	   ;; deleting blank lines at page boundaries when there were
	   ;; some.  We choose the first, so we comment the following
	   ;; line.
	   ;; (setq header (concat "\n" header)))
	   (while (search-forward header (nth 1 page) t)
	     (replace-match "")))
      (setq page-list (cdr page-list)))))

(defun Man-unindent ()
  "Delete the leading spaces that indent the manpage."
  (let ((buffer-read-only nil)
	(case-fold-search nil)
	(page-list Man-page-list))
    (while page-list
      (let ((page (car page-list))
	    (indent "")
	    (nindent 0))
	(narrow-to-region (car page) (car (cdr page)))
	(if Man-uses-untabify-flag
	    (untabify (point-min) (point-max)))
	(if (catch 'unindent
	      (goto-char (point-min))
	      (if (not (re-search-forward Man-first-heading-regexp nil t))
		  (throw 'unindent nil))
	      (beginning-of-line)
	      (setq indent (buffer-substring (point)
					     (progn
					       (skip-chars-forward " ")
					       (point))))
	      (setq nindent (length indent))
	      (if (zerop nindent)
		  (throw 'unindent nil))
	      (setq indent (concat indent "\\|$"))
	      (goto-char (point-min))
	      (while (not (eobp))
		(if (looking-at indent)
		    (forward-line 1)
		  (throw 'unindent nil)))
	      (goto-char (point-min)))
	    (while (not (eobp))
	      (or (eolp)
		  (delete-char nindent))
	      (forward-line 1)))
	(setq page-list (cdr page-list))
	))))


;; ======================================================================
;; Man mode commands

(defun Man-next-section (n)
  "Move point to Nth next section (default 1)."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (looking-at Man-heading-regexp)
	(forward-line 1))
    (if (re-search-forward Man-heading-regexp (point-max) t n)
	(beginning-of-line)
      (goto-char (point-max)))))

(defun Man-previous-section (n)
  "Move point to Nth previous section (default 1)."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (looking-at Man-heading-regexp)
	(forward-line -1))
    (if (re-search-backward Man-heading-regexp (point-min) t n)
	(beginning-of-line)
      (goto-char (point-min)))))

(defun Man-find-section (section)
  "Move point to SECTION if it exists, otherwise don't move point.
Returns t if section is found, nil otherwise."
  (let ((curpos (point))
	(case-fold-search nil))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" section) (point-max) t)
	(progn (beginning-of-line) t)
      (goto-char curpos)
      nil)
    ))

(defun Man-goto-section ()
  "Query for section to move point to."
  (interactive)
  (aput 'Man-sections-alist
	(let* ((default (aheadsym Man-sections-alist))
	       (completion-ignore-case t)
	       chosen
	       (prompt (concat "Go to section: (default " default ") ")))
	  (setq chosen (completing-read prompt Man-sections-alist))
	  (if (or (not chosen)
		  (string= chosen ""))
	      default
	    chosen)))
  (Man-find-section (aheadsym Man-sections-alist)))

(defun Man-goto-see-also-section ()
  "Move point the the \"SEE ALSO\" section.
Actually the section moved to is described by `Man-see-also-regexp'."
  (interactive)
  (if (not (Man-find-section Man-see-also-regexp))
      (error (concat "No " Man-see-also-regexp
		     " section found in the current manpage"))))

(defun Man-follow-manual-reference (reference)
  "Get one of the manpages referred to in the \"SEE ALSO\" section.
Specify which reference to use; default is based on word at point."
  (interactive
   (if (not Man-refpages-alist)
       (error "There are no references in the current man page")
     (list (let* ((default (or
			     (car (all-completions
				   (save-excursion
				     (skip-syntax-backward "w()")
				     (skip-chars-forward " \t")
				     (let ((word (current-word)))
				       ;; strip a trailing '-':
				       (if (string-match "-$" word)
					   (substring word 0
						      (match-beginning 0))
					 word)))
				   Man-refpages-alist))
			     (aheadsym Man-refpages-alist)))
		   chosen
		   (prompt (concat "Refer to: (default " default ") ")))
	      (setq chosen (completing-read prompt Man-refpages-alist nil t))
	      (if (or (not chosen)
		      (string= chosen ""))
		  default
		chosen)))))
  (if (not Man-refpages-alist)
      (error "Can't find any references in the current manpage")
    (aput 'Man-refpages-alist reference)
    (Man-getpage-in-background
     (Man-translate-references (aheadsym Man-refpages-alist)))))

(defun Man-kill ()
  "Kill the buffer containing the manpage."
  (interactive)
  (let ((buff (current-buffer)))
    (delete-windows-on buff)
    (kill-buffer buff))
  (if (and window-system
	   (or (eq Man-notify-method 'newframe)
	       (and pop-up-frames
		    (eq Man-notify-method 'bully))))
      (delete-frame)))

(defun Man-quit ()
  "Bury the buffer containing the manpage."
  (interactive)
  (let ((buff (current-buffer)))
    (delete-windows-on buff)
    (bury-buffer buff))
  (if (and window-system
	   (or (eq Man-notify-method 'newframe)
	       (and pop-up-frames
		    (eq Man-notify-method 'bully))))
      (delete-frame)))

(defun Man-goto-page (page)
  "Go to the manual page on page PAGE."
  (interactive
   (if (not Man-page-list)
       (let ((args Man-arguments))
	 (kill-buffer (current-buffer))
	 (error "Can't find the %s manpage" args))
     (if (= (length Man-page-list) 1)
	 (error "You're looking at the only manpage in the buffer")
       (list (read-minibuffer (format "Go to manpage [1-%d]: "
				      (length Man-page-list)))))))
  (if (not Man-page-list)
      (let ((args Man-arguments))
	(kill-buffer (current-buffer))
	(error "Can't find the %s manpage" args)))
  (if (or (< page 1)
	  (> page (length Man-page-list)))
      (error "No manpage %d found" page))
  (let* ((page-range (nth (1- page) Man-page-list))
	 (page-start (car page-range))
	 (page-end (car (cdr page-range))))
    (setq Man-current-page page
	  Man-page-mode-string (Man-make-page-mode-string))
    (widen)
    (goto-char page-start)
    (narrow-to-region page-start page-end)
    (Man-build-section-alist)
    (Man-build-references-alist)
    (goto-char (point-min))))


(defun Man-next-manpage ()
  "Find the next manpage entry in the buffer."
  (interactive)
  (if (= (length Man-page-list) 1)
      (error "This is the only manpage in the buffer"))
  (if (< Man-current-page (length Man-page-list))
      (Man-goto-page (1+ Man-current-page))
    (if Man-circular-pages-flag
	(Man-goto-page 1)
      (error "You're looking at the last manpage in the buffer"))))

(defun Man-previous-manpage ()
  "Find the previous manpage entry in the buffer."
  (interactive)
  (if (= (length Man-page-list) 1)
      (error "This is the only manpage in the buffer"))
  (if (> Man-current-page 1)
      (Man-goto-page (1- Man-current-page))
    (if Man-circular-pages-flag
	(Man-goto-page (length Man-page-list))
      (error "You're looking at the first manpage in the buffer"))))

(provide 'man)

;;; man.el ends here
