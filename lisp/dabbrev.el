;;; new-dabbrev.el --- dynamic abbreviation package
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; Author: Don Morrison
;; Maintainer: Lars Lindberg <Lars.Lindberg@sypro.cap.se>
;; Created: 16 Mars 1992
;; Version: 4.4.2 beta
(defun dabbrev--version () "4.4.2 beta")
;; Keywords: abbrev expand completion

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

;;; Commentary:

;; The purpose with this package is to let you write just a few
;; characters of words you've written earlier to be able to expand
;; them.
;;
;; To expand a word, just put the point right after the word and press
;; M-/ (dabbrev-expand) or M-C-/ (dabbrev-completion).
;;
;; There are powerful things in this package that aren't turned on by
;; default. I recommend you to do the following.
;;
;; Put the following 2 lines in your .emacs file:
;; (setq dabbrev-always-check-other-buffers t)
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
;;
;; Dabbrev will now search in all buffers with the same major mode for
;; your expansions. It will also search for complete symbols, the old
;; dabbrev package only looked half-heartedly for symbols.
;;
;; Check out the customizable variables below to learn about all the
;; features of this package.

;;; Hints and tips for major modes writers:

;; Recommended values		C/Lisp etc	text
;; dabbrev-case-fold-search	nil		t
;; dabbrev-case-replace		nil		t
;;
;; Set the variables you want special for your mode like this:
;; (set (make-local-variable 'dabbrev-case-replace) nil)
;; Then you don't interfer with other modes.
;;
;; If your mode handles buffers that refers to other buffers
;; (i.e. compilation-mode, gud-mode), then try to set
;; `dabbrev-select-buffers-function' or `dabbrev-friend-buffer-function'
;; to a function that point out those buffers.

;; Same goes for major-modes that are connected to other modes.  There
;; are for instance a number of mail-modes.  One for reading, one for
;; creating a new mail etc.  Maybe those should be connected.

;; Example for GNUS (when we write a reply, we want dabbrev to look in
;; the article for expansion):
;; (set (make-local-variable 'dabbrev-friend-buffer-function)
;;      (lambda (buffer)
;;         (save-excursion
;;           (set-buffer buffer)
;;           (memq major-mode '(news-reply-mode gnus-article-mode)))))

;;; Change Log
;;  4.4.2 1994-11-25
;;      Added new variable; `dabbrev-check-rest-of-buffers'.
;;  4.4.1 1994-11-24
;;      Now has correct creation date.
;;      Added kill-ring idea to "Future enhancements".
;;  4.4 1994-11-22
;;	Changed the copyright text.  Thanks [hymie].
;;	new-dabbrev now handles abbrevs that starts with symbol
;;	syntax. Thanks [burgett] for kicking me to do it.
;;	Now also handles `dabbrev-abbrev-skip-leading-regexp' better.
;;  4.3 1994-11-14
;;	Now only displays "Expansion found in <buffer>" when the
;;	expansion has been found in buffers that hasn't been examined
;;	yet. Thanks [kifer].
;;      Added a new variable; `dabbrev-search-these-buffers-only'.
;;      Fixed bug in mini-buffer completion. Thanks [kifer].
;;      Found a real time-waster when using `dabbrev-completion'.
;;      Thanks for the elp.el package Barry Warsaw!
;;	Found bug that made point move in other buffers.
;;      Now handles Lucid emacs define-key style.
;;      Thanks [jules].
;;      Now uses the `<symbol>' syntax in doc strings.
;;      Cosmetic bugs and syntactical bugs in documentation strings.
;;      Thanks [hawley].
;;  4.2 1994-03-04
;;      Now searches other buffers again. Thanks [ake].
;;  4.1 1994-02-22
;;      Introduced the new variables `dabbrev-case-fold-search' and
;;      `dabbrev-case-replace'.
;;      Introduced the new variable `dabbrev-select-buffers-function'.
;;      Introduced the new variable `dabbrev-abbrev-skip-leading-regexp'.
;;      Changed `dabbrev-always-check-other-buffers' to not buffer local.
;;      Thanks [kifer].
;;      Bug in `dabbrev-completion', x expanded to xxy instead of xy.
;;      Thanks [kifer].
;;      Added `dabbrev-submit-feedback' for better error-reporting.
;;      The hooks (`dabbrev-select-buffers-function' and
;;      `dabbrev-friend-buffer-function') are now not automatically
;;      buffer local.
;;      Now provides dabbrev too.
;;  3.2 1993-12-14
;;      Message for expansion found in buffer other than current.
;;	Minor bugs.
;;  3.1 1993-12-13
;;      Better comment for `dabbrev-abbrev-char-regexp'.
;;  3.0 1993-12-09
;;      Freshed things up for the release.
;;      `dabbrev-completion' now doesn't have to use the minibuffer.
;;      Thanks [alon].
;;  2.0 1993-12-02 Lars Lindberg <lli@sypro.cap.se>
;;      Searches in other buffers for the expansion.
;;      Works also for the minibuffer.
;;      Added `dabbrev-completion'.
;;      More efficient code.
;;      Found minor bugs.
;;  1.0 converted to Emacs Lisp by Spencer Thomas.
;;      Thoroughly cleaned up by Richard Stallman.
;;  0.0
;;      DABBREVS - "Dynamic abbreviations" hack, originally written by
;;      Don Morrison for Twenex Emacs.  Converted to mlisp by Russ Fish.
;;      Supports the table feature to avoid hitting the same expansion on
;;      re-expand, and the search size limit variable.

;; Known bugs and limitations.
;; - Possible to do several levels of `dabbrev-completion' in the
;;   minibuffer.
;; - dabbrev-completion doesn't handle resetting the globals variables
;;   right.  It resets them after finding the abbrev.

;; Future enhancements
;;  - Check the tags-files? Like tags-complete?
;;  - Add the possibility of searching both forward and backward to
;;    the nearest expansion.
;;  - Check the kill-ring when everything else fails. (Maybe something
;;  for hippie-expand?). [Bng] <boris@cs.rochester.edu>

;;; Thanks goes to
;;  [hymie]	Hyman Rosen <marks!hymie@jyacc.jyacc.com>
;;  [burgett]	Steve Burgett <burgett@bizet.eecs.berkeley.edu>
;;  [jules]	Julian Gosnell <jules@x.co.uk>
;;  [kifer]	Michael Kifer <kifer@sbcs.sunysb.edu>
;;  [ake]	Ake Stenhoff <extaksf@aom.ericsson.se>
;;  [alon]	Alon Albert <al%imercury@uunet.uu.net>
;;  [tromey]	Tom Tromey <tromey@busco.lanl.gov>
;;  [Rolf]	Rolf Schreiber <rolf@mathematik.uni-stuttgart.de>
;;  [Petri]	Petri Raitio <per@tekla.fi>
;;  [ejb]	Jay Berkenbilt <ejb@ERA.COM>
;;  [hawley]	Bob Hawley <rth1@quartet.mt.att.com>
;;  ... and to all the people who have participated in the beta tests.

;;; Code:
(require 'cl)

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
;;; Customization variables
;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
(defvar dabbrev-backward-only nil
  "*If non-NIL, `dabbrev-expand' only looks backwards.")

(defvar dabbrev-limit nil
  "*Limits region searched by `dabbrev-expand' to this many chars away.")

(defvar dabbrev-abbrev-skip-leading-regexp nil
  "*Regexp for skipping leading characters of an abbreviation.

Example: Set this to \"\\\\$\" for programming languages that sometimes
has and sometimes has not a leading $ for variable names.

Set this to nil if no characters should be skipped.")

;; I recommend that you set this to nil.
(defvar dabbrev-case-fold-search 'case-fold-search
  "*T if dabbrev searches should ignore case.
nil if case is significant.
Non-nil and not t means evaluate for value.

Example:Setting this to 'case-fold-search means evaluate that variable
to see if it is t or non-nil.")

(defvar dabbrev-upcase-means-case-search nil
  "*The significance of an uppercase character in an abbreviation.

This variable only makes sense when the value of
`dabbrev-case-fold-search' evaluates to t.

nil = case fold search
t = case sensitive search")

;; I recommend that you set this to nil.
(defvar dabbrev-case-replace 'case-replace
  "*T if dabbrev should preserve case when expanding the abbreviation.
nil if it should not.
Non-nil and not t means evaluate for value.

This variable only makes sense when the value of
`dabbrev-case-fold-search' evaluates to t.

Example: Setting this to 'case-replace means evaluate that variable to
see if it is t or non-nil.")

;; I recommend that you set this to "\\sw\\|\\s_"
(defvar dabbrev-abbrev-char-regexp nil
  "*A regexp that recognizes a character in an abbreviation or an
expansion.  Will be surrounded with \\\\( ... \\\\) when used.

Set this to \"\\\\sw\" if you want ordinary words or
\"\\\\sw\\\\|\\\\s_\" if you want symbols.

You can also set it to nil if you want old-style dabbrev searching
(the abbreviation is from point to previous word-start, the
search is for symbols).

For instance, if you are programming in Lisp, yes-or-no-p is a symbol,
while 'yes', 'or', 'no' and 'p' are considered words.  If you set this
variable to nil, then expanding yes-or-no- will look for a symbol
starting with or containing 'no-'.  If you set this variable to
\"\\\\sw\\\\|\\\\s_\" dabbrev will look for a symbol starting with
\"yes-or-no-\". Finally, if you set this variable to \"\\\\sw\", then an
error will be signalled, because \"-\" is not part of a word but if you
try to expand \"yes-or-no\", dabbrev will look for a word starting with
\"no\".

The recommended value is \"\\\\sw\\\\|\\\\s_\".")

;; I recommend that you set this to t.
(defvar dabbrev-check-rest-of-buffers nil
  "*Should dabbrev package search in all buffers?.

Non-nil means first look in buffers pointed out by
`dabbrev-select-buffers-function' and then look in the rest of the
buffers.")


;; I recommend that you set this to t.
(defvar dabbrev-always-check-other-buffers nil
  "*Should \\[dabbrev-expand] look in other buffers?\
nil = Don't look in other buffers.\n\
t = Look in other buffers.\n\
Value other than nil and t = ask the user if he want's to look in
other buffers.

The recommended value is t.")

;; I guess setting this to a function that selects all C- or C++-
;; mode buffers would be a good choice for a debugging buffer,
;; when debugging C- or C++-code.
(defvar dabbrev-select-buffers-function 'dabbrev--select-buffers
  "A function that selects buffers that should be searched by dabbrev.

The function should take no arguments and return a list of buffers to
search for expansions.  Have a look at `dabbrev--select-buffers' for
an example.

A mode setting this variable should make it buffer local.")

(defvar dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p
  "*A function to check if OTHER-BUFFER should be searched by dabbrev.

The function should take one argument, OTHER-BUFFER, and return
non-nil if that buffer should be searched.  Have a look at
`dabbrev--same-major-mode-p' for an example.

Setting this makes sense only if the function pointed out by
`dabbrev-select-buffers-function' uses it.  The package function
`dabbrev--select-buffers' is such a function.

A mode setting this variable should make it buffer local.")

(defvar dabbrev-search-these-buffers-only nil
  "Should be a list of buffers if non-nil.

Dabbrev search will only look in these buffers. It will not even look
in the current buffer if it is not a member of this list.")

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
;;; Internal variables
;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

;; Last obarray of completions in `dabbrev-completion'
(defvar dabbrev--last-obarray nil)

;; Table of expansions seen so far
(defvar dabbrev--last-table nil)

;; Last string we tried to expand.
(defvar dabbrev--last-abbreviation nil)

;; Location last abbreviation began
(defvar dabbrev--last-abbrev-location nil)

;; Direction of last dabbrevs search
(defvar dabbrev--last-direction 0)

;; Last expansion of an abbreviation.
(defvar dabbrev--last-expansion nil)

;; Location the last expansion was found.
(defvar dabbrev--last-expansion-location nil)

;; The list of remaining buffers with the same mode as current buffer.
(defvar dabbrev--friend-buffer-list nil)

;; The buffer we looked in last.
(defvar dabbrev--last-buffer nil)

;; The buffer we found the expansion last time.
(defvar dabbrev--last-buffer-found nil)

;; The buffer we last did a completion in.
(defvar dabbrev--last-completion-buffer nil)

;; Same as dabbrev-always-check-other-buffers, but is set for every expand.
(defvar dabbrev--check-other-buffers dabbrev-always-check-other-buffers)

;; The regexp for recognizing a character in an abbreviation.
(defvar dabbrev--abbrev-char-regexp nil)

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
;;; Macros
;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

;;; Get the buffer that mini-buffer was activated from
(defsubst dabbrev--minibuffer-origin ()
  (car (cdr (buffer-list))))

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
;;; Exported functions
;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

;;;###autoload
(define-key esc-map "/" 'dabbrev-expand)
;;;###autoload
(if (string-match "Lucid$" emacs-version)
    (define-key esc-map [(control /)] 'dabbrev-completion)
  (define-key esc-map [?\C-/] 'dabbrev-completion))

;;;###autoload
(defun dabbrev-completion (&optional arg)
  "Completion on current word.

Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

If you call this function with prefix ARG, then it searches all
buffers accepted by the function pointed out by
`dabbrev-friend-buffer-function' to find the completions.

With no prefix ARG it tries to reuse the old completion list
before making a new one."

  (interactive "*P")
  (let* ((dabbrev-always-check-other-buffers (and arg t))
	 (abbrev (dabbrev--abbrev-at-point))
	 (ignore-case-p  (and (eval dabbrev-case-fold-search)
				(or (not dabbrev-upcase-means-case-search)
				    (string= abbrev (downcase abbrev)))))
	 (my-obarray dabbrev--last-obarray)
	 init)
    (save-excursion
      (if (and (null arg)
	       my-obarray
	       (or (eq dabbrev--last-completion-buffer (current-buffer))
		   (and (window-minibuffer-p (selected-window))
			(eq dabbrev--last-completion-buffer
			    (dabbrev--minibuffer-origin))))
	       dabbrev--last-abbreviation
	       (>= (length abbrev) (length dabbrev--last-abbreviation))
	       (string= dabbrev--last-abbreviation
			(substring abbrev 0
				   (length dabbrev--last-abbreviation)))
	       (setq init (try-completion abbrev my-obarray)))
	  ;;--------------------------------
	  ;; This is a continue.
	  ;;--------------------------------
	  (progn)
	;;--------------------------------
	;; New abbreviation to expand.
	;;--------------------------------
	(dabbrev--reset-global-variables)
	(setq dabbrev--last-abbreviation abbrev)
	;; Find all expansion
	(let ((completion-list
	       (dabbrev--find-all-expansions abbrev ignore-case-p)))
	  ;; Make an obarray with all expansions
	  (setq my-obarray (make-vector (length completion-list) 0))
	  (or (> (length my-obarray) 0)
	      (error "No dynamic expansion for \"%s\" found%s."
		     abbrev
		     (if dabbrev--check-other-buffers "" " in this-buffer")))
	  (cond
	   ((or (not ignore-case-p)
		(not dabbrev-case-replace))
	    (mapc (function (lambda (string)
			      (intern string my-obarray)))
		  completion-list))
	   ((string= abbrev (upcase abbrev))
	    (mapc (function (lambda (string)
			      (intern (upcase string) my-obarray)))
		  completion-list))
	   ((string= (substring abbrev 0 1)
		     (upcase (substring abbrev 0 1)))
	    (mapc (function (lambda (string)
			      (intern (dabbrev--capitalize string) my-obarray)))
		  completion-list))
	   (t
	    (mapc (function (lambda (string)
			      (intern (downcase string) my-obarray)))
		  completion-list)))
	  (setq dabbrev--last-obarray my-obarray)
	  (setq dabbrev--last-completion-buffer (current-buffer))
	  ;; Find the longest common string.
	  (setq init (try-completion abbrev my-obarray)))))
    ;;--------------------------------
    ;; Let the user choose between the expansions
    ;;--------------------------------
    (or (stringp init)
	(setq init abbrev))
    (cond
     ;; * Replace string fragment with matched common substring completion.
     ((and (not (string-equal init ""))
	   (not (string-equal (downcase init) (downcase abbrev))))
      (if (> (length (all-completions init my-obarray)) 1)
	  (message "Repeat '%s' to see all completions" this-command)
	(message "The only possible completion"))
      (dabbrev--substitute-expansion nil abbrev init))
     (t
      ;; * String is a common substring completion already.  Make list.
      (message "Making completion list...")
      (with-output-to-temp-buffer " *Completions*"
	(display-completion-list (all-completions init my-obarray)))
      (message "Making completion list... Done.")))
    (and (window-minibuffer-p (selected-window))
	 (message nil))))

;;;###autoload
(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function'.

A positive prefix argument, N, says to take the Nth backward _distinct_
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

To make it more powerful, make sure that
`dabbrev-always-check-other-buffers' is set to t.

Also check out `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]."
  (interactive "*P")
  (let (abbrev expansion old direction)
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    (save-excursion
      (if (and (null arg)
	       dabbrev--last-abbrev-location
	       (or (eq last-command this-command)
		   (and (window-minibuffer-p (selected-window))
			(= dabbrev--last-abbrev-location
			   (point)))))
	  ;;--------------------------------
	  ;; This is a redo.
	  ;;--------------------------------
	  (progn
	    (setq abbrev dabbrev--last-abbreviation)
	    (setq old dabbrev--last-expansion)
	    (setq direction dabbrev--last-direction))
	;;--------------------------------
	;; New abbreviation to expand.
	;;--------------------------------
	(dabbrev--reset-global-variables)
	(setq direction (if (null arg)
			    (if dabbrev-backward-only 1 0)
			  (prefix-numeric-value arg)))
	(setq abbrev (dabbrev--abbrev-at-point))
	(setq old nil))

      ;;--------------------------------
      ;; Find the expansion
      ;;--------------------------------
      (setq expansion
	    (dabbrev--find-expansion abbrev direction
				     (and (eval dabbrev-case-fold-search)
					  (or (not dabbrev-upcase-means-case-search)
					      (string= abbrev (downcase abbrev)))))))
    (cond
     ((not expansion)
      (dabbrev--reset-global-variables)
      (if old
	  (save-excursion
	    (search-backward (substring old (length abbrev)))
	    (delete-region (match-beginning 0) (match-end 0))))
      (error "No%s dynamic expansion for \"%s\" found."
	     (if old " further" "") abbrev))
     (t
      (if (not (eq dabbrev--last-buffer dabbrev--last-buffer-found))
	  (progn
	    (message "Expansion found in '%s'"
		     (buffer-name dabbrev--last-buffer))
	    (setq dabbrev--last-buffer-found dabbrev--last-buffer))
	(message nil))
      ;; Success: stick it in and return.
      (dabbrev--substitute-expansion old abbrev expansion)
      ;; Save state for re-expand.
      (setq dabbrev--last-expansion expansion)	
      (setq dabbrev--last-abbreviation abbrev)
      (setq dabbrev--last-abbrev-location (point-marker))))))

(eval-when-compile (require 'reporter))
(defun dabbrev-submit-feedback ()
  "Submit via mail a bug report on the dabbrev package."
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on the dabbrev package? ")
       (reporter-submit-bug-report
	"Lars Lindberg <lli@sypro.cap.se>"
	(format "new-dabbrev.el (Release %s)" (dabbrev--version))
	'(dabbrev-backward-only
	  dabbrev-limit
	  dabbrev-case-fold-search
	  dabbrev-case-replace
	  dabbrev-upcase-means-case-search
	  dabbrev-abbrev-char-regexp
	  dabbrev-always-check-other-buffers
	  dabbrev-select-buffers-function
	  dabbrev-friend-buffer-function)
	nil nil nil)))

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
;;; Local functions
;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

(defun dabbrev--capitalize (string)
  ;; Capitalize STRING (See capitalize-word)
  (let ((new-string ""))
    (save-match-data
      (while (string-match "\\w+" string)
	(let* ((mb (match-beginning 0))
	       (me (match-end 0))
	       (ms (substring string mb me)))
	  (setq new-string
		(concat new-string
			(substring string 0 mb)
			(upcase (substring ms 0 1))
			(downcase (substring ms 1))))
	  (setq string (substring string me)))))
    new-string))

;;; Checks if OTHER-BUFFER has the same major mode as current buffer.
(defun dabbrev--same-major-mode-p (other-buffer)
  (let ((orig-mode major-mode))
    (save-excursion
      (set-buffer other-buffer)
      (eq orig-mode major-mode))))

;;; Back over all abbrev type characters and then moves forward over
;;; all skip characters.
(defun dabbrev--goto-start-of-abbrev ()
  ;; Move backwards over abbrev chars
  (save-match-data
    (when (not (bobp))
      (forward-char -1)
      (while (and (looking-at dabbrev--abbrev-char-regexp)
		  (not (bobp)))
	(forward-char -1))
      (or (looking-at dabbrev--abbrev-char-regexp)
	  (forward-char 1)))
    (and dabbrev-abbrev-skip-leading-regexp
	 (while (looking-at dabbrev-abbrev-skip-leading-regexp)
	   (forward-char 1)))))

;;; Extract the symbol at point to serve as abbrevitation.
(defun dabbrev--abbrev-at-point ()
  ;; Check for error
  (save-excursion
    (save-match-data
      (if (or (bobp)
	      (progn 
		(forward-char -1)
		(not (looking-at (concat "\\("
					 (or dabbrev-abbrev-char-regexp
					     "\\sw\\|\\s_")
					 "\\)+")))))
	  (error "Not positioned immediately after an abbreviation."))))
  ;; Return abbrev at point
  (save-excursion
    (setq dabbrev--last-abbrev-location (point))
    (buffer-substring (point) 
		      (progn (dabbrev--goto-start-of-abbrev)
			     (point)))))
	
;;; Initializes all global variables
(defun dabbrev--reset-global-variables ()
  ;; dabbrev--last-obarray and dabbrev--last-completion-buffer
  ;; must not be reset here.
  (setq dabbrev--last-table nil
	dabbrev--last-abbreviation nil
	dabbrev--last-abbrev-location nil
	dabbrev--last-direction nil
	dabbrev--last-expansion nil
	dabbrev--last-expansion-location nil
	dabbrev--friend-buffer-list nil
	dabbrev--last-buffer nil
	dabbrev--last-buffer-found nil
	dabbrev--abbrev-char-regexp (or dabbrev-abbrev-char-regexp
					"\\sw\\|\\s_")
	dabbrev--check-other-buffers dabbrev-always-check-other-buffers))

;;; Find all buffers that are considered "friends" according to the
;;; function pointed out by dabbrev-friend-buffer-function.
(defun dabbrev--select-buffers ()
  (save-excursion
    (and (window-minibuffer-p (selected-window))
	 (set-buffer (dabbrev--minibuffer-origin)))
    (let ((orig-buffer (current-buffer)))
      (loop for buffer
	    in (buffer-list)
	    if (and (not (eq orig-buffer buffer))
		    (boundp 'dabbrev-friend-buffer-function)
		    (funcall dabbrev-friend-buffer-function buffer))
	    collect buffer))))

;;; Try to find ABBREV in REVERSE direction N times.
(defun dabbrev--try-find (abbrev reverse n ignore-case)
  (save-excursion
    (let ((case-fold-search-is-local (memq 'case-fold-search
					   (buffer-local-variables)))
	  (expansion nil))
      (and dabbrev--last-expansion-location
	   (goto-char dabbrev--last-expansion-location))
      (unwind-protect
	  (progn
	    (or case-fold-search-is-local
		(make-local-variable 'case-fold-search))
	    ;; Tricky! If `case-fold-search' isn't buffer-local, then
	    ;; this innocent let creates a buffer-local variable and
	    ;; when the let returns, it is still there!  The
	    ;; unwind-protect stuff around this makes sure that there
	    ;; exists one before the let, and removes it afterwards.
	    (let ((case-fold-search ignore-case))
	      (loop repeat n
		    while (setq expansion (dabbrev--search abbrev
							   reverse
							   ignore-case)))))
	(or case-fold-search-is-local
	    (kill-local-variable 'case-fold-search)))
      (and expansion
	   (setq dabbrev--last-expansion-location (point)))
      expansion)))

;;; Find all expansions of ABBREV
(defun dabbrev--find-all-expansions (abbrev ignore-case)
  (let ((all-expansions nil)
	expansion)
    (save-excursion
      (goto-char (point-min))
      (while (setq expansion (dabbrev--find-expansion abbrev -1 ignore-case))
	(push expansion all-expansions)))
    all-expansions))

(defun dabbrev--scanning-message ()
  (message "Scanning '%s'" (buffer-name (current-buffer))))

;;; Find one occasion of ABBREV.
;;; DIRECTION > 0 means look that many times backwards.
;;; DIRECTION < 0 means look that many times forward.
;;; DIRECTION = 0 means try both backward and forward.
;;; IGNORE-CASE non-nil means ignore case when searching.
(defun dabbrev--find-expansion (abbrev direction ignore-case)
  (let (expansion)
    (save-excursion
      (cond
       (dabbrev--last-buffer
	(set-buffer dabbrev--last-buffer)
	(dabbrev--scanning-message))
       ((and (not dabbrev-search-these-buffers-only)
	     (window-minibuffer-p (selected-window)))
	(set-buffer (dabbrev--minibuffer-origin))
	;; In the minibuffer-origin buffer we will only search from
	;; the top and down.
	(goto-char (point-min))
	(setq direction -1)
	(dabbrev--scanning-message)))
      (cond
       ;; ------------------------------------------
       ;; Look backwards
       ;; ------------------------------------------
       ((and (not dabbrev-search-these-buffers-only)
	     (>= direction 0)
	     (setq dabbrev--last-direction (min 1 direction))
	     (setq expansion (dabbrev--try-find abbrev t
						(max 1 direction)
						ignore-case)))
	expansion)
       ;; ------------------------------------------
       ;; Look forward
       ;; ------------------------------------------
       ((and (or (not dabbrev-search-these-buffers-only)
		 dabbrev--last-buffer)
	     (<= direction 0)
	     (setq dabbrev--last-direction -1)
	     (setq expansion (dabbrev--try-find abbrev nil
						(max 1 (- direction))
						ignore-case)))
	expansion)
       ;; ------------------------------------------
       ;; Look in other buffers.
       ;; Start at (point-min) and look forward.
       ;; ------------------------------------------
       (t
	(setq dabbrev--last-direction -1)
	;; Make sure that we should check other buffers
	(or dabbrev--friend-buffer-list
	    dabbrev--last-buffer
	    (setq dabbrev--friend-buffer-list
		  (mapcar (function get-buffer)
			  dabbrev-search-these-buffers-only))
	    (not dabbrev--check-other-buffers)
	    (not (or (eq dabbrev--check-other-buffers t)
		     (progn
		       (setq dabbrev--check-other-buffers
			     (y-or-n-p "Check in other buffers this time? ")))))
	    (let* (friend-buffer-list non-friend-buffer-list)
	      (setq dabbrev--friend-buffer-list
		    (funcall dabbrev-select-buffers-function))
	      (when dabbrev-check-rest-of-buffers
		(setq non-friend-buffer-list
		      (nreverse
		       (loop for buffer
			     in (buffer-list)
			     if (not (memq buffer dabbrev--friend-buffer-list))
			     collect buffer)))
		(setq dabbrev--friend-buffer-list
		      (append dabbrev--friend-buffer-list
			      non-friend-buffer-list)))))
	;; Walk through the buffers
	(while (and (not expansion) dabbrev--friend-buffer-list)
	  (setq dabbrev--last-buffer
		(car dabbrev--friend-buffer-list))
	  (setq dabbrev--friend-buffer-list
		(cdr dabbrev--friend-buffer-list))
	  (set-buffer dabbrev--last-buffer)
	  (dabbrev--scanning-message)
	  (setq dabbrev--last-expansion-location (point-min))
	  (setq expansion (dabbrev--try-find abbrev nil 1 ignore-case)))
	expansion)))))

(eval-when-compile (require 'picture))

(defun dabbrev--safe-replace-match (string &optional fixedcase literal)
  (if (eq major-mode 'picture-mode)
      (picture-replace-match string fixedcase literal)
    (replace-match string fixedcase literal)))

;;;----------------------------------------------------------------
;;; Substitute the current string in buffer with the expansion
;;; OLD is nil or the last expansion substring.
;;; ABBREV is the abbreviation we are working with.
;;; EXPANSION is the expansion substring.
(defun dabbrev--substitute-expansion (old abbrev expansion)
  ;;(undo-boundary)
  (let ((use-case-replace (and (eval dabbrev-case-fold-search)
			       (or (not dabbrev-upcase-means-case-search)
				   (string= abbrev (downcase abbrev)))
			       (eval dabbrev-case-replace))))
    (and nil use-case-replace
	 (setq old (concat abbrev (or old "")))
	 (setq expansion (concat abbrev expansion)))
    (if old
	(save-excursion
	  (search-backward old))
      ;;(store-match-data (list (point-marker) (point-marker)))
      (search-backward abbrev))
    ;; Make case of replacement conform to case of abbreviation
    ;; provided (1) that kind of thing is enabled in this buffer
    ;; and (2) the replacement itself is all lower case.
    (dabbrev--safe-replace-match expansion
				 (not use-case-replace)
				 t)))


;;;----------------------------------------------------------------
;;; Search function used by dabbrevs library.

;;; ABBREV is string to find as prefix of word. Second arg, REVERSE,
;;; is t for reverse search, nil for forward.  Variable dabbrev-limit
;;; controls the maximum search region size.  Third argment IGNORE-CASE
;;; non-nil means treat case as insignificant while looking for a match
;;; and when comparing with previous matches.  Also if that's non-nil
;;; and the match is found at the beginning of a sentence and is in
;;; lower case except for the initial then it is converted to all lower
;;; case for return.

;;; Table of expansions already seen is examined in buffer
;;; `dabbrev--last-table' so that only distinct possibilities are found
;;; by dabbrev-re-expand.

;;; Value is the expansion, or nil if not found.

(defun dabbrev--search (abbrev reverse ignore-case)
  (save-match-data
    (let ((pattern1 (concat (regexp-quote abbrev)
			    "\\(" dabbrev--abbrev-char-regexp "\\)"))
	  (pattern2 (concat (regexp-quote abbrev)
			   "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
	  (found-string nil))
      ;; Limited search.
      (save-restriction
	(and dabbrev-limit
	     (narrow-to-region dabbrev--last-expansion-location
			       (+ (point)
				  (if reverse (- dabbrev-limit) dabbrev-limit))))
	;;--------------------------------
	;; Look for a distinct expansion, using dabbrev--last-table.
	;;--------------------------------
	(while (and (not found-string)
		    (if reverse
			(re-search-backward pattern1 nil t)
		      (re-search-forward pattern1 nil t)))
	  (cond
	   ((progn
	      (goto-char (match-beginning 0))
	      (dabbrev--goto-start-of-abbrev)
	      (/= (point) (match-beginning 0)))
	    ;; Prefix of found abbreviation not OK
	    nil)
	   (t
	    (goto-char (match-beginning 0))
	    (re-search-forward pattern2)
	    (setq found-string
		  (buffer-substring (match-beginning 1) (match-end 1)))
	    (and ignore-case (setq found-string (downcase found-string)))
	    ;; Throw away if found in table
	    (when (some
		  (function
		   (lambda (table-string) (string= found-string table-string)))
		  dabbrev--last-table)
	      (setq found-string nil))))
	  (if reverse
	      (goto-char (match-beginning 0))
	    (goto-char (match-end 0))))
	(cond
	 (found-string
	  ;;--------------------------------
	  ;; Put in `dabbrev--last-table' and decide if we should return
	  ;; result or (downcase result)
	  ;;--------------------------------
	  (push found-string dabbrev--last-table)
	  (let ((result (buffer-substring (match-beginning 0) (match-end 0))))
	    (if (and ignore-case (eval dabbrev-case-replace))
		(downcase result)
	      result))))))))

(provide 'new-dabbrev)
(provide 'dabbrev)
;; new-dabbrev.el ends here


