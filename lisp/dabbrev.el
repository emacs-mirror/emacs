;;; dabbrev.el --- dynamic abbreviation package

;; Copyright (C) 1985, 86, 92, 94, 96, 1997, 2000
;;   Free Software Foundation, Inc.

;; Author: Don Morrison
;; Maintainer: Lars Lindberg <Lars.Lindberg@sypro.cap.se>
;; Created: 16 Mars 1992
;; Lindberg's last update version: 5.7
;; Keywords: abbrev expand completion convenience

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The purpose with this package is to let you write just a few
;; characters of words you've written earlier to be able to expand
;; them.
;;
;; To expand a word, just put the point right after the word and press
;; M-/ (dabbrev-expand) or M-C-/ (dabbrev-completion).
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
;; Then you don't interfere with other modes.
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


;; Known bugs and limitations.
;; - Possible to do several levels of `dabbrev-completion' in the
;;   minibuffer.
;; - dabbrev-completion doesn't handle resetting the globals variables
;;   right.  It resets them after finding the abbrev.

;; Future enhancements
;;  - Check the tags-files? Like tags-complete?
;;  - Add the possibility of searching both forward and backward to
;;    the nearest expansion.
;;  - Check the kill-ring when everything else fails.  (Maybe something
;;  for hippie-expand?).  [Bng] <boris@cs.rochester.edu>

;;; These people gave suggestions:
;;  [hymie]	Hyman Rosen <marks!hymie@jyacc.jyacc.com>
;;  [burgett]	Steve Burgett <burgett@bizet.eecs.berkeley.edu>
;;  [jules]	Julian Gosnell <jules@x.co.uk>
;;  [kifer]	Michael Kifer <kifer@sbcs.sunysb.edu>
;;  [ake]	Ake Stenhoff <extaksf@aom.ericsson.se>
;;  [alon]	Alon Albert <al%imercury@uunet.uu.net>
;;  [tromey]	Tom Tromey <tromey@busco.lanl.gov>
;;  [Rolf]	Rolf Schreiber <rolf@mathematik.uni-stuttgart.de>
;;  [Petri]	Petri Raitio <per@tekla.fi>
;;  [ejb]	Jay Berkenbilt <ejb@ql.org>
;;  [hawley]	Bob Hawley <rth1@quartet.mt.att.com>
;;  ... and to all the people who have participated in the beta tests.

;;; Code:

;;----------------------------------------------------------------
;; Customization variables
;;----------------------------------------------------------------

(defgroup dabbrev nil
  "Dynamic Abbreviations"
  :tag "Dynamic Abbreviations"
  :group 'abbrev
  :group 'convenience)

(defcustom dabbrev-backward-only nil
  "*If non-nil, `dabbrev-expand' only looks backwards."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-limit nil
  "*Limits region searched by `dabbrev-expand' to this many chars away."
  :type '(choice (const :tag "off" nil)
		 integer)
  :group 'dabbrev)

(defcustom dabbrev-abbrev-skip-leading-regexp nil
  "*Regexp for skipping leading characters of an abbreviation.

Example: Set this to \"\\\\$\" for programming languages
in which variable names may appear with or without a leading `$'.
\(For example, in Makefiles.\)

Set this to nil if no characters should be skipped."
  :type '(choice regexp
		 (const :tag "off" nil))
  :group 'dabbrev)

(defcustom dabbrev-case-fold-search 'case-fold-search
  "*Control whether dabbrev searches should ignore case.
A value of nil means case is significant.
A value of `case-fold-search' means case is significant
 if `case-fold-search' is nil.
Any other non-nil version means case is not significant."
  :type '(choice (const :tag "off" nil)
		 (const :tag "like search" case-fold-search)
		 (other :tag "on" t))
  :group 'dabbrev)

(defcustom dabbrev-upcase-means-case-search nil
  "*The significance of an uppercase character in an abbreviation.
nil means case fold search, non-nil means case sensitive search.

This variable has an effect only when the value of
`dabbrev-case-fold-search' says to ignore case."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-case-replace 'case-replace
  "*Controls whether dabbrev preserves case when expanding the abbreviation.
A value of nil means preserve case.
A value of `case-replace' means preserve case if `case-replace' is nil.
Any other non-nil version means do not preserve case.

This variable has an effect only when the value of
`dabbrev-case-fold-search' specifies to ignore case."
  :type '(choice (const :tag "off" nil)
		 (const :tag "like M-x query-replace" case-replace)
		 (other :tag "on" t))
  :group 'dabbrev)

(defcustom dabbrev-abbrev-char-regexp nil
  "*Regexp to recognize a character in an abbreviation or expansion.
This regexp will be surrounded with \\\\( ... \\\\) when actually used.

Set this variable to \"\\\\sw\" if you want ordinary words or
\"\\\\sw\\\\|\\\\s_\" if you want symbols (including characters whose
syntax is \"symbol\" as well as those whose syntax is \"word\".

The value nil has a special meaning: the abbreviation is from point to
previous word-start, but the search is for symbols.

For instance, if you are programming in Lisp, `yes-or-no-p' is a symbol,
while `yes', `or', `no' and `p' are considered words.  If this
variable is nil, then expanding `yes-or-no-' looks for a symbol
starting with or containing `no-'.  If you set this variable to
\"\\\\sw\\\\|\\\\s_\", that expansion looks for a symbol starting with
`yes-or-no-'.  Finally, if you set this variable to \"\\\\sw\", then
expanding `yes-or-no-' signals an error because `-' is not part of a word;
but expanding `yes-or-no' looks for a word starting with `no'.

The recommended value is \"\\\\sw\\\\|\\\\s_\"."
  :type '(choice (const nil)
		 regexp)
  :group 'dabbrev)

(defcustom dabbrev-check-all-buffers t
  "*Non-nil means dabbrev package should search *all* buffers.

Dabbrev always searches the current buffer first.  Then, if
`dabbrev-check-other-buffers' says so, it searches the buffers
designated by `dabbrev-select-buffers-function'.

Then, if `dabbrev-check-all-buffers' is non-nil, dabbrev searches
all the other buffers, except those named in `dabbrev-ignored-buffer-names',
or matched by `dabbrev-ignored-regexps'."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-ignored-buffer-names '("*Messages*" "*Buffer List*")
  "*List of buffer names that dabbrev should not check.
See also `dabbrev-ignored-regexps'."
  :type '(repeat (string :tag "Buffer name"))
  :group 'dabbrev
  :version "20.3")

(defcustom dabbrev-ignored-regexps nil
  "*List of regexps matching names of buffers that dabbrev should not check.
See also `dabbrev-ignored-buffer-names'."
  :type '(repeat regexp)
  :group 'dabbrev
  :version "21.1")

(defcustom dabbrev-check-other-buffers t
  "*Should \\[dabbrev-expand] look in other buffers?\

nil: Don't look in other buffers.
t: Also look for expansions in the buffers pointed out by
   `dabbrev-select-buffers-function'.
Anything else: When we can't find any more expansions in
the current buffer, then ask the user whether to look in other
buffers too.

The default value is t."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (other :tag "ask" other))
  :group 'dabbrev)

;; I guess setting this to a function that selects all C- or C++-
;; mode buffers would be a good choice for a debugging buffer,
;; when debugging C- or C++-code.
(defvar dabbrev-select-buffers-function 'dabbrev--select-buffers
  "A function that selects buffers that should be searched by dabbrev.
The function should take no arguments and return a list of buffers to
search for expansions.  Have a look at `dabbrev--select-buffers' for
an example.

A mode setting this variable should make it buffer local.")

(defcustom dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p
  "*A function to decide whether dabbrev should search OTHER-BUFFER.
The function should take one argument, OTHER-BUFFER, and return
non-nil if that buffer should be searched.  Have a look at
`dabbrev--same-major-mode-p' for an example.

The value of `dabbrev-friend-buffer-function' has an effect only if
the value of `dabbrev-select-buffers-function' uses it.  The function
`dabbrev--select-buffers' is one function you can use here.

A mode setting this variable should make it buffer local."
  :type 'function
  :group 'dabbrev)

(defcustom dabbrev-search-these-buffers-only nil
  "If non-nil, a list of buffers which dabbrev should search.
If this variable is non-nil, dabbrev will only look in these buffers.
It will not even look in the current buffer if it is not a member of
this list.")

;;----------------------------------------------------------------
;; Internal variables
;;----------------------------------------------------------------

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

;; Non-nil means we should upcase
;; when copying successive words.
(defvar dabbrev--last-case-pattern nil)

;; Same as dabbrev-check-other-buffers, but is set for every expand.
(defvar dabbrev--check-other-buffers dabbrev-check-other-buffers)

;; The regexp for recognizing a character in an abbreviation.
(defvar dabbrev--abbrev-char-regexp nil)

;;----------------------------------------------------------------
;; Macros
;;----------------------------------------------------------------

;;; Get the buffer that mini-buffer was activated from
(defsubst dabbrev--minibuffer-origin ()
  (car (cdr (buffer-list))))

;; Make a list of some of the elements of LIST.
;; Check each element of LIST, storing it temporarily in the
;; variable ELEMENT, and include it in the result
;; if CONDITION evaluates non-nil.
(defmacro dabbrev-filter-elements (element list condition)
  `(let (dabbrev-result dabbrev-tail ,element)
    (setq dabbrev-tail ,list)
    (while dabbrev-tail
      (setq ,element (car dabbrev-tail))
      (if ,condition
          (setq dabbrev-result (cons ,element dabbrev-result)))
      (setq dabbrev-tail (cdr dabbrev-tail)))
    (nreverse dabbrev-result)))

;;----------------------------------------------------------------
;; Exported functions
;;----------------------------------------------------------------

;;;###autoload
(define-key esc-map "/" 'dabbrev-expand)
;;;??? Do we want this?
;;;###autoload
(define-key esc-map [?\C-/] 'dabbrev-completion)

;;;###autoload
(defun dabbrev-completion (&optional arg)
  "Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from C-u C-u),
then it searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."

  (interactive "*P")
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
	 (dabbrev-check-all-buffers
	  (and arg (= (prefix-numeric-value arg) 16)))
	 (abbrev (dabbrev--abbrev-at-point))
	 (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
				 case-fold-search
			       dabbrev-case-fold-search)
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
	  ;; We can reuse the existing completion list.
	  nil
	;;--------------------------------
	;; New abbreviation to expand.
	;;--------------------------------
	(setq dabbrev--last-abbreviation abbrev)
	;; Find all expansion
	(let ((completion-list
	       (dabbrev--find-all-expansions abbrev ignore-case-p))
	      (completion-ignore-case ignore-case-p))
	  ;; Make an obarray with all expansions
	  (setq my-obarray (make-vector (length completion-list) 0))
	  (or (> (length my-obarray) 0)
	      (error "No dynamic expansion for \"%s\" found%s"
		     abbrev
		     (if dabbrev--check-other-buffers "" " in this-buffer")))
	  (cond
	   ((or (not ignore-case-p)
		(not dabbrev-case-replace))
	    (mapcar (function (lambda (string)
				(intern string my-obarray)))
		    completion-list))
	   ((string= abbrev (upcase abbrev))
	    (mapcar (function (lambda (string)
				(intern (upcase string) my-obarray)))
		    completion-list))
	   ((string= (substring abbrev 0 1)
		     (upcase (substring abbrev 0 1)))
	    (mapcar (function (lambda (string)
				(intern (capitalize string) my-obarray)))
		    completion-list))
	   (t
	    (mapcar (function (lambda (string)
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
	  (message "Repeat `%s' to see all completions"
		   (key-description (this-command-keys)))
	(message "The only possible completion"))
      (dabbrev--substitute-expansion nil abbrev init))
     (t
      ;; * String is a common substring completion already.  Make list.
      (message "Making completion list...")
      (with-output-to-temp-buffer " *Completions*"
	(display-completion-list (all-completions init my-obarray)))
      (message "Making completion list...done")))
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

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]."
  (interactive "*P")
  (let (abbrev record-case-pattern
	       expansion old direction (orig-point (point)))
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    (save-excursion
      (if (and (null arg)
	       (markerp dabbrev--last-abbrev-location)
	       (marker-position dabbrev--last-abbrev-location)
	       (or (eq last-command this-command)
		   (and (window-minibuffer-p (selected-window))
			(= dabbrev--last-abbrev-location
			   (point)))))
	  ;; Find a different expansion for the same abbrev as last time.
	  (progn
	    (setq abbrev dabbrev--last-abbreviation)
	    (setq old dabbrev--last-expansion)
	    (setq direction dabbrev--last-direction))
	;; If the user inserts a space after expanding
	;; and then asks to expand again, always fetch the next word.
	(if (and (eq (preceding-char) ?\ )
		 (markerp dabbrev--last-abbrev-location)
		 (marker-position dabbrev--last-abbrev-location)
		 (= (point) (1+ dabbrev--last-abbrev-location)))
	    (progn
	      ;; The "abbrev" to expand is just the space.
	      (setq abbrev " ")
	      (save-excursion
		(if dabbrev--last-buffer
		    (set-buffer dabbrev--last-buffer))
		;; Find the end of the last "expansion" word.
		(if (or (eq dabbrev--last-direction 1)
			(and (eq dabbrev--last-direction 0)
			     (< dabbrev--last-expansion-location (point))))
		    (setq dabbrev--last-expansion-location
			  (+ dabbrev--last-expansion-location
			     (length dabbrev--last-expansion))))
		(goto-char dabbrev--last-expansion-location)
		;; Take the following word, with intermediate separators,
		;; as our expansion this time.
		(re-search-forward
		 (concat "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
		(setq expansion (buffer-substring-no-properties
				 dabbrev--last-expansion-location (point)))
		(if dabbrev--last-case-pattern
		    (setq expansion (upcase expansion)))

		;; Record the end of this expansion, in case we repeat this.
		(setq dabbrev--last-expansion-location (point)))
	      ;; Indicate that dabbrev--last-expansion-location is
	      ;; at the end of the expansion.
	      (setq dabbrev--last-direction -1))

	  ;; We have a different abbrev to expand.
	  (dabbrev--reset-global-variables)
	  (setq direction (if (null arg)
			      (if dabbrev-backward-only 1 0)
			    (prefix-numeric-value arg)))
	  (setq abbrev (dabbrev--abbrev-at-point))
	  (setq record-case-pattern t)
	  (setq old nil)))

      ;;--------------------------------
      ;; Find the expansion
      ;;--------------------------------
      (or expansion
	  (setq expansion
		(dabbrev--find-expansion abbrev direction
					 (and (if (eq dabbrev-case-fold-search 'case-fold-search)
						  case-fold-search
						dabbrev-case-fold-search)
					      (or (not dabbrev-upcase-means-case-search)
						  (string= abbrev (downcase abbrev))))))))
    (cond
     ((not expansion)
      (dabbrev--reset-global-variables)
      (if old
	  (save-excursion
	    (setq buffer-undo-list (cons orig-point buffer-undo-list))
	    ;; Put back the original abbrev with its original case pattern.
	    (search-backward old)
	    (insert abbrev)
	    (delete-region (point) (+ (point) (length old)))))
      (error "No%s dynamic expansion for `%s' found"
	     (if old " further" "") abbrev))
     (t
      (if (not (eq dabbrev--last-buffer dabbrev--last-buffer-found))
	  (progn
	    (message "Expansion found in '%s'"
		     (buffer-name dabbrev--last-buffer))
	    (setq dabbrev--last-buffer-found dabbrev--last-buffer))
	(message nil))
      (if (and (or (eq (current-buffer) dabbrev--last-buffer)
		   (null dabbrev--last-buffer))
	       (numberp dabbrev--last-expansion-location)
	       (and (> dabbrev--last-expansion-location (point))))
	  (setq dabbrev--last-expansion-location
		(copy-marker dabbrev--last-expansion-location)))
      ;; Success: stick it in and return.
      (setq buffer-undo-list (cons orig-point buffer-undo-list))
      (dabbrev--substitute-expansion old abbrev expansion)

      ;; If we are not copying successive words now,
      ;; set dabbrev--last-case-pattern.
      (and record-case-pattern
	   (setq dabbrev--last-case-pattern
		 (and (if (eq dabbrev-case-fold-search 'case-fold-search)
			  case-fold-search
			dabbrev-case-fold-search)
		      (not dabbrev-upcase-means-case-search)
		      (equal abbrev (upcase abbrev)))))

      ;; Save state for re-expand.
      (setq dabbrev--last-expansion expansion)
      (setq dabbrev--last-abbreviation abbrev)
      (setq dabbrev--last-abbrev-location (point-marker))))))

;;----------------------------------------------------------------
;; Local functions
;;----------------------------------------------------------------

;;; Checks if OTHER-BUFFER has the same major mode as current buffer.
(defun dabbrev--same-major-mode-p (other-buffer)
  (eq major-mode
      (save-excursion
	(set-buffer other-buffer)
	major-mode)))

;;; Back over all abbrev type characters and then moves forward over
;;; all skip characters.
(defun dabbrev--goto-start-of-abbrev ()
  ;; Move backwards over abbrev chars
  (save-match-data
    (if (not (bobp))
	(progn
	  (forward-char -1)
	  (while (and (looking-at dabbrev--abbrev-char-regexp)
		      (not (bobp)))
	    (forward-char -1))
	  (or (looking-at dabbrev--abbrev-char-regexp)
	      (forward-char 1))))
    (and dabbrev-abbrev-skip-leading-regexp
	 (while (looking-at dabbrev-abbrev-skip-leading-regexp)
	   (forward-char 1)))))

;;; Extract the symbol at point to serve as abbreviation.
(defun dabbrev--abbrev-at-point ()
  ;; Check for error
  (if (bobp)
      (error "No possible abbreviation preceding point"))
  ;; Return abbrev at point
  (save-excursion
    ;; Record the end of the abbreviation.
    (setq dabbrev--last-abbrev-location (point))
    ;; If we aren't right after an abbreviation,
    ;; move point back to just after one.
    ;; This is so the user can get successive words
    ;; by typing the punctuation followed by M-/.
    (save-match-data
      (if (save-excursion
	    (forward-char -1)
	    (not (looking-at (concat "\\("
				     (or dabbrev-abbrev-char-regexp
					 "\\sw\\|\\s_")
				     "\\)+"))))
	  (if (re-search-backward (or dabbrev-abbrev-char-regexp
				      "\\sw\\|\\s_")
				  nil t)
	      (forward-char 1)
	    (error "No possible abbreviation preceding point"))))
    ;; Now find the beginning of that one.
    (dabbrev--goto-start-of-abbrev)
    (buffer-substring-no-properties
     dabbrev--last-abbrev-location (point))))

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
	dabbrev--check-other-buffers dabbrev-check-other-buffers))

;;; Find all buffers that are considered "friends" according to the
;;; function pointed out by dabbrev-friend-buffer-function.
(defun dabbrev--select-buffers ()
  (save-excursion
    (and (window-minibuffer-p (selected-window))
	 (set-buffer (dabbrev--minibuffer-origin)))
    (let ((orig-buffer (current-buffer)))
      (dabbrev-filter-elements
       buffer (buffer-list)
       (and (not (eq orig-buffer buffer))
	    (boundp 'dabbrev-friend-buffer-function)
	    (funcall dabbrev-friend-buffer-function buffer))))))

;;; Search for ABBREV, N times, normally looking forward,
;;; but looking in reverse instead if REVERSE is non-nil.
(defun dabbrev--try-find (abbrev reverse n ignore-case)
  (save-excursion
    (save-restriction
      (widen)
      (let ((expansion nil))
	(and dabbrev--last-expansion-location
	     (goto-char dabbrev--last-expansion-location))
	(let ((case-fold-search ignore-case)
	      (count n))
	  (while (and (> count 0)
		      (setq expansion (dabbrev--search abbrev
						       reverse
						       ignore-case)))
	    (setq count (1- count))))
	(and expansion
	     (setq dabbrev--last-expansion-location (point)))
	expansion))))

;;; Find all expansions of ABBREV
(defun dabbrev--find-all-expansions (abbrev ignore-case)
  (let ((all-expansions nil)
	expansion)
    (save-excursion
      (goto-char (point-min))
      (while (setq expansion (dabbrev--find-expansion abbrev -1 ignore-case))
	(setq all-expansions (cons expansion all-expansions))))
    all-expansions))

(defun dabbrev--scanning-message ()
  (message "Scanning `%s'" (buffer-name (current-buffer))))

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
			     (y-or-n-p "Scan other buffers also? ")))))
	    (let* (friend-buffer-list non-friend-buffer-list)
	      (setq dabbrev--friend-buffer-list
		    (funcall dabbrev-select-buffers-function))
	      (if dabbrev-check-all-buffers
		  (setq non-friend-buffer-list
			(nreverse
			 (dabbrev-filter-elements
			  buffer (buffer-list)
			  (let ((bn (buffer-name buffer)))
			    (and (not (member bn dabbrev-ignored-buffer-names))
				 (not (memq buffer dabbrev--friend-buffer-list))
				 (not
				  (let ((tail dabbrev-ignored-regexps)
					(match nil))
				    (while (and tail (not match))
				      (setq match (string-match (car tail) bn)
					    tail (cdr tail)))
				    match))))))
			dabbrev--friend-buffer-list
			(append dabbrev--friend-buffer-list
				non-friend-buffer-list)))))
	;; Move buffers that are visible on the screen
	;; to the front of the list.  Remove the current buffer.
	(when dabbrev--friend-buffer-list
	  (let ((w (next-window (selected-window))))
	    (while (not (eq w (selected-window)))
	      (setq dabbrev--friend-buffer-list
		    (cons (window-buffer w)
			  (delq (window-buffer w) dabbrev--friend-buffer-list)))
	      (setq w (next-window w))))
	  (setq dabbrev--friend-buffer-list
		(delq (current-buffer) dabbrev--friend-buffer-list)))
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
  (let ((use-case-replace (and (if (eq dabbrev-case-fold-search 'case-fold-search)
				   case-fold-search
				 dabbrev-case-fold-search)
			       (or (not dabbrev-upcase-means-case-search)
				   (string= abbrev (downcase abbrev)))
			       (if (eq dabbrev-case-replace 'case-replace)
				   case-replace
				 dabbrev-case-replace))))
    (and nil use-case-replace
	 (setq old (concat abbrev (or old "")))
	 (setq expansion (concat abbrev expansion)))
    ;; If the expansion has mixed case
    ;; and it is not simply a capitalized word,
    ;; or if the abbrev has mixed case,
    ;; and if the given abbrev's case pattern
    ;; matches the start of the expansion,
    ;; copy the expansion's case
    ;; instead of downcasing all the rest.
    (let ((expansion-rest (substring expansion 1)))
      (if (and (not (and (or (string= expansion-rest (downcase expansion-rest))
			     (string= expansion-rest (upcase expansion-rest)))
			 (or (string= abbrev (downcase abbrev))
			     (string= abbrev (upcase abbrev)))))
	       (string= abbrev
			(substring expansion 0 (length abbrev))))
	  (setq use-case-replace nil)))
    (if (equal abbrev " ")
	(setq use-case-replace nil))
    (if use-case-replace
	(setq expansion (downcase expansion)))
    (if old
	(save-excursion
	  (search-backward old))
      ;;(set-match-data (list (point-marker) (point-marker)))
      (search-backward abbrev))
    ;; Make case of replacement conform to case of abbreviation
    ;; provided (1) that kind of thing is enabled in this buffer
    ;; and (2) the replacement itself is all lower case.
    (dabbrev--safe-replace-match expansion
				 (not use-case-replace)
				 t)))


;;;----------------------------------------------------------------
;;; Search function used by dabbrevs library.

;;; ABBREV is string to find as prefix of word.  Second arg, REVERSE,
;;; is t for reverse search, nil for forward.  Variable dabbrev-limit
;;; controls the maximum search region size.  Third argument IGNORE-CASE
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
	  (goto-char (match-beginning 0))
	  ;; In case we matched in the middle of a word,
	  ;; back up to start of word and verify we still match.
	  (dabbrev--goto-start-of-abbrev)

	  (if (not (looking-at pattern1))
	      nil
	    ;; We have a truly valid match.  Find the end.
	    (re-search-forward pattern2)
	    (setq found-string (buffer-substring-no-properties
				(match-beginning 1) (match-end 1)))
	    (and ignore-case (setq found-string (downcase found-string)))
	    ;; Ignore this match if it's already in the table.
	    (if (dabbrev-filter-elements
		 table-string dabbrev--last-table
		 (string= found-string table-string))
		(setq found-string nil)))
	  ;; Prepare to continue searching.
	  (if reverse
	      (goto-char (match-beginning 0))
	    (goto-char (match-end 0))))
	;; If we found something, use it.
	(if found-string
	    ;; Put it into `dabbrev--last-table'
	    ;; and return it (either downcased, or as is).
	    (let ((result (buffer-substring-no-properties
			   (match-beginning 0) (match-end 0))))
	      (setq dabbrev--last-table
		    (cons found-string dabbrev--last-table))
	      (if (and ignore-case (eval dabbrev-case-replace))
		  result
		result)))))))

(provide 'dabbrev)

;;; dabbrev.el ends here
