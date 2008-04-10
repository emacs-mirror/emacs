;;; crm.el --- read multiple strings with completion

;; Copyright (C) 1985, 1986, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Sen Nagata <sen@eccosys.com>
;; Keywords: completion, minibuffer, multiple elements

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code defines a function, `completing-read-multiple', which
;; provides the ability to read multiple strings in the minibuffer,
;; with completion.

;; By using this functionality, a user may specify multiple strings at
;; a single prompt, optionally using completion.

;; Multiple strings are specified by separating each of the strings
;; with a prespecified separator character.  For example, if the
;; separator character is a comma, the strings 'alice', 'bob', and
;; 'eve' would be specified as 'alice,bob,eve'.

;; The default value for the separator character is the value of
;; `crm-default-separator' (comma).  The separator character may be
;; changed by modifying the value of `crm-separator'.

;; Contiguous strings of non-separator-characters are referred to as
;; 'elements'.  In the aforementioned example, the elements are:
;; 'alice', 'bob', and 'eve'.

;; Completion is available on a per-element basis.  For example, if
;; the contents of the minibuffer are 'alice,bob,eve' and point is
;; between 'l' and 'i', pressing TAB operates on the element 'alice'.

;; For the moment, I have decided to not bind any special behavior to
;; the separator key.  In the future, the separator key might be used
;; to provide completion in certain circumstances.  One of the reasons
;; why this functionality is not yet provided is that it is unclear to
;; the author what the precise circumstances are, under which
;; separator-invoked completion should be provided.

;; Design note: `completing-read-multiple' is modeled after
;; `completing-read'.  They should be similar -- it was intentional.

;; Some of this code started out as translation from C code in
;; src/minibuf.c to Emacs Lisp code.

;; Thanks to Richard Stallman for all of his help (many of the good
;; ideas in here are from him), Gerd Moellmann for his attention,
;; Stefan Monnier for responding with a code sample and comments very
;; early on, and Kai Grossjohann & Soren Dayton for valuable feedback.

;;; Questions and Thoughts:

;; -the author has gone through a number of test-and-fix cycles w/
;;  this code, so it should be usable.  please let me know if you find
;;  any problems.

;; -should `completing-read-multiple' allow a trailing separator in
;; a return value when REQUIRE-MATCH is t?  if not, should beep when a user
;; tries to exit the minibuffer via RET?

;; -TODO: possibly make return values from `crm-do-completion' into constants

;; -TODO: find out whether there is an appropriate way to distinguish between
;;        functions intended for internal use and those that aren't.

;; -tip: use M-f and M-b for ease of navigation among elements.

;;; History:
;;
;; 2000-04-10:
;;
;;   first revamped version

;;; Code:
(defconst crm-default-separator ","
  "Default separator for `completing-read-multiple'.")

(defvar crm-separator crm-default-separator
  "Separator used for separating strings in `completing-read-multiple'.
It should be a single character string that doesn't appear in the list of
completion candidates.  Modify this value to make `completing-read-multiple'
use a separator other than `crm-default-separator'.")

;; actual filling in of these maps occurs below via `crm-init-keymaps'
(defvar crm-local-completion-map nil
  "Local keymap for minibuffer multiple input with completion.
Analog of `minibuffer-local-completion-map'.")

(defvar crm-local-must-match-map nil
  "Local keymap for minibuffer multiple input with exact match completion.
Analog of `minibuffer-local-must-match-map' for crm.")

(defvar crm-completion-table nil
  "An alist whose elements' cars are strings, or an obarray.
This is a table used for completion by `completing-read-multiple' and its
supporting functions.")

;; this is supposed to be analogous to last_exact_completion in src/minibuf.c
(defvar crm-last-exact-completion nil
  "Completion string if last attempt reported \"Complete, but not unique\".")

(defvar crm-left-of-element nil
  "String to the left of the current element.")

(defvar crm-current-element nil
  "The current element.")

(defvar crm-right-of-element nil
  "String to the right of the current element.")

(defvar crm-beginning-of-element nil
  "Buffer position representing the beginning of the current element.")

(defvar crm-end-of-element nil
  "Buffer position representing the end of the current element.")

;; emulates temp_echo_area_glyphs from src/minibuf.c
(defun crm-temp-echo-area-glyphs (message-string)
  "Temporarily display MESSAGE-STRING in echo area.
After user-input or 2 seconds, erase the displayed string."
  (save-excursion
    (goto-char (point-max))
    (insert message-string)
    (sit-for 2)
    (backward-char (length message-string))
    (delete-char (length message-string))))

;; this function evolved from a posting by Stefan Monnier
(defun crm-collection-fn (string predicate flag)
  "Function used by `completing-read-multiple' to compute completion values.
The value of STRING is the string to be completed.

The value of PREDICATE is a function to filter possible matches, or
nil if none.

The value of FLAG is used to specify the type of completion operation.
A value of nil specifies `try-completion'.  A value of t specifies
`all-completions'.  A value of lambda specifes a test for an exact match.

For more information on STRING, PREDICATE, and FLAG, see the Elisp
Reference sections on 'Programmed Completion' and 'Basic Completion
Functions'."
  (let ((lead ""))
    (when (string-match (concat ".*" crm-separator) string)
      (setq lead (substring string 0 (match-end 0)))
      (setq string (substring string (match-end 0))))
    (if (eq flag 'lambda)
	;; return t for exact match, nil otherwise
	(let ((result (try-completion string crm-completion-table predicate)))
	  (if (stringp result)
	      nil
	    (if result
		t
	      nil))))
      (if flag
	  ;; called via (all-completions string 'crm-completion-fn predicate)?
	  (all-completions string crm-completion-table predicate)
	;; called via (try-completion string 'crm-completion-fn predicate)?
	(let ((result (try-completion string crm-completion-table predicate)))
	  (if (stringp result)
	      (concat lead result)
	    result)))))

(defun crm-find-current-element ()
  "Parse the minibuffer to find the current element.
If no element can be found, return nil.

If an element is found, bind:

  -the variable `crm-current-element' to the current element,

  -the variables `crm-left-of-element' and `crm-right-of-element' to
   the strings to the left and right of the current element,
   respectively, and

  -the variables `crm-beginning-of-element' and `crm-end-of-element' to
   the buffer positions of the beginning and end of the current element
   respectively,

and return t."
  (let* ((prompt-end (minibuffer-prompt-end))
	 (minibuffer-string (buffer-substring prompt-end (point-max)))
	 (end-index (or (string-match "," minibuffer-string (- (point) prompt-end))
			(- (point-max) prompt-end)))
	 (target-string (substring minibuffer-string 0 end-index))
	 (index (or (string-match
		     (concat crm-separator "\\([^" crm-separator "]*\\)$")
		     target-string)
		    (string-match
		     (concat "^\\([^" crm-separator "]*\\)$")
		     target-string))))
    (if (not (numberp index))
	;; no candidate found
	nil
      (progn
	;;
	(setq crm-beginning-of-element (match-beginning 1))
	(setq crm-end-of-element (+ end-index prompt-end))
	;; string to the left of the current element
	(setq crm-left-of-element
	      (substring target-string 0 (match-beginning 1)))
	;; the current element
	(setq crm-current-element (match-string 1 target-string))
	;; string to the right of the current element
	(setq crm-right-of-element (substring minibuffer-string end-index))
	t))))

(defun crm-test-completion (candidate)
  "Return t if CANDIDATE is an exact match for a valid completion."
  (let ((completions
	 ;; TODO: verify whether the arguments are appropriate
	 (all-completions
	  candidate crm-completion-table minibuffer-completion-predicate)))
    (if (member candidate completions)
	t
      nil)))

(defun crm-minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer element."
  (interactive)
  (message "Making completion list...")
  (if (not (crm-find-current-element))
      nil
    (let ((completions (all-completions crm-current-element
					minibuffer-completion-table
					minibuffer-completion-predicate)))
      (message nil)
      (if (null completions)
	  (crm-temp-echo-area-glyphs " [No completions]")
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list
	   (sort completions 'string-lessp)
	   crm-current-element)))))
  nil)

(defun crm-do-completion ()
  "This is the internal completion engine.
This function updates the text in the minibuffer
to complete the current string, and returns a number between 0 and 6.
The meanings of the return values are:

    0 - the string has no possible completion
    1 - the string is already a valid and unique match
    2 - not used
    3 - the string is already a valid match (but longer matches exist too)
    4 - the string was completed to a valid match
    5 - some completion has been done, but the result is not a match
    6 - no completion was done, and the string is not an exact match"

  (if (not (crm-find-current-element))
      nil
    (let (last completion completedp)
      (setq completion
	    (try-completion crm-current-element
			    minibuffer-completion-table
			    minibuffer-completion-predicate))
      (setq last crm-last-exact-completion)
      (setq crm-last-exact-completion nil)

      (catch 'crm-exit

	(if (null completion) ; no possible completion
	    (progn
	      (crm-temp-echo-area-glyphs " [No match]")
	      (throw 'crm-exit 0)))

	(if (eq completion t) ; was already an exact and unique completion
	    (throw 'crm-exit 1))

	(setq completedp
	      (null (string-equal completion crm-current-element)))

	(if completedp
	    (progn
	      (delete-region (minibuffer-prompt-end) (point-max))
	      (insert crm-left-of-element completion)
	      ;;		(if crm-complete-up-to-point
	      ;;		    (insert crm-separator))
	      (insert crm-right-of-element)
	      (backward-char (length crm-right-of-element))
	      ;; TODO: is this correct?
	      (setq crm-current-element completion)))

	(if (null (crm-test-completion crm-current-element))
	    (progn
	      (if completedp ; some completion happened
		  (throw 'crm-exit 5)
		(if completion-auto-help
		    (crm-minibuffer-completion-help)
		  (crm-temp-echo-area-glyphs " [Next char not unique]")))
	      (throw 'crm-exit 6))
	  (if completedp
	      (throw 'crm-exit 4)))

	(setq crm-last-exact-completion completion)
	(if (not (null last))
	    (progn
	      (if (not (null (equal crm-current-element last)))
		  (crm-minibuffer-completion-help))))

	;; returning -- was already an exact completion
	(throw 'crm-exit 3)))))

(defun crm-minibuffer-complete ()
  "Complete the current element.
If no characters can be completed, display a list of possible completions.

Return t if the current element is now a valid match; otherwise return nil."
  (interactive)
  ;; take care of scrolling if necessary -- completely cribbed from minibuf.c
  (if (not (eq last-command this-command))
      ;; ok?
      (setq minibuffer-scroll-window nil))
  (let ((window minibuffer-scroll-window))
    (if (and (not (null window))
	     ;; ok?
	     (not (null (window-buffer window))))
	(let (tem)
	  (set-buffer (window-buffer window))
	  ;; ok?
	  (setq tem (pos-visible-in-window-p (point-max) window))
	  (if (not (null tem))
	      ;; ok?
	      (set-window-start window (point-min) nil)
	    (scroll-other-window nil))
	  ;; reaching here means exiting the function w/ return value of nil
	  nil)

      (let* (
	     ;(crm-end-of-element nil)
	     (result (crm-do-completion)))
	(cond
	((eq 0 result)
	 nil)
	((eq 1 result)
	 ;; adapted from Emacs 21
	 (if (not (eq (point) crm-end-of-element))
	     (goto-char (+ 1 crm-end-of-element)))
	 (crm-temp-echo-area-glyphs " [Sole completion]")
	 t)
	((eq 3 result)
	 ;; adapted from Emacs 21
	 (if (not (eq (point) crm-end-of-element))
	     (goto-char (+ 1 crm-end-of-element)))
	 (crm-temp-echo-area-glyphs " [Complete, but not unique]")
	 t))))))

;; i love traffic lights...but only when they're green
(defun crm-find-longest-completable-substring (string)
  "Determine the longest completable (left-anchored) substring of STRING.
The description \"left-anchored\" means the positions of the characters
in the substring must be the same as those of the corresponding characters
in STRING.  Anchoring is what `^' does in a regular expression.

The table and predicate used for completion are
`minibuffer-completion-table' and `minibuffer-completion-predicate',
respectively.

A non-nil return value means that there is some substring which is
completable.  A return value of t means that STRING itself is
completable.  If a string value is returned it is the longest
completable proper substring of STRING.  If nil is returned, STRING
does not have any non-empty completable substrings.

Remember: \"left-anchored\" substring"
  (let* ((length-of-string (length string))
	 (index length-of-string)
	 (done (if (> length-of-string 0)
		   nil
		 t))
	 (first t) ; ugh, special handling for first time through...
	 goal-string
	 result)
    ;; loop through left-anchored substrings in order of descending length,
    ;; find the first substring that is completable
    (while (not done)
      (setq result (try-completion (substring string 0 index)
				   minibuffer-completion-table
				   minibuffer-completion-predicate))
      (if result
	  ;; found completable substring
	  (progn
	    (setq done t)
	    (if (and (eq result t) first)
		;; exactly matching string first time through
		(setq goal-string t)
	      ;; fully-completed proper substring
	      (setq goal-string (substring string 0 index)))))
      (setq index (1- index))
      (if first
	  (setq first nil))
      (if (<= index 0)
	  (setq done t)))
    ;; possible values include: t, nil, some string
    goal-string))

;; TODO: decide whether trailing separator is allowed.  current
;;       implementation appears to allow it
(defun crm-strings-completed-p (separated-string)
  "Verify that strings in SEPARATED-STRING are completed strings.
A return value of t means that all strings were verified.  A number is
returned if verification was unsuccessful.  This number represents the
position in SEPARATED-STRING up to where completion was successful."
  (let ((strings (split-string separated-string crm-separator))
	;; buffers start at 1, not 0
	(current-position 1)
	current-string
	result
	done)
    (while (and strings (not done))
      (setq current-string (car strings)
	    result (try-completion current-string
				   minibuffer-completion-table
				   minibuffer-completion-predicate))
      (if (eq result t)
	  (setq strings (cdr strings)
		current-position (+ current-position
				    (length current-string)
				    ;; automatically adding 1 for separator
				    ;; character
				    1))
	;; still one more case of a match
	(if (stringp result)
	    (let ((string-list
		   (all-completions result
				    minibuffer-completion-table
				    minibuffer-completion-predicate)))
	      (if (member result string-list)
		  ;; ho ho, code duplication...
		  (setq strings (cdr strings)
			current-position (+ current-position
					    (length current-string)
					    1))
		(progn
		  (setq done t)
		  ;; current-string is a partially-completed string
		  (setq current-position (+ current-position
					    (length current-string))))))
	  ;; current-string cannot be completed
	  (let ((completable-substring
		 (crm-find-longest-completable-substring current-string)))
	    (setq done t)
	    (setq current-position (+ current-position
				      (length completable-substring)))))))
    ;; return our result
    (if (null strings)
	t
      current-position)))

;; try to complete candidate, then check all separated strings.  move
;; point to problem position if checking fails for some string.  if
;; checking succeeds for all strings, exit.
(defun crm-minibuffer-complete-and-exit ()
  "If all of the minibuffer elements are valid completions then exit.
All elements in the minibuffer must match.  If there is a mismatch, move point
to the location of mismatch and do not exit.

This function is modeled after `minibuffer_complete_and_exit' in src/minibuf.c"
  (interactive)

  (if (not (crm-find-current-element))
      nil
    (let (result)

      (setq result
	    (catch 'crm-exit

	      (if (eq (minibuffer-prompt-end) (point-max))
		  (throw 'crm-exit t))

	      ;; TODO: this test is suspect?
	      (if (not (null (crm-test-completion crm-current-element)))
		  (throw 'crm-exit "check"))

	      ;; TODO: determine how to detect errors
	      (let ((result (crm-do-completion)))

		(cond
		 ((or (eq 1 result)
		      (eq 3 result))
		  (throw 'crm-exit "check"))
		 ((eq 4 result)
		  (if (not (null minibuffer-completion-confirm))
		      (progn
			(crm-temp-echo-area-glyphs " [Confirm]")
			nil)
		    (throw 'crm-exit "check")))
		 (nil)))))

      (if (null result)
	  nil
	(if (equal result "check")
	    (let ((check-strings
		   (crm-strings-completed-p
		    (buffer-substring (minibuffer-prompt-end) (point-max)))))
	      ;; check all of minibuffer
	      (if (eq check-strings t)
		  (throw 'exit nil)
		(if (numberp check-strings)
		    (progn
		      (goto-char check-strings)
		      (crm-temp-echo-area-glyphs " [An element did not match]"))
		  (message "Unexpected error"))))
	  (if (eq result t)
	      (throw 'exit nil)
	    (message "Unexpected error")))))))

(defun crm-init-keymaps ()
  "Initialize the keymaps used by `completing-read-multiple'.
Two keymaps are used depending on the value of the REQUIRE-MATCH
argument of the function `completing-read-multiple'.

If REQUIRE-MATCH is nil, the keymap `crm-local-completion-map' is used.
This keymap inherits from the keymap named `minibuffer-local-completion-map'.
The only difference is that TAB is bound to `crm-minibuffer-complete' in
the inheriting keymap.

If REQUIRE-MATCH is non-nil, the keymap `crm-local-must-match-map' is used.
This keymap inherits from the keymap named `minibuffer-local-must-match-map'.
The inheriting keymap binds RET to `crm-minibuffer-complete-and-exit'
and TAB to `crm-minibuffer-complete'."
  (unless crm-local-completion-map
    (setq crm-local-completion-map (make-sparse-keymap))
    (set-keymap-parent crm-local-completion-map
		       minibuffer-local-completion-map)
    ;; key definitions
    (define-key crm-local-completion-map
      (kbd "TAB")
      (function crm-minibuffer-complete)))

  (unless crm-local-must-match-map
    (setq crm-local-must-match-map (make-sparse-keymap))
    (set-keymap-parent crm-local-must-match-map
		       minibuffer-local-must-match-map)
    ;; key definitions
    (define-key crm-local-must-match-map
      (kbd "RET")
      (function crm-minibuffer-complete-and-exit))
    (define-key crm-local-must-match-map
      (kbd "TAB")
      (function crm-minibuffer-complete))))

(crm-init-keymaps)

;; superemulates behavior of completing_read in src/minibuf.c
;;;###autoload
(defun completing-read-multiple
  (prompt table &optional predicate require-match initial-input
	  hist def inherit-input-method)
  "Read multiple strings in the minibuffer, with completion.
By using this functionality, a user may specify multiple strings at a
single prompt, optionally using completion.

Multiple strings are specified by separating each of the strings with
a prespecified separator character.  For example, if the separator
character is a comma, the strings 'alice', 'bob', and 'eve' would be
specified as 'alice,bob,eve'.

The default value for the separator character is the value of
`crm-default-separator' (comma).  The separator character may be
changed by modifying the value of `crm-separator'.

Contiguous strings of non-separator-characters are referred to as
'elements'.  In the aforementioned example, the elements are: 'alice',
'bob', and 'eve'.

Completion is available on a per-element basis.  For example, if the
contents of the minibuffer are 'alice,bob,eve' and point is between
'l' and 'i', pressing TAB operates on the element 'alice'.

The return value of this function is a list of the read strings.

See the documentation for `completing-read' for details on the arguments:
PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD."
  (let* ((minibuffer-completion-table (function crm-collection-fn))
	 (minibuffer-completion-predicate predicate)
	 ;; see completing_read in src/minibuf.c
	 (minibuffer-completion-confirm
	  (unless (eq require-match t) require-match))
	 (crm-completion-table table)
	 crm-last-exact-completion
	 crm-current-element
	 crm-left-of-element
	 crm-right-of-element
	 crm-beginning-of-element
	 crm-end-of-element
	 (map (if require-match
		  crm-local-must-match-map
		crm-local-completion-map))
	 ;; If the user enters empty input, read-from-minibuffer returns
	 ;; the empty string, not DEF.
	 (input (read-from-minibuffer
		 prompt initial-input map
		 nil hist def inherit-input-method)))
    (and def (string-equal input "") (setq input def))
    (split-string input crm-separator)))

;; testing and debugging
;; (defun crm-init-test-environ ()
;;   "Set up some variables for testing."
;;   (interactive)
;;   (setq my-prompt "Prompt: ")
;;   (setq my-table
;; 	'(("hi") ("there") ("man") ("may") ("mouth") ("ma")
;; 	  ("a") ("ab") ("abc") ("abd") ("abf") ("zab") ("acb")
;; 	  ("da") ("dab") ("dabc") ("dabd") ("dabf") ("dzab") ("dacb")
;; 	  ("fda") ("fdab") ("fdabc") ("fdabd") ("fdabf") ("fdzab") ("fdacb")
;; 	  ("gda") ("gdab") ("gdabc") ("gdabd") ("gdabf") ("gdzab") ("gdacb")
;; 	  ))
;;   (setq my-separator ","))

;(completing-read-multiple my-prompt my-table)
;(completing-read-multiple my-prompt my-table nil t)
;(completing-read-multiple my-prompt my-table nil "match")
;(completing-read my-prompt my-table nil t)
;(completing-read my-prompt my-table nil "match")

(provide 'crm)

;; arch-tag: db1911d9-86c6-4a42-b32a-4910701b15a6
;;; crm.el ends here
