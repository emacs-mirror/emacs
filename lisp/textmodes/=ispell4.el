;;; ispell.el --- this is the GNU EMACS interface to GNU ISPELL version 4.

;;Copyright (C) 1990, 1991, 1993 Free Software Foundation, Inc.

;; Keywords: wp

;;This file is part of GNU Emacs.
;;
;;GNU Emacs is free software; you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation; either version 2, or (at your option)
;;any later version.
;;
;;GNU Emacs is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.
;;
;;You should have received a copy of the GNU General Public License
;;along with GNU Emacs; see the file COPYING.  If not, write to
;;the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides a graceful interface to ispell, the GNU
;; spelling checker.

;;; Code:

(defvar ispell-have-new-look t
  "Non-nil means use the `-r' option when running `look'.")

(defvar ispell-enable-tex-parser nil
  "Non-nil enables experimental TeX parser in Ispell for TeX-mode buffers.")

(defvar ispell-process nil "The process running Ispell")
(defvar ispell-next-message nil
  "An integer: where in `*ispell*' buffer to find next message from Ispell.")

(defvar ispell-command "ispell"
  "Command for running Ispell.")
(defvar ispell-command-options nil
  "*String (or list of strings) to pass to Ispell as command arguments.
You can specify your private dictionary via the -p <filename> option.
The -S option is always passed to Ispell as the last parameter,
and need not be mentioned here.")

(defvar ispell-look-command "look"
  "*Command for running look.")

;Each marker in this list points to the start of a word that
;ispell thought was bad last time it did the :file command.
;Notice that if the user accepts or inserts a word into his
;private dictionary, then some "good" words will be on the list.
;We would like to deal with this by looking up the words again just before
;presenting them to the user, but that is too slow on machines
;without the select system call.  Therefore, see the variable
;ispell-recently-accepted.
(defvar ispell-bad-words nil
  "A list of markers reflecting the output of the Ispell `:file' command.")

;list of words that the user has accepted, but that might still
;be on the bad-words list
(defvar ispell-recently-accepted nil)

;; Non-nil means we have started showing an alternatives window.
;; This is the window config from before then.
(defvar ispell-window-configuration nil)

;t when :dump command needed
(defvar ispell-dump-needed nil)

(defun ispell-flush-bad-words ()
  (while ispell-bad-words
    (if (markerp (car ispell-bad-words))
        (set-marker (car ispell-bad-words) nil))
    (setq ispell-bad-words (cdr ispell-bad-words)))
  (setq ispell-recently-accepted nil))

(defun kill-ispell ()
  "Kill the Ispell process.
Any changes in your private dictionary
that have not already been dumped will be lost."
  (interactive)
  (if ispell-process
      (delete-process ispell-process))
  (setq ispell-process nil)
  (ispell-flush-bad-words))

(put 'ispell-startup-error 'error-conditions
     '(ispell-startup-error error))
(put 'ispell-startup-error 'error-message
     "Problem starting ispell - see buffer *ispell*")

;; Start an ispell subprocess; check the version; and display the greeting.

(defun start-ispell ()
  (message "Starting ispell ...")
  (let ((buf (get-buffer "*ispell*")))
    (if buf
	(kill-buffer buf)))
  (condition-case err
      (setq ispell-process
	    (apply 'start-process "ispell" "*ispell*" ispell-command
		   (append (if (listp ispell-command-options)
			       ispell-command-options
			     (list ispell-command-options))
			   '("-S"))))
    (file-error (signal 'ispell-startup-error nil)))
  (process-kill-without-query ispell-process)
  (buffer-disable-undo (process-buffer ispell-process))
  (accept-process-output ispell-process)
  (let (last-char)
    (save-excursion
      (set-buffer (process-buffer ispell-process))
      (bury-buffer (current-buffer))
      (setq last-char (- (point-max) 1))
      (while (not (eq (char-after last-char) ?=))
	(cond ((not (eq (process-status ispell-process) 'run))
	       (kill-ispell)
	       (signal 'ispell-startup-error nil)))
	(accept-process-output ispell-process)
	(setq last-char (- (point-max) 1)))
      (goto-char (point-min))
      (let ((greeting (read (current-buffer))))
	(if (not (= (car greeting) 1))
	    (error "Bad ispell version: wanted 1, got %d" (car greeting)))
	(message (car (cdr greeting))))
      (delete-region (point-min) last-char))))
  
;; Make sure ispell is ready for a command.
;; Leaves buffer set to *ispell*, point at '='.

(defun ispell-sync (intr)
  (if (or (null ispell-process)
	  (not (eq (process-status ispell-process) 'run)))
      (start-ispell))
  (if intr
      (interrupt-process ispell-process))
  (let (last-char)
    (set-buffer (process-buffer ispell-process))
    (bury-buffer (current-buffer))
    (setq last-char (- (point-max) 1))
    (while (not (eq (char-after last-char) ?=))
      (accept-process-output ispell-process)
      (setq last-char (- (point-max) 1)))
    (goto-char last-char)))

;; Send a command to ispell.  Choices are:
;; 
;; WORD		Check spelling of WORD.  Result is
;; 
;;			 nil			   not found
;;			 t			   spelled ok
;;			 list of strings		   near misses
;; 
;; :file FILENAME	scan the named file, and print the file offsets of
;;		 any misspelled words
;; 
;; :insert WORD	put word in private dictionary
;; 
;; :accept WORD	don't complain about word any more this session
;; 
;; :dump		write out the current private dictionary, if necessary.
;; 
;; :reload		reread private dictionary (default: `~/ispell.words')
;; 
;; :tex
;; :troff
;; :generic	set type of parser to use when scanning whole files

(defun ispell-cmd (&rest strings)
  (save-excursion
    (ispell-sync t)
    (set-buffer (process-buffer ispell-process))
    (bury-buffer (current-buffer))
    (erase-buffer)
    (setq ispell-next-message (point-min))
    (while strings
      (process-send-string ispell-process (car strings))
      (setq strings (cdr strings)))
    (process-send-string ispell-process "\n")
    (accept-process-output ispell-process)
    (ispell-sync nil)))

(defun ispell-dump ()
  (cond (ispell-dump-needed
	 (setq ispell-dump-needed nil)
	 (ispell-cmd ":dump"))))

(defun ispell-insert (word)
  (ispell-cmd ":insert " word)
  (if ispell-bad-words
      (setq ispell-recently-accepted (cons word ispell-recently-accepted)))
  (setq ispell-dump-needed t))

(defun ispell-accept (word)
  (ispell-cmd ":accept " word)
  (if ispell-bad-words
      (setq ispell-recently-accepted (cons word ispell-recently-accepted))))

;; Return the next message sent by the Ispell subprocess.

(defun ispell-next-message ()
  (save-excursion
    (set-buffer (process-buffer ispell-process))
    (bury-buffer (current-buffer))
    (save-restriction
      (goto-char ispell-next-message)
      (narrow-to-region (point)
                        (progn (forward-sexp 1) (point)))
      (setq ispell-next-message (point))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun ispell-tex-buffer-p ()
  (memq major-mode '(plain-tex-mode latex-mode slitex-mode)))

(defvar ispell-menu-map (make-sparse-keymap "Spell"))
(defalias 'ispell-menu-map ispell-menu-map)

(define-key ispell-menu-map [ispell-complete-word-interior-frag]
  '("Complete Interior Fragment" . ispell-complete-word-interior-frag))

(define-key ispell-menu-map [ispell-complete-word]
  '("Complete Word" . ispell-complete-word))

(define-key ispell-menu-map [reload-ispell]
  '("Reload Dictionary" . reload-ispell))

(define-key ispell-menu-map [ispell-next]
  '("Continue Check" . ispell-next))

(define-key ispell-menu-map [ispell-message]
  '("Check Message" . ispell-message))

(define-key ispell-menu-map [ispell-word]
  '("Check Word" . ispell-word))

(define-key ispell-menu-map [ispell-region]
  '("Check Region" . ispell-region))

(define-key ispell-menu-map [ispell-buffer]
  '("Check Buffer" . ispell))

;;;autoload
(defun ispell (&optional buf start end)
  "Run Ispell over current buffer's visited file.
First the file is scanned for misspelled words, then Ispell
enters a loop with the following commands for every misspelled word:

DIGIT	Near miss selector.  If the misspelled word is close to
	some words in the dictionary, they are offered as near misses.
r	Replace.  Replace the word with a string you type.  Each word
	of your new string is also checked.
i	Insert.  Insert this word in your private dictionary (by default,
	`$HOME/ispell.words').
a	Accept.  Accept this word for the rest of this editing session,
 	but don't put it in your private dictionary.
l	Lookup.  Look for a word in the dictionary by fast binary
	search, or search for a regular expression in the dictionary
	using grep.
SPACE	Accept the word this time, but complain if it is seen again.
q, \\[keyboard-quit]	Leave the command loop.  You can come back later with \\[ispell-next]."
  (interactive)
  (if (null start)
      (setq start 0))
  (if (null end)
      (setq end 0))

  (if (null buf)
      (setq buf (current-buffer)))
  (setq buf (get-buffer buf))
  (if (null buf)
      (error "Can't find buffer"))
  ;; Deactivate the mark, because we'll do it anyway if we change something,
  ;; and a region highlight while in the Ispell loop is distracting.
  (deactivate-mark)
  (save-excursion
    (set-buffer buf)
    (let ((filename buffer-file-name)
	  (delete-temp nil))
      (unwind-protect
	  (progn
	    (cond ((or (null filename)
		       (find-file-name-handler buffer-file-name nil))
		   (setq filename (make-temp-name "/usr/tmp/ispell"))
		   (setq delete-temp t)
		   (write-region (point-min) (point-max) filename))
		  ((and (buffer-modified-p buf)
			(y-or-n-p (format "Save file %s? " filename)))
		   (save-buffer)))
	    (message "Ispell scanning file...")
	    (if (and ispell-enable-tex-parser
		     (ispell-tex-buffer-p))
		(ispell-cmd ":tex")
	      (ispell-cmd ":generic"))
	    (ispell-cmd (format ":file %s %d %d" filename start end)))
	(if delete-temp
	    (condition-case ()
		(delete-file filename)
	      (file-error nil)))))
    (message "Parsing ispell output ...")
    (ispell-flush-bad-words)
    (let (pos bad-words)
      (while (numberp (setq pos (ispell-next-message)))
	;;ispell may check the words on the line following the end
	;;of the region - therefore, don't record anything out of range
	(if (or (= end 0)
		(< pos end))
	    (setq bad-words (cons (set-marker (make-marker) (+ pos 1))
				  bad-words))))
      (setq bad-words (cons pos bad-words))
      (setq ispell-bad-words (nreverse bad-words))))
  (cond ((not (markerp (car ispell-bad-words)))
	 (setq ispell-bad-words nil)
	 (message "No misspellings.")
	 t)
	(t
	 (message "Ispell parsing done.")
	 (ispell-next))))

;;;autoload
(defalias 'ispell-buffer 'ispell)

(defun ispell-next ()
  "Resume command loop for most recent Ispell command.
Return value is t unless exit is due to typing `q'."
  (interactive)
  (setq ispell-window-configuration nil)
  (prog1
      (unwind-protect
	  (catch 'ispell-quit
	    ;; There used to be a save-excursion here,
	    ;; but that was annoying: it's better if point doesn't move
	    ;; when you type q.
	    (let (next)
	      (while (markerp (setq next (car ispell-bad-words)))
		(switch-to-buffer (marker-buffer next))
		(push-mark)
		(ispell-point next "at saved position.")
		(setq ispell-bad-words (cdr ispell-bad-words))
		(set-marker next nil)))
	    t)
	(ispell-dehighlight)
	(if ispell-window-configuration
	    (set-window-configuration ispell-window-configuration))
	(cond ((null ispell-bad-words)
	       (error "Ispell has not yet been run"))
	      ((markerp (car ispell-bad-words))
	       (message (substitute-command-keys
			   "Type \\[ispell-next] to continue")))
	      ((eq (car ispell-bad-words) nil)
	       (setq ispell-bad-words nil)
	       (message "No more misspellings (but checker was interrupted)"))
	      ((eq (car ispell-bad-words) t)
	       (setq ispell-bad-words nil)
	       (message "Ispell done"))
	      (t
	       (setq ispell-bad-words nil)
	       (message "Bad ispell internal list"))))
    (ispell-dump)))

;;;autoload
(defun ispell-word (&optional resume)
  "Check the spelling of the word under the cursor.
See the command `ispell' for more information.
With a prefix argument, resume handling of the previous Ispell command."
  (interactive "P")
  (if resume
      (ispell-next)
    (condition-case err
	(unwind-protect
	    (catch 'ispell-quit
	      (save-window-excursion
		(ispell-point (point) "at point."))
	      (ispell-dump))
	  (ispell-dehighlight))
      (ispell-startup-error
       (cond ((y-or-n-p "Problem starting ispell, use old-style spell instead? ")
	      (load-library "spell")
	      (define-key esc-map "$" 'spell-word)
	      (spell-word)))))))

;;;autoload (define-key esc-map "$" 'ispell-word)

;;;autoload
(defun ispell-region (start &optional end)
  "Check the spelling for all of the words in the region."
  (interactive "r")
  (ispell (current-buffer) start end))

(defun ispell-letterp (c)
  (and c
       (or (and (>= c ?A) (<= c ?Z))
	   (and (>= c ?a) (<= c ?z))
	   (>= c 128))))

(defun ispell-letter-or-quotep (c)
  (and c
       (or (and (>= c ?A) (<= c ?Z))
	   (and (>= c ?a) (<= c ?z))
	   (= c ?')
	   (>= c 128))))

(defun ispell-find-word-start ()
  ;;backward to a letter
  (if (not (ispell-letterp (char-after (point))))
      (while (and (not (bobp))
		  (not (ispell-letterp (char-after (- (point) 1)))))
	(backward-char)))
  ;;backward to beginning of word
  (while (ispell-letter-or-quotep (char-after (- (point) 1)))
    (backward-char))
  (skip-chars-forward "'"))

(defun ispell-find-word-end ()
  (while (ispell-letter-or-quotep (char-after (point)))
    (forward-char))
  (skip-chars-backward "'"))

(defun ispell-next-word ()
  (while (and (not (eobp))
	      (not (ispell-letterp (char-after (point)))))
    (forward-char)))

;if end is nil, then do one word at start
;otherwise, do all words from the beginning of the word where
;start points, to the end of the word where end points
(defun ispell-point (start message)
  (let ((wend (make-marker))
	rescan
	end)
    ;; There used to be a save-excursion here,
    ;; but that was annoying: it's better if point doesn't move
    ;; when you type q.
    (goto-char start)
    (ispell-find-word-start)		;find correct word start
    (setq start (point-marker))
    (ispell-find-word-end)		;now find correct end
    (setq end (point-marker))
    ;; Do nothing if we don't find a word.
    (if (< start end)
	(while (< start end)
	  (goto-char start)
	  (ispell-find-word-end)	;find end of current word
					;could be before 'end' if
					;user typed replacement
					;that is more than one word
	  (set-marker wend (point))
	  (setq rescan nil)
	  (setq word (buffer-substring start wend))
	  (cond ((ispell-still-bad word)
;;; This just causes confusion. -- rms.
;;;	     (goto-char start)
;;;	     (sit-for 0)
		 (message (format "Ispell checking %s" word))
		 (ispell-cmd word)
		 (let ((message (ispell-next-message)))
		   (cond ((eq message t)
			  (message "%s: ok" word))
			 ((or (null message)
			      (consp message))
			  (setq rescan
				(ispell-command-loop word start wend message)))
			 (t
			  (error "unknown ispell response %s" message))))))
	  (cond ((null rescan)
		 (goto-char wend)
		 (ispell-next-word)
		 (set-marker start (point))))))
    ;;clear the choices buffer; otherwise it's hard for the user to tell
    ;;when we get back to the command loop
    (let ((buf (get-buffer "*ispell choices*")))
      (cond (buf
	     (set-buffer buf)
	     (erase-buffer))))
    (set-marker start nil)
    (set-marker end nil)
    (set-marker wend nil)))
  
(defun ispell-still-bad (word)
  (let ((words ispell-recently-accepted)
	(ret t)
	(case-fold-search t))
    (while words
      (cond ((eq (string-match (car words) word) 0)
	     (setq ret nil)
	     (setq words nil)))
      (setq words (cdr words)))
    ret))

(defun ispell-show-choices (word message first-line)
  ;;if there is only one window on the frame, make the ispell
  ;;messages winow be small.  otherwise just use the other window
  (let* ((selwin (selected-window))
	 (resize (eq selwin (next-window)))
	 (buf (get-buffer-create "*ispell choices*"))
	 w)
    (or ispell-window-configuration
	(setq ispell-window-configuration (current-window-configuration)))
    (setq w (display-buffer buf))
    (buffer-disable-undo buf)
    (if resize
	(unwind-protect
	    (progn
	      (select-window w)
	      (enlarge-window (- 6 (window-height w))))
	  (select-window selwin)))
    (save-excursion
      (set-buffer buf)
      (bury-buffer buf)
      (set-window-point w (point-min))
      (set-window-start w (point-min))
      (erase-buffer)
      (insert first-line "\n")
      (insert
       "SPC skip; A accept; I insert; DIGIT select; R replace; \
L lookup; Q quit\n")
      (cond ((not (null message))
	     (let ((i 0))
	       (while (< i 3)
		 (let ((j 0))
		   (while (< j 3)
		     (let* ((n (+ (* j 3) i))
			    (choice (nth n message)))
		       (cond (choice
			      (let ((str (format "%d %s" n choice)))
				(insert str)
				(insert-char ?  (- 20 (length str)))))))
		     (setq j (+ j 1))))
		 (insert "\n")
		 (setq i (+ i 1)))))))))

(defun ispell-command-loop (word start end message)
  (let ((flag t)
	(rescan nil)
	first-line)
    (if (null message)
	(setq first-line (concat "No near misses for '" word "'"))
      (setq first-line (concat "Near misses for '" word "'")))
    (ispell-highlight start end)
    (while flag
      (ispell-show-choices word message first-line)
      (message "Ispell command: ")
      (undo-boundary)
      (let ((c (downcase (read-char)))
	    replacement)
	(cond ((and (>= c ?0)
		    (<= c ?9)
		    (setq replacement (nth (- c ?0) message)))
	       (ispell-replace start end replacement)
	       (setq flag nil))
	      ((= c ?q)
	       (throw 'ispell-quit nil))
	      ((= c (nth 3 (current-input-mode)))
	       (keyboard-quit))
	      ((= c ? )
	       (setq flag nil))
	      ((= c ?r)
	       (ispell-replace start end (read-string "Replacement: "))
	       (setq rescan t)
	       (setq flag nil))
	      ((= c ?i)
	       (ispell-insert word)
	       (setq flag nil))
	      ((= c ?a)
	       (ispell-accept word)
	       (setq flag nil))
	      ((= c ?l)
	       (let ((val (ispell-do-look word)))
		 (setq first-line (car val))
		 (setq message (cdr val))))
	      ((= c ??)
	       (message
		"Type 'C-h d ispell' to the emacs main loop for more help")
	       (sit-for 2))
	      (t
	       (message "Bad ispell command")
	       (sit-for 2)))))
    rescan))

(defun ispell-do-look (bad-word)
  (let (regex buf words)
    (cond ((null ispell-have-new-look)
	   (setq regex (read-string "Lookup: ")))
	  (t
	   (setq regex (read-string "Lookup (regex): " "^"))))
    (setq buf (get-buffer-create "*ispell look*"))
    (save-excursion
      (set-buffer buf)
      (delete-region (point-min) (point-max))
      (if ispell-have-new-look
	  (call-process ispell-look-command nil buf nil "-r" regex)
	(call-process ispell-look-command nil buf nil regex))
      (goto-char (point-min))
      (forward-line 10)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (while (not (= (point-min) (point-max)))
	(end-of-line)
	(setq words (cons (buffer-substring (point-min) (point)) words))
	(forward-line)
	(delete-region (point-min) (point)))
      (kill-buffer buf)
      (cons (format "Lookup '%s'" regex)
	    (reverse words)))))
    
(defun ispell-replace (start end new)
  (goto-char start)
  (insert new)
  (delete-region (point) end))

(defun reload-ispell ()
  "Tell Ispell to re-read your private dictionary."
  (interactive)
  (ispell-cmd ":reload"))

(defun batch-make-ispell ()
  (byte-compile-file "ispell.el")
  (find-file "ispell.texinfo")
  (let ((old-dir default-directory)
	(default-directory "/tmp"))
    (texinfo-format-buffer))
  (Info-validate)
  (if (get-buffer " *problems in info file*")
      (kill-emacs 1))
  (write-region (point-min) (point-max) "ispell.info"))

(defvar ispell-highlight t
  "*Non-nil means to highlight ispell words.")

(defvar ispell-overlay nil)

(defun ispell-dehighlight ()
  (and ispell-overlay
       (progn
	 (delete-overlay ispell-overlay)
	 (setq ispell-overlay nil))))

(defun ispell-highlight (start end)
  (and ispell-highlight 
       window-system
       (progn
	 (or ispell-overlay
	     (progn
	       (setq ispell-overlay (make-overlay start end))
	       (overlay-put ispell-overlay 'face
			    (if (internal-find-face 'ispell)
				'ispell 'region))))
	 (move-overlay ispell-overlay start end (current-buffer)))))

;;;; ispell-complete-word

;;; Brief Description:
;;; Complete word fragment at point using dictionary and replace with full
;;; word.  Expansion done in current buffer like lisp-complete-symbol.
;;; Completion of interior word fragments possible with prefix argument.

;;; Known Problem: 
;;; Does not use private dictionary because GNU `look' does not use it.  It
;;; would be nice if GNU `look' took standard input; this would allow gzip'ed
;;; dictionaries to be used.  GNU `look' also has a bug, see
;;; `ispell-gnu-look-still-broken-p'.

;;; Motivation: 
;;; The `l', "regular expression look up", keymap option of ispell-word
;;; (ispell-do-look) can only be run after finding a misspelled word.  So
;;; ispell-do-look can not be used to look for words starting with `cat' to
;;; find `catechetical' since `cat' is a correctly spelled word.  Furthermore,
;;; ispell-do-look does not return the entire list returned by `look'.
;;;  
;;; ispell-complete-word allows you to get a completion list from the system
;;; dictionary and expand a word fragment at the current position in a buffer.
;;; These examples assume ispell-complete-word is bound to M-TAB as it is in
;;; text-mode; the `Complete Word' and `Complete Interior Fragment' entries of
;;; the "Spell" submenu under the "Edit" menu may also be used instead of
;;; M-TAB and C-u M-TAB, respectively.
;;;
;;;   EXAMPLE 1: The word `Saskatchewan' needs to be spelled.  The user may
;;;   type `Sas' and hit M-TAB and a completion list will be built using the
;;;   shell command `look' and displayed in the *Completions* buffer:
;;;
;;;        Possible completions are:
;;;        sash                               sashay
;;;        sashayed                           sashed
;;;        sashes                             sashimi
;;;        Saskatchewan                       Saskatoon
;;;        sass                               sassafras
;;;        sassier                            sassing
;;;        sasswood                           sassy
;;;
;;;   By viewing this list the user will hopefully be motivated to insert the
;;;   letter `k' after the `sas'.  When M-TAB is hit again the word `Saskat'
;;;   will be inserted in place of `sas' (note case) since this is a unique
;;;   substring completion.  The narrowed completion list can be viewed with
;;;   another M-TAB
;;;
;;;        Possible completions are:
;;;        Saskatchewan                       Saskatoon
;;;
;;;   Inserting the letter `c' and hitting M-TAB will narrow the completion
;;;   possibilities to just `Saskatchewan' and this will be inserted in the
;;;   buffer.  At any point the user may click the mouse on a completion to
;;;   select it.
;;;
;;;   EXAMPLE 2: The user has typed `Sasaquane' and M-$ (ispell-word) gives no
;;;   "near-misses" in which case you back up to `Sas' and hit M-TAB and find
;;;   the correct word as above.  The `Sas' will be replaced by `Saskatchewan'
;;;   and the remaining word fragment `aquane' can be deleted.
;;;
;;;   EXAMPLE 3: If a version of `look' is used that supports regular
;;;   expressions, then `ispell-have-new-look' should be t (its default) and
;;;   interior word fragments may also be used for the search.  The word
;;;   `pneumonia' needs to be spelled.  The user can only remember the
;;;   interior fragment `mon' in which case `C-u M-TAB' on `mon' gives a list
;;;   of all words containing the interior word fragment `mon'.  Typing `p'
;;;   and M-TAB will narrow this list to all the words starting with `p' and
;;;   containing `mon' from which `pneumonia' can be found as above.

;;; The user-defined variables are:
;;;
;;;  ispell-look-command
;;;  ispell-look-dictionary
;;;  ispell-gnu-look-still-broken-p

;;; Algorithm (some similarity to lisp-complete-symbol):
;;;  
;;; * call-process on command ispell-look-command (default: "look") to find
;;;   words in ispell-look-dictionary matching `string' (or `regexp' if 
;;;   ispell-have-new-look is t).  Parse output and store results in 
;;;   ispell-lookup-completions-alist.
;;; 
;;; * Build completion list using try-completion and `string'
;;; 
;;; * Replace `string' in buffer with matched common substring completion.
;;; 
;;; * Display completion list only if there is no matched common substring.
;;; 
;;; * Rebuild ispell-lookup-completions-alist, on a next call, only when
;;;   beginning of word fragment has changed.
;;;  
;;; * Interior fragments searches are performed similarly with the exception
;;;   that the entire fragment at point is initially removed from the buffer,
;;;   the STRING passed to try-completion and all-completions is just "" and
;;;   not the interior fragment; this allows all completions containing the
;;;   interior fragment to be shown.  The location in the buffer is stored to
;;;   decide whether future completion narrowing of the current list should be
;;;   done or if a new list should be built.  See interior fragment example
;;;   above.
;;;
;;; * Robust searches are done using a `look' with -r (regular expression) 
;;;   switch if ispell-have-new-look is t.

;;;; User-defined variables.

(defvar ispell-look-dictionary nil
  "*If non-nil then spelling dictionary as string for `ispell-complete-word'.
Overrides default dictionary file such as \"/usr/dict/words\" or GNU look's
\"${prefix}/lib/ispell/ispell.words\"")

(defvar ispell-gnu-look-still-broken-p nil
  "*t if GNU look -r can give different results with and without trialing `.*'.
Example: `look -dfr \"^ya\" foo' returns nothing, while `look -dfr \"^ya.*\" foo'
returns `yacc', where `foo' is a dictionary file containing the three lines

   y
   y's
   yacc

Both commands should return `yacc'.  If `ispell-complete-word' erroneously
states that no completions exist for a string, then setting this variable to t
will help find those completions.")

;;;; Internal variables.

;;; Possible completions for last word fragment.
(defvar ispell-lookup-completions-alist nil)

;;; Last word fragment processed by `ispell-complete-word'.
(defvar ispell-lookup-last-word nil)

;;; Buffer local variables.

;;; Value of interior-frag in last call to `ispell-complete-word'.
(defvar ispell-lookup-last-interior-p nil)
(make-variable-buffer-local 'ispell-lookup-last-interior-p)
(put 'ispell-lookup-last-interior-p 'permanent-local t)

;;; Buffer position in last call to `ispell-complete-word'.
(defvar ispell-lookup-last-bow nil)
(make-variable-buffer-local 'ispell-lookup-last-bow)
(put 'ispell-lookup-last-bow 'permanent-local t)

;;;; Interactive functions.
;;;autoload
(defun ispell-complete-word (&optional interior-frag)
  "Complete word using letters at point to word beginning using `look'.
With optional argument INTERIOR-FRAG, word fragment at point is assumed to be
an interior word fragment in which case `ispell-have-new-look' should be t.
See also `ispell-look-dictionary' and `ispell-gnu-look-still-broken-p'."

  (interactive "P")

  ;; `look' must support regexp expressions in order to perform an interior
  ;; fragment search.
  (if (and interior-frag (not ispell-have-new-look))
      (error (concat "Sorry, `ispell-have-new-look' is nil.  "
                     "You also will need GNU Ispell's `look'.")))

  (let* ((completion-ignore-case t)

         ;; Get location of beginning of word fragment.
         (bow (save-excursion (skip-chars-backward "a-zA-Z'") (point)))

         ;; Get the string to look up.
         (string (buffer-substring bow (point)))

         ;; Get regexp for which we search and, if necessary, an interior word
         ;; fragment.
         (regexp (if interior-frag
                     (concat "^.*" string ".*")
                   ;; If possible use fast binary search: no trailing `.*'.
                   (concat "^" string
                           (if ispell-gnu-look-still-broken-p ".*"))))

         ;; We want all completions for case of interior fragments so set
         ;; prefix to an empty string.
         (prefix (if interior-frag "" string))

         ;; Are we continuing from a previous interior fragment search?
         ;; Check last value of interior-word and if the point has moved.
         (continuing-an-interior-frag-p
          (and ispell-lookup-last-interior-p
               (equal ispell-lookup-last-bow bow)))

         ;; Are we starting a unique word fragment search?  Always t for
         ;; interior word fragment search.
         (new-unique-string-p
          (or interior-frag (null ispell-lookup-last-word)
              (let ((case-fold-search t))
                ;; Can we locate last word fragment as a substring of current
                ;; word fragment?  If the last word fragment is larger than
                ;; the current string then we will have to rebuild the list
                ;; later.
                (not (string-match
                      (concat "^" ispell-lookup-last-word) string)))))

         completion)

    ;; Check for perfect completion already.  That is, maybe the user has hit
    ;; M-x ispell-complete-word one too many times?
    (if (string-equal string "")
        (if (string-equal (concat ispell-lookup-last-word " ")
                          (buffer-substring
                           (save-excursion (forward-word -1) (point)) (point)))
            (error "Perfect match already")
          (error "No word fragment at point")))

    ;; Create list of words from system dictionary starting with `string' if
    ;; new string and not continuing from a previous interior fragment search.
    (if (and (not continuing-an-interior-frag-p) new-unique-string-p)
        (setq ispell-lookup-completions-alist
              (ispell-lookup-build-list string regexp)))

    ;; Check for a completion of `string' in the list and store `string' and
    ;; other variables for the next call.
    (setq completion (try-completion prefix ispell-lookup-completions-alist)
          ispell-lookup-last-word string
          ispell-lookup-last-interior-p interior-frag
          ispell-lookup-last-bow bow)

    ;; Test the completion status.
    (cond

     ;; * Guess is a perfect match.
     ((eq completion t)
      (insert " ")
      (message "Perfect match."))

     ;; * No possibilities.
     ((null completion)
      (message "Can't find completion for \"%s\"" string)
      (beep))

     ;; * Replace string fragment with matched common substring completion.
     ((and (not (string-equal completion ""))
           ;; Fold case so a completion list is built when `string' and common
           ;; substring differ only in case.
           (let ((case-fold-search t))
             (not (string-match (concat "^" completion "$") string))))
      (search-backward string bow)
      (replace-match completion nil t) ; FIXEDCASE doesn't work? or LITERAL?
      (message "Proposed unique substring.  Repeat for completions list."))

     ;; * String is a common substring completion already.  Make list.
     (t
      (message "Making completion list...")
      (if (string-equal completion "") (delete-region bow (point)))
      (let ((list (all-completions prefix ispell-lookup-completions-alist)))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list list)))
      (message "Making completion list...done")))))

;;;autoload
(defun ispell-complete-word-interior-frag ()
  "Runs `ispell-complete-word' with a non-nil INTERIOR-FRAG.
A completion list is built for word fragment at point which is assumed to be
an interior word fragment.  `ispell-have-new-look' should be t."
  (interactive)
  (ispell-complete-word t))

;;;; Internal Function.

;;; Build list of words using ispell-look-command from dictionary
;;; ispell-look-dictionary (if this is a non-nil string).  Look for words
;;; starting with STRING if ispell-have-new-look is nil or look for REGEXP if
;;; ispell-have-new-look is t.  Returns result as an alist suitable for use by
;;; try-completion, all-completions, and completing-read.
(defun ispell-lookup-build-list (string regexp)
  (save-excursion
    (message "Building list...")
    (set-buffer (get-buffer-create " *ispell look*"))
    (erase-buffer)

    (if (stringp ispell-look-dictionary)
        (if ispell-have-new-look
            (call-process ispell-look-command nil t nil "-fr" regexp
                          ispell-look-dictionary)
          (call-process ispell-look-command nil t nil "-f" string
                        ispell-look-dictionary))
      (if ispell-have-new-look
          (call-process ispell-look-command nil t nil "-fr" regexp)
        (call-process ispell-look-command nil t nil "-f" string)))

    ;; Build list for try-completion and all-completions by storing each line
    ;; of output starting from bottom of buffer and deleting upwards.
    (let (list)
      (goto-char (point-min))
      (while (not (= (point-min) (point-max)))
        (end-of-line)
        (setq list (cons (buffer-substring (point-min) (point)) list))
        (forward-line)
        (delete-region (point-min) (point)))

      ;; Clean.
      (erase-buffer)
      (message "Building list...done")

      ;; Make the list into an alist and return.
      (mapcar 'list (nreverse list)))))

;; Return regexp-quote of STRING if STRING is non-empty.
;; Otherwise return an unmatchable regexp.
(defun ispell-non-empty-string (string)
  (if (or (not string) (string-equal string ""))
      "\\'\\`" ; An unmatchable string if string is null.
    (regexp-quote string)))

(defvar ispell-message-cite-regexp "^   \\|^\t"
  "*Regular expression to match lines cited from one message into another.")

(defvar ispell-message-text-end
  (concat "^\\(" (mapconcat (function identity)
				'(
				  ;; Matches postscript files.
				  "%!PS-Adobe-2.0"
				  ;; Matches uuencoded text
				  "begin [0-9][0-9][0-9] .*\nM.*\nM.*\nM"
				  ;; Matches shell files (esp. auto-decoding)
				  "#! /bin/sh"
				  ;; Matches difference listing
				  "diff -c .*\n\\*\\*\\* .*\n--- "
				  ;; Matches "--------------------- cut here"
				  "[-=]+\\s cut here")
				"\\|")
	      "\\)")
  "*End of text which will be checked in ispell-message.
If it is a string, limit at first occurence of that regular expression.
Otherwise, it must be a function which is called to get the limit.")

(defvar ispell-message-limit (* 100 80)
  "*Ispell-message will check no more than this number of characters.")

;;;autoload
(defun ispell-message ()
  "Check the spelling of a mail message or news post.
Don't check spelling of message headers (except subject) or included messages.

To spell-check whenever a message is sent, include this line in .emacs:
   (setq news-inews-hook (setq mail-send-hook 'ispell-message))

Or you can bind the function to C-c i in gnus or mail with:
   (setq mail-mode-hook (setq news-reply-mode-hook
    (function (lambda () (local-set-key \"\\C-ci\" 'ispell-message)))))"
  (interactive)
  (save-excursion
    (let (non-internal-message
	  (old-case-fold-search case-fold-search)
	  (case-fold-search nil))
      (goto-char (point-min))
      ;; Don't spell-check the headers.
      (if (search-forward mail-header-separator nil t)
	  ;; Move to first body line.
	  (forward-line 1)
	(while (and (looking-at "[a-zA-Z-]+:\\|\t\\| ")
		    (not (eobp)))
	  (forward-line 1))
	(setq non-internal-message t)
	)
      (let* ((cite-regexp		;Prefix of inserted text
	     (cond
	      ((featurep 'supercite)	; sc 3.0
	       (concat "\\(" (sc-cite-regexp) "\\)" "\\|"
		       (ispell-non-empty-string sc-reference-tag-string)))
	      ((featurep 'sc)		; sc 2.3
	       (concat "\\(" sc-cite-regexp "\\)" "\\|"
		       (ispell-non-empty-string sc-reference-tag-string)))
	      (non-internal-message	; Assume nn sent us this message.
	       (concat "In [a-zA-Z.]+ you write:" "\\|"
		       "In <[^,;&+=]+> [^,;&+=]+ writes:" "\\|"
		       " *> *"))
	      ((equal major-mode 'news-reply-mode) ;Gnus
	       (concat "In article <" "\\|"
		       (if mail-yank-prefix
			   (ispell-non-empty-string mail-yank-prefix)
			 ispell-message-cite-regexp)))
	      ((boundp 'vm-included-text-prefix) ; VM mail message
	       (concat "[^,;&+=]+ writes:" "\\|"
		       (ispell-non-empty-string vm-included-text-prefix)
		       ))
	      ((boundp 'mh-ins-buf-prefix) ; mh mail message
	       (ispell-non-empty-string mh-ins-buf-prefix))
	      (mail-yank-prefix			; vanilla mail message.
	       (ispell-non-empty-string mail-yank-prefix))
	      (t ispell-message-cite-regexp)))
	    (continue t)
	    (limit
	     (min
	      (+ (point-min) ispell-message-limit)
	      (point-max)
	      (save-excursion
 		(cond
 		 ((not ispell-message-text-end) (point-max))
 		 ((char-or-string-p ispell-message-text-end)
 		  (if (re-search-forward ispell-message-text-end nil 'end)
 		      (match-beginning 0)
 		    (point-max)))
 		 (t (funcall ispell-message-text-end))))))
	    (search-limit ; Search limit which won't stop in middle of citation
	     (+ limit (length cite-regexp)))
	    )
 	;; Check the subject
 	(save-excursion
 	  (let ((case-fold-search t)
 		(message-begin (point)))
 	    (goto-char (point-min))
 	    ;; "\\s *" matches newline if subject is empty
 	    (if (and (re-search-forward "^Subject:[\t ]*" message-begin t)
 		     (not (looking-at "re\\>")))
 		(setq continue
 		      (ispell-region (- (point) 1)
 				     (progn
				       (end-of-line)
				       (while (looking-at "\n[ \t]")
					 (end-of-line 2))
				       (point))))
 	      )))

	;; Check the body.
	(while (and (< (point) limit) continue)
	  ;; Skip across text cited from other messages.
	  (while (and (looking-at (concat "^[ \t]*$\\|" cite-regexp))
		      (< (point) limit))
	    (forward-line 1))
	  (if (< (point) limit)
	      ;; Check the next batch of lines that *aren't* cited.
	      (let ((start (point)))
		(if (re-search-forward
		     (concat "^\\(" cite-regexp "\\)") search-limit 'end)
		    (beginning-of-line))
		(if (> (point) limit) (goto-char limit))
		(let ((case-fold-search old-case-fold-search))
		  (save-excursion
		    (setq continue (ispell-region (- start 1) (point))))))))))))

(provide 'ispell)

;;; ispell.el ends here
