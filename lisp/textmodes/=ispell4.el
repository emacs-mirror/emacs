;;; ispell.el --- this is the GNU EMACS interface to GNU ISPELL version 3.

;;Copyright (C) 1990, 1991 Free Software Foundation, Inc.

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
  "T if default 'look' program has the -r flag.")

(defvar ispell-enable-tex-parser nil
  "T to enable experimental tex parser in ispell for tex buffers.")

(defvar ispell-process nil "The process running ISPELL")
(defvar ispell-next-message nil
  "An integer telling where in the *ispell* buffer where
to look for the next message from the ISPELL program.")

;Each marker in this list points to the start of a word that
;ispell thought was bad last time it did the :file command.
;Notice that if the user accepts or inserts a word into his
;private dictionary, then some "good" words will be on the list.
;We would like to deal with this by looking up the words again just before
;presenting them to the user, but that is too slow on machines
;without the select system call.  Therefore, see the variable
;ispell-recently-accepted.
(defvar ispell-bad-words nil
  "A list of markers corresponding to the output of the ISPELL :file command.")

;list of words that the user has accepted, but that might still
;be on the bad-words list
(defvar ispell-recently-accepted nil)

;t when :dump command needed
(defvar ispell-dump-needed nil)

(defun ispell-flush-bad-words ()
  (while ispell-bad-words
    (if (markerp (car ispell-bad-words))
        (set-marker (car ispell-bad-words) nil))
    (setq ispell-bad-words (cdr ispell-bad-words)))
  (setq ispell-recently-accepted nil))

(defun kill-ispell ()
  "Kill the ispell process.
Any changes the your private dictionay
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

(defun start-ispell ()
  "Start an ispell subprocess; check the version; and display the greeting."
  (message "Starting ispell ...")
  (let ((buf (get-buffer "*ispell*")))
    (if buf
	(kill-buffer buf)))
  (condition-case err
      (setq ispell-process (start-process "ispell" "*ispell*" "ispell" "-S"))
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
  
;leaves buffer set to *ispell*, point at '='
(defun ispell-sync (intr)
  "Make sure ispell is ready for a command."
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

(defun ispell-cmd (&rest strings)
  "Send a command to ispell.  Choices are:

WORD		Check spelling of WORD.  Result is

			nil			   not found
			t			   spelled ok
			list of strings		   near misses

:file FILENAME	scan the named file, and print the file offsets of
		any misspelled words

:insert WORD	put word in private dictonary

:accept WORD	don't complain about word any more this session

:dump		write out the current private dictionary, if necessary.

:reload		reread `~/ispell.words'

:tex
:troff
:generic	set type of parser to use when scanning whole files
"
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


(defun ispell-next-message ()
  "Return the next message sent by the ispell subprocess."
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
  (memq major-mode '(plain-TeX-mode LaTeX-mode)))

;;;###autoload
(defun ispell (&optional buf start end)
  "Run Ispell over current buffer's visited file.
First the file is scanned for misspelled words, then Ispell
enters a loop with the following commands for every misspelled word:

DIGIT	Near miss selector.  If the misspelled word is close to
	some words in the dictionary, they are offered as near misses.
r	Replace.  Replace the word with a string you type.  Each word
	of your new string is also checked.
i	Insert.  Insert this word in your private dictonary (kept in
	`$HOME/ispell.words').
a	Accept.  Accept this word for the rest of this editing session,
 	but don't put it in your private dictonary.
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
  (save-excursion
    (set-buffer buf)
    (let ((filename buffer-file-name)
          (delete-temp nil))
      (unwind-protect
	  (progn
	    (cond ((null filename)
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
	 (message "No misspellings."))
	(t
	 (message "Ispell parsing done.")
	 (ispell-next))))

;;;###autoload
(defalias 'ispell-buffer 'ispell)

(defun ispell-next ()
  "Resume command loop for most recent ispell command."
  (interactive)
  (unwind-protect
      (catch 'quit
	(save-window-excursion
	  (save-excursion
	    (let (next)
	      (while (markerp (setq next (car ispell-bad-words)))
		(switch-to-buffer (marker-buffer next))
		(push-mark)
		(ispell-point next "at saved position.")
		(setq ispell-bad-words (cdr ispell-bad-words))
		(set-marker next nil))))))
    (cond ((null ispell-bad-words)
	   (error "Ispell has not yet been run."))
	  ((markerp (car ispell-bad-words))
	   (message (substitute-command-keys
                       "Type \\[ispell-next] to continue.")))
	  ((eq (car ispell-bad-words) nil)
	   (setq ispell-bad-words nil)
	   (message "No more misspellings (but checker was interrupted.)"))
	  ((eq (car ispell-bad-words) t)
	   (setq ispell-bad-words nil)
	   (message "Ispell done."))
	  (t
	   (setq ispell-bad-words nil)
	   (message "Bad ispell internal list"))))
  (ispell-dump))

;;;###autoload
(defun ispell-word (&optional resume)
  "Check the spelling of the word under the cursor.
See `ispell' for more information.
With a prefix argument, resume handling of the previous Ispell command."
  (interactive "P")
  (if resume
      (ispell-next)
    (condition-case err
	(catch 'quit
	  (save-window-excursion
	    (ispell-point (point) "at point."))
	  (ispell-dump))
      (ispell-startup-error
       (cond ((y-or-n-p "Problem starting ispell, use old-style spell instead? ")
	      (load-library "spell")
	      (define-key esc-map "$" 'spell-word)
	      (spell-word)))))))
;;;###autoload
(define-key esc-map "$" 'ispell-word)

;;;###autoload
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
  (save-excursion
    (goto-char start)
    (ispell-find-word-start)		;find correct word start
    (setq start (point-marker))
    (ispell-find-word-end)		;now find correct end
    (setq end (point-marker))
    (if (>= start end)
	(error "No word %s" message))
    (while (< start end)
      (goto-char start)
      (ispell-find-word-end)		;find end of current word
					;could be before 'end' if
					;user typed replacement
					;that is more than one word
      (set-marker wend (point))
      (setq rescan nil)
      (setq word (buffer-substring start wend))
      (cond ((ispell-still-bad word)
	     (goto-char start);just to show user where we are working
	     (sit-for 0)
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
	     (set-marker start (point)))))
    ;;clear the choices buffer; otherwise it's hard for the user to tell
    ;;when we get back to the command loop
    (let ((buf (get-buffer "*ispell choices*")))
      (cond (buf
	     (set-buffer buf)
	     (erase-buffer))))
    (set-marker start nil)
    (set-marker end nil)
    (set-marker wend nil))))
  
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
    (while flag
      (ispell-show-choices word message first-line)
      (message "Ispell command: ")
      (let ((c (downcase (read-char)))
	    replacement)
	(cond ((and (>= c ?0)
		    (<= c ?9)
		    (setq replacement (nth (- c ?0) message)))
	       (ispell-replace start end replacement)
	       (setq flag nil))
	      ((= c ?q)
	       (throw 'quit nil))
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
	  (call-process "look" nil buf nil "-r" regex)
	(call-process "look" nil buf nil regex))
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
  "Tell ispell to re-read your private dictionary."
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

;;; ispell.el ends here
