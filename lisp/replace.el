;;; replace.el --- replace commands for Emacs

;; Copyright (C) 1985, 86, 87, 92, 94, 96, 1997, 2000, 2001, 2002
;;  Free Software Foundation, Inc.

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

;; This package supplies the string and regular-expression replace functions
;; documented in the Emacs user's manual.

;;; Code:

(eval-when-compile
  (require 'cl))

(defcustom case-replace t
  "*Non-nil means `query-replace' should preserve case in replacements."
  :type 'boolean
  :group 'matching)

(defvar query-replace-history nil)

(defvar query-replace-interactive nil
  "Non-nil means `query-replace' uses the last search string.
That becomes the \"string to replace\".")

(defcustom query-replace-from-history-variable 'query-replace-history
  "History list to use for the FROM argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for the strings
or patterns to be replaced."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-to-history-variable 'query-replace-history
  "History list to use for the TO argument of `query-replace' commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'matching
  :type 'symbol
  :version "20.3")

(defcustom query-replace-skip-read-only nil
  "*Non-nil means `query-replace' and friends ignore read-only matches."
  :type 'boolean
  :group 'matching
  :version "21.3")

(defun query-replace-read-args (string regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " string)
				       nil nil nil
				       query-replace-from-history-variable
				       nil t))
      ;; Warn if user types \n or \t, but don't reject the input.
      (if (string-match "\\\\[nt]" from)
	  (let ((match (match-string 0 from)))
	    (cond
	     ((string= match "\\n")
	      (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
	     ((string= match "\\t")
	      (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
	    (sit-for 2))))

    (setq to (read-from-minibuffer (format "%s %s with: " string from)
				   nil nil nil
				   query-replace-to-history-variable from t))
    (if (and transient-mark-mode mark-active)
	(list from to current-prefix-arg (region-beginning) (region-end))
      (list from to current-prefix-arg nil nil))))

(defun query-replace (from-string to-string &optional delimited start end)
  "Replace some occurrences of FROM-STRING with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

Replacement transfers the case of the old text to the new text,
if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

To customize possible responses, change the \"bindings\" in `query-replace-map'."
  (interactive (query-replace-read-args "Query replace" nil))
  (perform-replace from-string to-string t nil delimited nil nil start end))

(define-key esc-map "%" 'query-replace)

(defun query-replace-regexp (regexp to-string &optional delimited start end)
  "Replace some things after point matching REGEXP with TO-STRING.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
 whatever what matched the Nth `\\(...\\)' in REGEXP."
  (interactive (query-replace-read-args "Query replace regexp" t))
  (perform-replace regexp to-string t t delimited nil nil start end))
(define-key esc-map [?\C-%] 'query-replace-regexp)

(defun query-replace-regexp-eval (regexp to-expr &optional delimited start end)
  "Replace some things after point matching REGEXP with the result of TO-EXPR.
As each match is found, the user must type a character saying
what to do with it.  For directions, type \\[help-command] at that time.

TO-EXPR is a Lisp expression evaluated to compute each replacement.  It may
reference `replace-count' to get the number of replacements already made.
If the result of TO-EXPR is not a string, it is converted to one using
`prin1-to-string' with the NOESCAPE argument (which see).

For convenience, when entering TO-EXPR interactively, you can use `\\&' or
`\\0' to stand for whatever matched the whole of REGEXP, and `\\N' (where
N is a digit) to stand for whatever matched the Nth `\\(...\\)' in REGEXP.
Use `\\#&' or `\\#N' if you want a number instead of a string.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the
minibuffer.

Preserves case in each replacement if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches that are surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (let (from to start end)
     (when (and transient-mark-mode mark-active)
       (setq start (region-beginning)
	     end (region-end)))
     (if query-replace-interactive
         (setq from (car regexp-search-ring))
       (setq from (read-from-minibuffer "Query replace regexp: "
                                        nil nil nil
                                        query-replace-from-history-variable
                                        nil t)))
     (setq to (list (read-from-minibuffer
                     (format "Query replace regexp %s with eval: " from)
                     nil nil t query-replace-to-history-variable from t)))
     ;; We make TO a list because replace-match-string-symbols requires one,
     ;; and the user might enter a single token.
     (replace-match-string-symbols to)
     (list from (car to) current-prefix-arg start end)))
  (perform-replace regexp (cons 'replace-eval-replacement to-expr)
		   t t delimited nil nil start end))

(defun map-query-replace-regexp (regexp to-strings &optional n start end)
  "Replace some matches for REGEXP with various strings, in rotation.
The second argument TO-STRINGS contains the replacement strings,
separated by spaces.  Third arg DELIMITED (prefix arg if interactive),
if non-nil, means replace only matches surrounded by word boundaries.
This command works like `query-replace-regexp' except that each
successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Non-interactively, TO-STRINGS may be a list of replacement strings.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

A prefix argument N says to use each replacement string N times
before rotating to the next.
Fourth and fifth arg START and END specify the region to operate on."
  (interactive
   (let (from to start end)
     (when (and transient-mark-mode mark-active)
       (setq start (region-beginning)
	     end (region-end)))
     (setq from (if query-replace-interactive
		    (car regexp-search-ring)
		  (read-from-minibuffer "Map query replace (regexp): "
					nil nil nil
					'query-replace-history nil t)))
     (setq to (read-from-minibuffer
	       (format "Query replace %s with (space-separated strings): "
		       from)
	       nil nil nil
	       'query-replace-history from t))
     (list from to start end current-prefix-arg)))
  (let (replacements)
    (if (listp to-strings)
	(setq replacements to-strings)
      (while (/= (length to-strings) 0)
	(if (string-match " " to-strings)
	    (setq replacements
		  (append replacements
			  (list (substring to-strings 0
					   (string-match " " to-strings))))
		  to-strings (substring to-strings
				       (1+ (string-match " " to-strings))))
	  (setq replacements (append replacements (list to-strings))
		to-strings ""))))
    (perform-replace regexp replacements t t nil n nil start end)))

(defun replace-string (from-string to-string &optional delimited start end)
  "Replace occurrences of FROM-STRING with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and FROM-STRING has no uppercase letters.
\(Preserving case means that if the string matched is all caps, or capitalized,
then its replacement is upcased or capitalized.)

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

If `query-replace-interactive' is non-nil, the last incremental search
string is used as FROM-STRING--you don't have to specify it with the
minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (search-forward FROM-STRING nil t)
    (replace-match TO-STRING nil t))
which will run faster and will not set the mark or print anything.
\(You may need a more complex loop if FROM-STRING can match the null string
and TO-STRING is also null.)"
  (interactive (query-replace-read-args "Replace string" nil))
  (perform-replace from-string to-string nil nil delimited nil nil start end))

(defun replace-regexp (regexp to-string &optional delimited start end)
  "Replace things after point matching REGEXP with TO-STRING.
Preserve case in each match if `case-replace' and `case-fold-search'
are non-nil and REGEXP has no uppercase letters.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Third arg DELIMITED (prefix arg if interactive), if non-nil, means replace
only matches surrounded by word boundaries.
Fourth and fifth arg START and END specify the region to operate on.

In TO-STRING, `\\&' stands for whatever matched the whole of REGEXP,
and `\\=\\N' (where N is a digit) stands for
 whatever what matched the Nth `\\(...\\)' in REGEXP.

If `query-replace-interactive' is non-nil, the last incremental search
regexp is used as REGEXP--you don't have to specify it with the minibuffer.

This function is usually the wrong thing to use in a Lisp program.
What you probably want is a loop like this:
  (while (re-search-forward REGEXP nil t)
    (replace-match TO-STRING nil nil))
which will run faster and will not set the mark or print anything."
  (interactive (query-replace-read-args "Replace regexp" t))
  (perform-replace regexp to-string nil t delimited nil nil start end))


(defvar regexp-history nil
  "History list for some commands that read regular expressions.")


(defalias 'delete-non-matching-lines 'keep-lines)
(defalias 'delete-matching-lines 'flush-lines)
(defalias 'count-matches 'how-many)


(defun keep-lines-read-args (prompt)
  "Read arguments for `keep-lines' and friends.
Prompt for a regexp with PROMPT.
Value is a list, (REGEXP)."
  (list (read-from-minibuffer prompt nil nil nil
			      'regexp-history nil t)))

(defun keep-lines (regexp &optional rstart rend)
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of the buffer."

  (interactive
   (keep-lines-read-args "Keep lines (containing match for regexp): "))
  (if rstart
      (goto-char (min rstart rend))
    (if (and transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point))
	  (case-fold-search  (and case-fold-search
				  (isearch-no-upper-case-p regexp t))))
      (while (< (point) rend)
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp rend 'move))
	    (delete-region start rend)
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))
	
	(setq start (save-excursion (forward-line 1) (point)))
	;; If the match was empty, avoid matching again at same place.
	(and (< (point) rend)
	     (= (match-beginning 0) (match-end 0))
	     (forward-char 1))))))


(defun flush-lines (regexp &optional rstart rend)
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of the buffer."

  (interactive
   (keep-lines-read-args "Flush lines (containing match for regexp): "))
  (if rstart
      (goto-char (min rstart rend))
    (if (and transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (copy-marker (region-end)))
      (setq rstart (point)
	    rend (point-max-marker)))
    (goto-char rstart))
  (let ((case-fold-search (and case-fold-search
			       (isearch-no-upper-case-p regexp t))))
    (save-excursion
      (while (and (< (point) rend)
		  (re-search-forward regexp rend t))
	(delete-region (save-excursion (goto-char (match-beginning 0))
				       (beginning-of-line)
				       (point))
		       (progn (forward-line 1) (point)))))))


(defun how-many (regexp &optional rstart rend)
  "Print number of matches for REGEXP following point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of the buffer."

  (interactive
   (keep-lines-read-args "How many matches for (regexp): "))
  (save-excursion
    (if rstart
	(goto-char (min rstart rend))
      (if (and transient-mark-mode mark-active)
	  (setq rstart (region-beginning)
		rend (copy-marker (region-end)))
	(setq rstart (point)
	      rend (point-max-marker)))
      (goto-char rstart))
    (let ((count 0)
	  opoint
	  (case-fold-search (and case-fold-search
				 (isearch-no-upper-case-p regexp t))))
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward regexp rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (message "%d occurrences" count))))


(defvar occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'occur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'occur-mode-goto-occurrence)
    (define-key map "\C-m" 'occur-mode-goto-occurrence)
    (define-key map "\o" 'occur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'occur-mode-display-occurrence)
    (define-key map "\M-n" 'occur-next)
    (define-key map "\M-p" 'occur-prev)
    (define-key map "g" 'revert-buffer)
    map)
  "Keymap for `occur-mode'.")

(defvar occur-revert-properties nil)

(put 'occur-mode 'mode-class 'special)
(defun occur-mode ()
  "Major mode for output from \\[occur].
\\<occur-mode-map>Move point to one of the items in this buffer, then use
\\[occur-mode-goto-occurrence] to go to the occurrence that the item refers to.
Alternatively, click \\[occur-mode-mouse-goto] on an item to go to it.

\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name "Occur")
  (make-local-variable 'revert-buffer-function)
  (set (make-local-variable 'font-lock-defaults)
       '(nil t nil nil nil
	     (font-lock-fontify-region-function . occur-fontify-region-function)
	     (font-lock-unfontify-region-function . occur-unfontify-region-function)))
  (setq revert-buffer-function 'occur-revert-function)
  (set (make-local-variable 'revert-buffer-function) 'occur-revert-function)
  (make-local-variable 'occur-revert-properties)
  (run-hooks 'occur-mode-hook))

(defun occur-revert-function (ignore1 ignore2)
  "Handle `revert-buffer' for *Occur* buffers."
  (apply 'occur-1 occur-revert-properties))

(defun occur-mode-mouse-goto (event)
  "In Occur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let ((buffer nil)
	(pos nil))
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(let ((props (occur-mode-find-occurrence)))
	  (setq buffer (car props))
	  (setq pos (cdr props)))))
    (pop-to-buffer buffer)
    (goto-char (marker-position pos))))

(defun occur-mode-find-occurrence ()
  (let ((props (get-text-property (point) 'occur-target)))
    (unless props
      (error "No occurrence on this line"))
    (unless (buffer-live-p (car props))
      (error "Buffer in which occurrence was found is deleted"))
    props))

(defun occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((target (occur-mode-find-occurrence)))
    (pop-to-buffer (car target))
    (goto-char (marker-position (cdr target)))))

(defun occur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((target (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (car target))
    (goto-char (marker-position (cdr target)))))

(defun occur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((target (occur-mode-find-occurrence))
	same-window-buffer-names
	same-window-regexps
	window)
    (setq window (display-buffer (car target)))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char (marker-position (cdr target))))))

(defun occur-next (&optional n)
  "Move to the Nth (default 1) next match in the *Occur* buffer."
  (interactive "p")
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
      (if (get-text-property (point) 'occur-point)
	  (forward-char 1))
      (setq r (next-single-property-change (point) 'occur-point))
      (if r
	  (goto-char r)
	(error "No more matches"))
      (setq n (1- n)))))

(defun occur-prev (&optional n)
  "Move to the Nth (default 1) previous match in the *Occur* buffer."
  (interactive "p")
  (if (not n) (setq n 1))
  (let ((r))
    (while (> n 0)
    
      (setq r (get-text-property (point) 'occur-point))
      (if r (forward-char -1))
      
      (setq r (previous-single-property-change (point) 'occur-point))
      (if r
	  (goto-char (- r 1))
	(error "No earlier matches"))
      
      (setq n (1- n)))))

(defcustom list-matching-lines-default-context-lines 0
  "*Default number of context lines included around `list-matching-lines' matches.
A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after."
  :type 'integer
  :group 'matching)

(defalias 'list-matching-lines 'occur)

(defcustom list-matching-lines-face 'bold
  "*Face used by \\[list-matching-lines] to show the text that matches.
If the value is nil, don't highlight the matching portions specially."
  :type 'face
  :group 'matching)

(defcustom list-matching-lines-buffer-name-face 'underline
  "*Face used by \\[list-matching-lines] to show the names of buffers.
If the value is nil, don't highlight the buffer names specially."
  :type 'face
  :group 'matching)

(defun occur-accumulate-lines (count &optional no-props)
  (save-excursion
    (let ((forwardp (> count 0))
	  (result nil))
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(bobp))))
	(if forwardp
	    (decf count)
	  (incf count))
	(push
	 (funcall (if no-props
		      #'buffer-substring-no-properties
		    #'buffer-substring)
	  (line-beginning-position)
	  (line-end-position))
	 result)
	(forward-line (if forwardp 1 -1)))
      (nreverse result))))

(defun occur-read-primary-args ()
  (list (let* ((default (car regexp-history))
	       (input
		(read-from-minibuffer
		 (if default
		     (format "List lines matching regexp (default `%s'): "
			     default)
		   "List lines matching regexp: ")
		 nil
		 nil
		 nil
		 'regexp-history)))
	  (if (equal input "")
	      default
	    input))
	current-prefix-arg))

(defun occur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<occur-mode-map>\\[describe-mode] in that buffer will explain how.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive."
  (interactive (occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

(defun multi-occur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`occur'."
  (interactive
   (cons
    (let ((bufs (list (read-buffer "First buffer to search: "
				   (current-buffer) t)))
	  (buf nil))
      (while (not (string-equal
		   (setq buf (read-buffer "Next buffer to search (RET to end): "
					  nil t))
		   ""))
	(push buf bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (occur-read-primary-args)))
  (occur-1 regexp nlines bufs))

(defun multi-occur-by-filename-regexp (bufregexp regexp &optional nlines)
  "Show all lines matching REGEXP in buffers named by BUFREGEXP.
See also `multi-occur'."
  (interactive
   (cons
    (let* ((default (car regexp-history))
	   (input
	    (read-from-minibuffer
	     "List lines in buffers whose filename matches regexp: "
	     nil
	     nil
	     nil
	     'regexp-history)))
      (if (equal input "")
	  default
	input))
    (occur-read-primary-args)))
  (when bufregexp
    (occur-1 regexp nlines
	     (delq nil
		   (mapcar (lambda (buf)
			     (when (and (buffer-file-name buf)
					(string-match bufregexp
						      (buffer-file-name buf)))
			       buf))
			   (buffer-list))))))

(defun occur-1 (regexp nlines bufs)
  (let ((occur-buf (get-buffer-create "*Occur*")))
    (with-current-buffer occur-buf
      (setq buffer-read-only nil)
      (occur-mode)
      (erase-buffer)
      (let ((count (occur-engine
		    regexp bufs occur-buf
		    (or nlines list-matching-lines-default-context-lines)
		    (and case-fold-search
			 (isearch-no-upper-case-p regexp t))
		    nil nil nil nil)))
	(message "Searched %d buffers; %s matches for `%s'" (length bufs)
		 (if (zerop count)
		     "no"
		   (format "%d" count))
		 regexp)
	(if (> count 0)
	    (display-buffer occur-buf)
	  (kill-buffer occur-buf)))
      (setq occur-revert-properties (list regexp nlines bufs)
	    buffer-read-only t))))

;; Most of these are macros becuase if we used `flet', it wouldn't
;; create a closure, so things would blow up at run time.  Ugh. :(
(macrolet ((insert-get-point (obj)
	     `(progn
		(insert ,obj)
		(point)))
	   (add-prefix (lines)
	     `(mapcar
		 #'(lambda (line)
		     (concat "      :" line "\n"))
		 ,lines)))
  (defun occur-engine (regexp buffers out-buf nlines case-fold-search
			      title-face prefix-face match-face keep-props)
    (with-current-buffer out-buf
      (setq buffer-read-only nil)
      (let ((globalcount 0))
	;; Map over all the buffers
	(dolist (buf buffers)
	  (when (buffer-live-p buf)
	    (let ((c 0)	;; count of matched lines
		  (l 1)	;; line count
		  (matchbeg 0)
		  (matchend 0)
		  (origpt nil)
		  (begpt nil)
		  (endpt nil)
		  (marker nil)
		  (curstring "")
		  (headerpt (with-current-buffer out-buf (point))))
	      (save-excursion
		(set-buffer buf)
		(save-excursion
		  (goto-char (point-min)) ;; begin searching in the buffer
		  (while (not (eobp))
		    (setq origpt (point))
		    (when (setq endpt (re-search-forward regexp nil t))
			(incf c) ;; increment match count
			(incf globalcount)
			(setq matchbeg (match-beginning 0)
			      matchend (match-end 0))
			(setq begpt (save-excursion
				      (goto-char matchbeg)
				      (line-beginning-position)))
			(incf l (1- (count-lines origpt endpt)))
			(setq marker (make-marker))
			(set-marker marker matchbeg)
			(setq curstring (buffer-substring begpt
					 (line-end-position)))
			;; Depropertize the string, and maybe
			;; highlight the matches
			(let ((len (length curstring))
				      (start 0))
				  (unless keep-props
				    (set-text-properties 0 len nil curstring))
				  (while (and (< start len)
					      (string-match regexp curstring start))
				    (add-text-properties (match-beginning 0)
							 (match-end 0)
							 (append
							  '(occur-match t)
							  (when match-face
							    `(face ,match-face)))
							 curstring)
				    (setq start (match-end 0))))
			;; Generate the string to insert for this match
			(let* ((out-line
				(concat
				 (apply #'propertize (format "%6d:" l)
					(append
					 (when prefix-face
					   `(face prefix-face))
					 '(occur-prefix t)))
				 curstring
				 "\n"))
			       (data
				(if (= nlines 0)
				    ;; The simple display style
				    out-line
				 ;; The complex multi-line display
				 ;; style.  Generate a list of lines,
				 ;; concatenate them all together.
				 (apply #'concat
					(nconc
					 (add-prefix (nreverse (cdr (occur-accumulate-lines (- (1+ nlines)) t))))
					 (list out-line)
					 (add-prefix (cdr (occur-accumulate-lines (1+ nlines) t))))))))
			  ;; Actually insert the match display data
			  (with-current-buffer out-buf
			    (let ((beg (point))
				  (end (insert-get-point data)))
			      (unless (= nlines 0)
				(insert-get-point "-------\n"))
			      (add-text-properties
			       beg (1- end)
			       `(occur-target ,(cons buf marker)
					      mouse-face highlight help-echo
					      "mouse-2: go to this occurrence")))))
			(goto-char endpt))
		    (incf l)
		    ;; On to the next match...
		    (forward-line 1))))
	      (when (not (zerop c)) ;; is the count zero?
		(with-current-buffer out-buf
		  (goto-char headerpt)
		  (let ((beg (point))
			(end (insert-get-point
			      (format "%d lines matching \"%s\" in buffer: %s\n"
				      c regexp (buffer-name buf)))))
		    (add-text-properties beg end
					 (append
					  (when title-face
					    `(face ,title-face))
					  `(occur-title ,buf))))
		  (goto-char (point-min)))))))
	;; Return the number of matches
	globalcount))))

(defun occur-fontify-on-property (prop face beg end)
  (let ((prop-beg (or (and (get-text-property (point) prop) (point))
		      (next-single-property-change (point) prop nil end))))
    (when (and prop-beg (not (= prop-beg end)))
      (let ((prop-end (next-single-property-change beg prop nil end)))
	(when (and prop-end (not (= prop-end end)))
	  (put-text-property prop-beg prop-end 'face face)
	  prop-end)))))

(defun occur-fontify-region-function (beg end &optional verbose)
  (when verbose (message "Fontifying..."))
  (let ((inhibit-read-only t))
    (save-excursion
      (dolist (e `((occur-title . ,list-matching-lines-buffer-name-face)
		   (occur-match . ,list-matching-lines-face)))
		   ; (occur-prefix . ,list-matching-lines-prefix-face)))
	(goto-char beg)
	(let ((change-end nil))
	  (while (setq change-end (occur-fontify-on-property (car e)
							     (cdr e)
							     (point)
							     end))
	    (goto-char change-end))))))
  (when verbose (message "Fontifying...done")))

(defun occur-unfontify-region-function (beg end)
  (let ((inhibit-read-only t))
    (remove-text-properties beg end '(face nil))))


;; It would be nice to use \\[...], but there is no reasonable way
;; to make that display both SPC and Y.
(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match,
E to edit the replacement string"
  "Help message while in `query-replace'.")

(defvar query-replace-map (make-sparse-keymap)
  "Keymap that defines the responses to questions in `query-replace'.
The \"bindings\" in this map are not commands; they are answers.
The valid answers include `act', `skip', `act-and-show',
`exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
`automatic', `backup', `exit-prefix', and `help'.")

(define-key query-replace-map " " 'act)
(define-key query-replace-map "\d" 'skip)
(define-key query-replace-map [delete] 'skip)
(define-key query-replace-map [backspace] 'skip)
(define-key query-replace-map "y" 'act)
(define-key query-replace-map "n" 'skip)
(define-key query-replace-map "Y" 'act)
(define-key query-replace-map "N" 'skip)
(define-key query-replace-map "e" 'edit-replacement)
(define-key query-replace-map "E" 'edit-replacement)
(define-key query-replace-map "," 'act-and-show)
(define-key query-replace-map "q" 'exit)
(define-key query-replace-map "\r" 'exit)
(define-key query-replace-map [return] 'exit)
(define-key query-replace-map "." 'act-and-exit)
(define-key query-replace-map "\C-r" 'edit)
(define-key query-replace-map "\C-w" 'delete-and-edit)
(define-key query-replace-map "\C-l" 'recenter)
(define-key query-replace-map "!" 'automatic)
(define-key query-replace-map "^" 'backup)
(define-key query-replace-map "\C-h" 'help)
(define-key query-replace-map [f1] 'help)
(define-key query-replace-map [help] 'help)
(define-key query-replace-map "?" 'help)
(define-key query-replace-map "\C-g" 'quit)
(define-key query-replace-map "\C-]" 'quit)
(define-key query-replace-map "\e" 'exit-prefix)
(define-key query-replace-map [escape] 'exit-prefix)

(defun replace-match-string-symbols (n)
  "Process a list (and any sub-lists), expanding certain symbols.
Symbol  Expands To
N     (match-string N)           (where N is a string of digits)
#N    (string-to-number (match-string N))
&     (match-string 0)
#&    (string-to-number (match-string 0))

Note that these symbols must be preceeded by a backslash in order to
type them."
  (while n
    (cond
     ((consp (car n))
      (replace-match-string-symbols (car n))) ;Process sub-list
     ((symbolp (car n))
      (let ((name (symbol-name (car n))))
        (cond
         ((string-match "^[0-9]+$" name)
          (setcar n (list 'match-string (string-to-number name))))
         ((string-match "^#[0-9]+$" name)
          (setcar n (list 'string-to-number
                          (list 'match-string
                                (string-to-number (substring name 1))))))
         ((string= "&" name)
          (setcar n '(match-string 0)))
         ((string= "#&" name)
          (setcar n '(string-to-number (match-string 0))))))))
    (setq n (cdr n))))

(defun replace-eval-replacement (expression replace-count)
  (let ((replacement (eval expression)))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-loop-through-replacements (data replace-count)
  ;; DATA is a vector contaning the following values:
  ;;   0 next-rotate-count
  ;;   1 repeat-count
  ;;   2 next-replacement
  ;;   3 replacements
  (if (= (aref data 0) replace-count)
      (progn
        (aset data 0 (+ replace-count (aref data 1)))
        (let ((next (cdr (aref data 2))))
          (aset data 2 (if (consp next) next (aref data 3))))))
  (car (aref data 2)))

(defun perform-replace (from-string replacements 
		        query-flag regexp-flag delimited-flag
			&optional repeat-count map start end)
  "Subroutine of `query-replace'.  Its complexity handles interactive queries.
Don't use this in your own program unless you want to query and set the mark
just as `query-replace' does.  Instead, write a simple loop like this:

  (while (re-search-forward \"foo[ \\t]+bar\" nil t)
    (replace-match \"foobar\" nil nil))

which will run faster and probably do exactly what you want.  Please
see the documentation of `replace-match' to find out how to simulate
`case-replace'."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(case-fold-search (and case-fold-search
			       (string-equal from-string
					     (downcase from-string))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(real-match-data nil)		; the match data for the current match
	(next-replacement nil)
	(keep-going t)
	(stack nil)
	(replace-count 0)
	(nonempty-match nil)

	;; If non-nil, it is marker saying where in the buffer to stop.
	(limit nil)

	;; Data for the next match.  If a cons, it has the same format as
	;; (match-data); otherwise it is t if a match is possible at point.
	(match-again t)

	(message
	 (if query-flag
	     (substitute-command-keys
	      "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) "))))

    ;; If region is active, in Transient Mark mode, operate on region.
    (when start
      (setq limit (copy-marker (max start end)))
      (goto-char (min start end))
      (deactivate-mark))

    ;; REPLACEMENTS is either a string, a list of strings, or a cons cell
    ;; containing a function and its first argument.  The function is
    ;; called to generate each replacement like this:
    ;;   (funcall (car replacements) (cdr replacements) replace-count)
    ;; It must return a string.
    (cond
     ((stringp replacements)
      (setq next-replacement replacements
            replacements     nil))
     ((stringp (car replacements)) ; If it isn't a string, it must be a cons
      (or repeat-count (setq repeat-count 1))
      (setq replacements (cons 'replace-loop-through-replacements
                               (vector repeat-count repeat-count
                                       replacements replacements)))))

    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (eobp))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (if (consp match-again)
			      (progn (goto-char (nth 1 match-again))
				     match-again)
			    (and (or match-again
				     ;; MATCH-AGAIN non-nil means we
				     ;; accept an adjacent match.  If
				     ;; we don't, move one char to the
				     ;; right.  This takes us a
				     ;; character too far at the end,
				     ;; but this is undone after the
				     ;; while-loop.
				     (progn (forward-char 1) (not (eobp))))
				 (funcall search-function search-string limit t)
				 ;; For speed, use only integers and
				 ;; reuse the list used last time.
				 (match-data t real-match-data)))))
	  ;; Optionally ignore matches that have a read-only property.
	  (unless (and query-replace-skip-read-only
		       (text-property-not-all
			(match-beginning 0) (match-end 0)
			'read-only nil))

	    ;; Record whether the match is nonempty, to avoid an infinite loop
	    ;; repeatedly matching the same empty string.
	    (setq nonempty-match
		  (/= (nth 0 real-match-data) (nth 1 real-match-data)))

	    ;; If the match is empty, record that the next one can't be
	    ;; adjacent.

	    ;; Otherwise, if matching a regular expression, do the next
	    ;; match now, since the replacement for this match may
	    ;; affect whether the next match is adjacent to this one.
	    ;; If that match is empty, don't use it.
	    (setq match-again
		  (and nonempty-match
		       (or (not regexp-flag)
			   (and (looking-at search-string)
				(let ((match (match-data)))
				  (and (/= (nth 0 match) (nth 1 match))
				       match))))))

	    ;; Calculate the replacement string, if necessary.
	    (when replacements
	      (set-match-data real-match-data)
	      (setq next-replacement
		    (funcall (car replacements) (cdr replacements)
			     replace-count)))
	    (if (not query-flag)
		(let ((inhibit-read-only query-replace-skip-read-only))
		  (set-match-data real-match-data)
		  (replace-match next-replacement nocasify literal)
		  (setq replace-count (1+ replace-count)))
	      (undo-boundary)
	      (let (done replaced key def)
		;; Loop reading commands until one of them sets done,
		;; which means it has finished handling this occurrence.
		(while (not done)
		  (set-match-data real-match-data)
		  (replace-highlight (match-beginning 0) (match-end 0))
		  ;; Bind message-log-max so we don't fill up the message log
		  ;; with a bunch of identical messages.
		  (let ((message-log-max nil))
		    (message message from-string next-replacement))
		  (setq key (read-event))
		  ;; Necessary in case something happens during read-event
		  ;; that clobbers the match data.
		  (set-match-data real-match-data)
		  (setq key (vector key))
		  (setq def (lookup-key map key))
		  ;; Restore the match data while we process the command.
		  (cond ((eq def 'help)
			 (with-output-to-temp-buffer "*Help*"
			   (princ
			    (concat "Query replacing "
				    (if regexp-flag "regexp " "")
				    from-string " with "
				    next-replacement ".\n\n"
				    (substitute-command-keys
				     query-replace-help)))
			   (with-current-buffer standard-output
			     (help-mode))))
			((eq def 'exit)
			 (setq keep-going nil)
			 (setq done t))
			((eq def 'backup)
			 (if stack
			     (let ((elt (car stack)))
			       (goto-char (car elt))
			       (setq replaced (eq t (cdr elt)))
			       (or replaced
				   (set-match-data (cdr elt)))
			       (setq stack (cdr stack)))
			   (message "No previous match")
			   (ding 'no-terminate)
			   (sit-for 1)))
			((eq def 'act)
			 (or replaced
			     (progn
			       (replace-match next-replacement nocasify literal)
			       (setq replace-count (1+ replace-count))))
			 (setq done t replaced t))
			((eq def 'act-and-exit)
			 (or replaced
			     (progn
			       (replace-match next-replacement nocasify literal)
			       (setq replace-count (1+ replace-count))))
			 (setq keep-going nil)
			 (setq done t replaced t))
			((eq def 'act-and-show)
			 (if (not replaced)
			     (progn
			       (replace-match next-replacement nocasify literal)
			       (setq replace-count (1+ replace-count))
			       (setq replaced t))))
			((eq def 'automatic)
			 (or replaced
			     (progn
			       (replace-match next-replacement nocasify literal)
			       (setq replace-count (1+ replace-count))))
			 (setq done t query-flag nil replaced t))
			((eq def 'skip)
			 (setq done t))
			((eq def 'recenter)
			 (recenter nil))
			((eq def 'edit)
			 (let ((opos (point-marker)))
			   (goto-char (match-beginning 0))
			   (save-excursion
			     (funcall search-function search-string limit t)
			     (setq real-match-data (match-data)))
			   (save-excursion
			     (save-window-excursion
			       (recursive-edit)))
			   (goto-char opos))
			 (set-match-data real-match-data)
			 ;; Before we make the replacement,
			 ;; decide whether the search string
			 ;; can match again just after this match.
			 (if (and regexp-flag nonempty-match)
			     (setq match-again (and (looking-at search-string)
						    (match-data)))))
		      
			;; Edit replacement.
			((eq def 'edit-replacement)
			 (setq next-replacement
			       (read-input "Edit replacement string: "
					   next-replacement))
			 (or replaced
			     (replace-match next-replacement nocasify literal))
			 (setq done t))
		      
			((eq def 'delete-and-edit)
			 (delete-region (match-beginning 0) (match-end 0))
			 (set-match-data
			  (prog1 (match-data)
			    (save-excursion (recursive-edit))))
			 (setq replaced t))
			;; Note: we do not need to treat `exit-prefix'
			;; specially here, since we reread
			;; any unrecognized character.
			(t
			 (setq this-command 'mode-exited)
			 (setq keep-going nil)
			 (setq unread-command-events
			       (append (listify-key-sequence key)
				       unread-command-events))
			 (setq done t))))
		;; Record previous position for ^ when we move on.
		;; Change markers to numbers in the match data
		;; since lots of markers slow down editing.
		(setq stack
		      (cons (cons (point)
				  (or replaced (match-data t)))
			    stack))))))

      ;; The code preventing adjacent regexp matches in the condition
      ;; of the while-loop above will haven taken us one character
      ;; beyond the last replacement.  Undo that.
      (when (and regexp-flag (not match-again) (> replace-count 0))
	(backward-char 1))
      
      (replace-dehighlight))
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (and keep-going stack)))

(defcustom query-replace-highlight t
  "*Non-nil means to highlight words during query replacement."
  :type 'boolean
  :group 'matching)

(defvar replace-overlay nil)

(defun replace-dehighlight ()
  (and replace-overlay
       (progn
	 (delete-overlay replace-overlay)
	 (setq replace-overlay nil))))

(defun replace-highlight (start end)
  (and query-replace-highlight
       (progn
	 (or replace-overlay
	     (progn
	       (setq replace-overlay (make-overlay start end))
	       (overlay-put replace-overlay 'face
			    (if (facep 'query-replace)
				'query-replace 'region))))
	 (move-overlay replace-overlay start end (current-buffer)))))

;;; replace.el ends here
