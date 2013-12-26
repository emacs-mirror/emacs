;;; electric.el --- window maker and Command loop for `electric' modes

;; Copyright (C) 1985-1986, 1995, 2001-2013 Free Software Foundation,
;; Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; "Electric" has been used in Emacs to refer to different things.
;; Among them:
;;
;; - electric modes and buffers: modes that typically pop-up in a modal kind of
;;   way a transient buffer that automatically disappears as soon as the user
;;   is done with it.
;;
;; - electric keys: self inserting keys which additionally perform some side
;;   operation which happens to be often convenient at that time.  Examples of
;;   such side operations are: reindenting code, inserting a newline,
;;   ... auto-fill-mode and abbrev-mode can be considered as built-in forms of
;;   electric key behavior.

;;; Code:

;; This loop is the guts for non-standard modes which retain control
;; until some event occurs.  It is a `do-forever', the only way out is
;; to throw.  It assumes that you have set up the keymap, window, and
;; everything else: all it does is read commands and execute them -
;; providing error messages should one occur (if there is no loop
;; function - which see).  The required argument is a tag which should
;; expect a value of nil if the user decides to punt. The second
;; argument is the prompt to be used: if nil, use "->", if 'noprompt,
;; don't use a prompt, if a string, use that string as prompt, and if
;; a function of no variable, it will be evaluated in every iteration
;; of the loop and its return value, which can be nil, 'noprompt or a
;; string, will be used as prompt.  Given third argument non-nil, it
;; INHIBITS quitting unless the user types C-g at toplevel.  This is
;; so user can do things like C-u C-g and not get thrown out.  Fourth
;; argument, if non-nil, should be a function of two arguments which
;; is called after every command is executed.  The fifth argument, if
;; provided, is the state variable for the function.  If the
;; loop-function gets an error, the loop will abort WITHOUT throwing
;; (moral: use unwind-protect around call to this function for any
;; critical stuff).  The second argument for the loop function is the
;; conditions for any error that occurred or nil if none.

(defun Electric-command-loop (return-tag
			      &optional prompt inhibit-quitting
					loop-function loop-state)

  (let (cmd
        (err nil)
        (inhibit-quit inhibit-quitting)
        (prompt-string prompt))
    (while t
      (if (functionp prompt)
          (setq prompt-string (funcall prompt)))
      (if (not (stringp prompt-string))
          (setq prompt-string (unless (eq prompt-string 'noprompt) "->")))
      (setq cmd (read-key-sequence prompt-string))
      (setq last-command-event (aref cmd (1- (length cmd)))
	    this-command (key-binding cmd t)
	    cmd this-command)
      (if (or (prog1 quit-flag (setq quit-flag nil))
	      (eq last-input-event ?\C-g))
	  (progn (setq unread-command-events nil
		       prefix-arg nil)
		 ;; If it wasn't canceling a prefix character, then quit.
		 (if (or (= (length (this-command-keys)) 1)
			 (not inhibit-quit)) ; safety
		     (progn (ding)
			    (message "Quit")
			    (throw return-tag nil))
		   (setq cmd nil))))
      (setq current-prefix-arg prefix-arg)
      (if cmd
	  (condition-case conditions
	      (progn (command-execute cmd)
		     (setq last-command this-command)
		     (if (or (prog1 quit-flag (setq quit-flag nil))
			     (eq last-input-event ?\C-g))
			 (progn (setq unread-command-events nil)
				(if (not inhibit-quit)
				    (progn (ding)
					   (message "Quit")
					   (throw return-tag nil))
				  (ding)))))
	    (buffer-read-only (if loop-function
				  (setq err conditions)
				(ding)
				(message "Buffer is read-only")
				(sit-for 2)))
	    (beginning-of-buffer (if loop-function
				     (setq err conditions)
				   (ding)
				   (message "Beginning of Buffer")
				   (sit-for 2)))
	    (end-of-buffer (if loop-function
			       (setq err conditions)
			     (ding)
			     (message "End of Buffer")
			     (sit-for 2)))
	    (error (if loop-function
		       (setq err conditions)
		     (ding)
		     (message "Error: %s"
			      (if (eq (car conditions) 'error)
				  (car (cdr conditions))
				(prin1-to-string conditions)))
		     (sit-for 2))))
	(ding))
      (if loop-function (funcall loop-function loop-state err))))
  (ding)
  (throw return-tag nil))

;; This function is like pop-to-buffer, sort of.
;; The algorithm is
;; If there is a window displaying buffer
;; 	Select it
;; Else if there is only one window
;; 	Split it, selecting the window on the bottom with height being
;; 	the lesser of max-height (if non-nil) and the number of lines in
;;      the buffer to be displayed subject to window-min-height constraint.
;; Else
;; 	Switch to buffer in the current window.
;;
;; Then if max-height is nil, and not all of the lines in the buffer
;; are displayed, grab the whole frame.
;;
;; Returns selected window on buffer positioned at point-min.

(defun Electric-pop-up-window (buffer &optional max-height)
  (let* ((win (or (get-buffer-window buffer) (selected-window)))
	 (buf (get-buffer buffer))
	 (one-window (one-window-p t))
	 (pop-up-windows t)
	 (pop-up-frames nil))
    (if (not buf)
	(error "Buffer %s does not exist" buffer)
      (cond ((and (eq (window-buffer win) buf))
	     (select-window win))
	    (one-window
	     (pop-to-buffer buffer)
	     (setq win (selected-window)))
	    (t
	     (switch-to-buffer buf)))
      ;; Don't shrink the window, but expand it if necessary.
      (goto-char (point-min))
      (unless (= (point-max) (window-end win t))
	(fit-window-to-buffer win max-height))
      win)))

;;; Electric keys.

(defgroup electricity ()
  "Electric behavior for self inserting keys."
  :group 'editing)

(defun electric--after-char-pos ()
  "Return the position after the char we just inserted.
Returns nil when we can't find this char."
  (let ((pos (point)))
    (when (or (eq (char-before) last-command-event) ;; Sanity check.
              (save-excursion
                (or (progn (skip-chars-backward " \t")
                           (setq pos (point))
                           (eq (char-before) last-command-event))
                    (progn (skip-chars-backward " \n\t")
                           (setq pos (point))
                           (eq (char-before) last-command-event)))))
      pos)))

(defun electric--sort-post-self-insertion-hook ()
  "Ensure order of electric functions in `post-self-insertion-hook'.

Hooks in this variable interact in non-trivial ways, so a
relative order must be maintained within it."
  (setq-default post-self-insert-hook
                (sort (default-value 'post-self-insert-hook)
                      #'(lambda (fn1 fn2)
                          (< (or (get fn1 'priority) 0)
                             (or (get fn2 'priority) 0))))))

;;; Electric indentation.

;; Autoloading variables is generally undesirable, but major modes
;; should usually set this variable by adding elements to the default
;; value, which only works well if the variable is preloaded.
;;;###autoload
(defvar electric-indent-chars '(?\n)
  "Characters that should cause automatic reindentation.")

(defvar electric-indent-functions nil
  "Special hook run to decide whether to auto-indent.
Each function is called with one argument (the inserted char), with
point right after that char, and it should return t to cause indentation,
`no-indent' to prevent indentation or nil to let other functions decide.")

(defvar-local electric-indent-inhibit nil
  "If non-nil, reindentation is not appropriate for this buffer.
This should be set by major modes such as `python-mode' since
Python does not lend itself to fully automatic indentation.")

(defvar electric-indent-functions-without-reindent
  '(indent-relative indent-to-left-margin indent-relative-maybe
    py-indent-line coffee-indent-line org-indent-line
    haskell-indentation-indent-line haskell-indent-cycle haskell-simple-indent)
  "List of indent functions that can't reindent.
If `line-indent-function' is one of those, then `electric-indent-mode' will
not try to reindent lines.  It is normally better to make the major
mode set `electric-indent-inhibit', but this can be used as a workaround.")

(defun electric-indent-post-self-insert-function ()
  ;; FIXME: This reindents the current line, but what we really want instead is
  ;; to reindent the whole affected text.  That's the current line for simple
  ;; cases, but not all cases.  We do take care of the newline case in an
  ;; ad-hoc fashion, but there are still missing cases such as the case of
  ;; electric-pair-mode wrapping a region with a pair of parens.
  ;; There might be a way to get it working by analyzing buffer-undo-list, but
  ;; it looks challenging.
  (let (pos)
    (when (and
           electric-indent-mode
           ;; Don't reindent while inserting spaces at beginning of line.
           (or (not (memq last-command-event '(?\s ?\t)))
               (save-excursion (skip-chars-backward " \t") (not (bolp))))
           (setq pos (electric--after-char-pos))
           (save-excursion
             (goto-char pos)
             (let ((act (or (run-hook-with-args-until-success
                             'electric-indent-functions
                             last-command-event)
                            (memq last-command-event electric-indent-chars))))
               (not
                (or (memq act '(nil no-indent))
                    ;; In a string or comment.
                    (unless (eq act 'do-indent) (nth 8 (syntax-ppss))))))))
      ;; For newline, we want to reindent both lines and basically behave like
      ;; reindent-then-newline-and-indent (whose code we hence copied).
      (when (<= pos (line-beginning-position))
        (let ((before (copy-marker (1- pos) t)))
          (save-excursion
            (unless (or (memq indent-line-function
                              electric-indent-functions-without-reindent)
                        electric-indent-inhibit)
              ;; Don't reindent the previous line if the indentation function
              ;; is not a real one.
              (goto-char before)
              (indent-according-to-mode))
            ;; We are at EOL before the call to indent-according-to-mode, and
            ;; after it we usually are as well, but not always.  We tried to
            ;; address it with `save-excursion' but that uses a normal marker
            ;; whereas we need `move after insertion', so we do the
            ;; save/restore by hand.
            (goto-char before)
	    (when (eolp)
	      ;; Remove the trailing whitespace after indentation because
	      ;; indentation may (re)introduce the whitespace.
	      (delete-horizontal-space t)))))
      (unless (and electric-indent-inhibit
                   (> pos (line-beginning-position)))
        (indent-according-to-mode)))))

(put 'electric-indent-post-self-insert-function 'priority  60)

(defun electric-indent-just-newline (arg)
  "Insert just a newline, without any auto-indentation."
  (interactive "*P")
  (let ((electric-indent-mode nil))
    (newline arg 'interactive)))

;;;###autoload
(define-minor-mode electric-indent-mode
  "Toggle on-the-fly reindentation (Electric Indent mode).
With a prefix argument ARG, enable Electric Indent mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a global minor mode.  When enabled, it reindents whenever
the hook `electric-indent-functions' returns non-nil, or you
insert a character from `electric-indent-chars'."
  :global t :group 'electricity
  :initialize 'custom-initialize-delay
  :init-value t
  (if (not electric-indent-mode)
      (progn
        (when (eq (lookup-key global-map [?\C-j])
                  'electric-indent-just-newline)
          (define-key global-map [?\C-j] 'newline-and-indent))
        (remove-hook 'post-self-insert-hook
                     #'electric-indent-post-self-insert-function))
    (when (eq (lookup-key global-map [?\C-j]) 'newline-and-indent)
      (define-key global-map [?\C-j] 'electric-indent-just-newline))
    (add-hook 'post-self-insert-hook
              #'electric-indent-post-self-insert-function)
    (electric--sort-post-self-insertion-hook)))

;;;###autoload
(define-minor-mode electric-indent-local-mode
  "Toggle `electric-indent-mode' only in this buffer."
  :variable (buffer-local-value 'electric-indent-mode (current-buffer))
  (cond
   ((eq electric-indent-mode (default-value 'electric-indent-mode))
    (kill-local-variable 'electric-indent-mode))
   ((not (default-value 'electric-indent-mode))
    ;; Locally enabled, but globally disabled.
    (electric-indent-mode 1)                ; Setup the hooks.
    (setq-default electric-indent-mode nil) ; But keep it globally disabled.
    )))

;;; Electric pairing.

(defcustom electric-pair-pairs
  '((?\" . ?\"))
  "Alist of pairs that should be used regardless of major mode.

Pairs of delimiters in this list are a fallback in case they have
no syntax relevant to `electric-pair-mode' in the mode's syntax
table.

See also the variable `electric-pair-text-pairs'."
  :version "24.1"
  :type '(repeat (cons character character)))

(defcustom electric-pair-text-pairs
  '((?\" . ?\" ))
  "Alist of pairs that should always be used in comments and strings.

Pairs of delimiters in this list are a fallback in case they have
no syntax relevant to `electric-pair-mode' in the syntax table
defined in `electric-pair-text-syntax-table'"
  :version "24.4"
  :type '(repeat (cons character character)))

(defcustom electric-pair-skip-self #'electric-pair-default-skip-self
  "If non-nil, skip char instead of inserting a second closing paren.

When inserting a closing paren character right before the same character,
just skip that character instead, so that hitting ( followed by ) results
in \"()\" rather than \"())\".

This can be convenient for people who find it easier to hit ) than C-f.

Can also be a function of one argument (the closer char just
inserted), in which case that function's return value is
considered instead."
  :version "24.1"
  :type '(choice
          (const :tag "Never skip" nil)
          (const :tag "Help balance" electric-pair-default-skip-self)
          (const :tag "Always skip" t)
          function))

(defcustom electric-pair-inhibit-predicate
  #'electric-pair-default-inhibit
  "Predicate to prevent insertion of a matching pair.

The function is called with a single char (the opening char just inserted).
If it returns non-nil, then `electric-pair-mode' will not insert a matching
closer."
  :version "24.4"
  :type '(choice
          (const :tag "Conservative" electric-pair-conservative-inhibit)
          (const :tag "Help balance" electric-pair-default-inhibit)
          (const :tag "Always pair" ignore)
          function))

(defcustom electric-pair-preserve-balance t
  "Non-nil if default pairing and skipping should help balance parentheses.

The default values of `electric-pair-inhibit-predicate' and
`electric-pair-skip-self' check this variable before delegating to other
predicates reponsible for making decisions on whether to pair/skip some
characters based on the actual state of the buffer's parenthesis and
quotes."
  :version "24.4"
  :type 'boolean)

(defcustom electric-pair-delete-adjacent-pairs t
  "If non-nil, backspacing an open paren also deletes adjacent closer.

Can also be a function of no arguments, in which case that function's
return value is considered instead."
  :version "24.4"
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          function))

(defcustom electric-pair-open-newline-between-pairs t
  "If non-nil, a newline between adjacent parentheses opens an extra one.

Can also be a function of no arguments, in which case that function's
return value is considered instead."
  :version "24.4"
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          function))

(defcustom electric-pair-skip-whitespace t
  "If non-nil skip whitespace when skipping over closing parens.

The specific kind of whitespace skipped is given by the variable
`electric-pair-skip-whitespace-chars'.

The symbol `chomp' specifies that the skipped-over whitespace
should be deleted.

Can also be a function of no arguments, in which case that function's
return value is considered instead."
  :version "24.4"
  :type '(choice
          (const :tag "Yes, jump over whitespace" t)
          (const :tag "Yes, and delete whitespace" 'chomp)
          (const :tag "No, no whitespace skipping" nil)
          function))

(defcustom electric-pair-skip-whitespace-chars (list ?\t ?\s ?\n)
  "Whitespace characters considered by `electric-pair-skip-whitespace'."
  :version "24.4"
  :type '(choice (set (const :tag "Space" ?\s)
                      (const :tag "Tab" ?\t)
                      (const :tag "Newline" ?\n))
                 (list character)))

(defun electric-pair--skip-whitespace ()
  "Skip whitespace forward, not crossing comment or string boundaries."
  (let ((saved (point))
        (string-or-comment (nth 8 (syntax-ppss))))
    (skip-chars-forward (apply #'string electric-pair-skip-whitespace-chars))
    (unless (eq string-or-comment (nth 8 (syntax-ppss)))
      (goto-char saved))))

(defvar electric-pair-text-syntax-table prog-mode-syntax-table
  "Syntax table used when pairing inside comments and strings.

`electric-pair-mode' considers this syntax table only when point in inside
quotes or comments. If lookup fails here, `electric-pair-text-pairs' will
be considered.")

(defun electric-pair-backward-delete-char (n &optional killflag untabify)
  "Delete characters backward, and maybe also two adjacent paired delimiters.

Remaining behaviour is given by `backward-delete-char' or, if UNTABIFY is
non-nil, `backward-delete-char-untabify'."
  (interactive "*p\nP")
  (let* ((prev (char-before))
         (next (char-after))
         (syntax-info (electric-pair-syntax-info prev))
         (syntax (car syntax-info))
         (pair (cadr syntax-info)))
    (when (and (if (functionp electric-pair-delete-adjacent-pairs)
                   (funcall electric-pair-delete-adjacent-pairs)
                 electric-pair-delete-adjacent-pairs)
               next
               (memq syntax '(?\( ?\" ?\$))
               (eq pair next))
      (delete-char 1 killflag))
    (if untabify
        (backward-delete-char-untabify n killflag)
        (backward-delete-char n killflag))))

(defun electric-pair-backward-delete-char-untabify (n &optional killflag)
  "Delete characters backward, and maybe also two adjacent paired delimiters.

Remaining behaviour is given by `backward-delete-char-untabify'."
  (interactive "*p\nP")
  (electric-pair-backward-delete-char n killflag t))

(defun electric-pair-conservative-inhibit (char)
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the second of "" or of ((.
   (and (eq char (char-before))
	(eq char (char-before (1- (point)))))
   ;; I also find it often preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)))

(defun electric-pair-syntax-info (command-event)
  "Calculate a list (SYNTAX PAIR UNCONDITIONAL STRING-OR-COMMENT-START).

SYNTAX is COMMAND-EVENT's syntax character.  PAIR is
COMMAND-EVENT's pair.  UNCONDITIONAL indicates the variables
`electric-pair-pairs' or `electric-pair-text-pairs' were used to
lookup syntax.  STRING-OR-COMMENT-START indicates that point is
inside a comment of string."
  (let* ((pre-string-or-comment (nth 8 (save-excursion
                                         (syntax-ppss (1- (point))))))
         (post-string-or-comment (nth 8 (syntax-ppss (point))))
         (string-or-comment (and post-string-or-comment
                                 pre-string-or-comment))
         (table (if string-or-comment
                    electric-pair-text-syntax-table
                  (syntax-table)))
         (table-syntax-and-pair (with-syntax-table table
                                  (list (char-syntax command-event)
                                        (or (matching-paren command-event)
                                            command-event))))
         (fallback (if string-or-comment
                       (append electric-pair-text-pairs
                               electric-pair-pairs)
                     electric-pair-pairs))
         (direct (assq command-event fallback))
         (reverse (rassq command-event fallback)))
    (cond
     ((memq (car table-syntax-and-pair)
            '(?\" ?\( ?\) ?\$))
      (append table-syntax-and-pair (list nil string-or-comment)))
     (direct (if (eq (car direct) (cdr direct))
                 (list ?\" command-event t string-or-comment)
               (list ?\( (cdr direct) t string-or-comment)))
     (reverse (list ?\) (car reverse) t string-or-comment)))))

(defun electric-pair--insert (char)
  (let ((last-command-event char)
	(blink-matching-paren nil)
	(electric-pair-mode nil))
    (self-insert-command 1)))

(defun electric-pair--syntax-ppss (&optional pos where)
  "Like `syntax-ppss', but sometimes fallback to `parse-partial-sexp'.

WHERE is list defaulting to '(string comment) and indicates
when to fallback to `parse-partial-sexp'."
  (let* ((pos (or pos (point)))
         (where (or where '(string comment)))
         (quick-ppss (syntax-ppss))
         (quick-ppss-at-pos (syntax-ppss pos)))
    (if (or (and (nth 3 quick-ppss) (memq 'string where))
            (and (nth 4 quick-ppss) (memq 'comment where)))
        (with-syntax-table electric-pair-text-syntax-table
          (parse-partial-sexp (1+ (nth 8 quick-ppss)) pos))
      ;; HACK! cc-mode apparently has some `syntax-ppss' bugs
      (if (memq major-mode '(c-mode c++ mode))
          (parse-partial-sexp (point-min) pos)
        quick-ppss-at-pos))))

;; Balancing means controlling pairing and skipping of parentheses so
;; that, if possible, the buffer ends up at least as balanced as
;; before, if not more. The algorithm is slightly complex because some
;; situations like "()))" need pairing to occur at the end but not at
;; the beginning. Balancing should also happen independently for
;; different types of parentheses, so that having your {}'s unbalanced
;; doesn't keep `electric-pair-mode' from balancing your ()'s and your
;; []'s.
(defun electric-pair--balance-info (direction string-or-comment)
  "Examine lists forward or backward according to DIRECTIONS's sign.

STRING-OR-COMMENT is info suitable for running `parse-partial-sexp'.

Return a cons of two descritions (MATCHED-P . PAIR) for the
innermost and outermost lists that enclose point. The outermost
list enclosing point is either the first top-level or first
mismatched list found by uplisting.

If the outermost list is matched, don't rely on its PAIR. If
point is not enclosed by any lists, return ((T) (T))."
  (let* (innermost
         outermost
         (table (if string-or-comment
                    electric-pair-text-syntax-table
                  (syntax-table)))
         (at-top-level-or-equivalent-fn
          ;; called when `scan-sexps' ran perfectly, when when it
          ;; found a parenthesis pointing in the direction of
          ;; travel. Also when travel started inside a comment and
          ;; exited it
          #'(lambda ()
              (setq outermost (list t))
              (unless innermost
                (setq innermost (list t)))))
         (ended-prematurely-fn
          ;; called when `scan-sexps' crashed against a parenthesis
          ;; pointing opposite the direction of travel. After
          ;; traversing that character, the idea is to travel one sexp
          ;; in the opposite direction looking for a matching
          ;; delimiter.
          #'(lambda ()
              (let* ((pos (point))
                     (matched
                      (save-excursion
                        (cond ((< direction 0)
                               (condition-case nil
                                   (eq (char-after pos)
                                       (with-syntax-table table
                                         (matching-paren
                                          (char-before
                                           (scan-sexps (point) 1)))))
                                 (scan-error nil)))
                              (t
                               ;; In this case, no need to use
                               ;; `scan-sexps', we can use some
                               ;; `electric-pair--syntax-ppss' in this
                               ;; case (which uses the quicker
                               ;; `syntax-ppss' in some cases)
                               (let* ((ppss (electric-pair--syntax-ppss
                                             (1- (point))))
                                      (start (car (last (nth 9 ppss))))
                                      (opener (char-after start)))
                                 (and start
                                      (eq (char-before pos)
                                          (or (with-syntax-table table
                                                (matching-paren opener))
                                              opener))))))))
                     (actual-pair (if (> direction 0)
                                      (char-before (point))
                                    (char-after (point)))))
                (unless innermost
                  (setq innermost (cons matched actual-pair)))
                (unless matched
                  (setq outermost (cons matched actual-pair)))))))
    (save-excursion
      (while (not outermost)
        (condition-case err
            (with-syntax-table table
              (scan-sexps (point) (if (> direction 0)
                                      (point-max)
                                    (- (point-max))))
              (funcall at-top-level-or-equivalent-fn))
          (scan-error
           (cond ((or
                   ;; some error happened and it is not of the "ended
                   ;; prematurely" kind"...
                   (not (string-match "ends prematurely" (nth 1 err)))
                   ;; ... or we were in a comment and just came out of
                   ;; it.
                   (and string-or-comment
                        (not (nth 8 (syntax-ppss)))))
                  (funcall at-top-level-or-equivalent-fn))
                 (t
                  ;; exit the sexp
                  (goto-char (nth 3 err))
                  (funcall ended-prematurely-fn)))))))
    (cons innermost outermost)))

(defun electric-pair--looking-at-unterminated-string-p (char)
  "Say if following string starts with CHAR and is unterminated."
  ;; FIXME: ugly/naive
  (save-excursion
    (skip-chars-forward (format "^%c" char))
    (while (not (zerop (% (save-excursion (skip-syntax-backward "\\")) 2)))
      (unless (eobp)
        (forward-char 1)
        (skip-chars-forward (format "^%c" char))))
    (and (not (eobp))
         (condition-case err
             (progn (forward-sexp) nil)
           (scan-error t)))))

(defun electric-pair--inside-string-p (char)
  "Say if point is inside a string started by CHAR.

A comments text is parsed with `electric-pair-text-syntax-table'.
Also consider strings within comments, but not strings within
strings."
  ;; FIXME: could also consider strings within strings by examining
  ;; delimiters.
  (let* ((ppss (electric-pair--syntax-ppss (point) '(comment))))
    (memq (nth 3 ppss) (list t char))))

(defun electric-pair-inhibit-if-helps-balance (char)
  "Return non-nil if auto-pairing of CHAR would hurt parentheses' balance.

Works by first removing the character from the buffer, then doing
some list calculations, finally restoring the situation as if nothing
happened."
  (pcase (electric-pair-syntax-info char)
    (`(,syntax ,pair ,_ ,s-or-c)
     (unwind-protect
         (progn
           (delete-char -1)
           (cond ((eq ?\( syntax)
                  (let* ((pair-data
                          (electric-pair--balance-info 1 s-or-c))
                         (innermost (car pair-data))
                         (outermost (cdr pair-data)))
                    (cond ((car outermost)
                           nil)
                          (t
                           (eq (cdr outermost) pair)))))
                 ((eq syntax ?\")
                  (electric-pair--looking-at-unterminated-string-p char))))
       (insert-char char)))))

(defun electric-pair-skip-if-helps-balance (char)
  "Return non-nil if skipping CHAR would benefit parentheses' balance.

Works by first removing the character from the buffer, then doing
some list calculations, finally restoring the situation as if nothing
happened."
  (pcase (electric-pair-syntax-info char)
    (`(,syntax ,pair ,_ ,s-or-c)
     (unwind-protect
         (progn
           (delete-char -1)
           (cond ((eq syntax ?\))
                  (let* ((pair-data
                          (electric-pair--balance-info
                           -1 s-or-c))
                         (innermost (car pair-data))
                         (outermost (cdr pair-data)))
                    (and
                     (cond ((car outermost)
                            (car innermost))
                           ((car innermost)
                            (not (eq (cdr outermost) pair)))))))
                 ((eq syntax ?\")
                  (electric-pair--inside-string-p char))))
       (insert-char char)))))

(defun electric-pair-default-skip-self (char)
  (if electric-pair-preserve-balance
      (electric-pair-skip-if-helps-balance char)
    t))

(defun electric-pair-default-inhibit (char)
  (if electric-pair-preserve-balance
      (electric-pair-inhibit-if-helps-balance char)
    (electric-pair-conservative-inhibit char)))

(defun electric-pair-post-self-insert-function ()
  (let* ((pos (and electric-pair-mode (electric--after-char-pos)))
         (skip-whitespace-info))
    (pcase (electric-pair-syntax-info last-command-event)
      (`(,syntax ,pair ,unconditional ,_)
       (cond
        ((null pos) nil)
        ;; Wrap a pair around the active region.
        ;;
        ((and (memq syntax '(?\( ?\) ?\" ?\$)) (use-region-p))
         ;; FIXME: To do this right, we'd need a post-self-insert-function
         ;; so we could add-function around it and insert the closer after
         ;; all the rest of the hook has run.
         (if (or (eq syntax ?\")
                 (and (eq syntax ?\))
                      (>= (point) (mark)))
                 (and (not (eq syntax ?\)))
                      (>= (mark) (point))))
             (save-excursion
               (goto-char (mark))
               (electric-pair--insert pair))
           (delete-region pos (1- pos))
           (electric-pair--insert pair)
           (goto-char (mark))
           (electric-pair--insert last-command-event)))
        ;; Backslash-escaped: no pairing, no skipping.
        ((save-excursion
           (goto-char (1- pos))
           (not (zerop (% (skip-syntax-backward "\\") 2))))
         nil)
        ;; Skip self.
        ((and (memq syntax '(?\) ?\" ?\$))
              (and (or unconditional
                       (if (functionp electric-pair-skip-self)
                           (funcall electric-pair-skip-self last-command-event)
                         electric-pair-skip-self))
                   (save-excursion
                     (when (setq skip-whitespace-info
                                 (if (functionp electric-pair-skip-whitespace)
                                     (funcall electric-pair-skip-whitespace)
                                   electric-pair-skip-whitespace))
                       (electric-pair--skip-whitespace))
                     (eq (char-after) last-command-event))))
         ;; This is too late: rather than insert&delete we'd want to only
         ;; skip (or insert in overwrite mode).  The difference is in what
         ;; goes in the undo-log and in the intermediate state which might
         ;; be visible to other post-self-insert-hook.  We'll just have to
         ;; live with it for now.
         (when skip-whitespace-info
           (electric-pair--skip-whitespace))
         (delete-region (1- pos) (if (eq skip-whitespace-info 'chomp)
                                     (point)
                                   pos))
         (forward-char))
        ;; Insert matching pair.
        ((and (memq syntax `(?\( ?\" ?\$))
              (not overwrite-mode)
              (or unconditional
                  (not (funcall electric-pair-inhibit-predicate
                                last-command-event))))
         (save-excursion (electric-pair--insert pair)))))
      (t
       (when (and (if (functionp electric-pair-open-newline-between-pairs)
                      (funcall electric-pair-open-newline-between-pairs)
                    electric-pair-open-newline-between-pairs)
                  (eq last-command-event ?\n)
                  (not (eobp))
                  (eq (save-excursion
                        (skip-chars-backward "\t\s")
                        (char-before (1- (point))))
                      (matching-paren (char-after))))
         (save-excursion (newline 1 t)))))))

(put 'electric-pair-post-self-insert-function   'priority  20)

(defun electric-pair-will-use-region ()
  (and (use-region-p)
       (memq (car (electric-pair-syntax-info last-command-event))
             '(?\( ?\) ?\" ?\$))))

(defvar electric-pair-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap backward-delete-char-untabify]
      'electric-pair-backward-delete-char-untabify)
    (define-key map [remap backward-delete-char]
      'electric-pair-backward-delete-char)
    (define-key map [remap delete-backward-char]
      'electric-pair-backward-delete-char)
    map)
  "Keymap used by `electric-pair-mode'.")

;;;###autoload
(define-minor-mode electric-pair-mode
  "Toggle automatic parens pairing (Electric Pair mode).
With a prefix argument ARG, enable Electric Pair mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Electric Pair mode is a global minor mode.  When enabled, typing
an open parenthesis automatically inserts the corresponding
closing parenthesis.  \(Likewise for brackets, etc.)."
  :global t :group 'electricity
  (if electric-pair-mode
      (progn
	(add-hook 'post-self-insert-hook
		  #'electric-pair-post-self-insert-function)
        (electric--sort-post-self-insertion-hook)
	(add-hook 'self-insert-uses-region-functions
		  #'electric-pair-will-use-region))
    (remove-hook 'post-self-insert-hook
                 #'electric-pair-post-self-insert-function)
    (remove-hook 'self-insert-uses-region-functions
                 #'electric-pair-will-use-region)))

;;; Electric newlines after/before/around some chars.

(defvar electric-layout-rules nil
  "List of rules saying where to automatically insert newlines.

Each rule has the form (CHAR . WHERE) where CHAR is the char that
was just inserted and WHERE specifies where to insert newlines
and can be: nil, `before', `after', `around', `after-stay', or a
function of no arguments that returns one of those symbols.

The symbols specify where in relation to CHAR the newline
character(s) should be inserted. `after-stay' means insert a
newline after CHAR but stay in the same place.")

(defun electric-layout-post-self-insert-function ()
  (let* ((rule (cdr (assq last-command-event electric-layout-rules)))
         pos)
    (when (and rule
               (setq pos (electric--after-char-pos))
               ;; Not in a string or comment.
               (not (nth 8 (save-excursion (syntax-ppss pos)))))
      (let ((end (copy-marker (point)))
            (sym (if (functionp rule) (funcall rule) rule)))
        (set-marker-insertion-type end (not (eq sym 'after-stay)))
        (goto-char pos)
        (pcase sym
          ;; FIXME: we used `newline' down here which called
          ;; self-insert-command and ran post-self-insert-hook recursively.
          ;; It happened to make electric-indent-mode work automatically with
          ;; electric-layout-mode (at the cost of re-indenting lines
          ;; multiple times), but I'm not sure it's what we want.
          ;;
          ;; FIXME: check eolp before inserting \n?
          (`before (goto-char (1- pos)) (skip-chars-backward " \t")
                   (unless (bolp) (insert "\n")))
          (`after  (insert "\n"))
          (`after-stay (save-excursion
                         (let ((electric-layout-rules nil))
                           (newline 1 t))))
          (`around (save-excursion
                     (goto-char (1- pos)) (skip-chars-backward " \t")
                     (unless (bolp) (insert "\n")))
                   (insert "\n")))      ; FIXME: check eolp before inserting \n?
        (goto-char end)))))

(put 'electric-layout-post-self-insert-function 'priority  40)

;;;###autoload
(define-minor-mode electric-layout-mode
  "Automatically insert newlines around some chars.
With a prefix argument ARG, enable Electric Layout mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The variable `electric-layout-rules' says when and how to insert newlines."
  :global t :group 'electricity
  (cond (electric-layout-mode
         (add-hook 'post-self-insert-hook
                   #'electric-layout-post-self-insert-function)
         (electric--sort-post-self-insertion-hook))
        (t
         (remove-hook 'post-self-insert-hook
                      #'electric-layout-post-self-insert-function))))

(provide 'electric)

;;; electric.el ends here
