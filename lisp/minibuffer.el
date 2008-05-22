;;; minibuffer.el --- Minibuffer completion functions

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; Names with "--" are for functions and variables that are meant to be for
;; internal use only.

;; Functional completion tables have an extended calling conventions:
;; - If completion-all-completions-with-base-size is set, then all-completions
;;   should return the base-size in the last cdr.
;; - The `action' can be (additionally to nil, t, and lambda) of the form
;;   (boundaries . POS) in which case it should return (boundaries START . END).
;;   Any other return value should be ignored (so we ignore values returned
;;   from completion tables that don't know about this new `action' form).
;;   See `completion-boundaries'.

;;; Bugs:

;; - completion-ignored-extensions is ignored by partial-completion because
;;   pcm merges the `all' output to synthesize a `try' output and
;;   read-file-name-internal's `all' output doesn't obey
;;   completion-ignored-extensions.
;; - choose-completion can't automatically figure out the boundaries
;;   corresponding to the displayed completions.  `base-size' gives the left
;;   boundary, but not the righthand one.  So we need to add
;;   completion-extra-size (and also completion-no-auto-exit).

;;; Todo:

;; - add support for ** to pcm.
;; - Make read-file-name-predicate obsolete.
;; - Add vc-file-name-completion-table to read-file-name-internal.
;; - A feature like completing-help.el.
;; - Make the `hide-spaces' arg of all-completions obsolete?

;;; Code:

(eval-when-compile (require 'cl))

(defvar completion-all-completions-with-base-size nil
  "If non-nil, `all-completions' may return the base-size in the last cdr.
The base-size is the length of the prefix that is elided from each
element in the returned list of completions.  See `completion-base-size'.")

;;; Completion table manipulation

;; New completion-table operation.
(defun completion-boundaries (string table pred pos)
  "Return the boundaries of the completions returned by TABLE at POS.
STRING is the string on which completion will be performed.
The result is of the form (START . END) and gives the start and end position
corresponding to the substring of STRING that can be completed by one
of the elements returned by
\(all-completions (substring STRING 0 POS) TABLE PRED).
I.e. START is the same as the `completion-base-size'.
E.g. for simple completion tables, the result is always (0 . (length STRING))
and for file names the result is the substring around POS delimited by
the closest directory separators."
  (let ((boundaries (if (functionp table)
                        (funcall table string pred (cons 'boundaries pos)))))
    (if (not (eq (car-safe boundaries) 'boundaries))
        (setq boundaries nil))
    (cons (or (cadr boundaries) 0)
          (or (cddr boundaries) (length string)))))

(defun completion--some (fun xs)
  "Apply FUN to each element of XS in turn.
Return the first non-nil returned value.
Like CL's `some'."
  (let ((firsterror nil)
        res)
    (while (and (not res) xs)
      (condition-case err
          (setq res (funcall fun (pop xs)))
        (error (unless firsterror (setq firsterror err)) nil)))
    (or res
        (if firsterror (signal (car firsterror) (cdr firsterror))))))

(defun apply-partially (fun &rest args)
  "Do a \"curried\" partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function that takes the remaining arguments,
and calls FUN."
  (lexical-let ((fun fun) (args1 args))
    (lambda (&rest args2) (apply fun (append args1 args2)))))

(defun complete-with-action (action table string pred)
  "Perform completion ACTION.
STRING is the string to complete.
TABLE is the completion table, which should not be a function.
PRED is a completion predicate.
ACTION can be one of nil, t or `lambda'."
  (cond
   ((functionp table) (funcall table string pred action))
   ((eq (car-safe action) 'boundaries)
    (cons 'boundaries (completion-boundaries string table pred (cdr action))))
   (t
    (funcall
     (cond
      ((null action) 'try-completion)
      ((eq action t) 'all-completions)
      (t 'test-completion))
     string table pred))))

(defun completion-table-dynamic (fun)
  "Use function FUN as a dynamic completion table.
FUN is called with one argument, the string for which completion is required,
and it should return an alist containing all the intended possible completions.
This alist may be a full list of possible completions so that FUN can ignore
the value of its argument.  If completion is performed in the minibuffer,
FUN will be called in the buffer from which the minibuffer was entered.

The result of the `dynamic-completion-table' form is a function
that can be used as the ALIST argument to `try-completion' and
`all-completions'.  See Info node `(elisp)Programmed Completion'."
  (lexical-let ((fun fun))
    (lambda (string pred action)
      (with-current-buffer (let ((win (minibuffer-selected-window)))
                             (if (window-live-p win) (window-buffer win)
                               (current-buffer)))
        (complete-with-action action (funcall fun string) string pred)))))

(defmacro lazy-completion-table (var fun)
  "Initialize variable VAR as a lazy completion table.
If the completion table VAR is used for the first time (e.g., by passing VAR
as an argument to `try-completion'), the function FUN is called with no
arguments.  FUN must return the completion table that will be stored in VAR.
If completion is requested in the minibuffer, FUN will be called in the buffer
from which the minibuffer was entered.  The return value of
`lazy-completion-table' must be used to initialize the value of VAR.

You should give VAR a non-nil `risky-local-variable' property."
  (declare (debug (symbolp lambda-expr)))
  (let ((str (make-symbol "string")))
    `(completion-table-dynamic
      (lambda (,str)
        (when (functionp ,var)
          (setq ,var (,fun)))
        ,var))))

(defun completion-table-with-context (prefix table string pred action)
  ;; TODO: add `suffix' maybe?
  ;; Notice that `pred' may not be a function in some abusive cases.
  (when (functionp pred)
    (setq pred
          (lexical-let ((pred pred))
            ;; Predicates are called differently depending on the nature of
            ;; the completion table :-(
            (cond
             ((vectorp table)           ;Obarray.
              (lambda (sym) (funcall pred (concat prefix (symbol-name sym)))))
             ((hash-table-p table)
              (lambda (s v) (funcall pred (concat prefix s))))
             ((functionp table)
              (lambda (s) (funcall pred (concat prefix s))))
             (t                         ;Lists and alists.
              (lambda (s)
                (funcall pred (concat prefix (if (consp s) (car s) s)))))))))
  (if (eq (car-safe action) 'boundaries)
      (let* ((len (length prefix))
             (bound (completion-boundaries string table pred
                                           (- (cdr action) len))))
        (list* 'boundaries (+ (car bound) len) (+ (cdr bound) len)))
    (let ((comp (complete-with-action action table string pred)))
      (cond
       ;; In case of try-completion, add the prefix.
       ((stringp comp) (concat prefix comp))
       ;; In case of non-empty all-completions,
       ;; add the prefix size to the base-size.
       ((consp comp)
        (let ((last (last comp)))
          (when completion-all-completions-with-base-size
            (setcdr last (+ (or (cdr last) 0) (length prefix))))
          comp))
       (t comp)))))

(defun completion-table-with-terminator (terminator table string pred action)
  (cond
   ((eq action nil)
    (let ((comp (try-completion string table pred)))
      (if (eq comp t)
          (concat string terminator)
        (if (and (stringp comp)
                 (eq (try-completion comp table pred) t))
            (concat comp terminator)
          comp))))
   ((eq action t)
    ;; FIXME: We generally want the `try' and `all' behaviors to be
    ;; consistent so pcm can merge the `all' output to get the `try' output,
    ;; but that sometimes clashes with the need for `all' output to look
    ;; good in *Completions*.
    ;; (let* ((all (all-completions string table pred))
    ;;        (last (last all))
    ;;        (base-size (cdr last)))
    ;;   (when all
    ;;     (setcdr all nil)
    ;;     (nconc (mapcar (lambda (s) (concat s terminator)) all) base-size)))
    (all-completions string table pred))
   ;; completion-table-with-terminator is always used for
   ;; "sub-completions" so it's only called if the terminator is missing,
   ;; in which case `test-completion' should return nil.
   ((eq action 'lambda) nil)))

(defun completion-table-with-predicate (table pred1 strict string pred2 action)
  "Make a completion table equivalent to TABLE but filtered through PRED1.
PRED1 is a function of one argument which returns non-nil if and only if the
argument is an element of TABLE which should be considered for completion.
STRING, PRED2, and ACTION are the usual arguments to completion tables,
as described in `try-completion', `all-completions', and `test-completion'.
If STRICT is t, the predicate always applies; if nil it only applies if
it does not reduce the set of possible completions to nothing.
Note: TABLE needs to be a proper completion table which obeys predicates."
  (cond
   ((and (not strict) (eq action 'lambda))
    ;; Ignore pred1 since it doesn't really have to apply anyway.
    (test-completion string table pred2))
   (t
    (or (complete-with-action action table string
                              (if (null pred2) pred1
                                (lexical-let ((pred1 pred2) (pred2 pred2))
                                  (lambda (x)
                                    ;; Call `pred1' first, so that `pred2'
                                    ;; really can't tell that `x' is in table.
                                    (if (funcall pred1 x) (funcall pred2 x))))))
        ;; If completion failed and we're not applying pred1 strictly, try
        ;; again without pred1.
        (and (not strict)
             (complete-with-action action table string pred2))))))

(defun completion-table-in-turn (&rest tables)
  "Create a completion table that tries each table in TABLES in turn."
  (lexical-let ((tables tables))
    (lambda (string pred action)
      (completion--some (lambda (table)
                          (complete-with-action action table string pred))
                        tables))))

;; (defmacro complete-in-turn (a b) `(completion-table-in-turn ,a ,b))
;; (defmacro dynamic-completion-table (fun) `(completion-table-dynamic ,fun))
(define-obsolete-function-alias
  'complete-in-turn 'completion-table-in-turn "23.1")
(define-obsolete-function-alias
  'dynamic-completion-table 'completion-table-dynamic "23.1")

;;; Minibuffer completion

(defgroup minibuffer nil
  "Controlling the behavior of the minibuffer."
  :link '(custom-manual "(emacs)Minibuffer")
  :group 'environment)

(defun minibuffer-message (message &rest args)
  "Temporarily display MESSAGE at the end of the minibuffer.
The text is displayed for `minibuffer-message-timeout' seconds,
or until the next input event arrives, whichever comes first.
Enclose MESSAGE in [...] if this is not yet the case.
If ARGS are provided, then pass MESSAGE through `format'."
  ;; Clear out any old echo-area message to make way for our new thing.
  (message nil)
  (setq message (if (and (null args) (string-match "\\[.+\\]" message))
                    ;; Make sure we can put-text-property.
                    (copy-sequence message)
                  (concat " [" message "]")))
  (when args (setq message (apply 'format message args)))
  (let ((ol (make-overlay (point-max) (point-max) nil t t)))
    (unwind-protect
        (progn
          (unless (zerop (length message))
            ;; The current C cursor code doesn't know to use the overlay's
            ;; marker's stickiness to figure out whether to place the cursor
            ;; before or after the string, so let's spoon-feed it the pos.
            (put-text-property 0 1 'cursor t message))
          (overlay-put ol 'after-string message)
          (sit-for (or minibuffer-message-timeout 1000000)))
      (delete-overlay ol))))

(defun minibuffer-completion-contents ()
  "Return the user input in a minibuffer before point as a string.
That is what completion commands operate on."
  (buffer-substring (field-beginning) (point)))

(defun delete-minibuffer-contents ()
  "Delete all user input in a minibuffer.
If the current buffer is not a minibuffer, erase its entire contents."
  (delete-field))

(defcustom completion-auto-help t
  "Non-nil means automatically provide help for invalid completion input.
If the value is t the *Completion* buffer is displayed whenever completion
is requested but cannot be done.
If the value is `lazy', the *Completions* buffer is only displayed after
the second failed attempt to complete."
  :type '(choice (const nil) (const t) (const lazy))
  :group 'minibuffer)

(defvar completion-styles-alist
  '((basic completion-basic-try-completion completion-basic-all-completions)
    (emacs22 completion-emacs22-try-completion completion-emacs22-all-completions)
    (emacs21 completion-emacs21-try-completion completion-emacs21-all-completions)
    (partial-completion
     completion-pcm-try-completion completion-pcm-all-completions))
  "List of available completion styles.
Each element has the form (NAME TRY-COMPLETION ALL-COMPLETIONS)
where NAME is the name that should be used in `completion-styles',
TRY-COMPLETION is the function that does the completion, and
ALL-COMPLETIONS is the function that lists the completions.")

(defcustom completion-styles '(basic partial-completion)
  "List of completion styles to use."
  :type `(repeat (choice ,@(mapcar (lambda (x) (list 'const (car x)))
                                   completion-styles-alist)))
  :group 'minibuffer
  :version "23.1")

(defun completion-try-completion (string table pred point)
  "Try to complete STRING using completion table TABLE.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value can be either nil to indicate that there is no completion,
t to indicate that STRING is the only possible completion,
or a pair (STRING . NEWPOINT) of the completed result string together with
a new position for point."
  ;; The property `completion-styles' indicates that this functional
  ;; completion-table claims to take care of completion styles itself.
  ;; [I.e. It will most likely call us back at some point. ]
  (if (and (symbolp table) (get table 'completion-styles))
      ;; Extended semantics for functional completion-tables:
      ;; They accept a 4th argument `point' and when called with action=nil
      ;; and this 4th argument (a position inside `string'), they should
      ;; return instead of a string a pair (STRING . NEWPOINT).
      (funcall table string pred nil point)
    (completion--some (lambda (style)
                        (funcall (nth 1 (assq style completion-styles-alist))
                                 string table pred point))
                      completion-styles)))

(defun completion-all-completions (string table pred point)
  "List the possible completions of STRING in completion table TABLE.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value is a list of completions and may contain the base-size
in the last `cdr'."
  (let ((completion-all-completions-with-base-size t))
    ;; The property `completion-styles' indicates that this functional
    ;; completion-table claims to take care of completion styles itself.
    ;; [I.e. It will most likely call us back at some point. ]
    (if (and (symbolp table) (get table 'completion-styles))
        ;; Extended semantics for functional completion-tables:
        ;; They accept a 4th argument `point' and when called with action=t
        ;; and this 4th argument (a position inside `string'), they may
        ;; return BASE-SIZE in the last `cdr'.
        (funcall table string pred t point)
      (completion--some (lambda (style)
                          (funcall (nth 2 (assq style completion-styles-alist))
                                   string table pred point))
                        completion-styles))))

(defun minibuffer--bitset (modified completions exact)
  (logior (if modified    4 0)
          (if completions 2 0)
          (if exact       1 0)))

(defun completion--do-completion (&optional try-completion-function)
  "Do the completion and return a summary of what happened.
M = completion was performed, the text was Modified.
C = there were available Completions.
E = after completion we now have an Exact match.

 MCE
 000  0 no possible completion
 001  1 was already an exact and unique completion
 010  2 no completion happened
 011  3 was already an exact completion
 100  4 ??? impossible
 101  5 ??? impossible
 110  6 some completion happened
 111  7 completed to an exact completion"
  (let* ((beg (field-beginning))
         (end (field-end))
         (string (buffer-substring beg end))
         (comp (funcall (or try-completion-function
			    'completion-try-completion)
			string
			minibuffer-completion-table
			minibuffer-completion-predicate
			(- (point) beg))))
    (cond
     ((null comp)
      (ding) (minibuffer-message "No match") (minibuffer--bitset nil nil nil))
     ((eq t comp) (minibuffer--bitset nil nil t)) ;Exact and unique match.
     (t
      ;; `completed' should be t if some completion was done, which doesn't
      ;; include simply changing the case of the entered string.  However,
      ;; for appearance, the string is rewritten if the case changes.
      (let* ((comp-pos (cdr comp))
	     (completion (car comp))
	     (completed (not (eq t (compare-strings completion nil nil
						    string nil nil t))))
	     (unchanged (eq t (compare-strings completion nil nil
					       string nil nil nil))))
        (unless unchanged

          ;; Insert in minibuffer the chars we got.
          (goto-char end)
          (insert completion)
          (delete-region beg end))
	;; Move point.
	(goto-char (+ beg comp-pos))

        (if (not (or unchanged completed))
	   ;; The case of the string changed, but that's all.  We're not sure
	   ;; whether this is a unique completion or not, so try again using
	   ;; the real case (this shouldn't recurse again, because the next
	   ;; time try-completion will return either t or the exact string).
           (completion--do-completion try-completion-function)

          ;; It did find a match.  Do we match some possibility exactly now?
          (let ((exact (test-completion completion
					minibuffer-completion-table
					minibuffer-completion-predicate)))
            (unless completed
              ;; Show the completion table, if requested.
              (cond
               ((not exact)
                (if (case completion-auto-help
                      (lazy (eq this-command last-command))
                      (t completion-auto-help))
                    (minibuffer-completion-help)
                  (minibuffer-message "Next char not unique")))
               ;; If the last exact completion and this one were the same,
               ;; it means we've already given a "Complete but not unique"
               ;; message and the user's hit TAB again, so now we give him help.
               ((eq this-command last-command)
                (if completion-auto-help (minibuffer-completion-help)))))

            (minibuffer--bitset completed t exact))))))))

(defun minibuffer-complete ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive)
  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (eq this-command last-command)
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
        (with-current-buffer (window-buffer window)
          (if (pos-visible-in-window-p (point-max) window)
	      ;; If end is in view, scroll up to the beginning.
	      (set-window-start window (point-min) nil)
	    ;; Else scroll down one screen.
	    (scroll-other-window))
	  nil)

      (case (completion--do-completion)
        (#b000 nil)
        (#b001 (goto-char (field-end))
               (minibuffer-message "Sole completion")
               t)
        (#b011 (goto-char (field-end))
               (minibuffer-message "Complete, but not unique")
               t)
        (t     t)))))

(defvar completion-all-sorted-completions nil)
(make-variable-buffer-local 'completion-all-sorted-completions)

(defun completion--flush-all-sorted-completions (&rest ignore)
  (setq completion-all-sorted-completions nil))

(defun completion-all-sorted-completions ()
  (or completion-all-sorted-completions
      (let* ((start (field-beginning))
             (end (field-end))
             (all (completion-all-completions (buffer-substring start end)
                                              minibuffer-completion-table
                                              minibuffer-completion-predicate
                                              (- (point) start)))
             (last (last all))
             (base-size (or (cdr last) 0)))
        (when last
          (setcdr last nil)
          ;; Prefer shorter completions.
          (setq all (sort all (lambda (c1 c2) (< (length c1) (length c2)))))
          ;; Prefer recently used completions.
          (let ((hist (symbol-value minibuffer-history-variable)))
            (setq all (sort all (lambda (c1 c2)
                                  (> (length (member c1 hist))
                                     (length (member c2 hist)))))))
          ;; Cache the result.  This is not just for speed, but also so that
          ;; repeated calls to minibuffer-force-complete can cycle through
          ;; all possibilities.
          (add-hook 'after-change-functions
                    'completion--flush-all-sorted-completions nil t)
          (setq completion-all-sorted-completions
                (nconc all base-size))))))

(defun minibuffer-force-complete ()
  "Complete the minibuffer to an exact match.
Repeated uses step through the possible completions."
  (interactive)
  ;; FIXME: Need to deal with the extra-size issue here as well.
  (let* ((start (field-beginning))
         (end (field-end))
         (all (completion-all-sorted-completions)))
    (if (not (consp all))
        (minibuffer-message (if all "No more completions" "No completions"))
      (goto-char end)
      (insert (car all))
      (delete-region (+ start (cdr (last all))) end)
      ;; If completing file names, (car all) may be a directory, so we'd now
      ;; have a new set of possible completions and might want to reset
      ;; completion-all-sorted-completions to nil, but we prefer not to,
      ;; so that repeated calls minibuffer-force-complete still cycle
      ;; through the previous possible completions.
      (setq completion-all-sorted-completions (cdr all)))))

(defun minibuffer-complete-and-exit ()
  "If the minibuffer contents is a valid completion then exit.
Otherwise try to complete it.  If completion leads to a valid completion,
a repetition of this command will exit.
If `minibuffer-completion-confirm' is equal to `confirm', then do not
try to complete, but simply ask for confirmation and accept any
input if confirmed."
  (interactive)
  (let ((beg (field-beginning))
        (end (field-end)))
    (cond
     ;; Allow user to specify null string
     ((= beg end) (exit-minibuffer))
     ((test-completion (buffer-substring beg end)
                       minibuffer-completion-table
                       minibuffer-completion-predicate)
      (when completion-ignore-case
        ;; Fixup case of the field, if necessary.
        (let* ((string (buffer-substring beg end))
               (compl (try-completion
                       string
                       minibuffer-completion-table
                       minibuffer-completion-predicate)))
          (when (and (stringp compl)
                     ;; If it weren't for this piece of paranoia, I'd replace
                     ;; the whole thing with a call to do-completion.
                     (= (length string) (length compl)))
            (goto-char end)
            (insert compl)
            (delete-region beg end))))
      (exit-minibuffer))

     ((eq minibuffer-completion-confirm 'confirm-only)
      ;; The user is permitted to exit with an input that's rejected
      ;; by test-completion, but at the condition to confirm her choice.
      (if (eq last-command this-command)
          (exit-minibuffer)
        (minibuffer-message "Confirm")
        nil))

     (t
      ;; Call do-completion, but ignore errors.
      (case (condition-case nil
                (completion--do-completion)
              (error 1))
        ((#b001 #b011) (exit-minibuffer))
        (#b111 (if (not minibuffer-completion-confirm)
                   (exit-minibuffer)
                 (minibuffer-message "Confirm")
                 nil))
        (t nil))))))

(defun completion--try-word-completion (string table predicate point)
  (let ((comp (completion-try-completion string table predicate point)))
    (if (not (consp comp))
        comp

      ;; If completion finds next char not unique,
      ;; consider adding a space or a hyphen.
      (when (= (length string) (length (car comp)))
        (let ((exts '(" " "-"))
              (before (substring string 0 point))
              (after (substring string point))
              ;; If the user hasn't entered any text yet, then she
              ;; presumably hits SPC to see the *completions*, but
              ;; partial-completion will often find a " " or a "-" to match.
              ;; So disable partial-completion in that situation.
              (completion-styles
               (or (and (equal string "")
                        (remove 'partial-completion completion-styles))
                   completion-styles))
	      tem)
	  (while (and exts (not (consp tem)))
            (setq tem (completion-try-completion
		       (concat before (pop exts) after)
		       table predicate (1+ point))))
	  (if (consp tem) (setq comp tem))))

      ;; Completing a single word is actually more difficult than completing
      ;; as much as possible, because we first have to find the "current
      ;; position" in `completion' in order to find the end of the word
      ;; we're completing.  Normally, `string' is a prefix of `completion',
      ;; which makes it trivial to find the position, but with fancier
      ;; completion (plus env-var expansion, ...) `completion' might not
      ;; look anything like `string' at all.
      (let* ((comppoint (cdr comp))
	     (completion (car comp))
	     (before (substring string 0 point))
	     (combined (concat before "\n" completion)))
        ;; Find in completion the longest text that was right before point.
        (when (string-match "\\(.+\\)\n.*?\\1" combined)
          (let* ((prefix (match-string 1 before))
                 ;; We used non-greedy match to make `rem' as long as possible.
                 (rem (substring combined (match-end 0)))
                 ;; Find in the remainder of completion the longest text
                 ;; that was right after point.
                 (after (substring string point))
                 (suffix (if (string-match "\\`\\(.+\\).*\n.*\\1"
                                           (concat after "\n" rem))
                             (match-string 1 after))))
            ;; The general idea is to try and guess what text was inserted
            ;; at point by the completion.  Problem is: if we guess wrong,
            ;; we may end up treating as "added by completion" text that was
            ;; actually painfully typed by the user.  So if we then cut
            ;; after the first word, we may throw away things the
            ;; user wrote.  So let's try to be as conservative as possible:
            ;; only cut after the first word, if we're reasonably sure that
            ;; our guess is correct.
            ;; Note: a quick survey on emacs-devel seemed to indicate that
            ;; nobody actually cares about the "word-at-a-time" feature of
            ;; minibuffer-complete-word, whose real raison-d'être is that it
            ;; tries to add "-" or " ".  One more reason to only cut after
            ;; the first word, if we're really sure we're right.
            (when (and (or suffix (zerop (length after)))
                       (string-match (concat
                                      ;; Make submatch 1 as small as possible
                                      ;; to reduce the risk of cutting
                                      ;; valuable text.
                                      ".*" (regexp-quote prefix) "\\(.*?\\)"
                                      (if suffix (regexp-quote suffix) "\\'"))
                                     completion)
                       ;; The new point in `completion' should also be just
                       ;; before the suffix, otherwise something more complex
                       ;; is going on, and we're not sure where we are.
                       (eq (match-end 1) comppoint)
                       ;; (match-beginning 1)..comppoint is now the stretch
                       ;; of text in `completion' that was completed at point.
		       (string-match "\\W" completion (match-beginning 1))
		       ;; Is there really something to cut?
		       (> comppoint (match-end 0)))
              ;; Cut after the first word.
              (let ((cutpos (match-end 0)))
                (setq completion (concat (substring completion 0 cutpos)
                                         (substring completion comppoint)))
                (setq comppoint cutpos)))))

	(cons completion comppoint)))))


(defun minibuffer-complete-word ()
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t."
  (interactive)
  (case (completion--do-completion 'completion--try-word-completion)
    (#b000 nil)
    (#b001 (goto-char (field-end))
           (minibuffer-message "Sole completion")
           t)
    (#b011 (goto-char (field-end))
           (minibuffer-message "Complete, but not unique")
           t)
    (t     t)))

(defun completion--insert-strings (strings)
  "Insert a list of STRINGS into the current buffer.
Uses columns to keep the listing readable but compact.
It also eliminates runs of equal strings."
  (when (consp strings)
    (let* ((length (apply 'max
			  (mapcar (lambda (s)
				    (if (consp s)
					(+ (string-width (car s))
                                           (string-width (cadr s)))
				      (string-width s)))
				  strings)))
	   (window (get-buffer-window (current-buffer) 0))
	   (wwidth (if window (1- (window-width window)) 79))
	   (columns (min
		     ;; At least 2 columns; at least 2 spaces between columns.
		     (max 2 (/ wwidth (+ 2 length)))
		     ;; Don't allocate more columns than we can fill.
		     ;; Windows can't show less than 3 lines anyway.
		     (max 1 (/ (length strings) 2))))
	   (colwidth (/ wwidth columns))
           (column 0)
	   (laststring nil))
      ;; The insertion should be "sensible" no matter what choices were made
      ;; for the parameters above.
      (dolist (str strings)
	(unless (equal laststring str)  ; Remove (consecutive) duplicates.
	  (setq laststring str)
	  (unless (bolp)
            (insert " \t")
            (setq column (+ column colwidth))
            ;; Leave the space unpropertized so that in the case we're
            ;; already past the goal column, there is still
            ;; a space displayed.
            (set-text-properties (- (point) 1) (point)
                                 ;; We can't just set tab-width, because
                                 ;; completion-setup-function will kill all
                                 ;; local variables :-(
                                 `(display (space :align-to ,column)))
	    (when (< wwidth (+ (max colwidth
				    (if (consp str)
					(+ (string-width (car str))
					   (string-width (cadr str)))
				      (string-width str)))
			       column))
	      (delete-char -2) (insert "\n") (setq column 0)))
	  (if (not (consp str))
	      (put-text-property (point) (progn (insert str) (point))
				 'mouse-face 'highlight)
	    (put-text-property (point) (progn (insert (car str)) (point))
			       'mouse-face 'highlight)
	    (put-text-property (point) (progn (insert (cadr str)) (point))
                               'mouse-face nil)))))))

(defvar completion-common-substring nil)
(make-obsolete-variable 'completion-common-substring nil "23.1")

(defvar completion-setup-hook nil
  "Normal hook run at the end of setting up a completion list buffer.
When this hook is run, the current buffer is the one in which the
command to display the completion list buffer was run.
The completion list buffer is available as the value of `standard-output'.
See also `display-completion-list'.")

(defface completions-first-difference
  '((t (:inherit bold)))
  "Face put on the first uncommon character in completions in *Completions* buffer."
  :group 'completion)

(defface completions-common-part
  '((t (:inherit default)))
  "Face put on the common prefix substring in completions in *Completions* buffer.
The idea of `completions-common-part' is that you can use it to
make the common parts less visible than normal, so that the rest
of the differing parts is, by contrast, slightly highlighted."
  :group 'completion)

(defun completion-hilit-commonality (completions prefix-len)
  (when completions
    (let* ((last (last completions))
           (base-size (cdr last))
           (com-str-len (- prefix-len (or base-size 0))))
      ;; Remove base-size during mapcar, and add it back later.
      (setcdr last nil)
      (nconc
       (mapcar
        (lambda (elem)
          (let ((str
                 ;; Don't modify the string itself, but a copy, since the
                 ;; the string may be read-only or used for other purposes.
                 ;; Furthermore, since `completions' may come from
                 ;; display-completion-list, `elem' may be a list.
                 (if (consp elem)
                     (car (setq elem (cons (copy-sequence (car elem))
                                           (cdr elem))))
                   (setq elem (copy-sequence elem)))))
            (put-text-property 0 com-str-len
                               'font-lock-face 'completions-common-part
                               str)
            (if (> (length str) com-str-len)
                (put-text-property com-str-len (1+ com-str-len)
                                   'font-lock-face 'completions-first-difference
                                   str)))
          elem)
        completions)
       base-size))))

(defun display-completion-list (completions &optional common-substring)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string
or may be a list of two strings to be printed as if concatenated.
If it is a list of two strings, the first is the actual completion
alternative, the second serves as annotation.
`standard-output' must be a buffer.
The actual completion alternatives, as inserted, are given `mouse-face'
properties of `highlight'.
At the end, this runs the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.
The obsolete optional second arg COMMON-SUBSTRING is a string.
It is used to put faces, `completions-first-difference' and
`completions-common-part' on the completion buffer.  The
`completions-common-part' face is put on the common substring
specified by COMMON-SUBSTRING."
  (if common-substring
      (setq completions (completion-hilit-commonality
                         completions (length common-substring))))
  (if (not (bufferp standard-output))
      ;; This *never* (ever) happens, so there's no point trying to be clever.
      (with-temp-buffer
	(let ((standard-output (current-buffer))
	      (completion-setup-hook nil))
	  (display-completion-list completions))
	(princ (buffer-string)))

    (with-current-buffer standard-output
      (goto-char (point-max))
      (if (null completions)
	  (insert "There are no possible completions of what you have typed.")

	(insert "Possible completions are:\n")
        (let ((last (last completions)))
          ;; Get the base-size from the tail of the list.
          (set (make-local-variable 'completion-base-size) (or (cdr last) 0))
          (setcdr last nil)) ;Make completions a properly nil-terminated list.
	(completion--insert-strings completions))))

  ;; The hilit used to be applied via completion-setup-hook, so there
  ;; may still be some code that uses completion-common-substring.
  (let ((completion-common-substring common-substring))
    (run-hooks 'completion-setup-hook))
  nil)

(defun minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (message "Making completion list...")
  (let* ((string (field-string))
         (completions (completion-all-completions
                       string
                       minibuffer-completion-table
                       minibuffer-completion-predicate
                       (- (point) (field-beginning)))))
    (message nil)
    (if (and completions
             (or (consp (cdr completions))
                 (not (equal (car completions) string))))
        (with-output-to-temp-buffer "*Completions*"
          (let* ((last (last completions))
                 (base-size (cdr last)))
            ;; Remove the base-size tail because `sort' requires a properly
            ;; nil-terminated list.
            (when last (setcdr last nil))
            (display-completion-list (nconc (sort completions 'string-lessp)
                                            base-size))))

      ;; If there are no completions, or if the current input is already the
      ;; only possible completion, then hide (previous&stale) completions.
      (let ((window (and (get-buffer "*Completions*")
                         (get-buffer-window "*Completions*" 0))))
        (when (and (window-live-p window) (window-dedicated-p window))
          (condition-case ()
              (delete-window window)
            (error (iconify-frame (window-frame window))))))
      (ding)
      (minibuffer-message
       (if completions "Sole completion" "No completions")))
    nil))

(defun exit-minibuffer ()
  "Terminate this minibuffer argument."
  (interactive)
  ;; If the command that uses this has made modifications in the minibuffer,
  ;; we don't want them to cause deactivation of the mark in the original
  ;; buffer.
  ;; A better solution would be to make deactivate-mark buffer-local
  ;; (or to turn it into a list of buffers, ...), but in the mean time,
  ;; this should do the trick in most cases.
  (setq deactivate-mark nil)
  (throw 'exit nil))

(defun self-insert-and-exit ()
  "Terminate minibuffer input."
  (interactive)
  (if (characterp last-command-char)
      (call-interactively 'self-insert-command)
    (ding))
  (exit-minibuffer))

;;; Key bindings.

(let ((map minibuffer-local-map))
  (define-key map "\C-g" 'abort-recursive-edit)
  (define-key map "\r" 'exit-minibuffer)
  (define-key map "\n" 'exit-minibuffer))

(let ((map minibuffer-local-completion-map))
  (define-key map "\t" 'minibuffer-complete)
  ;; M-TAB is already abused for many other purposes, so we should find
  ;; another binding for it.
  ;; (define-key map "\e\t" 'minibuffer-force-complete)
  (define-key map " " 'minibuffer-complete-word)
  (define-key map "?" 'minibuffer-completion-help))

(let ((map minibuffer-local-must-match-map))
  (define-key map "\r" 'minibuffer-complete-and-exit)
  (define-key map "\n" 'minibuffer-complete-and-exit))

(let ((map minibuffer-local-filename-completion-map))
  (define-key map " " nil))
(let ((map minibuffer-local-must-match-filename-map))
  (define-key map " " nil))

(let ((map minibuffer-local-ns-map))
  (define-key map " " 'exit-minibuffer)
  (define-key map "\t" 'exit-minibuffer)
  (define-key map "?" 'self-insert-and-exit))

;;; Completion tables.

(defun minibuffer--double-dollars (str)
  (replace-regexp-in-string "\\$" "$$" str))

(defun completion--make-envvar-table ()
  (mapcar (lambda (enventry)
            (substring enventry 0 (string-match "=" enventry)))
          process-environment))

(defconst completion--embedded-envvar-re
  (concat "\\(?:^\\|[^$]\\(?:\\$\\$\\)*\\)"
          "$\\([[:alnum:]_]*\\|{\\([^}]*\\)\\)\\'"))

(defun completion--embedded-envvar-table (string pred action)
  (if (eq (car-safe action) 'boundaries)
      ;; Compute the boundaries of the subfield to which this
      ;; completion applies.
      (let* ((pos (cdr action))
             (suffix (substring string pos)))
        (if (string-match completion--embedded-envvar-re
                          (substring string 0 pos))
            (list* 'boundaries (or (match-beginning 2) (match-beginning 1))
                   (when (string-match "[^[:alnum:]_]" suffix)
                     (+ pos (match-beginning 0))))))
    (when (string-match completion--embedded-envvar-re string)
      (let* ((beg (or (match-beginning 2) (match-beginning 1)))
             (table (completion--make-envvar-table))
             (prefix (substring string 0 beg)))
        (if (eq (aref string (1- beg)) ?{)
            (setq table (apply-partially 'completion-table-with-terminator
                                         "}" table)))
        (completion-table-with-context
         prefix table (substring string beg) pred action)))))

(defun completion--file-name-table (string pred action)
  "Internal subroutine for `read-file-name'.  Do not call this."
  (cond
   ((and (zerop (length string)) (eq 'lambda action))
    nil)                                ; FIXME: why?
   ((eq (car-safe action) 'boundaries)
    ;; FIXME: Actually, this is not always right in the presence of
    ;; envvars, but there's not much we can do, I think.
    (let ((start (length (file-name-directory
                          (substring string 0 (cdr action)))))
          (end (string-match "/" string (cdr action))))
      (list* 'boundaries start end)))
    
   (t
    (let* ((dir (if (stringp pred)
                    ;; It used to be that `pred' was abused to pass `dir'
                    ;; as an argument.
                    (prog1 (expand-file-name pred) (setq pred nil))
                  default-directory))
           (str (condition-case nil
                    (substitute-in-file-name string)
                  (error string)))
           (name (file-name-nondirectory str))
           (specdir (file-name-directory str))
           (realdir (if specdir (expand-file-name specdir dir)
                      (file-name-as-directory dir))))

      (cond
       ((null action)
        (let ((comp (file-name-completion name realdir
                                          read-file-name-predicate)))
          (if (stringp comp)
              ;; Requote the $s before returning the completion.
              (minibuffer--double-dollars (concat specdir comp))
            ;; Requote the $s before checking for changes.
            (setq str (minibuffer--double-dollars str))
            (if (string-equal string str)
                comp
              ;; If there's no real completion, but substitute-in-file-name
              ;; changed the string, then return the new string.
              str))))

       ((eq action t)
        (let ((all (file-name-all-completions name realdir))
              ;; FIXME: Actually, this is not always right in the presence
              ;; of envvars, but there's not much we can do, I think.
              (base-size (length (file-name-directory string))))

          ;; Check the predicate, if necessary.
          (unless (memq read-file-name-predicate '(nil file-exists-p))
            (let ((comp ())
                  (pred
                   (if (eq read-file-name-predicate 'file-directory-p)
                       ;; Brute-force speed up for directory checking:
                       ;; Discard strings which don't end in a slash.
                       (lambda (s)
                         (let ((len (length s)))
                           (and (> len 0) (eq (aref s (1- len)) ?/))))
                     ;; Must do it the hard (and slow) way.
                     read-file-name-predicate)))
              (let ((default-directory realdir))
                (dolist (tem all)
                  (if (funcall pred tem) (push tem comp))))
              (setq all (nreverse comp))))

          (if (and completion-all-completions-with-base-size (consp all))
              ;; Add base-size, but only if the list is non-empty.
              (nconc all base-size)
            all)))

       (t
        ;; Only other case actually used is ACTION = lambda.
        (let ((default-directory dir))
          (funcall (or read-file-name-predicate 'file-exists-p) str))))))))

(defalias 'read-file-name-internal
  (completion-table-in-turn 'completion--embedded-envvar-table
                            'completion--file-name-table)
  "Internal subroutine for `read-file-name'.  Do not call this.")

(defvar read-file-name-function nil
  "If this is non-nil, `read-file-name' does its work by calling this function.")

(defvar read-file-name-predicate nil
  "Current predicate used by `read-file-name-internal'.")

(defcustom read-file-name-completion-ignore-case
  (if (memq system-type '(ms-dos windows-nt darwin macos vax-vms axp-vms))
      t nil)
  "Non-nil means when reading a file name completion ignores case."
  :group 'minibuffer
  :type 'boolean
  :version "22.1")

(defcustom insert-default-directory t
  "Non-nil means when reading a filename start with default dir in minibuffer.

When the initial minibuffer contents show a name of a file or a directory,
typing RETURN without editing the initial contents is equivalent to typing
the default file name.

If this variable is non-nil, the minibuffer contents are always
initially non-empty, and typing RETURN without editing will fetch the
default name, if one is provided.  Note however that this default name
is not necessarily the same as initial contents inserted in the minibuffer,
if the initial contents is just the default directory.

If this variable is nil, the minibuffer often starts out empty.  In
that case you may have to explicitly fetch the next history element to
request the default name; typing RETURN without editing will leave
the minibuffer empty.

For some commands, exiting with an empty minibuffer has a special meaning,
such as making the current buffer visit no file in the case of
`set-visited-file-name'."
  :group 'minibuffer
  :type 'boolean)

;; Not always defined, but only called if next-read-file-uses-dialog-p says so.
(declare-function x-file-dialog "xfns.c"
                  (prompt dir &optional default-filename mustmatch only-dir-p))

(defun read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  except that if INITIAL is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg MUSTMATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
the resulting file name must satisfy (funcall PREDICATE NAME).
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (unless dir (setq dir default-directory))
  (unless (file-name-absolute-p dir) (setq dir (expand-file-name dir)))
  (unless default-filename
    (setq default-filename (if initial (expand-file-name initial dir)
                             buffer-file-name)))
  ;; If dir starts with user's homedir, change that to ~.
  (setq dir (abbreviate-file-name dir))
  ;; Likewise for default-filename.
  (if default-filename
      (setq default-filename (abbreviate-file-name default-filename)))
  (let ((insdef (cond
                 ((and insert-default-directory (stringp dir))
                  (if initial
                      (cons (minibuffer--double-dollars (concat dir initial))
                            (length (minibuffer--double-dollars dir)))
                    (minibuffer--double-dollars dir)))
                 (initial (cons (minibuffer--double-dollars initial) 0)))))

    (if read-file-name-function
        (funcall read-file-name-function
                 prompt dir default-filename mustmatch initial predicate)
      (let ((completion-ignore-case read-file-name-completion-ignore-case)
            (minibuffer-completing-file-name t)
            (read-file-name-predicate (or predicate 'file-exists-p))
            (add-to-history nil))

        (let* ((val
                (if (not (next-read-file-uses-dialog-p))
                    ;; We used to pass `dir' to `read-file-name-internal' by
                    ;; abusing the `predicate' argument.  It's better to
                    ;; just use `default-directory', but in order to avoid
                    ;; changing `default-directory' in the current buffer,
                    ;; we don't let-bind it.
                    (lexical-let ((dir (file-name-as-directory
                                        (expand-file-name dir))))
                      (minibuffer-with-setup-hook
                          (lambda () (setq default-directory dir))
                        (completing-read prompt 'read-file-name-internal
                                         nil mustmatch insdef 'file-name-history
                                         default-filename)))
                  ;; If DIR contains a file name, split it.
                  (let ((file (file-name-nondirectory dir)))
                    (when (and default-filename (not (zerop (length file))))
                      (setq default-filename file)
                      (setq dir (file-name-directory dir)))
                    (if default-filename
                        (setq default-filename
                              (expand-file-name default-filename dir)))
                    (setq add-to-history t)
                    (x-file-dialog prompt dir default-filename mustmatch
                                   (eq predicate 'file-directory-p)))))

               (replace-in-history (eq (car-safe file-name-history) val)))
          ;; If completing-read returned the inserted default string itself
          ;; (rather than a new string with the same contents),
          ;; it has to mean that the user typed RET with the minibuffer empty.
          ;; In that case, we really want to return ""
          ;; so that commands such as set-visited-file-name can distinguish.
          (when (eq val default-filename)
            ;; In this case, completing-read has not added an element
            ;; to the history.  Maybe we should.
            (if (not replace-in-history)
                (setq add-to-history t))
            (setq val ""))
          (unless val (error "No file name specified"))

          (if (and default-filename
                   (string-equal val (if (consp insdef) (car insdef) insdef)))
              (setq val default-filename))
          (setq val (substitute-in-file-name val))

          (if replace-in-history
              ;; Replace what Fcompleting_read added to the history
              ;; with what we will actually return.
              (let ((val1 (minibuffer--double-dollars val)))
                (if history-delete-duplicates
                    (setcdr file-name-history
                            (delete val1 (cdr file-name-history))))
                (setcar file-name-history val1))
            (if add-to-history
                ;; Add the value to the history--but not if it matches
                ;; the last value already there.
                (let ((val1 (minibuffer--double-dollars val)))
                  (unless (and (consp file-name-history)
                               (equal (car file-name-history) val1))
                    (setq file-name-history
                          (cons val1
                                (if history-delete-duplicates
                                    (delete val1 file-name-history)
                                  file-name-history)))))))
          val)))))

(defun internal-complete-buffer-except (&optional buffer)
  "Perform completion on all buffers excluding BUFFER.
Like `internal-complete-buffer', but removes BUFFER from the completion list."
  (lexical-let ((except (if (stringp buffer) buffer (buffer-name buffer))))
    (apply-partially 'completion-table-with-predicate
		     'internal-complete-buffer
		     (lambda (name)
		       (not (equal (if (consp name) (car name) name) except)))
		     nil)))

;;; Old-style completion, used in Emacs-21.

(defun completion-emacs21-try-completion (string table pred point)
  (let ((completion (try-completion string table pred)))
    (if (stringp completion)
        (cons completion (length completion))
      completion)))

(defun completion-emacs21-all-completions (string table pred point)
  (completion-hilit-commonality
   (all-completions string table pred t)
   (length string)))

;;; Basic completion, used in Emacs-22.

(defun completion-emacs22-try-completion (string table pred point)
  (let ((suffix (substring string point))
        (completion (try-completion (substring string 0 point) table pred)))
    (if (not (stringp completion))
        completion
      ;; Merge a trailing / in completion with a / after point.
      ;; We used to only do it for word completion, but it seems to make
      ;; sense for all completions.
      ;; Actually, claiming this feature was part of Emacs-22 completion
      ;; is pushing it a bit: it was only done in minibuffer-completion-word,
      ;; which was (by default) not bound during file completion, where such
      ;; slashes are most likely to occur.
      (if (and (not (zerop (length completion)))
               (eq ?/ (aref completion (1- (length completion))))
               (not (zerop (length suffix)))
               (eq ?/ (aref suffix 0)))
          ;; This leaves point after the / .
          (setq suffix (substring suffix 1)))
      (cons (concat completion suffix) (length completion)))))

(defun completion-emacs22-all-completions (string table pred point)
  (completion-hilit-commonality
   (all-completions (substring string 0 point) table pred t)
   point))

(defun completion-basic-try-completion (string table pred point)
  (let ((suffix (substring string point))
        (completion (try-completion (substring string 0 point) table pred)))
    (if (not (stringp completion))
        completion
      ;; Merge end of completion with beginning of suffix.
      ;; Simple generalization of the "merge trailing /" done in Emacs-22.
      (when (and (not (zerop (length suffix)))
                 (string-match "\\(.+\\)\n\\1" (concat completion "\n" suffix)
                               ;; Make sure we don't compress things to less
                               ;; than we started with.
                               point)
                 ;; Just make sure we didn't match some other \n.
                 (eq (match-end 1) (length completion)))
        (setq suffix (substring suffix (- (match-end 1) (match-beginning 1)))))

      (cons (concat completion suffix) (length completion)))))

(defalias 'completion-basic-all-completions 'completion-emacs22-all-completions)

;;; Partial-completion-mode style completion.

;; BUGS:

;; - "minibuffer-s- TAB" with minibuffer-selected-window ends up with
;;   "minibuffer--s-" which matches other options.

(defvar completion-pcm--delim-wild-regex nil)

(defun completion-pcm--prepare-delim-re (delims)
  (setq completion-pcm--delim-wild-regex (concat "[" delims "*]")))

(defcustom completion-pcm-word-delimiters "-_. "
  "A string of characters treated as word delimiters for completion.
Some arcane rules:
If `]' is in this string, it must come first.
If `^' is in this string, it must not come first.
If `-' is in this string, it must come first or right after `]'.
In other words, if S is this string, then `[S]' must be a valid Emacs regular
expression (not containing character ranges like `a-z')."
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Refresh other vars.
         (completion-pcm--prepare-delim-re value))
  :initialize 'custom-initialize-reset
  :group 'minibuffer
  :type 'string)

(defun completion-pcm--pattern-trivial-p (pattern)
  (and (stringp (car pattern)) (null (cdr pattern))))

(defun completion-pcm--string->pattern (string &optional point)
  "Split STRING into a pattern.
A pattern is a list where each element is either a string
or a symbol chosen among `any', `star', `point'."
  (if (and point (< point (length string)))
      (let ((prefix (substring string 0 point))
            (suffix (substring string point)))
        (append (completion-pcm--string->pattern prefix)
                '(point)
                (completion-pcm--string->pattern suffix)))
    (let ((pattern nil)
          (p 0)
          (p0 0))

      (while (setq p (string-match completion-pcm--delim-wild-regex string p))
        (push (substring string p0 p) pattern)
        (if (eq (aref string p) ?*)
            (progn
              (push 'star pattern)
              (setq p0 (1+ p)))
          (push 'any pattern)
          (setq p0 p))
        (incf p))

      ;; An empty string might be erroneously added at the beginning.
      ;; It should be avoided properly, but it's so easy to remove it here.
      (delete "" (nreverse (cons (substring string p0) pattern))))))

(defun completion-pcm--pattern->regex (pattern &optional group)
  (let ((re
  (concat "\\`"
          (mapconcat
           (lambda (x)
             (case x
                      ((star any point)
                       (if (if (consp group) (memq x group) group)
                                     "\\(.*?\\)" ".*?"))
               (t (regexp-quote x))))
           pattern
                  ""))))
    ;; Avoid pathological backtracking.
    (while (string-match "\\.\\*\\?\\(?:\\\\[()]\\)*\\(\\.\\*\\?\\)" re)
      (setq re (replace-match "" t t re 1)))
    re))

(defun completion-pcm--all-completions (prefix pattern table pred)
  "Find all completions for PATTERN in TABLE obeying PRED.
PATTERN is as returned by `completion-pcm--string->pattern'."
  ;; Find an initial list of possible completions.
  (if (completion-pcm--pattern-trivial-p pattern)

      ;; Minibuffer contains no delimiters -- simple case!
      (let* ((all (all-completions (concat prefix (car pattern)) table pred))
             (last (last all)))
        (if last (setcdr last nil))
        all)

    ;; Use all-completions to do an initial cull.  This is a big win,
    ;; since all-completions is written in C!
    (let* (;; Convert search pattern to a standard regular expression.
	   (regex (completion-pcm--pattern->regex pattern))
	   (completion-regexp-list (cons regex completion-regexp-list))
	   (compl (all-completions
                   (concat prefix (if (stringp (car pattern)) (car pattern) ""))
		   table pred))
           (last (last compl)))
      (when last
        (if (and (numberp (cdr last)) (/= (cdr last) (length prefix)))
            (message "Inconsistent base-size returned by completion table %s"
                     table))
        (setcdr last nil))
      (if (not (functionp table))
	  ;; The internal functions already obeyed completion-regexp-list.
	  compl
	(let ((case-fold-search completion-ignore-case)
              (poss ()))
	  (dolist (c compl)
	    (when (string-match regex c) (push c poss)))
	  poss)))))

(defun completion-pcm--hilit-commonality (pattern completions)
  (when completions
    (let* ((re (completion-pcm--pattern->regex pattern '(point)))
           (last (last completions))
           (base-size (cdr last)))
      ;; Remove base-size during mapcar, and add it back later.
      (setcdr last nil)
      (nconc
       (mapcar
        (lambda (str)
          ;; Don't modify the string itself.
          (setq str (copy-sequence str))
          (unless (string-match re str)
            (error "Internal error: %s does not match %s" re str))
          (let ((pos (or (match-beginning 1) (match-end 0))))
            (put-text-property 0 pos
                               'font-lock-face 'completions-common-part
                               str)
            (if (> (length str) pos)
                (put-text-property pos (1+ pos)
                                   'font-lock-face 'completions-first-difference
                                   str)))
          str)
        completions)
       base-size))))

(defun completion-pcm--find-all-completions (string table pred point)
  (let* ((bounds (completion-boundaries string table pred point))
         (prefix (substring string 0 (car bounds)))
         (suffix (substring string (cdr bounds)))
         (origstring string)
         firsterror)
    (setq string (substring string (car bounds) (cdr bounds)))
    (let* ((pattern (completion-pcm--string->pattern
                     string (- point (car bounds))))
           (all (condition-case err
                    (completion-pcm--all-completions prefix pattern table pred)
                  (error (unless firsterror (setq firsterror err)) nil))))
      (when (and (null all)
                 (> (car bounds) 0)
                 (null (ignore-errors (try-completion prefix table pred))))
        ;; The prefix has no completions at all, so we should try and fix
        ;; that first.
        (let ((substring (substring prefix 0 -1)))
          (destructuring-bind (subpat suball subprefix subsuffix)
              (completion-pcm--find-all-completions
               substring table pred (length substring))
            (let ((sep (aref prefix (1- (length prefix))))
                  ;; Text that goes between the new submatches and the
                  ;; completion substring.
                  (between nil))
              ;; Eliminate submatches that don't end with the separator.
              (dolist (submatch (prog1 suball (setq suball ())))
                (when (eq sep (aref submatch (1- (length submatch))))
                  (push submatch suball)))
              (when suball
                ;; Update the boundaries and corresponding pattern.
                ;; We assume that all submatches result in the same boundaries
                ;; since we wouldn't know how to merge them otherwise anyway.
                (let* ((newstring (concat subprefix (car suball) string suffix))
                       (newpoint (+ point (- (length newstring)
                                             (length origstring))))
                       (newbounds (completion-boundaries
                                   newstring table pred newpoint))
                       (newsubstring
                        (substring newstring (car newbounds) (cdr newbounds))))
                  (unless (or (equal newsubstring string)
                              ;; Refuse new boundaries if they step over
                              ;; the submatch.
                              (< (car newbounds)
                                 (+ (length subprefix) (length (car suball)))))
                    ;; The new completed prefix does change the boundaries
                    ;; of the completed substring.
                    (setq suffix (substring newstring (cdr newbounds)))
                    (setq string newsubstring)
                    (setq between (substring newstring
                                             (+ (length subprefix)
                                                (length (car suball)))
                                             (car newbounds)))
                    (setq pattern (completion-pcm--string->pattern
                                   string (- newpoint (car bounds)))))
                  (dolist (submatch suball)
                    (setq all (nconc (mapcar
                                      (lambda (s) (concat submatch between s))
                                      (completion-pcm--all-completions
                                       (concat subprefix submatch between)
                                       pattern table pred))
                                     all)))
                  ;; FIXME: This can come in handy for try-completion,
                  ;; but isn't right for all-completions, since it lists
                  ;; invalid completions.
                  ;; (unless all
                  ;;   ;; Even though we found expansions in the prefix, none
                  ;;   ;; leads to a valid completion.
                  ;;   ;; Let's keep the expansions, tho.
                  ;;   (dolist (submatch suball)
                  ;;     (push (concat submatch between newsubstring) all)))
                  ))
              (setq pattern (append subpat (list 'any (string sep))
                                    (if between (list between)) pattern))
              (setq prefix subprefix)))))
      (if (and (null all) firsterror)
          (signal (car firsterror) (cdr firsterror))
        (list pattern all prefix suffix)))))

(defun completion-pcm-all-completions (string table pred point)
  (destructuring-bind (pattern all &optional prefix suffix)
      (completion-pcm--find-all-completions string table pred point)
    (completion-pcm--hilit-commonality pattern all)))

(defun completion-pcm--merge-completions (strs pattern)
  "Extract the commonality in STRS, with the help of PATTERN."
  (cond
   ((null (cdr strs)) (list (car strs)))
   (t
    (let ((re (completion-pcm--pattern->regex pattern 'group))
          (ccs ()))                     ;Chopped completions.

      ;; First chop each string into the parts corresponding to each
      ;; non-constant element of `pattern', using regexp-matching.
      (let ((case-fold-search completion-ignore-case))
        (dolist (str strs)
          (unless (string-match re str)
            (error "Internal error: %s doesn't match %s" str re))
          (let ((chopped ())
                (i 1))
            (while (match-beginning i)
              (push (match-string i str) chopped)
              (setq i (1+ i)))
            ;; Add the text corresponding to the implicit trailing `any'.
            (push (substring str (match-end 0)) chopped)
            (push (nreverse chopped) ccs))))

      ;; Then for each of those non-constant elements, extract the
      ;; commonality between them.
      (let ((res ()))
        ;; Make the implicit `any' explicit.  We could make it explicit
        ;; everywhere, but it would slow down regexp-matching a little bit.
        (dolist (elem (append pattern '(any)))
          (if (stringp elem)
              (push elem res)
            (let ((comps ()))
              (dolist (cc (prog1 ccs (setq ccs nil)))
                (push (car cc) comps)
                (push (cdr cc) ccs))
              (let* ((prefix (try-completion "" comps))
                     (unique (or (and (eq prefix t) (setq prefix ""))
                                 (eq t (try-completion prefix comps)))))
                (unless (equal prefix "") (push prefix res))
                ;; If there's only one completion, `elem' is not useful
                ;; any more: it can only match the empty string.
                ;; FIXME: in some cases, it may be necessary to turn an
                ;; `any' into a `star' because the surrounding context has
                ;; changed such that string->pattern wouldn't add an `any'
                ;; here any more.
                (unless unique (push elem res))))))
        ;; We return it in reverse order.
        res)))))

(defun completion-pcm--pattern->string (pattern)
  (mapconcat (lambda (x) (cond
                     ((stringp x) x)
                     ((eq x 'star) "*")
                     ((eq x 'any) "")
                     ((eq x 'point) "")))
             pattern
             ""))

(defun completion-pcm-try-completion (string table pred point)
  (destructuring-bind (pattern all prefix suffix)
      (completion-pcm--find-all-completions string table pred point)
    (when all
      (let* ((mergedpat (completion-pcm--merge-completions all pattern))
             ;; `mergedpat' is in reverse order.  Place new point (by
	     ;; order of preference) either at the old point, or at
	     ;; the last place where there's something to choose, or
	     ;; at the very end.
             (pointpat (or (memq 'point mergedpat) (memq 'any mergedpat)
			   mergedpat))
             ;; New pos from the start.
             (newpos (length (completion-pcm--pattern->string pointpat)))
	     ;; Do it afterwards because it changes `pointpat' by sideeffect.
             (merged (completion-pcm--pattern->string (nreverse mergedpat))))
        (if (and (> (length merged) 0) (> (length suffix) 0)
                 (eq (aref merged (1- (length merged))) (aref suffix 0)))
            (setq suffix (substring suffix 1)))
        (cons (concat prefix merged suffix) (+ newpos (length prefix)))))))


(provide 'minibuffer)

;; arch-tag: ef8a0a15-1080-4790-a754-04017c02f08f
;;; minibuffer.el ends here
