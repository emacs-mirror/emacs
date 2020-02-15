;;; electric.el --- window maker and Command loop for `electric' modes

;; Copyright (C) 1985-1986, 1995, 2001-2020 Free Software Foundation,
;; Inc.

;; Author: K. Shane Hartman
;; Maintainer: emacs-devel@gnu.org
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
	;; This call is executed even if the window existed before, was
	;; reused, ... contradicting a claim in the comment before this
	;; function.
	(fit-window-to-buffer win max-height nil nil nil t))
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
    indent-relative-first-indent-point py-indent-line coffee-indent-line
    org-indent-line yaml-indent-line haskell-indentation-indent-line
    haskell-indent-cycle haskell-simple-indent yaml-indent-line)
  "List of indent functions that can't reindent.
If `indent-line-function' is one of those, then `electric-indent-mode' will
not try to reindent lines.  It is normally better to make the major
mode set `electric-indent-inhibit', but this can be used as a workaround.")

(defun electric-indent-post-self-insert-function ()
  "Function that `electric-indent-mode' adds to `post-self-insert-hook'.
This indents if the hook `electric-indent-functions' returns non-nil,
or if a member of `electric-indent-chars' was typed; but not in a string
or comment."
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
      ;; If we error during indent, silently give up since this is an
      ;; automatic action that the user didn't explicitly request.
      ;; But we don't want to suppress errors from elsewhere in *this*
      ;; function, hence the `condition-case' and `throw' (Bug#18764).
      (catch 'indent-error
        ;; For newline, we want to reindent both lines and basically
        ;; behave like reindent-then-newline-and-indent (whose code we
        ;; hence copied).
        (let ((at-newline (<= pos (line-beginning-position))))
          (when at-newline
            (let ((before (copy-marker (1- pos) t)))
              (save-excursion
                (unless
                    (or (memq indent-line-function
                              electric-indent-functions-without-reindent)
                        electric-indent-inhibit)
                  ;; Don't reindent the previous line if the
                  ;; indentation function is not a real one.
                  (goto-char before)
                  (condition-case-unless-debug ()
                      (indent-according-to-mode)
                    (error (throw 'indent-error nil))))
                (unless (eq electric-indent-inhibit 'electric-layout-mode)
                  ;; Unless we're operating under
                  ;; `electric-layout-mode' (Bug#35254), the goal here
                  ;; will be to remove the trailing whitespace after
                  ;; reindentation of the previous line because that
                  ;; may have (re)introduced it.
                  (goto-char before)
                  ;; We were at EOL in marker `before' before the call
                  ;; to `indent-according-to-mode' but after we may
                  ;; not be (Bug#15767).
                  (when (and (eolp))
                    (delete-horizontal-space t))))))
          (unless (and electric-indent-inhibit
                       (not at-newline))
            (condition-case-unless-debug ()
                (indent-according-to-mode)
              (error (throw 'indent-error nil)))))))))

(defun electric-indent-just-newline (arg)
  "Insert just a newline, without any auto-indentation."
  (interactive "*P")
  (let ((electric-indent-mode nil))
    (newline arg 'interactive)))

;;;###autoload
(define-key global-map "\C-j" 'electric-newline-and-maybe-indent)
;;;###autoload
(defun electric-newline-and-maybe-indent ()
  "Insert a newline.
If `electric-indent-mode' is enabled, that's that, but if it
is *disabled* then additionally indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'."
  (interactive "*")
  (if electric-indent-mode
      (electric-indent-just-newline nil)
    (newline-and-indent)))

;;;###autoload
(define-minor-mode electric-indent-mode
  "Toggle on-the-fly reindentation (Electric Indent mode).

When enabled, this reindents whenever the hook `electric-indent-functions'
returns non-nil, or if you insert a character from `electric-indent-chars'.

This is a global minor mode.  To toggle the mode in a single buffer,
use `electric-indent-local-mode'."
  :global t :group 'electricity
  :initialize 'custom-initialize-delay
  :init-value t
  (if (not electric-indent-mode)
      (unless (catch 'found
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (if electric-indent-mode (throw 'found t)))))
        (remove-hook 'post-self-insert-hook
                     #'electric-indent-post-self-insert-function))
    (add-hook 'post-self-insert-hook
              #'electric-indent-post-self-insert-function
              60)))

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

;;; Electric newlines after/before/around some chars.

(defvar electric-layout-rules nil
  "List of rules saying where to automatically insert newlines.

Each rule has the form (CHAR . WHERE), the rule matching if the
character just inserted was CHAR.  WHERE specifies where to
insert newlines, and can be:

* one of the symbols `before', `after', `around', `after-stay',
  or nil.

* a list of the preceding symbols, processed in order of
  appearance to insert multiple newlines;

* a function of no arguments that returns one of the previous
  values.

Each symbol specifies where, in relation to the position POS of
the character inserted, the newline character(s) should be
inserted.  `after-stay' means insert a newline after POS but stay
in the same place.

Instead of the (CHAR . WHERE) form, a rule can also be just a
function of a single argument, the character just inserted.  It
is called at that position, and should return a value compatible with
WHERE if the rule matches, or nil if it doesn't match.

If multiple rules match, only first one is executed.")

;; TODO: Make this a defcustom?
(defvar electric-layout-allow-duplicate-newlines nil
  "If non-nil, allow duplication of `before' newlines.")

(defun electric-layout-post-self-insert-function ()
  (when electric-layout-mode
    (electric-layout-post-self-insert-function-1)))

;; for edebug's sake, a separate function
(defun electric-layout-post-self-insert-function-1 ()
  (let* ((pos (electric--after-char-pos))
         probe
         (rules electric-layout-rules)
         (rule
          (catch 'done
            (when pos
              (while (setq probe (pop rules))
                (cond ((and (consp probe)
                            (eq (car probe) last-command-event))
                       (throw 'done (cdr probe)))
                      ((functionp probe)
                       (let ((res
                              (save-excursion
                                (goto-char pos)
                                (funcall probe last-command-event))))
                         (when res (throw 'done res))))))))))
    (when (and rule
               ;; Not in a string or comment.
               (not (nth 8 (save-excursion (syntax-ppss pos)))))
      (goto-char pos)
      (when (functionp rule) (setq rule (funcall rule)))
      (dolist (sym (if (symbolp rule) (list rule) rule))
        (let* ((nl-after
                (lambda ()
                  ;; FIXME: we use `newline', which calls
                  ;; `self-insert-command' and ran
                  ;; `post-self-insert-hook' recursively.  It happened
                  ;; to make `electric-indent-mode' work automatically
                  ;; with `electric-layout-mode' (at the cost of
                  ;; re-indenting lines multiple times), but I'm not
                  ;; sure it's what we want.
                  ;;
                  ;; JT@19/02/22: Indeed in the case of `before'
                  ;; newlines, re-indentation is prevented.
                  ;;
                  ;; FIXME: when `newline'ing, we exceptionally
                  ;; prevent a specific behavior of
                  ;; `eletric-pair-mode', that of opening an extra
                  ;; newline between newly inserted matching paris.
                  ;; In theory that behavior should be provided by
                  ;; `electric-layout-mode' instead, which should be
                  ;; possible given the current API.
                  ;;
                  ;; FIXME: check eolp before inserting \n?
                  (let ((electric-layout-mode nil)
                        (electric-pair-open-newline-between-pairs nil))
                    (newline 1 t))))
               (nl-before
                (lambda ()
                  (save-excursion
                    (goto-char (1- pos))
                    ;; Normally, we don't duplicate newlines, but when
                    ;; we're being called for i.e. a closer brace for
                    ;; `electric-pair-mode' generally make sense.  So
                    ;; consult `electric-layout-allow-duplicate-newlines'
                    (unless (and (not electric-layout-allow-duplicate-newlines)
                                 (progn (skip-chars-backward " \t")
                                        (bolp)))
                      ;; FIXME: JT@19/03/22: Make sure the `before'
                      ;; newline being inserted here does not trigger
                      ;; reindentation.  It doesn't seem to be our job
                      ;; to do so and it break with `cc-mode's
                      ;; indentation function.  Later on we can add a
                      ;; before-and-maybe-indent, or if the user
                      ;; really wants to reindent, then
                      ;; `last-command-event' should be in
                      ;; `electric-indent-chars'.
                      (let ((electric-indent-inhibit 'electric-layout-mode))
                        (funcall nl-after)))))))
            (pcase sym
              ('before (funcall nl-before))
              ('after  (funcall nl-after))
              ('after-stay (save-excursion (funcall nl-after)))
              ('around (funcall nl-before) (funcall nl-after))))))))

;;;###autoload
(define-minor-mode electric-layout-mode
  "Automatically insert newlines around some chars.

The variable `electric-layout-rules' says when and how to insert newlines."
  :global t :group 'electricity
  (cond (electric-layout-mode
         (add-hook 'post-self-insert-hook
                   #'electric-layout-post-self-insert-function
                   40))
        (t
         (remove-hook 'post-self-insert-hook
                      #'electric-layout-post-self-insert-function))))

;;;###autoload
(define-minor-mode electric-layout-local-mode
  "Toggle `electric-layout-mode' only in this buffer."
  :variable (buffer-local-value 'electric-layout-mode (current-buffer))
  (cond
   ((eq electric-layout-mode (default-value 'electric-layout-mode))
    (kill-local-variable 'electric-layout-mode))
   ((not (default-value 'electric-layout-mode))
    ;; Locally enabled, but globally disabled.
    (electric-layout-mode 1)		  ; Setup the hooks.
    (setq-default electric-layout-mode nil) ; But keep it globally disabled.
    )))

;;; Electric quoting.

(defcustom electric-quote-comment t
  "Non-nil means to use electric quoting in program comments."
  :version "25.1"
  :type 'boolean :safe 'booleanp :group 'electricity)

(defcustom electric-quote-string nil
  "Non-nil means to use electric quoting in program strings."
  :version "25.1"
  :type 'boolean :safe 'booleanp :group 'electricity)

(defcustom electric-quote-chars '(?‘ ?’ ?“ ?”)
  "Curved quote characters for `electric-quote-mode'.
This list's members correspond to left single quote, right single
quote, left double quote, and right double quote, respectively."
  :version "26.1"
  :type '(list character character character character)
  :safe #'(lambda (x)
	    (pcase x
	      (`(,(pred characterp) ,(pred characterp)
		 ,(pred characterp) ,(pred characterp))
	       t)))
  :group 'electricity)

(defcustom electric-quote-paragraph t
  "Non-nil means to use electric quoting in text paragraphs."
  :version "25.1"
  :type 'boolean :safe 'booleanp :group 'electricity)

(defcustom electric-quote-context-sensitive nil
  "Non-nil means to replace \\=' with an electric quote depending on context.
If `electric-quote-context-sensitive' is non-nil, Emacs replaces
\\=' and \\='\\=' with an opening quote after a line break,
whitespace, opening parenthesis, or quote and leaves \\=` alone."
  :version "26.1"
  :type 'boolean :safe #'booleanp :group 'electricity)

(defcustom electric-quote-replace-double nil
  "Non-nil means to replace \" with an electric double quote.
Emacs replaces \" with an opening double quote after a line
break, whitespace, opening parenthesis, or quote, and with a
closing double quote otherwise."
  :version "26.1"
  :type 'boolean :safe #'booleanp :group 'electricity)

(defvar electric-quote-inhibit-functions ()
  "List of functions that should inhibit electric quoting.
When the variable `electric-quote-mode' is non-nil, Emacs will
call these functions in order after the user has typed an \\=` or
\\=' character.  If one of them returns non-nil, electric quote
substitution is inhibited.  The functions are called after the
\\=` or \\=' character has been inserted with point directly
after the inserted character.  The functions in this hook should
not move point or change the current buffer.")

(defvar electric-pair-text-pairs)

(defun electric-quote-post-self-insert-function ()
  "Function that `electric-quote-mode' adds to `post-self-insert-hook'.
This requotes when a quoting key is typed."
  (when (and electric-quote-mode
             (or (eq last-command-event ?\')
                 (and (not electric-quote-context-sensitive)
                      (eq last-command-event ?\`))
                 (and electric-quote-replace-double
                      (eq last-command-event ?\")))
             (not (run-hook-with-args-until-success
                   'electric-quote-inhibit-functions))
             (if (derived-mode-p 'text-mode)
                 electric-quote-paragraph
               (and comment-start comment-use-syntax
                    (or electric-quote-comment electric-quote-string)
                    (let* ((syntax (syntax-ppss))
                           (beg (nth 8 syntax)))
                      (and beg
                           (or (and electric-quote-comment (nth 4 syntax))
                               (and electric-quote-string (nth 3 syntax)))
                           ;; Do not requote a quote that starts or ends
                           ;; a comment or string.
                           (eq beg (nth 8 (save-excursion
                                            (syntax-ppss (1- (point)))))))))))
    (pcase electric-quote-chars
      (`(,q< ,q> ,q<< ,q>>)
       (save-excursion
         (let ((backtick ?\`))
           (if (or (eq last-command-event ?\`)
                   (and (or electric-quote-context-sensitive
                            (and electric-quote-replace-double
                                 (eq last-command-event ?\")))
                        (save-excursion
                          (backward-char)
                          (skip-syntax-backward "\\")
                          (or (bobp) (bolp)
                              (memq (char-before) (list q< q<<))
                              (memq (char-syntax (char-before))
                                    '(?\s ?\())))
                        (setq backtick ?\')))
               (cond ((search-backward (string q< backtick) (- (point) 2) t)
                      (replace-match (string q<<))
                      (when (and electric-pair-mode
                                 (eq (cdr-safe
                                      (assq q< electric-pair-text-pairs))
                                     (char-after)))
                        (delete-char 1))
                      (setq last-command-event q<<))
                     ((search-backward (string backtick) (1- (point)) t)
                      (replace-match (string q<))
                      (setq last-command-event q<))
                     ((search-backward "\"" (1- (point)) t)
                      (replace-match (string q<<))
                      (setq last-command-event q<<)))
             (cond ((search-backward (string q> ?') (- (point) 2) t)
                    (replace-match (string q>>))
                    (setq last-command-event q>>))
                   ((search-backward "'" (1- (point)) t)
                    (replace-match (string q>))
                    (setq last-command-event q>))
                   ((search-backward "\"" (1- (point)) t)
                    (replace-match (string q>>))
                    (setq last-command-event q>>))))))))))

;;;###autoload
(define-minor-mode electric-quote-mode
  "Toggle on-the-fly requoting (Electric Quote mode).

When enabled, as you type this replaces \\=` with ‘, \\=' with ’,
\\=`\\=` with “, and \\='\\=' with ”.  This occurs only in comments, strings,
and text paragraphs, and these are selectively controlled with
`electric-quote-comment', `electric-quote-string', and
`electric-quote-paragraph'.

Customize `electric-quote-chars' to use characters other than the
ones listed here.

This is a global minor mode.  To toggle the mode in a single buffer,
use `electric-quote-local-mode'."
  :global t :group 'electricity
  :initialize 'custom-initialize-delay
  :init-value nil
  (if (not electric-quote-mode)
      (unless (catch 'found
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (if electric-quote-mode (throw 'found t)))))
        (remove-hook 'post-self-insert-hook
                     #'electric-quote-post-self-insert-function))
    (add-hook 'post-self-insert-hook
              #'electric-quote-post-self-insert-function
              10)))

;;;###autoload
(define-minor-mode electric-quote-local-mode
  "Toggle `electric-quote-mode' only in this buffer."
  :variable (buffer-local-value 'electric-quote-mode (current-buffer))
  (cond
   ((eq electric-quote-mode (default-value 'electric-quote-mode))
    (kill-local-variable 'electric-quote-mode))
   ((not (default-value 'electric-quote-mode))
    ;; Locally enabled, but globally disabled.
    (electric-quote-mode 1)                ; Setup the hooks.
    (setq-default electric-quote-mode nil) ; But keep it globally disabled.
    )))

(provide 'electric)

;;; electric.el ends here
