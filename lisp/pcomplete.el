;;; pcomplete.el --- programmable completion -*- lexical-binding: t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes abbrev

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

;; This module provides a programmable completion facility using
;; "completion functions".  Each completion function is responsible
;; for producing a list of possible completions relevant to the current
;; argument position.
;;
;; To use pcomplete with shell-mode, for example, you will need the
;; following in your init file:
;;
;;   (add-hook 'shell-mode-hook #'pcomplete-shell-setup)
;;
;; Most of the code below simply provides support mechanisms for
;; writing completion functions.  Completion functions themselves are
;; very easy to write.  They have few requirements beyond those of
;; regular Lisp functions.
;;
;; Consider the following example, which will complete against
;; filenames for the first two arguments, and directories for all
;; remaining arguments:
;;
;;   (defun pcomplete/my-command ()
;;     (pcomplete-here (pcomplete-entries))
;;     (pcomplete-here (pcomplete-entries))
;;     (while (pcomplete-here (pcomplete-dirs))))
;;
;; Here are the requirements for completion functions:
;;
;; @ They must be called "pcomplete/MAJOR-MODE/NAME", or
;;   "pcomplete/NAME".  This is how they are looked up, using the NAME
;;   specified in the command argument (the argument in first
;;   position).
;;
;; @ They must be callable with no arguments.
;;
;; @ Their return value is ignored.  If they actually return normally,
;;   it means no completions were available.
;;
;; @ In order to provide completions, they must throw the tag
;;   `pcomplete-completions'.  The value must be a completion table
;;   (i.e. a table that can be passed to try-completion and friends)
;;   for the final argument.
;;
;; @ To simplify completion function logic, the tag `pcompleted' may
;;   be thrown with a value of nil in order to abort the function.  It
;;   means that there were no completions available.
;;
;; When a completion function is called, the variable `pcomplete-args'
;; is in scope, and contains all of the arguments specified on the
;; command line.  The variable `pcomplete-last' is the index of the
;; last argument in that list.
;;
;; The variable `pcomplete-index' is used by the completion code to
;; know which argument the completion function is currently examining.
;; It always begins at 1, meaning the first argument after the command
;; name.
;;
;; To facilitate writing completion logic, a special macro,
;; `pcomplete-here', has been provided which does several things:
;;
;;  1. It will throw `pcompleted' (with a value of nil) whenever
;;     `pcomplete-index' exceeds `pcomplete-last'.
;;
;;  2. It will increment `pcomplete-index' if the final argument has
;;     not been reached yet.
;;
;;  3. It will evaluate the form passed to it, and throw the result
;;     using the `pcomplete-completions' tag, if it is called when
;;     `pcomplete-index' is pointing to the final argument.
;;
;; Sometimes a completion function will want to vary the possible
;; completions for an argument based on the previous one.  To
;; facilitate tests like this, the function `pcomplete-test' and
;; `pcomplete-match' are provided.  Called with one argument, they
;; test the value of the previous command argument.  Otherwise, a
;; relative index may be given as an optional second argument, where 0
;; refers to the current argument, 1 the previous, 2 the one before
;; that, etc.  The symbols `first' and `last' specify absolute
;; offsets.
;;
;; Here is an example which will only complete against directories for
;; the second argument if the first argument is also a directory:
;;
;;   (defun pcomplete/example ()
;;      (pcomplete-here (pcomplete-entries))
;;      (if (pcomplete-test #'file-directory-p)
;;          (pcomplete-here (pcomplete-dirs))
;;        (pcomplete-here (pcomplete-entries))))
;;
;; For generating completion lists based on directory contents, see
;; the functions `pcomplete-entries', `pcomplete-dirs',
;; `pcomplete-executables' and `pcomplete-all-entries'.
;;
;; Consult the documentation for `pcomplete-here' for information
;; about its other arguments.

;;; Code:

(require 'comint)
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(defgroup pcomplete nil
  "Programmable completion."
  :version "21.1"
  :group 'processes)

;;; User Variables:

(defcustom pcomplete-file-ignore nil
  "A regexp of filenames to be disregarded during file completion."
  :type '(choice regexp (const :tag "None" nil)))

(defcustom pcomplete-dir-ignore nil
  "A regexp of names to be disregarded during directory completion."
  :type '(choice regexp (const :tag "None" nil)))

(define-obsolete-variable-alias 'pcomplete-ignore-case 'completion-ignore-case
  "28.1")

(defcustom pcomplete-autolist nil
  "If non-nil, automatically list possibilities on partial completion.
This mirrors the optional behavior of tcsh."
  :type 'boolean)

(defcustom pcomplete-suffix-list (list ?/ ?:)
  "A list of characters which constitute a proper suffix."
  :type '(repeat character))
(make-obsolete-variable 'pcomplete-suffix-list nil "24.1")

(defcustom pcomplete-recexact nil
  "If non-nil, use shortest completion if characters cannot be added.
This mirrors the optional behavior of tcsh.

A non-nil value is useful if `pcomplete-autolist' is non-nil too."
  :type 'boolean)

(defcustom pcomplete-man-function #'man
  "A function to that will be called to display a manual page.
It will be passed the name of the command to document."
  :type 'function)

(defcustom pcomplete-compare-entry-function #'string-lessp
  "This function is used to order file entries for completion.
The behavior of most all shells is to sort alphabetically."
  :type '(radio (function-item string-lessp)
		(function-item file-newer-than-file-p)
		(function :tag "Other")))

(defcustom pcomplete-help nil
  "A string or function (or nil) used for context-sensitive help.
If a string, it should name an Info node that will be jumped to.
If non-nil, it must a sexp that will be evaluated, and whose
result will be shown in the minibuffer.
If nil, the function `pcomplete-man-function' will be called with the
current command argument."
  :type '(choice string sexp (const :tag "Use man page" nil)))

(defcustom pcomplete-expand-before-complete nil
  "If non-nil, expand the current argument before completing it.
This means that typing something such as `$HOME/bi' followed by
\\[pcomplete-argument] will cause the variable reference to be
resolved first, and the resultant value that will be completed against
to be inserted in the buffer.  Note that exactly what gets expanded
and how is entirely up to the behavior of the
`pcomplete-parse-arguments-function'."
  :type 'boolean)

(defvar pcomplete-allow-modifications nil
  "If non-nil, allow effects in `pcomplete-parse-arguments-function'.
For the `pcomplete' command, it was common for functions in
`pcomplete-parse-arguments-function' to make modifications to the
buffer, like expanding variables and such.
For `completion-at-point-functions', this is not an option any more, so
this variable is used to tell `pcomplete-parse-arguments-function'
whether it can do the modifications like it used to, or whether
it should refrain from doing so.")

(defcustom pcomplete-parse-arguments-function
  #'pcomplete-parse-buffer-arguments
  "A function to call to parse the current line's arguments.
It should be called with no parameters, and with point at the position
of the argument that is to be completed.

It must either return nil, or a cons cell of the form:

  ((ARG...) (BEG-POS...))

The two lists must be identical in length.  The first gives the final
value of each command line argument (which need not match the textual
representation of that argument), and BEG-POS gives the beginning
position of each argument, as it is seen by the user.  The establishes
a relationship between the fully resolved value of the argument, and
the textual representation of the argument."
  :type 'function)

(defcustom pcomplete-cycle-completions t
  "If non-nil, hitting the TAB key cycles through the completion list.
Typical Emacs behavior is to complete as much as possible, then pause
waiting for further input.  Then if TAB is hit again, show a list of
possible completions.  When `pcomplete-cycle-completions' is non-nil,
it acts more like zsh or 4nt, showing the first maximal match first,
followed by any further matches on each subsequent pressing of the TAB
key.  \\[pcomplete-list] is the key to press if the user wants to see
the list of possible completions."
  :type 'boolean)

(defcustom pcomplete-cycle-cutoff-length 5
  "If the number of completions is greater than this, don't cycle.
This variable is a compromise between the traditional Emacs style of
completion, and the \"cycling\" style.  Basically, if there are more
than this number of completions possible, don't automatically pick the
first one and then expect the user to press TAB to cycle through them.
Typically, when there are a large number of completion possibilities,
the user wants to see them in a list buffer so that they can know what
options are available.  But if the list is small, it means the user
has already entered enough input to disambiguate most of the
possibilities, and therefore they are probably most interested in
cycling through the candidates.  Set this value to nil if you want
cycling to always be enabled."
  :type '(choice integer (const :tag "Always cycle" nil)))

(defcustom pcomplete-restore-window-delay 1
  "The number of seconds to wait before restoring completion windows.
Once the completion window has been displayed, if the user then goes
on to type something else, that completion window will be removed from
the display (actually, the original window configuration before it was
displayed will be restored), after this many seconds of idle time.  If
set to nil, completion windows will be left on second until the user
removes them manually.  If set to 0, they will disappear immediately
after the user enters a key other than TAB."
  :type '(choice integer (const :tag "Never restore" nil)))

(defcustom pcomplete-try-first-hook nil
  "A list of functions which are called before completing an argument.
This can be used, for example, for completing things which might apply
to all arguments, such as variable names after a $."
  :type 'hook)

(defsubst pcomplete-executables (&optional regexp)
  "Complete amongst a list of directories and executables."
  (pcomplete-entries regexp #'file-executable-p))

(defmacro pcomplete-here (&optional form stub paring form-only)
  "Complete against the current argument, if at the end.
If completion is to be done here, evaluate FORM to generate the completion
table which will be used for completion purposes.  If STUB is a
string, use it as the completion stub instead of the default (which is
the entire text of the current argument).

For an example of when you might want to use STUB: if the current
argument text is `long-path-name/', you don't want the completions
list display to be cluttered by `long-path-name/' appearing at the
beginning of every alternative.  Not only does this make things less
intelligible, but it is also inefficient.  Yet, if the completion list
does not begin with this string for every entry, the current argument
won't complete correctly.

The solution is to specify a relative stub.  It allows you to
substitute a different argument from the current argument, almost
always for the sake of efficiency.

If PARING is nil, this argument will be pared against previous
arguments using the function `file-truename' to normalize them.
PARING may be a function, in which case that function is used for
normalization.  If PARING is t, the argument dealt with by this
call will not participate in argument paring.  If it is the
integer 0, all previous arguments that have been seen will be
cleared.

If FORM-ONLY is non-nil, only the result of FORM will be used to
generate the completions list.  This means that the hook
`pcomplete-try-first-hook' will not be run."
  (declare (debug t))
  `(pcomplete--here (lambda () ,form) ,stub ,paring ,form-only))

(defcustom pcomplete-command-completion-function
  (lambda ()
    (pcomplete-here (pcomplete-executables)))
  "Function called for completing the initial command argument."
  :type 'function)

(defcustom pcomplete-command-name-function #'pcomplete-command-name
  "Function called for determining the current command name."
  :type 'function)

(defcustom pcomplete-default-completion-function
  (lambda ()
    (while (pcomplete-here (pcomplete-entries))))
  "Function called when no completion rule can be found.
This function is used to generate completions for every argument."
  :type 'function)

(defcustom pcomplete-use-paring t
  "If t, pare alternatives that have already been used.
If nil, you will always see the completion set of possible options, no
matter which of those options have already been used in previous
command arguments."
  :type 'boolean)

(defcustom pcomplete-termination-string " "
  "A string that is inserted after any completion or expansion.
This is usually a space character, useful when completing lists of
words separated by spaces.  However, if your list uses a different
separator character, or if the completion occurs in a word that is
already terminated by a character, this variable should be locally
modified to be an empty string, or the desired separation string."
  :type 'string)

(defcustom pcomplete-hosts-file "/etc/hosts"
  "The name of the /etc/hosts file."
  :type '(choice (const :tag "No hosts file" nil) file))

;;; Internal Variables:

;; for cycling completion support
(defvar-local pcomplete-current-completions nil)
(defvar-local pcomplete-last-completion-length nil)
(defvar-local pcomplete-last-completion-stub nil)
(defvar-local pcomplete-last-completion-raw nil)
(defvar-local pcomplete-last-window-config nil)
(defvar-local pcomplete-window-restore-timer nil)

;; used for altering pcomplete's behavior.  These global variables
;; should always be nil.
(defvar pcomplete-show-help nil)
(defvar pcomplete-show-list nil)
(defvar pcomplete-expand-only-p nil)

;; for the sake of the byte-compiler, when compiling other files that
;; contain completion functions
(defvar pcomplete-args nil)
(defvar pcomplete-begins nil)
(defvar pcomplete-last nil)
(defvar pcomplete-index nil)
(defvar pcomplete-stub nil)
(defvar pcomplete-seen nil)
(defvar pcomplete-norm-func nil)

;;; User Functions:

(defun pcomplete-default-exit-function (_s status)
  "The default exit function to use in `pcomplete-completions-at-point'.
This just adds `pcomplete-termination-string' after the
completion if STATUS is `finished'."
  (unless (zerop (length pcomplete-termination-string))
    (when (eq status 'finished)
      (if (looking-at
           (regexp-quote pcomplete-termination-string))
          (goto-char (match-end 0))
        (insert pcomplete-termination-string)))))

(defvar pcomplete-exit-function #'pcomplete-default-exit-function
  "The exit function to call in `pcomplete-completions-at-point'.

This variable is let-bound in `pcomplete-completions-at-point',
so you can modify or advise it in order to adjust the behavior
for a specific completion.  For example, you might do the
following in a `pcomplete-try-first-hook' function to insert a
trailing slash after a completion:

  (add-function
   :before (var pcomplete-exit-function)
   (lambda (_ status)
     (when (eq status \\='finished)
       (insert \"/\"))))")

;;; Alternative front-end using the standard completion facilities.

;; The way pcomplete-parse-arguments and pcomplete-stub work only
;; works because of some deep hypothesis about the way the completion
;; work.  Basically, it makes it pretty much impossible to have
;; completion other than prefix-completion.
;;
;; pcomplete--common-suffix and completion-table-subvert try to work around
;; this difficulty with heuristics, but it's really a hack.

(defvar pcomplete-unquote-argument-function #'comint--unquote-argument)

(defsubst pcomplete-unquote-argument (s)
  (funcall pcomplete-unquote-argument-function s))

(defvar pcomplete-requote-argument-function #'comint--requote-argument)

(defun pcomplete--common-suffix (s1 s2)
  ;; Since S2 is expected to be the "unquoted/expanded" version of S1,
  ;; there shouldn't be any case difference, even if the completion is
  ;; case-insensitive.
  (let ((case-fold-search nil))
    (string-match
     ;; \x3FFF7F is just an arbitrary char among the ones Emacs accepts
     ;; that hopefully will never appear in normal text.
     "\\(?:.\\|\n\\)*?\\(\\(?:.\\|\n\\)*\\)\x3FFF7F\\(?:.\\|\n\\)*\\1\\'"
     (concat s1 "\x3FFF7F" s2))
    (- (match-end 1) (match-beginning 1))))

(defun pcomplete-completions-at-point ()
  "Provide standard completion using pcomplete's completion tables.
Same as `pcomplete' but using the standard completion UI."
  ;; FIXME: it only completes the text before point, whereas the
  ;; standard UI may also consider text after point.
  ;; FIXME: the `pcomplete' UI may be used internally during
  ;; pcomplete-completions and then throw to `pcompleted', thus
  ;; imposing the pcomplete UI over the standard UI.
  (catch 'pcompleted
    (let* ((pcomplete-stub)
           (buffer-read-only
            ;; Make sure the function obeys `pcomplete-allow-modifications'.
            (if pcomplete-allow-modifications buffer-read-only t))
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-exit-function pcomplete-exit-function)
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           ;; Apparently the vars above are global vars modified by
           ;; side-effects, whereas pcomplete-completions is the core
           ;; function that finds the chunk of text to complete
           ;; (returned indirectly in pcomplete-stub) and the set of
           ;; possible completions.
           (completions (pcomplete-completions))
           ;; Usually there's some close connection between pcomplete-stub
           ;; and the text before point.  But depending on what
           ;; pcomplete-parse-arguments-function does, that connection
           ;; might not be that close.  E.g. in eshell,
           ;; pcomplete-parse-arguments-function expands envvars.
           ;;
           ;; Since we use minibuffer-complete, which doesn't know
           ;; pcomplete-stub and works from the buffer's text instead,
           ;; we need to trick minibuffer-complete, into using
           ;; pcomplete-stub without its knowledge.  To that end, we
           ;; use completion-table-subvert to construct a completion
           ;; table which expects strings using a prefix from the
           ;; buffer's text but internally uses the corresponding
           ;; prefix from pcomplete-stub.
           ;;
           (argbeg (pcomplete-begin))
           ;; When completing an envvar within an argument in Eshell
           ;; (e.g. "cd /home/$US TAB"), `pcomplete-stub' will just be
           ;; "US" whereas `argbeg' will point to the first "/".
           ;; We could rely on c-t-subvert to handle the difference,
           ;; but we try here to guess the "real" beginning so as to
           ;; rely less on c-t-subvert.
           (beg (max (- (point) (length pcomplete-stub))
                     argbeg))
           buftext)
      ;; Try and improve our guess of `beg' in case the difference
      ;; between pcomplete-stub and the buffer's text is simply due to
      ;; some chars removed by unquoting.  Again, this is not
      ;; indispensable but reduces the reliance on c-t-subvert and
      ;; improves corner case behaviors.
      (while (progn (setq buftext (pcomplete-unquote-argument
                                   (buffer-substring beg (point))))
                    (and (> beg argbeg)
                         (> (length pcomplete-stub) (length buftext))))
        (setq beg (max argbeg (- beg (- (length pcomplete-stub)
                                        (length buftext))))))
      (when completions
        (let ((table
               (completion-table-with-quoting
                (if (equal pcomplete-stub buftext)
                    completions
                  ;; This may not always be strictly right, but given the lack
                  ;; of any other info, it's about as good as it gets, and in
                  ;; practice it should work just fine (fingers crossed).
                  (let ((suf-len (pcomplete--common-suffix
                                  pcomplete-stub buftext)))
                    (completion-table-subvert
                     completions
                     (substring buftext 0 (- (length buftext) suf-len))
                     (substring pcomplete-stub 0
                                (- (length pcomplete-stub) suf-len)))))
                pcomplete-unquote-argument-function
                pcomplete-requote-argument-function))
              (pred
               ;; Pare it down, if applicable.
               (when (and pcomplete-use-paring pcomplete-seen)
                 ;; Capture the dynbound values for later use.
                 (let ((norm-func pcomplete-norm-func)
                       (seen
			(mapcar (lambda (f)
				  (funcall pcomplete-norm-func
					   (directory-file-name f)))
				pcomplete-seen)))
                   (lambda (f)
                     (not (member
                           (funcall norm-func (directory-file-name f))
                           seen)))))))
          (when completion-ignore-case
            (setq table (completion-table-case-fold table)))
          (list beg (point) table
                :annotation-function
                (lambda (cand)
                  (when (stringp cand)
                    (get-text-property 0 'pcomplete-annotation cand)))
                :company-docsig
                (lambda (cand)
                  (when (stringp cand)
                    (get-text-property 0 'pcomplete-help cand)))
                :predicate pred
                :exit-function
                pcomplete-exit-function))))))

 ;; I don't think such commands are usable before first setting up buffer-local
 ;; variables to parse args, so there's no point autoloading it.
 ;; ;;;###autoload
(defun pcomplete-std-complete ()
  (let ((data (pcomplete-completions-at-point)))
    (completion-in-region (nth 0 data) (nth 1 data) (nth 2 data)
                          (plist-get :predicate (nthcdr 3 data)))))

;;; Pcomplete's native UI.

;;;###autoload
(defun pcomplete (&optional interactively)
  "Support extensible programmable completion.
To use this function, just bind the TAB key to it, or add it to your
completion functions list (it should occur fairly early in the list)."
  (declare (obsolete "use completion-at-point and pcomplete-completions-at-point" "27.1"))
  (interactive "p")
  (if (and interactively
	   pcomplete-cycle-completions
	   pcomplete-current-completions
	   (memq last-command '(pcomplete
				pcomplete-expand-and-complete
				pcomplete-reverse)))
      (progn
	(delete-char (- pcomplete-last-completion-length))
	(if (eq this-command 'pcomplete-reverse)
	    (progn
              (push (car (last pcomplete-current-completions))
                    pcomplete-current-completions)
	      (setcdr (last pcomplete-current-completions 2) nil))
	  (nconc pcomplete-current-completions
		 (list (car pcomplete-current-completions)))
	  (setq pcomplete-current-completions
		(cdr pcomplete-current-completions)))
	(pcomplete-insert-entry pcomplete-last-completion-stub
                                (car pcomplete-current-completions)
				nil pcomplete-last-completion-raw))
    (setq pcomplete-current-completions nil
	  pcomplete-last-completion-raw nil)
    (catch 'pcompleted
      (let* ((pcomplete-stub)
	     (pcomplete-allow-modifications t)
	     pcomplete-seen pcomplete-norm-func
	     pcomplete-args pcomplete-last pcomplete-index
	     (pcomplete-autolist pcomplete-autolist)
	     (pcomplete-suffix-list pcomplete-suffix-list)
	     (completions (pcomplete-completions))
	     (result (pcomplete-do-complete pcomplete-stub completions)))
	(and result
	     (not (eq (car result) 'listed))
	     (cdr result)
	     (pcomplete-insert-entry pcomplete-stub (cdr result)
				     (memq (car result)
					   '(sole shortest))
				     pcomplete-last-completion-raw))))))

;;;###autoload
(defun pcomplete-reverse ()
  "If cycling completion is in use, cycle backwards."
  (interactive)
  (call-interactively 'pcomplete))

;;;###autoload
(defun pcomplete-expand-and-complete ()
  "Expand the textual value of the current argument.
This will modify the current buffer."
  (interactive)
  (let ((pcomplete-expand-before-complete t)
	(pcomplete-allow-modifications t))
    (with-suppressed-warnings ((obsolete pcomplete))
      (pcomplete))))

;;;###autoload
(defun pcomplete-continue ()
  "Complete without reference to any cycling completions."
  (interactive)
  (setq pcomplete-current-completions nil
	pcomplete-last-completion-raw nil)
  (call-interactively 'pcomplete))

;;;###autoload
(defun pcomplete-expand ()
  "Expand the textual value of the current argument.
This will modify the current buffer."
  (interactive)
  (let ((pcomplete-expand-before-complete t)
	(pcomplete-allow-modifications t)
	(pcomplete-expand-only-p t))
    (with-suppressed-warnings ((obsolete pcomplete))
      (pcomplete))
    (when (and pcomplete-current-completions
	       (> (length pcomplete-current-completions) 0)) ;??
      (delete-char (- pcomplete-last-completion-length))
      (while pcomplete-current-completions
	(unless (pcomplete-insert-entry
		 "" (car pcomplete-current-completions) t
                 pcomplete-last-completion-raw)
	  (insert-and-inherit pcomplete-termination-string))
	(setq pcomplete-current-completions
	      (cdr pcomplete-current-completions))))))

;;;###autoload
(defun pcomplete-help ()
  "Display any help information relative to the current argument."
  (declare (obsolete "use completion-help-at-point and pcomplete-completions-at-point" "27.1"))
  (interactive)
  (let ((pcomplete-show-help t))
    (with-suppressed-warnings ((obsolete pcomplete))
      (pcomplete))))

;;;###autoload
(defun pcomplete-list ()
  "Show the list of possible completions for the current argument."
  (interactive)
  (when (and pcomplete-cycle-completions
	     pcomplete-current-completions
	     (eq last-command 'pcomplete-argument))
    (delete-char (- pcomplete-last-completion-length))
    (setq pcomplete-current-completions nil
	  pcomplete-last-completion-raw nil))
  (let ((pcomplete-show-list t))
    (with-suppressed-warnings ((obsolete pcomplete))
      (pcomplete))))

;;; Internal Functions:

;; argument handling
(defsubst pcomplete-actual-arg (&optional index offset)
  "Return the actual text representation of the last argument.
This is different from `pcomplete-arg', which returns the textual value
that the last argument evaluated to.  This function returns what the
user actually typed in."
  (buffer-substring (pcomplete-begin index offset) (point)))

(defun pcomplete-arg (&optional index offset)
  "Return the textual content of the INDEXth argument.
INDEX is based from the current processing position.  If INDEX is
positive, values returned are closer to the command argument; if
negative, they are closer to the last argument.  If the INDEX is
outside of the argument list, nil is returned.  The default value for
INDEX is 0, meaning the current argument being examined.

The special indices `first' and `last' may be used to access those
parts of the list.

The OFFSET argument is added to/taken away from the index that will be
used.  This is really only useful with `first' and `last', for
accessing absolute argument positions.

When the argument has been transformed into something that is not
a string by `pcomplete-parse-arguments-function', the text
representation of the argument, namely what the user actually
typed in, is returned, and the value of the argument is stored in
the pcomplete-arg-value text property of that string."
  (let ((arg
         (nth (+ (pcase index
	           ('first 0)
	           ('last  pcomplete-last)
	           (_      (- pcomplete-index (or index 0))))
	         (or offset 0))
              pcomplete-args)))
    (if (or (stringp arg)
            ;; FIXME: 'last' is handled specially in Emacs 29, because
            ;; 'pcomplete-parse-arguments' accepts a list of strings
            ;; (which are completion candidates) as return value for
            ;; (pcomplete-arg 'last).  See below: "it means it's a
            ;; list of completions computed during parsing,
            ;; e.g. Eshell uses that to turn globs into lists of
            ;; completions".  This special case will be dealt with
            ;; differently in Emacs 30: the pcomplete-arg-value
            ;; property will be used by 'pcomplete-parse-arguments'.
            (eq index 'last))
        arg
      (propertize
       (car (split-string (pcomplete-actual-arg index offset)))
       'pcomplete-arg-value arg))))

(defun pcomplete-begin (&optional index offset)
  "Return the beginning position of the INDEXth argument.
See the documentation for `pcomplete-arg'."
  (setq index
	(if (eq index 'first)
	    0
	  (if (eq index 'last)
	      pcomplete-last
	    (- pcomplete-index (or index 0)))))
  (if offset
      (setq index (+ index offset)))
  (nth index pcomplete-begins))

(defsubst pcomplete-next-arg ()
  "Move the various pointers to the next argument."
  (setq pcomplete-index (1+ pcomplete-index)
	pcomplete-stub (pcomplete-arg))
  (if (> pcomplete-index pcomplete-last)
      (progn
	(message "No completions")
	(throw 'pcompleted nil))))

(defun pcomplete-command-name ()
  "Return the command name of the first argument."
  (file-name-nondirectory (pcomplete-arg 'first)))

(defun pcomplete-match (regexp &optional index offset start)
  "Like `string-match', but on the current completion argument."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (if arg
	(string-match regexp arg start)
      (throw 'pcompleted nil))))

(defun pcomplete-match-string (which &optional index offset)
  "Like `match-string', but on the current completion argument."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (if arg
	(match-string which arg)
      (throw 'pcompleted nil))))

(define-obsolete-function-alias 'pcomplete-match-beginning #'match-beginning "29.1")
(define-obsolete-function-alias 'pcomplete-match-end #'match-end "29.1")

(defsubst pcomplete--test (pred arg)
  "Perform a programmable completion predicate match."
  (and pred
       (cond ((eq pred t) t)
	     ((functionp pred)
	      (funcall pred arg))
	     ((stringp pred)
	      (string-match (concat "^" pred "$") arg)))
       pred))

(defun pcomplete-test (predicates &optional index offset)
  "Predicates to test the current programmable argument with."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (unless (null predicates)
      (if (not (listp predicates))
	  (pcomplete--test predicates arg)
	(let ((pred predicates)
	      found)
	  (while (and pred (not found))
	    (setq found (pcomplete--test (car pred) arg)
		  pred (cdr pred)))
	  found)))))

(defun pcomplete-parse-buffer-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (point-min))
	(end (point-max))
	begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
	(skip-chars-forward " \t\n")
	(push (point) begins)
	(skip-chars-forward "^ \t\n")
	(push (buffer-substring-no-properties
               (car begins) (point))
              args))
      (cons (nreverse args) (nreverse begins)))))

;;;###autoload
(defun pcomplete-comint-setup (completef-sym)
  "Setup a comint buffer to use pcomplete.
COMPLETEF-SYM should be the symbol where the
dynamic-complete-functions are kept.  For comint mode itself,
this is `comint-dynamic-complete-functions'."
  (setq-local pcomplete-parse-arguments-function
              #'pcomplete-parse-comint-arguments)
  (add-hook 'completion-at-point-functions
            #'pcomplete-completions-at-point nil 'local)
  (set (make-local-variable completef-sym)
       (copy-sequence (symbol-value completef-sym)))
  (let* ((funs (symbol-value completef-sym))
	 (elem (or (memq 'comint-filename-completion funs)
                   (memq 'shell-filename-completion funs)
                   (memq 'shell-dynamic-complete-filename funs)
		   (memq 'comint-dynamic-complete-filename funs))))
    (if elem
	(setcar elem 'pcomplete)
      (add-to-list completef-sym 'pcomplete))))

;;;###autoload
(defun pcomplete-shell-setup ()
  "Setup `shell-mode' to use pcomplete."
  ;; FIXME: insufficient
  (pcomplete-comint-setup 'comint-dynamic-complete-functions))

(declare-function comint-bol "comint" (&optional arg))

(defun pcomplete-parse-comint-arguments ()
  "Parse whitespace separated arguments in the current region."
  (declare (obsolete comint-parse-pcomplete-arguments "24.1"))
  (let ((begin (save-excursion (comint-bol nil) (point)))
	(end (point))
	begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
	(skip-chars-forward " \t\n")
	(push (point) begins)
        (while
            (progn
              (skip-chars-forward "^ \t\n\\\\")
              (when (eq (char-after) ?\\)
                (forward-char 1)
                (unless (eolp)
                  (forward-char 1)
                  t))))
	(push (buffer-substring-no-properties (car begins) (point))
              args))
      (cons (nreverse args) (nreverse begins)))))

(defun pcomplete-parse-arguments (&optional expand-p)
  "Parse the command line arguments.  Most completions need this info."
  (let ((results (funcall pcomplete-parse-arguments-function)))
    (when results
      (setq pcomplete-args (or (car results) (list ""))
	    pcomplete-begins (or (cdr results) (list (point)))
	    pcomplete-last (1- (length pcomplete-args))
	    pcomplete-index 0
	    pcomplete-stub (pcomplete-arg 'last))
      (let ((begin (pcomplete-begin 'last)))
	(if (and (listp pcomplete-stub) ;??
		 (not pcomplete-expand-only-p))
	    ;; If `pcomplete-stub' is a list, it means it's a list of
            ;; completions computed during parsing, e.g. Eshell uses
            ;; that to turn globs into lists of completions.
	    (if (not pcomplete-allow-modifications)
	        (let ((completions pcomplete-stub))
	          ;; FIXME: The mapping from what's in the buffer to the list
                  ;; of completions can be arbitrary and will often fail to be
                  ;; understood by the completion style.  See bug#50470.
                  ;; E.g. `pcomplete-stub' may end up being "~/Down*"
                  ;; while the completions contain entries like
                  ;; "/home/<foo>/Downloads" which will fail to match the
                  ;; "~/Down*" completion pattern since the completion
                  ;; is neither told that it's a file nor a global pattern.
	          (setq pcomplete-stub (buffer-substring begin (point)))
                  (throw 'pcomplete-completions completions))
	      (let* ((completions pcomplete-stub)
		     (common-prefix (try-completion "" completions))
		     (len (length common-prefix)))
		(setq pcomplete-stub common-prefix
		      pcomplete-autolist t)
		(when (and begin (> len 0) (not pcomplete-show-list))
		  (delete-region begin (point))
		  (pcomplete-insert-entry "" pcomplete-stub))
		(throw 'pcomplete-completions completions)))
	  (when expand-p
	    (if (stringp pcomplete-stub)
		(when begin
		  (delete-region begin (point))
		  (insert-and-inherit pcomplete-stub))
	      (if (and (listp pcomplete-stub)
		       pcomplete-expand-only-p)
		  ;; this is for the benefit of `pcomplete-expand'
		  (setq pcomplete-last-completion-length (- (point) begin)
			pcomplete-current-completions pcomplete-stub)
		(error "Cannot expand argument"))))
	  (if pcomplete-expand-only-p
	      (throw 'pcompleted t)
	    pcomplete-args))))))

;; file-system completion lists

(defsubst pcomplete-dirs-or-entries (&optional regexp predicate)
  "Return either directories, or qualified entries."
  (pcomplete-entries
   nil
   (lambda (f)
     (or (file-directory-p f)
         (and (or (null regexp) (string-match regexp f))
              (or (null predicate) (funcall predicate f)))))))

(defun pcomplete--entries (&optional regexp predicate)
  "Like `pcomplete-entries' but without env-var handling."
  (let* ((ign-pred
          (when (or pcomplete-file-ignore pcomplete-dir-ignore)
            ;; Capture the dynbound value for later use.
            (let ((file-ignore pcomplete-file-ignore)
                  (dir-ignore pcomplete-dir-ignore))
              (lambda (file)
                (not
                 (if (eq (aref file (1- (length file))) ?/)
                     (and dir-ignore (string-match dir-ignore file))
                   (and file-ignore (string-match file-ignore file))))))))
         (reg-pred (if regexp (lambda (file) (string-match regexp file))))
         (pred (cond
                ((null (or ign-pred reg-pred))  predicate)
                ((null (or ign-pred predicate)) reg-pred)
                ((null (or reg-pred predicate)) ign-pred)
                (t (lambda (f)
                     (and (or (null reg-pred)  (funcall reg-pred f))
                          (or (null ign-pred)  (funcall ign-pred f))
                          (or (null predicate) (funcall predicate f))))))))
    (lambda (s p a)
      (if (and (eq a 'metadata) pcomplete-compare-entry-function)
          `(metadata (cycle-sort-function
                      . ,(lambda (comps)
                           (sort comps pcomplete-compare-entry-function)))
                     ,@(cdr (completion-file-name-table s p a)))
        (let ((completion-ignored-extensions nil)
	      (completion-ignore-case completion-ignore-case))
          (completion-table-with-predicate
           #'comint-completion-file-name-table pred 'strict s p a))))))

(defconst pcomplete--env-regexp
  "\\(?:\\`\\|[^\\]\\)\\(?:\\\\\\\\\\)*\\(\\$\\(?:{\\([^}]+\\)}\\|\\(?2:[[:alnum:]_]+\\)\\)\\)")

(defun pcomplete-entries (&optional regexp predicate)
  "Complete against a list of directory candidates.
If REGEXP is non-nil, it is a regular expression used to refine the
match (files not matching the REGEXP will be excluded).
If PREDICATE is non-nil, it will also be used to refine the match
\(files for which the PREDICATE returns nil will be excluded).
If no directory information can be extracted from the completed
component, `default-directory' is used as the basis for completion."
  ;; FIXME: The old code did env-var expansion here, so we reproduce this
  ;; behavior for now, but really env-var handling should be performed globally
  ;; rather than here since it also applies to non-file arguments.
  (let ((table (pcomplete--entries regexp predicate)))
    (lambda (string pred action)
      (let ((strings nil)
            (orig-length (length string)))
        ;; Perform env-var expansion.
        (while (string-match pcomplete--env-regexp string)
          (push (substring string 0 (match-beginning 1)) strings)
          (push (getenv (match-string 2 string)) strings)
          (setq string (substring string (match-end 1))))
        (if (not (and strings
                      (or (eq action t)
                          (eq (car-safe action) 'boundaries))))
            (let ((newstring
                   (mapconcat #'identity (nreverse (cons string strings)) "")))
              ;; FIXME: We could also try to return unexpanded envvars.
              (complete-with-action action table newstring pred))
          (let* ((envpos (apply #'+ (mapcar #' length strings)))
                 (newstring
                  (mapconcat #'identity (nreverse (cons string strings)) ""))
                 (bounds (completion-boundaries newstring table pred
                                                (or (cdr-safe action) ""))))
            (if (>= (car bounds) envpos)
                ;; The env-var is "out of bounds".
                (if (eq action t)
                    (complete-with-action action table newstring pred)
                  `(boundaries
                    ,(+ (car bounds) (- orig-length (length newstring)))
                    . ,(cdr bounds)))
              ;; The env-var is in the file bounds.
              (if (eq action t)
                  (let ((comps (complete-with-action
                                action table newstring pred))
                        (len (- envpos (car bounds))))
                    ;; Strip the part of each completion that's actually
                    ;; coming from the env-var.
                    (mapcar (lambda (s) (substring s len)) comps))
                `(boundaries
                  ,(+ envpos (- orig-length (length newstring)))
                  . ,(cdr bounds))))))))))

(defsubst pcomplete-all-entries (&optional regexp predicate)
  "Like `pcomplete-entries', but doesn't ignore any entries."
  (let (pcomplete-file-ignore
	pcomplete-dir-ignore)
    (pcomplete-entries regexp predicate)))

(defsubst pcomplete-dirs (&optional regexp)
  "Complete amongst a list of directories."
  (pcomplete-entries regexp #'file-directory-p))

;; generation of completion lists

(defun pcomplete-find-completion-function (command)
  "Find the completion function to call for the given COMMAND."
  (let ((sym (intern-soft
	      (concat "pcomplete/" (symbol-name major-mode) "/" command))))
    (unless sym
      (setq sym (intern-soft (concat "pcomplete/" command))))
    (and sym (fboundp sym) sym)))

(defun pcomplete-completions ()
  "Return a list of completions for the current argument position."
  (catch 'pcomplete-completions
    (when (pcomplete-parse-arguments pcomplete-expand-before-complete)
      (if (= pcomplete-index pcomplete-last)
	  (funcall pcomplete-command-completion-function)
	(let ((sym (or (pcomplete-find-completion-function
			(funcall pcomplete-command-name-function))
		       pcomplete-default-completion-function)))
	  (ignore
	   (pcomplete-next-arg)
	   (funcall sym)))))))

(defun pcomplete-opt (options &optional prefix _no-ganging _args-follow)
  "Complete a set of OPTIONS, each beginning with PREFIX (?- by default).
PREFIX may be t, in which case no PREFIX character is necessary.
If NO-GANGING is non-nil, each option is separate (-xy is not allowed).
If ARGS-FOLLOW is non-nil, then options which take arguments may have
the argument appear after a ganged set of options.  This is how tar
behaves, for example.
Arguments NO-GANGING and ARGS-FOLLOW are currently ignored."
  (if (and (= pcomplete-index pcomplete-last)
	   (string= (pcomplete-arg) "-"))
      (let ((len (length options))
	    (index 0)
	    char choices)
	(while (< index len)
	  (setq char (aref options index))
	  (if (eq char ?\()
	      (let ((result (read-from-string options index)))
		(setq index (cdr result)))
	    (unless (memq char '(?/ ?* ?? ?.))
	      (push (char-to-string char) choices))
	    (setq index (1+ index))))
	(throw 'pcomplete-completions
	       (mapcar
                (lambda (opt)
                  (concat "-" opt))
		(pcomplete-uniquify-list choices))))
    (let ((arg (pcomplete-arg)))
      (when (and (> (length arg) 1)
		 (stringp arg)
		 (eq (aref arg 0) (or prefix ?-)))
	(pcomplete-next-arg)
	(let ((char (aref arg 1))
	      (len (length options))
	      (index 0)
	      opt-char arg-char result)
	  (while (< (1+ index) len)
	    (setq opt-char (aref options index)
		  arg-char (aref options (1+ index)))
	    (if (eq arg-char ?\()
		(setq result
		      (read-from-string options (1+ index))
		      index (cdr result)
		      result (car result))
	      (setq result nil))
	    (when (and (eq char opt-char)
		       (memq arg-char '(?\( ?/ ?* ?? ?.)))
	      (if (< pcomplete-index pcomplete-last)
		  (pcomplete-next-arg)
		(throw 'pcomplete-completions
		       (cond ((eq arg-char ?/) (pcomplete-dirs))
			     ((eq arg-char ?*) (pcomplete-executables))
			     ((eq arg-char ??) nil)
			     ((eq arg-char ?.) (pcomplete-entries))
			     ((eq arg-char ?\() (eval result t))))))
	    (setq index (1+ index))))))))

(defun pcomplete--here (&optional form stub paring form-only)
  "Complete against the current argument, if at the end.
See the documentation for `pcomplete-here'."
  (if (< pcomplete-index pcomplete-last)
      (progn
	(if (eq paring 0)
	    (setq pcomplete-seen nil)
	  (unless (eq paring t)
	    (let ((arg (pcomplete-arg)))
	      (when (stringp arg)
                (push (if paring
                          (funcall paring arg)
                        (file-truename arg))
                      pcomplete-seen)))))
	(pcomplete-next-arg)
	t)
    (when pcomplete-show-help
      (pcomplete--help)
      (throw 'pcompleted t))
    (if stub
	(setq pcomplete-stub stub))
    (if (or (eq paring t) (eq paring 0))
	(setq pcomplete-seen nil)
      (setq pcomplete-norm-func (or paring #'file-truename)))
    (unless form-only
      (run-hooks 'pcomplete-try-first-hook))
    (throw 'pcomplete-completions
           (if (functionp form)
               (funcall form)
             ;; Old calling convention, might still be used by files
             ;; byte-compiled with the older code.
             (eval form t)))))


(defmacro pcomplete-here* (&optional form stub form-only)
  "An alternate form which does not participate in argument paring."
  (declare (debug t))
  `(pcomplete-here ,form ,stub t ,form-only))

;; display support

(defun pcomplete-restore-windows ()
  "If the only window change was due to Completions, restore things."
  (if pcomplete-last-window-config
      (let* ((cbuf (get-buffer "*Completions*"))
	     (cwin (and cbuf (get-buffer-window cbuf))))
	(when (window-live-p cwin)
	  (bury-buffer cbuf)
	  (set-window-configuration pcomplete-last-window-config))))
  (setq pcomplete-last-window-config nil
	pcomplete-window-restore-timer nil))

(define-obsolete-function-alias 'pcomplete-event-matches-key-specifier-p
  #'eq "27.1")

(define-obsolete-function-alias 'pcomplete-read-event #'read-event "27.1")

(defun pcomplete-show-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer."
  (when pcomplete-window-restore-timer
    (cancel-timer pcomplete-window-restore-timer)
    (setq pcomplete-window-restore-timer nil))
  (unless pcomplete-last-window-config
    (setq pcomplete-last-window-config (current-window-configuration)))
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list completions))
  (minibuffer-message "Hit space to flush")
  (let (event)
    (prog1
        (catch 'done
          (while (with-current-buffer (get-buffer "*Completions*")
                   (setq event (read-event)))
            (cond
             ((eq event ?\s)
              (set-window-configuration pcomplete-last-window-config)
              (setq pcomplete-last-window-config nil)
              (throw 'done nil))
             ((or (eq event 'tab)
                  ;; Needed on a terminal
                  (eq event 9))
              (let ((win (or (get-buffer-window "*Completions*" 0)
                             (display-buffer "*Completions*"
                                             'not-this-window))))
                (with-selected-window win
                  (if (pos-visible-in-window-p (point-max))
                      (goto-char (point-min))
                    (scroll-up))))
              (message ""))
             (t
              (push event unread-command-events)
              (throw 'done nil)))))
      (if (and pcomplete-last-window-config
               pcomplete-restore-window-delay)
          (setq pcomplete-window-restore-timer
                (run-with-timer pcomplete-restore-window-delay nil
                                #'pcomplete-restore-windows))))))

;; insert completion at point

(defun pcomplete-insert-entry (stub entry &optional addsuffix raw-p)
  "Insert a completion entry at point.
Returns non-nil if a space was appended at the end."
  (let ((here (point)))
    (if (not completion-ignore-case)
	(insert-and-inherit (if raw-p
				(substring entry (length stub))
			      (comint-quote-filename
			       (substring entry (length stub)))))
      ;; the stub is not quoted at this time, so to determine the
      ;; length of what should be in the buffer, we must quote it
      ;; FIXME: Here we presume that quoting `stub' gives us the exact
      ;; text in the buffer before point, which is not guaranteed;
      ;; e.g. it is not the case in eshell when completing ${FOO}tm[TAB].
      (delete-char (- (length (comint-quote-filename stub))))
      ;; if there is already a backslash present to handle the first
      ;; character, don't bother quoting it
      (when (eq (char-before) ?\\)
	(insert-and-inherit (substring entry 0 1))
	(setq entry (substring entry 1)))
      (insert-and-inherit (if raw-p
			      entry
			    (comint-quote-filename entry))))
    (let (space-added)
      (when (and (not (memq (char-before) pcomplete-suffix-list))
		 addsuffix)
	(insert-and-inherit pcomplete-termination-string)
	(setq space-added t))
      (setq pcomplete-last-completion-length (- (point) here)
	    pcomplete-last-completion-stub stub)
      space-added)))

;; Selection of completions.

(defun pcomplete-do-complete (stub completions)
  "Dynamically complete at point using STUB and COMPLETIONS.
This is basically just a wrapper for `pcomplete-stub' which does some
extra checking, and munging of the COMPLETIONS list."
  (unless (stringp stub)
    (message "Cannot complete argument")
    (throw 'pcompleted nil))
  (if (null completions)
      (ignore
       (if (and stub (> (length stub) 0))
	   (message "No completions of %s" stub)
	 (message "No completions")))
    ;; pare it down, if applicable
    (when (and pcomplete-use-paring pcomplete-seen)
      (setq pcomplete-seen
            (mapcar #'directory-file-name pcomplete-seen))
      (dolist (p pcomplete-seen)
        (add-to-list 'pcomplete-seen
                     (funcall pcomplete-norm-func p)))
      (setq completions
            (apply-partially #'completion-table-with-predicate
                             completions
                             (when pcomplete-seen
                               (lambda (f)
                                 (not (member
                                       (funcall pcomplete-norm-func
                                                (directory-file-name f))
                                       pcomplete-seen))))
                             'strict)))
    ;; OK, we've got a list of completions.
    (if pcomplete-show-list
        ;; FIXME: pay attention to boundaries.
	(pcomplete-show-completions (all-completions stub completions))
      (pcomplete-stub stub completions))))

(defun pcomplete-stub (stub candidates &optional cycle-p)
  "Dynamically complete STUB from CANDIDATES list.
This function inserts completion characters at point by completing
STUB from the strings in CANDIDATES.  A completions listing may be
shown in a help buffer if completion is ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the matches.
Returns `partial' if completed as far as possible with the matches.
Returns `listed' if a completion listing was shown."
  (let* ((completion-ignore-case completion-ignore-case)
	 (completions (all-completions stub candidates))
         (entry (try-completion stub candidates))
         result)
    (cond
     ((null entry)
      (if (and stub (> (length stub) 0))
          (message "No completions of %s" stub)
        (message "No completions")))
     ((eq entry t)
      (setq entry stub)
      (message "Sole completion")
      (setq result 'sole))
     ((= 1 (length completions))
      (setq result 'sole))
     ((and pcomplete-cycle-completions
           (or cycle-p
               (not pcomplete-cycle-cutoff-length)
               (<= (length completions)
                   pcomplete-cycle-cutoff-length)))
      (let ((bound (car (completion-boundaries stub candidates nil ""))))
        (unless (zerop bound)
          (setq completions (mapcar (lambda (c) (concat (substring stub 0 bound) c))
                                    completions)))
        (setq entry (car completions)
              pcomplete-current-completions completions)))
     ((and pcomplete-recexact
           (string-equal stub entry)
           (member entry completions))
      ;; It's not unique, but user wants shortest match.
      (message "Completed shortest")
      (setq result 'shortest))
     ((or pcomplete-autolist
          (string-equal stub entry))
      ;; It's not unique, list possible completions.
      ;; FIXME: pay attention to boundaries.
      (pcomplete-show-completions completions)
      (setq result 'listed))
     (t
      (message "Partially completed")
      (setq result 'partial)))
    (cons result entry)))

;; context sensitive help

(defun pcomplete--help ()
  "Produce context-sensitive help for the current argument.
If specific documentation can't be given, be generic."
  (if (and pcomplete-help
	   (or (and (stringp pcomplete-help)
		    (fboundp 'Info-goto-node))
	       (listp pcomplete-help)))
      (if (listp pcomplete-help)
	  (message "%s" (eval pcomplete-help t))
	(save-window-excursion (info))
	(declare-function Info-goto-node
	                  "info" (nodename &optional fork strict-case))
	(switch-to-buffer-other-window "*info*")
	(funcall #'Info-goto-node pcomplete-help))
    (if pcomplete-man-function
	(let ((cmd (funcall pcomplete-command-name-function)))
	  (if (and cmd (> (length cmd) 0))
	      (funcall pcomplete-man-function cmd)))
      (message "No context-sensitive help available"))))

;; general utilities

(defun pcomplete-uniquify-list (l)
  "Sort and remove multiples in L."
  (setq l (sort l #'string-lessp))
  (seq-uniq l))
(define-obsolete-function-alias 'pcomplete-uniqify-list #'pcomplete-uniquify-list "27.1")

(defun pcomplete-process-result (cmd &rest args)
  "Call CMD using `call-process' and return the simplest result."
  (with-temp-buffer
    (apply #'call-process cmd nil t nil args)
    (skip-chars-backward "\n")
    (buffer-substring (point-min) (point))))

;; hostname completion

(defvar pcomplete--host-name-cache nil
  "A cache the names of frequently accessed hosts.")

(defvar pcomplete--host-name-cache-timestamp nil
  "A timestamp of when the hosts file was read.")

(defun pcomplete-read-hosts-file (filename)
  "Read in the hosts from FILENAME, default `pcomplete-hosts-file'."
  (let (hosts)
    (with-temp-buffer
      (insert-file-contents (or filename pcomplete-hosts-file))
      (goto-char (point-min))
      (while (re-search-forward
              ;; "^ \t\\([^# \t\n]+\\)[ \t]+\\([^ \t\n]+\\)\\([ \t]*\\([^ \t\n]+\\)\\)?"
              "^[ \t]*\\([^# \t\n]+\\)[ \t]+\\([^ \t\n].+\\)" nil t)
        (push (cons (match-string 1)
                    (split-string (match-string 2)))
              hosts)))
    (nreverse hosts)))

(defun pcomplete-read-hosts (file result-var timestamp-var)
  "Read the contents of /etc/hosts for host names."
  (if (or (not (symbol-value result-var))
          (not (symbol-value timestamp-var))
          (time-less-p
           (symbol-value timestamp-var)
           (file-attribute-modification-time (file-attributes file))))
      (progn
        (set result-var (apply #'nconc (pcomplete-read-hosts-file file)))
        (set timestamp-var (current-time))))
  (symbol-value result-var))

(defun pcomplete-read-host-names ()
  "Read the contents of /etc/hosts for host names."
  (if pcomplete-hosts-file
      (pcomplete-read-hosts pcomplete-hosts-file 'pcomplete--host-name-cache
                   'pcomplete--host-name-cache-timestamp)))

;;; Parsing help messages

(defvar pcomplete-from-help (make-hash-table :test #'equal)
  "Memoization table for function `pcomplete-from-help'.")

(cl-defun pcomplete-from-help (command
                               &rest args
                               &key
                               (margin (rx bol (+ " ")))
                               (argument (rx "-" (+ (any "-" alnum)) (? "=")))
                               (metavar (rx (? " ")
                                            (or (+ (any alnum "_-"))
                                                (seq "[" (+? nonl) "]")
                                                (seq "<" (+? nonl) ">")
                                                (seq "{" (+? nonl) "}"))))
                               (separator (rx ", " symbol-start))
                               (description (rx (* nonl)
                                                (* "\n" (>= 9 " ") (* nonl))))
                               narrow-start
                               narrow-end)
  "Parse output of COMMAND into a list of completion candidates.

COMMAND can be a string to be executed in a shell or a list of
strings (program name and arguments).  It should print a help
message.

A list of arguments is collected after each match of MARGIN.
Each argument should match ARGUMENT, possibly followed by a match
of METAVAR.  If a match of SEPARATOR follows, then more
argument-metavar pairs are collected.  Finally, a match of
DESCRIPTION is collected.

Keyword ARGS:

MARGIN: regular expression after which argument descriptions are
  to be found.  Parsing continues at the end of the first match
  group or, failing that, the entire match.

ARGUMENT: regular expression matching an argument name.  The
  first match group (failing that, the entire match) is collected
  as the argument name.  Parsing continues at the end of the
  second matching group (failing that, the first group or entire
  match).

METAVAR: regular expression matching an argument parameter name.
  The first match group (failing that, the entire match) is
  collected as the parameter name and used as completion
  annotation.  Parsing continues at the end of the second
  matching group (failing that, the first group or entire match).

SEPARATOR: regular expression matching the separator between
  arguments.  Parsing continues at the end of the first match
  group (failing that, the entire match).

DESCRIPTION: regular expression matching the description of an
  argument.  The first match group (failing that, the entire
  match) is collected as the parameter name and used as
  completion help.  Parsing continues at the end of the first
  matching group (failing that, the entire match).

NARROW-START, NARROW-END: if non-nil, parsing of the help message
  is narrowed to the region between the end of the first match
  group (failing that, the entire match) of these regular
  expressions."
  (with-memoization (gethash (cons command args) pcomplete-from-help)
    (with-temp-buffer
      (let ((case-fold-search nil)
            (default-directory (expand-file-name "~/"))
            (command (if (stringp command)
                         (list shell-file-name
                               shell-command-switch
                               command)
                       command))
            i result)
        (apply #'call-process (car command) nil t nil (cdr command))
        (goto-char (point-min))
        (narrow-to-region (or (and narrow-start
                                   (re-search-forward narrow-start nil t)
                                   (or (match-beginning 1) (match-beginning 0)))
                              (point-min))
                          (or (and narrow-end
                                   (re-search-forward narrow-end nil t)
                                   (or (match-beginning 1) (match-beginning 0)))
                              (point-max)))
        (goto-char (point-min))
        (while (re-search-forward margin nil t)
          (goto-char (or (match-end 1) (match-end 0)))
          (setq i 0)
          (while (and (or (zerop i)
                          (and (looking-at separator)
                               (goto-char (or (match-end 1)
                                              (match-end 0)))))
                      (looking-at argument))
            (setq i (1+ i))
            (goto-char (seq-some #'match-end '(2 1 0)))
            (push (or (match-string 1) (match-string 0)) result)
            (when (looking-at metavar)
              (goto-char (seq-some #'match-end '(2 1 0)))
              (put-text-property 0 1
                                 'pcomplete-annotation
                                 (or (match-string 1) (match-string 0))
                                 (car result))))
          (when (looking-at description)
            (goto-char (seq-some #'match-end '(2 1 0)))
            (let ((help (string-clean-whitespace
                         (or (match-string 1) (match-string 0))))
                  (items (take i result)))
              (while items
                (put-text-property 0 1 'pcomplete-help help
                                   (pop items))))))
        (nreverse result)))))

(defun pcomplete-here-using-help (command &rest args)
  "Perform completion for a simple command.
Offer switches and directory entries as completion candidates.
The switches are obtained by calling `pcomplete-from-help' with
COMMAND and ARGS as arguments."
  (while (cond
          ((string= "--" (pcomplete-arg 1))
           (while (pcomplete-here (pcomplete-entries))))
          ((pcomplete-match "\\`--[^=]+=\\(.*\\)" 0)
           (pcomplete-here (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
          ((string-prefix-p "-" (pcomplete-arg 0))
           (pcomplete-here (apply #'pcomplete-from-help command args)))
          (t (pcomplete-here* (pcomplete-entries))))))

(provide 'pcomplete)

;;; pcomplete.el ends here
