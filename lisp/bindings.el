;;; bindings.el --- define standard key bindings and some variables.

;; Copyright (C) 1985,86,87,92,93,94,95,96,99 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; Special formatting conventions are used in this file!
;;;
;;; a backslash-newline is used at the beginning of a documentation string
;;; when that string should be stored in the file etc/DOCnnn, not in core.
;;;
;;; Such strings read into Lisp as numbers (during the pure-loading phase).
;;;
;;; But you must obey certain rules to make sure the string is understood
;;; and goes into etc/DOCnnn properly.
;;;
;;; The doc string must appear in the standard place in a call to
;;; defun, autoload, defvar or defconst.  No Lisp macros are recognized.
;;; The open-paren starting the definition must appear in column 0.
;;;
;;; In defvar and defconst, there is an additional rule:
;;; The double-quote that starts the string must be on the same
;;; line as the defvar or defconst.
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; Code:

(defconst mode-line-mule-info
  (purecopy '(""
	      (current-input-method ("" current-input-method-title))
	      "%Z"))
  "Mode-line control for displaying information of multilingual environment.
Normally it displays current input method (if any activated) and
mnemonics of the following coding systems:
  coding system for saving or writing the current buffer
  coding system for keyboard input (if Emacs is running on terminal)
  coding system for terminal output (if Emacs is running on terminal)"
;; Currently not:
;;;  coding system for decoding output of buffer process (if any)
;;;  coding system for encoding text to send to buffer process (if any)."
)

(make-variable-buffer-local 'mode-line-mule-info)

(defvar mode-line-buffer-identification (purecopy '("%12b")) "\
Mode-line control for identifying the buffer being displayed.
Its default value is (\"%12b\").
Major modes that edit things other than ordinary files may change this
\(e.g. Info, Dired,...)")

(make-variable-buffer-local 'mode-line-buffer-identification)

(defvar mode-line-frame-identification '("-%F  "))

(defvar mode-line-process nil "\
Mode-line control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")

(make-variable-buffer-local 'mode-line-process)

(defconst mode-line-modified
  (let ((s "%1*%1+")
	(map (make-sparse-keymap)))
    (define-key map [mode-line mouse-2]
      (lambda (event)
	(interactive "e")
	(save-selected-window
	  (select-window (posn-window (event-start event)))
	  (let ((binding (key-binding "\C-x\C-q")))
	    (if binding
		(funcall binding)
	      (toggle-read-only))))))
    (set-text-properties 0 (length s)
			 (list 'help-echo
			       "Read-only status: mouse-2 toggles it"
			       'local-map map)
			 s)
    (list s))
  "Mode-line control for displaying whether current buffer is modified.")

(make-variable-buffer-local 'mode-line-modified)

(setq-default mode-line-format
  (list (purecopy "-")
   'mode-line-mule-info
   'mode-line-modified
   'mode-line-frame-identification
   'mode-line-buffer-identification
   (purecopy "   ")
   'global-mode-string
   (purecopy "   %[(")
   '(:eval (mode-line-mode-name)) 'mode-line-process 'minor-mode-alist
   (purecopy "%n")
   (purecopy ")%]--")
   '(which-func-mode ("" which-func-format "--"))
   (purecopy '(line-number-mode "L%l--"))
   (purecopy '(column-number-mode "C%c--"))
   (purecopy '(-3 . "%p"))
   (purecopy "-%-")))

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line iff VARIABLE's value is non-nil.

Actually, STRING need not be a string; any possible mode-line element
is okay.  See `mode-line-format'.")
;; Don't use purecopy here--some people want to change these strings.
(setq minor-mode-alist '((abbrev-mode " Abbrev")
			 (overwrite-mode overwrite-mode)
			 (auto-fill-function " Fill")
			 ;; not really a minor mode...
			 (defining-kbd-macro " Def")))

(defvar mode-line-buffer-identification-keymap nil "\
Keymap for what is displayed by `mode-line-buffer-identification'.")

(defvar mode-line-minor-mode-keymap nil "\
Keymap for what is displayed by `mode-line-mode-name'.")

(defvar mode-line-mode-menu-keymap nil "\
Keymap for mode operations menu in the mode line.")

(defun mode-line-unbury-buffer () "\
Switch to the last buffer in the buffer list that is not hidden."
  (interactive)
  (let ((list (reverse (buffer-list))))
    (while (eq (aref (buffer-name (car list)) 0) ? )
      (setq list (cdr list)))
    (switch-to-buffer (car list))))

(defun mode-line-other-buffer () "\
Switch to the most recently selected buffer other than the current one."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun mode-line-mode-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (mode-line-mode-menu event))
	   (binding (and selection (lookup-key mode-line-mode-menu
					       (vector (car selection))))))
      (if binding
	  (call-interactively binding)))))

(defun mode-line-mode-name () "\
Return a string to display in the mode line for the current mode name."
  (let (length (result mode-name))
    (when mode-line-mouse-sensitive-p
      (let ((local-map (get-text-property 0 'local-map result))
	    (help-echo (get-text-property 0 'help-echo result)))
	(setq result (copy-sequence result))
	;; Add `local-map' property if there isn't already one.
	(when (and (null local-map)
		   (null (next-single-property-change 0 'local-map result)))
	  (put-text-property 0 (length result)
			     'local-map mode-line-minor-mode-keymap result))
	;; Add `help-echo' property if there isn't already one.
	(when (and (null help-echo)
		   (null (next-single-property-change 0 'help-echo result)))
	  (put-text-property 0 (length result)
			     'help-echo "mouse-3: minor mode menu" result))))
    result))

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(defvar mode-line-mouse-sensitive-p nil "\
Non-nil means mode line has been made mouse-sensitive.")

(defvar mode-line-mode-menu nil "\
Menu of mode operations in the mode line.")

(defun make-mode-line-mouse-sensitive ()
  (when (and window-system
	     (not mode-line-mouse-sensitive-p))
    (setq mode-line-mouse-sensitive-p t)
    (let ((map (make-sparse-keymap "Minor Modes")))
      (define-key map [abbrev-mode]
	'(menu-item "Abbrev" abbrev-mode
		    :active t :style toggle :selected abbrev-mode))
      (define-key map [auto-revert-mode]
	'(menu-item "Auto revert" auto-revert-mode
		    :active t :style toggle
		    :selected (bound-and-true-p auto-revert-mode)))
      (define-key map [auto-fill-mode]
	'(menu-item "Auto-fill" auto-fill-mode
		    :active t :style toggle :selected auto-fill-function))
      (define-key map [column-number-mode]
	'(menu-item "Column number" column-number-mode
		    :active t :style toggle :selected column-number-mode))
      (define-key map [flyspell-mode]
	'(menu-item "Flyspell" flyspell-mode
		    :active t :style toggle
		    :selected (bound-and-true-p flyspell-mode)))
      (define-key map [font-lock-mode]
	'(menu-item "Font-lock" font-lock-mode
		    :active t :style toggle :selected font-lock-mode))
      (define-key map [hide-ifdef-mode]
	'(menu-item "Hide ifdef" hide-ifdef-mode
		    :active t :style toggle
		    :selected (bound-and-true-p hide-ifdef-mode)))
      (define-key map [highlight-changes-mode]
	'(menu-item "Highlight changes" highlight-changes-mode
		    :active t :style toggle
		    :selected (bound-and-true-p highlight-changes-mode)))
      (define-key map [line-number-mode]
	'(menu-item "Line number" line-number-mode
		    :active t :style toggle :selected line-number-mode))
      (define-key map [outline-minor-mode]
	'(menu-item "Outline" outline-minor-mode
		    :active t :style toggle
		    :selected (bound-and-true-p outline-minor-mode)))
      (define-key map [overwrite-mode]
	'(menu-item "Overwrite" overwrite-mode
		    :active t :style toggle :selected overwrite-mode))
      (setq mode-line-mode-menu (copy-keymap map))
      (defun mode-line-mode-menu (event)
	(interactive "@e")
	(x-popup-menu event mode-line-mode-menu)))

    ;; Add menu of buffer operations to the buffer identification part
    ;; of the mode line.
    (let ((map (make-sparse-keymap))
	  (s (copy-sequence "%12b")))
      (define-key map [mode-line mouse-1] 'mode-line-other-buffer)
      (define-key map [header-line mouse-1] 'mode-line-other-buffer)
      (define-key map [mode-line M-mouse-2] 'mode-line-unbury-buffer)
      (define-key map [header-line M-mouse-2] 'mode-line-unbury-buffer)
      (define-key map [mode-line mouse-2] 'bury-buffer)
      (define-key map [header-line mouse-2] 'bury-buffer)
      (define-key map [mode-line down-mouse-3] 'mouse-buffer-menu)
      (define-key map [header-line down-mouse-3] 'mouse-buffer-menu)
      (setq mode-line-buffer-identification-keymap map)
      (setq-default mode-line-buffer-identification (list s))
      (put-text-property 0 (length s) 'face '(:weight bold) s)
      (put-text-property 0 (length s) 'help-echo
			 "mouse-1: other buffer, mouse-2: prev, M-mouse-2: next, mouse-3: buffer menu" s)
      (put-text-property 0 (length s) 'local-map map s))

    ;; Menu of minor modes.
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-3] 'mode-line-mode-menu-1)
      (define-key map [header-line down-mouse-3] 'mode-line-mode-menu-1)
      (setq mode-line-minor-mode-keymap map))
    
    (force-mode-line-update)))


;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (append
       (cond ((or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
	      '(".o" "~" ".bin" ".bak" ".obj" ".map"
		".a" ".ln" ".blg" ".bbl"))
	     ((eq system-type 'vax-vms)
	      '(".obj" ".exe" ".bin" ".lbin" ".sbin"
		".brn" ".rnt" ".mem" ".lni" ".lis"
		".olb" ".tlb" ".mlb" ".hlb"))
	     (t
	      '(".o" "~" ".bin" ".lbin" ".fasl" ".ufsl"
		".a" ".ln" ".blg" ".bbl")))
       '(".elc" ".lof"
	 ".glo" ".idx" ".lot"
	 ;; TeX-related
	 ".dvi" ".fmt" ".tfm" ".pdf"
	 ;; Java compiled
	 ".class"
	 ;; Clisp
	 ".fas" ".lib"
	 ;; CMUCL
	 ".x86f" ".sparcf"
	 ;; Texinfo-related
	 ".toc" ".log" ".aux"
	 ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
	 ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs")))

(setq debug-ignored-errors
      '(beginning-of-line beginning-of-buffer end-of-line
	end-of-buffer end-of-file buffer-read-only
	file-supersession
      	"^Previous command was not a yank$"
	"^Minibuffer window is not active$"
	"^End of history; no next item$"
	"^Beginning of history; no preceding item$"
	"^No recursive edit is in progress$"
	"^Changes to be undone are outside visible portion of buffer$"
	"^No undo information in this buffer$"
	"^No further undo information$"
	"^Save not confirmed$"
	"^Recover-file cancelled\\.$"
	"^Cannot switch buffers in a dedicated window$"

	;; comint
	"^Not at command line$"
	"^Empty input ring$"
	"^No history$"
	"^Not found$";; To common?
	"^Current buffer has no process$"

	;; dabbrev
	"^No dynamic expansion for .* found$"
	"^No further dynamic expansion for .* found$"
	"^No possible abbreviation preceding point$"

	;; Completion
	"^To complete, the point must be after a symbol at least [0-9]* character long\\.$"
	"^The string \".*\" is too short to be saved as a completion\\.$"

	;; Compile
	"^No more errors\\( yet\\|\\)$"

	;; Gnus
	"^NNTP: Connection closed\\.$"

	;; info
	"^Node has no Previous$"
	"^No menu in this node$"
	"^Node has no Next$"
	"^No \".*\" in index$"

	;; imenu
	"^No items suitable for an index found in this buffer\\.$"
	"^This buffer cannot use `imenu-default-create-index-function'$"
	"^The mode `.*' does not support Imenu$"

	;; ispell
	"^No word found to check!$"

	;; mh-e
	"^Cursor not pointing to message$"
	"^There is no other window$"

	;; man
	"^No manpage [0-9]* found$"
	"^Can't find the .* manpage$"

	;; etags
	"^No tags table in use; use .* to select one$"
	"^There is no default tag$"
	"^No previous tag locations$"
	"^File .* is not a valid tags table$"
	"^No \\(more \\|\\)tags \\(matching\\|containing\\) "
	"^Rerun etags: `.*' not found in "
	"^All files processed$"
	"^No .* or .* in progress$"
	"^File .* not in current tags tables$"
	"^No tags table loaded"
	"^Nothing to complete$"
	
	;; ediff
	"^Errors in diff output. Diff output is in "
	"^Hmm... I don't see an Ediff command around here...$"
	"^Undocumented command! Type `G' in Ediff Control Panel to drop a note to the Ediff maintainer$"
	": This command runs in Ediff Control Buffer only!$"
	": Invalid op in ediff-check-version$"
	"^ediff-shrink-window-C can be used only for merging jobs$"
	"^Lost difference info on these directories$"
	"^This command is inapplicable in the present context$"
	"^This session group has no parent$"
	"^Can't hide active session, $"
	"^Ediff: something wrong--no multiple diffs buffer$"
	"^Can't make context diff for Session $"
	"^The patch buffer wasn't found$"
	"^Aborted$"
	"^This Ediff session is not part of a session group$"
	"^No active Ediff sessions or corrupted session registry$"
	"^No session info in this line$"
	"^`.*' is not an ordinary file$"
	"^Patch appears to have failed$"
	"^Recomputation of differences cancelled$"
	"^No fine differences in this mode$"
	"^Lost connection to ancestor buffer...sorry$"
	"^Not merging with ancestor$"
	"^Don't know how to toggle read-only in buffer "
	"Emacs is not running as a window application$"
	"^This command makes sense only when merging with an ancestor$"
	"^At end of the difference list$"
	"^At beginning of the difference list$"
	"^Nothing saved for diff .* in buffer "
	"^Buffer is out of sync for file "
	"^Buffer out of sync for file "
	"^Output from `diff' not found$"
	"^You forgot to specify a region in buffer "
	"^All right. Make up your mind and come back...$"
	"^Current buffer is not visiting any file$"
	"^Failed to retrieve revision: $"
	"^Can't determine display width.$"
	"^File `.*' does not exist or is not readable$"
	"^File `.*' is a directory$"
	"^Buffer .* doesn't exist$"
	"^Directories . and . are the same: "
	"^Directory merge aborted$"
	"^Merge of directory revisions aborted$"
	"^Buffer .* doesn't exist$"
	"^There is no file to merge$"
	"^Version control package .*.el not found. Use vc.el instead$"
	
	;; cus-edit
	"^No user options have changed defaults in recent Emacs versions$"

	;; BBDB
	"^no previous record$"
	"^no next record$"))


(make-variable-buffer-local 'indent-tabs-mode)

;; We have base64 functions built in now.
(add-to-list 'features 'base64)

(define-key esc-map "\t" 'complete-symbol)

(defun complete-symbol (arg) "\
Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see).

With a prefix argument, this command does completion within
the collection of symbols listed in the index of the manual for the
language you are using."
  (interactive "P")
  (if arg
      (info-complete-symbol)
    (if (fboundp 'complete-tag)
	(complete-tag)
      ;; Don't autoload etags if we have no tags table.
      (error (substitute-command-keys
	      "No tags table loaded; use \\[visit-tags-table] to load one")))))

;; Reduce total amount of space we must allocate during this function
;; that we will not need to keep permanently.
(garbage-collect)

;; Make all multibyte characters self-insert.
(let ((l (generic-character-list))
      (table (nth 1 global-map)))
  (while l
    (set-char-table-default table (car l) 'self-insert-command)
    (setq l (cdr l))))

(setq help-event-list '(help f1))

(make-variable-buffer-local 'minor-mode-overriding-map-alist)

;These commands are defined in editfns.c
;but they are not assigned to keys there.
(put 'narrow-to-region 'disabled t)
(define-key ctl-x-map "nn" 'narrow-to-region)
(define-key ctl-x-map "nw" 'widen)
;; (define-key ctl-x-map "n" 'narrow-to-region)
;; (define-key ctl-x-map "w" 'widen)

(define-key global-map "\C-j" 'newline-and-indent)
(define-key global-map "\C-m" 'newline)
(define-key global-map "\C-o" 'open-line)
(define-key esc-map "\C-o" 'split-line)
(define-key global-map "\C-q" 'quoted-insert)
(define-key esc-map "^" 'delete-indentation)
(define-key esc-map "\\" 'delete-horizontal-space)
(define-key esc-map "m" 'back-to-indentation)
(define-key ctl-x-map "\C-o" 'delete-blank-lines)
(define-key esc-map " " 'just-one-space)
(define-key esc-map "z" 'zap-to-char)
(define-key esc-map "=" 'count-lines-region)
(define-key ctl-x-map "=" 'what-cursor-position)
(define-key esc-map ":" 'eval-expression)
;; Define ESC ESC : like ESC : for people who type ESC ESC out of habit.
(define-key esc-map "\M-:" 'eval-expression)
;; Changed from C-x ESC so that function keys work following C-x.
(define-key ctl-x-map "\e\e" 'repeat-complex-command)
;; New binding analogous to M-:.
(define-key ctl-x-map "\M-:" 'repeat-complex-command)
(define-key ctl-x-map "u" 'advertised-undo)
;; Many people are used to typing C-/ on X terminals and getting C-_.
(define-key global-map [?\C-/] 'undo)
(define-key global-map "\C-_" 'undo)
(define-key esc-map "!" 'shell-command)
(define-key esc-map "|" 'shell-command-on-region)

;; This is an experiment--make up and down arrows do history.
(define-key minibuffer-local-map [up] 'previous-history-element)
(define-key minibuffer-local-map [down] 'next-history-element)
(define-key minibuffer-local-ns-map [up] 'previous-history-element)
(define-key minibuffer-local-ns-map [down] 'next-history-element)
(define-key minibuffer-local-completion-map [up] 'previous-history-element)
(define-key minibuffer-local-completion-map [down] 'next-history-element)
(define-key minibuffer-local-must-match-map [up] 'previous-history-element)
(define-key minibuffer-local-must-match-map [down] 'next-history-element)

(define-key global-map "\C-u" 'universal-argument)
(let ((i ?0))
  (while (<= i ?9)
    (define-key esc-map (char-to-string i) 'digit-argument)
    (setq i (1+ i))))
(define-key esc-map "-" 'negative-argument)
;; Define control-digits.
(let ((i ?0))
  (while (<= i ?9)
    (define-key global-map (read (format "[?\\C-%c]" i)) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map [?\C--] 'negative-argument)
;; Define control-meta-digits.
(let ((i ?0))
  (while (<= i ?9)
    (define-key esc-map (read (format "[?\\C-%c]" i)) 'digit-argument)
    (setq i (1+ i))))
(define-key global-map [?\C-\M--] 'negative-argument)

(define-key global-map "\C-k" 'kill-line)
(define-key global-map "\C-w" 'kill-region)
(define-key esc-map "w" 'kill-ring-save)
(define-key esc-map "\C-w" 'append-next-kill)
(define-key global-map "\C-y" 'yank)
(define-key esc-map "y" 'yank-pop)

;; (define-key ctl-x-map "a" 'append-to-buffer)

(define-key global-map "\C-@" 'set-mark-command)
;; Many people are used to typing C-SPC and getting C-@.
(define-key global-map [?\C- ] 'set-mark-command)
(define-key ctl-x-map "\C-x" 'exchange-point-and-mark)
(define-key ctl-x-map "\C-@" 'pop-global-mark)
(define-key ctl-x-map [?\C- ] 'pop-global-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
(define-key ctl-x-map "\C-n" 'set-goal-column)

;;(defun function-key-error ()
;;  (interactive)
;;  (error "That function key is not bound to anything."))

(define-key global-map [menu] 'execute-extended-command)
(define-key global-map [find] 'search-forward)

;; natural bindings for terminal keycaps --- defined in X keysym order
(define-key global-map [home]		'beginning-of-buffer)
(define-key global-map [M-home]		'beginning-of-buffer-other-window)
(define-key global-map [left]		'backward-char)
(define-key global-map [up]		'previous-line)
(define-key global-map [right]		'forward-char)
(define-key global-map [down]		'next-line)
(define-key global-map [prior]		'scroll-down)
(define-key global-map [next]		'scroll-up)
(define-key global-map [C-up]		'backward-paragraph)
(define-key global-map [C-down]		'forward-paragraph)
(define-key global-map [C-prior]	'scroll-right)
(define-key global-map [C-next]		'scroll-left)
(define-key global-map [M-next]		'scroll-other-window)
(define-key global-map [M-prior]	'scroll-other-window-down)
(define-key global-map [end]		'end-of-buffer)
(define-key global-map [M-end]		'end-of-buffer-other-window)
(define-key global-map [begin]		'beginning-of-buffer)
(define-key global-map [M-begin]	'beginning-of-buffer-other-window)
;; (define-key global-map [select]	'function-key-error)
;; (define-key global-map [print]	'function-key-error)
(define-key global-map [execute]	'execute-extended-command)
(define-key global-map [insert]		'overwrite-mode)
(define-key global-map [C-insert]	'kill-ring-save)
(define-key global-map [S-insert]	'yank)
(define-key global-map [undo]		'undo)
(define-key global-map [redo]		'repeat-complex-command)
;; (define-key global-map [clearline]	'function-key-error)
(define-key global-map [insertline]	'open-line)
(define-key global-map [deleteline]	'kill-line)
;; (define-key global-map [insertchar]	'function-key-error)
(define-key global-map [deletechar]	'delete-char)
;; (define-key global-map [backtab]	'function-key-error)
;; (define-key global-map [f1]		'function-key-error)
;; (define-key global-map [f2]		'function-key-error)
;; (define-key global-map [f3]		'function-key-error)
;; (define-key global-map [f4]		'function-key-error)
;; (define-key global-map [f5]		'function-key-error)
;; (define-key global-map [f6]		'function-key-error)
;; (define-key global-map [f7]		'function-key-error)
;; (define-key global-map [f8]		'function-key-error)
;; (define-key global-map [f9]		'function-key-error)
;; (define-key global-map [f10]		'function-key-error)
;; (define-key global-map [f11]		'function-key-error)
;; (define-key global-map [f12]		'function-key-error)
;; (define-key global-map [f13]		'function-key-error)
;; (define-key global-map [f14]		'function-key-error)
;; (define-key global-map [f15]		'function-key-error)
;; (define-key global-map [f16]		'function-key-error)
;; (define-key global-map [f17]		'function-key-error)
;; (define-key global-map [f18]		'function-key-error)
;; (define-key global-map [f19]		'function-key-error)
;; (define-key global-map [f20]		'function-key-error)
;; (define-key global-map [f21]		'function-key-error)
;; (define-key global-map [f22]		'function-key-error)
;; (define-key global-map [f23]		'function-key-error)
;; (define-key global-map [f24]		'function-key-error)
;; (define-key global-map [f25]		'function-key-error)
;; (define-key global-map [f26]		'function-key-error)
;; (define-key global-map [f27]		'function-key-error)
;; (define-key global-map [f28]		'function-key-error)
;; (define-key global-map [f29]		'function-key-error)
;; (define-key global-map [f30]		'function-key-error)
;; (define-key global-map [f31]		'function-key-error)
;; (define-key global-map [f32]		'function-key-error)
;; (define-key global-map [f33]		'function-key-error)
;; (define-key global-map [f34]		'function-key-error)
;; (define-key global-map [f35]		'function-key-error)
;; (define-key global-map [kp-backtab]	'function-key-error)
;; (define-key global-map [kp-space]	'function-key-error)
;; (define-key global-map [kp-tab]		'function-key-error)
;; (define-key global-map [kp-enter]	'function-key-error)
;; (define-key global-map [kp-f1]		'function-key-error)
;; (define-key global-map [kp-f2]		'function-key-error)
;; (define-key global-map [kp-f3]		'function-key-error)
;; (define-key global-map [kp-f4]		'function-key-error)
;; (define-key global-map [kp-multiply]	'function-key-error)
;; (define-key global-map [kp-add]		'function-key-error)
;; (define-key global-map [kp-separator]	'function-key-error)
;; (define-key global-map [kp-subtract]	'function-key-error)
;; (define-key global-map [kp-decimal]	'function-key-error)
;; (define-key global-map [kp-divide]	'function-key-error)
;; (define-key global-map [kp-0]		'function-key-error)
;; (define-key global-map [kp-1]		'function-key-error)
;; (define-key global-map [kp-2]		'function-key-error)
;; (define-key global-map [kp-3]		'function-key-error)
;; (define-key global-map [kp-4]		'function-key-error)
;; (define-key global-map [kp-5]		'recenter)
;; (define-key global-map [kp-6]		'function-key-error)
;; (define-key global-map [kp-7]		'function-key-error)
;; (define-key global-map [kp-8]		'function-key-error)
;; (define-key global-map [kp-9]		'function-key-error)
;; (define-key global-map [kp-equal]	'function-key-error)

;; X11R6 distinguishes these keys from the non-kp keys.
;; Make them behave like the non-kp keys unless otherwise bound.
(define-key function-key-map [kp-home] [home])
(define-key function-key-map [kp-left] [left])
(define-key function-key-map [kp-up] [up])
(define-key function-key-map [kp-right] [right])
(define-key function-key-map [kp-down] [down])
(define-key function-key-map [kp-prior] [prior])
(define-key function-key-map [kp-next] [next])
(define-key function-key-map [M-kp-next] [M-next])
(define-key function-key-map [kp-end] [end])
(define-key function-key-map [kp-begin] [begin])
(define-key function-key-map [kp-insert] [insert])
(define-key function-key-map [kp-delete] [delete])

(define-key global-map [mouse-movement] 'ignore)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'indent-for-comment)
(define-key esc-map "j" 'indent-new-comment-line)
(define-key esc-map "\C-j" 'indent-new-comment-line)
(define-key ctl-x-map ";" 'set-comment-column)
(define-key ctl-x-map "f" 'set-fill-column)
(define-key ctl-x-map "$" 'set-selective-display)

(define-key esc-map "@" 'mark-word)
(define-key esc-map "f" 'forward-word)
(define-key esc-map "b" 'backward-word)
(define-key esc-map "d" 'kill-word)
(define-key esc-map "\177" 'backward-kill-word)

(define-key esc-map "<" 'beginning-of-buffer)
(define-key esc-map ">" 'end-of-buffer)
(define-key ctl-x-map "h" 'mark-whole-buffer)
(define-key esc-map "\\" 'delete-horizontal-space)

(defalias 'mode-specific-command-prefix (make-sparse-keymap))
(defvar mode-specific-map (symbol-function 'mode-specific-command-prefix)
  "Keymap for characters following C-c.")
(define-key global-map "\C-c" 'mode-specific-command-prefix)

(global-set-key [M-right]  'forward-word)
(global-set-key [M-left]   'backward-word)
;; ilya@math.ohio-state.edu says these bindings are standard on PC editors.
(global-set-key [C-right]  'forward-word)
(global-set-key [C-left]   'backward-word)
;; This is not quite compatible, but at least is analogous
(global-set-key [C-delete]   'backward-kill-word)
;; This is "move to the clipboard", or as close as we come.
(global-set-key [S-delete] 'kill-region)

(define-key esc-map "\C-f" 'forward-sexp)
(define-key esc-map "\C-b" 'backward-sexp)
(define-key esc-map "\C-u" 'backward-up-list)
(define-key esc-map "\C-@" 'mark-sexp)
(define-key esc-map [?\C-\ ] 'mark-sexp)
(define-key esc-map "\C-d" 'down-list)
(define-key esc-map "\C-k" 'kill-sexp)
(define-key global-map [C-M-delete] 'backward-kill-sexp)
(define-key global-map [C-M-backspace] 'backward-kill-sexp)
(define-key esc-map "\C-n" 'forward-list)
(define-key esc-map "\C-p" 'backward-list)
(define-key esc-map "\C-a" 'beginning-of-defun)
(define-key esc-map "\C-e" 'end-of-defun)
(define-key esc-map "\C-h" 'mark-defun)
(define-key ctl-x-map "nd" 'narrow-to-defun)
(define-key esc-map "(" 'insert-parentheses)
(define-key esc-map ")" 'move-past-close-and-reindent)

(define-key ctl-x-map "\C-e" 'eval-last-sexp)

(define-key ctl-x-map "m" 'compose-mail)
(define-key ctl-x-4-map "m" 'compose-mail-other-window)
(define-key ctl-x-5-map "m" 'compose-mail-other-frame)

(define-key ctl-x-map "r\C-@" 'point-to-register)
(define-key ctl-x-map [?r ?\C-\ ] 'point-to-register)
(define-key ctl-x-map "r " 'point-to-register)
(define-key ctl-x-map "rj" 'jump-to-register)
(define-key ctl-x-map "rs" 'copy-to-register)
(define-key ctl-x-map "rx" 'copy-to-register)
(define-key ctl-x-map "ri" 'insert-register)
(define-key ctl-x-map "rg" 'insert-register)
(define-key ctl-x-map "rr" 'copy-rectangle-to-register)
(define-key ctl-x-map "rn" 'number-to-register)
(define-key ctl-x-map "r+" 'increment-register)
(define-key ctl-x-map "rc" 'clear-rectangle)
(define-key ctl-x-map "rk" 'kill-rectangle)
(define-key ctl-x-map "rd" 'delete-rectangle)
(define-key ctl-x-map "ry" 'yank-rectangle)
(define-key ctl-x-map "ro" 'open-rectangle)
(define-key ctl-x-map "rt" 'string-rectangle)
(define-key ctl-x-map "rw" 'window-configuration-to-register)
(define-key ctl-x-map "rf" 'frame-configuration-to-register)

;; These key bindings are deprecated; use the above C-x r map instead.
;; We use these aliases so \[...] will show the C-x r bindings instead.
(defalias 'point-to-register-compatibility-binding 'point-to-register)
(defalias 'jump-to-register-compatibility-binding 'jump-to-register)
(defalias 'copy-to-register-compatibility-binding 'copy-to-register)
(defalias 'insert-register-compatibility-binding 'insert-register)
(define-key ctl-x-map "/" 'point-to-register-compatibility-binding)
(define-key ctl-x-map "j" 'jump-to-register-compatibility-binding)
(define-key ctl-x-map "x" 'copy-to-register-compatibility-binding)
(define-key ctl-x-map "g" 'insert-register-compatibility-binding)
;; (define-key ctl-x-map "r" 'copy-rectangle-to-register)

(define-key esc-map "q" 'fill-paragraph)
;; (define-key esc-map "g" 'fill-region)
(define-key ctl-x-map "." 'set-fill-prefix)

(define-key esc-map "{" 'backward-paragraph)
(define-key esc-map "}" 'forward-paragraph)
(define-key esc-map "h" 'mark-paragraph)
(define-key esc-map "a" 'backward-sentence)
(define-key esc-map "e" 'forward-sentence)
(define-key esc-map "k" 'kill-sentence)
(define-key ctl-x-map "\177" 'backward-kill-sentence)

(define-key ctl-x-map "[" 'backward-page)
(define-key ctl-x-map "]" 'forward-page)
(define-key ctl-x-map "\C-p" 'mark-page)
(define-key ctl-x-map "l" 'count-lines-page)
(define-key ctl-x-map "np" 'narrow-to-page)
;; (define-key ctl-x-map "p" 'narrow-to-page)

(define-key ctl-x-map "al" 'add-mode-abbrev)
(define-key ctl-x-map "a\C-a" 'add-mode-abbrev)
(define-key ctl-x-map "ag" 'add-global-abbrev)
(define-key ctl-x-map "a+" 'add-mode-abbrev)
(define-key ctl-x-map "aig" 'inverse-add-global-abbrev)
(define-key ctl-x-map "ail" 'inverse-add-mode-abbrev)
;; (define-key ctl-x-map "a\C-h" 'inverse-add-global-abbrev)
(define-key ctl-x-map "a-" 'inverse-add-global-abbrev)
(define-key ctl-x-map "ae" 'expand-abbrev)
(define-key ctl-x-map "a'" 'expand-abbrev)
;; (define-key ctl-x-map "\C-a" 'add-mode-abbrev)
;; (define-key ctl-x-map "\+" 'add-global-abbrev)
;; (define-key ctl-x-map "\C-h" 'inverse-add-mode-abbrev)
;; (define-key ctl-x-map "\-" 'inverse-add-global-abbrev)
(define-key esc-map "'" 'abbrev-prefix-mark)
(define-key ctl-x-map "'" 'expand-abbrev)

(define-key ctl-x-map "z" 'repeat)

;;; Don't compile this file; it contains no large function definitions.
;;; Don't look for autoload cookies in this file.
;;; Local Variables:
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:

;;; bindings.el ends here
