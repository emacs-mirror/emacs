;;; bindings.el --- define standard key bindings and some variables

;; Copyright (C) 1985,86,87,92,93,94,95,96,99,2000, 2001
;;   Free Software Foundation, Inc.

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

(defun make-mode-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))


(defun mode-line-toggle-read-only (event)
  "Like `toggle-read-only', for the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-read-only)
    (force-mode-line-update)))


(defun mode-line-toggle-modified (event)
  "Toggle the buffer-modified flag from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (set-buffer-modified-p (not (buffer-modified-p)))
    (force-mode-line-update)))


(defun mode-line-widen (event)
  "Widen a buffer from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (widen)
    (force-mode-line-update)))


(defun mode-line-abbrev-mode (event)
  "Turn off `abbrev-mode' from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (abbrev-mode)
    (force-mode-line-update)))


(defun mode-line-auto-fill-mode (event)
  "Turn off `auto-fill-mode' from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (auto-fill-mode)
    (force-mode-line-update)))


(defvar mode-line-input-method-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-2]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window
	   (posn-window (event-start e)))
	  (toggle-input-method)
	  (force-mode-line-update))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window
	   (posn-window (event-start e)))
	  (describe-current-input-method))))
    (purecopy map)))


(defvar mode-line-coding-system-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(save-selected-window
	  (select-window (posn-window (event-start e)))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (purecopy map))
  "Local keymap for the coding-system part of the mode line.")


(defvar mode-line-mule-info
  `(""
    (current-input-method
     (:propertize ("" current-input-method-title)
		  help-echo (concat
			     "Input method: "
			     current-input-method
			     ".  mouse-2: disable, mouse-3: describe")
		  local-map ,mode-line-input-method-map))
    ,(propertize
      "%Z"
      'help-echo
      #'(lambda (window object point)
	  (with-current-buffer (window-buffer window)
	    ;; Don't show this tip if the coding system is nil,
	    ;; it reads like a bug, and is not useful anyway.
	    (when buffer-file-coding-system
	      (if enable-multibyte-characters
		  (concat (symbol-name buffer-file-coding-system)
			  " buffer; mouse-3: describe coding system")
		(concat "Unibyte " (symbol-name buffer-file-coding-system)
			" buffer")))))
      'local-map mode-line-coding-system-map))
  "Mode-line control for displaying information of multilingual environment.
Normally it displays current input method (if any activated) and
mnemonics of the following coding systems:
  coding system for saving or writing the current buffer
  coding system for keyboard input (if Emacs is running on terminal)
  coding system for terminal output (if Emacs is running on terminal)"
;;; Currently not:
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

(defvar mode-line-frame-identification '("-%F  ")
  "Mode-line control to describe the current frame.")

(defvar mode-line-process nil "\
Mode-line control for displaying info on process status.
Normally nil in most modes, since there is no process to display.")

(make-variable-buffer-local 'mode-line-process)

(defvar mode-line-modified
  (list (propertize
	 "%1*"
	 'help-echo (purecopy (lambda (window object point)
 				(format "%sead-only: mouse-3 toggles"
					(save-selected-window
					  (select-window window)
					  (if buffer-read-only
					      "R"
					    "Not r")))))
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-3
			       #'mode-line-toggle-read-only)))
	(propertize
	 "%1+"
	 'help-echo  (purecopy (lambda (window object point)
				 (format "%sodified: mouse-3 toggles"
					 (save-selected-window
					   (select-window window)
					   (if (buffer-modified-p)
					     "M"
					   "Not m")))))
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-3 #'mode-line-toggle-modified))))
  "Mode-line control for displaying whether current buffer is modified.")

(make-variable-buffer-local 'mode-line-modified)

;; Actual initialization is below.
(defvar mode-line-position nil
  "Mode-line control for displaying line number, column number and fraction.")

(defvar mode-line-modes nil
  "Mode-line control for displaying major and minor modes.")

(defvar mode-line-minor-mode-keymap nil "\
Keymap to display on major and minor modes.")

;; Menu of minor modes.
(let ((map (make-sparse-keymap)))
  (define-key map [mode-line down-mouse-3] 'mode-line-mode-menu-1)
  (define-key map [header-line down-mouse-3] 'mode-line-mode-menu-1)
  (setq mode-line-minor-mode-keymap map))

(let* ((help-echo
	;; The multi-line message doesn't work terribly well on the
	;; bottom mode line...  Better ideas?
	;; 	  "\
	;; mouse-1: select window, mouse-2: delete others, mouse-3: delete,
	;; drag-mouse-1: resize, C-mouse-2: split horizontally"
	"mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete")
       (dashes (propertize "--" 'help-echo help-echo)))
  (setq-default mode-line-format
    (list
     (propertize "-" 'help-echo help-echo)
     'mode-line-mule-info
     'mode-line-modified
     'mode-line-frame-identification
     'mode-line-buffer-identification
     (propertize "   " 'help-echo help-echo)
     'global-mode-string
     'mode-line-modes
     `(which-func-mode ("" which-func-format ,dashes))
     'mode-line-position
     (propertize "-%-" 'help-echo help-echo)))

  (setq-default mode-line-modes
    (list
     (propertize "   %[(" 'help-echo help-echo)
     `(:propertize ("" mode-name mode-line-process minor-mode-alist)
		   help-echo "mouse-3: minor mode menu"
		   local-map ,mode-line-minor-mode-keymap)
     (propertize "%n" 'help-echo "mouse-2: widen"
		 'local-map (make-mode-line-mouse-map
			     'mouse-2 #'mode-line-widen))
     (propertize ")%]--" 'help-echo help-echo)))

  (setq-default mode-line-position
    `((line-number-mode (,(propertize "L%l" 'help-echo help-echo) ,dashes))
      (column-number-mode (,(propertize "C%c" 'help-echo help-echo) ,dashes))
      (-3 . ,(propertize "%p" 'help-echo help-echo)))))

(defvar mode-line-buffer-identification-keymap nil "\
Keymap for what is displayed by `mode-line-buffer-identification'.")

(defun last-buffer () "\
Return the last non-hidden buffer in the buffer list."
  (let ((list (reverse (buffer-list))))
    (while (eq (aref (buffer-name (car list)) 0) ? )
      (setq list (cdr list)))
    (car list)))

(defun unbury-buffer () "\
Switch to the last buffer in the buffer list."
  (interactive)
  (switch-to-buffer (last-buffer)))

(defun mode-line-unbury-buffer (event) "\
Call `unbury-buffer' in this window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (unbury-buffer)))

(defun mode-line-bury-buffer (event) "\
Like `bury-buffer', but temporarily select EVENT's window."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (bury-buffer)))

(defun mode-line-other-buffer () "\
Switch to the most recently selected buffer other than the current one."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defvar mode-line-mode-menu (make-sparse-keymap "Minor Modes") "\
Menu of mode operations in the mode line.")

(defun mode-line-mode-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (mode-line-mode-menu event))
	   (binding (and selection (lookup-key mode-line-mode-menu
					       (vector (car selection))))))
      (if binding
	  (call-interactively binding)))))

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(define-key mode-line-mode-menu [overwrite-mode]
  `(menu-item ,(purecopy "Overwrite (Ovwrt)") overwrite-mode
	      :button (:toggle . overwrite-mode)))
(define-key mode-line-mode-menu [outline-minor-mode]
  `(menu-item ,(purecopy "Outline (Outl)") outline-minor-mode
	      :button (:toggle . (bound-and-true-p outline-minor-mode))))
(define-key mode-line-mode-menu [line-number-mode]
  `(menu-item ,(purecopy "Line number") line-number-mode
	      :button (:toggle . line-number-mode)))
(define-key mode-line-mode-menu [highlight-changes-mode]
  `(menu-item ,(purecopy "Highlight changes (Chg)") highlight-changes-mode
	      :button (:toggle . highlight-changes-mode)))
(define-key mode-line-mode-menu [glasses-mode]
  `(menu-item ,(purecopy "Glasses (o^o)") glasses-mode
	      :button (:toggle . (bound-and-true-p glasses-mode))))
(define-key mode-line-mode-menu [hide-ifdef-mode]
  `(menu-item ,(purecopy "Hide ifdef (Ifdef)") hide-ifdef-mode
	      :button (:toggle . (bound-and-true-p hide-ifdef-mode))))
(define-key mode-line-mode-menu [font-lock-mode]
  `(menu-item ,(purecopy "Font Lock") font-lock-mode
	      :button (:toggle . font-lock-mode)))
(define-key mode-line-mode-menu [flyspell-mode]
  `(menu-item ,(purecopy "Flyspell (Fly)") flyspell-mode
	      :button (:toggle . (bound-and-true-p flyspell-mode))))
(define-key mode-line-mode-menu [column-number-mode]
  `(menu-item ,(purecopy "Column number") column-number-mode
	      :button (:toggle . column-number-mode)))
(define-key mode-line-mode-menu [auto-fill-mode]
  `(menu-item ,(purecopy "Auto Fill (Fill)") auto-fill-mode
	      :button (:toggle . auto-fill-function)))
(define-key mode-line-mode-menu [auto-revert-mode]
  `(menu-item ,(purecopy "Auto revert (ARev)") auto-revert-mode
	      :button (:toggle . auto-revert-mode)))
(define-key mode-line-mode-menu [abbrev-mode]
  `(menu-item ,(purecopy "Abbrev (Abbrev)") abbrev-mode
	      :button (:toggle . abbrev-mode)))

(defun mode-line-mode-menu (event)
  (interactive "@e")
  (x-popup-menu event mode-line-mode-menu))

;; Add menu of buffer operations to the buffer identification part
;; of the mode line.or header line.
;
(let ((map (make-sparse-keymap)))
  ;; Bind down- events so that the global keymap won't ``shine
  ;; through''.
  (define-key map [mode-line down-mouse-1] 'ignore)
  (define-key map [mode-line mouse-1] 'mode-line-unbury-buffer)
  (define-key map [header-line down-mouse-1] 'ignore)
  (define-key map [header-line mouse-1] 'mode-line-unbury-buffer)
  (define-key map [header-line down-mouse-3] 'ignore)
  (define-key map [mode-line mouse-3] 'mode-line-bury-buffer)
  (define-key map [header-line down-mouse-3] 'ignore)
  (define-key map [header-line mouse-3] 'mode-line-bury-buffer)
  (setq mode-line-buffer-identification-keymap map))

(defun propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
		    'face '(:weight bold)
		    'help-echo
		    (purecopy "mouse-1: previous buffer, mouse-3: next buffer")
		    'local-map mode-line-buffer-identification-keymap)))

(setq-default mode-line-buffer-identification
	      (propertized-buffer-identification "%12b"))

(defvar minor-mode-alist nil "\
Alist saying how to show minor modes in the mode line.
Each element looks like (VARIABLE STRING);
STRING is included in the mode line iff VARIABLE's value is non-nil.

Actually, STRING need not be a string; any possible mode-line element
is okay.  See `mode-line-format'.")
;; Don't use purecopy here--some people want to change these strings.
(setq minor-mode-alist
      (list
       (list 'abbrev-mode " Abbrev")
       '(overwrite-mode overwrite-mode)
       (list 'auto-fill-function " Fill")
       ;; not really a minor mode...
       '(defining-kbd-macro " Def")))

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.

;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      (append
       (cond ((memq system-type '(ms-dos windows-nt))
	      '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk"
		".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386"))
	     ((eq system-type 'vax-vms)
	      '(".obj" ".exe" ".bin" ".lbin" ".sbin"
		".brn" ".rnt" ".lni" ".lis"
		".olb" ".tlb" ".mlb" ".hlb"))
	     (t
	      '(".o" "~" ".bin" ".lbin" ".so"
		".a" ".ln" ".blg" ".bbl")))
       '(".elc" ".lof"
	 ".glo" ".idx" ".lot"
	 ;; TeX-related
	 ".dvi" ".fmt" ".tfm" ".pdf"
	 ;; Java compiled
	 ".class"
	 ;; CLISP
	 ".fas" ".lib" ".mem"
	 ;; CMUCL
	 ".x86f" ".sparcf"
         ;; Other CL implementations (Allegro, LispWorks)
         ".fasl" ".ufsl" ".fsl" ".dxl"
	 ;; Libtool
	 ".lo" ".la"
	 ;; Texinfo-related
	 ".toc" ".log" ".aux"
	 ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
	 ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs")))

;; Suffixes used for executables.
(setq exec-suffixes
      (cond
       ((memq system-type '(ms-dos windows-nt))
	'(".exe" ".com" ".bat" ".cmd" ".btm" ""))
       (t
	'(""))))

;; Packages should add to this list appropriately when they are
;; loaded, rather than listing everything here.
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
	"^Version control package .*.el not found. Use vc.el instead$"))


(make-variable-buffer-local 'indent-tabs-mode)

;; We have base64 and md5 functions built in now.
(provide 'base64)
(provide 'md5)
(provide 'overlay '(display syntax-table field))
(provide 'text-properties '(display syntax-table field point-entered))

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

;; From macros.c
(define-key ctl-x-map "(" 'start-kbd-macro)
(define-key ctl-x-map ")" 'end-kbd-macro)
(define-key ctl-x-map "e" 'call-last-kbd-macro)
;; From frame.c
(global-set-key [switch-frame] 'handle-switch-frame)
(global-set-key [delete-frame] 'handle-delete-frame)
(global-set-key [iconify-frame] 'ignore-event)
(global-set-key [make-frame-visible] 'ignore-event)


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

(let ((map minibuffer-local-map))
  (define-key map "\en"   'next-history-element)
  (define-key map [next]  'next-history-element)
  (define-key map [down]  'next-history-element)
  (define-key map "\ep"   'previous-history-element)
  (define-key map [prior] 'previous-history-element)
  (define-key map [up]    'previous-history-element)
  (define-key map "\es"   'next-matching-history-element)
  (define-key map "\er"   'previous-matching-history-element))

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
;;  (error "That function key is not bound to anything"))

(define-key global-map [menu] 'execute-extended-command)
(define-key global-map [find] 'search-forward)

;; Don't do this.  We define <delete> in function-key-map instead.
;(define-key global-map [delete] 'backward-delete-char)

;; natural bindings for terminal keycaps --- defined in X keysym order
(define-key global-map [home]		'beginning-of-line)
(define-key global-map [C-home]		'beginning-of-buffer)
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
(define-key global-map [end]		'end-of-line)
(define-key global-map [C-end]		'end-of-buffer)
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
(define-key function-key-map [backspace] [?\C-?])
(define-key function-key-map [delete] [?\C-?])
(define-key function-key-map [kp-delete] [?\C-?])
(define-key function-key-map [S-kp-end] [S-end])
(define-key function-key-map [S-kp-down] [S-down])
(define-key function-key-map [S-kp-next] [S-next])
(define-key function-key-map [S-kp-left] [S-left])
(define-key function-key-map [S-kp-right] [S-right])
(define-key function-key-map [S-kp-home] [S-home])
(define-key function-key-map [S-kp-up] [S-up])
(define-key function-key-map [S-kp-prior] [S-prior])
(define-key function-key-map [C-S-kp-end] [C-S-end])
(define-key function-key-map [C-S-kp-down] [C-S-down])
(define-key function-key-map [C-S-kp-next] [C-S-next])
(define-key function-key-map [C-S-kp-left] [C-S-left])
(define-key function-key-map [C-S-kp-right] [C-S-right])
(define-key function-key-map [C-S-kp-home] [C-S-home])
(define-key function-key-map [C-S-kp-up] [C-S-up])
(define-key function-key-map [C-S-kp-prior] [C-S-prior])
;; Don't bind shifted keypad numeric keys, they reportedly
;; interfere with the feature of some keyboards to produce
;; numbers when NumLock is off.
;(define-key function-key-map [S-kp-1] [S-end])
;(define-key function-key-map [S-kp-2] [S-down])
;(define-key function-key-map [S-kp-3] [S-next])
;(define-key function-key-map [S-kp-4] [S-left])
;(define-key function-key-map [S-kp-6] [S-right])
;(define-key function-key-map [S-kp-7] [S-home])
;(define-key function-key-map [S-kp-8] [S-up])
;(define-key function-key-map [S-kp-9] [S-prior])
(define-key function-key-map [C-S-kp-1] [C-S-end])
(define-key function-key-map [C-S-kp-2] [C-S-down])
(define-key function-key-map [C-S-kp-3] [C-S-next])
(define-key function-key-map [C-S-kp-4] [C-S-left])
(define-key function-key-map [C-S-kp-6] [C-S-right])
(define-key function-key-map [C-S-kp-7] [C-S-home])
(define-key function-key-map [C-S-kp-8] [C-S-up])
(define-key function-key-map [C-S-kp-9] [C-S-prior])

(define-key global-map [mouse-movement] 'ignore)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'comment-dwim)
(define-key esc-map "j" 'indent-new-comment-line)
(define-key esc-map "\C-j" 'indent-new-comment-line)
(define-key ctl-x-map ";" 'comment-set-column)
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
(global-set-key [C-delete] 'backward-kill-word)
(global-set-key [C-backspace] 'kill-word)
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
(define-key esc-map [C-delete] 'backward-kill-sexp)
(define-key esc-map [C-backspace] 'backward-kill-sexp)
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

;; Don't look for autoload cookies in this file.
;; Local Variables:
;; no-update-autoloads: t
;; End:

;;; bindings.el ends here
