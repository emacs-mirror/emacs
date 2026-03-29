;;; edmacro.el --- keyboard macro editor  -*- lexical-binding: t; -*-

;; Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Keywords: abbrev

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

;;; Usage:
;;
;; The `C-x C-k e' (`edit-kbd-macro') command edits a keyboard macro
;; in a special buffer.  It prompts you to type a key sequence,
;; which should be one of:
;;
;;  * RET or `C-x e' (call-last-kbd-macro), to edit the most
;;    recently defined keyboard macro.
;;
;;  * `M-x' followed by a command name, to edit a named command
;;    whose definition is a keyboard macro.
;;
;;  * `C-h l' (view-lossage), to edit the 300 most recent keystrokes
;;    and install them as the "current" macro.
;;
;;  * any key sequence whose definition is a keyboard macro.
;;
;; This file includes a version of `insert-kbd-macro' that uses the
;; more readable format defined by these routines.
;;
;; Also, the `read-kbd-macro' command parses the region as
;; a keyboard macro, and installs it as the "current" macro.
;; This and `format-kbd-macro' can also be called directly as
;; Lisp functions.

;; Type `C-h m', or see the documentation for `edmacro-mode' below,
;; for information about the format of written keyboard macros.

;; `edit-kbd-macro' formats the macro with one command per line,
;; including the command names as comments on the right.  If the
;; formatter gets confused about which keymap was used for the
;; characters, the command-name comments will be wrong but that
;; won't hurt anything.

;; With a prefix argument, `edit-kbd-macro' will format the
;; macro in a more concise way that omits the comments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'kmacro)

;;; The user-level commands for editing macros.

(defcustom edmacro-eight-bits nil
  "Non-nil if `edit-kbd-macro' should leave 8-bit characters intact.
Default nil means to write characters above \\177 in octal notation."
  :type 'boolean
  :group 'kmacro)

(defcustom edmacro-reverse-macro-lines nil
  "If non-nil, `edit-kbd-macro' shows most recent line of key sequences first.

This is useful when dealing with long lists of key sequences, such as
from `kmacro-edit-lossage'."
  :type 'boolean
  :group 'kmacro
  :version "30.1")

(defvar-keymap edmacro-mode-map
  "C-c C-c" #'edmacro-finish-edit
  "C-c C-q" #'edmacro-insert-key
  "C-c C-r" #'edmacro-set-macro-to-region-lines)

(defface edmacro-label
  '((default :inherit bold)
    (((class color) (background dark)) :foreground "light blue")
    (((min-colors 88) (class color) (background light)) :foreground "DarkBlue")
    (((class color) (background light)) :foreground "blue")
    (t :inherit bold))
  "Face used for labels in `edit-kbd-macro'."
  :version "29.1"
  :group 'kmacro)

(defvar edmacro-mode-font-lock-keywords
  `((,(rx bol (group (or "Command" "Key"
                         (seq "Macro" (zero-or-one " (most recent line first)")))
                     ":"))
     0 'edmacro-label)
    (,(rx bol
          (group ";; Keyboard Macro Editor.  Press ")
          (group (*? nonl))
          (group  " to finish; press "))
     (1 'font-lock-comment-face)
     (2 'help-key-binding)
     (3 'font-lock-comment-face)
     (,(rx (group (*? nonl))
           (group " to cancel" (* nonl)))
      nil nil
      (1 'help-key-binding)
      (2 'font-lock-comment-face)))
    (,(rx (one-or-more ";") (zero-or-more nonl)) 0 'font-lock-comment-face)))

(defvar edmacro-store-hook)
(defvar edmacro-finish-hook)
(defvar edmacro-original-buffer)

;;;###autoload
(defun edit-kbd-macro (keys &optional prefix finish-hook store-hook)
  "Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type \\[kmacro-end-and-call-macro] or \\`RET' to edit the last
keyboard macro, \\[view-lossage] to edit the last 300
keystrokes as a keyboard macro, or \\[execute-extended-command]
to edit a macro by its command name.
With a prefix argument, format the macro in a more concise way."
  (interactive
   (list (read-key-sequence (substitute-command-keys "Keyboard macro to edit \
\(\\[kmacro-end-and-call-macro], \\[execute-extended-command], \\[view-lossage],\
 or keys): "))
         current-prefix-arg))
  (when keys
    (let ((cmd (if (arrayp keys) (key-binding keys) keys))
          (cmd-noremap (when (arrayp keys) (key-binding keys nil t)))
	  (mac nil) (mac-counter nil) (mac-format nil))
      (cond (store-hook
	     (setq mac keys)
	     (setq cmd nil))
	    ((or (memq cmd '(call-last-kbd-macro kmacro-call-macro kmacro-end-or-call-macro kmacro-end-and-call-macro))
                 (memq cmd-noremap '(call-last-kbd-macro kmacro-call-macro kmacro-end-or-call-macro kmacro-end-and-call-macro))
		 (member keys '("\r" [return])))
	     (or last-kbd-macro
                 (y-or-n-p "No keyboard macro defined.  Create one?")
		 (keyboard-quit))
	     (setq mac (or last-kbd-macro ""))
	     (setq keys nil)
	     (setq cmd 'last-kbd-macro))
	    ((memq 'execute-extended-command (list cmd cmd-noremap))
	     (setq cmd (read-command "Name of keyboard macro to edit: "))
	     (if (string-equal cmd "")
		 (error "No command name given"))
	     (setq keys nil)
	     (setq mac (symbol-function cmd)))
	    ((or (memq cmd '(view-lossage electric-view-lossage))
                 (memq cmd-noremap '(view-lossage electric-view-lossage)))
	     (setq mac (recent-keys))
	     (setq keys nil)
	     (setq cmd 'last-kbd-macro))
	    ((null cmd)
	     (error "Key sequence %s is not defined" (key-description keys)))
	    ((symbolp cmd)
	     (setq mac (symbol-function cmd)))
	    (t
	     (setq mac cmd)
	     (setq cmd nil)))
      (when (kmacro-p mac)
	(setq mac-counter (kmacro--counter mac)
	      mac-format (kmacro--format mac)
              mac (kmacro--keys mac)))
      (unless (arrayp mac)
	(error "Key sequence %s is not a keyboard macro"
	       (key-description keys)))
      (message "Formatting keyboard macro...")
      (let* ((oldbuf (current-buffer))
	     (mmac (edmacro-fix-menu-commands mac))
	     (fmt (edmacro-format-keys mmac 1))
	     (fmtv (let ((fmtv (edmacro-format-keys mmac (not prefix))))
                     (if (not edmacro-reverse-macro-lines)
                         fmtv
                       (with-temp-buffer
                         (insert fmtv)
                         (reverse-region (point-min) (point-max))
                         (buffer-string)))))
	     (buf (get-buffer-create "*Edit Macro*")))
	(message "Formatting keyboard macro...done")
	(switch-to-buffer buf)
	(kill-all-local-variables)
	(use-local-map edmacro-mode-map)
	(setq buffer-read-only nil)
	(setq major-mode 'edmacro-mode)
	(setq mode-name "Edit Macro")
        (setq-local edmacro-original-buffer oldbuf)
        (setq-local edmacro-finish-hook finish-hook)
        (setq-local edmacro-store-hook store-hook)
        (setq-local font-lock-defaults
                    '(edmacro-mode-font-lock-keywords nil nil ((?\" . "w"))))
        (setq font-lock-multiline nil)
        ;; Make buffer-local so that the commands still work
        ;; even if the default value changes.
        (make-local-variable 'edmacro-reverse-macro-lines)
	(erase-buffer)
        (insert (substitute-command-keys
                 (concat
                  ;; When editing this, make sure to update
                  ;; `edmacro-mode-font-lock-keywords' to match.
                  ";; Keyboard Macro Editor.  Press \\[edmacro-finish-edit] "
                  "to finish; press \\[kill-buffer] \\`RET' to cancel.\n")
                 ;; Use 'no-face argument to not conflict with font-lock.
                 'no-face))
	(insert ";; Original keys: " fmt "\n")
	(unless store-hook
	  (insert "\nCommand: " (if cmd (symbol-name cmd) "none") "\n")
	  (let ((gkeys (where-is-internal (or cmd mac) '(keymap))))
	    (if (and keys (not (member keys gkeys)))
		(setq gkeys (cons keys gkeys)))
	    (if gkeys
		(while gkeys
		  (insert "Key: " (edmacro-format-keys (pop gkeys) 1) "\n"))
	      (insert "Key: none\n")))
	  (when (and mac-counter mac-format)
	    (insert (format "Counter: %d\nFormat: \"%s\"\n" mac-counter mac-format))))
	(insert (format "\nMacro%s:\n\n" (if edmacro-reverse-macro-lines
                                             " (most recent line first)"
                                           "")))
	(save-excursion
	  (insert fmtv "\n"))
	(recenter '(4))
	(when (eq mac mmac)
	  (set-buffer-modified-p nil))
	(run-hooks 'edmacro-format-hook)))))

;; The next two commands are provided for convenience and backward
;; compatibility.

;;;###autoload
(defun edit-last-kbd-macro (&optional prefix)
  "Edit the most recently defined keyboard macro."
  (interactive "P")
  (edit-kbd-macro 'call-last-kbd-macro prefix))

;;;###autoload
(defun edit-named-kbd-macro (&optional prefix)
  "Edit a keyboard macro which has been given a name by `name-last-kbd-macro'."
  (interactive "P")
  (edit-kbd-macro 'execute-extended-command prefix))

;;;###autoload
(defun read-kbd-macro (start &optional end)
  "Read the region as a keyboard macro definition.
The region between START and END is interpreted as spelled-out keystrokes,
e.g., \"M-x abc RET\".  See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result is a vector of input events.
Second argument NEED-VECTOR means to return an event vector always."
  (interactive "r")
  (if (stringp start)
      (edmacro-parse-keys start end)
    (setq last-kbd-macro (edmacro-parse-keys (buffer-substring start end)))))

;;;###autoload
(defun format-kbd-macro (&optional macro verbose)
  "Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is `1', put everything on one line.  If VERBOSE is omitted
or nil, use a compact 80-column format."
  (and macro (symbolp macro) (setq macro (symbol-function macro)))
  (edmacro-format-keys (or macro last-kbd-macro) verbose))


;;; Commands for *Edit Macro* buffer.

(defvar edmacro--skip-line-regexp
  "[ \t]*\\($\\|;;\\|REM[ \t\n]\\)"
  "A regexp identifying lines that should be ignored.")

(defvar edmacro--command-line-regexp
  "Command:[ \t]*\\([^ \t\n]*\\)[ \t]*$"
  "A regexp identifying the line containing the command name.")

(defvar edmacro--key-line-regexp
  "Key:\\(.*\\)$"
  "A regexp identifying the line containing the bound key sequence.")

(defvar edmacro--counter-line-regexp
  "Counter:[ \t]*\\([^ \t\n]*\\)[ \t]*$"
  "A regexp identifying the line containing the counter value.")

(defvar edmacro--format-line-regexp
  "Format:[ \t]*\"\\([^\n]*\\)\"[ \t]*$"
  "A regexp identifying the line containing the counter format.")

(defvar edmacro--macro-lines-regexp
  (rx "Macro"
      (zero-or-one " (most recent line first)")
      ":"
      (zero-or-more (any " \t\n")))
  "A regexp identifying the lines that precede the macro's contents.")

(defun edmacro-finish-edit ()
  (interactive nil edmacro-mode)
  (unless (eq major-mode 'edmacro-mode)
    (error
     "This command is valid only in buffers created by `edit-kbd-macro'"))
  (run-hooks 'edmacro-finish-hook)
  (let ((cmd nil) (keys nil) (no-keys nil)
	(mac-counter nil) (mac-format nil)
	(top (point-min)))
    (goto-char top)
    (let ((case-fold-search nil))
      (while (cond ((looking-at edmacro--skip-line-regexp)
		    t)
		   ((looking-at edmacro--command-line-regexp)
		    (when edmacro-store-hook
		      (error "\"Command\" line not allowed in this context"))
		    (let ((str (match-string 1)))
		      (unless (equal str "")
			(setq cmd (and (not (equal str "none"))
				       (intern str)))
			(and (fboundp cmd) (not (arrayp (symbol-function cmd)))
			     (not (get cmd 'kmacro))
			     (not (y-or-n-p
                                   (format
                                    "Command %s is already defined; proceed?"
                                    cmd)))
			     (keyboard-quit))))
		    t)
		   ((looking-at edmacro--key-line-regexp)
		    (when edmacro-store-hook
		      (error "\"Key\" line not allowed in this context"))
		    (let ((key (kbd (match-string 1))))
		      (unless (equal key "")
			(if (equal key "none")
			    (setq no-keys t)
			  (push key keys)
			  (let ((b (key-binding key)))
			    (and b (commandp b) (not (arrayp b))
				 (not (kmacro-p b))
				 (or (not (fboundp b))
				     (not (or (arrayp (symbol-function b))
					      (get b 'kmacro))))
				 (not (y-or-n-p
                                       (format
                                        "Key %s is already defined; proceed?"
                                        (edmacro-format-keys key 1))))
				 (keyboard-quit))))))
		    t)
		   ((looking-at edmacro--counter-line-regexp)
		    (when edmacro-store-hook
		      (error "\"Counter\" line not allowed in this context"))
		    (let ((str (match-string 1)))
		      (unless (equal str "")
			(setq mac-counter (string-to-number str))))
		    t)
		   ((looking-at edmacro--format-line-regexp)
		    (when edmacro-store-hook
		      (error "\"Format\" line not allowed in this context"))
		    (let ((str (match-string 1)))
		      (unless (equal str "")
			(setq mac-format str)))
		    t)
		   ((looking-at edmacro--macro-lines-regexp)
		    (goto-char (match-end 0))
		    nil)
		   ((eobp) nil)
		   (t (error "Expected a `Macro:' line")))
	(forward-line 1))
      (setq top (point)))
    (let* ((buf (current-buffer))
	   (str (buffer-substring top (point-max)))
	   (modp (buffer-modified-p))
	   (obuf edmacro-original-buffer)
	   (store-hook edmacro-store-hook)
	   (finish-hook edmacro-finish-hook))
      (unless (or cmd keys store-hook (equal str ""))
	(error "No command name or keys specified"))
      (when modp
	(when (buffer-name obuf)
	  (set-buffer obuf))
	(message "Compiling keyboard macro...")
	(let ((mac (edmacro-parse-keys (if edmacro-reverse-macro-lines
                                           (with-temp-buffer
                                             (insert str)
                                             (reverse-region (point-min)
                                                             (point-max))
                                             (buffer-string))
                                         str))))
	  (message "Compiling keyboard macro...done")
	  (if store-hook
	      (funcall store-hook mac)
	    (when (eq cmd 'last-kbd-macro)
	      (setq last-kbd-macro (and (> (length mac) 0) mac))
	      (setq cmd nil))
	    (when cmd
	      (if (= (length mac) 0)
		  (fmakunbound cmd)
		(fset cmd (kmacro mac mac-counter mac-format))))
	    (if no-keys
		(when cmd
		  (cl-loop for key in (where-is-internal cmd '(keymap)) do
                           (global-unset-key key)))
	      (when keys
		(if (= (length mac) 0)
		    (cl-loop for key in keys do (global-unset-key key))
		  (cl-loop for key in keys do
                           (global-set-key key
                                           (or cmd
                                               (kmacro mac mac-counter
                                                       mac-format))))))))))
      (kill-buffer buf)
      (when (buffer-name obuf)
	(switch-to-buffer obuf))
      (when finish-hook
	(funcall finish-hook)))))

(defun edmacro-insert-key (key)
  "Insert the written name of a KEY in the buffer."
  (interactive "kKey to insert: " edmacro-mode)
  (if (bolp)
      (insert (edmacro-format-keys key t) "\n")
    (insert (edmacro-format-keys key) " ")))

(defun edmacro-set-macro-to-region-lines (beg end)
  "Set the macro text to lines of text in the buffer between BEG and END.

Interactively, BEG and END are the beginning and end of the
region.  If the region does not begin at the start of a line or
if it does not end at the end of a line, the region is extended
to include complete lines.  If the region ends at the beginning
of a line, that final line is excluded."
  (interactive "*r" edmacro-mode)
  ;; Use `save-excursion' to restore region if there are any errors.
  ;; If there are no errors, update the macro text, then go to the
  ;; beginning of the macro text.
  (let ((final-position))
    (save-excursion
      (goto-char beg)
      (unless (bolp) (setq beg (pos-bol)))
      (goto-char end)
      (unless (or (bolp) (eolp)) (setq end (pos-eol)))
      (let ((text (buffer-substring beg end)))
        (goto-char (point-min))
        (if (not (let ((case-fold-search nil))
                   (re-search-forward edmacro--macro-lines-regexp nil t)))
            (user-error "\"Macro:\" line not found")
          (delete-region (match-end 0)
                         (point-max))
          (goto-char (point-max))
          (insert text)
          (setq final-position (match-beginning 0)))))
    (goto-char final-position)))

(defun edmacro-mode ()
  "Keyboard Macro Editing mode.
\\<edmacro-mode-map>Press \\[edmacro-finish-edit] to save and exit.
To abort the edit, just kill this buffer with \\[kill-buffer] \\`RET'.

Press \\[edmacro-insert-key] to insert the name of any key by typing the key.

The editing buffer contains a \"Command:\" line and any number of
\"Key:\" lines at the top.  These are followed by a \"Macro:\" line
and the macro itself as spelled-out keystrokes: `C-x C-f foo RET'.

The \"Command:\" line specifies the command name to which the macro
is bound, or \"none\" for no command name.  Write \"last-kbd-macro\"
to refer to the current keyboard macro (as used by \\[call-last-kbd-macro]).

The \"Key:\" lines specify key sequences to which the macro is bound,
or \"none\" for no key bindings.

You can edit these lines to change the places where the new macro
is stored.

Press \\[edmacro-set-macro-to-region-lines] to replace the text following the \"Macro:\" line
with the text of the lines overlapping the region of text between
point and mark.  If that region ends at the beginning of a line,
that final line is excluded.

Format of keyboard macros during editing:

Text is divided into \"words\" separated by whitespace.  Except for
the words described below, the characters of each word go directly
as characters of the macro.  The whitespace that separates words
is ignored.  Whitespace in the macro must be written explicitly,
as in \"foo SPC bar RET\".

 * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent
   special control characters.  The words must be written in uppercase.

 * A word in angle brackets, e.g., <return>, <down>, or <f1>, represents
   a function key.  (Note that in the standard configuration, the
   function key <return> and the control key RET are synonymous.)
   You can use angle brackets on the words RET, SPC, etc., but they
   are not required there.

 * Keys can be written by their ASCII code, using a backslash followed
   by up to six octal digits.  This is the only way to represent keys
   with codes above \\377.

 * One or more prefixes M- (meta), C- (control), S- (shift), A- (alt),
   H- (hyper), and s- (super) may precede a character or key notation.
   For function keys, the prefixes may go inside or outside of the
   brackets:  C-<down> = <C-down>.  The prefixes may be written in
   any order:  M-C-x = C-M-x.

   Prefixes are not allowed on multi-key words, e.g., C-abc, except
   that the Meta prefix is allowed on a sequence of digits and optional
   minus sign:  M--123 = M-- M-1 M-2 M-3.

 * The `^' notation for control characters also works:  ^M = C-m.

 * Double angle brackets enclose command names:  <<next-line>> is
   shorthand for M-x next-line RET.

 * Finally, REM or ;; causes the rest of the line to be ignored as a
   comment.

Any word may be prefixed by a multiplier in the form of a decimal
number and `*':  3*<right> = <right> <right> <right>, and
10*foo = foofoofoofoofoofoofoofoofoofoo.

Multiple text keys can normally be strung together to form a word,
but you may need to add whitespace if the word would look like one
of the above notations:  `; ; ;' is a keyboard macro with three
semicolons, but `;;;' is a comment.  Likewise, `\\ 1 2 3' is four
keys but `\\123' is a single key written in octal, and `< right >'
is seven keys but `<right>' is a single function key.  When in
doubt, use whitespace."
  (interactive)
  (error "This mode can be enabled only by `edit-kbd-macro'"))
(put 'edmacro-mode 'mode-class 'special)


;;; Formatting a keyboard macro as human-readable text.

(defun edmacro-format-keys (macro &optional verbose)
  (setq macro (edmacro-fix-menu-commands macro))
  (let* ((maps (current-active-maps))
	 (pkeys '(end-macro ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?\C-u
		  ?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6
		  ?\M-7 ?\M-8 ?\M-9))
	 (mdigs (nthcdr 13 pkeys))
	 (maxkey (if edmacro-eight-bits 255 127))
	 (case-fold-search nil)
	 (res-words '("NUL" "TAB" "LFD" "RET" "ESC" "SPC" "DEL" "REM"))
	 (rest-mac (vconcat macro [end-macro]))
	 (res "")
	 (len 0)
	 (one-line (eq verbose 1)))
    (if one-line (setq verbose nil))
    (when (stringp macro)
      (cl-loop for i below (length macro) do
               (when (>= (aref rest-mac i) 128)
                 (incf (aref rest-mac i) (- ?\M-\^@ 128)))))
    (while (not (eq (aref rest-mac 0) 'end-macro))
      (let* ((prefix
	      (or (and (integerp (aref rest-mac 0))
		       (memq (aref rest-mac 0) mdigs)
		       (memq (key-binding (cl-subseq rest-mac 0 1))
			     '(digit-argument negative-argument))
		       (let ((i 1))
			 (while (memq (aref rest-mac i) (cdr mdigs))
                           (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (vconcat "M-" (cl-subseq rest-mac 0 i) " ")
				(cl-callf cl-subseq rest-mac i)))))
		  (and (eq (aref rest-mac 0) ?\C-u)
		       (eq (key-binding [?\C-u]) 'universal-argument)
		       (let ((i 1))
			 (while (eq (aref rest-mac i) ?\C-u)
                           (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (cl-loop repeat i concat "C-u ")
				(cl-callf cl-subseq rest-mac i)))))
		  (and (eq (aref rest-mac 0) ?\C-u)
		       (eq (key-binding [?\C-u]) 'universal-argument)
		       (let ((i 1))
			 (when (eq (aref rest-mac i) ?-)
                           (incf i))
			 (while (memq (aref rest-mac i)
				      '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                           (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (vconcat "C-u " (cl-subseq rest-mac 1 i) " ")
				(cl-callf cl-subseq rest-mac i)))))))
	     (bind-len (apply #'max 1
			      (cl-loop for map in maps
                                       for b = (lookup-key map rest-mac)
                                       when b collect b)))
	     (key (cl-subseq rest-mac 0 bind-len))
	     (fkey nil) tlen tkey
	     (bind (or (cl-loop for map in maps for b = (lookup-key map key)
                                thereis (and (not (integerp b)) b))
		       (and (setq fkey (lookup-key local-function-key-map rest-mac))
			    (setq tlen fkey tkey (cl-subseq rest-mac 0 tlen)
				  fkey (lookup-key local-function-key-map tkey))
			    (cl-loop for map in maps
                                     for b = (lookup-key map fkey)
                                     when (and (not (integerp b)) b)
                                     do (setq bind-len tlen key tkey)
                                     and return b
                                     finally do (setq fkey nil)))))
	     (first (aref key 0))
	     (text
              (cl-loop for i from bind-len below (length rest-mac)
                       for ch = (aref rest-mac i)
                       while (and (integerp ch)
                                  (> ch 32) (< ch maxkey) (/= ch 92)
                                  (eq (key-binding (char-to-string ch))
                                      'self-insert-command)
                                  (or (> i (- (length rest-mac) 2))
                                      (not (eq ch (aref rest-mac (+ i 1))))
                                      (not (eq ch (aref rest-mac (+ i 2))))))
                       finally return i))
	     desc)
	(if (stringp bind) (setq bind nil))
	(cond ((and (eq bind #'self-insert-command) (not prefix)
		    (> text 1) (integerp first)
		    (> first 32) (<= first maxkey) (/= first 92)
		    (progn
		      (if (> text 30) (setq text 30))
		      (setq desc (concat (cl-subseq rest-mac 0 text)))
		      (when (string-match "^[ACHMsS]-." desc)
			(setq text 2)
			(cl-callf substring desc 0 2))
		      (not (string-match
			    "^;;\\|^<.*>$\\|^\\\\[0-9]+$\\|^[0-9]+\\*."
			    desc))))
	       (when (or (string-match "^\\^.$" desc)
			 (member desc res-words))
		 (setq desc (mapconcat #'char-to-string desc " ")))
	       (when verbose
		 (setq bind (format "%s * %d" bind text)))
	       (setq bind-len text))
	      ((and (eq bind #'execute-extended-command)
		    (> text bind-len)
		    (memq (aref rest-mac text) '(return 13))
		    (progn
		      (setq desc (concat (cl-subseq rest-mac bind-len text)))
		      (commandp (intern-soft desc))))
	       (if (commandp (intern-soft desc)) (setq bind desc))
	       (setq desc (format "<<%s>>" desc))
	       (setq bind-len (1+ text)))
	      (t
	       (setq desc (mapconcat
                           (lambda (ch)
                             (cond
                              ((integerp ch)
                               (concat
                                (cl-loop for pf across "ACHMsS"
                                         for bit in '( ?\A-\0 ?\C-\0 ?\H-\0
                                                       ?\M-\0 ?\s-\0 ?\S-\0)
                                         when (/= (logand ch bit) 0)
                                         concat (format "%c-" pf))
                                (let ((ch2 (logand ch (1- (ash 1 18)))))
                                  (cond ((<= ch2 32)
                                         (pcase ch2
                                           (0 "NUL") (9 "TAB") (10 "LFD")
                                           (13 "RET") (27 "ESC") (32 "SPC")
                                           (_
                                            (format "C-%c"
                                                    (+ (if (<= ch2 26) 96 64)
                                                       ch2)))))
                                        ((= ch2 127) "DEL")
                                        ((<= ch2 maxkey) (char-to-string ch2))
                                        (t (format "\\%o" ch2))))))
                              ((symbolp ch)
                               (format "<%s>" ch))
                              (t
                               (error "Unrecognized item in macro: %s" ch))))
			   (or fkey key) " "))))
	(if prefix
	    (setq desc (concat (edmacro-sanitize-for-string prefix) desc)))
	(unless (string-search " " desc)
	  (let ((times 1) (pos bind-len))
	    (while (not (cl-mismatch rest-mac rest-mac
				     :start1 0 :end1 bind-len
				     :start2 pos :end2 (+ bind-len pos)))
              (incf times)
              (incf pos bind-len))
	    (when (> times 1)
	      (setq desc (format "%d*%s" times desc))
	      (setq bind-len (* bind-len times)))))
	(setq rest-mac (cl-subseq rest-mac bind-len))
	(if verbose
	    (progn
	      (unless (equal res "") (cl-callf concat res "\n"))
	      (cl-callf concat res desc)
	      (when (and bind (or (stringp bind) (symbolp bind)))
		(cl-callf concat res
		  (make-string (max (- 3 (/ (length desc) 8)) 1) 9)
		  ";; " (if (stringp bind) bind (symbol-name bind))))
	      (setq len 0))
	  (if (and (> (+ len (length desc) 2) 72) (not one-line))
	      (progn
		(cl-callf concat res "\n ")
		(setq len 1))
	    (unless (equal res "")
	      (cl-callf concat res " ")
              (incf len)))
	  (cl-callf concat res desc)
          (incf len (length desc)))))
    res))

(defun edmacro-sanitize-for-string (seq)
  "Convert a key sequence vector SEQ into a string.
The string represents the same events; Meta is indicated by bit 7.
This function assumes that the events can be stored in a string."
  (setq seq (copy-sequence seq))
  (cl-loop for i below (length seq) do
           (setf (aref seq i) (logand (aref seq i) 127)))
  seq)

(defun edmacro-fix-menu-commands (macro &optional noerror)
  (if (vectorp macro)
      (let (result)
        ;; Not preloaded in a --without-x build.
        (require 'mwheel)
        (defvar mouse-wheel-down-event)
        (defvar mouse-wheel-up-event)
        (defvar mouse-wheel-right-event)
        (defvar mouse-wheel-left-event)
	;; Make a list of the elements.
	(setq macro (append macro nil))
	(dolist (ev macro)
	  (cond ((atom ev)
		 (push ev result))
		((eq (car ev) 'help-echo))
		((eq (car ev) 'switch-frame))
		((equal ev '(menu-bar))
		 (push 'menu-bar result))
                ((equal (cadadr ev) '(menu-bar))
		 (push (vector 'menu-bar (car ev)) result))
		;; It would be nice to do pop-up menus, too, but not enough
		;; info is recorded in macros to make this possible.
		((or (mouse-event-p ev) (mouse-movement-p ev)
		     (memq (event-basic-type ev)
			   (with-suppressed-warnings
			       ((obsolete
			         mouse-wheel-down-event mouse-wheel-right-event
			         mouse-wheel-up-event mouse-wheel-left-event))
			     `( ,mouse-wheel-down-event ,mouse-wheel-up-event
			        ,mouse-wheel-right-event ,mouse-wheel-left-event
			        wheel-down wheel-up wheel-left wheel-right))))
		 nil)
		(noerror nil)
		(t
		 (error "`edmacro-fix-menu-commands': Unsupported event: %S"
			ev))))
	;; Reverse them again and make them back into a vector.
	(vconcat (nreverse result)))
    macro))


;;; Parsing a human-readable keyboard macro.

(defun edmacro-parse-keys (string &optional _need-vector)
  (let ((result (kbd string)))
    ;; Always return a vector.  Stefan Monnier <monnier@iro.umontreal.ca>
    ;; writes: "I want to eliminate the use of strings that stand for a
    ;; sequence of events because it does nothing more than leave latent
    ;; bugs and create confusion (between the strings used as input to
    ;; `read-kbd-macro' and the strings that used to be output by
    ;; `read-kbd-macro'), while increasing the complexity of the rest of
    ;; the code which has to handle both vectors and strings."
    (if (stringp result)
        (seq-into result 'vector)
      result)))

(provide 'edmacro)

;;; edmacro.el ends here
