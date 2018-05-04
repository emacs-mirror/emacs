;;; help.el --- help commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-1986, 1993-1994, 1998-2018 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help, internal
;; Package: emacs

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

;; This code implements GNU Emacs's built-in help system, the one invoked by
;; `M-x help-for-help'.

;;; Code:

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))

;; This makes `with-output-to-temp-buffer' buffers use `help-mode'.
(add-hook 'temp-buffer-setup-hook 'help-mode-setup)
(add-hook 'temp-buffer-show-hook 'help-mode-finish)

;; `help-window-point-marker' is a marker you can move to a valid
;; position of the buffer shown in the help window in order to override
;; the standard positioning mechanism (`point-min') chosen by
;; `with-output-to-temp-buffer' and `with-temp-buffer-window'.
;; `with-help-window' has this point nowhere before exiting.  Currently
;; used by `view-lossage' to assert that the last keystrokes are always
;; visible.
(defvar help-window-point-marker (make-marker)
  "Marker to override default `window-point' in help windows.")

(defvar help-window-old-frame nil
  "Frame selected at the time `with-help-window' is invoked.")

(defvar help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (char-to-string help-char) 'help-for-help)
    (define-key map [help] 'help-for-help)
    (define-key map [f1] 'help-for-help)
    (define-key map "." 'display-local-help)
    (define-key map "?" 'help-for-help)

    (define-key map "\C-a" 'about-emacs)
    (define-key map "\C-c" 'describe-copying)
    (define-key map "\C-d" 'view-emacs-debugging)
    (define-key map "\C-e" 'view-external-packages)
    (define-key map "\C-f" 'view-emacs-FAQ)
    (define-key map "\C-m" 'view-order-manuals)
    (define-key map "\C-n" 'view-emacs-news)
    (define-key map "\C-o" 'describe-distribution)
    (define-key map "\C-p" 'view-emacs-problems)
    (define-key map "\C-s" 'search-forward-help-for-help)
    (define-key map "\C-t" 'view-emacs-todo)
    (define-key map "\C-w" 'describe-no-warranty)

    ;; This does not fit the pattern, but it is natural given the C-\ command.
    (define-key map "\C-\\" 'describe-input-method)

    (define-key map "C" 'describe-coding-system)
    (define-key map "F" 'Info-goto-emacs-command-node)
    (define-key map "I" 'describe-input-method)
    (define-key map "K" 'Info-goto-emacs-key-command-node)
    (define-key map "L" 'describe-language-environment)
    (define-key map "S" 'info-lookup-symbol)

    (define-key map "a" 'apropos-command)
    (define-key map "b" 'describe-bindings)
    (define-key map "c" 'describe-key-briefly)
    (define-key map "d" 'apropos-documentation)
    (define-key map "e" 'view-echo-area-messages)
    (define-key map "f" 'describe-function)
    (define-key map "g" 'describe-gnu-project)
    (define-key map "h" 'view-hello-file)

    (define-key map "i" 'info)
    (define-key map "4i" 'info-other-window)

    (define-key map "k" 'describe-key)
    (define-key map "l" 'view-lossage)
    (define-key map "m" 'describe-mode)
    (define-key map "o" 'describe-symbol)
    (define-key map "n" 'view-emacs-news)
    (define-key map "p" 'finder-by-keyword)
    (define-key map "P" 'describe-package)
    (define-key map "r" 'info-emacs-manual)
    (define-key map "s" 'describe-syntax)
    (define-key map "t" 'help-with-tutorial)
    (define-key map "w" 'where-is)
    (define-key map "v" 'describe-variable)
    (define-key map "q" 'help-quit)
    map)
  "Keymap for characters following the Help key.")

(define-key global-map (char-to-string help-char) 'help-command)
(define-key global-map [help] 'help-command)
(define-key global-map [f1] 'help-command)
(fset 'help-command help-map)

;; insert-button makes the action nil if it is not store somewhere
(defvar help-button-cache nil)


(defun help-quit ()
  "Just exit from the Help command's command loop."
  (interactive)
  nil)

(defvar help-return-method nil
  "What to do to \"exit\" the help buffer.
This is a list
 (WINDOW . t)              delete the selected window (and possibly its frame,
                           see `quit-window'), go to WINDOW.
 (WINDOW . quit-window)    do quit-window, then select WINDOW.
 (WINDOW BUF START POINT)  display BUF at START, POINT, then select WINDOW.")

(define-obsolete-function-alias 'print-help-return-message 'help-print-return-message "23.2")
(defun help-print-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
This function assumes that `standard-output' is the help buffer.
It computes a message, and applies the optional argument FUNCTION to it.
If FUNCTION is nil, it applies `message', thus displaying the message.
In addition, this function sets up `help-return-method', which see, that
specifies what to do when the user exits the help buffer.

Do not call this in the scope of `with-help-window'."
  (and (not (get-buffer-window standard-output))
       (let ((first-message
	      (cond ((or
		      pop-up-frames
		      (special-display-p (buffer-name standard-output)))
		     (setq help-return-method (cons (selected-window) t))
		     ;; If the help output buffer is a special display buffer,
		     ;; don't say anything about how to get rid of it.
		     ;; First of all, the user will do that with the window
		     ;; manager, not with Emacs.
		     ;; Secondly, the buffer has not been displayed yet,
		     ;; so we don't know whether its frame will be selected.
		     nil)
		    ((not (one-window-p t))
		     (setq help-return-method
			   (cons (selected-window) 'quit-window))
		     "Type \\[display-buffer] RET to restore the other window.")
		    (pop-up-windows
		     (setq help-return-method (cons (selected-window) t))
		     "Type \\[delete-other-windows] to remove help window.")
		    (t
		     (setq help-return-method
			   (list (selected-window) (window-buffer)
				 (window-start) (window-point)))
		     "Type \\[switch-to-buffer] RET to remove help window."))))
	 (funcall (or function 'message)
		  (concat
		   (if first-message
		       (substitute-command-keys first-message))
		   (if first-message "  ")
		   ;; If the help buffer will go in a separate frame,
		   ;; it's no use mentioning a command to scroll, so don't.
		   (if (or pop-up-windows
			   (special-display-p (buffer-name standard-output)))
		       nil
		     (if (same-window-p (buffer-name standard-output))
			 ;; Say how to scroll this window.
			 (substitute-command-keys
			  "\\[scroll-up] to scroll the help.")
		       ;; Say how to scroll some other window.
		       (substitute-command-keys
			"\\[scroll-other-window] to scroll the help."))))))))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

(defalias 'help 'help-for-help-internal)
;; find-function can find this.
(defalias 'help-for-help 'help-for-help-internal)
;; It can't find this, but nobody will look.
(make-help-screen help-for-help-internal
  (purecopy "Type a help option: [abcCdefFgiIkKlLmnprstvw.] C-[cdefmnoptw] or ?")
  ;; Don't purecopy this one, because it's not evaluated (it's
  ;; directly used as a docstring in a function definition, so it'll
  ;; be moved to the DOC file anyway: no need for purecopying it).
  "You have typed %THIS-KEY%, the help character.  Type a Help option:
\(Use SPC or DEL to scroll through this text.  Type \\<help-map>\\[help-quit] to exit the Help command.)

a PATTERN   Show commands whose name matches the PATTERN (a list of words
              or a regexp).  See also the `apropos' command.
b           Display all key bindings.
c KEYS      Display the command name run by the given key sequence.
C CODING    Describe the given coding system, or RET for current ones.
d PATTERN   Show a list of functions, variables, and other items whose
              documentation matches the PATTERN (a list of words or a regexp).
e           Go to the *Messages* buffer which logs echo-area messages.
f FUNCTION  Display documentation for the given function.
F COMMAND   Show the Emacs manual's section that describes the command.
g           Display information about the GNU project.
h           Display the HELLO file which illustrates various scripts.
i           Start the Info documentation reader: read included manuals.
I METHOD    Describe a specific input method, or RET for current.
k KEYS      Display the full documentation for the key sequence.
K KEYS      Show the Emacs manual's section for the command bound to KEYS.
l           Show last 300 input keystrokes (lossage).
L LANG-ENV  Describes a specific language environment, or RET for current.
m           Display documentation of current minor modes and current major mode,
              including their special commands.
n           Display news of recent Emacs changes.
o SYMBOL    Display the given function or variable's documentation and value.
p TOPIC     Find packages matching a given topic keyword.
P PACKAGE   Describe the given Emacs Lisp package.
r           Display the Emacs manual in Info mode.
s           Display contents of current syntax table, plus explanations.
S SYMBOL    Show the section for the given symbol in the Info manual
              for the programming language used in this buffer.
t           Start the Emacs learn-by-doing tutorial.
v VARIABLE  Display the given variable's documentation and value.
w COMMAND   Display which keystrokes invoke the given command (where-is).
.           Display any available local help at point in the echo area.

C-a         Information about Emacs.
C-c         Emacs copying permission (GNU General Public License).
C-d         Instructions for debugging GNU Emacs.
C-e         External packages and information about Emacs.
C-f         Emacs FAQ.
C-m         How to order printed Emacs manuals.
C-n         News of recent Emacs changes.
C-o         Emacs ordering and distribution information.
C-p         Info about known Emacs problems.
C-s         Search forward \"help window\".
C-t         Emacs TODO list.
C-w         Information on absence of warranty for GNU Emacs."
  help-map)



(defun function-called-at-point ()
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
            (save-excursion
              (or (not (zerop (skip-syntax-backward "_w")))
                  (eq (char-syntax (following-char)) ?w)
                  (eq (char-syntax (following-char)) ?_)
                  (forward-sexp -1))
              (skip-chars-forward "'")
              (let ((obj (read (current-buffer))))
                (and (symbolp obj) (fboundp obj) obj)))
          (error nil))
        (condition-case ()
            (save-excursion
              (save-restriction
                (narrow-to-region (max (point-min)
                                       (- (point) 1000)) (point-max))
                ;; Move up to surrounding paren, then after the open.
                (backward-up-list 1)
                (forward-char 1)
                ;; If there is space here, this is probably something
                ;; other than a real Lisp function call, so ignore it.
                (if (looking-at "[ \t]")
                    (error "Probably not a Lisp function call"))
                (let ((obj (read (current-buffer))))
                  (and (symbolp obj) (fboundp obj) obj))))
          (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (fboundp sym))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (fboundp sym) sym))))))))


;;; `User' help functions

(defun view-help-file (file &optional dir)
  (view-file (expand-file-name file (or dir data-directory)))
  (goto-address-mode 1)
  (goto-char (point-min)))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (view-help-file "DISTRIB"))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (view-help-file "COPYING"))

;; Maybe this command should just be removed.
(defun describe-gnu-project ()
  "Browse online information on the GNU project."
  (interactive)
  (browse-url "https://www.gnu.org/gnu/thegnuproject.html"))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "Disclaimer of Warranty")
    (forward-line 0)
    (recenter 0)))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let ((key (this-command-keys)))
    (describe-bindings
     (if (stringp key)
	 (substring key 0 (1- (length key)))
       (let ((prefix (make-vector (1- (length key)) nil))
	     (i 0))
	 (while (< i (length prefix))
	   (aset prefix i (aref key i))
	   (setq i (1+ i)))
	 prefix)))))
;; Make C-h after a prefix, when not specifically bound,
;; run describe-prefix-bindings.
(setq prefix-help-command 'describe-prefix-bindings)

(defun view-emacs-news (&optional version)
  "Display info on recent changes to Emacs.
With argument, display info only for the selected version."
  (interactive "P")
  (unless version
    (setq version emacs-major-version))
  (when (consp version)
    (let* ((all-versions
	    (let (res)
	      (mapc
	       (lambda (file)
		 (with-temp-buffer
		   (insert-file-contents
		    (expand-file-name file data-directory))
		   (while (re-search-forward
			   (if (member file '("NEWS.18" "NEWS.1-17"))
			       "Changes in \\(?:Emacs\\|version\\)?[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)"
			     "^\\* [^0-9\n]*\\([0-9]+\\.[0-9]+\\)") nil t)
		     (setq res (cons (match-string-no-properties 1) res)))))
	       (cons "NEWS"
		     (directory-files data-directory nil
				      "^NEWS\\.[0-9][-0-9]*$" nil)))
	      (sort (delete-dups res) #'string>)))
	   (current (car all-versions)))
      (setq version (completing-read
		     (format "Read NEWS for the version (default %s): " current)
		     all-versions nil nil nil nil current))
      (if (integerp (string-to-number version))
	  (setq version (string-to-number version))
	(unless (or (member version all-versions)
		    (<= (string-to-number version) (string-to-number current)))
	  (error "No news about version %s" version)))))
  (when (integerp version)
    (cond ((<= version 12)
	   (setq version (format "1.%d" version)))
	  ((<= version 18)
	   (setq version (format "%d" version)))
	  ((> version emacs-major-version)
	   (error "No news about Emacs %d (yet)" version))))
  (let* ((vn (if (stringp version)
		 (string-to-number version)
	       version))
	 (file (cond
		((>= vn emacs-major-version) "NEWS")
		((< vn 18) "NEWS.1-17")
		(t (format "NEWS.%d" vn))))
	 res)
    (view-file (expand-file-name file data-directory))
    (widen)
    (goto-char (point-min))
    (when (stringp version)
      (when (re-search-forward
	     (concat (if (< vn 19)
			 "Changes in Emacs[ \t]*"
		       "^\\* [^0-9\n]*") version "$")
	     nil t)
	(beginning-of-line)
	(narrow-to-region
	 (point)
	 (save-excursion
	   (while (and (setq res
			     (re-search-forward
			      (if (< vn 19)
				  "Changes in \\(?:Emacs\\|version\\)?[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)"
				"^\\* [^0-9\n]*\\([0-9]+\\.[0-9]+\\)") nil t))
		       (equal (match-string-no-properties 1) version)))
	   (or res (goto-char (point-max)))
	   (beginning-of-line)
	   (point)))))))

(defun view-emacs-todo (&optional _arg)
  "Display the Emacs TODO list."
  (interactive "P")
  (view-help-file "TODO"))

(defun view-echo-area-messages ()
  "View the log of recent echo-area messages: the `*Messages*' buffer.
The number of messages retained in that buffer
is specified by the variable `message-log-max'."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (display-buffer (current-buffer))))

(defun view-order-manuals ()
  "Display information on how to buy printed copies of Emacs manuals."
  (interactive)
;;  (view-help-file "ORDERS")
  (info "(emacs)Printed Books"))

(defun view-emacs-FAQ ()
  "Display the Emacs Frequently Asked Questions (FAQ) file."
  (interactive)
  ;; (find-file-read-only (expand-file-name "FAQ" data-directory))
  (info "(efaq)"))

(defun view-emacs-problems ()
  "Display info on known problems with Emacs and possible workarounds."
  (interactive)
  (view-help-file "PROBLEMS"))

(defun view-emacs-debugging ()
  "Display info on how to debug Emacs problems."
  (interactive)
  (view-help-file "DEBUG"))

;; This used to visit MORE.STUFF; maybe it should just be removed.
(defun view-external-packages ()
  "Display info on where to get more Emacs packages."
  (interactive)
  (info "(efaq)Packages that do not come with Emacs"))

(defun view-lossage ()
  "Display last few input keystrokes and the commands run.
For convenience this uses the same format as
`edit-last-kbd-macro'.

To record all your input, use `open-dribble-file'."
  (interactive)
  (help-setup-xref (list #'view-lossage)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ " ")
    (princ (mapconcat (lambda (key)
			(cond
			 ((and (consp key) (null (car key)))
			  (format ";; %s\n" (if (symbolp (cdr key)) (cdr key)
					      "anonymous-command")))
			 ((or (integerp key) (symbolp key) (listp key))
			  (single-key-description key))
			 (t
			  (prin1-to-string key nil))))
		      (recent-keys 'include-cmds)
		      " "))
    (with-current-buffer standard-output
      (goto-char (point-min))
      (let ((comment-start ";; ")
            (comment-column 24))
        (while (not (eobp))
          (comment-indent)
	  (forward-line 1)))
      ;; jidanni wants to see the last keystrokes immediately.
      (set-marker help-window-point-marker (point)))))


;; Key bindings

(defun describe-bindings (&optional prefix buffer)
  "Display a buffer showing a list of all defined keys, and their definitions.
The keys are displayed in order of precedence.

The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument BUFFER specifies which buffer's bindings
to display (default, the current buffer).  BUFFER can be a buffer
or a buffer name."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (help-setup-xref (list #'describe-bindings prefix buffer)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    ;; Be aware that `describe-buffer-bindings' puts its output into
    ;; the current buffer.
    (with-current-buffer (help-buffer)
      (describe-buffer-bindings buffer prefix))))

;; This function used to be in keymap.c.
(defun describe-bindings-internal (&optional menus prefix)
  "Show a list of all defined keys, and their definitions.
We put that list in a buffer, and display the buffer.

The optional argument MENUS, if non-nil, says to mention menu bindings.
\(Ordinarily these are omitted from the output.)
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix."
  (declare (obsolete describe-buffer-bindings "24.4"))
  (let ((buf (current-buffer)))
    (with-help-window (help-buffer)
      ;; Be aware that `describe-buffer-bindings' puts its output into
      ;; the current buffer.
      (with-current-buffer (help-buffer)
	(describe-buffer-bindings buf prefix menus)))))

(defun where-is (definition &optional insert)
  "Print message listing key sequences that invoke the command DEFINITION.
Argument is a command definition, usually a symbol with a function definition.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
		(if fn
		    (format "Where is command (default %s): " fn)
		  "Where is command: ")
		obarray 'commandp t nil nil
		(and fn (symbol-name fn))))
     (list (unless (equal val "") (intern val))
	   current-prefix-arg)))
  (unless definition (error "No command"))
  (let ((func (indirect-function definition))
        (defs nil)
        (standard-output (if insert (current-buffer) standard-output)))
    ;; In DEFS, find all symbols that are aliases for DEFINITION.
    (mapatoms (lambda (symbol)
		(and (fboundp symbol)
		     (not (eq symbol definition))
		     (eq func (condition-case ()
				  (indirect-function symbol)
				(error symbol)))
		     (push symbol defs))))
    ;; Look at all the symbols--first DEFINITION,
    ;; then its aliases.
    (dolist (symbol (cons definition defs))
      (let* ((remapped (command-remapping symbol))
	     (keys (where-is-internal
		    symbol overriding-local-map nil nil remapped))
	     (keys (mapconcat 'key-description keys ", "))
	     string)
	(setq string
	      (if insert
		  (if (> (length keys) 0)
		      (if remapped
			  (format "%s (%s) (remapped from %s)"
				  keys remapped symbol)
			(format "%s (%s)" keys symbol))
		    (format "M-x %s RET" symbol))
		(if (> (length keys) 0)
		    (if remapped
			(format "%s is remapped to %s which is on %s"
				symbol remapped keys)
		      (format "%s is on %s" symbol keys))
		  ;; If this is the command the user asked about,
		  ;; and it is not on any key, say so.
		  ;; For other symbols, its aliases, say nothing
		  ;; about them unless they are on keys.
		  (if (eq symbol definition)
		      (format "%s is not on any key" symbol)))))
	(when string
	  (unless (eq symbol definition)
	    (princ ";\n its alias "))
	  (princ string)))))
  nil)

(defun help-key-description (key untranslated)
  (let ((string (key-description key)))
    (if (or (not untranslated)
	    (and (eq (aref untranslated 0) ?\e) (not (eq (aref key 0) ?\e))))
	string
      (let ((otherstring (key-description untranslated)))
	(if (equal string otherstring)
	    string
	  (format "%s (translated from %s)" string otherstring))))))

(defun help--binding-undefined-p (defn)
  (or (null defn) (integerp defn) (equal defn 'undefined)))

(defun help--analyze-key (key untranslated)
  "Get information about KEY its corresponding UNTRANSLATED events.
Returns a list of the form (BRIEF-DESC DEFN EVENT MOUSE-MSG)."
  (if (numberp untranslated)
      (error "Missing `untranslated'!"))
  (let* ((event (when (> (length key) 0)
                  (aref key (if (and (symbolp (aref key 0))
		                     (> (length key) 1)
		                     (consp (aref key 1)))
                                ;; Look at the second event when the first
                                ;; is a pseudo-event like `mode-line' or
                                ;; `left-fringe'.
		                1
	                      0))))
	 (modifiers (event-modifiers event))
	 (mouse-msg (if (or (memq 'click modifiers) (memq 'down modifiers)
			    (memq 'drag modifiers))
                        " at that spot" ""))
	 (defn (key-binding key t)))
    ;; Handle the case where we faked an entry in "Select and Paste" menu.
    (when (and (eq defn nil)
	       (stringp (aref key (1- (length key))))
	       (eq (key-binding (substring key 0 -1)) 'yank-menu))
      (setq defn 'menu-bar-select-yank))
    ;; Don't bother user with strings from (e.g.) the select-paste menu.
    (when (stringp (aref key (1- (length key))))
      (aset key (1- (length key)) "(any string)"))
    (when (and untranslated
               (stringp (aref untranslated (1- (length untranslated)))))
      (aset untranslated (1- (length untranslated)) "(any string)"))
    (list
     ;; Now describe the key, perhaps as changed.
     (let ((key-desc (help-key-description key untranslated)))
       (if (help--binding-undefined-p defn)
           (format "%s%s is undefined" key-desc mouse-msg)
         (format "%s%s runs the command %S" key-desc mouse-msg defn)))
     defn event mouse-msg)))

(defun help--filter-info-list (info-list i)
  "Drop the undefined keys."
  (or
   ;; Remove all `undefined' keys.
   (delq nil (mapcar (lambda (x)
                       (unless (help--binding-undefined-p (nth i x)) x))
                     info-list))
   ;; If nothing left, then keep one (the last one).
   (last info-list)))

(defun describe-key-briefly (&optional key-list insert untranslated)
  "Print the name of the functions KEY-LIST invokes.
KEY-LIST is a list of pairs (SEQ . RAW-SEQ) of key sequences, where
RAW-SEQ is the untranslated form of the key sequence SEQ.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer.

While reading KEY-LIST interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them."
  (declare (advertised-calling-convention (key-list &optional insert) "27.1"))
  (interactive
   ;; Ignore mouse movement events because it's too easy to miss the
   ;; message while moving the mouse.
   (let ((key-list (help--read-key-sequence 'no-mouse-movement)))
     `(,key-list ,current-prefix-arg)))
  (when (arrayp key-list)
    ;; Old calling convention, changed
    (setq key-list (list (cons key-list
                               (if (numberp untranslated)
                                   (this-single-command-raw-keys)
                                 untranslated)))))
  (let* ((info-list (mapcar (lambda (kr)
                              (help--analyze-key (car kr) (cdr kr)))
                            key-list))
         (msg (mapconcat #'car (help--filter-info-list info-list 1) "\n")))
    (if insert (insert msg) (message "%s" msg))))

(defun help--key-binding-keymap (key &optional accept-default no-remap position)
  "Return a keymap holding a binding for KEY within current keymaps.
The effect of the arguments KEY, ACCEPT-DEFAULT, NO-REMAP and
POSITION is as documented in the function `key-binding'."
  (let* ((active-maps (current-active-maps t position))
         map found)
    ;; We loop over active maps like key-binding does.
    (while (and
            (not found)
            (setq map (pop active-maps)))
      (setq found (lookup-key map key accept-default))
      (when (integerp found)
        ;; The first `found' characters of KEY were found but not the
        ;; whole sequence.
        (setq found nil)))
    (when found
      (if (and (symbolp found)
               (not no-remap)
               (command-remapping found))
          ;; The user might want to know in which map the binding is
          ;; found, or in which map the remapping is found.  The
          ;; default is to show the latter.
          (help--key-binding-keymap (vector 'remap found))
        map))))

(defun help--binding-locus (key position)
  "Describe in which keymap KEY is defined.
Return a symbol pointing to that keymap if one exists ; otherwise
return nil.  The argument POSITION is as documented in the
function `key-binding'."
  (let ((map (help--key-binding-keymap key t nil position)))
    (when map
      (catch 'found
        (let ((advertised-syms (nconc
                                (list 'overriding-terminal-local-map
                                      'overriding-local-map)
                                (delq nil
                                      (mapcar
                                       (lambda (mode-and-map)
                                         (let ((mode (car mode-and-map)))
                                           (when (symbol-value mode)
                                             (intern-soft
                                              (format "%s-map" mode)))))
                                       minor-mode-map-alist))
                                (list 'global-map
                                      (intern-soft (format "%s-map" major-mode))))))
          ;; Look into these advertised symbols first.
          (dolist (sym advertised-syms)
            (when (and
                   (boundp sym)
                   (eq map (symbol-value sym)))
              (throw 'found sym)))
          ;; Only look in other symbols otherwise.
          (mapatoms
           (lambda (x)
             (when (and (boundp x)
                        ;; Avoid let-bound symbols.
                        (special-variable-p x)
                        (eq (symbol-value x) map))
               (throw 'found x))))
          nil)))))

(defun help--read-key-sequence (&optional no-mouse-movement)
  "Read a key sequence from the user.
Usually reads a single key sequence, except when that sequence might
hide another one (e.g. a down event, where the user is interested
in getting info about the up event, or a click event, where the user
wants to get info about the double click).
Return a list of elements of the form (SEQ . RAW-SEQ), where SEQ is a key
sequence, and RAW-SEQ is its untranslated form.
If NO-MOUSE-MOVEMENT is non-nil, ignore key sequences starting
with `mouse-movement' events."
  (let ((enable-disabled-menus-and-buttons t)
        (cursor-in-echo-area t)
        saved-yank-menu)
    (unwind-protect
        (let (last-modifiers key-list)
          ;; If yank-menu is empty, populate it temporarily, so that
          ;; "Select and Paste" menu can generate a complete event.
          (when (null (cdr yank-menu))
            (setq saved-yank-menu (copy-sequence yank-menu))
            (menu-bar-update-yank-menu "(any string)" nil))
          (while
              ;; Read at least one key-sequence.
              (or (null key-list)
                  ;; After a down event, also read the (presumably) following
                  ;; up-event.
                  (memq 'down last-modifiers)
                  ;; After a click, see if a double click is on the way.
                  (and (memq 'click last-modifiers)
                       (not (sit-for (/ double-click-time 1000.0) t))))
            (let* ((seq (read-key-sequence "\
Describe the following key, mouse click, or menu item: "))
                   (raw-seq (this-single-command-raw-keys))
                   (keyn (when (> (length seq) 0)
                           (aref seq (1- (length seq)))))
                   (base (event-basic-type keyn))
                   (modifiers (event-modifiers keyn)))
              (cond
               ((zerop (length seq)))   ;FIXME: Can this happen?
               ((and no-mouse-movement (eq base 'mouse-movement)) nil)
               ((eq base 'help-echo) nil)
               (t
                (setq last-modifiers modifiers)
                (push (cons seq raw-seq) key-list)))))
          (nreverse key-list))
      ;; Put yank-menu back as it was, if we changed it.
      (when saved-yank-menu
        (setq yank-menu (copy-sequence saved-yank-menu))
        (fset 'yank-menu (cons 'keymap yank-menu))))))

(defun describe-key (&optional key-list buffer up-event)
  "Display documentation of the function invoked by KEY-LIST.
KEY-LIST can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.  When calling from a program,
pass KEY-LIST as a list of elements (SEQ . RAW-SEQ) where SEQ is
a key-sequence and RAW-SEQ is its untranslated form.

While reading KEY-LIST interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them.

BUFFER is the buffer in which to lookup those keys; it defaults to the
current buffer."
  (declare (advertised-calling-convention (key-list &optional buffer) "27.1"))
  (interactive (list (help--read-key-sequence)))
  (when (arrayp key-list)
    ;; Compatibility with old calling convention.
    (setq key-list (cons (list key-list) (if up-event (list up-event))))
    (when buffer
      (let ((raw (if (numberp buffer) (this-single-command-raw-keys) buffer)))
        (setf (cdar (last key-list)) raw)))
    (setq buffer nil))
  (let* ((buf (or buffer (current-buffer)))
         (on-link
          (mapcar (lambda (kr)
                    (let ((raw (cdr kr)))
                      (and (not (memq mouse-1-click-follows-link '(nil double)))
                           (> (length raw) 0)
                           (eq (car-safe (aref raw 0)) 'mouse-1)
                           (with-current-buffer buf
                             (mouse-on-link-p (event-start (aref raw 0)))))))
                  key-list))
         (info-list
          (help--filter-info-list
           (with-current-buffer buf
             (mapcar (lambda (x)
                       (pcase-let* ((`(,seq . ,raw-seq) x)
                                    (`(,brief-desc ,defn ,event ,_mouse-msg)
                                     (help--analyze-key seq raw-seq))
                                    (locus
                                     (help--binding-locus
                                      seq (event-start event))))
                         `(,seq ,brief-desc ,defn ,locus)))
                     key-list))
           2)))
    (help-setup-xref (list (lambda (key-list buf)
                             (describe-key key-list
                                           (if (buffer-live-p buf) buf)))
                           key-list buf)
		     (called-interactively-p 'interactive))
    (if (and (<= (length info-list) 1)
             (help--binding-undefined-p (nth 2 (car info-list))))
        (message "%s" (nth 1 (car info-list)))
      (with-help-window (help-buffer)
        (when (> (length info-list) 1)
          ;; FIXME: Make this into clickable hyperlinks.
          (princ "There were several key-sequences:\n\n")
          (princ (mapconcat (lambda (info)
                              (pcase-let ((`(,_seq ,brief-desc ,_defn ,_locus)
                                           info))
                                (concat "  " brief-desc)))
                            info-list
                            "\n"))
          (when (delq nil on-link)
            (princ "\n\nThose are influenced by `mouse-1-click-follows-link'"))
          (princ "\n\nThey're all described below."))
        (pcase-dolist (`(,_seq ,brief-desc ,defn ,locus)
                       info-list)
          (when defn
            (when (> (length info-list) 1)
              (with-current-buffer standard-output
                (insert "\n\n"
                        ;; FIXME: Can't use eval-when-compile because purified
                        ;; strings lose their text properties :-(
                        (propertize "\n" 'face '(:height 0.1 :inverse-video t))
                        "\n")))

            (princ brief-desc)
            (when locus
              (princ (format " (found in %s)" locus)))
            (princ ", which is ")
	    (describe-function-1 defn)))))))

(defun describe-mode (&optional buffer)
  "Display documentation of current major mode and minor modes.
A brief summary of the minor modes comes first, followed by the
major mode description.  This is followed by detailed
descriptions of the minor modes, each on a separate page.

For this to work correctly for a minor mode, the mode's indicator
variable \(listed in `minor-mode-alist') must also be a function
whose documentation describes the minor mode.

If called from Lisp with a non-nil BUFFER argument, display
documentation for the major and minor modes of that buffer."
  (interactive "@")
  (unless buffer (setq buffer (current-buffer)))
  (help-setup-xref (list #'describe-mode buffer)
		   (called-interactively-p 'interactive))
  ;; For the sake of help-do-xref and help-xref-go-back,
  ;; don't switch buffers before calling `help-buffer'.
  (with-help-window (help-buffer)
    (with-current-buffer buffer
      (let (minor-modes)
	;; Older packages do not register in minor-mode-list but only in
	;; minor-mode-alist.
	(dolist (x minor-mode-alist)
	  (setq x (car x))
	  (unless (memq x minor-mode-list)
	    (push x minor-mode-list)))
	;; Find enabled minor mode we will want to mention.
	(dolist (mode minor-mode-list)
	  ;; Document a minor mode if it is listed in minor-mode-alist,
	  ;; non-nil, and has a function definition.
	  (let ((fmode (or (get mode :minor-mode-function) mode)))
	    (and (boundp mode) (symbol-value mode)
		 (fboundp fmode)
		 (let ((pretty-minor-mode
			(if (string-match "\\(\\(-minor\\)?-mode\\)?\\'"
					  (symbol-name fmode))
			    (capitalize
			     (substring (symbol-name fmode)
					0 (match-beginning 0)))
			  fmode)))
		   (push (list fmode pretty-minor-mode
			       (format-mode-line (assq mode minor-mode-alist)))
			 minor-modes)))))
	(setq minor-modes
	      (sort minor-modes
		    (lambda (a b) (string-lessp (cadr a) (cadr b)))))
	(when minor-modes
	  (princ "Enabled minor modes:\n")
	  (make-local-variable 'help-button-cache)
	  (with-current-buffer standard-output
	    (dolist (mode minor-modes)
	      (let ((mode-function (nth 0 mode))
		    (pretty-minor-mode (nth 1 mode))
		    (indicator (nth 2 mode)))
		(save-excursion
		  (goto-char (point-max))
		  (princ "\n\f\n")
		  (push (point-marker) help-button-cache)
		  ;; Document the minor modes fully.
                  (insert-text-button
                   pretty-minor-mode 'type 'help-function
                   'help-args (list mode-function)
                   'button '(t))
		  (princ (format " minor mode (%s):\n"
				 (if (zerop (length indicator))
				     "no indicator"
				   (format "indicator%s"
					   indicator))))
		  (princ (documentation mode-function)))
		(insert-button pretty-minor-mode
			       'action (car help-button-cache)
			       'follow-link t
			       'help-echo "mouse-2, RET: show full information")
		(newline)))
	    (forward-line -1)
	    (fill-paragraph nil)
	    (forward-line 1))

	  (princ "\n(Information about these minor modes follows the major mode info.)\n\n"))
	;; Document the major mode.
	(let ((mode mode-name))
	  (with-current-buffer standard-output
            (let ((start (point)))
              (insert (format-mode-line mode nil nil buffer))
              (add-text-properties start (point) '(face bold)))))
	(princ " mode")
	(let* ((mode major-mode)
	       (file-name (find-lisp-object-file-name mode nil)))
	  (when file-name
	    (princ (format-message " defined in `%s'"
                                   (file-name-nondirectory file-name)))
	    ;; Make a hyperlink to the library.
	    (with-current-buffer standard-output
	      (save-excursion
		(re-search-backward (substitute-command-keys "`\\([^`']+\\)'")
                                    nil t)
		(help-xref-button 1 'help-function-def mode file-name)))))
	(princ ":\n")
	(princ (documentation major-mode)))))
  ;; For the sake of IELM and maybe others
  nil)

(defun search-forward-help-for-help ()
  "Search forward \"help window\"."
  (interactive)
  ;; Move cursor to the "help window".
  (pop-to-buffer " *Metahelp*")
  ;; Do incremental search forward.
  (isearch-forward nil t))

(defun describe-minor-mode (minor-mode)
  "Display documentation of a minor mode given as MINOR-MODE.
MINOR-MODE can be a minor mode symbol or a minor mode indicator string
appeared on the mode-line."
  (interactive (list (completing-read
		      "Minor mode: "
			      (nconc
			       (describe-minor-mode-completion-table-for-symbol)
			       (describe-minor-mode-completion-table-for-indicator)
			       ))))
  (if (symbolp minor-mode)
      (setq minor-mode (symbol-name minor-mode)))
  (let ((symbols (describe-minor-mode-completion-table-for-symbol))
	(indicators (describe-minor-mode-completion-table-for-indicator)))
    (cond
     ((member minor-mode symbols)
      (describe-minor-mode-from-symbol (intern minor-mode)))
     ((member minor-mode indicators)
      (describe-minor-mode-from-indicator minor-mode))
     (t
      (error "No such minor mode: %s" minor-mode)))))

;; symbol
(defun describe-minor-mode-completion-table-for-symbol ()
  ;; In order to list up all minor modes, minor-mode-list
  ;; is used here instead of minor-mode-alist.
  (delq nil (mapcar 'symbol-name minor-mode-list)))

(defun describe-minor-mode-from-symbol (symbol)
  "Display documentation of a minor mode given as a symbol, SYMBOL"
  (interactive (list (intern (completing-read
			      "Minor mode symbol: "
			      (describe-minor-mode-completion-table-for-symbol)))))
  (if (fboundp symbol)
      (describe-function symbol)
    (describe-variable symbol)))

;; indicator
(defun describe-minor-mode-completion-table-for-indicator ()
  (delq nil
	(mapcar (lambda (x)
		  (let ((i (format-mode-line x)))
		    ;; remove first space if existed
		    (cond
		     ((= 0 (length i))
		      nil)
		     ((eq (aref i 0) ?\s)
		      (substring i 1))
		     (t
		      i))))
		minor-mode-alist)))

(defun describe-minor-mode-from-indicator (indicator)
  "Display documentation of a minor mode specified by INDICATOR.
If you call this function interactively, you can give indicator which
is currently activated with completion."
  (interactive (list
		(completing-read
		 "Minor mode indicator: "
		 (describe-minor-mode-completion-table-for-indicator))))
  (let ((minor-mode (lookup-minor-mode-from-indicator indicator)))
    (if minor-mode
	(describe-minor-mode-from-symbol minor-mode)
      (error "Cannot find minor mode for `%s'" indicator))))

(defun lookup-minor-mode-from-indicator (indicator)
  "Return a minor mode symbol from its indicator on the mode line."
  ;; remove first space if existed
  (if (and (< 0 (length indicator))
	   (eq (aref indicator 0) ?\s))
      (setq indicator (substring indicator 1)))
  (let ((minor-modes minor-mode-alist)
	result)
    (while minor-modes
      (let* ((minor-mode (car (car minor-modes)))
	     (anindicator (format-mode-line
			   (car (cdr (car minor-modes))))))
	;; remove first space if existed
	(if (and (stringp anindicator)
		 (> (length anindicator) 0)
		 (eq (aref anindicator 0) ?\s))
	    (setq anindicator (substring anindicator 1)))
	(if (equal indicator anindicator)
	    (setq result minor-mode
		  minor-modes nil)
	  (setq minor-modes (cdr minor-modes)))))
    result))

(declare-function x-display-pixel-height "xfns.c" (&optional terminal))
(declare-function x-display-pixel-width "xfns.c" (&optional terminal))

;;; Automatic resizing of temporary buffers.
(defcustom temp-buffer-max-height
  (lambda (_buffer)
    (if (and (display-graphic-p) (eq (selected-window) (frame-root-window)))
	(/ (x-display-pixel-height) (frame-char-height) 2)
      (/ (- (frame-height) 2) 2)))
  "Maximum height of a window displaying a temporary buffer.
This is effective only when Temp Buffer Resize mode is enabled.
The value is the maximum height (in lines) which
`resize-temp-buffer-window' will give to a window displaying a
temporary buffer.  It can also be a function to be called to
choose the height for such a buffer.  It gets one argument, the
buffer, and should return a positive integer.  At the time the
function is called, the window to be resized is selected."
  :type '(choice integer function)
  :group 'help
  :version "24.3")

(defcustom temp-buffer-max-width
  (lambda (_buffer)
    (if (and (display-graphic-p) (eq (selected-window) (frame-root-window)))
	(/ (x-display-pixel-width) (frame-char-width) 2)
      (/ (- (frame-width) 2) 2)))
  "Maximum width of a window displaying a temporary buffer.
This is effective only when Temp Buffer Resize mode is enabled.
The value is the maximum width (in columns) which
`resize-temp-buffer-window' will give to a window displaying a
temporary buffer.  It can also be a function to be called to
choose the width for such a buffer.  It gets one argument, the
buffer, and should return a positive integer.  At the time the
function is called, the window to be resized is selected."
  :type '(choice integer function)
  :group 'help
  :version "24.4")

(define-minor-mode temp-buffer-resize-mode
  "Toggle auto-resizing temporary buffer windows (Temp Buffer Resize Mode).
With a prefix argument ARG, enable Temp Buffer Resize mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Temp Buffer Resize mode is enabled, the windows in which we
show a temporary buffer are automatically resized in height to
fit the buffer's contents, but never more than
`temp-buffer-max-height' nor less than `window-min-height'.

A window is resized only if it has been specially created for the
buffer.  Windows that have shown another buffer before are not
resized.  A frame is resized only if `fit-frame-to-buffer' is
non-nil.

This mode is used by `help', `apropos' and `completion' buffers,
and some others."
  :global t :group 'help
  (if temp-buffer-resize-mode
      ;; `help-make-xrefs' may add a `back' button and thus increase the
      ;; text size, so `resize-temp-buffer-window' must be run *after* it.
      (add-hook 'temp-buffer-show-hook 'resize-temp-buffer-window 'append)
    (remove-hook 'temp-buffer-show-hook 'resize-temp-buffer-window)))

(defun resize-temp-buffer-window (&optional window)
  "Resize WINDOW to fit its contents.
WINDOW must be a live window and defaults to the selected one.
Do not resize if WINDOW was not created by `display-buffer'.

If WINDOW is part of a vertical combination, restrain its new
size by `temp-buffer-max-height' and do not resize if its minimum
accessible position is scrolled out of view.  If WINDOW is part
of a horizontal combination, restrain its new size by
`temp-buffer-max-width'.  In both cases, the value of the option
`fit-window-to-buffer-horizontally' can inhibit resizing.

If WINDOW is the root window of its frame, resize the frame
provided `fit-frame-to-buffer' is non-nil.

This function may call `preserve-window-size' to preserve the
size of WINDOW."
  (setq window (window-normalize-window window t))
  (let ((height (if (functionp temp-buffer-max-height)
		    (with-selected-window window
		      (funcall temp-buffer-max-height (window-buffer)))
		  temp-buffer-max-height))
	(width (if (functionp temp-buffer-max-width)
		   (with-selected-window window
		     (funcall temp-buffer-max-width (window-buffer)))
		 temp-buffer-max-width))
	(quit-cadr (cadr (window-parameter window 'quit-restore))))
    ;; Resize WINDOW iff it was made by `display-buffer'.
    (when (or (and (eq quit-cadr 'window)
		   (or (and (window-combined-p window)
			    (not (eq fit-window-to-buffer-horizontally
				     'only))
			    (pos-visible-in-window-p (point-min) window))
		       (and (window-combined-p window t)
			    fit-window-to-buffer-horizontally)))
	      (and (eq quit-cadr 'frame)
                   fit-frame-to-buffer
                   (eq window (frame-root-window window))))
	(fit-window-to-buffer window height nil width nil t))))

;;; Help windows.
(defcustom help-window-select nil
  "Non-nil means select help window for viewing.
Choices are:

 never (nil) Select help window only if there is no other window
             on its frame.

 other       Select help window if and only if it appears on the
             previously selected frame, that frame contains at
             least two other windows and the help window is
             either new or showed a different buffer before.

 always (t)  Always select the help window.

If this option is non-nil and the help window appears on another
frame, then give that frame input focus too.  Note also that if
the help window appears on another frame, it may get selected and
its frame get input focus even if this option is nil.

This option has effect if and only if the help window was created
by `with-help-window'."
  :type '(choice (const :tag "never (nil)" nil)
		 (const :tag "other" other)
		 (const :tag "always (t)" t))
  :group 'help
  :version "23.1")

(defcustom help-enable-auto-load t
  "Whether Help commands can perform autoloading.
If non-nil, whenever \\[describe-function] is called for an
autoloaded function whose docstring contains any key substitution
construct (see `substitute-command-keys'), the library is loaded,
so that the documentation can show the right key bindings."
  :type 'boolean
  :group 'help
  :version "24.3")

(defun help-window-display-message (quit-part window &optional scroll)
  "Display message telling how to quit and scroll help window.
QUIT-PART is a string telling how to quit the help window WINDOW.
Optional argument SCROLL non-nil means tell how to scroll WINDOW.
SCROLL equal `other' means tell how to scroll the \"other\"
window."
  (let ((scroll-part
	 (cond
	  ;; If we don't have QUIT-PART we probably reuse a window
	  ;; showing the same buffer so we don't show any message.
	  ((not quit-part) nil)
	  ((pos-visible-in-window-p
	    (with-current-buffer (window-buffer window)
	      (point-max)) window t)
	   ;; Buffer end is at least partially visible, no need to talk
	   ;; about scrolling.
	   ".")
	  ((eq scroll 'other)
	   ", \\[scroll-other-window] to scroll help.")
	  (scroll ", \\[scroll-up] to scroll help."))))
    (message "%s"
     (substitute-command-keys (concat quit-part scroll-part)))))

(defun help-window-setup (window &optional value)
  "Set up help window WINDOW for `with-help-window'.
WINDOW is the window used for displaying the help buffer.
Return VALUE."
  (let* ((help-buffer (when (window-live-p window)
			(window-buffer window)))
	 (help-setup (when (window-live-p window)
		       (car (window-parameter window 'quit-restore))))
	 (frame (window-frame window)))

    (when help-buffer
      ;; Handle `help-window-point-marker'.
      (when (eq (marker-buffer help-window-point-marker) help-buffer)
	(set-window-point window help-window-point-marker)
	;; Reset `help-window-point-marker'.
	(set-marker help-window-point-marker nil))

      ;; If the help window appears on another frame, select it if
      ;; `help-window-select' is non-nil and give that frame input focus
      ;; too.  See also Bug#19012.
      (when (and help-window-select
		 (frame-live-p help-window-old-frame)
		 (not (eq frame help-window-old-frame)))
	(select-window window)
	(select-frame-set-input-focus frame))

      (cond
       ((or (eq window (selected-window))
	    ;; If the help window is on the selected frame, select
	    ;; it if `help-window-select' is t or `help-window-select'
	    ;; is 'other, the frame contains at least three windows, and
	    ;; the help window did show another buffer before.  See also
	    ;; Bug#11039.
	    (and (eq frame (selected-frame))
		 (or (eq help-window-select t)
		     (and (eq help-window-select 'other)
			  (> (length (window-list nil 'no-mini)) 2)
			  (not (eq help-setup 'same))))
		 (select-window window)))
	;; The help window is or gets selected ...
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   ;; ... and is new, ...
	   "Type \"q\" to delete help window")
	  ((eq help-setup 'frame)
	   ;; ... on a new frame, ...
	   "Type \"q\" to quit the help frame")
	  ((eq help-setup 'other)
	   ;; ... or displayed some other buffer before.
	   "Type \"q\" to restore previous buffer"))
	 window t))
       ((and (eq (window-frame window) help-window-old-frame)
	     (= (length (window-list nil 'no-mini)) 2))
	;; There are two windows on the help window's frame and the
	;; other one is the selected one.
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   "Type \\[delete-other-windows] to delete the help window")
	  ((eq help-setup 'other)
	   "Type \"q\" in help window to restore its previous buffer"))
	 window 'other))
       (t
	;; The help window is not selected ...
	(help-window-display-message
	 (cond
	  ((eq help-setup 'window)
	   ;; ... and is new, ...
	   "Type \"q\" in help window to delete it")
	  ((eq help-setup 'other)
	   ;; ... or displayed some other buffer before.
	   "Type \"q\" in help window to restore previous buffer"))
	 window))))
    ;; Return VALUE.
    value))

;; `with-help-window' is a wrapper for `with-temp-buffer-window'
;; providing the following additional twists:

;; (1) It puts the buffer in `help-mode' (via `help-mode-setup') and
;;     adds cross references (via `help-mode-finish').

;; (2) It issues a message telling how to scroll and quit the help
;;     window (via `help-window-setup').

;; (3) An option (customizable via `help-window-select') to select the
;;     help window automatically.

;; (4) A marker (`help-window-point-marker') to move point in the help
;;     window to an arbitrary buffer position.
(defmacro with-help-window (buffer-or-name &rest body)
  "Evaluate BODY, send output to BUFFER-OR-NAME and show in a help window.
This construct is like `with-temp-buffer-window' but unlike that
puts the buffer specified by BUFFER-OR-NAME in `help-mode' and
displays a message about how to delete the help window when it's no
longer needed.  The help window will be selected if
`help-window-select' is non-nil.  See `help-window-setup' for
more options."
  (declare (indent 1) (debug t))
  `(progn
     ;; Make `help-window-point-marker' point nowhere.  The only place
     ;; where this should be set to a buffer position is within BODY.
     (set-marker help-window-point-marker nil)
     (let ((temp-buffer-window-setup-hook
	    (cons 'help-mode-setup temp-buffer-window-setup-hook))
	   (temp-buffer-window-show-hook
	    (cons 'help-mode-finish temp-buffer-window-show-hook)))
       (setq help-window-old-frame (selected-frame))
       (with-temp-buffer-window
	,buffer-or-name nil 'help-window-setup (progn ,@body)))))

;; Called from C, on encountering `help-char' when reading a char.
;; Don't print to *Help*; that would clobber Help history.
(defun help-form-show ()
  "Display the output of a non-nil `help-form'."
  (let ((msg (eval help-form)))
    (if (stringp msg)
	(with-output-to-temp-buffer " *Char Help*"
	  (princ msg)))))


(defun help--docstring-quote (string)
  "Return a doc string that represents STRING.
The result, when formatted by `substitute-command-keys', should equal STRING."
  (replace-regexp-in-string "['\\`‘’]" "\\\\=\\&" string))

;; The following functions used to be in help-fns.el, which is not preloaded.
;; But for various reasons, they are more widely needed, so they were
;; moved to this file, which is preloaded.  https://debbugs.gnu.org/17001

(defun help-split-fundoc (docstring def)
  "Split a function DOCSTRING into the actual doc and the usage info.
Return (USAGE . DOC) or nil if there's no usage info, where USAGE info
is a string describing the argument list of DEF, such as
\"(apply FUNCTION &rest ARGUMENTS)\".
DEF is the function whose usage we're looking for in DOCSTRING."
  ;; Functions can get the calling sequence at the end of the doc string.
  ;; In cases where `function' has been fset to a subr we can't search for
  ;; function's name in the doc string so we use `fn' as the anonymous
  ;; function name instead.
  (when (and docstring (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring))
    (let ((doc (unless (zerop (match-beginning 0))
		 (substring docstring 0 (match-beginning 0))))
	  (usage-tail (match-string 1 docstring)))
      (cons (format "(%s%s"
		    ;; Replace `fn' with the actual function name.
		    (if (symbolp def)
			(help--docstring-quote (format "%S" def))
		      'anonymous)
		    usage-tail)
	    doc))))

(defun help-add-fundoc-usage (docstring arglist)
  "Add the usage info to DOCSTRING.
If DOCSTRING already has a usage info, then just return it unchanged.
The usage info is built from ARGLIST.  DOCSTRING can be nil.
ARGLIST can also be t or a string of the form \"(FUN ARG1 ARG2 ...)\"."
  (unless (stringp docstring) (setq docstring ""))
  (if (or (string-match "\n\n(fn\\(\\( .*\\)?)\\)\\'" docstring)
          (eq arglist t))
      docstring
    (concat docstring
	    (if (string-match "\n?\n\\'" docstring)
		(if (< (- (match-end 0) (match-beginning 0)) 2) "\n" "")
	      "\n\n")
	    (if (stringp arglist)
                (if (string-match "\\`[^ ]+\\(.*\\))\\'" arglist)
                    (concat "(fn" (match-string 1 arglist) ")")
                  (error "Unrecognized usage format"))
	      (help--make-usage-docstring 'fn arglist)))))

(defun help-function-arglist (def &optional preserve-names)
  "Return a formal argument list for the function DEF.
If PRESERVE-NAMES is non-nil, return a formal arglist that uses
the same names as used in the original source code, when possible."
  ;; Handle symbols aliased to other symbols.
  (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
  ;; Advice wrappers have "catch all" args, so fetch the actual underlying
  ;; function to find the real arguments.
  (while (advice--p def) (setq def (advice--cdr def)))
  ;; If definition is a macro, find the function inside it.
  (if (eq (car-safe def) 'macro) (setq def (cdr def)))
  (cond
   ((and (byte-code-function-p def) (listp (aref def 0))) (aref def 0))
   ((eq (car-safe def) 'lambda) (nth 1 def))
   ((eq (car-safe def) 'closure) (nth 2 def))
   ((or (and (byte-code-function-p def) (integerp (aref def 0)))
        (subrp def) (module-function-p def))
    (or (when preserve-names
          (let* ((doc (condition-case nil (documentation def) (error nil)))
                 (docargs (if doc (car (help-split-fundoc doc nil))))
                 (arglist (if docargs
                              (cdar (read-from-string (downcase docargs)))))
                 (valid t))
            ;; Check validity.
            (dolist (arg arglist)
              (unless (and (symbolp arg)
                           (let ((name (symbol-name arg)))
                             (if (eq (aref name 0) ?&)
                                 (memq arg '(&rest &optional))
                               (not (string-match "\\." name)))))
                (setq valid nil)))
            (when valid arglist)))
        (let* ((arity (func-arity def))
               (max (cdr arity))
               (min (car arity))
               (arglist ()))
          (dotimes (i min)
            (push (intern (concat "arg" (number-to-string (1+ i)))) arglist))
          (when (and (integerp max) (> max min))
            (push '&optional arglist)
            (dotimes (i (- max min))
              (push (intern (concat "arg" (number-to-string (+ 1 i min))))
                    arglist)))
          (unless (integerp max) (push '&rest arglist) (push 'rest arglist))
          (nreverse arglist))))
   ((and (autoloadp def) (not (eq (nth 4 def) 'keymap)))
    "[Arg list not available until function definition is loaded.]")
   (t t)))

(defun help--make-usage (function arglist)
  (cons (if (symbolp function) function 'anonymous)
	(mapcar (lambda (arg)
		  (if (not (symbolp arg)) arg
		    (let ((name (symbol-name arg)))
		      (cond
                       ((string-match "\\`&" name) arg)
                       ((string-match "\\`_." name)
                        (intern (upcase (substring name 1))))
                       (t (intern (upcase name)))))))
		arglist)))

(define-obsolete-function-alias 'help-make-usage 'help--make-usage "25.1")

(defun help--make-usage-docstring (fn arglist)
  (let ((print-escape-newlines t))
    (help--docstring-quote (format "%S" (help--make-usage fn arglist)))))


(provide 'help)

;;; help.el ends here
