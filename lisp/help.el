;;; help.el --- help commands for Emacs

;; Copyright (C) 1985, 1986, 1993, 1994, 1998, 1999, 2000, 2001, 2002
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal

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

;; This code implements GNU Emacs' on-line help system, the one invoked by
;; `M-x help-for-help'.

;;; Code:

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))

;; This makes `with-output-to-temp-buffer' buffers use `help-mode'.
(add-hook 'temp-buffer-setup-hook 'help-mode-setup)
(add-hook 'temp-buffer-show-hook 'help-mode-finish)

(defvar help-map (make-sparse-keymap)
  "Keymap for characters following the Help key.")

(define-key global-map (char-to-string help-char) 'help-command)
(define-key global-map [help] 'help-command)
(define-key global-map [f1] 'help-command)
(fset 'help-command help-map)

(define-key help-map (char-to-string help-char) 'help-for-help)
(define-key help-map [help] 'help-for-help)
(define-key help-map [f1] 'help-for-help)
(define-key help-map "?" 'help-for-help)

(define-key help-map "\C-c" 'describe-copying)
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-e" 'view-emacs-problems)
(define-key help-map "\C-f" 'view-emacs-FAQ)
(define-key help-map "\C-m" 'view-order-manuals)
(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "\C-p" 'describe-project)
(define-key help-map "\C-t" 'view-todo)
(define-key help-map "\C-w" 'describe-no-warranty)

;; This does not fit the pattern, but it is natural given the C-\ command.
(define-key help-map "\C-\\" 'describe-input-method)

(define-key help-map "C" 'describe-coding-system)
(define-key help-map "F" 'Info-goto-emacs-command-node)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "K" 'Info-goto-emacs-key-command-node)
(define-key help-map "L" 'describe-language-environment)
(define-key help-map "S" 'info-lookup-symbol)

(define-key help-map "a" 'apropos-command)

(define-key help-map "b" 'describe-bindings)

(define-key help-map "c" 'describe-key-briefly)

(define-key help-map "e" 'view-echo-area-messages)

(define-key help-map "f" 'describe-function)

(define-key help-map "h" 'view-hello-file)

(define-key help-map "i" 'info)
(define-key help-map "4i" 'info-other-window)

(define-key help-map "k" 'describe-key)

(define-key help-map "l" 'view-lossage)

(define-key help-map "m" 'describe-mode)

(define-key help-map "n" 'view-emacs-news)

(define-key help-map "p" 'finder-by-keyword)
(autoload 'finder-by-keyword "finder"
  "Find packages matching a given keyword." t)

(define-key help-map "s" 'describe-syntax)

(define-key help-map "t" 'help-with-tutorial)

(define-key help-map "w" 'where-is)

(define-key help-map "v" 'describe-variable)

(define-key help-map "q" 'help-quit)


(defun help-quit ()
  "Just exit from the Help command's command loop."
  (interactive)
  nil)

(defvar help-return-method nil
  "What to do to \"exit\" the help buffer.
This is a list
 (WINDOW . t)              delete the selected window, go to WINDOW.
 (WINDOW . quit-window)    do quit-window, then select WINDOW.
 (WINDOW BUF START POINT)  display BUF at START, POINT, then select WINDOW.")

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (let ((first-message
	      (cond ((special-display-p (buffer-name standard-output))
		     (setq help-return-method (cons (selected-window) t))
		     ;; If the help output buffer is a special display buffer,
		     ;; don't say anything about how to get rid of it.
		     ;; First of all, the user will do that with the window
		     ;; manager, not with Emacs.
		     ;; Secondly, the buffer has not been displayed yet,
		     ;; so we don't know whether its frame will be selected.
		     nil)
		    (display-buffer-reuse-frames
		     (setq help-return-method (cons (selected-window)
						    'quit-window))
		     nil)
		    ((not (one-window-p t))
		     (setq help-return-method
			   (cons (selected-window) 'quit-window))
		     "Type \\[switch-to-buffer-other-window] RET to restore the other window.")
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
		   (if (special-display-p (buffer-name standard-output))
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

(defalias 'help 'help-for-help)
(make-help-screen help-for-help
  "a b c C e f F i I k C-k l L m p s t v w C-c C-d C-f C-n C-p C-t C-w or ? :"
  "You have typed %THIS-KEY%, the help character.  Type a Help option:
\(Use SPC or DEL to scroll through this text.  Type \\<help-map>\\[help-quit] to exit the Help command.)

a  command-apropos.  Give a substring, and see a list of commands
	(functions interactively callable) that contain
	that substring.  See also the  apropos  command.
b  describe-bindings.  Display table of all key bindings.
c  describe-key-briefly.  Type a command key sequence;
	it prints the function name that sequence runs.
C  describe-coding-system.  This describes either a specific coding system
        (if you type its name) or the coding systems currently in use
	(if you type just RET).
e  view-echo-area-messages.  Show the `*Messages*' buffer.
f  describe-function.  Type a function name and get documentation of it.
F  Info-goto-emacs-command-node.  Type a function name;
	it takes you to the Info node for that command.
h  Display the HELLO file which illustrates various scripts.
i  info. The  info  documentation reader.
I  describe-input-method.  Describe a specific input method (if you type
	its name) or the current input method (if you type just RET).
k  describe-key.  Type a command key sequence;
	it displays the full documentation.
K Info-goto-emacs-key-command-node.  Type a command key sequence;
	it takes you to the Info node for the command bound to that key.
l  view-lossage.  Show last 100 characters you typed.
L  describe-language-environment.  This describes either a
	specific language environment (if you type its name)
	or the current language environment (if you type just RET).
m  describe-mode.  Print documentation of current minor modes,
	and the current major mode, including their special commands.
p  finder-by-keyword. Find packages matching a given topic keyword.
s  describe-syntax.  Display contents of syntax table, plus explanations.
S  info-lookup-symbol.  Display the definition of a specific symbol
        as found in the manual for the language this buffer is written in.
t  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
v  describe-variable.  Type name of a variable;
	it displays the variable's documentation and value.
w  where-is.  Type command name; it prints which keystrokes
	invoke that command.

C-c Display Emacs copying permission (GNU General Public License).
C-d Display Emacs ordering information.
C-e Display info about Emacs problems.
C-f Display the Emacs FAQ.
C-m Display how to order printed Emacs manuals.
C-n Display news of recent Emacs changes.
C-p Display information about the GNU project.
C-t Display the Emacs TODO list.
C-w Display information on absence of warranty for GNU Emacs."
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
	  (error nil)))))


;;; `User' help functions

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (view-file (expand-file-name "DISTRIB" data-directory)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (view-file (expand-file-name "COPYING" data-directory))
  (goto-char (point-min)))

(defun describe-project ()
  "Display info on the GNU project."
  (interactive)
  (view-file (expand-file-name "THE-GNU-PROJECT" data-directory))
  (goto-char (point-min)))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let* ((key (this-command-keys)))
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

(defun view-emacs-news (&optional arg)
  "Display info on recent changes to Emacs.
With numeric argument, display information on correspondingly older changes."
  (interactive "P")
  (let* ((arg (if arg (prefix-numeric-value arg) 0))
	 (file (cond ((eq arg 0) "NEWS")
		     ((eq arg 1) "ONEWS")
		     (t
		      (nth (- arg 2)
			   (nreverse (directory-files data-directory
						      nil "^ONEWS\\.[0-9]+$"
						      nil)))))))
    (if file
	(view-file (expand-file-name file data-directory))
      (error "No such old news"))))

(defun view-todo (&optional arg)
  "Display the Emacs TODO list."
  (interactive "P")
  (view-file (expand-file-name "TODO" data-directory)))

(defun view-echo-area-messages ()
  "View the log of recent echo-area messages: the `*Messages*' buffer.
The number of messages retained in that buffer
is specified by the variable `message-log-max'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

(defun view-order-manuals ()
  "Display the Emacs ORDERS file."
  (interactive)
  (view-file (expand-file-name "ORDERS" data-directory))
  (goto-address))

(defun view-emacs-FAQ ()
  "Display the Emacs Frequently Asked Questions (FAQ) file."
  (interactive)
  ;; (find-file-read-only (expand-file-name "FAQ" data-directory))
  (info "(efaq)"))

(defun view-emacs-problems ()
  "Display info on known problems with Emacs and possible workarounds."
  (interactive)
  (view-file (expand-file-name "PROBLEMS" data-directory)))

(defun view-lossage ()
  "Display last 100 input keystrokes.

To record all your input on a file, use `open-dribble-file'."
  (interactive)
  (help-setup-xref (list #'view-lossage) (interactive-p))
  (with-output-to-temp-buffer (help-buffer)
    (princ (mapconcat (lambda (key)
			(if (or (integerp key) (symbolp key) (listp key))
			    (single-key-description key)
			  (prin1-to-string key nil)))
		      (recent-keys)
		      " "))
    (with-current-buffer standard-output
      (goto-char (point-min))
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n")))
    (print-help-return-message)))


;; Key bindings

(defun describe-bindings (&optional prefix buffer)
  "Show a list of all defined keys, and their definitions.
We put that list in a buffer, and display the buffer.

The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument BUFFER specifies which buffer's bindings
to display (default, the current buffer)."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (help-setup-xref (list #'describe-bindings prefix buffer) (interactive-p))
  (with-current-buffer buffer
    (describe-bindings-internal nil prefix)))

;; This function used to be in keymap.c.
(defun describe-bindings-internal (&optional menus prefix)
  "Show a list of all defined keys, and their definitions.
We put that list in a buffer, and display the buffer.

The optional argument MENUS, if non-nil, says to mention menu bindings.
\(Ordinarily these are omitted from the output.)
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix."
  (interactive)
  (let ((buf (current-buffer)))
    (with-output-to-temp-buffer "*Help*"
      (with-current-buffer standard-output
	(describe-buffer-bindings buf prefix menus)))))

(defun where-is (definition &optional insert)
  "Print message listing key sequences that invoke the command DEFINITION.
Argument is a command definition, usually a symbol with a function definition.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Where is command (default %s): " fn)
				  "Where is command: ")
				obarray 'commandp t))
     (list (if (equal val "")
	       fn (intern val))
	   current-prefix-arg)))
  (let* ((remapped (remap-command definition))
	 (keys (where-is-internal definition overriding-local-map nil nil remapped))
	 (keys1 (mapconcat 'key-description keys ", "))
	 (standard-output (if insert (current-buffer) t)))
    (if insert
	(if (> (length keys1) 0)
	    (if remapped
		(princ (format "%s (%s) (remapped from %s)" keys1 remapped definition))
	      (princ (format "%s (%s)" keys1 definition)))
	  (princ (format "M-x %s RET" definition)))
      (if (> (length keys1) 0)
	  (if remapped
	      (princ (format "%s is remapped to %s which is on %s" definition remapped keys1))
	    (princ (format "%s is on %s" definition keys1)))
	(princ (format "%s is not on any key" definition)))))
  nil)

(defun string-key-binding (key)
  "Value is the binding of KEY in a string.
If KEY is an event on a string, and that string has a `local-map'
or `keymap' property, return the binding of KEY in the string's keymap."
  (let* ((defn nil)
	 (start (when (vectorp key)
		  (if (memq (aref key 0)
			    '(mode-line header-line left-margin right-margin))
		      (event-start (aref key 1))
		    (and (consp (aref key 0))
			 (event-start (aref key 0))))))
	 (string-info (and (consp start) (nth 4 start))))
    (when string-info
      (let* ((string (car string-info))
	     (pos (cdr string-info))
	     (local-map (and (>= pos 0)
			     (< pos (length string))
			     (or (get-text-property pos 'local-map string)
				 (get-text-property pos 'keymap string)))))
	(setq defn (and local-map (lookup-key local-map key)))))
    defn))

(defun help-key-description (key untranslated)
  (let ((string (key-description key)))
    (if (or (not untranslated) (eq (aref untranslated 0) ?\e))
	string
      (let ((otherstring (key-description untranslated)))
	(if (equal string otherstring)
	    string
	  (format "%s (translated from %s)" string otherstring))))))
	  
(defun describe-key-briefly (key &optional insert untranslated)
  "Print the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer.
If non-nil UNTRANSLATED is a vector of the untranslated events.
It can also be a number in which case the untranslated events from
the last key hit are used."
  (interactive "kDescribe key briefly: \nP\np")
  (if (numberp untranslated)
      (setq untranslated (this-single-command-raw-keys)))
  (save-excursion
    (let ((modifiers (event-modifiers (aref key 0)))
	  (standard-output (if insert (current-buffer) t))
	  window position)
      ;; For a mouse button event, go to the button it applies to
      ;; to get the right key bindings.  And go to the right place
      ;; in case the keymap depends on where you clicked.
      (if (or (memq 'click modifiers) (memq 'down modifiers)
	      (memq 'drag modifiers))
	  (setq window (posn-window (event-start (aref key 0)))
		position (posn-point (event-start (aref key 0)))))
      (if (windowp window)
	  (progn
	    (set-buffer (window-buffer window))
	    (goto-char position)))
      ;; Ok, now look up the key and name the command.
      (let ((defn (or (string-key-binding key)
		      (key-binding key)))
	    (key-desc (help-key-description key untranslated)))
	(if (or (null defn) (integerp defn) (equal defn 'undefined))
	    (princ (format "%s is undefined" key-desc))
	  (princ (format (if (windowp window)
			     "%s at that spot runs the command %s"
			   "%s runs the command %s")
			 key-desc
			 (if (symbolp defn) defn (prin1-to-string defn)))))))))


(defun describe-key (key &optional untranslated)
  "Display documentation of the function invoked by KEY.
KEY should be a key sequence--when calling from a program,
pass a string or a vector.
If non-nil UNTRANSLATED is a vector of the untranslated events.
It can also be a number in which case the untranslated events from
the last key hit are used."
  (interactive "kDescribe key: \np")
  (if (numberp untranslated)
      (setq untranslated (this-single-command-raw-keys)))
  (save-excursion
    (let ((modifiers (event-modifiers (aref key 0)))
	  window position)
      ;; For a mouse button event, go to the button it applies to
      ;; to get the right key bindings.  And go to the right place
      ;; in case the keymap depends on where you clicked.
      (if (or (memq 'click modifiers) (memq 'down modifiers)
	      (memq 'drag modifiers))
	  (setq window (posn-window (event-start (aref key 0)))
		position (posn-point (event-start (aref key 0)))))
      (when (windowp window)
	    (set-buffer (window-buffer window))
	(goto-char position))
      (let ((defn (or (string-key-binding key) (key-binding key))))
	(if (or (null defn) (integerp defn) (equal defn 'undefined))
	    (message "%s is undefined" (help-key-description key untranslated))
	  (help-setup-xref (list #'describe-function defn) (interactive-p))
	  (with-output-to-temp-buffer (help-buffer)
	    (princ (help-key-description key untranslated))
	    (if (windowp window)
		(princ " at that spot"))
	    (princ " runs the command ")
	    (prin1 defn)
	    (princ "\n   which is ")
	    (describe-function-1 defn)
	    (print-help-return-message)))))))


(defun describe-mode (&optional buffer)
  "Display documentation of current major mode and minor modes.
The major mode description comes first, followed by the minor modes,
each on a separate page.
For this to work correctly for a minor mode, the mode's indicator variable
\(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (help-setup-xref (list #'describe-mode (or buffer (current-buffer)))
		   (interactive-p))
  ;; For the sake of help-do-xref and help-xref-go-back,
  ;; don't switch buffers before calling `help-buffer'.
  (with-output-to-temp-buffer (help-buffer)
    (save-excursion
      (when buffer (set-buffer buffer))
      (when minor-mode-alist
	(princ "The major mode is described first.
For minor modes, see following pages.\n\n"))
      (princ mode-name)
      (princ " mode:\n")
      (princ (documentation major-mode))
      (let ((minor-modes minor-mode-alist))
	(while minor-modes
	  (let* ((minor-mode (car (car minor-modes)))
		 (indicator (car (cdr (car minor-modes)))))
	    ;; Document a minor mode if it is listed in minor-mode-alist,
	    ;; bound locally in this buffer, non-nil, and has a function
	    ;; definition.
	    (if (and (boundp minor-mode)
		     (symbol-value minor-mode)
		     (fboundp minor-mode))
		(let ((pretty-minor-mode minor-mode))
		  (if (string-match "\\(-minor\\)?-mode\\'"
				    (symbol-name minor-mode))
		      (setq pretty-minor-mode
			    (capitalize
			     (substring (symbol-name minor-mode)
					0 (match-beginning 0)))))
		  (while (and indicator (symbolp indicator)
			      (boundp indicator)
			      (not (eq indicator (symbol-value indicator))))
		    (setq indicator (symbol-value indicator)))
		  (princ "\n\f\n")
		  (princ (format "%s minor mode (%s):\n"
				 pretty-minor-mode
				 (if indicator
				     (format "indicator%s" indicator)
				   "no indicator")))
		  (princ (documentation minor-mode)))))
	  (setq minor-modes (cdr minor-modes))))
      (print-help-return-message))))


;;; Automatic resizing of temporary buffers.

(defcustom temp-buffer-max-height (lambda (buffer) (/ (- (frame-height) 2) 2))
  "*Maximum height of a window displaying a temporary buffer.
This is the maximum height (in text lines) which `resize-temp-buffer-window'
will give to a window displaying a temporary buffer.
It can also be a function which will be called with the object corresponding
to the buffer to be displayed as argument and should return an integer
positive number."
  :type '(choice integer function)
  :group 'help
  :version "20.4")

(define-minor-mode temp-buffer-resize-mode
  "Toggle the mode which makes windows smaller for temporary buffers.
With prefix argument ARG, turn the resizing of windows displaying temporary
buffers on if ARG is positive or off otherwise.
This makes the window the right height for its contents, but never
more than `temp-buffer-max-height' nor less than `window-min-height'.
This applies to `help', `apropos' and `completion' buffers, and some others."
  :global t :group 'help
  (if temp-buffer-resize-mode
      ;; `help-make-xrefs' may add a `back' button and thus increase the
      ;; text size, so `resize-temp-buffer-window' must be run *after* it.
      (add-hook 'temp-buffer-show-hook 'resize-temp-buffer-window 'append)
    (remove-hook 'temp-buffer-show-hook 'resize-temp-buffer-window)))

(defun resize-temp-buffer-window ()
  "Resize the current window to fit its contents.
Will not make it higher than `temp-buffer-max-height' nor smaller than
`window-min-height'.  Do nothing if it is the only window on its frame, if it
is not as wide as the frame or if some of the window's contents are scrolled
out of view."
  (unless (or (one-window-p 'nomini)
              (not (pos-visible-in-window-p (point-min)))
              (/=  (frame-width) (window-width)))
    (fit-window-to-buffer
     (selected-window)
     (if (functionp temp-buffer-max-height)
	 (funcall temp-buffer-max-height (current-buffer))
       temp-buffer-max-height))))

;; Provide this for the sake of define-minor-mode which generates
;; defcustoms which require 'help'.
(provide 'help)

;;; help.el ends here
