;; Basic lisp subroutines for Emacs
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defun one-window-p (&optional arg)
  "Returns non-nil if there is only one window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active."
  (eq (selected-window)
      (next-window (selected-window) (if arg 'arg))))

(defun walk-windows (proc &optional minibuf all-screens)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active.
Optional third arg ALL-SCREENS t means include all windows in all screens;
otherwise cycle within the selected screen."
  (let* ((walk-windows-start (selected-window))
	 (walk-windows-current walk-windows-start))
    (while (progn
	     (setq walk-windows-current
		   (next-window walk-windows-current minibuf all-screens))
	     (funcall proc walk-windows-current)
	     (not (eq walk-windows-current walk-windows-start))))))

(defun read-quoted-char (&optional prompt)
  "Like `read-char', except that if the first character read is an octal
digit, we read up to two more octal digits and return the character
represented by the octal number consisting of those digits.
Optional argument PROMPT specifies a string to use to prompt the user."
  (let ((count 0) (code 0) char)
    (while (< count 3)
      (let ((inhibit-quit (zerop count))
	    (help-form nil))
	(and prompt (message "%s-" prompt))
	(setq char (read-char))
	(if inhibit-quit (setq quit-flag nil)))
      (cond ((null char))
	    ((and (<= ?0 char) (<= char ?7))
	     (setq code (+ (* code 8) (- char ?0))
		   count (1+ count))
	     (and prompt (message (setq prompt
					(format "%s %c" prompt char)))))
	    ((> count 0)
	     (setq unread-command-char char count 259))
	    (t (setq code char count 259))))
    (logand 255 code)))

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'."
  (while t
    (signal 'error (list (apply 'format args)))))

(defun undefined ()
  (interactive)
  (ding))

;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (let ((i 0))
    (while (<= i 127)
      (if (eql (lookup-key global-map (char-to-string i)) 'self-insert-command)
	  (define-key map (char-to-string i) 'undefined))
      (setq i (1+ i))))
  (or nodigits
      (let (loop)
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (define-key map (char-to-string loop) 'digit-argument)
	  (setq loop (1+ loop))))))

;; now in fns.c
;(defun nth (n list)
;  "Returns the Nth element of LIST.
;N counts from zero.  If LIST is not that long, nil is returned."
;  (car (nthcdr n list)))
;
;(defun copy-alist (alist)
;  "Return a copy of ALIST.
;This is a new alist which represents the same mapping
;from objects to objects, but does not share the alist structure with ALIST.
;The objects mapped (cars and cdrs of elements of the alist)
;are shared, however."
;  (setq alist (copy-sequence alist))
;  (let ((tail alist))
;    (while tail
;      (if (consp (car tail))
;	  (setcar tail (cons (car (car tail)) (cdr (car tail)))))
;      (setq tail (cdr tail))))
;  alist)

;Moved to keymap.c
;(defun copy-keymap (keymap)
;  "Return a copy of KEYMAP"  
;  (while (not (keymapp keymap))
;    (setq keymap (signal 'wrong-type-argument (list 'keymapp keymap))))
;  (if (vectorp keymap)
;      (copy-sequence keymap)
;      (copy-alist keymap)))

(defun substitute-key-definition (olddef newdef keymap)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Prefix keymaps reached from KEYMAP are not checked recursively;
perhaps they ought to be."
  (if (arrayp keymap)
      (let ((len (length keymap))
	    (i 0))
	(while (< i len)
	  (if (eq (aref keymap i) olddef)
	      (aset keymap i newdef))
	  (setq i (1+ i))))
    (while keymap
      (if (eq (cdr-safe (car-safe keymap)) olddef)
	  (setcdr (car keymap) newdef))
      (setq keymap (cdr keymap)))))

;; Avoids useless byte-compilation.
;; In the future, would be better to fix byte compiler
;; not to really compile in cases like this,
;; and use defun here.
(fset 'ignore '(lambda (&rest ignore) 
		 "Do nothing.
Accept any number of arguments, but ignore them."
		 nil))


; old names
(fset 'make-syntax-table 'copy-syntax-table)
(fset 'dot 'point)
(fset 'dot-marker 'point-marker)
(fset 'dot-min 'point-min)
(fset 'dot-max 'point-max)
(fset 'window-dot 'window-point)
(fset 'set-window-dot 'set-window-point)
(fset 'read-input 'read-string)
(fset 'send-string 'process-send-string)
(fset 'send-region 'process-send-region)
(fset 'show-buffer 'set-window-buffer)
(fset 'buffer-flush-undo 'buffer-disable-undo)

; alternate names
(fset 'string= 'string-equal)
(fset 'string< 'string-lessp)
(fset 'move-marker 'set-marker)
(fset 'eql 'eq)
(fset 'not 'null)
(fset 'numberp 'integerp)
(fset 'rplaca 'setcar)
(fset 'rplacd 'setcdr)
(fset 'beep 'ding) ;preserve lingual purtity
(fset 'indent-to-column 'indent-to)
(fset 'backward-delete-char 'delete-backward-char)
(fset 'search-forward-regexp (symbol-function 're-search-forward))
(fset 'search-backward-regexp (symbol-function 're-search-backward))

(defvar global-map nil
  "Default global keymap mapping Emacs keyboard input into commands.
The value is a keymap which is usually (but not necessarily) Emacs's
global map.")

(defvar ctl-x-map nil
  "Default keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")

(defvar esc-map nil
  "Default keymap for ESC (meta) commands.
The normal global definition of the character ESC indirects to this keymap.")

(defvar mouse-map nil
  "Keymap for mouse commands from the X window system.")

(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (let ((value (symbol-value sym)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (mapcar 'funcall value)
	       (funcall value)))))
    (setq hooklist (cdr hooklist))))

;; Tell C code how to call this function.
(defconst run-hooks 'run-hooks
  "Variable by which C primitives find the function `run-hooks'.
Don't change it.")

(defun add-hook (hook function)
  "Add to the value of HOOK the function FUNCTION unless already present.
HOOK should be a symbol, and FUNCTION may be any valid function.
HOOK's value should be a list of functions, not a single function.
If HOOK is void, it is first set to nil."
  (or (boundp hook) (set hook nil))
  (or (if (consp function)
	  ;; Clever way to tell whether a given lambda-expression
	  ;; is equal to anything in the hook.
	  (let ((tail (assoc (cdr function) (symbol-value hook))))
	    (equal function tail))
	(memq function (symbol-value hook)))
      (set hook (cons function hook))))

(defun momentary-string-display (string pos &optional exit-char message) 
  "Momentarily display STRING in the buffer at POS.
Display remains until next character is typed.
If the char is EXIT-CHAR (optional third arg, default is SPC) it is swallowed;
otherwise it is then available as input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\ ))
  (let ((buffer-read-only nil)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char pos)
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    (insert-before-markers string)
	    (setq insert-end (point)))
	  (message (or message "Type %s to continue editing.")
		   (single-key-description exit-char))
	  (let ((char (read-char)))
	    (or (eq char exit-char)
		(setq unread-command-char char))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handle as usual in the shell."
  (if (eq system-type 'vax-vms)
      (apply 'start-process name buffer args)
    (start-process name buffer shell-file-name "-c"
		   (concat "exec " (mapconcat 'identity args " ")))))

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  (nconc (assoc file after-load-alist) (list form))
  form)

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))

(defmacro defun-inline (name args &rest body)
  "Create an \"inline defun\" (actually a macro).
Use just like `defun'."
  (nconc (list 'defmacro name '(&rest args))
	 (if (stringp (car body))
	     (prog1 (list (car body))
	       (setq body (or (cdr body) body))))
	 (list (list 'cons (list 'quote
				 (cons 'lambda (cons args body)))
		     'args))))

(defun user-original-login-name ()
  "Return user's login name from original login.
This tries to remain unaffected by `su', by looking in environment variables."
  (or (getenv "LOGNAME") (getenv "USER") (user-login-name)))

(defun force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL then force then force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(defun keyboard-translate (from to)
  "Translate character FROM to TO at a low level.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it."
  (or (boundp 'keyboard-translate-table)
      (let ((table (make-string 256))
	    (i 0))
	(while (< i 256)
	  (aset table i i)
	  (setq i (1+ i)))
	(setq keyboard-translate-table table)))
  (aset keyboard-translate-table from to))
