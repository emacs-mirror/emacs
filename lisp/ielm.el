;;; ielm.el --- interaction mode for Emacs Lisp
;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: David Smith <maa036@lancaster.ac.uk>
;; Created: 25 Feb 1994
;; Keywords: lisp

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides a nice interface to evaluating Emacs Lisp expressions.
;; Input is handled by the comint package, and output is passed
;; through the pretty-printer.

;; To install: copy this file to a directory in your load-path, and
;; add the following line to your .emacs file:
;;
;;   (autoload 'ielm "ielm" "Start an inferior Emacs Lisp session" t)
;;
;; For completion to work, the comint.el from FSF Emacs 19.23 is
;; required.  If you do not have it, or if you are running Lemacs,
;; also add the following code to your .emacs:
;;
;;    (setq ielm-mode-hook
;; 	    '(lambda nil
;; 		 (define-key ielm-map "\t"
;; 		    '(lambda nil (interactive) (or (ielm-tab) 
;;                                                 (lisp-complete-symbol))))))

;; To start: M-x ielm.  Type C-h m in the *ielm* buffer for more info.

;; The latest version is available by WWW from 
;;      http://mathssun5.lancs.ac.uk:2080/~maa036/elisp/dir.html
;; or by anonymous FTP from
;;      /anonymous@wingra.stat.wisc.edu:pub/src/emacs-lisp/ielm.el.gz
;; or from the author: David M. Smith <maa036@lancaster.ac.uk>

;;; Code:

(require 'comint)
(require 'pp)

;;; User variables

(defvar ielm-noisy t
  "*If non-nil, IELM will beep on error.")

(defvar ielm-prompt "ELISP> "
  "Prompt used in IELM.")

(defvar ielm-dynamic-return t
  "*Controls whether \\<ielm-map>\\[ielm-return] has intelligent behaviour in IELM.
If non-nil, \\[ielm-return] evaluates input for complete sexps, or inserts a newline
and indents for incomplete sexps.  If nil, always inserts newlines.")

(defvar ielm-dynamic-multiline-inputs t
  "*Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs.")

(defvar ielm-mode-hook nil
  "*Hooks to be run when IELM (`inferior-emacs-lisp-mode') is started.")

;;; System variables

(defvar ielm-working-buffer nil
  "Buffer in which IELM sexps will be evaluated.
This variable is buffer-local.")

(defvar ielm-header 
  (concat
   "*** Welcome to IELM version "
   (substring "$Revision: 1.3 $" 11 -2)
   " ***  Type (describe-mode) for help.\n"
   "IELM has ABSOLUTELY NO WARRANTY; type (describe-no-warranty) for details.\n")
  "Message to display when IELM is started.")

(defvar ielm-map nil)
(if ielm-map nil
  (if (string-match "Lucid" emacs-version)
      ;; Lemacs
      (progn
	(setq ielm-map (make-sparse-keymap))
	(set-keymap-parent ielm-map comint-mode-map))
    ;; FSF
    (setq ielm-map (cons 'keymap comint-mode-map)))
  (define-key ielm-map "\t" 'comint-dynamic-complete)
  (define-key ielm-map "\C-m" 'ielm-return)
  (define-key ielm-map "\C-j" 'ielm-send-input)
  (define-key ielm-map "\e\C-x" 'eval-defun)         ; for consistency with
  (define-key ielm-map "\e\t" 'lisp-complete-symbol) ; lisp-interaction-mode
  ;; These bindings are from shared-lisp-mode-map -- can you inherit
  ;; from more than one keymap??
  (define-key ielm-map "\e\C-q" 'indent-sexp)      
  (define-key ielm-map "\eq" 'lisp-fill-paragraph) 
  (define-key ielm-map "\177" 'backward-delete-char-untabify)
  ;; Some convenience bindings for setting the working buffer
  (define-key ielm-map "\C-c\C-b" 'ielm-change-working-buffer)
  (define-key ielm-map "\C-c\C-f" 'ielm-display-working-buffer)
  (define-key ielm-map "\C-c\C-v" 'ielm-print-working-buffer))

;;; Completion stuff

(defun ielm-tab nil
  "Possibly indent the current line as lisp code."
  (interactive)
  (if (or (eq (preceding-char) ?\n)
	  (eq (char-syntax (preceding-char)) ? ))
      (progn
	(ielm-indent-line)
	t)))
  
(defun ielm-complete-symbol nil
  "Complete the lisp symbol before point."
  ;; A wrapper for lisp-complete symbol that returns non-nil if
  ;; completion has occurred
  (let* ((btick (buffer-modified-tick))
	 (cbuffer (get-buffer "*Completions*"))
	 (ctick (and cbuffer (buffer-modified-tick cbuffer))))
    (lisp-complete-symbol)
     ;; completion has occurred if:
    (or 
     ;; the buffer has been modified
     (not (= btick (buffer-modified-tick))) 
     ;; a completions buffer has been modifed or created
     (if cbuffer
	 (not (= ctick (buffer-modified-tick cbuffer)))
       (get-buffer "*Completions*")))))

(defun ielm-complete-filename nil
  "Dynamically complete filename before point, if in a string."
  (if (nth 3 (parse-partial-sexp comint-last-input-start (point)))
      (comint-dynamic-complete-filename)))
     
(defun ielm-indent-line nil
  "Indent the current line as Lisp code if it is not a prompt line."
  (if (save-excursion
	(beginning-of-line)
	(looking-at comint-prompt-regexp)) nil
    (lisp-indent-line)))

;;; Working buffer manipulation

(defun ielm-print-working-buffer nil
  "Print the current IELM working buffer's name in the echo area."
  (interactive)
  (message "The current working buffer is: %s" (buffer-name ielm-working-buffer)))

(defun ielm-display-working-buffer nil
  "Display the current IELM working buffer.
Don't forget that selecting that buffer will change its value of `point'
to its value of `window-point'!"
  (interactive)
  (display-buffer ielm-working-buffer)
  (ielm-print-working-buffer))

(defun ielm-change-working-buffer (buf)
  "Change the current IELM working buffer to BUF.
This is the buffer in which all sexps entered at the IELM prompt are
evaluated.  You can achieve the same effect with a call to
`set-buffer' at the IELM prompt."
  (interactive "bSet working buffer to: ")
  (setq ielm-working-buffer (or (get-buffer buf) (error "No such buffer")))
  (ielm-print-working-buffer))

;;; Other bindings

(defun ielm-return nil
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ielm-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if ielm-dynamic-return 
      (let ((state       
	     (save-excursion
	       (end-of-line)
	       (parse-partial-sexp (ielm-pm)
				   (point)))))
	(if (and (< (car state) 1) (not (nth 3 state)))
	    (ielm-send-input)
	  (if (and ielm-dynamic-multiline-inputs
		   (save-excursion
		     (beginning-of-line)
		     (looking-at comint-prompt-regexp)))
	      (save-excursion
		(goto-char (ielm-pm))
		(newline 1)))
	  (newline-and-indent)))
    (newline)))

(defun ielm-input-sender (proc input)
  ;; Just sets the variable ielm-input, which is in the scope of 
  ;; `ielm-send-input's call.
  (setq ielm-input input))

(defun ielm-send-input nil
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let ((buf (current-buffer))
	ielm-input)			; set by ielm-input-sender
    (comint-send-input)			; update history, markers etc.
    (ielm-eval-input ielm-input)))

;;; Utility functions

(defun ielm-is-whitespace (string)
  "Return non-nil if STRING is all whitespace."
  (or (string= string "") (string-match "\\`[ \t\n]+\\'" string)))

(defun ielm-format-errors (errlist)
  (let ((result ""))
    (while errlist
      (setq result (concat result (prin1-to-string (car errlist)) ", "))
      (setq errlist (cdr errlist)))
    (substring result 0 -2)))


(defun ielm-format-error (err)
  ;; Return a string form of the error ERR.
  (format "%s%s"
	  (or (get (car err) 'error-message) "Peculiar error")
	  (if (cdr err)
	      (format ": %s" (ielm-format-errors (cdr err)))
	    "")))

;;; Evaluation

(defun ielm-eval-input (ielm-string)
  "Evaluate the Lisp expression IELM-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  ;;
  ;; NOTE: all temporary variables in this function will be in scope
  ;; during the eval, and so need to have non-clashing names.
  (let (ielm-form			; form to evaluate
	ielm-pos			; End posn of parse in string
	ielm-result			; Result, or error message
	ielm-error-type			; string, nil if no error
	(ielm-output	"")		; result to display
	(ielm-wbuf ielm-working-buffer)	; current buffer after evaluation
	(ielm-pmark (ielm-pm)))
    (if (not (ielm-is-whitespace ielm-string))
	(progn
	  (condition-case err
	      (let (rout)
		(setq rout (read-from-string ielm-string))
		(setq ielm-form (car rout))
		(setq ielm-pos (cdr rout)))
	    (error (setq ielm-result (ielm-format-error err))
		   (setq ielm-error-type "Read error")))
	  (if ielm-error-type nil
	    ;; Make sure working buffer has not been killed
	    (if (not (buffer-name ielm-working-buffer))
		(setq ielm-result "Working buffer has been killed"
		      ielm-error-type "IELM Error"
		      ielm-wbuf (current-buffer))
	      (if (ielm-is-whitespace (substring ielm-string ielm-pos))
		  ;; need this awful let convolution to work around
		  ;; an Emacs bug involving local vbls and let binding
		  (let ((:save :)
			(::save ::)
			(:::save :::))
		    (save-excursion
		      (set-buffer ielm-working-buffer)
		      (condition-case err
			  (let ((: :save)
				(:: ::save)
				(::: :::save)
				(ielm-obuf (current-buffer)))
			    (setq ielm-result (eval ielm-form))
			    (setq ielm-wbuf (current-buffer))
			    ;; The eval may have changed current-buffer;
			    ;; need to set it back here to avoid a bug
			    ;; in let.  Don't want to use save-excursion
			    ;; because we want to allow changes in point.
			    (set-buffer ielm-obuf))
			(error (setq ielm-result (ielm-format-error err))
			       (setq ielm-error-type "Eval error"))
			(quit (setq ielm-result "Quit during evaluation")
			      (setq ielm-error-type "Eval error")))))
		(setq ielm-error-type "IELM error")
		(setq ielm-result "More than one sexp in input"))))

	  ;; If the eval changed the current buffer, mention it here
	  (if (eq ielm-wbuf ielm-working-buffer) nil
	    (message "current buffer is now: %s" ielm-wbuf)
	    (setq ielm-working-buffer ielm-wbuf))

	  (goto-char ielm-pmark)
	  (if (not ielm-error-type)
	      (condition-case err
		  ;; Self-referential objects cause loops in the printer, so
		  ;; trap quits here. May as well do errors, too
		  (setq ielm-output (concat ielm-output (pp-to-string ielm-result)))
		(error (setq ielm-error-type "IELM Error")
		       (setq ielm-result  "Error during pretty-printing (bug in pp)"))
		(quit  (setq ielm-error-type "IELM Error")
		       (setq ielm-result "Quit during pretty-printing"))))
	  (if ielm-error-type
	      (progn
		(if ielm-noisy (ding))
		(setq ielm-output (concat ielm-output "*** " ielm-error-type " ***  "))
		(setq ielm-output (concat ielm-output ielm-result)))
	    ;; There was no error, so shift the ::: values
	    (setq ::: ::)
	    (setq :: :)
	    (setq : ielm-result))
	  (setq ielm-output (concat ielm-output "\n"))))
    (setq ielm-output (concat ielm-output ielm-prompt))
    (comint-output-filter (ielm-process) ielm-output)))

;;; Process and marker utilities

(defun ielm-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun ielm-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun ielm-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Major mode

(defun inferior-emacs-lisp-mode nil
  "Major mode for interactively evaluating Emacs Lisp expressions.
Uses the interface provided by `comint-mode' (which see).

* \\<ielm-map>\\[ielm-send-input] evaluates the sexp following the prompt. There must be at most
  one top-level sexp per prompt.

* \\[ielm-return] inserts a newline and indents, or evaluates a
  complete expression (but see variable `ielm-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `ielm-dynamic-multiline-inputs').

* \\[comint-dynamic-complete] completes Lisp symbols (or filenames, within strings), 
  or indents the line if there is nothing to complete.

During evaluations, the values of the variables `:', `::', and `:::'
are the results of the previous, second previous and third previous
evaluations respectively.

The current working buffer may be changed (with a call to
`set-buffer', or with \\[ielm-change-working-buffer]), and its value
is preserved between successive evaluations.  In this way, expressions
may be evaluated in a different buffer than the *ielm* buffer.
Display the name of the working buffer with \\[ielm-print-working-buffer],
or the buffer itself with \\[ielm-display-working-buffer].

Expressions evaluated by IELM are not subject to `debug-on-quit' or
`debug-on-error'.

The behaviour of IELM may be customised with the following variables:
* To stop beeping on error, set `ielm-noisy' to nil
* If you don't like the prompt, you can change it by setting `ielm-prompt'.
* Set `ielm-dynamic-return' to nil for bindings like `lisp-interaction-mode'
* Entry to this mode runs `comint-mode-hook' and `ielm-mode-hook'
 (in that order).

Customised bindings may be defined in `ielm-map', which currently contains:
\\{ielm-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^" (regexp-quote ielm-prompt)))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'ielm-input-sender)
  (setq comint-process-echoes nil)
  (setq comint-dynamic-complete-functions 
	'(ielm-tab comint-replace-by-expanded-history ielm-complete-filename ielm-complete-symbol))
  (setq comint-get-old-input 'ielm-get-old-input)

  (setq major-mode 'inferior-emacs-lisp-mode)
  (setq mode-name "IELM")
  (use-local-map ielm-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (make-local-variable 'ielm-working-buffer)
  (setq ielm-working-buffer (current-buffer))
  (setq indent-line-function 'ielm-indent-line)

  ;; Value holders
  (setq : nil)
  (make-local-variable ':)
  (setq :: nil)
  (make-local-variable '::)
  (setq ::: nil)
  (make-local-variable ':::)

  ;; A dummy process to keep comint happy. It will never get any input
  (if (comint-check-proc (current-buffer)) nil
    (start-process "ielm" (current-buffer) "cat")
    (process-kill-without-query (ielm-process))
    (goto-char (point-max))
    ;; Add a silly header
    (insert ielm-header)
    (ielm-set-pm (point-max))
    (comint-output-filter (ielm-process) ielm-prompt)
    (set-marker comint-last-input-start (ielm-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))
  (run-hooks 'ielm-mode-hook))

(defun ielm-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (if (looking-at comint-prompt-regexp) nil
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;; User command

;;;###autoload (add-hook 'same-window-buffer-names "*ielm*")

;;;###autoload
(defun ielm nil
  "Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*', or creates it if it does not exist."
  (interactive)
  (if (comint-check-proc "*ielm*")
      nil
    (save-excursion
      (set-buffer (get-buffer-create "*ielm*"))
      (inferior-emacs-lisp-mode)))
  (pop-to-buffer "*ielm*"))

;;; ielm.el ends here
