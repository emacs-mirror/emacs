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

;; Provides a nice interface to evaluating Emacs-Lisp expressions.
;; Input is handled by the comint package, and output is passed
;; through the pretty-printer.

;; To install: copy this file to a directory in your load-path, and
;; add the line
;;
;;   (autoload 'ielm "ielm" "Start an inferior emacs-lisp session" t)
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
  "*If non-nil, beep on error")

(defvar ielm-prompt "ELISP> ")

(defvar ielm-dynamic-return t
  "*If non-nil, RET either evaluates input or inserts a newline,
depending on context")

(defvar ielm-mode-hook nil
  "*Hooks to be run when the inferior-emacs-lisp-mode is started")

;;; System variables

(defvar ielm-working-buffer nil
  "Buffer, if any, to use in ielm.  Usually buffer-local")

(defvar ielm-header 
  (concat
   "*** Welcome to IELM mode version "
   (substring "$Revision: 1.15 $" 11 -2)
   " ***  Type (describe-mode) for help.\n"
   "IELM has ABSOLUTELY NO WARRANTY; type (describe-no-warranty) for details\n"))

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
  (define-key ielm-map "\C-j" 'ielm-send-input))

;;; Completion stuff

(defun ielm-tab nil
  "Possibly indent the current line as lisp code"
  (interactive)
  (if (or (eq (preceding-char) ?\n)
	  (eq (char-syntax (preceding-char)) ? ))
      (progn
	(ielm-indent-line)
	t)))
  
(defun ielm-complete-symbol nil
  "Just like lisp-complete-symbol" 
  ;; except that it returns non-nil if completion has occurred
  (let* ((btick (buffer-modified-tick))
	 (cbuffer (get-buffer " *Completions*"))
	 (ctick (and cbuffer (buffer-modified-tick cbuffer))))
    (lisp-complete-symbol)
     ;; completion has occurred if:
    (or 
     ;; the buffer has been modified
     (not (= btick (buffer-modified-tick))) 
     ;; a completions buffer has been modifed or created
     (if cbuffer
	 (not (= ctick (buffer-modified-tick cbuffer)))
       (get-buffer " *Completions*")))))

(defun ielm-complete-filename nil
  ;; Completes filenames if in a string
  (if (nth 3 (parse-partial-sexp comint-last-input-start (point)))
      (comint-dynamic-complete-filename)))
     
(defun ielm-indent-line nil
  "Indent the current line as lisp code if it is not a prompt line"
  (if (save-excursion
	(beginning-of-line)
	(looking-at comint-prompt-regexp)) nil
    (lisp-indent-line)))

;;; Other bindings

(defun ielm-return nil
  "Evaluate the sexp at the prompt if it is complete, otherwise newline
and indent. If ielm-dynamic-return is nil, just insert a newline."
  (interactive)
  (if ielm-dynamic-return 
      (let ((state       
	     (save-excursion
	       (end-of-line)
	       (parse-partial-sexp (ielm-pm)
				   (point)))))
	(if (and (< (car state) 1) (not (nth 3 state)))
	    (ielm-send-input)
	  (newline-and-indent)))
    (newline)))

(defun ielm-input-sender (proc input)
  (setq ielm-input input))

(defun ielm-send-input nil
  "Evaluate the Emacs Lisp expression after the prompt"
  (interactive)
  (let ((buf (current-buffer))
	ielm-input)			; set by ielm-input-sender
    (comint-send-input)			; update history, markers etc.
    (ielm-eval-input ielm-input)))

;;; Utility functions

(defun ielm-is-whitespace (string)
  "Return non-nil if STRING is all whitespace"
  (or (string= string "") (string-match "\\`[ \t\n]+\\'" string)))

(defun ielm-format-errors (errlist)
  (let ((result ""))
    (while errlist
      (setq result (concat result (prin1-to-string (car errlist)) ", "))
      (setq errlist (cdr errlist)))
    (substring result 0 -2)))


(defun ielm-format-error (err)
  "Return a string form of the error ERR"
  (format "%s%s"
	  (or (get (car err) 'error-message) "Peculiar error")
	  (if (cdr err)
	      (format ": %s" (ielm-format-errors (cdr err)))
	    "")))

;;; Evaluation

(defun ielm-eval-input (string)
  "Evaluate the lisp expression STRING, and pretty-print the result"
  ;; This is the function that actually `sends' the input to the
  ;; `inferior lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  (let (form				; form to evaluate
	pos				; End posn of parse in string
	result				; Result, or error message
	error-type			; string, nil if no error
	(output	"")			; result to display
	(wbuf ielm-working-buffer)	; current buffer after evaluation
	(pmark (ielm-pm)))
    (if (not (ielm-is-whitespace string))
	(progn
	  (condition-case err
	      (let (rout)
		(setq rout (read-from-string string))
		(setq form (car rout))
		(setq pos (cdr rout)))
	    (error (setq result (ielm-format-error err))
		   (setq error-type "Read error")))
	  (if error-type nil
	    (if (ielm-is-whitespace (substring string pos))
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
			      (::: :::save))
			  (save-excursion
			    (setq result (eval form))
			    (setq wbuf (current-buffer))))
		      (error (setq result (ielm-format-error err))
			     (setq error-type "Eval error"))
		      (quit (setq result "Quit during evaluation")
			    (setq error-type "Eval error")))))
	      (setq error-type "IELM error")
	      (setq result "More than one sexp in input")))

	  ;; If the eval changed the current buffer, mention it here
	  (if (eq wbuf ielm-working-buffer) nil
	    (message "current buffer is now: %s" wbuf)
	    (setq ielm-working-buffer wbuf))

	  (goto-char pmark)
	  (if (not error-type)
	      (condition-case err
		  ;; Self-referential objects cause loops in the printer, so
		  ;; trap quits here. May as well do errors, too
		  (setq output (concat output (pp-to-string result)))
		(error (setq error-type "IELM Error")
		       (setq result  "Error during pretty-printing (bug in pp)"))
		(quit  (setq error-type "IELM Error")
		       (setq result "Quit during pretty-printing"))))
	  (if error-type
	      (progn
		(if ielm-noisy (ding))
		(setq output (concat output "*** " error-type " ***  "))
		(setq output (concat output result)))
	    ;; There was no error, so shift the ::: values
	    (setq ::: ::)
	    (setq :: :)
	    (setq : result))
	  (setq output (concat output "\n"))))
    (setq output (concat output ielm-prompt))
    (comint-output-filter (ielm-process) output)))

;;; Process and marker utilities

(defun ielm-process nil
  "Return the current buffer's process"
  (get-buffer-process (current-buffer)))

(defun ielm-pm nil
  "Return the process mark of the current buffer"
  (process-mark (get-buffer-process (current-buffer))))

(defun ielm-set-pm (pos)
  "Set the process mark in the current buffer to POS"
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Major mode

(defun inferior-emacs-lisp-mode nil
  "Major mode for interactively evaluating Emacs-Lisp expressions
Uses the interface provided by `comint-mode' (q.v.)

\\[ielm-send-input] evaluates the sexp following the prompt. There must be at most
one top-level sexp per prompt.
\\[ielm-return] inserts a newline and indents. However, if the variable 
ielm-dynamic-return is non-nil (the default) then it will also evaluate
a complete expression.
\\[comint-dynamic-complete] completes lisp symbols (or filenames, within strings), 
or indents the line if there is nothing to complete.

During evaluations, the values of the variables `:', `::', and `:::'
are the results of the previous, second previous and third previous
evaluations respectively.

The current buffer may be changed, and its value is preserved between
successive evaluations.  In this way, expressions may be evaluated in
a different buffer than the *ielm* buffer.

Expressions evaluated by IELM are not subject to debug-on-quit or
debug-on-error.

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

  (setq major-mode 'inferior-emacs-lisp-mode)
  (setq mode-name "IELM")
  (use-local-map ielm-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (make-local-variable 'ielm-working-buffer)
  (setq ielm-working-buffer (current-buffer))
  (setq indent-line-function 'ielm-indent-line)

  ;;; Value holders
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

;;; User command

(defun ielm nil
  "Switch to or create the buffer *ielm* for evaluating emacs-lisp expressions"
  (interactive)
  (if (comint-check-proc "*ielm*") nil
    (progn
      (set-buffer (get-buffer-create "*ielm*"))
      (inferior-emacs-lisp-mode)))
  (switch-to-buffer "*ielm*"))

;; ielm.el ends here
