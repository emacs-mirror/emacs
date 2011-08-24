;;; esh-test.el --- Eshell test suite

;; Copyright (C) 1999-2011  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The purpose of this module is to verify that Eshell works as
;; expected.  To run it on your system, use the command
;; \\[eshell-test].

;;; Code:

(eval-when-compile
  (require 'cl)				; assert
  (require 'eshell)
  (require 'esh-util))
(require 'esh-mode)

(defgroup eshell-test nil
  "This module is meant to ensure that Eshell is working correctly."
  :tag "Eshell test suite"
  :group 'eshell)

;;; User Variables:

(defface eshell-test-ok
  '((((class color) (background light)) (:foreground "Green" :bold t))
    (((class color) (background dark)) (:foreground "Green" :bold t)))
  "The face used to highlight OK result strings."
  :group 'eshell-test)
(define-obsolete-face-alias 'eshell-test-ok-face 'eshell-test-ok "22.1")

(defface eshell-test-failed
  '((((class color) (background light)) (:foreground "OrangeRed" :bold t))
    (((class color) (background dark)) (:foreground "OrangeRed" :bold t))
    (t (:bold t)))
  "The face used to highlight FAILED result strings."
  :group 'eshell-test)
(define-obsolete-face-alias 'eshell-test-failed-face 'eshell-test-failed "22.1")

(defcustom eshell-show-usage-metrics nil
  "If non-nil, display different usage metrics for each Eshell command."
  :set (lambda (symbol value)
	 (if value
	     (add-hook 'eshell-mode-hook 'eshell-show-usage-metrics)
	   (remove-hook 'eshell-mode-hook 'eshell-show-usage-metrics))
	 (set symbol value))
  :type '(choice (const :tag "No metrics" nil)
		 (const :tag "Cons cells consumed" t)
		 (const :tag "Time elapsed" 0))
  :group 'eshell-test)

;;; Code:

(defvar test-buffer)

(defun eshell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell-match-result (regexp)
  "Insert a command at the end of the buffer."
  (goto-char eshell-last-input-end)
  (looking-at regexp))

(defun eshell-command-result-p (text regexp &optional func)
  "Insert a command at the end of the buffer."
  (eshell-insert-command text func)
  (eshell-match-result regexp))

(defvar eshell-test-failures nil)

(defun eshell-run-test (module funcsym label command)
  "Test whether FORM evaluates to a non-nil value."
  (when (let ((sym (intern-soft (concat "eshell-" (symbol-name module)))))
	  (or (memq sym (eshell-subgroups 'eshell))
	      (eshell-using-module sym)))
    (with-current-buffer test-buffer
      (insert-before-markers
       (format "%-70s " (substring label 0 (min 70 (length label)))))
      (insert-before-markers "  ....")
      (eshell-redisplay))
    (let ((truth (eval command)))
      (with-current-buffer test-buffer
	(delete-char -6)
	(insert-before-markers
	 "[" (let (str)
	       (if truth
		   (progn
		     (setq str "  OK  ")
		     (put-text-property 0 6 'face 'eshell-test-ok str))
		 (setq str "FAILED")
		 (setq eshell-test-failures (1+ eshell-test-failures))
		 (put-text-property 0 6 'face 'eshell-test-failed str))
	       str) "]")
	(add-text-properties (line-beginning-position) (point)
			     (list 'test-func funcsym))
	(eshell-redisplay)))))

(defun eshell-test-goto-func ()
  "Jump to the function that defines a particular test."
  (interactive)
  (let ((fsym (get-text-property (point) 'test-func)))
    (when fsym
      (let* ((def (symbol-function fsym))
	     (library (locate-library (symbol-file fsym 'defun)))
	     (name (substring (symbol-name fsym)
			      (length "eshell-test--")))
	     (inhibit-redisplay t))
	(find-file library)
	(goto-char (point-min))
	(re-search-forward (concat "^(eshell-deftest\\s-+\\w+\\s-+"
				   name))
	(beginning-of-line)))))

(defun eshell-run-one-test (&optional arg)
  "Jump to the function that defines a particular test."
  (interactive "P")
  (let ((fsym (get-text-property (point) 'test-func)))
    (when fsym
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (let ((test-buffer (current-buffer)))
	(set-buffer (let ((inhibit-redisplay t))
		      (save-window-excursion (eshell t))))
	(funcall fsym)
	(unless arg
	  (kill-buffer (current-buffer)))))))

;;;###autoload
(defun eshell-test (&optional arg)
  "Test Eshell to verify that it works as expected."
  (interactive "P")
  (let* ((begin (float-time))
	 (test-buffer (get-buffer-create "*eshell test*")))
    (set-buffer (let ((inhibit-redisplay t))
		  (save-window-excursion (eshell t))))
    (with-current-buffer test-buffer
      (erase-buffer)
      (setq major-mode 'eshell-test-mode)
      (setq mode-name "EShell Test")
      (set (make-local-variable 'eshell-test-failures) 0)
      (local-set-key [(control ?c) (control ?c)] 'eshell-test-goto-func)
      (local-set-key [(control ?c) (control ?r)] 'eshell-run-one-test)
      (local-set-key [(control ?m)] 'eshell-test-goto-func)
      (local-set-key [return] 'eshell-test-goto-func)

      (insert "Testing Eshell under " (emacs-version))
      (switch-to-buffer test-buffer)
      (delete-other-windows))
    (dolist (funcname (sort (all-completions "eshell-test--"
						obarray 'functionp)
			       'string-lessp))
      (with-current-buffer test-buffer
	(insert "\n"))
      (funcall (intern-soft funcname)))
    (with-current-buffer test-buffer
      (insert (format "\n\n--- %s --- (completed in %d seconds)\n"
		      (current-time-string)
		      (- (float-time) begin)))
      (message "Eshell test suite completed: %s failure%s"
	       (if (> eshell-test-failures 0)
		   (number-to-string eshell-test-failures)
		 "No")
	       (if (= eshell-test-failures 1) "" "s"))))
  (goto-char eshell-last-output-end)
  (unless arg
    (kill-buffer (current-buffer))))


(defvar eshell-metric-before-command 0)
(defvar eshell-metric-after-command 0)

(defun eshell-show-usage-metrics ()
  "If run at Eshell mode startup, metrics are shown after each command."
  (set (make-local-variable 'eshell-metric-before-command)
       (if (eq eshell-show-usage-metrics t)
	   0
	 (current-time)))
  (set (make-local-variable 'eshell-metric-after-command)
       (if (eq eshell-show-usage-metrics t)
	   0
	 (current-time)))

  (add-hook 'eshell-pre-command-hook
	    (function
	     (lambda ()
	       (setq eshell-metric-before-command
		     (if (eq eshell-show-usage-metrics t)
			 (car (memory-use-counts))
		       (current-time))))) nil t)

  (add-hook 'eshell-post-command-hook
	    (function
	     (lambda ()
	       (setq eshell-metric-after-command
		     (if (eq eshell-show-usage-metrics t)
			 (car (memory-use-counts))
		       (current-time)))
	       (eshell-interactive-print
		(concat
		 (int-to-string
		  (if (eq eshell-show-usage-metrics t)
		      (- eshell-metric-after-command
			 eshell-metric-before-command 7)
		    (- (float-time
			eshell-metric-after-command)
		       (float-time
			eshell-metric-before-command))))
		 "\n"))))
	    nil t))


;;; The tests.

(defmacro eshell-deftest (module name label &rest forms)
  (declare (indent 2))
  (if (and (fboundp 'cl-compiling-file) (cl-compiling-file))
      nil
    (let ((fsym (intern (concat "eshell-test--" (symbol-name name)))))
      `(eval-when-compile
	 (ignore
	  (defun ,fsym () ,label
	    (eshell-run-test (quote ,module) (quote ,fsym) ,label
			     (quote (progn ,@forms)))))))))


(eshell-deftest mode same-window-buffer-names
  "`eshell-buffer-name' is a member of `same-window-buffer-names'"
  (member eshell-buffer-name same-window-buffer-names))

(eshell-deftest mode eshell-directory-exists
  "`eshell-directory-name' exists and is writable"
  (file-writable-p eshell-directory-name))

(eshell-deftest mode eshell-directory-modes
  "`eshell-directory-name' has correct access protections"
  (or (eshell-under-windows-p)
      (= (file-modes eshell-directory-name)
	 eshell-private-directory-modes)))

(eshell-deftest mode simple-command-result
  "`eshell-command-result' works with a simple command."
  (= (eshell-command-result "+ 1 2") 3))


(require 'em-banner)

(eshell-deftest banner banner-displayed
  "Startup banner is displayed at point-min"
  (assert eshell-banner-message)
  (let ((msg (eval eshell-banner-message)))
    (assert msg)
    (goto-char (point-min))
    (looking-at msg)))


(require 'esh-cmd)

(eshell-deftest var last-result-var
  "\"last result\" variable"
  (eshell-command-result-p "+ 1 2; + $$ 2" "3\n5\n"))

(eshell-deftest var last-result-var2
  "\"last result\" variable"
  (eshell-command-result-p "+ 1 2; + $$ $$" "3\n6\n"))

(eshell-deftest var last-arg-var
  "\"last arg\" variable"
  (eshell-command-result-p "+ 1 2; + $_ 4" "3\n6\n"))

(eshell-deftest cmd lisp-command
  "Evaluate Lisp command"
  (eshell-command-result-p "(+ 1 2)" "3"))

(eshell-deftest cmd lisp-command-args
  "Evaluate Lisp command (ignore args)"
  (eshell-command-result-p "(+ 1 2) 3" "3"))

(eshell-deftest cmd subcommand
  "Run subcommand"
  (eshell-command-result-p "{+ 1 2}" "3\n"))

(eshell-deftest cmd subcommand-args
  "Run subcommand (ignore args)"
  (eshell-command-result-p "{+ 1 2} 3" "3\n"))

(eshell-deftest cmd subcommand-lisp
  "Run subcommand + Lisp form"
  (eshell-command-result-p "{(+ 1 2)}" "3\n"))

(eshell-deftest cmd named-command
  "Execute named command"
  (eshell-command-result-p "+ 1 2" "3\n"))


(require 'esh-mode)

(eshell-deftest mode major-mode
  "Major mode is correct"
  (eq major-mode 'eshell-mode))

(eshell-deftest mode eshell-mode-variable
  "`eshell-mode' is true"
  (eq eshell-mode t))

(eshell-deftest var window-height
  "LINES equals window height"
  (let ((eshell-stringify-t t))
    (eshell-command-result-p "= $LINES (window-height)" "t\n")))

(eshell-deftest mode command-running-p
  "Modeline shows no command running"
  (or (featurep 'xemacs)
      (not eshell-status-in-modeline)
      (and (memq 'eshell-command-running-string mode-line-format)
	   (equal eshell-command-running-string "--"))))

(eshell-deftest arg forward-arg
  "Move across command arguments"
  (eshell-insert-command "echo $(+ 1 (- 4 3)) \"alpha beta\" file" 'ignore)
  (let ((here (point)) begin valid)
    (eshell-bol)
    (setq begin (point))
    (eshell-forward-argument 4)
    (setq valid (= here (point)))
    (eshell-backward-argument 4)
    (prog1
	(and valid (= begin (point)))
      (eshell-bol)
      (delete-region (point) (point-max)))))

(eshell-deftest mode queue-input
  "Queue command input"
  (eshell-insert-command "sleep 2")
  (eshell-insert-command "echo alpha" 'eshell-queue-input)
  (let ((count 10))
    (while (and eshell-current-command
		(> count 0))
      (sit-for 1 0)
      (setq count (1- count))))
  (eshell-match-result "alpha\n"))

; (eshell-deftest proc send-to-subprocess
;   "Send input to a subprocess"
;   ;; jww (1999-12-06): what about when bc is unavailable?
;   (if (not (eshell-search-path "bc"))
;       t
;     (eshell-insert-command "bc")
;     (eshell-insert-command "1 + 2")
;     (sit-for 1 0)
;     (forward-line -1)
;     (prog1
; 	(looking-at "3\n")
;       (eshell-insert-command "quit")
;       (sit-for 1 0))))

(eshell-deftest io flush-output
  "Flush previous output"
  (eshell-insert-command "echo alpha")
  (eshell-kill-output)
  (and (eshell-match-result (regexp-quote "*** output flushed ***\n"))
       (forward-line)
       (= (point) eshell-last-output-start)))

(eshell-deftest mode run-old-command
  "Re-run an old command"
  (eshell-insert-command "echo alpha")
  (goto-char eshell-last-input-start)
  (string= (eshell-get-old-input) "echo alpha"))


(require 'esh-var)

(eshell-deftest var interp-cmd
  "Interpolate command result"
  (eshell-command-result-p "+ ${+ 1 2} 3" "6\n"))

(eshell-deftest var interp-lisp
  "Interpolate Lisp form evaluation"
  (eshell-command-result-p "+ $(+ 1 2) 3" "6\n"))

(eshell-deftest var interp-concat
  "Interpolate and concat command"
  (eshell-command-result-p "+ ${+ 1 2}3 3" "36\n"))

(eshell-deftest var interp-concat-lisp
  "Interpolate and concat Lisp form"
  (eshell-command-result-p "+ $(+ 1 2)3 3" "36\n"))

(eshell-deftest var interp-concat2
  "Interpolate and concat two commands"
  (eshell-command-result-p "+ ${+ 1 2}${+ 1 2} 3" "36\n"))

(eshell-deftest var interp-concat-lisp2
  "Interpolate and concat two Lisp forms"
  (eshell-command-result-p "+ $(+ 1 2)$(+ 1 2) 3" "36\n"))


(provide 'esh-test)

;;; esh-test.el ends here
