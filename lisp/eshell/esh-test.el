;;; esh-test.el --- Eshell test suite

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The purpose of this module is to verify that Eshell works as
;; expected.  To run it on your system, use the command
;; \\[eshell-test].

;;; Code:

(eval-when-compile
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
  "*The face used to highlight OK result strings."
  :group 'eshell-test)
;; backward-compatibility alias
(put 'eshell-test-ok-face 'face-alias 'eshell-test-ok)

(defface eshell-test-failed
  '((((class color) (background light)) (:foreground "OrangeRed" :bold t))
    (((class color) (background dark)) (:foreground "OrangeRed" :bold t))
    (t (:bold t)))
  "*The face used to highlight FAILED result strings."
  :group 'eshell-test)
;; backward-compatibility alias
(put 'eshell-test-failed-face 'face-alias 'eshell-test-failed)

(defcustom eshell-show-usage-metrics nil
  "*If non-nil, display different usage metrics for each Eshell command."
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

(eval-when-compile
  (defvar test-buffer))

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
	(delete-backward-char 6)
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
  (let* ((begin (eshell-time-to-seconds (current-time)))
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
    (eshell-for funcname (sort (all-completions "eshell-test--"
						obarray 'functionp)
			       'string-lessp)
      (with-current-buffer test-buffer
	(insert "\n"))
      (funcall (intern-soft funcname)))
    (with-current-buffer test-buffer
      (insert (format "\n\n--- %s --- (completed in %d seconds)\n"
		      (current-time-string)
		      (- (eshell-time-to-seconds (current-time))
			 begin)))
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
		    (- (eshell-time-to-seconds
			eshell-metric-after-command)
		       (eshell-time-to-seconds
			eshell-metric-before-command))))
		 "\n"))))
	    nil t))

(provide 'esh-test)

;; arch-tag: 6e32275a-8285-4a4e-b7cf-819aa7c86b8e
;;; esh-test.el ends here
