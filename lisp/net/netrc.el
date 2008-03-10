;;; netrc.el --- .netrc parsing functionality
;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news
;;  Modularized by Ted Zlatanov <tzz@lifelogs.com>
;;  when it was part of Gnus.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Just the .netrc parsing functionality, abstracted so other packages
;; besides Gnus can use it.

;;; Code:

;;;
;;; .netrc and .authinfo rc parsing
;;;

;; use encrypt if loaded (encrypt-file-alist has to be set as well)
(eval-and-compile
  (autoload 'encrypt-find-model "encrypt")
  (autoload 'encrypt-insert-file-contents "encrypt"))
(defalias 'netrc-point-at-eol
  (if (fboundp 'point-at-eol)
      'point-at-eol
    'line-end-position))
(eval-when-compile
  (defvar encrypt-file-alist)
  ;; This is unnecessary in the compiled version as it is a macro.
  (if (fboundp 'bound-and-true-p)
      (defalias 'netrc-bound-and-true-p 'bound-and-true-p)
    (defmacro netrc-bound-and-true-p (var)
      "Return the value of symbol VAR if it is bound, else nil."
      `(and (boundp (quote ,var)) ,var))))

(defgroup netrc nil
 "Netrc configuration."
 :group 'comm)

(defvar netrc-services-file "/etc/services"
  "The name of the services file.")

(defun netrc-parse (file)
  (interactive "fFile to Parse: ")
  "Parse FILE and return a list of all entries in the file."
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((tokens '("machine" "default" "login"
		      "password" "account" "macdef" "force"
		      "port"))
	    (encryption-model (when (netrc-bound-and-true-p encrypt-file-alist)
				(encrypt-find-model file)))
	    alist elem result pair)
	(if encryption-model
	    (encrypt-insert-file-contents file encryption-model)
	  (insert-file-contents file))
	(goto-char (point-min))
	;; Go through the file, line by line.
	(while (not (eobp))
	  (narrow-to-region (point) (point-at-eol))
	  ;; For each line, get the tokens and values.
	  (while (not (eobp))
	    (skip-chars-forward "\t ")
	    ;; Skip lines that begin with a "#".
	    (if (eq (char-after) ?#)
		(goto-char (point-max))
	      (unless (eobp)
		(setq elem
		      (if (= (following-char) ?\")
			  (read (current-buffer))
			(buffer-substring
			 (point) (progn (skip-chars-forward "^\t ")
					(point)))))
		(cond
		 ((equal elem "macdef")
		  ;; We skip past the macro definition.
		  (widen)
		  (while (and (zerop (forward-line 1))
			      (looking-at "$")))
		  (narrow-to-region (point) (point)))
		 ((member elem tokens)
		  ;; Tokens that don't have a following value are ignored,
		  ;; except "default".
		  (when (and pair (or (cdr pair)
				      (equal (car pair) "default")))
		    (push pair alist))
		  (setq pair (list elem)))
		 (t
		  ;; Values that haven't got a preceding token are ignored.
		  (when pair
		    (setcdr pair elem)
		    (push pair alist)
		    (setq pair nil)))))))
	  (when alist
	    (push (nreverse alist) result))
	  (setq alist nil
		pair nil)
	  (widen)
	  (forward-line 1))
	(nreverse result)))))

(defun netrc-machine (list machine &optional port defaultport)
  "Return the netrc values from LIST for MACHINE or for the default entry.
If PORT specified, only return entries with matching port tokens.
Entries without port tokens default to DEFAULTPORT."
  (let ((rest list)
	result)
    (while list
      (when (equal (cdr (assoc "machine" (car list))) machine)
	(push (car list) result))
      (pop list))
    (unless result
      ;; No machine name matches, so we look for default entries.
      (while rest
	(when (assoc "default" (car rest))
	  (push (car rest) result))
	(pop rest)))
    (when result
      (setq result (nreverse result))
      (while (and result
		  (not (netrc-port-equal
			(or port defaultport "nntp")
			(or (netrc-get (car result) "port")
			    defaultport "nntp"))))
	(pop result))
      (car result))))

(defun netrc-machine-user-or-password (mode authinfo-file-or-list machines ports defaults)
  "Get the user name or password according to MODE from AUTHINFO-FILE-OR-LIST.
Matches a machine from MACHINES and a port from PORTS, giving
default ports DEFAULTS to `netrc-machine'.

MODE can be \"login\" or \"password\", suitable for passing to
`netrc-get'."
  (let ((authinfo-list (if (stringp authinfo-file-or-list)
			   (netrc-parse authinfo-file-or-list)
			 authinfo-file-or-list))
	(ports (or ports '(nil)))
	(defaults (or defaults '(nil)))
	info)
    (dolist (machine machines)
      (dolist (default defaults)
	(dolist (port ports)
	  (let ((alist (netrc-machine authinfo-list machine port default)))
	  (setq info (or (netrc-get alist mode) info))))))
    info))

(defun netrc-get (alist type)
  "Return the value of token TYPE from ALIST."
  (cdr (assoc type alist)))

(defun netrc-port-equal (port1 port2)
  (when (numberp port1)
    (setq port1 (or (netrc-find-service-name port1) port1)))
  (when (numberp port2)
    (setq port2 (or (netrc-find-service-name port2) port2)))
  (equal port1 port2))

(defun netrc-parse-services ()
  (when (file-exists-p netrc-services-file)
    (let ((services nil))
      (with-temp-buffer
	(insert-file-contents netrc-services-file)
	(while (search-forward "#" nil t)
	  (delete-region (1- (point)) (point-at-eol)))
	(goto-char (point-min))
	(while (re-search-forward
		"^ *\\([^ \n\t]+\\)[ \t]+\\([0-9]+\\)/\\([^ \t\n]+\\)" nil t)
	  (push (list (match-string 1) (string-to-number (match-string 2))
		      (intern (downcase (match-string 3))))
		services))
	(nreverse services)))))

(defun netrc-find-service-name (number &optional type)
  (let ((services (netrc-parse-services))
	service)
    (setq type (or type 'tcp))
    (while (and (setq service (pop services))
		(not (and (= number (cadr service))
			  (eq type (car (cddr service)))))))
    (car service)))

(defun netrc-find-service-number (name &optional type)
  (let ((services (netrc-parse-services))
	service)
    (setq type (or type 'tcp))
    (while (and (setq service (pop services))
		(not (and (string= name (car service))
			  (eq type (car (cddr service)))))))
    (cadr service)))

(provide 'netrc)

;;; arch-tag: af9929cc-2d12-482f-936e-eb4366f9fa55
;;; netrc.el ends here
