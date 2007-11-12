;;; compilation-perl.el --- error regexps for perl podchecker and Test

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: processes
;; URL: http://www.geocities.com/user42_kevin/compilation/index.html
;; EmacsWiki: PerlLanguage

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

;; This is a spot of code adding `compilation-error-regexp-alist'
;; patterns for perl podchecker (the Pod::Checker module) and Test and
;; Test::Harness module error output.
;;
;; Emacs already has patterns for perl's normal compile and run
;; errors, but podchecker and Test are a bit different.

;;; Install:

;; Put compilation-perl.el somewhere in your `load-path', and in
;; .emacs put
;;
;;     (eval-after-load "compile" '(require 'compilation-perl))
;;
;; There's an autoload cookie below for this, if you use
;; `update-file-autoloads' and friends.

;;; Emacsen:

;; Works in Emacs 22, Emacs 21, and XEmacs 21.

;;; History:

;; Version 1 - the first version.


;;; Code:

;;;DISABLE ###autoload (eval-after-load "compile" '(require 'compilation-perl))

(eval-after-load "compile"
  '(dolist
       (elem
	'(;; podchecker error messages, per Pod::Checker.
	  ;; The style is from the Pod::Checker::poderror() function, eg.
	  ;; *** ERROR: Spurious text after =cut at line 193 in file foo.pm
	  ;;
	  ;; Plus end_pod() can give "at line EOF" instead of a
	  ;; number, so for that match "on line N" which is the
	  ;; originating spot, eg.
	  ;; *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
	  ;;
	  ;; Plus command() can give both "on line N" and "at line N";
	  ;; the latter is desired and is matched because the .* is
	  ;; greedy.
	  ;; *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
	  ;;
	  (compilation-perl--Pod::Checker
	   "^\\*\\*\\* \\(?:ERROR\\|\\(WARNING\\)\\).* \\(?:at\\|on\\) line \\([0-9]+\\) \\(?:.* \\)?in file \\([^ \t\n]+\\)"
	   3 2 nil (1))

	  ;; perl Test module error messages.
	  ;; Style per the ok() function "$context", eg.
	  ;; # Failed test 1 in foo.t at line 6
	  ;;
	  (compilation-perl--Test
	   "^# Failed test [0-9]+ in \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
	   1 2)

	  ;; perl Test::Harness output, eg.
	  ;; NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
	  ;;
	  ;; Test::Harness is slightly designed for tty output, since
	  ;; it prints CRs to overwrite progress messages, but if you
	  ;; run it in with M-x compile this pattern can at least step
	  ;; through the failures.
	  ;;
	  (compilation-perl--Test::Harness
	   "^.*NOK.* \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
	   1 2)))

     (cond
      ((boundp 'compilation-error-regexp-systems-list)
       ;; xemacs21
       (setq elem (remove '(1) elem))	; drop "type" specifier
       (add-to-list 'compilation-error-regexp-alist-alist
		    (list (car elem) (cdr elem)))
       (compilation-build-compilation-error-regexp-alist))

      ((boundp 'compilation-error-regexp-alist-alist)
       ;; emacs22
       (add-to-list 'compilation-error-regexp-alist-alist elem)
       (add-to-list 'compilation-error-regexp-alist (car elem)))

      (t
       ;; emacs21
       (setq elem (remove '(1) elem))	; drop "type" specifier
       (add-to-list 'compilation-error-regexp-alist (cdr elem))))))

(provide 'compilation-perl)

;; arch-tag: 98818a96-69f4-4528-b9ea-4a357499a451
;;; compilation-perl.el ends here
