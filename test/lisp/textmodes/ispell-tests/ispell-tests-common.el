;;; ispell-tests-common.el --- Shared procedures for ispell tests.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lockywolf

;; Author: Lockywolf <for_emacs_1@lockywolf.net>
;; Keywords: languages, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared procedures used by most ispell test files.

;;; Code:

(require 'ert)
(require 'ert-x)

(defvar ispell-tests--data-directory
  (let ((ert-resource-directory-trim-right-regexp "-tests/.*-tests-common\\.el"))
    (ert-resource-directory))
  "Resource directory for ispell tests.")

(defvar fake-aspell-path
  (expand-file-name "fake-aspell-new.bash" ispell-tests--data-directory)
  "Path to the mock backend.")


(let* ((backend-binaries (list "ispell" "aspell"  "hunspell"  "enchant-2" fake-aspell-path))
       (filter-binaries (seq-filter
                         (lambda (b)
                           (and
                            (executable-find b)
                            (equal 0
                                   (with-temp-buffer
                                     (call-process b nil t nil "-a")))))
                         backend-binaries)))

  (defun ispell-tests--some-backend-available-p ()
    "Return t if some spellchecking backend is available. "
    (not
     (null filter-binaries)))

  (defun ispell-tests--some-backend ()
    "Return the string of some available backend."
    (let ((retval (car filter-binaries)))
      (message "available backend is:%s" retval)
      retval)))

(defun ispell-tests--some-valid-dictionary (backend)
  "Return some dictionary name working for BACKEND."
  (cond ((string-equal backend "ispell")
         (with-temp-buffer
           (call-process backend nil t nil "-vv")
           (let* ((s "LIBDIR = ")
                  (slen (length s))
                  (_ (search-backward s))
                  (b (+ (point) slen 1))
                  (e (- (line-end-position) 1))
                  (ldir (buffer-substring b e))
                  (d (file-name-sans-extension
                      (file-name-nondirectory
                       (car (directory-files ldir t "\\.aff\\'"))))))
             d)))
        ((string-equal backend "aspell")
         (with-temp-buffer
           (call-process backend nil t nil "dump" "dicts")
           (goto-char 1)
           (buffer-substring 1 (line-end-position))))
        ((string-equal backend "hunspell")
         (with-temp-buffer
           (call-process backend nil t nil "-D")
           (search-backward "AVAILABLE DICTIONARIES" nil t)
           (forward-line 1)
           (let* ((s (buffer-substring (point) (line-end-position))))
             (file-name-sans-extension
              (file-name-nondirectory s)))))
        (t "english")))

(eval-when-compile
  (require 'cl-macs))
(cl-defmacro ispell-tests--letopt (bindings &body body)
  "Bind BINDINGS with `setopt', then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a list (SYMBOL VALUEFORM)
\(which binds SYMBOL to the value of VALUEFORM with `setopt').
This macro is not expected to be used outside of
ispell-tests.  As `setopt' is naturally mutative,
the environment after the end of the form is not
guaranteed to be identical to the one before.  But the form
tries its best."
  (declare (indent 1) (debug cl-letf))
  (let* ((binding-var (lambda (binding) (car binding)))
	 (binding-val (lambda (binding) (cadr binding)))
	 (make-setopt
          (lambda (a b)
	    (list 'setopt a b)))
         (add-ignore-errors
          (lambda (a)
            (list 'ignore-errors a)))
	 (vars (seq-map binding-var bindings))
	 (values (seq-map binding-val bindings))
	 (temp-vars (seq-map #'gensym vars))
	 (savebindings (seq-mapn #'list temp-vars vars))
	 (tempbindings (seq-mapn make-setopt vars values))
	 (restorebindings (seq-mapn add-ignore-errors (seq-mapn make-setopt vars temp-vars))))
    `(let ,savebindings
       (unwind-protect (progn ,@tempbindings
			      ,@body)
	 ,@(reverse restorebindings)))))

(cl-defmacro ispell-tests--with-ispell-global-dictionary (ldict &body body)
  "Temporarily bind `ispell-global-dictionary' to value of LDICT, then eval BODY.
Then attempt to restore the original value of
`ispell-global-dictionary', which may fail, but this form tries
its best."
  (declare (indent 1) (debug t))
  (let* ((dictionary-val ldict)
	 (temp-var (gensym 'old-dictionary)))
    `(let ((,temp-var (symbol-value 'ispell-dictionary)))
       (unwind-protect (progn (ispell-change-dictionary ,dictionary-val t)
			      ,@body)
         (ignore-errors (ispell-change-dictionary ,temp-var))))))

(defconst ispell-tests--constants/english/correct-list
  '("hello" "test" "test" "more" "obvious" "word"))
(defconst ispell-tests--constants/english/correct-one "hello")
(defconst ispell-tests--constants/english/wrong "hellooooooo")
(defconst ispell-tests--constants/russian/correct "привет")
(defconst ispell-tests--constants/russian/wrong "ыфаывфафыввпфыв")
(defconst ispell-tests--constants/completion "waveguides")
(defconst ispell-tests--constants/nonexistent-dictionary "2110001888290146229")

(provide 'ispell-tests-common)

;;; ispell-tests-common.el ends here
