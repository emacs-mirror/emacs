;;; add-log-tests.el --- Test suite for add-log.

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Masatake YAMATO <yamato@redhat.com>
;; Keywords: vc tools

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

;;; Code:

(require 'ert)
(require 'add-log)

(defmacro add-log-current-defun-deftest (name doc major-mode
					      content marker expected-defun)
  "Generate an ert test for mode-own `add-log-current-defun-function'.
Run `add-log-current-defun' at the point where MARKER specifies in a
buffer which content is CONTENT under MAJOR-MODE. Then it compares the
result with EXPECTED-DEFUN."
  (let ((xname (intern (concat "add-log-current-defun-test-"
			       (symbol-name name)
			       ))))
    `(ert-deftest ,xname ()
	 ,doc
       (with-temp-buffer
	 (insert ,content)
	 (goto-char (point-min))
	 (funcall ',major-mode)
	 (should (equal (when (search-forward ,marker nil t)
			  (replace-match "" nil t)
			  (add-log-current-defun))
			,expected-defun))))))

(add-log-current-defun-deftest
 sh-func1
 "Test sh-current-defun-name can find function."
 sh-mode "
function foo
{
	><
}" "><" "foo")

(add-log-current-defun-deftest
 sh-func2
 "Test sh-current-defun-name can find function."
 sh-mode "
foo()
{
	><
}" "><" "foo")

(add-log-current-defun-deftest
 sh-func3
 "Test sh-current-defun-name can find function."
 sh-mode "
function foo()
{
	><
}" "><" "foo")

(add-log-current-defun-deftest
 sh-var
 "Test sh-current-defun-name can find variable definition."
 sh-mode "
PATH=a:/ab:/usr/abc
DIR=/pr><oc"
"><" "DIR")

(provide 'add-log-tests)

;;; add-log-tests.el ends here
