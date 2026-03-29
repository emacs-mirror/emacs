;;; shortdoc-tests.el --- tests for shortdoc.el   -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
(require 'shortdoc)
(require 'subr-x) ; `string-pad' in shortdoc group needed at run time
(require 'regexp-opt)    ; `regexp-opt-charset' not autoloaded

(defun shortdoc-tests--tree-contains (tree fun)
  "Whether TREE contains a call to FUN."
  (and (proper-list-p tree)
       (or (eq (car tree) fun)
           (cl-some (lambda (x) (shortdoc-tests--tree-contains x fun)) tree))))

(ert-deftest shortdoc-examples ()
  "Check that each example actually contains the corresponding form."
  (dolist (group shortdoc--groups)
    (dolist (item group)
      (when (consp item)
        (let ((fun (car item))
              (props (cdr item)))
          (while props
            (when (memq (car props) '(:eval :no-eval :no-eval* :no-value))
              (let* ((example (cadr props))
                     (expr (cond
                            ((consp example) example)
                            ((stringp example) (read example)))))
                (should (shortdoc-tests--tree-contains expr fun))))
            (setq props (cddr props))))))))

(ert-deftest shortdoc-all-functions-fboundp ()
  "Check that all functions listed in shortdoc groups are `fboundp'."
  (dolist (group shortdoc--groups)
    (dolist (item group)
      (when (consp item)
        (let ((fun (car item)))
          (should (fboundp fun)))))))

(ert-deftest shortdoc-all-groups-work ()
  "Test that all defined shortdoc groups display correctly."
  (dolist (group (mapcar #'car shortdoc--groups))
    (let ((buf-name (format "*Shortdoc %s*" group)) buf)
      (unwind-protect
          (progn
            (shortdoc-display-group group)
            (should (setq buf (get-buffer buf-name))))
        (when buf
          (kill-buffer buf))))))

(defun shortdoc-tests--to-ascii (x)
  "Translate Unicode arrows to ASCII for making the test work everywhere."
  (cond ((consp x)
         (cons (shortdoc-tests--to-ascii (car x))
               (shortdoc-tests--to-ascii (cdr x))))
        ((stringp x)
         (thread-last x
                      (string-replace "⇒" "=>")
                      (string-replace "→" "->")))
        (t x)))

(ert-deftest shortdoc-function-examples-test ()
  "Test the extraction of usage examples of some Elisp functions."
  (should (equal '((list . "(delete 2 (list 1 2 3 4))\n    => (1 3 4)\n  (delete \"a\" (list \"a\" \"b\" \"c\" \"d\"))\n    => (\"b\" \"c\" \"d\")"))
                 (shortdoc-tests--to-ascii
                  (shortdoc-function-examples 'delete))))
  (should (equal '((alist . "(assq 'foo '((foo . bar) (zot . baz)))\n    => (foo . bar)")
		   (list . "(assq 'b '((a . 1) (b . 2)))\n    => (b . 2)"))
                 (shortdoc-tests--to-ascii
                  (shortdoc-function-examples 'assq))))
  (should (equal '((regexp . "(string-match-p \"^[fo]+\" \"foobar\")\n    => 0"))
                 (shortdoc-tests--to-ascii
                  (shortdoc-function-examples 'string-match-p)))))

(ert-deftest shortdoc-help-fns-examples-function-test ()
  "Test that `shortdoc-help-fns-examples-function' correctly prints Lisp function examples."
  (with-temp-buffer
    (shortdoc-help-fns-examples-function 'string-fill)
    (should (equal "\n  Examples:\n\n  (string-fill \"Three short words\" 12)\n    => \"Three short\\nwords\"\n  (string-fill \"Long-word\" 3)\n    => \"Long-word\"\n\n"
                   (shortdoc-tests--to-ascii
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (erase-buffer)
    (shortdoc-help-fns-examples-function 'assq)
    (should (equal "\n  Examples:\n\n  (assq 'foo '((foo . bar) (zot . baz)))\n    => (foo . bar)\n\n  (assq 'b '((a . 1) (b . 2)))\n    => (b . 2)\n\n"
                   (shortdoc-tests--to-ascii
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (erase-buffer)
    (shortdoc-help-fns-examples-function 'string-trim)
    (should (equal "\n  Example:\n\n  (string-trim \" foo \")\n    => \"foo\"\n\n"
                   (shortdoc-tests--to-ascii
                    (buffer-substring-no-properties (point-min)
                                                    (point-max)))))))

(provide 'shortdoc-tests)

;;; shortdoc-tests.el ends here
