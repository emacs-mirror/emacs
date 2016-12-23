;;; files-tests.el --- tests for files.el.

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)

;; Set to t if the local variable was set, `query' if the query was
;; triggered.
(defvar files-test-result nil)

(defvar files-test-safe-result nil)
(put 'files-test-safe-result 'safe-local-variable 'booleanp)

(defun files-test-fun1 ()
  (setq files-test-result t))

;; Test combinations:
;; `enable-local-variables' t, nil, :safe, :all, or something else.
;; `enable-local-eval' t, nil, or something else.

(defvar files-test-local-variable-data
  ;; Unsafe eval form
  '((("eval: (files-test-fun1)")
     (t t         (eq files-test-result t))
     (t nil       (eq files-test-result nil))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result nil))
     (:all maybe  (eq files-test-result t)) ; This combination is ambiguous.
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result nil))
     (maybe maybe (eq files-test-result 'query)))
    ;; Unsafe local variable value
    (("files-test-result: t")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-result nil))
     (nil nil     (eq files-test-result nil))
     (nil maybe   (eq files-test-result nil))
     (:safe t     (eq files-test-result nil))
     (:safe nil   (eq files-test-result nil))
     (:safe maybe (eq files-test-result nil))
     (:all t      (eq files-test-result t))
     (:all nil    (eq files-test-result t))
     (:all maybe  (eq files-test-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable
    (("files-test-safe-result: t")
     (t t         (eq files-test-safe-result t))
     (t nil       (eq files-test-safe-result t))
     (t maybe     (eq files-test-safe-result t))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result t))
     (:safe nil   (eq files-test-safe-result t))
     (:safe maybe (eq files-test-safe-result t))
     (:all t      (eq files-test-safe-result t))
     (:all nil    (eq files-test-safe-result t))
     (:all maybe  (eq files-test-safe-result t))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query)))
    ;; Safe local variable with unsafe value
    (("files-test-safe-result: 1")
     (t t         (eq files-test-result 'query))
     (t nil       (eq files-test-result 'query))
     (t maybe     (eq files-test-result 'query))
     (nil t       (eq files-test-safe-result nil))
     (nil nil     (eq files-test-safe-result nil))
     (nil maybe   (eq files-test-safe-result nil))
     (:safe t     (eq files-test-safe-result nil))
     (:safe nil   (eq files-test-safe-result nil))
     (:safe maybe (eq files-test-safe-result nil))
     (:all t      (eq files-test-safe-result 1))
     (:all nil    (eq files-test-safe-result 1))
     (:all maybe  (eq files-test-safe-result 1))
     (maybe t     (eq files-test-result 'query))
     (maybe nil   (eq files-test-result 'query))
     (maybe maybe (eq files-test-result 'query))))
  "List of file-local variable tests.
Each list element should have the form

  (LOCAL-VARS-LIST . TEST-LIST)

where LOCAL-VARS-LISTS should be a list of local variable
definitions (strings) and TEST-LIST is a list of tests to
perform.  Each entry of TEST-LIST should have the form

 (ENABLE-LOCAL-VARIABLES ENABLE-LOCAL-EVAL FORM)

where ENABLE-LOCAL-VARIABLES is the value to assign to
`enable-local-variables', ENABLE-LOCAL-EVAL is the value to
assign to `enable-local-eval', and FORM is a desired `should'
form.")

(defun file-test--do-local-variables-test (str test-settings)
  (with-temp-buffer
    (insert str)
    (setq files-test-result nil
	  files-test-safe-result nil)
    (let ((enable-local-variables (nth 0 test-settings))
	  (enable-local-eval      (nth 1 test-settings))
	  ;; Prevent any dir-locals file interfering with the tests.
	  (enable-dir-local-variables nil)
	  (files-test-queried nil))
      (hack-local-variables)
      (eval (nth 2 test-settings)))))

(ert-deftest files-test-local-variables ()
  "Test the file-local variables implementation."
  (unwind-protect
      (progn
	(defadvice hack-local-variables-confirm (around files-test activate)
	  (setq files-test-result 'query)
	  nil)
	(dolist (test files-test-local-variable-data)
	  (let ((str (concat "text\n\n;; Local Variables:\n;; "
			     (mapconcat 'identity (car test) "\n;; ")
			     "\n;; End:\n")))
	    (dolist (subtest (cdr test))
	      (should (file-test--do-local-variables-test str subtest))))))
    (ad-disable-advice 'hack-local-variables-confirm 'around 'files-test)))

(defvar files-test-bug-18141-file
  (expand-file-name "data/files-bug18141.el.gz" (getenv "EMACS_TEST_DIRECTORY"))
  "Test file for bug#18141.")

(ert-deftest files-test-bug-18141 ()
  "Test for http://debbugs.gnu.org/18141 ."
  (skip-unless (executable-find "gzip"))
  (let ((tempfile (make-temp-file "files-test-bug-18141" nil ".gz")))
    (unwind-protect
	(progn
	  (copy-file files-test-bug-18141-file tempfile t)
	  (with-current-buffer (find-file-noselect tempfile)
	    (set-buffer-modified-p t)
	    (save-buffer)
	    (should (eq buffer-file-coding-system 'iso-2022-7bit-unix))))
      (delete-file tempfile))))


;; Stop the above "Local Var..." confusing Emacs.


(ert-deftest files-test-bug-21454 ()
  "Test for http://debbugs.gnu.org/21454 ."
  :expected-result :failed
  (let ((input-result
         '(("/foo/bar//baz/:/bar/foo/baz//" nil ("/foo/bar/baz/" "/bar/foo/baz/"))
           ("/foo/bar/:/bar/qux/:/qux/foo" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("//foo/bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo/bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo//bar/:/bar/qux/:/qux/foo/" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo//bar/:/bar/qux/:/qux/foo" nil ("/foo/bar/" "/bar/qux/" "/qux/foo/"))
           ("/foo/bar" "$FOO/baz/:/qux/foo/" ("/foo/bar/baz/" "/qux/foo/"))
           ("//foo/bar/" "$FOO/baz/:/qux/foo/" ("/foo/bar/baz/" "/qux/foo/"))))
        (foo-env (getenv "FOO"))
        (bar-env (getenv "BAR")))
    (unwind-protect
        (dolist (test input-result)
          (let ((foo (nth 0 test))
                (bar (nth 1 test))
                (res (nth 2 test)))
            (setenv "FOO" foo)
            (if bar
                (progn
                  (setenv "BAR" bar)
                  (should (equal res (parse-colon-path (getenv "BAR")))))
              (should (equal res (parse-colon-path "$FOO"))))))
      (setenv "FOO" foo-env)
      (setenv "BAR" bar-env))))

(ert-deftest files-test--save-buffers-kill-emacs--confirm-kill-processes ()
  "Test that `save-buffers-kill-emacs' honors
`confirm-kill-processes'."
  (cl-letf* ((yes-or-no-p-prompts nil)
             ((symbol-function #'yes-or-no-p)
              (lambda (prompt)
                (push prompt yes-or-no-p-prompts)
                nil))
             (kill-emacs-args nil)
             ((symbol-function #'kill-emacs)
              (lambda (&optional arg) (push arg kill-emacs-args)))
             (process
              (make-process
               :name "sleep"
               :command (list
                         (expand-file-name invocation-name invocation-directory)
                         "-batch" "-Q" "-eval" "(sleep-for 1000)")))
             (confirm-kill-processes nil))
    (save-buffers-kill-emacs)
    (kill-process process)
    (should-not yes-or-no-p-prompts)
    (should (equal kill-emacs-args '(nil)))))

(ert-deftest files-test-read-file-in-~ ()
  "Test file prompting in directory named '~'.
If we are in a directory named '~', the default value should not
be $HOME."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll &optional _pred _req init _hist def _)
               (or def init)))
            (dir (make-temp-file "read-file-name-test" t)))
    (unwind-protect
        (let ((subdir (expand-file-name "./~/" dir)))
          (make-directory subdir t)
          (with-temp-buffer
            (setq default-directory subdir)
            (should-not (equal
                         (expand-file-name (read-file-name "File: "))
                         (expand-file-name "~/")))
            ;; Don't overquote either!
            (setq default-directory (concat "/:" subdir))
            (should-not (equal
                         (expand-file-name (read-file-name "File: "))
                         (concat "/:/:" subdir)))))
      (delete-directory dir 'recursive))))

(provide 'files-tests)
;;; files-tests.el ends here
