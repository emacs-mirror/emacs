;;; em-glob-tests.el --- em-glob test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Commentary:

;; Tests for Eshell's glob expansion.

;;; Code:

(require 'tramp)
(require 'ert)
(require 'ert-x)
(require 'em-glob)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-prefer-lisp-functions)

(defmacro with-fake-files (files &rest body)
  "Evaluate BODY forms, pretending that FILES exist on the filesystem.
FILES is a list of file names that should be reported as
appropriate by `file-name-all-completions'.  Any file name
component ending in \"symlink\" is treated as a symbolic link."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'file-name-all-completions)
              (lambda (file directory)
                (cl-assert (string= file ""))
                (setq directory (expand-file-name directory))
                `("./" "../"
                  ,@(delete-dups
                     (remq nil
                           (mapcar
                            (lambda (file)
                              (setq file (expand-file-name file))
                              (when (string-prefix-p directory file)
                                (replace-regexp-in-string
                                 "/.*" "/"
                                 (substring file (length directory)))))
                            ,files))))))
             ((symbol-function 'file-symlink-p)
              (lambda (file)
                (string-suffix-p "symlink" file))))
     ,@body))

;;; Tests:


;; Glob expansion

(ert-deftest em-glob-test/expand/splice-results ()
  "Test that globs are spliced into the argument list when
`eshell-glob-splice-results' is non-nil."
  (let ((eshell-prefer-lisp-functions t)
        (eshell-glob-splice-results t))
    (with-fake-files '("a.el" "b.el" "c.txt")
      ;; Ensure the default expansion splices the glob.
      (eshell-command-result-equal "funcall list *.el" '("a.el" "b.el"))
      (eshell-command-result-equal "funcall list *.txt" '("c.txt"))
      ;; When splitting, no-matches cases also return a list containing
      ;; the original non-matching glob.
      (eshell-command-result-equal "funcall list *.no" '("*.no"))
      (when (eshell-tests-remote-accessible-p)
        (let ((remote (file-remote-p ert-remote-temporary-file-directory)))
          (eshell-command-result-equal (format "funcall list %s~/a.el" remote)
                                       `(,(format "%s~/a.el" remote))))))))

(ert-deftest em-glob-test/expand/no-splice-results ()
  "Test that globs are treated as lists when
`eshell-glob-splice-results' is nil."
  (let ((eshell-prefer-lisp-functions t)
        (eshell-glob-splice-results nil))
    (with-fake-files '("a.el" "b.el" "c.txt")
      ;; Ensure the default expansion splices the glob.
      (eshell-command-result-equal "funcall list *.el" '(("a.el" "b.el")))
      (eshell-command-result-equal "funcall list *.txt" '(("c.txt")))
      ;; The no-matches cases are special here: the glob is just the
      ;; string, not the list of results.
      (eshell-command-result-equal "funcall list *.no" '("*.no"))
      (when (eshell-tests-remote-accessible-p)
        (let ((remote (file-remote-p ert-remote-temporary-file-directory)))
          (eshell-command-result-equal (format "funcall list %s~/a.el" remote)
                                       `(,(format "%s~/a.el" remote))))))))

(ert-deftest em-glob-test/expand/explicitly-splice-results ()
  "Test explicitly splicing globs works the same no matter the
value of `eshell-glob-splice-results'."
  (let ((eshell-prefer-lisp-functions t))
    (dolist (eshell-glob-splice-results '(nil t))
      (ert-info ((format "eshell-glob-splice-results: %s"
                         eshell-glob-splice-results))
        (with-fake-files '("a.el" "b.el" "c.txt")
          (eshell-command-result-equal "funcall list $@{listify *.el}"
                                       '("a.el" "b.el"))
          (eshell-command-result-equal "funcall list $@{listify *.txt}"
                                       '("c.txt"))
          (eshell-command-result-equal "funcall list $@{listify *.no}"
                                       '("*.no")))))))

(ert-deftest em-glob-test/expand/explicitly-listify-results ()
  "Test explicitly listifying globs works the same no matter the
value of `eshell-glob-splice-results'."
  (let ((eshell-prefer-lisp-functions t))
    (dolist (eshell-glob-splice-results '(nil t))
      (ert-info ((format "eshell-glob-splice-results: %s"
                         eshell-glob-splice-results))
        (with-fake-files '("a.el" "b.el" "c.txt")
          (eshell-command-result-equal "funcall list ${listify *.el}"
                                       '(("a.el" "b.el")))
          (eshell-command-result-equal "funcall list ${listify *.txt}"
                                       '(("c.txt")))
          (eshell-command-result-equal "funcall list ${listify *.no}"
                                       '(("*.no"))))))))


;; Glob conversion

(ert-deftest em-glob-test/convert/current-start-directory ()
  "Test converting a glob starting in the current directory."
  (should (equal (eshell-glob-convert (eshell-parse-glob-string "*.el"))
                 '("./" (("\\`.*\\.el\\'" . "\\`\\.")) nil))))

(ert-deftest em-glob-test/convert/relative-start-directory ()
  "Test converting a glob starting in a relative directory."
  (should (equal (eshell-glob-convert
                  (eshell-parse-glob-string "some/where/*.el"))
                 '("./some/where/" (("\\`.*\\.el\\'" . "\\`\\.")) nil))))

(ert-deftest em-glob-test/convert/absolute-start-directory ()
  "Test converting a glob starting in an absolute directory."
  (should (equal (eshell-glob-convert
                  (eshell-parse-glob-string "/some/where/*.el"))
                 '("/some/where/" (("\\`.*\\.el\\'" . "\\`\\.")) nil))))

(ert-deftest em-glob-test/convert/remote-start-directory ()
  "Test converting a glob starting in a remote directory."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let* ((default-directory ert-remote-temporary-file-directory)
         (remote (file-remote-p default-directory)))
    (should (equal (eshell-glob-convert
                    (format (eshell-parse-glob-string "%s/some/where/*.el")
                            remote))
                 `(,(format "%s/some/where/" remote)
                   (("\\`.*\\.el\\'" . "\\`\\.")) nil)))))

(ert-deftest em-glob-test/convert/start-directory-with-spaces ()
  "Test converting a glob starting in a directory with spaces in its name."
  (should (equal (eshell-glob-convert
                  (eshell-parse-glob-string "some where/*.el"))
                 '("./some where/" (("\\`.*\\.el\\'" . "\\`\\.")) nil))))

(ert-deftest em-glob-test/convert/literal-characters ()
  "Test converting a \"glob\" with only literal characters."
  (should (equal (eshell-glob-convert "*.el") '("./*.el" nil nil)))
  (should (equal (eshell-glob-convert "**/") '("./**/" nil t))))

(ert-deftest em-glob-test/convert/mixed-literal-characters ()
  "Test converting a glob with some literal characters."
  (should (equal (eshell-glob-convert (eshell-parse-glob-string "\\*\\*/*.el"))
                  '("./**/" (("\\`.*\\.el\\'" . "\\`\\.")) nil)))
  (should (equal (eshell-glob-convert (eshell-parse-glob-string "**/\\*.el"))
                  '("./" (recurse ("\\`\\*\\.el\\'" . "\\`\\.")) nil))))


;; Glob matching

(ert-deftest em-glob-test/match-any-string ()
  "Test that \"*\" pattern matches any string."
  (with-fake-files '("a.el" "b.el" "c.txt" "dir/a.el")
    (should (equal (eshell-extended-glob "*.el")
                   '("a.el" "b.el")))))

(ert-deftest em-glob-test/match-any-directory ()
  "Test that \"*/\" pattern matches any directory."
  (with-fake-files '("a.el" "b.el" "dir/a.el" "dir/sub/a.el" "symlink/")
    (should (equal (eshell-extended-glob "*/")
                   '("dir/" "symlink/")))))

(ert-deftest em-glob-test/match-any-character ()
  "Test that \"?\" pattern matches any character."
  (with-fake-files '("a.el" "b.el" "ccc.el" "d.txt" "dir/a.el")
    (should (equal (eshell-extended-glob "?.el")
                   '("a.el" "b.el")))))

(ert-deftest em-glob-test/match-recursive ()
  "Test that \"**/\" recursively matches directories."
  (with-fake-files '("a.el" "b.el" "ccc.el" "d.txt" "dir/a.el" "dir/sub/a.el"
                     "dir/symlink/a.el" "symlink/a.el" "symlink/sub/a.el")
    (should (equal (eshell-extended-glob "**/a.el")
                   '("a.el" "dir/a.el" "dir/sub/a.el")))
    (should (equal (eshell-extended-glob "**/")
                   '("dir/" "dir/sub/")))))

(ert-deftest em-glob-test/match-recursive-follow-symlinks ()
  "Test that \"***/\" recursively matches directories, following symlinks."
  (with-fake-files '("a.el" "b.el" "ccc.el" "d.txt" "dir/a.el" "dir/sub/a.el"
                     "dir/symlink/a.el" "symlink/a.el" "symlink/sub/a.el")
    (should (equal (eshell-extended-glob "***/a.el")
                   '("a.el" "dir/a.el" "dir/sub/a.el" "dir/symlink/a.el"
                     "symlink/a.el" "symlink/sub/a.el")))
    (should (equal (eshell-extended-glob "***/")
                   '("dir/" "dir/sub/" "dir/symlink/" "symlink/"
                     "symlink/sub/")))))

(ert-deftest em-glob-test/match-recursive-mixed ()
  "Test combination of \"**/\" and \"***/\"."
  (with-fake-files '("dir/a.el" "dir/sub/a.el" "dir/sub2/a.el"
                     "dir/symlink/a.el" "dir/sub/symlink/a.el" "symlink/a.el"
                     "symlink/sub/a.el" "symlink/sub/symlink/a.el")
    (should (equal (eshell-extended-glob "**/sub/***/a.el")
                   '("dir/sub/a.el" "dir/sub/symlink/a.el")))
    (should (equal (eshell-extended-glob "***/sub/**/a.el")
                   '("dir/sub/a.el" "symlink/sub/a.el")))))

(ert-deftest em-glob-test/match-character-set-individual ()
  "Test \"[...]\" for individual characters."
  (with-fake-files '("a.el" "b.el" "c.el" "d.el" "dir/a.el")
    (should (equal (eshell-extended-glob "[ab].el")
                   '("a.el" "b.el")))
    (should (equal (eshell-extended-glob "[^ab].el")
                   '("c.el" "d.el")))))

(ert-deftest em-glob-test/match-character-set-range ()
  "Test \"[...]\" for character ranges."
  (with-fake-files '("a.el" "b.el" "c.el" "d.el" "dir/a.el")
    (should (equal (eshell-extended-glob "[a-c].el")
                   '("a.el" "b.el" "c.el")))
    (should (equal (eshell-extended-glob "[^a-c].el")
                   '("d.el")))))

(ert-deftest em-glob-test/match-character-set-class ()
  "Test \"[...]\" for character classes."
  (with-fake-files '("1.el" "a.el" "b.el" "c.el" "dir/a.el")
    (should (equal (eshell-extended-glob "[[:alpha:]].el")
                   '("a.el" "b.el" "c.el")))
    (should (equal (eshell-extended-glob "[^[:alpha:]].el")
                   '("1.el")))))

(ert-deftest em-glob-test/match-character-set-mixed ()
  "Test \"[...]\" with multiple kinds of members at once."
  (with-fake-files '("1.el" "a.el" "b.el" "c.el" "d.el" "dir/a.el")
    (should (equal (eshell-extended-glob "[ac-d[:digit:]].el")
                   '("1.el" "a.el" "c.el" "d.el")))
    (should (equal (eshell-extended-glob "[^ac-d[:digit:]].el")
                   '("b.el")))))

(ert-deftest em-glob-test/match-group-alternative ()
  "Test \"(x|y)\" matches either \"x\" or \"y\"."
  (with-fake-files '("em-alias.el" "em-banner.el" "esh-arg.el" "misc.el"
                     "test/em-xtra.el")
    (should (equal (eshell-extended-glob "e(m|sh)-*.el")
                   '("em-alias.el" "em-banner.el" "esh-arg.el")))))

(ert-deftest em-glob-test/match-n-or-more-characters ()
  "Test that \"x#\" and \"x#\" match zero or more instances of \"x\"."
  (with-fake-files '("h.el" "ha.el" "hi.el" "hii.el" "dir/hi.el")
    (should (equal (eshell-extended-glob "hi#.el")
                   '("h.el" "hi.el" "hii.el")))
    (should (equal (eshell-extended-glob "hi##.el")
                   '("hi.el" "hii.el")))))

(ert-deftest em-glob-test/match-n-or-more-groups ()
  "Test that \"(x)#\" and \"(x)#\" match zero or more instances of \"(x)\"."
  (with-fake-files '("h.el" "ha.el" "hi.el" "hah.el" "hahah.el" "dir/hah.el")
    (should (equal (eshell-extended-glob "h(ah)#.el")
                   '("h.el" "hah.el" "hahah.el")))
    (should (equal (eshell-extended-glob "h(ah)##.el")
                   '("hah.el" "hahah.el")))))

(ert-deftest em-glob-test/match-n-or-more-character-sets ()
  "Test that \"[x]#\" and \"[x]#\" match zero or more instances of \"[x]\"."
  (with-fake-files '("w.el" "wh.el" "wha.el" "whi.el" "whaha.el" "dir/wha.el")
    (should (equal (eshell-extended-glob "w[ah]#.el")
                   '("w.el" "wh.el" "wha.el" "whaha.el")))
    (should (equal (eshell-extended-glob "w[ah]##.el")
                   '("wh.el" "wha.el" "whaha.el")))))

(ert-deftest em-glob-test/match-x-but-not-y ()
  "Test that \"x~y\" matches \"x\" but not \"y\"."
  (with-fake-files '("1" "12" "123" "42" "dir/1")
    (should (equal (eshell-extended-glob "[[:digit:]]##~4?")
                   '("1" "12" "123")))))

(ert-deftest em-glob-test/match-dot-files ()
  "Test that dot files are matched correctly."
  (with-fake-files '("foo.el" ".emacs")
    (should (equal (eshell-extended-glob ".*")
                   '("../" "./" ".emacs")))
    (let (eshell-glob-include-dot-dot)
      (should (equal (eshell-extended-glob ".*")
                     '(".emacs"))))
    (let ((eshell-glob-include-dot-files t))
      (should (equal (eshell-extended-glob "*")
                     '("../" "./" ".emacs" "foo.el")))
      (let (eshell-glob-include-dot-dot)
        (should (equal (eshell-extended-glob "*")
                       '(".emacs" "foo.el")))))))

(ert-deftest em-glob-test/no-matches ()
  "Test behavior when a glob fails to match any files."
  (with-fake-files '("foo.el" "bar.el")
    (should (equal-including-properties (eshell-extended-glob "*.txt")
             "*.txt"))
    (let ((eshell-glob-splice-results t))
      (should (equal-including-properties (eshell-extended-glob "*.txt")
               '("*.txt"))))
    (let ((eshell-error-if-no-glob t))
      (should-error (eshell-extended-glob "*.txt")))))

(ert-deftest em-glob-test/remote-user-directory ()
  "Test that remote directories using \"~\" pass through unchanged."
  (skip-unless (eshell-tests-remote-accessible-p))
  (let* ((default-directory ert-remote-temporary-file-directory)
         (remote (file-remote-p default-directory))
         (eshell-error-if-no-glob t))
    (should (equal (eshell-extended-glob (format "%s~/file.txt" remote))
                   (format "%s~/file.txt" remote)))))

;; Compatibility tests


(ert-deftest em-glob-test/test-command-without-pred ()
  "Test that the \"[\" command works when `eshell-pred' is disabled."
  (skip-unless (executable-find "["))
  (let ((eshell-modules-list (remq 'eshell-pred eshell-modules-list)))
    (with-temp-eshell
      (eshell-match-command-output "[ foo = foo ]" "\\`\\'")
      (should (= eshell-last-command-status 0)))))

;; em-glob-tests.el ends here
