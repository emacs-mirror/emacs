;;; esh-opt-tests.el --- esh-opt test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

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
(require 'esh-opt)

(ert-deftest esh-opt-process-args-test ()
  "Unit tests which verify correct behavior of `eshell--process-args'."
  (should
   (equal '(t)
          (eshell--process-args
           "sudo"
           '("-a")
           '((?a "all" nil show-all "")))))
  (should
   (equal '(nil)
          (eshell--process-args
           "sudo"
           '("-g")
           '((?a "all" nil show-all "")))))
  (should
   (equal '("root" "world")
          (eshell--process-args
           "sudo"
           '("-u" "root" "world")
           '((?u "user" t user "execute a command as another USER")))))
  (should
   (equal '(nil "emerge" "-uDN" "world")
          (eshell--process-args
           "sudo"
           '("emerge" "-uDN" "world")
           '((?u "user" t user "execute a command as another USER")
             :parse-leading-options-only))))
  (should
   (equal '("root" "emerge" "-uDN" "world")
          (eshell--process-args
           "sudo"
           '("-u" "root" "emerge" "-uDN" "world")
           '((?u "user" t user "execute a command as another USER")
             :parse-leading-options-only))))
  (should
   (equal '("DN" "emerge" "world")
          (eshell--process-args
           "sudo"
           '("-u" "root" "emerge" "-uDN" "world")
           '((?u "user" t user "execute a command as another USER"))))))

(ert-deftest test-eshell-eval-using-options ()
  "Tests for `eshell-eval-using-options'."
  ;; Test short options.
  (eshell-eval-using-options
   "ls" '("-a" "/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all t))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all nil))
   (should (equal args '("/some/path"))))

  ;; Test long options.
  (eshell-eval-using-options
   "ls" '("--all" "/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all t))
   (should (equal args '("/some/path"))))

  ;; Test options with constant values.
  (eshell-eval-using-options
   "ls" '("/some/path" "-h")
   '((?h "human-readable" 1024 human-readable
         "print sizes in human readable format"))
   (should (eql human-readable 1024))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("/some/path" "--human-readable")
   '((?h "human-readable" 1024 human-readable
         "print sizes in human readable format"))
   (should (eql human-readable 1024))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("/some/path")
   '((?h "human-readable" 1024 human-readable
         "print sizes in human readable format"))
   (should (eq human-readable nil))
   (should (equal args '("/some/path"))))

  ;; Test options with user-specified values.
  (eshell-eval-using-options
   "ls" '("-I" "*.txt" "/some/path")
   '((?I "ignore" t ignore-pattern
         "do not list implied entries matching pattern"))
   (should (equal ignore-pattern "*.txt"))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("-I*.txt" "/some/path")
   '((?I "ignore" t ignore-pattern
         "do not list implied entries matching pattern"))
   (should (equal ignore-pattern "*.txt"))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("--ignore" "*.txt" "/some/path")
   '((?I "ignore" t ignore-pattern
         "do not list implied entries matching pattern"))
   (should (equal ignore-pattern "*.txt"))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("--ignore=*.txt" "/some/path")
   '((?I "ignore" t ignore-pattern
         "do not list implied entries matching pattern"))
   (should (equal ignore-pattern "*.txt"))
   (should (equal args '("/some/path"))))

  ;; Test multiple short options in a single token.
  (eshell-eval-using-options
   "ls" '("-al" "/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with .")
     (?l nil long-listing listing-style
         "use a long listing format"))
   (should (eq t show-all))
   (should (eql listing-style 'long-listing))
   (should (equal args '("/some/path"))))
  (eshell-eval-using-options
   "ls" '("-aI*.txt" "/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with .")
     (?I "ignore" t ignore-pattern
         "do not list implied entries matching pattern"))
   (should (eq t show-all))
   (should (equal ignore-pattern "*.txt"))
   (should (equal args '("/some/path"))))

  ;; Test that "--" terminates options.
  (eshell-eval-using-options
   "ls" '("--" "-a")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all nil))
   (should (equal args '("-a"))))
  (eshell-eval-using-options
   "ls" '("--" "--all")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all nil))
   (should (equal args '("--all"))))

  ;; Test :parse-leading-options-only.
  (eshell-eval-using-options
   "sudo" '("-u" "root" "whoami")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (equal user "root"))
   (should (equal args '("whoami"))))
  (eshell-eval-using-options
   "sudo" '("--user" "root" "whoami")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (equal user "root"))
   (should (equal args '("whoami"))))
  (eshell-eval-using-options
   "sudo" '("emerge" "-uDN" "world")
   '((?u "user" t user "execute a command as another USER"))
   (should (equal user "DN"))
   (should (equal args '("emerge" "world"))))
  (eshell-eval-using-options
   "sudo" '("emerge" "-uDN" "world")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (eq user nil))
   (should (equal args '("emerge" "-uDN" "world")))))

(provide 'esh-opt-tests)

;;; esh-opt-tests.el ends here
