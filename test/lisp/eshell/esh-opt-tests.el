;;; esh-opt-tests.el --- esh-opt test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

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

(ert-deftest esh-opt-test/process-args ()
  "Test behavior of `eshell--process-args'."
  (should
   (equal '(t)
          (eshell--process-args
           "sudo" '("-a")
           '((?a "all" nil show-all
                 "do not ignore entries starting with ."))
           '(show-all))))
  (should
   (equal '("root" "world")
          (eshell--process-args
           "sudo" '("-u" "root" "world")
           '((?u "user" t user
                 "execute a command as another USER"))
           '(user)))))

(ert-deftest esh-opt-test/process-args-parse-leading-options-only ()
  "Test behavior of :parse-leading-options-only in `eshell--process-args'."
  (should
   (equal '(nil "emerge" "-uDN" "world")
          (eshell--process-args
           "sudo" '("emerge" "-uDN" "world")
           '((?u "user" t user
                 "execute a command as another USER")
             :parse-leading-options-only)
           '(user))))
  (should
   (equal '("root" "emerge" "-uDN" "world")
          (eshell--process-args
           "sudo" '("-u" "root" "emerge" "-uDN" "world")
           '((?u "user" t user
                 "execute a command as another USER")
             :parse-leading-options-only)
           '(user))))
  (should
   (equal '("DN" "emerge" "world")
          (eshell--process-args
           "sudo" '("-u" "root" "emerge" "-uDN" "world")
           '((?u "user" t user
                 "execute a command as another USER"))
           '(user)))))

(ert-deftest esh-opt-test/process-args-external ()
  "Test behavior of :external in `eshell--process-args'."
  (cl-letf (((symbol-function 'eshell-search-path) #'ignore))
    (should
     (equal '(nil "/some/path")
            (eshell--process-args
             "ls" '("/some/path")
             '((?a "all" nil show-all
                   "do not ignore entries starting with .")
               :external "ls")
             '(show-all)))))
  (cl-letf (((symbol-function 'eshell-search-path) #'identity))
    (should
     (equal '(no-catch eshell-ext-command "ls")
            (should-error
             (eshell--process-args
              "ls" '("-u" "/some/path")
              '((?a "all" nil show-all
                    "do not ignore entries starting with .")
                :external "ls")
              '(show-all))
             :type 'no-catch))))
  (cl-letf (((symbol-function 'eshell-search-path) #'ignore))
    (should-error
     (eshell--process-args
      "ls" '("-u" "/some/path")
      '((?a "all" nil show-all
            "do not ignore entries starting with .")
        :external "ls")
      '(show-all))
     :type 'error)))

(ert-deftest esh-opt-test/eval-using-options-short ()
  "Test `eshell-eval-using-options' with short options."
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
   (should (equal args '("/some/path")))))

(ert-deftest esh-opt-test/eval-using-options-long ()
  "Test `eshell-eval-using-options' with long options."
  (eshell-eval-using-options
   "ls" '("--all" "/some/path")
   '((?a "all" nil show-all
         "do not ignore entries starting with ."))
   (should (eq show-all t))
   (should (equal args '("/some/path")))))

(ert-deftest esh-opt-test/eval-using-options-constant ()
  "Test `eshell-eval-using-options' with options with constant values."
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
   (should (equal args '("/some/path")))))

(ert-deftest esh-opt-test/eval-using-options-user-specified ()
  "Test `eshell-eval-using-options' with options with user-specified values."
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
   (should (equal args '("/some/path")))))

(ert-deftest esh-opt-test/eval-using-options-short-single-token ()
  "Test `eshell-eval-using-options' with multiple short options in one token."
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
   (should (equal args '("/some/path")))))

(ert-deftest esh-opt-test/eval-using-options-stdin ()
  "Test that \"-\" is a positional arg in `eshell-eval-using-options'."
  (eshell-eval-using-options
   "cat" '("-")
   '((?A "show-all" nil show-all
         "show all characters"))
   (should (eq show-all nil))
   (should (equal args '("-"))))
  (eshell-eval-using-options
   "cat" '("-A" "-")
   '((?A "show-all" nil show-all
         "show all characters"))
   (should (eq show-all t))
   (should (equal args '("-"))))
  (eshell-eval-using-options
   "cat" '("-" "-A")
   '((?A "show-all" nil show-all
         "show all characters"))
   (should (eq show-all t))
   (should (equal args '("-")))))

(ert-deftest esh-opt-test/eval-using-options-terminate-options ()
  "Test that \"--\" terminates options in `eshell-eval-using-options'."
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
   (should (equal args '("--all")))))

(ert-deftest esh-opt-test/eval-using-options-parse-leading-options-only ()
  "Test :parse-leading-options-only in `eshell-eval-using-options'."
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

(ert-deftest esh-opt-test/eval-using-options-unrecognized ()
  "Test `eshell-eval-using-options' with unrecognized options."
  (should-error
   (eshell-eval-using-options
    "ls" '("-u" "/some/path")
    '((?a "all" nil _show-all
          "do not ignore entries starting with ."))))
  (should-error
   (eshell-eval-using-options
    "ls" '("-au" "/some/path")
    '((?a "all" nil _show-all
          "do not ignore entries starting with ."))))
  (should-error
   (eshell-eval-using-options
    "ls" '("--unrecognized" "/some/path")
    '((?a "all" nil _show-all
          "do not ignore entries starting with .")))))

(ert-deftest esh-opt-test/eval-using-options-external ()
  "Test :external in `eshell-eval-using-options'."
  (cl-letf (((symbol-function 'eshell-search-path) #'identity)
            ((symbol-function 'eshell-external-command) #'list))
    (should
     (equal (catch 'eshell-external
              (eshell-eval-using-options
               "ls" '("/some/path" "-u")
               '((?a "all" nil _show-all
                     "do not ignore entries starting with .")
                 :external "ls")))
            '("ls" ("/some/path" "-u"))))
    (should
     (equal (catch 'eshell-external
              (eshell-eval-using-options
               "ls" '("/some/path2" "-u")
               '((?a "all" nil _show-all
                     "do not ignore entries starting with .")
                 :preserve-args
                 :external "ls")))
            '("ls" ("/some/path2" "-u"))))))

(provide 'esh-opt-tests)

;;; esh-opt-tests.el ends here
