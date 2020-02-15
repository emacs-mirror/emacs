;;; tests/esh-opt-tests.el --- esh-opt test suite

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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
   (equal '("world" "emerge")
          (eshell--process-args
           "sudo"
           '("-u" "root" "emerge" "-uDN" "world")
           '((?u "user" t user "execute a command as another USER"))))))

(ert-deftest test-eshell-eval-using-options ()
  "Tests for `eshell-eval-using-options'."
  (eshell-eval-using-options
   "sudo" '("-u" "root" "whoami")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (equal user "root")))
  (eshell-eval-using-options
   "sudo" '("--user" "root" "whoami")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (equal user "root")))

  (eshell-eval-using-options
   "sudo" '("emerge" "-uDN" "world")
   '((?u "user" t user "execute a command as another USER"))
   (should (equal user "world")))
  (eshell-eval-using-options
   "sudo" '("emerge" "-uDN" "world")
   '((?u "user" t user "execute a command as another USER")
     :parse-leading-options-only)
   (should (eq user nil)))

  (eshell-eval-using-options
   "ls" '("-I" "*.txt" "/dev/null")
   '((?I "ignore" t ignore-pattern
	 "do not list implied entries matching pattern"))
   (should (equal ignore-pattern "*.txt")))

  (eshell-eval-using-options
   "ls" '("-l" "/dev/null")
   '((?l nil long-listing listing-style
	 "use a long listing format"))
   (should (eql listing-style 'long-listing)))
  (eshell-eval-using-options
   "ls" '("/dev/null")
   '((?l nil long-listing listing-style
	 "use a long listing format"))
   (should (eq listing-style nil)))

  (eshell-eval-using-options
   "ls" '("/dev/null" "-h")
   '((?h "human-readable" 1024 human-readable
	 "print sizes in human readable format"))
   (should (eql human-readable 1024)))
  (eshell-eval-using-options
   "ls" '("/dev/null" "--human-readable")
   '((?h "human-readable" 1024 human-readable
	 "print sizes in human readable format"))
   (should (eql human-readable 1024)))
  (eshell-eval-using-options
   "ls" '("/dev/null")
   '((?h "human-readable" 1024 human-readable
	 "print sizes in human readable format"))
   (should (eq human-readable nil))))

(provide 'esh-opt-tests)

;;; esh-opt-tests.el ends here
