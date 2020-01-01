;;; rcirc-tests.el --- Tests for rcirc -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'rcirc)
(require 'cl-lib)

(defun rcirc-tests--parse-server-response (cmd text)
  (cl-letf* ((received-args nil)
             ((symbol-function (intern (concat "rcirc-handler-" cmd)))
              (lambda (_process sender args text)
                (setq received-args (list sender cmd args text))))
             (rcirc-receive-message-functions nil)
             (rcirc-trap-errors-flag nil))
    (rcirc-process-server-response nil text)
    received-args))

(defmacro rcirc-tests--server-response-parse-should-be
    (text expected-sender expected-cmd expected-args)
  (declare (debug t))
  (macroexp-let2* nil ((cmd expected-cmd))
    `(should (equal (rcirc-tests--parse-server-response ,cmd ,text)
                    (list ,expected-sender ,cmd ,expected-args ,text)))))

(ert-deftest rcirc-process-server-response ()
  (rcirc-tests--server-response-parse-should-be
   "MODE #cchan +kl a:b :999"
   nil "MODE" '("#cchan" "+kl" "a:b" "999"))
  (rcirc-tests--server-response-parse-should-be
   "MODE #cchan +kl a:b 999"
   nil "MODE" '("#cchan" "+kl" "a:b" "999"))
  (rcirc-tests--server-response-parse-should-be
    "MODE #cchan +kl :a:b"
    nil "MODE" '("#cchan" "+kl" "a:b")))

;;; rcirc-tests.el ends here
