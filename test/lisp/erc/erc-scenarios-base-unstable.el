;;; erc-scenarios-base-unstable.el --- base unstable scenarios -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join) (require 'warnings))

;; Not unstable, but stashed here for now

(ert-deftest erc-scenarios-aux-unix-socket ()
  :tags '(:expensive-test)
  (skip-unless (featurep 'make-network-process '(:family local)))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-server-flood-penalty 0.1)
       (sock (expand-file-name "erc-d.sock" temporary-file-directory))
       (erc-scenarios-common-extra-teardown (lambda () (delete-file sock)))
       (erc-server-connect-function
        (lambda (n b _ p &rest r)
          (apply #'make-network-process
                 `(:name ,n :buffer ,b :service ,p :family local ,@r))))
       (dumb-server (erc-d-run nil sock 'auto))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "fake"
                                       :port sock
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "fake:%s" sock)))))

    (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "foonet"))
      (erc-d-t-search-for 10 "Your new nickname is dummy"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#foo"))
        (erc-d-t-search-for 10 "dummy")
        (erc-d-t-search-for 10 "On Thursday")))))

;; See `erc-networks--rename-server-buffer'.  A perceived loss in
;; network connectivity turns out to be a false alarm, but the bouncer
;; has already accepted the second connection

(defun erc-scenarios--base-aborted-reconnect ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (erc-d-t-cleanup-sleep-secs 1)
       (dumb-server (erc-d-run "localhost" t 'aborted 'aborted-dupe))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (erc-d-t-wait-for 10 (get-buffer "FooNet"))
      (should-not (erc-scenarios-common-buflist "127.0.0.1"))
      (with-current-buffer erc-server-buffer-foo
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Channel buffer #chan alive and well")
      (with-current-buffer (erc-d-t-wait-for 4 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "welcome")))

    (ert-info ("Connect to foonet again")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (let ((inhibit-message noninteractive))
        (with-current-buffer erc-server-buffer-foo
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-wait-for 5 (not (erc-server-process-alive)))
          (erc-d-t-search-for 10 "FooNet still connected"))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (should (equal (list (get-buffer "FooNet"))
                     (erc-scenarios-common-buflist "FooNet")))
      (should (equal (list (get-buffer (format "127.0.0.1:%d" port)))
                     (erc-scenarios-common-buflist "127.0.0.1"))))

    (ert-info ("Channel buffer #chan still going")
      (with-current-buffer "#chan"
        (erc-d-t-search-for 10 "and be prosperous")))))

(ert-deftest erc-scenarios-base-aborted-reconnect ()
  :tags '(:unstable)
  (let ((tries 3)
        (timeout 1)
        failed)
    (while (condition-case _err
               (progn
                 (erc-scenarios--base-aborted-reconnect)
                 nil)
             (ert-test-failed
              (message "Test %S failed; %s attempt(s) remaining."
                       (ert-test-name (ert-running-test))
                       tries)
              (sleep-for (cl-incf timeout))
              (not (setq failed (zerop (cl-decf tries)))))))
    (should-not failed)))

;; The `erc-networks' library has slowly become a hard dependency of
;; the interactive client since its incorporation in 2006.  But its
;; module, which was added in ERC 5.3 (2008) and thereafter loaded by
;; default, only became quasi-required in ERC 5.5 (2022).  Despite
;; this, a basic connection should still always succeed, at least long
;; enough to warn users that their setup is abnormal.  Of course,
;; third-party code intentionally omitting the module will have to
;; override various erc-server-*-functions to avoid operating in a
;; degraded state, which has likely been the case for a while.

(ert-deftest erc-scenarios-networks-no-module ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "networks/no-module")
       (erc-server-flood-penalty 0.1)
       (erc-networks-mode-orig erc-networks-mode)
       (dumb-server (erc-d-run "localhost" t 'basic))
       (port (process-contact dumb-server :service))
       (erc-modules (remq 'networks erc-modules))
       (warning-suppress-log-types '((erc)))
       (expect (erc-d-t-make-expecter)))

    (erc-networks-mode -1)
    (ert-info ("Connect and retain dialed name")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "Required module `networks' not loaded")
        (funcall expect 10 "This server is in debug mode")
        ;; Buffer not named after network
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Join #chan, change nick, query op")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 20 "Even at thy teat thou")
        (erc-cmd-NICK "dummy")
        (funcall expect 10 "Your new nickname is dummy")
        (erc-scenarios-common-say "/msg alice hi")))

    (ert-info ("Switch to query and quit")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "alice"))
        (funcall expect 20 "bye"))

      (with-current-buffer (format "127.0.0.1:%d" port)
        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))
    (when erc-networks-mode-orig
      (erc-networks-mode +1))))

;;; erc-scenarios-base-unstable.el ends here
