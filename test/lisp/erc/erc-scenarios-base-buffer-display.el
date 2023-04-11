;;; erc-scenarios-base-buffer-display.el --- Buffer display scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

(eval-when-compile (require 'erc-join))

;; These first couple `erc-reconnect-display' tests used to live in
;; erc-scenarios-base-reconnect but have since been renamed.

(defun erc-scenarios-base-buffer-display--reconnect-common (test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'options 'options-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-auto-reconnect t)
       erc-autojoin-channels-alist
       erc-server-buffer)

    (should (memq 'autojoin erc-modules))

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer (erc :server "127.0.0.1"
                                   :port port
                                   :nick "tester"
                                   :password "changeme"
                                   :full-name "tester"))
      (with-current-buffer erc-server-buffer
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 10 "debug mode")))

    (ert-info ("Wait for some output in channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "welcome")))

    (ert-info ("Server buffer shows connection failed")
      (with-current-buffer erc-server-buffer
        (funcall expect 10 "Connection failed!  Re-establishing")))

    (should (equal erc-autojoin-channels-alist '((FooNet "#chan"))))

    (funcall test)

    ;; A manual /JOIN command tells ERC we're done auto-reconnecting
    (with-current-buffer "FooNet" (erc-cmd-JOIN "#spam"))

    (erc-d-t-ensure-for 1 "Newly joined chan ignores `erc-reconnect-display'"
      (not (eq (window-buffer) (get-buffer "#spam"))))

    (ert-info ("Wait for auto reconnect")
      (with-current-buffer erc-server-buffer
        (funcall expect 10 "still in debug mode")))

    (ert-info ("Wait for activity to recommence in channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "forest of Arden"))
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
        (funcall expect 10 "her elves come here anon")))))

(ert-deftest erc-scenarios-base-reconnect-options--buffer ()
  :tags '(:expensive-test)
  (should (eq erc-join-buffer 'bury))
  (should-not erc-reconnect-display)

  ;; FooNet (the server buffer) is not switched to because it's
  ;; already current (but not shown) when `erc-open' is called.  See
  ;; related conditional guard towards the end of that function.

  (let ((erc-reconnect-display 'buffer))
    (erc-scenarios-base-buffer-display--reconnect-common
     (lambda ()
       (pop-to-buffer-same-window "*Messages*")

       (erc-d-t-ensure-for 1 "Server buffer not shown"
         (not (eq (window-buffer) (get-buffer "FooNet"))))

       (erc-d-t-wait-for 5 "Channel #chan shown when autojoined"
         (eq (window-buffer) (get-buffer "#chan")))))))

(ert-deftest erc-scenarios-base-reconnect-options--default ()
  :tags '(:expensive-test)
  (should (eq erc-join-buffer 'bury))
  (should-not erc-reconnect-display)

  (erc-scenarios-base-buffer-display--reconnect-common

   (lambda ()
     (pop-to-buffer-same-window "*Messages*")

     (erc-d-t-ensure-for 1 "Server buffer not shown"
       (not (eq (window-buffer) (get-buffer "FooNet"))))

     (erc-d-t-ensure-for 3 "Channel #chan not shown"
       (not (eq (window-buffer) (get-buffer "#chan"))))

     (should (eq (window-buffer) (messages-buffer))))))


;; This shows that the option `erc-interactive-display' overrides
;; `erc-join-buffer' during cold opens and interactive /JOINs.

(ert-deftest erc-scenarios-base-buffer-display--interactive-default ()
  :tags '(:expensive-test)
  (should (eq erc-join-buffer 'bury))
  (should (eq erc-interactive-display 'window))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/legacy")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (url (format "tester:changeme@127.0.0.1:%d\r\r" port))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-auto-reconnect t)
       (erc-user-full-name "tester"))

    (ert-info ("Connect to foonet")
      (with-current-buffer (let (inhibit-interaction)
                             (ert-simulate-keys url
                               (call-interactively #'erc)))
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))

        (erc-d-t-wait-for 10 "Server buffer shown"
          (eq (window-buffer) (current-buffer)))
        (funcall expect 10 "debug mode")
        (erc-scenarios-common-say "/JOIN #chan")))

    (ert-info ("Wait for output in #chan")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "welcome")
        (erc-d-t-ensure-for 3 "Channel #chan shown"
          (eq (window-buffer) (current-buffer)))
        (funcall expect 10 "be prosperous")))))

;;; erc-scenarios-base-buffer-display.el ends here
