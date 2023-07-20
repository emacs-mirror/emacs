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

;; These first couple `erc-auto-reconnect-display' tests used to live
;; in erc-scenarios-base-reconnect but have since been renamed.

(defun erc-scenarios-base-buffer-display--reconnect-common
    (assert-server assert-chan assert-rest)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'options 'options-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-auto-reconnect t)
       erc-autojoin-channels-alist)

    (should (memq 'autojoin erc-modules))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall assert-server expect)
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 10 "debug mode")))

    (ert-info ("Wait for some output in channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall assert-chan expect)
        (funcall expect 10 "welcome")))

    (ert-info ("Server buffer shows connection failed")
      (with-current-buffer "FooNet"
        (funcall expect 10 "Connection failed!  Re-establishing")))

    (should (equal erc-autojoin-channels-alist '((FooNet "#chan"))))
    (delete-other-windows)
    (pop-to-buffer-same-window "*Messages*")

    (ert-info ("Wait for auto reconnect")
      (with-current-buffer "FooNet" (funcall expect 10 "still in debug mode")))

    (funcall assert-rest expect)

    (ert-info ("Wait for activity to recommence in both channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "forest of Arden"))
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
        (funcall expect 10 "her elves come here anon")))))

(ert-deftest erc-scenarios-base-buffer-display--defwin-recbury-intbuf ()
  :tags '(:expensive-test)
  (should (eq erc-buffer-display 'bury))
  (should (eq erc-interactive-display 'window))
  (should-not erc-auto-reconnect-display)

  (let ((erc-buffer-display 'window)
        (erc-interactive-display 'buffer)
        (erc-auto-reconnect-display 'bury))

    (erc-scenarios-base-buffer-display--reconnect-common

     (lambda (_)
       (should (eq (window-buffer) (current-buffer)))
       (should-not (frame-root-window-p (selected-window))))

     (lambda (_)
       (should (eq (window-buffer) (current-buffer)))
       (should (equal (get-buffer "FooNet") (window-buffer (next-window)))))

     (lambda (_)
       (with-current-buffer "FooNet"
         (should (eq (window-buffer) (messages-buffer)))
         (should (frame-root-window-p (selected-window))))

       ;; A manual /JOIN command tells ERC we're done auto-reconnecting
       (with-current-buffer "FooNet" (erc-scenarios-common-say "/JOIN #spam"))

       (ert-info ("#spam ignores `erc-auto-reconnect-display'")
         ;; Uses `erc-interactive-display' instead.
         (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
           (should (eq (window-buffer) (get-buffer "#spam")))
           ;; Option `buffer' replaces entire window (no split)
           (erc-d-t-wait-for 5 (frame-root-window-p (selected-window)))))))))

(ert-deftest erc-scenarios-base-buffer-display--defwino-recbury-intbuf ()
  :tags '(:expensive-test)
  (should (eq erc-buffer-display 'bury))
  (should (eq erc-interactive-display 'window))
  (should-not erc-auto-reconnect-display)

  (let ((erc-buffer-display 'window-noselect)
        (erc-auto-reconnect-display 'bury)
        (erc-interactive-display 'buffer))
    (erc-scenarios-base-buffer-display--reconnect-common

     (lambda (_)
       ;; Selected window shows some non-ERC buffer.  New server
       ;; buffer appears in another window (other side of split).
       (should-not (frame-root-window-p (selected-window)))
       (should-not (eq (window-buffer) (current-buffer)))
       (with-current-buffer (window-buffer)
         (should-not (derived-mode-p 'erc-mode)))
       (should (eq (current-buffer) (window-buffer (next-window)))))

     (lambda (_)
       (should-not (frame-root-window-p (selected-window)))
       ;; Current split likely shows scratch.
       (with-current-buffer (window-buffer)
         (should-not (derived-mode-p 'erc-mode)))
       (should (eq (current-buffer) (window-buffer (next-window)))))

     (lambda (_)
       (with-current-buffer "FooNet"
         (should (eq (window-buffer) (messages-buffer)))
         (should (frame-root-window-p (selected-window))))

       ;; A non-interactive JOIN command doesn't signal that we're
       ;; done auto-reconnecting, and `erc-interactive-display' is
       ;; ignored, so `erc-buffer-display' is again in charge (here,
       ;; that means `window-noselect').
       (ert-info ("Join chan noninteractively and open a /QUERY")
         (with-current-buffer "FooNet"
           (erc-cmd-JOIN "#spam")
           ;; However this will reset the option.
           (erc-scenarios-common-say "/QUERY bob")
           (should (eq (window-buffer) (get-buffer "bob")))
           (should (frame-root-window-p (selected-window)))))

       (ert-info ("Newly joined chan ignores `erc-auto-reconnect-display'")
         (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
           (should (eq (window-buffer) (get-buffer "bob")))
           (should-not (frame-root-window-p (selected-window)))
           (should (eq (current-buffer) (window-buffer (next-window))))))))))

(ert-deftest erc-scenarios-base-buffer-display--count-reset-timeout ()
  :tags '(:expensive-test)
  (should (eq erc-buffer-display 'bury))
  (should (eq erc-interactive-display 'window))
  (should (eq erc-auto-reconnect-display-timeout 10))
  (should-not erc-auto-reconnect-display)

  (let ((erc-buffer-display 'window-noselect)
        (erc-auto-reconnect-display 'bury)
        (erc-interactive-display 'buffer)
        (erc-auto-reconnect-display-timeout 0.5))
    (erc-scenarios-base-buffer-display--reconnect-common
     #'ignore #'ignore ; These two are identical to the previous test.

     (lambda (_)
       (with-current-buffer "FooNet"
         (should erc--server-reconnect-display-timer)
         (should (eq (window-buffer) (messages-buffer)))
         (should (frame-root-window-p (selected-window))))

       ;; A non-interactive JOIN command doesn't signal that we're
       ;; done auto-reconnecting
       (ert-info ("Join chan noninteractively")
         (with-current-buffer "FooNet"
           (erc-d-t-wait-for 1 (null erc--server-reconnect-display-timer))
           (erc-cmd-JOIN "#spam")))

       (ert-info ("Newly joined chan ignores `erc-auto-reconnect-display'")
         (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
           (should (eq (window-buffer) (messages-buffer)))
           ;; If `erc-auto-reconnect-display-timeout' were left alone, this
           ;; would be (frame-root-window-p #<window 1 on *scratch*>).
           (should-not (frame-root-window-p (selected-window)))
           (should (eq (current-buffer) (window-buffer (next-window))))))))))

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
