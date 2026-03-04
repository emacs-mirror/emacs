;;; erc-scenarios-base-buffer-display.el --- Buffer display scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
;; in erc-scenarios-base-reconnect but have since been renamed.  Note
;; that these are somewhat difficult to reason about because the user
;; joins a second channel after reconnecting, and the first is
;; controlled by `autojoin'.

(defun erc-scenarios-base-buffer-display--reconnect-common
    (assert-server assert-chan assert-rest)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'options 'options-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-reconnect-function #'erc-server-delayed-reconnect)
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
        (funcall expect 10 "welcome")
        (funcall expect 10 "welcome")))

    (ert-info ("Server buffer shows connection failed")
      (with-current-buffer "FooNet"
        (funcall expect 10 "Connection failed!  Re-establishing")))

    (should (equal erc-autojoin-channels-alist '((FooNet "#chan"))))
    (delete-other-windows)
    (pop-to-buffer-same-window "*Messages*")

    (ert-info ("Wait for auto reconnect")
      (with-current-buffer "FooNet" (funcall expect 10 "still in debug mode")))

    (ert-info ("Lone window still shows messages buffer")
      (should (eq (window-buffer) (messages-buffer)))
      (should (frame-root-window-p (selected-window))))

    (funcall assert-rest expect)

    (ert-info ("Wait for activity to recommence in both channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "forest of Arden"))
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
        (funcall expect 10 "her elves come here anon")))))

;; Interactively issuing a slash command resets the auto-reconnect
;; count, making ERC ignore the option `erc-auto-reconnect-display'
;; when next displaying a newly set up buffer.  In the case of a
;; /JOIN, the option `erc-interactive-display' takes precedence.
(ert-deftest erc-scenarios-base-buffer-display--defwin-recbury-intbuf ()
  :tags '(:expensive-test)
  (should (eq erc-buffer-display 'bury))
  (should (eq erc-interactive-display 'window))
  (should-not erc-auto-reconnect-display)

  (let ((erc-buffer-display 'window) ; defwin
        (erc-interactive-display 'buffer) ; intbuf
        (erc-auto-reconnect-display 'bury)) ; recbury

    (erc-scenarios-base-buffer-display--reconnect-common

     (lambda (_)
       (ert-info ("New server buffer appears in a selected split")
         (should (eq (window-buffer) (current-buffer)))
         (should-not (frame-root-window-p (selected-window)))))

     (lambda (_)
       (ert-info ("New channel buffer appears in other window")
         (should (eq (window-buffer) (current-buffer))) ; selected
         (should (equal (get-buffer "FooNet") (window-buffer (next-window))))))

     (lambda (expect)
       ;; If we /JOIN #spam now, we'll cancel the auto-reconnect
       ;; timer, and "#chan" may well pop up in a split before we can
       ;; verify that the lone window displays #spam (a race, IOW).
       (ert-info ("Autojoined channel #chan buried on JOIN")
         (with-current-buffer "#chan"
           (funcall expect 10 "You have joined channel #chan"))
         (should (frame-root-window-p (selected-window)))
         (should (eq (window-buffer) (messages-buffer))))

       (with-current-buffer "FooNet" (erc-scenarios-common-say "/JOIN #spam"))

       (ert-info ("A /JOIN ignores `erc-auto-reconnect-display'")
         (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
           (should (eq (window-buffer) (get-buffer "#spam")))
           ;; Option `erc-interactive-display' being `buffer' means
           ;; Emacs reuses the selected window (no split).
           (should (frame-root-window-p (selected-window)))))))))

(ert-deftest erc-scenarios-base-buffer-display--defwino-recbury-intbuf ()
  :tags '(:expensive-test)
  (should (eq erc-buffer-display 'bury))
  (should (eq erc-interactive-display 'window))
  (should-not erc-auto-reconnect-display)

  (let ((erc-buffer-display 'window-noselect) ; defwino
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
       ;; A JOIN command sent from lisp code is "non-interactive" and
       ;; doesn't reset the auto-reconnect count, so ERC treats the
       ;; response as possibly server-initiated or otherwise the
       ;; result of an autojoin and continues to favor
       ;; `erc-auto-reconnect-display'.
       (ert-info ("Join chan non-interactively and open a /QUERY")
         (with-current-buffer "FooNet"
           (erc-cmd-JOIN "#spam") ; "non-interactive" according to ERC
           (erc-scenarios-common-say "/QUERY bob") ; resets count
           (should (eq (window-buffer) (get-buffer "bob")))
           (should (frame-root-window-p (selected-window)))))

       ;; The /QUERY above resets the count, and `erc-buffer-display'
       ;; again decides how #spam is displayed.
       (ert-info ("Newly joined chan ignores `erc-auto-reconnect-display'")
         (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
           (should (eq (window-buffer) (get-buffer "bob")))
           (should-not (frame-root-window-p (selected-window))) ; noselect
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
         (erc-d-t-wait-for 1 erc--server-reconnect-display-timer))

       ;; A non-interactive JOIN command doesn't signal that we're
       ;; done auto-reconnecting.
       (ert-info ("Join channel #spam non-interactively")
         (with-current-buffer "FooNet"
           (erc-d-t-wait-for 1 (null erc--server-reconnect-display-timer))
           (erc-cmd-JOIN "#spam"))) ; not processed as a /JOIN

       (ert-info ("Option `erc-auto-reconnect-display' ignored w/o timer")
         (should (eq (window-buffer) (messages-buffer)))
         (erc-d-t-wait-for 10 (get-buffer "#spam"))
         ;; If `erc-auto-reconnect-display-timeout' were left alone,
         ;; this would be (frame-root-window-p #<window 1 on scratch*>).
         (should-not (frame-root-window-p (selected-window)))
         (should (eq (get-buffer "#spam") (window-buffer (next-window)))))))))

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
      (with-current-buffer (let ((inhibit-message noninteractive)
                                 (inhibit-interaction nil))
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
