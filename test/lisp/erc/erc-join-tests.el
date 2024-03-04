;;; erc-join-tests.el --- Tests for erc-join.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert-x)
(require 'erc-join)
(require 'erc-networks)

(ert-deftest erc-autojoin-channels--connect ()
  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))
  (should-not erc--autojoin-timer)

  (let (calls
        common
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (cl-letf (((symbol-function 'erc-server-send)
               (lambda (line) (push line calls))))

      (setq common
            (lambda ()
              (ert-with-test-buffer (:name "foonet")
                (erc-mode)
                (setq erc-server-process
                      (start-process "true" (current-buffer) "true")
                      erc-network 'FooNet
                      erc-session-server "irc.gnu.chat"
                      erc-server-current-nick "tester"
                      erc-networks--id (erc-networks--id-create nil)
                      erc-server-announced-name "foo.gnu.chat")
                (set-process-query-on-exit-flag erc-server-process nil)
                (erc-autojoin-channels erc-server-announced-name
                                       "tester")
                (should-not erc--autojoin-timer))))

      (ert-info ("Join immediately on connect; server")
        (let ((erc-autojoin-channels-alist '(("\\.gnu\\.chat\\'" "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan")))

      (ert-info ("Join immediately on connect; network")
        (let ((erc-autojoin-channels-alist '((FooNet "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan")))

      (ert-info ("Do nothing; server")
        (let ((erc-autojoin-channels-alist '(("bar\\.gnu\\.chat" "#chan"))))
          (funcall common))
        (should-not calls))

      (ert-info ("Do nothing; network")
        (let ((erc-autojoin-channels-alist '((BarNet "#chan"))))
          (funcall common))
        (should-not calls)))))

(ert-deftest erc-autojoin-channels--delay ()
  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))
  (should-not erc--autojoin-timer)

  (let (calls
        common
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook
        (erc-autojoin-timing 'ident)
        (erc-autojoin-delay 0.05))

    (cl-letf (((symbol-function 'erc-server-send)
               (lambda (line) (push line calls)))
              ((symbol-function 'erc-autojoin-after-ident)
               (lambda (&rest _r) (error "I ran but shouldn't have"))))

      (setq common
            (lambda ()
              (ert-with-test-buffer (:name "foonet")
                (erc-mode)
                (setq erc-server-process
                      (start-process "true" (current-buffer) "true")
                      erc-network 'FooNet
                      erc-session-server "irc.gnu.chat"
                      erc-server-current-nick "tester"
                      erc-networks--id (erc-networks--id-create nil)
                      erc-server-announced-name "foo.gnu.chat")
                (set-process-query-on-exit-flag erc-server-process nil)
                (should-not erc--autojoin-timer)
                (erc-autojoin-channels erc-server-announced-name "tester")
                (should erc--autojoin-timer)
                (should-not calls)
                (sleep-for 0.1))))

      (ert-info ("Deferred on connect; server")
        (let ((erc-autojoin-channels-alist '(("\\.gnu\\.chat\\'" "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan")))

      (ert-info ("Deferred on connect; network")
        (let ((erc-autojoin-channels-alist '((FooNet "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan")))

      (ert-info ("Do nothing; server")
        (let ((erc-autojoin-channels-alist '(("bar\\.gnu\\.chat" "#chan"))))
          (funcall common))
        (should-not calls)))))

(ert-deftest erc-autojoin-channels--ident ()
  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))
  (should-not erc--autojoin-timer)

  (let (calls
        common
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook
        (erc-autojoin-timing 'ident))

    (cl-letf (((symbol-function 'erc-server-send)
               (lambda (line) (push line calls))))

      (setq common
            (lambda ()
              (ert-with-test-buffer (:name "foonet")
                (erc-mode)
                (setq erc-server-process
                      (start-process "true" (current-buffer) "true")
                      erc-network 'FooNet
                      erc-server-current-nick "tester"
                      erc-networks--id (erc-networks--id-create nil)
                      erc-server-announced-name "foo.gnu.chat")
                (set-process-query-on-exit-flag erc-server-process nil)
                (erc-autojoin-after-ident 'FooNet "tester")
                (should-not erc--autojoin-timer))))

      (ert-info ("Join on NickServ hook; server")
        (let ((erc-autojoin-channels-alist '(("\\.gnu\\.chat\\'" "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan")))

      (ert-info ("Join on NickServ hook; network")
        (let ((erc-autojoin-channels-alist '((FooNet "#chan"))))
          (funcall common))
        (should (equal (pop calls) "JOIN #chan"))))))

(defun erc-join-tests--autojoin-add--common (setup &optional fwd)
  (let (calls
        erc-autojoin-channels-alist
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (cl-letf (((symbol-function 'erc-handle-parsed-server-response)
               (lambda (_p m) (push m calls))))

      (ert-with-test-buffer (:name "foonet")
        (erc-mode)
        (setq erc-server-process
              (start-process "true" (current-buffer) "true")
              erc-server-current-nick "tester"
              erc--isupport-params (make-hash-table)
              erc-server-announced-name "foo.gnu.chat")
        (puthash 'CHANTYPES '("&#") erc--isupport-params)
        (funcall setup)
        (set-process-query-on-exit-flag erc-server-process nil)
        (should-not calls)

        (ert-info ("Add #chan")
          (erc-parse-server-response erc-server-process
                                     (concat ":tester!~i@c.u JOIN #chan"
                                             (and fwd " * :Tes Ter")))
          (should calls)
          (erc-autojoin-add erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist '((FooNet "#chan")))))

        (ert-info ("More recently joined chans are prepended")
          (erc-parse-server-response
           erc-server-process ; with account username
           (concat ":tester!~i@c.u JOIN #spam" (and fwd " tester :Tes Ter")))
          (should calls)
          (erc-autojoin-add erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '((FooNet "#spam" "#chan")))))

        (ert-info ("Duplicates skipped")
          (erc-parse-server-response erc-server-process
                                     (concat ":tester!~i@c.u JOIN #chan"
                                             (and fwd " * :Tes Ter")))
          (should calls)
          (erc-autojoin-add erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '((FooNet "#spam" "#chan")))))

        (ert-info ("Server used for local channel")
          (erc-parse-server-response erc-server-process
                                     (concat ":tester!~i@c.u JOIN &local"
                                             (and fwd " * :Tes Ter")))
          (should calls)
          (erc-autojoin-add erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '(("foo\\.gnu\\.chat" "&local")
                           (FooNet "#spam" "#chan")))))))))

(ert-deftest erc-autojoin-add--network ()
  (erc-join-tests--autojoin-add--common
   (lambda () (setq erc-network 'FooNet
                    erc-networks--id (erc-networks--id-create nil)))))

(ert-deftest erc-autojoin-add--network-extended-syntax ()
  (erc-join-tests--autojoin-add--common
   (lambda () (setq erc-network 'FooNet
                    erc-networks--id (erc-networks--id-create nil)))
   'forward-compatible))

(ert-deftest erc-autojoin-add--network-id ()
  (erc-join-tests--autojoin-add--common
   (lambda () (setq erc-network 'invalid
                    erc-networks--id (erc-networks--id-create 'FooNet)))))

(ert-deftest erc-autojoin-add--server ()
  (let (calls
        erc-autojoin-channels-alist
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (cl-letf (((symbol-function 'erc-handle-parsed-server-response)
               (lambda (_p m) (push m calls))))

      (ert-info ("Network unavailable, announced name used")
        (setq erc-autojoin-channels-alist nil)
        (ert-with-test-buffer (:name "foonet")
          (erc-mode)
          (setq erc-server-process
                (start-process "true" (current-buffer) "true")
                erc-server-current-nick "tester"
                erc-server-announced-name "foo.gnu.chat"
                erc-networks--id (make-erc-networks--id)) ; assume too early
          (set-process-query-on-exit-flag erc-server-process nil)
          (should-not calls)
          (erc-parse-server-response erc-server-process
                                     ":tester!~u@q6ddatxcq6txy.irc JOIN #chan")
          (should calls)
          (erc-autojoin-add erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '(("gnu.chat" "#chan")))))))))

(defun erc-join-tests--autojoin-remove--common (setup)
  (let (calls
        erc-autojoin-channels-alist
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (cl-letf (((symbol-function 'erc-handle-parsed-server-response)
               (lambda (_p m) (push m calls))))

      (setq erc-autojoin-channels-alist ; mutated, so can't quote whole thing
            (list '(FooNet "#spam" "##chan")
                  '(BarNet "#bar" "##bar")
                  '("foo\\.gnu\\.chat" "&local")))

      (ert-with-test-buffer (:name "foonet")
        (erc-mode)
        (setq erc-server-process
              (start-process "true" (current-buffer) "true")
              erc-server-current-nick "tester"
              erc--isupport-params (make-hash-table)
              erc-server-announced-name "foo.gnu.chat")
        (puthash 'CHANTYPES '("&#") erc--isupport-params)
        (funcall setup)
        (set-process-query-on-exit-flag erc-server-process nil)
        (should-not calls)

        (ert-info ("Remove #chan")
          (erc-parse-server-response erc-server-process
                                     ":tester!~i@c.u PART ##chan")
          (should calls)
          (erc-autojoin-remove erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '((FooNet "#spam")
                           (BarNet "#bar" "##bar")
                           ("foo\\.gnu\\.chat" "&local")))))

        (ert-info ("Wrong network, nothing done")
          (erc-parse-server-response erc-server-process
                                     ":tester!~i@c.u PART #bar")
          (should calls)
          (erc-autojoin-remove erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '((FooNet "#spam")
                           (BarNet "#bar" "##bar")
                           ("foo\\.gnu\\.chat" "&local")))))

        (ert-info ("Local channel keyed by server found")
          (erc-parse-server-response erc-server-process
                                     ":tester!~i@c.u PART &local")
          (should calls)
          (erc-autojoin-remove erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '((FooNet "#spam") (BarNet "#bar" "##bar")))))))))

(ert-deftest erc-autojoin-remove--network ()
  (erc-join-tests--autojoin-remove--common
   (lambda () (setq erc-network 'FooNet
                    erc-networks--id (erc-networks--id-create nil)))))

(ert-deftest erc-autojoin-remove--network-id ()
  (erc-join-tests--autojoin-remove--common
   (lambda () (setq erc-network 'fake-a-roo
                    erc-networks--id (erc-networks--id-create 'FooNet)))))

(ert-deftest erc-autojoin-remove--server ()
  (let (calls
        erc-autojoin-channels-alist
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (cl-letf (((symbol-function 'erc-handle-parsed-server-response)
               (lambda (_p m) (push m calls))))

      (setq erc-autojoin-channels-alist (list '("gnu.chat" "#spam" "##chan")
                                              '("fsf.chat" "#bar" "##bar")))

      (ert-with-test-buffer (:name "foonet")
        (erc-mode)
        (setq erc-server-process
              (start-process "true" (current-buffer) "true")
              erc-server-current-nick "tester"
              erc-server-announced-name "foo.gnu.chat"
              ;; Assume special case without known network
              erc-networks--id (make-erc-networks--id))
        (set-process-query-on-exit-flag erc-server-process nil)
        (should-not calls)

        (ert-info ("Announced name matched, #chan removed")
          (erc-parse-server-response erc-server-process
                                     ":tester!~i@c.u PART ##chan")
          (should calls)
          (erc-autojoin-remove erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '(("gnu.chat" "#spam")
                           ("fsf.chat" "#bar" "##bar")))))

        (ert-info ("Wrong announced name, nothing done")
          (erc-parse-server-response erc-server-process
                                     ":tester!~i@c.u PART #bar")
          (should calls)
          (erc-autojoin-remove erc-server-process (pop calls))
          (should (equal erc-autojoin-channels-alist
                         '(("gnu.chat" "#spam")
                           ("fsf.chat" "#bar" "##bar")))))))))

;;; erc-join-tests.el ends here
