;;; erc-join-tests.el --- Tests for erc-join.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

;; This merely asserts that `erc-autojoin-channels' sends JOINs right
;; away when the logical connection is established, so long as
;; `erc-autojoin-timing' is the default of `connect'.
(defun erc-join-tests--autojoin-channels-connect (test)
  (should (eq erc-autojoin-timing 'connect))

  (erc-tests-common-make-server-buf)

  (cl-letf* ((calls nil)
             (check (lambda () (prog1 calls (setq calls nil))))
             ((symbol-function 'erc-server-send)
              (lambda (line) (push line calls))))

    (erc-autojoin-channels erc-server-announced-name "tester")
    (funcall test check)
    (should-not erc--autojoin-timer))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-autojoin-channels/server ()
  (let ((erc-autojoin-channels-alist '(("\\.foonet\\.org\\'" "#chan"))))
    (erc-join-tests--autojoin-channels-connect
     (lambda (check)
       (should (equal erc-session-server "irc.foonet.org"))
       (should (equal (funcall check) '("JOIN #chan")))))))

(ert-deftest erc-autojoin-channels/network ()
  (let ((erc-autojoin-channels-alist '((foonet "#chan"))))
    (erc-join-tests--autojoin-channels-connect
     (lambda (check)
       (should (eq erc-network 'foonet))
       (should (equal (funcall check) '("JOIN #chan")))))))

(ert-deftest erc-autojoin-channels/nomatch ()
  (let ((erc-autojoin-channels-alist '(("fake\\.foonet\\.org\\'" "#chan")
                                       (barnet "#chan"))))
    (erc-join-tests--autojoin-channels-connect
     (lambda (check)
       (should (eq erc-network 'foonet))
       (should (equal erc-server-announced-name "west.foonet.org"))
       (should-not (funcall check))))))

;; This doesn't cover the entirety of `erc-autojoin-timing' being
;; `ident'.  It only simulates `erc-autojoin-channels-delayed' running
;; `erc-autojoin-delay' seconds after MOTD's end.  A JOIN can also be
;; triggered before or after that if `erc-nickserv-identified-hook' runs
;; on a NOTICE login confirmation.
(defun erc-join-tests--autojoin-channels-ident (test)

  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))

  (erc-tests-common-make-server-buf)

  (cl-letf* ((erc-autojoin-timing 'ident)
             (erc-autojoin-delay 0.0625)
             (calls nil)
             (check (lambda () (prog1 calls (setq calls nil))))
             ((symbol-function 'erc-server-send)
              (lambda (line) (push line calls)))
             ((symbol-function 'erc-autojoin-after-ident)
              (lambda (&rest _r) (should-not "run"))))

    (should-not erc--autojoin-timer)

    (erc-autojoin-channels erc-server-announced-name "tester")
    (should erc--autojoin-timer)

    ;; May run forever on Solaris 10 (bug#79017).
    (with-timeout (5 (ert-fail "Timeout exceeded"))
      (while erc--autojoin-timer
        (sleep-for 0.125)))

    (funcall test check)

    (should-not calls)
    (should-not erc--autojoin-timer))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-autojoin-channels-delayed/server ()
  (let ((erc-autojoin-channels-alist '(("\\.foonet\\.org\\'" "#chan"))))
    (erc-join-tests--autojoin-channels-ident
     (lambda (check)
       (should (equal erc-session-server "irc.foonet.org"))
       (should (equal (funcall check) '("JOIN #chan")))))))

(ert-deftest erc-autojoin-channels-delayed/network ()
  (let ((erc-autojoin-channels-alist '((foonet "#chan"))))
    (erc-join-tests--autojoin-channels-ident
     (lambda (check)
       (should (eq erc-network 'foonet))
       (should (equal (funcall check) '("JOIN #chan")))))))

(ert-deftest erc-autojoin-channels-delayed/nomatch ()
  ;; Actual announced name is west.foonet.org.
  (let ((erc-autojoin-channels-alist '(("east\\.foonet\\.org" "#chan")
                                       (barnet "#chan"))))
    (erc-join-tests--autojoin-channels-ident
     (lambda (check)
       (should (equal erc-server-announced-name "west.foonet.org"))
       (should-not (funcall check))))))

;; This asserts that a JOIN is sent on match by the
;; `erc-nickserv-identified-hook' member managed by this module if
;; `erc-autojoin-timing' is set to `ident'.
(defun erc-join-tests--autojoin-after-ident (test)
  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))

  (erc-tests-common-make-server-buf)

  (cl-letf* ((erc-autojoin-timing 'ident)
             (calls nil)
             (check (lambda () (prog1 calls (setq calls nil))))
             ((symbol-function 'erc-server-send)
              (lambda (line) (push line calls))))

    (erc-autojoin-after-ident 'foonet "tester")
    (funcall test check)
    (should-not calls)
    (should-not erc--autojoin-timer))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-autojoin-after-ident/server ()
  (let ((erc-autojoin-channels-alist '(("\\.foonet\\.org\\'" "#chan"))))
    (erc-join-tests--autojoin-after-ident
     (lambda (check)
       (should (equal erc-session-server "irc.foonet.org"))
       (should (equal (funcall check) '("JOIN #chan")))))))

(ert-deftest erc-autojoin-after-ident/network ()
  (let ((erc-autojoin-channels-alist '((foonet "#chan"))))
    (erc-join-tests--autojoin-after-ident
     (lambda (check)
       (should (eq erc-network 'foonet))
       (should (equal (funcall check) '("JOIN #chan")))))))

(defun erc-join-tests--autojoin-add (setup &optional fwd)

  (erc-tests-common-make-server-buf)

  (let ((erc-server-JOIN-functions #'erc-autojoin-add)
        (erc-autojoin-channels-alist nil))

    (puthash 'CHANTYPES '("&#") erc--isupport-params) ; for &local
    (funcall setup)

    (ert-info ("Add #chan")
      (erc-tests-common-simulate-line
       (concat ":tester!~i@c.u JOIN #chan" (and fwd " * :Tes Ter")))
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#chan")))))

    (ert-info ("Prepends joined chans")
      (erc-tests-common-simulate-line ;with account username
       (concat ":tester!~i@c.u JOIN #spam" (and fwd " tester :Tes Ter")))
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#spam" "#chan")))))

    (ert-info ("Duplicates skipped")
      (erc-tests-common-simulate-line
       (concat ":tester!~i@c.u JOIN #chan" (and fwd " * :Tes Ter")))
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#spam" "#chan")))))

    (ert-info ("Announced server used for local channel key")
      (erc-tests-common-simulate-line
       (concat ":tester!~i@c.u JOIN &local" (and fwd " * :Tes Ter")))
      (should (equal erc-autojoin-channels-alist
                     '(("west\\.foonet\\.org" "&local")
                       (foonet "#spam" "#chan"))))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-autojoin-add--network ()
  (erc-join-tests--autojoin-add #'ignore))

(ert-deftest erc-autojoin-add--network-extended-syntax ()
  (erc-join-tests--autojoin-add #'ignore 'forward-compatible))

(ert-deftest erc-autojoin-add--network-id ()
  (erc-join-tests--autojoin-add
   (lambda ()
     (setq erc-network 'invalid
           erc-networks--id (erc-networks--id-create 'foonet)))))

;; This shows the fallback behavior for adding alist entries keyed by a
;; domain name (as an unquoted regexp).  It runs if the network is not
;; known and the user did not provide an :id keyword to the entry-point
;; command.
(ert-deftest erc-autojoin-add--server ()

  (erc-tests-common-make-server-buf)

  (let ((erc-server-JOIN-functions #'erc-autojoin-add)
        (erc-autojoin-channels-alist nil))

    (setq erc-network nil
          ;; Override the ID so that it's automatically derived instead
          ;; of a "given" one provided by the user.
          erc-networks--id (erc-networks--id-create nil))

    (erc-tests-common-simulate-line
     ":tester!~u@q6ddatxcq6txy.irc JOIN #chan")

    (should (equal erc-autojoin-channels-alist
                   '(("foonet.org" "#chan")))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(defun erc-join-tests--autojoin-remove (setup)

  (erc-tests-common-make-server-buf)

  (let ((erc-server-PART-functions #'erc-autojoin-remove)
        (erc-autojoin-channels-alist
         (list (list 'foonet "#spam" "##chan")
               (list 'barnet "#bar")
               (list "west\\.foonet\\.org" "&local"))))

    (puthash 'CHANTYPES '("&#") erc--isupport-params)
    (funcall setup)

    (ert-info ("Remove #chan")
      (erc-tests-common-simulate-line ":tester!~i@c.u PART ##chan")
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#spam")
                       (barnet "#bar")
                       ("west\\.foonet\\.org" "&local")))))

    (ert-info ("Wrong network, nothing done")
      (erc-tests-common-simulate-line ":tester!~i@c.u PART #bar")
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#spam")
                       (barnet "#bar")
                       ("west\\.foonet\\.org" "&local")))))

    (ert-info ("Local channel keyed by server found")
      (erc-tests-common-simulate-line ":tester!~i@c.u PART &local")
      (should (equal erc-autojoin-channels-alist
                     '((foonet "#spam") (barnet "#bar"))))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-autojoin-remove--network ()
  (erc-join-tests--autojoin-remove #'ignore))

;; This asserts that a given ID has precedence over the network.
(ert-deftest erc-autojoin-remove--network-id ()
  (erc-join-tests--autojoin-remove
   (lambda ()
     (setq erc-network 'fake
           erc-networks--id (erc-networks--id-create "foonet")))))

;; This asserts that domain names are tried if the network is unknown
;; and an explicit ID was not provided on entry-point invocation.
(ert-deftest erc-autojoin-remove--server ()

  (erc-tests-common-make-server-buf)

  (let ((erc-server-PART-functions #'erc-autojoin-remove)
        (erc-autojoin-channels-alist
         (list (list "foonet.org" "#spam" "##chan")
               (list "fsf.chat" "#bar" "##bar"))))

    (setq erc-network nil
          erc-networks--id (erc-networks--id-create nil))

    (ert-info ("Announced name matched, #chan removed")
      (erc-tests-common-simulate-line ":tester!~i@c.u PART ##chan")
      (should (equal erc-autojoin-channels-alist
                     '(("foonet.org" "#spam")
                       ("fsf.chat" "#bar" "##bar")))))

    (ert-info ("Wrong announced name, nothing done")
      (erc-tests-common-simulate-line ":tester!~i@c.u PART #bar")
      (should (equal erc-autojoin-channels-alist
                     '(("foonet.org" "#spam")
                       ("fsf.chat" "#bar" "##bar"))))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

;;; erc-join-tests.el ends here
