;;; erc-scenarios-base-association.el --- base assoc scenarios -*- lexical-binding: t -*-

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

(declare-function erc-network-name "erc-networks")
(declare-function erc-network "erc-networks")
(declare-function erc-track-get-active-buffer "erc-track" (arg))
(defvar erc-autojoin-channels-alist)
(defvar erc-track-mode)
(defvar erc-network)

;; Two networks, same channel name, no confusion (no bouncer).  Some
;; of this draws from bug#47522 "foil-in-server-buf".  It shows that
;; disambiguation-related changes added for bug#48598 are not specific
;; to bouncers.

(defun erc-scenarios-common--base-association-multi-net (second-join)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/multi-net")
       (erc-server-flood-penalty 0.1)
       (dumb-server-foonet-buffer (get-buffer-create "*server-foonet*"))
       (dumb-server-barnet-buffer (get-buffer-create "*server-barnet*"))
       (dumb-server-foonet (erc-d-run "localhost" t "server-foonet" 'foonet))
       (dumb-server-barnet (erc-d-run "localhost" t "server-barnet" 'barnet))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet, join #chan")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-foonet :service)
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 2 (get-buffer "#chan"))

    (ert-info ("Connect to barnet, join #chan")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-barnet :service)
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (funcall expect 5 "debug mode")))

    (funcall second-join)

    (erc-d-t-wait-for 3 (get-buffer "#chan@barnet"))

    (erc-d-t-wait-for 2 "Buf #chan now #chan@foonet"
      (and (get-buffer "#chan@foonet") (not (get-buffer "#chan"))))

    (ert-info ("All #chan@foonet output consumed")
      (with-current-buffer "#chan@foonet"
        (funcall expect 3 "bob")
        (funcall expect 3 "was created on")
        (funcall expect 10 "prosperous")))

    (ert-info ("All #chan@barnet output consumed")
      (with-current-buffer "#chan@barnet"
        (funcall expect 3 "mike")
        (funcall expect 3 "was created on")
        (funcall expect 20 "ingenuous")))))

(ert-deftest erc-scenarios-base-association-multi-net--baseline ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-association-multi-net
   (lambda () (with-current-buffer "barnet" (erc-cmd-JOIN "#chan")))))

;; The /join command only targets the current buffer's process.  This
;; recasts scenario bug#48598 "ambiguous-join" (which was based on
;; bug#47522) to show that issuing superfluous /join commands
;; (apparently fairly common) is benign.

(ert-deftest erc-scenarios-base-association-multi-net--ambiguous-join ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-association-multi-net
   (lambda ()
     (ert-info ("Nonsensical JOIN attempts silently dropped.")
       (with-current-buffer "foonet" (erc-cmd-JOIN "#chan"))
       (sit-for 0.1)
       (with-current-buffer "#chan" (erc-cmd-JOIN "#chan"))
       (sit-for 0.1)
       (erc-d-t-wait-for 2 (get-buffer "#chan"))
       (erc-d-t-wait-for 1 "Only one #chan buffer exists"
         (should (equal (erc-scenarios-common-buflist "#chan")
                        (list (get-buffer "#chan")))))
       (with-current-buffer "*server-barnet*"
         (erc-d-t-absent-for 0.1 "JOIN"))
       (with-current-buffer "barnet" (erc-cmd-JOIN "#chan"))))))

;; Playback for same channel on two networks routed correctly.
;; Originally from Bug#48598: 28.0.50; buffer-naming collisions
;; involving bouncers in ERC.

(ert-deftest erc-scenarios-base-association-bouncer-history ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (erc-d-t-cleanup-sleep-secs 1)
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.5)
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"))
        (setq erc-server-process-foo erc-server-process)
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (erc-d-t-wait-for 5 (get-buffer "#chan"))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"))
        (setq erc-server-process-bar erc-server-process)
        (erc-d-t-wait-for 5 "Temporary name assigned"
          (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar)))

    (ert-info ("Networks correctly determined and adopted as buffer names")
      (with-current-buffer erc-server-buffer-foo
        (erc-d-t-wait-for 3 "network name foonet becomes buffer name"
          (and (eq (erc-network) 'foonet) (string= (buffer-name) "foonet"))))
      (with-current-buffer erc-server-buffer-bar
        (erc-d-t-wait-for 3 "network name barnet becomes buffer name"
          (and (eq (erc-network) 'barnet) (string= (buffer-name) "barnet")))))

    (erc-d-t-wait-for 5 (get-buffer "#chan@barnet"))

    (ert-info ("Two channel buffers created, original #chan renamed")
      (should (= 4 (length (erc-buffer-list))))
      (should (equal (list (get-buffer "#chan@barnet")
                           (get-buffer "#chan@foonet"))
                     (erc-scenarios-common-buflist "#chan"))))

    (ert-info ("#chan@foonet is exclusive, no cross-contamination")
      (with-current-buffer "#chan@foonet"
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))))

    (ert-info ("#chan@barnet is exclusive, no cross-contamination")
      (with-current-buffer "#chan@barnet"
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (should (eq erc-server-process erc-server-process-bar))))

    (ert-info ("All output sent")
      (with-current-buffer "#chan@foonet"
        (erc-d-t-search-for 10 "please your lordship"))
      (with-current-buffer "#chan@barnet"
        (erc-d-t-search-for 10 "I'll bid adieu")))))

;; Some modules may need to perform housekeeping when a newly
;; connected server buffer is deemed a duplicate after its persistent
;; network context is discovered on MOTD end.  One such module is
;; `track', which needs to rid its list of modified channels of the
;; buffer being killed.  Without this, a user may encounter an
;; "Attempt to display deleted buffer" error when they try switching
;; to it.

(ert-deftest erc-scenarios-networks-merge-server-track ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "networks/merge-server")
       (dumb-server (erc-d-run "localhost" t 'track 'track))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (should erc-track-mode)
        (funcall expect 5 "changed mode for tester")
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Join channel and quit")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 5 "The hour that fools should ask")
        (erc-cmd-QUIT ""))
      (with-current-buffer "FooNet"
        (funcall expect 5 "finished")))

    (ert-info ("Reconnect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "changed mode for tester")))

    (with-current-buffer "#chan"
      (funcall expect 5 "The hour that fools should ask")
      ;; Simulate the old `erc-track-switch-buffer'
      (switch-to-buffer (erc-track-get-active-buffer 1))
      (erc-d-t-wait-for 10 (eq (get-buffer "FooNet") (current-buffer)))
      (erc-cmd-QUIT ""))))

;;; erc-scenarios-base-association.el ends here
