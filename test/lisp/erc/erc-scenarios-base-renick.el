;;; erc-scenarios-base-renick.el --- Re-nicking scenarios -*- lexical-binding: t -*-

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

(eval-when-compile (require 'erc-join))

;; The server changes your nick just after registration.

(ert-deftest erc-scenarios-base-renick-self-auto ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'auto))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "foonet"))
      (erc-d-t-search-for 10 "Your new nickname is dummy"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (erc-d-t-search-for 10 "dummy")
        (erc-d-t-search-for 10 "On Thursday")))))

;; You change your nickname manually in a server buffer; a message is
;; printed in channel buffers.

(ert-deftest erc-scenarios-base-renick-self-manual ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'manual))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 3 (get-buffer "foonet"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (funcall expect 5 "tester")
        (funcall expect 5 "On Thursday")
        (erc-with-server-buffer (erc-cmd-NICK "dummy"))
        (funcall expect 5 "Your new nickname is dummy")
        (funcall expect 5 "<bob> dummy: Hi")
        ;; Regression in which changing a nick would trigger #foo@foonet
        (erc-d-t-ensure-for 0.4 (equal (buffer-name) "#foo"))))))

;; You connect to the same network with two different nicks.  You
;; manually change the first nick at some point, and buffer names are
;; updated correctly.

(ert-deftest erc-scenarios-base-renick-self-qualified ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (dumb-server (erc-d-run "localhost" t 'qual-tester 'qual-chester))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       erc-serv-buf-a erc-serv-buf-b)

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer
          (setq erc-serv-buf-a (erc :server "127.0.0.1"
                                    :port port
                                    :nick "tester"
                                    :password "changeme"
                                    :full-name "tester"))
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer
          (setq erc-serv-buf-b (erc :server "127.0.0.1"
                                    :port port
                                    :nick "chester"
                                    :password "changeme"
                                    :full-name "chester"))))

    (erc-d-t-wait-for 3 "Dialed Buflist is Empty"
      (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer  "foonet/tester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (with-current-buffer  "foonet/chester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/tester"))
    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/chester"))

    (ert-info ("Greets other nick in same channel")
      (with-current-buffer "#chan@foonet/tester"
        (funcall expect 5 "<bob> chester, welcome!")
        (erc-cmd-NICK "dummy")
        (funcall expect 5 "Your new nickname is dummy")
        (funcall expect 5 "find the forester")
        (erc-d-t-wait-for 5 (string= (buffer-name) "#chan@foonet/dummy"))))

    (ert-info ("Renick propagated throughout all buffers of process")
      (should-not (get-buffer "#chan@foonet/tester"))
      (should-not (get-buffer "foonet/tester"))
      (should (get-buffer "foonet/dummy")))))

;; When a channel user changes their nick, any query buffers for them
;; are updated.

(ert-deftest erc-scenarios-base-renick-queries-solo ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/queries")
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 20)
       (dumb-server (erc-d-run "localhost" t 'solo))
       (expect (erc-d-t-make-expecter))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 10 (get-buffer "foonet"))

    (ert-info ("Joined by bouncer to #foo, pal Lal is present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (funcall expect 10 "<bob> alice: On Thursday")
        (erc-scenarios-common-say "hi")))

    (ert-info ("Query buffer appears from Lal, who renicks")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "Lal"))
        (funcall expect 10 "<Lal> hello")
        (erc-scenarios-common-say "hi")
        (should-not (erc-get-channel-member "tester"))
        (funcall expect 10 "is now known as Linguo")
        ;; No duplicate message.
        (funcall expect -0.1 "is now known as Linguo")
        ;; No duplicate buffer.
        (erc-d-t-wait-for 1 (equal (buffer-name) "Linguo"))
        (should-not (get-buffer "Lal"))
        ;; Channel member has been updated
        (should-not (erc-get-channel-member "Lal"))
        (should-not (erc-get-server-user "Lal"))
        (should (erc-get-channel-member "Linguo"))
        (erc-scenarios-common-say "howdy Linguo")))

    (with-current-buffer "#foo"
      (funcall expect 10 "is now known as Linguo")
      (funcall expect -0.1 "is now known as Linguo")
      (funcall expect 10 "has left"))

    ;; User parting a common channel removes them from queries.
    (with-current-buffer "Linguo"
      (should-not (erc-get-channel-member "tester"))
      (erc-d-t-wait-for 10 (null (erc-get-channel-member "Linguo")))
      (should-not (erc-get-server-user "Linguo")))

    ;; Leaving the client's only channel doesn't remove its user data
    ;; from the server table (see below, after "get along ...").
    (with-current-buffer "#foo"
      (erc-scenarios-common-say "/part"))

    ;; Server and "channel" user are *not* (re)created upon receiving
    ;; a direct message for a user we already have an open query with
    ;; but with whom we no longer share a channel.
    (with-current-buffer "Linguo"
      (funcall expect 10 "get along")
      (should-not (erc-get-channel-member "Linguo"))
      (should-not (erc-get-channel-member "tester"))
      (should (erc-get-server-user "tester")))))

;; Someone you have a query with disconnects and reconnects under a
;; new nick (perhaps due to their client appending a backtick or
;; underscore).  They then engage you in another query before
;; renicking to their original nick.  Prior to 5.5, ERC would add a
;; uniquifying suffix of the form bob<2> to the new, post-renick
;; query.  ERC 5.6+ acts differently.  It mimics popular standalone
;; clients in reusing existing query buffers.
(ert-deftest erc-scenarios-base-renick-queries/reassume ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/queries")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'reassume))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan"))))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester")
        (funcall expect 10 "This server is in debug mode")))

    (ert-info ("User dummy opens a query with you")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy"))
        (funcall expect 10 "hi")))

    (ert-info ("User dummy quits, reconnects as user warwick")
      (with-current-buffer "#chan"
        (funcall expect 10 "has quit")
        (should-not (erc-get-channel-member "dummy"))
        (with-current-buffer "dummy"
          (should-not (erc-get-channel-member "dummy")))
        (funcall expect 10 "<bob> Alas! sir")
        (funcall expect 10 "<bob> warwick, welcome")
        (funcall expect 10 "<warwick> hola")))

    (ert-info ("User warwick queries you, creating a new buffer")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "warwick"))
        (should (get-buffer "dummy")) ; not reused
        (funcall expect 10 "<warwick> howdy")
        (funcall expect 10 "is now known as dummy")
        (should-not (erc-get-channel-member "warwick"))
        (should-not (erc-get-channel-member "dummy"))))

    (ert-info ("User warwick renicks as user dummy")
      (with-current-buffer "#chan"
        (funcall expect 10 "is now known as dummy")
        (should-not (erc-get-channel-member "warwick"))))

    (with-current-buffer "dummy"
      (should-not (get-buffer "dummy<2>"))
      (funcall expect 10 "has quit" (point-min))
      (funcall expect -0.1 "merging buffer")
      (funcall expect 10 "is now known as dummy")
      (should (erc-get-channel-member "dummy"))
      (funcall expect 10 "<dummy> hey"))

    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Than those that"))))

;; This test asserts behavior for the other side of the conversation
;; described by `erc-scenarios-base-renick-queries/reassume' above.
;; After speaking with someone in a query, you disconnect and
;; reconnect under a new nick.  You then open a new query with the
;; same person before changing your nick back to the previous one.
;; The buffers for the two session should then be merged with the help
;; of `erc-networks--transplant-target-buffer-function' and
;; `erc-networks--copy-server-buffer-functions'.
(ert-deftest erc-scenarios-base-renick-self/merge-query ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'merge-query-a 'merge-query-b))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan"))))

    (ert-info ("Connect to foonet as tester")
      (with-current-buffer (erc :server "127.0.0.1" :port port :nick "tester")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (funcall expect 10 "<alice> bob: Speak to the people")
      (erc-scenarios-common-say "/query observer"))

    (with-current-buffer "observer"
      (erc-scenarios-common-say "hi")
      (funcall expect 10 "<observer> hi?"))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (erc-scenarios-common-say "/quit"))

    (with-current-buffer "foonet"
      (funcall expect 10 "*** ERC finished ***"))

    (ert-info ("Reconnect to foonet as dummy")
      (with-current-buffer (erc :server "127.0.0.1" :port port :nick "dummy")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer
        (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/dummy"))
      ;; Uniquification has been performed.
      (should-not (get-buffer "#chan"))
      (should (get-buffer "#chan@foonet/tester"))
      (should-not (get-buffer "foonet"))
      (should (get-buffer "foonet/tester"))
      (should (get-buffer "foonet/dummy"))
      (funcall expect 10 "<alice> bob: Pray you")
      (erc-scenarios-common-say "/query observer"))

    (with-current-buffer "observer@foonet/dummy"
      (should-not (get-buffer "observer"))
      (should (get-buffer "observer@foonet/tester"))
      (erc-scenarios-common-say "hola")
      (funcall expect 10 "<observer> whodis?"))

    (with-current-buffer
        (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/dummy"))
      (erc-scenarios-common-say "/nick tester"))

    ;; All buffers have been merged.
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "observer"))
      (should-not (get-buffer "observer@foonet/dummy"))
      (should-not (get-buffer "observer@foonet/tester"))
      ;; Goto last message from previous session.  Notice that the
      ;; quit message appears in all buffers, including queries.
      (funcall expect 10 "has quit" (point-min))
      (funcall expect -0.01 "\n\n[") ; duplicate date stamp removed
      (funcall expect 1 (concat "*** Grafting buffer `observer@foonet/dummy'"
                                " onto `observer@foonet/tester'"))
      (funcall expect 1 "<dummy> hola")
      (funcall expect 1 "<observer> whodis?")
      ;; The nickname change is announced in the query as well so that
      ;; the nature of the merge is clear.
      (funcall expect 1 "*** Your new nickname is tester"))

    (with-current-buffer "foonet"
      (should-not (get-buffer "foonet/dummy"))
      (should-not (get-buffer "foonet/tester"))
      ;; Goto last assertion.
      (funcall expect 10 "*** ERC finished ***" (point-min))
      (funcall expect -0.01 "\n\n[") ; duplicate date stamp removed
      (funcall expect 5 "Grafting buffer `foonet/dummy' onto `foonet/tester'"))

    (with-current-buffer "#chan"
      (should-not (get-buffer "#chan@foonet/dummy"))
      (should-not (get-buffer "#chan@foonet/tester"))
      (funcall expect 10 "has quit" (point-min))
      (funcall expect -0.01 "\n\n[") ; duplicate date stamp removed
      (funcall expect 1 (concat "*** Grafting buffer `#chan@foonet/dummy'"
                                " onto `#chan@foonet/tester'"))
      (funcall expect 1 "You have joined channel #chan")
      (funcall expect 1 "<bob> alice: Have here bereft")
      (funcall expect 1 "*** Your new nickname is tester"))))

;; You share a channel and a query buffer with a user on two different
;; networks (through a proxy).  The user changes their nick on both
;; networks at the same time.  Query buffers are updated accordingly.

(ert-deftest erc-scenarios-base-renick-queries-bouncer ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/queries")
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       (dumb-server (erc-d-run "localhost" t 'bouncer-foonet 'bouncer-barnet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-accidental-paste-threshold-seconds
       erc-autojoin-channels-alist
       erc-server-buffer-foo
       erc-server-buffer-bar)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 5 (get-buffer "foonet"))

    (ert-info ("Connect to barnet")
      (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "barnet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-bar
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 5 (get-buffer "barnet"))
    (should-not (erc-scenarios-common-buflist "127.0.0.1"))

    (ert-info ("Joined by bouncer to #chan@foonet, pal persent")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@foonet"))
        (funcall expect 1 "rando")
        (funcall expect 1 "simply misused")))

    (ert-info ("Joined by bouncer to #chan@barnet, pal persent")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@barnet"))
        (funcall expect 1 "rando")
        (funcall expect 5 "come, sir, I am")))

    (ert-info ("Query buffer exists for rando@foonet")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "rando@foonet"))
        (funcall expect 1 "guess not")
        (erc-scenarios-common-say "I here")))

    (ert-info ("Query buffer exists for rando@barnet")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "rando@barnet"))
        (funcall expect 2 "rentacop")
        (erc-scenarios-common-say "Linda said you were gonna kill me.")))

    (ert-info ("Sync convo for rando@foonet")
      (with-current-buffer "rando@foonet"
        (funcall expect 10 "u are dumb")
        (erc-scenarios-common-say "not so")))

    (ert-info ("Sync convo for rando@barnet")
      (with-current-buffer "rando@barnet"
        (funcall expect 3 "I never saw her before")
        (erc-scenarios-common-say "You aren't with Wage?")))

    (erc-d-t-wait-for 10 (get-buffer "frenemy@foonet"))
    (erc-d-t-wait-for 10 (get-buffer "frenemy@barnet"))
    (should-not (get-buffer "rando@foonet"))
    (should-not (get-buffer "rando@barnet"))

    (with-current-buffer "frenemy@foonet"
      (funcall expect 10 "now known as")
      (funcall expect 10 "doubly so"))

    (with-current-buffer "frenemy@barnet"
      (funcall expect 10 "now known as")
      (funcall expect 10 "reality picture"))

    (when noninteractive
      (with-current-buffer "frenemy@barnet" (kill-buffer))
      (erc-d-t-wait-for 2 (get-buffer "frenemy"))
      (should-not (get-buffer "frenemy@foonet")))

    (with-current-buffer "#chan@foonet"
      (funcall expect 10 "is now known as frenemy")
      (should-not (search-forward "now known as frenemy" nil t)) ; regression
      (funcall expect 10 "words are razors"))

    (with-current-buffer "#chan@barnet"
      (funcall expect 10 "is now known as frenemy")
      (should-not (search-forward "now known as frenemy" nil t))
      (erc-d-t-search-for 25 "I have lost"))))

;;; erc-scenarios-base-renick.el ends here
