;;; erc-scenarios-base-renick.el --- Re-nicking scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

    (erc-d-t-wait-for 1 (get-buffer "foonet"))

    (ert-info ("Joined by bouncer to #foo, pal persent")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (erc-d-t-search-for 1 "On Thursday")
        (erc-scenarios-common-say "hi")))

    (erc-d-t-wait-for 10 "Query buffer appears with message from pal"
      (get-buffer "Lal"))

    (ert-info ("Chat with pal, who changes name")
      (with-current-buffer "Lal"
        (erc-d-t-search-for 3 "hello")
        (erc-scenarios-common-say "hi")
        (erc-d-t-search-for 10 "is now known as Linguo")
        (should-not (search-forward "is now known as Linguo" nil t))))

    (erc-d-t-wait-for 1 (get-buffer "Linguo"))
    (should-not (get-buffer "Lal"))

    (with-current-buffer "Linguo" (erc-scenarios-common-say "howdy Linguo"))

    (with-current-buffer "#foo"
      (erc-d-t-search-for 10 "is now known as Linguo")
      (should-not (search-forward "is now known as Linguo" nil t))
      (erc-cmd-PART ""))

    (with-current-buffer "Linguo"
      (erc-d-t-search-for 10 "get along"))))

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
        (funcall expect 2 "come, sir, I am")))

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
        (funcall expect 1 "u are dumb")
        (erc-scenarios-common-say "not so")))

    (ert-info ("Sync convo for rando@barnet")
      (with-current-buffer "rando@barnet"
        (funcall expect 3 "I never saw her before")
        (erc-scenarios-common-say "You aren't with Wage?")))

    (erc-d-t-wait-for 3 (get-buffer "frenemy@foonet"))
    (erc-d-t-wait-for 3 (get-buffer "frenemy@barnet"))
    (should-not (get-buffer "rando@foonet"))
    (should-not (get-buffer "rando@barnet"))

    (with-current-buffer "frenemy@foonet"
      (funcall expect 1 "now known as")
      (funcall expect 1 "doubly so"))

    (with-current-buffer "frenemy@barnet"
      (funcall expect 1 "now known as")
      (funcall expect 1 "reality picture"))

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
