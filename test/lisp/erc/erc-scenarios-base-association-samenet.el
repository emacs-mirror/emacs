;;; erc-scenarios-base-association-samenet.el --- assoc samenet scenarios -*- lexical-binding: t -*-

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

(declare-function erc-network-name "erc-networks")
(declare-function erc-network "erc-networks")
(defvar erc-autojoin-channels-alist)
(defvar erc-network)

;; One network, two simultaneous connections, no IDs.
;; Reassociates on reconnect with and without server buffer.

(defun erc-scenarios-common--base-association-samenet (after)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/samenet")
       (dumb-server (erc-d-run "localhost" t 'tester 'chester 'tester2))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "chester"
                                :password "changeme"
                                :full-name "chester")
        (erc-scenarios-common-assert-initial-buf-name nil port)))

    (erc-d-t-wait-for 3 "Dialed Buflist is Empty"
      (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer "foonet/tester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/tester"))
    (with-current-buffer "foonet/chester" (funcall expect 3 "debug mode"))
    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/chester"))

    (ert-info ("Nick tester sees other nick chester in channel")
      (with-current-buffer "#chan@foonet/tester"
        (funcall expect 5 "chester")
        (funcall expect 5 "find the forester")
        (erc-cmd-QUIT "")))

    (ert-info ("Nick chester sees other nick tester in same channel")
      (with-current-buffer  "#chan@foonet/chester"
        (funcall expect 5 "tester")
        (funcall expect 5 "find the forester")))

    (funcall after expect)))

(ert-deftest erc-scenarios-base-association-samenet--reconnect-one ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-association-samenet
   (lambda (expect)

     (ert-info ("Connection tester reconnects")
       (with-current-buffer "foonet/tester"
         (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
         (funcall expect 10 "*** ERC finished")
         (erc-cmd-RECONNECT)
         (funcall expect 5 "debug mode")))

     (ert-info ("Reassociated to same channel")
       (with-current-buffer "#chan@foonet/tester"
         (funcall expect 5 "chester")
         (funcall expect 5 "welcome again")
         (erc-cmd-QUIT "")))

     (with-current-buffer "#chan@foonet/chester"
       (funcall expect 5 "tester")
       (funcall expect 5 "welcome again")
       (funcall expect 5 "welcome again")
       (erc-cmd-QUIT "")))))

(ert-deftest erc-scenarios-base-association-samenet--new-buffer ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-association-samenet
   (lambda (expect)

     (ert-info ("Tester kills buffer and connects from scratch")

       (let (port)
         (with-current-buffer "foonet/tester"
           (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
           (funcall expect 10 "*** ERC finished")
           (setq port erc-session-port)
           (kill-buffer))

         (with-current-buffer (erc :server "127.0.0.1"
                                   :port port
                                   :nick "tester"
                                   :password "changeme"
                                   :full-name "tester")

           (erc-d-t-wait-for 5 (eq erc-network 'foonet)))))

     (with-current-buffer "foonet/tester" (funcall expect 3 "debug mode"))

     (ert-info ("Reassociated to same channel")
       (with-current-buffer "#chan@foonet/tester"
         (funcall expect 5 "chester")
         (funcall expect 5 "welcome again")
         (erc-cmd-QUIT "")))

     (with-current-buffer "#chan@foonet/chester"
       (funcall expect 5 "tester")
       (funcall expect 5 "welcome again")
       (funcall expect 5 "welcome again")
       (erc-cmd-QUIT "")))))

;;; erc-scenarios-base-association-samenet.el ends here
