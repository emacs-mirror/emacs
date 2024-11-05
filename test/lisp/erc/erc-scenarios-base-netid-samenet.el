;;; erc-scenarios-base-netid-samenet.el --- One-network net-ID scenarios -*- lexical-binding: t -*-

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

(cl-defun erc-scenarios-common--base-network-id-same-network
    ((&key nick id server chan
           &aux (nick-a nick) (id-a id) (serv-buf-a server) (chan-buf-a chan))
     (&key nick id server chan
           &aux (nick-b nick) (id-b id) (serv-buf-b server) (chan-buf-b chan)))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/netid/samenet")
       (dumb-server (erc-d-run "localhost" t 'tester 'chester))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       erc-serv-buf-a erc-serv-buf-b)

    (when (and id-a (zerop (random 2))) (setq id-a (symbol-name id-a)))
    (when (and id-b (zerop (random 2))) (setq id-b (symbol-name id-b)))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer
          (setq erc-serv-buf-a (erc :server "127.0.0.1"
                                    :port port
                                    :nick nick-a
                                    :password "changeme"
                                    :full-name nick-a
                                    :id id-a))
        (erc-scenarios-common-assert-initial-buf-name id-a port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer
          (setq erc-serv-buf-b (erc :server "127.0.0.1"
                                    :port port
                                    :nick nick-b
                                    :password "changeme"
                                    :full-name nick-b
                                    :id id-b))
        (erc-scenarios-common-assert-initial-buf-name id-b port)))

    (erc-d-t-wait-for 3 (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer erc-serv-buf-a
      (should (string= (buffer-name) serv-buf-a))
      (funcall expect 8 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (with-current-buffer erc-serv-buf-b
      (should (string= (buffer-name) serv-buf-b))
      (funcall expect 8 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer chan-buf-a))
    (erc-d-t-wait-for 10 (get-buffer chan-buf-b))

    (ert-info ("Greets other nick in same channel")
      (with-current-buffer chan-buf-a
        (funcall expect 5 "chester")
        (funcall expect 5 "find the forester")
        (erc-cmd-MSG "#chan chester: hi")))

    (ert-info ("Sees other nick in same channel")
      (with-current-buffer chan-buf-b
        (funcall expect 5 "tester")
        (funcall expect 10 "<tester> chester: hi")
        (funcall expect 5 "This was lofty")
        (erc-cmd-MSG "#chan hi tester")))

    (with-current-buffer chan-buf-a
      (funcall expect 5 "To employ you towards")
      (erc-cmd-QUIT ""))

    (with-current-buffer chan-buf-b
      (funcall expect 5 "To employ you towards")
      (erc-cmd-QUIT ""))))

(ert-deftest erc-scenarios-base-network-id-same-network--two-ids ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id 'tester/foonet
         :server "tester/foonet"
         :chan "#chan@tester/foonet")
   (list :nick "chester"
         :id 'chester/foonet
         :server "chester/foonet"
         :chan "#chan@chester/foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--one-id-tester ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id 'tester/foonet
         :server "tester/foonet"
         :chan "#chan@tester/foonet")
   (list :nick "chester"
         :id nil
         :server "foonet"
         :chan "#chan@foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--one-id-chester ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id nil
         :server "foonet"
         :chan "#chan@foonet")
   (list :nick "chester"
         :id 'chester/foonet
         :server "chester/foonet"
         :chan "#chan@chester/foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--no-ids ()
  :tags '(:expensive-test)
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id nil
         :server "foonet/tester"
         :chan "#chan@foonet/tester") ; <- note net before nick
   (list :nick "chester"
         :id nil
         :server "foonet/chester"
         :chan "#chan@foonet/chester")))

;;; erc-scenarios-base-netid-samenet.el ends here
