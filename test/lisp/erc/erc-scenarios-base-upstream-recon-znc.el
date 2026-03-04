;;; erc-scenarios-base-upstream-recon-znc.el --- Bouncer recon scenario -*- lexical-binding: t -*-

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

;; Commentary:

;; These concern the loss and recovery of a proxy's IRC-side
;; connection (hence "upstream").

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-upstream-recon--znc ()
  :tags '(:expensive-test)
  (erc-scenarios-common--upstream-reconnect
   (lambda ()
     (with-current-buffer "*status@foonet"
       (erc-d-t-search-for 1 "Disconnected from IRC")
       (erc-d-t-search-for 1 "Connected!"))
     (with-current-buffer "*status@barnet"
       (erc-d-t-search-for 1 "Disconnected from IRC")
       (erc-d-t-search-for 1 "Connected!")))
   'znc-foonet
   'znc-barnet))

;; Here, the upstream connection is already severed when first
;; connecting.  The bouncer therefore sends query messages from an
;; administrative bot before the first numerics burst, which results
;; in a target buffer not being associated with an `erc-networks--id'.
;; The problem only manifests later, when the buffer-association
;; machinery checks the names of all target buffers and assumes a
;; non-nil `erc-networks--id'.
(ert-deftest erc-scenarios-upstream-recon--znc/severed ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/upstream-reconnect")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'znc-severed))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester@vanilla/foonet"
                                :password "changeme"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 6 (eq (erc-network) 'foonet))))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "*status"))
      (funcall expect 10 "Connection Refused.  Reconnecting...")
      (funcall expect 10 "Connected!"))

    (ert-info ("Join #chan")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "<alice> tester, welcome!")
        (funcall expect 10 "<bob> alice: And see a fearful sight")
        (funcall expect 10 "<eve> hola")
        (funcall expect 10 "<Evel> hell o")
        ;;
        (funcall expect 10 "<alice> bob: Or to drown my clothes")))

    (ert-info ("Buffer not renamed with net id")
      (should (get-buffer "*status")))

    (ert-info ("No error")
      (with-current-buffer (messages-buffer)
        (funcall expect -0.1 "error in process filter")))))

;;; erc-scenarios-base-upstream-recon-znc.el ends here
