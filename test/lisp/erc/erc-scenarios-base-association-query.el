;;; erc-scenarios-base-association-query.el --- assoc query scenarios -*- lexical-binding: t -*-

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


;; Non-ERC buffers exist whose names match the nicknames of query
;; targets, both newly arriving and outgoing.  No target buffers yet
;; exist for these, so new ones are created that feature a net-ID
;; @suffix.

(ert-deftest erc-scenarios-base-association-existing-non-erc-buffer ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/queries")
       (dumb-server (erc-d-run "localhost" t 'non-erc))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (nitwit (with-current-buffer (get-buffer-create "nitwit")
                 (prin1 (ert-test-name (ert-running-test)) (current-buffer))
                 (current-buffer))) ; these are killed on completion by macro
       (dummy (with-current-buffer (get-buffer-create "dummy")
                (prin1 (ert-test-name (ert-running-test)) (current-buffer))
                (current-buffer)))
       (erc-server-flood-penalty 0.1))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))
        (funcall expect 15 "debug mode")))

    (ert-info ("Nick dummy queries us")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy@foonet"))
        (should (erc-query-buffer-p))
        (funcall expect 5 "hi")

        (ert-info ("We query nick nitwit")
          (with-current-buffer (erc-cmd-QUERY "nitwit")
            (should (equal (buffer-name) "nitwit@foonet"))
            (erc-scenarios-common-say "hola")
            (funcall expect 5 "ciao")))

        (erc-scenarios-common-say "howdy")
        (funcall expect 5 "bye")
        (erc-cmd-QUIT "")))))

;; Someone sending you a PM has the same name as the network (bug#59976)

(ert-deftest erc-scenarios-base-association-some-nick-is-network ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/queries")
       (dumb-server (erc-d-run "localhost" t 'netnick))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Join common channel as nick foonet")
      (with-current-buffer "foonet"
        (funcall expect 15 "debug mode")
        (erc-cmd-JOIN "#chan"))
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 5 "welcome")))

    (ert-info ("Nick foonet PMs us")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet@foonet"))
        (should (erc-query-buffer-p))
        (funcall expect 5 "hi")))))

;;; erc-scenarios-base-association-query.el ends here
