;;; erc-scenarios-base-query-participants.el --- Query user tables -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

(ert-deftest erc-scenarios-base-query-participants/legacy ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/query-participants")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'legacy))
       (expect (erc-d-t-make-expecter))
       (erc--decouple-query-and-channel-membership-p t)
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")
        (erc-scenarios-common-say "/query bob")))

    (ert-info ("Opening query on untracked user bob doesn't create entry.")
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("DM from untracked user creates a query entry.")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy"))
        (funcall expect 10 "<dummy> hi")
        (should (erc-get-channel-member "dummy"))
        (should (erc-get-server-user "dummy"))))

    (with-current-buffer "foonet"
      (erc-scenarios-common-say "/join #chan"))

    (ert-info ("Members in new chan not added to existing query buffers")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "bob ")) ; some user bob is present in #chan
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("Opening query on tracked user doesn't create entry")
      ;; And DM'ing them makes no difference.
      (with-current-buffer "#chan"
        (funcall expect 10 " alice") ;; some user alice is present
        (erc-scenarios-common-say "hi channel")
        (funcall expect 10 "<tester> hi channel")
        (erc-scenarios-common-say "/query alice"))
      (with-current-buffer "alice"
        (should-not (erc-get-channel-member "alice"))))

    (ert-info ("DM from a tracked user creates entry in preexisting buffer")
      (with-current-buffer "bob"
        (funcall expect 10 "<bob> hi")
        (should (erc-get-channel-member "bob"))))

    (ert-info ("Query pal parting channel doesn't remove them from query")
      ;; Identical result if they're kicked: they're removed from the
      ;; server if they have no target buffers remaining, which can't
      ;; be true if a query with them remains.
      (with-current-buffer "#chan"
        (funcall expect 10 "has left")
        (should-not (erc-get-channel-member "dummy"))
        (should (erc-get-server-user "dummy")))
      (with-current-buffer "dummy"
        (should (erc-get-channel-member "dummy"))))

    (ert-info ("Query pal quitting channel removes them everywhere")
      (with-current-buffer "#chan"
        (funcall expect 10 "has quit")
        (should-not (erc-get-channel-member "bob"))
        (should-not (erc-get-server-user "bob")))
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("Query pal re-joining doesn't repopulate query")
      (with-current-buffer "#chan"
        (erc-scenarios-common-say "bob gone")
        (funcall expect 10 "<alice> bob, welcome back!")
        (should (erc-get-server-user "bob")))
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("Parting removes chan members from server unless in some query")
      (with-current-buffer "#chan"
        (erc-scenarios-common-say "/part")
        (funcall expect 10 "you have left")
        (should-not (erc-get-server-user "fsbot"))
        (should-not (erc-get-server-user "alice")) ; she never said anything
        (should-not (erc-get-server-user "bob")) ; missing from query
        (should (erc-get-server-user "dummy"))))))

(ert-deftest erc-scenarios-base-query-participants/coupled ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/query-participants")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'legacy))
       (expect (erc-d-t-make-expecter))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")
        (erc-scenarios-common-say "/query bob")))

    (ert-info ("Opening query on untracked user bob doesn't create entry.")
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("DM from untracked user also doesn't create a query entry.")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy"))
        (funcall expect 10 "<dummy> hi")
        (should-not (erc-get-channel-member "dummy"))
        (should-not (erc-get-server-user "dummy"))))

    (with-current-buffer "foonet"
      (erc-scenarios-common-say "/join #chan"))

    (ert-info ("Members in new chan added to existing query buffers")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "bob ")) ; bob is present in #chan (353)
      (with-current-buffer "bob"
        (should (erc-get-server-user "bob"))
        ;; Can't assert immediately: must wait until 366 arrives.
        (erc-d-t-wait-for 10 (erc-get-channel-member "bob"))))

    (ert-info ("Opening query on tracked user creates entry")
      (with-current-buffer "#chan"
        (funcall expect 10 " alice") ;; alice is present
        (erc-scenarios-common-say "hi channel") ; gate
        (funcall expect 10 "<tester> hi channel")
        (erc-scenarios-common-say "/query alice"))
      (with-current-buffer "alice"
        (should (erc-get-channel-member "alice"))))

    ;; Bob says something.
    (with-current-buffer "bob"
      (funcall expect 10 "<bob> hi")
      (should (erc-get-channel-member "bob")))

    (ert-info ("Query pal parting channel removes them from query")
      ;; Identical result if they're kicked: they're removed from the
      ;; server AND their target buffers
      (with-current-buffer "#chan"
        (funcall expect 10 "has left")
        (should-not (erc-get-channel-member "dummy"))
        (should-not (erc-get-server-user "dummy")))
      (with-current-buffer "dummy"
        (should-not (erc-get-channel-member "dummy"))))

    ;; This is unchanged from legacy behavior.
    (ert-info ("Query pal quitting channel removes them everywhere")
      (with-current-buffer "#chan"
        (funcall expect 10 "has quit")
        (should-not (erc-get-channel-member "bob"))
        (should-not (erc-get-server-user "bob")))
      (with-current-buffer "bob"
        (should-not (erc-get-channel-member "bob"))))

    (ert-info ("Query pal re-joining repopulates query")
      (with-current-buffer "#chan"
        (erc-scenarios-common-say "bob gone")
        (funcall expect 10 "<alice> bob, welcome back!")
        (should (erc-get-server-user "bob")))
      (with-current-buffer "bob"
        (should (erc-get-channel-member "bob"))))

    (ert-info ("Parting removes chan members from server and queries")
      (with-current-buffer "#chan"
        (erc-scenarios-common-say "/part")
        (funcall expect 10 "you have left")
        (should-not (erc-get-server-user "fsbot"))
        (should-not (erc-get-server-user "alice")) ; she never said anything
        (should-not (erc-get-server-user "bob")) ; missing from query
        (should-not (erc-get-server-user "dummy"))))))


;;; erc-scenarios-base-query-participants.el ends here
