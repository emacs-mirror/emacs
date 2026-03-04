;;; erc-scenarios-base-attach.el --- Reattach scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;;; Commentary:

;; See also: `erc-scenarios-base-channel-buffer-revival'.
;;
;; ERC 5.5 silently dropped support for the ancient option
;; `erc-query-on-unjoined-chan-privmsg' because the tangled logic in
;; and around the function `erc-auto-query' made it difficult to
;; divine its purpose.
;;
;; Based on the name, it was thought this option likely involved
;; controlling the creation of query buffers for unsolicited messages
;; from users with whom you don't share a common channel.  However,
;; additional spelunking has recently revealed that it was instead
;; meant to service a feature offered by most bouncers that sends
;; PRIVMSGs directed at a channel you're no longer in and that you
;; haven't received a(nother) JOIN message for.  IOW, this is meant to
;; support the following sequence of events:
;;
;;   1. /detach #chan
;;   2. kill buffer #chan or reconnect in new Emacs session
;;   3. /playbuffer #chan
;;
;; Note that the above slash commands are bouncer-specific aliases.
;;
;; Interested users can find more info by looking at this change set
;; from the ancient CVS repo:
;;
;;   Author:     Mario Lang <mlang@delysid.org>
;;   AuthorDate: Mon Nov 26 18:33:19 2001 +0000
;;
;;   * new function erc-BBDB-NICK to handle nickname annotation ...
;;   * Applied antifuchs/mhp patches, the latest on erc-help, unmodified
;;   * New variable: erc-reuse-buffers default to t.
;;   * Modified erc-generate-new-buffer-name to use it. it checks if
;;     server and port are the same, then one can assume that's the same
;;     channel/query target again.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-base-attach--ensure-target-buffer--enabled ()
  :tags '(:expensive-test)
  (should erc-ensure-target-buffer-on-privmsg)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/channel-buffer-revival")
       (dumb-server (erc-d-run "localhost" t 'reattach))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "tester@vanilla/foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet"))
      (erc-cmd-MSG "*status playbuffer #chan"))

    (ert-info ("Playback appears in buffer #chan")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "Buffer Playback...")
        (funcall expect 10 "Was I a child")
        (funcall expect 10 "Thou counterfeit'st most lively")
        (funcall expect 10 "Playback Complete")))

    (with-current-buffer "foonet"
      (erc-cmd-MSG "*status attach #chan"))

    (ert-info ("Live output from #chan after more playback")
      (with-current-buffer "#chan"
        (funcall expect 10 "Buffer Playback...")
        (funcall expect 10 "With what it loathes")
        (funcall expect 10 "Not by his breath")
        (funcall expect 10 "Playback Complete")
        (funcall expect 10 "Ay, and the captain")
        (erc-scenarios-common-say "bob: hi")
        (funcall expect 10 "Pawn me to this")))))

(ert-deftest erc-scenarios-base-attach--ensure-target-buffer--disabled ()
  :tags '(:expensive-test)
  (should erc-ensure-target-buffer-on-privmsg)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/channel-buffer-revival")
       (dumb-server (erc-d-run "localhost" t 'reattach))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-ensure-target-buffer-on-privmsg nil) ; off
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "tester@vanilla/foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet"))
      (erc-cmd-MSG "*status playbuffer #chan")
      (ert-info ("Playback appears in buffer server buffer")
        (erc-d-t-ensure-for -1 (not (get-buffer "#chan")))
        (funcall expect 10 "Buffer Playback...")
        (funcall expect 10 "Was I a child")
        (funcall expect 10 "Thou counterfeit'st most lively")
        (funcall expect 10 "Playback Complete"))
      (should-not (get-buffer "#chan"))
      (erc-cmd-MSG "*status attach #chan"))

    (ert-info ("Buffer #chan joined")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "Buffer Playback...")
        (funcall expect 10 "With what it loathes")
        (funcall expect 10 "Not by his breath")
        (funcall expect 10 "Playback Complete")
        (funcall expect 10 "Ay, and the captain")
        (erc-scenarios-common-say "bob: hi")
        (funcall expect 10 "Pawn me to this")))))


;; We omit the `enabled' case for queries because it's the default for
;; this option and already covered many times over by other tests in
;; this directory.

(ert-deftest erc-scenarios-base-attach--ensure-target-buffer--disabled-query ()
  :tags '(:expensive-test)
  (should erc-ensure-target-buffer-on-privmsg)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/queries")
       (dumb-server (erc-d-run "localhost" t 'non-erc))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-ensure-target-buffer-on-privmsg nil)
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

    (ert-info ("User dummy's greeting appears in server buffer")
      (erc-d-t-wait-for -1 (get-buffer "dummy"))
      (with-current-buffer "foonet"
        (funcall expect 5 "hi")

        (ert-info ("Option being nil doesn't queries we create")
          (with-current-buffer (erc-cmd-QUERY "nitwit")
            (should (equal (buffer-name) "nitwit"))
            (erc-scenarios-common-say "hola")
            (funcall expect 5 "ciao")))

        (erc-scenarios-common-say "howdy")
        (funcall expect 5 "no target")
        (erc-cmd-MSG "dummy howdy")
        (funcall expect 5 "bye")
        (erc-cmd-QUIT "")))))

;;; erc-scenarios-base-attach.el ends here
