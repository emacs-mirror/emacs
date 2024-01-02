;;; erc-scenarios-misc.el --- Misc scenarios for ERC -*- lexical-binding: t -*-

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

(ert-deftest erc-scenarios-base-flood ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/flood")
       (dumb-server (erc-d-run "localhost" t 'soju))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.5) ; this ratio MUST match
       (erc-server-flood-margin 1.5) ;  the default of 3:10
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist)

    (ert-info ("Connect to bouncer")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port port
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "Soju")))

    (ert-info ("#chan@foonet exists")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan/foonet"))
        (erc-d-t-search-for 2 "<bob/foonet>")
        (erc-d-t-absent-for 0.1 "<joe")
        (funcall expect 3 "was created on")))

    (ert-info ("#chan@barnet exists")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan/barnet"))
        (erc-d-t-search-for 2 "<joe/barnet>")
        (erc-d-t-absent-for 0.1 "<bob")
        (funcall expect 3 "was created on")
        (funcall expect 5 "To get good guard")))

    (ert-info ("Message not held in queue limbo")
      (with-current-buffer "#chan/foonet"
        ;; Without 'no-penalty param in `erc-server-send', should fail
        ;; after ~10 secs with:
        ;;
        ;;   (erc-d-timeout "Timed out awaiting request: (:name ~privmsg
        ;;    :pattern \\`PRIVMSG #chan/foonet :alice: hi :timeout 2
        ;;    :dialog soju)")
        ;;
        ;; Try reversing commit and spying on queue interactively
        (erc-cmd-MSG "#chan/foonet alice: hi")
        (funcall expect 5 "tester: Good, very good")))

    (ert-info ("All output sent")
      (with-current-buffer "#chan/foonet"
        (funcall expect 8 "Some man or other"))
      (with-current-buffer "#chan/barnet"
        (funcall expect 10 "That's he that was Othello")))))

;; Corner case demoing fallback behavior for an absent 004 RPL but a
;; present 422 or 375.  If this is unlikely enough, remove or guard
;; with `ert-skip' plus some condition so it only runs when explicitly
;; named via ERT specifier

(ert-deftest erc-scenarios-networks-announced-missing ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "networks/announced-missing")
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 1 "Failed to determine")
        (funcall expect 1 "Failed to determine")
        (funcall expect 1 "Connection failed")
        (should (string-prefix-p "Unknown" (erc-network-name)))
        (should (string= erc-server-announced-name "irc.foonet.org"))))))

;; Targets that are host/server masks like $*, $$*, and #* are routed
;; to the server buffer: https://github.com/ircdocs/wooooms/issues/5

(ert-deftest erc-scenarios-base-mask-target-routing ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/mask-target-routing")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 10 (get-buffer "foonet"))

    (ert-info ("Channel buffer #foo playback received")
      (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#foo"))
        (funcall expect 10 "Excellent workman")))

    (ert-info ("Global notices routed to server buffer")
      (with-current-buffer "foonet"
        (funcall expect 10 "going down soon")
        (funcall expect 10 "this is a warning")
        (funcall expect 10 "second warning")
        (funcall expect 10 "final warning")))

    (should-not (get-buffer "$*"))))

(ert-deftest erc-scenarios-dcc-chat-accept ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "dcc/chat")
       (dcc-server (erc-d-run "127.0.0.1" t "erc-dcc-server" 'accept-dcc
                              :ending "\n"))
       (dcc-port (process-contact dcc-server :service))
       (dumb-server (erc-d-run "localhost" t 'accept :tmpl-vars
                               `((port . ,(number-to-string dcc-port)))))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Offer received")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet"))
        (funcall expect 10 "DCC: chat offered by dummy")
        (erc-cmd-DCC "CHAT" "dummy")))

    ;; Regression
    (erc-d-t-ensure-for 1 (not (get-buffer "tester")))

    ;; Becomes current buffer by default (because `erc-join-buffer')
    (erc-d-t-wait-for 10 (get-buffer "DCC-CHAT-dummy"))

    (with-current-buffer "foonet"
      (funcall expect 10 "*** DCC: accepting chat from dummy"))

    (ert-info ("Chat with dummy")
      (with-current-buffer "DCC-CHAT-dummy"
        (erc-scenarios-common-say "Hi")
        (funcall expect 10 "Hola")))))

(defvar url-irc-function)

(ert-deftest erc-scenarios-handle-irc-url ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/legacy")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (url-irc-function 'url-irc-erc)
       (erc-url-connect-function
        (lambda (scheme &rest r)
          (ert-info ("Connect to foonet")
            (should (equal scheme "irc"))
            (with-current-buffer (apply #'erc `(:full-name "tester" ,@r))
              (should (string= (buffer-name)
                               (format "127.0.0.1:%d" port)))
              (current-buffer))))))

    (with-temp-buffer
      (insert (format ";; irc://tester:changeme@127.0.0.1:%d/#chan" port))
      (goto-char 10)
      (browse-url-at-point))

    (ert-info ("Connected")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "welcome")))))

;;; erc-scenarios-misc.el ends here
