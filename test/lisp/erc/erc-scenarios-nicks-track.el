;;; erc-scenarios-nicks-track.el --- erc-nicks track integration -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

(require 'erc-track)
(require 'erc-nicks)

(ert-deftest erc-scenarios-nicks-track/prioritize ()
  :tags '(:expensive-test)

  (should (eq erc-nicks-track-faces 'prioritize))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "nicks")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'track-prioritize))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (erc-modules (cons 'nicks erc-modules))
       (port (process-contact dumb-server :service))

       ;; Helper to look up current buffer in track alist.
       (assert-result (lambda (result)
                        (should (equal (alist-get (current-buffer)
                                                  erc-modified-channels-alist)
                                       result))))

       ;; Nicks-owned faces, assigned below as they arrive
       bob-face alice-face)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    ;; Wait for all the boilerplate messages to arrive, clear the
    ;; erc-track mode-line segment, and create control window.
    (with-current-buffer (switch-to-buffer
                          (erc-d-t-wait-for 10 (get-buffer "#chan")))
      (funcall expect 10 "#chan was created on"))
    (with-current-buffer "foonet"
      (erc-track-switch-buffer 1)
      (redisplay) ; ensure track segment is updated
      (erc-scenarios-common-say "/query control")
      (delete-other-windows))

    ;; The NOTICE face from Bob's JOIN message is the only thing in the
    ;; track segment.
    (with-current-buffer "control" (erc-send-message "1"))
    (with-current-buffer "#chan"
      (funcall expect 10 '(: "Bob" (+ nonl) " has joined channel #chan"))
      (funcall assert-result '(1 . erc-notice-face)))

    ;; Someone (Bob) speaks, and the mode-line changes to a `nicks'
    ;; owned composite face for the speaker.
    (with-current-buffer "control" (erc-send-message "2"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Most manifest"))
    (with-current-buffer "foonet"
      (setq bob-face (erc-nicks--get-face "bob" "bob@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(2 ,bob-face erc-nick-default-face)))

    ;; That same someone (Bob) speaks, and the mode-line indicator
    ;; changes to another "normal" face in the message body.
    (with-current-buffer "control" (erc-send-message "3"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> To bed, to bed")
      (funcall assert-result '(3 . erc-default-face)))

    ;; And yet again, which results in the indicator going back to 2.
    (with-current-buffer "control" (erc-send-message "4"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Since you can cog")
      (funcall assert-result `(4 ,bob-face erc-nick-default-face)))

    ;; Now the same person mentions another server user (Alice),
    ;; resulting in a change to *that* `nicks' owned face because it
    ;; appears later in the message content (timestamp is last).
    (with-current-buffer "control" (erc-send-message "5"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> alice: Not to-night"))
    (with-current-buffer "foonet"
      (setq alice-face (erc-nicks--get-face "alice" "alice@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(5 ,alice-face erc-default-face)))

    ;; The mentioned user (Alice) replies, mentioning the mentioner
    ;; (Bob).  But instead of the normal "normals" processing preferring
    ;; the ranked `erc-default-face', the `erc-nicks-track-faces' logic
    ;; kicks in via `erc-track--alt-normals-function' and provides a
    ;; `nicks' owned replacement in Bob's face.
    (with-current-buffer "control" (erc-send-message "6"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Madam, my lord")
      (funcall assert-result `(6 ,bob-face erc-default-face)))

    ;; Finally, another NOTICE arrives and clobbers everything.
    (with-current-buffer "control" (erc-send-message "7"))
    (with-current-buffer "#chan"
      (funcall expect 10 "has quit")
      (funcall assert-result '(7 . erc-notice-face)))))

(ert-deftest erc-scenarios-nicks-track/defer ()
  :tags '(:expensive-test)

  (should (eq erc-nicks-track-faces 'prioritize))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "nicks")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'track-prioritize))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (erc-modules (cons 'nicks erc-modules))
       (port (process-contact dumb-server :service))

       (erc-nicks-track-faces 'defer)

       ;; Helper to look up current buffer in track alist.
       (assert-result (lambda (result)
                        (should (equal (alist-get (current-buffer)
                                                  erc-modified-channels-alist)
                                       result))))

       ;; Nicks-owned faces, assigned below as they arrive
       bob-face alice-face)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    ;; Wait for all the boilerplate messages to arrive, clear the
    ;; erc-track mode-line segment, and create control window.
    (with-current-buffer (switch-to-buffer
                          (erc-d-t-wait-for 10 (get-buffer "#chan")))
      (funcall expect 10 "#chan was created on"))
    (with-current-buffer "foonet"
      (erc-track-switch-buffer 1)
      (redisplay) ; ensure track segment is updated
      (erc-scenarios-common-say "/query control")
      (delete-other-windows))

    ;; The NOTICE face from Bob's JOIN message is the only thing in the
    ;; track segment.
    (with-current-buffer "control" (erc-send-message "1"))
    (with-current-buffer "#chan"
      (funcall expect 10 '(: "Bob" (+ nonl) " has joined channel #chan"))
      (funcall assert-result '(1 . erc-notice-face)))

    ;; Someone (Bob) speaks, and the mode-line indicator changes to the
    ;; highest ranked face in the message.  (All `nicks' owned faces are
    ;; unranked).
    (with-current-buffer "control" (erc-send-message "2"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Most manifest"))
    (with-current-buffer "foonet"
      (setq bob-face (erc-nicks--get-face "bob" "bob@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result '(2 . erc-default-face)))

    ;; That same someone (Bob) speaks, and the mode-line indicator
    ;; changes to a `nicks' owned face.  It first reaches for the
    ;; highest ranked face in the message but then applies the "normals"
    ;; rules, resulting in a promoted alternate.
    (with-current-buffer "control" (erc-send-message "3"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> To bed, to bed")
      (funcall assert-result `(3 ,bob-face erc-nick-default-face)))

    ;; And yet again, which results in the indicator going back to 2.
    (with-current-buffer "control" (erc-send-message "4"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Since you can cog")
      (funcall assert-result '(4 . erc-default-face)))

    ;; The same person (Bob) mentions another server user (Alice),
    ;; resulting in a change to that `nicks' owned face because the
    ;; logic from 3 again applies.
    (with-current-buffer "control" (erc-send-message "5"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> alice: Not to-night"))
    (with-current-buffer "foonet"
      (setq alice-face (erc-nicks--get-face "alice" "alice@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(5 ,alice-face erc-default-face)))

    ;; The mentioned user (Alice) replies, mentioning the mentioner
    ;; (Bob).  However, the `nicks' module does NOT intercede in the
    ;; decision making to overrule the ranked nominee.
    (with-current-buffer "control" (erc-send-message "6"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Madam, my lord")
      (funcall assert-result '(6 . erc-default-face)))

    ;; Finally, another NOTICE arrives and clobbers everything.
    (with-current-buffer "control" (erc-send-message "7"))
    (with-current-buffer "#chan"
      (funcall expect 10 "has quit")
      (funcall assert-result '(7 . erc-notice-face)))))

(ert-deftest erc-scenarios-nicks-track/nil ()
  :tags '(:expensive-test)

  (should (eq erc-nicks-track-faces 'prioritize))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "nicks")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'track-prioritize))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (erc-modules (cons 'nicks erc-modules))
       (port (process-contact dumb-server :service))

       (erc-nicks-track-faces nil)

       ;; Helper to look up current buffer in track alist.
       (assert-result (lambda (result)
                        (should (equal (alist-get (current-buffer)
                                                  erc-modified-channels-alist)
                                       result))))

       ;; Nicks-owned faces, assigned below as they arrive
       bob-face alice-face)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    ;; Wait for all the boilerplate messages to arrive, clear the
    ;; erc-track mode-line segment, and create control window.
    (with-current-buffer (switch-to-buffer
                          (erc-d-t-wait-for 10 (get-buffer "#chan")))
      (funcall expect 10 "#chan was created on"))
    (with-current-buffer "foonet"
      (erc-track-switch-buffer 1)
      (redisplay) ; ensure track segment is updated
      (erc-scenarios-common-say "/query control")
      (delete-other-windows))

    ;; The NOTICE face from Bob's JOIN message is the only element in
    ;; the track segment.
    (with-current-buffer "control" (erc-send-message "1"))
    (with-current-buffer "#chan"
      (funcall expect 10 '(: "Bob" (+ nonl) " has joined channel #chan"))
      (funcall assert-result '(1 . erc-notice-face)))

    ;; Someone (Bob) speaks, and the mode-line indicator changes to the
    ;; only ranked face in the message, `erc-nick-default-face', which
    ;; is the "backing" face under the nicks-owned one in the speaker's
    ;; tag.
    (with-current-buffer "control" (erc-send-message "2"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Most manifest"))
    (with-current-buffer "foonet"
      (setq bob-face (erc-nicks--get-face "bob" "bob@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result '(2 . erc-nick-default-face)))

    ;; That same someone (Bob) speaks, and since another "normals"
    ;; face exists in the message, the indicator alternates to it.
    (with-current-buffer "control" (erc-send-message "3"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> To bed, to bed")
      (funcall assert-result '(3 . erc-default-face)))

    ;; And back again.
    (with-current-buffer "control" (erc-send-message "4"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Since you can cog")
      (funcall assert-result '(4 . erc-nick-default-face)))

    ;; The same person (Bob) mentions another server user (Alice), but
    ;; the same logic applies, and the indicator alternates because the
    ;; incumbent, `erc-nick-default-face', is still the highest ranked
    ;; non-nicks "normals" face in the current message.
    (with-current-buffer "control" (erc-send-message "5"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> alice: Not to-night"))
    (with-current-buffer "foonet"
      (setq alice-face (erc-nicks--get-face "alice" "alice@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(5 . erc-default-face)))

    ;; The mentioned user (Alice) replies, mentioning the mentioner
    ;; (Bob), and the same alternating logic applies.
    (with-current-buffer "control" (erc-send-message "6"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Madam, my lord")
      (funcall assert-result '(6 . erc-nick-default-face)))

    ;; Finally, another NOTICE arrives and clobbers everything.
    (with-current-buffer "control" (erc-send-message "7"))
    (with-current-buffer "#chan"
      (funcall expect 10 "has quit")
      (funcall assert-result '(7 . erc-notice-face)))))

(ert-deftest erc-scenarios-nicks-track/t ()
  :tags '(:expensive-test)

  (should (eq erc-nicks-track-faces 'prioritize))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "nicks")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'track-t))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (erc-modules (cons 'nicks erc-modules))
       (port (process-contact dumb-server :service))

       (erc-nicks-track-faces t)

       ;; Helper to look up current buffer in track alist.
       (assert-result (lambda (result)
                        (should (equal (alist-get (current-buffer)
                                                  erc-modified-channels-alist)
                                       result))))

       ;; Nicks-owned faces, assigned below as they arrive
       bob-face alice-face)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    ;; Wait for all the boilerplate messages to arrive, clear the
    ;; erc-track mode-line segment, and create control window.
    (with-current-buffer (switch-to-buffer
                          (erc-d-t-wait-for 10 (get-buffer "#chan")))
      (funcall expect 10 "#chan was created on"))
    (with-current-buffer "foonet"
      (erc-track-switch-buffer 1)
      (redisplay) ; ensure track segment is updated
      (erc-scenarios-common-say "/query control")
      (delete-other-windows))

    ;; The NOTICE face from Bob's JOIN message is the only element in
    ;; the track segment.
    (with-current-buffer "control" (erc-send-message "1"))
    (with-current-buffer "#chan"
      (funcall expect 10 '(: "Bob" (+ nonl) " has joined channel #chan"))
      (funcall assert-result '(1 . erc-notice-face)))

    ;; Someone (Bob) speaks, and the mode-line indicator changes to that
    ;; someone's `nicks'-owned face.
    (with-current-buffer "control" (erc-send-message "2"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> Most manifest"))
    (with-current-buffer "foonet"
      (setq bob-face (erc-nicks--get-face "bob" "bob@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(2 ,bob-face erc-nick-default-face)))

    ;; That same someone (Bob) speaks, and though one other "normal"
    ;; exists in the message, `erc-default-face', no update occurs.
    (with-current-buffer "control" (erc-send-message "3"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> To bed, to bed")
      (funcall assert-result `(3 ,bob-face erc-nick-default-face)))

    ;; Another server user (Alice) speaks, mentioning the previous
    ;; speaker (Bob), and the indicator is updated to reflect the new
    ;; speaker.
    (with-current-buffer "control" (erc-send-message "4"))
    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Madam, my lord"))
    (with-current-buffer "foonet"
      (setq alice-face (erc-nicks--get-face "alice" "alice@foonet")))
    (with-current-buffer "#chan"
      (funcall assert-result `(4 ,alice-face erc-nick-default-face)))

    ;; Finally, another NOTICE arrives and clobbers everything.
    (with-current-buffer "control" (erc-send-message "5"))
    (with-current-buffer "#chan"
      (funcall expect 10 "has quit")
      (funcall assert-result '(5 . erc-notice-face)))))

;;; erc-scenarios-nicks-track.el ends here
