;;; erc-scenarios-fill-wrap.el --- Fill-wrap module -*- lexical-binding: t -*-

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

(defun erc-scenarios-fill-wrap--merged-p ()
  (get-text-property (pos-bol) 'erc-fill--wrap-merge))

;; This asserts that an intervening date stamp between two messages
;; from the same speaker will trigger a break in merge detection, so
;; the second message's speaker tag won't be hidden.
(ert-deftest erc-scenarios-fill-wrap/merge-datestamp ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "fill/wrap")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'merge-datestamp))
       (erc-stamp--tz t)
       ;; Start at 2023-10-22T06:16:43.445Z
       (erc-stamp--current-time (if (< emacs-major-version 29)
                                    '(25908 23515 445000 0)
                                  '(1697930203445 . 1000)))
       (erc-timer-hook (cons (lambda (&rest _)
                               (setq erc-stamp--current-time
                                     (time-add erc-stamp--current-time 15)))
                             erc-timer-hook))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan" "#control")))
       (erc-modules `(nicks fill-wrap scrolltobottom ,@erc-modules))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy"))
      (funcall expect 10 "<dummy> hi")
      (funcall expect 10 "<dummy> there"))

    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> tester, welcome")

      ;; Force date change.
      (setq erc-stamp--current-time
            (time-add erc-stamp--current-time (* 60 60))))

    (with-current-buffer "#control"
      (erc-send-message "1"))

    (with-current-buffer "#chan"
      (funcall expect 10 "[Sun Oct 22 2023]")
      (funcall expect 10 "<bob> one")
      (should-not (erc-scenarios-fill-wrap--merged-p)))

    (with-current-buffer "#control"
      (erc-send-message "2"))

    (with-current-buffer "dummy"
      (funcall expect 10 "[Sun Oct 22 2023]")
      (funcall expect 10 "<dummy> again")
      (should-not (erc-scenarios-fill-wrap--merged-p)))

    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: He was famous"))

    (erc-scrolltobottom-mode -1)))

(defun erc-scenarios-fill-wrap/right-margin-stamp-face--assert-superior ()
  (erc-d-t-wait-for 10 (erc-scenarios-common-term-with-line-mode
                        (search-forward "[23:20]" nil t)))
  (should (member
           (get-text-property (match-beginning 0) 'font-lock-face)
           (list '((:foreground "#00FF00") term-bold) ; 31 graphical
                 '((:foreground "green3") term-bold) ; 31 non-graphical
                 '(( :foreground "green3" ; 29,30 non-graphical
                     :background "unspecified-bg"
                     :inverse-video nil)
                   term-bold)
                 '(( :foreground "green3" ; 28 non-graphical
                     :background "unspecified-bg"
                     :inverse-video nil)
                   :inherit term-bold)))))

;; Before Emacs 31, a display property's margin string would inherit
;; face properties residing at the same buffer position.  Beginning in
;; 31, only those on the spec's string itself were to be considered.  To
;; adapt, ERC added an explicit face to its margin stamps.  This test
;; checks the final "realized" appearance without regard for the
;; properties themselves.  It does so through very roundabout means,
;; using a term.el subprocess.  Obviously, reverting 9ba65aa9 "Fix
;; missing margin face on display prop in erc-stamp" or commenting out
;; its addition to `erc-insert-timestamp-right' makes this test fail
;; unless the inciting changes added by bug#80693 were to also be
;; reverted to restore pre-31 behavior.
(ert-deftest erc-scenarios-fill-wrap/right-margin-stamp-face ()
  :tags '(:expensive-test :unstable)

  (when (and noninteractive (= emacs-major-version 27))
    (ert-skip "May stall on Emacs 27"))

  ;; Force fixture to use inferior process so that face-merging behavior
  ;; can be verified in the controlling Emacs instance.
  (let ((erc-scenarios-common-interactive-debug-term-p (not noninteractive)))
    (erc-scenarios-common-with-noninteractive-in-term
        ((erc-scenarios-common-dialog "join/legacy")
         (erc-server-flood-penalty 0.1)
         (dumb-server (erc-d-run "localhost" t 'foonet))
         (erc-stamp--tz t)
         ;; Start at 2023-10-22T06:16:43.445Z
         (erc-stamp--current-time (if (< emacs-major-version 29)
                                      '(25908 23515 445000 0)
                                    '(1697930203445 . 1000)))
         (erc-timer-hook (cons (lambda (&rest _)
                                 (setq erc-stamp--current-time
                                       (time-add erc-stamp--current-time 15)))
                               erc-timer-hook))
         (expect (erc-d-t-make-expecter))
         (erc-autojoin-channels-alist '((FooNet "#chan")))
         (erc-modules `(fill-wrap ,@erc-modules))
         (port (process-contact dumb-server :service)))

      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode"))

      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "[23:20]")

        (erc-scenarios-common-term-call-in-superior
         #'erc-scenarios-fill-wrap/right-margin-stamp-face--assert-superior)
        (funcall expect 10 "<bob> tester, welcome")))))

;;; erc-scenarios-fill-wrap.el ends here
