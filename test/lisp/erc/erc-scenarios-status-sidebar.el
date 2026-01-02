;;; erc-scenarios-status-sidebar.el --- erc-sidebar/speedbar tests -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-status-sidebar)


(ert-deftest erc-scenarios-status-sidebar--bufbar ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/gapless-connect")
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-penalty erc-server-flood-penalty)
       (erc-modules `(bufbar ,@erc-modules))
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to two different endpoints")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (funcall expect 10 "MOTD File is missing"))
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "barnet:changeme"
                                :full-name "tester")
        (funcall expect 10 "marked as being away")))


    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#bar"))
      (funcall expect 10 "was created on")
      (funcall expect 2 "his second fit"))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#foo"))
      (funcall expect 10 "was created on")
      (funcall expect 2 "no use of him")
      (ert-info ("Activity marker is in the right spot")
        (let ((obuf (window-buffer))) ; *scratch*
          (set-window-buffer (selected-window) "#foo")
          (erc-d-t-wait-for 5
              (erc-status-sidebar-refresh)
            (with-current-buffer "*ERC Status*"
              (and (marker-position erc-status-sidebar--active-marker)
                   (goto-char erc-status-sidebar--active-marker)
                   ;; The " [N]" suffix disappears because it's selected
                   (search-forward "#foo" (pos-eol) t))))
          (set-window-buffer (selected-window) obuf))))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "*ERC Status*"))
      (ert-info ("Hierarchy printed correctly")
        (funcall expect 10 "barnet [")
        (funcall expect 10 "#bar [")
        (funcall expect 10 "foonet [")
        (funcall expect 10 "#foo")))

    (with-current-buffer "#foo"
      (ert-info ("Core toggle and kill commands work")
        ;; Avoid using API, e.g., `erc-status-sidebar-buffer-exists-p',
        ;; etc. for testing commands that call those same functions.
        (should (get-buffer-window "*ERC Status*"))
        (erc-bufbar-mode -1)
        (should-not (get-buffer-window "*ERC Status*"))
        (erc-status-sidebar-kill)
        (should-not (get-buffer "*ERC Status*"))))))

;; We can't currently run this on EMBA because it needs a usable
;; terminal, and we lack a fixture for that.  Please try running this
;; test interactively with both graphical Emacs and non.
(declare-function erc-nickbar-mode "erc-speedbar" (arg))
(declare-function erc-speedbar--get-timers "erc-speedbar" nil)
(declare-function speedbar-timer-fn "speedbar" nil)
(defvar erc-nickbar-mode)
(defvar speedbar-buffer)

;; FIXME move to own file because it takes 20+ seconds, uncompiled.
(ert-deftest erc-scenarios-status-sidebar--nickbar ()
  :tags `(:expensive-test :unstable ,@(and (getenv "ERC_TESTS_GRAPHICAL")
                                           '(:erc--graphical)))
  (when (and noninteractive (= emacs-major-version 27))
    (ert-skip "Hangs on Emacs 27, asking for input"))

  (erc-scenarios-common-with-noninteractive-in-term
      ((erc-scenarios-common-dialog "base/gapless-connect")
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-penalty erc-server-flood-penalty)
       (erc-modules `(nickbar ,@erc-modules))
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to two different endpoints")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (funcall expect 10 "MOTD File is missing"))
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "barnet:changeme"
                                :full-name "tester")
        (funcall expect 10 "marked as being away")))

    (erc-d-t-wait-for 20 (get-buffer "#bar"))
    (with-current-buffer (pop-to-buffer "#bar")
      (funcall expect 10 "was created on")
      (funcall expect 2 "his second fit")
      (erc-d-t-wait-for 10 (and speedbar-buffer (get-buffer speedbar-buffer)))
      (speedbar-timer-fn)
      (with-current-buffer speedbar-buffer
        (funcall expect 10 "#bar (3)")
        (funcall expect 10 '(| "@mike" "joe"))
        (funcall expect 10 '(| "@mike" "joe"))
        (funcall expect 10 "tester")))

    (erc-d-t-wait-for 20 (get-buffer "#foo"))
    (with-current-buffer (pop-to-buffer "#foo")
      (delete-other-windows)
      (funcall expect 10 "was created on")
      (funcall expect 2 "no use of him")
      (speedbar-timer-fn)
      (with-current-buffer speedbar-buffer
        (funcall expect 10 "#foo (3)")
        (funcall expect 10 '(| "alice" "@bob"))
        (funcall expect 10 '(| "alice" "@bob"))
        (funcall expect 10 "tester")))

    (with-current-buffer "#foo"
      (ert-info ("Core toggle and kill commands work")
        ;; Avoid using API, e.g., `erc-status-sidebar-buffer-exists-p',
        ;; etc. for testing commands that call those same functions.
        (call-interactively #'erc-nickbar-mode)
        (should-not erc-nickbar-mode)
        (should-not speedbar-buffer)
        (should-not (get-buffer " SPEEDBAR"))

        (erc-nickbar-mode +1)
        (should (and speedbar-buffer (get-buffer-window speedbar-buffer)))
        (should (eq speedbar-buffer (get-buffer " SPEEDBAR")))
        (should (get-buffer " SPEEDBAR"))

        (erc-nickbar-mode -1)
        (should-not (get-buffer " SPEEDBAR"))
        (should-not erc-nickbar-mode)
        (should-not (cdr (frame-list)))))

    (should-not (erc-speedbar--get-timers))))

;;; erc-scenarios-status-sidebar.el ends here
