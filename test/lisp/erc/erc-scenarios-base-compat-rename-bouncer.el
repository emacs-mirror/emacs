;;; erc-scenarios-base-compat-rename-bouncer.el --- Compat-rename scenarios -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join))

;; Ensure deprecated option still respected when old default value
;; explicitly set ("respected" in the sense of having names reflect
;; dialed TCP endpoints with possible uniquifiers but without any of
;; the old issues, pre-bug#48598).

(defun erc-scenarios-common--base-compat-no-rename-bouncer (dialogs auto more)
  (erc-scenarios-common-with-cleanup
      ;; These actually *are* (assigned-)network-id related because
      ;; our kludge assigns one after the fact.
      ((erc-scenarios-common-dialog "base/netid/bouncer")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (apply #'erc-d-run "localhost" t dialogs))
       (port (process-contact dumb-server :service))
       (chan-buf-foo (format "#chan@127.0.0.1:%d" port))
       (chan-buf-bar (format "#chan@127.0.0.1:%d<2>" port))
       (expect (erc-d-t-make-expecter))
       (erc-server-reconnect-function #'erc-server-delayed-reconnect)
       (erc-server-auto-reconnect auto)
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"
                                           :id nil))
        (setq erc-server-process-foo erc-server-process)
        (erc-d-t-wait-for 3 (eq (erc-network) 'foonet))
        (erc-d-t-wait-for 3 "Final buffer name determined"
          (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (ert-info ("Join #chan@foonet")
      (with-current-buffer erc-server-buffer-foo (erc-cmd-JOIN "#chan"))
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 5 "<alice>")))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"
                                           :id nil))
        (setq erc-server-process-bar erc-server-process)
        (erc-d-t-wait-for 10 (eq (erc-network) 'barnet))
        (erc-d-t-wait-for 3 "Final buffer name determined"
          (string= (buffer-name) (format "127.0.0.1:%d<2>" port)))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique, no names based on IPs")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar))
      (should (equal (erc-scenarios-common-buflist "127.0.0.1")
                     (list (get-buffer (format "127.0.0.1:%d<2>" port))
                           (get-buffer (format "127.0.0.1:%d" port))))))

    (ert-info ("Join #chan@barnet")
      (with-current-buffer erc-server-buffer-bar (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 5 "Exactly 2 #chan-prefixed buffers exist"
      (equal (list (get-buffer chan-buf-bar)
                   (get-buffer chan-buf-foo))
             (erc-scenarios-common-buflist "#chan")))

    (ert-info ("#chan@127.0.0.1:$port is exclusive to foonet")
      (with-current-buffer chan-buf-foo
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))
        (erc-d-t-search-for 10 "ape is dead")
        (erc-d-t-wait-for 5 (not (erc-server-process-alive)))))

    (ert-info ("#chan@127.0.0.1:$port<2> is exclusive to barnet")
      (with-current-buffer chan-buf-bar
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (should (eq erc-server-process erc-server-process-bar))
        (erc-d-t-search-for 10 "joe: It is a rupture")
        (erc-d-t-wait-for 5 (not (erc-server-process-alive)))))

    (when more (funcall more))))

(ert-deftest erc-scenarios-base-compat-no-rename-bouncer--basic ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-rename-buffers))
    (let (erc-rename-buffers)
      (erc-scenarios-common--base-compat-no-rename-bouncer
       '(foonet barnet) nil nil))))

(ert-deftest erc-scenarios-base-compat-no-rename-bouncer--reconnect ()
  :tags '(:expensive-test)
  (let ((erc-d-tmpl-vars '((token . (group (| "barnet" "foonet")))))
        (erc-d-match-handlers
         (list :pass #'erc-scenarios-common--clash-rename-pass-handler))
        (dialogs '(foonet-drop barnet-drop stub-again stub-again
                               foonet-again barnet-again))
        (after
         (lambda ()
           (pcase-let* ((`(,barnet ,foonet)
                         (erc-scenarios-common-buflist "127.0.0.1"))
                        (port (process-contact (with-current-buffer foonet
                                                 erc-server-process)
                                               :service)))

             (ert-info ("Sanity check: barnet retains uniquifying suffix")
               (should (string-suffix-p "<2>" (buffer-name barnet))))

             ;; Simulate disconnection and `erc-server-auto-reconnect'
             (ert-info ("Reconnect to foonet and barnet back-to-back")
               (with-current-buffer foonet
                 (erc-d-t-wait-for 5 (erc-server-process-alive)))
               (with-current-buffer barnet
                 (erc-d-t-wait-for 5 (erc-server-process-alive))))

             (ert-info ("#chan@127.0.0.1:<port> is exclusive to foonet")
               (with-current-buffer  (format "#chan@127.0.0.1:%d" port)
                 (erc-d-t-search-for 1 "<alice>")
                 (erc-d-t-absent-for 0.1 "<joe>")
                 (erc-d-t-search-for 10 "please your lordship")))

             (ert-info ("#chan@barnet is exclusive to barnet")
               (with-current-buffer  (format "#chan@127.0.0.1:%d<2>" port)
                 (erc-d-t-search-for 1 "<joe>")
                 (erc-d-t-absent-for 0.1 "<bob>")
                 (erc-d-t-search-for 1 "much in private")))

             ;; Ordering deterministic here even though not so for reconnect
             (should (equal (list barnet foonet)
                            (erc-scenarios-common-buflist "127.0.0.1")))
             (should (equal (list
                             (get-buffer (format "#chan@127.0.0.1:%d<2>" port))
                             (get-buffer (format "#chan@127.0.0.1:%d" port)))
                            (erc-scenarios-common-buflist "#chan")))))))

    (with-suppressed-warnings ((obsolete erc-rename-buffers))
      (let (erc-rename-buffers)
        (erc-scenarios-common--base-compat-no-rename-bouncer dialogs
                                                             'auto after)))))

;;; erc-scenarios-base-compat-rename-bouncer.el ends here
