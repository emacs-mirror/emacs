;;; erc-tests.el --- Tests for erc.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'erc-ring)

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))


(ert-deftest erc--read-time-period ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should (equal (erc--read-time-period "foo: ") nil)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should (equal (erc--read-time-period "foo: ") nil)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " 432 ")))
    (should (equal (erc--read-time-period "foo: ") 432)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "432")))
    (should (equal (erc--read-time-period "foo: ") 432)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1h")))
    (should (equal (erc--read-time-period "foo: ") 3600)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1h10s")))
    (should (equal (erc--read-time-period "foo: ") 3610)))

  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1d")))
    (should (equal (erc--read-time-period "foo: ") 86400))))

(ert-deftest erc--format-time-period ()
  (should (equal (erc--format-time-period 59) "59s"))
  (should (equal (erc--format-time-period 59.9) "59s"))
  (should (equal (erc--format-time-period 60) "1m0s"))
  (should (equal (erc--format-time-period 119) "1m59s"))
  (should (equal (erc--format-time-period 119.9) "1m59s"))
  (should (equal (erc--format-time-period 120.9) "2m0s"))
  (should (equal (erc--format-time-period 3599.9) "59m59s"))
  (should (equal (erc--format-time-period 3600) "1h0m0s")))

;; This asserts that the first pattern on file matching a supplied
;; `user' parameter will be removed after confirmation.
(ert-deftest erc-cmd-UNIGNORE ()
  ;; XXX these functions mutate `erc-ignore-list' via `delete'.
  (should (local-variable-if-set-p 'erc-ignore-list))
  (erc-tests-common-make-server-buf)

  (setq erc-ignore-list (list ".")) ; match anything
  (let ((inhibit-message noninteractive))
    (ert-simulate-keys (list ?\r)
      (erc-cmd-IGNORE "abc")))
  (should (equal erc-ignore-list (list "abc" ".")))

  (cl-letf (((symbol-function 'y-or-n-p) #'always))
    (erc-cmd-UNIGNORE "abcdef")
    (should (equal erc-ignore-list (list ".")))
    (erc-cmd-UNIGNORE "foo"))
  (should-not erc-ignore-list))

(ert-deftest erc-with-all-buffers-of-server ()
  (let (proc-exnet
        proc-onet
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer (get-buffer-create "OtherNet")
      (erc-mode)
      (setq proc-onet (start-process "sleep" (current-buffer) "sleep" "1")
            erc-server-process proc-onet
            erc-network 'OtherNet)
      (set-process-query-on-exit-flag erc-server-process nil))

    (with-current-buffer (get-buffer-create "ExampleNet")
      (erc-mode)
      (setq proc-exnet (start-process "sleep" (current-buffer) "sleep" "1")
            erc-server-process proc-exnet
            erc-network 'ExampleNet)
      (set-process-query-on-exit-flag erc-server-process nil))

    (with-current-buffer (get-buffer-create "#foo")
      (erc-mode)
      (setq erc-server-process proc-exnet)
      (setq erc--target (erc--target-from-string "#foo")))

    (with-current-buffer (get-buffer-create "#spam")
      (erc-mode)
      (setq erc-server-process proc-onet)
      (setq erc--target (erc--target-from-string "#spam")))

    (with-current-buffer (get-buffer-create "#bar")
      (erc-mode)
      (setq erc-server-process proc-onet)
      (setq erc--target (erc--target-from-string "#bar")))

    (with-current-buffer (get-buffer-create "#baz")
      (erc-mode)
      (setq erc-server-process proc-exnet)
      (setq erc--target (erc--target-from-string "#baz")))

    (should (eq (get-buffer-process "ExampleNet") proc-exnet))
    (erc-with-all-buffers-of-server (get-buffer-process "ExampleNet") nil
      (kill-buffer))

    (should-not (get-buffer "ExampleNet"))
    (should-not (get-buffer "#foo"))
    (should-not (get-buffer "#baz"))
    (should (get-buffer "OtherNet"))
    (should (get-buffer "#bar"))
    (should (get-buffer "#spam"))

    (let* ((test (lambda () (not (string= (buffer-name) "#spam"))))
           (calls 0)
           (get-test (lambda () (cl-incf calls) test)))

      (erc-with-all-buffers-of-server proc-onet (funcall get-test)
        (kill-buffer))

      (should (= calls 1)))

    (should-not (get-buffer "OtherNet"))
    (should-not (get-buffer "#bar"))
    (should (get-buffer "#spam"))
    (kill-buffer "#spam")))

(ert-deftest erc-with-server-buffer ()
  (setq erc-away 1)
  (erc-tests-common-init-server-proc "sleep" "1")

  (let (mockingp calls)
    (advice-add 'buffer-local-value :after
                (lambda (&rest r) (when mockingp (push r calls)))
                '((name . erc-with-server-buffer)))

    (should (= 1 (prog2 (setq mockingp t)
                     (erc-with-server-buffer erc-away)
                   (setq mockingp nil))))

    (should (equal (pop calls) (list 'erc-away (current-buffer))))

    (should (= 1 (prog2 (setq mockingp t)
                     (erc-with-server-buffer (ignore 'me) erc-away)
                   (setq mockingp nil))))
    (should-not calls)

    (advice-remove 'buffer-local-value 'erc-with-server-buffer)))

(ert-deftest erc-hide-prompt ()
  (let ((erc-hide-prompt erc-hide-prompt)
        (inhibit-message noninteractive)
        ;;
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer (get-buffer-create "ServNet")
      (erc-tests-common-prep-for-insertion)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (erc-tests-common-init-server-proc "sleep" "1")
      (set-process-sentinel erc-server-process #'ignore)
      (setq erc-network 'ServNet)
      (set-process-query-on-exit-flag erc-server-process nil))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-tests-common-prep-for-insertion)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (setq erc-server-process (buffer-local-value 'erc-server-process
                                                   (get-buffer "ServNet"))
            erc--target (erc--target-from-string "#chan")))

    (with-current-buffer (get-buffer-create "bob")
      (erc-tests-common-prep-for-insertion)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (setq erc-server-process (buffer-local-value 'erc-server-process
                                                   (get-buffer "ServNet"))
            erc--target (erc--target-from-string "bob")))

    (ert-info ("Value: t (default)")
      (should (eq erc-hide-prompt t))
      (with-current-buffer "ServNet"
        (should (= (point) erc-insert-marker))
        (erc--hide-prompt erc-server-process)
        (should (string= ">" (get-char-property (point) 'display))))

      (with-current-buffer "#chan"
        (goto-char erc-insert-marker)
        (should (string= ">" (get-char-property (point) 'display)))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (goto-char erc-input-marker)
        (ert-simulate-command '(self-insert-command 1 ?/))
        (goto-char erc-insert-marker)
        (should-not (get-char-property (point) 'display))
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook)))

      (with-current-buffer "bob"
        (goto-char erc-insert-marker)
        (should (string= ">" (get-char-property (point) 'display)))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (goto-char erc-input-marker)
        (ert-simulate-command '(self-insert-command 1 ?/))
        (goto-char erc-insert-marker)
        (should-not (get-char-property (point) 'display))
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook)))

      (with-current-buffer "ServNet"
        (should (get-char-property erc-insert-marker 'display))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (erc--unhide-prompt)
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook))
        (should-not (get-char-property erc-insert-marker 'display))))

    (ert-info ("Value: server")
      (setq erc-hide-prompt '(server))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) 'hidden))
        (should (string= ">" (get-char-property erc-insert-marker 'display))))

      (with-current-buffer "#chan"
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "ServNet"
        (erc--unhide-prompt)
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) t))
        (should-not (get-char-property erc-insert-marker 'display))))

    (ert-info ("Value: channel")
      (setq erc-hide-prompt '(channel))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should (string= ">" (get-char-property erc-insert-marker 'display)))
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) 'hidden))
        (erc--unhide-prompt)
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) t))
        (should-not (get-char-property erc-insert-marker 'display))))

    (ert-info ("Value: query")
      (setq erc-hide-prompt '(query))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should (string= ">" (get-char-property erc-insert-marker 'display)))
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) 'hidden))
        (erc--unhide-prompt)
        (should (eq (get-text-property erc-insert-marker 'erc-prompt) t))
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should-not (get-char-property erc-insert-marker 'display))))

    (ert-info ("Value: nil")
      (setq erc-hide-prompt nil)
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-char-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should-not (get-char-property erc-insert-marker 'display))
        (erc--unhide-prompt) ; won't blow up when prompt already showing
        (should-not (get-char-property erc-insert-marker 'display))))

    (when noninteractive
      (kill-buffer "#chan")
      (kill-buffer "bob")
      (kill-buffer "ServNet"))))

(ert-deftest erc--refresh-prompt ()
  (let* ((counter 0)
         (erc-prompt (lambda ()
                       (format "%s %d>"
                               (erc-format-target-and/or-network)
                               (cl-incf counter))))
         erc-accidental-paste-threshold-seconds
         erc-insert-modify-hook
         erc-send-modify-hook
         (erc-last-input-time 0)
         (erc-modules (remq 'stamp erc-modules))
         (erc-send-input-line-function #'ignore)
         (erc--input-review-functions erc--input-review-functions)
         erc-send-completed-hook)

    (ert-info ("Server buffer")
      (with-current-buffer (get-buffer-create "ServNet")
        (erc-tests-common-make-server-buf "ServNet")
        (goto-char erc-insert-marker)
        (should (looking-at-p "ServNet 3>"))
        (erc-tests-common-init-server-proc "sleep" "1")
        (set-process-sentinel erc-server-process #'ignore)
        (setq erc-server-current-nick "tester")
        ;; Incoming message redraws prompt
        (erc-display-message nil 'notice nil "Welcome")
        (should (looking-at-p (rx "*** Welcome")))
        (forward-line)
        (should (looking-at-p "ServNet 4>"))
        ;; Say something
        (goto-char erc-input-marker)
        (insert "Howdy")
        (erc-send-current-line)
        (save-excursion (forward-line -1)
                        (should (looking-at (rx "*** No target")))
                        (forward-line -1)
                        (should (looking-at "<tester> Howdy")))
        (should (looking-back "ServNet 6> "))
        (should (= erc-input-marker (point)))
        ;; Space after prompt is unpropertized
        (should (get-text-property (1- erc-input-marker) 'erc-prompt))
        (should-not (get-text-property erc-input-marker 'erc-prompt))
        ;; No sign of old prompts
        (save-excursion
          (goto-char (point-min))
          (should-not (search-forward (rx (any "3-5") ">") nil t)))))

    (ert-info ("Channel buffer")
      ;; Create buffer manually instead of using `erc--open-target' in
      ;; order to show prompt before/after network is known.
      (with-current-buffer (get-buffer-create "#chan")
        (erc-tests-common-prep-for-insertion)
        (goto-char erc-insert-marker)
        (should (looking-at-p "#chan 9>"))
        (goto-char erc-input-marker)
        (setq erc-server-process (buffer-local-value 'erc-server-process
                                                     (get-buffer "ServNet"))
              erc-networks--id (erc-with-server-buffer erc-networks--id)
              erc--target (erc--target-from-string "#chan")
              erc-default-recipients (list "#chan")
              erc-channel-users (make-hash-table :test 'equal))
        (erc-update-current-channel-member "alice" "alice")
        (erc-update-current-channel-member "bob" "bob")
        (erc-update-current-channel-member "tester" "tester")
        (erc-display-message nil nil (current-buffer)
                             (erc-format-privmessage "alice" "Hi" nil t))
        (should (looking-back "#chan@ServNet 10> "))
        (goto-char erc-input-marker)
        (insert "Howdy")
        (erc-send-current-line)
        (save-excursion (forward-line -1)
                        (should (looking-at "<tester> Howdy")))
        (should (looking-back "#chan@ServNet 11> "))
        (should (= (point) erc-input-marker))
        (insert "/query bob")
        (let (erc-modules)
          (erc-send-current-line))
        ;; Last command not inserted
        (save-excursion (forward-line -1)
                        (should (looking-at "<tester> Howdy")))
        ;; Query does not redraw (nor /help, only message input)
        (should (looking-back "#chan@ServNet 11> "))
        ;; No sign of old prompts
        (save-excursion
          (goto-char (point-min))
          (should-not (search-forward (rx (or "9" "10") ">") nil t)))))

    (ert-info ("Query buffer")
      (with-current-buffer "bob"
        (goto-char erc-insert-marker)
        (should (looking-at-p "bob@ServNet 14>"))
        (goto-char erc-input-marker)
        (erc-display-message nil nil (current-buffer)
                             (erc-format-privmessage "bob" "Hi" nil t))
        (should (looking-back "bob@ServNet 15> "))
        (goto-char erc-input-marker)
        (insert "Howdy")
        (erc-send-current-line)
        (save-excursion (forward-line -1)
                        (should (looking-at "<tester> Howdy")))
        (should (looking-back "bob@ServNet 16> "))
        ;; No sign of old prompts
        (save-excursion
          (goto-char (point-min))
          (should-not (search-forward (rx (or "14" "15") ">") nil t)))))

    (when noninteractive
      (kill-buffer "#chan")
      (kill-buffer "bob")
      (kill-buffer "ServNet"))))

(ert-deftest erc--initialize-markers ()
  (let ((proc (start-process "true" (current-buffer) "true"))
        erc-modules
        erc-connect-pre-hook
        erc-insert-modify-hook
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (set-process-query-on-exit-flag proc nil)
    (erc-mode)
    (setq erc-server-process proc
          erc-networks--id (erc-networks--id-create 'foonet))
    (erc-open "localhost" 6667 "tester" "Tester" nil
              "fake" nil "#chan" proc nil "user" nil)
    (with-current-buffer (should (get-buffer "#chan"))
      (should (= ?\n (char-after 1)))
      (should (= ?E (char-after erc-insert-marker)))
      (should (= 3 (marker-position erc-insert-marker)))
      (should (= 8 (marker-position erc-input-marker)))
      (should (= 8 (point-max)))
      (should (= 8 (point)))
      ;; These prompt properties are a continual source of confusion.
      ;; Including the literal defaults here can hopefully serve as a
      ;; quick reference for anyone operating in that area.
      (should (equal (buffer-string)
                     #("\n\nERC> "
                       2 6 ( font-lock-face erc-prompt-face
                             rear-nonsticky t
                             erc-prompt t
                             field erc-prompt
                             front-sticky t
                             read-only t)
                       6 7 ( rear-nonsticky t
                             erc-prompt t
                             field erc-prompt
                             front-sticky t
                             read-only t))))

      ;; Simulate some activity by inserting some text before and
      ;; after the prompt (multiline).
      (erc-display-error-notice nil "Welcome")
      (goto-char (point-max))
      (insert "Hello\nWorld")
      (goto-char 3)
      (should (looking-at-p (regexp-quote "*** Welcome"))))

    (ert-info ("Reconnect")
      (with-current-buffer (erc-server-buffer)
        (erc-open "localhost" 6667 "tester" "Tester" nil
                  "fake" nil "#chan" proc nil "user" nil))
      (should-not (get-buffer "#chan<2>")))

    (ert-info ("Existing prompt respected")
      (with-current-buffer (should (get-buffer "#chan"))
        (should (= ?\n (char-after 1)))
        (should (= ?E (char-after erc-insert-marker)))
        (should (= 15 (marker-position erc-insert-marker)))
        (should (= 20 (marker-position erc-input-marker)))
        (should (= 3 (point))) ; point restored
        (should (equal (buffer-string)
                       #("\n\n*** Welcome\nERC> Hello\nWorld"
                         2 13 (font-lock-face erc-error-face)
                         14 18 ( font-lock-face erc-prompt-face
                                 rear-nonsticky t
                                 erc-prompt t
                                 field erc-prompt
                                 front-sticky t
                                 read-only t)
                         18 19 ( rear-nonsticky t
                                 erc-prompt t
                                 field erc-prompt
                                 front-sticky t
                                 read-only t))))
        (when noninteractive
          (kill-buffer))))))

(ert-deftest erc--switch-to-buffer ()
  (defvar erc-modified-channels-alist) ; lisp/erc/erc-track.el

  (let ((proc (start-process "aNet" (current-buffer) "true"))
        (erc-modified-channels-alist `(("fake") (,(messages-buffer))))
        (inhibit-message noninteractive)
        (completion-fail-discreetly t) ; otherwise ^G^G printed to .log file
        ;;
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer (get-buffer-create "server")
      (erc-mode)
      (set-process-buffer (setq erc-server-process proc) (current-buffer))
      (set-process-query-on-exit-flag erc-server-process nil)
      (with-current-buffer (get-buffer-create "#chan")
        (erc-mode)
        (setq erc-server-process proc))
      (with-current-buffer (get-buffer-create "#foo")
        (erc-mode)
        (setq erc-server-process proc))

      (ert-info ("Channel #chan selectable from server buffer")
        (ert-simulate-keys (list ?# ?c ?h ?a ?n ?\C-m)
          (should (string= "#chan" (erc--switch-to-buffer))))))

    (ert-info ("Channel #foo selectable from non-ERC buffer")
      (ert-simulate-keys (list ?# ?f ?o ?o ?\C-m)
        (should (string= "#foo" (erc--switch-to-buffer)))))

    (ert-info ("Default selectable")
      (ert-simulate-keys (list ?\C-m)
        (should (string= "*Messages*" (erc--switch-to-buffer)))))

    (ert-info ("Extant but non-ERC buffer not selectable")
      (get-buffer-create "#fake") ; not ours
      (ert-simulate-keys (kbd "#fake C-m C-a C-k C-m")
        ;; Initial query fails ~~~~~~^; clearing input accepts default
        (should (string= "*Messages*" (erc--switch-to-buffer)))))

    (with-current-buffer (get-buffer-create "other")
      (erc-mode)
      (setq erc-server-process (start-process "bNet" (current-buffer) "true"))
      (set-process-query-on-exit-flag erc-server-process nil))

    (ert-info ("Foreign ERC buffer not selectable")
      (ert-simulate-keys (kbd "other C-m C-a C-k C-m")
        (with-current-buffer "server"
          (should (string= "*Messages*" (erc--switch-to-buffer))))))

    (ert-info ("Any ERC-buffer selectable from non-ERC buffer")
      (should-not (eq major-mode 'erc-mode))
      (ert-simulate-keys (list ?o ?t ?h ?e ?r ?\C-m)
        (should (string= "other" (erc--switch-to-buffer)))))

    (dolist (b '("server" "other" "#chan" "#foo" "#fake"))
      (kill-buffer b))))

(ert-deftest erc-setup-buffer--custom-action ()
  (erc-mode)
  (erc-tests-common-init-server-proc "sleep" "1")
  (setq erc--server-last-reconnect-count 0)
  (let ((owin (selected-window))
        (obuf (window-buffer))
        (mbuf (messages-buffer))
        calls)
    (cl-letf (((symbol-function 'switch-to-buffer) ; regression
               (lambda (&rest r) (push (cons 'switch-to-buffer r) calls)))
              ((symbol-function 'erc--test-fun)
               (lambda (&rest r) (push (cons 'erc--test-fun r) calls)))
              ((symbol-function 'display-buffer)
               (lambda (&rest r) (push (cons 'display-buffer r) calls))))

      ;; Baseline
      (let ((erc-join-buffer 'bury))
        (erc-setup-buffer mbuf)
        (should-not calls))

      (should-not erc--display-context)

      ;; `display-buffer'
      (let ((erc--display-context '((erc-buffer-display . 1)))
            (erc-join-buffer 'erc--test-fun))
        (erc-setup-buffer mbuf)
        (should (equal `(erc--test-fun ,mbuf (nil (erc-buffer-display . 1)))
                       (pop calls)))
        (should-not calls))

      ;; `pop-to-buffer' with `erc-auto-reconnect-display'
      (let* ((erc--server-last-reconnect-count 1)
             (erc--display-context '((erc-buffer-display . 1)))
             (erc-auto-reconnect-display 'erc--test-fun))
        (erc-setup-buffer mbuf)
        (should (equal `(erc--test-fun ,mbuf
                                       (nil (erc-auto-reconnect-display . t)
                                            (erc-buffer-display . 1)))
                       (pop calls)))
        (should-not calls)))

    ;; Mimic simplistic version of example in "(erc) display-buffer".
    (when (>= emacs-major-version 29)
      (let ((proc erc-server-process))
        (with-temp-buffer
          (should-not (eq (window-buffer) (current-buffer)))
          (erc-mode)
          (setq erc-server-process proc)

          (cl-letf (((symbol-function 'erc--test-fun-p)
                     (lambda (buf action)
                       (should (eql 1 (alist-get 'erc-buffer-display action)))
                       (push (cons 'erc--test-fun-p buf) calls)))
                    ((symbol-function 'action-fn)
                     (lambda (buf action)
                       (should (eql 1 (alist-get 'erc-buffer-display action)))
                       (should (eql 42 (alist-get 'foo action)))
                       (push (cons 'action-fn buf) calls)
                       (selected-window))))

            (let ((erc--display-context '((erc-buffer-display . 1)))
                  (display-buffer-alist
                   `(((and (major-mode . erc-mode) erc--test-fun-p)
                      action-fn (foo . 42))))
                  (erc-buffer-display 'display-buffer))

              (erc-setup-buffer (current-buffer))
              (should (equal 'action-fn (car (pop calls))))
              (should (equal 'erc--test-fun-p (car (pop calls))))
              (should-not calls))))))

    (should (eq owin (selected-window)))
    (should (eq obuf (window-buffer)))))

(ert-deftest erc-lurker-maybe-trim ()
  (let (erc-lurker-trim-nicks
        (erc-lurker-ignore-chars "_`"))

    (should (string= "nick`" (erc-lurker-maybe-trim "nick`")))

    (setq erc-lurker-trim-nicks t)
    (should (string= "nick" (erc-lurker-maybe-trim "nick`")))
    (should (string= "ni`_ck" (erc-lurker-maybe-trim "ni`_ck__``")))

    (setq erc-lurker-ignore-chars "_-`") ; set of chars, not character alts
    (should (string= "nick" (erc-lurker-maybe-trim "nick-_`")))))

(ert-deftest erc-parse-user ()
  (should (equal '("" "" "") (erc-parse-user "!@")))
  (should (equal '("" "!" "") (erc-parse-user "!!@")))
  (should (equal '("" "" "@") (erc-parse-user "!@@")))
  (should (equal '("" "!" "@") (erc-parse-user "!!@@")))

  (should (equal '("abc" "" "") (erc-parse-user "abc")))
  (should (equal '("" "123" "fake") (erc-parse-user "!123@fake")))
  (should (equal '("abc" "" "123") (erc-parse-user "abc!123")))

  (should (equal '("abc" "123" "fake") (erc-parse-user "abc!123@fake")))
  (should (equal '("abc" "!123" "@xy") (erc-parse-user "abc!!123@@xy")))

  (should (equal '("de" "fg" "xy") (erc-parse-user "abc\nde!fg@xy"))))

(ert-deftest erc--parse-nuh ()
  (should (equal '(nil nil nil) (erc--parse-nuh "!@")))
  (should (equal '(nil nil nil) (erc--parse-nuh "@")))
  (should (equal '(nil nil nil) (erc--parse-nuh "!")))
  (should (equal '(nil "!" nil) (erc--parse-nuh "!!@")))
  (should (equal '(nil "@" nil) (erc--parse-nuh "!@@")))
  (should (equal '(nil "!@" nil) (erc--parse-nuh "!!@@")))

  (should (equal '("abc" nil nil) (erc--parse-nuh "abc!")))
  (should (equal '(nil "abc" nil) (erc--parse-nuh "abc@")))
  (should (equal '(nil "abc" nil) (erc--parse-nuh "!abc@")))

  (should (equal '("abc" "123" "fake") (erc--parse-nuh "abc!123@fake")))
  (should (equal '("abc" "!123@" "xy") (erc--parse-nuh "abc!!123@@xy")))

  ;; Missing leading components.
  (should (equal '(nil "abc" "123") (erc--parse-nuh "abc@123")))
  (should (equal '(nil "123" "fake") (erc--parse-nuh "!123@fake")))
  (should (equal '(nil nil "gnu.org") (erc--parse-nuh "@gnu.org")))

  ;; Host "wins" over nick and user (sans "@").
  (should (equal '(nil nil "abc") (erc--parse-nuh "abc")))
  (should (equal '(nil nil "gnu.org") (erc--parse-nuh "gnu.org")))
  (should (equal '(nil nil "gnu.org") (erc--parse-nuh "!gnu.org")))
  (should (equal '("abc" nil "123") (erc--parse-nuh "abc!123")))

  ;; No fallback behavior.
  (should-not (erc--parse-nuh "abc\nde!fg@xy")))

(ert-deftest erc--parsed-prefix ()
  ;; Effectively a no-op in a non-ERC buffer.
  (should-not (erc--parsed-prefix))

  (erc-tests-common-make-server-buf (buffer-name))

  ;; Uses fallback values when no PREFIX parameter yet received, thus
  ;; ensuring caller can use slot accessors immediately instead of
  ;; checking if null beforehand.
  (should-not erc--parsed-prefix)
  (should (equal (erc--parsed-prefix)
                 #s(erc--parsed-prefix nil "vhoaq" "+%@&~"
                                       ((?q . ?~) (?a . ?&)
                                        (?o . ?@) (?h . ?%) (?v . ?+)))))
  (let ((cached (should erc--parsed-prefix)))
    (should (eq (erc--parsed-prefix) cached)))

  ;; Cache broken.  (Notice not setting `erc--parsed-prefix' to nil).
  (setq erc-server-parameters '(("PREFIX" . "(ov)@+")))

  (let ((proc erc-server-process)
        (expected '((?o . ?@) (?v . ?+)))
        cached)

    (with-temp-buffer
      (erc-mode)
      (setq erc-server-process proc)
      (should (equal expected
                     (erc--parsed-prefix-alist (erc--parsed-prefix)))))

    (should (equal expected (erc--parsed-prefix-alist erc--parsed-prefix)))
    (setq cached erc--parsed-prefix)
    (should (equal cached
                   #s(erc--parsed-prefix ("(ov)@+") "vo" "+@"
                                         ((?o . ?@) (?v . ?+)))))
    ;; Second target buffer reuses cached value.
    (with-temp-buffer
      (erc-mode)
      (setq erc-server-process proc)
      (should (eq cached (erc--parsed-prefix))))

    ;; New value computed when cache broken.
    (puthash 'PREFIX (list "(qh)~%") erc--isupport-params)
    (with-temp-buffer
      (erc-mode)
      (setq erc-server-process proc)
      (should-not (eq cached (erc--parsed-prefix)))
      (should (equal (erc--parsed-prefix-alist
                      (erc-with-server-buffer erc--parsed-prefix))
                     '((?q . ?~) (?h . ?%)))))))

(ert-deftest erc--get-prefix-flag ()
  (erc-tests-common-make-server-buf (buffer-name))
  (should-not erc--parsed-prefix)
  (should (= (erc--get-prefix-flag ?v) 1))
  (should (= (erc--get-prefix-flag ?h) 2))
  (should (= (erc--get-prefix-flag ?o) 4))
  (should (= (erc--get-prefix-flag ?a) 8))
  (should (= (erc--get-prefix-flag ?q) 16))

  (ert-info ("With optional `from-prefix-p'")
    (should (= (erc--get-prefix-flag ?+ nil 'fpp) 1))
    (should (= (erc--get-prefix-flag ?% nil 'fpp) 2))
    (should (= (erc--get-prefix-flag ?@ nil 'fpp) 4))
    (should (= (erc--get-prefix-flag ?& nil 'fpp) 8))
    (should (= (erc--get-prefix-flag ?~ nil 'fpp) 16)))
  (should erc--parsed-prefix))

(ert-deftest erc--init-cusr-fallback-status ()
  ;; Fallback behavior active because no `erc--parsed-prefix'.
  (should-not erc--parsed-prefix)
  (should (= 0 (erc--init-cusr-fallback-status nil nil nil nil nil)))
  (should (= 1 (erc--init-cusr-fallback-status t nil nil nil nil)))
  (should (= 4 (erc--init-cusr-fallback-status nil nil t nil nil)))
  (should-not erc--parsed-prefix) ; not created in non-ERC buffer.

  ;; Uses advertised server parameter.
  (erc-tests-common-make-server-buf (buffer-name))
  (setq erc-server-parameters '(("PREFIX" . "(YqaohvV)!~&@%+-")))
  (should (= 0 (erc--init-cusr-fallback-status nil nil nil nil nil)))
  (should (= 2 (erc--init-cusr-fallback-status t nil nil nil nil)))
  (should (= 8 (erc--init-cusr-fallback-status nil nil t nil nil)))
  (should erc--parsed-prefix))

(ert-deftest erc--compute-cusr-fallback-status ()
  ;; Useless without an `erc--parsed-prefix'.
  (should (= 0 (erc--compute-cusr-fallback-status 0 nil nil nil nil nil)))
  (should (= 0 (erc--compute-cusr-fallback-status 0 'on 'on 'on 'on 'on)))

  (erc-tests-common-make-server-buf (buffer-name))
  (should (= 0 (erc--compute-cusr-fallback-status 0 nil nil nil nil nil)))
  (should (= 1 (erc--compute-cusr-fallback-status 0 'on nil nil nil nil)))
  (should (= 1 (erc--compute-cusr-fallback-status 0 'on 'off 'off 'off 'off)))
  (should (= 1 (erc--compute-cusr-fallback-status 1 'on 'off 'off 'off 'off)))
  (should (= 1 (erc--compute-cusr-fallback-status 1 nil nil nil nil nil)))
  (should (= 1 (erc--compute-cusr-fallback-status 3 nil 'off nil nil nil)))
  (should (= 1 (erc--compute-cusr-fallback-status 7 nil 'off 'off nil nil)))
  (should (= 4 (erc--compute-cusr-fallback-status 1 'off nil 'on nil nil))))

(ert-deftest erc--cusr-status-p ()
  (erc-tests-common-make-server-buf (buffer-name))
  (should-not erc--parsed-prefix)
  (let ((cusr (make-erc-channel-user :voice t :op t)))
    (should-not (erc--cusr-status-p cusr ?q))
    (should-not (erc--cusr-status-p cusr ?a))
    (should-not (erc--cusr-status-p cusr ?h))
    (should (erc--cusr-status-p cusr ?o))
    (should (erc--cusr-status-p cusr ?v)))
  (should erc--parsed-prefix))

(ert-deftest erc--cusr-change-status ()
  (erc-tests-common-make-server-buf (buffer-name))
  (let ((cusr (make-erc-channel-user)))
    (should-not (erc--cusr-status-p cusr ?o))
    (should-not (erc--cusr-status-p cusr ?v))
    (erc--cusr-change-status cusr ?o t)
    (erc--cusr-change-status cusr ?v t)
    (should (erc--cusr-status-p cusr ?o))
    (should (erc--cusr-status-p cusr ?v))

    (ert-info ("Reset with optional param")
      (erc--cusr-change-status cusr ?q t 'reset)
      (should-not (erc--cusr-status-p cusr ?o))
      (should-not (erc--cusr-status-p cusr ?v))
      (should (erc--cusr-status-p cusr ?q)))

    (ert-info ("Clear with optional param")
      (erc--cusr-change-status cusr ?v t)
      (should (erc--cusr-status-p cusr ?v))
      (erc--cusr-change-status cusr ?q nil 'reset)
      (should-not (erc--cusr-status-p cusr ?v))
      (should-not (erc--cusr-status-p cusr ?q)))))

(defun erc-tests--make-combinations (flags)
  "Return a list of all combinations in FLAGS, preserving order."
  (let* ((flags (apply #'vector flags))
         (n (length flags))
         (max (1- (ash 1 n)))
         (mask 0)
         (out ()))
    (while (<= (cl-incf mask) max)
      (let ((i 0)
            (subset ()))
        (while (< i n)
          (unless (zerop (logand mask (ash 1 i)))
            (push (aref flags i) subset))
          (cl-incf i))
        (when (cdr subset)
          (push (nreverse subset) out))))
    out))

(defun erc-tests--with-channel-user-status-accessors (test)
  (erc-tests-common-make-server-buf)

  (let* ((u (make-erc-channel-user))
         (get (lambda (letter)
                (pcase letter
                  (?v (erc-channel-user-voice u))
                  (?h (erc-channel-user-halfop u))
                  (?o (erc-channel-user-op u))
                  (?a (erc-channel-user-admin u))
                  (?q (erc-channel-user-owner u)))))

         (set (lambda (letter val)
                (pcase letter
                  (?v (setf (erc-channel-user-voice u) val))
                  (?h (setf (erc-channel-user-halfop u) val))
                  (?o (setf (erc-channel-user-op u) val))
                  (?a (setf (erc-channel-user-admin u) val))
                  (?q (setf (erc-channel-user-owner u) val)))))

         (assert-null
          (lambda (&rest letters)
            (dolist (letter letters)
              (ert-info ((format "Assert null: %c" letter))
                (should-not (funcall get letter))))))

         (assert-set
          (lambda (letter &optional nop)
            (ert-info ((format "Assert: %c%s" letter (if nop " (no-op)" "")))
              (should-not (funcall get letter))
              (if (and nop erc-channel-user-signal-if-status-unknown)
                  (should-error (funcall set letter t))
                ;; If the flag is unsupported, always return nil,
                ;; and don't set anything, otherwise, return t.
                (let ((rv (funcall set letter t)))
                  (ert-info ((format "Set: %S" rv))
                    (should (xor rv nop)))))
              (let ((rv (funcall get letter)))
                (ert-info ((format "Get: %S" rv))
                  (should (xor rv nop)))))))

         (assert-solo
          (lambda (letter &optional nop)
            (setf (erc-channel-user-status u) 0) ; clear
            (funcall assert-set letter nop)
            (apply assert-null (seq-difference '(?v ?h ?o ?a ?q)
                                               (list letter)))))

         (assert-multi
          (lambda (&rest supported)
            ;; Set all defined flags from smallest to largest rank.
            (dolist (flags (erc-tests--make-combinations '(?v ?h ?o ?a ?q)))
              (setf (erc-channel-user-status u) 0)
              (ert-info ((let ((print-integers-as-characters t))
                           (format "Multi %S" (list :flags flags
                                                    :supported supported))))
                (let ((seen-supported ())
                      (seen-unsupported ()))
                  (dolist (a flags)
                    (let ((supportedp (memq a supported)))
                      (push a (if supportedp seen-supported seen-unsupported))
                      (funcall assert-set a (not supportedp))
                      ;; Addition of new flag has not corrupted others.
                      (dolist (aa seen-supported)
                        (ert-info ((format "Seen supported: %s %c" u aa))
                          (should (funcall get aa))))
                      (dolist (aa `(,@seen-unsupported ,@(cdr (memq a flags))))
                        (should-not (funcall get aa))))))
                ;; Unset in reverse, although not doing so is valid.
                (setq flags (nreverse flags))
                (let ((seen ()))
                  (while-let ((b (pop flags)))
                    (ert-info ((format "Unsetting: %S %c" u b))
                      (should-not (funcall set b nil))
                      (dolist (bb (push b seen))
                        (ert-info ((format "Seen unset: %c" bb))
                          (should-not (funcall get bb))))
                      (dolist (bb flags)
                        (ert-info ((format "Unseen set: %c" bb))
                          (if (memq bb supported)
                              (should (funcall get bb))
                            (should-not (funcall get bb)))))))))))))

    ;; Run the same test twice, with compat flag nil and non-nil.
    (let ((erc-channel-user-signal-if-status-unknown nil))
      (funcall test assert-null assert-set assert-solo assert-multi))

    (ert-info ("With `erc-channel-user-signal-if-status-unknown'")
      (setf (erc-channel-user-status u) 0) ; clear
      (let ((erc-channel-user-signal-if-status-unknown t))
        (funcall test assert-null assert-set assert-solo assert-multi)))

    (erc-tests-common-kill-buffers)))

(ert-deftest erc-channel-user/status-accessors/solo/default ()
  (erc-tests--with-channel-user-status-accessors
   (lambda (assert-null _assert-set assert-solo _assert-multi)

     (ert-info ("Baseline")
       (funcall assert-null ?v ?h ?o ?a ?q))

     (ert-info ("+v") (funcall assert-solo ?v))
     (ert-info ("+h") (funcall assert-solo ?h))
     (ert-info ("+o") (funcall assert-solo ?o))
     (ert-info ("+a") (funcall assert-solo ?a))
     (ert-info ("+q") (funcall assert-solo ?q)))))

(ert-deftest erc-channel-user/status-accessors/solo/ov ()
  (erc-tests--with-channel-user-status-accessors
   (lambda (assert-null _assert-set assert-solo _assert-multi)
     (erc-tests-common-simulate-line ":irc.gnu.org 005 tester PREFIX=(ov)@+")

     (ert-info ("Baseline")
       (funcall assert-null ?v ?h ?o ?a ?q))

     (ert-info ("+v") (funcall assert-solo ?v))
     (ert-info ("+h (unknown)") (funcall assert-solo ?h 'nop))
     (ert-info ("+o") (funcall assert-solo ?o))
     (ert-info ("+a (unknown)") (funcall assert-solo ?a 'nop))
     (ert-info ("+q (unknown)") (funcall assert-solo ?q 'nop)))))

(ert-deftest erc-channel-user/status-accessors/multi/default ()
  (erc-tests--with-channel-user-status-accessors
   (lambda (_assert-null _assert-set _assert-solo assert-multi)
     (funcall assert-multi ?v ?h ?o ?a ?q))))

(ert-deftest erc-channel-user/status-accessors/multi/ov ()
  (erc-tests--with-channel-user-status-accessors
   (lambda (_assert-null _assert-set _assert-solo assert-multi)
     (erc-tests-common-simulate-line ":irc.gnu.org 005 tester PREFIX=(ov)@+")
     (funcall assert-multi ?v ?o))))

;; This exists as a reference to assert legacy behavior in order to
;; preserve and incorporate it as a fallback in the 5.6+ replacement.
(ert-deftest erc-parse-modes ()
  (with-suppressed-warnings ((obsolete erc-parse-modes))
    (should (equal (erc-parse-modes "+u") '(("u") nil nil)))
    (should (equal (erc-parse-modes "-u") '(nil ("u") nil)))
    (should (equal (erc-parse-modes "+o bob") '(nil nil (("o" on "bob")))))
    (should (equal (erc-parse-modes "-o bob") '(nil nil (("o" off "bob")))))
    (should (equal (erc-parse-modes "+uo bob") '(("u") nil (("o" on "bob")))))
    (should (equal (erc-parse-modes "+o-u bob") '(nil ("u") (("o" on "bob")))))

    (should (equal (erc-parse-modes "+uo-tv bob alice")
                   '(("u") ("t") (("o" on "bob") ("v" off "alice")))))
    (should (equal (erc-parse-modes "+u-t+o-v bob alice")
                   '(("u") ("t") (("o" on "bob") ("v" off "alice")))))

    (ert-info ("Modes of type B are always grouped as unary")
      (should (equal (erc-parse-modes "+k h2") '(nil nil (("k" on "h2")))))
      ;; Channel key args are thrown away.
      (should (equal (erc-parse-modes "-k *") '(nil nil (("k" off nil))))))

    (ert-info ("Modes of type C are grouped as unary even when disabling")
      (should (equal (erc-parse-modes "+l 3") '(nil nil (("l" on "3")))))
      (should (equal (erc-parse-modes "-l") '(nil nil (("l" off nil))))))))

(ert-deftest erc--update-channel-modes ()
  (erc-tests-common-make-server-buf)
  (setq erc-channel-users (make-hash-table :test #'equal)
        erc--target (erc--target-from-string "#test"))

  (cl-letf ((calls ())
            ((symbol-function 'erc-update-mode-line) #'ignore))
    (advice-add 'erc--handle-channel-mode
                :before (lambda (&rest r) (push r calls))
                '((name . erc-tests-spy)))

    (ert-info ("Unknown user not created")
      (erc--update-channel-modes "+o" "bob")
      (should-not (erc-get-channel-user "bob")))

    (ert-info ("Status updated when user known")
      (puthash "bob" (cons (erc-add-server-user
                            "bob" (make-erc-server-user
                                   :nickname "bob"
                                   :buffers (list (current-buffer))))
                           (make-erc-channel-user))
               erc-channel-users)
      ;; Also asserts fallback behavior for traditional prefixes.
      (should-not (erc-channel-user-op-p "bob"))
      (erc--update-channel-modes "+o" "bob")
      (should (erc-channel-user-op-p "bob"))
      (erc--update-channel-modes "-o" "bob") ; status revoked
      (should-not (erc-channel-user-op-p "bob")))

    (ert-info ("Unknown nullary added and removed")
      (should-not erc--channel-modes)
      (should-not erc-channel-modes)
      (erc--update-channel-modes "+u")
      (should (equal erc-channel-modes '("u")))
      (should (eq t (gethash ?u erc--channel-modes)))
      (should (equal (pop calls) '(?d ?u t nil)))
      (erc--update-channel-modes "-u")
      (should (equal (pop calls) '(?d ?u nil nil)))
      (should-not (gethash ?u erc--channel-modes))
      (should-not erc-channel-modes)
      (should-not calls))

    (ert-info ("Fallback for Type B includes mode letter k")
      (erc--update-channel-modes "+k" "h2")
      (should (equal (pop calls) '(?b ?k t "h2")))
      (should-not erc-channel-modes)
      (should (equal "h2" (gethash ?k erc--channel-modes)))
      (erc--update-channel-modes "-k" "*")
      (should (equal (pop calls) '(?b ?k nil "*")))
      (should-not calls)
      (should-not (gethash ?k erc--channel-modes))
      (should-not erc-channel-modes))

    (ert-info ("Fallback for Type C includes mode letter l")
      (erc--update-channel-modes "+l" "3")
      (should (equal (pop calls) '(?c ?l t "3")))
      (should-not erc-channel-modes)
      (should (equal "3" (gethash ?l erc--channel-modes)))
      (erc--update-channel-modes "-l" nil)
      (should (equal (pop calls) '(?c ?l nil nil)))
      (should-not (gethash ?l erc--channel-modes))
      (should-not erc-channel-modes))

    (ert-info ("Advertised supersedes heuristics")
      (setq erc-server-parameters
            '(("PREFIX" . "(ov)@+")
              ;; Add phony 5th type for this CHANMODES value for
              ;; robustness in case some server gets creative.
              ("CHANMODES" . "eIbq,k,flj,CFLMPQRSTcgimnprstuz,FAKE")))
      (erc--update-channel-modes "+qu" "fool!*@*")
      (should (equal (pop calls) '(?d ?u t nil)))
      (should (equal (pop calls) '(?a ?q t "fool!*@*")))
      (should (equal 1 (gethash ?q erc--channel-modes)))
      (should (eq t (gethash ?u erc--channel-modes)))
      (should (equal erc-channel-modes '("u")))
      (should-not (erc-channel-user-owner-p "bob"))

      ;; Remove fool!*@* from list mode "q".
      (erc--update-channel-modes "-uq" "fool!*@*")
      (should (equal (pop calls) '(?a ?q nil "fool!*@*")))
      (should (equal (pop calls) '(?d ?u nil nil)))
      (should-not (gethash ?u erc--channel-modes))
      (should-not erc-channel-modes)
      (should (equal 0 (gethash ?q erc--channel-modes))))

    (should-not calls)
    (advice-remove 'erc--handle-channel-mode 'erc-tests-spy)))

(ert-deftest erc--channel-modes ()
  ;; Only mark :unstable when running locally.
  :tags (and (null (getenv "CI")) (null (getenv "EMACS_EMBA_CI")) '(:unstable))

  (setq erc--isupport-params (make-hash-table)
        erc--target (erc--target-from-string "#test")
        erc--channel-banlist-synchronized-p t
        erc-server-parameters
        '(("CHANMODES" . "eIbq,k,flj,CFLMPQRSTcgimnprstuz")))

  (erc-tests-common-init-server-proc "sleep" "1")

  (cl-letf ((erc--parsed-response (make-erc-response
                                   :sender "chop!~u@gnu.org"))
            ((symbol-function 'erc-update-mode-line) #'ignore))
    (should-not erc-channel-banlist)
    (erc--update-channel-modes "+bbltk" "fool!*@*" "spam!*@*" "3" "h2")
    (should (equal erc-channel-banlist '(("chop!~u@gnu.org" . "spam!*@*")
                                         ("chop!~u@gnu.org" . "fool!*@*")))))

  (should (equal (erc--channel-modes 'string) "klt"))
  (should (equal (erc--channel-modes 'strings) '("k" "l" "t")))
  (should (equal (erc--channel-modes) '((?k . "h2") (?l . "3") (?t))))
  (should (equal (erc--channel-modes 3 ",") "klt h2,3"))

  ;; The function this tests behaves differently in different
  ;; environments.  For example, on one GNU Linux system, it returns
  ;; truncation ellipsis when run interactively.  Rather than have
  ;; hard-to-read "nondeterministic" comparisons against sets of
  ;; acceptable values, we use separate tests.
  (when (char-displayable-p ?…) (ert-pass))

  ;; Truncation cache populated and used.
  (let ((cache (erc--channel-mode-types-shortargs erc--channel-mode-types))
        first-run)
    (should (zerop (hash-table-count cache)))
    (should (equal (erc--channel-modes 1 ",") "klt h,3"))
    (should (equal (setq first-run (map-pairs cache)) '(((1 ?k "h2") . "h"))))

    ;; Second call uses cache.
    (cl-letf (((symbol-function 'truncate-string-to-width)
               (lambda (&rest _) (ert-fail "Shouldn't run"))))
      (should (equal (erc--channel-modes 1 ",") "klt h,3")))

    ;; Same key for only entry matches that of first result.
    (should (pcase (map-pairs cache)
              ((and '(((1 ?k "h2") . "h")) second-run)
               (eq (pcase first-run (`((,k . ,_)) k))
                   (pcase second-run (`((,k . ,_)) k)))))))

  (should (equal (erc--channel-modes 0 ",") "klt ,"))
  (should (equal (erc--channel-modes 2) "klt h2 3"))
  (should (equal (erc--channel-modes 1) "klt h 3"))
  (should (equal (erc--channel-modes 0) "klt  "))) ; 2 spaces

(ert-deftest erc--channel-modes/graphic-p ()
  :tags `(:unstable ,@(and (getenv "ERC_TESTS_GRAPHICAL")
                           '(:erc--graphical)))
  (unless (char-displayable-p ?…) (ert-skip "See non-/graphic-p variant"))

  (erc-tests-common-init-server-proc "sleep" "1")
  (setq erc--isupport-params (make-hash-table)
        erc--target (erc--target-from-string "#test")
        erc--channel-banlist-synchronized-p t
        erc-server-parameters
        '(("CHANMODES" . "eIbq,k,flj,CFLMPQRSTcgimnprstuz")))

  (cl-letf ((erc--parsed-response (make-erc-response
                                   :sender "chop!~u@gnu.org"))
            ((symbol-function 'erc-update-mode-line) #'ignore))
    (should-not erc-channel-banlist)
    (erc--update-channel-modes "+bltk" "fool!*@*" "3" "hun2")
    (should (equal erc-channel-banlist '(("chop!~u@gnu.org" . "fool!*@*")))))

  ;; Truncation cache populated and used.
  (let ((cache (erc--channel-mode-types-shortargs erc--channel-mode-types))
        first-run)
    (should (zerop (hash-table-count cache)))
    (should (equal (erc--channel-modes 2 ",") "klt h…,3" ))
    (should (equal (setq first-run (map-pairs cache))
                   '(((2 ?k "hun2") . "h…"))))

    ;; Second call uses cache.
    (cl-letf (((symbol-function 'truncate-string-to-width)
               (lambda (&rest _) (ert-fail "Shouldn't run"))))
      (should (equal (erc--channel-modes 2 ",") "klt h…,3" )))

    ;; Same key for only entry matches that of first result.
    (should (pcase (map-pairs cache)
              ((and `(((2 ?k "hun2") . "h…")) second-run)
               (eq (pcase first-run (`((,k . ,_)) k))
                   (pcase second-run (`((,k . ,_)) k)))))))

  ;; A max length of 0 is nonsensical anyway, so skip those.
  (should (equal (erc--channel-modes 3) "klt hu… 3"))
  (should (equal (erc--channel-modes 2) "klt h… 3"))
  (should (equal (erc--channel-modes 1) "klt … 3")))

(ert-deftest erc--update-user-modes ()
  (let ((erc--user-modes (list ?a)))
    (should (equal (erc--update-user-modes "+a") '(?a)))
    (should (equal (erc--update-user-modes "-b") '(?a)))
    (should (equal erc--user-modes '(?a))))

  (let ((erc--user-modes (list ?b)))
    (should (equal (erc--update-user-modes "+ac") '(?a ?b ?c)))
    (should (equal (erc--update-user-modes "+a-bc") '(?a)))
    (should (equal erc--user-modes '(?a)))))

(ert-deftest erc--user-modes ()
  (let ((erc--user-modes '(?a ?b)))
    (should (equal (erc--user-modes) '(?a ?b)))
    (should (equal (erc--user-modes 'string) "ab"))
    (should (equal (erc--user-modes 'strings) '("a" "b")))))

(ert-deftest erc--parse-user-modes ()
  (should (equal (erc--parse-user-modes "a" '(?a)) '(() ())))
  (should (equal (erc--parse-user-modes "+a" '(?a)) '(() ())))
  (should (equal (erc--parse-user-modes "a" '()) '((?a) ())))
  (should (equal (erc--parse-user-modes "+a" '()) '((?a) ())))
  (should (equal (erc--parse-user-modes "-a" '()) '(() ())))
  (should (equal (erc--parse-user-modes "-a" '(?a)) '(() (?a))))

  (should (equal (erc--parse-user-modes "+a-b" '(?a)) '(() ())))
  (should (equal (erc--parse-user-modes "+a-b" '(?b)) '((?a) (?b))))
  (should (equal (erc--parse-user-modes "+ab-c" '(?b)) '((?a) ())))
  (should (equal (erc--parse-user-modes "+ab-c" '(?b ?c)) '((?a) (?c))))
  (should (equal (erc--parse-user-modes "+a-c+b" '(?b ?c)) '((?a) (?c))))
  (should (equal (erc--parse-user-modes "-c+ab" '(?b ?c)) '((?a) (?c))))

  ;; Param `extrap' returns groups of redundant chars.
  (should (equal (erc--parse-user-modes "+a" '() t) '((?a) () () ())))
  (should (equal (erc--parse-user-modes "+a" '(?a) t) '(() () (?a) ())))
  (should (equal (erc--parse-user-modes "-a" '() t) '(() () () (?a))))
  (should (equal (erc--parse-user-modes "-a" '(?a) t) '(() (?a) () ())))

  (should (equal (erc--parse-user-modes "+a-b" '(?a) t) '(() () (?a) (?b))))
  (should (equal (erc--parse-user-modes "-b+a" '(?a) t) '(() () (?a) (?b))))
  (should (equal (erc--parse-user-modes "+a-b" '(?b) t) '((?a) (?b) () ())))
  (should (equal (erc--parse-user-modes "-b+a" '(?b) t) '((?a) (?b) () ()))))

(ert-deftest erc--parse-isupport-value ()
  (should (equal (erc--parse-isupport-value "a,b") '("a" "b")))
  (should (equal (erc--parse-isupport-value "a,b,c") '("a" "b" "c")))

  (should (equal (erc--parse-isupport-value "abc") '("abc")))
  (should (equal (erc--parse-isupport-value "\\x20foo") '(" foo")))
  (should (equal (erc--parse-isupport-value "foo\\x20") '("foo ")))
  (should (equal (erc--parse-isupport-value "a\\x20b\\x20c") '("a b c")))
  (should (equal (erc--parse-isupport-value "a\\x20b\\x20c\\x20") '("a b c ")))
  (should (equal (erc--parse-isupport-value "\\x20a\\x20b\\x20c") '(" a b c")))
  (should (equal (erc--parse-isupport-value "a\\x20\\x20c") '("a  c")))
  (should (equal (erc--parse-isupport-value "\\x20\\x20\\x20") '("   ")))
  (should (equal (erc--parse-isupport-value "\\x5Co/") '("\\o/")))
  (should (equal (erc--parse-isupport-value "\\x7F,\\x19") '("\\x7F" "\\x19")))
  (should (equal (erc--parse-isupport-value "a\\x3Db") '("a=b")))
  (should (equal (erc--parse-isupport-value "a\\x2Cb,c") '("a,b" "c"))))

(ert-deftest erc--get-isupport-entry ()
  (let ((erc--isupport-params (make-hash-table))
        (erc-server-parameters '(("FOO" . "1") ("BAR") ("BAZ" . "A,B,C")
                                 ("SPAM" . "")))
        (items (lambda ()
                 (cl-loop for k being the hash-keys of erc--isupport-params
                          using (hash-values v) collect (cons k v)))))

    (should-not (erc--get-isupport-entry 'FAKE))
    (should-not (erc--get-isupport-entry 'FAKE 'single))
    (should (zerop (hash-table-count erc--isupport-params)))

    (should (equal (erc--get-isupport-entry 'BAR) '(BAR)))
    (should-not (erc--get-isupport-entry 'BAR 'single))
    (should (= 1 (hash-table-count erc--isupport-params)))

    (should (equal (erc--get-isupport-entry 'BAZ) '(BAZ "A" "B" "C")))
    (should (equal (erc--get-isupport-entry 'BAZ 'single) "A"))
    (should (= 2 (hash-table-count erc--isupport-params)))

    (should (equal (erc--get-isupport-entry 'FOO 'single) "1"))
    (should (equal (erc--get-isupport-entry 'FOO) '(FOO "1")))

    (should (equal (funcall items)
                   '((BAR . --empty--) (BAZ "A" "B" "C") (FOO "1"))))
    (should (equal (erc--get-isupport-entry 'SPAM) '(SPAM)))
    (should-not (erc--get-isupport-entry 'SPAM 'single))))

(ert-deftest erc-server-005 ()
  (let* ((hooked 0)
         (verify #'ignore)
         (hook (lambda (_ _) (funcall verify) (cl-incf hooked)))
         (erc-server-005-functions (list #'erc-server-005 hook #'ignore))
         erc-server-parameters
         erc--isupport-params
         erc-timer-hook
         calls
         args
         parsed)

    (cl-letf (((symbol-function 'erc-display-message)
               (lambda (_ _ _ line) (push line calls))))

      (ert-info ("Baseline")
        (setq args '("tester" "BOT=B" "CHANTYPES=" "EXCEPTS" "PREFIX=(ov)@+"
                     "are supp...")
              parsed (make-erc-response :command-args args :command "005"))

        (setq verify
              (lambda ()
                (should (equal erc-server-parameters
                               '(("PREFIX" . "(ov)@+") ("EXCEPTS")
                                 ;; Should be ("CHANTYPES") but
                                 ;; retained for compatibility.
                                 ("CHANTYPES" . "")
                                 ("BOT" . "B"))))
                (should (zerop (hash-table-count erc--isupport-params)))
                (should (equal "(ov)@+" (erc--get-isupport-entry 'PREFIX t)))
                (should (equal '(EXCEPTS) (erc--get-isupport-entry 'EXCEPTS)))
                (should (equal "B" (erc--get-isupport-entry 'BOT t)))
                (should (string=
                         (pop calls)
                         "BOT=B CHANTYPES= EXCEPTS PREFIX=(ov)@+ are supp..."))
                (should (equal args (erc-response.command-args parsed)))))

        (erc-call-hooks nil parsed))

      (ert-info ("Negated, updated")
        (setq args '("tester" "-EXCEPTS" "-CHANTYPES" "-FAKE" "PREFIX=(ohv)@%+"
                     "are su...")
              parsed (make-erc-response :command-args args :command "005"))

        (setq verify
              (lambda ()
                (should (equal erc-server-parameters
                               '(("PREFIX" . "(ohv)@%+") ("BOT" . "B"))))
                (should (string-prefix-p
                         "-EXCEPTS -CHANTYPES -FAKE PREFIX=(ohv)@%+ "
                         (pop calls)))
                (should (equal "(ohv)@%+" (erc--get-isupport-entry 'PREFIX t)))
                (should (equal "B" (erc--get-isupport-entry 'BOT t)))
                (should-not (erc--get-isupport-entry 'EXCEPTS))
                (should (equal args (erc-response.command-args parsed)))))

        (erc-call-hooks nil parsed))
      (should (= hooked 2)))))

(ert-deftest erc-downcase ()
  (let ((erc--isupport-params (make-hash-table)))

    (puthash 'PREFIX '("(ov)@+") erc--isupport-params)
    (puthash 'BOT '("B") erc--isupport-params)

    (ert-info ("ascii")
      (puthash 'CASEMAPPING  '("ascii") erc--isupport-params)
      (should (equal (erc-downcase "ABC 123 ΔΞΩΣ") "abc 123 ΔΞΩΣ"))
      (should (equal (erc-downcase "Bob[m]`") "bob[m]`"))
      (should (equal (erc-downcase "Tilde~") "tilde~" ))
      (should (equal (erc-downcase "\\O/") "\\o/" )))

    (ert-info ("rfc1459")
      (puthash 'CASEMAPPING  '("rfc1459") erc--isupport-params)
      (should (equal (erc-downcase "ABC 123 ΔΞΩΣ") "abc 123 ΔΞΩΣ"))
      (should (equal (erc-downcase "Bob[m]`") "bob{m}`" ))
      (should (equal (erc-downcase "Tilde~") "tilde^" ))
      (should (equal (erc-downcase "\\O/") "|o/" )))

    (ert-info ("rfc1459-strict")
      (puthash 'CASEMAPPING  '("rfc1459-strict") erc--isupport-params)
      (should (equal (erc-downcase "ABC 123 ΔΞΩΣ") "abc 123 ΔΞΩΣ"))
      (should (equal (erc-downcase "Bob[m]`") "bob{m}`"))
      (should (equal (erc-downcase "Tilde~") "tilde~" ))
      (should (equal (erc-downcase "\\O/") "|o/" )))))

(ert-deftest erc-channel-p ()
  (erc-tests-common-make-server-buf)

  (should (erc-channel-p "#chan"))
  (should (erc-channel-p "##chan"))
  (should (erc-channel-p "&chan"))
  (should-not (erc-channel-p "+chan"))
  (should-not (erc-channel-p "!chan"))
  (should-not (erc-channel-p "@chan"))

  ;; Server sends "CHANTYPES=#&+!"
  (should-not erc-server-parameters)
  (setq erc-server-parameters '(("CHANTYPES" . "#&+!")))
  (should (erc-channel-p "#chan"))
  (should (erc-channel-p "&chan"))
  (should (erc-channel-p "+chan"))
  (should (erc-channel-p "!chan"))

  (with-current-buffer (erc--open-target "#chan")
    (should (erc-channel-p (current-buffer))))
  (with-current-buffer (erc--open-target "+chan")
    (should (erc-channel-p (current-buffer))))
  (should (erc-channel-p (get-buffer "#chan")))
  (should (erc-channel-p (get-buffer "+chan")))

  ;; Server sends "CHANTYPES=" because it's query only.
  (puthash 'CHANTYPES '("CHANTYPES") erc--isupport-params)
  (should-not (erc-channel-p "#spam"))
  (should-not (erc-channel-p "&spam"))
  (should-not (erc-channel-p (save-excursion (erc--open-target "#spam"))))

  (erc-tests-common-kill-buffers))

(ert-deftest erc-query-buffer-p ()
  ;; Nil in a non-ERC buffer.
  (should-not (erc-query-buffer-p))
  (should-not (erc-query-buffer-p (current-buffer)))
  (should-not (erc-query-buffer-p (buffer-name)))

  (erc-tests-common-make-server-buf)
  ;; Nil in a server buffer.
  (should-not (erc-query-buffer-p))
  (should-not (erc-query-buffer-p (current-buffer)))
  (should-not (erc-query-buffer-p (buffer-name)))

  ;; Nil in a channel buffer.
  (with-current-buffer (erc--open-target "#chan")
    (should-not (erc-query-buffer-p))
    (should-not (erc-query-buffer-p (current-buffer)))
    (should-not (erc-query-buffer-p (buffer-name))))

  ;; Non-nil in a query buffer.
  (with-current-buffer (erc--open-target "alice")
    (should (erc-query-buffer-p))
    (should (erc-query-buffer-p (current-buffer)))
    (should (erc-query-buffer-p (buffer-name))))

  (should (erc-query-buffer-p (get-buffer "alice")))
  (should (erc-query-buffer-p "alice"))

  (erc-tests-common-kill-buffers))

(ert-deftest erc--valid-local-channel-p ()
  (ert-info ("Local channels not supported")
    (let ((erc--isupport-params (make-hash-table)))
      (puthash 'CHANTYPES  '("#") erc--isupport-params)
      (should-not (erc--valid-local-channel-p "#chan"))
      (should-not (erc--valid-local-channel-p "&local"))))
  (ert-info ("Local channels supported")
    (let ((erc--isupport-params (make-hash-table)))
      (puthash 'CHANTYPES  '("&#") erc--isupport-params)
      (should-not (erc--valid-local-channel-p "#chan"))
      (should (erc--valid-local-channel-p "&local")))))

(ert-deftest erc--target-from-string ()
  (should (equal (erc--target-from-string "#chan")
                 #s(erc--target-channel "#chan" \#chan nil)))

  (should (equal (erc--target-from-string "Bob")
                 #s(erc--target "Bob" bob)))

  (let ((erc--isupport-params (make-hash-table)))
    (puthash 'CHANTYPES  '("&#") erc--isupport-params)
    (should (equal (erc--target-from-string "&Bitlbee")
                   #s(erc--target-channel-local "&Bitlbee" &bitlbee nil)))))

(ert-deftest erc--modify-local-map ()
  (when (and (bound-and-true-p erc-irccontrols-mode)
             (fboundp 'erc-irccontrols-mode))
    (erc-irccontrols-mode -1))
  (when (and (bound-and-true-p erc-match-mode)
             (fboundp 'erc-match-mode))
    (erc-match-mode -1))
  (let* (calls
         (inhibit-message noninteractive)
         (cmd-foo (lambda () (interactive) (push 'foo calls)))
         (cmd-bar (lambda () (interactive) (push 'bar calls))))

    (ert-info ("Add non-existing")
      (erc--modify-local-map t "C-c C-c" cmd-foo "C-c C-k" cmd-bar)
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (use-local-map erc-mode-map)
        (execute-kbd-macro "\C-c\C-c")
        (execute-kbd-macro "\C-c\C-k"))
      (should (equal calls '(bar foo))))
    (setq calls nil)

    (ert-info ("Add existing") ; Attempt to swap definitions fails
      (erc--modify-local-map t "C-c C-c" cmd-bar "C-c C-k" cmd-foo)
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (use-local-map erc-mode-map)
        (execute-kbd-macro "\C-c\C-c")
        (execute-kbd-macro "\C-c\C-k"))
      (should (equal calls '(bar foo))))
    (setq calls nil)

    (ert-info ("Remove existing")
      (erc--modify-local-map nil "C-c C-c" cmd-foo "C-c C-k" cmd-bar)
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (use-local-map erc-mode-map)
        (cl-letf (((symbol-function 'undefined)
                   (lambda ()
                     (push (key-description (this-single-command-keys))
                           calls))))
          (execute-kbd-macro "\C-c\C-c")
          (execute-kbd-macro "\C-c\C-k")))
      (should (equal calls '("C-c C-k" "C-c C-c"))))))

(ert-deftest erc-ring-previous-command-base-case ()
  (ert-info ("Create ring when nonexistent and do nothing")
    (let (erc-input-ring
          erc-input-ring-index)
      (erc-previous-command)
      (should (ring-p erc-input-ring))
      (should (zerop (ring-length erc-input-ring)))
      (should-not erc-input-ring-index)))
  (should-not erc-input-ring))

(ert-deftest erc-ring-previous-command ()
  (with-current-buffer (get-buffer-create "*#fake*")
    (erc-mode)
    (erc-tests-common-prep-for-insertion)
    (setq erc-server-current-nick "tester")
    (setq-local erc-last-input-time 0)
    (should-not (local-variable-if-set-p 'erc-send-completed-hook))
    (setq-local erc-send-completed-hook nil) ; skip t (globals)
    ;; Just in case erc-ring-mode is already on
    (setq-local erc--input-review-functions erc--input-review-functions)
    (add-hook 'erc--input-review-functions #'erc-add-to-input-ring)
    ;;
    (cl-letf (((symbol-function 'erc-process-input-line)
               (lambda (&rest _)
                 (erc-display-message
                  nil 'notice (current-buffer) "echo: one\n")))
              ((symbol-function 'erc-command-no-process-p)
               (lambda (&rest _) t)))
      (ert-info ("Create ring, populate, recall")
        (insert "/one")
        (erc-send-current-line)
        (should (ring-p erc-input-ring))
        (should (zerop (ring-member erc-input-ring "/one"))) ; equal
        (should (save-excursion (forward-line -1)
                                (looking-at-p "[*]+ echo: one")))
        (should-not erc-input-ring-index)
        (erc-bol)
        (should (looking-at "$"))
        (erc-previous-command)
        (erc-bol)
        (should (looking-at "/one"))
        (should (zerop erc-input-ring-index)))
      (ert-info ("Back to one")
        (should (= (ring-length erc-input-ring) (1+ erc-input-ring-index)))
        (erc-previous-command)
        (should-not erc-input-ring-index)
        (erc-bol)
        (should (looking-at "$"))
        (should (equal (ring-ref erc-input-ring 0) "/one")))
      (ert-info ("Swap input after prompt with previous (#bug46339)")
        (insert "abc")
        (erc-previous-command)
        (should (= 1 erc-input-ring-index))
        (erc-bol)
        (should (looking-at "/one"))
        (should (equal (ring-ref erc-input-ring 0) "abc"))
        (should (equal (ring-ref erc-input-ring 1) "/one"))
        (erc-next-command)
        (erc-bol)
        (should (looking-at "abc")))))
  (when noninteractive
    (kill-buffer "*#fake*")))

(ert-deftest erc--debug-irc-protocol-mask-secrets ()
  (should-not erc-debug-irc-protocol)
  (should erc--debug-irc-protocol-mask-secrets)
  (with-temp-buffer
    (setq erc-server-process (start-process "fake" (current-buffer) "true")
          erc-server-current-nick "tester"
          erc-session-server "myproxy.localhost"
          erc-session-port 6667)
    (let ((inhibit-message noninteractive))
      (erc-toggle-debug-irc-protocol)
      (erc-log-irc-protocol
       (concat "PASS :" (erc--unfun (lambda () "changeme")) "\r\n")
       'outgoing)
      (set-process-query-on-exit-flag erc-server-process nil))
    (with-current-buffer "*erc-protocol*"
      (goto-char (point-min))
      (search-forward "\r\n\r\n")
      (search-forward "myproxy.localhost:6667 >> PASS :????????" (pos-eol)))
    (when noninteractive
      (kill-buffer "*erc-protocol*")
      (should-not erc-debug-irc-protocol))))

(ert-deftest erc-log-irc-protocol ()
  (should-not erc-debug-irc-protocol)
  (with-temp-buffer
    (setq erc-server-process (start-process "fake" (current-buffer) "true")
          erc-server-current-nick "tester"
          erc-session-server "myproxy.localhost"
          erc-session-port 6667)
    (let ((inhibit-message noninteractive))
      (erc-toggle-debug-irc-protocol)
      (erc-log-irc-protocol "PASS changeme\r\n" 'outgoing)
      (setq erc-server-announced-name "irc.gnu.org")
      (erc-log-irc-protocol ":irc.gnu.org 001 tester :Welcome")
      (erc-log-irc-protocol ":irc.gnu.org 002 tester :Your host is irc.gnu.org")
      (setq erc-network 'FooNet)
      (setq erc-networks--id (erc-networks--id-create nil))
      (erc-log-irc-protocol ":irc.gnu.org 422 tester :MOTD missing")
      (setq erc-networks--id (erc-networks--id-create 'BarNet))
      (erc-log-irc-protocol ":irc.gnu.org 221 tester +i")
      (set-process-query-on-exit-flag erc-server-process nil)))
  (with-current-buffer "*erc-protocol*"
    (goto-char (point-min))
    (search-forward "Version")
    (search-forward "\r\n\r\n")
    (search-forward "myproxy.localhost:6667 >> PASS" (pos-eol))
    (forward-line)
    (search-forward "irc.gnu.org << :irc.gnu.org 001" (pos-eol))
    (forward-line)
    (search-forward "irc.gnu.org << :irc.gnu.org 002" (pos-eol))
    (forward-line)
    (search-forward "FooNet << :irc.gnu.org 422" (pos-eol))
    (forward-line)
    (search-forward "BarNet << :irc.gnu.org 221" (pos-eol)))
  (when noninteractive
    (kill-buffer "*erc-protocol*")
    (should-not erc-debug-irc-protocol)))

(ert-deftest erc--split-line ()
  (let ((erc-split-line-length 0))
    (should (equal (erc--split-line "") '("")))
    (should (equal (erc--split-line " ") '(" ")))
    (should (equal (erc--split-line "1") '("1")))
    (should (equal (erc--split-line " 1") '(" 1")))
    (should (equal (erc--split-line "1 ") '("1 ")))
    (should (equal (erc--split-line "abc") '("abc"))))

  (let ((erc-default-recipients '("#chan"))
        (erc-split-line-length 10))
    (should (equal (erc--split-line "") '("")))
    (should (equal (erc--split-line "0123456789") '("0123456789")))
    (should (equal (erc--split-line "0123456789a") '("0123456789" "a")))

    (should (equal (erc--split-line "0123456789 ") '("0123456789" " ")))
    (should (equal (erc--split-line "01234567 89") '("01234567 " "89")))
    (should (equal (erc--split-line "0123456 789") '("0123456 " "789")))
    (should (equal (erc--split-line "0 123456789") '("0 " "123456789")))
    (should (equal (erc--split-line " 0123456789") '(" " "0123456789")))
    (should (equal (erc--split-line "012345678 9a") '("012345678 " "9a")))
    (should (equal (erc--split-line "0123456789 a") '("0123456789" " a")))

    ;; UTF-8 vs. KOI-8
    (should (= 10 (string-bytes "Русск"))) ; utf-8
    (should (equal (erc--split-line "Русск") '("Русск")))
    (should (equal (erc--split-line "РусскийТекст") '("Русск" "ийТек" "ст")))
    (should (equal (erc--split-line "Русский Текст") '("Русск" "ий " "Текст")))
    (let ((erc-encoding-coding-alist '(("#chan" . cyrillic-koi8))))
      (should (equal (erc--split-line "Русск") '("Русск")))
      (should (equal (erc--split-line "РусскийТекст") '("РусскийТек" "ст")))
      (should (equal (erc--split-line "Русский Текст") '("Русский " "Текст"))))

    ;; UTF-8 vs. Latin 1
    (should (= 17 (string-bytes "Hyvää päivää")))
    (should (equal (erc--split-line "Hyvää päivää") '("Hyvää " "päivää")))
    (should (equal (erc--split-line "HyvääPäivää") '("HyvääPä" "ivää")))
    (let ((erc-encoding-coding-alist '(("#chan" . latin-1))))
      (should (equal (erc--split-line "Hyvää päivää") '("Hyvää " "päivää")))
      (should (equal (erc--split-line "HyvääPäivää") '("HyvääPäivä" "ä"))))

    ;; Combining characters
    (should (= 10 (string-bytes "Åström")))
    (should (equal (erc--split-line "_Åström") '("_Åströ" "m")))
    (should (equal (erc--split-line "__Åström") '("__Åstr" "öm")))
    (should (equal (erc--split-line "___Åström") '("___Åstr" "öm")))
    (when (> emacs-major-version 27)
      (should (equal (erc--split-line "🏁🚩🎌🏴🏳️🏳️‍🌈🏳️‍⚧️🏴‍☠️")
                     '("🏁🚩" "🎌🏴" "🏳️" "🏳️‍🌈" "🏳️‍⚧️" "🏴‍☠️"))))))

(ert-deftest erc--input-line-delim-regexp ()
  (let ((p erc--input-line-delim-regexp))
    ;; none
    (should (equal '("a" "b") (split-string "a\r\nb" p)))
    (should (equal '("a" "b") (split-string "a\nb" p)))
    (should (equal '("a" "b") (split-string "a\rb" p)))

    ;; one
    (should (equal '("") (split-string "" p)))
    (should (equal '("a" "" "b") (split-string "a\r\rb" p)))
    (should (equal '("a" "" "b") (split-string "a\n\rb" p)))
    (should (equal '("a" "" "b") (split-string "a\n\nb" p)))
    (should (equal '("a" "" "b") (split-string "a\r\r\nb" p)))
    (should (equal '("a" "" "b") (split-string "a\n\r\nb" p)))
    (should (equal '("a" "") (split-string "a\n" p)))
    (should (equal '("a" "") (split-string "a\r" p)))
    (should (equal '("a" "") (split-string "a\r\n" p)))
    (should (equal '("" "b") (split-string "\nb" p)))
    (should (equal '("" "b") (split-string "\rb" p)))
    (should (equal '("" "b") (split-string "\r\nb" p)))

    ;; two
    (should (equal '("" "") (split-string "\r" p)))
    (should (equal '("" "") (split-string "\n" p)))
    (should (equal '("" "") (split-string "\r\n" p)))

    ;; three
    (should (equal '("" "" "") (split-string "\r\r" p)))
    (should (equal '("" "" "") (split-string "\n\n" p)))
    (should (equal '("" "" "") (split-string "\n\r" p)))))

(ert-deftest erc--check-prompt-input-functions ()
  (erc-tests-common-with-process-input-spy
   (lambda (next)
     (erc-tests-common-prep-for-insertion)

     (ert-info ("Errors when point not in prompt area") ; actually just dings
       (insert "/msg #chan hi")
       (forward-line -1)
       (let ((e (should-error (erc-send-current-line))))
         (should (equal "Point is not in the input area" (cadr e))))
       (goto-char (point-max))
       (ert-info ("Input remains untouched")
         (should (save-excursion (erc-bol) (looking-at "/msg #chan hi")))))

     (ert-info ("Errors when server buffer absent")
       (let ((e (should-error (erc-send-current-line))))
         (should (equal "Server buffer missing" (cadr e))))
       (ert-info ("Input remains untouched")
         (should (save-excursion (erc-bol) (looking-at "/msg #chan hi")))))

     (ert-info ("Errors when line contains empty newline")
       (erc-bol)
       (delete-region (point) (point-max))
       (insert "one\n")
       (let ((e (should-error (erc-send-current-line))))
         (should (string-prefix-p "Trailing line detected" (cadr e))))
       (goto-char (point-max))
       (ert-info ("Input remains untouched")
         (should (save-excursion (goto-char erc-input-marker)
                                 (looking-at "one\n")))))

     (should (= 0 erc-last-input-time))
     (should-not (funcall next)))))

;; These also indirectly tests `erc-send-input'

(ert-deftest erc-send-current-line ()
  (erc-tests-common-with-process-input-spy
   (lambda (next)
     (erc-tests-common-make-server-buf (buffer-name))
     (should (= 0 erc-last-input-time))

     (ert-info ("Simple command")
       (insert "/msg #chan hi")
       (erc-send-current-line)
       (ert-info ("Prompt restored")
         (forward-line 0)
         (should (looking-at-p erc-prompt)))
       (ert-info ("Input cleared")
         (erc-bol)
         (should (eq (point) (point-max))))
       ;; The `force' argument is irrelevant here because it can't
       ;; influence dispatched handlers, such as `erc-cmd-MSG'.
       (should (pcase (funcall next) (`("/msg #chan hi\n" ,_ nil) t))))

     (ert-info ("Simple non-command")
       (insert "hi")
       (erc-send-current-line)
       (should (eq (point) (point-max)))
       (should (save-excursion (forward-line -1)
                               (search-forward "<tester> hi")))
       ;; Non-commands are forced only when `erc-flood-protect' is
       ;; nil, which conflates two orthogonal concerns.
       (should (equal (funcall next) '("hi\n" nil t))))

     (should (consp erc-last-input-time)))))

(ert-deftest erc--discard-trailing-multiline-nulls ()
  (pcase-dolist (`(,input ,want) '((("") (""))
                                   (("" "") (""))
                                   (("a") ("a"))
                                   (("a" "") ("a"))
                                   (("" "a") ("" "a"))
                                   (("" "a" "") ("" "a"))))
    (ert-info ((format "Input: %S, want: %S" input want))
      (let ((s (make-erc--input-split :lines input)))
        (erc--discard-trailing-multiline-nulls s)
        (should (equal (erc--input-split-lines s) want))))))

(ert-deftest erc--count-blank-lines ()
  (pcase-dolist (`(,input ,want) '((() (0 0 0))
                                   (("") (1 1 0))
                                   (("" "") (2 1 1))
                                   (("" "" "") (3 1 2))
                                   ((" " "") (2 0 1))
                                   ((" " "" "") (3 0 2))
                                   (("" " " "") (3 1 1))
                                   (("" "" " ") (3 2 0))
                                   (("a") (0 0 0))
                                   (("a" "") (1 0 1))
                                   (("a" " " "") (2 0 1))
                                   (("a" "" "") (2 0 2))
                                   (("a" "b") (0 0 0))
                                   (("a" "" "b") (1 1 0))
                                   (("a" " " "b") (1 0 0))
                                   (("" "a") (1 1 0))
                                   ((" " "a") (1 0 0))
                                   (("" "a" "") (2 1 1))
                                   (("" " " "a" "" " ") (4 2 0))
                                   (("" " " "a" "" " " "") (5 2 1))))
    (ert-info ((format "Input: %S, want: %S" input want))
      (should (equal (erc--count-blank-lines input) want)))))

;; Opt `wb': `erc-warn-about-blank-lines'
;; Opt `sw': `erc-send-whitespace-lines'
;; `s': " \n",`a': "a\n",`b': "b\n"
(defvar erc-tests--check-prompt-input--expect
  ;;  opts     ""  " "   "\n"  "\n "   " \n" "\n\n" "a\n" "a\n " "a\n \nb"
  '(((+wb -sw) err err   err   err     err   err    err   err    err)
    ((-wb -sw) nop nop   nop   nop     nop   nop    nop   nop    nop)
    ((+wb +sw) err (s)   (0 s) (1 s s) (s)   (0 s)  (0 a) (a s)  (a s b))
    ((-wb +sw) nop (s)   (s)   (s s)   (s)   (s)    (a)   (a s)  (a s b))))

;; Help messages echoed (not IRC message) was emitted
(defvar erc-tests--check-prompt-input-messages
  '("Stripping" "Padding"))

(ert-deftest erc--check-prompt-input-for-multiline-blanks ()
  :tags '(:expensive-test)
  (ert-with-message-capture messages
    (erc-tests-common-with-process-input-spy
     (lambda (next)
       (erc-tests-common-make-server-buf (buffer-name))

       (should-not erc-send-whitespace-lines)
       (should erc-warn-about-blank-lines)

       (pcase-dolist (`((,wb ,sw) . ,ex) erc-tests--check-prompt-input--expect)
         (let ((print-escape-newlines t)
               (erc-warn-about-blank-lines (eq wb '+wb))
               (erc-send-whitespace-lines (eq sw '+sw))
               (samples '("" " " "\n" "\n " " \n" "\n\n"
                          "a\n" "a\n " "a\n \nb")))
           (setq ex `(,@ex (a) (a b)) ; baseline, same for all combos
                 samples `(,@samples "a" "a\nb"))
           (dolist (input samples)
             (insert input)
             (ert-info ((format "Opts: %S, Input: %S, want: %S"
                                (list wb sw) input (car ex)))
               (setq messages "")
               (pcase-exhaustive (pop ex)
                 ('err (let ((e (should-error (erc-send-current-line))))
                         (should (string-match (rx (| "trailing" "blank"))
                                               (cadr e))))
                       (should (equal (erc-user-input) input))
                       (should-not (funcall next)))
                 ('nop (erc-send-current-line)
                       (should (equal (erc-user-input) input))
                       (should-not (funcall next)))
                 ((and (pred consp) v)
                  (erc-send-current-line)
                  (should (string-empty-p (erc-user-input)))
                  (setq v (reverse v)) ; don't use `nreverse' here
                  (while v
                    (pcase (pop v)
                      ((and (pred integerp) n)
                       (should (string-search
                                (nth n erc-tests--check-prompt-input-messages)
                                messages)))
                      ('s (should (equal " \n" (car (funcall next)))))
                      ('a (should (equal "a\n" (car (funcall next)))))
                      ('b (should (equal "b\n" (car (funcall next)))))))
                  (should-not (funcall next)))))
             (delete-region erc-input-marker (point-max)))))))))

(ert-deftest erc--check-prompt-input-for-multiline-blanks/explanations ()
  (should erc-warn-about-blank-lines)
  (should-not erc-send-whitespace-lines)

  (let ((erc-send-whitespace-lines t))
    (pcase-dolist (`(,input ,msg)
                   '((("") "Padding (1) blank line")
                     (("" " ") "Padding (1) blank line")
                     ((" " "") "Stripping (1) blank line")
                     (("a" "") "Stripping (1) blank line")
                     (("" "") "Stripping (1) and padding (1) blank lines")
                     (("" "" "") "Stripping (2) and padding (1) blank lines")
                     (("" "a" "" "b" "" "c" "" "")
                      "Stripping (2) and padding (3) blank lines")))
      (ert-info ((format "Input: %S, Msg: %S" input msg))
        (let (erc--check-prompt-explanation)
          (should-not (erc--check-prompt-input-for-multiline-blanks nil input))
          (should (equal (list msg) erc--check-prompt-explanation))))))

  (pcase-dolist (`(,input ,msg)
                 '((("") "Blank line detected")
                   (("" " ") "2 blank lines detected")
                   ((" " "") "2 blank (1 trailing) lines detected")
                   (("a" "") "Trailing line detected")
                   (("" "") "2 blank (1 trailing) lines detected")
                   (("a" "" "") "2 trailing lines detected")
                   (("" "a" "" "b" "" "c" "" "")
                    "5 blank (2 trailing) lines detected")))
    (ert-info ((format "Input: %S, Msg: %S" input msg))
      (let ((rv (erc--check-prompt-input-for-multiline-blanks nil input)))
        (should (equal (concat msg " (see `erc-send-whitespace-lines')")
                       rv ))))))

(ert-deftest erc-send-whitespace-lines ()
  (erc-tests-common-with-process-input-spy
   (lambda (next)
     (erc-tests-common-make-server-buf (buffer-name))

     (setq-local erc-send-whitespace-lines t)

     (ert-info ("Multiline hunk with blank line correctly split")
       (insert "one\n\ntwo")
       (erc-send-current-line)
       (ert-info ("Prompt restored")
         (forward-line 0)
         (should (looking-at-p erc-prompt)))
       (ert-info ("Input cleared")
         (erc-bol)
         (should (eq (point) (point-max))))
       (should (equal (funcall next) '("two\n" nil t)))
       (should (equal (funcall next) '(" \n" nil t)))
       (should (equal (funcall next) '("one\n" nil t))))

     (ert-info ("Multiline hunk with trailing newline filtered")
       (insert "hi\n")
       (erc-send-current-line)
       (ert-info ("Input cleared")
         (erc-bol)
         (should (eq (point) (point-max))))
       (should (equal (funcall next) '("hi\n" nil t)))
       (should-not (funcall next)))

     (ert-info ("Multiline hunk with trailing carriage filtered")
       (insert "hi\r")
       (erc-send-current-line)
       (ert-info ("Input cleared")
         (erc-bol)
         (should (eq (point) (point-max))))
       (should (equal (funcall next) '("hi\n" nil t)))
       (should-not (funcall next)))

     (ert-info ("Multiline command with trailing blank filtered")
       (dolist (p '("/a b" "/a b\n" "/a b\n\n" "/a b\n\n\n"))
         (insert p)
         (erc-send-current-line)
         (erc-bol)
         (should (eq (point) (point-max)))
         (should (pcase (funcall next) (`(,cmd ,_ nil) (equal cmd "/a b\n"))))
         (should-not (funcall next))))

     (ert-info ("Multiline command with non-blanks errors")
       (dolist (p '("/a b\nc\n\n" "/a b\n/c\n\n" "/a b\n\nc\n\n"
                    "/a\n c\n" "/a\nb\n" "/a\n/b\n" "/a \n \n"))
         (insert p)
         (should-error (erc-send-current-line))
         (goto-char erc-input-marker)
         (delete-region (point) (point-max))
         (should-not (funcall next))))

     (ert-info ("Multiline hunk with trailing whitespace not filtered")
       (insert "there\n ")
       (erc-send-current-line)
       (should (equal (funcall next) '(" \n" nil t)))
       (should (equal (funcall next) '("there\n" nil t)))
       (should-not (funcall next))))))

(ert-deftest erc--check-prompt-input-for-excess-lines ()
  (ert-info ("Without `erc-inhibit-multiline-input'")
    (should-not erc-inhibit-multiline-input)
    (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b"))))

  (ert-info ("With `erc-inhibit-multiline-input' as t (2)")
    (let ((erc-inhibit-multiline-input t))
      (should-not (erc--check-prompt-input-for-excess-lines "" '("a")))
      ;; Does not trim trailing blanks.
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "")))
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "b")))))

  (ert-info ("With `erc-inhibit-multiline-input' as 3")
    (let ((erc-inhibit-multiline-input 3))
      (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b")))
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "b" "")))
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "b" "c")))))

  (ert-info ("With `erc-ask-about-multiline-input'")
    (let ((erc-inhibit-multiline-input t)
          (inhibit-message noninteractive)
          (erc-ask-about-multiline-input t))
      (ert-simulate-keys '(?n ?\r ?y ?\r)
        (should (erc--check-prompt-input-for-excess-lines "" '("a" "b")))
        (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b")))))
    (should-not erc-ask-about-multiline-input)))

(ert-deftest erc-extract-command-from-line ()
  ;; FIXME when next modifying `erc-command-regexp's default value,
  ;; move the single quote in the first group's character alternative
  ;; to the front, i.e., [A-Za-z'] -> ['A-Za-z], so we can assert
  ;; equivalence with this more readable `rx' form.
  (rx bol
      "/"
      (group (+ (in "'A-Za-z")))
      (group (| (: (+ (syntax whitespace)) (* nonl))
                (* (syntax whitespace))))
      eol)
  (erc-mode) ; for `erc-mode-syntax-table'

  ;; Non-command.
  (should-not (erc-extract-command-from-line "FAKE\n"))
  ;; Unknown command.
  (should (equal (erc-extract-command-from-line "/FAKE\n")
                 '(erc-cmd-default "/FAKE\n")))

  (ert-info ("With `do-not-parse-args'")
    (should (equal (erc-extract-command-from-line "/MSG\n")
                   '(erc-cmd-MSG "\n")))
    (should (equal (erc-extract-command-from-line "/MSG \n")
                   '(erc-cmd-MSG " \n")))
    (should (equal (erc-extract-command-from-line "/MSG \n\n")
                   '(erc-cmd-MSG " \n\n")))
    (should (equal (erc-extract-command-from-line "/MSG foo\n")
                   '(erc-cmd-MSG " foo")))
    (should (equal (erc-extract-command-from-line "/MSG foo\n\n")
                   '(erc-cmd-MSG " foo")))
    (should (equal (erc-extract-command-from-line "/MSG foo\n \n")
                   '(erc-cmd-MSG " foo")))
    (should (equal (erc-extract-command-from-line "/MSG    foo\n")
                   '(erc-cmd-MSG "    foo"))))

  (ert-info ("Without `do-not-parse-args'")
    (should (equal (erc-extract-command-from-line "/HELP\n")
                   '(erc-cmd-HELP nil)))
    (should (equal (erc-extract-command-from-line "/HELP \n")
                   '(erc-cmd-HELP nil)))
    (should (equal (erc-extract-command-from-line "/HELP foo\n")
                   '(erc-cmd-HELP ("foo"))))
    (should (equal (erc-extract-command-from-line "/HELP     foo\n")
                   '(erc-cmd-HELP ("foo"))))
    (should (equal (erc-extract-command-from-line "/HELP foo bar\n")
                   '(erc-cmd-HELP ("foo" "bar"))))))

;; The point of this test is to ensure output is handled identically
;; regardless of whether a command handler is summoned.

(ert-deftest erc-process-input-line ()
  (erc-tests-common-make-server-buf)
  (let ((orig-erc-cmd-MSG (symbol-function 'erc-cmd-MSG))
        (pop-flood-queue (lambda () (erc-with-server-buffer
                                      (pop erc-server-flood-queue))))
        calls)
    (setq erc-server-current-nick "tester")
    (with-current-buffer (erc--open-target "#chan")
      (cl-letf (((symbol-function 'erc-cmd-MSG)
                 (lambda (line)
                   (push line calls)
                   (should erc--called-as-input-p)
                   (funcall orig-erc-cmd-MSG line)))
                ((symbol-function 'erc-server-send-queue)
                 #'ignore))

        (ert-info ("Dispatch to user command handler")

          (ert-info ("Baseline")
            (erc-process-input-line "/msg #chan hi\n")
            (should (equal (pop calls) " #chan hi"))
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan :hi\r\n" . utf-8))))

          (ert-info ("Quote preserves line intact")
            (erc-process-input-line "/QUOTE FAKE foo bar\n")
            (should (equal (funcall pop-flood-queue)
                           '("FAKE foo bar\r\n" . utf-8))))

          (ert-info ("Unknown command respected")
            (erc-process-input-line "/FAKE foo bar\n")
            (should (equal (funcall pop-flood-queue)
                           '("FAKE foo bar\r\n" . utf-8))))

          (ert-info ("Spaces preserved")
            (erc-process-input-line "/msg #chan hi you\n")
            (should (equal (pop calls) " #chan hi you"))
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan :hi you\r\n" . utf-8))))

          (ert-info ("Empty line honored")
            (erc-process-input-line "/msg #chan\n")
            (should (equal (pop calls) " #chan"))
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan :\r\n" . utf-8)))))

        (ert-info ("Implicit cmd via `erc-send-input-line-function'")

          (ert-info ("Baseline")
            (erc-process-input-line "hi\n")
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan :hi\r\n" . utf-8))))

          (ert-info ("Spaces preserved")
            (erc-process-input-line "hi you\n")
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan :hi you\r\n" . utf-8))))

          (ert-info ("Empty line transmitted with injected-space kludge")
            (erc-process-input-line "\n")
            (should (equal (funcall pop-flood-queue)
                           '("PRIVMSG #chan : \r\n" . utf-8))))

          (should-not calls)))))
  (erc-tests-common-kill-buffers))

(ert-deftest erc--get-inserted-msg-beg/basic ()
  (erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg) (should (= 3 (erc--get-inserted-msg-beg arg))))))

(ert-deftest erc--get-inserted-msg-beg/truncated ()
  (erc-tests-common-assert-get-inserted-msg/truncated
   (lambda (arg) (should (= 1 (erc--get-inserted-msg-beg arg))))))

(ert-deftest erc--get-inserted-msg-end/basic ()
  (erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg) (should (= 11 (erc--get-inserted-msg-end arg))))))

(ert-deftest erc--get-inserted-msg-bounds/basic ()
  (erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg)
     (should (equal '(3 . 11) (erc--get-inserted-msg-bounds arg))))))

(ert-deftest erc--insert-before-markers-transplanting-hidden ()
  (with-current-buffer (get-buffer-create "*erc-test*")
    (erc-mode)
    (erc-tests-common-prep-for-insertion)

    ;; Create a message that has a foreign invisibility property on
    ;; its trailing newline that's not claimed by the next message.
    (let ((erc-insert-post-hook
           (lambda ()
             (put-text-property (point-min) (point-max) 'invisible 'b))))
      (erc-display-message nil 'notice (current-buffer) "before"))
    (should (eq 'b (get-text-property (1- erc-insert-marker) 'invisible)))

    ;; Insert a message that's hidden with `erc--hide-message'.  It
    ;; advertises `invisible' value `a', applied on the trailing
    ;; newline of the previous message.
    (let ((erc-insert-post-hook (lambda () (erc--hide-message 'a))))
      (erc-display-message nil 'notice (current-buffer) "after"))

    (goto-char (point-min))
    (should (search-forward "*** before\n" nil t))
    (should (equal '(a b) (get-text-property (1- (point)) 'invisible)))

    ;;  Splice in a new message.
    (let ((erc--insert-line-function
           #'erc--insert-before-markers-transplanting-hidden)
          (erc--insert-marker (copy-marker (point))))
      (goto-char (point-max))
      (erc-display-message nil 'notice (current-buffer) "middle"))

    (goto-char (point-min))
    (should (search-forward "*** before\n" nil t))
    (should (eq 'b (get-text-property (1- (point)) 'invisible)))
    (should (looking-at (rx "*** middle\n")))
    (should (eq 'a (get-text-property (pos-eol) 'invisible)))
    (forward-line)
    (should (looking-at (rx "*** after\n")))

    (setq buffer-invisibility-spec nil)
    (when noninteractive (kill-buffer))))

(ert-deftest erc--delete-inserted-message-naively ()
  (erc-mode)
  (erc--initialize-markers (point) nil)
  ;; Put unique invisible properties on the line endings.
  (erc-display-message nil 'notice nil "one")
  (put-text-property (1- erc-insert-marker) erc-insert-marker 'invisible 'a)
  (let ((erc--msg-prop-overrides '((erc--msg . datestamp) (erc--ts . 0))))
    (erc-display-message nil nil nil
                         (propertize "\n[date]" 'field 'erc-timestamp)))
  (put-text-property (1- erc-insert-marker) erc-insert-marker 'invisible 'b)
  (erc-display-message nil 'notice nil "two")

  (ert-info ("Date stamp deleted cleanly")
    (goto-char 11)
    (should (looking-at (rx "\n[date]")))
    (should (eq 'datestamp (get-text-property (point) 'erc--msg)))
    (should (eq (point) (field-beginning (1+ (point)))))

    (erc--delete-inserted-message-naively (point))

    ;; Preceding line ending clobbered, replaced by trailing.
    (should (looking-back (rx "*** one\n")))
    (should (looking-at (rx "*** two")))
    (should (eq 'b (get-text-property (1- (point)) 'invisible))))

  (ert-info ("Markers at pos-bol preserved")
    (erc-display-message nil 'notice nil "three")
    (should (looking-at (rx "*** two")))

    (let ((m (point-marker))
          (n (point-marker))
          (p (point)))
      (set-marker-insertion-type m t)
      (goto-char (point-max))
      (erc--delete-inserted-message-naively p)
      (should (= (marker-position n) p))
      (should (= (marker-position m) p))
      (goto-char p)
      (set-marker m nil)
      (set-marker n nil)
      (should (looking-back (rx "*** one\n")))
      (should (looking-at (rx "*** three")))))

  (ert-info ("Compat")
    (erc-display-message nil 'notice nil "four")
    (should (looking-at (rx "*** three\n")))
    (with-suppressed-warnings ((obsolete erc-legacy-invisible-bounds-p))
      (let ((erc-legacy-invisible-bounds-p t))
        (erc--delete-inserted-message-naively (point))))
    (should (looking-at (rx "*** four\n"))))

  (ert-info ("Deleting most recent message preserves markers")
    (let ((m (point-marker))
          (n (point-marker))
          (p (point)))
      (should (equal "*** four\n" (buffer-substring p erc-insert-marker)))
      (set-marker-insertion-type m t)
      (goto-char (point-max))
      (erc--delete-inserted-message-naively p)
      (should (= (marker-position m) p))
      (should (= (marker-position n) p))
      (goto-char p)
      (should (looking-back (rx "*** one\n")))
      (should (looking-at erc-prompt))
      (erc--assert-input-bounds)

      ;; However, `m' is now forever "trapped" at `erc-insert-marker'.
      (erc-display-message nil 'notice nil "two")
      (should (= m erc-insert-marker))
      (goto-char n)
      (should (looking-at (rx "*** two\n")))
      (set-marker m nil)
      (set-marker n nil))))

(ert-deftest erc--order-text-properties-from-hash ()
  (let ((table (map-into '((a . 1)
                           (erc--ts . 0)
                           (erc--msg . s005)
                           (b . 2)
                           (erc--cmd . 5)
                           (erc--spkr . "X")
                           (c . 3))
                         'hash-table)))
    (with-temp-buffer
      (erc-mode)
      (insert "abc\n")
      (add-text-properties 1 2 (erc--order-text-properties-from-hash table))
      (should (equal '( erc--msg s005
                        erc--spkr "X"
                        erc--ts 0
                        erc--cmd 5
                        a 1
                        b 2
                        c 3)
                     (text-properties-at (point-min)))))))

(ert-deftest erc--check-msg-prop ()
  (let ((erc--msg-props (map-into '((a . 1) (b . x)) 'hash-table)))
    (should (eq 1 (erc--check-msg-prop 'a)))
    (should (erc--check-msg-prop 'a 1))
    (should-not (erc--check-msg-prop 'a 2))

    (should (eq 'x (erc--check-msg-prop 'b)))
    (should (erc--check-msg-prop 'b 'x))
    (should-not (erc--check-msg-prop 'b 1))

    (should (erc--check-msg-prop 'a '(1 42)))
    (should-not (erc--check-msg-prop 'a '(2 42)))

    (let ((props '(42 x)))
      (should (erc--check-msg-prop 'b props)))
    (let ((v '(42 y)))
      (should-not (erc--check-msg-prop 'b v)))))

(ert-deftest erc--memq-msg-prop ()
  (let ((erc--msg-props (map-into '((a . 1) (b x y)) 'hash-table)))
    (should-not (erc--memq-msg-prop 'a 1))
    (should-not (erc--memq-msg-prop 'b 'z))
    (should (erc--memq-msg-prop 'b 'x))
    (should (erc--memq-msg-prop 'b 'y))))

(ert-deftest erc--merge-prop ()
  (with-current-buffer (get-buffer-create "*erc-test*")
    ;; Baseline.
    (insert "abc\n")
    (erc--merge-prop 1 3 'erc-test 'x)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc" 0 2 (erc-test x))))
    (erc--merge-prop 1 3 'erc-test 'y)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc" 0 2 (erc-test (y x)))))

    ;; Multiple intervals.
    (goto-char (point-min))
    (insert "def\n")
    (erc--merge-prop 1 2 'erc-test 'x)
    (erc--merge-prop 2 3 'erc-test 'y)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4)
             #("def" 0 1 (erc-test x) 1 2 (erc-test y))))
    (erc--merge-prop 1 3 'erc-test 'z)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4)
             #("def" 0 1 (erc-test (z x)) 1 2 (erc-test (z y)))))

    ;; New val as list.
    (goto-char (point-min))
    (insert "ghi\n")
    (erc--merge-prop 2 3 'erc-test '(y z))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi" 1 2 (erc-test (y z)))))
    (erc--merge-prop 1 3 'erc-test '(w x))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4)
             #("ghi" 0 1 (erc-test (w x)) 1 2 (erc-test (w x y z)))))

    ;; Flag `erc--merge-prop-behind-p'.
    (goto-char (point-min))
    (insert "jkl\n")
    (erc--merge-prop 2 3 'erc-test '(y z))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("jkl" 1 2 (erc-test (y z)))))
    (let ((erc--merge-prop-behind-p t))
      (erc--merge-prop 1 3 'erc-test '(w x)))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4)
             #("jkl" 0 1 (erc-test (w x)) 1 2 (erc-test (y z w x)))))

    (when noninteractive
      (kill-buffer))))

(ert-deftest erc--remove-from-prop-value-list ()
  (with-current-buffer (get-buffer-create "*erc-test*")
    ;; Non-list match.
    (insert "abc\n")
    (put-text-property 1 2 'erc-test 'a)
    (put-text-property 2 3 'erc-test 'b)
    (put-text-property 3 4 'erc-test 'c)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc"
                                      0 1 (erc-test a)
                                      1 2 (erc-test b)
                                      2 3 (erc-test c))))

    (erc--remove-from-prop-value-list 1 4 'erc-test 'b)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc"
                                      0 1 (erc-test a)
                                      2 3 (erc-test c))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'a)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc" 2 3 (erc-test c))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'c)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) "abc"))

    ;; List match.
    (goto-char (point-min))
    (insert "def\n")
    (put-text-property 1 2 'erc-test '(d x))
    (put-text-property 2 3 'erc-test '(e y))
    (put-text-property 3 4 'erc-test '(f z))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("def"
                                      0 1 (erc-test (d x))
                                      1 2 (erc-test (e y))
                                      2 3 (erc-test (f z)))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'y)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("def"
                                      0 1 (erc-test (d x))
                                      1 2 (erc-test e)
                                      2 3 (erc-test (f z)))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'd)
    (erc--remove-from-prop-value-list 1 4 'erc-test 'f)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("def"
                                      0 1 (erc-test x)
                                      1 2 (erc-test e)
                                      2 3 (erc-test z))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'e)
    (erc--remove-from-prop-value-list 1 4 'erc-test 'z)
    (erc--remove-from-prop-value-list 1 4 'erc-test 'x)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) "def"))

    ;; List match.
    (goto-char (point-min))
    (insert "ghi\n")
    (put-text-property 1 2 'erc-test '(g x))
    (put-text-property 2 3 'erc-test '(h x))
    (put-text-property 3 4 'erc-test '(i y))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi"
                                      0 1 (erc-test (g x))
                                      1 2 (erc-test (h x))
                                      2 3 (erc-test (i y)))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'x)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi"
                                      0 1 (erc-test g)
                                      1 2 (erc-test h)
                                      2 3 (erc-test (i y)))))
    (erc--remove-from-prop-value-list 1 2 'erc-test 'g) ; narrowed
    (erc--remove-from-prop-value-list 3 4 'erc-test 'i) ; narrowed
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi"
                                      1 2 (erc-test h)
                                      2 3 (erc-test y))))

    ;; Pathological (,c) case (hopefully not created by ERC)
    (goto-char (point-min))
    (insert "jkl\n")
    (put-text-property 1 2 'erc-test '(j x))
    (put-text-property 2 3 'erc-test '(k))
    (put-text-property 3 4 'erc-test '(k))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'k)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("jkl" 0 1 (erc-test (j x)))))

    (when noninteractive
      (kill-buffer))))

(ert-deftest erc--remove-from-prop-value-list/many ()
  (with-current-buffer (get-buffer-create "*erc-test*")
    ;; Non-list match.
    (insert "abc\n")
    (put-text-property 1 2 'erc-test 'a)
    (put-text-property 2 3 'erc-test 'b)
    (put-text-property 3 4 'erc-test 'c)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc"
                                      0 1 (erc-test a)
                                      1 2 (erc-test b)
                                      2 3 (erc-test c))))

    (erc--remove-from-prop-value-list 1 4 'erc-test '(a b))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc" 2 3 (erc-test c))))
    (erc--remove-from-prop-value-list 1 4 'erc-test 'a)
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("abc" 2 3 (erc-test c))))
    (erc--remove-from-prop-value-list 1 4 'erc-test '(c))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) "abc"))

    ;; List match.
    (goto-char (point-min))
    (insert "def\n")
    (put-text-property 1 2 'erc-test '(d x y))
    (put-text-property 2 3 'erc-test '(e y))
    (put-text-property 3 4 'erc-test '(f z))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("def"
                                      0 1 (erc-test (d x y))
                                      1 2 (erc-test (e y))
                                      2 3 (erc-test (f z)))))
    (erc--remove-from-prop-value-list 1 4 'erc-test '(d y f))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("def"
                                      0 1 (erc-test x)
                                      1 2 (erc-test e)
                                      2 3 (erc-test z))))
    (erc--remove-from-prop-value-list 1 4 'erc-test '(e z x))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) "def"))

    ;; Narrowed beg.
    (goto-char (point-min))
    (insert "ghi\n")
    (put-text-property 1 2 'erc-test '(g x))
    (put-text-property 2 3 'erc-test '(h x))
    (put-text-property 3 4 'erc-test '(i x))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi"
                                      0 1 (erc-test (g x))
                                      1 2 (erc-test (h x))
                                      2 3 (erc-test (i x)))))
    (erc--remove-from-prop-value-list 1 3 'erc-test '(x g i))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("ghi"
                                      1 2 (erc-test h)
                                      2 3 (erc-test (i x)))))

    ;; Narrowed middle.
    (goto-char (point-min))
    (insert "jkl\n")
    (put-text-property 1 2 'erc-test '(j x))
    (put-text-property 2 3 'erc-test '(k))
    (put-text-property 3 4 'erc-test '(l y z))
    (erc--remove-from-prop-value-list 3 4 'erc-test '(k x y z))
    (should (erc-tests-common-equal-with-props
             (buffer-substring 1 4) #("jkl"
                                      0 1 (erc-test (j x))
                                      1 2 (erc-test (k))
                                      2 3 (erc-test l))))

    (when noninteractive
      (kill-buffer))))

(ert-deftest erc--restore-important-text-props ()
  (erc-mode)
  (let ((erc--msg-props (map-into '((erc--important-prop-names a))
                                  'hash-table)))
    (insert (propertize "foo" 'a 'A 'b 'B 'erc--important-props '(a A))
            " "
            (propertize "bar" 'c 'C 'a 'A 'b 'B
                        'erc--important-props '(a A c C)))

    ;; Attempt to restore a and c when only a is registered.
    (remove-list-of-text-properties (point-min) (point-max) '(a c))
    (erc--restore-important-text-props '(a c))
    (should (erc-tests-common-equal-with-props
             (buffer-string)
             #("foo bar"
               0 3 (a A b B erc--important-props (a A))
               4 7 (a A b B erc--important-props (a A c C)))))

    ;; Add d between 3 and 6.
    (erc--reserve-important-text-props 3 6 '(d D))
    (put-text-property 3 6 'd 'D)
    (should (erc-tests-common-equal-with-props
             (buffer-string)
             #("foo bar" ; #1
               0 2 (a A b B erc--important-props (a A))
               2 3 (d D a A b B erc--important-props (d D a A))
               3 4 (d D erc--important-props (d D))
               4 5 (d D a A b B erc--important-props (d D a A c C))
               5 7 (a A b B erc--important-props (a A c C)))))
    ;; Remove a and d, and attempt to restore d.
    (remove-list-of-text-properties (point-min) (point-max) '(a d))
    (erc--restore-important-text-props '(d))
    (should (erc-tests-common-equal-with-props
             (buffer-string)
             #("foo bar"
               0 2 (b B erc--important-props (a A))
               2 3 (d D b B erc--important-props (d D a A))
               3 4 (d D erc--important-props (d D))
               4 5 (d D b B erc--important-props (d D a A c C))
               5 7 (b B erc--important-props (a A c C)))))

    ;; Restore a only.
    (erc--restore-important-text-props '(a))
    (should (erc-tests-common-equal-with-props
             (buffer-string)
             #("foo bar" ; same as #1 above
               0 2 (a A b B erc--important-props (a A))
               2 3 (d D a A b B erc--important-props (d D a A))
               3 4 (d D erc--important-props (d D))
               4 5 (d D a A b B erc--important-props (d D a A c C))
               5 7 (a A b B erc--important-props (a A c C)))))))

(ert-deftest erc--split-string-shell-cmd ()

  ;; Leading and trailing space
  (should (equal (erc--split-string-shell-cmd "1 2 3") '("1" "2" "3")))
  (should (equal (erc--split-string-shell-cmd " 1  2 3 ") '("1" "2" "3")))

  ;; Empty string
  (should (equal (erc--split-string-shell-cmd "\"\"") '("")))
  (should (equal (erc--split-string-shell-cmd " \"\" ") '("")))
  (should (equal (erc--split-string-shell-cmd "1 \"\"") '("1" "")))
  (should (equal (erc--split-string-shell-cmd "1 \"\" ") '("1" "")))
  (should (equal (erc--split-string-shell-cmd "\"\" 1") '("" "1")))
  (should (equal (erc--split-string-shell-cmd " \"\" 1") '("" "1")))

  (should (equal (erc--split-string-shell-cmd "''") '("")))
  (should (equal (erc--split-string-shell-cmd " '' ") '("")))
  (should (equal (erc--split-string-shell-cmd "1 ''") '("1" "")))
  (should (equal (erc--split-string-shell-cmd "1 '' ") '("1" "")))
  (should (equal (erc--split-string-shell-cmd "'' 1") '("" "1")))
  (should (equal (erc--split-string-shell-cmd " '' 1") '("" "1")))

  ;; Backslash
  (should (equal (erc--split-string-shell-cmd "\\ ") '(" ")))
  (should (equal (erc--split-string-shell-cmd " \\  ") '(" ")))
  (should (equal (erc--split-string-shell-cmd "1\\  ") '("1 ")))
  (should (equal (erc--split-string-shell-cmd "1\\ 2") '("1 2")))

  ;; Embedded
  (should (equal (erc--split-string-shell-cmd "\"\\\"\"") '("\"")))
  (should (equal (erc--split-string-shell-cmd "1 \"2 \\\" \\\" 3\"")
                 '("1" "2 \" \" 3")))
  (should (equal (erc--split-string-shell-cmd "1 \"2 ' ' 3\"")
                 '("1" "2 ' ' 3")))
  (should (equal (erc--split-string-shell-cmd "1 '2 \" \" 3'")
                 '("1" "2 \" \" 3")))
  (should (equal (erc--split-string-shell-cmd "1 '2 \\  3'")
                 '("1" "2 \\  3")))
  (should (equal (erc--split-string-shell-cmd "1 \"2 \\\\  3\"")
                 '("1" "2 \\  3"))) ; see comment re ^

  ;; Realistic
  (should (equal (erc--split-string-shell-cmd "GET bob \"my file.txt\"")
                 '("GET" "bob" "my file.txt")))
  (should (equal (erc--split-string-shell-cmd "GET EXAMPLE|bob \"my file.txt\"")
                 '("GET" "EXAMPLE|bob" "my file.txt")))) ; regression


;; The behavior of `erc-pre-send-functions' differs between versions
;; in how hook members see and influence a trailing newline that's
;; part of the original prompt submission:
;;
;;  5.4: both seen and sent
;;  5.5: seen but not sent*
;;  5.6: neither seen nor sent*
;;
;;  * requires `erc-send-whitespace-lines' for hook to run
;;
;; Two aspects that have remained consistent are
;;
;;   - a final nonempty line in any submission is always sent
;;   - a trailing newline appended by a hook member is always sent
;;
;; The last bullet would seem to contradict the "not sent" behavior of
;; 5.5 and 5.6, but what's actually happening is that exactly one
;; trailing newline is culled, so anything added always goes through.
;; Also, in ERC 5.6, all empty lines are actually padded, but this is
;; merely incidental WRT the above.
;;
;; Note that this test doesn't run any input-prep hooks and thus can't
;; account for the "seen" dimension noted above.

(ert-deftest erc--run-send-hooks ()
  (with-suppressed-warnings ((obsolete erc-send-this)
                             (obsolete erc-send-pre-hook))
    (should erc-insert-this)
    (should erc-send-this) ; populates `erc--input-split-sendp'

    (let (erc-pre-send-functions erc-send-pre-hook)

      (ert-info ("String preserved, lines rewritten, empties padded")
        (setq erc-pre-send-functions
              (lambda (o) (setf (erc-input-string o) "bar\n\nbaz\n")))
        (should (pcase (erc--run-send-hooks (make-erc--input-split
                                             :string "foo" :lines '("foo")))
                  ((cl-struct erc--input-split
                              (string "foo") (sendp 't) (insertp 't)
                              (lines '("bar" " " "baz" " ")) (cmdp 'nil))
                   t))))

      (ert-info ("Multiline commands rejected")
        (should-error (erc--run-send-hooks (make-erc--input-split
                                            :string "/mycmd foo"
                                            :lines '("/mycmd foo")
                                            :cmdp t))))

      (ert-info ("Single-line commands pass")
        (setq erc-pre-send-functions
              (lambda (o) (setf (erc-input-sendp o) nil
                                (erc-input-string o) "/mycmd bar")))
        (should (pcase (erc--run-send-hooks (make-erc--input-split
                                             :string "/mycmd foo"
                                             :lines '("/mycmd foo")
                                             :cmdp t))
                  ((cl-struct erc--input-split
                              (string "/mycmd foo") (sendp 'nil) (insertp 't)
                              (lines '("/mycmd bar")) (cmdp 't))
                   t))))

      (ert-info ("Legacy hook respected, special vars confined")
        (setq erc-send-pre-hook (lambda (_) (setq erc-send-this nil))
              erc-pre-send-functions (lambda (o) ; propagates
                                       (should-not (erc-input-sendp o))))
        (should (pcase (erc--run-send-hooks (make-erc--input-split
                                             :string "foo" :lines '("foo")))
                  ((cl-struct erc--input-split
                              (string "foo") (sendp 'nil) (insertp 't)
                              (lines '("foo")) (cmdp 'nil))
                   t)))
        (should erc-send-this))

      (ert-info ("Request to resplit honored")
        (setq erc-send-pre-hook nil
              erc-pre-send-functions
              (lambda (o) (setf (erc-input-string o) "foo bar baz"
                                (erc-input-refoldp o) t)))
        (let* ((split (make-erc--input-split :string "foo" :lines '("foo")))
               (erc--current-line-input-split split)
               (erc-split-line-length 8))
          (should
           (pcase (erc--run-send-hooks split)
             ((cl-struct erc--input-split
                         (string "foo") (sendp 't) (insertp 't)
                         (lines '("foo bar " "baz")) (cmdp 'nil))
              t))))))))

;; Note: if adding an erc-backend-tests.el, please relocate this there.

(ert-deftest erc-message ()
  (should-not erc-server-last-peers)
  (let (server-proc
        calls
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (cl-letf (((symbol-function 'erc-display-message)
               (lambda (_ _ _ msg &rest args)
                 (ignore (push (apply #'erc-format-message msg args) calls))))
              ((symbol-function 'erc-server-send)
               (lambda (line _) (push line calls)))
              ((symbol-function 'erc-server-buffer)
               (lambda () (process-buffer server-proc))))
      (with-current-buffer (get-buffer-create "ExampleNet")
        (erc-mode)
        (setq erc-server-current-nick "tester"
              server-proc (start-process "sleep" (current-buffer) "sleep" "1")
              erc-server-process server-proc
              erc-server-last-peers (cons nil nil)
              erc-server-users (make-hash-table :test 'equal)
              erc-network 'ExampleNet)
        (set-process-query-on-exit-flag erc-server-process nil))

      (with-current-buffer (get-buffer-create "#chan")
        (erc-mode)
        (setq erc-server-process (buffer-local-value 'erc-server-process
                                                     (get-buffer "ExampleNet"))
              erc--target (erc--target-from-string "#chan")
              erc-default-recipients '("#chan")
              erc-channel-users (make-hash-table :test 'equal)
              erc-network 'ExampleNet)
        (erc-update-current-channel-member "alice" "alice")
        (erc-update-current-channel-member "tester" "tester"))

      (with-current-buffer "ExampleNet"
        (erc-server-PRIVMSG erc-server-process
                            (make-erc-response
                             :sender "alice!~u@fsf.org"
                             :command "PRIVMSG"
                             :command-args '("#chan" "hi")
                             :unparsed ":alice!~u@fsf.org PRIVMSG #chan :hi"))
        (should (equal erc-server-last-peers '("alice")))
        (should (string-match "<alice>" (pop calls))))

      (with-current-buffer "#chan"
        (ert-info ("Shortcuts usable in target buffers")
          (should-not (local-variable-p 'erc-server-last-peers))
          (should-not erc-server-last-peers)
          (erc-message "PRIVMSG" ". hi")
          (should-not erc-server-last-peers)
          (should (equal "No target" (pop calls)))
          (erc-message "PRIVMSG" ", hi")
          (should-not erc-server-last-peers)
          (should (string-match "alice :hi" (pop calls)))))

      (with-current-buffer "ExampleNet"
        (ert-info ("Shortcuts local in server bufs")
          (should (equal erc-server-last-peers '("alice" . "alice")))
          (erc-message "PRIVMSG" ", hi")
          (should (equal erc-server-last-peers '("alice" . "alice")))
          (should (string-match "PRIVMSG alice :hi" (pop calls)))
          (setcdr erc-server-last-peers "bob")
          (erc-message "PRIVMSG" ". hi")
          (should (equal erc-server-last-peers '("alice" . "bob")))
          (should (string-match "PRIVMSG bob :hi" (pop calls)))))

      (with-current-buffer "#chan"
        (ert-info ("Non-shortcuts are local to server buffer")
          (should-not (local-variable-p 'erc-server-last-peers))
          (should-not erc-server-last-peers)
          (erc-message "PRIVMSG" "#chan hola")
          (should-not erc-server-last-peers)
          (should-not (default-value 'erc-server-last-peers))
          (should (equal (buffer-local-value 'erc-server-last-peers
                                             (get-buffer "ExampleNet"))
                         '("alice" . "#chan")))
          (should (string-match "hola" (pop calls))))))

    (should-not erc-server-last-peers)
    (should-not calls)
    (kill-buffer "ExampleNet")
    (kill-buffer "#chan")))

(ert-deftest erc-get-channel-membership-prefix ()
  (ert-info ("Uses default prefixes when `erc--parsed-prefix' not available")
    (should-not (erc--parsed-prefix))
    ;; Baseline.
    (should-not (erc-get-channel-membership-prefix nil))
    (should (equal (erc-get-channel-membership-prefix "Bob") ""))
    (should (equal (erc-get-channel-membership-prefix (make-erc-channel-user))
                   ""))
    ;; Defaults.
    (should
     (erc-tests-common-equal-with-props
      (erc-get-channel-membership-prefix (make-erc-channel-user :owner t))
      #("~" 0 1 (help-echo "owner"))))
    (should
     (erc-tests-common-equal-with-props
      (erc-get-channel-membership-prefix (make-erc-channel-user :admin t))
      #("&" 0 1 (help-echo "admin"))))
    (should
     (erc-tests-common-equal-with-props
      (erc-get-channel-membership-prefix (make-erc-channel-user :op t))
      #("@" 0 1 (help-echo "operator"))))
    (should
     (erc-tests-common-equal-with-props
      (erc-get-channel-membership-prefix (make-erc-channel-user :halfop t))
      #("%" 0 1 (help-echo "half-op"))))
    (should
     (erc-tests-common-equal-with-props
      (erc-get-channel-membership-prefix (make-erc-channel-user :voice t))
      #("+" 0 1 (help-echo "voice")))))

  (ert-info ("Uses advertised prefixes when `erc--parsed-prefix' is available")
    (erc-tests-common-make-server-buf (buffer-name))
    (push '("PREFIX" . "(ov)@+") erc-server-parameters)
    (should (erc--parsed-prefix))

    (with-current-buffer (erc--open-target "#chan")
      (erc-update-current-channel-member "Bob" nil t nil nil 'on)

      ;; Baseline.
      (should-not (erc-get-channel-membership-prefix nil))
      (should (string-empty-p (erc-get-channel-membership-prefix
                               (make-erc-channel-user))))

      ;; Defaults.
      (should (string-empty-p (erc-get-channel-membership-prefix
                               (make-erc-channel-user :owner t))))
      (should (string-empty-p (erc-get-channel-membership-prefix
                               (make-erc-channel-user :admin t))))
      (should (string-empty-p (erc-get-channel-membership-prefix
                               (make-erc-channel-user :halfop t))))

      (should (erc-tests-common-equal-with-props
               (erc-get-channel-membership-prefix "Bob")
               #("@" 0 1 (help-echo "operator"))))
      (should (erc-tests-common-equal-with-props
               (erc-get-channel-membership-prefix
                (make-erc-channel-user :voice t))
               #("+" 0 1 (help-echo "voice"))))

      (kill-buffer))))

;; This is an adapter that uses formatting templates from the
;; `-speaker' catalog to mimic `erc-format-privmessage', for testing
;; purposes.
(defun erc-tests--format-privmessage (nick msg privp msgp &optional inputp pfx)
  (let ((erc-current-message-catalog erc--message-speaker-catalog))
    (apply #'erc-format-message
           (erc--determine-speaker-message-format-args nick msg privp msgp
                                                       inputp nil pfx))))

;; This test demonstrates that ERC uses the same string for the
;; `erc--spkr' and `erc--speaker' text properties, which it gets from
;; the `nickname' shot of the speaker's server user.
(ert-deftest erc--speakerize-nick ()
  (erc-tests-common-make-server-buf)
  (setq erc-server-current-nick "tester")

  (let ((sentinel "alice"))
    (with-current-buffer (erc--open-target "#chan")
      (erc-update-current-channel-member "bob" "bob" t nil nil nil nil nil
                                         "example.org" "~u" "bob")
      (erc-update-current-channel-member "alice" sentinel t nil nil nil nil nil
                                         "fsf.org" "~u" "alice"))

    (erc-call-hooks nil (make-erc-response
                         :sender "alice!~u@fsf.org"
                         :command "PRIVMSG"
                         :command-args '("#chan" "one")
                         :contents "one"
                         :unparsed ":alice!~u@fsf.org PRIVMSG #chan :one"))
    (erc-call-hooks nil (make-erc-response
                         :sender "bob!~u@example.org"
                         :command "PRIVMSG"
                         :command-args '("#chan" "hi")
                         :contents "hi"
                         :unparsed ":bob!~u@example.org PRIVMSG #chan :hi"))
    (erc-call-hooks nil (make-erc-response
                         :sender "alice!~u@fsf.org"
                         :command "PRIVMSG"
                         :command-args '("#chan" "two")
                         :contents "two"
                         :unparsed ":alice!~u@fsf.org PRIVMSG #chan :two"))

    (with-current-buffer (get-buffer "#chan")
      (should (eq sentinel
                  (erc-server-user-nickname (erc-get-server-user "alice"))))
      (goto-char (point-min))

      (should (search-forward "<a" nil t))
      (should (looking-at "lice> one"))
      (should (eq (get-text-property (point) 'erc--speaker) sentinel))
      (should (eq (erc--get-inserted-msg-prop 'erc--spkr) sentinel))

      (should (search-forward "<bob> hi" nil t))

      (should (search-forward "<a" nil t))
      (should (looking-at "lice> two"))
      (should (eq (get-text-property (point) 'erc--speaker) sentinel))
      (should (eq (erc--get-inserted-msg-prop 'erc--spkr) sentinel))

      (when noninteractive (kill-buffer)))))

;; This asserts that `erc--determine-speaker-message-format-args'
;; behaves identically to `erc-format-privmessage', the function whose
;; role it basically replaced.
(ert-deftest erc--determine-speaker-message-format-args ()
  ;; Basic PRIVMSG.
  (let ((expect #("<bob> oh my"
                  0 1 (font-lock-face erc-default-face)
                  1 4 (erc--speaker "bob" font-lock-face erc-nick-default-face)
                  4 11 (font-lock-face erc-default-face)))
        (args (list (concat "bob") (concat "oh my") nil 'msgp)))
    (should (erc-tests-common-equal-with-props
             (apply #'erc-format-privmessage args)
             expect))
    (should (erc-tests-common-equal-with-props
             (apply #'erc-tests--format-privmessage args)
             expect)))

  ;; Basic NOTICE.
  (let ((expect #("-bob- oh my"
                  0 1 (font-lock-face erc-default-face)
                  1 4 (erc--speaker "bob" font-lock-face erc-nick-default-face)
                  4 11 (font-lock-face erc-default-face)))
        (args (list (copy-sequence "bob") (copy-sequence "oh my") nil nil)))
    (should (erc-tests-common-equal-with-props
             (apply #'erc-format-privmessage args)
             expect))
    (should (erc-tests-common-equal-with-props
             (apply #'erc-tests--format-privmessage args)
             expect)))

  ;; Status-prefixed PRIVMSG.
  (let* ((expect
          #("<@Bob> oh my"
            0 1 (font-lock-face erc-default-face)
            1 2 (font-lock-face erc-nick-prefix-face help-echo "operator")
            2 5 (erc--speaker "Bob" font-lock-face erc-nick-default-face)
            5 12 (font-lock-face erc-default-face)))
         (user (make-erc-server-user :nickname (copy-sequence "Bob")))
         (cuser (make-erc-channel-user :op t))
         (erc-channel-users (make-hash-table :test #'equal)))
    (puthash "bob" (cons user cuser) erc-channel-users)

    (with-suppressed-warnings ((obsolete erc-format-@nick))
      (should (erc-tests-common-equal-with-props
               (erc-format-privmessage (erc-format-@nick user cuser)
                                       (copy-sequence "oh my")
                                       nil 'msgp)
               expect)))
    (let ((nick "Bob")
          (msg "oh my"))
      (should (erc-tests-common-equal-with-props
               (erc-tests--format-privmessage nick msg nil 'msgp nil cuser)
               expect)) ; overloaded on PREFIX arg
      (should (erc-tests-common-equal-with-props
               (erc-tests--format-privmessage nick msg nil 'msgp nil t)
               expect))
      ;; The new version makes a copy instead of adding properties to
      ;; the input.
      (should-not
       (text-property-not-all 0 (length nick) 'font-lock-face nil nick))
      (should-not
       (text-property-not-all 0 (length msg) 'font-lock-face nil msg)))))

(ert-deftest erc--determine-speaker-message-format-args/queries-as-channel ()
  (should erc-format-query-as-channel-p)

  (with-current-buffer (get-buffer-create "bob")
    (erc-mode)
    (setq erc--target (erc--target-from-string "alice"))

    (insert "PRIVMSG\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp 'msgp))
    (should (erc-tests-common-equal-with-props
             #("<bob> oh my"
               0 1 (font-lock-face erc-default-face)
               1 4 (erc--speaker "bob" font-lock-face erc-nick-default-face)
               4 11 (font-lock-face erc-default-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nNOTICE\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp nil))
    (should (erc-tests-common-equal-with-props
             #("-bob- oh my"
               0 1 (font-lock-face erc-default-face)
               1 4 (erc--speaker "bob" font-lock-face erc-nick-default-face)
               4 11 (font-lock-face erc-default-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nInput PRIVMSG\n"
            (erc-tests--format-privmessage "bob" "oh my"
                                           'queryp 'privmsgp 'inputp))
    (should (erc-tests-common-equal-with-props
             #("<bob> oh my"
               0 1 (font-lock-face erc-default-face)
               1 4 (erc--speaker "bob" font-lock-face erc-my-nick-face)
               4 6 (font-lock-face erc-default-face)
               6 11 (font-lock-face erc-input-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nInput NOTICE\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp nil 'inputp))
    (should (erc-tests-common-equal-with-props
             #("-bob- oh my"
               0 1 (font-lock-face erc-default-face)
               1 4 (erc--speaker "bob" font-lock-face erc-my-nick-face)
               4 6 (font-lock-face erc-default-face)
               6 11 (font-lock-face erc-input-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (when noninteractive (kill-buffer))))

(ert-deftest erc--determine-speaker-message-format-args/queries ()
  (should erc-format-query-as-channel-p)

  (with-current-buffer (get-buffer-create "bob")
    (erc-mode)
    (setq-local erc-format-query-as-channel-p nil)
    (setq erc--target (erc--target-from-string "alice"))

    (insert "PRIVMSG\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp 'msgp))
    (should (erc-tests-common-equal-with-props
             #("*bob* oh my"
               0 1 (font-lock-face erc-direct-msg-face)
               1 4 (erc--speaker "bob" font-lock-face erc-nick-msg-face)
               4 11 (font-lock-face erc-direct-msg-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nNOTICE\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp nil))
    (should (erc-tests-common-equal-with-props
             #("-bob- oh my"
               0 1 (font-lock-face erc-direct-msg-face)
               1 4 (erc--speaker "bob" font-lock-face erc-nick-msg-face)
               4 11 (font-lock-face erc-direct-msg-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nInput PRIVMSG\n"
            (erc-tests--format-privmessage "bob" "oh my"
                                           'queryp 'privmsgp 'inputp))
    (should (erc-tests-common-equal-with-props
             #("*bob* oh my"
               0 1 (font-lock-face erc-direct-msg-face)
               1 4 (erc--speaker "bob" font-lock-face erc-my-nick-face)
               4 6 (font-lock-face erc-direct-msg-face)
               6 11 (font-lock-face erc-input-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (insert "\nInput NOTICE\n"
            (erc-tests--format-privmessage "bob" "oh my" 'queryp nil 'inputp))
    (should (erc-tests-common-equal-with-props
             #("-bob- oh my"
               0 1 (font-lock-face erc-direct-msg-face)
               1 4 (erc--speaker "bob" font-lock-face erc-my-nick-face)
               4 6 (font-lock-face erc-direct-msg-face)
               6 11 (font-lock-face erc-input-face))
             (buffer-substring (pos-bol) (pos-eol))))

    (when noninteractive (kill-buffer))))

(defun erc-tests--format-my-nick (message)
  (concat (erc-format-my-nick)
          (propertize message 'font-lock-face 'erc-input-face)))

;; This tests that the default behavior of the replacement formatting
;; function for prompt input, `erc--format-speaker-input-message'
;; matches that of the original being replaced, `erc-format-my-nick',
;; though it only handled the speaker portion.
(ert-deftest erc--format-speaker-input-message ()
  ;; No status prefix.
  (let ((erc-server-current-nick "tester")
        (expect #("<tester> oh my"
                  0 1 (font-lock-face erc-default-face)
                  1 7 (font-lock-face erc-my-nick-face erc--speaker "tester")
                  7 9 (font-lock-face erc-default-face)
                  9 14 (font-lock-face erc-input-face))))
    (should (equal (erc-tests--format-my-nick "oh my") expect))
    (should (equal (erc--format-speaker-input-message "oh my") expect)))

  ;; With channel-operator status prefix.
  (let* ((erc-server-current-nick "tester")
         (cmem (cons (make-erc-server-user :nickname "tester")
                     (make-erc-channel-user :op t)))
         (erc-channel-users (map-into (list "tester" cmem)
                                      '(hash-table :test equal)))
         (expect #("<@tester> oh my"
                   0 1 (font-lock-face erc-default-face)
                   1 2 (font-lock-face erc-my-nick-prefix-face)
                   2 5 (font-lock-face erc-my-nick-face erc--speaker "bob")
                   5 7 (font-lock-face erc-default-face)
                   7 12 (font-lock-face erc-input-face))))
    (should (equal (erc-tests--format-my-nick "oh my") expect))
    (should (equal (erc--format-speaker-input-message "oh my") expect))))

(ert-deftest erc-update-undo-list ()
  ;; Remove `stamp' so this can run in any locale.  Alternatively, we
  ;; could explicitly enable it and bind its format options to strings
  ;; that lack specifiers (perhaps in a separate test).
  (let ((erc-modules (remq 'stamp erc-modules))
        (erc-mode-hook erc-mode-hook)
        (erc-insert-modify-hook erc-insert-modify-hook)
        (erc-send-modify-hook erc-send-modify-hook)
        (inhibit-message noninteractive)
        marker)

    (erc-stamp-mode -1)
    (erc-tests-common-make-server-buf)
    (setq erc-server-current-nick "tester")

    (with-current-buffer (erc--open-target "#chan")
      ;; Add some filler to simulate more realistic values.
      (erc-tests-common-simulate-line
       ":irc.foonet.org 353 tester = #chan :bob tester alice")
      (erc-tests-common-simulate-line
       ":irc.foonet.org 366 tester #chan :End of NAMES list")
      (should (erc-get-server-user "bob"))

      (goto-char (point-max))
      (should (= (point) 45))

      ;; Populate undo list with contrived values.
      (let ((kill-ring (list "abc"))
            interprogram-paste-function)
        (yank))
      (push nil buffer-undo-list)
      (push (point-max) buffer-undo-list)
      (setq marker (point-marker))
      (put-text-property 46 47 'face 'warning)
      (call-interactively #'delete-backward-char 1)
      (push nil buffer-undo-list)
      (should (= (point) 47))
      (should (equal buffer-undo-list `(nil
                                        ("c" . -47)
                                        (,marker . -1)
                                        (nil face nil 46 . 47)
                                        48
                                        nil
                                        (45 . 48))))

      ;; The first char after the prompt is at buffer pos 45.
      (should (= 40 (- 45 (length (erc-prompt))) erc-insert-marker))

      ;; A new message arrives, growing the buffer by 11 chars.
      (erc-tests-common-simulate-privmsg "bob" "test")
      (should (equal (buffer-substring 40 erc-insert-marker) "<bob> test\n"))
      (should (= (point-max) 58))
      (should (= 11 (length "<bob> test\n") (- (point) 47)))

      ;; The list remains unchanged relative to the end of the buffer.
      (should (equal buffer-undo-list `(nil
                                        ("c" . -58)
                                        (,marker . -1)
                                        (nil face nil 57 . 58)
                                        59
                                        nil
                                        (56 . 59))))

      ;; Undo behavior works as expected.
      (undo nil)
      (should (erc-tests-common-equal-with-props
               (buffer-substring erc-input-marker (point-max))
               #("abc" 1 2 (face nil))))
      (should (equal (take 4 buffer-undo-list)
                     `((nil face warning 57 . 58)
                       (58 . 59)
                       nil
                       ("c" . -58))))
      (undo 2)
      (should (string-empty-p (erc-user-input)))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc--route-insertion ()
  (erc-tests-common-prep-for-insertion)
  (erc-tests-common-init-server-proc "sleep" "1")
  (setq erc-networks--id (erc-networks--id-create 'foonet))

  (let* ((erc-modules) ; for `erc--open-target'
         (server-buffer (current-buffer))
         (spam-buffer (save-excursion (erc--open-target "#spam")))
         (chan-buffer (save-excursion (erc--open-target "#chan")))
         calls)
    (cl-letf (((symbol-function 'erc-insert-line)
               (lambda (&rest r) (push (cons 'line-1 r) calls))))

      (with-current-buffer chan-buffer

        (ert-info ("Null `buffer' routes to live server-buffer")
          (erc--route-insertion "null" nil)
          (should (equal (pop calls) `(line-1 "null" ,server-buffer)))
          (should-not calls))

        (ert-info ("Cons `buffer' routes to live members")
          ;; Copies a let-bound `erc--msg-props' before mutating.
          (let* ((table (map-into '(erc--msg msg) 'hash-table))
                 (erc--msg-props table))
            (erc--route-insertion "cons" (list server-buffer spam-buffer))
            (should-not (eq table erc--msg-props)))
          (should (equal (pop calls) `(line-1 "cons" ,spam-buffer)))
          (should (equal (pop calls) `(line-1 "cons" ,server-buffer)))
          (should-not calls))

        (ert-info ("Variant `all' inserts in all session buffers")
          (erc--route-insertion "all" 'all)
          (should (equal (pop calls) `(line-1 "all" ,chan-buffer)))
          (should (equal (pop calls) `(line-1 "all" ,spam-buffer)))
          (should (equal (pop calls) `(line-1 "all" ,server-buffer)))
          (should-not calls))

        (ert-info ("Variant `active' routes to active buffer if alive")
          (should (eq chan-buffer (erc-with-server-buffer erc-active-buffer)))
          (erc-set-active-buffer spam-buffer)
          (erc--route-insertion "act" 'active)
          (should (equal (pop calls) `(line-1 "act" ,spam-buffer)))
          (should (eq (erc-active-buffer) spam-buffer))
          (should-not calls))

        (ert-info ("Variant `active' falls back to current buffer")
          (should (eq spam-buffer (erc-active-buffer)))
          (kill-buffer "#spam")
          (erc--route-insertion "nact" 'active)
          (should (equal (pop calls) `(line-1 "nact" ,server-buffer)))
          (should (eq (erc-with-server-buffer erc-active-buffer)
                      server-buffer))
          (should-not calls))

        (ert-info ("Dead single buffer defaults to live server-buffer")
          (should-not (get-buffer "#spam"))
          (erc--route-insertion "dead" 'spam-buffer)
          (should (equal (pop calls) `(line-1 "dead" ,server-buffer)))
          (should-not calls))))

    (should-not (buffer-live-p spam-buffer))
    (kill-buffer chan-buffer)))

(ert-deftest erc-normalize-port ()
  ;; The empty string, nil, and unsupported types become nil.
  (should-not (erc-normalize-port ""))
  (should-not (erc-normalize-port nil))
  (should-not (erc-normalize-port (current-buffer)))

  ;; Unrecognized names are coerced to 0.
  (should (equal 0 (erc-normalize-port "fake")))

  ;; Numbers pass through, but numeric strings are coerced.
  (should (equal 6667 (erc-normalize-port 6667)))
  (should (equal 6697 (erc-normalize-port "6697")))

  ;; Strange IANA mappings recognized.
  (should (equal 6665 (erc-normalize-port "ircu"))))

(defvar erc-tests--ipv6-examples
  '("1:2:3:4:5:6:7:8"
    "::ffff:10.0.0.1" "::ffff:1.2.3.4" "::ffff:0.0.0.0"
    "1:2:3:4:5:6:77:88" "::ffff:255.255.255.255"
    "fe08::7:8" "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
    "1:2:3:4:5:6:7:8" "1::" "1:2:3:4:5:6:7::" "1::8"
    "1:2:3:4:5:6::8" "1:2:3:4:5:6::8" "1::7:8" "1:2:3:4:5::7:8"
    "1:2:3:4:5::8" "1::6:7:8" "1:2:3:4::6:7:8" "1:2:3:4::8"
    "1::5:6:7:8" "1:2:3::5:6:7:8" "1:2:3::8" "1::4:5:6:7:8"
    "1:2::4:5:6:7:8" "1:2::8" "1::3:4:5:6:7:8" "1::3:4:5:6:7:8"
    "1::8" "::2:3:4:5:6:7:8" "::2:3:4:5:6:7:8" "::8"
    "::" "fe08::7:8%eth0" "fe08::7:8%1" "::255.255.255.255"
    "::ffff:255.255.255.255" "::ffff:0:255.255.255.255"
    "2001:db8:3:4::192.0.2.33" "64:ff9b::192.0.2.33"))

(ert-deftest erc--server-connect-dumb-ipv6-regexp ()
  (dolist (a erc-tests--ipv6-examples)
    (should-not (string-match erc--server-connect-dumb-ipv6-regexp a))
    (should (string-match erc--server-connect-dumb-ipv6-regexp
                          (concat "[" a "]")))))

(ert-deftest erc--with-entrypoint-environment ()
  (let ((env '((erc-join-buffer . foo)
               (erc-server-connect-function . bar))))
    (erc--with-entrypoint-environment env
      (should (eq erc-join-buffer 'foo))
      (should (eq erc-server-connect-function 'bar)))))

(ert-deftest erc-select-read-args ()

  (ert-info ("Prompts for switch to TLS by default")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "\r\r\r\ry\r"
                       (erc-select-read-args)))
                   (list :server "irc.libera.chat"
                         :port 6697
                         :nick (user-login-name)
                         '--interactive-env--
                         '((erc-server-connect-function . erc-open-tls-stream)
                           (erc-join-buffer . window))))))

  (ert-info ("Switches to TLS when port matches default TLS port")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "irc.gnu.org\r6697\r\r\r"
                       (erc-select-read-args)))
                   (list :server "irc.gnu.org"
                         :port 6697
                         :nick (user-login-name)
                         '--interactive-env--
                         '((erc-server-connect-function . erc-open-tls-stream)
                           (erc-join-buffer . window))))))

  (ert-info ("Switches to TLS when URL is ircs://")
    (let ((erc--display-context '((erc-interactive-display . erc))))
      (should (equal (let ((inhibit-message noninteractive))
                       (ert-simulate-keys "ircs://irc.gnu.org\r\r\r\r"
                         (erc-select-read-args)))
                     (list :server "irc.gnu.org"
                           :port 6697
                           :nick (user-login-name)
                           '--interactive-env--
                           '((erc-server-connect-function
                              . erc-open-tls-stream)
                             (erc--display-context
                              . ((erc-interactive-display . erc)))
                             (erc-join-buffer . window)))))))

  (setq-local erc-interactive-display nil) ; cheat to save space

  (ert-info ("Opt out of non-TLS warning manually")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "\r\r\r\rn\r"
                       (erc-select-read-args)))
                   (list :server "irc.libera.chat"
                         :port 6667
                         :nick (user-login-name)))))

  (ert-info ("Override default TLS")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "irc://irc.libera.chat\r\r\r\r"
                       (erc-select-read-args)))
                   (list :server "irc.libera.chat"
                         :port 6667
                         :nick (user-login-name)))))

  (ert-info ("Address includes port")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "localhost:6667\rnick\r\r"
                       (erc-select-read-args)))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"))))

  (ert-info ("Address includes nick, password skipped via option")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "nick@localhost:6667\r"
                       (let (erc-prompt-for-password)
                         (erc-select-read-args))))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"))))

  (ert-info ("Address includes nick and password")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "nick:sesame@localhost:6667\r\r"
                       (erc-select-read-args)))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"
                         :password "sesame"))))

  (ert-info ("IPv6 address plain")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "::1\r\r\r\r"
                       (erc-select-read-args)))
                   (list :server "[::1]"
                         :port 6667
                         :nick (user-login-name)))))

  (ert-info ("IPv6 address with port")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "[::1]:6667\r\r\r"
                       (erc-select-read-args)))
                   (list :server "[::1]"
                         :port 6667
                         :nick (user-login-name)))))

  (ert-info ("IPv6 address includes nick")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "nick@[::1]:6667\r\r"
                       (erc-select-read-args)))
                   (list :server "[::1]"
                         :port 6667
                         :nick "nick"))))

  (ert-info ("Extra args use URL nick by default")
    (should (equal (let ((inhibit-message noninteractive))
                     (ert-simulate-keys "nick:sesame@localhost:6667\r\r\r\r"
                       (let ((current-prefix-arg '(4)))
                         (erc-select-read-args))))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"
                         :user "nick"
                         :password "sesame"
                         :full-name "nick")))))

(ert-deftest erc-tls ()
  (let (calls env)
    (cl-letf ((inhibit-message noninteractive)
              ((symbol-function 'user-login-name)
               (lambda (&optional _) "tester"))
              ((symbol-function 'erc-open)
               (lambda (&rest r)
                 (push `((erc-join-buffer ,erc-join-buffer)
                         (erc--display-context ,@erc--display-context)
                         (erc-server-connect-function
                          ,erc-server-connect-function))
                       env)
                 (push r calls))))

      (ert-info ("Defaults")
        (erc-tls)
        (should (equal (pop calls)
                       '("irc.libera.chat" 6697 "tester" "unknown" t
                         nil nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer bury)
                         (erc--display-context (erc-buffer-display . erc-tls))
                         (erc-server-connect-function erc-open-tls-stream)))))

      (ert-info ("Full")
        (erc-tls :server "irc.gnu.org"
                 :port 7000
                 :user "bobo"
                 :nick "bob"
                 :full-name "Bob's Name"
                 :password "bob:changeme"
                 :client-certificate t
                 :id 'GNU.org)
        (should (equal (pop calls)
                       '("irc.gnu.org" 7000 "bob" "Bob's Name" t
                         "bob:changeme" nil nil nil t "bobo" GNU.org)))
        (should (equal (pop env)
                       '((erc-join-buffer bury)
                         (erc--display-context (erc-buffer-display . erc-tls))
                         (erc-server-connect-function erc-open-tls-stream)))))

      ;; Values are often nil when called by lisp code, which leads to
      ;; null params.  This is why `erc-open' recomputes almost
      ;; everything.
      (ert-info ("Fallback")
        (let ((erc-nick "bob")
              (erc-server "irc.gnu.org")
              (erc-email-userid "bobo")
              (erc-user-full-name "Bob's Name"))
          (erc-tls :server nil
                   :port 7000
                   :nick nil
                   :password "bob:changeme"))
        (should (equal (pop calls)
                       '(nil 7000 nil "Bob's Name" t
                             "bob:changeme" nil nil nil nil "bobo" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer bury)
                         (erc--display-context (erc-buffer-display . erc-tls))
                         (erc-server-connect-function erc-open-tls-stream)))))

      (ert-info ("Interactive")
        (ert-simulate-keys "nick:sesame@localhost:6667\r\r"
          (call-interactively #'erc-tls))
        (should (equal (pop calls)
                       '("localhost" 6667 "nick" "unknown" t "sesame"
                         nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer window)
                         (erc--display-context
                          (erc-interactive-display . erc-tls))
                         (erc-server-connect-function erc-open-tls-stream)))))

      (ert-info ("Custom connect function")
        (let ((erc-server-connect-function 'my-connect-func))
          (erc-tls)
          (should (equal (pop calls)
                         '("irc.libera.chat" 6697 "tester" "unknown" t
                           nil nil nil nil nil "user" nil)))
          (should (equal (pop env)
                         '((erc-join-buffer bury)
                           (erc--display-context
                            (erc-buffer-display . erc-tls))
                           (erc-server-connect-function my-connect-func))))))

      (ert-info ("Advised default function overlooked") ; intentional
        (advice-add 'erc-server-connect-function :around #'ignore
                    '((name . erc-tests--erc-tls)))
        (erc-tls)
        (should (equal (pop calls)
                       '("irc.libera.chat" 6697 "tester" "unknown" t
                         nil nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer bury)
                         (erc--display-context (erc-buffer-display . erc-tls))
                         (erc-server-connect-function erc-open-tls-stream))))
        (advice-remove 'erc-server-connect-function 'erc-tests--erc-tls))

      (ert-info ("Advised non-default function honored")
        (let ((f (lambda (&rest r) (ignore r))))
          (cl-letf (((symbol-value 'erc-server-connect-function) f))
            (advice-add 'erc-server-connect-function :around #'ignore
                        '((name . erc-tests--erc-tls)))
            (erc-tls)
            (should (equal (pop calls)
                           '("irc.libera.chat" 6697 "tester" "unknown" t
                             nil nil nil nil nil "user" nil)))
            (should (equal (pop env) `((erc-join-buffer bury)
                                       (erc--display-context
                                        (erc-buffer-display . erc-tls))
                                       (erc-server-connect-function ,f))))
            (advice-remove 'erc-server-connect-function
                           'erc-tests--erc-tls)))))))

;; See `erc-select-read-args' above for argument parsing.
;; This only tests the "hidden" arguments.

(ert-deftest erc--interactive ()
  (let (calls env)
    (cl-letf ((inhibit-message noninteractive)
              ((symbol-function 'user-login-name)
               (lambda (&optional _) "tester"))
              ((symbol-function 'erc-open)
               (lambda (&rest r)
                 (push `((erc-join-buffer ,erc-join-buffer)
                         (erc--display-context ,@erc--display-context)
                         (erc-server-connect-function
                          ,erc-server-connect-function))
                       env)
                 (push r calls))))

      (ert-info ("Default click-through accept TLS upgrade")
        (ert-simulate-keys "\r\r\r\ry\r"
          (call-interactively #'erc))
        (should (equal (pop calls)
                       '("irc.libera.chat" 6697 "tester" "unknown" t nil
                         nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer window)
                         (erc--display-context (erc-interactive-display . erc))
                         (erc-server-connect-function erc-open-tls-stream)))))

      (ert-info ("Nick supplied, decline TLS upgrade")
        (ert-simulate-keys "\r\rdummy\r\rn\r"
          (call-interactively #'erc))
        (should (equal (pop calls)
                       '("irc.libera.chat" 6667 "dummy" "unknown" t nil
                         nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer window)
                         (erc--display-context (erc-interactive-display . erc))
                         (erc-server-connect-function
                          erc-open-network-stream))))))))

(ert-deftest erc-server-select ()
  (let (calls env)
    (cl-letf ((inhibit-message noninteractive)
              ((symbol-function 'user-login-name)
               (lambda (&optional _) "tester"))
              ((symbol-function 'erc-open)
               (lambda (&rest r)
                 (push `((erc-join-buffer ,erc-join-buffer)
                         (erc--display-context ,@erc--display-context)
                         (erc-server-connect-function
                          ,erc-server-connect-function))
                       env)
                 (push r calls))))

      (ert-info ("Selects Libera.Chat Europe, automatic TSL")
        (ert-simulate-keys "Libera.Chat\rirc.eu.\t\r\r\r"
          (with-suppressed-warnings ((obsolete erc-server-select))
            (call-interactively #'erc-server-select)))
        (should (equal (pop calls)
                       '("irc.eu.libera.chat" 6697 "tester" "unknown" t nil
                         nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer window)
                         (erc--display-context (erc-interactive-display . erc))
                         (erc-server-connect-function erc-open-tls-stream)))))

      (ert-info ("Selects entry that doesn't support TLS")
        (ert-simulate-keys "IRCnet\rirc.fr.\t\rdummy\r\r"
          (with-suppressed-warnings ((obsolete erc-server-select))
            (call-interactively #'erc-server-select)))
        (should (equal (pop calls)
                       '("irc.fr.ircnet.net" 6667 "dummy" "unknown" t nil
                         nil nil nil nil "user" nil)))
        (should (equal (pop env)
                       '((erc-join-buffer window)
                         (erc--display-context (erc-interactive-display . erc))
                         (erc-server-connect-function
                          erc-open-network-stream))))))))

(ert-deftest erc-handle-irc-url ()
  (let* (calls
         rvbuf
         erc-networks-alist
         erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook
         (erc-url-connect-function
          (lambda (&rest r)
            (push r calls)
            (if (functionp rvbuf) (funcall rvbuf) rvbuf))))

    (cl-letf (((symbol-function 'erc-cmd-JOIN)
               (lambda (&rest r) (push r calls))))

      (with-current-buffer (erc-tests-common-make-server-buf "foonet")
        (setq rvbuf (current-buffer)))
      (erc-tests-common-make-server-buf "barnet")
      (erc-tests-common-make-server-buf "baznet")

      (ert-info ("Unknown network")
        (erc-handle-irc-url "irc.foonet.org" 6667 "#chan" nil nil "irc")
        (should (equal '("#chan" nil) (pop calls)))
        (should-not calls))

      (ert-info ("Unknown network, no port")
        (erc-handle-irc-url "irc.foonet.org" nil "#chan" nil nil "irc")
        (should (equal '("#chan" nil) (pop calls)))
        (should-not calls))

      (ert-info ("Known network, no port")
        (setq erc-networks-alist '((foonet "irc.foonet.org")))
        (erc-handle-irc-url "irc.foonet.org" nil "#chan" nil nil "irc")
        (should (equal '("#chan" nil) (pop calls)))
        (should-not calls))

      (ert-info ("Known network, different port")
        (erc-handle-irc-url "irc.foonet.org" 6697 "#chan" nil nil "irc")
        (should (equal '("#chan" nil) (pop calls)))
        (should-not calls))

      (ert-info ("Known network, existing chan with key")
        (save-excursion
          (with-current-buffer "foonet" (erc--open-target "#chan")))
        (erc-handle-irc-url "irc.foonet.org" nil "#chan?sec" nil nil "irc")
        (should (equal '("#chan" "sec") (pop calls)))
        (should-not calls))

      (ert-info ("Unknown network, connect, no chan")
        (erc-handle-irc-url "irc.gnu.org" nil nil nil nil "irc")
        (should (equal '("irc" :server "irc.gnu.org") (pop calls)))
        (should-not calls))

      (ert-info ("Unknown network, connect, chan")
        (with-current-buffer "foonet"
          (should-not (local-variable-p 'erc-after-connect)))
        (setq rvbuf (lambda () (erc-tests-common-make-server-buf "gnu")))
        (erc-handle-irc-url "irc.gnu.org" nil "#spam" nil nil "irc")
        (should (equal '("irc" :server "irc.gnu.org") (pop calls)))
        (should-not calls)
        (with-current-buffer "gnu"
          (should (local-variable-p 'erc-after-connect))
          (funcall (car erc-after-connect))
          (should (equal '("#spam" nil) (pop calls)))
          (should-not (local-variable-p 'erc-after-connect)))
        (should-not calls))))

  (when noninteractive
    (erc-tests-common-kill-buffers)))

;; For legacy accessors, like `erc-channel-user-halfop', this test only
;; demonstrates compat-oriented behavior in a non-ERC buffer.  See
;; `erc-tests--with-channel-user-status-accessors' based tests for
;; behavior in ERC buffers, both fallback and ISUPPORT-defined.
(ert-deftest erc-channel-user ()
  (should (= 0 (erc-channel-user-status (erc-channel-user--make))))
  (should-not (erc-channel-user-last-message-time (erc-channel-user--make)))

  (should (= 42 (erc-channel-user-last-message-time
                 (make-erc-channel-user :last-message-time 42))))

  (should (zerop (erc-channel-user-status (make-erc-channel-user))))

  (let ((u (make-erc-channel-user)))

    (ert-info ("Add voice status to user")
      (should (= 0 (erc-channel-user-status u)))
      (should-not (erc-channel-user-voice u))
      (should (eq t (setf (erc-channel-user-voice u) t)))
      (should (eq t (erc-channel-user-voice u))))

    (ert-info ("Add op status to user")
      (should (= 1 (erc-channel-user-status u)))
      (should-not (erc-channel-user-op u))
      (should (eq t (setf (erc-channel-user-op u) t)))
      (should (eq t (erc-channel-user-op u))))

    (ert-info ("Add owner status to user")
      (should (= 5 (erc-channel-user-status u)))
      (should-not (erc-channel-user-owner u))
      (should (eq t (setf (erc-channel-user-owner u) t)))
      (should (eq t (erc-channel-user-owner u))))

    (ert-info ("Remove owner status from user")
      (should (= 21 (erc-channel-user-status u)))
      (should-not (setf (erc-channel-user-owner u) nil))
      (should-not (erc-channel-user-owner u)))

    (ert-info ("Remove op status from user")
      (should (= 5 (erc-channel-user-status u)))
      (should-not (setf (erc-channel-user-op u) nil))
      (should-not (erc-channel-user-op u)))

    (ert-info ("Remove voice status from user")
      (should (= 1 (erc-channel-user-status u)))
      (should-not (setf (erc-channel-user-voice u) nil))
      (should-not (erc-channel-user-voice u)))

    (ert-info ("Remove voice status from zeroed user")
      (should (= 0 (erc-channel-user-status u)))
      (should-not (setf (erc-channel-user-voice u) nil))
      (should-not (erc-channel-user-voice u))
      (should (= 0 (erc-channel-user-status u))))))

(defconst erc-tests--modules
  '( autoaway autojoin bufbar button capab-identify
     command-indicator completion dcc fill identd
     imenu irccontrols keep-place list log match menu move-to-prompt netsplit
     networks nickbar nicks noncommands notifications notify page readonly
     replace ring sasl scrolltobottom services smiley sound
     spelling stamp track truncate unmorse xdcc))

;; Ensure that `:initialize' doesn't change the ordering of the
;; members because otherwise the widget's state is "edited".

(ert-deftest erc-modules--initialize ()
  ;; This is `custom--standard-value' from Emacs 28.
  (should (equal (eval (car (get 'erc-modules 'standard-value)) t)
                 erc-modules)))

;; Ensure the `:initialize' function for `erc-modules' successfully
;; tags all built-in modules with the internal property `erc--module'.

(ert-deftest erc-modules--internal-property ()
  (let (ours)
    (mapatoms (lambda (s)
                (when-let* ((v (get s 'erc--module))
                            ((eq v s)))
                  (push s ours))))
    (should (equal (sort ours #'string-lessp) erc-tests--modules))))

(ert-deftest erc--normalize-module-symbol ()
  (dolist (mod erc-tests--modules)
    (should (eq (erc--normalize-module-symbol mod) mod)))
  (should (eq (erc--normalize-module-symbol 'pcomplete) 'completion))
  (should (eq (erc--normalize-module-symbol 'Completion) 'completion))
  (should (eq (erc--normalize-module-symbol 'ctcp-page) 'page))
  (should (eq (erc--normalize-module-symbol 'ctcp-sound) 'sound))
  (should (eq (erc--normalize-module-symbol 'timestamp) 'stamp))
  (should (eq (erc--normalize-module-symbol 'nickserv) 'services)))

(defun erc-tests--assert-printed-in-subprocess (code expected)
  "Assert result emitted to standard output from CODE matches EXPECTED.
Expect CODE to print result using `prin1' as a list beginning with the
keyword :result."
  (with-current-buffer
      (get-buffer-create
       (concat "*" (symbol-name (ert-test-name (ert-running-test))) "*"))
    (unwind-protect
        (let ((proc (erc-tests-common-create-subprocess code '("-batch") nil)))
          (while (accept-process-output proc 10))
          (goto-char (point-min))
          (search-forward "(:result " nil t)
          (unless (equal (ignore-errors (read (current-buffer))) expected)
            (ert-fail (list "Mismatch"
                            :expected expected
                            :buffer-string (buffer-string)))))
      (when noninteractive
        (kill-buffer)))))

;; Worrying about which library a module comes from is mostly not
;; worth the hassle so long as ERC can find its minor mode.  However,
;; bugs involving multiple modules living in the same library may slip
;; by because a module's loading problems may remain hidden on account
;; of its place in the default ordering.

(ert-deftest erc--find-mode ()
  (erc-tests--assert-printed-in-subprocess
   '(let ((mods (mapcar #'cadddr (cdddr (get 'erc-modules 'custom-type))))
          moded)
      (setq mods (sort mods (lambda (a b) (if (zerop (random 2)) a b))))
      (dolist (mod mods)
        (unless (keywordp mod)
          (push (if-let* ((mode (erc--find-mode mod))) mod (list :missing mod))
                moded)))
      (prin1 (list :result
                   (sort moded (lambda (a b)
                                 (string< (symbol-name a) (symbol-name b)))))))
   erc-tests--modules))

(ert-deftest erc--essential-hook-ordering ()
  (erc-tests--assert-printed-in-subprocess
   '(progn
      (erc-update-modules)
      (prin1 (list :result
                   (list :erc-insert-modify-hook erc-insert-modify-hook
                         :erc-send-modify-hook erc-send-modify-hook))))

   '( :erc-insert-modify-hook (erc-controls-highlight ; 0
                               erc-button-add-buttons ; 30
                               erc-match-message ; 50
                               erc-fill ; 60
                               erc-add-timestamp) ; 70

      :erc-send-modify-hook ( erc-controls-highlight ; 0
                              erc-button-add-buttons ; 30
                              erc-fill ; 40
                              erc-add-timestamp)))) ; 70

(ert-deftest erc-migrate-modules ()
  (should (equal (erc-migrate-modules '(autojoin timestamp button))
                 '(autojoin stamp button)))
  ;; Default unchanged
  (should (equal (erc-migrate-modules erc-modules) erc-modules)))

(ert-deftest erc--find-group ()
  ;; These two are loaded by default
  (should (eq (erc--find-group 'keep-place nil) 'erc))
  (should (eq (erc--find-group 'networks nil) 'erc-networks))
  ;; These are fake
  (cl-letf (((get 'erc-bar 'group-documentation) "")
            ((get 'baz 'erc-group) 'erc-foo))
    (should (eq (erc--find-group 'foo 'bar) 'erc-bar))
    (should (eq (erc--find-group 'bar 'foo) 'erc-bar))
    (should (eq (erc--find-group 'bar nil) 'erc-bar))
    (should (eq (erc--find-group 'foo nil) 'erc))
    (should (eq (erc--find-group 'fake 'baz) 'erc-foo))))

(ert-deftest erc--find-group--real ()
  :tags '(:unstable)
  (require 'erc-services)
  (require 'erc-stamp)
  (require 'erc-sound)
  (require 'erc-page)
  (require 'erc-join)
  (require 'erc-capab)
  (require 'erc-pcomplete)
  (should (eq (erc--find-group 'services 'nickserv) 'erc-services))
  (should (eq (erc--find-group 'stamp 'timestamp) 'erc-stamp))
  (should (eq (erc--find-group 'sound 'ctcp-sound) 'erc-sound))
  (should (eq (erc--find-group 'page 'ctcp-page) 'erc-page))
  (should (eq (erc--find-group 'autojoin) 'erc-autojoin))
  (should (eq (erc--find-group 'pcomplete 'Completion) 'erc-pcomplete))
  (should (eq (erc--find-group 'capab-identify) 'erc-capab))
  (should (eq (erc--find-group 'completion) 'erc-pcomplete))
  ;; No group specified.
  (should (eq (erc--find-group 'smiley nil) 'erc))
  (should (eq (erc--find-group 'unmorse nil) 'erc)))

(ert-deftest erc--sort-modules ()
  (should (equal (erc--sort-modules '(networks foo fill bar fill stamp bar))
                 ;; Third-party mods appear in original order.
                 '(fill networks stamp foo bar))))

(defun erc-tests--update-modules (fn)
  (let* ((calls nil)
         (custom-modes nil)
         (on-load nil)
         (text-quoting-style 'grave)

         (get-calls (lambda () (prog1 (nreverse calls) (setq calls nil))))

         (add-onload (lambda (m k v)
                       (put (intern m) 'erc--feature k)
                       (push (cons k (lambda () (funcall v m))) on-load)))

         (mk-cmd (lambda (module)
                   (let ((mode (intern (format "erc-%s-mode" module))))
                     (fset mode (lambda (n) (push (cons mode n) calls))))))

         (mk-builtin (lambda (module-string)
                       (let ((s (intern module-string)))
                         (put s 'erc--module s))))

         (mk-global (lambda (module)
                      (push (intern (format "erc-%s-mode" module))
                            custom-modes))))

    (cl-letf (((symbol-function 'require)
               (lambda (s &rest _)
                 ;; Simulate library being loaded, things defined.
                 (when-let* ((h (alist-get s on-load))) (funcall h))
                 (push (cons 'req s) calls)))

              ;; Spoof global module detection.
              ((symbol-function 'custom-variable-p)
               (lambda (v) (memq v custom-modes))))

      (funcall fn get-calls add-onload mk-cmd mk-builtin mk-global))
    (should-not erc--aberrant-modules)))

(ert-deftest erc--update-modules/unknown ()
  (erc-tests--update-modules

   (lambda (get-calls _ mk-cmd _ mk-global)

     (ert-info ("Baseline")
       (let* ((erc-modules '(foo))
              (obarray (obarray-make))
              (err (should-error (erc--update-modules erc-modules))))
         (should (equal (cadr err) "`foo' is not a known ERC module"))
         (should (equal (mapcar #'prin1-to-string (funcall get-calls))
                        '("(req . erc-foo)")))))

     ;; Module's mode command exists but lacks an associated file.
     (ert-info ("Bad autoload flagged as suspect")
       (should-not erc--aberrant-modules)
       (let* ((erc--aberrant-modules nil)
              (obarray (obarray-make))
              (erc-modules (list (intern "foo"))))

         ;; Create a mode-activation command and make mode-var global.
         (funcall mk-cmd "foo")
         (funcall mk-global "foo")

         ;; No local modules to return.
         (should-not (erc--update-modules erc-modules))
         (should (equal (mapcar #'prin1-to-string erc--aberrant-modules)
                        '("foo")))
         ;; ERC requires the library via prefixed module name.
         (should (equal (mapcar #'prin1-to-string (funcall get-calls))
                        '("(req . erc-foo)" "(erc-foo-mode . 1)"))))))))

;; A local module (here, `lo2') lacks a mode toggle, so ERC tries to
;; load its defining library, first via the symbol property
;; `erc--feature', and then via an "erc-" prefixed symbol.
(ert-deftest erc--update-modules/local ()
  (erc-tests--update-modules

   (lambda (get-calls add-onload mk-cmd mk-builtin mk-global)

     (let* ((obarray (obarray-make 20))
            (erc-modules (mapcar #'intern '("glo" "lo1" "lo2"))))

       ;; Create a global and a local module.
       (mapc mk-cmd '("glo" "lo1"))
       (mapc mk-builtin '("glo" "lo1"))
       (funcall mk-global "glo")
       (funcall add-onload "lo2" 'explicit-feature-lib mk-cmd)

       ;; Returns local modules.
       (should (equal (mapcar #'symbol-name (erc--update-modules erc-modules))
                      '("erc-lo2-mode" "erc-lo1-mode")))

       ;; Requiring `erc-lo2' defines `erc-lo2-mode'.
       (should (equal (mapcar #'prin1-to-string (funcall get-calls))
                      `("(erc-glo-mode . 1)"
                        "(req . explicit-feature-lib)")))))))

(ert-deftest erc--update-modules/realistic ()
  (let ((calls nil)
        ;; Module `pcomplete' "resolves" to `completion'.
        (erc-modules '(pcomplete autojoin networks)))
    (cl-letf (((symbol-function 'require)
               (lambda (s &rest _) (push (cons 'req s) calls)))

              ;; Spoof global module detection.
              ((symbol-function 'custom-variable-p)
               (lambda (v)
                 (memq v '(erc-autojoin-mode erc-networks-mode
                                             erc-completion-mode))))
              ;; Mock and spy real builtins.
              ((symbol-function 'erc-autojoin-mode)
               (lambda (n) (push (cons 'autojoin n) calls)))
              ((symbol-function 'erc-networks-mode)
               (lambda (n) (push (cons 'networks n) calls)))
              ((symbol-function 'erc-completion-mode)
               (lambda (n) (push (cons 'completion n) calls))))

      (should-not (erc--update-modules erc-modules)) ; no locals
      (should (equal (nreverse calls)
                     '((completion . 1) (autojoin . 1) (networks . 1)))))))

(ert-deftest erc--merge-local-modes ()
  (cl-letf (((get 'erc-b-mode 'erc-module) 'b)
            ((get 'erc-c-mode 'erc-module) 'c)
            ((get 'erc-d-mode 'erc-module) 'd)
            ((get 'erc-e-mode 'erc-module) 'e))

    (ert-info ("No existing modes")
      (let ((old '((a) (b . t)))
            (new '(erc-c-mode erc-d-mode)))
        (should (equal (erc--merge-local-modes new old)
                       '((erc-c-mode erc-d-mode))))))

    (ert-info ("Active existing added, inactive existing removed, deduped")
      (let ((old '((a) (erc-b-mode) (c . t) (erc-d-mode . t) (erc-e-mode . t)))
            (new '(erc-b-mode erc-d-mode)))
        (should (equal (erc--merge-local-modes new old)
                       '((erc-d-mode erc-e-mode) . (erc-b-mode))))))

    (ert-info ("Non-module erc-prefixed mode ignored")
      (let ((old '((erc-b-mode) (erc-f-mode . t) (erc-d-mode . t)))
            (new '(erc-b-mode)))
        (should (equal (erc--merge-local-modes new old)
                       '((erc-d-mode) . (erc-b-mode))))))))

(ert-deftest define-erc-module--global ()
  (let ((global-module '(define-erc-module mname malias
                          "Some docstring."
                          ((ignore a) (ignore b))
                          ((ignore c) (ignore d)))))

    (should (equal (cl-letf (((symbol-function
                               'erc--prepare-custom-module-type)
                              #'symbol-name))
                     (macroexpand global-module))
                   `(progn

                      (define-minor-mode erc-mname-mode
                        "Toggle ERC mname mode.
If called interactively, enable `erc-mname-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Some docstring."
                        :global t
                        :group (erc--find-group 'mname 'malias)
                        :require 'nil
                        :type "mname"
                        (let ((erc--module-toggle-prefix-arg arg))
                          (if erc-mname-mode
                              (erc-mname-enable)
                            (erc-mname-disable))))

                      (defun erc-mname-enable ()
                        "Enable ERC mname mode."
                        (interactive)
                        (unless (or erc--inside-mode-toggle-p
                                    (memq 'mname erc-modules))
                          (let ((erc--inside-mode-toggle-p t))
                            (erc--favor-changed-reverted-modules-state
                             'mname #'cons)))
                        (setq erc-mname-mode t)
                        (ignore a) (ignore b))

                      (defun erc-mname-disable ()
                        "Disable ERC mname mode."
                        (interactive)
                        (unless (or erc--inside-mode-toggle-p
                                    (not (memq 'mname erc-modules)))
                          (let ((erc--inside-mode-toggle-p t))
                            (erc--favor-changed-reverted-modules-state
                             'mname #'delq)))
                        (setq erc-mname-mode nil)
                        (ignore c) (ignore d))

                      (defalias 'erc-malias-mode #'erc-mname-mode)
                      (put 'erc-malias-mode 'erc-module 'mname)

                      (put 'erc-mname-mode 'erc-module 'mname)
                      (put 'erc-mname-mode 'definition-name 'mname)
                      (put 'erc-mname-enable 'definition-name 'mname)
                      (put 'erc-mname-disable 'definition-name 'mname))))))

(ert-deftest define-erc-module--local ()
  (let* ((global-module '(define-erc-module mname nil ; no alias
                           "Some docstring."
                           ((ignore a) (ignore b))
                           ((ignore c) (ignore d))
                           'local))
         (got (macroexpand global-module))
         (arg-en (cadr (nth 2 (nth 2 got))))
         (arg-dis (cadr (nth 2 (nth 3 got)))))

    (should (equal got
                   `(progn
                      (define-minor-mode erc-mname-mode
                        "Toggle ERC mname mode locally.
If called interactively, enable `erc-mname-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Some docstring."
                        :global nil
                        :group (erc--find-group 'mname nil)
                        (let ((erc--module-toggle-prefix-arg arg))
                          (if erc-mname-mode
                              (erc-mname-enable)
                            (erc-mname-disable))))

                      (defun erc-mname-enable (&optional ,arg-en)
                        "Enable ERC mname mode locally.
When called interactively, do so in all buffers for the current
connection."
                        (interactive "p")
                        (when (derived-mode-p 'erc-mode)
                          (if ,arg-en
                              (erc-with-all-buffers-of-server
                                  erc-server-process nil
                                (erc-mname-enable))
                            (setq erc-mname-mode t)
                            (ignore a) (ignore b))))

                      (defun erc-mname-disable (&optional ,arg-dis)
                        "Disable ERC mname mode locally.
When called interactively, do so in all buffers for the current
connection."
                        (interactive "p")
                        (when (derived-mode-p 'erc-mode)
                          (if ,arg-dis
                              (erc-with-all-buffers-of-server
                                  erc-server-process nil
                                (erc-mname-disable))
                            (setq erc-mname-mode nil)
                            (ignore c) (ignore d))))

                      (put 'erc-mname-mode 'erc-module 'mname)
                      (put 'erc-mname-mode 'definition-name 'mname)
                      (put 'erc-mname-enable 'definition-name 'mname)
                      (put 'erc-mname-disable 'definition-name 'mname))))))

(ert-deftest erc-tests-common-string-to-propertized-parts ()
  :tags '(:unstable) ; only run this locally
  (unless (>= emacs-major-version 28) (ert-skip "Missing `object-intervals'"))

  (should (equal (erc-tests-common-string-to-propertized-parts
                  #("abc"
                    0 1 (face default foo 1)
                    1 3 (face (default italic) bar "2")))
                 '(concat (propertize "a" 'foo 1 'face 'default)
                          (propertize "bc" 'bar "2" 'face '(default italic)))))
  (should (equal #("abc"
                   0 1 (face default foo 1)
                   1 3 (face (default italic) bar "2"))
                 (concat (propertize "a" 'foo 1 'face 'default)
                         (propertize "bc" 'bar "2" 'face '(default italic))))))

(ert-deftest erc--make-message-variable-name ()
  (should (erc--make-message-variable-name 'english 'QUIT 'softp))
  (should (erc--make-message-variable-name 'english 'QUIT nil))

  (let ((obarray (obarray-make)))
    (should-not (erc--make-message-variable-name 'testcat 'testkey 'softp))
    (should (erc--make-message-variable-name 'testcat 'testkey nil))
    (should (intern-soft "erc-message-testcat-testkey" obarray))
    (should-not (erc--make-message-variable-name 'testcat 'testkey 'softp))
    (set (intern "erc-message-testcat-testkey" obarray) "hello world")
    (should (equal (symbol-value
                    (erc--make-message-variable-name 'testcat 'testkey nil))
                   "hello world")))

  ;; Hyphenated (internal catalog).
  (let ((obarray (obarray-make)))
    (should-not (erc--make-message-variable-name '-testcat 'testkey 'softp))
    (should (erc--make-message-variable-name '-testcat 'testkey nil))
    (should (intern-soft "erc--message-testcat-testkey" obarray))
    (should-not (erc--make-message-variable-name '-testcat 'testkey 'softp))
    (set (intern "erc--message-testcat-testkey" obarray) "hello world")
    (should (equal (symbol-value
                    (erc--make-message-variable-name '-testcat 'testkey nil))
                   "hello world"))))

(ert-deftest erc-retrieve-catalog-entry ()
  (should (eq 'english erc-current-message-catalog))
  (should (equal (erc-retrieve-catalog-entry 's221) "User modes for %n: %m"))

  ;; Local binding.
  (with-temp-buffer
    (should (equal (erc-retrieve-catalog-entry 's221) "User modes for %n: %m"))
    (setq erc-current-message-catalog 'test)
    ;; No catalog named `test'.
    (should (equal (erc-retrieve-catalog-entry 's221) "User modes for %n: %m"))

    (let ((obarray (obarray-make)))
      (set (intern "erc-message-test-s221") "test 221 val")
      (should (equal (erc-retrieve-catalog-entry 's221) "test 221 val"))
      (set (intern "erc-message-english-s221") "eng 221 val")

      (let ((erc-current-message-catalog 'english))
        (should (equal (erc-retrieve-catalog-entry 's221) "eng 221 val")))

      (with-temp-buffer
        (should (equal (erc-retrieve-catalog-entry 's221) "eng 221 val"))
        (let ((erc-current-message-catalog 'test))
          (should (equal (erc-retrieve-catalog-entry 's221) "test 221 val"))))

      (should (equal (erc-retrieve-catalog-entry 's221) "test 221 val")))

    (should (equal (erc-retrieve-catalog-entry 's221) "User modes for %n: %m"))
    (should (equal erc-current-message-catalog 'test)))

  ;; Default top-level value.
  (set-default-toplevel-value 'erc-current-message-catalog 'test-top)
  (should (equal (erc-retrieve-catalog-entry 's221) "User modes for %n: %m"))
  (set (intern "erc-message-test-top-s221") "test-top 221 val")
  (should (equal (erc-retrieve-catalog-entry 's221) "test-top 221 val"))

  (setq erc-current-message-catalog 'test-local)
  (should (equal (erc-retrieve-catalog-entry 's221) "test-top 221 val"))

  (makunbound (intern "erc-message-test-top-s221"))
  (unintern "erc-message-test-top-s221" obarray)

  ;; Inheritance.
  (let ((obarray (obarray-make)))
    (set (intern "erc-message-test1-abc") "val test1 abc")
    (set (intern "erc-message-test2-abc") "val test2 abc")
    (set (intern "erc-message-test2-def") "val test2 def")
    (put (intern "test0") 'erc--base-format-catalog (intern "test1"))
    (put (intern "test1") 'erc--base-format-catalog (intern "test2"))
    (should (equal (erc-retrieve-catalog-entry 'abc (intern "test0"))
                   "val test1 abc"))
    (should (equal (erc-retrieve-catalog-entry 'def (intern "test0"))
                   "val test2 def"))
    ;; Terminates.
    (should-not (erc-retrieve-catalog-entry 'ghi (intern "test0")))))

;;; erc-tests.el ends here
