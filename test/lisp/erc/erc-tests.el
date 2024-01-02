;;; erc-tests.el --- Tests for erc.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(require 'ert-x)
(require 'erc)
(require 'erc-ring)

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
      (setq erc-default-recipients '("#foo")))

    (with-current-buffer (get-buffer-create "#spam")
      (erc-mode)
      (setq erc-server-process proc-onet)
      (setq erc-default-recipients '("#spam")))

    (with-current-buffer (get-buffer-create "#bar")
      (erc-mode)
      (setq erc-server-process proc-onet)
      (setq erc-default-recipients '("#bar")))

    (with-current-buffer (get-buffer-create "#baz")
      (erc-mode)
      (setq erc-server-process proc-exnet)
      (setq erc-default-recipients '("#baz")))

    (should (eq (get-buffer-process "ExampleNet") proc-exnet))
    (erc-with-all-buffers-of-server (get-buffer-process "ExampleNet")
      nil
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

      (erc-with-all-buffers-of-server proc-onet
        (funcall get-test)
        (kill-buffer))

      (should (= calls 1)))

    (should-not (get-buffer "OtherNet"))
    (should-not (get-buffer "#bar"))
    (should (get-buffer "#spam"))
    (kill-buffer "#spam")))

(defun erc-tests--send-prep ()
  ;; Caller should probably shadow `erc-insert-modify-hook' or
  ;; populate user tables for erc-button.
  (erc-mode)
  (insert "\n\n")
  (setq erc-input-marker (make-marker)
        erc-insert-marker (make-marker))
  (set-marker erc-insert-marker (point-max))
  (erc-display-prompt)
  (should (= (point) erc-input-marker)))

(defun erc-tests--set-fake-server-process (&rest args)
  (setq erc-server-process
        (apply #'start-process (car args) (current-buffer) args))
  (set-process-query-on-exit-flag erc-server-process nil))

(ert-deftest erc-hide-prompt ()
  (let (erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer (get-buffer-create "ServNet")
      (erc-tests--send-prep)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (erc-tests--set-fake-server-process "sleep" "1")
      (set-process-sentinel erc-server-process #'ignore)
      (setq erc-network 'ServNet)
      (set-process-query-on-exit-flag erc-server-process nil))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-tests--send-prep)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (setq erc-server-process (buffer-local-value 'erc-server-process
                                                   (get-buffer "ServNet"))
            erc--target (erc--target-from-string "#chan")))

    (with-current-buffer (get-buffer-create "bob")
      (erc-tests--send-prep)
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
        (should (string= ">" (get-text-property (point) 'display))))

      (with-current-buffer "#chan"
        (goto-char erc-insert-marker)
        (should (string= ">" (get-text-property (point) 'display)))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (goto-char erc-input-marker)
        (ert-simulate-command '(self-insert-command 1 ?/))
        (goto-char erc-insert-marker)
        (should-not (get-text-property (point) 'display))
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook)))

      (with-current-buffer "bob"
        (goto-char erc-insert-marker)
        (should (string= ">" (get-text-property (point) 'display)))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (goto-char erc-input-marker)
        (ert-simulate-command '(self-insert-command 1 ?/))
        (goto-char erc-insert-marker)
        (should-not (get-text-property (point) 'display))
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook)))

      (with-current-buffer "ServNet"
        (should (get-text-property erc-insert-marker 'display))
        (should (memq #'erc--unhide-prompt-on-self-insert pre-command-hook))
        (erc--unhide-prompt)
        (should-not (memq #'erc--unhide-prompt-on-self-insert
                          pre-command-hook))
        (should-not (get-text-property erc-insert-marker 'display))))

    (ert-info ("Value: server")
      (setq erc-hide-prompt '(server))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should (string= ">" (get-text-property erc-insert-marker 'display))))

      (with-current-buffer "#chan"
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "ServNet"
        (erc--unhide-prompt)
        (should-not (get-text-property erc-insert-marker 'display))))

    (ert-info ("Value: channel")
      (setq erc-hide-prompt '(channel))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should (string= ">" (get-text-property erc-insert-marker 'display)))
        (erc--unhide-prompt)
        (should-not (get-text-property erc-insert-marker 'display))))

    (ert-info ("Value: query")
      (setq erc-hide-prompt '(query))
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should (string= ">" (get-text-property erc-insert-marker 'display)))
        (erc--unhide-prompt)
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should-not (get-text-property erc-insert-marker 'display))))

    (ert-info ("Value: nil")
      (setq erc-hide-prompt nil)
      (with-current-buffer "ServNet"
        (erc--hide-prompt erc-server-process)
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "bob"
        (should-not (get-text-property erc-insert-marker 'display)))

      (with-current-buffer "#chan"
        (should-not (get-text-property erc-insert-marker 'display))
        (erc--unhide-prompt) ; won't blow up when prompt already showing
        (should-not (get-text-property erc-insert-marker 'display))))

    (when noninteractive
      (kill-buffer "#chan")
      (kill-buffer "bob")
      (kill-buffer "ServNet"))))

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

(ert-deftest erc-lurker-maybe-trim ()
  (let (erc-lurker-trim-nicks
        (erc-lurker-ignore-chars "_`"))

    (should (string= "nick`" (erc-lurker-maybe-trim "nick`")))

    (setq erc-lurker-trim-nicks t)
    (should (string= "nick" (erc-lurker-maybe-trim "nick`")))
    (should (string= "ni`_ck" (erc-lurker-maybe-trim "ni`_ck__``")))

    (setq erc-lurker-ignore-chars "_-`") ; set of chars, not character alts
    (should (string= "nick" (erc-lurker-maybe-trim "nick-_`")))))

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
  (should (equal (erc--parse-isupport-value "a\\x2Cb,c") '("a,b" "c"))))

(ert-deftest erc--get-isupport-entry ()
  (let ((erc--isupport-params (make-hash-table))
        (erc-server-parameters '(("FOO" . "1") ("BAR") ("BAZ" . "A,B,C")))
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
                   '((BAR . --empty--) (BAZ "A" "B" "C") (FOO "1"))))))

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
        (setq args '("tester" "BOT=B" "EXCEPTS" "PREFIX=(ov)@+" "are supp...")
              parsed (make-erc-response :command-args args :command "005"))

        (setq verify
              (lambda ()
                (should (equal erc-server-parameters
                               '(("PREFIX" . "(ov)@+") ("EXCEPTS")
                                 ("BOT" . "B"))))
                (should (zerop (hash-table-count erc--isupport-params)))
                (should (equal "(ov)@+" (erc--get-isupport-entry 'PREFIX t)))
                (should (equal '(EXCEPTS) (erc--get-isupport-entry 'EXCEPTS)))
                (should (equal "B" (erc--get-isupport-entry 'BOT t)))
                (should (string= (pop calls)
                                 "BOT=B EXCEPTS PREFIX=(ov)@+ are supp..."))
                (should (equal args (erc-response.command-args parsed)))))

        (erc-call-hooks nil parsed))

      (ert-info ("Negated, updated")
        (setq args '("tester" "-EXCEPTS" "-FAKE" "PREFIX=(ohv)@%+" "are su...")
              parsed (make-erc-response :command-args args :command "005"))

        (setq verify
              (lambda ()
                (should (equal erc-server-parameters
                               '(("PREFIX" . "(ohv)@%+") ("BOT" . "B"))))
                (should (string= (pop calls)
                                 "-EXCEPTS -FAKE PREFIX=(ohv)@%+ are su..."))
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
                 #s(erc--target-channel "#chan" \#chan)))

  (should (equal (erc--target-from-string "Bob")
                 #s(erc--target "Bob" bob)))

  (let ((erc--isupport-params (make-hash-table)))
    (puthash 'CHANTYPES  '("&#") erc--isupport-params)
    (should (equal (erc--target-from-string "&Bitlbee")
                   #s(erc--target-channel-local "&Bitlbee" &bitlbee)))))

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
    (erc-tests--send-prep)
    (setq-local erc-last-input-time 0)
    (should-not (local-variable-if-set-p 'erc-send-completed-hook))
    (set (make-local-variable 'erc-send-completed-hook) nil) ; skip t (globals)
    ;; Just in case erc-ring-mode is already on
    (setq-local erc-pre-send-functions nil)
    (add-hook 'erc-pre-send-functions #'erc-add-to-input-ring)
    ;;
    (cl-letf (((symbol-function 'erc-process-input-line)
               (lambda (&rest _)
                 (insert-before-markers
                  (erc-display-message-highlight 'notice "echo: one\n"))))
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

(ert-deftest erc--blank-in-multiline-input-p ()
  (let ((check (lambda (s)
                 (erc--blank-in-multiline-input-p
                  (split-string s erc--input-line-delim-regexp)))))

    (ert-info ("With `erc-send-whitespace-lines'")
      (let ((erc-send-whitespace-lines t))
        (should (funcall check ""))
        (should-not (funcall check "\na"))
        (should-not (funcall check "/msg a\n")) ; real /cmd
        (should-not (funcall check "a\n\nb")) ; "" allowed
        (should-not (funcall check "/msg a\n\nb")) ; non-/cmd
        (should-not (funcall check " "))
        (should-not (funcall check "\t"))
        (should-not (funcall check "a\nb"))
        (should-not (funcall check "a\n "))
        (should-not (funcall check "a\n \t"))
        (should-not (funcall check "a\n \f"))
        (should-not (funcall check "a\n \nb"))
        (should-not (funcall check "a\n \t\nb"))
        (should-not (funcall check "a\n \f\nb"))))

    (should (funcall check ""))
    (should (funcall check " "))
    (should (funcall check "\t"))
    (should (funcall check "a\n\nb"))
    (should (funcall check "a\n\nb"))
    (should (funcall check "a\n "))
    (should (funcall check "a\n \t"))
    (should (funcall check "a\n \f"))
    (should (funcall check "a\n \nb"))
    (should (funcall check "a\n \t\nb"))

    (should-not (funcall check "a\rb"))
    (should-not (funcall check "a\nb"))
    (should-not (funcall check "a\r\nb"))))

(defun erc-tests--with-process-input-spy (test)
  (with-current-buffer (get-buffer-create "FakeNet")
    (let* ((erc-pre-send-functions
            (remove #'erc-add-to-input-ring erc-pre-send-functions)) ; for now
           (inhibit-message noninteractive)
           (erc-server-current-nick "tester")
           (erc-last-input-time 0)
           erc-accidental-paste-threshold-seconds
           erc-send-modify-hook
           ;;
           calls)
      (cl-letf (((symbol-function 'erc-process-input-line)
                 (lambda (&rest r) (push r calls)))
                ((symbol-function 'erc-server-buffer)
                 (lambda () (current-buffer))))
        (erc-tests--send-prep)
        (funcall test (lambda () (pop calls)))))
    (when noninteractive (kill-buffer))))

(ert-deftest erc--check-prompt-input-functions ()
  (erc-tests--with-process-input-spy
   (lambda (next)

     (ert-info ("Errors when point not in prompt area") ; actually just dings
       (insert "/msg #chan hi")
       (forward-line -1)
       (let ((e (should-error (erc-send-current-line))))
         (should (equal "Point is not in the input area" (cadr e))))
       (goto-char (point-max))
       (ert-info ("Input remains untouched")
         (should (save-excursion (erc-bol) (looking-at "/msg #chan hi")))))

     (ert-info ("Errors when no process running")
       (let ((e (should-error (erc-send-current-line))))
         (should (equal "ERC: No process running" (cadr e))))
       (ert-info ("Input remains untouched")
         (should (save-excursion (erc-bol) (looking-at "/msg #chan hi")))))

     (ert-info ("Errors when line contains empty newline")
       (erc-bol)
       (delete-region (point) (point-max))
       (insert "one\n")
       (let ((e (should-error (erc-send-current-line))))
         (should (equal "Blank line - ignoring..." (cadr e))))
       (goto-char (point-max))
       (ert-info ("Input remains untouched")
         (should (save-excursion (goto-char erc-input-marker)
                                 (looking-at "one\n")))))

     (should (= 0 erc-last-input-time))
     (should-not (funcall next)))))

;; These also indirectly tests `erc-send-input'

(ert-deftest erc-send-current-line ()
  (erc-tests--with-process-input-spy
   (lambda (next)
     (erc-tests--set-fake-server-process "sleep" "1")
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
       ;; Commands are forced (no flood protection)
       (should (equal (funcall next) '("/msg #chan hi\n" t nil))))

     (ert-info ("Simple non-command")
       (insert "hi")
       (erc-send-current-line)
       (should (eq (point) (point-max)))
       (should (save-excursion (forward-line -1)
                               (search-forward "<tester> hi")))
       ;; Non-ommands are forced only when `erc-flood-protect' is nil
       (should (equal (funcall next) '("hi\n" nil t))))

     (should (consp erc-last-input-time)))))

(ert-deftest erc-send-whitespace-lines ()
  (erc-tests--with-process-input-spy
   (lambda (next)
     (erc-tests--set-fake-server-process "sleep" "1")
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
       (should (equal (funcall next) '("\n" nil t)))
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
       (pcase-dolist (`(,p . ,q)
                      '(("/a b\r" "/a b\n") ("/a b\n" "/a b\n")
                        ("/a b\n\n" "/a b\n") ("/a b\r\n" "/a b\n")
                        ("a b\nc\n\n" "c\n" "a b\n")
                        ("/a b\nc\n\n" "c\n" "/a b\n")
                        ("/a b\n\nc\n\n" "c\n" "\n" "/a b\n")))
         (insert p)
         (erc-send-current-line)
         (erc-bol)
         (should (eq (point) (point-max)))
         (while q
           (should (equal (funcall next) (list (pop q) nil t))))
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
      (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "")))
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "b")))))

  (ert-info ("With `erc-inhibit-multiline-input' as 3")
    (let ((erc-inhibit-multiline-input 3))
      (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b")))
      (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b" "")))
      (should (erc--check-prompt-input-for-excess-lines "" '("a" "b" "c")))))

  (ert-info ("With `erc-ask-about-multiline-input'")
    (let ((erc-inhibit-multiline-input t)
          (erc-ask-about-multiline-input t))
      (ert-simulate-keys '(?n ?\r ?y ?\r)
        (should (erc--check-prompt-input-for-excess-lines "" '("a" "b")))
        (should-not (erc--check-prompt-input-for-excess-lines "" '("a" "b")))))
    (should-not erc-ask-about-multiline-input)))

;; The point of this test is to ensure output is handled identically
;; regardless of whether a command handler is summoned.

(ert-deftest erc-process-input-line ()
  (let (erc-server-last-sent-time
        erc-server-flood-queue
        (orig-erc-cmd-MSG (symbol-function 'erc-cmd-MSG))
        (erc-default-recipients '("#chan"))
        calls)
    (with-temp-buffer
      (cl-letf (((symbol-function 'erc-cmd-MSG)
                 (lambda (line)
                   (push line calls)
                   (funcall orig-erc-cmd-MSG line)))
                ((symbol-function 'erc-server-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'erc-server-process-alive)
                 (lambda () t))
                ((symbol-function 'erc-server-send-queue)
                 #'ignore))

        (ert-info ("Dispatch to user command handler")

          (ert-info ("Baseline")
            (erc-process-input-line "/msg #chan hi\n")
            (should (equal (pop calls) " #chan hi"))
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan :hi\r\n" . utf-8))))

          (ert-info ("Quote preserves line intact")
            (erc-process-input-line "/QUOTE FAKE foo bar\n")
            (should (equal (pop erc-server-flood-queue)
                           '("FAKE foo bar\r\n" . utf-8))))

          (ert-info ("Unknown command respected")
            (erc-process-input-line "/FAKE foo bar\n")
            (should (equal (pop erc-server-flood-queue)
                           '("FAKE foo bar\r\n" . utf-8))))

          (ert-info ("Spaces preserved")
            (erc-process-input-line "/msg #chan hi you\n")
            (should (equal (pop calls) " #chan hi you"))
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan :hi you\r\n" . utf-8))))

          (ert-info ("Empty line honored")
            (erc-process-input-line "/msg #chan\n")
            (should (equal (pop calls) " #chan"))
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan :\r\n" . utf-8)))))

        (ert-info ("Implicit cmd via `erc-send-input-line-function'")

          (ert-info ("Baseline")
            (erc-process-input-line "hi\n")
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan :hi\r\n" . utf-8))))

          (ert-info ("Spaces preserved")
            (erc-process-input-line "hi you\n")
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan :hi you\r\n" . utf-8))))

          (ert-info ("Empty line transmitted with injected-space kludge")
            (erc-process-input-line "\n")
            (should (equal (pop erc-server-flood-queue)
                           '("PRIVMSG #chan : \r\n" . utf-8))))

          (should-not calls))))))

;; Note: if adding an erc-backend-tests.el, please relocate this there.

(ert-deftest erc-message ()
  (should-not erc-server-last-peers)
  (let (server-proc
        calls
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (cl-letf (((symbol-function 'erc-display-message)
               (lambda (_ _ _ line) (push line calls)))
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
          (should (eq 'no-target (pop calls)))
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

(ert-deftest erc-select-read-args ()

  (ert-info ("Does not default to TLS")
    (should (equal (ert-simulate-keys "\r\r\r\r"
                     (erc-select-read-args))
                   (list :server "irc.libera.chat"
                         :port 6667
                         :nick (user-login-name)
                         :password nil))))

  (ert-info ("Override default TLS")
    (should (equal (ert-simulate-keys "irc://irc.libera.chat\r\r\r\r"
                     (erc-select-read-args))
                   (list :server "irc.libera.chat"
                         :port 6667
                         :nick (user-login-name)
                         :password nil))))

  (ert-info ("Address includes port")
    (should (equal (ert-simulate-keys
                       "localhost:6667\rnick\r\r"
                     (erc-select-read-args))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"
                         :password nil))))

  (ert-info ("Address includes nick, password skipped via option")
    (should (equal (ert-simulate-keys "nick@localhost:6667\r"
                     (let (erc-prompt-for-password)
                       (erc-select-read-args)))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"
                         :password nil))))

  (ert-info ("Address includes nick and password")
    (should (equal (ert-simulate-keys "nick:sesame@localhost:6667\r\r"
                     (erc-select-read-args))
                   (list :server "localhost"
                         :port 6667
                         :nick "nick"
                         :password "sesame"))))

  (ert-info ("IPv6 address plain")
    (should (equal (ert-simulate-keys "::1\r\r\r\r"
                     (erc-select-read-args))
                   (list :server "[::1]"
                         :port 6667
                         :nick (user-login-name)
                         :password nil))))

  (ert-info ("IPv6 address with port")
    (should (equal (ert-simulate-keys "[::1]:6667\r\r\r"
                     (erc-select-read-args))
                   (list :server "[::1]"
                         :port 6667
                         :nick (user-login-name)
                         :password nil))))

  (ert-info ("IPv6 address includes nick")
    (should (equal (ert-simulate-keys "nick@[::1]:6667\r\r"
                     (erc-select-read-args))
                   (list :server "[::1]"
                         :port 6667
                         :nick "nick"
                         :password nil)))))

(ert-deftest erc-tls ()
  (let (calls)
    (cl-letf (((symbol-function 'user-login-name)
               (lambda (&optional _) "tester"))
              ((symbol-function 'erc-open)
               (lambda (&rest r) (push r calls))))

      (ert-info ("Defaults")
        (erc-tls)
        (should (equal (pop calls)
                       '("irc.libera.chat" 6697 "tester" "unknown" t
                         nil nil nil nil nil "user" nil))))

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
                         "bob:changeme" nil nil nil t "bobo" GNU.org))))

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
                             "bob:changeme" nil nil nil nil "bobo" nil)))))))

(defun erc-tests--make-server-buf (name)
  (with-current-buffer (get-buffer-create name)
    (erc-mode)
    (setq erc-server-process (start-process "sleep" (current-buffer)
                                            "sleep" "1")
          erc-session-server (concat "irc." name ".org")
          erc-session-port 6667
          erc-network (intern name))
    (set-process-query-on-exit-flag erc-server-process nil)
    (current-buffer)))

(defun erc-tests--make-client-buf (server name)
  (unless (bufferp server)
    (setq server (get-buffer server)))
  (with-current-buffer (get-buffer-create name)
    (erc-mode)
    (setq erc--target (erc--target-from-string name))
    (dolist (v '(erc-server-process
                 erc-session-server
                 erc-session-port
                 erc-network))
      (set v (buffer-local-value v server)))
    (current-buffer)))

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

      (with-current-buffer (erc-tests--make-server-buf "foonet")
        (setq rvbuf (current-buffer)))
      (erc-tests--make-server-buf "barnet")
      (erc-tests--make-server-buf "baznet")

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
        (erc-tests--make-client-buf "foonet" "#chan")
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
        (setq rvbuf (lambda () (erc-tests--make-server-buf "gnu")))
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
    (kill-buffer "foonet")
    (kill-buffer "barnet")
    (kill-buffer "baznet")
    (kill-buffer "#chan")))

(ert-deftest erc-migrate-modules ()
  (should (equal (erc-migrate-modules '(autojoin timestamp button))
                 '(autojoin stamp button)))
  ;; Default unchanged
  (should (equal (erc-migrate-modules erc-modules) erc-modules)))

(ert-deftest erc--update-modules ()
  (let (calls
        erc-modules
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (cl-letf (((symbol-function 'require)
               (lambda (s &rest _) (push s calls)))

              ;; Local modules
              ((symbol-function 'erc-fake-bar-mode)
               (lambda (n) (push (cons 'fake-bar n) calls)))

              ;; Global modules
              ((symbol-function 'erc-fake-foo-mode)
               (lambda (n) (push (cons 'fake-foo n) calls)))
              ((get 'erc-fake-foo-mode 'standard-value) 'ignore)
              ((symbol-function 'erc-autojoin-mode)
               (lambda (n) (push (cons 'autojoin n) calls)))
              ((get 'erc-autojoin-mode 'standard-value) 'ignore)
              ((symbol-function 'erc-networks-mode)
               (lambda (n) (push (cons 'networks n) calls)))
              ((get 'erc-networks-mode 'standard-value) 'ignore)
              ((symbol-function 'erc-completion-mode)
               (lambda (n) (push (cons 'completion n) calls)))
              ((get 'erc-completion-mode 'standard-value) 'ignore))

      (ert-info ("Local modules")
        (setq erc-modules '(fake-foo fake-bar))
        (should (equal (erc--update-modules) '(erc-fake-bar-mode)))
        ;; Bar the feature is still required but the mode is not activated
        (should (equal (nreverse calls)
                       '(erc-fake-foo (fake-foo . 1) erc-fake-bar)))
        (setq calls nil))

      (ert-info ("Module name overrides")
        (setq erc-modules '(completion autojoin networks))
        (should-not (erc--update-modules)) ; no locals
        (should (equal (nreverse calls) '( erc-pcomplete (completion . 1)
                                           erc-join (autojoin . 1)
                                           erc-networks (networks . 1))))
        (setq calls nil)))))

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
                          "Some docstring"
                          ((ignore a) (ignore b))
                          ((ignore c) (ignore d)))))

    (should (equal (macroexpand global-module)
                   `(progn

                      (define-minor-mode erc-mname-mode
                        "Toggle ERC mname mode.
With a prefix argument ARG, enable mname if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
Some docstring"
                        :global t
                        :group 'erc-mname
                        (if erc-mname-mode
                            (erc-mname-enable)
                          (erc-mname-disable)))

                      (defun erc-mname-enable ()
                        "Enable ERC mname mode."
                        (interactive)
                        (cl-pushnew 'mname erc-modules)
                        (setq erc-mname-mode t)
                        (ignore a) (ignore b))

                      (defun erc-mname-disable ()
                        "Disable ERC mname mode."
                        (interactive)
                        (setq erc-modules (delq 'mname erc-modules))
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
                           "Some docstring"
                           ((ignore a) (ignore b))
                           ((ignore c) (ignore d))
                           'local))
         (got (macroexpand global-module))
         (arg-en (cadr (nth 2 (nth 2 got))))
         (arg-dis (cadr (nth 2 (nth 3 got)))))

    (should (equal got
                   `(progn
                      (define-minor-mode erc-mname-mode
                        "Toggle ERC mname mode.
With a prefix argument ARG, enable mname if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
Some docstring"
                        :global nil
                        :group 'erc-mname
                        (if erc-mname-mode
                            (erc-mname-enable)
                          (erc-mname-disable)))

                      (defun erc-mname-enable (&optional ,arg-en)
                        "Enable ERC mname mode.
When called interactively, do so in all buffers for the current connection."
                        (interactive "p")
                        (when (derived-mode-p 'erc-mode)
                          (if ,arg-en
                              (erc-with-all-buffers-of-server
                                  erc-server-process nil
                                (erc-mname-enable))
                            (setq erc-mname-mode t)
                            (ignore a) (ignore b))))

                      (defun erc-mname-disable (&optional ,arg-dis)
                        "Disable ERC mname mode.
When called interactively, do so in all buffers for the current connection."
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

;;; erc-tests.el ends here
