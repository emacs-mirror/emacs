;;; erc-tests.el --- Tests for erc.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
(require 'erc-networks)

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

(ert-deftest erc--meta--backend-dependencies ()
  (with-temp-buffer
    (insert-file-contents-literally
     (concat (file-name-sans-extension (symbol-file 'erc)) ".el"))
    (let ((beg (search-forward ";; Defined in erc-backend"))
          (end (search-forward "\n\n"))
          vars)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (with-syntax-table lisp-data-mode-syntax-table
            (condition-case _
                (while (push (cadr (read (current-buffer))) vars))
              (end-of-file)))))
      (should (= (point) end))
      (dolist (var vars)
        (setq var (concat "\\_<" (symbol-name var) "\\_>"))
        (ert-info (var)
          (should (save-excursion (search-forward-regexp var nil t))))))))

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
            erc-default-recipients '("#chan")))

    (with-current-buffer (get-buffer-create "bob")
      (erc-tests--send-prep)
      (goto-char erc-insert-marker)
      (should (looking-at-p (regexp-quote erc-prompt)))
      (setq erc-server-process (buffer-local-value 'erc-server-process
                                                   (get-buffer "ServNet"))
            erc-default-recipients '("bob")))

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
      (should (equal (erc-downcase "Bob[m]`") "bob[m]`"))
      (should (equal (erc-downcase "Tilde~") "tilde~" ))
      (should (equal (erc-downcase "\\O/") "\\o/" )))

    (ert-info ("rfc1459")
      (puthash 'CASEMAPPING  '("rfc1459") erc--isupport-params)
      (should (equal (erc-downcase "Bob[m]`") "bob{m}`" ))
      (should (equal (erc-downcase "Tilde~") "tilde^" ))
      (should (equal (erc-downcase "\\O/") "|o/" )))

    (ert-info ("rfc1459-strict")
      (puthash 'CASEMAPPING  '("rfc1459-strict") erc--isupport-params)
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
        (should (save-excursion (forward-line -1) (goto-char (pos-bol))
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

;;; erc-tests.el ends here
