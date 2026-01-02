;;; erc-scenarios-common.el --- Common helpers for ERC scenarios -*- lexical-binding: t -*-

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

;;; Commentary:

;; These are e2e-ish test cases primarily intended to assert core,
;; fundamental behavior expected of any modern IRC client.  Tests may
;; also simulate specific scenarios drawn from bug reports.  Incoming
;; messages are provided by playback scripts resembling I/O logs.  In
;; place of time stamps, they have time deltas, which are used to
;; govern the test server in a fashion reminiscent of music rolls (or
;; the script(1) UNIX program).  These scripts can be found in the
;; other directories under test/lisp/erc/resources.
;;
;; Isolation:
;;
;; The set of enabled modules is shared among all tests.  The function
;; `erc-update-modules' activates them (as minor modes), but it never
;; deactivates them.  So there's no going back, and let-binding
;; `erc-modules' is useless.  The safest route is therefore to (1)
;; assume the set of default modules is already activated or will be
;; over the course of the test session and (2) let-bind relevant user
;; options as needed.  For example, to limit the damage of
;; `erc-autojoin-channels-alist' to a given test, assume the
;; `erc-join' library has already been loaded or will be on the next
;; call to `erc-open'.  And then just let-bind
;; `erc-autojoin-channels-alist' for the duration of the test.
;;
;; Playing nice:
;;
;; Right now, these tests all rely on an ugly fixture macro named
;; `erc-scenarios-common-with-cleanup', which is defined just below.
;; It helps restore (but not really prepare) the environment by
;; destroying any stray processes or buffers named in the first
;; argument, a `let*'-style VAR-LIST.  Relying on such a macro is
;; unfortunate because in many ways it actually hampers readability by
;; favoring magic over verbosity.  But without it (or something
;; similar), any failing test would cause all subsequent tests in a
;; file to fail like dominoes (making all but the first backtrace
;; useless).
;;
;; Misc:
;;
;; Note that in the following examples, nicknames Alice and Bob are
;; always associated with the fake network FooNet, while nicks Joe and
;; Mike are always on BarNet.  (Networks are sometimes downcased.)
;;
;; Environment variables:
;;
;;  `ERC_TESTS_GRAPHICAL': Internal variable to unskip those few tests
;;   capable of running consecutively while interactive on a graphical
;;   display.  This triggers both the tests and the suite to commence
;;   with teardown activities normally skipped to allow for inspection
;;   while interactive.  This is also handy when needing to quickly
;;   run `ert-results-rerun-test-at-point-debugging-errors' on a
;;   failing test because you don't have to go around hunting for and
;;   killing associated buffers and processes.
;;
;;  `ERC_TESTS_GRAPHICAL_ALL': Currently targets a single "meta" test,
;;   `erc-scenarios-internal--run-interactive-all', that runs all
;;   tests tagged `:erc--graphical' in an interactive subprocess.
;;
;;  `ERC_TESTS_SUBPROCESS': Used internally to detect nested tests.
;;
;;  `ERC_D_DEBUG': Tells `erc-d' to emit debugging info to stderr.
;;
;; XXX This file should *not* contain any test cases.

;;; Code:

(require 'ert-x) ; cl-lib
(eval-and-compile
  (let* ((d (expand-file-name ".." (ert-resource-directory)))
         (load-path (cons (concat d "/erc-d") load-path)))
    (require 'erc-d-t)
    (require 'erc-d)))

(require 'erc)

(eval-when-compile (require 'erc-join)
                   (require 'erc-services)
                   (require 'erc-fill))

(declare-function erc-network "erc-networks")
(defvar erc-network)

(defvar erc-scenarios-common--resources-dir
  (expand-file-name "../" (ert-resource-directory)))

;; Teardown is already inhibited when running interactively, which
;; prevents subsequent tests from succeeding, so we might as well
;; treat inspection as the goal.
(unless noninteractive
  (setq erc-server-auto-reconnect nil))

(defvar erc-scenarios-common-dialog nil)
(defvar erc-scenarios-common-extra-teardown nil)
(defvar erc-scenarios-common--graphical-p nil)

(defun erc-scenarios-common--add-silence ()
  (advice-add #'erc-login :around #'erc-d-t-silence-around)
  (advice-add #'erc-handle-login :around #'erc-d-t-silence-around)
  (advice-add #'erc-server-connect :around #'erc-d-t-silence-around))

(defun erc-scenarios-common--remove-silence ()
  (advice-remove #'erc-login #'erc-d-t-silence-around)
  (advice-remove #'erc-handle-login #'erc-d-t-silence-around)
  (advice-remove #'erc-server-connect #'erc-d-t-silence-around))

(defun erc-scenarios-common--print-trace ()
  (when (and (boundp 'trace-buffer) (get-buffer trace-buffer))
    (with-current-buffer trace-buffer
      (message "%S" (buffer-string))
      (kill-buffer))))

(eval-and-compile
  (defun erc-scenarios-common--make-bindings (bindings)
    `((erc-scenarios-common--graphical-p
       (and (or erc-scenarios-common--graphical-p
                (memq :erc--graphical (ert-test-tags (ert-running-test))))
            (not (and noninteractive (ert-skip "Interactive only")))))
      (erc-d-u-canned-dialog-dir (expand-file-name
                                  (or erc-scenarios-common-dialog
                                      (cadr (assq 'erc-scenarios-common-dialog
                                                  ',bindings)))
                                  erc-scenarios-common--resources-dir))
      (erc-d-tmpl-vars `(,@erc-d-tmpl-vars
                         (quit . ,(erc-quit/part-reason-default))
                         (erc-version . ,erc-version)))
      (erc-modules (copy-sequence erc-modules))
      (debug-on-error t)
      (inhibit-interaction noninteractive)
      (auth-source-do-cache nil)
      (timer-list (copy-sequence timer-list))
      (timer-idle-list (copy-sequence timer-idle-list))
      (erc-auth-source-parameters-join-function nil)
      (erc-autojoin-channels-alist nil)
      (erc-server-auto-reconnect nil)
      (erc-after-connect nil)
      (erc-last-input-time 0)
      (erc-d-linger-secs 10)
      ,@bindings)))

(defmacro erc-scenarios-common-with-cleanup (bindings &rest body)
  "Provide boilerplate cleanup tasks after calling BODY with BINDINGS.

If an `erc-d' process exists, wait for it to start before running BODY.
If `erc-autojoin-mode' mode is bound, restore it during cleanup if
disabled by BODY.  Other defaults common to these test cases are added
below and can be overridden, except when wanting the \"real\" default
value, which must be looked up or captured outside of the calling form.

When running tests tagged as serially runnable while interactive
and the flag `erc-scenarios-common--graphical-p' is non-nil, run
teardown tasks normally inhibited when interactive.  That is,
behave almost as if `noninteractive' were also non-nil, and
ensure buffers and other resources are destroyed on completion.

Dialog resource directories are located by expanding the variable
`erc-scenarios-common-dialog' or its value in BINDINGS."
  (declare (indent 1))

  (let* ((orig-autojoin-mode (make-symbol "orig-autojoin-mode"))
         (combined `((,orig-autojoin-mode (bound-and-true-p erc-autojoin-mode))
                     ,@(erc-scenarios-common--make-bindings bindings))))

    `(erc-d-t-with-cleanup (,@combined)

         (ert-info ("Restore autojoin, etc., kill ERC buffers")
           (dolist (buf (buffer-list))
             (when-let* ((erc-d-u--process-buffer)
                         (proc (get-buffer-process buf)))
               (delete-process proc)))

           (erc-scenarios-common--remove-silence)

           (when erc-scenarios-common-extra-teardown
             (ert-info ("Running extra teardown")
               (funcall erc-scenarios-common-extra-teardown)))

           (erc-buffer-do #'erc-scenarios-common--assert-date-stamps)
           (when (and (boundp 'erc-autojoin-mode)
                      (not (eq erc-autojoin-mode ,orig-autojoin-mode)))
             (erc-autojoin-mode (if ,orig-autojoin-mode +1 -1)))

           (when (or noninteractive erc-scenarios-common--graphical-p)
             (when noninteractive
               (erc-scenarios-common--print-trace))
             (erc-d-t-kill-related-buffers)
             (delete-other-windows)))

       (erc-scenarios-common--add-silence)

       (ert-info ("Wait for dumb server")
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when erc-d-u--process-buffer
               (erc-d-t-search-for 3 "Starting")))))

       (ert-info ("Activate erc-debug-irc-protocol")
         (unless (and (or noninteractive erc-scenarios-common--graphical-p)
                      (not erc-debug-irc-protocol))
           (erc-toggle-debug-irc-protocol)))

       ,@body)))

(defvar erc-scenarios-common--term-size '(34 . 80))
(declare-function term-char-mode "term" nil)
(declare-function term-line-mode "term" nil)

;; Much of this concerns accommodating test environments outside of
;; the emacs.git tree, such as CI jobs running ERC's ELPA-package on
;; older Emacsen.  See also `erc-tests--assert-printed-in-subprocess'.
(defun erc-scenarios-common--run-in-term (&optional debug)
  (require 'term)
  (let* ((default-directory (or (getenv "EMACS_TEST_DIRECTORY")
                                (expand-file-name
                                 ".." erc-scenarios-common--resources-dir)))
         ;; In the emacs.git tree, "HOME" will be "/nonexistent", which
         ;; is fine because we don't need any ELPA packages.
         (process-environment (cons "ERC_TESTS_SUBPROCESS=1"
                                    process-environment))
         (name (ert-test-name (ert-running-test)))
         (temp-file (make-temp-file "erc-term-test-"))
         (cmd `(let (stats)
                 (setq enable-dir-local-variables nil)
                 (unwind-protect
                     (setq stats (ert-run-tests-batch ',name))
                   (unless ',debug
                     (let ((buf (with-current-buffer (messages-buffer)
                                  (buffer-string))))
                       (with-temp-file ,temp-file
                         (insert buf)))
                     (kill-emacs
                      (if stats (ert-stats-completed-unexpected stats) 1))))))
         ;; The `ert-test' object in Emacs 29 has a `file-name' field.
         (file-name (symbol-file name 'ert--test))
         (default-directory (expand-file-name (file-name-directory file-name)))
         (package (if-let* ((found (getenv "ERC_PACKAGE_NAME"))
                            ((string-prefix-p "erc-" found)))
                      (intern found)
                    'erc))
         (init (and-let* ((found (getenv "ERC_TESTS_INIT"))
                          (files (split-string found ",")))
                 (mapcan (lambda (f) (list "-l" f)) files)))
         (setup `(progn
                   ,@(and (not init) (featurep 'compat)
                          `((require 'package)
                            (let ((package-load-list
                                   '((compat t) (,package t))))
                              (package-initialize))))
                   (require 'erc)
                   (cl-assert (equal erc-version ,erc-version) t)))
         ;; Make subprocess terminal bigger than controlling.
         (buf (cl-letf (((symbol-function 'window-screen-lines)
                         (lambda () (car erc-scenarios-common--term-size)))
                        ((symbol-function 'window-max-chars-per-line)
                         (lambda () (cdr erc-scenarios-common--term-size))))
                (apply #'make-term (symbol-name name)
                       (expand-file-name invocation-name invocation-directory)
                       nil `(,@(or init '("-Q")) "-nw"
                             "-eval" ,(format "%S" setup)
                             "-l" ,file-name
                             "-eval" ,(format "%S" cmd)))))
         (proc (get-buffer-process buf))
         (err (lambda ()
                (with-temp-buffer
                  (insert-file-contents temp-file)
                  (message "Subprocess: %s" (buffer-string))
                  (delete-file temp-file)))))
    (unless noninteractive
      (set-window-buffer (selected-window) buf)
      (delete-other-windows))
    (with-current-buffer buf
      (set-process-query-on-exit-flag proc nil)
      (unless noninteractive (term-char-mode))
      (erc-d-t-wait-for 30 (process-live-p proc))
      (while (accept-process-output proc))
      (term-line-mode)
      (goto-char (point-min))
      ;; Otherwise gives process exited abnormally with exit-code >0
      (unless (search-forward (format "Process %s finished" name) nil t)
        (funcall err)
        (ert-fail (when (search-forward "exited" nil t)
                    (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position)))))
      (delete-file temp-file)
      (when noninteractive
        (kill-buffer)))))

(defvar erc-scenarios-common-interactive-debug-term-p nil
  "Non-nil means run test in an inferior Emacs, even if interactive.")

(defmacro erc-scenarios-common-with-noninteractive-in-term (&rest body)
  "Run BODY via `erc-scenarios-common-with-cleanup' in a `term' subprocess.
Also do so when `erc-scenarios-common-interactive-debug-term-p'
is non-nil.  When debugging, leave the `term-mode' buffer around
for inspection and name it after the test, bounded by asterisks.
When debugging, ensure the test always fails, as a reminder to
disable `erc-scenarios-common-interactive-debug-term-p'.

See Info node `(emacs) Term Mode' for the various commands."
  (declare (indent 1))
  `(if (and (or erc-scenarios-common-interactive-debug-term-p
                noninteractive)
            (not (getenv "ERC_TESTS_SUBPROCESS")))
       (progn
         (when (memq system-type '(windows-nt ms-dos cygwin haiku))
           (ert-skip "System must be UNIX-like"))
         (erc-scenarios-common--run-in-term
          erc-scenarios-common-interactive-debug-term-p))
     (erc-scenarios-common-with-cleanup ,@body)))

(defun erc-scenarios-common--assert-date-stamps ()
  "Ensure all date stamps are accounted for."
  (dolist (stamp erc-stamp--date-stamps)
    (should (eq 'datestamp (get-text-property (erc-stamp--date-marker stamp)
                                              'erc--msg)))))

(defun erc-scenarios-common-assert-initial-buf-name (id port)
  ;; Assert no limbo period when explicit ID given
  (should (string= (if id
                       (format "%s" id)
                     (format "127.0.0.1:%d" port))
                   (buffer-name))))

(defun erc-scenarios-common-buflist (prefix)
  "Return list of buffers with names sharing PREFIX."
  (let (case-fold-search)
    (erc-networks--id-sort-buffers
     (delq nil
           (mapcar (lambda (b)
                     (when (string-prefix-p prefix (buffer-name b)) b))
                   (buffer-list))))))

;; This is more realistic than `erc-send-message' because it runs
;; `erc-pre-send-functions', etc.  Keyboard macros may be preferable,
;; but they sometimes experience complications when an earlier test
;; has failed.
(defun erc-scenarios-common-say (str)
  (let (erc-accidental-paste-threshold-seconds)
    (goto-char erc-input-marker)
    (insert str)
    (erc-send-current-line)))

(defun erc-scenarios-common--at-win-end-p (&optional window)
  (= (window-body-height window)
     (count-screen-lines (window-start window) (point-max) nil window)))

(defun erc-scenarios-common--above-win-end-p (&optional window)
  (> (window-body-height window)
     (count-screen-lines (window-start window) (point-max))))

(defun erc-scenarios-common--prompt-past-win-end-p (&optional window)
  (< (window-body-height window)
     (count-screen-lines (window-start window) (point-max))))

(defun erc-scenarios-common--recenter-top-bottom-around (orig &rest args)
  (let (this-command last-command) (apply orig args)))

(defun erc-scenarios-common--recenter-top-bottom ()
  (advice-add 'recenter-top-bottom
              :around #'erc-scenarios-common--recenter-top-bottom-around)
  (execute-kbd-macro "\C-l")
  (advice-remove 'recenter-top-bottom
                 #'erc-scenarios-common--recenter-top-bottom-around))


;;;; Fixtures

(defun erc-scenarios-common-scrolltobottom--normal (test)
  (erc-scenarios-common-with-noninteractive-in-term
      ((erc-scenarios-common-dialog "scrolltobottom")
       (dumb-server (erc-d-run "localhost" t 'help))
       (port (process-contact dumb-server :service))
       (erc-modules `(scrolltobottom fill-wrap ,@erc-modules))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")
        (funcall expect 10 "debug mode")))

    (with-current-buffer "foonet"
      (should (looking-at " and"))
      (set-window-buffer nil (current-buffer))
      (delete-other-windows)
      (split-window-below 15)
      (recenter 0)

      (ert-info ("Moving into prompt in other window triggers scroll")
        (with-selected-window (next-window)
          (should-not (erc-scenarios-common--at-win-end-p))
          (goto-char (1- erc-insert-marker))
          (execute-kbd-macro "\C-n")
          ;; Ensure point is at prompt and aligned to bottom.
          (should (erc-scenarios-common--at-win-end-p))))

      (ert-info ("Module `move-to-prompt' still works")
        ;; Prompt is somewhere in the middle of the window.
        (should (erc-scenarios-common--above-win-end-p))
        ;; Hitting a self-insert key triggers `move-to-prompt' as well
        ;; as a scroll (to bottom).
        (execute-kbd-macro "hi")
        ;; Prompt and input appear on last line of window.
        (should (erc-scenarios-common--at-win-end-p)))

      (ert-info ("Command `recenter-top-bottom' disallowed at prompt")
        ;; Hitting C-l does not recenter the window.
        (erc-scenarios-common--recenter-top-bottom)
        (should (erc-scenarios-common--at-win-end-p))
        (erc-scenarios-common--recenter-top-bottom)
        (should (erc-scenarios-common--at-win-end-p)))

      (ert-info ("Command `beginning-of-buffer' allowed at prompt")
        ;; Hitting C-< goes to beginning of buffer.
        (call-interactively #'beginning-of-buffer)
        (should (= 1 (point)))
        (redisplay)
        (should (zerop (count-screen-lines (window-start) (point))))
        (should (erc-scenarios-common--prompt-past-win-end-p)))

      (ert-info ("New message doesn't trigger scroll when away from prompt")
        ;; Arriving insertions don't trigger a scroll when away from the
        ;; prompt.  New output not seen.
        (erc-cmd-MSG "NickServ help register")
        (save-excursion (erc-d-t-search-for 10 "End of NickServ"))
        (should (= 1 (point)))
        (redisplay)
        (should (zerop (count-screen-lines (window-start) (window-point))))
        (should (erc-scenarios-common--prompt-past-win-end-p)))

      (funcall test)

      (ert-info ("New message does trigger a scroll when at prompt")
        ;; Recenter so prompt is above rather than at window's end.
        (funcall expect 10 "If you are currently logged in")
        (recenter 0)
        ;; Prompt is somewhere in the middle of the window.
        (erc-d-t-wait-for 10 (erc-scenarios-common--above-win-end-p))
        (erc-scenarios-common-say "/msg NickServ help identify")
        ;; New arriving messages trigger a snap when inserted.
        (erc-d-t-wait-for 10 (erc-scenarios-common--at-win-end-p))
        (funcall expect 10 "IDENTIFY lets you login"))

      (erc-scrolltobottom-mode -1))))

(cl-defun erc-scenarios-common--base-network-id-bouncer
    ((&key autop foo-id bar-id after
           &aux
           (foo-id (and foo-id 'oofnet))
           (bar-id (and bar-id 'rabnet))
           (serv-buf-foo (if foo-id "oofnet" "foonet"))
           (serv-buf-bar (if bar-id "rabnet" "barnet"))
           (chan-buf-foo (if foo-id "#chan@oofnet" "#chan@foonet"))
           (chan-buf-bar (if bar-id "#chan@rabnet" "#chan@barnet")))
     &rest dialogs)
  "Ensure retired option `erc-rename-buffers' is now the default behavior.
The option `erc-rename-buffers' is now deprecated and on by default, so
this now just asserts baseline behavior.  Originally from scenario
clash-of-chans/rename-buffers as explained in Bug#48598: 28.0.50;
buffer-naming collisions involving bouncers in ERC."
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/netid/bouncer")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (apply #'erc-d-run "localhost" t dialogs))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-auto-reconnect autop)
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"
                                           :id foo-id))
        (setq erc-server-process-foo erc-server-process)
        (erc-scenarios-common-assert-initial-buf-name foo-id port)
        (erc-d-t-wait-for 6 (eq (erc-network) 'foonet))
        (erc-d-t-wait-for 3 (string= (buffer-name) serv-buf-foo))
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
                                           :id bar-id))
        (setq erc-server-process-bar erc-server-process)
        (erc-scenarios-common-assert-initial-buf-name bar-id port)
        (erc-d-t-wait-for 6 (eq (erc-network) 'barnet))
        (erc-d-t-wait-for 3 (string= (buffer-name) serv-buf-bar))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique, no names based on IPs")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar))
      (should-not (erc-scenarios-common-buflist "127.0.0.1")))

    (ert-info ("Join #chan@barnet")
      (with-current-buffer erc-server-buffer-bar (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 5 "Exactly 2 #chan-prefixed buffers exist"
      (equal (list (get-buffer chan-buf-bar)
                   (get-buffer chan-buf-foo))
             (erc-scenarios-common-buflist "#chan")))

    (ert-info ("#chan@<esid> is exclusive to foonet")
      (with-current-buffer chan-buf-foo
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))
        (erc-d-t-search-for 15 "ape is dead")
        (erc-d-t-wait-for 5 (not (erc-server-process-alive)))))

    (ert-info ("#chan@<esid> is exclusive to barnet")
      (with-current-buffer chan-buf-bar
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (erc-d-t-wait-for 5 (eq erc-server-process erc-server-process-bar))
        (erc-d-t-search-for 15 "joe: It is a rupture")
        (erc-d-t-wait-for 5 (not (erc-server-process-alive)))))

    (when after (funcall after))))

(defun erc-scenarios-common--clash-rename-pass-handler (dialog exchange)
  (when (eq (erc-d-dialog-name dialog) 'stub-again)
    (let* ((match (erc-d-exchange-match exchange 1))
           (sym (if (string= match "foonet") 'foonet-again 'barnet-again)))
      (should (member match (list "foonet" "barnet")))
      (erc-d-load-replacement-dialog dialog sym 1))))

(defun erc-scenarios-common--base-network-id-bouncer--reconnect (foo-id bar-id)
  (let ((erc-d-tmpl-vars '((token . (group (| "barnet" "foonet")))))
        (erc-d-match-handlers
         ;; Auto reconnect is nondeterministic, so let computer decide
         (list :pass #'erc-scenarios-common--clash-rename-pass-handler))
        (after
         (lambda ()
           ;; Simulate disconnection and `erc-server-auto-reconnect'
           (ert-info ("Reconnect to foonet and barnet back-to-back")
             (with-current-buffer (if foo-id "oofnet" "foonet")
               (erc-d-t-wait-for 10 (erc-server-process-alive)))
             (with-current-buffer (if bar-id "rabnet" "barnet")
               (erc-d-t-wait-for 10 (erc-server-process-alive))))

           (ert-info ("#chan@foonet is exclusive to foonet")
             (with-current-buffer (if foo-id "#chan@oofnet" "#chan@foonet")
               (erc-d-t-search-for 1 "<alice>")
               (erc-d-t-absent-for 0.1 "<joe>")
               (erc-d-t-search-for 20 "please your lordship")))

           (ert-info ("#chan@barnet is exclusive to barnet")
             (with-current-buffer (if bar-id "#chan@rabnet" "#chan@barnet")
               (erc-d-t-search-for 1 "<joe>")
               (erc-d-t-absent-for 0.1 "<bob>")
               (erc-d-t-search-for 20 "much in private")))

           ;; XXX this is important (reconnects overlapped, so we'd get
           ;; chan@127.0.0.1:6667)
           (should-not (erc-scenarios-common-buflist "127.0.0.1"))
           ;; Reconnection order doesn't matter here because session objects
           ;; are persisted, meaning original timestamps preserved.
           (should (equal (list (get-buffer (if bar-id "#chan@rabnet"
                                              "#chan@barnet"))
                                (get-buffer (if foo-id "#chan@oofnet"
                                              "#chan@foonet")))
                          (erc-scenarios-common-buflist "#chan"))))))
    (erc-scenarios-common--base-network-id-bouncer
     (list :autop t :foo-id foo-id :bar-id bar-id :after after)
     'foonet-drop 'barnet-drop
     'stub-again 'stub-again
     'foonet-again 'barnet-again)))

(defun erc-scenarios-common--upstream-reconnect (test &rest dialogs)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/upstream-reconnect")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (apply #'erc-d-run "localhost" t dialogs))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester@vanilla/foonet"
                                :password "changeme"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 6 (eq (erc-network) 'foonet))
        (erc-d-t-wait-for 3 (string= (buffer-name) "foonet"))
        (funcall expect 5 "foonet")))

    (ert-info ("Join #chan@foonet")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 5 "<alice>")))

    (ert-info ("Connect to barnet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester@vanilla/barnet"
                                :password "changeme"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 10 (eq (erc-network) 'barnet))
        (erc-d-t-wait-for 3 (string= (buffer-name) "barnet"))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique, no names based on IPs")
      (should-not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan@foonet"))
      (funcall expect 5 "#chan was created on ")
      (ert-info ("Joined again #chan@foonet")
        (funcall expect 10 "#chan was created on "))
      (funcall expect 10 "My lord, in heart"))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan@barnet"))
      (funcall expect 5 "#chan was created on ")
      (ert-info ("Joined again #chan@barnet")
        (funcall expect 10 "#chan was created on "))
      (funcall expect 10 "Go to; farewell"))

    (funcall test)))

;; XXX this is okay, but we also need to check that target buffers are
;; already associated with a new process *before* a JOIN is sent by a
;; server's playback burst.  This doesn't do that.
;;
;; This *does* check that superfluous JOINs sent by the autojoin
;; module are harmless when they're not acked (superfluous because the
;; bouncer/server intitates the JOIN).

(defun erc-scenarios-common--join-network-id (foo-reconnector foo-id bar-id)
  "Ensure channels rejoined by erc-join.el DTRT.
Originally from scenario clash-of-chans/autojoin as described in
Bug#48598: 28.0.50; buffer-naming collisions involving bouncers in ERC."
  (erc-scenarios-common-with-cleanup
      ((chan-buf-foo (format "#chan@%s" (or foo-id "foonet")))
       (chan-buf-bar (format "#chan@%s" (or bar-id "barnet")))
       (erc-scenarios-common-dialog "join/network-id")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.5)
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet 'foonet-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (should (memq 'autojoin erc-modules))

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"
                                           :id foo-id))
        (setq erc-server-process-foo erc-server-process)
        (erc-scenarios-common-assert-initial-buf-name foo-id port)
        (erc-d-t-wait-for 5 (eq (erc-network) 'foonet))
        (funcall expect 5 "foonet")))

    (ert-info ("Join #chan, find sentinel, quit")
      (with-current-buffer erc-server-buffer-foo (erc-cmd-JOIN "#chan"))
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 5 "vile thing")
        (erc-cmd-QUIT "")

        (ert-info ("Prompt hidden in channel buffer upon quitting")
          (erc-d-t-wait-for 10 (erc--prompt-hidden-p))
          (should (overlays-in erc-insert-marker erc-input-marker)))))

    (with-current-buffer erc-server-buffer-foo
      (ert-info ("Prompt hidden after process dies in server buffer")
        (erc-d-t-wait-for 2 (not (erc-server-process-alive)))
        (erc-d-t-wait-for 10 (erc--prompt-hidden-p))
        (should (overlays-in erc-insert-marker erc-input-marker))))

    (should (equal erc-autojoin-channels-alist
                   (if foo-id '((oofnet "#chan")) '((foonet "#chan")))))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"
                                           :id bar-id))
        (setq erc-server-process-bar erc-server-process)
        (erc-d-t-wait-for 5 (eq erc-network 'barnet))
        (should (string= (buffer-name) (if bar-id "rabnet" "barnet")))))

    (ert-info ("Server buffers are unique, no stray IP-based names")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar))
      (should-not (erc-scenarios-common-buflist "127.0.0.1")))

    (ert-info ("Only one #chan buffer exists")
      (should (equal (list (get-buffer "#chan"))
                     (erc-scenarios-common-buflist "#chan"))))

    (ert-info ("#chan is not auto-joined")
      (with-current-buffer "#chan"
        (erc-d-t-absent-for 0.1 "<joe>")
        (should-not (process-live-p erc-server-process))
        (erc-d-t-ensure-for 0.1 "server buffer remains foonet"
          (eq erc-server-process erc-server-process-foo))))

    (with-current-buffer erc-server-buffer-bar
      (erc-cmd-JOIN "#chan")
      (erc-d-t-wait-for 3 (get-buffer chan-buf-foo))
      (erc-d-t-wait-for 3 (get-buffer chan-buf-bar))
      (with-current-buffer chan-buf-bar
        (erc-d-t-wait-for 3 (eq erc-server-process erc-server-process-bar))
        (funcall expect 5 "marry her instantly")))

    (ert-info ("Reconnect to foonet")
      (with-current-buffer (setq erc-server-buffer-foo
                                 (funcall foo-reconnector))
        (should (member (if foo-id '(oofnet "#chan") '(foonet "#chan"))
                        erc-autojoin-channels-alist))
        (erc-d-t-wait-for 3 (erc-server-process-alive))
        (setq erc-server-process-foo erc-server-process)
        (erc-d-t-wait-for 2 (eq erc-network 'foonet))
        (should (string= (buffer-name) (if foo-id "oofnet" "foonet")))

        (ert-info ("Prompt unhidden")
          (should-not (erc--prompt-hidden-p))
          (should-not (overlays-in erc-insert-marker erc-input-marker)))
        (funcall expect 5 "foonet")))

    (ert-info ("#chan@foonet is clean, no cross-contamination")
      (with-current-buffer chan-buf-foo
        (erc-d-t-wait-for 3 (eq erc-server-process erc-server-process-foo))
        (funcall expect 3 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (funcall expect 30 "not given me")

        (ert-info ("Prompt unhidden")
          (should-not (erc--prompt-hidden-p))
          (should-not (overlays-in erc-insert-marker erc-input-marker)))))

    (ert-info ("All #chan@barnet output received")
      (with-current-buffer chan-buf-bar
        (funcall expect 10 "hath an uncle here")))))

(provide 'erc-scenarios-common)

;;; erc-scenarios-common.el ends here
