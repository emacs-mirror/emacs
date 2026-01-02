;;; erc-tests-common.el --- Common helpers for ERC tests -*- lexical-binding: t -*-

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

;; This file must *not* contain any `ert-deftest' definitions.  See
;; top of test/lisp/erc/erc-tests.el for loading example.
;;
;; Environment variables:
;;
;;  `ERC_PACKAGE_NAME': Name of the installed ERC package currently
;;   running.  ERC needs this in order to load the same package in
;;   tests that run in a subprocess.  Necessary even when the package
;;   name is `erc' and not something like `erc-49860'.
;;
;;  `ERC_TESTS_INIT': The name of an alternate init file.  Mainly for
;;   integrations tests involving starter kits.
;;
;;  `ERC_TESTS_SNAPSHOT_SAVE': When set, ERC saves the current test's
;;   snapshots to disk.
;;

;;; Code:
(require 'ert-x)
(require 'erc)
(eval-when-compile (require 'erc-stamp))
(eval-and-compile
  (let ((load-path (cons (expand-file-name "../erc-d" (ert-resource-directory))
                         load-path)))
    (require 'erc-d-i)))

(defmacro erc-tests-common-equal-with-props (a b)
  "Compare strings A and B for equality including text props.
Use `ert-equal-including-properties' on older Emacsen."
  (list (if (< emacs-major-version 29)
            'ert-equal-including-properties
          'equal-including-properties)
        a b))

;; Caller should probably shadow `erc-insert-modify-hook' or populate
;; user tables for erc-button.
;; FIXME explain this comment ^ in more detail or delete.
(defun erc-tests-common-prep-for-insertion ()
  "Initialize current buffer with essentials for message insertion.
Assume caller intends to use `erc-display-message'."
  (erc-mode)
  (erc--initialize-markers (point) nil)
  (should (= (point) erc-input-marker)))

(defun erc-tests-common-init-server-proc (&rest args)
  "Create a non-network process from ARGS with `make-process'.
Assign the result to `erc-server-process' in the current buffer."
  (setq erc-server-process
        (make-process :name (car args)
                      :buffer (current-buffer)
                      :command args
                      :connection-type 'pipe
                      :noquery t)))

;; After dropping support for Emacs 27, callers can use
;; `get-buffer-create' with INHIBIT-BUFFER-HOOKS.
(defun erc-tests-common-kill-buffers (&rest extra-buffers)
  "Kill all ERC buffers and possibly EXTRA-BUFFERS."
  (let (erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook
        ;; To facilitate automatic testing when a fake-server has already
	;; been created by an earlier ERT test.
	(kill-buffer-query-functions nil))
    (dolist (buf (erc-buffer-list))
      (kill-buffer buf))
    (named-let doit ((buffers extra-buffers))
      (dolist (buf buffers)
        (if (consp buf) (doit buf) (kill-buffer buf))))))

(defun erc-tests-common-with-process-input-spy (test-fn)
  "Mock `erc-process-input-line' and call TEST-FN.
Shadow `erc--input-review-functions' and `erc-pre-send-functions'
with `erc-add-to-input-ring' removed.  Shadow other relevant
variables as nil, and bind `erc-last-input-time' to 0.  Also mock
`erc-server-buffer' to return the current buffer.  Call TEST-FN
with a utility function that returns the set of arguments most
recently passed to the mocked `erc-process-input-line'.  Make
`inhibit-message' non-nil unless running interactively."
  (with-current-buffer (get-buffer-create "FakeNet")
    (let* ((erc--input-review-functions
            (remove 'erc-add-to-input-ring erc--input-review-functions))
           (erc-pre-send-functions
            (remove 'erc-add-to-input-ring erc-pre-send-functions)) ; for now
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
        (funcall test-fn (lambda () (pop calls)))))
    (when noninteractive (kill-buffer))))

(defun erc-tests-common-make-server-buf (&optional name)
  "Return a server buffer named NAME, creating it if necessary.
Use NAME for the network and the session server as well."
  (with-current-buffer (if name
                           (get-buffer-create name)
                         (and (string-search "temp" (buffer-name))
                              (setq name "foonet")
                              (buffer-name)))
    (erc-tests-common-prep-for-insertion)
    (erc-tests-common-init-server-proc "sleep" "1")
    (setq erc-session-server (concat "irc." name ".org")
          erc-server-announced-name (concat "west." name ".org")
          erc-server-users (make-hash-table :test #'equal)
          erc-server-parameters nil
          erc--isupport-params (make-hash-table)
          erc-session-port 6667
          erc-network (intern name)
          erc-server-current-nick "tester"
          ;; Derive ID from nick and network.  To create a "given"
          ;; variant, override manually with a non-nil argument.
          erc-networks--id (erc-networks--id-create nil))
    (current-buffer)))

(defun erc-tests-common-string-to-propertized-parts (string)
  "Return a sequence of `propertize' forms for generating STRING.
Expect maintainers manipulating template catalogs to use this
with `pp-eval-last-sexp' or similar to convert back and forth
between literal strings."
  `(concat
    ,@(mapcar
       (pcase-lambda (`(,beg ,end ,plist))
         ;; At the time of writing, `propertize' produces a string
         ;; with the order of the input plist reversed.
         `(propertize ,(substring-no-properties string beg end)
                      ,@(let (out)
                          (while-let ((plist)
                                      (k (pop plist))
                                      (v (pop plist)))
                            (push (if (or (consp v) (symbolp v)) `',v v) out)
                            (push `',k out))
                          out)))
       (object-intervals string))))

(defun erc-tests-common-pp-propertized-parts (arg)
  "Convert literal string before point into a `propertize'd form.
For simplicity, assume string evaluates to itself."
  (interactive "P")
  (let ((sexp (erc-tests-common-string-to-propertized-parts (pp-last-sexp))))
    (if arg (insert (pp-to-string sexp)) (pp-macroexpand-expression sexp))))


(cl-defun erc-tests-common-add-cmem
    (nick &optional (host "fsf.org")
          (user (concat "~" (substring nick 0 (min 10 (length nick)))))
          (full-name (upcase-initials nick)))
  "Create channel user for NICK with test-oriented defaults."
  (erc-update-channel-member (erc-target) nick nick t nil nil nil nil nil
                             host user full-name))

(defun erc-tests-common-parse-line (line)
  "Return a single `erc-response' parsed from line."
  (let ((parsed (erc-d-i--parse-message line)))
    (make-erc-response :unparsed (erc-d-i-message.unparsed parsed)
                       :sender (erc-d-i-message.sender parsed)
                       :command (erc-d-i-message.command parsed)
                       :command-args (erc-d-i-message.command-args parsed)
                       :contents (erc-d-i-message.contents parsed)
                       :tags (erc-d-i-message.tags parsed))))

(defun erc-tests-common-simulate-line (line)
  "Run response handlers for raw IRC protocol LINE."
  (let ((parsed (erc-tests-common-parse-line line))
        (erc--msg-prop-overrides (or erc--msg-prop-overrides
                                     '((erc--ts . 0)))))
    (erc-call-hooks erc-server-process parsed)))

(defun erc-tests-common-simulate-privmsg (nick msg)
  (erc-tests-common-simulate-line
   (format ":%s PRIVMSG %s :%s"
           (erc-user-spec (erc-get-server-user nick))
           (erc-target)
           msg)))

;; The following utilities are meant to help prepare tests for
;; `erc--get-inserted-msg-bounds' and friends.
(defun erc-tests-common-get-inserted-msg-setup ()
  (erc-tests-common-prep-for-insertion)
  (let ((parsed (make-erc-response :unparsed ":bob PRIVMSG #chan :hi"
                                   :sender "bob"
                                   :command "PRIVMSG"
                                   :command-args (list "#chan" "hi")
                                   :contents "hi"))
        (erc--msg-prop-overrides '((erc--ts . 0))))
    (erc-display-message parsed nil (current-buffer)
                         (erc-format-privmessage "bob" "hi" nil t)))
  (goto-char 3)
  (should (looking-at "<bob> hi")))

;; All these bounds-finding functions take an optional POINT argument.
;; So run each case with and without it at each pos in the message.
(defun erc-tests-common-assert-get-inserted-msg (from to assert-fn)
  (dolist (pt-arg '(nil t))
    (dolist (i (number-sequence from to))
      (goto-char i)
      (ert-info ((format "At %d (%c) %s param" i (char-after i)
                         (if pt-arg "with" "")))
        (funcall assert-fn (and pt-arg i))))))

(defun erc-tests-common-assert-get-inserted-msg/basic (test-fn)
  (erc-tests-common-get-inserted-msg-setup)
  (goto-char 11)
  (should (looking-back "<bob> hi"))
  (erc-tests-common-assert-get-inserted-msg 3 11 test-fn))

(defun erc-tests-common-assert-get-inserted-msg/truncated (test-fn)
  (erc-tests-common-get-inserted-msg-setup)
  (with-silent-modifications (delete-region 1 3))
  (goto-char 9)
  (should (looking-back "<bob> hi"))
  (erc-tests-common-assert-get-inserted-msg 1 9 test-fn))

;; This is a "mixin" and requires a base assertion function, like
;; `erc-tests-common-assert-get-inserted-msg/basic', to work.
(defun erc-tests-common-assert-get-inserted-msg-readonly-with
    (assert-fn test-fn)
  (defvar erc-readonly-mode)
  (defvar erc-readonly-mode-hook)
  (let ((erc-readonly-mode nil)
        (erc-readonly-mode-hook nil)
        (erc-send-post-hook erc-send-post-hook)
        (erc-insert-post-hook erc-insert-post-hook))
    (erc-readonly-mode +1)
    (funcall assert-fn test-fn)))

(defun erc-tests--common-display-message (orig &rest args)
  (require 'erc-stamp)
  (defvar erc-stamp--deferred-date-stamp)
  (let (erc-stamp--deferred-date-stamp)
    (prog1 (apply orig args)
      (when-let* ((inst erc-stamp--deferred-date-stamp)
                  (fn (erc-stamp--date-fn inst)))
        (funcall fn)))))

(defun erc-tests-common-display-message (&rest args)
  (apply #'erc-tests--common-display-message #'erc-display-message args))

(defmacro erc-tests-common-with-date-aware-display-message (&rest body)
  `(progn
     (advice-add 'erc-display-message
                 :around #'erc-tests--common-display-message)
     (unwind-protect (progn ,@body)
       (advice-remove 'erc-display-message
                      #'erc-tests--common-display-message))))

;;;; Buffer snapshots

;; Use this variable to generate new snapshots after carefully
;; reviewing the output of *each* snapshot (not just first and last).
;; Obviously, only run one test at a time.
(defvar erc-tests-common-snapshot-save-p (getenv "ERC_TESTS_SNAPSHOT_SAVE"))

(defun erc-tests-common-snapshot-compare (name dir trans-fn buf-init-fn)
  "Compare `buffer-string' to snapshot NAME.eld in DIR, if present.
When non-nil, run TRANS-FN to filter the current buffer string,
and expect a similar string in return.  Call BUF-INIT-FN, when
non-nil, in the preview buffer after inserting the filtered
string."
  (let* ((expect-file (file-name-with-extension (expand-file-name name dir)
                                                "eld"))
         (erc--own-property-names
          (seq-difference `(font-lock-face ,@erc--own-property-names)
                          `(field display wrap-prefix line-prefix
                                  erc--msg erc--cmd erc--spkr erc--ts erc--ctcp
                                  erc--ephemeral)
                          #'eq))
         (print-circle t)
         (print-escape-newlines t)
         (print-escape-nonascii t)
         (got (erc--remove-text-properties
               (buffer-substring (point-min) erc-insert-marker)))
         (repr (funcall (or trans-fn #'identity) (prin1-to-string got)))
         ;;
         xstr)
    (with-current-buffer (generate-new-buffer name)
      (with-silent-modifications
        (insert (setq got (read repr))))
      (when buf-init-fn (funcall buf-init-fn))
      (erc-mode))
    ;; LHS is a string, RHS is a symbol.
    (if (string= erc-tests-common-snapshot-save-p
                 (ert-test-name (ert-running-test)))
        (let (inhibit-message)
          (with-temp-file expect-file
            (insert repr))
          ;; Limit writing snapshots to one test at a time.
          (message "erc-tests-common-snapshot-compare: wrote %S" expect-file))
      (setq xstr (read (with-temp-buffer
                         (insert-file-contents-literally expect-file)
                         (buffer-string))))
      (unless noninteractive
        (with-current-buffer (generate-new-buffer (format "%s-xpt" name))
          (insert xstr)
          (erc-mode)))
      (if (file-exists-p expect-file)
          ;; Ensure string-valued properties, like timestamps, aren't
          ;; recursive (signals `max-lisp-eval-depth' exceeded).
          (named-let assert-equal
              ((latest (read repr))
               (expect xstr))
            (pcase latest
              ((or "" 'nil) t)
              ((pred stringp)
               (should (equal-including-properties latest expect))
               (let ((latest-intervals (object-intervals latest))
                     (expect-intervals (object-intervals expect)))
                 (while-let ((l-iv (pop latest-intervals))
                             (x-iv (pop expect-intervals))
                             (l-tab (map-into (nth 2 l-iv) 'hash-table))
                             (x-tab (map-into (nth 2 x-iv) 'hash-table)))
                   (pcase-dolist (`(,l-k . ,l-v) (map-pairs l-tab))
                     (assert-equal l-v (gethash l-k x-tab))
                     (remhash l-k x-tab))
                   (should (zerop (hash-table-count x-tab))))))
              ((pred sequencep)
               (assert-equal (seq-first latest) (seq-first expect))
               (assert-equal (seq-rest latest) (seq-rest expect)))
              (_ (should (equal latest expect)))))
        (message "Snapshot file missing: %S" expect-file)))))

(defun erc-tests-common-create-subprocess (code switches libs)
  "Return subprocess for running CODE in an inferior Emacs.
Include SWITCHES, like \"-batch\", as well as libs, after
interspersing \"-l\" between members."
  (let* ((package (if-let* ((found (getenv "ERC_PACKAGE_NAME"))
                            ((string-prefix-p "erc-" found)))
                      (intern found)
                    'erc))
         ;; For integrations testing with managed configs that use a
         ;; different package manager.
         (init (and-let* ((found (getenv "ERC_TESTS_INIT"))
                          (files (split-string found ",")))
                 (mapcan (lambda (f) (list "-l" f)) files)))
         (prog
          `(progn
             ,@(and (not init) (featurep 'compat)
                    `((require 'package)
                      (let ((package-load-list '((compat t) (,package t))))
                        (package-initialize))))
             (require 'erc)
             (cl-assert (equal erc-version ,erc-version) t)
             ,code))
         (proc (make-process
                :name (symbol-name (ert-test-name (ert-running-test)))
                :buffer (current-buffer)
                :command `(,(concat invocation-directory invocation-name)
                           ,@(or init '("-Q"))
                           ,@switches
                           ,@(mapcan (lambda (f) (list "-l" f)) libs)
                           "-eval" ,(format "%S" prog))
                :connection-type 'pipe
                :stderr (messages-buffer)
                :noquery t)))
    proc))

(declare-function erc-track--setup "erc-track" ())

(defun erc-tests-common-track-modified-channels (test)
  (erc-tests-common-prep-for-insertion)
  (setq erc--target (erc--target-from-string "#chan"))
  (erc-tests-common-track-modified-channels-sans-setup test))

(defun erc-tests-common-track-modified-channels-sans-setup (test)
  "Provide a fixture for testing `erc-track-modified-channels'.
Call function TEST with another function that sets the mocked return
value of `erc-track--collect-faces-in' to the given argument, a list of
faces in the reverse order they appear in an inserted message."
  (defvar erc-modified-channels-alist)
  (defvar erc-modified-channels-object)
  (defvar erc-track--attn-faces)
  (defvar erc-track--normal-faces)
  (defvar erc-track--priority-faces)
  (defvar erc-track-faces-normal-list)
  (defvar erc-track-faces-priority-list)
  (defvar erc-track-mode)

  (cl-letf* ((erc-track-mode t)
             (erc-modified-channels-alist nil)
             (erc-modified-channels-object erc-modified-channels-object)
             (faces ())
             ((symbol-function 'force-mode-line-update) #'ignore)
             ((symbol-function 'erc-faces-in) (lambda (_) faces))
             ((symbol-function 'erc-track--collect-faces-in)
              (lambda ()
                (cons (map-into (mapcar (lambda (f) (cons f t)) faces)
                                '(hash-table :test equal))
                      faces))))
    (erc-track--setup)

    ;; Faces from `erc-track--attn-faces' prepended.
    (should (= (+ (length erc-track--attn-faces)
                  (length erc-track-faces-priority-list))
               (hash-table-count erc-track--priority-faces)))
    (should (= (length erc-track-faces-normal-list)
               (hash-table-count erc-track--normal-faces)))

    (funcall test (lambda (arg) (setq faces arg)))))

;; To use this function, add something like
;;
;;   ("lisp/erc"
;;    (emacs-lisp-mode (eval erc-tests-common-add-imenu-expressions)))
;;
;; to your ~/emacs/master/.dir-locals-2.el.  Optionally, add the sexp
;;
;;   (erc-tests-common-add-imenu-expressions)
;;
;; to the user option `safe-local-eval-forms', and load this file before
;; hacking, possibly by autoloading this function in your init.el.
(defun erc-tests-common-add-imenu-expressions (&optional removep)
  "Tell `imenu' about ERC-defined macros.  With REMOVEP, do the opposite."
  (interactive "P")
  ;; This currently produces results like "ERC response FOO BAR", but it
  ;; would be preferable to end up with "erc-response-FOO" and
  ;; "erc-response-BAR" instead, possibly as separate items.  Likewise
  ;; for modules: "erc-foo-mode" instead of "ERC module foo".
  (dolist (item `(("ERC response"
                   ,(rx bol (* (syntax whitespace))
                        "(define-erc-response-handler (" (group (+ nonl)) ")")
                   1)
                  ("ERC module"
                   ,(rx bol (* (syntax whitespace))
                        ;; No `lisp-mode-symbol' in < Emacs 29.
                        "(define-erc-module " (group (+ (| (syntax word)
                                                           (syntax symbol)
                                                           (: "\\" nonl)))))
                   1)))
    ;; This should only run in `emacs-lisp-mode' buffers, which have
    ;; this variable set locally.
    (cl-assert (local-variable-p 'imenu-generic-expression))
    (if removep
        (setq imenu-generic-expression
              (remove item imenu-generic-expression))
      (cl-pushnew item imenu-generic-expression :test #'equal))))

(provide 'erc-tests-common)
