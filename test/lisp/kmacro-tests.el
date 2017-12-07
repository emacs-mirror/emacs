;;; kmacro-tests.el --- Tests for kmacro.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Gemini Lasswell <gazally@runbox.com>

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

;;; Code:

(require 'kmacro)
(require 'seq)
(require 'ert)
(require 'ert-x)

;;; Test fixtures:

(defmacro kmacro-tests-with-kmacro-clean-slate (&rest body)
  "Create a clean environment for a kmacro test BODY to run in."
  (declare (debug (body)))
  `(cl-letf* ((kmacro-execute-before-append t)
              (kmacro-ring-max 8)
              (kmacro-repeat-no-prefix t)
              (kmacro-call-repeat-key nil)
              (kmacro-call-repeat-with-arg nil)

              (kbd-macro-termination-hook nil)
              (defining-kbd-macro nil)
              (executing-kbd-macro nil)
              (executing-kbd-macro-index 0)
              (last-kbd-macro nil)

              (kmacro-ring nil)

              (kmacro-counter 0)
              (kmacro-default-counter-format "%d")
              (kmacro-counter-format "%d")
              (kmacro-counter-format-start "%d")
              (kmacro-counter-value-start 0)
              (kmacro-last-counter 0)
              (kmacro-initial-counter-value nil)

              (kmacro-tests-macros nil)
              (kmacro-tests-events nil)
              (kmacro-tests-sequences nil))
     (advice-add 'end-kbd-macro :after #'kmacro-tests-end-macro-advice)
     (advice-add 'read-event :around #'kmacro-tests-read-event-advice )
     (advice-add 'read-key-sequence :around #'kmacro-tests-read-key-sequence-advice)
     (unwind-protect
         (ert-with-test-buffer (:name "")
           (switch-to-buffer (current-buffer))
           ,@body)
       (advice-remove 'read-key-sequence #'kmacro-tests-read-key-sequence-advice)
       (advice-remove 'read-event #'kmacro-tests-read-event-advice)
       (advice-remove 'end-kbd-macro #'kmacro-tests-end-macro-advice))))

(defmacro kmacro-tests-deftest (name _args docstring &rest keys-and-body)
  "Define a kmacro unit test.
NAME is the name of the test, _ARGS should be nil, and DOCSTRING
is required.  To avoid having to duplicate ert's keyword parsing
here, its keywords and values (if any) must be inside a list
after the docstring, preceding the body, here combined with the
body in KEYS-AND-BODY."
  (declare (debug (&define name sexp stringp
                           [&optional (&rest &or [keywordp sexp])]
                           def-body))
           (doc-string 3)
           (indent 2))

  (let* ((keys (when (and (listp (car keys-and-body))
                          (keywordp (caar keys-and-body)))
                 (car keys-and-body)))
         (body (if keys (cdr keys-and-body)
                 keys-and-body)))
    `(ert-deftest ,name ()
       ,docstring ,@keys
       (kmacro-tests-with-kmacro-clean-slate ,@body))))

(defvar kmacro-tests-keymap
  (let ((map (make-sparse-keymap)))
    (dotimes (i 26)
      (define-key map (string (+ ?a i)) 'self-insert-command))
    (dotimes (i 10)
      (define-key map (string (+ ?0 i)) 'self-insert-command))
    ;; Define a few key sequences of different lengths.
    (dolist (item '(("\C-a"     . beginning-of-line)
                    ("\C-b"     . backward-char)
                    ("\C-e"     . end-of-line)
                    ("\C-f"     . forward-char)
                    ("\C-r"     . isearch-backward)
                    ("\C-u"     . universal-argument)
                    ("\C-w"     . kill-region)
                    ("\C-SPC"   . set-mark-command)
                    ("\M-w"     . kill-ring-save)
                    ("\M-x"     . execute-extended-command)
                    ("\C-cd"    . downcase-word)
                    ("\C-cxu"   . upcase-word)
                    ("\C-cxq"   . quoted-insert)
                    ("\C-cxi"   . kmacro-insert-counter)
                    ("\C-x\C-k" . kmacro-keymap)))
      (define-key map (car item) (cdr item)))
    map)
  "Keymap to use for testing keyboard macros.
This is used to obtain consistent results even if tests are run
in an environment with rebound keys.")

(defvar kmacro-tests-events nil
  "Input events used by the kmacro test in progress.")

(defun kmacro-tests-read-event-advice (orig-func &rest args)
  "Pop and return an event from `kmacro-tests-events'.
Return the result of calling ORIG-FUNC with ARGS if
`kmacro-tests-events' is empty, or if a keyboard macro is
running."
  (if (or executing-kbd-macro (null kmacro-tests-events))
      (apply orig-func args)
    (pop kmacro-tests-events)))

(defvar kmacro-tests-sequences nil
  "Input sequences used by the kmacro test in progress.")

(defun kmacro-tests-read-key-sequence-advice (orig-func &rest args)
  "Pop and return a string from `kmacro-tests-sequences'.
Return the result of calling ORIG-FUNC with ARGS if
`kmacro-tests-sequences' is empty, or if a keyboard macro is
running."
  (if (or executing-kbd-macro (null kmacro-tests-sequences))
      (apply orig-func args)
    (pop kmacro-tests-sequences)))

(defvar kmacro-tests-macros nil
  "Keyboard macros (in vector form) used by the kmacro test in progress.")

(defun kmacro-tests-end-macro-advice (&rest _args)
  "Pop a macro from `kmacro-tests-macros' and assign it to `last-kbd-macro'.
If `kmacro-tests-macros' is empty, do nothing."
  (when kmacro-tests-macros
    (setq last-kbd-macro (pop kmacro-tests-macros))))

;;; Some more powerful expectations:

(defmacro kmacro-tests-should-insert (value &rest body)
  "Verify that VALUE is inserted by the execution of BODY.
Execute BODY, then check that the string VALUE was inserted
into the current buffer at point."
  (declare (debug (stringp body))
           (indent 1))
  (let ((g-p (cl-gensym))
        (g-bsize (cl-gensym)))
    `(let ((,g-p (point))
           (,g-bsize (buffer-size)))
       ,@body
       (should (equal (buffer-substring ,g-p (point)) ,value))
       (should (equal (- (buffer-size) ,g-bsize) (length ,value))))))

(defmacro kmacro-tests-should-match-message (value &rest body)
  "Verify that a message matching VALUE is issued while executing BODY.
Execute BODY, and then if there is not a regexp match between
VALUE and any text written to *Messages* during the execution,
cause the current test to fail."
  (declare (debug (form body))
           (indent 1))
  (let ((g-captured-messages (cl-gensym)))
    `(ert-with-message-capture ,g-captured-messages
       ,@body
       (should (string-match-p ,value ,g-captured-messages)))))

;;; Tests:

(kmacro-tests-deftest kmacro-tests-test-insert-counter-01-nil ()
  "`kmacro-insert-counter' adds one to macro counter with nil arg."
  (kmacro-tests-should-insert "0"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil)))
  (kmacro-tests-should-insert "1"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil))))

(kmacro-tests-deftest kmacro-tests-test-insert-counter-02-int ()
  "`kmacro-insert-counter' increments by value of list argument."
  (kmacro-tests-should-insert "0"
    (kmacro-tests-simulate-command '(kmacro-insert-counter 2)))
  (kmacro-tests-should-insert "2"
    (kmacro-tests-simulate-command '(kmacro-insert-counter 3)))
  (kmacro-tests-should-insert "5"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil))))

(kmacro-tests-deftest kmacro-tests-test-insert-counter-03-list ()
  "`kmacro-insert-counter' doesn't increment when given universal argument."
  (kmacro-tests-should-insert "0"
    (kmacro-tests-simulate-command '(kmacro-insert-counter (16))))
  (kmacro-tests-should-insert "0"
    (kmacro-tests-simulate-command '(kmacro-insert-counter (4)))))

(kmacro-tests-deftest kmacro-tests-test-insert-counter-04-neg ()
  "`kmacro-insert-counter' decrements with '- prefix argument"
  (kmacro-tests-should-insert "0"
    (kmacro-tests-simulate-command '(kmacro-insert-counter -)))
  (kmacro-tests-should-insert "-1"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil))))

(kmacro-tests-deftest kmacro-tests-test-start-format-counter ()
  "`kmacro-insert-counter' uses start value and format."
  (kmacro-tests-simulate-command '(kmacro-set-counter 10))
  (kmacro-tests-should-insert "10"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil)))
  (kmacro-tests-should-insert "11"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil)))
  (kmacro-set-format "c=%s")
  (kmacro-tests-simulate-command '(kmacro-set-counter 50))
  (kmacro-tests-should-insert "c=50"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil))))

(kmacro-tests-deftest kmacro-tests-test-start-macro-when-defining-macro ()
  "Starting a macro while defining a macro does not start a second macro."
  (kmacro-tests-simulate-command '(kmacro-start-macro nil))
  ;; We should now be in the macro-recording state.
  (should defining-kbd-macro)
  (should-not last-kbd-macro)
  ;; Calling it again should leave us in the same state.
  (kmacro-tests-simulate-command '(kmacro-start-macro nil))
  (should defining-kbd-macro)
  (should-not last-kbd-macro))


(kmacro-tests-deftest kmacro-tests-set-macro-counter-while-defining ()
  "Use of the prefix arg with kmacro-start sets kmacro-counter."
  ;; Give kmacro-start-macro an argument.
  (kmacro-tests-simulate-command '(kmacro-start-macro 5))
  (should defining-kbd-macro)
  ;; Verify that the counter is set to that value.
  (kmacro-tests-should-insert "5"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil)))
  ;; Change it while defining a macro.
  (kmacro-tests-simulate-command '(kmacro-set-counter 1))
  (kmacro-tests-should-insert "1"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil)))
  ;; Using universal arg to set counter should reset to starting value.
  (kmacro-tests-simulate-command '(kmacro-set-counter (4)) '(4))
  (kmacro-tests-should-insert "5"
    (kmacro-tests-simulate-command '(kmacro-insert-counter nil))))


(kmacro-tests-deftest kmacro-tests-start-insert-counter-appends-to-macro ()
  "Use of the universal arg appends to the previous macro."
  (let ((kmacro-tests-macros (list (string-to-vector "hello"))))
    ;; Start recording a macro.
    (kmacro-tests-simulate-command '(kmacro-start-macro-or-insert-counter nil))
    ;; Make sure we are recording.
    (should defining-kbd-macro)
    ;; Call it again and it should insert the counter.
    (kmacro-tests-should-insert "0"
      (kmacro-tests-simulate-command '(kmacro-start-macro-or-insert-counter nil)))
    ;; We should still be in the recording state.
    (should defining-kbd-macro)
    ;; End recording with repeat count.
    (kmacro-tests-simulate-command '(kmacro-end-or-call-macro 3))
    ;; Recording should be finished.
    (should-not defining-kbd-macro)
    ;; Now use prefix arg to append to the previous macro.
    ;; This should run the previous macro first.
    (kmacro-tests-should-insert "hello"
      (kmacro-tests-simulate-command
       '(kmacro-start-macro-or-insert-counter (4))))
    ;;  Verify that the recording state has changed.
    (should (equal defining-kbd-macro 'append))))

(kmacro-tests-deftest kmacro-tests-end-call-macro-prefix-args ()
  "kmacro-end-call-macro changes behavior based on prefix arg."
  ;; "Record" two macros.
  (dotimes (i 2)
    (kmacro-tests-define-macro (vconcat (format "macro #%d" (1+  i)))))
  ;; With no prefix arg, it should call the second macro.
  (kmacro-tests-should-insert "macro #2"
    (kmacro-tests-simulate-command '(kmacro-end-or-call-macro nil)))
  ;; With universal arg, it should call the first one.
  (kmacro-tests-should-insert "macro #1"
    (kmacro-tests-simulate-command '(kmacro-end-or-call-macro (4)))))

(kmacro-tests-deftest kmacro-tests-end-and-call-macro ()
  "Keyboard command to end and call macro works under various conditions."
  ;; First, try it with no macro to record.
  (setq kmacro-tests-macros '(""))
  (kmacro-tests-simulate-command '(kmacro-start-macro nil))
  (condition-case err
      (kmacro-tests-simulate-command '(kmacro-end-and-call-macro 2) 2)
    (error (should (string= (cadr err)
                            "No kbd macro has been defined"))))

  ;; Check that it stopped defining and that no macro was recorded.
  (should-not defining-kbd-macro)
  (should-not last-kbd-macro)

  ;; Now try it while not recording, but first record a non-nil macro.
  (kmacro-tests-define-macro "macro")
  (kmacro-tests-should-insert "macro"
    (kmacro-tests-simulate-command '(kmacro-end-and-call-macro nil))))

(kmacro-tests-deftest kmacro-tests-end-and-call-macro-mouse ()
  "Commands to end and call macro work under various conditions.
This is a regression test for Bug#24992."
  (:expected-result :failed)
  (cl-letf (((symbol-function #'mouse-set-point) #'ignore))
    ;; First, try it with no macro to record.
    (setq kmacro-tests-macros '(""))
    (kmacro-tests-simulate-command '(kmacro-start-macro nil))
    (condition-case err
        (kmacro-tests-simulate-command '(kmacro-end-call-mouse 2) 2)
      (error (should (string= (cadr err)
                              "No kbd macro has been defined"))))

    ;; Check that it stopped defining and that no macro was recorded.
    (should-not defining-kbd-macro)
    (should-not last-kbd-macro)

    ;; Now try it while not recording, but first record a non-nil macro.
    (kmacro-tests-define-macro "macro")
    (kmacro-tests-should-insert "macro"
      (kmacro-tests-simulate-command '(kmacro-end-call-mouse nil)))))

(kmacro-tests-deftest kmacro-tests-call-macro-hint-and-repeat ()
  "`kmacro-call-macro' gives hint in Messages and sets up repeat keymap.
This is a regression test for: Bug#3412, Bug#11817."
  (kmacro-tests-define-macro [?m])
  (let ((kmacro-call-repeat-key t)
        (kmacro-call-repeat-with-arg t)
        (overriding-terminal-local-map overriding-terminal-local-map)
        (last-input-event ?e))
    (message "")  ; Clear the echo area. (Bug#3412)
    (kmacro-tests-should-match-message "Type e to repeat macro"
      (kmacro-tests-should-insert "mmmmmm"
        (cl-letf (((symbol-function #'this-single-command-keys) (lambda ()
                                                                  [?\C-x ?e])))
          (kmacro-call-macro 3))
        ;; Check that it set up for repeat, and run the repeat.
        (funcall (lookup-key overriding-terminal-local-map "e"))))))

(kmacro-tests-deftest
    kmacro-tests-run-macro-command-recorded-in-macro ()
  "No infinite loop if `kmacro-end-and-call-macro' is recorded in the macro.
\(Bug#15126)"
  (:expected-result :failed)
  (ert-skip "Skipping due to Bug#24921 (an ERT bug)")
  (kmacro-tests-define-macro (vconcat "foo" [return] "\M-x"
                                      "kmacro-end-and-call-macro"))
  (use-local-map kmacro-tests-keymap)
  (kmacro-tests-simulate-command '(kmacro-end-and-call-macro nil)))


(kmacro-tests-deftest kmacro-tests-test-ring-2nd-commands ()
  "2nd macro in ring is displayed and executed normally and on repeat."
  (use-local-map kmacro-tests-keymap)
  ;; Record one macro, with count.
  (push (vconcat "\C-cxi" "\C-u\C-cxi") kmacro-tests-macros)
  (kmacro-tests-simulate-command '(kmacro-start-macro 1))
  (kmacro-tests-simulate-command '(kmacro-end-macro nil))
  ;; Check that execute and display do nothing with no 2nd macro.
  (kmacro-tests-should-insert ""
    (kmacro-tests-simulate-command '(kmacro-call-ring-2nd nil)))
  (kmacro-tests-should-match-message "Only one keyboard macro defined"
    (kmacro-tests-simulate-command '(kmacro-view-ring-2nd)))
  ;; Record another one, with format.
  (kmacro-set-format "=%d=")
  (kmacro-tests-define-macro (vconcat "bar"))
  ;; Execute the first one, mocked up to insert counter.
  ;; Should get default format.
  (kmacro-tests-should-insert "11"
    (kmacro-tests-simulate-command '(kmacro-call-ring-2nd nil)))
  ;; Now display the 2nd ring macro and check result.
  (kmacro-tests-should-match-message "C-c x i C-u C-c x i"
    (kmacro-view-ring-2nd)))

(kmacro-tests-deftest kmacro-tests-fill-ring-and-rotate ()
  "Macro ring can shift one way, shift the other way, swap and pop."
  (cl-letf ((kmacro-ring-max 4))
    ;; Record enough macros that the first one drops off the history.
    (dotimes (n (1+ kmacro-ring-max))
      (kmacro-tests-define-macro (make-vector (1+ n) (+ ?a n))))
    ;; Cycle the ring and check that #2 comes up.
    (kmacro-tests-should-match-message "2*b"
      (kmacro-tests-simulate-command '(kmacro-cycle-ring-next nil)))
    ;; Execute the current macro and check arguments.
    (kmacro-tests-should-insert "bbbb"
      (kmacro-call-macro 2 t))
    ;; Cycle the ring the other way; #5 expected.
    (kmacro-tests-should-match-message "5*e" (kmacro-cycle-ring-previous nil))
    ;; Swapping the top two should give #4.
    (kmacro-tests-should-match-message "4*d" (kmacro-swap-ring))
    ;; Delete the top and expect #5.
    (kmacro-tests-should-match-message "5*e" (kmacro-delete-ring-head))))


(kmacro-tests-deftest kmacro-tests-test-ring-commands-when-no-macros ()
  "Ring commands give appropriate message when no macros exist."
  (dolist (cmd '((kmacro-cycle-ring-next nil)
                 (kmacro-cycle-ring-previous nil)
                 (kmacro-swap-ring)
                 (kmacro-delete-ring-head)
                 (kmacro-view-ring-2nd)
                 (kmacro-call-ring-2nd nil)
                 (kmacro-view-macro)))
    (kmacro-tests-should-match-message "No keyboard macro defined"
      (kmacro-tests-simulate-command cmd))))

(kmacro-tests-deftest kmacro-tests-repeat-on-last-key ()
  "Kmacro commands can be run in sequence without prefix keys."
  (let* ((prefix (where-is-internal 'kmacro-keymap nil t))
         ;; Make a sequence of events to run.
         ;; Comments are expected output of mock macros
         ;; on the first and second run of the sequence (see below).
         (events (mapcar #'kmacro-tests-get-kmacro-key
                         '(kmacro-end-or-call-macro-repeat ;c / b
                           kmacro-end-or-call-macro-repeat ;c / b
                           kmacro-call-ring-2nd-repeat     ;b / a
                           kmacro-cycle-ring-next
                           kmacro-end-or-call-macro-repeat ;a / a
                           kmacro-cycle-ring-previous
                           kmacro-end-or-call-macro-repeat ;c / b
                           kmacro-delete-ring-head
                           kmacro-end-or-call-macro-repeat ;b / a
                           )))
         (kmacro-tests-macros (list [?a] [?b] [?c]))
         ;; What we want kmacro to see as keyboard command sequence
         (first-event (seq-concatenate
                       'vector
                       prefix
                       (vector (kmacro-tests-get-kmacro-key
                                'kmacro-end-or-call-macro-repeat)))))
    (cl-letf
        ;; standardize repeat options
        ((kmacro-repeat-no-prefix t)
         (kmacro-call-repeat-key t)
         (kmacro-call-repeat-with-arg nil))
      ;; "Record" two macros
      (dotimes (_n 2)
        (kmacro-tests-simulate-command '(kmacro-start-macro nil))
        (kmacro-tests-simulate-command '(kmacro-end-macro nil)))
      ;; Start recording #3
      (kmacro-tests-simulate-command '(kmacro-start-macro nil))

      ;; Set up pending keyboard events and a fresh buffer
      ;; kmacro-set-counter is not one of the repeating kmacro
      ;; commands so it should end the sequence.
      (let* ((end-key (kmacro-tests-get-kmacro-key 'kmacro-set-counter))
             (kmacro-tests-events (append events (list end-key))))
        (cl-letf (((symbol-function #'this-single-command-keys)
                   (lambda () first-event)))
          (use-local-map kmacro-tests-keymap)
          (kmacro-tests-should-insert "ccbacb"
            ;; End #3 and launch loop to read events.
            (kmacro-end-or-call-macro-repeat nil))))

      ;; `kmacro-edit-macro-repeat' should also stop the sequence,
      ;; so run it again with that at the end.
      (let* ((end-key (kmacro-tests-get-kmacro-key 'kmacro-edit-macro-repeat))
             (kmacro-tests-events (append events (list end-key))))
        (cl-letf (((symbol-function #'edit-kbd-macro) #'ignore)
                  ((symbol-function #'this-single-command-keys)
                   (lambda () first-event)))
          (use-local-map kmacro-tests-keymap)
          (kmacro-tests-should-insert "bbbbbaaba"
            (kmacro-end-or-call-macro-repeat 3)))))))

(kmacro-tests-deftest kmacro-tests-repeat-view-and-run ()
  "Kmacro view cycles through ring and executes macro just viewed."
  (let* ((prefix (where-is-internal 'kmacro-keymap nil t))
         (kmacro-tests-events
          (mapcar #'kmacro-tests-get-kmacro-key
                  (append (make-list 5 'kmacro-view-macro-repeat)
                          '(kmacro-end-or-call-macro-repeat
                            kmacro-set-counter))))
         ;; Make kmacro see this as keyboard command sequence.
         (first-event (seq-concatenate
                       'vector
                       prefix
                       (vector (kmacro-tests-get-kmacro-key
                                'kmacro-view-macro-repeat))))
         ;; Construct a regexp to match the messages which should be
         ;; produced by repeated view-repeats.
         (macros-regexp (apply #'concat
                               (mapcar (lambda (c) (format ".+%s\n" c))
                                       '("d" "c" "b" "a" "d" "c")))))
    (cl-letf ((kmacro-repeat-no-prefix t)
              (kmacro-call-repeat-key t)
              (kmacro-call-repeat-with-arg nil)
              ((symbol-function #'this-single-command-keys) (lambda ()
                                                              first-event)))
      ;; "Record" some macros.
      (dotimes (n 4)
        (kmacro-tests-define-macro (make-vector 1 (+ ?a n))))

      (use-local-map kmacro-tests-keymap)
      ;; 6 views (the direct call plus the 5 in events) should
      ;; cycle through the ring and get to the second-to-last
      ;; macro defined.
      (kmacro-tests-should-insert "c"
        (kmacro-tests-should-match-message macros-regexp
          (kmacro-tests-simulate-command '(kmacro-view-macro-repeat nil)))))))

(kmacro-tests-deftest kmacro-tests-bind-to-key-when-recording ()
  "Bind to key doesn't bind a key during macro recording."
  (cl-letf ((global-map global-map)
            (saved-binding (key-binding "\C-a"))
            (kmacro-tests-sequences (list "\C-a")))
    (kmacro-tests-simulate-command '(kmacro-start-macro 1))
    (kmacro-bind-to-key nil)
    (should (eq saved-binding (key-binding "\C-a")))))

(kmacro-tests-deftest kmacro-tests-name-or-bind-to-key-when-no-macro ()
  "Bind to key, symbol or register fails when when no macro exists."
  (should-error (kmacro-bind-to-key nil))
  (should-error (kmacro-name-last-macro 'kmacro-tests-symbol-for-test))
  (should-error (kmacro-to-register)))

(kmacro-tests-deftest kmacro-tests-bind-to-key-bad-key-sequence ()
  "Bind to key fails to bind to ^G."
  (let ((global-map global-map)
        (saved-binding (key-binding "\C-g"))
        (kmacro-tests-sequences (list "\C-g")))
    (kmacro-tests-define-macro [1])
    (kmacro-bind-to-key nil)
    (should (eq saved-binding (key-binding "\C-g")))))

(kmacro-tests-deftest kmacro-tests-bind-to-key-with-key-sequence-in-use ()
  "Bind to key respects yes-or-no-p when given already bound key sequence."
  (kmacro-tests-define-macro (vconcat "abaab"))
  (let ((global-map global-map)
        (map (make-sparse-keymap))
        (kmacro-tests-sequences (make-list 2 "\C-hi")))
    (define-key map "\C-hi" 'info)
    (use-local-map map)
    ;; Try the command with yes-or-no-p set up to say no.
    (cl-letf (((symbol-function #'yes-or-no-p)
               (lambda (prompt)
                 (should (string-match-p "info" prompt))
                 (should (string-match-p "C-h i" prompt))
                 nil)))
      (kmacro-bind-to-key nil))

    (should (equal (where-is-internal 'info nil t)
                   (vconcat "\C-hi")))
    ;; Try it again with yes.
    (cl-letf (((symbol-function #' yes-or-no-p)
               (lambda (_prompt) t)))
      (kmacro-bind-to-key nil))

    (should-not (equal (where-is-internal 'info global-map t)
                       (vconcat "\C-hi")))
    (use-local-map nil)
    (kmacro-tests-should-insert "abaab"
      (funcall (key-binding "\C-hi")))))

(kmacro-tests-deftest kmacro-tests-kmacro-bind-to-single-key ()
  "Bind to key uses C-x C-k A when asked to bind to A."
  (let ((global-map global-map)
        (kmacro-tests-macros (list (string-to-vector "\C-cxi"))))
    (use-local-map kmacro-tests-keymap)

    ;; Record a macro with counter and format set.
    (kmacro-set-format "<%d>")
    (kmacro-tests-simulate-command '(kmacro-start-macro-or-insert-counter 5))
    (kmacro-tests-simulate-command '(kmacro-end-macro nil))

    (let ((kmacro-tests-sequences (list "A")))
      (kmacro-bind-to-key nil))

    ;; Record a second macro with different counter and format.
    (kmacro-set-format "%d")
    (kmacro-tests-define-macro [2])

    ;; Check the bound key and run it and verify correct counter
    ;; and format.
    (should (equal (string-to-vector "\C-cxi")
                   (car (kmacro-extract-lambda
                         (key-binding "\C-x\C-kA")))))
    (kmacro-tests-should-insert "<5>"
      (funcall (key-binding "\C-x\C-kA")))))

(kmacro-tests-deftest kmacro-tests-name-last-macro-unable-to-bind ()
  "Name last macro won't bind to symbol which is already bound."
  (kmacro-tests-define-macro [1])
  ;; Set up a test symbol which looks like a function.
  (setplist 'kmacro-tests-symbol-for-test nil)
  (fset 'kmacro-tests-symbol-for-test #'ignore)
  (should-error (kmacro-name-last-macro 'kmacro-tests-symbol-for-test))
  ;; The empty string symbol also can't be bound.
  (should-error (kmacro-name-last-macro (make-symbol ""))))

(kmacro-tests-deftest kmacro-tests-name-last-macro-bind-and-rebind ()
  "Name last macro can rebind a symbol it binds."
  ;; Make sure our symbol is unbound.
  (when (fboundp 'kmacro-tests-symbol-for-test)
    (fmakunbound 'kmacro-tests-symbol-for-test))
  (setplist 'kmacro-tests-symbol-for-test nil)
  ;; Make two macros and bind them to the same symbol.
  (dotimes (i 2)
    (kmacro-tests-define-macro (make-vector (1+ i) (+ ?a i)))
    (kmacro-name-last-macro 'kmacro-tests-symbol-for-test)
    (should (fboundp 'kmacro-tests-symbol-for-test)))

  ;; Now run the function bound to the symbol. Result should be the
  ;; second macro.
  (kmacro-tests-should-insert "bb"
    (kmacro-tests-simulate-command '(kmacro-tests-symbol-for-test))))

(kmacro-tests-deftest kmacro-tests-store-in-register ()
  "Macro can be stored in and retrieved from a register."
  (use-local-map kmacro-tests-keymap)
  ;; Save and restore register 200 so we can use it for the test.
  (let ((saved-reg-contents (get-register 200)))
    (unwind-protect
        (progn
          ;; Define a macro, and save it to a register.
          (kmacro-tests-define-macro (vconcat "a\C-a\C-cxu"))
          (kmacro-to-register 200)
          ;; Then make a new different macro.
          (kmacro-tests-define-macro (vconcat "bb\C-a\C-cxu"))
          ;; When called from the register, result should be first macro.
          (kmacro-tests-should-insert "AAA"
            (kmacro-tests-simulate-command '(jump-to-register 200 3) 3))
          (kmacro-tests-should-insert "a C-a C-c x u"
            (kmacro-tests-simulate-command '(insert-register 200 t) '(4))))
      (set-register 200 saved-reg-contents))))

(kmacro-tests-deftest kmacro-tests-step-edit-act ()
  "Step-edit steps-through a macro with act and act-repeat."
  (kmacro-tests-run-step-edit "he\C-u2lo"
                              :events (make-list 6 'act)
                              :result "hello"
                              :macro-result "he\C-u2lo")

  (kmacro-tests-run-step-edit "f\C-aoo\C-abar"
                              :events (make-list 5 'act-repeat)
                              :result "baroof"
                              :macro-result "f\C-aoo\C-abar"))

(kmacro-tests-deftest kmacro-tests-step-edit-skip ()
  "Step-editing can skip parts of macro."
  (kmacro-tests-run-step-edit "ofoofff"
                              :events '(skip skip-keep skip-keep skip-keep
                                             skip-rest)
                              :result ""
                              :macro-result "foo"))

(kmacro-tests-deftest kmacro-tests-step-edit-quit ()
  "Quit while step-editing leaves macro unchanged."
  (kmacro-tests-run-step-edit "bar"
                              :events '(help insert skip help quit)
                              :sequences '("f" "o" "o" "\C-j")
                              :result "foo"
                              :macro-result "bar"))

(kmacro-tests-deftest kmacro-tests-step-insert ()
  "Step edit can insert in macro."
  (kmacro-tests-run-step-edit "fbazbop"
                              :events '(insert act insert-1 act-repeat)
                              :sequences '("o" "o" "\C-a" "\C-j" "\C-e")
                              :result "foobazbop"
                              :macro-result "oo\C-af\C-ebazbop"))

(kmacro-tests-deftest kmacro-tests-step-edit-replace-digit-argument ()
  "Step-edit replace can replace a numeric argument in a macro.
This is a regression for item 1 in Bug#24991."
  (:expected-result :failed)
  (kmacro-tests-run-step-edit "\C-u3b\C-a\C-cxu"
                              :events '(act replace automatic)
                              :sequences '("8" "x" "\C-j")
                              :result "XXXXXXXX"
                              :macro-result "\C-u8x\C-a\C-cxu"))

(kmacro-tests-deftest kmacro-tests-step-edit-replace ()
  "Step-edit replace and replace-1 can replace parts of a macro."
  (kmacro-tests-run-step-edit "a\C-a\C-cxu"
                              :events '(act act replace)
                              :sequences '("b" "c" "\C-j")
                              :result "bca"
                              :macro-result "a\C-abc")
  (kmacro-tests-run-step-edit "a\C-a\C-cxucd"
                              :events '(act replace-1 automatic)
                              :sequences '("b")
                              :result "abcd"
                              :macro-result "ab\C-cxucd")
  (kmacro-tests-run-step-edit "by"
                              :events '(act replace)
                              :sequences '("a" "r" "\C-j")
                              :result "bar"
                              :macro-result "bar"))

(kmacro-tests-deftest kmacro-tests-step-edit-append ()
  "Step edit append inserts after point, and append-end inserts at end."
  (kmacro-tests-run-step-edit "f-b"
                              :events '(append append-end)
                              :sequences '("o" "o" "\C-j" "a" "r" "\C-j")
                              :result "foo-bar"
                              :macro-result "foo-bar")
  (kmacro-tests-run-step-edit "x"
                              :events '(append)
                              :sequences '("\C-a" "\C-cxu" "\C-e" "y" "\C-j")
                              :result "Xy"
                              :macro-result "x\C-a\C-cxu\C-ey"))

(kmacro-tests-deftest kmacro-tests-append-end-at-end-appends ()
  "Append-end when already at end of macro appends to end of macro.
This is a regression for item 2 in Bug#24991."
  (:expected-result :failed)
  (kmacro-tests-run-step-edit "x"
                              :events '(append-end)
                              :sequences '("\C-a" "\C-cxu" "\C-e" "y" "\C-j")
                              :result "Xy"
                              :macro-result "x\C-a\C-cxu\C-ey"))


(kmacro-tests-deftest kmacro-tests-step-edit-skip-entire ()
  "Skipping a whole macro in step-edit leaves macro unchanged.
This is a regression for item 3 in Bug#24991."
  (:expected-result :failed)
  (kmacro-tests-run-step-edit "xyzzy"
                              :events '(skip-rest)
                              :result ""
                              :macro-result "xyzzy"))

(kmacro-tests-deftest kmacro-tests-step-edit-step-through-negative-argument ()
  "Step edit works on macros using negative universal argument.
This is a regression for item 4 in Bug#24991."
  (:expected-result :failed)
  (kmacro-tests-run-step-edit "boo\C-u-\C-cu"
                              :events '(act-repeat automatic)
                              :result "BOO"
                              :macro-result "boo\C-u-\C-cd"))

(kmacro-tests-deftest kmacro-tests-step-edit-with-quoted-insert ()
  "Stepping through a macro that uses quoted insert leaves macro unchanged.
This is a regression for item 5 in Bug#24991."
  (:expected-result :failed)
  (let ((read-quoted-char-radix 8))
    (kmacro-tests-run-step-edit "\C-cxq17051i there"
                                :events '(act automatic)
                                :result "ḩi there"
                                :macro-result "\C-cxq17051i there")
    (kmacro-tests-run-step-edit "g\C-cxq17051i"
                                :events '(act insert-1 automatic)
                                :sequences '("-")
                                :result "g-ḩi"
                                :macro-result "g-\C-cxq17051i")))

(kmacro-tests-deftest kmacro-tests-step-edit-can-replace-meta-keys ()
  "Replacing C-w with M-w produces the expected result.
This is a regression for item 7 in Bug#24991."
  (:expected-result :failed)
  (kmacro-tests-run-step-edit "abc\C-b\C-b\C-SPC\C-f\C-w\C-e\C-y"
                              :events '(act-repeat act-repeat
                                                   act-repeat act-repeat
                                                   replace automatic)
                              :sequences '("\M-w" "\C-j")
                              :result "abcb"
                              :macro-result "abc\C-b\C-b\C-SPC\C-f\M-w\C-e\C-y")
  (kmacro-tests-should-insert "abcb" (kmacro-call-macro nil)))

(kmacro-tests-deftest kmacro-tests-step-edit-ignores-qr-map-commands ()
  "Unimplemented commands from `query-replace-map' are ignored."
  (kmacro-tests-run-step-edit "yep"
                              :events '(edit-replacement
                                        act-and-show act-and-exit
                                        delete-and-edit
                                        recenter backup
                                        scroll-up scroll-down
                                        scroll-other-window
                                        scroll-other-window-down
                                        exit-prefix
                                        act act act)
                              :result "yep"
                              :macro-result "yep"))

(kmacro-tests-deftest
    kmacro-tests-step-edit-edits-macro-with-extended-command ()
  "Step-editing a macro which uses the minibuffer can change the macro."
  (let ((mac (vconcat [?\M-x] "eval-expression" '[return]
                      "(insert-char (+ ?a \C-e" [?1] "))" '[return]))
        (mac-after (vconcat [?\M-x] "eval-expression" '[return]
                            "(insert-char (+ ?a \C-e" [?2] "))" '[return])))

    (kmacro-tests-run-step-edit mac
                                :events '(act act-repeat
                                              act act-repeat act
                                              replace-1 act-repeat act)
                                :sequences '("2")
                                :result "c"
                                :macro-result mac-after)))

(kmacro-tests-deftest kmacro-tests-step-edit-step-through-isearch ()
  "Step-editing can edit a macro which uses `isearch-backward' (Bug#22488)."
  (:expected-result :failed)
  (let ((mac (vconcat "test Input" '[return]
                      [?\C-r] "inp" '[return] "\C-cxu"))
        (mac-after (vconcat "test input" '[return]
                            [?\C-r] "inp" '[return] "\C-cd")))

    (kmacro-tests-run-step-edit mac
                                :events '(act-repeat act act
                                                     act-repeat act
                                                     replace-1)
                                :sequences '("\C-cd")
                                :result "test input\n"
                                :macro-result mac-after)))

(kmacro-tests-deftest kmacro-tests-step-edit-cleans-up-hook ()
  "Step-editing properly cleans up `post-command-hook.' (Bug #18708)"
  (let (post-command-hook)
    (setq-local post-command-hook '(t))
    (kmacro-tests-run-step-edit "x"
                                :events '(act)
                                :result "x"
                                :macro-result "x")
    (kmacro-tests-simulate-command '(beginning-of-line))))

(cl-defun kmacro-tests-run-step-edit
    (macro &key events sequences result macro-result)
  "Set up and run a test of `kmacro-step-edit-macro'.

Run `kmacro-step-edit-macro' with MACRO defined as a keyboard macro
and `read-event' and `read-key-sequence' set up to return items from
EVENTS and SEQUENCES respectively.  SEQUENCES may be nil, but
EVENTS should not be.  EVENTS should be a list of symbols bound
in `kmacro-step-edit-map' or `query-replace' map, and this function
will do the keymap lookup for you. SEQUENCES should contain
return values for `read-key-sequence'.

Before running the macro, the current buffer will be erased.
RESULT is the string that should be inserted during the
step-editing process, and MACRO-RESULT is the expected value of
`last-kbd-macro' after the editing is complete."

  (let* ((kmacro-tests-events (mapcar #'kmacro-tests-get-kmacro-step-edit-key events))
         (kmacro-tests-sequences sequences))

    (kmacro-tests-define-macro (string-to-vector macro))
    (use-local-map kmacro-tests-keymap)
    (erase-buffer)
    (kmacro-step-edit-macro)
    (when result
      (should (equal result (buffer-string))))
    (when macro-result
      (should (equal last-kbd-macro (string-to-vector macro-result))))))

;;; Utilities:

(defun kmacro-tests-simulate-command (command &optional arg)
  "Call `ert-simulate-command' after setting `current-prefix-arg'.
Sets `current-prefix-arg' to ARG if it is non-nil, otherwise to
the second element of COMMAND, before executing COMMAND using
`ert-simulate-command'."
  (let ((current-prefix-arg (or arg (cadr command))))
    (ert-simulate-command command)))

(defun kmacro-tests-define-macro (mac)
  "Define MAC as a keyboard macro using kmacro commands."
  (push mac kmacro-tests-macros)
  (kmacro-tests-simulate-command '(kmacro-start-macro nil))
  (should defining-kbd-macro)
  (kmacro-tests-simulate-command '(kmacro-end-macro nil))
  (should (equal mac last-kbd-macro)))

(defun kmacro-tests-get-kmacro-key (sym)
  "Look up kmacro command SYM in kmacro's keymap.
Return the integer key value found."
  (aref (where-is-internal sym kmacro-keymap t) 0))

(defun kmacro-tests-get-kmacro-step-edit-key (sym)
  "Return the first key bound to SYM in `kmacro-step-edit-map'."
  (let ((where (aref (where-is-internal sym kmacro-step-edit-map t) 0)))
    (if (consp where)
        (car where)
      where)))

(provide 'kmacro-tests)

;;; kmacro-tests.el ends here
