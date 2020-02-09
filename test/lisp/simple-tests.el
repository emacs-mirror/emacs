;;; simple-test.el --- Tests for simple.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(eval-when-compile (require 'cl-lib))

(defun simple-test--buffer-substrings ()
  "Return cons of buffer substrings before and after point."
  (cons (buffer-substring (point-min) (point))
        (buffer-substring (point) (point-max))))

(defmacro simple-test--dummy-buffer (&rest body)
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (setq indent-tabs-mode nil)
     (insert "(a b")
     (save-excursion (insert " c d)"))
     ,@body
     (with-no-warnings (simple-test--buffer-substrings))))


;;; `transpose-sexps'
(defmacro simple-test--transpositions (&rest body)
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert "(s1) (s2) (s3) (s4) (s5)")
     (backward-sexp 1)
     ,@body
     (simple-test--buffer-substrings)))

;;; Transposition with negative args (bug#20698, bug#21885)
(ert-deftest simple-transpose-subr ()
  (should (equal (simple-test--transpositions (transpose-sexps -1))
                 '("(s1) (s2) (s4)" . " (s3) (s5)")))
  (should (equal (simple-test--transpositions (transpose-sexps -2))
                 '("(s1) (s4)" . " (s2) (s3) (s5)"))))


;;; `newline'
(ert-deftest newline ()
  (should-error (newline -1))
  (should (equal (simple-test--dummy-buffer (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'newline))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer (newline 5))
                 '("(a b\n\n\n\n\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (newline 1))
                 '("(a b \n" . "c d)"))))

(ert-deftest newline-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1))
                 '("(a b\n" . " c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'newline)
                     (call-interactively #'newline)))
                 '("(a b\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (newline 5 'interactive))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'newline)))
                 '("(a b\n\n\n\n\n   " . "c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (newline 1 'interactive))
                 '("(a b\n   " . "c d)"))))


;;; `open-line'
(ert-deftest open-line ()
  (should-error (open-line -1))
  (should-error (open-line))
  (should (equal (simple-test--dummy-buffer (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-mode -1)
                   (call-interactively #'open-line))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer (open-line 5))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (open-line 1))
                 '("(a b " . "\nc d)"))))

(ert-deftest open-line-margin-and-prefix ()
  (should (equal (simple-test--dummy-buffer
                   (let ((left-margin 10))
                     (open-line 3)))
                 '("(a b" . "\n\n\n          c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((left-margin 2))
                     (open-line 1)))
                 '("  " . "\n  (a b c d)")))
  (should (equal (simple-test--dummy-buffer
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-line 0)
                   (let ((fill-prefix "- - "))
                     (open-line 1)))
                 '("- - " . "\n(a b c d)"))))

;; For a while, from 24 Oct - 21 Nov 2015, `open-line' in the Emacs
;; development tree became sensitive to `electric-indent-mode', which
;; it had not been before.  This sensitivity was reverted for the
;; Emacs 25 release, so it could be discussed further (see thread
;; "Questioning the new behavior of `open-line'." on the Emacs Devel
;; mailing list, and bug #21884).
(ert-deftest open-line-indent ()
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b" . "\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg nil))
                     (call-interactively #'open-line)
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (open-line 5))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (electric-indent-local-mode 1)
                   (let ((current-prefix-arg 5))
                     (call-interactively #'open-line)))
                 '("(a b" . "\n\n\n\n\n c d)")))
  (should (equal (simple-test--dummy-buffer
                   (forward-char 1)
                   (electric-indent-local-mode 1)
                   (open-line 1))
                 '("(a b " . "\nc d)"))))

;; From 24 Oct - 21 Nov 2015, `open-line' took a second argument
;; INTERACTIVE and ran `post-self-insert-hook' if the argument was
;; true.  This test tested that.  Currently, however, `open-line'
;; does not run `post-self-insert-hook' at all, so for now
;; this test just makes sure that it doesn't.
(ert-deftest open-line-hook ()
  (let* ((x 0)
         (inc (lambda () (setq x (1+ x)))))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1))
    (should (= x 0))
    (simple-test--dummy-buffer
      (add-hook 'post-self-insert-hook inc nil 'local)
      (open-line 1))
    (should (= x 0))

    (unwind-protect
        (progn
          (add-hook 'post-self-insert-hook inc)
          (simple-test--dummy-buffer
            (open-line 1))
          (should (= x 0))
          (simple-test--dummy-buffer
            (open-line 10))
          (should (= x 0)))
      (remove-hook 'post-self-insert-hook inc))))


;;; `delete-indentation'

(ert-deftest simple-delete-indentation-no-region ()
  "Test `delete-indentation' when no mark is set; see bug#35021."
  (with-temp-buffer
    (insert " first \n second \n third \n fourth ")
    (should-not (mark t))
    ;; Without prefix argument.
    (should-not (call-interactively #'delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '(" first \n second \n third" . " fourth ")))
    (should-not (call-interactively #'delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '(" first \n second" . " third fourth ")))
    ;; With prefix argument.
    (goto-char (point-min))
    (let ((current-prefix-arg '(4)))
      (should-not (call-interactively #'delete-indentation)))
    (should (equal (simple-test--buffer-substrings)
                   '(" first" . " second third fourth ")))))

(ert-deftest simple-delete-indentation-inactive-region ()
  "Test `delete-indentation'  with an inactive region."
  (with-temp-buffer
    (insert " first \n second \n third ")
    (set-marker (mark-marker) (point-min))
    (should (mark t))
    (should-not (call-interactively #'delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '(" first \n second" . " third ")))))

(ert-deftest simple-delete-indentation-blank-line ()
  "Test `delete-indentation' does not skip blank lines.
See bug#35036."
  (with-temp-buffer
    (insert "\n\n third \n \n \n sixth \n\n")
    ;; Without prefix argument.
    (should-not (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("\n\n third \n \n \n sixth \n" . "")))
    (should-not (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("\n\n third \n \n \n sixth" . "")))
    (should-not (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("\n\n third \n \n" . "sixth")))
    ;; With prefix argument.
    (goto-char (point-min))
    (should-not (delete-indentation t))
    (should (equal (simple-test--buffer-substrings)
                   '("" . "\n third \n \nsixth")))
    (should-not (delete-indentation t))
    (should (equal (simple-test--buffer-substrings)
                   '("" . "third \n \nsixth")))
    (should-not (delete-indentation t))
    (should (equal (simple-test--buffer-substrings)
                   '("third" . "\nsixth")))
    (should-not (delete-indentation t))
    (should (equal (simple-test--buffer-substrings)
                   '("third" . " sixth")))))

(ert-deftest simple-delete-indentation-boundaries ()
  "Test `delete-indentation' motion at buffer boundaries."
  (with-temp-buffer
    (insert " first \n second \n third ")
    ;; Stay at EOB.
    (should-not (delete-indentation t))
    (should (equal (simple-test--buffer-substrings)
                   '(" first \n second \n third " . "")))
    ;; Stay at BOB.
    (forward-line -1)
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (should-not (delete-indentation))
      (should (equal (simple-test--buffer-substrings)
                     '("" . " second ")))
      ;; Go to EOB.
      (should-not (delete-indentation t))
      (should (equal (simple-test--buffer-substrings)
                     '(" second " . ""))))
    ;; Go to BOB.
    (end-of-line 0)
    (should-not (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("" . " first \n second \n third ")))))

(ert-deftest simple-delete-indentation-region ()
  "Test `delete-indentation' with an active region."
  (with-temp-buffer
    ;; Empty region.
    (insert " first ")
    (should-not (delete-indentation nil (point) (point)))
    (should (equal (simple-test--buffer-substrings)
                   '(" first " . "")))
    ;; Single line.
    (should-not (delete-indentation
                 nil (line-beginning-position) (1- (point))))
    (should (equal (simple-test--buffer-substrings)
                   '("" . " first ")))
    (should-not (delete-indentation nil (1+ (point)) (line-end-position)))
    (should (equal (simple-test--buffer-substrings)
                   '(" " . "first ")))
    (should-not (delete-indentation
                 nil (line-beginning-position) (line-end-position)))
    (should (equal (simple-test--buffer-substrings)
                   '("" . " first ")))
    ;; Multiple lines.
    (goto-char (point-max))
    (insert "\n second \n third \n fourth ")
    (goto-char (point-min))
    (should-not (delete-indentation
                 nil (line-end-position) (line-beginning-position 2)))
    (should (equal (simple-test--buffer-substrings)
                   '(" first" . " second \n third \n fourth ")))
    (should-not (delete-indentation
                 nil (point) (1+ (line-beginning-position 2))))
    (should (equal (simple-test--buffer-substrings)
                   '(" first second" . " third \n fourth ")))
    ;; Prefix argument overrides region.
    (should-not (delete-indentation t (point-min) (point)))
    (should (equal (simple-test--buffer-substrings)
                   '(" first second third" . " fourth ")))))

(ert-deftest simple-delete-indentation-prefix ()
  "Test `delete-indentation' with a fill prefix."
  (with-temp-buffer
    (insert "> first \n> second \n> third \n> fourth ")
    (let ((fill-prefix ""))
      (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("> first \n> second \n> third" . " > fourth ")))
    (let ((fill-prefix "<"))
      (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("> first \n> second" . " > third > fourth ")))
    (let ((fill-prefix ">"))
      (delete-indentation))
    (should (equal (simple-test--buffer-substrings)
                   '("> first" . " second > third > fourth ")))))


;;; `delete-trailing-whitespace'
(ert-deftest simple-delete-trailing-whitespace--bug-21766 ()
  "Test bug#21766: delete-whitespace sometimes deletes non-whitespace."
  (defvar python-indent-guess-indent-offset)  ; to avoid a warning
  (let ((python (featurep 'python))
        (python-indent-guess-indent-offset nil)
        (delete-trailing-lines t))
    (unwind-protect
        (with-temp-buffer
          (python-mode)
          (insert (concat "query = \"\"\"WITH filtered AS \n"
                          "WHERE      \n"
                          "\"\"\".format(fv_)\n"
                          "\n"
                          "\n"))
          (delete-trailing-whitespace)
          (should (string-equal (buffer-string)
                                (concat "query = \"\"\"WITH filtered AS\n"
                                        "WHERE\n"
                                        "\"\"\".format(fv_)\n"))))
      ;; Let's clean up if running interactive
      (unless (or noninteractive python)
        (unload-feature 'python)))))

(ert-deftest simple-delete-trailing-whitespace--formfeeds ()
  "Test formfeeds are not deleted but whitespace past them is."
  (with-temp-buffer
    (with-syntax-table (make-syntax-table)
      (modify-syntax-entry ?\f " ")     ; Make sure \f is whitespace
      (insert " \f \n \f \f \n\nlast\n")
      (delete-trailing-whitespace)
      (should (string-equal (buffer-string) " \f\n \f \f\n\nlast\n"))
      (should (equal ?\s (char-syntax ?\f)))
      (should (equal ?\s (char-syntax ?\n))))))


;;; undo tests

(defun simple-tests--exec (cmds)
  (dolist (cmd cmds)
    (setq last-command this-command)
    (setq this-command cmd)
    (run-hooks 'pre-command-hook)
    (command-execute cmd)
    (run-hooks 'post-command-hook)
    (undo-boundary)))

(ert-deftest simple-tests--undo ()
  (with-temp-buffer
    (buffer-enable-undo)
    (dolist (x '("a" "b" "c" "d" "e"))
      (insert x)
      (undo-boundary))
    (should (equal (buffer-string) "abcde"))
    (simple-tests--exec '(undo undo))
    (should (equal (buffer-string) "abc"))
    (simple-tests--exec '(backward-char undo))
    (should (equal (buffer-string) "abcd"))
    (simple-tests--exec '(undo))
    (should (equal (buffer-string) "abcde"))
    (simple-tests--exec '(backward-char undo undo))
    (should (equal (buffer-string) "abc"))
    (simple-tests--exec '(backward-char undo-redo))
    (should (equal (buffer-string) "abcd"))
    (simple-tests--exec '(undo))
    (should (equal (buffer-string) "abc"))
    (simple-tests--exec '(backward-char undo-redo undo-redo))
    (should (equal (buffer-string) "abcde"))
    (simple-tests--exec '(undo undo))
    (should (equal (buffer-string) "abc"))
    (simple-tests--exec '(backward-char undo-only undo-only))
    (should (equal (buffer-string) "a"))
    (simple-tests--exec '(backward-char undo-redo undo-redo))
    (should (equal (buffer-string) "abc"))
    (simple-tests--exec '(backward-char undo-redo undo-redo))
    (should (equal (buffer-string) "abcde"))
    ))

;;; undo auto-boundary tests
(ert-deftest undo-auto-boundary-timer ()
  (should
   undo-auto-current-boundary-timer))

(ert-deftest undo-auto--boundaries-added ()
  ;; The change in the buffer should have caused addition
  ;; to undo-auto--undoably-changed-buffers.
  (should
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (member (current-buffer) undo-auto--undoably-changed-buffers)))
  ;; The head of buffer-undo-list should be the insertion event, and
  ;; therefore not nil
  (should
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (car buffer-undo-list)))
  ;; Now the head of the buffer-undo-list should be a boundary and so
  ;; nil. We have to call auto-boundary explicitly because we are out
  ;; of the command loop
  (should-not
   (with-temp-buffer
     (setq buffer-undo-list nil)
     (insert "hello")
     (undo-auto--boundaries 'test))))

;; Test for a regression introduced by undo-auto--boundaries changes.
;; https://lists.gnu.org/r/emacs-devel/2015-11/msg01652.html
(defun undo-test-kill-c-a-then-undo ()
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (setq buffer-undo-list nil)
    (insert "a\nb\nc\n")
    (goto-char (point-max))
    ;; We use a keyboard macro because it adds undo events in the same
    ;; way as if a user were involved.
    (kmacro-call-macro nil nil nil
                       [left
                        ;; Delete "c"
                        backspace
                        left left left
                        ;; Delete "a"
                        backspace
                        ;; C-/ or undo
                        67108911
                        ])
    (point)))

(defun undo-test-point-after-forward-kill ()
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (setq buffer-undo-list nil)
    (insert "kill word forward")
    ;; Move to word "word".
    (goto-char 6)
    (kmacro-call-macro nil nil nil
                       [
                        ;; kill-word
                        C-delete
                        ;; undo
                        67108911
                        ])
    (point)))

(ert-deftest undo-point-in-wrong-place ()
  (should
   ;; returns 5 with the bug
   (= 2
      (undo-test-kill-c-a-then-undo)))
  (should
   (= 6
      (undo-test-point-after-forward-kill))))

(defmacro simple-test-undo-with-switched-buffer (buffer &rest body)
  (declare (indent 1) (debug t))
  (let ((before-buffer (make-symbol "before-buffer")))
    `(let ((,before-buffer (current-buffer)))
       (unwind-protect
           (progn
             (switch-to-buffer ,buffer)
             ,@body)
         (switch-to-buffer ,before-buffer)))))

;; This tests for a regression in emacs 25.0 see bug #23632
(ert-deftest simple-test-undo-extra-boundary-in-tex ()
  (should
   (string=
    ""
    (simple-test-undo-with-switched-buffer
        "temp.tex"
      (latex-mode)
      ;; This macro calls `latex-insert-block'
      (execute-kbd-macro
       (read-kbd-macro
        "
C-c C-o			;; latex-insert-block
RET			;; newline
C-/                     ;; undo
"
        ))
      (buffer-substring-no-properties
       (point-min)
       (point-max))))))

(ert-deftest missing-record-point-in-undo ()
  "Check point is being restored correctly.

See Bug#21722."
  (should
   (= 5
      (with-temp-buffer
       (generate-new-buffer " *temp*")
       (emacs-lisp-mode)
       (setq buffer-undo-list nil)
       (insert "(progn (end-of-line) (insert \"hello\"))")
       (beginning-of-line)
       (forward-char 4)
       (undo-boundary)
       (eval-defun nil)
       (undo-boundary)
       (undo)
       (point)))))


;;; `eval-expression'

(ert-deftest eval-expression-print-format-sym ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) t)))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "t"))))))

(ert-deftest eval-expression-print-format-sym-echo ()
  ;; We can only check the echo area when running interactive.
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) t)))
      (let ((current-prefix-arg nil))
        (message nil)
        (call-interactively #'eval-expression)
        (should (equal (current-message) "t"))))))

(ert-deftest eval-expression-print-format-small-int ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) ?A)))
      (let ((current-prefix-arg '(4)))
        (erase-buffer)
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "65")))
      (let ((current-prefix-arg 0))
        (erase-buffer)
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "65 (#o101, #x41, ?A)"))))))

(ert-deftest eval-expression-print-format-small-int-echo ()
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) ?A)))
      (let ((current-prefix-arg nil))
        (message nil)
        (call-interactively #'eval-expression)
        (should (equal (current-message) "65 (#o101, #x41, ?A)"))))))

(ert-deftest eval-expression-print-format-large-int ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) ?B))
              (eval-expression-print-maximum-character ?A))
      (let ((current-prefix-arg '(4)))
        (erase-buffer)
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "66")))
      (let ((current-prefix-arg 0))
        (erase-buffer)
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "66 (#o102, #x42)")))
      (let ((current-prefix-arg -1))
        (erase-buffer)
        (call-interactively #'eval-expression)
        (should (equal (buffer-string) "66 (#o102, #x42, ?B)"))))))

(ert-deftest eval-expression-print-format-large-int-echo ()
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (cl-letf (((symbol-function 'read--expression) (lambda (&rest _) ?B))
              (eval-expression-print-maximum-character ?A))
      (let ((current-prefix-arg nil))
        (message nil)
        (call-interactively #'eval-expression)
        (should (equal (current-message) "66 (#o102, #x42)")))
      (let ((current-prefix-arg '-))
        (message nil)
        (call-interactively #'eval-expression)
        (should (equal (current-message) "66 (#o102, #x42, ?B)"))))))

(ert-deftest command-execute-prune-command-history ()
  "Check that Bug#31211 is fixed."
  (let ((history-length 1)
        (command-history ()))
    (dotimes (_ (1+ history-length))
      (command-execute "" t))
    (should (= (length command-history) history-length))))


;;; `line-number-at-pos'

(ert-deftest line-number-at-pos-in-widen-buffer ()
  (let ((target-line 3))
    (with-temp-buffer
      (insert "a\nb\nc\nd\n")
      (goto-char (point-min))
      (forward-line (1- target-line))
      (should (equal (line-number-at-pos) target-line))
      (should (equal (line-number-at-pos nil t) target-line)))))

(ert-deftest line-number-at-pos-in-narrow-buffer ()
  (let ((target-line 3))
    (with-temp-buffer
      (insert "a\nb\nc\nd\n")
      (goto-char (point-min))
      (forward-line (1- target-line))
      (narrow-to-region (line-beginning-position) (line-end-position))
      (should (equal (line-number-at-pos) 1))
      (should (equal (line-number-at-pos nil t) target-line)))))

(ert-deftest line-number-at-pos-keeps-restriction ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (goto-char (point-min))
    (forward-line 2)
    (narrow-to-region (line-beginning-position) (line-end-position))
    (should (equal (line-number-at-pos) 1))
    (line-number-at-pos nil t)
    (should (equal (line-number-at-pos) 1))))

(ert-deftest line-number-at-pos-keeps-point ()
  (let (pos)
    (with-temp-buffer
      (insert "a\nb\nc\nd\n")
      (goto-char (point-min))
      (forward-line 2)
      (setq pos (point))
      (line-number-at-pos)
      (line-number-at-pos nil t)
      (should (equal pos (point))))))

(ert-deftest line-number-at-pos-when-passing-point ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (should (equal (line-number-at-pos 1) 1))
    (should (equal (line-number-at-pos 3) 2))
    (should (equal (line-number-at-pos 5) 3))
    (should (equal (line-number-at-pos 7) 4))))


;;; Auto fill.

(ert-deftest auto-fill-mode-no-break-before-length-of-fill-prefix ()
  (with-temp-buffer
    (setq-local fill-prefix "   ")
    (set-fill-column 5)
    ;; Shouldn't break after 'foo' (3 characters) when the next
    ;; line is indented >= to that, that wouldn't result in shorter
    ;; lines.
    (insert "foo bar")
    (do-auto-fill)
    (should (string-equal (buffer-string) "foo bar"))))


;;; Shell command.

(ert-deftest simple-tests-async-shell-command-30280 ()
  "Test for https://debbugs.gnu.org/30280 ."
  (let* ((async-shell-command-buffer 'new-buffer)
         (async-shell-command-display-buffer nil)
         (base "name")
         (first (buffer-name (generate-new-buffer base)))
         (second (generate-new-buffer-name base))
         ;; `save-window-excursion' doesn't restore frame configurations.
         (pop-up-frames nil)
         (inhibit-message t)
         (emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-executable-p emacs))
    ;; Let `shell-command' create the buffer as needed.
    (kill-buffer first)
    (unwind-protect
        (save-window-excursion
          ;; One command has no output, the other does.
          ;; Removing the -eval argument also yields no output, but
          ;; then both commands exit simultaneously when
          ;; `accept-process-output' is called on the second command.
          (dolist (form '("(sleep-for 8)" "(message \"\")"))
            (async-shell-command (format "%s -Q -batch -eval '%s'"
                                         emacs form)
                                 first))
          ;; First command should neither have nor display output.
          (let* ((buffer (get-buffer first))
                 (process (get-buffer-process buffer)))
            (should (buffer-live-p buffer))
            (should process)
            (should (zerop (buffer-size buffer)))
            (should (not (get-buffer-window buffer))))
          ;; Second command should both have and display output.
          (let* ((buffer (get-buffer second))
                 (process (get-buffer-process buffer)))
            (should (buffer-live-p buffer))
            (should process)
            (should (accept-process-output process 4 nil t))
            (should (> (buffer-size buffer) 0))
            (should (get-buffer-window buffer))))
      (dolist (name (list first second))
        (let* ((buffer (get-buffer name))
               (process (and buffer (get-buffer-process buffer))))
          (when process (delete-process process))
          (when buffer (kill-buffer buffer)))))))


;;; Tests for shell-command-dont-erase-buffer

(defmacro with-shell-command-dont-erase-buffer (str output-buffer-is-current &rest body)
  (declare (debug (form &body)) (indent 2))
  (let ((expected (make-symbol "expected"))
        (command (make-symbol "command"))
        (caller-buf (make-symbol "caller-buf"))
        (output-buf (make-symbol "output-buf")))
    `(let* ((,caller-buf (generate-new-buffer "caller-buf"))
            (,output-buf (if ,output-buffer-is-current ,caller-buf
                           (generate-new-buffer "output-buf")))
            (emacs (expand-file-name invocation-name invocation-directory))
            (,command (format "%s -Q --batch --eval '(princ \"%s\")'"
                              emacs ,str))
            (inhibit-message t))
       (unwind-protect
           ;; Feature must work the same regardless how we specify the 2nd arg of `shell-command', ie,
           ;; as a buffer, buffer name (or t, if the output must go to the current buffer).
           (dolist (output (append (list ,output-buf (buffer-name ,output-buf))
                                   (if ,output-buffer-is-current '(t) nil)))
             (dolist (save-pos '(erase nil beg-last-out end-last-out save-point))
               (let ((shell-command-dont-erase-buffer save-pos))
                 (with-current-buffer ,output-buf (erase-buffer))
                 (with-current-buffer ,caller-buf
                   (dotimes (_ 2) (shell-command ,command output)))
                 (with-current-buffer ,output-buf
                   ,@body))))
         (kill-buffer ,caller-buf)
         (when (buffer-live-p ,output-buf)
           (kill-buffer ,output-buf))))))

(ert-deftest simple-tests-shell-command-39067 ()
  "The output buffer is erased or not according to `shell-command-dont-erase-buffer'."
  (let ((str "foo\n"))
    (dolist (output-current '(t nil))
      (with-shell-command-dont-erase-buffer str output-current
        (let ((expected (cond ((eq shell-command-dont-erase-buffer 'erase) str)
                              ((null shell-command-dont-erase-buffer)
                               (if output-current (concat str str)
                                 str))
                              (t (concat str str)))))
          (should (string= expected (buffer-string))))))))

(ert-deftest simple-tests-shell-command-dont-erase-buffer ()
  "The point is set at the expected position after execution of the command."
  (let* ((str "foo\n")
         (expected-point `((beg-last-out . ,(1+ (length str)))
                           (end-last-out . ,(1+ (* 2 (length str))))
                           (save-point . 1))))
    (dolist (output-buffer-is-current '(t ni))
      (with-shell-command-dont-erase-buffer str output-buffer-is-current
        (when (memq shell-command-dont-erase-buffer '(beg-last-out end-last-out save-point))
          (should (= (point) (alist-get shell-command-dont-erase-buffer expected-point))))))))


(provide 'simple-test)
;;; simple-test.el ends here
