;;; so-long-tests.el --- Test suite for so-long.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Keywords: convenience

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

;; Most of these tests use the shebang #!emacs to get `normal-mode' to
;; select `emacs-lisp-mode', as using a file-local mode variable would
;; usually trigger `so-long-file-local-mode-function'.  In cases where
;; we need to `hack-local-variables', we instead set `buffer-file-name'.
;; (We could consistently use the latter, but the mixture of approaches
;; means that we're testing more things.)

;; Running manually:
;;
;; for test in lisp/so-long-tests/*-tests.el; do make ${test%.el}; done \
;; 2>&1 | grep -E -v '^(Loading|Source file|make|Changed to so-long-mode)'
;;
;; Which is equivalent to:
;;
;; for test in lisp/so-long-tests/*-tests.el; do \
;; HOME=/nonexistent EMACSLOADPATH= LC_ALL=C EMACS_TEST_DIRECTORY=. \
;; "../src/emacs" --no-init-file --no-site-file --no-site-lisp \
;; -L ":." -l ert -l "$test" --batch --eval \
;; '(ert-run-tests-batch-and-exit (quote (not (tag :unstable))))'; \
;; done 2>&1 | grep -E -v '^(Loading|Source file|Changed to so-long-mode)'
;;
;; See also `ert-run-tests-batch-and-exit'.

;;; Code:

(require 'ert)
(require 'so-long)
(load (expand-file-name "so-long-tests-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))

(declare-function so-long-tests-remember "so-long-tests-helpers")
(declare-function so-long-tests-assert-active "so-long-tests-helpers")
(declare-function so-long-tests-assert-reverted "so-long-tests-helpers")
(declare-function so-long-tests-assert-and-revert "so-long-tests-helpers")
(declare-function so-long-tests-predicates "so-long-tests-helpers")

;; Enable the automated behavior for all tests.
(global-so-long-mode 1)

(ert-deftest so-long-tests-threshold-under ()
  "Under line length threshold."
  (dolist (so-long-predicate (so-long-tests-predicates))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1- so-long-threshold) ?x))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))))

(ert-deftest so-long-tests-threshold-at ()
  "At line length threshold."
  (dolist (so-long-predicate (so-long-tests-predicates))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1- so-long-threshold) ?x))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))))

(ert-deftest so-long-tests-threshold-over ()
  "Over line length threshold."
  (dolist (so-long-predicate (so-long-tests-predicates))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember)
      (insert (make-string (1+ so-long-threshold) ?x))
      (normal-mode)
      (so-long-tests-assert-and-revert 'so-long-mode))))

(ert-deftest so-long-tests-skip-comments ()
  "Skip leading shebang, whitespace, and comments."
  ;; Only for `so-long-detected-long-line-p' -- comments are not
  ;; treated differently when using `so-long-statistics-excessive-p'.
  (dolist (so-long-predicate (so-long-tests-predicates))
    ;; Long comment, no newline.
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1+ so-long-threshold) ?\;))
      (normal-mode)
      (should (eq major-mode
                  (cond ((eq so-long-predicate #'so-long-detected-long-line-p)
                         'emacs-lisp-mode)
                        ((eq so-long-predicate #'so-long-statistics-excessive-p)
                         'so-long-mode)))))
    ;; Long comment, with newline.
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1+ so-long-threshold) ?\;))
      (insert "\n")
      (normal-mode)
      (should (eq major-mode
                  (cond ((eq so-long-predicate #'so-long-detected-long-line-p)
                         'emacs-lisp-mode)
                        ((eq so-long-predicate #'so-long-statistics-excessive-p)
                         'so-long-mode)))))
    ;; Long comment, with short text following.
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1+ so-long-threshold) ?\;))
      (insert "\n")
      (insert (make-string so-long-threshold ?x))
      (normal-mode)
      (should (eq major-mode
                  (cond ((eq so-long-predicate #'so-long-detected-long-line-p)
                         'emacs-lisp-mode)
                        ((eq so-long-predicate #'so-long-statistics-excessive-p)
                         'so-long-mode)))))
    ;; Long comment, with long text following.
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (insert (make-string (1+ so-long-threshold) ?\;))
      (insert "\n")
      (insert (make-string (1+ so-long-threshold) ?x))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))))

(ert-deftest so-long-tests-max-lines ()
  "Give up after `so-long-max-lines'."
  ;; Only for `so-long-detected-long-line-p' -- the whole buffer is
  ;; 'seen' when using `so-long-statistics-excessive-p'.
  (dolist (so-long-predicate (so-long-tests-predicates))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      ;; Insert exactly `so-long-max-lines' non-comment lines, followed
      ;; by a long line.
      (dotimes (_ so-long-max-lines)
        (insert "x\n"))
      (insert (make-string (1+ so-long-threshold) ?x))
      (normal-mode)
      (should (eq major-mode
                  (cond ((eq so-long-predicate #'so-long-detected-long-line-p)
                         'emacs-lisp-mode)
                        ((eq so-long-predicate #'so-long-statistics-excessive-p)
                         'so-long-mode))))
      ;; If `so-long-max-lines' is nil, don't give up the search.
      (let ((so-long-max-lines nil))
        (normal-mode)
        (should (eq major-mode 'so-long-mode)))
      ;; If `so-long-skip-leading-comments' is nil, all lines are
      ;; counted, and so the shebang line counts, which makes the
      ;; long line one line further away.
      (let ((so-long-skip-leading-comments nil)
            (so-long-max-lines (1+ so-long-max-lines)))
        (normal-mode)
        (should (eq major-mode
                    (cond ((eq so-long-predicate #'so-long-detected-long-line-p)
                           'emacs-lisp-mode)
                          ((eq so-long-predicate #'so-long-statistics-excessive-p)
                           'so-long-mode))))
        (let ((so-long-max-lines (1+ so-long-max-lines)))
          (normal-mode)
          (should (eq major-mode 'so-long-mode)))))))

(ert-deftest so-long-tests-invisible-buffer-function ()
  "Call `so-long-invisible-buffer-function' in invisible buffers."
  ;; Visible buffer.
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (so-long-tests-assert-and-revert 'so-long-mode))
  ;; Invisible buffer.
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode))
    (should (eq nil (get-buffer-window)))
    ;; Displaying the buffer should invoke `so-long'.
    (display-buffer (current-buffer))
    (should (window-live-p (get-buffer-window)))
    (unless (version< emacs-version "27")
      ;; From Emacs 27 the `display-buffer' call is insufficient.
      ;; The various 'window change functions' are now invoked by the
      ;; redisplay, and redisplay does nothing at all in batch mode,
      ;; so we cannot test under this revised behavior.  Refer to:
      ;; https://lists.gnu.org/r/emacs-devel/2019-10/msg00971.html
      ;; For interactive (non-batch) test runs, calling `redisplay'
      ;; does do the trick; so do that first.
      (redisplay)
      (when noninteractive
        ;; In batch mode we need to cheat, and just pretend that
        ;; `redisplay' triggered `window-configuration-change-hook'.
        ;; This means the test is not as useful, but it still covers
        ;; part of the process, and so it's better than nothing.
        ;;
        ;; Also test `so-long--active', in case a future version of
        ;; Emacs adds the framework necessary to make `redisplay' work
        ;; in batch mode.
        (unless (eq so-long--active t)
          (with-suppressed-warnings
              ((obsolete run-window-configuration-change-hook))
            (run-window-configuration-change-hook)))))
    (so-long-tests-assert-and-revert 'so-long-mode))
  ;; `so-long-invisible-buffer-function' is nil.
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (let ((so-long-invisible-buffer-function nil))
      (normal-mode))
    (so-long-tests-assert-and-revert 'so-long-mode))
  ;; `so-long-invisible-buffer-function' is `so-long'.
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (let ((so-long-invisible-buffer-function #'so-long))
      (normal-mode))
    (so-long-tests-assert-and-revert 'so-long-mode))
  ;; `so-long-invisible-buffer-function' is `ignore'.
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (let ((so-long-invisible-buffer-function #'ignore))
      (normal-mode))
    (should (eq major-mode 'emacs-lisp-mode))
    (display-buffer (current-buffer))
    (unless (version< emacs-version "27")
      ;; See the "Invisible buffer" case earlier in this function.
      (redisplay)
      (when noninteractive
        (unless (eq so-long--active t)
          (with-suppressed-warnings
              ((obsolete run-window-configuration-change-hook))
            (run-window-configuration-change-hook)))))
    (should (eq major-mode 'emacs-lisp-mode))))

(ert-deftest so-long-tests-actions ()
  "Test each of the standard actions."
  (dolist (action (mapcar #'car so-long-action-alist))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember)
      (insert (make-string (1+ so-long-threshold) ?x))
      (let ((so-long-action action))
        (normal-mode)
        (so-long-tests-assert-and-revert action)))))

(ert-deftest so-long-tests-command-so-long ()
  "Test the `so-long' command."
  ;; Includes argument of nil, meaning the default `so-long-mode' action.
  (dolist (action (cons nil (mapcar #'car so-long-action-alist)))
    (with-temp-buffer
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember)
      (insert (make-string (1+ so-long-threshold) ?x))
      (so-long action)
      (so-long-tests-assert-and-revert (or action 'so-long-mode)))))

(ert-deftest so-long-tests-so-long-menu-item-replace-action ()
  "Test using the `so-long-menu' action commands."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (let ((menu (so-long-menu))
          action
          command)
      (dolist (item so-long-action-alist)
        (setq action (car item)
              command (lookup-key menu (vector action)))
        (funcall command)
        (so-long-tests-assert-active action))
      ;; After all actions have been used, revert to normal and assert
      ;; that the most recent action to have been applied is the one
      ;; we have just reverted.
      (funcall (lookup-key menu [so-long-revert]))
      (so-long-tests-assert-reverted action))))

(ert-deftest so-long-tests-major-mode ()
  "Test calling `so-long-mode' directly."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (so-long-mode)
    (so-long-tests-assert-and-revert 'so-long-mode)))

(ert-deftest so-long-tests-minor-mode ()
  "Test calling `so-long-minor-mode' directly."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (so-long-minor-mode 1)
    (so-long-tests-assert-active 'so-long-minor-mode)
    (so-long-minor-mode 0)
    (so-long-tests-assert-reverted 'so-long-minor-mode)))

(ert-deftest so-long-tests-target-modes ()
  "Targeted major modes."
  ;; Test the `so-long-target-modes' user option.
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?x))
    ;; Nil target modes.
    (let ((so-long-target-modes nil))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))
    ;; Non-matching target modes.
    (let ((so-long-target-modes '(text-mode)))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))
    ;; Matching mode (direct).
    (let ((so-long-target-modes '(emacs-lisp-mode)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))
    ;; Matching mode (indirect).
    (let ((so-long-target-modes '(prog-mode)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))))

(ert-deftest so-long-tests-preserved-variables-and-modes ()
  "Preserved variables and minor modes when using `so-long-mode'."
  ;; Test the user options `so-long-mode-preserved-variables' and
  ;; `so-long-mode-preserved-minor-modes'.  The minor mode `view-mode'
  ;; is 'preserved' by default (using both options).
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (normal-mode)
    ;; We enable `view-mode' before triggering `so-long'.
    (insert (make-string (1+ so-long-threshold) ?x))
    (view-mode 1)
    (should (eq view-mode t))
    (should (eq buffer-read-only t))
    (so-long-tests-remember)
    (let ((so-long-action 'so-long-mode)
          (menu (so-long-menu)))
      (so-long)
      (so-long-tests-assert-active 'so-long-mode)
      (should (eq view-mode t))
      (should (eq buffer-read-only t))
      ;; Revert.
      (funcall (lookup-key menu [so-long-revert]))
      (so-long-tests-assert-reverted 'so-long-mode)
      (should (eq view-mode t))
      (should (eq buffer-read-only t))
      ;; Disable `view-mode'.  Note that without the preserved
      ;; variables, the conflict between how `view-mode' and `so-long'
      ;; each deal with the buffer's original `buffer-read-only' value
      ;; would lead to a situation whereby the buffer would still be
      ;; read-only after `view-mode' had been disabled.
      (view-mode 0)
      (should (eq view-mode nil))
      (should (eq buffer-read-only nil))))
  ;; Without `view-mode'.
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (normal-mode)
    (insert (make-string (1+ so-long-threshold) ?x))
    (should (eq view-mode nil))
    (so-long-tests-remember)
    (let ((so-long-action 'so-long-mode)
          (menu (so-long-menu)))
      (so-long)
      (so-long-tests-assert-active 'so-long-mode)
      (should (eq view-mode nil))
      ;; Revert.
      (funcall (lookup-key menu [so-long-revert]))
      (so-long-tests-assert-reverted 'so-long-mode)
      (should (eq view-mode nil)))))

(ert-deftest so-long-tests-predicate ()
  "Custom predicate function."
  ;; Test the `so-long-predicate' user option.
  ;; Always true.  Trigger when we normally wouldn't.
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (let ((so-long-predicate (lambda () t)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode))))
  ;; Always false.  Don't trigger when we normally would.
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?x))
    (let ((so-long-predicate #'ignore))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))))

(ert-deftest so-long-tests-file-local-action ()
  "File-local action."
  ;; Test `so-long-action' as a file-local variable.
  ;; Only valid in Emacs26+. Refer to "Caveats" in the so-long.el Commentary.
  (unless (version< emacs-version "26")
    (with-temp-buffer
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember))
    ;; n.b. `run-mode-hooks' *only* runs `hack-local-variables' when there's a
    ;; (buffer-file-name), so the #!emacs approach is insufficient here.  It's
    ;; valid for the file-locals to be on the second line after the shebang,
    ;; but with the *.el filename we no longer need the shebang.
    (with-temp-buffer
      (display-buffer (current-buffer))
      (setq buffer-file-name (expand-file-name "so-long-tests-data.el"))
      (insert ";; -*- so-long-action:so-long-minor-mode; -*-\n")
      (put 'so-long-action 'safe-local-variable #'symbolp)
      (insert (make-string (1+ so-long-threshold) ?x))
      (normal-mode)
      (so-long-tests-assert-and-revert 'so-long-minor-mode))))

(ert-deftest so-long-tests-file-local-action-eval-so-long ()
  "File-local action and eval:(so-long)."
  ;; As per previous test, but using file-local `eval' to call `so-long'.
  ;; Only valid in Emacs26+. Refer to "Caveats" in the so-long.el Commentary.
  ;; See also `so-long-tests-file-local-action' above.
  (unless (version< emacs-version "26")
    (with-temp-buffer
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember))
    (with-temp-buffer
      (display-buffer (current-buffer))
      (setq buffer-file-name (concat (make-temp-name "so-long-tests-") ".el"))
      (insert ";; -*- so-long-action:so-long-minor-mode; eval:(so-long) -*-\n")
      (put 'so-long-action 'safe-local-variable #'symbolp)
      (push '(eval . (so-long)) safe-local-variable-values)
      (normal-mode)
      (so-long-tests-assert-and-revert 'so-long-minor-mode))))

(defvar so-long-tests-local-mode 'unset
  "Set by `so-long-tests-file-local-mode-function'.")

(defun so-long-tests-file-local-mode-function (mode)
  "A custom value for `so-long-file-local-mode-function'."
  (setq so-long-tests-local-mode mode))

;; Test `so-long-file-local-mode-function' when the file-local major
;; mode is `emacs-lisp-mode'.

(defmacro so-long-tests-deftest-file-local-emacs-lisp-mode
    (sym docstring prop-line &optional local-vars)
  "Generate tests for using `emacs-lisp-mode' as a file-local mode."
  (setq prop-line (or prop-line "")
        local-vars (or local-vars ""))
  `(ert-deftest ,sym ()
     ,docstring
     (let ((orig so-long-file-local-mode-function))
       ;; Do nothing at all when a file-local mode is used.
       (setq-default so-long-file-local-mode-function 'so-long-inhibit)
       (with-temp-buffer
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         ;; Remember the `emacs-lisp-mode' state.  The other cases
         ;; will validate the 'reverted' state against this.  (Note
         ;; that we haven't displayed the buffer, and therefore only
         ;; `so-long-invisible-buffer-function' has acted, so we are
         ;; still remembering the 'before' state.)
         (so-long-tests-remember)
         (should (eq major-mode 'emacs-lisp-mode)))
       ;; Downgrade the action from major mode to minor mode.
       (setq-default so-long-file-local-mode-function 'so-long-mode-downgrade)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         (so-long-tests-assert-and-revert 'so-long-minor-mode))
       ;; Do not treat the file-local mode specially.
       (setq-default so-long-file-local-mode-function nil)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         (so-long-tests-assert-and-revert 'so-long-mode))
       ;; Custom function
       (setq-default so-long-file-local-mode-function
                     #'so-long-tests-file-local-mode-function)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (let (so-long-tests-local-mode)
           (normal-mode)
           (should (eq so-long-tests-local-mode 'emacs-lisp-mode))
           (so-long-tests-assert-active 'so-long-mode)))
       ;; end
       (setq-default so-long-file-local-mode-function orig))))

(so-long-tests-deftest-file-local-emacs-lisp-mode
  so-long-tests-file-local-emacs-lisp-mode-short-form
  "File-local mode (short form). -*- emacs-lisp -*-"
  ";; -*- emacs-lisp -*-\n")

(so-long-tests-deftest-file-local-emacs-lisp-mode
  so-long-tests-file-local-emacs-lisp-mode-long-form
  "File-local mode (long form). -*- emacs-lisp -*-"
  ";; -*- mode: emacs-lisp -*-\n")

(so-long-tests-deftest-file-local-emacs-lisp-mode
  so-long-tests-file-local-emacs-lisp-mode-long-form2
  "File-local mode (long form). -*- emacs-lisp -*-"
  nil "\n;; Local Variables:\n;; mode: emacs-lisp\n;; End:\n")

;; Test `so-long-file-local-mode-function' when the file-local major
;; mode is `so-long-mode'.  In this case we should always end up with
;; the major mode being `so-long-mode'.

(defmacro so-long-tests-deftest-file-local-so-long-mode
    (sym docstring prop-line &optional local-vars)
  "Generate tests for using `so-long-mode' as a file-local mode."
  (setq prop-line (or prop-line "")
        local-vars (or local-vars ""))
  `(ert-deftest ,sym ()
     ,docstring
     (let ((orig so-long-file-local-mode-function))
       ;; Do nothing at all when a file-local mode is used.
       (setq-default so-long-file-local-mode-function 'so-long-inhibit)
       (with-temp-buffer
         (display-buffer (current-buffer))
         ;; Remember the new-buffer state.  The other cases will
         ;; validate the 'reverted' state against this.
         (so-long-tests-remember)
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         (so-long-tests-assert-and-revert 'so-long-mode))
       ;; Downgrade from major mode to minor mode.
       (setq-default so-long-file-local-mode-function 'so-long-mode-downgrade)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         (so-long-tests-assert-and-revert 'so-long-mode))
       ;; Do not treat the file-local mode specially.
       (setq-default so-long-file-local-mode-function nil)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (normal-mode)
         (so-long-tests-assert-and-revert 'so-long-mode))
       ;; Custom function.
       (setq-default so-long-file-local-mode-function
                     #'so-long-tests-file-local-mode-function)
       (with-temp-buffer
         (display-buffer (current-buffer))
         (insert ,prop-line)
         (insert (make-string (1+ so-long-threshold) ?x))
         (insert ,local-vars)
         (let (so-long-tests-local-mode)
           (normal-mode)
           (should (eq so-long--inhibited t))
           (should (eq so-long-tests-local-mode 'so-long-mode))
           (so-long-tests-assert-active 'so-long-mode)))
       ;; end
       (setq-default so-long-file-local-mode-function orig))))

(so-long-tests-deftest-file-local-so-long-mode
  so-long-tests-file-local-so-long-mode-short-form
  "File-local mode (short form). -*- so-long -*-"
  ";; -*- so-long -*-\n")

(so-long-tests-deftest-file-local-so-long-mode
  so-long-tests-file-local-so-long-mode-long-form
  "File-local mode (long form). -*- mode: so-long -*-"
  ";; -*- mode: so-long -*-\n")

(so-long-tests-deftest-file-local-so-long-mode
  so-long-tests-file-local-so-long-mode-long-form2
  "File-local mode (long form). -*- mode: so-long -*-"
  nil "\n;; Local Variables:\n;; mode: so-long\n;; End:\n")

(ert-deftest so-long-tests-commentary ()
  "Test the `so-long-commentary' command."
  (so-long-commentary)
  (should (string= (buffer-name) "*So Long: Commentary*"))
  (should (eq major-mode 'outline-mode))
  (should (eq view-mode t))
  (should (looking-at "^\\* Introduction$"))
  (goto-char (point-min))
  (should (looking-at "^so-long\\.el$"))
  (should (re-search-forward "^\\* Change Log:$")))

(ert-deftest so-long-tests-customize ()
  "Test the `so-long-customize' command."
  (so-long-customize)
  (should (string= (buffer-name) "*Customize Group: So Long*"))
  (should (eq major-mode 'Custom-mode)))

;; Page break to prevent the local vars strings above from
;; being misinterpreted as actual local vars declarations.

;;; so-long-tests.el ends here
