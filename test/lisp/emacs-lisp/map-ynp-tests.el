;;; map-ynp-tests.el --- Tests for map-ynp.el        -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Spencer Baugh <sbaugh@catern.com>
;; Maintainer: emacs-devel@gnu.org

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

;; Tests for map-ynp.el.

;;; Code:

(require 'ert)

(defun map-ynp-tests-simple-call ()
  (map-y-or-n-p ""  #'ignore '(1)))

(defun test-map-ynp-kmacro-1 ()
  (let ((eval-expression-debug-on-error nil)) ;; bug#67836
    (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET y"))
    (should-error
     (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET")))
    (unless noninteractive
      (let ((noninteractive t))
        (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET y"))
        (should-error
         (execute-kbd-macro (read-kbd-macro "M-: (map-ynp-tests-simple-call) RET")))))))

(ert-deftest test-map-ynp-kmacro ()
  "Test that `map-y-or-n-p' in a kmacro terminates on end of input."
  (let ((y-or-n-p-use-read-key nil))
    (test-map-ynp-kmacro-1))
  (let ((y-or-n-p-use-read-key t))
    (test-map-ynp-kmacro-1)))

(defvar map-ynp-tests-result nil)

(defvar-keymap map-ynp-tests-map
  "C-x s" 'map-ynp-tests-command)

(defun map-ynp-tests-command-symbol (obj)
  (interactive)
  (push obj map-ynp-tests-result))

(defun map-ynp-tests-command ()
  (interactive)
  (should (equal (map-y-or-n-p
                  "Prompt "
                  (lambda (obj)
                    (push obj map-ynp-tests-result))
                  '(1 2 3 4)
                  nil
                  `((?\C-r map-ynp-tests-command-symbol "C-r")
                    (?\M-~ ,(lambda (obj)
                              (push obj map-ynp-tests-result))
                           "M-~")))
                 (length map-ynp-tests-result))))

(defun map-ynp-tests-run (keys result)
  (setq map-ynp-tests-result nil)
  (execute-kbd-macro (read-kbd-macro (concat "C-x s " keys)))
  (should (equal (nreverse map-ynp-tests-result) result)))

(defun test-map-ynp-keys-1 ()
  (with-temp-buffer
    (save-window-excursion
      ;; `execute-kbd-macro' applied to window only
      (set-window-buffer nil (current-buffer))
      (use-local-map map-ynp-tests-map)

      (map-ynp-tests-run "y Y SPC n" '(1 2 3))
      (map-ynp-tests-run "n N DEL y" '(4))
      (map-ynp-tests-run "n !" '(2 3 4))
      (map-ynp-tests-run "." '(1))
      (map-ynp-tests-run "y q" '(1))
      (map-ynp-tests-run "y RET" '(1))
      (map-ynp-tests-run "C-r M-~ ESC ~ q" '(1 2 3))

      (map-ynp-tests-run "x q" nil) ;; x - random char

      (kill-buffer (help-buffer))
      (if y-or-n-p-use-read-key
          (map-ynp-tests-run "? q" nil)
        (map-ynp-tests-run "C-h q" nil))
      (should (get-buffer (help-buffer)))

      (should (equal 'quit
                     (condition-case err
                         (map-ynp-tests-run "C-g" nil)
                       (quit (car err)))))

      (should (equal 'quit
                     (condition-case err
                         (map-ynp-tests-run "ESC ESC ESC" nil)
                       (quit (car err))))))))

(ert-deftest test-map-ynp-keys ()
  "Test keys for `map-y-or-n-p'."
  (let ((y-or-n-p-use-read-key nil))
    (test-map-ynp-keys-1))
  (let ((y-or-n-p-use-read-key t))
    (test-map-ynp-keys-1)))

(provide 'map-ynp-tests)
;;; map-ynp-tests.el ends here
