;;; terminal-tests.el --- tests for terminal.c -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

(require 'ert)

(ert-deftest frame-initial-p ()
  "Test `frame-initial-p' behavior."
  (should-not (frame-initial-p t))
  (should-not (frame-initial-p (current-buffer)))
  (should-not (frame-initial-p (selected-window)))
  ;; "Initial frame" implies "initial terminal", and
  ;; no other terminal can have the initial frame.
  (should-not (xor (equal (terminal-name) "initial_terminal")
                   (frame-initial-p)))
  ;; Initial frame implies its terminal is a termcap-like
  ;; text-mode terminal.
  (should (or (not (frame-initial-p))
              (eq (terminal-live-p nil) t)))
  ;; It similarly implies a termcap-like text-mode frame.
  (should (or (not (frame-initial-p))
              (eq (frame-live-p (selected-frame)) t)))
  (dolist (ft (append '(nil) (frame-list) (terminal-list)))
    (ert-info ((prin1-to-string ft) :prefix "Argument: ")
      (should-not (xor (equal (terminal-name ft) "initial_terminal")
                       (frame-initial-p ft)))
      (should (or (not (frame-initial-p ft))
                  (eq (terminal-live-p ft) t)))))
  (cond (noninteractive
         ;; Batch mode should have an initial frame.
         (should (any #'frame-initial-p (frame-list)))
         (should (any #'frame-initial-p (terminal-list))))
        ((not (daemonp))
         ;; Non-daemon interactive mode should have none.
         (should-not (any #'frame-initial-p (frame-list)))
         (should-not (any #'frame-initial-p (terminal-list))))))

;;; terminal-tests.el ends here
