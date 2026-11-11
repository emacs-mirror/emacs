;;; kitty-kbd-proto.el --- support for the Kitty Keyboard Protocol  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

;; https://sw.kovidgoyal.net/kitty/keyboard-protocol/

;;; Code:

(defcustom tty-use-kitty-kbd-proto t
  "Whether to use the Kitty Keyboard Protocol where supported.
Emacs normally queries whether a terminal emulator supports the Kitty
Keyboard Protocol, and if it does, it makes use of the protocol.
This allows Emacs to distinguish between key sequences which would
otherwise be collapsed, such as \\`RET' and \\`C-<return>'.
If your terminal emulator's implementation of the Kitty Keyboard
Protocol is buggy, you can customize this variable to nil to stop Emacs
trying to activate it.  Then for any new terminals the Kitty Keyboard
Protocol won't be used.  Existing terminals aren't affected by toggling
this option."
  :type 'boolean
  :group 'terminals
  :version "32.1")

(defconst kitty-kbd-proto-progressive-enhancements 29
  "What Kitty Keyboard Protocol progressive enhancements this Emacs uses.
Encoded as a single integer.
29 is progressive enhancement  1 (disambiguate escape codes)
 plus progressive enhancement  4 (report alternate keys)
 plus progressive enhancement  8 (report all keys as escape codes)
 plus progressive enhancement 16 (report associated text).")

(defconst kitty-kbd-proto-query
  (format "\e[>%du\e[?u" kitty-kbd-proto-progressive-enhancements)
  "Sequence to turn on Kitty Keyboard Protocol and query enhancement level.")

(declare-function x-setup-function-keys "term/common-win")

(defun kitty-kbd-proto-init (already-active)
  "Terminal initialization for the Kitty Keyboard Protocol.
ALREADY-ACTIVE non-nil means the escape characters that tell the
terminal emulator we want to use the Kitty Keyboard Protocol have
already been sent."
  (let ((turn-on (format "\e[>%du" kitty-kbd-proto-progressive-enhancements)))
    (unless already-active (send-string-to-terminal turn-on))
    (push "\e[<u" (terminal-parameter nil 'tty-mode-reset-strings))
    (push turn-on (terminal-parameter nil 'tty-mode-set-strings))
    (set-terminal-parameter nil 'tty-kitty-kbd-proto t)
    (require 'term/common-win)
    (x-setup-function-keys (selected-frame))
    ;; Force `normal-erase-is-backspace-setup-frame' to recompute,
    ;; because enabling the Kitty Keyboard Protocol means the frame can
    ;; now distinguish DEL and [delete].
    (set-terminal-parameter nil 'normal-erase-is-backspace nil)
    (normal-erase-is-backspace-setup-frame)))

(defun kitty-kbd-proto-off ()
  "Send the string that turns off the Kitty Keyboard Protocol.
This doesn't undo the other effects of `kitty-kbd-proto-init'; that is
not supported."
  (send-string-to-terminal "\e[<u"))

(provide 'term/kitty-kbd-proto)
;;; kitty-kbd-proto.el ends here
