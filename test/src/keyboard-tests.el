;;; keyboard-tests.el --- Tests for keyboard.c -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

(ert-deftest keyboard-unread-command-events ()
  "Test `unread-command-events'."
  ;; Avoid hang on Cygwin; see bug#65325.
  (skip-unless (or (not (eq system-type 'cygwin))
                   (featurep 'gfilenotify)
                   (featurep 'dbus)
                   (featurep 'threads)))
  (let ((unread-command-events nil))
    (should (equal (progn (push ?\C-a unread-command-events)
                          (read-event nil nil 1))
                   ?\C-a))
    (should (equal (progn (run-with-timer
                           1 nil
                           (lambda () (push '(t . ?\C-b) unread-command-events)))
                          (read-event nil nil 2))
                   ?\C-b))))

(ert-deftest keyboard-lossage-size ()
  "Test `lossage-size'."
  (let ((min-value 100)
        (lossage-orig (lossage-size)))
    (dolist (factor (list 1 3 4 5 10 7 3))
      (let ((new-lossage (* factor min-value)))
        (should (= new-lossage (lossage-size new-lossage)))))
    ;; Wrong type
    (should-error (lossage-size -5))
    (should-error (lossage-size "200"))
    ;; Less that minimum value
    (should-error (lossage-size (1- min-value)))
    (should (= lossage-orig (lossage-size lossage-orig)))))

;; FIXME: This test doesn't currently work :-(
;; (ert-deftest keyboard-tests--echo-keystrokes-bug15332 ()
;;   (let ((msgs '())
;;         (unread-command-events nil)
;;         (redisplay--interactive t)
;;         (echo-keystrokes 2))
;;     (setq unread-command-events '(?\C-u))
;;     (let* ((timer1
;; 	    (run-with-timer 3 1
;; 			    (lambda ()
;; 			      (setq unread-command-events '(?5)))))
;; 	   (timer2
;; 	    (run-with-timer 2.5 1
;; 			    (lambda ()
;; 			      (push (current-message) msgs)))))
;;       (run-with-timer 5 nil
;; 	              (lambda ()
;; 	                (cancel-timer timer1)
;; 	                (cancel-timer timer2)
;; 	                (throw 'exit msgs)))
;;       (recursive-edit)
;;       (should (equal msgs '("C-u 55-" "C-u 5-" "C-u-"))))))

(ert-deftest keyboard-inhibit-interaction ()
  (let ((inhibit-interaction t))
    (should-error (read-char "foo: "))
    (should-error (read-event "foo: "))
    (should-error (read-char-exclusive "foo: "))))

;;; Tests for `read-key-sequence' code paths.

;;;; Helpers

(defun keyboard-tests--rks-fake-posn (window &optional area)
  "Return a synthetic mouse position list for WINDOW.
AREA, when non-nil, is a non-text part such as `mode-line'."
  (list window (or area 1) '(0 . 0) 0 nil 1
        '(0 . 0) nil '(0 . 0) '(1 . 1)))

(defun keyboard-tests--rks-observe (events local-map &optional dont-downcase-last)
  "Fill `read-key-sequence-vector' with EVENTS under LOCAL-MAP.
EVENTS is a list pushed into `unread-command-events'.
LOCAL-MAP is bound as `overriding-terminal-local-map'.
DONT-DOWNCASE-LAST is forwarded to `read-key-sequence-vector'.
Return a plist with :sequence, :binding, and :shift-translated."
  (clear-this-command-keys)
  (let* (this-command-keys-shift-translated
         ;; This rules over all other keymaps.
         (overriding-terminal-local-map local-map)
         ;; Has priority over keyboard events.
         (unread-command-events events)
         ;; Wrapper around `read-key-sequence'.  Pushes events through
         ;; the machinery.
         (sequence (read-key-sequence-vector nil nil dont-downcase-last))
         (binding (key-binding sequence)))
    (list :sequence sequence
          :binding binding
          :shift-translated this-command-keys-shift-translated)))

(defun keyboard-tests--rks-execute (events &optional local-map)
  "Execute EVENTS through the command loop using LOCAL-MAP."
  (let ((overriding-terminal-local-map local-map))
    ;; This runs translated events through the full command loop, not
    ;; just the sequence.
    (execute-kbd-macro (vconcat events))))

(defun keyboard-tests--rks-with-function-key-bindings (bindings thunk)
  "Run THUNK with BINDINGS installed in `local-function-key-map'."
  (let ((oldmap local-function-key-map)
        (local-function-key-map (make-sparse-keymap)))
    (set-keymap-parent local-function-key-map oldmap)
    (dolist (b bindings)
      (define-key local-function-key-map (car b) (cdr b)))
    (funcall thunk)))

(defun keyboard-tests--rks-with-event-kinds (kinds thunk)
  "Set the `event-kind' property of each KIND, call THUNK, then restore.
KINDS is a list of (SYMBOL . KIND)."
  (let ((saved (mapcar (lambda (k) (cons (car k) (get (car k) 'event-kind)))
                       kinds)))
    (unwind-protect
        (progn
          (dolist (k kinds) (put (car k) 'event-kind (cdr k)))
          (funcall thunk))
      (dolist (k saved) (put (car k) 'event-kind (cdr k))))))

(defun keyboard-tests--rks-call-with-live-window-maps (map-a map-b fn)
  "Call FN with two live buffers/windows whose local maps are MAP-A and MAP-B.
FN is called with BUFFER-A WINDOW-A BUFFER-B WINDOW-B."
  (let ((buffer-a (generate-new-buffer " *keyboard-tests-a*"))
        (buffer-b (generate-new-buffer " *keyboard-tests-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((window-a (selected-window))
                 (window-b (split-window-right)))
            (set-window-buffer window-a buffer-a)
            (set-window-buffer window-b buffer-b)
            (with-current-buffer buffer-a (use-local-map map-a))
            (with-current-buffer buffer-b (use-local-map map-b))
            (funcall fn buffer-a window-a buffer-b window-b)))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

;;;; Keymap interaction

(ert-deftest keyboard-tests-rks-prefix-then-key ()
    "Check reading a prefix key and a following key."
  ;; C-c a -> [C-c a]
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'ignore)
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
      (should (equal [?\C-c ?a] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding))))))

(ert-deftest keyboard-tests-rks-unbound-returns-sequence ()
  "An unbound symbol event returns as a one-element vector unchanged."
  ;; 'symbol-not-bound -> [symbol-not-bound]
  (let* ((map (make-sparse-keymap))
         (obs (keyboard-tests--rks-observe
               (list 'symbol-not-bound) map)))
    (should (equal [symbol-not-bound]
                   (plist-get obs :sequence)))))

(ert-deftest keyboard-tests-rks-bound-sequence-ends-read ()
  "Check that a complete binding ends the sequence.
This happens even when `input-decode-map' has a prefix match starting at
the tail.
Example: on a tty, ESC O B is mapped to `<down>' using
`input-decode-map'.  When the user types ESC ESC ESC O B, the last ESC
could be the start of an ESC O B escape sequence.  But once the keys read so
far have a complete binding, `read-key-sequence' returns immediately
even if `input-decode-map' has not yet scanned the tail."
  ;; feed C-c C-c C-c a b -> [C-c C-c C-c]; trailing a b is left
  ;; unscanned
  (let ((map (make-sparse-keymap))
        (input-decode-map (make-sparse-keymap)))
    (define-key input-decode-map [?\C-c ?a ?b] [f99])
    (define-key map [?\C-c ?\C-c ?\C-c] 'ignore)
    (define-key map [f99] 'undo)
    (let ((obs (keyboard-tests--rks-observe
                (list ?\C-c ?\C-c ?\C-c ?a ?b) map)))
      (should (equal [?\C-c ?\C-c ?\C-c] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding))))))

(ert-deftest keyboard-tests-rks-decode-fires-without-binding ()
  "With no prefix binding `input-decode-map' can translate the tail."
  ;; feed C-c C-c C-c a b -> [C-c C-c f99]; trailing a b is translated
  (let ((map (make-sparse-keymap))
        (input-decode-map (make-sparse-keymap)))
    (define-key input-decode-map [?\C-c ?a ?b] [f99])
    (define-key map [?\C-c ?\C-c f99] 'ignore)
    (let ((obs (keyboard-tests--rks-observe
                (list ?\C-c ?\C-c ?\C-c ?a ?b) map)))
      (should (equal [?\C-c ?\C-c f99] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding))))))

;;;; Translation pipeline
;;
;; `read_key_sequence' runs three translation machines (in THIS order):
;; `input-decode-map', then `local-function-key-map'/`function-key-map'
;; parent), then `key-translation-map'.
;;
;; These are not the same!
;;
;; 1. `input-decode-map' can rewrite raw input even when a binding exists;
;;
;; 2. `function-key-map' translation is only allowed when the sequence is undefined;
;;
;; 3. `key-translation-map' runs last and can rescan the result of
;; function-key translation.
;;
;; Example: if raw `C-c a' is decoded to `C-c b', function-key
;; translation must not look inside `C-c b' again.  But if function-key
;; translation rewrites `C-c a' to `C-c b', `key-translation-map' is
;; still allowed to turn that into `C-c c'.

(ert-deftest keyboard-tests-rks-input-decode-map ()
  "Input-decode rewrites a raw sequence before lookup."
  ;; C-c a -> [C-c b]
  (let ((map (make-sparse-keymap))
        (input-decode-map (make-sparse-keymap)))
    (define-key map [?\C-c ?b] 'ignore)
    (define-key input-decode-map [?\C-c ?a] [?\C-c ?b])
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
      (should (equal [?\C-c ?b] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding))))))

(ert-deftest keyboard-tests-rks-function-key-map ()
  "Check that `local-function-key-map' rewrites undefined bindings."
  ;;  C-c a -> [C-c b].
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?b] 'ignore)
    (keyboard-tests--rks-with-function-key-bindings
     '(([?\C-c ?a] . [?\C-c ?b]))
     (lambda ()
       (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
         (should (equal [?\C-c ?b] (plist-get obs :sequence)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-function-key-map-on-undefined ()
  "An explicit binding still allows function-key translation."
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'undefined)
    (define-key map [?\C-c ?b] 'ignore)
    (keyboard-tests--rks-with-function-key-bindings
     '(([?\C-c ?a] . [?\C-c ?b]))
     (lambda ()
       (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
         (should (equal [?\C-c ?b] (plist-get obs :sequence)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-input-decode-before-function-key ()
  "Check that input decode happens before function key translation."
  ;; input-decode-map rewrites C-c a -> C-c b; function-key-map cannot
  ;; rescan the translated positions to apply C-c b -> C-c c.
  (let ((map (make-sparse-keymap))
        (input-decode-map (make-sparse-keymap)))
    (define-key map [?\C-c ?c] 'ignore)
    (define-key input-decode-map [?\C-c ?a] [?\C-c ?b])
    (keyboard-tests--rks-with-function-key-bindings
     '(([?\C-c ?b] . [?\C-c ?c]))
     (lambda ()
       (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
         (should (equal [?\C-c ?b] (plist-get obs :sequence))))))))

(ert-deftest keyboard-tests-rks-function-key-skipped-when-bound ()
  "A real binding should win over `function-key-map'."
  ;; C-a a -> [C-a a] (no translation)
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'ignore)
    (define-key map [?\C-c ?b] 'undo)
    (keyboard-tests--rks-with-function-key-bindings
     '(([?\C-c ?a] . [?\C-c ?b]))
     (lambda ()
       (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
         (should (equal [?\C-c ?a] (plist-get obs :sequence)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-key-translation-after-function-key ()
  "Check that `key-translation-map' translates after `function-key-map'."
  ;; function-key-map rewrites C-c a -> C-c b; key-translation-map
  ;; rescans the translated positions to apply C-c b -> C-c c.
  (let ((map (make-sparse-keymap))
        (key-translation-map (make-sparse-keymap)))
    (define-key map [?\C-c ?c] 'ignore)
    (define-key key-translation-map [?\C-c ?b] [?\C-c ?c])
    (keyboard-tests--rks-with-function-key-bindings
     '(([?\C-c ?a] . [?\C-c ?b]))
     (lambda ()
       (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
         (should (equal [?\C-c ?c] (plist-get obs :sequence)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-key-translation-overrides-bound-sequence ()
  "The final translation layer can override a bound sequence."
  (let ((map (make-sparse-keymap))
        (key-translation-map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'ignore)
    (define-key map [?\C-c ?b] 'undo)
    (define-key key-translation-map [?\C-c ?a] [?\C-c ?b])
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?a) map)))
      (should (equal [?\C-c ?b] (plist-get obs :sequence)))
      (should (eq 'undo (plist-get obs :binding))))))

;;;; Postprocessing and fallbacks

(ert-deftest keyboard-tests-rks-upcase-fallback ()
  "Check that uppercase fallback works."
  ;; C-c A -> [C-c a]
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'ignore)
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?A) map)))
      (should (equal [?\C-c ?a] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding)))
      (should (plist-get obs :shift-translated)))))

(ert-deftest keyboard-tests-rks-no-upcase-fallback ()
  "Check that disabling uppercase translation works."
  ;; C-c A -> [C-c A] (with translate-upper-case-key-bindings set to t)
  (let ((map (make-sparse-keymap))
        (translate-upper-case-key-bindings nil))
    (define-key map [?\C-c ?a] 'ignore)
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?A) map)))
      (should (equal [?\C-c ?A] (plist-get obs :sequence)))
      (should-not (plist-get obs :shift-translated)))))

(ert-deftest keyboard-tests-rks-dont-downcase-last ()
  "Uppercase is restored after a lowercase lookup with dont-downcase-last."
  ;; C-c A -> [C-c A] (note the dont-downcase-last arg)
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?a] 'ignore)
    (let ((obs (keyboard-tests--rks-observe (list ?\C-c ?A) map t)))
      (should (equal [?\C-c ?A] (plist-get obs :sequence)))
      (should-not (plist-get obs :shift-translated)))))

(ert-deftest keyboard-tests-rks-shifted-function-key-fallback ()
  "Shifted function-key fallback works."
  ;; S-f2 to [f2]
  (let ((map (make-sparse-keymap)))
    (define-key map [f2] 'ignore)
    (let ((obs (keyboard-tests--rks-observe (list 'S-f2) map)))
      (should (equal [f2] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding)))
      (should (plist-get obs :shift-translated)))))

;;;; Frames, buffers, mouse events, help

(ert-deftest keyboard-tests-rks-switch-frame-delayed ()
  "A mid-sequence switch-frame is delayed, so the read is finished."
  ;; C-c (switch-event ..) b -> [C-c b]
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?b] 'ignore)
    (let* ((switch-event (list 'switch-frame (selected-frame)))
           (obs (keyboard-tests--rks-observe
                 (list ?\C-c switch-event ?b) map)))
      (should (equal [?\C-c ?b] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding)))
      ;; the (switch-frame ...) event is postponed but not lost
      (let ((unread-command-events nil))
        (should (equal switch-event (read-event nil nil 0.01)))))))

(ert-deftest keyboard-tests-rks-unbound-down-mouse-dropped ()
  "Drop an unbound down-mouse event and read the following key."
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] 'ignore)
    (let* ((down (list 'down-mouse-9
                       (keyboard-tests--rks-fake-posn (selected-window))))
           (obs (keyboard-tests--rks-observe (list down ?a) map)))
      (should (equal [?a] (plist-get obs :sequence)))
      (should (eq 'ignore (plist-get obs :binding))))))

(ert-deftest keyboard-tests-rks-mode-line-fake-prefix ()
  "A mode-line click gains the fake prefix `mode-line'."
  (keyboard-tests--rks-with-event-kinds
   '((mouse-1 . mouse-click))
   (lambda ()
     (let ((map (make-sparse-keymap)))
       (define-key map [mode-line mouse-1] 'ignore)
       (let* ((click (list 'mouse-1
                           (keyboard-tests--rks-fake-posn
                            (selected-window) 'mode-line)))
              (obs (keyboard-tests--rks-observe (list click) map))
              (seq (plist-get obs :sequence)))
         (should (= 2 (length seq)))
         (should (eq 'mode-line (aref seq 0)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-mode-line-fake-prefix-continued ()
  "After fake-prefix insertion, the next key continues in that sub-map."
  (keyboard-tests--rks-with-event-kinds
   '((mouse-1 . mouse-click))
   (lambda ()
     (let ((map (make-sparse-keymap)))
       (define-key map [mode-line mouse-1 ?a] 'ignore)
       (let* ((click (list 'mouse-1
                           (keyboard-tests--rks-fake-posn
                            (selected-window) 'mode-line)))
              (obs (keyboard-tests--rks-observe (list click ?a) map))
              (seq (plist-get obs :sequence)))
         (should (= 3 (length seq)))
         (should (eq 'mode-line (aref seq 0)))
         (should (eq 'mouse-1 (car-safe (aref seq 1))))
         (should (eq ?a (aref seq 2)))
         (should (eq 'ignore (plist-get obs :binding))))))))

(ert-deftest keyboard-tests-rks-clicked-buffer-switch ()
  "A first mouse click in another window selects that window's local map."
  (keyboard-tests--rks-with-event-kinds
   '((mouse-1 . mouse-click))
   (lambda ()
     (let ((map-a (make-sparse-keymap))
           (map-b (make-sparse-keymap))
           (called nil))
       (keyboard-tests--rks-call-with-live-window-maps
        map-a map-b
        (lambda (_buffer-a window-a _buffer-b window-b)
          (define-key map-a [mouse-1]
                      (lambda () (interactive) (setq called 'a)))
          (define-key map-b [mouse-1]
                      (lambda () (interactive) (setq called 'b)))
          (select-window window-a)
          (keyboard-tests--rks-execute
           (list (list 'mouse-1
                       (keyboard-tests--rks-fake-posn window-b))))
          (should (eq 'b called))))))))

(ert-deftest keyboard-tests-rks-help-char ()
  "Help-char read mid-sequence should end the read."
  (let* ((map (make-sparse-keymap))
         (called nil)
         (prefix-help-command (lambda () (interactive) (setq called t))))
    (define-key map [?\C-c ?x] 'ignore)
    (keyboard-tests--rks-execute (list ?\C-c ?\C-h) map)
    (should called)))

(provide 'keyboard-tests)
;;; keyboard-tests.el ends here
