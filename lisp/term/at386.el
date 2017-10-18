;;; Dell 325D (UNIX SVR4) as AT386 UNIX PC keyboard definitions
;;; Based on Brant Cheikes (brant@linc.cis.upenn.edu, manta!brant)
;;; unixpc.el.
;;;
;;; Mark J. Hewitt (mjh@uk.co.kernel)
;;; 8-apr-91
;;;
;;; The AT386 keyboard mapping has three types of prefix keys:
;;;
;;; <esc> [ for cursor positioning and keypad
;;; <esc> O for function keys
;;; <esc> N for ALT keys 
;;;
;;;   *NOTE* Care is required when using ALT bound as a simple META key.
;;;          It works for most normal key sequences, but some ALT-CTRL
;;;          (aka M-C-x) are intercepted locally.  F'rinstance M-C-d would
;;;          break to the kernel debugger, kdb (!).
;;;

(require 'keypad)			; for keypad-defaults

(defvar Dell-map-1 nil
  "The <esc>O keys (Function) on the Dell Unix PC.")
(defvar Dell-map-3 nil
  "The <esc>[ keys (Right-hand keypads) on the Dell Unix PC.")

(defun enable-function-keys ()
  "Enable the use of the keypad and function keys.
Because of the nature of the PC keyboard under Unix,
this unavoidably breaks a standard Emacs command (M-[);
therefore, it is not done by default, but only if you give this command."
  (interactive)
  (global-set-key "\eO" Dell-map-1)
  (global-set-key "\eN" 'ESC-prefix)
  (global-set-key "\e[" Dell-map-3)
)

;;; Create a few new keypad defaults.

(keypad-default "5" 'set-mark-command)
(keypad-default "I" 'yank)
(keypad-default "x" 'call-last-kbd-macro)
(keypad-default "\C-f" 'info)
(keypad-default "\C-g" 'overwrite-mode)
(keypad-default "\C-h" 'auto-fill-mode)
(keypad-default "\C-i" 'abbrev-mode)
(keypad-default "\C-j" 'browse-yank)
; There are no definitions for these functions.
;(keypad-default "\C-l" 'Dell-132)
;(keypad-default "\C-m" 'Dell-80)
(keypad-default "\C-n" 'scroll-other-window)
(keypad-default "\C-o" 'other-window)
(keypad-default "\C-p" 'repeat-complex-command)

;; Now populate the maps, if they are enabled.

(if Dell-map-1
    nil
  (setq Dell-map-1 (make-keymap))   ; <ESC>O (function key) commands
  (setup-terminal-keymap Dell-map-1
			 '(("P" . ??)   ; F1 (help)
			   ("p" . ?\^f) ; Shift F1 (info)
			   ("Q" . ?\^g) ; F2  (overwrite-mode)
			   ("q" . ?\^g) ; Shift F2  (overwrite-mode)
			   ("R" . ?\^h) ; F3  (auto-fill-mode)
			   ("r" . ?\^h) ; Shift F3  (auto-fill-mode)
			   ("S" . ?\^i) ; F4  (abbrev-mode)
			   ("s" . ?\^i) ; Shift F4  (abbrev-mode)
			   ("T" . ?\^j) ; F5  (browse-yank)
			   ("t" . ?\^j) ; Shift F5  (browse-yank)
			   ("U" . ?\^l) ; F6  (Dell-132)
			   ("u" . ?\^m) ; Shift F6  (Dell-80)
			   ("V" . nil)  ; F7
			   ("v" . nil)  ; Shift F7
			   ("W" . ?\^n) ; F8  (scroll-other-window)
			   ("w" . ?\^o) ; Shift F8  (other-window)
			   ("X" . nil)  ; F9
			   ("x" . nil)  ; Shift F9
			   ("Y" . nil)  ; F10
			   ("y" . nil)  ; Shift F10
			   ("Z" . ?\^p) ; F11 (repeat-complex-command)
			   ("z" . ?\^p) ; Shift F11 (repeat-complex-command)
			   ("A" . ?x)   ; F12 (call-last-kbd-macro)
			   ("a" . ?x)   ; Shift F12 (call-last-kbd-macro)
			   )))

(if Dell-map-3
    nil
  (setq Dell-map-3 (make-sparse-keymap))   ; <ESC>[ commands
  (setup-terminal-keymap Dell-map-3
			 '(("A" . ?u)	; Up Arrow (previous-line)
			   ("B" . ?d)	; Down Arrow (next-line)
			   ("C" . ?r)	; Right Arrow (forward-char)
			   ("D" . ?l)	; Left Arrow (backward-char)
			   ("H" . ?\^a)	; Home (beginning-of-line)
			   ("Y" . ?\^b) ; End (end-of-line)
			   ("@" . ?I)	; Insert (yank)
			   ("U" . ?N)	; Page Up (scroll-up)
			   ("V" . ?P)   ; Shift-Page (scroll-down)
			   ("G" . ?5)   ; pad 5 (set-mark-command)
			   )))
