;;; low-level-key.el --- Handling of key press/release events    -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

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

;; Emacs can send low-level key events, that correspond to key presses
;; and releases. These events are by default disabled.

;; When enabled by setting `enable-low-level-key-events' to a non-nil
;; value, emacs will begin sending them, and they will be ignored as
;; low-level-key is bound to `ignore' on `special-event-map'.

;; This file sets a handler for them (`llk-handle') which generates
;; input events for key presses, key releases and double and triple
;; taps.  These events can be bound to commands on normal keymaps.

;; Because generating these events for all keys would interfere with
;; normal keyboard input, they must be activated individually by calling
;; the function `llk-bind'.

;; The low-level-key event payload is described by the 'low-level-key'
;; struct.  Use 'cl-describe-type' to get more information about it.
;;
;; After loading this file and setting a non-nil value for
;; 'enable-low-level-key-events', events begin to be handled by
;; 'llk-handle', which tries to detect n-taps, and creates input
;; events. See 'llk-bind'.

;; Code:

(require 'cl-lib)

(defvar llk-tap-timeout (let ((time (mouse-double-click-time)))
                          (if (> time 0) time 800))
  "Time in milliseconds between key presses/releases to consider a double tap.
For triple taps, the time is twice this value.")

(cl-defstruct (low-level-key (:type list))
  "Structure for low level key events.
Received as low-level-key on `special-event-map'."
  (event-type nil :type symbol
   :documentation "Type of event: low-level-key")
  (is-key-press nil :type boolean
   :documentation "t if the key has been pressed, nil if it has been released.")
  (key nil :type integer
   :documentation "The keysym number.")
  (modifier nil :type symbol
   :documentation "Modifier associated with this key.  It is nil if the key is
not a modifier.  It can be one of the following symbols: shift, control, meta,
super, hyper, alt.  It can also be t if the key is a modifier but it can't be
identified, as in the PGTK backend.")
  (time nil :type integer
   :documentation "Timestamp in milliseconds of the event.")
  (frame nil :type frame
   :documentation "Frame where the event happened."))

(defvar llk-bindings nil
  "List of bindings for low level key events (press/release/tap).

Use the `llk-bind' function to add bindings.  See its documentation for
a description of the binding information.")

(defvar llk-keysyms nil
  "List of keysyms and their names.
Each element has the form (CODE . KEYSYM), where code is a NUMBER and
KEYSYM a symbol, such as `xk-shift-l'")

;; TODO docstring.
(defun llk-bind (key &rest events)
  "Activates low level event generation for a key.

KEY can be a number or a symbol.  The symbols `shift', `control',
`meta', `super', `hyper', `alt' activate events for the corresponding
modifier keys.  A number activates events for the corresponding KeySym.

EVENTS are symbols that activate one event. Possible values are `press',
`release', `double' and `triple'.

See `llk-keysyms' for a list of known values for KEY and their names.
For each of those, there is a corresponding variable.  It is better to
use the variables to specify keys, as numerical values are
platform-dependent.  The names are parallel to those for KeySyms on X,
as defined in `xkeysymdef.h'. For example, `XK_Shift_L' (the left shift
key), corresponds to `xk-shift-l'.

The `xkeysymdef.h' file defines different KeySyms for capital and small
versions of latin letters.  For this event, only the capital version is
used, with the variables `xk-a', `xk-b', etc.

Low level key events must be enabled with the variable
`enable-low-level-key-events'.

Once a key is activated with this function, input events will be
generated for them, and can be bound to commands using normal keymaps.

For example, activating the double tap for the left shift key:

  (llk-bind xk-shift-l \\='double)

will generate the event `double-xk-shift-l', than can be bound to a
command with:

  (keymap-global-set [double-xk-shift-l] COMMAND)

Prefixes for events are `press-key-', `release-key-', `double-' and
`triple-'.

If you use a KeySym number that is not on `llk-keysyms', the events will
use its numerical value."
  (setq llk-bindings
        (cl-delete-if (lambda (x) (eq (car x) key)) llk-bindings))
  (push (append (list key) events) llk-bindings))

;; We store the last events (key/modifier is-press timestamp) here to
;; test for multitap.  This is not the use the low-level-key struct.
(defvar llk--event-history-for-tap nil
  "Internal variable for detecting taps.")

(defun llk--detect-n-tap (n timeout)
  "Internal function to detect n-tap keys."
  ;; Only care about last 2xN events
  (ntake (* 2 n) llk--event-history-for-tap)
  ;; If we have:
  ;; - Exactly 2 * n events.
  ;; - down, up, down, up, ...
  ;; - not two much time between first and last
  (and (eq (* 2 n) (length llk--event-history-for-tap))
       (cl-every #'eq
                 (ntake (* 2 n)
                        (list nil t nil t nil t nil t
                              nil t nil t nil t nil t))
                 (mapcar 'cl-second llk--event-history-for-tap))
       (< (- (cl-third (cl-first llk--event-history-for-tap))
             (cl-third (car (last llk--event-history-for-tap))))
          timeout)
       (progn (setq llk--event-history-for-tap nil) t)))

(defun llk--generate-event (key event-type)
  (when (numberp key)
    (let ((sym (cdr (assoc key llk-keysyms))))
      (when sym
        (setq key sym))))
  (push (intern (format "%s-%s" event-type key))
                 unread-command-events))

(defun llk--generate-events (key is-press binding timestamp)
  (if is-press
      (when (member 'press binding)
        (llk--generate-event key 'press-key))
    (when (member 'release binding)
      (llk--generate-event key 'release-key)))

  (let ((double (member 'double binding))
        (triple (member 'triple binding)))
    ;; a non-tap key clears the event history.
    (if (or double triple)
        (progn
          ;; Clear the event history if it has events from another key.
          (unless (equal (car (car llk--event-history-for-tap)) key)
            (setq llk--event-history-for-tap nil))
          (push (list key is-press timestamp) llk--event-history-for-tap)
          (and double
               (llk--detect-n-tap 2 llk-tap-timeout)
               (llk--generate-event key 'double-key))
          (and triple
               (llk--detect-n-tap 3 (* 2 llk-tap-timeout))
               (llk--generate-event key 'triple-key)))
      ;; A non-tap key clears the event history.
      (setq llk--event-history-for-tap nil))))

(defun llk-handle ()
  (interactive)
  (let* ((key (low-level-key-key last-input-event))
         (modifier (low-level-key-modifier last-input-event))
         (timestamp (low-level-key-time last-input-event))
         (is-press (low-level-key-is-key-press last-input-event))
         (binding (assoc key llk-bindings))
         (binding-modifier (assoc modifier llk-bindings)))
    (if binding
        (llk--generate-events key is-press binding timestamp)
      (when binding-modifier
        (llk--generate-events modifier is-press binding-modifier timestamp)))))

(defmacro llk--define-xk (name x-keysym w32-keysym)
  "Internal macro to define keysyms."
  `(let ((ksym (pcase (window-system)
                 ((or 'pgtk 'x) ,x-keysym)
                 ('w32 ,w32-keysym))))
     (defconst ,name ksym "Constant for a keysym value.")
     (push (cons ksym ',name) llk-keysyms)))

(defun llk--define-keysyms ()
  "Initialize the keysym list, `llk-keysyms'."
  (setq llk-keysyms nil)

  ;; tty keys
  (llk--define-xk xk-backspace   #xff08 #x08) ;; XK_BackSpace VK_BACK
  (llk--define-xk xk-tab         #xff09 #x09) ;; XK_Tab VK_TAB
  (llk--define-xk xk-clear       #xff0b #x0C) ;; XK_Clear VK_CLEAR
  (llk--define-xk xk-return      #xff0d #x0D) ;; XK_Return VK_RETURN
  (llk--define-xk xk-pause       #xff13 #x13) ;; XK_Pause VK_PAUSE
  (llk--define-xk xk-scroll-lock #xff14 #x91) ;; XK_Scroll_Lock VK_SCROLL
  (llk--define-xk xk-escape      #xff1B #x1B) ;; XK_Escape VK_ESCAPE
  (llk--define-xk xk-delete      #xffff #x2E) ;; XK_Delete VK_DELETE

  ;; Cursor control and motion
  (llk--define-xk xk-home        #xff50 #x24) ;; XK_Home VK_HOME
  (llk--define-xk xk-left        #xff51 #x25) ;; XK_Left VK_LEFT
  (llk--define-xk xk-up          #xff52 #x26) ;; XK_Up VK_UP
  (llk--define-xk xk-right       #xff53 #x27) ;; XK_Right VK_RIGHT
  (llk--define-xk xk-down        #xff54 #x28) ;; XK_Down VK_DOWN
  (llk--define-xk xk-page-up     #xff55 #x21) ;; XK_Page_Up VK_PRIOR
  (llk--define-xk xk-page-down   #xff56 #x22) ;; XK_Page_Down VK_NEXT
  (llk--define-xk xk-end         #xff57 #x23) ;; XK_End VK_END
  (llk--define-xk xk-begin       #xff58 #x24) ;; XK_Begin VK_HOME

  ;; Special Windows keyboard keys
  (llk--define-xk xk-win-l       #xFF5B #x5B) ;; XK_Win_L VK_LWIN
  (llk--define-xk xk-win-r       #xFF5C #x5C) ;; XK_Win_R VK_RWIN
  (llk--define-xk xk-app         #xFF5D #x5D) ;; XK_App VK_APPS

  ;; Misc functions
  (llk--define-xk xk-select      #xff60 #x29) ;; XK_Select VK_SELECT
  (llk--define-xk xk-print       #xff61 #x2A) ;; XK_Print VK_PRINT
  (llk--define-xk xk-insert      #xff64 #x2D) ;; XK_Insert VK_INSERT
  (llk--define-xk xk-num-lock    #xff7f #x90) ;; XK_Num_Lock VK_NUMLOCK

  ;; Keypad
  ;; TODO: Check values for MS-Windows
  (llk--define-xk xk-kp-enter    #xff8d nil)  ;; XK_KP_Enter ???
  (llk--define-xk xk-kp-multiply #xffaa nil)  ;; XK_KP_Multiply ???
  (llk--define-xk xk-kp-add      #xffab nil)  ;; XK_KP_Add ???
  (llk--define-xk xk-kp-subtract #xffad nil)  ;; XK_KP_Subtract ???
  (llk--define-xk xk-kp-decimal  #xffae nil)  ;; XK_KP_Decimal ???
  (llk--define-xk xk-kp-divide   #xffaf nil)  ;; XK_KP_Divide ???
  (llk--define-xk xk-kp-0        #xffb0 #x60) ;; XK_KP_0 VK_NUMPAD0
  (llk--define-xk xk-kp-1        #xffb1 #x61) ;; XK_KP_1 VK_NUMPAD1
  (llk--define-xk xk-kp-2        #xffb2 #x62) ;; XK_KP_2 VK_NUMPAD2
  (llk--define-xk xk-kp-3        #xffb3 #x63) ;; XK_KP_3 VK_NUMPAD3
  (llk--define-xk xk-kp-4        #xffb4 #x64) ;; XK_KP_4 VK_NUMPAD4
  (llk--define-xk xk-kp-5        #xffb5 #x65) ;; XK_KP_5 VK_NUMPAD5
  (llk--define-xk xk-kp-6        #xffb6 #x66) ;; XK_KP_6 VK_NUMPAD6
  (llk--define-xk xk-kp-7        #xffb7 #x67) ;; XK_KP_7 VK_NUMPAD7
  (llk--define-xk xk-kp-8        #xffb8 #x68) ;; XK_KP_8 VK_NUMPAD8
  (llk--define-xk xk-kp-9        #xffb9 #x69) ;; XK_KP_9 VK_NUMPAD9

  ;; Function keys
  (llk--define-xk xk-f1          #xffbe #x70) ;; XK_F1 VK_F1
  (llk--define-xk xk-f2          #xffbf #x71) ;; XK_F2 VK_F2
  (llk--define-xk xk-f3          #xffc0 #x72) ;; XK_F3 VK_F3
  (llk--define-xk xk-f4          #xffc1 #x73) ;; XK_F4 VK_F4
  (llk--define-xk xk-f5          #xffc2 #x74) ;; XK_F5 VK_F5
  (llk--define-xk xk-f6          #xffc3 #x75) ;; XK_F6 VK_F6
  (llk--define-xk xk-f7          #xffc4 #x76) ;; XK_F7 VK_F7
  (llk--define-xk xk-f8          #xffc5 #x77) ;; XK_F8 VK_F8
  (llk--define-xk xk-f9          #xffc6 #x78) ;; XK_F9 VK_F9
  (llk--define-xk xk-f10         #xffc7 #x79) ;; XK_F10 VK_F10
  (llk--define-xk xk-f11         #xffc8 #x7A) ;; XK_F11 VK_F11
  (llk--define-xk xk-f12         #xffc9 #x7B) ;; XK_F12 VK_F12
  (llk--define-xk xk-f13         #xffca #x7C) ;; XK_F13 VK_F13
  (llk--define-xk xk-f14         #xffcb #x7D) ;; XK_F14 VK_F14
  (llk--define-xk xk-f15         #xffcc #x7E) ;; XK_F15 VK_F15
  (llk--define-xk xk-f16         #xffcd #x7F) ;; XK_F16 VK_F16
  (llk--define-xk xk-f17         #xffce #x80) ;; XK_F17 VK_F17
  (llk--define-xk xk-f18         #xffcf #x81) ;; XK_F18 VK_F18
  (llk--define-xk xk-f19         #xffd0 #x82) ;; XK_F19 VK_F19
  (llk--define-xk xk-f20         #xffd1 #x83) ;; XK_F20 VK_F20
  (llk--define-xk xk-f21         #xffd2 #x84) ;; XK_F21 VK_F21
  (llk--define-xk xk-f22         #xffd3 #x85) ;; XK_F22 VK_F22
  (llk--define-xk xk-f23         #xffd4 #x86) ;; XK_F23 VK_F23
  (llk--define-xk xk-f24         #xffd5 #x87) ;; XK_F24 VK_F24

  ;; Modifier keys
  (llk--define-xk xk-shift-l     #xffe1 #xA0) ;; XK_Shift_L VK_LSHIFT
  (llk--define-xk xk-shift-r     #xffe2 #xA1) ;; XK_Shift_R VK_RSHIFT
  (llk--define-xk xk-control-l   #xffe3 #xA2) ;; XK_Control_L VK_LCONTROL
  (llk--define-xk xk-control-r   #xffe4 #xA3) ;; XK_Control_R VK_RCONTROL
  (llk--define-xk xk-caps-lock   #xffe5 #x14) ;; XK_Caps_Lock VK_CAPITAL
  (llk--define-xk xk-meta-l      #xffe7 nil)  ;; XK_Meta_L
  (llk--define-xk xk-meta-r      #xffee nil)  ;; XK_Meta_R
  (llk--define-xk xk-alt-l       #xffe9 #xA4) ;; XK_Alt_L VK_LMENU
  (llk--define-xk xk-alt-r       #xffea #xA5) ;; XK_Alt_R VK_RMENU
  (llk--define-xk xk-super-l     #xffeb nil)  ;; XK_Super_L
  (llk--define-xk xk-super-r     #xffec nil)  ;; XK_Super_R
  (llk--define-xk xk-hyper-l     #xffed nil)  ;; XK_Hyper_L
  (llk--define-xk xk-hyper-r     #xffee nil)  ;; XK_Hyper_R

  ;; Latin 1
  ;; For numbers and letters, MS-Windows does not define constant names.
  ;; X11 defines distinct keysyms for lowercase and uppercase
  ;; letters.  We use only the uppercase ones.  Events with lowercase
  ;; letters are converted to uppercase.
  (llk--define-xk xk-space       #x0020 #x20) ;; XK_space VK_SPACE
  (llk--define-xk xk-0           #x0030 #x30) ;; XK_0
  (llk--define-xk xk-1           #x0031 #x31) ;; XK_1
  (llk--define-xk xk-2           #x0032 #x32) ;; XK_2
  (llk--define-xk xk-3           #x0033 #x33) ;; XK_3
  (llk--define-xk xk-4           #x0034 #x34) ;; XK_4
  (llk--define-xk xk-5           #x0035 #x35) ;; XK_5
  (llk--define-xk xk-6           #x0036 #x36) ;; XK_6
  (llk--define-xk xk-7           #x0037 #x37) ;; XK_7
  (llk--define-xk xk-8           #x0038 #x38) ;; XK_8
  (llk--define-xk xk-9           #x0039 #x39) ;; XK_9
  (llk--define-xk xk-a           #x0041 #x41) ;; XK_A
  (llk--define-xk xk-b           #x0042 #x42) ;; XK_B
  (llk--define-xk xk-c           #x0043 #x43) ;; XK_C
  (llk--define-xk xk-d           #x0044 #x44) ;; XK_D
  (llk--define-xk xk-e           #x0045 #x45) ;; XK_E
  (llk--define-xk xk-f           #x0046 #x46) ;; XK_F
  (llk--define-xk xk-g           #x0047 #x47) ;; XK_G
  (llk--define-xk xk-h           #x0048 #x48) ;; XK_H
  (llk--define-xk xk-i           #x0049 #x49) ;; XK_I
  (llk--define-xk xk-j           #x004A #x4A) ;; XK_J
  (llk--define-xk xk-k           #x004B #x4B) ;; XK_K
  (llk--define-xk xk-l           #x004C #x4C) ;; XK_L
  (llk--define-xk xk-m           #x004D #x4D) ;; XK_M
  (llk--define-xk xk-n           #x004E #x4E) ;; XK_N
  (llk--define-xk xk-o           #x004F #x4F) ;; XK_O
  (llk--define-xk xk-p           #x0050 #x50) ;; XK_P
  (llk--define-xk xk-q           #x0051 #x51) ;; XK_Q
  (llk--define-xk xk-r           #x0052 #x52) ;; XK_R
  (llk--define-xk xk-s           #x0053 #x53) ;; XK_S
  (llk--define-xk xk-t           #x0054 #x54) ;; XK_T
  (llk--define-xk xk-u           #x0055 #x55) ;; XK_U
  (llk--define-xk xk-v           #x0056 #x56) ;; XK_V
  (llk--define-xk xk-w           #x0057 #x57) ;; XK_W
  (llk--define-xk xk-x           #x0058 #x58) ;; XK_X
  (llk--define-xk xk-y           #x0059 #x59) ;; XK_Y
  (llk--define-xk xk-z           #x005A #x5A));; XK_Z

(defun describe-low-level-key ()
  "Wait for key press and describe the low-level key event it generates."
  (interactive)
  (define-key special-event-map [low-level-key] 'llk--describe))

(defun llk--describe ()
  "Internal function for `special-event-map' to describe low level key events."
  (interactive)
  (when (low-level-key-is-key-press last-input-event)
    (define-key special-event-map [low-level-key] 'llk-handle)
    (with-help-window (help-buffer)
      (insert "\n")
      (let* ((xk (low-level-key-key last-input-event))
             (sym (assoc xk llk-keysyms)))
        (insert (format "Keysym number: %d (#x%X),\n" xk xk))
        (if sym
            (insert (format "which corresponds to named key %s.\n\n" (cdr sym)))
          (insert "which does not correspond to any known named key.\n\n"))
        (if (low-level-key-modifier last-input-event)
            (insert (format "This key corresponds to the %s modifier.\n\n"
                            (low-level-key-modifier last-input-event)))
          (insert "This key does not correspond to a modifier.\n\n"))
        (insert "See the value of the `llk-keysyms' variable for a list of known keys.\n")))))

(llk--define-keysyms)
(define-key special-event-map [low-level-key] 'llk-handle)
(setq llk-bindings nil)
