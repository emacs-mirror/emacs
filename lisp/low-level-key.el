;;; -*- lexical-binding: t -*-

;; The physical-key event is like this:
;; (physical-key IS-KEY-PRESS KEY MODIFIER TIME FRAME)
;; IS-KEY-PRESS is t if the key has been pressed, nil if it has been released.
;; KEY is the keysym number.
;; MODIFIER is the modifier associated with this key. It is nil if the key is
;; not a modifier. It can be one of the following symbols: shift, control, meta,
;; super, hyper, alt. It can also be t if the key is a modifier but it can't be
;; identified.
;; TIME is the timestamp in milliseconds of the event.
;; FRAME is the frame where the event happened.
;;
;; After calling 'llk-init' and setting a non-nil value for
;; 'enable-low-level-key-events', events begin to be handled by 'llk-handler',
;; which tries to detect n-taps and calls the corresponding function.

(require 'cl-lib)

;; User options

(defvar llk-bindings nil
  "Bindings for low level key events (press/release/tap).
Use the `llk-bind' function to add bindings.  See its documentation for
a description of the binding information.")

(defvar llk-tap-count 2
  "Number or key press/releases to consider a tap.")

(defvar llk-tap-timeout 1000
  "Time in milliseconds between consecutive key presses/releases to
consider a tap.")

(defvar llk-tap-keys
  '(xk-shift-l xk-shift-r xk-control-l xk-control-r meta)
  "Keys that can generate taps.")

(defvar llk-keysyms nil
  "List of keysym numbers and their corresponding symbols.
Each element has the form (KEYSYM . SYMBOL).  The variable value for
each symbol is the keysym.  This list is initialized by `llk-init'.")

(defvar llk-describe-next-press nil
  "Internal variable to mark that next key press should be described.")

(defmacro define-xk (name x-keysym w32-keysym)
  "Internal macro to define keysyms."
  `(let ((ksym (pcase (window-system)
                 ('pgtk ,x-keysym)
                 ('x ,x-keysym)
                 ('w32 ,w32-keysym))))
     (defconst ,name ksym "Constant for a keysym value.")
     (push (cons ksym ',name) llk-keysyms)))

(defun llk-define-keysyms ()
  "Initialize the keysym list, `llk-keysyms'.  Called from `llk-init'."
  (setq llk-keysyms nil)

  ;; tty keys
  (define-xk xk-backspace   #xff08 #x08) ;; XK_BackSpace VK_BACK
  (define-xk xk-tab         #xff09 #x09) ;; XK_Tab VK_TAB
  (define-xk xk-clear       #xff0b #x0C) ;; XK_Clear VK_CLEAR
  (define-xk xk-return      #xff0d #x0D) ;; XK_Return VK_RETURN
  (define-xk xk-pause       #xff13 #x13) ;; XK_Pause VK_PAUSE
  (define-xk xk-scroll-lock #xff14 #x91) ;; XK_Scroll_Lock VK_SCROLL
  (define-xk xk-escape      #xff1B #x1B) ;; XK_Escape VK_ESCAPE
  (define-xk xk-delete      #xffff #x2E) ;; XK_Delete VK_DELETE

  ;; Cursor control and motion
  (define-xk xk-home        #xff50 #x24) ;; XK_Home VK_HOME
  (define-xk xk-left        #xff51 #x25) ;; XK_Left VK_LEFT
  (define-xk xk-up          #xff52 #x26) ;; XK_Up VK_UP
  (define-xk xk-right       #xff53 #x27) ;; XK_Right VK_RIGHT
  (define-xk xk-down        #xff54 #x28) ;; XK_Down VK_DOWN
  (define-xk xk-page-up     #xff55 #x21) ;; XK_Page_Up VK_PRIOR
  (define-xk xk-page-down   #xff56 #x22) ;; XK_Page_Down VK_NEXT
  (define-xk xk-end         #xff57 #x23) ;; XK_End VK_END
  (define-xk xk-begin       #xff58 #x24) ;; XK_Begin VK_HOME

  ;; Special Windows keyboard keys
  (define-xk xk-win-l       #xFF5B #x5B) ;; XK_Win_L VK_LWIN
  (define-xk xk-win-r       #xFF5C #x5C) ;; XK_Win_R VK_RWIN
  (define-xk xk-app         #xFF5D #x5D) ;; XK_App VK_APPS

  ;; Misc functions
  (define-xk xk-select      #xff60 #x29) ;; XK_Select VK_SELECT
  (define-xk xk-print       #xff61 #x2A) ;; XK_Print VK_PRINT
  (define-xk xk-insert      #xff64 #x2D) ;; XK_Insert VK_INSERT
  (define-xk xk-num-lock    #xff7f #x90) ;; XK_Num_Lock VK_NUMLOCK

  ;; Keypad
  ;; TODO: Check values for MS-Windows
  (define-xk xk-kp-enter    #xff8d nil) ;; XK_KP_Enter ???
  (define-xk xk-kp-multiply #xffaa nil) ;; XK_KP_Multiply ???
  (define-xk xk-kp-add      #xffab nil) ;; XK_KP_Add ???
  (define-xk xk-kp-subtract #xffad nil) ;; XK_KP_Subtract ???
  (define-xk xk-kp-decimal  #xffae nil) ;; XK_KP_Decimal ???
  (define-xk xk-kp-divide   #xffaf nil) ;; XK_KP_Divide ???
  (define-xk xk-kp-0        #xffb0 #x60) ;; XK_KP_0 VK_NUMPAD0
  (define-xk xk-kp-1        #xffb1 #x61) ;; XK_KP_1 VK_NUMPAD1
  (define-xk xk-kp-2        #xffb2 #x62) ;; XK_KP_2 VK_NUMPAD2
  (define-xk xk-kp-3        #xffb3 #x63) ;; XK_KP_3 VK_NUMPAD3
  (define-xk xk-kp-4        #xffb4 #x64) ;; XK_KP_4 VK_NUMPAD4
  (define-xk xk-kp-5        #xffb5 #x65) ;; XK_KP_5 VK_NUMPAD5
  (define-xk xk-kp-6        #xffb6 #x66) ;; XK_KP_6 VK_NUMPAD6
  (define-xk xk-kp-7        #xffb7 #x67) ;; XK_KP_7 VK_NUMPAD7
  (define-xk xk-kp-8        #xffb8 #x68) ;; XK_KP_8 VK_NUMPAD8
  (define-xk xk-kp-9        #xffb9 #x69) ;; XK_KP_9 VK_NUMPAD9

  ;; Function keys
  (define-xk xk-f1          #xffbe #x70) ;; XK_F1 VK_F1
  (define-xk xk-f2          #xffbf #x71) ;; XK_F2 VK_F2
  (define-xk xk-f3          #xffc0 #x72) ;; XK_F3 VK_F3
  (define-xk xk-f4          #xffc1 #x73) ;; XK_F4 VK_F4
  (define-xk xk-f5          #xffc2 #x74) ;; XK_F5 VK_F5
  (define-xk xk-f6          #xffc3 #x75) ;; XK_F6 VK_F6
  (define-xk xk-f7          #xffc4 #x76) ;; XK_F7 VK_F7
  (define-xk xk-f8          #xffc5 #x77) ;; XK_F8 VK_F8
  (define-xk xk-f9          #xffc6 #x78) ;; XK_F9 VK_F9
  (define-xk xk-f10         #xffc7 #x79) ;; XK_F10 VK_F10
  (define-xk xk-f11         #xffc8 #x7A) ;; XK_F11 VK_F11
  (define-xk xk-f12         #xffc9 #x7B) ;; XK_F12 VK_F12
  (define-xk xk-f13         #xffca #x7C) ;; XK_F13 VK_F13
  (define-xk xk-f14         #xffcb #x7D) ;; XK_F14 VK_F14
  (define-xk xk-f15         #xffcc #x7E) ;; XK_F15 VK_F15
  (define-xk xk-f16         #xffcd #x7F) ;; XK_F16 VK_F16
  (define-xk xk-f17         #xffce #x80) ;; XK_F17 VK_F17
  (define-xk xk-f18         #xffcf #x81) ;; XK_F18 VK_F18
  (define-xk xk-f19         #xffd0 #x82) ;; XK_F19 VK_F19
  (define-xk xk-f20         #xffd1 #x83) ;; XK_F20 VK_F20
  (define-xk xk-f21         #xffd2 #x84) ;; XK_F21 VK_F21
  (define-xk xk-f22         #xffd3 #x85) ;; XK_F22 VK_F22
  (define-xk xk-f23         #xffd4 #x86) ;; XK_F23 VK_F23
  (define-xk xk-f24         #xffd5 #x87) ;; XK_F24 VK_F24

  ;; Modifier keys
  (define-xk xk-shift-l     #xffe1 #xA0) ;; XK_Shift_L VK_LSHIFT
  (define-xk xk-shift-r     #xffe2 #xA1) ;; XK_Shift_R VK_RSHIFT
  (define-xk xk-control-l   #xffe3 #xA2) ;; XK_Control_L VK_LCONTROL
  (define-xk xk-control-r   #xffe4 #xA3) ;; XK_Control_R VK_RCONTROL
  (define-xk xk-caps-lock   #xffe5 #x14) ;; XK_Caps_Lock VK_CAPITAL
  (define-xk xk-metal-l     #xffe7 nil) ;; XK_Meta_L
  (define-xk xk-metal-t     #xffee nil) ;; XK_Meta_R
  (define-xk xk-alt-l       #xffe9 #xA4) ;; XK_Alt_L VK_LMENU
  (define-xk xk-alt-r       #xffea #xA5) ;; XK_Alt_R VK_RMENU
  (define-xk xk-super-l     #xffeb nil) ;; XK_Super_L
  (define-xk xk-super-r     #xffec nil) ;; XK_Super_R
  (define-xk xk-hyper-l     #xffed nil) ;; XK_Hyper_L
  (define-xk xk-hyper-r     #xffee nil) ;; XK_Hyper_R

  ;; Latin 1
  ;; For numbers and letters, MS-Windows does not define constant names.
  ;; X11 defines distinct keysyms for lowercase and uppercase
  ;; letters. We use only the uppercase ones. Events with lowercase
  ;; letters are converted to uppercase.
  (define-xk xk-space       #x0020 #x20) ;; XK_space VK_SPACE
  (define-xk xk-0           #x0030 #x30) ;; XK_0
  (define-xk xk-1           #x0031 #x31) ;; XK_1
  (define-xk xk-2           #x0032 #x32) ;; XK_2
  (define-xk xk-3           #x0033 #x33) ;; XK_3
  (define-xk xk-4           #x0034 #x34) ;; XK_4
  (define-xk xk-5           #x0035 #x35) ;; XK_5
  (define-xk xk-6           #x0036 #x36) ;; XK_6
  (define-xk xk-7           #x0037 #x37) ;; XK_7
  (define-xk xk-8           #x0038 #x38) ;; XK_8
  (define-xk xk-9           #x0039 #x39) ;; XK_9
  (define-xk xk-a           #x0041 #x41) ;; XK_A
  (define-xk xk-b           #x0042 #x42) ;; XK_B
  (define-xk xk-c           #x0043 #x43) ;; XK_C
  (define-xk xk-d           #x0044 #x44) ;; XK_D
  (define-xk xk-e           #x0045 #x45) ;; XK_E
  (define-xk xk-f           #x0046 #x46) ;; XK_F
  (define-xk xk-g           #x0047 #x47) ;; XK_G
  (define-xk xk-h           #x0048 #x48) ;; XK_H
  (define-xk xk-i           #x0049 #x49) ;; XK_I
  (define-xk xk-j           #x004A #x4A) ;; XK_J
  (define-xk xk-k           #x004B #x4B) ;; XK_K
  (define-xk xk-l           #x004C #x4C) ;; XK_L
  (define-xk xk-m           #x004D #x4D) ;; XK_M
  (define-xk xk-n           #x004E #x4E) ;; XK_N
  (define-xk xk-o           #x004F #x4F) ;; XK_O
  (define-xk xk-p           #x0050 #x50) ;; XK_P
  (define-xk xk-q           #x0051 #x51) ;; XK_Q
  (define-xk xk-r           #x0052 #x52) ;; XK_R
  (define-xk xk-s           #x0053 #x53) ;; XK_S
  (define-xk xk-t           #x0054 #x54) ;; XK_T
  (define-xk xk-u           #x0055 #x55) ;; XK_U
  (define-xk xk-v           #x0056 #x56) ;; XK_V
  (define-xk xk-w           #x0057 #x57) ;; XK_W
  (define-xk xk-x           #x0058 #x58) ;; XK_X
  (define-xk xk-y           #x0059 #x59) ;; XK_Y
  (define-xk xk-z           #x005A #x5A));; XK_Z

(defun llk-init ()
  "Initialize low-level key events.
Fills the `llk-keysyms' list, and binds the `low-level-key' event
to the `llk-handle' function.  Resets the `llk-bindings' list.
Besides calling this function, you need to set `enable-low-level-key-events'
to a non-nil value"
  (interactive)
  (llk-define-keysyms)
  (define-key special-event-map [low-level-key] 'llk-handle)
  (setq llk-bindings nil))

(defsubst event-is-key-press (event)
  "Return the value of the IS-KEY-PRESS field of the EVENT, a low level key event."
  (declare (side-effect-free t))
  (if (consp event) (nth 1 event)))

(defsubst event-keysym (event)
  "Return the value of the KEY field of the EVENT, a low level key event."
  (declare (side-effect-free t))
  (if (consp event) (nth 2 event)))

(defsubst event-modifier (event)
  "Return the value of the MODIFIER field of the EVENT, a low level key event."
  (declare (side-effect-free t))
  (if (consp event) (nth 3 event)))

(defsubst event-time (event)
  "Return the value of the TIME field of the EVENT, a low level key event."
  (declare (side-effect-free t))
  (if (consp event) (nth 4 event)))

;; For example:
;; Bind key tap to command
;;    (llk-bind 'tap 'xk-shift-l 'delete-other-windows)
;; Bind modifiry tap to command
;;     (llk-bind 'tap 'shift 'delete-other-windows)
;; Bind tap to hyper modifier
;;      (llk-bind 'tap 'xk-shift-r (lambda ()
;;                              (message "H-...")
;;                              (setq unread-command-events
;;                                    (append (event-apply-hyper-modifier nil) nil))))
;; Can bind to a command or function
(defun llk-bind (action key function)
  "Bind a command a function to a low level key event.
The only action supported currently is `tap'. The key can be a keysym
symbol, or a modifier symbol (shift, control, alt, meta, hyper, super).
If there is no keysym symbol for a key, use the keysym number.  "
  (push (list action key function) llk-bindings))

;; We store the last events (key/modifier is-press timestamp) here to
;; test for multitap.
(defvar llk-events nil
  "Internal variable for detecting taps.")

;; If positive, return key (xk-shift-l, etc) else return nil.
(defun llk-detect-n-tap (n timeout)
  "Internal function to detect n-tap keys."
  (let (key
        (is-press (event-is-key-press last-input-event))
        ;; convert number to keysym symbol
        (keysym (cdr (assoc (event-keysym last-input-event) llk-keysyms)))
        (timestamp (event-time last-input-event))
        (modifier (event-modifier last-input-event)))

    ;; if ehte is no symbol for this key, use its keysym number
    (unless keysym (setq keysym (event-keysym last-input-event)))

    ;; look in llk-tap-keys for the key, then the modifier
    (if (member keysym llk-tap-keys)
        (setq key keysym)
      (if (member modifier llk-tap-keys)
          (setq key modifier)))

    (if (not key)
        ;; Key not in tap list, clear history
        (setq llk-events nil)
      ;; Clear it also if the first element is from a different key
      (and llk-events
           (not (equal (car (car llk-events)) key))
           (setq llk-events nil))
      (push (list key is-press timestamp) llk-events)
      ;; Only care about last 2xN events
      (ntake (* 2 n) llk-events)
      ;; If we have:
      ;; - Exactly 2 * n events.
      ;; - down, up, down, up, ...
      ;; - not two much time between first and last
      (and (eq (* 2 n) (length llk-events))
           (cl-every 'eq
                     (ntake (* 2 n)
                            (list nil t nil t nil t nil t
                                  nil t nil t nil t nil t))
                     (mapcar 'cl-second llk-events))
           (< (- (cl-third (cl-first llk-events))
                 (cl-third (car (last llk-events))))
              timeout)
           (progn
             (setq llk-events nil)
             key)))))

(defun describe-low-level-key ()
  "Wait for the next key press and describe the low level key event it
generates."
  (interactive)
  (setq llk-describe-next-press t))

(defun llk-show-event-description ()
  "Shoe information about the last low level key event."
  (setq llk-describe-next-press nil)
  (with-help-window (help-buffer)
    (insert "\n")
    (let* ((xk (event-keysym last-input-event))
           (sym (assoc xk llk-keysyms)))
      (insert (format "Keysym number: %d (#x%X),\n" xk xk))
      (if sym
          (insert (format "which corresponds to named key %s.\n\n" (cdr sym)))
        (insert "which does not correspond to any known named key.\n\n"))
      (if (event-modifier last-input-event)
          (insert (format "This key corresponds to the %s modifier.\n\n"
                          (event-modifier last-input-event)))
        (insert "This key does not correspond to a modifier.\n\n"))
      (insert "See the value of the `llk-keysyms' variable for a list of known keys.\n"))))

(defun llk-handle ()
  "Internal function to handle low level key events."
  (interactive)
  (if (and (event-is-key-press last-input-event)
           llk-describe-next-press)
      (llk-show-event-description)
    (let ((tap-key (llk-detect-n-tap
                    llk-tap-count
                    llk-tap-timeout)))
      (when tap-key
        (let ((func (cl-third
                     (seq-find
                      (lambda (b)
                        (and (eq (cl-first b) 'tap)
                             (eq (cl-second b) tap-key)))
                      llk-bindings))))
          (cond
           ((commandp func) (call-interactively func))
           ((functionp func) (funcall func))))))))
