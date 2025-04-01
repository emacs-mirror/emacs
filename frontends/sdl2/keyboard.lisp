(defpackage :lem-sdl2/keyboard
  (:use :cl)
  (:export :set-keyboard-layout
           :keysym-to-key-event
           :handle-textediting
           :handle-text-input
           :handle-key-down
           :handle-key-up))
(in-package :lem-sdl2/keyboard)

(defstruct (keyinfo (:type list))
  keycode
  sym
  text-input-p)

(defparameter *code-name-table*
  `((,sdl2-ffi:+sdlk-backspace+ "Backspace" nil)
    (,sdl2-ffi:+sdlk-tab+ "Tab" nil)
    (,sdl2-ffi:+sdlk-return+ "Return" nil)
    (,sdl2-ffi:+sdlk-insert+ "Insert" nil)
    (,sdl2-ffi:+sdlk-delete+ "Delete" nil)
    (,sdl2-ffi:+sdlk-space+ "Space" t)
    (,sdl2-ffi:+sdlk-home+ "Home" nil)
    (,sdl2-ffi:+sdlk-end+ "End" nil)
    (,sdl2-ffi:+sdlk-pageup+ "PageUp" nil)
    (,sdl2-ffi:+sdlk-pagedown+ "PageDown" nil)
    (,sdl2-ffi:+sdlk-escape+ "Escape" nil)
    (,sdl2-ffi:+sdlk-left+ "Left" nil)
    (,sdl2-ffi:+sdlk-right+ "Right" nil)
    (,sdl2-ffi:+sdlk-up+ "Up" nil)
    (,sdl2-ffi:+sdlk-down+ "Down" nil)
    (,sdl2-ffi:+sdlk-f1+ "F1" nil)
    (,sdl2-ffi:+sdlk-f2+ "F2" nil)
    (,sdl2-ffi:+sdlk-f3+ "F3" nil)
    (,sdl2-ffi:+sdlk-f4+ "F4" nil)
    (,sdl2-ffi:+sdlk-f5+ "F5" nil)
    (,sdl2-ffi:+sdlk-f6+ "F6" nil)
    (,sdl2-ffi:+sdlk-f7+ "F7" nil)
    (,sdl2-ffi:+sdlk-f8+ "F8" nil)
    (,sdl2-ffi:+sdlk-f9+ "F9" nil)
    (,sdl2-ffi:+sdlk-f10+ "F10" nil)
    (,sdl2-ffi:+sdlk-f11+ "F11" nil)
    (,sdl2-ffi:+sdlk-f12+ "F12" nil)
    (,sdl2-ffi:+sdlk-f13+ "F13" nil)
    (,sdl2-ffi:+sdlk-f14+ "F14" nil)
    (,sdl2-ffi:+sdlk-f15+ "F15" nil)
    (,sdl2-ffi:+sdlk-f16+ "F16" nil)
    (,sdl2-ffi:+sdlk-f17+ "F17" nil)
    (,sdl2-ffi:+sdlk-f18+ "F18" nil)
    (,sdl2-ffi:+sdlk-f19+ "F19" nil)
    (,sdl2-ffi:+sdlk-f20+ "F20" nil)
    (,sdl2-ffi:+sdlk-f21+ "F21" nil)
    (,sdl2-ffi:+sdlk-f22+ "F22" nil)
    (,sdl2-ffi:+sdlk-f23+ "F23" nil)
    (,sdl2-ffi:+sdlk-f24+ "F24" nil)))

(defun convert-to-sym (code)
  (let ((keyinfo (assoc code *code-name-table*)))
    (if keyinfo
        (values (keyinfo-sym keyinfo) (keyinfo-text-input-p keyinfo))
        (when (<= code #x110000)
          (values (string (code-char code))
                  t)))))

(defun make-key (&key ctrl meta shift super hyper sym)
  (when (equal sym (string #\yen_sign))
    (setf sym "\\"))
  (lem:make-key :ctrl ctrl
                :meta meta
                :super super
                :hyper hyper
                :shift shift
                :sym sym))

(defstruct modifier
  shift
  ctrl
  meta
  super
  hyper)

(defstruct (key-event (:constructor %make-key-event))
  code
  modifier)

(defun make-key-event (code modifier)
  (%make-key-event :code code :modifier modifier))

(defun keysym-to-key-event (keysym)
  (let ((code (sdl2:sym-value keysym))
        (modifier (get-modifier keysym)))
    (make-key-event code modifier)))

(defparameter *modifier-code-table*
  `((:shift ,sdl2-ffi:+kmod-lshift+ ,sdl2-ffi:+kmod-rshift+)
    (:ctrl ,sdl2-ffi:+kmod-lctrl+ ,sdl2-ffi:+kmod-rctrl+)
    (:meta ,sdl2-ffi:+kmod-lalt+ ,sdl2-ffi:+kmod-ralt+)
    (:super ,sdl2-ffi:+kmod-lgui+)
    (:hyper ,sdl2-ffi:+kmod-rgui+)))

(defun mod-p (mod mod-type)
  (some (lambda (value)
          (= value (logand value mod)))
        (cdr (assoc mod-type *modifier-code-table*))))

(defun get-modifier (keysym)
  (let* ((mod (sdl2:mod-value keysym))
         (shift (mod-p mod :shift))
         (ctrl (mod-p mod :ctrl))
         (meta (mod-p mod :meta))
         (super (mod-p mod :super))
         (hyper (mod-p mod :hyper)))
    (make-modifier :shift shift :ctrl ctrl :meta meta :super super :hyper hyper)))

(defun update-modifier (modifier new-modifier)
  (setf (modifier-shift modifier) (modifier-shift new-modifier))
  (setf (modifier-ctrl modifier) (modifier-ctrl new-modifier))
  (setf (modifier-meta modifier) (modifier-meta new-modifier))
  (setf (modifier-super modifier) (modifier-super new-modifier))
  (setf (modifier-hyper modifier) (modifier-hyper new-modifier)))

(defvar *modifier* (make-modifier))
(defvar *textediting-text* "")

(defmethod handle-textediting (platform text)
  (setf *textediting-text* text))

(defun send-key-event (key)
  (if (lem:match-key key :ctrl t :sym "]")
      (lem:send-abort-event (lem:find-editor-thread) nil)
      (lem:send-event key)))

;; linux
(defun modifier-is-accept-text-input-p (modifier)
  (not (modifier-ctrl modifier)))

(defmethod handle-text-input ((platform lem-sdl2/platform:linux) text)
  (handle-text-input-unix text))

(defmethod handle-key-down ((platform lem-sdl2/platform:linux) key-event)
  (handle-key-down-unix key-event))

(defmethod handle-key-up ((platform lem-sdl2/platform:linux) key-event)
  (handle-key-up-unix key-event))

(defun handle-text-input-unix (text)
  (when (modifier-is-accept-text-input-p *modifier*)
    (loop :for c :across text
          :do (multiple-value-bind (sym text-input-p) (convert-to-sym (char-code c))
                (let ((key (lem:make-key :ctrl (modifier-ctrl *modifier*)
                                         :meta (modifier-meta *modifier*)
                                         :super (modifier-super *modifier*)
                                         :hyper (modifier-hyper *modifier*)
                                         :shift nil
                                         :sym sym)))
                  (when text-input-p
                    (send-key-event key)))))))

(defun handle-key-down-unix (key-event)
  (let ((modifier (key-event-modifier key-event))
        (code (key-event-code key-event)))
    (update-modifier *modifier* modifier)
    (multiple-value-bind (sym text-input-p) (convert-to-sym code)
      (when (and sym
                 (or (not text-input-p)
                     (not (modifier-is-accept-text-input-p *modifier*))
                     (< 256 code)))
        (let ((key (make-key-with-shift-careful :shift (modifier-shift modifier)
                                                :ctrl (modifier-ctrl modifier)
                                                :meta (modifier-meta modifier)
                                                :super (modifier-super modifier)
                                                :hyper (modifier-hyper modifier)
                                                :sym sym)))
          (send-key-event key))))))

(defun handle-key-up-unix (key-event)
  (update-modifier *modifier* (key-event-modifier key-event)))

;;
(defparameter *us-shift-layout*
  '((#\1 . #\!)
    (#\2 . #\@)
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\^)
    (#\7 . #\&)
    (#\8 . #\*)
    (#\9 . #\()
    (#\0 . #\))
    (#\- . #\_)
    (#\= . #\+)
    (#\` . #\~)
    (#\[ . #\{)
    (#\] . #\})
    (#\\ . #\|)
    (#\; . #\:)
    (#\' . #\")
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)))

(defparameter *jis-shift-layout*
  '((#\1 . #\!)
    (#\2 . #\")
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\&)
    (#\7 . #\')
    (#\8 . #\()
    (#\9 . #\))
    (#\0 . #\0)
    (#\- . #\=)
    (#\^ . #\_)
    (#\\ . #\|)
    (#\@ . #\`)
    (#\[ . #\{)
    (#\] . #\})
    (#\; . #\+)
    (#\: . #\*)
    (#\] . #\})
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)))

(defparameter *shift-layout* *us-shift-layout*)

(defun set-keyboard-layout (type)
  (check-type type (member :jis :us))
  (ecase type
    (:us
     (setf *shift-layout* *us-shift-layout*))
    (:jis
     (setf *shift-layout* *jis-shift-layout*))))

(defun shift-char (char)
  (if (alpha-char-p char)
      (values (char-upcase char) t)
      (let ((elt (assoc char *shift-layout*)))
        (if elt
            (values (cdr elt) t)
            (values char nil)))))

(defun make-key-with-shift-careful (&key shift ctrl meta super hyper sym)
  (or (and (= 1 (length sym))
           shift
           (multiple-value-bind (char shift-p)
               (shift-char (char sym 0))
             (and shift-p
                  (make-key :ctrl ctrl
                            :meta meta
                            :super super
                            :hyper hyper
                            :shift nil
                            :sym (string char)))))
      (make-key :ctrl ctrl
                :meta meta
                :super super
                :hyper hyper
                :shift shift
                :sym sym)))

(defun handle-text-input-internal (text)
  (unless (or (modifier-hyper *modifier*)
              (modifier-super *modifier*)
              (modifier-meta *modifier*)
              (modifier-ctrl *modifier*))
    (loop :for c :across text
          :do (let ((key (make-key :ctrl (modifier-ctrl *modifier*)
                                   :meta (modifier-meta *modifier*)
                                   :super (modifier-super *modifier*)
                                   :hyper (modifier-hyper *modifier*)
                                   :shift nil
                                   :sym (convert-to-sym (char-code c)))))
                (send-key-event key)))))

(defun handle-key-down-internal (key-event)
  (let ((code (key-event-code key-event))
        (modifier (key-event-modifier key-event)))
    (update-modifier *modifier* modifier)
    (when (equal *textediting-text* "")
      (multiple-value-bind (sym text-input-p) (convert-to-sym code)
        (when (and sym
                   (or (not text-input-p)
                       (modifier-ctrl modifier)
                       (modifier-meta modifier)
                       (modifier-super modifier)
                       (modifier-hyper modifier)
                       (< 256 code)))
          (let ((key (make-key-with-shift-careful :shift (modifier-shift modifier)
                                                  :ctrl (modifier-ctrl modifier)
                                                  :meta (modifier-meta modifier)
                                                  :super (modifier-super modifier)
                                                  :hyper (modifier-hyper modifier)
                                                  :sym sym)))
            (send-key-event key)))))))

(defun handle-key-up-internal (key-event)
  (update-modifier *modifier* (key-event-modifier key-event)))

(defmethod handle-text-input ((platform lem-sdl2/platform:mac) text)
  (handle-text-input-internal text))

(defmethod handle-key-down ((platform lem-sdl2/platform:mac) key-event)
  (handle-key-down-internal key-event))

(defmethod handle-key-up ((platform lem-sdl2/platform:mac) key-event)
  (handle-key-up-internal key-event))

;; windows
(defmethod handle-text-input ((platform lem-sdl2/platform:windows) text)
  (handle-text-input-internal text))

(defmethod handle-key-down ((platform lem-sdl2/platform:windows) key-event)
  (handle-key-down-internal key-event))

(defmethod handle-key-up ((platform lem-sdl2/platform:windows) key-event)
  (handle-key-up-internal key-event))
