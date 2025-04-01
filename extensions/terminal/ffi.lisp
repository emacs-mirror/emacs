(defpackage :lem-terminal/ffi
  (:use :cl))
(in-package :lem-terminal/ffi)

(pushnew (asdf:system-relative-pathname
          :lem-terminal (format nil
                                "~(lib/~A/~A/~)"
                                (uiop:operating-system)
                                (uiop:architecture)))
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library terminal
  (:unix "terminal.so"))

(ignore-errors
  (cffi:use-foreign-library terminal))

(defconstant VTERM_KEY_NONE 0)
(defconstant VTERM_KEY_ENTER 1)
(defconstant VTERM_KEY_TAB 2)
(defconstant VTERM_KEY_BACKSPACE 3)
(defconstant VTERM_KEY_ESCAPE 4)
(defconstant VTERM_KEY_UP 5)
(defconstant VTERM_KEY_DOWN 6)
(defconstant VTERM_KEY_LEFT 7)
(defconstant VTERM_KEY_RIGHT 8)
(defconstant VTERM_KEY_INS 9)
(defconstant VTERM_KEY_DEL 10)
(defconstant VTERM_KEY_HOME 11)
(defconstant VTERM_KEY_END 12)
(defconstant VTERM_KEY_PAGEUP 13)
(defconstant VTERM_KEY_PAGEDOWN 14)
(defconstant VTERM_KEY_FUNCTION_0 256)
(defconstant VTERM_KEY_FUNCTION_MAX 511)
(defconstant VTERM_KEY_KP_0 512)
(defconstant VTERM_KEY_KP_1 513)
(defconstant VTERM_KEY_KP_2 514)
(defconstant VTERM_KEY_KP_3 515)
(defconstant VTERM_KEY_KP_4 516)
(defconstant VTERM_KEY_KP_5 517)
(defconstant VTERM_KEY_KP_6 518)
(defconstant VTERM_KEY_KP_7 519)
(defconstant VTERM_KEY_KP_8 520)
(defconstant VTERM_KEY_KP_9 521)
(defconstant VTERM_KEY_KP_MULT 522)
(defconstant VTERM_KEY_KP_PLUS 523)
(defconstant VTERM_KEY_KP_COMMA 524)
(defconstant VTERM_KEY_KP_MINUS 525)
(defconstant VTERM_KEY_KP_PERIOD 526)
(defconstant VTERM_KEY_KP_DIVIDE 527)
(defconstant VTERM_KEY_KP_ENTER 528)
(defconstant VTERM_KEY_KP_EQUAL 529)
(defconstant VTERM_KEY_MAX 530)
(defconstant VTERM_N_KEYS 530)

(defconstant VTERM_MOD_NONE #x00)
(defconstant VTERM_MOD_SHIFT #x01)
(defconstant VTERM_MOD_ALT #x02)
(defconstant VTERM_MOD_CTRL #x04)

;; Define a type corresponding to char*[], where the array is null-terminated
(cffi:define-foreign-type string-of-strings ()
  ()
  (:actual-type :pointer))
(cffi:define-parse-method string-of-strings ()
  (make-instance 'string-of-strings))
;; It's inefficient to store the array of string pointers in both the second return
;; value of translate-to-foreign and (necessarily) foreign memory- but I can't get
;; the "efficient" way to work.
;; Also this likely isn't performance sensitive. Like at all.
(defmethod cffi:translate-to-foreign (val (type string-of-strings))
  (let ((string-pointers (map 'vector  (lambda (str) (cffi:foreign-string-alloc str)) val)))
    (values (cffi:foreign-alloc :pointer :null-terminated-p t :initial-contents string-pointers)
            string-pointers)))
(defmethod cffi:free-translated-object (pointer (type string-of-strings) string-pointers)
  (map nil (lambda (p) (cffi:foreign-string-free p))
       string-pointers)
  (cffi:foreign-free pointer))

(cffi:defcfun ("terminal_new" %terminal-new) :pointer
  (id :int)
  (rows :int)
  (cols :int)
  (program :string)
  (argv (string-of-strings))
  (cb_damage :pointer)
  (cb_moverect :pointer)
  (cb_movecursor :pointer)
  (cb_settermprop :pointer)
  (cb_bell :pointer)
  (cb_resize :pointer)
  (cb_sb_pushline :pointer)
  (cb_sb_popline :pointer))

(defun terminal-new (directory id rows cols)
  (let* ((shell (or (uiop:getenv "SHELL") "/bin/bash"))
         (argv (list shell "-c" (concatenate 'string "cd " directory "; " shell))))
    (%terminal-new id
                   rows
                   cols
                   shell
                   argv
                   (cffi:callback cb-damage)
                   (cffi:callback cb-moverect)
                   (cffi:callback cb-movecursor)
                   (cffi:callback cb-settermprop)
                   (cffi:callback cb-bell)
                   (cffi:callback cb-resize)
                   (cffi:callback cb-sb-pushline)
                   (cffi:callback cb-sb-popline))))

(cffi:defcfun ("terminal_delete" terminal-delete) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_input_char" terminal-input-char) :void
  (terminal :pointer)
  (c :uint32)
  (mod :int))

(cffi:defcfun ("terminal_input_key" terminal-input-key) :void
  (terminal :pointer)
  (key :int)
  (mod :int))

(cffi:defcfun ("terminal_process_input_wait" terminal-process-input-wait) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_process_input_nonblock" terminal-process-input-nonblock) :bool
  (terminal :pointer))

(cffi:defcfun ("terminal_process_input" terminal-process-input) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_query_cell" terminal-query-cell) :pointer
  (terminal :pointer)
  (x :int)
  (y :int))

(cffi:defcfun ("terminal_last_cell_chars" terminal-last-cell-chars) :pointer
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_width" terminal-last-cell-width) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_bold" terminal-last-cell-attrs-bold) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_underline" terminal-last-cell-attrs-underline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_italic" terminal-last-cell-attrs-italic) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_blink" terminal-last-cell-attrs-blink) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_reverse" terminal-last-cell-attrs-reverse) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_conceal" terminal-last-cell-attrs-conceal) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_strike" terminal-last-cell-attrs-strike) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_font" terminal-last-cell-attrs-font) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dwl" terminal-last-cell-attrs-dwl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dhl" terminal-last-cell-attrs-dhl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_small" terminal-last-cell-attrs-small) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_baseline" terminal-last-cell-attrs-baseline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_red" terminal-last-cell-fg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_green" terminal-last-cell-fg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_blue" terminal-last-cell-fg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_red" terminal-last-cell-bg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_green" terminal-last-cell-bg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_blue" terminal-last-cell-bg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_row" terminal-cursor-row) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_col" terminal-cursor-col) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_resize" terminal-resize) :void
  (terminal :pointer)
  (rows :int)
  (cols :int))

(cffi:defcstruct vterm-rect
  (start-row :int)
  (start-col :int)
  (end-row :int)
  (end-col :int))

(cffi:defcstruct vterm-pos
  (row :int)
  (col :int))

(cffi:defcenum vterm-prop
  (:VTERM_PROP_CURSORVISIBLE 1) ; bool
  :VTERM_PROP_CURSORBLINK       ; bool
  :VTERM_PROP_ALTSCREEN         ; bool
  :VTERM_PROP_TITLE             ; string
  :VTERM_PROP_ICONNAME          ; string
  :VTERM_PROP_REVERSE           ; bool
  :VTERM_PROP_CURSORSHAPE       ; number
  :VTERM_PROP_MOUSE             ; number
  :VTERM_PROP_FOCUSREPORT       ; bool
  )

(defvar *damage-callback* nil)
(defvar *moverect-callback* nil)
(defvar *movecursor-callback* nil)
(defvar *settermprop-callback* nil)
(defvar *bell-callback* nil)
(defvar *resize-callback* nil)
(defvar *sb-pushline-callback* nil)
(defvar *sb-popline-callback* nil)

(defun set-callbacks (&key damage
                           moverect
                           movecursor
                           settermprop
                           bell
                           resize
                           sb-pushline
                           sb-popline)
  (setf *damage-callback* damage
        *moverect-callback* moverect
        *movecursor-callback* movecursor
        *settermprop-callback* settermprop
        *bell-callback* bell
        *resize-callback* resize
        *sb-pushline-callback* sb-pushline
        *sb-popline-callback* sb-popline)
  (values))

(cffi:defcallback cb-damage :int ((rect (:pointer (:struct vterm-rect))) (id :int))
  (when *damage-callback*
    (funcall *damage-callback* rect id))
  0)

(cffi:defcallback cb-moverect :int ((dest (:pointer (:struct vterm-rect)))
                                    (src (:pointer (:struct vterm-rect)))
                                    (id :int))
  (when *moverect-callback*
    (funcall *moverect-callback* dest src id))
  0)

(cffi:defcallback cb-movecursor :int ((pos (:pointer (:struct vterm-pos)))
                                      (oldpos (:pointer (:struct vterm-pos)))
                                      (visible :int)
                                      (id :int))
  (when *movecursor-callback*
    (funcall *movecursor-callback* pos oldpos visible id))
  0)

(cffi:defcallback cb-settermprop :int ((prop vterm-prop)
                                       (val :pointer)
                                       (id :int))
  (when *settermprop-callback*
    (funcall *settermprop-callback* prop val id))
  0)

(cffi:defcallback cb-bell :int ((id :int))
  (when *bell-callback*
    (funcall *bell-callback* id))
  0)

(cffi:defcallback cb-resize :int ((rows :int) (cols :int) (id :int))
  (when *resize-callback*
    (funcall *resize-callback* rows cols id))
  0)

(cffi:defcallback cb-sb-pushline :int ((cols :int) (cells :pointer) (id :int))
  (when *sb-pushline-callback*
    (funcall *sb-pushline-callback* cols cells id))
  0)

(cffi:defcallback cb-sb-popline :int ((cols :int) (cells :pointer) (id :int))
  (when *sb-popline-callback*
    (funcall *sb-popline-callback* cols cells id))
  0)
