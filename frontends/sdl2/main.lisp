(defpackage :lem-sdl2
  (:use :cl
        :lem-sdl2/sdl2
        :lem-sdl2/keyboard
        :lem-sdl2/font
        :lem-sdl2/icon
        :lem-sdl2/platform
        :lem-sdl2/resource
        :lem-sdl2/log
        :lem-sdl2/mouse)
  (:local-nicknames (:display :lem-sdl2/display)
                    (:view :lem-sdl2/view))
  (:export :change-font
           :set-keyboard-layout
           :render
           :current-renderer))
(in-package :lem-sdl2)

(defconstant +display-width+ 100)
(defconstant +display-height+ 40)

(defun on-mouse-button-down (display button x y clicks)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4))))
    (when button
      (let ((char-x (display:scaled-char-width display x))
            (char-y (display:scaled-char-height display y)))
        (lem:send-event
         (lambda ()
           (lem:receive-mouse-button-down char-x char-y x y button
                                          clicks)))))))

(defun on-mouse-button-up (display button x y)
  (show-cursor)
  (let ((button
          (cond ((eql button sdl2-ffi:+sdl-button-left+) :button-1)
                ((eql button sdl2-ffi:+sdl-button-right+) :button-3)
                ((eql button sdl2-ffi:+sdl-button-middle+) :button-2)
                ((eql button 4) :button-4)))
        (char-x (display:scaled-char-width display x))
        (char-y (display:scaled-char-height display y)))
    (lem:send-event
     (lambda ()
       (lem:receive-mouse-button-up char-x char-y x y button)))))

(defun on-mouse-motion (display x y state)
  (show-cursor)
  (let ((button (if (= sdl2-ffi:+sdl-button-lmask+ (logand state sdl2-ffi:+sdl-button-lmask+))
                    :button-1
                    nil)))
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-motion char-x char-y x y button))))))

(defun on-mouse-wheel (display wheel-x wheel-y which direction)
  (declare (ignore which direction))
  (show-cursor)
  (multiple-value-bind (x y) (sdl2:mouse-state)
    (let ((char-x (display:scaled-char-width display x))
          (char-y (display:scaled-char-height display y)))
      (lem:send-event
       (lambda ()
         (lem:receive-mouse-wheel char-x char-y x y wheel-x wheel-y)
         (when (= 0 (lem:event-queue-length))
           (lem:redraw-display)))))))

(defun on-textediting (text)
  (handle-textediting (get-platform) text)
  (lem:send-event #'lem:redraw-display))

(defun on-textinput (value)
  (hide-cursor)
  (let ((text (etypecase value
                (integer (string (code-char value)))
                (string value))))
    (handle-text-input (get-platform) text)))

(defun on-keydown (key-event)
  (hide-cursor)
  (handle-key-down (get-platform) key-event))

(defun on-keyup (key-event)
  (handle-key-up (get-platform) key-event))

(defun on-windowevent (display event)
  (alexandria:switch (event)
    (sdl2-ffi:+sdl-windowevent-shown+
     (display:notify-required-redisplay display))
    (sdl2-ffi:+sdl-windowevent-exposed+
     (display:notify-required-redisplay display))
    (sdl2-ffi:+sdl-windowevent-resized+
     (display:update-texture display)
     (display:notify-required-redisplay display))
    (sdl2-ffi:+sdl-windowevent-focus-gained+
     (setf (display:display-focus-p display) t))
    (sdl2-ffi:+sdl-windowevent-focus-lost+
     (setf (display:display-focus-p display) nil))))

(defun on-filedrop (file)
  (lem:send-event (lambda () (lem:find-file file))))

(defun event-loop (display)
  (sdl2:with-event-loop (:method :wait)
    (:quit ()
     #+windows
     (cffi:foreign-funcall "_exit")
     t)
    (:textinput (:text text)
     (on-textinput text))
    (:textediting (:text text)
     (on-textediting text))
    (:keydown (:keysym keysym)
     (on-keydown (keysym-to-key-event keysym)))
    (:keyup (:keysym keysym)
     (on-keyup (keysym-to-key-event keysym)))
    (:mousebuttondown (:button button :x x :y y :clicks clicks)
     (on-mouse-button-down display button x y clicks))
    (:mousebuttonup (:button button :x x :y y)
     (on-mouse-button-up display button x y))
    (:mousemotion (:x x :y y :state state)
     (on-mouse-motion display x y state))
    (:mousewheel (:x x :y y :which which :direction direction)
     (on-mouse-wheel display x y which direction))
    (:dropfile (:file file)
     (on-filedrop file))
    (:windowevent (:event event)
     (on-windowevent display event))))

(defun init-application-icon (window)
  (let ((image (sdl2-image:load-image (get-resource-pathname "resources/icon.png"))))
    (sdl2-ffi.functions:sdl-set-window-icon window image)
    (sdl2:free-surface image)))

(defun create-display (function)
  (lem-sdl2/wm:set-x11-wm-class)
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (sdl2-image:init '(:png))
    (unwind-protect
         (let* ((font-config (make-font-config))
                (font (open-font font-config))
                (char-width (font-char-width font))
                (char-height (font-char-height font))
                (window-width (* +display-width+ char-width))
                (window-height (* +display-height+ char-height)))
           (sdl2:with-window (window :title "Lem"
                                     :w window-width
                                     :h window-height
                                     :flags '(:shown :resizable :allow-highdpi))
             (init-application-icon window)
             (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
               (let* ((renderer-size (multiple-value-list
                                               (sdl2:get-renderer-output-size renderer)))
                      (renderer-width (first renderer-size))
                      (renderer-height (second renderer-size))
                      (scale-x (/ renderer-width window-width))
                      (scale-y (/ renderer-height window-height))
                      (texture (lem-sdl2/utils:create-texture renderer
                                                              (* scale-x window-width)
                                                              (* scale-y window-height)))
                      (display (make-instance 'display:display
                                              :font-config font-config
                                              :font font
                                              :renderer renderer
                                              :window window
                                              :texture texture
                                              :char-width (font-char-width font)
                                              :char-height (font-char-height font)
                                              :scale (list scale-x scale-y))))
                 (setf (display:current-display) display)
                 (display:adapt-high-dpi-font-size display)
                 (sdl2:start-text-input)
                 (funcall function)
                 (event-loop display)))))
      (sdl2-ttf:quit)
      (sdl2-image:quit))))

(defun sbcl-on-darwin-p ()
  (or #+(and sbcl darwin)
      t
      nil))

(defmethod lem-if:invoke ((implementation sdl2) function)
  (flet ((thunk ()
           (let ((editor-thread
                   (funcall function
                            ;; initialize
                            (lambda ())
                            ;; finalize
                            (lambda (report)
                              (when report
                                (do-log report))
                              (sdl2:push-quit-event)))))
             (declare (ignore editor-thread))
             nil)))
    (progn
      ;; called *before* any sdl windows are created
      (sdl2:set-hint :video-mac-fullscreen-spaces
                     ;; the sdl2 library expects zero or one NOTE since this
                     ;; is a preference let's not change the default here
                     ;; because it's easy enough to change it via a user's
                     ;; config
                     (if (lem:config :darwin-use-native-fullscreen) 1 0))
      ;; sdl2 should not install any signal handlers, since the lisp runtime already does so
      (sdl2:set-hint :no-signal-handlers 1)
      ;; sdl2 should not disable the kwin compositor, since lem editor is not a game, and disable it will not bring noticeale performance improvement.
      (sdl2:set-hint :video-x11-net-wm-bypass-compositor 0)

      (tmt:with-body-in-main-thread ()
        (sdl2:make-this-thread-main (lambda ()
                                      (handler-bind
                                          (#+(and linux sbcl)
                                              (sb-sys:interactive-interrupt
                                               (lambda (c)
                                                 (declare (ignore c))
                                                 (invoke-restart 'sdl2::abort))))
                                        (progn
                                          (create-display #'thunk)
                                          (when (sbcl-on-darwin-p)
                                            (cffi:foreign-funcall "_exit"))))))))))

(defmethod lem-if:get-background-color ((implementation sdl2))
  (with-debug ("lem-if:get-background-color")
    (display:with-display (display)
      (display:display-background-color display))))

(defmethod lem-if:get-foreground-color ((implementation sdl2))
  (with-debug ("lem-if:get-foreground-color")
    (display:with-display (display)
      (display:display-foreground-color display))))

(defmethod lem-if:update-foreground ((implementation sdl2) color)
  (with-debug ("lem-if:update-foreground" color)
    (display:with-display (display)
      (setf (display:display-foreground-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-background ((implementation sdl2) color)
  (with-debug ("lem-if:update-background" color)
    (display:with-display (display)
      (setf (display:display-background-color display)
            (lem:parse-color color)))))

(defmethod lem-if:update-cursor-shape ((implementation sdl2) cursor-type)
  (with-debug ("lem-if:update-cursor-type")
    (display:with-display (display)
      (setf (display:display-cursor-type display) cursor-type))))

(defmethod lem-if:display-width ((implementation sdl2))
  (with-debug ("lem-if:display-width")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-width display) (display:display-char-width display))))))

(defmethod lem-if:display-height ((implementation sdl2))
  (with-debug ("lem-if:display-height")
    (display:with-display (display)
      (display:with-renderer (display)
        (floor (display:display-height display) (display:display-char-height display))))))

(defmethod lem-if:display-title ((implementation sdl2))
  (with-debug ("lem-if:display-title")
    (display:with-display (display)
      (sdl2:get-window-title (display:display-window display)))))

(defmethod lem-if:set-display-title ((implementation sdl2) title)
  (with-debug ("lem-if:set-display-title")
    (sdl2:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          (sdl2:set-window-title (display:display-window display) title)
          ;; return the title instead of nil
          title)))))

(defmethod lem-if:display-fullscreen-p ((implementation sdl2))
  (with-debug ("lem-if:display-fullscreen-p")
    (display:with-display (display)
      (not (null (member :fullscreen (sdl2:get-window-flags (display:display-window display))))))))

(defmethod lem-if:set-display-fullscreen-p ((implementation sdl2) fullscreen-p)
  (with-debug ("lem-if:set-display-fullscreen-p")
    (sdl2:in-main-thread ()
      (display:with-display (display)
        (display:with-renderer (display)
          ;; always send :desktop over :fullscreen due to weird bugs on macOS
          (sdl2:set-window-fullscreen (display:display-window display)
                                      (if fullscreen-p :desktop)))))))

(defmethod lem-if:maximize-frame ((implementation sdl2))
  (with-debug ("lem-if:maximize-frame")
    (sdl2:in-main-thread ()
      (display:with-display (display)
        (sdl2:maximize-window (lem-sdl2/display::display-window display))))))

(defmethod lem-if:minimize-frame ((implementation sdl2))
  (with-debug ("lem-if:minimize-frame")
    (sdl2:in-main-thread ()
      (display:with-display (display)
        (sdl2:minimize-window (lem-sdl2/display::display-window display))))))

(defmethod lem-if:make-view ((implementation sdl2) window x y width height use-modeline)
  (with-debug ("lem-if:make-view" window x y width height use-modeline)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:create-view display window x y width height use-modeline)))))

(defmethod lem-if:delete-view ((implementation sdl2) view)
  (with-debug ("lem-if:delete-view")
    (display:with-display (display)
      (display:with-renderer (display)
        (view:delete-view view)))))

(defmethod lem-if:clear ((implementation sdl2) view)
  (with-debug ("lem-if:clear" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-clear view display)))))

(defmethod lem-if:set-view-size ((implementation sdl2) view width height)
  (with-debug ("lem-if:set-view-size" view width height)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:resize view display width height)))))

(defmethod lem-if:set-view-pos ((implementation sdl2) view x y)
  (with-debug ("lem-if:set-view-pos" view x y)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:move-position view x y)))))

(defmethod lem-if:redraw-view-before ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-before" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (view:render-border-using-view view display)))))

(defgeneric render (texture window buffer))

(defmethod lem-if:redraw-view-after ((implementation sdl2) view)
  (with-debug ("lem-if:redraw-view-after" view)
    (display:with-display (display)
      (display:with-renderer (display)
        (sdl2:with-rects ((view-rect 0
                                     0
                                     (* (view:view-width view) (display:display-char-width display))
                                     (* (1- (view:view-height view)) (display:display-char-height display))))
          (sdl2:render-set-viewport (display:display-renderer display) view-rect)
          (render (view:view-texture view)
                  (view:view-window view)
                  (lem:window-buffer (view:view-window view)))
          (sdl2:render-set-viewport (display:display-renderer display) nil))
        (view:render-view-texture-to-display view display)))))

(defmethod lem-if:will-update-display ((implementation sdl2))
  (with-debug ("will-update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        (sdl2:set-render-target (display:display-renderer display) (display:display-texture display))
        (display:set-render-color display (display:display-background-color display))
        (sdl2:render-clear (display:display-renderer display))))))

(defun set-input-method (display)
  (let* ((view (lem:window-view (lem:current-window)))
         (cursor-x (view:last-cursor-x view display))
         (cursor-y (view:last-cursor-y view display))
         (text lem-sdl2/keyboard::*textediting-text*)
         (x (+ (* (view:view-x view) (display:display-char-width display))
               cursor-x))
         (y (+ (* (view:view-y view) (display:display-char-height display))
               cursor-y)))
    (sdl2:with-rects ((rect x y (* (display:display-char-width display) (lem:string-width text)) (display:display-char-height display)))
      (sdl2-ffi.functions:sdl-set-text-input-rect rect)
      (when (plusp (length text))
        (let* ((color (display:display-foreground-color display))
               (surface (sdl2-ttf:render-utf8-blended (display:display-cjk-normal-font display)
                                                      text
                                                      (lem:color-red color)
                                                      (lem:color-green color)
                                                      (lem:color-blue color)
                                                      0))
               (texture (sdl2:create-texture-from-surface (display:display-renderer display) surface)))
          (sdl2:with-rects ((rect x y (sdl2:surface-width surface) (sdl2:surface-height surface)))
            (display:set-render-color display (display:display-background-color display))
            (sdl2:render-fill-rect (display:display-renderer display) rect)
            (sdl2:render-copy (display:display-renderer display) texture :dest-rect rect))
          (sdl2:destroy-texture texture))))))

(defmethod lem-if:update-display ((implementation sdl2))
  (with-debug ("lem-if:update-display")
    (display:with-display (display)
      (display:with-renderer (display)
        (setf (display:display-redraw-at-least-once-p display) t)
        (sdl2:set-render-target (display:display-renderer display) nil)
        (sdl2:render-copy (display:display-renderer display) (display:display-texture display))
        (set-input-method display)
        (display:update-display display)))))

(defmethod lem-if:increase-font-size ((implementation sdl2))
  (with-debug ("increase-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (+ (font-config-size font-config) ratio))))))))

(defmethod lem-if:decrease-font-size ((implementation sdl2))
  (with-debug ("decrease-font-size")
    (display:with-display (display)
      (display:with-renderer (display)
        (let ((font-config (display:display-font-config display))
              (ratio (round (first (display:display-scale display)))))
          (display:change-font display
                               (change-size font-config
                                            (- (font-config-size font-config) ratio))))))))

(defmethod lem-if:set-font-size ((implementation sdl2) size)
  (display:with-display (display)
    (display:with-renderer (display)
      (let ((font-config (display:display-font-config display)))
        (display:change-font
         display
         (change-size font-config size))))))

(defmethod lem-if:resize-display-before ((implementation sdl2))
  (with-debug ("resize-display-before")
    (display:with-display (display)
      (display:with-renderer (display)
        (display:clear display)))))

(defmethod lem-if:get-font-list ((implementation sdl2))
  (get-font-list (get-platform)))

(defmethod lem-if:get-mouse-position ((implementation sdl2))
  (display:with-display (display)
    (if (not (cursor-shown-p))
        (values 0 0)
        (multiple-value-bind (x y bitmask)
            (sdl2:mouse-state)
          (declare (ignore bitmask))
          (values (display:scaled-char-width display x)
                  (display:scaled-char-height display y))))))

(defmethod lem-if:get-char-width ((implementation sdl2))
  (display:with-display (display)
    (display:display-char-width display)))

(defmethod lem-if:get-char-height ((implementation sdl2))
  (display:with-display (display)
    (display:display-char-height display)))

(defmethod lem-if:view-width ((implementation sdl2) view)
  (display:with-display (display)
    (* (display:display-char-width display)
       (lem-sdl2/view:view-width view))))

(defmethod lem-if:view-height ((implementation sdl2) view)
  (display:with-display (display)
    (* (display:display-char-height display)
       (lem-sdl2/view:view-height view))))

#-windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        (sdl2-ffi.functions:sdl-get-clipboard-text)))))

#+windows
(defmethod lem-if:clipboard-paste ((implementation sdl2))
  (lem-sdl2/log:with-debug ("clipboard-paste")
    (display:with-display (display)
      (display:with-renderer (display)
        (with-output-to-string (out)
          (let ((text (sdl2-ffi.functions:sdl-get-clipboard-text)))
            (loop :for string :in (split-sequence:split-sequence #\newline text)
                  :do (if (and (< 0 (length string))
                               (char= #\return (char string (1- (length string)))))
                          (write-line (subseq string 0 (1- (length string))) out)
                          (write-string string out)))))))))

(defmethod lem-if:clipboard-copy ((implementation sdl2) text)
  (lem-sdl2/log:with-debug ("clipboard-copy")
    (display:with-display (display)
      (display:with-renderer (display)
        (sdl2-ffi.functions:sdl-set-clipboard-text text)))))

(lem:enable-clipboard)


;; helpers
(defun char-width () (display:display-char-width (display:current-display)))
(defun char-height () (display:display-char-height (display:current-display)))
(defun current-renderer () (display:display-renderer (display:current-display)))
