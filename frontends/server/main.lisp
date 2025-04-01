(defpackage :lem-server
  (:use :cl
        :lem-server/utils
        :lem-server/view)
  (:local-nicknames (:display :lem-core/display)
                    (:queue :lem/common/queue)
                    (:mouse :lem-server/mouse))
  (:export :run-tcp-server
           :run-stdio-server
           :run-websocket-server
           :main))
(in-package :lem-server)

(defvar *server-runner*)

(defclass server-runner ()
  ())

;;;
(defclass websocket-server-runner (server-runner)
  ((port :initarg :port
         :reader websocket-server-runner-port)
   (host :initarg :host
         :reader websocket-server-runner-host)))

(defmethod server-listen ((runner websocket-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :websocket
                         :port (websocket-server-runner-port runner)
                         :host (websocket-server-runner-host runner)
                         :clack-handler 'clack-handler))

(defun clack-handler (env)
  (unless (wsd:websocket-p env)
    (let ((path (getf env :path-info)))
      (cond ((string= "/" path)
             `(200 (:content-type "text/html")
                   ,(asdf:system-relative-pathname :lem-server
                                                    #p"frontend/dist/index.html")))
            ((alexandria:starts-with-subseq "/assets/" path)
             `(200 (:content-type "application/javascript")
                   ,(asdf:system-relative-pathname :lem-server
                                                   (format nil
                                                           "frontend/dist/~A"
                                                           (string-left-trim "/" path)))))
            (t
             '(200 () ("ok")))))))

;;;
(defclass stdio-server-runner (server-runner)
  ())

(defmethod server-listen ((runner stdio-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :stdio))

;;;
(defclass local-domain-socket-server-runner (server-runner)
  ((address :initarg :address :reader local-domain-socket-server-runner-address)))

(defmethod server-listen ((runner local-domain-socket-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :local-domain-socket
                         :address (local-domain-socket-server-runner-address runner)))

(defclass server (jsonrpc:server) ())

(defclass jsonrpc (lem:implementation)
  ((server :initform (make-instance 'server)
           :reader jsonrpc-server)
   (display-width :initform 80
                  :accessor jsonrpc-display-width)
   (display-height :initform 24
                   :accessor jsonrpc-display-height)
   (background-color :accessor jsonrpc-background-color)
   (foreground-color :accessor jsonrpc-foreground-color)
   (message-queue :initform (queue:make-queue)
                  :reader jsonrpc-message-queue)
   (editor-thread :initform nil
                  :accessor jsonrpc-editor-thread))
  (:default-initargs
   :name :jsonrpc
   :redraw-after-modifying-floating-window t
   :window-left-margin 1))

(defun get-all-views ()
  (if (null (lem:current-frame))
      (vector)
      (coerce
       (loop :for frame :in (lem:all-frames)
             :append (loop :for window :in (append (lem:frame-header-windows frame)
                                                   (lem:window-list frame)
                                                   (lem:frame-floating-windows frame))
                           :collect (lem:window-view window)))
       'vector)))

(defmethod resize-display ((jsonrpc jsonrpc) width height)
  (setf (jsonrpc-display-width jsonrpc) width
        (jsonrpc-display-height jsonrpc) height))

(defmethod notify ((jsonrpc jsonrpc) method argument)
  (jsonrpc:broadcast (jsonrpc-server jsonrpc) method argument))

(defmethod notify* ((jsonrpc jsonrpc) method argument)
  (queue:enqueue (jsonrpc-message-queue jsonrpc)
                 (hash "method" method "argument" argument)))

(defmethod notify-all ((jsonrpc jsonrpc))
  (let ((argument (coerce (loop :until (queue:empty-p (jsonrpc-message-queue jsonrpc))
                                :collect (queue:dequeue (jsonrpc-message-queue jsonrpc)))
                          'vector)))
    (notify jsonrpc "bulk" argument)))

(defun handle-login (jsonrpc logged-in-callback params)
  (log:info "ready: ~A ~A" jsonrpc/connection:*connection* (pretty-json params))
  (with-error-handler ()
    (let* ((size (gethash "size" params))
           (foreground (gethash "foreground" params))
           (background (gethash "background" params)))

      (when size
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display jsonrpc width height)))
      (when background
        (alexandria:when-let (color (lem:parse-color background))
          (setf (jsonrpc-background-color jsonrpc) color)))
      (when foreground
        (alexandria:when-let (color (lem:parse-color foreground))
          (setf (jsonrpc-foreground-color jsonrpc) color)))
      (funcall logged-in-callback)

      (let ((response (hash "views" (with-error-handler () (get-all-views))
                            "foreground" (lem-core::foreground-color)
                            "background" (lem-core::background-color)
                            "size" (hash "width" (lem:display-width)
                                         "height" (lem:display-height)))))
        (log:info "login response: ~A" (pretty-json response))
        response))))

(defun login (jsonrpc logged-in-callback)
  (lambda (params)
    (handle-login jsonrpc logged-in-callback params)))

(defun redraw (args)
  (log:info "redraw: ~A" (pretty-json args))
  (with-error-handler ()
    (let ((size (and args (gethash "size" args))))
      (when size
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display (lem:implementation) width height)
          (notify (lem:implementation) "resize-display" size)))
      (lem:send-event (lambda ()
                        (lem-core::adjust-all-window-size)
                        (lem:redraw-display :force t))))))

(defmethod lem-if:invoke ((jsonrpc jsonrpc) function)
  (let ((ready nil))
    (setf (jsonrpc-editor-thread jsonrpc)
          (funcall function
                   (lambda ()
                     (loop :until ready)
                     (notify jsonrpc "startup" nil))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "login"
                    (login jsonrpc
                           (lambda ()
                             (setf ready t))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "input"
                    (lambda (args)
                      (input-callback jsonrpc args)))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "redraw"
                    'redraw)
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "got-clipboard-text"
                    'got-clipboard-text)

    (lem:add-hook lem:*exit-editor-hook*
                  (lambda ()
                    (notify jsonrpc "exit" nil)
                    (uiop:quit 0)))

    (server-listen *server-runner* (jsonrpc-server jsonrpc))))

(defmethod lem-if:get-background-color ((jsonrpc jsonrpc))
  (jsonrpc-background-color jsonrpc))

(defmethod lem-if:get-foreground-color ((jsonrpc jsonrpc))
  (jsonrpc-foreground-color jsonrpc))

(defmethod lem-if:update-foreground ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (notify jsonrpc "update-foreground" color-name)))

(defmethod lem-if:update-background ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (notify jsonrpc "update-background" color-name)))

(defmethod lem-if:update-cursor-shape ((jsonrpc jsonrpc) cursor-type)
  ;; TODO
  )

(defmethod lem-if:display-width ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-width jsonrpc)))

(defmethod lem-if:display-height ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-height jsonrpc)))

(defmethod lem-if:display-title ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-title ((jsonrpc jsonrpc) title)
  ;; TODO
  )

(defmethod lem-if:display-fullscreen-p ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-fullscreen-p ((jsonrpc jsonrpc) fullscreen-p)
  ;; TODO
  )

(defmethod lem-if:make-view ((jsonrpc jsonrpc) window x y width height use-modeline)
  (let ((view (make-view :window window
                         :x x
                         :y y
                         :width width
                         :height height
                         :use-modeline use-modeline
                         :kind (if (lem:floating-window-p window)
                                   "floating"
                                   "tile")
                         :border (and (lem:floating-window-p window)
                                      (lem:floating-window-border window))
                         :border-shape (and (lem:floating-window-p window)
                                            (lem:floating-window-border-shape window)))))
    (notify* jsonrpc "make-view" view)
    view))

(defmethod lem-if:view-width ((jsonrpc jsonrpc) view)
  (view-width view))

(defmethod lem-if:view-height ((jsonrpc jsonrpc) view)
  (view-height view))

(defmethod lem-if:delete-view ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify* jsonrpc "delete-view" (hash "viewInfo" view))))

(defmethod lem-if:clear ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify* jsonrpc "clear" (hash "viewInfo" view))))

(defmethod lem-if:set-view-size ((jsonrpc jsonrpc) view width height)
  (with-error-handler ()
    (resize-view view width height)
    (notify* jsonrpc
             "resize-view"
             (hash "viewInfo" view
                   "width" width
                   "height" height))))

(defmethod lem-if:set-view-pos ((jsonrpc jsonrpc) view x y)
  (with-error-handler ()
    (move-view view x y)
    (notify* jsonrpc
             "move-view"
             (hash "viewInfo" view
                   "x" x
                   "y" y))))

(defmethod lem-if:redraw-view-before ((jsonrpc jsonrpc) view)
  )

(defmethod lem-if:redraw-view-after ((jsonrpc jsonrpc) view)
  (notify* jsonrpc
           "redraw-view-after"
           (hash "viewInfo" view)))

(defmethod lem:redraw-buffer ((jsonrpc jsonrpc) (buffer lem:html-buffer) window force)
  )

(defmethod lem-if:will-update-display ((jsonrpc jsonrpc))
  )

(defmethod lem-if:update-display ((jsonrpc jsonrpc))
  (with-error-handler ()
    (let ((view (lem:window-view (lem:current-window)))
          (x (lem:last-print-cursor-x (lem:current-window)))
          (y (lem:last-print-cursor-y (lem:current-window))))
      (notify* jsonrpc
               "move-cursor"
               (hash "viewInfo" view "x" x "y" y)))
    (notify* jsonrpc "update-display" nil)
    (notify-all jsonrpc)))

(defvar *clipboard-wait-queue* (lem/common/queue:make-concurrent-queue))

(defmethod lem-if:clipboard-paste ((jsonrpc jsonrpc))
  (notify jsonrpc "get-clipboard-text" (hash))
  (lem/common/queue:dequeue *clipboard-wait-queue* :timeout 0.1))

(defun got-clipboard-text (params)
  (let ((text (gethash "text" params)))
    (lem/common/queue:enqueue *clipboard-wait-queue* text)))

(defmethod lem-if:clipboard-copy ((jsonrpc jsonrpc) text)
  (notify jsonrpc "set-clipboard-text" (hash "text" text)))

(defmethod lem-if:increase-font-size ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:decrease-font-size ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:resize-display-before ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-font-list ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-mouse-position ((jsonrpc jsonrpc))
  (mouse:get-position))

(defmethod lem-if:get-char-width ((jsonrpc jsonrpc))
  ;; TODO
  1)
(defmethod lem-if:get-char-height ((jsonrpc jsonrpc))
  ;; TODO
  1)

(defmethod lem-if:js-eval ((jsonrpc jsonrpc) view code &key wait)
  (let ((params (hash "viewInfo" view "code" code)))
    (if wait
        (let ((mailbox (sb-concurrency:make-mailbox :name "js-eval-mailbox")))
          (apply #'values
                 (loop :for connection
                       :in (jsonrpc/server::server-client-connections
                            (jsonrpc-server (lem:implementation)))
                       :do (jsonrpc:call-async-to
                            (jsonrpc-server (lem:implementation))
                            connection
                            "js-eval"
                            params
                            (lambda (res)
                              (sb-concurrency:send-message mailbox (list t res)))
                            (lambda (message code)
                              (sb-concurrency:send-message
                               mailbox
                               (list nil
                                     (make-condition 'jsonrpc/errors:jsonrpc-callback-error
                                                     :message message
                                                     :code code)))))))
          (destructuring-bind (ok value)
              (sb-concurrency:receive-message mailbox)
            (if ok
                value
                (error value))))
        (notify (lem:implementation) "js-eval" params))))

(lem:add-hook lem:*switch-to-buffer-hook* 'on-switch-to-buffer)

(defun on-switch-to-buffer (buffer)
  (cond ((and (typep buffer 'lem:html-buffer)
              (lem:html-buffer-updated-p buffer))
         (lem:invalidate-html-buffer-updated buffer)
         (notify* (lem:implementation)
                  "change-view"
                  (hash "viewInfo" (lem:window-view (lem:current-window))
                        "type" "html"
                        "content" (lem:html-buffer-html buffer))))
        ((and (typep (lem:current-buffer) 'lem:html-buffer)
              (not (typep buffer 'lem:html-buffer)))
         (notify* (lem:implementation)
                  "change-view"
                  (hash "viewInfo" (lem:window-view (lem:current-window))
                        "type" "editor")))
        ((and (not (typep (lem:current-buffer) 'lem:html-buffer))
              (typep buffer 'lem:html-buffer))
         (notify* (lem:implementation)
                  "change-view"
                  (hash "viewInfo" (lem:window-view (lem:current-window))
                        "type" "html"
                        "content" (lem:html-buffer-html buffer))))))
;;;;
(defun bool (x) (if x 'yason:true 'yason:false))

(defun ensure-rgb (color)
  (typecase color
    (lem:color
     (lem:color-to-hex-string color))
    (string
     (lem:color-to-hex-string (lem:parse-color color)))
    (otherwise
     color)))

(defmethod yason:encode ((attribute lem:attribute) &optional (stream *standard-output*))
  (with-error-handler ()
    (yason:with-output (stream)
      (yason:with-object ()
        (yason:encode-object-element "foreground" (ensure-rgb (lem:attribute-foreground attribute)))
        (yason:encode-object-element "background" (ensure-rgb (lem:attribute-background attribute)))
        (yason:encode-object-element "reverse" (bool (lem:attribute-reverse attribute)))
        (yason:encode-object-element "bold" (bool (lem:attribute-bold attribute)))
        (yason:encode-object-element "underline" (lem:attribute-underline attribute))))))



;;; drawing
(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object display:void-object))
  0)

(defmethod object-width ((drawing-object display:text-object))
  (lem-core:string-width (display:text-object-string drawing-object)))

(defmethod object-width ((drawing-object display:eol-cursor-object))
  0)

(defmethod object-width ((drawing-object display:extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object display:line-end-object))
  0)

(defmethod object-width ((drawing-object display:image-object))
  0)

(defgeneric draw-object (jsonrpc object x y view))

(defmethod draw-object (jsonrpc (object display:void-object) x y view)
  (values))

(defvar *put-target* :edit-area)

(defun ensure-attribute (attribute)
  (let ((attribute (lem:ensure-attribute attribute nil)))
    (when (and lem-if:*background-color-of-drawing-window*
               (null attribute))
      (setf attribute (lem:make-attribute :background lem-if:*background-color-of-drawing-window*)))
    attribute))

(defun put (jsonrpc view x y string attribute)
  (with-error-handler ()
    (notify* jsonrpc
             (ecase *put-target*
               (:edit-area "put")
               (:modeline "modeline-put"))
             (hash "viewInfo" view
                   "x" x
                   "y" y
                   "text" string
                   "textWidth" (lem:string-width string)
                   "attribute" (ensure-attribute attribute)))))

(defmethod draw-object (jsonrpc (object display:text-object) x y view)
  (let* ((string (display:text-object-string object))
         (attribute (display:text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (view-window view) x y))
    (put jsonrpc view x y string attribute)))

(defmethod draw-object (jsonrpc (object display:eol-cursor-object) x y view)
  (lem-core::set-last-print-cursor (view-window view) x y)
  (put jsonrpc view x y " "
       (lem:make-attribute
        :background
        (lem:color-to-hex-string (display:eol-cursor-object-color object)))))

(defmethod draw-object (jsonrpc (object display:extend-to-eol-object) x y view)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (put jsonrpc view x y
           (make-string (- width x) :initial-element #\space)
           (lem:make-attribute
            :background
            (lem:color-to-hex-string (display:extend-to-eol-object-color object)))))))

(defmethod draw-object (jsonrpc (object display:line-end-object) x y view)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object)))
    (put jsonrpc
         view
         (+ x (display:line-end-object-offset object))
         y
         string
         attribute)))

(defmethod draw-object (jsonrpc (object display:image-object) x y view)
  (values))

(defun render-line (jsonrpc view x y objects)
  (loop :for object :in objects
        :do (draw-object jsonrpc object x y view)
            (incf x (object-width object))))

(defun render-line-from-behind (jsonrpc view y objects)
  (loop :with current-x := (view-width view)
        :for object :in objects
        :do (decf current-x (object-width object))
            (draw-object jsonrpc object current-x y view)))

(defmethod lem-if:render-line ((jsonrpc jsonrpc) view x y objects height)
  (with-error-handler ()
    (notify* jsonrpc
             "clear-eol"
             (hash "viewInfo" view
                   "x" x
                   "y" y))
    (render-line jsonrpc view x y objects)))

(defmethod lem-if:render-line-on-modeline ((jsonrpc jsonrpc) view left-objects right-objects
                                           default-attribute height)
  (let ((*put-target* :modeline))
    (with-error-handler ()
      (notify* jsonrpc
               "modeline-put"
               (hash "viewInfo" view
                     "x" 0
                     "y" 0
                     "text" (make-string (view-width view) :initial-element #\space)
                     "textWidth" (view-width view)
                     "attribute" default-attribute))
      (render-line jsonrpc view 0 0 left-objects)
      (render-line-from-behind jsonrpc view 0 right-objects))))

(defmethod lem-if:object-width ((jsonrpc jsonrpc) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((jsonrpc jsonrpc) drawing-object)
  1)

(defmethod lem-if:clear-to-end-of-window ((jsonrpc jsonrpc) view y)
  (notify* jsonrpc
           "clear-eob"
           (hash "viewInfo" view
                 "x" 0
                 "y" y)))


;;;
(defconstant +abort+ 0)
(defconstant +keyevent+ 1)
(defconstant +resize+ 2)
(defconstant +input-string+ 3)

(defun convert-keyevent (e)
  (let ((key (gethash "key" e))
        (ctrl (gethash "ctrl" e))
        (meta (gethash "meta" e))
        (super (gethash "super" e))
        (shift (gethash "shift" e)))
    (cond ((string= key " ") (setf key "Space")))
    (lem:make-key :ctrl ctrl
                  :meta meta
                  :super super
                  :shift (if (lem:insertion-key-sym-p key)
                             nil
                             shift)
                  :sym (if (and (lem:insertion-key-sym-p key)
                                shift
                                meta)
                           (string-upcase key)
                           key))))

(defun convert-button (button)
  (case button
    (0 :button-1)
    (2 :button-3)
    (1 :button-2)))

(defun input-callback (jsonrpc args)
  (handler-case
      (let ((kind (gethash "kind" args))
            (value (gethash "value" args)))
        (alexandria:switch (kind :test #'equal)
          ("abort"
           (lem:send-abort-event (jsonrpc-editor-thread jsonrpc) nil))
          ("key"
           (when value
             (notify jsonrpc
                     "user-input"
                     (hash "value" value))
             (let ((key (convert-keyevent value)))
               (lem:send-event key))))
          ("clipboard-paste"
           (lem:send-event
            (lambda ()
              (let ((text (lem:get-clipboard-data))
                    (mode (lem:ensure-mode-object
                           (lem:current-major-mode-at-point
                            (lem:current-point)))))
                (lem:paste-using-mode mode text)))))
          ("mousedown"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value)))
                 (clicks (gethash "clicks" value)))
             (lem:send-event
              (lambda ()
                (lem:receive-mouse-button-down x
                                               y
                                               pixel-x
                                               pixel-y
                                               button
                                               clicks)))))
          ("mouseup"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value))))
             (when button
               (lem:send-event
                (lambda ()
                  (lem:receive-mouse-button-up x
                                               y
                                               pixel-x
                                               pixel-y
                                               button))))))
          ("mousemove"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (button (convert-button (gethash "button" value))))
             (lem:send-event
              (lambda ()
                (mouse:update-position x y)
                (lem:receive-mouse-motion x
                                          y
                                          pixel-x
                                          pixel-y
                                          button)))))
          ("wheel"
           (let ((x (gethash "x" value))
                 (y (gethash "y" value))
                 (pixel-x (gethash "pixelX" value))
                 (pixel-y (gethash "pixelY" value))
                 (wheel-x (gethash "wheelX" value))
                 (wheel-y (gethash "wheelY" value)))
             (lem:send-event
              (lambda ()
                (lem:receive-mouse-wheel x y pixel-x pixel-y wheel-x wheel-y)
                (when (= 0 (lem:event-queue-length))
                  (lem:redraw-display))))))
          ("resize"
           (resize-display jsonrpc
                           (gethash "width" value)
                           (gethash "height" value))
           (lem:send-event :resize))
          ("input-string"
           (notify jsonrpc
                   "user-input"
                   (hash "value" value))
           (loop :for c :across value
                 :for key := (convert-keyevent
                              (alexandria:plist-hash-table (list "key" (string c))
                                                           :test 'equal))
                 :do (lem:send-event key)))
          (otherwise (error "unexpected input kind: ~D" kind))))
    (error (e)
      (log:info "input-callback: ~A ~A" e
              (with-output-to-string (stream)
                (let ((stream (yason:make-json-output-stream stream)))
                  (yason:encode args stream)))))))

;;;
(defparameter +command-line-spec+
  '(("mode" :type string :optional t :documentation "\"websocket\", \"stdio\", \"local-domain-socket\"")
    ("port" :type integer :optional nil :documentation "port of \"websocket\"")
    ("host" :type string :optional t)
    ("address" :type string :optional t :documentation "address of \"local-domain-socket\"")))

(defun run-websocket-server (&key (port 50000) (hostname "127.0.0.1"))
  (let ((*server-runner*
          (make-instance 'websocket-server-runner
                         :port port
                         :host hostname)))
    (lem:lem)))

(defun run-stdio-server ()
  (let ((*server-runner* (make-instance 'stdio-server-runner)))
    (lem:lem)))

(defun run-local-domain-socket-server (&key address)
  (let ((*server-runner* (make-instance 'local-domain-socket-server-runner
                                        :address address)))
    (lem:lem)))

(defun check-port-specified (port)
  (unless port
    (command-line-arguments:show-option-help +command-line-spec+)
    (uiop:quit 1)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   (lambda (&key (mode "websocket")
                 port
                 (host "127.0.0.1")
                 address)
     (vom:config t :info)
     (cond ((string= mode "websocket")
            (check-port-specified port)
            (format t
                    "~%~4Topen http://localhost:~D/ in your browser~2%"
                    port)
            (run-websocket-server :port port
                                  :hostname host))
           ((string= mode "stdio")
            (run-stdio-server))
           ((string= mode "local-domain-socket")
            (run-local-domain-socket-server :address address))
           (t
            (command-line-arguments:show-option-help +command-line-spec+)
            (uiop:quit 1))))
   :name "lem-server"
   :positional-arity 0
   :command-line args))
