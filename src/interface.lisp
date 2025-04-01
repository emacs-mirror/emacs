(in-package :lem-core)

(defvar *implementation*)

(defclass implementation ()
  ((name
    :initform (alexandria:required-argument :name)
    :initarg :name
    :reader implementation-name)
   (redraw-after-modifying-floating-window
    :initform nil
    :initarg :redraw-after-modifying-floating-window
    :reader redraw-after-modifying-floating-window)
   (support-floating-window
    :initform t
    :initarg :support-floating-window
    :reader support-floating-window)
   (window-left-margin
    :initform 1
    :initarg :window-left-margin
    :reader window-left-margin)))

(defun get-default-implementation (&key (errorp t) (implementation :ncurses))
  (let* ((classes (c2mop:class-direct-subclasses (find-class 'implementation)))
         (class (case (length classes)
                  (0
                   (when errorp
                     (error "Implementation does not exist.~
                             (probably because you didn't load the lem-ncurses system)")))
                  (1
                   (first classes))
                  (otherwise
                   (dolist (class classes (first classes))
                     (when (string= implementation (class-name class))
                       (return class)))))))
    (when class
      (make-instance class))))

(defvar lem-if:*background-color-of-drawing-window* nil)

(deftype cursor-type ()
  '(member :box :bar :underline))

(defgeneric lem-if:invoke (implementation function))
(defgeneric lem-if:get-background-color (implementation))
(defgeneric lem-if:get-foreground-color (implementation))
(defgeneric lem-if:update-foreground (implementation color-name))
(defgeneric lem-if:update-background (implementation color-name))
(defgeneric lem-if:update-cursor-shape (implementation cursor-type)
  (:method (implementation cursor-type)))
(defgeneric lem-if:display-width (implementation))
(defgeneric lem-if:display-height (implementation))
(defgeneric lem-if:display-title (implementation))
(defgeneric lem-if:set-display-title (implementation title))
(defgeneric lem-if:display-fullscreen-p (implementation))
(defgeneric lem-if:set-display-fullscreen-p (implementation fullscreen-p))
(defgeneric lem-if:maximize-frame (implementation)
  (:method (implementation)))
(defgeneric lem-if:minimize-frame (implementation)
  (:method (implementation)))
(defgeneric lem-if:make-view (implementation window x y width height use-modeline))
(defgeneric lem-if:view-width (implementation view))
(defgeneric lem-if:view-height (implementation view))
(defgeneric lem-if:delete-view (implementation view))
(defgeneric lem-if:clear (implementation view))
(defgeneric lem-if:set-view-size (implementation view width height))
(defgeneric lem-if:set-view-pos (implementation view x y))
(defgeneric lem-if:redraw-view-before (implementation view)
  (:method (implementation view)))
(defgeneric lem-if:redraw-view-after (implementation view)
  (:method (implementation view)))
(defgeneric lem-if:will-update-display (implementation)
  (:method (implementation)))
(defgeneric lem-if:update-display (implementation))

(defgeneric lem-if:display-popup-menu (implementation items
                                       &key action-callback
                                            print-spec
                                            style
                                            max-display-items))
(defgeneric lem-if:popup-menu-update
    (implementation popup-menu items &key print-spec max-display-items keep-focus))
(defgeneric lem-if:popup-menu-quit (implementation popup-menu))
(defgeneric lem-if:popup-menu-down (implementation popup-menu))
(defgeneric lem-if:popup-menu-up (implementation popup-menu))
(defgeneric lem-if:popup-menu-first (implementation popup-menu))
(defgeneric lem-if:popup-menu-last (implementation popup-menu))
(defgeneric lem-if:popup-menu-select (implementation popup-menu))
(defgeneric lem-if:display-context-menu (implementation context-menu style)
  (:method (implementation context-menu style)))

(defgeneric lem-if:clipboard-paste (implementation)
  (:method (implementation)))
(defgeneric lem-if:clipboard-copy (implementation text)
  (:method (implementation text)))

(defgeneric lem-if:increase-font-size (implementation)
  (:method (implementation)))
(defgeneric lem-if:decrease-font-size (implementation)
  (:method (implementation)))
(defgeneric lem-if:set-font-size (implementation size)
  (:method (implementation size)))

(defgeneric lem-if:resize-display-before (implementation)
  (:method (implementation)))

(defgeneric lem-if:get-font-list (implementation)
  (:method (implementation) '()))

(defgeneric lem-if:get-mouse-position (implementation)
  (:method (implementation)
    (values 0 0)))

(defgeneric lem-if:get-char-width (implementation))
(defgeneric lem-if:get-char-height (implementation))

(defgeneric lem-if:render-line (implementation view x y objects height))
(defgeneric lem-if:render-line-on-modeline (implementation view left-objects right-objects
                                            default-attribute height))
(defgeneric lem-if:object-width (implementation drawing-object))
(defgeneric lem-if:object-height (implementation drawing-object))
(defgeneric lem-if:clear-to-end-of-window (implementation view y))

(defgeneric lem-if:js-eval (implementation view code &key wait)
  (:method (implementation view code &key wait)
    (declare (ignore wait))
    (error "unimplemented")))

(defvar *display-background-mode* nil)

(defun implementation ()
  *implementation*)

(defmacro with-implementation (implementation &body body)
  `(let* ((*implementation* ,implementation)
          (bt2:*default-special-bindings*
            (acons '*implementation*
                   *implementation*
                   bt2:*default-special-bindings*)))
     ,@body))

(defun display-background-mode ()
  (or *display-background-mode*
      (if (light-color-p (lem-if:get-background-color (implementation)))
          :light
          :dark)))

(defun set-display-background-mode (mode)
  (check-type mode (member :light :dark nil))
  (setf *display-background-mode* mode))

(defun set-foreground (name)
  (lem-if:update-foreground (implementation) name))

(defun set-background (name)
  (lem-if:update-background (implementation) name))

(defun attribute-foreground-color (attribute)
  (or (and attribute
           (parse-color (attribute-foreground attribute)))
      (lem-if:get-foreground-color (implementation))))

(defun attribute-background-color (attribute)
  (or (and attribute
           (parse-color (attribute-background attribute)))
      (lem-if:get-background-color (implementation))))

(defun attribute-foreground-with-reverse (attribute)
  (if (and attribute (attribute-reverse attribute))
      (attribute-background-color attribute)
      (attribute-foreground-color attribute)))

(defun attribute-background-with-reverse (attribute)
  (if (and attribute (attribute-reverse attribute))
      (attribute-foreground-color attribute)
      (attribute-background-color attribute)))

(defun display-width () (lem-if:display-width (implementation)))
(defun display-height () (lem-if:display-height (implementation)))
(defun display-title () (lem-if:display-title (implementation)))
(defun (setf display-title) (title)
  (lem-if:set-display-title (implementation) title))
(defun display-fullscreen-p () (lem-if:display-fullscreen-p (implementation)))
(defun (setf display-fullscreen-p) (fullscreen-p)
  (lem-if:set-display-fullscreen-p (implementation) fullscreen-p))

(defun invoke-frontend (function &key (implementation
                                       (get-default-implementation)))
  (setf *implementation* implementation)
  (lem-if:invoke implementation function))
