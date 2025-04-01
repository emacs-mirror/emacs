(in-package :lem-core)

(defvar *attributes* '())

(defclass attribute ()
  ((foreground
    :initarg :foreground
    :reader attribute-foreground)
   (background
    :initarg :background
    :reader attribute-background)
   (reverse
    :initarg :reverse
    :reader attribute-reverse)
   (bold
    :initarg :bold
    :reader attribute-bold)
   (underline
    :initarg :underline
    :reader attribute-underline)
   (cache
    :initform nil
    :accessor attribute-cache)
   (plist
    :initarg :plist
    :initform nil
    :accessor attribute-plist)))

(defmethod print-object ((attribute attribute) stream)
  (print-unreadable-object (attribute stream :type t :identity t)
    (format stream "(~A ~A)~:[~; reverse~]~:[~; bold~]~:[~; underline~]"
            (or (attribute-foreground attribute) "")
            (or (attribute-background attribute) "")
            (attribute-reverse attribute)
            (attribute-bold attribute)
            (attribute-underline attribute))))

(defun attribute-p (x)
  (typep x 'attribute))

(defun attribute-value (attribute key)
  (getf (attribute-plist attribute) key))

(defun (setf attribute-value) (value attribute key)
  (setf (getf (attribute-plist attribute) key) value))

(defun make-attribute (&key foreground background reverse bold underline plist)
  (make-instance 'attribute
                 :foreground (or (ensure-color foreground) nil)
                 :background (or (ensure-color background) nil)
                 :reverse reverse
                 :bold bold
                 :underline (or (ensure-color underline) underline)
                 :plist plist))

(defun ensure-attribute (x &optional (errorp t))
  (cond ((symbolp x)
         (let ((fn (get x 'attribute)))
           (cond (fn (funcall fn))
                 (errorp (error "invalid attribute: ~A" x))
                 (t nil))))
        ((attribute-p x)
         x)
        (t
         nil)))

(defun merge-attribute (under over)
  (make-attribute :foreground (or (attribute-foreground over)
                                  (attribute-foreground under))
                  :background (or (attribute-background over)
                                  (attribute-background under))
                  :bold (or (attribute-bold over)
                            (attribute-bold under))
                  :reverse (or (attribute-reverse over)
                               (attribute-reverse under))
                  :underline (or (attribute-underline over)
                                 (attribute-underline under))
                  :plist (append (attribute-plist over)
                                 (attribute-plist under))))

(defun attribute-equal (attribute1 attribute2)
  (if (or (null attribute1) (null attribute2))
      (and (null attribute1) (null attribute2))
      (let ((attribute1 (lem-core:ensure-attribute attribute1))
            (attribute2 (lem-core:ensure-attribute attribute2)))
        (and (equal (attribute-foreground attribute1)
                    (attribute-foreground attribute2))
             (equal (attribute-background attribute1)
                    (attribute-background attribute2))
             (equal (attribute-reverse attribute1)
                    (attribute-reverse attribute2))
             (equal (attribute-bold attribute1)
                    (attribute-bold attribute2))
             (equal (attribute-underline attribute1)
                    (attribute-underline attribute2))))))

(defun set-attribute (attribute &key (foreground nil foregroundp)
                                     (background nil backgroundp)
                                     reverse bold underline)
  (let ((attribute (ensure-attribute attribute t)))
    (setf (attribute-cache attribute) nil)
    (when foregroundp
      (setf (slot-value attribute 'foreground) foreground))
    (when backgroundp
      (setf (slot-value attribute 'background) background))
    (setf (slot-value attribute 'reverse) reverse)
    (setf (slot-value attribute 'bold) bold)
    (setf (slot-value attribute 'underline) underline)))

(macrolet ((def (setter slot-name)
             `(defun ,setter (attribute value)
                (let ((attribute (ensure-attribute attribute t)))
                  (setf (attribute-cache attribute) nil)
                  (setf (slot-value attribute ',slot-name) value)))))
  (def set-attribute-foreground foreground)
  (def set-attribute-background background)
  (def set-attribute-reverse reverse)
  (def set-attribute-bold bold)
  (def set-attribute-underline underline))

(defun clear-all-attribute-cache ()
  (dolist (attribute *attributes*)
    (setf (get attribute '%attribute-value) nil)))

(defun get-attribute-cache (attribute &rest args &key background)
  (declare (ignore background))
  (if (null (attribute-cache attribute))
      nil
      (cdr (assoc args
                  (attribute-cache attribute)
                  :test #'equal))))

(defun (setf get-attribute-cache) (value attribute &rest args &key background)
  (declare (ignore background))
  (alexandria:if-let ((elt (assoc args
                                  (attribute-cache attribute)
                                  :test #'equal)))
    (setf (cdr elt) value)
    (setf (attribute-cache attribute)
          (acons args
                 value
                 (attribute-cache attribute))))
  value)

(defun display-light-p ()
  (eq :light (display-background-mode)))

(defun display-dark-p ()
  (eq :dark (display-background-mode)))

(defmacro define-attribute (name &body specs)
  (check-type name symbol)
  `(progn
     ,(when (eql *package* (symbol-package name))
        ;; for the purpose of source location.
        `(defvar ,name))
     (pushnew ',name *attributes*)
     (setf (get ',name '%attribute-value) nil)
     (setf (get ',name 'attribute)
           (lambda ()
             (or (get ',name '%attribute-value)
                 (setf (get ',name '%attribute-value)
                       (cond
                         ,@(if (null specs)
                               `((t (make-attribute)))
                               (loop :for (pattern . args) :in specs
                                     :collect (if (eq pattern t)
                                                  `(t (make-attribute ,@args))
                                                  `((or ,@(mapcar
                                                           (lambda (p)
                                                             (cond ((eq p :light)
                                                                    '(display-light-p))
                                                                   ((eq p :dark)
                                                                    `(display-dark-p))))
                                                           (alexandria:ensure-list pattern)))
                                                    (make-attribute ,@args))))))))))
     ',name))

(define-attribute cursor
  (:light :background "black")
  (:dark :background "white"))

(define-attribute fake-cursor
  (:light :foreground "white" :background "blue")
  (:dark :foreground "black" :background "yellow"))

(define-attribute region
  (:light :foreground nil :background "#eedc82")
  (:dark :foreground nil :background "blue"))

(define-attribute modeline
  (t :bold t :background "#404040" :foreground "white"))

(define-attribute modeline-inactive
  (t :bold t :background "#212121" :foreground "#707070"))

(define-attribute truncate-attribute)

(define-attribute special-char-attribute
  (t :foreground "red"))

(define-attribute compiler-note-attribute
  (t :underline "red"))

(define-attribute syntax-warning-attribute
  (t :foreground "red"))

(define-attribute syntax-string-attribute
  (:light :foreground "#8B2252")
  (:dark :foreground "light salmon"))

(define-attribute syntax-comment-attribute
  (:light :foreground "#cd0000")
  (:dark :foreground "chocolate1"))

(define-attribute syntax-keyword-attribute
  (:light :foreground "purple")
  (:dark :foreground "cyan1"))

(define-attribute syntax-constant-attribute
  (:light :foreground "#ff00ff")
  (:dark :foreground "LightSteelBlue"))

(define-attribute syntax-function-name-attribute
  (:light :foreground "#0000ff")
  (:dark :foreground "LightSkyBlue"))

(define-attribute syntax-variable-attribute
  (:light :foreground "#8D5232")
  (:dark :foreground "LightGoldenrod"))

(define-attribute syntax-type-attribute
  (:light :foreground "#00875f")
  (:dark :foreground "PaleGreen"))

(define-attribute syntax-builtin-attribute
  (t :foreground "#FF87FF"))

(define-attribute document-header1-attribute
  (:light :foreground "#000000" :bold t)
  (:dark :foreground "#FFFFFF" :bold t))

(define-attribute document-header2-attribute
  (:light :foreground "#005A9C" :bold t)
  (:dark :foreground "#87CEFA" :bold t))

(define-attribute document-header3-attribute
  (:light :foreground "#0000FF" :bold t)
  (:dark :foreground "#ADD8E6" :bold t))

(define-attribute document-header4-attribute
  (:light :foreground "#8B008B" :bold t)
  (:dark :foreground "#DDA0DD" :bold t))

(define-attribute document-header5-attribute
  (:light :foreground "#A0522D" :bold t)
  (:dark :foreground "#F4A460" :bold t))

(define-attribute document-header6-attribute
  (:light :foreground "#2E8B57" :bold t)
  (:dark :foreground "#98FB98" :bold t))

(define-attribute document-bold-attribute
  (t :bold t))

(define-attribute document-italic-attribute
  (:light :foreground "#8B4513")
  (:dark :foreground "#DEB887"))

(define-attribute document-underline-attribute
  (t :underline t))

(define-attribute document-link-attribute
  (:light :foreground "#0000EE" :underline t)
  (:dark :foreground "#87CEFA" :underline t))

(define-attribute document-list-attribute
  (:light :foreground "#8B4513")
  (:dark :foreground "#DEB887"))

(define-attribute document-code-block-attribute
  (:light :background "#F0F0F0" :foreground "#000000")
  (:dark :background "#2F4F4F" :foreground "#E0FFFF"))

(define-attribute document-inline-code-attribute
  (:light :background "#F0F0F0" :foreground "#8B008B")
  (:dark :background "#2F4F4F" :foreground "#FF69B4"))

(define-attribute document-blockquote-attribute
  (:light :foreground "#696969")
  (:dark :foreground "#D3D3D3"))

(define-attribute document-table-attribute
  (:light :foreground "#000000" :background "#E6E6FA")
  (:dark :foreground "#FFFFFF" :background "#4B0082"))

(define-attribute document-task-list-attribute
  (:light :foreground "#228B22")
  (:dark :foreground "#98FB98"))

(define-attribute document-metadata-attribute
  (:light :foreground "#4682B4")
  (:dark :foreground "#87CEEB"))

(defun attribute-value* (attribute key)
  (let ((attribute (ensure-attribute attribute nil)))
    (when attribute
      (attribute-value attribute key))))

(defun attribute-image (attribute)
  (attribute-value* attribute :image))

(defun attribute-width (attribute)
  (attribute-value* attribute :width))

(defun attribute-height (attribute)
  (attribute-value* attribute :height))

(defun attribute-font (attribute)
  (attribute-value* attribute :font))

(defun cursor-attribute-p (attribute)
  (and (attribute-p attribute)
       (attribute-value attribute :cursor)))

(defun set-cursor-attribute (attribute &key (value t))
  (setf (attribute-value attribute :cursor) value))
