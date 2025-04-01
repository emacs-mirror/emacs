(in-package :lem-core)

(defvar *line-wrap* nil)

(defun window-view-width (window)
  (lem-if:view-width (implementation) (window-view window)))

(defun window-view-height (window)
  (lem-if:view-height (implementation) (window-view window)))

(defun drawing-cache (window)
  (window-parameter window 'redrawing-cache))

(defun (setf drawing-cache) (value window)
  (setf (window-parameter window 'redrawing-cache) value))

(defclass drawing-object ()
  ())

(defclass void-object (drawing-object) ())

(defclass text-object (drawing-object)
  ((surface :initarg :surface :initform nil :accessor text-object-surface)
   (string :initarg :string :reader text-object-string)
   (attribute :initarg :attribute :reader text-object-attribute)
   (type :initarg :type :reader text-object-type)
   (within-cursor :initform nil :initarg :within-cursor :reader text-object-within-cursor-p)))

(defclass control-character-object (text-object) ())

(defclass icon-object (text-object) ())
(defclass folder-object (text-object) ())
(defclass emoji-object (text-object) ())

(defclass eol-cursor-object (drawing-object)
  ((color :initarg :color
          :reader eol-cursor-object-color)
   (attribute :initarg :attribute
              :reader eol-cursor-object-attribute)
   (true-cursor-p :initarg :true-cursor-p
                  :initform nil
                  :reader eol-cursor-object-true-cursor-p)))

(defclass extend-to-eol-object (drawing-object)
  ((color :initarg :color
          :reader extend-to-eol-object-color)))

(defclass line-end-object (text-object)
  ((offset :initarg :offset
           :reader line-end-object-offset)))

(defclass image-object (drawing-object)
  ((image :initarg :image :reader image-object-image)
   (width :initarg :width :reader image-object-width)
   (height :initarg :height :reader image-object-height)
   (attribute :initarg :attribute :reader image-object-attribute)))

(defmethod cursor-object-p (drawing-object)
  nil)

(defmethod cursor-object-p ((drawing-object text-object))
  (text-object-within-cursor-p drawing-object))

(defmethod cursor-object-p ((drawing-object eol-cursor-object))
  t)

(defgeneric drawing-object-equal (drawing-object-1 drawing-object-2))

(defmethod drawing-object-equal (drawing-object-1 drawing-object-2)
  nil)

(defmethod drawing-object-equal ((drawing-object-1 void-object) (drawing-object-2 void-object))
  t)

(defmethod drawing-object-equal ((drawing-object-1 text-object) (drawing-object-2 text-object))
  (and (equal (text-object-string drawing-object-1)
              (text-object-string drawing-object-2))
       (attribute-equal (text-object-attribute drawing-object-1)
                        (text-object-attribute drawing-object-2))
       (eq (text-object-type drawing-object-1)
           (text-object-type drawing-object-2))
       (eq (text-object-within-cursor-p drawing-object-1)
           (text-object-within-cursor-p drawing-object-2))))

(defmethod drawing-object-equal ((drawing-object-1 eol-cursor-object) (drawing-object-2 eol-cursor-object))
  (equal (eol-cursor-object-color drawing-object-1)
         (eol-cursor-object-color drawing-object-2)))

(defmethod drawing-object-equal ((drawing-object-1 extend-to-eol-object) (drawing-object-2 extend-to-eol-object))
  (equal (extend-to-eol-object-color drawing-object-1)
         (extend-to-eol-object-color drawing-object-2)))

(defmethod drawing-object-equal ((drawing-object-1 line-end-object) (drawing-object-2 line-end-object))
  (and (call-next-method)
       (equal (line-end-object-offset drawing-object-1)
              (line-end-object-offset drawing-object-2))))

(defmethod drawing-object-equal ((drawing-object-1 image-object) (drawing-object-2 image-object))
  nil)

(defun object-width (drawing-object)
  (lem-if:object-width (implementation) drawing-object))

(defun object-height (drawing-object)
  (lem-if:object-height (implementation) drawing-object))

(defun split-string-by-character-type (string)
  (loop :with pos := 0 :and items := '()
        :while (< pos (length string))
        :for type := (char-type (char string pos))
        :do (loop :with start := pos
                  :do (incf pos)
                  :while (and (< pos (length string))
                              (eq type (char-type (char string pos)))
                              (not (eq type :control)))
                  :finally (push (cons type (subseq string start pos)) items))
        :finally (return (nreverse items))))

(defun make-line-end-object (string attribute type offset)
  (let ((attribute (and attribute (ensure-attribute attribute nil))))
    (make-instance 'line-end-object
                   :offset offset
                   :string string
                   :attribute attribute
                   :type type)))

(defun make-object-with-type (string attribute type)
  (let ((attribute (and attribute (ensure-attribute attribute nil))))
    (make-instance (case type
                     (:folder 'folder-object)
                     (:icon 'icon-object)
                     (:emoji 'emoji-object)
                     (:control 'control-character-object)
                     (otherwise 'text-object))
                   :string (case type
                             (:control
                              (control-char (char string 0)))
                             (:zero-width
                              (make-string (length string) :initial-element #\·))
                             (otherwise
                              string))
                   :attribute (case type
                                ((:control :zero-width)
                                 (let ((attr (ensure-attribute 'special-char-attribute nil)))
                                   (if attribute
                                       (merge-attribute attribute attr)
                                       attr)))
                                (otherwise attribute))
                   :type type
                   :within-cursor (and attribute
                                       (cursor-attribute-p attribute)))))

(defun create-drawing-object (item)
  (cond ((and *line-wrap* (typep item 'eol-cursor-item))
         (list (make-instance 'eol-cursor-object
                              :attribute (eol-cursor-item-attribute item)
                              :color (parse-color
                                      (attribute-background
                                       (eol-cursor-item-attribute item)))
                              :true-cursor-p (eol-cursor-item-true-cursor-p item))))
        ((typep item 'extend-to-eol-item)
         (list (make-instance 'extend-to-eol-object :color (extend-to-eol-item-color item))))
        ((typep item 'line-end-item)
         (let ((string (line-end-item-text item))
               (attribute (line-end-item-attribute item)))
           (loop :for (type . string) :in (split-string-by-character-type string)
                 :unless (alexandria:emptyp string)
                 :collect (make-line-end-object string
                                                attribute
                                                type
                                                (line-end-item-offset item)))))
        (t
         (let ((string (item-string item))
               (attribute (item-attribute item)))
           (cond ((alexandria:emptyp string)
                  (list (make-instance 'void-object)))
                 ((and attribute (attribute-image attribute))
                  (list (make-instance 'image-object
                                       :image (attribute-image attribute)
                                       :width (attribute-width attribute)
                                       :height (attribute-height attribute)
                                       :attribute attribute)))
                 (t
                  (loop :for (type . string) :in (split-string-by-character-type string)
                        :unless (alexandria:emptyp string)
                        :collect (make-object-with-type string attribute type))))))))

(defun create-drawing-objects (logical-line)
  (multiple-value-bind (items line-end-item)
      (compute-items-from-logical-line logical-line)
    (append (loop :for item :in items
                  :append (create-drawing-object item))
            (when line-end-item
              (create-drawing-object line-end-item)))))

(defun make-letter-object (character attribute)
  (make-object-with-type (string character)
                         attribute
                         (char-type character)))

(defun separate-objects-by-width (objects view-width buffer)
  (flet ((explode-object (text-object)
           (check-type text-object text-object)
           (let* ((string (text-object-string text-object))
                  (char-type (char-type (char string 0)))
                  (n (floor (length string) 2)))
             (loop :for part-string :in (list (subseq string 0 n)
                                              (subseq string n))
                   :unless (alexandria:emptyp part-string)
                   :collect (make-object-with-type
                             part-string
                             (text-object-attribute text-object) char-type)))))
    (let ((wrap-line-character (variable-value 'wrap-line-character :default buffer))
          (wrap-line-attribute (variable-value 'wrap-line-attribute :default buffer)))
      (loop
        :until (null objects)
        :collect (loop :with total-width := 0
                       :and physical-line-objects := '()
                       :for object := (pop objects)
                       :while object
                       :do (cond ((and (typep object 'text-object)
                                       (<= view-width (+ total-width (object-width object))))
                                  (cond ((< 1 (length (text-object-string object)))
                                         (setf objects (nconc (explode-object object) objects)))
                                        (t
                                         (push object objects)
                                         (push (make-letter-object wrap-line-character
                                                                   wrap-line-attribute)
                                               physical-line-objects)
                                         (return (nreverse physical-line-objects)))))
                                 (t
                                  (incf total-width (object-width object))
                                  (push object physical-line-objects)))
                       :finally (return (nreverse physical-line-objects)))))))

(defun render-line (view x y objects height)
  (lem-if:render-line (implementation) view x y objects height))

(defun validate-cache-p (window y height objects)
  (loop :for (cache-y cache-height cache-objects) :in (drawing-cache window)
        :when (and (= y cache-y)
                   (= height cache-height)
                   (alexandria:length= objects cache-objects)
                   (every #'drawing-object-equal objects cache-objects))
        :return t))

(defun invalidate-cache (window y height)
  (setf (drawing-cache window)
        (remove-if-not (lambda (elt)
                         (destructuring-bind (cache-y cache-height cache-logical-line) elt
                           (declare (ignore cache-logical-line))
                           (or (< (+ y height)
                                  cache-y)
                               (<= (+ cache-y cache-height)
                                   y))))
                       (drawing-cache window))))

(defun update-and-validate-cache-p (window y height objects)
  (cond ((validate-cache-p window y height objects) t)
        (t
         (invalidate-cache window y height)
         (push (list y height objects)
               (drawing-cache window))
         nil)))

(defun render-line-with-caching (window x y objects height)
  (unless (update-and-validate-cache-p window y height objects)
    (render-line (window-view window) x y objects height)))

(defun max-height-of-objects (objects)
  (loop :for object :in objects
        :maximize (object-height object)))

(defun redraw-logical-line-when-line-wrapping (window
                                               y
                                               logical-line
                                               left-side-objects
                                               left-side-width)
  (let* ((left-side-characters (loop :for obj :in left-side-objects
                                     :when (typep obj 'text-object)
                                     :sum (length (text-object-string obj))))
         (objects-per-physical-line
           (separate-objects-by-width (create-drawing-objects logical-line)
                                      (- (window-view-width window) left-side-width)
                                      (window-buffer window))))
    (loop :for objects :in objects-per-physical-line
          :for all-objects := (append left-side-objects objects)
          :for height := (max-height-of-objects all-objects)
          :do (render-line-with-caching window 0 y all-objects height)
              (incf y height)
              (setq left-side-objects (copy-list (compute-wrap-left-area-content
                                                  left-side-width left-side-characters)))
          :sum height)))

(defun find-cursor-object (objects)
  (loop :for object :in objects
        :and x := 0 :then (+ x (object-width object))
        :when (cursor-object-p object)
        :return (values object x)))

(defun horizontal-scroll-start (window)
  (or (window-parameter window 'horizontal-scroll-start)
      0))

(defun (setf horizontal-scroll-start) (x window)
  (setf (window-parameter window 'horizontal-scroll-start) x))

(defun extract-object-in-display-range (objects start-x end-x)
  (loop :for object :in objects
        :and x := 0 :then (+ x (object-width object))
        :when (and (<= start-x x)
                   (<= (+ x (object-width object)) end-x))
        :collect object))

(defun redraw-logical-line-when-horizontal-scroll (window
                                                   y
                                                   logical-line
                                                   left-side-objects
                                                   left-side-width)
  (flet ((explode-object (text-object)
           (check-type text-object text-object)
           (loop :for c :across (text-object-string text-object)
                 :collect (make-letter-object c (text-object-attribute text-object)))))
    (let* ((objects (create-drawing-objects logical-line))
           (height
             (max (max-height-of-objects left-side-objects)
                  (max-height-of-objects objects))))
      (multiple-value-bind (cursor-object cursor-x)
          (find-cursor-object objects)
        (when cursor-object
          (let ((width (- (window-view-width window) left-side-width)))
            (cond ((< cursor-x (horizontal-scroll-start window))
                   (setf (horizontal-scroll-start window) cursor-x))
                  ((< (+ (horizontal-scroll-start window)
                         width)
                      (+ cursor-x (object-width cursor-object)))
                   (setf (horizontal-scroll-start window)
                         (+ (- cursor-x width)
                            (object-width cursor-object))))))
          (setf objects
                (extract-object-in-display-range
                 (mapcan (lambda (object)
                           (if (typep object 'text-object)
                               (explode-object object)
                               (list object)))
                         objects)
                 (horizontal-scroll-start window)
                 (+ (horizontal-scroll-start window)
                    (window-view-width window)))))
        (render-line-with-caching window 0 y (append left-side-objects objects) height))
      height)))

(defun redraw-lines (window)
  (let* ((*line-wrap* (variable-value 'line-wrap
                                      :default (window-buffer window)))
         (redraw-fn (if *line-wrap*
                        #'redraw-logical-line-when-line-wrapping
                        #'redraw-logical-line-when-horizontal-scroll)))
    (let ((y 0)
          (height (window-view-height window))
          left-side-width)
      (block outer
        (do-logical-line (logical-line window)
          (let* ((left-side-objects
                   (alexandria:when-let (content (logical-line-left-content logical-line))
                     (mapcan #'create-drawing-object
                             (compute-items-from-string-and-attributes
                              (lem/buffer/line:content-string content)
                              (lem/buffer/line:content-attributes content))))))
            (setf left-side-width
                  (loop :for object :in left-side-objects
                        :sum (object-width object)))
            (incf y (funcall redraw-fn window y logical-line left-side-objects left-side-width))
            (unless (< y height)
              (return-from outer)))))
      (when (< y height)
        (lem-if:clear-to-end-of-window (implementation) (window-view window) y))
      (setf (window-left-width window)
            (floor left-side-width (lem-if:get-char-width (implementation)))))))

(defun call-with-display-error (function)
  (handler-bind ((error (lambda (e)
                          (log:error "~A"
                                     (with-output-to-string (out)
                                       (format out "~A~%" e)
                                       (uiop:print-backtrace :stream out :condition e)))
                          (message "~A" e)
                          (return-from call-with-display-error))))
    (funcall function)))

(defmacro with-display-error (() &body body)
  `(call-with-display-error (lambda () ,@body)))

(defun make-modeline-objects (window default-attribute)
  (let ((left-objects '())
        (right-objects '()))
    (modeline-apply window
                    (lambda (string attribute alignment)
                      (case alignment
                        ((:right)
                         (alexandria:nconcf
                          right-objects
                          (create-drawing-object
                           (make-string-with-attribute-item :string string
                                                            :attribute attribute))))
                        (otherwise
                         (alexandria:nconcf left-objects
                                            (create-drawing-object
                                             (make-string-with-attribute-item :string string
                                                                              :attribute attribute))))))
                    default-attribute)
    (values left-objects
            right-objects)))

(defun redraw-modeline (window force)
  (declare (ignore force))
  ;; TODO: cache
  (when (window-use-modeline-p window)
    (let* ((view (window-view window))
           (default-attribute (ensure-attribute
                               (if (eq window (current-window))
                                   'modeline
                                   'modeline-inactive))))
      (multiple-value-bind (left-objects right-objects)
          (make-modeline-objects window default-attribute)
        (lem-if:render-line-on-modeline (implementation)
                                        view
                                        left-objects
                                        right-objects
                                        default-attribute
                                        (max (max-height-of-objects left-objects)
                                             (max-height-of-objects right-objects)))))))

(defun get-background-color-of-window (window)
  (cond ((typep window 'floating-window)
         (floating-window-background-color window))
        ((eq window (current-window))
         nil)
        ((eq window (window-parent (current-window)))
         nil)
        ((and (inactive-window-background-color)
              (eq 'window (type-of window)))
         (inactive-window-background-color))
        (t nil)))

(defmethod redraw-buffer :around (implementation buffer window force)
  (with-display-error ()
    (lem-if:redraw-view-before (implementation)
                               (window-view window))
    (let ((lem-if:*background-color-of-drawing-window*
            (get-background-color-of-window window)))
      (call-next-method))
    (when (window-use-modeline-p window)
      (redraw-modeline window
                       (or (window-need-to-redraw-p window)
                           force)))
    (lem-if:redraw-view-after (implementation)
                              (window-view window))))

(defun clear-cache-if-screen-modified (window force)
  (when (or force (window-need-to-redraw-p window))
    (setf (drawing-cache window) '())))

(defmethod redraw-buffer (implementation (buffer text-buffer) window force)
  (assert (eq buffer (window-buffer window)))
  (clear-cache-if-screen-modified window force)
  (redraw-lines window)
  (finish-redraw window))
