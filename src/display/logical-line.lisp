(in-package :lem-core)

(defstruct logical-line
  string
  attributes
  left-content
  end-of-line-cursor-attribute
  extend-to-end
  line-end-overlay)

(defun overlay-within-point-p (overlay point)
  (or (point<= (overlay-start overlay)
               point
               (overlay-end overlay))
      (same-line-p (overlay-start overlay)
                   point)
      (same-line-p (overlay-end overlay)
                   point)))

(defun expand-tab (string attributes tab-width)
  (setf attributes (copy-tree attributes))
  (values (with-output-to-string (out)
            (loop :with i := 0
                  :for c :across string
                  :do (cond ((char= c #\tab)
                             (let ((n (- tab-width (mod i tab-width))))
                               (loop :for elt :in attributes
                                     :do (cond ((< i (first elt))
                                                (incf (first elt) (1- n))
                                                (incf (second elt) (1- n)))
                                               ((and (< i (second elt))
                                                     (not (cursor-attribute-p (third elt))))
                                                (incf (second elt) (1- n)))))
                               (loop :repeat n
                                     :do (write-char #\space out))
                               (incf i n)))
                            (t
                             (write-char c out)
                             (incf i)))))
          attributes))

(defun overlay-attributes (under-attributes over-start over-end over-attribute)
  ;; under-attributes := ((start-charpos end-charpos attribute) ...)
  (let* ((over-attribute (ensure-attribute over-attribute))
         (under-part-attributes (lem/buffer/line:subseq-elements under-attributes
                                                               over-start
                                                               over-end))
         (merged-attributes (lem/buffer/line:remove-elements under-attributes
                                                           over-start
                                                           over-end)))
    (flet ((add-element (start end attribute)
             (when (< start end)
               (push (list start end (ensure-attribute attribute))
                     merged-attributes))))
      (if (null under-part-attributes)
          (add-element over-start over-end over-attribute)
          (loop :for prev-under := 0 :then under-end-offset
                :for (under-start-offset under-end-offset under-attribute)
                :in under-part-attributes
                :do (add-element (+ over-start prev-under)
                                 (+ over-start under-start-offset)
                                 over-attribute)
                    (add-element (+ over-start under-start-offset)
                                 (+ over-start under-end-offset)
                                 (alexandria:if-let (under-attribute
                                                     (ensure-attribute under-attribute nil))
                                   (merge-attribute under-attribute
                                                    over-attribute)
                                   over-attribute))
                :finally (add-element (+ over-start under-end-offset)
                                      over-end
                                      over-attribute))))
    (lem/buffer/line:normalization-elements merged-attributes)))

(defun create-logical-line (point overlays active-modes)
  (flet ((overlay-start-charpos (overlay point)
           (if (same-line-p point (overlay-start overlay))
               (point-charpos (overlay-start overlay))
               0))
         (overlay-end-charpos (overlay point)
           (when (same-line-p point (overlay-end overlay))
             (point-charpos (overlay-end overlay)))))
    (let* ((end-of-line-cursor-attribute nil)
           (extend-to-end-attribute nil)
           (line-end-overlay nil)
           (left-content
             (compute-left-display-area-content active-modes
                                                (point-buffer point)
                                                point))
           (tab-width (variable-value 'tab-width :default point)))
      (destructuring-bind (string . attributes)
          (get-string-and-attributes-at-point point)
        (loop :for overlay :in overlays
              :when (overlay-within-point-p overlay point)
              :do (cond ((typep overlay 'line-endings-overlay)
                         (when (same-line-p (overlay-end overlay) point)
                           (setf line-end-overlay overlay)))
                        ((typep overlay 'line-overlay)
                         (let ((attribute (overlay-attribute overlay)))
                           (setf attributes
                                 (overlay-attributes attributes
                                                     0
                                                     (length string)
                                                     attribute))
                           (setf extend-to-end-attribute attribute)))
                        ((typep overlay 'cursor-overlay)
                         (let* ((overlay-start-charpos (overlay-start-charpos overlay point))
                                (overlay-end-charpos (1+ overlay-start-charpos))
                                (overlay-attribute (overlay-attribute overlay)))
                           (unless (cursor-overlay-fake-p overlay)
                             (set-cursor-attribute overlay-attribute))
                           (if (<= (length string) overlay-start-charpos)
                               (setf end-of-line-cursor-attribute overlay-attribute)
                               (setf attributes
                                     (overlay-attributes
                                      attributes
                                      overlay-start-charpos
                                      overlay-end-charpos
                                      overlay-attribute)))))
                        (t
                         (let ((overlay-start-charpos (overlay-start-charpos overlay point))
                               (overlay-end-charpos (overlay-end-charpos overlay point))
                               (overlay-attribute (overlay-attribute overlay)))
                           (unless overlay-end-charpos
                             (setf extend-to-end-attribute
                                   (overlay-attribute overlay)))
                           (setf attributes
                                 (overlay-attributes
                                  attributes
                                  overlay-start-charpos
                                  (or overlay-end-charpos (length string))
                                  overlay-attribute))))))
        (setf (values string attributes) (expand-tab string attributes tab-width))
        (let ((charpos (point-charpos point)))
          (when (< 0 charpos)
            (psetf string (subseq string charpos)
                   attributes (lem/buffer/line:subseq-elements attributes charpos (length string)))))
        (make-logical-line :string string
                           :attributes attributes
                           :left-content left-content
                           :extend-to-end extend-to-end-attribute
                           :end-of-line-cursor-attribute end-of-line-cursor-attribute
                           :line-end-overlay line-end-overlay)))))

(defstruct string-with-attribute-item
  string
  attribute)

(defstruct cursor-item
  attribute
  string)

(defstruct eol-cursor-item
  attribute
  true-cursor-p)

(defstruct extend-to-eol-item
  color)

(defstruct line-end-item
  text
  attribute
  offset)

(defmethod item-string ((item string-with-attribute-item))
  (string-with-attribute-item-string item))

(defmethod item-string ((item cursor-item))
  (cursor-item-string item))

(defmethod item-string ((item eol-cursor-item))
  " ")

(defmethod item-string ((item extend-to-eol-item))
  "")

(defmethod item-attribute ((item string-with-attribute-item))
  (string-with-attribute-item-attribute item))

(defmethod item-attribute ((item cursor-item))
  (cursor-item-attribute item))

(defmethod item-attribute ((item eol-cursor-item))
  (eol-cursor-item-attribute item))

(defmethod item-attribute ((item extend-to-eol-item))
  nil)

(defun compute-items-from-string-and-attributes (string attributes)
  (handler-case
      (let ((items '()))
        (flet ((add (item)
                 (if (null items)
                     (push item items)
                     (let ((last-item (first items)))
                       (if (and (string-with-attribute-item-p last-item)
                                (string-with-attribute-item-p item)
                                (equal (string-with-attribute-item-attribute last-item)
                                       (string-with-attribute-item-attribute item)))
                           (setf (string-with-attribute-item-string (first items))
                                 (str:concat (string-with-attribute-item-string last-item)
                                             (string-with-attribute-item-string item)))
                           (push item items))))))
          (loop :for last-pos := 0 :then end
                :for (start end attribute) :in attributes
                :do (unless (= last-pos start)
                      (add (make-string-with-attribute-item :string (subseq string last-pos start))))
                    (add (if (cursor-attribute-p attribute)
                             (make-cursor-item :string (subseq string start end) :attribute attribute)
                             (make-string-with-attribute-item
                              :string (subseq string start end)
                              :attribute attribute)))
                :finally (push (make-string-with-attribute-item :string (subseq string last-pos))
                               items)))
        items)
    (error (e)
      (log:error e string attributes)
      nil)))

(defun compute-items-from-logical-line (logical-line)
  (let ((items
          (compute-items-from-string-and-attributes (logical-line-string logical-line)
                                                    (logical-line-attributes logical-line))))
    (alexandria:when-let (attribute
                          (logical-line-extend-to-end logical-line))
      (push (make-extend-to-eol-item :color (attribute-background-color attribute))
            items))
    (alexandria:when-let (attribute
                          (logical-line-end-of-line-cursor-attribute logical-line))
      (push (make-eol-cursor-item :attribute attribute
                                  :true-cursor-p (cursor-attribute-p attribute))
            items))
    (values (nreverse items)
            (alexandria:when-let (overlay
                                  (logical-line-line-end-overlay logical-line))
              (make-line-end-item :text (line-endings-overlay-text overlay)
                                  :attribute (overlay-attribute overlay)
                                  :offset (line-endings-overlay-offset overlay))))))

(defun make-temporary-highlight-line-overlay (buffer)
  (when (and (variable-value 'highlight-line :default (current-buffer))
             (current-theme))
    (alexandria:when-let ((color (highlight-line-color)))
      (make-line-overlay (buffer-point buffer)
                         (make-attribute :background color)
                         :temporary t))))

(defun make-temporary-region-overlay-from-cursor (cursor)
  (let ((mark (cursor-mark cursor)))
    (when (mark-active-p mark)
      (make-overlay cursor
                    (mark-point mark)
                    'region
                    :temporary t))))

(defun make-cursor-overlay* (point)
  (make-cursor-overlay
   point
   (if (typep point 'fake-cursor)
       'fake-cursor
       'cursor)
   :fake (typep point 'fake-cursor)))

(defun get-window-overlays (window)
  (let* ((buffer (window-buffer window))
         (overlays (buffer-overlays buffer)))
    (when (eq (current-window) window)
      (dolist (cursor (buffer-cursors buffer))
        (if-push (make-temporary-region-overlay-from-cursor cursor)
                 overlays))
      (if-push (make-temporary-highlight-line-overlay buffer)
               overlays))
    (if (and (eq window (current-window))
             (not (window-cursor-invisible-p window)))
        (append overlays
                (mapcar #'make-cursor-overlay*
                        (buffer-cursors (window-buffer window))))
        overlays)))

(defun call-do-logical-line (window function)
  (with-point ((point (window-view-point window)))
    (let ((overlays (get-window-overlays window))
          (active-modes (get-active-modes-class-instance (window-buffer window))))
      (loop :for logical-line := (create-logical-line point overlays active-modes)
            :do (funcall function logical-line)
                (unless (line-offset point 1)
                  (return))))))

(defmacro do-logical-line ((logical-line window) &body body)
  `(call-do-logical-line ,window (lambda (,logical-line) ,@body)))
