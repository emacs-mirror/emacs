(defpackage :lem/buffer/line
  (:use :cl)
  (:export :make-content
           :content-string
           :content-attributes
           :line
           :line-previous
           :line-next
           :line-string
           :line-plist
           :line-syntax-context
           :line-points
           :make-line
           :make-empty-line
           :line-free
           :line-alive-p
           :line-char
           :line-length
           :remove-elements
           :normalization-elements
           :subseq-elements
           :offset-elements
           :put-elements
           :merge-plist
           :line-merge
           :line-normalization-plist
           :line-remove-property
           :line-add-property
           :line-clear-property
           :line-search-property
           :line-search-property-range
           :line-property-insert-pos
           :line-delete-property-region
           :line-string/attributes
           :line-substring
           :insert-string
           :insert-newline
           :delete-region
           :merge-with-next-line))
(in-package :lem/buffer/line)

(defstruct content
  string
  attributes)

(defclass line ()
  ((previous
    :initarg :previous
    :initform nil
    :accessor line-previous)
   (next
    :initarg :next
    :initform nil
    :accessor line-next)
   (string
    :initarg :string
    :initform nil
    :reader line-string
    :writer set-line-string)
   (plist
    :initarg :plist
    :initform nil
    :accessor line-plist)
   (syntax-context
    :initarg :syntax-context
    :initform nil
    :accessor line-syntax-context)
   (points
    :initarg :points
    :initform nil
    :accessor line-points)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "string: ~S, plist: ~S"
            (line-string object)
            (line-plist object))))

(defun make-line (previous next string)
  (let ((line (make-instance 'line
                             :next next
                             :previous previous
                             :string string)))
    (when next
      (setf (line-previous next) line))
    (when previous
      (setf (line-next previous) line))
    line))

(defun make-empty-line ()
  (make-line nil nil ""))

(defun line-free (line)
  (when (line-previous line)
    (setf (line-next (line-previous line))
          (line-next line)))
  (when (line-next line)
    (setf (line-previous (line-next line))
          (line-previous line)))
  (setf (line-previous line) nil)
  (setf (line-next line) nil)
  (setf (line-points line) nil)
  (set-line-string nil line))

(defun line-alive-p (line)
  (not (null (line-string line))))

(defun line-char (line i)
  (if (= i (line-length line))
      #\newline
      (char (line-string line) i)))

(defun line-length (line)
  (length (line-string line)))

(defun remove-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       nil)
      ((<= start start1 end end1)
       (iter:collect (list end end1 value1)))
      ((<= start1 start end1 end)
       (iter:collect (list start1 start value1)))
      ((<= start1 start end end1)
       (iter:collect (list start1 start value1))
       (iter:collect (list end end1 value1)))
      (t
       (iter:collect (list start1 end1 value1))))))

(defun normalization-elements (elements)
  (flet ((start (elt) (first elt))
         (end (elt) (second elt))
         (value (elt) (third elt)))
    (setf elements (sort elements #'< :key #'first))
    (iter:iter (iter:until (null elements))
      (cond
        ((and (eql (end (first elements))
                   (start (second elements)))
              (equal (value (first elements))
                     (value (second elements))))
         (iter:collect (list (start (first elements))
                             (end (second elements))
                             (value (first elements))))
         (setf elements (cddr elements)))
        (t
         (iter:collect (first elements))
         (setf elements (cdr elements)))))))

(defun subseq-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       (iter:collect (list (- start1 start) (- end1 start) value1)))
      ((<= start start1 end end1)
       (iter:collect (list (- start1 start) (- end start) value1)))
      ((<= start1 start end1 end)
       (iter:collect (list (- start start) (- end1 start) value1)))
      ((<= start1 start end end1)
       (iter:collect (list (- start start) (- end start) value1))))))

(defun offset-elements (elements n)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (iter:collect (list (+ n start1) (+ n end1) value1))))

(defun put-elements (elements start end value &optional contp)
  (normalization-elements
   (cons (list start end value contp)
         (remove-elements elements start end))))

(defun merge-plist (plist1 plist2)
  (let ((new-plist '()))
    (flet ((f (plist)
             (loop :for (k v) :on plist :by #'cddr
                   :do (setf (getf new-plist k)
                             (nconc (getf new-plist k) v)))))
      (f plist1)
      (f plist2))
    new-plist))

(defun line-merge-plist (curr-line next-line pos)
  (setf (line-plist curr-line)
        (merge-plist
         (line-plist curr-line)
         (loop :for (key elements) :on (line-plist next-line) :by #'cddr
               :append (let ((new-elements
                               (loop :for (start end value) :in elements
                                     :collect (list (+ start pos)
                                                    (+ end pos)
                                                    value))))
                         (when new-elements
                           (list key new-elements)))))))

(defun line-normalization-plist (line)
  (loop :for (key elements) :on (line-plist line) :by #'cddr
        :collect (cons key (normalization-elements elements))))

(defun line-remove-property (line start end key)
  (setf (getf (line-plist line) key)
        (normalization-elements (remove-elements (getf (line-plist line) key) start end))))

(defun line-add-property (line start end key value contp)
  (assert (<= 0 start (line-length line)))
  (assert (<= 0 end (line-length line)))
  (assert (<= start end))
  (setf (getf (line-plist line) key)
        (put-elements (getf (line-plist line) key)
                      start end value contp)))

(defun line-clear-property (line key)
  (setf (getf (line-plist line) key) nil))

(defun line-search-property (line key pos)
  (loop :for (start end value contp) :in (getf (line-plist line) key)
        :do (when (if contp
                      (<= start pos end)
                      (<= start pos (1- end)))
              (return value))))

(defun line-search-property-range (line key pos-start pos-end)
  (when (null pos-end)
    (setq pos-end most-positive-fixnum))
  (loop :for (start end value contp) :in (getf (line-plist line) key)
        :do (when (or (and (<= pos-start start) (< start pos-end))
                      (if contp
                          (<= start pos-start end)
                          (<= start pos-start (1- end))))
              (return value))))

(defun line-property-insert-pos (line pos offset)
  (loop :for values :in (cdr (line-plist line)) :by #'cddr
        :do (loop :for v :in values
                  :for (start end) := v
                  :do (cond ((<= pos start)
                             (incf (first v) offset)
                             (incf (second v) offset))
                            ((< start pos end)
                             (incf (second v) offset))
                            ((< pos end)
                             (incf (second v) offset))))))

(defun line-property-insert-newline (line next-line pos)
  (let ((new-plist '()))
    (loop :for plist-rest :on (line-plist line) :by #'cddr
          :do (let ((new-values '())
                    (new-values-last nil))
                (setf (cadr plist-rest)
                      (iter:iter
                        (iter:for elt iter:in (cadr plist-rest))
                        (iter:for (start end value) iter:next elt)
                        (cond ((<= pos start)
                               (let ((new-elt (list (list (- start pos) (- end pos) value))))
                                 (cond
                                   (new-values-last
                                    (setf (cdr new-values-last) new-elt)
                                    (setf new-values-last (cdr new-values-last)))
                                   (t
                                    (setf new-values new-elt)
                                    (setf new-values-last new-elt)))))
                              ((<= pos end)
                               (iter:collect (list start pos value)))
                              (t
                               (iter:collect elt)))))
                (unless (null new-values)
                  (setf (getf new-plist (car plist-rest)) new-values))))
    (setf (line-plist next-line) new-plist)))

(defun line-delete-property-region (line start &optional end)
  (unless end (setf end (line-length line)))
  (assert (<= start end))
  (loop :for plist-rest :on (line-plist line) :by #'cddr
        :do (setf (cadr plist-rest)
                  (loop :for elt :in (cadr plist-rest)
                        :for (start1 end1 value) := elt

                        :if (<= start start1 end1 (1- end))
                        :do (progn)

                        :else :if (<= start end start1)
                        :collect (list (- start1 (- end start))
                                       (- end1 (- end start))
                                       value)

                        :else :if (< start start1 end)
                        :collect (list start (- end1 (- end start)) value)

                        :else :if (<= start1 start end end1)
                        :collect (list start1 (- end1 (- end start)) value)

                        :else :if (<= start1 start end1 end)
                        :collect (list start1 start value)

                        :else
                        :collect elt))))

(defun line-string/attributes (line)
  (cons (line-string line)
        (alexandria:if-let (sticky-attribute (getf (line-plist line) :sticky-attribute))
          (loop :with attributes := (getf (line-plist line) :attribute)
                :for (start end value contp) :in sticky-attribute
                :do (setf attributes (put-elements attributes start end value contp))
                :finally (return attributes))
          (getf (line-plist line) :attribute))))

(defun line-substring (line &key (start 0) end)
  (cond ((and (= start 0) (or (null end) (= end (line-length line))))
         (line-string line))
        (t
         (subseq (line-string line) start end))))

(defun insert-string (line string index)
  (line-property-insert-pos line index (length string))
  (set-line-string (concatenate 'string
                                (line-substring line :start 0 :end index)
                                string
                                (line-substring line :start index))
                   line))

(defun insert-newline (line position)
  (let ((before-string (line-substring line :start 0 :end position))
        (after-string (line-substring line :start position)))
    (set-line-string before-string line)
    (let ((next (make-line line (line-next line) after-string)))
      (line-property-insert-newline line next position))))

(defun delete-region (line &key start end)
  (line-delete-property-region line start end)
  (set-line-string (concatenate 'string
                                (line-substring line :start 0 :end start)
                                (line-substring line :start (or end (line-length line))))
                   line))

(defun merge-with-next-line (line &key (start 0))
  (assert (line-next line))
  (line-delete-property-region line start)
  (line-merge-plist line (line-next line) start)
  (set-line-string (concatenate 'string
                                (line-substring line :start 0 :end start)
                                (line-string (line-next line)))
                   line)
  (line-free (line-next line)))
