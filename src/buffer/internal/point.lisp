(in-package :lem/buffer/internal)

(deftype point-kind ()
  '(member :temporary :left-inserting :right-inserting))

(defclass point ()
  ((buffer
    :reader point-buffer
    :type buffer)
   (linum
    :accessor point-linum
    :type fixnum)
   (line
    :accessor point-line
    :type line)
   (charpos
    :accessor point-charpos
    :type fixnum)
   (kind
    :reader point-kind
    :type point-kind))
  (:documentation
   "`point` is an object that points to the position of the text in the buffer.
It has a `buffer` slot, a `line` number, and `charpos` is an offset from the beginning of the line, starting at zero.
`point` has a `kind` type. The position after inserting and deleting in the buffer depends on the value of `kind`:
- when `kind` is `temporary`, `point` is used for temporary reads.
 The overhead on creation and deletion is low, and there is no need to explicitly delete the point.
 If you edit the buffer before the position, the `point` cannot be used correctly any more.
- when `kind` is `:left-inserting` or `:right-inserting`, and if you insert content before the point, then the point position is adjusted by the length of your edit.
 If you insert content at the point position, with `:right-inserting` the original position is unchanged, and with `:left-inserting` the position is moved.
When using `:left-inserting` or `:right-inserting`, you must explicitly delete the point after use with `delete-point`. For this reason, you should use `with-point`.
"))

(setf (documentation 'point-buffer 'function)
      "Return the `buffer` pointed to by `point`.")

(setf (documentation 'point-kind 'function)
      "Return the type of `point` (`:temporary`, `:left-inserting` or `:right-inserting`).")

(defun current-point ()
  "Return the current`point`."
  (buffer-point (current-buffer)))

(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "(~D, ~D) ~S"
            (point-linum object)
            (point-charpos object)
            (line:line-string (point-line object)))))

(defun pointp (x)
  "Returns T if `x` is a `point`, otherwise returns NIL."
  (typep x 'point))

(defun initialize-point-slot-values
    (point &key (buffer (alexandria:required-argument :buffer))
                (linum (alexandria:required-argument :linum))
                (line (alexandria:required-argument :line))
                (charpos (alexandria:required-argument :charpos))
                (kind (alexandria:required-argument :kind)))
  (setf (slot-value point 'buffer) buffer
        (slot-value point 'linum) linum
        (slot-value point 'line) line
        (slot-value point 'charpos) charpos
        (slot-value point 'kind) kind)
  (values))

(defun initialize-point (point kind)
  (unless (eq :temporary kind)
    (push point (line:line-points (point-line point)))
    (push point (buffer-points (point-buffer point)))))

(defun make-point (buffer linum line charpos &key (kind :right-inserting))
  (check-type kind point-kind)
  (let ((point (make-instance 'point)))
    (initialize-point-slot-values point
                                  :buffer buffer
                                  :linum linum
                                  :line line
                                  :charpos charpos
                                  :kind kind)
    (initialize-point point kind)
    point))

(defmethod copy-point-using-class ((point point) from-point kind)
  (check-type kind point-kind)
  (initialize-point-slot-values point
                                :buffer (point-buffer from-point)
                                :linum (point-linum from-point)
                                :line (point-line from-point)
                                :charpos (point-charpos from-point)
                                :kind kind)
  (initialize-point point kind)
  point)

(defun copy-point (point &optional kind)
  "Make and return a copy of `point`
`kind` is `:temporary`, `:left-inserting` or `:right-inserting`.
If omitted, is copied from `point`."
  (copy-point-using-class (make-instance 'point)
                          point
                          (or kind (point-kind point))))

(defun delete-point (point)
  "Delete `point`.
If `point-kind` is `:temporary` this is unnecessary."
  (unless (point-temporary-p point)
    (setf (line:line-points (point-line point))
          (delete point (line:line-points (point-line point))))
    (let ((buffer (point-buffer point)))
      (setf (buffer-points buffer)
            (delete point (buffer-points buffer))))
    (values)))

(defun alive-point-p (point)
  (alexandria:when-let (line (point-line point))
    (line:line-alive-p line)))

(defun point-change-line (point new-linum new-line)
  (unless (point-temporary-p point)
    (let ((old-line (point-line point)))
      (if (line:line-alive-p old-line)
          (do ((scan (line:line-points old-line) (cdr scan))
               (prev nil scan))
              ((eq (car scan) point)
               (if prev
                   (setf (cdr prev) (cdr scan))
                   (setf (line:line-points old-line) (cdr scan)))
               (setf (cdr scan) (line:line-points new-line)
                     (line:line-points new-line) scan))
            (assert (not (null scan))))
          (push point (line:line-points new-line)))))
  (setf (point-linum point) new-linum)
  (setf (point-line point) new-line))

(defun point-temporary-p (point)
  (eq (point-kind point) :temporary))

(defun %always-same-buffer (point more-points)
  (loop :with buffer1 := (point-buffer point)
        :for point2 :in more-points
        :for buffer2 := (point-buffer point2)
        :always (eq buffer1 buffer2)))

(defun %point= (point1 point2)
  (and (= (point-linum point1)
          (point-linum point2))
       (= (point-charpos point1)
          (point-charpos point2))))

(defun %point< (point1 point2)
  (or (< (point-linum point1) (point-linum point2))
      (and (= (point-linum point1) (point-linum point2))
           (< (point-charpos point1) (point-charpos point2)))))

(defun point= (point &rest more-points)
  "Return T if all of its argument points have same line and point, NIL otherwise."
  (assert (%always-same-buffer point more-points))
  (loop :for point2 :in more-points
        :always (%point= point point2)))

(defun point/= (point &rest more-points)
  "Return T if no two of its argument points have same line and point, NIL otherwise."
  (assert (%always-same-buffer point more-points))
  (loop :for point1 := point :then (first points)
        :for points :on more-points
        :always (loop :for point2 :in points
                      :never (%point= point1 point2))))

(defun point< (point &rest more-points)
  "Return T if its argument points are in strictly increasing order, NIL otherwise."
  (assert (%always-same-buffer point more-points))
  (loop :for point1 := point :then point2
        :for point2 :in more-points
        :always (%point< point1 point2)))

(defun point<= (point &rest more-points)
  "Return T if argument points are in strictly non-decreasing order, NIL otherwise."
  (assert (%always-same-buffer point more-points))
  (loop :for point1 := point :then point2
        :for point2 :in more-points
        :always (or (%point< point1 point2)
                    (%point= point1 point2))))

(defun point> (point &rest more-points)
  "Return T if its argument points are in strictly decreasing order, NIL otherwise."
  (loop :for point1 := point :then point2
        :for point2 :in more-points
        :always (%point< point2 point1)))

(defun point>= (point &rest more-points)
  "Return T if argument points are in strictly non-increasing order, NIL otherwise."
  (assert (%always-same-buffer point more-points))
  (loop :for point1 := point :then point2
        :for point2 :in more-points
        :always (or (%point< point2 point1)
                    (%point= point2 point1))))

(defun point-closest (point point-list &key (direction :up) (same-line nil))
  "Return the closest point on the POINT-LIST compare to POINT.
DIRECTION can be :up or :down depending on the desired point.
SAME-LINE if T the point in POINT-LIST can be in the same line as POINT."
  (labels ((up (p closest)
             (or (and (point>= point p closest)
                      (or same-line
                          (not (same-line-p p point))))
                 (point>= closest point)))
           (down (p closest)
             (or (and
                  (point> p point)
                  (<=  (- (point-linum p) (point-linum point))
                       (- (point-linum closest) (point-linum point)))
                  (or same-line (not (same-line-p p point))))

                 (point<= closest point))))
    (loop :for p :in point-list
          :for flag := t :then nil
          :with closest := nil
          :do (progn
                (when flag (setf closest p))
                (when (and
                       (not flag)
                       (or (and (eq direction :up)
                                (up p closest))
                           (and (eq direction :down)
                                (down p closest))))
                  (setf closest p)))
          :finally (return (and (or (and (eq direction :up)
                                         (point> point closest))
                                    (and (eq direction :down)
                                         (point< point closest)))
                                closest)))))

(defun point-min (point &rest more-points)
  (assert (%always-same-buffer point more-points))
  (loop :with min := point
        :for point :in more-points
        :do (when (point< point min)
              (setf min point))
        :finally (return min)))

(defun point-max (point &rest more-points)
  (assert (%always-same-buffer point more-points))
  (loop :with max := point
        :for point :in more-points
        :do (when (point< max point)
              (setf max point))
        :finally (return max)))

(defmacro with-point (bindings &body body)
  "This macro creates each `point` to be used in `body` with `bindings`.
When you exit `body`, it removes each `point` and returns the value of `body`.
If there is an error in `body`, each `point` is removed.
The format of `bindings` is a list of (`var` `point` &optional `kind`).
Example:
\(with-point ((p3 expr1)
             (p1 expr2 :left-inserting)
             (p2 expr3 :right-inserting))
  ...)
```
"
  (let ((cleanups
          (mapcan (lambda (b)
                    (destructuring-bind (var point &optional (kind :temporary)) b
                      (declare (ignore point))
                      (unless (eq :temporary kind)
                        `((delete-point ,var)))))
                  bindings)))
    `(let ,(mapcar (lambda (b)
                     (destructuring-bind (var point &optional (kind :temporary)) b
                       `(,var (copy-point ,point ,kind))))
                   bindings)
       ,(if cleanups
            `(unwind-protect (progn ,@body)
               ,@cleanups)
            `(progn ,@body)))))

;; TODO: delete this ugly function
(defun get-string-and-attributes-at-point (point)
  (line:line-string/attributes (point-line point)))
