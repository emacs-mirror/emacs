(in-package :lem/buffer/internal)

(define-condition corruption-warning (simple-warning) ())

(defmacro debug-assert (form &rest args)
  `(unless ,form
     (log:error "assertion failed" ,form ,@args)
     (warn 'corruption-warning)))

(defun check-line-corruption (line line-number buffer)
  (check-type line line:line)
  (check-type buffer buffer)
  (when (line:line-previous line)
    (debug-assert (eq line (line:line-next (line:line-previous line)))
                  "line.prev.next is not line"
                  line))
  (when (line:line-next line)
    (debug-assert (eq line (line:line-previous (line:line-next line)))
                  "line.next.prev is not line"
                  line))
  (dolist (point (line:line-points line))
    (debug-assert (eq buffer (point-buffer point))
                  "point.buffer is not buffer"
                  point)
    (debug-assert (point-line point)
                  "point.line is not null"
                  point)
    (debug-assert (= line-number (point-linum point))
                  "point.linum is wrong line number"
                  point
                  (point-linum point)
                  line-number)))

(defun check-lines-corruption (first-line buffer)
  (loop :for prev-line := nil :then line
        :for line := first-line :then (line:line-next line)
        :for line-number :from 1
        :while line
        :do (check-line-corruption line line-number buffer)
        :finally (return prev-line)))

(defun check-buffer-points-corruption (buffer)
  (let ((collected-buffer-points
          (loop :for line := (point-line (buffer-start-point buffer)) :then (line:line-next line)
                :while line
                :append (line:line-points line))))
    (debug-assert (alexandria:set-equal (buffer-points buffer)
                                        collected-buffer-points
                                        :test #'eq)
                  (set-difference (buffer-points buffer)
                                  collected-buffer-points))))

(defun check-buffer-corruption (buffer)
  (check-type buffer buffer)
  (let ((first-line (point-line (buffer-start-point buffer))))
    (debug-assert (null (line:line-previous first-line)))
    (let ((last-line (check-lines-corruption first-line buffer)))
      (debug-assert (null (line:line-next last-line)))
      (debug-assert (eq last-line
                        (point-line (buffer-end-point buffer))))
      (debug-assert (member (buffer-end-point buffer)
                            (line:line-points last-line)))))
  (check-buffer-points-corruption buffer)
  (debug-assert (member (buffer-point buffer) (buffer-points buffer)))
  (debug-assert (member (buffer-start-point buffer) (buffer-points buffer)))
  (debug-assert (member (buffer-end-point buffer) (buffer-points buffer)))
  (dolist (point (buffer-points buffer))
    (debug-assert (point<= (buffer-start-point buffer)
                           point
                           (buffer-end-point buffer))))
  (let ((end-point (buffer-end-point buffer)))
    (debug-assert (= (buffer-nlines buffer)
                     (line-number-at-point end-point)))
    (debug-assert (= (lem/buffer/line:line-length (point-line end-point))
                     (point-charpos end-point)))))

(defun check-all-buffers-corruption ()
  (dolist (buffer (buffer-list))
    (check-buffer-corruption buffer)))
