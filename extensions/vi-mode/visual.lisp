(defpackage :lem-vi-mode/visual
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:import-from :lem-vi-mode/core
                :ensure-state)
  (:import-from :lem-vi-mode/states
                :*motion-keymap*
                :*normal-keymap*
                :normal)
  (:import-from :lem-vi-mode/modeline
                :state-modeline-orange)
  (:import-from :lem
                :alive-point-p)
  (:import-from :alexandria
                :last-elt)
  (:export :*visual-keymap*
           :vi-visual-end
           :vi-visual-char
           :vi-visual-line
           :vi-visual-block
           :visual
           :visual-p
           :visual-char-p
           :visual-line-p
           :visual-block-p
           :visual-range
           :apply-visual-range
           :vi-visual-insert
           :vi-visual-append
           :vi-visual-swap-points
           :vi-visual-opposite-side))
(in-package :lem-vi-mode/visual)

(defvar *start-point* nil)
(defvar *visual-overlays* '())

(defvar *visual-keymap* (make-keymap :name '*visual-keymap*))

(define-state visual (vi-state) ()
  (:default-initargs
   :modeline-color 'state-modeline-orange
   :keymaps (list *visual-keymap* *motion-keymap* *normal-keymap*)))

(define-state visual-char (visual)
  ()
  (:default-initargs :name "VISUAL"))

(define-state visual-line (visual) ()
  (:default-initargs
   :name "V-LINE"))

(define-state visual-block (visual) ()
  (:default-initargs
   :name "V-BLOCK"))

(defmethod state-enabled-hook :after ((state visual))
  (setf *start-point* (copy-point (current-point))))

(defmethod state-disabled-hook ((state visual))
  (delete-point *start-point*)
  (setf *start-point* nil)
  (clear-visual-overlays))

(defun disable ()
  (clear-visual-overlays))

(defun clear-visual-overlays ()
  (mapc 'delete-overlay *visual-overlays*)
  (setf *visual-overlays* '()))

(defmethod post-command-hook ((state visual))
  (clear-visual-overlays)
  (if (not (eq (current-buffer) (point-buffer *start-point*))) 
      (vi-visual-end)
      (state-setup state)))

(defgeneric state-setup (visual-state))

(defmethod state-setup ((state visual-char))
  (with-point ((start *start-point*)
               (end (current-point)))
    (when (point< end start)
      (rotatef start end))
    (character-offset end 1)
    (push (make-overlay start end 'region)
          *visual-overlays*)))

(defmethod state-setup ((state visual-line))
  (apply-region-lines *start-point* (current-point)
                      (lambda (p)
                        (push (make-line-overlay p 'region)
                              *visual-overlays*))))

(defmethod state-setup ((state visual-block))
  (with-point ((start *start-point*)
               (end (current-point)))
    (let ((start-column (point-column start))
          (end-column (point-column end)))
      (cond
        ;; left-top or left-bottom
        ((< end-column start-column)
         (character-offset start 1)
         (setf start-column (point-column start)))
        ;; right-top or right-bottom
        (t
         (unless (= end-column (length (line-string end)))
           (character-offset end 1))
         (setf end-column (point-column end))))
      (apply-region-lines start end
                          (lambda (p)
                            (with-point ((s p) (e p))
                              (move-to-column s start-column)
                              (move-to-column e end-column)
                              (push (make-overlay s e 'region) *visual-overlays*)))))))

(define-command vi-visual-end () ()
  (clear-visual-overlays)
  (change-state 'normal))

(defun enable-visual (new-state)
  (let ((new-state (ensure-state new-state))
        (current-state (current-state)))
    (cond
      ((typep current-state (class-name (class-of new-state)))
       (vi-visual-end))
      ((typep current-state 'visual)
       (check-type *start-point* point)
       (assert (alive-point-p *start-point*))
       (let ((start (copy-point *start-point*)))
         (prog1 (change-state new-state)
           (setf *start-point* start))))
      (t
       (change-state new-state)))))

(define-command vi-visual-char () ()
  (enable-visual 'visual-char))

(define-command vi-visual-line () ()
  (enable-visual 'visual-line))

(define-command vi-visual-block () ()
  (enable-visual 'visual-block))

(defun visual-p ()
  (typep (current-main-state) 'visual))

(defun visual-char-p ()
  (typep (current-main-state) 'visual-char))

(defun visual-line-p ()
  (typep (current-main-state) 'visual-line))

(defun visual-block-p ()
  (typep (current-main-state) 'visual-block))

(defun visual-range ()
  (with-point ((start *start-point*)
               (end (current-point)))
    (cond
      ((visual-char-p)
       (cond ((point<= start end)
              (character-offset end 1))
             ((point< end start)
              (character-offset start 1)))
       (list start end))
      ((visual-block-p)
       (list start end))
      (t
       (when (point< end start)
         (rotatef start end))
       (line-start start)
       (or (line-offset end 1 0)
           (line-end end))
       (list start end)))))

(defun (setf visual-range) (new-range)
  (check-type new-range list)
  (destructuring-bind (start end) new-range
    (cond
      ((point< start end)
       (character-offset end -1))
      ((point< end start)
       (character-offset start -1)))
    (cond
      ((or (visual-char-p)
           (visual-block-p))
       (setf *start-point* start)
       (move-point (current-point) end))
      ((visual-line-p)
       (unless (same-line-p *start-point* start)
         (setf *start-point* start))
       (unless (same-line-p end (current-point))
         (move-point (current-point) end))))))

(defun apply-visual-range (function)
  (if (visual-line-p)
      (apply function (visual-range))
      (dolist (ov (sort (copy-list *visual-overlays*) #'point< :key #'overlay-start))
        (funcall function
                 (overlay-start ov)
                 (overlay-end ov)))))

(defun string-without-escape ()
  (concatenate 'string
               (loop for key-char = (key-to-char (read-key))
                     while (char/= #\Escape key-char)
                     collect key-char)))

(define-command vi-visual-append () ()
  (when (visual-block-p)
    (let ((str (string-without-escape))
          (max-end (apply #'max (mapcar (lambda (ov)
                                          (point-charpos (overlay-end ov)))
                                        *visual-overlays*))))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (let* ((space-len (- max-end (point-charpos end)))
                                   (spaces (make-string space-len
                                                        :initial-element #\Space)))
                              (insert-string end (concatenate 'string
                                                              spaces
                                                              str))))))
    (vi-visual-end)))

(define-command vi-visual-insert () ()
  (when (visual-block-p)
    (let ((str (string-without-escape)))
      (apply-visual-range (lambda (start end)
                            (unless (point< start end)
                              (rotatef start end))
                            (insert-string start str))))
    (vi-visual-end)))

(define-command vi-visual-swap-points () ()
  (with-point ((start *start-point*))
    (move-point *start-point* (current-point))
    (move-point (current-point) start)))

(define-command vi-visual-opposite-side () ()
  (if (visual-block-p)
      (let ((start-col (point-charpos *start-point*))
            (end-col (point-charpos (current-point))))
        (move-to-column *start-point* end-col)
        (move-to-column (current-point) start-col))
      (vi-visual-swap-points)))

(defmethod check-marked-using-global-mode ((global-mode vi-mode) buffer)
  nil)

(defmethod set-region-point-using-global-mode ((global-mode vi-mode) (start point) (end point))
  (declare (ignore global-mode))
  (when (visual-p)
    (let ((v-range (visual-range)))
      (move-point start (car v-range))
      (move-point end (cadr v-range)))))

(defmethod region-beginning-using-global-mode ((global-mode vi-mode)
                                         &optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (if (visual-p)
      (car (visual-range))
      (editor-error "Not in visual mode")))

(defmethod region-end-using-global-mode ((global-mode vi-mode)
                                   &optional (buffer (current-buffer)))
  (declare (ignore buffer))
  (if (visual-p)
      (cadr (visual-range))
      (editor-error "Not in visual mode")))
