(defpackage :lem/peek-source
  (:use :cl :lem)
  (:export :filename-attribute
           :position-attribute
           :with-collecting-sources
           :with-appending-source
           :with-insert
           :collector-buffer
           :get-move-function
           :show-matched-line
           :highlight-matched-line
           :peek-source-next
           :peek-source-previous
           :*peek-source-keymap*)
  #+sbcl
  (:lock t))
(in-package :lem/peek-source)

(define-attribute filename-attribute
  (t :foreground :base0D :bold t))

(define-attribute position-attribute
  (t :foreground :base08 :bold t))

(define-attribute highlight
  (t :background :base01))

(defvar *collector*)

(defclass collector ()
  ((buffer :initarg :buffer
           :reader collector-buffer)
   (count :initform 0
          :accessor collector-count)))

(defvar *peek-window*)
(defvar *source-window*)
(defvar *parent-window*)

(define-minor-mode peek-source-mode
    (:name "Peek"
     :keymap *peek-source-keymap*)
  (setf (not-switchable-buffer-p (current-buffer)) t))

(define-key *peek-source-keymap* "Return" 'peek-source-select)
(define-key *peek-source-keymap* 'next-line 'peek-source-next)
(define-key *peek-source-keymap* 'previous-line 'peek-source-previous)
(define-key *peek-source-keymap* "Escape" 'peek-source-quit)  ;; also C-x 0 by default.
(define-key *peek-source-keymap* "C-c C-k" 'peek-source-quit)
(define-key *peek-source-keymap* "M-q" 'peek-source-quit)

(defclass peek-window (floating-window) ())
(defclass source-window (floating-window) ())

(defmethod lem-core::%delete-window :before ((window peek-window))
  (finalize-peek-source))

(defmethod lem-core::%delete-window :before ((window source-window))
  (finalize-peek-source))

(defmethod compute-window-list ((current-window peek-window))
  (list *peek-window* *source-window*))

(defmethod compute-window-list ((current-window source-window))
  (list *source-window* *peek-window*))

(defvar *is-finalzing* nil)

(defun finalize-peek-source ()
  (unless *is-finalzing*
    (let ((*is-finalzing* t))
      (finalize-highlight-overlays)
      (setf (current-window) *parent-window*)
      (delete-window *source-window*)
      (delete-window *peek-window*))))

(defun set-move-function (start end move-function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end 'move-marker t))
  (put-text-property start end 'move-function move-function))

(defun get-move-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point 'move-function)))

(defun start-move-point (point)
  (buffer-start point)
  (unless (text-property-at point 'move-marker)
    (next-move-point point)))

(defun next-move-point (point)
  (when (text-property-at point 'move-marker)
    (next-single-property-change point 'move-marker))
  (next-single-property-change point 'move-marker))

(defun previous-move-point (point)
  (when (text-property-at point 'move-marker)
    (previous-single-property-change point 'move-marker))
  (previous-single-property-change point 'move-marker))

(defun make-two-side-by-side-windows (buffer)
  (let* ((x-margin 4)
         (y-margin 2)
         (width (- (floor (display-width) 2) 2 x-margin))
         (height (- (display-height) 2 (* 2 y-margin)))
         (peek-window (make-instance 'peek-window
                                     :buffer buffer
                                     :x (+ 1 x-margin)
                                     :y (+ 1 y-margin)
                                     :width width
                                     :height height
                                     :use-border t))
         (source-window (make-instance 'source-window
                                       :buffer (make-buffer "*source*" :temporary t :enable-undo-p nil)
                                       :x (+ (window-x peek-window) (window-width peek-window) 2)
                                       :y (+ 1 y-margin)
                                       :width width
                                       :height height
                                       :use-border t)))
    (list peek-window source-window)))

(defun display (collector)
  (destructuring-bind (peek-window source-window)
      (make-two-side-by-side-windows (collector-buffer collector))

    (setf *parent-window* (current-window))
    (setf *peek-window* peek-window)
    (setf *source-window* source-window)

    (setf (current-window) peek-window)
    (peek-source-mode t)

    (start-move-point (buffer-point (collector-buffer collector)))
    (show-matched-line)))

(defun make-peek-source-buffer ()
  (let ((buffer (make-buffer "*peek-source*" :temporary t :enable-undo-p t)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    buffer))

(defun call-with-collecting-sources (function &key read-only)
  (let ((*collector* (make-instance 'collector :buffer (make-peek-source-buffer))))
    (funcall function *collector*)
    (when read-only
      (setf (buffer-read-only-p (collector-buffer *collector*)) t))
    (unless (zerop (collector-count *collector*))
      (display *collector*))))

(defmacro with-collecting-sources ((collector &key (read-only t)) &body body)
  `(call-with-collecting-sources (lambda (,collector)
                                   (declare (ignorable ,collector))
                                   ,@body)
                                 :read-only ,read-only))

(defun call-with-appending-source (insert-function move-function)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (funcall insert-function point)
      (unless (start-line-p point)
        (insert-string point (string #\newline) :read-only t))
      (set-move-function start point move-function))
    (incf (collector-count *collector*))))

(defmacro with-appending-source ((point &key move-function) &body body)
  `(call-with-appending-source (lambda (,point) ,@body)
                               ,move-function))

(defun call-with-insert (function)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (funcall function point)))

(defmacro with-insert ((point) &body body)
  `(call-with-insert (lambda (,point) ,@body)))

;;;
(define-attribute match-line-attribute
  (t :background :base01))

(defun get-matched-point ()
  (alexandria:when-let* ((move (get-move-function (buffer-point (window-buffer *peek-window*))))
                         (point (funcall move)))
    point))

(defun show-matched-line ()
  (alexandria:when-let (point (get-matched-point))
    (let* ((point (copy-point point :temporary))
           (buffer (point-buffer point)))
      (with-current-window *source-window*
        (switch-to-buffer buffer nil nil)
        (update-highlight-overlay point)
        (move-point (buffer-point buffer) point)
        (window-see (current-window))))))

(defmethod execute :after ((mode peek-source-mode) command argument)
  (when (eq (current-window) *peek-window*)
    (show-matched-line)))

(defun highlight-matched-line (point)
  (let ((overlay (make-line-overlay point 'highlight)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :name "highlight-matched-line")
                 300)))

(define-command peek-source-select () ()
  (alexandria:when-let ((point (get-matched-point)))
    (let ((line (line-number-at-point point)))
      (peek-source-quit)
      (switch-to-buffer (point-buffer point))
      (move-to-line (current-point) line)
      (highlight-matched-line (current-point)))))

(define-command peek-source-next () ()
  (next-move-point (current-point)))

(define-command peek-source-previous () ()
  (previous-move-point (current-point)))

(define-command peek-source-quit () ()
  (setf (current-window) *parent-window*)
  (start-timer
   (make-idle-timer (lambda ()
                      (delete-window *peek-window*)
                      (delete-window *source-window*)))
   0))

;;;
(defvar *highlight-overlays* '())

(defun set-highlight-overlay (point)
  (let ((overlay (make-line-overlay point (ensure-attribute 'match-line-attribute))))
    (push overlay *highlight-overlays*)
    (setf (buffer-value (point-buffer point) 'highlight-overlay) overlay)))

(defun get-highlight-overlay (point)
  (buffer-value (point-buffer point) 'highlight-overlay))

(defun update-highlight-overlay (point)
  (let ((overlay (get-highlight-overlay point)))
    (cond (overlay
           (move-point (overlay-start overlay) point)
           (move-point (overlay-end overlay) point))
          (t
           (set-highlight-overlay point)))))

(defun finalize-highlight-overlays ()
  (dolist (overlay *highlight-overlays*)
    (buffer-unbound (overlay-buffer overlay) 'highlight-overlay)
    (delete-overlay overlay))
  (setf *highlight-overlays* '()))
