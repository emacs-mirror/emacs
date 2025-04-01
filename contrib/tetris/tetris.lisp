(defpackage :lem.tetris
  (:use :cl :lem)
  (:export))

(in-package :lem.tetris)

(define-major-mode tetris-mode nil
  (:name "tetris"
   :keymap *tetris-mode-keymap*))

(defconstant +field-width+ 12)
(defconstant +field-height+ 21)
(defconstant +tetrimino-size+ 4)

(defconstant +void+ 0)
(defconstant +wall+ -1)

(defconstant +num-blocks+ 7)

(defparameter *wall-attribute*
  (make-attribute :foreground "gray" :reverse t))

(defparameter *block-attributes*
  (map 'vector (lambda (color-name)
                 (make-attribute :foreground color-name :reverse t))
       '("black" "cyan" "yellow" "green" "red" "blue" "white" "magenta")))

(defvar *tetris-buffer*)

(defvar *tetrimino-table*
  #(#(0 0 0 0
      0 0 0 0
      1 1 1 1
      0 0 0 0)
    #(0 0 0 0
      0 2 2 0
      0 2 2 0
      0 0 0 0)
    #(0 0 0 0
      0 3 3 0
      3 3 0 0
      0 0 0 0)
    #(0 0 0 0
      0 4 4 0
      0 0 4 4
      0 0 0 0)
    #(0 0 0 0
      5 0 0 0
      5 5 5 0
      0 0 0 0)
    #(0 0 0 0
      0 0 6 0
      6 6 6 0
      0 0 0 0)
    #(0 0 0 0
      0 7 0 0
      7 7 7 0
      0 0 0 0)))

(defvar *field*)
(defvar *timer*)

(defvar *playing-p* nil)

(defvar *point-x*)
(defvar *point-y*)
(defvar *current-tetrimino*)
(defvar *next-tetrimino*)
(defvar *hard-dropped*)

(defvar *score*)
(defvar *delete-nlines*)
(defvar *level*)

(defun block-attribute (b)
  (cond ((= b +wall+) *wall-attribute*)
        ((<= 0 b (1- (length *block-attributes*)))
         (aref *block-attributes* b))))

(defun random-tetrimino ()
  (aref *tetrimino-table* (random +num-blocks+)))

(defun init-player ()
  (setq *score* 0)
  (setq *delete-nlines* 0)
  (setq *level* 0)
  (setq *next-tetrimino* (random-tetrimino))
  (reset-player))

(defun reset-player ()
  (setq *point-x* 4)
  (setq *point-y* -1)
  (setq *current-tetrimino* *next-tetrimino*)
  (setq *next-tetrimino* (random-tetrimino))
  (setq *hard-dropped* nil))

(defun init-field ()
  (setq *field*
        (make-array (list +field-width+ +field-height+)
                    :initial-element +void+))
  (dotimes (y +field-height+)
    (setf (aref *field* 0 y)
          (setf (aref *field* (1- +field-width+) y)
                +wall+)))
  (dotimes (x +field-width+)
    (setf (aref *field* x (1- +field-height+)) +wall+))
  *field*)

(defun insert-block (attribute)
  (insert-string (current-point) "  "
                 :attribute attribute))

(defun draw-field-internal (field)
  (dotimes (y +field-height+)
    (dotimes (x +field-width+)
      (let ((b (aref field x y)))
        (insert-block (block-attribute b))))
    (insert-character (current-point) #\newline)))

(defun draw-field ()
  (let ((field (make-array (list +field-width+ +field-height+))))
    (dotimes (y +field-height+)
      (dotimes (x +field-width+)
        (setf (aref field x y)
              (aref *field* x y))))
    (put-tetrimino *current-tetrimino* *point-x* *point-y* field)
    (draw-field-internal field)
    field))

(defun draw-next ()
  (dotimes (y +tetrimino-size+)
    (dotimes (x +tetrimino-size+)
      (insert-block (block-attribute (aref *next-tetrimino* (2d->1d x y)))))
    (insert-character (current-point) #\newline)))

(defun draw-score ()
  (insert-character (current-point) #\newline)
  (insert-string (current-point) (format nil "score: ~d~%" *score*))
  (insert-string (current-point) (format nil "lines: ~d~%" *delete-nlines*))
  (insert-string (current-point) (format nil "level: ~d~%" *level*)))

(defun in-field-point-p (x y)
  (and (<= 0 x (1- +field-width+))
       (<= 0 y (1- +field-height+))))

(defun put-tetrimino (tetrimino point-x point-y field)
  (let ((i 0))
    (loop :for y :from point-y :repeat +tetrimino-size+ :do
      (loop :for x :from point-x :repeat +tetrimino-size+ :do
        (when (and (in-field-point-p x y)
                   (/= +void+ (aref tetrimino i)))
          (setf (aref field x y)
                (aref tetrimino i)))
        (incf i)))))

(defun draw ()
  (erase-buffer *tetris-buffer*)
  (draw-next)
  (draw-field)
  (draw-score))

(defun override-p (point-x
                   point-y
                   &optional
                   (field *field*)
                   (tetrimino *current-tetrimino*))
  (let ((i -1))
    (dotimes (y +tetrimino-size+)
      (dotimes (x +tetrimino-size+)
        (incf i)
        (let ((x (+ x point-x))
              (y (+ y point-y)))
          (when (in-field-point-p x y)
            (when (and (/= +void+ (aref tetrimino i))
                       (/= +void+ (aref field x y)))
              (return-from override-p t))))))))

(defun ride-p ()
  (override-p *point-x* (1+ *point-y*)))

(defun fill-line-p (y)
  (loop :for x :from 1 :below (1- +field-width+) :do
    (when (= (aref *field* x y) +void+)
      (return-from fill-line-p nil)))
  t)

(defun delete-line (y)
  (loop :for x :from 1 :below (1- +field-width+) :do
    (setf (aref *field* x y) +void+)))

(defun shift-lines (start-y)
  (loop :for y :from start-y :downto 1 :do
    (loop :for x :from 1 :below (1- +field-width+) :do
      (setf (aref *field* x y)
            (aref *field* x (1- y))))))

(defun delete-lines ()
  (let ((count 0)
        (y (- +field-height+ 2)))
    (loop
      (cond ((= y 0) (return))
            ((fill-line-p y)
             (incf count)
             (delete-line y)
             (shift-lines y))
            (t
             (decf y))))
    (when (< 0 count)
      (let ((prev-nlines *delete-nlines*))
        (incf *delete-nlines* count)
        (incf *score*
              (* (1+ (if (<= 8 *level*)
                         4
                         (floor *level* 2)))
                 (ecase count
                   (1 100)
                   (2 400)
                   (3 900)
                   (4 2000))))
        (when (< 0 (- (floor *delete-nlines* 4)
                      (floor prev-nlines 4)))
          (incf *level*)
          (when (< 100 (lem/common/timer:timer-ms *timer*))
            (decf (lem/common/timer:timer-ms *timer*) 100)))))))

(defun gameover-p ()
  (loop :for x :from 1 :below (1- +field-width+) :do
    (when (/= +void+ (aref *field* x 0))
      (return t))))

(defun fix ()
  (put-tetrimino *current-tetrimino* *point-x* *point-y* *field*)
  (delete-lines)
  (when (gameover-p)
    (setq *playing-p* nil)
    (stop-timer *timer*)))

(defun 2d->1d (x y)
  (+ x (* y +tetrimino-size+)))

(defun rotated-tetrimino (tetrimino)
  (let ((new-tetrimino (make-array (* +tetrimino-size+ +tetrimino-size+))))
    (loop
      :for y1 :from 0 :below +tetrimino-size+
      :for x2 :from 3 :downto 0
      :do (loop
            :for x1 :from 0 :below +tetrimino-size+
            :for y2 :from 0 :below +tetrimino-size+
            :do (setf (aref new-tetrimino (2d->1d x2 y2))
                      (aref tetrimino (2d->1d x1 y1)))))
    new-tetrimino))

(defun rotate ()
  (let ((tetrimino (rotated-tetrimino *current-tetrimino*)))
    (unless (override-p *point-x* *point-y* *field* tetrimino)
      (setq *current-tetrimino* tetrimino))))

(define-command tetris-move-left () ()
  (when (and *playing-p* (not *hard-dropped*))
    (unless (override-p (1- *point-x*) *point-y*)
      (decf *point-x*)
      (draw))))

(define-command tetris-move-right () ()
  (when (and *playing-p* (not *hard-dropped*))
    (unless (override-p (1+ *point-x*) *point-y*)
      (incf *point-x*)
      (draw))))

(define-command tetris-move-down () ()
  (when (and *playing-p* (not *hard-dropped*))
    (unless (override-p *point-x* (1+ *point-y*))
      (incf *point-y*)
      (draw))))

(define-command tetris-hard-drop () ()
  (when *playing-p*
    (loop :until (override-p *point-x* (1+ *point-y*))
          :do (incf *point-y*))
    (draw)
    (setf *hard-dropped* t)))

(define-command tetris-rotate () ()
  (when *playing-p*
    (rotate)
    (draw)))

(define-command tetris-quit () ()
  (setq *playing-p* nil)
  (stop-timer *timer*))

(define-key *tetris-mode-keymap* "Left" 'tetris-move-left)
(define-key *tetris-mode-keymap* "Right" 'tetris-move-right)
(define-key *tetris-mode-keymap* "Down" 'tetris-move-down)
(define-key *tetris-mode-keymap* "Space" 'tetris-hard-drop)
(define-key *tetris-mode-keymap* "Up" 'tetris-rotate)
(define-key *tetris-mode-keymap* "q" 'tetris-quit)

(defun update (tetris-buffer)
  (when (and (eq (current-buffer) tetris-buffer)
             *playing-p*)
    (cond ((ride-p)
           (fix)
           (reset-player)
           (draw))
          (t
           (tetris-move-down)))))

(define-command tetris () ()
  (setf *tetris-buffer* (make-buffer "*tetris*"))
  (switch-to-buffer *tetris-buffer*)
  (tetris-mode)
  (add-hook (variable-value 'kill-buffer-hook :buffer *tetris-buffer*)
            #'(lambda (buffer) (declare (ignore buffer)) (tetris-quit)))
  (init-field)
  (init-player)
  (draw)
  (setq *playing-p* t)
  (setq *timer* (start-timer (make-timer (lambda () (update *tetris-buffer*))
                                         :handle-function (lambda (condition)
                                                            (pop-up-backtrace condition)
                                                            (stop-timer *timer*)))
                             1000
                             :repeat t)))
