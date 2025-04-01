(defpackage :lem-ncurses/view
  (:use :cl
        :lem-ncurses/style)
  (:export :make-view
           :view
           :view-border
           :view-scrwin
           :view-modeline-scrwin
           :view-x
           :view-y
           :view-width
           :view-height
           :delete-view
           :clear
           :set-view-size
           :set-view-pos
           :redraw-view-after
           :render-line
           :render-line-on-modeline
           :clear-to-end-of-window
           :update-cursor
           :set-last-print-cursor))
(in-package :lem-ncurses/view)

(defclass view ()
  ((border :initarg :border :reader view-border)
   (scrwin :initarg :scrwin :reader view-scrwin)
   (modeline-scrwin :initarg :modeline-scrwin :reader view-modeline-scrwin)
   (x :initarg :x :accessor view-x)
   (y :initarg :y :accessor view-y)
   (width :initarg :width :accessor view-width)
   (height :initarg :height :accessor view-height)
   (last-print-cursor-x :initform 0 :accessor view-last-print-cursor-x)
   (last-print-cursor-y :initform 0 :accessor view-last-print-cursor-y)
   (cursor-invisible-p :initarg :cursor-invisible :reader view-cursor-invisible-p)))

(defun set-last-print-cursor (view x y)
  (setf (view-last-print-cursor-x view) x
        (view-last-print-cursor-y view) y))

(deftype border-shape ()
  '(member nil :drop-curtain :left-border))

(defstruct border
  win
  width
  height
  size
  (shape nil :type border-shape))

(defun compute-border-window-position (x y border-size border-shape)
  (case border-shape
    (:left-border
     (list (- x border-size) y))
    (otherwise
     (let ((x (- x border-size))
           (y (- y border-size)))
       (list x y)))))

(defun compute-border-window-size (width height border-size border-shape)
  (case border-shape
    (:left-border
     (list 1 height))
    (otherwise
     (let ((width (+ width (* border-size 2)))
           (height (+ height (* border-size 2))))
       (list width height)))))

(defun make-view (x y width height &key modeline type border border-shape cursor-invisible)
  (check-type type (or null (member :tile :floating)))
  (check-type border (or null integer))
  (check-type border-shape border-shape)
  (flet ((newwin (nlines ncols begin-y begin-x)
           (let ((win (charms/ll:newwin nlines ncols begin-y begin-x)))
             (when modeline (charms/ll:keypad win 1))
             win)))
    (make-instance 'view
                   :border (when (and border (< 0 border))
                             (destructuring-bind (x y)
                                 (compute-border-window-position x y border border-shape)
                               (destructuring-bind (width height)
                                   (compute-border-window-size width height border border-shape)
                                 (let ((win (newwin height width y x)))
                                   (make-border :win win
                                                :width width
                                                :height height
                                                :size border
                                                :shape border-shape)))))
                   :scrwin (newwin height width y x)
                   :modeline-scrwin (when modeline (newwin 1 width (+ y height) x))
                   :x x
                   :y y
                   :width width
                   :height height
                   :cursor-invisible cursor-invisible)))

(defun delete-view (view)
  (charms/ll:delwin (view-scrwin view))
  (when (view-modeline-scrwin view)
    (charms/ll:delwin (view-modeline-scrwin view))))

(defun clear (view)
  (charms/ll:clearok (view-scrwin view) 1)
  (when (view-modeline-scrwin view)
    (charms/ll:clearok (view-modeline-scrwin view) 1)))

(defun set-view-size (view width height)
  (setf (view-width view) width)
  (setf (view-height view) height)
  (charms/ll:wresize (view-scrwin view) height width)
  (alexandria:when-let (border (view-border view))
    (destructuring-bind (b-width b-height)
        (compute-border-window-size width height (border-size border) (border-shape border))
      (setf (border-width border) b-width
            (border-height border) b-height)
      (charms/ll:wresize (border-win border) b-height b-width)))
  (when (view-modeline-scrwin view)
    (charms/ll:mvwin (view-modeline-scrwin view)
                     (+ (view-y view) height)
                     (view-x view))
    (charms/ll:wresize (view-modeline-scrwin view)
                       1
                       width)))

(defun set-view-pos (view x y)
  (setf (view-x view) x)
  (setf (view-y view) y)
  (charms/ll:mvwin (view-scrwin view) y x)
  (alexandria:when-let (border (view-border view))
    (destructuring-bind (b-x b-y)
        (compute-border-window-position x y (border-size border) (border-shape border))
      (cond ((eq :left-border (border-shape (view-border view)))
             (charms/ll:mvwin (border-win border) b-y b-x))
            (t
             (charms/ll:mvwin (border-win border) b-y b-x)))))
  (when (view-modeline-scrwin view)
    (charms/ll:mvwin (view-modeline-scrwin view)
                     (+ y (view-height view))
                     x)))

(defun draw-border (border)
  (let ((win (border-win border))
        (h (1- (border-height border)))
        (w (1- (border-width border)))
        (attr (lem-ncurses/attribute:attribute-to-bits (border-attribute))))
    (charms/ll:wattron win attr)
    (flet ((drop-curtain ()
             (charms/ll:mvwaddstr win 0 0 (border-vertical-and-right))
             (charms/ll:mvwaddstr win 0 w (border-vertical-and-left))
             (charms/ll:mvwaddstr win h 0 (border-downleft))
             (charms/ll:mvwaddstr win h w (border-downright))
             (loop :for x :from 1 :below w
                   :do (charms/ll:mvwaddstr win 0 x (border-up))
                       (charms/ll:mvwaddstr win (1- (border-height border)) x (border-down)))
             (loop :for y :from 1 :below h
                   :do (charms/ll:mvwaddstr win y 0 (border-left))
                       (charms/ll:mvwaddstr win y w (border-right))))
           (left-border ()
             (loop :for y :from 0 :to h
                   :do (charms/ll:mvwaddstr win y 0 (border-left))))
           (normal ()
             (charms/ll:mvwaddstr win 0 0 (border-upleft))
             (charms/ll:mvwaddstr win 0 w (border-upright))
             (charms/ll:mvwaddstr win h 0 (border-downleft))
             (charms/ll:mvwaddstr win h w (border-downright))
             (loop :for x :from 1 :below w
                   :do (charms/ll:mvwaddstr win 0 x (border-up))
                       (charms/ll:mvwaddstr win (1- (border-height border)) x (border-down)))
             (loop :for y :from 1 :below h
                   :do (charms/ll:mvwaddstr win y 0 (border-left))
                       (charms/ll:mvwaddstr win y w (border-right)))))
      (case (border-shape border)
        (:drop-curtain (drop-curtain))
        (:left-border (left-border))
        (otherwise (normal))))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh win)))

(defun redraw-view-after (view)
  (alexandria:when-let (border (view-border view))
    (draw-border border))
  (let ((attr (lem-ncurses/attribute:attribute-to-bits 'modeline-inactive)))
    (charms/ll:attron attr)
    (when (and (view-modeline-scrwin view)
               (< 0 (view-x view)))
      (charms/ll:move (view-y view) (1- (view-x view)))
      (loop :for y :from 0 :to (view-height view)
            :do (charms/ll:mvaddstr (+ (view-y view) y)
                                    (1- (view-x view))
                                    (border-left))))
    (charms/ll:attroff attr)
    (charms/ll:wnoutrefresh charms/ll:*stdscr*))
  (when (view-modeline-scrwin view)
    (charms/ll:wnoutrefresh (view-modeline-scrwin view)))
  (charms/ll:wnoutrefresh (view-scrwin view)))

(defun update-cursor (view)
  (let ((cursor-x (view-last-print-cursor-x view))
        (cursor-y (view-last-print-cursor-y view))
        (scrwin (view-scrwin view)))
    (cond ((view-cursor-invisible-p view)
           (charms/ll:curs-set 0))
          (t
           (charms/ll:curs-set 1)
           (charms/ll:wmove scrwin cursor-y cursor-x)))
    (charms/ll:wnoutrefresh scrwin)
    (charms/ll:doupdate)))
