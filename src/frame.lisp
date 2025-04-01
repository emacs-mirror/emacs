(in-package :lem-core)

(defvar *display-frame-map* (make-hash-table))
(defvar *frames* '())

(defgeneric update-prompt-window (window)
  (:method (window)))

(defclass frame ()
  ((window-left-margin
    :initarg :window-left-margin
    :reader frame-window-left-margin)
   (current-window
    :initarg :current-window
    :initform nil
    :accessor frame-current-window)
   (window-tree
    :initarg :window-tree
    :initform nil
    :accessor frame-window-tree)
   (floating-windows
    :initarg :floating-windows
    :initform '()
    :accessor frame-floating-windows)
   (header-windows
    :initarg :header-windows
    :initform '()
    :reader frame-header-windows)
   (modified-floating-windows
    :initform nil
    :reader frame-modified-floating-windows
    :writer set-frame-modified-floating-windows
    :documentation "このスロットがTの場合、
floating-windowが追加/削除/大きさの変更などがあったことを示します。
redraw-display関数でこのスロットを参照して、必要なウィンドウの再描画を判断するために使います。
またredraw-display内でNILにセットされます。
このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (modified-header-windows
    :initform nil
    :reader frame-modified-header-windows
    :writer set-frame-modified-header-windows
    :documentation "このスロットがTの場合、header-windowが追加または削除されたことを示します。
redraw-display関数でこのスロットを参照して、画面の再描画時にウィンドウの大きさを
計算仕直す必要があるか判断します。
またredraw-display内でNILにセットされます。
このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (require-redisplay-windows
    :initform nil
    :reader frame-require-redisplay-windows
    :writer set-frame-require-redisplay-windows
    :documentation "このスロットがTの場合、
redraw-display関数でキャッシュを捨てて画面全体を再描画します。

このスロットが使われるケース
- frame-multiplexer-delete呼び出し後にredraw-displayで再描画時に画面の最小限の更新だけでは
  変更前のフレームが残ってしまう問題がありキャッシュを無効化する必要がある
- インアクティブなウィンドウの背景色を変える場合、
  ウィンドウ切り替え時などに画面全体を再描画する必要がある

このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (prompt-window
    :initform nil
    :accessor frame-floating-prompt-window)
   (message-window
    :initform nil
    :accessor frame-message-window)
   (leftside-window
    :initform nil
    :accessor frame-leftside-window)
   (rightside-window
    :initform nil
    :accessor frame-rightside-window)))

(defmethod notify-floating-window-modified ((frame frame))
  (set-frame-modified-floating-windows t frame))

(defmethod notify-header-window-modified ((frame frame))
  (set-frame-modified-header-windows t frame))

(defmethod notify-frame-redisplay-required ((frame frame))
  (set-frame-require-redisplay-windows t frame))

(defmethod notify-frame-redraw-finished ((frame frame))
  (set-frame-modified-header-windows nil frame)
  (set-frame-modified-floating-windows nil frame)
  (set-frame-require-redisplay-windows nil frame))

(defmethod frame-prompt-window ((frame frame))
  (frame-floating-prompt-window frame))

(defmethod frame-caller-of-prompt-window ((frame frame))
  (caller-of-prompt-window (frame-prompt-window frame)))

(defmethod frame-prompt-active-p ((frame frame))
  (alexandria:when-let (prompt (frame-prompt-window frame))
    (prompt-active-p prompt)))

(defun make-frame (&optional (old-frame (current-frame)))
  (let ((frame (make-instance 'frame :window-left-margin (window-left-margin (implementation)))))
    (push frame *frames*)
    (when old-frame
      (dolist (window (frame-header-windows old-frame))
        (add-header-window frame window)))
    frame))

(defun map-frame (display frame)
  (setf (gethash display *display-frame-map*) frame))

(defun get-frame (display)
  (gethash display *display-frame-map*))

(defun all-frames ()
  *frames*)

(defun current-frame ()
  (get-frame (implementation)))

(defun unmap-frame (display)
  (let ((frame (gethash display *display-frame-map*)))
    (remhash display frame)
    frame))

(defun setup-frame (frame buffer)
  (setup-frame-windows frame buffer))

(defun teardown-frame (frame)
  (alexandria:deletef *frames* frame)
  (teardown-windows frame))

(defun teardown-frames ()
  (maphash (lambda (k v)
             (declare (ignore k))
             (teardown-frame v))
           *display-frame-map*))

(defun window-in-frame-p (window frame)
  (when (or (find window (window-list frame))
            (find window (frame-floating-windows frame))
            (find window (frame-header-windows frame))
            (eq window (frame-leftside-window frame))
            (eq window (frame-rightside-window frame)))
    t))

(defun get-frame-of-window (window)
  ;; TODO: frameのリストを用意し、その中から探すようにする
  (when (window-in-frame-p window (current-frame))
    (current-frame)))

(defun update-floating-prompt-window (frame)
  (when (frame-floating-prompt-window frame)
    (update-prompt-window (frame-floating-prompt-window frame))))


(defun add-floating-window (frame window)
  (alexandria:nconcf (frame-floating-windows frame)
                     (list window)))

(defun remove-floating-windows (frame window)
  (alexandria:deletef (frame-floating-windows frame)
                      window))

(defun add-header-window (frame window)
  (with-slots (header-windows) frame
    (alexandria:nconcf header-windows (list window))))

(defun remove-header-window (frame window)
  (with-slots (header-windows) frame
    (setf header-windows (remove window header-windows))))


(defun topleft-window-y (frame)
  (length (frame-header-windows frame)))

(defun topleft-window-x (frame)
  (if (null (frame-leftside-window frame))
      0
      (1+ (window-width (frame-leftside-window frame)))))

(defun max-window-width (frame)
  (- (display-width)
     (topleft-window-x frame)
     (if (frame-rightside-window frame)
         (+ (window-width (frame-rightside-window frame))
            1 ; border
            )
         0)))

(defun max-window-height (frame)
  (- (display-height)
     (topleft-window-y frame)))


(defun within-window-p (window x y)
  (check-type x integer)
  (check-type y integer)
  (and (<= (window-x window) x (+ (window-x window) (window-width window) -1))
       (<= (window-y window) y (+ (window-y window) (window-height window) -1))))

(defun focus-window-position (frame x y)
  (check-type x integer)
  (check-type y integer)
  (dolist (window (append (frame-header-windows frame)
                          ;; リストの後ろにあるウィンドウほど手前に出てくるという前提
                          (reverse (frame-floating-windows frame))
                          (window-list frame)))
    (when (within-window-p window x y)
      (let ((overlay-x-offset (window-left-width window)))
        (return (values window
                        (- x (window-x window) overlay-x-offset)
                        (- y (window-y window))))))))

(defun focus-separator-position (frame x y)
  (when (and (frame-leftside-window frame)
             (= x (window-width (frame-leftside-window frame))))
    (return-from focus-separator-position (values :leftside (frame-leftside-window frame))))
  (when (and (frame-rightside-window frame)
             (= x (1- (window-x (frame-rightside-window frame)))))
    (return-from focus-separator-position (values :rightside (frame-rightside-window frame))))
  (dolist (window (window-list frame))
    (when (and (= x (1- (window-x window)))
               (<= (window-y window) y (+ (window-y window) (window-height window) -1)))
      (return (values :vertical (left-window window) window)))
    (when (and (window-use-modeline-p window)
               (<= (window-x window) x (+ (window-x window) (window-width window) -1))
               (= y (+ (window-y window) (window-height window) -1)))
      (return (values :horizontal (down-window window) window)))))
