(uiop:define-package :lem-terminal/terminal
  (:use :cl :lem)
  (:local-nicknames (:ffi :lem-terminal/ffi)
                    (:queue :lem/common/queue))
  (:export :find-terminal-buffer
           :create
           :destroy
           :copy-mode-on
           :copy-mode-off
           :clear
           :render
           :update
           :input-character
           :input-key
           :resize
           :adjust-point))
(in-package :lem-terminal/terminal)

;;; id generator
(defvar *terminal-id-counter* 0)

(defun generate-terminal-id ()
  (incf *terminal-id-counter*))

;;; terminals
(defvar *terminals* '())

(defun add-terminal (terminal)
  (push terminal *terminals*))

(defun find-terminal-by-id (id)
  (find id *terminals* :key #'terminal-id))

(defun remove-terminal (terminal)
  (alexandria:deletef *terminals* terminal))

(defun find-terminal-buffer ()
  (alexandria:when-let (terminal (first *terminals*))
    (terminal-buffer terminal)))

;;; terminal
(defclass terminal ()
  ((id :initarg :id
       :reader terminal-id)
   (viscus :initarg :viscus
           :reader terminal-viscus)
   (thread :initarg :thread
           :accessor terminal-thread)
   (buffer :initarg :buffer
           :reader terminal-buffer)
   (rows :initarg :rows
         :accessor terminal-rows)
   (cols :initarg :cols
         :accessor terminal-cols)
   (copy-mode :initform nil
              :accessor terminal-copy-mode)))

(defun update (terminal)
  (process-input terminal)
  (when (and (= 0 (event-queue-length))
             (not (terminal-copy-mode terminal)))
    (render terminal)
    (redraw-display)))

(defun create (&key (rows (alexandria:required-argument :rows))
                    (cols (alexandria:required-argument :cols))
                    (buffer (alexandria:required-argument :buffer))
                    (directory (alexandria:required-argument :directory)))
  (declare (type (string) directory)
           (type (integer) rows)
           (type (integer) cols))
  (let* ((id (generate-terminal-id))
         (terminal
           (make-instance 'terminal
                          :id id
                          :viscus (ffi::terminal-new directory id rows cols)
                          :buffer buffer
                          :rows rows
                          :cols cols)))
    (let ((queue (queue:make-concurrent-queue)))
      (setf (terminal-thread terminal)
            (bt2:make-thread
             (lambda ()
               (loop
                 (ffi::terminal-process-input-wait (terminal-viscus terminal))
                 (send-event
                  (lambda ()
                    ;; XXX: If this place is executed at the time the terminal is deleted, an error will occur.
                    (ignore-errors (update terminal))
                    (queue:enqueue queue 1)))
                 (queue:dequeue queue)))
             :name (format nil "Terminal ~D" id))))
    (add-terminal terminal)
    terminal))

(defmethod destroy ((terminal terminal))
  (bt2:destroy-thread (terminal-thread terminal))
  (remove-terminal terminal)
  (ffi::terminal-delete (terminal-viscus terminal)))

(defmethod copy-mode-on ((terminal terminal))
  (setf (terminal-copy-mode terminal) t))

(defmethod copy-mode-off ((terminal terminal))
  (setf (terminal-copy-mode terminal) nil))

(defun get-foreground-color (viscus)
  (let ((r (ffi::terminal-last-cell-fg-red viscus))
        (g (ffi::terminal-last-cell-fg-green viscus))
        (b (ffi::terminal-last-cell-fg-blue viscus)))
    (make-color r g b)))

(defun get-background-color (viscus)
  (let ((r (ffi::terminal-last-cell-bg-red viscus))
        (g (ffi::terminal-last-cell-bg-green viscus))
        (b (ffi::terminal-last-cell-bg-blue viscus)))
    (make-color r g b)))

(defun fix-blue-color (color)
  (if (lem/common/color::color-equal color (make-color 0 0 #xe0))
      (parse-color "#3465A4")
      color))

(defun get-cell-attribute (viscus)
  (let ((foreground (get-foreground-color viscus))
        (background (get-background-color viscus))
        (reverse (= 1 (ffi::terminal-last-cell-attrs-reverse viscus)))
        (underline (= 1 (ffi::terminal-last-cell-attrs-underline viscus)))
        (bold (= 1 (ffi::terminal-last-cell-attrs-bold viscus))))
    (make-attribute :foreground (fix-blue-color foreground)
                    :background (fix-blue-color background)
                    :reverse reverse
                    :bold bold
                    :underline underline)))

(defun get-cell-character (viscus)
  (let ((chars (ffi::terminal-last-cell-chars viscus)))
    (when chars
      (let ((char (ignore-errors (code-char (cffi:mem-ref chars :uint32 0)))))
        char))))

(defmethod render ((terminal terminal))
  (let* ((viscus (terminal-viscus terminal))
         (rows (terminal-rows terminal))
         (cols (terminal-cols terminal))
         (buffer (terminal-buffer terminal))
         (point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (loop :for row :from 0 :below rows
            :do (loop :with previous-attribute := nil
                      :and string := ""
                      :for col :from 0 :below cols
                      :do (ffi::terminal-query-cell viscus col row)
                          (let ((char (get-cell-character viscus))
                                (attribute (get-cell-attribute viscus)))
                            (when char
                              (unless (attribute-equal attribute previous-attribute)
                                (insert-string point string :attribute previous-attribute)
                                (setf previous-attribute attribute)
                                (setf string ""))
                              (setf string
                                    (concatenate 'string
                                                 string
                                                 (string (if (eql char #\Nul) #\Space char))))))
                      :finally (insert-string point string :attribute previous-attribute))
                (insert-character point #\newline)))
    (move-to-line point (1+ (ffi::terminal-cursor-row viscus)))
    (move-to-column point (ffi::terminal-cursor-col viscus))))

(defmethod adjust-point ((terminal terminal))
  (let* ((buffer (terminal-buffer terminal))
         (point (buffer-point buffer))
         (viscus (terminal-viscus terminal)))
    (move-to-line point (1+ (ffi::terminal-cursor-row viscus)))
    (move-to-column point (ffi::terminal-cursor-col viscus))))

(defmethod process-input ((terminal terminal))
  (ffi::terminal-process-input-nonblock (terminal-viscus terminal)))

(defmethod input-character ((terminal terminal) character &key (mod 0))
  (ffi::terminal-input-char (terminal-viscus terminal)
                            (char-code character)
                            mod))

(defmethod input-key ((terminal terminal) key &key (mod 0))
  (ffi::terminal-input-key (terminal-viscus terminal)
                           key
                           mod))

(defun same-size-p (terminal rows cols)
  (and (= (terminal-rows terminal) rows)
       (= (terminal-cols terminal) cols)))

(defmethod resize ((terminal terminal)
                   &key (rows (alexandria:required-argument :rows))
                        (cols (alexandria:required-argument :cols)))
  (unless (same-size-p terminal rows cols)
    (setf (terminal-rows terminal) rows
          (terminal-cols terminal) cols)
    (ffi::terminal-resize (terminal-viscus terminal) rows cols)))

;;; callbacks
(defun cb-damage (rect id)
  (declare (ignore rect id)))

(defun cb-moverect (dest src id)
  (declare (ignore dest src id)))

(defun cb-movecursor (pos oldpos visible id)
  (declare (ignore pos oldpos visible id)))

(defun cb-settermprop (prop val id)
  (declare (ignore prop val id)))

(defun cb-bell (id)
  (declare (ignore id)))

(defun cb-resize (rows cols id)
  (let ((terminal (find-terminal-by-id id)))
    (setf (terminal-rows terminal) rows
          (terminal-cols terminal) cols)))

(defun cb-sb-pushline (cols cells id)
  (declare (ignore cols cells id)))

(defun cb-sb-popline (cols cells id)
  (declare (ignore cols cells id)))

(ffi::set-callbacks :damage 'cb-damage
                    :moverect 'cb-moverect
                    :movecursor 'cb-movecursor
                    :settermprop 'cb-settermprop
                    :bell 'cb-bell
                    :resize 'cb-resize
                    :sb-pushline 'cb-sb-pushline
                    :sb-popline 'cb-sb-popline)
