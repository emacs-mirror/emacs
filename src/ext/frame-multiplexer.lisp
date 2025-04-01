(defpackage :lem/frame-multiplexer
  (:use :cl
        :lem
        :lem/button
        :lem/common/ring)
  (:export :*keymap*
           :frame-multiplexer-active-frame-name-attribute
           :frame-multiplexer-frame-name-attribute
           :frame-multiplexer-background-attribute
           :frame-multiplexer-advice
           :frame-multiplexer-next
           :frame-multiplexer-prev
           :frame-multiplexer-switch
           :frame-multiplexer-switch-0
           :frame-multiplexer-switch-1
           :frame-multiplexer-switch-2
           :frame-multiplexer-switch-3
           :frame-multiplexer-switch-4
           :frame-multiplexer-switch-5
           :frame-multiplexer-switch-6
           :frame-multiplexer-switch-7
           :frame-multiplexer-switch-8
           :frame-multiplexer-switch-9
           :frame-multiplexer-create-with-new-buffer-list
           :frame-multiplexer-delete
           :frame-multiplexer-recent
           :frame-multiplexer-rename
           :toggle-frame-multiplexer
           :frame-multiplexer-normalize-ids)
  #+sbcl
  (:lock t))
(in-package :lem/frame-multiplexer)

(defconstant +max-number-of-frames+ 256)
(defconstant +max-width-of-each-frame-name+ 20)

(defvar *virtual-frame-map* (make-hash-table))
(defvar *recent-list* nil)

(define-attribute frame-multiplexer-active-frame-name-attribute
  (t :foreground "white" :background "CornflowerBlue" :bold t))

(define-attribute frame-multiplexer-frame-name-attribute
  (t :foreground "black" :background "dark gray" :bold t))

(define-attribute frame-multiplexer-background-attribute
  (t :foreground "white" :background "#262626"))

(define-editor-variable frame-multiplexer nil ""
  (lambda (value)
    (if value
        (frame-multiplexer-on)
        (frame-multiplexer-off))))

(defvar *keymap*
  (make-keymap :name '*frame-multiplexer-keymap*)
  "Keymap for commands related to the frame-multiplexer.")

(define-key *keymap* "c" 'frame-multiplexer-create-with-new-buffer-list)
(define-key *keymap* "C" 'frame-multiplexer-create-with-previous-buffer)
(define-key *keymap* "d" 'frame-multiplexer-delete)
(define-key *keymap* "p" 'frame-multiplexer-prev)
(define-key *keymap* "n" 'frame-multiplexer-next)
(define-key *keymap* "r" 'frame-multiplexer-rename)
(define-key *keymap* "C-z" 'frame-multiplexer-recent)
(define-key *keymap* "z" 'frame-multiplexer-recent)
(define-key *keymap* "0" 'frame-multiplexer-switch-0)
(define-key *keymap* "1" 'frame-multiplexer-switch-1)
(define-key *keymap* "2" 'frame-multiplexer-switch-2)
(define-key *keymap* "3" 'frame-multiplexer-switch-3)
(define-key *keymap* "4" 'frame-multiplexer-switch-4)
(define-key *keymap* "5" 'frame-multiplexer-switch-5)
(define-key *keymap* "6" 'frame-multiplexer-switch-6)
(define-key *keymap* "7" 'frame-multiplexer-switch-7)
(define-key *keymap* "8" 'frame-multiplexer-switch-8)
(define-key *keymap* "9" 'frame-multiplexer-switch-9)
(define-key *global-keymap* "C-z" *keymap*)

(defstruct tab
  focus-p
  number
  buffer-name)

(defstruct frame-table-entry
  frame
  name)

(defun make-tabs (virtual-frame frames)
  (loop :for frame :in frames
        :for id := (find-frame-id virtual-frame frame)
        :for entry := (aref (virtual-frame-id/frame-table virtual-frame) id)
        :collect (make-tab
                  :focus-p (eq frame
                               (virtual-frame-current virtual-frame))
                  :number id
                  :buffer-name (let* ((buffer (window-buffer (frame-current-window frame)))
                                      (name (if (and (frame-table-entry-name entry)
                                                     (< 0 (length (frame-table-entry-name entry))))
                                                (frame-table-entry-name entry)
                                                (buffer-name buffer))))
                                 (if (>= (length name) +max-width-of-each-frame-name+)
                                     (format nil "~a..."
                                             (subseq name 0 +max-width-of-each-frame-name+))
                                     name)))))

(defun tab-content (tab)
  (values (format nil " ~A: ~A "
                  (tab-number tab)
                  (tab-buffer-name tab))
          (if (tab-focus-p tab)
              'frame-multiplexer-active-frame-name-attribute
              'frame-multiplexer-frame-name-attribute)))

(defun tab= (tab1 tab2)
  (and (equal (tab-focus-p tab1) (tab-focus-p tab2))
       (equal (tab-number tab1) (tab-number tab2))
       (equal (tab-buffer-name tab1) (tab-buffer-name tab2))))

(defun equal-tabs (tabs1 tabs2)
  (and (= (length tabs1) (length tabs2))
       (every #'tab= tabs1 tabs2)))

(defclass virtual-frame (header-window)
  ((implementation
    :initarg :impl
    :initform nil
    :accessor virtual-frame-impl
    :type (or null implementation))
   (id/frame-table
    :initarg :id/frame-table
    :accessor virtual-frame-id/frame-table
    :type array)
   (current
    :initarg :current
    :accessor virtual-frame-current
    :type frame)
   (display-width
    :initarg :width
    :accessor virtual-frame-width)
   (display-height
    :initarg :height
    :accessor virtual-frame-height)
   (buffer
    :initarg :buffer
    :accessor virtual-frame-header-buffer)
   (last-displayed-tabs
    :initform nil
    :accessor virtual-frame-last-displayed-tabs)))

(defun make-virtual-frame (impl frame)
  (declare (type frame frame))
  (let* ((buffer (make-buffer "*frame-multiplexer*" :enable-undo-p nil :temporary t))
         (id/frame-table (make-array +max-number-of-frames+ :initial-element nil)))
    (setf (aref id/frame-table 0) (make-frame-table-entry :frame frame :name nil))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (make-instance 'virtual-frame
                   :impl impl
                   :buffer buffer
                   :width (display-width)
                   :height (display-height)
                   :id/frame-table id/frame-table
                   :current frame)))

(defun switch-current-frame (virtual-frame frame)
  ;; record in the recent list
  (when *recent-list*
    (ring-push *recent-list* (find-frame-id virtual-frame (virtual-frame-current virtual-frame))))
  ;; save buffer-point to window-point
  (move-point (lem-core::%window-point (current-window))
              (lem-core::window-buffer-point (current-window)))

  (setf (virtual-frame-current virtual-frame) frame)
  (notify-frame-redisplay-required frame)
  (map-frame (implementation) frame)

  ;; set current-buffer
  (setf (current-buffer) (window-buffer (current-window)))

  ;; restore buffer-point from window-point
  (move-point (lem-core::window-buffer-point (current-window))
              (lem-core::%window-point (current-window))))

(defun find-unused-frame-id (virtual-frame)
  (position-if #'null (virtual-frame-id/frame-table virtual-frame)))

(defun find-frame-id (virtual-frame frame)
  (position-if (lambda (entry) (and entry (eq frame (frame-table-entry-frame entry))))
               (virtual-frame-id/frame-table virtual-frame)))

(defun find-frame-table-entry (virtual-frame frame)
  (find-if (lambda (entry) (and entry (eq frame (frame-table-entry-frame entry))))
           (virtual-frame-id/frame-table virtual-frame)))

(defun num-frames (virtual-frame)
  (count-if-not #'null (virtual-frame-id/frame-table virtual-frame)))

(defun allocate-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-unused-frame-id virtual-frame)))
    (assert id)
    ;; NOTE:
    ;; primordial-bufferは現在のバッファリストから*tmp*バッファを返すが
    ;; バッファリストをフレームごとに管理する場合にここで*tmp*バッファを返すと
    ;; 元のフレームの*tmp*バッファを新しい方のフレームから参照することになってしまう
    (setup-frame frame (primordial-buffer))
    (setf (aref (virtual-frame-id/frame-table virtual-frame) id)
          (make-frame-table-entry :frame frame :name nil))))

(defun free-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((id (find-frame-id virtual-frame frame)))
    (assert id)
    (teardown-frame frame)
    (setf (aref (virtual-frame-id/frame-table virtual-frame) id)
          nil)))

(defun get-frame-from-id (virtual-frame id)
  (let ((entry (aref (virtual-frame-id/frame-table virtual-frame) id)))
    (when entry
      (frame-table-entry-frame entry))))

(defun linear-search-frame (virtual-frame frame dir wrap)
  (let ((id (find-frame-id virtual-frame frame)))
    (loop :for n := (funcall wrap (+ id dir)) :then (funcall wrap (+ n dir))
          :until (= n id)
          :do (unless (null (get-frame-from-id virtual-frame n))
                (return-from linear-search-frame (get-frame-from-id virtual-frame n))))))

(defun search-previous-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((len (length (virtual-frame-id/frame-table virtual-frame))))
    (linear-search-frame virtual-frame
                         frame
                         -1
                         (lambda (n)
                           (if (minusp n)
                               (+ (1- len) n)
                               n)))))

(defun search-next-frame (virtual-frame frame)
  (declare (type frame frame))
  (let ((len (length (virtual-frame-id/frame-table virtual-frame))))
    (linear-search-frame virtual-frame
                         frame
                         1
                         (lambda (n)
                           (if (>= n len)
                               (- len n)
                               n)))))

(defun virtual-frame-frames (virtual-frame)
  (map 'list (lambda (entry) (frame-table-entry-frame entry))
       (remove-if #'null (virtual-frame-id/frame-table virtual-frame))))

(defun virtual-frame-entries (virtual-frame)
  (coerce (remove-if #'null (virtual-frame-id/frame-table virtual-frame)) 'list))

(defun insert-tab-content (point tab action)
  (insert-string point " " :attribute 'frame-multiplexer-background-attribute)
  (multiple-value-bind (text attribute) (tab-content tab)
    (insert-button point
                   text
                   action
                   :attribute attribute
                   :without-mouse-hover-highlight t)))

(defun write-tabs-to-buffer (window frames tabs)
  (let* ((buffer (virtual-frame-header-buffer window))
         (p (buffer-point buffer))
         (charpos (point-charpos p)))
    (erase-buffer buffer)
    (loop :for frame :in frames
          :for tab :in tabs
          :do (let ((start-pos (point-charpos p)))
                (insert-tab-content p
                                    tab
                                    (let ((frame frame))
                                      (lambda ()
                                        (switch-current-frame window frame)
                                        (lem:update-on-display-resized))))
                (when (tab-focus-p tab)
                  ;; set buffer-point to that focused tab position
                  (let ((end-pos (point-charpos p)))
                    (unless (<= start-pos charpos (1- end-pos))
                      (setf charpos start-pos))))))

    ;; fill right margin after the tabs
    (let ((margin-right (- (display-width) (point-column p))))
      (when (> margin-right 0)
        (insert-string p (make-string margin-right :initial-element #\space)
                       :attribute 'frame-multiplexer-background-attribute)))
    (line-offset p 0 charpos)))

(defun display-resized-p (virtual-frame)
  (not (and (= (display-width)
               (virtual-frame-width virtual-frame))
            (= (display-height)
               (virtual-frame-height virtual-frame)))))

(defmethod window-redraw ((window virtual-frame) force)
  (let* ((frames (virtual-frame-frames window))
         (tabs (make-tabs window frames)))
    (when (or force
              (display-resized-p window)
              (not (equal-tabs tabs (virtual-frame-last-displayed-tabs window))))
      (setf (virtual-frame-last-displayed-tabs window) tabs)
      (write-tabs-to-buffer window frames tabs))
    (setf (virtual-frame-width window) (display-width))
    (setf (virtual-frame-height window) (display-height))
    (call-next-method)))

(defun frame-multiplexer-init ()
  (clrhash *virtual-frame-map*)
  (loop
    :for impl :in (list (implementation))  ; for multi-frame support in the future...
    :do (let ((vf (make-virtual-frame impl (get-frame impl))))
          (setf (gethash impl *virtual-frame-map*) vf)
          (switch-current-frame vf (virtual-frame-current vf)))))

(defun enabled-frame-multiplexer-p ()
  (variable-value 'frame-multiplexer :global))

(defun check-frame-multiplexer-usable ()
  (unless (enabled-frame-multiplexer-p)
    (editor-error "frame-multiplexer-mode is not enabled"))
  (when (lem/prompt-window:current-prompt-window)
    (editor-error "prompt window is active")))

(defun frame-multiplexer-on ()
  (unless (enabled-frame-multiplexer-p)
    (frame-multiplexer-init)))

(defun frame-multiplexer-off ()
  (when (enabled-frame-multiplexer-p)
    (maphash (lambda (k v)
               (declare (ignore k))
               (delete-window v))
             *virtual-frame-map*)
    (clrhash *virtual-frame-map*)))

(defclass frame-multiplexer-advice () ())

(define-command (toggle-frame-multiplexer (:advice-classes frame-multiplexer-advice)) () ()
  (setf (variable-value 'frame-multiplexer :global)
        (not (variable-value 'frame-multiplexer :global))))

(define-command (frame-multiplexer-normalize-ids (:advice-classes frame-multiplexer-advice))
    ()
    ()
  "Normalize the IDs of all the frames.
Assigns a smaller ID to a frame, if there is a smaller unused ID.
This does not change the order of the frames."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*)))
    (flet ((next-free (start)
             (loop :for x :upfrom start :below (length (virtual-frame-id/frame-table vf))
                   :when (null (aref (virtual-frame-id/frame-table vf) x))
                   :return x)))
      (loop :with free-index := (next-free 0)
            :for index :upfrom (1+ free-index) :below (length (virtual-frame-id/frame-table vf))
            :while free-index
            :when (aref (virtual-frame-id/frame-table vf) index)
            :do (setf (aref (virtual-frame-id/frame-table vf) free-index)
                      (aref (virtual-frame-id/frame-table vf) index))
                (setf (aref (virtual-frame-id/frame-table vf) index) nil)
                (setq free-index (next-free (1+ free-index)))))))

(define-command (frame-multiplexer-create-with-new-buffer-list
                 (:advice-classes frame-multiplexer-advice))
    ()
    ()
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (id (find-unused-frame-id vf)))
    (when (null id)
      (editor-error "it's full of frames in virtual frame"))
    (let ((frame (make-frame (current-frame))))
      (allocate-frame vf frame)
      (switch-current-frame vf frame))))

(define-command (frame-multiplexer-create-with-previous-buffer
                 (:advice-classes frame-multiplexer-advice))
    ()
    ()
  "Create a new frame with the previously opened buffer."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (id (find-unused-frame-id vf)))
    (when (null id)
      (editor-error "It's full of frames in virtual frame"))
    (let* ((prev-buffer (current-buffer))
           (new-frame (make-frame (current-frame))))
      (allocate-frame vf new-frame)
      (switch-current-frame vf new-frame)
      (when prev-buffer
        (switch-to-buffer prev-buffer)))))

(define-command (frame-multiplexer-delete (:advice-classes frame-multiplexer-advice))
    (&optional id) (:universal-nil)
  "Delete the current frame.
With prefix argument ID, delete the frame with the given ID."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (num (num-frames vf)))
    (when (= num 1)
      (editor-error "Can not delete the last virtual frame"))
    (when (and id (null (aref (virtual-frame-id/frame-table vf) id)))
      (editor-error "No frame with ID ~a" id))
    (let ((frame-now  (if id
                          (frame-table-entry-frame (aref (virtual-frame-id/frame-table vf) id))
                          (virtual-frame-current vf))))
      (when (eq frame-now (virtual-frame-current vf))
        (switch-current-frame vf (search-previous-frame vf frame-now)))
      (free-frame vf frame-now))))

(define-command (frame-multiplexer-prev (:advice-classes frame-multiplexer-advice))
    (&optional (n 1)) (:universal)
  "Switch to the Nth previous frame.
The prefix argument N defaults to 1."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (virtual-frame-current vf)))
    (loop :repeat n
          :while frame
          :do (setq frame (search-previous-frame vf frame)))
    (when frame
      (switch-current-frame vf frame))))

(define-command (frame-multiplexer-next (:advice-classes frame-multiplexer-advice))
    (&optional (n 1)) (:universal)
  "Switch to the Nth next frame.
The prefix argument N defaults to 1."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (frame (virtual-frame-current vf)))
    (loop :repeat n
          :while frame
          :do (setq frame (search-next-frame vf frame)))
    (when frame
      (switch-current-frame vf frame))))

(define-command (frame-multiplexer-switch (:advice-classes frame-multiplexer-advice))
    (&optional (id 1)) (:universal)
  "Switch to the frame with ID.
The prefix argument ID defaults to 1."
  ;; TODO: It would be great to enhance this by showing a prompt
  ;; and asking for the frame name (or buffer of the frame, if it has no name)
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (entry (aref (virtual-frame-id/frame-table vf) id))
         (same-frame-p (eq (current-frame) (frame-table-entry-frame entry))))
    (unless same-frame-p
      (if entry
          (switch-current-frame vf (frame-table-entry-frame entry))
          (editor-error "No frame with ID ~a" id)))))

(macrolet ((def (command-name n)
             `(define-command (,command-name (:advice-classes frame-multiplexer-advice))
                  () ()
                (frame-multiplexer-switch ,n))))
  (def frame-multiplexer-switch-0 0)
  (def frame-multiplexer-switch-1 1)
  (def frame-multiplexer-switch-2 2)
  (def frame-multiplexer-switch-3 3)
  (def frame-multiplexer-switch-4 4)
  (def frame-multiplexer-switch-5 5)
  (def frame-multiplexer-switch-6 6)
  (def frame-multiplexer-switch-7 7)
  (def frame-multiplexer-switch-8 8)
  (def frame-multiplexer-switch-9 9))

(define-command (frame-multiplexer-recent (:advice-classes frame-multiplexer-advice))
    (&optional (n 1)) (:universal)
  "Switch to the Nth most recent frame selected.
The prefix argument N defaults to 1."
  (check-frame-multiplexer-usable)
  (unless (or (ring-empty-p *recent-list*)
              (>= 0 n))
    (let* ((vf (gethash (implementation) *virtual-frame-map*))
           ;; Loop over the ring and skip IDs which are no longer valid.
           (recent-frame-id (loop :with valid-count := 0
                                  :for ref-n :upfrom 0
                                  :when (>= ref-n (ring-length *recent-list*))
                                    :return nil
                                  :when (aref (virtual-frame-id/frame-table vf) (ring-ref *recent-list* ref-n))
                                    :do (incf valid-count)
                                  :when (= valid-count n)
                                    :return (ring-ref *recent-list* ref-n))))
      (if (null recent-frame-id)
          (editor-error "No more recent frames")
          (let ((entry (aref (virtual-frame-id/frame-table vf) recent-frame-id)))
            (switch-current-frame vf (frame-table-entry-frame entry)))))))

(define-command (frame-multiplexer-rename (:advice-classes frame-multiplexer-advice))
    (name &optional id) ((:string "New name: ") :universal-nil)
  "Rename the current frame to NAME.
With prefix argument ID, rename the frame with the given ID."
  (check-frame-multiplexer-usable)
  (let* ((vf (gethash (implementation) *virtual-frame-map*))
         (entry (if (null id)
                    (find-frame-table-entry vf (virtual-frame-current vf))
                    (aref (virtual-frame-id/frame-table vf) id))))
    (if entry
        (setf (frame-table-entry-name entry) name)
        (editor-error "No frame with ID ~a" id))))

(defun enable-frame-multiplexer ()
  (setf (variable-value 'frame-multiplexer :global) t)
  (setq *recent-list* (make-ring 100)))

(defun disable-frame-multiplexer ()
  (setf (variable-value 'frame-multiplexer :global) nil)
  (setq *recent-list* nil))

(add-hook *after-init-hook* 'enable-frame-multiplexer)
(add-hook *exit-editor-hook* 'disable-frame-multiplexer)

;; devel:
#+(or)
(define-command frame-multiplexer-test () ()
  (labels ((vf ()
             (maphash (lambda (k v)
                        (declare (ignore k))
                        (return-from vf v))
                      *virtual-frame-map*))
           (check-nth-frame (expected-id)
             (let* ((virtual-frame (vf))
                    (actual-id (find-frame-id virtual-frame (virtual-frame-current virtual-frame))))
               (assert (= expected-id actual-id))))
           (check-tabs (&rest nums)
             (let ((virtual-frame (vf)))
               (assert (equal nums (mapcar (lambda (frame)
                                             (find-frame-id virtual-frame frame))
                                           (virtual-frame-frames virtual-frame)))))))
    (when (enabled-frame-multiplexer-p)
      (editor-error "frame-multiplexer-mode is already enabled"))
    ;; frame-multiplexer-create-with-new-buffer-list
    (toggle-frame-multiplexer)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 1)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 2)
    (frame-multiplexer-create-with-new-buffer-list)
    (check-nth-frame 3)
    (frame-multiplexer-next)
    ;; frame-multiplexer-next, frame-multiplexer-prev
    (frame-multiplexer-next)
    (check-nth-frame 1)
    (frame-multiplexer-prev)
    (check-nth-frame 0)
    (frame-multiplexer-prev)
    (check-nth-frame 3)
    (frame-multiplexer-next)
    (check-nth-frame 0)
    (check-tabs 0 1 2 3)
    ;; frame-multiplexer-delete
    (frame-multiplexer-delete)
    (check-tabs 1 2 3)
    (check-nth-frame 3)
    (frame-multiplexer-prev)
    (check-nth-frame 2)
    (frame-multiplexer-delete)
    (check-tabs 1 3)
    (check-nth-frame 1)
    ))
