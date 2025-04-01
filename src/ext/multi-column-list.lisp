(defpackage :lem/multi-column-list
  (:use :cl :lem)
  (:export :multi-column-list
           :multi-column-list-item
           :multi-column-list-of-window
           :select-item
           :delete-item
           :map-columns
           :display
           :update
           :quit
           :collect-checked-items
           :delete-checked-items))
(in-package :lem/multi-column-list)

(defvar *multi-column-list-mode-keymap*
  (make-keymap :undef-hook 'multi-column-list/default))

(define-minor-mode multi-column-list-mode
    (:name "multi-column-list"
     :keymap *multi-column-list-mode-keymap*))

(define-key *multi-column-list-mode-keymap* 'keyboard-quit 'multi-column-list/quit)
(define-key *multi-column-list-mode-keymap* 'escape 'multi-column-list/quit)
(define-key *multi-column-list-mode-keymap* 'next-line 'multi-column-list/down)
(define-key *multi-column-list-mode-keymap* 'previous-line 'multi-column-list/up)
(define-key *multi-column-list-mode-keymap* 'move-to-end-of-buffer 'multi-column-list/last)
(define-key *multi-column-list-mode-keymap* 'move-to-beginning-of-buffer 'multi-column-list/first)
(define-key *multi-column-list-mode-keymap* "Return" 'multi-column-list/select)
(define-key *multi-column-list-mode-keymap* "Space" 'multi-column-list/check-and-down)
(define-key *multi-column-list-mode-keymap* "M-Space" 'multi-column-list/up-and-check)
(define-key *multi-column-list-mode-keymap* "C-k" 'multi-column-list/delete-items)
(define-key *multi-column-list-mode-keymap* 'show-context-menu 'show-context-menu)
(define-key *multi-column-list-mode-keymap* 'delete-previous-char 'multi-column-list/delete-previous-char)

(define-command multi-column-list/default () ()
  (alexandria:when-let ((c (insertion-key-p (last-read-key-sequence))))
    (setf (multi-column-list-search-string (current-multi-column-list))
          (concatenate 'string
                       (multi-column-list-search-string (current-multi-column-list))
                       (string c)))
    (update (current-multi-column-list))))

(define-command multi-column-list/delete-previous-char () ()
  (let ((length (length (multi-column-list-search-string (current-multi-column-list)))))
    (when (< 0 length)
      (setf (multi-column-list-search-string (current-multi-column-list))
            (subseq (multi-column-list-search-string (current-multi-column-list))
                    0
                    (1- length)))))
  (update (current-multi-column-list)))

(define-command multi-column-list/quit () ()
  (quit (current-multi-column-list)))

(define-command multi-column-list/down () ()
  (popup-menu-down (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/up () ()
  (popup-menu-up (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/first () ()
  (popup-menu-first (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/last () ()
  (popup-menu-last (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/select () ()
  (popup-menu-select (multi-column-list-popup-menu (current-multi-column-list))))

(define-command multi-column-list/check-and-down () ()
  (check-current-item (current-multi-column-list))
  (multi-column-list/down))

(define-command multi-column-list/up-and-check () ()
  (multi-column-list/up)
  (check-current-item (current-multi-column-list)))

(define-command multi-column-list/delete-items () ()
  (delete-checked-items (current-multi-column-list)))

;;
(defgeneric select-item (component item))
(defgeneric delete-item (component item))
(defgeneric map-columns (component item))

(defclass multi-column-list-item ()
  ((checked :initform nil
            :accessor multi-column-list-item-checked-p)))

(defclass default-multi-column-list-item (multi-column-list-item)
  ((value :initarg :value
          :reader default-multi-column-list-item-value)))

(defun wrap (value)
  (if (typep value 'multi-column-list-item)
      value
      (make-instance 'default-multi-column-list-item :value value)))

(defun unwrap (value)
  (if (typep value 'default-multi-column-list-item)
      (default-multi-column-list-item-value value)
      value))

(defmethod select-item :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defmethod delete-item :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defmethod map-columns :around (component (item default-multi-column-list-item))
  (call-next-method component (unwrap item)))

(defclass multi-column-list ()
  ((columns :initarg :columns
            :initform nil
            :reader multi-column-list-columns)
   (items :initarg :items
          :accessor multi-column-list-items)
   (filter-function :initarg :filter-function
                    :initform nil
                    :reader multi-column-list-filter-function)
   (select-callback :initarg :select-callback
                    :initform nil
                    :reader multi-column-list-select-callback)
   (delete-callback :initarg :delete-callback
                    :initform nil
                    :accessor multi-column-list-delete-callback)
   (column-function :initarg :column-function
                    :initform nil
                    :accessor multi-column-list-column-function)
   (use-check :initform nil
              :initarg :use-check
              :reader multi-column-list-use-check-p)
   (context-menu :initform '()
                 :initarg :context-menu
                 :reader multi-column-list-context-menu)
   (print-spec :accessor multi-column-list-print-spec)
   (popup-menu :accessor multi-column-list-popup-menu)
   (search-string :initform ""
                  :accessor multi-column-list-search-string)))

(defmethod initialize-instance ((instance multi-column-list) &rest initargs &key items &allow-other-keys)
  (apply #'call-next-method
         instance
         :items (mapcar #'wrap items)
         initargs))

(defmethod map-columns :around ((component multi-column-list) item)
  (append (if (multi-column-list-use-check-p component)
              (list (if (multi-column-list-item-checked-p item)
                        "x "
                        "  "))
              nil)
          (mapcar #'princ-to-string (call-next-method))))

(defmethod select-item ((component multi-column-list) item)
  (when (multi-column-list-select-callback component)
    (funcall (multi-column-list-select-callback component) component item)))

(defmethod delete-item ((component multi-column-list) item)
  (when (multi-column-list-delete-callback component)
    (funcall (multi-column-list-delete-callback component) component item)))

(defmethod map-columns ((component multi-column-list) item)
  (if (multi-column-list-column-function component)
      (funcall (multi-column-list-column-function component) component item)
      (list (princ-to-string item))))

(defmethod multi-column-list-columns :around ((multi-column-list multi-column-list))
  (append (if (multi-column-list-use-check-p multi-column-list)
              (list "")
              nil)
          (call-next-method)))

(defclass print-spec ()
  ((multi-column-list :initarg :multi-column-list
                      :reader print-spec-multi-column-list)
   (column-width-list :initarg :column-width-list
                      :reader print-spec-column-width-list)))

(defmethod lem/popup-menu:write-header ((print-spec print-spec) point)
  (let* ((multi-column-list (print-spec-multi-column-list print-spec))
         (search-string (multi-column-list-search-string multi-column-list))
         (columns (multi-column-list-columns multi-column-list)))
    (cond ((< 0 (length search-string))
           (with-point ((start point))
             (insert-string point " ")
             (insert-string point search-string)
             (move-to-column point
                             (1+ (loop :for width :in (print-spec-column-width-list print-spec)
                                       :sum (1+ width)))
                             t)
             (put-text-property start point :attribute (make-attribute :underline t))))
          (columns
           (with-point ((start point))
             (loop :for width :in (print-spec-column-width-list print-spec)
                   :for column-header :in columns
                   :do (insert-string point " ")
                       (let ((column (point-column point)))
                         (insert-string point column-header)
                         (move-to-column point (+ column width) t)))
             (insert-string point " ")
             (put-text-property start point :attribute (make-attribute :underline t)))))))

(defmethod lem/popup-menu:apply-print-spec ((print-spec print-spec) point item)
  (check-type item multi-column-list-item)
  (with-point ((start point))
    (loop :for value :in (map-columns (print-spec-multi-column-list print-spec) item)
          :for width :in (print-spec-column-width-list print-spec)
          :do (insert-string point " ")
              (let ((column (point-column point)))
                (insert-string point value)
                (move-to-column point (+ column width) t)))
    (insert-string point " ")
    (put-text-property start
                       point
                       :click-callback (lambda (window point)
                                         (click-menu-item
                                          (print-spec-multi-column-list print-spec)
                                          window
                                          point)))
    (put-text-property start
                       point
                       :hover-callback (lambda (window point)
                                         (declare (ignore window))
                                         (hover-menu-item
                                          (print-spec-multi-column-list print-spec)
                                          point)))))

(defun compute-column-width-list (multi-column-list)
  (let ((width-matrix
          (loop :for row :in
                   (append (alexandria:when-let
                               (columns (multi-column-list-columns multi-column-list))
                             (list columns))
                           (mapcar (lambda (item) (map-columns multi-column-list item))
                                   (multi-column-list-items multi-column-list)))
                :collect (loop :for value :in row
                               :collect (string-width value)))))
    (loop :repeat (length (first width-matrix))
          :for i :from 0
          :collect (loop :for width-list :in width-matrix :maximize (elt width-list i)))))

(defun multi-column-list-of-window (window)
  (window-parameter window 'multi-column-list))

(defun (setf multi-column-list-of-window) (value window)
  (setf (window-parameter window 'multi-column-list) value))

(defun current-multi-column-list ()
  (multi-column-list-of-window (current-window)))

(defun multi-column-list-window (multi-column-list)
  (lem/popup-menu::popup-menu-window
   (multi-column-list-popup-menu multi-column-list)))

(defun max-display-items ()
  (- (display-height) 4))

(defmethod display ((component multi-column-list) &key (style '(:gravity :center)) (index 0))
  (let ((print-spec (make-instance
                     'print-spec
                     :multi-column-list component
                     :column-width-list (compute-column-width-list component))))
    (setf (multi-column-list-print-spec component) print-spec)
    (let* ((popup-menu
             (display-popup-menu (multi-column-list-items component)
                                 :print-spec print-spec
                                 :action-callback (lambda (item)
                                                    (select-item component item))
                                 :style style
                                 :max-display-items (max-display-items)))
           (window (lem/popup-menu::popup-menu-window popup-menu)))
      (add-hook (window-leave-hook window)
                (lambda (old-window)
                  (declare (ignore old-window))
                  (multi-column-list/quit)))
      (setf (multi-column-list-popup-menu component) popup-menu)
      (setf (current-window) window)
      (setf (lem-core::buffer-context-menu (window-buffer (current-window)))
            (multi-column-list-context-menu component))
      (setf (multi-column-list-of-window (current-window)) component)
      (multi-column-list-mode t)
      (popup-menu-first popup-menu)
      (loop :repeat index :do (popup-menu-down popup-menu))
      component)))

(defmethod quit ((component multi-column-list))
  (let* ((popup-menu (multi-column-list-popup-menu component))
         (popup-window (lem/popup-menu::popup-menu-window popup-menu)))
    (when (eq (current-window) popup-window)
      (setf (current-window) (window-parent popup-window)))
    (popup-menu-quit popup-menu)))

(defun filtered-items (multi-column-list)
  (let ((filtered-elements
          (funcall (multi-column-list-filter-function multi-column-list)
                   (multi-column-list-search-string multi-column-list))))
    (let ((items (loop :for element :in filtered-elements
                       :collect (let ((item (find element
                                                  (multi-column-list-items multi-column-list)
                                                  :key #'default-multi-column-list-item-value)))
                                  (or item (wrap element))))))
      (dolist (filtered-item items)
        (let ((item (find (default-multi-column-list-item-value filtered-item)
                          (multi-column-list-items multi-column-list)
                          :key #'default-multi-column-list-item-value)))
          (when item
            (setf (multi-column-list-item-checked-p filtered-item)
                  (multi-column-list-item-checked-p item)))))
      items)))

(defmethod update ((component multi-column-list))
  (let ((items (multi-column-list-items component)))
    (when (and (multi-column-list-filter-function component)
               (< 0 (length (multi-column-list-search-string component))))
      (setf items (filtered-items component)))
    (popup-menu-update (multi-column-list-popup-menu component)
                       items
                       :print-spec (multi-column-list-print-spec component)
                       :max-display-items (max-display-items)
                       :keep-focus t)))

(defun hover-menu-item (multi-column-list point)
  (lem/popup-menu::move-focus (multi-column-list-popup-menu multi-column-list)
                              (lambda (focus-point)
                                (move-point focus-point point))))

(defun click-menu-item (multi-column-list window point)
  (move-point (buffer-point (window-buffer window)) point)
  (popup-menu-select (multi-column-list-popup-menu multi-column-list)))

(defun current-focus-item (multi-column-list)
  (lem/popup-menu:get-focus-item
   (multi-column-list-popup-menu multi-column-list)))

(defun check-current-item (multi-column-list)
  (when (multi-column-list-use-check-p multi-column-list)
    (let ((item (current-focus-item multi-column-list)))
      (setf (multi-column-list-item-checked-p item)
            (not (multi-column-list-item-checked-p item))))
    (update multi-column-list)))

(defun checked-items (multi-column-list)
  (or (remove-if-not #'multi-column-list-item-checked-p
                     (multi-column-list-items multi-column-list))
      (list (current-focus-item multi-column-list))))

(defun collect-checked-items (multi-column-list)
  (mapcar #'unwrap (checked-items multi-column-list)))

(defun delete-checked-items (multi-column-list)
  (let ((whole-items (multi-column-list-items multi-column-list)))
    (dolist (item (checked-items multi-column-list))
      (delete-item multi-column-list item)
      (setf whole-items
            (delete item whole-items)))
    (setf (multi-column-list-items multi-column-list) whole-items)
    (update multi-column-list)))
