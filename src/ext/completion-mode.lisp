(defpackage :lem/completion-mode
  (:use :cl :lem)
  (:export :make-completion-spec
           :make-completion-item
           :completion-item
           :completion-item-label
           :completion-item-detail
           :run-completion
           :completion-end
           :completion-mode)
  #+sbcl
  (:lock t))
(in-package :lem/completion-mode)

(defparameter *limit-number-of-items* 100)

(defvar *completion-context* nil)
(defvar *completion-reverse* nil)

(defclass completion-context ()
  ((spec
    :initarg :spec
    :reader context-spec
    :type completion-spec)
   (last-items
    :initform '()
    :accessor context-last-items)
   (popup-menu
    :initform nil
    :accessor context-popup-menu)))

(defclass completion-spec ()
  ((function
    :initarg :function
    :reader spec-function)
   (async
    :initarg :async
    :initform nil
    :reader spec-async-p)))

(defun make-completion-spec (function &key async)
  (make-instance 'completion-spec :function function :async async))

(defun ensure-completion-spec (completion-spec)
  (typecase completion-spec
    (completion-spec
     completion-spec)
    (otherwise
     (make-completion-spec (alexandria:ensure-function completion-spec)))))

(defun call-sync-function (completion-spec point)
  (assert (not (spec-async-p completion-spec)))
  (funcall (spec-function completion-spec) point))

(defun call-async-function (completion-spec point then)
  (assert (spec-async-p completion-spec))
  (funcall (spec-function completion-spec) point then))

(defclass completion-item ()
  ((label
    :initarg :label
    :initform ""
    :reader completion-item-label
    :type string)
   (chunks
    :initarg :chunks
    :initform nil
    :reader completion-item-chunks
    :type list)
   (detail
    :initarg :detail
    :initform ""
    :reader completion-item-detail
    :type string)
   (start
    :initarg :start
    :initform nil
    :reader completion-item-start
    :type (or null point))
   (end
    :initarg :end
    :initform nil
    :reader completion-item-end
    :type (or null point))
   (focus-action
    :initarg :focus-action
    :initform nil
    :reader completion-item-focus-action
    :type (or null function))))

(defun make-completion-item (&rest initargs
                             &key label chunks detail start end focus-action)
  (declare (ignore label chunks detail start end focus-action))
  (apply #'make-instance 'completion-item initargs))

(defvar *completion-mode-keymap* (make-keymap :name '*completion-mode-keymap*
                                              :undef-hook 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* 'next-line 'completion-next-line)
(define-key *completion-mode-keymap* "M-n"    'completion-next-line)
(define-key *completion-mode-keymap* "Tab"    'completion-narrowing-down-or-next-line)
(define-key *completion-mode-keymap* "Shift-Tab"    'completion-previous-line)
(define-key *completion-mode-keymap* 'previous-line 'completion-previous-line)
(define-key *completion-mode-keymap* "M-p"    'completion-previous-line)
(define-key *completion-mode-keymap* 'move-to-end-of-buffer 'completion-end-of-buffer)
(define-key *completion-mode-keymap* 'move-to-beginning-of-buffer 'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "Return"    'completion-select)
(define-key *completion-mode-keymap* "Space"    'completion-insert-space-and-cancel)
(define-key *completion-mode-keymap* 'delete-previous-char 'completion-delete-previous-char)
(define-key *completion-mode-keymap* 'backward-delete-word 'completion-backward-delete-word)

(define-attribute detail-attribute
  (t :foreground :base03))

(define-attribute chunk-attribute
  (t :foreground :base0D))

(defclass print-spec ()
  ((label-width
    :initarg :label-width
    :reader label-width)))

(defun compute-label-width (items)
  (loop :for item :in items
        :maximize (1+ (length (completion-item-label item)))))

(defun make-print-spec (items)
  (make-instance 'print-spec
                 :label-width
                 (compute-label-width items)))

(defmethod lem/popup-menu:apply-print-spec ((print-spec print-spec) point item)
  (with-point ((start point))
    (insert-string point " ")
    (insert-string point (completion-item-label item))
    (loop :for (offset-start . offset-end) :in (completion-item-chunks item)
          :do (with-point ((start point) (end point))
                (character-offset (line-start start) (1+ offset-start))
                (character-offset (line-start end) (1+ offset-end))
                (put-text-property start end :attribute 'chunk-attribute)))
    (move-to-column point (label-width print-spec) t)
    (line-end point)
    (insert-string point "  ")
    (unless (alexandria:emptyp (completion-item-detail item))
      (insert-string point (completion-item-detail item)
                     :attribute 'detail-attribute)
      (insert-string point " "))
    (put-text-property start
                       point
                       :click-callback (lambda (window dest-point)
                                         (declare (ignore window dest-point))
                                         (completion-select)))
    (let ((context *completion-context*))
      (put-text-property start
                         point
                         :hover-callback (lambda (window dest-point)
                                           (declare (ignore window))
                                           (lem/popup-menu::move-focus
                                            (context-popup-menu context)
                                            (lambda (point)
                                              (move-point point dest-point))))))))

(defun completion-end ()
  (when *completion-context*
    (completion-mode nil)
    (alexandria:when-let (popup-menu (context-popup-menu *completion-context*))
      (popup-menu-quit popup-menu))
    (setf (context-popup-menu *completion-context*) nil)
    (setf *completion-context* nil)))

(defun call-focus-action ()
  (when *completion-context*
    (alexandria:when-let* ((menu (context-popup-menu *completion-context*))
                           (item (lem/popup-menu:get-focus-item menu))
                           (fn (completion-item-focus-action item)))
      (funcall fn *completion-context*))))

(define-command completion-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c)
             (continue-completion *completion-context*))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(define-command completion-delete-previous-char (n) (:universal)
  (delete-previous-char n)
  (continue-completion *completion-context*))

(define-command completion-backward-delete-word (n) (:universal)
  (backward-delete-word n)
  (continue-completion *completion-context*))

(define-command completion-next-line () ()
  (popup-menu-down (context-popup-menu *completion-context*))
  (call-focus-action))

(define-command completion-previous-line () ()
  (popup-menu-up (context-popup-menu *completion-context*))
  (call-focus-action))

(define-command completion-end-of-buffer () ()
  (popup-menu-last (context-popup-menu *completion-context*))
  (call-focus-action))

(define-command completion-beginning-of-buffer () ()
  (popup-menu-first (context-popup-menu *completion-context*))
  (call-focus-action))

(define-command completion-select () ()
  (popup-menu-select (context-popup-menu *completion-context*)))

(define-command completion-insert-space-and-cancel () ()
  (insert-character (current-point) #\space)
  (completion-end))

(defun completion-item-range (point item)
  (let ((start (or (completion-item-start item)
                   (with-point ((start point))
                     (skip-chars-backward start #'syntax-symbol-char-p)
                     start)))
        (end (or (completion-item-end item)
                 point)))
    (values start end)))

(defun completion-insert (point item &optional begin)
  (when item
    (multiple-value-bind (start end) (completion-item-range point item)
      (delete-between-points start end)
      (insert-string point (subseq (completion-item-label item) 0 begin)))))

(defun partial-match (strings)
  (when strings
    (let ((n nil))
      (loop :for rest :on strings
            :do (loop :for rest2 :on (cdr rest)
                      :for mismatch := (mismatch (first rest) (first rest2))
                      :do (and mismatch
                               (setf n
                                     (if n
                                         (min n mismatch)
                                         mismatch)))))
      n)))

(defun narrowing-down (context last-items)
  (when last-items
    (let ((n (partial-match (mapcar #'completion-item-label last-items))))
      (multiple-value-bind (start end)
          (completion-item-range (current-point) (first last-items))
        (cond ((and n (plusp n) (< (count-characters start end) n))
               (completion-insert (current-point)
                                  (first last-items)
                                  n)
               (continue-completion context)
               t)
              ((alexandria:length= last-items 1)
               (completion-insert (current-point)
                                  (first last-items))
               (continue-completion context)
               t)
              (t
               nil))))))

(define-command completion-narrowing-down-or-next-line () ()
  (or (narrowing-down *completion-context* (context-last-items *completion-context*))
      (if *completion-reverse*
          (completion-previous-line)
          (completion-next-line))))

(defun limitation-items (items)
  (let ((result (if (and *limit-number-of-items*
                         (< *limit-number-of-items* (length items)))
                    (subseq items 0 *limit-number-of-items*)
                    items)))
    (if *completion-reverse*
        (reverse result)
        result)))

(defun compute-completion-items (context then)
  (flet ((update-items-and-then (items)
           (let ((items (limitation-items items)))
             (setf (context-last-items context) items)
             (funcall then items))))
    (let ((spec (context-spec context)))
      (if (spec-async-p spec)
          (call-async-function spec
                               (current-point)
                               #'update-items-and-then)
          (update-items-and-then (call-sync-function spec (current-point)))))))

(defun start-completion (context items style)
  (when items
    (setf (context-popup-menu *completion-context*)
          (apply #'display-popup-menu
                 items
                 :action-callback (lambda (item)
                                    (completion-insert (current-point) item)
                                    (completion-end))
                 :print-spec (make-print-spec items)
                 (when style `(:style ,style))))
    (completion-mode t)
    (unless (spec-async-p (context-spec context))
      (narrowing-down context items))
    (when *completion-reverse*
      (completion-end-of-buffer))
    (call-focus-action)))

(defun continue-completion (context)
  (compute-completion-items
   context
   (lambda (items)
     (cond ((null items)
            (completion-end))
           (t
            (when *completion-context*
              (popup-menu-update (context-popup-menu *completion-context*)
                                 items
                                 :print-spec (make-print-spec items))
              (call-focus-action))))))
  (when *completion-reverse*
    (ignore-errors (completion-end-of-buffer))))

(defun run-completion (completion-spec &key style)
  (let* ((spec (ensure-completion-spec completion-spec))
         (context (make-instance 'completion-context :spec spec)))
    (setf *completion-context* context)
    (with-point ((before-point (current-point)))
      (compute-completion-items
       context
       (if (spec-async-p (context-spec context))
           (lambda (items)
             (when (point= before-point (current-point))
               (start-completion context items style)))
           (lambda (items)
             (when items
               (if (alexandria:length= items 1)
                   (completion-insert (current-point) (first items))
                   (start-completion context items style)))))))))
