(defpackage :lem-vi-mode/jumplist
  (:use :cl
        :lem)
  (:import-from :lem
                :alive-point-p)
  (:import-from :alexandria
                :when-let
                :ignore-some-conditions)
  (:export :with-jumplist
           :without-jumplist
           :jump-back
           :jump-next
           :window-jumplist
           :current-jumplist
           :copy-jumplist))
(in-package :lem-vi-mode/jumplist)

(defvar *max-jumplist-size* 100)

(defstruct jumplist
  (history nil :type list)
  (index 0 :type (integer 0))
  (current nil :type (or null point)))

(defmethod print-object ((object jumplist) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (history index current)
        object
      (format stream ":HISTORY (~A items) :INDEX ~A"
              (+ (length history)
                 (if current 1 0))
              index))))

(defun delete-jumplist (jumplist)
  (check-type jumplist jumplist)
  (mapc #'delete-point (jumplist-history jumplist))
  (when (jumplist-current jumplist)
    (delete-point (jumplist-current jumplist)))
  (values))

(define-condition jumplist-invalid-index (error)
  ((jumplist :initarg :jumplist)
   (index :initarg :index))
  (:report (lambda (condition stream)
             (with-slots (jumplist index) condition
               (format stream "Invalid index ~A for ~A"
                       index
                       jumplist)))))

(defun jumplist-ref (jumplist index)
  (check-type index integer)
  (cond
    ((= index 0)
     (jumplist-current jumplist))
    ((< 0 index)
     (or (nth (1- index)
              (jumplist-history jumplist))
         (error 'jumplist-invalid-index
                :jumplist jumplist
                :index index)))
    (t
     (error 'jumplist-invalid-index
            :jumplist jumplist
            :index index))))

(defun jumplist-find-backward (jumplist from-index)
  (loop for i from from-index below *max-jumplist-size*
        for point = (jumplist-ref jumplist i)
        until (or (null point)
                  (alive-point-p point))
        finally (return (values point i))))

(defun jumplist-find-forward (jumplist from-index)
  (loop for i downfrom from-index
        for point = (jumplist-ref jumplist i)
        until (or (null point)
                  (alive-point-p point))
        finally (return (values point i))))

(defun delete-exceeded-elements (list count)
  (when-let ((last-cdr (nthcdr (- count 1) list)))
    (mapc #'delete-point (cdr last-cdr))
    (setf (cdr last-cdr) nil))
  list)

(defun jumplist-history-push (jumplist point)
  (with-slots (history index current) jumplist
    ;; The jumplist-history is a list structure, for performance consideration, we use jumplist-current to track the item pointed by the jumplist-index.
    ;; When jumplist-index is 0, the jumplist-current should be nil. (Means we are NOT navigating the jumplist-history.)
    ;; If jumplist-index > 0 (means we are now navigating the jumplist-history), or in other words, the jumplist-current is NOT nil.
    ;; We only delete jumplist-current when jumplist-index > 0, since we don't want to destroy the jumplist while we are navigating back (C-o) and forth (C-o) the items in jumplist-history.
    (when (< 0 index)
      (when current
        (delete-point current))
      (setf index 0
            current nil))

    ;; Set history of jumplist.
    (setf history
          (cons (copy-point point :left-inserting)
                ;; Remove the one existing equal point. (Only testing the same line for equality, ignoring the column-number to avoid jumplist spam. )
                (remove point
                        history
                        :test (lambda (new-point point)
                                (when (and (eq (point-buffer new-point)
                                               (point-buffer point))
                                           (= (line-number-at-point new-point)
                                              (line-number-at-point point)))
                                  (delete-point point)
                                  t))
                        :count 1)))
    
    ;; Keep only *max-jumplist-size* points (including `current`)
    (delete-exceeded-elements history (1- *max-jumplist-size*))
    
    point))

(defun jumplist-history-back (jumplist)
  (with-slots (index current) jumplist
    ;; NOTE: In vim, the jump-back (C-o) command will save (current-point) as the first element of jumplist-history, if jumplist-current is nil (which means that we did n't use C-i before, and history-index = 0, and now we are ready to enter jumplist navigating mode.), so that it's possible to back to this location in the future.
    (let ((enter-jumplist-navigating-mode-p nil))
      (when (zerop index)
        (setf current (copy-point (current-point) :left-inserting))
        (setf enter-jumplist-navigating-mode-p t))
      
      (ignore-some-conditions (jumplist-invalid-index)
        (multiple-value-bind (point new-index)
          
            (if enter-jumplist-navigating-mode-p
                (progn
                  ;; Ensure (current-point) is the first element of jumplist-history.
                  (jumplist-history-push jumplist current)
                  ;; Because we have pushed a new element, so the original index is outdated.
                  (jumplist-find-backward jumplist (+ 2 index)))
                (progn
                  (jumplist-find-backward jumplist (1+ index))))
          
          ;; Update the value of jumplist-index, making the jumplist-stack-pointer points current.
          (setf index new-index)
          
          point)) 
      )))

(defun jumplist-history-next (jumplist)
  (with-slots (index current) jumplist
    (when (<= 0 (1- index))
      (ignore-some-conditions (jumplist-invalid-index)
        (multiple-value-bind (point new-index)
            (jumplist-find-forward jumplist (1- index))
          (setf index new-index)
          (when (zerop index)
            (delete-point current)
            (setf current nil))
          point)))))

(defun window-jumplist (window)
  (or (window-parameter window :vi-mode-jumplist)
      (let ((jumplist (make-jumplist)))
        (setf (window-parameter window :vi-mode-jumplist)
              jumplist)
        (add-hook (window-delete-hook window)
                  (lambda () (delete-jumplist jumplist)))
        jumplist)))

(defun (setf window-jumplist) (jumplist window)
  (check-type jumplist jumplist)
  (when-let ((current-jumplist (window-parameter window :vi-mode-jumplist)))
    (delete-jumplist current-jumplist))
  (setf (window-parameter window :vi-mode-jumplist)
        jumplist))

(defun current-jumplist ()
  (window-jumplist (current-window)))

(defun jumplist-table (jumplist)
  (flet ((buffer-identifier (buffer)
           (if (buffer-filename buffer)
               (enough-namestring (buffer-filename buffer))
               (buffer-name buffer))))
    (with-slots (index current) jumplist
      (loop with results = (if (and current
                                    (alive-point-p current))
                               ;; Initialize with empty list, means we are in jumplist navigating mode. (C-o is used at least once.)
                               (list)
                               ;; Initialize with a NIL entry, to mean we are not at jumplist navigating mode. (jumplist-current = nil and jumplist-index = 0)
                               (list (list (= 0 index)
                                           (abs (- 0 index))
                                           nil nil nil)))
            with dead-count = 0
            for point in (jumplist-history jumplist)
            for i from 1
            if (alive-point-p point)
            ;; current-p index line column file/buffer-name
            do (push (list
                      (= (- i dead-count) index)
                      (abs (- (- i dead-count) index))
                      (line-number-at-point point)
                      (point-column point)
                      (buffer-identifier (point-buffer point)))
                     results)
            else do (incf dead-count)
            finally (return results)))))

(defun print-jumplist (jumplist &optional (stream *standard-output*))
  (let ((table (jumplist-table jumplist)))
    (dolist (row table)
      (destructuring-bind (current-p index line column file)
          row
        (format stream "~:[  ~;> ~]~A: ~A~%"
                current-p
                index
                (and line column
                     (format nil "(~A, ~A)  ~A" line column file)))))))

(defvar *disable-jumplist* nil)

(defun call-with-jumplist (fn)
  (with-point ((p (current-point)))
    (let ((*disable-jumplist* t))
      (prog1 (funcall fn)
        (unless (and (eq (point-buffer p)
                         (current-buffer))
                     (= (line-number-at-point p)
                        (line-number-at-point (current-point))))
          (jumplist-history-push (current-jumplist) p))))))

(defmacro with-jumplist (&body body)
  `(if *disable-jumplist*
       (progn ,@body)
       (call-with-jumplist
        (lambda () ,@body))))

(defmacro without-jumplist (&body body)
  `(let ((*disable-jumplist* t))
     ,@body))

(defun jump-back ()
  (when-let ((point (jumplist-history-back (current-jumplist))))
    (switch-to-buffer (point-buffer point))
    (move-point (current-point) point)
    point))

(defun jump-next ()
  (when-let ((point (jumplist-history-next (current-jumplist))))
    (switch-to-buffer (point-buffer point))
    (move-point (current-point) point)))
