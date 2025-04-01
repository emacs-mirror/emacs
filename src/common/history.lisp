(defpackage :lem/common/history
  (:use :cl)
  (:export :make-history
           :history-data-list
           :save-file
           :last-history
           :add-history
           :remove-history
           :previous-history
           :next-history
           :previous-matching
           :next-matching
           :backup-edit-string
           :restore-edit-string)
  #+sbcl
  (:lock t))
(in-package :lem/common/history)

(defstruct (history (:constructor %make-history))
  pathname
  data
  index
  edit-string
  limit)

(defun history-data-list (history)
  "Return the history data as a list (and not a vector)."
  (coerce (history-data history) 'list))

(defun require-additions-to-history-p (input last-input)
  "Return t if the current input is valid and different than the previous input."
  (and (not (equal input last-input))
       (not (equal input ""))))

(defun make-history (&key pathname limit)
  (let* ((initial-contents
           (when (and pathname (uiop:file-exists-p pathname))
             (uiop:read-file-form pathname)))
         (num-contents (length initial-contents)))
    (%make-history
     :pathname pathname
     :data (make-array num-contents :fill-pointer num-contents :adjustable t :initial-contents initial-contents)
     :index num-contents
     :limit limit)))

(defun save-file (history)
  (when (history-pathname history)
    (ensure-directories-exist (history-pathname history))
    (with-open-file (stream (history-pathname history)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (print (coerce (history-data history) 'list) stream))))

(defun last-history (history)
  (when (< 0 (length (history-data history)))
    (aref (history-data history)
          (1- (length (history-data history))))))

(defun add-history (history input &key (allow-duplicates t) (test #'equal) move-to-top)
  "Add this INPUT to the HISTORY.

  Doesn't add the same INPUT as the previous one.
  If ALLOW-DUPLICATES is non T, don't add duplicates at all.
  If LIMIT is set, overwrite oldest entry when reached.
  If MOVE-TO-TOP is T, move the entry to the top of the stack if it already exists."
  (let ((existing-position (position input (history-data history) :test test)))
    (cond
      ((and existing-position move-to-top)
       (let ((item (aref (history-data history) existing-position)))
         ;; Remove the item from its current position
         (replace (history-data history)
                  (history-data history)
                  :start1 existing-position
                  :start2 (1+ existing-position))
         (decf (fill-pointer (history-data history)))
         ;; Add the item to the top
         (vector-push-extend item (history-data history))))
      ((and (not existing-position)
            (require-additions-to-history-p input (last-history history)))
       (let ((limit (history-limit history)))
         (cond
           ((and limit (>= (length (history-data history)) limit))
            ;; Shift by 1, overwriting the oldest
            (replace (history-data history)
                     (history-data history)
                     :start1 0 
                     :start2 1
                     :end2 limit)
            (setf (aref (history-data history) (1- limit)) input))
           (t
            ;; Add new element normally
            (vector-push-extend input (history-data history))))))
      (allow-duplicates
       ;; If duplicates are allowed, add the input
       (vector-push-extend input (history-data history))))
    (setf (history-index history)
          (length (history-data history))))
  input)

(defun remove-history (history input)
  (let* ((new (remove input (history-data history) :test #'equal))
         (len (length new))
         (array (make-array len :fill-pointer len :adjustable t :initial-contents new)))
    (setf (history-data history) array)
    (setf (history-index history) len)))

(defun previous-history (history)
  (when (< 0 (history-index history))
    (values (aref (history-data history)
                  (decf (history-index history)))
            t)))

(defun next-history (history)
  (when (< (history-index history)
           (1- (length (history-data history))))
    (values (aref (history-data history)
                  (incf (history-index history)))
            t)))

(defun matchp (sub-string whole-string)
  ;; fuzzy matcher
  #+(or)
  (loop :with start := 0 :and matches := '()
        :for c :across sub-string
        :do (let ((pos (position c whole-string :start start)))
              (unless pos (return))
              (push pos matches)
              (setf start (1+ pos)))
        :finally (return (nreverse matches)))
  (let ((pos (search sub-string whole-string)))
    (when pos
      (loop :for offset :from 0
            :repeat (length sub-string)
            :collect (+ pos offset)))))

(defun previous-matching (history string &key (start-index (1- (history-index history))))
  (loop :for i :downfrom start-index :to 0
        :for matches := (matchp string (aref (history-data history) i))
        :do (when matches
              (return (values (aref (history-data history) i)
                              i
                              matches)))))

(defun next-matching (history string &key (start-index (1+ (history-index history))))
  (loop :for i :from start-index :below (length (history-data history))
        :for matches := (matchp string (aref (history-data history) i))
        :do (when matches
              (return (values (aref (history-data history) i)
                              i
                              matches)))))

(defun previous-matching-exclude-duplicates (history string last-matched-string
                                             &key (start-index (1- (history-index history))))
  (loop :with index := start-index
        :do (multiple-value-bind (matched-string matched-index matches)
                (previous-matching history string :start-index index)
              (cond ((null matched-string)
                     (return))
                    ((string/= matched-string last-matched-string)
                     (return (values matched-string matched-index matches)))
                    (t
                     (setf index (1- matched-index)))))))

(defun next-matching-exclude-duplicates (history string last-matched-string
                                         &key (start-index (1- (history-index history))))
  (loop :with index := start-index
        :do (multiple-value-bind (matched-string matched-index matches)
                (next-matching history string :start-index index)
              (cond ((null matched-string)
                     (return))
                    ((string/= matched-string last-matched-string)
                     (return (values matched-string matched-index matches)))
                    (t
                     (setf index (1+ matched-index)))))))

(defun backup-edit-string (history input)
  (when (or (>= (history-index history)
                (length (history-data history)))
            (not (equal input
                        (aref (history-data history)
                              (history-index history)))))
    (setf (history-edit-string history) input)
    (setf (history-index history) (length (history-data history)))))

(defun restore-edit-string (history)
  (setf (history-index history) (length (history-data history)))
  (values (history-edit-string history)
          t))
