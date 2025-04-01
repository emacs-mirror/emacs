(defpackage :lem/abbrev
  (:use :cl :lem :lem/completion-mode)
  (:export :abbrev
           :abbrev-with-pop-up-window)
  #+sbcl
  (:lock t))
(in-package :lem/abbrev)

(defun preceding-word (point)
  (with-point ((cur point)
               (end point))
    (skip-symbol-backward cur)
    (points-to-string cur end)))

(defun scan-line-words (str)
  (let ((words))
    (do ((i 0 (1+ i)))
        ((>= i (length str)))
      (when (syntax-symbol-char-p (aref str i))
        (push (subseq str i
                      (do ((j i (1+ j)))
                          ((or (>= j (length str))
                               (not (syntax-symbol-char-p (aref str j))))
                           (setq i j)
                           j)))
              words)))
    (nreverse words)))

(defun scan-buffer-words (buffer)
  (with-open-stream (in (make-buffer-input-stream (buffer-start-point buffer)))
    (loop :for str := (read-line in nil)
          :while str
          :append (scan-line-words str))))

(defun collect-buffer-words-order-proximity (point)
  (with-point ((point point)
               (start-point point))
    (let ((words '()))
      (loop
        (setf words (append (scan-line-words (line-string point)) words))
        (unless (line-offset point -1)
          (buffer-end point))
        (when (same-line-p start-point point)
          (return)))
      (nreverse (remove-duplicates words :test #'equal)))))

(defun filter-word (current-word words)
  (loop :with output = nil
        :for word :in words
        :when (and (string/= word current-word)
                   (alexandria:starts-with-subseq current-word word))
        :do (pushnew word output :test #'string=)
        :finally (return output)))

(defun scan-all-buffer-words (word &optional point)
  (unless (string= word "")
    (filter-word word
                 (append (if point
                             (collect-buffer-words-order-proximity point)
                             (scan-buffer-words (current-buffer)))
                         (remove-duplicates
                          (mapcan (lambda (buffer)
                                    (unless (eq buffer (current-buffer))
                                      (scan-buffer-words buffer)))
                                  (buffer-list))
                          :test #'equal)))))

(define-key *global-keymap* "C-x /" 'abbrev-with-pop-up-window)
(define-command abbrev-with-pop-up-window () ()
  (run-completion (lambda (point)
                    (let* ((src-word (preceding-word point))
                           (words (scan-all-buffer-words src-word)))
                      (with-point ((start point)
                                   (end point))
                        (skip-chars-backward start #'syntax-symbol-char-p)
                        (mapcar (lambda (word)
                                  (make-completion-item :label word
                                                        :start start
                                                        :end end))
                                words))))))

(defvar *rest-words* nil)
(defvar *all-words* nil)
(defvar *start-charpos* nil)

(define-key *global-keymap* "M-/" 'abbrev)
(define-command abbrev () ()
  (let ((point (current-point)))
    (cond ((continue-flag :abbrev)
           (when (null *rest-words*)
             (setf *rest-words* *all-words*))
           (let ((n (- (point-charpos point) *start-charpos*)))
             (line-offset point 0 *start-charpos*)
             (delete-character point n))
           (insert-string point (first *rest-words*))
           (setf *rest-words* (rest *rest-words*)))
          (t
           (let* ((src-word (preceding-word point))
                  (words (scan-all-buffer-words src-word point)))
             (when words (delete-character point (- (length src-word))))
             (setf *rest-words* (rest words))
             (setf *all-words* words)
             (setf *start-charpos* (point-charpos point))
             (if words
                 (insert-string point (first words))
                 (message (format nil "No dynamic expansion for '~A' found" src-word))))))))
