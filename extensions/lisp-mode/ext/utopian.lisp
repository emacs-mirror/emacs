(defpackage :lem-lisp-mode/utopian
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/utopian)

(pushnew 'find-utopian-route *find-definitions*)

(defun find-utopian-route (point)
  (when (in-string-p point)
    (with-point ((start point)
                 (end point))
      (maybe-beginning-of-string start)
      (move-point end start)
      (character-offset start 1)
      (form-offset end 1)
      (character-offset end -1)
      (let* ((route (points-to-string start end))
             (parts (uiop:split-string route :separator ":")))
        (when (= 2 (length parts))
          (destructuring-bind (path name)
              parts
            (let ((filename (expand-file-name
                             (format nil "../controllers/~A.lisp" path)
                             (buffer-directory (current-buffer)))))
              (unless (probe-file filename)
                (editor-error "~A does not exists" filename))
              (lem/language-mode:make-xref-location
               :filespec (probe-file filename)
               :position (let ((buffer (find-file-buffer filename
                                                         :temporary t
                                                         :enable-undo-p nil)))
                           (with-point ((point (buffer-point buffer)))
                             (buffer-start point)
                             (search-forward-regexp
                              point
                              `(:sequence
                                "(def"
                                (:greedy-repetition 1 nil (:CHAR-CLASS :WORD-CHAR-CLASS #\-))
                                (:greedy-repetition 1 nil :whitespace-char-class)
                                (:greedy-repetition 0 1 #\()
                                (:greedy-repetition 0 nil :whitespace-char-class)
                                ,name
                                :whitespace-char-class))
                             (line-start point)
                             (position-at-point point)))))))))))
