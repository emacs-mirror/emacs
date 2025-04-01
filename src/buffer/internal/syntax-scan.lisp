(in-package :lem/buffer/internal)

(flet ((%match (str1 str2 str1-pos)
         (let ((end1 (+ str1-pos (length str2))))
           (when (and (<= end1 (length str1))
                      (string= str1 str2
                               :start1 str1-pos
                               :end1 end1))
             (length str2)))))

  (defun syntax-start-block-syntax-string-p (line-string pos pairs)
    (dolist (pair pairs)
      (let ((start (car pair)))
        (let ((result (%match line-string start pos)))
          (when result
            (return (values result pair)))))))

  (defun syntax-end-block-syntax-string-p (line-string pos pairs)
    (dolist (pair pairs)
      (let* ((end (cdr pair))
             (pos (- pos (length end))))
        (when (<= 0 pos)
          (let ((result (%match line-string end pos)))
            (when result
              (return (values result pair))))))))

  (defun syntax-line-comment-p (point)
    (%match (line-string point)
            (syntax-table-line-comment-string (current-syntax))
            (point-charpos point)))

  (defun syntax-start-block-comment-p (point)
    (syntax-start-block-syntax-string-p (line-string point)
                                        (point-charpos point)
                                        (syntax-table-block-comment-pairs
                                         (current-syntax))))

  (defun syntax-end-block-comment-p (point)
    (syntax-end-block-syntax-string-p (line-string point)
                                      (point-charpos point)
                                      (syntax-table-block-comment-pairs
                                       (current-syntax))))

  (defun syntax-start-block-string-p (point)
    (syntax-start-block-syntax-string-p (line-string point)
                                        (point-charpos point)
                                        (syntax-table-block-string-pairs
                                         (current-syntax))))

  (defun syntax-end-block-string-p (point)
    (syntax-end-block-syntax-string-p (line-string point)
                                      (point-charpos point)
                                      (syntax-table-block-string-pairs
                                       (current-syntax)))))


(let ((cache (make-hash-table :test 'equal)))
  (defun %create-pair-regex (pair)
    (let ((tree
            `(:positive-lookahead
              (:alternation
               (:sequence ,(car pair))
               (:sequence ,(cdr pair))))))
      (or (gethash tree cache)
          (setf (gethash tree cache)
                (ppcre:create-scanner tree))))))

(defun syntax-escape-point-p (point offset)
  (let ((count 0))
    (loop :with string := (line-string point)
          :for i :downfrom (+ (1- (point-charpos point)) offset) :to 0
          :do (if (syntax-escape-char-p (schar string i))
                  (incf count)
                  (return)))
    (when (oddp count)
      count)))

(defun inline-line-comment-p (point)
  (flet ((search-line-comment-backward (point)
           (with-point ((point point))
             (loop
               (when (syntax-line-comment-p point)
                 (return t))
               (when (start-line-p point)
                 (return nil))
               (unless (character-offset point -1)
                 (return nil))))))
    (when (search-line-comment-backward point)
      (let ((pps-state (syntax-ppss point)))
        (when (pps-state-comment-p pps-state)
          (move-point point (pps-state-token-start-point pps-state)))))))

(defun %skip-comment-forward (point)
  (multiple-value-bind (n pair)
      (syntax-start-block-comment-p point)
    (cond (n
           (let ((regex (%create-pair-regex pair)))
             (with-point ((curr point))
               (character-offset curr n)
               (loop :with depth := 1
                     :do (cond ((not (search-forward-regexp curr regex))
                                (return (values nil nil)))
                               ((match-string-at curr (cdr pair))
                                (character-offset curr (length (cdr pair)))
                                (when (= 0 (decf depth))
                                  (return (values (move-point point curr) t))))
                               (t
                                (character-offset curr (length (car pair)))
                                (incf depth)))))))
          ((syntax-line-comment-p point)
           (values (line-offset point 1) t))
          (t
           (values nil t)))))

(defun %skip-block-comment-backward (point)
  (multiple-value-bind (n pair)
      (syntax-end-block-comment-p point)
    (if n
        (let ((regex (%create-pair-regex pair)))
          (with-point ((curr point))
            (character-offset curr (- n))
            (loop :with depth := 1
                  :do (cond ((not (search-backward-regexp curr regex))
                             (return (values nil nil)))
                            ((match-string-at curr (car pair))
                             (when (= 0 (decf depth))
                               (return (values (move-point point curr) t))))
                            (t
                             (incf depth))))))
        (values nil t))))

(defun %skip-comment-backward (point)
  (multiple-value-bind (p win)
      (%skip-block-comment-backward point)
    (cond ((not win)
           (values nil nil))
          ((null p)
           (alexandria:if-let (p (inline-line-comment-p point))
             (values (move-point point p) t)
             (values nil t)))
          (t
           (values point win)))))

(defun skip-space-and-comment-forward (point)
  (with-point-syntax point
    (loop
      (skip-chars-forward point #'syntax-space-char-p)
      (multiple-value-bind (result success)
          (%skip-comment-forward point)
        (unless result
          (return success))))))

(defun skip-space-and-comment-backward (point)
  (with-point-syntax point
    (labels ((skip-spaces ()
               (skip-chars-backward point #'syntax-space-char-p))
             (skip ()
               (loop
                 (skip-spaces)
                 (multiple-value-bind (result success)
                     (%skip-comment-backward point)
                   (unless result
                     (return success))))))
      (cond ((syntax-string-quote-char-p (character-at point -1))
             nil)
            ((syntax-end-block-comment-p point)
             (skip))
            ((inline-line-comment-p point)
             (skip-spaces))
            (t
             (skip))))))

(defun %skip-symbol-forward (point)
  (loop :for c := (character-at point 0)
        :do (cond ((syntax-escape-char-p c)
                   (unless (character-offset point 2)
                     (return)))
                  ((syntax-fence-char-p c)
                   (unless (%skip-fence-forward point)
                     (return)))
                  ((not (or (syntax-symbol-char-p c)
                            (syntax-expr-prefix-char-p c)))
                   (return point))
                  (t
                   (unless (character-offset point 1)
                     (return))))))

(defun %skip-symbol-backward (point)
  (loop :for c := (character-at point -1)
        :do (let ((skip-count (syntax-escape-point-p point -1)))
              (cond (skip-count
                     (character-offset point (- (1+ skip-count))))
                    ((syntax-fence-char-p c)
                     (unless (%skip-fence-backward point)
                       (return)))
                    ((or (syntax-symbol-char-p c)
                         (syntax-expr-prefix-char-p c)
                         (syntax-escape-char-p c))
                     (character-offset point -1))
                    (t
                     (return point))))))

(defun %skip-quote-forward (point)
  (loop :with quote-char := (character-at point 0)
        :do (unless (character-offset point 1)
              (return nil))
            (let ((c (character-at point)))
              (cond ((syntax-escape-char-p c)
                     (character-offset point 1))
                    ((eql c quote-char)
                     (character-offset point 1)
                     (return point))))))

(defun %skip-quote-backward (point)
  (character-offset point -1)
  (loop :with quote-char := (character-at point)
        :do (unless (character-offset point -1)
              (return nil))
            (if (syntax-escape-point-p point 0)
                (character-offset point -1)
                (let ((c (character-at point)))
                  (cond ((eql c quote-char)
                         (return point)))))))

(defun %skip-string-forward (point)
  (%skip-quote-forward point))

(defun %skip-string-backward (point)
  (%skip-quote-backward point))

(defun %skip-fence-forward (point)
  (%skip-quote-forward point))

(defun %skip-fence-backward (point)
  (%skip-quote-backward point))

(defun %skip-list-forward (point depth limit-point)
  (loop :with paren-stack := '()
        :do (unless (skip-space-and-comment-forward point)
              (return nil))
            (when (if limit-point
                      (point<= limit-point point)
                      (end-buffer-p point))
              (return nil))
            (let ((c (character-at point 0))
                  n pair)
              (cond ((multiple-value-setq (n pair) (syntax-start-block-string-p point))
                     (character-offset point n)
                     (unless (search-forward point (cdr pair))
                       (return nil)))
                    ((syntax-open-paren-char-p c)
                     (push c paren-stack)
                     (character-offset point 1)
                     (when (zerop (incf depth))
                       (return point)))
                    ((syntax-closed-paren-char-p c)
                     (unless (or (and (< 0 depth)
                                      (null paren-stack))
                                 (syntax-equal-paren-p c (car paren-stack)))
                       (return nil))
                     (pop paren-stack)
                     (character-offset point 1)
                     (when (zerop (decf depth))
                       (return point)))
                    ((syntax-string-quote-char-p c)
                     (%skip-string-forward point))
                    ((syntax-fence-char-p c)
                     (%skip-fence-forward point))
                    ((syntax-escape-char-p c)
                     (unless (character-offset point 2)
                       (return nil)))
                    (t
                     (character-offset point 1))))))

(defun %skip-list-backward (point depth limit-point)
  (loop :with paren-stack := '()
        :do (when (if limit-point
                      (point<= point limit-point)
                      (start-buffer-p point))
              (return nil))
            (let ((c (character-at point -1))
                  n pair)
              (cond ((syntax-escape-point-p point -1)
                     (character-offset point -1))
                    ((syntax-closed-paren-char-p c)
                     (push c paren-stack)
                     (character-offset point -1)
                     (when (zerop (incf depth))
                       (return point)))
                    ((syntax-open-paren-char-p c)
                     (unless (or (and (< 0 depth)
                                      (null paren-stack))
                                 (syntax-equal-paren-p c (car paren-stack)))
                       (return nil))
                     (pop paren-stack)
                     (character-offset point -1)
                     (when (zerop (decf depth))
                       (return point)))
                    ((multiple-value-setq (n pair) (syntax-end-block-string-p point))
                     (character-offset point (- n))
                     (unless (search-backward point (car pair))
                       (return nil)))
                    ((syntax-string-quote-char-p c)
                     (%skip-string-backward point))
                    ((syntax-fence-char-p c)
                     (%skip-fence-backward point))
                    (t
                     (multiple-value-bind (p win)
                         (%skip-block-comment-backward point)
                       (unless win
                         (return nil))
                       (unless p
                         (character-offset point -1))))))
            (when (end-line-p point)
              (alexandria:when-let (p (inline-line-comment-p point))
                (move-point point p)))))

(defun %form-offset-positive (point)
  (skip-space-and-comment-forward point)
  (when (end-buffer-p point)
    (return-from %form-offset-positive nil))
  (syntax-skip-expr-prefix-forward point)
  (skip-chars-forward point #'syntax-expr-prefix-char-p)
  (unless (end-buffer-p point)
    (let ((c (character-at point))
          n pair)
      (let ((point
              (cond ((multiple-value-setq (n pair) (syntax-start-block-string-p point))
                     (character-offset point n)
                     (if (search-forward point (cdr pair))
                         point
                         nil))
                    ((syntax-open-paren-char-p c)
                     (%skip-list-forward point 0 nil))
                    ((or (syntax-symbol-char-p c)
                         (syntax-escape-char-p c))
                     (%skip-symbol-forward point))
                    ((syntax-expr-prefix-char-p c)
                     (character-offset point 1))
                    ((syntax-closed-paren-char-p c)
                     nil)
                    ((syntax-string-quote-char-p c)
                     (%skip-string-forward point))
                    ((syntax-fence-char-p c)
                     (%skip-fence-forward point))
                    (t
                     (character-offset point 1)))))
        (when point
          (when (syntax-table-expr-suffix-chars (current-syntax))
            (skip-chars-forward point
                                (syntax-table-expr-suffix-chars
                                 (current-syntax))))
          point)))))

(defun %form-offset-negative (point)
  (skip-space-and-comment-backward point)
  (when (start-buffer-p point)
    (return-from %form-offset-negative nil))
  (when (syntax-table-expr-suffix-chars (current-syntax))
    (skip-chars-backward point (syntax-table-expr-suffix-chars (current-syntax))))
  (let ((c (character-at point -1))
        (escape-point-p (syntax-escape-point-p point -1))
        n pair)
    (prog1 (cond ((and (syntax-closed-paren-char-p c)
                       (not escape-point-p))
                  (%skip-list-backward point 0 nil))
                 ((or (syntax-symbol-char-p c)
                      (syntax-escape-char-p c)
                      (syntax-expr-prefix-char-p c)
                      escape-point-p)
                  (%skip-symbol-backward point))
                 ((syntax-open-paren-char-p c)
                  nil)
                 ((multiple-value-setq (n pair) (syntax-end-block-string-p point))
                  (character-offset point (- n))
                  (if (search-backward point (car pair))
                      point
                      nil))
                 ((syntax-string-quote-char-p c)
                  (%skip-string-backward point))
                 ((syntax-fence-char-p c)
                  (%skip-fence-backward point))
                 (t
                  (character-offset point -1)))
      (skip-chars-backward point #'syntax-expr-prefix-char-p)
      (syntax-skip-expr-prefix-backward point))))

(defun form-offset (point n)
  (with-point-syntax point
    (with-point ((curr point))
      (when (cond ((plusp n)
                   (dotimes (_ n t)
                     (unless (%form-offset-positive curr)
                       (return nil))))
                  (t
                   (dotimes (_ (- n) t)
                     (unless (%form-offset-negative curr)
                       (return nil)))))
        (move-point point curr)))))

(defun scan-lists (point n depth &optional no-errors limit-point)
  (with-point-syntax point
    (with-point ((curr point))
      (when (cond ((plusp n)
                   (dotimes (_ n t)
                     (unless (%skip-list-forward curr depth limit-point)
                       (if no-errors
                           (return nil)
                           (scan-error)))))
                  (t
                   (dotimes (_ (- n) t)
                     (unless (%skip-list-backward curr depth limit-point)
                       (if no-errors
                           (return nil)
                           (scan-error))))))
        (move-point point curr)))))

(defun forward-down-list (point &optional no-errors limit-point)
  (scan-lists point 1 -1 no-errors limit-point))

(defun forward-up-list (point &optional no-errors limit-point)
  (scan-lists point 1 1 no-errors limit-point))

(defun backward-up-list (point &optional no-errors limit-point)
  (scan-lists point -1 1 no-errors limit-point))

(defun backward-down-list (point &optional no-errors limit-point)
  (scan-lists point -1 -1 no-errors limit-point))

(flet ((non-newline-whitespace-p (c)
         (and (char/= c #\newline)
              (syntax-space-char-p c))))

  (defun skip-whitespace-forward (point &optional (oneline nil))
    (with-point-syntax point
      (if oneline
          (skip-chars-forward point #'non-newline-whitespace-p)
          (skip-chars-forward point #'syntax-space-char-p))))

  (defun skip-whitespace-backward (point &optional (oneline nil))
    (with-point-syntax point
      (if oneline
          (skip-chars-backward point #'non-newline-whitespace-p)
          (skip-chars-backward point #'syntax-space-char-p)))))

(defun skip-symbol-forward (point)
  (with-point-syntax point
    (skip-chars-forward point #'syntax-symbol-char-p)))

(defun skip-symbol-backward (point)
  (with-point-syntax point
    (skip-chars-backward point #'syntax-symbol-char-p)))

(defun symbol-region-at-point (point)
  (with-point-syntax point
    (with-point ((point point))
      (skip-chars-backward point #'syntax-symbol-char-p)
      (unless (syntax-symbol-char-p (character-at point))
        (return-from symbol-region-at-point nil))
      (with-point ((start point))
        (skip-chars-forward point #'syntax-symbol-char-p)
        (values start point)))))

(defun symbol-string-at-point (point)
  (multiple-value-bind (start end)
      (symbol-region-at-point point)
    (when start
      (points-to-string start end))))
