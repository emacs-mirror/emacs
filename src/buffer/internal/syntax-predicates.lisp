(in-package :lem/buffer/internal)

(defvar *current-syntax* nil)

(defun current-syntax ()
  (or *current-syntax*
      (buffer-syntax-table (current-buffer))
      (fundamental-syntax-table)))

(defmacro with-current-syntax (syntax &body body)
  `(let ((*current-syntax* ,syntax))
     ,@body))

(defmacro with-point-syntax (point &body body)
  `(with-current-syntax (buffer-syntax-table (point-buffer ,point))
     ,@body))

(defun syntax-word-char-p (c)
  (and (characterp c)
       (alphanumericp c)))

(defun syntax-space-char-p (c)
  (member c (syntax-table-space-chars (current-syntax))))

(defun syntax-symbol-char-p (c)
  (or (syntax-word-char-p c)
      (member c (syntax-table-symbol-chars (current-syntax)))))

(defun syntax-open-paren-char-p (c)
  (assoc c (syntax-table-paren-pairs (current-syntax))))

(defun syntax-closed-paren-char-p (c)
  (rassoc c (syntax-table-paren-pairs (current-syntax))))

(defun syntax-equal-paren-p (x y)
  (flet ((f (c)
           (if (syntax-open-paren-char-p c)
               c
               (car (rassoc c (syntax-table-paren-pairs (current-syntax)))))))
    (eql (f x) (f y))))

(defun syntax-string-quote-char-p (c)
  (member c (syntax-table-string-quote-chars (current-syntax))))

(defun syntax-fence-char-p (c)
  (member c (syntax-table-fence-chars (current-syntax))))

(defun syntax-escape-char-p (c)
  (member c (syntax-table-escape-chars (current-syntax))))

(defun syntax-expr-prefix-char-p (c)
  (member c (syntax-table-expr-prefix-chars (current-syntax))))

(defun syntax-skip-expr-prefix-forward (point)
  (let ((f (syntax-table-expr-prefix-forward-function (current-syntax))))
    (if f (funcall f point) t)))

(defun syntax-skip-expr-prefix-backward (point)
  (let ((f (syntax-table-expr-prefix-backward-function (current-syntax))))
    (if f (funcall f point) t)))
