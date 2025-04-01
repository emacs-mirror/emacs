(in-package :lem/buffer/internal)

(defstruct pps-state
  type
  token-start-point
  end-char
  block-comment-depth
  block-pair
  paren-stack
  (paren-depth 0))

(defun parse-partial-sexp (from to &optional state comment-stop)
  (assert (eq (point-buffer from)
              (point-buffer to)))
  (unless state (setf state (make-pps-state)))
  (with-point-syntax from
    (let ((p from)
          (type (pps-state-type state))
          (token-start-point (pps-state-token-start-point state))
          (end-char (pps-state-end-char state))
          (block-comment-depth (pps-state-block-comment-depth state))
          (block-pair (pps-state-block-pair state))
          (paren-stack (pps-state-paren-stack state))
          (paren-depth (pps-state-paren-depth state)))
      (flet ((update-token-start-point (p)
               (if token-start-point
                   (move-point token-start-point p)
                   (setf token-start-point (copy-point p :temporary)))))
        (block outer
          (loop
            (case type
              (:block-string
               ;; TODO: a203e4cf9のブロックコメントの修正をここにもすること
               (cond
                 ((search-forward p (cdr block-pair) to)
                  (setf block-pair nil)
                  (setf type nil)
                  (setf token-start-point nil))
                 (t
                  (move-point p to)
                  (return-from outer))))
              ((:string :fence)
               (loop
                 (when (point<= to p)
                   (return-from outer))
                 (let ((c (character-at p)))
                   (cond ((syntax-escape-char-p c)
                          (character-offset p 1))
                         ((char= c end-char)
                          (setf end-char nil)
                          (setf type nil)
                          (setf token-start-point nil)
                          (return (character-offset p 1))))
                   (character-offset p 1))))
              (:block-comment
               (let ((regex (%create-pair-regex block-pair)))
                 (loop
                   (unless (search-forward-regexp p regex to)
                     (move-point p to)
                     (return-from outer))
                   (let ((end-block-p (match-string-at p (cdr block-pair))))
                     (let ((offset
                             (length (if end-block-p
                                         (cdr block-pair)
                                         (car block-pair)))))
                       (character-offset p offset)
                       (when (point< to p)
                         (character-offset p (- offset))
                         (return-from outer)))
                     (cond
                       (end-block-p
                        (when (= 0 (decf block-comment-depth))
                          (setf block-comment-depth nil)
                          (setf block-pair nil)
                          (setf type nil)
                          (setf token-start-point nil)
                          (return p)))
                       (t
                        (incf block-comment-depth)))))))
              (:line-comment
               (when (and (point<= p to)
                          (same-line-p p to))
                 (return-from outer))
               (line-offset p 1)
               (setf type nil)
               (setf token-start-point nil))
              (otherwise
               (loop
                 (when (point<= to p)
                   (return-from outer))
                 (let ((c (character-at p)))
                   (cond
                     ((syntax-escape-char-p c)
                      (character-offset p 1))
                     ((multiple-value-bind (n pair)
                          (syntax-start-block-string-p p)
                        (when n
                          (update-token-start-point p)
                          (setf type :block-string)
                          (setf block-pair pair)
                          (character-offset p n)
                          (return))))
                     ((or (syntax-string-quote-char-p c)
                          (syntax-fence-char-p c))
                      (setf type (if (syntax-string-quote-char-p c)
                                     :string
                                     :fence))
                      (setf end-char c)
                      (update-token-start-point p)
                      (character-offset p 1)
                      (return))
                     ((multiple-value-bind (n pair)
                          (syntax-start-block-comment-p p)
                        (when n
                          (when comment-stop
                            (return-from outer))
                          (update-token-start-point p)
                          (setf type :block-comment)
                          (setf block-comment-depth 1)
                          (setf block-pair pair)
                          (character-offset p n)
                          (return))))
                     ((syntax-line-comment-p p)
                      (when comment-stop
                        (return-from outer))
                      (setf type :line-comment)
                      (update-token-start-point p)
                      (return))
                     ((syntax-open-paren-char-p c)
                      (incf paren-depth)
                      (push c paren-stack))
                     ((syntax-closed-paren-char-p c)
                      (decf paren-depth)
                      (when (syntax-equal-paren-p c (car paren-stack))
                        (pop paren-stack))))
                   (character-offset p 1)))))))
        (without-interrupts
          (setf (pps-state-type state) type
                (pps-state-token-start-point state) token-start-point
                (pps-state-end-char state) end-char
                (pps-state-block-comment-depth state) block-comment-depth
                (pps-state-block-pair state) block-pair
                (pps-state-paren-stack state) paren-stack
                (pps-state-paren-depth state) paren-depth))
        state))))

(define-editor-variable syntax-ppss-cache nil)

(defmacro syntax-ppss-cache (buffer)
  `(variable-value 'syntax-ppss-cache :buffer ,buffer))

(flet ((cache-point (cache) (car cache))
       (cache-state (cache) (cdr cache)))
  (defun syntax-ppss (point)
    (let* ((buffer (point-buffer point))
           (cache-list (syntax-ppss-cache buffer))
           state)
      (do ((rest cache-list (cdr rest))
           (prev nil rest))
          ((null rest)
           (setf state (parse-partial-sexp (copy-point (buffer-start-point buffer) :temporary)
                                           point))
           (let ((new-rest (list (cons (copy-point point :temporary) state))))
             (if prev
                 (setf (cdr prev) new-rest)
                 (setf cache-list new-rest))))
        (cond ((point= point (cache-point (car rest)))
               (setf state (cache-state (car rest)))
               (return))
              ((point> point (cache-point (car rest)))
               (setf state (parse-partial-sexp (copy-point (cache-point (car rest)) :temporary)
                                               point
                                               (copy-pps-state (cache-state (car rest)))))
               (let ((new-rest (cons (cons (copy-point point :temporary) state)
                                     rest)))
                 (if prev
                     (setf (cdr prev) new-rest)
                     (setf cache-list new-rest))
                 (return)))))
      (add-hook (variable-value 'before-change-functions :buffer buffer)
                'syntax-ppss-clear-cache)
      (setf (syntax-ppss-cache buffer)
            cache-list)
      state))

  (defun syntax-ppss-clear-cache (point &rest ignore-args)
    (declare (ignore ignore-args))
    (if (null point)
        (setf (syntax-ppss-cache point) nil)
        (let ((list (member-if (lambda (cache)
                                 (point< (cache-point cache) point))
                               (syntax-ppss-cache point))))
          (setf (syntax-ppss-cache point) list))))

  (defun check-ppss-cache (buffer)
    (loop :for prev := nil :then (cache-point cache)
          :for cache :in (syntax-ppss-cache buffer)
          :do (assert (alive-point-p (cache-point cache)))
              (when prev
                (assert (point< (cache-point cache) prev))))))

(defun pps-state-string-p (state)
  (member (pps-state-type state) '(:block-string :string)))

(defun pps-state-comment-p (state)
  (member (pps-state-type state) '(:line-comment :block-comment)))

(defun pps-state-string-or-comment-p (state)
  (member (pps-state-type state)
          '(:block-string
            :string
            :line-comment
            :block-comment)))

(defun in-string-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-string-p state)))

(defun in-comment-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-comment-p state)))

(defun in-string-or-comment-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-string-or-comment-p state)))

(defun maybe-beginning-of-string (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state) '(:block-string :string))
      (move-point point (pps-state-token-start-point state)))))

(defun maybe-beginning-of-comment (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state) '(:line-comment :block-comment))
      (move-point point (pps-state-token-start-point state)))))

(defun maybe-beginning-of-string-or-comment (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state)
                  '(:block-string
                    :string
                    :line-comment
                    :block-comment))
      (move-point point (pps-state-token-start-point state)))))
