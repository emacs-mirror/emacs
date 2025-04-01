(in-package :lem/buffer/internal)

(defvar *case-fold-search* nil)

(defun char-compare-function ()
  (if *case-fold-search*
      #'char=
      #'char-equal))

(defun search-step (point first-search search step move-matched endp)
  (with-point ((start-point point))
    (let ((result
            (let ((res (funcall first-search point)))
              (cond (res
                     (funcall move-matched point res)
                     (not (funcall endp point)))
                    (t
                     (loop :until (funcall endp point) :do
                              (unless (funcall step point)
                                (return nil))
                              (let ((res (funcall search point)))
                                (when res
                                  (funcall move-matched point res)
                                  (return t)))))))))
      (if (and result (not (funcall endp point)))
          (when result point)
          (progn
            (move-point point start-point)
            nil)))))

(defun search-forward-endp-function (limit-point)
  (if limit-point
      (lambda (point)
        (or (point< limit-point point)
            (end-buffer-p point)))
      (constantly nil)))

(defun search-backward-endp-function (limit-point)
  (if limit-point
      (lambda (point)
        (point< point limit-point))
      (constantly nil)))

(defun search-with-case-fold-search (sub-seq main-seq &rest args)
  (apply #'search
         sub-seq main-seq
         :test (char-compare-function)
         args))

(defun search-forward (point string &optional limit-point)
  (let ((nlines (count #\newline string)))
    (flet ((take-string (point)
             (with-point ((start-point point)
                          (end-point point))
               (points-to-string (line-start start-point)
                                 (line-end (or (line-offset end-point nlines)
                                               (buffer-end end-point)))))))
      (search-step point
                   (lambda (point)
                     (search-with-case-fold-search string
                                                   (take-string point)
                                                   :start2 (point-charpos point)))
                   (lambda (point)
                     (search-with-case-fold-search string (take-string point)))
                   (lambda (point)
                     (line-offset point 1))
                   (lambda (point charpos)
                     (character-offset (line-start point)
                                       (+ charpos (length string))))
                   (search-forward-endp-function limit-point)))))

(defun search-backward (point string &optional limit-point)
  (let ((nlines (count #\newline string)))
    (flet ((search-from-end (point end-charpos)
             (with-point ((point point))
               (when (line-offset point (- nlines))
                 (search-with-case-fold-search
                  string
                  (points-to-string
                   (line-start (copy-point point :temporary))
                   (with-point ((point point))
                     (unless (line-offset point nlines)
                       (buffer-end point))
                     (if end-charpos
                         (character-offset point end-charpos)
                         (line-end point))))
                  :from-end t)))))
      (let ((end-charpos (point-charpos point)))
        (search-step point
                     (lambda (point)
                       (search-from-end point end-charpos))
                     (lambda (point)
                       (search-from-end point nil))
                     (lambda (point)
                       (line-offset point -1))
                     (lambda (point charpos)
                       (unless (zerop nlines)
                         (line-offset point (- nlines)))
                       (character-offset (line-start point) charpos))
                     (search-backward-endp-function limit-point))))))

(defun search-forward-regexp (point regex &optional limit-point)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex)))
        (string)
        (reg-starts)
        (reg-ends))
    (when scanner
      (when (search-step point
                         (lambda (point)
                           (multiple-value-bind (start end reg-starts-1 reg-ends-1)
                               (ppcre:scan scanner
                                           (line-string point)
                                           :start (point-charpos point))
                             (when (and start (<= (point-charpos point) start))
                               (setf string (line-string point))
                               (setf reg-starts reg-starts-1)
                               (setf reg-ends reg-ends-1)
                               end)))
                         (lambda (point)
                           (multiple-value-bind (_start end reg-starts-1 reg-ends-1)
                               (ppcre:scan scanner (line-string point))
                             (declare (ignore _start))
                             (when end
                               (setf string (line-string point))
                               (setf reg-starts reg-starts-1)
                               (setf reg-ends reg-ends-1)
                               end)))
                         (lambda (point)
                           (line-offset point 1))
                         (lambda (point charpos)
                           (character-offset (line-start point) charpos))
                         (search-forward-endp-function limit-point))
        (if reg-starts
            (apply #'values
                   point
                   (map 'list (lambda (reg-start reg-end)
                                (when reg-start
                                  (subseq string reg-start reg-end)))
                        reg-starts
                        reg-ends))
            t)))))

(define-compiler-macro search-forward-regexp (&whole form &environment env
                                                     point regex &optional limit-point)
  (if (constantp regex env)
      `(search-forward-regexp ,point (load-time-value (ppcre:create-scanner ,regex))
                              ,limit-point)
      form))

(defun search-backward-regexp (point regex &optional limit-point)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex)))
        (string)
        (reg-starts)
        (reg-ends))
    (when scanner
      (when (search-step point
                         (lambda (point)
                           (let ((pos)
                                 (str (line-string point)))
                             (ppcre:do-scans (start end reg-starts-1 reg-ends-1 scanner
                                                    str nil :end (point-charpos point))
                               (setf string str)
                               (setf reg-starts reg-starts-1)
                               (setf reg-ends reg-ends-1)
                               (setf pos start))
                             pos))
                         (lambda (point)
                           (let ((pos)
                                 (str (line-string point)))
                             (ppcre:do-scans (start end reg-starts-1 reg-ends-1 scanner
                                                    str nil
                                                    :start (point-charpos point))
                               (setf string str)
                               (setf reg-starts reg-starts-1)
                               (setf reg-ends reg-ends-1)
                               (setf pos start))
                             pos))
                         (lambda (point)
                           (line-offset point -1))
                         (lambda (point charpos)
                           (character-offset (line-start point) charpos))
                         (search-backward-endp-function limit-point))
        (if reg-starts
            (apply #'values
                   point
                   (map 'list (lambda (reg-start reg-end)
                                (when reg-start
                                  (subseq string reg-start reg-end)))
                        reg-starts
                        reg-ends))
            t)))))

(define-compiler-macro search-backward-regexp (&whole form &environment env
                                                      point regex &optional limit-point)
  (if (constantp form env)
      `(search-backward-regexp ,point (load-time-value (ppcre:create-scanner ,regex))
                               ,limit-point)
      form))

(defun search-symbol (string name &key (start 0) (end (length string)) from-end)
  (loop :while (< start end)
        :do (let ((pos (search name string :start2 start :end2 end :from-end from-end
                               :test (char-compare-function))))
              (when pos
                (let ((pos2 (+ pos (length name))))
                  (when (and (or (zerop pos)
                                 (not (syntax-symbol-char-p (aref string (1- pos)))))
                             (or (>= pos2 (length string))
                                 (not (syntax-symbol-char-p (aref string pos2)))))
                    (return (cons pos pos2)))))
              (if from-end
                  (setf end (1- (or pos end)))
                  (setf start (1+ (or pos start)))))))

(defun search-forward-symbol (point name &optional limit-point)
  (check-type point point)
  (check-type name string)
  ;; NOTE:
  ;; search-symbol関数ないで使っているsyntax-symbol-char-pがcurrent-syntaxを参照しているので
  ;; ここでcurrent-syntaxを一時的にpointのものに切り替える
  (with-current-syntax (buffer-syntax-table (point-buffer point))
    (let ((charpos (point-charpos point)))
      (search-step point
                   (lambda (point)
                     (cdr (search-symbol (line-string point) name :start charpos)))
                   (lambda (point)
                     (cdr (search-symbol (line-string point) name)))
                   (lambda (point)
                     (line-offset point 1))
                   (lambda (point charpos)
                     (line-offset point 0 charpos))
                   (search-forward-endp-function limit-point)))))

(defun search-backward-symbol (point name &optional limit-point)
  (check-type point point)
  (check-type name string)
  ;; NOTE:
  ;; search-symbol関数ないで使っているsyntax-symbol-char-pがcurrent-syntaxを参照しているので
  ;; ここでcurrent-syntaxを一時的にpointのものに切り替える
  (with-current-syntax (buffer-syntax-table (point-buffer point))
    (search-step point
                 (lambda (point)
                   (car (search-symbol (line-string point)
                                       name
                                       :end (point-charpos point)
                                       :from-end t)))
                 (lambda (point)
                   (car (search-symbol (line-string point)
                                       name
                                       :from-end t)))
                 (lambda (point)
                   (line-offset point -1))
                 (lambda (point charpos)
                   (line-offset point 0 charpos))
                 (search-backward-endp-function limit-point))))

(defun looking-at (point regex)
  "Look if the line content after POINT matches REGEX.

Return multiple values: the matching string or nil, a vector of matching substrings."
  (let ((start (point-charpos point))
        (string (line-string point)))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan regex string :start start)
      (when (eql match-start start)
        (values (subseq string match-start match-end)
                (map 'vector
                     (lambda (reg-start reg-end)
                       (when reg-start
                         (subseq string reg-start reg-end)))
                     reg-starts
                     reg-ends))))))

(define-compiler-macro looking-at (&whole form &environment env point regex)
  (if (constantp regex env)
      `(looking-at ,point (load-time-value (ppcre:create-scanner ,regex)))
      form))

(defun match-string-at (point string &optional across-line-p)
  (let ((overp
          (> (+ (point-charpos point) (length string))
             (length (line-string point)))))
    (cond ((and across-line-p overp)
           (string= string
                    (points-to-string point
                                      (character-offset (copy-point point :temporary)
                                                        (length string)))))
          (overp
           nil)
          (t
           (string= (line-string point) string
                    :start1 (point-charpos point)
                    :end1 (+ (length string)
                             (point-charpos point)))))))
