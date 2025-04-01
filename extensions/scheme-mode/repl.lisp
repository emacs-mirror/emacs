(in-package :lem-scheme-mode)

(define-major-mode scheme-repl-mode scheme-mode
    (:name "Scheme REPL"
     :keymap *scheme-repl-mode-keymap*)
  (cond
    ((or (eq (scheme-repl-type :kind :current) :scheme-process)
         (eq (scheme-repl-type :kind :current) :scheme-slime))
     (lem/listener-mode:start-listener-mode)
     (when (eq (scheme-repl-type :kind :current) :scheme-process)
       ;; disable listener-mode functions
       (setf (variable-value 'lem/listener-mode:listener-set-prompt-function)
             #'(lambda (point) point)
             (variable-value 'lem/listener-mode:listener-check-input-function)
             #'(lambda (point) (declare (ignore point)))
             (variable-value 'lem/listener-mode:listener-execute-function)
             #'(lambda (point string) (declare (ignore point string)))))
     (repl-reset-input)
     (setf *write-string-function* 'write-string-to-repl)
     (setf (variable-value 'completion-spec) 'repl-completion)
     ;; overwrite listener-mode keymap
     (scheme-repl-input-mode t))
    (t
     (editor-error "No connection for repl. Did you mean 'start-scheme-repl' command?"))))

(define-minor-mode scheme-repl-input-mode
    (:name "scheme-repl-input"
     :keymap *scheme-repl-input-mode-keymap*))
(define-key *scheme-repl-input-mode-keymap* "Return" 'scheme-eval-or-newline)

(defun scheme-repl-type (&key kind)
  "Return scheme repl type, which is :scheme-slime or :scheme-process."
  (case kind
    ((:initial)
     (cond
       ((connected-p)
        :scheme-slime)
       ((and *use-scheme-process*
             (boundp '*scheme-process*))
        :scheme-process)))
    ((:current)
     (cond
       ((eq (repl-buffer) (current-buffer))
        :scheme-slime)
       ((and *use-scheme-process*
             (boundp '*scheme-process*)
             (eq (scheme-process-buffer :auto-make nil) (current-buffer)))
        :scheme-process)))
    (t
     (cond
       ((repl-buffer)
        :scheme-slime)
       ((and *use-scheme-process*
             (boundp '*scheme-process*)
             (scheme-process-buffer :auto-make nil))
        :scheme-process)))))

(define-command start-scheme-repl () ()
  (case (scheme-repl-type :kind :initial)
    ((:scheme-slime)
     (lem/listener-mode:listener-start "*scheme-repl*" 'scheme-repl-mode))
    ((:scheme-process)
     (scheme-run-process)
     (switch-to-window (pop-to-buffer (scheme-process-buffer))))
    (t
     (editor-error "Scheme repl is not available."))))

(define-command scheme-switch-to-repl-buffer () ()
  (let ((buffer (repl-buffer)))
    (if buffer
        (switch-to-window (pop-to-buffer buffer))
        (start-scheme-repl))))

(define-command scheme-eval-or-newline () ()
  (cond
    ((eq (scheme-repl-type :kind :current) :scheme-slime)
     (check-connection)
     (lem/listener-mode:listener-return))
    ((and (eq (scheme-repl-type :kind :current) :scheme-process)
          (point<= (lem/listener-mode:input-start-point (current-buffer))
                   (current-point))
          (repl-paren-correspond-p (current-point)))
     (let ((str (points-to-string
                 (lem/listener-mode:input-start-point (current-buffer))
                 (current-point))))
       (lem/common/history:add-history (lem/listener-mode::current-listener-history) str)
       (scheme-run-process-and-output-newline)
       (scheme-send-input str)))
    (t
     (insert-character (current-point) #\newline))))

(defun repl-paren-correspond-p (point)
  (with-point ((start (lem/listener-mode:input-start-point
                       (point-buffer point))))
    (let ((state (parse-partial-sexp start point)))
      (and (not (member (pps-state-type state)
                        '(:string :fence :block-string :block-comment)))
           (>= 0 (pps-state-paren-depth state))))))

(define-command scheme-eval-last-expression (p) (:universal-nil)
  (declare (ignore p))
  (cond
    ((eq (scheme-repl-type) :scheme-slime)
     (check-connection)
     (with-point ((start (current-point))
                  (end (current-point)))
       (form-offset start -1)
       (run-hooks (variable-value 'before-eval-functions) start end)
       (let ((string (points-to-string start end)))
         ;(if p
         ;    (eval-print string (- (window-width (current-window)) 2))
         ;    (interactive-eval string))
         (repl-eval nil string)
         )))
    ((eq (scheme-repl-type :kind :initial) :scheme-process)
     (with-point ((start (current-point))
                  (end   (current-point)))
       (form-offset start -1)
       (scheme-run-process-and-output-newline)
       (scheme-send-input (points-to-string start end))))
    (t
     (editor-error "Scheme repl is not available."))))

(define-command scheme-eval-region (start end) (:region)
  (cond
    ((eq (scheme-repl-type) :scheme-slime)
     (check-connection)
     ;(eval-with-transcript
     ; `(swank:interactive-eval-region
     ;   ,(points-to-string start end)))
     (repl-eval nil (points-to-string start end))
     )
    ((eq (scheme-repl-type :kind :initial) :scheme-process)
     (let ((string (points-to-string start end)))
       (scheme-run-process-and-output-newline)
       (scheme-send-input string)))
    (t
     (editor-error "Scheme repl is not available."))))

(define-command scheme-load-file (filename)
    ((prompt-for-file "Load File: "
                      :directory (or (buffer-filename) (buffer-directory))
                      :default nil
                      :existing t))
  (cond
    ((eq (scheme-repl-type) :scheme-slime)
     (check-connection)
     (when (and (probe-file filename)
                (not (uiop:directory-pathname-p filename)))
       (run-hooks (variable-value 'load-file-functions) filename)
       ;(interactive-eval
       ; (prin1-to-string
       ;  `(if (and (find-package :roswell)
       ;            (find-symbol (string :load) :roswell))
       ;       (uiop:symbol-call :roswell :load ,filename)
       ;       (swank:load-file ,filename))))
       (scheme-eval-from-string
        (format nil "(swank:load-file ~S)" filename))
       ;(when (repl-buffer)
       ;  (lem/listener-mode:refresh-prompt (repl-buffer)))
       (message "Loaded")))
    ((eq (scheme-repl-type :kind :initial) :scheme-process)
     (scheme-run-process-and-output-newline)
     (when (and (probe-file filename)
                (not (uiop:directory-pathname-p filename)))
       (scheme-send-input (format nil "(~a \"~a\")" *scheme-load-command* filename))
       (message "Loaded")))
    (t
     (editor-error "Scheme repl is not available."))))


;; from lisp-mode/repl.lisp

(defun read-string-thread-stack ()
  (buffer-value (repl-buffer) 'read-string-thread-stack))

(defun (setf read-string-thread-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-thread-stack) val))

(defun read-string-tag-stack ()
  (buffer-value (repl-buffer) 'read-string-tag-stack))

(defun (setf read-string-tag-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-tag-stack) val))

(define-key *scheme-repl-mode-keymap* "C-c C-c" 'scheme-repl-interrupt)
(define-key *scheme-repl-mode-keymap* "," 'scheme-repl-shortcut)

(define-command scheme-repl-interrupt () ()
  (check-connection)
  (send-message-string *connection*
                       (format nil "(:emacs-interrupt ~(~S~))"
                               (or (car (read-string-thread-stack))
                                   :repl-thread))))

(defvar *scheme-repl-shortcuts* '())

(defun prompt-for-shortcuts ()
  (let* ((*scheme-repl-shortcuts* *scheme-repl-shortcuts*)
         (names (mapcar #'car *scheme-repl-shortcuts*)))
    (cdr (assoc (prompt-for-string
                 "Command:"
                 :completion-function (lambda (x) (completion-strings x names))
                 :test-function (lambda (name) (member name names :test #'string=))
                 :history-symbol 'mh-scheme-repl-shortcuts)
                *scheme-repl-shortcuts* :test #'equal))))

(define-command scheme-repl-shortcut (n) (:universal)
  (cond
    ((not *use-scheme-repl-shortcut*)
     (insert-character (current-point) #\,))
    ((or (eq (scheme-repl-type :kind :current) :scheme-process)
         (eq (scheme-repl-type :kind :current) :scheme-slime))
     (with-point ((point (current-point)))
       (if (point>= (lem/listener-mode:input-start-point (current-buffer)) point)
           (let ((fun (prompt-for-shortcuts)))
             (when fun
               (funcall fun n)))
           (let ((c (insertion-key-p (last-read-key-sequence))))
             (insert-character point c n)))))
    (t
     (editor-error "You are not in repl."))))

(defmacro define-repl-shortcut (name lambda-list &body body)
  (if (symbolp lambda-list)
      `(progn
         (setf *scheme-repl-shortcuts* (remove ,(string-downcase name) *scheme-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',lambda-list) *scheme-repl-shortcuts*)
         ',name)
      `(progn
         (setf *scheme-repl-shortcuts* (remove ,(string-downcase name) *scheme-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',name) *scheme-repl-shortcuts*)
         (defun ,name ,lambda-list ,@body))))

(defun repl-buffer ()
  (get-buffer "*scheme-repl*"))

(defun repl-set-prompt (point)
  (insert-string point
                 (format nil "~A> " (connection-prompt-string *connection*)))

  ;; for r7rs-swank (move point to buffer-end)
  (alexandria:when-let ((window (get-repl-window)))
    (with-current-window window
      (buffer-end point)
      (window-see window)))

  point)

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
            'repl-set-prompt
            (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
            'repl-paren-correspond-p
            (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
            'repl-eval))))

(defun repl-change-read-line-input ()
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function)
        'repl-read-line))

(defun clear-repl ()
  (lem/listener-mode:clear-listener (repl-buffer)))

(defun get-repl-window ()
  (let* ((buffer (repl-buffer)))
    (if (eq buffer (window-buffer (current-window)))
        (current-window)
        (first (get-buffer-windows buffer)))))

(defun repl-buffer-width ()
  (alexandria:when-let* ((window (get-repl-window))
                         (width (- (window-width window) 2)))
    width))

(defun repl-highlight-notes (notes)
  (let ((buffer (repl-buffer)))
    (dolist (note notes)
      (trivia:match note
        ((and (trivia:property :location location)
              (trivia:property :message _))
         (let* ((xref-loc (source-location-to-xref-location location))
                (offset (xref-location-position xref-loc)))
           (with-point ((start (buffer-point buffer)))
             (move-point start (lem/listener-mode:input-start-point buffer))
             (form-offset start -1)
             (character-offset start (if (plusp offset) (1- offset) offset))
             (with-point ((end start))
               (form-offset end 1)
               (put-text-property start end :attribute 'compiler-note-attribute)))))))))

(defun repl-completion (point)
  (with-point ((p point))
    (cond ((maybe-beginning-of-string p)
           (character-offset p 1)
           (let ((str (points-to-string p point)))
             (mapcar (lambda (filename)
                       (make-completion-item :label filename
                                             :start p
                                             :end point))
                     (completion-file str (lem:buffer-directory (point-buffer p))))))
          (t
           (completion-symbol p)))))

(defvar *repl-compiler-check* nil)

(defvar *repl-temporary-file*
  (merge-pathnames "scheme-slime-repl.tmp" (uiop:temporary-directory)))

(defun repl-eval (point string)
  (declare (ignore point))
  (check-connection)
  (cond
    (*repl-compiler-check*
     (with-open-file (stream *repl-temporary-file*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (write-string string stream))
     (let ((result
             (let ((*write-string-function* (constantly nil)))
               (scheme-eval `(swank:compile-file-for-emacs *repl-temporary-file* nil)))))
       (destructuring-bind (notes successp duration loadp fastfile)
           (cdr result)
         (declare (ignore successp duration loadp fastfile))
         (repl-highlight-notes notes)
         (listener-eval string))))
    (t
     (listener-eval string))))

(defparameter *record-history-of-repl* nil)
(defvar *repl-history* '())

(defun listener-eval (string)

  ;; for r7rs-swank (suppress useless return value)
  (when (string= (string-trim '(#\space #\tab) string) "")
    (setf string "(values)"))

  ;; for r7rs-swank (error check)
  (handler-case
      (request-listener-eval
       *connection*
       string
       (lambda (value)
         (declare (ignore value))
         (lem/listener-mode:refresh-prompt (repl-buffer))
         (when *record-history-of-repl*
           (start-timer (make-idle-timer
                         (lambda ()
                           (when (position-if (complement #'syntax-space-char-p) string)
                             (push (cons string (scheme-eval-from-string "CL:/" "CL"))
                                   *repl-history*))))
                        0)))
       ;(repl-buffer-width)
       )
    (error () (scheme-slime-quit)
      (editor-error "No connection for repl. (eval failed)"))))

(defun repl-read-string (thread tag)
  (unless (repl-buffer) (start-scheme-repl))
  (let ((buffer (repl-buffer)))
    (push thread (read-string-thread-stack))
    (push tag (read-string-tag-stack))
    (switch-to-window (pop-to-buffer buffer))
    (buffer-end (current-point))
    (lem/listener-mode:change-input-start-point (current-point))
    (repl-change-read-line-input)))

(defun repl-pop-stack ()
  (let ((thread (pop (read-string-thread-stack)))
        (tag (pop (read-string-tag-stack))))
    (when (null (read-string-thread-stack))
      (repl-reset-input))
    (values thread tag)))

(defun repl-abort-read (thread tag)
  (declare (ignore thread tag))
  (repl-pop-stack)
  (message "Read aborted"))

(defun repl-read-line (point string)
  (declare (ignore point))
  (multiple-value-bind (thread tag) (repl-pop-stack)
    (dispatch-message (list :emacs-return-string
                            thread
                            tag
                            (concatenate 'string
                                         string
                                         (string #\newline))))))

(defun write-string-to-repl (string)
  (let ((buffer (repl-buffer)))
    (unless buffer
      (start-scheme-repl)
      (setf buffer (repl-buffer)))
    (with-point ((start (buffer-end-point buffer) :left-inserting))
      (when (text-property-at start :field -1)
        (insert-character start #\newline))
      (insert-escape-sequence-string (buffer-end-point buffer) string))
    (lem/listener-mode:change-input-start-point (buffer-end-point buffer))
    (buffer-end (buffer-point buffer))
    (alexandria:when-let ((window (get-repl-window)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window)))))

(defvar *escape-sequence-argument-specs*
  '(("0" :bold nil :reverse nil :underline nil)
    ("1" :bold t)
    ("2")
    ("4" :underline t)
    ("5")
    ("7" :reverse t)
    ("8")
    ("22" :bold nil)
    ("24" :underline nil)
    ("25")
    ("27" :reverse nil)
    ("28")

    ("30" :foreground "black")
    ("31" :foreground "red")
    ("32" :foreground "green")
    ("33" :foreground "yellow")
    ("34" :foreground "blue")
    ("35" :foreground "magenta")
    ("36" :foreground "cyan")
    ("37" :foreground "white")

    ("40" :background "black")
    ("41" :background "red")
    ("42" :background "green")
    ("43" :background "yellow")
    ("44" :background "blue")
    ("45" :background "magenta")
    ("46" :background "cyan")
    ("47" :background "white")

    ("90" :foreground "dim gray")
    ("91" :foreground "red")
    ("92" :foreground "green")
    ("93" :foreground "yello")
    ("94" :foreground "royalblue")
    ("95" :foreground "darkorchid1")
    ("96" :foreground "cyan1")
    ("97" :foreground "white")

    ("100" :background "dim gray")
    ("101" :background "red")
    ("102" :background "green")
    ("103" :background "yello")
    ("104" :background "royalblue")
    ("105" :background "darkorchid1")
    ("106" :background "cyan1")
    ("107" :background "white")))

(defun raw-seq-to-attribute (string)
  (let ((arguments (uiop:split-string string :separator '(#\;)))
        (attribute-parameters '()))
    (dolist (argument arguments)
      (alexandria:when-let (spec (assoc argument *escape-sequence-argument-specs*
                                        :test #'string=))
        (loop :for (key value) :on (rest spec) :by #'cddr
              :do (setf (getf attribute-parameters key) value))))
    (apply #'make-attribute attribute-parameters)))

(defun split-escape-sequence-string (string)
  (let ((acc '())
        (pos 0))
    (loop
      (multiple-value-bind (start end reg-starts reg-ends)
          (ppcre:scan "\\e\\[([^m]*)m" string :start pos)
        (unless (and start end reg-starts reg-ends) (return))
        (unless (= pos start)
          (push (subseq string pos start) acc))
        (push (raw-seq-to-attribute
               (subseq string
                       (aref reg-starts 0)
                       (aref reg-ends 0)))
              acc)
        (setf pos end)))
    (push (subseq string pos) acc)
    (nreverse acc)))

(defun parse-escape-sequence (string)
  (split-escape-sequence-string string))

(defun insert-escape-sequence-string (point string)
  (let ((tokens (parse-escape-sequence string))
        (current-attribute nil))
    (dolist (token tokens)
      (etypecase token
        (null
         (setf current-attribute nil))
        (attribute
         (setf current-attribute token))
        (string
         (insert-string point token :attribute current-attribute))))))

(define-repl-shortcut sayonara (n)
  (declare (ignorable n))
  ;(if (self-connection-p *connection*)
  ;    (message "Can't say sayonara because it's self connection.")
  ;    ;(interactive-eval "(swank:quit-lisp)")
  ;    ;(scheme-eval-from-string "(swank:quit-lisp)")
  ;    (interactive-eval "(exit)")
  ;    ))
  (case (scheme-repl-type)
    ((:scheme-slime)
     (scheme-slime-quit))
    ((:scheme-process)
     (scheme-kill-process))
    (t
     (editor-error "Scheme repl is not available."))))
