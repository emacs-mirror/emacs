(defpackage :lem/language-mode
  (:use :cl :lem)
  (:export
   :*language-mode-keymap*
   :language-mode
   :idle-function
   :beginning-of-defun-function
   :end-of-defun-function
   :line-comment
   :insertion-line-comment
   :find-definitions-function
   :find-definitions
   :find-references-function
   :find-references
   :language-mode-tag
   :buffer-language-mode
   :completion-spec
   :complete-symbol
   :indent-size
   :root-uri-patterns
   :detective-search
   :go-to-location
   :indent
   :newline-and-indent
   :indent-region
   :xref-headline-attribute
   :xref-content-attribute
   :xref-insert-content
   :make-xref-location
   :make-xref-references
   :xref-location-filespec
   :xref-location-position
   :xref-location-content
   :xref-references-type
   :xref-references-locations
   :xref-filespec-to-buffer
   :xref-filespec-to-filename
   :move-to-xref-location-position
   :display-xref-locations
   :display-xref-references
   :find-root-directory
   :buffer-root-directory)
  #+sbcl
  (:lock t))
(in-package :lem/language-mode)

(define-editor-variable idle-function nil)
(define-editor-variable beginning-of-defun-function nil)
(define-editor-variable end-of-defun-function nil)
(define-editor-variable line-comment nil)
(define-editor-variable insertion-line-comment nil)
(define-editor-variable find-definitions-function nil)
(define-editor-variable find-references-function nil)
(define-editor-variable language-mode-tag nil)
(define-editor-variable completion-spec nil)
(define-editor-variable indent-size 2)
(define-editor-variable root-uri-patterns '())
(define-editor-variable detective-search nil)

(defun prompt-for-symbol (prompt history-name)
  (prompt-for-string prompt :history-symbol history-name))

(defvar *idle-timer* nil)

(defun language-idle-function ()
  (alexandria:when-let ((fn (variable-value 'idle-function :buffer)))
    (funcall fn)))

(define-major-mode language-mode ()
    (:name "language"
     :keymap *language-mode-keymap*)
  (when (or (null *idle-timer*)
            (timer-expired-p *idle-timer*))
    (setf *idle-timer*
          (start-timer (make-idle-timer 'language-idle-function
                                        :handle-function (lambda (condition)
                                                           (stop-timer *idle-timer*)
                                                           (pop-up-backtrace condition)
                                                           (setf *idle-timer* nil))
                                        :name "language-idle-function")
                       200 :repeat t))))

(define-key *language-mode-keymap* "C-M-a" 'beginning-of-defun)
(define-key *language-mode-keymap* "C-M-e" 'end-of-defun)
(define-key *language-mode-keymap* "Tab" 'indent-line-and-complete-symbol)
(define-key *global-keymap* "C-j" 'newline-and-indent)
(define-key *global-keymap* "M-j" 'newline-and-indent)
(define-key *language-mode-keymap* "C-M-\\" 'indent-region)
(define-key *language-mode-keymap* "M-;" 'comment-or-uncomment-region)
(define-key *language-mode-keymap* "M-." 'find-definitions)
(define-key *language-mode-keymap* "M-_" 'find-references)
(define-key *language-mode-keymap* "M-?" 'find-references)
(define-key *language-mode-keymap* "M-," 'pop-definition-stack)
(define-key *language-mode-keymap* "C-M-i" 'complete-symbol)
(define-key *global-keymap* "M-(" 'insert-\(\)-or-wrap)
(define-key *global-keymap* "M-)" 'move-over-\)-or-wrap)

(defun beginning-of-defun-1 (n)
  (alexandria:when-let ((fn (variable-value 'beginning-of-defun-function :buffer)))
    (when fn (funcall fn (current-point) n))))

(define-command (beginning-of-defun (:advice-classes movable-advice)) (n) (:universal)
  (if (minusp n)
      (end-of-defun (- n))
      (beginning-of-defun-1 n)))

(define-command (end-of-defun (:advice-classes movable-advice)) (n) (:universal)
  (if (minusp n)
      (beginning-of-defun (- n))
      (alexandria:if-let ((fn (variable-value 'end-of-defun-function :buffer)))
        (funcall fn (current-point) n)
        (beginning-of-defun-1 (- n)))))

(define-command (indent (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  (if (variable-value 'calc-indent-function)
      (indent-line (current-point))
      (self-insert n)))

(defun trim-eol (point)
  (with-point ((start point)
               (end point))
    (skip-whitespace-backward (line-end start) t)
    (line-end end)
    (delete-between-points start end)))

(define-command (newline-and-indent (:advice-classes editable-advice)) (n) (:universal)
  (trim-eol (current-point))
  (insert-character (current-point) #\newline n)
  (indent-line (current-point)))

(define-command indent-region (start end) (:region)
  (indent-points start end))

(defmethod execute :around (mode
                            (command indent-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (indent-points (cursor-region-beginning (current-point))
                   (cursor-region-end (current-point)))))

(defun space*-p (point)
  (with-point ((point point))
    (skip-whitespace-forward point t)
    (end-line-p point)))

(defun indentation-point-p (point)
  (with-point ((p point))
    (back-to-indentation p)
    (point<= point p)))

(define-command (comment-or-uncomment-region (:advice-classes editable-advice)) () ()
  (if (commented-region-p)
      (uncomment-region)
      (comment-region)))

(defun select-current-line-if-no-region-is-selected (start end)
  (when (point= start end)
    (setf start (line-start start))
    (setf end (line-end end))))

(defun commented-region-p ()
  (alexandria:when-let ((line-comment (variable-value 'line-comment :buffer)))
    (with-point ((start (current-point))
                 (end (current-point)))
      (set-region-point-using-global-mode (current-global-mode) start end)
      (select-current-line-if-no-region-is-selected start end)
      
      (loop
        (skip-whitespace-forward start)
        (when (point>= start end)
          (return t))
        (unless (looking-at start line-comment)
          (return nil))
        (unless (line-offset start 1)
          (return t))))))

(define-command (comment-region (:advice-classes editable-advice)) () ()
  (save-excursion ; To keep to mark-set
    (let ((line-comment (or (variable-value 'insertion-line-comment :buffer)
                            (variable-value 'line-comment :buffer))))
      (when line-comment
        (with-point ((start (current-point) :right-inserting)
                     (end (current-point) :left-inserting))
          (set-region-point-using-global-mode (current-global-mode) start end)
          (select-current-line-if-no-region-is-selected start end)
          
          (skip-whitespace-forward start)
          (when (point>= start end)
            (insert-string (current-point) line-comment)
            (return-from comment-region))
          (let ((charpos (point-charpos start)))
            (loop
              (when (same-line-p start end)
                (cond ((space*-p start))
                      ((indentation-point-p end))
                      (t
                       (insert-string start line-comment)
                       (unless (space*-p end)
                         (insert-character end #\newline))))
                (return))
              (unless (space*-p start)
                (insert-string start line-comment))
              (line-offset start 1 charpos))))))))

(define-command (uncomment-region (:advice-classes editable-advice)) () ()
  (save-excursion ; To keep to mark-set
    (let* ((line-comment (variable-value 'line-comment :buffer))
           (insertion-line-comment (or (variable-value 'insertion-line-comment :buffer)
                                       line-comment)))
      (when line-comment
        (with-point ((start (current-point) :right-inserting)
                     (end (current-point) :right-inserting))
          (set-region-point-using-global-mode (current-global-mode) start end)
          (select-current-line-if-no-region-is-selected start end)
          
          (let ((p start))
            (loop
              (parse-partial-sexp p end nil t)
              (when (point<= end p) (return))
              (when (looking-at p line-comment)
                (let ((res (looking-at p insertion-line-comment)))
                  (if res
                      (delete-character p (length res))
                      (loop :while (looking-at p line-comment)
                            :do (delete-character p (length line-comment))))))
              (unless (line-offset p 1) (return)))))))))

(define-attribute xref-headline-attribute
  (t :foreground :base07 :bold t))

(define-attribute xref-content-attribute
  (t :foreground :base0D :bold t))

(defun xref-insert-headline (headline point level)
  (insert-string point
                 (concatenate 'string
                              (make-string (* 2 level) :initial-element #\space)
                              (princ-to-string headline))
                 :attribute 'xref-headline-attribute)
  (insert-character point #\newline))

(defgeneric xref-insert-content (content point level)
  (:method (content point level)
    (xref-insert-content (princ-to-string content) point level))
  (:method ((content string) point level)
    (insert-string point
                   (concatenate 'string
                                (make-string (* 2 level) :initial-element #\space)
                                content)
                   :attribute 'xref-content-attribute)
    (insert-character point #\newline)))

(defstruct xref-position
  line-number
  charpos)

;; TODO
(defun make-position (line-number charpos)
  (make-xref-position :line-number line-number :charpos charpos))

(defun move-to-location-position (point position)
  (etypecase position
    (point
     (move-point point position))
    (xref-position
     (move-to-line point (xref-position-line-number position))
     (line-offset point 0 (xref-position-charpos position)))
    (integer
     (move-to-position point position))))

(defstruct xref-location
  (filespec nil :read-only t :type (or buffer string pathname))
  (position 1 :read-only t :type (or point xref-position integer))
  (content "" :read-only t))

(defstruct xref-references
  (type nil :read-only t)
  (locations nil :read-only t))

(defun xref-filespec-to-buffer (filespec &key temporary)
  (cond ((bufferp filespec)
         filespec)
        (t
         (assert (or (stringp filespec) (pathnamep filespec)))
         (when (probe-file filespec)
           (or (get-file-buffer (file-namestring filespec))
               (find-file-buffer filespec :temporary temporary))))))

(defun xref-filespec-to-filename (filespec)
  (etypecase filespec
    (buffer (buffer-filename filespec))
    (string filespec)
    (pathname (namestring filespec))))

(defun move-to-xref-location-position (point position)
  (etypecase position
    (integer
     (move-to-position point position))
    (xref-position
     (move-to-line point (xref-position-line-number position))
     (line-offset point 0 (xref-position-charpos position)))
    (point
     (let ((line-number (line-number-at-point position))
           (charpos (point-charpos position)))
       (move-to-line point line-number)
       (line-offset point 0 charpos)))))

(defun location-to-point (location &key temporary)
  (alexandria:when-let ((buffer (xref-filespec-to-buffer (xref-location-filespec location)
                                                         :temporary temporary)))
    (with-point ((point (buffer-point buffer)))
      (move-to-xref-location-position point (xref-location-position location))
      point)))

(defun go-to-location (location &optional set-buffer-fn)
  (let ((point (location-to-point location :temporary nil)))
    (unless point
      (editor-error "~A does not exist." (xref-location-filespec location)))
    (let ((buffer (point-buffer point)))
      (when set-buffer-fn
        (funcall set-buffer-fn buffer))
      (move-point (buffer-point buffer) point))))

(defgeneric location-position< (position1 position2)
  (:method ((position1 integer) (position2 integer))
    (< position1 position2))
  (:method ((position1 xref-position) (position2 xref-position))
    (or (< (xref-position-line-number position1)
           (xref-position-line-number position2))
        (and (= (xref-position-line-number position1)
                (xref-position-line-number position2))
             (< (xref-position-charpos position1)
                (xref-position-charpos position2)))))
  (:method ((position1 point) (position2 point))
    (point< position1 position2))
  (:method (position1 position2)
    nil))

(defun sort-xref-locations (locations)
  (stable-sort (copy-list locations)
               (lambda (location1 location2)
                 (flet ((filespec (location)
                          (xref-filespec-to-filename
                           (xref-location-filespec location))))
                   (or (string< (filespec location1) (filespec location2))
                       (and (string= (filespec location1) (filespec location2))
                            (location-position< (xref-location-position location1)
                                                (xref-location-position location2))))))))

(defun same-locations-p (locations)
  (let ((line-numbers
          (loop :for location :in locations
                :for point := (location-to-point location :temporary t)
                :when point
                :collect (line-number-at-point point))))
    (when line-numbers
      (apply #'= line-numbers))))

(defun display-xref-locations (locations)
  (unless locations
    (editor-error "No definitions found"))
  (push-location-stack (current-point))
  (setf locations (uiop:ensure-list locations))
  (cond ((same-locations-p locations)
         (go-to-location (first locations) #'switch-to-buffer)
         (lem/peek-source:highlight-matched-line (current-point)))
        (t
         (let ((prev-file nil))
           (lem/peek-source:with-collecting-sources (collector)
             (dolist (location (sort-xref-locations locations))
               (let ((file (xref-filespec-to-filename (xref-location-filespec location)))
                     (content (xref-location-content location)))
                 (unless (equal prev-file file)
                   (lem/peek-source:with-insert (point)
                     (xref-insert-headline file point 0)))
                 (lem/peek-source:with-appending-source
                     (point :move-function (let ((location location))
                                             (lambda ()
                                               (go-to-location location))))
                   (xref-insert-content content point 1))
                 (setf prev-file file))))))))

(define-command find-definitions (&optional (point (current-point))) ()
  (alexandria:when-let (fn (variable-value 'find-definitions-function :buffer point))
    (funcall fn point)))

(defun xref-references-length=1 (xref-references-list)
  (and (alexandria:length= 1 xref-references-list)
       (alexandria:length= 1 (xref-references-locations (first xref-references-list)))))

(defun display-xref-references (refs)
  (unless refs
    (editor-error "No references found"))
  (push-location-stack (current-point))
  (let ((refs (uiop:ensure-list refs)))
    (cond ((xref-references-length=1 refs)
           (go-to-location (first (xref-references-locations (first refs)))
                           #'switch-to-buffer)
           (lem/peek-source:highlight-matched-line (current-point)))
          (t
           (lem/peek-source:with-collecting-sources (collector)
             (dolist (ref refs)
               (let ((type (xref-references-type ref)))
                 (when type
                   (lem/peek-source:with-insert (point)
                     (xref-insert-headline type point 0)))
                 (let ((prev-file nil))
                   (dolist (location (sort-xref-locations (xref-references-locations ref)))
                     (let ((file (xref-filespec-to-filename (xref-location-filespec location)))
                           (content (xref-location-content location)))
                       (unless (equal prev-file file)
                         (lem/peek-source:with-insert (point)
                           (xref-insert-headline file point 1)))
                       (lem/peek-source:with-appending-source
                           (point :move-function (let ((location location))
                                                   (alexandria:curry #'go-to-location location)))
                         (xref-insert-content content point 2))
                       (setf prev-file file)))))))))))

(define-command find-references () ()
  (alexandria:when-let (fn (variable-value 'find-references-function :buffer))
    (funcall fn (current-point))))

(defvar *xref-stack-table* (make-hash-table :test 'equal))
(defvar *xref-history-table* (make-hash-table :test 'equal))

(defun buffer-language-mode (buffer)
  (or (variable-value 'language-mode-tag :buffer buffer)
      (buffer-major-mode buffer)))

(defun push-location-stack (point)
  (run-hooks *set-location-hook* point)
  (let* ((buffer (point-buffer point))
         (key (buffer-language-mode buffer))
         (elt (list (buffer-name buffer)
                    (line-number-at-point point)
                    (point-charpos point))))
    (setf (gethash key *xref-history-table*)
          (cons elt (delete elt (gethash key *xref-history-table*)
                            :test #'equal)))
    (push elt (gethash key *xref-stack-table*))))

(define-command pop-definition-stack () ()
  (let ((elt (pop (gethash (buffer-language-mode (current-buffer))
                           *xref-stack-table*))))
    (when elt
      (destructuring-bind (buffer-name line-number charpos) elt
        (unless (get-buffer buffer-name)
          (pop-definition-stack)
          (return-from pop-definition-stack))
        (run-hooks *set-location-hook* (current-point))
        (select-buffer buffer-name)
        (move-to-line (current-point) line-number)
        (line-offset (current-point) 0 charpos)
        (lem/peek-source:highlight-matched-line (current-point))))))

(define-command complete-symbol () ()
  (alexandria:when-let (completion (variable-value 'completion-spec :buffer))
    (lem/completion-mode:run-completion completion)))

(define-command indent-line-and-complete-symbol () ()
  (if (variable-value 'calc-indent-function :buffer)
      (let* ((p (current-point))
             (old (point-charpos p)))
        (let ((charpos (point-charpos p)))
          (handler-case (indent-line p)
            (editor-condition ()
              (line-offset p 0 charpos))))
        (when (= old (point-charpos p))
          (complete-symbol)))
      (complete-symbol)))

(define-command (insert-\(\)-or-wrap (:advice-classes editable-advice)) () ()
  (if (mark-active-p (cursor-mark (current-point)))
      (with-point ((start (cursor-region-beginning (current-point)))
                   (end (cursor-region-end (current-point))))
        (when (point< start (current-point))
          (exchange-point-mark))
        (insert-character end #\))
        (insert-character start #\())
      (let ((p (current-point)))
        (insert-character p #\()
        (insert-character p #\))
        (character-offset p -1))))

(defun backward-search-rper ()
  (save-excursion
    (do* ((p (character-offset (current-point) -1))
          (c (character-at p)
             (character-at p)))
        ((char= #\) c) p)
      (unless (syntax-space-char-p c)
        (return nil))
      (character-offset p -1))))

(defun backward-delete-to-rper ()
  (save-excursion
    (do* ((p (character-offset (current-point) -1))
          (c (character-at p)
             (character-at p)))
        ((char= #\) c) p)
      (unless (syntax-space-char-p c)
        (return nil))
      (delete-character p)
      (character-offset p -1))))

(define-command (move-over-\)-or-wrap (:advice-classes movable-advice editable-advice)) () ()
  (if (mark-active-p (cursor-mark (current-point)))
      (with-point ((start (cursor-region-beginning (current-point)))
                   (end (cursor-region-end (current-point))))
        (when (point> end (current-point))
          (exchange-point-mark))
        (insert-character end #\))
        (insert-character start #\())
      (let ((rper (backward-search-rper)))
        (if rper
            (progn
              (backward-delete-to-rper)
              (scan-lists (current-point) 1 1 T)
              (newline-and-indent 1))
            (progn
              (scan-lists (current-point) 1 1 T)
              (newline-and-indent 1))))))

(defun match-pattern-p (pattern file)
  (etypecase pattern
    (function (funcall pattern file))
    (string (search pattern file))))

(defun find-root-directory-1 (directory patterns)
  (labels ((matchp (directory)
             (dolist (pathname (list-directory directory))
               (dolist (pattern patterns)
                 (when (match-pattern-p pattern (file-namestring pathname))
                   (return-from matchp t)))))
           (recursive (directory)
             (cond ((matchp directory) directory)
                   ((uiop:pathname-equal directory (user-homedir-pathname)) nil)
                   (t (recursive (uiop:pathname-parent-directory-pathname directory))))))
    (recursive directory)))

(defun find-root-directory (directory root-uri-patterns)
  (or (find-root-directory-1 directory
                             (or root-uri-patterns
                                 (list (lambda (name) (string= name ".git")))))
      directory))
