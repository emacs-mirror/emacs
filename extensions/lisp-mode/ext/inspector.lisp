(defpackage :lem-lisp-mode/inspector
  (:use :cl
        :lem
        :lem/button
        :lem-lisp-mode/internal
        :lem-lisp-mode/ui-mode)
  (:import-from :lem-lisp-mode/message-dispatcher
                :define-message)
  (:export :inspector-label-attribute
           :inspector-value-attribute
           :inspector-action-attribute
           :*inspector-limit*
           :*lisp-inspector-keymap*
           :open-inspector
           :lisp-inspect
           :lisp-inspector-pop
           :lisp-inspector-next
           :lisp-inspector-quit
           :lisp-inspector-describe
           :lisp-inspector-pprint
           :lisp-inspector-eval
           :lisp-inspector-history
           :lisp-inspector-show-source
           :lisp-inspector-reinspect
           :lisp-inspector-toggle-verbose
           :inspector-insert-more-button
           :lisp-inspector-fetch-all))
(in-package :lem-lisp-mode/inspector)

(define-attribute inspector-label-attribute)

(define-attribute inspector-value-attribute
  (t :foreground :base0D :bold t))

(define-attribute inspector-action-attribute
  (:dark :foreground :base09 :bold t))

(defvar *inspector-limit* 500)
(defvar *inspector-mark-stack* '())

(define-major-mode lisp-inspector-mode lisp-ui-mode
    (:name "Inspector"
     :keymap *lisp-inspector-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *lisp-mode-keymap* "C-c I" 'lisp-inspect)

(define-key *lisp-inspector-keymap* "l" 'lisp-inspector-pop)
(define-key *lisp-inspector-keymap* "n" 'lisp-inspector-next)
(define-key *lisp-inspector-keymap* "Space" 'lisp-inspector-next)
(define-key *lisp-inspector-keymap* "d" 'lisp-inspector-describe)
(define-key *lisp-inspector-keymap* "p" 'lisp-inspector-pprint)
(define-key *lisp-inspector-keymap* "e" 'lisp-inspector-eval)
(define-key *lisp-inspector-keymap* "h" 'lisp-inspector-history)
(define-key *lisp-inspector-keymap* "g" 'lisp-inspector-reinspect)
(define-key *lisp-inspector-keymap* "v" 'lisp-inspector-toggle-verbose)
(define-key *lisp-inspector-keymap* "." 'lisp-inspector-show-source)
(define-key *lisp-inspector-keymap* ">" 'lisp-inspector-fetch-all)
(define-key *lisp-inspector-keymap* "q" 'lisp-inspector-quit)
(define-key *lisp-inspector-keymap* "M-q" 'lisp-inspector-quit)
(define-key *lisp-inspector-keymap* "M-Return" 'lisp-inspector-copy-down-to-repl)

(define-command lisp-inspect (string &key (self-evaluation nil) (focus t))
    ((or (symbol-string-at-point (current-point))
         (prompt-for-sexp "Inspect value (evaluated): ")))
  (lisp-eval-async (if self-evaluation
                       `(micros:init-inspector
                         (format nil "(quote ~a)" ,string))
                       `(micros:init-inspector ,string))
                   (lambda (inspected-parts) (open-inspector inspected-parts nil nil focus))))

(defun inspector-buffer ()
  (or (get-buffer "*lisp-inspector*")
      (let ((buffer (make-buffer "*lisp-inspector*" :enable-undo-p nil)))
        (setf *inspector-mark-stack* '())
        (change-buffer-mode buffer 'lisp-inspector-mode)
        buffer)))

(defmethod open-inspector-by-repl (inspected-parts)
  (with-current-window (get-repl-window)
    (open-inspector inspected-parts)))

(defun open-inspector (inspected-parts &optional inspector-position hook focus)
  (let ((buffer (inspector-buffer)))
    (flet ((body ()
             (let ((point (current-point)))
               (when hook
                 (add-hook (variable-value 'kill-buffer-hook :buffer buffer) hook))
               (let ((*inhibit-read-only* t))
                 (erase-buffer buffer)
                 (destructuring-bind (&key id title content) inspected-parts
                   (insert-button point title
                                  (make-inspect-action :part id)
                                  'part id
                                  :attribute 'inspector-value-attribute)
                   (delete-between-points point (buffer-end-point buffer))
                   (insert-string point
                                  (format nil "~%--------------------~%")
                                  :attribute 'inspector-label-attribute)
                   (save-excursion
                     (inspector-insert-content content))))
               (when inspector-position
                 (move-to-line point (car inspector-position))
                 (line-offset point 0 (cdr inspector-position))))))
      (cond (focus
             (switch-to-window (pop-to-buffer buffer))
             (body))
            (t
             (with-current-window (pop-to-buffer buffer)
               (body)))))))

(defun inspector-insert-content (content)
  (inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((*inhibit-read-only* t))
       (inspector-insert-chunk chunk t t)))))

(defun inspector-insert-chunk (chunk prev next)
  (destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (inspector-insert-more-button start t))
    (mapc 'inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (inspector-insert-more-button end nil))))

(defun inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert-string (current-point) ispec)
      (alexandria:destructuring-ecase ispec
        ((:value string id)
         (insert-button (current-point) string
                        (make-inspect-action :part id)
                        'part id
                        :attribute 'inspector-value-attribute))
        ((:label string)
         (insert-string (current-point) string :attribute 'inspector-label-attribute))
        ((:action string id)
         (insert-button (current-point) string
                        (make-inspect-action :action id)
                        :attribute 'inspector-action-attribute)))))

(defun inspector-position (point)
  (cons (line-number-at-point point)
        (point-charpos point)))

(defun inspector-opener (parts)
  (when parts
    (open-inspector parts (inspector-position (current-point)))))

(defun inspector-new-opener (parts)
  (when parts
    (open-inspector parts)))

(defun make-inspect-action (type value)
  (lambda ()
    (ecase type
      ((:part)
       (lisp-eval-async `(micros:inspect-nth-part ,value)
                        'inspector-new-opener))
      ((:range)
       (inspector-fetch-more value))
      ((:action)
       (lisp-eval-async `(micros::inspector-call-nth-action ,value)
                        'inspector-opener)))))

(define-command lisp-inspector-pop () ()
  (lisp-eval-async `(micros:inspector-pop)
                   (lambda (result)
                     (cond (result
                            (open-inspector result (pop *inspector-mark-stack*)))
                           (t
                            (display-message "No previous object"))))))

(define-command lisp-inspector-next () ()
  (let ((result (lisp-eval `(micros:inspector-next))))
    (cond (result
           (push (inspector-position (current-point)) *inspector-mark-stack*)
           (open-inspector result))
          (t
           (display-message "No next object")))))

(define-command lisp-inspector-quit () ()
  (lisp-eval-async `(micros:quit-inspector))
  (quit-active-window t))

;; slime-find-inspectable-object
;; slime-inspector-next-inspectable-object
;; slime-inspector-previous-inspectable-object

(defun lisp-eval-describe (form)
  (lisp-eval-async form (lambda (string) (show-message string))))

(define-command lisp-inspector-describe () ()
  (lisp-eval-describe `(micros:describe-inspectee)))

(defun inspector-get-part ()
  (let* ((button (button-at (current-point)))
         (part (and button (button-get button 'part))))
    (unless part (editor-error "No part at point"))
    part))

(define-command lisp-inspector-pprint (part)
    ((inspector-get-part))
  (lisp-eval-describe `(micros:pprint-inspector-part ,part)))

(define-command lisp-inspector-eval (string)
    ((prompt-for-sexp "Inspector eval: "))
  (eval-with-transcript `(micros:inspector-eval ,string)))

(define-command lisp-inspector-history () ()
  (lisp-eval-describe `(micros:inspector-history)))

(define-command lisp-inspector-show-source (part)
    ((inspector-get-part))
  (lisp-eval-async `(micros:find-source-location-for-emacs '(:inspector ,part))
                   #'show-source-location))

(define-command lisp-inspector-reinspect () ()
  (lisp-eval-async '(micros:inspector-reinspect)
                   (let ((pos (inspector-position (current-point))))
                     (lambda (parts)
                       (open-inspector parts pos)))))

(define-command lisp-inspector-toggle-verbose () ()
  (lisp-eval-async `(micros:inspector-toggle-verbose)
                   (let ((pos (inspector-position (current-point))))
                     (lambda (parts)
                       (open-inspector parts pos)))))

(defun inspector-insert-more-button (index previous)
  (insert-button (current-point)
                 (format nil (if previous " [--more--]~%" " [--more--]"))
                 (make-inspect-action :range (cons index previous))
                 :attribute 'inspector-action-attribute))

(define-command lisp-inspector-fetch-all () ()
  (let ((button (button-at (character-offset (buffer-end (current-point)) -1))))
    (when button
      (let ((*inspector-limit*))
        (button-action button)))))

(defun inspector-fetch-more (index-previous-pair)
  (destructuring-bind (index . prev) index-previous-pair
    (inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (alexandria:rcurry
      (lambda (chunk prev)
        (let ((*inhibit-read-only* t))
          (let ((button (button-at (current-point))))
            (delete-between-points (button-start button) (button-end button))
            (inspector-insert-chunk chunk prev (not prev)))))
      prev))))

(defun inspector-fetch-chunk (chunk prev cont)
  (inspector-fetch chunk *inspector-limit* prev cont))

(defun inspector-fetch (chunk limit prev cont)
  (destructuring-bind (from to)
      (inspector-next-range chunk limit prev)
    (if (and from to)
        (lisp-eval-async `(micros:inspector-range ,from ,to)
                         (alexandria:rcurry (lambda (chunk2 chunk1 limit prev cont)
                                              (inspector-fetch
                                               (inspector-join-chunks chunk1 chunk2)
                                               limit prev cont))
                                            chunk limit prev cont))
        (funcall cont chunk))))

(defun inspector-next-range (chunk limit prev)
  (destructuring-bind (_ len start end) chunk
    (declare (ignore _))
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun inspector-join-chunks (chunk1 chunk2)
  (destructuring-bind (i1 _l1 s1 e1) chunk1
    (declare (ignore _l1))
    (destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(define-command lisp-inspector-copy-down-to-repl () ()
  (copy-down-to-repl 'micros:inspector-nth-part (inspector-get-part)))

(define-message (:inspect what thread tag)
  (let ((hook (when (and thread tag)
                (alexandria:curry (lambda (sexp)
                                    (lem-lisp-mode/connection:send-message-string
                                     (current-connection)
                                     sexp))
                                  `(:emacs-return ,thread ,tag nil)))))
    (open-inspector what nil hook)))
