(defpackage :lem/listener-mode
  (:use :cl :lem)
  (:export
   ;; keymap
   :*listener-mode-keymap*
   ;; functions
   :start-listener-mode
   :input-start-point
   :listener-start
   :change-input-start-point
   :refresh-prompt
   :clear-listener-using-mode
   :clear-listener
   ;; editor variables
   :listener-prompt-attribute
   :listener-set-prompt-function
   :listener-check-input-function
   :listener-execute-function
   ;; commands
   :listener-mode
   :listener-return
   :listener-previous-input
   :listener-next-input
   :listener-previous-startswith-input
   :listener-next-startswith-input
   :listener-previous-matching-input
   :listener-clear-buffer
   :listener-clear-input)
  #+sbcl
  (:lock t))
(in-package :lem/listener-mode)

(define-attribute listener-prompt-attribute
  (t :foreground :base0D :bold t))

(define-editor-variable listener-prompt-attribute 'listener-prompt-attribute)

(defun input-start-point (buffer)
  (buffer-value buffer '%input-start-point))

(defun set-input-start-point (buffer point)
  (setf (buffer-value buffer '%input-start-point) point))

(defun listener-history (buffer)
  (buffer-value buffer '%listener-history))

(defun (setf listener-history) (history buffer)
  (setf (buffer-value buffer '%listener-history) history))

(define-editor-variable listener-set-prompt-function)
(define-editor-variable listener-check-input-function)
(define-editor-variable listener-execute-function)

(define-minor-mode listener-mode
    (:name "Listener"
     :keymap *listener-mode-keymap*))

(define-key *listener-mode-keymap* "Return" 'listener-return)
(define-key *listener-mode-keymap* "M-p" 'listener-previous-input)
(define-key *listener-mode-keymap* "M-n" 'listener-next-input)
(define-key *listener-mode-keymap* "M-r" 'listener-isearch-history)
(define-key *listener-mode-keymap* "C-c M-o" 'listener-clear-buffer)
(define-key *listener-mode-keymap* "C-c C-u" 'listener-clear-input)

(defun start-listener-mode (&optional history-pathname)
  (listener-mode t)
  (unless (listener-history (current-buffer))
    (setf (listener-history (current-buffer))
          (lem/common/history:make-history :pathname history-pathname))
    (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer))
              'save-history))
  (add-hook *exit-editor-hook* 'save-all-histories)
  (unless (input-start-point (current-buffer))
    (change-input-start-point (current-point))))

(defun listener-buffer-p (buffer)
  (mode-active-p buffer 'listener-mode))

(defun save-history (buffer)
  (assert (listener-buffer-p buffer))
  (lem/common/history:save-file (listener-history buffer)))

(defun all-listener-buffers ()
  (remove-if-not #'listener-buffer-p (buffer-list)))

(defun save-all-histories ()
  (mapc #'save-history (all-listener-buffers)))

(defun current-listener-history ()
  (listener-history (current-buffer)))

(defun default-switch-to-buffer (buffer)
  (switch-to-window (pop-to-buffer buffer)))

(defun listener-start (buffer-name mode &key (switch-to-buffer-function 'default-switch-to-buffer))
  (let ((buffer (make-buffer buffer-name)))
    (funcall switch-to-buffer-function buffer)
    (funcall mode)
    (refresh-prompt buffer)))

(defun change-input-start-point (point)
  (check-type point point)
  (let ((buffer (point-buffer point)))
    (when (input-start-point buffer)
      (delete-point (input-start-point buffer)))
    (set-input-start-point buffer
                          (copy-point point :right-inserting))))

(defun write-prompt (point)
  (let ((buffer (point-buffer point)))
    (funcall (variable-value 'listener-set-prompt-function
                             :buffer buffer)
             point)
    (with-point ((s point))
      (line-start s)
      (let ((attribute (variable-value 'listener-prompt-attribute :default buffer)))
        (when attribute
          (put-text-property s point :sticky-attribute attribute)))
      (put-text-property s point :read-only t)
      (put-text-property s point :field t))))

(defun refresh-prompt (&optional (buffer (current-buffer)) (fresh-line t))
  (let ((point (buffer-point buffer)))
    (buffer-end point)
    (when fresh-line
      (unless (start-line-p point)
        (insert-character point #\newline 1)
        (buffer-end point)))
    (write-prompt point)
    (buffer-end point)
    (buffer-undo-boundary buffer)
    (change-input-start-point point)))

(define-command listener-return () ()
  (if (point< (current-point)
              (input-start-point (current-buffer)))
      (insert-character (current-point) #\newline)
      (with-point ((point (buffer-end (current-point)) :left-inserting))
        (if (not (funcall (variable-value 'listener-check-input-function) point))
            (insert-character point #\newline)
            (let ((start (input-start-point (current-buffer))))
              (unless (point<= start point)
                (refresh-prompt)
                (return-from listener-return))
              (let ((str (points-to-string start point)))
                (lem/common/history:add-history (current-listener-history) str)
                (buffer-end point)
                (insert-character point #\newline)
                (change-input-start-point (current-point))
                (funcall (variable-value 'listener-execute-function) point str)))))))

(defun replace-textarea (buffer str)
  (let ((start (input-start-point buffer))
        (end (buffer-end-point buffer)))
    (save-excursion
      (delete-between-points start end)
      (insert-string start str)
      (move-point (input-start-point buffer) start))
    (buffer-end (buffer-point buffer))))

(defun backup-edit-string (buffer)
  (lem/common/history:backup-edit-string
   (listener-history buffer)
   (points-to-string (input-start-point buffer)
                     (buffer-end-point buffer))))

(defun restore-edit-string (buffer)
  (multiple-value-bind (str win)
      (lem/common/history:restore-edit-string (listener-history buffer))
    (when win
      (replace-textarea buffer str))))

(define-command listener-previous-startswith-input () ()
  (block nil
    (let* ((buffer (current-buffer))
           (point (buffer-point buffer))
           (charpos (point-charpos point))
           (prefix (points-to-string (input-start-point buffer) point)))
      (backup-edit-string (current-buffer))
      (flet ((commit (str)
               (replace-textarea buffer str)
               (setf (point-charpos point) charpos)
               (return)))
        (loop
          (multiple-value-bind (str win)
              (lem/common/history:previous-history (current-listener-history))
            (if win
                (when (eql 0 (search prefix str :test #'string=))
                  (commit str))
                (return))))))))

(define-command listener-next-startswith-input () ()
  (block nil
    (let* ((buffer (current-buffer))
           (point (buffer-point buffer))
           (charpos (point-charpos point))
           (prefix (points-to-string (input-start-point buffer) point)))
      (backup-edit-string (current-buffer))
      (flet ((commit (str)
               (replace-textarea buffer str)
               (setf (point-charpos point) charpos)
               (return))
             (rollback ()
               (restore-edit-string buffer)
               (return)))
        (loop
          (multiple-value-bind (str win)
              (lem/common/history:next-history (current-listener-history))
            (if win
                (when (eql 0 (search prefix str :test #'string=))
                  (commit str))
                (rollback))))))))

(define-command listener-previous-input () ()
  (backup-edit-string (current-buffer))
  (multiple-value-bind (str win)
      (lem/common/history:previous-history (current-listener-history))
    (when win
      (replace-textarea (current-buffer) str))))

(define-command listener-next-input () ()
  (backup-edit-string (current-buffer))
  (multiple-value-bind (str win)
      (lem/common/history:next-history (current-listener-history))
    (if win
        (replace-textarea (current-buffer) str)
        (restore-edit-string (current-buffer)))))

(define-command listener-previous-matching-input (regexp)
    ((prompt-for-string "Previous element matching (regexp): "))
  (backup-edit-string (current-buffer))
  (multiple-value-bind (str win)
      (lem/common/history:previous-matching (current-listener-history) regexp)
    (when win
      (replace-textarea (current-buffer) str))))

(defmethod clear-listener-using-mode (mode buffer)
  (let ((*inhibit-read-only* t))
    (erase-buffer buffer))
  (refresh-prompt buffer))

(defun clear-listener (buffer)
  (clear-listener-using-mode (lem-core::get-active-modes-class-instance (current-buffer))
                             buffer))

(define-command listener-clear-buffer () ()
  (clear-listener (current-buffer)))

(define-command listener-clear-input () ()
  (delete-between-points (input-start-point (current-buffer))
                         (buffer-end-point (current-buffer))))

;;;
(define-attribute unmatch-isearch-attribute
  (t :reverse t))

(defvar *history-isearch-keymap* (make-keymap))
(define-key *history-isearch-keymap* "M-r" 'listener-isearch-history-previous)
(define-key *history-isearch-keymap* "C-r" 'listener-isearch-history-previous)
(define-key *history-isearch-keymap* "M-s" 'listener-isearch-history-next)
(define-key *history-isearch-keymap* "C-s" 'listener-isearch-history-next)

(defvar *listener-buffer*)
(defvar *listener-window*)
(defvar *history-window* nil)
(defvar *history-matched-index*)
(defvar *history-matched-string*)

(defun make-highlight-matches-buffer (target-string matches)
  (let* ((buffer (make-buffer nil :enable-undo-p nil :temporary t))
         (point (buffer-point buffer))
         (attribute (make-attribute :foreground "black" :background "SkyBlue1")))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (insert-string point target-string)
    (loop :for position :in matches
          :do (buffer-start point)
              (character-offset point position)
              (with-point ((start point)
                           (end point))
                (character-offset end 1)
                (put-text-property start end :sticky-attribute attribute)))
    (buffer-start point)
    buffer))

(defun redisplay-popup (buffer)
  (when *history-window*
    (delete-window *history-window*))
  (let* ((x (window-x *listener-window*))
         (y (+ (window-y *listener-window*)
               (window-cursor-y *listener-window*)
               1))
         (w (window-width *listener-window*))
         (h (alexandria:clamp (buffer-nlines buffer)
                              1
                              (max 1 (- (window-height *listener-window*)
                                        (window-cursor-y *listener-window*)
                                        2)))))
    (setf *history-window*
          (make-floating-window :buffer buffer
                                :x x
                                :y y
                                :width w
                                :height h))))

(defun isearch-continue (next-or-previous-matching)
  (let ((buffer *listener-buffer*))
    (multiple-value-bind (matched-string matched-index matches)
        (funcall next-or-previous-matching
                 (listener-history buffer))
      (cond ((null matched-string)
             (when *history-window*
               (let ((buffer (make-buffer nil :temporary t)))
                 (insert-string (buffer-point buffer) "Failed" :attribute 'unmatch-isearch-attribute)
                 (redisplay-popup buffer))))
            (t
             (redisplay-popup (make-highlight-matches-buffer matched-string matches))
             (setf *history-matched-index* matched-index)
             (setf *history-matched-string* matched-string))))))

(define-command listener-isearch-history-previous () ()
  (when *history-matched-index*
    (let ((input-string (get-prompt-input-string (current-window))))
      (isearch-continue
       (lambda (history)
         (lem/common/history::previous-matching-exclude-duplicates
          history
          input-string
          *history-matched-string*
          :start-index (1- *history-matched-index*)))))))

(define-command listener-isearch-history-next () ()
  (when *history-matched-index*
    (let ((input-string (get-prompt-input-string (current-window))))
      (isearch-continue
       (lambda (history)
         (lem/common/history::next-matching-exclude-duplicates
          history
          input-string
          *history-matched-string*
          :start-index (1+ *history-matched-index*)))))))

(define-command listener-isearch-history () ()
  (let ((buffer (current-buffer)))
    (buffer-end (buffer-point buffer))
    (let ((*listener-buffer* buffer)
          (*history-matched-index* nil)
          (*history-window* nil)
          (*history-matched-string* nil)
          (*listener-window* (current-window)))
      (unwind-protect
           (let ((lem/prompt-window::*fill-width* nil))
             (prompt-for-string
              "(reverse-i-search) "
              :special-keymap *history-isearch-keymap*
              :edit-callback (lambda (input-string)
                               (isearch-continue
                                (lambda (history)
                                  (lem/common/history:previous-matching history input-string))))
              :gravity :cursor
              :use-border nil)
             (when *history-matched-string*
               (replace-textarea buffer *history-matched-string*)))
        (when *history-window*
          (delete-window *history-window*))))))
