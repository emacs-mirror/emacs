(in-package :lem-core)

(define-minor-mode typeout-mode
    (:name "typeout"
     :keymap *typeout-mode-keymap*
     :enable-hook (lambda ()
                    (setf (variable-value 'line-wrap :buffer (current-buffer)) nil))))

(define-key *typeout-mode-keymap* "q" 'dismiss-typeout-window)
(define-key *typeout-mode-keymap* "M-q" 'dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Space" 'next-page-or-dismiss-typeout-window)
(define-key *typeout-mode-keymap* "Backspace" 'previous-page)
(define-key *typeout-mode-keymap* 'delete-active-window 'dismiss-typeout-window)

(defvar *typeout-window* nil)
(defvar *typeout-before-window* nil)
(defvar *typeout-window-rewinding-values* nil)

(defun pop-up-typeout-window (buffer &key function erase (read-only t))
  (when (and *typeout-window*
             (not (eq buffer (window-buffer *typeout-window*))))
    (dismiss-typeout-window)
    (redraw-display))
  (with-buffer-read-only buffer nil
    (when erase
      (erase-buffer buffer))
    (when function
      (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
        (funcall function out)
        (fresh-line out))))
  (when read-only
    (setf (buffer-read-only-p buffer) t))
  (let* ((window-height
           (min (floor (display-height) 1.1) (1+ (buffer-nlines buffer))))
         (window
           (cond (*typeout-window*
                  (lem-core::window-set-size *typeout-window* (display-width) window-height)
                  *typeout-window*)
                 (t
                  (let ((typeout-buffer-p (buffer-value buffer 'typeout-buffer-p))
                        (not-switchable-buffer-p (not-switchable-buffer-p buffer)))
                    (setf *typeout-window-rewinding-values*
                          (list typeout-buffer-p
                                not-switchable-buffer-p)))
                  (let ((window (make-floating-window :buffer buffer
                                                      :x 0
                                                      :y 0
                                                      :width (display-width)
                                                      :height window-height
                                                      :use-modeline-p t)))
                    (setf (window-modeline-format window) '(typeout-window-modeline))
                    (add-hook (window-delete-hook window)
                              'delete-typeout-window-hook)
                    (add-hook (window-switch-to-buffer-hook window)
                              'typeout-window-switch-to-buffer-hook)
                    (setf *typeout-window* window
                          *typeout-before-window* (current-window))
                    window)))))
    (setf (buffer-value buffer 'typeout-buffer-p) t)
    ;; (setf (not-switchable-buffer-p buffer) t)
    (bury-buffer buffer)
    (setf (current-window) window)
    (typeout-mode t)
    (redraw-display)
    (values)))

(defun make-border-line (length)
  (with-output-to-string (out)
    (loop :repeat length :do (write-string " " out))))

(defun typeout-window-modeline (typeout-window)
  (values (let* ((posline (string-trim " " (modeline-posline typeout-window)))
                 (text (cond ((member posline '("All" "Bot") :test #'string=)
                              "Press Space to continue")
                             (t posline)))
                 (line (concatenate 'string
                                    (make-border-line (- (floor (display-width) 2)
                                                         (floor (length text) 2)
                                                         1))
                                    " "
                                    text
                                    " "))
                 (line (concatenate 'string
                                    line
                                    (make-border-line (- (display-width) (length text))))))
            line)
          (make-attribute :foreground (foreground-color) :background (background-color) :underline t)
          nil))

(defun delete-typeout-window-hook ()
  (setf *typeout-window* nil
        *typeout-before-window* nil))

(defun typeout-window-switch-to-buffer-hook (&rest args)
  (declare (ignore args))
  (dismiss-typeout-window))

(define-command dismiss-typeout-window () ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (typeout-mode nil)
    (when (deleted-window-p *typeout-before-window*)
      (setf *typeout-before-window* (first (window-list))))
    (setf (current-window) *typeout-before-window*)
    (when *typeout-window-rewinding-values*
      (let ((buffer (window-buffer *typeout-window*)))
        (destructuring-bind (typeout-buffer-p
                             not-switchable-buffer-p)
            *typeout-window-rewinding-values*
          (setf (buffer-value buffer 'typeout-buffer-p) typeout-buffer-p)
          (setf (not-switchable-buffer-p buffer) not-switchable-buffer-p))))
    (delete-window *typeout-window*)))

(define-command next-page-or-dismiss-typeout-window () ()
  (unless (deleted-window-p *typeout-window*)
    (setf (current-window) *typeout-window*)
    (move-point (current-point) (window-view-point (current-window)))
    (unless (line-offset (current-point) (window-height (current-window)))
      (dismiss-typeout-window))))

(add-hook *post-command-hook* 'dismiss-typeout-window-if-getout)
(defun dismiss-typeout-window-if-getout ()
  (when (and (not (mode-active-p (window-buffer (current-window)) 'typeout-mode))
             *typeout-window*)
    (dismiss-typeout-window)))
