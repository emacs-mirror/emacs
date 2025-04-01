(defpackage :lem-lisp-mode/highlight
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/highlight)

(defvar *timer* nil)

(define-attribute highlight-attribute
  (t :underline t :foreground "cyan"))

(define-overlay-accessors highlight-overlays
  :clear-function clear-highlight-overlays
  :add-function add-highlight-overlays)

(defun toplevel-form-p (point)
  (start-line-p point))

(defun compute-path-at-point (point)
  (with-point ((point point))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (loop :collect (loop :while (form-offset point -1) :count t)
          :until (or (null (backward-up-list point t))
                     (toplevel-form-p point)))))

(defun form-string-at-point (point)
  (with-point ((start point)
               (end point))
    (loop :while (backward-up-list start t))
    (loop :while (forward-up-list end t))
    (points-to-string start end)))

(defun move-path (point path)
  (loop :for n :in (reverse path)
        :do (forward-down-list point t)
            (form-offset point n))
  (skip-whitespace-forward point))

(defun highlight-symbol (point)
  (with-point ((start point)
               (end point))
    (form-offset end 1)
    (add-highlight-overlays (point-buffer point) (make-overlay start end 'highlight-attribute))))

(define-command lisp-highlight () ()
  (clear-highlight-overlays (current-buffer))
  (unless (syntax-space-char-p (character-at (current-point)))
    (lisp-eval-async `(micros/walker:highlight ,(form-string-at-point (current-point))
                                               ',(compute-path-at-point (current-point))
                                               ,(buffer-package (current-buffer)))
                     (lambda (result)
                       (alexandria:destructuring-ecase result
                         ((:read-error message)
                          (declare (ignore message)))
                         ((:error message)
                          (log:error message))
                         ((:ok paths)
                          (with-point ((start (current-point)))
                            (loop :while (backward-up-list start t))
                            (dolist (path paths)
                              (with-point ((point start))
                                (move-path point path)
                                (highlight-symbol point))))))))))

(defun init-highlight-timer ()
  (let ((timer (make-idle-timer 'lisp-highlight
                                :name "lisp-highlight"
                                :handle-function 'stop-highlight-timer)))
    (setf *timer* timer)
    (start-timer timer 100 :repeat t)))

(defun stop-highlight-timer ()
  (when *timer*
    (stop-timer *timer*)
    (setf *timer* nil)))

(define-command experimental/lisp-toggle-highlight () ()
  (if *timer*
      (stop-highlight-timer)
      (init-highlight-timer)))
