(defpackage :lem-lisp-mode/macroexpand
  (:use :cl
        :alexandria
        :lem
        :lem-lisp-mode/internal)
  (:export :lisp-macrostep-expand
           :lisp-macroexpand-all)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-mode/macroexpand)

(define-attribute expand-attribute
  (t :background :base01))

(define-attribute subform-attribute
  (t :underline :base07 :bold t))

(define-minor-mode macrostep-mode
    (:name "Macrostep"
     :keymap *macrostep-mode-keymap*
     :enable-hook 'enable-macrostep
     :disable-hook 'disable-macrostep))

(define-key *lisp-mode-keymap* "C-c Return" 'lisp-macrostep-expand)
(define-key *macrostep-mode-keymap* "q" 'lisp-macrostep-quit)
(define-key *macrostep-mode-keymap* "Tab" 'lisp-macrostep-next)
(define-key *macrostep-mode-keymap* "Shift-Tab" 'lisp-macrostep-previous)
(define-key *macrostep-mode-keymap* "Return" 'lisp-macrostep-expand-next)
(define-key *macrostep-mode-keymap* "Backspace" 'lisp-macrostep-undo)
(define-key *lisp-mode-keymap* "C-c M-m" 'lisp-macroexpand-all)

(defun enable-macrostep ()
  (setf (buffer-read-only-p (current-buffer)) t))

(defun disable-macrostep ()
  (setf (buffer-read-only-p (current-buffer)) nil)
  (clear-macrostep-overlays (current-buffer))
  (clear-expanded-overlays (current-buffer))
  (loop :while (pop-undo (current-buffer))))

(define-overlay-accessors subform-overlays
  :clear-function clear-macrostep-overlays
  :add-function add-subform-overlay)

(define-overlay-accessors expanded-overlays
  :clear-function clear-expanded-overlays
  :add-function add-expanded-overlay)

(defun make-subform-overlay (start end)
  (make-overlay start end 'subform-attribute))

(defun get-sorted-subform-overlays (buffer)
  (sort (copy-list (subform-overlays buffer))
        #'point<
        :key #'overlay-start))

(defun point-within-subform-p (point)
  (loop :for overlay :in (get-sorted-subform-overlays (point-buffer point))
        :when (point<= (overlay-start overlay) point (overlay-end overlay))
        :return t))

(defun search-next-subform-overlay (point)
  (loop :with overlays := (get-sorted-subform-overlays (point-buffer point))
        :for overlay :in overlays
        :when (point< point (overlay-start overlay))
        :return overlay
        :finally (return (first overlays))))

(defun search-previous-subform-overlay (point)
  (loop :for (overlay next-overlay) :on (get-sorted-subform-overlays (point-buffer point))
        :if (null next-overlay)
        :return overlay
        :if (point<= (overlay-end overlay) point (overlay-start next-overlay))
        :return overlay))

(defun remove-overlays-within-points (start end)
  (loop :with buffer := (point-buffer start)
        :for overlay :in (get-sorted-subform-overlays buffer)
        :if (point<= start (overlay-start overlay) (overlay-end overlay) end)
        :collect overlay :into garbage-overlays
        :else
        :collect overlay :into alive-overlays
        :finally (map () #'delete-overlay garbage-overlays)
                 (setf (subform-overlays buffer) alive-overlays)))

(defun dump-subforms (buffer)
  (loop :for overlay :in (subform-overlays buffer)
        :collect (cons (position-at-point (overlay-start overlay))
                       (position-at-point (overlay-end overlay)))))

(defun replace-at-points (start end string)
  (remove-overlays-within-points start end)
  (delete-between-points start end)
  (insert-string start string))

(defun positions-to-points (buffer start-pos end-pos)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (move-to-position start start-pos)
    (move-to-position end end-pos)
    (values start end)))

(defun empty-undo-stack-p (buffer)
  (null (buffer-value buffer 'undo)))

(defun pop-undo (buffer)
  (when (buffer-value buffer 'undo)
    (let ((*inhibit-read-only* t))
      (destructuring-bind (start-pos end-pos string subforms is-mark)
          (pop (buffer-value buffer 'undo))
        (multiple-value-bind (start end)
            (positions-to-points buffer start-pos end-pos)
          (replace-at-points start end string)
          (loop :for (start-pos . end-pos) :in subforms
                :do (multiple-value-bind (start end)
                        (positions-to-points buffer start-pos end-pos)
                      (add-subform-overlay buffer (make-subform-overlay start end))))
          (move-point (buffer-point buffer) start)
          (unless is-mark
            (buffer-unmark buffer)))))
    t))

(defun push-undo (start end string subforms is-mark)
  (let ((buffer (point-buffer start))
        (start-pos (position-at-point start))
        (end-pos (position-at-point end)))
    (push (list start-pos end-pos string subforms is-mark)
          (buffer-value buffer 'undo))))

(defun replace-with-macrostep-expand (start end expansion-string subform-info)
  (let ((*inhibit-read-only* t)
        (buffer (point-buffer start)))
    (replace-at-points start end expansion-string)
    (add-expanded-overlay buffer (make-overlay start end 'expand-attribute))
    (loop :for (name kind offset) :in subform-info
          :do (with-point ((point start))
                (character-offset point offset)
                (assert (forward-down-list point t))
                (with-point ((start point)
                             (end point))
                  (when (form-offset end 1)
                    (add-subform-overlay buffer (make-subform-overlay start end))))))
    (indent-points start end)))

(defun get-form-points (point)
  (maybe-beginning-of-string point)
  (unless (syntax-open-paren-char-p (character-at point))
    (backward-up-list point)
    (skip-chars-backward point #'syntax-expr-prefix-char-p))
  (values point
          (form-offset (copy-point point :temporary) 1)))

(defmacro with-form-points ((start end point) &body body)
  (check-type start symbol)
  (check-type end symbol)
  `(multiple-value-bind (,start ,end) (get-form-points ,point)
     (with-point ((,start ,start :right-inserting)
                  (,end ,end :left-inserting))
       ,@body)))

(defun get-context (point)
  (with-point ((start point)
               (end point))
    (loop :while (backward-up-list start t))
    (form-offset (move-point end start) 1)
    (list (points-to-string start point)
          (points-to-string point end))))

(defun macrostep-expand (point)
  (with-form-points (start end point)
    (let ((string (points-to-string start end))
          (context (get-context point)))
      (destructuring-ecase
          (lisp-eval `(micros/macrostep:macrostep-expand-1 ,string t ',context))
        ((:ok expansion-string subform-info)
         (let ((subforms (dump-subforms (point-buffer point)))
               (is-mark (buffer-modified-p (point-buffer point))))
           (replace-with-macrostep-expand start end expansion-string subform-info)
           (push-undo start end string subforms is-mark))
         (move-point point start)
         t)
        ((:error message)
         (show-message (format nil "Error: ~A" message))
         nil)))))

(defclass macrostep-advice () ())

(defmethod execute :before (mode (command macrostep-advice) argument)
  (unless (mode-active-p (current-buffer) 'macrostep-mode)
    (editor-error "macrostep is not activated.")))

(define-command (lisp-macrostep-quit (:advice-classes macrostep-advice)) () ()
  (macrostep-mode nil))

(define-command (lisp-macrostep-next (:advice-classes macrostep-advice)) () ()
  (when-let (overlay (search-next-subform-overlay (current-point)))
    (move-point (current-point) (overlay-start overlay))))

(define-command (lisp-macrostep-previous (:advice-classes macrostep-advice)) () ()
  (when-let (overlay (search-previous-subform-overlay (current-point)))
    (move-point (current-point) (overlay-start overlay))))

(define-command (lisp-macrostep-expand-next (:advice-classes macrostep-advice)) () ()
  (unless (point-within-subform-p (current-point))
    (lisp-macrostep-next))
  (macrostep-expand (current-point)))

(define-command (lisp-macrostep-undo (:advice-classes macrostep-advice)) () ()
  (pop-undo (current-buffer))
  (when (empty-undo-stack-p (current-buffer))
    (macrostep-mode nil)))

(defun display-help-p ()
  (not (config :disable-macrostep-display-help)))

(define-command lisp-macrostep-disable-help () ()
  (setf (config :disable-macrostep-display-help) t))

(define-command lisp-macrostep-expand () ()
  (when (macrostep-expand (current-point))
    (macrostep-mode t)
    (when (display-help-p)
      (when (prompt-for-y-or-n-p
             ;; TODO: Prepare help and guide them from here.
             "Press \"q\" to undo.
Do you want to disable this message in the future?"
             :gravity (make-instance 'lem/popup-window::gravity-cursor
                                     :offset-x 1
                                     :offset-y 1))
        (lisp-macrostep-disable-help)))))

(defun guard () (error 'read-only-error))
(defmethod execute ((mode macrostep-mode) (command undo) argument) (guard))
(defmethod execute ((mode macrostep-mode) (command redo) argument) (guard))

(defun macroexpand-internal (expander)
  (let* ((self (eq (current-buffer) (get-buffer "*lisp-macroexpand*")))
         (orig-package-name (buffer-package (current-buffer) "CL-USER"))
         (p (and self (copy-point (current-point) :temporary))))
    (lisp-eval-async `(,expander ,(lem-lisp-mode/internal::form-string-at-point))
                     (lambda (string)
                       (let ((buffer (make-buffer "*lisp-macroexpand*")))
                         (with-buffer-read-only buffer nil
                           (unless self (erase-buffer buffer))
                           (change-buffer-mode buffer 'lisp-mode)
                           (setf (buffer-package buffer) orig-package-name)
                           (when self
                             (move-point (current-point) p)
                             (kill-sexp))
                           (insert-string (buffer-point buffer)
                                          string)
                           (indent-points (buffer-start-point buffer)
                                          (buffer-end-point buffer))
                           (with-pop-up-typeout-window (s buffer)
                             (declare (ignore s)))
                           (when self
                             (move-point (buffer-point buffer) p))))))))

(define-command lisp-macroexpand-in-place () ()
  (check-connection)
  (lisp-eval-async
   `(micros:swank-macroexpand-1
     (lem-lisp-mode/internal::form-string-at-point))
   (lambda (string)
     (kill-sexp)
     (insert-string (current-point) string)
     (indent-buffer (current-buffer)))))

(define-command lisp-macroexpand () ()
  (check-connection)
  (macroexpand-internal 'micros:swank-macroexpand-1))

(define-command lisp-macroexpand-all () ()
  (check-connection)
  (macroexpand-internal 'micros:swank-macroexpand-all))
