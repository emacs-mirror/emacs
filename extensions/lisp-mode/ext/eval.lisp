(defpackage :lem-lisp-mode/eval
  (:use :cl :lem :lem-lisp-mode/internal)
  (:import-from :lem-lisp-mode/inspector
                :lisp-inspect
                :open-inspector)
  (:import-from :lem-lisp-mode/connection
                :new-request-id
                :send-message
                :with-broadcast-connections)
  (:export :redisplay-evaluated-message))
(in-package :lem-lisp-mode/eval)

(define-attribute eval-error-attribute
  (t :foreground :base08 :bold t))

(define-attribute eval-value-attribute
  (t :foreground :base0D :bold t))

(define-key *lisp-mode-keymap* "C-x C-e" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "C-c C-e" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "C-c i" 'lisp-eval-interrupt-at-point)
(define-key *lisp-mode-keymap* "M-Return" 'lisp-eval-copy-down-to-repl)

(defun fold-one-line-message (message)
  (let ((pos (position #\newline message)))
    (if (not pos)
        message
        (format nil "~A..." (subseq message 0 pos)))))

(defun buffer-eval-result-overlays (buffer)
  (buffer-value buffer 'eval-result-overlays))

(defun (setf buffer-eval-result-overlays) (value buffer)
  (setf (buffer-value buffer 'eval-result-overlays) value))

(defun clear-eval-results (buffer)
  (mapc #'remove-eval-result-overlay
        (buffer-eval-result-overlays buffer)))

(defun overlay-eval-id (overlay)
  (overlay-get overlay :id))

(defun remove-eval-result-overlay (overlay)
  (let ((id (overlay-eval-id overlay)))
    (lisp-eval-async `(micros/pretty-eval:remove-evaluated-values ,id))
    (delete-overlay overlay)
    (when (overlay-get overlay 'relation-overlay)
      (delete-overlay (overlay-get overlay 'relation-overlay)))
    (alexandria:removef (buffer-eval-result-overlays (overlay-buffer overlay))
                        overlay)))

(defun find-overlays (start end &key including-after-point)
  (let ((buffer (point-buffer start)))
    (loop :for ov :in (buffer-eval-result-overlays buffer)
          :unless (if (point= (overlay-start ov)
                              (overlay-end ov))
                      (not (point<= start (overlay-start ov) end))
                      (or (point<= end (overlay-start ov))
                          (if including-after-point
                              (point< (overlay-end ov) start)
                              (point<= (overlay-end ov) start))))
          :collect ov)))

(defun find-overlay (point)
  (first (find-overlays point point :including-after-point t)))

(defun remove-eval-result-overlay-between (start end)
  (dolist (ov (find-overlays start end))
    (remove-eval-result-overlay ov)))

(defun remove-touch-overlay (start arg)
  (with-point ((end start))
    (character-offset end
                      (etypecase arg
                        (string (length arg))
                        (character 0)
                        (integer arg)))
    (remove-eval-result-overlay-between start end)))

;; copied from src/display.lisp, TODO: extract this utils
(defun compute-evaluated-background-color ()
  (alexandria:when-let (color (parse-color (background-color)))
    (multiple-value-bind (h s v)
        (rgb-to-hsv (color-red color)
                    (color-green color)
                    (color-blue color))
      (multiple-value-bind (r g b)
          (hsv-to-rgb h
                      s
                      (+ v (if (< v 50) 5 -5)))
        (color-to-hex-string (make-color r g b))))))

(defun display-evaluated-message
    (start
     end
     message
     &key is-error
          id
          attribute
          (background-attribute
           (make-attribute :background (compute-evaluated-background-color))))
  (let ((popup-overlay
          (make-line-endings-overlay
           start
           end
           (or attribute
               (if is-error
                   'eval-error-attribute
                   'eval-value-attribute))
           :start-point-kind :left-inserting
           :end-point-kind :right-inserting
           :text (fold-one-line-message message)
           :offset 1))
        (background-overlay
          (when background-attribute
            (make-overlay start
                          end
                          background-attribute
                          :start-point-kind :left-inserting
                          :end-point-kind :right-inserting)))
        (buffer (point-buffer start)))
    (overlay-put popup-overlay 'relation-overlay background-overlay)
    (overlay-put popup-overlay :id id)
    (push popup-overlay (buffer-eval-result-overlays buffer))
    (add-hook (variable-value 'before-change-functions :buffer buffer)
              'remove-touch-overlay)))

(defun redisplay-evaluated-message (start end value
                                    &rest args
                                    &key is-error attribute background-attribute)
  (declare (ignore is-error attribute background-attribute))
  (remove-eval-result-overlay-between start end)
  (apply #'display-evaluated-message start end value args))

(defun display-spinner-message (spinner &optional message is-error id)
  (lem/loading-spinner:with-line-spinner-points (start end spinner)
    (display-evaluated-message start end message :is-error is-error :id id)))

(defun spinner-eval-request-id (spinner)
  (lem/loading-spinner:spinner-value spinner 'eval-id))

(defun (setf spinner-eval-request-id) (eval-id spinner)
  (setf (lem/loading-spinner:spinner-value spinner 'eval-id) eval-id))

(defun eval-region (start end)
  (skip-whitespace-backward end)
  (remove-eval-result-overlay-between start end)
  (let ((spinner (lem/loading-spinner:start-loading-spinner :region :start start :end end))
        (string (points-to-string start end))
        (request-id (new-request-id)))
    (setf (spinner-eval-request-id spinner) request-id)
    (with-broadcast-connections (connection)
      (lem-lisp-mode/internal:with-remote-eval
          (`(micros/pretty-eval:pretty-eval ,string)
           :request-id request-id
           :connection connection)
        (lambda (value)
          (alexandria:destructuring-ecase value
            ((:ok result)
             (destructuring-bind (&key value id) result
               (lem/loading-spinner:stop-loading-spinner spinner)
               (display-spinner-message spinner value nil id)))
            ((:abort condition)
             (lem/loading-spinner:stop-loading-spinner spinner)
             (display-spinner-message spinner condition t))))))))

(defun eval-last-expression (point)
  (with-point ((start point)
               (end point))
    (form-offset start -1)
    (eval-region start end)))

(define-command lisp-eval-at-point () ()
  (check-connection)
  (cond ((buffer-mark-p (current-buffer))
         (with-point ((start (region-beginning (current-buffer)))
                      (end (region-end (current-buffer))))
           (eval-region start end)))
        (t
         (eval-last-expression (current-point)))))

(define-command lisp-eval-interrupt-at-point () ()
  (dolist (spinner (lem/loading-spinner:get-line-spinners (current-point)))
    (let ((request-id (spinner-eval-request-id spinner)))
      (with-broadcast-connections (connection)
        (send-message connection
                      `(:interrupt-thread ,request-id))))))

(defun get-evaluation-value-id-at-point (point)
  (alexandria:when-let* ((overlay (find-overlay point))
                         (id (overlay-eval-id overlay)))
    id))

(defmethod execute :around (mode (command lisp-inspect) argument)
  (alexandria:if-let ((id (get-evaluation-value-id-at-point (current-point))))
    (lisp-eval-async `(micros/pretty-eval:inspect-evaluation-value ,id)
                     'open-inspector)
    (call-next-method)))

(define-command lisp-eval-copy-down-to-repl () ()
  (let ((id (get-evaluation-value-id-at-point (current-point))))
    (copy-down-to-repl 'micros/pretty-eval:get-evaluation-value id)))

(define-command lisp-eval-clear () ()
  (clear-eval-results (current-buffer)))

(defun compute-context-menu-items ()
  (cons (lem/context-menu:make-item
         :label "Clear eval results"
         :callback (lambda (&rest args)
                     (declare (ignore args))
                     (lisp-eval-clear)))
        (when (get-evaluation-value-id-at-point (current-point))
          (list (lem/context-menu:make-item
                 :label "Copy evaluation value to repl"
                 :callback (lambda (&rest args)
                             (declare (ignore args))
                             (lisp-eval-copy-down-to-repl)))
                (lem/context-menu:make-item
                 :label "Inspect evaluation value"
                 :callback (lambda (&rest args)
                             (declare (ignore args))
                             (call-command 'lisp-inspect nil)))))))

(defun eval-print (string &optional print-right-margin)
  (let ((value (lisp-eval (if print-right-margin
                              `(let ((*print-right-margin* ,print-right-margin))
                                 (micros:eval-and-grab-output ,string))
                              `(micros:eval-and-grab-output ,string)))))
    (insert-string (current-point) (first value))
    (insert-character (current-point) #\newline)
    (insert-string (current-point) (second value))))

(define-command lisp-eval-last-expression-and-insert () ()
  (check-connection)
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (run-hooks (variable-value 'before-eval-functions) start end)
    (let ((string (points-to-string start end)))
      (eval-print string)
      (move-point (current-point) end))))

(define-command lisp-eval-region (start end) (:region)
  "Execute the region as Lisp code."
  (check-connection)
  (eval-with-transcript
   `(micros:interactive-eval-region
     ,(points-to-string start end))))

(define-command lisp-eval-buffer () ()
  "Execute the accessible portion of current buffer as Lisp code."
  (lisp-eval-region (buffer-start-point (current-buffer)) (buffer-end-point (current-buffer))))
