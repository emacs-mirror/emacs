(defpackage :lem-lisp-mode/sldb
  (:use :cl
        :lem
        :lem/button
        :lem-lisp-mode/internal
        :lem-lisp-mode/ui-mode)
  (:import-from :lem-lisp-mode/message-dispatcher
                :define-message)
  (:export :topline-attribute
           :condition-attribute
           :section-attribute
           :restart-number-attribute
           :restart-type-attribute
           :restart-attribute
           :frame-label-attribute
           :local-name-attribute
           :local-value-attribute
           :catch-tag-attribute
           :*sldb-keymap*
           :*lisp-sldb-mode-hook*
           :sldb-down
           :sldb-up
           :sldb-details-down
           :sldb-details-up
           :sldb-quit
           :sldb-continue
           :sldb-abort
           :sldb-restart-frame
           :sldb-invoke-restart
           :sldb-invoke-restart-0
           :sldb-invoke-restart-1
           :sldb-invoke-restart-2
           :sldb-invoke-restart-3
           :sldb-invoke-restart-4
           :sldb-invoke-restart-5
           :sldb-invoke-restart-6
           :sldb-invoke-restart-7
           :sldb-invoke-restart-8
           :sldb-invoke-restart-9
           :sldb-invoke-restart-by-name
           :sldb-show-frame-source
           :sldb-eval-in-frame
           :sldb-pprint-eval-in-frame
           :sldb-inspect-in-frame
           :sldb-step
           :sldb-next
           :sldb-out
           :sldb-break-on-return
           :sldb-inspect-condition
           :sldb-print-condition
           :sldb-recompile-in-frame-source))
(in-package :lem-lisp-mode/sldb)

(define-attribute topline-attribute
  (t :bold t))

(define-attribute condition-attribute
  (t :foreground :base08 :bold t))

(define-attribute section-attribute
  (t :bold t :reverse t))

(define-attribute subsection-attribute
  (t :bold t :underline t))

(define-attribute restart-number-attribute
  (t :bold t))

(define-attribute restart-type-attribute
  (t :foreground :base09 :bold t))

(define-attribute restart-attribute
  (t :bold t))

(define-attribute frame-label-attribute
  (t :bold t))

(define-attribute local-name-attribute
  (t :bold t))

(define-attribute local-value-attribute
  (t :foreground :base0D :bold t))

(define-attribute catch-tag-attribute
  (t :bold t))

(define-major-mode sldb-mode lisp-ui-mode
    (:name "SLDB"
     :keymap *sldb-keymap*
     :mode-hook *lisp-sldb-mode-hook*))

(define-key *sldb-keymap* "n" 'sldb-down)
(define-key *sldb-keymap* "p" 'sldb-up)
(define-key *sldb-keymap* "M-n" 'sldb-details-down)
(define-key *sldb-keymap* "M-p" 'sldb-details-up)
(define-key *sldb-keymap* "q" 'sldb-quit)
(define-key *sldb-keymap* "M-q" 'sldb-quit)
(define-key *sldb-keymap* "c" 'sldb-continue)
(define-key *sldb-keymap* "a" 'sldb-abort)
(define-key *sldb-keymap* "r" 'sldb-restart-frame)
(define-key *sldb-keymap* "0" 'sldb-invoke-restart-0)
(define-key *sldb-keymap* "1" 'sldb-invoke-restart-1)
(define-key *sldb-keymap* "2" 'sldb-invoke-restart-2)
(define-key *sldb-keymap* "3" 'sldb-invoke-restart-3)
(define-key *sldb-keymap* "4" 'sldb-invoke-restart-4)
(define-key *sldb-keymap* "5" 'sldb-invoke-restart-5)
(define-key *sldb-keymap* "6" 'sldb-invoke-restart-6)
(define-key *sldb-keymap* "7" 'sldb-invoke-restart-7)
(define-key *sldb-keymap* "8" 'sldb-invoke-restart-8)
(define-key *sldb-keymap* "9" 'sldb-invoke-restart-9)
(define-key *sldb-keymap* "I" 'sldb-invoke-restart-by-name)
(define-key *sldb-keymap* "v" 'sldb-show-frame-source)
(define-key *sldb-keymap* "e" 'sldb-eval-in-frame)
(define-key *sldb-keymap* "d" 'sldb-pprint-eval-in-frame)
(define-key *sldb-keymap* "i" 'sldb-inspect-in-frame)
(define-key *sldb-keymap* "s" 'sldb-step)
(define-key *sldb-keymap* "x" 'sldb-next)
(define-key *sldb-keymap* "o" 'sldb-out)
(define-key *sldb-keymap* "b" 'sldb-break-on-return)
(define-key *sldb-keymap* "C" 'sldb-inspect-condition)
(define-key *sldb-keymap* "C-c C-c" 'sldb-recompile-in-frame-source)
(define-key *sldb-keymap* "M-Return" 'sldb-copy-down-to-repl)

(defun get-sldb-buffer (thread)
  (dolist (buffer (buffer-list))
    (when (eql thread (buffer-thread-id buffer))
      (return buffer))))

(defun get-sldb-buffer-create (thread)
  (or (get-sldb-buffer thread)
      (make-buffer (format nil "*sldb ~D*" thread))))

(defun frame-number (frame) (first frame))
(defun frame-string (frame) (second frame))
(defun frame-plist (frame) (third frame))

(defun prune-initial-frames (frames)
  (or (loop :for frame :in frames
            :for (n form) := frame
            :until (ppcre:scan (load-time-value
                                (ppcre:create-scanner
                                 "^(?:[() ]|lambda)*micros\\b"
                                 :case-insensitive-mode t))
                               form)
            :collect frame)
      frames))

(defun sldb-setup (thread level condition restarts frames conts)
  (let ((buffer (get-sldb-buffer-create thread)))
    (switch-to-window (pop-to-buffer buffer))
    (change-buffer-mode buffer 'sldb-mode)
    (setf (buffer-read-only-p buffer) nil)
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (setf (buffer-thread-id buffer)
          thread
          (buffer-value buffer 'level)
          level
          (buffer-value buffer 'condition)
          condition
          (buffer-value buffer 'restarts)
          restarts
          (buffer-value buffer 'continuations)
          conts)
    (erase-buffer buffer)
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
              (lambda (buffer)
                (declare (ignore buffer))
                (sldb-quit)))
    (let ((point (buffer-point buffer)))
      (sldb-insert-condition point condition)
      (insert-string point (format nil "~%Restarts:~%") :attribute 'section-attribute)
      (sldb-insert-restarts point restarts)
      (insert-string point (format nil "~%Backtrace:~%") :attribute 'section-attribute)
      (setf (buffer-value buffer 'backtrace-start-point)
            (copy-point point :right-inserting))
      (save-excursion
        (sldb-insert-frames point (prune-initial-frames frames) t)))
    (setf (buffer-read-only-p buffer) t)
    (lisp-ui-forward-button)))

(defun sldb-insert-condition (point condition)
  (destructuring-bind (message type extras) condition
    (declare (ignore extras))
    (insert-string point message :attribute 'topline-attribute)
    (insert-character point #\newline)
    (insert-string point type :attribute 'condition-attribute)
    (insert-character point #\newline)))

(defun sldb-insert-restarts (point restarts)
  (loop :for n :from 0
        :for (title description) :in restarts
        :do
           (insert-string point " ")
           (insert-string point (format nil "~D: " n) :attribute 'restart-number-attribute)
           (insert-button point (format nil "[~A] " title)
                          (let ((n n)) (lambda () (sldb-invoke-restart n)))
                          :attribute 'restart-type-attribute)
           (insert-string point description :attribute 'restart-attribute)
           (insert-character point #\newline)))

(defun sldb-insert-frames (point frames more)
  (dolist (frame frames)
    (sldb-insert-frame point frame))
  (when more
    (insert-button point " --more--"
                   (lambda () (with-context () (sldb-fetch-all-frames)))
                   :attribute 'section-attribute
                   :button-tag 'sldb-more-frames)))

(defun sldb-insert-frame (point frame)
  (let ((number (frame-number frame))
        (string (frame-string frame)))
    (insert-string point " ")
    (insert-string point (format nil "~2d:" number) :attribute 'frame-label-attribute)
    (insert-string point " ")
    (insert-button point
                   string
                   (lambda ()
                     (with-context ()
                       (sldb-toggle-details)))
                   'frame frame
                   :button-tag 'sldb-frame)
    (insert-character point #\newline)))

(defun sldb-fetch-all-frames ()
  (let ((*inhibit-read-only* t)
        (p (current-point)))
    (move-point p (buffer-value (current-buffer) 'backtrace-start-point))
    (delete-between-points p (buffer-end-point (current-buffer)))
    (save-excursion
      (sldb-insert-frames p (lisp-eval '(micros:backtrace 0 nil)) nil))))

(defun sldb-toggle-details (&optional on)
  (let* ((point (current-point))
         (frame-button (button-at point)))
    (when (and frame-button (button-get frame-button 'frame))
      (save-excursion
        (let ((*inhibit-read-only* t))
          (if (or on (not (button-get frame-button 'toggle)))
              (sldb-show-frame-details point frame-button)
              (sldb-hide-frame-details point frame-button)))))))

(defun sldb-show-frame-details (point frame-button)
  (unless (button-get frame-button 'toggle)
    (destructuring-bind (locals catches)
        (lisp-eval `(micros:frame-locals-and-catch-tags
                     ,(frame-number
                       (button-get frame-button 'frame))))
      (setf (button-get frame-button 'toggle) t)
      (move-point point (button-end frame-button))
      (let ((indent1 (make-string 7 :initial-element #\space))
            (indent2 (make-string 9 :initial-element #\space)))
        (insert-character point #\newline)
        (insert-string point indent1)
        (insert-string point (if locals "Locals:" "[No Locals]")
                       :attribute 'subsection-attribute)
        (loop :for i :from 0
              :for var :in locals
              :do (destructuring-bind (&key name id value) var
                    (insert-character point #\newline)
                    (insert-string point indent2)
                    (insert-button point (format nil "~A~A" name
                                                 (if (zerop id) "" (format nil "#~D" id)))
                                   (let ((var i)
                                         (frame-number (frame-number
                                                        (button-get frame-button 'frame))))
                                     (lambda ()
                                       (with-context ()
                                         (sldb-inspect-var frame-number var))))
                                   :attribute 'local-name-attribute
                                   'sldb-var i
                                   'frame (button-get frame-button 'frame)
                                   :button-tag 'sldb-var)
                    (insert-string point " = ")
                    (insert-string point value :attribute 'local-value-attribute)))
        (when catches
          (insert-character point #\newline)
          (insert-string point indent1)
          (insert-string point "Catch-tags:" :attribute 'subsection-attribute)
          (dolist (tag catches)
            (insert-character point #\newline)
            (insert-string point indent2)
            (insert-string point tag :attribute 'catch-tag-attribute)))))))

(defun sldb-inspect-var (frame-number var)
  (let ((window (current-window)))
    (lisp-eval-async `(micros:inspect-frame-var ,frame-number ,var)
                     (lambda (inspected-parts)
                       (with-current-window window
                         (lem-lisp-mode/inspector:open-inspector inspected-parts))))))

(defun sldb-hide-frame-details (point frame-button)
  (when (button-get frame-button 'toggle)
    (setf (button-get frame-button 'toggle) nil)
    (move-point point (button-end frame-button))
    (with-point ((start point))
      (character-offset start 1)
      (sldb-down point)
      (line-start point)
      (delete-between-points start point))))

(defun sldb-activate (thread level select)
  (let ((buffer (get-sldb-buffer thread)))
    (cond ((and buffer
                (= level (buffer-value buffer 'level -1)))
           (when select (pop-to-buffer buffer)))
          (t
           (sldb-reinitialize thread level)))))

(defun sldb-reinitialize (thread level)
  (with-remote-eval ('(micros:debugger-info-for-emacs 0 10)
                     :thread thread)
    (lambda (value)
      (alexandria:destructuring-ecase value
        ((:ok result)
         (apply #'sldb-setup thread level result))
        ((:abort _)
         (declare (ignore _)))))))

(defun sldb-exit (thread level stepping)
  (declare (ignore level))
  (let ((buffer (get-sldb-buffer thread)))
    (when buffer
      (cond (stepping
             (setf (buffer-value buffer 'level) nil)
             (start-timer (make-idle-timer
                           (lambda ()
                             (when (get-buffer (buffer-name buffer))
                               (let ((window (car (get-buffer-windows buffer))))
                                 (when (and window
                                            (not (deleted-window-p window))
                                            (not (buffer-value buffer 'level)))
                                   (quit-window window :kill-buffer t))))))
                          0))
            ((eq buffer (window-buffer (current-window)))
             (quit-window (current-window) :kill-buffer t))
            (t
             (kill-buffer buffer))))))

(define-command sldb-down (p) ((current-point))
  (next-single-property-change p 'sldb-frame)
  (when (end-line-p p)
    (or (next-single-property-change p 'sldb-frame)
        (next-single-property-change p 'sldb-more-frames))))

(define-command sldb-up (p) ((current-point))
  (let ((sp (buffer-value (current-buffer)
                          'backtrace-start-point)))
    (cond ((point< p sp)
           (move-point p sp))
          (t
           (previous-single-property-change p 'sldb-frame)
           (when (end-line-p p)
             (previous-single-property-change p 'sldb-frame))))))

(define-command sldb-details-down () ()
  (sldb-down (current-point))
  (sldb-toggle-details t))

(define-command sldb-details-up () ()
  (sldb-up (current-point))
  (sldb-toggle-details t))

(define-command sldb-quit () ()
  (with-remote-eval ('(micros:throw-to-toplevel))
    (lambda (value)
      (alexandria:destructuring-ecase
          value
        ((:ok x) (editor-error "sldb-quit returned [%s]" x))
        ((:abort _) (declare (ignore _)))))))

(define-command sldb-continue () ()
  (when (null (buffer-value (current-buffer) 'restarts))
    (error "continue called outside of debug buffer"))
  (with-remote-eval ('(micros:sldb-continue))
    (lambda (value)
      (alexandria:destructuring-case value
        ((:ok x)
         (editor-error "sldb-quit returned [~A]" x))))))

(define-command sldb-abort () ()
  (lisp-eval-async '(micros:sldb-abort)
                   (lambda (v)
                     (display-message "Restart returned: ~A" v))))

(defun frame-number-at-point (point)
  (or (when (text-property-at point 'sldb-frame)
        (frame-number (button-get (button-at point) 'frame)))
      (with-point ((start point))
        (and (previous-single-property-change start 'sldb-frame)
             (text-property-at start 'sldb-frame -1)
             (character-offset start -1)
             (frame-number (button-get (button-at start) 'frame))))
      (editor-error "No frame at point")))

(defun frame-var-number-at-point (point)
  (let* ((button (button-at point))
         (var (and button (button-get button 'sldb-var))))
    (or var
        (editor-error "No variable at point"))))

(define-command sldb-restart-frame (frame-number)
    ((frame-number-at-point (current-point)))
  (when frame-number
    (with-remote-eval (`(micros:restart-frame ,frame-number))
      (lambda (v)
        (alexandria:destructuring-ecase v
          ((:ok value) (display-message "~A" value))
          ((:abort _) (declare (ignore _))))))))

(defun sldb-invoke-restart (n)
  (check-type n integer)
  (with-remote-eval (`(micros:invoke-nth-restart-for-emacs
                       ,(buffer-value (current-buffer) 'level -1)
                       ,n))
    (lambda (x)
      (alexandria:destructuring-ecase x
        ((:ok value) (display-message "Restart returned: %s" value))
        ((:abort _) (declare (ignore _)))))))

(define-command sldb-invoke-restart-0 () () (sldb-invoke-restart 0))
(define-command sldb-invoke-restart-1 () () (sldb-invoke-restart 1))
(define-command sldb-invoke-restart-2 () () (sldb-invoke-restart 2))
(define-command sldb-invoke-restart-3 () () (sldb-invoke-restart 3))
(define-command sldb-invoke-restart-4 () () (sldb-invoke-restart 4))
(define-command sldb-invoke-restart-5 () () (sldb-invoke-restart 5))
(define-command sldb-invoke-restart-6 () () (sldb-invoke-restart 6))
(define-command sldb-invoke-restart-7 () () (sldb-invoke-restart 7))
(define-command sldb-invoke-restart-8 () () (sldb-invoke-restart 8))
(define-command sldb-invoke-restart-9 () () (sldb-invoke-restart 9))

(define-command sldb-invoke-restart-by-name (restart-name)
    ((:splice
      (let ((restarts (buffer-value (current-buffer) 'restarts)))
        (prompt-for-string "Restart:"
                           :completion-function (lambda (s) (completion s restarts))
                           :test-function (lambda (s)
                                            (member s restarts :test #'string-equal :key #'first))
                           :history-symbol 'sldb-restarts))))
  (sldb-invoke-restart
   (position restart-name
             (buffer-value (current-buffer) 'restarts)
             :test #'string-equal
             :key #'first)))

(define-command sldb-show-frame-source (frame-number)
    ((frame-number-at-point (current-point)))
  (lisp-eval-async `(micros:frame-source-location ,frame-number)
                   #'show-source-location))

(defun eval-form-for-frame (format-string)
  (let* ((frame (frame-number-at-point (current-point)))
         (pkg (lisp-eval `(micros:frame-package-name ,frame))))
    (list frame
          (let ((*current-package* pkg))
            (prompt-for-sexp (format nil format-string pkg)))
          pkg)))

(define-command sldb-eval-in-frame (frame string package)
    ((:splice (eval-form-for-frame "Eval in frame (~A)> ")))
  (lisp-eval-async `(micros:eval-string-in-frame ,string ,frame ,package)
                   (lambda (string)
                     (display-message "~A" string))))

(define-command sldb-pprint-eval-in-frame (frame string package)
    ((:splice (eval-form-for-frame "Eval in frame (~A)> ")))
  (lisp-eval-async `(micros:pprint-eval-string-in-frame ,string ,frame ,package)
                   #'write-string-to-repl))

(define-command sldb-inspect-in-frame (string)
    ((prompt-for-sexp "Inspect in frame (evaluated): "))
  (let ((frame-number (frame-number-at-point (current-point))))
    (lisp-eval-async `(micros:inspect-in-frame ,string ,frame-number)
                     'lem-lisp-mode/inspector:open-inspector)))

(define-command sldb-step () ()
  (lisp-eval-async `(micros:sldb-step ,(frame-number-at-point (current-point)))))

(define-command sldb-next () ()
  (lisp-eval-async `(micros:sldb-step ,(frame-number-at-point (current-point)))))

(define-command sldb-out () ()
  (lisp-eval-async `(micros:sldb-out ,(frame-number-at-point (current-point)))))

(define-command sldb-break-on-return (name)
    ((prompt-for-symbol-name "Function: "))
  (lisp-eval-async `(micros:sldb-break ,name)
                   (lambda (message)
                     (display-message "~A" message))))

(define-command sldb-inspect-condition () ()
  (lisp-eval-async '(micros:inspect-current-condition)
                   'lem-lisp-mode/inspector:open-inspector))

(define-command sldb-print-condition () ()
  (lisp-eval-async '(micros:sdlb-print-condition)
                   (lambda (message)
                     (display-message "~A" message))))

(defun recompile-location (source-location)
  (save-excursion
    (lem/language-mode:go-to-location source-location
                                      (lambda (buffer)
                                        (switch-to-window
                                              (pop-to-buffer buffer))))
    (lisp-compile-defun)))

(define-command sldb-recompile-in-frame-source () ()
  (lisp-eval-async `(micros:frame-source-location ,(frame-number-at-point (current-point)))
                   (lambda (source-location)
                     (alexandria:destructuring-case source-location
                       ((:error message)
                        (display-message "~A" message))
                       ((t &rest _)
                        (declare (ignore _))
                        (recompile-location source-location))))))

(define-command sldb-copy-down-to-repl () ()
  (copy-down-to-repl 'micros/backend:frame-var-value
                     (frame-number-at-point (current-point))
                     (frame-var-number-at-point (current-point))))

(defun source-location-to-point (source-location)
  (alexandria:destructuring-case source-location
    ((t &rest _)
     (declare (ignore _))
     (let ((xref-location (source-location-to-xref-location source-location)))
       (lem/language-mode::location-to-point xref-location :temporary nil)))))

(defun display-break-value-at-source-location (source-location value)
  (alexandria:when-let (point (source-location-to-point source-location))
    (with-point ((start point)
                 (end point))
      (scan-lists start -1 1)
      (form-offset (move-point end start) 1)
      (lem-lisp-mode/eval:redisplay-evaluated-message start end value))))

(define-message (:print-watching-value source-location value)
  (display-break-value-at-source-location source-location value))

(define-message (:debug thread level condition restarts frames conts)
  (cond ((and (consp condition)
              ;; see micros:watch macro
              (equal "!!! micros-break !!!" (first condition)))
         (with-remote-eval (`(micros:fetch-watching-value-and-continue) :thread thread)
           (lambda (value)
             (declare (ignore value)))))
        (t
         (sldb-setup thread level condition restarts frames conts))))

(define-message (:debug-return thread level stepping)
  (sldb-exit thread level stepping))
