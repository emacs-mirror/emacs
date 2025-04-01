(defpackage :lem/isearch
  (:use :cl :lem)
  (:export :handle-current-highlight
           :get-current-highlight-overlay
           :*isearch-keymap*
           :*isearch-finish-hooks*
           :isearch-mode
           :isearch-highlight-attribute
           :isearch-highlight-active-attribute
           :isearch-forward
           :isearch-backward
           :isearch-forward-regexp
           :isearch-backward-regexp
           :isearch-forward-symbol
           :isearch-backward-symbol
           :isearch-forward-symbol-at-point
           :isearch-abort
           :isearch-delete-char
           :isearch-raw-insert
           :isearch-end
           :isearch-finish
           :isearch-next
           :isearch-prev
           :isearch-yank
           :isearch-self-insert
           :isearch-replace-highlight
           :isearch-next-highlight
           :isearch-prev-highlight
           :isearch-toggle-highlighting
           :isearch-add-cursor-to-next-match
           :isearch-add-cursor-to-prev-match
           :read-query-replace-args
           :query-replace
           :query-replace-regexp
           :query-replace-symbol)
  #+sbcl
  (:lock t))
(in-package :lem/isearch)

(defvar *isearch-keymap* (make-keymap :name '*isearch-keymap*
                                      :undef-hook 'isearch-self-insert))
(defvar *isearch-prompt*)
(defvar *isearch-string*)
(defvar *isearch-previous-string* nil)
(defvar *isearch-start-point*)
(defvar *isearch-search-function*)
(defvar *isearch-search-forward-function*)
(defvar *isearch-search-backward-function*)
(defvar *isearch-popup-message* nil)
(defvar *isearch-finish-hooks* '())

(define-attribute isearch-highlight-attribute
  (t :foreground :base00 :background :base05))

(define-attribute isearch-highlight-active-attribute
  (t :foreground :base00 :background :base0D))

(define-editor-variable isearch-next-last nil)
(define-editor-variable isearch-prev-last nil)

(define-minor-mode isearch-mode
    (:name "Search"
     :keymap *isearch-keymap*
     :disable-hook 'disable-hook))

(define-key *global-keymap* "C-s" 'isearch-forward)
(define-key *global-keymap* "C-r" 'isearch-backward)
(define-key *global-keymap* "C-M-s" 'isearch-forward-regexp)
(define-key *global-keymap* "C-M-r" 'isearch-backward-regexp)
(define-key *global-keymap* "M-s _" 'isearch-forward-symbol)
(define-key *global-keymap* "M-s M-_" 'isearch-backward-symbol)
(define-key *global-keymap* "M-s ." 'isearch-forward-symbol-at-point)
(define-key *isearch-keymap* "C-g" 'isearch-abort)
(define-key *isearch-keymap* "C-h" 'isearch-delete-char)
(define-key *isearch-keymap* "Backspace" 'isearch-delete-char)
(define-key *isearch-keymap* "Delete" 'isearch-delete-char)
(define-key *isearch-keymap* "C-q" 'isearch-raw-insert)
(define-key *isearch-keymap* "C-j" 'isearch-finish)
(define-key *isearch-keymap* "Return" 'isearch-finish)
(define-key *isearch-keymap* "C-s" 'isearch-next)
(define-key *isearch-keymap* "C-r" 'isearch-prev)
(define-key *isearch-keymap* "C-y" 'isearch-yank)
(define-key *global-keymap* "F2" 'isearch-replace-highlight)
(define-key *global-keymap* "M-s M-n" 'isearch-next-highlight)
(define-key *global-keymap* "M-s n" 'isearch-next-highlight)
(define-key *global-keymap* "M-s M-p" 'isearch-prev-highlight)
(define-key *global-keymap* "M-s p" 'isearch-prev-highlight)
(define-key *global-keymap* "F3" 'isearch-next-highlight)
(define-key *global-keymap* "Shift-F3" 'isearch-prev-highlight)
(define-key *global-keymap* "M-s t" 'isearch-toggle-highlighting)
(define-key *global-keymap* "M-s M-t" 'isearch-toggle-highlighting)
(define-key *isearch-keymap* "C-M-n" 'isearch-add-cursor-to-next-match)
(define-key *isearch-keymap* "C-M-p" 'isearch-add-cursor-to-prev-match)

(defun disable-hook ()
  (setf (variable-value 'isearch-next-last :buffer) nil)
  (setf (variable-value 'isearch-prev-last :buffer) nil)
  (delete-popup-message *isearch-popup-message*))


(defun isearch-default-string ()
  (unless *isearch-previous-string*
    (setf *isearch-previous-string* (config :isearch-previous-string "")))
  *isearch-previous-string*)

(defun change-previous-string (string)
  (setf (config :isearch-previous-string) string
        *isearch-previous-string* string))


(defgeneric handle-current-highlight (mode overlay))

(defmethod handle-current-highlight (mode overlay)
  nil)

(defun on-current-highlight (overlay)
  (handle-current-highlight
   (lem-core::get-active-modes-class-instance (point-buffer (overlay-start overlay)))
   overlay)
  overlay)

(defun get-current-highlight-overlay (buffer)
  (dolist (ov (buffer-value buffer 'isearch-overlays))
    (when (attribute-equal (overlay-attribute ov) 'isearch-highlight-active-attribute)
      (return ov))))


(defun isearch-overlays (buffer)
  (buffer-value buffer 'isearch-overlays))

(defun isearch-reset-overlays (buffer)
  (mapc #'delete-overlay (buffer-value buffer 'isearch-overlays))
  (setf (buffer-value buffer 'isearch-overlays) '()))

(defun isearch-add-overlay (buffer overlay)
  (push overlay (buffer-value buffer 'isearch-overlays)))

(defun isearch-sort-overlays (buffer)
  (setf (buffer-value buffer 'isearch-overlays)
        (sort (buffer-value buffer 'isearch-overlays) #'point< :key #'overlay-start)))

(defun isearch-visible-overlays (buffer)
  (not (null (buffer-value buffer 'isearch-overlays))))

(defun isearch-next-overlay-point (point)
  (dolist (ov (buffer-value point 'isearch-overlays))
    (when (point< point (overlay-start ov))
      (return (overlay-start ov)))))

(defun isearch-prev-overlay-point (point)
  (let ((prev))
    (dolist (ov (buffer-value point 'isearch-overlays)
                (when prev
                  (overlay-start prev)))
      (when (point<= point (overlay-start ov))
        (return (overlay-start prev)))
      (setf prev ov))))

(defun isearch-current-overlay (point)
  (dolist (ov (buffer-value point 'isearch-overlays))
    (when (point<= (overlay-start ov) point (overlay-end ov))
      (return ov))))

(defun activate-current-highlight (point)
  (alexandria:when-let (ov (isearch-current-overlay point))
    (dolist (ov (buffer-value point 'isearch-overlays))
      (set-overlay-attribute 'isearch-highlight-attribute ov))
    (set-overlay-attribute 'isearch-highlight-active-attribute ov)
    (on-current-highlight ov)))


(defun isearch-update-buffer (&optional (point (current-point))
                                        (search-string *isearch-string*))
  (let ((buffer (point-buffer point)))
    (isearch-reset-overlays buffer)
    (unless (equal search-string "")
      (dolist (window (get-buffer-windows buffer :include-floating-windows t))
        (with-point ((curr (window-view-point window))
                     (limit (window-view-point window)))
          (unless (line-offset limit (window-height window))
            (setf limit nil))
          (loop :with prev
                :do (when (and prev (point= prev curr)) (return))
                    (setf prev (copy-point curr :temporary))
                    (unless (funcall *isearch-search-forward-function*
                                     curr search-string limit)
                      (return))
                    (with-point ((before curr))
                      (unless (funcall *isearch-search-backward-function*
                                       before search-string prev)
                        (return))
                      (when (point= before curr)
                        (return))
                      (let* ((active (and (point<= before point)
                                          (point<= point curr)))
                             (ov (make-overlay before
                                               curr
                                               (if active
                                                   'isearch-highlight-active-attribute
                                                   'isearch-highlight-attribute))))
                        (isearch-add-overlay buffer ov)
                        (when active
                          (on-current-highlight ov)))))))
      (isearch-sort-overlays buffer))))

(defun highlight-region (start-point end-point search-string)
  (let ((end-point (or end-point
                       (buffer-end-point (point-buffer start-point)))))
    (assert (point<= start-point end-point))
    (let ((buffer (point-buffer start-point)))
      (isearch-reset-overlays buffer)
      (when (string= search-string "")
        (return-from highlight-region nil))
      (with-point ((p start-point))
        (loop 
          (unless (funcall *isearch-search-forward-function* p search-string end-point)
            (return))
          (when (point= start-point p)
            (return))
          (with-point ((before p))
            (funcall *isearch-search-backward-function* before search-string)
            (isearch-add-overlay buffer
                                 (make-overlay before p 'isearch-highlight-attribute)))))
      (isearch-sort-overlays buffer))))

(defun isearch-update-display ()
  (isearch-update-message)
  (window-see (current-window))
  (isearch-update-buffer))

(defun isearch-update-message ()
  (setf *isearch-popup-message*
        (display-popup-message (format nil
                                       "~A~A~A"
                                       (if (or (variable-value 'isearch-next-last :buffer)
                                               (variable-value 'isearch-prev-last :buffer))
                                           "Failing "
                                           "")
                                       *isearch-prompt*
                                       *isearch-string*)
                               :timeout nil
                               :style '(:gravity :topright)
                               :destination-window *isearch-popup-message*)))

(defun make-add-char-callback (search-function)
  (lambda (point string)
    (alexandria:when-let (p (funcall search-function
                                     (copy-point *isearch-start-point* :temporary)
                                     string))
      (move-point point p))))

(define-command isearch-forward (&optional prompt) ()
  (isearch-start
   (or prompt "ISearch: ")
   (make-add-char-callback #'search-forward)
   #'search-forward
   #'search-backward
   ""))

(define-command isearch-backward (&optional prompt) ()
  (isearch-start
   (or prompt "ISearch: ")
   (make-add-char-callback #'search-backward)
   #'search-forward
   #'search-backward
   ""))

(define-command isearch-forward-regexp (&optional prompt) ()
  (isearch-start (or prompt "ISearch Regexp: ")
                 (make-add-char-callback #'search-forward-regexp)
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-command isearch-backward-regexp (&optional prompt) ()
  (isearch-start (or prompt "ISearch Regexp: ")
                 (make-add-char-callback #'search-backward-regexp)
                 #'search-forward-regexp
                 #'search-backward-regexp
                 ""))

(define-command isearch-forward-symbol (&optional prompt) ()
  (isearch-start (or prompt "ISearch Symbol: ")
                 #'search-forward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-command isearch-backward-symbol (&optional prompt) ()
  (isearch-start (or prompt "ISearch Symbol: ")
                 #'search-backward-symbol
                 #'search-forward-symbol
                 #'search-backward-symbol
                 ""))

(define-command isearch-forward-symbol-at-point () ()
  (let ((point (current-point)))
    (skip-chars-forward point #'syntax-symbol-char-p)
    (skip-chars-backward point (complement #'syntax-symbol-char-p))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (with-point ((start point))
      (skip-chars-forward point #'syntax-symbol-char-p)
      (with-point ((end point))
        (isearch-start "ISearch Symbol: "
                       #'search-forward-symbol
                       #'search-forward-symbol
                       #'search-backward-symbol
                       (points-to-string start end))))))

(defun isearch-start (prompt
                      search-func
                      search-forward-function
                      search-backward-function
                      initial-string)
  (run-hooks *set-location-hook* (current-point))
  (isearch-mode t)
  (setq *isearch-prompt* prompt)
  (setq *isearch-string* initial-string)
  (setq *isearch-search-function* search-func)
  (setq *isearch-start-point* (copy-point (current-point) :temporary))
  (setq *isearch-search-forward-function* search-forward-function)
  (setq *isearch-search-backward-function* search-backward-function)
  (isearch-update-display)
  t)

(define-command isearch-abort () ()
  (when (null (buffer-fake-cursors (current-buffer)))
    (move-point (current-point) *isearch-start-point*))
  (isearch-reset-overlays (current-buffer))
  (isearch-end)
  t)

(define-command isearch-delete-char () ()
  (when (plusp (length *isearch-string*))
    (setq *isearch-string*
          (subseq *isearch-string*
                  0
                  (1- (length *isearch-string*))))
    (funcall *isearch-search-function* (current-point) *isearch-string*)
    (isearch-update-display)))

(define-command isearch-raw-insert () ()
  (alexandria:when-let ((char (key-to-char (read-key))))
    (isearch-add-char char)))

(defun isearch-end ()
  (when (boundp '*isearch-string*)
    (isearch-reset-overlays (current-buffer))
    (change-previous-string *isearch-string*)
    (buffer-unbound (current-buffer) 'isearch-redisplay-string)
    (remove-hook (variable-value 'after-change-functions :buffer)
                 'isearch-change-buffer-hook)
    (isearch-mode nil)
    t))

(defun isearch-redisplay-inactive (buffer)
  (alexandria:when-let ((string (buffer-value buffer 'isearch-redisplay-string)))
    (isearch-update-buffer (buffer-start-point buffer) string)))

(defun isearch-scroll-hook (window)
  (isearch-redisplay-inactive (window-buffer window)))

(defun isearch-change-buffer-hook (start &rest args)
  (declare (ignore args))
  (isearch-redisplay-inactive (point-buffer start)))

(defun isearch-add-hooks ()
  (add-hook *window-scroll-functions*
            'isearch-scroll-hook)
  (add-hook (variable-value 'after-change-functions :buffer)
            'isearch-change-buffer-hook))

(define-command isearch-finish () ()
  (setf (buffer-value (current-buffer) 'isearch-redisplay-string) *isearch-string*)
  (change-previous-string *isearch-string*)
  (isearch-add-hooks)
  (run-hooks *isearch-finish-hooks* *isearch-string*)
  (isearch-redisplay-inactive (current-buffer))
  (isearch-mode nil))

(define-command isearch-next () ()
  (when (and (boundp '*isearch-string*)
             (boundp '*isearch-search-forward-function*))
    (when (string= "" *isearch-string*)
      (setq *isearch-string* (isearch-default-string)))
    (setf (variable-value 'isearch-prev-last :buffer) nil)
    (prog1
        (cond ((variable-value 'isearch-next-last :buffer)
               (setf (variable-value 'isearch-next-last :buffer) nil)
               (with-point ((p (current-point)))
                 (buffer-start p)
                 (if (funcall *isearch-search-forward-function* p *isearch-string*)
                     (progn
                       (move-point (current-point) p)
                       t)
                     nil)))
              ((not (funcall *isearch-search-forward-function* (current-point) *isearch-string*))
               (setf (variable-value 'isearch-next-last :buffer) t)
               nil)
              (t))
      (isearch-update-display))))

(define-command isearch-prev () ()
  (when (and (boundp '*isearch-string*)
             (boundp '*isearch-search-backward-function*))
    (when (string= "" *isearch-string*)
      (setq *isearch-string* (isearch-default-string)))
    (setf (variable-value 'isearch-next-last :buffer) nil)
    (prog1
        (cond ((variable-value 'isearch-prev-last :buffer)
               (setf (variable-value 'isearch-prev-last :buffer) nil)
               (with-point ((p (current-point)))
                 (buffer-end p)
                 (if (funcall *isearch-search-backward-function* p *isearch-string*)
                     (progn
                       (move-point (current-point) p)
                       t)
                     nil)))
              ((not (funcall *isearch-search-backward-function* (current-point) *isearch-string*))
               (setf (variable-value 'isearch-prev-last :buffer) t)
               nil)
              (t))
      (isearch-update-display))))

(define-command isearch-yank () ()
  (let ((str (yank-from-clipboard-or-killring)))
    (when str
      (setq *isearch-string* str)
      (funcall *isearch-search-function* (current-point) *isearch-string*)
      (isearch-update-display))))

(defun isearch-add-char (c)
  (setq *isearch-string*
        (concatenate 'string
                     *isearch-string*
                     (string c)))
  (unless (funcall *isearch-search-function* (current-point) *isearch-string*)
    (move-point (current-point) *isearch-start-point*))
  (isearch-update-display)
  t)

(define-command isearch-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (isearch-add-char c))
          (t (isearch-update-display)
             (unread-key-sequence (last-read-key-sequence))
             (isearch-end)))))

(define-command isearch-replace-highlight () ()
  (let ((buffer (current-buffer)))
    (let ((old-string (buffer-value buffer 'isearch-redisplay-string)))
      (unless old-string
        (return-from isearch-replace-highlight))
      (let ((new-string (prompt-for-string "Replace: " :initial-value old-string)))
        (save-excursion
          (unless (buffer-mark-p buffer) (buffer-start (current-point)))
          (query-replace-internal old-string
                                  new-string
                                  *isearch-search-forward-function*
                                  *isearch-search-backward-function*
                                  :query nil))))))

(defun search-next-matched (point n)
  (alexandria:when-let ((string (or (buffer-value (current-buffer) 'isearch-redisplay-string)
                                    (and (boundp '*isearch-string*)
                                         *isearch-string*))))
    (let ((search-fn
            (if (plusp n)
                *isearch-search-forward-function*
                *isearch-search-backward-function*))
          (search-previous-fn
            (if (plusp n)
                *isearch-search-backward-function*
                *isearch-search-forward-function*)))
      (with-point ((start point)
                   (end point))
        (funcall search-fn end string)
        (funcall search-previous-fn (move-point start end) string)
        (when (point< end point)
          (move-point point end)))
      (dotimes (_ (abs n) point)
        (unless (funcall search-fn point string)
          (return nil))))))

(define-command isearch-next-highlight (n) (:universal)
  (cond ((zerop n) nil)
        ((minusp n) (search-next-matched (current-point) (1+ n)))
        (t (search-next-matched (current-point) n))))

(define-command isearch-prev-highlight (n) (:universal)
  (isearch-next-highlight (- n)))

(define-command isearch-toggle-highlighting () ()
  (cond
    ((isearch-overlays (current-buffer))
     (isearch-end))
    ((boundp '*isearch-string*)
     (isearch-update-buffer))))

(defun determine-start-end-current-search (point string is-forward)
  (with-point ((start point)
               (end point))
    (if is-forward
        (progn
          (funcall *isearch-search-forward-function* end string)
          (funcall *isearch-search-backward-function* (move-point start end) string))
        (progn
          (funcall *isearch-search-backward-function* start string)
          (funcall *isearch-search-forward-function* (move-point end start) string)))
    (list start end)))

(defun mark-by-direction (is-forward buffer)
  (alexandria:when-let* ((string (or (buffer-value (current-buffer) 'isearch-redisplay-string)
                                     (and (boundp '*isearch-string*)
                                          *isearch-string*))))
    (let* ((cur-p (current-point))
           (start-end (determine-start-end-current-search cur-p string is-forward))
           (start (first start-end))
           (end (second start-end))
           (offset-pos (cond
                         ((and (point= start cur-p) is-forward) (length string))
                         ((and (point= end cur-p) (not is-forward)) (* -1 (length string)))
                         (t 0)))
           (cursors (buffer-cursors buffer))
           (sorted-cursors (if is-forward (reverse cursors) cursors))
           (cursor (first sorted-cursors)))
      (with-point ((point cursor))
        (character-offset point offset-pos)
        (if (search-next-matched point (if is-forward 1 0))
            (progn
              (character-offset point (* -1 offset-pos))
              (make-fake-cursor point)
              (message "Mark set ~A" (+ (length cursors) 1)))
            (message "No more matches found"))))))

(define-command isearch-add-cursor-to-next-match () ()
  (mark-by-direction t (current-buffer)))

(define-command isearch-add-cursor-to-prev-match () ()
  (mark-by-direction nil (current-buffer)))


(defvar *replace-before-string* nil)
(defvar *replace-after-string* nil)

(defun read-query-replace-args ()
  (let ((before)
        (after))
    (setq before
          (prompt-for-string
           (if *replace-before-string*
               (format nil "Before (~a with ~a): "
                       *replace-before-string*
                       *replace-after-string*)
               "Before: ")))
    (when (equal "" before)
      (cond (*replace-before-string*
             (setq before *replace-before-string*)
             (setq after *replace-after-string*)
             (return-from read-query-replace-args
               (list before after)))
            (t
             (message "Before string is empty")
             (return-from read-query-replace-args
               (list nil nil)))))
    (setq after (prompt-for-string "After: "))
    (setq *replace-before-string* before)
    (setq *replace-after-string* after)
    (list before after)))

(defun query-replace-internal-body (cur-point goal-point before after query count)
  (let ((pass-through (not query)))
    (with-point ((cur-point cur-point :left-inserting))
      (highlight-region cur-point goal-point before)
      (loop
        :repeat (or count most-positive-fixnum)
        :do (when (or (not (funcall *isearch-search-forward-function* cur-point before))
                      (and goal-point (point< goal-point cur-point)))
              (when goal-point
                (move-point (current-point) goal-point))
              (return))
            (with-point ((end cur-point :right-inserting))
              (funcall *isearch-search-backward-function* cur-point before)
              (with-point ((start cur-point :right-inserting))
                (loop :for c := (unless pass-through
                                  (save-excursion
                                    (move-point (current-point) cur-point)
                                    (activate-current-highlight cur-point)
                                    (redraw-display)
                                    (prompt-for-character (format nil "Replace ~s with ~s [y/n/!]" before after))))
                      :do (cond
                            ((or pass-through (char= c #\y))
                             (delete-between-points start end)
                             (insert-string cur-point after)
                             (return))
                            ((char= c #\n)
                             (move-point cur-point end)
                             (return))
                            ((char= c #\!)
                             (setf pass-through t))))))))))

(defun query-replace-internal (before after search-forward-function search-backward-function
                               &key query (start nil start-p) (end nil end-p) count)
  (let ((buffer (current-buffer)))
    (unwind-protect
         (let ((*isearch-search-forward-function* search-forward-function)
               (*isearch-search-backward-function* search-backward-function))
           (when (and before after)
             (cond ((or start-p end-p)
                    (with-point ((s (or start (buffer-start-point (current-buffer))))
                                 (e (or end (buffer-end-point (current-buffer)))))
                      (query-replace-internal-body s e before after query count)))
                   ((buffer-mark-p buffer)
                    (with-point ((mark-point (buffer-mark buffer) :right-inserting))
                      (cond ((point< mark-point (buffer-point buffer))
                             (query-replace-internal-body mark-point
                                                          (buffer-point buffer)
                                                          before after query count))
                            (t
                             (query-replace-internal-body (buffer-point buffer)
                                                          mark-point
                                                          before after query count)))))
                   (t (query-replace-internal-body (buffer-point buffer)
                                                   nil before after query count)))))
      (isearch-reset-overlays buffer))))

(define-key *global-keymap* "M-%" 'query-replace)

(define-command query-replace (before after)
    ((:splice (read-query-replace-args)))
  (query-replace-internal before
                          after
                          #'search-forward
                          #'search-backward
                          :query t))

(define-command query-replace-regexp (before after)
    ((:splice (read-query-replace-args)))
  (query-replace-internal before
                          after
                          #'search-forward-regexp
                          #'search-backward-regexp
                          :query t))

(define-command query-replace-symbol (before after)
    ((:splice (read-query-replace-args)))
  (query-replace-internal before
                          after
                          #'search-forward-symbol
                          #'search-backward-symbol
                          :query t))
