(defpackage :lem-core/commands/move
  (:use :cl :lem-core)
  (:export :next-line
           :next-logical-line
           :previous-line
           :previous-logical-line
           :forward-char
           :backward-char
           :move-to-beginning-of-buffer
           :move-to-end-of-buffer
           :move-to-beginning-of-line
           :move-to-beginning-of-logical-line
           :move-to-end-of-line
           :move-to-end-of-logical-line
           :next-page
           :previous-page
           :next-page-char
           :previous-page-char
           :goto-line)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/move)

(define-key *global-keymap* "C-n" 'next-line)
(define-key *global-keymap* "Down" 'next-line)
(define-key *global-keymap* "C-p" 'previous-line)
(define-key *global-keymap* "Up" 'previous-line)
(define-key *global-keymap* "C-f" 'forward-char)
(define-key *global-keymap* "Right" 'forward-char)
(define-key *global-keymap* "C-b" 'backward-char)
(define-key *global-keymap* "Left" 'backward-char)
(define-key *global-keymap* "M-<" 'move-to-beginning-of-buffer)
(define-key *global-keymap* "C-Home" 'move-to-beginning-of-buffer)
(define-key *global-keymap* "M->" 'move-to-end-of-buffer)
(define-key *global-keymap* "C-End" 'move-to-end-of-buffer)
(define-key *global-keymap* "C-a" 'move-to-beginning-of-line)
(define-key *global-keymap* "Home" 'move-to-beginning-of-line)
(define-key *global-keymap* "C-e" 'move-to-end-of-line)
(define-key *global-keymap* "End" 'move-to-end-of-line)
(define-key *global-keymap* "C-v" 'next-page)
(define-key *global-keymap* "PageDown" 'next-page)
(define-key *global-keymap* "M-v" 'previous-page)
(define-key *global-keymap* "PageUp" 'previous-page)
(define-key *global-keymap* "C-x ]" 'next-page-char)
(define-key *global-keymap* "C-x [" 'previous-page-char)
(define-key *global-keymap* "M-g" 'goto-line)

(defun next-line-aux (n
                      point-column-fn
                      forward-line-fn
                      move-to-column-fn)
  (if (continue-flag :next-line)
      (unless (not (null (cursor-saved-column (current-point))))
        (log:error "asseriton error: (not (null (cursor-saved-column (current-point))))"))
      (setf (cursor-saved-column (current-point))
            (funcall point-column-fn (current-point))))
  (unless (prog1 (funcall forward-line-fn (current-point) n)
            (funcall move-to-column-fn (current-point) (cursor-saved-column (current-point))))
    (cond ((plusp n)
           (move-to-end-of-buffer)
           (error 'end-of-buffer :point (current-point)))
          ((minusp n)
           (move-to-beginning-of-buffer)
           (error 'beginning-of-buffer :point (current-point))))))

(define-command (next-line (:advice-classes movable-advice)) (&optional n) (:universal)
  "Move the cursor to next line."
  (next-line-aux n
                 #'point-virtual-line-column
                 #'move-to-next-virtual-line
                 #'move-to-virtual-line-column))

(define-command (next-logical-line (:advice-classes movable-advice)) (&optional n) (:universal)
  "Move the cursor to the next logical line."
  (next-line-aux n
                 #'point-column
                 #'line-offset
                 #'move-to-column))

(define-command (previous-line (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous line."
  (next-line (- n)))

(define-command (previous-logical-line (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous logical line."
  (next-logical-line (- n)))

(define-command (forward-char (:advice-classes movable-advice))
    (&optional (n 1)) (:universal)
  "Move the cursor to the next character."
  (or (character-offset (current-point) n)
      (error 'end-of-buffer :point (current-point))))

(define-command (backward-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous character."
  (or (character-offset (current-point) (- n))
      (error 'beginning-of-buffer :point (current-point))))

(define-command (move-to-beginning-of-buffer (:advice-classes jump-cursor-advice)) () ()
  "Move the cursor to the beginning of the buffer."
  (run-hooks *set-location-hook* (current-point))
  (buffer-start (current-point)))

(define-command (move-to-end-of-buffer (:advice-classes jump-cursor-advice)) () ()
  "Move the cursor to the end of the buffer."
  (run-hooks *set-location-hook* (current-point))
  (buffer-end (current-point)))

(define-command (move-to-beginning-of-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the beginning of the line."
  (let ((bol (backward-line-wrap (copy-point (current-point) :temporary)
                                 (current-window)
                                 t)))
    (cond ((text-property-at (current-point) :field -1))
          ((previous-single-property-change (current-point)
                                            :field
                                            bol))
          ((start-line-p bol)
           (back-to-indentation bol)
           (if (point= bol (current-point))
               (line-start (current-point))
               (move-point (current-point) bol)))
          (t (move-point (current-point) bol)))))

(define-command (move-to-beginning-of-logical-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the beginning of the logical line."
  (line-start (current-point)))

(define-command (move-to-end-of-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the end of the line."
  (or (and (forward-line-wrap (current-point) (current-window))
           (character-offset (current-point) -1))
      (line-end (current-point))))

(define-command (move-to-end-of-logical-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the end of the logical line."
  (line-end (current-point)))

(define-command (next-page (:advice-classes movable-advice)) (&optional n) (:universal-nil)
  "Move the cursor to the next page by one page."
  (if n
      (scroll (current-window) n)
      (progn
        (next-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

(define-command (previous-page (:advice-classes movable-advice)) (&optional n) (:universal-nil)
  "Move the cursor to the previous page by one page."
  (if n
      (scroll (current-window) (- n))
      (progn
        (previous-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

(define-command (next-page-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the next page character (^L)."
  (let ((point (current-point)))
    (dotimes (_ (abs n))
      (loop
        (unless (line-offset point (if (plusp n) 1 -1))
          (return-from next-page-char))
        (when (eql #\page (character-at point 0))
          (return))))))

(define-command (previous-page-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous page character (^L)."
  (next-page-char (- n)))

(define-command (goto-line (:advice-classes jump-cursor-advice)) (n) ((:number "Line to GOTO: "))
  "Move the cursor to the specified line number."
  (cond ((< n 1)
         (setf n 1))
        ((< #1=(buffer-nlines (current-buffer)) n)
         (setf n #1#)))
  (run-hooks *set-location-hook* (current-point))
  (line-offset (buffer-start (current-point)) (1- n)))
