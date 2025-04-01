(defpackage :lem-core/commands/s-expression
  (:use :cl
        :lem-core
        :lem-core/commands/edit)
  (:export :forward-sexp
           :backward-sexp
           :forward-list
           :backward-list
           :down-list
           :up-list
           :mark-sexp
           :kill-sexp
           :transpose-sexps)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/s-expression)

(define-key *global-keymap* "C-M-f" 'forward-sexp)
(define-key *global-keymap* "C-M-b" 'backward-sexp)
(define-key *global-keymap* "C-M-n" 'forward-list)
(define-key *global-keymap* "C-M-p" 'backward-list)
(define-key *global-keymap* "C-M-d" 'down-list)
(define-key *global-keymap* "C-M-u" 'up-list)
(define-key *global-keymap* "C-M-@" 'mark-sexp)
(define-key *global-keymap* "C-M-Space" 'mark-sexp)
(define-key *global-keymap* "C-M-k" 'kill-sexp)
(define-key *global-keymap* "C-M-t" 'transpose-sexps)
(define-key *global-keymap* "C-M-y" 'kill-around-form)

(define-command (forward-sexp (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the forward expression."
  (with-point ((prev (current-point)))
    (let ((point (form-offset (current-point) n)))
      (or point
          (progn
            (move-point (current-point) prev)
            (if no-errors
                nil
                (scan-error)))))))

(define-command (backward-sexp (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the backward expression."
  (forward-sexp (- n) no-errors))

(define-command (forward-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the forward list."
  (scan-lists (current-point) n 0 no-errors))

(define-command (backward-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the backward list."
  (scan-lists (current-point) (- n) 0 no-errors))

(define-command (down-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the inner expression."
  (scan-lists (current-point) n -1 no-errors))

(define-command (up-list (:advice-classes movable-advice)) (&optional (n 1) no-errors) (:universal)
  "Move the cursor to the outer expression."
  (or (maybe-beginning-of-string (current-point))
      (scan-lists (current-point) (- n) 1 no-errors)))

(define-command mark-sexp () ()
  "Select the forward expression as a region."
  (cond
    ((continue-flag :mark-sexp)
     (form-offset (mark-point (cursor-mark (current-point))) 1))
    (t
     (save-excursion
       (form-offset (current-point) 1)
       (set-cursor-mark (current-point) (current-point))))))

(define-command (kill-sexp (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Kill the forward expression as a region."
  (dotimes (_ n t)
    (let ((end (form-offset (copy-point (current-point) :temporary) 1)))
      (if end
          (with-point ((end end :right-inserting))
            (kill-region (current-point) end))
          (scan-error)))))

(define-command (transpose-sexps (:advice-classes editable-advice)) () ()
  "Swaps the expression before and after the cursor."
  (with-point ((point1 (current-point) :left-inserting)
               (point2 (current-point) :left-inserting))
    (when (and (form-offset point1 -1)
               (form-offset point2 1))
      (alexandria:when-let*
          ((form-string1
            (with-point ((start point1)
                         (end point1 :right-inserting))
              (when (form-offset end 1)
                (prog1 (points-to-string start end)
                  (delete-between-points start end)))))
           (form-string2
            (with-point ((end point2)
                         (start point2))
              (when (form-offset start -1)
                (with-point ((end end :right-inserting))
                  (prog1 (points-to-string start end)
                    (delete-between-points start end)))))))
        (insert-string point1 form-string2)
        (insert-string point2 form-string1)))))

(define-command (kill-around-form (:advice-classes editable-advice)) () ()
  "Kill the outer expression."
  (with-point ((end (current-point) :right-inserting))
    (let ((start (current-point)))
      (unless (form-offset end 1)
        (scan-error))
      (when (symbol-string-at-point start)
        (skip-symbol-backward start))
      (let ((remaining-text (points-to-string start end)))
        (delete-between-points start end)
        (up-list)
        (kill-sexp)
        (save-excursion (insert-string start remaining-text)))
      (form-offset end 1)
      (indent-points start end))))

(defmethod execute :around (mode
                            (command mark-sexp)
                            argument)
  (process-each-cursors #'call-next-method))
