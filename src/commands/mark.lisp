(defpackage :lem-core/commands/mark
  (:use :cl :lem-core)
  (:export :mark-set
           :exchange-point-mark
           :mark-set-whole-buffer)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/mark)

(define-key *global-keymap* "C-@" 'mark-set)
(define-key *global-keymap* "C-Space" 'mark-set)
(define-key *global-keymap* "C-x C-x" 'exchange-point-mark)
(define-key *global-keymap* "C-x h" 'mark-set-whole-buffer)

(define-command mark-set () ()
  "Sets a mark at the current cursor position."
  (run-hooks *set-location-hook* (current-point))
  (set-cursor-mark (current-point) (current-point))
  (message "Mark set"))

(define-command exchange-point-mark () ()
  "Exchange the current cursor position with the marked position."
  (check-marked)
  (alexandria:when-let ((mark (mark-point (cursor-mark (current-point)))))
    (with-point ((current (current-point)))
      (move-point (current-point) mark)
      (set-cursor-mark (current-point) current))))

(define-command (mark-set-whole-buffer (:advice-classes jump-cursor-advice)) () ()
  "Select the whole buffer as a region."
  (buffer-end (current-point))
  (set-current-mark (current-point))
  (buffer-start (current-point))
  (message "Mark set whole buffer"))

(defmethod execute :around (mode
                            (command mark-set)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command exchange-point-mark)
                            argument)
  (process-each-cursors #'call-next-method))
