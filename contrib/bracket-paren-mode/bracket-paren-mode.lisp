(defpackage :lem-bracket-paren-mode
  (:use :cl :lem)
  (:export :bracket-paren-mode))

(in-package :lem-bracket-paren-mode)

(define-minor-mode bracket-paren-mode
    (:name "bracket-paren-mode"
     :keymap *bracket-paren-keymap*))

(define-key *bracket-paren-keymap* ")" 'insert-closed-paren)
(define-key *bracket-paren-keymap* "}" 'insert-closed-paren)
(define-key *bracket-paren-keymap* "]" 'insert-closed-paren)

(defun get-closed-paren-list ()
  (mapcar #'cdr
          (syntax-table-paren-pairs (current-syntax))))

(define-command insert-closed-paren () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (flet ((check-open-paren ()
             (scan-lists (copy-point (current-point) :temporary) -1 0 t))
           (cancel-insert ()
             (character-offset (current-point) -1)
             (delete-character (current-point))))
      (insert-character (current-point) c)
      (when (and (syntax-closed-paren-char-p c)
                 (not (check-open-paren)))
        (unless (loop for closed-paren in (get-closed-paren-list)
                      do

                         (cancel-insert)
                         (insert-character (current-point) closed-paren)
                         (if (check-open-paren)
                             (return t)))
          (cancel-insert)
          (insert-character (current-point) c))))))
