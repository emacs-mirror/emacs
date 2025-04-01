(defpackage :lem-trailing-spaces
  (:use :cl :lem)
  (:export))
(in-package :lem-trailing-spaces)

(define-attribute space-attribute
  (t :background "cyan"))

(defvar *space-attribute* (make-attribute :background "cyan"))

(define-minor-mode trailing-spaces
    (:name "Trailing Spaces"
     :global t
     :enable-hook 'enable
     :disable-hook 'disable))

(defun clear-space-attribute (start end)
  (with-point ((p start)
               (p2 start))
    (loop :while (point< p end)
          :do (when (eq 'space-attribute (text-property-at p :attribute))
                (remove-text-property p (character-offset (move-point p2 p) 1) :attribute))
              (unless (character-offset p 1)
                (return)))))

(defun scan-trailing-spaces (start end)
  (unless (lem-core:not-switchable-buffer-p (lem:current-buffer))
    (with-point ((p1 start)
                 (p2 start)
                 (end end))
      (line-start p1)
      (line-end end)
      (clear-space-attribute p1 end)
      (loop :while (point< p1 end)
            :do (line-end p1)
            (move-point p2 p1)
            (skip-whitespace-backward p1 t)
            (put-text-property p1 p2 :attribute 'space-attribute)
            (unless (line-offset p1 1)
              (return))))))

(defun enable ()
  (add-hook (variable-value 'after-syntax-scan-hook :global)
            'scan-trailing-spaces))

(defun disable ()
  (remove-hook (variable-value 'after-syntax-scan-hook :global)
               'scan-trailing-spaces))
