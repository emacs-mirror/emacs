(defpackage :lem/kbdmacro
  (:use :cl :lem)
  (:export
   :kbdmacro-start
   :kbdmacro-end
   :kbdmacro-execute
   :apply-macro-to-region-lines)
  #+sbcl
  (:lock t))
(in-package :lem/kbdmacro)

(defvar *last-macro-chars* nil)
(defvar *macro-running-p* nil)

(define-key *global-keymap* "C-x (" 'kbdmacro-start)
(define-command kbdmacro-start () ()
  (cond ((key-recording-p)
         (stop-record-key)
         (editor-error "Macro already active"))
        (t
         (start-record-key))))

(define-key *global-keymap* "C-x )" 'kbdmacro-end)
(define-command kbdmacro-end () ()
  (cond (*macro-running-p* t)
        ((not (key-recording-p))
         (editor-error "Macro not active"))
        (t
         (setq *last-macro-chars* (stop-record-key)))))

(define-key *global-keymap* "C-x e" 'kbdmacro-execute)
(define-command kbdmacro-execute (n) (:universal)
  (cond ((key-recording-p)
         (editor-error "Macro already active"))
        (*macro-running-p*
         (editor-error "Macro already active"))
        ((null *last-macro-chars*))
        (t
         (let ((*macro-running-p* t))
           (loop
             :repeat n
             :do (handler-case
                     (execute-key-sequence *last-macro-chars*)
                   (editor-condition () (return))))))))

(define-command apply-macro-to-region-lines (start-point end-point) (:region)
  (with-point ((next-line-point start-point :left-inserting)
               (end-point end-point :right-inserting))
    (loop :while (point< next-line-point end-point)
          :do (save-excursion
                (move-point (current-point) next-line-point)
                (line-offset next-line-point 1)
                (kbdmacro-execute 1)))))
