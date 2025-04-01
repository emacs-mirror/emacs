(defpackage :lem-tests/frame-multiplexer
  (:use :cl
        :lem
        :testif))
(in-package :lem-tests/frame-multiplexer)

;;; このテストは不十分で動作も不安定なので、まだ動かさない

(defstruct datum
  id
  result)

(defstruct (error-datum (:include datum)))

(test test
  (ql:quickload :lem-fake-interface :silent t)
  (lem)
  (let ((event-queue (lem-core::make-event-queue)))
    (send-event (lambda ()
                  (block outer
                    (handler-bind ((error (lambda (c)
                                            (send-event (make-error-datum :id 3 :result c))
                                            (return-from outer))))
                      (redraw-display)
                      (unless (lem-frame-multiplexer::enabled-frame-multiplexer-p)
                        (lem-frame-multiplexer::toggle-frame-multiplexer))
                      (delete-between-points (buffer-start-point (current-buffer))
                                             (buffer-end-point (current-buffer)))
                      (insert-string (current-point) "abc")
                      (redraw-display t)
                      (lem-frame-multiplexer::frame-multiplexer-create-with-new-buffer-list)
                      (redraw-display t)
                      (send-event (make-datum :id 1 :result (uiop:symbol-call :lem-fake-interface :display))
                                  event-queue)
                      (lem-frame-multiplexer::frame-multiplexer-next)
                      (send-event (make-datum :id 2 :result (uiop:symbol-call :lem-fake-interface :display))
                                  event-queue)))))
    (let* ((datum1 (lem-core::dequeue-event 1 event-queue))
           (datum2 (lem-core::dequeue-event 1 event-queue)))
      (ok (string= (datum-result datum1) "abc "))
      (ok (string= (datum-result datum2) "abc ")))
    (send-event (lambda ()
                  (exit-lem nil)
                  (clrhash lem-frame-multiplexer::*virtual-frame-map*)))))
