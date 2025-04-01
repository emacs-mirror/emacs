(in-package :lem-core)

(define-editor-variable kill-buffer-hook '())

(defun strip-buffer-from-frame-windows (buffer frame)
  (dolist (window (get-buffer-windows buffer :frame frame :include-floating-windows t))
    (with-current-window window
      (switch-to-buffer (or (get-previous-buffer buffer)
                            (first (last (buffer-list))))))))

(defmethod delete-buffer-using-manager :before
    ((manager buffer-list-manager)
     buffer)
  (dolist (frame (all-frames))
    (strip-buffer-from-frame-windows buffer frame))
  (run-hooks (make-per-buffer-hook :var 'kill-buffer-hook :buffer buffer) buffer))
