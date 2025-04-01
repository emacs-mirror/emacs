(defpackage :lem-color-preview
  (:use :cl :lem)
  (:export :invoke-color-picker))
(in-package :lem-color-preview)

(define-minor-mode color-preview
    (:name "Color Preview"
     :hide-from-modeline t)
  (scan-color-in-window (current-window)))

(defmethod invoke-color-picker (frontend callback))

(defun click-callback (window point)
  (with-point ((start (maybe-beginning-of-string point)))
    (with-point ((end start))
      (form-offset end 1)
      (character-offset start 1)
      (character-offset end -1)
      (let ((start-pos (position-at-point start))
            (end-pos (position-at-point end))
            (buffer (point-buffer start)))
        (invoke-color-picker (implementation)
                             (lambda (color)
                               (when color
                                 (with-point ((start (buffer-point buffer))
                                              (end (buffer-point buffer)))
                                   (move-to-position start start-pos)
                                   (move-to-position end end-pos)
                                   (delete-between-points start end)
                                   (insert-string start (color-to-hex-string color))
                                   (scan-color-in-window window)))))))))

(define-overlay-accessors color-ovelray
  :clear-function clear-color-overlays
  :add-function add-color-overlay)

(defun scan-color-in-window (window)
  (scan-color-in-region (line-start (copy-point (window-view-point window) :temporary))
                        (or (line-offset (copy-point (window-view-point window) :temporary)
                                         (window-height window))
                            (buffer-end-point (window-buffer window)))))

(defun scan-color-in-region (start end)
  (clear-color-overlays (point-buffer start))
  (with-point ((point start))
    (loop :while (point< point end)
          :do (cond
                ((syntax-string-quote-char-p (character-at point))
                 (with-point ((string-start point))
                   (unless (form-offset point 1)
                     (return))
                   (with-point ((string-end point))
                     (character-offset string-end -1)
                     (character-offset string-start 1)
                     (let ((color (points-to-string string-start string-end)))
                       (when (parse-color color)
                         (character-offset string-end 1)
                         (character-offset string-start -1)
                         (let ((attribute
                                 (make-attribute :background color
                                                 :foreground (if (light-color-p color)
                                                                 "#000000"
                                                                 "#ffffff"))))
                           (add-color-overlay (point-buffer start)
                                              (make-overlay string-start
                                                            string-end
                                                            attribute)))
                         (lem-core::set-clickable string-start
                                                  string-end
                                                  'click-callback))))))
                (t
                 (character-offset point 1))))))

(defmethod execute :after ((mode color-preview) command argument)
  (let ((window (current-window)))
    (scan-color-in-window window)))
