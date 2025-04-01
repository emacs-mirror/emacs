(in-package :lem-core)

(define-major-mode color-theme-selector-mode ()
    (:name "Themes"
     :keymap *color-theme-selector-keymap*))

(define-key *color-theme-selector-keymap* "Return" 'color-theme-selector-select)

(define-command color-theme-selector-select () ()
  (with-point ((point (current-point)))
    (line-start point)
    (let ((theme (text-property-at point 'theme)))
      (load-theme theme))))

(define-command list-color-themes () ()
  (let* ((buffer (make-buffer "*Color Themes*"))
         (point (buffer-point buffer))
         (dark-themes '())
         (light-themes '()))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (dolist (name (all-color-themes))
        (let ((theme (find-color-theme name)))
          (if (eq :dark (get-color-theme-color theme :display-background-mode))
              (push (cons name theme) dark-themes)
              (push (cons name theme) light-themes))))
      (loop :for (name . theme) :in (append dark-themes light-themes)
            :do (let ((button-start (copy-point point :temporary))
                      (theme-name name))
                  (lem/button:insert-button  
                   point
                   theme-name 
                   (lambda () (load-theme theme-name))
                   :attribute (make-attribute
                               :foreground (get-color-theme-color theme :foreground)
                               :background (get-color-theme-color theme :background)))
                  (put-text-property button-start point 'theme theme-name)
                  (insert-character point #\newline))))
    (buffer-start point)
    (setf (buffer-read-only-p buffer) t)
    (switch-to-buffer buffer)
    (change-buffer-mode buffer 'color-theme-selector-mode)))
