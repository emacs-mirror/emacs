;; The Linux console handles Latin-1 by default.

(unless (terminal-coding-system)
  (set-terminal-coding-system 'iso-latin-1))

;; Make Latin-1 input characters work, too.
;; Meta will continue to work, because the kernel
;; turns that into Escape.

(let ((value (current-input-mode)))
  ;; The third arg only matters in that it is not t or nil.
  (set-input-mode (nth 0 value) (nth 1 value) 'iso-latin-1 (nth 3 value)))

;;; linux.el ends here
