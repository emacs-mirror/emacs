(require 'timer)

;;;###autoload
(defvar hanoi-break-interval (* 60 30)
  "*Number of seconds between Hanoi breaks.")

(add-hook 'post-command-hook 'hanoi-break-check t)

(defvar hanoi-break-p nil
  "Non-nil if we need a Hanoi break real soon now.")

(defun hanoi-break-check ()
  "Take a Hanoi break if the time has come."
  (and (not (input-pending-p))
       (prog1 hanoi-break-p
	 (setq hanoi-break-p nil))
       (hanoi-break)))

;;;###autoload
(defun hanoi-break ()
  "Take a Hanoi break, son."
  (interactive)
  (save-window-excursion
    (eval (condition-case error
	      (if (not (yes-or-no-p "Take a break now? "))
		  '(hanoi-break-schedule 60) ; Bug him again in one minute.
		;; Eat the screen.
		(if (eq (selected-window) (minibuffer-window))
		    (other-window 1))
		(delete-other-windows)
		(scroll-right (window-width))
		;; Send him on his way.
		(message "Take a break, son.")
		(if (get-buffer "*Hanoi*")
		    (kill-buffer "*Hanoi*"))
		(condition-case ()
		    (progn
		      (hanoi (/ (window-width) 8))
		      ;; Wait for him to come back.
		      (read-char)
		      (kill-buffer "*Hanoi*"))
		  (quit nil))
		'(hanoi-break-schedule)) ; Schedule next break.
	    (quit '(hanoi-break-schedule 60)) ; Bug him again in one minute.
	    ;;(error t)
	    ))))

;;;###autoload
(defun hanoi-break-schedule (&optional time)
  "Schedule a break for ARG seconds from now (default: hanoi-break-interval)."
  (interactive (list (and current-prefix-arg
			  (prefix-numeric-value current-prefix-arg))))
  (or time (setq time hanoi-break-interval))
  (run-at-time time nil 'hanoi-break-soon))

(defun hanoi-break-soon ()
  "Take a Hanoi break very soon."
  (setq hanoi-break-p t))

(defun cancel-hanoi-break ()
  "Cancel scheduled Hanoi breaks."
  (interactive)
  (cancel-function-timers 'hanoi-break-soon))

(provide 'hanoi-break)
