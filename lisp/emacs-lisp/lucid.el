(defun add-timeout (secs function object &optional resignal)
  (run-at-time secs resignal function object))

(defun disable-timeout (timeout)
  (cancel-timer timeout))

(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree))
	    (copy-tree (cdr tree)))
    (if (vectorp tree)
	(let ((new (copy-sequence tree))
	      (i (1- (length new))))
	  (while (>= i 0)
	    (aset new i (copy-tree (aref new i)))
	    (setq i (1- i)))
	  new)
      tree)))

(fset 'current-time-seconds 'current-time)

(defun keymap-parent (keymap)
  (let ((tail (cdr keymap)))
    (while (and tail (not (eq (car tail) 'keymap)))
      (setq tail (cdr tail)))
    tail))

(defun set-keymap-parent (keymap new-parent)
  (let ((tail (cdr keymap)))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

(defun remove-hook (hook-var function)
  (if (boundp 'hook-var)
      (set hook-var (delq function (symbol-value hook-var)))))

(defun remprop (symbol prop)
  (let ((plist (symbol-plist symbol)))
    (while (eq (car plist) prop)
      (setplist symbol (setq plist (cdr (cdr plist)))))
    (while plist
      (if (eq (nth 2 plist) prop)
	  (setcdr (cdr plist) (nthcdr 4 plist)))
      (setq plist (cdr (cdr plist))))))

(defun map-keymap (function keymap &optional sort-first)
  "Call FUNCTION for every binding in KEYMAP.
This includes bindings inherited from a parent keymap.
FUNCTION receives two arguments each time it is called:
the character (more generally, the event type) that is bound,
and the binding it has."
  (if sort-first
      (let (list)
	(map-keymap (function (lambda (a b)
				(setq list (cons (cons a b) list))))
		    keymap)
	(setq list (sort list
			 (function (lambda (a b)
				     (setq a (car a) b (car b))
				     (if (integerp a)
					 (if (integerp b) (< a b)
					   t)
				       (if (integerp b) t
					 (string< a b)))))))
	(while list
	  (funcall function (car (car list)) (cdr (car list)))
	  (setq list (cdr list))))
    (while (consp keymap)
      (if (consp (car keymap))
	  (funcall function (car (car keymap)) (cdr (car keymap)))
	(if (vectorp (car keymap))
	    (let ((i (1- (length (car keymap))))
		  (vector (car keymap)))
	      (while (>= i 0)
		(funcall function i (aref vector i))
		(setq i (1- i))))))
      (setq keymap (cdr keymap)))))

(defun real-path-name (name &optional default)
  (file-truename (expand-file-name name default)))

;; It's not clear what to return if the mouse is not in FRAME.
(defun read-mouse-position (frame)
  (let ((pos (mouse-position)))
    (if (eq (car pos) frame)
	(cdr pos))))

(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.
With a numeric arg N, switch to the Nth most recent buffer.
With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))

;; Support the Lucid names with `screen' instead of `frame'.

(fset 'current-screen-configuration 'current-frame-configuration)
(fset 'delete-screen 'delete-frame)
(fset 'find-file-new-screen 'find-file-other-frame)
(fset 'find-file-read-only-new-screen 'find-file-read-only-other-frame)
(fset 'find-tag-new-screen 'find-tag-other-frame)
;;(fset 'focus-screen 'focus-frame)
(fset 'iconify-screen 'iconify-frame)
(fset 'mail-new-screen 'mail-other-frame)
(fset 'make-screen-invisible 'make-frame-invisible)
(fset 'make-screen-visible 'make-frame-visible)
;; (fset 'minibuffer-screen-list 'minibuffer-frame-list)
(fset 'modify-screen-parameters 'modify-frame-parameters)
(fset 'next-screen 'next-frame)
;; (fset 'next-multiscreen-window 'next-multiframe-window)
;; (fset 'previous-multiscreen-window 'previous-multiframe-window)
;; (fset 'redirect-screen-focus 'redirect-frame-focus)
(fset 'redraw-screen 'redraw-frame)
;; (fset 'screen-char-height 'frame-char-height)
;; (fset 'screen-char-width 'frame-char-width)
;; (fset 'screen-configuration-to-register 'frame-configuration-to-register)
;; (fset 'screen-focus 'frame-focus)
(fset 'screen-height 'frame-height)
(fset 'screen-list 'frame-list)
;; (fset 'screen-live-p 'frame-live-p)
(fset 'screen-parameters 'frame-parameters)
(fset 'screen-pixel-height 'frame-pixel-height)
(fset 'screen-pixel-width 'frame-pixel-width)
(fset 'screen-root-window 'frame-root-window)
(fset 'screen-selected-window 'frame-selected-window)
(fset 'lower-screen 'frame-to-back)
(fset 'raise-screen 'frame-to-front)
(fset 'screen-visible-p 'frame-visible-p)
(fset 'screen-width 'frame-width)
(fset 'screenp 'framep)
(fset 'select-screen 'select-frame)
(fset 'selected-screen 'selected-frame)
;; (fset 'set-screen-configuration 'set-frame-configuration)
;; (fset 'set-screen-height 'set-frame-height)
(fset 'set-screen-position 'set-frame-position)
(fset 'set-screen-size 'set-frame-size)
ll (fset 'set-screen-width 'set-frame-width)
(fset 'switch-to-buffer-new-screen 'switch-to-buffer-other-frame)
;; (fset 'unfocus-screen 'unfocus-frame)
(fset 'visible-screen-list 'visible-frame-list)
(fset 'window-screen 'window-frame)
(fset 'x-create-screen 'x-create-frame)
(fset 'x-new-screen 'new-frame)
