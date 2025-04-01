(defpackage :lem-fbar
  (:use :cl :lem)
  (:export #:*fbar-path*
	   #:*fbar-width*
	   #:fbar))

(in-package :lem-fbar)


;; structure to track files.
(defstruct fb tab path dir open)

;;==============================================================================
;;
(defparameter *fbar-path* (truename "~/"))
(defparameter *fbar-buffer* nil)
(defparameter *fbar-window* nil)
(defparameter *fbar-width* 32)

(define-major-mode fbar-mode nil; fundamental-mode
    (:name "fbar" :keymap *fbar-mode-keymap*))

(define-attribute fbar-file
  (:light :foreground "black" )
  (:dark :foreground "white"
	 :background "#333333"))
(define-attribute fbar-dir
  (:light :foreground "blue" :bold t )
  (:dark :foreground "sky blue" :bold t
	 :background "#333333"))


;;==============================================================================
;;
;; FBAR is invoked with a <C-x f>;
;; dismiss with <Escape>
;; open-close directories with <Return>
;; select files with <Return>

(define-key *global-keymap* "C-x f" 'fbar-on)
(define-key *fbar-mode-keymap* "j" 'next-line)
(define-key *fbar-mode-keymap* "k" 'previous-line)
(define-key *fbar-mode-keymap* "Down" 'next-line)
(define-key *fbar-mode-keymap* "Up" 'previous-line)
(define-key *fbar-mode-keymap* "Return" 'fbar-select)
(define-key *fbar-mode-keymap* "Escape" 'fbar-off)
;; brutal, but seems effective in preventing unwanted strokes
(define-key *fbar-mode-keymap* "C-x" 'fbar-off)


;;==============================================================================

(defun fbar-insert-entry (pt path dirp tab)
  (flet ((spaces (count) (format nil "~v,,,v<~>" count #\space))
	 (dname ()  (format nil "+ ~A" (car (last (pathname-directory path)))))
	 (fname ()  (let ((suffix (pathname-type path)))
		      (if suffix
			  (format nil "~A.~A" (pathname-name path) suffix)
			  (format nil "~A" (pathname-name path))))))

    ;;pad the filename to create a 'background'
    (let* ((nstring (if dirp (dname) (fname)))
           (rem (max 0 (- *fbar-width* (lem:string-width nstring) tab))))
      (setf nstring (concatenate 'string
				 (spaces tab)
				 nstring
				 (spaces rem)
				 "
"))
      (insert-string
       pt nstring
       :attribute  (if dirp 'fbar-dir  'fbar-file)
       'type (make-fb :tab tab :path path  :dir dirp :open nil)))))

(define-command fbar-select () ()
  (let ((prop (text-property-at (current-point) 'type)))
    (setf (buffer-read-only-p *fbar-buffer*) nil)
    (with-slots (dir open path tab) prop
      (if dir
	  (save-excursion ;; directories are only opened or closed...
	    (if (setf open (not open))
		;; if closed, open it
		(let ((newtab (+ 2 tab)))
		  (character-offset (line-end (current-point)) 1)
		  (loop for f in (uiop:subdirectories path)
		     for pt = (current-point) do
		       (fbar-insert-entry pt f t newtab))
		  (loop for f in (uiop:directory-files path) do
		       (fbar-insert-entry (current-point) f nil newtab )))
		;; if open, close it
		(let ((ourtab (fb-tab prop)))
		  (next-line 1) (line-start (current-point))
		  (loop
		     for tab = (fb-tab (text-property-at (current-point) 'type))
		     while (> tab ourtab) do
		       (kill-line) (kill-line))
		  nil)))
	    ;; file!
	    (progn
	      (fbar-off)
	      (read-file path))))
    (setf (buffer-read-only-p *fbar-buffer*) t)))


;; Closure to preserve fbar global state.
(let ((old-window nil)
      (old-buffer nil)
      (old-point nil))

  (define-command fbar-on () ()
    (unless *fbar-window*
      (fbar)
      (setf old-window (current-window)
	    old-buffer (current-buffer)
	    old-point (copy-point  (current-point) :temporary  ))
      (setf *fbar-window*
            (make-floating-window :buffer *fbar-buffer*
                                  :x 0
                                  :y 0
                                  :width (1+ *fbar-width*)
                                  :height (1- (display-height))
                                  :use-modeline-p nil))

      (setf (current-window) *fbar-window*)
      (setf (current-buffer) *fbar-buffer*)
      (redraw-display)))


  (define-command fbar-off () ()
    (when *fbar-window*
      (setf (current-window) old-window)
      (setf (current-buffer) old-buffer)
      (delete-window *fbar-window*)
      (move-point (current-point) old-point)
      (switch-to-buffer old-buffer t t)
      ;;(delete-point *old-point*)
      (setf *fbar-window* nil)
      (redraw-display t))))



(defun fbar (&optional (path *fbar-path* custom-path))
  (when custom-path (setf *fbar-path* path))
  (let* ((buffer (make-buffer "FBAR" :temporary t
			      :enable-undo-p nil ))
	 (point (buffer-point buffer)))
    (erase-buffer buffer)
    (loop for f in (uiop:subdirectories path) do
	 (fbar-insert-entry point f t 1))
    (loop for f in (uiop:directory-files path) do
	 (fbar-insert-entry point f nil 1))
    (buffer-start point)
    (change-buffer-mode buffer 'fbar-mode)
    (setf *fbar-buffer* buffer)
    (setf (buffer-read-only-p *fbar-buffer*) t)
    ))
