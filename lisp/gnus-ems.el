;;; gnus-ems.el --- functions for making Gnus work under different Emacsen
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-group-mode-hook ())
(defvar gnus-summary-mode-hook ())
(defvar gnus-article-mode-hook ())

(defalias 'gnus-make-overlay 'make-overlay)
(defalias 'gnus-overlay-put 'overlay-put)
(defalias 'gnus-move-overlay 'move-overlay)

(or (fboundp 'mail-file-babyl-p)
    (fset 'mail-file-babyl-p 'rmail-file-p))

;; Don't warn about these undefined variables.
					;defined in gnus.el
(defvar gnus-active-hashtb)
(defvar gnus-article-buffer)
(defvar gnus-auto-center-summary)
(defvar gnus-buffer-list)
(defvar gnus-current-headers)
(defvar gnus-level-killed)
(defvar gnus-level-zombie)
(defvar gnus-newsgroup-bookmarks)
(defvar gnus-newsgroup-dependencies)
(defvar gnus-newsgroup-headers-hashtb-by-number)
(defvar gnus-newsgroup-selected-overlay)
(defvar gnus-newsrc-hashtb)
(defvar gnus-read-mark)
(defvar gnus-refer-article-method)
(defvar gnus-reffed-article-number)
(defvar gnus-unread-mark)
(defvar gnus-version)
(defvar gnus-view-pseudos)
(defvar gnus-view-pseudos-separately)
(defvar gnus-visual)
(defvar gnus-zombie-list)
					;defined in gnus-msg.el
(defvar gnus-article-copy)
(defvar gnus-check-before-posting)
					;defined in gnus-vis.el
(defvar gnus-article-button-face)
(defvar gnus-article-mouse-face)
(defvar gnus-summary-selected-face)


;; We do not byte-compile this file, because error messages are such a
;; bore.  

(defun gnus-set-text-properties-xemacs (start end props &optional buffer)
  "You should NEVER use this function.  It is ideologically blasphemous.
It is provided only to ease porting of broken FSF Emacs programs."
  (if (and (stringp buffer) (not (setq buffer (get-buffer buffer))))
      nil
    (map-extents (lambda (extent ignored)
		   (remove-text-properties 
		    start end
		    (list (extent-property extent 'text-prop) nil)
		    buffer))
		 buffer start end nil nil 'text-prop)
    (add-text-properties start end props buffer)))

(eval
 '(progn
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	()
      ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
      (defvar gnus-display-type 
	(condition-case nil
	    (let ((display-resource (x-get-resource ".displayType" "DisplayType")))
	      (cond (display-resource (intern (downcase display-resource)))
		    ((x-display-color-p) 'color)
		    ((x-display-grayscale-p) 'grayscale)
		    (t 'mono)))
	  (error 'mono))
	"A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'. If Emacs
guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your
`~/.Xdefaults'. See also `gnus-background-mode'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")

      (defvar gnus-background-mode 
	(condition-case nil
	    (let ((bg-resource (x-get-resource ".backgroundMode"
					       "BackgroundMode"))
		  (params (frame-parameters)))
	      (cond (bg-resource (intern (downcase bg-resource)))
		    ((and (cdr (assq 'background-color params))
			  (< (apply '+ (x-color-values
					(cdr (assq 'background-color params))))
			     (/ (apply '+ (x-color-values "white")) 3)))
		     'dark)
		    (t 'light)))
	  (error 'light))
	"A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `gnus-display-type'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves."))

    (cond 
     ((string-match "XEmacs\\|Lucid" emacs-version)
      ;; XEmacs definitions.

      (setq gnus-mouse-2 [button2])

      (or (memq 'underline (list-faces))
	  (and (fboundp 'make-face)
	       (funcall (intern "make-face") 'underline)))
      ;; Must avoid calling set-face-underline-p directly, because it
      ;; is a defsubst in emacs19, and will make the .elc files non
      ;; portable!
      (or (face-differs-from-default-p 'underline)
	  (funcall 'set-face-underline-p 'underline t))

      (defalias 'gnus-make-overlay 'make-extent)
      (defalias 'gnus-overlay-put 'set-extent-property)
      (defun gnus-move-overlay (extent start end &optional buffer)
	(set-extent-endpoints extent start end))
      
      (require 'text-props)
      (fset 'set-text-properties 'gnus-set-text-properties-xemacs)

      (or (boundp 'standard-display-table) (setq standard-display-table nil))
      (or (boundp 'read-event) (fset 'read-event 'next-command-event))

      ;; Fix by "jeff (j.d.) sparkes" <jsparkes@bnr.ca>.
      (defvar gnus-display-type (device-class)
	"A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'. If Emacs
guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your
`~/.Xdefaults'. See also `gnus-background-mode'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")


      (or (fboundp 'x-color-values)
	  (fset 'x-color-values 
		(lambda (color)
		  (color-instance-rgb-components
		   (make-color-instance color)))))
    
      (defvar gnus-background-mode 
	(let ((bg-resource 
	       (condition-case ()
		   (x-get-resource ".backgroundMode" "BackgroundMode" 'string)
		 (error nil)))
	      (params (frame-parameters)))
	  (cond (bg-resource (intern (downcase bg-resource)))
		((and (assq 'background-color params)
		      (< (apply '+ (x-color-values
				    (cdr (assq 'background-color params))))
			 (/ (apply '+ (x-color-values "white")) 3)))
		 'dark)
		(t 'light)))
	"A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `gnus-display-type'.

This is a meta-variable that will affect what default values other
variables get.  You would normally not change this variable, but
pounce directly on the real variables themselves.")


      (defun gnus-install-mouse-tracker ()
	(require 'mode-motion)
	(setq mode-motion-hook 'mode-motion-highlight-line)))

     ((and (not (string-match "28.9" emacs-version)) 
	   (not (string-match "29" emacs-version)))
      ;; Remove the `intangible' prop.
      (let ((props (and (boundp 'gnus-hidden-properties) 
			gnus-hidden-properties)))
	(while (and props (not (eq (car (cdr props)) 'intangible)))
	  (setq props (cdr props)))
	(and props (setcdr props (cdr (cdr (cdr props))))))
      (or (fboundp 'buffer-substring-no-properties)
	  (defun buffer-substring-no-properties (beg end)
	    (format "%s" (buffer-substring beg end)))))
   
     ((boundp 'MULE)
      (provide 'gnusutil))
   
     )))

(eval-and-compile
  (cond
   ((not window-system)
    (defun gnus-dummy-func (&rest args))
    (let ((funcs '(mouse-set-point set-face-foreground
				   set-face-background x-popup-menu)))
      (while funcs
	(or (fboundp (car funcs))
	    (fset (car funcs) 'gnus-dummy-func))
	(setq funcs (cdr funcs))))))
  (or (fboundp 'file-regular-p)
      (defun file-regular-p (file)
	(and (not (file-directory-p file))
	     (not (file-symlink-p file))
	     (file-exists-p file))))
  (or (fboundp 'face-list)
      (defun face-list (&rest args)))
  )

(defun gnus-highlight-selected-summary-xemacs ()
  ;; Highlight selected article in summary buffer
  (if gnus-summary-selected-face
      (progn
	(if gnus-newsgroup-selected-overlay
	    (delete-extent gnus-newsgroup-selected-overlay))
	(setq gnus-newsgroup-selected-overlay 
	      (make-extent (gnus-point-at-bol) (gnus-point-at-eol)))
	(set-extent-face gnus-newsgroup-selected-overlay
			 gnus-summary-selected-face))))

(defun gnus-summary-recenter-xemacs ()
  (let* ((top (cond ((< (window-height) 4) 0)
		    ((< (window-height) 7) 1)
		    (t 2)))
	 (height (- (window-height) 2))
	 (bottom (save-excursion (goto-char (point-max))
				 (forward-line (- height))
				 (point)))
	 (window (get-buffer-window (current-buffer))))
    (and 
     ;; The user has to want it,
     gnus-auto-center-summary 
     ;; the article buffer must be displayed,
     (get-buffer-window gnus-article-buffer)
     ;; Set the window start to either `bottom', which is the biggest
     ;; possible valid number, or the second line from the top,
     ;; whichever is the least.
     (set-window-start
      window (min bottom (save-excursion (forward-line (- top)) 
					 (point)))))))

(defun gnus-group-insert-group-line-info-xemacs (group)
  (let ((entry (gnus-gethash group gnus-newsrc-hashtb)) 
	(beg (point))
	active info)
    (if entry
	(progn
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line 
	   nil group (nth 1 info) (nth 3 info) (car entry) (nth 4 info)))
      (setq active (gnus-gethash group gnus-active-hashtb))
	  
      (gnus-group-insert-group-line 
       nil group (if (member group gnus-zombie-list) gnus-level-zombie
		   gnus-level-killed)
       nil (if active (- (1+ (cdr active)) (car active)) 0) nil))
    (save-excursion
      (goto-char beg)
      (remove-text-properties 
       (1+ (gnus-point-at-bol)) (1+ (gnus-point-at-eol))
       '(gnus-group nil)))))

(defun gnus-summary-refer-article-xemacs (message-id)
  "Refer article specified by MESSAGE-ID.
NOTE: This command only works with newsgroups that use real or simulated NNTP."
  (interactive "sMessage-ID: ")
  (if (or (not (stringp message-id))
	  (zerop (length message-id)))
      ()
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (or (string-match "^<" message-id)
	(setq message-id (concat "<" message-id)))
    (or (string-match ">$" message-id)
	(setq message-id (concat message-id ">")))
    (let ((header (car (gnus-gethash (downcase message-id)
				     gnus-newsgroup-dependencies))))
      (if header
	  (or (gnus-summary-goto-article (mail-header-number header))
	      ;; The header has been read, but the article had been
	      ;; expunged, so we insert it again.
	      (let ((beg (point)))
		(gnus-summary-insert-line
		 nil header 0 nil gnus-read-mark nil nil
		 (mail-header-subject header))
		(save-excursion
		  (goto-char beg)
		  (remove-text-properties
		   (1+ (gnus-point-at-bol)) (1+ (gnus-point-at-eol))
		   '(gnus-number nil gnus-mark nil gnus-level nil)))
		(forward-line -1)
		(mail-header-number header)))
	(let ((gnus-override-method gnus-refer-article-method)
	      (gnus-ancient-mark gnus-read-mark)
	      (tmp-point (window-start
			  (get-buffer-window gnus-article-buffer)))
	      number tmp-buf)
	  (and gnus-refer-article-method
	       (gnus-check-server gnus-refer-article-method))
	  ;; Save the old article buffer.
	  (save-excursion
	    (set-buffer gnus-article-buffer)
	    (gnus-kill-buffer " *temp Article*")
	    (setq tmp-buf (rename-buffer " *temp Article*")))
	  (prog1
	      (if (gnus-article-prepare 
		   message-id nil (gnus-read-header message-id))
		  (progn
		    (setq number (mail-header-number gnus-current-headers))
		    (gnus-rebuild-thread message-id)
		    (gnus-summary-goto-subject number)
		    (gnus-summary-recenter)
		    (gnus-article-set-window-start 
		     (cdr (assq number gnus-newsgroup-bookmarks)))
		    message-id)
		;; We restore the old article buffer.
		(save-excursion
		  (kill-buffer gnus-article-buffer)
		  (set-buffer tmp-buf)
		  (rename-buffer gnus-article-buffer)
		  (let ((buffer-read-only nil))
		    (and tmp-point
			 (set-window-start (get-buffer-window (current-buffer))
					   tmp-point)))))))))))

(defun gnus-summary-insert-pseudos-xemacs (pslist &optional not-view)
  (let ((buffer-read-only nil)
	(article (gnus-summary-article-number))
	b)
    (or (gnus-summary-goto-subject article)
	(error (format "No such article: %d" article)))
    (or gnus-newsgroup-headers-hashtb-by-number
	(gnus-make-headers-hashtable-by-number))
    (gnus-summary-position-cursor)
    ;; If all commands are to be bunched up on one line, we collect
    ;; them here.  
    (if gnus-view-pseudos-separately
	()
      (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
	    files action)
	(while ps
	  (setq action (cdr (assq 'action (car ps))))
	  (setq files (list (cdr (assq 'name (car ps)))))
	  (while (and ps (cdr ps)
		      (string= (or action "1")
			       (or (cdr (assq 'action (car (cdr ps)))) "2")))
	    (setq files (cons (cdr (assq 'name (car (cdr ps)))) files))
	    (setcdr ps (cdr (cdr ps))))
	  (if (not files)
	      ()
	    (if (not (string-match "%s" action))
		(setq files (cons " " files)))
	    (setq files (cons " " files))
	    (and (assq 'execute (car ps))
		 (setcdr (assq 'execute (car ps))
			 (funcall (if (string-match "%s" action)
				      'format 'concat)
				  action 
				  (mapconcat (lambda (f) f) files " ")))))
	  (setq ps (cdr ps)))))
    (if (and gnus-view-pseudos (not not-view))
	(while pslist
	  (and (assq 'execute (car pslist))
	       (gnus-execute-command (cdr (assq 'execute (car pslist)))
				     (eq gnus-view-pseudos 'not-confirm)))
	  (setq pslist (cdr pslist)))
      (save-excursion
	(while pslist
	  (gnus-summary-goto-subject (or (cdr (assq 'article (car pslist)))
					 (gnus-summary-article-number)))
	  (forward-line 1)
	  (setq b (point))
	  (insert "          " 
		  (file-name-nondirectory (cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (add-text-properties 
	   b (1+ b) (list 'gnus-number gnus-reffed-article-number
			  'gnus-mark gnus-unread-mark 
			  'gnus-level 0
			  'gnus-pseudo (car pslist)))
	  ;; Fucking XEmacs redisplay bug with truncated lines.
	  (goto-char b)
	  (sit-for 0)
	  ;; Grumble.. Fucking XEmacs stickyness of text properties.
	  (remove-text-properties
	   (1+ b) (1+ (gnus-point-at-eol))
	   '(gnus-number nil gnus-mark nil gnus-level nil))
	  (forward-line -1)
	  (gnus-sethash (int-to-string gnus-reffed-article-number)
			(car pslist) gnus-newsgroup-headers-hashtb-by-number)
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	  (setq pslist (cdr pslist)))))))


(defun gnus-copy-article-buffer-xemacs (&optional article-buffer)
  (setq gnus-article-copy (get-buffer-create " *gnus article copy*"))
  (buffer-disable-undo gnus-article-copy)
  (or (memq gnus-article-copy gnus-buffer-list)
      (setq gnus-buffer-list (cons gnus-article-copy gnus-buffer-list)))
  (let ((article-buffer (or article-buffer gnus-article-buffer))
	buf)
    (if (and (get-buffer article-buffer)
	     (buffer-name (get-buffer article-buffer)))
	(save-excursion
	  (set-buffer article-buffer)
	  (widen)
	  (setq buf (buffer-substring (point-min) (point-max)))
	  (set-buffer gnus-article-copy)
	  (erase-buffer)
	  (insert (format "%s" buf))))))

(defun gnus-article-push-button-xemacs (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (let* ((pos (event-closest-point event))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (if fun (funcall fun data))))

;; Re-build the thread containing ID.
(defun gnus-rebuild-thread-xemacs  (id)
  (let ((dep gnus-newsgroup-dependencies)
	(buffer-read-only nil)
	parent headers refs thread art)
    (while (and id (setq headers
			 (car (setq art (gnus-gethash (downcase id) 
						      dep)))))
      (setq parent art)
      (setq id (and (setq refs (mail-header-references headers))
		    (string-match "\\(<[^>]+>\\) *$" refs)
		    (substring refs (match-beginning 1) (match-end 1)))))
    (setq thread (gnus-make-sub-thread (car parent)))
    (gnus-rebuild-remove-articles thread)
    (let ((beg (point)))
      (gnus-summary-prepare-threads (list thread) 0)
      (save-excursion
	(while (and (>= (point) beg)
		    (not (bobp)))
	  (or (eobp)
	      (remove-text-properties
	       (1+ (gnus-point-at-bol)) (1+ (gnus-point-at-eol))
	       '(gnus-number nil gnus-mark nil gnus-level nil)))
	  (forward-line -1)))
      (gnus-summary-update-lines beg (point)))))


;; Fixed by Christopher Davis <ckd@loiosh.kei.com>.
(defun gnus-article-add-button-xemacs (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to) 'face gnus-article-button-face))
  (add-text-properties from to
		       (append
			(and gnus-article-mouse-face
			     (list 'mouse-face gnus-article-mouse-face))
			(list 'gnus-callback fun)
			(and data (list 'gnus-data data))
			(list 'highlight t))))

(defun gnus-window-top-edge-xemacs (&optional window)
  (nth 1 (window-pixel-edges window)))

;; Select the lowest window on the frame.
(defun gnus-appt-select-lowest-window-xemacs ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-pixel-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr 
                                               (window-pixel-edges 
						this-window)))))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))

        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))

(defun gnus-ems-redefine ()
  (cond 
   ((string-match "XEmacs\\|Lucid" emacs-version)
    ;; XEmacs definitions.
    (fset 'gnus-mouse-face-function 'identity)
    (fset 'gnus-summary-make-display-table (lambda () nil))
    (fset 'gnus-visual-turn-off-edit-menu 'identity)
    (fset 'gnus-highlight-selected-summary
	  'gnus-highlight-selected-summary-xemacs)
    (fset 'gnus-summary-recenter 'gnus-summary-recenter-xemacs)
    (fset 'gnus-group-insert-group-line-info
	  'gnus-group-insert-group-line-info-xemacs)
    (fset 'gnus-copy-article-buffer 'gnus-copy-article-buffer-xemacs)
    (fset 'gnus-summary-refer-article 'gnus-summary-refer-article-xemacs)
    (fset 'gnus-summary-insert-pseudos 'gnus-summary-insert-pseudos-xemacs)
    (fset 'gnus-article-push-button 'gnus-article-push-button-xemacs)
    (fset 'gnus-rebuild-thread 'gnus-rebuild-thread-xemacs)
    (fset 'gnus-article-add-button 'gnus-article-add-button-xemacs)
    (fset 'gnus-window-top-edge 'gnus-window-top-edge-xemacs)
    (fset 'set-text-properties 'gnus-set-text-properties-xemacs)

    (or (fboundp 'appt-select-lowest-window)
	(fset 'appt-select-lowest-window 
	      'gnus-appt-select-lowest-window-xemacs))

    (if (not gnus-visual)
	()
      (setq gnus-group-mode-hook
	    (cons
	     '(lambda ()
		(easy-menu-add gnus-group-reading-menu)
		(easy-menu-add gnus-group-group-menu)
		(easy-menu-add gnus-group-misc-menu)
		(gnus-install-mouse-tracker)) 
	     gnus-group-mode-hook))
      (setq gnus-summary-mode-hook
	    (cons
	     '(lambda ()
		(easy-menu-add gnus-summary-article-menu)
		(easy-menu-add gnus-summary-thread-menu)
		(easy-menu-add gnus-summary-misc-menu)
		(easy-menu-add gnus-summary-post-menu)
		(easy-menu-add gnus-summary-kill-menu)
		(gnus-install-mouse-tracker)) 
	     gnus-summary-mode-hook))
      (setq gnus-article-mode-hook
	    (cons
	     '(lambda ()
		(easy-menu-add gnus-article-article-menu)
		(easy-menu-add gnus-article-treatment-menu))
	     gnus-article-mode-hook)))

    (defvar gnus-logo (make-glyph (make-specifier 'image)))

    (defun gnus-group-startup-xmessage (&optional x y)
      "Insert startup message in current buffer."
      ;; Insert the message.
      (erase-buffer)
      (if (featurep 'xpm)
	  (progn
	    (set-glyph-property gnus-logo 'image  "~/tmp/gnus.xpm")
	    (set-glyph-image gnus-logo "~/tmp/gnus.xpm" 'global 'x)

	    (insert " ")
	    (set-extent-begin-glyph (make-extent (point) (point)) gnus-logo)
	    (insert "
   Gnus * A newsreader for Emacsen
 A Praxis Release * larsi@ifi.uio.no")
	    (goto-char (point-min))
	    (while (not (eobp))
	      (insert (make-string (/ (max (- (window-width) (or x 35)) 0) 2)
				   ? ))
	      (forward-line 1))
	    (goto-char (point-min))
	    ;; +4 is fuzzy factor.
	    (insert-char ?\n (/ (max (- (window-height) (or y 24)) 0) 2)))

	(insert
	 (format "
     %s
           A newsreader 
      for GNU Emacs

        Based on GNUS 
             written by 
     Masanobu UMEDA

       A Praxis Release
      larsi@ifi.uio.no
" 
		 gnus-version))
	;; And then hack it.
	;; 18 is the longest line.
	(indent-rigidly (point-min) (point-max) 
			(/ (max (- (window-width) (or x 28)) 0) 2))
	(goto-char (point-min))
	;; +4 is fuzzy factor.
	(insert-char ?\n (/ (max (- (window-height) (or y 12)) 0) 2)))

      ;; Fontify some.
      (goto-char (point-min))
      (search-forward "Praxis")
      (put-text-property (match-beginning 0) (match-end 0) 'face 'bold)
      (goto-char (point-min)))



    )

   ((boundp 'MULE)
    ;; Mule definitions
    (if (not (fboundp 'truncate-string))
	(defun truncate-string (str width)
	  (let ((w (string-width str))
		(col 0) (idx 0) (p-idx 0) chr)
	    (if (<= w width)
		str
	      (while (< col width)
		(setq chr (aref str idx)
		      col (+ col (char-width chr))
		      p-idx idx
		      idx (+ idx (char-bytes chr))
		      ))
	      (substring str 0 (if (= col width)
				   idx
				 p-idx))
	      )))
      )
    (defalias 'gnus-truncate-string 'truncate-string)

    (defun gnus-cite-add-face (number prefix face)
      ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
      (if face
	  (let ((inhibit-point-motion-hooks t)
		from to)
	    (goto-line number)
	    (if (boundp 'MULE)
		(forward-char (chars-in-string prefix))
	      (forward-char (length prefix)))
	    (skip-chars-forward " \t")
	    (setq from (point))
	    (end-of-line 1)
	    (skip-chars-backward " \t")
	    (setq to (point))
	    (if (< from to)
		(gnus-overlay-put (gnus-make-overlay from to) 'face face)))))

    (defun gnus-max-width-function (el max-width)
      (` (let* ((val (eval (, el)))
		(valstr (if (numberp val)
			    (int-to-string val) val)))
	   (if (> (length valstr) (, max-width))
	       (truncate-string valstr (, max-width))
	     valstr))))

    (fset 'gnus-summary-make-display-table (lambda () nil))
    
    (if (boundp 'gnus-check-before-posting)
	(setq gnus-check-before-posting
	      (delq 'long-lines
		    (delq 'control-chars gnus-check-before-posting)))
      )
    )
   ))

(provide 'gnus-ems)

;; Local Variables:
;; byte-compile-warnings: '(redefine callargs)
;; End:

;;; gnus-ems.el ends here
