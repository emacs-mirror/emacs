;;; thumbs.el --- Thumbnails previewer for images files
;;;
;; Author: Jean-Philippe Theberge <jphiltheberge@videotron.ca>
;;              
;; Thanks: Alex Schroeder <alex@gnu.org> for maintaining the package at some time
;;         The peoples at #emacs@freenode.net for numerous help
;;         RMS for emacs and the GNU project.
;;
;; Keywords: Multimedia

(defconst thumbs-version "2.0")

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package create two new mode: thumbs-mode and
;; thumbs-view-image-mode. It is used for images browsing and viewing
;; from within emacs. Minimal image manipulation functions are also
;; available via external programs.
;;
;; The 'convert' program from 'ImageMagick'
;; [URL:http://www.imagemagick.org/] is required.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGELOG
;;
;; This is version 2.0
;;
;; USAGE
;;
;; Type M-x thumbs RET DIR RET to view the directory DIR in Thumbs mode.
;; That should be a directory containing image files.
;; from dired, C-t m enter in thumbs-mode with all marked files
;;             C-t a enter in thumbs-mode with all files in current-directory
;; In thumbs-mode, pressing <return> on a image will bring you in image view mode
;; for that image.  C-h m will give you a list of available keybinding.

;;; History:
;; 

;;; Code:

(require 'dired)

;; Abort if in-line imaging isn't supported (i.e. Emacs-20.7)

(when (not (display-images-p))
  (error "Your Emacs version (%S) doesn't support in-line images,
was not compiled with image support or is run in console mode.  
Upgrade to Emacs 21.1 or newer, compile it with image support 
or use a window-system"  
	 emacs-version))

;; CUSTOMIZATIONS

(defgroup thumbs nil
  "Thumbnails previewer."
  :group 'multimedia)

(defcustom thumbs-thumbsdir
  (expand-file-name "~/.emacs-thumbs")
  "*Directory to store thumbnails."
  :type 'directory
  :group 'thumbs)

(defcustom thumbs-geometry "100x100"
  "*Size of thumbnails."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-per-line 5
  "*Number of thumbnails per line to show in directory."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-thumbsdir-max-size 50000000
  "Max size for thumbnails directory.
When it reach that size (in bytes), a warning is send."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-conversion-program
  (if (equal 'windows-nt system-type)
      "convert.exe"
    (or (executable-find "convert")
	"/usr/X11R6/bin/convert"))
  "*Name of conversion program for thumbnails generation.
It must be 'convert'."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-setroot-command
  "xloadimage -onroot -fullscreen *"
  "Command to set the root window."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-relief 5
  "*Size of button-like border around thumbnails."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-margin 2
  "*Size of the margin around thumbnails.
This is where you see the cursor."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-thumbsdir-auto-clean t
  "If set, delete older file in the thumbnails directory.
Deletion is done at load time when the directory size is bigger
than 'thumbs-thumbsdir-max-size'."
  :type 'boolean
  :group 'thumbs)

(defcustom thumbs-image-resizing-step 10
  "Step by wich to resize image."
  :type 'string
  :group 'thumbs)

(defcustom thumbs-temp-dir
  "/tmp/"
  "Temporary directory to use.
Leaving it to default '/tmp/' can let another user
see some of your images."
  :type 'directory
  :group 'thumbs)

(defcustom thumbs-temp-prefix "emacsthumbs"
  "Prefix to add to temp files."
  :type 'string
  :group 'thumbs)

;; Initialize some variable, for later use.
(defvar thumbs-temp-file 
  (concat thumbs-temp-dir thumbs-temp-prefix) 
  "Temporary filesname for images.")

(defvar thumbs-current-tmp-filename 
  nil 
  "Temporary filename of current image.")
(defvar thumbs-current-image-filename 
  nil
  "Filename of current image.")
(defvar thumbs-current-image-size 
  nil
  "Size of current image.")
(defvar thumbs-image-num 
  nil
  "Number of current image.")
(defvar thumbs-current-dir 
  nil
  "Current directory.")
(defvar thumbs-markedL 
  nil
  "List of marked files.")

;; Make sure auto-image-file-mode is ON.
(auto-image-file-mode t)

;; Create the thumbs directory if it does not exists.
(setq thumbs-thumbsdir (expand-file-name thumbs-thumbsdir))

(when (not (file-directory-p thumbs-thumbsdir))
  (progn
    (make-directory thumbs-thumbsdir)
    (message "Creating thumbnails directory")))

(when (not (fboundp 'ignore-errors))
  (defmacro ignore-errors (&rest body)
    "Execute FORMS; if anz error occurs, return nil.
Otherwise, return result of last FORM."
    (let ((err (thumbs-gensym)))
      (list 'condition-case err (cons 'progn body) '(error nil))))) 

(when (not (fboundp 'time-less-p))
  (defun time-less-p (t1 t2)
    "Say whether time T1 is less than time T2."
    (or (< (car t1) (car t2))
	(and (= (car t1) (car t2))
	     (< (nth 1 t1) (nth 1 t2))))))

(when (not (fboundp 'caddar))
  (defun caddar (x)
    "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
    (car (cdr (cdr (car x))))))

(defvar thumbs-gensym-counter 0)

(defun thumbs-gensym (&optional arg)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"Thumbs\"."
  (let ((prefix (if (stringp arg) arg "Thumbs"))
	(num (if (integerp arg) arg
	       (prog1 
		   thumbs-gensym-counter
		 (setq thumbs-gensym-counter (1+ thumbs-gensym-counter))))))
    (make-symbol (format "%s%d" prefix num))))

(defun thumbs-cleanup-thumbsdir ()
  "Clean the thumbnails directory.
If the total size of all files in 'thumbs-thumbsdir' is bigger than
'thumbs-thumbsdir-max-size', files are deleted until the max size is
reached."
  (let* ((filesL
	  (sort
	   (mapcar
	    (lambda (f)
	      (let ((fattribsL (file-attributes f)))
		`(,(nth 4 fattribsL) ,(nth 7 fattribsL) ,f)))
	    (directory-files thumbs-thumbsdir t (image-file-name-regexp)))
	   '(lambda (l1 l2) (time-less-p (car l1)(car l2)))))
	 (dirsize (apply '+ (mapcar (lambda (x) (cadr x)) filesL))))
    (while (> dirsize thumbs-thumbsdir-max-size)
      (progn
	(message "Deleting file %s" (caddar filesL)))
      (delete-file (caddar filesL))
      (setq dirsize (- dirsize (cadar filesL)))
      (setq filesL (cdr filesL)))))

;; Check the thumbsnail directory size and clean it if necessary.
(when thumbs-thumbsdir-auto-clean
  (thumbs-cleanup-thumbsdir))

(defun thumbs-call-convert (filein fileout action
				   &optional arg output-format action-prefix)
  "Call the convert program.
FILEIN is the input file,
FILEOUT is the output file,
ACTION is the command to send to convert.
Optional argument are:
ARG any arguments to the ACTION command,
OUTPUT-FORMAT is the file format to output, default is jpeg
ACTION-PREFIX is the symbol to place before the ACTION command
              (default to '-' but can sometime be '+')."
  (let ((command (format "%s %s%s %s \"%s\" \"%s:%s\""
			 thumbs-conversion-program
			 (or action-prefix "-")
			 action
			 (or arg "")
			 filein
			 (or output-format "jpeg")
			 fileout)))
    (shell-command command)))

(defun thumbs-increment-image-size-element (n d)
  "Increment number N by D percent."
  (round (+ n (/ (* d n) 100))))

(defun thumbs-decrement-image-size-element (n d)
  "Decrement number N by D percent."
  (round (- n (/ (* d n) 100))))

(defun thumbs-increment-image-size (s)
  "Increment S (a cons of width x heigh)."
  (cons
   (thumbs-increment-image-size-element (car s)
					thumbs-image-resizing-step)
   (thumbs-increment-image-size-element (cdr s)
					thumbs-image-resizing-step)))
 
(defun thumbs-decrement-image-size (s)
  "Decrement S (a cons of width x heigh)."
  (cons
   (thumbs-decrement-image-size-element (car s)
					thumbs-image-resizing-step)
   (thumbs-decrement-image-size-element (cdr s)
					thumbs-image-resizing-step)))

(defun thumbs-resize-image (&optional increment size)
  "Resize image in current buffer.
if INCREMENT is set, make the image bigger, else smaller.
Or, alternatively, a SIZE may be specified."
  (interactive)
  ;; cleaning of old temp file
  (ignore-errors 
    (apply 'delete-file
	   (directory-files
	    thumbs-temp-dir t
	    thumbs-temp-prefix)))
  (let ((buffer-read-only nil)
	(x (if size
	       size
	     (if increment
		 (thumbs-increment-image-size
		  thumbs-current-image-size)
	       (thumbs-decrement-image-size
		thumbs-current-image-size))))
	(tmp (format "%s%s.jpg" thumbs-temp-file (thumbs-gensym))))
    (erase-buffer)
    (thumbs-call-convert thumbs-current-image-filename
			 tmp "sample"
			 (concat (number-to-string (car x)) "x"
				 (number-to-string (cdr x))))
    (thumbs-insert-image tmp 'jpeg 0)
    (setq thumbs-current-tmp-filename tmp)))

(defun thumbs-resize-interactive (width height)
  "Resize Image interactively to specified WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (thumbs-resize-image nil (cons width height)))
  
(defun thumbs-resize-image-size-down ()
  "Resize image (smaller)."
  (interactive)
  (thumbs-resize-image nil))

(defun thumbs-resize-image-size-up ()
  "Resize image (bigger)."
  (interactive)
  (thumbs-resize-image t))

(defun thumbs-subst-char-in-string (orig rep string)
  "Replace occurrences of character ORIG with character REP in STRING.
Return the resulting (new) string.  -- (defun borowed to Dave Love)"
  (let ((string (copy-sequence string))
	(l (length string))
	(i 0))
    (while (< i l)
      (if (= (aref string i) orig)
	  (aset string i rep))
      (setq i (1+ i)))
    string))

(defun thumbs-thumbname (img)
  "Return a thumbnail name for the image IMG."
  (concat thumbs-thumbsdir "/"
	  (thumbs-subst-char-in-string
	   ?\  ?\_
	   (apply
	    'concat
	    (split-string
	     (expand-file-name img) "/")))))

(defun thumbs-make-thumb (img)
  "Create the thumbnail for IMG."
  (let* ((fn (expand-file-name img))
	 (tn (thumbs-thumbname img)))
    (if (or (not (file-exists-p tn))
	    (not (equal (thumbs-file-size tn) thumbs-geometry)))
	(thumbs-call-convert fn tn "sample" thumbs-geometry))
    tn))
  
(defun thumbs-image-type (img)
  "Return image type from filename IMG."
  (cond ((string-match ".*\\.jpe?g\\'" img) 'jpeg)
	((string-match ".*\\.xpm\\'" img) 'xpm)
	((string-match ".*\\.xbm\\'" img) 'xbm)
	((string-match ".*\\.gif\\'" img) 'gif)
	((string-match ".*\\.bmp\\'" img) 'bmp)
	((string-match ".*\\.png\\'" img) 'png)
	((string-match ".*\\.tiff?\\'" img) 'tiff)))

(defun thumbs-file-size (img)
  (let ((i (image-size (find-image `((:type ,(thumbs-image-type img) :file ,img))) t)))
    (concat (number-to-string (round (car i)))
	    "x"
	    (number-to-string (round (cdr i))))))
  
;;;###autoload
(defun thumbs-find-thumb (img)
  "Display the thumbnail for IMG."
  (interactive "f")
  (find-file (thumbs-make-thumb img)))

(defun thumbs-insert-image (img type relief &optional marked)
  "Insert image IMG at point.
TYPE and RELIEF will be used in constructing the image; see `image'
in the emacs-lisp manual for further documentation.
if MARKED is non-nil, the image is marked."
  (let ((i `(image :type ,type
		   :file ,img
		   :relief ,relief
		   :conversion ,(if marked 'disabled)
		   :margin ,thumbs-margin)))
    (insert-image i)
    (setq thumbs-current-image-size
	  (image-size i t))))

(defun thumbs-insert-thumb (img &optional marked)
  "Insert the thumbnail for IMG at point.
if MARKED is non-nil, the image is marked"
  (thumbs-insert-image
   (thumbs-make-thumb img) 'jpeg thumbs-relief marked))

(defun thumbs-do-thumbs-insertion (L)
  "Insert all thumbs in list L."
  (setq thumbs-fileL nil)
  (let ((i 0))
    (while L
      (when (= 0 (mod (setq i (1+ i)) thumbs-per-line))
	(newline))
      (setq thumbs-fileL (cons (cons (point)
				     (car L))
			       thumbs-fileL))
      (thumbs-insert-thumb (car L)
			   (member (car L) thumbs-markedL))
      (setq L (cdr L)))))

(defun thumbs-show-thumbs-list (L &optional buffer-name same-window)
  (funcall (if same-window 'switch-to-buffer 'pop-to-buffer)
	   (or buffer-name "*THUMB-View*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (thumbs-mode)
    (make-variable-buffer-local 'thumbs-fileL)
    (setq thumbs-fileL nil)
    (thumbs-do-thumbs-insertion L)
    (goto-char (point-min))
    (setq thumbs-current-dir default-directory)
    (make-variable-buffer-local 'thumbs-current-dir)))

;;;###autoload
(defun thumbs-show-all-from-dir (dir &optional reg same-window)
  "Make a preview buffer for all images in DIR.
Optional argument REG to select file matching a regexp,
and SAME-WINDOW to show thumbs in the same window."
  (interactive "DDir: ")
  (thumbs-show-thumbs-list
   (directory-files dir t
		    (or reg (image-file-name-regexp)))
   (concat "*Thumbs: " dir) same-window))

;;;###autoload
(defun thumbs-dired-show-marked ()
  "In Dired, make a thumbs buffer with all marked files."
  (interactive)
  (thumbs-show-thumbs-list (dired-get-marked-files) nil t))

;;;###autoload
(defun thumbs-dired-show-all ()
  "In dired, make a thumbs buffer with all files in current directory."
  (interactive)
  (thumbs-show-all-from-dir default-directory nil t))

;;;###autoload
(defalias 'thumbs 'thumbs-show-all-from-dir)

(defun thumbs-find-image (img L &optional num otherwin)
  (funcall 
   (if otherwin 'switch-to-buffer-other-window 'switch-to-buffer)
   (concat "*Image: " (file-name-nondirectory img) " - "
	   (number-to-string (or num 0)) "*"))
  (thumbs-view-image-mode)
  (let ((inhibit-read-only t))
    (setq thumbs-current-image-filename img
	  thumbs-current-tmp-filename nil
	  thumbs-image-num (or num 0))
    (make-variable-buffer-local 'thumbs-current-image-filename)
    (make-variable-buffer-local 'thumbs-current-tmp-filename)
    (make-variable-buffer-local 'thumbs-current-image-size)
    (make-variable-buffer-local 'thumbs-image-num)
    (make-variable-buffer-local 'thumbs-fileL)
    (setq thumbs-fileL L)
    (delete-region (point-min)(point-max))
    (thumbs-insert-image img (thumbs-image-type img) 0)))

(defun thumbs-find-image-at-point (&optional img otherwin)
  "Display image IMG for thumbnail at point.
use another window it OTHERWIN is t."
  (interactive)
  (let* ((L thumbs-fileL)
	 (n (point))
	 (i (or img (cdr (assoc n L)))))
    (thumbs-find-image i L n otherwin)))

(defun thumbs-find-image-at-point-other-window ()
  "Display image for thumbnail at point in the preview buffer.
Open another window."
  (interactive)
  (thumbs-find-image-at-point nil t))

(defun thumbs-call-setroot-command (img)
  "Call the setroot program for IMG."
  (run-hooks 'thumbs-before-setroot-hook)
  (shell-command (replace-regexp-in-string
		  "\\*"
		  (shell-quote-argument (expand-file-name img))
		  thumbs-setroot-command nil t))
  (run-hooks 'thumbs-after-setroot-hook))
   
(defun thumbs-set-image-at-point-to-root-window ()
  "Set the image at point as the desktop wallpaper."
  (interactive)
  (thumbs-call-setroot-command (cdr (assoc (point) thumbs-fileL))))

(defun thumbs-set-root ()
  "Set the current image as root."
  (interactive)
  (thumbs-call-setroot-command
   (or thumbs-current-tmp-filename
       thumbs-current-image-filename)))

(defun thumbs-delete-images ()
  "Delete the image at point (and it's thumbnail) (or marked files if any)."
  (interactive)
  (let ((f (or thumbs-markedL (list (cdr (assoc (point) thumbs-fileL))))))
    (if (yes-or-no-p "Really delete %d files?" (length f))
	(progn
	  (mapcar (lambda (x)
		    (setq thumbs-fileL (delete (rassoc x thumbs-fileL) thumbs-fileL))
		    (delete-file x)
		    (delete-file (thumbs-thumbname x))) f)
	  (thumbs-redraw-buffer)))))

(defun thumbs-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (ignore-errors (delete-window (selected-window)))
    (kill-buffer buffer)))

(defun thumbs-show-image-num (num)
  "Show the image with number NUM."
  (let ((inhibit-read-only t))
    (delete-region (point-min)(point-max))
    (let ((i (cdr (assoc num thumbs-fileL))))
      (thumbs-insert-image i (thumbs-image-type i) 0)
      (sleep-for 2)
      (rename-buffer (concat "*Image: "
			     (file-name-nondirectory i)
			     " - "
			     (number-to-string num) "*")))
    (setq thumbs-image-num num
	  thumbs-current-image-filename i)))

(defun thumbs-next-image ()
  "Show next image."
  (interactive)
  (let* ((i (1+ thumbs-image-num))
	 (l (caar thumbs-fileL))
	 (num
	  (cond ((assoc i thumbs-fileL) i)
		((>= i l) 1)
		(t (1+ i)))))
    (thumbs-show-image-num num)))

(defun thumbs-previous-image ()
  "Show the previous image."
  (interactive)
  (let* ((i (- thumbs-image-num 1))
	 (l (caar thumbs-fileL))
	 (num
	  (cond ((assoc i thumbs-fileL) i)
		((<= i 1) l)
		(t (- i 1)))))
    (thumbs-show-image-num num)))

(defun thumbs-redraw-buffer ()
  "Redraw the current thumbs buffer."
  (let ((p (point))
	(inhibit-read-only t))
    (delete-region (point-min)(point-max))
    (thumbs-do-thumbs-insertion (reverse (mapcar 'cdr thumbs-fileL)))
    (goto-char (1+ p))))
  
(defun thumbs-mark ()
  "Mark the image at point."
  (interactive)
  (setq thumbs-markedL (cons (cdr (assoc (point) thumbs-fileL)) thumbs-markedL))
  (let ((inhibit-read-only t))
    (delete-char 1)
    (thumbs-insert-thumb (cdr (assoc (point) thumbs-fileL)) t))
  (when (eolp)(forward-char)))
  
;; Image modification routines

(defun thumbs-modify-image (action &optional arg)
  "Call convert to do ACTION on image with argument ARG.
ACTION and ARG should be legal convert command."
  (interactive "sAction: \nsValue: ")
  ;; cleaning of old temp file
  (mapc 'delete-file
	(directory-files
	 thumbs-temp-dir
	 t
	 thumbs-temp-prefix))
  (let ((buffer-read-only nil)
	(tmp (format "%s%s.jpg" thumbs-temp-file (thumbs-gensym))))
    (erase-buffer)
    (thumbs-call-convert thumbs-current-image-filename
			 tmp
			 action
			 (or arg ""))
    (thumbs-insert-image tmp 'jpeg 0)
    (setq thumbs-current-tmp-filename tmp)))

(defun thumbs-emboss-image (emboss)
  "Emboss the image with value EMBOSS."
  (interactive "nEmboss value: ")
  (if (or (< emboss 3)(> emboss 31)(evenp emboss))
      (error "Arg must be a odd number between 3 and 31"))
  (thumbs-modify-image "emboss" (number-to-string emboss)))

(defun thumbs-monochrome-image ()
  "Turn the image to monochrome."
  (interactive)
  (thumbs-modify-image "monochrome"))

(defun thumbs-negate-image ()
  "Negate the image."
  (interactive)
  (thumbs-modify-image "negate"))

(defun thumbs-rotate-left ()
  "Rotate the image 90 degrees counter-clockwise."
  (interactive)
  (thumbs-modify-image "rotate" "270"))

(defun thumbs-rotate-right ()
  "Rotate the image 90 degrees clockwise."
  (interactive)
  (thumbs-modify-image "rotate" "90"))

(defun thumbs-forward-char ()
  "Move forward one image."
  (interactive)
  (forward-char)
  (when (eolp)(forward-char))
  (thumbs-show-name))

(defun thumbs-backward-char ()
  "Move backward one image."
  (interactive)
  (forward-char -1)
  (thumbs-show-name))

(defun thumbs-forward-line ()
  "Move down one line."
  (interactive)
  (forward-line 1)
  (thumbs-show-name))

(defun thumbs-backward-line ()
  "Move up one line."
  (interactive)
  (forward-line -1)
  (thumbs-show-name))

(defun thumbs-show-name ()
  "Show the name of the current file."
  (interactive)
  (let ((f (cdr (assoc (point) thumbs-fileL))))
    (message "%s [%s]" f (thumbs-file-size f))))

(defun thumbs-save-current-image ()
  "Save the current image."
  (interactive)
  (let ((f (or thumbs-current-tmp-filename
	       thumbs-current-image-filename))
	(sa (read-from-minibuffer "save file as: "
				  thumbs-current-image-filename)))
    (copy-file f sa)))

(defun thumbs-dired ()
  "Use `dired' on the current thumbs directory."
  (interactive)
  (dired thumbs-current-dir))

;; thumbs-mode

(defvar thumbs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'thumbs-find-image-at-point)
    (define-key map [(meta return)] 'thumbs-find-image-at-point-other-window)
    (define-key map [(control return)] 'thumbs-set-image-at-point-to-root-window)
    (define-key map [delete] 'thumbs-delete-images)
    (define-key map [right] 'thumbs-forward-char)
    (define-key map [left] 'thumbs-backward-char)
    (define-key map [up] 'thumbs-backward-line)
    (define-key map [down] 'thumbs-forward-line)
    (define-key map "d" 'thumbs-dired)
    (define-key map "m" 'thumbs-mark)
    (define-key map "s" 'thumbs-show-name)
    (define-key map "q" 'thumbs-kill-buffer)
    map)
  "Keymap for `thumbs-mode'.")

(define-derived-mode thumbs-mode
  fundamental-mode "thumbs"
  "Preview images in a thumbnails buffer"
  (make-variable-buffer-local 'thumbs-markedL)
  (setq thumbs-markedL nil))

(defvar thumbs-view-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [prior] 'thumbs-previous-image)
    (define-key map [next] 'thumbs-next-image)
    (define-key map "-" 'thumbs-resize-image-size-down)
    (define-key map "+" 'thumbs-resize-image-size-up)
    (define-key map "<" 'thumbs-rotate-left)
    (define-key map ">" 'thumbs-rotate-right)
    (define-key map "e" 'thumbs-emboss-image)
    (define-key map "r" 'thumbs-resize-interactive)
    (define-key map "s" 'thumbs-save-current-image)
    (define-key map "q" 'thumbs-kill-buffer)
    (define-key map "w" 'thunbs-set-root)
    map)
  "Keymap for `thumbs-view-image-mode'.")

;; thumbs-view-image-mode
(define-derived-mode thumbs-view-image-mode
  fundamental-mode "image-view-mode")

;;;###autoload
(defun thumbs-dired-setroot ()
  "In dired, Call the setroot program on the image at point."
  (interactive)
  (thumbs-call-setroot-command (dired-get-filename)))

;; Modif to dired mode map
(define-key dired-mode-map "\C-ta" 'thumbs-dired-show-all)
(define-key dired-mode-map "\C-tm" 'thumbs-dired-show-marked)
(define-key dired-mode-map "\C-tw" 'thumbs-dired-setroot)

(provide 'thumbs)

;;; thumbs.el ends here


;;; arch-tag: f9ac1ef8-83fc-42c0-8069-1fae43fd2e5c
