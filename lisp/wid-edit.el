;;; wid-edit.el --- Functions for creating and using widgets.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: extensions
;; Version: 1.84
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `widget.el'.

;;; Code:

(require 'widget)

(eval-and-compile
  (require 'cl))

;;; Compatibility.

(eval-and-compile
  (autoload 'pp-to-string "pp")
  (autoload 'Info-goto-node "info")

  (when (string-match "XEmacs" emacs-version)
    (condition-case nil
	(require 'overlay)
      (error (load-library "x-overlay"))))
  
  (if (string-match "XEmacs" emacs-version)
      ;; XEmacs spell `intangible' as `atomic'.
      (defun widget-make-intangible (from to side)
	"Make text between FROM and TO atomic with regard to movement.
Third argument should be `start-open' if it should be sticky to the rear,
and `end-open' if it should sticky to the front."
	(require 'atomic-extents)
	(let ((ext (make-extent from to)))
	   ;; XEmacs doesn't understant different kinds of read-only, so
	   ;; we have to use extents instead.  
	  (put-text-property from to 'read-only nil)
	  (set-extent-property ext 'read-only t)
	  (set-extent-property ext 'start-open nil)
	  (set-extent-property ext 'end-open nil)
	  (set-extent-property ext side t)
	  (set-extent-property ext 'atomic t)))
    (defun widget-make-intangible (from to size)
      "Make text between FROM and TO intangible."
      (put-text-property from to 'intangible 'front)))
	  
;; The following should go away when bundled with Emacs.
  (condition-case ()
      (require 'custom)
    (error nil))

  (unless (and (featurep 'custom) (fboundp 'custom-declare-variable))
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args) nil)
    (defmacro defcustom (var value doc &rest args) 
      `(defvar ,var ,value ,doc))
    (defmacro defface (&rest args) nil)
    (define-widget-keywords :prefix :tag :load :link :options :type :group)
    (when (fboundp 'copy-face)
      (copy-face 'default 'widget-documentation-face)
      (copy-face 'bold 'widget-button-face)
      (copy-face 'italic 'widget-field-face)))

  (unless (fboundp 'event-point)
    ;; XEmacs function missing in Emacs.
    (defun event-point (event)
      "Return the character position of the given mouse-motion, button-press,
or button-release event.  If the event did not occur over a window, or did
not occur over text, then this returns nil.  Otherwise, it returns an index
into the buffer visible in the event's window."
      (posn-point (event-start event))))

  (unless (fboundp 'error-message-string)
    ;; Emacs function missing in XEmacs.
    (defun error-message-string (obj)
      "Convert an error value to an error message."
      (let ((buf (get-buffer-create " *error-message*")))
	(erase-buffer buf)
	(display-error obj buf)
	(buffer-string buf)))))

;;; Customization.

(defgroup widgets nil
  "Customization support for the Widget Library."
  :link '(custom-manual "(widget)Top")
  :link '(url-link :tag "Development Page" 
		   "http://www.dina.kvl.dk/~abraham/custom/")
  :prefix "widget-"
  :group 'extensions
  :group 'faces
  :group 'hypermedia)

(defface widget-documentation-face '((((class color)
				       (background dark))
				      (:foreground "lime green"))
				     (((class color)
				       (background light))
				      (:foreground "dark green"))
				     (t nil))
  "Face used for documentation text."
  :group 'widgets)

(defface widget-button-face '((t (:bold t)))
  "Face used for widget buttons."
  :group 'widgets)

(defcustom widget-mouse-face 'highlight
  "Face used for widget buttons when the mouse is above them."
  :type 'face
  :group 'widgets)

(defface widget-field-face '((((class grayscale color)
			       (background light))
			      (:background "light gray"))
			     (((class grayscale color)
			       (background dark))
			      (:background "dark gray"))
			     (t 
			      (:italic t)))
  "Face used for editable fields."
  :group 'widgets)

(defcustom widget-menu-max-size 40
  "Largest number of items allowed in a popup-menu.
Larger menus are read through the minibuffer."
  :group 'widgets
  :type 'integer)

;;; Utility functions.
;;
;; These are not really widget specific.

(defsubst widget-plist-member (plist prop)
  ;; Return non-nil if PLIST has the property PROP.
  ;; PLIST is a property list, which is a list of the form
  ;; (PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol.
  ;; Unlike `plist-get', this allows you to distinguish between a missing
  ;; property and a property with the value nil.
  ;; The value is actually the tail of PLIST whose car is PROP.
  (while (and plist (not (eq (car plist) prop)))
    (setq plist (cdr (cdr plist))))
  plist)

(defun widget-princ-to-string (object)
  ;; Return string representation of OBJECT, any Lisp object.
  ;; No quoting characters are used; no delimiters are printed around
  ;; the contents of strings.
  (save-excursion
    (set-buffer (get-buffer-create " *widget-tmp*"))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (princ object))
    (buffer-string)))

(defun widget-clear-undo ()
  "Clear all undo information."
  (buffer-disable-undo (current-buffer))
  (buffer-enable-undo))

(defun widget-choose (title items &optional event)
  "Choose an item from a list.

First argument TITLE is the name of the list.
Second argument ITEMS is an alist (NAME . VALUE).
Optional third argument EVENT is an input event.

The user is asked to choose between each NAME from the items alist,
and the VALUE of the chosen element will be returned.  If EVENT is a
mouse event, and the number of elements in items is less than
`widget-menu-max-size', a popup menu will be used, otherwise the
minibuffer."
  (cond ((and (< (length items) widget-menu-max-size)
	      event (fboundp 'x-popup-menu) window-system)
	 ;; We are in Emacs-19, pressed by the mouse
	 (x-popup-menu event
		       (list title (cons "" items))))
	((and (< (length items) widget-menu-max-size)
	      event (fboundp 'popup-menu) window-system)
	 ;; We are in XEmacs, pressed by the mouse
	 (let ((val (get-popup-menu-response
		     (cons title
			   (mapcar
			    (function
			     (lambda (x)
			       (vector (car x) (list (car x)) t)))
			    items)))))
	   (setq val (and val
			  (listp (event-object val))
			  (stringp (car-safe (event-object val)))
			  (car (event-object val))))
	   (cdr (assoc val items))))
	(t
	 (let ((val (completing-read (concat title ": ") items nil t)))
	   (if (stringp val)
	       (let ((try (try-completion val items)))
		 (when (stringp try)
		   (setq val try))
		 (cdr (assoc val items)))
	     nil)))))

(defun widget-get-sibling (widget)
  "Get the item WIDGET is assumed to toggle.
This is only meaningful for radio buttons or checkboxes in a list."
  (let* ((parent (widget-get widget :parent))
	 (children (widget-get parent :children))
	 child)
    (catch 'child
      (while children
	(setq child (car children)
	      children (cdr children))
	(when (eq (widget-get child :button) widget)
	  (throw 'child child)))
      nil)))

;;; Widget text specifications.
;; 
;; These functions are for specifying text properties. 

(defun widget-specify-none (from to)
  ;; Clear all text properties between FROM and TO.
  (set-text-properties from to nil))

(defun widget-specify-text (from to)
  ;; Default properties.
  (add-text-properties from to (list 'read-only t
				     'front-sticky t
				     'start-open t
				     'end-open t
				     'rear-nonsticky nil)))

(defun widget-specify-field (widget from to)
  ;; Specify editable button for WIDGET between FROM and TO.
  (widget-specify-field-update widget from to)

  ;; Make it possible to edit the front end of the field.
  (add-text-properties (1- from) from (list 'rear-nonsticky t
					      'end-open t
					      'invisible t))
  (when (or (string-match "\\(.\\|\n\\)%v" (widget-get widget :format))
	    (widget-get widget :hide-front-space))
    ;; WARNING: This is going to lose horrible if the character just
    ;; before the field can be modified (e.g. if it belongs to a
    ;; choice widget).  We try to compensate by checking the format
    ;; string, and hope the user hasn't changed the :create method.
    (widget-make-intangible (- from 2) from 'end-open))
  
  ;; Make it possible to edit back end of the field.
  (add-text-properties to (1+ to) (list 'front-sticky nil
					'read-only t
					'start-open t))

  (cond ((widget-get widget :size)
	 (put-text-property to (1+ to) 'invisible t)
	 (when (or (string-match "%v\\(.\\|\n\\)" (widget-get widget :format))
		   (widget-get widget :hide-rear-space))
	   ;; WARNING: This is going to lose horrible if the character just
	   ;; after the field can be modified (e.g. if it belongs to a
	   ;; choice widget).  We try to compensate by checking the format
	   ;; string, and hope the user hasn't changed the :create method.
	   (widget-make-intangible to (+ to 2) 'start-open)))
	((string-match "XEmacs" emacs-version)
	 ;; XEmacs does not allow you to insert before a read-only
	 ;; character, even if it is start.open.
	 ;; XEmacs does allow you to delete an read-only extent, so
	 ;; making the terminating newline read only doesn't help.
	 ;; I tried putting an invisible intangible read-only space
	 ;; before the newline, which gave really weird effects.
	 ;; So for now, we just have trust the user not to delete the
	 ;; newline.  
	 (put-text-property to (1+ to) 'read-only nil))))

(defun widget-specify-field-update (widget from to)
  ;; Specify editable button for WIDGET between FROM and TO.
  (let ((map (widget-get widget :keymap))
	(secret (widget-get widget :secret))
	(secret-to to)
	(size (widget-get widget :size))
	(face (or (widget-get widget :value-face)
		  'widget-field-face))
	(help-echo (widget-get widget :help-echo))
	(help-property (if (featurep 'balloon-help)
			   'balloon-help
			 'help-echo)))
    (unless (or (stringp help-echo) (null help-echo))
      (setq help-echo 'widget-mouse-help))

    (when secret 
      (while (and size
		  (not (zerop size))
		  (> secret-to from)
		  (eq (char-after (1- secret-to)) ?\ ))
	(setq secret-to (1- secret-to)))

      (save-excursion
	(goto-char from)
	(while (< (point) secret-to)
	  (let ((old (get-text-property (point) 'secret)))
	    (when old
	      (subst-char-in-region (point) (1+ (point)) secret old)))
	  (forward-char))))

    (set-text-properties from to (list 'field widget
				       'read-only nil
				       'keymap map
				       'local-map map
				       help-property help-echo
				       'face face))
    
    (when secret 
      (save-excursion
	(goto-char from)
	(while (< (point) secret-to)
	  (let ((old (following-char)))
	    (subst-char-in-region (point) (1+ (point)) old secret)
	    (put-text-property (point) (1+ (point)) 'secret old))
	  (forward-char))))

    (unless (widget-get widget :size)
      (add-text-properties to (1+ to) (list 'field widget
					    help-property help-echo
					    'face face)))
    (add-text-properties to (1+ to) (list 'local-map map
					  'keymap map))))

(defun widget-specify-button (widget from to)
  ;; Specify button for WIDGET between FROM and TO.
  (let ((face (widget-apply widget :button-face-get))
	(help-echo (widget-get widget :help-echo))
	(help-property (if (featurep 'balloon-help)
			   'balloon-help
			 'help-echo)))
    (unless (or (null help-echo) (stringp help-echo))
      (setq help-echo 'widget-mouse-help))
    (add-text-properties from to (list 'button widget
				       'mouse-face widget-mouse-face
				       'start-open t
				       'end-open t
				       help-property help-echo
				       'face face))))

(defun widget-mouse-help (extent)
  "Find mouse help string for button in extent."
  (let* ((widget (widget-at (extent-start-position extent)))
	 (help-echo (and widget (widget-get widget :help-echo))))
    (cond ((stringp help-echo)
	   help-echo)
	  ((and (symbolp help-echo) (fboundp help-echo)
		(stringp (setq help-echo (funcall help-echo widget))))
	   help-echo)
	  (t
	   (format "(widget %S :help-echo %S)" widget help-echo)))))

(defun widget-specify-sample (widget from to)
  ;; Specify sample for WIDGET between FROM and TO.
  (let ((face (widget-apply widget :sample-face-get)))
    (when face
      (add-text-properties from to (list 'start-open t
					 'end-open t
					 'face face)))))

(defun widget-specify-doc (widget from to)
  ;; Specify documentation for WIDGET between FROM and TO.
  (add-text-properties from to (list 'widget-doc widget
				     'face 'widget-documentation-face)))

(defmacro widget-specify-insert (&rest form)
  ;; Execute FORM without inheriting any text properties.
  `(save-restriction
     (let ((inhibit-read-only t)
	   result
	   after-change-functions)
       (insert "<>")
       (narrow-to-region (- (point) 2) (point))
       (widget-specify-none (point-min) (point-max))
       (goto-char (1+ (point-min)))
       (setq result (progn ,@form))
       (delete-region (point-min) (1+ (point-min)))
       (delete-region (1- (point-max)) (point-max))
       (goto-char (point-max))
       result)))

(defface widget-inactive-face '((((class grayscale color)
				  (background dark))
				 (:foreground "light gray"))
				(((class grayscale color)
				  (background light))
				 (:foreground "dark gray"))
				(t 
				 (:italic t)))
  "Face used for inactive widgets."
  :group 'widgets)

(defun widget-specify-inactive (widget from to)
  "Make WIDGET inactive for user modifications."
  (unless (widget-get widget :inactive)
    (let ((overlay (make-overlay from to nil t nil)))
      (overlay-put overlay 'face 'widget-inactive-face)
      (overlay-put overlay 'evaporate 't)
      (overlay-put overlay (if (string-match "XEmacs" emacs-version)
			       'read-only
			     'modification-hooks) '(widget-overlay-inactive))
      (widget-put widget :inactive overlay))))

(defun widget-overlay-inactive (&rest junk)
  "Ignoring the arguments, signal an error."
  (unless inhibit-read-only
    (error "Attempt to modify inactive widget")))


(defun widget-specify-active (widget)
  "Make WIDGET active for user modifications."
  (let ((inactive (widget-get widget :inactive)))
    (when inactive
      (delete-overlay inactive)
      (widget-put widget :inactive nil))))

;;; Widget Properties.

(defsubst widget-type (widget)
  "Return the type of WIDGET, a symbol."
  (car widget))

(defun widget-put (widget property value)
  "In WIDGET set PROPERTY to VALUE.
The value can later be retrived with `widget-get'."
  (setcdr widget (plist-put (cdr widget) property value)))

(defun widget-get (widget property)
  "In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'."
  (let ((missing t)
	value tmp)
    (while missing
      (cond ((setq tmp (widget-plist-member (cdr widget) property))
	     (setq value (car (cdr tmp))
		   missing nil))
	    ((setq tmp (car widget))
	     (setq widget (get tmp 'widget-type)))
	    (t 
	     (setq missing nil))))
    value))

(defun widget-member (widget property)
  "Non-nil iff there is a definition in WIDGET for PROPERTY."
  (cond ((widget-plist-member (cdr widget) property)
	 t)
	((car widget)
	 (widget-member (get (car widget) 'widget-type) property))
	(t nil)))

;;;###autoload
(defun widget-apply (widget property &rest args)
  "Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function."
  (apply (widget-get widget property) widget args))

(defun widget-value (widget)
  "Extract the current value of WIDGET."
  (widget-apply widget
		:value-to-external (widget-apply widget :value-get)))

(defun widget-value-set (widget value)
  "Set the current value of WIDGET to VALUE."
  (widget-apply widget
		:value-set (widget-apply widget
					 :value-to-internal value)))

(defun widget-match-inline (widget vals)
  ;; In WIDGET, match the start of VALS.
  (cond ((widget-get widget :inline)
	 (widget-apply widget :match-inline vals))
	((and vals
	      (widget-apply widget :match (car vals)))
	 (cons (list (car vals)) (cdr vals)))
	(t nil)))

(defun widget-apply-action (widget &optional event)
  "Apply :action in WIDGET in response to EVENT."
  (if (widget-apply widget :active)
      (widget-apply widget :action event)
    (error "Attempt to perform action on inactive widget")))
    
;;; Glyphs.

(defcustom widget-glyph-directory (concat data-directory "custom/")
  "Where widget glyphs are located.
If this variable is nil, widget will try to locate the directory
automatically. This does not work yet."
  :group 'widgets
  :type 'directory)

(defcustom widget-glyph-enable t
  "If non nil, use glyphs in images when available."
  :group 'widgets
  :type 'boolean)

(defun widget-glyph-insert (widget tag image)
  "In WIDGET, insert the text TAG or, if supported, IMAGE.
IMAGE should either be a glyph, or a name sans extension of an xpm or
xbm file located in `widget-glyph-directory'.

WARNING: If you call this with a glyph, and you want the user to be
able to activate the glyph, make sure it is unique.  If you use the
same glyph for multiple widgets, activating any of the glyphs will
cause the last created widget to be activated."
  (cond ((not (and (string-match "XEmacs" emacs-version)
		   widget-glyph-enable
		   (fboundp 'make-glyph)
		   image))
	 ;; We don't want or can't use glyphs.
	 (insert tag))
	((and (fboundp 'glyphp)
	      (glyphp image))
	 ;; Already a glyph.  Insert it.
	 (widget-glyph-insert-glyph widget tag image))
	(t
	 ;; A string.  Look it up in.
	 (let ((file (concat widget-glyph-directory 
			    (if (string-match "/\\'" widget-glyph-directory)
				""
			      "/")
			    image
			    (if (featurep 'xpm) ".xpm" ".xbm"))))
	   (if (file-readable-p file)
	       (widget-glyph-insert-glyph widget tag (make-glyph file))
	     ;; File not readable, give up.
	     (insert tag))))))

(defun widget-glyph-insert-glyph (widget tag glyph)
  "In WIDGET, with alternative text TAG, insert GLYPH."
  (set-glyph-image glyph (cons 'tty tag))
  (set-glyph-property glyph 'widget widget)
  (insert "*")
  (add-text-properties (1- (point)) (point) 
		       (list 'invisible t
			     'end-glyph glyph))
  (let ((help-echo (widget-get widget :help-echo)))
    (when help-echo
      (let ((extent (extent-at (1- (point)) nil 'end-glyph))
	    (help-property (if (featurep 'balloon-help)
			       'balloon-help
			     'help-echo)))
	(set-extent-property extent help-property (if (stringp help-echo)
						      help-echo
						    'widget-mouse-help))))))

;;; Creating Widgets.

;;;###autoload
(defun widget-create (type &rest args)
  "Create widget of TYPE.  
The optional ARGS are additional keyword arguments."
  (let ((widget (apply 'widget-convert type args)))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-and-convert (parent type &rest args)
  "As part of the widget PARENT, create a child widget TYPE.
The child is converted, using the keyword arguments ARGS."
  (let ((widget (apply 'widget-convert type args)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child (parent type)
  "Create widget of TYPE."
  (let ((widget (copy-list type)))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

(defun widget-create-child-value (parent type value)
  "Create widget of TYPE with value VALUE."
  (let ((widget (copy-list type)))
    (widget-put widget :value (widget-apply widget :value-to-internal value))
    (widget-put widget :parent parent)
    (unless (widget-get widget :indent)
      (widget-put widget :indent (+ (or (widget-get parent :indent) 0)
				    (or (widget-get widget :extra-offset) 0)
				    (widget-get parent :offset))))
    (widget-apply widget :create)
    widget))

;;;###autoload
(defun widget-delete (widget)
  "Delete WIDGET."
  (widget-apply widget :delete))

(defun widget-convert (type &rest args)
  "Convert TYPE to a widget without inserting it in the buffer. 
The optional ARGS are additional keyword arguments."
  ;; Don't touch the type.
  (let* ((widget (if (symbolp type) 
		     (list type)
		   (copy-list type)))
	 (current widget)
	 (keys args))
    ;; First set the :args keyword.
    (while (cdr current)		;Look in the type.
      (let ((next (car (cdr current))))
	(if (and (symbolp next) (eq (aref (symbol-name next) 0) ?:))
	    (setq current (cdr (cdr current)))
	  (setcdr current (list :args (cdr current)))
	  (setq current nil))))
    (while args				;Look in the args.
      (let ((next (nth 0 args)))
	(if (and (symbolp next) (eq (aref (symbol-name next) 0) ?:))
	    (setq args (nthcdr 2 args))
	  (widget-put widget :args args)
	  (setq args nil))))
    ;; Then Convert the widget.
    (setq type widget)
    (while type
      (let ((convert-widget (plist-get (cdr type) :convert-widget)))
	(if convert-widget
	    (setq widget (funcall convert-widget widget))))
      (setq type (get (car type) 'widget-type)))
    ;; Finally set the keyword args.
    (while keys 
      (let ((next (nth 0 keys)))
	(if (and (symbolp next) (eq (aref (symbol-name next) 0) ?:))
	    (progn 
	      (widget-put widget next (nth 1 keys))
	      (setq keys (nthcdr 2 keys)))
	  (setq keys nil))))
    ;; Convert the :value to internal format.
    (if (widget-member widget :value)
	(let ((value (widget-get widget :value)))
	  (widget-put widget
		      :value (widget-apply widget :value-to-internal value))))
    ;; Return the newly create widget.
    widget))

(defun widget-insert (&rest args)
  "Call `insert' with ARGS and make the text read only."
  (let ((inhibit-read-only t)
	after-change-functions
	(from (point)))
    (apply 'insert args)
    (widget-specify-text from (point))))

;;; Keymap and Commands.

(defvar widget-keymap nil
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.")

(unless widget-keymap 
  (setq widget-keymap (make-sparse-keymap))
  (define-key widget-keymap "\C-k" 'widget-kill-line)
  (define-key widget-keymap "\t" 'widget-forward)
  (define-key widget-keymap "\M-\t" 'widget-backward)
  (define-key widget-keymap [(shift tab)] 'widget-backward)
  (define-key widget-keymap [backtab] 'widget-backward)
  (if (string-match "XEmacs" (emacs-version))
      (progn 
	(define-key widget-keymap [button2] 'widget-button-click)
	(define-key widget-keymap [button1] 'widget-button1-click))
    (define-key widget-keymap [mouse-2] 'ignore)
    (define-key widget-keymap [down-mouse-2] 'widget-button-click))
  (define-key widget-keymap "\C-m" 'widget-button-press))

(defvar widget-global-map global-map
  "Keymap used for events the widget does not handle themselves.")
(make-variable-buffer-local 'widget-global-map)

(defvar widget-field-keymap nil
  "Keymap used inside an editable field.")

(unless widget-field-keymap 
  (setq widget-field-keymap (copy-keymap widget-keymap))
  (unless (string-match "XEmacs" (emacs-version))
    (define-key widget-field-keymap [menu-bar] 'nil))
  (define-key widget-field-keymap "\C-m" 'widget-field-activate)
  (define-key widget-field-keymap "\C-a" 'widget-beginning-of-line)
  (define-key widget-field-keymap "\C-e" 'widget-end-of-line)
  (set-keymap-parent widget-field-keymap global-map))

(defvar widget-text-keymap nil
  "Keymap used inside a text field.")

(unless widget-text-keymap 
  (setq widget-text-keymap (copy-keymap widget-keymap))
  (unless (string-match "XEmacs" (emacs-version))
    (define-key widget-text-keymap [menu-bar] 'nil))
  (define-key widget-text-keymap "\C-a" 'widget-beginning-of-line)
  (define-key widget-text-keymap "\C-e" 'widget-end-of-line)
  (set-keymap-parent widget-text-keymap global-map))

(defun widget-field-activate (pos &optional event)
  "Activate the ediable field at point."
  (interactive "@d")
  (let ((field (get-text-property pos 'field)))
    (if field
	(widget-apply-action field event)
      (call-interactively
       (lookup-key widget-global-map (this-command-keys))))))

(defun widget-button-click (event)
  "Activate button below mouse pointer."
  (interactive "@e")
  (cond ((and (fboundp 'event-glyph)
	      (event-glyph event))
	 (let ((widget (glyph-property (event-glyph event) 'widget)))
	   (if widget
	       (widget-apply-action widget event)
	     (message "You clicked on a glyph."))))
	((event-point event)
	 (let ((button (get-text-property (event-point event) 'button)))
	   (if button
	       (widget-apply-action button event)
	     (call-interactively 
	      (or (lookup-key widget-global-map [ button2 ])
		  (lookup-key widget-global-map [ down-mouse-2 ])
		  (lookup-key widget-global-map [ mouse-2]))))))
	(t
	 (message "You clicked somewhere weird."))))

(defun widget-button1-click (event)
  "Activate glyph below mouse pointer."
  (interactive "@e")
  (if (and (fboundp 'event-glyph)
	   (event-glyph event))
      (let ((widget (glyph-property (event-glyph event) 'widget)))
	(if widget
	    (widget-apply-action widget event)
	  (message "You clicked on a glyph.")))
    (call-interactively (lookup-key widget-global-map (this-command-keys)))))

(defun widget-button-press (pos &optional event)
  "Activate button at POS."
  (interactive "@d")
  (let ((button (get-text-property pos 'button)))
    (if button
	(widget-apply-action button event)
      (let ((command (lookup-key widget-global-map (this-command-keys))))
	(when (commandp command)
	  (call-interactively command))))))

(defun widget-move (arg)
  "Move point to the ARG next field or button.
ARG may be negative to move backward."
  (while (> arg 0)
    (setq arg (1- arg))
    (let ((next (cond ((get-text-property (point) 'button)
		       (next-single-property-change (point) 'button))
		      ((get-text-property (point) 'field)
		       (next-single-property-change (point) 'field))
		      (t
		       (point)))))
      (if (null next)			; Widget extends to end. of buffer
	  (setq next (point-min)))
      (let ((button (next-single-property-change next 'button))
	    (field (next-single-property-change next 'field)))
	(cond ((or (get-text-property next 'button)
		   (get-text-property next 'field))
	       (goto-char next))
	      ((and button field)
	       (goto-char (min button field)))
	      (button (goto-char button))
	      (field (goto-char field))
	      (t
	       (let ((button (next-single-property-change (point-min) 'button))
		     (field (next-single-property-change (point-min) 'field)))
		 (cond ((and button field) (goto-char (min button field)))
		       (button (goto-char button))
		       (field (goto-char field))
		       (t
			(error "No buttons or fields found"))))))
	(setq button (widget-at (point)))
	(if (and button (widget-get button :tab-order)
		 (< (widget-get button :tab-order) 0))
	    (setq arg (1+ arg))))))
  (while (< arg 0)
    (if (= (point-min) (point))
	(forward-char 1))
    (setq arg (1+ arg))
    (let ((previous (cond ((get-text-property (1- (point)) 'button)
			   (previous-single-property-change (point) 'button))
			  ((get-text-property (1- (point)) 'field)
			   (previous-single-property-change (point) 'field))
			  (t
			   (point)))))
      (if (null previous)		; Widget extends to beg. of buffer
	  (setq previous (point-max)))
      (let ((button (previous-single-property-change previous 'button))
	    (field (previous-single-property-change previous 'field)))
	(cond ((and button field)
	       (goto-char (max button field)))
	      (button (goto-char button))
	      (field (goto-char field))
	      (t
	       (let ((button (previous-single-property-change
			      (point-max) 'button))
		     (field (previous-single-property-change
			     (point-max) 'field)))
		 (cond ((and button field) (goto-char (max button field)))
		       (button (goto-char button))
		       (field (goto-char field))
		       (t
			(error "No buttons or fields found"))))))))
    (let ((button (previous-single-property-change (point) 'button))
	  (field (previous-single-property-change (point) 'field)))
      (cond ((and button field)
	     (goto-char (max button field)))
	    (button (goto-char button))
	    (field (goto-char field)))
      (setq button (widget-at (point)))
      (if (and button (widget-get button :tab-order)
	       (< (widget-get button :tab-order) 0))
	  (setq arg (1- arg)))))
  (widget-echo-help (point))
  (run-hooks 'widget-move-hook))

(defun widget-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'widget-forward-hook)
  (widget-move arg))

(defun widget-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'widget-backward-hook)
  (widget-move (- arg)))

(defun widget-beginning-of-line ()
  "Go to beginning of field or beginning of line, whichever is first."
  (interactive)
  (let ((bol (save-excursion (beginning-of-line) (point)))
	(prev (previous-single-property-change (point) 'field)))
    (goto-char (max bol (or prev bol)))))

(defun widget-end-of-line ()
  "Go to end of field or end of line, whichever is first."
  (interactive)
  (let ((bol (save-excursion (end-of-line) (point)))
	(prev (next-single-property-change (point) 'field)))
    (goto-char (min bol (or prev bol)))))

(defun widget-kill-line ()
  "Kill to end of field or end of line, whichever is first."
  (interactive)
  (let ((field (get-text-property (point) 'field))
	(newline (save-excursion (search-forward "\n")))
	(next (next-single-property-change (point) 'field)))
    (if (and field (> newline next))
	(kill-region (point) next)
      (call-interactively 'kill-line))))

;;; Setting up the buffer.

(defvar widget-field-new nil)
;; List of all newly created editable fields in the buffer.
(make-variable-buffer-local 'widget-field-new)

(defvar widget-field-list nil)
;; List of all editable fields in the buffer.
(make-variable-buffer-local 'widget-field-list)

(defun widget-setup ()
  "Setup current buffer so editing string widgets works."
  (let ((inhibit-read-only t)
	(after-change-functions nil)
	field)
    (while widget-field-new
      (setq field (car widget-field-new)
	    widget-field-new (cdr widget-field-new)
	    widget-field-list (cons field widget-field-list))
      (let ((from (widget-get field :value-from))
	    (to (widget-get field :value-to)))
	(widget-specify-field field from to)
	(move-marker from (1- from))
	(move-marker to (1+ to)))))
  (widget-clear-undo)
  ;; We need to maintain text properties and size of the editing fields.
  (make-local-variable 'after-change-functions)
  (if widget-field-list
      (setq after-change-functions '(widget-after-change))
    (setq after-change-functions nil)))

(defvar widget-field-last nil)
;; Last field containing point.
(make-variable-buffer-local 'widget-field-last)

(defvar widget-field-was nil)
;; The widget data before the change.
(make-variable-buffer-local 'widget-field-was)

(defun widget-field-find (pos)
  ;; Find widget whose editing field is located at POS.
  ;; Return nil if POS is not inside and editing field.
  ;; 
  ;; This is only used in `widget-field-modified', since ordinarily
  ;; you would just test the field property.
  (let ((fields widget-field-list)
	field found)
    (while fields
      (setq field (car fields)
	    fields (cdr fields))
      (let ((from (widget-get field :value-from))
	    (to (widget-get field :value-to)))
	(if (and from to (< from pos) (> to  pos))
	    (setq fields nil
		  found field))))
    found))

(defun widget-after-change (from to old)
  ;; Adjust field size and text properties.
  (condition-case nil
      (let ((field (widget-field-find from))
	    (inhibit-read-only t))
	(cond ((null field))
	      ((not (eq field (widget-field-find to)))
	       (debug)
	       (message "Error: `widget-after-change' called on two fields"))
	      (t
	       (let ((size (widget-get field :size)))
		 (if size 
		     (let ((begin (1+ (widget-get field :value-from)))
			   (end (1- (widget-get field :value-to))))
		       (widget-specify-field-update field begin end)
		       (cond ((< (- end begin) size)
			      ;; Field too small.
			      (save-excursion
				(goto-char end)
				(insert-char ?\  (- (+ begin size) end))
				(widget-specify-field-update field 
							     begin
							     (+ begin size))))
			     ((> (- end begin) size)
			      ;; Field too large and
			      (if (or (< (point) (+ begin size))
				      (> (point) end))
				  ;; Point is outside extra space.
				  (setq begin (+ begin size))
				;; Point is within the extra space.
				(setq begin (point)))
			      (save-excursion
				(goto-char end)
				(while (and (eq (preceding-char) ?\ )
					    (> (point) begin))
				  (delete-backward-char 1))))))
		   (widget-specify-field-update field from to)))
	       (widget-apply field :notify field))))
    (error (debug))))

;;; Widget Functions
;;
;; These functions are used in the definition of multiple widgets. 

(defun widget-children-value-delete (widget)
  "Delete all :children and :buttons in WIDGET."
  (mapcar 'widget-delete (widget-get widget :children))
  (widget-put widget :children nil)
  (mapcar 'widget-delete (widget-get widget :buttons))
  (widget-put widget :buttons nil))

(defun widget-types-convert-widget (widget)
  "Convert :args as widget types in WIDGET."
  (widget-put widget :args (mapcar 'widget-convert (widget-get widget :args)))
  widget)

;;; The `default' Widget.

(define-widget 'default nil
  "Basic widget other widgets are derived from."
  :value-to-internal (lambda (widget value) value)
  :value-to-external (lambda (widget value) value)
  :create 'widget-default-create
  :indent nil
  :offset 0
  :format-handler 'widget-default-format-handler
  :button-face-get 'widget-default-button-face-get 
  :sample-face-get 'widget-default-sample-face-get 
  :delete 'widget-default-delete
  :value-set 'widget-default-value-set
  :value-inline 'widget-default-value-inline
  :menu-tag-get 'widget-default-menu-tag-get
  :validate (lambda (widget) nil)
  :active 'widget-default-active
  :activate 'widget-specify-active
  :deactivate 'widget-default-deactivate
  :action 'widget-default-action
  :notify 'widget-default-notify)

(defun widget-default-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-specify-insert
   (let ((from (point))
	 (tag (widget-get widget :tag))
	 (glyph (widget-get widget :tag-glyph))
	 (doc (widget-get widget :doc))
	 button-begin button-end
	 sample-begin sample-end
	 doc-begin doc-end
	 value-pos)
     (insert (widget-get widget :format))
     (goto-char from)
     ;; Parse escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (cond ((eq escape ?%)
		(insert "%"))
	       ((eq escape ?\[)
		(setq button-begin (point)))
	       ((eq escape ?\])
		(setq button-end (point)))
	       ((eq escape ?\{)
		(setq sample-begin (point)))
	       ((eq escape ?\})
		(setq sample-end (point)))
	       ((eq escape ?n)
		(when (widget-get widget :indent)
		  (insert "\n")
		  (insert-char ?  (widget-get widget :indent))))
	       ((eq escape ?t)
		(cond (glyph 
		       (widget-glyph-insert widget (or tag "image") glyph))
		      (tag
		       (insert tag))
		      (t
		       (let ((standard-output (current-buffer)))
			 (princ (widget-get widget :value))))))
	       ((eq escape ?d)
		(when doc
		  (setq doc-begin (point))
		  (insert doc)
		  (while (eq (preceding-char) ?\n)
		    (delete-backward-char 1))
		  (insert "\n")
		  (setq doc-end (point))))
	       ((eq escape ?v)
		(if (and button-begin (not button-end))
		    (widget-apply widget :value-create)
		  (setq value-pos (point))))
	       (t 
		(widget-apply widget :format-handler escape)))))
     ;; Specify button, sample, and doc, and insert value.
     (and button-begin button-end
	  (widget-specify-button widget button-begin button-end))
     (and sample-begin sample-end
	  (widget-specify-sample widget sample-begin sample-end))
     (and doc-begin doc-end
	  (widget-specify-doc widget doc-begin doc-end))
     (when value-pos
       (goto-char value-pos)
       (widget-apply widget :value-create)))
   (let ((from (copy-marker (point-min)))
	 (to (copy-marker (point-max))))
     (widget-specify-text from to)
     (set-marker-insertion-type from t)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to))))

(defun widget-default-format-handler (widget escape)
  ;; We recognize the %h escape by default.
  (let* ((buttons (widget-get widget :buttons))
	 (doc-property (widget-get widget :documentation-property))
	 (doc-try (cond ((widget-get widget :doc))
			((symbolp doc-property)
			 (documentation-property (widget-get widget :value)
						 doc-property))
			(t
			 (funcall doc-property (widget-get widget :value)))))
	 (doc-text (and (stringp doc-try)
			(> (length doc-try) 1)
			doc-try)))
    (cond ((eq escape ?h)
	   (when doc-text
	     (and (eq (preceding-char) ?\n)
		  (widget-get widget :indent)
		  (insert-char ?  (widget-get widget :indent)))
	     ;; The `*' in the beginning is redundant.
	     (when (eq (aref doc-text  0) ?*)
	       (setq doc-text (substring doc-text 1)))
	     ;; Get rid of trailing newlines.
	     (when (string-match "\n+\\'" doc-text)
	       (setq doc-text (substring doc-text 0 (match-beginning 0))))
	     (push (if (string-match "\n." doc-text)
		       ;; Allow multiline doc to be hiden.
		       (widget-create-child-and-convert
			widget 'widget-help 
			:doc (progn
			       (string-match "\\`.*" doc-text)
			       (match-string 0 doc-text))
			:widget-doc doc-text
			"?")
		     ;; A single line is just inserted.
		     (widget-create-child-and-convert
		      widget 'item :format "%d" :doc doc-text nil))
		   buttons)))
	  (t 
	   (error "Unknown escape `%c'" escape)))
    (widget-put widget :buttons buttons)))

(defun widget-default-button-face-get (widget)
  ;; Use :button-face or widget-button-face
  (or (widget-get widget :button-face) 'widget-button-face))

(defun widget-default-sample-face-get (widget)
  ;; Use :sample-face.
  (widget-get widget :sample-face))

(defun widget-default-delete (widget)
  ;; Remove widget from the buffer.
  (let ((from (widget-get widget :from))
	(to (widget-get widget :to))
	(inhibit-read-only t)
	after-change-functions)
    (widget-apply widget :value-delete)
    (when (< from to)
      ;; Kludge: this doesn't need to be true for empty formats.
      (delete-region from to))
    (set-marker from nil)
    (set-marker to nil)))

(defun widget-default-value-set (widget value)
  ;; Recreate widget with new value.
  (save-excursion
    (goto-char (widget-get widget :from))
    (widget-apply widget :delete)
    (widget-put widget :value value)
    (widget-apply widget :create)))

(defun widget-default-value-inline (widget)
  ;; Wrap value in a list unless it is inline.
  (if (widget-get widget :inline)
      (widget-value widget)
    (list (widget-value widget))))

(defun widget-default-menu-tag-get (widget)
  ;; Use tag or value for menus.
  (or (widget-get widget :menu-tag)
      (widget-get widget :tag)
      (widget-princ-to-string (widget-get widget :value))))

(defun widget-default-active (widget)
  "Return t iff this widget active (user modifiable)."
  (and (not (widget-get widget :inactive))
       (let ((parent (widget-get widget :parent)))
	 (or (null parent) 
	     (widget-apply parent :active)))))

(defun widget-default-deactivate (widget)
  "Make WIDGET inactive for user modifications."
  (widget-specify-inactive widget
			   (widget-get widget :from)
			   (widget-get widget :to)))

(defun widget-default-action (widget &optional event)
  ;; Notify the parent when a widget change
  (let ((parent (widget-get widget :parent)))
    (when parent
      (widget-apply parent :notify widget event))))

(defun widget-default-notify (widget child &optional event)
  ;; Pass notification to parent.
  (widget-default-action widget event))

;;; The `item' Widget.

(define-widget 'item 'default
  "Constant items for inclusion in other widgets."
  :convert-widget 'widget-item-convert-widget
  :value-create 'widget-item-value-create
  :value-delete 'ignore
  :value-get 'widget-item-value-get
  :match 'widget-item-match
  :match-inline 'widget-item-match-inline
  :action 'widget-item-action
  :format "%t\n")

(defun widget-item-convert-widget (widget)
  ;; Initialize :value from :args in WIDGET.
  (let ((args (widget-get widget :args)))
    (when args 
      (widget-put widget :value (widget-apply widget
					      :value-to-internal (car args)))
      (widget-put widget :args nil)))
  widget)

(defun widget-item-value-create (widget)
  ;; Insert the printed representation of the value.
  (let ((standard-output (current-buffer)))
    (princ (widget-get widget :value))))

(defun widget-item-match (widget value)
  ;; Match if the value is the same.
  (equal (widget-get widget :value) value))

(defun widget-item-match-inline (widget values)
  ;; Match if the value is the same.
  (let ((value (widget-get widget :value)))
    (and (listp value)
	 (<= (length value) (length values))
	 (let ((head (subseq values 0 (length value))))
	   (and (equal head value)
		(cons head (subseq values (length value))))))))

(defun widget-item-action (widget &optional event)
  ;; Just notify itself.
  (widget-apply widget :notify widget event))

(defun widget-item-value-get (widget)
  ;; Items are simple.
  (widget-get widget :value))

;;; The `push-button' Widget.

(defcustom widget-push-button-gui t
  "If non nil, use GUI push buttons when available."
  :group 'widgets
  :type 'boolean)

;; Cache already created GUI objects.
(defvar widget-push-button-cache nil)

(define-widget 'push-button 'item
  "A pushable button."
  :value-create 'widget-push-button-value-create
  :text-format "[%s]"
  :format "%[%v%]")

(defun widget-push-button-value-create (widget)
  ;; Insert text representing the `on' and `off' states.
  (let* ((tag (or (widget-get widget :tag)
		  (widget-get widget :value)))
	 (text (format (widget-get widget :text-format) tag))
	 (gui (cdr (assoc tag widget-push-button-cache))))
    (if (and (fboundp 'make-gui-button)
	     (fboundp 'make-glyph)
	     widget-push-button-gui
	     (fboundp 'device-on-window-system-p)
	     (device-on-window-system-p)
	     (string-match "XEmacs" emacs-version))
	(progn 
	  (unless gui
	    (setq gui (make-gui-button tag 'widget-gui-action widget))
	    (push (cons tag gui) widget-push-button-cache))
	  (widget-glyph-insert-glyph widget text
				     (make-glyph (car (aref gui 1)))))
      (insert text))))

(defun widget-gui-action (widget)
  "Apply :action for WIDGET."
  (widget-apply-action widget (this-command-keys)))

;;; The `link' Widget.

(define-widget 'link 'item
  "An embedded link."
  :help-echo "Follow the link."
  :format "%[_%t_%]")

;;; The `info-link' Widget.

(define-widget 'info-link 'link
  "A link to an info file."
  :action 'widget-info-link-action)

(defun widget-info-link-action (widget &optional event)
  "Open the info node specified by WIDGET."
  (Info-goto-node (widget-value widget)))

;;; The `url-link' Widget.

(define-widget 'url-link 'link
  "A link to an www page."
  :action 'widget-url-link-action)

(defun widget-url-link-action (widget &optional event)
  "Open the url specified by WIDGET."
  (require 'browse-url)
  (funcall browse-url-browser-function (widget-value widget)))

;;; The `editable-field' Widget.

(define-widget 'editable-field 'default
  "An editable text field."
  :convert-widget 'widget-item-convert-widget
  :keymap widget-field-keymap
  :format "%v"
  :value ""
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "No match"
  :value-create 'widget-field-value-create
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)

;; History of field minibuffer edits.
(defvar widget-field-history nil)

(defun widget-field-action (widget &optional event)
  ;; Edit the value in the minibuffer.
  (let ((tag (widget-apply widget :menu-tag-get))
	(invalid (widget-apply widget :validate)))
    (when invalid
      (error (widget-get invalid :error)))
    (widget-value-set widget 
		      (widget-apply widget 
				    :value-to-external
				    (read-string (concat tag ": ") 
						 (widget-apply 
						  widget
						  :value-to-internal
						  (widget-value widget))
						 'widget-field-history)))
    (widget-apply widget :notify widget event)
    (widget-setup)))

(defun widget-field-validate (widget)
  ;; Valid if the content matches `:valid-regexp'.
  (save-excursion
    (let ((value (widget-apply widget :value-get))
	  (regexp (widget-get widget :valid-regexp)))
      (if (string-match regexp value)
	  nil
	widget))))

(defun widget-field-value-create (widget)
  ;; Create an editable text field.
  (insert " ")
  (let ((size (widget-get widget :size))
	(value (widget-get widget :value))
	(from (point)))
    (insert value)
    (and size
	 (< (length value) size)
	 (insert-char ?\  (- size (length value))))
    (unless (memq widget widget-field-list)
      (setq widget-field-new (cons widget widget-field-new)))
    (widget-put widget :value-to (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget :value-to) nil)
    (if (null size)
	(insert ?\n)
      (insert ?\ ))
    (widget-put widget :value-from (copy-marker from))
    (set-marker-insertion-type (widget-get widget :value-from) t)))

(defun widget-field-value-delete (widget)
  ;; Remove the widget from the list of active editing fields.
  (setq widget-field-list (delq widget widget-field-list))
  ;; These are nil if the :format string doesn't contain `%v'.
  (when (widget-get widget :value-from)
    (set-marker (widget-get widget :value-from) nil))
  (when (widget-get widget :value-from)
    (set-marker (widget-get widget :value-to) nil)))

(defun widget-field-value-get (widget)
  ;; Return current text in editing field.
  (let ((from (widget-get widget :value-from))
	(to (widget-get widget :value-to))
	(size (widget-get widget :size))
	(secret (widget-get widget :secret))
	(old (current-buffer)))
    (if (and from to)
	(progn 
	  (set-buffer (marker-buffer from))
	  (setq from (1+ from)
		to (1- to))
	  (while (and size
		      (not (zerop size))
		      (> to from)
		      (eq (char-after (1- to)) ?\ ))
	    (setq to (1- to)))
	  (let ((result (buffer-substring-no-properties from to)))
	    (when secret
	      (let ((index 0))
		(while (< (+ from index) to)
		  (aset result index
			(get-text-property (+ from index) 'secret))
		  (setq index (1+ index)))))
	    (set-buffer old)
	    result))
      (widget-get widget :value))))

(defun widget-field-match (widget value)
  ;; Match any string.
  (stringp value))

;;; The `text' Widget.

(define-widget 'text 'editable-field
  :keymap widget-text-keymap
  "A multiline text area.")

;;; The `menu-choice' Widget.

(define-widget 'menu-choice 'default
  "A menu of options."
  :convert-widget  'widget-types-convert-widget
  :format "%[%t%]: %v"
  :case-fold t
  :tag "choice"
  :void '(item :format "invalid (%t)\n")
  :value-create 'widget-choice-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-choice-value-get
  :value-inline 'widget-choice-value-inline
  :action 'widget-choice-action
  :error "Make a choice"
  :validate 'widget-choice-validate
  :match 'widget-choice-match
  :match-inline 'widget-choice-match-inline)

(defun widget-choice-value-create (widget)
  ;; Insert the first choice that matches the value.
  (let ((value (widget-get widget :value))
	(args (widget-get widget :args))
	current)
    (while args
      (setq current (car args)
	    args (cdr args))
      (when (widget-apply current :match value)
	(widget-put widget :children (list (widget-create-child-value
					    widget current value)))
	(widget-put widget :choice current)
	(setq args nil
	      current nil)))
    (when current
      (let ((void (widget-get widget :void)))
	(widget-put widget :children (list (widget-create-child-and-convert
					    widget void :value value)))
	(widget-put widget :choice void)))))

(defun widget-choice-value-get (widget)
  ;; Get value of the child widget.
  (widget-value (car (widget-get widget :children))))

(defun widget-choice-value-inline (widget)
  ;; Get value of the child widget.
  (widget-apply (car (widget-get widget :children)) :value-inline))

(defun widget-choice-action (widget &optional event)
  ;; Make a choice.
  (let ((args (widget-get widget :args))
	(old (widget-get widget :choice))
	(tag (widget-apply widget :menu-tag-get))
	(completion-ignore-case (widget-get widget :case-fold))
	current choices)
    ;; Remember old value.
    (if (and old (not (widget-apply widget :validate)))
	(let* ((external (widget-value widget))
	       (internal (widget-apply old :value-to-internal external)))
	  (widget-put old :value internal)))
    ;; Find new choice.
    (setq current
	  (cond ((= (length args) 0)
		 nil)
		((= (length args) 1)
		 (nth 0 args))
		((and (= (length args) 2)
		      (memq old args))
		 (if (eq old (nth 0 args))
		     (nth 1 args)
		   (nth 0 args)))
		(t
		 (while args
		   (setq current (car args)
			 args (cdr args))
		   (setq choices
			 (cons (cons (widget-apply current :menu-tag-get)
				     current)
			       choices)))
		 (widget-choose tag (reverse choices) event))))
    (when current
      (widget-value-set widget 
			(widget-apply current :value-to-external
				      (widget-get current :value)))
    (widget-apply widget :notify widget event)
    (widget-setup)))
  ;; Notify parent.
  (widget-apply widget :notify widget event)
  (widget-clear-undo))

(defun widget-choice-validate (widget)
  ;; Valid if we have made a valid choice.
  (let ((void (widget-get widget :void))
	(choice (widget-get widget :choice))
	(child (car (widget-get widget :children))))
    (if (eq void choice)
	widget
      (widget-apply child :validate))))

(defun widget-choice-match (widget value)
  ;; Matches if one of the choices matches.
  (let ((args (widget-get widget :args))
	current found)
    (while (and args (not found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-apply current :match value)))
    found))

(defun widget-choice-match-inline (widget values)
  ;; Matches if one of the choices matches.
  (let ((args (widget-get widget :args))
	current found)
    (while (and args (null found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-match-inline current values)))
    found))

;;; The `toggle' Widget.

(define-widget 'toggle 'item
  "Toggle between two states."
  :format "%[%v%]\n"
  :value-create 'widget-toggle-value-create
  :action 'widget-toggle-action
  :match (lambda (widget value) t)
  :on "on"
  :off "off")

(defun widget-toggle-value-create (widget)
  ;; Insert text representing the `on' and `off' states.
  (if (widget-value widget)
      (widget-glyph-insert widget 
			   (widget-get widget :on) 
			   (widget-get widget :on-glyph))
    (widget-glyph-insert widget
			 (widget-get widget :off)
			 (widget-get widget :off-glyph))))

(defun widget-toggle-action (widget &optional event)
  ;; Toggle value.
  (widget-value-set widget (not (widget-value widget)))
  (widget-apply widget :notify widget event))
  
;;; The `checkbox' Widget.

(define-widget 'checkbox 'toggle
  "A checkbox toggle."
  :format "%[%v%]"
  :on "[X]"
  :on-glyph "check1"
  :off "[ ]"
  :off-glyph "check0"
  :action 'widget-checkbox-action)

(defun widget-checkbox-action (widget &optional event)
  "Toggle checkbox, notify parent, and set active state of sibling."
  (widget-toggle-action widget event)
  (let ((sibling (widget-get-sibling widget)))
    (when sibling
      (if (widget-value widget)
	  (widget-apply sibling :activate)
	(widget-apply sibling :deactivate)))))

;;; The `checklist' Widget.

(define-widget 'checklist 'default
  "A multiple choice widget."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :offset 4
  :entry-format "%b %v"
  :menu-tag "checklist"
  :greedy nil
  :value-create 'widget-checklist-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-checklist-value-get
  :validate 'widget-checklist-validate
  :match 'widget-checklist-match
  :match-inline 'widget-checklist-match-inline)

(defun widget-checklist-value-create (widget)
  ;; Insert all values
  (let ((alist (widget-checklist-match-find widget (widget-get widget :value)))
	(args (widget-get widget :args)))
    (while args 
      (widget-checklist-add-item widget (car args) (assq (car args) alist))
      (setq args (cdr args)))
    (widget-put widget :children (nreverse (widget-get widget :children)))))

(defun widget-checklist-add-item (widget type chosen)
  ;; Create checklist item in WIDGET of type TYPE.
  ;; If the item is checked, CHOSEN is a cons whose cdr is the value.
  (and (eq (preceding-char) ?\n)
       (widget-get widget :indent)
       (insert-char ?  (widget-get widget :indent)))
  (widget-specify-insert 
   (let* ((children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
	  (from (point))
	  child button)
     (insert (widget-get widget :entry-format))
     (goto-char from)
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\([bv%]\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (cond ((eq escape ?%)
		(insert "%"))
	       ((eq escape ?b)
		(setq button (apply 'widget-create-child-and-convert
				    widget 'checkbox
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child
		      (cond ((not chosen)
			     (let ((child (widget-create-child widget type)))
			       (widget-apply child :deactivate)
			       child))
			    ((widget-get type :inline)
			     (widget-create-child-value
			      widget type (cdr chosen)))
			    (t
			     (widget-create-child-value
			      widget type (car (cdr chosen)))))))
	       (t 
		(error "Unknown escape `%c'" escape)))))
     ;; Update properties.
     (and button child (widget-put child :button button))
     (and button (widget-put widget :buttons (cons button buttons)))
     (and child (widget-put widget :children (cons child children))))))

(defun widget-checklist-match (widget values)
  ;; All values must match a type in the checklist.
  (and (listp values)
       (null (cdr (widget-checklist-match-inline widget values)))))

(defun widget-checklist-match-inline (widget values)
  ;; Find the values which match a type in the checklist.
  (let ((greedy (widget-get widget :greedy))
	(args (copy-list (widget-get widget :args)))
	found rest)
    (while values
      (let ((answer (widget-checklist-match-up args values)))
	(cond (answer 
	       (let ((vals (widget-match-inline answer values)))
		 (setq found (append found (car vals))
		       values (cdr vals)
		       args (delq answer args))))
	      (greedy
	       (setq rest (append rest (list (car values)))
		     values (cdr values)))
	      (t 
	       (setq rest (append rest values)
		     values nil)))))
    (cons found rest)))

(defun widget-checklist-match-find (widget vals)
  ;; Find the vals which match a type in the checklist.
  ;; Return an alist of (TYPE MATCH).
  (let ((greedy (widget-get widget :greedy))
	(args (copy-list (widget-get widget :args)))
	found)
    (while vals
      (let ((answer (widget-checklist-match-up args vals)))
	(cond (answer 
	       (let ((match (widget-match-inline answer vals)))
		 (setq found (cons (cons answer (car match)) found)
		       vals (cdr match)
		       args (delq answer args))))
	      (greedy
	       (setq vals (cdr vals)))
	      (t 
	       (setq vals nil)))))
    found))

(defun widget-checklist-match-up (args vals)
  ;; Rerturn the first type from ARGS that matches VALS.
  (let (current found)
    (while (and args (null found))
      (setq current (car args)
	    args (cdr args)
	    found (widget-match-inline current vals)))
    (if found
	current
      nil)))

(defun widget-checklist-value-get (widget)
  ;; The values of all selected items.
  (let ((children (widget-get widget :children))
	child result)
    (while children 
      (setq child (car children)
	    children (cdr children))
      (if (widget-value (widget-get child :button))
	  (setq result (append result (widget-apply child :value-inline)))))
    result))

(defun widget-checklist-validate (widget)
  ;; Ticked chilren must be valid.
  (let ((children (widget-get widget :children))
	child button found)
    (while (and children (not found))
      (setq child (car children)
	    children (cdr children)
	    button (widget-get child :button)
	    found (and (widget-value button)
		       (widget-apply child :validate))))
    found))

;;; The `option' Widget

(define-widget 'option 'checklist
  "An widget with an optional item."
  :inline t)

;;; The `choice-item' Widget.

(define-widget 'choice-item 'item
  "Button items that delegate action events to their parents."
  :action 'widget-choice-item-action
  :format "%[%t%] \n")

(defun widget-choice-item-action (widget &optional event)
  ;; Tell parent what happened.
  (widget-apply (widget-get widget :parent) :action event))

;;; The `radio-button' Widget.

(define-widget 'radio-button 'toggle
  "A radio button for use in the `radio' widget."
  :notify 'widget-radio-button-notify
  :format "%[%v%]"
  :on "(*)"
  :on-glyph "radio1"
  :off "( )"
  :off-glyph "radio0")

(defun widget-radio-button-notify (widget child &optional event)
  ;; Tell daddy.
  (widget-apply (widget-get widget :parent) :action widget event))

;;; The `radio-button-choice' Widget.

(define-widget 'radio-button-choice 'default
  "Select one of multiple options."
  :convert-widget 'widget-types-convert-widget
  :offset 4
  :format "%v"
  :entry-format "%b %v"
  :menu-tag "radio"
  :value-create 'widget-radio-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-radio-value-get
  :value-inline 'widget-radio-value-inline
  :value-set 'widget-radio-value-set
  :error "You must push one of the buttons"
  :validate 'widget-radio-validate
  :match 'widget-choice-match
  :match-inline 'widget-choice-match-inline
  :action 'widget-radio-action)

(defun widget-radio-value-create (widget)
  ;; Insert all values
  (let ((args (widget-get widget :args))
	arg)
    (while args 
      (setq arg (car args)
	    args (cdr args))
      (widget-radio-add-item widget arg))))

(defun widget-radio-add-item (widget type)
  "Add to radio widget WIDGET a new radio button item of type TYPE."
  ;; (setq type (widget-convert type))
  (and (eq (preceding-char) ?\n)
       (widget-get widget :indent)
       (insert-char ?  (widget-get widget :indent)))
  (widget-specify-insert 
   (let* ((value (widget-get widget :value))
	  (children (widget-get widget :children))
	  (buttons (widget-get widget :buttons))
	  (button-args (or (widget-get type :sibling-args)
			   (widget-get widget :button-args)))
	  (from (point))
	  (chosen (and (null (widget-get widget :choice))
		       (widget-apply type :match value)))
	  child button)
     (insert (widget-get widget :entry-format))
     (goto-char from)
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\([bv%]\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (cond ((eq escape ?%)
		(insert "%"))
	       ((eq escape ?b)
		(setq button (apply 'widget-create-child-and-convert
				    widget 'radio-button 
				    :value (not (null chosen))
				    button-args)))
	       ((eq escape ?v)
		(setq child (if chosen
				(widget-create-child-value
				 widget type value)
			      (widget-create-child widget type)))
		(unless chosen 
		  (widget-apply child :deactivate)))
	       (t 
		(error "Unknown escape `%c'" escape)))))
     ;; Update properties.
     (when chosen
       (widget-put widget :choice type))
     (when button 
       (widget-put child :button button)
       (widget-put widget :buttons (nconc buttons (list button))))
     (when child
       (widget-put widget :children (nconc children (list child))))
     child)))

(defun widget-radio-value-get (widget)
  ;; Get value of the child widget.
  (let ((chosen (widget-radio-chosen widget)))
    (and chosen (widget-value chosen))))

(defun widget-radio-chosen (widget)
  "Return the widget representing the chosen radio button."
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (let* ((button (widget-get current :button))
	     (value (widget-apply button :value-get)))
	(when value
	  (setq found current
		children nil))))
    found))

(defun widget-radio-value-inline (widget)
  ;; Get value of the child widget.
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (let* ((button (widget-get current :button))
	     (value (widget-apply button :value-get)))
	(when value
	  (setq found (widget-apply current :value-inline)
		children nil))))
    found))

(defun widget-radio-value-set (widget value)
  ;; We can't just delete and recreate a radio widget, since children
  ;; can be added after the original creation and won't be recreated
  ;; by `:create'.
  (let ((children (widget-get widget :children))
	current found)
    (while children
      (setq current (car children)
	    children (cdr children))
      (let* ((button (widget-get current :button))
	     (match (and (not found)
			 (widget-apply current :match value))))
	(widget-value-set button match)
	(if match 
	    (progn 
	      (widget-value-set current value)
	      (widget-apply current :activate))
	  (widget-apply current :deactivate))
	(setq found (or found match))))))

(defun widget-radio-validate (widget)
  ;; Valid if we have made a valid choice.
  (let ((children (widget-get widget :children))
	current found button)
    (while (and children (not found))
      (setq current (car children)
	    children (cdr children)
	    button (widget-get current :button)
	    found (widget-apply button :value-get)))
    (if found
	(widget-apply current :validate)
      widget)))

(defun widget-radio-action (widget child event)
  ;; Check if a radio button was pressed.
  (let ((children (widget-get widget :children))
	(buttons (widget-get widget :buttons))
	current)
    (when (memq child buttons)
      (while children
	(setq current (car children)
	      children (cdr children))
	(let* ((button (widget-get current :button)))
	  (cond ((eq child button)
		 (widget-value-set button t)
		 (widget-apply current :activate))
		((widget-value button)
		 (widget-value-set button nil)
		 (widget-apply current :deactivate)))))))
  ;; Pass notification to parent.
  (widget-apply widget :notify child event))

;;; The `insert-button' Widget.

(define-widget 'insert-button 'push-button
  "An insert button for the `editable-list' widget."
  :tag "INS"
  :help-echo "Insert a new item into the list at this position."
  :action 'widget-insert-button-action)

(defun widget-insert-button-action (widget &optional event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent) 
		:insert-before (widget-get widget :widget)))

;;; The `delete-button' Widget.

(define-widget 'delete-button 'push-button
  "A delete button for the `editable-list' widget."
  :tag "DEL"
  :help-echo "Delete this item from the list."
  :action 'widget-delete-button-action)

(defun widget-delete-button-action (widget &optional event)
  ;; Ask the parent to insert a new item.
  (widget-apply (widget-get widget :parent) 
		:delete-at (widget-get widget :widget)))

;;; The `editable-list' Widget.

(defcustom widget-editable-list-gui nil
  "If non nil, use GUI push-buttons in editable list when available."
  :type 'boolean
  :group 'widgets)

(define-widget 'editable-list 'default
  "A variable list of widgets of the same type."
  :convert-widget 'widget-types-convert-widget
  :offset 12
  :format "%v%i\n"
  :format-handler 'widget-editable-list-format-handler
  :entry-format "%i %d %v"
  :menu-tag "editable-list"
  :value-create 'widget-editable-list-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-editable-list-value-get
  :validate 'widget-editable-list-validate
  :match 'widget-editable-list-match
  :match-inline 'widget-editable-list-match-inline
  :insert-before 'widget-editable-list-insert-before
  :delete-at 'widget-editable-list-delete-at)

(defun widget-editable-list-format-handler (widget escape)
  ;; We recognize the insert button.
  (let ((widget-push-button-gui widget-editable-list-gui))
    (cond ((eq escape ?i)
	   (and (widget-get widget :indent)
		(insert-char ?  (widget-get widget :indent)))
	   (apply 'widget-create-child-and-convert 
		  widget 'insert-button
		  (widget-get widget :append-button-args)))
	  (t 
	   (widget-default-format-handler widget escape)))))

(defun widget-editable-list-value-create (widget)
  ;; Insert all values
  (let* ((value (widget-get widget :value))
	 (type (nth 0 (widget-get widget :args)))
	 (inlinep (widget-get type :inline))
	 children)
    (widget-put widget :value-pos (copy-marker (point)))
    (set-marker-insertion-type (widget-get widget :value-pos) t)
    (while value
      (let ((answer (widget-match-inline type value)))
	(if answer
	    (setq children (cons (widget-editable-list-entry-create
				  widget
				  (if inlinep
				      (car answer)
				    (car (car answer)))
				  t)
				 children)
		  value (cdr answer))
	  (setq value nil))))
    (widget-put widget :children (nreverse children))))

(defun widget-editable-list-value-get (widget)
  ;; Get value of the child widget.
  (apply 'append (mapcar (lambda (child) (widget-apply child :value-inline))
			 (widget-get widget :children))))

(defun widget-editable-list-validate (widget)
  ;; All the chilren must be valid.
  (let ((children (widget-get widget :children))
	child found)
    (while (and children (not found))
      (setq child (car children)
	    children (cdr children)
	    found (widget-apply child :validate)))
    found))

(defun widget-editable-list-match (widget value)
  ;; Value must be a list and all the members must match the type.
  (and (listp value)
       (null (cdr (widget-editable-list-match-inline widget value)))))

(defun widget-editable-list-match-inline (widget value)
  (let ((type (nth 0 (widget-get widget :args)))
	(ok t)
	found)
    (while (and value ok)
      (let ((answer (widget-match-inline type value)))
	(if answer 
	    (setq found (append found (car answer))
		  value (cdr answer))
	  (setq ok nil))))
    (cons found value)))

(defun widget-editable-list-insert-before (widget before)
  ;; Insert a new child in the list of children.
  (save-excursion
    (let ((children (widget-get widget :children))
	  (inhibit-read-only t)
	  after-change-functions)
      (cond (before 
	     (goto-char (widget-get before :entry-from)))
	    (t
	     (goto-char (widget-get widget :value-pos))))
      (let ((child (widget-editable-list-entry-create 
		    widget nil nil)))
	(when (< (widget-get child :entry-from) (widget-get widget :from))
	  (set-marker (widget-get widget :from)
		      (widget-get child :entry-from)))
	(widget-specify-text (widget-get child :entry-from)
			     (widget-get child :entry-to))
	(if (eq (car children) before)
	    (widget-put widget :children (cons child children))
	  (while (not (eq (car (cdr children)) before))
	    (setq children (cdr children)))
	  (setcdr children (cons child (cdr children)))))))
  (widget-setup)
 widget (widget-apply widget :notify widget))

(defun widget-editable-list-delete-at (widget child)
  ;; Delete child from list of children.
  (save-excursion
    (let ((buttons (copy-list (widget-get widget :buttons)))
	  button
	  (inhibit-read-only t)
	  after-change-functions)
      (while buttons
	(setq button (car buttons)
	      buttons (cdr buttons))
	(when (eq (widget-get button :widget) child)
	  (widget-put widget
		      :buttons (delq button (widget-get widget :buttons)))
	  (widget-delete button))))
    (let ((entry-from (widget-get child :entry-from))
	  (entry-to (widget-get child :entry-to))
	  (inhibit-read-only t)
	  after-change-functions)
      (widget-delete child)
      (delete-region entry-from entry-to)
      (set-marker entry-from nil)
      (set-marker entry-to nil))
    (widget-put widget :children (delq child (widget-get widget :children))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun widget-editable-list-entry-create (widget value conv)
  ;; Create a new entry to the list.
  (let ((type (nth 0 (widget-get widget :args)))
	(widget-push-button-gui widget-editable-list-gui)
	child delete insert)
    (widget-specify-insert 
     (save-excursion
       (and (widget-get widget :indent)
	    (insert-char ?  (widget-get widget :indent)))
       (insert (widget-get widget :entry-format)))
     ;; Parse % escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (aref (match-string 1) 0)))
	 (replace-match "" t t)
	 (cond ((eq escape ?%)
		(insert "%"))
	       ((eq escape ?i)
		(setq insert (apply 'widget-create-child-and-convert
				    widget 'insert-button
				    (widget-get widget :insert-button-args))))
	       ((eq escape ?d)
		(setq delete (apply 'widget-create-child-and-convert
				    widget 'delete-button
				    (widget-get widget :delete-button-args))))
	       ((eq escape ?v)
		(if conv
		    (setq child (widget-create-child-value 
				 widget type value))
		  (setq child (widget-create-child widget type))))
	       (t 
		(error "Unknown escape `%c'" escape)))))
     (widget-put widget 
		 :buttons (cons delete 
				(cons insert
				      (widget-get widget :buttons))))
     (let ((entry-from (copy-marker (point-min)))
	   (entry-to (copy-marker (point-max))))
       (widget-specify-text entry-from entry-to)
       (set-marker-insertion-type entry-from t)
       (set-marker-insertion-type entry-to nil)
       (widget-put child :entry-from entry-from)
       (widget-put child :entry-to entry-to)))
    (widget-put insert :widget child)
    (widget-put delete :widget child)
    child))

;;; The `group' Widget.

(define-widget 'group 'default
  "A widget which group other widgets inside."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :value-create 'widget-group-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-editable-list-value-get
  :validate 'widget-editable-list-validate
  :match 'widget-group-match
  :match-inline 'widget-group-match-inline)

(defun widget-group-value-create (widget)
  ;; Create each component.
  (let ((args (widget-get widget :args))
	(value (widget-get widget :value))
	arg answer children)
    (while args
      (setq arg (car args)
	    args (cdr args)
	    answer (widget-match-inline arg value)
	    value (cdr answer))
      (and (eq (preceding-char) ?\n)
	   (widget-get widget :indent)
	   (insert-char ?  (widget-get widget :indent)))
      (push (cond ((null answer)
		   (widget-create-child widget arg))
		  ((widget-get arg :inline)
		   (widget-create-child-value widget arg  (car answer)))
		  (t
		   (widget-create-child-value widget arg  (car (car answer)))))
	    children))
    (widget-put widget :children (nreverse children))))

(defun widget-group-match (widget values)
  ;; Match if the components match.
  (and (listp values)
       (let ((match (widget-group-match-inline widget values)))
	 (and match (null (cdr match))))))

(defun widget-group-match-inline (widget vals)
  ;; Match if the components match.
  (let ((args (widget-get widget :args))
	argument answer found)
    (while args
      (setq argument (car args)
	    args (cdr args)
	    answer (widget-match-inline argument vals))
      (if answer 
	  (setq vals (cdr answer)
		found (append found (car answer)))
	(setq vals nil
	      args nil)))
    (if answer
	(cons found vals)
      nil)))

;;; The `widget-help' Widget.

(define-widget 'widget-help 'push-button
  "The widget documentation button."
  :format "%[[%t]%] %d"
  :help-echo "Toggle display of documentation."
  :action 'widget-help-action)

(defun widget-help-action (widget &optional event)
  "Toggle documentation for WIDGET."
  (let ((old (widget-get widget :doc))
	(new (widget-get widget :widget-doc)))
    (widget-put widget :doc new)
    (widget-put widget :widget-doc old))
  (widget-value-set widget (widget-value widget)))

;;; The Sexp Widgets.

(define-widget 'const 'item
  "An immutable sexp."
  :format "%t\n%d")

(define-widget 'function-item 'item
  "An immutable function name."
  :format "%v\n%h"
  :documentation-property (lambda (symbol)
			    (condition-case nil
				(documentation symbol t)
			      (error nil))))

(define-widget 'variable-item 'item
  "An immutable variable name."
  :format "%v\n%h"
  :documentation-property 'variable-documentation)

(define-widget 'string 'editable-field
  "A string"
  :tag "String"
  :format "%[%t%]: %v")

(define-widget 'regexp 'string
  "A regular expression."
  ;; Should do validation.
  :tag "Regexp")

(define-widget 'file 'string
  "A file widget.  
It will read a file name from the minibuffer when activated."
  :format "%[%t%]: %v"
  :tag "File"
  :action 'widget-file-action)

(defun widget-file-action (widget &optional event)
  ;; Read a file name from the minibuffer.
  (let* ((value (widget-value widget))
	 (dir (file-name-directory value))
	 (file (file-name-nondirectory value))
	 (menu-tag (widget-apply widget :menu-tag-get))
	 (must-match (widget-get widget :must-match))
	 (answer (read-file-name (concat menu-tag ": (default `" value "') ")
				 dir nil must-match file)))
    (widget-value-set widget (abbreviate-file-name answer))
    (widget-apply widget :notify widget event)
    (widget-setup)))

(define-widget 'directory 'file
  "A directory widget.  
It will read a directory name from the minibuffer when activated."
  :tag "Directory")

(define-widget 'symbol 'string
  "A lisp symbol."
  :value nil
  :tag "Symbol"
  :match (lambda (widget value) (symbolp value))
  :value-to-internal (lambda (widget value)
		       (if (symbolp value)
			   (symbol-name value)
			 value))
  :value-to-external (lambda (widget value)
		       (if (stringp value)
			   (intern value)
			 value)))

(define-widget 'function 'sexp
  ;; Should complete on functions.
  "A lisp function."
  :tag "Function")

(define-widget 'variable 'symbol
  ;; Should complete on variables.
  "A lisp variable."
  :tag "Variable")

(define-widget 'sexp 'string
  "An arbitrary lisp expression."
  :tag "Lisp expression"
  :value nil
  :validate 'widget-sexp-validate
  :match (lambda (widget value) t)
  :value-to-internal 'widget-sexp-value-to-internal
  :value-to-external (lambda (widget value) (read value)))

(defun widget-sexp-value-to-internal (widget value)
  ;; Use pp for printer representation.
  (let ((pp (pp-to-string value)))
    (while (string-match "\n\\'" pp)
      (setq pp (substring pp 0 -1)))
    (if (or (string-match "\n\\'" pp)
	    (> (length pp) 40))
	(concat "\n" pp)
      pp)))

(defun widget-sexp-validate (widget)
  ;; Valid if we can read the string and there is no junk left after it.
  (save-excursion
    (let ((buffer (set-buffer (get-buffer-create " *Widget Scratch*"))))
      (erase-buffer)
      (insert (widget-apply widget :value-get))
      (goto-char (point-min))
      (condition-case data
	  (let ((value (read buffer)))
	    (if (eobp)
		(if (widget-apply widget :match value)
		    nil
		  (widget-put widget :error (widget-get widget :type-error))
		  widget)
	      (widget-put widget
			  :error (format "Junk at end of expression: %s"
					 (buffer-substring (point)
							   (point-max))))
	      widget))
	(error (widget-put widget :error (error-message-string data))
	       widget)))))

(define-widget 'integer 'sexp
  "An integer."
  :tag "Integer"
  :value 0
  :type-error "This field should contain an integer"
  :value-to-internal (lambda (widget value)
		       (if (integerp value) 
			   (prin1-to-string value)
			 value))
  :match (lambda (widget value) (integerp value)))

(define-widget 'character 'string
  "An character."
  :tag "Character"
  :value 0
  :size 1 
  :format "%{%t%}: %v\n"
  :type-error "This field should contain a character"
  :value-to-internal (lambda (widget value)
		       (if (integerp value) 
			   (char-to-string value)
			 value))
  :value-to-external (lambda (widget value)
		       (if (stringp value)
			   (aref value 0)
			 value))
  :match (lambda (widget value) (integerp value)))

(define-widget 'number 'sexp
  "A floating point number."
  :tag "Number"
  :value 0.0
  :type-error "This field should contain a number"
  :value-to-internal (lambda (widget value)
		       (if (numberp value)
			   (prin1-to-string value)
			 value))
  :match (lambda (widget value) (numberp value)))

(define-widget 'list 'group
  "A lisp list."
  :tag "List"
  :format "%{%t%}:\n%v")

(define-widget 'vector 'group
  "A lisp vector."
  :tag "Vector"
  :format "%{%t%}:\n%v"
  :match 'widget-vector-match
  :value-to-internal (lambda (widget value) (append value nil))
  :value-to-external (lambda (widget value) (apply 'vector value)))

(defun widget-vector-match (widget value) 
  (and (vectorp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

(define-widget 'cons 'group
  "A cons-cell."
  :tag "Cons-cell"
  :format "%{%t%}:\n%v"
  :match 'widget-cons-match
  :value-to-internal (lambda (widget value)
		       (list (car value) (cdr value)))
  :value-to-external (lambda (widget value)
		       (cons (nth 0 value) (nth 1 value))))

(defun widget-cons-match (widget value) 
  (and (consp value)
       (widget-group-match widget
			   (widget-apply widget :value-to-internal value))))

(define-widget 'choice 'menu-choice
  "A union of several sexp types."
  :tag "Choice"
  :format "%[%t%]: %v")

(define-widget 'radio 'radio-button-choice
  "A union of several sexp types."
  :tag "Choice"
  :format "%{%t%}:\n%v")

(define-widget 'repeat 'editable-list
  "A variable length homogeneous list."
  :tag "Repeat"
  :format "%{%t%}:\n%v%i\n")

(define-widget 'set 'checklist
  "A list of members from a fixed set."
  :tag "Set"
  :format "%{%t%}:\n%v")

(define-widget 'boolean 'toggle
  "To be nil or non-nil, that is the question."
  :tag "Boolean"
  :format "%{%t%}: %[%v%]\n")

;;; The `color' Widget.

(define-widget 'color-item 'choice-item
  "A color name (with sample)."
  :format "%v (%{sample%})\n"
  :sample-face-get 'widget-color-item-button-face-get)

(defun widget-color-item-button-face-get (widget)
  ;; We create a face from the value.
  (require 'facemenu)
  (condition-case nil
      (facemenu-get-face (intern (concat "fg:" (widget-value widget))))
    (error 'default)))

(define-widget 'color 'push-button
  "Choose a color name (with sample)."
  :format "%[%t%]: %v"
  :tag "Color"
  :value "black"
  :value-create 'widget-color-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'widget-color-value-get
  :value-set 'widget-color-value-set
  :action 'widget-color-action
  :match 'widget-field-match
  :tag "Color")

(defvar widget-color-choice-list nil)
;; Variable holding the possible colors.

(defun widget-color-choice-list ()
  (unless widget-color-choice-list
    (setq widget-color-choice-list 
	  (mapcar '(lambda (color) (list color))
		  (x-defined-colors))))
  widget-color-choice-list)

(defun widget-color-value-create (widget)
  (let ((child (widget-create-child-and-convert
		widget 'color-item (widget-get widget :value))))
    (widget-put widget :children (list child))))

(defun widget-color-value-get (widget)
  ;; Pass command to first child.
  (widget-apply (car (widget-get widget :children)) :value-get))

(defun widget-color-value-set (widget value)
  ;; Pass command to first child.
  (widget-apply (car (widget-get widget :children)) :value-set value))

(defvar widget-color-history nil
  "History of entered colors")

(defun widget-color-action (widget &optional event)
  ;; Prompt for a color.
  (let* ((tag (widget-apply widget :menu-tag-get))
	 (prompt (concat tag ": "))
	 (answer (cond ((string-match "XEmacs" emacs-version)
			(read-color prompt))
		       ((fboundp 'x-defined-colors)
			(completing-read (concat tag ": ")
					 (widget-color-choice-list) 
					 nil nil nil 'widget-color-history))
		       (t
			(read-string prompt (widget-value widget))))))
    (unless (zerop (length answer))
      (widget-value-set widget answer)
      (widget-apply widget :notify widget event)
      (widget-setup))))

;;; The Help Echo

(defun widget-echo-help-mouse ()
  "Display the help message for the widget under the mouse.
Enable with (run-with-idle-timer 1 t 'widget-echo-help-mouse)"
  (let* ((pos (mouse-position))
	 (frame (car pos))
	 (x (car (cdr pos)))
	 (y (cdr (cdr pos)))
	 (win (window-at x y frame))
	 (where (coordinates-in-window-p (cons x y) win)))
    (when (consp where)
      (save-window-excursion
	(progn ; save-excursion
	  (select-window win)
	  (let* ((result (compute-motion (window-start win)
					 '(0 . 0)
					 (window-end win)
					 where
					 (window-width win)
					 (cons (window-hscroll) 0)
					 win)))
	    (when (and (eq (nth 1 result) x)
		       (eq (nth 2 result) y))
	      (widget-echo-help (nth 0 result))))))))
  (unless track-mouse
    (setq track-mouse t)
    (add-hook 'post-command-hook 'widget-stop-mouse-tracking)))

(defun widget-stop-mouse-tracking (&rest args)
  "Stop the mouse tracking done while idle."
  (remove-hook 'post-command-hook 'widget-stop-mouse-tracking)
  (setq track-mouse nil))

(defun widget-at (pos)
  "The button or field at POS."
  (or (get-text-property pos 'button)
      (get-text-property pos 'field)))

(defun widget-echo-help (pos)
  "Display the help echo for widget at POS."
  (let* ((widget (widget-at pos))
	 (help-echo (and widget (widget-get widget :help-echo))))
    (cond ((stringp help-echo)
	   (message "%s" help-echo))
	  ((and (symbolp help-echo) (fboundp help-echo)
		(stringp (setq help-echo (funcall help-echo widget))))
	   (message "%s" help-echo)))))

;;; The End:

(provide 'wid-edit)

;; wid-edit.el ends here
