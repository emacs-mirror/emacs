;;; cus-edit.el --- Tools for customization Emacs.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: 1.71
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; See `custom.el'.

;;; Code:

(require 'cus-face)
(require 'wid-edit)
(require 'easymenu)

(define-widget-keywords :custom-prefixes :custom-menu :custom-show
  :custom-magic :custom-state :custom-level :custom-form
  :custom-set :custom-save :custom-reset-current :custom-reset-saved 
  :custom-reset-factory)

;;; Customization Groups.

(defgroup emacs nil
  "Customization of the One True Editor."
  :link '(custom-manual "(emacs)Top"))

;; Most of these groups are stolen from `finder.el',
(defgroup editing nil
  "Basic text editing facilities."
  :group 'emacs)

(defgroup abbrev nil
  "Abbreviation handling, typing shortcuts, macros."
  :tag "Abbreviations"
  :group 'editing)

(defgroup matching nil
  "Various sorts of searching and matching."
  :group 'editing)

(defgroup emulations nil
  "Emulations of other editors."
  :group 'editing)

(defgroup mouse nil
  "Mouse support."
  :group 'editing)

(defgroup outlines nil
  "Support for hierarchical outlining."
  :group 'editing)

(defgroup external nil
  "Interfacing to external utilities."
  :group 'emacs)

(defgroup bib nil
  "Code related to the `bib' bibliography processor."
  :tag "Bibliography"
  :group 'external)

(defgroup processes nil
  "Process, subshell, compilation, and job control support."
  :group 'external
  :group 'development)

(defgroup programming nil
  "Support for programming in other languages."
  :group 'emacs)

(defgroup languages nil
  "Specialized modes for editing programming languages."
  :group 'programming)

(defgroup lisp nil
  "Lisp support, including Emacs Lisp."
  :group 'languages
  :group 'development)

(defgroup c nil
  "Support for the C language and related languages."
  :group 'languages)

(defgroup tools nil
  "Programming tools."
  :group 'programming)

(defgroup oop nil
  "Support for object-oriented programming."
  :group 'programming)

(defgroup applications nil
  "Applications written in Emacs."
  :group 'emacs)

(defgroup calendar nil
  "Calendar and time management support."
  :group 'applications)

(defgroup mail nil
  "Modes for electronic-mail handling."
  :group 'applications)

(defgroup news nil
  "Support for netnews reading and posting."
  :group 'applications)

(defgroup games nil
  "Games, jokes and amusements."
  :group 'applications)

(defgroup development nil
  "Support for further development of Emacs."
  :group 'emacs)

(defgroup docs nil
  "Support for Emacs documentation."
  :group 'development)

(defgroup extensions nil
  "Emacs Lisp language extensions."
  :group 'development)

(defgroup internal nil
  "Code for Emacs internals, build process, defaults."
  :group 'development)

(defgroup maint nil
  "Maintenance aids for the Emacs development group."
  :tag "Maintenance"
  :group 'development)

(defgroup environment nil
  "Fitting Emacs with its environment."
  :group 'emacs)

(defgroup comm nil
  "Communications, networking, remote access to files."
  :tag "Communication"
  :group 'environment)

(defgroup hardware nil
  "Support for interfacing with exotic hardware."
  :group 'environment)

(defgroup terminals nil
  "Support for terminal types."
  :group 'environment)

(defgroup unix nil
  "Front-ends/assistants for, or emulators of, UNIX features."
  :group 'environment)

(defgroup vms nil
  "Support code for vms."
  :group 'environment)

(defgroup i18n nil
  "Internationalization and alternate character-set support."
  :group 'environment
  :group 'editing)

(defgroup frames nil
  "Support for Emacs frames and window systems."
  :group 'environment)

(defgroup data nil
  "Support editing files of data."
  :group 'emacs)

(defgroup wp nil
  "Word processing."
  :group 'emacs)

(defgroup tex nil
  "Code related to the TeX formatter."
  :group 'wp)

(defgroup faces nil
  "Support for multiple fonts."
  :group 'emacs)

(defgroup hypermedia nil
  "Support for links between text or other media types."
  :group 'emacs)

(defgroup help nil
  "Support for on-line help systems."
  :group 'emacs)

(defgroup local nil
  "Code local to your site."
  :group 'emacs)

(defgroup customize '((widgets custom-group))
  "Customization of the Customization support."
  :link '(custom-manual "(custom)Top")
  :link '(url-link :tag "Development Page" 
		   "http://www.dina.kvl.dk/~abraham/custom/")
  :prefix "custom-"
  :group 'help
  :group 'faces)

;;; Utilities.

(defun custom-quote (sexp)
  "Quote SEXP iff it is not self quoting."
  (if (or (memq sexp '(t nil))
	  (and (symbolp sexp)
	       (eq (aref (symbol-name sexp) 0) ?:))
	  (and (listp sexp)
	       (memq (car sexp) '(lambda)))
	  (stringp sexp)
	  (numberp sexp)
	  (and (fboundp 'characterp)
	       (characterp sexp)))
      sexp
    (list 'quote sexp)))

(defun custom-split-regexp-maybe (regexp)
  "If REGEXP is a string, split it to a list at `\\|'.
You can get the original back with from the result with: 
  (mapconcat 'identity result \"\\|\")

IF REGEXP is not a string, return it unchanged."
  (if (stringp regexp)
      (let ((start 0)
	    all)
	(while (string-match "\\\\|" regexp start)
	  (setq all (cons (substring regexp start (match-beginning 0)) all)
		start (match-end 0)))
	(nreverse (cons (substring regexp start) all)))
    regexp))

(defvar custom-prefix-list nil
  "List of prefixes that should be ignored by `custom-unlispify'")

(defcustom custom-unlispify-menu-entries t
  "Display menu entries as words instead of symbols if non nil."
  :group 'customize
  :type 'boolean)

(defun custom-unlispify-menu-entry (symbol &optional no-suffix)
  "Convert symbol into a menu entry."
  (cond ((not custom-unlispify-menu-entries)
	 (symbol-name symbol))
	((get symbol 'custom-tag)
	 (if no-suffix
	     (get symbol 'custom-tag)
	   (concat (get symbol 'custom-tag) "...")))
	(t
	 (save-excursion
	   (set-buffer (get-buffer-create " *Custom-Work*"))
	   (erase-buffer)
	   (princ symbol (current-buffer))
	   (goto-char (point-min))
	   (let ((prefixes custom-prefix-list)
		 prefix)
	     (while prefixes
	       (setq prefix (car prefixes))
	       (if (search-forward prefix (+ (point) (length prefix)) t)
		   (progn 
		     (setq prefixes nil)
		     (delete-region (point-min) (point)))
		 (setq prefixes (cdr prefixes)))))
	   (subst-char-in-region (point-min) (point-max) ?- ?\  t)
	   (capitalize-region (point-min) (point-max))
	   (unless no-suffix 
	     (goto-char (point-max))
	     (insert "..."))
	   (buffer-string)))))

(defcustom custom-unlispify-tag-names t
  "Display tag names as words instead of symbols if non nil."
  :group 'customize
  :type 'boolean)

(defun custom-unlispify-tag-name (symbol)
  "Convert symbol into a menu entry."
  (let ((custom-unlispify-menu-entries custom-unlispify-tag-names))
    (custom-unlispify-menu-entry symbol t)))

(defun custom-prefix-add (symbol prefixes)
  ;; Addd SYMBOL to list of ignored PREFIXES.
  (cons (or (get symbol 'custom-prefix)
	    (concat (symbol-name symbol) "-"))
	prefixes))

;;; The Custom Mode.

(defvar custom-options nil
  "Customization widgets in the current buffer.")

(defvar custom-mode-map nil
  "Keymap for `custom-mode'.")
  
(unless custom-mode-map
  (setq custom-mode-map (make-sparse-keymap))
  (set-keymap-parent custom-mode-map widget-keymap)
  (define-key custom-mode-map "q" 'bury-buffer))

(easy-menu-define custom-mode-menu 
    custom-mode-map
  "Menu used in customization buffers."
    '("Custom"
      ["Set" custom-set t]
      ["Save" custom-save t]
      ["Reset to Current" custom-reset-current t]
      ["Reset to Saved" custom-reset-saved t]
      ["Reset to Factory Settings" custom-reset-factory t]
      ["Info" (Info-goto-node "(custom)The Customization Buffer") t]))

(defcustom custom-mode-hook nil
  "Hook called when entering custom-mode."
  :type 'hook
  :group 'customize)

(defun custom-mode ()
  "Major mode for editing customization buffers.

The following commands are available:

\\[widget-forward]		Move to next button or editable field.
\\[widget-backward]		Move to previous button or editable field.
\\[widget-button-click]		Activate button under the mouse pointer.
\\[widget-button-press]		Activate button under point.
\\[custom-set]			Set all modifications.
\\[custom-save]		Make all modifications default.
\\[custom-reset-current]        Reset all modified options. 
\\[custom-reset-saved]		Reset all modified or set options.
\\[custom-reset-factory]	Reset all options.

Entry to this mode calls the value of `custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (easy-menu-add custom-mode-menu)
  (make-local-variable 'custom-options)
  (run-hooks 'custom-mode-hook))

;;; Custom Mode Commands.

(defun custom-set ()
  "Set changes in all modified options."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-set)))
	    children)))

(defun custom-save ()
  "Set all modified group members and save them."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (memq (widget-get child :custom-state) '(modified set))
		(widget-apply child :custom-save)))
	    children))
  (custom-save-all))

(defvar custom-reset-menu 
  '(("Current" . custom-reset-current)
    ("Saved" . custom-reset-saved)
    ("Factory Settings" . custom-reset-factory))
  "Alist of actions for the `Reset' button.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-reset (event)
  "Select item from reset menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose "Reset to"
				custom-reset-menu
				event)))
    (if answer
	(funcall answer))))

(defun custom-reset-current ()
  "Reset all modified group members to their current value."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset-current)))
	    children)))

(defun custom-reset-saved ()
  "Reset all modified or set group members to their saved value."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset-current)))
	    children)))

(defun custom-reset-factory ()
  "Reset all modified, set, or saved group members to their factory settings."
  (interactive)
  (let ((children custom-options))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset-current)))
	    children)))

;;; The Customize Commands

;;;###autoload
(defun customize (symbol)
  "Customize SYMBOL, which must be a customization group."
  (interactive (list (completing-read "Customize group: (default emacs) "
				      obarray 
				      (lambda (symbol)
					(get symbol 'custom-group))
				      t)))

  (when (stringp symbol)
    (if (string-equal "" symbol)
	(setq symbol 'emacs)
      (setq symbol (intern symbol))))
  (custom-buffer-create (list (list symbol 'custom-group))))

;;;###autoload
(defun customize-variable (symbol)
  "Customize SYMBOL, which must be a variable."
  (interactive
   ;; Code stolen from `help.el'.
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read 
		(if v
		    (format "Customize variable (default %s): " v)
		  "Customize variable: ")
		obarray 'boundp t))
     (list (if (equal val "")
	       v (intern val)))))
  (custom-buffer-create (list (list symbol 'custom-variable))))

;;;###autoload
(defun customize-face (&optional symbol)
  "Customize SYMBOL, which should be a face name or nil.
If SYMBOL is nil, customize all faces."
  (interactive (list (completing-read "Customize face: (default all) " 
				      obarray 'custom-facep)))
  (if (or (null symbol) (and (stringp symbol) (zerop (length symbol))))
      (let ((found nil))
	(message "Looking for faces...")
	(mapcar (lambda (symbol)
		  (setq found (cons (list symbol 'custom-face) found)))
		(face-list))
	(custom-buffer-create found))
    (if (stringp symbol)
	(setq symbol (intern symbol)))
    (unless (symbolp symbol)
      (error "Should be a symbol %S" symbol))
    (custom-buffer-create (list (list symbol 'custom-face)))))

;;;###autoload
(defun customize-customized ()
  "Customize all already customized user options."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (get symbol 'saved-face)
		     (custom-facep symbol)
		     (setq found (cons (list symbol 'custom-face) found)))
		(and (get symbol 'saved-value)
		     (boundp symbol)
		     (setq found
			   (cons (list symbol 'custom-variable) found)))))
    (if found 
	(custom-buffer-create found)
      (error "No customized user options"))))

;;;###autoload
(defun customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL (e.g., started with a prefix key), include options which are not
user-settable."
  (interactive "sCustomize regexp: \nP")
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(when (string-match regexp (symbol-name symbol))
		  (when (get symbol 'custom-group)
		    (setq found (cons (list symbol 'custom-group) found)))
		  (when (custom-facep symbol)
		    (setq found (cons (list symbol 'custom-face) found)))
		  (when (and (boundp symbol)
			     (or (get symbol 'saved-value)
				 (get symbol 'factory-value)
				 (if all
				     (get symbol 'variable-documentation)
				   (user-variable-p symbol))))
		    (setq found
			  (cons (list symbol 'custom-variable) found))))))
    (if found 
	(custom-buffer-create found)
      (error "No matches"))))

;;;###autoload
(defun custom-buffer-create (options)
  "Create a buffer containing OPTIONS.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (message "Creating customization buffer...")
  (kill-buffer (get-buffer-create "*Customization*"))
  (switch-to-buffer (get-buffer-create "*Customization*"))
  (custom-mode)
  (widget-insert "This is a customization buffer.
Push RET or click mouse-2 on the word ")
  ;; (put-text-property 1 2 'start-open nil)
  (widget-create 'info-link 
		 :tag "help"
		 :help-echo "Read the online help."
		 "(custom)The Customization Buffer")
  (widget-insert " for more information.\n\n")
  (setq custom-options 
	(if (= (length options) 1)
	    (mapcar (lambda (entry)
		      (widget-create (nth 1 entry)
				     :custom-state 'unknown
				     :tag (custom-unlispify-tag-name
					   (nth 0 entry))
				     :value (nth 0 entry)))
		    options)
	  (let ((count 0)
		(length (length options)))
	    (mapcar (lambda (entry)
			(prog2
			    (message "Creating customization items %2d%%..."
				     (/ (* 100.0 count) length))
			    (widget-create (nth 1 entry)
					 :tag (custom-unlispify-tag-name
					       (nth 0 entry))
					 :value (nth 0 entry))
			  (setq count (1+ count))
			  (unless (eq (preceding-char) ?\n)
			    (widget-insert "\n"))
			  (widget-insert "\n")))
		      options))))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  (widget-insert "\n")
  (message "Creating customization magic...")
  (mapcar 'custom-magic-reset custom-options)
  (message "Creating customization buttons...")
  (widget-create 'push-button
		 :tag "Set"
		 :help-echo "Set all modifications for this session."
		 :action (lambda (widget &optional event)
			   (custom-set)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Save"
		 :help-echo "\
Make the modifications default for future sessions."
		 :action (lambda (widget &optional event)
			   (custom-save)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Reset"
		 :help-echo "Undo all modifications."
		 :action (lambda (widget &optional event)
			   (custom-reset event)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Done"
		 :help-echo "Bury the buffer."
		 :action (lambda (widget &optional event)
			   (bury-buffer)
			   ;; Steal button release event.
			   (if (and (fboundp 'button-press-event-p)
				    (fboundp 'next-command-event))
			       ;; XEmacs
			       (and event
				    (button-press-event-p event)
				    (next-command-event))
			     ;; Emacs
			     (when (memq 'down (event-modifiers event))
			       (read-event)))))
  (widget-insert "\n")
  (message "Creating customization setup...")
  (widget-setup)
  (goto-char (point-min))
  (message "Creating customization buffer...done"))

;;; Modification of Basic Widgets.
;;
;; We add extra properties to the basic widgets needed here.  This is
;; fine, as long as we are careful to stay within out own namespace.
;;
;; We want simple widgets to be displayed by default, but complex
;; widgets to be hidden.

(widget-put (get 'item 'widget-type) :custom-show t)
(widget-put (get 'editable-field 'widget-type)
	    :custom-show (lambda (widget value)
			   (let ((pp (pp-to-string value)))
			     (cond ((string-match "\n" pp)
				    nil)
				   ((> (length pp) 40)
				    nil)
				   (t t)))))
(widget-put (get 'menu-choice 'widget-type) :custom-show t)

;;; The `custom-manual' Widget.

(define-widget 'custom-manual 'info-link
  "Link to the manual entry for this customization option."
  :help-echo "Read the manual entry for this option."
  :tag "Manual")

;;; The `custom-magic' Widget.

(defface custom-invalid-face '((((class color))
				(:foreground "yellow" :background "red"))
			       (t
				(:bold t :italic t :underline t)))
  "Face used when the customize item is invalid.")

(defface custom-rogue-face '((((class color))
			      (:foreground "pink" :background "black"))
			     (t
			      (:underline t)))
  "Face used when the customize item is not defined for customization.")

(defface custom-modified-face '((((class color)) 
				 (:foreground "white" :background "blue"))
				(t
				 (:italic t :bold)))
  "Face used when the customize item has been modified.")

(defface custom-set-face '((((class color)) 
				(:foreground "blue" :background "white"))
			       (t
				(:italic t)))
  "Face used when the customize item has been set.")

(defface custom-changed-face '((((class color)) 
				(:foreground "white" :background "blue"))
			       (t
				(:italic t)))
  "Face used when the customize item has been changed.")

(defface custom-saved-face '((t (:underline t)))
  "Face used when the customize item has been saved.")

(defcustom custom-magic-alist '((nil "#" underline "\
uninitialized, you should not see this.")
				(unknown "?" italic "\
unknown, you should not see this.")
				(hidden "-" default "\
hidden, press the state button to show.")
				(invalid "x" custom-invalid-face "\
the value displayed for this item is invalid and cannot be set.")
				(modified "*" custom-modified-face "\
you have edited the item, and can now set it.")
				(set "+" custom-set-face "\
you have set this item, but not saved it.")
				(changed ":" custom-changed-face "\
this item has been changed outside customize.")
				(saved "!" custom-saved-face "\
this item has been saved.")
				(rogue "@" custom-rogue-face "\
this item is not prepared for customization.")
				(factory " " nil "\
this item is unchanged from its factory setting."))
  "Alist of customize option states.
Each entry is of the form (STATE MAGIC FACE DESCRIPTION), where 

STATE is one of the following symbols:

`nil'
   For internal use, should never occur.
`unknown'
   For internal use, should never occur.
`hidden'
   This item is not being displayed. 
`invalid'
   This item is modified, but has an invalid form.
`modified'
   This item is modified, and has a valid form.
`set'
   This item has been set but not saved.
`changed'
   The current value of this item has been changed temporarily.
`saved'
   This item is marked for saving.
`rogue'
   This item has no customization information.
`factory'
   This item is unchanged from the factory default.

MAGIC is a string used to present that state.

FACE is a face used to present the state.

DESCRIPTION is a string describing the state.

The list should be sorted most significant first."
  :type '(list (checklist :inline t
			  (group (const nil)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const unknown)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const hidden)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const invalid)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const modified)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const set)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const changed)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const saved)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const rogue)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description"))
			  (group (const factory)
				 (string :tag "Magic")
				 face 
				 (string :tag "Description")))
	       (editable-list :inline t
			      (group symbol
				     (string :tag "Magic")
				     face
				     (string :tag "Description"))))
  :group 'customize)

(defcustom custom-magic-show 'long
  "Show long description of the state of each customization option."
  :type '(choice (const :tag "no" nil)
		 (const short)
		 (const long))
  :group 'customize)

(defcustom custom-magic-show-button t
  "Show a magic button indicating the state of each customization option."
  :type 'boolean
  :group 'customize)

(define-widget 'custom-magic 'default
  "Show and manipulate state for a customization option."
  :format "%v"
  :action 'widget-choice-item-action
  :value-get 'ignore
  :value-create 'custom-magic-value-create
  :value-delete 'widget-children-value-delete)

(defun custom-magic-value-create (widget)
  ;; Create compact status report for WIDGET.
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state))
	 (entry (assq state custom-magic-alist))
	 (magic (nth 1 entry))
	 (face (nth 2 entry))
	 (text (nth 3 entry))
	 (lisp (eq (widget-get parent :custom-form) 'lisp))
	 children)
    (when custom-magic-show
      (push (widget-create-child-and-convert widget 'choice-item 
					     :help-echo "\
Change the state of this item."
					     :format "%[%t%]"
					     :tag "State")
	    children)
      (insert ": ")
      (if (eq custom-magic-show 'long)
	  (insert text)
	(insert (symbol-name state)))
      (when lisp 
	(insert " (lisp)"))
      (insert "\n"))
    (when custom-magic-show-button
      (when custom-magic-show
	(let ((indent (widget-get parent :indent)))
	  (when indent
	    (insert-char ?  indent))))
      (push (widget-create-child-and-convert widget 'choice-item 
					     :button-face face
					     :help-echo "Change the state."
					     :format "%[%t%]"
					     :tag (if lisp 
						      (concat "(" magic ")")
						    (concat "[" magic "]")))
	    children)
      (insert " "))
    (widget-put widget :children children)))

(defun custom-magic-reset (widget)
  "Redraw the :custom-magic property of WIDGET."
  (let ((magic (widget-get widget :custom-magic)))
    (widget-value-set magic (widget-value magic))))

;;; The `custom-level' Widget.

(define-widget 'custom-level 'item
  "The custom level buttons."
  :format "%[%t%]"
  :help-echo "Expand or collapse this item."
  :action 'custom-level-action)

(defun custom-level-action (widget &optional event)
  "Toggle visibility for parent to WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state)))
    (cond ((memq state '(invalid modified))
	   (error "There are unset changes"))
	  ((eq state 'hidden)
	   (widget-put parent :custom-state 'unknown))
	  (t
	   (widget-put parent :custom-state 'hidden)))
    (custom-redraw parent)))

;;; The `custom' Widget.

(define-widget 'custom 'default
  "Customize a user option."
  :convert-widget 'custom-convert-widget
  :format "%l%[%t%]: %v%m%h%a"
  :format-handler 'custom-format-handler
  :notify 'custom-notify
  :custom-level 1
  :custom-state 'hidden
  :documentation-property 'widget-subclass-responsibility
  :value-create 'widget-subclass-responsibility
  :value-delete 'widget-children-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-editable-list-validate
  :match (lambda (widget value) (symbolp value)))

(defun custom-convert-widget (widget)
  ;; Initialize :value and :tag from :args in WIDGET.
  (let ((args (widget-get widget :args)))
    (when args 
      (widget-put widget :value (widget-apply widget
					      :value-to-internal (car args)))
      (widget-put widget :tag (custom-unlispify-tag-name (car args)))
      (widget-put widget :args nil)))
  widget)

(defun custom-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let* ((buttons (widget-get widget :buttons))
	 (state (widget-get widget :custom-state))
	 (level (widget-get widget :custom-level)))
    (cond ((eq escape ?l)
	   (when level 
	     (push (widget-create-child-and-convert
		    widget 'custom-level (make-string level ?*))
		   buttons)
	     (widget-insert " ")
	     (widget-put widget :buttons buttons)))
	  ((eq escape ?L)
	   (when (eq state 'hidden)
	     (widget-insert " ...")))
	  ((eq escape ?m)
	   (and (eq (preceding-char) ?\n)
		(widget-get widget :indent)
		(insert-char ?  (widget-get widget :indent)))
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons)
	     (widget-put widget :buttons buttons)))
	  ((eq escape ?a)
	   (let* ((symbol (widget-get widget :value))
		  (links (get symbol 'custom-links))
		  (many (> (length links) 2)))
	     (when links
	       (and (eq (preceding-char) ?\n)
		    (widget-get widget :indent)
		    (insert-char ?  (widget-get widget :indent)))
	       (insert "See also ")
	       (while links
		 (push (widget-create-child-and-convert widget (car links))
		       buttons)
		 (setq links (cdr links))
		 (cond ((null links)
			(insert ".\n"))
		       ((null (cdr links))
			(if many
			    (insert ", and ")
			  (insert " and ")))
		       (t 
			(insert ", "))))
	       (widget-put widget :buttons buttons))))
	  (t 
	   (widget-default-format-handler widget escape)))))

(defun custom-notify (widget &rest args)
  "Keep track of changes."
  (unless (memq (widget-get widget :custom-state) '(nil unknown hidden))
    (widget-put widget :custom-state 'modified))
  (let ((buffer-undo-list t))
    (custom-magic-reset widget))
  (apply 'widget-default-notify widget args))

(defun custom-redraw (widget)
  "Redraw WIDGET with current settings."
  (let ((pos (point))
	(from (marker-position (widget-get widget :from)))
	(to (marker-position (widget-get widget :to))))
    (save-excursion
      (widget-value-set widget (widget-value widget))
      (custom-redraw-magic widget))
    (when (and (>= pos from) (<= pos to))
      (goto-char pos))))

(defun custom-redraw-magic (widget)
  "Redraw WIDGET state with current settings."
  (while widget 
    (let ((magic (widget-get widget :custom-magic)))
      (unless magic 
	(debug))
      (widget-value-set magic (widget-value magic))
      (when (setq widget (widget-get widget :group))
	(custom-group-state-update widget))))
  (widget-setup))

(defun custom-show (widget value)
  "Non-nil if WIDGET should be shown with VALUE by default."
  (let ((show (widget-get widget :custom-show)))
    (cond ((null show)
	   nil)
	  ((eq t show)
	   t)
	  (t
	   (funcall show widget value)))))

(defun custom-load-symbol (symbol)
  "Load all dependencies for SYMBOL."
  (let ((loads (get symbol 'custom-loads))
	load)
    (while loads
      (setq load (car loads)
	    loads (cdr loads))
      (cond ((symbolp load)
	     (condition-case nil
		 (require load)
	       (error nil)))
	    ((assoc load load-history))
	    (t
	     (condition-case nil
		 (load-library load)
	       (error nil)))))))

(defun custom-load-widget (widget)
  "Load all dependencies for WIDGET."
  (custom-load-symbol (widget-value widget)))

;;; The `custom-variable' Widget.

(defface custom-variable-sample-face '((t (:underline t)))
  "Face used for unpushable variable tags."
  :group 'customize)

(defface custom-variable-button-face '((t (:underline t :bold t)))
  "Face used for pushable variable tags."
  :group 'customize)

(define-widget 'custom-variable 'custom
  "Customize variable."
  :format "%l%v%m%h%a"
  :help-echo "Set or reset this variable."
  :documentation-property 'variable-documentation
  :custom-state nil
  :custom-menu 'custom-variable-menu-create
  :custom-form 'edit
  :value-create 'custom-variable-value-create
  :action 'custom-variable-action
  :custom-set 'custom-variable-set
  :custom-save 'custom-variable-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-variable-reset-saved
  :custom-reset-factory 'custom-variable-reset-factory)

(defun custom-variable-value-create (widget)
  "Here is where you edit the variables value."
  (custom-load-widget widget)
  (let* ((buttons (widget-get widget :buttons))
	 (children (widget-get widget :children))
	 (form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (symbol (widget-get widget :value))
	 (options (get symbol 'custom-options))
	 (child-type (or (get symbol 'custom-type) 'sexp))
	 (tag (widget-get widget :tag))
	 (type (let ((tmp (if (listp child-type)
			      (copy-list child-type)
			    (list child-type))))
		 (when options
		   (widget-put tmp :options options))
		 tmp))
	 (conv (widget-convert type))
	 (value (if (default-boundp symbol)
		    (default-value symbol)
		  (widget-get conv :value))))
    ;; If the widget is new, the child determine whether it is hidden.
    (cond (state)
	  ((custom-show type value)
	   (setq state 'unknown))
	  (t
	   (setq state 'hidden)))
    ;; If we don't know the state, see if we need to edit it in lisp form.
    (when (eq state 'unknown)
      (unless (widget-apply conv :match value)
	;; (widget-apply (widget-convert type) :match value)
	(setq form 'lisp)))
    ;; Now we can create the child widget.
    (cond ((eq state 'hidden)
	   ;; Indicate hidden value.
	   (push (widget-create-child-and-convert 
		  widget 'item
		  :format "%{%t%}: ..."
		  :sample-face 'custom-variable-sample-face
		  :tag tag
		  :parent widget)
		 children))
	  ((eq form 'lisp)
	   ;; In lisp mode edit the saved value when possible.
	   (let* ((value (cond ((get symbol 'saved-value)
				(car (get symbol 'saved-value)))
			       ((get symbol 'factory-value)
				(car (get symbol 'factory-value)))
			       ((default-boundp symbol)
				(custom-quote (default-value symbol)))
			       (t
				(custom-quote (widget-get conv :value))))))
	     (push (widget-create-child-and-convert 
		    widget 'sexp 
		    :button-face 'custom-variable-button-face
		    :tag (symbol-name symbol)
		    :parent widget
		    :value value)
		   children)))
	  (t
	   ;; Edit mode.
	   (push (widget-create-child-and-convert
		  widget type 
		  :tag tag
		  :button-face 'custom-variable-button-face
		  :sample-face 'custom-variable-sample-face
		  :value value)
		 children)))
    ;; Now update the state.
    (unless (eq (preceding-char) ?\n)
      (widget-insert "\n"))
    (if (eq state 'hidden)
	(widget-put widget :custom-state state)
      (custom-variable-state-set widget))
    (widget-put widget :custom-form form)	     
    (widget-put widget :buttons buttons)
    (widget-put widget :children children)))

(defun custom-variable-state-set (widget)
  "Set the state of WIDGET."
  (let* ((symbol (widget-value widget))
	 (value (if (default-boundp symbol)
		    (default-value symbol)
		  (widget-get widget :value)))
	 tmp
	 (state (cond ((setq tmp (get symbol 'customized-value))
		       (if (condition-case nil
			       (equal value (eval (car tmp)))
			     (error nil))
			   'set
			 'changed))
		      ((setq tmp (get symbol 'saved-value))
		       (if (condition-case nil
			       (equal value (eval (car tmp)))
			     (error nil))
			   'saved
			 'changed))
		      ((setq tmp (get symbol 'factory-value))
		       (if (condition-case nil
			       (equal value (eval (car tmp)))
			     (error nil))
			   'factory
			 'changed))
		      (t 'rogue))))
    (widget-put widget :custom-state state)))

(defvar custom-variable-menu 
  '(("Edit" . custom-variable-edit)
    ("Edit Lisp" . custom-variable-edit-lisp)
    ("Set" . custom-variable-set)
    ("Save" . custom-variable-save)
    ("Reset to Current" . custom-redraw)
    ("Reset to Saved" . custom-variable-reset-saved)
    ("Reset to Factory Settings" . custom-variable-reset-factory))
  "Alist of actions for the `custom-variable' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-variable-action (widget &optional event)
  "Show the menu for `custom-variable' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (progn 
	(widget-put widget :custom-state 'unknown)
	(custom-redraw widget))
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (custom-unlispify-tag-name
				   (widget-get widget :value))
				  custom-variable-menu
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-variable-edit (widget)
  "Edit value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'edit)
  (custom-redraw widget))

(defun custom-variable-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-variable-set (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let ((form (widget-get widget :custom-form))
	(state (widget-get widget :custom-state))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget))
	val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable."))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "%s" (widget-get val :error)))
	  ((eq form 'lisp)
	   (set symbol (eval (setq val (widget-value child))))
	   (put symbol 'customized-value (list val)))
	  (t
	   (set symbol (setq val (widget-value child)))
	   (put symbol 'customized-value (list (custom-quote val)))))
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-save (widget)
  "Set the default value for the variable being edited by WIDGET."
  (let ((form (widget-get widget :custom-form))
	(state (widget-get widget :custom-state))
	(child (car (widget-get widget :children)))
	(symbol (widget-value widget))
	val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable."))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "%s" (widget-get val :error)))
	  ((eq form 'lisp)
	   (put symbol 'saved-value (list (widget-value child)))
	   (set symbol (eval (widget-value child))))
	  (t
	   (put symbol
		'saved-value (list (custom-quote (widget-value
						  child))))
	   (set symbol (widget-value child))))
    (put symbol 'customized-value nil)
    (custom-save-all)
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-reset-saved (widget)
  "Restore the saved value for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (if (get symbol 'saved-value)
	(condition-case nil
	    (set symbol (eval (car (get symbol 'saved-value))))
	  (error nil))
      (error "No saved value for %s" symbol))
    (put symbol 'customized-value nil)
    (widget-put widget :custom-state 'unknown)
    (custom-redraw widget)))

(defun custom-variable-reset-factory (widget)
  "Restore the factory setting for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (if (get symbol 'factory-value)
	(set symbol (eval (car (get symbol 'factory-value))))
      (error "No factory default for %S" symbol))
    (put symbol 'customized-value nil)
    (when (get symbol 'saved-value)
      (put symbol 'saved-value nil)
      (custom-save-all))
    (widget-put widget :custom-state 'unknown)
    (custom-redraw widget)))

;;; The `custom-face-edit' Widget.

(define-widget 'custom-face-edit 'checklist
  "Edit face attributes."
  :format "%t: %v"
  :tag "Attributes"
  :extra-offset 12
  :button-args '(:help-echo "Control whether this attribute have any effect.")
  :args (mapcar (lambda (att)
		  (list 'group 
			:inline t
			:sibling-args (widget-get (nth 1 att) :sibling-args)
			(list 'const :format "" :value (nth 0 att)) 
			(nth 1 att)))
		custom-face-attributes))

;;; The `custom-display' Widget.

(define-widget 'custom-display 'menu-choice
  "Select a display type."
  :tag "Display"
  :value t
  :help-echo "Specify frames where the face attributes should be used."
  :args '((const :tag "all" t)
	  (checklist
	   :offset 0
	   :extra-offset 9
	   :args ((group :sibling-args (:help-echo "\
Only match the specified window systems.")
			 (const :format "Type: "
				type)
			 (checklist :inline t
				    :offset 0
				    (const :format "X "
					   :sibling-args (:help-echo "\
The X11 Window System.")
					   x)
				    (const :format "PM "
					   :sibling-args (:help-echo "\
OS/2 Presentation Manager.")
					   pm)
				    (const :format "Win32 "
					   :sibling-args (:help-echo "\
Windows NT/95/97.")
					   win32)
				    (const :format "DOS "
					   :sibling-args (:help-echo "\
Plain MS-DOS.")
					   pc)
				    (const :format "TTY%n"
					   :sibling-args (:help-echo "\
Plain text terminals.")
					   tty)))
		  (group :sibling-args (:help-echo "\
Only match the frames with the specified color support.")
			 (const :format "Class: "
				class)
			 (checklist :inline t
				    :offset 0
				    (const :format "Color "
					   :sibling-args (:help-echo "\
Match color frames.")
					   color)
				    (const :format "Grayscale "
					   :sibling-args (:help-echo "\
Match grayscale frames.")
					   grayscale)
				    (const :format "Monochrome%n"
					   :sibling-args (:help-echo "\
Match frames with no color support.")
					   mono)))
		  (group :sibling-args (:help-echo "\
Only match frames with the specified intensity.")
			 (const :format "\
Background brightness: "
				background)
			 (checklist :inline t
				    :offset 0
				    (const :format "Light "
					   :sibling-args (:help-echo "\
Match frames with light backgrounds.")
					   light)
				    (const :format "Dark\n"
					   :sibling-args (:help-echo "\
Match frames with dark backgrounds.")
					   dark)))))))

;;; The `custom-face' Widget.

(defface custom-face-tag-face '((t (:underline t)))
  "Face used for face tags."
  :group 'customize)

(define-widget 'custom-face 'custom
  "Customize face."
  :format "%l%{%t%}: %s%m%h%a%v"
  :format-handler 'custom-face-format-handler
  :sample-face 'custom-face-tag-face
  :help-echo "Set or reset this face."
  :documentation-property '(lambda (face)
			     (face-doc-string face))
  :value-create 'custom-face-value-create
  :action 'custom-face-action
  :custom-form 'selected
  :custom-set 'custom-face-set
  :custom-save 'custom-face-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-face-reset-saved
  :custom-reset-factory 'custom-face-reset-factory
  :custom-menu 'custom-face-menu-create)

(defun custom-face-format-handler (widget escape)
  ;; We recognize extra escape sequences.
  (let (child
	(symbol (widget-get widget :value)))
    (cond ((eq escape ?s)
	   (and (string-match "XEmacs" emacs-version)
		;; XEmacs cannot display initialized faces.
		(not (custom-facep symbol))
		(copy-face 'custom-face-empty symbol))
	   (setq child (widget-create-child-and-convert 
			widget 'item
			:format "(%{%t%})\n"
			:sample-face symbol
			:tag "sample")))
	  (t 
	   (custom-format-handler widget escape)))
    (when child
      (widget-put widget
		  :buttons (cons child (widget-get widget :buttons))))))

(define-widget 'custom-face-all 'editable-list 
  "An editable list of display specifications and attributes."
  :entry-format "%i %d %v"
  :insert-button-args '(:help-echo "Insert new display specification here.")
  :append-button-args '(:help-echo "Append new display specification here.")
  :delete-button-args '(:help-echo "Delete this display specification.")
  :args '((group :format "%v" custom-display custom-face-edit)))

(defconst custom-face-all (widget-convert 'custom-face-all)
  "Converted version of the `custom-face-all' widget.")

(define-widget 'custom-display-unselected 'item
  "A display specification that doesn't match the selected display."
  :match 'custom-display-unselected-match)

(defun custom-display-unselected-match (widget value)
  "Non-nil if VALUE is an unselected display specification."
  (and (listp value)
       (eq (length value) 2)
       (not (custom-display-match-frame value (selected-frame)))))

(define-widget 'custom-face-selected 'group 
  "Edit the attributes of the selected display in a face specification."
  :args '((repeat :format ""
		  :inline t
		  (group custom-display-unselected sexp))
	  (group (sexp :format "") custom-face-edit)
	  (repeat :format ""
		  :inline t
		  sexp)))

(defconst custom-face-selected (widget-convert 'custom-face-selected)
  "Converted version of the `custom-face-selected' widget.")

(defun custom-face-value-create (widget)
  ;; Create a list of the display specifications.
  (unless (eq (preceding-char) ?\n)
    (insert "\n"))
  (when (not (eq (widget-get widget :custom-state) 'hidden))
    (message "Creating face editor...")
    (custom-load-widget widget)
    (let* ((symbol (widget-value widget))
	   (spec (or (get symbol 'saved-face)
		     (get symbol 'factory-face)
		     ;; Attempt to construct it.
		     (list (list t (custom-face-attributes-get 
				    symbol (selected-frame))))))
	   (form (widget-get widget :custom-form))
	   (indent (widget-get widget :indent))
	   (edit (widget-create-child-and-convert
		  widget
		  (cond ((and (eq form 'selected)
			      (widget-apply custom-face-selected :match spec))
			 (when indent (insert-char ?\  indent))
			 'custom-face-selected)
			((and (not (eq form 'lisp))
			      (widget-apply custom-face-all :match spec))
			 'custom-face-all)
			(t 
			 (when indent (insert-char ?\  indent))
			 'sexp))
		  :value spec)))
      (custom-face-state-set widget)
      (widget-put widget :children (list edit)))
    (message "Creating face editor...done")))

(defvar custom-face-menu 
  '(("Edit Selected" . custom-face-edit-selected)
    ("Edit All" . custom-face-edit-all)
    ("Edit Lisp" . custom-face-edit-lisp)
    ("Set" . custom-face-set)
    ("Save" . custom-face-save)
    ("Reset to Saved" . custom-face-reset-saved)
    ("Reset to Factory Setting" . custom-face-reset-factory))
  "Alist of actions for the `custom-face' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-face-edit-selected (widget)
  "Edit selected attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'selected)
  (custom-redraw widget))

(defun custom-face-edit-all (widget)
  "Edit all attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'all)
  (custom-redraw widget))

(defun custom-face-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-face-state-set (widget)
  "Set the state of WIDGET."
  (let ((symbol (widget-value widget)))
    (widget-put widget :custom-state (cond ((get symbol 'customized-face)
					    'set)
					   ((get symbol 'saved-face)
					    'saved)
					   ((get symbol 'factory-face)
					    'factory)
					   (t 
					    'rogue)))))

(defun custom-face-action (widget &optional event)
  "Show the menu for `custom-face' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (progn 
	(widget-put widget :custom-state 'unknown)
	(custom-redraw widget))
    (let* ((completion-ignore-case t)
	   (symbol (widget-get widget :value))
	   (answer (widget-choose (custom-unlispify-tag-name symbol)
				  custom-face-menu event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-face-set (widget)
  "Make the face attributes in WIDGET take effect."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (put symbol 'customized-face value)
    (when (fboundp 'copy-face)
      (copy-face 'custom-face-empty symbol))
    (custom-face-display-set symbol value)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-save (widget)
  "Make the face attributes in WIDGET default."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child)))
    (when (fboundp 'copy-face)
      (copy-face 'custom-face-empty symbol))
    (custom-face-display-set symbol value)
    (put symbol 'saved-face value)
    (put symbol 'customized-face nil)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-reset-saved (widget)
  "Restore WIDGET to the face's default attributes."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'saved-face)))
    (unless value
      (error "No saved value for this face"))
    (put symbol 'customized-face nil)
    (when (fboundp 'copy-face)
      (copy-face 'custom-face-empty symbol))
    (custom-face-display-set symbol value)
    (widget-value-set child value)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-reset-factory (widget)
  "Restore WIDGET to the face's factory settings."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'factory-face)))
    (unless value
      (error "No factory default for this face"))
    (put symbol 'customized-face nil)
    (when (get symbol 'saved-face)
      (put symbol 'saved-face nil)
      (custom-save-all))
    (when (fboundp 'copy-face)
      (copy-face 'custom-face-empty symbol))
    (custom-face-display-set symbol value)
    (widget-value-set child value)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

;;; The `face' Widget.

(define-widget 'face 'default
  "Select and customize a face."
  :convert-widget 'widget-item-convert-widget
  :format "%[%t%]: %v"
  :tag "Face"
  :value 'default
  :value-create 'widget-face-value-create
  :value-delete 'widget-face-value-delete
  :value-get 'widget-item-value-get
  :validate 'widget-editable-list-validate
  :action 'widget-face-action
  :match '(lambda (widget value) (symbolp value)))

(defun widget-face-value-create (widget)
  ;; Create a `custom-face' child.
  (let* ((symbol (widget-value widget))
	 (child (widget-create-child-and-convert
		 widget 'custom-face
		 :format "%t %s%m%h%v"
		 :custom-level nil
		 :value symbol)))
    (custom-magic-reset child)
    (setq custom-options (cons child custom-options))
    (widget-put widget :children (list child))))

(defun widget-face-value-delete (widget)
  ;; Remove the child from the options.
  (let ((child (car (widget-get widget :children))))
    (setq custom-options (delq child custom-options))
    (widget-children-value-delete widget)))

(defvar face-history nil
  "History of entered face names.")

(defun widget-face-action (widget &optional event)
  "Prompt for a face."
  (let ((answer (completing-read "Face: "
				 (mapcar (lambda (face)
					   (list (symbol-name face)))
					 (face-list))
				 nil nil nil				 
				 'face-history)))
    (unless (zerop (length answer))
      (widget-value-set widget (intern answer))
      (widget-apply widget :notify widget event)
      (widget-setup))))

;;; The `hook' Widget.

(define-widget 'hook 'list
  "A emacs lisp hook"
  :convert-widget 'custom-hook-convert-widget
  :tag "Hook")

(defun custom-hook-convert-widget (widget)
  ;; Handle `:custom-options'.
  (let* ((options (widget-get widget :options))
	 (other `(editable-list :inline t 
				:entry-format "%i %d%v"
				(function :format " %v")))
	 (args (if options
		   (list `(checklist :inline t
				     ,@(mapcar (lambda (entry)
						 `(function-item ,entry))
					       options))
			 other)
		 (list other))))
    (widget-put widget :args args)
    widget))

;;; The `custom-group' Widget.

(defcustom custom-group-tag-faces '(custom-group-tag-face-1)
  ;; In XEmacs, this ought to play games with font size.
  "Face used for group tags.
The first member is used for level 1 groups, the second for level 2,
and so forth.  The remaining group tags are shown with
`custom-group-tag-face'."
  :type '(repeat face)
  :group 'customize)

(defface custom-group-tag-face-1 '((((class color)
				     (background dark))
				    (:foreground "pink" :underline t))
				   (((class color)
				     (background light))
				    (:foreground "red" :underline t))
				   (t (:underline t)))
  "Face used for group tags.")

(defface custom-group-tag-face '((((class color)
				   (background dark))
				  (:foreground "light blue" :underline t))
				 (((class color)
				   (background light))
				  (:foreground "blue" :underline t))
				 (t (:underline t)))
  "Face used for low level group tags."
  :group 'customize)

(define-widget 'custom-group 'custom
  "Customize group."
  :format "%l%{%t%}:%L\n%m%h%a%v"
  :sample-face-get 'custom-group-sample-face-get
  :documentation-property 'group-documentation
  :help-echo "Set or reset all members of this group."
  :value-create 'custom-group-value-create
  :action 'custom-group-action
  :custom-set 'custom-group-set
  :custom-save 'custom-group-save
  :custom-reset-current 'custom-group-reset-current
  :custom-reset-saved 'custom-group-reset-saved
  :custom-reset-factory 'custom-group-reset-factory
  :custom-menu 'custom-group-menu-create)

(defun custom-group-sample-face-get (widget)
  ;; Use :sample-face.
  (or (nth (1- (widget-get widget :custom-level)) custom-group-tag-faces)
      'custom-group-tag-face))

(defun custom-group-value-create (widget)
  (let ((state (widget-get widget :custom-state)))
    (unless (eq state 'hidden)
      (message "Creating group...")
      (custom-load-widget widget)
      (let* ((level (widget-get widget :custom-level))
	     (symbol (widget-value widget))
	     (members (get symbol 'custom-group))
	     (prefixes (widget-get widget :custom-prefixes))
	     (custom-prefix-list (custom-prefix-add symbol prefixes))
	     (length (length members))
	     (count 0)
	     (children (mapcar (lambda (entry)
				 (widget-insert "\n")
				 (message "Creating group members... %2d%%"
					  (/ (* 100.0 count) length))
				 (setq count (1+ count))
				 (prog1
				     (widget-create-child-and-convert
				      widget (nth 1 entry)
				      :group widget
				      :tag (custom-unlispify-tag-name
					    (nth 0 entry))
				      :custom-prefixes custom-prefix-list
				      :custom-level (1+ level)
				      :value (nth 0 entry))
				   (unless (eq (preceding-char) ?\n)
				     (widget-insert "\n"))))
			       members)))
	(message "Creating group magic...")
	(mapcar 'custom-magic-reset children)
	(message "Creating group state...")
	(widget-put widget :children children)
	(custom-group-state-update widget)
	(message "Creating group... done")))))

(defvar custom-group-menu 
  '(("Set" . custom-group-set)
    ("Save" . custom-group-save)
    ("Reset to Current" . custom-group-reset-current)
    ("Reset to Saved" . custom-group-reset-saved)
    ("Reset to Factory" . custom-group-reset-factory))
  "Alist of actions for the `custom-group' widget.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-group-action (widget &optional event)
  "Show the menu for `custom-group' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (progn 
	(widget-put widget :custom-state 'unknown)
	(custom-redraw widget))
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (custom-unlispify-tag-name
				   (widget-get widget :value))
				  custom-group-menu
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-group-set (widget)
  "Set changes in all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-set)))
	    children )))

(defun custom-group-save (widget)
  "Save all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (memq (widget-get child :custom-state) '(modified set))
		(widget-apply child :custom-save)))
	    children )))

(defun custom-group-reset-current (widget)
  "Reset all modified group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (eq (widget-get child :custom-state) 'modified)
		(widget-apply child :custom-reset-current)))
	    children )))

(defun custom-group-reset-saved (widget)
  "Reset all modified or set group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (memq (widget-get child :custom-state) '(modified set))
		(widget-apply child :custom-reset-saved)))
	    children )))

(defun custom-group-reset-factory (widget)
  "Reset all modified, set, or saved group members."
  (let ((children (widget-get widget :children)))
    (mapcar (lambda (child)
	      (when (memq (widget-get child :custom-state)
			  '(modified set saved))
		(widget-apply child :custom-reset-factory)))
	    children )))

(defun custom-group-state-update (widget)
  "Update magic."
  (unless (eq (widget-get widget :custom-state) 'hidden)
    (let* ((children (widget-get widget :children))
	   (states (mapcar (lambda (child)
			     (widget-get child :custom-state))
			   children))
	   (magics custom-magic-alist)
	   (found 'factory))
      (while magics
	(let ((magic (car (car magics))))
	  (if (and (not (eq magic 'hidden))
		   (memq magic states))
	      (setq found magic
		    magics nil)
	    (setq magics (cdr magics)))))
      (widget-put widget :custom-state found)))
  (custom-magic-reset widget))

;;; The `custom-save-all' Function.

(defcustom custom-file "~/.emacs"
  "File used for storing customization information.
If you change this from the default \"~/.emacs\" you need to
explicitly load that file for the settings to take effect."
  :type 'file
  :group 'customize)

(defun custom-save-delete (symbol)
  "Delete the call to SYMBOL form `custom-file'.
Leave point at the location of the call, or after the last expression."
  (set-buffer (find-file-noselect custom-file))
  (goto-char (point-min))
  (catch 'found
    (while t
      (let ((sexp (condition-case nil
		      (read (current-buffer))
		    (end-of-file (throw 'found nil)))))
	(when (and (listp sexp)
		   (eq (car sexp) symbol))
	  (delete-region (save-excursion
			   (backward-sexp)
			   (point))
			 (point))
	  (throw 'found nil))))))

(defun custom-save-variables ()
  "Save all customized variables in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (let ((standard-output (current-buffer)))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-variables")
      (mapatoms (lambda (symbol)
		  (let ((value (get symbol 'saved-value)))
		    (when value
		      (princ "\n '(")
		      (princ symbol)
		      (princ " ")
		      (prin1 (car value))
		      (if (or (get symbol 'factory-value)
			      (and (not (boundp symbol))
				   (not (get symbol 'force-value))))
			  (princ ")")
			(princ " t)"))))))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

(defun custom-save-faces ()
  "Save all customized faces in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-set-faces)
    (let ((standard-output (current-buffer)))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-faces")
      (mapatoms (lambda (symbol)
		  (let ((value (get symbol 'saved-face)))
		    (when value
		      (princ "\n '(")
		      (princ symbol)
		      (princ " ")
		      (prin1 value)
		      (if (or (get symbol 'factory-face)
			      (and (not (custom-facep symbol))
				   (not (get symbol 'force-face))))
			  (princ ")")
			(princ " t)"))))))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

;;;###autoload
(defun custom-save-all ()
  "Save all customizations in `custom-file'."
  (custom-save-variables)
  (custom-save-faces)
  (save-excursion
    (set-buffer (find-file-noselect custom-file))
    (save-buffer)))

;;; The Customize Menu.

(defcustom custom-menu-nesting 2
  "Maximum nesting in custom menus."
  :type 'integer
  :group 'customize)

(defun custom-face-menu-create (widget symbol)
  "Ignoring WIDGET, create a menu entry for customization face SYMBOL."
  (vector (custom-unlispify-menu-entry symbol)
	  `(custom-buffer-create '((,symbol custom-face)))
	  t))

(defun custom-variable-menu-create (widget symbol)
  "Ignoring WIDGET, create a menu entry for customization variable SYMBOL."
  (let ((type (get symbol 'custom-type)))
    (unless (listp type)
      (setq type (list type)))
    (if (and type (widget-get type :custom-menu))
	(widget-apply type :custom-menu symbol)
      (vector (custom-unlispify-menu-entry symbol)
	      `(custom-buffer-create '((,symbol custom-variable)))
	      t))))

(widget-put (get 'boolean 'widget-type)
	    :custom-menu (lambda (widget symbol)
			   (vector (custom-unlispify-menu-entry symbol)
				   `(custom-buffer-create
				     '((,symbol custom-variable)))
				   ':style 'toggle
				   ':selected symbol)))

(if (string-match "XEmacs" emacs-version)
    ;; XEmacs can create menus dynamically.
    (defun custom-group-menu-create (widget symbol)
      "Ignoring WIDGET, create a menu entry for customization group SYMBOL."
      `( ,(custom-unlispify-menu-entry symbol t)
	 :filter (lambda (&rest junk)
		   (cdr (custom-menu-create ',symbol)))))
  ;; But emacs can't.
  (defun custom-group-menu-create (widget symbol)
    "Ignoring WIDGET, create a menu entry for customization group SYMBOL."
    ;; Limit the nesting.
    (let ((custom-menu-nesting (1- custom-menu-nesting)))
      (custom-menu-create symbol))))

(defun custom-menu-create (symbol &optional name)
  "Create menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu. 
Otherwise make up a name from SYMBOL.
The menu is in a format applicable to `easy-menu-define'."
  (unless name
    (setq name (custom-unlispify-menu-entry symbol)))
  (let ((item (vector name
		      `(custom-buffer-create '((,symbol custom-group)))
		      t)))
    (if (and (>= custom-menu-nesting 0)
	     (< (length (get symbol 'custom-group)) widget-menu-max-size))
	(let ((custom-prefix-list (custom-prefix-add symbol
						     custom-prefix-list)))
	  (custom-load-symbol symbol)
	  `(,(custom-unlispify-menu-entry symbol t)
	    ,item
	    "--"
	    ,@(mapcar (lambda (entry)
			(widget-apply (if (listp (nth 1 entry))
					  (nth 1 entry)
					(list (nth 1 entry)))
				      :custom-menu (nth 0 entry)))
		      (get symbol 'custom-group))))
      item)))

;;;###autoload
(defun custom-menu-update (event)
  "Update customize menu."
  (interactive "e")
  (add-hook 'custom-define-hook 'custom-menu-reset)
  (let* ((emacs (widget-apply '(custom-group) :custom-menu 'emacs))
	 (menu `(,(car custom-help-menu)
		 ,emacs
		 ,@(cdr (cdr custom-help-menu)))))
    (let ((map (easy-menu-create-keymaps (car menu) (cdr menu))))
      (define-key global-map [menu-bar help-menu customize-menu]
	(cons (car menu) map)))))

;;; Dependencies.

;;;###autoload
(defun custom-make-dependencies ()
  "Batch function to extract custom dependencies from .el files.
Usage: emacs -batch *.el -f custom-make-dependencies > deps.el"
  (let ((buffers (buffer-list)))
    (while buffers
      (set-buffer (car buffers))
      (setq buffers (cdr buffers))
      (let ((file (buffer-file-name)))
	(when (and file (string-match "\\`\\(.*\\)\\.el\\'" file))
	  (goto-char (point-min))
	  (condition-case nil
	      (let ((name (file-name-nondirectory (match-string 1 file))))
		(while t
		  (let ((expr (read (current-buffer))))
		    (when (and (listp expr)
			       (memq (car expr) '(defcustom defface defgroup)))
		      (eval expr)
		      (put (nth 1 expr) 'custom-where name)))))
	    (error nil))))))
  (mapatoms (lambda (symbol)
	      (let ((members (get symbol 'custom-group))
		    item where found)
		(when members
		  (princ "(put '")
		  (princ symbol)
		  (princ " 'custom-loads '(")
		  (while members
		    (setq item (car (car members))
			  members (cdr members)
			  where (get item 'custom-where))
		    (unless (or (null where)
				(member where found))
		      (when found
			(princ " "))
		      (prin1 where)
		      (push where found)))
		  (princ "))\n"))))))

;;; The End.

(provide 'cus-edit)

;; cus-edit.el ends here
