;;; forms.el -- Forms Mode - A GNU Emacs Major Mode
;;; SCCS Status     : @(#)@ forms	1.2.7
;;; Author          : Johan Vromans
;;; Created On      : 1989
;;; Last Modified By: Johan Vromans
;;; Last Modified On: Mon Jul  1 14:13:20 1991
;;; Update Count    : 15
;;; Status          : OK

;;; This file is part of GNU Emacs.
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities. 
;;; If you don't have this copy, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

;;; HISTORY 
;;; 1-Jul-1991		Johan Vromans	
;;;    Normalized error messages.
;;; 30-Jun-1991		Johan Vromans	
;;;    Add support for forms-modified-record-filter.
;;;    Allow the filter functions to be the name of a function.
;;;    Fix: parse--format used forms--dynamic-text destructively.
;;;    Internally optimized the forms-format-list.
;;;    Added support for debugging.
;;;    Stripped duplicate documentation.
;;;   
;;; 29-Jun-1991		Johan Vromans	
;;;    Add support for functions and lisp symbols in forms-format-list.
;;;    Add function forms-enumerate.

(provide 'forms-mode)

;;; Visit a file using a form.
;;;
;;; === Naming conventions
;;;
;;; The names of all variables and functions start with 'form-'.
;;; Names which start with 'form--' are intended for internal use, and
;;; should *NOT* be used from the outside.
;;;
;;; All variables are buffer-local, to enable multiple forms visits 
;;; simultaneously.
;;; Variable 'forms--mode-setup' is local to *ALL* buffers, for it 
;;; controls if forms-mode has been enabled in a buffer.
;;;
;;; === How it works ===
;;;
;;; Forms mode means visiting a data file which is supposed to consist
;;; of records each containing a number of fields. The records are
;;; separated by a newline, the fields are separated by a user-defined
;;; field separater (default: TAB).
;;; When shown, a record is transferred to an emacs buffer and
;;; presented using a user-defined form. One record is shown at a
;;; time.
;;;
;;; Forms mode is a composite mode. It involves two files, and two
;;; buffers.
;;; The first file, called the control file, defines the name of the
;;; data file and the forms format. This file buffer will be used to
;;; present the forms.
;;; The second file holds the actual data. The buffer of this file
;;; will be buried, for it is never accessed directly.
;;;
;;; Forms mode is invoked using "forms-find-file control-file".
;;; Alternativily forms-find-file-other-window can be used.
;;;
;;; You may also visit the control file, and switch to forms mode by hand
;;; with M-x forms-mode .
;;;
;;; Automatic mode switching is supported, so you may use "find-file"
;;; if you specify "-*- forms -*-" in the first line of the control file.
;;; 
;;; The control file is visited, evaluated using
;;; eval-current-buffer, and should set at least the following
;;; variables:
;;;
;;;	forms-file		    [string] the name of the data file.
;;;
;;;	forms-number-of-fields	    [integer]
;;;			The number of fields in each record.
;;;
;;;	forms-format-list           [list]   formatting instructions.
;;;
;;; The forms-format-list should be a list, each element containing
;;;
;;;  - a string, e.g. "hello" (which is inserted \"as is\"),
;;;
;;;  - an integer, denoting a field number. The contents of the field
;;;    are inserted at this point.
;;;    The first field has number one.
;;;
;;;  - a function call, e.g. (insert "text"). This function call is 
;;;    dynamically evaluated and should return a string. It should *NOT*
;;;    have side-effects on the forms being constructed.
;;;    The current fields are available to the function in the variable
;;;    forms-fields, they should *NOT* be modified.
;;;
;;;  - a lisp symbol, that must evaluate to one of the above.
;;;
;;; Optional variables which may be set in the control file:
;;;
;;;	forms-field-sep				[string, default TAB]
;;;			The field separator used to separate the
;;;			fields in the data file. It may be a string.
;;;
;;;	forms-read-only				[bool, default nil]
;;;			't' means that the data file is visited read-only.
;;;			If no write access to the data file is
;;;			possible, read-only mode is enforced. 
;;;
;;;	forms-multi-line			[string, default "^K"]
;;;			If non-null the records of the data file may
;;;			contain fields which span multiple lines in
;;;			the form.
;;;			This variable denoted the separator character
;;;			to be used for this purpose. Upon display, all
;;;			occurrencies of this character are translated
;;;			to newlines. Upon storage they are translated
;;;			back to the separator.
;;;
;;;	forms-forms-scroll			[bool, default t]
;;;			If non-nil: redefine scroll-up/down to perform
;;;			forms-next/prev-field if in forms mode.
;;;
;;;	forms-forms-jump			[bool, default t]
;;;			If non-nil: redefine beginning/end-of-buffer
;;;			to performs forms-first/last-field if in
;;;			forms mode.
;;;
;;;	forms-new-record-filter			[symbol, no default]
;;;			If defined: this should be the name of a 
;;;			function that is called when a new
;;;			record is created. It can be used to fill in
;;;			the new record with default fields, for example.
;;;			Instead of the name of the function, it may
;;;			be the function itself.
;;;
;;;	forms-modified-record-filter		[symbol, no default]
;;;			If defined: this should be the name of a 
;;;			function that is called when a record has
;;;			been modified. It is called after the fields
;;;			are parsed. It can be used to register
;;;			modification dates, for example.
;;;			Instead of the name of the function, it may
;;;			be the function itself.
;;;
;;; After evaluating the control file, its buffer is cleared and used
;;; for further processing.
;;; The data file (as designated by "forms-file") is visited in a buffer
;;; (forms--file-buffer) which will not normally be shown.
;;; Great malfunctioning may be expected if this file/buffer is modified
;;; outside of this package while it's being visited!
;;;
;;; A record from the data file is transferred from the data file,
;;; split into fields (into forms--the-record-list), and displayed using
;;; the specs in forms-format-list.
;;; A format routine 'forms--format' is built upon startup to format 
;;; the records.
;;;
;;; When a form is changed the record is updated as soon as this form
;;; is left. The contents of the form are parsed using forms-format-list,
;;; and the fields which are deduced from the form are modified. So,
;;; fields not shown on the forms retain their origional values.
;;; The newly formed record and replaces the contents of the
;;; old record in forms--file-buffer.
;;; A parse routine 'forms--parser' is built upon startup to parse
;;; the records.
;;;
;;; Two exit functions exist: forms-exit (which saves) and forms-exit-no-save
;;; (which doesn't). However, if forms-exit-no-save is executed and the file
;;; buffer has been modified, emacs will ask questions.
;;;
;;; Other functions are:
;;;
;;;	paging (forward, backward) by record
;;;	jumping (first, last, random number)
;;;	searching
;;;	creating and deleting records
;;;	reverting the form (NOT the file buffer)
;;;	switching edit <-> view mode v.v.
;;;	jumping from field to field
;;;
;;; As an documented side-effect: jumping to the last record in the
;;; file (using forms-last-record) will adjust forms--total-records if
;;; needed.
;;;
;;; Commands and keymaps:
;;;
;;; A local keymap 'forms-mode-map' is used in the forms buffer.
;;; As conventional, this map can be accessed with C-c prefix.
;;; In read-only mode, the C-c prefix must be omitted.
;;;
;;; Default bindings:
;;;
;;;	\C-c	forms-mode-map
;;;	TAB	forms-next-field
;;;	SPC 	forms-next-record
;;;	<	forms-first-record
;;;	>	forms-last-record
;;;	?	describe-mode
;;;	d	forms-delete-record
;;;	e	forms-edit-mode
;;;	i	forms-insert-record
;;;	j	forms-jump-record
;;;	n	forms-next-record
;;;	p	forms-prev-record
;;;	q	forms-exit
;;;	s	forms-search
;;;	v	forms-view-mode
;;;	x	forms-exit-no-save
;;;	DEL	forms-prev-record
;;;
;;; Standard functions scroll-up, scroll-down, beginning-of-buffer and
;;; end-of-buffer are wrapped with re-definitions, which map them to
;;; next/prev record and first/last record.
;;; Buffer-local variables forms-forms-scroll and forms-forms-jump
;;; may be used to control these redefinitions.
;;;
;;; Function save-buffer is also wrapped to perform a sensible action.
;;; A revert-file-hook is defined to revert a forms to original.
;;;
;;; For convenience, TAB is always bound to forms-next-field, so you
;;; don't need the C-c prefix for this command.
;;;
;;; Global variables and constants

(defconst forms-version "1.2.7"
  "Version of forms-mode implementation")

(defvar forms-forms-scrolls t
  "If non-null: redefine scroll-up/down to be used with forms-mode.")

(defvar forms-forms-jumps t
  "If non-null: redefine beginning/end-of-buffer to be used with forms-mode.")

(defvar forms-mode-hooks nil
  "Hook functions to be run upon entering forms mode.")
;;;
;;; Mandatory variables - must be set by evaluating the control file

(defvar forms-file nil
  "Name of the file holding the data.")

(defvar forms-format-list nil
  "List of formatting specifications.")

(defvar forms-number-of-fields nil
  "Number of fields per record.")

;;;
;;; Optional variables with default values

(defvar forms-field-sep "\t"
  "Field separator character (default TAB)")

(defvar forms-read-only nil
  "Read-only mode (defaults to the write access on the data file).")

(defvar forms-multi-line "\C-k"
  "Character to separate multi-line fields (default ^K)")

(defvar forms-forms-scroll t
  "Redefine scroll-up/down to perform forms-next/prev-record when in
 forms mode.")

(defvar forms-forms-jump t
  "Redefine beginning/end-of-buffer to perform forms-first/last-record
 when in forms mode.")

;;;
;;; Internal variables.

(defvar forms--file-buffer nil
  "Buffer which holds the file data")

(defvar forms--total-records 0
  "Total number of records in the data file.")

(defvar forms--current-record 0
  "Number of the record currently on the screen.")

(defvar forms-mode-map nil		; yes - this one is global
   "Keymap for form buffer.")

(defvar forms--markers nil
  "Field markers in the screen.")

(defvar forms--number-of-markers 0
  "Number of fields on screen.")

(defvar forms--the-record-list nil 
   "List of strings of the current record, as parsed from the file.")

(defvar forms--search-regexp nil
  "Last regexp used by forms-search.")

(defvar forms--format nil
  "Formatting routine.")

(defvar forms--parser nil
  "Forms parser routine.")

(defvar forms--mode-setup nil
  "Internal - keeps track of forms-mode being set-up.")
(make-variable-buffer-local 'forms--mode-setup)

(defvar forms--new-record-filter nil
  "Internal - set if a new record filter has been defined.")

(defvar forms--modified-record-filter nil
  "Internal - set if a modified record filter has been defined.")

(defvar forms--dynamic-text nil
  "Internal - holds dynamic text to insert between fields.")

(defvar forms-fields nil
  "List with fields of the current forms. First field has number 1.")

;;;
;;; forms-mode
;;;
;;; This is not a simple major mode, as usual. Therefore, forms-mode
;;; takes an optional argument 'primary' which is used for the initial
;;; set-up. Normal use would leave 'primary' to nil.
;;;
;;; A global buffer-local variable 'forms--mode-setup' has the same effect
;;; but makes it possible to auto-invoke forms-mode using find-file.
;;;
;;; Note: although it seems logical to have (make-local-variable) executed
;;; where the variable is first needed, I deliberately placed all calls
;;; in the forms-mode function.
 
(defun forms-mode (&optional primary)
  "Major mode to visit files in a field-structured manner using a form.

 Commands (prefix with C-c if not in read-only mode):
 \\{forms-mode-map}"

  (interactive)				; no - 'primary' is not prefix arg

  ;; Primary set-up: evaluate buffer and check if the mandatory
  ;; variables have been set.
  (if (or primary (not forms--mode-setup))
      (progn
	(kill-all-local-variables)

	;; make mandatory variables
	(make-local-variable 'forms-file)
	(make-local-variable 'forms-number-of-fields)
	(make-local-variable 'forms-format-list)

	;; make optional variables
	(make-local-variable 'forms-field-sep)
        (make-local-variable 'forms-read-only)
        (make-local-variable 'forms-multi-line)
	(make-local-variable 'forms-forms-scroll)
	(make-local-variable 'forms-forms-jump)
	(fmakunbound 'forms-new-record-filter)

	;; eval the buffer, should set variables
	(eval-current-buffer)

	;; check if the mandatory variables make sense.
	(or forms-file
	    (error "'forms-file' has not been set"))
	(or forms-number-of-fields
	    (error "'forms-number-of-fields' has not been set"))
	(or (> forms-number-of-fields 0)
	    (error "'forms-number-of-fields' must be > 0")
	(or (stringp forms-field-sep))
	    (error "'forms-field-sep' is not a string"))
	(if forms-multi-line
	    (if (and (stringp forms-multi-line)
		     (eq (length forms-multi-line) 1))
		(if (string= forms-multi-line forms-field-sep)
		    (error "'forms-multi-line' is equal to 'forms-field-sep'"))
	      (error "'forms-multi-line' must be nil or a one-character string")))
	    
	;; validate and process forms-format-list
	(make-local-variable 'forms--number-of-markers)
	(make-local-variable 'forms--markers)
	(forms--process-format-list)

	;; build the formatter and parser
	(make-local-variable 'forms--format)
	(forms--make-format)
	(make-local-variable 'forms--parser)
	(forms--make-parser)

	;; check if record filters are defined
	(make-local-variable 'forms--new-record-filter)
	(setq forms--new-record-filter 
	      (cond
	       ((fboundp 'forms-new-record-filter)
		(symbol-function 'forms-new-record-filter))
	       ((and (boundp 'forms-new-record-filter)
		     (fboundp forms-new-record-filter))
		forms-new-record-filter)))
	(fmakunbound 'forms-new-record-filter)
	(make-local-variable 'forms--modified-record-filter)
	(setq forms--modified-record-filter 
	      (cond
	       ((fboundp 'forms-modified-record-filter)
		(symbol-function 'forms-modified-record-filter))
	       ((and (boundp 'forms-modified-record-filter)
		     (fboundp forms-modified-record-filter))
		forms-modified-record-filter)))
	(fmakunbound 'forms-modified-record-filter)

	;; dynamic text support
	(make-local-variable 'forms--dynamic-text)
	(make-local-variable 'forms-fields)

	;; prepare this buffer for further processing
	(setq buffer-read-only nil)

	;; prevent accidental overwrite of the control file and autosave
	(setq buffer-file-name nil)
	(auto-save-mode nil)

	;; and clean it
	(erase-buffer)))

  ;; make local variables
  (make-local-variable 'forms--file-buffer)
  (make-local-variable 'forms--total-records)
  (make-local-variable 'forms--current-record)
  (make-local-variable 'forms--the-record-list)
  (make-local-variable 'forms--search-rexexp)

  ;; A bug in the current Emacs release prevents a keymap
  ;; which is buffer-local from being used by 'describe-mode'.
  ;; Hence we'll leave it global.
  ;;(make-local-variable 'forms-mode-map)
  (if forms-mode-map			; already defined
      nil
    (setq forms-mode-map (make-keymap))
    (forms--mode-commands forms-mode-map)
    (forms--change-commands))

  ;; find the data file
  (setq forms--file-buffer (find-file-noselect forms-file))

  ;; count the number of records, and set see if it may be modified
  (let (ro)
    (setq forms--total-records
	  (save-excursion
	    (set-buffer forms--file-buffer)
	    (bury-buffer (current-buffer))
	    (setq ro buffer-read-only)
	    (count-lines (point-min) (point-max))))
    (if ro
	(setq forms-read-only t)))

  ;; set the major mode indicator
  (setq major-mode 'forms-mode)
  (setq mode-name "Forms")
  (make-local-variable 'minor-mode-alist) ; needed?
  (forms--set-minor-mode)
  (forms--set-keymaps)

  (set-buffer-modified-p nil)

  ;; We have our own revert function - use it
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'forms-revert-buffer)

  ;; setup the first (or current) record to show
  (if (< forms--current-record 1)
      (setq forms--current-record 1))
  (forms-jump-record forms--current-record)

  ;; user customising
  (run-hooks 'forms-mode-hooks)

  ;; be helpful
  (forms--help)

  ;; initialization done
  (setq forms--mode-setup t))

;;;
;;; forms-process-format-list
;;;
;;; Validates forms-format-list.
;;;
;;; Sets forms--number-of-markers and forms--markers.

(defun forms--process-format-list ()
  "Validate forms-format-list and set some global variables."

  (forms--debug "forms-forms-list before 1st pass:\n"
		'forms-format-list)

  ;; it must be non-nil
  (or forms-format-list
      (error "'forms-format-list' has not been set"))
  ;; it must be a list ...
  (or (listp forms-format-list)
      (error "'forms-format-list' is not a list"))

  (setq forms--number-of-markers 0)

  (let ((the-list forms-format-list)	; the list of format elements
	(this-item 0)			; element in list
	(field-num 0))			; highest field number 

    (setq forms-format-list nil)	; gonna rebuild

    (while the-list

      (let ((el (car-safe the-list))
	    (rem (cdr-safe the-list)))

	;; if it is a symbol, eval it first
	(if (and (symbolp el)
		 (boundp el))
	    (setq el (eval el)))

	(cond

	 ;; try string ...
	 ((stringp el))			; string is OK
	  
	 ;; try numeric ...
	 ((numberp el) 

	  (if (or (<= el 0)
		  (> el forms-number-of-fields))
	      (error
	       "Forms error: field number %d out of range 1..%d"
	       el forms-number-of-fields))

	  (setq forms--number-of-markers (1+ forms--number-of-markers))
	  (if (> el field-num)
	      (setq field-num el)))

	 ;; try function
	 ((listp el)
	  (or (fboundp (car-safe el))
	      (error 
	       "Forms error: not a function: %s"
	       (prin1-to-string (car-safe el)))))

	 ;; else
	 (t
	  (error "Invalid element in 'forms-format-list': %s"
		 (prin1-to-string el))))

	;; advance to next element of the list
	(setq the-list rem)
	(setq forms-format-list
	      (append forms-format-list (list el) nil)))))

  (forms--debug "forms-forms-list after 1st pass:\n"
		'forms-format-list)

  ;; concat adjacent strings
  (setq forms-format-list (forms--concat-adjacent forms-format-list))

  (forms--debug "forms-forms-list after 2nd pass:\n"
		'forms-format-list
		'forms--number-of-markers)

  (setq forms--markers (make-vector forms--number-of-markers nil)))


;;;
;;; Build the format routine from forms-format-list.
;;;
;;; The format routine (forms--format) will look like
;;; 
;;; (lambda (arg)
;;;   (setq forms--dynamic-text nil)
;;;   ;;  "text: "
;;;   (insert "text: ")
;;;   ;;  6
;;;   (aset forms--markers 0 (point-marker))
;;;   (insert (elt arg 5))
;;;   ;;  "\nmore text: "
;;;   (insert "\nmore text: ")
;;;   ;;  (tocol 40)
;;;   (let ((the-dyntext (tocol 40)))
;;;     (insert the-dyntext)
;;;     (setq forms--dynamic-text (append forms--dynamic-text
;;;					  (list the-dyntext))))
;;;   ;;  9
;;;   (aset forms--markers 1 (point-marker))
;;;   (insert (elt arg 8))
;;;
;;;   ... )
;;; 

(defun forms--make-format ()
  "Generate format function for forms"
  (setq forms--format (forms--format-maker forms-format-list))
  (forms--debug 'forms--format))

(defun forms--format-maker (the-format-list)
  "Returns the parser function for forms"
  (let ((the-marker 0))
    (` (lambda (arg)
	 (setq forms--dynamic-text nil)
	 (,@ (apply 'append
		    (mapcar 'forms--make-format-elt the-format-list)))))))

(defun forms--make-format-elt (el)
  (cond ((stringp el)
	 (` ((insert (, el)))))
	((numberp el)
	 (prog1
	     (` ((aset forms--markers (, the-marker) (point-marker))
		 (insert (elt arg (, (1- el))))))
	   (setq the-marker (1+ the-marker))))
	((listp el)
	 (prog1
	     (` ((let ((the-dyntext (, el)))
		   (insert the-dyntext)
		   (setq forms--dynamic-text (append forms--dynamic-text
						     (list the-dyntext)))))
		)))
	))


(defun forms--concat-adjacent (the-list)
  "Concatenate adjacent strings in the-list and return the resulting list"
  (if (consp the-list)
      (let ((the-rest (forms--concat-adjacent (cdr the-list))))
	(if (and (stringp (car the-list)) (stringp (car the-rest)))
	    (cons (concat (car the-list) (car the-rest))
		  (cdr the-rest))
	    (cons (car the-list) the-rest)))
      the-list))
;;;
;;; forms--make-parser.
;;;
;;; Generate parse routine from forms-format-list.
;;;
;;; The parse routine (forms--parser) will look like (give or take
;;; a few " " .
;;; 
;;; (lambda nil
;;;   (let (here)
;;;     (goto-char (point-min))
;;; 
;;;	;;  "text: "
;;;     (if (not (looking-at "text: "))
;;; 	    (error "Parse error: cannot find \"text: \""))
;;;     (forward-char 6)	; past "text: "
;;; 
;;;     ;;  6
;;;	;;  "\nmore text: "
;;;     (setq here (point))
;;;     (if (not (search-forward "\nmore text: " nil t nil))
;;; 	    (error "Parse error: cannot find \"\\nmore text: \""))
;;;     (aset the-recordv 5 (buffer-substring here (- (point) 12)))
;;;
;;;	;;  (tocol 40)
;;;	(let ((the-dyntext (car-safe forms--dynamic-text)))
;;;	  (if (not (looking-at (regexp-quote the-dyntext)))
;;;	      (error "Parse error: not looking at \"%s\"" the-dyntext))
;;;	  (forward-char (length the-dyntext))
;;;	  (setq forms--dynamic-text (cdr-safe forms--dynamic-text)))
;;;     ... 
;;;     ;; final flush (due to terminator sentinel, see below)
;;;	(aset the-recordv 7 (buffer-substring (point) (point-max)))
;;; 

(defun forms--make-parser ()
  "Generate parser function for forms"
  (setq forms--parser (forms--parser-maker forms-format-list))
  (forms--debug 'forms--parser))

(defun forms--parser-maker (the-format-list)
  "Returns the parser function for forms"
  (let ((the-field nil)
	(seen-text nil)
	the--format-list)
    ;; add a terminator sentinel
    (setq the--format-list (append the-format-list (list nil)))
    (` (lambda nil
	 (let (here)
	   (goto-char (point-min))
	 (,@ (apply 'append
		    (mapcar 'forms--make-parser-elt the--format-list))))))))

(defun forms--make-parser-elt (el)
  (cond
   ((stringp el)
    (prog1
	(if the-field
	    (` ((setq here (point))
		(if (not (search-forward (, el) nil t nil))
		    (error "Parse error: cannot find \"%s\"" (, el)))
		(aset the-recordv (, (1- the-field))
		      (buffer-substring here
					(- (point) (, (length el)))))))
	  (` ((if (not (looking-at (, (regexp-quote el))))
		  (error "Parse error: not looking at \"%s\"" (, el)))
	      (forward-char (, (length el))))))
      (setq seen-text t)
      (setq the-field nil)))
   ((numberp el)
    (if the-field
	(error "Cannot parse adjacent fields %d and %d"
	       the-field el)
      (setq the-field el)
      nil))
   ((null el)
    (if the-field
	(` ((aset the-recordv (, (1- the-field))
		  (buffer-substring (point) (point-max)))))))
   ((listp el)
    (prog1
	(if the-field
	    (` ((let ((here (point))
		      (the-dyntext (car-safe forms--dynamic-text)))
		  (if (not (search-forward the-dyntext nil t nil))
		      (error "Parse error: cannot find \"%s\"" the-dyntext))
		  (aset the-recordv (, (1- the-field))
			(buffer-substring here
					  (- (point) (length the-dyntext))))
		  (setq forms--dynamic-text (cdr-safe forms--dynamic-text)))))
	  (` ((let ((the-dyntext (car-safe forms--dynamic-text)))
		(if (not (looking-at (regexp-quote the-dyntext)))
		    (error "Parse error: not looking at \"%s\"" the-dyntext))
		(forward-char (length the-dyntext))
		(setq forms--dynamic-text (cdr-safe forms--dynamic-text))))))
      (setq seen-text t)
      (setq the-field nil)))
   ))
;;;

(defun forms--set-minor-mode ()
  (setq minor-mode-alist
	(if forms-read-only
	    " View"
	  nil)))

(defun forms--set-keymaps ()
  "Set the keymaps used in this mode."

  (if forms-read-only
      (use-local-map forms-mode-map)
    (use-local-map (make-sparse-keymap))
    (define-key (current-local-map) "\C-c" forms-mode-map)
    (define-key (current-local-map) "\t"   'forms-next-field)))

(defun forms--mode-commands (map)
  "Fill map with all commands."
  (define-key map "\t" 'forms-next-field)
  (define-key map " " 'forms-next-record)
  (define-key map "d" 'forms-delete-record)
  (define-key map "e" 'forms-edit-mode)
  (define-key map "i" 'forms-insert-record)
  (define-key map "j" 'forms-jump-record)
  (define-key map "n" 'forms-next-record)
  (define-key map "p" 'forms-prev-record)
  (define-key map "q" 'forms-exit)
  (define-key map "s" 'forms-search)
  (define-key map "v" 'forms-view-mode)
  (define-key map "x" 'forms-exit-no-save)
  (define-key map "<" 'forms-first-record)
  (define-key map ">" 'forms-last-record)
  (define-key map "?" 'describe-mode)
  (define-key map "\177" 'forms-prev-record)
 ;  (define-key map "\C-c" map)
  (define-key map "\e" 'ESC-prefix)
  (define-key map "\C-x" ctl-x-map)
  (define-key map "\C-u" 'universal-argument)
  (define-key map "\C-h" help-map)
  )
;;;
;;; Changed functions
;;;
;;; Emacs (as of 18.55) lacks the functionality of buffer-local
;;; funtions. Therefore we save the original meaning of some handy
;;; functions, and replace them with a wrapper.

(defun forms--change-commands ()
  "Localize some commands."
  ;;
  ;; scroll-down -> forms-prev-record
  ;;
  (if (fboundp 'forms--scroll-down)
      nil
    (fset 'forms--scroll-down (symbol-function 'scroll-down))
    (fset 'scroll-down
	  '(lambda (&optional arg) 
	     (interactive "P")
	     (if (and forms--mode-setup
		      forms-forms-scroll)
		 (forms-prev-record arg)
	       (forms--scroll-down arg)))))
  ;;
  ;; scroll-up -> forms-next-record
  ;;
  (if (fboundp 'forms--scroll-up)
      nil
    (fset 'forms--scroll-up   (symbol-function 'scroll-up))
    (fset 'scroll-up
	  '(lambda (&optional arg) 
	     (interactive "P")
	     (if (and forms--mode-setup
		      forms-forms-scroll)
		 (forms-next-record arg)
	       (forms--scroll-up arg)))))
  ;;
  ;; beginning-of-buffer -> forms-first-record
  ;;
  (if (fboundp 'forms--beginning-of-buffer)
      nil
    (fset 'forms--beginning-of-buffer (symbol-function 'beginning-of-buffer))
    (fset 'beginning-of-buffer
	  '(lambda ()
	     (interactive)
	     (if (and forms--mode-setup
		      forms-forms-jump)
		 (forms-first-record)
	       (forms--beginning-of-buffer)))))
  ;;
  ;; end-of-buffer -> forms-end-record
  ;;
  (if (fboundp 'forms--end-of-buffer)
      nil
    (fset 'forms--end-of-buffer (symbol-function 'end-of-buffer))
    (fset 'end-of-buffer
	  '(lambda ()
	     (interactive)
	     (if (and forms--mode-setup
		      forms-forms-jump)
		 (forms-last-record)
	       (forms--end-of-buffer)))))
  ;;
  ;; save-buffer -> forms--save-buffer
  ;;
  (if (fboundp 'forms--save-buffer)
      nil
    (fset 'forms--save-buffer (symbol-function 'save-buffer))
    (fset 'save-buffer
	  '(lambda (&optional arg)
	     (interactive "p")
	     (if forms--mode-setup
		 (progn
		   (forms--checkmod)
		   (save-excursion
		     (set-buffer forms--file-buffer)
		     (forms--save-buffer arg)))
	       (forms--save-buffer arg)))))
  ;;
  )

(defun forms--help ()
  "Initial help."
  ;; We should use
  ;;(message (substitute-command-keys (concat
  ;;"\\[forms-next-record]:next"
  ;;"   \\[forms-prev-record]:prev"
  ;;"   \\[forms-first-record]:first"
  ;;"   \\[forms-last-record]:last"
  ;;"   \\[describe-mode]:help"
  ;;"   \\[forms-exit]:exit")))
  ;; but it's too slow ....
  (if forms-read-only
      (message "SPC:next   DEL:prev   <:first   >:last   ?:help   q:exit")
    (message "C-c n:next   C-c p:prev   C-c <:first   C-c >:last   C-c ?:help   C-c q:exit")))

(defun forms--trans (subj arg rep)
  "Translate in SUBJ all chars ARG into char REP. ARG and REP should
 be single-char strings."
  (let ((i 0)
	(x (length subj))
	(re (regexp-quote arg))
	(k (string-to-char rep)))
    (while (setq i (string-match re subj i))
      (aset subj i k)
      (setq i (1+ i)))))

(defun forms--exit (query &optional save)
  (let ((buf (buffer-name forms--file-buffer)))
    (forms--checkmod)
    (if (and save
	     (buffer-modified-p forms--file-buffer))
	(save-excursion
	  (set-buffer forms--file-buffer)
	  (save-buffer)))
    (save-excursion
      (set-buffer forms--file-buffer)
      (delete-auto-save-file-if-necessary)
      (kill-buffer (current-buffer)))
    (if (get-buffer buf)	; not killed???
      (if save
	  (progn
	    (beep)
	    (message "Problem saving buffers?")))
      (delete-auto-save-file-if-necessary)
      (kill-buffer (current-buffer)))))

(defun forms--get-record ()
  "Fetch the current record from the file buffer."
  ;;
  ;; This function is executed in the context of the forms--file-buffer.
  ;;
  (or (bolp)
      (beginning-of-line nil))
  (let ((here (point)))
    (prog2
     (end-of-line)
     (buffer-substring here (point))
     (goto-char here))))

(defun forms--show-record (the-record)
  "Format THE-RECORD according to forms-format-list,
 and display it in the current buffer."

  ;; split the-record
  (let (the-result
	(start-pos 0)
	found-pos
	(field-sep-length (length forms-field-sep)))
    (if forms-multi-line
	(forms--trans the-record forms-multi-line "\n"))
    ;; add an extra separator (makes splitting easy)
    (setq the-record (concat the-record forms-field-sep))
    (while (setq found-pos (string-match forms-field-sep the-record start-pos))
      (let ((ent (substring the-record start-pos found-pos)))
	(setq the-result
	      (append the-result (list ent)))
	(setq start-pos (+ field-sep-length found-pos))))
    (setq forms--the-record-list the-result))

  (setq buffer-read-only nil)
  (erase-buffer)

  ;; verify the number of fields, extend forms--the-record-list if needed
  (if (= (length forms--the-record-list) forms-number-of-fields)
      nil
    (beep)
    (message "Record has %d fields instead of %d."
	     (length forms--the-record-list) forms-number-of-fields)
    (if (< (length forms--the-record-list) forms-number-of-fields)
	(setq forms--the-record-list 
	      (append forms--the-record-list
		      (make-list 
		       (- forms-number-of-fields 
			  (length forms--the-record-list))
		       "")))))

  ;; call the formatter function
  (setq forms-fields (append (list nil) forms--the-record-list nil))
  (funcall forms--format forms--the-record-list)

  ;; prepare
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only forms-read-only)
  (setq mode-line-process
	(concat " " forms--current-record "/" forms--total-records)))

(defun forms--parse-form ()
  "Parse contents of form into list of strings."
  ;; The contents of the form are parsed, and a new list of strings
  ;; is constructed.
  ;; A vector with the strings from the original record is 
  ;; constructed, which is updated with the new contents. Therefore
  ;; fields which were not in the form are not modified.
  ;; Finally, the vector is transformed into a list for further processing.

  (let (the-recordv)

    ;; build the vector
    (setq the-recordv (vconcat forms--the-record-list))

    ;; parse the form and update the vector
    (let ((forms--dynamic-text forms--dynamic-text))
      (funcall forms--parser))

    (if forms--modified-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (vconcat [nil] the-recordv)))
	  (setq the-fields (funcall forms--modified-record-filter the-fields))
	  (cdr (append the-fields nil)))

      ;; transform to a list and return
      (append the-recordv nil))))

(defun forms--update ()
  "Update current record with contents of form. As a side effect: sets
forms--the-record-list ."
  (if forms-read-only
      (progn
	(message "Read-only buffer!")
	(beep))

    (let (the-record)
      ;; build new record
      (setq forms--the-record-list (forms--parse-form))
      (setq the-record
	    (mapconcat 'identity forms--the-record-list forms-field-sep))

      ;; handle multi-line fields, if allowed
      (if forms-multi-line
	  (forms--trans the-record "\n" forms-multi-line))

      ;; a final sanity check before updating
      (if (string-match "\n" the-record)
	  (progn
	    (message "Multi-line fields in this record - update refused!")
	    (beep))

	(save-excursion
	  (set-buffer forms--file-buffer)
	  ;; Insert something before kill-line is called. See kill-line
	  ;; doc. Bugfix provided by Ignatios Souvatzis.
	  (insert "*")
	  (beginning-of-line)
	  (kill-line nil)
	  (insert the-record)
	  (beginning-of-line))))))

(defun forms--checkmod ()
  "Check if this form has been modified, and call forms--update if so."
  (if (buffer-modified-p nil)
      (let ((here (point)))
	(forms--update)
	(set-buffer-modified-p nil)
	(goto-char here))))

;;;
;;; Start and exit
(defun forms-find-file (fn)
  "Visit file FN in forms mode"
  (interactive "fForms file: ")
  (find-file-read-only fn)
  (or forms--mode-setup (forms-mode t)))

(defun forms-find-file-other-window (fn)
  "Visit file FN in form mode in other window"
  (interactive "fFbrowse file in other window: ")
  (find-file-other-window fn)
  (eval-current-buffer)
  (or forms--mode-setup (forms-mode t)))

(defun forms-exit (query)
  "Normal exit. Modified buffers are saved."
  (interactive "P")
  (forms--exit query t))

(defun forms-exit-no-save (query)
  "Exit without saving buffers."
  (interactive "P")
  (forms--exit query nil))

;;;
;;; Navigating commands

(defun forms-next-record (arg)
  "Advance to the ARGth following record."
  (interactive "P")
  (forms-jump-record (+ forms--current-record (prefix-numeric-value arg)) t))

(defun forms-prev-record (arg)
  "Advance to the ARGth previous record."
  (interactive "P")
  (forms-jump-record (- forms--current-record (prefix-numeric-value arg)) t))

(defun forms-jump-record (arg &optional relative)
  "Jump to a random record."
  (interactive "NRecord number: ")

  ;; verify that the record number is within range
  (if (or (> arg forms--total-records)
	  (<= arg 0))
    (progn
      (beep)
      ;; don't give the message if just paging
      (if (not relative)
	  (message "Record number %d out of range 1..%d"
		   arg forms--total-records))
      )

    ;; flush
    (forms--checkmod)

    ;; calculate displacement
    (let ((disp (- arg forms--current-record))
	  (cur forms--current-record))

      ;; forms--show-record needs it now
      (setq forms--current-record arg)

      ;; get the record and show it
      (forms--show-record
       (save-excursion
	 (set-buffer forms--file-buffer)
	 (beginning-of-line)

	 ;; move, and adjust the amount if needed (shouldn't happen)
	 (if relative
	     (if (zerop disp)
		 nil
	       (setq cur (+ cur disp (- (forward-line disp)))))
	   (setq cur (+ cur disp (- (goto-line arg)))))

	 (forms--get-record)))

      ;; this shouldn't happen
      (if (/= forms--current-record cur)
	  (progn
	    (setq forms--current-record cur)
	    (beep)
	    (message "Stuck at record %d." cur))))))

(defun forms-first-record ()
  "Jump to first record."
  (interactive)
  (forms-jump-record 1))

(defun forms-last-record ()
  "Jump to last record. As a side effect: re-calculates the number
 of records in the data file."
  (interactive)
  (let
      ((numrec 
	(save-excursion
	  (set-buffer forms--file-buffer)
	  (count-lines (point-min) (point-max)))))
    (if (= numrec forms--total-records)
	nil
      (beep)
      (setq forms--total-records numrec)
      (message "Number of records reset to %d." forms--total-records)))
  (forms-jump-record forms--total-records))

;;;
;;; Other commands
(defun forms-view-mode ()
  "Visit buffer read-only."
  (interactive)
  (if forms-read-only
      nil
    (forms--checkmod)			; sync
    (setq forms-read-only t)
    (forms-mode)))

(defun forms-edit-mode ()
  "Make form suitable for editing, if possible."
  (interactive)
  (let ((ro forms-read-only))
    (if (save-excursion
	  (set-buffer forms--file-buffer)
	  buffer-read-only)
	(progn
	  (setq forms-read-only t)
	  (message "No write access to \"%s\"" forms-file)
	  (beep))
      (setq forms-read-only nil))
    (if (equal ro forms-read-only)
	nil
      (forms-mode))))

;; Sample:
;; (defun my-new-record-filter (the-fields)
;;   ;; numbers are relative to 1
;;   (aset the-fields 4 (current-time-string))
;;   (aset the-fields 6 (user-login-name))
;;   the-list)
;; (setq forms-new-record-filter 'my-new-record-filter)

(defun forms-insert-record (arg)
  "Create a new record before the current one. With ARG: store the
 record after the current one.
 If a function forms-new-record-filter is defined, or forms-new-record-filter
 contains the name of a function, it is called to
 fill (some of) the fields with default values."
 ; The above doc is not true, but for documentary purposes only

  (interactive "P")

  (let ((ln (if arg (1+ forms--current-record) forms--current-record))
        the-list the-record)

    (forms--checkmod)
    (if forms--new-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (make-vector (1+ forms-number-of-fields) "")))
	  (setq the-fields (funcall forms--new-record-filter the-fields))
	  (setq the-list (cdr (append the-fields nil))))
      (setq the-list (make-list forms-number-of-fields "")))

    (setq the-record
	  (mapconcat
	  'identity
	  the-list
	  forms-field-sep))

    (save-excursion
      (set-buffer forms--file-buffer)
      (goto-line ln)
      (open-line 1)
      (insert the-record)
      (beginning-of-line))
    
    (setq forms--current-record ln))

  (setq forms--total-records (1+ forms--total-records))
  (forms-jump-record forms--current-record))

(defun forms-delete-record (arg)
  "Deletes a record. With ARG: don't ask."
  (interactive "P")
  (forms--checkmod)
  (if (or arg
	  (y-or-n-p "Really delete this record? "))
      (let ((ln forms--current-record))
	(save-excursion
	  (set-buffer forms--file-buffer)
	  (goto-line ln)
	  (kill-line 1))
	(setq forms--total-records (1- forms--total-records))
	(if (> forms--current-record forms--total-records)
	    (setq forms--current-record forms--total-records))
	(forms-jump-record forms--current-record)))
  (message ""))

(defun forms-search (regexp)
  "Search REGEXP in file buffer."
  (interactive 
   (list (read-string (concat "Search for" 
				  (if forms--search-regexp
				   (concat " ("
					   forms--search-regexp
					   ")"))
				  ": "))))
  (if (equal "" regexp)
      (setq regexp forms--search-regexp))
  (forms--checkmod)

  (let (the-line the-record here
		 (fld-sep forms-field-sep))
    (if (save-excursion
	  (set-buffer forms--file-buffer)
	  (setq here (point))
	  (end-of-line)
	  (if (null (re-search-forward regexp nil t))
	      (progn
		(goto-char here)
		(message (concat "\"" regexp "\" not found."))
		nil)
	    (setq the-record (forms--get-record))
	    (setq the-line (1+ (count-lines (point-min) (point))))))
	(progn
	  (setq forms--current-record the-line)
	  (forms--show-record the-record)
	  (re-search-forward regexp nil t))))
  (setq forms--search-regexp regexp))

(defun forms-revert-buffer (&optional arg noconfirm)
  "Reverts current form to un-modified."
  (interactive "P")
  (if (or noconfirm
	  (yes-or-no-p "Revert form to unmodified? "))
      (progn
	(set-buffer-modified-p nil)
	(forms-jump-record forms--current-record))))

(defun forms-next-field (arg)
  "Jump to ARG-th next field."
  (interactive "p")

  (let ((i 0)
	(here (point))
	there
	(cnt 0))

    (if (zerop arg)
	(setq cnt 1)
      (setq cnt (+ cnt arg)))

    (if (catch 'done
	  (while (< i forms--number-of-markers)
	    (if (or (null (setq there (aref forms--markers i)))
		    (<= there here))
		nil
	      (if (<= (setq cnt (1- cnt)) 0)
		  (progn
		    (goto-char there)
		    (throw 'done t))))
	    (setq i (1+ i))))
	nil
      (goto-char (aref forms--markers 0)))))

;;;
;;; Special service
;;;
(defun forms-enumerate (the-fields)
  "Take a quoted list of symbols, and set their values to the numbers
1, 2 and so on. Returns the higest number.

Usage: (setq forms-number-of-fields
             (forms-enumerate
              '(field1 field2 field2 ...)))"

  (let ((the-index 0))
    (while the-fields
      (setq the-index (1+ the-index))
      (let ((el (car-safe the-fields)))
	(setq the-fields (cdr-safe the-fields))
	(set el the-index)))
    the-index))

;;;
;;; Debugging
;;;
(defvar forms--debug nil
  "*Enables forms-mode debugging if not nil.")

(defun forms--debug (&rest args)
  "Internal - debugging routine"
  (if forms--debug
      (let ((ret nil))
	(while args
	  (let ((el (car-safe args)))
	    (setq args (cdr-safe args))
	    (if (stringp el)
		(setq ret (concat ret el))
	      (setq ret (concat ret (prin1-to-string el) " = "))
	      (if (boundp el)
		  (let ((vel (eval el)))
		    (setq ret (concat ret (prin1-to-string vel) "\n")))
		(setq ret (concat ret "<unbound>" "\n")))
	      (if (fboundp el)
		  (setq ret (concat ret (prin1-to-string (symbol-function el)) 
				    "\n"))))))
	(save-excursion
	  (set-buffer (get-buffer-create "*forms-mode debug*"))
	  (goto-char (point-max))
	  (insert ret)))))

;;; Local Variables:
;;; eval: (headers)
;;; eval: (setq comment-start ";;; ")
;;; End:
