;;; gnus-spec.el --- format spec functions for Gnus
;; Copyright (C) 1996,97 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'gnus)

;;; Internal variables.

(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)
(defvar gnus-group-indentation "")

;; Format specs.  The chunks below are the machine-generated forms
;; that are to be evaled as the result of the default format strings.
;; We write them in here to get them byte-compiled.  That way the
;; default actions will be quite fast, while still retaining the full
;; flexibility of the user-defined format specs.

;; First we have lots of dummy defvars to let the compiler know these
;; are really dynamic variables.

(defvar gnus-tmp-unread)
(defvar gnus-tmp-replied)
(defvar gnus-tmp-score-char)
(defvar gnus-tmp-indentation)
(defvar gnus-tmp-opening-bracket)
(defvar gnus-tmp-lines)
(defvar gnus-tmp-name)
(defvar gnus-tmp-closing-bracket)
(defvar gnus-tmp-subject-or-nil)
(defvar gnus-tmp-subject)
(defvar gnus-tmp-marked)
(defvar gnus-tmp-marked-mark)
(defvar gnus-tmp-subscribed)
(defvar gnus-tmp-process-marked)
(defvar gnus-tmp-number-of-unread)
(defvar gnus-tmp-group-name)
(defvar gnus-tmp-group)
(defvar gnus-tmp-article-number)
(defvar gnus-tmp-unread-and-unselected)
(defvar gnus-tmp-news-method)
(defvar gnus-tmp-news-server)
(defvar gnus-tmp-article-number)
(defvar gnus-mouse-face)
(defvar gnus-mouse-face-prop)

(defun gnus-summary-line-format-spec ()
  (insert gnus-tmp-unread gnus-tmp-replied
	  gnus-tmp-score-char gnus-tmp-indentation)
  (gnus-put-text-property
   (point)
   (progn
     (insert
      gnus-tmp-opening-bracket
      (format "%4d: %-20s"
	      gnus-tmp-lines
	      (if (> (length gnus-tmp-name) 20)
		  (substring gnus-tmp-name 0 20)
		gnus-tmp-name))
      gnus-tmp-closing-bracket)
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject-or-nil "\n"))

(defvar gnus-summary-line-format-spec
  (gnus-byte-code 'gnus-summary-line-format-spec))

(defun gnus-summary-dummy-line-format-spec ()
  (insert "*  ")
  (gnus-put-text-property
   (point)
   (progn
     (insert ":				 :")
     (point))
   gnus-mouse-face-prop gnus-mouse-face)
  (insert " " gnus-tmp-subject "\n"))

(defvar gnus-summary-dummy-line-format-spec
  (gnus-byte-code 'gnus-summary-dummy-line-format-spec))

(defun gnus-group-line-format-spec ()
  (insert gnus-tmp-marked-mark gnus-tmp-subscribed
	  gnus-tmp-process-marked
	  gnus-group-indentation
	  (format "%5s: " gnus-tmp-number-of-unread))
  (gnus-put-text-property
   (point)
   (progn
     (insert gnus-tmp-group "\n")
     (1- (point)))
   gnus-mouse-face-prop gnus-mouse-face))
(defvar gnus-group-line-format-spec
  (gnus-byte-code 'gnus-group-line-format-spec))

(defvar gnus-format-specs
  `((version . ,emacs-version)
    (group "%M\%S\%p\%P\%5y: %(%g%)%l\n" ,gnus-group-line-format-spec)
    (summary-dummy "*  %(:                          :%) %S\n"
		   ,gnus-summary-dummy-line-format-spec)
    (summary "%U\%R\%z\%I\%(%[%4L: %-20,20n%]%) %s\n"
	     ,gnus-summary-line-format-spec))
  "Alist of format specs.")

(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)

;;; Phew.  All that gruft is over, fortunately.

;;;###autoload
(defun gnus-update-format (var)
  "Update the format specification near point."
  (interactive
   (list
    (save-excursion
      (eval-defun nil)
      ;; Find the end of the current word.
      (re-search-forward "[ \t\n]" nil t)
      ;; Search backward.
      (when (re-search-backward "\\(gnus-[-a-z]+-line-format\\)" nil t)
	(match-string 1)))))
  (let* ((type (intern (progn (string-match "gnus-\\([-a-z]+\\)-line" var)
			      (match-string 1 var))))
	 (entry (assq type gnus-format-specs))
	 value spec)
    (when entry
      (setq gnus-format-specs (delq entry gnus-format-specs)))
    (set
     (intern (format "%s-spec" var))
     (gnus-parse-format (setq value (symbol-value (intern var)))
			(symbol-value (intern (format "%s-alist" var)))
			(not (string-match "mode" var))))
    (setq spec (symbol-value (intern (format "%s-spec" var))))
    (push (list type value spec) gnus-format-specs)

    (pop-to-buffer "*Gnus Format*")
    (erase-buffer)
    (lisp-interaction-mode)
    (insert (pp-to-string spec))))

(defun gnus-update-format-specifications (&optional force &rest types)
  "Update all (necessary) format specifications."
  ;; Make the indentation array.
  ;; See whether all the stored info needs to be flushed.
  (when (or force
	    (not (equal emacs-version
			(cdr (assq 'version gnus-format-specs)))))
    (setq gnus-format-specs nil))

  ;; Go through all the formats and see whether they need updating.
  (let (new-format entry type val)
    (while (setq type (pop types))
      ;; Jump to the proper buffer to find out the value of
      ;; the variable, if possible.  (It may be buffer-local.)
      (save-excursion
	(let ((buffer (intern (format "gnus-%s-buffer" type)))
	      val)
	  (when (and (boundp buffer)
		     (setq val (symbol-value buffer))
		     (get-buffer val)
		     (buffer-name (get-buffer val)))
	    (set-buffer (get-buffer val)))
	  (setq new-format (symbol-value
			    (intern (format "gnus-%s-line-format" type)))))
	(setq entry (cdr (assq type gnus-format-specs)))
	(if (and (car entry)
		 (equal (car entry) new-format))
	    ;; Use the old format.
	    (set (intern (format "gnus-%s-line-format-spec" type))
		 (cadr entry))
	  ;; This is a new format.
	  (setq val
		(if (not (stringp new-format))
		    ;; This is a function call or something.
		    new-format
		  ;; This is a "real" format.
		  (gnus-parse-format
		   new-format
		   (symbol-value
		    (intern (format "gnus-%s-line-format-alist"
				    (if (eq type 'article-mode)
					'summary-mode type))))
		   (not (string-match "mode$" (symbol-name type))))))
	  ;; Enter the new format spec into the list.
	  (if entry
	      (progn
		(setcar (cdr entry) val)
		(setcar entry new-format))
	    (push (list type new-format val) gnus-format-specs))
	  (set (intern (format "gnus-%s-line-format-spec" type)) val)))))

  (unless (assq 'version gnus-format-specs)
    (push (cons 'version emacs-version) gnus-format-specs)))

(defvar gnus-mouse-face-0 'highlight)
(defvar gnus-mouse-face-1 'highlight)
(defvar gnus-mouse-face-2 'highlight)
(defvar gnus-mouse-face-3 'highlight)
(defvar gnus-mouse-face-4 'highlight)

(defun gnus-mouse-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    gnus-mouse-face-prop
    ,(if (equal type 0)
	 'gnus-mouse-face
       `(quote ,(symbol-value (intern (format "gnus-mouse-face-%d" type)))))))

(defvar gnus-face-0 'bold)
(defvar gnus-face-1 'italic)
(defvar gnus-face-2 'bold-italic)
(defvar gnus-face-3 'bold)
(defvar gnus-face-4 'bold)

(defun gnus-face-face-function (form type)
  `(gnus-put-text-property
    (point) (progn ,@form (point))
    'face ',(symbol-value (intern (format "gnus-face-%d" type)))))

(defun gnus-tilde-max-form (el max-width)
  "Return a form that limits EL to MAX-WIDTH."
  (let ((max (abs max-width)))
    (if (symbolp el)
	`(if (> (length ,el) ,max)
	     ,(if (< max-width 0)
		  `(substring ,el (- (length el) ,max))
		`(substring ,el 0 ,max))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (length val) ,max)
	     ,(if (< max-width 0)
		  `(substring val (- (length val) ,max))
		`(substring val 0 ,max))
	   val)))))

(defun gnus-tilde-cut-form (el cut-width)
  "Return a form that cuts CUT-WIDTH off of EL."
  (let ((cut (abs cut-width)))
    (if (symbolp el)
	`(if (> (length ,el) ,cut)
	     ,(if (< cut-width 0)
		  `(substring ,el 0 (- (length el) ,cut))
		`(substring ,el ,cut))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (length val) ,cut)
	     ,(if (< cut-width 0)
		  `(substring val 0 (- (length val) ,cut))
		`(substring val ,cut))
	   val)))))

(defun gnus-tilde-ignore-form (el ignore-value)
  "Return a form that is blank when EL is IGNORE-VALUE."
  (if (symbolp el)
      `(if (equal ,el ,ignore-value)
	   "" ,el)
    `(let ((val (eval ,el)))
       (if (equal val ,ignore-value)
	   "" val))))

(defun gnus-parse-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  (if (string-match
       "\\`\\(.*\\)%[0-9]?[{(]\\(.*\\)%[0-9]?[})]\\(.*\n?\\)\\'"
       format)
      (gnus-parse-complex-format format spec-alist)
    ;; This is a simple format.
    (gnus-parse-simple-format format spec-alist insert)))

(defun gnus-parse-complex-format (format spec-alist)
  (save-excursion
    (gnus-set-work-buffer)
    (insert format)
    (goto-char (point-min))
    (while (re-search-forward "\"" nil t)
      (replace-match "\\\"" nil t))
    (goto-char (point-min))
    (insert "(\"")
    (while (re-search-forward "%\\([0-9]+\\)?\\([{}()]\\)" nil t)
      (let ((number (if (match-beginning 1)
			(match-string 1) "0"))
	    (delim (aref (match-string 2) 0)))
	(if (or (= delim ?\() (= delim ?\{))
	    (replace-match (concat "\"(" (if (= delim ?\() "mouse" "face")
				   " " number " \""))
	  (replace-match "\")\""))))
    (goto-char (point-max))
    (insert "\")")
    (goto-char (point-min))
    (let ((form (read (current-buffer))))
      (cons 'progn (gnus-complex-form-to-spec form spec-alist)))))

(defun gnus-complex-form-to-spec (form spec-alist)
  (delq nil
	(mapcar
	 (lambda (sform)
	   (if (stringp sform)
	       (gnus-parse-simple-format sform spec-alist t)
	     (funcall (intern (format "gnus-%s-face-function" (car sform)))
		      (gnus-complex-form-to-spec (cddr sform) spec-alist)
		      (nth 1 sform))))
	 form)))

(defun gnus-parse-simple-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return a
  ;; string.
  (let ((max-width 0)
	spec flist fstring elem result dontinsert user-defined
	type value pad-width spec-beg cut-width ignore-value
	tilde-form tilde elem-type)
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "%" nil t)
	(setq user-defined nil
	      spec-beg nil
	      pad-width nil
	      max-width nil
	      cut-width nil
	      ignore-value nil
	      tilde-form nil)
	(setq spec-beg (1- (point)))

	;; Parse this spec fully.
	(while
	    (cond
	     ((looking-at "\\([-.0-9]+\\)\\(,[-0-9]+\\)?")
	      (setq pad-width (string-to-number (match-string 1)))
	      (when (match-beginning 2)
		(setq max-width (string-to-number (buffer-substring
						   (1+ (match-beginning 2))
						   (match-end 2)))))
	      (goto-char (match-end 0)))
	     ((looking-at "~")
	      (forward-char 1)
	      (setq tilde (read (current-buffer))
		    type (car tilde)
		    value (cadr tilde))
	      (cond
	       ((memq type '(pad pad-left))
		(setq pad-width value))
	       ((eq type 'pad-right)
		(setq pad-width (- value)))
	       ((memq type '(max-right max))
		(setq max-width value))
	       ((eq type 'max-left)
		(setq max-width (- value)))
	       ((memq type '(cut cut-left))
		(setq cut-width value))
	       ((eq type 'cut-right)
		(setq cut-width (- value)))
	       ((eq type 'ignore)
		(setq ignore-value
		      (if (stringp value) value (format "%s" value))))
	       ((eq type 'form)
		(setq tilde-form value))
	       (t
		(error "Unknown tilde type: %s" tilde)))
	      t)
	     (t
	      nil)))
	;; User-defined spec -- find the spec name.
	(when (= (setq spec (following-char)) ?u)
	  (forward-char 1)
	  (setq user-defined (following-char)))
	(forward-char 1)
	(delete-region spec-beg (point))

	;; Now we have all the relevant data on this spec, so
	;; we start doing stuff.
	(insert "%")
	(if (eq spec ?%)
	    ;; "%%" just results in a "%".
	    (insert "%")
	  (cond
	   ;; Do tilde forms.
	   ((eq spec ?@)
	    (setq elem (list tilde-form ?s)))
	   ;; Treat user defined format specifiers specially.
	   (user-defined
	    (setq elem
		  (list
		   (list (intern (format "gnus-user-format-function-%c"
					 user-defined))
			 'gnus-tmp-header)
		   ?s)))
	   ;; Find the specification from `spec-alist'.
	   ((setq elem (cdr (assq spec spec-alist))))
	   (t
	    (setq elem '("*" ?s))))
	  (setq elem-type (cadr elem))
	  ;; Insert the new format elements.
	  (when pad-width
	    (insert (number-to-string pad-width)))
	  ;; Create the form to be evaled.
	  (if (or max-width cut-width ignore-value)
	      (progn
		(insert ?s)
		(let ((el (car elem)))
		  (cond ((= (cadr elem) ?c)
			 (setq el (list 'char-to-string el)))
			((= (cadr elem) ?d)
			 (setq el (list 'int-to-string el))))
		  (when ignore-value
		    (setq el (gnus-tilde-ignore-form el ignore-value)))
		  (when cut-width
		    (setq el (gnus-tilde-cut-form el cut-width)))
		  (when max-width
		    (setq el (gnus-tilde-max-form el max-width)))
		  (push el flist)))
	    (insert elem-type)
	    (push (car elem) flist))))
      (setq fstring (buffer-string)))

    ;; Do some postprocessing to increase efficiency.
    (setq
     result
     (cond
      ;; Emptyness.
      ((string= fstring "")
       nil)
      ;; Not a format string.
      ((not (string-match "%" fstring))
       (list fstring))
      ;; A format string with just a single string spec.
      ((string= fstring "%s")
       (list (car flist)))
      ;; A single character.
      ((string= fstring "%c")
       (list (car flist)))
      ;; A single number.
      ((string= fstring "%d")
       (setq dontinsert)
       (if insert
	   (list `(princ ,(car flist)))
	 (list `(int-to-string ,(car flist)))))
      ;; Just lots of chars and strings.
      ((string-match "\\`\\(%[cs]\\)+\\'" fstring)
       (nreverse flist))
      ;; A single string spec at the beginning of the spec.
      ((string-match "\\`%[sc][^%]+\\'" fstring)
       (list (car flist) (substring fstring 2)))
      ;; A single string spec in the middle of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\([^%]+\\)\\'" fstring)
       (list (match-string 1 fstring) (car flist) (match-string 2 fstring)))
      ;; A single string spec in the end of the spec.
      ((string-match "\\`\\([^%]+\\)%[sc]\\'" fstring)
       (list (match-string 1 fstring) (car flist)))
      ;; A more complex spec.
      (t
       (list (cons 'format (cons fstring (nreverse flist)))))))

    (if insert
	(when result
	  (if dontinsert
	      result
	    (cons 'insert result)))
      (cond ((stringp result)
	     result)
	    ((consp result)
	     (cons 'concat result))
	    (t "")))))

(defun gnus-eval-format (format &optional alist props)
  "Eval the format variable FORMAT, using ALIST.
If PROPS, insert the result."
  (let ((form (gnus-parse-format format alist props)))
    (if props
	(gnus-add-text-properties (point) (progn (eval form) (point)) props)
      (eval form))))

(defun gnus-compile ()
  "Byte-compile the user-defined format specs."
  (interactive)
  (when gnus-xemacs
    (error "Can't compile specs under XEmacs"))
  (let ((entries gnus-format-specs)
	(byte-compile-warnings '(unresolved callargs redefine))
	entry gnus-tmp-func)
    (save-excursion
      (gnus-message 7 "Compiling format specs...")

      (while entries
	(setq entry (pop entries))
	(if (eq (car entry) 'version)
	    (setq gnus-format-specs (delq entry gnus-format-specs))
	  (when (and (listp (caddr entry))
		     (not (eq 'byte-code (caaddr entry))))
	    (fset 'gnus-tmp-func `(lambda () ,(caddr entry)))
	    (byte-compile 'gnus-tmp-func)
	    (setcar (cddr entry) (gnus-byte-code 'gnus-tmp-func)))))

      (push (cons 'version emacs-version) gnus-format-specs)
      ;; Mark the .newsrc.eld file as "dirty".
      (gnus-dribble-enter " ")
      (gnus-message 7 "Compiling user specs...done"))))

(provide 'gnus-spec)

;;; gnus-spec.el ends here
