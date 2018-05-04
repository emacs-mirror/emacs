;;; gnus-spec.el --- format spec functions for Gnus

;; Copyright (C) 1996-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))
(defvar gnus-newsrc-file-version)

(require 'gnus)

;;; Internal variables.

(defvar gnus-summary-mark-positions nil)
(defvar gnus-group-mark-positions nil)
(defvar gnus-group-indentation "")

;; Format specs.  The chunks below are the machine-generated forms
;; that are to be evalled as the result of the default format strings.
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
(defvar gnus-mouse-face)
(defvar gnus-tmp-header)
(defvar gnus-tmp-from)

(declare-function gnus-summary-from-or-to-or-newsgroups "gnus-sum"
                  (header gnus-tmp-from))

(defmacro gnus-lrm-string-p (string)
  ;; LRM, RLM, PDF characters as integers to avoid breaking Emacs
  ;; 23.
  `(memq (aref ,string (1- (length ,string))) '(8206 8207 8236)))

(defvar gnus-lrm-string (if (ignore-errors (string 8206))
			    (propertize (string 8206) 'invisible t)
			  ""))

(defvar gnus-summary-line-format-spec nil)
(defvar gnus-summary-dummy-line-format-spec nil)
(defvar gnus-group-line-format-spec nil)

(defvar gnus-format-specs
  `((version . ,emacs-version)
    (gnus-version . ,(gnus-continuum-version)))
  "Alist of format specs.")

(defvar gnus-default-format-specs gnus-format-specs)

(defvar gnus-article-mode-line-format-spec nil)
(defvar gnus-summary-mode-line-format-spec nil)
(defvar gnus-group-mode-line-format-spec nil)

;;; Phew.  All that gruft is over with, fortunately.

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
    (insert (gnus-pp-to-string spec))))

(defun gnus-update-format-specifications (&optional force &rest types)
  "Update all (necessary) format specifications.
Return a list of updated types."
  ;; Make the indentation array.
  ;; See whether all the stored info needs to be flushed.
  (when (or force
	    (not gnus-newsrc-file-version)
	    (not (equal (gnus-continuum-version)
			(gnus-continuum-version gnus-newsrc-file-version)))
	    (not (equal emacs-version
			(cdr (assq 'version gnus-format-specs)))))
    (setq gnus-format-specs nil))
  ;; Go through all the formats and see whether they need updating.
  (let (new-format entry type val updated)
    (while (setq type (pop types))
      ;; Jump to the proper buffer to find out the value of the
      ;; variable, if possible.  (It may be buffer-local.)
      (save-excursion
	(let ((buffer (intern (format "gnus-%s-buffer" type))))
	  (when (and (boundp buffer)
		     (setq val (symbol-value buffer))
		     (gnus-buffer-exists-p val))
	    (set-buffer val))
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
		    (intern (format "gnus-%s-line-format-alist" type)))
		   (not (string-match "mode$" (symbol-name type))))))
	  ;; Enter the new format spec into the list.
	  (if entry
	      (progn
		(setcar (cdr entry) val)
		(setcar entry new-format))
	    (push (list type new-format val) gnus-format-specs))
	  (set (intern (format "gnus-%s-line-format-spec" type)) val)
	  (push type updated))))

    (unless (assq 'version gnus-format-specs)
      (push (cons 'version emacs-version) gnus-format-specs))
    updated))

(defcustom gnus-mouse-face-0 'highlight
  "The \"%(hello%)\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-mouse-face-1 'highlight
  "The \"%1(hello%)\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-mouse-face-2 'highlight
  "The \"%2(hello%)\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-mouse-face-3 'highlight
  "The \"%3(hello%)\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-mouse-face-4 'highlight
  "The \"%4(hello%)\" face."
  :group 'gnus-format
  :type 'face)

(defun gnus-mouse-face-function (form type)
  `(put-text-property
    (point) (progn ,@form (point))
    'mouse-face
    ,(if (equal type 0)
	 'gnus-mouse-face
       `(quote ,(symbol-value (intern (format "gnus-mouse-face-%d" type)))))))

(defcustom gnus-face-0 'bold
  "The \"%{hello%}\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-face-1 'italic
  "The \"%1{hello%}\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-face-2 'bold-italic
  "The \"%2{hello%}\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-face-3 'bold
  "The \"%3{hello%}\" face."
  :group 'gnus-format
  :type 'face)

(defcustom gnus-face-4 'bold
  "The \"%4{hello%}\" face."
  :group 'gnus-format
  :type 'face)

(defun gnus-face-face-function (form type)
  `(add-text-properties
    (point) (progn ,@form (point))
    (cons 'face
	  (cons
	   ;; Delay consing the value of the `face' property until
	   ;; `add-text-properties' runs, since it will be modified
	   ;; by `put-text-property-excluding-characters-with-faces'.
	   (list ',(symbol-value (intern (format "gnus-face-%d" type))) 'default)
	   ;; Redundant now, but still convenient.
	   '(gnus-face t)))))

(defun gnus-balloon-face-function (form type)
  `(put-text-property
    (point) (progn ,@form (point)) 'help-echo
    ,(intern (format "gnus-balloon-face-%d" type))))

(defun gnus-spec-tab (column)
  (if (> column 0)
      `(insert-char ?  (max (- ,column (current-column)) 0))
    (let ((column (abs column)))
      `(if (> (current-column) ,column)
	   (let ((end (point)))
	     (if (= (move-to-column ,column) ,column)
		 (delete-region (point) end)
	       (delete-region (1- (point)) end)
	       (insert " ")))
	 (insert-char ?  (max (- ,column (current-column)) 0))))))

(defun gnus-correct-length (string)
  "Return the correct width of STRING."
  (apply #'+ (mapcar #'char-width string)))

(defun gnus-correct-substring (string start &optional end)
  (let ((wstart 0)
	(wend 0)
	(wseek 0)
	(seek 0)
	(length (length string))
	(string (concat string "\0")))
    ;; Find the start position.
    (while (and (< seek length)
		(< wseek start))
      (cl-incf wseek (char-width (aref string seek)))
      (cl-incf seek))
    (setq wstart seek)
    ;; Find the end position.
    (while (and (<= seek length)
		(or (not end)
		    (<= wseek end)))
      (cl-incf wseek (char-width (aref string seek)))
      (cl-incf seek))
    (setq wend seek)
    (substring string wstart (1- wend))))

(defun gnus-tilde-max-form (el max-width)
  "Return a form that limits EL to MAX-WIDTH."
  (let ((max (abs max-width)))
    (if (symbolp el)
	`(if (> (string-width ,el) ,max)
	     ,(if (< max-width 0)
		  `(gnus-correct-substring ,el (- (string-width ,el) ,max))
		`(if (gnus-lrm-string-p ,el)
		     (concat (gnus-correct-substring ,el 0 ,max)
			     ,gnus-lrm-string)
		   (gnus-correct-substring ,el 0 ,max)))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (string-width val) ,max)
	     ,(if (< max-width 0)
		  `(gnus-correct-substring val (- (string-width val) ,max))
		`(if (gnus-lrm-string-p val)
		     (concat (gnus-correct-substring val 0 ,max)
			     ,gnus-lrm-string)
		   (gnus-correct-substring val 0 ,max)))
	   val)))))

(defun gnus-tilde-cut-form (el cut-width)
  "Return a form that cuts CUT-WIDTH off of EL."
  (let ((cut (abs cut-width)))
    (if (symbolp el)
	`(if (> (string-width ,el) ,cut)
	     ,(if (< cut-width 0)
		  `(gnus-correct-substring ,el 0 (- (string-width ,el) ,cut))
		`(gnus-correct-substring ,el ,cut))
	   ,el)
      `(let ((val (eval ,el)))
	 (if (> (string-width val) ,cut)
	     ,(if (< cut-width 0)
		  `(gnus-correct-substring val 0 (- (string-width val) ,cut))
		`(gnus-correct-substring val ,cut))
	   val)))))

(defun gnus-tilde-ignore-form (el ignore-value)
  "Return a form that is blank when EL is IGNORE-VALUE."
  (if (symbolp el)
      `(if (equal ,el ,ignore-value)
	   "" ,el)
    `(let ((val (eval ,el)))
       (if (equal val ,ignore-value)
	   "" val))))

(defun gnus-pad-form (el pad-width)
  "Return a form that pads EL to PAD-WIDTH accounting for multi-column
characters correctly. This is because `format' may pad to columns or to
characters when given a pad value."
  (let ((pad (abs pad-width))
	(side (< 0 pad-width)))
    (if (symbolp el)
	`(let ((need (- ,pad (string-width ,el))))
	   (if (> need 0)
	       (concat ,(when side '(make-string need ?\ ))
		       ,el
		       ,(when (not side) '(make-string need ?\ )))
	     ,el))
      `(let* ((val (eval ,el))
	      (need (- ,pad (string-width val))))
	 (if (> need 0)
	     (concat ,(when side '(make-string need ?\ ))
		     val
		     ,(when (not side) '(make-string need ?\ )))
	   val)))))

(defun gnus-parse-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return the
  ;; string.  If the FORMAT string contains the specifiers %( and %)
  ;; the text between them will have the mouse-face text property.
  ;; If the FORMAT string contains the specifiers %[ and %], the text between
  ;; them will have the balloon-help text property.
  (let ((case-fold-search nil))
    (if (string-match
	 "\\`\\(.*\\)%[0-9]?[{(«]\\(.*\\)%[0-9]?[»})]\\(.*\n?\\)\\'\\|%[-0-9]*=\\|%[-0-9]*\\*"
	 format)
	(gnus-parse-complex-format format spec-alist)
      ;; This is a simple format.
      (gnus-parse-simple-format format spec-alist insert))))

(defun gnus-parse-complex-format (format spec-alist)
  (let ((cursor-spec nil))
    (save-excursion
      (gnus-set-work-buffer)
      (insert format)
      (goto-char (point-min))
      (while (re-search-forward "\"" nil t)
	(replace-match "\\\"" nil t))
      (goto-char (point-min))
      (insert "(\"")
      ;; Convert all font specs into font spec lists.
      (while (re-search-forward "%\\([0-9]+\\)?\\([«»{}()]\\)" nil t)
	(let ((number (if (match-beginning 1)
			  (match-string 1) "0"))
	      (delim (aref (match-string 2) 0)))
	  (if (or (= delim ?\()
		  (= delim ?\{)
		  (= delim 171)) ; «
	      (replace-match (concat "\"("
				     (cond ((= delim ?\() "mouse")
					   ((= delim ?\{) "face")
					   (t "balloon"))
				     " " number " \"")
			     t t)
	    (replace-match "\")\""))))
      (goto-char (point-max))
      (insert "\")")
      ;; Convert point position commands.
      (goto-char (point-min))
      (let ((case-fold-search nil))
	(while (re-search-forward "%\\([-0-9]+\\)?\\*" nil t)
	  (replace-match "\"(point)\"" t t)
	  (setq cursor-spec t)))
      ;; Convert TAB commands.
      (goto-char (point-min))
      (while (re-search-forward "%\\([-0-9]+\\)=" nil t)
	(replace-match (format "\"(tab %s)\"" (match-string 1)) t t))
      ;; Convert the buffer into the spec.
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
	(if cursor-spec
	    `(let (gnus-position)
	       ,@(gnus-complex-form-to-spec form spec-alist)
	       (if gnus-position
		   (put-text-property gnus-position (1+ gnus-position)
					   'gnus-position t)))
	  `(progn
	     ,@(gnus-complex-form-to-spec form spec-alist)))))))

(defun gnus-complex-form-to-spec (form spec-alist)
  (delq nil
	(mapcar
	 (lambda (sform)
	   (cond
	    ((stringp sform)
	     (gnus-parse-simple-format sform spec-alist t))
	    ((eq (car sform) 'point)
	     '(setq gnus-position (point)))
	    ((eq (car sform) 'tab)
	     (gnus-spec-tab (cadr sform)))
	    (t
	     (funcall (intern (format "gnus-%s-face-function" (car sform)))
		      (gnus-complex-form-to-spec (cddr sform) spec-alist)
		      (nth 1 sform)))))
	 form)))

(defun gnus-parse-simple-format (format spec-alist &optional insert)
  ;; This function parses the FORMAT string with the help of the
  ;; SPEC-ALIST and returns a list that can be eval'ed to return a
  ;; string.
  (let ((max-width 0)
	spec flist fstring elem result dontinsert user-defined
	type value pad-width spec-beg cut-width ignore-value
	tilde-form tilde elem-type extended-spec)
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
	      tilde-form nil
	      extended-spec nil)
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
	(cond
	 ;; User-defined spec -- find the spec name.
	 ((eq (setq spec (char-after)) ?u)
	  (forward-char 1)
	  (when (and (eq (setq user-defined (char-after)) ?&)
		     (looking-at "&\\([^;]+\\);"))
	    (setq user-defined (match-string 1))
	    (goto-char (match-end 1))))
	 ;; extended spec
	 ((and (eq spec ?&) (looking-at "&\\([^;]+\\);"))
	  (setq extended-spec (intern (match-string 1)))
	  (goto-char (match-end 1))))
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
		   (list (intern (format
				  (if (stringp user-defined)
				      "gnus-user-format-function-%s"
				    "gnus-user-format-function-%c")
				  user-defined))
			 'gnus-tmp-header)
		   ?s)))
	   ;; Find the specification from `spec-alist'.
	   ((setq elem (cdr (assq (or extended-spec spec) spec-alist))))
	   ;; We used to use "%l" for displaying the grouplens score.
	   ((eq spec ?l)
	    (setq elem '("" ?s)))
	   (t
	    (setq elem '("*" ?s))))
	  (setq elem-type (cadr elem))
	  ;; Insert the new format elements.
	  (when pad-width
	    (insert (number-to-string pad-width)))
	  ;; Create the form to be evalled.
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
		  (when pad-width
		    (setq el (gnus-pad-form el pad-width)))
		  (push el flist)))
	    (insert elem-type)
	    (push (car elem) flist))))
      (setq fstring (buffer-substring-no-properties (point-min) (point-max))))

    ;; Do some postprocessing to increase efficiency.
    (setq
     result
     (cond
      ;; Emptiness.
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
       (setq dontinsert t)
       (if insert
	   `(insert (int-to-string ,(car flist)))
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
	(add-text-properties (point) (progn (eval form) (point)) props)
      (eval form))))

(defun gnus-set-format (type &optional insertable)
  (set (intern (format "gnus-%s-line-format-spec" type))
       (gnus-parse-format
	(symbol-value (intern (format "gnus-%s-line-format" type)))
	(symbol-value (intern (format "gnus-%s-line-format-alist" type)))
	insertable)))


    (defun gnus-summary-line-format-spec ()
      (insert gnus-tmp-unread gnus-tmp-replied
	      gnus-tmp-score-char gnus-tmp-indentation)
      (put-text-property
       (point)
       (progn
	 (insert
	  gnus-tmp-opening-bracket
	  (format "%4d: %-20s"
		  gnus-tmp-lines
		  (if (> (length gnus-tmp-name) 20)
		      (truncate-string-to-width gnus-tmp-name 20)
		    gnus-tmp-name))
	  gnus-tmp-closing-bracket)
	 (point))
       'mouse-face gnus-mouse-face)
      (insert " " gnus-tmp-subject-or-nil "\n"))

(provide 'gnus-spec)

;; Local Variables:
;; coding: utf-8
;; End:

;;; gnus-spec.el ends here
