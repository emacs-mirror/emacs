;;; apropos.el --- apropos commands for users and programmers.

;; Copyright (C) 1989, 1994, 1995 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bigbird.bu.edu>
;; Rewritten: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: help

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

;; The ideas for this package were derived from the C code in
;; src/keymap.c and elsewhere.  The functions in this file should
;; always be byte-compiled for speed.  Someone should rewrite this in
;; C (as part of src/keymap.c) for speed.

;; The idea for super-apropos is based on the original implementation
;; by Lynn Slater <lrs@esl.com>.

;; History:
;; Fixed bug, current-local-map can return nil.
;; Change, doesn't calculate key-bindings unless needed.
;; Added super-apropos capability, changed print functions.
;;; Made fast-apropos and super-apropos share code.
;;; Sped up fast-apropos again.
;; Added apropos-do-all option.
;;; Added fast-command-apropos.
;; Changed doc strings to comments for helping functions.
;;; Made doc file buffer read-only, buried it.
;; Only call substitute-command-keys if do-all set.

;; Optionally use configurable faces to make the output more legible.
;; Differentiate between command, function and macro.
;; Apropos-command (ex command-apropos) does cmd and optionally user var.
;; Apropos shows all 3 aspects of symbols (fn, var and plist)
;; Apropos-documentation (ex super-apropos) now finds all it should.
;; New apropos-value snoops through all values and optionally plists.
;; Reading DOC file doesn't load nroff.
;; Added hypertext following of documentation, mouse-2 on variable gives value
;;   from buffer in active window.

;;; Code:

(defgroup apropos nil
  "Apropos commands for users and programmers"
  :group 'help
  :prefix "apropos")

;; I see a degradation of maybe 10-20% only.
(defcustom apropos-do-all nil
  "*Whether the apropos commands should do more.

Slows them down more or less.  Set this non-nil if you have a fast machine."
  :group 'apropos
  :type 'boolean)


(defcustom apropos-symbol-face 'bold
  "*Face for symbol name in Apropos output, or nil for none."
  :group 'apropos
  :type 'face)

(defcustom apropos-keybinding-face 'underline
  "*Face for lists of keybinding in Apropos output, or nil for none."
  :group 'apropos
  :type 'face)

(defcustom apropos-label-face 'italic
  "*Face for label (`Command', `Variable' ...) in Apropos output.
A value of nil means don't use any special font for them, and also
turns off mouse highlighting."
  :group 'apropos
  :type 'face)

(defcustom apropos-property-face 'bold-italic
  "*Face for property name in apropos output, or nil for none."
  :group 'apropos
  :type 'face)

(defcustom apropos-match-face 'secondary-selection
  "*Face for matching text in Apropos documentation/value, or nil for none.
This applies when you look for matches in the documentation or variable value
for the regexp; the part that matches gets displayed in this font."
  :group 'apropos
  :type 'face)


(defvar apropos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'apropos-follow)
    (define-key map " "    'scroll-up)
    (define-key map "\177" 'scroll-down)
    (define-key map "q"    'quit-window)
    (define-key map [mouse-2] 'apropos-mouse-follow)
    (define-key map [down-mouse-2] nil)
    map)
  "Keymap used in Apropos mode.")


(defvar apropos-regexp nil
  "Regexp used in current apropos run.")

(defvar apropos-files-scanned ()
  "List of elc files already scanned in current run of `apropos-documentation'.")

(defvar apropos-accumulator ()
  "Alist of symbols already found in current apropos run.")

(defvar apropos-item ()
  "Current item in or for apropos-accumulator.")

;;;###autoload
(defun apropos-mode ()
  "Major mode for following hyperlinks in output of apropos commands.

\\{apropos-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map apropos-mode-map)
  (setq major-mode 'apropos-mode
	mode-name "Apropos"))

;;;###autoload
(defun apropos-variable (regexp &optional do-all)
  "Show user variables that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
normal variables."
  (interactive (list (read-string
                      (concat "Apropos "
                              (if (or current-prefix-arg apropos-do-all)
				  "variable"
				"user option")
                              " (regexp): "))
                     current-prefix-arg))
  (apropos-command regexp nil
		   (if (or do-all apropos-do-all)
		       #'(lambda (symbol)
			   (and (boundp symbol)
				(get symbol 'variable-documentation)))
		     'user-variable-p)))

;; For auld lang syne:
;;;###autoload
(fset 'command-apropos 'apropos-command)
;;;###autoload
(defun apropos-command (apropos-regexp &optional do-all var-predicate)
  "Show commands (interactively callable functions) that match REGEXP.
With optional prefix ARG, or if `apropos-do-all' is non-nil, also show
noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
  (interactive (list (read-string (concat
				   "Apropos command "
				   (if (or current-prefix-arg
					   apropos-do-all)
				       "or function ")
				   "(regexp): "))
		     current-prefix-arg))
  (let ((message
	 (let ((standard-output (get-buffer-create "*Apropos*")))
	   (print-help-return-message 'identity))))
    (or do-all (setq do-all apropos-do-all))
    (setq apropos-accumulator
	  (apropos-internal apropos-regexp
			    (or var-predicate
				(if do-all 'functionp 'commandp))))
    (let ((tem apropos-accumulator))
      (while tem
	(if (get (car tem) 'apropos-inhibit)
	    (setq apropos-accumulator (delq (car tem) apropos-accumulator)))
	(setq tem (cdr tem))))
    (let ((p apropos-accumulator)
	  doc symbol)
      (while p
	(setcar p (list
		   (setq symbol (car p))
		   (unless var-predicate
		     (if (functionp symbol)
			 (if (setq doc (documentation symbol t))
			     (substring doc 0 (string-match "\n" doc))
			   "(not documented)")))
		   (and var-predicate
			(funcall var-predicate symbol)
			(if (setq doc (documentation-property
				       symbol 'variable-documentation t))
			    (substring doc 0
				       (string-match "\n" doc))))))
	(setq p (cdr p))))
    (and (apropos-print t nil)
	 message
	 (message message))))


;;;###autoload
(defun apropos (apropos-regexp &optional do-all)
  "Show all bound symbols whose names match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show unbound
symbols and key bindings, which is a little more time-consuming.
Returns list of symbols and documentation found."
  (interactive "sApropos symbol (regexp): \nP")
  (setq apropos-accumulator
	(apropos-internal apropos-regexp
			  (and (not do-all)
			       (not apropos-do-all)
			       (lambda (symbol)
				 (or (fboundp symbol)
				     (boundp symbol)
				     (facep symbol)
				     (symbol-plist symbol))))))
  (let ((tem apropos-accumulator))
    (while tem
      (if (get (car tem) 'apropos-inhibit)
	  (setq apropos-accumulator (delq (car tem) apropos-accumulator)))
      (setq tem (cdr tem))))
  (let ((p apropos-accumulator)
	symbol doc properties)
    (while p
      (setcar p (list
		 (setq symbol (car p))
		 (when (fboundp symbol)
		   (if (setq doc (condition-case nil
				     (documentation symbol t)
				   (void-function
				    "(alias for undefined function)")))
		       (substring doc 0 (string-match "\n" doc))
		     "(not documented)"))
		 (when (boundp symbol)
		   (if (setq doc (documentation-property
				  symbol 'variable-documentation t))
		       (substring doc 0 (string-match "\n" doc))
		     "(not documented)"))
		 (when (setq properties (symbol-plist symbol))
		   (setq doc (list (car properties)))
		   (while (setq properties (cdr (cdr properties)))
		     (setq doc (cons (car properties) doc)))
		   (mapconcat #'symbol-name (nreverse doc) " "))
		 (when (get symbol 'widget-type)
		   (if (setq doc (documentation-property
				  symbol 'widget-documentation t))
		       (substring doc 0
				  (string-match "\n" doc))
		     "(not documented)"))
		 (when (facep symbol)
		   (if (setq doc (documentation-property
				  symbol 'face-documentation t))
		       (substring doc 0
				  (string-match "\n" doc))
		     "(not documented)"))
		 (when (get symbol 'custom-group)
		   (if (setq doc (documentation-property
				  symbol 'group-documentation t))
		       (substring doc 0
				  (string-match "\n" doc))
		     "(not documented)"))))
      (setq p (cdr p))))
  (apropos-print
   (or do-all apropos-do-all)
   nil))


;;;###autoload
(defun apropos-value (apropos-regexp &optional do-all)
  "Show all symbols whose value's printed image matches REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also looks
at the function and at the names and values of properties.
Returns list of symbols and values found."
  (interactive "sApropos value (regexp): \nP")
  (or do-all (setq do-all apropos-do-all))
  (setq apropos-accumulator ())
   (let (f v p)
     (mapatoms
      (lambda (symbol)
	(setq f nil v nil p nil)
	(or (memq symbol '(apropos-regexp do-all apropos-accumulator
					  symbol f v p))
	    (setq v (apropos-value-internal 'boundp symbol 'symbol-value)))
	(if do-all
	    (setq f (apropos-value-internal 'fboundp symbol 'symbol-function)
		  p (apropos-format-plist symbol "\n    " t)))
	(if (or f v p)
	    (setq apropos-accumulator (cons (list symbol f v p)
					    apropos-accumulator))))))
  (apropos-print nil t))


;;;###autoload
(defun apropos-documentation (apropos-regexp &optional do-all)
  "Show symbols whose documentation contain matches for REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also use
documentation that is not stored in the documentation file and show key
bindings.
Returns list of symbols and documentation found."
  (interactive "sApropos documentation (regexp): \nP")
  (or do-all (setq do-all apropos-do-all))
  (setq apropos-accumulator () apropos-files-scanned ())
  (let ((standard-input (get-buffer-create " apropos-temp"))
	f v)
    (unwind-protect
	(save-excursion
	  (set-buffer standard-input)
	  (apropos-documentation-check-doc-file)
	  (if do-all
	      (mapatoms
	       (lambda (symbol)
		 (setq f (apropos-safe-documentation symbol)
		       v (get symbol 'variable-documentation))
		 (if (integerp v) (setq v))
		 (setq f (apropos-documentation-internal f)
		       v (apropos-documentation-internal v))
		 (if (or f v)
		     (if (setq apropos-item
			       (cdr (assq symbol apropos-accumulator)))
			 (progn
			   (if f
			       (setcar apropos-item f))
			   (if v
			       (setcar (cdr apropos-item) v)))
		       (setq apropos-accumulator
			     (cons (list symbol f v)
				   apropos-accumulator)))))))
	  (apropos-print nil t))
      (kill-buffer standard-input))))


(defun apropos-value-internal (predicate symbol function)
  (if (funcall predicate symbol)
      (progn
	(setq symbol (prin1-to-string (funcall function symbol)))
	(if (string-match apropos-regexp symbol)
	    (progn
	      (if apropos-match-face
		  (put-text-property (match-beginning 0) (match-end 0)
				     'face apropos-match-face
				     symbol))
	      symbol)))))

(defun apropos-documentation-internal (doc)
  (if (consp doc)
      (apropos-documentation-check-elc-file (car doc))
    (and doc
	 (string-match apropos-regexp doc)
	 (progn
	   (if apropos-match-face
	       (put-text-property (match-beginning 0)
				  (match-end 0)
				  'face apropos-match-face
				  (setq doc (copy-sequence doc))))
	   doc))))

(defun apropos-format-plist (pl sep &optional compare)
  (setq pl (symbol-plist pl))
  (let (p p-out)
    (while pl
      (setq p (format "%s %S" (car pl) (nth 1 pl)))
      (if (or (not compare) (string-match apropos-regexp p))
	  (if apropos-property-face
	      (put-text-property 0 (length (symbol-name (car pl)))
				 'face apropos-property-face p))
	(setq p nil))
      (if p
	  (progn
	    (and compare apropos-match-face
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face apropos-match-face
				    p))
	    (setq p-out (concat p-out (if p-out sep) p))))
      (setq pl (nthcdr 2 pl)))
    p-out))


;; Finds all documentation related to APROPOS-REGEXP in internal-doc-file-name.

(defun apropos-documentation-check-doc-file ()
  (let (type symbol (sepa 2) sepb beg end)
    (insert ?\^_)
    (backward-char)
    (insert-file-contents (concat doc-directory internal-doc-file-name))
    (forward-char)
    (while (save-excursion
	     (setq sepb (search-forward "\^_"))
	     (not (eobp)))
      (beginning-of-line 2)
      (if (save-restriction
	    (narrow-to-region (point) (1- sepb))
	    (re-search-forward apropos-regexp nil t))
	  (progn
	    (setq beg (match-beginning 0)
		  end (point))
	    (goto-char (1+ sepa))
	    (or (setq type (if (eq ?F (preceding-char))
			       1	; function documentation
			     2)		; variable documentation
		      symbol (read)
		      beg (- beg (point) 1)
		      end (- end (point) 1)
		      doc (buffer-substring (1+ (point)) (1- sepb))
		      apropos-item (assq symbol apropos-accumulator))
		(setq apropos-item (list symbol nil nil)
		      apropos-accumulator (cons apropos-item
						apropos-accumulator)))
	    (if apropos-match-face
		(put-text-property beg end 'face apropos-match-face doc))
	    (setcar (nthcdr type apropos-item) doc)))
      (setq sepa (goto-char sepb)))))

(defun apropos-documentation-check-elc-file (file)
  (if (member file apropos-files-scanned)
      nil
    (let (symbol doc beg end this-is-a-variable)
      (setq apropos-files-scanned (cons file apropos-files-scanned))
      (erase-buffer)
      (insert-file-contents file)
      (while (search-forward "\n#@" nil t)
	;; Read the comment length, and advance over it.
	(setq end (read)
	      beg (1+ (point))
	      end (+ (point) end -1))
	(forward-char)
	(if (save-restriction
	      ;; match ^ and $ relative to doc string
	      (narrow-to-region beg end)
	      (re-search-forward apropos-regexp nil t))
	    (progn
	      (goto-char (+ end 2))
	      (setq doc (buffer-substring beg end)
		    end (- (match-end 0) beg)
		    beg (- (match-beginning 0) beg)
		    this-is-a-variable (looking-at "(def\\(var\\|const\\) ")
		    symbol (progn
			     (skip-chars-forward "(a-z")
			     (forward-char)
			     (read))
		    symbol (if (consp symbol)
			       (nth 1 symbol)
			     symbol))
	      (if (if this-is-a-variable
		      (get symbol 'variable-documentation)
		    (and (fboundp symbol) (apropos-safe-documentation symbol)))
		  (progn
		    (or (setq apropos-item (assq symbol apropos-accumulator))
			(setq apropos-item (list symbol nil nil)
			      apropos-accumulator (cons apropos-item
							apropos-accumulator)))
		    (if apropos-match-face
			(put-text-property beg end 'face apropos-match-face
					   doc))
		    (setcar (nthcdr (if this-is-a-variable 2 1)
				    apropos-item)
			    doc)))))))))



(defun apropos-safe-documentation (function)
  "Like documentation, except it avoids calling `get_doc_string'.
Will return nil instead."
  (while (and function (symbolp function))
    (setq function (if (fboundp function)
		       (symbol-function function))))
  (if (eq (car-safe function) 'macro)
      (setq function (cdr function)))
  (setq function (if (byte-code-function-p function)
		     (if (> (length function) 4)
			 (aref function 4))
		   (if (eq (car-safe function) 'autoload)
		       (nth 2 function)
		     (if (eq (car-safe function) 'lambda)
			 (if (stringp (nth 2 function))
			     (nth 2 function)
			   (if (stringp (nth 3 function))
			       (nth 3 function)))))))
  (if (integerp function)
      nil
    function))



(defvar apropos-label-properties nil
  "List of face properties to use for a label.
Bound by `apropos-print' for use by `apropos-print-doc'.")

(defun apropos-print (do-keys spacing)
  "Output result of apropos searching into buffer `*Apropos*'.
The value of `apropos-accumulator' is the list of items to output.
Each element should have the format (SYMBOL FN-DOC VAR-DOC [PLIST-DOC]).
The return value is the list that was in `apropos-accumulator', sorted
alphabetically by symbol name; but this function also sets
`apropos-accumulator' to nil before returning."
  (if (null apropos-accumulator)
      (message "No apropos matches for `%s'" apropos-regexp)
    (setq apropos-accumulator
	  (sort apropos-accumulator (lambda (a b)
				      (string-lessp (car a) (car b)))))
    (setq apropos-label-properties
	  (if (and apropos-label-face
		   (symbolp apropos-label-face))
	      `(face ,apropos-label-face
		     mouse-face highlight)))
    (with-output-to-temp-buffer "*Apropos*"
      (let ((p apropos-accumulator)
	    (old-buffer (current-buffer))
	    symbol item point1 point2)
	(set-buffer standard-output)
	(apropos-mode)
	(if window-system
	    (insert "If you move the mouse over text that changes color,\n"
		    (substitute-command-keys
		     "you can click \\[apropos-mouse-follow] to get more information.\n")))
	(insert (substitute-command-keys
		 "In this buffer, type \\[apropos-follow] to get full documentation.\n\n"))
	(while (consp p)
	  (or (not spacing) (bobp) (terpri))
	  (setq apropos-item (car p)
		symbol (car apropos-item)
		p (cdr p)
		point1 (point))
	  (princ symbol)		        ; print symbol name
	  (setq point2 (point))
	  ;; Calculate key-bindings if we want them.
	  (and do-keys
	       (commandp symbol)
	       (indent-to 30 1)
	       (if (let ((keys
			  (save-excursion
			    (set-buffer old-buffer)
			    (where-is-internal symbol)))
			 filtered)
		     ;; Copy over the list of key sequences,
		     ;; omitting any that contain a buffer or a frame.
		     (while keys
		       (let ((key (car keys))
			     (i 0)
			     loser)
			 (while (< i (length key))
			   (if (or (framep (aref key i))
				   (bufferp (aref key i)))
			       (setq loser t))
			   (setq i (1+ i)))
			 (or loser
			     (setq filtered (cons key filtered))))
		       (setq keys (cdr keys)))
		     (setq item filtered))
		   ;; Convert the remaining keys to a string and insert.
		   (insert
		    (mapconcat
		     (lambda (key)
		       (setq key (condition-case () 
				     (key-description key)
				   (error)))
		       (if apropos-keybinding-face
			   (put-text-property 0 (length key)
					      'face apropos-keybinding-face
					      key))
		       key)
		     item ", "))
		 (insert "M-x")
		 (put-text-property (- (point) 3) (point)
				    'face apropos-keybinding-face)
		 (insert " " (symbol-name symbol) " ")
		 (insert "RET")
		 (put-text-property (- (point) 3) (point)
				    'face apropos-keybinding-face)))
	  (terpri)
	  ;; only now so we don't propagate text attributes all over
	  (put-text-property point1 point2 'item
			     (if (eval `(or ,@(cdr apropos-item)))
				 (car apropos-item)
			       apropos-item))
	  (if apropos-symbol-face
	      (put-text-property point1 point2 'face apropos-symbol-face))
	  (apropos-print-doc 'describe-function 1
			     (if (commandp symbol)
				 "Command"
			       (if (apropos-macrop symbol)
				   "Macro"
				 "Function"))
			     t)
	  ;; We used to use customize-variable-other-window instead
	  ;; for a customizable variable, but that is slow.
	  ;; It is better to show an ordinary help buffer
	  ;; and let the user click on the customization button
	  ;; in that buffer, if he wants to.
	  (apropos-print-doc 'describe-variable 2 "Variable" t)
	  (apropos-print-doc 'customize-group-other-window 6 "Group" t)
	  (apropos-print-doc 'customize-face-other-window 5 "Face" t)
	  (apropos-print-doc 'widget-browse-other-window 4 "Widget" t)
	  (apropos-print-doc 'apropos-describe-plist 3
			     "Plist" nil))
	(setq buffer-read-only t))))
  (prog1 apropos-accumulator
    (setq apropos-accumulator ())))	; permit gc


(defun apropos-macrop (symbol)
  "T if SYMBOL is a Lisp macro."
  (and (fboundp symbol)
       (consp (setq symbol
		    (symbol-function symbol)))
       (or (eq (car symbol) 'macro)
	   (if (eq (car symbol) 'autoload)
	       (memq (nth 4 symbol)
		     '(macro t))))))


(defun apropos-print-doc (action i str do-keys)
  (if (stringp (setq i (nth i apropos-item)))
      (progn
	(insert "  ")
	(put-text-property (- (point) 2) (1- (point))
			   'action action)
	(insert str ": ")
	(if apropos-label-properties
	    (add-text-properties (- (point) (length str) 2)
				 (1- (point))
				 apropos-label-properties))
	(insert (if do-keys (substitute-command-keys i) i))
	(or (bolp) (terpri)))))


(defun apropos-mouse-follow (event)
  (interactive "e")
  (let ((other (if (eq (current-buffer) (get-buffer "*Apropos*"))
		   ()
		 (current-buffer))))
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (goto-char (posn-point (event-start event)))
      (or (and (not (eobp)) (get-text-property (point) 'mouse-face))
	  (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	  (error "There is nothing to follow here"))
      (apropos-follow other))))


(defun apropos-follow (&optional other)
  (interactive)
  (let* (;; Properties are always found at the beginning of the line.
	 (bol (save-excursion (beginning-of-line) (point)))
	 ;; If there is no `item' property here, look behind us.
	 (item (get-text-property bol 'item))
	 (item-at (if item nil (previous-single-property-change bol 'item)))
	 ;; Likewise, if there is no `action' property here, look in front.
	 (action (get-text-property bol 'action))
	 (action-at (if action nil (next-single-property-change bol 'action))))
    (and (null item) item-at
	 (setq item (get-text-property (1- item-at) 'item)))
    (and (null action) action-at
	 (setq action (get-text-property action-at 'action)))
    (if (not (and item action))
	(error "There is nothing to follow here"))
    (if (consp item) (error "There is nothing to follow in `%s'" (car item)))
    (if other (set-buffer other))
    (funcall action item)))



(defun apropos-describe-plist (symbol)
  "Display a pretty listing of SYMBOL's plist."
  (with-output-to-temp-buffer "*Help*"
    (set-buffer standard-output)
    (princ "Symbol ")
    (prin1 symbol)
    (princ "'s plist is\n (")
    (if apropos-symbol-face
	(put-text-property 8 (- (point) 14) 'face apropos-symbol-face))
    (insert (apropos-format-plist symbol "\n  "))
    (princ ")")
    (print-help-return-message)))

(provide 'apropos)

;;; apropos.el ends here
