;;; apropos.el --- apropos commands for users and programmers.

;; Copyright (C) 1989, 1994, 1995 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bigbird.bu.edu>
;; Rewritten: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

;; I see a degradation of maybe 10-20% only.
(defvar apropos-do-all nil
  "*Whether the apropos commands should do more.
Slows them down more or less.  Set this non-nil if you have a fast machine.")


(defvar apropos-symbol-face (if window-system 'bold)
  "*Face for symbol name in apropos output or `nil'.  
This looks good, but slows down the commands several times.")

(defvar apropos-keybinding-face (if window-system 'underline)
  "*Face for keybinding display in apropos output or `nil'.  
This looks good, but slows down the commands several times.")

(defvar apropos-label-face (if window-system 'italic)
  "*Face for label (Command, Variable ...) in apropos output or `nil'.
If this is `nil' no mouse highlighting occurs.
This looks good, but slows down the commands several times.
When this is a face name, as it is initially, it gets transformed to a
text-property list for efficiency.")

(defvar apropos-property-face (if window-system 'bold-italic)
  "*Face for property name in apropos output or `nil'.  
This looks good, but slows down the commands several times.")

(defvar apropos-match-face (if window-system 'highlight)
  "*Face for matching part in apropos-documentation/value output or `nil'.  
This looks good, but slows down the commands several times.")


(defvar apropos-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'apropos-follow)
    (define-key map [mouse-2] 'apropos-mouse-follow)
    (define-key map [down-mouse-2] nil)
    map)
  "Local map active when displaying apropos output.")


(defvar apropos-regexp nil
  "Regexp used in current apropos run.")

(defvar apropos-files-scanned ()
  "List of elc files already scanned in current run of `apropos-documentaion'.")

(defvar apropos-accumulator ()
  "Alist of symbols already found in current apropos run.")

(defvar apropos-item ()
  "Current item in or for apropos-accumulator.")

;; For auld lang syne:
;;;###autoload
(fset 'command-apropos 'apropos-command)
;;;###autoload
(defun apropos-command (apropos-regexp &optional do-all)
  "Shows commands (interactively callable functions) that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
variables."
  (interactive (list (read-string (concat "Apropos command "
					  (if (or current-prefix-arg
						  apropos-do-all)
					      "or variable ")
					  "(regexp): "))
		     current-prefix-arg))
  (let ((message
	 (let ((standard-output (get-buffer-create "*Help*")))
	   (print-help-return-message 'identity))))
    (or do-all (setq do-all apropos-do-all))
    (setq apropos-accumulator
	  (apropos-internal apropos-regexp
			    (if do-all
				(lambda (symbol) (or (commandp symbol)
						     (user-variable-p symbol)))
			      'commandp)))
    (if (apropos-print
	 t
	 (lambda (p)
	   (let (doc symbol)
	     (while p
	       (setcar p (list
			  (setq symbol (car p))
			  (if (commandp symbol)
			      (if (setq doc (documentation symbol t))
				  (substring doc 0 (string-match "\n" doc))
				"(not documented)"))
			  (and do-all
			       (user-variable-p symbol)
			       (if (setq doc (documentation-property
					      symbol 'variable-documentation t))
				   (substring doc 0
					      (string-match "\n" doc))))))
	       (setq p (cdr p)))))
	 nil)
	(and message (message message)))))


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
				     (symbol-plist symbol))))))
  (apropos-print
   (or do-all apropos-do-all)
   (lambda (p)
     (let (symbol doc)
       (while p
	 (setcar p (list
		    (setq symbol (car p))
		    (if (fboundp symbol)
			(if (setq doc (documentation symbol t))
			    (substring doc 0 (string-match "\n" doc))
			  "(not documented)"))
		    (if (boundp symbol)
			(if (setq doc (documentation-property
				       symbol 'variable-documentation t))
			    (substring doc 0
				       (string-match "\n" doc))
			  "(not documented)"))
		    (if (setq doc (symbol-plist symbol))
			(if (eq (setq doc (/ (length doc) 2)) 1)
			    "1 property"
			  (concat doc " properties")))))
	 (setq p (cdr p)))))
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
  (apropos-print nil nil t))


;;;###autoload
(defun apropos-documentation (apropos-regexp &optional do-all)
  "Show symbols whose names or documentation contain matches for REGEXP.
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
		       v (get symbol 'variable-documentation)
		       v (if (integerp v) nil v))
		 (or (string-match apropos-regexp (symbol-name symbol))
		     (setq f (apropos-documentation-internal f)
			   v (apropos-documentation-internal v)))
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
	  (apropos-print do-all nil t))
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
  (let (type symbol beg end)
    (insert-file-contents (concat doc-directory internal-doc-file-name))
    (while (re-search-forward apropos-regexp nil t)
      (setq beg (match-beginning 0)
	    end (point))
      (search-backward "\C-_")
      (if (> (point) beg)
	  ()
	(or (setq type (if (eq ?F (char-after (1+ (point))))
			   1		;function documentation
			 2)		;variable documentation
		  symbol (prog2
			     (forward-char 2)
			     (read))
		  beg (- beg (point) 1)
		  end (- end (point) 1)
		  doc (buffer-substring
		       (1+ (point))
		       (if (search-forward "\C-_" nil 'move)
			   (1- (point))
			 (point)))
		  apropos-item (assq symbol apropos-accumulator))
	    (setq apropos-item (list symbol nil nil)
		  apropos-accumulator (cons apropos-item apropos-accumulator)))
	(and apropos-match-face
	     (>= beg 0)
	     (put-text-property beg end 'face apropos-match-face doc))
	(setcar (nthcdr type apropos-item) doc)))))

(defun apropos-documentation-check-elc-file (file)
  (if (member file apropos-files-scanned)
      nil
    (let (symbol doc beg end end1 this-is-a-variable)
      (setq apropos-files-scanned (cons file apropos-files-scanned))
      (erase-buffer)
      (insert-file-contents file)
      (while (search-forward "\n#@" nil t)
	;; Read the comment length, and advance over it.
	(setq end (read)
	      beg (point)
	      end (+ (point) end 1))
	(if (re-search-forward apropos-regexp end t)
	    (progn
	      (goto-char end)
	      (setq doc (buffer-substring (1+ beg) (- end 2))
		    end1 (- (match-end 0) beg 1)
		    beg (- (match-beginning 0) beg 1)
		    this-is-a-variable (looking-at "(defvar\\|(defconst")
		    symbol (progn
			     (skip-chars-forward "(a-z")
			     (forward-char 1)
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
			(put-text-property beg end1 'face apropos-match-face
					   doc))
		    (setcar (nthcdr (if this-is-a-variable 2 1)
				    apropos-item)
			    doc)))))
	(goto-char end)))))



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



(defun apropos-print (do-keys doc-fn spacing)
  "Output result of various apropos commands with `apropos-regexp'.
APROPOS-ACCUMULATOR is a list.  Optional DOC-FN is called for each element
of apropos-accumulator and may modify it resulting in (symbol fn-doc
var-doc [plist-doc]).  Returns sorted list of symbols and documentation
found."
  (if (null apropos-accumulator)
      (message "No apropos matches for `%s'" apropos-regexp)
    (if doc-fn
	(funcall doc-fn apropos-accumulator))
    (setq apropos-accumulator
	  (sort apropos-accumulator (lambda (a b)
				 (string-lessp (car a) (car b)))))
    (and apropos-label-face
	 (symbolp apropos-label-face)
	 (setq apropos-label-face `(face ,apropos-label-face
					 mouse-face highlight)))
    (with-output-to-temp-buffer "*Apropos*"
      (let ((p apropos-accumulator)
	    (old-buffer (current-buffer))
	    symbol item point1 point2)
	(save-excursion
	  (set-buffer standard-output)
	  (if window-system
	      (insert (substitute-command-keys
		       "Click \\<apropos-local-map>\\[apropos-mouse-follow] to get full documentation.\n")))
	  (insert (substitute-command-keys
		   "In this buffer, type \\<apropos-local-map>\\[apropos-follow] to get full documentation.\n\n"))
	  (while (consp p)
	    (or (not spacing) (bobp) (terpri))
	    (setq apropos-item (car p)
		  symbol (car apropos-item)
		  p (cdr p)
		  point1 (point))
	    (princ symbol)		        ;print symbol name
	    (setq point2 (point))
	    ;; don't calculate key-bindings unless needed
	    (and do-keys
		 (commandp symbol)
		 (indent-to 30 1)
		 (insert
		  (if (setq item (save-excursion
				   (set-buffer old-buffer)
				   (where-is-internal symbol)))
		      (mapconcat
		       (if apropos-keybinding-face
			   (lambda (key)
			     (setq key (key-description key))
			     (put-text-property 0 (length key)
						'face apropos-keybinding-face
						key)
			     key)
			 'key-description)
		       item ", ")
		    "(not bound to any keys)")))
	    (terpri)
	    ;; only now so we don't propagate text attributes all over
	    (put-text-property point1 (1+ point1) 'item
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
			       do-keys)
	    (apropos-print-doc 'describe-variable 2
			       "Variable" do-keys)
	    (apropos-print-doc 'apropos-describe-plist 3
			       "Plist" nil))
	  (put-text-property 1 (point) 'local-map apropos-local-map)))))
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
	(if apropos-label-face
	    (add-text-properties (- (point) (length str) 2)
				 (1- (point))
				 apropos-label-face))
	(insert (if do-keys (substitute-command-keys i) i))
	(or (bolp) (terpri)))))


(defun apropos-mouse-follow (event)
  (interactive "e")
  (let ((other (if (eq (current-buffer) (get-buffer "*Help*"))
		   ()
		 (current-buffer))))
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    ;; somehow when clicking with the point in another window, undoes badly
    (undo-boundary)
    (apropos-follow other)))


(defun apropos-follow (&optional other)
  (interactive)
  (let ((point (point))
	(item (get-text-property (point) 'item))
	action action-point)
    (or item
	(setq item (if (bobp)
		       ()
		     (previous-single-property-change (point) 'item))
	      item (get-text-property
		    (1- (goto-char
			 (if item
			     item
			   (1+ (next-single-property-change (point) 'item)))))
		    'item)))
    (if (consp item)
	(error "%s is just a lonely symbol" (car item)))
    (while (if (setq action-point
		     (next-single-property-change (point) 'action))
	       (<= action-point point))
      (goto-char (1+ action-point))
      (setq action action-point))
    (funcall
     (prog1 (get-text-property (or action action-point (point)) 'action)
       (if other (set-buffer other)))
     item)))



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
    (princ ")")))

;;; apropos.el ends here
