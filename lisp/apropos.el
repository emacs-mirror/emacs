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

;; Optionally use faces to make the output more legible.
;; Differentiate between command and function.
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


(defvar apropos-use-faces window-system
  "*Whether the apropos commands display output using bold and italic.
This looks good, but slows down the commands several times.")


(defvar apropos-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'apropos-follow)
    (define-key map [mouse-2] 'apropos-mouse-follow)
    (define-key map [down-mouse-2] nil)
    map)
  "Local map active when displaying apropos output.")



;;;###autoload (fset 'command-apropos 'apropos-command)
;;;###autoload
(defun apropos-command (regexp &optional do-all)
  "Shows commands (interactively callable functions) that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
variables."
  (interactive (list (read-string (concat "Apropos command "
					  (if (or current-prefix-arg
						   apropos-do-all)
					      "or variable ")
					  "(regexp): "))
		     (or current-prefix-arg  apropos-do-all)))
  (let ((message
	 (let ((standard-output (get-buffer-create "*Help*")))
	   (print-help-return-message 'identity))))
    (if (apropos-print
	 regexp
	 (apropos-internal regexp
			   (if do-all
			       (lambda (x) (or (commandp x)
					       (user-variable-p x)))
			     'commandp))
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
(defun apropos (regexp &optional do-all)
  "Show all symbols whose names match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show key
bindings, which is a little more time-consuming.
Returns list of symbols and documentation found."
  (interactive "sApropos symbol (regexp): \nP")
  (apropos-print
   regexp (apropos-internal regexp)
   (or apropos-do-all do-all)
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
(defun apropos-value (regexp &optional do-all)
  "Show all symbols whose value's printed image matches REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also looks
at the function and at the names and values of properties.
Returns list of symbols and documentation found."
  (interactive "sApropos value (regexp): \nP")
  (setq do-all (or apropos-do-all do-all))
  (apropos-print
   regexp
   (let (accumulator f v p)
     (mapatoms
      (lambda (symbol)
	(setq f nil v nil p nil)
	(or (memq symbol '(regexp do-all accumulator symbol v pl p))
	    (if (boundp symbol)
		(setq v (prin1-to-string (symbol-value symbol))
		      v (if (string-match regexp v) v))))
	(if do-all
	    (progn
	      (if (fboundp symbol)
		  (setq f (prin1-to-string (symbol-function symbol))
			f (if (string-match regexp f) f)))
	      (setq p (apropos-format-plist symbol "\n    " regexp))))
	;;	(if p-out (insert p-out))
	(if (or f v p)
	    (setq accumulator (cons (list symbol f v p) accumulator)))))
     accumulator)
   nil nil t))


(defun apropos-format-plist (pl sep &optional regexp)
  (setq pl (symbol-plist pl))
  (let (p p-out)
    (while pl
      (setq p (format "%s %S" (car pl) (nth 1 pl)))
      (if (string-match (or regexp "") p)
	  (if apropos-use-faces
	      (put-text-property 0 (length (symbol-name (car pl)))
				 'face 'bold-italic p))
	(setq p nil))
      (if p (setq p-out (concat p-out (if p-out sep) p)))
      (setq pl (nthcdr 2 pl)))
    p-out))



;;;###autoload
(defun apropos-documentation (regexp &optional do-all)
  "Show symbols whose names or documentation contain matches for REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also use
documentation that is not stored in the documentation file and show key
bindings.
Returns list of symbols and documentation found."
  (interactive "sApropos documentation (regexp): \nP")
  (setq do-all (or apropos-do-all do-all))
  (let (accumulator fn-doc var-doc item)
    (setq accumulator (apropos-documentation-check-doc-file regexp))
    (if do-all
	(mapatoms
	 (lambda (symbol)
	   (setq fn-doc (safe-documentation symbol)
		 var-doc (get symbol 'variable-documentation))
	   (if (numberp var-doc)
	       (setq var-doc nil))
	   (if (string-match regexp (symbol-name symbol))
	       ()
	     (if fn-doc
		 (or (string-match regexp fn-doc)
		     (setq fn-doc nil)))
	     (if var-doc
		 (or (string-match regexp var-doc)
		     (setq var-doc nil))))
	   (if (or fn-doc var-doc)
	       (if (setq item (cdr (assq symbol accumulator)))
		   (progn
		     (if fn-doc
			 (setcar item fn-doc))
		     (if var-doc
			 (setcar (cdr item) var-doc)))
		 (setq accumulator
		       (cons (list symbol fn-doc var-doc)
			     accumulator)))))))
    (apropos-print regexp accumulator do-all nil t)))



;; Finds all documentation related to REGEXP in internal-doc-file-name.
;; Returns an alist of form ((symbol fn-doc var-doc) ...).

(defun apropos-documentation-check-doc-file (regexp)
  (let ((doc-buffer (get-buffer-create " *apropos-doc*"))
	;; item is already let
	type symbol sym-list)
    (set-buffer doc-buffer)
    (goto-char (point-min))
    (if (eobp)
	(insert-file-contents (concat doc-directory internal-doc-file-name)))
    (while (re-search-forward regexp nil t)
      (search-backward "\C-_")
      (or (setq type (if (eq ?F (char-after (1+ (point))))
			 1		;function documentation
		       2)		;variable documentation
		symbol (progn
			 (forward-char 2)
			 (read doc-buffer))
		doc (buffer-substring
		     (1+ (point))
		     (if (search-forward "\C-_" nil 'move)
			 (1- (point))
		       (point)))
		item (assq symbol sym-list))
	  (setq item (list symbol nil nil)
		sym-list (cons item sym-list)))
      (setcar (nthcdr type item) doc))
    sym-list))



;; This function is misnamed, it is simply a variety of the original
;; that might be handled easier and more efficiently by that with a flag.
;; Otherwise it might be inlined above.

(defun safe-documentation (function)
  "Like documentation, except it avoids calling `get_doc_string'.
Will return nil instead."
  (while (and function (symbolp function))
    (setq function (if (fboundp function)
		       (symbol-function function))))
  (if (eq (car-safe function) 'macro)
      (setq function (cdr function)))
  (setq function (if (byte-code-function-p function)
		     (condition-case nil
			 (aref function 4)
		       (error))
			(if (memq (car-safe function) '(lambda autoload))
			    (nth 2 function))))
  (if (stringp function)
      function))



(defun apropos-print (regexp apropos-result do-keys doc-fn spacing)
  "Output result of various appropos commands with REGEXP.
APROPOS-RESULT is a list.  Optional DOC-FN is called for each element
of apropos-result and may modify it resulting in (symbol fn-doc
var-doc [plist-doc]).  Returns sorted list of symbols and documentation
found."
  (if (null apropos-result)
      (message "No apropos matches for `%s'" regexp)
    (if doc-fn
	(funcall doc-fn apropos-result))
    (setq apropos-result
	  (sort apropos-result (lambda (a b)
				 (string-lessp (car a) (car b)))))
    (with-output-to-temp-buffer "*Help*"
      (let ((p apropos-result)
	    (old-buffer (current-buffer))
	    symbol item tem point1 point2)
	(save-excursion
	  (set-buffer standard-output)
	  (if window-system
	      (insert (substitute-command-keys
		       "Click \\<apropos-local-map>\\[apropos-mouse-follow] to get full documentation.\n")))
	  (insert (substitute-command-keys
		   "In this buffer, type \\<apropos-local-map>\\[apropos-follow] to get full documentation.\n\n"))
	  (while (consp p)
	    (or (not spacing) (bobp) (terpri))
	    (setq item (car p)
		  symbol (car item)
		  p (cdr p)
		  point1 (point))
	    (princ symbol)		        ;print symbol name
	    (setq point2 (point))
	    ;; don't calculate key-bindings unless needed
	    (and do-keys
		 (commandp symbol)
		 (indent-to 30 1)
		 (princ (if (setq tem (save-excursion
					(set-buffer old-buffer)
					(where-is-internal symbol)))
			    (mapconcat 'key-description tem ", ")
			  "(not bound to any keys)")))
	    (terpri)
	    ;; only now so we don't propagate text attributes all over
	    (put-text-property point1 (1+ point1) 'item
			       (if (or (nth 1 item) (nth 2 item) (nth 3 item))
				   (car item)
				 item))
	    (if apropos-use-faces
		(put-text-property point1 point2 'face 'bold))
	    (apropos-print-documentation 'describe-function (nth 1 item)
					 (if (commandp symbol)
					     "Command: "
					   "Function: ")
					 do-keys)
	    (apropos-print-documentation 'describe-variable (nth 2 item)
					 "Variable: " do-keys)
	    (apropos-print-documentation 'apropos-describe-plist (nth 3 item)
					 "Plist: " nil))
	  (put-text-property 1 (point) 'local-map apropos-local-map)))))
  apropos-result)


(defun apropos-print-documentation (action tem str do-keys)
  (if tem
      (progn
	(insert "  ")
	(put-text-property (- (point) 2) (1- (point))
			   'action action)
	(princ str)
	(if apropos-use-faces
	    (add-text-properties (- (point) (length str))
				 (1- (point))
				 '(face italic
				   mouse-face highlight)))
	(insert (if do-keys (substitute-command-keys tem) tem))))
	(or (bolp) (terpri)))



(defun apropos-mouse-follow (event)
  (interactive "e")
  (let ((other (if (eq (current-buffer) (get-buffer "*Help*"))
		   ()
		 (current-buffer))))
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    ;; somehow when clicking with the point in another window, doesn't undo
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
	(error "%s is just a lonely smbol." (car item)))
    (while (if (setq action-point
		     (next-single-property-change (point) 'action))
	       (<= action-point point))
      (goto-char (1+ action-point))
      (setq action action-point))
    (funcall
     (prog1 (get-text-property (or action action-point (point)) 'action)
       (if other (set-buffer other)))
     item))
  (message "%sype %s (undo) to get back to apropos-listing."
	   (if other "In *Help* buffer t" "T")
	   (key-description (where-is-internal 'undo nil 1))))



(defun apropos-describe-plist (symbol)
  "Display a pretty listing of SYMBOL's plist."
  (with-output-to-temp-buffer "*Help*"
    (set-buffer standard-output)
    (princ "Symbol ")
    (prin1 symbol)
    (princ "'s plist is\n (")
    (if apropos-use-faces
	(put-text-property 8 (- (point) 14) 'face 'bold))
    (insert (apropos-format-plist symbol "\n  "))
    (princ ")")))

;;; apropos.el ends here
