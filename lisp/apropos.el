;;; apropos.el --- faster apropos commands.

;; Copyright (C) 1989, 1994 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bigbird.bu.edu>
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
;; Made fast-apropos and super-apropos share code.
;; Sped up fast-apropos again.
;; Added apropos-do-all option.
;; Added fast-command-apropos.
;; Changed doc strings to comments for helping functions.
;; Made doc file buffer read-only, buried it.
;; Only call substitute-command-keys if do-all set.

;;; Code:

(defvar apropos-do-all nil
  "*Whether `apropos' and `super-apropos' should do everything that they can.
Makes them run 2 or 3 times slower.  Set this non-nil if you have a fast
machine.")

;;;###autoload
(defun apropos (regexp &optional do-all pred no-header)
  "Show all symbols whose names contain matches for REGEXP.
If optional argument DO-ALL is non-nil (prefix argument if interactive),
or if `apropos-do-all' is non-nil, does more (time-consuming) work such as
showing key bindings.  Optional argument PRED is called with each symbol, and
if it returns nil, the symbol is not shown.

Optional argument NO-HEADER means don't print `Function:' or `Variable:'
in the output.

Returns list of symbols and documentation found."
  (interactive "sApropos (regexp): \nP")
  (setq do-all (or apropos-do-all do-all))
  (let ((apropos-accumulate (apropos-internal regexp pred)))
    (if (null apropos-accumulate)
	(message "No apropos matches for `%s'" regexp)
      (apropos-get-doc apropos-accumulate)
      (with-output-to-temp-buffer "*Help*"
	(apropos-print-matches apropos-accumulate regexp nil
			       do-all no-header)))
    apropos-accumulate))

;; Takes LIST of symbols and adds documentation.  Modifies LIST in place.
;; Resulting alist is of form ((symbol fn-doc var-doc) ...).  Should only be
;; called by apropos.  Returns LIST.

(defun apropos-get-doc (list)
  (let ((p list)
	fn-doc var-doc symbol)
    (while (consp p)
      (setq symbol (car p)
	    fn-doc (and (fboundp symbol)
			(documentation symbol))
	    var-doc (documentation-property symbol 'variable-documentation)
	    fn-doc (and fn-doc
			(substring fn-doc 0 (string-match "\n" fn-doc)))
	    var-doc (and var-doc
			 (substring var-doc 0 (string-match "\n" var-doc))))
      (setcar p (list symbol fn-doc var-doc))
      (setq p (cdr p)))
    list))

;; Variables bound by super-apropos and used by its subroutines.
;; It would be good to say what each one is for, but I don't know -- rms.
(defvar apropos-item)
(defvar apropos-var-doc)
(defvar apropos-fn-doc)
(defvar apropos-accumulate)
(defvar apropos-regexp
  "Within `super-apropos', this holds the REGEXP argument.")

;;;###autoload
(defun super-apropos (regexp &optional do-all)
  "Show symbols whose names/documentation contain matches for REGEXP.
If optional argument DO-ALL is non-nil (prefix argument if interactive),
or if `apropos-do-all' is non-nil, does more (time-consuming) work such as
showing key bindings and documentation that is not stored in the documentation
file.

Returns list of symbols and documentation found."
  (interactive "sSuper Apropos: \nP")
  (setq do-all (or apropos-do-all do-all))
  (let ((apropos-regexp regexp)
	apropos-accumulate apropos-fn-doc apropos-var-doc apropos-item)
    (setq apropos-accumulate (super-apropos-check-doc-file apropos-regexp))
    (if (null apropos-accumulate)
	(message "No apropos matches for `%s'" apropos-regexp)
      (if do-all (mapatoms 'super-apropos-accumulate))
      (with-output-to-temp-buffer "*Help*"
	(apropos-print-matches apropos-accumulate nil t do-all)))
    apropos-accumulate))

;; Finds all documentation related to REGEXP in internal-doc-file-name.
;; Returns an alist of form ((symbol fn-doc var-doc) ...).

(defun super-apropos-check-doc-file (regexp)
  (let* ((doc-file (concat doc-directory internal-doc-file-name))
	 (doc-buffer
	  ;; Force fundamental mode for the DOC file.
	  (let (auto-mode-alist)
	    (find-file-noselect doc-file t)))
	type symbol doc sym-list)
    (save-excursion
      (set-buffer doc-buffer)
      ;; a user said he might accidentally edit the doc file
      (setq buffer-read-only t)
      (bury-buffer doc-buffer)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(search-backward "\C-_")
	(setq type (if (eq ?F (char-after (1+ (point))))
		       1		;function documentation
		     2)			;variable documentation
	      symbol (progn
		       (forward-char 2)
		       (read doc-buffer))
	      doc (buffer-substring
		   (point)
		   (progn
		     (if (search-forward "\C-_" nil 'move)
			 (1- (point))
		       (point))))
	      apropos-item (assq symbol sym-list))
	(and (if (= type 1)
		 (and (fboundp symbol) (documentation symbol))
	       (documentation-property symbol 'variable-documentation))
	     (or apropos-item
		 (setq apropos-item (list symbol nil nil)
		       sym-list (cons apropos-item sym-list)))
	     (setcar (nthcdr type apropos-item) doc))))
    sym-list))

;; This is passed as the argument to map-atoms, so it is called once for every
;; symbol in obarray.  Takes one argument SYMBOL, and finds any memory-resident
;; documentation on that symbol if it matches a variable regexp.

(defun super-apropos-accumulate (symbol)
  (cond ((string-match apropos-regexp (symbol-name symbol))
	 (setq apropos-item (apropos-get-accum-item symbol))
	 (setcar (cdr apropos-item) (or (safe-documentation symbol)
				(nth 1 apropos-item)))
	 (setcar (nthcdr 2 apropos-item) (or (safe-documentation-property symbol)
				     (nth 2 apropos-item))))
	(t
	 (and (setq apropos-fn-doc (safe-documentation symbol))
	      (string-match apropos-regexp apropos-fn-doc)
	      (setcar (cdr (apropos-get-accum-item symbol)) apropos-fn-doc))
	 (and (setq apropos-var-doc (safe-documentation-property symbol))
	      (string-match apropos-regexp apropos-var-doc)
	      (setcar (nthcdr 2 (apropos-get-accum-item symbol))
		      apropos-var-doc))))
  nil)

;; Prints the symbols and documentation in alist MATCHES of form ((symbol
;; fn-doc var-doc) ...).  Uses optional argument REGEXP to speed up searching
;; for keybindings.  The names of all symbols in MATCHES must match REGEXP.
;; Displays in the buffer pointed to by standard-output.  Optional argument
;; SPACING means put blank lines in between each symbol's documentation.
;; Optional argument DO-ALL means do more time-consuming work, specifically,
;; consulting key bindings.  Should only be called within a
;; with-output-to-temp-buffer.

(defun apropos-print-matches (matches &optional regexp
				      spacing do-all no-header)
  (setq matches (sort matches (function
			       (lambda (a b)
				 (string-lessp (car a) (car b))))))
  (let ((p matches)
	(old-buffer (current-buffer))
	item keys-done symbol tem)
    (save-excursion
      (set-buffer standard-output)
      (or matches (princ "No matches found."))
      (while (consp p)
	(setq item (car p)
	      symbol (car item)
	      p (cdr p))
	(or (not spacing) (bobp) (terpri))
	(princ symbol)		        ;print symbol name
	;; don't calculate key-bindings unless needed
	(cond ((and do-all (commandp symbol) (not keys-done))
	       (save-excursion
		 (set-buffer old-buffer)
		 (apropos-match-keys matches regexp))
	       (setq keys-done t)))
	(cond ((and do-all
		    (or (setq tem (nthcdr 3 item))
			(commandp symbol)))
	       (indent-to 30 1)
	       (if tem
		   (princ (mapconcat 'key-description tem ", "))
		 (princ "(not bound to any keys)"))))
	(terpri)
	(cond ((setq tem (nth 1 item))
	       (let ((substed (if do-all (substitute-command-keys tem) tem)))
		 (if no-header
		     (princ "  ")
		   (princ "  Function: ")
		   (if (> (length substed) 67)
		       (princ "\n  ")))
		 (princ substed))))
	(or (bolp) (terpri))
	(cond ((setq tem (nth 2 item))
	       (let ((substed (if do-all (substitute-command-keys tem) tem)))
		 (if no-header
		     (princ "  ")
		   (princ "  Variable: ")
		   (if (> (length substed) 67)
		       (princ "\n  ")))
		 (princ substed))))
	(or (bolp) (terpri)))
      (help-mode)))
  t)

;; Find key bindings for symbols that are cars in ALIST.  Optionally, first
;; match the symbol name against REGEXP.  Modifies ALIST in place.  Each key
;; binding is added as a string to the end of the list in ALIST whose car is
;; the corresponding symbol.  The pointer to ALIST is returned.

(defun apropos-match-keys (alist &optional regexp)
  (let* ((current-local-map (current-local-map))
	 ;; Get a list of the top-level maps now active.
	 (top-maps
	  (if overriding-local-map
	      (list overriding-local-map (current-global-map))
	    (append (current-minor-mode-maps)
		    (if current-local-map
			(list current-local-map (current-global-map))
		      (list (current-global-map))))))
	 ;; Turn that into a list of all the maps including submaps.
	 (maps (apply 'append (mapcar 'accessible-keymaps top-maps)))
	 map				;map we are now inspecting
	 sequence			;key sequence to reach map
	 i				;index into vector map
	 command			;what is bound to current keys
	 key				;last key to reach command
	 local				;local binding for sequence + key
	 item)				;symbol data item in alist
    ;; examine all reachable keymaps
    (while (consp maps)
      (setq map (cdr (car maps))
	    sequence (car (car maps))	;keys to reach this map
	    maps (cdr maps))
      ;; Skip the leading `keymap', doc string, etc.
      (if (eq (car map) 'keymap)
	  (setq map (cdr map)))
      (while (stringp (car-safe map))
	(setq map (cdr map)))

      (while (consp map)
	(cond ((consp (car map))
	       (setq command (cdr (car map))
		     key (car (car map)))
	       ;; Skip any menu prompt and help string in this key binding.
	       (while (and (consp command) (stringp (car command)))
		 (setq command (cdr command)))
	       ;; Skip any cached equivalent key.
	       (and (consp command)
		    (consp (car command))
		    (setq command (cdr command)))
	       ;; if is a symbol, and matches optional regexp, and is a car
	       ;; in alist, and is not shadowed by a different local binding,
	       ;; record it
	       (and (symbolp command)
		    (if regexp
			(string-match regexp (symbol-name command))
		      t)
		    (setq item (assq command alist))
		    (if (or (vectorp sequence) (not (integerp key)))
			(setq key (vconcat sequence (vector key)))
		      (setq key (concat sequence (char-to-string key))))
		    ;; checking if shadowed by local binding.
		    ;; either no local map, no local binding, or runs off the
		    ;; binding tree (number), or is the same binding
		    (or (not current-local-map)
			(not (setq local (lookup-key current-local-map key)))
			(numberp local)
			(eq command local))
		    ;; check if this binding is already recorded
		    ;; (this can happen due to inherited keymaps)
		    (not (member key (nthcdr 3 item)))
		    ;; add this key binding to the item in alist
		    (nconc item (cons key nil))))
	      ((vectorp (car map))
	       (let ((i 0)
		     (vec (car map))
		     (len (length (car map))))
		 (while (< i len)
		   (setq command (aref vec i))
		   (setq key i)
		   ;; Skip any menu prompt in this key binding.
		   (and (consp command) (symbolp (cdr command))
			(setq command (cdr command)))
		   ;; This is the same as the code in the previous case.
		   (and (symbolp command)
			(if regexp
			    (string-match regexp (symbol-name command))
			  t)
			(setq item (assq command alist))
			(if (or (vectorp sequence) (not (integerp key)))
			    (setq key (vconcat sequence (vector key)))
			  (setq key (concat sequence (char-to-string key))))
			;; checking if shadowed by local binding.
			;; either no local map, no local binding, or runs off the
			;; binding tree (number), or is the same binding
			(or (not current-local-map)
			    (not (setq local (lookup-key current-local-map key)))
			    (numberp local)
			    (eq command local))
			;; check if this binding is already recorded
			;; (this can happen due to inherited keymaps)
			(not (member key (nthcdr 3 item)))
			;; add this key binding to the item in alist
			(nconc item (cons key nil)))
		   (setq i (1+ i))))))
	(setq map (cdr map)))))
  alist)

;; Get an alist item in alist apropos-accumulate whose car is SYMBOL.  Creates
;; the item if not already present.  Modifies apropos-accumulate in place.

(defun apropos-get-accum-item (symbol)
  (or (assq symbol apropos-accumulate)
      (progn
	(setq apropos-accumulate
	      (cons (list symbol nil nil) apropos-accumulate))
	(assq symbol apropos-accumulate))))

(defun safe-documentation (function)
  "Like documentation, except it avoids calling `get_doc_string'.
Will return nil instead."
  (while (symbolp function)
    (setq function (if (fboundp function)
		       (symbol-function function)
		     0)))
  (if (eq (car-safe function) 'macro)
      (setq function (cdr function)))
  (if (not (consp function))
      nil
    (if (not (memq (car function) '(lambda autoload)))
	nil
      (setq function (nth 2 function))
      (if (stringp function)
	  function
	nil))))

(defun safe-documentation-property (symbol)
  "Like documentation-property, except it avoids calling `get_doc_string'.
Will return nil instead."
  (setq symbol (get symbol 'variable-documentation))
  (if (numberp symbol)
      nil
    symbol))

;;; apropos.el ends here
