;;; tmm.el - text mode access to menu-bar

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Ilya Zakharevich <ilya@math.mps.ohio-state.edu>

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

;;; Commentary ============================================================

;;; To use this package add 

;;; (autoload 'tmm-menubar 'tmm "Text mode substitute for menubar" t) 
;;; (global-set-key [f10] 'tmm-menubar)

;;; to your .emacs file. You can also add your own access to different
;;; menus available in Window System Emacs modelling definition after
;;; tmm-menubar.

(require 'electric)
(define-key completion-list-mode-map "\e\e" 'abort-recursive-edit)
(define-key completion-list-mode-map [left] 'backward-word)
(define-key completion-list-mode-map [right] 'forward-word)
;(define-key minibuffer-local-must-match-map [pageup] 'tmm-goto-completions)
;(define-key minibuffer-local-must-match-map [prior] 'tmm-goto-completions)
;(define-key minibuffer-local-must-match-map "\ev" 'tmm-goto-completions)
(define-key minibuffer-local-must-match-map [up] 'previous-history-element)
(define-key minibuffer-local-must-match-map [down] 'next-history-element)

;;; The following will be localized, added only to pacify the compiler.
(defvar tmm-short-cuts)
(defvar tmm-old-mb-map)
(defvar tmm-old-comp-map)
(defvar tmm-c-prompt)
(defvar tmm-km-list)
(defvar tmm-table-undef)

;;;###autoload
(defun tmm-menubar ()
  "Text-mode emulation of looking and choosing from a menubar.
See the documentation for `tmm-prompt'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (tmm-prompt (tmm-get-keybind [menu-bar])))

(defvar tmm-mid-prompt "==>"
  "String to insert between shortcut and menu item or nil.")

(defvar tmm-mb-map nil
  "A place to store minibuffer map.")

(defvar tmm-completion-prompt 
  "Press PageUp Key to reach this buffer from the minibuffer.
Alternatively, you can use Up/Down keys (or your History keys) to change
the item in the minibuffer, and press RET when you are done, or press the 
marked letters to pick up your choice. ESC ESC to cancel.
"
  "What insert on top of completion buffer.")

;;;###autoload
(defun tmm-prompt (bind &optional in-popup)
  "Text-mode emulation of calling the bindings in keymap.
Creates a text-mode menu of possible choices. You can access the elements
in the menu:
   *)  Either via history mechanism from minibuffer;
   *)  Or via completion-buffer that is automatically shown.
The last alternative is currently a hack, you cannot use mouse reliably.
If the optional argument IN-POPUP is set, is argument-compatible with 
`x-popup-menu', otherwise the argument BIND should be a cdr of sparse keymap."
  (if in-popup (if bind (setq bind in-popup) (x-popup-menu nil in-popup)))
  (let (gl-str tmm-km-list out compl-list compl-list-l tmm-table-undef tmm-c-prompt
	       tmm-old-mb-map tmm-old-comp-map tmm-short-cuts)
    (run-hooks 'activate-menubar-hook)
    (mapcar (function (lambda (elt)
			(if (stringp elt)
			    (setq gl-str elt)
			  (and (listp elt) (tmm-get-keymap elt in-popup)))
			)) bind)
    (and tmm-km-list
	 (if tmm-mid-prompt
	     (setq tmm-km-list (reverse (tmm-add-shortcuts tmm-km-list)))
	   t)
	 (setq compl-list (mapcar 'car tmm-km-list))
	 (setq compl-list-l (length compl-list))
	 (setq compl-list (append compl-list compl-list compl-list compl-list))
	 (setq tmm-c-prompt (nth (1- compl-list-l) compl-list))
	 (add-hook 'minibuffer-setup-hook 'tmm-add-prompt)
	 (unwind-protect
	     (setq out
		   (completing-read
		    (concat gl-str " (up/down to change, PgUp to menu): ")
		    tmm-km-list nil t nil
		    (cons 'compl-list (* 2 compl-list-l))))
	   ;;(add-hook 'minibuffer-setup-hook 'tmm-remove-shortcuts)
	   ;;(save-excursion
	   ;;  (set-buffer "*Completions*")
	   ;;  (use-local-map tmm-old-mb-map))
	   (save-excursion
	     (set-buffer "*Completions*")
	     (use-local-map tmm-old-comp-map)
	     (bury-buffer (current-buffer)))
	   ))
    (setq bind (cdr (assoc out tmm-km-list)))
    (and (null bind)
	 (> (length out) (length tmm-c-prompt))
	 (string= (substring out 0 (length tmm-c-prompt)) tmm-c-prompt)
	 (setq out (substring out (length tmm-c-prompt))
	       bind (cdr (assoc out tmm-km-list))))
    (setq last-command-event (car bind))
    (setq bind (cdr bind))
    (if bind
	 (if in-popup (tmm-prompt t bind)
	   (if (keymapp bind)
	       (if (listp bind)
		   (progn
		     (condition-case nil
			 (require 'mouse)
		       (error nil))
		     (condition-case nil
			 (x-popup-menu nil bind) ; Get the shortcuts
		       (error nil))
		     (tmm-prompt bind))
		 (tmm-prompt (symbol-value bind))
		 )
	     (if last-command-event
		 (call-interactively bind)
	       bind)))
      gl-str)))

(defun tmm-remove-shortcuts ()
  (use-local-map tmm-mb-map))

(defun tmm-add-shortcuts (list)
  "Adds shortcuts to cars of elements of the list.
Takes a list of lists with a string as car, returns list with
shortcuts added to these cars. Adds the shortcuts to a free variable
`tmm-short-cuts'."
  (mapcar (lambda (elt)
	    (let ((str (car elt)) f b)
	      (setq f (upcase (substring str 0 1)))
	      ;; If does not work, try beginning of the other word
	      (if (and (member f tmm-short-cuts)
		       (string-match " \\([^ ]\\)" str))
		  (setq f (upcase (substring
				   str
				   (setq b (match-beginning 1)) (1+ b)))))
	      (if (member f tmm-short-cuts)
		  elt
		(setq tmm-short-cuts (cons f tmm-short-cuts))
		(cons (concat f tmm-mid-prompt str) (cdr elt)))))
	  (reverse list)))

(defun tmm-add-prompt ()
  (remove-hook 'minibuffer-setup-hook 'tmm-add-prompt)
  (add-hook 'minibuffer-exit-hook 'tmm-delete-map)
  (let ((map (make-sparse-keymap)) (win (selected-window)))
    (mapcar (lambda (str)
	      (define-key map str 'tmm-shortcut)
	      (define-key map (downcase str) 'tmm-shortcut))
	    tmm-short-cuts)
    (define-key map [pageup] 'tmm-goto-completions)
    (define-key map [prior] 'tmm-goto-completions)
    (define-key map "\ev" 'tmm-goto-completions)
    (define-key map "\e\e" 'abort-recursive-edit)
    (setq tmm-old-mb-map (current-local-map))
    (use-local-map (append map (cdr tmm-old-mb-map)))
    ;; Get window and hide it for electric mode to get correct size
    (save-window-excursion 
      (minibuffer-completion-help)
      (set-buffer "*Completions*")
      (goto-char 1)
      (insert tmm-completion-prompt)
      )
    (save-excursion
      (other-window 1)			; Electric-pop-up-window does
					; not work in minibuffer
      (set-buffer (window-buffer (Electric-pop-up-window "*Completions*")))
      (setq tmm-old-comp-map (current-local-map))
      (use-local-map (append map (cdr tmm-old-comp-map)))
      (select-window win)		; Cannot use
					; save-window-excursion, since
					; it restores the size
      )
    (insert tmm-c-prompt)))

(defun tmm-delete-map ()
  (remove-hook 'minibuffer-exit-hook 'tmm-delete-map)
  (use-local-map tmm-old-mb-map))

(defun tmm-shortcut ()
  (interactive)
  (let ((c (upcase (char-to-string last-command-char))) s)
    (if (member c tmm-short-cuts)
	  (if (equal (buffer-name) "*Completions*")
	      (progn
		(beginning-of-buffer)
		(re-search-forward
		 (concat "\\(^\\|[ \t]\\)" c tmm-mid-prompt))
		(choose-completion))
	    (erase-buffer)		; In minibuffer
	    (mapcar (lambda (elt)
		      (if (string=
		       (substring (car elt) 0 
				  (min (1+ (length tmm-mid-prompt))
				       (length (car elt))))
		       (concat c tmm-mid-prompt))
			  (setq s (car elt))))
		    tmm-km-list)
	    (insert s)
	    (exit-minibuffer)))))

(defun tmm-goto-completions ()
  (interactive)
  (setq tmm-c-prompt (buffer-string))
  (erase-buffer)
  (switch-to-buffer-other-window
   "*Completions*")
  (search-forward tmm-c-prompt)
  (search-backward tmm-c-prompt))


(defun tmm-get-keymap (elt &optional in-x-menu) 
  "Prepends (DOCSTRING EVENT BINDING) to free variable `tmm-km-list'.
The values are deduced from the argument ELT, that should be an
element of keymap, on `x-popup-menu' argument, or an element of
`x-popup-menu' argument (when IN-X-MENU is not-nil).
Does it only if it is not already there. Uses free variable 
`tmm-table-undef' to keep undefined keys."
  (let (km str cache (event (car elt)))
    (setq elt (cdr elt))
    (if (eq elt 'undefined)
	(setq tmm-table-undef (cons (cons event nil) tmm-table-undef))
      (or
       (assoc event tmm-table-undef)
       (and (if (listp elt)
		(keymapp elt)
	      (fboundp elt))
	    (setq km elt))
       (and (if (listp (cdr-safe elt))
		(keymapp (cdr-safe elt))
	      (fboundp (cdr-safe elt)))
	    (setq km (cdr elt))
	    (and (stringp (car elt)) (setq str (car elt))))
       (and (if (listp (cdr-safe (cdr-safe elt)))
		(keymapp (cdr-safe (cdr-safe elt)))
	      (fboundp (cdr-safe (cdr-safe elt))))
	    (setq km (cdr (cdr elt)))
	    (and (stringp (car elt)) (setq str (car elt)))
	    (or (and str
		     (stringp (cdr (car (cdr elt)))) ; keyseq cache
		     (setq cache (cdr (car (cdr elt))))
		     cache (setq str (concat str cache))) str))
       (and (if (listp (cdr-safe (cdr-safe (cdr-safe elt))))
		(keymapp (cdr-safe (cdr-safe (cdr-safe elt))))
	      (fboundp (cdr-safe (cdr-safe (cdr-safe elt)))))
					; New style of easy-menu
	    (setq km (cdr (cdr (cdr elt))))
	    (and (stringp (car elt)) (setq str (car elt)))
	    (or (and str
		     (stringp (cdr (car (cdr (cdr elt))))) ; keyseq cache
		     (setq cache (cdr (car (cdr (cdr elt)))))
		     cache (setq str (concat str cache)))
		str))
	    (and (stringp event)	; x-popup or x-popup element
		 (if (or in-x-menu (stringp (car-safe elt)))
		     (setq str event event nil km elt)
		   (setq str event event nil km (cons 'keymap elt))
		   )))
      (and km (stringp km) (setq str km))
      (and km str
	   (or (assoc str tmm-km-list)
	       (setq tmm-km-list 
		     (cons (cons str (cons event km)) tmm-km-list)))
	   ))))


(defun tmm-get-keybind (keyseq)
  "Gets binding from all the tables, can have some junk inside."
  (let (allbind bind)
    (setq allbind (mapcar 'cdr (minor-mode-key-binding keyseq))) 
    (setq allbind (append allbind (list (local-key-binding keyseq))))
    (setq allbind (append allbind (list (global-key-binding keyseq))))
					; list of bindings
    (mapcar (lambda (in)
	      (if (and (symbolp in) (keymapp in))
		  (setq in (symbol-value in)))
	      (and in
		   (or (eq bind 'undefined) (not bind)
		       (and (keymapp bind) (keymapp in)))
		   (if (keymapp bind)
		       (setq bind (append bind (cdr in)))
		     (setq bind in)
		     )
		   )
	      )
	    allbind)
    bind))

(add-hook 'calendar-load-hook (lambda () (require 'cal-menu)))


(provide 'tmm)


;;; tmm.el ends here
