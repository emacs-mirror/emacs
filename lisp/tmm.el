;;; tmm.el --- text mode access to menu-bar

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Ilya Zakharevich <ilya@math.mps.ohio-state.edu>
;; Maintainer: FSF

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

;;; The following will be localized, added only to pacify the compiler.
(defvar tmm-short-cuts)
(defvar tmm-old-mb-map nil)
(defvar tmm-old-comp-map)
(defvar tmm-c-prompt)
(defvar tmm-km-list)
(defvar tmm-table-undef)

;;;###autoload (define-key global-map "\M-`" 'tmm-menubar)
;;;###autoload (define-key global-map [f10] 'tmm-menubar)
;;;###autoload (define-key global-map [menu-bar mouse-1] 'tmm-menubar)

;;;###autoload
(defun tmm-menubar ()
  "Text-mode emulation of looking and choosing from a menubar.
See the documentation for `tmm-prompt'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  ;; Obey menu-bar-final-items; put those items last.
  (let ((menu-bar (tmm-get-keybind [menu-bar])))
    (let ((list menu-bar-final-items))
      (while list
	(let ((item (car list)))
	  ;; ITEM is the name of an item that we want to put last.
	  ;; Find it in MENU-BAR and move it to the end.
	  (let ((this-one (assq item menu-bar)))
	    (setq menu-bar (append (delq this-one menu-bar)
				   (list this-one)))))
	(setq list (cdr list))))
    (tmm-prompt menu-bar)))

(defvar tmm-mid-prompt "==>"
  "String to insert between shortcut and menu item or nil.")

(defvar tmm-mb-map nil
  "A place to store minibuffer map.")

(defvar tmm-completion-prompt 
  "Press PageUp Key to reach this buffer from the minibuffer.
Alternatively, you can use Up/Down keys (or your History keys) to change
the item in the minibuffer, and press RET when you are done, or press the 
marked letters to pick up your choice.  Type C-g or ESC ESC ESC to cancel.
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
  (let (gl-str tmm-km-list out history history-len tmm-table-undef tmm-c-prompt
	       tmm-old-mb-map tmm-old-comp-map tmm-short-cuts)
    (run-hooks 'activate-menubar-hook)
    (mapcar (function (lambda (elt)
			(if (stringp elt)
			    (setq gl-str elt)
			  (and (listp elt) (tmm-get-keymap elt in-popup)))))
	    bind)
    (and tmm-km-list
	 (progn
	   (if tmm-mid-prompt
	       (setq tmm-km-list (tmm-add-shortcuts tmm-km-list))
	     t)
	   (setq history (reverse (mapcar 'car tmm-km-list)))
	   (setq history-len (length history))
	   (setq history (append history history history history))
	   (setq tmm-c-prompt (nth (1- history-len) history))
	   (add-hook 'minibuffer-setup-hook 'tmm-add-prompt)
	   (unwind-protect
	       (setq out
		     (completing-read
		      (concat gl-str " (up/down to change, PgUp to menu): ")
		      tmm-km-list nil t nil
		      (cons 'history (* 2 history-len))))
	     (save-excursion
	       (remove-hook 'minibuffer-setup-hook 'tmm-add-prompt)
	       (if (get-buffer "*Completions*")
		   (progn
		     (set-buffer "*Completions*")
		     (use-local-map tmm-old-comp-map)
		     (bury-buffer (current-buffer)))))
	     )))
    (setq bind (cdr (assoc out tmm-km-list)))
    (and (null bind)
	 (> (length out) (length tmm-c-prompt))
	 (string= (substring out 0 (length tmm-c-prompt)) tmm-c-prompt)
	 (setq out (substring out (length tmm-c-prompt))
	       bind (cdr (assoc out tmm-km-list))))
    (and (null bind)
	 (setq out (try-completion out tmm-km-list)
	       bind (cdr (assoc  out tmm-km-list))))
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


(defun tmm-add-shortcuts (list)
  "Adds shortcuts to cars of elements of the list.
Takes a list of lists with a string as car, returns list with
shortcuts added to these cars.
Stores a list of all the shortcuts in the free variable `tmm-short-cuts'."
  (let ((next-shortcut-number 0))
    (mapcar (lambda (elt)
	      (let ((str (car elt)) f b)
		(setq f (upcase (substring str 0 1)))
		;; If does not work, try beginning of the other word
		(if (and (member f tmm-short-cuts)
			 (string-match " \\([^ ]\\)" str))
		    (setq f (upcase (substring
				     str
				     (setq b (match-beginning 1)) (1+ b)))))
		;; If we don't have an unique letter shortcut,
		;; pick a digit as a shortcut instead.
		(if (member f tmm-short-cuts)
		    (if (< next-shortcut-number 10)
			(setq f (format "%d" next-shortcut-number)
			      next-shortcut-number (1+ next-shortcut-number))
		      (setq f nil)))
		(if (null f)
		    elt
		  (setq tmm-short-cuts (cons f tmm-short-cuts))
		  (cons (concat f tmm-mid-prompt str) (cdr elt)))))
	    (reverse list))))

(defun tmm-define-keys ()
  (mapcar (lambda (str)
	    (define-key (current-local-map) str 'tmm-shortcut)
	    (define-key (current-local-map) (downcase str) 'tmm-shortcut))
	  tmm-short-cuts)
  (define-key (current-local-map) [pageup] 'tmm-goto-completions)
  (define-key (current-local-map) [prior] 'tmm-goto-completions)
  (define-key (current-local-map) "\ev" 'tmm-goto-completions)
  (define-key (current-local-map) "\C-n" 'next-history-element)
  (define-key (current-local-map) "\C-p" 'previous-history-element))

(defun tmm-add-prompt ()
  (remove-hook 'minibuffer-setup-hook 'tmm-add-prompt)
  (make-local-hook 'minibuffer-exit-hook)
  (add-hook 'minibuffer-exit-hook 'tmm-delete-map nil t)
  (let ((win (selected-window)))
    (setq tmm-old-mb-map (current-local-map))
    (use-local-map (append (make-sparse-keymap) tmm-old-mb-map))
    (tmm-define-keys)
    ;; Get window and hide it for electric mode to get correct size
    (save-window-excursion 
      (let ((completions
	     (mapcar 'car minibuffer-completion-table)))
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list completions)))
      (set-buffer "*Completions*")
      (goto-char 1)
      (insert tmm-completion-prompt)
      )
    (save-excursion
      (other-window 1)			; Electric-pop-up-window does
					; not work in minibuffer
      (set-buffer (window-buffer (Electric-pop-up-window "*Completions*")))
      (setq tmm-old-comp-map (current-local-map))
      (use-local-map (append (make-sparse-keymap) tmm-old-comp-map))
      (tmm-define-keys)
      (select-window win)		; Cannot use
					; save-window-excursion, since
					; it restores the size
      )
    (insert tmm-c-prompt)))

(defun tmm-delete-map ()
  (remove-hook 'minibuffer-exit-hook 'tmm-delete-map t)
  (if tmm-old-mb-map
      (use-local-map tmm-old-mb-map)))

(defun tmm-shortcut ()
  "Choose the shortcut that the user typed."
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
  (switch-to-buffer-other-window "*Completions*")
  (search-forward tmm-c-prompt)
  (search-backward tmm-c-prompt))


(defun tmm-get-keymap (elt &optional in-x-menu) 
  "Prepends (DOCSTRING EVENT BINDING) to free variable `tmm-km-list'.
The values are deduced from the argument ELT, that should be an
element of keymap, an `x-popup-menu' argument, or an element of
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
  "Return the current binding of KEYSEQ, merging prefix definitions.
If KEYSEQ is a prefix key that has local and gloibal bindings,
we merge them into a single keymap which shows the proper order of the menu.
However, for the menu bar itself, the value does not take account
of `menu-bar-final-items'."
  (let (allbind bind)
    (setq bind (key-binding keyseq))
    ;; If KEYSEQ is a prefix key, then BIND is either nil
    ;; or a symbol defined as a keymap (which satisfies keymapp).
    (if (keymapp bind)
	(setq bind nil))
    ;; If we have a non-keymap definition, return that.
    (or bind
	(progn
	  ;; Otherwise, it is a prefix, so make a list of the subcommands.
	  ;; Make a list of all the bindings in all the keymaps.
	  (setq allbind (mapcar 'cdr (minor-mode-key-binding keyseq))) 
	  (setq allbind (cons (local-key-binding keyseq) allbind))
	  (setq allbind (cons (global-key-binding keyseq) allbind))
	  ;; Merge all the elements of ALLBIND into one keymap.
	  (mapcar (lambda (in)
		    (if (and (symbolp in) (keymapp in))
			(setq in (symbol-function in)))
		    (and in (keymapp in)
			 (if (keymapp bind)
			     (setq bind (nconc bind (copy-sequence (cdr in))))
			   (setq bind (copy-sequence in)))))
		  allbind)
	  ;; Return that keymap.
	  bind))))

(add-hook 'calendar-load-hook (lambda () (require 'cal-menu)))


(provide 'tmm)


;;; tmm.el ends here
