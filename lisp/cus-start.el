;;; cus-start.el --- define customization properties of builtins
;;
;; Copyright (C) 1997, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: internal

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file adds customize support for built-in variables.

;; While dumping Emacs, this file is loaded, but it only records
;; the standard values; it does not do the rest of the job.
;; Later on, if the user makes a customization buffer,
;; this file is loaded again with (require 'cus-start);
;; then it does the whole job.

;;; Code:

(let ((all '(;; abbrev.c
	     (abbrev-all-caps abbrev-mode boolean)
	     (pre-abbrev-expand-hook abbrev-mode hook)
	     ;; alloc.c
	     (gc-cons-threshold alloc integer)
	     (garbage-collection-messages alloc boolean)
	     ;; buffer.c
	     (mode-line-format modeline sexp) ;Hard to do right.
	     (default-major-mode internal function)
	     (enable-multibyte-characters mule boolean)
	     (case-fold-search matching boolean)
	     (fill-column fill integer)
	     (left-margin fill integer)
	     (tab-width editing-basics integer)
	     (ctl-arrow display boolean)
	     (truncate-lines display boolean)
	     (selective-display-ellipses display boolean)
	     (indicate-empty-lines fringe boolean "21.1")
	     (indicate-buffer-boundaries
	      fringe
	      (choice
	       (const :tag "No indicators" nil)
	       (const :tag "On left, with arrows" left)
	       (const :tag "On right, with arrows" right)
	       (set :tag "Pick your own design"
		    :value ((t . nil))
		    :format "%{%t%}:\n%v\n%d"
		    :doc "You can specify a default and then override it \
for individual indicators.
Leaving \"Default\" unchecked is equivalent with specifying a default of
\"Do not show\"."
		    (choice :tag "Default"
			    :value (t . nil)
			    (const :tag "Do not show" (t . nil))
			    (const :tag "On the left" (t . left))
			    (const :tag "On the right" (t . right)))
		    (choice :tag "Top"
			    :value (top . left)
			    (const :tag "Do not show" (top . nil))
			    (const :tag "On the left" (top . left))
			    (const :tag "On the right" (top . right)))
		    (choice :tag "Bottom"
			    :value (bottom . left)
			    (const :tag "Do not show" (bottom . nil))
			    (const :tag "On the left" (bottom . left))
			    (const :tag "On the right" (bottom . right)))
		    (choice :tag "Up arrow"
			    :value (up . left)
			    (const :tag "Do not show" (up . nil))
			    (const :tag "On the left" (up . left))
			    (const :tag "On the right" (up . right)))
		    (choice :tag "Down arrow"
			    :value (down . left)
			    (const :tag "Do not show" (down . nil))
			    (const :tag "On the left" (down . left))
			    (const :tag "On the right" (down . right))))
	       (other :tag "On left, no arrows" t))
	      "22.1")
	     (scroll-up-aggressively windows
				     (choice (const :tag "off" nil) number)
				     "21.1")
	     (scroll-down-aggressively windows
				       (choice (const :tag "off" nil) number)
				       "21.1")
	     (line-spacing display (choice (const :tag "none" nil) integer))
	     ;; callint.c
	     (mark-even-if-inactive editing-basics boolean)
	     ;; callproc.c
	     (shell-file-name execute file)
	     (exec-path execute
			(repeat (choice (const :tag "default directory" nil)
					(directory :format "%v"))))
	     ;; coding.c
	     (inhibit-eol-conversion mule boolean)
	     (eol-mnemonic-undecided mule string)
	     (eol-mnemonic-unix mule string)
	     (eol-mnemonic-dos mule string)
	     (eol-mnemonic-mac mule string)
	     (file-coding-system-alist
	      mule
	      (alist
	       :key-type (regexp :tag "File regexp")
	       :value-type (choice
			    :value (undecided . undecided)
			    (cons :tag "Encoding/decoding pair"
				  :value (undecided . undecided)
				  (coding-system :tag "Decoding")
				  (coding-system :tag "Encoding"))
			    (coding-system :tag "Single coding system"
					   :value undecided)
			    (function :value ignore))))
	     (selection-coding-system mule coding-system)
	     ;; dired.c
	     (completion-ignored-extensions dired
					    (repeat (string :format "%v")))
	     ;; dispnew.c
	     (baud-rate display integer)
	     (inverse-video display boolean)
	     (visible-bell display boolean)
	     (no-redraw-on-reenter display boolean)
	     ;; editfns.c
	     (user-full-name mail string)
	     ;; eval.c
	     (max-specpdl-size limits integer)
	     (max-lisp-eval-depth limits integer)
	     (stack-trace-on-error debug
				   (choice (const :tag "off")
					   (repeat :menu-tag "When"
						   :value (nil)
						   (symbol :format "%v"))
					   (const :tag "always" t)))
	     (debug-on-error debug
			     (choice (const :tag "off")
				     (repeat :menu-tag "When"
					     :value (nil)
					     (symbol :format "%v"))
				     (const :tag "always" t)))
	     (debug-ignored-errors debug (repeat (choice symbol regexp)))
	     (debug-on-quit debug
			    (choice (const :tag "off")
				    (repeat :menu-tag "When"
					    :value (nil)
					    (symbol :format "%v"))
				    (const :tag "always" t)))
	     ;; fileio.c
	     (insert-default-directory minibuffer boolean)
	     (read-file-name-completion-ignore-case minibuffer boolean "22.1")
	     ;; fns.c
	     (use-dialog-box menu boolean "21.1")
	     (use-file-dialog menu boolean "22.1")
	     ;; frame.c
	     (default-frame-alist frames
	       (repeat (cons :format "%v"
			     (symbol :tag "Parameter")
			     (sexp :tag "Value"))))
	     (mouse-highlight mouse (choice (const :tag "disabled" nil)
					    (const :tag "always shown" t)
					    (other :tag "hidden by keypress" 1)))
	     ;; fringe.c
	     (overflow-newline-into-fringe fringe boolean "22.1")
	     ;; indent.c
	     (indent-tabs-mode fill boolean)
	     ;; keyboard.c
	     (meta-prefix-char keyboard character)
	     (auto-save-interval auto-save integer)
	     (auto-save-timeout auto-save (choice (const :tag "off" nil)
						  (integer :format "%v")))
	     (echo-keystrokes minibuffer number)
	     (polling-period keyboard integer)
	     (double-click-time mouse (restricted-sexp
				       :match-alternatives (integerp 'nil 't)))
	     (double-click-fuzz mouse integer)
	     (inhibit-local-menu-bar-menus menu boolean)
	     (help-char keyboard character)
	     (help-event-list keyboard (repeat (sexp :format "%v")))
	     (menu-prompting menu boolean)
	     (suggest-key-bindings keyboard (choice (const :tag "off" nil)
						    (integer :tag "time" 2)
						    (other :tag "on")))

;; This is not good news because it will use the wrong
;; version-specific directories when you upgrade.  We need
;; customization of the front of the list, maintaining the standard
;; value intact at the back.
;;; 	     (load-path environment
;;; 			(repeat (choice :tag "[Current dir?]"
;;; 					:format "%[Current dir?%] %v"
;;; 					(const :tag " current dir" nil)
;;;					(directory :format "%v"))))
	     ;; minibuf.c
	     (completion-auto-help minibuffer boolean)
	     (enable-recursive-minibuffers minibuffer boolean)
	     (history-length minibuffer
			     (choice (const :tag "Infinite" t)
				     integer))
	     (history-delete-duplicates minibuffer boolean)
	     (minibuffer-prompt-properties
	      minibuffer
	      (list
	       (checklist :inline t
			  (const :tag "Read-Only"
				 :doc "Prevent prompt from being modified"
				 :format "%t%n%h"
				 :inline t
				 (read-only t))
			  (const :tag "Don't Enter"
				 :doc "Prevent point from ever entering prompt"
				 :format "%t%n%h"
				 :inline t
				 (point-entered minibuffer-avoid-prompt)))
	       (repeat :inline t
		       :tag "Other Properties"
		       (list :inline t
			     :format "%v"
			     (symbol :tag "Property")
			     (sexp :tag "Value"))))
	      "21.1")
	     (minibuffer-auto-raise minibuffer boolean)
	     ;; options property set at end
	     (read-buffer-function minibuffer
				   (choice (const nil)
					   (function-item iswitchb-read-buffer)
					   function))
	     ;; msdos.c
	     (dos-unsupported-char-glyph display integer)
	     ;; process.c
	     (delete-exited-processes processes-basics boolean)
	     ;; syntax.c
	     (parse-sexp-ignore-comments editing-basics boolean)
	     (words-include-escapes editing-basics boolean)
	     (open-paren-in-column-0-is-defun-start editing-basics boolean
						    "21.1")
	     ;; undo.c
	     (undo-limit undo integer)
	     (undo-strong-limit undo integer)
	     (undo-outer-limit undo
			       (choice integer
				       (const :tag "No limit"
					      :format "%t\n%d"
					      :doc
					      "With this choice, \
the undo info for the current command never gets discarded.
This should only be chosen under exceptional circumstances,
since it could result in memory overflow and make Emacs crash."
					      nil))
			       "22.1")
	     ;; window.c
	     (temp-buffer-show-function windows (choice (const nil) function))
	     (display-buffer-function windows (choice (const nil) function))
	     (pop-up-frames frames boolean)
	     (pop-up-frame-function frames function)
	     (special-display-buffer-names
	      frames
	      (repeat (choice :tag "Buffer"
			      :value ""
			      (string :format "%v")
			      (cons :tag "With attributes"
				    :format "%v"
				    :value ("" . nil)
				    (string :format "%v")
				    (repeat :tag "Attributes"
					    (cons :format "%v"
						  (symbol :tag "Parameter")
						  (sexp :tag "Value")))))))
	     (special-display-regexps
	      frames
	      (repeat (choice :tag "Buffer"
			      :value ""
			      (regexp :format "%v")
			      (cons :tag "With attributes"
				    :format "%v"
				    :value ("" . nil)
				    (regexp :format "%v")
				    (repeat :tag "Attributes"
					    (cons :format "%v"
						  (symbol :tag "Parameter")
						  (sexp :tag "Value")))))))
	     (special-display-function frames function)
	     (same-window-buffer-names windows (repeat (string :format "%v")))
	     (same-window-regexps windows (repeat (regexp :format "%v")))
	     (pop-up-windows windows boolean)
	     (even-window-heights windows boolean)
	     (next-screen-context-lines windows integer)
	     (split-height-threshold windows integer)
	     (window-min-height windows integer)
	     (window-min-width windows integer)
	     (scroll-preserve-screen-position windows boolean)
	     (display-buffer-reuse-frames windows boolean "21.1")
	     ;; xdisp.c
	     (scroll-step windows integer)
	     (scroll-conservatively windows integer)
	     (scroll-margin windows integer)
	     (hscroll-margin windows integer "21.3")
	     (hscroll-step windows number "21.3")
	     (truncate-partial-width-windows display boolean)
	     (mode-line-inverse-video modeline boolean)
	     (mode-line-in-non-selected-windows modeline boolean "21.3")
	     (line-number-display-limit display
					(choice integer
						(const :tag "No limit" nil)))
	     (line-number-display-limit-width display integer)
	     (highlight-nonselected-windows display boolean)
	     (message-log-max debug (choice (const :tag "Disable" nil)
					    (integer :menu-tag "lines"
						     :format "%v")
					    (other :tag "Unlimited" t)))
	     (unibyte-display-via-language-environment mule boolean)
	     (blink-cursor-alist cursor alist "22.1")
	     ;; xfaces.c
	     (scalable-fonts-allowed display boolean)
	     ;; xfns.c
	     (x-bitmap-file-path installation
				 (repeat (directory :format "%v")))
	     (x-use-old-gtk-file-dialog menu boolean "22.1")
	     ;; xterm.c
             (mouse-autoselect-window display boolean "21.3")
	     (x-use-underline-position-properties display boolean "21.3")
	     (x-stretch-cursor display boolean "21.1")))
      this symbol group type standard version native-p
      ;; This function turns a value
      ;; into an expression which produces that value.
      (quoter (lambda (sexp)
		(if (or (memq sexp '(t nil))
			(keywordp sexp)
			(and (listp sexp)
			     (memq (car sexp) '(lambda)))
			(stringp sexp)
			(numberp sexp))
		    sexp
		  (list 'quote sexp)))))
  (while all
    (setq this (car all)
	  all (cdr all)
	  symbol (nth 0 this)
	  group (nth 1 this)
	  type (nth 2 this)
	  version (nth 3 this)
	  ;; If we did not specify any standard value expression above,
	  ;; use the current value as the standard value.
	  standard (if (nthcdr 4 this)
		       (nth 4 this)
		     (when (default-boundp symbol)
		       (funcall quoter (default-value symbol))))
	  ;; Don't complain about missing variables which are
	  ;; irrelevant to this platform.
	  native-p (save-match-data
		     (cond
		      ((string-match "\\`dos-" (symbol-name symbol))
		       (eq system-type 'ms-dos))
		      ((string-match "\\`w32-" (symbol-name symbol))
		       (eq system-type 'windows-nt))
		      ((string-match "\\`x-.*gtk" (symbol-name symbol))
		       (or (boundp 'gtk)
			   (and window-system
				(not (eq window-system 'pc))
				(not (eq system-type 'windows-nt)))))
		      ((string-match "\\`x-" (symbol-name symbol))
		       (fboundp 'x-create-frame))
		      ((string-match "selection" (symbol-name symbol))
		       (fboundp 'x-selection-exists-p))
		      ((string-match "fringe" (symbol-name symbol))
		       (fboundp 'define-fringe-bitmap))
		      (t t))))
    (if (not (boundp symbol))
	;; If variables are removed from C code, give an error here!
	(and native-p
	     (message "Note, built-in variable `%S' not bound" symbol))
      ;; Save the standard value, unless we already did.
      (or (get symbol 'standard-value)
	  (put symbol 'standard-value (list standard)))
      ;; If this is NOT while dumping Emacs,
      ;; set up the rest of the customization info.
      (unless purify-flag
	;; Add it to the right group.
	(custom-add-to-group group symbol 'custom-variable)
	;; Set the type.
	(put symbol 'custom-type type)
	(put symbol 'custom-version version)))))

(custom-add-to-group 'iswitchb 'read-buffer-function 'custom-variable)
(custom-add-to-group 'font-lock 'open-paren-in-column-0-is-defun-start
		     'custom-variable)
(put 'selection-coding-system 'custom-set
     (lambda (symbol value)
       (set-selection-coding-system value)
       (set symbol value)))

;; Record cus-start as loaded
;; if we have set up all the info that we can set up.
;; Don't record cus-start as loaded
;; if we have set up only the standard values.
(unless purify-flag
  (provide 'cus-start))

;;; arch-tag: 4502730d-bcb3-4f5e-99a3-a86f2d54af60
;;; cus-start.el ends here
