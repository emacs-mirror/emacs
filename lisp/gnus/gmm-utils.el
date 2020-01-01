;;; gmm-utils.el --- Utility functions for Gnus, Message and MML

;; Copyright (C) 2006-2020 Free Software Foundation, Inc.

;; Author: Reiner Steib <reiner.steib@gmx.de>
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

;; This library provides self-contained utility functions.  The functions are
;; used in Gnus, Message and MML, but within this library there are no
;; dependencies on Gnus, Message, or MML.

;;; Code:

(defgroup gmm nil
  "Utility functions for Gnus, Message and MML."
  :prefix "gmm-"
  :version "22.1" ;; Gnus 5.10.9
  :group 'lisp)

;; Helper functions from `gnus-utils.el': gmm-verbose, gmm-message, gmm-error

(defcustom gmm-verbose 7
  "Integer that says how verbose gmm should be.
The higher the number, the more messages will flash to say what
it did.  At zero, it will be totally mute; at five, it will
display most important messages; and at ten, it will keep on
jabbering all the time."
  :type 'integer
  :group 'gmm)

;;;###autoload
(defun gmm-regexp-concat (regexp)
  "Potentially concat a list of regexps into a single one.
The concatenation is done with logical ORs."
  (cond ((null regexp)
	 nil)
	((stringp regexp)
	 regexp)
	((listp regexp)
	 (mapconcat (lambda (elt) (concat "\\(" elt "\\)"))
		    regexp
		    "\\|"))))

;;;###autoload
(defun gmm-message (level &rest args)
  "If LEVEL is lower than `gmm-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages
3 - non-serious error messages
5 - messages for things that take a long time
7 - not very important messages on stuff
9 - messages inside loops."
  (if (<= level gmm-verbose)
      (apply 'message args)
    ;; We have to do this format thingy here even if the result isn't
    ;; shown - the return value has to be the same as the return value
    ;; from `message'.
    (apply 'format args)))

;;;###autoload
(defun gmm-error (level &rest args)
  "Beep an error if LEVEL is equal to or less than `gmm-verbose'.
ARGS are passed to `message'."
  (when (<= (floor level) gmm-verbose)
    (apply 'message args)
    (ding)
    (let (duration)
      (when (and (floatp level)
		 (not (zerop (setq duration (* 10 (- level (floor level)))))))
	(sit-for duration))))
  nil)

;;;###autoload
(defun gmm-widget-p (symbol)
  "Non-nil if SYMBOL is a widget."
  (get symbol 'widget-type))

(autoload 'widget-create-child-value "wid-edit")
(autoload 'widget-convert "wid-edit")
(autoload 'widget-default-get "wid-edit")

;; Note: The format of `gmm-tool-bar-item' may change if some future Emacs
;; version will provide customizable tool bar buttons using a different
;; interface.

;; TODO: Extend API so that the "Command" entry can be a function or a plist.
;; In case of a list it should have the format...
;;
;;  (:none command-without-modifier
;;   :shift command-with-shift-pressed
;;   :control command-with-ctrl-pressed
;;   :control-shift command-with-control-and-shift-pressed
;;   ;; mouse-2 and mouse-3 can't be used in Emacs yet.
;;   :mouse-2 command-on-mouse-2-press
;;   :mouse-3 command-on-mouse-3-press) ;; typically a menu of related commands
;;
;; Combinations of mouse-[23] plus shift and/or control might be overkill.
;;
;; Then use (plist-get rs-command :none), (plist-get rs-command :shift)

(define-widget 'gmm-tool-bar-item 'lazy
  "Tool bar list item."
  :tag "Tool bar item"
  :type '(choice
	  (list :tag "Command and Icon"
		(function :tag "Command")
		(string :tag "Icon file")
		(choice
		 (const :tag "Default map" nil)
		 ;; Note: Usually we need non-nil attributes if map is t.
		 (const :tag "No menu" t)
		 (sexp :tag "Other map"))
		(plist :inline t :tag "Properties"))
	  (list :tag "Separator"
		(const :tag "No command" gmm-ignore)
		(string :tag "Icon file")
		(const :tag "No map")
		(plist :inline t :tag "Properties"))))

(define-widget 'gmm-tool-bar-zap-list 'lazy
  "Tool bar zap list."
  :tag "Tool bar zap list"
  :type '(choice (const :tag "Zap all" t)
		 (const :tag "Keep all" nil)
		 (list
		  ;; :value
		  ;; Work around (bug in customize?), see
		  ;; <news:v9is48jrj1.fsf@marauder.physik.uni-ulm.de>
		  ;; (new-file open-file dired kill-buffer write-file
		  ;; 	    print-buffer customize help)
		  (set :inline t
		       (const new-file)
		       (const open-file)
		       (const dired)
		       (const kill-buffer)
		       (const save-buffer)
		       (const write-file)
		       (const undo)
		       (const cut)
		       (const copy)
		       (const paste)
		       (const search-forward)
		       (const print-buffer)
		       (const customize)
		       (const help))
		  (repeat :inline t
			  :tag "Other"
			  (symbol :tag "Icon item")))))

(defcustom gmm-tool-bar-style
  (if (and (boundp 'tool-bar-mode)
	   tool-bar-mode
	   (memq (display-visual-class)
		 (list 'static-gray 'gray-scale
		       'static-color 'pseudo-color)))
      'gnome
    'retro)
  "Preferred tool bar style."
  :type '(choice (const :tag "GNOME style" gnome)
		 (const :tag "Retro look"  retro))
  :group 'gmm)

(defvar tool-bar-map)

;;;###autoload
(defun gmm-tool-bar-from-list (icon-list zap-list default-map)
  "Make a tool bar from ICON-LIST.

Within each entry of ICON-LIST, the first element is a menu
command, the second element is an icon file name and the third
element is a test function.  You can use \\[describe-key]
<menu-entry> to find out the name of a menu command.  The fourth
and all following elements are passed as the PROPS argument to the
function `tool-bar-local-item'.

If ZAP-LIST is a list, remove those item from the default
`tool-bar-map'.  If it is t, start with a new sparse map.  You
can use \\[describe-key] <icon> to find out the name of an icon
item.  When \\[describe-key] <icon> shows \"<tool-bar> <new-file>
runs the command find-file\", then use `new-file' in ZAP-LIST.

DEFAULT-MAP specifies the default key map for ICON-LIST."
  (let ((map (if (eq zap-list t)
		 (make-sparse-keymap)
	       (copy-keymap tool-bar-map))))
    (when (listp zap-list)
      ;; Zap some items which aren't relevant for this mode and take up space.
      (dolist (key zap-list)
	(define-key map (vector key) nil)))
    (mapc (lambda (el)
	    (let ((command (car el))
		  (icon (nth 1 el))
		  (fmap (or (nth 2 el) default-map))
		  (props  (cdr (cdr (cdr el)))) )
	      ;; command may stem from different from-maps:
	      (cond ((eq command 'gmm-ignore)
		     ;; The dummy `gmm-ignore', see `gmm-tool-bar-item'
		     ;; widget.  Suppress tooltip by adding `:enable nil'.
		     (if (fboundp 'tool-bar-local-item)
			 (apply 'tool-bar-local-item icon nil nil
				map :enable nil props)
		       ;; (tool-bar-local-item ICON DEF KEY MAP &rest PROPS)
		       ;; (tool-bar-add-item ICON DEF KEY &rest PROPS)
		       (apply 'tool-bar-add-item icon nil nil :enable nil props)))
		    ((equal fmap t) ;; Not a menu command
		     (apply 'tool-bar-local-item
			    icon command
			    (intern icon) ;; reuse icon or fmap here?
			    map props))
		    (t ;; A menu command
		     (apply 'tool-bar-local-item-from-menu
			    ;; (apply 'tool-bar-local-item icon def key
			    ;; tool-bar-map props)
			    command icon map (symbol-value fmap)
			    props)))
	      t))
	  (if (symbolp icon-list)
	      (eval icon-list)
	    icon-list))
    map))

(defmacro defun-gmm (name function arg-list &rest body)
  "Create function NAME.
If FUNCTION exists, then NAME becomes an alias for FUNCTION.
Otherwise, create function NAME with ARG-LIST and BODY."
  (let ((defined-p (fboundp function)))
    (if defined-p
        `(defalias ',name ',function)
      `(defun ,name ,arg-list ,@body))))

(defun gmm-customize-mode (&optional mode)
  "Customize customization group for MODE.
If mode is nil, use `major-mode' of the current buffer."
  (interactive)
  (customize-group
   (or mode
       (intern (let ((mode (symbol-name major-mode)))
		 (string-match "^\\(.+\\)-mode$" mode)
		 (match-string 1 mode))))))

(define-obsolete-function-alias 'gmm-format-time-string 'format-time-string
  "26.1")

(provide 'gmm-utils)

;;; gmm-utils.el ends here
