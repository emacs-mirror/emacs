;;; assistant.el --- guiding users through Emacs setup
;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: util

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'widget)
(require 'wid-edit)

(autoload 'gnus-error "gnus-util")
(autoload 'netrc-get "netrc")
(autoload 'netrc-machine "netrc")
(autoload 'netrc-parse "netrc")

(defvar assistant-readers
  '(("variable" assistant-variable-reader)
    ("validate" assistant-sexp-reader)
    ("result" assistant-list-reader)
    ("next" assistant-list-reader)
    ("text" assistant-text-reader)))

(defface assistant-field '((t (:bold t)))
  "Face used for editable fields."
  :group 'gnus-article-emphasis)
;; backward-compatibility alias
(put 'assistant-field-face 'face-alias 'assistant-field)

;;; Internal variables

(defvar assistant-data nil)
(defvar assistant-current-node nil)
(defvar assistant-previous-nodes nil)
(defvar assistant-widgets nil)

(defun assistant-parse-buffer ()
  (let (results command value)
    (goto-char (point-min))
    (while (search-forward "@" nil t)
      (if (not (looking-at "[^ \t\n]+"))
	  (error "Dangling @")
	(setq command (downcase (match-string 0)))
	(goto-char (match-end 0)))
      (setq value
	    (if (looking-at "[ \t]*\n")
		(let (start)
		  (forward-line 1)
		  (setq start (point))
		  (unless (re-search-forward (concat "^@end " command) nil t)
		    (error "No @end %s found" command))
		  (beginning-of-line)
		  (prog1
		      (buffer-substring start (point))
		    (forward-line 1)))
	      (skip-chars-forward " \t")
	      (prog1
		  (buffer-substring (point) (point-at-eol))
		(forward-line 1))))
      (push (list command (assistant-reader command value))
	    results))
    (assistant-segment (nreverse results))))

(defun assistant-text-reader (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((start (point))
	  (sections nil))
      (while (re-search-forward "@\\([^{]+\\){\\([^}]+\\)}" nil t)
	(push (buffer-substring start (match-beginning 0))
	      sections)
	(push (list (match-string 1) (match-string 2))
	      sections)
	(setq start (point)))
      (push (buffer-substring start (point-max))
	    sections)
      (nreverse sections))))

;; Segment the raw assistant data into a list of nodes.
(defun assistant-segment (list)
  (let ((ast nil)
	(node nil)
	(title (pop list)))
    (dolist (elem list)
      (when (and (equal (car elem) "node")
		 node)
	(push (list "save" nil) node)
	(push (nreverse node) ast)
	(setq node nil))
      (push elem node))
    (when node
      (push (list "save" nil) node)
      (push (nreverse node) ast))
    (cons title (nreverse ast))))

(defun assistant-reader (command value)
  (let ((formatter (cadr (assoc command assistant-readers))))
    (if (not formatter)
	value
      (funcall formatter value))))

(defun assistant-list-reader (value)
  (car (read-from-string (concat "(" value ")"))))

(defun assistant-variable-reader (value)
  (let ((section (car (read-from-string (concat "(" value ")")))))
    (append section (list 'default))))

(defun assistant-sexp-reader (value)
  (if (zerop (length value))
      nil
    (car (read-from-string value))))

(defun assistant-buffer-name (title)
  (format "*Assistant %s*" title))

(defun assistant-get (ast command)
  (cadr (assoc command ast)))

(defun assistant-set (ast command value)
  (let ((elem (assoc command ast)))
    (when elem
      (setcar (cdr elem) value))))

(defun assistant-get-list (ast command)
  (let ((result nil))
    (dolist (elem ast)
      (when (equal (car elem) command)
	(push elem result)))
    (nreverse result)))

;;;###autoload
(defun assistant (file)
  "Assist setting up Emacs based on FILE."
  (interactive "fAssistant file name: ")
  (let ((ast
	 (with-temp-buffer
	   (insert-file-contents file)
	   (assistant-parse-buffer))))
    (pop-to-buffer (assistant-buffer-name (assistant-get ast "title")))
    (assistant-render ast)))

(defun assistant-render (ast)
  (let ((first-node (assistant-get (nth 1 ast) "node")))
    (set (make-local-variable 'assistant-data) ast)
    (set (make-local-variable 'assistant-current-node) nil)
    (set (make-local-variable 'assistant-previous-nodes) nil)
    (assistant-render-node first-node)))

(defun assistant-find-node (node-name)
  (let ((ast (cdr assistant-data)))
    (while (and ast
		(not (string= node-name (assistant-get (car ast) "node"))))
      (pop ast))
    (car ast)))

(defun assistant-node-name (node)
  (assistant-get node "node"))

(defun assistant-previous-node-text (node)
  (format "<< Go back to %s" node))

(defun assistant-next-node-text (node)
  (if (and node
	   (not (eq node 'finish)))
      (format "Proceed to %s >>" node)
    "Finish"))

(defun assistant-set-defaults (node &optional forcep)
  (dolist (variable (assistant-get-list node "variable"))
    (setq variable (cadr variable))
    (when (or (eq (nth 3 variable) 'default)
	      forcep)
      (setcar (nthcdr 3 variable)
	      (assistant-eval (nth 2 variable))))))

(defun assistant-get-variable (node variable &optional type raw)
  (let ((variables (assistant-get-list node "variable"))
	(result nil)
	elem)
    (while (and (setq elem (pop variables))
		(not result))
      (setq elem (cadr elem))
      (when (eq (intern variable) (car elem))
	(if type
	    (setq result (nth 1 elem))
	  (setq result (if raw (nth 3 elem)
			 (format "%s" (nth 3 elem)))))))
    result))
    
(defun assistant-set-variable (node variable value)
  (let ((variables (assistant-get-list node "variable"))
	elem)
    (while (setq elem (pop variables))
      (setq elem (cadr elem))
      (when (eq (intern variable) (car elem))
	(setcar (nthcdr 3 elem) value)))))
    
(defun assistant-render-text (text node)
  (unless (and text node)
    (gnus-error 
     5 
     "The assistant was asked to render invalid text or node data"))
  (dolist (elem text)
    (if (stringp elem)
	;; Ordinary text
	(insert elem)
      ;; A variable to be inserted as a widget.
      (let* ((start (point))
	     (variable (cadr elem))
	     (type (assistant-get-variable node variable 'type)))
	(cond
	 ((eq (car-safe type) :radio)
	  (push
	   (apply
	    #'widget-create
	    'radio-button-choice
	    :assistant-variable variable
	    :assistant-node node
	    :value (assistant-get-variable node variable)
	    :notify (lambda (widget &rest ignore)
		      (assistant-set-variable
		       (widget-get widget :assistant-node)
		       (widget-get widget :assistant-variable)
		       (widget-value widget))
		      (assistant-render-node
		       (assistant-get
			(widget-get widget :assistant-node)
			"node")))
	    (cadr type))
	   assistant-widgets))
	 ((eq (car-safe type) :set)
	  (push
	   (apply
	    #'widget-create
	    'set
	    :assistant-variable variable
	    :assistant-node node
	    :value (assistant-get-variable node variable nil t)
	    :notify (lambda (widget &rest ignore)
		      (assistant-set-variable
		       (widget-get widget :assistant-node)
		       (widget-get widget :assistant-variable)
		       (widget-value widget))
		      (assistant-render-node
		       (assistant-get
			(widget-get widget :assistant-node)
			"node")))
	    (cadr type))
	   assistant-widgets))
	 (t
	  (push 
	   (widget-create
	    'editable-field
	    :value-face 'assistant-field
	    :assistant-variable variable
	    (assistant-get-variable node variable))
	   assistant-widgets)
	  ;; The editable-field widget apparently inserts a newline;
	  ;; remove it.
	  (delete-char -1)
	  (add-text-properties start (point)
			       (list
				'bold t
				'face 'assistant-field
				'not-read-only t))))))))

(defun assistant-render-node (node-name)
  (let ((node (assistant-find-node node-name))
	(inhibit-read-only t)
	(previous assistant-current-node)
	(buffer-read-only nil))
    (unless node
      (gnus-error 5 "The node for %s could not be found" node-name))
    (set (make-local-variable 'assistant-widgets) nil)
    (assistant-set-defaults node)
    (if (equal (assistant-get node "type") "interstitial")
	(assistant-render-node (nth 0 (assistant-find-next-nodes node-name)))
      (setq assistant-current-node node-name)
      (when previous
	(push previous assistant-previous-nodes))
      (erase-buffer)
      (insert (cadar assistant-data) "\n\n")
      (insert node-name "\n\n")
      (assistant-render-text (assistant-get node "text") node)
      (insert "\n\n")
      (when assistant-previous-nodes
	(assistant-node-button 'previous (car assistant-previous-nodes)))
      (widget-create
       'push-button
       :assistant-node node-name
       :notify (lambda (widget &rest ignore)
		 (let* ((node (widget-get widget :assistant-node)))
		   (assistant-set-defaults (assistant-find-node node) 'force)
		   (assistant-render-node node)))
       "Reset")
      (insert "\n")
      (dolist (nnode (assistant-find-next-nodes))
	(assistant-node-button 'next nnode)
	(insert "\n"))

      (goto-char (point-min))
      (assistant-make-read-only))))

(defun assistant-make-read-only ()
  (let ((start (point-min))
	end)
    (while (setq end (text-property-any start (point-max) 'not-read-only t))
      (put-text-property start end 'read-only t)
      (put-text-property start end 'rear-nonsticky t)
      (while (get-text-property end 'not-read-only)
	(incf end))
      (setq start end))
    (put-text-property start (point-max) 'read-only t)))

(defun assistant-node-button (type node)
  (let ((text (if (eq type 'next)
		  (assistant-next-node-text node)
		(assistant-previous-node-text node))))
    (widget-create
     'push-button
     :assistant-node node
     :assistant-type type
     :notify (lambda (widget &rest ignore)
	       (let* ((node (widget-get widget :assistant-node))
		      (type (widget-get widget :assistant-type)))
		 (if (eq type 'previous)
		     (progn
		       (setq assistant-current-node nil)
		       (pop assistant-previous-nodes))
		   (assistant-get-widget-values)
		   (assistant-validate))
		 (if (null node)
		     (assistant-finish)
		   (assistant-render-node node))))
     text)
    (use-local-map widget-keymap)))

(defun assistant-validate-types (node)
  (dolist (variable (assistant-get-list node "variable"))
    (setq variable (cadr variable))
    (let ((type (nth 1 variable))
	  (value (nth 3 variable)))
      (when 
	  (cond
	   ((eq type :number)
	    (string-match "[^0-9]" value))
	   (t
	    nil))
	(error "%s is not of type %s: %s"
	       (car variable) type value)))))

(defun assistant-get-widget-values ()
  (let ((node (assistant-find-node assistant-current-node)))
    (dolist (widget assistant-widgets)
      (assistant-set-variable
       node (widget-get widget :assistant-variable)
       (widget-value widget)))))

(defun assistant-validate ()
  (let* ((node (assistant-find-node assistant-current-node))
	 (validation (assistant-get node "validate"))
	 result)
    (assistant-validate-types node)
    (when validation
      (when (setq result (assistant-eval validation))
	(unless (y-or-n-p (format "Error: %s.  Continue? " result))
	  (error "%s" result))))
    (assistant-set node "save" t)))

;; (defun assistant-find-next-node (&optional node)
;;   (let* ((node (assistant-find-node (or node assistant-current-node)))
;; 	 (node-name (assistant-node-name node))
;; 	 (nexts (assistant-get-list node "next"))
;; 	 next elem applicable)

;;     (while (setq elem (pop nexts))
;;       (when (assistant-eval (car (cadr elem)))
;; 	(setq applicable (cons elem applicable))))

;;     ;; return the first thing we can
;;     (cadr (cadr (pop applicable)))))

(defun assistant-find-next-nodes (&optional node)
  (let* ((node (assistant-find-node (or node assistant-current-node)))
	 (nexts (assistant-get-list node "next"))
	 next elem applicable return)

    (while (setq elem (pop nexts))
      (when (assistant-eval (car (cadr elem)))
	(setq applicable (cons elem applicable))))

    ;; return the first thing we can
    
    (while (setq elem (pop applicable))
      (push (cadr (cadr elem)) return))

    return))

(defun assistant-get-all-variables ()
  (let ((variables nil))
    (dolist (node (cdr assistant-data))
      (setq variables
	    (append (assistant-get-list node "variable")
		    variables)))
    variables))
  
(defun assistant-eval (form)
  (let ((bindings nil))
    (dolist (variable (assistant-get-all-variables))
      (setq variable (cadr variable))
      (push (list (car variable) 
		  (if (eq (nth 3 variable) 'default)
		      nil
		    (if (listp (nth 3 variable))
			`(list ,@(nth 3 variable))
		      (nth 3 variable))))
	    bindings))
    (eval
     `(let ,bindings
	,form))))

(defun assistant-finish ()
  (let ((results nil)
	result)
    (dolist (node (cdr assistant-data))
      (when (assistant-get node "save")
	(setq result (assistant-get node "result"))
	(push (list (car result)
		    (assistant-eval (cadr result)))
	      results)))
    (message "Results: %s"
	     (nreverse results))))

;;; Validation functions.

(defun assistant-validate-connect-to-server (server port)
  (let* ((error nil)
	 (stream
	  (condition-case err
	      (open-network-stream "nntpd" nil server port)
	    (error (setq error err)))))
    (if (and (processp stream)
	     (memq (process-status stream) '(open run)))
	(progn
	  (delete-process stream)
	  nil)
      error)))

(defun assistant-authinfo-data (server port type)
  (when (file-exists-p "~/.authinfo")
    (netrc-get (netrc-machine (netrc-parse "~/.authinfo")
			      server port)
	       (if (eq type 'user)
		   "login"
		 "password"))))

(defun assistant-password-required-p ()
  nil)

(provide 'assistant)

;;; arch-tag: 0404bfa2-9226-4611-8d3f-335c2416175b
;;; assistant.el ends here
