;;; widget.el --- a library of user interface components.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; Version: 1.84
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; If you want to use this code, please visit the URL above.
;;
;; This file only contain the code needed to define new widget types.
;; Everything else is autoloaded from `wid-edit.el'.

;;; Code:

(eval-when-compile (require 'cl))

(defmacro define-widget-keywords (&rest keys)
  (`
   (eval-and-compile
     (let ((keywords (quote (, keys))))
       (while keywords
	 (or (boundp (car keywords))
	     (set (car keywords) (car keywords)))
	 (setq keywords (cdr keywords)))))))

(define-widget-keywords :text-format :deactivate :active :inactive 
  :activate :sibling-args :delete-button-args
  :insert-button-args :append-button-args :button-args 
  :tag-glyph :off-glyph :on-glyph :valid-regexp
  :secret :sample-face :sample-face-get :case-fold :widget-doc 
  :create :convert-widget :format :value-create :offset :extra-offset
  :tag :doc :from :to :args :value :value-from :value-to :action
  :value-set :value-delete :match :parent :delete :menu-tag-get
  :value-get :choice :void :menu-tag :on :off :on-type :off-type
  :notify :entry-format :button :children :buttons :insert-before
  :delete-at :format-handler :widget :value-pos :value-to-internal
  :indent :size :value-to-external :validate :error :directory
  :must-match :type-error :value-inline :inline :match-inline :greedy
  :button-face-get :button-face :value-face :keymap :entry-from
  :entry-to :help-echo :documentation-property :hide-front-space
  :hide-rear-space :tab-order) 

;; These autoloads should be deleted when the file is added to Emacs.
(unless (fboundp 'load-gc)
  (autoload 'widget-apply "wid-edit")
  (autoload 'widget-create "wid-edit")
  (autoload 'widget-insert "wid-edit")
  (autoload 'widget-browse "wid-browse" nil t)
  (autoload 'widget-browse-other-window "wid-browse" nil t)
  (autoload 'widget-browse-at "wid-browse" nil t))

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply 'widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc))

;;; The End.

(provide 'widget)

;; widget.el ends here
