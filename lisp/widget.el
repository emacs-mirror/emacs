;;; widget.el --- a library of user interface components  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 1996-2026 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; URL: http://www.dina.kvl.dk/~abraham/custom/
;; Package: emacs

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
;;
;; The widget library is partially documented in the `widget' Info
;; file.
;;
;; This file only contains the code needed to define new widget types.
;; Everything else is autoloaded from `wid-edit.el'.

;;; Code:

(defmacro define-widget-keywords (&rest _keys)
  (declare (obsolete nil "27.1") (indent defun))
  nil)

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply #\\='widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (declare (doc-string 3) (indent defun))
  (unless (or (null doc) (stringp doc))
    (error "Widget documentation must be nil or a string"))
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc)
  name)

(define-obsolete-function-alias 'widget-plist-member #'plist-member "26.1")

(provide 'widget)

;;; widget.el ends here
