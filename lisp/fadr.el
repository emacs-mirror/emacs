;;; fadr.el --- convinient access to recursive list structures

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author: Dmitry Dzhus <dima@sphinx.net.ru>
;; Keywords: lisp, internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code allows accessing data stored in recursive association and
;; plain lists using a compact notation.
;;
;; Consider the following list:
;; 
;;     (setq basket '((apples . (((color . green) (taste . delicious)) ((color . red) (taste . disgusting))))))
;;
;; Its contents may be accessed using `fadr-member':
;;
;;     (fadr-member basket ".apples[1].color")
;;     red
;;
;; Associated values are selected using a dot followed by a key, while
;; lists accept an index (0-based) in square brackets.
;;
;; `fadr-q' is a one-argument shortcut fro `fadr-member', where
;; (fadr-q "res.path") results to (fadr-member res ".path"):
;;
;;     (fadr-q "basket.apples[0].taste")
;;     delicious
;;
;; `fadr-expand' substitutes ~PATH with results of `fadr-member' calls
;; with respective arguments:
;;
;;     (fadr-expand "~.color apple is ~.taste" (fadr-member basket ".apples[0]"))
;;     "green apple is delicious"
;;
;; `fadr-format' is like `fadr-expand', but it performs %-substitutions first:
;;
;;     (fadr-format "%s #%d is ~.color and ~.taste" (fadr-member basket ".apples[1]") "Apple" 1)
;;     "Apple #1 is red and disgusting"

;;; Code:

(defun fadr-get-field-value (field object)
  "Get value of FIELD from OBJECT.

FIELD is a symbol."
  (cdr (assoc field object)))

(defsubst bol-regexp (regexp)
  (concat "^" regexp))
(defconst fadr-field-name-regexp
  "[[:alpha:]_-]+")
(defconst fadr-field-selector-regexp
  (concat "\\.\\(" fadr-field-name-regexp "\\)"))
(defconst fadr-index-selector-regexp
  "\\[\\([[:digit:]]+\\)\\]")
(defconst fadr-path-regexp
  (concat "\\(" fadr-field-selector-regexp "\\|"
          fadr-index-selector-regexp
          "\\)+"))

(defmacro fadr-define-select (name regexp &optional doc filter)
  "Define a function NAME of one string argument which will
  extract data from it using the first subgroup in REGEXP. If
  FILTER is specified, it will be called with the resulting
  string."
  `(defun ,name (path)
     ,doc
     (let ((string (if (string-match ,regexp path)
                       (match-string-no-properties 1 path)
                     nil)))
       (if string
           ,(if filter
                `(funcall ,filter string)
              'string)
         nil))))

(fadr-define-select fadr-index-select
  (bol-regexp fadr-index-selector-regexp)
  "Extract name of the next field selected in PATH as a symbol."
  'string-to-number)

;; Bad case: (fadr-field-select ".nil")
(fadr-define-select fadr-field-select
  (bol-regexp fadr-field-selector-regexp)
  "Extract value of the next list index selected in PATH as a
  number."
  'intern)

;; TODO: define this function using macros to ease the adding of new
;; selector types
(defun fadr-member (object path)
  "Access data in OBJECT using PATH.

This function is not match-safe, meaning that you may need to
wrap a call to it with `save-match-data'."
  (if (string= path "")
      object
    (let ((index (fadr-index-select path))
          (field (fadr-field-select path)))
      (cond (index
             (fadr-member (elt object index)
                                    (fadr-peel-path path)))
            (field
             (fadr-member (fadr-get-field-value field object)
                                    (fadr-peel-path path)))
            (t (error "Bad path"))))))

(defun fadr-q (full-path)
  (catch 'bad-path
    (if (string-match fadr-path-regexp full-path)
        (if (not (= (match-beginning 0) 0))
            (let ((object (eval (intern (substring full-path 0 (match-beginning 0)))))
                  (path (substring full-path (match-beginning 0))))
              (fadr-member object path))
          (throw 'bad-path (error "No object specified")))
      (throw 'bad-path (error "Incorrect path")))))

(defun fadr-peel-path (path)
  "Return PATH without first selector."
  (cond ((fadr-field-select path)
         (string-match (bol-regexp fadr-field-selector-regexp) path))
        ((fadr-index-select path)
         (string-match (bol-regexp fadr-index-selector-regexp) path))
        (t (error "Could not peel path")))
  (substring path (match-end 0)))

(defun fadr-expand (string object)
  "Format STRING using OBJECT members.

All ~.<path> substrings within STRING are replaced with
respective values of OBJECT members."
  (replace-regexp-in-string
   (concat "~\\(" fadr-path-regexp "\\)")
   #'(lambda (text)
       (save-match-data
         (format "%s"
                 (fadr-member object (substring text 1)))))
   string))

(defun fadr-format (string object &rest objects)
  "Format STRING with OBJECTS, then `fadr-expand' the result with
OBJECT."
  (let ((new-string (apply 'format (append (list string) objects))))
    (fadr-expand new-string object)))

(provide 'fadr)
;;; fadr.el ends here
