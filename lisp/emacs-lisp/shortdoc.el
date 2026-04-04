;;; shortdoc.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Keywords: lisp, help
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

;; This package lists functions based on various groupings.
;;
;; For instance, `string-trim' and `mapconcat' are `string' functions,
;; so `M-x shortdoc RET string RET' will give an overview of these and
;; other functions that operate on strings.
;;
;; The documentation groups are created with the
;; `define-short-documentation-group' macro.

;;; Code:

(require 'seq)
(require 'text-property-search)
(eval-when-compile (require 'cl-lib))

(defgroup shortdoc nil
  "Short documentation."
  :group 'lisp)

(defface shortdoc-heading
  '((t :inherit variable-pitch :height 1.3 :weight bold))
  "Face used for a heading."
  :version "28.1")

(defface shortdoc-section
  '((t :inherit variable-pitch))
  "Face used for a section.")


;; Almost all past Emacs versions (but see note on Emacs 30 below)
;; understand the following shortdoc group structure:
;;
;;  (SYMBOL                                 ;; shortdoc group name
;;   (:group [:KEYWORD VALUE ...])          ;; group properties
;;   STRING                                 ;; shortdoc section title
;;   (:section [:KEYWORD VALUE ...])        ;; section properties
;;
;;   (SYMBOL                                ;; shortdoc item
;;    [:KEYWORD VALUE ...])                 ;; item properties
;;   ([:item] FORM                          ;; generalized shortdoc item
;;    [:KEYWORD VALUE ...]))                ;; item properties
;;
;; Where:
;; - a group definition must contain at least one section title or item;
;; - group and section properties must occur at most once after the
;;   group name and a section title, respectively;
;; - the leading `:item' keyword of a generalized shortdoc item may be
;;   omitted if the shortdoc group is not intended to be used on Emacs
;;   versions older than Emacs 32;
;; - the group, secion, or item properties may be empty.
;;
;; That does not mean that any such shortdoc group is meaningful.  And
;; that does not mean that past Emacs version actually use all the bits
;; available in such a definition.  But they will not error out when
;; processing a definition with the format layed out above, they will
;; simply silently ignore those bits unknown to them (specifically
;; unknown keywords) and attempt to make the best out of the rest.
;;
;; Why is this important?  Because it gives package authors a guarantee
;; that they can use shortdoc features of newer Emacs versions without
;; older Emacs versions breaking on them.
;;
;; So Emacs developers, please
;;
;; - stick to above structure when extending shortdoc.el (so that past
;;   Emacs versions can grok your extensions without breaking); and
;;
;; - do not impose any additional restrictions on the format described
;;   above and on the allowed keywords (so that you do not limit the
;;   options of future Emacs versions).
;;
;; Emacs 30, for example, had introduced some restrictions on item
;; property keywords.  As a result, we need that hack mentioned in the
;; "boilerplate template for Emacs package authors" above.

(defun shortdoc--keyword-plist-p (object)
  "Return non-nil if OBJECT is a plist with keywords as property names."
  (let ((ok (proper-list-p object)))
    (while (and ok object)
      (setq ok (and (keywordp (car object)) (cdr object))
            object (cddr object)))
    ok))

(defun shortdoc--check (group definition)
  "Ensure that (GROUP DEFINITION) is a valid shortdoc group definition.
Signal an error if that is not the case."
  (unless (symbolp group)
    (signal 'wrong-type-argument (list 'symbolp group)))
  (unless (proper-list-p definition)
    (signal 'wrong-type-argument (list 'proper-list-p definition)))
  (let ((has-content nil)
        entry keyword type
        (prev-type 'group-name))
    (while definition
      (setq entry (car definition)
            keyword (car-safe entry)
            type (cond
                  ((and (eq keyword :group)
                        (shortdoc--keyword-plist-p (cdr entry)))
                   'group-properties)
                  ((stringp entry) 'section-title)
                  ((and (eq keyword :section)
                        (shortdoc--keyword-plist-p (cdr entry)))
                   'section-properties)
                  ((and (eq keyword :item)
                        (shortdoc--keyword-plist-p entry))
                   'item-definition)
                  ((and (consp entry)
                        (shortdoc--keyword-plist-p (cdr entry)))
                   'item-definition)
                  (t 'invalid)))
      (cond ((memq type '(section-title item-definition))
             (setq has-content t))
            ((and (eq type 'group-properties)
                  (eq prev-type 'group-name)))
            ((and (eq type 'section-properties)
                  (eq prev-type 'section-title)))
            (t
             (error "Shortdoc group %s with invalid entry %S"
                              group entry)))
      (setq prev-type type
            definition (cdr definition)))
    (unless has-content
      (error "Shortdoc group %s without content" group))))

;;;###autoload
(defvar shortdoc--groups nil)

;;;###autoload
(defmacro define-short-documentation-group (group &rest functions)
  "Add GROUP to the list of defined documentation groups.
FUNCTIONS is a list of elements on the form:

  (FUNC
   :no-manual BOOL
   :args ARGS
   :eval EVAL
   :no-eval EXAMPLE-FORM
   :no-value EXAMPLE-FORM
   :no-eval* EXAMPLE-FORM
   :result RESULT-FORM
   :result-string RESULT-STRING
   :eg-result RESULT-FORM
   :eg-result-string RESULT-STRING)

FUNC is the function being documented.

NO-MANUAL should be non-nil if FUNC isn't documented in the
manual.

ARGS is optional list of function FUNC's arguments.  FUNC's
signature is displayed automatically if ARGS is not present.
Specifying ARGS might be useful where you don't want to document
some of the uncommon arguments a function might have.

While the `:no-manual' and `:args' property can be used for
any (FUNC ..) form, all of the other properties shown above
cannot be used simultaneously in such a form.

Here are some common forms with examples of properties that go
together:

1. Document a form or string, and its evaluated return value.
   (FUNC
    :eval EVAL)

If EVAL is a string, it will be inserted as is, and then that
string will be `read' and evaluated.

2. Document a form or string, but manually document its evaluation
   result.  The provided form will not be evaluated.

  (FUNC
   :no-eval EXAMPLE-FORM
   :result RESULT-FORM)   ;Use `:result-string' if value is in string form

Using `:no-value' is the same as using `:no-eval'.

Use `:no-eval*' instead of `:no-eval' where the successful
execution of the documented form depends on some conditions.

3. Document a form or string EXAMPLE-FORM.  Also manually
   document an example result.  This result could be unrelated to
   the documented form.

  (FUNC
   :no-eval EXAMPLE-FORM
   :eg-result RESULT-FORM) ;Use `:eg-result-string' if value is in string form

A FUNC form can have any number of `:no-eval' (or `:no-value'),
`:no-eval*', `:result', `:result-string', `:eg-result' and
`:eg-result-string' properties."
  (declare (indent defun))
  (let ((err
         (condition-case err
             (progn (shortdoc--check group functions) nil)
           (error err)))
        (exp
         `(progn
            (setq shortdoc--groups (delq (assq ',group shortdoc--groups)
                                         shortdoc--groups))
            (push (cons ',group ',functions) shortdoc--groups))))
    (if (null err)
        exp
      (macroexp-warn-and-return
       (error-message-string err) exp nil t))))

;; FIXME: As long as we do not have a better mechanism to load shortdoc
;; definitions on demand, we must require `shortdoc-doc' after above
;; macro to avoid loading cycles.  But at least we do not require
;; `shortdoc-doc' while compiling this file, only when loading it.
(if t (require 'shortdoc-doc))


;;;###autoload
(defun shortdoc-display-group (group &optional function same-window)
  "Pop to a buffer with short documentation summary for functions in GROUP.
Interactively, prompt for GROUP.
If FUNCTION is non-nil, place point on the entry for FUNCTION (if any).
If SAME-WINDOW, don't pop to a new window."
  (interactive (list (completing-read
                      "Group of functions for which to show summary: "
                      (mapcar #'car shortdoc--groups))))
  (when (stringp group)
    (setq group (intern group)))
  (unless (assq group shortdoc--groups)
    (error "No such documentation group %s" group))
  (let ((buf (get-buffer-create (format "*Shortdoc %s*" group))))
    (shortdoc--insert-group-in-buffer group buf)
    (funcall (if same-window
                 #'pop-to-buffer-same-window
               #'pop-to-buffer)
             buf))
  (goto-char (point-min))
  (when function
    (text-property-search-forward 'shortdoc-function function t)
    (beginning-of-line)))

;;;###autoload
(defalias 'shortdoc #'shortdoc-display-group)

(defun shortdoc--insert-group-in-buffer (group &optional buf)
  "Insert a short documentation summary for functions in GROUP in buffer BUF.
BUF defaults to the current buffer if nil or omitted."
  (with-current-buffer (or buf (current-buffer))
    (let ((inhibit-read-only t)
          (prev nil))
      (erase-buffer)
      (shortdoc-mode)
      (button-mode)
      (mapc
       (lambda (data)
         (cond
          ((stringp data)
           (setq prev nil)
           (unless (bobp)
             (insert "\n"))
           (insert (propertize
                    (substitute-command-keys data)
                    'face 'shortdoc-heading
                    'shortdoc-section t
                    'outline-level 1))
           (insert (propertize
                    "\n\n"
                    'face 'shortdoc-heading
                    'shortdoc-section t)))
          ;; There may be functions not yet defined in the data.
          ((fboundp (car data))
           (when prev
             (insert (make-separator-line)
                     ;; This helps with hidden outlines (bug#53981)
                     (propertize "\n" 'face '(:height 0))))
           (setq prev t)
           (shortdoc--display-function data))))
       (cdr (assq group shortdoc--groups))))))

(defun shortdoc--display-function (data)
  (let ((function (pop data))
        (start-section (point))
        arglist-start)
    ;; Function calling convention.
    (insert (propertize "(" 'shortdoc-function function 'outline-level 2))
    (if (plist-get data :no-manual)
        (insert-text-button
         (symbol-name function)
         'face 'button
         'action (lambda (_)
                   (describe-function function))
         'follow-link t
         'help-echo "mouse-1, RET: describe function")
      (insert-text-button
       (symbol-name function)
       'face 'button
       'action (lambda (_)
                 (info-lookup-symbol function 'emacs-lisp-mode))
       'follow-link t
       'help-echo "mouse-1, RET: show \
function's documentation in the Info manual"))
    (setq arglist-start (point))
    (insert ")\n")
    ;; Doc string.
    (insert "  "
            (or (plist-get data :doc)
                (car (split-string (or (documentation function)
                                       "Error: missing docstring.")
                                   "\n"))))
    (insert "\n")
    (add-face-text-property start-section (point) 'shortdoc-section t)
    (let ((print-escape-newlines t)
          (double-arrow (if (char-displayable-p ?⇒)
                            "⇒"
                          "=>"))
          (single-arrow (if (char-displayable-p ?→)
                            "→"
                          "->"))
          (start-example (point)))
      (cl-loop for (type value) on data by #'cddr
               do
               (cl-case type
                 (:eval
                  (insert "  ")
                  (if (stringp value)
                      (insert value)
                    (prin1 value (current-buffer)))
                  (insert "\n    " double-arrow " ")
                  (let ((expr (if (stringp value)
                                  (car (read-from-string value))
                                value)))
                    (prin1 (eval expr) (current-buffer)))
                    (insert "\n"))
                 (:no-eval*
                  (if (stringp value)
                      (insert "  " value "\n")
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n    " single-arrow " "
                          (propertize "[it depends]"
                                      'face 'shortdoc-section)
                          "\n"))
                 (:no-value
                  (if (stringp value)
                      (insert "  " value)
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n"))
                 (:no-eval
                  (if (stringp value)
                      (insert "  " value)
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n"))
                 (:result
                  (insert "    " double-arrow " ")
                  (prin1 value (current-buffer))
                  (insert "\n"))
                 (:result-string
                  (insert "    " double-arrow " ")
                  (princ value (current-buffer))
                  (insert "\n"))
                 (:eg-result
                  (insert "    e.g. " double-arrow " ")
                  (prin1 value (current-buffer))
                  (insert "\n"))
                 (:eg-result-string
                  (insert "    e.g. " double-arrow " ")
                  (princ value (current-buffer))
                  (insert "\n"))))
      (add-text-properties start-example (point) `(shortdoc-example ,function)))
    ;; Insert the arglist after doing the evals, in case that's pulled
    ;; in the function definition.
    (save-excursion
      (goto-char arglist-start)
      (dolist (param (or (plist-get data :args)
                         (help-function-arglist function t)))
        (insert " " (symbol-name param)))
      (add-face-text-property arglist-start (point) 'shortdoc-section t))))

(defun shortdoc-function-examples (function)
  "Return all shortdoc examples for FUNCTION.
The result is an alist with items of the form (GROUP . EXAMPLES),
where GROUP is a shortdoc group where FUNCTION appears, and
EXAMPLES is a string with the usage examples of FUNCTION defined
in GROUP.  Return nil if FUNCTION is not a function or if it
doesn't has any shortdoc information."
  (let ((groups (and (symbolp function)
                     (shortdoc-function-groups function)))
        (examples nil))
    (mapc
     (lambda (group)
       (with-temp-buffer
         (shortdoc--insert-group-in-buffer group)
         (goto-char (point-min))
         (let ((match (text-property-search-forward
                       'shortdoc-example function t)))
           (push `(,group . ,(string-trim
                              (buffer-substring-no-properties
                               (prop-match-beginning match)
                               (prop-match-end match))))
                 examples))))
     groups)
    examples))

(defun shortdoc-help-fns-examples-function (function)
  "Insert Emacs Lisp examples for FUNCTION into the current buffer.
You can add this function to the `help-fns-describe-function-functions'
hook to show examples of using FUNCTION in *Help* buffers produced
by \\[describe-function]."
  (let* ((examples (shortdoc-function-examples function))
         (num-examples (length examples))
         (times 0))
    (dolist (example examples)
      (when (zerop times)
        (if (> num-examples 1)
            (insert "\n  Examples:\n\n")
          ;; Some functions have more than one example per group.
          ;; Count the number of arrows to know if we need to
          ;; pluralize "Example".
          (let* ((text (cdr example))
                 (count 0)
                 (pos 0)
                 (end (length text))
                 (double-arrow (if (char-displayable-p ?⇒)
                                   "    ⇒"
                                 "    =>"))
                 (double-arrow-example (if (char-displayable-p ?⇒)
                                           "    e.g. ⇒"
                                         "    e.g. =>"))
                 (single-arrow (if (char-displayable-p ?→)
                                   "    →"
                                 "    ->")))
            (while (and (< pos end)
                        (or (string-match double-arrow text pos)
                            (string-match double-arrow-example text pos)
                            (string-match single-arrow text pos)))
              (setq count (1+ count)
                    pos (match-end 0)))
            (if (> count 1)
                (insert "\n  Examples:\n\n")
              (insert "\n  Example:\n\n")))))
      (setq times (1+ times))
      (insert "  ")
      (insert (cdr example))
      (insert "\n\n"))))

(defun shortdoc-function-groups (function)
  "Return all shortdoc groups FUNCTION appears in."
  (cl-loop for group in shortdoc--groups
           when (assq function (cdr group))
           collect (car group)))

(defun shortdoc-add-function (group section elem)
  "Add ELEM to shortdoc GROUP in SECTION.
If GROUP doesn't exist, it will be created.
If SECTION doesn't exist, it will be added.

ELEM is a Lisp form.  See `define-short-documentation-group' for
details.

Example:

  (shortdoc-add-function
    \\='file \"Predicates\"
    \\='(file-locked-p :no-eval (file-locked-p \"/tmp\")))"
  ;; Rely on `shortdoc--check' checking GROUP.
  (unless (stringp section)
    (signal 'wrong-type-argument (list 'stringp section)))
  (shortdoc--check group (list section elem))
  (let ((glist (assq group shortdoc--groups)))
    (unless glist
      (setq glist (list group))
      (push glist shortdoc--groups))
    (let ((slist (member section glist)))
      (unless slist
        (setq slist (list section))
        (nconc glist slist))
      (while (and (cdr slist)
                  (not (stringp (cadr slist))))
        (setq slist (cdr slist)))
      (setcdr slist (cons elem (cdr slist))))))

(defvar-keymap shortdoc-mode-map
  :doc "Keymap for `shortdoc-mode'."
  "n"       #'shortdoc-next
  "p"       #'shortdoc-previous
  "N"       #'shortdoc-next-section
  "P"       #'shortdoc-previous-section
  "C-c C-n" #'shortdoc-next-section
  "C-c C-p" #'shortdoc-previous-section
  "w"       #'shortdoc-copy-function-as-kill)

(define-derived-mode shortdoc-mode special-mode "shortdoc"
  "Mode for shortdoc."
  :interactive nil
  (setq-local outline-search-function #'outline-search-level
              outline-level (lambda ()
                              (get-text-property (point) 'outline-level))))

(defun shortdoc--goto-section (arg sym &optional reverse)
  (unless (natnump arg)
    (setq arg 1))
  (while (> arg 0)
    (funcall
     (if reverse 'text-property-search-backward
       'text-property-search-forward)
     sym nil t)
    (setq arg (1- arg))))

(defun shortdoc-next (&optional arg)
  "Move point to the next function.
With prefix numeric argument ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-function))

(defun shortdoc-previous (&optional arg)
  "Move point to the previous function.
With prefix numeric argument ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-function t)
  (backward-char 1))

(defun shortdoc-next-section (&optional arg)
  "Move point to the next section.
With prefix numeric argument ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-section))

(defun shortdoc-previous-section (&optional arg)
  "Move point to the previous section.
With prefix numeric argument ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-section t)
  (forward-line -2))

(defun shortdoc-copy-function-as-kill ()
  "Copy name of the function near point into the kill ring."
  (interactive)
  (save-excursion
    (goto-char (pos-bol))
    (when-let* ((re (rx bol "(" (group (+ (not (in " )"))))))
                (string
                 (and (or (looking-at re)
                          (re-search-backward re nil t))
                      (match-string 1))))
      (set-text-properties 0 (length string) nil string)
      (kill-new string)
      (message string))))

(provide 'shortdoc)

;;; shortdoc.el ends here
