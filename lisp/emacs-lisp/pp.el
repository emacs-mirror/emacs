;;; pp.el --- pretty printer for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 1989, 1993, 2001-2026 Free Software Foundation, Inc.

;; Author: Randal Schwartz <merlyn@stonehenge.com>
;; Keywords: lisp

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

;;; Code:

(require 'cl-lib)

(defgroup pp nil
  "Pretty printer for Emacs Lisp."
  :prefix "pp-"
  :group 'lisp)

(defcustom pp-escape-newlines t
  "Value of `print-escape-newlines' used by pp-* functions."
  :type 'boolean)

(defcustom pp-max-width t
  "Max width to use when formatting.
If nil, there's no max width.  If t, use the window width.
Otherwise this should be a number."
  :type '(choice (const :tag "none" nil)
                 (const :tag "window width" t)
                 number)
  :version "29.1")

(defcustom pp-use-max-width nil
  "If non-nil, `pp'-related functions will try to fold lines.
The target width is given by the `pp-max-width' variable.
Note that this could slow down `pp' considerably when formatting
large lists."
  :type 'boolean
  :version "29.1")
(make-obsolete-variable 'pp-use-max-width 'pp-default-function "30.1")

(defcustom pp-default-function #'pp-fill
  ;; FIXME: The best pretty printer to use depends on the use-case
  ;; so maybe we should allow callers to specify what they want (maybe with
  ;; options like `fast', `compact', `code', `data', ...) and these
  ;; can then be mapped to actual pretty-printing algorithms.
  ;; Then again, callers can just directly call the corresponding function.
  "Function that `pp' should dispatch to for pretty printing.
That function can be called in one of two ways:
- with a single argument, which it should insert and pretty-print at point.
- with two arguments which delimit a region containing Lisp sexps
  which should be pretty-printed.
In both cases, the function can presume that the buffer is setup for
Lisp syntax."
  :type '(choice
          (const :tag "Fit within `fill-column'" pp-fill)
          (const :tag "Emacs<29 algorithm, fast and good enough" pp-28)
          (const :tag "Work hard for code (slow on large inputs)"
                 pp-emacs-lisp-code)
          (const :tag "Work hard for code if `pp-use-max-width' non-nil, else as in Emacs<29"
                 pp-29)
          (function :tag "Custom function"))
  :version "30.1")

(defvar pp--inhibit-function-formatting nil)

;; There are basically two APIs for a pretty-printing function:
;;
;; - either the function takes an object (and prints it in addition to
;;   prettifying it).
;; - or the function takes a region containing an already printed object
;;   and prettifies its content.
;;
;; `pp--object' and `pp--region' are helper functions to convert one
;; API to the other.


(defun pp--object (object region-function)
  "Pretty-print OBJECT at point.
The prettifying is done by REGION-FUNCTION which is
called with two positions as arguments and should fold lines
within that region.  Returns the result as a string."
  (let ((print-escape-newlines pp-escape-newlines)
        (print-quoted t)
        (beg (point)))
    ;; FIXME: In many cases it would be preferable to use `cl-prin1' here.
    (prin1 object (current-buffer))
    (funcall region-function beg (point))))

(defun pp--region (beg end object-function)
  "Pretty-print the object(s) contained within BEG..END.
OBJECT-FUNCTION is called with a single object as argument
and should pretty print it at point into the current buffer."
  (save-excursion
    (with-restriction beg end
      (goto-char (point-min))
      (while
          (progn
            ;; We'll throw away all the comments within objects, but let's
            ;; try at least to preserve the comments between objects.
            (forward-comment (point-max))
            (let ((beg (point))
                  (object (ignore-error end-of-buffer
                              (list (read (current-buffer))))))
              (when (consp object)
                (delete-region beg (point))
                (funcall object-function (car object))
                t)))))))

(defun pp-29 (beg-or-sexp &optional end) ;FIXME: Better name?
  "Prettify the current region with printed representation of a Lisp object.
Uses the pretty-printing algorithm that was standard in Emacs 29,
which, depending on `pp-use-max-width', will either use `pp-28'
or `pp-emacs-lisp-code'."
  (if pp-use-max-width
      (let ((pp--inhibit-function-formatting t)) ;FIXME: Why?
        (pp-emacs-lisp-code beg-or-sexp end))
    (pp-28 beg-or-sexp end)))

;;;###autoload
(defun pp-to-string (object &optional pp-function)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible.
Optional argument PP-FUNCTION overrides `pp-default-function'."
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (funcall (or pp-function pp-default-function) object)
    ;; Preserve old behavior of (usually) finishing with a newline.
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun pp--within-fill-column-p ()
  "Return non-nil if point is within `fill-column'."
  ;; Try and make it O(fill-column) rather than O(current-column),
  ;; so as to avoid major slowdowns on long lines.
  ;; FIXME: This doesn't account for invisible text or `display' properties :-(
  (and (save-excursion
         (re-search-backward
          "^\\|\n" (max (point-min) (- (point) fill-column)) t))
       (<= (current-column) fill-column)))

(defun pp-fill (beg &optional end)
  "Break lines in Lisp code between BEG and END so it fits within `fill-column'.
Presumes the current buffer has syntax and indentation properly
configured for that.
Designed under the assumption that the region occupies a single line,
tho it should also work if that's not the case.
Can also be called with a single argument, in which case
it inserts and pretty-prints that arg at point."
  (interactive "r")
  (if (null end) (pp--object beg #'pp-fill)
    (goto-char beg)
    (let* ((end (copy-marker end t))
           (avoid-unbreakable
            (lambda ()
              (and (memq (char-before) '(?# ?s ?f))
                   (memq (char-after) '(?\[ ?\())
                   (looking-back "#[sf]?" (- (point) 2))
                   (goto-char (match-beginning 0)))))
           (newline (lambda ()
                      (skip-chars-forward ")]}")
                      (unless (save-excursion (skip-chars-forward " \t") (eolp))
                        (funcall avoid-unbreakable)
                        (insert "\n")
                        (indent-according-to-mode)))))
      (while (progn (forward-comment (point-max))
                    (< (point) end))
        (let ((beg (point))
              ;; Whether we're in front of an element with paired delimiters.
              ;; Can be something funky like #'(lambda ..) or ,'#s(...)
              ;; Or also #^[..].
              (paired (when (looking-at "['`,#]*[[:alpha:]^]*\\([({[\"]\\)")
                        (match-beginning 1))))
          ;; Go to the end of the sexp.
          (goto-char (or (scan-sexps (or paired (point)) 1) end))
          (unless
              (and
               ;; The sexp is all on a single line.
               (save-excursion (not (search-backward "\n" beg t)))
               ;; And its end is within `fill-column'.
               (or (pp--within-fill-column-p)
                   ;; If the end of the sexp is beyond `fill-column',
                   ;; try to move the sexp to its own line.
                   (and
                    (save-excursion
                      (goto-char beg)
                      ;; We skip backward over open parens because cutting
                      ;; the line right after an open paren does not help
                      ;; reduce the indentation depth.
                      ;; Similarly, we prefer to cut before a "." than after
                      ;; it because it reduces the indentation depth.
                      (while
                          (progn
                            (funcall avoid-unbreakable)
                            (let ((pos (point)))
                              (skip-chars-backward " \t({[',.")
                              (while (and (memq (char-after) '(?\. ?\{))
                                          (not (memq (char-before)
                                                     '(nil ?\n ?\) \" ?\]))))
                                ;; `.' and `{' within symbols?  (Bug#76715)
                                (forward-char 1))
                              (not (eql pos (point))))))
                      (if (bolp)
                          ;; The sexp already starts on its own line.
                          (progn (goto-char beg) nil)
                        (setq beg (copy-marker beg t))
                        (if paired (setq paired (copy-marker paired t)))
                        ;; We could try to undo this insertion if it
                        ;; doesn't reduce the indentation depth, but I'm
                        ;; not sure it's worth the trouble.
                        (insert "\n") (indent-according-to-mode)
                        t))
                    ;; Check again if we moved the whole exp to a new line.
                    (pp--within-fill-column-p))))
            ;; The sexp is spread over several lines, and/or its end is
            ;; (still) beyond `fill-column'.
            (when (and paired (not (eq ?\" (char-after paired))))
              ;; The sexp has sub-parts, so let's try and spread
              ;; them over several lines.
              (save-excursion
                (goto-char beg)
                (when (looking-at "(\\([^][()\" \t\n;']+\\)")
                  ;; Inside an expression of the form (SYM ARG1
                  ;; ARG2 ... ARGn) where SYM has a `lisp-indent-function'
                  ;; property that's a number, insert a newline after
                  ;; the corresponding ARGi, because it tends to lead to
                  ;; more natural and less indented code.
                  (let* ((sym (intern-soft (match-string 1)))
                         (lif (and sym (get sym 'lisp-indent-function))))
                    (if (eq lif 'defun) (setq lif 2))
                    (when (natnump lif)
                      (goto-char (match-end 0))
                      ;; Do nothing if there aren't enough args.
                      (ignore-error scan-error
                        (forward-sexp lif)
                        (funcall newline))))))
              (save-excursion
                (pp-fill (1+ paired) (1- (point)))))
            ;; Now the sexp either ends beyond `fill-column' or is
            ;; spread over several lines (or both).  Either way, the
            ;; rest of the line should be moved to its own line.
            (funcall newline)))))))

;;;###autoload
(defun pp-buffer ()
  "Prettify the current buffer with printed representation of a Lisp object."
  (interactive)
  ;; The old code used `indent-sexp' which mostly works "anywhere",
  ;; so let's make sure we also work right in buffers that aren't
  ;; setup specifically for Lisp.
  (if (and (eq (syntax-table) emacs-lisp-mode-syntax-table)
           (eq indent-line-function #'lisp-indent-line))
      (funcall pp-default-function (point-min) (point-max))
    (with-syntax-table emacs-lisp-mode-syntax-table
        (let ((indent-line-function #'lisp-indent-line))
          (funcall pp-default-function (point-min) (point-max)))))
  ;; Preserve old behavior of (usually) finishing with a newline and
  ;; with point at BOB.
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (goto-char (point-min)))

(defun pp-28 (beg &optional end)        ;FIXME: Better name?
  "Prettify the current region with printed representation of a Lisp object.
Uses the pretty-printing algorithm that was standard before Emacs 30.
Non-interactively can also be called with a single argument, in which
case that argument will be inserted pretty-printed at point."
  (interactive "r")
  (if (null end) (pp--object beg #'pp-29)
    (with-restriction beg end
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((ignore-errors (down-list 1) t)
          (save-excursion
            (backward-char 1)
            (skip-chars-backward "'`#^")
            (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
              (delete-region
               (point)
               (progn (skip-chars-backward " \t\n") (point)))
              (insert "\n"))))
         ((ignore-errors (up-list 1) t)
          (skip-syntax-forward ")")
          (delete-region
           (point)
           (progn (skip-chars-forward " \t\n") (point)))
          (insert ?\n))
         (t (goto-char (point-max)))))
      (goto-char (point-min))
      (indent-sexp))))

;;;###autoload
(defun pp (object &optional stream)
  "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.

Uses the pretty-printing code specified in `pp-default-function'.

Output stream is STREAM, or value of `standard-output' (which see)."
  (let ((stream (or stream standard-output)))
    (cond
     ((and (eq stream (current-buffer))
           ;; Make sure the current buffer is setup sanely.
           (eq (syntax-table) emacs-lisp-mode-syntax-table)
           (eq indent-line-function #'lisp-indent-line))
      ;; Skip the buffer->string->buffer middle man.
      (funcall pp-default-function object)
      ;; Preserve old behavior of (usually) finishing with a newline.
      (unless (bolp) (insert "\n")))
     (t
      (save-current-buffer
        (when (bufferp stream) (set-buffer stream))
        (let ((begin (point))
              (cols (current-column)))
          (princ (pp-to-string object) (or stream standard-output))
          (when (and (> cols 0) (bufferp stream))
            (indent-rigidly begin (point) cols))))))))

;;;###autoload
(defun pp-display-expression (expression out-buffer-name &optional lisp)
  "Prettify and display EXPRESSION in an appropriate way, depending on length.
If LISP, format with `pp-emacs-lisp-code'; use `pp' otherwise.

If a temporary buffer is needed for representation, it will be named
after OUT-BUFFER-NAME."
  (let* ((lexical lexical-binding)
         (old-show-function temp-buffer-show-function)
	 ;; Use this function to display the buffer.
	 ;; This function either decides not to display it at all
	 ;; or displays it in the usual way.
	 (temp-buffer-show-function
          (lambda (buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (end-of-line 1)
              (if (or (< (1+ (point)) (point-max))
                      (>= (- (point) (point-min)) (frame-width)))
                  (let ((temp-buffer-show-function old-show-function)
                        (old-selected (selected-window))
                        (window (display-buffer buf)))
                    (goto-char (point-min)) ; expected by some hooks ...
                    (make-frame-visible (window-frame window))
                    (unwind-protect
                        (progn
                          (select-window window)
                          (run-hooks 'temp-buffer-show-hook))
                      (when (window-live-p old-selected)
                        (select-window old-selected))))
                (message "%s" (buffer-substring (point-min) (point))))))))
    (with-output-to-temp-buffer out-buffer-name
      (if lisp
          (with-current-buffer standard-output
            (pp-emacs-lisp-code expression))
        (pp expression))
      (with-current-buffer standard-output
	(emacs-lisp-mode)
        (setq lexical-binding lexical)
	(setq buffer-read-only nil)
        (setq-local font-lock-verbose nil)))))

(defun pp-insert-short-sexp (sexp &optional width)
  "Insert a short description of SEXP in the current buffer.
WIDTH is the maximum width to use for it and it defaults to the
space available between point and the window margin."
  (let ((printed (format "%S" sexp)))
    (if (and (not (string-search "\n" printed))
	     (<= (string-width printed)
	         (or width (- (window-width) (current-column)))))
	(insert printed)
      (insert-text-button
       "[Show]"
       'follow-link t
       'action (lambda (&rest _ignore)
                 ;; FIXME: Why "eval output"?
                 (pp-display-expression sexp "*Pp Eval Output*"))
       'help-echo "mouse-2, RET: pretty print value in another buffer"))))

;;;###autoload
(defun pp-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print its value.
Also add the value to the front of the list in the variable `values'."
  (interactive
   (list (read--expression "Eval: ")))
  (message "Evaluating...")
  (let ((result (eval expression lexical-binding)))
    (values--store-value result)
    (pp-display-expression result "*Pp Eval Output*" pp-use-max-width)))

;;;###autoload
(defun pp-macroexpand-expression (expression)
  "Macroexpand EXPRESSION and pretty-print its value."
  (interactive
   (list (read--expression "Macroexpand: ")))
  (pp-display-expression (macroexpand-1 expression) "*Pp Macroexpand Output*"
                         pp-use-max-width))

(defun pp-last-sexp ()
  "Read sexp before point.  Ignore leading comment characters."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((pt (point)))
      (save-excursion
        (forward-sexp -1)
        ;; Make `pp-eval-last-sexp' work the same way `eval-last-sexp'
        ;; does.
        (when (looking-at ",@?")
          (goto-char (match-end 0)))
        (read
         ;; If first line is commented, ignore all leading comments:
         (if (save-excursion (beginning-of-line) (looking-at-p "[ \t]*;"))
             (let ((exp (buffer-substring (point) pt))
                   (start nil))
               (while (string-match "\n[ \t]*;+" exp start)
                 (setq start (1+ (match-beginning 0))
                       exp (concat (substring exp 0 start)
                                   (substring exp (match-end 0)))))
               exp)
           (current-buffer)))))))

;;;###autoload
(defun pp-eval-last-sexp (arg)
  "Run `pp-eval-expression' on sexp before point.
With ARG, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (elisp--eval-defun-1
                                   (macroexpand (pp-last-sexp)))
                                  lexical-binding)))
    (pp-eval-expression (elisp--eval-defun-1
                         (macroexpand (pp-last-sexp))))))

;;;###autoload
(defun pp-macroexpand-last-sexp (arg)
  "Run `pp-macroexpand-expression' on sexp before point.
With ARG, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (macroexpand-1 (pp-last-sexp))))
    (pp-macroexpand-expression (pp-last-sexp))))

;;;###autoload
(defun pp-emacs-lisp-code (sexp &optional end)
  "Insert SEXP into the current buffer, formatted as Emacs Lisp code.
Use the `pp-max-width' variable to control the desired line length.
Note that this could be slow for large SEXPs.
Can also be called with two arguments, in which case they're taken to be
the bounds of a region containing Lisp code to pretty-print."
  (require 'edebug)
  (if end (pp--region sexp end #'pp-emacs-lisp-code)
    (let ((obuf (current-buffer)))
      (with-temp-buffer
        (emacs-lisp-mode)
        (pp--insert-lisp sexp)
        (insert "\n")
        (goto-char (point-min))
        (indent-sexp)
        (while (re-search-forward " +$" nil t)
          (replace-match ""))
        (insert-into-buffer obuf)))))

(defvar pp--quoting-syntaxes
  `((quote    . "'")
    (function . "#'")
    (,backquote-backquote-symbol . "`")
    (,backquote-unquote-symbol   . ",")
    (,backquote-splice-symbol    . ",@")))

(defun pp--quoted-or-unquoted-form-p (cons)
  ;; Return non-nil when CONS has one of the forms 'X, `X, ,X or ,@X
  (let ((head (car cons)))
    (and (symbolp head)
         (assq head pp--quoting-syntaxes)
         (let ((rest (cdr cons)))
           (and (consp rest) (null (cdr rest)))))))

(defun pp--insert-lisp (sexp)
  (cl-case (type-of sexp)
    (vector (pp--format-vector sexp))
    (cons (cond
           ((consp (cdr sexp))
            (let ((head (car sexp)))
              (if-let* (((null (cddr sexp)))
                        (syntax-entry (assq head pp--quoting-syntaxes)))
                  (progn
                    (insert (cdr syntax-entry))
                    (pp--insert-lisp (cadr sexp)))
                (pp--format-list sexp))))
           (t
            (pp--format-list sexp))))
    ;; Print some of the smaller integers as characters, perhaps?
    (integer
     (if (<= ?0 sexp ?z)
         (princ (prin1-char sexp) (current-buffer))
       (prin1 sexp (current-buffer))))
    (string
     (let ((print-escape-newlines t))
       (prin1 sexp (current-buffer))))
    (otherwise (prin1 sexp (current-buffer)))))

(defun pp--format-vector (sexp)
  (insert "[")
  (cl-loop for i from 0
           for element across sexp
           do (pp--insert (and (> i 0) " ") element))
  (insert "]"))

(defun pp--format-list (sexp &optional start)
  (if (not (let ((head (car sexp)))
             (or pp--inhibit-function-formatting
                 (not (symbolp head))
                 (keywordp head)
                 (let ((l sexp))
                   (catch 'not-funcall
                     (while l
                       (when (or
                              (atom l) ; SEXP is a dotted list
                              ;; Does SEXP have a form like (ELT... . ,X) ?
                              (pp--quoted-or-unquoted-form-p l))
                         (throw 'not-funcall t))
                       (setq l (cdr l)))
                     nil)))))
      (pp--format-function sexp)
    (insert "(")
    (pp--insert start (pop sexp))
    (while sexp
      (if (consp sexp)
          (if (not (pp--quoted-or-unquoted-form-p sexp))
              (pp--insert " " (pop sexp))
            (pp--insert " . " sexp)
            (setq sexp nil))
        (pp--insert " . " sexp)
        (setq sexp nil)))
    (insert ")")))

(defun pp--format-function (sexp)
  (let* ((sym (car sexp))
         (edebug (get sym 'edebug-form-spec))
         (indent (get sym 'lisp-indent-function))
         (doc (get sym 'doc-string-elt)))
    (when (eq indent 'defun)
      (setq indent 2))
    ;; We probably want to keep all the elements before the doc string
    ;; on a single line.
    (when doc
      (setq indent (1- doc)))
    ;; Special-case closures -- these shouldn't really exist in actual
    ;; source code, so there's no indentation information.  But make
    ;; them output slightly better.
    (when (and (not indent)
               (eq sym 'closure))
      (setq indent 0))
    (pp--insert "(" sym)
    (pop sexp)
    ;; Get the first entries on the first line.
    (if indent
        (pp--format-definition sexp indent edebug)
      (let ((prev 0))
        (while sexp
          (let ((start (point)))
            ;; Don't put sexps on the same line as a multi-line sexp
            ;; preceding it.
            (pp--insert (if (> prev 1) "\n" " ")
                        (pop sexp))
            (setq prev (count-lines start (point)))))))
    (insert ")")))

(defun pp--format-definition (sexp indent edebug)
  (while (and (plusp indent)
              sexp)
    (insert " ")
    ;; We don't understand all the edebug specs.
    (unless (consp edebug)
      (setq edebug nil))
    (if (and (consp (car edebug))
             (eq (caar edebug) '&rest)
             (proper-list-p (car sexp)))
        (pp--insert-binding (pop sexp))
      (if (null (car sexp))
          (insert "()")
        (pp--insert-lisp (car sexp)))
      (pop sexp))
    (pop edebug)
    (decf indent))
  (when (stringp (car sexp))
    (insert "\n")
    (prin1 (pop sexp) (current-buffer)))
  ;; Then insert the rest with line breaks before each form.
  (while sexp
    (insert "\n")
    (if (keywordp (car sexp))
        (progn
          (pp--insert-lisp (pop sexp))
          (when sexp
            (pp--insert " " (pop sexp))))
      (pp--insert-lisp (pop sexp)))))

(defun pp--insert-binding (sexp)
  (insert "(")
  (while sexp
    (if (consp (car sexp))
        ;; Newlines after each (...) binding.
        (progn
          (pp--insert-lisp (car sexp))
          (when (cdr sexp)
            (insert "\n")))
      ;; Keep plain symbols on the same line.
      (pp--insert " " (car sexp)))
    (pop sexp))
  (insert ")"))

(defun pp--insert (delim &rest things)
  (let ((start (if (markerp delim)
                   (prog1
                       delim
                     (setq delim nil))
                 (point-marker))))
    (when delim
      (insert delim))
    (dolist (thing things)
      (pp--insert-lisp thing))
    ;; We need to indent what we have so far to see if we have to fold.
    (pp--indent-buffer)
    (when (> (current-column) (pp--max-width))
      (save-excursion
        (goto-char start)
        (unless (looking-at "[ \t]+$")
          (insert "\n"))
        (pp--indent-buffer)
        (goto-char (point-max))
        ;; If we're still too wide, then go up one step and try to
        ;; insert a newline there.
        (when (> (current-column) (pp--max-width))
          (condition-case ()
              (backward-up-list 1)
            (:success (when (and (not (bobp)) (looking-back " " 2))
                        (insert "\n")))
            (error nil)))))))

(defun pp--max-width ()
  (cond ((numberp pp-max-width)
         pp-max-width)
        ((null pp-max-width)
         most-positive-fixnum)
        ((eq pp-max-width t)
         (window-width))
        (t
         (error "Invalid pp-max-width value: %s" pp-max-width))))

(defun pp--indent-buffer ()
  (goto-char (point-min))
  (while (not (eobp))
    (lisp-indent-line)
    (forward-line 1)))

(provide 'pp)				; so (require 'pp) works

;;; pp.el ends here
