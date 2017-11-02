;;; bbdb-anniv.el --- get anniversaries from BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2011-2017  Free Software Foundation, Inc.

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Anniversaries are stored in xfields as defined via `bbdb-anniv-alist'.
;; Each such field may contain multiple anniversaries entries with separators
;; defined via `bbdb-separator-alist' (newlines by default).
;; Each anniversary entry is a string DATE followed by optional TEXT.
;; DATE may take the same format as the date of ordinary diary entries.
;; In particular, `calendar-date-style' is obeyed via `diary-date-forms'.
;; If `bbdb-anniv-alist' has a non-nil FORM for this type of anniversary,
;; FORM is used to display the anniversary entry in the diary buffer.
;; If FORM is nil, TEXT is used instead to display the anniversary entry
;; in the diary buffer.
;;
;; To display BBDB anniversaries in the Emacs diary,
;; call `bbdb-initialize' with arg `anniv'.
;;
;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'diary-lib)
(eval-when-compile
  (require 'cl-lib))

(defcustom bbdb-anniv-alist
  '((birthday . "%n's %d%s birthday")
    (wedding  . "%n's %d%s wedding anniversary")
    (anniversary))
  "Alist of rules for formatting anniversaries in the diary buffer.
Each element is of the form (LABEL . FORM).
LABEL is the xfield where this type of anniversaries is stored.
FORM is a format string with the following substitutions:
  %n  name of the record
  %d  number of years
  %s  ordinal suffix (st, nd, rd, th) for the year.
  %t  the optional text following the date string in field LABEL.
If FORM is nil, use the text following the date string in field LABEL
as format string."
  :type '(repeat (cons :tag "Rule"
                       (symbol :tag "Label")
                       (choice (string)
                               (const nil))))
  :group 'bbdb-utilities-anniv)

;; `bbdb-anniv-diary-entries' becomes a member of  `diary-list-entries-hook'.
;; When this hook is run by `diary-list-entries', the variable `original-date'
;; is bound to the value of arg DATE of `diary-list-entries'.
;; Also, `number' is arg NUMBER of `diary-list-entries'.
;; `diary-list-entries' selects the entries for NUMBER days starting with DATE.

(defvar original-date) ; defined in diary-lib
(with-no-warnings (defvar number)) ; defined in diary-lib

;;;###autoload
(defun bbdb-anniv-diary-entries ()
  "Add anniversaries from BBDB records to `diary-list-entries'.
This obeys `calendar-date-style' via `diary-date-forms'.
To enable this feature, put the following into your .emacs:

 \(add-hook 'diary-list-entries-hook 'bbdb-anniv-diary-entries)"
  ;; Loop over NUMBER dates starting from ORGINAL-DATE.
  (let* ((num-date (1- (calendar-absolute-from-gregorian original-date)))
         (end-date (+ num-date number)))
    (while (<= (setq num-date (1+ num-date)) end-date)
      (let* ((date (calendar-gregorian-from-absolute num-date))
             (dd (calendar-extract-day date))
             (mm (calendar-extract-month date))
             (yy (calendar-extract-year date))
             ;; We construct a regexp that only uses shy groups,
             ;; except for the part of the regexp matching the year.
             ;; This way we can grab the year from the date string.
             (year "\\([0-9]+\\)\\|\\*")
             (dayname (format "%s\\|%s\\.?" (calendar-day-name date)
                              (calendar-day-name date 'abbrev)))
             (lex-env `((day . ,(format "0*%d" dd))
                        (month . ,(format "0*%d" mm)) (year . ,year)
                        (dayname . ,dayname)
                        (monthname . ,(format "%s\\|%s" (calendar-month-name mm)
                                              (calendar-month-name mm 'abbrev)))))
             ;; Require that the matched date is at the beginning of the string.
             (fmt (format "\\`%s?\\(?:%%s\\)"
                          (regexp-quote diary-nonmarking-symbol)))
             date-forms)

        (cl-flet ((fun (date-form)
                       (push (cons (format fmt
                                           (mapconcat (lambda (form) (eval form lex-env))
                                                      (if (eq (car date-form) 'backup)
                                                          (cdr date-form) date-form)
                                                      "\\)\\(?:"))
                                   (eq (car date-form) 'backup))
                             date-forms)))
          (mapc #'fun diary-date-forms)

          ;; The anniversary of February 29 is considered to be March 1
          ;; in non-leap years.  So we search for February 29, too.
          (when (and (= mm 3) (= dd 1)
                     (not (calendar-leap-year-p yy)))
            (setq lex-env `((day . "0*29") (month . "0*2") (year . ,year)
                            (dayname . ,dayname)
                            (monthname . ,(format "%s\\|%s" (calendar-month-name 2)
                                                  (calendar-month-name 2 'abbrev)))))
            (mapc #'fun diary-date-forms)))

        (dolist (record (bbdb-records))
          (dolist (rule bbdb-anniv-alist)
            (dolist (anniv (bbdb-record-xfield-split record (car rule)))
              (let ((date-forms date-forms)
                    (anniv-string (concat anniv " X")) ; for backup forms
                    (case-fold-search t)
                    form yr text)
                (while (setq form (pop date-forms))
                  (when (string-match (car form) anniv-string)
                    (setq date-forms nil
                          yr (match-string 1 anniv-string)
                          yr (if (and yr (string-match-p "[0-9]+" yr))
                                 (- yy (string-to-number yr))
                               100) ; as in `diary-anniversary'
                          ;; For backup forms we should search backward in
                          ;; anniv-string from (match-end 0) for "\\<".
                          ;; That gets too complicated here!
                          ;; Yet for the default value of `diary-date-forms'
                          ;; this would matter only if anniv-string started
                          ;; with a time. That is rather rare for anniversaries.
                          ;; Then we may simply step backward by one character.
                          text (substring anniv-string (if (cdr form) ; backup
                                                           (1- (match-end 0))
                                                         (match-end 0))
                                          -1)
                          text (replace-regexp-in-string "\\`[ \t]+" "" text)
                          text (replace-regexp-in-string "[ \t]+\\'" "" text))
                    (if (cdr rule)
                        (setq text (replace-regexp-in-string "%t" text (cdr rule))))
                    ;; Add the anniversaries to `diary-entries-list'.
                    (if (and (numberp yr) (< 0 (length text)))
                        (diary-add-to-list
                         date
                         ;; `diary-add-to-list' expects an arg SPECIFIER for being
                         ;; able to jump to the location of the entry in the diary
                         ;; file.  Here we only have BBDB records.  So we use
                         ;; an empty string for SPECIFIER, but instead we `propertize'
                         ;; the STRING passed to `diary-add-to-list'.
                         (propertize
                          (format
                           ;; Text substitution similar to `diary-anniversary'.
                           (replace-regexp-in-string "%n" (bbdb-record-name record) text)
                           yr (diary-ordinal-suffix yr))
                          'diary-goto-entry (list 'bbdb-display-records (list record)))
                         ""))))))))))))

;; based on `diary-goto-entry'
(defun bbdb-anniv-goto-entry (button)
  "Jump to the diary entry for the BUTTON at point.
The character at point may have a text property `diary-goto-entry'
which should be a list (FUNCTION ARG1 ARG2 ...).  Then call FUNCTION
with args ARG1, ARG2, ... to locate the entry.  Otherwise follow
the rules used by `diary-goto-entry'."
  (let* ((fun-call (get-text-property (overlay-start button)
                                      'diary-goto-entry))
         (locator (button-get button 'locator))
         (marker (car locator))
         markbuf file)
    (cond (fun-call
           (apply (car fun-call) (cdr fun-call)))
          ;; If marker pointing to diary location is valid, use that.
          ((and marker (setq markbuf (marker-buffer marker)))
           (pop-to-buffer markbuf)
           (goto-char (marker-position marker)))
          ;; Marker is invalid (eg buffer has been killed).
          ((and (setq file (cadr locator))
                (file-exists-p file)
                (find-file-other-window file))
           (when (eq major-mode (default-value 'major-mode)) (diary-mode))
           (goto-char (point-min))
           (if (re-search-forward (format "%s.*\\(%s\\)"
                                          (regexp-quote (nth 2 locator))
                                          (regexp-quote (nth 3 locator)))
                                  nil t)
               (goto-char (match-beginning 1))))
          (t
           (message "Unable to locate this diary entry")))))

;; `diary-goto-entry-function' is rather inflexible if multiple packages
;; want to use it for its purposes: this variable can be hijacked
;; only once.  Here our function `bbdb-anniv-goto-entry' should work
;; for other packages, too.
(setq diary-goto-entry-function 'bbdb-anniv-goto-entry)

(provide 'bbdb-anniv)

;;; bbdb-anniv.el ends here
