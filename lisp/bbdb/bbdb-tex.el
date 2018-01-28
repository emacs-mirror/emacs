;;; bbdb-tex.el --- feed BBDB into LaTeX  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  Free Software Foundation, Inc.

;; Authors: Boris Goldowsky <boris@cs.rochester.edu>
;;          Dirk Grunwald <grunwald@cs.colorado.edu>
;;          Luigi Semenzato <luigi@paris.cs.berkeley.edu>

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

;; This file lets you feed BBDB into LaTeX.
;; See the BBDB info manual for documentation.
;;
;; In the *BBDB* buffer, type M-x `bbdb-tex' to convert the listing
;; to LaTeX format.
;;
;; TeX macros appearing in the output:
;;   \name{first}{last}
;;   \organization{foo bar}
;;   \affix{foo bar}
;;   \aka{foo bar}
;;   \phone{key}{123 456 7890}
;;   \address{key}{foo bar}
;;   \mail{foo@bar.com}{Smith <foo@bar.com>}
;;   \xfield{key}{value}
;; Each macro may appear multiple times.
;;
;; The detailed grammar of the output is defined in `bbdb-tex-alist'.
;; The output starts with a prolog where you can specify LaTeX packages
;; and other customizations in the usual way.  The above macros should get
;; defined, too. By default, this happens in the style file bbdb.sty that
;; is shipped with BBDB.
;;
;; The body of the output contains the BBDB records.  Usually, the records
;; are placed inside some "bbdb" environment.  You can customize which fields
;; of each record should appear in the listing and in which order.
;; Also, you can put separators between individual fields.  A separator macro
;; can also separate records when the first character of the last name differs
;; from the first character of the last name of the previous record.
;; The listing ends with an epilog.

;; A few notes on "advanced usage" of `bbdb-tex':
;;
;; It should be possible to use `bbdb-tex' with all the bells and whistles
;; of LaTeX by loading the appropriate LaTeX style files and packages or
;; embedding the output of `bbdb-tex' into more complex LaTeX documents.
;; For this you can customize the rules in `bbdb-tex-alist' and use
;; customized style files for interpreting the TeX macros used by `bbdb-tex'.
;;
;; Generally, lisp customizations for `bbdb-tex' are intended to provide control
;; of *what* appears in the TeX listing.  But there are no lisp customization
;; options to control the actual layout that should be handled by LaTeX.
;; BBDB is shipped with one basic LaTeX style file bbdb.sty to handle
;; the TeX macros listed above.  You should customize this LaTeX style file
;; to match your taste and / or your needs.  Note also that `bbdb-tex-alist'
;; allows you to specify an arbitrary number of rules that may use different
;; style files for the above TeX macros.

;; Generally, it will be advantageous to make all relevant style files
;; and packages known to LaTeX by putting them in the appropriate directories
;; of your TeX installation.  Likely, the user variable `bbdb-tex-path'
;; should not be used in such advanced cases.  The main purpose of the
;; inlining mechanism provided via `bbdb-tex-path' is that we can ship
;; and install BBDB without worrying about the tricky question where to
;; (auto-) install the basic style file bbdb.sty shipped with BBDB so that
;; TeX finds it.  Most often, it will be best to manually install even bbdb.sty
;; in a directory where TeX finds it and bind `bbdb-tex-path' to t to fully
;; suppress the inlining.
;;
;; Before generating the TeX output, the field values of a record are massaged
;; by `bbdb-tex-field' that passes these values by default to `bbdb-tex-replace',
;; see also `bbdb-tex-replace-list'.  Instead the user may also define functions
;; `bbdb-tex-output-...' that take precedence, see `bbdb-tex-field'.
;;
;; `bbdb-tex' understands one new BBDB xfield: tex-name, see also
;; `bbdb-tex-name'.  If this xfield is defined for a record,
;; this will be used for the TeXed listing instead of the name field
;; of that record.  The value of the xfield tex-name is used verbatim,
;; it does not see `bbdb-tex-field' and `bbdb-tex-replace-list'.
;;
;;
;; This program was adapted for BBDB by Boris Goldowsky
;; <boris@cs.rochester.edu> and Dirk Grunwald
;; <grunwald@cs.colorado.edu> using a TeX format designed by Luigi
;; Semenzato <luigi@paris.cs.berkeley.edu>.
;; We are also grateful to numerous people on the bbdb-info
;; mailing list for suggestions and bug reports.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)

;;; Variables:

(defcustom bbdb-tex-name 'tex-name
  "Xfield holding the name in TeX format.
The string in this field gets split into first and last name
using `bbdb-separator-alist'.  The separator defaults to \"#\"."
  :group 'bbdb-utilities-tex
  :type '(symbol :tag "Xfield"))

(defcustom bbdb-tex-alist
  `((multi-line
     (demand (or address phone))
     (prolog ,(concat "\\documentclass{article}\n\\usepackage{bbdb}\n"
                      "\\usepackage{multicol}\n"
                      "\\begin{document}\n\\begin{multicols}{2}"))
     (record "\\begin{bbdbrecord}" name organization ; affix aka
             (address t) (phone t) (mail t)
             (xfields t nil
                      (omit ,bbdb-tex-name mail-alias creation-date timestamp))
             "\\end{bbdbrecord}\n")
     (separator "\\bbdbseparator{%s}\n")
     (epilog ,(concat "\\noindent\\hrulefill\\\\\nPrinted \\today\n"
                      "\\end{multicols}\n\\end{document}"))
     (options (bbdb-tex-linebreak "\\\\\\\\\n")
              (bbdb-tex-address-layout 2)))

    (one-line
     (demand phone)
     (prolog ,(concat "\\documentclass{article}\n\\usepackage{bbdb}\n"
                      "\\begin{document}\n\\begin{bbdb}{llllll}"))
     (record name "&" (organization 1) "&" (phone 2 "&") "&" (mail 1)
             "&" (address 1) "\\\\")
     (separator "\\bbdbseparator{%s}")
     (epilog "\\end{bbdb}\n\\end{document}")
     (options (bbdb-tex-linebreak ", ")
              (bbdb-tex-address-layout 3)))

    (phone
     (demand phone)
     (prolog ,(concat "\\documentclass{article}\n\\usepackage{bbdb}\n"
                      "\\begin{document}\n\\begin{bbdb}{ll}"))
     (record name "&" (phone 2 "&") "\\\\")
     (separator "\\bbdbseparator{%s}")
     (epilog "\\end{bbdb}\n\\end{document}")
     (options (bbdb-tex-linebreak ", ")
              (bbdb-tex-address-layout 3)))

    (example ; another rule with more examples
     (demand (or address phone))
     (prolog ,(concat "\\documentclass{article}\n\\usepackage{bbdb}\n"
                      "\\usepackage{multicol}\n"
                      "\\begin{document}\n\\begin{multicols}{2}"))
     (record "\\begin{bbdbrecord}" name organization
             (address 1 nil (omit "work"))
             (phone 2 nil (admit "home" "cell"))
             (mail t)
             (birthday t)
             (xfields t nil
                      (omit ,bbdb-tex-name mail-alias creation-date timestamp))
             "\\end{bbdbrecord}\n")
     (separator "\\bbdbseparator{%s}\n")
     (epilog ,(concat "\\noindent\\hrulefill\\\\\nPrinted \\today\n"
                      "\\end{multicols}\n\\end{document}"))
     (options (bbdb-tex-linebreak "\\\\\\\\\n")
              (bbdb-tex-address-layout 2))))

  "Alist of rules for passing BBDB to LaTeX.
Each rule has the form (RULE LIST1 LIST2 ...).
The symbol RULE identifies the rule.
The remainder are lists LIST that should have one of these forms:

 (demand FORM)

  Here FORM is a lisp expression.  A record will be TeXed only
  if evaluating FORM yields a non-nil value for this record.
  When FORM is evaluated, the symbols name, affix, organization, mail,
  phone, address, and xfields are set to the corresponding values
  of this record; these symbols are nil if the respective field
  does not exist for this record.

 (prolog STRING)

  The string STRING is inserted at the beginning of the buffer.
  If STRING contains the substring \"\\usepackage{foo}\" and
  a file \"foo.sty\" exists within `bbdb-tex-path', replace
  \"\\usepackage{foo}\" with the content of the file \"foo.sty\",
  surrounded by \"\\makeatletter\" and \"\\makeatother\".
  Note: This fails with more sophisticated LaTeX style files
  using, e.g., optional arguments for the \"\\usepackage\" macro.

 (record ELT1 ELT2 ...)

  Here ELT may be one of the following:

  IF ELT is name, this expands to \"\\name{first}{last}\"

  If ELT is affix, organization, or aka, ELT expands to \"\\ELT{value}\".
    Here the elements of ELT are concatenated to get one value.

  If ELT is the key of an xfield, ELT expands to \"\\xfield{ELT}{value}\".

  If ELT is a string, this is inserted \"as is\" in the TeX buffer.

  ELT may also be a loop (FLD COUNT [SEPARATOR] [OPT...])
  looping over the values of FLD.

  If FLD is mail, this expands to \"\\mail{short}{long}\",
    such as \"\\mail{foo@bar.com}{Smith <foo@bar.com>}\",
  If FLD is phone, this expands to \"\\phone{key}{number}\"
  If FLD is address, this expands to \"\\address{key}{value}\".
  If FLD is xfields, this expands to \"\\xfield{key}{value}\".
  If FLD is the key of an xfield, split the value of FLD
    using `bbdb-separator-alist' to generate a list of values,
    which then expand to \"\\xfield{FLD}{value}\".

  If COUNT is a number, process at most COUNT values of FLD.
  IF COUNT is t, process all values of FLD.

  If SEPARATOR is non-nil, it is a string that is inserted between
  the values of FLD.  Insert COUNT - 1 instances of SEPARATOR,
  even if there are fewer values of FLD.

  If FLD is mail, phone, address, or xfields,
  OPT may be a list (admit KEY ...) or (omit KEY ...).
  Then a value is admitted or omitted if its key KEY is listed here.

 (separator STRING)

  When the first letter of the records' sortkey increases compared with
  the previous record in the TeX listing, the new letter is formatted
  using the format string STRING to generate a separator macro.

 (epilog STRING)

  The string STRING is inserted at the end of the buffer."
  :group 'bbdb-utilities-TeX
  :type '(repeat (cons (symbol :tag "rule")
                       (repeat
                        (choice (cons :tag "demand" (const demand) sexp)
                                (list :tag "prolog" (const prolog) string)
                                (cons :tag "record" (const record) sexp)
                                (list :tag "separator" (const separator) string)
                                (list :tag "epilog" (const epilog) string)
                                (cons :tag "options" (const options) sexp))))))

(defcustom bbdb-tex-rule-default 'multi-line
  "Default rule for BBDB tex.
This symbol should be a key in `bbdb-tex-alist'."
  :group 'bbdb-utilities-tex
  :type '(symbol :tag "rule"))

;; FIXME
;; (defcustom bbdb-tex-empty-fields nil
;;   "If non-nil generate TeX output even for empty fields."
;;   :group 'bbdb-utilities-tex)

(defcustom bbdb-tex-replace-list
  '(("[#$%&_]" . "\\\\\\&")
    ("<" . "\\\\textless ")
    (">" . "\\\\textgreater ")
    ("~" . "\\\\textasciitilde ")
    ("{" . "\\\\textbraceleft ")
    ("}" . "\\\\textbraceright "))
  "Replacement list for TeX's special characters.
Each element is of the form (REGEXP . REPLACE)."
  :group 'bbdb-utilities-tex
  :type '(repeat (cons regexp string)))

(defcustom bbdb-tex-linebreak "\\\\\\\\\n"
  "Replacement for linebreaks."
  :group 'bbdb-utilities-tex
  :type 'string)

(defcustom bbdb-tex-address-format-list bbdb-address-format-list
  "List of address formatting rules for `bbdb-tex'.
Each element may take the same values as in `bbdb-address-format-list'.
The elements EDIT of `bbdb-address-format-list' are ignored."
  :group 'bbdb-utilities-tex
  :type '(repeat (list (choice (const :tag "Default" t)
                               (function :tag "Function")
                               (repeat (string)))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function")))))

(defcustom bbdb-tex-address-layout 2
  "Address layout according to `bbdb-tex-address-format-list'.
2 is multi-line layout, 3 is one-line layout."
  :group 'bbdb-utilities-TeX
  :type '(choice (const :tag "multi-line" 2)
                 (const :tag "one-line" 3)))

(defcustom bbdb-tex-file "~/bbdb.tex"
  "Default file name for TeXing BBDB."
  :group 'bbdb-utilities-tex
  :type 'file)

;;; Internal variables

(defvar bbdb-tex-rule-last bbdb-tex-rule-default
  "Last rule used for TeXing BBDB.")

(defvar bbdb-tex-file-last bbdb-tex-file
  "Last used TeX file")

;;; Functions:

;; While we use `bbdb-tex-replace' only once in `bbdb-tex-field',
;; we keep it as a separate function so that it can also be used
;; inside user-defined functions `bbdb-tex-output-...'.
(defun bbdb-tex-replace (string)
  "Apply replacement rules `bbdb-tex-replace-list' to STRING.
Also, replace linebreaks by `bbdb-tex-linebreak'."
  (if (not string)
      ""
    (dolist (elt bbdb-tex-replace-list)
      (setq string (replace-regexp-in-string (car elt) (cdr elt) string)))
    (replace-regexp-in-string "\n" bbdb-tex-linebreak string)))

(defun bbdb-tex-field (field str)
  "Massage string STR for LaTeX.
By default, STR is passed to `bbdb-tex-replace'.
The user may also define a function `bbdb-tex-output-FIELD'
that takes precedence."
  (let ((fun (intern-soft (format "bbdb-tex-output-%s" field))))
    (if fun
        (funcall fun str)
      (bbdb-tex-replace str))))

(defun bbdb-tex-list (list rule fun)
  "Use function FUN to generate output for LIST according to RULE.
LIST is a list of field values such as a list of addresses.
RULE is an element of a record list as in `bbdb-tex-alist'
used to select the elements of LIST that get processed by calling FUN."
  (let ((admit (cdr (assq 'admit rule)))
        (omit (cdr (assq 'omit rule)))
        (num (if (numberp (nth 1 rule)) (nth 1 rule)))
        (sep (if (nth 2 rule) (concat (nth 2 rule) "\n")))
        (i -1)
        new-list elt)

    ;; Select the relevant elements of LIST.
    (cond (admit
           (dolist (l list)
             (if (member (elt l 0) admit)
                 (push l new-list)))
           (setq new-list (nreverse new-list)))

          (omit
           (dolist (l list)
             (unless (member (elt l 0) omit)
               (push l new-list)))
           (setq new-list (nreverse new-list)))

          (t
           (setq new-list list)))

    (cond ((not num)
           (insert (mapconcat fun new-list (or sep ""))))
          ((not sep)
           (while (and (< (setq i (1+ i)) num)
                       (setq elt (pop new-list)))
             (insert (funcall fun elt))))
          (t
           (while (< (setq i (1+ i)) num)
             (if (setq elt (pop new-list))
                 (insert (funcall fun elt)))
             (if (< (1+ i) num)
                 (insert sep)))))))

;;;###autoload
(defun bbdb-tex (records file rule)
  "Generate FILE for TeXing RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
RULE should be an element of `bbdb-tex-alist'."
  (interactive
   (list (bbdb-do-records)
         (read-file-name
          (format "TeX file: (default %s) "
                  (abbreviate-file-name bbdb-tex-file-last))
          (file-name-directory bbdb-tex-file-last)
          bbdb-tex-file-last)
         (intern (completing-read (format "Rule: (default %s) "
                                          bbdb-tex-rule-last)
                                  bbdb-tex-alist nil t
                                  nil nil (symbol-name bbdb-tex-rule-last)))))
  ;; Remember our choice for `bbdb-tex-file-last'.
  (setq bbdb-tex-file-last (expand-file-name file))

  (find-file bbdb-tex-file-last)
  (let* ((buffer-undo-list t)
         (rule (assq rule bbdb-tex-alist))
         (demand (nth 1 (assq 'demand rule)))
         (separator (nth 1 (assq 'separator rule)))
         current-letter p-symbols p-values)
    (erase-buffer)

    ;; Options
    (dolist (option (cdr (assq 'options rule)))
      (push (car option) p-symbols)
      (push (cadr option) p-values))
    (cl-progv p-symbols p-values

      ;; Prolog
      (let ((prolog (nth 1 (assq 'prolog rule))))
        (when prolog
          (insert prolog)
          (when (consp bbdb-tex-path)
            (goto-char (point-min))
            (while (re-search-forward "\\\\usepackage[ \t\n]*{\\([^}]+\\)}" nil t)
              (let ((sty (locate-file (match-string 1) bbdb-tex-path '(".sty"))))
                (when sty
                  (replace-match (format "\n\\\\makeatletter\n%% begin %s\n%% end %s\n\\\\makeatother\n" sty sty))
                  (save-excursion
                    (forward-line -2)
                    (insert-file-contents sty))))))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "% end BBDB prolog\n")))

      ;; Process Records
      (dolist (record (bbdb-record-list records))
        (let* ((first-letter
                (substring (bbdb-record-sortkey record) 0 1))
               (firstname (bbdb-record-firstname record))
               (lastname (bbdb-record-lastname record))
               (name    (bbdb-record-name record))
               (name-lf (bbdb-record-name-lf record))
               (organization (bbdb-record-organization record))
               (affix   (bbdb-record-affix record))
               (aka     (bbdb-record-aka record))
               (mail    (bbdb-record-mail record))
               (phone   (bbdb-record-phone record))
               (address (bbdb-record-address record))
               (xfields (bbdb-record-xfields record))
               (lex-env `((firstname . ,firstname) (lastname . ,lastname)
                          (name . ,name) (name-lf . ,name-lf) (aka . ,aka)
                          (organization . ,organization) (affix . ,affix)
                          (mail . ,mail) (phone . ,phone)
                          (address . ,address) (xfields . ,xfields)))
               (bbdb-address-format-list bbdb-tex-address-format-list))

          ;; A record is processed only if the form DEMAND
          ;; evaluates to a non-nil value.
          (when (or (not demand)
                    (eval demand lex-env))

            ;; Separator
            (if (and separator
                     (not (and current-letter
                               (equal first-letter current-letter))))
                (insert (format separator (upcase first-letter)) "\n"))
            (setq current-letter first-letter)

            (dolist (elt (cdr (assq 'record rule)))
              (cond ((stringp elt)
                     (insert elt "\n"))

                    ((eq elt 'name) ; name of record
                     (let ((tex-name (and bbdb-tex-name
                                          (bbdb-record-field record bbdb-tex-name)))
                           (fmt "\\name{%s}{%s}\n"))
                       (if tex-name
                           (let ((first-last (bbdb-split bbdb-tex-name tex-name)))
                             (cond ((eq 2 (length first-last))
                                    (insert (format fmt (car first-last) (cadr first-last))))
                                   ((eq 1 (length first-last))
                                    (insert (format fmt "" (car first-last))))
                                   (t (error "TeX name %s cannot be split" tex-name))))
                         (insert (format fmt
                                         (bbdb-tex-field 'firstname firstname)
                                         (bbdb-tex-field 'lastname  lastname))))))

                    ;; organization, affix or aka as single string
                    ((memq elt '(organization affix aka))
                     (let ((val (bbdb-record-field record elt)))
                       (if val
                           (insert (format "\\%s{%s}\n" elt
                                           (bbdb-tex-field elt (bbdb-concat elt val)))))))

                    ;; organization, affix or aka as list of strings
                    ((memq (car elt) '(organization affix aka))
                     (bbdb-tex-list
                      (bbdb-record-field record (car elt))
                      elt
                      `(lambda (o)
                         (format "\\%s{%s}\n" ',(car elt)
                                 (bbdb-tex-field ',(car elt) o)))))

                    ((eq (car elt) 'mail) ; mail
                     (bbdb-tex-list
                      mail elt
                      (lambda (m)
                        (format "\\mail{%s}{%s}\n"
                                ;; No processing of plain mail address
                                (nth 1 (bbdb-decompose-bbdb-address m))
                                (bbdb-tex-field 'mail m)))))

                    ((eq (car elt) 'address) ; address
                     (bbdb-tex-list
                      address elt
                      (lambda (a)
                        (format "\\address{%s}{%s}\n"
                                (bbdb-tex-field 'address-label (bbdb-address-label a))
                                (bbdb-tex-field 'address (bbdb-format-address
                                                          a bbdb-tex-address-layout))))))

                    ((eq (car elt) 'phone) ; phone
                     (bbdb-tex-list
                      phone elt
                      (lambda (p)
                        (format "\\phone{%s}{%s}\n"
                                (bbdb-tex-field 'phone-label (bbdb-phone-label p))
                                (bbdb-tex-field 'phone (bbdb-phone-string p))))))

                    ((eq (car elt) 'xfields) ; list of xfields
                     (bbdb-tex-list
                      (bbdb-record-field record 'xfields)
                      elt
                      (lambda (x)
                        (format "\\xfield{%s}{%s}\n"
                                (bbdb-tex-field 'xfield-label (symbol-name (car x)))
                                (bbdb-tex-field 'xfield (cdr x))))))

                    ((symbolp elt) ; xfield as single string
                     ;; The value of an xfield may be a sexp instead of a string.
                     ;; Ideally, a sexp should be formatted by `pp-to-string',
                     ;; then printed verbatim.
                     (let ((val (format "%s" (bbdb-record-field record elt))))
                       (if val
                           (insert (format "\\xfield{%s}{%s}\n" elt
                                           (bbdb-tex-field elt (bbdb-concat elt val)))))))

                    ((consp elt) ; xfield as list of strings
                     (bbdb-tex-list
                      (bbdb-split (car elt)
                                  (format "%s" (bbdb-record-field record (car elt))))
                      elt
                      `(lambda (x)
                         (format "\\xfield{%s}{%s}\n" ',(car elt)
                                 (bbdb-tex-field ',(car elt) x)))))

                    (t (error "Rule `%s' undefined" elt)))))))

      ;; Epilog
      (let ((epilog (nth 1 (assq 'epilog rule))))
        (when epilog
          (insert "% begin BBDB epilog\n" epilog)
          (unless (bolp) (insert "\n"))))))
  (setq buffer-undo-list nil)
  (save-buffer))

(provide 'bbdb-tex)

;;; bbdb-tex.el ends here
