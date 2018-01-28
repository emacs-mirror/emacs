;;; bbdb-com.el --- user-level commands of BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  Free Software Foundation, Inc.

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
;; This file contains most of the user-level interactive commands for BBDB.
;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'mailabbrev)

(eval-and-compile
  (autoload 'build-mail-aliases "mailalias")
  (autoload 'browse-url-url-at-point "browse-url"))

(require 'crm)
(defvar bbdb-crm-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map " " 'self-insert-command)
    map)
  "Keymap used for BBDB crm completions.")

(defun bbdb-get-records (prompt)
  "If inside the *BBDB* buffer get the current records.
In other buffers ask the user."
  (if (string= bbdb-buffer-name (buffer-name))
      (bbdb-do-records)
    (bbdb-completing-read-records prompt)))

;; Note about the arg RECORDS of various BBDB commands:
;;  - Usually, RECORDS is a list of records.  (Interactively,
;;    this list of records is set up by `bbdb-do-records'.)
;;  - If these commands are used, e.g., in `bbdb-create-hook' or
;;    `bbdb-change-hook', they will be called with one arg, a single record.
;; So depending on context the value of RECORDS will be a single record
;; or a list of records, and we want to handle both cases.
;; So we pass RECORDS to `bbdb-record-list' to handle both cases.
(defun bbdb-record-list (records &optional full)
  "Ensure that RECORDS is a list of records.
If RECORDS is a single record turn it into a list.
If FULL is non-nil, assume that RECORDS include display information."
  (if records
      (if full
          (if (vectorp (car records)) (list records) records)
        (if (vectorp records) (list records) records))))

;; Note about BBDB prefix commands:
;; `bbdb-do-all-records', `bbdb-append-display' and `bbdb-search-invert'
;; are fake prefix commands. They need not precede the main commands.
;; Also, `bbdb-append-display' can act on multiple commands.

(defun bbdb-prefix-message ()
  "Display a message about selected BBDB prefix commands."
  (let ((msg (bbdb-concat " " (elt bbdb-modeline-info 1)
                          (elt bbdb-modeline-info 3)
                          (elt bbdb-modeline-info 5))))
    (unless (string= "" msg) (message "%s" msg))))

;;;###autoload
(defun bbdb-do-all-records (&optional arg)
  "Command prefix for operating on all records currently displayed.
With prefix ARG a positive number, operate on all records.
With prefix ARG a negative number, operate on current record only.
This only works for certain commands."
  (interactive "P")
  (setq bbdb-do-all-records
        (or (and (numberp arg) (< 0 arg))
            (and (not (numberp arg)) (not bbdb-do-all-records))))
  (aset bbdb-modeline-info 4 (if bbdb-do-all-records "all"))
  (aset bbdb-modeline-info 5
        (if bbdb-do-all-records
            (substitute-command-keys
             "\\<bbdb-mode-map>\\[bbdb-do-all-records]")))
  (bbdb-prefix-message))

;;;###autoload
(defun bbdb-do-records (&optional full)
  "Return list of records to operate on.
Normally this list includes only the current record.
It includes all currently displayed records if the command prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records] is used.
If FULL is non-nil, the list of records includes display information."
  (if bbdb-do-all-records
      (progn
        (setq bbdb-do-all-records nil)
        (aset bbdb-modeline-info 4 nil)
        (aset bbdb-modeline-info 5 nil)
        (if full bbdb-records (mapcar 'car bbdb-records)))
    (list (bbdb-current-record full))))

;;;###autoload
(defun bbdb-append-display-p ()
  "Return variable `bbdb-append-display' and reset."
  (let ((job (cond ((eq t bbdb-append-display))
                   ((numberp bbdb-append-display)
                    (setq bbdb-append-display (1- bbdb-append-display))
                    (if (zerop bbdb-append-display)
                        (setq bbdb-append-display nil))
                    t)
                   (bbdb-append-display
                    (setq bbdb-append-display nil)
                    t))))
    (cond ((numberp bbdb-append-display)
           (aset bbdb-modeline-info 0
                 (format "(add %dx)" bbdb-append-display)))
          ((not bbdb-append-display)
           (aset bbdb-modeline-info 0 nil)
           (aset bbdb-modeline-info 1 nil)))
    job))

;;;###autoload
(defun bbdb-append-display (&optional arg)
  "Toggle appending next searched records in the *BBDB* buffer.
With prefix ARG \\[universal-argument] always append.
With ARG a positive number append for that many times.
With ARG a negative number do not append."
  (interactive "P")
  (setq bbdb-append-display
        (cond ((and arg (listp arg)) t)
              ((and (numberp arg) (< 1 arg)) arg)
              ((or (and (numberp arg) (< arg 0)) bbdb-append-display) nil)
              (t 'once)))
  (aset bbdb-modeline-info 0
        (cond ((numberp bbdb-append-display)
               (format "(add %dx)" bbdb-append-display))
              ((eq t bbdb-append-display) "Add")
              (bbdb-append-display "add")
              (t nil)))
  (aset bbdb-modeline-info 1
        (if bbdb-append-display
            (substitute-command-keys
             "\\<bbdb-mode-map>\\[bbdb-append-display]")))
  (bbdb-prefix-message))

(defsubst bbdb-layout-prefix ()
  "Set the LAYOUT arg interactively using the prefix arg."
  (cond ((eq current-prefix-arg 0) 'one-line)
        (current-prefix-arg 'multi-line)
        (t bbdb-layout)))

(defun bbdb-search-invert-p ()
  "Return variable `bbdb-search-invert' and set it to nil.
To set it again, use command `bbdb-search-invert'."
  (let ((result bbdb-search-invert))
    (setq bbdb-search-invert nil)
    (aset bbdb-modeline-info 2 nil)
    (aset bbdb-modeline-info 3 nil)
    result))

;;;###autoload
(defun bbdb-search-invert (&optional arg)
  "Toggle inversion of the next search command.
With prefix ARG a positive number, invert next search.
With prefix ARG a negative number, do not invert next search."
  (interactive "P")
  (setq bbdb-search-invert
        (or (and (numberp arg) (< 0 arg))
            (and (not (numberp arg)) (not bbdb-search-invert))))
  (aset bbdb-modeline-info 2 (if bbdb-search-invert "inv"))
  (aset bbdb-modeline-info 3 (if bbdb-search-invert
                                 (substitute-command-keys
                                  "\\<bbdb-mode-map>\\[bbdb-search-invert]")))
  (bbdb-prefix-message))

(defmacro bbdb-search (records &rest spec)
  "Search RECORDS for fields matching SPEC.
The following keywords are supported in SPEC to search fields in RECORDS
matching the regexps RE:

:name RE          Match RE against first-last name.
:name-fl RE       Match RE against last-first name.
:all-names RE     Match RE against first-last, last-first, and aka.
:affix RE         Match RE against affixes.
:aka RE           Match RE against akas.
:organization RE  Match RE against organizations.
:mail RE          Match RE against mail addresses.
:xfield RE        Match RE against `bbdb-default-xfield'.
                  RE may also be a cons (LABEL . REGEXP).
                  Then REGEXP is matched against xfield LABEL.
                  If LABEL is '* then RE is matched against all xfields.
:creation-date RE Match RE against creation-date.
:timestamp RE     Match RE against timestamp.

Each of these keywords may appear multiple times.
Other keywords:

:bool BOOL        Combine the search for multiple fields using BOOL.
                  BOOL may be either `or' (match either field)
                  or `and' (match all fields) with default `or'.

To reverse the search, bind `bbdb-search-invert' to t.
See also `bbdb-message-search' for fast searches using `bbdb-hashtable'
but not allowing for regexps.

For backward compatibility, SPEC may also consist of the optional args
  NAME ORGANIZATION MAIL XFIELD PHONE ADDRESS
which is equivalent to
  :all-names NAME :organization ORGANIZATION :mail MAIL
  :xfield XFIELD :phone PHONE :address ADDRESS
This usage is discouraged."
  (when (not (keywordp (car spec)))
    ;; Old format for backward compatibility
    (unless (get 'bbdb-search 'bbdb-outdated)
      (put 'bbdb-search 'bbdb-outdated t)
      (message "Outdated usage of `bbdb-search'")
      (sit-for 2))
    (let (newspec val)
      (dolist (key '(:all-names :organization :mail :xfield :phone :address))
        (if (setq val (pop spec))
            (push (list key val) newspec)))
      (setq spec (apply 'append newspec))))

  (let* ((count 0)
         (sym-list (mapcar (lambda (_)
                             (make-symbol
                              (format "bbdb-re-%d" (setq count (1+ count)))))
                           spec))
         (bool (make-symbol "bool"))
         (not-invert (make-symbol "not-invert"))
         (matches (make-symbol "matches"))
         keyw re-list clauses)
    (set bool ''or) ; default

    ;; Check keys.
    (while (keywordp (setq keyw (car spec)))
      (setq spec (cdr spec))
      (pcase keyw
	(`:name
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(string-match ,sym (bbdb-record-name record)) clauses)))

	(`:name-lf
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(string-match ,sym (bbdb-record-name-lf record)) clauses)))

	(`:all-names
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(or (string-match ,sym (bbdb-record-name record))
                      (string-match ,sym (bbdb-record-name-lf record))
                      (let ((akas (bbdb-record-field record 'aka-all))
                            aka done)
                        (while (and (setq aka (pop akas)) (not done))
                          (setq done (string-match ,sym aka)))
                        done))
                 clauses)))

	(`:affix
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((affixs (bbdb-record-field record 'affix-all))
                        affix done)
                    (if affix
                        (while (and (setq affix (pop affixs)) (not done))
                          (setq done (string-match ,sym affix)))
                      ;; so that "^$" matches records without affix
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:aka
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((akas (bbdb-record-field record 'aka-all))
                        aka done)
                    (if aka
                        (while (and (setq aka (pop akas)) (not done))
                          (setq done (string-match ,sym aka)))
                      ;; so that "^$" matches records without aka
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:organization
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((organizations (bbdb-record-organization record))
                        org done)
                    (if organizations
                        (while (and (setq org (pop organizations)) (not done))
                          (setq done (string-match ,sym org)))
                      ;; so that "^$" matches records without organizations
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:phone
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((phones (bbdb-record-phone record))
                        ph done)
                    (if phones
                        (while (and (setq ph (pop phones)) (not done))
                          (setq done (string-match ,sym
                                                   (bbdb-phone-string ph))))
                      ;; so that "^$" matches records without phones
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:address
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((addresses (bbdb-record-address record))
                        a done)
                    (if addresses
                        (while (and (setq a (pop addresses)) (not done))
                          (setq done (string-match ,sym
                                                   (bbdb-format-address a 2))))
                      ;; so that "^$" matches records without addresses
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:mail
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(let ((mails (bbdb-record-mail record))
                        (bbdb-case-fold-search t) ; there is no case for mails
                        m done)
                    (if mails
                        (while (and (setq m (pop mails)) (not done))
                          (setq done (string-match ,sym m)))
                      ;; so that "^$" matches records without mail
                      (setq done (string-match ,sym "")))
                    done)
                 clauses)))

	(`:xfield
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(cond ((stringp ,sym)
                         ;; check xfield `bbdb-default-xfield'
                         ;; "^$" matches records without notes field
                         (string-match ,sym
                                       (or (bbdb-record-xfield-string
                                            record bbdb-default-xfield) "")))
                        ((eq (car ,sym) '*)
                         ;; check all xfields
                         (let ((labels bbdb-xfield-label-list) done tmp)
                           (while (and (not done) labels)
                             (setq tmp (bbdb-record-xfield-string record (car labels))
                                   done (and tmp (string-match (cdr ,sym)
                                                               tmp))
                                   labels (cdr labels)))
                           done))
                        (t ; check one field
                         (string-match (cdr ,sym)
                                       (or (bbdb-record-xfield-string
                                            record (car ,sym)) ""))))
                 clauses)))

	(`:creation-date
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(string-match ,sym (bbdb-record-creation-date record))
                 clauses)))

	(`:timestamp
         (let ((sym (pop sym-list)))
           (push `(,sym ,(pop spec)) re-list)
           (push `(string-match ,sym (bbdb-record-timestamp record))
                 clauses)))

        (`:bool
         (set bool (pop spec)))

        ;; Do we need other keywords?

        (_ (error "Keyword `%s' undefines" keyw))))

    `(let ((case-fold-search bbdb-case-fold-search)
           (,not-invert (not (bbdb-search-invert-p)))
           ,@re-list ,matches)
       ;; Are there any use cases for `bbdb-search' where BOOL is only
       ;; known at run time?  A smart byte compiler will hopefully
       ;; simplify the code below if we know BOOL already at compile time.
       ;; Alternatively, BOOL could also be a user function that
       ;; defines more complicated boolian expressions.  Yet then we loose
       ;; the efficiency of `and' and `or' that evaluate its arguments
       ;; as needed.  We would need instead boolian macros that the compiler
       ;; can analyze at compile time.
       (if (eq 'and ,(symbol-value bool))
           (dolist (record ,records)
             (unless (eq ,not-invert (not (and ,@clauses)))
                 (push record ,matches)))
         (dolist (record ,records)
           (unless (eq ,not-invert (not (or ,@clauses)))
               (push record ,matches))))
       (nreverse ,matches))))

(defun bbdb-search-read (&optional field)
  "Read regexp to search FIELD values of records."
  (read-string (format "Search records%s %smatching regexp: "
                       (if field (concat " with " field) "")
                       (if bbdb-search-invert "not " ""))))

;;;###autoload
(defun bbdb (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP
in either the name(s), organization, address, phone, mail, or xfields."
  (interactive (list (bbdb-search-read) (bbdb-layout-prefix)))
  (let ((records (bbdb-search (bbdb-records) :all-names regexp
                              :organization regexp :mail regexp
                              :xfield (cons '* regexp)
                              :phone regexp :address regexp :bool 'or)))
    (if records
        (bbdb-display-records records layout nil t)
      (message "No records matching '%s'" regexp))))

;;;###autoload
(defun bbdb-search-name (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the name
\(or ``alternate'' names\)."
  (interactive (list (bbdb-search-read "names") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) :all-names regexp) layout))

;;;###autoload
(defun bbdb-search-organization (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the organization field."
  (interactive (list (bbdb-search-read "organization") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) :organization regexp)
                        layout))

;;;###autoload
(defun bbdb-search-address (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the address fields."
  (interactive (list (bbdb-search-read "address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) :address regexp)
                        layout))

;;;###autoload
(defun bbdb-search-mail (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the mail address."
  (interactive (list (bbdb-search-read "mail address") (bbdb-layout-prefix)))
  (bbdb-display-records (bbdb-search (bbdb-records) :mail regexp) layout))

;;;###autoload
(defun bbdb-search-phone (regexp &optional layout)
  "Display all records in the BBDB matching REGEXP in the phones field."
  (interactive (list (bbdb-search-read "phone") (bbdb-layout-prefix)))
  (bbdb-display-records
   (bbdb-search (bbdb-records) :phone regexp) layout))

;;;###autoload
(defun bbdb-search-xfields (field regexp &optional layout)
  "Display all BBDB records for which xfield FIELD matches REGEXP."
  (interactive
   (let ((field (completing-read "Xfield to search (RET for all): "
                                 (mapcar 'list bbdb-xfield-label-list) nil t)))
     (list (if (string= field "") '* (intern field))
           (bbdb-search-read (if (string= field "")
                                   "any xfield"
                                 field))
           (bbdb-layout-prefix))))
  (bbdb-display-records (bbdb-search (bbdb-records) :xfield (cons field regexp))
                        layout))
(define-obsolete-function-alias 'bbdb-search-notes 'bbdb-search-xfields "3.0")

;;;###autoload
(defun bbdb-search-changed (&optional layout)
  ;; FIXME: "changes" in BBDB lingo are often called "modifications"
  ;; in Emacs lingo
  "Display records which have been changed since BBDB was last saved."
  (interactive (list (bbdb-layout-prefix)))
  (if (bbdb-search-invert-p)
      (let (unchanged-records)
        (dolist (record (bbdb-records))
          (unless (memq record bbdb-changed-records)
            (push record unchanged-records)))
        (bbdb-display-records unchanged-records layout))
    (bbdb-display-records bbdb-changed-records layout)))

(defun bbdb-search-prog (fun &optional layout)
  "Search records using function FUN.
FUN is called with one argument, the record, and should return
the record to be displayed or nil otherwise."
  (bbdb-display-records (delq nil (mapcar fun (bbdb-records))) layout))


;; clean-up functions

;; Sometimes one gets mail from foo@bar.baz.com, and then later gets mail
;; from foo@baz.com.  At this point, one would like to delete the bar.baz.com
;; address, since the baz.com address is obviously superior.

(defun bbdb-mail-redundant-re (mail)
  "Return a regexp matching redundant variants of email address MAIL.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
Return nil if MAIL is not a valid plain email address.
In particular, ignore addresses \"Joe Smith <foo@baz.com>\"."
  (let* ((match (string-match "\\`\\([^ ]+\\)@\\(.+\\)\\'" mail))
         (name (and match (match-string 1 mail)))
         (host (and match (match-string 2 mail))))
    (if (and name host)
        (concat (regexp-quote name) "@.*\\." (regexp-quote host)))))

(defun bbdb-delete-redundant-mails (records &optional query update)
  "Delete redundant or duplicate mails from RECORDS.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
Duplicates may (but should not) occur if we feed BBDB automatically.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If QUERY is non-nil (as in interactive calls, unless we use a prefix arg)
query before deleting the redundant mail addresses.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity.

Noninteractively, this may be used as an element of `bbdb-notice-record-hook'
or `bbdb-change-hook'.  However, see also `bbdb-ignore-redundant-mails',
which is probably more suited for your needs."
  (interactive (list (bbdb-do-records) (not current-prefix-arg) t))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (let (mails redundant okay)
      ;; We do not look at the canonicalized mail addresses of RECORD.
      ;; An address "Joe Smith <foo@baz.com>" can only be entered manually
      ;; into BBDB, and we assume that this is what the user wants.
      ;; Anyway, if a mail field contains all the elements
      ;; foo@baz.com, "Joe Smith <foo@baz.com>", "Jonathan Smith <foo@baz.com>"
      ;; we do not know which address to keep and which ones to throw.
      (dolist (mail (bbdb-record-mail record))
        (if (assoc-string mail mails t) ; duplicate mail address
            (push mail redundant)
          (push mail mails)))
      (let ((mail-re (delq nil (mapcar 'bbdb-mail-redundant-re mails)))
            (case-fold-search t))
        (if (not (cdr mail-re)) ; at most one mail-re address to consider
            (setq okay (nreverse mails))
          (setq mail-re (concat "\\`\\(?:" (mapconcat 'identity mail-re "\\|")
                                "\\)\\'"))
          (dolist (mail mails)
            (if (string-match mail-re mail) ; redundant mail address
                (push mail redundant)
              (push mail okay)))))
      (let ((form (format "redundant mail%s %s"
                          (if (< 1 (length redundant)) "s" "")
                          (bbdb-concat 'mail (nreverse redundant)))))
        (when (and redundant
                   (or (not query)
                       (y-or-n-p (format "Delete %s: " form))))
          (unless query (message "Deleting %s" form))
          (bbdb-record-set-field record 'mail okay)
          (when update
            (bbdb-change-record record)))))))
(define-obsolete-function-alias 'bbdb-delete-duplicate-mails
  'bbdb-delete-redundant-mails "3.0")

(defun bbdb-search-duplicates (&optional fields)
  "Search all records that have duplicate entries for FIELDS.
The list FIELDS may contain the symbols `name', `mail', and `aka'.
If FIELDS is nil use all these fields.  With prefix, query for FIELDS.
The search results are displayed in the BBDB buffer."
  (interactive (list (if current-prefix-arg
                         (list (intern (completing-read "Field: "
                                                        '("name" "mail" "aka")
                                                        nil t))))))
  (setq fields (or fields '(name mail aka)))
  (let (hash ret)
    (dolist (record (bbdb-records))

      (when (and (memq 'name fields)
                 (bbdb-record-name record)
                 (setq hash (bbdb-gethash (bbdb-record-name record)
                                          '(fl-name lf-name aka)))
                 (> (length hash) 1))
        (setq ret (append hash ret))
        (message "BBDB record `%s' has duplicate name."
                 (bbdb-record-name record))
        (sit-for 0))

      (if (memq 'mail fields)
          (dolist (mail (bbdb-record-mail-canon record))
              (setq hash (bbdb-gethash mail '(mail)))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "BBDB record `%s' has duplicate mail `%s'."
                         (bbdb-record-name record) mail)
                (sit-for 0))))

      (if (memq 'aka fields)
          (dolist (aka (bbdb-record-aka record))
            (setq hash (bbdb-gethash aka '(fl-name lf-name aka)))
            (when (> (length hash) 1)
              (setq ret (append hash ret))
              (message "BBDB record `%s' has duplicate aka `%s'"
                       (bbdb-record-name record) aka)
              (sit-for 0)))))

    (bbdb-display-records (sort (delete-dups ret)
                                'bbdb-record-lessp))))

(defun bbdb-fix-records (records)
  "Fix broken RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records)))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    ;; For the fields which take a list of strings (affix, organization,
    ;; aka, and mail) `bbdb=record-set-field' calls `bbdb-list-strings'
    ;; which removes all elements from such a list which are not non-empty
    ;; strings.  This should fix most problems with these fields.
    (bbdb-record-set-field record 'affix (bbdb-record-affix record))
    (bbdb-record-set-field record 'organization (bbdb-record-organization record))
    (bbdb-record-set-field record 'aka (bbdb-record-aka record))
    (bbdb-record-set-field record 'mail (bbdb-record-mail record))
    (bbdb-change-record record))
  (bbdb-sort-records))

(defun bbdb-touch-records (records)
  "Touch RECORDS by calling `bbdb-change-hook' unconditionally.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records)))
  (bbdb-editable)
  (let ((bbdb-update-unchanged-records t))
    (dolist (record (bbdb-record-list records))
      (bbdb-change-record record))))

;;; Time-based functions

(defmacro bbdb-compare-records (cmpval label compare)
  "Builds a lambda comparison function that takes one argument, RECORD.
RECORD is returned if (COMPARE VALUE CMPVAL) is t, where VALUE
is the value of field LABEL of RECORD."
  `(lambda (record)
     (let ((val (bbdb-record-field record ,label)))
       (if (and val (,compare val ,cmpval))
           record))))

(defsubst bbdb-string> (a b)
  (not (or (string= a b)
           (string< a b))))

;;;###autoload
(defun bbdb-timestamp-older (date &optional layout)
  "Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Timestamp older than: (yyyy-mm-dd) ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp string<) layout))

;;;###autoload
(defun bbdb-timestamp-newer (date &optional layout)
  "Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Timestamp newer than: (yyyy-mm-dd) ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'timestamp bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-older (date &optional layout)
  "Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Creation older than: (yyyy-mm-dd) ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date string<) layout))

;;;###autoload
(defun bbdb-creation-newer (date &optional layout)
  "Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Creation newer than: (yyyy-mm-dd) ")
                     (bbdb-layout-prefix)))
  (bbdb-search-prog (bbdb-compare-records date 'creation-date bbdb-string>) layout))

;;;###autoload
(defun bbdb-creation-no-change (&optional layout)
  "Display records that have the same timestamp and creation-date."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-search-prog
   ;; RECORD is bound in `bbdb-compare-records'.
   (bbdb-compare-records (bbdb-record-timestamp record)
                         'creation-date string=)
   layout))

;;; Parsing phone numbers
;; XXX this needs expansion to handle international prefixes properly
;; i.e. +353-number without discarding the +353 part. Problem being
;; that this will necessitate yet another change in the database
;; format for people who are using north american numbers.

(defsubst bbdb-subint (string num)
  "Used for parsing phone numbers."
  (string-to-number (match-string num string)))

(defun bbdb-parse-phone (string &optional style)
  "Parse a phone number from STRING and return a list of integers the form
\(area-code exchange number extension).
This is both lenient and strict in what it will parse - whitespace may
appear (or not) between any of the groups of digits, parentheses around the
area code are optional, as is a dash between the exchange and number, and
a '1' preceeding the area code; but there must be three digits in the area
code and exchange, and four in the number (if they are present).
All of these are unambigously parsable:

  ( 415 ) 555 - 1212 x123   -> (415 555 1212 123)
  (415)555-1212 123         -> (415 555 1212 123)
  (1-415) 555-1212 123      -> (415 555 1212 123)
  1 (415)-555-1212 123      -> (415 555 1212 123)
  555-1212 123              -> (0 555 1212 123)
  555 1212                  -> (0 555 1212 0)
  415 555 1212              -> (415 555 1212 0)
  1 415 555 1212            -> (415 555 1212 0)
  5551212                   -> (0 555 1212 0)
  4155551212                -> (415 555 1212 0)
  4155551212123             -> (415 555 1212 123)
  5551212x123               -> (0 555 1212 123)
  1234                      -> (0 0 0 1234)

Note that \"4151212123\" is ambiguous; it could be interpreted either as
\"(415) 121-2123\" or as \"415-1212 x123\".

Return a list containing four numbers or one string."

  ;; RW: Missing parts of NANP numbers are replaced by zeros.
  ;; Is this always correct?  What about an extension zero?
  ;; Should we use nil instead of zeros?
  (unless style (setq style bbdb-phone-style))
  (let ((area-regexp (concat "(?[ \t]*\\+?1?[ \t]*[-\(]?[ \t]*[-\(]?[ \t]*"
                             "\\([2-9][0-9][0-9]\\)[ \t]*)?[-./ \t]*"))
        (main-regexp (concat "\\([1-9][0-9][0-9]\\)[ \t]*[-.]?[ \t]*"
                             "\\([0-9][0-9][0-9][0-9]\\)[ \t]*"))
        (ext-regexp "x?[ \t]*\\([0-9]+\\)[ \t]*"))
    (cond ((not (eq style 'nanp))
           (list (bbdb-string-trim string)))
          ((string-match ;; (415) 555-1212 x123
            (concat "^[ \t]*" area-regexp main-regexp ext-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) (bbdb-subint string 4)))
          ;; (415) 555-1212
          ((string-match (concat "^[ \t]*" area-regexp main-regexp "$") string)
           (list (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3) 0))
          ;; 555-1212 x123
          ((string-match (concat "^[ \t]*" main-regexp ext-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2)
                 (bbdb-subint string 3)))
          ;; 555-1212
          ((string-match (concat "^[ \t]*" main-regexp "$") string)
           (list 0 (bbdb-subint string 1) (bbdb-subint string 2) 0))
          ;; x123
          ((string-match (concat "^[ \t]*" ext-regexp "$") string)
           (list 0 0 0 (bbdb-subint string 1)))
          ;; We trust the user she knows what she wants
          (t (list (bbdb-string-trim string))))))

(defun bbdb-message-search (name mail)
  "Return list of BBDB records matching NAME and/or MAIL.
First try to find a record matching both NAME and MAIL.
If this fails try to find a record matching MAIL.
If this fails try to find a record matching NAME.
NAME may match FIRST_LAST, LAST_FIRST or AKA.

This function performs a fast search using `bbdb-hashtable'.
NAME and MAIL must be strings or nil.
See `bbdb-search' for searching records with regexps."
  (when (or name mail)
    (bbdb-buffer)  ; make sure database is loaded and up-to-date
    (let ((mrecords (if mail (bbdb-gethash mail '(mail))))
          (nrecords (if name (bbdb-gethash name '(fl-name lf-name aka)))))
      ;; (1) records matching NAME and MAIL
      (or (and mrecords nrecords
               (let (records)
                 (dolist (record nrecords)
                   (mapc (lambda (mr) (if (and (eq record mr)
                                               (not (memq record records)))
                                          (push record records)))
                         mrecords))
                 records))
          ;; (2) records matching MAIL
          mrecords
          ;; (3) records matching NAME
          nrecords))))

(defun bbdb-read-record (&optional first-and-last)
  "Read and return a new BBDB record.
Does not insert it into the database or update the hashtables,
but does ensure that there will not be name collisions."
  (bbdb-editable)
  (let ((record (bbdb-empty-record)))
    (let (name)
      (bbdb-error-retry
       (setq name (bbdb-read-name first-and-last))
       (bbdb-check-name (car name) (cdr name)))
      (bbdb-record-set-firstname record (car name))
      (bbdb-record-set-lastname record (cdr name)))

    ;; organization
    (bbdb-record-set-organization record (bbdb-read-organization))

    ;; mail
    (bbdb-record-set-mail
     record (bbdb-split 'mail (bbdb-read-string "E-Mail Addresses: ")))
    ;; address
    (let (addresses label address)
      (while (not (string= ""
                           (setq label
                                 (bbdb-read-string
                                  "Snail Mail Address Label [RET when done]: "
                                  nil
                                  bbdb-address-label-list))))
        (setq address (make-vector bbdb-address-length nil))
        (bbdb-record-edit-address address label t)
        (push address addresses))
      (bbdb-record-set-address record (nreverse addresses)))

    ;; phones
    (let (phones phone-list label)
      (while (not (string= ""
                           (setq label
                                 (bbdb-read-string
                                  "Phone Label [RET when done]: " nil
                                  bbdb-phone-label-list))))
        (setq phone-list
              (bbdb-error-retry
               (bbdb-parse-phone
                (read-string "Phone: "
                             (and (integerp bbdb-default-area-code)
                                  (format "(%03d) "
                                          bbdb-default-area-code))))))
        (push (apply 'vector label phone-list) phones))
      (bbdb-record-set-phone record (nreverse phones)))

    ;; `bbdb-default-xfield'
    (let ((xfield (bbdb-read-xfield bbdb-default-xfield)))
      (unless (string= "" xfield)
        (bbdb-record-set-xfields
         record (list (cons bbdb-default-xfield xfield)))))

    record))

(defun bbdb-read-name (&optional first-and-last dfirst dlast)
  "Read name for a record from minibuffer.
FIRST-AND-LAST controls the reading mode:
If it is 'first-last read first and last name separately.
If it is 'last-first read last and first name separately.
If it is 'fullname read full name at once.
If it is t read name parts separately, obeying `bbdb-read-name-format' if possible.
Otherwise use `bbdb-read-name-format'.
DFIRST and DLAST are default values for the first and last name.
Return cons with first and last name."
  (unless (memq first-and-last '(first-last last-first fullname))
    ;; We do not yet know how to read the name
    (setq first-and-last
          (if (and first-and-last
                   (not (memq bbdb-read-name-format '(first-last last-first))))
              'first-last
            bbdb-read-name-format)))
  (let ((name (cond ((eq first-and-last 'last-first)
                     (let (fn ln)
                       (setq ln (bbdb-read-string "Last Name: " dlast)
                             fn (bbdb-read-string "First Name: " dfirst))
                       (cons fn ln)))
                    ((eq first-and-last 'first-last)
                     (cons (bbdb-read-string "First Name: " dfirst)
                           (bbdb-read-string "Last Name: " dlast)))
                    (t
                     (bbdb-divide-name (bbdb-read-string
                                        "Name: " (bbdb-concat 'name-first-last
                                                              dfirst dlast)))))))
    (if (string= (car name) "") (setcar name nil))
    (if (string= (cdr name) "") (setcdr name nil))
    name))

;;;###autoload
(defun bbdb-create (record)
  "Add a new RECORD to BBDB.
When called interactively read all relevant info.
Do not call this from a program; call `bbdb-create-internal' instead."
  (interactive (list (bbdb-read-record current-prefix-arg)))
  (bbdb-change-record record)
  (bbdb-display-records (list record)))

(defsubst bbdb-split-maybe (separator string)
  "Split STRING into list of substrings bounded by matches for SEPARATORS.
If STRING is a list, return STRING.  Throw error if STRING is neither a string
nor a list."
  (cond ((stringp string)
         (bbdb-split separator string))
        ((listp string) string)
        (t (error "Cannot convert %s to list" string))))

;;;###autoload
(defun bbdb-create-internal (&rest spec)
  "Add a new record to the database and return it.

The following keywords are supported in SPEC:
:name VAL          String or a cons cell (FIRST . LAST), the name of the person.
                   An error is thrown if VAL is already in use
                   and `bbdb-allow-duplicates' is nil.
:affix VAL         List of strings.
:aka VAL           List of strings.
:organization VAL  List of strings.
:mail VAL          String with comma-separated mail address
                   or a list of strings.
                   An error is thrown if a mail address in MAIL is already
                   in use and `bbdb-allow-duplicates' is nil.
:phone VAL         List of phone-number objects.  A phone-number is a vector
                   [\"label\" areacode prefix suffix extension-or-nil]
                   or [\"label\" \"phone-number\"]
:address VAL       List of addresses.  An address is a vector of the form
                   \[\"label\" (\"line1\" \"line2\" ... ) \"City\"
                   \"State\" \"Postcode\" \"Country\"].
:xfields VAL       Alist associating symbols with strings.
:uuid VAL          String, the uuid.
:creation-date VAL String, the creation date.
:check             If present, throw an error if a field value is not
                   syntactically correct."
  (bbdb-editable)
  (let ((record (bbdb-empty-record))
        (record-type (cdr bbdb-record-type))
        (check (prog1 (memq :check spec)
                 (setq spec (delq :check spec))))
        keyw)

    ;; Check keys.
    (while (keywordp (setq keyw (car spec)))
      (setq spec (cdr spec))
      (pcase keyw
	(`:name
         (let ((name (pop spec)))
           (cond ((stringp name)
                  (setq name (bbdb-divide-name name)))
                 (check (bbdb-check-type name '(or (const nil)
                                                   (cons string string))
                                         t)))
           (let ((firstname (car name))
                 (lastname (cdr name)))
             (bbdb-check-name firstname lastname) ; check for duplicates
             (bbdb-record-set-firstname record firstname)
             (bbdb-record-set-lastname record lastname))))

        (`:affix
         (let ((affix (bbdb-split-maybe 'affix (pop spec))))
           (if check (bbdb-check-type affix (bbdb-record-affix record-type) t))
           (bbdb-record-set-affix record affix)))

        (`:organization
         (let ((organization (bbdb-split-maybe 'organization (pop spec))))
           (if check (bbdb-check-type
                      organization (bbdb-record-organization record-type) t))
           (bbdb-record-set-organization record organization)))

        (`:aka
         (let ((aka (bbdb-split-maybe 'aka (pop spec))))
           (if check (bbdb-check-type aka (bbdb-record-aka record-type) t))
           (bbdb-record-set-aka record aka)))

        (`:mail
         (let ((mail (bbdb-split-maybe 'mail (pop spec))))
           (if check (bbdb-check-type mail (bbdb-record-mail record-type) t))
           (unless bbdb-allow-duplicates
             (dolist (elt mail)
               (if (bbdb-gethash elt '(mail))
                   (error "%s is already in the database" elt))))
           (bbdb-record-set-mail record mail)))

        (`:phone
         (let ((phone (pop spec)))
           (if check (bbdb-check-type phone (bbdb-record-phone record-type) t))
           (bbdb-record-set-phone phone record)))

        (`:address
         (let ((address (pop spec)))
           (if check (bbdb-check-type address (bbdb-record-address record-type) t))
           (bbdb-record-set-address record address)))

        (`:xfields
         (let ((xfields (pop spec)))
           (if check (bbdb-check-type xfields (bbdb-record-xfields record-type) t))
           (bbdb-record-set-xfields record xfields)))

        (`:uuid
         (let ((uuid (pop spec)))
           (if check (bbdb-check-type uuid (bbdb-record-uuid record-type) t))
           (bbdb-record-set-uuid record uuid)))

        (`:creation-date
         (let ((creation-date (pop spec)))
           (if check (bbdb-check-type
                      creation-date (bbdb-record-creation-date record-type) t))
           (bbdb-record-set-creation-date record creation-date)))

        (_ (error "Keyword `%s' undefined" keyw))))

    (bbdb-change-record record)))

;;;###autoload
(defun bbdb-insert-field (record field value)
  "For RECORD, add a new FIELD with value VALUE.
Interactively, read FIELD and VALUE; RECORD is the current record.
A non-nil prefix arg is passed on to `bbdb-read-field' as FLAG (see there)."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (list (append bbdb-xfield-label-list
                        '(affix organization aka phone address mail)))
          (field "")
          (completion-ignore-case t)
          (present (mapcar 'car (bbdb-record-xfields record))))
     (if (bbdb-record-affix record) (push 'affix present))
     (if (bbdb-record-organization record) (push 'organization present))
     (if (bbdb-record-mail record) (push 'mail present))
     (if (bbdb-record-aka record) (push 'aka present))
     (dolist (field present)
       (setq list (remq field list)))
     (setq list (mapcar 'symbol-name list))
     (while (string= field "")
       (setq field (downcase (completing-read "Insert Field: " list))))
     (setq field (intern field))
     (if (memq field present)
         (error "Field \"%s\" already exists" field))
     (list record field (bbdb-read-field record field current-prefix-arg))))

  (cond (;; affix
         (eq field 'affix)
         (if (bbdb-record-affix record)
             (error "Affix field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'affix value)))
         (bbdb-record-set-field record 'affix value))
        ;; organization
        ((eq field 'organization)
         (if (bbdb-record-organization record)
             (error "Organization field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'organization value)))
         (bbdb-record-set-field record 'organization value))
        ;; phone
        ((eq field 'phone)
         (bbdb-record-set-field record 'phone
                                (nconc (bbdb-record-phone record)
                                       (list value))))
        ;; address
        ((eq field 'address)
         (bbdb-record-set-field record 'address
                                (nconc (bbdb-record-address record)
                                       (list value))))
        ;; mail
        ((eq field 'mail)
         (if (bbdb-record-mail record)
             (error "Mail field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'mail value)))
         (bbdb-record-set-field record 'mail value))
        ;; AKA
        ((eq field 'aka)
         (if (bbdb-record-aka record)
             (error "Alternate names field exists already"))
         (if (stringp value)
             (setq value (bbdb-split 'aka value)))
         (bbdb-record-set-field record 'aka value))
        ;; xfields
        ((assq field (bbdb-record-xfields record))
         (error "Xfield \"%s\" already exists" field))
        (t
         (bbdb-record-set-xfield record field value)))
  (unless (bbdb-change-record record)
    (message "Record unchanged")))

(defun bbdb-read-field (record field &optional flag)
  "For RECORD read new FIELD interactively.
- The phone number style is controlled via `bbdb-phone-style'.
  A prefix FLAG inverts the style,
- If a mail address lacks a domain, append `bbdb-default-domain'
  if this variable non-nil.  With prefix FLAG do not alter the mail address.
- The value of an xfield is a string.  With prefix FLAG the value may be
  any lisp object."
  (let* ((init-f (intern-soft (concat "bbdb-init-" (symbol-name field))))
         (init (if (and init-f (functionp init-f))
                   (funcall init-f record))))
    (cond (;; affix
           (eq field 'affix) (bbdb-read-string "Affix: " init))
          ;; organization
          ((eq field 'organization) (bbdb-read-organization init))
          ;; mail
          ((eq field 'mail)
           (let ((mail (bbdb-read-string "Mail: " init)))
             (if (string-match "^mailto:" mail)
                 (setq mail (substring mail (match-end 0))))
             (if (or (not bbdb-default-domain)
                     flag (string-match "[@%!]" mail))
                 mail
               (concat mail "@" bbdb-default-domain))))
          ;; AKA
          ((eq field 'aka) (bbdb-read-string "Alternate Names: " init))
          ;; Phone
          ((eq field 'phone)
           (let ((bbdb-phone-style
                  (if flag (if (eq bbdb-phone-style 'nanp) nil 'nanp)
                    bbdb-phone-style)))
             (apply 'vector
                    (bbdb-read-string "Label: " nil bbdb-phone-label-list)
                    (bbdb-error-retry
                     (bbdb-parse-phone
                      (read-string "Phone: "
                                   (and (integerp bbdb-default-area-code)
                                        (format "(%03d) "
                                                bbdb-default-area-code))))))))
          ;; Address
          ((eq field 'address)
           (let ((address (make-vector bbdb-address-length nil)))
             (bbdb-record-edit-address address nil t)
             address))
          ;; xfield
          ((or (memq field bbdb-xfield-label-list)
               ;; New xfield
               (y-or-n-p
                (format "\"%s\" is an unknown field name.  Define it? " field))
               (error "Aborted"))
           (bbdb-read-xfield field init flag)))))

;;;###autoload
(defun bbdb-edit-field (record field &optional value flag)
  "Edit the contents of FIELD of RECORD.
If point is in the middle of a multi-line field (e.g., address),
then the entire field is edited, not just the current line.
For editing phone numbers or addresses, VALUE must be the phone number
or address that gets edited. An error is thrown when attempting to edit
a phone number or address with VALUE being nil.

- The value of an xfield is a string.  With prefix FLAG the value may be
  any lisp object."
  (interactive
   (save-excursion
     (bbdb-editable)
     ;; when at the end of the line take care of it
     (if (and (eolp) (not (bobp)) (not (bbdb-current-field)))
         (backward-char 1))
     (let* ((field-l (bbdb-current-field))
            (field (car field-l))
            (value (nth 1 field-l)))
       (unless field (error "Point not in a field"))
       (list (bbdb-current-record)
             (if (memq field '(name affix organization aka mail phone address
                                    uuid creation-date timestamp))
                 field ; not an xfield
               (elt value 0)) ; xfield
             value current-prefix-arg))))
  (let (edit-str)
    (cond ((memq field '(firstname lastname xfields))
           ;; FIXME: We could also edit first and last names.
           (error "Field `%s' not editable this way." field))
          ((eq field 'name)
           (bbdb-error-retry
            (bbdb-record-set-field
             record 'name
             (bbdb-read-name
              (if flag
                  ;; Here we try to obey the name-format xfield for
                  ;; editing the name field.  Is this useful?  Or is this
                  ;; irritating overkill and we better obey consistently
                  ;; `bbdb-read-name-format'?
                  (or (bbdb-record-xfield-intern record 'name-format)
                      flag))
              (bbdb-record-firstname record)
              (bbdb-record-lastname record)))))

          ((eq field 'phone)
           (unless value (error "No phone specified"))
           (bbdb-record-edit-phone (bbdb-record-phone record) value))
          ((eq field 'address)
           (unless value (error "No address specified"))
           (bbdb-record-edit-address value nil flag))
          ((eq field 'organization)
           (bbdb-record-set-field
            record field
            (bbdb-read-organization
             (bbdb-concat field (bbdb-record-organization record)))))
          ((setq edit-str (assq field '((affix . "Affix")
                                        (mail . "Mail") (aka . "AKA"))))
           (bbdb-record-set-field
            record field
            (bbdb-split field (bbdb-read-string
                               (format "%s: " (cdr edit-str))
                               (bbdb-concat field
                                            (bbdb-record-field record field))))))
          ((eq field 'uuid)
           (bbdb-record-set-field
            record 'uuid (bbdb-read-string "uuid (edit at your own risk): " (bbdb-record-uuid record))))
          ((eq field 'creation-date)
           (bbdb-record-set-creation-date
            record (bbdb-read-string "creation-date: " (bbdb-record-creation-date record))))
          ;; The timestamp is set automatically whenever we save a modified record.
          ;; So any editing gets overwritten.
          ((eq field 'timestamp)) ; do nothing
          (t ; xfield
           (bbdb-record-set-xfield
            record field
            (bbdb-read-xfield field (bbdb-record-xfield record field) flag))))
    (cond ((eq field 'timestamp)
           (message "timestamp not editable"))
          ((bbdb-change-record record))
          (t (message "Record unchanged")))))

(defun bbdb-edit-foo (record field &optional nvalue)
  "For RECORD edit some FIELD (mostly interactively).
FIELD may take the same values as the elements of the variable `bbdb-edit-foo'.
If FIELD is 'phone or 'address, NVALUE should be an integer in order to edit
the NVALUEth phone or address field; otherwise insert a new phone or address
field.

Interactively, if called without a prefix, the value of FIELD is the car
of the variable `bbdb-edit-foo'.  When called with a prefix, the value
of FIELD is the cdr of this variable.  Then use minibuffer completion
to select the field."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (bbdb-current-record))
          (tmp (if current-prefix-arg (cdr bbdb-edit-foo) (car bbdb-edit-foo)))
          (field (if (memq tmp '(current-fields all-fields))
                     ;; Do not require match so that we can define new xfields.
                     (intern (completing-read
                              "Edit field: " (mapcar 'list (if (eq tmp 'all-fields)
                                                               (append '(name affix organization aka mail phone address uuid creation-date)
                                                                       bbdb-xfield-label-list)
                                                             (append (if (bbdb-record-affix record) '(affix))
                                                                     (if (bbdb-record-organization record) '(organization))
                                                                     (if (bbdb-record-aka record) '(aka))
                                                                     (if (bbdb-record-mail record) '(mail))
                                                                     (if (bbdb-record-phone record) '(phone))
                                                                     (if (bbdb-record-address record) '(address))
                                                                     (mapcar 'car (bbdb-record-xfields record))
                                                                     '(name uuid creation-date))))))
                   tmp))
          ;; Multiple phone and address fields may use the same label.
          ;; So we cannot use these labels to uniquely identify
          ;; a phone or address field.  So instead we number these fields
          ;; consecutively.  But we do use the labels to annotate the numbers
          ;; (available starting from GNU Emacs 24.1).
          (nvalue (cond ((eq field 'phone)
                         (let* ((phones (bbdb-record-phone record))
                                (collection (cons (cons "new" "new phone #")
                                                  (mapcar (lambda (n)
                                                            (cons (format "%d" n) (bbdb-phone-label (nth n phones))))
                                                          (number-sequence 0 (1- (length phones))))))
                                (completion-extra-properties
                                 `(:annotation-function
                                   (lambda (s) (format "  (%s)" (cdr (assoc s ',collection)))))))
                           (if (< 0 (length phones))
                               (completing-read "Phone field: " collection nil t)
                             "new")))
                        ((eq field 'address)
                         (let* ((addresses (bbdb-record-address record))
                                (collection (cons (cons "new" "new address")
                                                  (mapcar (lambda (n)
                                                            (cons (format "%d" n) (bbdb-address-label (nth n addresses))))
                                                          (number-sequence 0 (1- (length addresses))))))
                                (completion-extra-properties
                                 `(:annotation-function
                                   (lambda (s) (format "  (%s)" (cdr (assoc s ',collection)))))))
                           (if (< 0 (length addresses))
                               (completing-read "Address field: " collection nil t)
                             "new"))))))
     (list record field (and (stringp nvalue)
                             (if (string= "new" nvalue)
                                 'new
                               (string-to-number nvalue))))))

  (if (memq field '(firstname lastname name-lf aka-all mail-aka mail-canon))
      (error "Field `%s' illegal" field))
  (let ((value (if (numberp nvalue)
                   (nth nvalue (cond ((eq field 'phone) (bbdb-record-phone record))
                                     ((eq field 'address) (bbdb-record-address record))
                                     (t (error "%s: nvalue %s meaningless" field nvalue)))))))
    (if (and (numberp nvalue) (not value))
        (error "%s: nvalue %s out of range" field nvalue))
    (if (or (memq field '(name uuid creation-date))
            (and (eq field 'affix) (bbdb-record-affix record))
            (and (eq field 'organization) (bbdb-record-organization record))
            (and (eq field 'mail) (bbdb-record-mail record))
            (and (eq field 'aka) (bbdb-record-aka record))
            (assq field (bbdb-record-xfields record))
            value)
        (bbdb-edit-field record field value)
      (bbdb-insert-field record field
                         (bbdb-read-field record field)))))

(defun bbdb-read-xfield (field &optional init sexp)
  "Read xfield FIELD with optional INIT.
This calls bbdb-read-xfield-FIELD if it exists."
  (let ((read-fun (intern-soft (format "bbdb-read-xfield-%s" field))))
    (cond ((fboundp read-fun)
           (funcall read-fun init))
          ((and (not sexp) (string-or-null-p init))
           (bbdb-read-string (format "%s: " field) init))
          (t (read-minibuffer (format "%s (sexp): " field)
                              (prin1-to-string init))))))

(defun bbdb-read-organization (&optional init)
  "Read organization."
  (if (string< "24.3" (substring emacs-version 0 4))
      (let ((crm-separator
             (concat "[ \t\n]*"
                     (cadr (assq 'organization bbdb-separator-alist))
                     "[ \t\n]*"))
            (crm-local-completion-map bbdb-crm-local-completion-map))
        (completing-read-multiple "Organizations: " bbdb-organization-list
                                  nil nil init))
    (bbdb-split 'organization (bbdb-read-string "Organizations: " init))))

(defun bbdb-record-edit-address (address &optional label ignore-country)
  "Edit ADDRESS.
If LABEL is nil, edit the label sub-field of the address as well.
If the country field of ADDRESS is nonempty and IGNORE-COUNTRY is nil,
use the rule from `bbdb-address-format-list' matching this country.
Otherwise, use the default rule according to `bbdb-address-format-list'."
  (unless label
    (setq label (bbdb-read-string "Label: "
                                  (bbdb-address-label address)
                                  bbdb-address-label-list)))
  (let ((country (or (bbdb-address-country address) ""))
        new-addr edit)
    (unless (or ignore-country (string= "" country))
      (let ((list bbdb-address-format-list)
            identifier elt)
        (while (and (not edit) (setq elt (pop list)))
          (setq identifier (car elt))
          (if (or (and (listp identifier)
                       (member-ignore-case country identifier))
                  (and (functionp identifier)
                       (funcall identifier address)))
              (setq edit (nth 1 elt))))))
    (unless edit
      (setq edit (nth 1 (assq t bbdb-address-format-list))))
    (unless edit (error "No address editing function defined"))
    (if (functionp edit)
        (setq new-addr (funcall edit address))
      (setq new-addr (make-vector 5 ""))
      (dolist (elt (string-to-list edit))
        (cond ((eq elt ?s)
               (aset new-addr 0 (bbdb-edit-address-street
                                 (bbdb-address-streets address))))
              ((eq elt ?c)
               (aset new-addr 1 (bbdb-read-string
                                 "City: " (bbdb-address-city address)
                                 bbdb-city-list)))
              ((eq elt ?S)
               (aset new-addr 2 (bbdb-read-string
                                 "State: " (bbdb-address-state address)
                                 bbdb-state-list)))
              ((eq elt ?p)
               (aset new-addr 3
                     (bbdb-error-retry
                      (bbdb-parse-postcode
                       (bbdb-read-string
                        "Postcode: " (bbdb-address-postcode address)
                        bbdb-postcode-list)))))
              ((eq elt ?C)
               (aset new-addr 4
                     (bbdb-read-string
                      "Country: " (or (bbdb-address-country address)
                                      bbdb-default-country)
                      bbdb-country-list))))))
    (bbdb-address-set-label address label)
    (bbdb-address-set-streets address (elt new-addr 0))
    (bbdb-address-set-city address (elt new-addr 1))
    (bbdb-address-set-state address (elt new-addr 2))
    (bbdb-address-set-postcode address (elt new-addr 3))
    (if (string= "" (bbdb-concat "" (elt new-addr 0) (elt new-addr 1)
                                 (elt new-addr 2) (elt new-addr 3)
                                 (elt new-addr 4)))
        ;; User did not enter anything. this causes a display bug.
        ;; The following is a temporary fix.  Ideally, we would simply discard
        ;; the entire address, but that requires bigger hacking.
        (bbdb-address-set-country address "Emacs")
      (bbdb-address-set-country address (elt new-addr 4)))))

(defun bbdb-edit-address-street (streets)
  "Edit list STREETS."
  (let ((n 0) street list)
    (while (not (string= "" (setq street
                                  (bbdb-read-string
                                   (format "Street, line %d: " (1+ n))
                                   (nth n streets) bbdb-street-list))))
      (push street list)
      (setq n (1+ n)))
    (reverse list)))

;; This function can provide some guidance for writing
;; your own address editing function
(defun bbdb-edit-address-default (address)
  "Function to use for address editing.
The sub-fields and the prompts used are:
Street, line n:  (nth n street)
City:            city
State:           state
Postcode:        postcode
Country:         country"
  (list (bbdb-edit-address-street (bbdb-address-streets address))
        (bbdb-read-string "City: " (bbdb-address-city address) bbdb-city-list)
        (bbdb-read-string "State: " (bbdb-address-state address)
                          bbdb-state-list)
        (bbdb-error-retry
         (bbdb-parse-postcode
          (bbdb-read-string "Postcode: " (bbdb-address-postcode address)
                            bbdb-postcode-list)))
        (bbdb-read-string "Country: " (or (bbdb-address-country address)
                                          bbdb-default-country)
                          bbdb-country-list)))

(defun bbdb-record-edit-phone (phones phone)
  "For list PHONES edit PHONE number."
  ;; Phone numbers are special.  They are vectors with either
  ;; two or four elements.  We do not know whether after editing PHONE
  ;; we still have a number requiring the same format as PHONE.
  ;; So we take all numbers PHONES of the record so that we can
  ;; replace the element PHONE in PHONES.
  (setcar (memq phone phones)
          (apply 'vector
                 (bbdb-read-string "Label: "
                                   (bbdb-phone-label phone)
                                   bbdb-phone-label-list)
                 (bbdb-error-retry
                  (bbdb-parse-phone
                   (read-string "Phone: " (bbdb-phone-string phone)))))))

;; (bbdb-list-transpose '(a b c d) 1 3)
(defun bbdb-list-transpose (list i j)
  "For LIST transpose elements I and J destructively.
I and J start with zero.  Return the modified LIST."
  (if (eq i j)
      list ; ignore that i, j could be invalid
    (let (a b c)
      ;; Travel down LIST only once
      (if (> i j) (setq a i i j j a)); swap
      (setq a (nthcdr i list)
            b (nthcdr (- j i) a)
            c (car b))
      (unless b (error "Args %i, %i beyond length of list." i j))
      (setcar b (car a))
      (setcar a c)
      list)))

(defun bbdb-ident-point (&optional point)
  "Return identifier (RECNUM FIELD NUM) for position POINT.
If POINT is nil use current value of point.
RECNUM is the number of the record (starting from zero).
FIELD is the field type.
If FIELD's value is a list, NUM is the position of the subfield within FIELD.
If any of these terms is not defined at POINT, the respective value is nil."
  (unless point (setq point (point)))
  (let ((recnum (get-text-property point 'bbdb-record-number))
        (field (get-text-property point 'bbdb-field)))
    (cond ((not field)
           (list recnum nil nil))
          ((eq (car field) 'name)
           (list recnum 'name nil))
          ((not (nth 1 field))
           (list recnum (car field) nil))
          (t
           (let* ((record (car (nth recnum bbdb-records)))
                  (fields (bbdb-record-field record (car field)))
                  (val (nth 1 field))
                  (num 0) done elt)
             ;; For xfields we only check the label because the rest of VAL
             ;; can be anything.  (xfields are unique within a record.)
             (if (eq 'xfields (car field))
                 (setq val (car val)
                       fields (mapcar 'car fields)))
             (while (and (not done) (setq elt (pop fields)))
               (if (eq val elt)
                   (setq done t)
                 (setq num (1+ num))))
             (unless done (error "Field %s not found" val))
             (list recnum (car field) num))))))

;;;###autoload
(defun bbdb-transpose-fields (arg)
  "Transpose previous and current field of a BBDB record.
With numeric prefix ARG, take previous field and move it past ARG fields.
With region active or ARG 0, transpose field point is in and field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone numbers
or email addresses are listed, but you cannot use it to make an address appear
before a phone number; the order of field types is fixed).

If the current field is the name field, transpose first and last name,
irrespective of the value of ARG."
  ;; This functionality is inspired by `transpose-lines'.
  (interactive "p")
  (bbdb-editable)
  (let* ((ident (bbdb-ident-point))
         (record (and (car ident) (car (nth (car ident) bbdb-records))))
         num1 num2)
    (cond ((not (car ident))
           (error "Point not in BBDB record"))
          ((not (nth 1 ident))
           (error "Point not in BBDB field"))
          ((eq 'name (nth 1 ident))
           ;; Transpose firstname and lastname
           (bbdb-record-set-name record (bbdb-record-lastname record)
                                 (bbdb-record-firstname record)))
          ((not (integerp arg))
           (error "Arg `%s' not an integer" arg))
          ((not (nth 2 ident))
           (error "Point not in a transposable field"))
          (t
           (if (or (use-region-p) (zerop arg))
               (let ((ident2 (bbdb-ident-point
                              (or (mark) (error "No mark set in this buffer")))))
                 (unless (and (eq (car ident) (car ident2))
                              (eq (cadr ident) (cadr ident2))
                              (integerp (nth 2 ident2)))
                   (error "Mark (or point) not on transposable field"))
                 (setq num1 (nth 2 ident)
                       num2 (nth 2 ident2)))
             (setq num1 (1- (nth 2 ident))
                   num2 (+ num1 arg))
             (if (or (< (min num1 num2) 0)
                     (>= (max num1 num2) (length (bbdb-record-field
                                                  record (nth 1 ident)))))
                 (error "Cannot transpose fields of different types")))
           (bbdb-record-set-field
            record (nth 1 ident)
            (bbdb-list-transpose (bbdb-record-field record (nth 1 ident))
                                 num1 num2))))
    (bbdb-change-record record)))

;;;###autoload
(defun bbdb-delete-field-or-record (records field &optional noprompt)
  "For RECORDS delete FIELD.
If FIELD is the `name' field, delete RECORDS from datanbase.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records',
and FIELD is the field point is on.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  ;; The value of FIELD is whatever `bbdb-current-field' returns.
  ;; This way we can identify more accurately what really needs
  ;; to be done.
  (interactive
   (list (bbdb-do-records) (bbdb-current-field) current-prefix-arg))
  (bbdb-editable)
  (unless field (error "Not a field"))
  (setq records (bbdb-record-list records))
  (let* ((type (car field))
         (type-x (if (eq type 'xfields)
                     (car (nth 1 field))
                   type)))
    (if (eq type 'name)
        (bbdb-delete-records records noprompt)
      (if (memq type '(firstname lastname))
          (error "Cannot delete field `%s'" type))
      (dolist (record records)
        (when (or noprompt
                  (y-or-n-p (format "delete this `%s' field (of %s)? "
                                    type-x (bbdb-record-name record))))
          (cond ((memq type '(phone address))
                 (bbdb-record-set-field
                  record type
                  ;; We use `delete' which deletes all phone and address
                  ;; fields equal to the current one.  This works for
                  ;; multiple records.
                  (delete (nth 1 field)
                          (bbdb-record-field record type))))
                ((memq type '(affix organization mail aka))
                 (bbdb-record-set-field record type nil))
                ((eq type 'xfields)
                 (bbdb-record-set-xfield record type-x nil))
                (t (error "Unknown field %s" type)))
          (bbdb-change-record record))))))

;;;###autoload
(defun bbdb-delete-records (records &optional noprompt)
  "Delete RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  (interactive (list (bbdb-do-records) current-prefix-arg))
  (bbdb-editable)
  (let ((all-records (bbdb-with-db-buffer bbdb-records)))
    (dolist (record (bbdb-record-list records))
      (cond ((not (memq record all-records))
             ;; Possibly we changed RECORD before deleting it.
             ;; Otherwise, do nothing if RECORD is unknown to BBDB.
             (setq bbdb-changed-records (delq record bbdb-changed-records)))
            ((or noprompt
                 (y-or-n-p (format "Delete the BBDB record of %s? "
                                   (or (bbdb-record-name record)
                                       (car (bbdb-record-mail record))))))
             (bbdb-delete-record-internal record t)
             (setq bbdb-changed-records (delq record bbdb-changed-records)))))))

;;;###autoload
(defun bbdb-display-all-records (&optional layout)
  "Show all records.
If invoked in a *BBDB* buffer point stays on the currently visible record.
Inverse of `bbdb-display-current-record'."
  (interactive (list (bbdb-layout-prefix)))
  (let ((current (ignore-errors (bbdb-current-record))))
    (bbdb-display-records (bbdb-records) layout)
    (when (setq current (assq current bbdb-records))
      (redisplay) ; Strange display bug??
      (goto-char (nth 2 current)))))
      ;; (set-window-point (selected-window) (nth 2 current)))))

;;;###autoload
(defun bbdb-display-current-record (&optional layout)
  "Narrow to current record.  Inverse of `bbdb-display-all-records'."
  (interactive (list (bbdb-layout-prefix)))
  (bbdb-display-records (list (bbdb-current-record)) layout))

(defun bbdb-change-records-layout (records layout)
  (dolist (record records)
    (unless (eq layout (nth 1 record))
      (setcar (cdr record) layout)
      (bbdb-redisplay-record (car record)))))

;;;###autoload
(defun bbdb-toggle-records-layout (records &optional arg)
  "Toggle layout of RECORDS (elided or expanded).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
With prefix ARG 0, RECORDS are displayed elided.
With any other non-nil ARG, RECORDS are displayed expanded."
  (interactive (list (bbdb-do-records t) current-prefix-arg))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout-alist
          ;; Try to consider only those layouts that have the `toggle'
          ;; option set
          (or (delq nil (mapcar (lambda (l)
                                    (if (and (assq 'toggle l)
                                             (cdr (assq 'toggle l)))
                                        l))
                                  bbdb-layout-alist))
              bbdb-layout-alist))
         (layout
          (cond ((eq arg 0)
                 'one-line)
                ((null current-layout)
                 'multi-line)
                 ;; layout is not the last element of layout-alist
                 ;; and we switch to the following element of layout-alist
                ((caar (cdr (memq (assq current-layout layout-alist)
                                  layout-alist))))
                (t ; layout is the last element of layout-alist
                 ;;  and we switch to the first element of layout-alist
                 (caar layout-alist)))))
    (message "Using %S layout" layout)
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-completely (records)
  "Display RECORDS using layout `full-multi-line' (i.e., display all fields).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let* ((record (bbdb-current-record))
         (current-layout (nth 1 (assq record bbdb-records)))
         (layout (if (not (eq current-layout 'full-multi-line))
                     'full-multi-line
                   'multi-line)))
    (bbdb-change-records-layout (bbdb-record-list records t) layout)))

;;;###autoload
(defun bbdb-display-records-with-layout (records layout)
  "Display RECORDS using LAYOUT.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive
   (list (bbdb-do-records t)
         (intern (completing-read "Layout: "
                                  (mapcar (lambda (i)
                                            (list (symbol-name (car i))))
                                          bbdb-layout-alist)))))
  (bbdb-change-records-layout (bbdb-record-list records t) layout))

;;;###autoload
(defun bbdb-omit-record (n)
  "Remove current record from the display without deleting it from BBDB.
With prefix N, omit the next N records.  If negative, omit backwards."
  (interactive "p")
  (let ((num  (get-text-property (if (and (not (bobp)) (eobp))
                                     (1- (point)) (point))
                                 'bbdb-record-number)))
    (if (> n 0)
        (setq n (min n (- (length bbdb-records) num)))
      (setq n (min (- n) num))
      (bbdb-prev-record n))
    (dotimes (_i n)
      (bbdb-redisplay-record (bbdb-current-record) nil t))))

;;; Fixing up bogus records

;;;###autoload
(defun bbdb-merge-records (record1 record2)
  "Merge RECORD1 into RECORD2, then delete RECORD1 and return RECORD2.
If both records have name fields ask which one to use.
Concatenate other fields, ignoring duplicates.
RECORD1 need not be known to BBDB, its hash and cache are ignored.
Update hash and cache for RECORD2.

Interactively, RECORD1 is the current record; prompt for RECORD2.
With prefix, RECORD2 defaults to the first record with the same name."
  (interactive
   (let* ((_ (bbdb-editable))
          (record1 (bbdb-current-record))
          (name (bbdb-record-name record1))
          (record2 (and current-prefix-arg
                           ;; take the first record with the same name
                           (car (delq record1
                                      (bbdb-search (bbdb-records) :all-names name))))))
     (when record2
       (message "Merge current record with duplicate record `%s'" name)
       (sit-for 1))
     (list record1
           (or record2
               (bbdb-completing-read-record
                (format "merge record \"%s\" into: "
                        (or (bbdb-record-name record1)
                            (car (bbdb-record-mail record1))
                            "???"))
                (list record1))))))

  (bbdb-editable)
  (cond ((eq record1 record2) (error "Records are equal"))
        ((null record2) (error "No record to merge with")))

  ;; Merge names
  (let* ((new-name (bbdb-record-name record2))
         (old-name (bbdb-record-name record1))
         (old-aka  (bbdb-record-aka  record1))
         extra-name
         (name
          (cond ((or (string= "" old-name)
                     (bbdb-string= old-name new-name))
                 (cons (bbdb-record-firstname record2)
                       (bbdb-record-lastname record2)))
                ((string= "" new-name)
                 (cons (bbdb-record-firstname record1)
                       (bbdb-record-lastname record1)))
                (t (prog1
                       (if (y-or-n-p
                            (format "Use name \"%s\" instead of \"%s\"? "
                                    old-name new-name))
                           (progn
                             (setq extra-name new-name)
                             (cons (bbdb-record-firstname record1)
                                   (bbdb-record-lastname record1)))
                         (setq extra-name old-name)
                         (cons (bbdb-record-firstname record2)
                               (bbdb-record-lastname record2)))
                     (unless (bbdb-eval-spec
                              (bbdb-add-job bbdb-add-aka record2 extra-name)
                              (format "Keep \"%s\" as an alternate name? "
                                      extra-name))
                       (setq extra-name nil)))))))

    (bbdb-record-set-name record2 (car name) (cdr name))

    (if extra-name (push extra-name old-aka))
    ;; It is better to delete RECORD1 at the end.
    ;; So we must temporarily allow duplicates in RECORD2.
    (let ((bbdb-allow-duplicates t))
      (bbdb-record-set-field record2 'aka old-aka t)))

  ;; Merge other stuff
  (bbdb-record-set-field record2 'affix
                         (bbdb-record-affix record1) t)
  (bbdb-record-set-field record2 'organization
                         (bbdb-record-organization record1) t)
  (bbdb-record-set-field record2 'phone
                         (bbdb-record-phone record1) t)
  (bbdb-record-set-field record2 'address
                         (bbdb-record-address record1) t)
  (let ((bbdb-allow-duplicates t))
    (bbdb-record-set-field record2 'mail
                           (bbdb-record-mail record1) t))
  (bbdb-record-set-field record2 'xfields
                         (bbdb-record-xfields record1) t)

  ;; `bbdb-delete-records' does nothing if RECORD1 is not known to BBDB.
  (bbdb-delete-records (list record1) 'noprompt)
  (bbdb-change-record record2)
  record2)

;; The following sorting functions are also intended for use
;; in `bbdb-change-hook'.  Then they will be called with one arg, the record.

;;;###autoload
(defun bbdb-sort-addresses (records &optional update)
  "Sort the addresses in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-address
     record (sort (bbdb-record-address record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (if update
        (bbdb-change-record record))))

;;;###autoload
(defun bbdb-sort-phones (records &optional update)
  "Sort the phones in RECORDS according to the label.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-phone
     record (sort (bbdb-record-phone record)
                  (lambda (xx yy) (string< (aref xx 0) (aref yy 0)))))
    (if update
        (bbdb-change-record record))))

;;;###autoload
(defun bbdb-sort-xfields (records &optional update)
  "Sort the xfields in RECORDS according to `bbdb-xfields-sort-order'.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook')."
  (interactive (list (bbdb-do-records) t))
  (bbdb-editable)
  (dolist (record (bbdb-record-list records))
    (bbdb-record-set-xfields
     record (sort (bbdb-record-xfields record)
                  (lambda (a b)
                    (< (or (cdr (assq (car a) bbdb-xfields-sort-order)) 100)
                       (or (cdr (assq (car b) bbdb-xfields-sort-order)) 100)))))
    (if update
        (bbdb-change-record record))))
(define-obsolete-function-alias 'bbdb-sort-notes 'bbdb-sort-xfields "3.0")

;;; Send-Mail interface

;;;###autoload
(defun bbdb-dwim-mail (record &optional mail)
  ;; Do What I Mean!
  "Return a string to use as the mail address of RECORD.
The name in the mail address is formatted obeying `bbdb-mail-name-format'
and `bbdb-mail-name'.  However, if both the first name and last name
are constituents of the address as in John.Doe@Some.Host,
and `bbdb-mail-avoid-redundancy' is non-nil, then the address is used as is
and `bbdb-mail-name-format' and `bbdb-mail-name' are ignored.
If `bbdb-mail-avoid-redundancy' is 'mail-only the name is never included.
MAIL may be a mail address to be used for RECORD.
If MAIL is an integer, use the MAILth mail address of RECORD.
If MAIL is nil use the first mail address of RECORD."
  (unless mail
    (let ((mails (bbdb-record-mail record)))
      (setq mail (or (and (integerp mail) (nth mail mails))
                     (car mails)))))
  (unless mail (error "Record has no mail addresses"))
  (let (name fn ln)
    (cond ((let ((address (bbdb-decompose-bbdb-address mail)))
             ;; We need to know whether we should quote the name part of MAIL
             ;; because of special characters.
             (if (car address)
                 (setq mail (cadr address)
                       name (car address)
                       ln name))))
          ((functionp bbdb-mail-name)
           (setq name (funcall bbdb-mail-name record))
           (if (consp name)
               (setq fn (car name) ln (cdr name)
                     name (if (eq bbdb-mail-name-format 'first-last)
                              (bbdb-concat 'name-first-last fn ln)
                            (bbdb-concat 'name-last-first ln fn)))
             (let ((pair (bbdb-divide-name name)))
               (setq fn (car pair) ln (cdr pair)))))
          ((setq name (bbdb-record-xfield record bbdb-mail-name))
           (let ((pair (bbdb-divide-name name)))
             (setq fn (car pair) ln (cdr pair))))
          (t
           (setq name (if (eq bbdb-mail-name-format 'first-last)
                          (bbdb-record-name record)
                        (bbdb-record-name-lf record))
                 fn (bbdb-record-firstname record)
                 ln (bbdb-record-lastname  record))))
    (if (or (not name) (equal "" name)
            (eq 'mail-only bbdb-mail-avoid-redundancy)
            (and bbdb-mail-avoid-redundancy
                 (cond ((and fn ln)
                        (let ((fnq (regexp-quote fn))
                              (lnq (regexp-quote ln)))
                          (or (string-match (concat "\\`[^!@%]*\\b" fnq
                                                    "\\b[^!%@]+\\b" lnq "\\b")
                                            mail)
                            (string-match (concat "\\`[^!@%]*\\b" lnq
                                                  "\\b[^!%@]+\\b" fnq "\\b")
                                          mail))))
                       ((or fn ln)
                        (string-match (concat "\\`[^!@%]*\\b"
                                              (regexp-quote (or fn ln)) "\\b")
                                      mail)))))
        mail
      ;; If the name contains backslashes or double-quotes, backslash them.
      (setq name (replace-regexp-in-string "[\\\"]" "\\\\\\&" name))
      ;; If the name contains control chars or RFC822 specials, it needs
      ;; to be enclosed in quotes.  This quotes a few extra characters as
      ;; well (!,%, and $) just for common sense.
      ;; `define-mail-alias' uses regexp "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]".
      (format (if (string-match "[][[:cntrl:]\177()<>@,;:.!$%[:nonascii:]]" name)
                  "\"%s\" <%s>"
                "%s <%s>")
              name mail))))

(defun bbdb-compose-mail (&rest args)
  "Start composing a mail message to send.
Use `bbdb-mail-user-agent' or (if nil) use `mail-user-agent'.
ARGS are passed to `compose-mail'."
  (let ((mail-user-agent (or bbdb-mail-user-agent mail-user-agent)))
    (apply 'compose-mail args)))

;;;###autoload
(defun bbdb-mail (records &optional subject n verbose)
  "Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records) nil
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (let ((to (bbdb-mail-address records n nil verbose)))
      (unless (string= "" to)
        (bbdb-compose-mail to subject)))))

(defun bbdb-mail-address (records &optional n kill-ring-save verbose)
  "Return mail addresses of RECORDS as a string.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If KILL-RING-SAVE is non-nil (as in interactive calls), copy mail addresses
to kill ring.  If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (bbdb-do-records)
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (progn (if verbose (message "No records")) "")
    (let ((good "") bad)
      (dolist (record records)
        (let ((mails (bbdb-record-mail record)))
          (cond ((not mails)
                 (push record bad))
                ((eq n t)
                 (setq good (bbdb-concat ",\n\t"
                                         good
                                         (mapcar (lambda (mail)
                                                   (bbdb-dwim-mail record mail))
                                                 mails))))
                (t
                 (setq good (bbdb-concat ",\n\t" good
                            (bbdb-dwim-mail record (or (and (numberp n)
                                                            (nth (1- n) mails))
                                                       (car mails)))))))))
      (when (and bad verbose)
        (message "No mail addresses for %s."
                 (mapconcat 'bbdb-record-name (nreverse bad) ", "))
        (unless (string= "" good) (sit-for 2)))
      (when (and kill-ring-save (not (string= good "")))
        (kill-new good)
        (if verbose (message "%s" good)))
      good)))

;; Is there better way to yank selected mail addresses from the BBDB
;; buffer into a message buffer?  We need some kind of a link between
;; the BBDB buffer and the message buffer, where the mail addresses
;; are supposed to go. Then we could browse the BBDB buffer and copy
;; selected mail addresses from the BBDB buffer into a message buffer.

(defun bbdb-mail-yank ()
  "CC the people displayed in the *BBDB* buffer on this mail message.
The primary mail of each of the records currently listed in the
*BBDB* buffer will be appended to the CC: field of the current buffer."
  (interactive)
  (let ((addresses (with-current-buffer bbdb-buffer-name
                     (delq nil
                           (mapcar (lambda (x)
                                     (if (bbdb-record-mail (car x))
                                         (bbdb-dwim-mail (car x))))
                                   bbdb-records))))
        (case-fold-search t))
    (goto-char (point-min))
    (if (re-search-forward "^CC:[ \t]*" nil t)
        ;; We have a CC field. Move to the end of it, inserting a comma
        ;; if there are already addresses present.
        (unless (eolp)
          (end-of-line)
          (while (looking-at "\n[ \t]")
            (forward-char) (end-of-line))
          (insert ",\n")
          (indent-relative))
      ;; Otherwise, if there is an empty To: field, move to the end of it.
      (unless (and (re-search-forward "^To:[ \t]*" nil t)
                   (eolp))
        ;; Otherwise, insert an empty CC: field.
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert "\nCC:")
        (indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addresses
      (insert (car addresses))
      (when (cdr addresses) (insert ",\n") (indent-relative))
      (setq addresses (cdr addresses)))))
(define-obsolete-function-alias 'bbdb-yank-addresses 'bbdb-mail-yank "3.0")

;;; completion

;;;###autoload
(defun bbdb-completion-predicate (key records)
  "For use as the third argument to `completing-read'.
Obey `bbdb-completion-list'."
  (cond ((null bbdb-completion-list)
         nil)
        ((eq t bbdb-completion-list)
         t)
        (t
         (catch 'bbdb-hash-ok
           (dolist (record records)
             (bbdb-hash-p key record bbdb-completion-list))
           nil))))

(defun bbdb-completing-read-records (prompt &optional omit-records)
  "Read and return list of records from the bbdb.
Completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned.  Otherwise, a valid response is forced."
  (let* ((completion-ignore-case t)
         (string (completing-read prompt bbdb-hashtable
                                  'bbdb-completion-predicate t)))
    (unless (string= "" string)
      (let (records)
        (dolist (record (gethash string bbdb-hashtable))
          (if (not (memq record omit-records))
              (push record records)))
        (delete-dups records)))))

(defun bbdb-completing-read-record (prompt &optional omit-records)
  "Prompt for and return a single record from the bbdb;
completion is done according to `bbdb-completion-list'.  If the user
just hits return, nil is returned. Otherwise, a valid response is forced.
If OMIT-RECORDS is non-nil it should be a list of records to dis-allow
completion with."
  (let ((records (bbdb-completing-read-records prompt omit-records)))
    (cond ((eq (length records) 1)
           (car records))
          ((> (length records) 1)
           (bbdb-display-records records 'one-line)
           (let* ((count (length records))
                  (result (completing-read
                           (format "Which record (1-%s): " count)
                           (mapcar 'number-to-string (number-sequence 1 count))
                           nil t)))
             (nth (1- (string-to-number result)) records))))))

;;;###autoload
(defun bbdb-completing-read-mails (prompt &optional init)
  "Like `read-string', but allows `bbdb-complete-mail' style completion."
  (read-from-minibuffer prompt init
                        bbdb-completing-read-mails-map))

(defconst bbdb-quoted-string-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax-table to parse matched quotes.  Used by `bbdb-complete-mail'.")

;;;###autoload
(defun bbdb-complete-mail (&optional beg cycle-completion-buffer)
  "In a mail buffer, complete the user name or mail before point.
Completion happens up to the preceeding colon, comma, or BEG.
Return non-nil if there is a valid completion, else return nil.

Completion behaviour obeys `bbdb-completion-list' (see there).
If what has been typed matches a unique BBDB record, insert an address
formatted by `bbdb-dwim-mail' (see there).  Also, display this record
if `bbdb-completion-display-record' is non-nil,
If what has been typed is a valid completion but does not match
a unique record, display a list of completions.
If the completion is done and `bbdb-complete-mail-allow-cycling' is t
then cycle through the mails for the matching record.  If BBDB
would format a given address different from what we have in the mail buffer,
the first round of cycling reformats the address accordingly, then we cycle
through the mails for the matching record.
With prefix CYCLE-COMPLETION-BUFFER non-nil, display a list of all mails
available for cycling.

Set the variable `bbdb-complete-mail' non-nil for enabling this feature
as part of the MUA insinuation."
  (interactive (list nil current-prefix-arg))

  (bbdb-buffer) ; Make sure the database is initialized.

  ;; Completion should begin after the preceding comma (separating
  ;; two addresses) or colon (separating the header field name
  ;; from the header field body).  We want to ignore these characters
  ;; if they appear inside a quoted string (RFC 5322, Sec. 3.2.4).
  ;; Note also that a quoted string may span multiple lines
  ;; (RFC 5322, Sec. 2.2.3).
  ;; So to be save, we go back to the beginning of the header field body
  ;; (past the colon, when we are certainly not inside a quoted string),
  ;; then we parse forward, looking for commas not inside a quoted string
  ;; and positioned before END.  - This fails with an unbalanced quote.
  ;; But an unbalanced quote is bound to fail anyway.
  (when (and (not beg)
             (<= (point)
                 (save-restriction ; `mail-header-end'
                   (widen)
                   (save-excursion
                     (rfc822-goto-eoh)
                     (point)))))
    (let ((end (point))
          start pnt state)
      (save-excursion
        ;; A header field name must appear at the beginning of a line,
        ;; and it must be terminated by a colon.
        (re-search-backward "^[^ \t\n:][^:]*:[ \t\n]+")
        (setq beg (match-end 0)
              start beg)
        (goto-char beg)
        ;; If we are inside a syntactically correct header field,
        ;; all continuation lines in between the field name and point
        ;; must begin with a white space character.
        (if (re-search-forward "\n[^ \t]" end t)
            ;; An invalid header is identified via BEG set to nil.
            (setq beg nil)
          ;; Parse field body up to END
          (with-syntax-table bbdb-quoted-string-syntax-table
            (while (setq pnt (re-search-forward ",[ \t\n]*" end t))
              (setq state (parse-partial-sexp start pnt nil nil state)
                    start pnt)
              (unless (nth 3 state) (setq beg pnt))))))))

  ;; Do we have a meaningful way to set BEG if we are not in a message header?
  (unless beg
    (message "Not a valid buffer position for mail completion")
    (sit-for 1))

  (let* ((end (point))
         (done (unless beg 'nothing))
         (orig (and beg (buffer-substring beg end)))
         (completion-ignore-case t)
         (completion (and orig
                          (try-completion orig bbdb-hashtable
                                          'bbdb-completion-predicate)))
         all-completions dwim-completions one-record)

    (unless done
      ;; We get fooled if a partial COMPLETION matches "," (for example,
      ;; a comma in lf-name).  Such a partial COMPLETION cannot be protected
      ;; by quoting.  Then the comma gets interpreted as BEG.
      ;; So we never perform partial completion beyond the first comma.
      ;; This works even if we have just one record matching ORIG (thus
      ;; allowing dwim-completion) because ORIG is a substring of COMPLETION
      ;; even after COMPLETION got truncated; and ORIG by itself must be
      ;; sufficient to identify this record.
      ;; Yet if multiple records match ORIG we can only offer a *Completions*
      ;; buffer.
      (if (and (stringp completion)
               (string-match "," completion))
          (setq completion (substring completion 0 (match-beginning 0))))

      (setq all-completions (all-completions orig bbdb-hashtable
                                             'bbdb-completion-predicate))
      ;; Resolve the records matching ORIG:
      ;; Multiple completions may match the same record
      (let ((records (delete-dups
                      (apply 'append (mapcar (lambda (compl)
                                               (gethash compl bbdb-hashtable))
                                             all-completions)))))
        ;; Is there only one matching record?
        (setq one-record (and (not (cdr records))
                              (car records))))

      ;; Clean up *Completions* buffer window, if it exists
      (let ((window (get-buffer-window "*Completions*")))
        (if (window-live-p window)
            (quit-window nil window)))

      (cond
       ;; Match for a single record
       (one-record
        (let ((completion-list (if (eq t bbdb-completion-list)
                                   '(fl-name lf-name mail aka organization)
                                 bbdb-completion-list))
              (mails (bbdb-record-mail one-record))
              mail elt)
          (if (not mails)
              (progn
                (message "Matching record has no mail field")
                (sit-for 1)
                (setq done 'nothing))

            ;; Determine the mail address of ONE-RECORD to use for ADDRESS.
            ;; Do we have a preferential order for the following tests?
            ;; (1) If ORIG matches name, AKA, or organization of ONE-RECORD,
            ;;     then ADDRESS will be the first mail address of ONE-RECORD.
            (if (try-completion orig
                                (append
                                 (if (memq 'fl-name completion-list)
                                     (list (or (bbdb-record-name one-record) "")))
                                 (if (memq 'lf-name completion-list)
                                     (list (or (bbdb-record-name-lf one-record) "")))
                                 (if (memq 'aka completion-list)
                                     (bbdb-record-field one-record 'aka-all))
                                 (if (memq 'organization completion-list)
                                     (bbdb-record-organization one-record))))
                (setq mail (car mails)))
            ;; (2) If ORIG matches one or multiple mail addresses of ONE-RECORD,
            ;;     then we take the first one matching ORIG.
            ;;     We got here with MAIL nil only if `bbdb-completion-list'
            ;;     includes 'mail or 'primary.
            (unless mail
              (while (setq elt (pop mails))
                (if (try-completion orig (list elt))
                    (setq mail elt
                          mails nil))))
            ;; This error message indicates a bug!
            (unless mail (error "No match for %s" orig))

            (let ((dwim-mail (bbdb-dwim-mail one-record mail)))
              (if (string= dwim-mail orig)
                  ;; We get here if `bbdb-mail-avoid-redundancy' is 'mail-only
                  ;; and `bbdb-completion-list' includes 'mail.
                  (unless (and bbdb-complete-mail-allow-cycling
                               (< 1 (length (bbdb-record-mail one-record))))
                    (setq done 'unchanged))
                ;; Replace the text with the expansion
                (delete-region beg end)
                (insert dwim-mail)
                (bbdb-complete-mail-cleanup dwim-mail beg)
                (setq done 'unique))))))

       ;; Partial completion
       ((and (stringp completion)
             (not (bbdb-string= orig completion)))
        (delete-region beg end)
        (insert completion)
        (setq done 'partial))

       ;; Partial match not allowing further partial completion
       (completion
        (let ((completion-list (if (eq t bbdb-completion-list)
                                   '(fl-name lf-name mail aka organization)
                                 bbdb-completion-list)))
          ;; Now collect all the dwim-addresses for each completion.
          ;; Add it if the mail is part of the completions
          (dolist (key all-completions)
            (dolist (record (gethash key bbdb-hashtable))
              (let ((mails (bbdb-record-mail record))
                    accept)
                (when mails
                  (dolist (field completion-list)
                    (cond ((eq field 'fl-name)
                           (if (bbdb-string= key (bbdb-record-name record))
                               (push (car mails) accept)))
                          ((eq field 'lf-name)
                           (if (bbdb-string= key (bbdb-cache-lf-name
                                                  (bbdb-record-cache record)))
                               (push (car mails) accept)))
                          ((eq field 'aka)
                           (if (member-ignore-case key (bbdb-record-field
                                                        record 'aka-all))
                               (push (car mails) accept)))
                          ((eq field 'organization)
                           (if (member-ignore-case key (bbdb-record-organization
                                                        record))
                               (push (car mails) accept)))
                          ((eq field 'primary)
                           (if (bbdb-string= key (car mails))
                               (push (car mails) accept)))
                          ((eq field 'mail)
                           (dolist (mail mails)
                             (if (bbdb-string= key mail)
                                 (push mail accept))))))
                  (dolist (mail (delete-dups accept))
                    (push (bbdb-dwim-mail record mail) dwim-completions))))))

          (setq dwim-completions (sort (delete-dups dwim-completions)
                                       'string-lessp))
          (cond ((not dwim-completions)
                 (message "Matching record has no mail field")
                 (sit-for 1)
                 (setq done 'nothing))
                ;; DWIM-COMPLETIONS may contain only one element,
                ;; if multiple completions match the same record.
                ;; Then we may proceed with DONE set to `unique'.
                ((eq 1 (length dwim-completions))
                 (delete-region beg end)
                 (insert (car dwim-completions))
                 (bbdb-complete-mail-cleanup (car dwim-completions) beg)
                 (setq done 'unique))
                (t (setq done 'choose)))))))

    ;; By now, we have considered all possiblities to perform a completion.
    ;; If nonetheless we haven't done anything so far, consider cycling.
    ;;
    ;; Completion and cycling are really two very separate things.
    ;; Completion is controlled by the user variable `bbdb-completion-list'.
    ;; Cycling assumes that ORIG already holds a valid RFC 822 mail address.
    ;; Therefore cycling may consider different records than completion.
    (when (and (not done) bbdb-complete-mail-allow-cycling)
      ;; find the record we are working on.
      (let* ((address (bbdb-extract-address-components orig))
             (record (car (bbdb-message-search
                           (car address) (cadr address)))))
        (if (and record
                 (setq dwim-completions
                       (mapcar (lambda (m) (bbdb-dwim-mail record m))
                               (bbdb-record-mail record))))
            (cond ((and (= 1 (length dwim-completions))
                        (string= orig (car dwim-completions)))
                   (setq done 'unchanged))
                  (cycle-completion-buffer ; use completion buffer
                   (setq done 'cycle-choose))
                  ;; Reformatting / Clean up:
                  ;; If the canonical mail address (nth 1 address)
                  ;; matches the Nth canonical mail address of RECORD,
                  ;; but ORIG is not `equal' to (bbdb-dwim-mail record n),
                  ;; then we replace ORIG by (bbdb-dwim-mail record n).
                  ;; For example, the address "JOHN SMITH <FOO@BAR.COM>"
                  ;; gets reformatted as "John Smith <foo@bar.com>".
                  ;; We attempt this reformatting before the yet more
                  ;; aggressive proper cycling.
                  ((let* ((cmails (bbdb-record-mail-canon record))
                          (len (length cmails))
                          mail dwim-mail)
                     (while (and (not done)
                                 (setq mail (pop cmails)))
                       (when (and (bbdb-string= mail (nth 1 address)) ; ignore case
                                  (not (string= orig (setq dwim-mail
                                                           (nth (- len 1 (length cmails))
                                                                dwim-completions)))))
                         (delete-region beg end)
                         (insert dwim-mail)
                         (bbdb-complete-mail-cleanup dwim-mail beg)
                         (setq done 'reformat)))
                     done))

                  (t
                   ;; ORIG is `equal' to an element of DWIM-COMPLETIONS
                   ;; Use the next element of DWIM-COMPLETIONS.
                   (let ((dwim-mail (or (nth 1 (member orig dwim-completions))
                                        (nth 0 dwim-completions))))
                     ;; replace with new mail address
                     (delete-region beg end)
                     (insert dwim-mail)
                     (bbdb-complete-mail-cleanup dwim-mail beg)
                     (setq done 'cycle)))))))

    (when (member done '(choose cycle-choose))
      ;; Pop up a completions window using DWIM-COMPLETIONS.
      ;; `completion-in-region' does not work here as DWIM-COMPLETIONS
      ;; is not a collection for completion in the usual sense, but it
      ;; is really a list of replacements.
      (let ((status (not (eq (selected-window) (minibuffer-window))))
            (completion-base-position (list beg end))
            ;; We first call the default value of
            ;; `completion-list-insert-choice-function'
            ;; before performing our own stuff.
            (completion-list-insert-choice-function
             `(lambda (beg end text)
                ,(if (boundp 'completion-list-insert-choice-function)
                     `(funcall ',completion-list-insert-choice-function
                               beg end text))
                (bbdb-complete-mail-cleanup text beg))))
        (if status (message "Making completion list..."))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list dwim-completions))
        (if status (message "Making completion list...done"))))

    ;; If DONE is `nothing' return nil so that possibly some other code
    ;; can take over.
    (unless (eq done 'nothing)
      done)))

;;;###autoload
(define-obsolete-function-alias 'bbdb-complete-name 'bbdb-complete-mail "3.0")

(defun bbdb-complete-mail-cleanup (mail beg)
  "Clean up after inserting MAIL at position BEG.
If we are past `fill-column', wrap at the previous comma."
  (if (and (not (auto-fill-function))
           (>= (current-column) fill-column))
      (save-excursion
        (goto-char beg)
        (when (search-backward "," (line-beginning-position) t)
          (forward-char 1)
          (insert "\n")
          (indent-relative)
          (if (looking-at "[ \t\n]+")
              (delete-region (point) (match-end 0))))))
  (if (or bbdb-completion-display-record bbdb-complete-mail-hook)
      (let* ((address (bbdb-extract-address-components mail))
             (records (bbdb-message-search (car address) (nth 1 address))))
        ;; Update the *BBDB* buffer if desired.
        (if bbdb-completion-display-record
            (let ((bbdb-silent-internal t))
              ;; FIXME: This pops up *BBDB* before removing *Completions*
              (bbdb-display-records records nil t)))
        ;; `bbdb-complete-mail-hook' may access MAIL, ADDRESS, and RECORDS.
        (run-hooks 'bbdb-complete-mail-hook))))

;;; interface to mail-abbrevs.el.

;;;###autoload
(defun bbdb-mail-aliases (&optional force-rebuilt noisy)
  "Define mail aliases for the records in the database.
Define a mail alias for every record that has a `mail-alias' field
which is the contents of that field.
If there are multiple comma-separated words in the `mail-alias' field,
then all of those words will be defined as aliases for that person.

If multiple records in the database have the same mail alias,
then that alias expands to a comma-separated list of the mail addresses
of all of these people.
Add this command to `mail-setup-hook'.

Mail aliases are (re)built only if `bbdb-mail-aliases-need-rebuilt' is non-nil
because the database was newly loaded or it has been edited.
Rebuilding the aliases is enforced if prefix FORCE-REBUILT is t."
  (interactive (list current-prefix-arg t))
  ;; Build `mail-aliases' if not yet done.
  ;; Note: `mail-abbrevs-setup' rebuilds the mail-aliases only if
  ;; `mail-personal-alias-file' has changed.  So it would not do anything
  ;; if we want to rebuild the mail-aliases because of changes in BBDB.
  (if (or force-rebuilt (eq t mail-aliases)) (build-mail-aliases))

  ;; We should be cleverer here and instead of rebuilding all aliases
  ;; we should just do what's necessary, i.e. remove deleted records
  ;; and add new records
  ;; Calling `bbdb-records' can change `bbdb-mail-aliases-need-rebuilt'
  (let ((records (bbdb-search (bbdb-records) :xfield (cons bbdb-mail-alias-field ".")))
        results match)
    (if (not (or force-rebuilt bbdb-mail-aliases-need-rebuilt))
        (if noisy (message "BBDB mail alias: nothing to do"))
      (setq bbdb-mail-aliases-need-rebuilt nil)

      ;; collect an alist of (alias rec1 [rec2 ...])
      (dolist (record records)
        (if (bbdb-record-mail record)
            (dolist (alias (bbdb-record-xfield-split record bbdb-mail-alias-field))
              (if (setq match (assoc alias results))
                  ;; If an alias appears more than once, we collect all records
                  ;; that refer to it.
                  (nconc match (list record))
                (push (list alias record) results)))
          (unless bbdb-silent
            (bbdb-warn "record %S has no mail address, but the aliases: %s"
                       (bbdb-record-name record)
                       (bbdb-record-xfield record bbdb-mail-alias-field))
            (sit-for 1))))

      ;; Iterate over the results and create the aliases
      (dolist (result results)
        (let* ((aliasstem (car result))
               (expansions
                (if (cddr result)
                    ;; for group aliases we just take all the primary mails
                    ;; and define only one expansion!
                    (list (mapconcat (lambda (record) (bbdb-dwim-mail record))
                                     (cdr result) mail-alias-separator-string))
                  ;; this is an alias for a single person so deal with it
                  ;; according to `bbdb-mail-alias'
                  (let* ((record (nth 1 result))
                         (mails (bbdb-record-mail record)))
                    (if (or (eq 'first bbdb-mail-alias)
                            (not (cdr mails)))
                        ;; Either we want to define only one alias for
                        ;; the first mail address or there is anyway
                        ;; only one address.  In either case, we take
                        ;; take only the first address.
                        (list (bbdb-dwim-mail record (car mails)))
                      ;; We need to deal with more than one mail address...
                      (let* ((all (mapcar (lambda (m) (bbdb-dwim-mail record m))
                                          mails))
                             (star (bbdb-concat mail-alias-separator-string all)))
                        (if (eq 'star bbdb-mail-alias)
                            (list star (car all))
                          ;; if `bbdb-mail-alias' is 'all, we create
                          ;; two aliases for the primary mail address
                          (cons star (cons (car all) all))))))))
               (count -1) ; n=-1: <alias>*;  n=0: <alias>;  n>0: <alias>n
               (len (length expansions))
               alias f-alias)

          ;; create the aliases for each expansion
          (dolist (expansion expansions)
            (cond ((or (= 1 len)
                       (= count 0))
                   (setq alias aliasstem))
                  ((= count -1) ;; all the mails of a record
                   (setq alias (concat aliasstem "*")))
                  (t ;; <alias>n for each mail of a record
                   (setq alias (format "%s%s" aliasstem count))))
            (setq count (1+ count))

            (bbdb-pushnew (cons alias expansion) mail-aliases)

            (define-mail-abbrev alias expansion)
            (unless (setq f-alias (intern-soft (downcase alias) mail-abbrevs))
              (error "Cannot find the alias"))

            ;; `define-mail-abbrev' initializes f-alias to be
            ;; `mail-abbrev-expand-hook'. We replace this by
            ;; `bbdb-mail-abbrev-expand-hook'
            (unless (eq (symbol-function f-alias) 'mail-abbrev-expand-hook)
              (error "mail-aliases contains unexpected hook %s"
                     (symbol-function f-alias)))
            ;; `bbdb-mail-abbrev-hook' is called with mail addresses instead of
            ;; bbdb records to avoid keeping pointers to records, which would
            ;; lose if the database was reverted.
            ;; `bbdb-mail-abbrev-hook' uses `bbdb-message-search' to convert
            ;; these mail addresses to records, which is plenty fast.
            ;; FIXME: The value of arg MAILS for `bbdb-mail-abbrev-hook'
            ;; is wrong. Currently it is based on the list of records that have
            ;; referenced ALIASTEM and we simply take the first mail address
            ;; from each of these records.
            ;; Then `bbdb-message-search' will find the correct records
            ;; (assuming that each mail address appears only once in the
            ;; database).  Nonethless, arg MAILS for `bbdb-mail-abbrev-hook'
            ;; does not, in general, contain the actual mail addresses
            ;; of EXPANSION.  So what we would need is to go back from
            ;; EXPANSION to the mail addresses it contains (which is tricky
            ;; because mail addresses in the database can be shortcuts for
            ;; the addresses in EXPANSION).
            (fset f-alias `(lambda ()
                             (bbdb-mail-abbrev-expand-hook
                              ,alias
                              ',(mapcar (lambda (r) (car (bbdb-record-mail r)))
                                        (cdr result))))))))

      (if noisy (message "BBDB mail alias: rebuilding done")))))

(defun bbdb-mail-abbrev-expand-hook (alias mails)
  (run-hook-with-args 'bbdb-mail-abbrev-expand-hook alias mails)
  (mail-abbrev-expand-hook)
  (when bbdb-completion-display-record
    (let ((bbdb-silent-internal t))
      (bbdb-display-records
       (apply 'append
              (mapcar (lambda (mail) (bbdb-message-search nil mail)) mails))
       nil t))))

(defun bbdb-get-mail-aliases ()
  "Return a list of mail aliases used in the BBDB."
  (let ((records (bbdb-search (bbdb-records) :xfield (cons bbdb-mail-alias-field ".")))
        result)
    (dolist (record records)
      (dolist (alias (bbdb-record-xfield-split record bbdb-mail-alias-field))
        (bbdb-pushnew alias result)))
    result))

;;;###autoload
(defsubst bbdb-mail-alias-list (alias)
  (if (stringp alias)
      (bbdb-split bbdb-mail-alias-field alias)
    alias))

(defun bbdb-add-mail-alias (records &optional alias delete)
  "Add ALIAS to RECORDS.
If prefix DELETE is non-nil, remove ALIAS from RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Arg ALIAS is ignored if list RECORDS contains more than one record.
Instead read ALIAS interactively for each record in RECORDS.
If the function `bbdb-init-mail-alias' is defined, it is called with
one arg RECORD to define the default value for ALIAS of RECORD."
  (interactive (list (bbdb-do-records) nil current-prefix-arg))
  (bbdb-editable)
  (setq records (bbdb-record-list records))
  (if (< 1 (length records)) (setq alias nil))
  (let* ((tmp (intern-soft
               (concat "bbdb-init-" (symbol-name bbdb-mail-alias-field))))
         (init-f (if (functionp tmp) tmp)))
    (dolist (record records)
      (let ((r-a-list (bbdb-record-xfield-split record bbdb-mail-alias-field))
            (alias alias)
            a-list)
        (if alias
            (setq a-list (bbdb-mail-alias-list alias))
          (when init-f
            (setq a-list (bbdb-mail-alias-list (funcall init-f record))
                  alias (if a-list (bbdb-concat bbdb-mail-alias-field a-list))))
          (let ((crm-separator
                 (concat "[ \t\n]*"
                         (cadr (assq bbdb-mail-alias-field bbdb-separator-alist))
                         "[ \t\n]*"))
                (crm-local-completion-map bbdb-crm-local-completion-map)
                (prompt (format "%s mail alias:%s " (if delete "Remove" "Add")
                                (if alias (format " (default %s)" alias) "")))
                (collection (if delete
                                (or r-a-list (error "Record has no alias"))
                              (bbdb-get-mail-aliases))))
            (setq a-list (if (string< "24.3" (substring emacs-version 0 4))
                             (completing-read-multiple prompt collection nil
                                                       delete nil nil alias)
                          (bbdb-split bbdb-mail-alias-field
                                      (completing-read prompt collection nil
                                                       delete nil nil alias))))))
        (dolist (a a-list)
          (if delete
              (setq r-a-list (delete a r-a-list))
            ;; Add alias only if it is not there yet
            (bbdb-pushnew a r-a-list)))
        ;; This also handles `bbdb-mail-aliases-need-rebuilt'
        (bbdb-record-set-xfield record bbdb-mail-alias-field
                                (bbdb-concat bbdb-mail-alias-field r-a-list))
        (bbdb-change-record record)))))

;;; Dialing numbers from BBDB

(defun bbdb-dial-number (phone-string)
  "Dial the number specified by PHONE-STRING.
This uses the tel URI syntax passed to `browse-url' to make the call.
If `bbdb-dial-function' is non-nil then that is called to make the phone call."
  (interactive "sDial number: ")
  (if bbdb-dial-function
      (funcall bbdb-dial-function phone-string)
    (browse-url (concat "tel:" phone-string))))

;;;###autoload
(defun bbdb-dial (phone force-area-code)
  "Dial the number at point.
If the point is at the beginning of a record, dial the first phone number.
Use rules from `bbdb-dial-local-prefix-alist' unless prefix FORCE-AREA-CODE
is non-nil.  Do not dial the extension."
  (interactive (list (bbdb-current-field) current-prefix-arg))
  (if (eq (car-safe phone) 'name)
      (setq phone (car (bbdb-record-phone (bbdb-current-record)))))
  (if (eq (car-safe phone) 'phone)
      (setq phone (car (cdr phone))))
  (or (vectorp phone) (error "Not on a phone field"))

  (let ((number (bbdb-phone-string phone))
        shortnumber)

    ;; cut off the extension
    (if (string-match "x[0-9]+$" number)
        (setq number (substring number 0 (match-beginning 0))))

    (unless force-area-code
      (let ((alist bbdb-dial-local-prefix-alist) prefix)
        (while (setq prefix (pop alist))
          (if (string-match (concat "^" (eval (car prefix))) number)
              (setq shortnumber (concat (cdr prefix)
                                        (substring number (match-end 0)))
                    alist nil)))))

    (if shortnumber
        (setq number shortnumber)

      ;; This is terrifically Americanized...
      ;; Leading 0 => local number (?)
      (if (and bbdb-dial-local-prefix
               (string-match "^0" number))
          (setq number (concat bbdb-dial-local-prefix number)))

      ;; Leading + => long distance/international number
      (if (and bbdb-dial-long-distance-prefix
               (string-match "^\+" number))
          (setq number (concat bbdb-dial-long-distance-prefix " "
                               (substring number 1)))))

    (unless bbdb-silent
      (message "Dialing %s" number))
    (bbdb-dial-number number)))

;;; url interface

;;;###autoload
(defun bbdb-browse-url (records &optional which)
  "Brwose URLs stored in the `url' field of RECORDS.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL."
  (interactive (list (bbdb-get-records "Visit (URL): ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (unless which (setq which 0))
  (dolist (record (bbdb-record-list records))
    (let ((url (bbdb-record-xfield-split record 'url)))
      (when url
        (setq url (read-string "fetch: " (nth which url)))
        (unless (string= "" url)
          (browse-url url))))))

;;;###autoload
(defun bbdb-grab-url (record url)
  "Grab URL and store it in RECORD."
  (interactive (let ((url (browse-url-url-at-point)))
                 (unless url (error "No URL at point"))
                 (list (bbdb-completing-read-record
                        (format "Add `%s' for: " url))
                       url)))
  (bbdb-record-set-field record 'url url t)
  (bbdb-change-record record)
  (bbdb-display-records (list record)))

;;; Copy to kill ring

;;;###autoload
(defun bbdb-copy-records-as-kill (records)
  "Copy RECORDS to kill ring.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive (list (bbdb-do-records t)))
  (let (drec)
    (dolist (record (bbdb-record-list records t))
      (push (buffer-substring (nth 2 record)
                              (or (nth 2 (car (cdr (memq record bbdb-records))))
                                  (point-max)))
            drec))
    (kill-new (replace-regexp-in-string
               "[ \t\n]*\\'" "\n"
               (mapconcat 'identity (nreverse drec) "")))))

;;;###autoload
(defun bbdb-copy-fields-as-kill (records field &optional num)
  "For RECORDS copy values of FIELD at point to kill ring.
If FIELD is an address or phone with a label, copy only field values
with the same label.  With numeric prefix NUM, if the value of FIELD
is a list, copy only the NUMth list element.
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'."
  (interactive
   (list (bbdb-do-records t) (bbdb-current-field)
         (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))
  (unless field (error "Not a field"))
  (let* ((type (if (eq (car field) 'xfields)
                   (car (nth 1 field))
                 (car field)))
         (label (if (memq type '(phone address))
                    (aref (cadr field) 0)))
	 (ident (and (< 1 (length records))
                     (not (eq type 'name))))
	 val-list)
    (dolist (record (bbdb-record-list records))
      (let ((raw-val (bbdb-record-field (car record) type))
            value)
        (if raw-val
            (cond ((eq type 'phone)
                   (dolist (elt raw-val)
                     (if (equal label (aref elt 0))
                         (push (bbdb-phone-string elt) value)))
                   (setq value (bbdb-concat 'phone (nreverse value))))
                  ((eq type 'address)
                   (dolist (elt raw-val)
                     (if (equal label (aref elt 0))
                         (push (bbdb-format-address
                                elt (if (eq (nth 1 record) 'one-line) 3 2))
                               value)))
                   (setq value (bbdb-concat 'address (nreverse value))))
                  ((consp raw-val)
                   (setq value (if num (nth num raw-val)
                                 (bbdb-concat type raw-val))))
                  (t (setq value raw-val))))
        (if value
            (push (if ident
                      (bbdb-concat 'name-field
                                   (bbdb-record-name (car record)) value)
                    value) val-list))))
    (let ((str (bbdb-concat 'record (nreverse val-list))))
      (kill-new str)
      (message "%s" str))))

;;; Help and documentation

;;;###autoload
(defun bbdb-info ()
  (interactive)
  (info (format "(%s)Top" (or bbdb-info-file "bbdb"))))

;;;###autoload
(defun bbdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<bbdb-mode-map>\
new field: \\[bbdb-insert-field]; \
edit field: \\[bbdb-edit-field]; \
delete field: \\[bbdb-delete-field-or-record]; \
mode help: \\[describe-mode]; \
info: \\[bbdb-info]")))

(provide 'bbdb-com)

;;; bbdb-com.el ends here
