;;; bbdb-snarf.el --- convert free-form text to BBDB records -*- lexical-binding: t -*-

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

;; The commands `bbdb-snarf', `bbdb-snarf-yank' and `bbdb-snarf-paragraph'
;; create BBDB records by picking the name, addresses, phones, etc.
;; out of a (buffer) string.  Things are recognized by context (e.g., URLs
;; start with http:// or www.).  See `bbdb-snarf-rule-alist' for details.
;;
;; The rule `eu' should work out of the box for many continental
;; European countries.  It can be further customized by defining
;; a suitable postcode regexp passed to `bbdb-snarf-address-eu'.
;; `mail' is a simple rule that can pick a single mail address from,
;; say, a long list of mail addresses in a message.
;;
;; RW: `bbdb-snarf' is an interesting proof of concept.  Yet I find
;; its snarfing algorithms often too simplistic to be useful in real life.
;; How can this possibly be improved?  Suggestions welcome.

;;; Code:

(require 'bbdb-com)

(defcustom bbdb-snarf-rule-alist
  '((us bbdb-snarf-surrounding-space
        bbdb-snarf-phone-nanp
        bbdb-snarf-url
        bbdb-snarf-mail
        bbdb-snarf-empty-lines
        bbdb-snarf-name
        bbdb-snarf-address-us
        bbdb-snarf-empty-lines
        bbdb-snarf-notes
        bbdb-snarf-name-mail) ; currently useless
    (eu bbdb-snarf-surrounding-space
        bbdb-snarf-phone-eu
        bbdb-snarf-url
        bbdb-snarf-mail
        bbdb-snarf-empty-lines
        bbdb-snarf-name
        bbdb-snarf-address-eu
        bbdb-snarf-empty-lines
        bbdb-snarf-notes
        bbdb-snarf-name-mail) ; currently useless
   (mail bbdb-snarf-mail-address))
  "Alist of rules for snarfing.
Each rule is of the form (KEY FUNCTION FUNCTION ...).
The symbol KEY identifies the rule, see also `bbdb-snarf-rule-default'.

Snarfing is a cumulative process.  The text is copied to a temporary
snarf buffer that becomes current during snarfing.
Each FUNCTION is called with one arg, the RECORD we are snarfing,
and with point at the beginning of the snarf buffer.  FUNCTION should populate
the fields of RECORD.  It may delete the part of the snarf buffer
that it has processed so that the remaining FUNCTIONs operate only
on those parts that were not yet snarfed.  The order of the FUNCTION calls
in a rule is then crucial.
Unlike other parts of BBDB, FUNCTIONs need not update the cache and
hash table for RECORD which is done at the end by `bbdb-snarf'."
  :group 'bbdb-utilities-snarf
  :type '(repeat (cons (symbol :tag "Key")
                       (repeat (function :tag "Snarf function")))))

(defcustom bbdb-snarf-rule-default 'us
  "Default rule for snarfing."
  :group 'bbdb-utilities-snarf
  :type 'symbol)

(defcustom bbdb-snarf-name-regexp
  "^[ \t'\"]*\\([- .,[:word:]]*[[:word:]]\\)"
  "Regexp matching a name.  Case is ignored.
The first subexpression becomes the name."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-mail-regexp
  (concat "\\(?:\\(?:mailto:\\|e?mail:?\\)[ \t]*\\)?"
          "<?\\([^ \t\n<]+@[^ \t\n>]+\\)>?")
  "Regexp matching a mail address.  Case is ignored.
The first subexpression becomes the mail address."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-phone-nanp-regexp
  (concat "\\(?:phone:?[ \t]*\\)?"
          "\\(\\(?:([2-9][0-9][0-9])[-. ]?\\|[2-9][0-9][0-9][-. ]\\)?"
          "[0-9][0-9][0-9][-. ][0-9][0-9][0-9][0-9]"
          "\\(?: *\\(?:x\\|ext\\.?\\) *[0-9]+\\)?\\)")
  "Regexp matching a NANP phone number.  Case is ignored.
NANP is the North American Numbering Plan used in North and Central America.
The first subexpression becomes the phone number."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-phone-eu-regexp
  (concat "\\(?:phone?:?[ \t]*\\)?"
          "\\(\\(?:\\+[1-9]\\|(\\)[-0-9()\s]+\\)")
  "Regexp matching a European phone number.
The first subexpression becomes the phone number."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-postcode-us-regexp
  ;; US postcode appears at end of line
  (concat "\\(\\<[0-9][0-9][0-9][0-9][0-9]"
          "\\(-[0-9][0-9][0-9][0-9]\\)?"
          "\\>\\)$")
  "Regexp matching US postcodes.
The first subexpression becomes the postcode."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-address-us-country nil
  "Country to use for US addresses.  If nil leave country blank."
  :group 'bbdb-utilities-snarf
  :type '(choice (const :tag "Leave blank" nil)
                 (string :tag "Country")))

(defcustom bbdb-snarf-postcode-eu-regexp
  "^\\([0-9][0-9][0-9][0-9][0-9]?\\)" ; four or five digits
  "Regexp matching many European postcodes.
`bbdb-snarf-address-eu' assumes that the address appears at the beginning
of a line followed by the name of the city."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defcustom bbdb-snarf-address-eu-country nil
  "Country to use for EU addresses.  If nil leave country blank."
  :group 'bbdb-utilities-snarf
  :type '(choice (const :tag "Leave blank" nil)
                 (string :tag "Country")))

(defcustom bbdb-snarf-default-label-alist
  '((phone . "work") (address . "work"))
  "Default labels for snarfing.
This is an alist where each element is a cons pair (FIELD . LABEL).
The symbol FIELD denotes a record field like `phone' or `address'.
The string LABEL denotes the default label for FIELD."
  :group 'bbdb-utilities-snarf
  :type '(repeat (cons (symbol :tag "Field")
                       (string :tag "Label"))))

(defcustom bbdb-snarf-url 'url
  "What xfield BBDB should use for URLs, or nil to not snarf URLs."
  :group 'bbdb-utilities-snarf
  :type 'symbol)

(defcustom bbdb-snarf-url-regexp "\\(\\(?:http://\\|www\\.\\)[^ \t\n]+\\)"
  "Regexp matching a URL.  Case is ignored.
The first subexpression becomes the URL."
  :group 'bbdb-utilities-snarf
  :type 'regexp)

(defun bbdb-snarf-surrounding-space (_record)
  "Discard beginning and trailing space when snarfing RECORD."
  (while (re-search-forward "^[ \t]+" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\s-+$" nil t)
    (replace-match "")))

(defun bbdb-snarf-empty-lines (_record)
  "Discard empty lines when snarfing RECORD."
  (while (re-search-forward "^[ \t]*\n" nil t)
    (replace-match "")))

(defun bbdb-snarf-name (record)
  "Snarf name for RECORD."
  (if (and (not (bbdb-record-lastname record))
           (let ((case-fold-search t))
             (re-search-forward bbdb-snarf-name-regexp nil t)))
      (let ((name (match-string 1)))
        (replace-match "")
        (setq name (bbdb-divide-name name))
        (bbdb-record-set-firstname record (car name))
        (bbdb-record-set-lastname record (cdr name)))))

(defun bbdb-snarf-name-mail (record)
  "Snarf name from mail address for RECORD."
  ;; Fixme: This is currently useless because `bbdb-snarf-mail-regexp'
  ;; cannot handle names in RFC 5322-like addresses "John Smith <foo@bar.com>".
  (let ((name (bbdb-record-lastname record)))
    (when (and (not name)
               (bbdb-record-mail record)
               (setq name (car (bbdb-extract-address-components
                                (car (bbdb-record-mail record)))))
               (setq name (bbdb-divide-name name)))
      (bbdb-record-set-firstname record (car name))
      (bbdb-record-set-lastname record (cadr name)))))

(defun bbdb-snarf-mail-address (record)
  "Snarf name and mail address for RECORD."
  ;; The voodoo of `mail-extract-address-components' makes
  ;; the following quite powerful.  If this function is used as part of
  ;; a more complex rule, the buffer should be narrowed appropriately.
  (let* ((data (bbdb-extract-address-components (buffer-string)))
         (name (and (car data) (bbdb-divide-name (car data)))))
    (bbdb-record-set-firstname record (car name))
    (bbdb-record-set-lastname  record (cdr name))
    (bbdb-record-set-mail record (list (cadr data)))
    (delete-region (point-min) (point-max))))

(defun bbdb-snarf-mail (record)
  "Snarf mail addresses for RECORD.
This uses the first subexpresion of `bbdb-snarf-mail-regexp'."
  (let ((case-fold-search t) mails)
    (while (re-search-forward bbdb-snarf-mail-regexp nil t)
      (push (match-string 1) mails)
      (replace-match ""))
    (bbdb-record-set-mail record (nconc (bbdb-record-mail record) mails))))

(defun bbdb-snarf-label (field)
  "Extract the label before point, or return default label for FIELD."
  (save-match-data
    (if (looking-back "\\(?:^\\|[,:]\\)\\([^\n,:]+\\):[ \t]*"
                      (line-beginning-position))
        (prog1 (match-string 1)
          (delete-region (match-beginning 1) (match-end 0)))
      (cdr (assq field bbdb-snarf-default-label-alist)))))

(defun bbdb-snarf-phone-nanp (record)
  "Snarf NANP phone numbers for RECORD.
NANP is the North American Numbering Plan used in North and Central America.
This uses the first subexpresion of `bbdb-snarf-phone-nanp-regexp'."
  (let ((case-fold-search t) phones)
    (while (re-search-forward bbdb-snarf-phone-nanp-regexp nil t)
      (goto-char (match-beginning 0))
      (if (save-match-data
            (looking-back "[0-9A-Z]" nil)) ;; not really an NANP phone number
          (goto-char (match-end 0))
        (push (vconcat (list (bbdb-snarf-label 'phone))
                       (save-match-data
                         (bbdb-parse-phone (match-string 1))))
              phones)
        (replace-match "")))
    (bbdb-record-set-phone record (nconc (bbdb-record-phone record)
                                         (nreverse phones)))))

(defun bbdb-snarf-phone-eu (record &optional phone-regexp)
  "Snarf European phone numbers for RECORD.
PHONE-REGEXP is the regexp to match a phone number.
It defaults to `bbdb-snarf-phone-eu-regexp'."
  (let ((case-fold-search t) phones)
    (while (re-search-forward (or phone-regexp
                                  bbdb-snarf-phone-eu-regexp) nil t)
      (goto-char (match-beginning 0))
      (push (vector (bbdb-snarf-label 'phone)
                    (match-string 1))
            phones)
      (replace-match ""))
    (bbdb-record-set-phone record (nconc (bbdb-record-phone record)
                                         (nreverse phones)))))

(defun bbdb-snarf-streets (address)
  "Snarf streets for ADDRESS.  This assumes a narrowed region."
  (bbdb-address-set-streets address (bbdb-split "\n" (buffer-string)))
  (delete-region (point-min) (point-max)))

(defun bbdb-snarf-address-us (record)
  "Snarf a US address for RECORD."
  (let ((address (make-vector bbdb-address-length nil)))
    (cond ((re-search-forward bbdb-snarf-postcode-us-regexp nil t)
           ;; Streets, City, State Postcode
           (save-restriction
             (narrow-to-region (point-min) (match-end 0))
             ;; Postcode
             (goto-char (match-beginning 0))
             (bbdb-address-set-postcode address
              (bbdb-parse-postcode (match-string 1)))
             ;; State
             (skip-chars-backward " \t")
             (let ((pos (point)))
               (skip-chars-backward "^ \t,")
               (bbdb-address-set-state address (buffer-substring (point) pos)))
             ;; City
             (skip-chars-backward " \t,")
             (let ((pos (point)))
               (beginning-of-line)
               (bbdb-address-set-city address (buffer-substring (point) pos)))
             ;; Toss it
             (forward-char -1)
             (delete-region (point) (point-max))
             ;; Streets
             (goto-char (point-min))
             (bbdb-snarf-streets address)))
          ;; Try for just Streets, City, State
          ((let (case-fold-search)
             (re-search-forward "^\\(.*\\), \\([A-Z][A-Za-z]\\)$" nil t))
           (bbdb-address-set-city address (match-string 1))
           (bbdb-address-set-state address (match-string 2))
           (replace-match "")
           (save-restriction
             (narrow-to-region (point-min) (match-beginning 0))
             (goto-char (point-min))
             (bbdb-snarf-streets address))))
    (when (bbdb-address-city address)
      (if bbdb-snarf-address-us-country
          (bbdb-address-set-country address bbdb-snarf-address-us-country))
      ;; Fixme: There are no labels anymore.  `bbdb-snarf-streets' snarfed
      ;; everything that was left!
      (bbdb-address-set-label address (bbdb-snarf-label 'address))
      (bbdb-record-set-address record
                               (nconc (bbdb-record-address record)
                                      (list address))))))

(defun bbdb-snarf-address-eu (record &optional postcode-regexp country)
  "Snarf a European address for RECORD.
POSTCODE-REGEXP is a regexp matching the postcode assumed to appear
at the beginning of a line followed by the name of the city.  This format
is used in many continental European countries.
POSTCODE-REGEXP defaults to `bbdb-snarf-postcode-eu-regexp'.
COUNTRY is the country to use.  It defaults to `bbdb-snarf-address-eu-country'."
  (when (re-search-forward (or postcode-regexp
                               bbdb-snarf-postcode-eu-regexp) nil t)
    (let ((address (make-vector bbdb-address-length nil)))
      (save-restriction
        (goto-char (match-end 0))
        (narrow-to-region (point-min) (line-end-position))
        ;; Postcode
        (bbdb-address-set-postcode address (match-string 1))
        ;; City
        (skip-chars-forward " \t")
        (bbdb-address-set-city address (buffer-substring (point) (point-max)))
        ;; Toss it
        (delete-region (match-beginning 0) (point-max))
        ;; Streets
        (goto-char (point-min))
        (bbdb-snarf-streets address))
      (unless country (setq country bbdb-snarf-address-eu-country))
      (if country (bbdb-address-set-country address country))
      (bbdb-address-set-label address (bbdb-snarf-label 'address))
      (bbdb-record-set-address record
                               (nconc (bbdb-record-address record)
                                      (list address))))))

(defun bbdb-snarf-url (record)
  "Snarf URL for RECORD.
This uses the first subexpresion of `bbdb-snarf-url-regexp'."
  (when (and bbdb-snarf-url
             (let ((case-fold-search t))
               (re-search-forward bbdb-snarf-url-regexp nil t)))
    (bbdb-record-set-xfields
     record
     (nconc (bbdb-record-xfields record)
            (list (cons bbdb-snarf-url (match-string 1)))))
    (replace-match "")))

(defun bbdb-snarf-notes (record)
  "Snarf notes for RECORD."
  (when (/= (point-min) (point-max))
    (bbdb-record-set-xfields
     record
     (nconc (bbdb-record-xfields record)
            (list (cons bbdb-default-xfield (buffer-string)))))
    (erase-buffer)))

(defsubst bbdb-snarf-rule-interactive ()
  "Read snarf rule interactively."
  (intern
   (completing-read
    (format "Rule: (default `%s') " bbdb-snarf-rule-default)
    bbdb-snarf-rule-alist nil t nil nil
    (symbol-name bbdb-snarf-rule-default))))

;;;###autoload
(defun bbdb-snarf-paragraph (pos &optional rule)
  "Snarf BBDB record from paragraph around position POS using RULE.
The paragraph is the one that contains POS or follows POS.
Interactively POS is the position of point.
RULE defaults to `bbdb-snarf-rule-default'.
See `bbdb-snarf-rule-alist' for details."
  (interactive (list (point) (bbdb-snarf-rule-interactive)))
  (bbdb-snarf (save-excursion
                (goto-char pos)
                ;; similar to `mark-paragraph'
                (let ((end (progn (forward-paragraph 1) (point))))
                  (buffer-substring-no-properties
                   (progn (backward-paragraph 1) (point))
                   end)))
              rule))

;;;###autoload
(defun bbdb-snarf-yank (&optional rule)
  "Snarf a BBDB record from latest kill using RULE.
The latest kill may also be a window system selection, see `current-kill'.
RULE defaults to `bbdb-snarf-rule-default'.
See `bbdb-snarf-rule-alist' for details."
  (interactive (list (bbdb-snarf-rule-interactive)))
  (bbdb-snarf (current-kill 0) rule))

;;;###autoload
(defun bbdb-snarf (string &optional rule)
  "Snarf a BBDB record in STRING using RULE.  Display and return this record.
Interactively, STRING is the current region.
RULE defaults to `bbdb-snarf-rule-default'.
See `bbdb-snarf-rule-alist' for details."
  (interactive
   (list (buffer-substring-no-properties (region-beginning) (region-end))
         (bbdb-snarf-rule-interactive)))

  (bbdb-editable)
  (let ((record (bbdb-empty-record)))
    (with-current-buffer (get-buffer-create " *BBDB Snarf*")
      (erase-buffer)
      (insert (substring-no-properties string))
      (mapc (lambda (fun)
              (goto-char (point-min))
              (funcall fun record))
            (cdr (assq (or rule bbdb-snarf-rule-default)
                       bbdb-snarf-rule-alist))))
    (let ((old-record (car (bbdb-message-search
                            (bbdb-concat 'name-first-last
                                         (bbdb-record-firstname record)
                                         (bbdb-record-lastname record))
                            (car (bbdb-record-mail record))))))
      ;; Install RECORD after searching for OLD-RECORD
      (bbdb-change-record record)
      (if old-record (bbdb-merge-records old-record record)))
    (bbdb-display-records (list record))
    record))

;; Some test cases
;;
;; US:
;;
;; another test person
;; 1234 Gridley St.
;; Los Angeles, CA 91342
;; 555-1212
;; test@person.net
;; http://www.foo.bar/
;; other stuff about this person
;;
;; test person
;; 1234 Gridley St.
;; St. Los Angeles, CA 91342-1234
;; 555-1212
;; <test@person.net>
;;
;; x test person
;; 1234 Gridley St.
;; Los Angeles, California 91342-1234
;; work: 555-1212
;; home: 555-1213
;; test@person.net
;;
;; y test person
;; 1234 Gridley St.
;; Los Angeles, CA
;; 555-1212
;; test@person.net
;;
;; z test person
;; 555-1212
;; test@person.net
;;
;; EU:
;;
;; Maja Musterfrau
;; Strasse 15
;; 12345 Ort
;; +49 12345
;; phon: (110) 123 456
;; mobile: (123) 456 789
;; xxx.xxx@xxxx.xxx
;; http://www.xxx.xx
;; notes bla bla bla

(provide 'bbdb-snarf)

;;; bbdb-snarf.el ends here
