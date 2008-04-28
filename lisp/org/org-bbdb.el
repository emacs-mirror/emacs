;;; org-bbdb.el --- Support for links to BBDB entries from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>,
;;         Thomas Baumann <thomas dot baumann at ch dot tum dot de>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to BBDB database entries from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.


;; It also implements an interface (based on Ivar Rummelhoff's
;; bbdb-anniv.el) for those org-mode users, who do not use the diary
;; but who do want to include the anniversaries stored in the BBDB
;; into the org-agenda.  If you already include the `diary' into the
;; agenda, you might want to prefer to include the anniversaries in
;; the diary using bbdb-anniv.el.
;;
;; Put the following in /somewhere/at/home/diary.org and make sure
;; that this file is in `org-agenda-files`
;;
;; %%(org-bbdb-anniversaries)
;;
;; For example my diary.org looks like:
;; * Anniversaries
;; #+CATEGORY: Anniv
;; %%(org-bbdb-anniversaries)
;;
;;
;; The anniversaries are stored in BBDB in the field `anniversary'
;; in the format
;;
;;     YYYY-MM-DD{ CLASS-OR-FORMAT-STRING}*
;;     {\nYYYY-MM-DD CLASS-OR-FORMAT-STRING}*
;;
;; CLASS-OR-FORMAT-STRING is one of two things:
;;
;;  * an identifier for a class of anniversaries (eg. birthday or
;;    wedding) from `org-bbdb-anniversary-format-alist'.
;;  * the (format) string displayed in the diary.
;;
;; It defaults to the value of `org-bbdb-default-anniversary-format'
;; ("birthday" by default).
;;
;; The substitutions in the format string are (in order):
;;  * the name of the record containing this anniversary
;;  * the number of years
;;  * an ordinal suffix (st, nd, rd, th) for the year
;;
;; See the documentation of `org-bbdb-anniversary-format-alist' for
;; further options.
;;
;; Example
;;
;;       1973-06-22
;;       20??-??-?? wedding
;;       1998-03-12 %s created bbdb-anniv.el %d years ago

;;; Code:

(require 'org)
(eval-when-compile
  (require 'cl))

;; Declare external functions and variables

(declare-function bbdb "ext:bbdb-com" (string elidep))
(declare-function bbdb-company "ext:bbdb-com" (string elidep))
(declare-function bbdb-current-record "ext:bbdb-com"
		  (&optional planning-on-modifying))
(declare-function bbdb-name "ext:bbdb-com" (string elidep))
(declare-function bbdb-record-getprop "ext:bbdb" (record property))
(declare-function bbdb-record-name "ext:bbdb" (record))
(declare-function bbdb-records "ext:bbdb"
          (&optional dont-check-disk already-in-db-buffer))
(declare-function bbdb-split "ext:bbdb" (string separators))
(declare-function bbdb-string-trim "ext:bbdb" (string))
(declare-function calendar-leap-year-p "calendar" (year))
(declare-function diary-ordinal-suffix "diary-lib" (n))

(defvar date)

;; Customization

(defgroup org-bbdb-anniversaries nil
  "Customizations for including anniversaries from BBDB into Agenda."
  :group 'org-bbdb)

(defcustom org-bbdb-default-anniversary-format "birthday"
  "Default anniversary class."
  :type  'string
  :group 'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-anniversary-format-alist
  '( ("birthday" . "Birthday: %s (%d%s)")
     ("wedding"  . "%s's %d%s wedding anniversary") )
  "How different types of anniversaries should be formatted.
An alist of elements (STRING . FORMAT) where STRING is the name of an
anniversary class and format is either:
1) A format string with the following substitutions (in order):
    * the name of the record containing this anniversary
    * the number of years
    * an ordinal suffix (st, nd, rd, th) for the year

2) A function to be called with three arguments: NAME YEARS SUFFIX
   (string int string) returning a string for the diary or nil.

3) An Emacs Lisp form that should evaluate to a string (or nil) in the
   scope of variables NAME, YEARS and SUFFIX (among others)."
  :type 'sexp
  :group 'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-anniversary-field 'anniversary
  "The BBDB field which contains anniversaries.
The anniversaries are stored in the following format

YYYY-MM-DD Class-or-Format-String

where class is one of the customized classes for anniversaries;
birthday and wedding are predefined.  Format-String can take three
substitutions 1) the name of the record containing this
anniversary, 2) the number of years, and 3) an ordinal suffix for
the year.

Multiple anniversaries can be separated by \\n"
  :type    'symbol
  :group   'org-bbdb-anniversaries
  :require 'bbdb)

(defcustom org-bbdb-extract-date-fun 'org-bbdb-anniv-extract-date
  "How to retrieve `month date year' from the anniversary field.

Customize if you have already filled your bbdb with dates
different from YYYY-MM-DD.  The function must return a list (month
date year)"
  :type 'function
  :group 'org-bbdb-anniversaries
  :require 'bbdb)


;; Install the link type
(org-add-link-type "bbdb" 'org-bbdb-open 'org-bbdb-export)
(add-hook 'org-store-link-functions 'org-bbdb-store-link)

;; Implementation
(defun org-bbdb-store-link ()
  "Store a link to a BBDB database entry."
  (when (eq major-mode 'bbdb-mode)
    ;; This is BBDB, we make this link!
    (let* ((name (bbdb-record-name (bbdb-current-record)))
	   (company (bbdb-record-getprop (bbdb-current-record) 'company))
	   (link (org-make-link "bbdb:" name)))
      (org-store-link-props :type "bbdb" :name name :company company
			    :link link :description name)
      link)))

(defun org-bbdb-export (path desc format)
  "Create the export version of a BBDB link specified by PATH or DESC.
If exporting to either HTML or LaTeX FORMAT the link will be
italicised, in all other cases it is left unchanged."
  "Create the exprt verison of a bbdb link."
  (cond
   ((eq format 'html) (format "<i>%s</i>" (or desc path)))
   ((eq format 'latex) (format "\\textit{%s}" (or desc path)))
   (t (or desc path))))

(defun org-bbdb-open (name)
  "Follow a BBDB link to NAME."
  (require 'bbdb)
  (let ((inhibit-redisplay (not debug-on-error))
	(bbdb-electric-p nil))
    (catch 'exit
      ;; Exact match on name
      (bbdb-name (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Exact match on name
      (bbdb-company (concat "\\`" name "\\'") nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on name
      (bbdb-name name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; Partial match on company
      (bbdb-company name nil)
      (if (< 0 (buffer-size (get-buffer "*BBDB*"))) (throw 'exit nil))
      ;; General match including network address and notes
      (bbdb name nil)
      (when (= 0 (buffer-size (get-buffer "*BBDB*")))
	(delete-window (get-buffer-window "*BBDB*"))
	(error "No matching BBDB record")))))

(defun org-bbdb-anniv-extract-date (time-str)
  "Convert YYYY-MM-DD to (month date year).
Argument TIME-STR is the value retrieved from BBDB."
  (multiple-value-bind (y m d) (bbdb-split time-str "-")
    (list (string-to-number m)
	  (string-to-number d)
	  (string-to-number y))))

(defun org-bbdb-anniv-split (str)
  "Split mutliple entries in the BBDB anniversary field.
Argument STR is the anniversary field in BBDB."
  (let ((pos (string-match "[ \t]" str)))
    (if pos (list (substring str 0 pos)
		  (bbdb-string-trim (substring str pos)))
      (list str nil))))


;;;###autoload
(defun org-bbdb-anniversaries ()
  "Extract anniversaries from BBDB for display in the agenda."
  (require 'diary-lib)
  (let ((dates (list (cons (cons (car date)    ; month
                                 (nth 1 date)) ; day
                           (nth 2 date))))     ; year
        (text ())
        annivs date years
        split class form)
    (dolist (rec (bbdb-records))
      (when (setq annivs (bbdb-record-getprop
                          rec org-bbdb-anniversary-field))
        (setq annivs (bbdb-split annivs "\n"))
        (while annivs
          (setq split (org-bbdb-anniv-split (pop annivs)))
          (multiple-value-bind (m d y)
              (funcall org-bbdb-extract-date-fun (car split))

            (when (and (or (setq date (assoc (cons m d) dates))
                           (and (= d 29)
                                (= m 2)
                                (setq date (assoc '(3 . 1) dates))
                                (not (calendar-leap-year-p (cdr date)))))
                       (< 0 (setq years (-  (cdr date) y))))
              (let* ((class (or (cadr split)
                                org-bbdb-default-anniversary-format))
                     (form (or (cdr (assoc class
                                           org-bbdb-anniversary-format-alist))
                               class))	; (as format string)
                     (name (bbdb-record-name rec))
                     (suffix (diary-ordinal-suffix years))
                     (tmp (cond
                           ((functionp form)
                            (funcall form name years suffix))
                           ((listp form) (eval form))
                           (t (format form name years suffix)))))
                (if text
                    (setq text (append text (list tmp)))
                  (setq text (list tmp))))
              )))))
    (when text
      (mapconcat 'identity text "; "))))

(provide 'org-bbdb)

;; arch-tag: 9e4f275d-d080-48c1-b040-62247f66b5c2
;;; org-bbdb.el ends here
