;;; org-datetree.el --- Create date entries in a tree -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2026 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code to create entries in a tree where the
;; top-level nodes represent years, the level 2 nodes represent the
;; months, and the level 1 entries days.  It also implements
;; extensions to the datetree that allow for other levels such as
;; quarters and weeks.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cal-iso)
(require 'org)
(require 'org-element)

(defcustom org-datetree-add-timestamp nil
  "When non-nil, add a time stamp matching date of entry.
Added time stamp is active unless value is `inactive'."
  :group 'org-capture
  :version "24.3"
  :type '(choice
	  (const :tag "Do not add a time stamp" nil)
	  (const :tag "Add an inactive time stamp" inactive)
	  (const :tag "Add an active time stamp" active)))

;;;###autoload
(defun org-datetree-find-date-create (d &optional keep-restriction)
  "Find or create a day entry for date D.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (org-datetree-find-create-entry '(year month day) d keep-restriction))

;;;###autoload
(defun org-datetree-find-month-create (d &optional keep-restriction)
  "Find or create a month entry for date D.
Compared to `org-datetree-find-date-create' this function creates
entries grouped by year-month instead of year-month-day.
If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point."
  (org-datetree-find-create-entry '(year month) d keep-restriction))

;;;###autoload
(defun org-datetree-find-iso-week-create (d &optional keep-restriction)
  "Find or create an ISO week entry for date D.
Compared to `org-datetree-find-date-create' this function creates
entries grouped by year-week-day instead of year-month-day.  If
KEEP-RESTRICTION is non-nil, do not widen the buffer.  When it is
nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then
the tree will be built under the headline at point."
  (org-datetree-find-create-entry '(year week day) d keep-restriction))

;;;###autoload
(defun org-datetree-find-create-entry
    (time-grouping d &optional keep-restriction)
  "Find or create an entry for date D.
Moves point to the beginning of the entry.

TIME-GROUPING specifies the grouping levels of the datetree, and
should be a subset of `(year quarter month week day)'.  Weeks are
assigned to years according to ISO-8601.  If TIME-GROUPING
contains both `month' and `week', then weeks are assigned to the
month containing Thursday, for consistency with the ISO-8601
year-week rule.  If TIME-GROUPING contains `quarter' and `week'
but not `month', quarters are defined as 13-week periods;
otherwise they are defined as 3-month periods.

If KEEP-RESTRICTION is non-nil, do not widen the buffer.  When it
is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then
the tree will be built under the headline at point.

If `org-datetree-add-timestamp' is non-nil and TIME-GROUPING
includes `day' and a new entry is created, adds a time stamp
after the new headline."
  (when-let* ((setdiff (seq-difference time-grouping
                                      '(year quarter month week day))))
    (error (format "Unrecognized datetree grouping elements %s" setdiff)))
  (let* ((year (calendar-extract-year d))
	 (month (calendar-extract-month d))
	 (day (calendar-extract-day d))
         (time (org-encode-time 0 0 0 day month year))
         (iso-date (calendar-iso-from-absolute
		    (calendar-absolute-from-gregorian d)))
         (week (nth 0 iso-date))
         (nominal-year
          (if (memq 'week time-grouping)
              (nth 2 iso-date)
            year))
         (nominal-month
          (if (memq 'week time-grouping)
              (calendar-extract-month
               ;; anchor on Thurs, to be consistent with weekyear
               (calendar-gregorian-from-absolute
                (calendar-iso-to-absolute
                 `(,week 4 ,nominal-year))))
            month))
         (quarter (if (and (memq 'week time-grouping)
                           (not (memq 'month time-grouping)))
                      (min 4 (1+ (/ (1- week) 13)))
                    (1+ (/ (1- nominal-month) 3))))
         (found-p
          (org-datetree-find-create-hierarchy
           (append
            (when (memq 'year time-grouping)
              (list (list (number-to-string nominal-year)
                          (org-datetree-comparefun-from-regex
                           "\\([12][0-9]\\{3\\}\\)"))))
            (when (memq 'quarter time-grouping)
              (list (list (format "%d-Q%d" nominal-year quarter)
                          (org-datetree-comparefun-from-regex
                           "\\([12][0-9]\\{3\\}-Q[1-4]\\)"))))
            (when (memq 'month time-grouping)
              (list (list (format-time-string
                           "%Y-%m %B" (org-encode-time 0 0 0 1 nominal-month
                                                       nominal-year))
                          (org-datetree-comparefun-from-regex
                           "\\([12][0-9]\\{3\\}-[01][0-9]\\) \\w+"))))
            (when (memq 'week time-grouping)
              (list (list (format-time-string "%G-W%V" time)
                          (org-datetree-comparefun-from-regex
                           "\\([12][0-9]\\{3\\}-W[0-5][0-9]\\)"))))
            (when (memq 'day time-grouping)
              ;; Use regular date instead of ISO-week year/month
              (list (list (format-time-string
                           "%Y-%m-%d %A" (org-encode-time 0 0 0 day month year))
                          (org-datetree-comparefun-from-regex
                           "\\([12][0-9]\\{3\\}-[01][0-9]-[0123][0-9]\\) \\w+")))))
           keep-restriction
           ;; Support the old way of tree placement, using a property
           (cond
            ((seq-set-equal-p time-grouping '(year month day))
             "DATE_TREE")
            ((seq-set-equal-p time-grouping '(year month))
             "DATE_TREE")
            ((seq-set-equal-p time-grouping '(year week day))
             "WEEK_TREE")))))
    (when (memq 'day time-grouping)
      (when (and (not found-p) org-datetree-add-timestamp)
        (save-excursion
          (end-of-line)
          (insert "\n")
          (org-indent-line)
          (org-insert-timestamp
           (org-encode-time 0 0 0 day month year)
           nil
           (eq org-datetree-add-timestamp 'inactive)))))))

(defun org-datetree-comparefun-from-regex (sibling-regex)
  "Construct comparison function based on regular expression.
The generated comparison function can be used with
`org-datetree-find-create-hierarchy'.  SIBLING-REGEX should be a
regex that matches the headline and its siblings, with 1 match
group.  Headlines are compared by the lexicographic ordering of
match group 1.  The generated function returns -1 if the first
argument is earlier, 1 if later, 0 if equal, or nil if either
argument doesn't match."
  (lambda (sibling-title new-title)
    (let ((target-match (and (string-match sibling-regex new-title)
                             (match-string 1 new-title)))
          (sibling-match (and (string-match sibling-regex sibling-title)
                              (match-string 1 sibling-title))))
      (cond
       ((not (and target-match sibling-match)) nil)
       ((string< sibling-match target-match) -1)
       ((string> sibling-match target-match) 1)
       (t 0)))))

(defun org-datetree-find-create-hierarchy
    (hier-pairs &optional keep-restriction legacy-prop)
  "Find or create entry in datetree using the full date hierarchy.
Moves point to the beginning of the entry.  Returns non-nil if an
existing entry was found, or nil if a new entry was created.

HIER-PAIRS is a list whose first entry corresponds to the outermost element
(e.g. year) and last entry corresponds to the innermost (e.g. day).
Each entry of the list is a pair, the car is the headline for that level
(e.g. \"2024\" or \"2024-12-28 Saturday\"), and the cadr is a
string comparison function for sorting each headline among its
siblings.  The comparison function should take 2 arguments,
corresponding to the titles of 2 headlines, and return a negative
number if the first headline is earlier, a positive number if the
second headline is earlier, 0 or t if the headlines are at the
same time, or `nil' if a headline isn't a valid datetree
subheading.  For example, HIER-PAIRS could look like

   ((\"2024\" compare-year-fun)
    (\"2024-12 December\" compare-month-fun)
    (\"2024-12-28 Saturday\" compare-day-fun))

where compare-month-fun would be some function where
(compare-month-fun \"2024-11 November\" \"2024-12 December\") is
negative, and (compare-month-fun \"2024-12-December\" \"Potato\")
is nil.  One way to construct such a comparison function is with
`org-datetree-comparefun-from-regex'.

If KEEP-RESTRICTION is non-nil, do not widen the buffer.
When it is nil, the buffer will be widened to make sure an existing date
tree can be found.  If it is the symbol `subtree-at-point', then the tree
will be built under the headline at point.

If LEGACY-PROP is non-nil, the tree is located by searching for a
headline with property LEGACY-PROP, supporting the old way of
tree placement via a property."
  (let ((level 1)
        found-p)
    (save-restriction
      ;; get the datetree base and narrow to it
      (if (eq keep-restriction 'subtree-at-point)
          (progn
	    (unless (org-at-heading-p) (error "Not at heading"))
	    (widen)
	    (org-narrow-to-subtree)
            (setq level (org-get-valid-level (org-current-level) 1)))
        (unless keep-restriction (widen))
        ;; Support the old way of tree placement, using a property
        (let ((prop (and legacy-prop (org-find-property legacy-prop))))
          (when prop
            (progn
              (goto-char prop)
	      (org-narrow-to-subtree)
              (setq level (org-get-valid-level (org-current-level) 1))))))
      (cl-loop
       for pair in hier-pairs
       do
       (setq found-p (org-datetree--find-create-subheading
                      (cadr pair) (car pair) level))
       (setq level (1+ level))))
    found-p))

(defun org-datetree--find-create-subheading
    (compare-fun new-title level)
  "Find datetree subheading, or create it if it doesn't exist.
After insertion, move point to beginning of the subheading, and
narrow to its subtree.  Returns non-nil if the heading was found,
or nil if a new heading was created.

NEW-TITLE is the title of the subheading to be found or created.
LEVEL is the level of the headline to be found or created.
COMPARE-FUN is a function of 2 arguments for comparing headline
titles; it should return a negative number if the first headline
precedes the second, a positive number if the second number has
precedence, 0 or t if the headlines are at the same time, and nil
if a headline isn't a valid datetree subheading at this level."
  (let* ((nstars (if org-odd-levels-only (1- (* 2 level)) level))
         (heading-re (format "^\\*\\{%d\\}" nstars))
         (sibling (car (org-element-cache-map
                        (lambda (d)
                          (when (= (org-element-property :level d) level)
                            (let ((compare-result
                                   (funcall compare-fun
                                            (org-element-property :raw-value d)
                                            new-title)))
                              (and compare-result
                                   (or (eq compare-result t) (>= compare-result 0))
                                   d))))
                        :granularity 'headline
                        :restrict-elements '(headline)
                        :next-re heading-re
                        :fail-re heading-re
                        :narrow t
                        :limit-count 1))))
    ;; go to headline, or first successor sibling, or end of buffer
    (if sibling
        (goto-char (org-element-property :begin sibling))
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))
    (if (and sibling
             (memq (funcall compare-fun
                            (org-element-property :raw-value sibling)
                            new-title)
                   '(0 t)))
        ;; narrow and return the matched headline
        (progn
          (org-narrow-to-subtree)
          t)
      ;; insert new headline, narrow, and return it
      (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) (point))
      (when (org--blank-before-heading-p) (insert "\n"))
      (insert
       (format "\n%s %s\n"
               (make-string nstars ?*)
               new-title))
      (forward-line -1)
      (org-narrow-to-subtree)
      nil)))

(defun org-datetree-file-entry-under (txt d)
  "Insert a node TXT into the date tree under date D."
  (org-datetree-find-date-create d)
  (let ((level (org-get-valid-level (funcall outline-level) 1)))
    (org-end-of-subtree t t)
    (org-back-over-empty-lines)
    (org-paste-subtree level txt)))

(defun org-datetree-cleanup ()
  "Make sure all entries in the current tree are under the correct date.
It may be useful to restrict the buffer to the applicable portion
before running this command, even though the command tries to be smart."
  (interactive)
  (goto-char (point-min))
  (let ((dre (concat "\\<" org-deadline-string "\\>[ \t]*\\'"))
	(sre (concat "\\<" org-scheduled-string "\\>[ \t]*\\'")))
    (while (re-search-forward org-ts-regexp nil t)
      (catch 'next
	(let ((tmp (buffer-substring
		    (max (line-beginning-position)
			 (- (match-beginning 0) org-ds-keyword-length))
		    (match-beginning 0))))
	  (when (or (string-suffix-p "-" tmp)
		    (string-match dre tmp)
		    (string-match sre tmp))
	    (throw 'next nil))
	  (let* ((dct (decode-time (org-time-string-to-time (match-string 0))))
		 (date (list (nth 4 dct) (nth 3 dct) (nth 5 dct)))
		 (year (nth 2 date))
		 (month (car date))
		 (day (nth 1 date))
		 (pos (point))
		 (hdl-pos (progn (org-back-to-heading t) (point))))
	    (unless (org-up-heading-safe)
	      ;; No parent, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (unless (looking-at "\\*+[ \t]+[0-9]+-[0-1][0-9]-[0-3][0-9]")
	      ;; Parent looks wrong, we are not in a date tree.
	      (goto-char pos)
	      (throw 'next nil))
	    (when (looking-at (format "\\*+[ \t]+%d-%02d-%02d" year month day))
	      ;; At correct date already, do nothing.
	      (goto-char pos)
	      (throw 'next nil))
	    ;; OK, we need to refile this entry.
	    (goto-char hdl-pos)
	    (org-cut-subtree)
	    (save-excursion
	      (save-restriction
		(org-datetree-file-entry-under (current-kill 0) date)))))))))

(provide 'org-datetree)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-datetree.el ends here
