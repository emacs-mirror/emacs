;;; org-num.el --- Dynamic Headlines Numbering  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Keywords: outlines, hypermedia, calendar, text

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library provides dynamic numbering for Org headlines.  Use
;;
;;     <M-x org-num-mode>
;;
;; to toggle it.
;;
;; You can select what is numbered according to level, tags, COMMENT
;; keyword, or UNNUMBERED property.  You can also skip footnotes
;; sections.  See `org-num-max-level', `org-num-skip-tags',
;; `org-num-skip-commented', `org-num-skip-unnumbered', and
;; `org-num-skip-footnotes' for details.
;;
;; You can also control how the numbering is displayed by setting
;;`org-num-face' and `org-num-format-function'.
;;
;; Internally, the library handles an ordered list, per buffer
;; position, of overlays in `org-num--overlays'.  These overlays are
;; marked with the `org-num' property set to a non-nil value.
;;
;; Overlays store the level of the headline in the `level' property,
;; and the face used for the numbering in `numbering-face'.
;;
;; The `skip' property is set to t when the corresponding headline has
;; some characteristic -- e.g., a node property, or a tag -- that
;; prevents it from being numbered.
;;
;; An overlay with `org-num' property set to `invalid' is called an
;; invalid overlay.  Modified overlays automatically become invalid
;; and set `org-num--invalid-flag' to a non-nil value.  After
;; a change, `org-num--invalid-flag' indicates numbering needs to be
;; updated and invalid overlays indicate where the buffer needs to be
;; parsed.  So does `org-num--missing-overlay' variable.  See
;; `org-num--verify' function for details.
;;
;; Numbering display is done through the `after-string' property.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'org-macs)
(require 'org) ;Otherwise `org-num--comment-re' burps on `org-comment-string'

(defvar org-comment-string)
(defvar org-complex-heading-regexp)
(defvar org-cycle-level-faces)
(defvar org-footnote-section)
(defvar org-level-faces)
(defvar org-n-level-faces)
(defvar org-odd-levels-only)

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-reduced-level "org" (l))


;;; Customization

;;;###autoload
(defcustom org-num-face nil
  "Face to use for numbering.
When nil, use the same face as the headline.  This value is
ignored if `org-num-format-function' specifies a face for its
output."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type '(choice (const :tag "Like the headline" nil)
                 (face :tag "Use face"))
  :safe (lambda (val) (or (null val) (facep val))))

(defcustom org-num-format-function #'org-num-default-format
  "Function used to display numbering.
It is called with one argument, a list of numbers, and should
return a string, or nil.  When nil, no numbering is displayed.
Any `face' text property on the returned string overrides
`org-num-face'."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'function)

;;;###autoload
(defcustom org-num-max-level nil
  "Level below which headlines are not numbered.
When set to nil, all headlines are numbered."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type '(choice (const :tag "Number everything" nil)
                 (integer :tag "Stop numbering at level"))
  :safe (lambda (val) (or (null val) (wholenump val))))

;;;###autoload
(defcustom org-num-skip-commented nil
  "Non-nil means commented sub-trees are not numbered."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'boolean
  :safe #'booleanp)

;;;###autoload
(defcustom org-num-skip-footnotes nil
  "Non-nil means footnotes sections are not numbered."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'boolean
  :safe #'booleanp)

;;;###autoload
(defcustom org-num-skip-tags nil
  "List of tags preventing the numbering of sub-trees.

For example, add \"ARCHIVE\" to this list to avoid numbering
archived sub-trees.

Tag in this list prevent numbering the whole sub-tree,
irrespective to `org-use-tag-inheritance', or other means to
control tag inheritance."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type '(repeat (string :tag "Tag"))
  :safe #'org-list-of-strings-p)

;;;###autoload
(defcustom org-num-skip-unnumbered nil
  "Non-nil means numbering obeys to UNNUMBERED property."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'boolean
  :safe #'booleanp)


;;; Internal Variables

(defconst org-num--comment-re (format "\\`%s\\(?: \\|$\\)" org-comment-string)
  "Regexp matching a COMMENT keyword at headline beginning.")

(defvar-local org-num--overlays nil
  "Ordered list of overlays used for numbering outlines.")
(put 'org-num--overlays 'permanent-local t)

(defvar-local org-num--skip-level nil
  "Level below which headlines from current tree are not numbered.
When nil, all headlines are numbered.  It is used to handle
inheritance of no-numbering attributes.")

(defvar-local org-num--numbering nil
  "Current headline numbering.
A numbering is a list of integers, in reverse order.  So numbering
for headline \"1.2.3\" is (3 2 1).")

(defvar-local org-num--missing-overlay nil
  "Buffer position signaling a headline without an overlay.")

(defvar-local org-num--invalid-flag nil
  "Non-nil means an overlay became invalid since last update.")


;;; Internal Functions

(defsubst org-num--headline-regexp ()
  "Return regexp matching a numbered headline."
  (if (null org-num-max-level) (org-with-limited-levels org-outline-regexp-bol)
    (format "^\\*\\{1,%d\\} "
            (if org-odd-levels-only (1- (* 2 org-num-max-level))
              org-num-max-level))))

(defsubst org-num--overlay-p (o)
  "Non-nil if overlay O is a numbering overlay."
  (overlay-get o 'org-num))

(defsubst org-num--valid-overlay-p (o)
  "Non-nil if overlay O is still active in the buffer."
  (not (eq 'invalid (overlay-get o 'org-num))))

(defsubst org-num--invalidate-overlay (o)
  "Mark overlay O as invalid.
Update `org-num--invalid-flag' accordingly."
  (overlay-put o 'org-num 'invalid)
  (setq org-num--invalid-flag t))

(defun org-num--clear ()
  "Remove all numbering overlays in current buffer."
  (mapc #'delete-overlay org-num--overlays)
  (setq org-num--overlays nil))

(defun org-num--make-overlay (numbering level skip)
  "Return overlay for numbering headline at point.

NUMBERING is the numbering to use, as a list of integers, or nil
if nothing should be displayed.  LEVEL is the level of the
headline.  SKIP is its skip value.

Assume point is at a headline."
  (let ((after-edit-functions
         (list (lambda (o &rest _) (org-num--invalidate-overlay o))))
        (o (save-excursion
             (forward-line 0)
             (skip-chars-forward "*")
             (make-overlay (line-beginning-position) (1+ (point))))))
    (overlay-put o 'org-num t)
    (overlay-put o 'skip skip)
    (overlay-put o 'level level)
    (overlay-put o 'numbering-face
                 (or org-num-face
                     ;; Compute face that would be used at the
                     ;; headline.  We cannot extract it from the
                     ;; buffer: at the time the overlay is created,
                     ;; Font Lock has not proceeded yet.
                     (nth (if org-cycle-level-faces
                              (% (1- level) org-n-level-faces)
                            (1- (min level org-n-level-faces)))
                          org-level-faces)))
    (overlay-put o 'modification-hooks after-edit-functions)
    (overlay-put o 'insert-in-front-hooks after-edit-functions)
    (org-num--refresh-display o numbering)
    o))

(defun org-num--refresh-display (overlay numbering)
  "Refresh OVERLAY's display.
NUMBERING specifies the new numbering, as a list of integers, or
nil if nothing should be displayed.  Assume OVERLAY is valid."
  (let ((display (and numbering
                      (funcall org-num-format-function (reverse numbering)))))
    (when (and display (not (get-text-property 0 'face display)))
      (org-add-props display `(face ,(overlay-get overlay 'numbering-face))))
    (overlay-put overlay 'after-string display)))

(defun org-num--skip-value ()
  "Return skip value for headline at point.
Value is t when headline should not be numbered, and nil
otherwise."
  (org-match-line org-complex-heading-regexp)
  (let ((title (match-string 4))
        (tags (and org-num-skip-tags
                   (match-end 5)
                   (org-split-string (match-string 5) ":"))))
    (or (and org-num-skip-footnotes
             org-footnote-section
             (equal title org-footnote-section))
        (and org-num-skip-commented
	     title
             (let ((case-fold-search nil))
               (string-match org-num--comment-re title))
             t)
        (and org-num-skip-tags
             (cl-some (lambda (tag) (member tag org-num-skip-tags))
                      tags)
             t)
        (and org-num-skip-unnumbered
             (org-entry-get (point) "UNNUMBERED" 'selective)
             t))))

(defun org-num--current-numbering (level skip)
  "Return numbering for current headline.
LEVEL is headline's level, and SKIP its skip value.  Return nil
if headline should be skipped."
  (cond
   ;; Skipped by inheritance.
   ((and org-num--skip-level (> level org-num--skip-level)) nil)
   ;; Skipped by a non-nil skip value; set `org-num--skip-level'
   ;; to skip the whole sub-tree later on.
   (skip (setq org-num--skip-level level) nil)
   (t
    (setq org-num--skip-level nil)
    ;; Compute next numbering, and update `org-num--numbering'.
    (let ((last-level (length org-num--numbering)))
      (setq org-num--numbering
            (cond
             ;; First headline : nil => (1), or (1 0)...
             ((null org-num--numbering) (cons 1 (make-list (1- level) 0)))
             ;; Sibling: (1 1) => (2 1).
             ((= level last-level)
              (cons (1+ (car org-num--numbering)) (cdr org-num--numbering)))
             ;; Parent: (1 1 1) => (2 1), or (2).
             ((< level last-level)
              (let ((suffix (nthcdr (- last-level level) org-num--numbering)))
                (cons (1+ (car suffix)) (cdr suffix))))
             ;; Child: (1 1) => (1 1 1), or (1 0 1 1)...
             (t
              (append (cons 1 (make-list (- level last-level 1) 0))
                      org-num--numbering))))))))

(defun org-num--number-region (start end)
  "Add numbering overlays between START and END positions.
When START or END are nil, use buffer boundaries.  Narrowing, if
any, is ignored.  Return the list of created overlays, newest
first."
  (org-with-point-at (or start 1)
    ;; Do not match headline starting at START.
    (when start (end-of-line))
    (let ((regexp (org-num--headline-regexp))
          (new nil))
      (while (re-search-forward regexp end t)
        (let* ((level (org-reduced-level
                       (- (match-end 0) (match-beginning 0) 1)))
               (skip (org-num--skip-value))
               (numbering (org-num--current-numbering level skip)))
          ;; Apply numbering to current headline.  Store overlay for
          ;; the return value.
          (push (org-num--make-overlay numbering level skip)
                new)))
      new)))

(defun org-num--update ()
  "Update buffer's numbering.
This function removes invalid overlays and refreshes numbering
for the valid ones in the numbering overlays list.  It also adds
missing overlays to that list."
  (setq org-num--skip-level nil)
  (setq org-num--numbering nil)
  (let ((new-overlays nil)
        (overlay nil))
    (while (setq overlay (pop org-num--overlays))
      (cond
       ;; Valid overlay.
       ;;
       ;; First handle possible missing overlays OVERLAY.  If missing
       ;; overlay marker is pointing before next overlay and after the
       ;; last known overlay, make sure to parse the buffer between
       ;; these two overlays.
       ((org-num--valid-overlay-p overlay)
        (let ((next (overlay-start overlay))
              (last (and new-overlays (overlay-start (car new-overlays)))))
          (cond
           ((null org-num--missing-overlay))
           ((> org-num--missing-overlay next))
           ((or (null last) (> org-num--missing-overlay last))
            (setq org-num--missing-overlay nil)
            (setq new-overlays (nconc (org-num--number-region last next)
                                      new-overlays)))
           ;; If it is already after the last known overlay, reset it:
           ;; some previous invalid overlay already triggered the
           ;; necessary parsing.
           (t
            (setq org-num--missing-overlay nil))))
        ;; Update OVERLAY's numbering.
        (let* ((level (overlay-get overlay 'level))
               (skip (overlay-get overlay 'skip))
               (numbering (org-num--current-numbering level skip)))
          (org-num--refresh-display overlay numbering)
          (push overlay new-overlays)))
       ;; Invalid overlay.  It indicates that the buffer needs to be
       ;; parsed again between the two surrounding valid overlays or
       ;; buffer boundaries.
       (t
        ;; Delete all consecutive invalid overlays: we re-create all
        ;; overlays between last valid overlay and the next one.
        (delete-overlay overlay)
        (while (and org-num--overlays
                    (not (org-num--valid-overlay-p (car org-num--overlays))))
          (delete-overlay (pop org-num--overlays)))
        ;; Create and register new overlays.
        (let ((last (and new-overlays (overlay-start (car new-overlays))))
              (next (and org-num--overlays
                         (overlay-start (car org-num--overlays)))))
          (setq new-overlays (nconc (org-num--number-region last next)
                                    new-overlays))))))
    ;; If invalid position hasn't been handled yet, it must be located
    ;; between last valid overlay and end of the buffer.  Parse that
    ;; area before returning.
    (when org-num--missing-overlay
      (let ((last (and new-overlays (overlay-start (car new-overlays)))))
        (setq new-overlays (nconc (org-num--number-region last nil)
                                  new-overlays))))
    ;; Numbering is now up-to-date.  Reset invalid flag.  Also return
    ;; `org-num--overlays' in a sorted fashion.
    (setq org-num--invalid-flag nil)
    (setq org-num--overlays (nreverse new-overlays))))

(defun org-num--verify (beg end _)
  "Check numbering integrity; update it if necessary.
This function is meant to be used in `after-change-functions'.
See this variable for the meaning of BEG and END."
  (setq org-num--missing-overlay nil)
  (save-match-data
    (org-with-point-at beg
      (let ((regexp (org-num--headline-regexp)))
        ;; At this point, directly altered overlays between BEG and
        ;; END are marked as invalid and will trigger a full update.
        ;; However, there are still two cases to handle.
        ;;
        ;; First, some valid overlays may need to be invalidated, due
        ;; to an indirect change.  That happens when the skip value --
        ;; see `org-num--skip-value' -- of the heading BEG belongs to
        ;; is altered, or when deleting the newline character right
        ;; before the next headline.
        (save-excursion
          ;; Bail out if we're before first headline or within
          ;; a headline too deep to be numbered.
          (when (and (org-with-limited-levels
                      (ignore-errors (org-back-to-heading t)))
                     (looking-at regexp))
            (pcase (get-char-property-and-overlay (point) 'org-num)
              (`(nil)
               ;; At a headline, without a numbering overlay: change
               ;; just created one.  Mark it for parsing.
               (setq org-num--missing-overlay (point)))
              (`(t . ,o)
               ;; Check if skip value changed.  Invalidate overlay
               ;; accordingly.
               (unless (eq (org-num--skip-value) (overlay-get o 'skip))
                 (org-num--invalidate-overlay o)))
              (_ nil))))
        ;; Deleting the newline character before a numbering overlay
        ;; doesn't invalidate it, even though it could land in the
        ;; middle of a line.  Be sure to catch this case.
        (when (and (= beg end) (not (bolp)))
          (pcase (get-char-property-and-overlay (point) 'org-num)
            (`(t . ,o) (org-num--invalidate-overlay o))
            (_ nil)))
        ;; Second, if nothing is marked as invalid, and therefore if
        ;; no full update is due so far, changes may still have
        ;; created new headlines, at BEG -- which is actually handled
        ;; by the previous phase --, or, in case of a multi-line
        ;; insertion, at END, or in-between.
        (unless (or org-num--invalid-flag
                    org-num--missing-overlay
                    (<= end (line-end-position))) ;single line change
          (forward-line)
          (when (or (re-search-forward regexp end 'move)
                    ;; Check if change created a headline after END.
                    (progn (skip-chars-backward "*") (looking-at regexp)))
            (setq org-num--missing-overlay (line-beginning-position))))))
    ;; Update numbering only if a headline was altered or created.
    (when (or org-num--missing-overlay org-num--invalid-flag)
      (org-num--update))))


;;; Public Functions

;;;###autoload
(defun org-num-default-format (numbering)
  "Default numbering display function.
NUMBERING is a list of numbers."
  (concat (mapconcat #'number-to-string numbering ".") " "))

;;;###autoload
(define-minor-mode org-num-mode
  "Dynamic numbering of headlines in an Org buffer."
  :lighter " o#"
  (cond
   (org-num-mode
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot activate headline numbering outside Org mode"))
    (org-num--clear)
    (setq org-num--numbering nil)
    (setq org-num--overlays (nreverse (org-num--number-region nil nil)))
    (add-hook 'after-change-functions #'org-num--verify nil t)
    (add-hook 'change-major-mode-hook #'org-num--clear nil t))
   (t
    (org-num--clear)
    (remove-hook 'after-change-functions #'org-num--verify t)
    (remove-hook 'change-major-mode-hook #'org-num--clear t))))

(provide 'org-num)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-num.el ends here
