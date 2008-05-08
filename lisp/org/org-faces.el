;;; org-faces.el --- Face definitions for Org-mode.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the face definitons for Org.

;;; Code:

(require 'org-macs)
(require 'org-compat)

(defgroup org-faces nil
  "Faces in Org-mode."
  :tag "Org Faces"
  :group 'org-font-lock)

(defface org-hide
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black")))
  "Face used to hide leading stars in headlines.
The foreground color of this face should be equal to the background
color of the frame."
  :group 'org-faces)

(defface org-level-1 ;; originally copied from font-lock-function-name-face
  (org-compatible-face 'outline-1
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for level 1 headlines."
  :group 'org-faces)

(defface org-level-2 ;; originally copied from font-lock-variable-name-face
  (org-compatible-face 'outline-2
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark))  (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)  (background light)) (:foreground "yellow"))
      (((class color) (min-colors 8)  (background dark))  (:foreground "yellow" :bold t))
      (t (:bold t))))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defface org-level-3 ;; originally copied from font-lock-keyword-face
  (org-compatible-face 'outline-3
    '((((class color) (min-colors 88) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 88) (background dark))  (:foreground "Cyan1"))
      (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 16) (background dark))  (:foreground "Cyan"))
      (((class color) (min-colors 8)  (background light)) (:foreground "purple" :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "cyan" :bold t))
      (t (:bold t))))
  "Face used for level 3 headlines."
  :group 'org-faces)

(defface org-level-4   ;; originally copied from font-lock-comment-face
  (org-compatible-face 'outline-4
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark))  (:foreground "chocolate1"))
      (((class color) (min-colors 16) (background light)) (:foreground "red"))
      (((class color) (min-colors 16) (background dark))  (:foreground "red1"))
      (((class color) (min-colors 8) (background light))  (:foreground "red" :bold t))
      (((class color) (min-colors 8) (background dark))   (:foreground "red" :bold t))
      (t (:bold t))))
  "Face used for level 4 headlines."
  :group 'org-faces)

(defface org-level-5 ;; originally copied from font-lock-type-face
  (org-compatible-face 'outline-5
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 5 headlines."
  :group 'org-faces)

(defface org-level-6 ;; originally copied from font-lock-constant-face
  (org-compatible-face 'outline-6
    '((((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
      (((class color) (min-colors 8)) (:foreground "magenta"))))
  "Face used for level 6 headlines."
  :group 'org-faces)

(defface org-level-7 ;; originally copied from font-lock-builtin-face
  (org-compatible-face 'outline-7
    '((((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
      (((class color) (min-colors 8)) (:foreground "blue"))))
  "Face used for level 7 headlines."
  :group 'org-faces)

(defface org-level-8 ;; originally copied from font-lock-string-face
  (org-compatible-face 'outline-8
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-special-keyword ;; originally copied from font-lock-string-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (t (:italic t))))
  "Face used for special keywords."
  :group 'org-faces)

(defface org-drawer ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for drawers."
  :group 'org-faces)

(defface org-property-value nil
  "Face used for the value of a property."
  :group 'org-faces)

(defface org-column
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light))
       (:background "grey90"))
      (((class color) (min-colors 16) (background dark))
       (:background "grey30"))
      (((class color) (min-colors 8))
       (:background "cyan" :foreground "black"))
      (t (:inverse-video t))))
  "Face for column display of entry properties."
  :group 'org-faces)

(defface org-column-title
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light))
       (:background "grey90" :underline t :weight bold))
      (((class color) (min-colors 16) (background dark))
       (:background "grey30" :underline t :weight bold))
      (((class color) (min-colors 8))
       (:background "cyan" :foreground "black" :underline t :weight bold))
      (t (:inverse-video t))))
  "Face for column display of entry properties."
  :group 'org-faces)

(when (fboundp 'set-face-attribute)
  ;; Make sure that a fixed-width face is used when we have a column table.
  (set-face-attribute 'org-column nil
		      :height (face-attribute 'default :height)
		      :family (face-attribute 'default :family)))

(defface org-warning
  (org-compatible-face 'font-lock-warning-face
    '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
      (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
      (t (:bold t))))
  "Face for deadlines and TODO keywords."
  :group 'org-faces)

(defface org-archived    ; similar to shadow
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for headline with the ARCHIVE tag."
  :group 'org-faces)

(defface org-link
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-ellipsis
  '((((class color) (background light)) (:foreground "DarkGoldenrod" :underline t))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :underline t))
    (t (:strike-through t)))
  "Face for the ellipsis in folded text."
  :group 'org-faces)

(defface org-target
  '((((class color) (background light)) (:underline t))
    (((class color) (background dark)) (:underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-date
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-sexp-date
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-tag
  '((t (:bold t)))
  "Face for tags."
  :group 'org-faces)

(defface org-todo ; font-lock-warning-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
      (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
      (t (:inverse-video t :bold t))))
  "Face for TODO keywords."
  :group 'org-faces)

(defface org-done ;; originally copied from font-lock-type-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-headline-done ;; originally copied from font-lock-string-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)  (background light)) (:bold nil))))
  "Face used to indicate that a headline is DONE.
This face is only used if `org-fontify-done-headline' is set.  If applies
to the part of the headline after the DONE keyword."
  :group 'org-faces)

(defcustom org-todo-keyword-faces nil
  "Faces for specific TODO keywords.
This is a list of cons cells, with TODO keywords in the car
and faces in the cdr.  The face can be a symbol, or a property
list of attributes, like (:foreground \"blue\" :weight bold :underline t)."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (string :tag "keyword")
	   (sexp :tag "face"))))

(defface org-table ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)  (background light)) (:foreground "blue"))
      (((class color) (min-colors 8)  (background dark)))))
  "Face used for tables."
  :group 'org-faces)

(defface org-formula
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red"))
      (t (:bold t :italic t))))
  "Face for formulas."
  :group 'org-faces)

(defface org-code
  (org-compatible-face nil
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for fixed-with text like code snippets."
  :group 'org-faces
  :version "22.1")

(defface org-verbatim
  (org-compatible-face nil
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50" :underline t))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70" :underline t))
      (((class color) (min-colors 8) (background light))
       (:foreground "green" :underline t))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow" :underline t))))
  "Face for fixed-with text like code snippets."
  :group 'org-faces
  :version "22.1")

(defface org-agenda-structure ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used in agenda for captions and dates."
  :group 'org-faces)

(unless (facep 'org-agenda-date)
  (copy-face 'org-agenda-structure 'org-agenda-date)
  (set-face-doc-string 'org-agenda-date
		       "Face used in agenda for normal days."))

(unless (facep 'org-agenda-date-weekend)
  (copy-face 'org-agenda-date 'org-agenda-date-weekend)
  (set-face-doc-string 'org-agenda-date-weekend
		       "Face used in agenda for weekend days.
See the variable `org-agenda-weekend-days' for a definition of which days
belong to the weekend.")
  (when (fboundp 'set-face-attribute)
    (set-face-attribute 'org-agenda-date-weekend nil :weight 'bold)))

(defface org-scheduled-today
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
      (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t :italic t))))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-scheduled-previously
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
      (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-upcoming-deadline
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
      (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defcustom org-agenda-deadline-faces
  '((1.0 . org-warning)
    (0.5 . org-upcoming-deadline)
    (0.0 . default))
  "Faces for showing deadlines in the agenda.
This is a list of cons cells.  The cdr of each cell is a face to be used,
and it can also just be like '(:foreground \"yellow\").
Each car is a fraction of the head-warning time that must have passed for
this the face in the cdr to be used for display.  The numbers must be
given in descending order.  The head-warning time is normally taken
from `org-deadline-warning-days', but can also be specified in the deadline
timestamp itself, like this:

   DEADLINE: <2007-08-13 Mon -8d>

You may use d for days, w for weeks, m for months and y for years.  Months
and years will only be treated in an approximate fashion (30.4 days for a
month and 365.24 days for a year)."
  :group 'org-faces
  :group 'org-agenda-daily/weekly
  :type '(repeat
	  (cons
	   (number :tag "Fraction of head-warning time passed")
	   (sexp :tag "Face"))))

(defface org-agenda-restriction-lock
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:background "yellow1"))
      (((class color) (min-colors 88) (background dark))  (:background "skyblue4"))
      (((class color) (min-colors 16) (background light)) (:background "yellow1"))
      (((class color) (min-colors 16) (background dark))  (:background "skyblue4"))
      (((class color) (min-colors 8)) (:background "cyan" :foreground "black"))
      (t (:inverse-video t))))
  "Face for showing the agenda restriction lock."
  :group 'org-faces)

(defface org-time-grid ;; originally copied from font-lock-variable-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)) (:foreground "yellow" :weight light))))
  "Face used for time grids."
  :group 'org-faces)

(defconst org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8
    ))

(defcustom org-n-level-faces (length org-level-faces)
  "The number of different faces to be used for headlines.
Org-mode defines 8 different headline faces, so this can be at most 8.
If it is less than 8, the level-1 face gets re-used for level N+1 etc."
  :type 'number
  :group 'org-faces)

(defface org-latex-and-export-specials
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math latex and other special exporter stuff."
  :group 'org-faces)

(provide 'org-faces)

;; arch-tag: 9dab5f91-c4b9-4d6f-bac3-1f6211ad0a04
;;; org-faces.el ends here
