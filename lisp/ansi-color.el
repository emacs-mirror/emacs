;;; ansi-color.el --- translate ANSI into text-properties

;; Copyright (C) 1999, 2000  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.4.0
;; Keywords: comm processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a function that takes a string containing Select
;; Graphic Rendition (SGR) control sequences (formerly known as ANSI
;; escape sequences) and tries to replace these with text-properties.
;;
;; This allows you to run ls --color=yes in shell-mode: If
;; `ansi-color-for-shell-mode' is non-nil, the SGR control sequences are
;; translated into text-properties, colorizing the ls output.  If
;; `ansi-color-for-shell-mode' is nil, the SGR control sequences are
;; stripped, making the ls output legible.
;;
;; SGR control sequences are defined in section 3.8.117 of the ECMA-48
;; standard (identical to ISO/IEC 6429), which is freely available as a
;; PDF file <URL:http://www.ecma.ch/ecma1/STAND/ECMA-048.HTM>.  The
;; "Graphic Rendition Combination Mode (GRCM)" implemented is
;; "cumulative mode" as defined in section 7.2.8.  Cumulative mode means
;; that whenever possible, SGR control sequences are combined (ie. blue
;; and bold).

;; The basic functions are:
;;
;; `ansi-color-apply' to colorize a string containing SGR control
;; sequences.
;;
;; `ansi-color-filter-apply' to filter SGR control sequences from a
;; string.
;;
;; `ansi-color-apply-on-region' to colorize a region containing SGR
;; control sequences.
;;
;; `ansi-color-filter-region' to filter SGR control sequences from a
;; region.

;; Instead of defining lots of new faces, this package uses
;; text-properties as described in the elisp manual
;; *Note (elisp)Special Properties::.

;;; Thanks

;; Georges Brun-Cottan <gbruncot@emc.com> for improving ansi-color.el
;; substantially by adding the code needed to cope with arbitrary chunks
;; of output and the filter functions.
;;
;; Markus Kuhn <Markus.Kuhn@cl.cam.ac.uk> for pointing me to ECMA-48.



;;; Code:

;; Customization

(defgroup ansi-colors nil
  "Translating SGR control sequences to text-properties.
This translation effectively colorizes strings and regions based upon
SGR control sequences embedded in the text.  SGR (Select Graphic
Rendition) control sequences are defined in section 3.8.117 of the
ECMA-48 standard \(identical to ISO/IEC 6429), which is freely available
as a PDF file <URL:http://www.ecma.ch/ecma1/STAND/ECMA-048.HTM>."
  :version "20.7"
  :group 'processes)

(defcustom ansi-color-faces-vector
  [default bold default italic underline bold bold-italic modeline]
  "Faces used for SGR control sequences determining a face.
This vector holds the faces used for SGR control sequence parameters 0
to 7.

Parameter  Description        Face used by default
  0        default            default
  1        bold               bold
  2        faint              default
  3        italic             italic
  4        underlined         underline
  5        slowly blinking    bold
  6        rapidly blinking   bold-italic
  7        negative image     modeline

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'."
  :type '(vector face face face face face face face face)
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defcustom ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Colors used for SGR control sequences determining a color.
This vector holds the colors used for SGR control sequences parameters
30 to 37 \(foreground colors) and 40 to 47 (background colors).

Parameter  Color
  30  40   black
  31  41   red
  32  42   green
  33  43   yellow
  34  44   blue
  35  45   magenta
  36  46   cyan
  37  47   white

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'."
  :type '(vector string string string string string string string string)
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defcustom ansi-color-for-shell-mode nil
  "Determine wether font-lock or ansi-color get to fontify shell buffers.

If non-nil and `global-font-lock-mode' is non-nil, ansi-color will be
used.  This adds `ansi-color-apply' to
`comint-preoutput-filter-functions' and removes
`ansi-color-filter-apply' for all shell-mode buffers.

If non-nil and global-font-lock-mode is nil, both `ansi-color-apply' and
`ansi-color-filter-apply' will be removed from
`comint-preoutput-filter-functions' for all shell-mode buffers.

If nil, font-lock will be used (if it is enabled).  This adds
`ansi-color-filter-apply' to `comint-preoutput-filter-functions' and
removes `ansi-color-apply' for all shell-mode buffers."
  :version "20.8"
  :type 'boolean
  :set (function (lambda (symbol value)
		   (set-default symbol value)
		   (save-excursion
		     (let ((buffers (buffer-list))
			   buffer)
		       (while buffers
			 (setq buffer (car buffers)
			       buffers (cdr buffers))
			 (set-buffer buffer)
			 (when (eq major-mode 'shell-mode)
			   (if value
			       (if global-font-lock-mode
				   (progn
				     (font-lock-mode 0)
				     (remove-hook 'comint-preoutput-filter-functions 
						  'ansi-color-filter-apply)
				     (add-hook 'comint-preoutput-filter-functions 
					       'ansi-color-apply))
				 (remove-hook 'comint-preoutput-filter-functions 
					      'ansi-color-filter-apply)
				 (remove-hook 'comint-preoutput-filter-functions 
					      'ansi-color-apply))
			     (if global-font-lock-mode
				 (font-lock-mode 1))
			     (remove-hook 'comint-preoutput-filter-functions 
					  'ansi-color-apply)
			     (add-hook 'comint-preoutput-filter-functions 
				       'ansi-color-filter-apply))))))))
  :initialize 'custom-initialize-reset
  :group 'ansi-colors)

(defconst ansi-color-regexp "\033\\[\\([0-9;]*\\)m"
  "Regexp that matches SGR control sequences.")

(defconst ansi-color-parameter-regexp "\\([0-9]*\\)[m;]"
  "Regexp that matches SGR control sequence parameters.")


;; Main functions


(defun ansi-color-filter-apply (s)
  "Filter out all SGR control sequences from S.

This function can be added to `comint-preoutput-filter-functions'."
  (while (string-match ansi-color-regexp s)
    (setq s (replace-match "" t t s)))
  s)


(defun ansi-color-filter-region (begin end)
  "Filter out all SGR control sequences from region START END.

Returns the first point it is safe to start with.  Used to speedup
further processing.

Design to cope with arbitrary chunk of output such as the ones get by
comint-output-filter-functions, e.g.:

\(defvar last-context nil)
\(make-variable-buffer-local 'last-context)

\(defun filter-out-color-in-buffer (s)
  \(setq last-context
        \(ansi-color-filter-region
         \(if last-context
             last-context
           \(if (marker-position comint-last-output-start)
               \(marker-position comint-last-output-start)
             1))
         \(marker-position (process-mark (get-buffer-process (current-buffer)))) ))
  s)

\(add-hook 'comint-output-filter-functions 'filter-out-color-in-buffer)
"
  (let ((endm (copy-marker end)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward ansi-color-regexp endm t)
        (replace-match ""))
      (if (re-search-forward "\033" endm t)
          (match-beginning 0)
        (marker-position endm)))))


(defun ansi-color-apply (string)
  "Translates SGR control sequences into text-properties.

Applies SGR control sequences setting foreground and background colors
to STRING and returns the result.  The colors used are given in
`ansi-color-faces-vector' and `ansi-color-names-vector'.

This function can be added to `comint-preoutput-filter-functions'."
  (let (face (start 0) end escape-sequence null-sequence result)
    ;; find the next escape sequence
    (while (setq end (string-match ansi-color-regexp string start))
      ;; store escape sequence
      (setq escape-sequence (match-string 1 string)
	    null-sequence (string-equal escape-sequence ""))
      ;; colorize the old block from start to end using old face
      (if face
	  (put-text-property start end 'face face string))
      (setq result (concat result (substring string start end))
	    start (match-end 0))
      ;; create new face by applying all the parameters in the escape sequence
      (if null-sequence
	  (setq face nil)
	(setq face (ansi-color-get-face escape-sequence))))
    (concat result (substring string start))))


(defun ansi-color-apply-on-region (begin end &optional context)
  "Translates SGR control sequences into text-properties.

Applies SGR control sequences setting foreground and background colors
to text in region. The colors used are given in
`ansi-color-faces-vector' and `ansi-color-names-vector'.
Returns a context than can be used to speedup further processing.
Context is a (begin (start . face)) list.

Design to cope with arbitrary chunk of output such as the ones get by
comint-output-filter-functions, e.g.:

\(defvar last-context nil)
\(make-variable-buffer-local 'last-context)

\(defun ansi-output-filter (s)
  \(setq last-context
        \(ansi-color-apply-on-region
         \(if last-context
             \(car last-context)
           \(if (marker-position comint-last-output-start)
               \(marker-position comint-last-output-start)
             1))
         \(process-mark (get-buffer-process (current-buffer)))
         last-context ))
  s)

\(add-hook 'comint-output-filter-functions 'ansi-output-filter)
"
  (let ((endm (copy-marker end))
        (face (if (and context (cdr context))
                  (cdr (cdr context))))
	(face-start (if (and context (cdr context))
                        (car (cdr context))))
        (next-safe-start begin)
        escape-sequence
        null-sequence
        stop )
    (save-excursion
      (goto-char begin)
      ;; find the next escape sequence
      (while (setq stop (re-search-forward ansi-color-regexp endm t))
        ;; store escape sequence
        (setq escape-sequence (match-string 1))
        (setq null-sequence (string-equal (match-string 1) ""))
        (setq next-safe-start (match-beginning 0))
        (if face
            (put-text-property face-start next-safe-start 'face face)) ; colorize
        (replace-match "") ; delete the ANSI sequence
        (if null-sequence
            (setq face nil)
          (setq face-start next-safe-start)
          (setq face (ansi-color-get-face escape-sequence))))
      (setq next-safe-start
            (if (re-search-forward "\033" endm t)
                (match-beginning 0)
              (marker-position endm))))
    (cons next-safe-start
          (if face
              (cons face-start face))) ))

;; Helper functions

(defun ansi-color-make-color-map ()
  "Creates a vector of face definitions and returns it.

The index into the vector is an ANSI code.  See the documentation of
`ansi-color-map' for an example.

The face definitions are based upon the variables
`ansi-color-faces-vector' and `ansi-color-names-vector'."
  (let ((ansi-color-map (make-vector 50 nil))
        (index 0))
    ;; miscellaneous attributes
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index e)
                 (setq index (1+ index)) ))
     ansi-color-faces-vector)

    ;; foreground attributes
    (setq index 30)
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index
                       (cons 'foreground-color e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)

    ;; background attributes
    (setq index 40)
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index
                       (cons 'background-color e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ansi-color-map))

(defvar ansi-color-map (ansi-color-make-color-map)
  "A brand new color map suitable for ansi-color-get-face.

The value of this variable is usually constructed by
`ansi-color-make-color-map'.  The values in the array are such that the
numbers included in an SGR control sequences point to the correct
foreground or background colors.

Example: The sequence \033[34m specifies a blue foreground.  Therefore:
     (aref ansi-color-map 34)
          => \(foreground-color . \"blue\")")

(defun ansi-color-map-update (symbol value)
  "Update `ansi-color-map'.

Whenever the vectors used to construct `ansi-color-map' are changed,
this function is called.  Therefore this function is listed as the :set
property of `ansi-color-faces-vector' and `ansi-color-names-vector'."
  (set-default symbol value)
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun ansi-color-get-face-1 (ansi-code)
  "Get face definition from `ansi-color-map'.
ANSI-CODE is used as an index into the vector."
  (condition-case nil
      (aref ansi-color-map ansi-code)
    ('args-out-of-range nil)))

(defun ansi-color-get-face (escape-seq)
  "Create a new face by applying all the parameters in ESCAPE-SEQ.

ESCAPE-SEQ is a SGR control sequences such as \033[34m.  The parameter
34 is used by `ansi-color-get-face-1' to return a face definition."
  (let ((ansi-color-r "[0-9][0-9]?")
        (i 0)
        f)
    (while (string-match ansi-color-r escape-seq i)
      (setq i (match-end 0))
      (add-to-list 'f
                   (ansi-color-get-face-1
                    (string-to-int (match-string 0 escape-seq) 10))))
    f))

(provide 'ansi-color)

;;; ansi-color.el ends here
