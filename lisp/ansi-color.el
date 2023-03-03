;;; ansi-color.el --- translate ANSI escape sequences into faces -*- lexical-binding: t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Version: 3.4.2
;; Keywords: comm processes terminals services

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

;; This file provides a function that takes a string or a region
;; containing Select Graphic Rendition (SGR) control sequences (formerly
;; known as ANSI escape sequences) and tries to translate these into
;; faces.
;;
;; This allows you to run ls --color=yes in shell-mode.  It is now
;; enabled by default; to disable it, set ansi-color-for-comint-mode
;; to nil.
;;
;; Note that starting your shell from within Emacs might set the TERM
;; environment variable.  The new setting might disable the output of
;; SGR control sequences.  Using ls --color=yes forces ls to produce
;; these.
;;
;; SGR control sequences are defined in section 3.8.117 of the ECMA-48
;; standard (identical to ISO/IEC 6429), which is freely available as a
;; PDF file <URL:https://www.ecma-international.org/publications/standards/Ecma-048.htm>.
;; The "Graphic Rendition Combination Mode (GRCM)" implemented is
;; "cumulative mode" as defined in section 7.2.8.  Cumulative mode
;; means that whenever possible, SGR control sequences are combined
;; (i.e. blue and bold).

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

;;; Thanks

;; Georges Brun-Cottan <gbruncot@emc.com> for improving ansi-color.el
;; substantially by adding the code needed to cope with arbitrary chunks
;; of output and the filter functions.
;;
;; Markus Kuhn <Markus.Kuhn@cl.cam.ac.uk> for pointing me to ECMA-48.
;;
;; Stefan Monnier <foo@acm.com> for explaining obscure font-lock stuff and for
;; code suggestions.



;;; Code:

(defvar comint-last-output-start)
(defvar compilation-filter-start)

;; Customization

(defgroup ansi-colors nil
  "Translating SGR control sequences to faces.
This translation effectively colorizes strings and regions based upon
SGR control sequences embedded in the text.  SGR (Select Graphic
Rendition) control sequences are defined in section 8.3.117 of the
ECMA-48 standard (identical to ISO/IEC 6429), which is freely available
at <URL:https://www.ecma-international.org/publications/standards/Ecma-048.htm>
as a PDF file."
  :version "21.1"
  :group 'processes)

(defface ansi-color-bold
  '((t :inherit bold))
  "Face used to render bold text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-faint
  '((t :weight light))
  "Face used to render faint text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-italic
  '((t :inherit italic))
  "Face used to render italic text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-underline
  '((t :inherit underline))
  "Face used to render underlined text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-slow-blink
  '((t :box (:line-width -1)))
  "Face used to render slowly blinking text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-fast-blink
  '((t :box (:line-width -1)))
  "Face used to render rapidly blinking text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-inverse
  '((t :inverse-video t))
  "Face used to render inverted video text."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-black
  '((t :foreground "black" :background "black"))
  "Face used to render black color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-red
  '((t :foreground "red3" :background "red3"))
  "Face used to render red color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-green
  '((t :foreground "green3" :background "green3"))
  "Face used to render green color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face used to render yellow color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face used to render blue color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face used to render magenta color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face used to render cyan color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-white
  '((t :foreground "grey90" :background "gray90"))
  "Face used to render white color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-black
  '((t :foreground "gray30" :background "gray30"))
  "Face used to render bright black color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-red
  '((t :foreground "red2" :background "red2"))
  "Face used to render bright red color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-green
  '((t :foreground "green2" :background "green2"))
  "Face used to render bright green color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-yellow
  '((t :foreground "yellow2" :background "yellow2"))
  "Face used to render bright yellow color code."
  :group 'ansi-colors)

(defface ansi-color-bright-blue
  '((t :foreground "blue1" :background "blue1"))
  "Face used to render bright blue color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-magenta
  '((t :foreground "magenta2" :background "magenta2"))
  "Face used to render bright magenta color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-cyan
  '((t :foreground "cyan2" :background "cyan2"))
  "Face used to render bright cyan color code."
  :group 'ansi-colors
  :version "28.1")

(defface ansi-color-bright-white
  '((t :foreground "white" :background "white"))
  "Face used to render bright white color code."
  :group 'ansi-colors
  :version "28.1")

(defcustom ansi-color-faces-vector
  [default bold default italic underline success warning error]
  "Faces used for SGR control sequences determining a face.
This vector holds the faces used for SGR control sequence parameters 0
to 7.

This variable is obsolete.  To customize the display of faces used by
ansi-color, change `ansi-color-FACE', e.g. `ansi-color-bold'.  To
customize the actual faces used (e.g. to temporarily display SGR
control sequences differently), use `ansi-color-basic-faces-vector'."
  :type '(vector face face face face face face face face)
  :group 'ansi-colors)
(make-obsolete-variable 'ansi-color-faces-vector 'ansi-color-basic-faces-vector
                        "28.1")

(defcustom ansi-color-names-vector
  ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]
  "Colors used for SGR control sequences determining a color.
This vector holds the colors used for SGR control sequence parameters
30 to 37 (foreground colors) and 40 to 47 (background colors).

This variable is obsolete.  To customize the display of colors used by
ansi-color, change `ansi-color-COLOR', e.g. `ansi-color-red'.  To
customize the actual faces used (e.g. to temporarily display SGR
control sequences differently), use `ansi-color-normal-colors-vector'."
  :type '(vector (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color)))
  :version "24.4" ; default colors copied from `xterm-standard-colors'
  :group 'ansi-colors)
(make-obsolete-variable 'ansi-color-faces-vector
                        'ansi-color-normal-colors-vector "28.1")

(defvar ansi-color-basic-faces-vector
  [nil
   ansi-color-bold
   ansi-color-faint
   ansi-color-italic
   ansi-color-underline
   ansi-color-slow-blink
   ansi-color-fast-blink
   ansi-color-inverse]
  "Faces used for SGR control sequences determining a face.
This vector holds the faces used for SGR control sequence parameters 0
to 7.

Parameter  Description
  0        default
  1        bold
  2        faint
  3        italic
  4        underlined
  5        slowly blinking
  6        rapidly blinking
  7        negative image")

(defvar ansi-color-normal-colors-vector
  [ansi-color-black
   ansi-color-red
   ansi-color-green
   ansi-color-yellow
   ansi-color-blue
   ansi-color-magenta
   ansi-color-cyan
   ansi-color-white]
  "Faces used for SGR control sequences determining a color.
This vector holds the faces used for SGR control sequence parameters
30 to 37 (foreground colors) and 40 to 47 (background colors).

Parameter  Color
  30  40   black
  31  41   red
  32  42   green
  33  43   yellow
  34  44   blue
  35  45   magenta
  36  46   cyan
  37  47   white")

(defvar ansi-color-bright-colors-vector
  [ansi-color-bright-black
   ansi-color-bright-red
   ansi-color-bright-green
   ansi-color-bright-yellow
   ansi-color-bright-blue
   ansi-color-bright-magenta
   ansi-color-bright-cyan
   ansi-color-bright-white]
  "Faces used for SGR control sequences determining a \"bright\" color.
This vector holds the faces used for SGR control sequence parameters
90 to 97 (bright foreground colors) and 100 to 107 (bright background
colors).

Parameter   Color
  90  100   bright black
  91  101   bright red
  92  102   bright green
  93  103   bright yellow
  94  104   bright blue
  95  105   bright magenta
  96  106   bright cyan
  97  107   bright white")

(defcustom ansi-color-bold-is-bright nil
  "If set to non-nil, combining ANSI bold and a color produces the bright
version of that color."
  :type 'boolean
  :version "28.1"
  :group 'ansi-colors)

(defconst ansi-color-control-seq-regexp
  ;; See ECMA 48, section 5.4 "Control Sequences".
  "\e\\[[\x30-\x3F]*[\x20-\x2F]*[\x40-\x7E]"
  "Regexp matching an ANSI control sequence.")

(defconst ansi-color--control-seq-fragment-regexp
  "\e\\[[\x30-\x3F]*[\x20-\x2F]*\\|\e"
  "Regexp matching a partial ANSI control sequence.")

(defconst ansi-color-parameter-regexp "\\([0-9]*\\)[m;]"
  "Regexp that matches SGR control sequence parameters.")

;; Convenience functions for comint modes (eg. shell-mode)


(defcustom ansi-color-for-comint-mode t
  "Determines what to do with comint output.
If nil, do nothing.
If the symbol `filter', then filter all SGR control sequences.
If anything else (such as t), then translate SGR control sequences
into text properties.

In order for this to have any effect, `ansi-color-process-output' must
be in `comint-output-filter-functions'.

This can be used to enable colorized ls --color=yes output
in shell buffers.  You set this variable by calling one of:
\\[ansi-color-for-comint-mode-on]
\\[ansi-color-for-comint-mode-off]
\\[ansi-color-for-comint-mode-filter]"
  :type '(choice (const :tag "Do nothing" nil)
		 (const :tag "Filter" filter)
		 (const :tag "Translate" t))
  :group 'ansi-colors
  :version "23.2")

(defcustom ansi-color-for-compilation-mode t
  "Determines what to do with compilation output.
If nil, do nothing.

If the symbol `filter', then filter all ANSI graphical control
sequences.

If anything else (such as t), then translate ANSI graphical
control sequences into text properties.

In order for this to have any effect, `ansi-color-compilation-filter'
must be in `compilation-filter-hook'."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Filter" filter)
                 (other :tag "Translate" t))
  :group 'ansi-colors
  :version "28.1")

(defvar ansi-color-apply-face-function #'ansi-color-apply-overlay-face
  "Function for applying an Ansi Color face to text in a buffer.
This function should accept three arguments: BEG, END, and FACE,
and it should apply face FACE to the text between BEG and END.")

;;;###autoload
(defun ansi-color-for-comint-mode-on ()
  "Set `ansi-color-for-comint-mode' to t."
  (interactive)
  (setq ansi-color-for-comint-mode t))

(defun ansi-color-for-comint-mode-off ()
  "Set `ansi-color-for-comint-mode' to nil."
  (interactive)
  (setq ansi-color-for-comint-mode nil))

(defun ansi-color-for-comint-mode-filter ()
  "Set `ansi-color-for-comint-mode' to symbol `filter'."
  (interactive)
  (setq ansi-color-for-comint-mode 'filter))

;;;###autoload
(defun ansi-color-process-output (_ignored)
  "Maybe translate SGR control sequences of comint output into text properties.

Depending on variable `ansi-color-for-comint-mode' the comint output is
either not processed, SGR control sequences are filtered using
`ansi-color-filter-region', or SGR control sequences are translated into
text properties using `ansi-color-apply-on-region'.

The comint output is assumed to lie between the marker
`comint-last-output-start' and the process-mark.

This is a good function to put in `comint-output-filter-functions'."
  (let ((start-marker (if (and (markerp comint-last-output-start)
			       (eq (marker-buffer comint-last-output-start)
				   (current-buffer))
			       (marker-position comint-last-output-start))
			  comint-last-output-start
			(point-min-marker)))
	(end-marker (process-mark (get-buffer-process (current-buffer)))))
    (cond ((eq ansi-color-for-comint-mode nil))
	  ((eq ansi-color-for-comint-mode 'filter)
	   (ansi-color-filter-region start-marker end-marker))
	  (t
	   (ansi-color-apply-on-region start-marker end-marker)))))

;;;###autoload
(defun ansi-color-compilation-filter ()
  "Maybe translate SGR control sequences into text properties.
This function depends on the `ansi-color-for-compilation-mode'
variable, and is meant to be used in `compilation-filter-hook'."
  (let ((inhibit-read-only t))
    (pcase ansi-color-for-compilation-mode
      ('nil nil)
      ('filter
       (ansi-color-filter-region compilation-filter-start (point)))
      (_
       (ansi-color-apply-on-region compilation-filter-start (point))))))

;; Working with strings
(defvar-local ansi-color-context nil
  "Context saved between two calls to `ansi-color-apply'.
This is a list of the form (FACE-VEC FRAGMENT) or nil.  FACE-VEC
represents the state the last call to `ansi-color-apply' ended
with, currently a list of the form:

  (BASIC-FACES FG BG)

BASIC-FACES is a bool-vector that specifies which basic faces
from `ansi-color-basic-faces-vector' to apply.  FG and BG are
ANSI color codes for the foreground and background color.

FRAGMENT is a string starting with an escape sequence, possibly
the start of a new escape sequence.")

(defun ansi-color-filter-apply (string)
  "Filter out all ANSI control sequences from STRING.

Every call to this function will set and use the buffer-local variable
`ansi-color-context' to save partial escape sequences.  This information
will be used for the next call to `ansi-color-apply'.  Set
`ansi-color-context' to nil if you don't want this.

This function can be added to `comint-preoutput-filter-functions'."
  (let ((context (ansi-color--ensure-context 'ansi-color-context nil))
        (start 0) end result)
    ;; if context was saved and is a string, prepend it
    (setq string (concat (cadr context) string))
    (setcar (cdr context) "")
    ;; find the next escape sequence
    (while (setq end (string-match ansi-color-control-seq-regexp string start))
      (push (substring string start end) result)
      (setq start (match-end 0)))
    ;; save context, add the remainder of the string to the result
    (let ((fragment ""))
      (push (substring string start
                       (if (string-match
                            (concat "\\(?:"
                                    ansi-color--control-seq-fragment-regexp
                                    "\\)\\'")
                            string start)
                           (let ((pos (match-beginning 0)))
                             (setq fragment (substring string pos))
                             pos)
                         nil))
            result)
      (setcar (cdr context) fragment))
    (apply #'concat (nreverse result))))

(defun ansi-color-apply (string)
  "Translates SGR control sequences into text properties.
Delete all other control sequences without processing them.

Applies SGR control sequences setting foreground and background colors
to STRING using text properties and returns the result.  See function
`ansi-color-apply-sequence' for details.

Every call to this function will set and use the buffer-local variable
`ansi-color-context' to save partial escape sequences and current ansi codes.
This information will be used for the next call to `ansi-color-apply'.
Set `ansi-color-context' to nil if you don't want this.

This function can be added to `comint-preoutput-filter-functions'."
  (let* ((context
          (ansi-color--ensure-context 'ansi-color-context nil))
         (face-vec (car context))
         (start 0)
         end result)
    ;; If context was saved and is a string, prepend it.
    (setq string (concat (cadr context) string))
    (setcar (cdr context) "")
    ;; Find the next escape sequence.
    (while (setq end (string-match ansi-color-control-seq-regexp string start))
      (let ((esc-end (match-end 0)))
        ;; Colorize the old block from start to end using old face.
        (when-let ((face (ansi-color--face-vec-face face-vec)))
          (put-text-property start end 'font-lock-face
                             face string))
        (push (substring string start end) result)
        (setq start (match-end 0))
        ;; If this is a color escape sequence,
        (when (eq (aref string (1- esc-end)) ?m)
          ;; create a new face from it.
          (let ((cur-pos end))
            (ansi-color--update-face-vec
             face-vec
             (lambda ()
               (when (string-match ansi-color-parameter-regexp
                                   string cur-pos)
                 (setq cur-pos (match-end 0))
                 (when (<= cur-pos esc-end)
                   (string-to-number (match-string 1 string))))))))))
    ;; if the rest of the string should have a face, put it there
    (when-let ((face (ansi-color--face-vec-face face-vec)))
      (put-text-property start (length string)
                         'font-lock-face face string))
    ;; save context, add the remainder of the string to the result
    (if (string-match
         (concat "\\(?:" ansi-color--control-seq-fragment-regexp "\\)\\'")
         string start)
        (let ((pos (match-beginning 0)))
          (setcar (cdr context) (substring string pos))
          (push (substring string start pos) result))
      (push (substring string start) result))
    (apply 'concat (nreverse result))))

(defun ansi-color--ensure-context (context-sym position)
  "Return CONTEXT-SYM's value as a valid context.
If it is nil, set CONTEXT-SYM's value to a new context and return
it. Context is a list of the form as described in
`ansi-color-context' if POSITION is nil, or
`ansi-color-context-region' if POSITION is non-nil.

If CONTEXT-SYM's value is already non-nil, return it. If its
marker doesn't point anywhere yet, position it before character
number POSITION, if non-nil."
  (let ((context (symbol-value context-sym)))
    (if context
        (if position
            (let ((marker (cadr context)))
              (unless (marker-position marker)
                (set-marker marker position))
              context)
          context)
      (set context-sym
           (list (list (make-bool-vector 8 nil)
                       nil nil)
                 (if position
                     (copy-marker position)
                   ""))))))

(defun ansi-color--face-vec-face (face-vec)
  "Return the face corresponding to FACE-VEC.
FACE-VEC is a list containing information about the ANSI sequence
code.  It is usually stored as the car of the variable
`ansi-color-context-region'."
  (let* ((basic-faces (car face-vec))
         (colors (cdr face-vec))
         (bright (and ansi-color-bold-is-bright (aref basic-faces 1)))
         (faces nil))

    (when-let ((fg (car colors)))
      (push
       `(:foreground
         ,(or (ansi-color--code-as-hex fg)
              (face-foreground
               (aref (if (or bright (>= fg 8))
                         ansi-color-bright-colors-vector
                       ansi-color-normal-colors-vector)
                     (mod fg 8))
               nil 'default)))
       faces))
    (when-let ((bg (cadr colors)))
      (push
       `(:background
         ,(or (ansi-color--code-as-hex bg)
              (face-background
               (aref (if (or bright (>= bg 8))
                         ansi-color-bright-colors-vector
                       ansi-color-normal-colors-vector)
                     (mod bg 8))
               nil 'default)))
       faces))

    (let ((i 8))
      (while (> i 0)
        (setq i (1- i))
        (when (aref basic-faces i)
          (push (aref ansi-color-basic-faces-vector i) faces))))
    ;; Avoid some long-lived conses in the common case.
    (if (cdr faces)
        faces
      (car faces))))

(defun ansi-color--code-as-hex (color)
  "Convert COLOR to hexadecimal string representation.
COLOR is an ANSI color code.  If it is between 16 and 255
inclusive, it corresponds to a color from an 8-bit color cube.
If it is greater or equal than 256, it is subtracted by 256 to
directly specify a 24-bit color.

Return a hexadecimal string, specifying the color, or nil, if
COLOR is less than 16."
  (cond
   ((< color 16) nil)
   ((>= color 256) (format "#%06X" (- color 256)))
   ((>= color 232) ;; Grayscale
    (format "#%06X" (* #x010101 (+ 8 (* 10 (- color 232))))))
   (t ;; 6x6x6 color cube
    (setq color (- color 16))
    (let ((res 0)
          (frac (* 6 6)))
      (while (<= 1 frac)                ; Repeat 3 times
        (setq res (* res #x000100))
        (let ((color-num (mod (/ color frac) 6)))
          (unless (zerop color-num)
            (setq res (+ res #x37 (* #x28 color-num)))))
        (setq frac (/ frac 6)))
      (format "#%06X" res)))))

;; Working with regions

(defvar-local ansi-color-context-region nil
  "Context saved between two calls to `ansi-color-apply-on-region'.
This is a list of the form (FACE-VEC MARKER) or nil.  FACE-VEC
represents the state the last call to `ansi-color-apply-on-region'
ended with, currently a list of the form:

  (BASIC-FACES FG BG).

BASIC-FACES is a bool-vector that specifies which basic faces
from `ansi-color-basic-faces-vector' to apply.  FG and BG are
ANSI color codes for the foreground and background color.

MARKER is a buffer position within an escape sequence or the last
position processed.")

(defun ansi-color-filter-region (begin end)
  "Filter out all ANSI control sequences from region BEGIN to END.

Every call to this function will set and use the buffer-local variable
`ansi-color-context-region' to save position.  This information will be
used for the next call to `ansi-color-apply-on-region'.  Specifically,
it will override BEGIN, the start of the region.  Set
`ansi-color-context-region' to nil if you don't want this."
  (let* ((end-marker (copy-marker end))
         (context (ansi-color--ensure-context
                   'ansi-color-context-region begin))
         (start (cadr context)))
    (save-excursion
      (goto-char start)
      ;; Delete escape sequences.
      (while (re-search-forward ansi-color-control-seq-regexp end-marker t)
        (delete-region (match-beginning 0) (match-end 0)))
      ;; save context, add the remainder of the string to the result
      (set-marker start (point))
      (while (re-search-forward ansi-color--control-seq-fragment-regexp
                                end-marker t))
      (if (and (/= (point) start)
               (= (point) end-marker))
	  (set-marker start (match-beginning 0))
        (set-marker start nil)))))

(defun ansi-color-apply-on-region (begin end &optional preserve-sequences)
  "Translates SGR control sequences into overlays or extents.
Delete all other control sequences without processing them.

SGR control sequences are applied by calling the function
specified by `ansi-color-apply-face-function'.  The default
function sets foreground and background colors to the text
between BEGIN and END, using overlays.  See function
`ansi-color-apply-sequence' for details.

Every call to this function will set and use the buffer-local
variable `ansi-color-context-region' to save position and current
ansi codes.  This information will be used for the next call to
`ansi-color-apply-on-region'.  Specifically, it will override
BEGIN, the start of the region and set the face with which to
start.  Set `ansi-color-context-region' to nil if you don't want
this.

If PRESERVE-SEQUENCES is t, the sequences are hidden instead of
being deleted."
  (let* ((context (ansi-color--ensure-context
                   'ansi-color-context-region begin))
         (face-vec (car context))
         (start-marker (cadr context))
         (end-marker (copy-marker end)))
    (save-excursion
      (goto-char start-marker)
      ;; Find the next escape sequence.
      (while (re-search-forward ansi-color-control-seq-regexp end-marker t)
        ;; Extract escape sequence.
        (let ((esc-beg (match-beginning 0))
              (esc-end (point)))
          ;; Colorize the old block from start to end using old face.
          (funcall ansi-color-apply-face-function
                   (prog1 (marker-position start-marker)
                     ;; Store new start position.
                     (set-marker start-marker esc-end))
                   esc-beg (ansi-color--face-vec-face face-vec))
          ;; If this is a color sequence,
          (when (eq (char-before esc-end) ?m)
            (goto-char esc-beg)
            (ansi-color--update-face-vec
             face-vec (lambda ()
                        (when (re-search-forward ansi-color-parameter-regexp
                                                 esc-end t)
                          (string-to-number (match-string 1))))))

          (if preserve-sequences
              ;; Make the escape sequence transparent.
              (overlay-put (make-overlay esc-beg esc-end) 'invisible t)
            ;; Otherwise, strip.
            (delete-region esc-beg esc-end))))
      ;; search for the possible start of a new escape sequence
      (while (re-search-forward ansi-color--control-seq-fragment-regexp
                                end-marker t))
      (if (and (/= (point) start-marker)
               (= (point) end-marker))
          (progn
            (goto-char (match-beginning 0))
            (funcall ansi-color-apply-face-function
                     start-marker (point)
                     (ansi-color--face-vec-face face-vec))
            (set-marker start-marker (point)))
        (let ((faces (ansi-color--face-vec-face face-vec)))
          (funcall ansi-color-apply-face-function
                   start-marker end-marker faces)
          ;; Save a restart position when there are codes active. It's
          ;; convenient for man.el's process filter to pass `begin'
          ;; positions that overlap regions previously colored; these
          ;; `codes' should not be applied to that overlap, so we need
          ;; to know where they should really start.
          (set-marker start-marker (when faces end-marker)))))
    ;; Clean up our temporary marker.
    (set-marker end-marker nil)))

(defun ansi-color-apply-overlay-face (beg end face)
  "Make an overlay from BEG to END, and apply face FACE.
If FACE is nil, do nothing."
  (when face
    (overlay-put (ansi-color-make-extent beg end) 'face face)))

(defun ansi-color-apply-text-property-face (beg end face)
  "Set the `font-lock-face' property to FACE in region BEG..END.
If FACE is nil, do nothing."
  (when face
    (put-text-property beg end 'font-lock-face face)))

;; This function helps you look for overlapping overlays.  This is
;; useful in comint-buffers.  Overlapping overlays should not happen!
;; A possible cause for bugs are the markers.  If you create an overlay
;; up to the end of the region, then that end might coincide with the
;; process-mark.  As text is added BEFORE the process-mark, the overlay
;; will keep growing.  Therefore, as more overlays are created later on,
;; there will be TWO OR MORE overlays covering the buffer at that point.
;; This function helps you check your buffer for these situations.
; (defun ansi-color-debug-overlays ()
;   (interactive)
;   (let ((pos (point-min)))
;     (while (< pos (point-max))
;       (if (<= 2 (length (overlays-at pos)))
; 	  (progn
; 	    (goto-char pos)
; 	    (error "%d overlays at %d" (length (overlays-at pos)) pos))
; 	(let (message-log-max)
; 	  (message  "Reached %d." pos)))
;       (setq pos (next-overlay-change pos)))))

(defun ansi-color-make-face (property color)
  "Return a face with PROPERTY set to COLOR.
PROPERTY can be either symbol `foreground' or symbol `background'.

For Emacs, we just return the cons cell (PROPERTY . COLOR)."
  (cond ((eq property 'foreground)
	 (cons 'foreground-color color))
	((eq property 'background)
	 (cons 'background-color color))
	(t
	 (cons property color))))

(defun ansi-color-make-extent (from to &optional buffer)
  "Make an extent for the range [FROM, TO) in BUFFER.

BUFFER defaults to the current buffer."
  ;; The overlay might end at the process-mark in comint
  ;; buffers.  In that case, new text will be inserted before the
  ;; process-mark, ie. inside the overlay (using insert-before-marks).
  ;; In order to avoid this, we use the `insert-behind-hooks' overlay
  ;; property to make sure it works.
  (let ((overlay (make-overlay from to buffer)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(ansi-color-freeze-overlay))
    (overlay-put overlay 'insert-behind-hooks '(ansi-color-freeze-overlay))
    overlay))

(defun ansi-color-freeze-overlay (overlay is-after begin end &optional len)
  "Prevent OVERLAY from being extended.
This function can be used for the `modification-hooks' overlay
property."
  ;; if stuff was inserted at the end of the overlay
  (when (and is-after
	     (= 0 len)
	     (= end (overlay-end overlay)))
    ;; reset the end of the overlay
    (move-overlay overlay (overlay-start overlay) begin)))

(defun ansi-color-set-extent-face (extent face)
  "Set the `face' property of EXTENT to FACE."
  (declare (obsolete overlay-put "27.1"))
  (overlay-put extent 'face face))

;; Helper functions

(defsubst ansi-color-parse-sequence (escape-seq)
  "Return the list of all the parameters in ESCAPE-SEQ.

ESCAPE-SEQ is a SGR control sequences such as \\033[34m.  The parameter
34 is used by `ansi-color-get-face-1' to return a face definition.

Returns nil only if there's no match for `ansi-color-parameter-regexp'."
  (let ((i 0)
	codes val)
    (while (string-match ansi-color-parameter-regexp escape-seq i)
      (setq i (match-end 0)
	    val (string-to-number (match-string 1 escape-seq) 10))
      ;; It so happens that (string-to-number "") => 0.
      (push val codes))
    (nreverse codes)))

(defun ansi-color-apply-sequence (escape-sequence codes)
  "Apply ESCAPE-SEQUENCE to CODES and return the new list of codes.

ESCAPE-SEQUENCE is an escape sequence parsed by
`ansi-color-parse-sequence'.

For each new code, the following happens: if it is 1-7, add it to
the list of codes; if it is 21-25 or 27, delete appropriate
parameters from the list of codes; if it is 30-37 (or 90-97) resp. 39,
the foreground color code is replaced or added resp. deleted; if it
is 40-47 (or 100-107) resp. 49, the background color code is replaced
or added resp. deleted; any other code is discarded together with the
old codes.  Finally, the so changed list of codes is returned."
  (declare (obsolete ansi-color--update-face-vec "29.1"))
  (let ((new-codes (ansi-color-parse-sequence escape-sequence)))
    (while new-codes
      (let* ((new (pop new-codes))
	     (q (/ new 10)))
	(setq codes
	      (pcase q
		(0 (unless (memq new '(0 8 9))
		     (cons new (remq new codes))))
		(2 (unless (memq new '(20 26 28 29))
		     ;; The standard says `21 doubly underlined' while
                     ;; https://en.wikipedia.org/wiki/ANSI_escape_code claims
		     ;; `21 Bright/Bold: off or Underline: Double'.
		     (remq (- new 20) (pcase new
					(22 (remq 1 codes))
					(25 (remq 6 codes))
					(_ codes)))))
		((or 3 4 9 10) (let ((r (mod new 10)))
			    (unless (= r 8)
			      (let (beg)
				(while (and codes (/= q (/ (car codes) 10)))
				  (push (pop codes) beg))
				(setq codes (nconc (nreverse beg) (cdr codes)))
				(if (= r 9)
				    codes
				  (cons new codes))))))
		(_ nil)))))
    codes))

(defun ansi-color--update-face-vec (face-vec iterator)
  "Apply escape sequences to FACE-VEC.

Destructively modify FACE-VEC, which should be a list containing
face information.  It is described in
`ansi-color-context-region'.  ITERATOR is a function which is
called repeatedly with zero arguments and should return either
the next ANSI code in the current sequence as a number or nil if
there are no more ANSI codes left.

For each new code, the following happens: if it is 1-7, set the
corresponding properties; if it is 21-25 or 27, unset appropriate
properties; if it is 30-37 (or 90-97) or resp. 39, set the
foreground color or resp. unset it; if it is 40-47 (or 100-107)
resp. 49, set the background color or resp. unset it; if it is 38
or 48, the following codes are used to set the foreground or
background color and the correct color mode; any other code will
unset all properties and colors."
  (let ((basic-faces (car face-vec))
        (colors (cdr face-vec))
        new q do-clear)
    (while (setq new (funcall iterator))
      (setq q (/ new 10))
      (pcase q
        (0 (if (memq new '(0 8 9))
               (setq do-clear t)
             (aset basic-faces new t)))
        (2 (if (memq new '(20 26 28 29))
               (setq do-clear t)
             ;; The standard says `21 doubly underlined' while
             ;; https://en.wikipedia.org/wiki/ANSI_escape_code claims
             ;; `21 Bright/Bold: off or Underline: Double'.
             (aset basic-faces (- new 20) nil)
             (aset basic-faces (pcase new (22 1) (25 6) (_ 0)) nil)))
        ((or 3 4 9 10)
         (let ((r (mod new 10))
               (cell (if (memq q '(3 9)) colors (cdr colors))))
           (pcase r
             (8
              (pcase (funcall iterator)
                (5 (setq new (setcar cell (funcall iterator)))
                   (setq do-clear (or (null new) (>= new 256))))
                (2
                 (let ((red (funcall iterator))
                       (green (funcall iterator))
                       (blue (funcall iterator)))
                   (if (and red green blue
                            (progn
                              (setq new (+ (* #x010000 red)
                                           (* #x000100 green)
                                           (* #x000001 blue)))
                              (<= new #xFFFFFF)))
                       (setcar cell (+ 256 new))
                     (setq do-clear t))))
                (_ (setq do-clear t))))
             (9 (setcar cell nil))
             (_ (setcar cell (+ (if (memq q '(3 4)) 0 8) r))))))
        (_ (setq do-clear t)))

      (when do-clear
        (setq do-clear nil)
        ;; Zero out our bool vector without any allocation.
        (bool-vector-intersection basic-faces #&8"\0" basic-faces)
        (setcar colors nil)
        (setcar (cdr colors) nil)))))

(defun ansi-color-make-color-map ()
  "Create a vector of face definitions and return it.

The index into the vector is an ANSI code.  See the documentation of
`ansi-color-map' for an example.

The face definitions are based upon the variables
`ansi-color-faces-vector' and `ansi-color-names-vector'.

This function is obsolete, and no longer needed to use ansi-color."
  (let ((map (make-vector 50 nil))
        (index 0))
    ;; miscellaneous attributes
    (mapc
     (lambda (e)
       (aset map index e)
       (setq index (1+ index)) )
     ansi-color-faces-vector)
    ;; foreground attributes
    (setq index 30)
    (mapc
     (lambda (e)
       (aset map index
             (ansi-color-make-face 'foreground
                         (if (consp e) (car e) e)))
       (setq index (1+ index)) )
     ansi-color-names-vector)
    ;; background attributes
    (setq index 40)
    (mapc
     (lambda (e)
       (aset map index
             (ansi-color-make-face 'background
                         (if (consp e) (cdr e) e)))
       (setq index (1+ index)) )
     ansi-color-names-vector)
    map))
(make-obsolete 'ansi-color-make-color-map "you can remove it." "28.1")

(defvar ansi-color-map
  (with-no-warnings (ansi-color-make-color-map))
  "A brand new color map, formerly suitable for `ansi-color-get-face'.

The value of this variable is usually constructed by
`ansi-color-make-color-map'.  The values in the array are such that the
numbers included in an SGR control sequences point to the correct
foreground or background colors.

This variable is obsolete, and no longer needed to use ansi-color.")
(make-obsolete-variable 'ansi-color-map "you can remove it." "28.1")

(defun ansi-color-map-update (symbol value)
  "Update `ansi-color-map'.

This function is obsolete, and no longer needed to use ansi-color."
  (set-default symbol value)
  (with-no-warnings
    (setq ansi-color-map (ansi-color-make-color-map))))
(make-obsolete 'ansi-color-map-update "you can remove it." "28.1")

(defun ansi-color-get-face-1 (ansi-code &optional bright)
  "Get face definition for ANSI-CODE.
BRIGHT, if non-nil, requests \"bright\" ANSI colors, even if ANSI-CODE
is a normal-intensity color."
  (declare (obsolete ansi-color--face-vec-face "29.1"))
  (when (and bright (<= 30 ansi-code 49))
    (setq ansi-code (+ ansi-code 60)))
  (cond ((<= 0 ansi-code 7)
         (aref ansi-color-basic-faces-vector ansi-code))
        ((<= 30 ansi-code 38)
         (list :foreground
               (face-foreground
                (aref ansi-color-normal-colors-vector (- ansi-code 30))
                nil 'default)))
        ((<= 40 ansi-code 48)
         (list :background
               (face-background
                (aref ansi-color-normal-colors-vector (- ansi-code 40))
                nil 'default)))
        ((<= 90 ansi-code 98)
         (list :foreground
               (face-foreground
                (aref ansi-color-bright-colors-vector (- ansi-code 90))
                nil 'default)))
        ((<= 100 ansi-code 108)
         (list :background
               (face-background
                (aref ansi-color-bright-colors-vector (- ansi-code 100))
                nil 'default)))))

(provide 'ansi-color)

;;; ansi-color.el ends here
