;;; erc-nicks.el -- Nick colors for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: David Leatherman <leathekd@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides the `nicks' module for automatic nickname
;; highlighting.  Add `nicks' to `erc-modules' to get started.
;;
;; Use the command `erc-nicks-refresh' to review changes after
;; adjusting an option, like `erc-nicks-contrast-range'.  To change
;; the color of a nickname in a target buffer, click on it and choose
;; "Edit face" from the completion interface, and then perform your
;; adjustments in the resulting Customize menu.  Non-Customize users
;; on Emacs 28+ can persist changes permanently by clicking on the
;; face's "location" hyperlink and copying the generated code snippet
;; (`defface' or `use-package') to their init.el.  Customize users
;; need only click "Apply and Save", as usual.

;;; History:

;; This module has enjoyed a number of contributors across several
;; variants over the years, including:
;;
;;   Thibault Polge <thibault@thb.lt>
;;   Jay Kamat <jaygkamat@gmail.com>
;;   Alex Kost <alezost@gmail.com>
;;   Antoine Levitt <antoine dot levitt at gmail>
;;   Adam Porter <adam@alphapapa.net>
;;
;; To those not mentioned, your efforts are no less appreciated.

;; 2023/05 - erc-nicks
;;           Rewrite using internal API, and rebrand for ERC 5.6
;; 2020/03 - erc-hl-nicks 1.3.4
;;           Final release, see [1] for intervening history
;; 2014/05 - erc-highlight-nicknames.el
;;           Final release, see [2] for intervening history
;; 2011/08 - erc-hl-nicks 1.0
;;           Initial release forked from erc-highlight-nicknames.el
;; 2008/12 - erc-highlight-nicknames.el
;;           First release from Andy Stewart
;; 2007/09 - erc-highlight-nicknames.el
;;           Initial release by André Riemann

;; [1] <https://www.github.com/leathekd/erc-hl-nicks>
;; [2] <https://www.emacswiki.org/emacs/ErcHighlightNicknames>

;;; Code:

(require 'erc-button)
(require 'color)

(defgroup erc-nicks nil
  "Colorize nicknames in ERC target buffers."
  :package-version '(ERC . "5.6")
  :group 'erc)

(defcustom erc-nicks-ignore-chars ",`'_-"
  "Trailing characters in a nick to ignore while highlighting.
Value should be a string containing characters typically appended
by IRC clients to secure a nickname after a rejection (see option
`erc-nick-uniquifier').  A value of nil means don't trim
anything."
  :type '(choice (string :tag "Chars to trim")
                 (const :tag "Don't trim" nil)))

(defcustom erc-nicks-skip-nicks nil
  "Nicks to avoid highlighting.
ERC only considers this option during module activation, so users
should adjust it before connecting."
  :type '(repeat string))

(defcustom erc-nicks-skip-faces '( erc-notice-face erc-current-nick-face
                                   erc-my-nick-face erc-pal-face erc-fool-face)
  "Faces to avoid highlighting atop."
  :type  (erc--with-dependent-type-match (repeat face) erc-match))

(defcustom erc-nicks-backing-face erc-button-nickname-face
  "Face to mix with generated one for emphasizing non-speakers."
  :type '(choice face (const nil)))

(defcustom erc-nicks-bg-color
  (frame-parameter (selected-frame) 'background-color)
  "Background color for calculating contrast.
Set this explicitly when the background color isn't discoverable,
which may be the case in terminal Emacs.  Even when automatically
initialized, this value may need adjustment mid-session, such as
after loading a new theme.  Remember to run \\[erc-nicks-refresh]
after doing so."
  :type 'string)

(defcustom erc-nicks-color-adjustments
  '(erc-nicks-add-contrast erc-nicks-cap-contrast erc-nicks-ensaturate)
  "Treatments applied to improve aesthetics or visibility.
For example, the function `erc-nicks-invert' inverts a nick when
it's too close to the background, and `erc-nicks-add-contrast'
attempts to find a decent contrast ratio by brightening or
darkening.  When `erc-nicks-colors' is set to the symbol
`defined' or a user-provided list of colors, ERC uses this option
as a guide for culling any colors that don't fall within
`erc-nicks-contrast-range' or `erc-nicks-saturation-range', as
appropriate.  For example, if `erc-nicks-cap-contrast' is present
in this option's value, and a color's contrast exceeds the CDR of
`erc-nicks-contrast-range', ERC will purge that color from its
rolls when initializing this module.  Specify a value of nil to
inhibit this process."
  :type '(repeat
          (choice (function-item :tag "Invert" erc-nicks-invert)
                  (function-item :tag "Add contrast" erc-nicks-add-contrast)
                  (function-item :tag "Cap contrast" erc-nicks-cap-contrast)
                  (function-item :tag "Bound saturation" erc-nicks-ensaturate)
                  function)))

(defcustom erc-nicks-contrast-range '(4.3 . 12.5)
  "Desired range of contrast as a cons of (MIN . MAX).
When `erc-nicks-add-contrast' and/or `erc-nicks-invert' appear in
`erc-nicks-color-adjustments', MIN specifies the minimum amount
of contrast allowed between a buffer's background and its
foreground colors.  Depending on the background, nicks may appear
tinted in pastels or shaded with muted grays.  MAX works
similarly for reducing contrast, but only when
`erc-nicks-cap-contrast' is active.  Users with lighter
backgrounds may want to lower MAX significantly.  Either value
can range from 1.0 to 21.0(:1) but may produce unsatisfactory
results toward either extreme."
  :type '(cons float float))

(defcustom erc-nicks-saturation-range '(0.2 . 0.8)
  "Desired range for constraining saturation.
Expressed as a cons of decimal proportions.  Only matters when
`erc-nicks-ensaturate' appears in `erc-nicks-color-adjustments'."
  :type '(cons float float))

(defcustom erc-nicks-colors 'all
  "Pool of colors.
List of colors as strings (hex or named) or, alternatively, a
single symbol representing a set of colors, like that produced by
the function `defined-colors', which ERC associates with the
symbol `defined'.  Similarly, `all' tells ERC to use any 24-bit
color.  To change the value mid-session, try
\\[erc-nicks-refresh]."
  :type `(choice (const :tag "All 24-bit colors" all)
                 (const :tag "Defined terminal colors" defined)
                 (const :tag "Font Lock faces" font-lock)
                 (const :tag "ANSI color faces" ansi-color)
                 (repeat :tag "User-provided list" string)))

(defcustom erc-nicks-key-suffix-format "@%n"
  "Template for latter portion of keys to generate colors from.
ERC passes this to `format-spec' with the following specifiers:
%n for the current network and %m for your nickname (not the one
being colorized).  If you don't like the generated palette, try
adding extra characters or padding, for example, with something
like \"@%-012n\"."
  :type 'string)

(defcustom erc-nicks-track-faces 'prioritize
  "Show nick faces in the `track' module's portion of the mode line.
A value of nil means don't show nick faces at all.  A value of
`defer' means have `track' consider nick faces only after those
ranked faces in `erc-track-faces-normal-list'.  This has the
effect of \"alternating\" between a ranked \"normal\" and a nick.
The value `prioritize' means have `track' consider nick faces to
be \"normal\" unless the current speaker is the same as the
previous one, in which case pretend the value is `defer'.  Like
most options in this module, updating the value mid-session is
not officially supported, although cycling \\[erc-nicks-mode] may
be worth a shot."
  :type '(choice (const nil) (const defer) (const prioritize)))

(defvar erc-nicks--max-skip-search 3 ; make this an option?
  "Max number of faces to visit when testing `erc-nicks-skip-faces'.")

(defvar erc-nicks--colors-rejects nil)
(defvar erc-nicks--custom-keywords '(:group erc-nicks :group erc-faces))
(defvar erc-nicks--grad-steps 9)

(defvar-local erc-nicks--face-table nil
  "Hash table mapping nicks to unique, named faces.
Keys are nonempty strings but need not be valid nicks.")

(defvar-local erc-nicks--downcased-skip-nicks nil
  "Case-mapped copy of `erc-nicks-skip-nicks'.")

(defvar-local erc-nicks--bg-luminance nil)
(defvar-local erc-nicks--bg-mode-value nil)
(defvar-local erc-nicks--colors-len nil)
(defvar-local erc-nicks--colors-pool nil)
(defvar-local erc-nicks--fg-rgb nil)

(defvar help-xref-stack)
(defvar help-xref-stack-item)
(defvar erc-track--normal-faces)

;; https://stackoverflow.com/questions/596216#answer-56678483
(defun erc-nicks--get-luminance (color)
  "Return relative luminance of COLOR.
COLOR can be a list of normalized values or a name.  This is the
same as the Y component returned by `color-srgb-to-xyz'."
  (let ((out 0)
        (coefficients '(0.2126 0.7152 0.0722))
        (chnls (if (stringp color) (color-name-to-rgb color) color)))
    (dolist (ch chnls out)
      (cl-incf out (* (pop coefficients)
                      (if (<= ch 0.04045)
                          (/ ch 12.92)
                        (expt (/ (+ ch 0.055) 1.055) 2.4)))))))

(defun erc-nicks--get-contrast (fg &optional bg)
  "Return a float between 1 and 21 for colors FG and BG.
If FG or BG are floats, interpret them as luminance values."
  (let* ((lum-fg (if (numberp fg) fg (erc-nicks--get-luminance fg)))
         (lum-bg (if bg
                     (if (numberp bg) bg (erc-nicks--get-luminance bg))
                   (or erc-nicks--bg-luminance
                       (setq erc-nicks--bg-luminance
                             (erc-nicks--get-luminance erc-nicks-bg-color))))))
    (when (< lum-fg lum-bg) (cl-rotatef lum-fg lum-bg))
    (/ (+ 0.05 lum-fg) (+ 0.05 lum-bg))))

(defmacro erc-nicks--bg-mode ()
  `(or erc-nicks--bg-mode-value
       (setq erc-nicks--bg-mode-value
             ,(cond ((fboundp 'frame--current-background-mode)
                     '(frame--current-background-mode (selected-frame)))
                    ((fboundp 'frame--current-backround-mode)
                     '(frame--current-backround-mode (selected-frame)))
                    (t
                     '(frame-parameter (selected-frame) 'background-mode))))))

;; https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html
(defun erc-nicks--adjust-contrast (color target &optional decrease)
  (cl-assert erc-nicks--fg-rgb)
  (let* ((lum-bg (or erc-nicks--bg-luminance
                     (setq erc-nicks--bg-luminance
                           (erc-nicks--get-luminance erc-nicks-bg-color))))
         (stop (if decrease
                   (color-name-to-rgb erc-nicks-bg-color)
                 erc-nicks--fg-rgb))
         ;; From `color-gradient' in color.el
         (r (nth 0 color))
         (g (nth 1 color))
         (b (nth 2 color))
         (interval (float (1+ (expt 2 erc-nicks--grad-steps))))
         (r-step (/ (- (nth 0 stop) r) interval))
         (g-step (/ (- (nth 1 stop) g) interval))
         (b-step (/ (- (nth 2 stop) b) interval))
         (maxtries erc-nicks--grad-steps)
         started)
    ;; FIXME stop when sufficiently close instead of exhausting.
    (while (let* ((lum-fg (erc-nicks--get-luminance (list r g b)))
                  (darker (if (< lum-bg lum-fg) lum-bg lum-fg))
                  (lighter (if (= darker lum-bg) lum-fg lum-bg))
                  (cur (/ (+ 0.05 lighter) (+ 0.05 darker)))
                  (scale (expt 2 maxtries)))
             (cond ((if decrease (> cur target) (< cur target))
                    (setq r (+ r (* r-step scale))
                          g (+ g (* g-step scale))
                          b (+ b (* b-step scale))))
                   (started
                    (setq r (- r (* r-step scale))
                          g (- g (* g-step scale))
                          b (- b (* b-step scale))))
                   (t (setq maxtries 1)))
             (unless started
               (setq started t))
             (setq r (min 1.0 (max 0 r))
                   g (min 1.0 (max 0 g))
                   b (min 1.0 (max 0 b)))
             (not (zerop (cl-decf maxtries)))))
    (list r g b)))

(defun erc-nicks-add-contrast (color)
  "Increase COLOR's contrast by blending it with the foreground.
Unless sufficient contrast exists between COLOR and the
background, raise it to meet the lower bound of
`erc-nicks-contrast-range'."
  (erc-nicks--adjust-contrast color (car erc-nicks-contrast-range)))

(defun erc-nicks-cap-contrast (color)
  "Reduce COLOR's contrast by blending it with the background.
If excessive contrast exists between COLOR and the background,
lower it to the upper bound of `erc-nicks-contrast-range'."
  (erc-nicks--adjust-contrast color (cdr erc-nicks-contrast-range) 'remove))

(defun erc-nicks-invert (color)
  "Invert COLOR based on the CAR of `erc-nicks-contrast-range'.
Don't bother if the inverted color has less contrast than the
input."
  (if-let ((con-input (erc-nicks--get-contrast color))
           ((< con-input (car erc-nicks-contrast-range)))
           (flipped (mapcar (lambda (c) (- 1.0 c)) color))
           ((> (erc-nicks--get-contrast flipped) con-input)))
      flipped
    color))

(defun erc-nicks-ensaturate (color)
  "Ensure COLOR falls within `erc-nicks-saturation-range'."
  (pcase-let ((`(,min . ,max) erc-nicks-saturation-range)
              (`(,h ,s ,l) (apply #'color-rgb-to-hsl color)))
    (cond ((> s max) (setq color (color-hsl-to-rgb h max l)))
          ((< s min) (setq color (color-hsl-to-rgb h min l)))))
  color)

;; From https://elpa.gnu.org/packages/ement.  The bit depth has been
;; scaled up to try and avoid components being exactly 0.0, which our
;; contrast function doesn't seem to like.
(defun erc-nicks--gen-color (string)
  "Generate normalized RGB color from STRING."
  (let* ((ratio (/ (float (abs (random string))) (float most-positive-fixnum)))
         (color-num (round (* #xffffffffffff ratio))))
    (list (/ (float (logand color-num #xffff)) #xffff)
          (/ (float (ash (logand color-num #xffff0000) -16)) #xffff)
          (/ (float (ash (logand color-num #xffff00000000) -32)) #xffff))))

;; This doesn't add an entry to the face table because "@" faces are
;; interned in the global `obarray' and thus easily accessible.
(defun erc-nicks--revive (new-face old-face nick net)
  (put new-face 'erc-nicks--custom-face t)
  (put new-face 'erc-nicks--nick nick)
  (put new-face 'erc-nicks--netid erc-networks--id)
  (put old-face 'erc-nicks--key nil)
  (apply #'custom-declare-face new-face (face-user-default-spec old-face)
         (format "Persistent `erc-nicks' color for %s on %s." nick net)
         erc-nicks--custom-keywords))

(defun erc-nicks--create-defface-template (face)
  (pop-to-buffer (get-buffer-create (format "*New face %s*" face)))
  (erase-buffer)
  (lisp-interaction-mode)
  (insert ";; If you *don't* use Customize, put something like this in your\n"
          (substitute-command-keys
           ";; init.el and use \\[eval-last-sexp] to apply any edits.\n\n")
          (format "(defface %s\n  '%S\n  %S"
                  face (face-user-default-spec face) (face-documentation face))
          (cl-loop for (k v) on erc-nicks--custom-keywords by #'cddr
                   concat (format "\n  %s %S" k (list 'quote v)))
          ")\n\n;; Or, if you use use-package\n(use-package erc-nicks\n"
          "  :custom-face\n"
          (format "  (%s %S)" face (face-user-default-spec face))
          ")\n"))

(defun erc-nicks--redirect-face-widget-link (args)
  (pcase args
    (`(,widget face-link . ,plist)
     (when-let ((face (widget-value widget))
                ((get face 'erc-nicks--custom-face)))
       (unless (symbol-file face)
         (setf (plist-get plist :action)
               (lambda (&rest _) (erc-nicks--create-defface-template face))))
       (setf (plist-get plist :help-echo) "Create or edit `defface'."
             (cddr args) plist))))
  args)

(defun erc-nicks--reduce (color)
  "Fold adjustment strategies over COLOR, a string or normalized triple.
Return a hex string."
  (apply #'color-rgb-to-hex
         (seq-reduce (lambda (color strategy) (funcall strategy color))
                     erc-nicks-color-adjustments
                     (if (stringp color) (color-name-to-rgb color) color))))

(defvar erc-nicks--create-pool-function #'erc-nicks--create-coerced-pool
  "Filter function for initializing the pool of colors.
Takes a list of adjustment functions, such as those named in
`erc-nicks-color-adjustments', and a list of colors.  Returns
another list whose members need not be among the original
candidates.  Users should note that this variable, along with its
predefined function values, `erc-nicks--create-coerced-pool' and
`erc-nicks--create-culled-pool', can be made public in a future
version of this module, perhaps as a single user option, given
sufficient demand.")

(defun erc-nicks--create-coerced-pool (adjustments colors)
  "Return COLORS that fall within parameters heeded by ADJUSTMENTS.
Apply ADJUSTMENTS and dedupe after replacing adjusted values with
those nearest defined for the terminal.  Only perform one pass.
That is, accept the nearest initially found as \"close enough,\"
knowing that values may fall outside desired parameters and thus
yield a larger pool than simple culling might produce.  When
debugging, add candidates to `erc-nicks--colors-rejects' that map
to the same output color as some prior candidate."
  (let* ((seen (make-hash-table :test #'equal))
         (erc-nicks-color-adjustments adjustments)
         pool)
    (dolist (color colors)
      (let ((quantized (car (tty-color-approximate
                             (color-values (erc-nicks--reduce color))))))
        (if (gethash quantized seen)
            (when erc-nicks--colors-rejects
              (push color erc-nicks--colors-rejects))
          (push quantized pool)
          (puthash quantized color seen))))
    (nreverse pool)))

(defun erc-nicks--create-culled-pool (adjustments colors)
  "Return COLORS that fall within parameters indicated by ADJUSTMENTS."
  (let (addp capp satp pool)
    (dolist (adjustment adjustments)
      (pcase adjustment
        ((or 'erc-nicks-invert 'erc-nicks-add-contrast) (setq addp t))
        ('erc-nicks-cap-contrast (setq capp t))
        ('erc-nicks-ensaturate (setq satp t))))
    (dolist (color colors)
      (let* ((rgb (color-name-to-rgb color))
             (contrast (and (or addp capp) (erc-nicks--get-contrast rgb))))
        (if (or (and addp (< contrast (car erc-nicks-contrast-range)))
                (and capp (> contrast (cdr erc-nicks-contrast-range)))
                (and-let* ((satp)
                           (s (cadr (apply #'color-rgb-to-hsl rgb))))
                  (or (< s (car erc-nicks-saturation-range))
                      (> s (cdr erc-nicks-saturation-range)))))
            (when erc-nicks--colors-rejects
              (push color erc-nicks--colors-rejects))
          (push color pool))))
    (nreverse pool)))

(defun erc-nicks--init-pool ()
  "Initialize colors and optionally display faces or color palette."
  (unless (eq erc-nicks-colors 'all)
    (let* ((colors (or (and (listp erc-nicks-colors) erc-nicks-colors)
                       (and (memq erc-nicks-colors '(font-lock ansi-color))
                            (erc-nicks--colors-from-faces
                             (format "%s-" erc-nicks-colors)))
                       (defined-colors)))
           (pool (funcall erc-nicks--create-pool-function
                          erc-nicks-color-adjustments colors)))
      (setq erc-nicks--colors-pool pool
            erc-nicks--colors-len (length pool)))))

(defun erc-nicks--determine-color (key)
  (if (eq erc-nicks-colors 'all)
      (erc-nicks--reduce (erc-nicks--gen-color key))
    (let ((pool (erc-with-server-buffer erc-nicks--colors-pool))
          (len (erc-with-server-buffer erc-nicks--colors-len)))
      (nth (% (abs (random key)) len) pool))))

(defun erc-nicks--get-face (nick key)
  "Retrieve a face for trimmed and downcased NICK.
If NICK is new, use KEY to derive color, and store under NICK.
Favor a custom erc-nicks-NICK@NETWORK-face when defined."
  (let ((table (erc-with-server-buffer erc-nicks--face-table)))
    (or (gethash nick table)
        (and-let* ((face (intern-soft (concat "erc-nicks-" nick "@"
                                              (erc-network-name) "-face")))
                   ((or (and (facep face) face)
                        (erc-nicks--revive face face nick (erc-network))))))
        (let ((color (erc-nicks--determine-color key))
              (new-face (make-symbol (concat "erc-nicks-" nick "-face"))))
          (put new-face 'erc-nicks--nick nick)
          (put new-face 'erc-nicks--netid erc-networks--id)
          (put new-face 'erc-nicks--key key)
          (face-spec-set new-face `((t :foreground ,color
                                       :inherit ,erc-nicks-backing-face))
                         'face-defface-spec)
          (set-face-documentation
           new-face (format "Internal face for %s on %s." nick (erc-network)))
          (puthash nick new-face table)))))

(define-inline erc-nicks--anon-face-p (face)
  (inline-quote (and (consp ,face) (pcase (car ,face)
                                     ((pred keywordp) t)
                                     ('foreground-color t)
                                     ('background-color t)))))

(defun erc-nicks--skip-p (prop option limit)
  "Return non-nil if a face in PROP appears in OPTION.
Abandon search after examining LIMIT faces."
  (setq prop (if (erc-nicks--anon-face-p prop) (list prop) (ensure-list prop)))
  (catch 'found
    (while-let (((> limit 0))
                (elem (pop prop)))
      (while (and (consp elem) (not (erc-nicks--anon-face-p elem)))
        (when (cdr elem)
          (push (cdr elem) prop))
        (setq elem (car elem)))
      (when elem
        (cl-decf limit)
        (when (if (symbolp elem) (memq elem option) (member elem option))
          (throw 'found elem))))))

(defun erc-nicks--trim (nickname)
  "Return downcased NICKNAME sans trailing `erc-nicks-ignore-chars'."
  (erc-downcase
   (if erc-nicks-ignore-chars
       (string-trim-right nickname
                          (rx-to-string
                           `(: (+ (any ,erc-nicks-ignore-chars)) eot)))
     nickname)))

(defun erc-nicks--gen-key-from-format-spec (nickname)
  "Generate key for NICKNAME according to `erc-nicks-key-suffix-format'."
  (concat nickname (format-spec erc-nicks-key-suffix-format
                                `((?n . ,(erc-network))
                                  (?m . ,(erc-current-nick))))))

(defun erc-nicks--highlight (nickname &optional base-face)
  "Return face for NICKNAME unless it or BASE-FACE is blacklisted."
  (when-let ((trimmed (erc-nicks--trim nickname))
             ((not (member trimmed erc-nicks--downcased-skip-nicks)))
             ((not (and base-face
                        (erc-nicks--skip-p base-face erc-nicks-skip-faces
                                           erc-nicks--max-skip-search))))
             (key (erc-nicks--gen-key-from-format-spec trimmed)))
    (erc-nicks--get-face trimmed key)))

(defun erc-nicks--highlight-button (nick-object)
  "Possibly add face to `erc-button--nick-user' NICK-OBJECT."
  (when-let
      ((nick-object)
       (face (get-text-property (car (erc-button--nick-bounds nick-object))
                                'font-lock-face))
       (nick (erc-server-user-nickname (erc-button--nick-user nick-object)))
       (out (erc-nicks--highlight nick face)))
    (setf (erc-button--nick-nickname-face nick-object) out
          ;;
          (erc-button--nick-face-cache nick-object)
          (and erc-nicks-track-faces
               (bound-and-true-p erc-track--normal-faces)
               #'erc-nicks--remember-face-for-track)))
  nick-object)

(define-erc-module nicks nil
  "Uniquely colorize nicknames in target buffers."
  ((if erc--target
       (progn
         (erc-with-server-buffer
           (unless erc-nicks-mode
             (erc--warn-once-before-connect 'erc-nicks-mode
               "Module `nicks' must be enabled or disabled session-wide."
               " Toggling it in individual target buffers is unsupported.")
             (erc-nicks-mode +1))) ; but do it anyway
         (setq erc-nicks--downcased-skip-nicks
               (mapcar #'erc-downcase erc-nicks-skip-nicks)
               erc-nicks--fg-rgb (erc-with-server-buffer erc-nicks--fg-rgb))
         (add-function :filter-return (local 'erc-button--modify-nick-function)
                       #'erc-nicks--highlight-button '((depth . 80)))
         (erc-button--phantom-users-mode +1))
     (unless erc-button-mode
       (unless (memq 'button erc-modules)
         (erc--warn-once-before-connect 'erc-nicks-mode
           "Enabling default global module `button' needed by local"
           " module `nicks'. This will impact \C-]all\C-] ERC"
           " sessions. Add `button' to `erc-modules' to avoid this"
           " warning. See Info:\"(erc) Modules\" for more."))
       (erc-button-mode +1))
     (when (equal erc-nicks-bg-color "unspecified-bg")
       (let ((temp (if (eq (erc-nicks--bg-mode) 'light) "white" "black")))
         (erc-button--display-error-notice-with-keys
          "Module `nicks' unable to determine background color.  Setting to \""
          temp "\" globally.  Please see `erc-nicks-bg-color'.")
         (custom-set-variables (list 'erc-nicks-bg-color temp))))
     (setq erc-nicks--fg-rgb
           (or (color-name-to-rgb
                (face-foreground 'erc-default-face nil 'default))
               (color-name-to-rgb
                (readable-foreground-color erc-nicks-bg-color))))
     (erc-nicks--init-pool)
     (erc--restore-initialize-priors erc-nicks-mode
       erc-nicks--face-table (make-hash-table :test #'equal)))
   (setf (alist-get "Edit face" erc-button--nick-popup-alist nil nil #'equal)
         #'erc-nicks-customize-face)
   (erc-nicks--setup-track-integration)
   (add-hook 'erc-track-mode #'erc-nicks--setup-track-integration 50 t)
   (advice-add 'widget-create-child-and-convert :filter-args
               #'erc-nicks--redirect-face-widget-link))
  ((kill-local-variable 'erc-nicks--face-table)
   (kill-local-variable 'erc-nicks--bg-mode-value)
   (kill-local-variable 'erc-nicks--bg-luminance)
   (kill-local-variable 'erc-nicks--fg-rgb)
   (kill-local-variable 'erc-nicks--colors-len)
   (kill-local-variable 'erc-nicks--colors-pool)
   (kill-local-variable 'erc-nicks--downcased-skip-nicks)
   (when (fboundp 'erc-button--phantom-users-mode)
     (erc-button--phantom-users-mode -1))
   (remove-function (local 'erc-track--face-reject-function)
                    #'erc-nicks--reject-uninterned-faces)
   (remove-function (local 'erc-button--modify-nick-function)
                    #'erc-nicks--highlight-button)
   (remove-function (local 'erc-track--alt-normals-function)
                    #'erc-nicks--check-normals)
   (setf (alist-get "Edit face"
                    erc-button--nick-popup-alist nil 'remove #'equal)
         nil)
   (unless erc-button--nick-popup-alist
     (kill-local-variable 'erc-button--nick-popup-alist)))
  'local)

(defun erc-nicks-customize-face (nick)
  "Customize or create persistent face for NICK."
  (interactive (list (or (car (get-text-property (point) 'erc-data))
                         (completing-read "nick: " (or erc-channel-users
                                                       erc-server-users)))))
  (setq nick (erc-nicks--trim (substring-no-properties nick)))
  (let* ((net (erc-network))
         (key (erc-nicks--gen-key-from-format-spec nick))
         (old-face (erc-nicks--get-face nick key))
         (new-face (intern (format "erc-nicks-%s@%s-face" nick net))))
    (unless (eq new-face old-face)
      (erc-nicks--revive new-face old-face nick net)
      (set-face-attribute old-face nil :foreground 'unspecified)
      (set-face-attribute old-face nil :inherit new-face))
    (customize-face new-face)))

(defun erc-nicks--list-faces-help-button-action (face)
  (when-let (((or (get face 'erc-nicks--custom-face)
                  (y-or-n-p (format "Create new persistent face for %s?"
                                    (get face 'erc-nicks--key)))))
             (nid (get face 'erc-nicks--netid))
             (foundp (lambda ()
                       (erc-networks--id-equal-p nid erc-networks--id)))
             (server-buffer (car (erc-buffer-filter foundp))))
    (with-current-buffer server-buffer
      (erc-nicks-customize-face (get face 'erc-nicks--nick)))))

(defun erc-nicks-list-faces ()
  "Show faces owned by ERC-nicks in a help buffer."
  (interactive)
  (save-excursion
    (list-faces-display (rx bot "erc-nicks-"))
    (with-current-buffer "*Faces*"
      (setq help-xref-stack nil
            help-xref-stack-item '(erc-nicks-list-faces))
      (with-silent-modifications
        (goto-char (point-min))
        (while (zerop (forward-line))
          (when (and (get-text-property (point) 'button)
                     (facep (car (button-get (point) 'help-args))))
            (button-put (point) 'help-function
                        #'erc-nicks--list-faces-help-button-action)
            (if-let ((face (car (button-get (point) 'help-args)))
                     ((not (get face 'erc-nicks--custom-face)))
                     ((not (get face 'erc-nicks--key))))
                (progn (delete-region (pos-bol) (1+ (pos-eol)))
                       (forward-line -1))
              (when-let ((nid (get face 'erc-nicks--netid))
                         (net (symbol-name (erc-networks--id-symbol nid))))
                (goto-char (button-end (point)))
                (skip-syntax-forward "-")
                (put-text-property (point) (1+ (point)) 'rear-nonsticky nil)
                (forward-char)
                (when (stringp (face-foreground face))
                  (setq net (format "%-13.13s %s" (substring-no-properties
                                                   (face-foreground face))
                                    net)))
                (insert-and-inherit net)
                (delete-region (button-start (point))
                               (1+ (button-start (point))))
                (delete-region (point) (pos-eol))))))))))

(defun erc-nicks-refresh (debug)
  "Recompute faces for all nicks on current network.
With DEBUG, review affected faces or colors.  Exactly which of
the two depends on the value of `erc-nicks-colors'.  Note that
the list of rejected faces may include duplicates of accepted
ones."
  (interactive "P")
  (unless (derived-mode-p 'erc-mode)
    (user-error "Not an ERC buffer"))
  (erc-with-server-buffer
    (unless erc-nicks-mode (user-error "Module `nicks' disabled"))
    (let ((erc-nicks--colors-rejects (and debug (list t))))
      (erc-nicks--init-pool)
      (unless erc-nicks--colors-pool
        (user-error "Pool empty: all colors rejected"))
      (dolist (nick (hash-table-keys erc-nicks--face-table))
        ;; User-tuned faces do not have an `erc-nicks--key' property.
        (when-let ((face (gethash nick erc-nicks--face-table))
                   (key (get face 'erc-nicks--key)))
          (setq key (erc-nicks--gen-key-from-format-spec nick))
          (put face 'erc-nicks--key key)
          (set-face-foreground face (erc-nicks--determine-color key))))
      (when debug
        (if (eq erc-nicks-colors 'all)
            (erc-nicks-list-faces)
          (pcase-dolist (`(,name ,pool)
                         `(("*erc-nicks-pool*" ,erc-nicks--colors-pool)
                           ("*erc-nicks-rejects*"
                            ,(cdr (nreverse erc-nicks--colors-rejects)))))
            (when (buffer-live-p (get-buffer name))
              (kill-buffer name))
            (when pool
              (save-excursion
                (list-colors-display
                 pool name
                 (lambda (c)
                   (message "contrast: %.3f :saturation: %.3f"
                            (erc-nicks--get-contrast c)
                            (cadr (apply #'color-rgb-to-hsl
                                         (color-name-to-rgb c))))))))))))))

(defun erc-nicks--colors-from-faces (prefix)
  "Extract foregrounds from faces with PREFIX
Expect PREFIX to be something like \"ansi-color-\" or \"font-lock-\"."
  (let (out)
    (dolist (face (face-list) (nreverse out))
      (when-let (((string-prefix-p prefix (symbol-name face)))
                 (color (face-foreground face)))
        (push color out)))))

(defun erc-nicks--reject-uninterned-faces (candidate)
  "Remove own faces from CANDIDATE if it's a combination of faces."
  (while-let ((next (car-safe candidate))
              ((facep next))
              ((not (intern-soft next))))
    (setq candidate (cdr candidate)))
  (if (and (consp candidate) (not (cdr candidate))) (car candidate) candidate))

(define-inline erc-nicks--oursp (face)
  (inline-quote
   (and-let* ((sym (car-safe ,face))
              ((symbolp sym))
              ((get sym 'erc-nicks--key)))
     sym)))

(defun erc-nicks--check-normals (current contender contenders normals)
  "Return a viable `nicks'-owned face from NORMALS in CONTENDERS.
But only do so if the CURRENT face is also one of ours and in
NORMALS and if the highest ranked CONTENDER among new faces is
`erc-default-face', the lowest ranking default priority face."
  (and-let* (((eq contender 'erc-default-face))
             ((or (null current) (gethash current normals)))
             (spkr (or (null current) (erc-nicks--oursp current))))
    (catch 'contender
      (dolist (candidate (cdr contenders) contender)
        (when-let (((not (equal candidate current)))
                   ((gethash candidate normals))
                   (s (erc-nicks--oursp candidate))
                   ((not (eq s spkr))))
          (throw 'contender candidate))))))

(defun erc-nicks--setup-track-integration ()
  "Restore traditional \"alternating normal\" face functionality to mode-line."
  (when (bound-and-true-p erc-track-mode)
    (pcase erc-nicks-track-faces
      ;; Variant `defer' is handled elsewhere.
      ('prioritize
       (add-function :override (local 'erc-track--alt-normals-function)
                     #'erc-nicks--check-normals))
      ('nil
       (add-function :override (local 'erc-track--face-reject-function)
                     #'erc-nicks--reject-uninterned-faces)))))

(defun erc-nicks--remember-face-for-track (face)
  "Add FACE to local hash table maintained by `track' module."
  (or (gethash face erc-track--normal-faces)
      (if-let ((sym (or (car-safe face) face))
               ((symbolp sym))
               ((get sym 'erc-nicks--key)))
          (puthash face face erc-track--normal-faces)
        face)))

(provide 'erc-nicks)

;;; erc-nicks.el ends here
