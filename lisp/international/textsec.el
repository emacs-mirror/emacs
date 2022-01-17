;;; textsec.el --- Functions for handling homoglyphs and the like  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)

(defvar textsec--char-scripts nil)

(eval-and-compile
  (defun textsec--create-script-table (data)
    "Create the textsec--char-scripts char table."
    (setq textsec--char-scripts (make-char-table nil))
    (dolist (scripts data)
      (dolist (range (cadr scripts))
        (set-char-table-range textsec--char-scripts
                              range (car scripts)))))
  (require 'uni-scripts))

(defun textsec-scripts (string)
  "Return a list of Unicode scripts used in STRING.
The scripts returned by this function use the Unicode Script property
as defined by the Unicode Standard Annex 24 (UAX#24)."
  (seq-map (lambda (char)
             (elt textsec--char-scripts char))
           string))

(defun textsec-single-script-p (string)
  "Return non-nil if STRING is all in a single Unicode script.

Note that the concept of \"single script\" used by this function
isn't obvious -- some mixtures of scripts count as a \"single
script\".  See

  https://www.unicode.org/reports/tr39/#Mixed_Script_Detection

for details.  The Unicode scripts are as defined by the
Unicode Standard Annex 24 (UAX#24)."
  (let ((scripts (mapcar
                  (lambda (s)
                    (append s
                            ;; Some scripts used in East Asia are
                            ;; commonly used across borders, so we add
                            ;; those.
                            (mapcan (lambda (script)
                                      (copy-sequence
                                       (textsec--augment-script script)))
                                    s)))
                         (textsec-scripts string))))
    (catch 'empty
      (cl-loop for s1 in scripts
               do (cl-loop for s2 in scripts
                           ;; Common/inherited chars can be used in
                           ;; text with all scripts.
                           when (and (not (memq 'common s1))
                                     (not (memq 'common s2))
                                     (not (memq 'inherited s1))
                                     (not (memq 'inherited s2))
                                     (not (seq-intersection s1 s2)))
                           do (throw 'empty nil)))
      t)))

(defun textsec--augment-script (script)
  (cond
   ((eq script 'han)
    '(hangul japan korea))
   ((or (eq script 'hiragana)
        (eq script 'katakana))
    '(japan))
   ((or (eq script 'hangul)
        (eq script 'bopomofo))
    '(korea))))

(defun textsec-covering-scripts (string)
  "Return a minimal list of scripts used in STRING.
Note that a string may have several different minimal cover sets.
The scripts are as defined by the Unicode Standard Annex 24 (UAX#24)."
  (let* ((scripts (textsec-scripts string))
         (set (car scripts)))
    (dolist (s scripts)
      (setq set (seq-union set (seq-difference s set))))
    (sort (delq 'common (delq 'inherited set)) #'string<)))

(defun textsec-restriction-level (string)
  "Say what restriction level STRING qualifies for.
Levels are (in order of restrictiveness) `ascii-only',
`single-script', `highly-restrictive', `moderately-restrictive',
`minimally-restrictive' and `unrestricted'."
  (let ((scripts (textsec-covering-scripts string)))
  (cond
   ((string-match "\\`[[:ascii:]]+\\'" string)
    'ascii-only)
   ((textsec-single-script-p string)
    'single-script)
   ((or (null (seq-difference scripts '(latin han hiragana katakana)))
        (null (seq-difference scripts '(latin han bopomofo)))
        (null (seq-difference scripts '(latin han hangul))))
    'highly-restrictive)
   ((and (= (length scripts) 2)
         (memq 'latin scripts)
         ;; This list comes from
         ;; https://www.unicode.org/reports/tr31/#Table_Recommended_Scripts
         ;; (but is without latin, cyrillic and greek).
         (seq-intersection scripts
                           '(arabic
                             armenian
                             bengali
                             bopomofo
                             devanagari
                             ethiopic
                             georgian
                             gujarati
                             gurmukhi
                             hangul
                             han
                             hebrew
                             hiragana
                             katakana
                             kannada
                             khmer
                             lao
                             malayalam
                             myanmar
                             oriya
                             sinhala
                             tamil
                             telugu
                             thaana
                             thai
                             tibetan)))
    ;; The string is covered by Latin and any one other Recommended
    ;; script, except Cyrillic, Greek.
    'moderately-retrictive)
   ;; Fixme `minimally-restrictive' -- needs well-formedness criteria
   ;; and Identifier Profile.
   (t
    'unrestricted))))

(defun textsec-mixed-numbers-p (string)
  "Return non-nil if there are numbers from different decimal systems in STRING."
  (>
   (length
    (seq-uniq
     (mapcar
      (lambda (char)
        (get-char-code-property char 'numeric-value))
      (seq-filter (lambda (char)
                    ;; We're selecting the characters that
                    ;; have a numeric property.
                    (eq (get-char-code-property char 'general-category)
                        'Nd))
                  string))))
   1))

(provide 'textsec)

;;; textsec.el ends here
