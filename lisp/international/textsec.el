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
(require 'uni-confusable)
(require 'ucs-normalize)
(require 'idna-mapping)
(require 'puny)
(require 'mail-parse)

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
Levels are (in decreasing order of restrictiveness) `ascii-only',
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

(defun textsec-ascii-confusable-p (string)
  "Return non-nil if STRING isn't ASCII, but is confusable with ASCII."
  (and (not (eq (textsec-restriction-level string) 'ascii-only))
       (eq (textsec-restriction-level (textsec-unconfuse-string string))
           'ascii-only)))

(defun textsec-unconfuse-string (string)
  "Return a de-confused version of STRING.
This algorithm is described in:

  https://www.unicode.org/reports/tr39/#Confusable_Detection"
  (ucs-normalize-NFD-string
   (apply #'concat
          (seq-map (lambda (char)
                     (or (gethash char uni-confusable-table)
                         (string char)))
                   (ucs-normalize-NFD-string string)))))

(defun textsec-resolved-script-set (string)
  "Return the resolved script set for STRING.
This is the minimal covering script set for STRING, but is nil is
STRING isn't a single script string."
  (and (textsec-single-script-p string)
       (textsec-covering-scripts string)))

(defun textsec-single-script-confusable-p (string1 string2)
  "Say whether STRING1 and STRING2 are single script confusables."
  (and (equal (textsec-unconfuse-string string1)
              (textsec-unconfuse-string string2))
       ;; And they have to have at least one resolved script in
       ;; common.
       (seq-intersection (textsec-resolved-script-set string1)
                         (textsec-resolved-script-set string2))))

(defun textsec-mixed-script-confusable-p (string1 string2)
  "Say whether STRING1 and STRING2 are mixed script confusables."
  (and (equal (textsec-unconfuse-string string1)
              (textsec-unconfuse-string string2))
       ;; And they have no resolved scripts in common.
       (null (seq-intersection (textsec-resolved-script-set string1)
                               (textsec-resolved-script-set string2)))))

(defun textsec-whole-script-confusable-p (string1 string2)
  "Say whether STRING1 and STRING2 are whole script confusables."
  (and (textsec-mixed-script-confusable-p string1 string2)
       (textsec-single-script-p string1)
       (textsec-single-script-p string2)))

(defun textsec-domain-suspicious-p (domain)
  "Say whether DOMAIN looks suspicious.
If it isn't, nil is returned.  If it is, a string explaining the
problem is returned."
  (catch 'found
    (seq-do
     (lambda (char)
       (when (eq (elt idna-mapping-table char) t)
         (throw 'found (format "Disallowed character: `%s' (#x%x)"
                               (string char) char))))
     domain)
    (unless (puny-highly-restrictive-domain-p domain)
      (throw 'found "%s is not highly restrictive"))
    nil))

(defun textsec-local-address-suspicious-p (local)
  "Say whether LOCAL looks suspicious.
LOCAL is the bit before \"@\" in an email address.

If it suspicious, nil is returned.  If it is, a string explaining
the problem is returned."
  (cond
   ((not (equal local (ucs-normalize-NFKC-string local)))
    (format "`%s' is not in normalized format `%s'"
            local (ucs-normalize-NFKC-string local)))
   ((textsec-mixed-numbers-p local)
    (format "`%s' contains numbers from different number systems" local))
   ((eq (textsec-restriction-level local) 'unrestricted)
    (format "`%s' isn't restrictive enough" local))
   ((string-match-p "\\`\\.\\|\\.\\'\\|\\.\\." local)
    (format "`%s' contains invalid dots" local))))

(defun textsec-name-suspicious-p (name)
  "Say whether NAME looks suspicious.
NAME is (for instance) the free-text name from an email address.

If it suspicious, nil is returned.  If it is, a string explaining
the problem is returned."
  (cond
   ((not (equal name (ucs-normalize-NFC-string name)))
    (format "`%s' is not in normalized format `%s'"
            name (ucs-normalize-NFC-string name)))
   ((seq-find (lambda (char)
                (and (member char bidi-control-characters)
                     (not (member char
                                  '( ?\N{left-to-right mark}
                                     ?\N{right-to-left mark}
                                     ?\N{arabic letter mark})))))
              name)
    (format "The string contains bidirectional control characters"))
   ((textsec-suspicious-nonspacing-p name))))

(defun textsec-suspicious-nonspacing-p (string)
  "Say whether STRING has a suspicious use of nonspacing characters.
If it suspicious, nil is returned.  If it is, a string explaining
the problem is returned."
  (let ((prev nil)
        (nonspace-count 0))
    (catch 'found
      (seq-do
       (lambda (char)
         (let ((nonspacing
                (memq (get-char-code-property char 'general-category)
                      '(Cf Cc Mn))))
           (when (and nonspacing
                      (equal char prev))
             (throw 'found "Two identical nonspacing characters in a row"))
           (setq nonspace-count (if nonspacing
                                    (1+ nonspace-count)
                                  0))
           (when (> nonspace-count 4)
             (throw 'found
                    "Excessive number of nonspacing characters in a row"))
           (setq prev char)))
       string)
      nil)))

(defun textsec-email-suspicious-p (email)
  "Say whether EMAIL looks suspicious.
If it isn't, nil is returned.  If it is, a string explaining the
problem is returned."
  (pcase-let* ((`(,address . ,name) (mail-header-parse-address email t))
               (`(,local ,domain) (split-string address "@")))
    (or
     (textsec-domain-suspicious-p domain)
     (textsec-local-address-suspicious-p local)
     (and name (textsec-name-suspicious-p name)))))

(provide 'textsec)

;;; textsec.el ends here
