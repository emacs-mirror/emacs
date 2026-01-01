;;; textsec.el --- Functions for handling homoglyphs and the like  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(require 'url)

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
  "Return a list of Unicode scripts used by characters in STRING.
The return value is a list where for each character in STRING,
there is a list of script symbols for that character.  Thus, each
script's symbol can appear more than once; use `textsec-covering-scripts'
to obtain a list in which each script appears at most once.
The script symbols returned by this function follow the Unicode Script
property of characters as defined by the Unicode Standard Annex 24 (UAX#24).
See the Unicode UCD file Scripts.txt for the scripts defined by Unicode."
  (seq-map (lambda (char)
             (elt textsec--char-scripts char))
           string))

(defun textsec-single-script-p (string)
  "Return non-nil if STRING's characters belong to a single Unicode script.

Note that the concept of \"single script\" used by this function
isn't obvious -- some mixtures of scripts count as a \"single
script\".  See

  https://www.unicode.org/reports/tr39/#Mixed_Script_Detection

for details.  The Unicode script property of a characters is defined by
the Unicode Standard Annex 24 (UAX#24)."
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
  "Return a minimal list of scripts used by characters in STRING.
Note that a string may have several different minimal cover sets.
The return value is a list of script symbols.
The script property of characters is defined by the Unicode Standard
Annex 24 (UAX#24)."
  (let* ((scripts (textsec-scripts string))
         (set (car scripts)))
    (dolist (s scripts)
      (setq set (seq-union set (seq-difference s set))))
    (sort (delq 'common (delq 'inherited set)) #'string<)))

(defun textsec-restriction-level (string)
  "Return the restriction level for which STRING qualifies.
The return value is a symbol.
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
         ;; (but without latin, cyrillic and greek).
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
    'moderately-restrictive)
   ;; Fixme `minimally-restrictive' -- needs well-formedness criteria
   ;; and Identifier Profile.
   (t
    'unrestricted))))

(defun textsec-mixed-numbers-p (string)
  "Return non-nil if STRING includes numbers from different decimal systems.

This function examines only characters in STRING whose Unicode general
category, as reported by `get-char-code-property' with its second
argument \\='general-category, is Decimal_Numbers (Nd).  It returns
non-nil if it finds numerical characters from different numerical
systems.  For example, ASCII digit characters and ARABIC-INDIC DIGIT
characters belong to different decimal systems."
  (>
   (length
    (seq-uniq
     (mapcar
      (lambda (char)
        ;; Compare zeros in the respective decimal systems.
        (- char (get-char-code-property char 'numeric-value)))
      (seq-filter (lambda (char)
                    ;; We're selecting the characters that
                    ;; have a numeric property.
                    (eq (get-char-code-property char 'general-category)
                        'Nd))
                  string))))
   1))

(defun textsec-ascii-confusable-p (string)
  "Return non-nil if non-ASCII STRING can be confused with ASCII on display."
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
The value is a list whose members are symbols of the minimal covering
script set for STRING; the value is nil if STRING isn't a single-script
string.
The script property of characters is defined by the Unicode Standard
Annex 24 (UAX#24)."
  (and (textsec-single-script-p string)
       (textsec-covering-scripts string)))

(defun textsec-single-script-confusable-p (string1 string2)
  "Say whether STRING1 and STRING2 are single-script confusables.
Two strings are said to be confusables if they might look very
similarly on display.
The script property of characters is defined by the Unicode Standard
Annex 24 (UAX#24)."
  (and (equal (textsec-unconfuse-string string1)
              (textsec-unconfuse-string string2))
       ;; And they have to have at least one resolved script in
       ;; common.
       (seq-intersection (textsec-resolved-script-set string1)
                         (textsec-resolved-script-set string2))))

(defun textsec-mixed-script-confusable-p (string1 string2)
  "Say whether STRING1 and STRING2 are mixed-script confusables.
Two strings are said to be confusables if they might look very
similarly on display.
The script property of characters is defined by the Unicode Standard
Annex 24 (UAX#24)."
  (and (equal (textsec-unconfuse-string string1)
              (textsec-unconfuse-string string2))
       ;; And they have no resolved scripts in common.
       (null (seq-intersection (textsec-resolved-script-set string1)
                               (textsec-resolved-script-set string2)))))

(defun textsec-whole-script-confusable-p (string1 string2)
  "Say whether two single-script strings STRING1 and STRING2 are confusables.
Two strings are said to be confusables if they might look very
similarly on display.
The script property of characters is defined by the Unicode Standard
Annex 24 (UAX#24)."
  (and (textsec-mixed-script-confusable-p string1 string2)
       (textsec-single-script-p string1)
       (textsec-single-script-p string2)))

(defun textsec--ipvx-address-p (domain)
  "Return non-nil if DOMAIN is an ipv4 or ipv6 address."
  ;; This is a very relaxed pattern for IPv4 or IPv6 addresses.  The
  ;; assumption is that any malformed address accepted by this rule
  ;; will be rejected by the actual address parser eventually.
  (let ((case-fold-search t))
    (rx-let ((ipv4 (** 1 4
                       (** 1 3 (in "0-9"))
                       (? ".")))
             (ipv6 (: (** 1 7
                          (** 0 4 (in "0-9a-f"))
                          ":")
                      (** 0 4 (in "0-9a-f"))
                      (? ":" ipv4))))
      (string-match-p (rx bos (or ipv4 ipv6 (: "[" ipv6 "]")) eos) domain))))

(defun textsec-domain-suspicious-p (domain)
  "Say whether DOMAIN's name looks suspicious.
Return nil if it isn't suspicious.  If it is, return a string explaining
the potential problem.

Domain names are considered suspicious if they use characters
that can look similar to other characters when displayed, or
use characters that are not allowed by Unicode's IDNA mapping,
or use certain other unusual mixtures of characters."
  (catch 'found
    ;; Plain domains aren't suspicious.
    (when (textsec--ipvx-address-p domain)
      (throw 'found nil))
    (seq-do
     (lambda (char)
       (when (eq (elt idna-mapping-table char) t)
         (throw 'found
                (format "Disallowed character in domain%s (#x%x, %s)"
                        (if (eq (get-char-code-property char 'general-category)
                                'Cf)
                            ""
                          (concat ": " (string char)))
                        char
                        (char-to-name char)))))
     domain)
    ;; Does IDNA allow it?
    (unless (puny-highly-restrictive-domain-p domain)
      (throw
       'found
       (format "`%s' mixes characters from different scripts in suspicious ways"
               domain)))
    ;; Check whether any segment of the domain name is confusable with
    ;; an ASCII-only segment.
    (dolist (elem (split-string domain "\\."))
      (when (textsec-ascii-confusable-p elem)
        (throw 'found (format "`%s' includes characters confusable with ASCII"
                              elem))))
    nil))

(defun textsec-local-address-suspicious-p (local)
  "Say whether LOCAL part of an email address looks suspicious.
LOCAL is the part before \"@\" in an email address, a string.

If it isn't suspicious, return nil.  If it is, return a string explaining
the potential problem.

Email addresses are considered suspicious if they use characters
that can look similar to other characters when displayed, or use
certain other unusual mixtures of characters."
  (cond
   ((not (equal local (ucs-normalize-NFKC-string local)))
    (format "`%s' is not in normalized form `%s', its display might deceive"
            local (ucs-normalize-NFKC-string local)))
   ((textsec-mixed-numbers-p local)
    (format "`%s' contains numbers from different number systems" local))
   ((eq (textsec-restriction-level local) 'unrestricted)
    (format "`%s' uses characters from too many unusual scripts" local))
   ((string-match-p "\\`\\.\\|\\.\\'\\|\\.\\." local)
    (format "`%s' contains invalid dot characters" local))))

(defun textsec-bidi-controls-suspicious-p (string)
  "Return non-nil of STRING uses bidirectional controls in suspicious ways.
If STRING doesn't include any suspicious uses of bidirectional
formatting control characters, return nil.  Otherwise, return the
index of the first character in STRING affected by such suspicious
use of bidi controls.  If the returned value is beyond the length
of STRING, it means any text following STRING on display might be
affected by bidi controls in STRING."
  (with-temp-buffer
    ;; We follow STRING with text that's representative of some text
    ;; that could follow it, with the purpose of detecting residual bidi
    ;; state at end of STRING which could then affect the following
    ;; text.
    (insert string "a1א:!")
    (let ((pos (bidi-find-overridden-directionality
                (point-min) (point-max) nil)))
      (and (fixnump pos)
           (1- pos)))))

(defun textsec-name-suspicious-p (name)
  "Say whether NAME looks suspicious.
NAME is a string, for instance, the free-text display name part
of an email address.

If it isn't suspicious, return nil.  If it is, return a string
explaining the potential problem.

Names are considered suspicious if they use characters that can
look similar to other characters when displayed, or use certain
other unusual mixtures of characters."
  (cond
   ((not (equal name (ucs-normalize-NFC-string name)))
    (format "`%s' is not in normalized form `%s', its display might deceive"
            name (ucs-normalize-NFC-string name)))
   ((and (seq-find (lambda (char)
                     (and (member char bidi-control-characters)
                          (not (member char
                                       '( ?\N{left-to-right mark}
                                          ?\N{right-to-left mark}
                                          ?\N{arabic letter mark})))))
                   name)
         ;; We have bidirectional formatting characters, but check
         ;; whether they affect any other characters in suspicious
         ;; ways.  If not, NAME is not suspicious.
         (fixnump (textsec-bidi-controls-suspicious-p name)))
    (format "`%s' contains suspicious uses of bidirectional control characters"
            name))
   ((textsec-suspicious-nonspacing-p name))))

(defun textsec-suspicious-nonspacing-p (string)
  "Say whether STRING uses nonspacing characters in suspicious ways.
If it doesn't, return nil.  If it does, return a string explaining
the potential problem.

Nonspacing characters are those whose general Unicode category is
Mn (nonspacing mark) or Me (enclosing mark).  Examples include
diacritics and accents.

Use of nonspacing characters is considered suspicious if there are
two or more consecutive identical nonspacing characters, or too many
consecutive nonspacing characters."
  (let ((prev nil)
        (nonspace-count 0))
    (catch 'found
      (seq-do
       (lambda (char)
         (let ((nonspacing
                (memq (get-char-code-property char 'general-category)
                      '(Mn Me))))
           (when (and nonspacing
                      (equal char prev))
             (throw 'found "Two identical consecutive accent/diacritic characters"))
           (setq nonspace-count (if nonspacing
                                    (1+ nonspace-count)
                                  0))
           (when (> nonspace-count 4)
             (throw 'found
                    "Too many consecutive accent/diacritic characters"))
           (setq prev char)))
       string)
      nil)))

(defun textsec-email-address-suspicious-p (address)
  "Say whether email ADDRESS looks suspicious.
If it isn't, return nil.  If it is, return a string explaining the
potential problem.

ADDRESS should be a string that specifies an email address.
An email address is considered suspicious if either of its two
parts -- the local address name or the domain -- are found to be
suspicious by, respectively, `textsec-local-address-suspicious-p'
and `textsec-domain-suspicious-p'."
  (pcase-let ((`(,local ,domain) (split-string address "@")))
    (or
     (if domain (textsec-domain-suspicious-p domain))
     (textsec-local-address-suspicious-p local))))

(defun textsec-email-address-header-suspicious-p (email)
  "Say whether EMAIL address specification looks suspicious.
If it isn't, return nil.  If it is, return a string explaining the
potential problem.

Note that EMAIL has to be a valid email specification according
to RFC2047bis -- strings that can't be parsed will be flagged as
suspicious.

An email specification is considered suspicious if either of its
two parts -- the address or the name -- are found to be
suspicious by, respectively, `textsec-email-address-suspicious-p'
and `textsec-name-suspicious-p'."
  (catch 'end
    (pcase-let ((`(,address . ,name)
                 (condition-case nil
                     (mail-header-parse-address email t)
                   (error (throw 'end "Email address can't be parsed.")))))
      (or
       (and address (textsec-email-address-suspicious-p address))
       (and name (textsec-name-suspicious-p name))))))

(defun textsec-url-suspicious-p (url)
  "Say whether URL looks suspicious.
If it isn't, return nil.  If it is, return a string explaining the
potential problem."
  (let ((parsed (url-generic-parse-url url)))
    ;; The URL may not have a domain.
    (and (url-host parsed)
         (textsec-domain-suspicious-p (url-host parsed)))))

(defun textsec-link-suspicious-p (link)
  "Say whether LINK is suspicious.
LINK should be a cons cell where the first element is the URL,
and the second element is the link text.

This function will return non-nil if it seems like the link text
is misleading about where the URL takes you.  This is typical
when the link text looks like an URL itself, but doesn't lead to
the same domain as the URL."
  (let* ((url (car link))
         (text (string-trim (cdr link))))
    (catch 'found
      (let ((udomain (url-host (url-generic-parse-url url)))
            (tdomain (url-host (url-generic-parse-url text))))
        (cond
         ((and udomain
               tdomain
               (not (equal udomain tdomain))
               ;; One may be a sub-domain of the other, but don't allow too
               ;; short domains.
               (not (or (and (string-suffix-p udomain tdomain)
                             (url-domsuf-cookie-allowed-p udomain))
                        (and (string-suffix-p tdomain udomain)
                             (url-domsuf-cookie-allowed-p tdomain)))))
          (throw 'found
                 (format "Text `%s' doesn't point to link URL `%s'"
                         text url)))
         ((and tdomain
               (textsec-domain-suspicious-p tdomain))
          (throw 'found
                 (format "Domain `%s' in the link text is suspicious"
                         (bidi-string-strip-control-characters
                          tdomain)))))))))

(provide 'textsec)

;;; textsec.el ends here
