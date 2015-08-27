;;; character-fold.el --- match unicode to similar ASCII -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: matching

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;;###autoload
(defvar character-fold-search nil
  "Non-nil if searches should fold similar characters.
This means some characters will match entire groups of characters.
For instance, \" will match all variants of double quotes, and
the letter a will match all of its accented versions (and then
some).")

(defconst character-fold-table
  (eval-when-compile
    (let* ((equiv (make-char-table 'character-fold-table))
           (table (unicode-property-table-internal 'decomposition))
           (func (char-table-extra-slot table 1)))
      ;; Ensure the table is populated.
      (map-char-table
       (lambda (i v) (when (consp i) (funcall func (car i) v table)))
       table)

      ;; Compile a list of all complex characters that each simple
      ;; character should match.
      (map-char-table
       (lambda (i dec)
         (when (consp dec)
           ;; Discard a possible formatting tag.
           (when (symbolp (car dec))
             (setq dec (cdr dec)))
           ;; Skip trivial cases like ?a decomposing to (?a).
           (unless (or (and (eq i (car dec))
                            (not  (cdr dec))))
             (let ((d dec)
                   (fold-decomp t)
                   k found)
               (while (and d (not found))
                 (setq k (pop d))
                 ;; Is k a number or letter, per unicode standard?
                 (setq found (memq (get-char-code-property k 'general-category)
                                   '(Lu Ll Lt Lm Lo Nd Nl No))))
               (if found
                   ;; Check if the decomposition has more than one letter,
                   ;; because then we don't want the first letter to match
                   ;; the decomposition.
                   (dolist (k d)
                     (when (and fold-decomp
                                (memq (get-char-code-property k 'general-category)
                                      '(Lu Ll Lt Lm Lo Nd Nl No)))
                       (setq fold-decomp nil)))
                 ;; If there's no number or letter on the
                 ;; decomposition, take the first character in it.
                 (setq found (car-safe dec)))
               ;; Finally, we only fold multi-char decomposition if at
               ;; least one of the chars is non-spacing (combining).
               (when fold-decomp
                 (setq fold-decomp nil)
                 (dolist (k dec)
                   (when (and (not fold-decomp)
                              (> (get-char-code-property k 'canonical-combining-class) 0))
                     (setq fold-decomp t))))
               ;; Add i to the list of characters that k can
               ;; represent. Also possibly add its decomposition, so we can
               ;; match multi-char representations like (format "a%c" 769)
               (when (and found (not (eq i k)))
                 (let ((chars (cons (char-to-string i) (aref equiv k))))
                   (aset equiv k
                         (if fold-decomp
                             (cons (apply #'string dec) chars)
                           chars))))))))
       table)

      ;; Add some manual entries.
      (dolist (it '((?\" "＂" "“" "”" "”" "„" "⹂" "〞" "‟" "‟" "❞" "❝" "❠" "“" "„" "〝" "〟" "🙷" "🙶" "🙸" "«" "»")
                    (?' "❟" "❛" "❜" "‘" "’" "‚" "‛" "‚" "󠀢" "❮" "❯" "‹" "›")
                    (?` "❛" "‘" "‛" "󠀢" "❮" "‹")))
        (let ((idx (car it))
              (chars (cdr it)))
          (aset equiv idx (append chars (aref equiv idx)))))

      ;; Convert the lists of characters we compiled into regexps.
      (map-char-table
       (lambda (i v) (let ((re (regexp-opt (cons (char-to-string i) v))))
                  (if (consp i)
                      (set-char-table-range equiv i re)
                    (aset equiv i re))))
       equiv)
      equiv))
  "Used for folding characters of the same group during search.")

;;;###autoload
(defun character-fold-to-regexp (string &optional lax)
  "Return a regexp matching anything that character-folds into STRING.
If `character-fold-search' is nil, `regexp-quote' string.
Otherwise, any character in STRING that has an entry in
`character-fold-table' is replaced with that entry (which is a
regexp) and other characters are `regexp-quote'd.
If LAX is non-nil, any single whitespace character is allowed to
match any number of times."
  (if character-fold-search
      (apply #'concat
             (mapcar (lambda (c) (if (and lax (memq c '(?\s ?\t ?\r ?\n)))
                                "[ \t\n\r\xa0\x2002\x2d\x200a\x202f\x205f\x3000]+"
                              (or (aref character-fold-table c)
                                  (regexp-quote (string c)))))
                     string))
    (regexp-quote string)))

;;; character-fold.el ends here
