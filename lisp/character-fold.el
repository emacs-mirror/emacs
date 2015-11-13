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

(defun character-fold--make-space-string (n)
  "Return a string that matches N spaces."
  (format "\\(?:%s\\|%s\\)"
          (make-string n ?\s)
          (apply #'concat
                 (make-list n (or (aref character-fold-table ?\s) " ")))))

;;;###autoload
(defun character-fold-to-regexp (string &optional _lax)
  "Return a regexp matching anything that character-folds into STRING.
Any character in STRING that has an entry in
`character-fold-table' is replaced with that entry (which is a
regexp) and other characters are `regexp-quote'd."
  (let* ((spaces 0)
         (chars (mapcar #'identity string))
         (out chars))
    ;; When the user types a space, we want to match the table entry,
    ;; but we also want the ?\s to be visible to `search-spaces-regexp'.
    ;; See commit message for a longer description.
    (while chars
      (let ((c (car chars)))
        (setcar chars
                (cond
                 ((eq c ?\s)
                  (setq spaces (1+ spaces))
                  nil)
                 ((> spaces 0)
                  (prog1 (concat (character-fold--make-space-string spaces)
                                 (or (aref character-fold-table c)
                                     (regexp-quote (string c))))
                    (setq spaces 0)))
                 (t (or (aref character-fold-table c)
                        (regexp-quote (string c))))))
        (setq chars (cdr chars))))
    (concat (apply #'concat out)
            (when (> spaces 0)
              (character-fold--make-space-string spaces)))))


;;; Commands provided for completeness.
(defun character-fold-search-forward (string &optional bound noerror count)
  "Search forward for a character-folded version of STRING.
STRING is converted to a regexp with `character-fold-to-regexp',
which is searched for with `re-search-forward'.
BOUND NOERROR COUNT are passed to `re-search-forward'."
  (interactive "sSearch: ")
  (re-search-forward (character-fold-to-regexp string) bound noerror count))

(defun character-fold-search-backward (string &optional bound noerror count)
  "Search backward for a character-folded version of STRING.
STRING is converted to a regexp with `character-fold-to-regexp',
which is searched for with `re-search-backward'.
BOUND NOERROR COUNT are passed to `re-search-backward'."
  (interactive "sSearch: ")
  (re-search-backward (character-fold-to-regexp string) bound noerror count))

(provide 'character-fold)

;;; character-fold.el ends here
