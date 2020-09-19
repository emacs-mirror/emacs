;;; shorthand.el --- namespacing system  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple-minded namespacing in Emacs:

;; 1. Do this on an Emacs you don't care about, since this advises basic
;;    functions;
;; 2. Load `shorthand.el` (or byte-compile and load it);
;; 3. Construct an example user of this library.
;;
;;    magnar-string.el is constructed by taking s.el, renaming it to
;;    magnar-string.el, and then appending this to the end of the file:
;;
;;    ;;; magnar-string.el ends here,
;;    Local Variables:
;;    shorthand-shorthands: (("^s-" . "magnar-string-"))
;;    End:
;;
;; 4. Load `magnar-string.el` or byte-compile it and load `magnar-string.elc`;
;; 5. Try C-h f and check there's no "s-" pollution; Not even the `s-`
;;    symbols are interned.  All the relevant functions are namespaced
;;    under "magnar-string-";
;; 6. Open test.el, and play around there.  Open test2.el and play around
;;    with magnar-string.el under a different "mstring-" prefix;
;; 7. Evaluating code should work.  Eldoc should also work.  Xref (`M-.`)
;;    is broken.  Anything else might breaks spectacularly;

;; Read `shorthand.el`: it's less than 50 loc.  The idea is to keep only
;; one obarray, but instruments `read` to not pollute it with symbols
;; that with the shorthands for other longer named symbols.

;;; Code:
(require 'cl-lib)

(put 'shorthand-shorthands 'safe-local-variable #'consp)

(defun shorthand-load-wrapper (wrappee file &rest stuff)
  "Load Elisp FILE, aware of file-local `shortand-shorthands'."
  (let (file-local-shorthands)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (hack-local-variables)
        (setq file-local-shorthands shorthand-shorthands)))
    (let ((shorthand-shorthands file-local-shorthands))
      (apply wrappee file stuff))))

(advice-add 'load        :around #'shorthand-load-wrapper)

(provide 'shorthand)
;;; shorthand.el ends here
