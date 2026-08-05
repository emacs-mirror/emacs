;;; typescript-ts-mode-tests.el --- Tests for Tree-sitter-based TypeScript mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'treesit)

(ert-deftest typescript-ts-mode-test-indentation ()
  (skip-unless (and (treesit-ready-p 'typescript)
                    (treesit-ready-p 'tsx)))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(ert-deftest tsx-ts-mode-test-jsx-tag-syntax-propertize ()
  "Every JSX tag's angle brackets must pair with that tag's own bounds.

Make sure jsx element's `<' pair with the closing `>' correctly instead
of the wrong `>' from its attributes contained an arrow function `=>'."
  (skip-unless (treesit-ready-p 'tsx))
  (dolist (src '(;; A fragment, arrow-function attributes, and an arrow
                 ;; that returns a nested `<div></div>' element -- all
                 ;; inside the opening tag's range.
                 "const A = () => (
  <>
    <Component
      onClick={() => {}}
      panel={() => {
        return <div></div>;
      }}
    >
      {children}
    </Component>
  </>
);
"
                 ;; A self-closing element nested inside an attribute
                 ;; expression.
                 "const B = () => (
  <Component slot={<Panel onClick={() => cb()}/>}>
    {children}
  </Component>
);
"))
    (with-temp-buffer
      (insert src)
      (tsx-ts-mode)
      (syntax-propertize (point-max))
      (pcase-dolist (`(,_ . ,node)
                     (treesit-query-capture
                      'tsx '(((jsx_opening_element) @el)
                             ((jsx_closing_element) @el)
                             ((jsx_self_closing_element) @el))))
        (let ((ns (treesit-node-start node))
              (ne (treesit-node-end node)))
          (should (eq (char-after ns) ?<))
          (should (eq (char-before ne) ?>))
          ;; Forward from the tag's `<' lands exactly after its own
          ;; `>', and backward round-trips to the `<'.
          (should (= (scan-sexps ns 1) ne))
          (should (= (scan-sexps ne -1) ns)))))))

(provide 'typescript-ts-mode-tests)
;;; typescript-ts-mode-tests.el ends here
