;;; erc-scenarios-scrolltobottom.el --- erc-scrolltobottom-mode -*- lexical-binding: t -*-

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

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-goodies)

;; These two actually seem to run fine on Emacs 28, but skip them for
;; now to stay in sync with `erc-scenarios-scrolltobottom--relaxed'.

(ert-deftest erc-scenarios-scrolltobottom--normal ()
  :tags `(:expensive-test ,@(and (getenv "ERC_TESTS_GRAPHICAL")
                                 '(:erc--graphical)))
  (when (version< emacs-version "29") (ert-skip "Times out"))

  (should-not erc-scrolltobottom-all)

  (erc-scenarios-common-scrolltobottom--normal
   (lambda ()
     (ert-info ("New insertion doesn't anchor prompt in other window")
       (let ((w (next-window)))
         ;; We're at prompt but not aligned to bottom.
         (should (>= (window-point w) erc-input-marker))
         (erc-d-t-wait-for 10
             (not (erc-scenarios-common--at-win-end-p w))))))))

(ert-deftest erc-scenarios-scrolltobottom--all ()
  :tags `(:expensive-test ,@(and (getenv "ERC_TESTS_GRAPHICAL")
                                 '(:erc--graphical)))
  (when (version< emacs-version "29") (ert-skip "Times out"))

  (should-not erc-scrolltobottom-all)

  (let ((erc-scrolltobottom-all t))

    (erc-scenarios-common-scrolltobottom--normal
     (lambda ()
       (ert-info ("New insertion anchors prompt in other window")
         (let ((w (next-window)))
           ;; We're at prompt and aligned to bottom.
           (should (>= (window-point w) erc-input-marker))
           (erc-d-t-wait-for 10
               (erc-scenarios-common--at-win-end-p w))
           (erc-d-t-ensure-for 0.5
               (erc-scenarios-common--at-win-end-p w))))))))

;;; erc-scenarios-scrolltobottom.el ends here
