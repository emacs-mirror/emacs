;;; ethiopic.el --- Support for Ethiopic

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Ethiopic

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(define-ccl-program ccl-encode-ethio-font
  '(0
    ;; In:  R0:ethio (not checked)
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
    ((r1 -= 33)
     (r2 -= 33)
     (r1 *= 94)
     (r2 += r1)
     (if (r2 < 256) (r1 = 0) ((r2 -= 256) (r1 = 1)))))
  "CCL program to encode an Ehitopic code to code point of Ehitopic font.")

(setq font-ccl-encoder-alist
      (cons (cons "ethio" ccl-encode-ethio-font) font-ccl-encoder-alist))

(register-input-method
 "Ethiopic" '("quail-ethio" quail-use-package "quail/ethio"))

(defun setup-ethio-environment ()
  (setq primary-language "Ethiopic")

  (setq default-input-method '("Ethiopic" . "quail-ethio"))

  ;;
  ;;  key bindings
  ;;
  (define-key global-map [f4] 'sera-to-fidel-buffer)
  (define-key global-map [S-f4] 'sera-to-fidel-region)
  (define-key global-map [C-f4] 'sera-to-fidel-marker)
  (define-key global-map [f5] 'fidel-to-sera-buffer)
  (define-key global-map [S-f5] 'fidel-to-sera-region)
  (define-key global-map [C-f5] 'fidel-to-sera-marker)
  (define-key global-map [f6] 'ethio-modify-vowel)
  (define-key global-map [f7] 'ethio-replace-space)
  (define-key global-map [S-f2] 'ethio-replace-space) ; as requested
  (define-key global-map [f8] 'ethio-input-special-character)

  (add-hook
   'rmail-mode-hook
   '(lambda ()
      (define-key rmail-mode-map [C-f4] 'sera-to-fidel-mail)
      (define-key rmail-mode-map [C-f5] 'fidel-to-sera-mail)))

  (add-hook
   'mail-mode-hook
   '(lambda ()
      (define-key mail-mode-map [C-f4] 'sera-to-fidel-mail)
      (define-key mail-mode-map [C-f5] 'fidel-to-sera-mail)))
  )

(set-language-info-alist
 "Ethiopic" '((setup-function . setup-ethio-environment)
	      (charset . (ethiopic))
	      (documentation . t)
	      (sample-text
	       . "Amharic ($(2"S!,!6!l(B) $(2#Q!$!.(B, Tigrigna ($(2!V#>!6!l(B) $(2"C!$!,!V(B")))

;;; ethiopic.el ends here
