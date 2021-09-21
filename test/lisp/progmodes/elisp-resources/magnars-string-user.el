;;; magnars-string-user.el --- playground file that uses magnars-string.el and shorthands -*- lexical-binding: t; -*-

;; require this library
(require 'magnars-string)

;; can't live without these
(show-paren-mode 1)
(electric-pair-mode 1)

;; will be useful later
(flymake-mode 1)
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; just for geeks, watch the echo area when I eval this
(benchmark-run 1 (elisp--completion-local-symbols))
(intern (symbol-name (gensym)))
(benchmark-run 1 (elisp--completion-local-symbols))

(defun silly ()

  )

;; Things to demo:
;; * C-M-i completion
;; * Eldoc
;; * M-. and then M-,
;; * C-h f
;; * Changing the shorthand, reload with M-x revert-buffer
;; * Flymake

;; Local Variables:
;; elisp-shorthands: (("s-" . "magnars-string-"))
;; End:
