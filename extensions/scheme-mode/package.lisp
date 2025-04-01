(defpackage :lem-scheme-mode
  (:use :cl
        :lem
        :lem/completion-mode
        :lem/language-mode
        :lem-scheme-mode.errors
        :lem-scheme-mode.swank-protocol)
  (:export
   ;; scheme-mode.lisp
   :scheme-mode
   :*scheme-mode-keymap*
   :*scheme-mode-hook*
   :*use-scheme-process*
   :*scheme-run-command*
   :*scheme-load-command*
   :*use-scheme-slime*
   :*use-scheme-set-library*
   :*use-scheme-autodoc*
   :*scheme-swank-server-run-command*
   :*use-scheme-repl-shortcut*
   :*scheme-completion-names*
   :scheme-keyword-data
   :scheme-beginning-of-defun
   :scheme-end-of-defun
   :scheme-indent-sexp
   ;; swank-connection
   :scheme-slime-connect
   :scheme-slime
   ;; eval.lisp
   :scheme-kill-process
   ;; repl.lisp
   :scheme-repl-mode
   :scheme-repl-input-mode
   :*scheme-repl-mode-keymap*
   :*scheme-repl-input-mode-keymap*
   :start-scheme-repl
   :scheme-switch-to-repl-buffer
   :scheme-eval-or-newline
   :scheme-eval-last-expression
   :scheme-eval-region
   :scheme-load-file
   :scheme-repl-shortcut))
