;; To see the effect of text shadows, use the command:
;;  emacs -q -l moe/shadow-demo.el

(set-face-attribute 'font-lock-function-name-face nil :shadow '(1.5 . "red"))
(set-face-attribute 'font-lock-variable-name-face nil :shadow '(1.5 . "green"))
(set-face-attribute 'font-lock-keyword-face nil :shadow '1.0)
(set-face-attribute 'font-lock-type-face nil :shadow '1.0)
