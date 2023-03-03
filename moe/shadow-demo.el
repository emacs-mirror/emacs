;; To see the effect of text shadows, use the command:
;;  emacs -q -l moe/shadow-demo.el

(set-face-attribute 'default nil :shadow 10.0)
(set-face-attribute 'font-lock-function-name-face nil :shadow '(5.0 "red"))
(set-face-attribute 'font-lock-variable-name-face nil :shadow '(5.0 "green"))
