; MPW does not allow saving a file with name beginning with a period.
; Use Emacs or SimpleText to edit and save this file instead.

(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)))

(setq default-frame-alist '((font . "fontset-mac")))
