;;; eglot-init.el --- Initialization for eglot-tests.el on emba  -*- lexical-binding:t -*-

(defvar lp
  (directory-files
   (locate-user-emacs-file "elpa") t directory-files-no-dot-files-regexp))
(use-package company :load-path lp)
(use-package yasnippet :load-path lp)

;; This is is needed for pragrams installed by pipx.
(setq exec-path (cons "/root/.local/bin" exec-path))
(setenv "PATH" (concat (getenv "PATH") ":/root/.local/bin"))
