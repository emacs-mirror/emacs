;;; newcomers-presets-theme.el --- Theme of user options for newcomers  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>

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

;;; Commentary

;; This theme configures user options that we can reasonably expect the
;; average, new user to want to enable, but would otherwise be unlikely
;; to discover on their own.  This includes support for convenience
;; features, adjustment of default settings that are in place for
;; historical reasons, aiding discoverability (at the potential cost of
;; more visual noise) and trying and follow common conventions that
;; other editors have established over the past decades.

;; The goal is to help providing a better starting point for users who
;; would otherwise feel overwhelmed when first starting to use Emacs,
;; without having to burden existing users with invasive changes to
;; Emacs' default behavior.  Options in the theme should NOT hinder
;; developing a better understanding of Emacs (e.g. enabling emulation
;; modes) or make opinionated cosmetic changes.

;;; Code:

;; We define a `newcomers-presets-mode' that we can use to execute custom code
;; that we cannot express by setting existing users options.

;; FIXME: Themes should be able to define these kinds of mode-specific
;; customizations.
(defvar newcomers-presets-mode-enabled-local-modes
  `((prog-mode-hook ,#'display-line-numbers-mode
                    ,#'flymake-mode
                    ,#'flyspell-prog-mode)

    (text-mode-hook ,#'display-line-numbers-mode
                    ,#'flyspell-mode))
  "Alist mapping hooks to a list of function to add to the hook.
The functions are added to the corresponding hooks when enabling
`newcomers-presets-mode', and removed when disabling the mode.")

(defconst newcomers-presets--dnt-prop
  (make-symbol "newcomers-presets-no-not-touch")
  "A fresh constant to use as a symbol property.
The symbol property is used by `newcomers-presets-mode'")

;;;###autoload
(define-minor-mode newcomers-presets-mode
  "A minor mode associated with the `newcomers-presets' theme.
This minor mode will enable and disable the theme on startup."
  :global t
  (cl-letf (((symbol-function 'newcomers-presets-mode)
             ;; As the theme enables/disables this mode we have to
             ;; prevent in indefinite mutual recursion to allow
             ;; `newcomers-presets-mode' to enable the `newcomers-presets' theme and
             ;; vice versa.
             #'ignore))
    (if newcomers-presets-mode
        (enable-theme 'newcomers-presets)
      (disable-theme 'newcomers-presets)))
  ;; TODO: extend `custom-theme-set-variables' to support function local
  ;; hooks.
  (pcase-dolist (`(,hook . ,fns) newcomers-presets-mode-enabled-local-modes)
    (dolist (fn fns)
      (cond
       (newcomers-presets-mode
        ;; We check if a function is already in the hook, to avoid
        ;; removing it later if the user disables the theme.
        (when (run-hook-wrapped hook (lambda (ent &rest _) (eq fn ent)))
          (push fn (get hook newcomers-presets--dnt-prop)))
        (add-hook hook fn))
       (t
        (unless (memq fn (get hook newcomers-presets--dnt-prop))
          (remove-hook hook fn))
        (put hook newcomers-presets--dnt-prop '()))))))

;;;###theme-autoload
(deftheme newcomers-presets
  "Theme of user options settings interesting for newcomers."
  :kind 'user-options)

(custom-theme-set-variables
 'newcomers-presets

 '(newcomers-presets-mode t)

;;;; Appearance-related options
 '(font-use-system-font t)
 '(frame-resize-pixelwise t)
 '(window-resize-pixelwise t)
 '(mode-line-compact 'long)

;;;; Mouse-related options
 '(context-menu-mode t)
 '(save-interprogram-paste-before-kill t)
 '(mouse-yank-at-point t)
 '(pixel-scroll-mode t)
 ;; '(pixel-scroll-precision-mode t) ;; see bug#69972
 '(mouse-drag-and-drop-region t)
 '(mouse-drag-and-drop-region-cross-program t)
 '(mouse-drag-mode-line-buffer t)
 '(global-xref-mouse-mode t)

;;;; Persistence-related options
 '(savehist-mode t)
 '(save-place-mode t)
 '(recentf-mode t)

;;;; Editing-related options
 '(electric-pair-mode t)
 '(repeat-mode t)
 '(delete-selection-mode t)
 '(editorconfig-mode t)
 '(indent-tabs-mode nil)
 '(imenu-auto-rescan t)
 '(view-read-only t)
 '(column-number-mode t)

;;;; Directory managment-related options
 '(dired-auto-revert-buffer t)
 '(dired-mouse-drag-files t)
 '(shell-command-prompt-show-cwd t)

;;;; File-related options
 '(etags-regen-mode t)
 '(vc-auto-revert-mode t)
 '(vc-deduce-backend-nonvc-modes t)
 '(vc-dir-save-some-buffers-on-revert t)
 '(vc-find-revision-no-save t)
 '(vc-follow-symlinks t)
 '(vc-use-incoming-outgoing-prefixes t)

;;;; Completion-related options
 '(minibuffer-visible-completions t)
 '(completions-detailed t)
 '(completions-group t)
 '(completion-auto-select 'second-tab)
 '(completion-eager-update t)
 '(completion-styles '(basic emacs22 flex))
 '(global-completion-preview-mode t)
 '(tab-always-indent 'complete)
 '(which-key-mode t)

;;;; Package-related options
 '(package-autosuggest-mode t)
 '(package-menu-use-current-if-no-marks nil)

;;;; Frame- and window-related options
 '(frame-inhibit-implied-resize t)
 '(tab-bar-history-mode t)
 '(tab-bar-show 0)

;;;; Programming-related options
 '(compilation-scroll-output 'first-error))

(provide-theme 'newcomers-presets)
;;; newcomers-presets-theme.el ends here
