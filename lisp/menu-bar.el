(define-key global-map [menu-bar] (make-sparse-keymap "menu-bar"))
(setq menu-bar-file-menu (make-sparse-keymap "File"))
(define-key global-map [menu-bar file] menu-bar-file-menu)
(setq menu-bar-edit-menu (make-sparse-keymap "Edit"))
(define-key global-map [menu-bar edit] menu-bar-edit-menu)
(setq menu-bar-buffer-menu (make-sparse-keymap "Buffer"))
(define-key global-map [menu-bar buffer] menu-bar-buffer-menu)
(setq menu-bar-help-menu (make-sparse-keymap "Help"))
(define-key global-map [menu-bar help] menu-bar-help-menu)

(define-key menu-bar-file-map [new-frame] '("New Frame" . new-frame))
(define-key menu-bar-file-map [open-file] '("Open File..." . find-file))
(define-key menu-bar-file-map [save-buffer] '("Save Buffer" . save-buffer))
(define-key menu-bar-file-map [write-file]
  '("Save Buffer As..." . write-file))
(define-key menu-bar-file-map [revert-buffer]
  '("Revert Buffer" . revert-buffer))
(define-key menu-bar-file-map [print-buffer] '("Print Buffer" . print-buffer))
(define-key menu-bar-file-map [delete-frame] '("Delete Frame" . delete-frame))
(define-key menu-bar-file-map [kill-buffer]
  '("Kill Buffer" . kill-this-buffer))
(define-key menu-bar-file-map [exit-emacs]
  '("Exit Emacs" . save-buffers-kill-emacs))

(define-key menu-bar-edit-map [undo] '("Undo" . advertised-undo))
(define-key menu-bar-edit-map [cut] '("Cut" . x-kill-primary-selection))
(define-key menu-bar-edit-map [copy] '("Copy" . x-copy-primary-selection))
(define-key menu-bar-edit-map [paste] '("Paste" . x-yank-clipboard-selection))
(define-key menu-bar-edit-map [clear] '("Clear" . x-delete-primary-selection))

(define-key menu-bar-help-map [info] '("Info" . info))
(define-key menu-bar-help-map [describe-mode]
  '("Describe Mode" . describe-mode))
(define-key menu-bar-help-map [command-apropos]
  '("Command Apropos..." . command-apropos))
(define-key menu-bar-help-map [list-keybindings]
  '("List Keybindings" . describe-bindings))
(define-key menu-bar-help-map [describe-key]
  '("Describe Key..." . describe-key))
(define-key menu-bar-help-map [describe-function]
  '("Describe Function..." . describe-function))
(define-key menu-bar-help-map [describe-variable]
  '("Describe Variable..." . describe-variable))
(define-key menu-bar-help-map [man] '("Man..." . manual-entry))
(define-key menu-bar-help-map [emacs-tutorial]
  '("Emacs Tutorial" . help-with-tutorial))
(define-key menu-bar-help-map [emacs-news] '("Emacs News" . view-emacs-news))


(defun kill-this-buffer ()	; for the menubar
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(put 'save-buffer 'menu-enable '(buffer-modified-p))
(put 'revert-buffer 'menu-enable 'buffer-file-name)
(put 'delete-frame 'menu-enable '(null (cdr (visible-frame-list)))))
(put 'x-kill-primary-selection 'menu-enable '(x-selection-owner-p))
(put 'x-copy-primary-selection 'menu-enable '(x-selection-owner-p))
(put 'x-yank-clipboard-selection 'menu-enable '(x-selection-owner-p))
(put 'x-delete-primary-selection 'menu-enable
     '(x-selection-exists-p 'CLIPBOARD))
(put 'advertised-undo 'menu-enable
     '(and (not (eq t buffer-undo-list))
	   (if (eq last-command 'undo)
	       (setq undoing-more
		     (and (boundp 'pending-undo-list)
			  pending-undo-list)
		     buffer-undo-list))))