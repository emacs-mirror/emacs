(defun pc-bindings-mode ()
  "Set up certain key bindings for PC compatibility.
The keys affected are:
Delete (and its variants) delete forward instead of backward.
C-Backspace kills backward a word (as C-Delete normally would).
M-Backspace does undo.
Home and End move to beginning and end of line
C-Home and C-End move to beginning and end of buffer.
C-Escape does list-buffers."

  (interactive)
  (define-key function-key-map [delete] "\C-d")
  (define-key function-key-map [M-delete] [?\M-d])
  (define-key function-key-map [C-delete] [?\M-d])
  (global-set-key [C-M-delete] 'kill-sexp)
  (global-set-key [C-backspace] 'backward-kill-word)
  (global-set-key [M-backspace] 'undo)

  (global-set-key [C-escape] 'list-buffers)

  (global-set-key [home] 'beginning-of-line)
  (global-set-key [end] 'end-of-line)
  (global-set-key [C-home] 'beginning-of-buffer)
  (global-set-key [C-end] 'end-of-buffer))

