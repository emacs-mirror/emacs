(defun pc-bindings-mode ()
  "Set up certain key bindings for PC compatibility.
The keys affected are:
  DELETE (and its variants) delete forward instead of backward.
  HOME and END move to beginning and end of line
  C-HOME and C-END move to beginning and end of buffer."

  (interactive)
  (define-key function-key-map [delete] "\C-d")
  (define-key function-key-map [M-delete] [?\M-\C-d])
  (global-set-key [C-M-delete] 'kill-sexp)

  (global-set-key [home] 'beginning-of-line)
  (global-set-key [end] 'end-of-line)
  (global-set-key [C-home] 'beginning-of-buffer)
  (global-set-key [C-end] 'end-of-buffer))

