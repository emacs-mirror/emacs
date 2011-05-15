(defun semantic-test-all-format-tag-functions (&optional arg)
  "Test all outputs from `semantic-format-tag-functions'.
Output is generated from the function under `point'.
Optional argument ARG specifies not to use color."
  (interactive "P")
  (semantic-fetch-tags)
  (let* ((tag (semantic-current-tag))
         (par (semantic-current-tag-parent))
         (fns semantic-format-tag-functions))
    (with-output-to-temp-buffer "*format-tag*"
      (princ "Tag->format function tests:")
      (while fns
        (princ "\n")
        (princ (car fns))
        (princ ":\n ")
        (let ((s (funcall (car fns) tag par (not arg))))
          (save-excursion
            (set-buffer "*format-tag*")
            (goto-char (point-max))
            (insert s)))
        (setq fns (cdr fns))))
      ))
