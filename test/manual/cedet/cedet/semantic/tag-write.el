(defun semantic-tag-write-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-one-tag (semantic-current-tag))))

(defun semantic-tag-write-list-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-tag-list (semantic-fetch-tags))))
