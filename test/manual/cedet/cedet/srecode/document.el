;;;###autoload
(defun srecode-document-function-comment-extract-test ()
  "Test old comment extraction.
Dump out the extracted dictionary."
  (interactive)

  (srecode-load-tables-for-mode major-mode)
  (srecode-load-tables-for-mode major-mode 'document)

  (if (not (srecode-table))
      (error "No template table found for mode %s" major-mode))

  (let* ((temp (srecode-template-get-table (srecode-table)
                                           "function-comment"
                                           "declaration"
                                           'document))
         (fcn-in (semantic-current-tag)))

    (if (not temp)
        (error "No templates for function comments"))

    ;; Try to figure out the tag we want to use.
    (when (or (not fcn-in)
             (not (semantic-tag-of-class-p fcn-in 'function)))
      (error "No tag of class 'function to insert comment for"))

    (let ((lextok (semantic-documentation-comment-preceeding-tag fcn-in 'lex))
          )

      (when (not lextok)
        (error "No comment to attempt an extraction"))

      (let ((s (semantic-lex-token-start lextok))
            (e (semantic-lex-token-end lextok))
            (extract nil))

        (pulse-momentary-highlight-region s e)

        ;; Extract text from the existing comment.
        (setq extract (srecode-extract temp s e))

        (with-output-to-temp-buffer "*SRECODE DUMP*"
          (princ "EXTRACTED DICTIONARY FOR ")
          (princ (semantic-tag-name fcn-in))
          (princ "\n--------------------------------------------\n")
          (srecode-dump extract))
        ))))
