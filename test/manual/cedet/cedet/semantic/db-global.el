(defvar semanticdb-test-gnu-global-startfile "~/src/global-5.7.3/global/global.c"
  "File to use for testing.")

(defun semanticdb-test-gnu-global (searchfor &optional standardfile)
  "Test the GNU Global semanticdb.
Argument SEARCHFOR is the text to search for.
If optional arg STANDARDFILE is non nil, use a standard file w/ global enabled."
  (interactive "sSearch For Tag: \nP")

  (save-excursion
    (when standardfile
      (save-match-data
        (set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile))))

    (condition-case err
        (semanticdb-enable-gnu-global-in-buffer)
      (error (if standardfile
                 (error err)
               (save-match-data
                 (set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile)))
               (semanticdb-enable-gnu-global-in-buffer))))

    (let* ((db (semanticdb-project-database-global "global"))
           (tab (semanticdb-file-table db (buffer-file-name)))
           (result (semanticdb-deep-find-tags-for-completion-method tab searchfor)))
      (data-debug-new-buffer "*SemanticDB Gnu Global Result*")
      (data-debug-insert-thing result "?" ""))))
