(deftheme custom--test
  "A test theme.")

(custom-theme-set-variables
 'custom--test
 '(custom--test-user-option 'bar)
 '(custom--test-variable 'bar))

(provide-theme 'custom--test)
