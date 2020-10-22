;;; custom--test-theme.el -- A test theme.  -*- lexical-binding:t -*-

(deftheme custom--test
  "A test theme.")

(custom-theme-set-variables
 'custom--test
 '(custom--test-user-option 'bar)
 '(custom--test-variable 'bar))

(provide-theme 'custom--test)
