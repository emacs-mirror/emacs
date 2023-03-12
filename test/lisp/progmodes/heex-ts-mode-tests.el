(require 'ert)
(require 'ert-x)
(require 'treesit)

(ert-deftest heex-ts-mode-test-indentation ()
  (skip-unless (treesit-ready-p 'heex))
  (ert-test-erts-file (ert-resource-file "indent.erts")))

(provide 'heex-ts-mode-tests)
