;;; igc-tests.el --- tests for src/igc.c -*- lexical-binding: t -*-

(require 'ert)

(ert-deftest set-commit-limit-test ()
  (should (equal (igc--set-commit-limit (ash 1 30)) nil))
  (should (equal (assoc-string "commit-limit" (igc-info))
                 '("commit-limit" 1 1073741824 0)))
  (should-error (igc--set-commit-limit -1)
                :type 'args-out-of-range)
  (should-error (igc--set-commit-limit (- (ash 1 64) 1))
                :type 'args-out-of-range)
  (should (equal (igc--set-commit-limit nil) nil))
  (should (equal (assoc-string "commit-limit" (igc-info))
                 '("commit-limit" 1 -1 0))))
