(ert-deftest simple-succeeding-test ()
  (should t))


(ert-deftest simple-failing-test ()
  :expected-result :failed
  (should nil))
