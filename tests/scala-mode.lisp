(defpackage :lem-tests/scala-mode
  (:use :cl
        :rove
        :lem-tests/utilities))
(in-package :lem-tests/scala-mode)

(deftest scala-indent-region
  (with-testing-buffer (buffer (lem:find-file-buffer (sample-file "LemScalaIndent.scala")))
    (testing "Test indent region"
      (let ((before (lem:buffer-text buffer))
            (after)
            (is-correct))
        (lem:indent-current-buffer)
        (setq after (lem:buffer-text buffer))
        (setq is-correct (string= before after))
        (ok is-correct
            (if is-correct
                "Indent is correct"
                (diff-text before after)))))))
