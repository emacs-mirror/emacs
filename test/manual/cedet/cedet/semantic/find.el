(defun semantic-find-benchmark ()
  "Run some simple benchmarks to see how we are doing.
Optional argument ARG is the number of iterations to run."
  (interactive)
  (require 'benchmark)
  (let ((f-name nil)
        (b-name nil)
        (f-comp)
        (b-comp)
        (f-regex)
        )
    (garbage-collect)
    (setq f-name
          (benchmark-run-compiled
           1000 (semantic-find-first-tag-by-name "class3"
                                                 "test/test.cpp")))
    (garbage-collect)
    (setq b-name
          (benchmark-run-compiled
              1000 (semantic-brute-find-first-tag-by-name "class3"
                                                          "test/test.cpp")))
    (garbage-collect)
    (setq f-comp
          (benchmark-run-compiled
              1000 (semantic-find-tags-for-completion "method"
                                                      "test/test.cpp")))
    (garbage-collect)
    (setq b-comp
          (benchmark-run-compiled
              1000 (semantic-brute-find-tag-by-name-regexp "^method"
                                                           "test/test.cpp")))
    (garbage-collect)
    (setq f-regex
          (benchmark-run-compiled
              1000 (semantic-find-tags-by-name-regexp "^method"
                                                      "test/test.cpp")))

    (message "Name [new old] [ %.3f %.3f ] Complete [newc/new old] [ %.3f/%.3f %.3f ]"
             (car f-name) (car b-name)
             (car f-comp) (car f-regex)
             (car b-comp))
  ))
