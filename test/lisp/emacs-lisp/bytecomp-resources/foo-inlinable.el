;; -*- lexical-binding: t; -*-

(defsubst foo-inlineable (foo-var)
  (+ foo-var 2))

(provide 'foo-inlinable)
