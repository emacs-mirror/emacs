;;; -*- lexical-binding: t; -*-
(let ((foo nil))
  (remove-hook 'foo #'next-line)
  foo)
