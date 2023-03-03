;;; -*- lexical-binding: t; -*-
(let ((foo nil))
  (run-hook-with-args-until-success 'foo #'next-line))
