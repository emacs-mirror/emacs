;;; -*- lexical-binding: t; -*-
(let ((foo nil))
  (run-hook-with-args-until-failure 'foo))
