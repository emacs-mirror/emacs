;;; -*- lexical-binding: t -*-
(defun foo-bar ()
  "This should not warn:
(fn COMMAND &rest ARGS &key (MARGIN (rx bol (+ \" \"))) (ARGUMENT (rx \"-\" (+ (any \"-\" alnum)) (32 \"=\"))) (METAVAR (rx (32 \" \") (or (+ (any alnum \"_-\")) (seq \"[\" (+? nonl) \"]\") (seq \"<\" (+? nonl) \">\") (seq \"{\" (+? nonl) \"}\")))) (SEPARATOR (rx \", \" symbol-start)) (DESCRIPTION (rx (* nonl) (* \"\\=\\n\" (>= 9 \" \") (* nonl)))) NARROW-START NARROW-END)")
