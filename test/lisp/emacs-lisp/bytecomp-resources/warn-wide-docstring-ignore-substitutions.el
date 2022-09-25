;;; -*- lexical-binding: t -*-
(defalias 'foo #'ignore
  "None of this should be considered too wide.

; this should be treated as 60 characters - no warning
\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]\\[quit-window]

; 64 * 'x' does not warn
\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'\\`x'

; keymaps are just ignored
\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>\\<foo-bar-map>

\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}\\{foo-bar-map}

bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar
")
