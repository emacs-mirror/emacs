(ql:quickload :lem-sdl2)
(ql:quickload :lem-ncurses)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:main
                          :executable t)
