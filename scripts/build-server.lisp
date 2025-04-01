(ql:quickload :lem-server)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem-server"
                          :toplevel #'lem-server:main
                          :executable t)
