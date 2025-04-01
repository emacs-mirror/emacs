(defpackage :lem-vi-mode/tests/kbdmacro
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-vi-mode/commands
                :*kbdmacro-recording-register*
                :*last-recorded-macro*)
  (:import-from :lem-vi-mode/registers
                :register)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/kbdmacro)

(in-readtable :interpol-syntax)

(deftest vi-record-macro
  (with-fake-interface ()
    (with-vi-buffer ("")
      (ok (signals (cmd "q<C-g>") 'editor-abort))
      (ok (not *kbdmacro-recording-register*))
      (ok (signals (cmd "q<Escape>") 'editor-abort))
      (ok (not *kbdmacro-recording-register*))
      (ok (signals (cmd "q#") 'editor-error))
      (cmd "qa")
      (ok (char= *kbdmacro-recording-register* #\a))
      (cmd "q")
      (ok (null *kbdmacro-recording-register*))
      (ok (char= *last-recorded-macro* #\a))
      (cmd "qA")
      (ok (char= *kbdmacro-recording-register* #\A))
      (cmd "q")
      (ok (char= *last-recorded-macro* #\a))
      (cmd "q1")
      (ok (char= *kbdmacro-recording-register* #\1))
      (cmd "q")
      (ok (char= *last-recorded-macro* #\1)))
    (with-vi-buffer (#?"[d]efine-command\ndefine-command\ndefine-command\n")
      (setf (register #\a) nil)
      (cmd "qa")
      (cmd "^dt-")
      (cmd "q")
      (ok (= 4 (length (register #\a))))
      (ok (buf= #?"[-]command\ndefine-command\ndefine-command\n"))
      (cmd "qA")
      (cmd "iset<Escape>j")
      (cmd "q")
      (ok (= 10 (length (register #\a))))
      (ok (buf= #?"set-command\nde[f]ine-command\ndefine-command\n"))
      (cmd "@a")
      (ok (buf= #?"set-command\nset-command\nde[f]ine-command\n")))
    (with-vi-buffer (#?"[d]efine-command\ndefine-command\ndefine-command\n")
      (cmd "Q")
      (ok (buf= #?"set-command\nde[f]ine-command\ndefine-command\n"))
      (cmd "2Q")
      (ok (buf= #?"set-command\nset-command\nset-command\n[]"))
      (cmd "u")
      (ok (text= #?"set-command\ndefine-command\ndefine-command\n")))
    (with-vi-buffer (#?"[d]efine-command\ndefine-command\ndefine-command\n")
      (handler-case
          (cmd "10@a")
        (end-of-buffer ()))
      (ok (buf= #?"set-command\nset-command\nset-command\nset[]")))))
