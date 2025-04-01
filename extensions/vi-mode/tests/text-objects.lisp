(defpackage :lem-vi-mode/tests/text-objects
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/text-objects)

(in-readtable :interpol-syntax)

(deftest word-object
  (with-fake-interface ()
    (with-vi-buffer (#?"abc\n  [ ] def\n")
      (cmd "viw")
      (ok (buf= #?"abc\n<   [ ]>def\n")))))

(deftest broad-word-object
  (with-fake-interface ()
    (with-vi-buffer (#?"abc [d]ef.ghi \n")
      (cmd "viW")
      (ok (buf= #?"abc <def.gh[i]> \n"))
      (cmd "vvaW")
      (ok (buf= #?"abc <def.ghi[ ]>\n"))
      )))

(deftest double-quoted
  (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks t)
    (with-fake-interface ()
      (with-vi-buffer ("[ ]\"foo\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" [\"]foo\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"f[o]o\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"foo[\"] \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"foo\"[ ]\"bar\"")
        (cmd "va\"")
        ;; NOTE: This behavior is not same as Vim
        (ok (buf= " \"foo\"< \"bar[\"]>")))
      (with-vi-buffer (" \"foo\" \"bar[\"]")
        (cmd "va\"")
        (ok (buf= " \"foo\"< \"bar[\"]>")))

      ;; Escape Character
      ; v i
      (with-vi-buffer (" \" f[o]o \\\" bar \"")
        (cmd "vi\"")
        (ok (buf= " \"< foo \\\" bar[ ]>\"")))
      (with-vi-buffer (" \" foo \\\" b[a]r \"")
        (cmd "vi\"")
        (ok (buf= " \"< foo \\\" bar[ ]>\"")))
      ; v a
      (with-vi-buffer (" \" f[o]o \\\" bar \"")
        (cmd "va\"")
        (ok (buf= " <\" foo \\\" bar [\"]>")))
      (with-vi-buffer (" \" foo \\\" b[a]r \"")
        (cmd "va\"")
        (ok (buf= " <\" foo \\\" bar [\"]>"))))))

(deftest vi-operator-surrounding-blanks
  (with-fake-interface ()
    (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks nil)
      (with-vi-buffer (" \"f[o]o\"   \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo[\"]>   \"bar\""))))
    (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks t)
      (with-vi-buffer (" \"f[o]o\"   \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"  [ ]>\"bar\""))))))

(deftest paragraph-object
  (with-fake-interface ()
    (with-vi-buffer (#?" \n \n f[o]o \n bar \n \n \n")
      (cmd "vip")
      (ok (buf= #?" \n \n< foo \n bar [\n]> \n \n")))
    (with-vi-buffer (#?" \n \n f[o]o \n bar \n \n \n")
      (cmd "vap")
      (ok (buf= #?" \n \n< foo \n bar \n \n [\n]>")))))

