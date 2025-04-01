(defpackage :lem-vi-mode/tests/jumplist
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-vi-mode/jumplist
                :current-jumplist
                :jumplist-history-back
                :jumplist-history-next
                :jumplist-history-push
                :print-jumplist
                :*max-jumplist-size*)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/jumplist)

(in-readtable :interpol-syntax)

(deftest jumplist-go-back-and-forth
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\n")
      (let ((jumplist (current-jumplist)))
        (ok (not (signals (jumplist-history-next jumplist))))
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  3: (1, 0)  NIL"
                          "  2: (2, 0)  NIL"
                          "  1: (3, 0)  NIL"
                          "> 0: NIL")))
        (jumplist-history-back jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  2: (1, 0)  NIL"
                          "  1: (2, 0)  NIL"
                          "> 0: (3, 0)  NIL"
                          "  1: (4, 0)  NIL")))
        (jumplist-history-back jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  1: (1, 0)  NIL"
                          "> 0: (2, 0)  NIL"
                          "  1: (3, 0)  NIL"
                          "  2: (4, 0)  NIL")))
        (jumplist-history-back jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "> 0: (1, 0)  NIL"
                          "  1: (2, 0)  NIL"
                          "  2: (3, 0)  NIL"
                          "  3: (4, 0)  NIL")))
        (ok (not (signals (jumplist-history-back jumplist))))
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "> 0: (1, 0)  NIL"
                          "  1: (2, 0)  NIL"
                          "  2: (3, 0)  NIL"
                          "  3: (4, 0)  NIL")))
        (jumplist-history-next jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  1: (1, 0)  NIL"
                          "> 0: (2, 0)  NIL"
                          "  1: (3, 0)  NIL"
                          "  2: (4, 0)  NIL")))
        (jumplist-history-next jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  2: (1, 0)  NIL"
                          "  1: (2, 0)  NIL"
                          "> 0: (3, 0)  NIL"
                          "  1: (4, 0)  NIL")))
        (jumplist-history-next jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  3: (1, 0)  NIL"
                          "  2: (2, 0)  NIL"
                          "  1: (3, 0)  NIL"
                          "> 0: (4, 0)  NIL")))
        (ok (not (signals (jumplist-history-next jumplist))))))))

(deftest jumplist-delete-newer-history
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\nmno\n")
      (let ((jumplist (current-jumplist)))
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-back jumplist)
        (jumplist-history-back jumplist)
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  1: (1, 0)  NIL"
                          "> 0: (2, 0)  NIL"
                          "  1: (3, 0)  NIL"
                          "  2: (4, 0)  NIL")))
        (jumplist-history-push jumplist (copy-point (buffer-end-point (current-buffer))))
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  5: (1, 0)  NIL"
                          "  4: (2, 0)  NIL"
                          "  3: (3, 0)  NIL"
                          "  2: (4, 0)  NIL"
                          "  1: (6, 0)  NIL"
                          "> 0: NIL")))))))

(deftest jumplist-remove-same-line-points
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\nmno\n")
      (let ((jumplist (current-jumplist)))
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "j")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (cmd "kl")
        (jumplist-history-push jumplist (copy-point (current-point)))
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  2: (2, 0)  NIL"
                          "  1: (1, 1)  NIL"
                          "> 0: NIL")))))))

(deftest jumplist-max-size
  (with-fake-interface ()
    (let ((*max-jumplist-size* 3))
      (with-vi-buffer (#?"[a]bc\ndef\nghi\njkl\n")
        (let ((jumplist (current-jumplist)))
          (jumplist-history-push jumplist (copy-point (current-point)))
          (cmd "j")
          (jumplist-history-push jumplist (copy-point (current-point)))
          (ok (equal (with-output-to-string (s)
                       (print-jumplist jumplist s))
                     (lines "  2: (1, 0)  NIL"
                            "  1: (2, 0)  NIL"
                            "> 0: NIL")))
          (cmd "j")
          (jumplist-history-push jumplist (copy-point (current-point)))
          (ok (equal (with-output-to-string (s)
                       (print-jumplist jumplist s))
                     (lines "  2: (2, 0)  NIL"
                            "  1: (3, 0)  NIL"
                            "> 0: NIL"))))))))

(deftest jumplist-push-current-point-into-jumplist-before-jump-history-back
  (with-fake-interface ()
    (with-vi-buffer (#?"second-location\n[s]tart\n\nfirst-location\n\n\nthird-location")
      (let ((jumplist (current-jumplist)))
        (cmd "2gg")
        (cmd "4gg")
        (cmd "gg")
        (cmd "G")
        (cmd "gg")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  4: (2, 0)  NIL"
                          "  3: (4, 0)  NIL"
                          "  2: (1, 0)  NIL"
                          "  1: (7, 0)  NIL"
                          "> 0: NIL")))
        (cmd "<C-o>")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  2: (2, 0)  NIL"
                          "  1: (4, 0)  NIL"
                          "> 0: (7, 0)  NIL"
                          "  1: (1, 0)  NIL")))))))


(deftest jumplist-go-previous-only-once
  (with-fake-interface ()
    (with-vi-buffer (#?"second-location\n[s]tart\n\nfirst-location\n\n\nthird-location")
      (let ((jumplist (current-jumplist)))
        (cmd "2gg")
        (cmd "4gg")
        (cmd "gg")
        (cmd "G")
        (cmd "gg")
        (cmd "''")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  4: (2, 0)  NIL"
                          "  3: (4, 0)  NIL"
                          "  2: (7, 0)  NIL"
                          "  1: (1, 0)  NIL"
                          "> 0: NIL")))))))

(deftest jumplist-go-previous-twice
  (with-fake-interface ()
    (with-vi-buffer (#?"second-location\n[s]tart\n\nfirst-location\n\n\nthird-location")
      (let ((jumplist (current-jumplist)))
        (cmd "2gg")
        (cmd "4gg")
        (cmd "gg")
        (cmd "G")
        (cmd "gg")
        (cmd "''")
        (cmd "''")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  4: (2, 0)  NIL"
                          "  3: (4, 0)  NIL"
                          "  2: (1, 0)  NIL"
                          "  1: (7, 0)  NIL"
                          "> 0: NIL")))))))

(deftest jumplist-go-previous-while-navigating-history
  (with-fake-interface ()
    (with-vi-buffer (#?"second-location\n[s]tart\n\nfirst-location\n\n\nthird-location")
      (let ((jumplist (current-jumplist)))
        (cmd "2gg")
        (cmd "4gg")
        (cmd "gg")
        (cmd "G")
        (cmd "gg")
        (cmd "<C-o>")
        (cmd "<C-o>")
        (cmd "''")
        (ok (equal (with-output-to-string (s)
                     (print-jumplist jumplist s))
                   (lines "  4: (2, 0)  NIL"
                          "  3: (7, 0)  NIL"
                          "  2: (1, 0)  NIL"
                          "  1: (4, 0)  NIL"
                          "> 0: NIL")))))))
