;;; erc-scenarios-base-split-line.el --- ERC line splitting -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-base-split-line--koi8-r ()
  :tags '(:expensive-test)
  (should (equal erc-split-line-length 440))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/flood")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'koi8-r))
       (erc-encoding-coding-alist '(("#koi8" . cyrillic-koi8)))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#koi8")))

    (with-current-buffer (erc-d-t-wait-for 8 (get-buffer "#koi8"))
      (funcall expect 10 "короче теперь")
      (ert-info ("Message well within `erc-split-line-length'")
        (erc-scenarios-common-say
         (concat
          "короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"))
        (funcall expect 1 "<tester>")
        (funcall expect -0.1 "<tester>"))

      (ert-info ("Message over `erc-split-line-length'")
        (erc-scenarios-common-say
         (concat
          "короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " будет разрыв строки непонятно где"))
        (funcall expect 1 "<tester>")
        (funcall expect 1 "<tester> разрыв")))

    (with-current-buffer "foonet"
      (erc-cmd-QUIT "")
      (funcall expect 10 "finished"))))

(ert-deftest erc-scenarios-base-split-line--ascii ()
  :tags '(:expensive-test)
  (should (equal erc-split-line-length 440))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/flood")
       (msg-432 (string-join (make-list 18 "twenty-three characters") " "))
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'ascii))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#ascii")))

    (with-current-buffer (erc-d-t-wait-for 8 (get-buffer "#ascii"))
      (ert-info ("Message with spaces fits exactly")
        (funcall expect 10 "Welcome")
        (should (= (length (concat msg-432 " 12345678")) 440))
        (erc-scenarios-common-say (concat msg-432 " 12345678"))
        (funcall expect 1 "<tester>")
        ;; Sent in a single go, hence no second <speaker>.
        (funcall expect -0.1 "<tester>")
        (funcall expect 0.1 "12345678"))

      (ert-info ("Message with spaces too long.")
        (erc-scenarios-common-say (concat msg-432 " 123456789"))
        (funcall expect 1 "<tester>")
        ;; Sent in two passes, split at last word.
        (funcall expect 0.1 "<tester> 123456789"))

      (ert-info ("Message sans spaces fits exactly")
        (erc-scenarios-common-say (make-string 440 ?x))
        (funcall expect 1 "<tester>")
        ;; Sent in a single go, hence no second <speaker>.
        (funcall expect -0.1 "<tester>"))

      (ert-info ("Message sans spaces too long.")
        (erc-scenarios-common-say (concat (make-string 440 ?y) "z"))
        (funcall expect 1 "<tester>")
        ;; Sent in two passes, split at last word.
        (funcall expect 0.1 "<tester> z"))

      (ert-info ("Rejected when escape-hatch set")
        (let ((erc--reject-unbreakable-lines t))
          (should-error
           (erc-scenarios-common-say
            (concat
             "https://mail.example.org/verify?token="
             (string-join (make-list 18 "twenty-three_characters") "_")))))))

    (with-current-buffer "foonet"
      (erc-cmd-QUIT "")
      (funcall expect 10 "finished"))))

(ert-deftest erc-scenarios-base-split-line--utf-8 ()
  :tags '(:expensive-test)
  (unless (> emacs-major-version 27)
    (ert-skip "No emojis in Emacs 27"))

  (should (equal erc-split-line-length 440))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/flood")
       (msg-432 (string-join (make-list 18 "twenty-three characters") " "))
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'utf-8))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#utf-8")))

    (with-current-buffer (erc-d-t-wait-for 8 (get-buffer "#utf-8"))
      (funcall expect 10 "Welcome")

      (ert-info ("Message with spaces over `erc-split-line-length'")
        (erc-scenarios-common-say
         (concat
          "короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " короче теперь если по русски написать все четко или все равно"
          " будет разрыв строки непонятно где"
          " будет разрыв строки непонятно где"))
        (funcall expect 1 "<tester> короче")
        (funcall expect 1 "<tester> все")
        (funcall expect 1 "<tester> разрыв")
        (funcall expect 1 "Entirely honour"))

      (ert-info ("Message sans spaces over `erc-split-line-length'")
        (erc-scenarios-common-say
         (concat "話說天下大勢，分久必合，合久必分：周末七國分爭，并入於秦。"
                 "及秦滅之後，楚、漢分爭，又并入於漢。漢朝自高祖斬白蛇而起義，"
                 "一統天下。後來光武中興，傳至獻帝，遂分為三國。推其致亂之由，"
                 "殆始於桓、靈二帝。桓帝禁錮善類，崇信宦官。及桓帝崩，靈帝即位，"
                 "大將軍竇武、太傅陳蕃，共相輔佐。時有宦官曹節等弄權，竇武、陳蕃謀誅之，"
                 "作事不密，反為所害。中涓自此愈橫"))
        (funcall expect 1 "<tester>")
        ;; Sent in two passes, split at last word.
        (funcall expect 0.1 "<tester> 竇武")
        (funcall expect 1 "this prey out"))

      ;; Combining emojis are respected.
      (ert-info ("Message sans spaces over small `erc-split-line-length'")
        (let ((erc-split-line-length 100))
          (erc-scenarios-common-say
           "будет разрыв строки непонятно где🏁🚩🎌🏴🏳️🏳️‍🌈🏳️‍⚧️🏴‍☠️"))
        (funcall expect 1 "<tester>")
        (funcall expect 1 "<tester> 🏳️‍🌈")))

    (with-current-buffer "foonet"
      (erc-cmd-QUIT "")
      (funcall expect 10 "finished"))))

;;; erc-scenarios-base-split-line.el ends here
