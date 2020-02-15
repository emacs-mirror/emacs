;;; flow-fill-tests.el --- Tests for flow-fill.el  -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'flow-fill)

(ert-deftest fill-flow-tests-fill-flowed-decode ()
  (let ((input
         (concat
          "> Thou villainous ill-breeding spongy dizzy-eyed \n"
          "> reeky elf-skinned pigeon-egg! \n"
          ">> Thou artless swag-bellied milk-livered \n"
          ">> dismal-dreaming idle-headed scut!\n"
          ">>> Thou errant folly-fallen spleeny reeling-ripe \n"
          ">>> unmuzzled ratsbane!\n"
          ">>>> Henceforth, the coding style is to be strictly \n"
          ">>>> enforced, including the use of only upper case.\n"
          ">>>>> I've noticed a lack of adherence to the coding \n"
          ">>>>> styles, of late.\n"
          ">>>>>> Any complaints?\n"))
        (output
         (concat
          "> Thou villainous ill-breeding spongy dizzy-eyed reeky elf-skinned\n"
          "> pigeon-egg! \n"
          ">> Thou artless swag-bellied milk-livered dismal-dreaming idle-headed\n"
          ">> scut!\n"
          ">>> Thou errant folly-fallen spleeny reeling-ripe unmuzzled ratsbane!\n"
          ">>>> Henceforth, the coding style is to be strictly enforced,\n"
          ">>>> including the use of only upper case.\n"
          ">>>>> I've noticed a lack of adherence to the coding styles, of late.\n"
          ">>>>>> Any complaints?\n"))
        (fill-flowed-display-column 69))
    (with-temp-buffer
      (insert input)
      (fill-flowed)
      (message "foo")
      (should (equal (buffer-string) output)))))

(ert-deftest fill-flow-tests-fill-flowed-encode ()
  (let ((input
         (concat
          "> Thou villainous ill-breeding spongy dizzy-eyed \n"
          "> reeky elf-skinned pigeon-egg! \n"
          ">> Thou artless swag-bellied milk-livered \n"
          ">> dismal-dreaming idle-headed scut!\n"
          ">>> Thou errant folly-fallen spleeny reeling-ripe \n"
          ">>> unmuzzled ratsbane!\n"
          ">>>> Henceforth, the coding style is to be strictly \n"
          ">>>> enforced, including the use of only upper case.\n"
          ">>>>> I've noticed a lack of adherence to the coding \n"
          ">>>>> styles, of late.\n"
          ">>>>>> Any complaints?\n"))
        (output
         (concat
          "> Thou villainous ill-breeding spongy dizzy-eyed \n"
          "> reeky elf-skinned pigeon-egg! \n"
          ">> Thou artless swag-bellied milk-livered \n"
          ">> dismal-dreaming idle-headed scut!\n"
          ">>> Thou errant folly-fallen spleeny reeling-ripe \n"
          ">>> unmuzzled ratsbane!\n"
          ">>>> Henceforth, the coding style is to be strictly \n"
          ">>>> enforced, including the use of only upper case.\n"
          ">>>>> I've noticed a lack of adherence to the coding \n"
          ">>>>> styles, of late.\n"
          ">>>>>> Any complaints?\n"))
        (fill-flowed-display-column 69))
    (with-temp-buffer
      (insert input)
      (fill-flowed-encode)
      (should (equal (buffer-string) output)))))

(ert-deftest fill-flow-tests-fill-flowed-stuffed ()
  (let ((input
         (concat
          " > From space-stuffed with a \n"
          "continuation.\n"))
        (output
         "> From space-stuffed with a continuation.\n")
        (fill-flowed-display-column 69))
    (with-temp-buffer
      (insert input)
      (fill-flowed)
      (should (equal (buffer-string) output)))))

(provide 'flow-fill-tests)
;;; flow-fill-tests.el ends here
