;;; erc-scenarios-join-display-context.el --- buffer-display autojoin ctx -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-join-display-context--errors ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/buffer-display")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'mode-context))
       (port (process-contact dumb-server :service))
       (erc-buffer-display (lambda (buf action)
                             (when (equal
                                    (alist-get 'erc-autojoin-mode action)
                                    "#chan")
                               (pop-to-buffer buf))))
       (erc-autojoin-channels-alist '((foonet "#chan" "#spam" "#foo")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        ;; FIXME test for effect rather than inspecting interval variables.
        (erc-d-t-wait-for 10 (equal erc-join--requested-channels
                                    '("#foo" "#spam" "#chan")))
        (funcall expect 10 "Max occupancy for channel #spam exceeded")
        (funcall expect 10 "Channel #foo is invitation only")))

    (ert-info ("New #chan buffer displayed in new window")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (eq (window-buffer) (current-buffer)))
        (funcall expect 10 "#chan was created on")))

    ;; FIXME find a less dishonest way to do this than inspecting
    ;; interval variables.
    (ert-info ("Ensure channels no longer tracked")
      (should-not erc-join--requested-channels))))

;;; erc-scenarios-join-display-context.el ends here
