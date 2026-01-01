;;; erc-scenarios-join-display-context.el --- buffer-display autojoin ctx -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

;; This module uses the list `erc-join--requested-channels' to detect
;; whether a JOIN response was likely triggered by an outgoing JOIN
;; emitted on behalf of `erc-autojoin-channels-alist'.  When a related
;; error response, such as a 471, arrives in lieu of a JOIN, the module
;; removes the affected channel's name from the list so that any calls
;; to `display-buffer' in `erc-setup-buffer' on subsequent JOINs aren't
;; misled into thinking the JOIN was module-initiated.
(ert-deftest erc-scenarios-join-display-context--errors ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/buffer-display")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'mode-context))
       (port (process-contact dumb-server :service))
       (calls ())
       (erc-buffer-display
        (lambda (buf action)
          (when (equal (alist-get 'erc-buffer-display action) 'JOIN)
            (when (equal (buffer-name buf) "#chan")
              (should (equal (alist-get 'erc-autojoin-mode action) "#chan")))
            (push (cons buf action) calls)
            (pop-to-buffer buf))))
       (erc-autojoin-channels-alist '((foonet "#chan" "#spam" "#foo")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 10 "Max occupancy for channel #spam exceeded")
        (funcall expect 10 "Channel #foo is invitation only")))

    (ert-info ("New #chan buffer displayed in new window")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (eq (window-buffer) (current-buffer)))
        (should (equal calls `((,(current-buffer) nil
                                (erc-buffer-display . JOIN)
                                (erc-autojoin-mode . "#chan")))))
        (setq calls nil)
        (funcall expect 10 "invites you to channel #foo")
        (funcall expect 10 "#chan was created on")))

    (with-current-buffer "foonet"
      ;; Don't simulate an interactive /join because that overrides the
      ;; display context.
      (erc-cmd-JOIN "#foo"))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#foo"))
      ;; `erc-autojoin-mode' is absent from the action alist.
      (should (equal calls `((,(current-buffer) nil
                              (erc-buffer-display . JOIN))))))))

;;; erc-scenarios-join-display-context.el ends here
