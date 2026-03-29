;;; erc-scenarios-base-kill-on-part.el --- killing buffers on part -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;; Assert channel buffer is killed when `erc-kill-buffer-on-part' is
;; enabled and a user issues a /part.  Also assert that code in
;; `erc-kill-channel-hook' can detect when `erc-response-PART' is
;; killing a buffer on behalf of that option.
(ert-deftest erc-scenarios-base-kill-on-part--enabled ()
  :tags '(:expensive-test)
  (should-not erc-kill-buffer-on-part)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reuse-buffers/channel")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (erc-kill-buffer-on-part t)
       (calls nil)
       (erc-part-hook (lambda (b) (push (buffer-name b) calls)))
       (erc-kill-channel-hook
        (cons (lambda () (push erc-killing-buffer-on-part-p calls))
              erc-kill-channel-hook))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#chan"))
      (funcall expect 10 "<alice> bob: Whilst I can shake")
      (erc-scenarios-common-say "/part"))

    (erc-d-t-wait-for 20 (null (get-buffer "#chan")))
    (should (equal calls '(t "#chan")))))

;; When `erc-kill-buffer-on-part' is non-nil, and the parted buffer has
;; already been killed, don't kill the server buffer.  Bug#70840
(ert-deftest erc-scenarios-base-kill-on-part--enabled/killed ()
  :tags '(:expensive-test)
  (should-not erc-kill-buffer-on-part)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reuse-buffers/channel")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (erc-kill-buffer-on-part t)
       (calls nil)
       (erc-part-hook (lambda (b) (push b calls)))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#chan"))
      (funcall expect 10 "<alice> bob: Whilst I can shake")
      (kill-buffer))

    (erc-d-t-wait-for 20 (null (get-buffer "#chan")))
    (erc-d-t-wait-for 10 (equal calls '(nil)))
    (erc-d-t-ensure-for 0.1 (get-buffer "foonet"))))

;;; erc-scenarios-base-kill-on-part.el ends here
