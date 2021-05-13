;;; erc-scenarios-base-reuse-buffers.el --- base-reuse-buffers scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join))

(defun erc-scenarios-common--base-reuse-buffers-server-buffers (&optional more)
  "Show that `erc-reuse-buffers' doesn't affect server buffers.
Overlaps some with `clash-of-chans/uniquify'.  Adapted from
rebuffed/reuseless, described in Bug#48598: 28.0.50; buffer-naming
collisions involving bouncers in ERC.  Run EXTRA."
  (erc-scenarios-common-with-cleanup
      ((dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name)
                         (format "127.0.0.1:%d/127.0.0.1" port)))
        (erc-d-t-search-for 12 "marked as being away")))

    (ert-info ("Connect to barnet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "barnet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name)
                         (format "127.0.0.1:%d/127.0.0.1<2>" port)))
        (erc-d-t-search-for 45 "marked as being away")))

    (erc-d-t-wait-for 2 (get-buffer (format "127.0.0.1:%d/127.0.0.1" port)))
    (erc-d-t-wait-for 2 (get-buffer (format "127.0.0.1:%d/127.0.0.1<2>" port)))

    (ert-info ("Server buffers are unique, no IP-based names")
      (should (cdr (erc-scenarios-common-buflist "127.0.0.1"))))
    (when more (funcall more port))))

;; XXX maybe remove: already covered many times over by other scenarios
(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--enabled ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers))
  (let ((erc-scenarios-common-dialog "base/reuse-buffers/server"))
    (erc-scenarios-common-with-cleanup
        ((dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
         (port (process-contact dumb-server :service))
         erc-autojoin-channels-alist)

      (ert-info ("Connect to foonet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :password "foonet:changeme"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-search-for 12 "marked as being away")))

      (ert-info ("Connect to barnet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :password "barnet:changeme"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-search-for 45 "marked as being away")))

      (erc-d-t-wait-for 2 (get-buffer "foonet"))
      (erc-d-t-wait-for 2 (get-buffer "barnet"))

      (ert-info ("Server buffers are unique, no IP-based names")
        (should-not (eq (get-buffer "foonet") (get-buffer "barnet")))
        (should-not (erc-scenarios-common-buflist "127.0.0.1"))))))

;; FIXME no sense in running this twice (JOIN variant includes this)
(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--disabled ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers)
    (let ((erc-scenarios-common-dialog "base/reuse-buffers/server")
          erc-reuse-buffers)
      (erc-scenarios-common--base-reuse-buffers-server-buffers nil))))

;;; erc-scenarios-base-reuse-buffers.el ends here
