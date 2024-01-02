;;; erc-scenarios-join-netid-recon.el --- join-netid-recon scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(ert-deftest erc-scenarios-join-netid--recon ()
  :tags '(:expensive-test)
  (let ((connect (lambda ()
                   (with-current-buffer "foonet"
                     (erc-cmd-RECONNECT)
                     (should (eq (current-buffer)
                                 (process-buffer erc-server-process)))
                     (current-buffer)))))
    (erc-scenarios-common--join-network-id connect nil nil)))

;;; erc-scenarios-join-netid-recon.el ends here
