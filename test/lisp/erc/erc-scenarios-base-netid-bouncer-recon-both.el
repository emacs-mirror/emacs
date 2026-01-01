;;; erc-scenarios-base-netid-bouncer-recon-both.el --- net-id both scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.
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

(require 'erc-scenarios-common)

(ert-deftest erc-scenarios-base-netid-bouncer--recon-both ()
  :tags '(:expensive-test)
  (let ((erc-server-reconnect-function #'erc-server-delayed-reconnect))
    (erc-scenarios-common--base-network-id-bouncer--reconnect 'foo-id
                                                              'bar-id)))

;;; erc-scenarios-base-netid-bouncer-recon-both.el ends here
