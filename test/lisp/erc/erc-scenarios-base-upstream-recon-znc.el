;;; erc-scenarios-base-upstream-recon-znc.el --- Bouncer recon scenario -*- lexical-binding: t -*-

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

;; Commentary:

;; These concern the loss and recovery of a proxy's IRC-side
;; connection (hence "upstream").

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-upstream-recon--znc ()
  :tags '(:expensive-test)
  (erc-scenarios-common--upstream-reconnect
   (lambda ()
     (with-current-buffer "*status@foonet"
       (erc-d-t-search-for 1 "Disconnected from IRC")
       (erc-d-t-search-for 1 "Connected!"))
     (with-current-buffer "*status@barnet"
       (erc-d-t-search-for 1 "Disconnected from IRC")
       (erc-d-t-search-for 1 "Connected!")))
   'znc-foonet
   'znc-barnet))

;;; erc-scenarios-base-upstream-recon-znc.el ends here
