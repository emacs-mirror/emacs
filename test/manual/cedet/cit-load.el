;;; cit-load.el --- Configuration when running the integration tests.

;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cit-load.el,v 1.5 2009-10-18 16:15:59 zappo Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Do a setup for running the integration tests.
;;
;; Disable various caches as needed to mimic a CEDET install that
;; is bootstrapping up for the first time.

;;; Code:

(defvar cit-src-dir
  (let ((dir (file-name-directory
	      (or load-file-name (buffer-file-name)))))
    (add-to-list 'load-path dir)
    dir)
  "Src dir to CIT testing suite.")

(setq inhibit-splash-screen t)

;; Fix bug in Emacs 21
(when (< emacs-major-version 22)
  (require 'semantic-c))

;; Disables all caches related to semantic DB so all
;; tests run as if we have bootstrapped CEDET for the
;; first time.
(setq-default semanticdb-new-database-class 'semanticdb-project-database)
(message "Disabling existing Semantic Database Caches.")

;; Disabling the srecoder map, we won't load a pre-existing one
;; and will be forced to bootstrap a new one.
(setq srecode-map-save-file nil)

(require 'cedet-integ-test)

(provide 'cit-load)
;;; cit-load.el ends here
