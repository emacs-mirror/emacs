;;; url-ftp.el --- FTP wrapper

;; Copyright (C) 1996, 1997, 1998, 1999, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; We knew not what we did when we overloaded 'file' to mean 'file'
;; and 'ftp' back in the dark ages of the web.
;;
;; This stub file is just here to please the auto-scheme-loading code
;; in url-methods.el and just maps everything onto the code in
;; url-file.

;;; Code:

(require 'url-parse)
(require 'url-file)

(defconst url-ftp-default-port 21 "Default FTP port.")
(defconst url-ftp-asynchronous-p t "FTP transfers are asynchronous.")
(defalias 'url-ftp-expand-file-name 'url-default-expander)
(defalias 'url-ftp 'url-file)

(provide 'url-ftp)

;; arch-tag: 9c3e70c4-350f-4d4a-bb51-a1e9b459e7dc
;;; url-ftp.el ends here
