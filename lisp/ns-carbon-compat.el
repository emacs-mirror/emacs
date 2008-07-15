;;; ns-carbon-compat.el ---
;;;     Carbon compatibility layer for Mac users of NS (Cocoa) GUI.
;;; Copyright (C) 2008 Free Software Foundation, Inc.

;;; Author: Adrian Robert
;;; Keywords: Carbon, MacOSX

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; ns-carbon-compat.el:  this file is loaded from termp/ns-win.el when
;; run on a Mac OS X system.  It sets up a number of aliases and other
;; layers to enable human and machine users (Mac distributions of GNU Emacs)
;; to pretend they are using the Choi/Mitsuharu Carbon port.

;;; Code:

(defvaralias 'mac-allow-anti-aliasing 'ns-antialias-text)
(defvaralias 'mac-command-modifier 'ns-command-modifier)
(defvaralias 'mac-control-modifier 'ns-control-modifier)
(defvaralias 'mac-option-modifier 'ns-option-modifier)
(defvaralias 'mac-function-modifier 'ns-function-modifier)
