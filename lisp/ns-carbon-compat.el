;;; ns-carbon-compat.el --- Carbon compatibility layer for Mac users of NS (Cocoa) GUI.

;; Author: Adrian Robert
;; Keywords: Carbon, MacOSX

;; Add a license if this becomes non-trivial (first year 2008).

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

;; arch-tag: b03b7d78-2b97-4953-90be-5d4f71b64ec1
