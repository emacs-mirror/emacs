;;; x-win.el --- parse relevant switches and set up for Android  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n, android

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

;; This file contains the support for initializing the Lisp side of
;; Android windowing.

;;; Code:


(unless (featurep 'android)
  (error "%s: Loading android-win without having Android"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'fontset)
(require 'dnd)
(require 'touch-screen)

(add-to-list 'display-format-alist '(".*" . android))

;; Window system initialization.  This is extremely simple because all
;; initialization is done in android_term_init.

(cl-defmethod window-system-initialization (&context (window-system android)
                                                     &optional _ignored)
  "Set up the window system.  WINDOW-SYSTEM must be ANDROID.
DISPLAY is ignored on Android."
  ;; Just make sure the window system was initialized at startup.
  (android-get-connection))

(cl-defmethod frame-creation-function (params &context (window-system android))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (_ignored &context (window-system android))
  ;; Nothing to do here: Android has no command line to provide
  ;; arguments on.
  (ignore))

(provide 'android-win)
;; android-win.el ends here.
