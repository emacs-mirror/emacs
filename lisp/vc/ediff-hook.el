;;; ediff-hook.el --- setup for Ediff's menus and autoloads  -*- lexical-binding:t -*-

;; Copyright (C) 1995-2026 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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

;;; Code:

;;;   These must be placed in menu-bar.el in Emacs
;;
;;      (define-key menu-bar-tools-menu [ediff-misc]
;;	'("Ediff Miscellanea" . menu-bar-ediff-misc-menu))
;;      (define-key menu-bar-tools-menu [epatch]
;;	'("Apply Patch" . menu-bar-epatch-menu))
;;      (define-key menu-bar-tools-menu [ediff-merge]
;;	'("Merge" . menu-bar-ediff-merge-menu))
;;      (define-key menu-bar-tools-menu [ediff]
;;	'("Compare" . menu-bar-ediff-menu))

;; allow menus to be set up without ediff-wind.el being loaded

;; initialize menu bar keymaps
(defvar menu-bar-ediff-misc-menu
  (make-sparse-keymap "Ediff Miscellanea"))
(fset 'menu-bar-ediff-misc-menu
      menu-bar-ediff-misc-menu)
(defvar-keymap menu-bar-epatch-menu :name "Apply Patch")
(fset 'menu-bar-epatch-menu menu-bar-epatch-menu)
(defvar-keymap menu-bar-ediff-merge-menu :name "Merge")
(fset 'menu-bar-ediff-merge-menu
      menu-bar-ediff-merge-menu)
(defvar-keymap menu-bar-ediff-menu :name "Compare")
(fset 'menu-bar-ediff-menu menu-bar-ediff-menu)

;; define ediff compare menu
(define-key menu-bar-ediff-menu [ediff-misc]
  '(menu-item "Ediff Miscellanea" menu-bar-ediff-misc-menu))
(define-key menu-bar-ediff-menu [separator-ediff-misc] menu-bar-separator)
(define-key menu-bar-ediff-menu [window]
  '(menu-item "This Window and Next Window" compare-windows
              :help "Compare the current window and the next window"))
(define-key menu-bar-ediff-menu [ediff-windows-linewise]
  '(menu-item "Windows Line-by-line..." ediff-windows-linewise
              :help "Compare windows line-wise"))
(define-key menu-bar-ediff-menu [ediff-windows-wordwise]
  '(menu-item "Windows Word-by-word..." ediff-windows-wordwise
              :help "Compare windows word-wise"))
(define-key menu-bar-ediff-menu [separator-ediff-windows] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-regions-linewise]
  '(menu-item "Regions Line-by-line..." ediff-regions-linewise
              :help "Compare regions line-wise"))
(define-key menu-bar-ediff-menu [ediff-regions-wordwise]
  '(menu-item "Regions Word-by-word..." ediff-regions-wordwise
              :help "Compare regions word-wise"))
(define-key menu-bar-ediff-menu [separator-ediff-regions] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-dir-revision]
  '(menu-item "Directory Revisions..." ediff-directory-revisions
              :help "Compare directory files with their older versions"))
(define-key menu-bar-ediff-menu [ediff-revision]
  '(menu-item "File with Revision..." ediff-revision
              :help "Compare file with its older versions"))
(define-key menu-bar-ediff-menu [separator-ediff-directories] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-directories3]
  '(menu-item "Three Directories..." ediff-directories3
              :help "Compare files common to three directories simultaneously"))
(define-key menu-bar-ediff-menu [ediff-directories]
  '(menu-item "Two Directories..." ediff-directories
              :help "Compare files common to two directories simultaneously"))
(define-key menu-bar-ediff-menu [separator-ediff-files] menu-bar-separator)
(define-key menu-bar-ediff-menu [ediff-buffers3]
  '(menu-item "Three Buffers..." ediff-buffers3
              :help "Compare three buffers simultaneously"))
(define-key menu-bar-ediff-menu [ediff-files3]
  '(menu-item "Three Files..." ediff-files3
              :help "Compare three files simultaneously"))
(define-key menu-bar-ediff-menu [ediff-buffers]
  '(menu-item "Two Buffers..." ediff-buffers
              :help "Compare two buffers simultaneously"))
(define-key menu-bar-ediff-menu [ediff-files]
  '(menu-item "Two Files..." ediff-files
              :help "Compare two files simultaneously"))

;; define ediff merge menu
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
  '(menu-item "Directory Revisions with Ancestor..."
              ediff-merge-directory-revisions-with-ancestor
              :help "Merge versions of the files in the same directory by comparing the files with common ancestors"))
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
  '(menu-item "Directory Revisions..." ediff-merge-directory-revisions
              :help "Merge versions of the files in the same directory (without using ancestor information)"))
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
  '(menu-item "Revisions with Ancestor..."
              ediff-merge-revisions-with-ancestor
              :help "Merge versions of the same file by comparing them with a common ancestor"))
(define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
  '(menu-item "Revisions..." ediff-merge-revisions
              :help "Merge versions of the same file (without using ancestor information)"))
(define-key menu-bar-ediff-merge-menu [separator-ediff-merge] menu-bar-separator)
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
  '(menu-item "Directories with Ancestor..."
              ediff-merge-directories-with-ancestor
              :help "Merge files common to a pair of directories by comparing the files with common ancestors"))
(define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
  '(menu-item "Directories..." ediff-merge-directories
              :help "Merge files common to a pair of directories"))
(define-key
  menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] menu-bar-separator)
(define-key
  menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
  '(menu-item "Buffers with Ancestor..." ediff-merge-buffers-with-ancestor
              :help "Merge buffers by comparing their contents with a common ancestor"))
(define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
  '(menu-item "Buffers..." ediff-merge-buffers
              :help "Merge buffers (without using ancestor information)"))
(define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
  '(menu-item "Files with Ancestor..." ediff-merge-files-with-ancestor
              :help "Merge files by comparing them with a common ancestor"))
(define-key menu-bar-ediff-merge-menu [ediff-merge-files]
  '(menu-item "Files..." ediff-merge-files
              :help "Merge files (without using ancestor information)"))

;; define epatch menu
(define-key menu-bar-epatch-menu [ediff-patch-buffer]
  '(menu-item "To a Buffer..." ediff-patch-buffer
              :help "Apply a patch to the contents of a buffer"))
(define-key menu-bar-epatch-menu [ediff-patch-file]
  '(menu-item "To a File..." ediff-patch-file
              :help "Apply a patch to a file"))

;; define ediff miscellanea
(define-key menu-bar-ediff-misc-menu [emultiframe]
  '(menu-item "Use separate control buffer frame"
              ediff-toggle-multiframe
              :help "Switch between the single-frame presentation mode and the multi-frame mode"
              :button (:toggle . (eq (bound-and-true-p ediff-window-setup-function)
		                     #'ediff-setup-windows-multiframe))))
;; FIXME: Port XEmacs's toolbar support!
;; ["Use a toolbar with Ediff control buffer"
;;  ediff-toggle-use-toolbar
;;  :style toggle
;;  :selected (if (featurep 'ediff-tbar)
;;       	 (ediff-use-toolbar-p))]
(define-key menu-bar-ediff-misc-menu [eregistry]
  '(menu-item "List Ediff Sessions" ediff-show-registry
              :help "List all active Ediff sessions; it is a convenient way to find and resume such a session"))
(define-key menu-bar-ediff-misc-menu [ediff-cust]
  '(menu-item "Customize Ediff" ediff-customize
              :help "Change some of the parameters that govern the behavior of Ediff"))
(define-key menu-bar-ediff-misc-menu [ediff-doc]
  '(menu-item "Ediff Manual" ediff-documentation
              :help "Bring up the Ediff manual"))

(provide 'ediff-hook)
;;; ediff-hook.el ends here
