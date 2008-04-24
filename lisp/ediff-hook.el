;;; ediff-hook.el --- setup for Ediff's menus and autoloads

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>

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

;; Compiler pacifier
(defvar ediff-menu)
(defvar ediff-merge-menu)
(defvar epatch-menu)
(defvar ediff-misc-menu)
;; end pacifier

;; allow menus to be set up without ediff-wind.el being loaded
(defvar ediff-window-setup-function)

;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
;;;###autoload
(if (featurep 'xemacs)
    (progn
      (defun ediff-xemacs-init-menus ()
	(when (featurep 'menubar)
	  (add-submenu
	   '("Tools") ediff-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") ediff-merge-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") epatch-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") ediff-misc-menu "OO-Browser...")
	  (add-menu-button
	   '("Tools") "-------" "OO-Browser...")
	  ))
      (defvar ediff-menu
	'("Compare"
	  ["Two Files..."  ediff-files t]
	  ["Two Buffers..." ediff-buffers t]
	  ["Three Files..."  ediff-files3 t]
	  ["Three Buffers..." ediff-buffers3 t]
	  "---"
	  ["Two Directories..." ediff-directories t]
	  ["Three Directories..." ediff-directories3 t]
	  "---"
	  ["File with Revision..."  ediff-revision t]
	  ["Directory Revisions..."  ediff-directory-revisions t]
	  "---"
	  ["Windows Word-by-word..." ediff-windows-wordwise t]
	  ["Windows Line-by-line..." ediff-windows-linewise t]
	  "---"
	  ["Regions Word-by-word..." ediff-regions-wordwise t]
	  ["Regions Line-by-line..." ediff-regions-linewise t]
	  ))
      (defvar ediff-merge-menu
	'("Merge"
	  ["Files..."  ediff-merge-files t]
	  ["Files with Ancestor..." ediff-merge-files-with-ancestor t]
	  ["Buffers..."  ediff-merge-buffers t]
	  ["Buffers with Ancestor..."
	   ediff-merge-buffers-with-ancestor t]
	  "---"
	  ["Directories..."  ediff-merge-directories t]
	  ["Directories with Ancestor..."
	   ediff-merge-directories-with-ancestor t]
	  "---"
	  ["Revisions..."  ediff-merge-revisions t]
	  ["Revisions with Ancestor..."
	   ediff-merge-revisions-with-ancestor t]
	  ["Directory Revisions..." ediff-merge-directory-revisions t]
	  ["Directory Revisions with Ancestor..."
	   ediff-merge-directory-revisions-with-ancestor t]
	  ))
      (defvar epatch-menu
	'("Apply Patch"
	  ["To a file..."  ediff-patch-file t]
	  ["To a buffer..." ediff-patch-buffer t]
	  ))
      (defvar ediff-misc-menu
	'("Ediff Miscellanea"
	  ["Ediff Manual" ediff-documentation t]
	  ["Customize Ediff" ediff-customize t]
	  ["List Ediff Sessions" ediff-show-registry t]
	  ["Use separate frame for Ediff control buffer"
	   ediff-toggle-multiframe
	   :style toggle
	   :selected (if (and (featurep 'ediff-util)
			      (boundp 'ediff-window-setup-function))
			 (eq ediff-window-setup-function
			     'ediff-setup-windows-multiframe))]
	  ["Use a toolbar with Ediff control buffer"
	   ediff-toggle-use-toolbar
	   :style toggle
	   :selected (if (featurep 'ediff-tbar)
			 (ediff-use-toolbar-p))]))
      
      ;; put these menus before Object-Oriented-Browser in Tools menu
      (if (and (featurep 'menubar) (not (featurep 'infodock))
	       (not (featurep 'ediff-hook)))
	  (ediff-xemacs-init-menus)))
  ;; Emacs
  ;; initialize menu bar keymaps
  (defvar menu-bar-ediff-misc-menu
    (make-sparse-keymap "Ediff Miscellanea"))
  (fset 'menu-bar-ediff-misc-menu
	(symbol-value 'menu-bar-ediff-misc-menu))
  (defvar menu-bar-epatch-menu (make-sparse-keymap "Apply Patch"))
  (fset 'menu-bar-epatch-menu (symbol-value 'menu-bar-epatch-menu))
  (defvar menu-bar-ediff-merge-menu (make-sparse-keymap "Merge"))
  (fset 'menu-bar-ediff-merge-menu
	(symbol-value 'menu-bar-ediff-merge-menu))
  (defvar menu-bar-ediff-menu (make-sparse-keymap "Compare"))
  (fset 'menu-bar-ediff-menu (symbol-value 'menu-bar-ediff-menu))
  
  ;; define ediff-menu
  (define-key menu-bar-ediff-menu [window]
    '("This Window and Next Window" . compare-windows))
  (define-key menu-bar-ediff-menu [ediff-windows-linewise]
    '("Windows Line-by-line..." . ediff-windows-linewise))
  (define-key menu-bar-ediff-menu [ediff-windows-wordwise]
    '("Windows Word-by-word..." . ediff-windows-wordwise))
  (define-key menu-bar-ediff-menu [separator-ediff-windows] '("--"))
  (define-key menu-bar-ediff-menu [ediff-regions-linewise]
    '("Regions Line-by-line..." . ediff-regions-linewise))
  (define-key menu-bar-ediff-menu [ediff-regions-wordwise]
    '("Regions Word-by-word..." . ediff-regions-wordwise))
  (define-key menu-bar-ediff-menu [separator-ediff-regions] '("--"))
  (define-key menu-bar-ediff-menu [ediff-dir-revision]
    '("Directory Revisions..." . ediff-directory-revisions))
  (define-key menu-bar-ediff-menu [ediff-revision]
    '("File with Revision..." . ediff-revision))
  (define-key menu-bar-ediff-menu [separator-ediff-directories] '("--"))
  (define-key menu-bar-ediff-menu [ediff-directories3]
    '("Three Directories..." . ediff-directories3))
  (define-key menu-bar-ediff-menu [ediff-directories]
    '("Two Directories..." . ediff-directories))
  (define-key menu-bar-ediff-menu [separator-ediff-files] '("--"))
  (define-key menu-bar-ediff-menu [ediff-buffers3]
    '("Three Buffers..." . ediff-buffers3))
  (define-key menu-bar-ediff-menu [ediff-files3]
    '("Three Files..." . ediff-files3))
  (define-key menu-bar-ediff-menu [ediff-buffers]
    '("Two Buffers..." . ediff-buffers))
  (define-key menu-bar-ediff-menu [ediff-files]
    '("Two Files..." . ediff-files))

  ;; define merge menu
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
    '("Directory Revisions with Ancestor..."
      . ediff-merge-directory-revisions-with-ancestor))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
    '("Directory Revisions..." . ediff-merge-directory-revisions))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
    '("Revisions with Ancestor..."
      . ediff-merge-revisions-with-ancestor))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
    '("Revisions..." . ediff-merge-revisions))
  (define-key menu-bar-ediff-merge-menu [separator-ediff-merge] '("--"))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
    '("Directories with Ancestor..."
      . ediff-merge-directories-with-ancestor))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
    '("Directories..." . ediff-merge-directories))
  (define-key
    menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] '("--"))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
    '("Buffers with Ancestor..." . ediff-merge-buffers-with-ancestor))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
    '("Buffers..." . ediff-merge-buffers))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
    '("Files with Ancestor..." . ediff-merge-files-with-ancestor))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files]
    '("Files..." . ediff-merge-files))

  ;; define epatch menu
  (define-key menu-bar-epatch-menu [ediff-patch-buffer]
    '("To a Buffer..." . ediff-patch-buffer))
  (define-key menu-bar-epatch-menu [ediff-patch-file]
    '("To a File..." . ediff-patch-file))

  ;; define ediff miscellanea
  (define-key menu-bar-ediff-misc-menu [emultiframe]
    '("Toggle use of separate control buffer frame"
      . ediff-toggle-multiframe))
  (define-key menu-bar-ediff-misc-menu [eregistry]
    '("List Ediff Sessions" . ediff-show-registry))
  (define-key menu-bar-ediff-misc-menu [ediff-cust]
    '("Customize Ediff" . ediff-customize))
  (define-key menu-bar-ediff-misc-menu [ediff-doc]
    '("Ediff Manual" . ediff-documentation)))

(provide 'ediff-hook)


;; arch-tag: 512f8656-8a4b-4789-af5d-5c6144498df3
;;; ediff-hook.el ends here
