;;; ediff-hook.el --- setup for Ediff's menus and autoloads

;; Copyright (C) 1995, 96, 97, 98, 99, 2000, 01, 02 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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

;; This macro is used to avoid compilation warnings.
;; The macro will expand into the form that is appropriate to the
;; compiler at hand (emacs or xemacs).
;; The autoload, below, is useless in Emacs because ediff-hook.el
;; is dumped with emacs, but it is needed in XEmacs
(defmacro ediff-cond-compile-for-xemacs-or-emacs (xemacs-form emacs-form)
  (if (string-match "XEmacs" emacs-version)
      xemacs-form emacs-form))
 
;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
(ediff-cond-compile-for-xemacs-or-emacs
 ;; xemacs form
 (defun ediff-xemacs-init-menus ()
   (if (featurep 'menubar)
       (progn
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
	 )))
 nil ; emacs form
 )


;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
(ediff-cond-compile-for-xemacs-or-emacs
 (progn
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
       ["Ediff Manual..." ediff-documentation t]
       ["Customize Ediff..." ediff-customize t]
       ["List Ediff Sessions..." ediff-show-registry t]
       ["Use separate frame for Ediff control buffer..."
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
		      (ediff-use-toolbar-p))]
       ))
   
   ;; put these menus before Object-Oriented-Browser in Tools menu
   (if (and (featurep 'menubar) (not (featurep 'infodock))
	    (not (featurep 'ediff-hook)))
	   (ediff-xemacs-init-menus)))
 
 ;; Emacs--only if menu-bar is loaded
 (if (featurep 'menu-bar)
     (progn
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
	 '("Toggle use of separate control buffer frame..."
	   . ediff-toggle-multiframe))
       (define-key menu-bar-ediff-misc-menu [eregistry]
	 '("List Ediff Sessions..." . ediff-show-registry))
       (define-key menu-bar-ediff-misc-menu [ediff-cust]
	 '("Customize Ediff..." . ediff-customize))
       (define-key menu-bar-ediff-misc-menu [ediff-doc]
	 '("Ediff Manual..." . ediff-documentation))
       )
      
      ) ; emacs case
 ) ; ediff-cond-compile-for-xemacs-or-emacs

;; arrange for autoloads
(if purify-flag
    () ; if dumping, autoloads are set up in loaddefs.el
  ;; if the user decides to load this file, set up autoloads
  ;; compare files and buffers
  (autoload 'ediff "ediff" "Compare two files" t)
  (autoload 'ediff-files "ediff" "Compare two files" t)
  (autoload 'ediff-buffers "ediff" "Compare two bufers" t)
  (autoload 'ebuffers "ediff" "Compare two bufers" t)
  (autoload 'ediff3  "ediff"  "Compare three files" t)
  (autoload 'ediff-files3 "ediff" "Compare three files" t)
  (autoload 'ediff-buffers3 "ediff" "Compare three bufers" t)
  (autoload 'ebuffers3 "ediff" "Compare three bufers" t)

  (autoload 'erevision "ediff" "Compare versions of a file" t)
  (autoload 'ediff-revision "ediff" "Compare versions of a file" t)

  ;; compare regions and windows
  (autoload 'ediff-windows-wordwise 
    "ediff" "Compare two windows word-by-word." t)
  (autoload 'ediff-regions-wordwise 
    "ediff" "Compare two regions word-by-word." t)
  (autoload 'ediff-windows-linewise 
    "ediff" "Compare two windows line-by-line." t)
  (autoload 'ediff-regions-linewise 
    "ediff" "Compare two regions line-by-line." t)

  ;; patch
  (autoload 'ediff-patch-file "ediff" "Patch a file." t)
  (autoload 'epatch "ediff" "Patch a file." t)
  (autoload 'ediff-patch-buffer "ediff" "Patch a buffer.")
  (autoload 'epatch-buffer "ediff" "Patch a buffer." t)

  ;; merge
  (autoload 'ediff-merge "ediff" "Merge two files." t)
  (autoload 'ediff-merge-files "ediff" "Merge two files." t)
  (autoload 'ediff-merge-files-with-ancestor
    "ediff" "Merge two files using a third file as an ancestor." t)
  (autoload 'ediff-merge-buffers "ediff" "Merge two buffers." t)
  (autoload 'ediff-merge-buffers-with-ancestor
    "ediff" "Merge two buffers using a third buffer as an ancestor." t)

  (autoload 'ediff-merge-revisions "ediff" "Merge two versions of a file." t)
  (autoload 'ediff-merge-revisions-with-ancestor
    "ediff" "Merge two versions of a file." t)

  ;; compare directories
  (autoload 'edirs "ediff" "Compare files in two directories." t)
  (autoload 'ediff-directories "ediff" "Compare files in two directories." t)
  (autoload 'edirs3 "ediff" "Compare files in three directories." t)
  (autoload
    'ediff-directories3 "ediff" "Compare files in three directories." t)

  (autoload 'edir-revisions 
    "ediff" "Compare two versions of a file." t)
  (autoload 'ediff-directory-revisions 
    "ediff" "Compare two versions of a file." t)

  ;; merge directories
  (autoload 'edirs-merge "ediff" "Merge files in two directories." t)
  (autoload 'ediff-merge-directories
    "ediff" "Merge files in two directories." t)
  (autoload 'edirs-merge-with-ancestor
    "ediff"
    "Merge files in two directories using files in a third dir as ancestors."
    t)
  (autoload 'ediff-merge-directories-with-ancestor
    "ediff"
    "Merge files in two directories using files in a third dir as ancestors."
    t)

  (autoload 'edir-merge-revisions 
    "ediff" "Merge versions of files in a directory." t)
  (autoload 'ediff-merge-directory-revisions 
    "ediff" "Merge versions of files in a directory." t)
  (autoload 'ediff-merge-directory-revisions-with-ancestor
    "ediff"
    "Merge versions of files in a directory using other versions as ancestors."
    t)
  (autoload 'edir-merge-revisions-with-ancestor
    "ediff"
    "Merge versions of files in a directory using other versions as ancestors."
    t)

  ;; misc
  (autoload 'ediff-show-registry
    "ediff-mult"
    "Display the registry of active Ediff sessions."
    t)
  (autoload 'eregistry
    "ediff-mult"
    "Display the registry of active Ediff sessions."
    t)
  (autoload 'ediff-documentation
    "ediff"
    "Display Ediff's manual."
    t)
  (autoload 'ediff-version
    "ediff"
    "Show Ediff's version and last modification date."
    t)
  (autoload 'ediff-toggle-multiframe
    "ediff-util"
    "Toggle the use of separate frame for Ediff control buffer."
    t)
  (autoload 'ediff-toggle-use-toolbar
    "ediff-util"
    "Toggle the use of Ediff toolbar."
    t)
  
  ) ; if purify-flag


(provide 'ediff-hook)

;;; ediff-hook.el ends here
