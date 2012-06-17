;;; bind-key --- A simple way to manage personal keybindings

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 16 Jun 2012
;; Version: 1.0
;; Keywords: keys keybinding config dotemacs
;; X-URL: https://github.com/jwiegley/bind-key

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If you have lots of keybindings set in your .emacs file, it can be hard to
;; know which ones you haven't set yet, and which may now be overriding some
;; new default in a new Emacs version.  This module aims to solve that
;; problem.
;;
;; Bind keys as follows in your .emacs:
;;
;;   (require 'bind-key)
;;
;;   (bind-key "C-c x" 'my-ctrl-c-x-command)
;;
;; If you want the keybinding to override all minor modes that may also bind
;; the same key, use the `bind-key*' form:
;;
;;   (bind-key* "<C-return>" 'other-window)
;;
;; If you want to rebind a key only in a particular key, use:
;;
;;   (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;;
;; To unbind a key within a keymap (for example, to stop your favorite major
;; mode from changing a binding that you don't want to override everywhere),
;; use `unbind-key':
;;
;;   (unbind-key "C-c x" some-other-mode-map)
;;
;; After Emacs loads, you can see a summary of all your personal keybindings
;; currently in effect with this command:
;;
;;   M-x describe-personal-keybindings
;;
;; This display will tell you if you've overriden a default keybinding, and
;; what the default was.  Also, it will tell you if the key was rebound after
;; your binding it with `bind-key', and what it was rebound it to.

(require 'easy-mmode)

(defgroup bind-key nil
  "A simple way to manage personal keybindings"
  :group 'emacs)

(defcustom bind-key-segregation-regexp
  "\\`\\(\\(C-[chx] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)"
  "Regular expression used to divide key sets in the output from
\\[describe-personal-keybindings]."
  :type 'regexp
  :group 'bind-key)

;; Create override-global-mode to force key remappings

(defvar override-global-map (make-keymap)
  "override-global-mode keymap")

(define-minor-mode override-global-mode
  "A minor mode so that keymap settings override other modes."
  t "" override-global-map)

(add-hook 'after-init-hook
          (function
           (lambda ()
             (override-global-mode 1))))

(defvar personal-keybindings nil)

(defmacro bind-key (key-name command &optional keymap)
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (bindingvar (make-symbol "binding"))
        (entryvar (make-symbol "entry")))
    `(let* ((,namevar ,(eval key-name))
            (,keyvar (read-kbd-macro ,namevar))
            (,bindingvar (lookup-key (or ,keymap global-map)
                                     ,keyvar)))
       (let ((,entryvar (assoc (cons ,namevar (quote ,keymap))
                               personal-keybindings)))
         (if ,entryvar
             (setq personal-keybindings
                   (delq ,entryvar personal-keybindings))))
       (setq personal-keybindings
             (cons (list (cons ,namevar (quote ,keymap))
                         ,command
                         (unless (numberp ,bindingvar) ,bindingvar))
                   personal-keybindings))
       (define-key (or ,keymap global-map) ,keyvar ,command))))

(defmacro unbind-key (key-name &optional keymap)
  `(bind-key ,key-name nil ,keymap))

(defmacro bind-key* (key-name command)
  `(progn
     (bind-key ,key-name ,command)
     (define-key override-global-map ,(read-kbd-macro key-name) ,command)))

(defun get-binding-description (elem)
  (cond
   ((listp elem)
    (cond
     ((eq 'lambda (car elem))
      "#<lambda>")
     ((eq 'closure (car elem))
      "#<closure>")
     ((eq 'keymap (car elem))
      "#<keymap>")
     (t
      elem)))
   ((keymapp elem)
    "#<keymap>")
   ((symbolp elem)
    elem)
   (t
    "#<byte-compiled lambda>")))

(defun compare-keybindings (l r)
  (let* ((regex bind-key-segregation-regexp)
         (lgroup (and (string-match regex (caar l))
                      (match-string 0 (caar l))))
         (rgroup (and (string-match regex (caar r))
                      (match-string 0 (caar r))))
         (lkeymap (cdar l))
         (rkeymap (cdar r)))
    (cond
     ((and (null lkeymap) rkeymap)
      (cons t t))
     ((and lkeymap (null rkeymap))
      (cons nil t))
     ((and lkeymap rkeymap
           (not (string= (symbol-name lkeymap) (symbol-name rkeymap))))
      (cons (string< (symbol-name lkeymap) (symbol-name rkeymap)) t))
     ((and (null lgroup) rgroup)
      (cons t t))
     ((and lgroup (null rgroup))
      (cons nil t))
     ((and lgroup rgroup)
      (if (string= lgroup rgroup)
          (cons (string< (caar l) (caar r)) nil)
        (cons (string< lgroup rgroup) t)))
     (t
      (cons (string< (caar l) (caar r)) nil)))))

(defun describe-personal-keybindings ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Personal Keybindings*")
    (delete-region (point-min) (point-max))
    (insert "Key name          Command                                 Comments
----------------- --------------------------------------- ---------------------
")
    (let (last-binding)
      (dolist (binding
               (setq personal-keybindings
                     (sort personal-keybindings
                           #'(lambda (l r)
                               (car (compare-keybindings l r))))))

        (if (not (eq (cdar last-binding) (cdar binding)))
            (insert ?\n (format "\n%s\n%s\n\n"
                                (cdar binding)
                                (make-string 79 ?-)))
          (if (and last-binding
                   (cdr (compare-keybindings last-binding binding)))
              (insert ?\n)))

        (let* ((key-name (caar binding))
               (at-present (lookup-key (or (symbol-value (cdar binding))
                                           (current-global-map))
                                       (read-kbd-macro key-name)))
               (command (nth 1 binding))
               (was-command (nth 2 binding))
               (command-desc (get-binding-description command))
               (was-command-desc (and was-command
                                      (get-binding-description was-command)))
               (at-present-desc (get-binding-description at-present))
               )
          (insert
           (format
            "%-18s%-40s%s\n"
            key-name command-desc
            (if (string= command-desc at-present-desc)
                (if (or (null was-command)
                        (string= command-desc was-command-desc))
                    ""
                  (format "(%s)" was-command-desc))
              (format "[now: %s]" at-present)))))

        (setq last-binding binding)))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'bind-key)

;;; bind-key.el ends here
