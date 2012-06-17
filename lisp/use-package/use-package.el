;;; use-package --- A use-package declaration for simplifying your .emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 17 Jun 2012
;; Version: 1.0
;; Keywords: dotemacs startup speed config package
;; X-URL: https://github.com/jwiegley/use-package

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

;; The `use-package' declaration macro allows you to isolate package
;; configuration in your ".emacs" in a way that is performance-oriented and,
;; well, just tidy.  I created it because I have over 80 packages that I use
;; in Emacs, and things were getting difficult to manage.  Yet with this
;; utility my total load time is just under 1 second, with no loss of
;; functionality!
;;
;; Here is the simplest `use-package' declaration:
;;
;;   (use-package foo)
;;
;; This loads in the package foo, but only if foo is available on your system.
;; If not, a warning is logged to your `*Messages*' buffer.  If it succeeds a
;; message about "Loading foo" is logged, along with the time it took to load,
;; if that time is over 0.01s.
;;
;; Use the :init keywoard to do some stuff to initialize foo, but only if foo
;; actually gets loaded:
;;
;;   (use-package foo
;;     :init
;;     (progn
;;       (setq foo-variable t)
;;       (foo-mode 1)))
;;
;; A very command thing to do when loading a module is to bind a key to
;; primary commands within that module:
;;
;;   (use-package ace-jump-mode
;;     :bind ("C-." . ace-jump-mode))
;;
;; This does two things: first, it creates autoload for the `ace-jump-mode'
;; command, and defers loading of `ace-jump-mode' until you actually use it.
;; Second, it binds the key `C-.' to that command.  After loading, you can use
;; `M-x describe-personal-keybindings' to see all such bindings you've set
;; throughout your Emacs.
;;
;; A more literal way to do the exact same thing is:
;;
;;   (use-package ace-jump-mode
;;     :commands ace-jump-mode
;;     :init
;;     (bind-key "C-." 'ace-jump-mode))
;;
;; When you use the `:commands' keyword, it creates autoloads for those
;; commands and defers loading of the module until they are used.  In this
;; case, the `:init' form is always run -- even if ace-jump-mode might not be
;; on your system.  So remember to keep `:init' activities to only those that
;; would succeed either way.
;;
;; If you aren't used `:commands' or `:bind' (which implies `:commands'), you
;; can still defer loading with `:defer' keyword:
;;
;;   (use-package ace-jump-mode
;;     :defer t
;;     :init
;;     (progn
;;       (autoload 'ace-jump-mode "ace-jump-mode" nil t)
;;       (bind-key "C-." 'ace-jump-mode)))
;;
;; This does exactly the same thing as the other two commands above.
;;
;; A companion to the `:init' keyword is `:config'.  Although `:init' always
;; happens in the case of deferred modules (which are likely to be the most
;; common kind), `:config' form only run after the module has been loaded by
;; Emacs:
;;
;;   (use-package ace-jump-mode
;;     :bind ("C-." . ace-jump-mode)
;;     :config
;;     (message "Yay, ace-jump-mode was actually loaded!"))
;;
;; You will see a "Configured..." message in your `*Messages*' log when a
;; package is configured, and a timing if the configuration time was longer
;; than 0.01s.  You should keep `:init' forms as simple as possible, and put
;; as much as you can get away with on the `:config' side.
;;
;; You can have both `:init' and `:config':
;;
;;   (use-package haskell-mode
;;     :commands haskell-mode
;;     :init
;;     (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
;;     :config
;;     (progn
;;       (use-package inf-haskell)
;;       (use-package hs-lint)))
;;
;; In this case, I want to autoload the command `haskell-mode' from
;; "haskell-mode.el", add it to `auto-mode-alist' at the time ".emacs" is
;; loaded, but wait until after I've opened a Haskell file before loading
;; "inf-haskell.el" and "hs-lint.el".
;;
;; The `:bind' keyword takes either a cons or a list of conses:
;;
;;   (use-package hi-lock
;;     :bind (("M-o l" . highlight-lines-matching-regexp)
;;            ("M-o r" . highlight-regexp)
;;            ("M-o w" . highlight-phrase)))
;;
;; The `:commands' keyword likewise takes either a symbol or a list of
;; symbols.
;;
;; You can use the `:if' keyword to predicate the loading and initialization
;; of a module.  For example, I only want an `edit-server' running for my
;; main, graphical Emacs, not for Emacsen I may start at the command line:
;;
;;   (use-package edit-server
;;     :if window-system
;;     :init
;;     (progn
;;       (add-hook 'after-init-hook 'server-start t)
;;       (add-hook 'after-init-hook 'edit-server-start t)))
;;
;; The `:disabled' keyword can be used to turn off a module that you're having
;; difficulties with, or to stop loading something you're not really using at
;; the present time:
;;
;;   (use-package ess-site
;;     :disabled t
;;     :commands R)
;;
;; Another feature of `use-package' is that it always loads every file that it
;; can when your ".emacs" is being byte-compiled (if you do that, which I
;; recommend).  This helps to silence spurious warnings about unknown
;; variables and functions.
;;
;; However, there are times when this is just not enough.  For those times,
;; use the `:defines' keyword to introduce empty variable definitions solely
;; for the sake of the byte-compiler:
;;
;;   (use-package texinfo
;;     :defines texinfo-section-list
;;     :commands texinfo-mode
;;     :init
;;     (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))
;;
;; If you need to silence a missing function warning, do it with an autoload
;; stub in your `:init' block:
;;
;;   (use-package w3m
;;     :commands (w3m-browse-url w3m-session-crash-recovery-remove)
;;     :init
;;     (eval-when-compile
;;       (autoload 'w3m-search-escape-query-string "w3m-search")))
;;
;; Lastly, `use-package' provides built-in support for the diminish utility,
;; if you have that installed.  It's purpose is to remove strings from your
;; mode-line that would otherwise always be there and provide no useful
;; information.  It is invoked with the `:diminish' keyword, which is passed
;; the minor mode symbol:
;;
;;   (use-package abbrev
;;     :diminish abbrev-mode
;;     :init
;;     (if (file-exists-p abbrev-file-name)
;;         (quietly-read-abbrev-file))
;;
;;     :config
;;     (add-hook 'expand-load-hook
;;               (lambda ()
;;                 (add-hook 'expand-expand-hook 'indent-according-to-mode)
;;                 (add-hook 'expand-jump-hook 'indent-according-to-mode))))
;;
;; If you noticed that this declaration has neither a `:bind', `:commands' or
;; `:defer' keyword: congratulations, you're an A student!  What it means is
;; that both the `:init' and `:config' forms will be executed when ".emacs" is
;; loaded, with no delays until later.  Is this useful?  Not really.  I just
;; happen to like separating my configuration into things that must happen at
;; startup time, and things that could potentioally wait until after the
;; actual load.  In this case, everything could be put inside `:init' and
;; there would be no difference.

(require 'bind-key)

(defgroup use-package nil
  "A use-package declaration for simplifying your .emacs"
  :group 'startup)

;;;_ , Create use-package macro, to simplify customizations

(eval-when-compile
  (require 'cl))

(require 'bind-key)
(require 'diminish nil t)

(defcustom use-package-verbose t
  "Whether to report about loading and configuration details."
  :type 'boolean
  :group 'use-package)

(defmacro with-elapsed-timer (text &rest forms)
  `(let ((now ,(if use-package-verbose
                   '(current-time))))
     ,(if use-package-verbose
          `(message "%s..." ,text))
     ,@forms
     ,(when use-package-verbose
        `(let ((elapsed
                (float-time (time-subtract (current-time) now))))
           (if (> elapsed 0.01)
               (message "%s...done (%.3fs)" ,text elapsed)
             (message "%s...done" ,text))))))

(put 'with-elapsed-timer 'lisp-indent-function 1)

(defmacro use-package (name &rest args)
  (let* ((commands (plist-get args :commands))
         (init-body (plist-get args :init))
         (config-body (plist-get args :config))
         (diminish-var (plist-get args :diminish))
         (defines (plist-get args :defines))
         (keybindings (plist-get args :bind))
         (predicate (plist-get args :if))
         (defines-eval (if (null defines)
                           nil
                         (if (listp defines)
                             (mapcar (lambda (var) `(defvar ,var)) defines)
                           `((defvar ,defines)))))
         (requires (plist-get args :requires))
         (requires-test (if (null requires)
                            t
                          (if (listp requires)
                              `(not (member nil (mapcar #'featurep
                                                        (quote ,requires))))
                            `(featurep (quote ,requires)))))
         (name-string (if (stringp name) name
                        (symbol-name name))))

    (if diminish-var
        (setq config-body
              `(progn
                 ,config-body
                 (ignore-errors
                   ,@(if (listp diminish-var)
                         (mapcar (lambda (var) `(diminish (quote ,var)))
                                 diminish-var)
                       `((diminish (quote ,diminish-var))))))))

    (when keybindings
      (if (and commands (symbolp commands))
          (setq commands (list commands)))
      (setq init-body
            `(progn
               ,init-body
               ,@(mapcar #'(lambda (binding)
                             (push (cdr binding) commands)
                             `(bind-key ,(car binding)
                                        (quote ,(cdr binding))))
                         (if (and (consp keybindings)
                                  (stringp (car keybindings)))
                             (list keybindings)
                           keybindings)))))

    (unless (plist-get args :disabled)
      `(progn
         (eval-when-compile
           ,@defines-eval
           ,(if (stringp name)
                `(load ,name t)
              `(require ',name nil t)))
         ,(if (or commands (plist-get args :defer))
              (let (form)
                (unless (listp commands)
                  (setq commands (list commands)))
                (mapc #'(lambda (command)
                          (push `(autoload (function ,command)
                                   ,name-string nil t) form))
                      commands)

                `(when ,(or predicate t)
                   ,@form
                   ,init-body
                   ,(unless (null config-body)
                      `(eval-after-load ,name-string
                         '(if ,requires-test
                              (with-elapsed-timer
                                  ,(format "Configuring package %s" name-string)
                                ,config-body))))
                   t))
            `(if (and ,(or predicate t)
                      ,requires-test)
                 (with-elapsed-timer
                     ,(format "Loading package %s" name-string)
                   (if (not ,(if (stringp name)
                                 `(load ,name t)
                               `(require ',name nil t)))
                       (message "Could not load package %s" ,name-string)
                     ,init-body
                     ,config-body
                     t))))))))

(put 'use-package 'lisp-indent-function 1)

(provide 'use-package)

;;; use-package.el ends here
