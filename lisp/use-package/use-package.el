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
;; A very common thing to do when loading a module is to bind a key to primary
;; commands within that module:
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
;; Similar to `:bind', you can use `:mode' and `:interpreter' to establish a
;; deferred binding within `auto-mode-alist' and `interpreter-mode-alist'.
;; The specifier to either keyword can be a single cons or a list:
;;
;;   (use-package python-mode
;;     :mode ("\\.py$" . python-mode)
;;     :interpreter ("python" . python-mode))
;;
;; If you aren't using `:commands', `:bind', `:mode', or `:interpreter' (all
;; of which imply `:commands'), you can still defer loading with the `:defer'
;; keyword:
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
;; Another similar option to `:init' is `:idle'. Like `:init' this always run,
;; however, it does so when Emacs is idle at some time in the future after
;; load. This is particularly useful for convienience minor modes which can be
;; slow to load. For instance, in this case, I want Emacs to always use
;; `global-pabbrev-mode'. `:commands' creates an appropriate autoload; `:idle'
;; will run this command at some point in the future. If you start Emacs and
;; beginning typing straight-away, loading will happen eventually.
;;
;; (use-package pabbrev
;;   :commands global-pabbrev-mode
;;   :idle (global-pabbrev-mode))
;;
;; Idle functions are run in the order in which they are evaluated. If you
;; have many, it may take sometime for all to run. `use-package' will always
;; tell you if there is an error in the form which can otherwise be difficult
;; to debug. It may tell you about functions being eval'd, depending on the
;; value of `use-package-verbose'. Other good candidates for `:idle' are
;; `yasnippet', `auto-complete' and `autopair'.
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
;; If your package needs a directory added to the `load-path' in order load,
;; use `:load-path'.  It takes a string or a list of strings.  If the path is
;; relative, it will be expanded within `user-emacs-directory':
;;
;;   (use-package ess-site
;;     :disabled t
;;     :load-path "site-lisp/ess/lisp/"
;;     :commands R)
;;
;; Lastly, `use-package' provides built-in support for the diminish utility,
;; if you have that installed.  It's purpose is to remove strings from your
;; mode-line that would otherwise always be there and provide no useful
;; information.  It is invoked with the `:diminish' keyword, which is passed
;; either the minor mode symbol, or a cons of the symbol and a replacement string:
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
;;
;; * For el-get users
;;
;; You can use `use-package' as a way to create source definitions for el-get.
;; All that's needed is to add a `:type' keyword to your declaration.  When
;; this is present, certain keywords get translated to what el-get expects in
;; the `el-get-sources' list:
;;
;;   :config   -> :after
;;   :requires -> :depends
;;
;; A `:name' will be added also, if one is not provided explicitly, which will
;; be the same as the name of the package.
;;
;; But why would you want to use `use-package' when you have el-get?  My
;; answer is that I'd like to use el-get to install and update some packages,
;; but I don't want it managing configuration.  Just loading el-get -- without
;; call (el-get 'sync) -- takes a quarter second on my machine.  That's 25% of
;; my load time!  `use-package' is designed for performance, so I only want to
;; load el-get when it's time to install or update on of my used packages.
;;
;; Here is the `use-package' declaration I use for setting up el-get, but only
;; when I want to install or update:
;;
;;   (defvar el-get-sources nil)
;;
;;   (use-package el-get
;;     :commands (el-get
;;                el-get-install
;;                el-get-update
;;                el-get-list-packages)
;;     :config
;;     (defun el-get-read-status-file ()
;;       (mapcar #'(lambda (entry)
;;                   (cons (plist-get entry :symbol)
;;                         `(status "installed" recipe ,entry)))
;;               el-get-sources)))

;;; Code:

(require 'bind-key)
(require 'bytecomp)
(require 'diminish nil t)

(eval-when-compile
  (require 'cl))

(defgroup use-package nil
  "A use-package declaration for simplifying your .emacs"
  :group 'startup)

(defcustom use-package-verbose t
  "Whether to report about loading and configuration details."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-debug nil
  "Whether to report more information, mostly regarding el-get"
  :type 'boolean
  :group 'use-package)

(defcustom use-package-minimum-reported-time 0.01
  "Minimal load time that will be reported"
  :type 'number
  :group 'use-package
  )

(defmacro with-elapsed-timer (text &rest forms)
  `(let ((now ,(if use-package-verbose
                   '(current-time))))
     ,(if use-package-verbose
          `(message "%s..." ,text))
     (prog1
         ,@forms
       ,(when use-package-verbose
          `(let ((elapsed
                  (float-time (time-subtract (current-time) now))))
             (if (> elapsed use-package-minimum-reported-time)
                 (message "%s...done (%.3fs)" ,text elapsed)
               (message "%s...done" ,text)))))))

(put 'with-elapsed-timer 'lisp-indent-function 1)

(defun use-package-discover-el-get-type (args)
  (let* ((pkg-name (plist-get args :name))
         (git-config  (expand-file-name
                       (concat pkg-name "/.git/config")
                       (if (boundp 'user-site-lisp-directory)
                           user-site-lisp-directory
                         user-emacs-directory))))

    (catch 'found
      ;; Look for a readable .git/config with at least one defined remote.
      (if (file-readable-p git-config)
          (with-temp-buffer
            (insert-file-contents-literally git-config)
            (while (re-search-forward "\\[remote" nil t)
              (if (re-search-forward "url = \\(.+\\)"
                                     (save-excursion
                                       (re-search-forward "\\[remote" nil t)
                                       (point)) t)
                  (nconc args (list :type 'git
                                    :url (match-string 1))))))))
    args))

(defvar use-package-idle-timer nil)
(defvar use-package-idle-forms nil)

(defun use-package-start-idle-timer ()
  "Ensure that the idle timer is running"
  (unless use-package-idle-timer
    (setq use-package-idle-timer
          (run-with-idle-timer
           3 t
           'use-package-idle-eval))))

(defun use-package-init-on-idle (form)
  "Add a new form to the idle queue"
  (use-package-start-idle-timer)
  (if use-package-idle-forms
      (add-to-list 'use-package-idle-forms
                   form t)
    (setq use-package-idle-forms (list form))
    ))

(defun use-package-idle-eval()
  "Start to eval idle-commands from the idle queue"
  (let ((next (pop use-package-idle-forms)))
    (if next
        (progn
          (when use-package-verbose
            (message "use-package idle:%s" next))

          (condition-case e
              (funcall next)
            (error
             (message
              "Failure on use-package idle. Form: %s, Error: %s"
              next e)))
	  ;; recurse after a bit
          (when (sit-for 3)
	    (use-package-idle-eval)))
      ;; finished (so far!)
      (cancel-timer use-package-idle-timer)
      (setq use-package-idle-timer nil))))

(defun use-package-ensure-elpa (package)
  (when (not (package-installed-p package))
    (package-install package)))


(defmacro use-package (name &rest args)
"Use a package with configuration options.

For full documentation. please see commentary.

  (use-package package-name
     :keyword option)

:init Code to run when `use-package' form evals.
:bind Perform key bindings, and define autoload for bound
      commands.
:commands Define autoloads for given commands.
:mode Form to be added to `auto-mode-alist'.
:interpreter Form to be added to `interpreter-mode-alist'.
:defer Defer loading of package -- automatic
       if :commands, :bind, :mode or :interpreter are used.
:config Runs if and when package loads.
:if Conditional loading.
:disabled Ignore everything.
:defines Define vars to silence byte-compiler.
:load-path Add to `load-path' before loading.
:diminish Support for diminish package (if it's installed).
:idle adds a form to run on an idle timer
"
  (let* ((commands (plist-get args :commands))
         (pre-init-body (plist-get args :pre-init))
         (init-body (plist-get args :init))
         (config-body (plist-get args :config))
         (diminish-var (plist-get args :diminish))
         (defines (plist-get args :defines))
         (idle-body (plist-get args :idle))
         (keybindings )
         (mode-alist )
         (interpreter-alist )
         (predicate (plist-get args :if))
         (pkg-load-path (plist-get args :load-path))
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
         (name-string (if (stringp name) name (symbol-name name)))
         (name-symbol (if (stringp name) (intern name) name)))

    ;; force this immediately -- one off cost
    (unless (plist-get args :disabled)
      (let* ((ensure (plist-get args :ensure))
             (package-name
              (or (and (eq ensure t)
                       name)
                  ensure)))

        (when package-name
          (use-package-ensure-elpa package-name)))


      (if diminish-var
          (setq config-body
                `(progn
                   ,config-body
                   (ignore-errors
                     ,@(if (listp diminish-var)
                           (if (listp (cdr diminish-var))
                               (mapcar (lambda (var)
                                           (if (listp var)
                                               `(diminish (quote ,(car var)) ,(cdr var))
                                               `(diminish (quote ,var))))
                                diminish-var)
                               `((diminish (quote ,(car diminish-var)) ,(cdr diminish-var)))
                           )
                         `((diminish (quote ,diminish-var))))))))

      (if (and commands (symbolp commands))
          (setq commands (list commands)))


      (when idle-body
        (setq init-body
              `(progn
                 (use-package-init-on-idle (lambda () ,idle-body))
                   ,init-body)))


      (flet ((init-for-commands
              (func sym-or-list)
              (let ((cons-list (if (and (consp sym-or-list)
                                        (stringp (car sym-or-list)))
                                   (list sym-or-list)
                                 sym-or-list)))
                (if cons-list
                    (setq init-body
                          `(progn
                             ,init-body
                             ,@(mapcar #'(lambda (elem)
                                           (push (cdr elem) commands)
                                           (funcall func elem))
                                       cons-list)))))))

        (init-for-commands #'(lambda (binding)
                               `(bind-key ,(car binding)
                                          (quote ,(cdr binding))))
                           (plist-get args :bind))

        (init-for-commands #'(lambda (mode)
                               `(add-to-list 'auto-mode-alist
                                             (quote ,mode)))
                           (plist-get args :mode))

        (init-for-commands #'(lambda (interpreter)
                               `(add-to-list 'interpreter-mode-alist
                                             (quote ,interpreter)))
                           (plist-get args :interpreter)))

      `(progn
         ,@(mapcar
            #'(lambda (path)
                `(add-to-list 'load-path
                              ,(if (file-name-absolute-p path)
                                   path
                                 (expand-file-name path user-emacs-directory))))
            (cond ((stringp pkg-load-path)
                   (list pkg-load-path))
                  ((functionp pkg-load-path)
                   (funcall pkg-load-path))
                  (t
                   pkg-load-path)))

         (when byte-compile-current-file
           ,@defines-eval
           ,(if (stringp name)
                `(load ,name t)
              `(require ',name nil t)))

         ,(when (boundp 'el-get-sources)
            (require 'el-get)

            (let ((recipe (ignore-errors
                            (el-get-read-recipe name-symbol))))
              (if (null recipe)
                  (if use-package-debug
                      (message "No el-get recipe found for package `%s'"
                               name-symbol))
                (setq args
                      (mapcar #'(lambda (arg)
                                  (cond
                                   ((eq arg :config)
                                    :after)
                                   ((eq arg :requires)
                                    :depends)
                                   (t
                                    arg)))
                              args))

                (nconc args (list :symbol (intern name-string)))

                (let ((elem args))
                  (while elem
                    (unless (plist-get recipe (car elem))
                      (plist-put recipe (car elem) (cadr elem)))
                    (setq elem (cddr elem))))

                (unless (plist-get recipe :name)
                  (nconc recipe (list :name name-string)))

                (unless (plist-get recipe :type)
                  (setq recipe (use-package-discover-el-get-type recipe)))

                (ignore
                 (setq el-get-sources (cons recipe el-get-sources))))))

         ,(if (or commands (plist-get args :defer))
              (let (form)
                (mapc #'(lambda (command)
                          (push `(autoload (function ,command)
                                   ,name-string nil t) form))
                      commands)

                `(when ,(or predicate t)
                   ,pre-init-body
                   ,@form
                   ,init-body
                   ,(unless (null config-body)
                      `(eval-after-load ,name-string
                         (quote
                           (if ,requires-test
                               ,(macroexpand-all
                                  `(with-elapsed-timer
                                       ,(format "Configuring package %s" name-string)
                                     ,config-body))))))
                   t))
            `(if (and ,(or predicate t)
                      ,requires-test)
                 (with-elapsed-timer
                     ,(format "Loading package %s" name-string)
                   (if (not ,(if (stringp name)
                                 `(load ,name t)
                               `(require ',name nil t)))
                       (message "Could not load package %s" ,name-string)
                     ,pre-init-body
                     ,init-body
                     ,config-body
                     t))))))))

(put 'use-package 'lisp-indent-function 1)

(provide 'use-package)

;;; use-package.el ends here
