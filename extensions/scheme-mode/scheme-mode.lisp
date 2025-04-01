(in-package :lem-scheme-mode)

(define-major-mode scheme-mode language-mode
    (:name "Scheme"
     :keymap *scheme-mode-keymap*
     :syntax-table lem-scheme-syntax:*syntax-table*
     :mode-hook *scheme-mode-hook*)
  (modeline-add-status-list 'connection-mode-line (current-buffer))
  (setf (variable-value 'beginning-of-defun-function) 'scheme-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'scheme-end-of-defun)
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'scheme-calc-indent)
  (setf (variable-value 'line-comment) ";")
  (setf (variable-value 'insertion-line-comment) ";; ")
  (setf (variable-value 'language-mode-tag) 'scheme-mode)
  (setf (variable-value 'find-definitions-function) 'scheme-find-definitions)
  (setf (variable-value 'find-references-function) 'scheme-find-references)
  (setf (variable-value 'completion-spec) 'completion-symbol)
  (setf (variable-value 'idle-function) 'scheme-idle-function)
  (set-syntax-parser lem-scheme-syntax:*syntax-table* (make-tmlanguage-scheme)))

(define-keys *scheme-mode-keymap*
  ("C-M-q"       'scheme-indent-sexp)
  ("C-c M-p"     'scheme-set-library)
  ("C-c M-:"     'scheme-eval-string)
  ("C-c C-e"     'scheme-eval-last-expression)
  ("C-x C-e"     'scheme-eval-last-expression)
  ("C-M-x"       'scheme-eval-define)
  ("C-c C-r"     'scheme-eval-region)
  ("C-c C-l"     'scheme-load-file)
  ("C-c M-c"     'scheme-remove-notes)
  ("C-c C-k"     'scheme-compile-and-load-file)
  ("C-c C-c"     'scheme-compile-define)
  ("C-c Return"  'scheme-macroexpand)
  ("C-c M-m"     'scheme-macroexpand-all)
  ("Space"       'scheme-insert-space-and-autodoc)
  ("M-a"         'scheme-autodoc)
  ("C-c C-d C-a" 'scheme-autodoc-with-typeout)
  ("C-c C-d d"   'scheme-describe-symbol)
  ("C-c C-z"     'scheme-switch-to-repl-buffer)
  ("C-c z"       'scheme-switch-to-repl-buffer)
  ("C-c C-b"     'scheme-connection-list)
  ("C-c g"       'scheme-interrupt))
;;(define-key *scheme-mode-keymap* "C-c C-d C-a" 'scheme-autodoc)


;; There are two methods for connecting to Scheme processing system.
;; One is 'scheme process' executed by async-process library
;; (https://github.com/cxxxr/async-process is required).
;; Another is 'scheme slime' function using r7rs-swank server
;; (https://github.com/ecraven/r7rs-swank is required).
;;
;; By default, 'scheme process' is used to evaluate a scheme program.
;; But, after executing 'scheme-slime' or 'scheme-slime-connect' command,
;; 'scheme slime' function is used to evaluate a scheme program.

;; settings for 'scheme process'
;;  *use-scheme-process*
;;    t     : enable scheme process commands
;;    nil   : disable scheme process commands
(defvar *use-scheme-process* t)
(defvar *scheme-run-command* '("gosh" "-i"))
(defvar *scheme-load-command* "load") ; it might be "include" for R6RS Scheme

;; settings for 'scheme slime'
;;  *use-scheme-slime*
;;    t     : enable scheme slime commands
;;    :auto : enable only 'scheme-slime' and 'scheme-slime-connect' commands.
;;            other scheme slime commands are disabled until executing
;;            'scheme-slime' or 'scheme-slime-connect' command.
;;    nil   : disable scheme slime commands
;;  *use-scheme-set-library*
;;    t     : use 'scheme-set-library' command for each buffer
;;    :repl : use 'scheme-set-library' command for only repl
;;    nil   : disable 'scheme-set-library' command
;;  *use-scheme-autodoc*
;;    t     : display function information in popup window when space key is input
;;    :auto : display function information in popup window automatically
;;    nil   : don't display function information
(defvar *use-scheme-slime* :auto)
(defvar *use-scheme-set-library* :repl)
(defvar *use-scheme-autodoc* t)
(defvar *scheme-swank-server-run-command*
  '("gosh" "-AC:/work/r7rs-swank" "-e(begin (import (gauche-swank)) (start-swank ,port))"))

;; settings for scheme repl
;;  *use-scheme-repl-shortcut*
;;    t     : enable scheme repl shortcut command
;;    nil   : disable scheme repl shortcut command
(defvar *use-scheme-repl-shortcut* nil)


;; this is used only when no scheme slime connection exists
(defvar *scheme-completion-names*
  (lem-scheme-syntax:get-scheme-completion-data))

;; base data is defined in syntax-data.lisp
(defun scheme-keyword-data ()
  lem-scheme-syntax.data::*scheme-data*)
(defun (setf scheme-keyword-data) (v)
  ;; update base data
  (setf lem-scheme-syntax.data::*scheme-data* v)
  ;; update completion data
  (setf *scheme-completion-names*
        (lem-scheme-syntax:get-scheme-completion-data))
  ;; update indentation data
  (setf lem-scheme-syntax.indent::*static-indent-table*
        (lem-scheme-syntax.indent::make-static-indent-table))
  v)

(defun scheme-calc-indent (point)
  (lem-scheme-syntax:calc-indent point))

(defun scheme-beginning-of-defun (point n)
  (lem-scheme-syntax:beginning-of-defun point (- n)))

(defun scheme-end-of-defun (point n)
  (if (minusp n)
      (scheme-beginning-of-defun point (- n))
      (dotimes (_ n)
        (with-point ((p point))
          (cond ((and (lem-scheme-syntax:beginning-of-defun p -1)
                      (point<= p point)
                      (or (form-offset p 1)
                          (progn
                            (move-point point p)
                            (return)))
                      (point< point p))
                 (move-point point p)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1)))
                (t
                 (form-offset point 1)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1))))))))

(define-command scheme-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-points (current-point) end))))

(define-command scheme-scratch () ()
  (let ((buffer (primordial-buffer)))
    (change-buffer-mode buffer 'scheme-mode)
    (switch-to-buffer buffer)))

(defun disable-commands (cmds &optional cmds-backup-table)
  (loop :for cmd in cmds
        :for cmd-str := (string-downcase cmd)
        :do (alexandria:when-let (value (find-command cmd-str))
              (when cmds-backup-table
                (add-command cmd-str
                             value
                             cmds-backup-table))
              (remove-command cmd-str))))

(defun enable-commands (cmds cmds-backup-table)
  (loop :for cmd in cmds
        :for cmd-str := (string-downcase cmd)
        :do (unless (exist-command-p cmd-str)
              (alexandria:when-let (value (find-command cmd-str cmds-backup-table))
                (add-command cmd-str value)))))

;; disable scheme process commands
(let ((cmds-1 '(scheme-kill-process)))
  (defun disable-scheme-process-commands ()
    (unless *use-scheme-process*
      (disable-commands cmds-1)))
  (add-hook *after-init-hook* 'disable-scheme-process-commands))

;; disable/enable scheme slime commands
(let ((cmds-backup-table (make-command-table))
      (cmds-1 '(scheme-slime-connect
                scheme-slime))
      (cmds-2 '(scheme-repl-interrupt
                scheme-connection-list
                scheme-set-library
                scheme-interrupt
                scheme-eval-string
                scheme-eval-define
                scheme-echo-arglist
                scheme-autodoc-with-typeout
                scheme-autodoc
                scheme-remove-notes
                scheme-compile-and-load-file
                scheme-compile-region
                scheme-compile-define
                scheme-macroexpand
                scheme-macroexpand-all
                scheme-describe-symbol
                scheme-slime-quit
                scheme-slime-restart)))
  (defun disable-scheme-slime-commands ()
    (case *use-scheme-slime*
      ((:auto)
       (disable-commands cmds-2 cmds-backup-table))
      ((nil)
       (disable-commands cmds-1)
       (disable-commands cmds-2))))
  (defun enable-scheme-slime-commands ()
    (case *use-scheme-slime*
      ((:auto)
       (enable-commands  cmds-2 cmds-backup-table))))
  (add-hook *after-init-hook* 'disable-scheme-slime-commands))

(define-file-type ("scm" "sld" "rkt" "ss") scheme-mode)
