;;; em-extpipe.el --- external shell pipelines  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

;; When constructing shell pipelines that will move a lot of data, it
;; is a good idea to bypass Eshell's own pipelining support and use
;; the operating system shell's instead.  This module tries to make
;; that easy to do.

;;; Code:

(require 'cl-lib)
(require 'esh-arg)
(require 'esh-cmd)
(require 'esh-io)
(require 'esh-util)

(eval-when-compile (require 'files-x))

;;; Functions:

(defun eshell-extpipe-initialize () ;Called from `eshell-mode' via intern-soft!
  "Initialize external pipelines support."
  (when (boundp 'eshell-special-chars-outside-quoting)
    (setq-local
     eshell-special-chars-outside-quoting
     (append eshell-special-chars-outside-quoting (list ?\*))))
  (add-hook 'eshell-parse-argument-hook
            #'eshell-parse-external-pipeline -20 t)
  (add-hook 'eshell-pre-rewrite-command-hook
            #'eshell-rewrite-external-pipeline -20 t))

(defmacro eshell-extpipe--or-with-catch (&rest disjuncts)
  "Evaluate DISJUNCTS like `or' but catch `eshell-incomplete'.

If `eshell-incomplete' is thrown during the evaluation of a
disjunct, that disjunct yields nil."
  (let ((result (gensym)))
    `(let (,result)
       (or ,@(cl-loop for disjunct in disjuncts collect
                      `(if (catch 'eshell-incomplete
                             (ignore (setq ,result ,disjunct)))
                           nil
                         ,result))))))

(defun eshell-parse-external-pipeline ()
  "Parse a pipeline intended for execution by the external shell.

A sequence of arguments is rewritten to use the operating system
shell when it contains `*|', `*<' or `*>', where the asterisk is
preceded by whitespace or located at the start of input.

The command extends to the next `|' character which is not
preceded by an unescaped asterisk following whitespace, or the
end of input, except that any Eshell-specific output redirections
occurring at the end are excluded.  Any other `<' or `>'
appearing before the end of the command are treated as though
preceded by (whitespace and) an asterisk.

For example,

    foo <bar *| baz >#<buffer quux>

is equivalent to

    sh -c \"foo <bar | baz\" >#<buffer quux>

when `shell-file-name' is `sh' and `shell-command-switch' is
`-c', but in

    foo >#<buffer quux> *| baz

and

    foo *| baz >#<buffer quux> --some-argument

the Eshell-specific redirect will be passed on to the operating
system shell, probably leading to undesired results.

This function must appear early in `eshell-parse-argument-hook'
to ensure that operating system shell syntax is not interpreted
as though it were Eshell syntax."
  ;; Our goal is to wrap the external command to protect it from the
  ;; other members of `eshell-parse-argument-hook'.  We must avoid
  ;; misinterpreting a quoted `*|', `*<' or `*>' as indicating an
  ;; external pipeline, hence the structure of the loop in `findbeg1'.
  (cl-flet
      ((findbeg1 (pat &optional go (bound (point-max)))
         (let* ((start (point))
                (result
                 (catch 'found
                   (while (> bound (point))
                     (let* ((found
                             (save-excursion
                               (re-search-forward
                                "\\(?:#?'\\|\"\\|\\\\\\)" bound t)))
                            (next (or (and found (match-beginning 0))
                                      bound)))
                       (if (re-search-forward pat next t)
                           (throw 'found (match-beginning 1))
                         (goto-char next)
                         (while (eshell-extpipe--or-with-catch
                                 (eshell-parse-lisp-argument)
                                 (eshell-parse-backslash)
                                 (eshell-parse-double-quote)
                                 (eshell-parse-literal-quote)))
                         ;; Guard against an infinite loop if none of
                         ;; the parsers moved us forward.
                         (unless (or (> (point) next) (eobp))
                           (forward-char 1))))))))
           (goto-char (if (and result go) (match-end 0) start))
           result)))
    (unless (or eshell-current-argument eshell-current-quoted)
      (let ((beg (point)) end
            (next-marked (findbeg1 "\\(?:\\=\\|\\s-\\)\\(\\*[|<>]\\)"))
            (next-unmarked
             (or (findbeg1 "\\(?:\\=\\|[^*]\\|\\S-\\*\\)\\(|\\)")
                 (point-max))))
        (when (and next-marked (> next-unmarked next-marked)
                   (or (> next-marked (point))
                       (looking-back "\\`\\|\\s-" nil)))
          ;; Skip to the final segment of the external pipeline.
          (while (findbeg1 "\\(?:\\=\\|\\s-\\)\\(\\*|\\)" t))
          ;; Find output redirections.
          (while (findbeg1
                  "\\([0-9]?>+&?[0-9]?\\s-*\\S-\\)" t next-unmarked)
            ;; Is the output redirection Eshell-specific?  We have our
            ;; own logic, rather than calling `eshell-parse-argument',
            ;; to avoid specifying here all the possible cars of
            ;; parsed special references -- `get-buffer-create' etc.
            (forward-char -1)
            (let ((this-end
                   (save-match-data
                     (cond ((looking-at "#<")
                            (forward-char 1)
                            (1+ (eshell-find-delimiter ?\< ?\>)))
                           ((and (looking-at "/\\S-+")
                                 (assoc (match-string 0)
                                        eshell-virtual-targets))
                            (match-end 0))))))
              (cond ((and this-end end)
                     (goto-char this-end))
                    (this-end
                     (goto-char this-end)
                     (setq end (match-beginning 0)))
                    (t
                     (setq end nil)))))
          ;; We've moved past all Eshell-specific output redirections
          ;; we could find.  If there is only whitespace left, then
          ;; `end' is right before redirections we should exclude;
          ;; otherwise, we must include everything.
          (unless (and end (skip-syntax-forward "\s" next-unmarked)
                       (= next-unmarked (point)))
            (setq end next-unmarked))
          (let ((cmd (string-trim
                      (buffer-substring-no-properties beg end))))
            (goto-char end)
            ;; We must now drop the asterisks, unless quoted/escaped.
            (with-temp-buffer
              (insert cmd)
              (goto-char (point-min))
              (cl-loop
               for next = (findbeg1 "\\(?:\\=\\|\\s-\\)\\(\\*[|<>]\\)" t)
               while next do (forward-char -2) (delete-char 1))
              (eshell-finish-arg
               `(eshell-external-pipeline ,(buffer-string))))))))))

(defun eshell-rewrite-external-pipeline (terms)
  "Rewrite an external pipeline in TERMS as parsed by
`eshell-parse-external-pipeline', which see."
  (while terms
    (when (and (listp (car terms))
               (eq (caar terms) 'eshell-external-pipeline))
      (with-connection-local-variables
       (setcdr terms (cl-list*
                      shell-command-switch (cadar terms) (cdr terms)))
       (setcar terms shell-file-name)))
    (setq terms (cdr terms))))

(defsubst eshell-external-pipeline (&rest _args)
  "Stub to generate an error if a pipeline is not rewritten."
  (error "Unhandled external pipeline in input text"))

(provide 'em-extpipe)
;;; esh-extpipe.el ends here
