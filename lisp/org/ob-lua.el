;;; ob-lua.el --- Org Babel functions for Lua evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2016-2026 Free Software Foundation, Inc.

;; Authors: Dieter Schoen
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; Org Babel support for evaluating Lua source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)
(require 'cl-lib)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("lua" . "lua"))

(defvar org-babel-default-header-args:lua '())

(defcustom org-babel-lua-command "lua"
  "Name of the command for executing Lua code."
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-babel
  :type 'string)

(defcustom org-babel-lua-mode 'lua-mode
  "Preferred Lua mode for use in running Lua interactively.
This will typically be `lua-mode'."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'symbol)

(defcustom org-babel-lua-hline-to "None"
  "Replace `hlines' in incoming tables with this when translating to Lua."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'string)

(defcustom org-babel-lua-None-to 'hline
  "Replace `None' in Lua tables with this before returning."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'symbol)

(defcustom org-babel-lua-multiple-values-separator ", "
  "Separate multiple values with this string."
  :group 'org-babel
  :package-version '(Org . "9.7")
  :type 'string)

(defun org-babel-execute:lua (body params)
  "Execute Lua BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assq :session params)))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (eq result-type 'value)
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
         (full-body
	  (org-babel-expand-body:generic
	   (concat body (if return-val (format "\nreturn %s" return-val) ""))
	   params (org-babel-variable-assignments:lua params)))
         (result (org-babel-lua-evaluate
		  full-body result-type result-params preamble)))
    (when (and session (not (equal session "none")))
      (user-error "Sessions not supported for Lua"))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

;; helper functions

(defun org-babel-variable-assignments:lua (params)
  "Return a list of Lua statements assigning the block's variables.
The variable definitions are defining in PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-lua-var-to-lua (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-lua-var-to-lua (var)
  "Convert an Emacs Lisp value to a Lua variable.
Convert an Emacs Lisp value, VAR, into a string of Lua source code
specifying a variable of the same value."
  (if (listp var)
      (if (and (= 1 (length var)) (not (listp (car var))))
          (org-babel-lua-var-to-lua (car var))
        (if (and
             (= 2 (length var))
             (not (listp (car var))))
            (concat
             (substring-no-properties (car var))
             "="
             (org-babel-lua-var-to-lua (cdr var)))
          (concat "{" (mapconcat #'org-babel-lua-var-to-lua var ", ") "}")))
    (if (eq var 'hline)
        org-babel-lua-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "[=[%s]=]" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-lua-table-or-string (results)
  "Convert RESULTS into an appropriate Emacs Lisp value.
If the results look like a list or tuple, then convert them into an
Emacs Lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq el 'None)
				 org-babel-lua-None-to el))
                res)
      res)))

(defvar org-babel-lua-buffers '((:default . "*Lua*")))

(defvar org-babel-lua-eoe-indicator "--eoe"
  "A string to indicate that evaluation has completed.")

(defvar org-babel-lua-wrapper-method
  "
function main()
%s
end

function dump(it, indent)
   if indent == nil then
      indent = ''
   end

   if type(it) == 'table' and %s then
      local result = ''

      if #indent ~= 0 then
         result = result .. '\\n'
      end

      local keys = {}
      for key in pairs(it) do
        table.insert(keys, key)
      end

      table.sort(keys)

      for index, key in pairs(keys) do
         local value = it[key]
         result = result
            .. indent
            .. dump(key)
            .. ' = '
            .. dump(value, indent .. '  ')
         if index ~= #keys then
            result = result .. '\\n'
         end
      end

      return result
   else
      return string.gsub(tostring(it), '\"', '\\\"')
   end
end

function combine(...)
  local result = {}

  for index = 1, select('#', ...) do
    result[index] = dump(select(index, ...))
  end

  if #result == 1 then
    local value = result[1]
    if string.find(value, '[%%(%%[{]') == 1 then
      return '\"' .. value .. '\"'
    else
      return value
    end
  end

  return '\"' .. table.concat(result, '%s') .. '\"'
end

output = io.open('%s', 'w')
output:write(combine(main()))
output:close()")

(defun org-babel-lua-evaluate
    (body &optional result-type result-params preamble)
  "Evaluate BODY in external Lua process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as Emacs Lisp.
RESULT-PARAMS list all the :result header arg parameters.
PREAMBLE string is appended to BODY."
  (let ((raw
         (pcase result-type
           (`output (org-babel-eval org-babel-lua-command
				    (concat preamble (and preamble "\n")
					    body)))
           (`value (let ((tmp-file (org-babel-temp-file "lua-")))
		     (org-babel-eval
		      org-babel-lua-command
		      (concat
		       preamble (and preamble "\n")
		       (format
                        org-babel-lua-wrapper-method
			(mapconcat
			 (lambda (line) (format "\t%s" line))
			 (split-string
			  (org-remove-indentation
			   (org-trim body))
			  "[\r\n]")
                         "\n")
                        (if (member "pp" result-params)
                            "true" "false")
                        org-babel-lua-multiple-values-separator
			(org-babel-process-file-name tmp-file 'noquote))))
		     (org-babel-eval-read-file tmp-file))))))
    (org-babel-result-cond result-params
      raw
      (org-babel-lua-table-or-string (org-trim raw)))))

(provide 'ob-lua)

;;; ob-lua.el ends here
