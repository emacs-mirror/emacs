;;; ob-java.el --- org-babel functions for java evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2011-2022 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;          Dan Davison
;; Maintainer: Ian Martins <ianxm@jhu.edu>
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

;; Org-Babel support for evaluating java source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("java" . "java"))

(defvar org-babel-temporary-directory) ; from ob-core

(defvar org-babel-default-header-args:java '((:results . "output")
					     (:dir . "."))
  "Default header args for java source blocks.
The docs say functional mode should be the default [1], but
ob-java didn't originally support functional mode, so we keep
scripting mode as the default for now to maintain previous
behavior.

Most languages write tempfiles to babel's temporary directory,
but ob-java originally had to write them to the current
directory, so we keep that as the default behavior.

[1] https://orgmode.org/manual/Results-of-Evaluation.html")

(defconst org-babel-header-args:java
  '((dir       . :any)
    (classname . :any)
    (imports   . :any)
    (cmpflag   . :any)
    (cmdline   . :any)
    (cmdarg    . :any))
  "Java-specific header arguments.")

(defcustom org-babel-java-command "java"
  "Name of the java command.
May be either a command in the path, like java or an absolute
path name, like /usr/local/bin/java.  Parameters may be used,
like java -verbose."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom org-babel-java-compiler "javac"
  "Name of the java compiler.
May be either a command in the path, like javac or an absolute
path name, like /usr/local/bin/javac.  Parameters may be used,
like javac -verbose."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom org-babel-java-hline-to "null"
  "Replace hlines in incoming tables with this when translating to java."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom org-babel-java-null-to 'hline
  "Replace `null' in java tables with this before returning."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'symbol)

(defconst org-babel-java--package-re (rx line-start (0+ space) "package"
					 (1+ space) (group (1+ (in alnum ?_ ?.))) ; capture the package name
					 (0+ space) ?\; line-end)
  "Regexp for the package statement.")
(defconst org-babel-java--imports-re (rx line-start (0+ space) "import"
                                         (opt (1+ space) "static")
					 (1+ space) (group (1+ (in alnum ?_ ?. ?*))) ; capture the fully qualified class name
					 (0+ space) ?\; line-end)
  "Regexp for import statements.")
(defconst org-babel-java--class-re (rx line-start (0+ space) (opt (seq "public" (1+ space)))
				       "class" (1+ space)
				       (group (1+ (in alnum ?_))) ; capture the class name
				       (0+ space) ?{)
  "Regexp for the class declaration.")
(defconst org-babel-java--main-re
  (rx line-start (0+ space) "public"
      (1+ space) "static"
      (1+ space) "void"
      (1+ space) "main"
      (0+ space) ?\(
      (0+ space) "String"
      (1+ (in alnum ?_ ?\[ ?\] space)) ; "[] args" or "args[]"
      ?\)
      (0+ space) (opt "throws" (1+ (in alnum ?_ ?, ?. space)))
      ?{)
  "Regexp for the main method declaration.")
(defconst org-babel-java--any-method-re
  (rx line-start
      (0+ space) (opt (seq (1+ alnum) (1+ space)))   ; visibility
      (opt (seq "static" (1+ space)))                ; binding
      (1+ (in alnum ?_ ?\[ ?\]))                     ; return type
      (1+ space) (1+ (in alnum ?_))                  ; method name
      (0+ space) ?\(
      (0+ (in alnum ?_ ?\[ ?\] ?, space)) ; params
      ?\)
      (0+ space) (opt "throws" (1+ (in alnum ?_ ?, ?. space)))
      ?{)
  "Regexp for any method.")
(defconst org-babel-java--result-wrapper "\n    public static String __toString(Object val) {
        if (val instanceof String) {
            return \"\\\"\" + val + \"\\\"\";
        } else if (val == null) {
            return \"null\";
        } else if (val.getClass().isArray()) {
            StringBuffer sb = new StringBuffer();
            Object[] vals = (Object[])val;
            sb.append(\"[\");
            for (int ii=0; ii<vals.length; ii++) {
                sb.append(__toString(vals[ii]));
                if (ii<vals.length-1)
                    sb.append(\",\");
            }
            sb.append(\"]\");
            return sb.toString();
        } else if (val instanceof List) {
            StringBuffer sb = new StringBuffer();
            List vals = (List)val;
            sb.append(\"[\");
            for (int ii=0; ii<vals.size(); ii++) {
                sb.append(__toString(vals.get(ii)));
                if (ii<vals.size()-1)
                    sb.append(\",\");
            }
            sb.append(\"]\");
            return sb.toString();
        } else {
            return String.valueOf(val);
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedWriter output = new BufferedWriter(new FileWriter(\"%s\"));
        output.write(__toString(_main(args)));
        output.close();
    }"
  "Code to inject into a class so that we can capture the value it returns.
This implementation was inspired by ob-python, although not as
elegant.  This modified the source block to write out the value
it wants to return to a temporary file so that ob-java can read
it back.  The name of the temporary file to write must be
replaced in this string.")

(defun org-babel-execute:java (body params)
  "Execute a java source block with BODY code and PARAMS params."
  (let* (;; allow header overrides
         (org-babel-java-compiler
          (or (cdr (assq :javac params))
              org-babel-java-compiler))
         (org-babel-java-command
          (or (cdr (assq :java params))
              org-babel-java-command))
         ;; if true, run from babel temp directory
         (run-from-temp (not (cdr (assq :dir params))))
         ;; class and package
         (fullclassname (or (cdr (assq :classname params))
                            (org-babel-java-find-classname body)))
         ;; just the class name
         (classname (car (last (split-string fullclassname "\\."))))
         ;; just the package name
         (packagename (if (string-match-p "\\." fullclassname)
                          (file-name-base fullclassname)))
         ;; the base dir that contains the top level package dir
         (basedir (file-name-as-directory
                   (if run-from-temp
                       (org-babel-temp-directory)
                     default-directory)))
         ;; the dir to write the source file
         (packagedir (if (and (not run-from-temp) packagename)
                         (file-name-as-directory
                          (concat basedir (replace-regexp-in-string "\\." "/" packagename)))
                       basedir))
         ;; the filename of the source file
         (src-file (concat packagedir classname ".java"))
         ;; compiler flags
         (cmpflag (or (cdr (assq :cmpflag params)) ""))
         ;; runtime flags
         (cmdline (or (cdr (assq :cmdline params)) ""))
         ;; command line args
         (cmdargs (or (cdr (assq :cmdargs params)) ""))
         ;; the command to compile and run
         (cmd (concat org-babel-java-compiler " " cmpflag " "
                      (org-babel-process-file-name src-file 'noquote)
                      " && " org-babel-java-command
                      " -cp " (org-babel-process-file-name basedir 'noquote)
                      " " cmdline " " (if run-from-temp classname fullclassname)
                      " " cmdargs))
         ;; header args for result processing
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (result-file (and (eq result-type 'value)
                           (org-babel-temp-file "java-")))
         ;; the expanded body of the source block
         (full-body (org-babel-expand-body:java body params)))

    ;; created package-name directories if missing
    (unless (or (not packagedir) (file-exists-p packagedir))
      (make-directory packagedir 'parents))

    ;; write the source file
    (setq full-body (org-babel-java--expand-for-evaluation
                     full-body run-from-temp result-type result-file))
    (with-temp-file src-file (insert full-body))

    ;; compile, run, process result
    (org-babel-reassemble-table
     (org-babel-java-evaluate cmd result-type result-params result-file)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

;; helper functions

(defun org-babel-java-find-classname (body)
  "Try to find fully qualified class name in BODY.
Look through BODY for the package and class.  If found, put them
together into a fully qualified class name and return.  Else just
return class name.  If that isn't found either, default to Main."
  (let ((package (if (string-match org-babel-java--package-re body)
                     (match-string 1 body)))
        (class (if (string-match org-babel-java--class-re body)
                   (match-string 1 body))))
    (or (and package class (concat package "." class))
        (and class class)
        (and package (concat package ".Main"))
        "Main")))

(defun org-babel-java--expand-for-evaluation (body suppress-package-p result-type result-file)
  "Expand source block for evaluation.
In order to return a value we have to add a __toString method.
In order to prevent classes without main methods from erroring we
add a dummy main method if one is not provided.  These
manipulations are done outside of `org-babel--expand-body' so
that they are hidden from tangles.

BODY is the file content before instrumentation.

SUPPRESS-PACKAGE-P if true, suppress the package statement.

RESULT-TYPE is taken from params.

RESULT-FILE is the temp file to write the result."
  (with-temp-buffer
    (insert body)

    ;; suppress package statement
    (goto-char (point-min))
    (when (and suppress-package-p
               (re-search-forward org-babel-java--package-re nil t))
      (replace-match ""))

    ;; add a dummy main method if needed
    (goto-char (point-min))
    (when (not (re-search-forward org-babel-java--main-re nil t))
      (org-babel-java--move-past org-babel-java--class-re)
      (insert "\n    public static void main(String[] args) {
        System.out.print(\"success\");
    }\n\n"))

    ;; special handling to return value
    (when (eq result-type 'value)
      (goto-char (point-min))
      (org-babel-java--move-past org-babel-java--class-re)
      (insert (format org-babel-java--result-wrapper
                      (org-babel-process-file-name result-file 'noquote)))
      (search-forward "public static void main(") ; rename existing main
      (replace-match "public static Object _main("))

    ;; add imports
    (org-babel-java--import-maybe "java.util" "List")
    (org-babel-java--import-maybe "java.util" "Arrays")
    (org-babel-java--import-maybe "java.io" "BufferedWriter")
    (org-babel-java--import-maybe "java.io" "FileWriter")
    (org-babel-java--import-maybe "java.io" "IOException")

    (buffer-string)))

(defun org-babel-java--move-past (re)
  "Move point past the first occurrence of the given regexp RE."
  (while (re-search-forward re nil t)
    (goto-char (1+ (match-end 0)))))

(defun org-babel-java--import-maybe (package class)
  "Import from PACKAGE the given CLASS if it is used and not already imported."
  (let (class-found import-found)
    (goto-char (point-min))
    (setq class-found (re-search-forward class nil t))
    (goto-char (point-min))
    (setq import-found
          (re-search-forward (concat "^import .*" package ".*\\(?:\\*\\|" class "\\);") nil t))
    (when (and class-found (not import-found))
      (org-babel-java--move-past org-babel-java--package-re)
      (insert (concat "import " package "." class ";\n")))))

(defun org-babel-expand-body:java (body params)
  "Expand BODY with PARAMS.
BODY could be a few statements, or could include a full class
definition specifying package, imports, and class.  Because we
allow this flexibility in what the source block can contain, it
is simplest to expand the code block from the inside out."
  (let* ((fullclassname (or (cdr (assq :classname params)) ; class and package
                            (org-babel-java-find-classname body)))
         (classname (car (last (split-string fullclassname "\\.")))) ; just class name
         (packagename (if (string-match-p "\\." fullclassname)       ; just package name
                          (file-name-base fullclassname)))
         (var-lines (org-babel-variable-assignments:java params))
         (imports-val (assq :imports params))
         (imports (if imports-val
                      (split-string (org-babel-read (cdr imports-val) nil) " ")
                    nil)))
    (with-temp-buffer
      (insert body)

      ;; wrap main.  If there are methods defined, but no main method
      ;; and no class, wrap everything in a generic main method.
      (goto-char (point-min))
      (when (and (not (re-search-forward org-babel-java--main-re nil t))
                 (not (re-search-forward org-babel-java--any-method-re nil t)))
        (org-babel-java--move-past org-babel-java--package-re) ; if package is defined, move past it
        (org-babel-java--move-past org-babel-java--imports-re) ; if imports are defined, move past them
        (insert "public static void main(String[] args) {\n")
        (indent-code-rigidly (point) (point-max) 4)
        (goto-char (point-max))
        (insert "\n}"))

      ;; wrap class.  If there's no class, wrap everything in a
      ;; generic class.
      (goto-char (point-min))
      (when (not (re-search-forward org-babel-java--class-re nil t))
        (org-babel-java--move-past org-babel-java--package-re) ; if package is defined, move past it
        (org-babel-java--move-past org-babel-java--imports-re) ; if imports are defined, move past them
        (insert (concat "\npublic class " (file-name-base classname) " {\n"))
        (indent-code-rigidly (point) (point-max) 4)
        (goto-char (point-max))
        (insert "\n}"))
      (goto-char (point-min))

      ;; insert variables from source block headers
      (when var-lines
        (goto-char (point-min))
        (org-babel-java--move-past org-babel-java--class-re)   ; move inside class
        (insert (mapconcat 'identity var-lines "\n"))
        (insert "\n"))

      ;; add imports from source block headers
      (when imports
        (goto-char (point-min))
        (org-babel-java--move-past org-babel-java--package-re) ; if package is defined, move past it
        (insert (mapconcat (lambda (package) (concat "import " package ";")) imports "\n") "\n"))

      ;; add package at the top
      (goto-char (point-min))
      (when (and packagename (not (re-search-forward org-babel-java--package-re nil t)))
        (insert (concat "package " packagename ";\n")))

      ;; return expanded body
      (buffer-string))))

(defun org-babel-variable-assignments:java (params)
  "Return a list of java statements assigning the block's variables.
variables are contained in PARAMS."
  (mapcar
   (lambda (pair)
     (let* ((type-data (org-babel-java-val-to-type (cdr pair)))
            (basetype (car type-data))
            (var-to-java (lambda (var) (funcall #'org-babel-java-var-to-java var basetype))))
       (format "    static %s %s = %s;"
               (cdr type-data)                     ; type
               (car pair)                          ; name
               (funcall var-to-java (cdr pair))))) ; value
   (org-babel--get-vars params)))

(defun org-babel-java-var-to-java (var basetype)
  "Convert an elisp value to a java variable.
Convert an elisp value, VAR, of type BASETYPE into a string of
java source code specifying a variable of the same value."
  (cond ((and (sequencep var) (not (stringp var)))
         (let ((var-to-java (lambda (var) (funcall #'org-babel-java-var-to-java var basetype))))
           (concat "Arrays.asList(" (mapconcat var-to-java var ", ") ")")))
        ((eq var 'hline) org-babel-java-hline-to)
        ((eq basetype 'integerp) (format "%d" var))
        ((eq basetype 'floatp) (format "%f" var))
        ((eq basetype 'stringp) (if (and (stringp var) (string-match-p ".\n+." var))
                                    (error "Java does not support multiline string literals")
                                  (format "\"%s\"" var)))))

(defun org-babel-java-val-to-type (val)
  "Determine the type of VAL.
Return (BASETYPE . LISTTYPE), where BASETYPE is a symbol
representing the type of the individual items in VAL, and
LISTTYPE is a string name of the type parameter for a container
for BASETYPE items."
  (let* ((basetype (org-babel-java-val-to-base-type val))
         (basetype-str (pcase basetype
                         (`integerp "Integer")
                         (`floatp "Double")
                         (`stringp "String")
                         (_ (error "Unknown type %S" basetype)))))
    (cond
     ((and (listp val) (listp (car val))) ; a table
      (cons basetype (format "List<List<%s>>" basetype-str)))
     ((or (listp val) (vectorp val))      ; a list declared in the source block header
      (cons basetype (format "List<%s>" basetype-str)))
     (t                                   ; return base type
      (cons basetype basetype-str)))))

(defun org-babel-java-val-to-base-type (val)
  "Determine the base type of VAL.
VAL may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
              (pcase (org-babel-java-val-to-base-type v)
                (`stringp (setq type 'stringp))
                (`floatp
                 (when (or (not type) (eq type 'integerp))
                   (setq type 'floatp)))
                (`integerp
                 (unless type (setq type 'integerp)))))
            val)
      type))
   (t 'stringp)))

(defun org-babel-java-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or vector, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq 'null el)
                                 org-babel-java-null-to
                               el))
                res)
      res)))

(defun org-babel-java-evaluate (cmd result-type result-params result-file)
  "Evaluate using an external java process.
CMD the command to execute.

If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value
returned by the source block, as elisp.

RESULT-PARAMS input params used to format the response.

RESULT-FILE filename of the tempfile to store the returned value in
for `value' RESULT-TYPE.  Not used for `output' RESULT-TYPE."
  (let ((raw (pcase result-type
               (`output (org-babel-eval cmd ""))
               (`value (org-babel-eval cmd "")
                       (org-babel-eval-read-file result-file)))))
    (org-babel-result-cond result-params raw
                           (org-babel-java-table-or-string raw))))

(provide 'ob-java)

;;; ob-java.el ends here
