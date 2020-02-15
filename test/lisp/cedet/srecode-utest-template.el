;;; srecode/test.el --- SRecode Core Template tests.

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

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
;;
;; Tests of SRecode template insertion routines and tricks.
;;


(require 'srecode/map)
(require 'srecode/insert)
(require 'srecode/dictionary)


;;; Code:

;;; MAP DUMP TESTING
(defun srecode-utest-map-reset ()
  "Reset, then dump the map of SRecoder templates.
Probably should be called 'describe-srecode-maps'."
  (interactive)
  (message "SRecode Template Path: %S" srecode-map-load-path)
  ;; Interactive call allows us to dump.
  (call-interactively 'srecode-get-maps)
  (switch-to-buffer "*SRECODE MAP*")
  (message (buffer-string))
  )

;;; OUTPUT TESTING
;;
(defclass srecode-utest-output ()
  ((point        :initarg  :point
                 :type     string
                 :documentation
                 "Name of this test point.")
   (name         :initarg  :name
		 :type     string
	         :documentation
		 "Name of the template tested.")
   (output       :initarg  :output
		 :type     string
	         :documentation
		 "Expected output of the template.")
   (dict-entries :initarg  :dict-entries
		 :initform nil
		 :type     list
		 :documentation
		 "Additional dictionary entries to specify.")
   (pre-fill     :initarg  :pre-fill
		 :type     (or null string)
	         :initform nil
	         :documentation
		 "Text to prefill a buffer with.
Place cursor on the ! and delete it.
If there is a second !, the put the mark there."))
  "A single template test.")

(cl-defmethod srecode-utest-test ((o srecode-utest-output))
  "Perform the insertion and test the output.
Assumes that the current buffer is the testing buffer.
Return NIL on success, or a diagnostic on failure."
  (let ((fail nil))
    (catch 'fail-early
      (with-slots (name (output-1 output) dict-entries pre-fill) o
        ;; Prepare buffer: erase content and maybe insert pre-fill
        ;; content.
        (erase-buffer)
        (insert (or pre-fill ""))
        (goto-char (point-min))
        (let ((start nil))
          (when (re-search-forward "!" nil t)
	    (goto-char (match-beginning 0))
	    (setq start (point))
	    (replace-match ""))
          (when (re-search-forward "!" nil t)
	    (push-mark (match-beginning 0) t t)
	    (replace-match ""))
          (when start (goto-char start)))

        ;; Find a template, perform an insertion and validate the output.
        (let ((dict (srecode-create-dictionary))
	      (temp (or (srecode-template-get-table
		         (srecode-table) name "test" 'tests)
		        (progn
		          (srecode-map-update-map)
		          (srecode-template-get-table
		           (srecode-table) name "test" 'tests))
                        (progn
                          (setq fail (format "Test template \"%s\" for `%s' not loaded!"
			                     name major-mode))
                          (throw 'fail-early t)
                          )))
	      (srecode-handle-region-when-non-active-flag t))

          ;; RESOLVE AND INSERT
          (let ((entry dict-entries))
	    (while entry
	      (srecode-dictionary-set-value
	       dict (nth 0 entry) (nth 1 entry))
	      (setq entry (nthcdr 1 entry))))

          (srecode-insert-fcn temp dict)

          ;; COMPARE THE OUTPUT
          (let ((actual (buffer-substring-no-properties
		         (point-min) (point-max))))
	    (if (string= output-1 actual)
	        nil

	      (goto-char (point-max))
	      (insert "\n\n ------------- ^^  actual  ^^ ------------\n\n
 ------------- vv expected vv ------------\n\n"
		      output-1)
              (setq fail
	            (list (format "Entry %s failed:" (oref o point))
                          (buffer-string))
                    )))))
      )
    fail))

;;; ARG HANDLER
;;
(defun srecode-semantic-handle-:utest (dict)
  "Add macros into the dictionary DICT for unit testing purposes."
  (srecode-dictionary-set-value dict "UTESTVAR1" "ARG HANDLER ONE")
  (srecode-dictionary-set-value dict "UTESTVAR2" "ARG HANDLER TWO")
  )

(defun srecode-semantic-handle-:utestwitharg (dict)
  "Add macros into the dictionary DICT based on other vars in DICT."
  (let ((val1 (srecode-dictionary-lookup-name dict "UTWA"))
	(nval1 nil))
    ;; If there is a value, mutate it
    (if (and val1 (stringp val1))
	(setq nval1 (upcase val1))
      ;; No value, make stuff up
      (setq nval1 "NO VALUE"))

    (srecode-dictionary-set-value dict "UTESTARGXFORM" nval1))

  (let ((dicts (srecode-dictionary-lookup-name dict "UTLOOP")))
    (dolist (D dicts)
      ;; For each dictionary, lookup NAME, and transform into
      ;; something in DICT instead.
      (let ((sval (srecode-dictionary-lookup-name D "NAME")))
	(srecode-dictionary-set-value dict (concat "FOO_" sval) sval)
	)))
  )

;;; TEST POINTS
;;
(defvar srecode-utest-output-entries
  (list
   (srecode-utest-output
    :point "test1" :name "test"
    :output (concat ";; " (user-full-name) "\n"
		    ";; " (upcase (user-full-name))) )
   (srecode-utest-output
    :point "subs" :name "subs"
    :output ";; Before Loop
;; After Loop" )
   (srecode-utest-output
    :point "firstlast" :name "firstlast"
    :output "
;; << -- FIRST
;; I'm First
;; I'm Not Last
;; -- >>

;; << -- MIDDLE
;; I'm Not First
;; I'm Not Last
;; -- >>

;; << -- LAST
;; I'm Not First
;; I'm Last
;; -- >>
" )
   (srecode-utest-output
    :point "gapsomething" :name "gapsomething"
    :output ";; First Line
### ALL ALONE ON A LINE ###
;;Second Line"
    :pre-fill ";; First Line
!;;Second Line")
   (srecode-utest-output
    :point "wrapsomething" :name "wrapsomething"
    :output ";; Put this line in front:
;; First Line
;; Put this line at the end:"
    :pre-fill "!;; First Line
!")
   (srecode-utest-output
    :point "inlinetext" :name "inlinetext"
    :output ";; A big long comment XX*In the middle*XX with cursor in middle"
    :pre-fill ";; A big long comment XX!XX with cursor in middle")

   (srecode-utest-output
    :point "wrapinclude-basic" :name "wrapinclude-basic"
    :output ";; An includable  we could use.
;; \n;; Text after a point inserter."
    )
   (srecode-utest-output
    :point "wrapinclude-basic2" :name "wrapinclude-basic"
    :output ";; An includable MOOSE we could use.
;; \n;; Text after a point inserter."
    :dict-entries '("COMMENT" "MOOSE")
    )
   (srecode-utest-output
    :point "wrapinclude-around" :name "wrapinclude-around"
    :output ";; An includable  we could use.
;; [VAR]Intermediate Comments
;; Text after a point inserter."
    )
   (srecode-utest-output
    :point "wrapinclude-around1" :name "wrapinclude-around"
    :output ";; An includable PENGUIN we could use.
;; [VAR]Intermediate Comments
;; Text after a point inserter."
    :dict-entries '("COMMENT" "PENGUIN")
    )
   (srecode-utest-output
    :point "complex-subdict" :name "complex-subdict"
    :output ";; I have a cow and a dog.")
   (srecode-utest-output
    :point "wrap-new-template" :name "wrap-new-template"
    :output "template newtemplate
\"A nice doc string goes here.\"
----
Random text in the new template
----
bind \"a\""
    :dict-entries '( "NAME" "newtemplate" "KEY" "a" )
    )
   (srecode-utest-output
    :point "column-data" :name "column-data"
    :output "Table of Values:
Left Justified       | Right Justified
FIRST                |                FIRST
VERY VERY LONG STRIN | VERY VERY LONG STRIN
MIDDLE               |               MIDDLE
S                    |                    S
LAST                 |                 LAST")
   (srecode-utest-output
    :point "custom-arg-handler" :name "custom-arg-handler"
    :output "OUTSIDE SECTION: ARG HANDLER ONE
INSIDE SECTION: ARG HANDLER ONE")
   (srecode-utest-output
    :point "custom-arg-w-arg none" :name "custom-arg-w-arg"
    :output "Value of xformed UTWA: NO VALUE")
   (srecode-utest-output
    :point "custom-arg-w-arg upcase" :name "custom-arg-w-arg"
    :dict-entries '( "UTWA" "uppercaseme" )
    :output "Value of xformed UTWA: UPPERCASEME")
   (srecode-utest-output
    :point "custom-arg-w-subdict" :name "custom-arg-w-subdict"
    :output "All items here: item1 item2 item3")

   ;; Test cases for new "section ... end" dictionary syntax
   (srecode-utest-output
    :point "nested-dictionary-syntax-flat"
    :name   "nested-dictionary-syntax-flat"
    :output "sub item1")
   (srecode-utest-output
    :point "nested-dictionary-syntax-nesting"
    :name   "nested-dictionary-syntax-nesting"
    :output "item11-item11-item21-item31  item21-item11-item21-item31  item31-item311-item321  ")
   (srecode-utest-output
    :point "nested-dictionary-syntax-mixed"
    :name   "nested-dictionary-syntax-mixed"
    :output "item1 item2"))
  "Test point entries for the template output tests.")

;;; Master Harness
;;
(defvar srecode-utest-testfile
  (expand-file-name (concat (make-temp-name "srecode-utest-") ".srt") temporary-file-directory)
  "File used to do testing.")

(ert-deftest srecode-utest-template-output ()
  "Test various template insertion options."
  (save-excursion
    (let ((testbuff (find-file-noselect srecode-utest-testfile)))

      (set-buffer testbuff)

      (srecode-load-tables-for-mode major-mode)
      (srecode-load-tables-for-mode major-mode 'tests)

      (should (srecode-table major-mode))

      ;; Loop over the output testpoints.

      (dolist (p srecode-utest-output-entries)
	(set-buffer testbuff) ;; XEmacs causes a buffer switch.  I don't know why
	(should-not (srecode-utest-test p))
	)

      ))
  (when (file-exists-p srecode-utest-testfile)
    (delete-file srecode-utest-testfile)))

;;; Project test
;;
;; Test that "project" specification works ok.

(ert-deftest srecode-utest-project ()
  "Test that project filtering works."
  :expected-result (if (getenv "EMACS_HYDRA_CI") :failed :passed) ; fixme
  (save-excursion
    (let ((testbuff (find-file-noselect srecode-utest-testfile))
	  (temp nil))

      (set-buffer testbuff)
      (erase-buffer)

      ;; Load the basics, and test that we can't find the application templates.
      (srecode-load-tables-for-mode major-mode)

      (should (srecode-table major-mode))

      (setq temp (srecode-template-get-table (srecode-table)
					     "test-project"
					     "test"
					     'tests
					     ))
      (when temp
        (should-not "App Template Loaded when not specified."))

      ;; Load the application templates, and make sure we can find them.
      (srecode-load-tables-for-mode major-mode 'tests)

      (setq temp (srecode-template-get-table (srecode-table)
					     "test-project"
					     "test"
					     'tests
					     ))

      (when (not temp)
        (should-not "Failed to load app specific template when available."))

      ;; Temporarily change the home of this file.  This will make the
      ;; project template go out of scope.
      (let ((default-directory (expand-file-name "~/")))

	(setq temp (srecode-template-get-table (srecode-table)
					       "test-project"
					       "test"
					       'tests
					       ))

        (when temp
	  (should-not "Project specific template available when in wrong directory."))

        )))
  (when (file-exists-p srecode-utest-testfile)
    (delete-file srecode-utest-testfile)))


(provide 'cedet/srecode-utest-template)
;;; srecode-utest-template.el ends here
