;;; srecode/test-getset.el --- Test the getset inserter.

;; Copyright (C) 2008, 2009, 2011, 2019-2020 Free Software Foundation, Inc

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
;; Unit tests for the getset inserter application.

(require 'srecode/semantic)

;;; Code:
(defvar srecode-utest-getset-pre-fill
  "// Test Class for getset tests in c++.

class myClass {
public:
  myClass() { };
  ~myClass() { };
  /** miscFunction
   */
  int miscFunction(int);

private:
  int fStartingField;

};

"
  "The pre-fill class for the getset tests.")


;;; Master Harness
;;
(defvar srecode-utest-getset-testfile
  (expand-file-name
   (concat (make-temp-name "srecode-utest-getset-") ".cpp")
   temporary-file-directory)
  "File used to do testing.")

(ert-deftest srecode-utest-getset-output ()
  "Test various template insertion options."
  (save-excursion
    (let ((testbuff (find-file-noselect srecode-utest-getset-testfile))
	  (srecode-insert-getset-fully-automatic-flag t))

      (set-buffer testbuff)
      (semantic-mode 1)
      (srecode-load-tables-for-mode major-mode)
      (srecode-load-tables-for-mode major-mode 'getset)

      (should (srecode-table))
      ;;(error "No template table found for mode %s" major-mode))

      (condition-case nil
	  (erase-buffer)
	(error nil))

      (insert srecode-utest-getset-pre-fill)
      (goto-char (point-min))

      ;; Test PRE FILL
      (should-not
       (srecode-utest-getset-tagcheck '("public"
				        "myClass"
				        "myClass"
				        "miscFunction"
				        "private"
				        "fStartingField")))
      (should-not
       (srecode-utest-getset-jumptotag "fStartingField"))

      ;; Startup with fully automatic selection.
      (srecode-insert-getset)

      ;; * Post get-set "StartingField"
      (should-not
       (srecode-utest-getset-tagcheck '("public"
				        "myClass"
				        "myClass"
				        "getStartingField"
				        "setStartingField"
				        "miscFunction"
				        "private"
				        "fStartingField")))

      ;; Now try convenience args.
      (goto-char (point-min))
      (should-not
       (srecode-utest-getset-jumptotag "fStartingField"))
      (end-of-line)
      (insert "\n")

      (srecode-insert-getset nil "AutoInsertField")

      ;; * Post get-set "AutoInsertField"
      (should-not
       (srecode-utest-getset-tagcheck '("public"
				        "myClass"
				        "myClass"
				        "getStartingField"
				        "setStartingField"
				        "getAutoInsertField"
				        "setAutoInsertField"
				        "miscFunction"
				        "private"
				        "fStartingField"
				        "fAutoInsertField")))

      ;; Make sure all the comments are in the right place.
      (should-not
       (srecode-utest-getset-jumptotag "miscFunction"))

      (let ((pos (point)))
	(skip-chars-backward " \t\n") ; xemacs forward-comment is different.
	(forward-comment -1)
	(re-search-forward "miscFunction" pos))

      ))
  (when (file-exists-p srecode-utest-getset-testfile)
    (delete-file srecode-utest-getset-testfile))
  )

(defun srecode-utest-getset-tagcheck (expected-members)
  "Make sure that the tags in myClass have EXPECTED-MEMBERS."
  (semantic-fetch-tags)
  (let* ((mc (semantic-find-tags-by-name "myClass" (current-buffer)))
	 (mem (semantic-tag-type-members (car mc)))
         (fail nil))
    (catch 'fail-early
      (while (and mem expected-members)
        (when (not (string= (semantic-tag-name (car mem))
			    (car expected-members)))
	  (switch-to-buffer (current-buffer))
	  (setq fail (format "Did not find %s in %s" (car expected-members)
                             (buffer-file-name)))
          (throw 'fail-early nil))
        (setq mem (cdr mem)
	      expected-members (cdr expected-members)))
      (when expected-members
        (switch-to-buffer (current-buffer))
        (setq fail (format "Did not find all expected tags in class: %s" (buffer-file-name)))
        (throw 'fail-early t))
      (when mem
        (switch-to-buffer (current-buffer))
        (setq fail (format "Found extra tags in class: %s" (buffer-file-name)))))

    (when fail (message "%s" (buffer-string)))
    fail))

(defun srecode-utest-getset-jumptotag (tagname)
  "Jump to the tag named TAGNAME."
  (semantic-fetch-tags)
  (let ((fail nil)
        (tag (semantic-deep-find-tags-by-name tagname (current-buffer))))
    (if tag
	(semantic-go-to-tag (car tag))
      (setq fail (format "Failed to jump to tag %s" tagname)))
    fail))

(provide 'cedet/srecode/test-getset)
;;; srecode/test-getset.el ends here
