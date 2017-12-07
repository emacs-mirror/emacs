;;; faceup.el --- Markup language for faces and font-lock regression testing  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Version: 0.0.6
;; Created: 2013-01-21
;; Keywords: faces languages
;; URL: https://github.com/Lindydancer/faceup

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

;; Emacs is capable of highlighting buffers based on language-specific
;; `font-lock' rules.  This package makes it possible to perform
;; regression test for packages that provide font-lock rules.
;;
;; The underlying idea is to convert text with highlights ("faces")
;; into a plain text representation using the Faceup markup
;; language.  This language is semi-human readable, for example:
;;
;;     «k:this» is a keyword
;;
;; By comparing the current highlight with a highlight performed with
;; stable versions of a package, it's possible to automatically find
;; problems that otherwise would have been hard to spot.
;;
;; This package is designed to be used in conjunction with Ert, the
;; standard Emacs regression test system.
;;
;; The Faceup markup language is a generic markup language, regression
;; testing is merely one way to use it.

;; Regression test examples:
;;
;; This section describes the two typical ways regression testing with
;; this package is performed.
;;
;;
;; Full source file highlighting:
;;
;; The most straight-forward way to perform regression testing is to
;; collect a number of representative source files.  From each source
;; file, say `alpha.mylang', you can use `M-x faceup-write-file RET'
;; to generate a Faceup file named `alpha.mylang.faceup', this file
;; use the Faceup markup language to represent the text with
;; highlights and is used as a reference in future tests.
;;
;; An Ert test case can be defined as follows:
;;
;;    (require 'faceup)
;;
;;    (defvar mylang-font-lock-test-dir (faceup-this-file-directory))
;;
;;    (defun mylang-font-lock-test-apps (file)
;;      "Test that the mylang FILE is fontifies as the .faceup file describes."
;;      (faceup-test-font-lock-file 'mylang-mode
;;                                  (concat mylang-font-lock-test-dir file)))
;;    (faceup-defexplainer mylang-font-lock-test-apps)
;;
;;    (ert-deftest mylang-font-lock-file-test ()
;;      (should (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
;;      ;; ... Add more test files here ...
;;      )
;;
;; To execute the tests, run something like `M-x ert RET t RET'.
;;
;;
;; Source snippets:
;;
;; To test smaller snippets of code, you can use the
;; `faceup-test-font-lock-string'.  It takes a major mode and a string
;; written using the Faceup markup language.  The functions strips away
;; the Faceup markup, inserts the plain text into a temporary buffer,
;; highlights it, converts the result back into the Faceup markup
;; language, and finally compares the result with the original Faceup
;; string.
;;
;; For example:
;;
;;    (defun mylang-font-lock-test (faceup)
;;      (faceup-test-font-lock-string 'mylang-mode faceup))
;;    (faceup-defexplainer mylang-font-lock-test)
;;
;;    (ert-deftest mylang-font-lock-test-simple ()
;;      "Simple MyLang font-lock tests."
;;      (should (mylang-font-lock-test "«k:this» is a keyword"))
;;      (should (mylang-font-lock-test "«k:function» «f:myfunc» («v:var»)")))
;;

;; Executing the tests:
;;
;; Once the tests have been defined, you can use `M-x ert RET t RET'
;; to execute them.  Hopefully, you will be given the "all clear".
;; However, if there is a problem, you will be presented with
;; something like:
;;
;;     F mylang-font-lock-file-test
;;         (ert-test-failed
;;          ((should
;;            (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
;;           :form
;;           (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang")
;;           :value nil :explanation
;;           ((on-line 2
;;     		("but_«k:this»_is_not_a_keyword")
;;     		("but_this_is_not_a_keyword")))))
;;
;; You should read this that on line 2, the old font-lock rules
;; highlighted `this' inside `but_this_is_not_a_keyword' (which is
;; clearly wrong), whereas the new doesn't.  Of course, if this is the
;; desired result (for example, the result of a recent change) you can
;; simply regenerate the .faceup file and store it as the reference
;; file for the future.

;; The Faceup markup language:
;;
;; The Faceup markup language is designed to be human-readable and
;; minimalistic.
;;
;; The two special characters `«' and `»' marks the start and end of a
;; range of a face.
;;
;;
;; Compact format for special faces:
;;
;; The compact format `«<LETTER>:text»' is used for a number of common
;; faces.  For example, `«U:abc»' means that the text `abc' is
;; underlined.
;;
;; See `faceup-face-short-alist' for the known faces and the
;; corresponding letter.
;;
;;
;; Full format:
;;
;; The format `«:<NAME OF FACE>:text»' is used use to encode other
;; faces.
;;
;; For example `«:my-special-face:abc»' meanst that `abc' has the face
;; `my-special-face'.
;;
;;
;; Anonymous faces:
;;
;; An "anonymous face" is when the `face' property contains a property
;; list (plist) on the form `(:key value)'.  This is represented using
;; a variant of the full format: `«:(:key value):text»'.
;;
;; For example, `«:(:background "red"):abc»' represent the text `abc'
;; with a red background.
;;
;;
;; Multiple properties:
;;
;; In case a text contains more than one face property, they are
;; represented using nested sections.
;;
;; For example:
;;
;; * `«B:abc«U:def»»' represent the text `abcdef' that is both *bold*
;;   and *underlined*.
;;
;; * `«W:abc«U:def»ghi»' represent the text `abcdefghi' where the
;;   entire text is in *warning* face and `def' is *underlined*.
;;
;; In case two faces partially overlap, the ranges will be split when
;; represented in Faceup.  For example:
;;
;; * `«B:abc«U:def»»«U:ghi»' represent the text `abcdefghi' where
;;   `abcdef' is bold and `defghi' is underlined.
;;
;;
;; Escaping start and end markers:
;;
;; Any occurrence of the start or end markers in the original text
;; will be escaped using the start marker in the Faceup
;; representation.  In other words, the sequences `««' and `«»'
;; represent a start and end marker, respectively.
;;
;;
;; Other properties:
;;
;; In addition to representing the `face' property (or, more
;; correctly, the value of `faceup-default-property') other properties
;; can be encoded.  The variable `faceup-properties' contains a list of
;; properties to track.  If a property behaves like the `face'
;; property, it is encoded as described above, with the addition of
;; the property name placed in parentheses, for example:
;; `«(my-face)U:abd»'.
;;
;; The variable `faceup-face-like-properties' contains a list of
;; properties considered face-like.
;;
;; Properties that are not considered face-like are always encoded
;; using the full format and the don't nest.  For example:
;; `«(my-fibonacci-property):(1 1 2 3 5 8):abd»'.
;;
;; Examples of properties that could be tracked are:
;;
;; * `font-lock-face' -- an alias to `face' when `font-lock-mode' is
;;   enabled.
;;
;; * `syntax-table' -- used by a custom `syntax-propertize' to
;;   override the default syntax table.
;;
;; * `help-echo' -- provides tooltip text displayed when the mouse is
;;   held over a text.

;; Reference section:
;;
;; Faceup commands and functions:
;;
;; `M-x faceup-write-file RET' - generate a Faceup file based on the
;; current buffer.
;;
;; `M-x faceup-view-file RET' - view the current buffer converted to
;; Faceup.
;;
;; `faceup-markup-{string,buffer}' - convert text with properties to
;; the Faceup markup language.
;;
;; `faceup-render-view-buffer' - convert buffer with Faceup markup to
;; a buffer with real text properties and display it.
;;
;; `faceup-render-string' - return string with real text properties
;; from a string with Faceup markup.
;;
;; `faceup-render-to-{buffer,string}' - convert buffer with Faceup
;; markup to a buffer/string with real text properties.
;;
;; `faceup-clean-{buffer,string}' - remove Faceup markup from buffer
;; or string.
;;
;;
;; Regression test support:
;;
;; The following functions can be used as Ert test functions, or can
;; be used to implement new Ert test functions.
;;
;; `faceup-test-equal' - Test function, work like Ert:s `equal', but
;; more ergonomically when reporting multi-line string errors.
;; Concretely, it breaks down multi-line strings into lines and
;; reports which line number the error occurred on and the content of
;; that line.
;;
;; `faceup-test-font-lock-buffer' - Test that a buffer is highlighted
;; according to a reference Faceup text, for a specific major mode.
;;
;; `faceup-test-font-lock-string' - Test that a text with Faceup
;; markup is refontified to match the original Faceup markup.
;;
;; `faceup-test-font-lock-file' - Test that a file is highlighted
;; according to a reference .faceup file.
;;
;; `faceup-defexplainer' - Macro, define an explainer function and set
;; the `ert-explainer' property on the original function, for
;; functions based on the above test functions.
;;
;; `faceup-this-file-directory' - Macro, the directory of the current
;; file.

;; Real-world examples:
;;
;; The following are examples of real-world package that use faceup to
;; test their font-lock keywords.
;;
;; * [cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)
;;   an advanced set of font-lock keywords for the CMake language
;;
;; * [objc-font-lock](https://github.com/Lindydancer/objc-font-lock)
;;   highlight Objective-C function calls.
;;

;; Other Font Lock Tools:
;;
;; This package is part of a suite of font-lock tools.  The other
;; tools in the suite are:
;;
;;
;; Font Lock Studio:
;;
;; Interactive debugger for font-lock keywords (Emacs syntax
;; highlighting rules).
;;
;; Font Lock Studio lets you *single-step* Font Lock keywords --
;; matchers, highlights, and anchored rules, so that you can see what
;; happens when a buffer is fontified.  You can set *breakpoints* on
;; or inside rules and *run* until one has been hit.  When inside a
;; rule, matches are *visualized* using a palette of background
;; colors.  The *explainer* can describe a rule in plain-text English.
;; Tight integration with *Edebug* allows you to step into Lisp
;; expressions that are part of the Font Lock keywords.
;;
;;
;; Font Lock Profiler:
;;
;; A profiler for font-lock keywords.  This package measures time and
;; counts the number of times each part of a font-lock keyword is
;; used.  For matchers, it counts the total number and the number of
;; successful matches.
;;
;; The result is presented in table that can be sorted by count or
;; time.  The table can be expanded to include each part of the
;; font-lock keyword.
;;
;; In addition, this package can generate a log of all font-lock
;; events.  This can be used to verify font-lock implementations,
;; concretely, this is used for back-to-back tests of the real
;; font-lock engine and Font Lock Studio, an interactive debugger for
;; font-lock keywords.
;;
;;
;; Highlight Refontification:
;;
;; Minor mode that visualizes how font-lock refontifies a buffer.
;; This is useful when developing or debugging font-lock keywords,
;; especially for keywords that span multiple lines.
;;
;; The background of the buffer is painted in a rainbow of colors,
;; where each band in the rainbow represent a region of the buffer
;; that has been refontified.  When the buffer is modified, the
;; rainbow is updated.
;;
;;
;; Face Explorer:
;;
;; Library and tools for faces and text properties.
;;
;; This library is useful for packages that convert syntax highlighted
;; buffers to other formats.  The functions can be used to determine
;; how a face or a face text property looks, in terms of primitive
;; face attributes (e.g. foreground and background colors).  Two sets
;; of functions are provided, one for existing frames and one for
;; fictitious displays, like 8 color tty.
;;
;; In addition, the following tools are provided:
;;
;; - `face-explorer-list-faces' -- list all available faces.  Like
;;   `list-faces-display' but with information on how a face is
;;   defined.  In addition, a sample for the selected frame and for a
;;   fictitious display is shown.
;;
;; - `face-explorer-describe-face' -- Print detailed information on
;;   how a face is defined, and list all underlying definitions.
;;
;; - `face-explorer-describe-face-prop' -- Describe the `face' text
;;   property at the point in terms of primitive face attributes.
;;   Also show how it would look on a fictitious display.
;;
;; - `face-explorer-list-display-features' -- Show which features a
;;   display supports.  Most graphical displays support all, or most,
;;   features.  However, many tty:s don't support, for example,
;;   strike-through.  Using specially constructed faces, the resulting
;;   buffer will render differently in different displays, e.g. a
;;   graphical frame and a tty connected using `emacsclient -nw'.
;;
;; - `face-explorer-list-face-prop-examples' -- Show a buffer with an
;;   assortment of `face' text properties.  A sample text is shown in
;;   four variants: Native, a manually maintained reference vector,
;;   the result of `face-explorer-face-prop-attributes' and
;;   `face-explorer-face-prop-attributes-for-fictitious-display'.  Any
;;   package that convert a buffer to another format (like HTML, ANSI,
;;   or LaTeX) could use this buffer to ensure that everything work as
;;   intended.
;;
;; - `face-explorer-list-overlay-examples' -- Show a buffer with a
;;   number of examples of overlays, some are mixed with `face' text
;;   properties.  Any package that convert a buffer to another format
;;   (like HTML, ANSI, or LaTeX) could use this buffer to ensure that
;;   everything work as intended.
;;
;; - `face-explorer-tooltip-mode' -- Minor mode that shows tooltips
;;   containing text properties and overlays at the mouse pointer.
;;
;; - `face-explorer-simulate-display-mode' -- Minor mode for make a
;;   buffer look like it would on a fictitious display.  Using this
;;   you can, for example, see how a theme would look in using dark or
;;   light background, a 8 color tty, or on a grayscale graphical
;;   monitor.
;;
;;
;; Font Lock Regression Suite:
;;
;; A collection of example source files for a large number of
;; programming languages, with ERT tests to ensure that syntax
;; highlighting does not accidentally change.
;;
;; For each source file, font-lock reference files are provided for
;; various Emacs versions.  The reference files contains a plain-text
;; representation of source file with syntax highlighting, using the
;; format "faceup".
;;
;; Of course, the collection source file can be used for other kinds
;; of testing, not limited to font-lock regression testing.

;;; Code:


(defvar faceup-default-property 'face
  "The property that should be represented in Faceup without the (prop) part.")

(defvar faceup-properties '(face)
  "List of properties that should be converted to the Faceup format.

Only face-like property use the short format.  All other use the
non-nesting full format.  (See `faceup-face-like-properties'.)" )


(defvar faceup-face-like-properties '(face font-lock-face)
  "List of properties that behave like `face'.

The following properties are assumed about face-like properties:

* Elements are either symbols or property lists, or lists thereof.

* A plain element and a list containing the same element are
  treated as equal

* Property lists and sequences of property lists are considered
  equal.  For example:

     ((:underline t :foreground \"red\"))

  and

     ((:underline t) (:foreground \"red\"))

Face-like properties are converted to faceup in a nesting fashion.

For example, the string AAAXXXAAA (where the property `prop' has
the value `(a)' on the A:s and `(a b)' on the X:s) is converted
as follows, when treated as a face-like property:

    «(prop):a:AAA«(prop):b:XXX»AAAA»

When treated as a non-face-like property:

    «(prop):(a):AAA»«(prop):(a b):XXX»«(prop):(a):AAA»")


(defvar faceup-markup-start-char ?«)
(defvar faceup-markup-end-char   ?»)

(defvar faceup-face-short-alist
  '(;; Generic faces (uppercase letters)
    (bold                                . "B")
    (bold-italic                         . "Q")
    (default                             . "D")
    (error                               . "E")
    (highlight                           . "H")
    (italic                              . "I")
    (underline                           . "U")
    (warning                             . "W")
    ;; font-lock-specific faces (lowercase letters)
    (font-lock-builtin-face              . "b")
    (font-lock-comment-delimiter-face    . "m")
    (font-lock-comment-face              . "x")
    (font-lock-constant-face             . "c")
    (font-lock-doc-face                  . "d")
    (font-lock-function-name-face        . "f")
    (font-lock-keyword-face              . "k")
    (font-lock-negation-char-face        . "n")
    (font-lock-preprocessor-face         . "p")
    (font-lock-regexp-grouping-backslash . "h")
    (font-lock-regexp-grouping-construct . "o")
    (font-lock-string-face               . "s")
    (font-lock-type-face                 . "t")
    (font-lock-variable-name-face        . "v")
    (font-lock-warning-face              . "w"))
  "Alist from faces to one-character representation.")


;; Plain: «W....»
;; Nested: «W...«W...»»

;; Overlapping:   xxxxxxxxxx
;;                    yyyyyyyyyyyy
;;                «X..«Y..»»«Y...»


(defun faceup-markup-string (s)
  "Return the faceup version of the string S."
  (with-temp-buffer
    (insert s)
    (faceup-markup-buffer)))


;;;###autoload
(defun faceup-view-buffer ()
  "Display the faceup representation of the current buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*FaceUp*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (faceup-markup-to-buffer buffer)
    (display-buffer buffer)))


;;;###autoload
(defun faceup-write-file (&optional file-name confirm)
  "Save the faceup representation of the current buffer to the file FILE-NAME.

Unless a name is given, the file will be named xxx.faceup, where
xxx is the file name associated with the buffer.

If optional second arg CONFIRM is non-nil, this function
asks for confirmation before overwriting an existing file.
Interactively, confirmation is required unless you supply a prefix argument."
  (interactive
   (let ((suggested-name (and (buffer-file-name)
                              (concat (buffer-file-name)
                                      ".faceup"))))
     (list (read-file-name "Write faceup file: "
                           default-directory
                           suggested-name
                           nil
                           (file-name-nondirectory suggested-name))
           (not current-prefix-arg))))
  (unless file-name
    (setq file-name (concat (buffer-file-name) ".faceup")))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (faceup-markup-to-buffer (current-buffer) buffer)
      ;; Note: Must set `require-final-newline' inside
      ;; `with-temp-buffer', otherwise the value will be overridden by
      ;; the buffers local value.
      ;;
      ;; Clear `window-size-change-functions' as a workaround for
      ;; Emacs bug#19576 (`write-file' saves the wrong buffer if a
      ;; function in the list change current buffer).
      (let ((require-final-newline nil)
            (window-size-change-functions '()))
        (write-file file-name confirm)))))


(defun faceup-markup-buffer ()
  "Return a string with the content of the buffer using faceup markup."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (faceup-markup-to-buffer (current-buffer) buf)
      (buffer-substring-no-properties (point-min) (point-max)))))


;; Idea:
;;
;; Typically, only one face is used. However, when two faces are used,
;; the one of top is typically shorter. Hence, the faceup variant
;; should treat the inner group of nested ranges the upper (i.e. the
;; one towards the front.) For example:
;;
;;     «f:aaaaaaa«U:xxxx»aaaaaa»

(defun faceup-copy-and-quote (start end to-buffer)
  "Quote and insert the text between START and END into TO-BUFFER."
  (let ((not-markup (concat "^"
                            (make-string 1 faceup-markup-start-char)
                            (make-string 1 faceup-markup-end-char))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((old (point)))
          (skip-chars-forward not-markup end)
          (let ((s (buffer-substring-no-properties old (point))))
            (with-current-buffer to-buffer
              (insert s))))
        ;; Quote stray markup characters.
        (unless (= (point) end)
          (let ((next-char (following-char)))
            (with-current-buffer to-buffer
              (insert faceup-markup-start-char)
              (insert next-char)))
          (forward-char))))))


;; A face (string or symbol) can be on the top level.
;;
;; A face text property can be a arbitrary deep lisp structure.  Each
;; list in the tree structure contains faces (symbols or strings) up
;; to the first keyword, e.g. :foreground, thereafter the list is
;; considered a property list, regardless of the content.  A special
;; case are `(foreground-color . COLOR)' and `(background-color
;; . COLOR)', old forms used to represent the foreground and
;; background colors, respectively.
;;
;; Some of this is undocumented, and took some effort to reverse
;; engineer.
(defun faceup-normalize-face-property (value)
  "Normalize VALUES into a list of faces and (KEY VALUE) entries."
  (cond ((null value)
         '())
        ((symbolp value)
         (list value))
        ((stringp value)
         (list (intern value)))
        ((consp value)
         (cond ((eq (car value) 'foreground-color)
                (list (list :foreground (cdr value))))
               ((eq (car value) 'background-color)
                (list (list :background (cdr value))))
               (t
                ;; A list
                (if (keywordp (car value))
                    ;; Once a keyword has been seen, the rest of the
                    ;; list is treated as a property list, regardless
                    ;; of what it contains.
                    (let ((res '()))
                      (while value
                        (let ((key (pop value))
                              (val (pop value)))
                          (when (keywordp key)
                            (push (list key val) res))))
                      res)
                  (append
                   (faceup-normalize-face-property (car value))
                   (faceup-normalize-face-property (cdr value)))))))
        (t
         (error "Unexpected text property %s" value))))


(defun faceup-get-text-properties (pos)
  "Alist of properties and values at POS.

Face-like properties are normalized -- value is a list of
faces (symbols) and short (KEY VALUE) lists.  The list is
reversed to that later elements take precedence over earlier."
  (let ((res '()))
    (dolist (prop faceup-properties)
      (let ((value (get-text-property pos prop)))
        (when value
          (when (memq prop faceup-face-like-properties)
            ;; Normalize face-like properties.
            (setq value (reverse (faceup-normalize-face-property value))))
          (push (cons prop value) res))))
    res))


(defun faceup-markup-to-buffer (to-buffer &optional buffer)
  "Convert content of BUFFER to faceup form and insert in TO-BUFFER."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    ;; Font-lock often only fontifies the visible sections. This
    ;; ensures that the entire buffer is fontified before converting
    ;; it.
    (if (and font-lock-mode
             ;; Prevent clearing out face attributes explicitly
             ;; inserted by functions like `list-faces-display'.
             ;; (Font-lock mode is enabled, for some reason, in those
             ;; buffers.)
             (not (and (eq major-mode 'help-mode)
                       (not font-lock-defaults))))
        (font-lock-fontify-region (point-min) (point-max)))
    (let ((last-pos (point-min))
          (pos nil)
          ;; List of (prop . value), representing open faceup blocks.
          (state '()))
      (while (setq pos (faceup-next-property-change pos))
        ;; Insert content.
        (faceup-copy-and-quote last-pos pos to-buffer)
        (setq last-pos pos)
        (let ((prop-values (faceup-get-text-properties pos)))
          (let ((next-state '()))
            (setq state (reverse state))
            ;; Find all existing sequences that should continue.
            (let ((cont t))
              (while (and state
                          prop-values
                          cont)
                (let* ((prop (car (car state)))
                       (value (cdr (car state)))
                       (pair (assq prop prop-values)))
                  (if (memq prop faceup-face-like-properties)
                      ;; Element by element.
                      (if (equal value (car (cdr pair)))
                          (setcdr pair (cdr (cdr pair)))
                        (setq cont nil))
                    ;; Full value.
                    ;;
                    ;; Note: Comparison is done by `eq', since (at
                    ;; least) the `display' property treats
                    ;; eq-identical values differently than when
                    ;; comparing using `equal'. See "Display Specs
                    ;; That Replace The Text" in the elisp manual.
                    (if (eq value (cdr pair))
                        (setq prop-values (delq pair prop-values))
                      (setq cont nil))))
                (when cont
                  (push (pop state) next-state))))
            ;; End values that should not be included in the next state.
            (while state
              (with-current-buffer to-buffer
                (insert (make-string 1 faceup-markup-end-char)))
              (pop state))
            ;; Start new ranges.
            (with-current-buffer to-buffer
              (while prop-values
                (let ((pair (pop prop-values)))
                  (if (memq (car pair) faceup-face-like-properties)
                      ;; Face-like.
                      (dolist (element (cdr pair))
                        (insert (make-string 1 faceup-markup-start-char))
                        (unless (eq (car pair) faceup-default-property)
                          (insert "(")
                          (insert (symbol-name (car pair)))
                          (insert "):"))
                        (if (symbolp element)
                            (let ((short
                                   (assq element faceup-face-short-alist)))
                              (if short
                                  (insert (cdr short) ":")
                                (insert ":" (symbol-name element) ":")))
                          (insert ":")
                          (prin1 element (current-buffer))
                          (insert ":"))
                        (push (cons (car pair) element) next-state))
                    ;; Not face-like.
                    (insert (make-string 1 faceup-markup-start-char))
                    (insert "(")
                    (insert (symbol-name (car pair)))
                    (insert "):")
                    (prin1 (cdr pair) (current-buffer))
                    (insert ":")
                    (push pair next-state)))))
            ;; Insert content.
            (setq state next-state))))
      ;; Insert whatever is left after the last face change.
      (faceup-copy-and-quote last-pos (point-max) to-buffer))))



;; Some basic facts:
;;
;; (get-text-property (point-max) ...) always return nil. To check the
;; last character in the buffer, use (- (point-max) 1).
;;
;; If a text has more than one face, the first one in the list
;; takes precedence, when being viewed in Emacs.
;;
;;   (let ((s "ABCDEF"))
;;      (set-text-properties 1 4
;;        '(face (font-lock-warning-face font-lock-variable-name-face)) s)
;;      (insert s))
;;
;;   => ABCDEF
;;
;; Where DEF is drawn in "warning" face.


(defun faceup-has-any-text-property (pos)
  "True if any properties in `faceup-properties' are defined at POS."
  (let ((res nil))
    (dolist (prop faceup-properties)
      (when (get-text-property pos prop)
        (setq res t)))
    res))


(defun faceup-next-single-property-change (pos)
  "Next position a property in `faceup-properties' changes after POS, or nil."
  (let ((res nil))
    (dolist (prop faceup-properties)
      (let ((next (next-single-property-change pos prop)))
        (when next
          (setq res (if res
                        (min res next)
                      next)))))
    res))


(defun faceup-next-property-change (pos)
  "Next position after POS where one of the tracked properties change.

If POS is nil, also include `point-min' in the search.
If last character contains a tracked property, return `point-max'.

See `faceup-properties' for a list of tracked properties."
  (if (eq pos (point-max))
      ;; Last search returned `point-max'. There is no more to search
      ;; for.
      nil
    (if (and (null pos)
             (faceup-has-any-text-property (point-min)))
        ;; `pos' is `nil' and the character at `point-min' contains a
        ;; tracked property, return `point-min'.
        (point-min)
      (unless pos
        ;; Start from the beginning.
        (setq pos (point-min)))
      ;; Do a normal search. Compensate for that
      ;; `next-single-property-change' does not include the end of the
      ;; buffer, even when a property reach it.
      (let ((res (faceup-next-single-property-change pos)))
        (if (and (not res)              ; No more found.
                 (not (eq pos (point-max))) ; Not already at the end.
                 (not (eq (point-min) (point-max))) ; Not an empty buffer.
                 (faceup-has-any-text-property (- (point-max) 1)))
            ;; If a property goes all the way to the end of the
            ;; buffer, return `point-max'.
            (point-max)
          res)))))


;; ----------------------------------------------------------------------
;; Renderer
;;

;; Functions to convert from the faceup textual representation to text
;; with real properties.

(defun faceup-render-string (faceup)
  "Return string with properties from FACEUP written with Faceup markup."
  (with-temp-buffer
    (insert faceup)
    (faceup-render-to-string)))


;;;###autoload
(defun faceup-render-view-buffer (&optional buffer)
  "Convert BUFFER containing Faceup markup to a new buffer and display it."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((dest-buffer (get-buffer-create "*FaceUp rendering*")))
      (with-current-buffer dest-buffer
        (delete-region (point-min) (point-max)))
      (faceup-render-to-buffer dest-buffer)
      (display-buffer dest-buffer))))


(defun faceup-render-to-string (&optional buffer)
  "Convert BUFFER containing faceup markup to a string with faces."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-temp-buffer
    (faceup-render-to-buffer (current-buffer) buffer)
    (buffer-substring (point-min) (point-max))))


(defun faceup-render-to-buffer (to-buffer &optional buffer)
  "Convert BUFFER containing faceup markup into text with faces in TO-BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (let ((last-point (point))
          (state '())                   ; List of (prop . element)
          (not-markup (concat
                       "^"
                       (make-string 1 faceup-markup-start-char)
                       (make-string 1 faceup-markup-end-char))))
      (while (progn
               (skip-chars-forward not-markup)
               (if (not (eq last-point (point)))
                   (let ((text (buffer-substring-no-properties
                                last-point (point)))
                         (prop-elements-alist '()))
                     ;; Accumulate all values for each property.
                     (dolist (prop-element state)
                       (let ((property (car prop-element))
                             (element (cdr prop-element)))
                         (let ((pair (assq property prop-elements-alist)))
                           (unless pair
                             (setq pair (cons property '()))
                             (push pair prop-elements-alist))
                           (push element (cdr pair)))))
                     ;; Apply all properties.
                     (dolist (pair prop-elements-alist)
                       (let ((property (car pair))
                             (elements (reverse (cdr pair))))
                         ;; Create one of:
                         ;;    (property element) or
                         ;;    (property (element element ...))
                         (when (eq (length elements) 1)
                           ;; This ensures that non-face-like
                           ;; properties are restored to their
                           ;; original state.
                           (setq elements (car elements)))
                         (add-text-properties 0 (length text)
                                              (list property elements)
                                              text)))
                     (with-current-buffer to-buffer
                       (insert text))
                     (setq last-point (point))))
               (not (eobp)))
        (if (eq (following-char) faceup-markup-start-char)
            ;; Start marker.
            (progn
              (forward-char)
              (if (or (eq (following-char) faceup-markup-start-char)
                      (eq (following-char) faceup-markup-end-char))
                  ;; Escaped markup character.
                  (progn
                    (setq last-point (point))
                    (forward-char))
                ;; Markup sequence.
                (let ((property faceup-default-property))
                  (when (eq (following-char) ?\( )
                    (forward-char)      ; "("
                    (let ((p (point)))
                      (forward-sexp)
                      (setq property (intern (buffer-substring p (point)))))
                    (forward-char))     ; ")"
                  (let ((element
                         (if (eq (following-char) ?:)
                             ;; :element:
                             (progn
                               (forward-char)
                               (prog1
                                   (let ((p (point)))
                                     (forward-sexp)
                                     ;; Note: (read (current-buffer))
                                     ;; doesn't work, as it reads more
                                     ;; than a sexp.
                                     (read (buffer-substring p (point))))
                                 (forward-char)))
                           ;; X:
                           (prog1
                               (car (rassoc (buffer-substring-no-properties
                                             (point) (+ (point) 1))
                                            faceup-face-short-alist))
                             (forward-char 2)))))
                    (push (cons property element) state)))
                (setq last-point (point))))
          ;; End marker.
          (pop state)
          (forward-char)
          (setq last-point (point)))))))

;; ----------------------------------------------------------------------

;;;###autoload
(defun faceup-clean-buffer ()
  "Remove faceup markup from buffer."
  (interactive)
  (goto-char (point-min))
  (let ((not-markup (concat
                     "^"
                     (make-string 1 faceup-markup-start-char)
                     (make-string 1 faceup-markup-end-char))))
    (while (progn (skip-chars-forward not-markup)
                  (not (eobp)))
      (if (eq (following-char) faceup-markup-end-char)
          ;; End markers are always on their own.
          (delete-char 1)
        ;; Start marker.
        (delete-char 1)
        (if (or (eq (following-char) faceup-markup-start-char)
                (eq (following-char) faceup-markup-end-char))
            ;; Escaped markup character, delete the escape and skip
            ;; the original character.
            (forward-char)
          ;; Property name (if present)
          (if (eq (following-char) ?\( )
              (let ((p (point)))
                (forward-sexp)
                (delete-region p (point))))
          ;; Markup sequence.
          (if (eq (following-char) ?:)
              ;; :value:
              (let ((p (point)))
                (forward-char)
                (forward-sexp)
                (unless (eobp)
                  (forward-char))
                (delete-region p (point)))
            ;; X:
            (delete-char 1)             ; The one-letter form.
            (delete-char 1)))))))       ; The colon.


(defun faceup-clean-string (s)
  "Remove faceup markup from string S."
  (with-temp-buffer
    (insert s)
    (faceup-clean-buffer)
    (buffer-substring (point-min) (point-max))))


;; ----------------------------------------------------------------------
;; Regression test support
;;

(defvar faceup-test-explain nil
  "When non-nil, tester functions returns a text description on failure.

Of course, this only work for test functions aware of this
variable, like `faceup-test-equal' and functions based on this
function.

This is intended to be used to simplify `ert' explain functions,
which could be defined as:

    (defun my-test (args...) ...)
    (defun my-test-explain (args...)
      (let ((faceup-test-explain t))
        (the-test args...)))
    (put 'my-test 'ert-explainer 'my-test-explain)

Alternative, you can use the macro `faceup-defexplainer' as follows:

    (defun my-test (args...) ...)
    (faceup-defexplainer my-test)

Test functions, like `faceup-test-font-lock-buffer', built on top
of `faceup-test-equal', and other functions that adhere to this
variable, can easily define their own explainer functions.")

;;;###autoload
(defmacro faceup-defexplainer (function)
  "Define an Ert explainer function for FUNCTION.

FUNCTION must return an explanation when the test fails and
`faceup-test-explain' is set."
  (let ((name (intern (concat (symbol-name function) "-explainer"))))
    `(progn
       (defun ,name (&rest args)
         (let ((faceup-test-explain t))
           (apply (quote ,function) args)))
       (put (quote ,function) 'ert-explainer (quote ,name)))))


;; ------------------------------
;; Multi-line string support.
;;

(defun faceup-test-equal (lhs rhs)
  "Compares two (multi-line) strings, LHS and RHS, for equality.

This is intended to be used in Ert regression test rules.

When `faceup-test-explain' is non-nil, instead of returning nil
on inequality, a list is returned with a explanation what
differs.  Currently, this function reports 1) if the number of
lines in the strings differ.  2) the lines and the line numbers on
which the string differed.

For example:
    (let ((a \"ABC\\nDEF\\nGHI\")
          (b \"ABC\\nXXX\\nGHI\\nZZZ\")
          (faceup-test-explain t))
      (message \"%s\" (faceup-test-equal a b)))

    ==> (4 3 number-of-lines-differ (on-line 2 (DEF) (XXX)))

When used in an `ert' rule, the output is as below:

    (ert-deftest faceup-test-equal-example ()
      (let ((a \"ABC\\nDEF\\nGHI\")
            (b \"ABC\\nXXX\\nGHI\\nZZZ\"))
        (should (faceup-test-equal a b))))

    F faceup-test-equal-example
        (ert-test-failed
         ((should
           (faceup-test-equal a b))
          :form
          (faceup-test-equal \"ABC\\nDEF\\nGHI\" \"ABC\\nXXX\\nGHI\\nZZZ\")
          :value nil :explanation
          (4 3 number-of-lines-differ
             (on-line 2
                      (\"DEF\")
                      (\"XXX\")))))"
  (if (equal lhs rhs)
      t
    (if faceup-test-explain
        (let ((lhs-lines (split-string lhs "\n"))
              (rhs-lines (split-string rhs "\n"))
              (explanation '())
              (line 1))
          (unless (= (length lhs-lines) (length rhs-lines))
            (setq explanation (list 'number-of-lines-differ
                                    (length lhs-lines) (length rhs-lines))))
          (while lhs-lines
            (let ((one (pop lhs-lines))
                  (two (pop rhs-lines)))
              (unless (equal one two)
                (setq explanation
                      (cons (list 'on-line line (list one) (list two))
                            explanation)))
              (setq line (+ line 1))))
          (nreverse explanation))
      nil)))

(faceup-defexplainer faceup-test-equal)


;; ------------------------------
;; Font-lock regression test support.
;;

(defun faceup-test-font-lock-buffer (mode faceup &optional buffer)
  "Verify that BUFFER is fontified as FACEUP for major mode MODE.

If BUFFER is not specified the current buffer is used.

Note that the major mode of the buffer is set to MODE and that
the buffer is fontified.

If MODE is a list, the first element is the major mode, the
remaining are additional functions to call, e.g. minor modes."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (if (listp mode)
        (dolist (m mode)
          (funcall m))
      (funcall mode))
    (font-lock-fontify-region (point-min) (point-max))
    (let ((result (faceup-markup-buffer)))
      (faceup-test-equal faceup result))))

(faceup-defexplainer faceup-test-font-lock-buffer)


(defun faceup-test-font-lock-string (mode faceup)
  "True if FACEUP is re-fontified as the faceup markup for major mode MODE.

The string FACEUP is stripped from markup, inserted into a
buffer, the requested major mode activated, the buffer is
fontified, the result is again converted to the faceup form, and
compared with the original string."
  (with-temp-buffer
    (insert faceup)
    (faceup-clean-buffer)
    (faceup-test-font-lock-buffer mode faceup)))

(faceup-defexplainer faceup-test-font-lock-string)


(defun faceup-test-font-lock-file (mode file &optional faceup-file)
  "Verify that FILE is fontified as FACEUP-FILE for major mode MODE.

If FACEUP-FILE is omitted, FILE.faceup is used."
  (unless faceup-file
    (setq faceup-file (concat file ".faceup")))
  (let ((faceup (with-temp-buffer
                  (insert-file-contents faceup-file)
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert-file-contents file)
      (faceup-test-font-lock-buffer mode faceup))))

(faceup-defexplainer faceup-test-font-lock-file)


;; ------------------------------
;; Get current file directory. Test cases can use this to locate test
;; files.
;;

(defun faceup-this-file-directory ()
  "The directory of the file where the call to this function is located in.
Intended to be called when a file is loaded."
  (expand-file-name
   (if load-file-name
       ;; File is being loaded.
       (file-name-directory load-file-name)
     ;; File is being evaluated using, for example, `eval-buffer'.
     default-directory)))


;; ----------------------------------------------------------------------
;; The end
;;

(provide 'faceup)

;;; faceup.el ends here
