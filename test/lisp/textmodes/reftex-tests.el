;;; reftex-tests.el --- Test suite for reftex. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords:       internal
;; Human-Keywords: internal

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

;;; Code:

(require 'ert)
(require 'ert-x)

;;; reftex
(require 'reftex)

;;; reftex-parse
(require 'reftex-parse)

(ert-deftest reftex-locate-bibliography-files ()
  "Test `reftex-locate-bibliography-files'."
  (ert-with-temp-directory temp-dir
    (let ((files '("ref1.bib" "ref2.bib"))
          (test '(("\\addbibresource{ref1.bib}\n" . ("ref1.bib"))
                  ("\\\\addbibresource[label=x]{ref2.bib}\\n" . ("ref2.bib"))
                  ("\\begin{document}\n\\bibliographystyle{plain}\n
\\bibliography{ref1,ref2}\n\\end{document}" . ("ref1.bib" "ref2.bib"))))
          (reftex-bibliography-commands
           ;; Default value: See reftex-vars.el `reftex-bibliography-commands'
           '("bibliography" "nobibliography" "setupbibtex\\[.*?database="
             "addbibresource")))
      (with-temp-buffer
        (insert "test\n")
        (mapc
         (lambda (file)
           (write-region (point-min) (point-max) (expand-file-name file
                                                                   temp-dir)))
         files))
      (mapc
       (lambda (data)
         (with-temp-buffer
           (insert (car data))
           (let ((res (mapcar #'file-name-nondirectory
                              (reftex-locate-bibliography-files temp-dir))))
             (should (equal res (cdr data))))))
       test))))

(ert-deftest reftex-what-environment-test ()
  "Test `reftex-what-environment'."
  (with-temp-buffer
    (insert "\\begin{equation}\n  x=y^2\n")
    (let ((pt (point))
          pt2)
      (insert "\\end{equation}\n")
      (goto-char pt)

      (should (equal (reftex-what-environment 1) '("equation" . 1)))
      (should (equal (reftex-what-environment t) '(("equation" . 1))))

      (insert "\\begin{something}\nxxx")
      (setq pt2 (point))
      (insert "\\end{something}")
      (goto-char pt2)
      (should (equal (reftex-what-environment 1) `("something" . ,pt)))
      (should (equal (reftex-what-environment t) `(("something" . ,pt)
                                                   ("equation" . 1))))
      (should (equal (reftex-what-environment t pt) `(("something" . ,pt))))
      (should (equal (reftex-what-environment '("equation"))
                     '("equation" . 1))))))

(ert-deftest reftex-roman-number-test ()
  "Test `reftex-roman-number'."
  (let ((hindu-arabic '(1     2    4   9    14   1050))
        (roman        '("I" "II" "IV" "IX" "XIV" "ML")))
    (while (and hindu-arabic roman)
      (should (string= (reftex-roman-number (car hindu-arabic))
                       (car roman)))
      (pop roman)
      (pop hindu-arabic))))

(ert-deftest reftex-parse-from-file-test ()
  "Test `reftex-parse-from-file'."
  ;; Use file-truename to convert 8+3 aliases in $TEMP value on
  ;; MS-Windows into their long file-name equivalents, which is
  ;; necessary for the 'equal' and 'string=' comparisons below.  This
  ;; also resolves any symlinks, which cannot be bad for the same
  ;; reason.  (An alternative solution would be to use file-equal-p,
  ;; but I'm too lazy to do that, as one of the tests compares a
  ;; list.)
  (ert-with-temp-directory temp-dir
    (let* ((tex-file (expand-file-name "test.tex" temp-dir))
           (bib-file (expand-file-name "ref.bib" temp-dir)))
      (with-temp-buffer
        (insert
         "\\begin{document}
\\section{test}\\label{sec:test}
\\subsection{subtest}

\\begin{align*}\\label{eq:foo}
  x &= y^2
\\end{align*}

\\bibliographystyle{plain}
\\bibliography{ref}
\\end{document}")
        (write-region (point-min) (point-max) tex-file))
      (with-temp-buffer
        (insert "test\n")
        (write-region (point-min) (point-max) bib-file))
      (reftex-ensure-compiled-variables)
      (let ((parsed (reftex-parse-from-file tex-file nil temp-dir)))
        (should (equal (car parsed) `(eof ,tex-file)))
        (pop parsed)
        (while parsed
          (let ((entry (pop parsed)))
            (cond
             ((eq (car entry) 'bib)
              (should (string= (cadr entry) bib-file)))
             ((eq (car entry) 'toc)) ;; ...
             ((string= (car entry) "eq:foo"))
             ((string= (car entry) "sec:test"))
             ((eq (car entry) 'bof)
              (should (string= (cadr entry) tex-file))
              (should (null parsed)))
             (t (should-not t)))))))))

;;; reftex-cite
(require 'reftex-cite)

(ert-deftest reftex-parse-bibtex-entry-test ()
  "Test `reftex-parse-bibtex-entry'."
  (let ((entry "@Book{Stallman12,
  author =    {Richard Stallman\net al.},
  title =        {The Emacs Editor},
  publisher =    {GNU Press},
  year =         2012,
  edition =   {17th},
  note   =      {Updated for Emacs   Version 24.2}
}")
        (check (lambda (parsed)
                 (should (string= (reftex-get-bib-field "&key" parsed)
                                  "Stallman12"))
                 (should (string= (reftex-get-bib-field "&type" parsed)
                                  "book"))
                 (should (string= (reftex-get-bib-field "author" parsed)
                                  "Richard Stallman et al."))
                 (should (string= (reftex-get-bib-field "title" parsed)
                                  "The Emacs Editor"))
                 (should (string= (reftex-get-bib-field "publisher" parsed)
                                  "GNU Press"))
                 (should (string= (reftex-get-bib-field "year" parsed)
                                  "2012"))
                 (should (string= (reftex-get-bib-field "edition" parsed)
                                  "17th"))
                 (should (string= (reftex-get-bib-field "note" parsed)
                                  "Updated for Emacs Version 24.2")))))
    (funcall check (reftex-parse-bibtex-entry entry))
    (with-temp-buffer
      (insert entry)
      (funcall check (reftex-parse-bibtex-entry nil (point-min)
                                                (point-max))))))

(ert-deftest reftex-get-bib-names-test ()
  "Test `reftex-get-bib-names'."
  (let ((entry (reftex-parse-bibtex-entry "@article{Foo123,
   author =   {Jane Roe and\tJohn Doe  and   W. Public},
}")))
    (should (equal (reftex-get-bib-names "author" entry)
                   '("Jane Roe" "John Doe" "Public"))))
  (let ((entry (reftex-parse-bibtex-entry "@article{Foo123,
   editor =   {Jane Roe and\tJohn Doe  and   W. Public},
}")))
    (should (equal (reftex-get-bib-names "author" entry)
                   '("Jane Roe" "John Doe" "Public")))))

(ert-deftest reftex-format-citation-test ()
  "Test `reftex-format-citation'."
  (let ((entry (reftex-parse-bibtex-entry "\
@article{Foo13,
  author =    {Jane Roe and John Doe and Jane Q. Taxpayer},
  title =        {Some Article},
  journal =    {Some Journal},
  year =         2013,
  pages = {1--333}
}"))
        (entry2 (reftex-parse-bibtex-entry "\
@article{Abels:slice,
author       = {Abels, H.},
title        = {Parallelizability of proper actions, global
                {$K$}-slices and maximal compact subgroups},
journaltitle = {Math. Ann.},
year         = 1974,
volume       = 212,
pages        = {1--19}
}")))
    (should (string= (reftex-format-citation entry nil) "\\cite{Foo13}"))
    (should (string= (reftex-format-citation entry "%l:%A:%y:%t %j %P %a")
                     "Foo13:Jane Roe:2013:Some Article Some Journal 1 Jane Roe, John Doe \\& Jane Taxpayer"))
    ;; Test for biblatex field journaltitle (bug#38762):
    (should (string=
             (reftex-format-citation entry2
                                     "[%4a, \\textit{%t}, \
%b %e, %u, %r %h %j \\textbf{%v} (%y), %p %<]")
             "[Abels, \\textit{Parallelizability of proper actions, \
global {$K$}-slices and maximal compact subgroups}, \
Math. Ann. \\textbf{212} (1974), 1--19]"))))

(ert-deftest reftex-all-used-citation-keys ()
  "Test `reftex-all-used-citation-keys'.
Take the cite macros provided by biblatex package as reference."
  (ert-with-temp-directory temp-dir
    (let ((tex-file (expand-file-name "keys.tex" temp-dir))
          keys)
      (with-temp-buffer
        (insert "\
\\documentclass{article}
\\usepackage{biblatex}
\\begin{document}

Standard commands:
\\cite[pre][pos]{cite:2022}
\\Cite[pos]{Cite:2022}
\\parencite{parencite:2022}
\\Parencite[pre][]{Parencite:2022}
\\footcite[][]{footcite:2022}
\\footcitetext[pre][pos]{footcitetext:2022}

Style specific commands:
\\textcite{textcite:2022}
\\Textcite[pos]{Textcite:2022}
\\smartcite[pre][pos]{smartcite:2022}
\\Smartcite[pre][]{Smartcite:2022}
\\cite*[pre][pos]{cite*:2022}
\\parencite*[][]{parencite*:2022}

Style independent commands:
\\autocite[pre][pos]{autocite:2022}
\\autocite*[pos]{autocite*:2022}
\\Autocite[pre][]{Autocite:2022}
\\Autocite*{Autocite*:2022}

Text commands:
\\citeauthor[pre][pos]{citeauthor:2022}
\\citeauthor*[pre][]{citeauthor*:2022}
\\Citeauthor[pos]{Citeauthor:2022}
\\Citeauthor*{Citeauthor*:2022}
\\citetitle[][]{citetitle:2022}
\\citetitle*[pre][pos]{citetitle*:2022}
\\citeyear[pre][pos]{citeyear:2022}
\\citeyear*[pre][pos]{citeyear*:2022}
\\citedate[pre][pos]{citedate:2022}
\\citedate*[pre][pos]{citedate*:2022}
\\citeurl[pre][pos]{citeurl:2022}

Special commands:
\\nocite{nocite:2022}
\\fullcite[pos]{fullcite:2022}
\\footfullcite[][]{fullfootcite:2022}
``volcite'' macros have different number of args.
\\volcite{2}{volcite:2022}
\\Volcite[pre]{1}{Volcite:2022}
\\pvolcite{1}[pg]{pvolcite:2022}
\\Pvolcite[pre]{2}[pg]{Pvolcite:2022}
\\fvolcite[pre]{3}[pg]{fvolcite:2022}
\\ftvolcite[pre]{3}[pg]{ftvolcite:2022}
\\svolcite[pre]{2}[pg]{svolcite:2022}
\\Svolcite[pre]{4}[pg]{Svolcite:2022}
\\tvolcite[pre]{5}[pg]{tvolcite:2022}
\\Tvolcite[pre]{2}[pg]{Tvolcite:2022}
\\avolcite[pre]{3}[pg]{avolcite:2022}
\\Avolcite[pre]{1}[pg]{Avolcite:2022}
\\Notecite[pre]{Notecite:2022}
\\pnotecite[pre]{pnotecite:2022}
\\Pnotecite[pre]{Pnotecite:2022}
\\fnotecite[pre]{fnotecite:2022}

Natbib compatibility commands:
\\citet{citet:2022}
\\citet*[pre][pos]{citet*:2022}
\\citep[pre][pos]{citep:2022}
\\citep*[pos]{citep*:2022}
\\citealt[pre][]{citealt:2022}
\\citealt*[][]{citealt*:2022}
\\citealp[pre][pos]{citealp:2022}
\\citealp*{citealp*:2022}
\\Citet[pre][pos]{Citet:2022}
\\Citet*[pre][pos]{Citet*:2022}
\\Citep[pre][pos]{Citep:2022}
\\Citep*[pre][pos]{Citep*:2022}

Qualified Citation Lists:
\\cites(Global Prenote)(Global Postnote)[pre][post]{cites:1}[pre][post]{cites:2}
\\Cites(Global Prenote)(Global Postnote)[pre][post]{Cites:1}[pre][post]{Cites:2}
\\parencites(Global Prenote)(Global Postnote)[pre][post]{parencites:1}
  [pre][post]{parencites:2}
\\Parencites(Global Prenote)(Global Postnote)[pre][post]{Parencites:1}{Parencites:2}
\\footcites[pre][post]{footcites:1}[pre][post]{footcites:2}
\\footcitetexts{footcitetexts:1}{footcitetexts:2}
\\smartcites{smartcites:1}
% This is comment about \\smartcites{smartcites:2}
[pre][post]{smartcites:2}
% And this should be ignored \\smartcites{smartcites:3}{smartcites:4}


Test for bug#56655:
There was a few \\% of increase in budget \\Citep*{bug:56655}.

And this should be % \\cite{ignored}.
\\end{document}")
        (write-region (point-min) (point-max) tex-file))
      (find-file tex-file)
      (setq keys (reftex-all-used-citation-keys))
      (should (equal (sort keys #'string<)
                     (sort (list
                             ;; Standard commands:
                             "cite:2022"      "Cite:2022"
                             "parencite:2022" "Parencite:2022"
                             "footcite:2022"  "footcitetext:2022"
                             ;; Style specific commands:
                             "textcite:2022"  "Textcite:2022"
                             "smartcite:2022" "Smartcite:2022"
                             "cite*:2022" "parencite*:2022"
                             ;; Style independent commands:
                             "autocite:2022"  "autocite*:2022"
                             "Autocite:2022"  "Autocite*:2022"
                             ;; Text commands
                             "citeauthor:2022" "citeauthor*:2022"
                             "Citeauthor:2022" "Citeauthor*:2022"
                             "citetitle:2022"  "citetitle*:2022"
                             "citeyear:2022"   "citeyear*:2022"
                             "citedate:2022"   "citedate*:2022"
                             "citeurl:2022"
                             ;; Special commands:
                             "nocite:2022"     "fullcite:2022"
                             "fullfootcite:2022"
                             "volcite:2022"   "Volcite:2022"
                             "pvolcite:2022"  "Pvolcite:2022"
                             "fvolcite:2022"  "ftvolcite:2022"
                             "svolcite:2022"  "Svolcite:2022"
                             "tvolcite:2022"  "Tvolcite:2022"
                             "avolcite:2022"  "Avolcite:2022"
                             "Notecite:2022"  "pnotecite:2022"
                             "Pnotecite:2022" "fnotecite:2022"
                             ;; Natbib compatibility commands:
                             "citet:2022"   "citet*:2022"
                             "citep:2022"   "citep*:2022"
                             "citealt:2022" "citealt*:2022"
                             "citealp:2022" "citealp*:2022"
                             "Citet:2022"   "Citet*:2022"
                             "Citep:2022"   "Citep*:2022"
                             ;; Qualified Citation Lists
                             "cites:1"         "cites:2"
                             "Cites:1"         "Cites:2"
                             "parencites:1"    "parencites:2"
                             "Parencites:1"    "Parencites:2"
                             "footcites:1"     "footcites:2"
                             "footcitetexts:1" "footcitetexts:2"
                             "smartcites:1"    "smartcites:2"
                             "bug:56655")
                           #'string<)))
      (kill-buffer (file-name-nondirectory tex-file)))))

(ert-deftest reftex-renumber-simple-labels ()
  "Test `reftex-renumber-simple-labels'.
The function must recognize labels defined with macros like
\\label and the ones as key=value option in optional or mandatory
argument of other macros or environments."
  (ert-with-temp-directory temp-dir
    (let ((tex-file (expand-file-name "renumber.tex" temp-dir)))
      (with-temp-buffer
        (insert "\
\\documentclass{article}
\\usepackage{tcolorbox}
\\tcbuselibrary{theorems}
\\usepackage{fancyvrb}
\\usepackage{listings}

\\begin{document}

This is with tcolorbox package:
\\begin{problem}[%
    colback                = white          ,
    colframe               = red!50!black   ,
    fonttitle              = \\bfseries      ,
    description delimiters = {\\flqq}{\\frqq} ,
    label                  = {problem:2}]{Prove RH2}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:2}
  2
\\end{equation}
By \\eqref{eq:2} and \\ref{problem:2}

This is with tcolorbox package:
\\begin{problem}[%
    colback=white,
    colframe=red!50!black,
    fonttitle=\\bfseries,
    theorem label supplement={hypertarget={XYZ-##1}},
    theorem full label supplement={code={\\marginnote{##1}}},
    label={problem:1}]{Prove RH1}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:1}
  1
\\end{equation}

\\Cref{problem:1} and \\pageref{eq:1}.

\\begin{problem}[label={problem:6}]{Some Problem}{}
  Problem
\\end{problem}

\\Ref{problem:6}.

This is with fancyvrb package:
\\begin{Verbatim}[reflabel={lst:6}]
Some Verb Content
\\end{Verbatim}

\\pageref{lst:6}

This is with listings package:
\\begin{lstlisting}[language=elisp,caption=Some Caption,label={lst:3}]
(car (cons 1 '(2)))
\\end{lstlisting}

\\ref{lst:3}

\\end{document}")
        (write-region (point-min) (point-max) tex-file))
      ;; The label prefix must be known to RefTeX:
      (add-to-list 'reftex-label-alist
                   '("problem" ?p "problem:" "~\\ref{%s}"
                     nil nil nil)
                   t)
      (add-to-list 'reftex-label-alist
                   '("Verbatim" ?l "lst:" "~\\ref{%s}"
                     nil nil nil)
                   t)
      ;; The environments must be known to RefTeX otherwise the labels
      ;; aren't parsed correctly:
      (add-to-list 'reftex-label-regexps
                   (concat "\\\\begin{\\(?:problem\\|Verbatim\\)}"
                           "\\[[^][]*"
                           "\\(?:{[^}{]*"
                           "\\(?:{[^}{]*"
                           "\\(?:{[^}{]*}[^}{]*\\)*"
                           "}[^}{]*\\)*"
                           "}[^][]*\\)*"
                           "\\<\\(?:ref\\)?label[[:space:]]*=[[:space:]]*"
                           "{?\\(?1:[^] ,}\r\n\t%]+\\)"
                           "[^]]*\\]")
                   t)
      ;; Always run this after changing `reftex-label-regexps':
      (reftex-compile-variables)
      (find-file tex-file)
      ;; Silence the user query:
      (cl-letf (((symbol-function 'yes-or-no-p) #'always))
        (reftex-renumber-simple-labels))
      (should (string= (buffer-string)
                       "\
\\documentclass{article}
\\usepackage{tcolorbox}
\\tcbuselibrary{theorems}
\\usepackage{fancyvrb}
\\usepackage{listings}

\\begin{document}

This is with tcolorbox package:
\\begin{problem}[%
    colback                = white          ,
    colframe               = red!50!black   ,
    fonttitle              = \\bfseries      ,
    description delimiters = {\\flqq}{\\frqq} ,
    label                  = {problem:1}]{Prove RH2}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:1}
  2
\\end{equation}
By \\eqref{eq:1} and \\ref{problem:1}

This is with tcolorbox package:
\\begin{problem}[%
    colback=white,
    colframe=red!50!black,
    fonttitle=\\bfseries,
    theorem label supplement={hypertarget={XYZ-##1}},
    theorem full label supplement={code={\\marginnote{##1}}},
    label={problem:2}]{Prove RH1}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:2}
  1
\\end{equation}

\\Cref{problem:2} and \\pageref{eq:2}.

\\begin{problem}[label={problem:3}]{Some Problem}{}
  Problem
\\end{problem}

\\Ref{problem:3}.

This is with fancyvrb package:
\\begin{Verbatim}[reflabel={lst:1}]
Some Verb Content
\\end{Verbatim}

\\pageref{lst:1}

This is with listings package:
\\begin{lstlisting}[language=elisp,caption=Some Caption,label={lst:2}]
(car (cons 1 '(2)))
\\end{lstlisting}

\\ref{lst:2}

\\end{document}"))
      (kill-buffer (file-name-nondirectory tex-file)))))

;;; non-file buffers

(ert-deftest reftex-all-used-citation-keys-buffer ()
  "Test `reftex-all-used-citation-keys' on a buffer without a file."
  (with-temp-buffer
    (insert "\
\\documentclass{article}
\\usepackage{biblatex}
\\begin{document}

Standard commands:
\\cite[pre][pos]{cite:2022}
\\Cite[pos]{Cite:2022}
\\parencite{parencite:2022}
\\Parencite[pre][]{Parencite:2022}
\\footcite[][]{footcite:2022}
\\footcitetext[pre][pos]{footcitetext:2022}

Style specific commands:
\\textcite{textcite:2022}
\\Textcite[pos]{Textcite:2022}
\\smartcite[pre][pos]{smartcite:2022}
\\Smartcite[pre][]{Smartcite:2022}
\\cite*[pre][pos]{cite*:2022}
\\parencite*[][]{parencite*:2022}

Style independent commands:
\\autocite[pre][pos]{autocite:2022}
\\autocite*[pos]{autocite*:2022}
\\Autocite[pre][]{Autocite:2022}
\\Autocite*{Autocite*:2022}

Text commands:
\\citeauthor[pre][pos]{citeauthor:2022}
\\citeauthor*[pre][]{citeauthor*:2022}
\\Citeauthor[pos]{Citeauthor:2022}
\\Citeauthor*{Citeauthor*:2022}
\\citetitle[][]{citetitle:2022}
\\citetitle*[pre][pos]{citetitle*:2022}
\\citeyear[pre][pos]{citeyear:2022}
\\citeyear*[pre][pos]{citeyear*:2022}
\\citedate[pre][pos]{citedate:2022}
\\citedate*[pre][pos]{citedate*:2022}
\\citeurl[pre][pos]{citeurl:2022}

Special commands:
\\nocite{nocite:2022}
\\fullcite[pos]{fullcite:2022}
\\footfullcite[][]{fullfootcite:2022}
``volcite'' macros have different number of args.
\\volcite{2}{volcite:2022}
\\Volcite[pre]{1}{Volcite:2022}
\\pvolcite{1}[pg]{pvolcite:2022}
\\Pvolcite[pre]{2}[pg]{Pvolcite:2022}
\\fvolcite[pre]{3}[pg]{fvolcite:2022}
\\ftvolcite[pre]{3}[pg]{ftvolcite:2022}
\\svolcite[pre]{2}[pg]{svolcite:2022}
\\Svolcite[pre]{4}[pg]{Svolcite:2022}
\\tvolcite[pre]{5}[pg]{tvolcite:2022}
\\Tvolcite[pre]{2}[pg]{Tvolcite:2022}
\\avolcite[pre]{3}[pg]{avolcite:2022}
\\Avolcite[pre]{1}[pg]{Avolcite:2022}
\\Notecite[pre]{Notecite:2022}
\\pnotecite[pre]{pnotecite:2022}
\\Pnotecite[pre]{Pnotecite:2022}
\\fnotecite[pre]{fnotecite:2022}

Natbib compatibility commands:
\\citet{citet:2022}
\\citet*[pre][pos]{citet*:2022}
\\citep[pre][pos]{citep:2022}
\\citep*[pos]{citep*:2022}
\\citealt[pre][]{citealt:2022}
\\citealt*[][]{citealt*:2022}
\\citealp[pre][pos]{citealp:2022}
\\citealp*{citealp*:2022}
\\Citet[pre][pos]{Citet:2022}
\\Citet*[pre][pos]{Citet*:2022}
\\Citep[pre][pos]{Citep:2022}
\\Citep*[pre][pos]{Citep*:2022}

Qualified Citation Lists:
\\cites(Global Prenote)(Global Postnote)[pre][post]{cites:1}[pre][post]{cites:2}
\\Cites(Global Prenote)(Global Postnote)[pre][post]{Cites:1}[pre][post]{Cites:2}
\\parencites(Global Prenote)(Global Postnote)[pre][post]{parencites:1}
  [pre][post]{parencites:2}
\\Parencites(Global Prenote)(Global Postnote)[pre][post]{Parencites:1}{Parencites:2}
\\footcites[pre][post]{footcites:1}[pre][post]{footcites:2}
\\footcitetexts{footcitetexts:1}{footcitetexts:2}
\\smartcites{smartcites:1}
% This is comment about \\smartcites{smartcites:2}
[pre][post]{smartcites:2}
% And this should be ignored \\smartcites{smartcites:3}{smartcites:4}


Test for bug#56655:
There was a few \\% of increase in budget \\Citep*{bug:56655}.

And this should be % \\cite{ignored}.
\\end{document}")
    (tex-mode)
    (let ((keys (reftex-all-used-citation-keys)))
      (should (equal (sort keys #'string<)
                     (sort (list
                           ;; Standard commands:
                           "cite:2022"      "Cite:2022"
                           "parencite:2022" "Parencite:2022"
                           "footcite:2022"  "footcitetext:2022"
                           ;; Style specific commands:
                           "textcite:2022"  "Textcite:2022"
                           "smartcite:2022" "Smartcite:2022"
                           "cite*:2022" "parencite*:2022"
                           ;; Style independent commands:
                           "autocite:2022"  "autocite*:2022"
                           "Autocite:2022"  "Autocite*:2022"
                           ;; Text commands
                           "citeauthor:2022" "citeauthor*:2022"
                           "Citeauthor:2022" "Citeauthor*:2022"
                           "citetitle:2022"  "citetitle*:2022"
                           "citeyear:2022"   "citeyear*:2022"
                           "citedate:2022"   "citedate*:2022"
                           "citeurl:2022"
                           ;; Special commands:
                           "nocite:2022"     "fullcite:2022"
                           "fullfootcite:2022"
                           "volcite:2022"   "Volcite:2022"
                           "pvolcite:2022"  "Pvolcite:2022"
                           "fvolcite:2022"  "ftvolcite:2022"
                           "svolcite:2022"  "Svolcite:2022"
                           "tvolcite:2022"  "Tvolcite:2022"
                           "avolcite:2022"  "Avolcite:2022"
                           "Notecite:2022"  "pnotecite:2022"
                           "Pnotecite:2022" "fnotecite:2022"
                           ;; Natbib compatibility commands:
                           "citet:2022"   "citet*:2022"
                           "citep:2022"   "citep*:2022"
                           "citealt:2022" "citealt*:2022"
                           "citealp:2022" "citealp*:2022"
                           "Citet:2022"   "Citet*:2022"
                           "Citep:2022"   "Citep*:2022"
                           ;; Qualified Citation Lists
                           "cites:1"         "cites:2"
                           "Cites:1"         "Cites:2"
                           "parencites:1"    "parencites:2"
                           "Parencites:1"    "Parencites:2"
                           "footcites:1"     "footcites:2"
                           "footcitetexts:1" "footcitetexts:2"
                           "smartcites:1"    "smartcites:2"
                           "bug:56655")
                         #'string<))))))

(ert-deftest reftex-renumber-simple-labels-buffer ()
  "Test `reftex-renumber-simple-labels' on a buffer without a file."
  (let ((temp-buffer (generate-new-buffer " *temp*")))
    (unwind-protect
        (with-current-buffer temp-buffer
          (insert "\
\\documentclass{article}
\\usepackage{tcolorbox}
\\tcbuselibrary{theorems}
\\usepackage{fancyvrb}
\\usepackage{listings}

\\begin{document}

This is with tcolorbox package:
\\begin{problem}[%
    colback                = white          ,
    colframe               = red!50!black   ,
    fonttitle              = \\bfseries      ,
    description delimiters = {\\flqq}{\\frqq} ,
    label                  = {problem:2}]{Prove RH2}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:2}
  2
\\end{equation}
By \\eqref{eq:2} and \\ref{problem:2}

This is with tcolorbox package:
\\begin{problem}[%
    colback=white,
    colframe=red!50!black,
    fonttitle=\\bfseries,
    theorem label supplement={hypertarget={XYZ-##1}},
    theorem full label supplement={code={\\marginnote{##1}}},
    label={problem:1}]{Prove RH1}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:1}
  1
\\end{equation}

\\Cref{problem:1} and \\pageref{eq:1}.

\\begin{problem}[label={problem:6}]{Some Problem}{}
  Problem
\\end{problem}

\\Ref{problem:6}.

This is with fancyvrb package:
\\begin{Verbatim}[reflabel={lst:6}]
Some Verb Content
\\end{Verbatim}

\\pageref{lst:6}

This is with listings package:
\\begin{lstlisting}[language=elisp,caption=Some Caption,label={lst:3}]
(car (cons 1 '(2)))
\\end{lstlisting}

\\ref{lst:3}

\\end{document}")

          ;; The label prefix must be known to RefTeX:
          (add-to-list 'reftex-label-alist
                       '("problem" ?p "problem:" "~\\ref{%s}"
                         nil nil nil)
                       t)
          (add-to-list 'reftex-label-alist
                       '("Verbatim" ?l "lst:" "~\\ref{%s}"
                         nil nil nil)
                       t)
          ;; The environments must be known to RefTeX otherwise the labels
          ;; aren't parsed correctly:
          (add-to-list 'reftex-label-regexps
                       (concat "\\\\begin{\\(?:problem\\|Verbatim\\)}"
                               "\\[[^][]*"
                               "\\(?:{[^}{]*"
                               "\\(?:{[^}{]*"
                               "\\(?:{[^}{]*}[^}{]*\\)*"
                               "}[^}{]*\\)*"
                               "}[^][]*\\)*"
                               "\\<\\(?:ref\\)?label[[:space:]]*=[[:space:]]*"
                               "{?\\(?1:[^] ,}\r\n\t%]+\\)"
                               "[^]]*\\]")
                       t)
          ;; Always run this after changing `reftex-label-regexps':
          (reftex-compile-variables)

          ;; Silence the user query:
          (cl-letf (((symbol-function 'yes-or-no-p) #'always))
            (reftex-renumber-simple-labels))

          (should (string= (buffer-string)
                           "\
\\documentclass{article}
\\usepackage{tcolorbox}
\\tcbuselibrary{theorems}
\\usepackage{fancyvrb}
\\usepackage{listings}

\\begin{document}

This is with tcolorbox package:
\\begin{problem}[%
    colback                = white          ,
    colframe               = red!50!black   ,
    fonttitle              = \\bfseries      ,
    description delimiters = {\\flqq}{\\frqq} ,
    label                  = {problem:1}]{Prove RH2}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:1}
  2
\\end{equation}
By \\eqref{eq:1} and \\ref{problem:1}

This is with tcolorbox package:
\\begin{problem}[%
    colback=white,
    colframe=red!50!black,
    fonttitle=\\bfseries,
    theorem label supplement={hypertarget={XYZ-##1}},
    theorem full label supplement={code={\\marginnote{##1}}},
    label={problem:2}]{Prove RH1}{}
  Problem
\\end{problem}

This is with vanilla \\LaTeX:
\\begin{equation}
  \\label{eq:2}
  1
\\end{equation}

\\Cref{problem:2} and \\pageref{eq:2}.

\\begin{problem}[label={problem:3}]{Some Problem}{}
  Problem
\\end{problem}

\\Ref{problem:3}.

This is with fancyvrb package:
\\begin{Verbatim}[reflabel={lst:1}]
Some Verb Content
\\end{Verbatim}

\\pageref{lst:1}

This is with listings package:
\\begin{lstlisting}[language=elisp,caption=Some Caption,label={lst:2}]
(car (cons 1 '(2)))
\\end{lstlisting}

\\ref{lst:2}

\\end{document}")))
      (kill-buffer temp-buffer))))


;;; Autoload tests

;; Test to check whether reftex autoloading mechanisms are working
;; correctly.
(ert-deftest reftex-autoload-auc ()
  "Tests to see whether reftex-auc has been autoloaded"
  (should
   (fboundp 'reftex-arg-label))
  (should
   (autoloadp
    (symbol-function
     'reftex-arg-label))))


(provide 'reftex-tests)
;;; reftex-tests.el ends here.
