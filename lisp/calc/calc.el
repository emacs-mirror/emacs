;;; calc.el --- the GNU Emacs calculator

;; Copyright (C) 1990, 1991, 1992, 1993, 2001, 2002 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Colin Walters <walters@debian.org>
;; Keywords: convenience, extensions
;; Version: 2.02g

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;; Calc is split into many files.  This file is the main entry point.
;; This file includes autoload commands for various other basic Calc
;; facilities.  The more advanced features are based in calc-ext, which
;; in turn contains autoloads for the rest of the Calc files.  This
;; odd set of interactions is designed to make Calc's loading time
;; be as short as possible when only simple calculations are needed.

;; Original author's address:
;;  Dave Gillespie, daveg@synaptics.com, uunet!synaptx!daveg.
;;  Synaptics, Inc., 2698 Orchard Parkway, San Jose, CA 95134.
;;
;; The old address daveg@csvax.cs.caltech.edu will continue to
;; work for the foreseeable future.
;;
;; Bug reports and suggestions are always welcome!  (Type M-x
;; report-calc-bug to send them).

;; All functions, macros, and Lisp variables defined here begin with one
;; of the prefixes "math", "Math", or "calc", with the exceptions of
;; "full-calc", "full-calc-keypad", "another-calc", "quick-calc",
;; "report-calc-bug", and "defmath".  User-accessible variables begin
;; with "var-".

;;; TODO:

;;   Fix rewrite mechanism to do less gratuitous rearrangement of terms.
;;   Implement a pattern-based "refers" predicate.
;;
;;   Make it possible to Undo a selection command.
;;   Figure out how to allow selecting rows of matrices.
;;   If cursor was in selection before, move it after j n, j p, j L, etc.
;;   Consider reimplementing calc-delete-selection using rewrites.
;;
;;   Implement line-breaking in non-flat compositions (is this desirable?).
;;   Implement matrix formatting with multi-line components.
;;
;;   Have "Z R" define a user command based on a set of rewrite rules.
;;   Support "incf" and "decf" in defmath definitions.
;;   Have defmath generate calls to calc-binary-op or calc-unary-op.
;;   Make some way to define algebraic functions using keyboard macros.
;;
;;   Allow calc-word-size=0 => Common Lisp-style signed bitwise arithmetic.
;;   Consider digamma function (and thus arb. prec. Euler's gamma constant).
;;   May as well make continued-fractions stuff available to the user.
;;
;;   How about matrix eigenvalues, SVD, pseudo-inverse, etc.?
;;   Should cache matrix inverses as well as decompositions.
;;   If dividing by a non-square matrix, use least-squares automatically.
;;   Consider supporting matrix exponentials.
;;
;;   Have ninteg detect and work around singularities at the endpoints.
;;   Use an adaptive subdivision algorithm for ninteg.
;;   Provide nsum and nprod to go along with ninteg.
;;
;;   Handle TeX-mode parsing of \matrix{ ... } where ... contains braces.
;;   Support AmS-TeX's \{d,t,}frac, \{d,t,}binom notations.
;;   Format and parse sums and products in Eqn and Math modes.
;;
;;   Get math-read-big-expr to read sums, products, etc.
;;   Change calc-grab-region to use math-read-big-expr.
;;   Have a way to define functions using := in Embedded Mode.
;;
;;   Support polar plotting with GNUPLOT.
;;   Make a calc-graph-histogram function.
;;
;;   Replace hokey formulas for complex functions with formulas designed
;;      to minimize roundoff while maintaining the proper branch cuts.
;;   Test accuracy of advanced math functions over whole complex plane.
;;   Extend Bessel functions to provide arbitrary precision.
;;   Extend advanced math functions to handle error forms and intervals.
;;   Provide a better implementation for math-sin-cos-raw.
;;   Provide a better implementation for math-hypot.
;;   Provide a better implementation for math-make-frac.
;;   Provide a better implementation for calcFunc-prfac.
;;   Provide a better implementation for calcFunc-factor.
;;
;;   Provide more examples in the tutorial section of the manual.
;;   Cover in the tutorial:  simplification modes, declarations,
;;       bitwise stuff, selections, matrix mapping, financial functions.
;;   Provide more Lisp programming examples in the manual.
;;   Finish the Internals section of the manual (and bring it up to date).
;;
;;   Tim suggests adding spreadsheet-like features.
;;   Implement language modes for Gnuplot, Lisp, Ada, APL, ...?
;;
;; For atan series, if x > tan(pi/12) (about 0.268) reduce using the identity
;;   atan(x) = atan((x * sqrt(3) - 1) / (sqrt(3) + x)) + pi/6.
;;
;; A better integration algorithm:
;;   Use breadth-first instead of depth-first search, as follows:
;;	The integral cache allows unfinished integrals in symbolic notation
;;	on the righthand side.  An entry with no unfinished integrals on the
;;	RHS is "complete"; references to it elsewhere are replaced by the
;;	integrated value.  More than one cache entry for the same integral
;;	may exist, though if one becomes complete, the others may be deleted.
;;	The integrator works by using every applicable rule (such as
;;	substitution, parts, linearity, etc.) to generate possible righthand
;;	sides, all of which are entered into the cache.  Now, as long as the
;;	target integral is not complete (and the time limit has not run out)
;;	choose an incomplete integral from the cache and, for every integral
;;	appearing in its RHS's, add those integrals to the cache using the
;;	same substitition, parts, etc. rules.  The cache should be organized
;;	as a priority queue, choosing the "simplest" incomplete integral at
;;	each step, or choosing randomly among equally simple integrals.
;;	Simplicity equals small size, and few steps removed from the original
;;	target integral.  Note that when the integrator finishes, incomplete
;;	integrals can be left in the cache, so the algorithm can start where
;;	it left off if another similar integral is later requested.
;;   Breadth-first search would avoid the nagging problem of, e.g., whether
;;   to use parts or substitution first, and which decomposition is best.
;;   All are tried, and any path that diverges will quickly be put on the
;;   back burner by the priority queue.
;;   Note: Probably a good idea to call math-simplify-extended before
;;   measuring a formula's simplicity.

;; From: "Robert J. Chassell" <bob@rattlesnake.com>
;; Subject: Re: fix for `Cannot open load file: calc-alg-3'
;; To: walters@debian.org
;; Date: Sat, 24 Nov 2001 21:44:21 +0000 (UTC)
;; 
;; Could you add logistic curve fitting to the current list?
;; 
;; (I guess the key binding for a logistic curve would have to be `s'
;; since a logistic curve is an `s' curve; both `l' and `L' are already
;; taken for logarithms.)
;; 
;; Here is the current list for curve fitting;
;; 
;;     `1'
;;          Linear or multilinear.  a + b x + c y + d z.
;; 
;;     `2-9'
;;          Polynomials.  a + b x + c x^2 + d x^3.
;; 
;;     `e'
;;          Exponential.  a exp(b x) exp(c y).
;; 
;;     `E'
;;          Base-10 exponential.  a 10^(b x) 10^(c y).
;; 
;;     `x'
;;          Exponential (alternate notation).  exp(a + b x + c y).
;; 
;;     `X'
;;          Base-10 exponential (alternate).  10^(a + b x + c y).
;; 
;;     `l'
;;          Logarithmic.  a + b ln(x) + c ln(y).
;; 
;;     `L'
;;          Base-10 logarithmic.  a + b log10(x) + c log10(y).
;; 
;;     `^'
;;          General exponential.  a b^x c^y.
;; 
;;     `p'
;;          Power law.  a x^b y^c.
;; 
;;     `q'
;;          Quadratic.  a + b (x-c)^2 + d (x-e)^2.
;; 
;;     `g'
;;          Gaussian.  (a / b sqrt(2 pi)) exp(-0.5*((x-c)/b)^2).
;; 
;; 
;; Logistic curves are used a great deal in ecology, and in predicting
;; human actions, such as use of different kinds of energy in a country
;; (wood, coal, oil, natural gas, etc.) or the number of scientific
;; papers a person publishes, or the number of movies made.
;; 
;; (The less information on which to base the curve, the higher the error
;; rate.  Theodore Modis ran some Monte Carlo simulations and produced
;; what may be useful set of confidence levels for different amounts of
;; initial information.)

;;; Code:

(provide 'calc)
(require 'calc-macs)

;;; The "###autoload" comment will be used by Emacs version 19 for
;;; maintaining the loaddefs.el file automatically.

;;;###autoload
(defvar calc-info-filename "calc.info"
  "*File name in which to look for the Calculator's Info documentation.")

;;;###autoload
(defvar calc-settings-file user-init-file
  "*File in which to record permanent settings; default is `user-init-file'.")

;;;###autoload
(defvar calc-autoload-directory nil
  "Name of directory from which additional \".elc\" files for Calc should be
loaded.  Should include a trailing \"/\".
If nil, use original installation directory.
This can safely be nil as long as the Calc files are on the load-path.")

;;;###autoload
(defvar calc-gnuplot-name "gnuplot"
  "*Name of GNUPLOT program, for calc-graph features.")

;;;###autoload
(defvar calc-gnuplot-plot-command nil
  "*Name of command for displaying GNUPLOT output; %s = file name to print.")

;;;###autoload
(defvar calc-gnuplot-print-command "lp %s"
  "*Name of command for printing GNUPLOT output; %s = file name to print.")

;; Address of the author of Calc, for use by report-calc-bug.
(defvar calc-bug-address "walters@debian.org")

;; If t, scan keymaps to find all DEL-like keys.
;; if nil, only DEL itself is mapped to calc-pop.
(defvar calc-scan-for-dels t)

(defvar calc-extensions-loaded nil)

;; Calculator stack.
;; Entries are 3-lists:  Formula, Height (in lines), Selection (or nil).
(defvar calc-stack '((top-of-stack 1 nil)))

(defvar calc-show-banner t
  "*If non-nil, show a friendly greeting above the stack.")

;; Index into calc-stack of "top" of stack.
;; This is 1 unless calc-truncate-stack has been used.
;;(defvar calc-stack-top 1)

;; If non-nil, load the calc-ext module automatically when calc is loaded.
;;(defvar calc-always-load-extensions nil)

;; If non-nil, display line numbers in Calculator stack.
;;(defvar calc-line-numbering t)

;; If non-nil, break long values across multiple lines in Calculator stack.
;;(defvar calc-line-breaking t)

;; If nil, stack display is left-justified.
;; If 'right, stack display is right-justified.
;; If 'center, stack display is centered."
;;(defvar calc-display-just nil)

;; Horizontal origin of displayed stack entries.
;; In left-justified mode, this is effectively indentation.  (Default 0).
;; In right-justified mode, this is effectively window width.
;; In centered mode, center of stack entry is placed here.
;;(defvar calc-display-origin nil)

;; Radix for entry and display of numbers in calc-mode, 2-36.
;;(defvar calc-number-radix 10)

;; If non-nil, leading zeros are provided to pad integers to calc-word-size.
;;(defvar calc-leading-zeros nil)

;; If non-nil, group digits in large displayed integers by inserting spaces.
;; If an integer, group that many digits at a time.
;; If 't', use 4 for binary and hex, 3 otherwise.
;;(defvar calc-group-digits nil)

;; The character (in the form of a string) to be used for grouping digits.
;; This is used only when calc-group-digits mode is on.
;;(defvar calc-group-char ",")

;; The character (in the form of a string) to be used as a decimal point.
;;(defvar calc-point-char ".")

;; Format of displayed fractions; a string of one or two of ":" or "/".
;;(defvar calc-frac-format '(":" nil))

;; If non-nil, prefer fractional over floating-point results.
;;(defvar calc-prefer-frac nil)

;; Format of displayed hours-minutes-seconds angles, a format string.
;; String must contain three %s marks for hours, minutes, seconds respectively.
;;(defvar calc-hms-format "%s@ %s' %s\"")

;; Format of displayed date forms.
;;(defvar calc-date-format '((H ":" mm ":" SS pp " ") Www " " Mmm " " D ", " YYYY))

;; Format to use for display of floating-point numbers in calc-mode.
;; Must be a list of one of the following forms:
;;  (float 0)      Floating point format, display full precision.
;;  (float N)      N > 0: Floating point format, at most N significant figures.
;;  (float -N)     -N < 0: Floating point format, calc-internal-prec - N figs.
;;  (fix N)        N >= 0: Fixed point format, N places after decimal point.
;;  (sci 0)        Scientific notation, full precision.
;;  (sci N)        N > 0: Scientific notation, N significant figures.
;;  (sci -N)       -N < 0: Scientific notation, calc-internal-prec - N figs.
;;  (eng 0)        Engineering notation, full precision.
;;  (eng N)        N > 0: Engineering notation, N significant figures.
;;  (eng -N)       -N < 0: Engineering notation, calc-internal-prec - N figs.
;;(defvar calc-float-format '(float 0))

;; Format to use when full precision must be displayed.
;;(defvar calc-full-float-format '(float 0))

;; Format to use for display of complex numbers in calc-mode.  Must be one of:
;;   nil            Use (x, y) form.
;;   i              Use x + yi form.
;;   j              Use x + yj form.
;;(defvar calc-complex-format nil)

;; Preferred form, either 'cplx or 'polar, for complex numbers.
;;(defvar calc-complex-mode 'cplx)

;; If nil, 1 / 0 is left unsimplified.
;; If 0, 1 / 0 is changed to inf (zeros are considered positive).
;; Otherwise, 1 / 0 is changed to uinf (undirected infinity).
;;(defvar calc-infinite-mode nil)

;; If non-nil, display vectors of byte-sized integers as strings.
;;(defvar calc-display-strings nil)

;; If nil, vector elements are left-justified.
;; If 'right, vector elements are right-justified.
;; If 'center, vector elements are centered."
;;(defvar calc-matrix-just 'center)

;; If non-nil, display vectors one element per line.
;;(defvar calc-break-vectors nil)

;; If non-nil, display long vectors in full.  If nil, use abbreviated form.
;;(defvar calc-full-vectors t)

;; If non-nil, display long vectors in full in the trail.
;;(defvar calc-full-trail-vectors t)

;; If non-nil, separate elements of displayed vectors with this string.
;;(defvar calc-vector-commas ",")

;; If non-nil, surround displayed vectors with these characters.
;;(defvar calc-vector-brackets "[]")

;; A list of code-letter symbols that control "big" matrix display.
;; If 'R is present, display inner brackets for matrices.
;; If 'O is present, display outer brackets for matrices (above/below).
;; If 'C is present, display outer brackets for matrices (centered).
;;(defvar calc-matrix-brackets '(R O))

;; Language or format for entry and display of stack values.  Must be one of:
;;   nil            Use standard Calc notation.
;;   flat           Use standard Calc notation, one-line format.
;;   big 	    Display formulas in 2-d notation (enter w/std notation).
;;   unform	    Use unformatted display: add(a, mul(b,c)).
;;   c              Use C language notation.
;;   pascal         Use Pascal language notation.
;;   fortran        Use Fortran language notation.
;;   tex            Use TeX notation.
;;   eqn	    Use eqn notation.
;;   math           Use Mathematica(tm) notation.
;;   maple	    Use Maple notation.
;;(defvar calc-language nil)

;; Numeric prefix argument for the command that set calc-language.
;;(defvar calc-language-option nil)

;; Open-parenthesis string for function call notation.
;;(defvar calc-function-open "(")

;; Close-parenthesis string for function call notation.
;;(defvar calc-function-close ")")

;; Function through which to pass strings after formatting.
;;(defvar calc-language-output-filter nil)

;; Function through which to pass strings before parsing.
;;(defvar calc-language-input-filter nil)

;; Formatting function used for non-decimal numbers.
;;(defvar calc-radix-formatter nil)

;; Label to display at left of formula.
;;(defvar calc-left-label "")

;; Label to display at right of formula.
;;(defvar calc-right-label "")

;; Minimum number of bits per word, if any, for binary operations in calc-mode.
;;(defvar calc-word-size 32)

;; Most recently used value of M in a modulo form.
;;(defvar calc-previous-modulo nil)

;; Type of simplification applied to results.
;; If 'none, results are not simplified when pushed on the stack.
;; If 'num, functions are simplified only when args are constant.
;; If nil, only fast simplifications are applied.
;; If 'binary, math-clip is applied if appropriate.
;; If 'alg, math-simplify is applied.
;; If 'ext, math-simplify-extended is applied.
;; If 'units, math-simplify-units is applied.
;;(defvar calc-simplify-mode nil)

;; If non-nil, recompute evalto's automatically when necessary.
;;(defvar calc-auto-recompute t)

;; If non-nil, display shows unformatted Lisp exprs.  (For debugging)
;;(defvar calc-display-raw nil)

;; Number of digits of internal precision for calc-mode calculations.
;;(defvar calc-internal-prec 12)

;; If non-nil, next operation is Inverse.
;;(defvar calc-inverse-flag nil)

;; If non-nil, next operation is Hyperbolic.
;;(defvar calc-hyperbolic-flag nil)

;; If non-nil, next operation should not remove its arguments from stack.
;;(defvar calc-keep-args-flag nil)

;; If deg, angles are in degrees; if rad, angles are in radians.
;; If hms, angles are in degrees-minutes-seconds.
;;(defvar calc-angle-mode 'deg)

;; If non-nil, numeric entry accepts whole algebraic expressions.
;; If nil, algebraic expressions must be preceded by "'".
;;(defvar calc-algebraic-mode nil)

;; Like calc-algebraic-mode except only affects ( and [ keys.
;;(defvar calc-incomplete-algebraic-mode nil)

;; If non-nil, inexact numeric computations like sqrt(2) are postponed.
;; If nil, computations on numbers always yield numbers where possible.
;;(defvar calc-symbolic-mode nil)

;; If 'matrix, variables are assumed to be matrix-valued.
;; If a number, variables are assumed to be NxN matrices.
;; If 'scalar, variables are assumed to be scalar-valued.
;; If nil, symbolic math routines make no assumptions about variables.
;;(defvar calc-matrix-mode nil)

;; If non-nil, shifted letter keys are prefix keys rather than normal meanings.
;;(defvar calc-shift-prefix nil)

;; Initial height of Calculator window.
;;(defvar calc-window-height 7)

;; If non-nil, M-x calc creates a window to display Calculator trail.
;;(defvar calc-display-trail t)

;; If non-nil, selected sub-formulas are shown by obscuring rest of formula.
;; If nil, selected sub-formulas are highlighted by obscuring the sub-formulas.
;;(defvar calc-show-selections t)

;; If non-nil, commands operate only on selected portions of formulas.
;; If nil, selections displayed but ignored.
;;(defvar calc-use-selections t)

;; If non-nil, selection hides deep structure of associative formulas.
;;(defvar calc-assoc-selections t)

;; If non-nil, display "Working..." for potentially slow Calculator commands.
;;(defvar calc-display-working-message 'lots)

;; If non-nil, automatically execute a "why" command to explain odd results.
;;(defvar calc-auto-why nil)

;; If non-nil, display timing information on each slow command.
;;(defvar calc-timing nil)

;; Floating-point numbers with this positive exponent or higher above the
;; current precision are displayed in scientific notation in calc-mode.
(defvar calc-display-sci-high 0)

;; Floating-point numbers with this negative exponent or lower are displayed
;; scientific notation in calc-mode.
(defvar calc-display-sci-low -3)


;; List of used-defined strings to append to Calculator mode line.
(defvar calc-other-modes nil)

;; List of strings for Y prefix help.
(defvar calc-Y-help-msgs nil)

;; t if calc-settings-file has been loaded yet.
(defvar calc-loaded-settings-file nil)



(defconst calc-mode-var-list '((calc-always-load-extensions nil)
			       (calc-mode-save-mode local)
			       (calc-line-numbering t)
			       (calc-line-breaking t)
			       (calc-display-just nil)
			       (calc-display-origin nil)
			       (calc-left-label "")
			       (calc-right-label "")
			       (calc-number-radix 10)
			       (calc-leading-zeros nil)
			       (calc-group-digits nil)
			       (calc-group-char ",")
			       (calc-point-char ".")
			       (calc-frac-format (":" nil))
			       (calc-prefer-frac nil)
			       (calc-hms-format "%s@ %s' %s\"")
			       (calc-date-format ((H ":" mm C SS pp " ")
						  Www " " Mmm " " D ", " YYYY))
			       (calc-standard-date-formats
				("N"
				 "<H:mm:SSpp >Www Mmm D, YYYY"
				 "D Mmm YYYY<, h:mm:SS>"
				 "Www Mmm BD< hh:mm:ss> YYYY"
				 "M/D/Y< H:mm:SSpp>"
				 "D.M.Y< h:mm:SS>"
				 "M-D-Y< H:mm:SSpp>"
				 "D-M-Y< h:mm:SS>"
				 "j<, h:mm:SS>"
				 "YYddd< hh:mm:ss>"))
			       (calc-float-format (float 0))
			       (calc-full-float-format (float 0))
			       (calc-complex-format nil)
			       (calc-matrix-just center)
			       (calc-full-vectors t)
			       (calc-full-trail-vectors t)
			       (calc-break-vectors nil)
			       (calc-vector-commas ",")
			       (calc-vector-brackets "[]")
			       (calc-matrix-brackets (R O))
			       (calc-complex-mode cplx)
			       (calc-infinite-mode nil)
			       (calc-display-strings nil)
			       (calc-simplify-mode nil)
			       (calc-auto-recompute t)
			       (calc-word-size 32)
			       (calc-previous-modulo nil)
			       (calc-display-raw nil)
			       (calc-internal-prec 12)
			       (calc-angle-mode deg)
			       (calc-algebraic-mode nil)
			       (calc-incomplete-algebraic-mode nil)
			       (calc-symbolic-mode nil)
			       (calc-matrix-mode nil)
			       (calc-autorange-units nil)
			       (calc-shift-prefix nil)
			       (calc-window-height 7)
			       (calc-was-keypad-mode nil)
			       (calc-full-mode nil)
			       (calc-language nil)
			       (calc-language-option nil)
			       (calc-user-parse-tables nil)
			       (calc-show-selections t)
			       (calc-use-selections t)
			       (calc-assoc-selections t)
			       (calc-display-trail t)
			       (calc-display-working-message lots)
			       (calc-auto-why 'maybe)
			       (calc-timing nil)
			       (calc-gnuplot-default-device "default")
			       (calc-gnuplot-default-output "STDOUT")
			       (calc-gnuplot-print-device "postscript")
			       (calc-gnuplot-print-output "auto")
			       (calc-gnuplot-geometry nil)
			       (calc-graph-default-resolution 15)
			       (calc-graph-default-resolution-3d 5)
			       (calc-invocation-macro nil)
			       (calc-show-banner t)))

(defconst calc-local-var-list '(calc-stack
				calc-stack-top
				calc-undo-list
				calc-redo-list
				calc-always-load-extensions
				calc-mode-save-mode
				calc-display-raw
				calc-line-numbering
				calc-line-breaking
				calc-display-just
				calc-display-origin
				calc-left-label
				calc-right-label
				calc-auto-why
				calc-algebraic-mode
				calc-incomplete-algebraic-mode
				calc-symbolic-mode
				calc-matrix-mode
				calc-inverse-flag
				calc-hyperbolic-flag
				calc-keep-args-flag
				calc-angle-mode
				calc-number-radix
				calc-leading-zeros
				calc-group-digits
				calc-group-char
				calc-point-char
				calc-frac-format
				calc-prefer-frac
				calc-hms-format
				calc-date-format
				calc-standard-date-formats
				calc-float-format
				calc-full-float-format
				calc-complex-format
				calc-matrix-just
				calc-full-vectors
				calc-full-trail-vectors
				calc-break-vectors
				calc-vector-commas
				calc-vector-brackets
				calc-matrix-brackets
				calc-complex-mode
				calc-infinite-mode
				calc-display-strings
				calc-simplify-mode
				calc-auto-recompute
				calc-autorange-units
				calc-show-plain
				calc-show-selections
				calc-use-selections
				calc-assoc-selections
				calc-word-size
				calc-internal-prec))


(defun calc-init-base ()

  ;; Verify that Calc is running on the right kind of system.
  (setq calc-emacs-type-epoch (and (fboundp 'epoch::version) epoch::version)
	calc-emacs-type-19 (not (or calc-emacs-type-epoch
				    (string-lessp emacs-version "19")))
	calc-emacs-type-lucid (not (not (string-match "Lucid" emacs-version)))
	calc-emacs-type-gnu19 (and calc-emacs-type-19
				   (not calc-emacs-type-lucid)))

  ;; Set up the standard keystroke (M-#) to run the Calculator, if that key
  ;; has not yet been bound to anything.  For best results, the user should
  ;; do this before Calc is even loaded, so that M-# can auto-load Calc.
  (or (global-key-binding "\e#")
      (global-set-key "\e#" 'calc-dispatch))

  ;; Set up the autoloading linkage.
  (let ((name (and (fboundp 'calc-dispatch)
		   (eq (car-safe (symbol-function 'calc-dispatch)) 'autoload)
		   (nth 1 (symbol-function 'calc-dispatch))))
	(p load-path))

    ;; If Calc files exist on the load-path, we're all set.
    (while (and p (not (file-exists-p
			(expand-file-name "calc-misc.elc" (car p)))))
      (setq p (cdr p)))
    (or p

	;; If Calc is autoloaded using a path name, look there for Calc files.
	;; This works for both relative ("calc/calc.elc") and absolute paths.
	(and name (file-name-directory name)
	     (let ((p2 load-path)
		   (name2 (concat (file-name-directory name)
				  "calc-misc.elc")))
	       (while (and p2 (not (file-exists-p
				    (expand-file-name name2 (car p2)))))
		 (setq p2 (cdr p2)))
	       (if p2
		   (setq load-path (nconc load-path
					  (list
					   (directory-file-name
					    (file-name-directory
					     (expand-file-name
					      name (car p2))))))))))

	;; If calc-autoload-directory is given, use that (and hope it works!).
	(and calc-autoload-directory
	     (not (equal calc-autoload-directory ""))
	     (setq load-path (nconc load-path
				    (list (directory-file-name
					   calc-autoload-directory)))))))

  ;; The following modes use specially-formatted data.
  (put 'calc-mode 'mode-class 'special)
  (put 'calc-trail-mode 'mode-class 'special)
  
  ;; Define "inexact-result" as an e-lisp error symbol.
  (put 'inexact-result 'error-conditions '(error inexact-result calc-error))
  (put 'inexact-result 'error-message "Calc internal error (inexact-result)")
  
  ;; Define "math-overflow" and "math-underflow" as e-lisp error symbols.
  (put 'math-overflow 'error-conditions '(error math-overflow calc-error))
  (put 'math-overflow 'error-message "Floating-point overflow occurred")
  (put 'math-underflow 'error-conditions '(error math-underflow calc-error))
  (put 'math-underflow 'error-message "Floating-point underflow occurred")
  
  (setq calc-version "2.02g"
	calc-version-date "Mon Nov 19 2001"
	calc-trail-pointer nil		; "Current" entry in trail buffer.
        calc-trail-overlay nil		; Value of overlay-arrow-string.
        calc-undo-list nil		; List of previous operations for undo.
        calc-redo-list nil		; List of recent undo operations.
        calc-main-buffer nil		; Pointer to Calculator buffer.
	calc-trail-buffer nil		; Pointer to Calc Trail buffer.
        calc-why nil			; Explanations of most recent errors.
        calc-next-why nil
	calc-inverse-flag nil
	calc-hyperbolic-flag nil
	calc-keep-args-flag nil
	calc-function-open "("
	calc-function-close ")"
	calc-language-output-filter nil
	calc-language-input-filter nil
	calc-radix-formatter nil
        calc-last-kill nil		; Last number killed in calc-mode.
        calc-previous-alg-entry nil	; Previous algebraic entry.
        calc-dollar-values nil		; Values to be used for '$'.
        calc-dollar-used nil		; Highest order of '$' that occurred.
	calc-hashes-used nil            ; Highest order of '#' that occurred.
        calc-quick-prev-results nil	; Previous results from Quick Calc.
	calc-said-hello nil		; Has welcome message been said yet?
	calc-executing-macro nil	; Kbd macro executing from "K" key.
	calc-any-selections nil 	; Nil means no selections present.
	calc-help-phase 0		; Count of consecutive "?" keystrokes.
	calc-full-help-flag nil		; Executing calc-full-help?
	calc-refresh-count 0		; Count of calc-refresh calls.
	calc-display-dirty nil
	calc-prepared-composition nil
	calc-selection-cache-default-entry nil
	calc-embedded-info nil
	calc-embedded-active nil
	calc-standalone-flag nil
	var-EvalRules nil
	math-eval-rules-cache-tag t
	math-radix-explicit-format t
	math-expr-function-mapping nil
	math-expr-variable-mapping nil
	math-read-expr-quotes nil
	math-working-step nil
	math-working-step-2 nil
        var-i '(special-const (math-imaginary 1))
        var-pi '(special-const (math-pi))
        var-e '(special-const (math-e))
	var-phi '(special-const (math-phi))
        var-gamma '(special-const (math-gamma-const))
	var-Modes '(special-const (math-get-modes-vec)))

  (mapcar (function (lambda (v) (or (boundp (car v)) (set (car v) (nth 1 v)))))
	  calc-mode-var-list)
  (mapcar (function (lambda (v) (or (boundp v) (set v nil))))
	  calc-local-var-list)

  (unless (boundp 'calc-mode-map)
    (setq calc-mode-map (make-keymap))
    (suppress-keymap calc-mode-map t)
    (define-key calc-mode-map "+" 'calc-plus)
    (define-key calc-mode-map "-" 'calc-minus)
    (define-key calc-mode-map "*" 'calc-times)
    (define-key calc-mode-map "/" 'calc-divide)
    (define-key calc-mode-map "%" 'calc-mod)
    (define-key calc-mode-map "&" 'calc-inv)
    (define-key calc-mode-map "^" 'calc-power)
    (define-key calc-mode-map "\M-%" 'calc-percent)
    (define-key calc-mode-map "e" 'calcDigit-start)
    (define-key calc-mode-map "i" 'calc-info)
    (define-key calc-mode-map "n" 'calc-change-sign)
    (define-key calc-mode-map "q" 'calc-quit)
    (define-key calc-mode-map "Y" 'nil)
    (define-key calc-mode-map "Y?" 'calc-shift-Y-prefix-help)
    (define-key calc-mode-map "?" 'calc-help)
    (define-key calc-mode-map " " 'calc-enter)
    (define-key calc-mode-map "'" 'calc-algebraic-entry)
    (define-key calc-mode-map "$" 'calc-auto-algebraic-entry)
    (define-key calc-mode-map "\"" 'calc-auto-algebraic-entry)
    (define-key calc-mode-map "\t" 'calc-roll-down)
    (define-key calc-mode-map "\M-\t" 'calc-roll-up)
    (define-key calc-mode-map "\C-m" 'calc-enter)
    (define-key calc-mode-map "\M-\C-m" 'calc-last-args-stub)
    (define-key calc-mode-map "\C-j" 'calc-over)

    (mapcar (function
	     (lambda (x)
	       (define-key calc-mode-map (char-to-string x) 'undefined)))
	    "lOW")
    (mapcar (function
	     (lambda (x)
	       (define-key calc-mode-map (char-to-string x)
		 'calc-missing-key)))
	    (concat "ABCDEFGHIJKLMNPQRSTUVXZabcdfghjkmoprstuvwxyz"
		    ":\\|!()[]<>{},;=~`\C-k\M-k\C-w\M-w\C-y\C-_"))
    (mapcar (function
	     (lambda (x)
	       (define-key calc-mode-map (char-to-string x) 'calcDigit-start)))
	    "_0123456789.#@")

    (setq calc-digit-map (make-keymap))
    (if calc-emacs-type-lucid
	(map-keymap (function
		     (lambda (keys bind)
		       (define-key calc-digit-map keys
			 (if (eq bind 'undefined)
			     'undefined 'calcDigit-nondigit))))
		    calc-mode-map)
      (let ((cmap (if calc-emacs-type-19 (nth 1 calc-mode-map) calc-mode-map))
	    (dmap (if calc-emacs-type-19 (nth 1 calc-digit-map)
		    calc-digit-map))
	    (i 0))
	(while (< i 128)
	  (aset dmap i
		(if (eq (aref cmap i) 'undefined)
		    'undefined 'calcDigit-nondigit))
	  (setq i (1+ i)))))
    (mapcar (function
	     (lambda (x)
	       (define-key calc-digit-map (char-to-string x)
		 'calcDigit-key)))
	    "_0123456789.e+-:n#@oh'\"mspM")
    (mapcar (function
	     (lambda (x)
	       (define-key calc-digit-map (char-to-string x)
		 'calcDigit-letter)))
	    "abcdfgijklqrtuvwxyzABCDEFGHIJKLNOPQRSTUVWXYZ")
    (define-key calc-digit-map "'" 'calcDigit-algebraic)
    (define-key calc-digit-map "`" 'calcDigit-edit)
    (define-key calc-digit-map "\C-g" 'abort-recursive-edit)

    (mapcar (function
	     (lambda (x)
	       (condition-case err
		   (progn
		     (define-key calc-digit-map x 'calcDigit-backspace)
		     (define-key calc-mode-map x 'calc-pop)
		     (define-key calc-mode-map
		       (if (vectorp x)
			   (if calc-emacs-type-lucid
			       (if (= (length x) 1)
				   (vector (if (consp (aref x 0))
					       (cons 'meta (aref x 0))
					     (list 'meta (aref x 0))))
				 "\e\C-d")
			     (vconcat "\e" x))
			 (concat "\e" x))
		       'calc-pop-above))
		 (error nil))))
	    (if calc-scan-for-dels
		(append (where-is-internal 'delete-backward-char global-map)
			(where-is-internal 'backward-delete-char global-map)
			'("\C-d"))
	      '("\177" "\C-d")))

    (setq calc-dispatch-map (make-keymap))
    (mapcar (function
	     (lambda (x)
	       (define-key calc-dispatch-map (char-to-string (car x)) (cdr x))
	       (if (string-match "abcdefhijklnopqrstuwxyz"
				 (char-to-string (car x)))
		   (define-key calc-dispatch-map
		     (char-to-string (- (car x) ?a -1)) (cdr x)))
	       (define-key calc-dispatch-map (format "\e%c" (car x)) (cdr x))))
	    '( ( ?a . calc-embedded-activate )
	       ( ?b . calc-big-or-small )
	       ( ?c . calc )
	       ( ?d . calc-embedded-duplicate )
	       ( ?e . calc-embedded )
	       ( ?f . calc-embedded-new-formula )
	       ( ?g . calc-grab-region )
	       ( ?h . calc-dispatch-help )
	       ( ?i . calc-info )
	       ( ?j . calc-embedded-select )
	       ( ?k . calc-keypad )
	       ( ?l . calc-load-everything )
	       ( ?m . read-kbd-macro )
	       ( ?n . calc-embedded-next )
	       ( ?o . calc-other-window )
	       ( ?p . calc-embedded-previous )
	       ( ?q . quick-calc )
	       ( ?r . calc-grab-rectangle )
	       ( ?s . calc-info-summary )
	       ( ?t . calc-tutorial )
	       ( ?u . calc-embedded-update-formula )
	       ( ?w . calc-embedded-word )
	       ( ?x . calc-quit )
	       ( ?y . calc-copy-to-buffer )
	       ( ?z . calc-user-invocation )
	       ( ?= . calc-embedded-update-formula )
	       ( ?\' . calc-embedded-new-formula )
	       ( ?\` . calc-embedded-edit )
	       ( ?: . calc-grab-sum-down )
	       ( ?_ . calc-grab-sum-across )
	       ( ?0 . calc-reset )
	       ( ?# . calc-same-interface )
	       ( ?? . calc-dispatch-help ) ))
    )

  (autoload 'calc-extensions "calc-ext")
  (autoload 'calc-need-macros "calc-macs")

;;;; (Autoloads here)
  (mapcar (function (lambda (x)
    (mapcar (function (lambda (func)
      (autoload func (car x)))) (cdr x))))
    '(

 ("calc-aent" calc-Need-calc-aent calc-alg-digit-entry calc-alg-entry
calc-check-user-syntax calc-do-alg-entry calc-do-calc-eval
calc-do-quick-calc calc-match-user-syntax math-build-parse-table
math-find-user-tokens math-read-expr-list math-read-exprs math-read-if
math-read-token math-remove-dashes)

 ("calc-misc" calc-Need-calc-misc 
calc-do-handle-whys calc-do-refresh calc-num-prefix-name
calc-record-list calc-record-why calc-report-bug calc-roll-down-stack
calc-roll-up-stack calc-temp-minibuffer-message calcFunc-floor
calcFunc-inv calcFunc-trunc math-concat math-constp math-div2
math-div2-bignum math-do-working math-evenp math-fixnatnump
math-fixnump math-floor math-imod math-ipow math-looks-negp math-mod
math-negp math-posp math-pow math-read-radix-digit math-reject-arg
math-trunc math-zerop)

))

  (mapcar (function (lambda (x)
    (mapcar (function (lambda (cmd)
      (autoload cmd (car x) nil t))) (cdr x))))
    '(

 ("calc-aent" calc-algebraic-entry calc-auto-algebraic-entry
calcDigit-algebraic calcDigit-edit)

 ("calc-misc" another-calc calc-big-or-small calc-dispatch-help
calc-help calc-info calc-info-summary calc-inv calc-last-args-stub
calc-missing-key calc-mod calc-other-window calc-over calc-percent
calc-pop-above calc-power calc-roll-down calc-roll-up
calc-shift-Y-prefix-help calc-tutorial calcDigit-letter
report-calc-bug))))

(calc-init-base)


;;;###autoload (global-set-key "\e#" 'calc-dispatch)

;;;###autoload
(defun calc-dispatch (&optional arg)
  "Invoke the GNU Emacs Calculator.  See `calc-dispatch-help' for details."
  (interactive "P")
  (sit-for echo-keystrokes)
  (condition-case err   ; look for other keys bound to calc-dispatch
      (let ((keys (this-command-keys)))
	(or (not (stringp keys))
	    (string-match "\\`\C-u\\|\\`\e[-0-9#]\\|`[\M--\M-0-\M-9]" keys)
	    (eq (lookup-key calc-dispatch-map keys) 'calc-same-interface)
	    (progn
	      (and (string-match "\\`[\C-@-\C-_]" keys)
		   (symbolp
		    (lookup-key calc-dispatch-map (substring keys 0 1)))
		   (define-key calc-dispatch-map (substring keys 0 1) nil))
	      (define-key calc-dispatch-map keys 'calc-same-interface))))
    (error nil))
  (calc-do-dispatch arg))

(defvar calc-dispatch-help nil)
(defun calc-do-dispatch (arg)
  (let ((key (calc-read-key-sequence
	      (if calc-dispatch-help
		  "Calc options: Calc, Keypad, Quick, Embed; eXit; Info, Tutorial; Grab; ?=more"
		(format "%s  (Type ? for a list of Calc options)"
			(key-description (this-command-keys))))
	      calc-dispatch-map)))
    (setq key (lookup-key calc-dispatch-map key))
    (message "")
    (if key
	(progn
	  (or (commandp key) (calc-extensions))
	  (call-interactively key))
      (beep))))

(defun calc-read-key-sequence (prompt map)
  (let ((prompt2 (format "%s " (key-description (this-command-keys))))
	(glob (current-global-map))
	(loc (current-local-map)))
    (or (input-pending-p) (message prompt))
    (let ((key (calc-read-key t)))
      (calc-unread-command (cdr key))
      (unwind-protect
	  (progn
	    (use-global-map map)
	    (use-local-map nil)
	    (read-key-sequence
	     (if (commandp (key-binding (if calc-emacs-type-19
					    (vector (cdr key))
					  (char-to-string (cdr key)))))
		 "" prompt2)))
	(use-global-map glob)
	(use-local-map loc)))))



(defun calc-mode ()
  "Calculator major mode.

This is an RPN calculator featuring arbitrary-precision integer, rational,
floating-point, complex, matrix, and symbolic arithmetic.

RPN calculation:  2 RET 3 +    produces 5.
Algebraic style:  ' 2+3 RET    produces 5.

Basic operators are +, -, *, /, ^, & (reciprocal), % (modulo), n (change-sign).

Press ? repeatedly for more complete help.  Press `h i' to read the
Calc manual on-line, `h s' to read the summary, or `h t' for the tutorial.

Notations:  3.14e6     3.14 * 10^6
            _23        negative number -23 (or type `23 n')
            17:3       the fraction 17/3
            5:2:3      the fraction 5 and 2/3
            16#12C     the integer 12C base 16 = 300 base 10
            8#177:100  the fraction 177:100 base 8 = 127:64 base 10
            (2, 4)     complex number 2 + 4i
            (2; 4)     polar complex number (r; theta)
            [1, 2, 3]  vector  ([[1, 2], [3, 4]] is a matrix)
            [1 .. 4)   semi-open interval, 1 <= x < 4
            2 +/- 3    (p key) number with mean 2, standard deviation 3
            2 mod 3    (M key) number 2 computed modulo 3
	    <1 jan 91> Date form (enter using ' key)


\\{calc-mode-map}
"
  (interactive)
  (mapcar (function
	   (lambda (v) (set-default v (symbol-value v)))) calc-local-var-list)
  (kill-all-local-variables)
  (use-local-map (if (eq calc-algebraic-mode 'total)
		     (progn (calc-extensions) calc-alg-map) calc-mode-map))
  (mapcar (function (lambda (v) (make-local-variable v))) calc-local-var-list)
  (make-local-variable 'overlay-arrow-position)
  (make-local-variable 'overlay-arrow-string)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'calc-mode)
  (setq mode-name "Calculator")
  (setq calc-stack-top (length (or (memq (assq 'top-of-stack calc-stack)
					 calc-stack)
				   (setq calc-stack (list (list 'top-of-stack
								1 nil))))))
  (setq calc-stack-top (- (length calc-stack) calc-stack-top -1))
  (or calc-loaded-settings-file
      (null calc-settings-file)
      (string-match "\\.emacs" calc-settings-file)
      (progn
	(setq calc-loaded-settings-file t)
	(load calc-settings-file t)))   ; t = missing-ok
  (if (and (eq window-system 'x) (boundp 'mouse-map))
      (substitute-key-definition 'x-paste-text 'calc-x-paste-text
				 mouse-map))
  (let ((p command-line-args))
    (while p
      (and (equal (car p) "-f")
	   (string-match "calc" (nth 1 p))
	   (string-match "full" (nth 1 p))
	   (setq calc-standalone-flag t))
      (setq p (cdr p))))
  (run-hooks 'calc-mode-hook)
  (calc-refresh t)
  (calc-set-mode-line)
  ;; The calc-defs variable is a relic.  Use calc-define properties instead.
  (if (and (boundp 'calc-defs)
	   calc-defs)
      (progn
	(message "Evaluating calc-defs...")
	(calc-need-macros)
	(eval (cons 'progn calc-defs))
	(setq calc-defs nil)
	(calc-set-mode-line)))
  (calc-check-defines))

(defvar calc-check-defines 'calc-check-defines)  ; suitable for run-hooks
(defun calc-check-defines ()
  (if (symbol-plist 'calc-define)
      (let ((plist (copy-sequence (symbol-plist 'calc-define))))
	(while (and plist (null (nth 1 plist)))
	  (setq plist (cdr (cdr plist))))
	(if plist
	    (save-excursion
	      (calc-extensions)
	      (calc-need-macros)
	      (set-buffer "*Calculator*")
	      (while plist
		(put 'calc-define (car plist) nil)
		(eval (nth 1 plist))
		(setq plist (cdr (cdr plist))))
	      ;; See if this has added any more calc-define properties.
	      (calc-check-defines))
	  (setplist 'calc-define nil)))))

(defun calc-trail-mode (&optional buf)
  "Calc Trail mode.
This mode is used by the *Calc Trail* buffer, which records all results
obtained by the GNU Emacs Calculator.

Calculator commands beginning with the `t' key are used to manipulate
the Trail.

This buffer uses the same key map as the *Calculator* buffer; calculator
commands given here will actually operate on the *Calculator* stack."
  (interactive)
  (fundamental-mode)
  (use-local-map calc-mode-map)
  (setq major-mode 'calc-trail-mode)
  (setq mode-name "Calc Trail")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (make-local-variable 'overlay-arrow-position)
  (make-local-variable 'overlay-arrow-string)
  (if buf
      (progn
	(make-local-variable 'calc-main-buffer)
	(setq calc-main-buffer buf)))
  (if (= (buffer-size) 0)
      (let ((buffer-read-only nil))
	(insert "Emacs Calculator v" calc-version " by Dave Gillespie\n")))
  (run-hooks 'calc-trail-mode-hook))

(defun calc-create-buffer ()
  (set-buffer (get-buffer-create "*Calculator*"))
  (or (eq major-mode 'calc-mode)
      (calc-mode))
  (setq max-lisp-eval-depth (max max-lisp-eval-depth 1000))
  (if calc-always-load-extensions
      (calc-extensions))
  (if calc-language
      (progn
	(calc-extensions)
	(calc-set-language calc-language calc-language-option t))))

;;;###autoload
(defun calc (&optional arg full-display interactive)
  "The Emacs Calculator.  Full documentation is listed under \"calc-mode\"."
  (interactive "P")
  (if arg
      (or (eq arg 0)
	  (progn
	    (calc-extensions)
	    (if (= (prefix-numeric-value arg) -1)
		(calc-grab-region (region-beginning) (region-end) nil)
	      (if (= (prefix-numeric-value arg) -2)
		  (calc-keypad)))))
    (if (get-buffer-window "*Calc Keypad*")
	(progn
	  (calc-keypad)
	  (set-buffer (window-buffer (selected-window)))))
    (if (eq major-mode 'calc-mode)
	(calc-quit)
      (let ((oldbuf (current-buffer)))
	(calc-create-buffer)
	(setq calc-was-keypad-mode nil)
	(if (or (eq full-display t)
		(and (null full-display) calc-full-mode))
	    (switch-to-buffer (current-buffer) t)
	  (if (get-buffer-window (current-buffer))
	      (select-window (get-buffer-window (current-buffer)))
	    (if (and (boundp 'calc-window-hook) calc-window-hook)
		(run-hooks 'calc-window-hook)
	      (let ((w (get-largest-window)))
		(if (and pop-up-windows
			 (> (window-height w)
			    (+ window-min-height calc-window-height 2)))
		    (progn
		      (setq w (split-window w
					    (- (window-height w)
					       calc-window-height 2)
					    nil))
		      (set-window-buffer w (current-buffer))
		      (select-window w))
		  (pop-to-buffer (current-buffer)))))))
	(save-excursion
	  (set-buffer (calc-trail-buffer))
	  (and calc-display-trail
	       (= (window-width) (frame-width))
	       (calc-trail-display 1 t)))
	(message "Welcome to the GNU Emacs Calculator!  Press `?' or `h' for help, `q' to quit")
	(run-hooks 'calc-start-hook)
	(and (windowp full-display)
	     (window-point full-display)
	     (select-window full-display))
	(calc-check-defines)
	(and calc-said-hello
	     (or (interactive-p) interactive)
	     (progn
	       (sit-for 2)
	       (message "")))
	(setq calc-said-hello t)))))

;;;###autoload
(defun full-calc ()
  "Invoke the Calculator and give it a full-sized window."
  (interactive)
  (calc nil t (interactive-p)))

(defun calc-same-interface (arg)
  "Invoke the Calculator using the most recent interface (calc or calc-keypad)."
  (interactive "P")
  (if (and (equal (buffer-name) "*Gnuplot Trail*")
	   (> (recursion-depth) 0))
      (exit-recursive-edit)
    (if (eq major-mode 'calc-edit-mode)
	(calc-edit-finish arg)
      (if (eq major-mode 'MacEdit-mode)
	  (MacEdit-finish-edit)
	(if calc-was-keypad-mode
	    (calc-keypad)
	  (calc arg calc-full-mode t))))))


(defun calc-quit (&optional non-fatal)
  (interactive)
  (and calc-standalone-flag (not non-fatal)
       (save-buffers-kill-emacs nil))
  (if (and (equal (buffer-name) "*Gnuplot Trail*")
	   (> (recursion-depth) 0))
      (exit-recursive-edit))
  (if (eq major-mode 'calc-edit-mode)
      (calc-edit-cancel)
    (if (eq major-mode 'MacEdit-mode)
	(MacEdit-cancel-edit)
      (if (and (interactive-p)
	       calc-embedded-info
	       (eq (current-buffer) (aref calc-embedded-info 0)))
	  (calc-embedded nil)
	(unless (eq major-mode 'calc-mode)
	  (calc-create-buffer))
	(run-hooks 'calc-end-hook)
	(setq calc-undo-list nil calc-redo-list nil)
	(mapcar (function (lambda (v) (set-default v (symbol-value v))))
		calc-local-var-list)
	(let ((buf (current-buffer))
	      (win (get-buffer-window (current-buffer)))
	      (kbuf (get-buffer "*Calc Keypad*")))
	  (delete-windows-on (calc-trail-buffer))
	  (if (and win
		   (< (window-height win) (1- (frame-height)))
		   (= (window-width win) (frame-width))  ; avoid calc-keypad
		   (not (get-buffer-window "*Calc Keypad*")))
	      (setq calc-window-height (- (window-height win) 2)))
	  (progn
	    (delete-windows-on buf)
	    (delete-windows-on kbuf))
	  (bury-buffer buf)
	  (bury-buffer calc-trail-buffer)
	  (and kbuf (bury-buffer kbuf)))))))

;;;###autoload
(defun quick-calc ()
  "Do a quick calculation in the minibuffer without invoking full Calculator."
  (interactive)
  (calc-do-quick-calc))

;;;###autoload
(defun calc-eval (str &optional separator &rest args)
  "Do a quick calculation and return the result as a string.
Return value will either be the formatted result in string form,
or a list containing a character position and an error message in string form."
  (calc-do-calc-eval str separator args))

;;;###autoload
(defun calc-keypad ()
  "Invoke the Calculator in \"visual keypad\" mode.
This is most useful in the X window system.
In this mode, click on the Calc \"buttons\" using the left mouse button.
Or, position the cursor manually and do M-x calc-keypad-press."
  (interactive)
  (calc-extensions)
  (calc-do-keypad calc-full-mode (interactive-p)))

;;;###autoload
(defun full-calc-keypad ()
  "Invoke the Calculator in full-screen \"visual keypad\" mode.
See calc-keypad for details."
  (interactive)
  (calc-extensions)
  (calc-do-keypad t (interactive-p)))


(defvar calc-aborted-prefix nil)
(defvar calc-start-time nil)
;;; Note that modifications to this function may break calc-pass-errors.
(defun calc-do (do-body &optional do-slow)
  (calc-check-defines)
  (let* ((calc-command-flags nil)
	 (calc-start-time (and calc-timing (not calc-start-time)
			       (calc-extensions)
			       (current-time-string)))
	 (gc-cons-threshold (max gc-cons-threshold
				 (if calc-timing 2000000 100000))))
    (setq calc-aborted-prefix "")
    (unwind-protect
	(condition-case err
	    (save-excursion
	      (if calc-embedded-info
		  (calc-embedded-select-buffer)
		(calc-select-buffer))
	      (and (eq calc-algebraic-mode 'total)
		   (calc-extensions)
		   (use-local-map calc-alg-map))
	      (when (and do-slow calc-display-working-message)
		(message "Working...")
		(calc-set-command-flag 'clear-message))
	      (funcall do-body)
	      (setq calc-aborted-prefix nil)
	      (when (memq 'renum-stack calc-command-flags)
		(calc-renumber-stack))
	      (when (memq 'clear-message calc-command-flags)
		(message "")))
	  (error
	   (if (and (eq (car err) 'error)
		    (stringp (nth 1 err))
		    (string-match "max-specpdl-size\\|max-lisp-eval-depth"
				  (nth 1 err)))
	       (error "Computation got stuck or ran too long.  Type `M' to increase the limit")
	     (setq calc-aborted-prefix nil)
	     (signal (car err) (cdr err)))))
      (setq calc-old-aborted-prefix calc-aborted-prefix)
      (when calc-aborted-prefix
	(calc-record "<Aborted>" calc-aborted-prefix))
      (and calc-start-time
	   (let* ((calc-internal-prec 12)
		  (calc-date-format nil)
		  (end-time (current-time-string))
		  (time (if (equal calc-start-time end-time)
			    0
			  (math-sub
			   (calcFunc-unixtime (math-parse-date end-time) 0)
			   (calcFunc-unixtime (math-parse-date calc-start-time)
					      0)))))
	     (if (math-lessp 1 time)
		 (calc-record time "(t)"))))
      (or (memq 'no-align calc-command-flags)
	  (eq major-mode 'calc-trail-mode)
	  (calc-align-stack-window))
      (and (memq 'position-point calc-command-flags)
	   (if (eq major-mode 'calc-mode)
	       (progn
		 (goto-line calc-final-point-line)
		 (move-to-column calc-final-point-column))
	     (save-excursion
	       (calc-select-buffer)
	       (goto-line calc-final-point-line)
	       (move-to-column calc-final-point-column))))
      (unless (memq 'keep-flags calc-command-flags)
	(save-excursion
	  (calc-select-buffer)
	  (setq calc-inverse-flag nil
		calc-hyperbolic-flag nil
		calc-keep-args-flag nil)))
      (when (memq 'do-edit calc-command-flags)
	(switch-to-buffer (get-buffer-create "*Calc Edit*")))
      (calc-set-mode-line)
      (when calc-embedded-info
	(calc-embedded-finish-command))))
  (identity nil))  ; allow a GC after timing is done


(defun calc-set-command-flag (f)
  (unless (memq f calc-command-flags)
    (setq calc-command-flags (cons f calc-command-flags))))

(defun calc-select-buffer ()
  (or (eq major-mode 'calc-mode)
      (if calc-main-buffer
	  (set-buffer calc-main-buffer)
	(let ((buf (get-buffer "*Calculator*")))
	  (if buf
	      (set-buffer buf)
	    (error "Calculator buffer not available"))))))

(defun calc-cursor-stack-index (&optional index)
  (goto-char (point-max))
  (forward-line (- (calc-substack-height (or index 1)))))

(defun calc-stack-size ()
  (- (length calc-stack) calc-stack-top))

(defun calc-substack-height (n)
  (let ((sum 0)
	(stack calc-stack))
    (setq n (+ n calc-stack-top))
    (while (and (> n 0) stack)
      (setq sum (+ sum (nth 1 (car stack)))
	    n (1- n)
	    stack (cdr stack)))
    sum))

(defun calc-set-mode-line ()
  (save-excursion
    (calc-select-buffer)
    (let* ((fmt (car calc-float-format))
	   (figs (nth 1 calc-float-format))
	   (new-mode-string
	    (format "Calc%s%s: %d %s %-14s"
		    (if calc-embedded-info "Embed" "")
		    (if (and (> (length (buffer-name)) 12)
			     (equal (substring (buffer-name) 0 12)
				    "*Calculator*"))
			(substring (buffer-name) 12)
		      "")
		    calc-internal-prec
		    (capitalize (symbol-name calc-angle-mode))
		    (concat

		     ;; Input-related modes
		     (if (eq calc-algebraic-mode 'total) "Alg* "
		       (if calc-algebraic-mode "Alg "
			 (if calc-incomplete-algebraic-mode "Alg[( " "")))

		     ;; Computational modes
		     (if calc-symbolic-mode "Symb " "")
		     (cond ((eq calc-matrix-mode 'matrix) "Matrix ")
			   ((integerp calc-matrix-mode)
			    (format "Matrix%d " calc-matrix-mode))
			   ((eq calc-matrix-mode 'scalar) "Scalar ")
			   (t ""))
		     (if (eq calc-complex-mode 'polar) "Polar " "")
		     (if calc-prefer-frac "Frac " "")
		     (cond ((null calc-infinite-mode) "")
			   ((eq calc-infinite-mode 1) "+Inf ")
			   (t "Inf "))
		     (cond ((eq calc-simplify-mode 'none) "NoSimp ")
			   ((eq calc-simplify-mode 'num) "NumSimp ")
			   ((eq calc-simplify-mode 'binary)
			    (format "BinSimp%d " calc-word-size))
			   ((eq calc-simplify-mode 'alg) "AlgSimp ")
			   ((eq calc-simplify-mode 'ext) "ExtSimp ")
			   ((eq calc-simplify-mode 'units) "UnitSimp ")
			   (t ""))

		     ;; Display modes
		     (cond ((= calc-number-radix 10) "")
			   ((= calc-number-radix 2) "Bin ")
			   ((= calc-number-radix 8) "Oct ")
			   ((= calc-number-radix 16) "Hex ")
			   (t (format "Radix%d " calc-number-radix)))
		     (if calc-leading-zeros "Zero " "")
		     (cond ((null calc-language) "")
			   ((eq calc-language 'tex) "TeX ")
			   (t (concat
			       (capitalize (symbol-name calc-language))
			       " ")))
		     (cond ((eq fmt 'float)
			    (if (zerop figs) "" (format "Norm%d " figs)))
			   ((eq fmt 'fix) (format "Fix%d " figs))
			   ((eq fmt 'sci)
			    (if (zerop figs) "Sci " (format "Sci%d " figs)))
			   ((eq fmt 'eng)
			    (if (zerop figs) "Eng " (format "Eng%d " figs))))
		     (cond ((not calc-display-just)
			    (if calc-display-origin
				(format "Left%d " calc-display-origin) ""))
			   ((eq calc-display-just 'right)
			    (if calc-display-origin
				(format "Right%d " calc-display-origin)
			      "Right "))
			   (t
			    (if calc-display-origin
				(format "Center%d " calc-display-origin)
			      "Center ")))
		     (cond ((integerp calc-line-breaking)
			    (format "Wid%d " calc-line-breaking))
			   (calc-line-breaking "")
			   (t "Wide "))

		     ;; Miscellaneous other modes/indicators
		     (if calc-assoc-selections "" "Break ")
		     (cond ((eq calc-mode-save-mode 'save) "Save ")
			   ((not calc-embedded-info) "")
			   ((eq calc-mode-save-mode 'local) "Local ")
			   ((eq calc-mode-save-mode 'edit) "LocEdit ")
			   ((eq calc-mode-save-mode 'perm) "LocPerm ")
			   ((eq calc-mode-save-mode 'global) "Global ")
			   (t ""))
		     (if calc-auto-recompute "" "Manual ")
		     (if (and (fboundp 'calc-gnuplot-alive)
			      (calc-gnuplot-alive)) "Graph " "")
		     (if (and calc-embedded-info
			      (> (calc-stack-size) 0)
			      (calc-top 1 'sel)) "Sel " "")
		     (if calc-display-dirty "Dirty " "")
		     (if calc-inverse-flag "Inv " "")
		     (if calc-hyperbolic-flag "Hyp " "")
		     (if calc-keep-args-flag "Keep " "")
		     (if (/= calc-stack-top 1) "Narrow " "")
		     (apply 'concat calc-other-modes)))))
      (if (equal new-mode-string mode-line-buffer-identification)
	  nil
	(setq mode-line-buffer-identification new-mode-string)
	(set-buffer-modified-p (buffer-modified-p))
	(and calc-embedded-info (calc-embedded-mode-line-change))))))

(defun calc-align-stack-window ()
  (if (eq major-mode 'calc-mode)
      (progn
	(let ((win (get-buffer-window (current-buffer))))
	  (if win
	      (progn
		(calc-cursor-stack-index 0)
		(vertical-motion (- 2 (window-height win)))
		(set-window-start win (point)))))
	(calc-cursor-stack-index 0)
	(if (looking-at " *\\.$")
	    (goto-char (1- (match-end 0)))))
    (save-excursion
      (calc-select-buffer)
      (calc-align-stack-window))))

(defun calc-check-stack (n)
  (if (> n (calc-stack-size))
      (error "Too few elements on stack"))
  (if (< n 0)
      (error "Invalid argument")))

(defun calc-push-list (vals &optional m sels)
  (while vals
    (if calc-executing-macro
	(calc-push-list-in-macro vals m sels)
      (save-excursion
	(calc-select-buffer)
	(let* ((val (car vals))
	       (entry (list val 1 (car sels)))
	       (mm (+ (or m 1) calc-stack-top)))
	  (calc-cursor-stack-index (1- (or m 1)))
	  (if (> mm 1)
	      (setcdr (nthcdr (- mm 2) calc-stack)
		      (cons entry (nthcdr (1- mm) calc-stack)))
	    (setq calc-stack (cons entry calc-stack)))
	  (let ((buffer-read-only nil))
	    (insert (math-format-stack-value entry) "\n"))
	  (calc-record-undo (list 'push mm))
	  (calc-set-command-flag 'renum-stack))))
    (setq vals (cdr vals)
	  sels (cdr sels))))

(defun calc-pop-push-list (n vals &optional m sels)
  (if (and calc-any-selections (null sels))
      (calc-replace-selections n vals m)
    (calc-pop-stack n m sels)
    (calc-push-list vals m sels)))

(defun calc-pop-push-record-list (n prefix vals &optional m sels)
  (or (and (consp vals)
	   (or (integerp (car vals))
	       (consp (car vals))))
      (and vals (setq vals (list vals)
		      sels (and sels (list sels)))))
  (calc-check-stack (+ n (or m 1) -1))
  (if prefix
      (if (cdr vals)
	  (calc-record-list vals prefix)
	(calc-record (car vals) prefix)))
  (calc-pop-push-list n vals m sels))

(defun calc-enter-result (n prefix vals &optional m)
  (setq calc-aborted-prefix prefix)
  (if (and (consp vals)
	   (or (integerp (car vals))
	       (consp (car vals))))
      (setq vals (mapcar 'calc-normalize vals))
    (setq vals (calc-normalize vals)))
  (or (and (consp vals)
	   (or (integerp (car vals))
	       (consp (car vals))))
      (setq vals (list vals)))
  (if (equal vals '((nil)))
      (setq vals nil))
  (calc-pop-push-record-list n prefix vals m)
  (calc-handle-whys))

(defun calc-normalize (val)
  (if (memq calc-simplify-mode '(nil none num))
      (math-normalize val)
    (calc-extensions)
    (calc-normalize-fancy val)))

(defun calc-handle-whys ()
  (if calc-next-why
      (calc-do-handle-whys)))


(defun calc-pop-stack (&optional n m sel-ok)  ; pop N objs at level M of stack.
  (or n (setq n 1))
  (or m (setq m 1))
  (or calc-keep-args-flag
      (let ((mm (+ m calc-stack-top)))
	(if (and calc-any-selections (not sel-ok)
		 (calc-top-selected n m))
	    (calc-sel-error))
	(if calc-executing-macro
	    (calc-pop-stack-in-macro n mm)
	  (calc-record-undo (list 'pop mm (calc-top-list n m 'full)))
	  (save-excursion
	    (calc-select-buffer)
	    (let ((buffer-read-only nil))
	      (if (> mm 1)
		  (progn
		    (calc-cursor-stack-index (1- m))
		    (let ((bot (point)))
		      (calc-cursor-stack-index (+ n m -1))
		      (delete-region (point) bot))
		    (setcdr (nthcdr (- mm 2) calc-stack)
			    (nthcdr (+ n mm -1) calc-stack)))
		(calc-cursor-stack-index n)
		(setq calc-stack (nthcdr n calc-stack))
		(delete-region (point) (point-max))))
	    (calc-set-command-flag 'renum-stack))))))

(defun calc-get-stack-element (x)
  (cond ((eq sel-mode 'entry)
	 x)
	((eq sel-mode 'sel)
	 (nth 2 x))
	((or (null (nth 2 x))
	     (eq sel-mode 'full)
	     (not calc-use-selections))
	 (car x))
	(sel-mode
	 (calc-sel-error))
	(t (nth 2 x))))

;; Get the Nth element of the stack (N=1 is the top element).
(defun calc-top (&optional n sel-mode)
  (or n (setq n 1))
  (calc-check-stack n)
  (calc-get-stack-element (nth (+ n calc-stack-top -1) calc-stack)))

(defun calc-top-n (&optional n sel-mode)    ; in case precision has changed
  (math-check-complete (calc-normalize (calc-top n sel-mode))))

(defun calc-top-list (&optional n m sel-mode)
  (or n (setq n 1))
  (or m (setq m 1))
  (calc-check-stack (+ n m -1))
  (and (> n 0)
       (let ((top (copy-sequence (nthcdr (+ m calc-stack-top -1)
					 calc-stack))))
	 (setcdr (nthcdr (1- n) top) nil)
	 (nreverse (mapcar 'calc-get-stack-element top)))))

(defun calc-top-list-n (&optional n m sel-mode)
  (mapcar 'math-check-complete
	  (mapcar 'calc-normalize (calc-top-list n m sel-mode))))


(defun calc-renumber-stack ()
  (if calc-line-numbering
      (save-excursion
	(calc-cursor-stack-index 0)
	(let ((lnum 1)
	      (buffer-read-only nil)
	      (stack (nthcdr calc-stack-top calc-stack)))
	  (if (re-search-forward "^[0-9]+[:*]" nil t)
	      (progn
		(beginning-of-line)
		(while (re-search-forward "^[0-9]+[:*]" nil t)
		  (let ((buffer-read-only nil))
		    (beginning-of-line)
		    (delete-char 4)
		    (insert "    ")))
		(calc-cursor-stack-index 0)))
	  (while (re-search-backward "^[0-9]+[:*]" nil t)
	    (delete-char 4)
	    (if (> lnum 999)
		(insert (format "%03d%s" (% lnum 1000)
				(if (and (nth 2 (car stack))
					 calc-use-selections) "*" ":")))
	      (let ((prefix (int-to-string lnum)))
		(insert prefix (if (and (nth 2 (car stack))
					calc-use-selections) "*" ":")
			(make-string (- 3 (length prefix)) 32))))
	    (beginning-of-line)
	    (setq lnum (1+ lnum)
		  stack (cdr stack))))))
  (and calc-embedded-info (calc-embedded-stack-change)))

(defun calc-refresh (&optional align)
  (interactive)
  (and (eq major-mode 'calc-mode)
       (not calc-executing-macro)
       (let* ((buffer-read-only nil)
	      (save-point (point))
	      (save-mark (condition-case err (mark) (error nil)))
	      (save-aligned (looking-at "\\.$"))
	      (thing calc-stack))
	 (setq calc-any-selections nil
	       calc-any-evaltos nil)
	 (erase-buffer)
	 (when calc-show-banner
	   (insert "--- Emacs Calculator Mode ---\n"))
	 (while thing
	   (goto-char (point-min))
	   (when calc-show-banner
	     (forward-line 1))
	   (insert (math-format-stack-value (car thing)) "\n")
	   (setq thing (cdr thing)))
	 (calc-renumber-stack)
	 (if calc-display-dirty
	     (calc-wrapper (setq calc-display-dirty nil)))
	 (and calc-any-evaltos calc-auto-recompute
	      (calc-wrapper (calc-refresh-evaltos)))
	 (if (or align save-aligned)
	     (calc-align-stack-window)
	   (goto-char save-point))
	 (if save-mark (set-mark save-mark))))
  (and calc-embedded-info (not (eq major-mode 'calc-mode))
       (save-excursion
	 (set-buffer (aref calc-embedded-info 1))
	 (calc-refresh align)))
  (setq calc-refresh-count (1+ calc-refresh-count)))


(defun calc-x-paste-text (arg)
  "Move point to mouse position and insert window system cut buffer contents.
If mouse is pressed in Calc window, push cut buffer contents onto the stack."
  (x-mouse-select arg)
  (if (memq major-mode '(calc-mode calc-trail-mode))
      (progn
	(calc-wrapper
	 (calc-extensions)
	 (let* ((buf (x-get-cut-buffer))
		(val (math-read-exprs (calc-clean-newlines buf))))
	   (if (eq (car-safe val) 'error)
	       (progn
		 (setq val (math-read-exprs buf))
		 (if (eq (car-safe val) 'error)
		     (error "%s in yanked data" (nth 2 val)))))
	   (calc-enter-result 0 "Xynk" val))))
    (x-paste-text arg)))



;;;; The Calc Trail buffer.

(defun calc-check-trail-aligned ()
  (save-excursion
    (let ((win (get-buffer-window (current-buffer))))
      (and win
	   (pos-visible-in-window-p (1- (point-max)) win)))))

(defun calc-trail-buffer ()
  (and (or (null calc-trail-buffer)
	   (null (buffer-name calc-trail-buffer)))
       (save-excursion
	 (setq calc-trail-buffer (get-buffer-create "*Calc Trail*"))
	 (let ((buf (or (and (not (eq major-mode 'calc-mode))
			     (get-buffer "*Calculator*"))
			(current-buffer))))
	   (set-buffer calc-trail-buffer)
	   (or (eq major-mode 'calc-trail-mode)
	       (calc-trail-mode buf)))))
  (or (and calc-trail-pointer
	   (eq (marker-buffer calc-trail-pointer) calc-trail-buffer))
      (save-excursion
	(set-buffer calc-trail-buffer)
	(goto-line 2)
	(setq calc-trail-pointer (point-marker))))
  calc-trail-buffer)

(defun calc-record (val &optional prefix)
  (setq calc-aborted-prefix nil)
  (or calc-executing-macro
      (let* ((mainbuf (current-buffer))
	     (buf (calc-trail-buffer))
	     (calc-display-raw nil)
	     (calc-can-abbrev-vectors t)
	     (fval (if val
		       (if (stringp val)
			   val
			 (math-showing-full-precision
			  (math-format-flat-expr val 0)))
		     "")))
	(save-excursion
	  (set-buffer buf)
	  (let ((aligned (calc-check-trail-aligned))
		(buffer-read-only nil))
	    (goto-char (point-max))
	    (cond ((null prefix) (insert "     "))
		  ((and (> (length prefix) 4)
			(string-match " " prefix 4))
		   (insert (substring prefix 0 4) " "))
		  (t (insert (format "%4s " prefix))))
	    (insert fval "\n")
	    (let ((win (get-buffer-window buf)))
	      (if (and aligned win (not (memq 'hold-trail calc-command-flags)))
		  (calc-trail-here))
	      (goto-char (1- (point-max))))))))
  val)


(defun calc-trail-display (flag &optional no-refresh)
  (interactive "P")
  (let ((win (get-buffer-window (calc-trail-buffer))))
    (if (setq calc-display-trail
	      (not (if flag (memq flag '(nil 0)) win)))
	(if (null win)
	    (progn
	      (if (and (boundp 'calc-trail-window-hook) calc-trail-window-hook)
		  (run-hooks 'calc-trail-window-hook)
		(let ((w (split-window nil (/ (* (window-width) 2) 3) t)))
		  (set-window-buffer w calc-trail-buffer)))
	      (calc-wrapper
	       (setq overlay-arrow-string calc-trail-overlay
		     overlay-arrow-position calc-trail-pointer)
	       (or no-refresh
		   (if (interactive-p)
		       (calc-do-refresh)
		     (calc-refresh))))))
      (if win
	  (progn
	    (delete-window win)
	    (calc-wrapper
	     (or no-refresh
		 (if (interactive-p)
		     (calc-do-refresh)
		   (calc-refresh))))))))
  calc-trail-buffer)

(defun calc-trail-here ()
  (interactive)
  (if (eq major-mode 'calc-trail-mode)
      (progn
	(beginning-of-line)
	(if (bobp)
	    (forward-line 1)
	  (if (eobp)
	      (forward-line -1)))
	(if (or (bobp) (eobp))
	    (setq overlay-arrow-position nil)   ; trail is empty
	  (set-marker calc-trail-pointer (point) (current-buffer))
	  (setq calc-trail-overlay (concat (buffer-substring (point)
							     (+ (point) 4))
					   ">")
		overlay-arrow-string calc-trail-overlay
		overlay-arrow-position calc-trail-pointer)
	  (forward-char 4)
	  (let ((win (get-buffer-window (current-buffer))))
	    (if win
		(save-excursion
		  (forward-line (/ (window-height win) 2))
		  (forward-line (- 1 (window-height win)))
		  (set-window-start win (point))
		  (set-window-point win (+ calc-trail-pointer 4))
		  (set-buffer calc-main-buffer)
		  (setq overlay-arrow-string calc-trail-overlay
			overlay-arrow-position calc-trail-pointer))))))
    (error "Not in Calc Trail buffer")))




;;;; The Undo list.

(defun calc-record-undo (rec)
  (or calc-executing-macro
      (if (memq 'undo calc-command-flags)
	  (setq calc-undo-list (cons (cons rec (car calc-undo-list))
				     (cdr calc-undo-list)))
	(setq calc-undo-list (cons (list rec) calc-undo-list)
	      calc-redo-list nil)
	(calc-set-command-flag 'undo))))




;;; Arithmetic commands.

(defun calc-binary-op (name func arg &optional ident unary func2)
  (setq calc-aborted-prefix name)
  (if (null arg)
      (calc-enter-result 2 name (cons (or func2 func)
				      (mapcar 'math-check-complete
					      (calc-top-list 2))))
    (calc-extensions)
    (calc-binary-op-fancy name func arg ident unary)))

(defun calc-unary-op (name func arg &optional func2)
  (setq calc-aborted-prefix name)
  (if (null arg)
      (calc-enter-result 1 name (list (or func2 func)
				      (math-check-complete (calc-top 1))))
    (calc-extensions)
    (calc-unary-op-fancy name func arg)))


(defun calc-plus (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "+" 'calcFunc-add arg 0 nil '+)))

(defun calc-minus (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "-" 'calcFunc-sub arg 0 'neg '-)))

(defun calc-times (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "*" 'calcFunc-mul arg 1 nil '*)))

(defun calc-divide (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "/" 'calcFunc-div arg 0 'calcFunc-inv '/)))


(defun calc-change-sign (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "chs" 'neg arg)))



;;; Stack management commands.

(defun calc-enter (n)
  (interactive "p")
  (calc-wrapper
   (cond ((< n 0)
	  (calc-push-list (calc-top-list 1 (- n))))
	 ((= n 0)
	  (calc-push-list (calc-top-list (calc-stack-size))))
	 (t
	  (calc-push-list (calc-top-list n))))))


(defun calc-pop (n)
  (interactive "P")
  (calc-wrapper
   (let* ((nn (prefix-numeric-value n))
	  (top (and (null n) (calc-top 1))))
     (cond ((and (null n)
		 (eq (car-safe top) 'incomplete)
		 (> (length top) (if (eq (nth 1 top) 'intv) 3 2)))
	    (calc-pop-push-list 1 (let ((tt (copy-sequence top)))
				    (setcdr (nthcdr (- (length tt) 2) tt) nil)
				    (list tt))))
	   ((< nn 0)
	    (if (and calc-any-selections
		     (calc-top-selected 1 (- nn)))
		(calc-delete-selection (- nn))
	      (calc-pop-stack 1 (- nn) t)))
	   ((= nn 0)
	    (calc-pop-stack (calc-stack-size) 1 t))
	   (t
	    (if (and calc-any-selections
		     (= nn 1)
		     (calc-top-selected 1 1))
		(calc-delete-selection 1)
	      (calc-pop-stack nn)))))))




;;;; Reading a number using the minibuffer.

(defun calcDigit-start ()
  (interactive)
  (calc-wrapper
   (if (or calc-algebraic-mode
	   (and (> calc-number-radix 14) (eq last-command-char ?e)))
       (calc-alg-digit-entry)
     (calc-unread-command)
     (setq calc-aborted-prefix nil)
     (let* ((calc-digit-value nil)
	    (calc-prev-char nil)
	    (calc-prev-prev-char nil)
	    (calc-buffer (current-buffer))
	    (buf (if calc-emacs-type-lucid
		     (catch 'calc-foo
		       (catch 'execute-kbd-macro
			 (throw 'calc-foo
				(read-from-minibuffer
				 "Calc: " "" calc-digit-map)))
		       (error "Lucid Emacs requires RET after %s"
			      "digit entry in kbd macro"))
		   (let ((old-esc (lookup-key global-map "\e")))
		     (unwind-protect
			 (progn
			   (define-key global-map "\e" nil)
			   (read-from-minibuffer "Calc: " "" calc-digit-map))
		       (define-key global-map "\e" old-esc))))))
       (or calc-digit-value (setq calc-digit-value (math-read-number buf)))
       (if (stringp calc-digit-value)
	   (calc-alg-entry calc-digit-value)
	 (if calc-digit-value
	     (calc-push-list (list (calc-record (calc-normalize
						 calc-digit-value))))))
       (if (eq calc-prev-char 'dots)
	   (progn
	     (calc-extensions)
	     (calc-dots)))))))

(defsubst calc-minibuffer-size ()
  (- (point-max) (minibuffer-prompt-end)))

(defun calcDigit-nondigit ()
  (interactive)
  ;; Exercise for the reader:  Figure out why this is a good precaution!
  (or (boundp 'calc-buffer)
      (use-local-map minibuffer-local-map))
  (let ((str (minibuffer-contents)))
    (setq calc-digit-value (save-excursion
			     (set-buffer calc-buffer)
			     (math-read-number str))))
  (if (and (null calc-digit-value) (> (calc-minibuffer-size) 0))
      (progn
	(beep)
	(calc-temp-minibuffer-message " [Bad format]"))
    (or (memq last-command-char '(32 13))
	(progn (setq prefix-arg current-prefix-arg)
	       (calc-unread-command (if (and (eq last-command-char 27)
					     (>= last-input-char 128))
					last-input-char
				      nil))))
    (exit-minibuffer)))


(defun calc-minibuffer-contains (rex)
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (looking-at rex)))

(defun calcDigit-key ()
  (interactive)
  (goto-char (point-max))
  (if (or (and (memq last-command-char '(?+ ?-))
	       (> (buffer-size) 0)
	       (/= (preceding-char) ?e))
	  (and (memq last-command-char '(?m ?s))
	       (not (calc-minibuffer-contains "[-+]?[0-9]+\\.?0*[@oh].*"))
	       (not (calc-minibuffer-contains "[-+]?\\(1[1-9]\\|[2-9][0-9]\\)#.*"))))
      (calcDigit-nondigit)
    (if (calc-minibuffer-contains "\\([-+]?\\|.* \\)\\'")
	(cond ((memq last-command-char '(?. ?@)) (insert "0"))
	      ((and (memq last-command-char '(?o ?h ?m))
		    (not (calc-minibuffer-contains ".*#.*"))) (insert "0"))
	      ((memq last-command-char '(?: ?e)) (insert "1"))
	      ((eq last-command-char ?#)
	       (insert (int-to-string calc-number-radix)))))
    (if (and (calc-minibuffer-contains "\\([-+]?[0-9]+#\\|[^:]*:\\)\\'")
	     (eq last-command-char ?:))
	(insert "1"))
    (if (and (calc-minibuffer-contains "[-+]?[0-9]+#\\'")
	     (eq last-command-char ?.))
	(insert "0"))
    (if (and (calc-minibuffer-contains "[-+]?0*\\([2-9]\\|1[0-4]\\)#\\'")
	     (eq last-command-char ?e))
	(insert "1"))
    (if (or (and (memq last-command-char '(?h ?o ?m ?s ?p))
		 (calc-minibuffer-contains ".*#.*"))
	    (and (eq last-command-char ?e)
		 (calc-minibuffer-contains "[-+]?\\(1[5-9]\\|[2-9][0-9]\\)#.*"))
	    (and (eq last-command-char ?n)
		 (calc-minibuffer-contains "[-+]?\\(2[4-9]\\|[3-9][0-9]\\)#.*")))
	(setq last-command-char (upcase last-command-char)))
    (cond
     ((memq last-command-char '(?_ ?n))
      (goto-char (minibuffer-prompt-end))
      (if (and (search-forward " +/- " nil t)
	       (not (search-forward "e" nil t)))
	  (beep)
	(and (not (calc-minibuffer-contains "[-+]?\\(1[5-9]\\|[2-9][0-9]\\)#.*"))
	     (search-forward "e" nil t))
	(if (looking-at "+")
	    (delete-char 1))
	(if (looking-at "-")
	    (delete-char 1)
	  (insert "-")))
      (goto-char (point-max)))
     ((eq last-command-char ?p)
      (if (or (calc-minibuffer-contains ".*\\+/-.*")
	      (calc-minibuffer-contains ".*mod.*")
	      (calc-minibuffer-contains ".*#.*")
	      (calc-minibuffer-contains ".*[-+e:]\\'"))
	  (beep)
	(if (not (calc-minibuffer-contains ".* \\'"))
	    (insert " "))
	(insert "+/- ")))
     ((and (eq last-command-char ?M)
	   (not (calc-minibuffer-contains
		 "[-+]?\\(2[3-9]\\|[3-9][0-9]\\)#.*")))
      (if (or (calc-minibuffer-contains ".*\\+/-.*")
	      (calc-minibuffer-contains ".*mod *[^ ]+")
	      (calc-minibuffer-contains ".*[-+e:]\\'"))
	  (beep)
	(if (calc-minibuffer-contains ".*mod \\'")
	    (if calc-previous-modulo
		(insert (math-format-flat-expr calc-previous-modulo 0))
	      (beep))
	  (if (not (calc-minibuffer-contains ".* \\'"))
	      (insert " "))
	  (insert "mod "))))
     (t
      (insert (char-to-string last-command-char))
      (if (or (and (calc-minibuffer-contains "[-+]?\\(.*\\+/- *\\|.*mod *\\)?\\([0-9][0-9]?\\)#[0-9a-zA-Z]*\\(:[0-9a-zA-Z]*\\(:[0-9a-zA-Z]*\\)?\\|.[0-9a-zA-Z]*\\(e[-+]?[0-9]*\\)?\\)?\\'")
		   (let ((radix (string-to-int
				 (buffer-substring
				  (match-beginning 2) (match-end 2)))))
		     (and (>= radix 2)
			  (<= radix 36)
			  (or (memq last-command-char '(?# ?: ?. ?e ?+ ?-))
			      (let ((dig (math-read-radix-digit
					  (upcase last-command-char))))
				(and dig
				     (< dig radix)))))))
	      (calc-minibuffer-contains
	       "[-+]?\\(.*\\+/- *\\|.*mod *\\)?\\([0-9]+\\.?0*[@oh] *\\)?\\([0-9]+\\.?0*['m] *\\)?[0-9]*\\(\\.?[0-9]*\\(e[-+]?[0-3]?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?\\)?\\|[0-9]:\\([0-9]+:\\)?[0-9]*\\)?[\"s]?\\'"))
	  (if (and (memq last-command-char '(?@ ?o ?h ?\' ?m))
		   (string-match " " calc-hms-format))
	      (insert " "))
	(if (and (eq this-command last-command)
		 (eq last-command-char ?.))
	    (progn
	      (calc-extensions)
	      (calc-digit-dots))
	  (delete-backward-char 1)
	  (beep)
	  (calc-temp-minibuffer-message " [Bad format]"))))))
  (setq calc-prev-prev-char calc-prev-char
	calc-prev-char last-command-char))


(defun calcDigit-backspace ()
  (interactive)
  (goto-char (point-max))
  (cond ((calc-minibuffer-contains ".* \\+/- \\'")
	 (backward-delete-char 5))
	((calc-minibuffer-contains ".* mod \\'")
	 (backward-delete-char 5))
	((calc-minibuffer-contains ".* \\'")
	 (backward-delete-char 2))
	((eq last-command 'calcDigit-start)
	 (erase-buffer))
	(t (backward-delete-char 1)))
  (if (= (calc-minibuffer-size) 0)
      (progn
	(setq last-command-char 13)
	(calcDigit-nondigit))))







;;;; Arithmetic routines.
;;;
;;; An object as manipulated by one of these routines may take any of the
;;; following forms:
;;;
;;; integer                 An integer.  For normalized numbers, this format
;;;			    is used only for -999999 ... 999999.
;;;
;;; (bigpos N0 N1 N2 ...)   A big positive integer, N0 + N1*1000 + N2*10^6 ...
;;; (bigneg N0 N1 N2 ...)   A big negative integer, - N0 - N1*1000 ...
;;;			    Each digit N is in the range 0 ... 999.
;;;			    Normalized, always at least three N present,
;;;			    and the most significant N is nonzero.
;;;
;;; (frac NUM DEN)          A fraction.  NUM and DEN are small or big integers.
;;;                         Normalized, DEN > 1.
;;;
;;; (float NUM EXP)         A floating-point number, NUM * 10^EXP;
;;;                         NUM is a small or big integer, EXP is a small int.
;;;			    Normalized, NUM is not a multiple of 10, and
;;;			    abs(NUM) < 10^calc-internal-prec.
;;;			    Normalized zero is stored as (float 0 0).
;;;
;;; (cplx REAL IMAG)        A complex number; REAL and IMAG are any of above.
;;;			    Normalized, IMAG is nonzero.
;;;
;;; (polar R THETA)         Polar complex number.  Normalized, R > 0 and THETA
;;;                         is neither zero nor 180 degrees (pi radians).
;;;
;;; (vec A B C ...)         Vector of objects A, B, C, ...  A matrix is a
;;;                         vector of vectors.
;;;
;;; (hms H M S)             Angle in hours-minutes-seconds form.  All three
;;;                         components have the same sign; H and M must be
;;;                         numerically integers; M and S are expected to
;;;                         lie in the range [0,60).
;;;
;;; (date N)                A date or date/time object.  N is an integer to
;;;			    store a date only, or a fraction or float to
;;;			    store a date and time.
;;;
;;; (sdev X SIGMA)          Error form, X +/- SIGMA.  When normalized,
;;;                         SIGMA > 0.  X is any complex number and SIGMA
;;;			    is real numbers; or these may be symbolic
;;;                         expressions where SIGMA is assumed real.
;;;
;;; (intv MASK LO HI)       Interval form.  MASK is 0=(), 1=(], 2=[), or 3=[].
;;;                         LO and HI are any real numbers, or symbolic
;;;			    expressions which are assumed real, and LO < HI.
;;;			    For [LO..HI], if LO = HI normalization produces LO,
;;;			    and if LO > HI normalization produces [LO..LO).
;;;			    For other intervals, if LO > HI normalization
;;;			    sets HI equal to LO.
;;;
;;; (mod N M)	    	    Number modulo M.  When normalized, 0 <= N < M.
;;;			    N and M are real numbers.
;;;
;;; (var V S)		    Symbolic variable.  V is a Lisp symbol which
;;;			    represents the variable's visible name.  S is
;;;			    the symbol which actually stores the variable's
;;;			    value:  (var pi var-pi).
;;;
;;; In general, combining rational numbers in a calculation always produces
;;; a rational result, but if either argument is a float, result is a float.

;;; In the following comments, [x y z] means result is x, args must be y, z,
;;; respectively, where the code letters are:
;;;
;;;    O  Normalized object (vector or number)
;;;    V  Normalized vector
;;;    N  Normalized number of any type
;;;    N  Normalized complex number
;;;    R  Normalized real number (float or rational)
;;;    F  Normalized floating-point number
;;;    T  Normalized rational number
;;;    I  Normalized integer
;;;    B  Normalized big integer
;;;    S  Normalized small integer
;;;    D  Digit (small integer, 0..999)
;;;    L  Normalized bignum digit list (without "bigpos" or "bigneg" symbol)
;;;       or normalized vector element list (without "vec")
;;;    P  Predicate (truth value)
;;;    X  Any Lisp object
;;;    Z  "nil"
;;;
;;; Lower-case letters signify possibly un-normalized values.
;;; "L.D" means a cons of an L and a D.
;;; [N N; n n] means result will be normalized if argument is.
;;; Also, [Public] marks routines intended to be called from outside.
;;; [This notation has been neglected in many recent routines.]

;;; Reduce an object to canonical (normalized) form.  [O o; Z Z] [Public]
(defun math-normalize (a)
  (cond
   ((not (consp a))
    (if (integerp a)
	(if (or (>= a 1000000) (<= a -1000000))
	    (math-bignum a)
	  a)
      a))
   ((eq (car a) 'bigpos)
    (if (eq (nth (1- (length a)) a) 0)
	(let* ((last (setq a (copy-sequence a))) (digs a))
	  (while (setq digs (cdr digs))
	    (or (eq (car digs) 0) (setq last digs)))
	  (setcdr last nil)))
    (if (cdr (cdr (cdr a)))
	a
      (cond
       ((cdr (cdr a)) (+ (nth 1 a) (* (nth 2 a) 1000)))
       ((cdr a) (nth 1 a))
       (t 0))))
   ((eq (car a) 'bigneg)
    (if (eq (nth (1- (length a)) a) 0)
	(let* ((last (setq a (copy-sequence a))) (digs a))
	  (while (setq digs (cdr digs))
	    (or (eq (car digs) 0) (setq last digs)))
	  (setcdr last nil)))
    (if (cdr (cdr (cdr a)))
	a
      (cond
       ((cdr (cdr a)) (- (+ (nth 1 a) (* (nth 2 a) 1000))))
       ((cdr a) (- (nth 1 a)))
       (t 0))))
   ((eq (car a) 'float)
    (math-make-float (math-normalize (nth 1 a)) (nth 2 a)))
   ((or (memq (car a) '(frac cplx polar hms date mod sdev intv vec var quote
			     special-const calcFunc-if calcFunc-lambda
			     calcFunc-quote calcFunc-condition
			     calcFunc-evalto))
	(integerp (car a))
	(and (consp (car a)) (not (eq (car (car a)) 'lambda))))
    (calc-extensions)
    (math-normalize-fancy a))
   (t
    (or (and calc-simplify-mode
	     (calc-extensions)
	     (math-normalize-nonstandard))
	(let ((args (mapcar 'math-normalize (cdr a))))
	  (or (condition-case err
		  (let ((func (assq (car a) '( ( + . math-add )
					       ( - . math-sub )
					       ( * . math-mul )
					       ( / . math-div )
					       ( % . math-mod )
					       ( ^ . math-pow )
					       ( neg . math-neg )
					       ( | . math-concat ) ))))
		    (or (and var-EvalRules
			     (progn
			       (or (eq var-EvalRules math-eval-rules-cache-tag)
				   (progn
				     (calc-extensions)
				     (math-recompile-eval-rules)))
			       (and (or math-eval-rules-cache-other
					(assq (car a) math-eval-rules-cache))
				    (math-apply-rewrites
				     (cons (car a) args)
				     (cdr math-eval-rules-cache)
				     nil math-eval-rules-cache))))
			(if func
			    (apply (cdr func) args)
			  (and (or (consp (car a))
				   (fboundp (car a))
				   (and (not calc-extensions-loaded)
					(calc-extensions)
					(fboundp (car a))))
			       (apply (car a) args)))))
		(wrong-number-of-arguments
		 (calc-record-why "*Wrong number of arguments"
				  (cons (car a) args))
		 nil)
		(wrong-type-argument
		 (or calc-next-why (calc-record-why "Wrong type of argument"
						    (cons (car a) args)))
		 nil)
		(args-out-of-range
		 (calc-record-why "*Argument out of range" (cons (car a) args))
		 nil)
		(inexact-result
		 (calc-record-why "No exact representation for result"
				  (cons (car a) args))
		 nil)
		(math-overflow
		 (calc-record-why "*Floating-point overflow occurred"
				  (cons (car a) args))
		 nil)
		(math-underflow
		 (calc-record-why "*Floating-point underflow occurred"
				  (cons (car a) args))
		 nil)
		(void-variable
		 (if (eq (nth 1 err) 'var-EvalRules)
		     (progn
		       (setq var-EvalRules nil)
		       (math-normalize (cons (car a) args)))
		   (calc-record-why "*Variable is void" (nth 1 err)))))
	      (if (consp (car a))
		  (math-dimension-error)
		(cons (car a) args))))))))



;;; True if A is a floating-point real or complex number.  [P x] [Public]
(defun math-floatp (a)
  (cond ((eq (car-safe a) 'float) t)
	((memq (car-safe a) '(cplx polar mod sdev intv))
	 (or (math-floatp (nth 1 a))
	     (math-floatp (nth 2 a))
	     (and (eq (car a) 'intv) (math-floatp (nth 3 a)))))
	((eq (car-safe a) 'date)
	 (math-floatp (nth 1 a)))))



;;; Verify that A is a complete object and return A.  [x x] [Public]
(defun math-check-complete (a)
  (cond ((integerp a) a)
	((eq (car-safe a) 'incomplete)
	 (calc-incomplete-error a))
	((consp a) a)
	(t (error "Invalid data object encountered"))))



;;; Coerce integer A to be a bignum.  [B S]
(defun math-bignum (a)
  (if (>= a 0)
      (cons 'bigpos (math-bignum-big a))
    (cons 'bigneg (math-bignum-big (- a)))))

(defun math-bignum-big (a)   ; [L s]
  (if (= a 0)
      nil
    (cons (% a 1000) (math-bignum-big (/ a 1000)))))


;;; Build a normalized floating-point number.  [F I S]
(defun math-make-float (mant exp)
  (if (eq mant 0)
      '(float 0 0)
    (let* ((ldiff (- calc-internal-prec (math-numdigs mant))))
      (if (< ldiff 0)
	  (setq mant (math-scale-rounding mant ldiff)
		exp (- exp ldiff))))
    (if (consp mant)
	(let ((digs (cdr mant)))
	  (if (= (% (car digs) 10) 0)
	      (progn
		(while (= (car digs) 0)
		  (setq digs (cdr digs)
			exp (+ exp 3)))
		(while (= (% (car digs) 10) 0)
		  (setq digs (math-div10-bignum digs)
			exp (1+ exp)))
		(setq mant (math-normalize (cons (car mant) digs))))))
      (while (= (% mant 10) 0)
	(setq mant (/ mant 10)
	      exp (1+ exp))))
    (if (and (<= exp -4000000)
	     (<= (+ exp (math-numdigs mant) -1) -4000000))
	(signal 'math-underflow nil)
      (if (and (>= exp 3000000)
	       (>= (+ exp (math-numdigs mant) -1) 4000000))
	  (signal 'math-overflow nil)
	(list 'float mant exp)))))

(defun math-div10-bignum (a)   ; [l l]
  (if (cdr a)
      (cons (+ (/ (car a) 10) (* (% (nth 1 a) 10) 100))
	    (math-div10-bignum (cdr a)))
    (list (/ (car a) 10))))

;;; Coerce A to be a float.  [F N; V V] [Public]
(defun math-float (a)
  (cond ((Math-integerp a) (math-make-float a 0))
	((eq (car a) 'frac) (math-div (math-float (nth 1 a)) (nth 2 a)))
	((eq (car a) 'float) a)
	((memq (car a) '(cplx polar vec hms date sdev mod))
	 (cons (car a) (mapcar 'math-float (cdr a))))
	(t (math-float-fancy a))))


(defun math-neg (a)
  (cond ((not (consp a)) (- a))
	((eq (car a) 'bigpos) (cons 'bigneg (cdr a)))
	((eq (car a) 'bigneg) (cons 'bigpos (cdr a)))
	((memq (car a) '(frac float))
	 (list (car a) (Math-integer-neg (nth 1 a)) (nth 2 a)))
	((memq (car a) '(cplx vec hms date calcFunc-idn))
	 (cons (car a) (mapcar 'math-neg (cdr a))))
	(t (math-neg-fancy a))))


;;; Compute the number of decimal digits in integer A.  [S I]
(defun math-numdigs (a)
  (if (consp a)
      (if (cdr a)
	  (let* ((len (1- (length a)))
		 (top (nth len a)))
	    (+ (* len 3) (cond ((>= top 100) 0) ((>= top 10) -1) (t -2))))
	0)
    (cond ((>= a 100) (+ (math-numdigs (/ a 1000)) 3))
	  ((>= a 10) 2)
	  ((>= a 1) 1)
	  ((= a 0) 0)
	  ((> a -10) 1)
	  ((> a -100) 2)
	  (t (math-numdigs (- a))))))

;;; Multiply (with truncation toward 0) the integer A by 10^N.  [I i S]
(defun math-scale-int (a n)
  (cond ((= n 0) a)
	((> n 0) (math-scale-left a n))
	(t (math-normalize (math-scale-right a (- n))))))

(defun math-scale-left (a n)   ; [I I S]
  (if (= n 0)
      a
    (if (consp a)
	(cons (car a) (math-scale-left-bignum (cdr a) n))
      (if (>= n 3)
	  (if (or (>= a 1000) (<= a -1000))
	      (math-scale-left (math-bignum a) n)
	    (math-scale-left (* a 1000) (- n 3)))
	(if (= n 2)
	    (if (or (>= a 10000) (<= a -10000))
		(math-scale-left (math-bignum a) 2)
	      (* a 100))
	  (if (or (>= a 100000) (<= a -100000))
	      (math-scale-left (math-bignum a) 1)
	    (* a 10)))))))

(defun math-scale-left-bignum (a n)
  (if (>= n 3)
      (while (>= (setq a (cons 0 a)
		       n (- n 3)) 3)))
  (if (> n 0)
      (math-mul-bignum-digit a (if (= n 2) 100 10) 0)
    a))

(defun math-scale-right (a n)   ; [i i S]
  (if (= n 0)
      a
    (if (consp a)
	(cons (car a) (math-scale-right-bignum (cdr a) n))
      (if (<= a 0)
	  (if (= a 0)
	      0
	    (- (math-scale-right (- a) n)))
	(if (>= n 3)
	    (while (and (> (setq a (/ a 1000)) 0)
			(>= (setq n (- n 3)) 3))))
	(if (= n 2)
	    (/ a 100)
	  (if (= n 1)
	      (/ a 10)
	    a))))))

(defun math-scale-right-bignum (a n)   ; [L L S; l l S]
  (if (>= n 3)
      (setq a (nthcdr (/ n 3) a)
	    n (% n 3)))
  (if (> n 0)
      (cdr (math-mul-bignum-digit a (if (= n 2) 10 100) 0))
    a))

;;; Multiply (with rounding) the integer A by 10^N.   [I i S]
(defun math-scale-rounding (a n)
  (cond ((>= n 0)
	 (math-scale-left a n))
	((consp a)
	 (math-normalize
	  (cons (car a)
		(let ((val (if (< n -3)
			       (math-scale-right-bignum (cdr a) (- -3 n))
			     (if (= n -2)
				 (math-mul-bignum-digit (cdr a) 10 0)
			       (if (= n -1)
				   (math-mul-bignum-digit (cdr a) 100 0)
				 (cdr a))))))  ; n = -3
		  (if (and val (>= (car val) 500))
		      (if (cdr val)
			  (if (eq (car (cdr val)) 999)
			      (math-add-bignum (cdr val) '(1))
			    (cons (1+ (car (cdr val))) (cdr (cdr val))))
			'(1))
		    (cdr val))))))
	(t
	 (if (< a 0)
	     (- (math-scale-rounding (- a) n))
	   (if (= n -1)
	       (/ (+ a 5) 10)
	     (/ (+ (math-scale-right a (- -1 n)) 5) 10))))))


;;; Compute the sum of A and B.  [O O O] [Public]
(defun math-add (a b)
  (or
   (and (not (or (consp a) (consp b)))
	(progn
	  (setq a (+ a b))
	  (if (or (<= a -1000000) (>= a 1000000))
	      (math-bignum a)
	    a)))
   (and (Math-zerop a) (not (eq (car-safe a) 'mod))
	(if (and (math-floatp a) (Math-ratp b)) (math-float b) b))
   (and (Math-zerop b) (not (eq (car-safe b) 'mod))
	(if (and (math-floatp b) (Math-ratp a)) (math-float a) a))
   (and (Math-objvecp a) (Math-objvecp b)
	(or
	 (and (Math-integerp a) (Math-integerp b)
	      (progn
		(or (consp a) (setq a (math-bignum a)))
		(or (consp b) (setq b (math-bignum b)))
		(if (eq (car a) 'bigneg)
		    (if (eq (car b) 'bigneg)
			(cons 'bigneg (math-add-bignum (cdr a) (cdr b)))
		      (math-normalize
		       (let ((diff (math-sub-bignum (cdr b) (cdr a))))
			 (if (eq diff 'neg)
			     (cons 'bigneg (math-sub-bignum (cdr a) (cdr b)))
			   (cons 'bigpos diff)))))
		  (if (eq (car b) 'bigneg)
		      (math-normalize
		       (let ((diff (math-sub-bignum (cdr a) (cdr b))))
			 (if (eq diff 'neg)
			     (cons 'bigneg (math-sub-bignum (cdr b) (cdr a)))
			   (cons 'bigpos diff))))
		    (cons 'bigpos (math-add-bignum (cdr a) (cdr b)))))))
	 (and (Math-ratp a) (Math-ratp b)
	      (calc-extensions)
	      (calc-add-fractions a b))
	 (and (Math-realp a) (Math-realp b)
	      (progn
		(or (and (consp a) (eq (car a) 'float))
		    (setq a (math-float a)))
		(or (and (consp b) (eq (car b) 'float))
		    (setq b (math-float b)))
		(math-add-float a b)))
	 (and (calc-extensions)
	      (math-add-objects-fancy a b))))
   (and (calc-extensions)
	(math-add-symb-fancy a b))))

(defun math-add-bignum (a b)   ; [L L L; l l l]
  (if a
      (if b
	  (let* ((a (copy-sequence a)) (aa a) (carry nil) sum)
	    (while (and aa b)
	      (if carry
		  (if (< (setq sum (+ (car aa) (car b))) 999)
		      (progn
			(setcar aa (1+ sum))
			(setq carry nil))
		    (setcar aa (+ sum -999)))
		(if (< (setq sum (+ (car aa) (car b))) 1000)
		    (setcar aa sum)
		  (setcar aa (+ sum -1000))
		  (setq carry t)))
	      (setq aa (cdr aa)
		    b (cdr b)))
	    (if carry
		(if b
		    (nconc a (math-add-bignum b '(1)))
		  (while (eq (car aa) 999)
		    (setcar aa 0)
		    (setq aa (cdr aa)))
		  (if aa
		      (progn
			(setcar aa (1+ (car aa)))
			a)
		    (nconc a '(1))))
	      (if b
		  (nconc a b)
		a)))
	a)
    b))

(defun math-sub-bignum (a b)   ; [l l l]
  (if b
      (if a
	  (let* ((a (copy-sequence a)) (aa a) (borrow nil) sum)
	    (while (and aa b)
	      (if borrow
		  (if (>= (setq diff (- (car aa) (car b))) 1)
		      (progn
			(setcar aa (1- diff))
			(setq borrow nil))
		    (setcar aa (+ diff 999)))
		(if (>= (setq diff (- (car aa) (car b))) 0)
		    (setcar aa diff)
		  (setcar aa (+ diff 1000))
		  (setq borrow t)))
	      (setq aa (cdr aa)
		    b (cdr b)))
	    (if borrow
		(progn
		  (while (eq (car aa) 0)
		    (setcar aa 999)
		    (setq aa (cdr aa)))
		  (if aa
		      (progn
			(setcar aa (1- (car aa)))
			a)
		    'neg))
	      (while (eq (car b) 0)
		(setq b (cdr b)))
	      (if b
		  'neg
		a)))
	(while (eq (car b) 0)
	  (setq b (cdr b)))
	(and b
	     'neg))
    a))

(defun math-add-float (a b)   ; [F F F]
  (let ((ediff (- (nth 2 a) (nth 2 b))))
    (if (>= ediff 0)
	(if (>= ediff (+ calc-internal-prec calc-internal-prec))
	    a
	  (math-make-float (math-add (nth 1 b)
				     (if (eq ediff 0)
					 (nth 1 a)
				       (math-scale-left (nth 1 a) ediff)))
			   (nth 2 b)))
      (if (>= (setq ediff (- ediff))
	      (+ calc-internal-prec calc-internal-prec))
	  b
	(math-make-float (math-add (nth 1 a)
				   (math-scale-left (nth 1 b) ediff))
			 (nth 2 a))))))

;;; Compute the difference of A and B.  [O O O] [Public]
(defun math-sub (a b)
  (if (or (consp a) (consp b))
      (math-add a (math-neg b))
    (setq a (- a b))
    (if (or (<= a -1000000) (>= a 1000000))
	(math-bignum a)
      a)))

(defun math-sub-float (a b)   ; [F F F]
  (let ((ediff (- (nth 2 a) (nth 2 b))))
    (if (>= ediff 0)
	(if (>= ediff (+ calc-internal-prec calc-internal-prec))
	    a
	  (math-make-float (math-add (Math-integer-neg (nth 1 b))
				     (if (eq ediff 0)
					 (nth 1 a)
				       (math-scale-left (nth 1 a) ediff)))
			   (nth 2 b)))
      (if (>= (setq ediff (- ediff))
	      (+ calc-internal-prec calc-internal-prec))
	  b
	(math-make-float (math-add (nth 1 a)
				   (Math-integer-neg
				    (math-scale-left (nth 1 b) ediff)))
			 (nth 2 a))))))


;;; Compute the product of A and B.  [O O O] [Public]
(defun math-mul (a b)
  (or
   (and (not (consp a)) (not (consp b))
	(< a 1000) (> a -1000) (< b 1000) (> b -1000)
	(* a b))
   (and (Math-zerop a) (not (eq (car-safe b) 'mod))
	(if (Math-scalarp b)
	    (if (and (math-floatp b) (Math-ratp a)) (math-float a) a)
	  (calc-extensions)
	  (math-mul-zero a b)))
   (and (Math-zerop b) (not (eq (car-safe a) 'mod))
	(if (Math-scalarp a)
	    (if (and (math-floatp a) (Math-ratp b)) (math-float b) b)
	  (calc-extensions)
	  (math-mul-zero b a)))
   (and (Math-objvecp a) (Math-objvecp b)
	(or
	 (and (Math-integerp a) (Math-integerp b)
	      (progn
		(or (consp a) (setq a (math-bignum a)))
		(or (consp b) (setq b (math-bignum b)))
		(math-normalize
		 (cons (if (eq (car a) (car b)) 'bigpos 'bigneg)
		       (if (cdr (cdr a))
			   (if (cdr (cdr b))
			       (math-mul-bignum (cdr a) (cdr b))
			     (math-mul-bignum-digit (cdr a) (nth 1 b) 0))
			 (math-mul-bignum-digit (cdr b) (nth 1 a) 0))))))
	 (and (Math-ratp a) (Math-ratp b)
	      (calc-extensions)
	      (calc-mul-fractions a b))
	 (and (Math-realp a) (Math-realp b)
	      (progn
		(or (and (consp a) (eq (car a) 'float))
		    (setq a (math-float a)))
		(or (and (consp b) (eq (car b) 'float))
		    (setq b (math-float b)))
		(math-make-float (math-mul (nth 1 a) (nth 1 b))
				 (+ (nth 2 a) (nth 2 b)))))
	 (and (calc-extensions)
	      (math-mul-objects-fancy a b))))
   (and (calc-extensions)
	(math-mul-symb-fancy a b))))

(defun math-infinitep (a &optional undir)
  (while (and (consp a) (memq (car a) '(* / neg)))
    (if (or (not (eq (car a) '*)) (math-infinitep (nth 1 a)))
	(setq a (nth 1 a))
      (setq a (nth 2 a))))
  (and (consp a)
       (eq (car a) 'var)
       (memq (nth 2 a) '(var-inf var-uinf var-nan))
       (if (and undir (eq (nth 2 a) 'var-inf))
	   '(var uinf var-uinf)
	 a)))

;;; Multiply digit lists A and B.  [L L L; l l l]
(defun math-mul-bignum (a b)
  (and a b
       (let* ((sum (if (<= (car b) 1)
		       (if (= (car b) 0)
			   (list 0)
			 (copy-sequence a))
		     (math-mul-bignum-digit a (car b) 0)))
	      (sump sum) c d aa ss prod)
	 (while (setq b (cdr b))
	   (setq ss (setq sump (or (cdr sump) (setcdr sump (list 0))))
		 d (car b)
		 c 0
		 aa a)
	   (while (progn
		    (setcar ss (% (setq prod (+ (+ (car ss) (* (car aa) d))
						c)) 1000))
		    (setq aa (cdr aa)))
	     (setq c (/ prod 1000)
		   ss (or (cdr ss) (setcdr ss (list 0)))))
	   (if (>= prod 1000)
	       (if (cdr ss)
		   (setcar (cdr ss) (+ (/ prod 1000) (car (cdr ss))))
		 (setcdr ss (list (/ prod 1000))))))
	 sum)))

;;; Multiply digit list A by digit D.  [L L D D; l l D D]
(defun math-mul-bignum-digit (a d c)
  (if a
      (if (<= d 1)
	  (and (= d 1) a)
	(let* ((a (copy-sequence a)) (aa a) prod)
	  (while (progn
		   (setcar aa (% (setq prod (+ (* (car aa) d) c)) 1000))
		   (cdr aa))
	    (setq aa (cdr aa)
		  c (/ prod 1000)))
	  (if (>= prod 1000)
	      (setcdr aa (list (/ prod 1000))))
	  a))
    (and (> c 0)
	 (list c))))


;;; Compute the integer (quotient . remainder) of A and B, which may be
;;; small or big integers.  Type and consistency of truncation is undefined
;;; if A or B is negative.  B must be nonzero.  [I.I I I] [Public]
(defun math-idivmod (a b)
  (if (eq b 0)
      (math-reject-arg a "*Division by zero"))
  (if (or (consp a) (consp b))
      (if (and (natnump b) (< b 1000))
	  (let ((res (math-div-bignum-digit (cdr a) b)))
	    (cons
	     (math-normalize (cons (car a) (car res)))
	     (cdr res)))
	(or (consp a) (setq a (math-bignum a)))
	(or (consp b) (setq b (math-bignum b)))
	(let ((res (math-div-bignum (cdr a) (cdr b))))
	  (cons
	   (math-normalize (cons (if (eq (car a) (car b)) 'bigpos 'bigneg)
				 (car res)))
	   (math-normalize (cons (car a) (cdr res))))))
    (cons (/ a b) (% a b))))

(defun math-quotient (a b)   ; [I I I] [Public]
  (if (and (not (consp a)) (not (consp b)))
      (if (= b 0)
	  (math-reject-arg a "*Division by zero")
	(/ a b))
    (if (and (natnump b) (< b 1000))
	(if (= b 0)
	    (math-reject-arg a "*Division by zero")
	  (math-normalize (cons (car a)
				(car (math-div-bignum-digit (cdr a) b)))))
      (or (consp a) (setq a (math-bignum a)))
      (or (consp b) (setq b (math-bignum b)))
      (let* ((alen (1- (length a)))
	     (blen (1- (length b)))
	     (d (/ 1000 (1+ (nth (1- blen) (cdr b)))))
	     (res (math-div-bignum-big (math-mul-bignum-digit (cdr a) d 0)
				       (math-mul-bignum-digit (cdr b) d 0)
				       alen blen)))
	(math-normalize (cons (if (eq (car a) (car b)) 'bigpos 'bigneg)
			      (car res)))))))


;;; Divide a bignum digit list by another.  [l.l l L]
;;; The following division algorithm is borrowed from Knuth vol. II, sec. 4.3.1
(defun math-div-bignum (a b)
  (if (cdr b)
      (let* ((alen (length a))
	     (blen (length b))
	     (d (/ 1000 (1+ (nth (1- blen) b))))
	     (res (math-div-bignum-big (math-mul-bignum-digit a d 0)
				       (math-mul-bignum-digit b d 0)
				       alen blen)))
	(if (= d 1)
	    res
	  (cons (car res)
		(car (math-div-bignum-digit (cdr res) d)))))
    (let ((res (math-div-bignum-digit a (car b))))
      (cons (car res) (list (cdr res))))))

;;; Divide a bignum digit list by a digit.  [l.D l D]
(defun math-div-bignum-digit (a b)
  (if a
      (let* ((res (math-div-bignum-digit (cdr a) b))
	     (num (+ (* (cdr res) 1000) (car a))))
	(cons
	 (cons (/ num b) (car res))
	 (% num b)))
    '(nil . 0)))

(defun math-div-bignum-big (a b alen blen)   ; [l.l l L]
  (if (< alen blen)
      (cons nil a)
    (let* ((res (math-div-bignum-big (cdr a) b (1- alen) blen))
	   (num (cons (car a) (cdr res)))
	   (res2 (math-div-bignum-part num b blen)))
      (cons
       (cons (car res2) (car res))
       (cdr res2)))))

(defun math-div-bignum-part (a b blen)   ; a < b*1000  [D.l l L]
  (let* ((num (+ (* (or (nth blen a) 0) 1000) (or (nth (1- blen) a) 0)))
	 (den (nth (1- blen) b))
	 (guess (min (/ num den) 999)))
    (math-div-bignum-try a b (math-mul-bignum-digit b guess 0) guess)))

(defun math-div-bignum-try (a b c guess)   ; [D.l l l D]
  (let ((rem (math-sub-bignum a c)))
    (if (eq rem 'neg)
	(math-div-bignum-try a b (math-sub-bignum c b) (1- guess))
      (cons guess rem))))


;;; Compute the quotient of A and B.  [O O N] [Public]
(defun math-div (a b)
  (or
   (and (Math-zerop b)
	(calc-extensions)
	(math-div-by-zero a b))
   (and (Math-zerop a) (not (eq (car-safe b) 'mod))
	(if (Math-scalarp b)
	    (if (and (math-floatp b) (Math-ratp a)) (math-float a) a)
	  (calc-extensions)
	  (math-div-zero a b)))
   (and (Math-objvecp a) (Math-objvecp b)
	(or
	 (and (Math-integerp a) (Math-integerp b)
	      (let ((q (math-idivmod a b)))
		(if (eq (cdr q) 0)
		    (car q)
		  (if calc-prefer-frac
		      (progn
			(calc-extensions)
			(math-make-frac a b))
		    (math-div-float (math-make-float a 0)
				    (math-make-float b 0))))))
	 (and (Math-ratp a) (Math-ratp b)
	      (calc-extensions)
	      (calc-div-fractions a b))
	 (and (Math-realp a) (Math-realp b)
	      (progn
		(or (and (consp a) (eq (car a) 'float))
		    (setq a (math-float a)))
		(or (and (consp b) (eq (car b) 'float))
		    (setq b (math-float b)))
		(math-div-float a b)))
	 (and (calc-extensions)
	      (math-div-objects-fancy a b))))
   (and (calc-extensions)
	(math-div-symb-fancy a b))))

(defun math-div-float (a b)   ; [F F F]
  (let ((ldiff (max (- (1+ calc-internal-prec)
		       (- (math-numdigs (nth 1 a)) (math-numdigs (nth 1 b))))
		    0)))
    (math-make-float (math-quotient (math-scale-int (nth 1 a) ldiff) (nth 1 b))
		     (- (- (nth 2 a) (nth 2 b)) ldiff))))





;;; Format the number A as a string.  [X N; X Z] [Public]
(defun math-format-stack-value (entry)
  (setq calc-selection-cache-entry calc-selection-cache-default-entry)
  (let* ((a (car entry))
	 (math-comp-selected (nth 2 entry))
	 (c (cond ((null a) "<nil>")
		  ((eq calc-display-raw t) (format "%s" a))
		  ((stringp a) a)
		  ((eq a 'top-of-stack) ".")
		  (calc-prepared-composition
		   calc-prepared-composition)
		  ((and (Math-scalarp a)
			(memq calc-language '(nil flat unform))
			(null math-comp-selected))
		   (math-format-number a))
		  (t (calc-extensions)
		     (math-compose-expr a 0))))
	 (off (math-stack-value-offset c))
	 s w)
    (and math-comp-selected (setq calc-any-selections t))
    (setq w (cdr off)
	  off (car off))
    (if (> off 0)
	(setq c (math-comp-concat (make-string off ? ) c)))
    (or (equal calc-left-label "")
	(setq c (math-comp-concat (if (eq a 'top-of-stack)
				      (make-string (length calc-left-label) ? )
				    calc-left-label)
				  c)))
    (if calc-line-numbering
	(setq c (math-comp-concat (if (eq calc-language 'big)
				      (if math-comp-selected
					  '(tag t "1:  ") "1:  ")
				    "    ")
				  c)))
    (or (equal calc-right-label "")
	(eq a 'top-of-stack)
	(progn
	  (calc-extensions)
	  (setq c (list 'horiz c
			(make-string (max (- w (math-comp-width c)
					     (length calc-right-label)) 0) ? )
			'(break -1)
			calc-right-label))))
    (setq s (if (stringp c)
		(if calc-display-raw
		    (prin1-to-string c)
		  c)
	      (math-composition-to-string c w)))
    (if calc-language-output-filter
	(setq s (funcall calc-language-output-filter s)))
    (if (eq calc-language 'big)
	(setq s (concat s "\n"))
      (if calc-line-numbering
	  (progn
	    (aset s 0 ?1)
	    (aset s 1 ?:))))
    (setcar (cdr entry) (calc-count-lines s))
    s))

(defun math-stack-value-offset (c)
  (let* ((num (if calc-line-numbering 4 0))
	 (wid (calc-window-width))
	 off)
    (if calc-display-just
	(progn
	  (calc-extensions)
	  (math-stack-value-offset-fancy))
      (setq off (or calc-display-origin 0))
      (if (integerp calc-line-breaking)
	  (setq wid calc-line-breaking)))
    (cons (max (- off (length calc-left-label)) 0)
	  (+ wid num))))

(defun calc-count-lines (s)
  (let ((pos 0)
	(num 1))
    (while (setq newpos (string-match "\n" s pos))
      (setq pos (1+ newpos)
	    num (1+ num)))
    num))

(defun math-format-value (a &optional w)
  (if (and (Math-scalarp a)
	   (memq calc-language '(nil flat unform)))
      (math-format-number a)
    (calc-extensions)
    (let ((calc-line-breaking nil))
      (math-composition-to-string (math-compose-expr a 0) w))))

(defun calc-window-width ()
  (if calc-embedded-info
      (let ((win (get-buffer-window (aref calc-embedded-info 0))))
	(1- (if win (window-width win) (frame-width))))
    (- (window-width (get-buffer-window (current-buffer)))
       (if calc-line-numbering 5 1))))

(defun math-comp-concat (c1 c2)
  (if (and (stringp c1) (stringp c2))
      (concat c1 c2)
    (list 'horiz c1 c2)))



;;; Format an expression as a one-line string suitable for re-reading.

(defun math-format-flat-expr (a prec)
  (cond
   ((or (not (or (consp a) (integerp a)))
	(eq calc-display-raw t))
    (let ((print-escape-newlines t))
      (concat "'" (prin1-to-string a))))
   ((Math-scalarp a)
    (let ((calc-group-digits nil)
	  (calc-point-char ".")
	  (calc-frac-format (if (> (length (car calc-frac-format)) 1)
				'("::" nil) '(":" nil)))
	  (calc-complex-format nil)
	  (calc-hms-format "%s@ %s' %s\"")
	  (calc-language nil))
      (math-format-number a)))
   (t
    (calc-extensions)
    (math-format-flat-expr-fancy a prec))))



;;; Format a number as a string.
(defun math-format-number (a &optional prec)   ; [X N]   [Public]
  (cond
   ((eq calc-display-raw t) (format "%s" a))
   ((and (nth 1 calc-frac-format) (Math-integerp a))
    (calc-extensions)
    (math-format-number (math-adjust-fraction a)))
   ((integerp a)
    (if (not (or calc-group-digits calc-leading-zeros))
	(if (= calc-number-radix 10)
	    (int-to-string a)
	  (if (< a 0)
	      (concat "-" (math-format-number (- a)))
	    (calc-extensions)
	    (if math-radix-explicit-format
		(if calc-radix-formatter
		    (funcall calc-radix-formatter
			     calc-number-radix
			     (if (= calc-number-radix 2)
				 (math-format-binary a)
			       (math-format-radix a)))
		  (format "%d#%s" calc-number-radix
			  (if (= calc-number-radix 2)
			      (math-format-binary a)
			    (math-format-radix a))))
	      (math-format-radix a))))
      (math-format-number (math-bignum a))))
   ((stringp a) a)
   ((not (consp a)) (prin1-to-string a))
   ((eq (car a) 'bigpos) (math-format-bignum (cdr a)))
   ((eq (car a) 'bigneg) (concat "-" (math-format-bignum (cdr a))))
   ((and (eq (car a) 'float) (= calc-number-radix 10))
    (if (Math-integer-negp (nth 1 a))
	(concat "-" (math-format-number (math-neg a)))
      (let ((mant (nth 1 a))
	    (exp (nth 2 a))
	    (fmt (car calc-float-format))
	    (figs (nth 1 calc-float-format))
	    (point calc-point-char)
	    str)
	(if (and (eq fmt 'fix)
		 (or (and (< figs 0) (setq figs (- figs)))
		     (> (+ exp (math-numdigs mant)) (- figs))))
	    (progn
	      (setq mant (math-scale-rounding mant (+ exp figs))
		    str (if (integerp mant)
			    (int-to-string mant)
			  (math-format-bignum-decimal (cdr mant))))
	      (if (<= (length str) figs)
		  (setq str (concat (make-string (1+ (- figs (length str))) ?0)
				    str)))
	      (if (> figs 0)
		  (setq str (concat (substring str 0 (- figs)) point
				    (substring str (- figs))))
		(setq str (concat str point)))
	      (when calc-group-digits
		(require 'calc-ext)
		(setq str (math-group-float str))))
	  (if (< figs 0)
	      (setq figs (+ calc-internal-prec figs)))
	  (if (> figs 0)
	      (let ((adj (- figs (math-numdigs mant))))
		(if (< adj 0)
		    (setq mant (math-scale-rounding mant adj)
			  exp (- exp adj)))))
	  (setq str (if (integerp mant)
			(int-to-string mant)
		      (math-format-bignum-decimal (cdr mant))))
	  (let* ((len (length str))
		 (dpos (+ exp len)))
	    (if (and (eq fmt 'float)
		     (<= dpos (+ calc-internal-prec calc-display-sci-high))
		     (>= dpos (+ calc-display-sci-low 2)))
		(progn
		  (cond
		   ((= dpos 0)
		    (setq str (concat "0" point str)))
		   ((and (<= exp 0) (> dpos 0))
		    (setq str (concat (substring str 0 dpos) point
				      (substring str dpos))))
		   ((> exp 0)
		    (setq str (concat str (make-string exp ?0) point)))
		   (t   ; (< dpos 0)
		    (setq str (concat "0" point
				      (make-string (- dpos) ?0) str))))
		  (when calc-group-digits
		    (require 'calc-ext)
		    (setq str (math-group-float str))))
	      (let* ((eadj (+ exp len))
		     (scale (if (eq fmt 'eng)
				(1+ (math-mod (+ eadj 300002) 3))
			      1)))
		(if (> scale (length str))
		    (setq str (concat str (make-string (- scale (length str))
						       ?0))))
		(if (< scale (length str))
		    (setq str (concat (substring str 0 scale) point
				      (substring str scale))))
		(when calc-group-digits
		  (require 'calc-ext)
		  (setq str (math-group-float str)))
		(setq str (format (if (memq calc-language '(math maple))
				      (if (and prec (> prec 191))
					  "(%s*10.^%d)" "%s*10.^%d")
				    "%se%d")
				  str (- eadj scale)))))))
	str)))
   (t
    (calc-extensions)
    (math-format-number-fancy a prec))))

(defun math-format-bignum (a)   ; [X L]
  (if (and (= calc-number-radix 10)
	   (not calc-leading-zeros)
	   (not calc-group-digits))
      (math-format-bignum-decimal a)
    (calc-extensions)
    (math-format-bignum-fancy a)))

(defun math-format-bignum-decimal (a)   ; [X L]
  (if a
      (let ((s ""))
	(while (cdr (cdr a))
	  (setq s (concat (format "%06d" (+ (* (nth 1 a) 1000) (car a))) s)
		a (cdr (cdr a))))
	(concat (int-to-string (+ (* (or (nth 1 a) 0) 1000) (car a))) s))
    "0"))



;;; Parse a simple number in string form.   [N X] [Public]
(defun math-read-number (s)
  (math-normalize
   (cond

    ;; Integers (most common case)
    ((string-match "\\` *\\([0-9]+\\) *\\'" s)
     (let ((digs (math-match-substring s 1)))
       (if (and (eq calc-language 'c)
		(> (length digs) 1)
		(eq (aref digs 0) ?0))
	   (math-read-number (concat "8#" digs))
	 (if (<= (length digs) 6)
	     (string-to-int digs)
	   (cons 'bigpos (math-read-bignum digs))))))

    ;; Clean up the string if necessary
    ((string-match "\\`\\(.*\\)[ \t\n]+\\([^\001]*\\)\\'" s)
     (math-read-number (concat (math-match-substring s 1)
			       (math-match-substring s 2))))

    ;; Plus and minus signs
    ((string-match "^[-_+]\\(.*\\)$" s)
     (let ((val (math-read-number (math-match-substring s 1))))
       (and val (if (eq (aref s 0) ?+) val (math-neg val)))))

    ;; Forms that require extensions module
    ((string-match "[^-+0-9eE.]" s)
     (calc-extensions)
     (math-read-number-fancy s))

    ;; Decimal point
    ((string-match "^\\([0-9]*\\)\\.\\([0-9]*\\)$" s)
     (let ((int (math-match-substring s 1))
	   (frac (math-match-substring s 2)))
       (let ((ilen (length int))
	     (flen (length frac)))
	 (let ((int (if (> ilen 0) (math-read-number int) 0))
	       (frac (if (> flen 0) (math-read-number frac) 0)))
	   (and int frac (or (> ilen 0) (> flen 0))
		(list 'float
		      (math-add (math-scale-int int flen) frac)
		      (- flen)))))))

    ;; "e" notation
    ((string-match "^\\(.*\\)[eE]\\([-+]?[0-9]+\\)$" s)
     (let ((mant (math-match-substring s 1))
	   (exp (math-match-substring s 2)))
       (let ((mant (if (> (length mant) 0) (math-read-number mant) 1))
	     (exp (if (<= (length exp) (if (memq (aref exp 0) '(?+ ?-)) 8 7))
		      (string-to-int exp))))
	 (and mant exp (Math-realp mant) (> exp -4000000) (< exp 4000000)
	      (let ((mant (math-float mant)))
		(list 'float (nth 1 mant) (+ (nth 2 mant) exp)))))))

    ;; Syntax error!
    (t nil))))

(defun math-match-substring (s n)
  (if (match-beginning n)
      (substring s (match-beginning n) (match-end n))
    ""))

(defun math-read-bignum (s)   ; [l X]
  (if (> (length s) 3)
      (cons (string-to-int (substring s -3))
	    (math-read-bignum (substring s 0 -3)))
    (list (string-to-int s))))


(defconst math-tex-ignore-words
  '( ("\\hbox") ("\\mbox") ("\\text") ("\\left") ("\\right")
     ("\\,") ("\\>") ("\\:") ("\\;") ("\\!") ("\\ ")
     ("\\quad") ("\\qquad") ("\\hfil") ("\\hfill")
     ("\\displaystyle") ("\\textstyle") ("\\dsize") ("\\tsize")
     ("\\scriptstyle") ("\\scriptscriptstyle") ("\\ssize") ("\\sssize")
     ("\\rm") ("\\bf") ("\\it") ("\\sl")
     ("\\roman") ("\\bold") ("\\italic") ("\\slanted")
     ("\\cal") ("\\mit") ("\\Cal") ("\\Bbb") ("\\frak") ("\\goth")
     ("\\evalto")
     ("\\matrix" mat) ("\\bmatrix" mat) ("\\pmatrix" mat)
     ("\\cr" punc ";") ("\\\\" punc ";") ("\\*" punc "*")
     ("\\{" punc "[") ("\\}" punc "]")
))

(defconst math-eqn-ignore-words
  '( ("roman") ("bold") ("italic") ("mark") ("lineup") ("evalto")
     ("left" ("floor") ("ceil"))
     ("right" ("floor") ("ceil"))
     ("arc" ("sin") ("cos") ("tan") ("sinh") ("cosh") ("tanh"))
     ("size" n) ("font" n) ("fwd" n) ("back" n) ("up" n) ("down" n)
     ("above" punc ",")
))

(defconst math-standard-opers
  '( ( "_"     calcFunc-subscr 1200 1201 )
     ( "%"     calcFunc-percent 1100 -1 )
     ( "u+"    ident	     -1 1000 )
     ( "u-"    neg	     -1 1000 197 )
     ( "u!"    calcFunc-lnot -1 1000 )
     ( "mod"   mod	     400 400 185 )
     ( "+/-"   sdev	     300 300 185 )
     ( "!!"    calcFunc-dfact 210 -1 )
     ( "!"     calcFunc-fact 210  -1 )
     ( "^"     ^             201 200 )
     ( "**"    ^             201 200 )
     ( "*"     *             196 195 )
     ( "2x"    *             196 195 )
     ( "/"     /             190 191 )
     ( "%"     %             190 191 )
     ( "\\"    calcFunc-idiv 190 191 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "|"     |	     170 171 )
     ( "<"     calcFunc-lt   160 161 )
     ( ">"     calcFunc-gt   160 161 )
     ( "<="    calcFunc-leq  160 161 )
     ( ">="    calcFunc-geq  160 161 )
     ( "="     calcFunc-eq   160 161 )
     ( "=="    calcFunc-eq   160 161 )
     ( "!="    calcFunc-neq  160 161 )
     ( "&&"    calcFunc-land 110 111 )
     ( "||"    calcFunc-lor  100 101 )
     ( "?"     (math-read-if) 91  90 )
     ( "!!!"   calcFunc-pnot  -1  85 )
     ( "&&&"   calcFunc-pand  80  81 )
     ( "|||"   calcFunc-por   75  76 )
     ( ":="    calcFunc-assign 51 50 )
     ( "::"    calcFunc-condition 45 46 )
     ( "=>"    calcFunc-evalto 40 41 )
     ( "=>"    calcFunc-evalto 40 -1 )))
(defvar math-expr-opers math-standard-opers)

;;;###autoload
(defun calc-grab-region (top bot arg)
  "Parse the region as a vector of numbers and push it on the Calculator stack."
  (interactive "r\nP")
  (calc-extensions)
  (calc-do-grab-region top bot arg))

;;;###autoload
(defun calc-grab-rectangle (top bot arg)
  "Parse a rectangle as a matrix of numbers and push it on the Calculator stack."
  (interactive "r\nP")
  (calc-extensions)
  (calc-do-grab-rectangle top bot arg))

(defun calc-grab-sum-down (top bot arg)
  "Parse a rectangle as a matrix of numbers and sum its columns."
  (interactive "r\nP")
  (calc-extensions)
  (calc-do-grab-rectangle top bot arg 'calcFunc-reduced))

(defun calc-grab-sum-across (top bot arg)
  "Parse a rectangle as a matrix of numbers and sum its rows."
  (interactive "r\nP")
  (calc-extensions)
  (calc-do-grab-rectangle top bot arg 'calcFunc-reducea))


;;;###autoload
(defun calc-embedded (arg &optional end obeg oend)
  "Start Calc Embedded mode on the formula surrounding point."
  (interactive "P")
  (calc-extensions)
  (calc-do-embedded arg end obeg oend))

;;;###autoload
(defun calc-embedded-activate (&optional arg cbuf)
  "Scan the current editing buffer for all embedded := and => formulas.
Also looks for the equivalent TeX words, \\gets and \\evalto."
  (interactive "P")
  (calc-do-embedded-activate arg cbuf))


(defun calc-user-invocation ()
  (interactive)
  (or (stringp calc-invocation-macro)
      (error "Use `Z I' inside Calc to define a `M-# Z' keyboard macro"))
  (execute-kbd-macro calc-invocation-macro nil))




;;; User-programmability.

;;;###autoload
(defmacro defmath (func args &rest body)   ;  [Public]
  (calc-extensions)
  (math-do-defmath func args body))


;;; Functions needed for Lucid Emacs support.

(defun calc-read-key (&optional optkey)
  (cond (calc-emacs-type-lucid
	 (let ((event (next-command-event)))
	   (let ((key (event-to-character event t t)))
	     (or key optkey (error "Expected a plain keystroke"))
	     (cons key event))))
	(calc-emacs-type-gnu19
	 (let ((key (read-event)))
	   (cons key key)))
	(t
	 (let ((key (read-char)))
	   (cons key key)))))

(defun calc-unread-command (&optional input)
  (if (featurep 'xemacs)
      (setq unread-command-event
	    (if (integerp input) (character-to-event input)
	      (or input last-command-event)))
    (push (or input last-command-event) unread-command-events)))

(defun calc-clear-unread-commands ()
  (if (featurep 'xemacs) 
	(calc-emacs-type-lucid (setq unread-command-event nil))
    (setq unread-command-events nil)))

(if calc-always-load-extensions
    (progn
      (calc-extensions)
      (calc-load-everything)))


(run-hooks 'calc-load-hook)

;;; calc.el ends here
