;;; cperl-mode.el --- Perl code editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 91, 92, 93, 94, 95, 96, 97, 98, 99,
;;               2000, 2003, 2005, 2006
;;     Free Software Foundation, Inc.

;; Author: Ilya Zakharevich and Bob Olson
;; Maintainer: Ilya Zakharevich <ilyaz@cpan.org>
;; Keywords: languages, Perl

;; This file is part of GNU Emacs.

;;; This code started from the following message of long time ago
;;; (IZ), but Bob does not maintain this mode any more:

;;; From: olson@mcs.anl.gov (Bob Olson)
;;; Newsgroups: comp.lang.perl
;;; Subject: cperl-mode: Another perl mode for Gnuemacs
;;; Date: 14 Aug 91 15:20:01 GMT

;; Copyright (C) Ilya Zakharevich and Bob Olson

;; This file may be distributed
;; either under the same terms as GNU Emacs, or under the same terms
;; as Perl. You should have received a copy of Perl Artistic license
;; along with the Perl distribution.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Corrections made by Ilya Zakharevich ilyaz@cpan.org
;;; XEmacs changes by Peter Arius arius@informatik.uni-erlangen.de

;;; Commentary:

;; $Id: cperl-mode.el,v 5.23 2007/02/15 11:34:23 vera Exp vera $

;;; If your Emacs does not default to `cperl-mode' on Perl files:
;;; To use this mode put the following into
;;; your .emacs file:

;; (autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)

;; You can either fine-tune the bells and whistles of this mode or
;; bulk enable them by putting

;; (setq cperl-hairy t)

;; in your .emacs file.  (Emacs rulers do not consider it politically
;; correct to make whistles enabled by default.)

;; DO NOT FORGET to read micro-docs (available from `Perl' menu)   <<<<<<
;; or as help on variables `cperl-tips', `cperl-problems',         <<<<<<
;; `cperl-non-problems', `cperl-praise', `cperl-speed'.            <<<<<<

;; Additional useful commands to put into your .emacs file (before
;; RMS Emacs 20.3):

;; (setq auto-mode-alist
;;      (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))
;; (setq interpreter-mode-alist (append interpreter-mode-alist
;; 				        '(("miniperl" . perl-mode))))

;; The mode information (on C-h m) provides some customization help.
;; If you use font-lock feature of this mode, it is advisable to use
;; either lazy-lock-mode or fast-lock-mode.  I prefer lazy-lock.

;; Faces used now: three faces for first-class and second-class keywords
;; and control flow words, one for each: comments, string, labels,
;; functions definitions and packages, arrays, hashes, and variable
;; definitions.  If you do not see all these faces, your font-lock does
;; not define them, so you need to define them manually.
;; Maybe you have an obsolete font-lock from 19.28 or earlier.  Upgrade.

;; If you have a grayscale monitor, and do not have the variable
;; font-lock-display-type bound to 'grayscale, insert

;; (setq font-lock-display-type 'grayscale)

;; into your .emacs file (this is relevant before RMS Emacs 20).

;; This mode supports font-lock, imenu and mode-compile.  In the
;; hairy version font-lock is on, but you should activate imenu
;; yourself (note that mode-compile is not standard yet).  Well, you
;; can use imenu from keyboard anyway (M-x imenu), but it is better
;; to bind it like that:

;; (define-key global-map [M-S-down-mouse-3] 'imenu)

;;; Font lock bugs as of v4.32:

;; The following kinds of Perl code erroneously start strings:
;; \$`  \$'  \$"
;; $opt::s  $opt_s  $opt{s}  (s => ...)  /\s+.../
;; likewise with m, tr, y, q, qX instead of s

;;; In fact the version of font-lock that this version supports can be
;;; much newer than the version you actually have. This means that a
;;; lot of faces can be set up, but are not visible on your screen
;;; since the coloring rules for this faces are not defined.

;;; Updates: ========================================

;;; Made less hairy by default: parentheses not electric,
;;; linefeed not magic. Bug with abbrev-mode corrected.

;;;; After 1.4:
;;;  Better indentation:
;;;  subs inside braces should work now,
;;;  Toplevel braces obey customization.
;;;  indent-for-comment knows about bad cases, cperl-indent-for-comment
;;;  moves cursor to a correct place.
;;;  cperl-indent-exp written from the scratch! Slow... (quadratic!) :-(
;;;        (50 secs on DB::DB (sub of 430 lines), 486/66)
;;;  Minor documentation fixes.
;;;  Imenu understands packages as prefixes (including nested).
;;;  Hairy options can be switched off one-by-one by setting to null.
;;;  Names of functions and variables changed to conform to `cperl-' style.

;;;; After 1.5:
;;;  Some bugs with indentation of labels (and embedded subs) corrected.
;;;  `cperl-indent-region' done (slow :-()).
;;;  `cperl-fill-paragraph' done.
;;;  Better package support for `imenu'.
;;;  Progress indicator for indentation (with `imenu' loaded).
;;;  `Cperl-set' was busted, now setting the individual hairy option
;;;     should be better.

;;;; After 1.6:
;;; `cperl-set-style' done.
;;; `cperl-check-syntax' done.
;;; Menu done.
;;; New config variables `cperl-close-paren-offset' and `cperl-comment-column'.
;;; Bugs with `cperl-auto-newline' corrected.
;;; `cperl-electric-lbrace' can work with `cperl-auto-newline' in situation
;;; like $hash{.

;;;; 1.7 XEmacs (arius@informatik.uni-erlangen.de):
;;; - use `next-command-event', if `next-command-events' does not exist
;;; - use `find-face' as def. of `is-face'
;;; - corrected def. of `x-color-defined-p'
;;; - added const defs for font-lock-comment-face,
;;;   font-lock-keyword-face and font-lock-function-name-face
;;; - added def. of font-lock-variable-name-face
;;; - added (require 'easymenu) inside an `eval-when-compile'
;;; - replaced 4-argument `substitute-key-definition' with ordinary
;;;   `define-key's
;;; - replaced `mark-active' in menu definition by `cperl-use-region-p'.
;;; Todo (at least):
;;; - use emacs-vers.el (http://www.cs.utah.edu/~eeide/emacs/emacs-vers.el.gz)
;;;   for portable code?
;;; - should `cperl-mode' do a
;;;	(if (featurep 'easymenu) (easy-menu-add cperl-menu))
;;;   or should this be left to the user's `cperl-mode-hook'?

;;; Some bugs introduced by the above fix corrected (IZ ;-).
;;; Some bugs under XEmacs introduced by the correction corrected.

;;; Some more can remain since there are two many different variants.
;;; Please feedback!

;;; We do not support fontification of arrays and hashes under
;;; obsolete font-lock any more. Upgrade.

;;;; after 1.8 Minor bug with parentheses.
;;;; after 1.9 Improvements from Joe Marzot.
;;;; after 1.10
;;;  Does not need easymenu to compile under XEmacs.
;;;  `vc-insert-headers' should work better.
;;;  Should work with 19.29 and 19.12.
;;;  Small improvements to fontification.
;;;  Expansion of keywords does not depend on C-? being backspace.

;;; after 1.10+
;;; 19.29 and 19.12 supported.
;;; `cperl-font-lock-enhanced' deprecated. Use font-lock-extra.el.
;;; Support for font-lock-extra.el.

;;;; After 1.11:
;;; Tools submenu.
;;; Support for perl5-info.
;;; `imenu-go-find-at-position' in Tools requires imenu-go.el (see hints above)
;;; Imenu entries do not work with stock imenu.el. Patch sent to maintainers.
;;; Fontifies `require a if b;', __DATA__.
;;; Arglist for auto-fill-mode was incorrect.

;;;; After 1.12:
;;; `cperl-lineup-step' and `cperl-lineup' added: lineup constructions
;;; vertically.
;;; `cperl-do-auto-fill' updated for 19.29 style.
;;; `cperl-info-on-command' now has a default.
;;; Workaround for broken C-h on XEmacs.
;;; VC strings escaped.
;;; C-h f now may prompt for function name instead of going on,
;;; controlled by `cperl-info-on-command-no-prompt'.

;;;; After 1.13:
;;; Msb buffer list includes perl files
;;; Indent-for-comment uses indent-to
;;; Can write tag files using etags.

;;;; After 1.14:
;;; Recognizes (tries to ;-) {...} which are not blocks during indentation.
;;; `cperl-close-paren-offset' affects ?\] too (and ?\} if not block)
;;; Bug with auto-filling comments started with "##" corrected.

;;;; Very slow now: on DB::DB 0.91, 486/66:

;;;Function Name                             Call Count  Elapsed Time  Average Time
;;;========================================  ==========  ============  ============
;;;cperl-block-p                             469         3.7799999999  0.0080597014
;;;cperl-get-state                           505         163.39000000  0.3235445544
;;;cperl-comment-indent                      12          0.0299999999  0.0024999999
;;;cperl-backward-to-noncomment              939         4.4599999999  0.0047497337
;;;cperl-calculate-indent                    505         172.22000000  0.3410297029
;;;cperl-indent-line                         505         172.88000000  0.3423366336
;;;cperl-use-region-p                        40          0.0299999999  0.0007499999
;;;cperl-indent-exp                          1           177.97000000  177.97000000
;;;cperl-to-comment-or-eol                   1453        3.9800000000  0.0027391603
;;;cperl-backward-to-start-of-continued-exp  9           0.0300000000  0.0033333333
;;;cperl-indent-region                       1           177.94000000  177.94000000

;;;; After 1.15:
;;; Takes into account white space after opening parentheses during indent.
;;; May highlight pods and here-documents: see `cperl-pod-here-scan',
;;; `cperl-pod-here-fontify', `cperl-pod-face'. Does not use this info
;;; for indentation so far.
;;; Fontification updated to 19.30 style.
;;; The change 19.29->30 did not add all the required functionality,
;;;     but broke "font-lock-extra.el". Get "choose-color.el" from
;;;       http://ilyaz.org/software/emacs

;;;; After 1.16:
;;;       else # comment
;;;    recognized as a start of a block.
;;;  Two different font-lock-levels provided.
;;;  `cperl-pod-head-face' introduced. Used for highlighting.
;;;  `imenu' marks pods, +Packages moved to the head.

;;;; After 1.17:
;;;  Scan for pods highlights here-docs too.
;;;  Note that the tag of here-doc may be rehighlighted later by lazy-lock.
;;;  Only one here-doc-tag per line is supported, and one in comment
;;;  or a string may break fontification.
;;;  POD headers were supposed to fill one line only.

;;;; After 1.18:
;;;  `font-lock-keywords' were set in 19.30 style _always_. Current scheme
;;;    may  break under XEmacs.
;;;  `cperl-calculate-indent' dis suppose that `parse-start' was defined.
;;;  `fontified' tag is added to fontified text as well as `lazy-lock' (for
;;;    compatibility with older lazy-lock.el) (older one overfontifies
;;;    something nevertheless :-().
;;;  Will not indent something inside pod and here-documents.
;;;  Fontifies the package name after import/no/bootstrap.
;;;  Added new entry to menu with meta-info about the mode.

;;;; After 1.19:
;;;  Prefontification works much better with 19.29. Should be checked
;;;   with 19.30 as well.
;;;  Some misprints in docs corrected.
;;;  Now $a{-text} and -text => "blah" are fontified as strings too.
;;;  Now the pod search is much stricter, so it can help you to find
;;;    pod sections which are broken because of whitespace before =blah
;;;    - just observe the fontification.

;;;; After 1.20
;;;  Anonymous subs are indented with respect to the level of
;;;    indentation of `sub' now.
;;;  {} is recognized as hash after `bless' and `return'.
;;;  Anonymous subs are split by `cperl-linefeed' as well.
;;;  Electric parens embrace a region if present.
;;;  To make `cperl-auto-newline' useful,
;;;    `cperl-auto-newline-after-colon' is introduced.
;;;  `cperl-electric-parens' is now t or nul. The old meaning is moved to
;;;  `cperl-electric-parens-string'.
;;;  `cperl-toggle-auto-newline' introduced, put on C-c C-a.
;;;  `cperl-toggle-abbrev' introduced, put on C-c C-k.
;;;  `cperl-toggle-electric' introduced, put on C-c C-e.
;;;  Beginning-of-defun-regexp was not anchored.

;;;; After 1.21
;;;  Auto-newline grants `cperl-extra-newline-before-brace' if "{" is typed
;;;    after ")".
;;;  {} is recognized as expression after `tr' and friends.

;;;; After 1.22
;;;  Entry Hierarchy added to imenu. Very primitive so far.
;;;  One needs newer `imenu-go'.el. A patch to `imenu' is needed as well.
;;;  Writes its own TAGS files.
;;;  Class viewer based on TAGS files. Does not trace @ISA so far.
;;;  19.31: Problems with scan for PODs corrected.
;;;  First POD header correctly fontified.
;;;  I needed (setq imenu-use-keymap-menu t) to get good imenu in 19.31.
;;;  Apparently it makes a lot of hierarchy code obsolete...

;;;; After 1.23
;;;  Tags filler now scans *.xs as well.
;;;  The info from *.xs scan is used by the hierarchy viewer.
;;;  Hierarchy viewer documented.
;;;  Bug in 19.31 imenu documented.

;;;; After 1.24
;;;  New location for info-files mentioned,
;;;  Electric-; should work better.
;;;  Minor bugs with POD marking.

;;;; After 1.25 (probably not...)
;;;  `cperl-info-page' introduced.
;;;  To make `uncomment-region' working, `comment-region' would
;;;  not insert extra space.
;;;  Here documents delimiters better recognized
;;;  (empty one, and non-alphanums in quotes handled). May be wrong with 1<<14?
;;;  `cperl-db' added, used in menu.
;;;  imenu scan removes text-properties, for better debugging
;;;    - but the bug is in 19.31 imenu.
;;;  formats highlighted by font-lock and prescan, embedded comments
;;;  are not treated.
;;;  POD/friends scan merged in one pass.
;;;  Syntax class is not used for analyzing the code, only char-syntax
;;;  may be checked against _ or'ed with w.
;;;  Syntax class of `:' changed to be _.
;;;  `cperl-find-bad-style' added.

;;;; After 1.25
;;;  When search for here-documents, we ignore commented << in simplest cases.
;;;  `cperl-get-help' added, available on C-h v and from menu.
;;;  Auto-help added. Default with `cperl-hairy', switchable on/off
;;;   with startup variable `cperl-lazy-help-time' and from
;;;   menu. Requires `run-with-idle-timer'.
;;;  Highlighting of @abc{@efg} was wrong - interchanged two regexps.

;;;; After 1.27
;;;  Indentation: At toplevel after a label - fixed.
;;;  1.27 was put to archives in binary mode ===> DOSish :-(

;;;; After 1.28
;;;  Thanks to Martin Buchholz <mrb@Eng.Sun.COM>: misprints in
;;;  comments and docstrings corrected, XEmacs support cleaned up.
;;;  The closing parenths would enclose the region into matching
;;;  parens under the same conditions as the opening ones.
;;;  Minor updates to `cperl-short-docs'.
;;;  Will not consider <<= as start of here-doc.

;;;; After 1.29
;;;  Added an extra advice to look into Micro-docs. ;-).
;;;  Enclosing of region when you press a closing parenth is regulated by
;;;  `cperl-electric-parens-string'.
;;;  Minor updates to `cperl-short-docs'.
;;;  `initialize-new-tags-table' called only if present (Does this help
;;;     with generation of tags under XEmacs?).
;;;  When creating/updating tag files, new info is written at the old place,
;;;     or at the end (is this a wanted behaviour? I need this in perl build directory).

;;;; After 1.30
;;;  All the keywords from keywords.pl included (maybe with dummy explanation).
;;;  No auto-help inside strings, comment, here-docs, formats, and pods.
;;;  Shrinkwrapping of info, regulated by `cperl-max-help-size',
;;;  `cperl-shrink-wrap-info-frame'.
;;;  Info on variables as well.
;;;  Recognision of HERE-DOCS improved yet more.
;;;  Autonewline works on `}' without warnings.
;;;  Autohelp works again on $_[0].

;;;; After 1.31
;;;  perl-descr.el found its author - hi, Johan!
;;;  Some support for correct indent after here-docs and friends (may
;;;  be superseeded by eminent change to Emacs internals).
;;;  Should work with older Emaxen as well ( `-style stuff removed).

;;;; After 1.32

;;;  Started to add support for `syntax-table' property (should work
;;;  with patched Emaxen), controlled by
;;;  `cperl-use-syntax-table-text-property'. Currently recognized:
;;;    All quote-like operators: m, s, y, tr, qq, qw, qx, q,
;;;    // in most frequent context:
;;;          after block or
;;;                    ~ { ( = | & + - * ! , ;
;;;          or
;;;                    while if unless until and or not xor split grep map
;;;    Here-documents, formats, PODs,
;;;    ${...}
;;;    'abc$'
;;;    sub a ($); sub a ($) {}
;;;  (provide 'cperl-mode) was missing!
;;;  `cperl-after-expr-p' is now much smarter after `}'.
;;;  `cperl-praise' added to mini-docs.
;;;  Utilities try to support subs-with-prototypes.

;;;; After 1.32.1
;;;  `cperl-after-expr-p' is now much smarter after "() {}" and "word {}":
;;;     if word is "else, map, grep".
;;;  Updated for new values of syntax-table constants.
;;;  Uses `help-char' (at last!) (disabled, does not work?!)
;;;  A couple of regexps where missing _ in character classes.
;;;  -s could be considered as start of regexp, 1../blah/ was not,
;;;  as was not /blah/ at start of file.

;;;; After 1.32.2
;;;  "\C-hv" was wrongly "\C-hf"
;;;  C-hv was not working on `[index()]' because of [] in skip-chars-*.
;;;  `__PACKAGE__' supported.
;;;  Thanks for Greg Badros: `cperl-lazy-unstall' is more complete,
;;;  `cperl-get-help' is made compatible with `query-replace'.

;;;; As of Apr 15, development version of 19.34 supports
;;;; `syntax-table' text properties. Try setting
;;;; `cperl-use-syntax-table-text-property'.

;;;; After 1.32.3
;;;  We scan for s{}[] as well (in simplest situations).
;;;  We scan for $blah'foo as well.
;;;  The default is to use `syntax-table' text property if Emacs is good enough.
;;;  `cperl-lineup' is put on C-M-| (=C-M-S-\\).
;;;  Start of `cperl-beautify-regexp'.

;;;; After 1.32.4
;;; `cperl-tags-hier-init' did not work in text-mode.
;;; `cperl-noscan-files-regexp' had a misprint.
;;; Generation of Class Hierarchy was broken due to a bug in `x-popup-menu'
;;;  in 19.34.

;;;; After 1.33:
;;; my,local highlight vars after {} too.
;;; TAGS could not be created before imenu was loaded.
;;; `cperl-indent-left-aligned-comments' created.
;;; Logic of `cperl-indent-exp' changed a little bit, should be more
;;;  robust w.r.t. multiline strings.
;;; Recognition of blah'foo takes into account strings.
;;; Added '.al' to the list of Perl extensions.
;;; Class hierarchy is "mostly" sorted (need to rethink algorthm
;;;  of pruning one-root-branch subtrees to get yet better sorting.)
;;; Regeneration of TAGS was busted.
;;; Can use `syntax-table' property when generating TAGS
;;;  (governed by  `cperl-use-syntax-table-text-property-for-tags').

;;;; After 1.35:
;;; Can process several =pod/=cut sections one after another.
;;; Knows of `extproc' when under `emx', indents with `__END__' and `__DATA__'.
;;; `cperl-under-as-char' implemented (XEmacs people like broken behaviour).
;;; Beautifier for regexps fixed.
;;; `cperl-beautify-level', `cperl-contract-level' coded
;;;
;;;; Emacs's 20.2 problems:
;;; `imenu.el' has bugs, `imenu-add-to-menubar' does not work.
;;; Couple of others problems with 20.2 were reported, my ability to check/fix
;;; them is very reduced now.

;;;; After 1.36:
;;;  'C-M-|' in XEmacs fixed

;;;; After 1.37:
;;;  &&s was not recognized as start of regular expression;
;;;  Will "preprocess" the contents of //e part of s///e too;
;;;  What to do with s# blah # foo #e ?
;;;  Should handle s;blah;foo;; better.
;;;  Now the only known problems with regular expression recognition:
;;;;;;;  s<foo>/bar/	- different delimiters (end ignored)
;;;;;;;  s/foo/\\bar/	- backslash at start of subst (made into one chunk)
;;;;;;;  s/foo//	- empty subst (made into one chunk + '/')
;;;;;;;  s/foo/(bar)/	- start-group at start of subst (internal group will not match backwards)

;;;; After 1.38:
;;;  We highlight closing / of s/blah/foo/e;
;;;  This handles s# blah # foo #e too;
;;;  s//blah/, s///, s/blah// works again, and s#blah## too, the algorithm
;;;   is much simpler now;
;;;  Next round of changes: s\\\ works, s<blah>/foo/,
;;;   comments between the first and the second part allowed
;;;  Another problem discovered:
;;;;;;;  s[foo] <blah>e	- e part delimited by different <> (will not match)
;;;  `cperl-find-pods-heres' somehow maybe called when string-face is undefined
;;;   - put a stupid workaround for 20.1

;;;; After 1.39:
;;;  Could indent here-docs for comments;
;;;  These problems fixed:
;;;;;;;  s/foo/\\bar/	- backslash at start of subst (made into two chunk)
;;;;;;;  s[foo] <blah>e	- "e" part delimited by "different" <> (will match)
;;;  Matching brackets honor prefices, may expand abbreviations;
;;;  When expanding abbrevs, will remove last char only after
;;;    self-inserted whitespace;
;;;  More convenient "Refress hard constructs" in menu;
;;;  `cperl-add-tags-recurse', `cperl-add-tags-recurse-noxs'
;;;    added (for -batch mode);
;;;  Better handling of errors when scanning for Perl constructs;
;;;;;;;  Possible "problem" with class hierarchy in Perl distribution
;;;;;;;    directory: ./ext duplicates ./lib;
;;;  Write relative paths for generated TAGS;

;;;; After 1.40:
;;;  s  /// may be separated by "\n\f" too;
;;;  `s  #blah' recognized as a comment;
;;;  Would highlight s/abc//s wrong;
;;;  Debugging code in `cperl-electric-keywords' was leaking a message;

;;;; After 1.41:
;;;  RMS changes for 20.3 merged

;;;; 2.0.1.0: RMS mode (has 3 misprints)

;;;; After 2.0:
;;;  RMS whitespace changes for 20.3 merged

;;;; After 2.1:
;;;  History updated

;;;; After 2.2:
;;;  Merge `c-style-alist' since `c-mode' is no more.  (Somebody who
;;;    uses the styles should check that they work OK!)
;;;  All the variable warnings go away, some undef functions too.

;;;; After 2.3:
;;;  Added `cperl-perldoc' (thanks to Anthony Foiani <afoiani@uswest.com>)
;;;  Added `cperl-pod-to-manpage' (thanks to Nick Roberts <Nick.Roberts@src.bae.co.uk>)
;;;  All the function warnings go away.

;;;; After 2.4:
;;;  `Perl doc', `Regexp' submenus created (latter to allow short displays).
;;;  `cperl-clobber-lisp-bindings' added.
;;;  $a->y() is not y///.
;;;  `cperl-after-block-p' was missing a `save-excursion' => wrong results.
;;;  `cperl-val' was defined too late.
;;;  `cperl-init-faces' was failing.
;;;  Init faces when loading `ps-print'.

;;;; After 2.4:
;;;  `cperl-toggle-autohelp' implemented.
;;;  `while SPACE LESS' was buggy.
;;;  `-text' in `[-text => 1]' was not highlighted.
;;;  `cperl-after-block-p' was FALSE after `sub f {}'.

;;;; After 2.5:
;;;  `foreachmy', `formy' expanded too.
;;;  Expand `=pod-directive'.
;;;  `cperl-linefeed' behaves reasonable in POD-directive lines.
;;;  `cperl-electric-keyword' prints a message, governed by
;;;    `cperl-message-electric-keyword'.

;;;; After 2.6:
;;;  Typing `}' was not checking for being block or not.
;;;  Beautifying levels in RE: Did not know about lookbehind;
;;;			       finding *which* level was not intuitive;
;;;			       `cperl-beautify-levels' added.
;;;  Allow here-docs contain `=head1' and friends (at least for keywords).

;;;; After 2.7:
;;;  Fix for broken `font-lock-unfontify-region-function'.  Should
;;;    preserve `syntax-table' properties even with `lazy-lock'.

;;;; After 2.8:
;;;  Some more compile time warnings crept in.
;;;  `cperl-indent-region-fix-else' implemented.
;;;  `cperl-fix-line-spacing' implemented.
;;;  `cperl-invert-if-unless' implemented (C-c C-t and in Menu).
;;;  Upgraded hints to mention 20.2's goods/bads.
;;;  Started to use `cperl-extra-newline-before-brace-multiline',
;;;    `cperl-break-one-line-blocks-when-indent',
;;;    `cperl-fix-hanging-brace-when-indent', `cperl-merge-trailing-else'.

;;;; After 2.9:
;;;  Workaround for another `font-lock's `syntax-table' text-property bug.
;;;  `zerop' could be applied to nil.
;;;  At last, may work with `font-lock' without setting `cperl-font-lock'.
;;;    (We expect that starting from 19.33, `font-lock' supports keywords
;;;     being a function - what is a correct version?)
;;;  Rename `cperl-indent-region-fix-else' to
;;;    `cperl-indent-region-fix-constructs'.
;;;  `cperl-fix-line-spacing' could be triggered inside strings, would not
;;;     know what to do with BLOCKs of map/printf/etc.
;;;  `cperl-merge-trailing-else' and `cperl-fix-line-spacing' handle
;;;     `continue' too.
;;;  Indentation after {BLOCK} knows about map/printf/etc.
;;;  Finally: treat after-comma lines as continuation lines.

;;;; After 2.10:
;;;  `continue' made electric.
;;;  Electric `do' inserts `do/while'.
;;;  Some extra compile-time warnings crept in.
;;;  `font-lock' of 19.33 could not handle font-lock-keywords being a function
;;;      returning a symbol.

;;;; After 2.11:
;;;  Changes to make syntaxification to be autoredone via `font-lock'.
;;;    Switched on by `cperl-syntaxify-by-font-lock', off by default so far.

;;;; After 2.12:
;;;  Remove some commented out chunks.
;;;  Styles are slightly updated (a lot of work is needed, especially
;;;    with new `cperl-fix-line-spacing').

;;;; After 2.13:
;;;  Old value of style is memorized when choosing a new style, may be
;;;    restored from the same menu.
;;;  Mode-documentation added to micro-docs.
;;;  `cperl-praise' updated.
;;;  `cperl-toggle-construct-fix' added on C-c C-w and menu.
;;;  `auto-fill-mode' added on C-c C-f and menu.
;;;  `PerlStyle' style added.
;;;  Message for termination of scan corrected.

;;;; After 2.14:

;;;  Did not work with -q

;;;; After 2.15:

;;;  `cperl-speed' hints added.
;;;  Minor style fixes.

;;;; After 2.15:
;;;  Make backspace electric after expansion of `else/continue' too.

;;;; After 2.16:
;;;  Starting to merge changes to RMS emacs version.

;;;; After 2.17:
;;;  Merged custom stuff and darn `font-lock-constant-face'.

;;;; After 2.18:
;;;  Bumped the version to 3.1

;;;; After 3.1:
;;;  Fixed customization to honor cperl-hairy.
;;;  Created customization groups.  Sent to RMS to include into 2.3.

;;;; After 3.2:
;;;  Interaction of `font-lock-hot-pass' and `cperl-syntaxify-by-font-lock'.
;;;  (`cperl-after-block-and-statement-beg'):
;;;  (`cperl-after-block-p'):
;;;  (`cperl-after-expr-p'):	It is BLOCK if we reach lim when backup sexp.
;;;  (`cperl-indent-region'):	Make a marker for END - text added/removed.
;;;  (`cperl-style-alist', `cperl-styles-entries')
;;;		Include `cperl-merge-trailing-else' where the value is clear.

;;;; After 3.3:
;;;  (`cperl-tips'):
;;;  (`cperl-problems'):	Improvements to docs.

;;;; After 3.4:
;;;  (`cperl-mode'):		Make lazy syntaxification possible.
;;;  (`cperl-find-pods-heres'): Safe a position in buffer where it is safe to
;;;				restart syntaxification.
;;;  (`cperl-syntaxify-by-font-lock'): Set to t, should be safe now.

;;;; After 3.5:
;;;  (`cperl-syntaxify-by-font-lock'): Better default, customizes to
;;;				`message' too.

;;;; After 3.6:
;;;  (`cperl-find-pods-heres'): changed so that -d ?foo? is a RE.
;;;  (`cperl-array-face'): changed name from `font-lock-emphasized-face'.
;;;  (`cperl-hash-face'): changed name from  `font-lock-other-emphasized-face'.
;;;  Use `defface' to define these two extra faces.

;;;; After 3.7:
;;;  Can use linear algorithm for indentation if Emacs supports it:
;;;  indenting DB::DB (800+ lines) improved from 69 sec to 11 sec
;;;  (73 vs 15 with imenu).
;;;  (`cperl-emacs-can-parse'):	New state.
;;;  (`cperl-indent-line'):	Corrected to use global state.
;;;  (`cperl-calculate-indent'):	Likewise.
;;;  (`cperl-fix-line-spacing'):	Likewise (not used yet).

;;;; After 3.8:
;;;  (`cperl-choose-color'):	Converted to a function (to be compilable in text-mode).

;;;; After 3.9:
;;;  (`cperl-dark-background '):	Disable without window-system.

;;;; After 3.10:
;;;  Do `defface' only if window-system.

;;;; After 3.11:
;;;  (`cperl-fix-line-spacing'):	sped up to bail out early.
;;;  (`cperl-indent-region'):	Disable hooks during the call (how to call them later?).

;;;  Now indents 820-line-long function in 6.5 sec (including syntaxification) the first time
;;;  (when buffer has few properties), 7.1 sec the second time.

;;;Function Name                              Call Count  Elapsed Time  Average Time
;;;=========================================  ==========  ============  ============
;;;cperl-indent-exp                           1           10.039999999  10.039999999
;;;cperl-indent-region                        1           10.0          10.0
;;;cperl-indent-line                          821         6.2100000000  0.0075639464
;;;cperl-calculate-indent                     821         5.0199999999  0.0061144945
;;;cperl-backward-to-noncomment               2856        2.0500000000  0.0007177871
;;;cperl-fontify-syntaxically                 2           1.78          0.8900000000
;;;cperl-find-pods-heres                      2           1.78          0.8900000000
;;;cperl-update-syntaxification               1           1.78          1.78
;;;cperl-fix-line-spacing                     769         1.4800000000  0.0019245773
;;;cperl-after-block-and-statement-beg        163         1.4100000000  0.0086503067
;;;cperl-block-p                              775         1.1800000000  0.0015225806
;;;cperl-to-comment-or-eol                    3652        1.1200000000  0.0003066812
;;;cperl-after-block-p                        165         1.0500000000  0.0063636363
;;;cperl-commentify                           141         0.22          0.0015602836
;;;cperl-get-state                            813         0.16          0.0001968019
;;;cperl-backward-to-start-of-continued-exp   26          0.12          0.0046153846
;;;cperl-delay-update-hook                    2107        0.0899999999  4.271...e-05
;;;cperl-protect-defun-start                  141         0.0700000000  0.0004964539
;;;cperl-after-label                          407         0.0599999999  0.0001474201
;;;cperl-forward-re                           139         0.0299999999  0.0002158273
;;;cperl-comment-indent                       26          0.0299999999  0.0011538461
;;;cperl-use-region-p                         8           0.0           0.0
;;;cperl-lazy-hook                            15          0.0           0.0
;;;cperl-after-expr-p                         8           0.0           0.0
;;;cperl-font-lock-unfontify-region-function  1           0.0           0.0

;;;Function Name                              Call Count  Elapsed Time  Average Time
;;;=========================================  ==========  ============  ============
;;;cperl-fix-line-spacing                     769         1.4500000000  0.0018855656
;;;cperl-indent-line                          13          0.3100000000  0.0238461538
;;;cperl-after-block-and-statement-beg        69          0.2700000000  0.0039130434
;;;cperl-after-block-p                        69          0.2099999999  0.0030434782
;;;cperl-calculate-indent                     13          0.1000000000  0.0076923076
;;;cperl-backward-to-noncomment               177         0.0700000000  0.0003954802
;;;cperl-get-state                            13          0.0           0.0
;;;cperl-to-comment-or-eol                    179         0.0           0.0
;;;cperl-get-help-defer                       1           0.0           0.0
;;;cperl-lazy-hook                            11          0.0           0.0
;;;cperl-after-expr-p                         2           0.0           0.0
;;;cperl-block-p                              13          0.0           0.0
;;;cperl-after-label                          5           0.0           0.0

;;;; After 3.12:
;;;  (`cperl-find-pods-heres'): do not warn on `=cut' if doing a chunk only.

;;;; After 3.13:
;;;  (`cperl-mode'): load pseudo-faces on `cperl-find-pods-heres' (for 19.30).
;;;  (`x-color-defined-p'): was not compiling on XEmacs
;;;  (`cperl-find-pods-heres'): 1 << 6 was OK, but 1<<6 was considered as HERE
;;;                             <file/glob> made into a string.

;;;; After 3.14:
;;;  (`cperl-find-pods-heres'): Postpone addition of faces after syntactic step
;;;				Recognition of <FH> was wrong.
;;;  (`cperl-clobber-lisp-bindings'): if set, C-c variants are the old ones
;;;  (`cperl-unwind-to-safe'):	New function.
;;;  (`cperl-fontify-syntaxically'): Use `cperl-unwind-to-safe' to start at reasonable position.

;;;; After 3.15:
;;;  (`cperl-forward-re'):	Highlight the trailing / in s/foo// as string.
;;;			Highlight the starting // in s//foo/ as function-name.

;;;; After 3.16:
;;;  (`cperl-find-pods-heres'): Highlight `gem' in s///gem as a keyword.

;;;; After 4.0:
;;;  (`cperl-find-pods-heres'): `qr' added
;;;  (`cperl-electric-keyword'):	Likewise
;;;  (`cperl-electric-else'):		Likewise
;;;  (`cperl-to-comment-or-eol'):	Likewise
;;;  (`cperl-make-regexp-x'):	Likewise
;;;  (`cperl-init-faces'):	Likewise, and `lock' (as overridable?).
;;;  (`cperl-find-pods-heres'): Knows that split// is null-RE.
;;;				Highlights separators in 3-parts expressions
;;;				as labels.

;;;; After 4.1:
;;;  (`cperl-find-pods-heres'):	<> was considered as a glob
;;;  (`cperl-syntaxify-unwind'): New configuration variable
;;;  (`cperl-fontify-m-as-s'):	New configuration variable

;;;; After 4.2:
;;;  (`cperl-find-pods-heres'): of the last line being `=head1' fixed.

;;;  Handling of a long construct is still buggy if only the part of
;;;  construct touches the updated region (we unwind to the start of
;;;  long construct, but the end may have residual properties).

;;;  (`cperl-unwind-to-safe'):	would not go to beginning of buffer.
;;;  (`cperl-electric-pod'):	check for after-expr was performed
;;;				inside of POD too.

;;;; After 4.3:
;;;  (`cperl-backward-to-noncomment'):	better treatment of PODs and HEREs.

;;;  Indent-line works good, but indent-region does not - at toplevel...
;;;  (`cperl-unwind-to-safe'):	Signature changed.
;;;  (`x-color-defined-p'):     was defmacro'ed with a tick.  Remove another def.
;;;  (`cperl-clobber-mode-lists'): New configuration variable.
;;;  (`cperl-array-face'): One of definitions was garbled.

;;;; After 4.4:
;;;  (`cperl-not-bad-style-regexp'):	Updated.
;;;  (`cperl-make-regexp-x'):	Misprint in a message.
;;;  (`cperl-find-pods-heres'):	$a-1 ? foo : bar; was a regexp.
;;;                             `<< (' was considered a start of POD.
;;;  Init:			`cperl-is-face' was busted.
;;;  (`cperl-make-face'):	New macros.
;;;  (`cperl-force-face'):	New macros.
;;;  (`cperl-init-faces'):	Corrected to use new macros;
;;;				`if' for copying `reference-face' to
;;;				`constant-face' was backward.
;;;  (`font-lock-other-type-face'): Done via `defface' too.

;;;; After 4.5:
;;;  (`cperl-init-faces-weak'):	use `cperl-force-face'.
;;;  (`cperl-after-block-p'):	After END/BEGIN we are a block.
;;;  (`cperl-mode'):		`font-lock-unfontify-region-function'
;;;				was set to a wrong function.
;;;  (`cperl-comment-indent'):	Commenting __END__ was not working.
;;;  (`cperl-indent-for-comment'):	Likewise.
;;;				(Indenting is still misbehaving at toplevel.)

;;;; After 4.5:
;;;  (`cperl-unwind-to-safe'):	Signature changed, unwinds end too.
;;;  (`cperl-find-pods-heres'):	mark qq[]-etc sections as syntax-type=string
;;;  (`cperl-fontify-syntaxically'): Unwinds start and end to go out of
;;;				     long strings (not very successful).

;;;   >>>>  CPerl should be usable in write mode too now <<<<

;;;  (`cperl-syntaxify-by-font-lock'): Better default - off in text-mode.
;;;  (`cperl-tips'): 		Updated docs.
;;;  (`cperl-problems'):	Updated docs.

;;;; After 4.6:
;;;  (`cperl-calculate-indent'):	Did not consider `,' as continuation mark for statements.
;;;  (`cperl-write-tags'):	Correct for XEmacs's `visit-tags-table-buffer'.

;;;; After 4.7:
;;;  (`cperl-calculate-indent'): Avoid parse-data optimization at toplevel.
;;;				 Should indent correctly at toplevel too.
;;;  (`cperl-tags-hier-init'):	Gross hack to pretend we work (are we?).
;;;  (`cperl-find-pods-heres'):	Was not processing sub protos after a comment ine.
;;;				Was treating $a++ <= 5 as a glob.

;;;; After 4.8:
;;;  (toplevel):		require custom unprotected => failure on 19.28.
;;;  (`cperl-xemacs-p')		defined when compile too
;;;  (`cperl-tags-hier-init'):	Another try to work around XEmacs problems
;;;				Better progress messages.
;;;  (`cperl-find-tags'):	Was writing line/pos in a wrong order,
;;;				pos off by 1 and not at beg-of-line.
;;;  (`cperl-etags-snarf-tag'): New macro
;;;  (`cperl-etags-goto-tag-location'): New macro
;;;  (`cperl-write-tags'):	When removing old TAGS info was not
;;;				relativizing filename

;;;; After 4.9:
;;;  (`cperl-version'):		New variable.  New menu entry

;;;; After 4.10:
;;;  (`cperl-tips'):		Updated.
;;;  (`cperl-non-problems'):	Updated.
;;;  random:			References to future 20.3 removed.

;;;; After 4.11:
;;;  (`perl-font-lock-keywords'): Would not highlight `sub foo($$);'.
;;;  Docstrings:		Menu was described as `CPerl' instead of `Perl'

;;;; After 4.12:
;;;  (`cperl-toggle-construct-fix'): Was toggling to t instead of 1.
;;;  (`cperl-ps-print-init'):	Associate `cperl-array-face', `cperl-hash-face'
;;;				remove `font-lock-emphasized-face'.
;;;				remove `font-lock-other-emphasized-face'.
;;;				remove `font-lock-reference-face'.
;;;				remove `font-lock-keyword-face'.
;;;				Use `eval-after-load'.
;;;  (`cperl-init-faces'):	remove init `font-lock-other-emphasized-face'.
;;;				remove init `font-lock-emphasized-face'.
;;;				remove init `font-lock-keyword-face'.
;;;  (`cperl-tips-faces'):	New variable and an entry into Mini-docs.
;;;  (`cperl-indent-region'):	Do not indent whitespace lines
;;;  (`cperl-indent-exp'):	Was not processing else-blocks.
;;;  (`cperl-calculate-indent'): Remove another parse-data optimization
;;;				 at toplevel: would indent correctly.
;;;  (`cperl-get-state'):	NOP line removed.

;;;; After 4.13:
;;;  (`cperl-ps-print-init'):	Remove not-CPerl-related faces.
;;;  (`cperl-ps-print'):	New function and menu entry.
;;;  (`cperl-ps-print-face-properties'):	New configuration variable.
;;;  (`cperl-invalid-face'):	New configuration variable.
;;;  (`cperl-nonoverridable-face'):	New face.  Renamed from
;;;					`font-lock-other-type-face'.
;;;  (`perl-font-lock-keywords'):	Highlight trailing whitespace
;;;  (`cperl-contract-levels'):	Documentation corrected.
;;;  (`cperl-contract-level'):	Likewise.

;;;; After 4.14:
;;;  (`cperl-ps-print'): `ps-print-face-extension-alist' was not in old Emaxen,
;;;				same with `ps-extend-face-list'
;;;  (`cperl-ps-extend-face-list'):	New macro.

;;;; After 4.15:
;;;  (`cperl-init-faces'):	Interpolate `cperl-invalid-face'.
;;;  (`cperl-forward-re'):	Emit a meaningful error instead of a cryptic
;;;				one for uncomplete REx near end-of-buffer.
;;;  (`cperl-find-pods-heres'):	Tolerate unfinished REx at end-of-buffer.

;;;; After 4.16:
;;;  (`cperl-find-pods-heres'): `unwind-protect' was left commented.

;;;; After 4.17:
;;;  (`cperl-invalid-face'):	Change to ''underline.

;;;; After 4.18:
;;;  (`cperl-find-pods-heres'):	/ and ? after : start a REx.
;;;  (`cperl-after-expr-p'):	Skip labels when checking
;;;  (`cperl-calculate-indent'): Correct for labels when calculating
;;;					indentation of continuations.
;;;				Docstring updated.

;;;; After 4.19:
;;;  Minor (mostly spelling) corrections from 20.3.3 merged.

;;;; After 4.20:
;;;  (`cperl-tips'):		Another workaround added.  Sent to RMS for 20.4.

;;;; After 4.21:
;;;  (`cperl-praise'):		Mention linear-time indent.
;;;  (`cperl-find-pods-heres'):	@if ? a : b was considered a REx.

;;;; After 4.22:
;;;  (`cperl-after-expr-p'):	Make true after __END__.
;;;  (`cperl-electric-pod'):	"SYNOPSIS" was misspelled.

;;;; After 4.23:
;;;  (`cperl-beautify-regexp-piece'):	Was not allowing for *? after a class.
;;;					Allow for POSIX char-classes.
;;;					Remove trailing whitespace when
;;;					adding new linebreak.
;;;					Add a level counter to stop shallow.
;;;					Indents unprocessed groups rigidly.
;;;  (`cperl-beautify-regexp'):	Add an optional count argument to go that
;;;				many levels deep.
;;;  (`cperl-beautify-level'):	Likewise
;;;  Menu:			Add new entries to Regexp menu to do one level
;;;  (`cperl-contract-level'):	Was entering an infinite loop
;;;  (`cperl-find-pods-heres'):	Typo (double quoting).
;;;				Was detecting < $file > as FH instead of glob.
;;;				Support for comments in RExen (except
;;;				for m#\#comment#x), governed by
;;;				`cperl-regexp-scan'.
;;;  (`cperl-regexp-scan'):	New customization variable.
;;;  (`cperl-forward-re'):	Improve logic of resetting syntax table.

;;;; After 4.23 and: After 4.24:
;;;  (`cperl-contract-levels'):	Restore position.
;;;  (`cperl-beautify-level'):	Likewise.
;;;  (`cperl-beautify-regexp'):	Likewise.
;;;  (`cperl-commentify'):	Rudimental support for length=1 runs
;;;  (`cperl-find-pods-heres'):	Process 1-char long REx comments too /a#/x
;;;				Processes REx-comments in #-delimited RExen.
;;;				MAJOR BUG CORRECTED: after a misparse
;;;				  a body of a subroutine could be corrupted!!!
;;;				  One might need to reeval the function body
;;;				  to fix things.  (A similar bug was
;;;				  present in `cperl-indent-region' eons ago.)
;;; To reproduce:
;;   (defun foo () (let ((a '(t))) (insert (format "%s" a)) (setcar a 'BUG) t))
;;   (foo)
;;   (foo)
;;; C-x C-e the above three lines (at end-of-line).  First evaluation
;;; of `foo' inserts (t), second one inserts (BUG) ?!
;;;
;;; In CPerl it was triggered by inserting then deleting `/' at start of
;;;      /  a (?# asdf  {[(}asdf )ef,/;

;;;; After 4.25:
;;; (`cperl-commentify'):	Was recognizing length=2 "strings" as length=1.
;;; (`imenu-example--create-perl-index'):
;;;				Was not enforcing syntaxification-to-the-end.
;;; (`cperl-invert-if-unless'):	Allow `for', `foreach'.
;;; (`cperl-find-pods-heres'):	Quote `cperl-nonoverridable-face'.
;;;				Mark qw(), m()x as indentable.
;;; (`cperl-init-faces'):	Highlight `sysopen' too.
;;;				Highlight $var in `for my $var' too.
;;; (`cperl-invert-if-unless'):	Was leaving whitespace at end.
;;; (`cperl-linefeed'):		Was splitting $var{$foo} if point after `{'.
;;; (`cperl-calculate-indent'): Remove old commented out code.
;;;				Support (primitive) indentation of qw(), m()x.


;;;; After 4.26:
;;; (`cperl-problems'):		Mention `fill-paragraph' on comment. \"" and
;;;				q [] with intervening newlines.
;;; (`cperl-autoindent-on-semi'):	New customization variable.
;;; (`cperl-electric-semi'):	Use `cperl-autoindent-on-semi'.
;;; (`cperl-tips'):		Mention how to make CPerl the default mode.
;;; (`cperl-mode'):		Support `outline-minor-mode'
;;;				(Thanks to Mark A. Hershberger).
;;; (`cperl-outline-level'):	New function.
;;; (`cperl-highlight-variables-indiscriminately'):	New customization var.
;;; (`cperl-init-faces'):	Use `cperl-highlight-variables-indiscriminately'.
;;;				(Thanks to Sean Kamath <kamath@pogo.wv.tek.com>).
;;; (`cperl-after-block-p'):	Support CHECK and INIT.
;;; (`cperl-init-faces'):	Likewise and "our".
;;;				(Thanks to Doug MacEachern <dougm@covalent.net>).
;;; (`cperl-short-docs'):	Likewise and "our".


;;;; After 4.27:
;;; (`cperl-find-pods-heres'):	Recognize \"" as a string.
;;;				Mark whitespace and comments between q and []
;;;				  as `syntax-type' => `prestring'.
;;;				Allow whitespace between << and "FOO".
;;; (`cperl-problems'):		Remove \"" and q [] with intervening newlines.
;;;				Mention multiple <<EOF as unsupported.
;;; (`cperl-highlight-variables-indiscriminately'):	Doc misprint fixed.
;;; (`cperl-indent-parens-as-block'):	New configuration variable.
;;; (`cperl-calculate-indent'):	Merge cases of indenting non-BLOCK groups.
;;;				Use `cperl-indent-parens-as-block'.
;;; (`cperl-find-pods-heres'):	Test for =cut without empty line instead of
;;;				complaining about no =cut.
;;; (`cperl-electric-pod'):	Change the REx for POD from "\n\n=" to "^\n=".
;;; (`cperl-find-pods-heres'):	Likewise.
;;; (`cperl-electric-pod'):	Change `forward-sexp' to `forward-word':
;;;				POD could've been marked as comment already.
;;; (`cperl-unwind-to-safe'):	Unwind before start of POD too.

;;;; After 4.28:
;;; (`cperl-forward-re'):	Throw an error at proper moment REx unfinished.

;;;; After 4.29:
;;; (`x-color-defined-p'):	Make an extra case to peacify the warning.
;;; Toplevel:			`defvar' to peacify the warnings.
;;; (`cperl-find-pods-heres'):	Could access `font-lock-comment-face' in -nw.
;;;;				No -nw-compile time warnings now.
;;; (`cperl-find-tags'):	TAGS file had too short substring-to-search.
;;;				Be less verbose in non-interactive mode
;;; (`imenu-example--create-perl-index'):	Set index-marker after name
;;; (`cperl-outline-regexp'):	New variable.
;;; (`cperl-outline-level'):	Made compatible with `cperl-outline-regexp'.
;;; (`cperl-mode'):		Made use `cperl-outline-regexp'.

;;;; After 4.30:
;;; (`cperl-find-pods-heres'):	=cut the last thing, no blank line, was error.
;;; (`cperl-outline-level'):	Make start-of-file same level as `package'.

;;;; After 4.31:
;;; (`cperl-electric-pod'):	`head1' and `over' electric only if empty.
;;; (`cperl-unreadable-ok'):	New variable.
;;; (`cperl-find-tags'):	Use `cperl-unreadable-ok', do not fail
;;;				on an unreadable file
;;; (`cperl-write-tags'):	Use `cperl-unreadable-ok', do not fail
;;;				on an unreadable directory

;;;; After 4.32:
;;;  Syncronized with v1.60 from Emacs 21.3.
;;;  Mostly docstring and formatting changes, and:

;;;  (`cperl-noscan-files-regexp'): Do not scan CVS subdirs
;;;  (`cperl-problems'):	Note that newer XEmacsen may syntaxify too
;;;  (`imenu-example--create-perl-index'):
;;;				Renamed to `cperl-imenu--create-perl-index'
;;;  (`cperl-mode'):		Replace `make-variable-buffer-local' by `make-local-variable'
;;;  (`cperl-setup-tmp-buf'):	Likewise
;;;  (`cperl-fix-line-spacing'): Fix a misprint of "t" for "\t"
;;;  (`cperl-next-bad-style'):  Fix misprints in character literals

;;;; After 4.33:
;;;  (`cperl-font-lock-keywords'): +etc: Aliased to perl-font-lock-keywords.

;;;; After 4.34:
;;;  Further updates of whitespace and spelling w.r.t. RMS version.
;;;  (`cperl-font-lock-keywords'): +etc: Avoid warnings when aliasing.
;;;  (`cperl-mode'):		Use `normal-auto-fill-function' if present.
;;;  (`cperl-use-major-mode'):	New variable
;;;  (`cperl-can-font-lock'):	New variable; replaces `window-system'
;;;  (`cperl-tags-hier-init'):	use `display-popup-menus-p' (if present)
;;;				to choose `x-popup-menu' vs `tmm-prompt'

;;;; 4.35 has the following differences from version 1.40+ of RMS Emacs:

;;; New variables `cperl-use-major-mode', `cperl-can-font-lock';
;;; `cperl-use-major-mode' is (effectively) 'cperl-mode in RMS.
;;; `cperl-under-as-char'  is nil in RMS.
;;; Minor differences in docstrings, and `cperl-non-problems'.
;;; Backward compatibility addressed: (`); (function (lambda ...)); font-lock;
;;; (:italic t bold t) vs (:slant italic :weight bold) in faces;
;;; `normal-auto-fill-function'.
;;; RMS version has wrong logic in `cperl-calculate-indent': $a = { } is
;;; wrongly indented if the closing brace is on a separate line.
;;; Different choice of ordering if's for is-x-REx and (eq (char-after b) ?\#)
;;; in `cperl-find-pods-heres'. [Cosmetic]

;;;; After 4.35:
;;;  (`cperl-find-pods-heres'):	If no end of HERE-doc found, mark to the end
;;;				of buffer.  This enables recognition of end
;;;				of HERE-doc "as one types".
;;;				Require "\n" after trailing tag of HERE-doc.
;;;				\( made non-quoting outside of string/comment
;;;				(gdj-contributed).
;;;				Likewise for \$.
;;;				Remove `here-doc-group' text property at start
;;;				(makes this property reliable).
;;;				Text property `first-format-line' ==> t.
;;;				Do not recognize $opt_s and $opt::s as s///.
;;;  (`cperl-perldoc'):		Use case-sensitive search (contributed).
;;;  (`cperl-fix-line-spacing'): Allow "_" in $vars of foreach etc. when
;;;				underscore isn't a word char (gdj-contributed).
;;;  (`defun-prompt-regexp'):	Allow prototypes.
;;;  (`cperl-vc-header-alist'):	Extract numeric version from the Id.
;;;  Toplevel:			Put toggle-autohelp into the mode menu.
;;;				Better docs for toggle/set/unset autohelp.
;;;  (`cperl-electric-backspace-untabify'): New customization variable
;;;  (`cperl-after-expr-p'):	Works after here-docs, formats, and PODs too
;;;				(affects many electric constructs).
;;;  (`cperl-calculate-indent'): Takes into account `first-format-line' ==>
;;;				works after format.
;;;  (`cperl-short-docs'):	Make it work with ... too.
;;;				"array context" ==> "list context"
;;;  (`cperl-electric-keyword'): make $if (etc: "$@%&*") non-electric
;;;				'(' after keyword would insert a doubled paren
;;;  (`cperl-electric-paren'):	documented affected by `cperl-electric-parens'
;;;  (`cperl-electric-rparen'):	Likewise
;;;  (`cperl-build-manpage'):	New function by Nick Roberts
;;;  (`cperl-perldoc'):		Make it work in XEmacs too

;;;; After 4.36:
;;;  (`cperl-find-pods-heres'):	Recognize s => 1 and {s} (as a key or varname),
;;;				{ s:: } and { s::bar::baz } as varnames.
;;;  (`cperl-after-expr-p'):	Updates syntaxification before checks
;;;  (`cperl-calculate-indent'): Likewise
;;;				Fix wrong indent of blocks starting with POD
;;;  (`cperl-after-block-p'):	Optional argument for checking for a pre-block
;;;				Recognize `continue' blocks too.
;;;  (`cperl-electric-brace'):	use `cperl-after-block-p' for detection;
;;;				Now works for else/continue/sub blocks
;;;  (`cperl-short-docs'):	Minor edits; make messages fit 80-column screen

;;;; After 5.0:
;;;  `cperl-add-tags-recurse-noxs-fullpath': new function (for -batch mode)

;;;; After 5.1:
;;;;;; Major edit.  Summary of most visible changes:

;;;;;; a) Multiple <<HERE per line allowed.
;;;;;; b) Handles multiline subroutine declaration headers (with comments).
;;;;;;    (The exception is `cperl-etags' - but it is not used in the rest
;;;;;;    of the mode.)
;;;;;; c) Fontifies multiline my/our declarations (even with comments,
;;;;;;    and with legacy `font-lock').
;;;;;; d) Major speedup of syntaxification, both immediate and postponed
;;;;;;    (3.5x to 15x [for different CPUs and versions of Emacs] on the
;;;;;;    huge real-life document I tested).
;;;;;; e) New bindings, edits to imenu.
;;;;;; f) "_" is made into word-char during fontification/syntaxification;
;;;;;;    some attempts to recognize non-word "_" during other operations too.
;;;;;; g) Detect bug in Emacs with `looking-at' inside `narrow' and bulk out.
;;;;;; h) autoload some more perldoc-related stuff
;;;;;; i) Some new convenience features: ISpell POD/HEREDOCs, narrow-to-HEREDOC
;;;;;; j) Attempt to incorporate XEmacs edits which reached me

;;;; Fine-grained changelog:
;;; `cperl-hook-after-change':	New configuration variable
;;; `cperl-vc-sccs-header':	Likewise
;;; `cperl-vc-sccs-header':	Likewise
;;; `cperl-vc-header-alist':	Default via two preceding variables
;;; `cperl-invalid-face':	Remove double quoting under XEmacs
;;;					(still needed under 21.2)
;;; `cperl-tips':		Update URLs for resources
;;; `cperl-problems':		Likewise
;;; `cperl-praise':		Mention new features
;;; New C-c key bindings:	for `cperl-find-bad-style',
;;;	`cperl-pod-spell', `cperl-here-doc-spell', `cperl-narrow-to-here-doc',
;;;	`cperl-perdoc', `cperl-perldoc-at-point'
;;; CPerl Mode menu changes:	"Fix style by spaces", "Imenu on Perl Info"
;;;	moved, new submenu of Tools with Ispell entries and narrowing.
;;; `cperl-after-sub-regexp':	New defsubst
;;; `cperl-imenu--function-name-regexp-perl': Use `cperl-after-sub-regexp',
;;;				Allows heads up to head4
;;;				Allow "package;"
;;; `defun-prompt-regexp':	Use `cperl-after-sub-regexp',
;;; `paren-backwards-message':	??? Something for XEmacs???
;;; `cperl-mode':		Never auto-switch abbrev-mode off
;;;				Try to allow '_' be non-word char
;;;				Do not use `font-lock-unfontify-region-function' on XEmacs
;;;				Reset syntax cache on mode start
;;;				Support multiline facification (even
;;;				        on legacy `font-lock')
;;; `cperl-facemenu-add-face-function':	??? Some contributed code ???
;;; `cperl-after-change-function':	Since `font-lock' and `lazy-lock'
;;;         refuse to inform us whether the fontification is due to lazy
;;;         calling or due to edit to a buffer, install our own hook
;;;         (controlled by `cperl-hook-after-change')
;;; `cperl-electric-pod':	=cut may have been recognized as start
;;; `cperl-block-p':		Moved, updated for attributes
;;; `cperl-calculate-indent':	Try to allow '_' be non-word char
;;;				Support subs with attributes
;;; `cperl-where-am-i':		Queit (?) a warning
;;; `cperl-cached-syntax-table'	New function
;;; `cperl-forward-re':		Use `cperl-cached-syntax-table'
;;; `cperl-unwind-to-safe':	Recognize `syntax-type' property
;;;					changing in a middle of line
;;; `cperl-find-sub-attrs':	New function
;;; `cperl-find-pods-heres':	Allow many <<EOP per line
;;;				Allow subs with attributes
;;;				Major speedups (3.5x..15x on a real-life
;;;				        test file nph-proxy.pl)
;;;				Recognize "extproc " (OS/2)
;;;				        case-folded and only at start
;;;				/x on s///x with empty replacement was
;;;				        not recognized
;;;				Better comments
;;; `cperl-after-block-p':	Remarks on diff with `cperl-block-p'
;;;				Allow subs with attributes, labels
;;;				Do not confuse "else::foo" with "else"
;;;				Minor optimizations...
;;; `cperl-after-expr-p':	Try to allow '_' be non-word char
;;; `cperl-fill-paragraph':	Try to detect a major bug in Emacs
;;;         with `looking-at' inside `narrow' and bulk out if found
;;; `cperl-imenu--create-perl-index':	Updates for new
;;;         `cperl-imenu--function-name-regexp-perl'
;;; `cperl-outline-level':	Likewise
;;; `cperl-init-faces':		Allow multiline subroutine headers
;;;         and my/our declarations, and ones with comments
;;;				Allow subroutine attributes
;;; `cperl-imenu-on-info':	Better docstring.
;;; `cperl-etags'		Rudimentary support for attributes
;;;				Support for packages and "package;"
;;; `cperl-add-tags-recurse-noxs':	Better (?) docstring
;;; `cperl-add-tags-recurse-noxs-fullpath': Likewise
;;; `cperl-tags-hier-init':	Misprint for `fboundp' fixed
;;; `cperl-not-bad-style-regexp':	Try to allow '_' be non-word char
;;; `cperl-perldoc':		Add autoload
;;; `cperl-perldoc-at-point':	Likewise
;;; `cperl-here-doc-spell':	New function
;;; `cperl-pod-spell':		Likewise
;;; `cperl-map-pods-heres':	Likewise
;;; `cperl-get-here-doc-region':	Likewise
;;; `cperl-font-lock-fontify-region-function': Likewise (backward compatibility
;;;					        for legacy `font-lock')
;;; `cperl-font-lock-unfontify-region-function': Fix style
;;; `cperl-fontify-syntaxically':	Recognize and optimize away
;;;         deferred calls with no-change.  Governed by `cperl-hook-after-change'
;;; `cperl-fontify-update':	Recognize that syntaxification region
;;;         can be larger than fontification one.
;;;         XXXX we leave `cperl-postpone' property, so this is quadratic...
;;; `cperl-fontify-update-bad':	Temporary placeholder until
;;;         it is clear how to implement `cperl-fontify-update'.
;;; `cperl-time-fontification':	New function
;;; `attrib-group':		New text attribute
;;; `multiline':		New value: `syntax-type' text attribute

;;;; After 5.2:
;;; `cperl-emulate-lazy-lock':	New function
;;; `cperl-fontify-syntaxically': Would skip large regions
;;; Add `cperl-time-fontification', `cperl-emulate-lazy-lock' to menu
;;; Some globals were declared, but uninitialized

;;;; After 5.3, 5.4:
;;; `cperl-facemenu-add-face-function':	Add docs, fix U<>
;;; Copyright message updated.
;;; `cperl-init-faces':		Work around a bug in `font-lock'. May slow
;;;					facification down a bit.
;;;				Misprint for my|our|local for old `font-lock'
;;;				"our" was not fontified same as "my|local"
;;;				Highlight variables after "my" etc even in
;;;					a middle of an expression
;;;				Do not facify multiple variables after my etc
;;;					unless parentheses are present

;;; After 5.5, 5.6
;;; `cperl-fontify-syntaxically': after-change hook could reset
;;;	`cperl-syntax-done-to' to a middle of line; unwind to BOL.

;;; After 5.7:
;;; `cperl-init-faces':		Allow highlighting of local ($/)
;;; `cperl-problems-old-emaxen': New variable (for the purpose of DOCSTRING).
;;; `cperl-problems':		Remove fixed problems.
;;; `cperl-find-pods-heres':	Recognize #-comments in m##x too
;;;				Recognize charclasses (unless delimiter is \).
;;; `cperl-fontify-syntaxically': Unwinding to safe was done in wrong order
;;; `cperl-regexp-scan':	Update docs
;;; `cperl-beautify-regexp-piece': use information got from regexp scan

;;; After 5.8:
;;; Major user visible changes:
;;; Recognition and fontification of character classes in RExen.
;;; Variable indentation of RExen according to groups
;;;
;;; `cperl-find-pods-heres':	Recognize POSIX classes in REx charclasses
;;;				Fontify REx charclasses in variable-name face
;;;				Fontify POSIX charclasses in "type" face
;;;				Fontify unmatched "]" in function-name face
;;;				Mark first-char of HERE-doc as `front-sticky'
;;;				Reset `front-sticky' property when needed
;;; `cperl-calculate-indent':	Indents //x -RExen accordning to parens level
;;; `cperl-to-comment-or-eol':	Recognize ends of `syntax-type' constructs
;;; `cperl-backward-to-noncomment': Recognize stringy `syntax-type' constructs
;;;				Support `narrow'ed buffers.
;;; `cperl-praise':		Remove a reservation
;;; `cperl-make-indent':	New function
;;; `cperl-indent-for-comment':	Use `cperl-make-indent'
;;; `cperl-indent-line':	Likewise
;;; `cperl-lineup':		Likewise
;;; `cperl-beautify-regexp-piece': Likewise
;;; `cperl-contract-level':	Likewise
;;; `cperl-toggle-set-debug-unwind': New function
;;;				New menu entry for this
;;; `fill-paragraph-function':	Use when `boundp'
;;; `cperl-calculate-indent':	Take into account groups when indenting RExen
;;; `cperl-to-comment-or-eol':	Recognize # which end a string
;;; `cperl-modify-syntax-type':	Make only syntax-table property non-sticky
;;; `cperl-fill-paragraph':	Return t: needed for `fill-paragraph-function'
;;; `cperl-fontify-syntaxically': More clear debugging message
;;; `cperl-pod2man-build-command': XEmacs portability: check `Man-filter-list'
;;; `cperl-init-faces':		More complicated highlight even on XEmacs (new)
;;; Merge cosmetic changes from XEmacs

;;; After 5.9:
;;; `cperl-1+':			Moved to before the first use
;;; `cperl-1-':			Likewise

;;; After 5.10:

;;; This code may lock Emacs hard!!!  Use on your own risk!

;;; `cperl-font-locking':	New internal variable
;;; `cperl-beginning-of-property': New function
;;; `cperl-calculate-indent':	Use `cperl-beginning-of-property'
;;;	instead of `previous-single-property-change'
;;; `cperl-unwind-to-safe':	Likewise
;;; `cperl-after-expr-p':	Likewise
;;; `cperl-get-here-doc-region': Likewise
;;; `cperl-font-lock-fontify-region-function': Likewise
;;; `cperl-to-comment-or-eol':	Do not call `cperl-update-syntaxification'
;;;					recursively
;;;				Bound `next-single-property-change'
;;;					via `point-max'
;;; `cperl-unwind-to-safe':	Bound likewise
;;; `cperl-font-lock-fontify-region-function': Likewise
;;; `cperl-find-pods-heres':	Mark as recursive for `cperl-to-comment-or-eol'
;;;				Initialization of
;;;	`cperl-font-lock-multiline-start' could be missed if the "main"
;;;	fontification did not run due to the keyword being already fontified.
;;; `cperl-pod-spell':		Return t from do-one-chunk function
;;; `cperl-map-pods-heres':	Stop when the worker returns nil
;;;				Call `cperl-update-syntaxification'
;;; `cperl-get-here-doc-region': Call `cperl-update-syntaxification'
;;; `cperl-get-here-doc-delim':	Remove unused function

;;; After 5.11:

;;;  The possible lockup of Emacs (introduced in 5.10) fixed

;;; `cperl-unwind-to-safe':	`cperl-beginning-of-property' won't return nil
;;; `cperl-syntaxify-for-menu':	New customization variable
;;; `cperl-select-this-pod-or-here-doc': New function
;;; `cperl-get-here-doc-region': Extra argument
;;;				Do not adjust pos by 1

;;; New menu entries (Perl/Tools): selection of current POD or HERE-DOC section
;;;				(Debugging CPerl:) backtrace on fontification

;;; After 5.12:
;;; `cperl-cached-syntax-table': use `car-safe'
;;; `cperl-forward-re':		Remove spurious argument SET-ST
;;;				Add documentation
;;; `cperl-forward-group-in-re': New function
;;; `cperl-find-pods-heres':	Find and highlight (?{}) blocks in RExen
;;;	(XXXX Temporary (?) hack is to syntax-mark them as comment)

;;; After 5.13:
;;; `cperl-string-syntax-table': Make { and } not-grouping
;;;   (Sometimes they ARE grouping in RExen, but matching them would only
;;;    confuse in many situations when they are not)
;;; `beginning-of-buffer':	Replaced two occurences with goto-char...
;;; `cperl-calculate-indent':	`char-after' could be nil...
;;; `cperl-find-pods-heres':	REx can start after "[" too
;;;				Hightlight (??{}) in RExen too 
;;; `cperl-maybe-white-and-comment-rex': New constant
;;; `cperl-white-and-comment-rex': Likewise
;;;				XXXX Not very efficient, but hard to make
;;;				better while keeping 1 group

;;; After 5.13:
;;; `cperl-find-pods-heres':	$foo << identifier() is not a HERE-DOC
;;;				Likewise for 1 << identifier

;;; After 5.14:
;;; `cperl-find-pods-heres':	Different logic for $foo .= <<EOF etc
;;;				Error-less condition-case could fail
;;; `cperl-font-lock-fontify-region-function': Likewise
;;; `cperl-init-faces':		Likewise

;;; After 5.15:
;;; `cperl-find-pods-heres':	Support property REx-part2
;;; `cperl-calculate-indent':	Likewise
;;;				Don't special-case REx with non-empty 1st line
;;; `cperl-find-pods-heres':	In RExen, highlight non-literal backslashes
;;;				Invert highlighting of charclasses: 
;;;					now the envelop is highlighted
;;;				Highlight many others 0-length builtins
;;; `cperl-praise':		Mention indenting and highlight in RExen

;;; After 5.15:
;;; `cperl-find-pods-heres':	Highlight capturing parens in REx

;;; After 5.16:
;;; `cperl-find-pods-heres':	Highlight '|' for alternation
;;;	Initialize `font-lock-warning-face' if not present
;;; `cperl-find-pods-heres':	Use `font-lock-warning-face' instead of
;;;					 `font-lock-function-name-face'
;;; `cperl-look-at-leading-count': Likewise
;;; `cperl-find-pods-heres':	localize `font-lock-variable-name-face'
;;;					`font-lock-keyword-face' (needed for
;;;					batch processing) etc
;;;				Use `font-lock-builtin-face' for builtin in REx
;;;					Now `font-lock-variable-name-face'
;;;					is used for interpolated variables
;;;				Use "talking aliases" for faces inside REx
;;;				Highlight parts of REx (except in charclasses)
;;;					according to the syntax and/or semantic
;;;				Syntax-mark a {}-part of (?{}) as "comment"
;;;					(it was the ()-part)
;;;				Better logic to distinguish what is what in REx
;;; `cperl-tips-faces':		Document REx highlighting
;;; `cperl-praise':		Mention REx syntax highlight etc.

;;; After 5.17:
;;; `cperl-find-sub-attrs':	Would not always manage to print error message
;;; `cperl-find-pods-heres':	localize `font-lock-constant-face'

;;; After 5.18:
;;; `cperl-find-pods-heres':	Misprint in REx for parsing REx
;;;				Very minor optimization
;;;				`my-cperl-REx-modifiers-face' got quoted
;;;				Recognize "print $foo <<END" as HERE-doc
;;;				Put `REx-interpolated' text attribute if needed
;;; `cperl-invert-if-unless-modifiers':	New function
;;; `cperl-backward-to-start-of-expr': Likewise
;;; `cperl-forward-to-end-of-expr': Likewise
;;; `cperl-invert-if-unless':	Works in "the opposite way" too
;;;				Cursor position on return is on the switch-word
;;;				Indents comments better
;;; `REx-interpolated':		New text attribute
;;; `cperl-next-interpolated-REx': New function
;;; `cperl-next-interpolated-REx-0': Likewise
;;; `cperl-next-interpolated-REx-1': Likewise
;;; "\C-c\C-x", "\C-c\C-y", "\C-c\C-v":	New keybinding for these functions
;;; Perl/Regexp menu:		3 new entries for `cperl-next-interpolated-REx'
;;; `cperl-praise':		Mention finded interpolated RExen

;;; After 5.19:
;;; `cperl-init-faces':		Highlight %$foo, @$foo too
;;; `cperl-short-docs':		Better docs for system, exec
;;; `cperl-find-pods-heres':	Better detect << after print {FH} <<EOF etc.
;;;				Would not find HERE-doc ended by EOF without NL
;;; `cperl-short-docs':		Correct not-doubled \-escapes
;;; start block:		Put some `defvar' for stuff gone from XEmacs

;;; After 5.20:
;;; initial comment:		Extend copyright, fix email address
;;; `cperl-indent-comment-at-column-0': New customization variable
;;; `cperl-comment-indent':	Indentation after $#a would increasy by 1
;;; `cperl-mode':		Make `defun-prompt-regexp' grok BEGIN/END etc
;;; `cperl-find-pods-heres':	Mark CODE of s///e as `syntax-type' `multiline'
;;; `cperl-at-end-of-expr':	Would fail if @BAR=12 follows after ";"
;;; `cperl-init-faces':		If `cperl-highlight-variables-indiscriminately'
;;;					highlight $ in $foo too (UNTESTED)
;;; `cperl-set-style':		Docstring missed some available styles
;;; toplevel:			Menubar/Perl/Indent-Styles had FSF, now K&R
;;;				Change "Current" to "Memorize Current"
;;; `cperl-indent-wrt-brace':	New customization variable; the default is
;;;				as for pre-5.2 version
;;; `cperl-styles-entries':	Keep `cperl-extra-newline-before-brace-multiline'
;;; `cperl-style-alist':	Likewise
;;; `cperl-fix-line-spacing':	Support `cperl-merge-trailing-else' being nil,
;;;				and `cperl-extra-newline-before-brace' etc
;;;				being t
;;; `cperl-indent-exp':		Plans B and C to find continuation blocks even
;;;				if `cperl-extra-newline-before-brace' is t

;;; After 5.21:
;;; Improve some docstrings concerning indentation.
;;; `cperl-indent-rules-alist':	New variable
;;; `cperl-sniff-for-indent':	New function name
;;				(separated from `cperl-calculate-indent')
;;; `cperl-calculate-indent':	Separated the sniffer and the indenter;
;;;				uses `cperl-sniff-for-indent' now
;;; `cperl-comment-indent':	Test for `cperl-indent-comment-at-column-0'
;;;				was inverted;
;;;				Support `comment-column' = 0

;;; After 5.22:
;;; `cperl-where-am-i':		Remove function
;;; `cperl-backward-to-noncomment': Would go too far when skipping POD/HEREs
;;; `cperl-sniff-for-indent':	[string] and [comment] were inverted
;;;				When looking for label, skip s:m:y:tr
;;; `cperl-indent-line':	Likewise.
;;; `cperl-mode':		`font-lock-multiline' was assumed auto-local
;;; `cperl-windowed-init':	Wrong `ps-print' handling
;;;				 (both thanks to Chong Yidong)
;;; `cperl-look-at-leading-count': Could fail with unfinished RExen
;;; `cperl-find-pods-heres':	If the second part of s()[] is missing,
;;;					could try to highlight delimiters...

;;; Code:

(if (fboundp 'eval-when-compile)
    (eval-when-compile
      (condition-case nil
	  (require 'custom)
	(error nil))
      (condition-case nil
	  (require 'man)
	(error nil))
      (defconst cperl-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))
      (defvar cperl-can-font-lock
	(or cperl-xemacs-p
	    (and (boundp 'emacs-major-version)
		 (or window-system
		     (> emacs-major-version 20)))))
      (if cperl-can-font-lock
	  (require 'font-lock))
      (defvar msb-menu-cond)
      (defvar gud-perldb-history)
      (defvar font-lock-background-mode) ; not in Emacs
      (defvar font-lock-display-type)	; ditto
      (defvar paren-backwards-message)	; Not in newer XEmacs?
      (defvar vc-rcs-header)		; likewise?
      (defvar vc-sccs-header)		; likewise?
      (or (fboundp 'defgroup)
	  (defmacro defgroup (name val doc &rest arr)
	    nil))
      (or (fboundp 'custom-declare-variable)
	  (defmacro defcustom (name val doc &rest arr)
	    (` (defvar (, name) (, val) (, doc)))))
      (or (and (fboundp 'custom-declare-variable)
	       (string< "19.31" emacs-version))	;  Checked with 19.30: defface does not work
	  (defmacro defface (&rest arr)
	    nil))
      ;; Avoid warning (tmp definitions)
      (or (fboundp 'x-color-defined-p)
	  (defmacro x-color-defined-p (col)
	    (cond ((fboundp 'color-defined-p) (` (color-defined-p (, col))))
		  ;; XEmacs >= 19.12
		  ((fboundp 'valid-color-name-p) (` (valid-color-name-p (, col))))
		  ;; XEmacs 19.11
		  ((fboundp 'x-valid-color-name-p) (` (x-valid-color-name-p (, col))))
		  (t '(error "Cannot implement color-defined-p")))))
      (defmacro cperl-is-face (arg)	; Takes quoted arg
	(cond ((fboundp 'find-face)
	       (` (find-face (, arg))))
	      (;;(and (fboundp 'face-list)
	       ;;	(face-list))
	       (fboundp 'face-list)
	       (` (member (, arg) (and (fboundp 'face-list)
				       (face-list)))))
	      (t
	       (` (boundp (, arg))))))
      (defmacro cperl-make-face (arg descr) ; Takes unquoted arg
	(cond ((fboundp 'make-face)
	       (` (make-face (quote (, arg)))))
	      (t
	       (` (defvar (, arg) (quote (, arg)) (, descr))))))
      (defmacro cperl-force-face (arg descr) ; Takes unquoted arg
	(` (progn
	     (or (cperl-is-face (quote (, arg)))
		 (cperl-make-face (, arg) (, descr)))
	     (or (boundp (quote (, arg))) ; We use unquoted variants too
		 (defvar (, arg) (quote (, arg)) (, descr))))))
      (if cperl-xemacs-p
	  (defmacro cperl-etags-snarf-tag (file line)
	    (` (progn
		 (beginning-of-line 2)
		 (list (, file) (, line)))))
	(defmacro cperl-etags-snarf-tag (file line)
	  (` (etags-snarf-tag))))
      (if cperl-xemacs-p
	  (defmacro cperl-etags-goto-tag-location (elt)
	    (`;;(progn
	     ;; (switch-to-buffer (get-file-buffer (elt (, elt) 0)))
	     ;; (set-buffer (get-file-buffer (elt (, elt) 0)))
	     ;; Probably will not work due to some save-excursion???
	     ;; Or save-file-position?
	     ;; (message "Did I get to line %s?" (elt (, elt) 1))
	     (goto-line (string-to-int (elt (, elt) 1)))))
	;;)
	(defmacro cperl-etags-goto-tag-location (elt)
	  (` (etags-goto-tag-location (, elt)))))))

(defconst cperl-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version))

(defvar cperl-can-font-lock
  (or cperl-xemacs-p
      (and (boundp 'emacs-major-version)
	   (or window-system
	       (> emacs-major-version 20)))))

(condition-case nil
    (require 'custom)
  (error nil))				; Already fixed by eval-when-compile

(defun cperl-choose-color (&rest list)
  (let (answer)
    (while list
      (or answer
	  (if (or (x-color-defined-p (car list))
		  (null (cdr list)))
	      (setq answer (car list))))
      (setq list (cdr list)))
    answer))


(defgroup cperl nil
  "Major mode for editing Perl code."
  :prefix "cperl-"
  :group 'languages)

(defgroup cperl-indentation-details nil
  "Indentation."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-affected-by-hairy nil
  "Variables affected by `cperl-hairy'."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-autoinsert-details nil
  "Auto-insert tuneup."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-faces nil
  "Fontification colors."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-speed nil
  "Speed vs. validity tuneup."
  :prefix "cperl-"
  :group 'cperl)

(defgroup cperl-help-system nil
  "Help system tuneup."
  :prefix "cperl-"
  :group 'cperl)


(defcustom cperl-extra-newline-before-brace nil
  "*Non-nil means that if, elsif, while, until, else, for, foreach
and do constructs look like:

	if ()
	{
	}

instead of:

	if () {
	}"
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-extra-newline-before-brace-multiline
  cperl-extra-newline-before-brace
  "*Non-nil means the same as `cperl-extra-newline-before-brace', but
for constructs with multiline if/unless/while/until/for/foreach condition."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-indent-level 2
  "*Indentation of CPerl statements with respect to containing block."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-lineup-step nil
  "*`cperl-lineup' will always lineup at multiple of this number.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice (const nil) integer)
  :group 'cperl-indentation-details)

(defcustom cperl-brace-imaginary-offset 0
  "*Imagined indentation of a Perl open brace that actually follows a statement.
An open brace following other text is treated as if it were this far
to the right of the start of its line."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-label-offset -2
  "*Offset of CPerl label lines relative to usual indentation."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-min-label-indent 1
  "*Minimal offset of CPerl label lines."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-statement-offset 2
  "*Extra indent for lines not starting new statements."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to cperl-continued-statement-offset."
  :type 'integer
  :group 'cperl-indentation-details)
(defcustom cperl-close-paren-offset -1
  "*Extra indent for substatements that start with close-parenthesis."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-indent-wrt-brace t
  "*Non-nil means indent statements in if/etc block relative brace, not if/etc.
Versions 5.2 ... 5.20 behaved as if this were `nil'."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in CPerl code.  The following
\\[cperl-electric-backspace] will remove the inserted whitespace.
Insertion after colons requires both this variable and
`cperl-auto-newline-after-colon' set."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-autoindent-on-semi nil
  "*Non-nil means automatically indent after insertion of (semi)colon.
Active if `cperl-auto-newline' is false."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-auto-newline-after-colon nil
  "*Non-nil means automatically newline even after colons.
Subject to `cperl-auto-newline' setting."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-tab-always-indent t
  "*Non-nil means TAB in CPerl mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-font-lock nil
  "*Non-nil (and non-null) means CPerl buffers will use `font-lock-mode'.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-lbrace-space nil
  "*Non-nil (and non-null) means { after $ should be preceded by ` '.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens-string "({[]})<"
  "*String of parentheses that should be electric in CPerl.
Closing ones are electric only if the region is highlighted."
  :type 'string
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-parens nil
  "*Non-nil (and non-null) means parentheses should be electric in CPerl.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defvar zmacs-regions)			; Avoid warning

(defcustom cperl-electric-parens-mark
  (and window-system
       (or (and (boundp 'transient-mark-mode) ; For Emacs
		transient-mark-mode)
	   (and (boundp 'zmacs-regions) ; For XEmacs
		zmacs-regions)))
  "*Not-nil means that electric parens look for active mark.
Default is yes if there is visual feedback on mark."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-electric-linefeed nil
  "*If true, LFD should be hairy in CPerl, otherwise C-c LFD is hairy.
In any case these two mean plain and hairy linefeeds together.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-keywords nil
  "*Not-nil (and non-null) means keywords are electric in CPerl.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-electric-backspace-untabify t
  "*Not-nil means electric-backspace will untabify in CPerl."
  :type 'boolean
  :group 'cperl-autoinsert-details)

(defcustom cperl-hairy nil
  "*Not-nil means most of the bells and whistles are enabled in CPerl.
Affects: `cperl-font-lock', `cperl-electric-lbrace-space',
`cperl-electric-parens', `cperl-electric-linefeed', `cperl-electric-keywords',
`cperl-info-on-command-no-prompt', `cperl-clobber-lisp-bindings',
`cperl-lazy-help-time'."
  :type 'boolean
  :group 'cperl-affected-by-hairy)

(defcustom cperl-comment-column 32
  "*Column to put comments in CPerl (use \\[cperl-indent] to lineup with code)."
  :type 'integer
  :group 'cperl-indentation-details)

(defcustom cperl-indent-comment-at-column-0 nil
  "*Non-nil means that comment started at column 0 should be indentable."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-vc-sccs-header '("($sccs) = ('%W\%' =~ /(\\d+(\\.\\d+)+)/) ;")
  "*Special version of `vc-sccs-header' that is used in CPerl mode buffers."
  :type '(repeat string)
  :group 'cperl)

(defcustom cperl-vc-rcs-header '("($rcs) = (' $Id\$ ' =~ /(\\d+(\\.\\d+)+)/);")
  "*Special version of `vc-rcs-header' that is used in CPerl mode buffers."
  :type '(repeat string)
     :group 'cperl)

;; This became obsolete...
(defcustom cperl-vc-header-alist '()
  "*What to use as `vc-header-alist' in CPerl.
Obsolete, with newer Emacsen use `cperl-vc-rcs-header' or
`cperl-vc-sccs-header' instead.  If this list is empty, `vc-header-alist'
will be reconstructed basing on these two variables."
  :type '(repeat (list symbol string))
  :group 'cperl)

(defcustom cperl-clobber-mode-lists
  (not
   (and
    (boundp 'interpreter-mode-alist)
    (assoc "miniperl" interpreter-mode-alist)
    (assoc "\\.\\([pP][Llm]\\|al\\)$" auto-mode-alist)))
  "*Whether to install us into `interpreter-' and `extension' mode lists."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-info-on-command-no-prompt nil
  "*Not-nil (and non-null) means not to prompt on C-h f.
The opposite behaviour is always available if prefixed with C-c.
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-clobber-lisp-bindings nil
  "*Not-nil (and non-null) means not overwrite C-h f.
The function is available on \\[cperl-info-on-command], \\[cperl-get-help].
Can be overwritten by `cperl-hairy' if nil."
  :type '(choice (const null) boolean)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-lazy-help-time nil
  "*Not-nil (and non-null) means to show lazy help after given idle time.
Can be overwritten by `cperl-hairy' to be 5 sec if nil."
  :type '(choice (const null) (const nil) integer)
  :group 'cperl-affected-by-hairy)

(defcustom cperl-pod-face 'font-lock-comment-face
  "*The result of evaluation of this expression is used for POD highlighting."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-pod-head-face 'font-lock-variable-name-face
  "*The result of evaluation of this expression is used for POD highlighting.
Font for POD headers."
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-here-face 'font-lock-string-face
  "*The result of evaluation of this expression is used for here-docs highlighting."
  :type 'face
  :group 'cperl-faces)

;;; Some double-evaluation happened with font-locks...  Needed with 21.2...
(defvar cperl-singly-quote-face cperl-xemacs-p)

(defcustom cperl-invalid-face		; Does not customize with '' on XEmacs
  (if cperl-singly-quote-face
      'underline ''underline) ; On older Emacsen was evaluated by `font-lock'
  (if cperl-singly-quote-face
      "*This face is used for highlighting trailing whitespace."
    "*The result of evaluation of this expression highlights trailing whitespace.")
  :type 'face
  :group 'cperl-faces)

(defcustom cperl-pod-here-fontify '(featurep 'font-lock)
  "*Not-nil after evaluation means to highlight POD and here-docs sections."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-fontify-m-as-s t
  "*Not-nil means highlight 1arg regular expressions operators same as 2arg."
  :type 'boolean
  :group 'cperl-faces)

(defcustom cperl-highlight-variables-indiscriminately nil
  "*Non-nil means perform additional highlighting on variables.
Currently only changes how scalar variables are highlighted.
Note that that variable is only read at initialization time for
the variable `perl-font-lock-keywords-2', so changing it after you've
entered CPerl mode the first time will have no effect."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-pod-here-scan t
  "*Not-nil means look for POD and here-docs sections during startup.
You can always make lookup from menu or using \\[cperl-find-pods-heres]."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-regexp-scan t
  "*Not-nil means make marking of regular expression more thorough.
Effective only with `cperl-pod-here-scan'."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-hook-after-change t
  "*Not-nil means install hook to know which regions of buffer are changed.
May significantly speed up delayed fontification.  Changes take effect
after reload."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-imenu-addback nil
  "*Not-nil means add backreferences to generated `imenu's.
May require patched `imenu' and `imenu-go'.  Obsolete."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-max-help-size 66
  "*Non-nil means shrink-wrapping of info-buffer allowed up to these percents."
  :type '(choice integer (const nil))
  :group 'cperl-help-system)

(defcustom cperl-shrink-wrap-info-frame t
  "*Non-nil means shrink-wrapping of info-buffer-frame allowed."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-info-page "perl"
  "*Name of the info page containing perl docs.
Older version of this page was called `perl5', newer `perl'."
  :type 'string
  :group 'cperl-help-system)

(defcustom cperl-use-syntax-table-text-property
  (boundp 'parse-sexp-lookup-properties)
  "*Non-nil means CPerl sets up and uses `syntax-table' text property."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-use-syntax-table-text-property-for-tags
  cperl-use-syntax-table-text-property
  "*Non-nil means: set up and use `syntax-table' text property generating TAGS."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-scan-files-regexp "\\.\\([pP][Llm]\\|xs\\)$"
  "*Regexp to match files to scan when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-noscan-files-regexp
  "/\\(\\.\\.?\\|SCCS\\|RCS\\|CVS\\|blib\\)$"
  "*Regexp to match files/dirs to skip when generating TAGS."
  :type 'regexp
  :group 'cperl)

(defcustom cperl-regexp-indent-step nil
  "*Indentation used when beautifying regexps.
If nil, the value of `cperl-indent-level' will be used."
  :type '(choice integer (const nil))
  :group 'cperl-indentation-details)

(defcustom cperl-indent-left-aligned-comments t
  "*Non-nil means that the comment starting in leftmost column should indent."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-under-as-char t
  "*Non-nil means that the _ (underline) should be treated as word char."
  :type 'boolean
  :group 'cperl)

(defcustom cperl-extra-perl-args ""
  "*Extra arguments to use when starting Perl.
Currently used with `cperl-check-syntax' only."
  :type 'string
  :group 'cperl)

(defcustom cperl-message-electric-keyword t
  "*Non-nil means that the `cperl-electric-keyword' prints a help message."
  :type 'boolean
  :group 'cperl-help-system)

(defcustom cperl-indent-region-fix-constructs 1
  "*Amount of space to insert between `}' and `else' or `elsif'
in `cperl-indent-region'.  Set to nil to leave as is.  Values other
than 1 and nil will probably not work."
  :type '(choice (const nil) (const 1))
  :group 'cperl-indentation-details)

(defcustom cperl-break-one-line-blocks-when-indent t
  "*Non-nil means that one-line if/unless/while/until/for/foreach BLOCKs
need to be reformatted into multiline ones when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-fix-hanging-brace-when-indent t
  "*Non-nil means that BLOCK-end `}' may be put on a separate line
when indenting a region.
Braces followed by else/elsif/while/until are excepted."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-merge-trailing-else t
  "*Non-nil means that BLOCK-end `}' followed by else/elsif/continue
may be merged to be on the same line when indenting a region."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-indent-parens-as-block nil
  "*Non-nil means that non-block ()-, {}- and []-groups are indented as blocks,
but for trailing \",\" inside the group, which won't increase indentation.
One should tune up `cperl-close-paren-offset' as well."
  :type 'boolean
  :group 'cperl-indentation-details)

(defcustom cperl-syntaxify-by-font-lock
  (and cperl-can-font-lock
       (boundp 'parse-sexp-lookup-properties))
  "*Non-nil means that CPerl uses `font-lock's routines for syntaxification."
  :type '(choice (const message) boolean)
  :group 'cperl-speed)

(defcustom cperl-syntaxify-unwind
  t
  "*Non-nil means that CPerl unwinds to a start of a long construction
when syntaxifying a chunk of buffer."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-syntaxify-for-menu
  t
  "*Non-nil means that CPerl syntaxifies up to the point before showing menu.
This way enabling/disabling of menu items is more correct."
  :type 'boolean
  :group 'cperl-speed)

(defcustom cperl-ps-print-face-properties
  '((font-lock-keyword-face		nil nil		bold shadow)
    (font-lock-variable-name-face	nil nil		bold)
    (font-lock-function-name-face	nil nil		bold italic box)
    (font-lock-constant-face		nil "LightGray"	bold)
    (cperl-array-face			nil "LightGray"	bold underline)
    (cperl-hash-face			nil "LightGray"	bold italic underline)
    (font-lock-comment-face		nil "LightGray"	italic)
    (font-lock-string-face		nil nil		italic underline)
    (cperl-nonoverridable-face		nil nil		italic underline)
    (font-lock-type-face		nil nil		underline)
    (font-lock-warning-face		nil "LightGray"	bold italic box)
    (underline				nil "LightGray"	strikeout))
  "List given as an argument to `ps-extend-face-list' in `cperl-ps-print'."
  :type '(repeat (cons symbol
		       (cons (choice (const nil) string)
			     (cons (choice (const nil) string)
				   (repeat symbol)))))
  :group 'cperl-faces)

(if cperl-can-font-lock
    (progn
      (defvar cperl-dark-background
	(cperl-choose-color "navy" "os2blue" "darkgreen"))
      (defvar cperl-dark-foreground
	(cperl-choose-color "orchid1" "orange"))

      (defface cperl-nonoverridable-face
	(` ((((class grayscale) (background light))
	     (:background "Gray90" :italic t :underline t))
	    (((class grayscale) (background dark))
	     (:foreground "Gray80" :italic t :underline t :bold t))
	    (((class color) (background light))
	     (:foreground "chartreuse3"))
	    (((class color) (background dark))
	     (:foreground (, cperl-dark-foreground)))
	    (t (:bold t :underline t))))
	"Font Lock mode face used to highlight array names."
	:group 'cperl-faces)

      (defface cperl-array-face
	(` ((((class grayscale) (background light))
	     (:background "Gray90" :bold t))
	    (((class grayscale) (background dark))
	     (:foreground "Gray80" :bold t))
	    (((class color) (background light))
	     (:foreground "Blue" :background "lightyellow2" :bold t))
	    (((class color) (background dark))
	     (:foreground "yellow" :background (, cperl-dark-background) :bold t))
	    (t (:bold t))))
	"Font Lock mode face used to highlight array names."
	:group 'cperl-faces)

      (defface cperl-hash-face
	(` ((((class grayscale) (background light))
	     (:background "Gray90" :bold t :italic t))
	    (((class grayscale) (background dark))
	     (:foreground "Gray80" :bold t :italic t))
	    (((class color) (background light))
	     (:foreground "Red" :background "lightyellow2" :bold t :italic t))
	    (((class color) (background dark))
	     (:foreground "Red" :background (, cperl-dark-background) :bold t :italic t))
	    (t (:bold t :italic t))))
	"Font Lock mode face used to highlight hash names."
	:group 'cperl-faces)))



;;; Short extra-docs.

(defvar cperl-tips 'please-ignore-this-line
  "Get maybe newer version of this package from
  http://ilyaz.org/software/emacs
Subdirectory `cperl-mode' may contain yet newer development releases and/or
patches to related files.

For best results apply to an older Emacs the patches from
  ftp://ftp.math.ohio-state.edu/pub/users/ilya/cperl-mode/patches
\(this upgrades syntax-parsing abilities of RMS Emaxen v19.34 and
v20.2 up to the level of RMS Emacs v20.3 - a must for a good Perl
mode.)  As of beginning of 2003, XEmacs may provide a similar ability.

Get support packages choose-color.el (or font-lock-extra.el before
19.30), imenu-go.el from the same place.  \(Look for other files there
too... ;-).  Get a patch for imenu.el in 19.29.  Note that for 19.30 and
later you should use choose-color.el *instead* of font-lock-extra.el
\(and you will not get smart highlighting in C :-().

Note that to enable Compile choices in the menu you need to install
mode-compile.el.

If your Emacs does not default to `cperl-mode' on Perl files, and you
want it to: put the following into your .emacs file:

  (autoload 'perl-mode \"cperl-mode\" \"alternate mode for editing Perl programs\" t)

or

  (defalias 'perl-mode 'cperl-mode)

Get perl5-info from
  $CPAN/doc/manual/info/perl5-old/perl5-info.tar.gz
Also, one can generate a newer documentation running `pod2texi' converter
  $CPAN/doc/manual/info/perl5/pod2texi-0.1.tar.gz

If you use imenu-go, run imenu on perl5-info buffer (you can do it
from Perl menu).  If many files are related, generate TAGS files from
Tools/Tags submenu in Perl menu.

If some class structure is too complicated, use Tools/Hierarchy-view
from Perl menu, or hierarchic view of imenu.  The second one uses the
current buffer only, the first one requires generation of TAGS from
Perl/Tools/Tags menu beforehand.

Run Perl/Tools/Insert-spaces-if-needed to fix your lazy typing.

Switch auto-help on/off with Perl/Tools/Auto-help.

Though with contemporary Emaxen CPerl mode should maintain the correct
parsing of Perl even when editing, sometimes it may be lost.  Fix this by

  M-x norm RET

or

  \\[normal-mode]

In cases of more severe confusion sometimes it is helpful to do

  M-x load-l RET cperl-mode RET
  M-x norm RET

or

  \\[load-library] cperl-mode RET
  \\[normal-mode]

Before reporting (non-)problems look in the problem section of online
micro-docs on what I know about CPerl problems.")

(defvar cperl-problems 'please-ignore-this-line
  "Description of problems in CPerl mode.
Some faces will not be shown on some versions of Emacs unless you
install choose-color.el, available from
  http://ilyaz.org/software/emacs

`fill-paragraph' on a comment may leave the point behind the
paragraph.  It also triggers a bug in some versions of Emacs (CPerl tries
to detect it and bulk out).

See documentation of a variable `cperl-problems-old-emaxen' for the
problems which disappear if you upgrade Emacs to a reasonably new
version (20.3 for RMS Emacs, and those of 2004 for XEmacs).")

(defvar cperl-problems-old-emaxen 'please-ignore-this-line
  "Description of problems in CPerl mode specific for older Emacs versions.

Emacs had a _very_ restricted syntax parsing engine until RMS's Emacs
20.1.  Most problems below are corrected starting from this version of
Emacs, and all of them should be fixed in RMS's version 20.3.  (Or apply
patches to Emacs 19.33/34 - see tips.)  XEmacs was very backward in
this respect (until 2003).

Note that even with newer Emacsen in some very rare cases the details
of interaction of `font-lock' and syntaxification may be not cleaned
up yet.  You may get slightly different colors basing on the order of
fontification and syntaxification.  Say, the initial faces is correct,
but editing the buffer breaks this.

Even with older Emacsen CPerl mode tries to corrects some Emacs
misunderstandings, however, for efficiency reasons the degree of
correction is different for different operations.  The partially
corrected problems are: POD sections, here-documents, regexps.  The
operations are: highlighting, indentation, electric keywords, electric
braces.

This may be confusing, since the regexp s#//#/#\; may be highlighted
as a comment, but it will be recognized as a regexp by the indentation
code.  Or the opposite case, when a POD section is highlighted, but
may break the indentation of the following code (though indentation
should work if the balance of delimiters is not broken by POD).

The main trick (to make $ a \"backslash\") makes constructions like
${aaa} look like unbalanced braces.  The only trick I can think of is
to insert it as $ {aaa} (legal in perl5, not in perl4).

Similar problems arise in regexps, when /(\\s|$)/ should be rewritten
as /($|\\s)/.  Note that such a transposition is not always possible.

The solution is to upgrade your Emacs or patch an older one.  Note
that RMS's 20.2 has some bugs related to `syntax-table' text
properties.  Patches are available on the main CPerl download site,
and on CPAN.

If these bugs cannot be fixed on your machine (say, you have an inferior
environment and cannot recompile), you may still disable all the fancy stuff
via `cperl-use-syntax-table-text-property'.")

(defvar cperl-non-problems 'please-ignore-this-line
"As you know from `problems' section, Perl syntax is too hard for CPerl on
older Emacsen.  Here is what you can do if you cannot upgrade, or if
you want to switch off these capabilities on RMS Emacs 20.2 (+patches) or 20.3
or better.  Please skip this docs if you run a capable Emacs already.

Most of the time, if you write your own code, you may find an equivalent
\(and almost as readable) expression (what is discussed below is usually
not relevant on newer Emacsen, since they can do it automatically).

Try to help CPerl: add comments with embedded quotes to fix CPerl
misunderstandings about the end of quotation:

$a='500$';      # ';

You won't need it too often.  The reason: $ \"quotes\" the following
character (this saves a life a lot of times in CPerl), thus due to
Emacs parsing rules it does not consider tick (i.e., ' ) after a
dollar as a closing one, but as a usual character.  This is usually
correct, but not in the above context.

Even with older Emacsen the indentation code is pretty wise.  The only
drawback is that it relied on Emacs parsing to find matching
parentheses.  And Emacs *could not* match parentheses in Perl 100%
correctly.  So
	1 if s#//#/#;
would not break indentation, but
	1 if ( s#//#/# );
would.  Upgrade.

By similar reasons
	s\"abc\"def\";
could confuse CPerl a lot.

If you still get wrong indentation in situation that you think the
code should be able to parse, try:

a) Check what Emacs thinks about balance of your parentheses.
b) Supply the code to me (IZ).

Pods were treated _very_ rudimentally.  Here-documents were not
treated at all (except highlighting and inhibiting indentation).  Upgrade.

To speed up coloring the following compromises exist:
   a) sub in $mypackage::sub may be highlighted.
   b) -z in [a-z] may be highlighted.
   c) if your regexp contains a keyword (like \"s\"), it may be highlighted.


Imenu in 19.31 is broken.  Set `imenu-use-keymap-menu' to t, and remove
`car' before `imenu-choose-buffer-index' in `imenu'.
`imenu-add-to-menubar' in 20.2 is broken.
A lot of things on XEmacs may be broken too, judging by bug reports I
receive.  Note that some releases of XEmacs are better than the others
as far as bugs reports I see are concerned.")

(defvar cperl-praise 'please-ignore-this-line
  "Advantages of CPerl mode.

0) It uses the newest `syntax-table' property ;-);

1) It does 99% of Perl syntax correct (as opposed to 80-90% in Perl
mode - but the latter number may have improved too in last years) even
with old Emaxen which do not support `syntax-table' property.

When using `syntax-table' property for syntax assist hints, it should
handle 99.995% of lines correct - or somesuch.  It automatically
updates syntax assist hints when you edit your script.

2) It is generally believed to be \"the most user-friendly Emacs
package\" whatever it may mean (I doubt that the people who say similar
things tried _all_ the rest of Emacs ;-), but this was not a lonely
voice);

3) Everything is customizable, one-by-one or in a big sweep;

4) It has many easily-accessable \"tools\":
        a) Can run program, check syntax, start debugger;
        b) Can lineup vertically \"middles\" of rows, like `=' in
                a  = b;
                cc = d;
        c) Can insert spaces where this impoves readability (in one
                interactive sweep over the buffer);
        d) Has support for imenu, including:
                1) Separate unordered list of \"interesting places\";
                2) Separate TOC of POD sections;
                3) Separate list of packages;
                4) Hierarchical view of methods in (sub)packages;
                5) and functions (by the full name - with package);
        e) Has an interface to INFO docs for Perl; The interface is
                very flexible, including shrink-wrapping of
                documentation buffer/frame;
        f) Has a builtin list of one-line explanations for perl constructs.
        g) Can show these explanations if you stay long enough at the
                corresponding place (or on demand);
        h) Has an enhanced fontification (using 3 or 4 additional faces
                comparing to font-lock - basically, different
                namespaces in Perl have different colors);
        i) Can construct TAGS basing on its knowledge of Perl syntax,
                the standard menu has 6 different way to generate
                TAGS (if \"by directory\", .xs files - with C-language
                bindings - are included in the scan);
        j) Can build a hierarchical view of classes (via imenu) basing
                on generated TAGS file;
        k) Has electric parentheses, electric newlines, uses Abbrev
                for electric logical constructs
                        while () {}
                with different styles of expansion (context sensitive
                to be not so bothering).  Electric parentheses behave
                \"as they should\" in a presence of a visible region.
        l) Changes msb.el \"on the fly\" to insert a group \"Perl files\";
        m) Can convert from
		if (A) { B }
	   to
		B if A;

        n) Highlights (by user-choice) either 3-delimiters constructs
	   (such as tr/a/b/), or regular expressions and `y/tr';
	o) Highlights trailing whitespace;
	p) Is able to manipulate Perl Regular Expressions to ease
	   conversion to a more readable form.
        q) Can ispell POD sections and HERE-DOCs.
	r) Understands comments and character classes inside regular
	   expressions; can find matching () and [] in a regular expression.
	s) Allows indentation of //x-style regular expressions;
	t) Highlights different symbols in regular expressions according
	   to their function; much less problems with backslashitis;
	u) Allows to find regular expressions which contain interpolated parts.

5) The indentation engine was very smart, but most of tricks may be
not needed anymore with the support for `syntax-table' property.  Has
progress indicator for indentation (with `imenu' loaded).

6) Indent-region improves inline-comments as well; also corrects
whitespace *inside* the conditional/loop constructs.

7) Fill-paragraph correctly handles multi-line comments;

8) Can switch to different indentation styles by one command, and restore
the settings present before the switch.

9) When doing indentation of control constructs, may correct
line-breaks/spacing between elements of the construct.

10) Uses a linear-time algorith for indentation of regions (on Emaxen with
capable syntax engines).

11) Syntax-highlight, indentation, sexp-recognition inside regular expressions.
")

(defvar cperl-speed 'please-ignore-this-line
  "This is an incomplete compendium of what is available in other parts
of CPerl documentation.  (Please inform me if I skept anything.)

There is a perception that CPerl is slower than alternatives.  This part
of documentation is designed to overcome this misconception.

*By default* CPerl tries to enable the most comfortable settings.
From most points of view, correctly working package is infinitely more
comfortable than a non-correctly working one, thus by default CPerl
prefers correctness over speed.  Below is the guide how to change
settings if your preferences are different.

A)  Speed of loading the file.  When loading file, CPerl may perform a
scan which indicates places which cannot be parsed by primitive Emacs
syntax-parsing routines, and marks them up so that either

    A1) CPerl may work around these deficiencies (for big chunks, mostly
        PODs and HERE-documents), or
    A2) On capable Emaxen CPerl will use improved syntax-handlings
	which reads mark-up hints directly.

    The scan in case A2 is much more comprehensive, thus may be slower.

    User can disable syntax-engine-helping scan of A2 by setting
       `cperl-use-syntax-table-text-property'
    variable to nil (if it is set to t).

    One can disable the scan altogether (both A1 and A2) by setting
       `cperl-pod-here-scan'
    to nil.

B) Speed of editing operations.

    One can add a (minor) speedup to editing operations by setting
       `cperl-use-syntax-table-text-property'
    variable to nil (if it is set to t).  This will disable
    syntax-engine-helping scan, thus will make many more Perl
    constructs be wrongly recognized by CPerl, thus may lead to
    wrongly matched parentheses, wrong indentation, etc.

    One can unset `cperl-syntaxify-unwind'.  This might speed up editing
    of, say, long POD sections.")

(defvar cperl-tips-faces 'please-ignore-this-line
  "CPerl mode uses following faces for highlighting:

  `cperl-array-face'		Array names
  `cperl-hash-face'		Hash names
  `font-lock-comment-face'	Comments, PODs and whatever is considered
				syntaxically to be not code
  `font-lock-constant-face'	HERE-doc delimiters, labels, delimiters of
				2-arg operators s/y/tr/ or of RExen,
  `font-lock-warning-face'	Special-cased m// and s//foo/,
  `font-lock-function-name-face' _ as a target of a file tests, file tests,
				subroutine names at the moment of definition
				(except those conflicting with Perl operators),
				package names (when recognized), format names
  `font-lock-keyword-face'	Control flow switch constructs, declarators
  `cperl-nonoverridable-face'	Non-overridable keywords, modifiers of RExen
  `font-lock-string-face'	Strings, qw() constructs, RExen, POD sections,
				literal parts and the terminator of formats
				and whatever is syntaxically considered
				as string literals
  `font-lock-type-face'		Overridable keywords
  `font-lock-variable-name-face' Variable declarations, indirect array and
				hash names, POD headers/item names
  `cperl-invalid-face'		Trailing whitespace

Note that in several situations the highlighting tries to inform about
possible confusion, such as different colors for function names in
declarations depending on what they (do not) override, or special cases
m// and s/// which do not do what one would expect them to do.

Help with best setup of these faces for printout requested (for each of
the faces: please specify bold, italic, underline, shadow and box.)

In regular expressions (except character classes):
  `font-lock-string-face'	\"Normal\" stuff and non-0-length constructs
  `font-lock-constant-face':	Delimiters
  `font-lock-warning-face'	Special-cased m// and s//foo/,
				Mismatched closing delimiters, parens
				we couldn't match, misplaced quantifiers,
				unrecognized escape sequences
  `cperl-nonoverridable-face'	Modifiers, as gism in m/REx/gism
  `font-lock-type-face'		POSIX classes inside charclasses,
				escape sequences with arguments (\x \23 \p \N)
				and others match-a-char escape sequences
  `font-lock-keyword-face'	Capturing parens, and |
  `font-lock-function-name-face' Special symbols: $ ^ . [ ] [^ ] (?{ }) (??{ })
  `font-lock-builtin-face'	\"Remaining\" 0-length constructs, executable
				parts of a REx, not-capturing parens
  `font-lock-variable-name-face' Interpolated constructs, embedded code
  `font-lock-comment-face'	Embedded comments

")



;;; Portability stuff:

(defmacro cperl-define-key (emacs-key definition &optional xemacs-key)
  (` (define-key cperl-mode-map
       (, (if xemacs-key
	      (` (if cperl-xemacs-p (, xemacs-key) (, emacs-key)))
	    emacs-key))
       (, definition))))

(defvar cperl-del-back-ch
  (car (append (where-is-internal 'delete-backward-char)
	       (where-is-internal 'backward-delete-char-untabify)))
  "Character generated by key bound to `delete-backward-char'.")

(and (vectorp cperl-del-back-ch) (= (length cperl-del-back-ch) 1)
     (setq cperl-del-back-ch (aref cperl-del-back-ch 0)))

(defun cperl-mark-active () (mark))	; Avoid undefined warning
(if cperl-xemacs-p
    (progn
      ;; "Active regions" are on: use region only if active
      ;; "Active regions" are off: use region unconditionally
      (defun cperl-use-region-p ()
	(if zmacs-regions (mark) t)))
  (defun cperl-use-region-p ()
    (if transient-mark-mode mark-active t))
  (defun cperl-mark-active () mark-active))

(defsubst cperl-enable-font-lock ()
  cperl-can-font-lock)

(defun cperl-putback-char (c)		; Emacs 19
  (set 'unread-command-events (list c))) ; Avoid undefined warning

(if (boundp 'unread-command-events)
    (if cperl-xemacs-p
	(defun cperl-putback-char (c)	; XEmacs >= 19.12
	  (setq unread-command-events (list (eval '(character-to-event c))))))
  (defun cperl-putback-char (c)		; XEmacs <= 19.11
    (set 'unread-command-event (eval '(character-to-event c))))) ; Avoid warnings

(or (fboundp 'uncomment-region)
    (defun uncomment-region (beg end)
      (interactive "r")
      (comment-region beg end -1)))

(defvar cperl-do-not-fontify
  (if (string< emacs-version "19.30")
      'fontified
    'lazy-lock)
  "Text property which inhibits refontification.")

(defsubst cperl-put-do-not-fontify (from to &optional post)
  ;; If POST, do not do it with postponed fontification
  (if (and post cperl-syntaxify-by-font-lock)
      nil
    (put-text-property (max (point-min) (1- from))
		       to cperl-do-not-fontify t)))

(defcustom cperl-mode-hook nil
  "Hook run by CPerl mode."
  :type 'hook
  :group 'cperl)

(defvar cperl-syntax-state nil)
(defvar cperl-syntax-done-to nil)
(defvar cperl-emacs-can-parse (> (length (save-excursion
					   (parse-partial-sexp (point) (point)))) 9))

;; Make customization possible "in reverse"
(defsubst cperl-val (symbol &optional default hairy)
  (cond
   ((eq (symbol-value symbol) 'null) default)
   (cperl-hairy (or hairy t))
   (t (symbol-value symbol))))


(defun cperl-make-indent (column &optional minimum keep)
  "Makes indent of the current line the requested amount.
Unless KEEP, removes the old indentation.  Works around a bug in ancient
versions of Emacs."
  (let ((prop (get-text-property (point) 'syntax-type)))
    (or keep
	(delete-horizontal-space))
    (indent-to column minimum)
    ;; In old versions (e.g., 19.33) `indent-to' would not inherit properties
    (and prop
	 (> (current-column) 0)
	 (save-excursion
	   (beginning-of-line)
	   (or (get-text-property (point) 'syntax-type)
	       (and (looking-at "\\=[ \t]")
		      (put-text-property (point) (match-end 0)
					 'syntax-type prop)))))))

;;; Probably it is too late to set these guys already, but it can help later:

(and cperl-clobber-mode-lists
     (setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))
     (and (boundp 'interpreter-mode-alist)
	  (setq interpreter-mode-alist (append interpreter-mode-alist
					       '(("miniperl" . perl-mode))))))
(if (fboundp 'eval-when-compile)
    (eval-when-compile
      (mapcar (lambda (p)
		(condition-case nil
		    (require p)
		  (error nil)))
	      '(imenu easymenu etags timer man info))
      (if (fboundp 'ps-extend-face-list)
	  (defmacro cperl-ps-extend-face-list (arg)
	    (` (ps-extend-face-list (, arg))))
	(defmacro cperl-ps-extend-face-list (arg)
	  (` (error "This version of Emacs has no `ps-extend-face-list'"))))
      ;; Calling `cperl-enable-font-lock' below doesn't compile on XEmacs,
      ;; macros instead of defsubsts don't work on Emacs, so we do the
      ;; expansion manually.  Any other suggestions?
      (if cperl-can-font-lock
	  (require 'font-lock))
      (require 'cl)))

(defvar cperl-mode-abbrev-table nil
  "Abbrev table in use in CPerl mode buffers.")

(add-hook 'edit-var-mode-alist '(perl-mode (regexp . "^cperl-")))

(defvar cperl-mode-map () "Keymap used in CPerl mode.")

(if cperl-mode-map nil
  (setq cperl-mode-map (make-sparse-keymap))
  (cperl-define-key "{" 'cperl-electric-lbrace)
  (cperl-define-key "[" 'cperl-electric-paren)
  (cperl-define-key "(" 'cperl-electric-paren)
  (cperl-define-key "<" 'cperl-electric-paren)
  (cperl-define-key "}" 'cperl-electric-brace)
  (cperl-define-key "]" 'cperl-electric-rparen)
  (cperl-define-key ")" 'cperl-electric-rparen)
  (cperl-define-key ";" 'cperl-electric-semi)
  (cperl-define-key ":" 'cperl-electric-terminator)
  (cperl-define-key "\C-j" 'newline-and-indent)
  (cperl-define-key "\C-c\C-j" 'cperl-linefeed)
  (cperl-define-key "\C-c\C-t" 'cperl-invert-if-unless)
  (cperl-define-key "\C-c\C-a" 'cperl-toggle-auto-newline)
  (cperl-define-key "\C-c\C-k" 'cperl-toggle-abbrev)
  (cperl-define-key "\C-c\C-w" 'cperl-toggle-construct-fix)
  (cperl-define-key "\C-c\C-f" 'auto-fill-mode)
  (cperl-define-key "\C-c\C-e" 'cperl-toggle-electric)
  (cperl-define-key "\C-c\C-b" 'cperl-find-bad-style)
  (cperl-define-key "\C-c\C-p" 'cperl-pod-spell)
  (cperl-define-key "\C-c\C-d" 'cperl-here-doc-spell)
  (cperl-define-key "\C-c\C-n" 'cperl-narrow-to-here-doc)
  (cperl-define-key "\C-c\C-v" 'cperl-next-interpolated-REx)
  (cperl-define-key "\C-c\C-x" 'cperl-next-interpolated-REx-0)
  (cperl-define-key "\C-c\C-y" 'cperl-next-interpolated-REx-1)
  (cperl-define-key "\C-c\C-ha" 'cperl-toggle-autohelp)
  (cperl-define-key "\C-c\C-hp" 'cperl-perldoc)
  (cperl-define-key "\C-c\C-hP" 'cperl-perldoc-at-point)
  (cperl-define-key "\e\C-q" 'cperl-indent-exp) ; Usually not bound
  (cperl-define-key [?\C-\M-\|] 'cperl-lineup
		    [(control meta |)])
  ;;(cperl-define-key "\M-q" 'cperl-fill-paragraph)
  ;;(cperl-define-key "\e;" 'cperl-indent-for-comment)
  (cperl-define-key "\177" 'cperl-electric-backspace)
  (cperl-define-key "\t" 'cperl-indent-command)
  ;; don't clobber the backspace binding:
  (cperl-define-key "\C-c\C-hF" 'cperl-info-on-command
		    [(control c) (control h) F])
  (if (cperl-val 'cperl-clobber-lisp-bindings)
      (progn
	(cperl-define-key "\C-hf"
			  ;;(concat (char-to-string help-char) "f") ; does not work
			  'cperl-info-on-command
			  [(control h) f])
	(cperl-define-key "\C-hv"
			  ;;(concat (char-to-string help-char) "v") ; does not work
			  'cperl-get-help
			  [(control h) v])
	(cperl-define-key "\C-c\C-hf"
			  ;;(concat (char-to-string help-char) "f") ; does not work
			  (key-binding "\C-hf")
			  [(control c) (control h) f])
	(cperl-define-key "\C-c\C-hv"
			  ;;(concat (char-to-string help-char) "v") ; does not work
			  (key-binding "\C-hv")
			  [(control c) (control h) v]))
    (cperl-define-key "\C-c\C-hf" 'cperl-info-on-current-command
		      [(control c) (control h) f])
    (cperl-define-key "\C-c\C-hv"
		      ;;(concat (char-to-string help-char) "v") ; does not work
		      'cperl-get-help
		      [(control c) (control h) v]))
  (if (and cperl-xemacs-p
	   (<= emacs-minor-version 11) (<= emacs-major-version 19))
      (progn
	;; substitute-key-definition is usefulness-deenhanced...
	;;;;;(cperl-define-key "\M-q" 'cperl-fill-paragraph)
	(cperl-define-key "\e;" 'cperl-indent-for-comment)
	(cperl-define-key "\e\C-\\" 'cperl-indent-region))
    (or (boundp 'fill-paragraph-function)
	(substitute-key-definition
	 'fill-paragraph 'cperl-fill-paragraph
	 cperl-mode-map global-map))
    (substitute-key-definition
     'indent-sexp 'cperl-indent-exp
     cperl-mode-map global-map)
    (substitute-key-definition
     'indent-region 'cperl-indent-region
     cperl-mode-map global-map)
    (substitute-key-definition
     'indent-for-comment 'cperl-indent-for-comment
     cperl-mode-map global-map)))

(defvar cperl-menu)
(defvar cperl-lazy-installed)
(defvar cperl-old-style nil)
(condition-case nil
    (progn
      (require 'easymenu)
      (easy-menu-define
       cperl-menu cperl-mode-map "Menu for CPerl mode"
       '("Perl"
	 ["Beginning of function" beginning-of-defun t]
	 ["End of function" end-of-defun t]
	 ["Mark function" mark-defun t]
	 ["Indent expression" cperl-indent-exp t]
	 ["Fill paragraph/comment" cperl-fill-paragraph t]
	 "----"
	 ["Line up a construction" cperl-lineup (cperl-use-region-p)]
	 ["Invert if/unless/while etc" cperl-invert-if-unless t]
	 ("Regexp"
	  ["Beautify" cperl-beautify-regexp
	   cperl-use-syntax-table-text-property]
	  ["Beautify one level deep" (cperl-beautify-regexp 1)
	   cperl-use-syntax-table-text-property]
	  ["Beautify a group" cperl-beautify-level
	   cperl-use-syntax-table-text-property]
	  ["Beautify a group one level deep" (cperl-beautify-level 1)
	   cperl-use-syntax-table-text-property]
	  ["Contract a group" cperl-contract-level
	   cperl-use-syntax-table-text-property]
	  ["Contract groups" cperl-contract-levels
	   cperl-use-syntax-table-text-property]
	  "----"
	  ["Find next interpolated" cperl-next-interpolated-REx 
	   (next-single-property-change (point-min) 'REx-interpolated)]
	  ["Find next interpolated (no //o)"
	   cperl-next-interpolated-REx-0
	   (or (text-property-any (point-min) (point-max) 'REx-interpolated t)
	       (text-property-any (point-min) (point-max) 'REx-interpolated 1))]
	  ["Find next interpolated (neither //o nor whole-REx)"
	   cperl-next-interpolated-REx-1
	   (text-property-any (point-min) (point-max) 'REx-interpolated t)])
	 ["Insert spaces if needed to fix style" cperl-find-bad-style t]
	 ["Refresh \"hard\" constructions" cperl-find-pods-heres t]
	 "----"
	 ["Indent region" cperl-indent-region (cperl-use-region-p)]
	 ["Comment region" cperl-comment-region (cperl-use-region-p)]
	 ["Uncomment region" cperl-uncomment-region (cperl-use-region-p)]
	 "----"
	 ["Run" mode-compile (fboundp 'mode-compile)]
	 ["Kill" mode-compile-kill (and (fboundp 'mode-compile-kill)
					(get-buffer "*compilation*"))]
	 ["Next error" next-error (get-buffer "*compilation*")]
	 ["Check syntax" cperl-check-syntax (fboundp 'mode-compile)]
	 "----"
	 ["Debugger" cperl-db t]
	 "----"
	 ("Tools"
	  ["Imenu" imenu (fboundp 'imenu)]
	  ["Imenu on Perl Info" cperl-imenu-on-info (featurep 'imenu)]
	  "----"
	  ["Ispell PODs" cperl-pod-spell
	   ;; Better not to update syntaxification here:
	   ;; debugging syntaxificatio can be broken by this???
	   (or
	    (get-text-property (point-min) 'in-pod)
	    (< (progn
		 (and cperl-syntaxify-for-menu
		      (cperl-update-syntaxification (point-max) (point-max)))
		 (next-single-property-change (point-min) 'in-pod nil (point-max)))
	       (point-max)))]
	  ["Ispell HERE-DOCs" cperl-here-doc-spell
	   (< (progn
		(and cperl-syntaxify-for-menu
		     (cperl-update-syntaxification (point-max) (point-max)))
		(next-single-property-change (point-min) 'here-doc-group nil (point-max)))
	      (point-max))]
	  ["Narrow to this HERE-DOC" cperl-narrow-to-here-doc
	   (eq 'here-doc  (progn
		(and cperl-syntaxify-for-menu
		     (cperl-update-syntaxification (point) (point)))
		(get-text-property (point) 'syntax-type)))]
	  ["Select this HERE-DOC or POD section"
	   cperl-select-this-pod-or-here-doc
	   (memq (progn
		   (and cperl-syntaxify-for-menu
			(cperl-update-syntaxification (point) (point)))
		   (get-text-property (point) 'syntax-type))
		 '(here-doc pod))]
	  "----"
	  ["CPerl pretty print (exprmntl)" cperl-ps-print
	   (fboundp 'ps-extend-face-list)]
	  "----"
	  ["Syntaxify region" cperl-find-pods-heres-region
	   (cperl-use-region-p)]
	  ["Profile syntaxification" cperl-time-fontification t]
	  ["Debug errors in delayed fontification" cperl-emulate-lazy-lock t]
	  ["Debug unwind for syntactic scan" cperl-toggle-set-debug-unwind t]
	  ["Debug backtrace on syntactic scan (BEWARE!!!)"
	   (cperl-toggle-set-debug-unwind nil t) t]
	  "----"
	  ["Class Hierarchy from TAGS" cperl-tags-hier-init t]
	  ;;["Update classes" (cperl-tags-hier-init t) tags-table-list]
	  ("Tags"
;;;	     ["Create tags for current file" cperl-etags t]
;;;	     ["Add tags for current file" (cperl-etags t) t]
;;;	     ["Create tags for Perl files in directory" (cperl-etags nil t) t]
;;;	     ["Add tags for Perl files in directory" (cperl-etags t t) t]
;;;	     ["Create tags for Perl files in (sub)directories"
;;;	      (cperl-etags nil 'recursive) t]
;;;	     ["Add tags for Perl files in (sub)directories"
;;;	      (cperl-etags t 'recursive) t])
;;;; cperl-write-tags (&optional file erase recurse dir inbuffer)
	   ["Create tags for current file" (cperl-write-tags nil t) t]
	   ["Add tags for current file" (cperl-write-tags) t]
	   ["Create tags for Perl files in directory"
	    (cperl-write-tags nil t nil t) t]
	   ["Add tags for Perl files in directory"
	    (cperl-write-tags nil nil nil t) t]
	   ["Create tags for Perl files in (sub)directories"
	    (cperl-write-tags nil t t t) t]
	   ["Add tags for Perl files in (sub)directories"
	    (cperl-write-tags nil nil t t) t]))
	 ("Perl docs"
	  ["Define word at point" imenu-go-find-at-position
	   (fboundp 'imenu-go-find-at-position)]
	  ["Help on function" cperl-info-on-command t]
	  ["Help on function at point" cperl-info-on-current-command t]
	  ["Help on symbol at point" cperl-get-help t]
	  ["Perldoc" cperl-perldoc t]
	  ["Perldoc on word at point" cperl-perldoc-at-point t]
	  ["View manpage of POD in this file" cperl-build-manpage t]
	  ["Auto-help on" cperl-lazy-install
	   (and (fboundp 'run-with-idle-timer)
		(not cperl-lazy-installed))]
	  ["Auto-help off" cperl-lazy-unstall
	   (and (fboundp 'run-with-idle-timer)
		cperl-lazy-installed)])
	 ("Toggle..."
	  ["Auto newline" cperl-toggle-auto-newline t]
	  ["Electric parens" cperl-toggle-electric t]
	  ["Electric keywords" cperl-toggle-abbrev t]
	  ["Fix whitespace on indent" cperl-toggle-construct-fix t]
	  ["Auto-help on Perl constructs" cperl-toggle-autohelp t]
	  ["Auto fill" auto-fill-mode t])
	 ("Indent styles..."
	  ["CPerl" (cperl-set-style "CPerl") t]
	  ["PerlStyle" (cperl-set-style "PerlStyle") t]
	  ["GNU" (cperl-set-style "GNU") t]
	  ["C++" (cperl-set-style "C++") t]
	  ["K&R" (cperl-set-style "K&R") t]
	  ["BSD" (cperl-set-style "BSD") t]
	  ["Whitesmith" (cperl-set-style "Whitesmith") t]
	  ["Memorize Current" (cperl-set-style "Current") t]
	  ["Memorized" (cperl-set-style-back) cperl-old-style])
	 ("Micro-docs"
	  ["Tips" (describe-variable 'cperl-tips) t]
	  ["Problems" (describe-variable 'cperl-problems) t]
	  ["Non-problems" (describe-variable 'cperl-non-problems) t]
	  ["Speed" (describe-variable 'cperl-speed) t]
	  ["Praise" (describe-variable 'cperl-praise) t]
	  ["Faces" (describe-variable 'cperl-tips-faces) t]
	  ["CPerl mode" (describe-function 'cperl-mode) t]
	  ["CPerl version"
	   (message "The version of master-file for this CPerl is %s"
		    cperl-version) t]))))
  (error nil))

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

;;; These two must be unwound, otherwise take exponential time
(defconst cperl-maybe-white-and-comment-rex "[ \t\n]*\\(#[^\n]*\n[ \t\n]*\\)*"
"Regular expression to match optional whitespace with interpspersed comments.
Should contain exactly one group.")

;;; This one is tricky to unwind; still very inefficient...
(defconst cperl-white-and-comment-rex "\\([ \t\n]\\|#[^\n]*\n\\)+"
"Regular expression to match whitespace with interpspersed comments.
Should contain exactly one group.")


;;; Is incorporated in `cperl-imenu--function-name-regexp-perl'
;;; `cperl-outline-regexp', `defun-prompt-regexp'.
;;; Details of groups in this may be used in several functions; see comments
;;; near mentioned above variable(s)...
;;; sub($$):lvalue{}  sub:lvalue{} Both allowed...
(defsubst cperl-after-sub-regexp (named attr) ; 9 groups without attr...
  "Match the text after `sub' in a subroutine declaration.
If NAMED is nil, allows anonymous subroutines.  Matches up to the first \":\"
of attributes (if present), or end of the name or prototype (whatever is
the last)."
  (concat				; Assume n groups before this...
   "\\("				; n+1=name-group
     cperl-white-and-comment-rex	; n+2=pre-name
     "\\(::[a-zA-Z_0-9:']+\\|[a-zA-Z_'][a-zA-Z_0-9:']*\\)" ; n+3=name
   "\\)"				; END n+1=name-group
   (if named "" "?")
   "\\("				; n+4=proto-group
     cperl-maybe-white-and-comment-rex	; n+5=pre-proto
     "\\(([^()]*)\\)"			; n+6=prototype
   "\\)?"				; END n+4=proto-group
   "\\("				; n+7=attr-group
     cperl-maybe-white-and-comment-rex	; n+8=pre-attr
     "\\("				; n+9=start-attr
        ":"
	(if attr (concat
		  "\\("
		     cperl-maybe-white-and-comment-rex ; whitespace-comments
		     "\\(\\sw\\|_\\)+"	; attr-name
		     ;; attr-arg (1 level of internal parens allowed!)
		     "\\((\\(\\\\.\\|[^\\\\()]\\|([^\\\\()]*)\\)*)\\)?"
		     "\\("		; optional : (XXX allows trailing???)
		        cperl-maybe-white-and-comment-rex ; whitespace-comments
		     ":\\)?"
		  "\\)+")
	  "[^:]")
     "\\)"
   "\\)?"				; END n+6=proto-group
   ))

;;; Details of groups in this are used in `cperl-imenu--create-perl-index'
;;;  and `cperl-outline-level'.
;;;; Was: 2=sub|package; now 2=package-group, 5=package-name 8=sub-name (+3)
(defvar cperl-imenu--function-name-regexp-perl
  (concat
   "^\\("				; 1 = all
       "\\([ \t]*package"		; 2 = package-group
          "\\("				; 3 = package-name-group
	    cperl-white-and-comment-rex ; 4 = pre-package-name
	       "\\([a-zA-Z_0-9:']+\\)\\)?\\)" ; 5 = package-name
       "\\|"
          "[ \t]*sub"
	  (cperl-after-sub-regexp 'named nil) ; 8=name 11=proto 14=attr-start
	  cperl-maybe-white-and-comment-rex	; 15=pre-block
   "\\|"
     "=head\\([1-4]\\)[ \t]+"		; 16=level
     "\\([^\n]+\\)$"			; 17=text
   "\\)"))

(defvar cperl-outline-regexp
  (concat cperl-imenu--function-name-regexp-perl "\\|" "\\`"))

(defvar cperl-mode-syntax-table nil
  "Syntax table in use in CPerl mode buffers.")

(defvar cperl-string-syntax-table nil
  "Syntax table in use in CPerl mode string-like chunks.")

(defsubst cperl-1- (p)
  (max (point-min) (1- p)))

(defsubst cperl-1+ (p)
  (min (point-max) (1+ p)))

(if cperl-mode-syntax-table
    ()
  (setq cperl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" cperl-mode-syntax-table)
  (modify-syntax-entry ?/ "." cperl-mode-syntax-table)
  (modify-syntax-entry ?* "." cperl-mode-syntax-table)
  (modify-syntax-entry ?+ "." cperl-mode-syntax-table)
  (modify-syntax-entry ?- "." cperl-mode-syntax-table)
  (modify-syntax-entry ?= "." cperl-mode-syntax-table)
  (modify-syntax-entry ?% "." cperl-mode-syntax-table)
  (modify-syntax-entry ?< "." cperl-mode-syntax-table)
  (modify-syntax-entry ?> "." cperl-mode-syntax-table)
  (modify-syntax-entry ?& "." cperl-mode-syntax-table)
  (modify-syntax-entry ?$ "\\" cperl-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cperl-mode-syntax-table)
  (modify-syntax-entry ?# "<" cperl-mode-syntax-table)
  (modify-syntax-entry ?' "\"" cperl-mode-syntax-table)
  (modify-syntax-entry ?` "\"" cperl-mode-syntax-table)
  (if cperl-under-as-char
      (modify-syntax-entry ?_ "w" cperl-mode-syntax-table))
  (modify-syntax-entry ?: "_" cperl-mode-syntax-table)
  (modify-syntax-entry ?| "." cperl-mode-syntax-table)
  (setq cperl-string-syntax-table (copy-syntax-table cperl-mode-syntax-table))
  (modify-syntax-entry ?$ "." cperl-string-syntax-table)
  (modify-syntax-entry ?\{ "." cperl-string-syntax-table)
  (modify-syntax-entry ?\} "." cperl-string-syntax-table)
  (modify-syntax-entry ?# "." cperl-string-syntax-table)) ; (?# comment )



;; provide an alias for working with emacs 19.  the perl-mode that comes
;; with it is really bad, and this lets us seamlessly replace it.
;;;###autoload
(fset 'perl-mode 'cperl-mode)
(defvar cperl-faces-init nil)
;; Fix for msb.el
(defvar cperl-msb-fixed nil)
(defvar font-lock-syntactic-keywords)
(defvar perl-font-lock-keywords)
(defvar perl-font-lock-keywords-1)
(defvar perl-font-lock-keywords-2)
(defvar outline-level)
(if (fboundp 'defvaralias)
    (let ((f 'defvaralias)) ; Some functions deduce stuff from the mode name...
      (funcall f 'cperl-font-lock-keywords   'perl-font-lock-keywords)
      (funcall f 'cperl-font-lock-keywords-1 'perl-font-lock-keywords-1)
      (funcall f 'cperl-font-lock-keywords-2 'perl-font-lock-keywords-2)))

(defvar cperl-use-major-mode 'perl-mode)
(defvar cperl-font-lock-multiline-start nil)
(defvar cperl-font-lock-multiline nil)
(defvar cperl-compilation-error-regexp-alist nil)
(defvar cperl-font-locking nil)

;;;###autoload
(defun cperl-mode ()
  "Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>.  When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.  The processing of < is special,
since most the time you mean \"less\".  CPerl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do,
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  eg. when the user types a space
following \"if\" the following appears in the buffer: if () { or if ()
} { } and the cursor is between the parentheses.  The user can then
type some boolean expression within the parens.  Having done that,
typing \\[cperl-linefeed] places you - appropriately indented - on a
new line between the braces (if you typed \\[cperl-linefeed] in a POD
directive line, then appropriate number of new lines is inserted).

If CPerl decides that you want to insert \"English\" style construct like

            bite if angry;

it will not do any expansion.  See also help on variable
`cperl-extra-newline-before-brace'.  (Note that one can switch the
help message on expansion by setting `cperl-message-electric-keyword'
to nil.)

\\[cperl-linefeed] is a convenience replacement for typing carriage
return.  It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like

            foreach (@lines) {print; print}

and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line.  If you need a usual
`newline-and-indent' behaviour, it is on \\[newline-and-indent],
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on font-lock-mode
\(even with older Emacsen), `cperl-electric-lbrace-space' to t switches
on electric space between $ and {, `cperl-electric-parens-string' is
the string that contains parentheses that should be electric in CPerl
\(see also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl.  `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable.  You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t.  In this case you can switch separate options off
by setting them to `null'.  Note that one may undo the extra
whitespace inserted by semis and braces in `auto-newline'-mode by
consequent \\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' and `cperl-clobber-lisp-bindings'
\(in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help], and one can run perldoc or
man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].  Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of POD and
here-docs sections.  With capable Emaxen results of scan are used
for indentation too, otherwise they are used for highlighting only.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-indent-left-aligned-comments'
    Non-nil means that the comment starting in leftmost column should indent.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code.  The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and
    `cperl-auto-newline-after-colon' set.
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for classic indent-styles: K&R BSD=C++ GNU PerlStyle=Whitesmith
  `cperl-indent-level'                5   4       2   4
  `cperl-brace-offset'                0   0       0   0
  `cperl-continued-brace-offset'     -5  -4       0   0
  `cperl-label-offset'               -5  -4      -2  -4
  `cperl-continued-statement-offset'  5   4       2   4

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this.  Use
\\[cperl-set-style-back] to restore the memorized preexisting values
\(both available from menu).  See examples in `cperl-style-examples'.

Part of the indentation style is how different parts of if/elsif/else
statements are broken into lines; in CPerl, this is reflected on how
templates for these constructs are created (controlled by
`cperl-extra-newline-before-brace'), and how reflow-logic should treat \"continuation\" blocks of else/elsif/continue, controlled by the same variable,
and by `cperl-extra-newline-before-brace-multiline',
`cperl-merge-trailing-else', `cperl-indent-region-fix-constructs'.

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-non-problems', `cperl-praise', `cperl-speed'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map cperl-mode-map)
  (if (cperl-val 'cperl-electric-linefeed)
      (progn
	(local-set-key "\C-J" 'cperl-linefeed)
	(local-set-key "\C-C\C-J" 'newline-and-indent)))
  (if (and
       (cperl-val 'cperl-clobber-lisp-bindings)
       (cperl-val 'cperl-info-on-command-no-prompt))
      (progn
	;; don't clobber the backspace binding:
	(cperl-define-key "\C-hf" 'cperl-info-on-current-command [(control h) f])
	(cperl-define-key "\C-c\C-hf" 'cperl-info-on-command
			  [(control c) (control h) f])))
  (setq major-mode cperl-use-major-mode)
  (setq mode-name "CPerl")
  (if (not cperl-mode-abbrev-table)
      (let ((prev-a-c abbrevs-changed))
	(define-abbrev-table 'cperl-mode-abbrev-table '(
		("if" "if" cperl-electric-keyword 0)
		("elsif" "elsif" cperl-electric-keyword 0)
		("while" "while" cperl-electric-keyword 0)
		("until" "until" cperl-electric-keyword 0)
		("unless" "unless" cperl-electric-keyword 0)
		("else" "else" cperl-electric-else 0)
		("continue" "continue" cperl-electric-else 0)
		("for" "for" cperl-electric-keyword 0)
		("foreach" "foreach" cperl-electric-keyword 0)
		("formy" "formy" cperl-electric-keyword 0)
		("foreachmy" "foreachmy" cperl-electric-keyword 0)
		("do" "do" cperl-electric-keyword 0)
		("=pod" "=pod" cperl-electric-pod 0)
		("=over" "=over" cperl-electric-pod 0)
		("=head1" "=head1" cperl-electric-pod 0)
		("=head2" "=head2" cperl-electric-pod 0)
		("pod" "pod" cperl-electric-pod 0)
		("over" "over" cperl-electric-pod 0)
		("head1" "head1" cperl-electric-pod 0)
		("head2" "head2" cperl-electric-pod 0)))
	(setq abbrevs-changed prev-a-c)))
  (setq local-abbrev-table cperl-mode-abbrev-table)
  (if (cperl-val 'cperl-electric-keywords)
      (abbrev-mode 1))
  (set-syntax-table cperl-mode-syntax-table)
  ;; Until Emacs is multi-threaded, we do not actually need it local:
  (make-local-variable 'cperl-font-lock-multiline-start)
  (make-local-variable 'cperl-font-locking)
  (make-local-variable 'outline-regexp)
  ;; (setq outline-regexp imenu-example--function-name-regexp-perl)
  (setq outline-regexp cperl-outline-regexp)
  (make-local-variable 'outline-level)
  (setq outline-level 'cperl-outline-level)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (if cperl-xemacs-p
    (progn
      (make-local-variable 'paren-backwards-message)
      (set 'paren-backwards-message t)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cperl-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column cperl-comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'defun-prompt-regexp)
;;;       "[ \t]*sub"
;;;	  (cperl-after-sub-regexp 'named nil) ; 8=name 11=proto 14=attr-start
;;;	  cperl-maybe-white-and-comment-rex	; 15=pre-block
  (setq defun-prompt-regexp
	(concat "[ \t]*\\(sub"
		(cperl-after-sub-regexp 'named 'attr-groups)
		"\\|"			; per toke.c
		"\\(BEGIN\\|CHECK\\|INIT\\|END\\|AUTOLOAD\\|DESTROY\\)"
		"\\)"
		cperl-maybe-white-and-comment-rex))
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'cperl-comment-indent)
  (and (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(set 'fill-paragraph-function 'cperl-fill-paragraph)))
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'cperl-indent-region)
  ;;(setq auto-fill-function 'cperl-do-auto-fill) ; Need to switch on and off!
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function
	(function cperl-imenu--create-perl-index))
  (make-local-variable 'imenu-sort-function)
  (setq imenu-sort-function nil)
  (make-local-variable 'vc-rcs-header)
  (set 'vc-rcs-header cperl-vc-rcs-header)
  (make-local-variable 'vc-sccs-header)
  (set 'vc-sccs-header cperl-vc-sccs-header)
  ;; This one is obsolete...
  (make-local-variable 'vc-header-alist)
  (set 'vc-header-alist (or cperl-vc-header-alist ; Avoid warning
			    (` ((SCCS (, (car cperl-vc-sccs-header)))
				     (RCS (, (car cperl-vc-rcs-header)))))))
  (cond ((boundp 'compilation-error-regexp-alist-alist);; xemacs 20.x
	 (make-local-variable 'compilation-error-regexp-alist-alist)
	 (set 'compilation-error-regexp-alist-alist
	      (cons (cons 'cperl cperl-compilation-error-regexp-alist)
		    (symbol-value 'compilation-error-regexp-alist-alist)))
	 (let ((f 'compilation-build-compilation-error-regexp-alist))
	   (funcall f)))
	((boundp 'compilation-error-regexp-alist);; xmeacs 19.x
	 (make-local-variable 'compilation-error-regexp-alist)
	 (set 'compilation-error-regexp-alist
	       (cons cperl-compilation-error-regexp-alist
		     (symbol-value 'compilation-error-regexp-alist)))))
  (make-local-variable 'font-lock-defaults)
  (setq	font-lock-defaults
	(cond
	 ((string< emacs-version "19.30")
	  '(perl-font-lock-keywords-2 nil nil ((?_ . "w"))))
	 ((string< emacs-version "19.33") ; Which one to use?
	  '((perl-font-lock-keywords
	     perl-font-lock-keywords-1
	     perl-font-lock-keywords-2) nil nil ((?_ . "w"))))
	 (t
	  '((cperl-load-font-lock-keywords
	     cperl-load-font-lock-keywords-1
	     cperl-load-font-lock-keywords-2) nil nil ((?_ . "w"))))))
  (make-local-variable 'cperl-syntax-state)
  (setq cperl-syntax-state nil)		; reset syntaxification cache
  (if cperl-use-syntax-table-text-property
      (progn
	(make-local-variable 'parse-sexp-lookup-properties)
	;; Do not introduce variable if not needed, we check it!
	(set 'parse-sexp-lookup-properties t)
	;; Fix broken font-lock:
	(or (boundp 'font-lock-unfontify-region-function)
	    (set 'font-lock-unfontify-region-function
		 'font-lock-default-unfontify-region))
	(unless cperl-xemacs-p		; Our: just a plug for wrong font-lock
	  (make-local-variable 'font-lock-unfontify-region-function)
	  (set 'font-lock-unfontify-region-function ; not present with old Emacs
	       'cperl-font-lock-unfontify-region-function))
	(make-local-variable 'cperl-syntax-done-to)
	(setq cperl-syntax-done-to nil)	; reset syntaxification cache
	;; Another bug: unless font-lock-syntactic-keywords, font-lock
	;;  ignores syntax-table text-property.  (t) is a hack
	;;  to make font-lock think that font-lock-syntactic-keywords
	;;  are defined
	(make-local-variable 'font-lock-syntactic-keywords)
	(setq font-lock-syntactic-keywords
	      (if cperl-syntaxify-by-font-lock
		  '(t (cperl-fontify-syntaxically))
		'(t)))))
  (if (boundp 'font-lock-multiline)	; Newer font-lock; use its facilities
      (progn
	(setq cperl-font-lock-multiline t) ; Not localized...
	(set (make-local-variable 'font-lock-multiline) t))
    (make-local-variable 'font-lock-fontify-region-function)
    (set 'font-lock-fontify-region-function ; not present with old Emacs
	 'cperl-font-lock-fontify-region-function))
  (make-local-variable 'font-lock-fontify-region-function)
  (set 'font-lock-fontify-region-function ; not present with old Emacs
       'cperl-font-lock-fontify-region-function)
  (make-local-variable 'cperl-old-style)
  (if (boundp 'normal-auto-fill-function) ; 19.33 and later
      (set (make-local-variable 'normal-auto-fill-function)
	   'cperl-do-auto-fill)	      ; RMS has it as #'cperl-do-auto-fill ???
    (or (fboundp 'cperl-old-auto-fill-mode)
	(progn
	  (fset 'cperl-old-auto-fill-mode (symbol-function 'auto-fill-mode))
	  (defun auto-fill-mode (&optional arg)
	    (interactive "P")
	    (eval '(cperl-old-auto-fill-mode arg)) ; Avoid a warning
	    (and auto-fill-function (memq major-mode '(perl-mode cperl-mode))
		 (setq auto-fill-function 'cperl-do-auto-fill))))))
  (if (cperl-enable-font-lock)
      (if (cperl-val 'cperl-font-lock)
	  (progn (or cperl-faces-init (cperl-init-faces))
		 (font-lock-mode 1))))
  (set (make-local-variable 'facemenu-add-face-function)
       'cperl-facemenu-add-face-function) ; XXXX What this guy is for???
  (and (boundp 'msb-menu-cond)
       (not cperl-msb-fixed)
       (cperl-msb-fix))
  (if (featurep 'easymenu)
      (easy-menu-add cperl-menu))	; A NOP in RMS Emacs.
  (run-hooks 'cperl-mode-hook)
  (if cperl-hook-after-change
      (progn
	(make-local-hook 'after-change-functions)
	(add-hook 'after-change-functions 'cperl-after-change-function nil t)))
  ;; After hooks since fontification will break this
  (if cperl-pod-here-scan
      (or cperl-syntaxify-by-font-lock
       (progn (or cperl-faces-init (cperl-init-faces-weak))
	      (cperl-find-pods-heres)))))

;; Fix for perldb - make default reasonable
(defun cperl-db ()
  (interactive)
  (require 'gud)
  (perldb (read-from-minibuffer "Run perldb (like this): "
				(if (consp gud-perldb-history)
				    (car gud-perldb-history)
				  (concat "perl " ;;(file-name-nondirectory
					  ;; I have problems
					  ;; in OS/2
					  ;; otherwise
					  (buffer-file-name)))
				nil nil
				'(gud-perldb-history . 1))))

(defun cperl-msb-fix ()
  ;; Adds perl files to msb menu, supposes that msb is already loaded
  (setq cperl-msb-fixed t)
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(memq major-mode '(cperl-mode perl-mode))
		     handle
		     "Perl Files (%d)")
		    last))))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in CPerl code
;; based on its context.  Do fallback if comment is found wrong.

(defvar cperl-wrong-comment)
(defvar cperl-st-cfence '(14))		; Comment-fence
(defvar cperl-st-sfence '(15))		; String-fence
(defvar cperl-st-punct '(1))
(defvar cperl-st-word '(2))
(defvar cperl-st-bra '(4 . ?\>))
(defvar cperl-st-ket '(5 . ?\<))


(defun cperl-comment-indent ()		; called at point at supposed comment
  (let ((p (point)) (c (current-column)) was phony)
    (if (and (not cperl-indent-comment-at-column-0)
	     (looking-at "^#"))
	0	; Existing comment at bol stays there.
      ;; Wrong comment found
      (save-excursion
	(setq was (cperl-to-comment-or-eol)
	      phony (eq (get-text-property (point) 'syntax-table)
			cperl-st-cfence))
	(if phony
	    (progn			; Too naive???
	      (re-search-forward "#\\|$") ; Hmm, what about embedded #?
	      (if (eq (preceding-char) ?\#)
		  (forward-char -1))
	      (setq was nil)))
	(if (= (point) p)		; Our caller found a correct place
	    (progn
	      (skip-chars-backward " \t")
	      (setq was (current-column))
	      (if (eq was 0)
		  comment-column
		(max (1+ was) ; Else indent at comment column
		     comment-column)))
	  ;; No, the caller found a random place; we need to edit ourselves
	  (if was nil
	    (insert comment-start)
	    (backward-char (length comment-start)))
	  (setq cperl-wrong-comment t)
	  (cperl-make-indent comment-column 1) ; Indent min 1
	  c)))))

;;;(defun cperl-comment-indent-fallback ()
;;;  "Is called if the standard comment-search procedure fails.
;;;Point is at start of real comment."
;;;  (let ((c (current-column)) target cnt prevc)
;;;    (if (= c comment-column) nil
;;;      (setq cnt (skip-chars-backward "[ \t]"))
;;;      (setq target (max (1+ (setq prevc
;;;			     (current-column))) ; Else indent at comment column
;;;		   comment-column))
;;;      (if (= c comment-column) nil
;;;	(delete-backward-char cnt)
;;;	(while (< prevc target)
;;;	  (insert "\t")
;;;	  (setq prevc (current-column)))
;;;	(if (> prevc target) (progn (delete-char -1) (setq prevc (current-column))))
;;;	(while (< prevc target)
;;;	  (insert " ")
;;;	  (setq prevc (current-column)))))))

(defun cperl-indent-for-comment ()
  "Substitute for `indent-for-comment' in CPerl."
  (interactive)
  (let (cperl-wrong-comment)
    (indent-for-comment)
    (if cperl-wrong-comment		; set by `cperl-comment-indent'
	(progn (cperl-to-comment-or-eol)
	       (forward-char (length comment-start))))))

(defun cperl-comment-region (b e arg)
  "Comment or uncomment each line in the region in CPerl mode.
See `comment-region'."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e arg)))

(defun cperl-uncomment-region (b e arg)
  "Uncomment or comment each line in the region in CPerl mode.
See `comment-region'."
  (interactive "r\np")
  (let ((comment-start "#"))
    (comment-region b e (- arg))))

(defvar cperl-brace-recursing nil)

(defun cperl-electric-brace (arg &optional only-before)
  "Insert character and correct line's indentation.
If ONLY-BEFORE and `cperl-auto-newline', will insert newline before the
place (even in empty line), but not after.  If after \")\" and the inserted
char is \"{\", insert extra newline before only if
`cperl-extra-newline-before-brace'."
  (interactive "P")
  (let (insertpos
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-mark-active)
			    (< (mark) (point)))
		       (mark)
		     nil)))
    (if (and other-end
	     (not cperl-brace-recursing)
	     (cperl-val 'cperl-electric-parens)
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point)))
	;; Need to insert a matching pair
	(progn
	  (save-excursion
	    (setq insertpos (point-marker))
	    (goto-char other-end)
	    (setq last-command-char ?\{)
	    (cperl-electric-lbrace arg insertpos))
	  (forward-char 1))
      ;; Check whether we close something "usual" with `}'
      (if (and (eq last-command-char ?\})
	       (not
		(condition-case nil
		    (save-excursion
		      (up-list (- (prefix-numeric-value arg)))
		      ;;(cperl-after-block-p (point-min))
		      (or (cperl-after-expr-p nil "{;)")
			  ;; after sub, else, continue
			  (cperl-after-block-p nil 'pre)))
		  (error nil))))
	  ;; Just insert the guy
	  (self-insert-command (prefix-numeric-value arg))
	(if (and (not arg)		; No args, end (of empty line or auto)
		 (eolp)
		 (or (and (null only-before)
			  (save-excursion
			    (skip-chars-backward " \t")
			    (bolp)))
		     (and (eq last-command-char ?\{) ; Do not insert newline
			  ;; if after ")" and `cperl-extra-newline-before-brace'
			  ;; is nil, do not insert extra newline.
			  (not cperl-extra-newline-before-brace)
			  (save-excursion
			    (skip-chars-backward " \t")
			    (eq (preceding-char) ?\))))
		     (if cperl-auto-newline
			 (progn (cperl-indent-line) (newline) t) nil)))
	    (progn
	      (self-insert-command (prefix-numeric-value arg))
	      (cperl-indent-line)
	      (if cperl-auto-newline
		  (setq insertpos (1- (point))))
	      (if (and cperl-auto-newline (null only-before))
		  (progn
		    (newline)
		    (cperl-indent-line)))
	      (save-excursion
		(if insertpos (progn (goto-char insertpos)
				     (search-forward (make-string
						      1 last-command-char))
				     (setq insertpos (1- (point)))))
		(delete-char -1))))
	(if insertpos
	    (save-excursion
	      (goto-char insertpos)
	      (self-insert-command (prefix-numeric-value arg)))
	  (self-insert-command (prefix-numeric-value arg)))))))

(defun cperl-electric-lbrace (arg &optional end)
  "Insert character, correct line's indentation, correct quoting by space."
  (interactive "P")
  (let ((cperl-brace-recursing t)
	(cperl-auto-newline cperl-auto-newline)
	(other-end (or end
		       (if (and cperl-electric-parens-mark
				(cperl-mark-active)
				(> (mark) (point)))
			   (save-excursion
			     (goto-char (mark))
			     (point-marker))
			 nil)))
	pos after)
    (and (cperl-val 'cperl-electric-lbrace-space)
	 (eq (preceding-char) ?$)
	 (save-excursion
	   (skip-chars-backward "$")
	   (looking-at "\\(\\$\\$\\)*\\$\\([^\\$]\\|$\\)"))
	 (insert ?\ ))
    ;; Check whether we are in comment
    (if (and
	 (save-excursion
	   (beginning-of-line)
	   (not (looking-at "[ \t]*#")))
	 (cperl-after-expr-p nil "{;)"))
	nil
      (setq cperl-auto-newline nil))
    (cperl-electric-brace arg)
    (and (cperl-val 'cperl-electric-parens)
	 (eq last-command-char ?{)
	 (memq last-command-char
	       (append cperl-electric-parens-string nil))
	 (or (if other-end (goto-char (marker-position other-end)))
	     t)
	 (setq last-command-char ?} pos (point))
	 (progn (cperl-electric-brace arg t)
		(goto-char pos)))))

(defun cperl-electric-paren (arg)
  "Insert an opening parenthesis or a matching pair of parentheses.
See `cperl-electric-parens'."
  (interactive "P")
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-mark-active)
			    (> (mark) (point)))
		       (save-excursion
			 (goto-char (mark))
			 (point-marker))
		     nil)))
    (if (and (cperl-val 'cperl-electric-parens)
	     (memq last-command-char
		   (append cperl-electric-parens-string nil))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     ;;(not (save-excursion (search-backward "#" beg t)))
	     (if (eq last-command-char ?<)
		 (progn
		   (and abbrev-mode ; later it is too late, may be after `for'
			(expand-abbrev))
		   (cperl-after-expr-p nil "{;(,:="))
	       1))
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (if other-end (goto-char (marker-position other-end)))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-char '((?{ .?})
						   (?[ . ?])
						   (?( . ?))
						   (?< . ?>))))))
	  (forward-char (- (prefix-numeric-value arg))))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-rparen (arg)
  "Insert a matching pair of parentheses if marking is active.
If not, or if we are not at the end of marking range, would self-insert.
Affected by `cperl-electric-parens'."
  (interactive "P")
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(other-end (if (and cperl-electric-parens-mark
			    (cperl-val 'cperl-electric-parens)
			    (memq last-command-char
				  (append cperl-electric-parens-string nil))
			    (cperl-mark-active)
			    (< (mark) (point)))
		       (mark)
		     nil))
	p)
    (if (and other-end
	     (cperl-val 'cperl-electric-parens)
	     (memq last-command-char '( ?\) ?\] ?\} ?\> ))
	     (>= (save-excursion (cperl-to-comment-or-eol) (point)) (point))
	     ;;(not (save-excursion (search-backward "#" beg t)))
	     )
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  (setq p (point))
	  (if other-end (goto-char other-end))
	  (insert (make-string
		   (prefix-numeric-value arg)
		   (cdr (assoc last-command-char '((?\} . ?\{)
						   (?\] . ?\[)
						   (?\) . ?\()
						   (?\> . ?\<))))))
	  (goto-char (1+ p)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-keyword ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(dollar (and (eq last-command-char ?$)
		     (eq this-command 'self-insert-command)))
	(delete (and (memq last-command-char '(?\ ?\n ?\t ?\f))
		     (memq this-command '(self-insert-command newline))))
	my do)
    (and (save-excursion
	   (condition-case nil
	       (progn
		 (backward-sexp 1)
		 (setq do (looking-at "do\\>")))
	     (error nil))
	   (cperl-after-expr-p nil "{;:"))
	 (save-excursion
	   (not
	    (re-search-backward
	     "[#\"'`]\\|\\<q\\(\\|[wqxr]\\)\\>"
	     beg t)))
	 (save-excursion (or (not (re-search-backward "^=" nil t))
			     (or
			      (looking-at "=cut")
			      (and cperl-use-syntax-table-text-property
				   (not (eq (get-text-property (point)
							       'syntax-type)
					    'pod))))))
	 (save-excursion (forward-sexp -1)
			 (not (memq (following-char) (append "$@%&*" nil))))
	 (progn
	   (and (eq (preceding-char) ?y)
		(progn			; "foreachmy"
		  (forward-char -2)
		  (insert " ")
		  (forward-char 2)
		  (setq my t dollar t
			delete
			(memq this-command '(self-insert-command newline)))))
	   (and dollar (insert " $"))
	   (cperl-indent-line)
	   ;;(insert " () {\n}")
 	   (cond
 	    (cperl-extra-newline-before-brace
 	     (insert (if do "\n" " ()\n"))
 	     (insert "{")
 	     (cperl-indent-line)
 	     (insert "\n")
 	     (cperl-indent-line)
 	     (insert "\n}")
	     (and do (insert " while ();")))
 	    (t
 	     (insert (if do " {\n} while ();" " () {\n}"))))
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (if dollar (progn (search-backward "$")
			     (if my
				 (forward-char 1)
			       (delete-char 1)))
	     (search-backward ")")
	     (if (eq last-command-char ?\()
		 (progn			; Avoid "if (())"
		   (delete-backward-char 1)
		   (delete-backward-char -1))))
	   (if delete
	       (cperl-putback-char cperl-del-back-ch))
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-ensure-newlines (n &optional pos)
  "Make sure there are N newlines after the point."
  (or pos (setq pos (point)))
  (if (looking-at "\n")
      (forward-char 1)
    (insert "\n"))
  (if (> n 1)
      (cperl-ensure-newlines (1- n) pos)
    (goto-char pos)))

(defun cperl-electric-pod ()
  "Insert a POD chunk appropriate after a =POD directive."
  (let ((delete (and (memq last-command-char '(?\ ?\n ?\t ?\f))
		     (memq this-command '(self-insert-command newline))))
	head1 notlast name p really-delete over)
    (and (save-excursion
	   (forward-word -1)
	   (and
	    (eq (preceding-char) ?=)
	    (progn
	      (setq head1 (looking-at "head1\\>[ \t]*$"))
	      (setq over (and (looking-at "over\\>[ \t]*$")
			      (not (looking-at "over[ \t]*\n\n\n*=item\\>"))))
	      (forward-char -1)
	      (bolp))
	    (or
	     (get-text-property (point) 'in-pod)
	     (cperl-after-expr-p nil "{;:")
	     (and (re-search-backward "\\(\\`\n?\\|^\n\\)=\\sw+" (point-min) t)
		  (not (looking-at "\n*=cut"))
		  (or (not cperl-use-syntax-table-text-property)
		      (eq (get-text-property (point) 'syntax-type) 'pod))))))
	 (progn
	   (save-excursion
	     (setq notlast (re-search-forward "^\n=" nil t)))
	   (or notlast
	       (progn
		 (insert "\n\n=cut")
		 (cperl-ensure-newlines 2)
		 (forward-word -2)
		 (if (and head1
			  (not
			   (save-excursion
			     (forward-char -1)
			     (re-search-backward "\\(\\`\n?\\|\n\n\\)=head1\\>"
						 nil t)))) ; Only one
		     (progn
		       (forward-word 1)
		       (setq name (file-name-sans-extension
				   (file-name-nondirectory (buffer-file-name)))
			     p (point))
		       (insert " NAME\n\n" name
			       " - \n\n=head1 SYNOPSIS\n\n\n\n"
			       "=head1 DESCRIPTION")
		       (cperl-ensure-newlines 4)
		       (goto-char p)
		       (forward-word 2)
		       (end-of-line)
		       (setq really-delete t))
		   (forward-word 1))))
	   (if over
	       (progn
		 (setq p (point))
		 (insert "\n\n=item \n\n\n\n"
			 "=back")
		 (cperl-ensure-newlines 2)
		 (goto-char p)
		 (forward-word 1)
		 (end-of-line)
		 (setq really-delete t)))
	   (if (and delete really-delete)
	       (cperl-putback-char cperl-del-back-ch))))))

(defun cperl-electric-else ()
  "Insert a construction appropriate after a keyword.
Help message may be switched off by setting `cperl-message-electric-keyword'
to nil."
  (let ((beg (save-excursion (beginning-of-line) (point))))
    (and (save-excursion
	   (backward-sexp 1)
	   (cperl-after-expr-p nil "{;:"))
	 (save-excursion
	   (not
	    (re-search-backward
	     "[#\"'`]\\|\\<q\\(\\|[wqxr]\\)\\>"
	     beg t)))
	 (save-excursion (or (not (re-search-backward "^=" nil t))
			     (looking-at "=cut")
			     (and cperl-use-syntax-table-text-property
				  (not (eq (get-text-property (point)
							      'syntax-type)
					   'pod)))))
	 (progn
	   (cperl-indent-line)
	   ;;(insert " {\n\n}")
 	   (cond
 	    (cperl-extra-newline-before-brace
 	     (insert "\n")
 	     (insert "{")
 	     (cperl-indent-line)
 	     (insert "\n\n}"))
 	    (t
 	     (insert " {\n\n}")))
	   (or (looking-at "[ \t]\\|$") (insert " "))
	   (cperl-indent-line)
	   (forward-line -1)
	   (cperl-indent-line)
	   (cperl-putback-char cperl-del-back-ch)
	   (setq this-command 'cperl-electric-else)
	   (if cperl-message-electric-keyword
	       (message "Precede char by C-q to avoid expansion"))))))

(defun cperl-linefeed ()
  "Go to end of line, open a new line and indent appropriately.
If in POD, insert appropriate lines."
  (interactive)
  (let ((beg (save-excursion (beginning-of-line) (point)))
	(end (save-excursion (end-of-line) (point)))
	(pos (point)) start over cut res)
    (if (and				; Check if we need to split:
					; i.e., on a boundary and inside "{...}"
	 (save-excursion (cperl-to-comment-or-eol)
			 (>= (point) pos)) ; Not in a comment
	 (or (save-excursion
	       (skip-chars-backward " \t" beg)
	       (forward-char -1)
	       (looking-at "[;{]"))     ; After { or ; + spaces
	     (looking-at "[ \t]*}")	; Before }
	     (re-search-forward "\\=[ \t]*;" end t)) ; Before spaces + ;
	 (save-excursion
	   (and
	    (eq (car (parse-partial-sexp pos end -1)) -1)
					; Leave the level of parens
	    (looking-at "[,; \t]*\\($\\|#\\)") ; Comma to allow anon subr
					; Are at end
	    (cperl-after-block-p (point-min))
	    (progn
	      (backward-sexp 1)
	      (setq start (point-marker))
	      (<= start pos)))))	; Redundant?  Are after the
					; start of parens group.
	(progn
	  (skip-chars-backward " \t")
	  (or (memq (preceding-char) (append ";{" nil))
	      (insert ";"))
	  (insert "\n")
	  (forward-line -1)
	  (cperl-indent-line)
	  (goto-char start)
	  (or (looking-at "{[ \t]*$")	; If there is a statement
					; before, move it to separate line
	      (progn
		(forward-char 1)
		(insert "\n")
		(cperl-indent-line)))
	  (forward-line 1)		; We are on the target line
	  (cperl-indent-line)
	  (beginning-of-line)
	  (or (looking-at "[ \t]*}[,; \t]*$") ; If there is a statement
					; after, move it to separate line
	      (progn
		(end-of-line)
		(search-backward "}" beg)
		(skip-chars-backward " \t")
		(or (memq (preceding-char) (append ";{" nil))
		    (insert ";"))
		(insert "\n")
		(cperl-indent-line)
		(forward-line -1)))
	  (forward-line -1)		; We are on the line before target
	  (end-of-line)
	  (newline-and-indent))
      (end-of-line)			; else - no splitting
      (cond
       ((and (looking-at "\n[ \t]*{$")
	     (save-excursion
	       (skip-chars-backward " \t")
	       (eq (preceding-char) ?\)))) ; Probably if () {} group
					; with an extra newline.
	(forward-line 2)
	(cperl-indent-line))
       ((save-excursion			; In POD header
	  (forward-paragraph -1)
	  ;; (re-search-backward "\\(\\`\n?\\|\n\n\\)=head1\\b")
	  ;; We are after \n now, so look for the rest
	  (if (looking-at "\\(\\`\n?\\|\n\\)=\\sw+")
	      (progn
		(setq cut (looking-at "\\(\\`\n?\\|\n\\)=cut\\>"))
		(setq over (looking-at "\\(\\`\n?\\|\n\\)=over\\>"))
		t)))
	(if (and over
		 (progn
		   (forward-paragraph -1)
		   (forward-word 1)
		   (setq pos (point))
		   (setq cut (buffer-substring (point)
					       (save-excursion
						 (end-of-line)
						 (point))))
		   (delete-char (- (save-excursion (end-of-line) (point))
				   (point)))
		   (setq res (expand-abbrev))
		   (save-excursion
		     (goto-char pos)
		     (insert cut))
		   res))
	    nil
	  (cperl-ensure-newlines (if cut 2 4))
	  (forward-line 2)))
       ((get-text-property (point) 'in-pod) ; In POD section
	(cperl-ensure-newlines 4)
	(forward-line 2))
       ((looking-at "\n[ \t]*$")	; Next line is empty - use it.
        (forward-line 1)
	(cperl-indent-line))
       (t
	(newline-and-indent))))))

(defun cperl-electric-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if cperl-auto-newline
      (cperl-electric-terminator arg)
    (self-insert-command (prefix-numeric-value arg))
    (if cperl-autoindent-on-semi
	(cperl-indent-line))))

(defun cperl-electric-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let ((end (point))
	(auto (and cperl-auto-newline
		   (or (not (eq last-command-char ?:))
		       cperl-auto-newline-after-colon)))
	insertpos)
    (if (and ;;(not arg)
	     (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or
		     ;; Ignore in comment lines
		     (= (following-char) ?#)
		     ;; Colon is special only after a label
		     ;; So quickly rule out most other uses of colon
		     ;; and do no indentation for them.
		     (and (eq last-command-char ?:)
			  (save-excursion
			    (forward-word 1)
			    (skip-chars-forward " \t")
			    (and (< (point) end)
				 (progn (goto-char (- end 1))
					(not (looking-at ":"))))))
		     (progn
		       (beginning-of-defun)
		       (let ((pps (parse-partial-sexp (point) end)))
			 (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (self-insert-command (prefix-numeric-value arg))
	  ;;(forward-char -1)
	  (if auto (setq insertpos (point-marker)))
	  ;;(forward-char 1)
	  (cperl-indent-line)
	  (if auto
	      (progn
		(newline)
		(cperl-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1- (marker-position insertpos)))
	      (forward-char -1))
	    (delete-char 1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun cperl-electric-backspace (arg)
  "Backspace, or remove the whitespace around the point inserted by an electric
key.  Will untabivy if `cperl-electric-backspace-untabify' is non-nil."
  (interactive "p")
  (if (and cperl-auto-newline
	   (memq last-command '(cperl-electric-semi
				cperl-electric-terminator
				cperl-electric-lbrace))
	   (memq (preceding-char) '(?\  ?\t ?\n)))
      (let (p)
	(if (eq last-command 'cperl-electric-lbrace)
	    (skip-chars-forward " \t\n"))
	(setq p (point))
	(skip-chars-backward " \t\n")
	(delete-region (point) p))
    (and (eq last-command 'cperl-electric-else)
	 ;; We are removing the whitespace *inside* cperl-electric-else
	 (setq this-command 'cperl-electric-else-really))
    (if (and cperl-auto-newline
	     (eq last-command 'cperl-electric-else-really)
	     (memq (preceding-char) '(?\  ?\t ?\n)))
	(let (p)
	  (skip-chars-forward " \t\n")
	  (setq p (point))
	  (skip-chars-backward " \t\n")
	  (delete-region (point) p))
      (if cperl-electric-backspace-untabify
	  (backward-delete-char-untabify arg)
	(delete-backward-char arg)))))

(defun cperl-inside-parens-p ()		;; NOT USED????
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun cperl-indent-command (&optional whole-exp)
  "Indent current line as Perl code, or in some cases insert a tab character.
If `cperl-tab-always-indent' is non-nil (the default), always indent current
line.  Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (cperl-update-syntaxification (point) (point))
  (if whole-exp
      ;; If arg, always indent this line as Perl
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (cperl-indent-line))
	    beg end)
	(save-excursion
	  (if cperl-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (and shift-amt (> end beg))
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not cperl-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (cperl-indent-line))))

(defun cperl-indent-line (&optional parse-data)
  "Indent current line as Perl code.
Return the amount the indentation changed by."
  (let ((case-fold-search nil)
	(pos (- (point-max) (point)))
	indent i beg shift-amt)
    (setq indent (cperl-calculate-indent parse-data)
	  i indent)
    (beginning-of-line)
    (setq beg (point))
    (cond ((or (eq indent nil) (eq indent t))
	   (setq indent (current-indentation) i nil))
	  ;;((eq indent t)    ; Never?
	  ;; (setq indent (cperl-calculate-indent-within-comment)))
	  ;;((looking-at "[ \t]*#")
	  ;; (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((and (looking-at "[A-Za-z_][A-Za-z_0-9]*:[^:]")
		       (not (looking-at "[smy]:\\|tr:")))
		  (and (> indent 0)
		       (setq indent (max cperl-min-label-indent
					 (+ indent cperl-label-offset)))))
		 ((= (following-char) ?})
		  (setq indent (- indent cperl-indent-level)))
		 ((memq (following-char) '(?\) ?\])) ; To line up with opening paren.
		  (setq indent (+ indent cperl-close-paren-offset)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent cperl-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (and i (- indent (current-column))))
    (if (or (not shift-amt)
	    (zerop shift-amt))
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      ;;;(delete-region beg (point))
      ;;;(indent-to indent)
      (cperl-make-indent indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun cperl-after-label ()
  ;; Returns true if the point is after label.  Does not do save-excursion.
  (and (eq (preceding-char) ?:)
       (memq (char-syntax (char-after (- (point) 2)))
	     '(?w ?_))
       (progn
	 (backward-sexp)
	 (looking-at "[a-zA-Z_][a-zA-Z0-9_]*:[^:]"))))

(defun cperl-get-state (&optional parse-start start-state)
  ;; returns list (START STATE DEPTH PRESTART),
  ;; START is a good place to start parsing, or equal to
  ;; PARSE-START if preset,
  ;; STATE is what is returned by `parse-partial-sexp'.
  ;; DEPTH is true is we are immediately after end of block
  ;; which contains START.
  ;; PRESTART is the position basing on which START was found.
  (save-excursion
    (let ((start-point (point)) depth state start prestart)
      (if (and parse-start
	       (<= parse-start start-point))
	  (goto-char parse-start)
	(beginning-of-defun)
	(setq start-state nil))
      (setq prestart (point))
      (if start-state nil
	;; Try to go out, if sub is not on the outermost level
	(while (< (point) start-point)
	  (setq start (point) parse-start start depth nil
		state (parse-partial-sexp start start-point -1))
	  (if (> (car state) -1) nil
	    ;; The current line could start like }}}, so the indentation
	    ;; corresponds to a different level than what we reached
	    (setq depth t)
	    (beginning-of-line 2)))	; Go to the next line.
	(if start (goto-char start)))	; Not at the start of file
      (setq start (point))
      (or state (setq state (parse-partial-sexp start start-point -1 nil start-state)))
      (list start state depth prestart))))

(defvar cperl-look-for-prop '((pod in-pod) (here-doc-delim here-doc-group)))

(defun cperl-beginning-of-property (p prop &optional lim)
  "Given that P has a property PROP, find where the property starts.
Will not look before LIM."
  ;;; XXXX What to do at point-max???
  (or (previous-single-property-change (cperl-1+ p) prop lim)
      (point-min))
;;;  (cond ((eq p (point-min))
;;;	 p)
;;;	((and lim (<= p lim))
;;;	 p)
;;;	((not (get-text-property (1- p) prop))
;;;	 p)
;;;	(t (or (previous-single-property-change p look-prop lim)
;;;	       (point-min))))
  )

(defun cperl-sniff-for-indent (&optional parse-data) ; was parse-start
  ;; Old workhorse for calculation of indentation; the major problem
  ;; is that it mixes the sniffer logic to understand what the current line
  ;; MEANS with the logic to actually calculate where to indent it.
  ;; The latter part should be eventually moved to `cperl-calculate-indent';
  ;; actually, this is mostly done now...
  (cperl-update-syntaxification (point) (point))
  (let ((res (get-text-property (point) 'syntax-type)))
    (save-excursion
      (cond
       ((and (memq res '(pod here-doc here-doc-delim format))
	     (not (get-text-property (point) 'indentable)))
	(vector res))
       ;; before start of POD - whitespace found since do not have 'pod!
       ((looking-at "[ \t]*\n=")
	(error "Spaces before POD section!"))
       ((and (not cperl-indent-left-aligned-comments)
	     (looking-at "^#"))
	[comment-special:at-beginning-of-line])
       ((get-text-property (point) 'in-pod)
	[in-pod])
       (t
	(beginning-of-line)
	(let* ((indent-point (point))
	       (char-after-pos (save-excursion
				 (skip-chars-forward " \t")
				 (point)))
	       (char-after (char-after char-after-pos))
	       (pre-indent-point (point))
	       p prop look-prop is-block delim)
	  (save-excursion		; Know we are not in POD, find appropriate pos before
	    (cperl-backward-to-noncomment nil)
	    (setq p (max (point-min) (1- (point)))
		  prop (get-text-property p 'syntax-type)
		  look-prop (or (nth 1 (assoc prop cperl-look-for-prop))
				'syntax-type))
	    (if (memq prop '(pod here-doc format here-doc-delim))
		(progn
		  (goto-char (cperl-beginning-of-property p look-prop))
		  (beginning-of-line)
		  (setq pre-indent-point (point)))))
	  (goto-char pre-indent-point)	; Orig line skipping preceeding pod/etc
	  (let* ((case-fold-search nil)
		 (s-s (cperl-get-state (car parse-data) (nth 1 parse-data)))
		 (start (or (nth 2 parse-data) ; last complete sexp terminated
			    (nth 0 s-s))) ; Good place to start parsing
		 (state (nth 1 s-s))
		 (containing-sexp (car (cdr state)))
		 old-indent)
	    (if (and
		 ;;containing-sexp		;; We are buggy at toplevel :-(
		 parse-data)
		(progn
		  (setcar parse-data pre-indent-point)
		  (setcar (cdr parse-data) state)
		  (or (nth 2 parse-data)
		      (setcar (cddr parse-data) start))
		  ;; Before this point: end of statement
		  (setq old-indent (nth 3 parse-data))))
	    (cond ((get-text-property (point) 'indentable)
		   ;; indent to "after" the surrounding open
		   ;; (same offset as `cperl-beautify-regexp-piece'),
		   ;; skip blanks if we do not close the expression.
		   (setq delim		; We do not close the expression
			 (get-text-property
			  (cperl-1+ char-after-pos) 'indentable)
			 p (1+ (cperl-beginning-of-property
				(point) 'indentable))
			 is-block	; misused for: preceeding line in REx
			 (save-excursion ; Find preceeding line
			   (cperl-backward-to-noncomment p)
			   (beginning-of-line)
			   (if (<= (point) p)
			       (progn	; get indent from the first line
				 (goto-char p)
				 (skip-chars-forward " \t")
				 (if (memq (char-after (point))
					   (append "#\n" nil))
				     nil ; Can't use intentation of this line...
				   (point)))
			     (skip-chars-forward " \t")
			     (point)))
			 prop (parse-partial-sexp p char-after-pos))
		   (cond ((not delim)	; End the REx, ignore is-block
			  (vector 'indentable 'terminator p is-block))
			 (is-block	; Indent w.r.t. preceeding line
			  (vector 'indentable 'cont-line char-after-pos
				  is-block char-after p))
			 (t		; No preceeding line...
			  (vector 'indentable 'first-line p))))
		  ((get-text-property char-after-pos 'REx-part2)
		   (vector 'REx-part2 (point)))
		  ((nth 4 state)
		   [comment])
		  ((nth 3 state)
		   [string])
		  ;; XXXX Do we need to special-case this?
		  ((null containing-sexp)
		   ;; Line is at top level.  May be data or function definition,
		   ;; or may be function argument declaration.
		   ;; Indent like the previous top level line
		   ;; unless that ends in a closeparen without semicolon,
		   ;; in which case this line is the first argument decl.
		   (skip-chars-forward " \t")
		   (cperl-backward-to-noncomment (or old-indent (point-min)))
		   (setq state
			 (or (bobp)
			     (eq (point) old-indent) ; old-indent was at comment
			     (eq (preceding-char) ?\;)
			     ;;  Had ?\) too
			     (and (eq (preceding-char) ?\})
				  (cperl-after-block-and-statement-beg
				   (point-min))) ; Was start - too close
			     (memq char-after (append ")]}" nil))
			     (and (eq (preceding-char) ?\:) ; label
				  (progn
				    (forward-sexp -1)
				    (skip-chars-backward " \t")
				    (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*[ \t]*:")))
			     (get-text-property (point) 'first-format-line)))
		   
		   ;; Look at previous line that's at column 0
		   ;; to determine whether we are in top-level decls
		   ;; or function's arg decls.  Set basic-indent accordingly.
		   ;; Now add a little if this is a continuation line.
		   (and state
			parse-data
			(not (eq char-after ?\C-j))
			(setcdr (cddr parse-data)
				(list pre-indent-point)))
		   (vector 'toplevel start char-after state (nth 2 s-s)))
		  ((not
		    (or (setq is-block
			      (and (setq delim (= (char-after containing-sexp) ?{))
				   (save-excursion ; Is it a hash?
				     (goto-char containing-sexp)
				     (cperl-block-p))))
			cperl-indent-parens-as-block))
		   ;; group is an expression, not a block:
		   ;; indent to just after the surrounding open parens,
		   ;; skip blanks if we do not close the expression.
		   (goto-char (1+ containing-sexp))
		   (or (memq char-after
			     (append (if delim "}" ")]}") nil))
		       (looking-at "[ \t]*\\(#\\|$\\)")
		       (skip-chars-forward " \t"))
		   (setq old-indent (point)) ; delim=is-brace
		   (vector 'in-parens char-after (point) delim containing-sexp))
		  (t
		   ;; Statement level.  Is it a continuation or a new statement?
		   ;; Find previous non-comment character.
		   (goto-char pre-indent-point) ; Skip one level of POD/etc
		   (cperl-backward-to-noncomment containing-sexp)
		   ;; Back up over label lines, since they don't
		   ;; affect whether our line is a continuation.
		   ;; (Had \, too)
		   (while;;(or (eq (preceding-char) ?\,)
		       (and (eq (preceding-char) ?:)
			    (or;;(eq (char-after (- (point) 2)) ?\') ; ????
			     (memq (char-syntax (char-after (- (point) 2)))
				   '(?w ?_))))
		     ;;)
		     ;; This is always FALSE?
		     (if (eq (preceding-char) ?\,)
			 ;; Will go to beginning of line, essentially.
			 ;; Will ignore embedded sexpr XXXX.
			 (cperl-backward-to-start-of-continued-exp containing-sexp))
		     (beginning-of-line)
		     (cperl-backward-to-noncomment containing-sexp))
		   ;; Now we get non-label preceeding the indent point
		   (if (not (or (eq (1- (point)) containing-sexp)
				(memq (preceding-char)
				      (append (if is-block " ;{" " ,;{") '(nil)))
				(and (eq (preceding-char) ?\})
				     (cperl-after-block-and-statement-beg
				      containing-sexp))
				(get-text-property (point) 'first-format-line)))
		       ;; This line is continuation of preceding line's statement;
		       ;; indent  `cperl-continued-statement-offset'  more than the
		       ;; previous line of the statement.
		       ;;
		       ;; There might be a label on this line, just
		       ;; consider it bad style and ignore it.
		       (progn
			 (cperl-backward-to-start-of-continued-exp containing-sexp)
			 (vector 'continuation (point) char-after is-block delim))
		     ;; This line starts a new statement.
		     ;; Position following last unclosed open brace
		     (goto-char containing-sexp)
		     ;; Is line first statement after an open-brace?
		     (or
		      ;; If no, find that first statement and indent like
		      ;; it.  If the first statement begins with label, do
		      ;; not believe when the indentation of the label is too
		      ;; small.
		      (save-excursion
			(forward-char 1)
			(let ((colon-line-end 0))
			  (while
			      (progn (skip-chars-forward " \t\n")
				     ;; s: foo : bar :x is NOT label
				     (and (looking-at "#\\|\\([a-zA-Z0-9_$]+\\):[^:]\\|=[a-zA-Z]")
					  (not (looking-at "[sym]:\\|tr:"))))
			    ;; Skip over comments and labels following openbrace.
			    (cond ((= (following-char) ?\#)
				   (forward-line 1))
				  ((= (following-char) ?\=)
				   (goto-char
				    (or (next-single-property-change (point) 'in-pod)
					(point-max)))) ; do not loop if no syntaxification
				  ;; label:
				  (t
				   (save-excursion (end-of-line)
						   (setq colon-line-end (point)))
				   (search-forward ":"))))
			  ;; We are at beginning of code (NOT label or comment)
			  ;; First, the following code counts
			  ;; if it is before the line we want to indent.
			  (and (< (point) indent-point)
			       (vector 'have-prev-sibling (point) colon-line-end
				       containing-sexp))))
		      (progn
			;; If no previous statement,
			;; indent it relative to line brace is on.

			;; For open-braces not the first thing in a line,
			;; add in cperl-brace-imaginary-offset.

			;; If first thing on a line:  ?????
			;; Move back over whitespace before the openbrace.
			(setq		; brace first thing on a line
			 old-indent (progn (skip-chars-backward " \t") (bolp)))
			;; Should we indent w.r.t. earlier than start?
			;; Move to start of control group, possibly on a different line
			(or cperl-indent-wrt-brace
			    (cperl-backward-to-noncomment (point-min)))
			;; If the openbrace is preceded by a parenthesized exp,
			;; move to the beginning of that;
			(if (eq (preceding-char) ?\))
			    (progn
			      (forward-sexp -1)
			      (cperl-backward-to-noncomment (point-min))))
			;; In the case it starts a subroutine, indent with
			;; respect to `sub', not with respect to the
			;; first thing on the line, say in the case of
			;; anonymous sub in a hash.
			(if (and;; Is it a sub in group starting on this line?
			     (cond ((get-text-property (point) 'attrib-group)
				    (goto-char (cperl-beginning-of-property
						(point) 'attrib-group)))
				   ((eq (preceding-char) ?b)
				    (forward-sexp -1)
				    (looking-at "sub\\>")))
			     (setq p (nth 1 ; start of innermost containing list
					  (parse-partial-sexp
					   (save-excursion (beginning-of-line)
							   (point))
					   (point)))))
			    (progn
			      (goto-char (1+ p)) ; enclosing block on the same line
			      (skip-chars-forward " \t")
			      (vector 'code-start-in-block containing-sexp char-after
				      (and delim (not is-block)) ; is a HASH
				      old-indent ; brace first thing on a line
				      t (point) ; have something before...
				      )
			      ;;(current-column)
			      )
			  ;; Get initial indentation of the line we are on.
			  ;; If line starts with label, calculate label indentation
			  (vector 'code-start-in-block containing-sexp char-after
				  (and delim (not is-block)) ; is a HASH
				  old-indent ; brace first thing on a line
				  nil (point))))))))))))))) ; nothing interesting before

(defvar cperl-indent-rules-alist
  '((pod nil)				; via `syntax-type' property
    (here-doc nil)			; via `syntax-type' property
    (here-doc-delim nil)		; via `syntax-type' property
    (format nil)			; via `syntax-type' property
    (in-pod nil)			; via `in-pod' property
    (comment-special:at-beginning-of-line nil)
    (string t)
    (comment nil))
  "Alist of indentation rules for CPerl mode.
The values mean:
  nil: do not indent;
  number: add this amount of indentation.")

(defun cperl-calculate-indent (&optional parse-data) ; was parse-start
  "Return appropriate indentation for current line as Perl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment.

Will not correct the indentation for labels, but will correct it for braces
and closing parentheses and brackets."
  ;; This code is still a broken architecture: in some cases we need to
  ;; compensate for some modifications which `cperl-indent-line' will add later
  (save-excursion
    (let ((i (cperl-sniff-for-indent parse-data)) what p)
      (cond
       ;;((or (null i) (eq i t) (numberp i))
       ;;  i)
       ((vectorp i)
	(setq what (assoc (elt i 0) cperl-indent-rules-alist))
	(cond
	 (what (cadr what))		; Load from table
	 ;;
	 ;; Indenters for regular expressions with //x and qw()
	 ;;
	 ((eq 'REx-part2 (elt i 0)) ;; [self start] start of /REP in s//REP/x
	  (goto-char (elt i 1))
	  (condition-case nil	; Use indentation of the 1st part
	      (forward-sexp -1))
	  (current-column))
	 ((eq 'indentable (elt i 0))	; Indenter for REGEXP qw() etc
	  (cond		       ;;; [indentable terminator start-pos is-block]
	   ((eq 'terminator (elt i 1)) ; Lone terminator of "indentable string"
	    (goto-char (elt i 2))	; After opening parens
	    (1- (current-column)))
	   ((eq 'first-line (elt i 1)); [indentable first-line start-pos]
	    (goto-char (elt i 2))
	    (+ (or cperl-regexp-indent-step cperl-indent-level)
	       -1
	       (current-column)))
	   ((eq 'cont-line (elt i 1)); [indentable cont-line pos prev-pos first-char start-pos]
	    ;; Indent as the level after closing parens
	    (goto-char (elt i 2))	; indent line
	    (skip-chars-forward " \t)") ; Skip closing parens
	    (setq p (point))
	    (goto-char (elt i 3))	; previous line
	    (skip-chars-forward " \t)") ; Skip closing parens
	    ;; Number of parens in between:
	    (setq p (nth 0 (parse-partial-sexp (point) p))
		  what (elt i 4))	; First char on current line
	    (goto-char (elt i 3))	; previous line
	    (+ (* p (or cperl-regexp-indent-step cperl-indent-level))
	       (cond ((eq what ?\) )
		      (- cperl-close-paren-offset)) ; compensate
		     ((eq what ?\| )
		      (- (or cperl-regexp-indent-step cperl-indent-level)))
		     (t 0))
	       (if (eq (following-char) ?\| )
		   (or cperl-regexp-indent-step cperl-indent-level)
		 0)
	       (current-column)))
	   (t
	    (error "Unrecognized value of indent: " i))))
	 ;;
	 ;; Indenter for stuff at toplevel
	 ;;
	 ((eq 'toplevel (elt i 0)) ;; [toplevel start char-after state immed-after-block]
	  (+ (save-excursion		; To beg-of-defun, or end of last sexp
	       (goto-char (elt i 1))	; start = Good place to start parsing
	       (- (current-indentation) ; 
		  (if (elt i 4) cperl-indent-level 0)))	; immed-after-block
	     (if (eq (elt i 2) ?{) cperl-continued-brace-offset 0) ; char-after
	     ;; Look at previous line that's at column 0
	     ;; to determine whether we are in top-level decls
	     ;; or function's arg decls.  Set basic-indent accordingly.
	     ;; Now add a little if this is a continuation line.
	     (if (elt i 3)		; state (XXX What is the semantic???)
		 0
	       cperl-continued-statement-offset)))
	 ;;
	 ;; Indenter for stuff in "parentheses" (or brackets, braces-as-hash)
	 ;;
	 ((eq 'in-parens (elt i 0))
	  ;; in-parens char-after old-indent-point is-brace containing-sexp

	  ;; group is an expression, not a block:
	  ;; indent to just after the surrounding open parens,
	  ;; skip blanks if we do not close the expression.
	  (+ (progn
	       (goto-char (elt i 2))		; old-indent-point
	       (current-column))
	     (if (and (elt i 3)		; is-brace
		      (eq (elt i 1) ?\})) ; char-after
		 ;; Correct indentation of trailing ?\}
		 (+ cperl-indent-level cperl-close-paren-offset)
	       0)))
	 ;;
	 ;; Indenter for continuation lines
	 ;;
	 ((eq 'continuation (elt i 0))
	  ;; [continuation statement-start char-after is-block is-brace]
	  (goto-char (elt i 1))		; statement-start
	  (+ (if (memq (elt i 2) (append "}])" nil)) ; char-after
		 0			; Closing parenth
	       cperl-continued-statement-offset)
	     (if (or (elt i 3)		; is-block
		     (not (elt i 4))		; is-brace
		     (not (eq (elt i 2) ?\}))) ; char-after
		 0
	       ;; Now it is a hash reference
	       (+ cperl-indent-level cperl-close-paren-offset))
	     ;; Labels do not take :: ...
	     (if (looking-at "\\(\\w\\|_\\)+[ \t]*:")
		 (if (> (current-indentation) cperl-min-label-indent)
		     (- (current-indentation) cperl-label-offset)
		   ;; Do not move `parse-data', this should
		   ;; be quick anyway (this comment comes
		   ;; from different location):
		   (cperl-calculate-indent))
	       (current-column))
	     (if (eq (elt i 2) ?\{)	; char-after
		 cperl-continued-brace-offset 0)))
	 ;;
	 ;; Indenter for lines in a block which are not leading lines
	 ;;
	 ((eq 'have-prev-sibling (elt i 0))
	  ;; [have-prev-sibling sibling-beg colon-line-end block-start]
	  (goto-char (elt i 1))		; sibling-beg
	  (if (> (elt i 2) (point)) ; colon-line-end; have label before point
	      (if (> (current-indentation)
		     cperl-min-label-indent)
		  (- (current-indentation) cperl-label-offset)
		;; Do not believe: `max' was involved in calculation of indent
		(+ cperl-indent-level
		   (save-excursion
		     (goto-char (elt i 3)) ; block-start
		     (current-indentation))))
	    (current-column)))
	 ;;
	 ;; Indenter for the first line in a block
	 ;;
	 ((eq 'code-start-in-block (elt i 0))
	  ;;[code-start-in-block before-brace char-after
	  ;; is-a-HASH-ref brace-is-first-thing-on-a-line
	  ;; group-starts-before-start-of-sub start-of-control-group]
	  (goto-char (elt i 1))
	  ;; For open brace in column zero, don't let statement
	  ;; start there too.  If cperl-indent-level=0,
	  ;; use cperl-brace-offset + cperl-continued-statement-offset instead.
	  (+ (if (and (bolp) (zerop cperl-indent-level))
		 (+ cperl-brace-offset cperl-continued-statement-offset)
	       cperl-indent-level)
	     (if (and (elt i 3)	; is-a-HASH-ref
		      (eq (elt i 2) ?\})) ; char-after: End of a hash reference
		 (+ cperl-indent-level cperl-close-paren-offset)
	       0)
	     ;; Unless openbrace is the first nonwhite thing on the line,
	     ;; add the cperl-brace-imaginary-offset.
	     (if (elt i 4) 0		; brace-is-first-thing-on-a-line
	       cperl-brace-imaginary-offset)
	     (progn
	       (goto-char (elt i 6))	; start-of-control-group
	       (if (elt i 5)		; group-starts-before-start-of-sub
		   (current-column)
		 ;; Get initial indentation of the line we are on.
		 ;; If line starts with label, calculate label indentation
		 (if (save-excursion
		       (beginning-of-line)
		       (looking-at "[ \t]*[a-zA-Z_][a-zA-Z_0-9]*:[^:]"))
		     (if (> (current-indentation) cperl-min-label-indent)
			 (- (current-indentation) cperl-label-offset)
		       ;; Do not move `parse-data', this should
		       ;; be quick anyway:
		       (cperl-calculate-indent))
		   (current-indentation))))))
	 (t
	  (error "Unrecognized value of indent: " i))))
       (t
	(error (format "Got strange value of indent: " i)))))))

(defun cperl-calculate-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq end (point))
      (and (= (following-char) ?#)
	   (forward-line -1)
	   (cperl-to-comment-or-eol)
	   (setq end (point)))
      (goto-char end)
      (current-column))))


(defun cperl-to-comment-or-eol ()
  "Go to position before comment on the current line, or to end of line.
Returns true if comment is found.  In POD will not move the point."
  ;; If the line is inside other syntax groups (qq-style strings, HERE-docs)
  ;; then looks for literal # or end-of-line.
  (let (state stop-in cpoint (lim (progn (end-of-line) (point))) pr e)
    (or cperl-font-locking
	(cperl-update-syntaxification lim lim))
    (beginning-of-line)
    (if (setq pr (get-text-property (point) 'syntax-type))
	(setq e (next-single-property-change (point) 'syntax-type nil (point-max))))
    (if (or (eq pr 'pod)
	    (if (or (not e) (> e lim))	; deep inside a group
		(re-search-forward "\\=[ \t]*\\(#\\|$\\)" lim t)))
	(if (eq (preceding-char) ?\#) (progn (backward-char 1) t))
      ;; Else - need to do it the hard way
      (and (and e (<= e lim))
	   (goto-char e))
      (while (not stop-in)
	(setq state (parse-partial-sexp (point) lim nil nil nil t))
					; stop at comment
	;; If fails (beginning-of-line inside sexp), then contains not-comment
	(if (nth 4 state)		; After `#';
					; (nth 2 state) can be
					; beginning of m,s,qq and so
					; on
	    (if (nth 2 state)
		(progn
		  (setq cpoint (point))
		  (goto-char (nth 2 state))
		  (cond
		   ((looking-at "\\(s\\|tr\\)\\>")
		    (or (re-search-forward
			 "\\=\\w+[ \t]*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*"
			 lim 'move)
			(setq stop-in t)))
		   ((looking-at "\\(m\\|q\\([qxwr]\\)?\\)\\>")
		    (or (re-search-forward
			 "\\=\\w+[ \t]*#\\([^\n\\\\#]\\|\\\\[\\\\#]\\)*#"
			 lim 'move)
			(setq stop-in t)))
		   (t			; It was fair comment
		    (setq stop-in t)	; Finish
		    (goto-char (1- cpoint)))))
	      (setq stop-in t)		; Finish
	      (forward-char -1))
	  (setq stop-in t)))		; Finish
      (nth 4 state))))

(defsubst cperl-modify-syntax-type (at how)
  (if (< at (point-max))
      (progn
	(put-text-property at (1+ at) 'syntax-table how)
	(put-text-property at (1+ at) 'rear-nonsticky '(syntax-table)))))

(defun cperl-protect-defun-start (s e)
  ;; C code looks for "^\\s(" to skip comment backward in "hard" situations
  (save-excursion
    (goto-char s)
    (while (re-search-forward "^\\s(" e 'to-end)
      (put-text-property (1- (point)) (point) 'syntax-table cperl-st-punct))))

(defun cperl-commentify (bb e string &optional noface)
  (if cperl-use-syntax-table-text-property
      (if (eq noface 'n)		; Only immediate
	  nil
	;; We suppose that e is _after_ the end of construction, as after eol.
	(setq string (if string cperl-st-sfence cperl-st-cfence))
	(if (> bb (- e 2))
	    ;; one-char string/comment?!
	    (cperl-modify-syntax-type bb cperl-st-punct)
	  (cperl-modify-syntax-type bb string)
	  (cperl-modify-syntax-type (1- e) string))
	(if (and (eq string cperl-st-sfence) (> (- e 2) bb))
	    (put-text-property (1+ bb) (1- e)
			       'syntax-table cperl-string-syntax-table))
	(cperl-protect-defun-start bb e))
    ;; Fontify
    (or noface
	(not cperl-pod-here-fontify)
	(put-text-property bb e 'face (if string 'font-lock-string-face
					'font-lock-comment-face)))))

(defvar cperl-starters '(( ?\( . ?\) )
			 ( ?\[ . ?\] )
			 ( ?\{ . ?\} )
			 ( ?\< . ?\> )))

(defun cperl-cached-syntax-table (st)
  "Get a syntax table cached in ST, or create and cache into ST a syntax table.
All the entries of the syntax table are \".\", except for a backslash, which
is quoting."
  (if (car-safe st)
      (car st)
    (setcar st (make-syntax-table))
    (setq st (car st))
    (let ((i 0))
      (while (< i 256)
	(modify-syntax-entry i "." st)
	(setq i (1+ i))))
    (modify-syntax-entry ?\\ "\\" st)
    st))

(defun cperl-forward-re (lim end is-2arg st-l err-l argument
			     &optional ostart oend)
"Find the end of a regular expression or a stringish construct (q[] etc).
The point should be before the starting delimiter.

Goes to LIM if none is found.  If IS-2ARG is non-nil, assumes that it
is s/// or tr/// like expression.  If END is nil, generates an error
message if needed.  If SET-ST is non-nil, will use (or generate) a
cached syntax table in ST-L.  If ERR-L is non-nil, will store the
error message in its CAR (unless it already contains some error
message).  ARGUMENT should be the name of the construct (used in error
messages).  OSTART, OEND may be set in recursive calls when processing
the second argument of 2ARG construct.

Works *before* syntax recognition is done.  In IS-2ARG situation may
modify syntax-type text property if the situation is too hard."
  (let (b starter ender st i i2 go-forward reset-st set-st)
    (skip-chars-forward " \t")
    ;; ender means matching-char matcher.
    (setq b (point)
	  starter (if (eobp) 0 (char-after b))
	  ender (cdr (assoc starter cperl-starters)))
    ;; What if starter == ?\\  ????
    (setq st (cperl-cached-syntax-table st-l))
    (setq set-st t)
    ;; Whether we have an intermediate point
    (setq i nil)
    ;; Prepare the syntax table:
    (if (not ender)		; m/blah/, s/x//, s/x/y/
	(modify-syntax-entry starter "$" st)
      (modify-syntax-entry starter (concat "(" (list ender)) st)
      (modify-syntax-entry ender  (concat ")" (list starter)) st))
    (condition-case bb
	(progn
	  ;; We use `$' syntax class to find matching stuff, but $$
	  ;; is recognized the same as $, so we need to check this manually.
	  (if (and (eq starter (char-after (cperl-1+ b)))
		   (not ender))
	      ;; $ has TeXish matching rules, so $$ equiv $...
	      (forward-char 2)
	    (setq reset-st (syntax-table))
	    (set-syntax-table st)
	    (forward-sexp 1)
	    (if (<= (point) (1+ b))
		(error "Unfinished regular expression"))
	    (set-syntax-table reset-st)
	    (setq reset-st nil)
	    ;; Now the problem is with m;blah;;
	    (and (not ender)
		 (eq (preceding-char)
		     (char-after (- (point) 2)))
		 (save-excursion
		   (forward-char -2)
		   (= 0 (% (skip-chars-backward "\\\\") 2)))
		 (forward-char -1)))
	  ;; Now we are after the first part.
	  (and is-2arg			; Have trailing part
	       (not ender)
	       (eq (following-char) starter) ; Empty trailing part
	       (progn
		 (or (eq (char-syntax (following-char)) ?.)
		     ;; Make trailing letter into punctuation
		     (cperl-modify-syntax-type (point) cperl-st-punct))
		 (setq is-2arg nil go-forward t))) ; Ignore the tail
	  (if is-2arg			; Not number => have second part
	      (progn
		(setq i (point) i2 i)
		(if ender
		    (if (memq (following-char) '(?\  ?\t ?\n ?\f))
			(progn
			  (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
			      (goto-char (match-end 0))
			    (skip-chars-forward " \t\n\f"))
			  (setq i2 (point))))
		  (forward-char -1))
		(modify-syntax-entry starter (if (eq starter ?\\) "\\" ".") st)
		(if ender (modify-syntax-entry ender "." st))
		(setq set-st nil)
		(setq ender (cperl-forward-re lim end nil st-l err-l
					      argument starter ender)
		      ender (nth 2 ender)))))
      (error (goto-char lim)
	     (setq set-st nil)
	     (if reset-st
		 (set-syntax-table reset-st))
	     (or end
		 (message
		  "End of `%s%s%c ... %c' string/RE not found: %s"
		  argument
		  (if ostart (format "%c ... %c" ostart (or oend ostart)) "")
		  starter (or ender starter) bb)
		 (or (car err-l) (setcar err-l b)))))
    (if set-st
	(progn
	  (modify-syntax-entry starter (if (eq starter ?\\) "\\" ".") st)
	  (if ender (modify-syntax-entry ender "." st))))
    ;; i: have 2 args, after end of the first arg
    ;; i2: start of the second arg, if any (before delim iff `ender').
    ;; ender: the last arg bounded by parens-like chars, the second one of them
    ;; starter: the starting delimiter of the first arg
    ;; go-forward: has 2 args, and the second part is empty
    (list i i2 ender starter go-forward)))

(defun cperl-forward-group-in-re (&optional st-l)
  "Find the end of a group in a REx.
Return the error message (if any).  Does not work if delimiter is `)'.
Works before syntax recognition is done."
  ;; Works *before* syntax recognition is done
  (or st-l (setq st-l (list nil)))	; Avoid overwriting '()
  (let (st b reset-st)
    (condition-case b
	(progn
	  (setq st (cperl-cached-syntax-table st-l))
	  (modify-syntax-entry ?\( "()" st)
	  (modify-syntax-entry ?\) ")(" st)
	  (setq reset-st (syntax-table))
	  (set-syntax-table st)
	  (forward-sexp 1))
      (error (message
	      "cperl-forward-group-in-re: error %s" b)))
    ;; now restore the initial state
    (if st
	(progn
	  (modify-syntax-entry ?\( "." st)
	  (modify-syntax-entry ?\) "." st)))
    (if reset-st
	(set-syntax-table reset-st))
    b))


(defvar font-lock-string-face)
;;(defvar font-lock-reference-face)
(defvar font-lock-constant-face)
(defsubst cperl-postpone-fontification (b e type val &optional now)
  ;; Do after syntactic fontification?
  (if cperl-syntaxify-by-font-lock
      (or now (put-text-property b e 'cperl-postpone (cons type val)))
    (put-text-property b e type val)))

;;; Here is how the global structures (those which cannot be
;;; recognized locally) are marked:
;;	a) PODs:
;;		Start-to-end is marked `in-pod' ==> t
;;		Each non-literal part is marked `syntax-type' ==> `pod'
;;		Each literal part is marked `syntax-type' ==> `in-pod'
;;	b) HEREs:
;;		Start-to-end is marked `here-doc-group' ==> t
;;		The body is marked `syntax-type' ==> `here-doc'
;;		The delimiter is marked `syntax-type' ==> `here-doc-delim'
;;	c) FORMATs:
;;		First line (to =) marked `first-format-line' ==> t
;;		After-this--to-end is marked `syntax-type' ==> `format'
;;	d) 'Q'uoted string:
;;		part between markers inclusive is marked `syntax-type' ==> `string'
;;		part between `q' and the first marker is marked `syntax-type' ==> `prestring'
;;		second part of s///e is marked `syntax-type' ==> `multiline'
;;	e) Attributes of subroutines: `attrib-group' ==> t
;;		(or 0 if declaration); up to `{' or ';': `syntax-type' => `sub-decl'.
;;      f) Multiline my/our declaration lists etc: `syntax-type' => `multiline'

;;; In addition, some parts of RExes may be marked as `REx-interpolated'
;;; (value: 0 in //o, 1 if "interpolated variable" is whole-REx, t otherwise).

(defun cperl-unwind-to-safe (before &optional end)
  ;; if BEFORE, go to the previous start-of-line on each step of unwinding
  (let ((pos (point)) opos)
    (while (and pos (progn
		      (beginning-of-line)
		      (get-text-property (setq pos (point)) 'syntax-type)))
      (setq opos pos
	    pos (cperl-beginning-of-property pos 'syntax-type))
      (if (eq pos (point-min))
	  (setq pos nil))
      (if pos
	  (if before
	      (progn
		(goto-char (cperl-1- pos))
		(beginning-of-line)
		(setq pos (point)))
	    (goto-char (setq pos (cperl-1- pos))))
	;; Up to the start
	(goto-char (point-min))))
    ;; Skip empty lines
    (and (looking-at "\n*=")
	 (/= 0 (skip-chars-backward "\n"))
	 (forward-char))
    (setq pos (point))
    (if end
	;; Do the same for end, going small steps
	(save-excursion
	  (while (and end (get-text-property end 'syntax-type))
	    (setq pos end
		  end (next-single-property-change end 'syntax-type nil (point-max)))
	    (if end (progn (goto-char end)
			   (or (bolp) (forward-line 1))
			   (setq end (point)))))
	  (or end pos)))))

;;; These are needed for byte-compile (at least with v19)
(defvar cperl-nonoverridable-face)
(defvar font-lock-variable-name-face)
(defvar font-lock-function-name-face)
(defvar font-lock-keyword-face)
(defvar font-lock-builtin-face)
(defvar font-lock-type-face)
(defvar font-lock-comment-face)
(defvar font-lock-warning-face)

(defun cperl-find-sub-attrs (&optional st-l b-fname e-fname pos)
  "Syntaxically mark (and fontify) attributes of a subroutine.
Should be called with the point before leading colon of an attribute."
  ;; Works *before* syntax recognition is done
  (or st-l (setq st-l (list nil)))	; Avoid overwriting '()
  (let (st b p reset-st after-first (start (point)) start1 end1)
    (condition-case b
	(while (looking-at
		(concat
		 "\\("			; 1=optional? colon
		   ":" cperl-maybe-white-and-comment-rex ; 2=whitespace/comment?
		 "\\)"
		 (if after-first "?" "")
		 ;; No space between name and paren allowed...
		 "\\(\\sw+\\)"		; 3=name
		 "\\((\\)?"))		; 4=optional paren
	  (and (match-beginning 1)
	       (cperl-postpone-fontification
		(match-beginning 0) (cperl-1+ (match-beginning 0))
		'face font-lock-constant-face))
	  (setq start1 (match-beginning 3) end1 (match-end 3))
	  (cperl-postpone-fontification start1 end1
					'face font-lock-constant-face)
	  (goto-char end1)		; end or before `('
	  (if (match-end 4)		; Have attribute arguments...
	      (progn
		(if st nil
		  (setq st (cperl-cached-syntax-table st-l))
		  (modify-syntax-entry ?\( "()" st)
		  (modify-syntax-entry ?\) ")(" st))
		(setq reset-st (syntax-table) p (point))
		(set-syntax-table st)
		(forward-sexp 1)
		(set-syntax-table reset-st)
		(setq reset-st nil)
		(cperl-commentify p (point) t))) ; mark as string
	  (forward-comment (buffer-size))
	  (setq after-first t))
      (error (message
	      "L%d: attribute `%s': %s"
	      (count-lines (point-min) (point))
	      (and start1 end1 (buffer-substring start1 end1)) b)
	     (setq start nil)))
    (and start
	 (progn
	   (put-text-property start (point)
			      'attrib-group (if (looking-at "{") t 0))
	   (and pos
		(< 1 (count-lines (+ 3 pos) (point))) ; end of `sub'
		;; Apparently, we do not need `multiline': faces added now
		(put-text-property (+ 3 pos) (cperl-1+ (point))
				   'syntax-type 'sub-decl))
	   (and b-fname			; Fontify here: the following condition
		(cperl-postpone-fontification ; is too hard to determine by
		 b-fname e-fname 'face ; a REx, so do it here
		(if (looking-at "{")
		    font-lock-function-name-face
		  font-lock-variable-name-face)))))
    ;; now restore the initial state
    (if st
	(progn
	  (modify-syntax-entry ?\( "." st)
	  (modify-syntax-entry ?\) "." st)))
    (if reset-st
	(set-syntax-table reset-st))))

(defsubst cperl-look-at-leading-count (is-x-REx e)
  (if (and
       (< (point) e)
       (re-search-forward (concat "\\=" (if is-x-REx "[ \t\n]*" "") "[{?+*]")
			  (1- e) t))	; return nil on failure, no moving
      (if (eq ?\{ (preceding-char)) nil
	(cperl-postpone-fontification
	 (1- (point)) (point)
	 'face font-lock-warning-face))))

;;; Debugging this may require (setq max-specpdl-size 2000)...
(defun cperl-find-pods-heres (&optional min max non-inter end ignore-max end-of-here-doc)
  "Scans the buffer for hard-to-parse Perl constructions.
If `cperl-pod-here-fontify' is not-nil after evaluation, will fontify
the sections using `cperl-pod-head-face', `cperl-pod-face',
`cperl-here-face'."
  (interactive)
 (or min (setq min (point-min)
		cperl-syntax-state nil
		cperl-syntax-done-to min))
  (or max (setq max (point-max)))
  (let* ((cperl-pod-here-fontify (eval cperl-pod-here-fontify)) go tmpend
	 face head-face here-face b e bb tag qtag b1 e1 argument i c tail tb
	 is-REx is-x-REx REx-subgr-start REx-subgr-end was-subgr i2 hairy-RE
	 (case-fold-search nil) (inhibit-read-only t) (buffer-undo-list t)
	 (modified (buffer-modified-p)) overshoot is-o-REx
	 (after-change-functions nil)
	 (cperl-font-locking t)
	 (use-syntax-state (and cperl-syntax-state
				(>= min (car cperl-syntax-state))))
	 (state-point (if use-syntax-state
			  (car cperl-syntax-state)
			(point-min)))
	 (state (if use-syntax-state
		    (cdr cperl-syntax-state)))
	 ;; (st-l '(nil)) (err-l '(nil)) ; Would overwrite - propagates from a function call to a function call!
	 (st-l (list nil)) (err-l (list nil))
	 ;; Somehow font-lock may be not loaded yet...
	 ;; (e.g., when building TAGS via command-line call)
	 (font-lock-string-face (if (boundp 'font-lock-string-face)
				    font-lock-string-face
				  'font-lock-string-face))
	 (my-cperl-delimiters-face (if (boundp 'font-lock-constant-face)
				      font-lock-constant-face
				    'font-lock-constant-face))
	 (my-cperl-REx-spec-char-face	; [] ^.$ and wrapper-of ({})
	  (if (boundp 'font-lock-function-name-face)
	      font-lock-function-name-face
	    'font-lock-function-name-face))
	 (font-lock-variable-name-face	; interpolated vars and ({})-code
	  (if (boundp 'font-lock-variable-name-face)
	      font-lock-variable-name-face
	    'font-lock-variable-name-face))
	 (font-lock-function-name-face	; used in `cperl-find-sub-attrs'
	  (if (boundp 'font-lock-function-name-face)
	      font-lock-function-name-face
	    'font-lock-function-name-face))
	 (font-lock-constant-face	; used in `cperl-find-sub-attrs'
	  (if (boundp 'font-lock-constant-face)
	      font-lock-constant-face
	    'font-lock-constant-face))
	 (my-cperl-REx-0length-face ; 0-length, (?:)etc, non-literal \
	  (if (boundp 'font-lock-builtin-face)
	      font-lock-builtin-face
	    'font-lock-builtin-face))
	 (font-lock-comment-face
	  (if (boundp 'font-lock-comment-face)
	      font-lock-comment-face
	    'font-lock-comment-face))
	 (font-lock-warning-face
	  (if (boundp 'font-lock-warning-face)
	      font-lock-warning-face
	    'font-lock-warning-face))
	 (my-cperl-REx-ctl-face		; (|)
	  (if (boundp 'font-lock-keyword-face)
	      font-lock-keyword-face
	    'font-lock-keyword-face))
	 (my-cperl-REx-modifiers-face	; //gims
	  (if (boundp 'cperl-nonoverridable-face)
	      cperl-nonoverridable-face
	    'cperl-nonoverridable-face))
	 (my-cperl-REx-length1-face	; length=1 escaped chars, POSIX classes
	  (if (boundp 'font-lock-type-face)
	      font-lock-type-face
	    'font-lock-type-face))
	 (stop-point (if ignore-max
			 (point-max)
		       max))
	 (search
	  (concat
	   "\\(\\`\n?\\|^\n\\)="	; POD
	   "\\|"
	   ;; One extra () before this:
	   "<<"				; HERE-DOC
	   "\\("			; 1 + 1
	   ;; First variant "BLAH" or just ``.
	   "[ \t]*"			; Yes, whitespace is allowed!
	   "\\([\"'`]\\)"		; 2 + 1 = 3
	   "\\([^\"'`\n]*\\)"		; 3 + 1
	   "\\3"
	   "\\|"
	   ;; Second variant: Identifier or \ID (same as 'ID') or empty
	   "\\\\?\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)?\\)" ; 4 + 1, 5 + 1
	   ;; Do not have <<= or << 30 or <<30 or << $blah.
	   ;; "\\([^= \t0-9$@%&]\\|[ \t]+[^ \t\n0-9$@%&]\\)" ; 6 + 1
	   "\\(\\)"		; To preserve count of pars :-( 6 + 1
	   "\\)"
	   "\\|"
	   ;; 1+6 extra () before this:
	   "^[ \t]*\\(format\\)[ \t]*\\([a-zA-Z0-9_]+\\)?[ \t]*=[ \t]*$" ;FRMAT
	   (if cperl-use-syntax-table-text-property
	       (concat
		"\\|"
		;; 1+6+2=9 extra () before this:
		"\\<\\(q[wxqr]?\\|[msy]\\|tr\\)\\>" ; QUOTED CONSTRUCT
		"\\|"
		;; 1+6+2+1=10 extra () before this:
		"\\([?/<]\\)"	; /blah/ or ?blah? or <file*glob>
		"\\|"
		;; 1+6+2+1+1=11 extra () before this
		"\\<sub\\>"		;  sub with proto/attr
		"\\("
		   cperl-white-and-comment-rex
		   "\\(::[a-zA-Z_:'0-9]*\\|[a-zA-Z_'][a-zA-Z_:'0-9]*\\)\\)?" ; name
		"\\("
		   cperl-maybe-white-and-comment-rex
		   "\\(([^()]*)\\|:[^:]\\)\\)" ; prototype or attribute start
		"\\|"
		;; 1+6+2+1+1+6=17 extra () before this:
		"\\$\\(['{]\\)"		; $' or ${foo}
		"\\|"
		;; 1+6+2+1+1+6+1=18 extra () before this (old pack'var syntax;
		;; we do not support intervening comments...):
		"\\(\\<sub[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'"
		;; 1+6+2+1+1+6+1+1=19 extra () before this:
		"\\|"
		"__\\(END\\|DATA\\)__"	; __END__ or __DATA__
		;; 1+6+2+1+1+6+1+1+1=20 extra () before this:
		"\\|"
		"\\\\\\(['`\"($]\\)")	; BACKWACKED something-hairy
	     ""))))
    (unwind-protect
	(progn
	  (save-excursion
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions..."))
	    ;;(message "find: %s --> %s" min max)
	    (and cperl-pod-here-fontify
		 ;; We had evals here, do not know why...
		 (setq face cperl-pod-face
		       head-face cperl-pod-head-face
		       here-face cperl-here-face))
	    (remove-text-properties min max
				    '(syntax-type t in-pod t syntax-table t
						  attrib-group t
						  REx-interpolated t
						  cperl-postpone t
						  syntax-subtype t
						  rear-nonsticky t
						  front-sticky t
						  here-doc-group t
						  first-format-line t
						  REx-part2 t
						  indentable t))
	    ;; Need to remove face as well...
	    (goto-char min)
	    (and (eq system-type 'emx)
		 (eq (point) 1)
		 (let ((case-fold-search t))
		   (looking-at "extproc[ \t]")) ; Analogue of #!
		 (cperl-commentify min
				   (save-excursion (end-of-line) (point))
				   nil))
	    (while (and
		    (< (point) max)
		    (re-search-forward search max t))
	      (setq tmpend nil)		; Valid for most cases
	      (setq b (match-beginning 0)
		    state (save-excursion (parse-partial-sexp
					   state-point b nil nil state))
		    state-point b)
	      (cond
	       ;; 1+6+2+1+1+6=17 extra () before this:
	       ;;    "\\$\\(['{]\\)"
	       ((match-beginning 18) ; $' or ${foo}
		(if (eq (preceding-char) ?\') ; $'
		    (progn
		      (setq b (1- (point))
			    state (parse-partial-sexp
				   state-point (1- b) nil nil state)
			    state-point (1- b))
		      (if (nth 3 state)	; in string
			  (cperl-modify-syntax-type (1- b) cperl-st-punct))
		      (goto-char (1+ b)))
		  ;; else: ${
		  (setq bb (match-beginning 0))
		  (cperl-modify-syntax-type bb cperl-st-punct)))
	       ;; No processing in strings/comments beyond this point:
	       ((or (nth 3 state) (nth 4 state))
		t)			; Do nothing in comment/string
	       ((match-beginning 1)	; POD section
		;;  "\\(\\`\n?\\|^\n\\)="
		(setq b (match-beginning 0)
		      state (parse-partial-sexp
			     state-point b nil nil state)
		      state-point b)
		(if (or (nth 3 state) (nth 4 state)
			(looking-at "cut\\>"))
		    (if (or (nth 3 state) (nth 4 state) ignore-max)
			nil		; Doing a chunk only
		      (message "=cut is not preceded by a POD section")
		      (or (car err-l) (setcar err-l (point))))
		  (beginning-of-line)

		  (setq b (point)
			bb b
			tb (match-beginning 0)
			b1 nil)		; error condition
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (or (re-search-forward "^\n=cut\\>" stop-point 'toend)
		      (progn
			(goto-char b)
			(if (re-search-forward "\n=cut\\>" stop-point 'toend)
			    (progn
			      (message "=cut is not preceded by an empty line")
			      (setq b1 t)
			      (or (car err-l) (setcar err-l b))))))
		  (beginning-of-line 2)	; An empty line after =cut is not POD!
		  (setq e (point))
		  (and (> e max)
		       (progn
			 (remove-text-properties
			  max e '(syntax-type t in-pod t syntax-table t
					      attrib-group t
					      REx-interpolated t
					      cperl-postpone t
					      syntax-subtype t
					      here-doc-group t
					      rear-nonsticky t
					      front-sticky t
					      first-format-line t
					      REx-part2 t
					      indentable t))
			 (setq tmpend tb)))
		  (put-text-property b e 'in-pod t)
		  (put-text-property b e 'syntax-type 'in-pod)
		  (goto-char b)
		  (while (re-search-forward "\n\n[ \t]" e t)
		    ;; We start 'pod 1 char earlier to include the preceding line
		    (beginning-of-line)
		    (put-text-property (cperl-1- b) (point) 'syntax-type 'pod)
		    (cperl-put-do-not-fontify b (point) t)
		    ;; mark the non-literal parts as PODs
		    (if cperl-pod-here-fontify
			(cperl-postpone-fontification b (point) 'face face t))
		    (re-search-forward "\n\n[^ \t\f\n]" e 'toend)
		    (beginning-of-line)
		    (setq b (point)))
		  (put-text-property (cperl-1- (point)) e 'syntax-type 'pod)
		  (cperl-put-do-not-fontify (point) e t)
		  (if cperl-pod-here-fontify
		      (progn
			;; mark the non-literal parts as PODs
			(cperl-postpone-fontification (point) e 'face face t)
			(goto-char bb)
			(if (looking-at
			     "=[a-zA-Z0-9_]+\\>[ \t]*\\(\\(\n?[^\n]\\)+\\)$")
			    ;; mark the headers
			    (cperl-postpone-fontification
			     (match-beginning 1) (match-end 1)
			     'face head-face))
			(while (re-search-forward
				;; One paragraph
				"^\n=[a-zA-Z0-9_]+\\>[ \t]*\\(\\(\n?[^\n]\\)+\\)$"
				e 'toend)
			  ;; mark the headers
			  (cperl-postpone-fontification
			   (match-beginning 1) (match-end 1)
			   'face head-face))))
		  (cperl-commentify bb e nil)
		  (goto-char e)
		  (or (eq e (point-max))
		      (forward-char -1)))) ; Prepare for immediate POD start.
	       ;; Here document
	       ;; We can do many here-per-line;
	       ;; but multiline quote on the same line as <<HERE confuses us...
               ;; ;; One extra () before this:
	       ;;"<<"
	       ;;  "\\("			; 1 + 1
	       ;;  ;; First variant "BLAH" or just ``.
	       ;;     "[ \t]*"			; Yes, whitespace is allowed!
	       ;;     "\\([\"'`]\\)"	; 2 + 1
	       ;;     "\\([^\"'`\n]*\\)"	; 3 + 1
	       ;;     "\\3"
	       ;;  "\\|"
	       ;;  ;; Second variant: Identifier or \ID or empty
	       ;;    "\\\\?\\(\\([a-zA-Z_][a-zA-Z_0-9]*\\)?\\)" ; 4 + 1, 5 + 1
	       ;;    ;; Do not have <<= or << 30 or <<30 or << $blah.
	       ;;    ;; "\\([^= \t0-9$@%&]\\|[ \t]+[^ \t\n0-9$@%&]\\)" ; 6 + 1
	       ;;    "\\(\\)"		; To preserve count of pars :-( 6 + 1
	       ;;  "\\)"
	       ((match-beginning 2)	; 1 + 1
		(setq b (point)
		      tb (match-beginning 0)
		      c (and		; not HERE-DOC
			 (match-beginning 5)
			 (save-match-data
			   (or (looking-at "[ \t]*(") ; << function_call()
			       (save-excursion ; 1 << func_name, or $foo << 10
				 (condition-case nil
				     (progn
				       (goto-char tb)
	       ;;; XXX What to do: foo <<bar ???
	       ;;; XXX Need to support print {a} <<B ???
				       (forward-sexp -1)
				       (save-match-data	
					; $foo << b; $f .= <<B;
					; ($f+1) << b; a($f) . <<B;
					; foo 1, <<B; $x{a} <<b;
					 (cond
					  ((looking-at "[0-9$({]")
					   (forward-sexp 1)
					   (and
					    (looking-at "[ \t]*<<")
					    (condition-case nil
						;; print $foo <<EOF
						(progn
						  (forward-sexp -2)
						  (not
						   (looking-at "\\(printf?\\|system\\|exec\\|sort\\)\\>")))
						(error t)))))))
				   (error nil))) ; func(<<EOF)
			       (and (not (match-beginning 6)) ; Empty
				    (looking-at
				     "[ \t]*[=0-9$@%&(]"))))))
		(if c			; Not here-doc
		    nil			; Skip it.
		  (setq c (match-end 2)) ; 1 + 1
		  (if (match-beginning 5) ;4 + 1
		      (setq b1 (match-beginning 5) ; 4 + 1
			    e1 (match-end 5)) ; 4 + 1
		    (setq b1 (match-beginning 4) ; 3 + 1
			  e1 (match-end 4))) ; 3 + 1
		  (setq tag (buffer-substring b1 e1)
			qtag (regexp-quote tag))
		  (cond (cperl-pod-here-fontify
			 ;; Highlight the starting delimiter
			 (cperl-postpone-fontification 
			  b1 e1 'face my-cperl-delimiters-face)
			 (cperl-put-do-not-fontify b1 e1 t)))
		  (forward-line)
		  (setq i (point))
		  (if end-of-here-doc
		      (goto-char end-of-here-doc))
		  (setq b (point))
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (or (and (re-search-forward (concat "^" qtag "$")
					      stop-point 'toend)
			   ;;;(eq (following-char) ?\n) ; XXXX WHY???
			   )
		    (progn		; Pretend we matched at the end
		      (goto-char (point-max))
		      (re-search-forward "\\'")
		      (message "End of here-document `%s' not found." tag)
		      (or (car err-l) (setcar err-l b))))
		  (if cperl-pod-here-fontify
		      (progn
			;; Highlight the ending delimiter
			(cperl-postpone-fontification
			 (match-beginning 0) (match-end 0)
			 'face my-cperl-delimiters-face)
			(cperl-put-do-not-fontify b (match-end 0) t)
			;; Highlight the HERE-DOC
			(cperl-postpone-fontification b (match-beginning 0)
						      'face here-face)))
		  (setq e1 (cperl-1+ (match-end 0)))
		  (put-text-property b (match-beginning 0)
				     'syntax-type 'here-doc)
		  (put-text-property (match-beginning 0) e1
				     'syntax-type 'here-doc-delim)
		  (put-text-property b e1 'here-doc-group t)
		  ;; This makes insertion at the start of HERE-DOC update
		  ;; the whole construct:
		  (put-text-property b (cperl-1+ b) 'front-sticky '(syntax-type))
		  (cperl-commentify b e1 nil)
		  (cperl-put-do-not-fontify b (match-end 0) t)
		  ;; Cache the syntax info...
		  (setq cperl-syntax-state (cons state-point state))
		  ;; ... and process the rest of the line...
		  (setq overshoot
			(elt		; non-inter ignore-max
			 (cperl-find-pods-heres c i t end t e1) 1))
		  (if (and overshoot (> overshoot (point)))
		      (goto-char overshoot)
		    (setq overshoot e1))
		  (if (> e1 max)
		      (setq tmpend tb))))
	       ;; format
	       ((match-beginning 8)
		;; 1+6=7 extra () before this:
		;;"^[ \t]*\\(format\\)[ \t]*\\([a-zA-Z0-9_]+\\)?[ \t]*=[ \t]*$"
		(setq b (point)
		      name (if (match-beginning 8) ; 7 + 1
			       (buffer-substring (match-beginning 8) ; 7 + 1
						 (match-end 8)) ; 7 + 1
			     "")
		      tb (match-beginning 0))
		(setq argument nil)
		(put-text-property (save-excursion
				     (beginning-of-line)
				     (point))
				   b 'first-format-line 't)
		(if cperl-pod-here-fontify
		    (while (and (eq (forward-line) 0)
				(not (looking-at "^[.;]$")))
		      (cond
		       ((looking-at "^#")) ; Skip comments
		       ((and argument	; Skip argument multi-lines
			     (looking-at "^[ \t]*{"))
			(forward-sexp 1)
			(setq argument nil))
		       (argument	; Skip argument lines
			(setq argument nil))
		       (t		; Format line
			(setq b1 (point))
			(setq argument (looking-at "^[^\n]*[@^]"))
			(end-of-line)
			;; Highlight the format line
			(cperl-postpone-fontification b1 (point)
						      'face font-lock-string-face)
			(cperl-commentify b1 (point) nil)
			(cperl-put-do-not-fontify b1 (point) t))))
		  ;; We do not search to max, since we may be called from
		  ;; some hook of fontification, and max is random
		  (re-search-forward "^[.;]$" stop-point 'toend))
		(beginning-of-line)
		(if (looking-at "^\\.$") ; ";" is not supported yet
		    (progn
		      ;; Highlight the ending delimiter
		      (cperl-postpone-fontification (point) (+ (point) 2)
						    'face font-lock-string-face)
		      (cperl-commentify (point) (+ (point) 2) nil)
		      (cperl-put-do-not-fontify (point) (+ (point) 2) t))
		  (message "End of format `%s' not found." name)
		  (or (car err-l) (setcar err-l b)))
		(forward-line)
		(if (> (point) max)
		    (setq tmpend tb))
		(put-text-property b (point) 'syntax-type 'format))
	       ;; qq-like String or Regexp:
	       ((or (match-beginning 10) (match-beginning 11))
		;; 1+6+2=9 extra () before this:
		;; "\\<\\(q[wxqr]?\\|[msy]\\|tr\\)\\>"
		;; "\\|"
		;; "\\([?/<]\\)"	; /blah/ or ?blah? or <file*glob>
		(setq b1 (if (match-beginning 10) 10 11)
		      argument (buffer-substring
				(match-beginning b1) (match-end b1))
		      b (point)		; end of qq etc
		      i b
		      c (char-after (match-beginning b1))
		      bb (char-after (1- (match-beginning b1))) ; tmp holder
		      ;; bb == "Not a stringy"
		      bb (if (eq b1 10) ; user variables/whatever
			     (and (memq bb (append "$@%*#_:-&>" nil)) ; $#y)
				  (cond ((eq bb ?-) (eq c ?s)) ; -s file test
					((eq bb ?\:) ; $opt::s
					 (eq (char-after
					      (- (match-beginning b1) 2))
					     ?\:))
					((eq bb ?\>) ; $foo->s
					 (eq (char-after
					      (- (match-beginning b1) 2))
					     ?\-))
					((eq bb ?\&)
					 (not (eq (char-after ; &&m/blah/
						   (- (match-beginning b1) 2))
						  ?\&)))
					(t t)))
			   ;; <file> or <$file>
			   (and (eq c ?\<)
				;; Do not stringify <FH>, <$fh> :
				(save-match-data
				  (looking-at
				   "\\$?\\([_a-zA-Z:][_a-zA-Z0-9:]*\\)?>"))))
		      tb (match-beginning 0))
		(goto-char (match-beginning b1))
		(cperl-backward-to-noncomment (point-min))
		(or bb
		    (if (eq b1 11)	; bare /blah/ or ?blah? or <foo>
			(setq argument ""
			      b1 nil
			      bb	; Not a regexp?
			      (not
			       ;; What is below: regexp-p?
			       (and
				(or (memq (preceding-char)
					  (append (if (memq c '(?\? ?\<))
						      ;; $a++ ? 1 : 2
						      "~{(=|&*!,;:["
						    "~{(=|&+-*!,;:[") nil))
				    (and (eq (preceding-char) ?\})
					 (cperl-after-block-p (point-min)))
				    (and (eq (char-syntax (preceding-char)) ?w)
					 (progn
					   (forward-sexp -1)
;;; After these keywords `/' starts a RE.  One should add all the
;;; functions/builtins which expect an argument, but ...
					   (if (eq (preceding-char) ?-)
					       ;; -d ?foo? is a RE
					       (looking-at "[a-zA-Z]\\>")
					     (and
					      (not (memq (preceding-char)
							 '(?$ ?@ ?& ?%)))
					      (looking-at
					       "\\(while\\|if\\|unless\\|until\\|and\\|or\\|not\\|xor\\|split\\|grep\\|map\\|print\\)\\>")))))
				    (and (eq (preceding-char) ?.)
					 (eq (char-after (- (point) 2)) ?.))
				    (bobp))
				;;  m|blah| ? foo : bar;
				(not
				 (and (eq c ?\?)
				      cperl-use-syntax-table-text-property
				      (not (bobp))
				      (progn
					(forward-char -1)
					(looking-at "\\s|"))))))
			      b (1- b))
		      ;; s y tr m
		      ;; Check for $a -> y
		      (setq b1 (preceding-char)
			    go (point))
		      (if (and (eq b1 ?>)
			       (eq (char-after (- go 2)) ?-))
			  ;; Not a regexp
			  (setq bb t))))
		(or bb
		    (progn
		      (goto-char b)
		      (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
			  (goto-char (match-end 0))
			(skip-chars-forward " \t\n\f"))
		      (cond ((and (eq (following-char) ?\})
				  (eq b1 ?\{))
			     ;; Check for $a[23]->{ s }, @{s} and *{s::foo}
			     (goto-char (1- go))
			     (skip-chars-backward " \t\n\f")
			     (if (memq (preceding-char) (append "$@%&*" nil))
				 (setq bb t) ; @{y}
			       (condition-case nil
				   (forward-sexp -1)
				 (error nil)))
			     (if (or bb
				     (looking-at ; $foo -> {s}
				      "[$@]\\$*\\([a-zA-Z0-9_:]+\\|[^{]\\)\\([ \t\n]*->\\)?[ \t\n]*{")
				     (and ; $foo[12] -> {s}
				      (memq (following-char) '(?\{ ?\[))
				      (progn
					(forward-sexp 1)
					(looking-at "\\([ \t\n]*->\\)?[ \t\n]*{"))))
				 (setq bb t)
			       (goto-char b)))
			    ((and (eq (following-char) ?=)
				  (eq (char-after (1+ (point))) ?\>))
			     ;; Check for { foo => 1, s => 2 }
			     ;; Apparently s=> is never a substitution...
			     (setq bb t))
			    ((and (eq (following-char) ?:)
				  (eq b1 ?\{) ; Check for $ { s::bar }
				  (looking-at "::[a-zA-Z0-9_:]*[ \t\n\f]*}")
				  (progn
				    (goto-char (1- go))
				    (skip-chars-backward " \t\n\f")
				    (memq (preceding-char)
					  (append "$@%&*" nil))))
			     (setq bb t))
			    ((eobp)
			     (setq bb t)))))
		(if bb
		    (goto-char i)
		  ;; Skip whitespace and comments...
		  (if (looking-at "[ \t\n\f]+\\(#[^\n]*\n[ \t\n\f]*\\)+")
		      (goto-char (match-end 0))
		    (skip-chars-forward " \t\n\f"))
		  (if (> (point) b)
		      (put-text-property b (point) 'syntax-type 'prestring))
		  ;; qtag means two-arg matcher, may be reset to
		  ;;   2 or 3 later if some special quoting is needed.
		  ;; e1 means matching-char matcher.
		  (setq b (point)	; before the first delimiter
			;; has 2 args
			i2 (string-match "^\\([sy]\\|tr\\)$" argument)
			;; We do not search to max, since we may be called from
			;; some hook of fontification, and max is random
			i (cperl-forward-re stop-point end
					    i2
					    st-l err-l argument)
			;; If `go', then it is considered as 1-arg, `b1' is nil
			;; as in s/foo//x; the point is before final "slash"
			b1 (nth 1 i)	; start of the second part
			tag (nth 2 i)	; ender-char, true if second part
					; is with matching chars []
			go (nth 4 i)	; There is a 1-char part after the end
			i (car i)	; intermediate point
			e1 (point)	; end
			;; Before end of the second part if non-matching: ///
			tail (if (and i (not tag))
				 (1- e1))
			e (if i i e1)	; end of the first part
			qtag nil	; need to preserve backslashitis
			is-x-REx nil is-o-REx nil); REx has //x //o modifiers
		  ;; If s{} (), then b/b1 are at "{", "(", e1/i after ")", "}"
		  ;; Commenting \\ is dangerous, what about ( ?
		  (and i tail
		       (eq (char-after i) ?\\)
		       (setq qtag t))
		  (and (if go (looking-at ".\\sw*x")
			 (looking-at "\\sw*x")) ; qr//x
		       (setq is-x-REx t))
		  (and (if go (looking-at ".\\sw*o")
			 (looking-at "\\sw*o")) ; //o
		       (setq is-o-REx t))
		  (if (null i)
		      ;; Considered as 1arg form
		      (progn
			(cperl-commentify b (point) t)
			(put-text-property b (point) 'syntax-type 'string)
			(if (or is-x-REx
				;; ignore other text properties:
				(string-match "^qw$" argument))
			    (put-text-property b (point) 'indentable t))
			(and go
			     (setq e1 (cperl-1+ e1))
			     (or (eobp)
				 (forward-char 1))))
		    (cperl-commentify b i t)
		    (if (looking-at "\\sw*e") ; s///e
			(progn
			  ;; Cache the syntax info...
			  (setq cperl-syntax-state (cons state-point state))
			  (and
			   ;; silent:
			   (car (cperl-find-pods-heres b1 (1- (point)) t end))
			   ;; Error
			   (goto-char (1+ max)))
			  (if (and tag (eq (preceding-char) ?\>))
			      (progn
				(cperl-modify-syntax-type (1- (point)) cperl-st-ket)
				(cperl-modify-syntax-type i cperl-st-bra)))
			  (put-text-property b i 'syntax-type 'string)
			  (put-text-property i (point) 'syntax-type 'multiline)
			  (if is-x-REx
			      (put-text-property b i 'indentable t)))
		      (cperl-commentify b1 (point) t)
		      (put-text-property b (point) 'syntax-type 'string)
		      (if is-x-REx
			  (put-text-property b i 'indentable t))
		      (if qtag
			  (cperl-modify-syntax-type (1+ i) cperl-st-punct))
		      (setq tail nil)))
		  ;; Now: tail: if the second part is non-matching without ///e
		  (if (eq (char-syntax (following-char)) ?w)
		      (progn
			(forward-word 1) ; skip modifiers s///s
			(if tail (cperl-commentify tail (point) t))
			(cperl-postpone-fontification
			 e1 (point) 'face my-cperl-REx-modifiers-face)))
		  ;; Check whether it is m// which means "previous match"
		  ;; and highlight differently
		  (setq is-REx
			(and (string-match "^\\([sm]?\\|qr\\)$" argument)
			     (or (not (= (length argument) 0))
				 (not (eq c ?\<)))))
		  (if (and is-REx
			   (eq e (+ 2 b))
			   ;; split // *is* using zero-pattern
			   (save-excursion
			     (condition-case nil
				 (progn
				   (goto-char tb)
				   (forward-sexp -1)
				   (not (looking-at "split\\>")))
			       (error t))))
		      (cperl-postpone-fontification
		       b e 'face font-lock-warning-face)
		    (if (or i2		; Has 2 args
			    (and cperl-fontify-m-as-s
				 (or
				  (string-match "^\\(m\\|qr\\)$" argument)
				  (and (eq 0 (length argument))
				       (not (eq ?\< (char-after b)))))))
			(progn
			  (cperl-postpone-fontification
			   b (cperl-1+ b) 'face my-cperl-delimiters-face)
			  (cperl-postpone-fontification
			   (1- e) e 'face my-cperl-delimiters-face)))
		    (if (and is-REx cperl-regexp-scan)
			;; Process RExen: embedded comments, charclasses and ]
;;;/\3333\xFg\x{FFF}a\ppp\PPP\qqq\C\99f(?{  foo  })(??{  foo  })/;
;;;/a\.b[^a[:ff:]b]x$ab->$[|$,$ab->[cd]->[ef]|$ab[xy].|^${a,b}{c,d}/;
;;;/(?<=foo)(?<!bar)(x)(?:$ab|\$\/)$|\\\b\x888\776\[\:$/xxx;
;;;m?(\?\?{b,a})? + m/(??{aa})(?(?=xx)aa|bb)(?#aac)/;
;;;m$(^ab[c]\$)$ + m+(^ab[c]\$\+)+ + m](^ab[c\]$|.+)] + m)(^ab[c]$|.+\));
;;;m^a[\^b]c^ + m.a[^b]\.c.;
			(save-excursion
			  (goto-char (1+ b))
			  ;; First 
			  (cperl-look-at-leading-count is-x-REx e)
			  (setq hairy-RE
				(concat
				 (if is-x-REx
				     (if (eq (char-after b) ?\#)
					 "\\((\\?\\\\#\\)\\|\\(\\\\#\\)"
				       "\\((\\?#\\)\\|\\(#\\)")
				   ;; keep the same count: add a fake group
				   (if (eq (char-after b) ?\#)
				       "\\((\\?\\\\#\\)\\(\\)"
				     "\\((\\?#\\)\\(\\)"))
				 "\\|"
				    "\\(\\[\\)" ; 3=[
				 "\\|"
				    "\\(]\\)" ; 4=]
				 "\\|"
				 ;; XXXX Will not be able to use it in s)))
				 (if (eq (char-after b) ?\) )
				     "\\())))\\)" ; Will never match
				   (if (eq (char-after b) ?? )
				       ;;"\\((\\\\\\?\\(\\\\\\?\\)?{\\)"
				       "\\((\\\\\\?\\\\\\?{\\|()\\\\\\?{\\)"
				     "\\((\\?\\??{\\)")) ; 5= (??{ (?{
				 "\\|"	; 6= 0-length, 7: name, 8,9:code, 10:group
				    "\\(" ;; XXXX 1-char variables, exc. |()\s
				       "[$@]"
				       "\\("
				          "[_a-zA-Z:][_a-zA-Z0-9:]*"
				       "\\|"
				          "{[^{}]*}" ; only one-level allowed
				       "\\|"
				          "[^{(|) \t\r\n\f]"
				       "\\)"
				       "\\(" ;;8,9:code part of array/hash elt
				          "\\(" "->" "\\)?"
				          "\\[[^][]*\\]"
					  "\\|"
				          "{[^{}]*}"
				       "\\)*"
				    ;; XXXX: what if u is delim?
				    "\\|"
				       "[)^|$.*?+]"
				    "\\|"
				       "{[0-9]+}"
				    "\\|"
				       "{[0-9]+,[0-9]*}"
				    "\\|"
				       "\\\\[luLUEQbBAzZG]"
				    "\\|"
				       "(" ; Group opener
				       "\\(" ; 10 group opener follower
				          "\\?\\((\\?\\)" ; 11: in (?(?=C)A|B)
				       "\\|"
				          "\\?[:=!>?{]"	; "?" something
				       "\\|"
				          "\\?[-imsx]+[:)]" ; (?i) (?-s:.)
				       "\\|"
				          "\\?([0-9]+)"	; (?(1)foo|bar)
				       "\\|"
					  "\\?<[=!]"
				       ;;;"\\|"
				       ;;;   "\\?"
				       "\\)?"
				    "\\)"
				 "\\|"
				    "\\\\\\(.\\)" ; 12=\SYMBOL
				 ))
			  (while
			      (and (< (point) (1- e))
				   (re-search-forward hairy-RE (1- e) 'to-end))
			    (goto-char (match-beginning 0))
			    (setq REx-subgr-start (point)
				  was-subgr (following-char))
			    (cond
			     ((match-beginning 6) ; 0-length builtins, groups
			      (goto-char (match-end 0))
			      (if (match-beginning 11)
				  (goto-char (match-beginning 11)))
			      (if (>= (point) e)
				  (goto-char (1- e)))
			      (cperl-postpone-fontification
			       (match-beginning 0) (point)
			       'face
			       (cond
				((eq was-subgr ?\) )
				 (condition-case nil
				     (save-excursion
				       (forward-sexp -1)
				       (if (> (point) b)
					   (if (if (eq (char-after b) ?? )
						   (looking-at "(\\\\\\?")
						 (eq (char-after (1+ (point))) ?\?))
					       my-cperl-REx-0length-face
					     my-cperl-REx-ctl-face)
					 font-lock-warning-face))
				   (error font-lock-warning-face)))
				((eq was-subgr ?\| )
				 my-cperl-REx-ctl-face)
				((eq was-subgr ?\$ )
				 (if (> (point) (1+ REx-subgr-start))
				     (progn
				       (put-text-property
					(match-beginning 0) (point)
					'REx-interpolated
					(if is-o-REx 0
					    (if (and (eq (match-beginning 0)
							 (1+ b))
						     (eq (point)
							 (1- e))) 1 t)))
				       font-lock-variable-name-face)
				   my-cperl-REx-spec-char-face))
				((memq was-subgr (append "^." nil) )
				 my-cperl-REx-spec-char-face)
				((eq was-subgr ?\( )
				 (if (not (match-beginning 10))
				     my-cperl-REx-ctl-face
				   my-cperl-REx-0length-face))
				(t my-cperl-REx-0length-face)))
			      (if (and (memq was-subgr (append "(|" nil))
				       (not (string-match "(\\?[-imsx]+)"
							  (match-string 0))))
				  (cperl-look-at-leading-count is-x-REx e))
			      (setq was-subgr nil)) ; We do stuff here
			     ((match-beginning 12) ; \SYMBOL
			      (forward-char 2)
			      (if (>= (point) e)
				  (goto-char (1- e))
				;; How many chars to not highlight:
				;; 0-len special-alnums in other branch =>
				;; Generic:  \non-alnum (1), \alnum (1+face)
				;; Is-delim: \non-alnum (1/spec-2) alnum-1 (=what hai)
				(setq REx-subgr-start (point)
				      qtag (preceding-char))
				(cperl-postpone-fontification
				 (- (point) 2) (- (point) 1) 'face
				 (if (memq qtag
					   (append "ghijkmoqvFHIJKMORTVY" nil))
				     font-lock-warning-face
				   my-cperl-REx-0length-face))
				(if (and (eq (char-after b) qtag)
					 (memq qtag (append ".])^$|*?+" nil)))
				    (progn
				      (if (and cperl-use-syntax-table-text-property
					       (eq qtag ?\) ))
					  (put-text-property
					   REx-subgr-start (1- (point))
					   'syntax-table cperl-st-punct))
				      (cperl-postpone-fontification
				       (1- (point)) (point) 'face
					; \] can't appear below
				       (if (memq qtag (append ".]^$" nil))
					   'my-cperl-REx-spec-char-face
					 (if (memq qtag (append "*?+" nil))
					     'my-cperl-REx-0length-face
					   'my-cperl-REx-ctl-face))))) ; )|
				;; Test for arguments:
				(cond
				 ;; This is not pretty: the 5.8.7 logic:
				 ;; \0numx  -> octal (up to total 3 dig)
				 ;; \DIGIT  -> backref unless \0
				 ;; \DIGITs -> backref if legal
				 ;;	     otherwise up to 3 -> octal
				 ;; Do not try to distinguish, we guess
				 ((or (and (memq qtag (append "01234567" nil))
					   (re-search-forward
					    "\\=[01234567]?[01234567]?"
					    (1- e) 'to-end))
				      (and (memq qtag (append "89" nil))
					   (re-search-forward 
					    "\\=[0123456789]*" (1- e) 'to-end))
				      (and (eq qtag ?x)
					   (re-search-forward
					    "\\=[0-9a-fA-F][0-9a-fA-F]?\\|\\={[0-9a-fA-F]+}"
					    (1- e) 'to-end))
				      (and (memq qtag (append "pPN" nil))
					   (re-search-forward "\\={[^{}]+}\\|."
					    (1- e) 'to-end))
				      (eq (char-syntax qtag) ?w))
				  (cperl-postpone-fontification
				   (1- REx-subgr-start) (point)
				   'face my-cperl-REx-length1-face))))
			      (setq was-subgr nil)) ; We do stuff here
			     ((match-beginning 3) ; [charclass]
			      (forward-char 1)
			      (if (eq (char-after b) ?^ )
				  (and (eq (following-char) ?\\ )
				       (eq (char-after (cperl-1+ (point)))
					   ?^ )
				       (forward-char 2))
				(and (eq (following-char) ?^ )
				     (forward-char 1)))
			      (setq argument b ; continue?
				    tag nil ; list of POSIX classes
				    qtag (point))
			      (if (eq (char-after b) ?\] )
				  (and (eq (following-char) ?\\ )
				       (eq (char-after (cperl-1+ (point)))
					   ?\] )
				       (setq qtag (1+ qtag))
				       (forward-char 2))
				(and (eq (following-char) ?\] )
				     (forward-char 1)))
			      ;; Apparently, I can't put \] into a charclass
			      ;; in m]]: m][\\\]\]] produces [\\]]
;;; POSIX?  [:word:] [:^word:] only inside []
;;;				       "\\=\\(\\\\.\\|[^][\\\\]\\|\\[:\\^?\sw+:]\\|\\[[^:]\\)*]")
			      (while 
				  (and argument
				       (re-search-forward
					(if (eq (char-after b) ?\] )
					    "\\=\\(\\\\[^]]\\|[^]\\\\]\\)*\\\\]"
					  "\\=\\(\\\\.\\|[^]\\\\]\\)*]")
					(1- e) 'toend))
				;; Is this ] an end of POSIX class?
				(if (save-excursion
				      (and
				       (search-backward "[" argument t)
				       (< REx-subgr-start (point))
				       (not
					(and ; Should work with delim = \
					 (eq (preceding-char) ?\\ )
					 (= (% (skip-chars-backward
						"\\\\") 2) 0)))
				       (looking-at
					(cond
					 ((eq (char-after b) ?\] )
					  "\\\\*\\[:\\^?\\sw+:\\\\\\]")
					 ((eq (char-after b) ?\: )
					  "\\\\*\\[\\\\:\\^?\\sw+\\\\:]")
					 ((eq (char-after b) ?^ )
					  "\\\\*\\[:\\(\\\\\\^\\)?\\sw+:\]")
					 ((eq (char-syntax (char-after b))
					      ?w)
					  (concat
					   "\\\\*\\[:\\(\\\\\\^\\)?\\(\\\\"
					   (char-to-string (char-after b))
					   "\\|\\sw\\)+:\]"))
					 (t "\\\\*\\[:\\^?\\sw*:]")))
				       (setq argument (point))))
				    (setq tag (cons (cons argument (point))
						    tag)
					  argument (point)) ; continue
				  (setq argument nil)))
			      (and argument
				   (message "Couldn't find end of charclass in a REx, pos=%s"
					    REx-subgr-start))
			      (if (and cperl-use-syntax-table-text-property
				       (> (- (point) 2) REx-subgr-start))
				  (put-text-property
				   (1+ REx-subgr-start) (1- (point))
				   'syntax-table cperl-st-punct))
			      (cperl-postpone-fontification
			       REx-subgr-start qtag
			       'face my-cperl-REx-spec-char-face)
			      (cperl-postpone-fontification
			       (1- (point)) (point) 'face
			       my-cperl-REx-spec-char-face)
			      (if (eq (char-after b) ?\] )
				  (cperl-postpone-fontification
				   (- (point) 2) (1- (point))
				   'face my-cperl-REx-0length-face))
			      (while tag
				(cperl-postpone-fontification
				 (car (car tag)) (cdr (car tag))
				 'face my-cperl-REx-length1-face)
				(setq tag (cdr tag)))
			      (setq was-subgr nil)) ; did facing already
			     ;; Now rare stuff:
			     ((and (match-beginning 2) ; #-comment
				   (/= (match-beginning 2) (match-end 2)))
			      (beginning-of-line 2)
			      (if (> (point) e)
				  (goto-char (1- e))))
			     ((match-beginning 4) ; character "]"
			      (setq was-subgr nil) ; We do stuff here
			      (goto-char (match-end 0))
			      (if cperl-use-syntax-table-text-property
				  (put-text-property
				   (1- (point)) (point)
				   'syntax-table cperl-st-punct))
			      (cperl-postpone-fontification
			       (1- (point)) (point)
			       'face font-lock-warning-face))
			     ((match-beginning 5) ; before (?{}) (??{})
			      (setq tag (match-end 0))
			      (if (or (setq qtag
					    (cperl-forward-group-in-re st-l))
				      (and (>= (point) e)
					   (setq qtag "no matching `)' found"))
				      (and (not (eq (char-after (- (point) 2))
						    ?\} ))
					   (setq qtag "Can't find })")))
				  (progn
				    (goto-char (1- e))
				    (message qtag))
				(cperl-postpone-fontification
				 (1- tag) (1- (point))
				 'face font-lock-variable-name-face)
				(cperl-postpone-fontification
				 REx-subgr-start (1- tag)
				 'face my-cperl-REx-spec-char-face)
				(cperl-postpone-fontification
				 (1- (point)) (point)
				 'face my-cperl-REx-spec-char-face)
				(if cperl-use-syntax-table-text-property
				    (progn
				      (put-text-property
				       (- (point) 2) (1- (point))
				       'syntax-table cperl-st-cfence)
				      (put-text-property
				       (+ REx-subgr-start 2)
				       (+ REx-subgr-start 3)
				       'syntax-table cperl-st-cfence))))
			      (setq was-subgr nil))
			     (t		; (?#)-comment
			      ;; Inside "(" and "\" arn't special in any way
			      ;; Works also if the outside delimiters are ().
			      (or;;(if (eq (char-after b) ?\) )
			       ;;(re-search-forward
			       ;; "[^\\\\]\\(\\\\\\\\\\)*\\\\)"
			       ;; (1- e) 'toend)
			       (search-forward ")" (1- e) 'toend)
			       ;;)
			       (message
				"Couldn't find end of (?#...)-comment in a REx, pos=%s"
				REx-subgr-start))))
			    (if (>= (point) e)
				(goto-char (1- e)))
			    (cond
			     (was-subgr
			      (setq REx-subgr-end (point))
			      (cperl-commentify
			       REx-subgr-start REx-subgr-end nil)
			      (cperl-postpone-fontification
			       REx-subgr-start REx-subgr-end
			       'face font-lock-comment-face))))))
		    (if (and is-REx is-x-REx)
			(put-text-property (1+ b) (1- e)
					   'syntax-subtype 'x-REx)))
		  (if (and i2 e1 b1 (> e1 b1))
		      (progn		; No errors finding the second part...
			(cperl-postpone-fontification
			 (1- e1) e1 'face my-cperl-delimiters-face)
			(if (assoc (char-after b) cperl-starters)
			    (progn
			      (cperl-postpone-fontification
			       b1 (1+ b1) 'face my-cperl-delimiters-face)
			      (put-text-property b1 (1+ b1)
					   'REx-part2 t)))))
		  (if (> (point) max)
		      (setq tmpend tb))))
	       ((match-beginning 17)	; sub with prototype or attribute
		;; 1+6+2+1+1=11 extra () before this (sub with proto/attr):
		;;"\\<sub\\>\\("			;12
		;;   cperl-white-and-comment-rex	;13
		;;   "\\([a-zA-Z_:'0-9]+\\)\\)?" ; name	;14
		;;"\\(" cperl-maybe-white-and-comment-rex	;15,16
		;;   "\\(([^()]*)\\|:[^:]\\)\\)" ; 17:proto or attribute start
		(setq b1 (match-beginning 14) e1 (match-end 14))
		(if (memq (char-after (1- b))
			  '(?\$ ?\@ ?\% ?\& ?\*))
		    nil
		  (goto-char b)
		  (if (eq (char-after (match-beginning 17)) ?\( )
		      (progn
			(cperl-commentify ; Prototypes; mark as string
			 (match-beginning 17) (match-end 17) t)
			(goto-char (match-end 0))
			;; Now look for attributes after prototype:
			(forward-comment (buffer-size))
			(and (looking-at ":[^:]")
			     (cperl-find-sub-attrs st-l b1 e1 b)))
		    ;; treat attributes without prototype
		    (goto-char (match-beginning 17))
		    (cperl-find-sub-attrs st-l b1 e1 b))))
	       ;; 1+6+2+1+1+6+1=18 extra () before this:
	       ;;    "\\(\\<sub[ \t\n\f]+\\|[&*$@%]\\)[a-zA-Z0-9_]*'")
	       ((match-beginning 19)	; old $abc'efg syntax
		(setq bb (match-end 0))
		;;;(if (nth 3 state) nil	; in string
		(put-text-property (1- bb) bb 'syntax-table cperl-st-word)
		(goto-char bb))
	       ;; 1+6+2+1+1+6+1+1=19 extra () before this:
	       ;; "__\\(END\\|DATA\\)__"
	       ((match-beginning 20)	; __END__, __DATA__
		(setq bb (match-end 0))
		;; (put-text-property b (1+ bb) 'syntax-type 'pod) ; Cheat
		(cperl-commentify b bb nil)
		(setq end t))
	       ;; "\\\\\\(['`\"($]\\)"
	       ((match-beginning 21)
		;; Trailing backslash; make non-quoting outside string/comment
		(setq bb (match-end 0))
		(goto-char b)
		(skip-chars-backward "\\\\")
		;;;(setq i2 (= (% (skip-chars-backward "\\\\") 2) -1))
		(cperl-modify-syntax-type b cperl-st-punct)
		(goto-char bb))
	       (t (error "Error in regexp of the sniffer")))
	      (if (> (point) stop-point)
		  (progn
		    (if end
			(message "Garbage after __END__/__DATA__ ignored")
		      (message "Unbalanced syntax found while scanning")
		      (or (car err-l) (setcar err-l b)))
		    (goto-char stop-point))))
	    (setq cperl-syntax-state (cons state-point state)
		  ;; Do not mark syntax as done past tmpend???
		  cperl-syntax-done-to (or tmpend (max (point) max)))
	    ;;(message "state-at=%s, done-to=%s" state-point cperl-syntax-done-to)
	    )
	  (if (car err-l) (goto-char (car err-l))
	    (or non-inter
		(message "Scanning for \"hard\" Perl constructions... done"))))
      (and (buffer-modified-p)
	   (not modified)
	   (set-buffer-modified-p nil))
      (set-syntax-table cperl-mode-syntax-table))
    (list (car err-l) overshoot)))

(defun cperl-find-pods-heres-region (min max)
  (interactive "r")
  (cperl-find-pods-heres min max))

(defun cperl-backward-to-noncomment (lim)
  ;; Stops at lim or after non-whitespace that is not in comment
  ;; XXXX Wrongly understands end-of-multiline strings with # as comment
  (let (stop p pr)
    (while (and (not stop) (> (point) (or lim (point-min))))
      (skip-chars-backward " \t\n\f" lim)
      (setq p (point))
      (beginning-of-line)
      (if (memq (setq pr (get-text-property (point) 'syntax-type))
		'(pod here-doc here-doc-delim))
	  (progn
	    (cperl-unwind-to-safe nil)
	    (setq pr (get-text-property (point) 'syntax-type))))
      (or (and (looking-at "^[ \t]*\\(#\\|$\\)")
	       (not (memq pr '(string prestring))))
	  (progn (cperl-to-comment-or-eol) (bolp))
	  (progn
	    (skip-chars-backward " \t")
	    (if (< p (point)) (goto-char p))
	    (setq stop t))))))

;; Used only in `cperl-calculate-indent'...
(defun cperl-block-p ()		   ; Do not C-M-q !  One string contains ";" !
  ;; Positions is before ?\{.  Checks whether it starts a block.
  ;; No save-excursion!  This is more a distinguisher of a block/hash ref...
  (cperl-backward-to-noncomment (point-min))
  (or (memq (preceding-char) (append ";){}$@&%\C-@" nil)) ; Or label!  \C-@ at bobp
					; Label may be mixed up with `$blah :'
      (save-excursion (cperl-after-label))
      (get-text-property (cperl-1- (point)) 'attrib-group)
      (and (memq (char-syntax (preceding-char)) '(?w ?_))
	   (progn
	     (backward-sexp)
	     ;; sub {BLK}, print {BLK} $data, but NOT `bless', `return', `tr'
	     (or (and (looking-at "[a-zA-Z0-9_:]+[ \t\n\f]*[{#]") ; Method call syntax
		      (not (looking-at "\\(bless\\|return\\|q[wqrx]?\\|tr\\|[smy]\\)\\>")))
		 ;; sub bless::foo {}
		 (progn
		   (cperl-backward-to-noncomment (point-min))
		   (and (eq (preceding-char) ?b)
			(progn
			  (forward-sexp -1)
			  (looking-at "sub[ \t\n\f#]")))))))))

;;; What is the difference of (cperl-after-block-p lim t) and (cperl-block-p)?
;;; No save-excursion; condition-case ...  In (cperl-block-p) the block
;;; may be a part of an in-statement construct, such as
;;;   ${something()}, print {FH} $data.
;;; Moreover, one takes positive approach (looks for else,grep etc)
;;; another negative (looks for bless,tr etc)
(defun cperl-after-block-p (lim &optional pre-block)
  "Return true if the preceeding } (if PRE-BLOCK, following {) delimits a block.
Would not look before LIM.  Assumes that LIM is a good place to begin a
statement.  The kind of block we treat here is one after which a new
statement would start; thus the block in ${func()} does not count."
  (save-excursion
    (condition-case nil
	(progn
	  (or pre-block (forward-sexp -1))
	  (cperl-backward-to-noncomment lim)
	  (or (eq (point) lim)
	      ;; if () {}   // sub f () {}   // sub f :a(') {}
	      (eq (preceding-char) ?\) )
	      ;; label: {}
	      (save-excursion (cperl-after-label))
	      ;; sub :attr {}
	      (get-text-property (cperl-1- (point)) 'attrib-group)
	      (if (memq (char-syntax (preceding-char)) '(?w ?_)) ; else {}
		  (save-excursion
		    (forward-sexp -1)
		    ;; else {}     but not    else::func {}
		    (or (and (looking-at "\\(else\\|continue\\|grep\\|map\\|BEGIN\\|END\\|CHECK\\|INIT\\)\\>")
			     (not (looking-at "\\(\\sw\\|_\\)+::")))
			;; sub f {}
			(progn
			  (cperl-backward-to-noncomment lim)
			  (and (eq (preceding-char) ?b)
			       (progn
				 (forward-sexp -1)
				 (looking-at "sub[ \t\n\f#]"))))))
		;; What preceeds is not word...  XXXX Last statement in sub???
		(cperl-after-expr-p lim))))
      (error nil))))

(defun cperl-after-expr-p (&optional lim chars test)
  "Return true if the position is good for start of expression.
TEST is the expression to evaluate at the found position.  If absent,
CHARS is a string that contains good characters to have before us (however,
`}' is treated \"smartly\" if it is not in the list)."
  (let ((lim (or lim (point-min)))
	stop p pr)
    (cperl-update-syntaxification (point) (point))
    (save-excursion
      (while (and (not stop) (> (point) lim))
	(skip-chars-backward " \t\n\f" lim)
	(setq p (point))
	(beginning-of-line)
	;;(memq (setq pr (get-text-property (point) 'syntax-type))
	;;      '(pod here-doc here-doc-delim))
	(if (get-text-property (point) 'here-doc-group)
	    (progn
	      (goto-char
	       (cperl-beginning-of-property (point) 'here-doc-group))
	      (beginning-of-line 0)))
	(if (get-text-property (point) 'in-pod)
	    (progn
	      (goto-char
	       (cperl-beginning-of-property (point) 'in-pod))
	      (beginning-of-line 0)))
	(if (looking-at "^[ \t]*\\(#\\|$\\)") nil ; Only comment, skip
	  ;; Else: last iteration, or a label
	  (cperl-to-comment-or-eol)	; Will not move past "." after a format
	  (skip-chars-backward " \t")
	  (if (< p (point)) (goto-char p))
	  (setq p (point))
	  (if (and (eq (preceding-char) ?:)
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n\f" lim)
		     (memq (char-syntax (preceding-char)) '(?w ?_))))
	      (forward-sexp -1)		; Possibly label.  Skip it
	    (goto-char p)
	    (setq stop t))))
      (or (bobp)			; ???? Needed
	  (eq (point) lim)
	  (looking-at "[ \t]*__\\(END\\|DATA\\)__") ; After this anything goes
	  (progn
	    (if test (eval test)
	      (or (memq (preceding-char) (append (or chars "{;") nil))
		  (and (eq (preceding-char) ?\})
		       (cperl-after-block-p lim))
		  (and (eq (following-char) ?.)	; in format: see comment above
		       (eq (get-text-property (point) 'syntax-type)
			   'format)))))))))

(defun cperl-backward-to-start-of-expr (&optional lim)
  (condition-case nil
      (progn
	(while (and (or (not lim)
			(> (point) lim))
		    (not (cperl-after-expr-p lim)))
	  (forward-sexp -1)
	  ;; May be after $, @, $# etc of a variable
	  (skip-chars-backward "$@%#")))
    (error nil)))

(defun cperl-at-end-of-expr (&optional lim)
  ;; Since the SEXP approach below is very fragile, do some overengineering
  (or (looking-at (concat cperl-maybe-white-and-comment-rex "[;}]"))
      (condition-case nil
	  (save-excursion
	    ;; If nothing interesting after, does as (forward-sexp -1);
	    ;; otherwise fails, or ends at a start of following sexp.
	    ;; XXXX PROBLEMS: if what follows (after ";") @FOO, or ${bar}
	    ;; may be stuck after @ or $; just put some stupid workaround now:
	    (let ((p (point)))
	      (forward-sexp 1)
	      (forward-sexp -1)
	      (while (memq (preceding-char) (append "%&@$*" nil))
		(forward-char -1))
	      (or (< (point) p)
		  (cperl-after-expr-p lim))))
	(error t))))

(defun cperl-forward-to-end-of-expr (&optional lim)
  (let ((p (point))))
  (condition-case nil
      (progn
	(while (and (< (point) (or lim (point-max)))
		    (not (cperl-at-end-of-expr)))
	  (forward-sexp 1)))
    (error nil)))

(defun cperl-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) (append ")]}\"'`" nil))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun cperl-after-block-and-statement-beg (lim)
  ;; We assume that we are after ?\}
  (and
   (cperl-after-block-p lim)
   (save-excursion
     (forward-sexp -1)
     (cperl-backward-to-noncomment (point-min))
     (or (bobp)
	 (eq (point) lim)
	 (not (= (char-syntax (preceding-char)) ?w))
	 (progn
	   (forward-sexp -1)
	   (not
	    (looking-at
	     "\\(map\\|grep\\|printf?\\|system\\|exec\\|tr\\|s\\)\\>")))))))


(defvar innerloop-done nil)
(defvar last-depth nil)

(defun cperl-indent-exp ()
  "Simple variant of indentation of continued-sexp.

Will not indent comment if it starts at `comment-indent' or looks like
continuation of the comment on the previous line.

If `cperl-indent-region-fix-constructs', will improve spacing on
conditional/loop constructs."
  (interactive)
  (save-excursion
    (let ((tmp-end (progn (end-of-line) (point))) top done)
      (save-excursion
	(beginning-of-line)
	(while (null done)
	  (setq top (point))
	  ;; Plan A: if line has an unfinished paren-group, go to end-of-group
	  (while (= -1 (nth 0 (parse-partial-sexp (point) tmp-end -1)))
	    (setq top (point)))		; Get the outermost parenths in line
	  (goto-char top)
	  (while (< (point) tmp-end)
	    (parse-partial-sexp (point) tmp-end nil t) ; To start-sexp or eol
	    (or (eolp) (forward-sexp 1)))
	  (if (> (point) tmp-end)	; Yes, there an unfinished block
	      nil
	    (if (eq ?\) (preceding-char))
		(progn ;; Plan B: find by REGEXP block followup this line
		  (setq top (point))
		  (condition-case nil
		      (progn
			(forward-sexp -2)
			(if (eq (following-char) ?$ ) ; for my $var (list)
			    (progn
			      (forward-sexp -1)
			      (if (looking-at "\\(my\\|local\\|our\\)\\>")
				  (forward-sexp -1))))
			(if (looking-at
			     (concat "\\(\\elsif\\|if\\|unless\\|while\\|until"
				     "\\|for\\(each\\)?\\>\\(\\("
				     cperl-maybe-white-and-comment-rex
				     "\\(my\\|local\\|our\\)\\)?"
				     cperl-maybe-white-and-comment-rex
				     "\\$[_a-zA-Z0-9]+\\)?\\)\\>"))
			    (progn
			      (goto-char top)
			      (forward-sexp 1)
			      (setq top (point)))))
		    (error (setq done t)))
		  (goto-char top))
	      (if (looking-at		; Try Plan C: continuation block
		   (concat cperl-maybe-white-and-comment-rex
			   "\\<\\(else\\|elsif\|continue\\)\\>"))
		  (progn
		    (goto-char (match-end 0))
		    (save-excursion
		      (end-of-line)
		      (setq tmp-end (point))))
		(setq done t))))
	  (save-excursion
	    (end-of-line)
	    (setq tmp-end (point))))
	(goto-char tmp-end)
	(setq tmp-end (point-marker)))
      (if cperl-indent-region-fix-constructs
	  (cperl-fix-line-spacing tmp-end))
      (cperl-indent-region (point) tmp-end))))

(defun cperl-fix-line-spacing (&optional end parse-data)
  "Improve whitespace in a conditional/loop construct.
Returns some position at the last line."
  (interactive)
  (or end
      (setq end (point-max)))
  (let ((ee (save-excursion (end-of-line) (point)))
	(cperl-indent-region-fix-constructs
	 (or cperl-indent-region-fix-constructs 1))
	p pp ml have-brace ret)
    (save-excursion
      (beginning-of-line)
      (setq ret (point))
      ;;  }? continue
      ;;  blah; }
      (if (not
	   (or (looking-at "[ \t]*\\(els\\(e\\|if\\)\\|continue\\|if\\|while\\|for\\(each\\)?\\|until\\)")
	       (setq have-brace (save-excursion (search-forward "}" ee t)))))
	  nil				; Do not need to do anything
	;; Looking at:
	;; }
	;; else
	(if cperl-merge-trailing-else
	    (if (looking-at
		 "[ \t]*}[ \t]*\n[ \t\n]*\\(els\\(e\\|if\\)\\|continue\\)\\>")
		(progn
		  (search-forward "}")
		  (setq p (point))
		  (skip-chars-forward " \t\n")
		  (delete-region p (point))
		  (insert (make-string cperl-indent-region-fix-constructs ?\ ))
		  (beginning-of-line)))
	  (if (looking-at "[ \t]*}[ \t]*\\(els\\(e\\|if\\)\\|continue\\)\\>")
	      (save-excursion
		  (search-forward "}")
		  (delete-horizontal-space)
		  (insert "\n")
		  (setq ret (point))
		  (if (cperl-indent-line parse-data)
		      (progn
			(cperl-fix-line-spacing end parse-data)
			(setq ret (point)))))))
	;; Looking at:
	;; }     else
	(if (looking-at "[ \t]*}\\(\t*\\|[ \t][ \t]+\\)\\<\\(els\\(e\\|if\\)\\|continue\\)\\>")
	    (progn
	      (search-forward "}")
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	      (beginning-of-line)))
	;; Looking at:
	;; else   {
	(if (looking-at
	     "[ \t]*}?[ \t]*\\<\\(\\els\\(e\\|if\\)\\|continue\\|unless\\|if\\|while\\|for\\(each\\)?\\|until\\)\\>\\(\t*\\|[ \t][ \t]+\\)[^ \t\n#]")
	    (progn
	      (forward-word 1)
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	      (beginning-of-line)))
	;; Looking at:
	;; foreach my    $var
	(if (looking-at
	     "[ \t]*\\<for\\(each\\)?[ \t]+\\(my\\|local\\|our\\)\\(\t*\\|[ \t][ \t]+\\)[^ \t\n]")
	    (progn
	      (forward-word 2)
	      (delete-horizontal-space)
	      (insert (make-string cperl-indent-region-fix-constructs ?\ ))
	      (beginning-of-line)))
	;; Looking at:
	;; foreach my $var     (
	(if (looking-at
	     "[ \t]*\\<for\\(each\\)?[ \t]+\\(my\\|local\\|our\\)[ \t]*\\$[_a-zA-Z0-9]+\\(\t*\\|[ \t][ \t]+\\)[^ \t\n#]")
	    (progn
	      (forward-sexp 3)
	      (delete-horizontal-space)
	      (insert
	       (make-string cperl-indent-region-fix-constructs ?\ ))
	      (beginning-of-line)))
	;; Looking at (with or without "}" at start, ending after "({"):
	;; } foreach my $var ()         OR   {
	(if (looking-at
	     "[ \t]*\\(}[ \t]*\\)?\\<\\(\\els\\(e\\|if\\)\\|continue\\|if\\|unless\\|while\\|for\\(each\\)?\\(\\([ \t]+\\(my\\|local\\|our\\)\\)?[ \t]*\\$[_a-zA-Z0-9]+\\)?\\|until\\)\\>\\([ \t]*(\\|[ \t\n]*{\\)\\|[ \t]*{")
	    (progn
	      (setq ml (match-beginning 8)) ; "(" or "{" after control word
	      (re-search-forward "[({]")
	      (forward-char -1)
	      (setq p (point))
	      (if (eq (following-char) ?\( )
		  (progn
		    (forward-sexp 1)
		    (setq pp (point)))	; past parenth-group
		;; after `else' or nothing
		(if ml			; after `else'
		    (skip-chars-backward " \t\n")
		  (beginning-of-line))
		(setq pp nil))
	      ;; Now after the sexp before the brace
	      ;; Multiline expr should be special
	      (setq ml (and pp (save-excursion (goto-char p)
					       (search-forward "\n" pp t))))
	      (if (and (or (not pp) (< pp end))	; Do not go too far...
		       (looking-at "[ \t\n]*{"))
		  (progn
		    (cond
		     ((bolp)		; Were before `{', no if/else/etc
		      nil)
		     ((looking-at "\\(\t*\\| [ \t]+\\){") ; Not exactly 1 SPACE
		      (delete-horizontal-space)
		      (if (if ml
			      cperl-extra-newline-before-brace-multiline
			    cperl-extra-newline-before-brace)
			  (progn
			    (delete-horizontal-space)
			    (insert "\n")
			    (setq ret (point))
			    (if (cperl-indent-line parse-data)
				(progn
				  (cperl-fix-line-spacing end parse-data)
				  (setq ret (point)))))
			(insert
			 (make-string cperl-indent-region-fix-constructs ?\ ))))
		     ((and (looking-at "[ \t]*\n")
			   (not (if ml
				    cperl-extra-newline-before-brace-multiline
				  cperl-extra-newline-before-brace)))
		      (setq pp (point))
		      (skip-chars-forward " \t\n")
		      (delete-region pp (point))
		      (insert
		       (make-string cperl-indent-region-fix-constructs ?\ )))
		     ((and (looking-at "[\t ]*{")
			   (if ml cperl-extra-newline-before-brace-multiline
			     cperl-extra-newline-before-brace))
		      (delete-horizontal-space)
		      (insert "\n")
		      (setq ret (point))
		      (if (cperl-indent-line parse-data)
			  (progn
			    (cperl-fix-line-spacing end parse-data)
			    (setq ret (point))))))
		    ;; Now we are before `{'
		    (if (looking-at "[ \t\n]*{[ \t]*[^ \t\n#]")
			(progn
			  (skip-chars-forward " \t\n")
			  (setq pp (point))
			  (forward-sexp 1)
			  (setq p (point))
			  (goto-char pp)
			  (setq ml (search-forward "\n" p t))
			  (if (or cperl-break-one-line-blocks-when-indent ml)
			      ;; not good: multi-line BLOCK
			      (progn
				(goto-char (1+ pp))
				(delete-horizontal-space)
				(insert "\n")
				(setq ret (point))
				(if (cperl-indent-line parse-data)
				    (setq ret (cperl-fix-line-spacing end parse-data)))))))))))
	(beginning-of-line)
	(setq p (point) pp (save-excursion (end-of-line) (point))) ; May be different from ee.
	;; Now check whether there is a hanging `}'
	;; Looking at:
	;; } blah
	(if (and
	     cperl-fix-hanging-brace-when-indent
	     have-brace
	     (not (looking-at "[ \t]*}[ \t]*\\(\\<\\(els\\(if\\|e\\)\\|continue\\|while\\|until\\)\\>\\|$\\|#\\)"))
	     (condition-case nil
		 (progn
		   (up-list 1)
		   (if (and (<= (point) pp)
			    (eq (preceding-char) ?\} )
			    (cperl-after-block-and-statement-beg (point-min)))
		       t
		     (goto-char p)
		     nil))
	       (error nil)))
	    (progn
	      (forward-char -1)
	      (skip-chars-backward " \t")
	      (if (bolp)
		  ;; `}' was the first thing on the line, insert NL *after* it.
		  (progn
		    (cperl-indent-line parse-data)
		    (search-forward "}")
		    (delete-horizontal-space)
		    (insert "\n"))
		(delete-horizontal-space)
		(or (eq (preceding-char) ?\;)
		    (bolp)
		    (and (eq (preceding-char) ?\} )
			 (cperl-after-block-p (point-min)))
		    (insert ";"))
		(insert "\n")
		(setq ret (point)))
	      (if (cperl-indent-line parse-data)
		  (setq ret (cperl-fix-line-spacing end parse-data)))
	      (beginning-of-line)))))
    ret))

(defvar cperl-update-start)		; Do not need to make them local
(defvar cperl-update-end)
(defun cperl-delay-update-hook (beg end old-len)
  (setq cperl-update-start (min beg (or cperl-update-start (point-max))))
  (setq cperl-update-end (max end (or cperl-update-end (point-min)))))

(defun cperl-indent-region (start end)
  "Simple variant of indentation of region in CPerl mode.
Should be slow.  Will not indent comment if it starts at `comment-indent'
or looks like continuation of the comment on the previous line.
Indents all the lines whose first character is between START and END
inclusive.

If `cperl-indent-region-fix-constructs', will improve spacing on
conditional/loop constructs."
  (interactive "r")
  (cperl-update-syntaxification end end)
  (save-excursion
    (let (cperl-update-start cperl-update-end (h-a-c after-change-functions))
      (let ((indent-info (if cperl-emacs-can-parse
			     (list nil nil nil)	; Cannot use '(), since will modify
			   nil))
	    (pm 0) (imenu-scanning-message "Indenting... (%3d%%)")
	    after-change-functions	; Speed it up!
	    st comm old-comm-indent new-comm-indent p pp i empty)
	(if h-a-c (add-hook 'after-change-functions 'cperl-delay-update-hook))
	(goto-char start)
	(setq old-comm-indent (and (cperl-to-comment-or-eol)
				   (current-column))
	      new-comm-indent old-comm-indent)
	(goto-char start)
	(setq end (set-marker (make-marker) end)) ; indentation changes pos
	(or (bolp) (beginning-of-line 2))
	(or (fboundp 'imenu-progress-message)
	    (message "Indenting... For feedback load `imenu'..."))
	(while (and (<= (point) end) (not (eobp))) ; bol to check start
	  (and (fboundp 'imenu-progress-message)
	       (imenu-progress-message
		pm (/ (* 100 (- (point) start)) (- end start -1))))
	  (setq st (point))
	  (if (or
	       (setq empty (looking-at "[ \t]*\n"))
	       (and (setq comm (looking-at "[ \t]*#"))
		    (or (eq (current-indentation) (or old-comm-indent
						      comment-column))
			(setq old-comm-indent nil))))
	      (if (and old-comm-indent
		       (not empty)
		       (= (current-indentation) old-comm-indent)
		       (not (eq (get-text-property (point) 'syntax-type) 'pod))
		       (not (eq (get-text-property (point) 'syntax-table)
				cperl-st-cfence)))
		  (let ((comment-column new-comm-indent))
		    (indent-for-comment)))
	    (progn
	      (setq i (cperl-indent-line indent-info))
	      (or comm
		  (not i)
		  (progn
		    (if cperl-indent-region-fix-constructs
			(goto-char (cperl-fix-line-spacing end indent-info)))
		    (if (setq old-comm-indent
			      (and (cperl-to-comment-or-eol)
				   (not (memq (get-text-property (point)
								 'syntax-type)
					      '(pod here-doc)))
				   (not (eq (get-text-property (point)
							       'syntax-table)
					    cperl-st-cfence))
				   (current-column)))
			(progn (indent-for-comment)
			       (skip-chars-backward " \t")
			       (skip-chars-backward "#")
			       (setq new-comm-indent (current-column))))))))
	  (beginning-of-line 2))
      	(if (fboundp 'imenu-progress-message)
	    (imenu-progress-message pm 100)
	  (message nil)))
      ;; Now run the update hooks
      (and after-change-functions
	   cperl-update-end
	   (save-excursion
	     (goto-char cperl-update-end)
	     (insert " ")
	     (delete-char -1)
	     (goto-char cperl-update-start)
	     (insert " ")
	     (delete-char -1))))))

;; Stolen from lisp-mode with a lot of improvements

(defun cperl-fill-paragraph (&optional justify iteration)
  "Like \\[fill-paragraph], but handle CPerl comments.
If any of the current line is a comment, fill the comment or the
block of it that point is in, preserving the comment's initial
indentation and initial hashes.  Behaves usually outside of comment."
  (interactive "P")
  (let (;; Non-nil if the current line contains a comment.
	has-comment
	fill-paragraph-function		; do not recurse
	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	;; Line that contains code and comment (or nil)
	start
	c spaces len dc (comment-column comment-column))
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((cperl-to-comment-or-eol)
	(setq has-comment t)
	(looking-at "#+[ \t]*")
	(setq start (point) c (current-column)
	      comment-fill-prefix
	      (concat (make-string (current-column) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))
	      spaces (progn (skip-chars-backward " \t")
			    (buffer-substring (point) start))
	      dc (- c (current-column)) len (- start (point))
	      start (point-marker))
	(delete-char len)
	(insert (make-string dc ?-)))))	; Placeholder (to avoid splitting???)
    (if (not has-comment)
	(fill-paragraph justify)       ; Do the usual thing outside of comment
      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
	(narrow-to-region
	 ;; Find the first line we should include in the region to fill.
	 (if start (progn (beginning-of-line) (point))
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")))
	     ;; We may have gone to far.  Go forward again.
	     (or (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")
		 (forward-line 1))
	     (point)))
	 ;; Find the beginning of the first line past the region to fill.
	 (save-excursion
	   (while (progn (forward-line 1)
			 (looking-at "^[ \t]*#+[ \t]*[^ \t\n#]")))
	   (point)))
	;; Remove existing hashes
	(goto-char (point-min))
	(while (progn (forward-line 1) (< (point) (point-max)))
	  (skip-chars-forward " \t")
	  (if (looking-at "#+")
	      (progn
		(if (and (eq (point) (match-beginning 0))
			 (not (eq (point) (match-end 0)))) nil
		    (error
 "Bug in Emacs: `looking-at' in `narrow-to-region': match-data is garbage"))
		(delete-char (- (match-end 0) (match-beginning 0))))))

	;; Lines with only hashes on them can be paragraph boundaries.
	(let ((paragraph-start (concat paragraph-start "\\|^[ \t#]*$"))
	      (paragraph-separate (concat paragraph-start "\\|^[ \t#]*$"))
	      (fill-prefix comment-fill-prefix))
	  (fill-paragraph justify)))
      (if (and start)
	  (progn
	    (goto-char start)
	    (if (> dc 0)
		(progn (delete-char dc) (insert spaces)))
	    (if (or (= (current-column) c) iteration) nil
	      (setq comment-column c)
	      (indent-for-comment)
	      ;; Repeat once more, flagging as iteration
	      (cperl-fill-paragraph justify t))))))
  t)

(defun cperl-do-auto-fill ()
  ;; Break out if the line is short enough
  (if (> (save-excursion
	   (end-of-line)
	   (current-column))
	 fill-column)
      (let ((c (save-excursion (beginning-of-line)
			       (cperl-to-comment-or-eol) (point)))
	    (s (memq (following-char) '(?\ ?\t))) marker)
	(if (>= c (point)) nil
	  (setq marker (point-marker))
	  (cperl-fill-paragraph)
	  (goto-char marker)
	  ;; Is not enough, sometimes marker is a start of line
	  (if (bolp) (progn (re-search-forward "#+[ \t]*")
			    (goto-char (match-end 0))))
	  ;; Following space could have gone:
	  (if (or (not s) (memq (following-char) '(?\ ?\t))) nil
	    (insert " ")
	    (backward-char 1))
	  ;; Previous space could have gone:
	  (or (memq (preceding-char) '(?\ ?\t)) (insert " "))))))

(defun cperl-imenu-addback (lst &optional isback name)
  ;; We suppose that the lst is a DAG, unless the first element only
  ;; loops back, and ISBACK is set.  Thus this function cannot be
  ;; applied twice without ISBACK set.
  (cond ((not cperl-imenu-addback) lst)
	(t
	 (or name
	     (setq name "+++BACK+++"))
	 (mapcar (lambda (elt)
		   (if (and (listp elt) (listp (cdr elt)))
		       (progn
			 ;; In the other order it goes up
			 ;; one level only ;-(
			 (setcdr elt (cons (cons name lst)
					   (cdr elt)))
			 (cperl-imenu-addback (cdr elt) t name))))
		 (if isback (cdr lst) lst))
	 lst)))

(defun cperl-imenu--create-perl-index (&optional regexp)
  (require 'cl)
  (require 'imenu)			; May be called from TAGS creator
  (let ((index-alist '()) (index-pack-alist '()) (index-pod-alist '())
	(index-unsorted-alist '()) (i-s-f (default-value 'imenu-sort-function))
	(index-meth-alist '()) meth
	packages ends-ranges p marker is-proto
	(prev-pos 0) is-pack index index1 name (end-range 0) package)
    (goto-char (point-min))
    (if noninteractive
	(message "Scanning Perl for index")
      (imenu-progress-message prev-pos 0))
    (cperl-update-syntaxification (point-max) (point-max))
    ;; Search for the function
    (progn ;;save-match-data
      (while (re-search-forward
	      (or regexp cperl-imenu--function-name-regexp-perl)
	      nil t)
	(or noninteractive
	    (imenu-progress-message prev-pos))
	;; 2=package-group, 5=package-name 8=sub-name
	(cond
	 ((and				; Skip some noise if building tags
	   (match-beginning 5)		; package name
	   ;;(eq (char-after (match-beginning 2)) ?p) ; package
	   (not (save-match-data
		  (looking-at "[ \t\n]*;")))) ; Plain text word 'package'
	  nil)
	 ((and
	   (or (match-beginning 2)
	       (match-beginning 8))		; package or sub
	   ;; Skip if quoted (will not skip multi-line ''-strings :-():
	   (null (get-text-property (match-beginning 1) 'syntax-table))
	   (null (get-text-property (match-beginning 1) 'syntax-type))
	   (null (get-text-property (match-beginning 1) 'in-pod)))
	  (setq is-pack (match-beginning 2))
	  ;; (if (looking-at "([^()]*)[ \t\n\f]*")
	  ;;    (goto-char (match-end 0)))	; Messes what follows
	  (setq meth nil
		p (point))
	  (while (and ends-ranges (>= p (car ends-ranges)))
	    ;; delete obsolete entries
	    (setq ends-ranges (cdr ends-ranges) packages (cdr packages)))
	  (setq package (or (car packages) "")
		end-range (or (car ends-ranges) 0))
	  (if is-pack			; doing "package"
	      (progn
		(if (match-beginning 5)	; named package
		    (setq name (buffer-substring (match-beginning 5)
						 (match-end 5))
			  name (progn
				 (set-text-properties 0 (length name) nil name)
				 name)
			  package (concat name "::")
			  name (concat "package " name))
		  ;; Support nameless packages
		  (setq name "package;" package ""))
		(setq end-range
		      (save-excursion
			(parse-partial-sexp (point) (point-max) -1) (point))
		      ends-ranges (cons end-range ends-ranges)
		      packages (cons package packages)))
	    (setq is-proto
		  (or (eq (following-char) ?\;)
		      (eq 0 (get-text-property (point) 'attrib-group)))))
	  ;; Skip this function name if it is a prototype declaration.
	  (if (and is-proto (not is-pack)) nil
	    (or is-pack
		(setq name
		      (buffer-substring (match-beginning 8) (match-end 8)))
		(set-text-properties 0 (length name) nil name))
	    (setq marker (make-marker))
	    (set-marker marker (match-end (if is-pack 2 8)))
	    (cond (is-pack nil)
		  ((string-match "[:']" name)
		   (setq meth t))
		  ((> p end-range) nil)
		  (t
		   (setq name (concat package name) meth t)))
	    (setq index (cons name marker))
	    (if is-pack
		(push index index-pack-alist)
	      (push index index-alist))
	    (if meth (push index index-meth-alist))
	    (push index index-unsorted-alist)))
	 ((match-beginning 16)		; POD section
	  (setq name (buffer-substring (match-beginning 17) (match-end 17))
		marker (make-marker))
	  (set-marker marker (match-beginning 17))
	  (set-text-properties 0 (length name) nil name)
	  (setq name (concat (make-string
			      (* 3 (- (char-after (match-beginning 16)) ?1))
			      ?\ )
			     name)
		index (cons name marker))
	  (setq index1 (cons (concat "=" name) (cdr index)))
	  (push index index-pod-alist)
	  (push index1 index-unsorted-alist)))))
    (or noninteractive
	(imenu-progress-message prev-pos 100))
    (setq index-alist
	  (if (default-value 'imenu-sort-function)
	      (sort index-alist (default-value 'imenu-sort-function))
	    (nreverse index-alist)))
    (and index-pod-alist
	 (push (cons "+POD headers+..."
		     (nreverse index-pod-alist))
	       index-alist))
    (and (or index-pack-alist index-meth-alist)
	 (let ((lst index-pack-alist) hier-list pack elt group name)
	   ;; Remove "package ", reverse and uniquify.
	   (while lst
	     (setq elt (car lst) lst (cdr lst) name (substring (car elt) 8))
	     (if (assoc name hier-list) nil
	       (setq hier-list (cons (cons name (cdr elt)) hier-list))))
	   (setq lst index-meth-alist)
	   (while lst
	     (setq elt (car lst) lst (cdr lst))
	     (cond ((string-match "\\(::\\|'\\)[_a-zA-Z0-9]+$" (car elt))
		    (setq pack (substring (car elt) 0 (match-beginning 0)))
		    (if (setq group (assoc pack hier-list))
			(if (listp (cdr group))
			    ;; Have some functions already
			    (setcdr group
				    (cons (cons (substring
						 (car elt)
						 (+ 2 (match-beginning 0)))
						(cdr elt))
					  (cdr group)))
			  (setcdr group (list (cons (substring
						     (car elt)
						     (+ 2 (match-beginning 0)))
						    (cdr elt)))))
		      (setq hier-list
			    (cons (cons pack
					(list (cons (substring
						     (car elt)
						     (+ 2 (match-beginning 0)))
						    (cdr elt))))
				  hier-list))))))
	   (push (cons "+Hierarchy+..."
		       hier-list)
		 index-alist)))
    (and index-pack-alist
	 (push (cons "+Packages+..."
		     (nreverse index-pack-alist))
	       index-alist))
    (and (or index-pack-alist index-pod-alist
	     (default-value 'imenu-sort-function))
	 index-unsorted-alist
	 (push (cons "+Unsorted List+..."
		     (nreverse index-unsorted-alist))
	       index-alist))
    (cperl-imenu-addback index-alist)))


;; Suggested by Mark A. Hershberger
(defun cperl-outline-level ()
  (looking-at outline-regexp)
  (cond ((not (match-beginning 1)) 0)	; beginning-of-file
;;;; 2=package-group, 5=package-name 8=sub-name 16=head-level
	((match-beginning 2) 0)		; package
	((match-beginning 8) 1)		; sub
	((match-beginning 16)
	 (- (char-after (match-beginning 16)) ?0)) ; headN ==> N
	(t 5)))				; should not happen


(defvar cperl-compilation-error-regexp-alist
  ;; This look like a paranoiac regexp: could anybody find a better one? (which WORKS).
  '(("^[^\n]* \\(file\\|at\\) \\([^ \t\n]+\\) [^\n]*line \\([0-9]+\\)[\\., \n]"
     2 3))
  "Alist that specifies how to match errors in perl output.")

(if (fboundp 'eval-after-load)
    (eval-after-load
	"mode-compile"
      '(setq perl-compilation-error-regexp-alist
	     cperl-compilation-error-regexp-alist)))


(defun cperl-windowed-init ()
  "Initialization under windowed version."
  (cond ((featurep 'ps-print)
	 (or cperl-faces-init
	     (progn
	       (and (boundp 'font-lock-multiline)
		    (setq cperl-font-lock-multiline t))
	       (cperl-init-faces))))
	((not cperl-faces-init)
	 (add-hook 'font-lock-mode-hook
		   (function
		    (lambda ()
		      (if (memq major-mode '(perl-mode cperl-mode))
			  (progn
			    (or cperl-faces-init (cperl-init-faces)))))))
	 (if (fboundp 'eval-after-load)
	     (eval-after-load
		 "ps-print"
	       '(or cperl-faces-init (cperl-init-faces)))))))

(defun cperl-load-font-lock-keywords ()
  (or cperl-faces-init (cperl-init-faces))
  perl-font-lock-keywords)

(defun cperl-load-font-lock-keywords-1 ()
  (or cperl-faces-init (cperl-init-faces))
  perl-font-lock-keywords-1)

(defun cperl-load-font-lock-keywords-2 ()
  (or cperl-faces-init (cperl-init-faces))
  perl-font-lock-keywords-2)

(defvar perl-font-lock-keywords-1 nil
  "Additional expressions to highlight in Perl mode.  Minimal set.")
(defvar perl-font-lock-keywords nil
  "Additional expressions to highlight in Perl mode.  Default set.")
(defvar perl-font-lock-keywords-2 nil
  "Additional expressions to highlight in Perl mode.  Maximal set.")

(defvar font-lock-background-mode)
(defvar font-lock-display-type)
(defun cperl-init-faces-weak ()
  ;; Allow `cperl-find-pods-heres' to run.
  (or (boundp 'font-lock-constant-face)
      (cperl-force-face font-lock-constant-face
                        "Face for constant and label names"))
  (or (boundp 'font-lock-warning-face)
      (cperl-force-face font-lock-warning-face
			"Face for things which should stand out"))
  ;;(setq font-lock-constant-face 'font-lock-constant-face)
  )

(defun cperl-init-faces ()
  (condition-case errs
      (progn
	(require 'font-lock)
	(and (fboundp 'font-lock-fontify-anchored-keywords)
	     (featurep 'font-lock-extra)
	     (message "You have an obsolete package `font-lock-extra'.  Install `choose-color'."))
	(let (t-font-lock-keywords t-font-lock-keywords-1 font-lock-anchored)
	  (if (fboundp 'font-lock-fontify-anchored-keywords)
	      (setq font-lock-anchored t))
	  (setq
	   t-font-lock-keywords
	   (list
	    (list "[ \t]+$" 0 cperl-invalid-face t)
	    (cons
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      (mapconcat
	       'identity
	       '("if" "until" "while" "elsif" "else" "unless" "for"
		 "foreach" "continue" "exit" "die" "last" "goto" "next"
		 "redo" "return" "local" "exec" "sub" "do" "dump" "use" "our"
		 "require" "package" "eval" "my" "BEGIN" "END" "CHECK" "INIT")
	       "\\|")			; Flow control
	      "\\)\\>") 2)		; was "\\)[ \n\t;():,\|&]"
					; In what follows we use `type' style
					; for overwritable builtins
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "CORE" "__FILE__" "__LINE__" "abs" "accept" "alarm"
	      ;; "and" "atan2" "bind" "binmode" "bless" "caller"
	      ;; "chdir" "chmod" "chown" "chr" "chroot" "close"
	      ;; "closedir" "cmp" "connect" "continue" "cos" "crypt"
	      ;; "dbmclose" "dbmopen" "die" "dump" "endgrent"
	      ;; "endhostent" "endnetent" "endprotoent" "endpwent"
	      ;; "endservent" "eof" "eq" "exec" "exit" "exp" "fcntl"
	      ;; "fileno" "flock" "fork" "formline" "ge" "getc"
	      ;; "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
	      ;; "gethostbyname" "gethostent" "getlogin"
	      ;; "getnetbyaddr" "getnetbyname" "getnetent"
	      ;; "getpeername" "getpgrp" "getppid" "getpriority"
	      ;; "getprotobyname" "getprotobynumber" "getprotoent"
	      ;; "getpwent" "getpwnam" "getpwuid" "getservbyname"
	      ;; "getservbyport" "getservent" "getsockname"
	      ;; "getsockopt" "glob" "gmtime" "gt" "hex" "index" "int"
	      ;; "ioctl" "join" "kill" "lc" "lcfirst" "le" "length"
	      ;; "link" "listen" "localtime" "lock" "log" "lstat" "lt"
	      ;; "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "ne"
	      ;; "not" "oct" "open" "opendir" "or" "ord" "pack" "pipe"
	      ;; "quotemeta" "rand" "read" "readdir" "readline"
	      ;; "readlink" "readpipe" "recv" "ref" "rename" "require"
	      ;; "reset" "reverse" "rewinddir" "rindex" "rmdir" "seek"
	      ;; "seekdir" "select" "semctl" "semget" "semop" "send"
	      ;; "setgrent" "sethostent" "setnetent" "setpgrp"
	      ;; "setpriority" "setprotoent" "setpwent" "setservent"
	      ;; "setsockopt" "shmctl" "shmget" "shmread" "shmwrite"
	      ;; "shutdown" "sin" "sleep" "socket" "socketpair"
	      ;; "sprintf" "sqrt" "srand" "stat" "substr" "symlink"
	      ;; "syscall" "sysopen" "sysread" "system" "syswrite" "tell"
	      ;; "telldir" "time" "times" "truncate" "uc" "ucfirst"
	      ;; "umask" "unlink" "unpack" "utime" "values" "vec"
	      ;; "wait" "waitpid" "wantarray" "warn" "write" "x" "xor"
	      "a\\(bs\\|ccept\\|tan2\\|larm\\|nd\\)\\|"
	      "b\\(in\\(d\\|mode\\)\\|less\\)\\|"
	      "c\\(h\\(r\\(\\|oot\\)\\|dir\\|mod\\|own\\)\\|aller\\|rypt\\|"
	      "lose\\(\\|dir\\)\\|mp\\|o\\(s\\|n\\(tinue\\|nect\\)\\)\\)\\|"
	      "CORE\\|d\\(ie\\|bm\\(close\\|open\\)\\|ump\\)\\|"
	      "e\\(x\\(p\\|it\\|ec\\)\\|q\\|nd\\(p\\(rotoent\\|went\\)\\|"
	      "hostent\\|servent\\|netent\\|grent\\)\\|of\\)\\|"
	      "f\\(ileno\\|cntl\\|lock\\|or\\(k\\|mline\\)\\)\\|"
	      "g\\(t\\|lob\\|mtime\\|e\\(\\|t\\(p\\(pid\\|r\\(iority\\|"
	      "oto\\(byn\\(ame\\|umber\\)\\|ent\\)\\)\\|eername\\|w"
	      "\\(uid\\|ent\\|nam\\)\\|grp\\)\\|host\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|s\\(erv\\(by\\(port\\|name\\)\\|ent\\)\\|"
	      "ock\\(name\\|opt\\)\\)\\|c\\|login\\|net\\(by\\(addr\\|name\\)\\|"
	      "ent\\)\\|gr\\(ent\\|nam\\|gid\\)\\)\\)\\)\\|"
	      "hex\\|i\\(n\\(t\\|dex\\)\\|octl\\)\\|join\\|kill\\|"
	      "l\\(i\\(sten\\|nk\\)\\|stat\\|c\\(\\|first\\)\\|t\\|e"
	      "\\(\\|ngth\\)\\|o\\(c\\(altime\\|k\\)\\|g\\)\\)\\|m\\(sg\\(rcv\\|snd\\|"
	      "ctl\\|get\\)\\|kdir\\)\\|n\\(e\\|ot\\)\\|o\\(pen\\(\\|dir\\)\\|"
	      "r\\(\\|d\\)\\|ct\\)\\|p\\(ipe\\|ack\\)\\|quotemeta\\|"
	      "r\\(index\\|and\\|mdir\\|e\\(quire\\|ad\\(pipe\\|\\|lin"
	      "\\(k\\|e\\)\\|dir\\)\\|set\\|cv\\|verse\\|f\\|winddir\\|name"
	      "\\)\\)\\|s\\(printf\\|qrt\\|rand\\|tat\\|ubstr\\|e\\(t\\(p\\(r"
	      "\\(iority\\|otoent\\)\\|went\\|grp\\)\\|hostent\\|s\\(ervent\\|"
	      "ockopt\\)\\|netent\\|grent\\)\\|ek\\(\\|dir\\)\\|lect\\|"
	      "m\\(ctl\\|op\\|get\\)\\|nd\\)\\|h\\(utdown\\|m\\(read\\|ctl\\|"
	      "write\\|get\\)\\)\\|y\\(s\\(read\\|call\\|open\\|tem\\|write\\)\\|"
	      "mlink\\)\\|in\\|leep\\|ocket\\(pair\\|\\)\\)\\|t\\(runcate\\|"
	      "ell\\(\\|dir\\)\\|ime\\(\\|s\\)\\)\\|u\\(c\\(\\|first\\)\\|"
	      "time\\|mask\\|n\\(pack\\|link\\)\\)\\|v\\(alues\\|ec\\)\\|"
	      "w\\(a\\(rn\\|it\\(pid\\|\\)\\|ntarray\\)\\|rite\\)\\|"
	      "x\\(\\|or\\)\\|__\\(FILE__\\|LINE__\\|PACKAGE__\\)"
	      "\\)\\>") 2 'font-lock-type-face)
	    ;; In what follows we use `other' style
	    ;; for nonoverwritable builtins
	    ;; Somehow 's', 'm' are not auto-generated???
	    (list
	     (concat
	      "\\(^\\|[^$@%&\\]\\)\\<\\("
	      ;; "AUTOLOAD" "BEGIN" "CHECK" "DESTROY" "END" "INIT" "__END__" "chomp"
	      ;; "chop" "defined" "delete" "do" "each" "else" "elsif"
	      ;; "eval" "exists" "for" "foreach" "format" "goto"
	      ;; "grep" "if" "keys" "last" "local" "map" "my" "next"
	      ;; "no" "our" "package" "pop" "pos" "print" "printf" "push"
	      ;; "q" "qq" "qw" "qx" "redo" "return" "scalar" "shift"
	      ;; "sort" "splice" "split" "study" "sub" "tie" "tr"
	      ;; "undef" "unless" "unshift" "untie" "until" "use"
	      ;; "while" "y"
	      "AUTOLOAD\\|BEGIN\\|CHECK\\|cho\\(p\\|mp\\)\\|d\\(e\\(fined\\|lete\\)\\|"
	      "o\\)\\|DESTROY\\|e\\(ach\\|val\\|xists\\|ls\\(e\\|if\\)\\)\\|"
	      "END\\|for\\(\\|each\\|mat\\)\\|g\\(rep\\|oto\\)\\|INIT\\|if\\|keys\\|"
	      "l\\(ast\\|ocal\\)\\|m\\(ap\\|y\\)\\|n\\(ext\\|o\\)\\|our\\|"
	      "p\\(ackage\\|rint\\(\\|f\\)\\|ush\\|o\\(p\\|s\\)\\)\\|"
	      "q\\(\\|q\\|w\\|x\\|r\\)\\|re\\(turn\\|do\\)\\|s\\(pli\\(ce\\|t\\)\\|"
	      "calar\\|tudy\\|ub\\|hift\\|ort\\)\\|t\\(r\\|ie\\)\\|"
	      "u\\(se\\|n\\(shift\\|ti\\(l\\|e\\)\\|def\\|less\\)\\)\\|"
	      "while\\|y\\|__\\(END\\|DATA\\)__" ;__DATA__ added manually
	      "\\|[sm]"			; Added manually
	      "\\)\\>") 2 'cperl-nonoverridable-face)
	    ;;		(mapconcat 'identity
	    ;;			   '("#endif" "#else" "#ifdef" "#ifndef" "#if"
	    ;;			     "#include" "#define" "#undef")
	    ;;			   "\\|")
	    '("-[rwxoRWXOezsfdlpSbctugkTBMAC]\\>\\([ \t]+_\\>\\)?" 0
	      font-lock-function-name-face keep) ; Not very good, triggers at "[a-z]"
	    ;; This highlights declarations and definitions differenty.
	    ;; We do not try to highlight in the case of attributes:
	    ;; it is already done by `cperl-find-pods-heres'
	    (list (concat "\\<sub"
			  cperl-white-and-comment-rex ; whitespace/comments
			  "\\([^ \n\t{;()]+\\)" ; 2=name (assume non-anonymous)
			  "\\("
			    cperl-maybe-white-and-comment-rex ;whitespace/comments?
			    "([^()]*)\\)?" ; prototype
			  cperl-maybe-white-and-comment-rex ; whitespace/comments?
			  "[{;]")
		  2 (if cperl-font-lock-multiline
			'(if (eq (char-after (cperl-1- (match-end 0))) ?\{ )
			     'font-lock-function-name-face
			   'font-lock-variable-name-face)
		      ;; need to manually set 'multiline' for older font-locks
		      '(progn
			 (if (< 1 (count-lines (match-beginning 0)
					       (match-end 0)))
			     (put-text-property
			      (+ 3 (match-beginning 0)) (match-end 0)
			      'syntax-type 'multiline))
			 (if (eq (char-after (cperl-1- (match-end 0))) ?\{ )
			     'font-lock-function-name-face
			   'font-lock-variable-name-face))))
	    '("\\<\\(package\\|require\\|use\\|import\\|no\\|bootstrap\\)[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t;]" ; require A if B;
	      2 font-lock-function-name-face)
	    '("^[ \t]*format[ \t]+\\([a-zA-z_][a-zA-z_0-9:]*\\)[ \t]*=[ \t]*$"
	      1 font-lock-function-name-face)
	    (cond ((featurep 'font-lock-extra)
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     (0 '(restart 2 t)))) ; To highlight $a{bc}{ef}
		  (font-lock-anchored
		   '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		     (2 font-lock-string-face t)
		     ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		      nil nil
		      (1 font-lock-string-face t))))
		  (t '("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
		       2 font-lock-string-face t)))
	    '("[\[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
	      font-lock-string-face t)
	    '("^[ \t]*\\([a-zA-Z0-9_]+[ \t]*:\\)[ \t]*\\($\\|{\\|\\<\\(until\\|while\\|for\\(each\\)?\\|do\\)\\>\\)" 1
	      font-lock-constant-face)	; labels
	    '("\\<\\(continue\\|next\\|last\\|redo\\|goto\\)\\>[ \t]+\\([a-zA-Z0-9_:]+\\)" ; labels as targets
	      2 font-lock-constant-face)
	    ;; Uncomment to get perl-mode-like vars
            ;;; '("[$*]{?\\(\\sw+\\)" 1 font-lock-variable-name-face)
            ;;; '("\\([@%]\\|\\$#\\)\\(\\sw+\\)"
            ;;;  (2 (cons font-lock-variable-name-face '(underline))))
	    (cond ((featurep 'font-lock-extra)
		   '("^[ \t]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
		     (3 font-lock-variable-name-face)
		     (4 '(another 4 nil
				  ("\\=[ \t]*,[ \t]*\\([$@%*][a-zA-Z0-9_:]+\\)\\([ \t]*,\\)?"
				   (1 font-lock-variable-name-face)
				   (2 '(restart 2 nil) nil t)))
			nil t)))	; local variables, multiple
		  (font-lock-anchored
		   ;; 1=my_etc, 2=white? 3=(+white? 4=white? 5=var
		   (` ((, (concat "\\<\\(my\\|local\\|our\\)"
				  cperl-maybe-white-and-comment-rex
				  "\\(("
				     cperl-maybe-white-and-comment-rex
				  "\\)?\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)"))
		       (5 (, (if cperl-font-lock-multiline
				 'font-lock-variable-name-face
			       '(progn  (setq cperl-font-lock-multiline-start
					      (match-beginning 0))
					'font-lock-variable-name-face))))
		       ((, (concat "\\="
				   cperl-maybe-white-and-comment-rex
				   ","
				   cperl-maybe-white-and-comment-rex
				   "\\([$@%*]\\([a-zA-Z0-9_:]+\\|[^a-zA-Z0-9_]\\)\\)"))
			;; Bug in font-lock: limit is used not only to limit 
			;; searches, but to set the "extend window for
			;; facification" property.  Thus we need to minimize.
			(, (if cperl-font-lock-multiline
			     '(if (match-beginning 3)
				  (save-excursion
				    (goto-char (match-beginning 3))
				    (condition-case nil
					(forward-sexp 1)
				      (error
				       (condition-case nil
					   (forward-char 200)
					 (error nil)))) ; typeahead
				    (1- (point))) ; report limit
				(forward-char -2)) ; disable continued expr
			     '(if (match-beginning 3)
				  (point-max) ; No limit for continuation
				(forward-char -2)))) ; disable continued expr
			(, (if cperl-font-lock-multiline
			       nil
			     '(progn	; Do at end
				;; "my" may be already fontified (POD),
				;; so cperl-font-lock-multiline-start is nil
				(if (or (not cperl-font-lock-multiline-start)
					(> 2 (count-lines
					      cperl-font-lock-multiline-start
					      (point))))
				    nil
				  (put-text-property
				   (1+ cperl-font-lock-multiline-start) (point)
				   'syntax-type 'multiline))
				(setq cperl-font-lock-multiline-start nil))))
			(3 font-lock-variable-name-face)))))
		  (t '("^[ \t{}]*\\(my\\|local\\|our\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)"
		       3 font-lock-variable-name-face)))
	    '("\\<for\\(each\\)?\\([ \t]+\\(my\\|local\\|our\\)\\)?[ \t]*\\(\\$[a-zA-Z_][a-zA-Z_0-9]*\\)[ \t]*("
	      4 font-lock-variable-name-face)))
	  (setq
	   t-font-lock-keywords-1
	   (and (fboundp 'turn-on-font-lock) ; Check for newer font-lock
		;; not yet as of XEmacs 19.12, works with 21.1.11
		(or
		 (not cperl-xemacs-p)
		 (string< "21.1.9" emacs-version)
		 (and (string< "21.1.10" emacs-version)
		      (string< emacs-version "21.1.2")))
		'(
		  ("\\(\\([@%]\\|\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
		   (if (eq (char-after (match-beginning 2)) ?%)
		       cperl-hash-face
		     cperl-array-face)
		   t)			; arrays and hashes
		  ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)"
		   1
		   (if (= (- (match-end 2) (match-beginning 2)) 1)
		       (if (eq (char-after (match-beginning 3)) ?{)
			   cperl-hash-face
			 cperl-array-face) ; arrays and hashes
		     font-lock-variable-name-face) ; Just to put something
		   t)
		  ("\\(@\\|\\$#\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
		   (1 cperl-array-face)
		   (2 font-lock-variable-name-face))
		  ("\\(%\\)\\(\\$+\\([a-zA-Z_:][a-zA-Z0-9_:]*\\|[^ \t\n]\\)\\)"
		   (1 cperl-hash-face)
		   (2 font-lock-variable-name-face))
		  ;;("\\([smy]\\|tr\\)\\([^a-z_A-Z0-9]\\)\\(\\([^\n\\]*||\\)\\)\\2")
		       ;;; Too much noise from \s* @s[ and friends
		  ;;("\\(\\<\\([msy]\\|tr\\)[ \t]*\\([^ \t\na-zA-Z0-9_]\\)\\|\\(/\\)\\)"
		  ;;(3 font-lock-function-name-face t t)
		  ;;(4
		  ;; (if (cperl-slash-is-regexp)
		  ;;    font-lock-function-name-face 'default) nil t))
		  )))
	  (if cperl-highlight-variables-indiscriminately
	      (setq t-font-lock-keywords-1
		    (append t-font-lock-keywords-1
			    (list '("\\([$*]{?\\sw+\\)" 1
				    font-lock-variable-name-face)))))
	  (setq perl-font-lock-keywords-1
		(if cperl-syntaxify-by-font-lock
		    (cons 'cperl-fontify-update
			  t-font-lock-keywords)
		  t-font-lock-keywords)
		perl-font-lock-keywords perl-font-lock-keywords-1
		perl-font-lock-keywords-2 (append
					   perl-font-lock-keywords-1
					   t-font-lock-keywords-1)))
	(if (fboundp 'ps-print-buffer) (cperl-ps-print-init))
	(if (or (featurep 'choose-color) (featurep 'font-lock-extra))
	    (eval			; Avoid a warning
	     '(font-lock-require-faces
	       (list
		;; Color-light    Color-dark      Gray-light      Gray-dark Mono
		(list 'font-lock-comment-face
		      ["Firebrick"	"OrangeRed" 	"DimGray"	"Gray80"]
		      nil
		      [nil		nil		t		t	t]
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-string-face
		      ["RosyBrown"	"LightSalmon" 	"Gray50"	"LightGray"]
		      nil
		      nil
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-function-name-face
		      (vector
		       "Blue"		"LightSkyBlue"	"Gray50"	"LightGray"
		       (cdr (assq 'background-color ; if mono
				  (frame-parameters))))
		      (vector
		       nil		nil		nil		nil
		       (cdr (assq 'foreground-color ; if mono
				  (frame-parameters))))
		      [nil		nil		t		t	t]
		      nil
		      nil)
		(list 'font-lock-variable-name-face
		      ["DarkGoldenrod"	"LightGoldenrod" "DimGray"	"Gray90"]
		      nil
		      [nil		nil		t		t	t]
		      [nil		nil		t		t	t]
		      nil)
		(list 'font-lock-type-face
		      ["DarkOliveGreen"	"PaleGreen" 	"DimGray"	"Gray80"]
		      nil
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t])
		(list 'font-lock-warning-face
		      ["Pink"		"Red"		"Gray50"	"LightGray"]
		      ["gray20"		"gray90"
							"gray80"	"gray20"]
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t]
		      )
		(list 'font-lock-constant-face
		      ["CadetBlue"	"Aquamarine" 	"Gray50"	"LightGray"]
		      nil
		      [nil		nil		t		t	t]
		      nil
		      [nil		nil		t		t	t])
		(list 'cperl-nonoverridable-face
		      ["chartreuse3"	("orchid1" "orange")
		       nil		"Gray80"]
		      [nil		nil		"gray90"]
		      [nil		nil		nil		t	t]
		      [nil		nil		t		t]
		      [nil		nil		t		t	t])
		(list 'cperl-array-face
		      ["blue"		"yellow" 	nil		"Gray80"]
		      ["lightyellow2"	("navy" "os2blue" "darkgreen")
		       "gray90"]
		      t
		      nil
		      nil)
		(list 'cperl-hash-face
		      ["red"		"red"	 	nil		"Gray80"]
		      ["lightyellow2"	("navy" "os2blue" "darkgreen")
		       "gray90"]
		      t
		      t
		      nil))))
	  ;; Do it the dull way, without choose-color
	  (defvar cperl-guessed-background nil
	    "Display characteristics as guessed by cperl.")
	  ;;	  (or (fboundp 'x-color-defined-p)
	  ;;	      (defalias 'x-color-defined-p
	  ;;		(cond ((fboundp 'color-defined-p) 'color-defined-p)
	  ;;		      ;; XEmacs >= 19.12
	  ;;		      ((fboundp 'valid-color-name-p) 'valid-color-name-p)
	  ;;		      ;; XEmacs 19.11
	  ;;		      (t 'x-valid-color-name-p))))
	  (cperl-force-face font-lock-constant-face
			    "Face for constant and label names")
	  (cperl-force-face font-lock-variable-name-face
			    "Face for variable names")
	  (cperl-force-face font-lock-type-face
			    "Face for data types")
	  (cperl-force-face cperl-nonoverridable-face
			    "Face for data types from another group")
	  (cperl-force-face font-lock-warning-face
			    "Face for things which should stand out")
	  (cperl-force-face font-lock-comment-face
			    "Face for comments")
	  (cperl-force-face font-lock-function-name-face
			    "Face for function names")
	  (cperl-force-face cperl-hash-face
			    "Face for hashes")
	  (cperl-force-face cperl-array-face
			    "Face for arrays")
	  ;;(defvar font-lock-constant-face 'font-lock-constant-face)
	  ;;(defvar font-lock-variable-name-face 'font-lock-variable-name-face)
	  ;;(or (boundp 'font-lock-type-face)
	  ;;    (defconst font-lock-type-face
	  ;;	'font-lock-type-face
	  ;;	"Face to use for data types."))
	  ;;(or (boundp 'cperl-nonoverridable-face)
	  ;;    (defconst cperl-nonoverridable-face
	  ;;	'cperl-nonoverridable-face
	  ;;	"Face to use for data types from another group."))
	  ;;(if (not cperl-xemacs-p) nil
	  ;;  (or (boundp 'font-lock-comment-face)
	  ;;	(defconst font-lock-comment-face
	  ;;	  'font-lock-comment-face
	  ;;	  "Face to use for comments."))
	  ;;  (or (boundp 'font-lock-keyword-face)
	  ;;	(defconst font-lock-keyword-face
	  ;;	  'font-lock-keyword-face
	  ;;	  "Face to use for keywords."))
	  ;;  (or (boundp 'font-lock-function-name-face)
	  ;;	(defconst font-lock-function-name-face
	  ;;	  'font-lock-function-name-face
	  ;;	  "Face to use for function names.")))
	  (if (and
	       (not (cperl-is-face 'cperl-array-face))
	       (cperl-is-face 'font-lock-emphasized-face))
	      (copy-face 'font-lock-emphasized-face 'cperl-array-face))
	  (if (and
	       (not (cperl-is-face 'cperl-hash-face))
	       (cperl-is-face 'font-lock-other-emphasized-face))
	      (copy-face 'font-lock-other-emphasized-face
			 'cperl-hash-face))
	  (if (and
	       (not (cperl-is-face 'cperl-nonoverridable-face))
	       (cperl-is-face 'font-lock-other-type-face))
	      (copy-face 'font-lock-other-type-face
			 'cperl-nonoverridable-face))
	  ;;(or (boundp 'cperl-hash-face)
	  ;;    (defconst cperl-hash-face
	  ;;	'cperl-hash-face
	  ;;	"Face to use for hashes."))
	  ;;(or (boundp 'cperl-array-face)
	  ;;    (defconst cperl-array-face
	  ;;	'cperl-array-face
	  ;;	"Face to use for arrays."))
	  ;; Here we try to guess background
	  (let ((background
		 (if (boundp 'font-lock-background-mode)
		     font-lock-background-mode
		   'light))
		(face-list (and (fboundp 'face-list) (face-list))))
;;;;	    (fset 'cperl-is-face
;;;;		  (cond ((fboundp 'find-face)
;;;;			 (symbol-function 'find-face))
;;;;			(face-list
;;;;			 (function (lambda (face) (member face face-list))))
;;;;			(t
;;;;			 (function (lambda (face) (boundp face))))))
	    (defvar cperl-guessed-background
	      (if (and (boundp 'font-lock-display-type)
		       (eq font-lock-display-type 'grayscale))
		  'gray
		background)
	      "Background as guessed by CPerl mode")
	    (and (not (cperl-is-face 'font-lock-constant-face))
		 (cperl-is-face 'font-lock-reference-face)
		 (copy-face 'font-lock-reference-face 'font-lock-constant-face))
	    (if (cperl-is-face 'font-lock-type-face) nil
	      (copy-face 'default 'font-lock-type-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "seagreen")
					 "seagreen"
				       "sea green")))
	       ((eq background 'dark)
		(set-face-foreground 'font-lock-type-face
				     (if (x-color-defined-p "os2pink")
					 "os2pink"
				       "pink")))
	       (t
		(set-face-background 'font-lock-type-face "gray90"))))
	    (if (cperl-is-face 'cperl-nonoverridable-face)
		nil
	      (copy-face 'font-lock-type-face 'cperl-nonoverridable-face)
	      (cond
	       ((eq background 'light)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "chartreuse3")
					 "chartreuse3"
				       "chartreuse")))
	       ((eq background 'dark)
		(set-face-foreground 'cperl-nonoverridable-face
				     (if (x-color-defined-p "orchid1")
					 "orchid1"
				       "orange")))))
;;;	    (if (cperl-is-face 'font-lock-other-emphasized-face) nil
;;;	      (copy-face 'bold-italic 'font-lock-other-emphasized-face)
;;;	      (cond
;;;	       ((eq background 'light)
;;;		(set-face-background 'font-lock-other-emphasized-face
;;;				     (if (x-color-defined-p "lightyellow2")
;;;					 "lightyellow2"
;;;				       (if (x-color-defined-p "lightyellow")
;;;					   "lightyellow"
;;;					 "light yellow"))))
;;;	       ((eq background 'dark)
;;;		(set-face-background 'font-lock-other-emphasized-face
;;;				     (if (x-color-defined-p "navy")
;;;					 "navy"
;;;				       (if (x-color-defined-p "darkgreen")
;;;					   "darkgreen"
;;;					 "dark green"))))
;;;	       (t (set-face-background 'font-lock-other-emphasized-face "gray90"))))
;;;	    (if (cperl-is-face 'font-lock-emphasized-face) nil
;;;	      (copy-face 'bold 'font-lock-emphasized-face)
;;;	      (cond
;;;	       ((eq background 'light)
;;;		(set-face-background 'font-lock-emphasized-face
;;;				     (if (x-color-defined-p "lightyellow2")
;;;					 "lightyellow2"
;;;				       "lightyellow")))
;;;	       ((eq background 'dark)
;;;		(set-face-background 'font-lock-emphasized-face
;;;				     (if (x-color-defined-p "navy")
;;;					 "navy"
;;;				       (if (x-color-defined-p "darkgreen")
;;;					   "darkgreen"
;;;					 "dark green"))))
;;;	       (t (set-face-background 'font-lock-emphasized-face "gray90"))))
	    (if (cperl-is-face 'font-lock-variable-name-face) nil
	      (copy-face 'italic 'font-lock-variable-name-face))
	    (if (cperl-is-face 'font-lock-constant-face) nil
	      (copy-face 'italic 'font-lock-constant-face))))
	(setq cperl-faces-init t))
    (error (message "cperl-init-faces (ignored): %s" errs))))


(defun cperl-ps-print-init ()
  "Initialization of `ps-print' components for faces used in CPerl."
  (eval-after-load "ps-print"
    '(setq ps-bold-faces
	   ;; 			font-lock-variable-name-face
	   ;;			font-lock-constant-face
	   (append '(cperl-array-face
		     cperl-hash-face)
		   ps-bold-faces)
	   ps-italic-faces
	   ;;			font-lock-constant-face
	   (append '(cperl-nonoverridable-face
		     cperl-hash-face)
		   ps-italic-faces)
	   ps-underlined-faces
	   ;;	     font-lock-type-face
	   (append '(cperl-array-face
		     cperl-hash-face
		     underline
		     cperl-nonoverridable-face)
		   ps-underlined-faces))))

(defvar ps-print-face-extension-alist)

(defun cperl-ps-print (&optional file)
  "Pretty-print in CPerl style.
If optional argument FILE is an empty string, prints to printer, otherwise
to the file FILE.  If FILE is nil, prompts for a file name.

Style of printout regulated by the variable `cperl-ps-print-face-properties'."
  (interactive)
  (or file
      (setq file (read-from-minibuffer
		  "Print to file (if empty - to printer): "
		  (concat (buffer-file-name) ".ps")
		  nil nil 'file-name-history)))
  (or (> (length file) 0)
      (setq file nil))
  (require 'ps-print)			; To get ps-print-face-extension-alist
  (let ((ps-print-color-p t)
	(ps-print-face-extension-alist ps-print-face-extension-alist))
    (cperl-ps-extend-face-list cperl-ps-print-face-properties)
    (ps-print-buffer-with-faces file)))

;;; (defun cperl-ps-print-init ()
;;;   "Initialization of `ps-print' components for faces used in CPerl."
;;;   ;; Guard against old versions
;;;   (defvar ps-underlined-faces nil)
;;;   (defvar ps-bold-faces nil)
;;;   (defvar ps-italic-faces nil)
;;;   (setq ps-bold-faces
;;; 	(append '(font-lock-emphasized-face
;;; 		  cperl-array-face
;;; 		  font-lock-keyword-face
;;; 		  font-lock-variable-name-face
;;; 		  font-lock-constant-face
;;; 		  font-lock-reference-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face)
;;; 		ps-bold-faces))
;;;   (setq ps-italic-faces
;;; 	(append '(cperl-nonoverridable-face
;;; 		  font-lock-constant-face
;;; 		  font-lock-reference-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face)
;;; 		ps-italic-faces))
;;;   (setq ps-underlined-faces
;;; 	(append '(font-lock-emphasized-face
;;; 		  cperl-array-face
;;; 		  font-lock-other-emphasized-face
;;; 		  cperl-hash-face
;;; 		  cperl-nonoverridable-face font-lock-type-face)
;;; 		ps-underlined-faces))
;;;   (cons 'font-lock-type-face ps-underlined-faces))


(if (cperl-enable-font-lock) (cperl-windowed-init))

(defconst cperl-styles-entries
  '(cperl-indent-level cperl-brace-offset cperl-continued-brace-offset
    cperl-label-offset cperl-extra-newline-before-brace
    cperl-extra-newline-before-brace-multiline
    cperl-merge-trailing-else
    cperl-continued-statement-offset))

(defconst cperl-style-examples
"##### Numbers etc are: cperl-indent-level cperl-brace-offset
##### cperl-continued-brace-offset cperl-label-offset
##### cperl-continued-statement-offset
##### cperl-merge-trailing-else cperl-extra-newline-before-brace

########### (Do not forget cperl-extra-newline-before-brace-multiline)

### CPerl	(=GNU - extra-newline-before-brace + merge-trailing-else) 2/0/0/-2/2/t/nil
if (foo) {
  bar
    baz;
 label:
  {
    boon;
  }
} else {
  stop;
}

### PerlStyle	(=CPerl with 4 as indent)		4/0/0/-4/4/t/nil
if (foo) {
    bar
	baz;
 label:
    {
	boon;
    }
} else {
    stop;
}

### GNU							2/0/0/-2/2/nil/t
if (foo)
  {
    bar
      baz;
  label:
    {
      boon;
    }
  }
else
  {
    stop;
  }

### C++		(=PerlStyle with braces aligned with control words) 4/0/-4/-4/4/nil/t
if (foo)
{
    bar
	baz;
 label:
    {
	boon;
    }
}
else
{
    stop;
}

### BSD		(=C++, but will not change preexisting merge-trailing-else
###		 and extra-newline-before-brace )		4/0/-4/-4/4
if (foo)
{
    bar
	baz;
 label:
    {
	boon;
    }
}
else
{
    stop;
}

### K&R		(=C++ with indent 5 - merge-trailing-else, but will not
###		 change preexisting extra-newline-before-brace)	5/0/-5/-5/5/nil
if (foo)
{
     bar
	  baz;
 label:
     {
	  boon;
     }
}
else
{
     stop;
}

### Whitesmith	(=PerlStyle, but will not change preexisting
###		 extra-newline-before-brace and merge-trailing-else) 4/0/0/-4/4
if (foo)
    {
	bar
	    baz;
    label:
	{
	    boon;
	}
    }
else
    {
	stop;
    }
"
"Examples of if/else with different indent styles (with v4.23).")

(defconst cperl-style-alist
  '(("CPerl" ;; =GNU - extra-newline-before-brace + cperl-merge-trailing-else
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-continued-statement-offset .  2)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  t))

    ("PerlStyle"			; CPerl with 4 as indent
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     (cperl-extra-newline-before-brace .  nil)
     (cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  t))

    ("GNU"
     (cperl-indent-level               .  2)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -2)
     (cperl-continued-statement-offset .  2)
     (cperl-extra-newline-before-brace .  t)
     (cperl-extra-newline-before-brace-multiline .  t)
     (cperl-merge-trailing-else	       .  nil))

    ("K&R"
     (cperl-indent-level               .  5)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -5)
     (cperl-label-offset               . -5)
     (cperl-continued-statement-offset .  5)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     (cperl-merge-trailing-else	       .  nil))

    ("BSD"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     ;;(cperl-merge-trailing-else	       .  nil) ; ???
     )

    ("C++"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     . -4)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     (cperl-extra-newline-before-brace .  t)
     (cperl-extra-newline-before-brace-multiline .  t)
     (cperl-merge-trailing-else	       .  nil))

    ("Whitesmith"
     (cperl-indent-level               .  4)
     (cperl-brace-offset               .  0)
     (cperl-continued-brace-offset     .  0)
     (cperl-label-offset               . -4)
     (cperl-continued-statement-offset .  4)
     ;;(cperl-extra-newline-before-brace .  nil) ; ???
     ;;(cperl-extra-newline-before-brace-multiline .  nil)
     ;;(cperl-merge-trailing-else	       .  nil) ; ???
     )
    ("Current"))
  "List of variables to set to get a particular indentation style.
Should be used via `cperl-set-style' or via Perl menu.

See examples in `cperl-style-examples'.")

(defun cperl-set-style (style)
  "Set CPerl mode variables to use one of several different indentation styles.
The arguments are a string representing the desired style.
The list of styles is in `cperl-style-alist', available styles
are CPerl, PerlStyle, GNU, K&R, BSD, C++ and Whitesmith.

The current value of style is memorized (unless there is a memorized
data already), may be restored by `cperl-set-style-back'.

Chosing \"Current\" style will not change style, so this may be used for
side-effect of memorizing only.  Examples in `cperl-style-examples'."
  (interactive
   (let ((list (mapcar (function (lambda (elt) (list (car elt))))
		       cperl-style-alist)))
     (list (completing-read "Enter style: " list nil 'insist))))
  (or cperl-old-style
      (setq cperl-old-style
	    (mapcar (function
		     (lambda (name)
		       (cons name (eval name))))
		    cperl-styles-entries)))
  (let ((style (cdr (assoc style cperl-style-alist))) setting str sym)
    (while style
      (setq setting (car style) style (cdr style))
      (set (car setting) (cdr setting)))))

(defun cperl-set-style-back ()
  "Restore a style memorised by `cperl-set-style'."
  (interactive)
  (or cperl-old-style (error "The style was not changed"))
  (let (setting)
    (while cperl-old-style
      (setq setting (car cperl-old-style)
	    cperl-old-style (cdr cperl-old-style))
      (set (car setting) (cdr setting)))))

(defun cperl-check-syntax ()
  (interactive)
  (require 'mode-compile)
  (let ((perl-dbg-flags (concat cperl-extra-perl-args " -wc")))
    (eval '(mode-compile))))		; Avoid a warning

(defun cperl-info-buffer (type)
  ;; Returns buffer with documentation.  Creates if missing.
  ;; If TYPE, this vars buffer.
  ;; Special care is taken to not stomp over an existing info buffer
  (let* ((bname (if type "*info-perl-var*" "*info-perl*"))
	 (info (get-buffer bname))
	 (oldbuf (get-buffer "*info*")))
    (if info info
      (save-window-excursion
	;; Get Info running
	(require 'info)
	(cond (oldbuf
	       (set-buffer oldbuf)
	       (rename-buffer "*info-perl-tmp*")))
	(save-window-excursion
	  (info))
	(Info-find-node cperl-info-page (if type "perlvar" "perlfunc"))
	(set-buffer "*info*")
	(rename-buffer bname)
	(cond (oldbuf
	       (set-buffer "*info-perl-tmp*")
	       (rename-buffer "*info*")
	       (set-buffer bname)))
	(make-local-variable 'window-min-height)
	(setq window-min-height 2)
	(current-buffer)))))

(defun cperl-word-at-point (&optional p)
  "Return the word at point or at P."
  (save-excursion
    (if p (goto-char p))
    (or (cperl-word-at-point-hard)
	(progn
	  (require 'etags)
	  (funcall (or (and (boundp 'find-tag-default-function)
			    find-tag-default-function)
		       (get major-mode 'find-tag-default-function)
		       ;; XEmacs 19.12 has `find-tag-default-hook'; it is
		       ;; automatically used within `find-tag-default':
		       'find-tag-default))))))

(defun cperl-info-on-command (command)
  "Show documentation for Perl command COMMAND in other window.
If perl-info buffer is shown in some frame, uses this frame.
Customized by setting variables `cperl-shrink-wrap-info-frame',
`cperl-max-help-size'."
  (interactive
   (let* ((default (cperl-word-at-point))
	  (read (read-string
		 (format "Find doc for Perl function (default %s): "
			 default))))
     (list (if (equal read "")
	       default
	     read))))

  (let ((buffer (current-buffer))
	(cmd-desc (concat "^" (regexp-quote command) "[^a-zA-Z_0-9]")) ; "tr///"
	pos isvar height iniheight frheight buf win fr1 fr2 iniwin not-loner
	max-height char-height buf-list)
    (if (string-match "^-[a-zA-Z]$" command)
	(setq cmd-desc "^-X[ \t\n]"))
    (setq isvar (string-match "^[$@%]" command)
	  buf (cperl-info-buffer isvar)
	  iniwin (selected-window)
	  fr1 (window-frame iniwin))
    (set-buffer buf)
    (goto-char (point-min))
    (or isvar
	(progn (re-search-forward "^-X[ \t\n]")
	       (forward-line -1)))
    (if (re-search-forward cmd-desc nil t)
	(progn
	  ;; Go back to beginning of the group (ex, for qq)
	  (if (re-search-backward "^[ \t\n\f]")
	      (forward-line 1))
	  (beginning-of-line)
	  ;; Get some of
	  (setq pos (point)
		buf-list (list buf "*info-perl-var*" "*info-perl*"))
	  (while (and (not win) buf-list)
	    (setq win (get-buffer-window (car buf-list) t))
	    (setq buf-list (cdr buf-list)))
	  (or (not win)
	      (eq (window-buffer win) buf)
	      (set-window-buffer win buf))
	  (and win (setq fr2 (window-frame win)))
	  (if (or (not fr2) (eq fr1 fr2))
	      (pop-to-buffer buf)
	    (special-display-popup-frame buf) ; Make it visible
	    (select-window win))
	  (goto-char pos)		; Needed (?!).
	  ;; Resize
	  (setq iniheight (window-height)
		frheight (frame-height)
		not-loner (< iniheight (1- frheight))) ; Are not alone
	  (cond ((if not-loner cperl-max-help-size
		   cperl-shrink-wrap-info-frame)
		 (setq height
		       (+ 2
			  (count-lines
			   pos
			   (save-excursion
			     (if (re-search-forward
				  "^[ \t][^\n]*\n+\\([^ \t\n\f]\\|\\'\\)" nil t)
				 (match-beginning 0) (point-max)))))
		       max-height
		       (if not-loner
			   (/ (* (- frheight 3) cperl-max-help-size) 100)
			 (setq char-height (frame-char-height))
			 ;; Non-functioning under OS/2:
			 (if (eq char-height 1) (setq char-height 18))
			 ;; Title, menubar, + 2 for slack
			 (- (/ (x-display-pixel-height) char-height) 4)))
		 (if (> height max-height) (setq height max-height))
		 ;;(message "was %s doing %s" iniheight height)
		 (if not-loner
		     (enlarge-window (- height iniheight))
		   (set-frame-height (window-frame win) (1+ height)))))
	  (set-window-start (selected-window) pos))
      (message "No entry for %s found." command))
    ;;(pop-to-buffer buffer)
    (select-window iniwin)))

(defun cperl-info-on-current-command ()
  "Show documentation for Perl command at point in other window."
  (interactive)
  (cperl-info-on-command (cperl-word-at-point)))

(defun cperl-imenu-info-imenu-search ()
  (if (looking-at "^-X[ \t\n]") nil
    (re-search-backward
     "^\n\\([-a-zA-Z_]+\\)[ \t\n]")
    (forward-line 1)))

(defun cperl-imenu-info-imenu-name ()
  (buffer-substring
   (match-beginning 1) (match-end 1)))

(defun cperl-imenu-on-info ()
  "Shows imenu for Perl Info Buffer.
Opens Perl Info buffer if needed."
  (interactive)
  (let* ((buffer (current-buffer))
	 imenu-create-index-function
	 imenu-prev-index-position-function
	 imenu-extract-index-name-function
	 (index-item (save-restriction
		       (save-window-excursion
			 (set-buffer (cperl-info-buffer nil))
			 (setq imenu-create-index-function
			       'imenu-default-create-index-function
			       imenu-prev-index-position-function
			       'cperl-imenu-info-imenu-search
			       imenu-extract-index-name-function
			       'cperl-imenu-info-imenu-name)
			 (imenu-choose-buffer-index)))))
    (and index-item
	 (progn
	   (push-mark)
	   (pop-to-buffer "*info-perl*")
	   (cond
	    ((markerp (cdr index-item))
	     (goto-char (marker-position (cdr index-item))))
	    (t
	     (goto-char (cdr index-item))))
	   (set-window-start (selected-window) (point))
	   (pop-to-buffer buffer)))))

(defun cperl-lineup (beg end &optional step minshift)
  "Lineup construction in a region.
Beginning of region should be at the start of a construction.
All first occurrences of this construction in the lines that are
partially contained in the region are lined up at the same column.

MINSHIFT is the minimal amount of space to insert before the construction.
STEP is the tabwidth to position constructions.
If STEP is nil, `cperl-lineup-step' will be used
\(or `cperl-indent-level', if `cperl-lineup-step' is `nil').
Will not move the position at the start to the left."
  (interactive "r")
  (let (search col tcol seen b)
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end (point-marker))
      (goto-char beg)
      (skip-chars-forward " \t\f")
      (setq beg (point-marker))
      (indent-region beg end nil)
      (goto-char beg)
      (setq col (current-column))
      (if (looking-at "[a-zA-Z0-9_]")
	  (if (looking-at "\\<[a-zA-Z0-9_]+\\>")
	      (setq search
		    (concat "\\<"
			    (regexp-quote
			     (buffer-substring (match-beginning 0)
					       (match-end 0))) "\\>"))
	    (error "Cannot line up in a middle of the word"))
	(if (looking-at "$")
	    (error "Cannot line up end of line"))
	(setq search (regexp-quote (char-to-string (following-char)))))
      (setq step (or step cperl-lineup-step cperl-indent-level))
      (or minshift (setq minshift 1))
      (while (progn
	       (beginning-of-line 2)
	       (and (< (point) end)
		    (re-search-forward search end t)
		    (goto-char (match-beginning 0))))
	(setq tcol (current-column) seen t)
	(if (> tcol col) (setq col tcol)))
      (or seen
	  (error "The construction to line up occurred only once"))
      (goto-char beg)
      (setq col (+ col minshift))
      (if (/= (% col step) 0) (setq step (* step (1+ (/ col step)))))
      (while
	  (progn
	    (cperl-make-indent col)
	    (beginning-of-line 2)
	    (and (< (point) end)
		 (re-search-forward search end t)
		 (goto-char (match-beginning 0)))))))) ; No body

(defun cperl-etags (&optional add all files) ;; NOT USED???
  "Run etags with appropriate options for Perl files.
If optional argument ALL is `recursive', will process Perl files
in subdirectories too."
  (interactive)
  (let ((cmd "etags")
	(args '("-l" "none" "-r"
		;;       1=fullname  2=package?             3=name                       4=proto?             5=attrs? (VERY APPROX!)
		"/\\<sub[ \\t]+\\(\\([a-zA-Z0-9:_]*::\\)?\\([a-zA-Z0-9_]+\\)\\)[ \\t]*\\(([^()]*)[ \t]*\\)?\\([ \t]*:[^#{;]*\\)?\\([{#]\\|$\\)/\\3/"
		"-r"
		"/\\<package[ \\t]+\\(\\([a-zA-Z0-9:_]*::\\)?\\([a-zA-Z0-9_]+\\)\\)[ \\t]*\\([#;]\\|$\\)/\\1/"
		"-r"
		"/\\<\\(package\\)[ \\t]*;/\\1;/"))
	res)
    (if add (setq args (cons "-a" args)))
    (or files (setq files (list buffer-file-name)))
    (cond
     ((eq all 'recursive)
      ;;(error "Not implemented: recursive")
      (setq args (append (list "-e"
			       "sub wanted {push @ARGV, $File::Find::name if /\\.[pP][Llm]$/}
				use File::Find;
				find(\\&wanted, '.');
				exec @ARGV;"
			       cmd) args)
	    cmd "perl"))
     (all
      ;;(error "Not implemented: all")
      (setq args (append (list "-e"
			       "push @ARGV, <*.PL *.pl *.pm>;
				exec @ARGV;"
			       cmd) args)
	    cmd "perl"))
     (t
      (setq args (append args files))))
    (setq res (apply 'call-process cmd nil nil nil args))
    (or (eq res 0)
	(message "etags returned \"%s\"" res))))

(defun cperl-toggle-auto-newline ()
  "Toggle the state of `cperl-auto-newline'."
  (interactive)
  (setq cperl-auto-newline (not cperl-auto-newline))
  (message "Newlines will %sbe auto-inserted now."
	   (if cperl-auto-newline "" "not ")))

(defun cperl-toggle-abbrev ()
  "Toggle the state of automatic keyword expansion in CPerl mode."
  (interactive)
  (abbrev-mode (if abbrev-mode 0 1))
  (message "Perl control structure will %sbe auto-inserted now."
	   (if abbrev-mode "" "not ")))


(defun cperl-toggle-electric ()
  "Toggle the state of parentheses doubling in CPerl mode."
  (interactive)
  (setq cperl-electric-parens (if (cperl-val 'cperl-electric-parens) 'null t))
  (message "Parentheses will %sbe auto-doubled now."
	   (if (cperl-val 'cperl-electric-parens) "" "not ")))

(defun cperl-toggle-autohelp ()
  "Toggle the state of Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
  (interactive)
  (if (fboundp 'run-with-idle-timer)
      (progn
	(if cperl-lazy-installed
	    (cperl-lazy-unstall)
	  (cperl-lazy-install))
	(message "Perl help messages will %sbe automatically shown now."
		 (if cperl-lazy-installed "" "not ")))
    (message "Cannot automatically show Perl help messages - run-with-idle-timer missing.")))

(defun cperl-toggle-construct-fix ()
  "Toggle whether `indent-region'/`indent-sexp' fix whitespace too."
  (interactive)
  (setq cperl-indent-region-fix-constructs
	(if cperl-indent-region-fix-constructs
	    nil
	  1))
  (message "indent-region/indent-sexp will %sbe automatically fix whitespace."
	   (if cperl-indent-region-fix-constructs "" "not ")))

(defun cperl-toggle-set-debug-unwind (arg &optional backtrace)
  "Toggle (or, with numeric argument, set) debugging state of syntaxification.
Nonpositive numeric argument disables debugging messages.  The message
summarizes which regions it was decided to rescan for syntactic constructs.

The message looks like this:

  Syxify req=123..138 actual=101..146 done-to: 112=>146 statepos: 73=>117

Numbers are character positions in the buffer.  REQ provides the range to
rescan requested by `font-lock'.  ACTUAL is the range actually resyntaxified;
for correct operation it should start and end outside any special syntactic
construct.  DONE-TO and STATEPOS indicate changes to internal caches maintained
by CPerl."
  (interactive "P")
  (or arg
      (setq arg (if (eq cperl-syntaxify-by-font-lock 
			(if backtrace 'backtrace 'message)) 0 1)))
  (setq arg (if (> arg 0) (if backtrace 'backtrace 'message) t))
  (setq cperl-syntaxify-by-font-lock arg)
  (message "Debugging messages of syntax unwind %sabled."
	   (if (eq arg t) "dis" "en")))

;;;; Tags file creation.

(defvar cperl-tmp-buffer " *cperl-tmp*")

(defun cperl-setup-tmp-buf ()
  (set-buffer (get-buffer-create cperl-tmp-buffer))
  (set-syntax-table cperl-mode-syntax-table)
  (buffer-disable-undo)
  (auto-fill-mode 0)
  (if cperl-use-syntax-table-text-property-for-tags
      (progn
	(make-local-variable 'parse-sexp-lookup-properties)
	;; Do not introduce variable if not needed, we check it!
	(set 'parse-sexp-lookup-properties t))))

(defun cperl-xsub-scan ()
  (require 'cl)
  (require 'imenu)
  (let ((index-alist '())
	(prev-pos 0) index index1 name package prefix)
    (goto-char (point-min))
    (if noninteractive
	(message "Scanning XSUB for index")
      (imenu-progress-message prev-pos 0))
    ;; Search for the function
    (progn ;;save-match-data
      (while (re-search-forward
	      "^\\([ \t]*MODULE\\>[^\n]*\\<PACKAGE[ \t]*=[ \t]*\\([a-zA-Z_][a-zA-Z_0-9:]*\\)\\>\\|\\([a-zA-Z_][a-zA-Z_0-9]*\\)(\\|[ \t]*BOOT:\\)"
	      nil t)
	(or noninteractive
	    (imenu-progress-message prev-pos))
	(cond
	 ((match-beginning 2)		; SECTION
	  (setq package (buffer-substring (match-beginning 2) (match-end 2)))
	  (goto-char (match-beginning 0))
	  (skip-chars-forward " \t")
	  (forward-char 1)
	  (if (looking-at "[^\n]*\\<PREFIX[ \t]*=[ \t]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\>")
	      (setq prefix (buffer-substring (match-beginning 1) (match-end 1)))
	    (setq prefix nil)))
	 ((not package) nil)		; C language section
	 ((match-beginning 3)		; XSUB
	  (goto-char (1+ (match-beginning 3)))
	  (setq index (imenu-example--name-and-position))
	  (setq name (buffer-substring (match-beginning 3) (match-end 3)))
	  (if (and prefix (string-match (concat "^" prefix) name))
	      (setq name (substring name (length prefix))))
	  (cond ((string-match "::" name) nil)
		(t
		 (setq index1 (cons (concat package "::" name) (cdr index)))
		 (push index1 index-alist)))
	  (setcar index name)
	  (push index index-alist))
	 (t				; BOOT: section
	  ;; (beginning-of-line)
	  (setq index (imenu-example--name-and-position))
	  (setcar index (concat package "::BOOT:"))
	  (push index index-alist)))))
    (or noninteractive
	(imenu-progress-message prev-pos 100))
    index-alist))

(defvar cperl-unreadable-ok nil)

(defun cperl-find-tags (ifile xs topdir)
  (let ((b (get-buffer cperl-tmp-buffer)) ind lst elt pos ret rel
	(cperl-pod-here-fontify nil) f file)
    (save-excursion
      (if b (set-buffer b)
	(cperl-setup-tmp-buf))
      (erase-buffer)
      (condition-case err
	  (setq file (car (insert-file-contents ifile)))
	(error (if cperl-unreadable-ok nil
		 (if (y-or-n-p
		      (format "File %s unreadable.  Continue? " ifile))
		     (setq cperl-unreadable-ok t)
		   (error "Aborting: unreadable file %s" ifile)))))
      (if (not file)
	  (message "Unreadable file %s" ifile)
	(message "Scanning file %s ..." file)
	(if (and cperl-use-syntax-table-text-property-for-tags
		 (not xs))
	    (condition-case err		; after __END__ may have garbage
		(cperl-find-pods-heres nil nil noninteractive)
	      (error (message "While scanning for syntax: %s" err))))
	(if xs
	    (setq lst (cperl-xsub-scan))
	  (setq ind (cperl-imenu--create-perl-index))
	  (setq lst (cdr (assoc "+Unsorted List+..." ind))))
	(setq lst
	      (mapcar
	       (function
		(lambda (elt)
		  (cond ((string-match "^[_a-zA-Z]" (car elt))
			 (goto-char (cdr elt))
			 (beginning-of-line) ; pos should be of the start of the line
			 (list (car elt)
			       (point)
			       (1+ (count-lines 1 (point))) ; 1+ since at beg-o-l
			       (buffer-substring (progn
						   (goto-char (cdr elt))
						   ;; After name now...
						   (or (eolp) (forward-char 1))
						   (point))
						 (progn
						   (beginning-of-line)
						   (point))))))))
	       lst))
	(erase-buffer)
	(while lst
	  (setq elt (car lst) lst (cdr lst))
	  (if elt
	      (progn
		(insert (elt elt 3)
			127
			(if (string-match "^package " (car elt))
			    (substring (car elt) 8)
			  (car elt) )
			1
			(number-to-string (elt elt 2)) ; Line
			","
			(number-to-string (1- (elt elt 1))) ; Char pos 0-based
			"\n")
		(if (and (string-match "^[_a-zA-Z]+::" (car elt))
			 (string-match "^sub[ \t]+\\([_a-zA-Z]+\\)[^:_a-zA-Z]"
				       (elt elt 3)))
		    ;; Need to insert the name without package as well
		    (setq lst (cons (cons (substring (elt elt 3)
						     (match-beginning 1)
						     (match-end 1))
					  (cdr elt))
				    lst))))))
	(setq pos (point))
	(goto-char 1)
	(setq rel file)
	;; On case-preserving filesystems (EMX on OS/2) case might be encoded in properties
	(set-text-properties 0 (length rel) nil rel)
	(and (equal topdir (substring rel 0 (length topdir)))
	     (setq rel (substring file (length topdir))))
	(insert "\f\n" rel "," (number-to-string (1- pos)) "\n")
	(setq ret (buffer-substring 1 (point-max)))
	(erase-buffer)
	(or noninteractive
	    (message "Scanning file %s finished" file))
	ret))))

(defun cperl-add-tags-recurse-noxs ()
  "Add to TAGS data for \"pure\" Perl files in the current directory and kids.
Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \
        -f cperl-add-tags-recurse-noxs
"
  (cperl-write-tags nil nil t t nil t))

(defun cperl-add-tags-recurse-noxs-fullpath ()
  "Add to TAGS data for \"pure\" Perl in the current directory and kids.
Writes down fullpath, so TAGS is relocatable (but if the build directory
is relocated, the file TAGS inside it breaks). Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \
        -f cperl-add-tags-recurse-noxs-fullpath
"
  (cperl-write-tags nil nil t t nil t ""))

(defun cperl-add-tags-recurse ()
  "Add to TAGS file data for Perl files in the current directory and kids.
Use as
  emacs -batch -q -no-site-file -l emacs/cperl-mode.el \
        -f cperl-add-tags-recurse
"
  (cperl-write-tags nil nil t t))

(defun cperl-write-tags (&optional file erase recurse dir inbuffer noxs topdir)
  ;; If INBUFFER, do not select buffer, and do not save
  ;; If ERASE is `ignore', do not erase, and do not try to delete old info.
  (require 'etags)
  (if file nil
    (setq file (if dir default-directory (buffer-file-name)))
    (if (and (not dir) (buffer-modified-p)) (error "Save buffer first!")))
  (or topdir
      (setq topdir default-directory))
  (let ((tags-file-name "TAGS")
	(case-fold-search (eq system-type 'emx))
	xs rel tm)
    (save-excursion
      (cond (inbuffer nil)		; Already there
	    ((file-exists-p tags-file-name)
	     (if cperl-xemacs-p
		 (visit-tags-table-buffer)
	       (visit-tags-table-buffer tags-file-name)))
	    (t (set-buffer (find-file-noselect tags-file-name))))
      (cond
       (dir
	(cond ((eq erase 'ignore))
	      (erase
	       (erase-buffer)
	       (setq erase 'ignore)))
	(let ((files
	       (condition-case err
		   (directory-files file t
				    (if recurse nil cperl-scan-files-regexp)
				    t)
		 (error
		  (if cperl-unreadable-ok nil
		    (if (y-or-n-p
			 (format "Directory %s unreadable.  Continue? " file))
			(setq cperl-unreadable-ok t
			      tm nil)	; Return empty list
		      (error "Aborting: unreadable directory %s" file)))))))
	  (mapcar (function
		   (lambda (file)
		     (cond
		      ((string-match cperl-noscan-files-regexp file)
		       nil)
		      ((not (file-directory-p file))
		       (if (string-match cperl-scan-files-regexp file)
			   (cperl-write-tags file erase recurse nil t noxs topdir)))
		      ((not recurse) nil)
		      (t (cperl-write-tags file erase recurse t t noxs topdir)))))
		  files)))
       (t
	(setq xs (string-match "\\.xs$" file))
	(if (not (and xs noxs))
	    (progn
	      (cond ((eq erase 'ignore) (goto-char (point-max)))
		    (erase (erase-buffer))
		    (t
		     (goto-char 1)
		     (setq rel file)
		     ;; On case-preserving filesystems (EMX on OS/2) case might be encoded in properties
		     (set-text-properties 0 (length rel) nil rel)
		     (and (equal topdir (substring rel 0 (length topdir)))
			  (setq rel (substring file (length topdir))))
		     (if (search-forward (concat "\f\n" rel ",") nil t)
			 (progn
			   (search-backward "\f\n")
			   (delete-region (point)
					  (save-excursion
					    (forward-char 1)
					    (if (search-forward "\f\n"
								nil 'toend)
						(- (point) 2)
					      (point-max)))))
		       (goto-char (point-max)))))
	      (insert (cperl-find-tags file xs topdir))))))
      (if inbuffer nil			; Delegate to the caller
	(save-buffer 0)			; No backup
	(if (fboundp 'initialize-new-tags-table) ; Do we need something special in XEmacs?
	    (initialize-new-tags-table))))))

(defvar cperl-tags-hier-regexp-list
  (concat
   "^\\("
      "\\(package\\)\\>"
     "\\|"
      "sub\\>[^\n]+::"
     "\\|"
      "[a-zA-Z_][a-zA-Z_0-9:]*(\C-?[^\n]+::" ; XSUB?
     "\\|"
      "[ \t]*BOOT:\C-?[^\n]+::"		; BOOT section
   "\\)"))

(defvar cperl-hierarchy '(() ())
  "Global hierarchy of classes.")

(defun cperl-tags-hier-fill ()
  ;; Suppose we are in a tag table cooked by cperl.
  (goto-char 1)
  (let (type pack name pos line chunk ord cons1 file str info fileind)
    (while (re-search-forward cperl-tags-hier-regexp-list nil t)
      (setq pos (match-beginning 0)
	    pack (match-beginning 2))
      (beginning-of-line)
      (if (looking-at (concat
		       "\\([^\n]+\\)"
		       "\C-?"
		       "\\([^\n]+\\)"
		       "\C-a"
		       "\\([0-9]+\\)"
		       ","
		       "\\([0-9]+\\)"))
	  (progn
	    (setq ;;str (buffer-substring (match-beginning 1) (match-end 1))
		  name (buffer-substring (match-beginning 2) (match-end 2))
		  ;;pos (buffer-substring (match-beginning 3) (match-end 3))
		  line (buffer-substring (match-beginning 3) (match-end 3))
		  ord (if pack 1 0)
		  file (file-of-tag)
		  fileind (format "%s:%s" file line)
		  ;; Moves to beginning of the next line:
		  info (cperl-etags-snarf-tag file line))
	    ;; Move back
	    (forward-char -1)
	    ;; Make new member of hierarchy name ==> file ==> pos if needed
	    (if (setq cons1 (assoc name (nth ord cperl-hierarchy)))
		;; Name known
		(setcdr cons1 (cons (cons fileind (vector file info))
				    (cdr cons1)))
	      ;; First occurrence of the name, start alist
	      (setq cons1 (cons name (list (cons fileind (vector file info)))))
	      (if pack
		  (setcar (cdr cperl-hierarchy)
			  (cons cons1 (nth 1 cperl-hierarchy)))
		(setcar cperl-hierarchy
			(cons cons1 (car cperl-hierarchy)))))))
      (end-of-line))))

(defun cperl-tags-hier-init (&optional update)
  "Show hierarchical menu of classes and methods.
Finds info about classes by a scan of loaded TAGS files.
Supposes that the TAGS files contain fully qualified function names.
One may build such TAGS files from CPerl mode menu."
  (interactive)
  (require 'etags)
  (require 'imenu)
  (if (or update (null (nth 2 cperl-hierarchy)))
      (let ((remover (function (lambda (elt) ; (name (file1...) (file2..))
				 (or (nthcdr 2 elt)
				     ;; Only in one file
				     (setcdr elt (cdr (nth 1 elt)))))))
	    pack name cons1 to l1 l2 l3 l4 b)
	;; (setq cperl-hierarchy '(() () ())) ; Would write into '() later!
	(setq cperl-hierarchy (list l1 l2 l3))
	(if cperl-xemacs-p		; Not checked
	    (progn
	      (or tags-file-name
		  ;; Does this work in XEmacs?
		  (call-interactively 'visit-tags-table))
	      (message "Updating list of classes...")
	      (set-buffer (get-file-buffer tags-file-name))
	      (cperl-tags-hier-fill))
	  (or tags-table-list
	      (call-interactively 'visit-tags-table))
	  (mapcar
	   (function
	    (lambda (tagsfile)
	      (message "Updating list of classes... %s" tagsfile)
	      (set-buffer (get-file-buffer tagsfile))
	      (cperl-tags-hier-fill)))
	   tags-table-list)
	  (message "Updating list of classes... postprocessing..."))
	(mapcar remover (car cperl-hierarchy))
	(mapcar remover (nth 1 cperl-hierarchy))
	(setq to (list nil (cons "Packages: " (nth 1 cperl-hierarchy))
		       (cons "Methods: " (car cperl-hierarchy))))
	(cperl-tags-treeify to 1)
	(setcar (nthcdr 2 cperl-hierarchy)
		(cperl-menu-to-keymap (cons '("+++UPDATE+++" . -999) (cdr to))))
	(message "Updating list of classes: done, requesting display...")
	;;(cperl-imenu-addback (nth 2 cperl-hierarchy))
	))
  (or (nth 2 cperl-hierarchy)
      (error "No items found"))
  (setq update
;;;	(imenu-choose-buffer-index "Packages: " (nth 2 cperl-hierarchy))
	(if (if (fboundp 'display-popup-menus-p)
		(let ((f 'display-popup-menus-p))
		  (funcall f))
	      window-system)
	    (x-popup-menu t (nth 2 cperl-hierarchy))
	  (require 'tmm)
	  (tmm-prompt (nth 2 cperl-hierarchy))))
  (if (and update (listp update))
      (progn (while (cdr update) (setq update (cdr update)))
	     (setq update (car update)))) ; Get the last from the list
  (if (vectorp update)
      (progn
	(find-file (elt update 0))
	(cperl-etags-goto-tag-location (elt update 1))))
  (if (eq update -999) (cperl-tags-hier-init t)))

(defun cperl-tags-treeify (to level)
  ;; cadr of `to' is read-write.  On start it is a cons
  (let* ((regexp (concat "^\\(" (mapconcat
				 'identity
				 (make-list level "[_a-zA-Z0-9]+")
				 "::")
			 "\\)\\(::\\)?"))
	 (packages (cdr (nth 1 to)))
	 (methods (cdr (nth 2 to)))
	 l1 head tail cons1 cons2 ord writeto packs recurse
	 root-packages root-functions ms many_ms same_name ps
	 (move-deeper
	  (function
	   (lambda (elt)
	     (cond ((and (string-match regexp (car elt))
			 (or (eq ord 1) (match-end 2)))
		    (setq head (substring (car elt) 0 (match-end 1))
			  tail (if (match-end 2) (substring (car elt)
							    (match-end 2)))
			  recurse t)
		    (if (setq cons1 (assoc head writeto)) nil
		      ;; Need to init new head
		      (setcdr writeto (cons (list head (list "Packages: ")
						  (list "Methods: "))
					    (cdr writeto)))
		      (setq cons1 (nth 1 writeto)))
		    (setq cons2 (nth ord cons1)) ; Either packs or meths
		    (setcdr cons2 (cons elt (cdr cons2))))
		   ((eq ord 2)
		    (setq root-functions (cons elt root-functions)))
		   (t
		    (setq root-packages (cons elt root-packages))))))))
    (setcdr to l1)			; Init to dynamic space
    (setq writeto to)
    (setq ord 1)
    (mapcar move-deeper packages)
    (setq ord 2)
    (mapcar move-deeper methods)
    (if recurse
	(mapcar (function (lambda (elt)
			  (cperl-tags-treeify elt (1+ level))))
		(cdr to)))
    ;;Now clean up leaders with one child only
    (mapcar (function (lambda (elt)
			(if (not (and (listp (cdr elt))
				      (eq (length elt) 2))) nil
			    (setcar elt (car (nth 1 elt)))
			    (setcdr elt (cdr (nth 1 elt))))))
	    (cdr to))
    ;; Sort the roots of subtrees
    (if (default-value 'imenu-sort-function)
	(setcdr to
		(sort (cdr to) (default-value 'imenu-sort-function))))
    ;; Now add back functions removed from display
    (mapcar (function (lambda (elt)
			(setcdr to (cons elt (cdr to)))))
	    (if (default-value 'imenu-sort-function)
		(nreverse
		 (sort root-functions (default-value 'imenu-sort-function)))
	      root-functions))
    ;; Now add back packages removed from display
    (mapcar (function (lambda (elt)
			(setcdr to (cons (cons (concat "package " (car elt))
					       (cdr elt))
					 (cdr to)))))
	    (if (default-value 'imenu-sort-function)
		(nreverse
		 (sort root-packages (default-value 'imenu-sort-function)))
	      root-packages))))

;;;(x-popup-menu t
;;;   '(keymap "Name1"
;;;	    ("Ret1" "aa")
;;;	    ("Head1" "ab"
;;;	     keymap "Name2"
;;;	     ("Tail1" "x") ("Tail2" "y"))))

(defun cperl-list-fold (list name limit)
  (let (list1 list2 elt1 (num 0))
    (if (<= (length list) limit) list
      (setq list1 nil list2 nil)
      (while list
	(setq num (1+ num)
	      elt1 (car list)
	      list (cdr list))
	(if (<= num imenu-max-items)
	    (setq list2 (cons elt1 list2))
	  (setq list1 (cons (cons name
				  (nreverse list2))
			    list1)
		list2 (list elt1)
		num 1)))
      (nreverse (cons (cons name
			    (nreverse list2))
		      list1)))))

(defun cperl-menu-to-keymap (menu &optional name)
  (let (list)
    (cons 'keymap
	  (mapcar
	   (function
	    (lambda (elt)
	      (cond ((listp (cdr elt))
		     (setq list (cperl-list-fold
				 (cdr elt) (car elt) imenu-max-items))
		     (cons nil
			   (cons (car elt)
				 (cperl-menu-to-keymap list))))
		    (t
		     (list (cdr elt) (car elt) t))))) ; t is needed in 19.34
	   (cperl-list-fold menu "Root" imenu-max-items)))))


(defvar cperl-bad-style-regexp
  (mapconcat 'identity
	     '("[^-\n\t <>=+!.&|(*/'`\"#^][-=+<>!|&^]" ; char sign
	       "[-<>=+^&|]+[^- \t\n=+<>~]") ; sign+ char
	     "\\|")
  "Finds places such that insertion of a whitespace may help a lot.")

(defvar cperl-not-bad-style-regexp
  (mapconcat
   'identity
   '("[^-\t <>=+]\\(--\\|\\+\\+\\)"	; var-- var++
     "[a-zA-Z0-9_][|&][a-zA-Z0-9_$]"	; abc|def abc&def are often used.
     "&[(a-zA-Z0-9_$]"			; &subroutine &(var->field)
     "<\\$?\\sw+\\(\\.\\(\\sw\\|_\\)+\\)?>"	; <IN> <stdin.h>
     "-[a-zA-Z][ \t]+[_$\"'`a-zA-Z]"	; -f file, -t STDIN
     "-[0-9]"				; -5
     "\\+\\+"				; ++var
     "--"				; --var
     ".->"				; a->b
     "->"				; a SPACE ->b
     "\\[-"				; a[-1]
     "\\\\[&$@*\\\\]"			; \&func
     "^="				; =head
     "\\$."				; $|
     "<<[a-zA-Z_'\"`]"			; <<FOO, <<'FOO'
     "||"
     "&&"
     "[CBIXSLFZ]<\\(\\sw\\|\\s \\|\\s_\\|[\n]\\)*>" ; C<code like text>
     "-[a-zA-Z_0-9]+[ \t]*=>"		; -option => value
     ;; Unaddressed trouble spots: = -abc, f(56, -abc) --- specialcased below
     ;;"[*/+-|&<.]+="
     )
   "\\|")
  "If matches at the start of match found by `my-bad-c-style-regexp',
insertion of a whitespace will not help.")

(defvar found-bad)

(defun cperl-find-bad-style ()
  "Find places in the buffer where insertion of a whitespace may help.
Prompts user for insertion of spaces.
Currently it is tuned to C and Perl syntax."
  (interactive)
  (let (found-bad (p (point)))
    (setq last-nonmenu-event 13)	; To disable popup
    (goto-char (point-min))
    (map-y-or-n-p "Insert space here? "
		  (lambda (arg) (insert " "))
		  'cperl-next-bad-style
		  '("location" "locations" "insert a space into")
		  '((?\C-r (lambda (arg)
			     (let ((buffer-quit-function
				    'exit-recursive-edit))
			       (message "Exit with Esc Esc")
			       (recursive-edit)
			       t))	; Consider acted upon
			   "edit, exit with Esc Esc")
		    (?e (lambda (arg)
			  (let ((buffer-quit-function
				 'exit-recursive-edit))
			    (message "Exit with Esc Esc")
			    (recursive-edit)
			    t))		; Consider acted upon
			"edit, exit with Esc Esc"))
		  t)
    (if found-bad (goto-char found-bad)
      (goto-char p)
      (message "No appropriate place found"))))

(defun cperl-next-bad-style ()
  (let (p (not-found t) (point (point)) found)
    (while (and not-found
		(re-search-forward cperl-bad-style-regexp nil 'to-end))
      (setq p (point))
      (goto-char (match-beginning 0))
      (if (or
	   (looking-at cperl-not-bad-style-regexp)
	   ;; Check for a < -b and friends
	   (and (eq (following-char) ?\-)
		(save-excursion
		  (skip-chars-backward " \t\n")
		  (memq (preceding-char) '(?\= ?\> ?\< ?\, ?\( ?\[ ?\{))))
	   ;; Now check for syntax type
	   (save-match-data
	     (setq found (point))
	     (beginning-of-defun)
	     (let ((pps (parse-partial-sexp (point) found)))
	       (or (nth 3 pps) (nth 4 pps) (nth 5 pps)))))
	  (goto-char (match-end 0))
	(goto-char (1- p))
	(setq not-found nil
	      found-bad found)))
    (not not-found)))


;;; Getting help
(defvar cperl-have-help-regexp
  ;;(concat "\\("
  (mapconcat
   'identity
   '("[$@%*&][0-9a-zA-Z_:]+\\([ \t]*[[{]\\)?" ; Usual variable
     "[$@]\\^[a-zA-Z]"			; Special variable
     "[$@][^ \n\t]"			; Special variable
     "-[a-zA-Z]"			; File test
     "\\\\[a-zA-Z0]"			; Special chars
     "^=[a-z][a-zA-Z0-9_]*"		; POD sections
     "[-!&*+,-./<=>?\\\\^|~]+"		; Operator
     "[a-zA-Z_0-9:]+"			; symbol or number
     "x="
     "#!")
   ;;"\\)\\|\\("
   "\\|")
  ;;"\\)"
  ;;)
  "Matches places in the buffer we can find help for.")

(defvar cperl-message-on-help-error t)
(defvar cperl-help-from-timer nil)

(defun cperl-word-at-point-hard ()
  ;; Does not save-excursion
  ;; Get to the something meaningful
  (or (eobp) (eolp) (forward-char 1))
  (re-search-backward "[-a-zA-Z0-9_:!&*+,-./<=>?\\\\^|~$%@]"
		      (save-excursion (beginning-of-line) (point))
		      'to-beg)
  ;;  (cond
  ;;   ((or (eobp) (looking-at "[][ \t\n{}();,]")) ; Not at a symbol
  ;;    (skip-chars-backward " \n\t\r({[]});,")
  ;;    (or (bobp) (backward-char 1))))
  ;; Try to backtrace
  (cond
   ((looking-at "[a-zA-Z0-9_:]")	; symbol
    (skip-chars-backward "a-zA-Z0-9_:")
    (cond
     ((and (eq (preceding-char) ?^)	; $^I
	   (eq (char-after (- (point) 2)) ?\$))
      (forward-char -2))
     ((memq (preceding-char) (append "*$@%&\\" nil)) ; *glob
      (forward-char -1))
     ((and (eq (preceding-char) ?\=)
	   (eq (current-column) 1))
      (forward-char -1)))		; =head1
    (if (and (eq (preceding-char) ?\<)
	     (looking-at "\\$?[a-zA-Z0-9_:]+>")) ; <FH>
	(forward-char -1)))
   ((and (looking-at "=") (eq (preceding-char) ?x)) ; x=
    (forward-char -1))
   ((and (looking-at "\\^") (eq (preceding-char) ?\$)) ; $^I
    (forward-char -1))
   ((looking-at "[-!&*+,-./<=>?\\\\^|~]")
    (skip-chars-backward "-!&*+,-./<=>?\\\\^|~")
    (cond
     ((and (eq (preceding-char) ?\$)
	   (not (eq (char-after (- (point) 2)) ?\$))) ; $-
      (forward-char -1))
     ((and (eq (following-char) ?\>)
	   (string-match "[a-zA-Z0-9_]" (char-to-string (preceding-char)))
	   (save-excursion
	     (forward-sexp -1)
	     (and (eq (preceding-char) ?\<)
		  (looking-at "\\$?[a-zA-Z0-9_:]+>")))) ; <FH>
      (search-backward "<"))))
   ((and (eq (following-char) ?\$)
	 (eq (preceding-char) ?\<)
	 (looking-at "\\$?[a-zA-Z0-9_:]+>")) ; <$fh>
    (forward-char -1)))
  (if (looking-at cperl-have-help-regexp)
      (buffer-substring (match-beginning 0) (match-end 0))))

(defun cperl-get-help ()
  "Get one-line docs on the symbol at the point.
The data for these docs is a little bit obsolete and may be in fact longer
than a line.  Your contribution to update/shorten it is appreciated."
  (interactive)
  (save-match-data			; May be called "inside" query-replace
    (save-excursion
      (let ((word (cperl-word-at-point-hard)))
	(if word
	    (if (and cperl-help-from-timer ; Bail out if not in mainland
		     (not (string-match "^#!\\|\\\\\\|^=" word)) ; Show help even in comments/strings.
		     (or (memq (get-text-property (point) 'face)
			       '(font-lock-comment-face font-lock-string-face))
			 (memq (get-text-property (point) 'syntax-type)
			       '(pod here-doc format))))
		nil
	      (cperl-describe-perl-symbol word))
	  (if cperl-message-on-help-error
	      (message "Nothing found for %s..."
		       (buffer-substring (point) (min (+ 5 (point)) (point-max))))))))))

;;; Stolen from perl-descr.el by Johan Vromans:

(defvar cperl-doc-buffer " *perl-doc*"
  "Where the documentation can be found.")

(defun cperl-describe-perl-symbol (val)
  "Display the documentation of symbol at point, a Perl operator."
  (let ((enable-recursive-minibuffers t)
	args-file regexp)
    (cond
     ((string-match "^[&*][a-zA-Z_]" val)
      (setq val (concat (substring val 0 1) "NAME")))
     ((string-match "^[$@]\\([a-zA-Z_:0-9]+\\)[ \t]*\\[" val)
      (setq val (concat "@" (substring val 1 (match-end 1)))))
     ((string-match "^[$@]\\([a-zA-Z_:0-9]+\\)[ \t]*{" val)
      (setq val (concat "%" (substring val 1 (match-end 1)))))
     ((and (string= val "x") (string-match "^x=" val))
      (setq val "x="))
     ((string-match "^\\$[\C-a-\C-z]" val)
      (setq val (concat "$^" (char-to-string (+ ?A -1 (aref val 1))))))
     ((string-match "^CORE::" val)
      (setq val "CORE::"))
     ((string-match "^SUPER::" val)
      (setq val "SUPER::"))
     ((and (string= "<" val) (string-match "^<\\$?[a-zA-Z0-9_:]+>" val))
      (setq val "<NAME>")))
    (setq regexp (concat "^"
			 "\\([^a-zA-Z0-9_:]+[ \t]+\\)?"
			 (regexp-quote val)
			 "\\([ \t([/]\\|$\\)"))

    ;; get the buffer with the documentation text
    (cperl-switch-to-doc-buffer)

    ;; lookup in the doc
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (list
       (if (re-search-forward regexp (point-max) t)
	   (save-excursion
	     (beginning-of-line 1)
	     (let ((lnstart (point)))
	       (end-of-line)
	       (message "%s" (buffer-substring lnstart (point)))))
	 (if cperl-message-on-help-error
	     (message "No definition for %s" val)))))))

(defvar cperl-short-docs 'please-ignore-this-line
  ;; Perl4 version was written by Johan Vromans (jvromans@squirrel.nl)
  "# based on '@(#)@ perl-descr.el 1.9 - describe-perl-symbol' [Perl 5]
...	Range (list context); flip/flop [no flop when flip] (scalar context).
! ...	Logical negation.
... != ...	Numeric inequality.
... !~ ...	Search pattern, substitution, or translation (negated).
$!	In numeric context: errno.  In a string context: error string.
$\"	The separator which joins elements of arrays interpolated in strings.
$#	The output format for printed numbers.  Default is %.15g or close.
$$	Process number of this script.  Changes in the fork()ed child process.
$%	The current page number of the currently selected output channel.

	The following variables are always local to the current block:

$1	Match of the 1st set of parentheses in the last match (auto-local).
$2	Match of the 2nd set of parentheses in the last match (auto-local).
$3	Match of the 3rd set of parentheses in the last match (auto-local).
$4	Match of the 4th set of parentheses in the last match (auto-local).
$5	Match of the 5th set of parentheses in the last match (auto-local).
$6	Match of the 6th set of parentheses in the last match (auto-local).
$7	Match of the 7th set of parentheses in the last match (auto-local).
$8	Match of the 8th set of parentheses in the last match (auto-local).
$9	Match of the 9th set of parentheses in the last match (auto-local).
$&	The string matched by the last pattern match (auto-local).
$'	The string after what was matched by the last match (auto-local).
$`	The string before what was matched by the last match (auto-local).

$(	The real gid of this process.
$)	The effective gid of this process.
$*	Deprecated: Set to 1 to do multiline matching within a string.
$+	The last bracket matched by the last search pattern.
$,	The output field separator for the print operator.
$-	The number of lines left on the page.
$.	The current input line number of the last filehandle that was read.
$/	The input record separator, newline by default.
$0	Name of the file containing the current perl script (read/write).
$:     String may be broken after these characters to fill ^-lines in a format.
$;	Subscript separator for multi-dim array emulation.  Default \"\\034\".
$<	The real uid of this process.
$=	The page length of the current output channel.  Default is 60 lines.
$>	The effective uid of this process.
$?	The status returned by the last ``, pipe close or `system'.
$@	The perl error message from the last eval or do @var{EXPR} command.
$ARGV	The name of the current file used with <> .
$[	Deprecated: The index of the first element/char in an array/string.
$\\	The output record separator for the print operator.
$]	The perl version string as displayed with perl -v.
$^	The name of the current top-of-page format.
$^A     The current value of the write() accumulator for format() lines.
$^D	The value of the perl debug (-D) flags.
$^E     Information about the last system error other than that provided by $!.
$^F	The highest system file descriptor, ordinarily 2.
$^H     The current set of syntax checks enabled by `use strict'.
$^I	The value of the in-place edit extension (perl -i option).
$^L     What formats output to perform a formfeed.  Default is \f.
$^M     A buffer for emergency memory allocation when running out of memory.
$^O     The operating system name under which this copy of Perl was built.
$^P	Internal debugging flag.
$^T	The time the script was started.  Used by -A/-M/-C file tests.
$^W	True if warnings are requested (perl -w flag).
$^X	The name under which perl was invoked (argv[0] in C-speech).
$_	The default input and pattern-searching space.
$|	Auto-flush after write/print on current output channel?  Default 0.
$~	The name of the current report format.
... % ...	Modulo division.
... %= ...	Modulo division assignment.
%ENV	Contains the current environment.
%INC	List of files that have been require-d or do-ne.
%SIG	Used to set signal handlers for various signals.
... & ...	Bitwise and.
... && ...	Logical and.
... &&= ...	Logical and assignment.
... &= ...	Bitwise and assignment.
... * ...	Multiplication.
... ** ...	Exponentiation.
*NAME	Glob: all objects refered by NAME.  *NAM1 = *NAM2 aliases NAM1 to NAM2.
&NAME(arg0, ...)	Subroutine call.  Arguments go to @_.
... + ...	Addition.		+EXPR	Makes EXPR into scalar context.
++	Auto-increment (magical on strings).	++EXPR	EXPR++
... += ...	Addition assignment.
,	Comma operator.
... - ...	Subtraction.
--	Auto-decrement (NOT magical on strings).	--EXPR	EXPR--
... -= ...	Subtraction assignment.
-A	Access time in days since script started.
-B	File is a non-text (binary) file.
-C	Inode change time in days since script started.
-M	Age in days since script started.
-O	File is owned by real uid.
-R	File is readable by real uid.
-S	File is a socket .
-T	File is a text file.
-W	File is writable by real uid.
-X	File is executable by real uid.
-b	File is a block special file.
-c	File is a character special file.
-d	File is a directory.
-e	File exists .
-f	File is a plain file.
-g	File has setgid bit set.
-k	File has sticky bit set.
-l	File is a symbolic link.
-o	File is owned by effective uid.
-p	File is a named pipe (FIFO).
-r	File is readable by effective uid.
-s	File has non-zero size.
-t	Tests if filehandle (STDIN by default) is opened to a tty.
-u	File has setuid bit set.
-w	File is writable by effective uid.
-x	File is executable by effective uid.
-z	File has zero size.
.	Concatenate strings.
..	Range (list context); flip/flop (scalar context) operator.
.=	Concatenate assignment strings
... / ...	Division.	/PATTERN/ioxsmg	Pattern match
... /= ...	Division assignment.
/PATTERN/ioxsmg	Pattern match.
... < ...    Numeric less than.	<pattern>	Glob.	See <NAME>, <> as well.
<NAME>	Reads line from filehandle NAME (a bareword or dollar-bareword).
<pattern>	Glob (Unless pattern is bareword/dollar-bareword - see <NAME>).
<>	Reads line from union of files in @ARGV (= command line) and STDIN.
... << ...	Bitwise shift left.	<<	start of HERE-DOCUMENT.
... <= ...	Numeric less than or equal to.
... <=> ...	Numeric compare.
... = ...	Assignment.
... == ...	Numeric equality.
... =~ ...	Search pattern, substitution, or translation
... > ...	Numeric greater than.
... >= ...	Numeric greater than or equal to.
... >> ...	Bitwise shift right.
... >>= ...	Bitwise shift right assignment.
... ? ... : ...	Condition=if-then-else operator.   ?PAT? One-time pattern match.
?PATTERN?	One-time pattern match.
@ARGV	Command line arguments (not including the command name - see $0).
@INC	List of places to look for perl scripts during do/include/use.
@_    Parameter array for subroutines; result of split() unless in list context.
\\  Creates reference to what follows, like \$var, or quotes non-\w in strings.
\\0	Octal char, e.g. \\033.
\\E	Case modification terminator.  See \\Q, \\L, and \\U.
\\L	Lowercase until \\E .  See also \\l, lc.
\\U	Upcase until \\E .  See also \\u, uc.
\\Q	Quote metacharacters until \\E .  See also quotemeta.
\\a	Alarm character (octal 007).
\\b	Backspace character (octal 010).
\\c	Control character, e.g. \\c[ .
\\e	Escape character (octal 033).
\\f	Formfeed character (octal 014).
\\l	Lowercase the next character.  See also \\L and \\u, lcfirst.
\\n	Newline character (octal 012 on most systems).
\\r	Return character (octal 015 on most systems).
\\t	Tab character (octal 011).
\\u	Upcase the next character.  See also \\U and \\l, ucfirst.
\\x	Hex character, e.g. \\x1b.
... ^ ...	Bitwise exclusive or.
__END__	Ends program source.
__DATA__	Ends program source.
__FILE__	Current (source) filename.
__LINE__	Current line in current source.
__PACKAGE__	Current package.
ARGV	Default multi-file input filehandle.  <ARGV> is a synonym for <>.
ARGVOUT	Output filehandle with -i flag.
BEGIN { ... }	Immediately executed (during compilation) piece of code.
END { ... }	Pseudo-subroutine executed after the script finishes.
CHECK { ... }	Pseudo-subroutine executed after the script is compiled.
INIT { ... }	Pseudo-subroutine executed before the script starts running.
DATA	Input filehandle for what follows after __END__	or __DATA__.
accept(NEWSOCKET,GENERICSOCKET)
alarm(SECONDS)
atan2(X,Y)
bind(SOCKET,NAME)
binmode(FILEHANDLE)
caller[(LEVEL)]
chdir(EXPR)
chmod(LIST)
chop[(LIST|VAR)]
chown(LIST)
chroot(FILENAME)
close(FILEHANDLE)
closedir(DIRHANDLE)
... cmp ...	String compare.
connect(SOCKET,NAME)
continue of { block } continue { block }.  Is executed after `next' or at end.
cos(EXPR)
crypt(PLAINTEXT,SALT)
dbmclose(%HASH)
dbmopen(%HASH,DBNAME,MODE)
defined(EXPR)
delete($HASH{KEY})
die(LIST)
do { ... }|SUBR while|until EXPR	executes at least once
do(EXPR|SUBR([LIST]))	(with while|until executes at least once)
dump LABEL
each(%HASH)
endgrent
endhostent
endnetent
endprotoent
endpwent
endservent
eof[([FILEHANDLE])]
... eq ...	String equality.
eval(EXPR) or eval { BLOCK }
exec([TRUENAME] ARGV0, ARGVs)     or     exec(SHELL_COMMAND_LINE)
exit(EXPR)
exp(EXPR)
fcntl(FILEHANDLE,FUNCTION,SCALAR)
fileno(FILEHANDLE)
flock(FILEHANDLE,OPERATION)
for (EXPR;EXPR;EXPR) { ... }
foreach [VAR] (@ARRAY) { ... }
fork
... ge ...	String greater than or equal.
getc[(FILEHANDLE)]
getgrent
getgrgid(GID)
getgrnam(NAME)
gethostbyaddr(ADDR,ADDRTYPE)
gethostbyname(NAME)
gethostent
getlogin
getnetbyaddr(ADDR,ADDRTYPE)
getnetbyname(NAME)
getnetent
getpeername(SOCKET)
getpgrp(PID)
getppid
getpriority(WHICH,WHO)
getprotobyname(NAME)
getprotobynumber(NUMBER)
getprotoent
getpwent
getpwnam(NAME)
getpwuid(UID)
getservbyname(NAME,PROTO)
getservbyport(PORT,PROTO)
getservent
getsockname(SOCKET)
getsockopt(SOCKET,LEVEL,OPTNAME)
gmtime(EXPR)
goto LABEL
... gt ...	String greater than.
hex(EXPR)
if (EXPR) { ... } [ elsif (EXPR) { ... } ... ] [ else { ... } ] or EXPR if EXPR
index(STR,SUBSTR[,OFFSET])
int(EXPR)
ioctl(FILEHANDLE,FUNCTION,SCALAR)
join(EXPR,LIST)
keys(%HASH)
kill(LIST)
last [LABEL]
... le ...	String less than or equal.
length(EXPR)
link(OLDFILE,NEWFILE)
listen(SOCKET,QUEUESIZE)
local(LIST)
localtime(EXPR)
log(EXPR)
lstat(EXPR|FILEHANDLE|VAR)
... lt ...	String less than.
m/PATTERN/iogsmx
mkdir(FILENAME,MODE)
msgctl(ID,CMD,ARG)
msgget(KEY,FLAGS)
msgrcv(ID,VAR,SIZE,TYPE.FLAGS)
msgsnd(ID,MSG,FLAGS)
my VAR or my (VAR1,...)	Introduces a lexical variable ($VAR, @ARR, or %HASH).
our VAR or our (VAR1,...) Lexically enable a global variable ($V, @A, or %H).
... ne ...	String inequality.
next [LABEL]
oct(EXPR)
open(FILEHANDLE[,EXPR])
opendir(DIRHANDLE,EXPR)
ord(EXPR)	ASCII value of the first char of the string.
pack(TEMPLATE,LIST)
package NAME	Introduces package context.
pipe(READHANDLE,WRITEHANDLE)	Create a pair of filehandles on ends of a pipe.
pop(ARRAY)
print [FILEHANDLE] [(LIST)]
printf [FILEHANDLE] (FORMAT,LIST)
push(ARRAY,LIST)
q/STRING/	Synonym for 'STRING'
qq/STRING/	Synonym for \"STRING\"
qx/STRING/	Synonym for `STRING`
rand[(EXPR)]
read(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
readdir(DIRHANDLE)
readlink(EXPR)
recv(SOCKET,SCALAR,LEN,FLAGS)
redo [LABEL]
rename(OLDNAME,NEWNAME)
require [FILENAME | PERL_VERSION]
reset[(EXPR)]
return(LIST)
reverse(LIST)
rewinddir(DIRHANDLE)
rindex(STR,SUBSTR[,OFFSET])
rmdir(FILENAME)
s/PATTERN/REPLACEMENT/gieoxsm
scalar(EXPR)
seek(FILEHANDLE,POSITION,WHENCE)
seekdir(DIRHANDLE,POS)
select(FILEHANDLE | RBITS,WBITS,EBITS,TIMEOUT)
semctl(ID,SEMNUM,CMD,ARG)
semget(KEY,NSEMS,SIZE,FLAGS)
semop(KEY,...)
send(SOCKET,MSG,FLAGS[,TO])
setgrent
sethostent(STAYOPEN)
setnetent(STAYOPEN)
setpgrp(PID,PGRP)
setpriority(WHICH,WHO,PRIORITY)
setprotoent(STAYOPEN)
setpwent
setservent(STAYOPEN)
setsockopt(SOCKET,LEVEL,OPTNAME,OPTVAL)
shift[(ARRAY)]
shmctl(ID,CMD,ARG)
shmget(KEY,SIZE,FLAGS)
shmread(ID,VAR,POS,SIZE)
shmwrite(ID,STRING,POS,SIZE)
shutdown(SOCKET,HOW)
sin(EXPR)
sleep[(EXPR)]
socket(SOCKET,DOMAIN,TYPE,PROTOCOL)
socketpair(SOCKET1,SOCKET2,DOMAIN,TYPE,PROTOCOL)
sort [SUBROUTINE] (LIST)
splice(ARRAY,OFFSET[,LENGTH[,LIST]])
split[(/PATTERN/[,EXPR[,LIMIT]])]
sprintf(FORMAT,LIST)
sqrt(EXPR)
srand(EXPR)
stat(EXPR|FILEHANDLE|VAR)
study[(SCALAR)]
sub [NAME [(format)]] { BODY }	sub NAME [(format)];	sub [(format)] {...}
substr(EXPR,OFFSET[,LEN])
symlink(OLDFILE,NEWFILE)
syscall(LIST)
sysread(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
system([TRUENAME] ARGV0 [,ARGV])     or     system(SHELL_COMMAND_LINE)
syswrite(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
tell[(FILEHANDLE)]
telldir(DIRHANDLE)
time
times
tr/SEARCHLIST/REPLACEMENTLIST/cds
truncate(FILE|EXPR,LENGTH)
umask[(EXPR)]
undef[(EXPR)]
unless (EXPR) { ... } [ else { ... } ] or EXPR unless EXPR
unlink(LIST)
unpack(TEMPLATE,EXPR)
unshift(ARRAY,LIST)
until (EXPR) { ... }					EXPR until EXPR
utime(LIST)
values(%HASH)
vec(EXPR,OFFSET,BITS)
wait
waitpid(PID,FLAGS)
wantarray	Returns true if the sub/eval is called in list context.
warn(LIST)
while  (EXPR) { ... }					EXPR while EXPR
write[(EXPR|FILEHANDLE)]
... x ...	Repeat string or array.
x= ...	Repetition assignment.
y/SEARCHLIST/REPLACEMENTLIST/
... | ...	Bitwise or.
... || ...	Logical or.
~ ...		Unary bitwise complement.
#!	OS interpreter indicator.  If contains `perl', used for options, and -x.
AUTOLOAD {...}	Shorthand for `sub AUTOLOAD {...}'.
CORE::		Prefix to access builtin function if imported sub obscures it.
SUPER::		Prefix to lookup for a method in @ISA classes.
DESTROY		Shorthand for `sub DESTROY {...}'.
... EQ ...	Obsolete synonym of `eq'.
... GE ...	Obsolete synonym of `ge'.
... GT ...	Obsolete synonym of `gt'.
... LE ...	Obsolete synonym of `le'.
... LT ...	Obsolete synonym of `lt'.
... NE ...	Obsolete synonym of `ne'.
abs [ EXPR ]	absolute value
... and ...		Low-precedence synonym for &&.
bless REFERENCE [, PACKAGE]	Makes reference into an object of a package.
chomp [LIST]	Strips $/ off LIST/$_.  Returns count.  Special if $/ eq ''!
chr		Converts a number to char with the same ordinal.
else		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
elsif		Part of if/unless {BLOCK} elsif {BLOCK} else {BLOCK}.
exists $HASH{KEY}	True if the key exists.
format [NAME] =	 Start of output format.  Ended by a single dot (.) on a line.
formline PICTURE, LIST	Backdoor into \"format\" processing.
glob EXPR	Synonym of <EXPR>.
lc [ EXPR ]	Returns lowercased EXPR.
lcfirst [ EXPR ]	Returns EXPR with lower-cased first letter.
grep EXPR,LIST  or grep {BLOCK} LIST	Filters LIST via EXPR/BLOCK.
map EXPR, LIST	or map {BLOCK} LIST	Applies EXPR/BLOCK to elts of LIST.
no PACKAGE [SYMBOL1, ...]  Partial reverse for `use'.  Runs `unimport' method.
not ...		Low-precedence synonym for ! - negation.
... or ...		Low-precedence synonym for ||.
pos STRING    Set/Get end-position of the last match over this string, see \\G.
quotemeta [ EXPR ]	Quote regexp metacharacters.
qw/WORD1 .../		Synonym of split('', 'WORD1 ...')
readline FH	Synonym of <FH>.
readpipe CMD	Synonym of `CMD`.
ref [ EXPR ]	Type of EXPR when dereferenced.
sysopen FH, FILENAME, MODE [, PERM]	(MODE is numeric, see Fcntl.)
tie VAR, PACKAGE, LIST	Hide an object behind a simple Perl variable.
tied		Returns internal object for a tied data.
uc [ EXPR ]	Returns upcased EXPR.
ucfirst [ EXPR ]	Returns EXPR with upcased first letter.
untie VAR	Unlink an object from a simple Perl variable.
use PACKAGE [SYMBOL1, ...]  Compile-time `require' with consequent `import'.
... xor ...		Low-precedence synonym for exclusive or.
prototype \&SUB	Returns prototype of the function given a reference.
=head1		Top-level heading.
=head2		Second-level heading.
=head3		Third-level heading (is there such?).
=over [ NUMBER ]	Start list.
=item [ TITLE ]		Start new item in the list.
=back		End list.
=cut		Switch from POD to Perl.
=pod		Switch from Perl to POD.
")

(defun cperl-switch-to-doc-buffer ()
  "Go to the perl documentation buffer and insert the documentation."
  (interactive)
  (let ((buf (get-buffer-create cperl-doc-buffer)))
    (if (interactive-p)
	(switch-to-buffer-other-window buf)
      (set-buffer buf))
    (if (= (buffer-size) 0)
	(progn
	  (insert (documentation-property 'cperl-short-docs
					  'variable-documentation))
	  (setq buffer-read-only t)))))

(defun cperl-beautify-regexp-piece (b e embed level)
  ;; b is before the starting delimiter, e before the ending
  ;; e should be a marker, may be changed, but remains "correct".
  ;; EMBED is nil iff we process the whole REx.
  ;; The REx is guaranteed to have //x
  ;; LEVEL shows how many levels deep to go
  ;; position at enter and at leave is not defined
  (let (s c tmp (m (make-marker)) (m1 (make-marker)) c1 spaces inline code pos)
    (if (not embed)
	(goto-char (1+ b))
      (goto-char b)
      (cond ((looking-at "(\\?\\\\#")	;  (?#) wrongly commented when //x-ing
	     (forward-char 2)
	     (delete-char 1)
	     (forward-char 1))
	    ((looking-at "(\\?[^a-zA-Z]")
	     (forward-char 3))
	    ((looking-at "(\\?")	; (?i)
	     (forward-char 2))
	    (t
	     (forward-char 1))))
    (setq c (if embed (current-indentation) (1- (current-column)))
	  c1 (+ c (or cperl-regexp-indent-step cperl-indent-level)))
    (or (looking-at "[ \t]*[\n#]")
	(progn
	  (insert "\n")))
    (goto-char e)
    (beginning-of-line)
    (if (re-search-forward "[^ \t]" e t)
	(progn			       ; Something before the ending delimiter
	  (goto-char e)
	  (delete-horizontal-space)
	  (insert "\n")
	  (cperl-make-indent c)
	  (set-marker e (point))))
    (goto-char b)
    (end-of-line 2)
    (while (< (point) (marker-position e))
      (beginning-of-line)
      (setq s (point)
	    inline t)
      (skip-chars-forward " \t")
      (delete-region s (point))
      (cperl-make-indent c1)
      (while (and
	      inline
	      (looking-at
	       (concat "\\([a-zA-Z0-9]+[^*+{?]\\)" ; 1 word
		       "\\|"		; Embedded variable
		       "\\$\\([a-zA-Z0-9_]+\\([[{]\\)?\\|[^\n \t)|]\\)" ; 2 3
		       "\\|"		; $ ^
		       "[$^]"
		       "\\|"		; simple-code simple-code*?
		       "\\(\\\\.\\|[^][()#|*+?\n]\\)\\([*+{?]\\??\\)?" ; 4 5
		       "\\|"		; Class
		       "\\(\\[\\)"	; 6
		       "\\|"		; Grouping
		       "\\((\\(\\?\\)?\\)" ; 7 8
		       "\\|"		; |
		       "\\(|\\)")))	; 9
	(goto-char (match-end 0))
	(setq spaces t)
	(cond ((match-beginning 1)	; Alphanum word + junk
	       (forward-char -1))
	      ((or (match-beginning 3)	; $ab[12]
		   (and (match-beginning 5) ; X* X+ X{2,3}
			(eq (preceding-char) ?\{)))
	       (forward-char -1)
	       (forward-sexp 1))
	      ((and			; [], already syntaxified
		(match-beginning 6)
		cperl-regexp-scan
		cperl-use-syntax-table-text-property)
	       (forward-char -1)
	       (forward-sexp 1)
	       (or (eq (preceding-char) ?\])
		   (error "[]-group not terminated"))
	       (re-search-forward
		"\\=\\([*+?]\\|{[0-9]+\\(,[0-9]*\\)?}\\)\\??" e t))
	      ((match-beginning 6)	; []
	       (setq tmp (point))
	       (if (looking-at "\\^?\\]")
		   (goto-char (match-end 0)))
	       ;; XXXX POSIX classes?!
	       (while (and (not pos)
			   (re-search-forward "\\[:\\|\\]" e t))
		 (if (eq (preceding-char) ?:)
		     (or (re-search-forward ":\\]" e t)
			 (error "[:POSIX:]-group in []-group not terminated"))
		   (setq pos t)))
	       (or (eq (preceding-char) ?\])
		   (error "[]-group not terminated"))
	       (re-search-forward
		"\\=\\([*+?]\\|{[0-9]+\\(,[0-9]*\\)?}\\)\\??" e t))
	      ((match-beginning 7)	; ()
	       (goto-char (match-beginning 0))
	       (setq pos (current-column))
	       (or (eq pos c1)
		   (progn
		     (delete-horizontal-space)
		     (insert "\n")
		     (cperl-make-indent c1)))
	       (setq tmp (point))
	       (forward-sexp 1)
	       ;;	       (or (forward-sexp 1)
	       ;;		   (progn
	       ;;		     (goto-char tmp)
	       ;;		     (error "()-group not terminated")))
	       (set-marker m (1- (point)))
	       (set-marker m1 (point))
	       (if (= level 1)
		   (if (progn		; indent rigidly if multiline
			 ;; In fact does not make a lot of sense, since
			 ;; the starting position can be already lost due
			 ;; to insertion of "\n" and " "
			 (goto-char tmp)
			 (search-forward "\n" m1 t))
		       (indent-rigidly (point) m1 (- c1 pos)))
		 (setq level (1- level))
		 (cond
		  ((not (match-beginning 8))
		   (cperl-beautify-regexp-piece tmp m t level))
		  ((eq (char-after (+ 2 tmp)) ?\{) ; Code
		   t)
		  ((eq (char-after (+ 2 tmp)) ?\() ; Conditional
		   (goto-char (+ 2 tmp))
		   (forward-sexp 1)
		   (cperl-beautify-regexp-piece (point) m t level))
		  ((eq (char-after (+ 2 tmp)) ?<) ; Lookbehind
		   (goto-char (+ 3 tmp))
		   (cperl-beautify-regexp-piece (point) m t level))
		  (t
		   (cperl-beautify-regexp-piece tmp m t level))))
	       (goto-char m1)
	       (cond ((looking-at "[*+?]\\??")
		      (goto-char (match-end 0)))
		     ((eq (following-char) ?\{)
		      (forward-sexp 1)
		      (if (eq (following-char) ?\?)
			  (forward-char))))
	       (skip-chars-forward " \t")
	       (setq spaces nil)
	       (if (looking-at "[#\n]")
		   (progn
		     (or (eolp) (indent-for-comment))
		     (beginning-of-line 2))
		 (delete-horizontal-space)
		 (insert "\n"))
	       (end-of-line)
	       (setq inline nil))
	      ((match-beginning 9)	; |
	       (forward-char -1)
	       (setq tmp (point))
	       (beginning-of-line)
	       (if (re-search-forward "[^ \t]" tmp t)
		   (progn
		     (goto-char tmp)
		     (delete-horizontal-space)
		     (insert "\n"))
		 ;; first at line
		 (delete-region (point) tmp))
	       (cperl-make-indent c)
	       (forward-char 1)
	       (skip-chars-forward " \t")
	       (setq spaces nil)
	       (if (looking-at "[#\n]")
		   (beginning-of-line 2)
		 (delete-horizontal-space)
		 (insert "\n"))
	       (end-of-line)
	       (setq inline nil)))
	(or (looking-at "[ \t\n]")
	    (not spaces)
	    (insert " "))
	(skip-chars-forward " \t"))
      (or (looking-at "[#\n]")
	  (error "Unknown code `%s' in a regexp"
		 (buffer-substring (point) (1+ (point)))))
      (and inline (end-of-line 2)))
    ;; Special-case the last line of group
    (if (and (>= (point) (marker-position e))
	     (/= (current-indentation) c))
	(progn
	  (beginning-of-line)
	  (cperl-make-indent c)))))

(defun cperl-make-regexp-x ()
  ;; Returns position of the start
  ;; XXX this is called too often!  Need to cache the result!
  (save-excursion
    (or cperl-use-syntax-table-text-property
	(error "I need to have a regexp marked!"))
    ;; Find the start
    (if (looking-at "\\s|")
	nil				; good already
      (if (looking-at "\\([smy]\\|qr\\)\\s|")
	  (forward-char 1)
	(re-search-backward "\\s|")))	; Assume it is scanned already.
    ;;(forward-char 1)
    (let ((b (point)) (e (make-marker)) have-x delim (c (current-column))
	  (sub-p (eq (preceding-char) ?s)) s)
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (setq delim (preceding-char))
      (if (and sub-p (eq delim (char-after (- (point) 2))))
	  (error "Possible s/blah// - do not know how to deal with"))
      (if sub-p (forward-sexp 1))
      (if (looking-at "\\sw*x")
	  (setq have-x t)
	(insert "x"))
      ;; Protect fragile " ", "#"
      (if have-x nil
	(goto-char (1+ b))
	(while (re-search-forward "\\(\\=\\|[^\\\\]\\)\\(\\\\\\\\\\)*[ \t\n#]" e t) ; Need to include (?#) too?
	  (forward-char -1)
	  (insert "\\")
	  (forward-char 1)))
      b)))

(defun cperl-beautify-regexp (&optional deep)
  "Do it.  (Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (setq deep (if deep (prefix-numeric-value deep) -1))
  (save-excursion
    (goto-char (cperl-make-regexp-x))
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (cperl-beautify-regexp-piece b e nil deep))))

(defun cperl-regext-to-level-start ()
  "Goto start of an enclosing group in regexp.
We suppose that the regexp is scanned already."
  (interactive)
  (let ((limit (cperl-make-regexp-x)) done)
    (while (not done)
      (or (eq (following-char) ?\()
	  (search-backward "(" (1+ limit) t)
	  (error "Cannot find `(' which starts a group"))
      (setq done
	    (save-excursion
	      (skip-chars-backward "\\")
	      (looking-at "\\(\\\\\\\\\\)*(")))
      (or done (forward-char -1)))))

(defun cperl-contract-level ()
  "Find an enclosing group in regexp and contract it.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive)
  ;; (save-excursion		; Can't, breaks `cperl-contract-levels'
  (cperl-regext-to-level-start)
  (let ((b (point)) (e (make-marker)) c)
    (forward-sexp 1)
    (set-marker e (1- (point)))
    (goto-char b)
    (while (re-search-forward "\\(#\\)\\|\n" e 'to-end)
      (cond
       ((match-beginning 1)		; #-comment
	(or c (setq c (current-indentation)))
	(beginning-of-line 2)		; Skip
	(cperl-make-indent c))
       (t
	(delete-char -1)
	(just-one-space))))))

(defun cperl-contract-levels ()
  "Find an enclosing group in regexp and contract all the kids.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive)
  (save-excursion
    (condition-case nil
	(cperl-regext-to-level-start)
      (error				; We are outside outermost group
       (goto-char (cperl-make-regexp-x))))
    (let ((b (point)) (e (make-marker)) s c)
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (goto-char (1+ b))
      (while (re-search-forward "\\(\\\\\\\\\\)\\|(" e t)
	(cond
	 ((match-beginning 1)		; Skip
	  nil)
	 (t				; Group
	  (cperl-contract-level)))))))

(defun cperl-beautify-level (&optional deep)
  "Find an enclosing group in regexp and beautify it.
\(Experimental, may change semantics, recheck the result.)
We suppose that the regexp is scanned already."
  (interactive "P")
  (setq deep (if deep (prefix-numeric-value deep) -1))
  (save-excursion
    (cperl-regext-to-level-start)
    (let ((b (point)) (e (make-marker)))
      (forward-sexp 1)
      (set-marker e (1- (point)))
      (cperl-beautify-regexp-piece b e nil deep))))

(defun cperl-invert-if-unless-modifiers ()
  "Change `B if A;' into `if (A) {B}' etc if possible.
\(Unfinished.)"
  (interactive)				; 
  (let (A B pre-B post-B pre-if post-if pre-A post-A if-string
	  (w-rex "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>"))
    (and (= (char-syntax (preceding-char)) ?w)
	 (forward-sexp -1))
    (setq pre-if (point))
    (cperl-backward-to-start-of-expr)
    (setq pre-B (point))
    (forward-sexp 1)		; otherwise forward-to-end-of-expr is NOP
    (cperl-forward-to-end-of-expr)
    (setq post-A (point))
    (goto-char pre-if)
    (or (looking-at w-rex)
	;; Find the position
	(progn (goto-char post-A)
	       (while (and
		       (not (looking-at w-rex))
		       (> (point) pre-B))
		 (forward-sexp -1))
	       (setq pre-if (point))))
    (or (looking-at w-rex)
	(error "Can't find `if', `unless', `while', `until', `for' or `foreach'"))
    ;; 1 B 2 ... 3 B-com ... 4 if 5 ... if-com 6 ... 7 A 8
    (setq if-string (buffer-substring (match-beginning 0) (match-end 0)))
    ;; First, simple part: find code boundaries
    (forward-sexp 1)
    (setq post-if (point))
    (forward-sexp -2)
    (forward-sexp 1)
    (setq post-B (point))
    (cperl-backward-to-start-of-expr)
    (setq pre-B (point))
    (setq B (buffer-substring pre-B post-B))
    (goto-char pre-if)
    (forward-sexp 2)
    (forward-sexp -1)
    ;; May be after $, @, $# etc of a variable
    (skip-chars-backward "$@%#")
    (setq pre-A (point))
    (cperl-forward-to-end-of-expr)
    (setq post-A (point))
    (setq A (buffer-substring pre-A post-A))
    ;; Now modify (from end, to not break the stuff)
    (skip-chars-forward " \t;")
    (delete-region pre-A (point))	; we move to pre-A
    (insert "\n" B ";\n}")
    (and (looking-at "[ \t]*#") (cperl-indent-for-comment))
    (delete-region pre-if post-if)
    (delete-region pre-B post-B)
    (goto-char pre-B)
    (insert if-string " (" A ") {")
    (setq post-B (point))
    (if (looking-at "[ \t]+$")
	(delete-horizontal-space)
      (if (looking-at "[ \t]*#")
	  (cperl-indent-for-comment)
	(just-one-space)))
    (forward-line 1)
    (if (looking-at "[ \t]*$")
	(progn				; delete line
	  (delete-horizontal-space)
	  (delete-region (point) (1+ (point)))))
    (cperl-indent-line)
    (goto-char (1- post-B))
    (forward-sexp 1)
    (cperl-indent-line)
    (goto-char pre-B)))

(defun cperl-invert-if-unless ()
  "Change `if (A) {B}' into `B if A;' etc (or visa versa) if possible.
If the cursor is not on the leading keyword of the BLOCK flavor of
construct, will assume it is the STATEMENT flavor, so will try to find
the appropriate statement modifier."
  (interactive)
  (and (= (char-syntax (preceding-char)) ?w)
       (forward-sexp -1))
  (if (looking-at "\\<\\(if\\|unless\\|while\\|until\\|for\\|foreach\\)\\>")
      (let ((pre-if (point))
	    pre-A post-A pre-B post-B A B state p end-B-code is-block B-comment
	    (if-string (buffer-substring (match-beginning 0) (match-end 0))))
	(forward-sexp 2)
	(setq post-A (point))
	(forward-sexp -1)
	(setq pre-A (point))
	(setq is-block (and (eq (following-char) ?\( )
			    (save-excursion
			      (condition-case nil
				  (progn
				    (forward-sexp 2)
				    (forward-sexp -1)
				    (eq (following-char) ?\{ ))
				(error nil)))))
	(if is-block
	    (progn
	      (goto-char post-A)
	      (forward-sexp 1)
	      (setq post-B (point))
	      (forward-sexp -1)
	      (setq pre-B (point))
	      (if (and (eq (following-char) ?\{ )
		       (progn
			 (cperl-backward-to-noncomment post-A)
			 (eq (preceding-char) ?\) )))
		  (if (condition-case nil
			  (progn
			    (goto-char post-B)
			    (forward-sexp 1)
			    (forward-sexp -1)
			    (looking-at "\\<els\\(e\\|if\\)\\>"))
			(error nil))
		      (error
		       "`%s' (EXPR) {BLOCK} with `else'/`elsif'" if-string)
		    (goto-char (1- post-B))
		    (cperl-backward-to-noncomment pre-B)
		    (if (eq (preceding-char) ?\;)
			(forward-char -1))
		    (setq end-B-code (point))
		    (goto-char pre-B)
		    (while (re-search-forward "\\<\\(for\\|foreach\\|if\\|unless\\|while\\|until\\)\\>\\|;" end-B-code t)
		      (setq p (match-beginning 0)
			    A (buffer-substring p (match-end 0))
			    state (parse-partial-sexp pre-B p))
		      (or (nth 3 state)
			  (nth 4 state)
			  (nth 5 state)
			  (error "`%s' inside `%s' BLOCK" A if-string))
		      (goto-char (match-end 0)))
		    ;; Finally got it
		    (goto-char (1+ pre-B))
		    (skip-chars-forward " \t\n")
		    (setq B (buffer-substring (point) end-B-code))
		    (goto-char end-B-code)
		    (or (looking-at ";?[ \t\n]*}")
			(progn
			  (skip-chars-forward "; \t\n")
			  (setq B-comment
				(buffer-substring (point) (1- post-B)))))
		    (and (equal B "")
			 (setq B "1"))
		    (goto-char (1- post-A))
		    (cperl-backward-to-noncomment pre-A)
		    (or (looking-at "[ \t\n]*)")
			(goto-char (1- post-A)))
		    (setq p (point))
		    (goto-char (1+ pre-A))
		    (skip-chars-forward " \t\n")
		    (setq A (buffer-substring (point) p))
		    (delete-region pre-B post-B)
		    (delete-region pre-A post-A)
		    (goto-char pre-if)
		    (insert B " ")
		    (and B-comment (insert B-comment " "))
		    (just-one-space)
		    (forward-word 1)
		    (setq pre-A (point))
		    (insert " " A ";")
		    (delete-horizontal-space)
		    (setq post-B (point))
		    (if (looking-at "#")
			(indent-for-comment))
		    (goto-char post-B)
		    (forward-char -1)
		    (delete-horizontal-space)
		    (goto-char pre-A)
		    (just-one-space)
		    (goto-char pre-if)
		    (setq pre-A (set-marker (make-marker) pre-A))
		    (while (<= (point) (marker-position pre-A))
		      (cperl-indent-line)
		      (forward-line 1))
		    (goto-char (marker-position pre-A))
		    (if B-comment
			(progn
			  (forward-line -1)
			  (indent-for-comment)
			  (goto-char (marker-position pre-A)))))
		(error "`%s' (EXPR) not with an {BLOCK}" if-string)))
	  ;; (error "`%s' not with an (EXPR)" if-string)
	  (forward-sexp -1)
	  (cperl-invert-if-unless-modifiers)))
    ;;(error "Not at `if', `unless', `while', `until', `for' or `foreach'")
    (cperl-invert-if-unless-modifiers)))

;;; By Anthony Foiani <afoiani@uswest.com>
;;; Getting help on modules in C-h f ?
;;; This is a modified version of `man'.
;;; Need to teach it how to lookup functions
;;;###autoload
(defun cperl-perldoc (word)
  "Run `perldoc' on WORD."
  (interactive
   (list (let* ((default-entry (cperl-word-at-point))
                (input (read-string
                        (format "perldoc entry%s: "
                                (if (string= default-entry "")
                                    ""
                                  (format " (default %s)" default-entry))))))
           (if (string= input "")
               (if (string= default-entry "")
                   (error "No perldoc args given")
                 default-entry)
             input))))
  (require 'man)
  (let* ((case-fold-search nil)
	 (is-func (and
		   (string-match "^[a-z]+$" word)
		   (string-match (concat "^" word "\\>")
				 (documentation-property
				  'cperl-short-docs
				  'variable-documentation))))
	 (manual-program (if is-func "perldoc -f" "perldoc")))
    (cond
     (cperl-xemacs-p
      (let ((Manual-program "perldoc")
	    (Manual-switches (if is-func (list "-f"))))
	(manual-entry word)))
     (t
      (Man-getpage-in-background word)))))

;;;###autoload
(defun cperl-perldoc-at-point ()
  "Run a `perldoc' on the word around point."
  (interactive)
  (cperl-perldoc (cperl-word-at-point)))

(defcustom pod2man-program "pod2man"
  "*File name for `pod2man'."
  :type 'file
  :group 'cperl)

;;; By Nick Roberts <Nick.Roberts@src.bae.co.uk> (with changes)
(defun cperl-pod-to-manpage ()
  "Create a virtual manpage in Emacs from the Perl Online Documentation."
  (interactive)
  (require 'man)
  (let* ((pod2man-args (concat buffer-file-name " | nroff -man "))
	 (bufname (concat "Man " buffer-file-name))
	 (buffer (generate-new-buffer bufname)))
    (save-excursion
      (set-buffer buffer)
      (let ((process-environment (copy-sequence process-environment)))
        ;; Prevent any attempt to use display terminal fanciness.
        (setenv "TERM" "dumb")
        (set-process-sentinel
         (start-process pod2man-program buffer "sh" "-c"
                        (format (cperl-pod2man-build-command) pod2man-args))
         'Man-bgproc-sentinel)))))

;;; Updated version by him too
(defun cperl-build-manpage ()
  "Create a virtual manpage in Emacs from the POD in the file."
  (interactive)
  (require 'man)
  (cond
   (cperl-xemacs-p
    (let ((Manual-program "perldoc"))
      (manual-entry buffer-file-name)))
   (t
    (let* ((manual-program "perldoc"))
      (Man-getpage-in-background buffer-file-name)))))

(defun cperl-pod2man-build-command ()
  "Builds the entire background manpage and cleaning command."
  (let ((command (concat pod2man-program " %s 2>/dev/null"))
        (flist (and (boundp 'Man-filter-list) Man-filter-list)))
    (while (and flist (car flist))
      (let ((pcom (car (car flist)))
            (pargs (cdr (car flist))))
        (setq command
              (concat command " | " pcom " "
                      (mapconcat '(lambda (phrase)
                                    (if (not (stringp phrase))
                                        (error "Malformed Man-filter-list"))
                                    phrase)
                                 pargs " ")))
        (setq flist (cdr flist))))
    command))


(defun cperl-next-interpolated-REx-1 ()
  "Move point to next REx which has interpolated parts without //o.
Skips RExes consisting of one interpolated variable.

Note that skipped RExen are not performance hits."
  (interactive "")
  (cperl-next-interpolated-REx 1))

(defun cperl-next-interpolated-REx-0 ()
  "Move point to next REx which has interpolated parts without //o."
  (interactive "")
  (cperl-next-interpolated-REx 0))

(defun cperl-next-interpolated-REx (&optional skip beg limit)
  "Move point to next REx which has interpolated parts.
SKIP is a list of possible types to skip, BEG and LIMIT are the starting
point and the limit of search (default to point and end of buffer).

SKIP may be a number, then it behaves as list of numbers up to SKIP; this
semantic may be used as a numeric argument.

Types are 0 for / $rex /o (interpolated once), 1 for /$rex/ (if $rex is
a result of qr//, this is not a performance hit), t for the rest."
  (interactive "P")
  (if (numberp skip) (setq skip (list 0 skip)))
  (or beg (setq beg (point)))
  (or limit (setq limit (point-max)))	; needed for n-s-p-c
  (let (pp)
    (and (eq (get-text-property beg 'syntax-type) 'string)
	 (setq beg (next-single-property-change beg 'syntax-type nil limit)))
    (cperl-map-pods-heres
     (function (lambda (s e p)
		 (if (memq (get-text-property s 'REx-interpolated) skip)
		     t
		   (setq pp s)
		   nil)))	; nil stops
     'REx-interpolated beg limit)
    (if pp (goto-char pp)
      (message "No more interpolated REx"))))

;;; Initial version contributed by Trey Belew
(defun cperl-here-doc-spell (&optional beg end)
  "Spell-check HERE-documents in the Perl buffer.
If a region is highlighted, restricts to the region."
  (interactive "")
  (cperl-pod-spell t beg end))

(defun cperl-pod-spell (&optional do-heres beg end)
  "Spell-check POD documentation.
If invoked with prefix argument, will do HERE-DOCs instead.
If a region is highlighted, restricts to the region."
  (interactive "P")
  (save-excursion
    (let (beg end)
      (if (cperl-mark-active)
	  (setq beg (min (mark) (point))
		end (max (mark) (point)))
	(setq beg (point-min)
	      end (point-max)))
      (cperl-map-pods-heres (function
			     (lambda (s e p)
			       (if do-heres
				   (setq e (save-excursion
					     (goto-char e)
					     (forward-line -1)
					     (point))))
			       (ispell-region s e)
			       t))
			    (if do-heres 'here-doc-group 'in-pod)
			    beg end))))

(defun cperl-map-pods-heres (func &optional prop s end)
  "Executes a function over regions of pods or here-documents.
PROP is the text-property to search for; default to `in-pod'.  Stop when
function returns nil."
  (let (pos posend has-prop (cont t))
    (or prop (setq prop 'in-pod))
    (or s (setq s (point-min)))
    (or end (setq end (point-max)))
    (cperl-update-syntaxification end end)
    (save-excursion
      (goto-char (setq pos s))
      (while (and cont (< pos end))
	(setq has-prop (get-text-property pos prop))
	(setq posend (next-single-property-change pos prop nil end))
	(and has-prop
	     (setq cont (funcall func pos posend prop)))
	(setq pos posend)))))

;;; Based on code by Masatake YAMATO:
(defun cperl-get-here-doc-region (&optional pos pod)
  "Return HERE document region around the point.
Return nil if the point is not in a HERE document region.  If POD is non-nil,
will return a POD section if point is in a POD section."
  (or pos (setq pos (point)))
  (cperl-update-syntaxification pos pos)
  (if (or (eq 'here-doc  (get-text-property pos 'syntax-type))
	  (and pod
	       (eq 'pod (get-text-property pos 'syntax-type))))
      (let ((b (cperl-beginning-of-property pos 'syntax-type))
	    (e (next-single-property-change pos 'syntax-type)))
	(cons b (or e (point-max))))))

(defun cperl-narrow-to-here-doc (&optional pos)
  "Narrows editing region to the HERE-DOC at POS.
POS defaults to the point."
  (interactive "d")
  (or pos (setq pos (point)))
  (let ((p (cperl-get-here-doc-region pos)))
    (or p (error "Not inside a HERE document"))
    (narrow-to-region (car p) (cdr p))
    (message
     "When you are finished with narrow editing, type C-x n w")))

(defun cperl-select-this-pod-or-here-doc (&optional pos)
  "Select the HERE-DOC (or POD section) at POS.
POS defaults to the point."
  (interactive "d")
  (let ((p (cperl-get-here-doc-region pos t)))
    (if p
	(progn
	  (goto-char (car p))
	  (push-mark (cdr p) nil t))	; Message, activate in transient-mode
      (message "I do not think POS is in POD or a HERE-doc..."))))

(defun cperl-facemenu-add-face-function (face end)
  "A callback to process user-initiated font-change requests.
Translates `bold', `italic', and `bold-italic' requests to insertion of
corresponding POD directives, and `underline' to C<> POD directive.

Such requests are usually bound to M-o LETTER."
  (or (get-text-property (point) 'in-pod)
      (error "Faces can only be set within POD"))
  (setq facemenu-end-add-face (if (eq face 'bold-italic) ">>" ">"))
  (cdr (or (assq face '((bold . "B<")
			(italic . "I<")
			(bold-italic . "B<I<")
			(underline . "C<")))
	   (error "Face %s not configured for cperl-mode"
		  face))))

(defun cperl-time-fontification (&optional l step lim)
  "Times how long it takes to do incremental fontification in a region.
L is the line to start at, STEP is the number of lines to skip when
doing next incremental fontification, LIM is the maximal number of
incremental fontification to perform.  Messages are accumulated in
*Messages* buffer.

May be used for pinpointing which construct slows down buffer fontification:
start with default arguments, then refine the slowdown regions."
  (interactive "nLine to start at: \nnStep to do incremental fontification: ")
  (or l (setq l 1))
  (or step (setq step 500))
  (or lim (setq lim 40))
  (let* ((timems (function (lambda ()
			     (let ((tt (current-time)))
			       (+ (* 1000 (nth 1 tt)) (/ (nth 2 tt) 1000))))))
	 (tt (funcall timems)) (c 0) delta tot)
    (goto-line l)
    (cperl-mode)
    (setq tot (- (- tt (setq tt (funcall timems)))))
    (message "cperl-mode at %s: %s" l tot)
    (while (and (< c lim) (not (eobp)))
      (forward-line step)
      (setq l (+ l step))
      (setq c (1+ c))
      (cperl-update-syntaxification (point) (point))
      (setq delta (- (- tt (setq tt (funcall timems)))) tot (+ tot delta))
      (message "to %s:%6s,%7s" l delta tot))
    tot))

(defun cperl-emulate-lazy-lock (&optional window-size)
  "Emulate `lazy-lock' without `condition-case', so `debug-on-error' works.
Start fontifying the buffer from the start (or end) using the given
WINDOW-SIZE (units is lines).  Negative WINDOW-SIZE starts at end, and
goes backwards; default is -50.  This function is not CPerl-specific; it
may be used to debug problems with delayed incremental fontification."
  (interactive
   "nSize of window for incremental fontification, negative goes backwards: ")
  (or window-size (setq window-size -50))
  (let ((pos (if (> window-size 0)
		 (point-min)
	       (point-max)))
	p)
    (goto-char pos)
    (normal-mode)
    ;; Why needed???  With older font-locks???
    (set (make-local-variable 'font-lock-cache-position) (make-marker))
    (while (if (> window-size 0)
	       (< pos (point-max))
	     (> pos (point-min)))
      (setq p (progn
		(forward-line window-size)
		(point)))
      (font-lock-fontify-region (min p pos) (max p pos))
      (setq pos p))))


(defun cperl-lazy-install ())		; Avoid a warning
(defun cperl-lazy-unstall ())		; Avoid a warning

(if (fboundp 'run-with-idle-timer)
    (progn
      (defvar cperl-help-shown nil
	"Non-nil means that the help was already shown now.")

      (defvar cperl-lazy-installed nil
	"Non-nil means that the lazy-help handlers are installed now.")

      (defun cperl-lazy-install ()
	"Switches on Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
	(interactive)
	(make-variable-buffer-local 'cperl-help-shown)
	(if (and (cperl-val 'cperl-lazy-help-time)
		 (not cperl-lazy-installed))
	    (progn
	      (add-hook 'post-command-hook 'cperl-lazy-hook)
	      (run-with-idle-timer
	       (cperl-val 'cperl-lazy-help-time 1000000 5)
	       t
	       'cperl-get-help-defer)
	      (setq cperl-lazy-installed t))))

      (defun cperl-lazy-unstall ()
	"Switches off Auto-Help on Perl constructs (put in the message area).
Delay of auto-help controlled by `cperl-lazy-help-time'."
	(interactive)
	(remove-hook 'post-command-hook 'cperl-lazy-hook)
	(cancel-function-timers 'cperl-get-help-defer)
	(setq cperl-lazy-installed nil))

      (defun cperl-lazy-hook ()
	(setq cperl-help-shown nil))

      (defun cperl-get-help-defer ()
	(if (not (memq major-mode '(perl-mode cperl-mode))) nil
	  (let ((cperl-message-on-help-error nil) (cperl-help-from-timer t))
	    (cperl-get-help)
	    (setq cperl-help-shown t))))
      (cperl-lazy-install)))


;;; Plug for wrong font-lock:

(defun cperl-font-lock-unfontify-region-function (beg end)
  (let* ((modified (buffer-modified-p)) (buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 before-change-functions after-change-functions
	 deactivate-mark buffer-file-name buffer-file-truename)
    (remove-text-properties beg end '(face nil))
    (if (and (not modified) (buffer-modified-p))
      (set-buffer-modified-p nil))))

(defun cperl-font-lock-fontify-region-function (beg end loudly)
  "Extends the region to safe positions, then calls the default function.
Newer `font-lock's can do it themselves.
We unwind only as far as needed for fontification.  Syntaxification may
do extra unwind via `cperl-unwind-to-safe'."
  (save-excursion
    (goto-char beg)
    (while (and beg
		(progn
		  (beginning-of-line)
		  (eq (get-text-property (setq beg (point)) 'syntax-type)
		      'multiline)))
      (if (setq beg (cperl-beginning-of-property beg 'syntax-type))
	  (goto-char beg)))
    (setq beg (point))
    (goto-char end)
    (while (and end
		(progn
		  (or (bolp) (condition-case nil
				 (forward-line 1)
			       (error nil)))
		  (eq (get-text-property (setq end (point)) 'syntax-type)
		      'multiline)))
      (setq end (next-single-property-change end 'syntax-type nil (point-max)))
      (goto-char end))
    (setq end (point)))
  (font-lock-default-fontify-region beg end loudly))

(defvar cperl-d-l nil)
(defun cperl-fontify-syntaxically (end)
  ;; Some vars for debugging only
  ;; (message "Syntaxifying...")
  (let ((dbg (point)) (iend end) (idone cperl-syntax-done-to)
	(istate (car cperl-syntax-state))
	start from-start edebug-backtrace-buffer)
    (if (eq cperl-syntaxify-by-font-lock 'backtrace)
	(progn
	  (require 'edebug)
	  (let ((f 'edebug-backtrace))
	    (funcall f))))	; Avoid compile-time warning
    (or cperl-syntax-done-to
	(setq cperl-syntax-done-to (point-min)
	      from-start t))
    (setq start (if (and cperl-hook-after-change
			 (not from-start))
		    cperl-syntax-done-to ; Fontify without change; ignore start
		  ;; Need to forget what is after `start'
		  (min cperl-syntax-done-to (point))))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (and cperl-syntaxify-unwind
	 (setq end (cperl-unwind-to-safe t end)
	       start (point)))
    (and (> end start)
	 (setq cperl-syntax-done-to start) ; In case what follows fails
	 (cperl-find-pods-heres start end t nil t))
    (if (memq cperl-syntaxify-by-font-lock '(backtrace message))
	(message "Syxify req=%s..%s actual=%s..%s done-to: %s=>%s statepos: %s=>%s"
		 dbg iend start end idone cperl-syntax-done-to
		 istate (car cperl-syntax-state))) ; For debugging
    nil))				; Do not iterate

(defun cperl-fontify-update (end)
  (let ((pos (point-min)) prop posend)
    (setq end (point-max))
    (while (< pos end)
      (setq prop (get-text-property pos 'cperl-postpone)
	    posend (next-single-property-change pos 'cperl-postpone nil end))
      (and prop (put-text-property pos posend (car prop) (cdr prop)))
      (setq pos posend)))
  nil)					; Do not iterate

(defun cperl-fontify-update-bad (end)
  ;; Since fontification happens with different region than syntaxification,
  ;; do to the end of buffer, not to END;;; likewise, start earlier if needed
  (let* ((pos (point)) (prop (get-text-property pos 'cperl-postpone)) posend)
    (if prop
	(setq pos (or (cperl-beginning-of-property
		       (cperl-1+ pos) 'cperl-postpone)
		      (point-min))))
    (while (< pos end)
      (setq posend (next-single-property-change pos 'cperl-postpone))
      (and prop (put-text-property pos posend (car prop) (cdr prop)))
      (setq pos posend)
      (setq prop (get-text-property pos 'cperl-postpone))))
  nil)					; Do not iterate

;; Called when any modification is made to buffer text.
(defun cperl-after-change-function (beg end old-len)
  ;; We should have been informed about changes by `font-lock'.  Since it
  ;; does not inform as which calls are defered, do it ourselves
  (if cperl-syntax-done-to
      (setq cperl-syntax-done-to (min cperl-syntax-done-to beg))))

(defun cperl-update-syntaxification (from to)
  (if (and cperl-use-syntax-table-text-property
	   cperl-syntaxify-by-font-lock
	   (or (null cperl-syntax-done-to)
	       (< cperl-syntax-done-to to)))
      (progn
	(save-excursion
	  (goto-char from)
	  (cperl-fontify-syntaxically to)))))

(defvar cperl-version
  (let ((v  "$Revision: 5.23 $"))
    (string-match ":\\s *\\([0-9.]+\\)" v)
    (substring v (match-beginning 1) (match-end 1)))
  "Version of IZ-supported CPerl package this file is based on.")

(provide 'cperl-mode)

;;; cperl-mode.el ends here
