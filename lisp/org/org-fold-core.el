;;; org-fold-core.el --- Folding buffer text -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2024 Free Software Foundation, Inc.
;;
;; Author: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: folding, invisible text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains library to control temporary invisibility
;; (folding and unfolding) of text in buffers.

;; The file implements the following functionality:
;;
;; - Folding/unfolding regions of text
;; - Searching and examining boundaries of folded text
;; - Interactive searching in folded text (via isearch)
;; - Handling edits in folded text
;; - Killing/yanking (copying/pasting) of the folded text

;; To setup folding in an arbitrary buffer, one must call
;; `org-fold-core-initialize', optionally providing the list of folding specs to be
;; used in the buffer.  The specs can be added, removed, or
;; re-configured later.  Read below for more details.

;;; Folding/unfolding regions of text

;; User can temporarily hide/reveal (fold/unfold) arbitrary regions or
;; text.  The folds can be nested.

;; Internally, nested folds are marked with different folding specs
;; Overlapping folds marked with the same folding spec are
;; automatically merged, while folds with different folding specs can
;; coexist and be folded/unfolded independently.

;; When multiple folding specs are applied to the same region of text,
;; text visibility is decided according to the folding spec with
;; topmost priority.

;; By default, we define two types of folding specs:
;; - 'org-fold-visible :: the folded text is not hidden
;; - 'org-fold-hidden  :: the folded text is completely hidden
;;
;; The 'org-fold-visible spec has highest priority allowing parts of
;; text folded with 'org-fold-hidden to be shown unconditionally.

;; Consider the following Org mode link:
;; [[file:/path/to/file/file.ext][description]]
;; Only the word "description" is normally visible in this link.
;;
;; The way this partial visibility is achieved is combining the two
;; folding specs.  The whole link is folded using 'org-fold-hidden
;; folding spec, but the visible part is additionally folded using
;; 'org-fold-visible:
;;
;; <begin org-fold-hidden>[[file:/path/to/file/file.ext][<begin org-fold-visible>description<end org-fold-visible>]]<end org-fold-hidden>
;;
;; Because 'org-fold-visible has higher priority than
;; 'org-fold-hidden, it suppresses the 'org-fold-hidden effect and
;; thus reveals the description part of the link.

;; Similar to 'org-fold-visible, display of any arbitrary folding spec
;; can be configured using folding spec properties.  In particular,
;; `:visible' folding spec property controls whether the folded text
;; is visible or not.  If the `:visible' folding spec property is nil,
;; folded text is hidden or displayed as a constant string (ellipsis)
;; according to the value of `:ellipsis' folding spec property.  See
;; docstring of `org-fold-core--specs' for the description of all the available
;; folding spec properties.

;; Folding spec properties of any valid folding spec can be changed
;; any time using `org-fold-core-set-folding-spec-property'.

;; If necessary, one can add or remove folding specs using
;; `org-fold-core-add-folding-spec' and `org-fold-core-remove-folding-spec'.

;; If a buffer initialized with `org-fold-core-initialize' is cloned into indirect
;; buffers, it's folding state is copied to that indirect buffer.
;; The folding states are independent.

;; When working with indirect buffers that are handled by this
;; library, one has to keep in mind that folding state is preserved on
;; copy when using non-interactive functions.  Moreover, the folding
;; states of all the indirect buffers will be copied together.
;;
;; Example of the implications:
;; Consider a base buffer and indirect buffer with the following state:
;; ----- base buffer --------
;; * Heading<begin fold>
;; Some text folded in the base buffer, but unfolded in the indirect buffer<end fold>
;; * Other heading
;; Heading unfolded in both the buffers.
;; ---------------------------
;; ------ indirect buffer ----
;; * Heading
;; Some text folded in the base buffer, but unfolded in the indirect buffer
;; * Other heading
;; Heading unfolded in both the buffers.
;; ----------------------------
;; If some Elisp code copies the whole "Heading" from the indirect
;; buffer with `buffer-substring' or match data and inserts it into
;; the base buffer, the inserted heading will be folded since the
;; internal setting for the folding state is shared between the base
;; and indirect buffers.  It's just that the indirect buffer ignores
;; the base buffer folding settings.  However, as soon as the text is
;; copied back to the base buffer, the folding state will become
;; respected again.

;; If the described situation is undesired, Elisp code can use
;; `filter-buffer-substring' instead of `buffer-substring'.  All the
;; folding states that do not belong to the currently active buffer
;; will be cleared in the copied text then.  See
;; `org-fold-core--buffer-substring-filter' for more details.

;; Because of details of implementation of the folding, it is also not
;; recommended to set text visibility in buffer directly by setting
;; `invisible' text property to anything other than t.  While this
;; should usually work just fine, normal folding can be broken if one
;; sets `invisible' text property to a value not listed in
;; `buffer-invisibility-spec'.

;;; Searching and examining boundaries of folded text

;; It is possible to examine folding specs (there may be several) of
;; text at point or search for regions with the same folding spec.
;; See functions defined under ";;;; Searching and examining folded
;; text" below for details.

;; All the folding specs can be specified by symbol representing their
;; name.  However, this is not always convenient, especially if the
;; same spec can be used for fold different syntactical structures.
;; Any folding spec can be additionally referenced by a symbol listed
;; in the spec's `:alias' folding spec property.  For example, Org
;; mode's `org-fold-outline' folding spec can be referenced as any
;; symbol from the following list: '(headline heading outline
;; inlinetask plain-list) The list is the value of the spec's `:alias'
;; property.

;; Most of the functions defined below that require a folding spec
;; symbol as their argument, can also accept any symbol from the
;; `:alias' spec property to reference that folding spec.

;; If one wants to search invisible text without using the provided
;; functions, it is important to keep in mind that 'invisible text
;; property may have multiple possible values (not just nil and
;; t). Hence, (next-single-char-property-change pos 'invisible) is not
;; guaranteed to return the boundary of invisible/visible text.

;;; Interactive searching inside folded text (via isearch)

;; The library provides a way to control if the folded text can be
;; searchable using isearch.  If the text is searchable, it is also
;; possible to control to unfold it temporarily during interactive
;; isearch session.

;; The isearch behavior is controlled on per-folding-spec basis by
;; setting `isearch-open' and `isearch-ignore' folding spec
;; properties.  The the docstring of `org-fold-core--specs' for more details.

;;; Handling edits inside folded text

;; The visibility of the text inserted in front, rear, or in the
;; middle of a folded region is managed according to `:front-sticky'
;; and `:rear-sticky' folding properties of the corresponding folding
;; spec.  The rules are the same with stickiness of text properties in
;; Elisp.

;; If a text being inserted into the buffer is already folded and
;; invisible (before applying the stickiness rules), then it is
;; revealed.  This behavior can be changed by wrapping the insertion
;; code into `org-fold-core-ignore-modifications' macro.  The macro will disable
;; all the processing related to buffer modifications.

;; The library also provides a way to unfold the text after some
;; destructive changes breaking syntactical structure of the buffer.
;; For example, Org mode automatically reveals folded drawers when the
;; drawer becomes syntactically incorrect:
;; ------- before modification -------
;; :DRAWER:<begin fold>
;; Some folded text inside drawer
;; :END:<end fold>
;; -----------------------------------
;; If the ":END:" is edited, drawer syntax is not correct anymore and
;; the folded text is automatically unfolded.
;; ------- after modification --------
;; :DRAWER:
;; Some folded text inside drawer
;; :EN:
;; -----------------------------------

;; The described automatic unfolding is controlled by `:fragile'
;; folding spec property.  It's value can be a function checking if
;; changes inside (or around) the fold should drigger the unfold.  By
;; default, only changes that directly involve folded regions will
;; trigger the check.  In addition, `org-fold-core-extend-changed-region-functions'
;; can be set to extend the checks to all folded regions intersecting
;; with the region returned by the functions listed in the variable.

;; The fragility checks can be bypassed if the code doing
;; modifications is wrapped into `org-fold-core-ignore-fragility-checks' macro.

;;; Performance considerations

;; This library is using text properties to hide text.  Text
;; properties are much faster than overlays, that could be used for
;; the same purpose.  Overlays are implemented with O(n) complexity in
;; Emacs (as for 2021-03-11).  It means that any attempt to move
;; through hidden text in a file with many invisible overlays will
;; require time scaling with the number of folded regions (the problem
;; Overlays note of the manual warns about).  For curious, historical
;; reasons why overlays are not efficient can be found in
;; https://www.jwz.org/doc/lemacs.html.

;; Despite using text properties, the performance is still limited by
;; Emacs display engine.  For example, >7Mb of text hidden within
;; visible part of a buffer may cause noticeable lags (which is still
;; orders of magnitude better in comparison with overlays).  If the
;; performance issues become critical while using this library, it is
;; recommended to minimize the number of folding specs used in the
;; same buffer at a time.

;; Alternatively, the library provides `org-fold-core--optimise-for-huge-buffers'
;; for additional speedup.  This can be used as a file-local variable
;; in huge buffers.  The variable can be set to enable various levels
;; of extra optimization.  See the docstring for detailed information.

;; It is worth noting that when using `org-fold-core--optimise-for-huge-buffers'
;; with `grab-invisible' option, folded regions copied to other
;; buffers (including buffers that do not use this library) will
;; remain invisible.  org-fold-core provides functions to work around
;; this issue: `org-fold-core-remove-optimisation' and `org-fold-core-update-optimisation', but
;; it is unlikely that a random external package will use them.

;; Another possible bottleneck is the fragility check after the change
;; related to the folded text.  The functions used in `:fragile'
;; folding properties must be optimized.  Also,
;; `org-fold-core-ignore-fragility-checks' or even `org-fold-core-ignore-modifications' may be
;; used when appropriate in the performance-critical code.  When
;; inserting text from within `org-fold-core-ignore-modifications' macro, it is
;; recommended to use `insert-and-inherit' instead of `insert' and
;; `insert-before-markers-and-inherit' instead of
;; `insert-before-markers' to avoid revealing inserted text in the
;; middle of a folded region.

;; Performance of isearch is currently limited by Emacs isearch
;; implementation.  For now, Emacs isearch only supports searching
;; through text hidden using overlays.  This library handles isearch
;; by converting folds with matching text to overlays, which may
;; affect performance in case of large number of matches.  In the
;; future, Emacs will hopefully accept the relevant patch allowing
;; isearch to work with text hidden via text properties, but the
;; performance hit has to be accepted meanwhile.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-compat)

(declare-function isearch-filter-visible "isearch" (beg end))

;;; Customization

(defcustom org-fold-core-style (if (version< emacs-version "29")
                                   'text-properties
                                 'overlays)
  "Internal implementation detail used to hide folded text.
Can be either `text-properties' or `overlays'.
The former is faster on large files in Emacs <29, while the latter is
generally less error-prone with regard to third-party packages.

Important: This variable must be set before loading Org."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Overlays" overlays)
          (const :tag "Text properties" text-properties)))

(defvar-local org-fold-core-isearch-open-function #'org-fold-core--isearch-reveal
  "Function used to reveal hidden text found by isearch.
The function is called with a single argument - point where text is to
be revealed.")

(defvar-local org-fold-core--optimise-for-huge-buffers nil
  "Non-nil turns on extra speedup on huge buffers (Mbs of folded text).

This setting is risky and may cause various artifacts and degraded
functionality, especially when using external packages.  It is
recommended to enable it on per-buffer basis as file-local variable.

When set to non-nil, must be a list containing one or multiple the
following symbols:

- `grab-invisible': Use `invisible' text property to hide text.  This
  will reduce the load on Emacs display engine and one may use it if
  moving point across folded regions becomes slow.  However, as a side
  effect, some external packages extracting i.e. headlings from folded
  parts of buffer may keep the text invisible.

- `ignore-fragility-checks': Do not try to detect when user edits
  break structure of the folded elements.  This will speed up
  modifying the folded regions at the cost that some higher-level
  functions relying on this package might not be able to unfold the
  edited text.  For example, removed leading stars from a folded
  headline in Org mode will break visibility cycling since Org mode
  will not be aware that the following folded text belonged to
  headline.

- `ignore-modification-checks': Do not try to detect insertions in the
  middle of the folded regions.  This will speed up non-interactive
  edits of the folded regions.  However, text inserted in the middle
  of the folded regions may become visible for some external packages
  inserting text using `insert' instead of `insert-and-inherit' (the
  latter is rarely used in practice).

- `ignore-indirect': Do not decouple folding state in the indirect
  buffers.  This can speed up Emacs display engine (and thus motion of
  point), especially when large number of indirect buffers is being
  used.

- `merge-folds': Do not distinguish between different types of folding
  specs.  This is the most aggressive optimization with unforeseen and
  potentially drastic effects.")
(put 'org-fold-core--optimise-for-huge-buffers 'safe-local-variable 'listp)

;;; Core functionality

;;;; Folding specs

(defvar-local org-fold-core--specs '((org-fold-visible
	                 (:visible . t)
                         (:alias . (visible)))
                        (org-fold-hidden
			 (:ellipsis . "...")
                         (:isearch-open . t)
                         (:alias . (hidden))))
  "Folding specs defined in current buffer.

Each spec is a list (SPEC-SYMBOL SPEC-PROPERTIES).
SPEC-SYMBOL is the symbol representing the folding spec.
SPEC-PROPERTIES is an alist defining folding spec properties.

If a text region is folded using multiple specs, only the folding spec
listed earlier is used.

The following properties are known:
- :ellipsis         :: must be nil or string to show when text is folded
                      using this spec.
- :global           :: non-nil means that folding state will be preserved
                      when copying folded text between buffers.
- :isearch-ignore   :: non-nil means that folded text is not searchable
                      using isearch.
- :isearch-open     :: non-nil means that isearch can reveal text hidden
                      using this spec.  This property does nothing
                      when `isearch-ignore' property is non-nil.
- :front-sticky     :: non-nil means that text prepended to the folded text
                      is automatically folded.
- :rear-sticky      :: non-nil means that text appended to the folded text
                      is folded.
- :visible          :: non-nil means that folding spec visibility is not
                       managed.  Instead, visibility settings in
                       `buffer-invisibility-spec' will be used as is.
                       Note that changing this property from nil to t may
                       clear the setting in `buffer-invisibility-spec'.
- :font-lock        :: non-nil means that newlines after the fold should
                       be re-fontified upon folding/unfolding.  See
                       `org-activate-folds'.
- :alias            :: a list of aliases for the SPEC-SYMBOL.
- :fragile          :: Must be a function accepting two arguments.
                       Non-nil means that changes in region may cause
                       the region to be revealed.  The region is
                       revealed after changes if the function returns
                       non-nil.
                       The function called after changes are made with
                       two arguments: cons (beg . end) representing the
                       folded region and spec symbol.")
(defvar-local org-fold-core--spec-symbols nil
  "Alist holding buffer spec symbols and aliases.

This variable is defined to reduce load on Emacs garbage collector
reducing the number of transiently allocated variables.")
(defvar-local org-fold-core--spec-list nil
  "List holding buffer spec symbols, but not aliases.

This variable is defined to reduce load on Emacs garbage collector
reducing the number of transiently allocated variables.")

(defvar-local org-fold-core-extend-changed-region-functions nil
  "Special hook run just before handling changes in buffer.

This is used to account changes outside folded regions that still
affect the folded region visibility.  For example, removing all stars
at the beginning of a folded Org mode heading should trigger the
folded text to be revealed.  Each function is called with two
arguments: beginning and the end of the changed region.")

;;; Utility functions

(defsubst org-fold-core-folding-spec-list (&optional buffer)
  "Return list of all the folding spec symbols in BUFFER."
  (or (buffer-local-value 'org-fold-core--spec-list (or buffer (current-buffer)))
      (with-current-buffer (or buffer (current-buffer))
        (setq org-fold-core--spec-list (mapcar #'car org-fold-core--specs)))))

(defun org-fold-core-get-folding-spec-from-alias (spec-or-alias)
  "Return the folding spec symbol for SPEC-OR-ALIAS.
Return nil when there is no matching folding spec."
  (when spec-or-alias
    (unless org-fold-core--spec-symbols
      (dolist (spec (org-fold-core-folding-spec-list))
        (push (cons spec spec) org-fold-core--spec-symbols)
        (dolist (alias (cdr (assq :alias (assq spec org-fold-core--specs))))
          (push (cons alias spec) org-fold-core--spec-symbols))))
    (alist-get spec-or-alias org-fold-core--spec-symbols)))

(defsubst org-fold-core-folding-spec-p (spec-or-alias)
  "Check if SPEC-OR-ALIAS is a registered folding spec."
  (org-fold-core-get-folding-spec-from-alias spec-or-alias))

(defsubst org-fold-core--check-spec (spec-or-alias)
  "Throw an error if SPEC-OR-ALIAS is not in `org-fold-core-folding-spec-list'."
  (unless (org-fold-core-folding-spec-p spec-or-alias)
    (error "%s is not a valid folding spec" spec-or-alias)))

(defsubst org-fold-core-get-folding-spec-property (spec-or-alias property)
  "Get PROPERTY of a folding SPEC-OR-ALIAS.
Possible properties can be found in `org-fold-core--specs' docstring."
  (org-fold-core--check-spec spec-or-alias)
  (if (and (memql 'ignore-indirect org-fold-core--optimise-for-huge-buffers)
           (eq property :global))
      t
    (if (and (memql 'merge-folds org-fold-core--optimise-for-huge-buffers)
             (eq property :visible))
        nil
      (cdr (assq property (assq (org-fold-core-get-folding-spec-from-alias spec-or-alias) org-fold-core--specs))))))

(defconst org-fold-core--spec-property-prefix "org-fold--spec-"
  "Prefix used to create property symbol.")

(defsubst org-fold-core-get-folding-property-symbol (spec &optional buffer global)
  "Get folding text property using to store SPEC in current buffer or BUFFER.
If GLOBAL is non-nil, do not make the property unique in the BUFFER."
  (if (memql 'merge-folds org-fold-core--optimise-for-huge-buffers)
      (intern (format "%s-global" org-fold-core--spec-property-prefix))
    (intern (format (concat org-fold-core--spec-property-prefix "%s-%S")
                    (symbol-name spec)
                    ;; (sxhash buf) appears to be not constant over time.
                    ;; Using buffer-name is safe, since the only place where
                    ;; buffer-local text property actually matters is an indirect
                    ;; buffer, where the name cannot be same anyway.
                    (if (or global
                            (memql 'ignore-indirect org-fold-core--optimise-for-huge-buffers))
                        'global
                      (sxhash (buffer-name (or buffer (current-buffer)))))))))

(defsubst org-fold-core-get-folding-spec-from-folding-prop (folding-prop)
  "Return folding spec symbol used for folding property with name FOLDING-PROP."
  (catch :exit
    (dolist (spec (org-fold-core-folding-spec-list))
      ;; We know that folding properties have
      ;; folding spec in their name.
      (when (string-match-p (symbol-name spec)
                            (symbol-name folding-prop))
        (throw :exit spec)))))

(defvar org-fold-core--property-symbol-cache (make-hash-table :test 'equal)
  "Saved values of folding properties for (buffer . spec) conses.")
(defvar-local org-fold-core--indirect-buffers nil
  "List of indirect buffers created from current buffer.

The first element of the list is always the current buffer.

This variable is needed to work around Emacs bug#46982, while Emacs
does not provide a way `after-change-functions' in any other buffer
than the buffer where the change was actually made.")

(defmacro org-fold-core-cycle-over-indirect-buffers (&rest body)
  "Execute BODY in current buffer and all its indirect buffers.

Also, make sure that folding properties from killed buffers are not
hanging around."
  (declare (debug (form body)) (indent 0))
  `(let (buffers dead-properties)
     (if (and (not (buffer-base-buffer))
              (not (memq (current-buffer) org-fold-core--indirect-buffers)))
         ;; We are in base buffer with `org-fold-core--indirect-buffers' value from
         ;; different buffer.  This can happen, for example, when
         ;; org-capture copies local variables into *Capture* buffer.
         (setq buffers (list (current-buffer)))
       (let ((all-buffers (buffer-local-value
                           'org-fold-core--indirect-buffers
                           (or (buffer-base-buffer) (current-buffer)))))
         (dolist (buf (cons (or (buffer-base-buffer) (current-buffer))
                            (buffer-local-value 'org-fold-core--indirect-buffers (or (buffer-base-buffer) (current-buffer)))))
           (if (buffer-live-p buf)
               (push buf buffers)
             (dolist (spec (org-fold-core-folding-spec-list))
               (when (and (not (org-fold-core-get-folding-spec-property spec :global))
                          (gethash (cons buf spec) org-fold-core--property-symbol-cache))
                 ;; Make sure that dead-properties variable can be passed
                 ;; as argument to `remove-text-properties'.
                 (push t dead-properties)
                 (push (gethash (cons buf spec) org-fold-core--property-symbol-cache)
                       dead-properties)))))
         (when dead-properties
           (with-current-buffer (or (buffer-base-buffer) (current-buffer))
             (setq-local org-fold-core--indirect-buffers
                         (seq-filter #'buffer-live-p all-buffers))))))
     (dolist (buf buffers)
       (with-current-buffer buf
         (when dead-properties
           (with-silent-modifications
             (save-restriction
               (widen)
               (remove-text-properties
                (point-min) (point-max)
                dead-properties))))
         ,@body))))

;; This is the core function used to fold text in buffers.  We use
;; text properties to hide folded text, however 'invisible property is
;; not directly used (unless risky `org-fold-core--optimise-for-huge-buffers' is
;; enabled). Instead, we define unique text property (folding
;; property) for every possible folding spec and add the resulting
;; text properties into `char-property-alias-alist', so that
;; 'invisible text property is automatically defined if any of the
;; folding properties is non-nil.  This approach lets us maintain
;; multiple folds for the same text region - poor man's overlays (but
;; much faster).  Additionally, folding properties are ensured to be
;; unique for different buffers (especially for indirect
;; buffers). This is done to allow different folding states in
;; indirect buffers.
(defun org-fold-core--property-symbol-get-create (spec &optional buffer return-only)
  "Return a unique symbol suitable as folding text property.
Return value is unique for folding SPEC in BUFFER.
If the buffer already have buffer-local setup in `char-property-alias-alist'
and the setup appears to be created for different buffer,
copy the old invisibility state into new buffer-local text properties,
unless RETURN-ONLY is non-nil."
  (if (eq org-fold-core-style 'overlays)
      (or (gethash (cons 'global spec) org-fold-core--property-symbol-cache)
          (puthash (cons 'global spec)
                   (org-fold-core-get-folding-property-symbol spec nil 'global)
                   org-fold-core--property-symbol-cache))
    (let* ((buf (or buffer (current-buffer))))
      ;; Create unique property symbol for SPEC in BUFFER
      (let ((local-prop (or (gethash (cons buf spec) org-fold-core--property-symbol-cache)
			    (puthash (cons buf spec)
                                     (org-fold-core-get-folding-property-symbol
                                      spec buf
                                      (org-fold-core-get-folding-spec-property spec :global))
                                     org-fold-core--property-symbol-cache))))
        (prog1
            local-prop
          (unless return-only
	    (with-current-buffer buf
              ;; Update folding properties carried over from other
              ;; buffer (implying that current buffer is indirect
              ;; buffer). Normally, `char-property-alias-alist' in new
              ;; indirect buffer is a copy of the same variable from
              ;; the base buffer. Then, `char-property-alias-alist'
              ;; would contain folding properties, which are not
              ;; matching the generated `local-prop'.
	      (unless (member local-prop (cdr (assq 'invisible char-property-alias-alist)))
                ;; Copy all the old folding properties to preserve the folding state
                (with-silent-modifications
                  (dolist (old-prop (cdr (assq 'invisible char-property-alias-alist)))
                    (org-with-wide-buffer
                     (let* ((pos (point-min))
	                    (spec (org-fold-core-get-folding-spec-from-folding-prop old-prop))
                            ;; Generate new buffer-unique folding property
	                    (new-prop (when spec (org-fold-core--property-symbol-get-create spec nil 'return-only))))
                       ;; Copy the visibility state for `spec' from `old-prop' to `new-prop'
                       (unless (eq old-prop new-prop)
                         (while (< pos (point-max))
	                   (let ((val (get-text-property pos old-prop))
                                 (next (next-single-char-property-change pos old-prop)))
	                     (when val
	                       (put-text-property pos next new-prop val))
                             (setq pos next)))))))
                  ;; Update `char-property-alias-alist' with folding
                  ;; properties unique for the current buffer.
                  (setq-local char-property-alias-alist
	                      (cons (cons 'invisible
			                  (mapcar (lambda (spec)
				                    (org-fold-core--property-symbol-get-create spec nil 'return-only))
				                  (org-fold-core-folding-spec-list)))
		                    (remove (assq 'invisible char-property-alias-alist)
			                    char-property-alias-alist)))
                  ;; Set folding property stickiness according to
                  ;; their `:font-sticky' and `:rear-sticky'
                  ;; parameters.
                  (let (full-prop-list)
                    (org-fold-core-cycle-over-indirect-buffers
                      (setq full-prop-list
                            (append full-prop-list
                                    (delq nil
                                          (mapcar (lambda (spec)
                                                    (cond
                                                     ((org-fold-core-get-folding-spec-property spec :front-sticky)
                                                      (cons (org-fold-core--property-symbol-get-create spec nil 'return-only)
                                                            nil))
                                                     ((org-fold-core-get-folding-spec-property spec :rear-sticky)
                                                      nil)
                                                     (t
                                                      (cons (org-fold-core--property-symbol-get-create spec nil 'return-only)
                                                            t))))
                                                  (org-fold-core-folding-spec-list))))))
                    (org-fold-core-cycle-over-indirect-buffers
                      (setq-local text-property-default-nonsticky
                                  (delete-dups (append
                                                text-property-default-nonsticky
                                                full-prop-list))))))))))))))

(defun org-fold-core--update-buffer-folds ()
  "Copy folding state in a new buffer with text copied from old buffer."
  (org-fold-core--property-symbol-get-create (car (org-fold-core-folding-spec-list))))

(defun org-fold-core-decouple-indirect-buffer-folds ()
  "Copy and decouple folding state in a newly created indirect buffer.
This function is mostly intended to be used in
`clone-indirect-buffer-hook'."
  ;; Add current buffer to the list of indirect buffers in the base buffer.
  (when (buffer-base-buffer)
    (let ((new-buffer (current-buffer)))
      (with-current-buffer (buffer-base-buffer)
        (setq-local org-fold-core--indirect-buffers
                    (let (bufs)
                      (org-fold-core-cycle-over-indirect-buffers
                        (push (current-buffer) bufs))
                      (push new-buffer bufs)
                      (delete-dups bufs))))))
  (when (and (buffer-base-buffer)
             (eq org-fold-core-style 'text-properties)
             (not (memql 'ignore-indirect org-fold-core--optimise-for-huge-buffers)))
    (org-fold-core--update-buffer-folds)))

;;; API

;;;; Modifying folding specs

(defun org-fold-core-set-folding-spec-property (spec property value &optional force)
  "Set PROPERTY of a folding SPEC to VALUE.
Possible properties and values can be found in `org-fold-core--specs' docstring.
Do not check previous value when FORCE is non-nil."
  (pcase property
    (:ellipsis
     (unless (and (not force) (equal value (org-fold-core-get-folding-spec-property spec :ellipsis)))
       (remove-from-invisibility-spec (cons spec (org-fold-core-get-folding-spec-property spec :ellipsis)))
       (unless (org-fold-core-get-folding-spec-property spec :visible)
         (add-to-invisibility-spec (cons spec value)))))
    (:visible
     (unless (or (memql 'merge-folds org-fold-core--optimise-for-huge-buffers)
                 (and (not force) (equal value (org-fold-core-get-folding-spec-property spec :visible))))
       (if value
	   (remove-from-invisibility-spec (cons spec (org-fold-core-get-folding-spec-property spec :ellipsis)))
         (add-to-invisibility-spec (cons spec (org-fold-core-get-folding-spec-property spec :ellipsis))))))
    (:alias
     ;; Clear symbol cache.
     (setq org-fold-core--spec-symbols nil))
    (:isearch-open nil)
    (:isearch-ignore nil)
    (:front-sticky nil)
    (:rear-sticky nil)
    (_ nil))
  (setf (cdr (assq property (assq spec org-fold-core--specs))) value))

(defun org-fold-core-add-folding-spec (spec &optional properties buffer append)
  "Add a new folding SPEC with PROPERTIES in BUFFER.

SPEC must be a symbol.  BUFFER can be a buffer to set SPEC in or nil to
set SPEC in current buffer.

By default, the added SPEC will have highest priority among the
previously defined specs.  When optional APPEND argument is non-nil,
SPEC will have the lowest priority instead.  If SPEC was already
defined earlier, it will be redefined according to provided optional
arguments.
`
The folding spec properties will be set to PROPERTIES (see
`org-fold-core--specs' for details)."
  (when (eq spec 'all) (error "Cannot use reserved folding spec symbol 'all"))
  (with-current-buffer (or buffer (current-buffer))
    ;; Clear the cache.
    (setq org-fold-core--spec-list nil
          org-fold-core--spec-symbols nil)
    (let* ((full-properties (mapcar (lambda (prop) (cons prop (cdr (assq prop properties))))
                                    '( :visible :ellipsis :isearch-ignore
                                       :global :isearch-open :front-sticky
                                       :rear-sticky :fragile :alias :font-lock)))
           (full-spec (cons spec full-properties)))
      (add-to-list 'org-fold-core--specs full-spec append)
      (mapc (lambda (prop-cons) (org-fold-core-set-folding-spec-property spec (car prop-cons) (cdr prop-cons) 'force)) full-properties)
      ;; Update buffer inivisibility specs.
      (org-fold-core--property-symbol-get-create spec))))

(defun org-fold-core-remove-folding-spec (spec &optional buffer)
  "Remove a folding SPEC in BUFFER.

SPEC must be a symbol.

BUFFER can be a buffer to remove SPEC in, nil to remove SPEC in current
buffer, or `all' to remove SPEC in all open `org-mode' buffers and all
future org buffers."
  (org-fold-core--check-spec spec)
  (when (eq buffer 'all)
    (setq-default org-fold-core--specs (delete (cdr (assq spec org-fold-core--specs)) org-fold-core--specs))
    (mapc (lambda (buf)
	    (org-fold-core-remove-folding-spec spec buf))
	  (buffer-list)))
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      ;; Clear the cache.
      (setq org-fold-core--spec-list nil
            org-fold-core--spec-symbols nil)
      (org-fold-core-set-folding-spec-property spec :visible t)
      (setq org-fold-core--specs (delete (cdr (assq spec org-fold-core--specs)) org-fold-core--specs)))))

(defun org-fold-core-initialize (&optional specs)
  "Setup folding in current buffer using SPECS as value of `org-fold-core--specs'."
  ;; Preserve the priorities.
  (when specs (setq specs (nreverse specs)))
  (unless specs (setq specs org-fold-core--specs))
  (setq org-fold-core--specs nil
        org-fold-core--spec-list nil
        org-fold-core--spec-symbols nil)
  (dolist (spec specs)
    (org-fold-core-add-folding-spec (car spec) (cdr spec)))
  (add-hook 'after-change-functions 'org-fold-core--fix-folded-region nil 'local)
  (add-hook 'clone-indirect-buffer-hook #'org-fold-core-decouple-indirect-buffer-folds nil 'local)
  ;; Setup killing text
  (setq-local filter-buffer-substring-function #'org-fold-core--buffer-substring-filter)
  (if (and (boundp 'isearch-opened-regions)
           (eq org-fold-core-style 'text-properties))
      ;; Use new implementation of isearch allowing to search inside text
      ;; hidden via text properties.
      (org-fold-core--isearch-setup 'text-properties)
    (org-fold-core--isearch-setup 'overlays)))

;;;; Searching and examining folded text

(defsubst org-fold-core-folded-p (&optional pos spec-or-alias)
  "Non-nil if the character after POS is folded.
If POS is nil, use `point' instead.
If SPEC-OR-ALIAS is a folding spec or a list, only check the given
folding spec or the listed specs."
  (if (and spec-or-alias (listp spec-or-alias))
      (catch :found
        (dolist (spec spec-or-alias)
          (let ((val (org-fold-core-get-folding-spec spec pos)))
            (when val (throw :found val)))))
    (org-fold-core-get-folding-spec spec-or-alias pos)))

(defun org-fold-core-region-folded-p (beg end &optional spec-or-alias)
  "Non-nil if the region between BEG and END is folded.
If SPEC-OR-ALIAS is a folding spec, only check the given folding spec."
  (org-with-point-at beg
    (catch :visible
      (while (< (point) end)
        (unless (org-fold-core-get-folding-spec spec-or-alias) (throw :visible nil))
        (goto-char (org-fold-core-next-folding-state-change spec-or-alias nil end)))
      t)))

(defun org-fold-core-get-folding-spec (&optional spec-or-alias pom)
  "Get folding state at `point' or POM.
Return nil if there is no folding at point or POM.
If SPEC-OR-ALIAS is nil, return a folding spec with highest priority
among present at `point' or POM.
If SPEC-OR-ALIAS is `all', return the list of all present folding
specs.
If SPEC-OR-ALIAS is a valid folding spec or a spec alias, return the
corresponding folding spec (if the text is folded using that spec)."
  (let ((spec (if (eq spec-or-alias 'all)
                  'all
                (org-fold-core-get-folding-spec-from-alias spec-or-alias))))
    (when (and spec (not (eq spec 'all))) (org-fold-core--check-spec spec))
    (org-with-point-at pom
      (cond
       ((or (null spec) (eq spec 'all))
        (catch :single-spec
          (let ((result))
	    (dolist (lspec (org-fold-core-folding-spec-list))
	      (let ((val (if (eq org-fold-core-style 'text-properties)
                             (get-text-property (point) (org-fold-core--property-symbol-get-create lspec nil t))
                           (get-char-property (point) (org-fold-core--property-symbol-get-create lspec nil t)))))
                (when (and val (null spec)) (throw :single-spec val))
	        (when val (push val result))))
            (reverse result))))
       (t (if (eq org-fold-core-style 'text-properties)
              (get-text-property (point) (org-fold-core--property-symbol-get-create spec nil t))
            (get-char-property (point) (org-fold-core--property-symbol-get-create spec nil t))))))))

(defun org-fold-core-get-folding-specs-in-region (beg end)
  "Get all folding specs in region from BEG to END."
  (let ((pos beg)
	all-specs)
    (while (< pos end)
      (setq all-specs (append all-specs (org-fold-core-get-folding-spec nil pos)))
      (setq pos (org-fold-core-next-folding-state-change nil pos end)))
    (unless (listp all-specs) (setq all-specs (list all-specs)))
    (delete-dups all-specs)))

(defun org-fold-core-get-region-at-point (&optional spec-or-alias pom)
  "Return region folded using SPEC-OR-ALIAS at POM.
If SPEC is nil, return the largest possible folded region.
The return value is a cons of beginning and the end of the region.
Return nil when no fold is present at point of POM."
  (let ((spec (org-fold-core-get-folding-spec-from-alias spec-or-alias)))
    (org-with-point-at (or pom (point))
      (if spec
          (if (eq org-fold-core-style 'text-properties)
	      (org-find-text-property-region (point) (org-fold-core--property-symbol-get-create spec nil t))
            (let ((ov (cdr (get-char-property-and-overlay (point) (org-fold-core--property-symbol-get-create spec nil t)))))
              (when ov (cons (overlay-start ov) (overlay-end ov)))))
        (let ((region (cons (point) (point))))
	  (dolist (spec (org-fold-core-get-folding-spec 'all))
            (let ((local-region (org-fold-core-get-region-at-point spec)))
              (when (< (car local-region) (car region))
                (setcar region (car local-region)))
              (when (> (cdr local-region) (cdr region))
                (setcdr region (cdr local-region)))))
	  (unless (eq (car region) (cdr region)) region))))))

(defun org-fold-core-next-visibility-change (&optional pos limit ignore-hidden-p previous-p)
  "Return next point from POS up to LIMIT where text becomes visible/invisible.
By default, text hidden by any means (i.e. not only by folding, but
also via fontification) will be considered.
If IGNORE-HIDDEN-P is non-nil, consider only folded text.
If PREVIOUS-P is non-nil, search backwards."
  (let* ((pos (or pos (point)))
	 (invisible-p (if ignore-hidden-p
			  #'org-fold-core-folded-p
			#'invisible-p))
         (invisible-initially? (funcall invisible-p pos))
	 (limit (or limit (if previous-p
			      (point-min)
			    (point-max))))
         (cmp (if previous-p #'> #'<))
	 (next-change (if previous-p
			  (if ignore-hidden-p
                              (lambda (p) (org-fold-core-previous-folding-state-change (org-fold-core-get-folding-spec nil p) p limit))
			    (lambda (p) (max limit (previous-single-char-property-change p 'invisible nil limit))))
                        (if ignore-hidden-p
                            (lambda (p) (org-fold-core-next-folding-state-change (org-fold-core-get-folding-spec nil p) p limit))
			  (lambda (p) (next-single-char-property-change p 'invisible nil limit)))))
	 (next pos))
    (while (and (funcall cmp next limit)
		(not (org-xor
                    invisible-initially?
                    (funcall invisible-p
                             (if previous-p
                                 ;; NEXT-1 -> NEXT is the change.
                                 (max limit (1- next))
                               ;; NEXT -> NEXT+1 is the change.
                               next)))))
      (setq next (funcall next-change next)))
    next))

(defun org-fold-core-previous-visibility-change (&optional pos limit ignore-hidden-p)
  "Call `org-fold-core-next-visibility-change' searching backwards."
  (org-fold-core-next-visibility-change pos limit ignore-hidden-p 'previous))

(defun org-fold-core-next-folding-state-change (&optional spec-or-alias pos limit previous-p)
  "Return point after POS where folding state changes up to LIMIT.
If SPEC-OR-ALIAS is nil, return next point where _any_ single folding
spec changes.
For example, (org-fold-core-next-folding-state-change nil) with point
somewhere in the below structure will return the nearest <...> point.

* Headline <begin outline fold>
:PROPERTIES:<begin drawer fold>
:ID: test
:END:<end drawer fold>

Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo,
quis tempor ligula erat quis odio.

** Another headline
:DRAWER:<begin drawer fold>
:END:<end drawer fold>
** Yet another headline
<end of outline fold>

If SPEC-OR-ALIAS is a folding spec symbol, only consider that folding
spec.

If SPEC-OR-ALIAS is a list, only consider changes of folding specs
from the list.

Search backwards when PREVIOUS-P is non-nil."
  (when (and spec-or-alias (symbolp spec-or-alias))
    (setq spec-or-alias (list spec-or-alias)))
  (when spec-or-alias
    (setq spec-or-alias
	  (mapcar (lambda (spec-or-alias)
		    (or (org-fold-core-get-folding-spec-from-alias spec-or-alias)
			spec-or-alias))
                  spec-or-alias))
    (mapc #'org-fold-core--check-spec spec-or-alias))
  (unless spec-or-alias
    (setq spec-or-alias (org-fold-core-folding-spec-list)))
  (setq pos (or pos (point)))
  (let ((limit (or limit (if previous-p (point-min) (point-max)))))
    (catch :limit
      (dolist (prop (mapcar
                     (lambda (el)
                       (org-fold-core--property-symbol-get-create el nil t))
                     spec-or-alias))
        (when (= limit pos) (throw :limit limit))
        (setq
         limit
         (if previous-p
             (previous-single-char-property-change pos prop nil limit)
           (next-single-char-property-change pos prop nil limit))))
      limit)))

(defun org-fold-core-previous-folding-state-change (&optional spec-or-alias pos limit)
  "Call `org-fold-core-next-folding-state-change' searching backwards."
  (org-fold-core-next-folding-state-change spec-or-alias pos limit 'previous))

(defun org-fold-core-search-forward (spec-or-alias &optional limit)
  "Search next region folded via folding SPEC-OR-ALIAS up to LIMIT.
Move point right after the end of the region, to LIMIT, or
`point-max'.  The `match-data' will contain the region."
  (let ((spec (org-fold-core-get-folding-spec-from-alias spec-or-alias)))
    (let ((prop-symbol (org-fold-core--property-symbol-get-create spec nil t)))
      (goto-char (or (next-single-char-property-change (point) prop-symbol nil limit) limit (point-max)))
      (when (and (< (point) (or limit (point-max)))
	         (not (org-fold-core-get-folding-spec spec)))
        (goto-char (next-single-char-property-change (point) prop-symbol nil limit)))
      (when (org-fold-core-get-folding-spec spec)
        (let ((region (org-fold-core-get-region-at-point spec)))
	  (when (< (cdr region) (or limit (point-max)))
	    (goto-char (1+ (cdr region)))
            (set-match-data (list (set-marker (make-marker) (car region) (current-buffer))
				  (set-marker (make-marker) (cdr region) (current-buffer))))))))))

(cl-defun org-fold-core-get-regions (&key specs from to with-markers relative)
  "Find all the folded regions in current buffer.

Each element of the returned list represent folded region boundaries
and the folding spec: (BEG END SPEC).

Search folds intersecting with (FROM TO) buffer region if FROM and TO
are provided.

If FROM is non-nil and TO is nil, search the folded regions at FROM.

When both FROM and TO are nil, search folded regions in the whole buffer.

When SPECS is non-nil it should be a list of folding specs or a symbol.
Only return the matching fold types.

When WITH-MARKERS is non-nil, use markers to represent region
boundaries.

When RELATIVE is a buffer position, regions boundaries are given
relative to that position.
When RELATIVE is t, use FROM as the position.
WITH-MARKERS must be nil when RELATIVE is non-nil."
  (when (and relative with-markers)
    (error "Cannot use markers in non-absolute region boundaries"))
  (when (eq relative t) (setq relative from))
  (unless (listp specs) (setq specs (list specs)))
  (let (regions region mk-region)
    (org-with-wide-buffer
     (when (and (not from) (not to))
       (setq from (point-min)
             to (point-max)))
     (when (and from (not to)) (setq to (point-max)))
     (when (and from (< from (point-min))) (setq from (point-min)))
     (when (and to (> to (point-max))) (setq to (point-max)))
     (unless from (setq from (point-min)))
     (dolist (spec (or specs (org-fold-core-folding-spec-list)) regions)
       (goto-char from)
       (catch :exit
         (while (or (not to) (< (point) to))
           (when (org-fold-core-get-folding-spec spec)
             (setq region (org-fold-core-get-region-at-point spec))
             (when relative
               (cl-decf (car region) relative)
               (cl-decf (cdr region) relative))
             (if (not with-markers)
                 (setq mk-region `(,(car region) ,(cdr region) ,spec))
               (setq mk-region `(,(make-marker) ,(make-marker) ,spec))
               (move-marker (nth 0 mk-region) (car region))
               (move-marker (nth 1 mk-region) (cdr region)))
             (push mk-region regions))
           (unless to (throw :exit nil))
           (goto-char (org-fold-core-next-folding-state-change spec nil to))))))))

;;;; Changing visibility

;;;;; Region visibility

(defvar org-fold-core--keep-overlays nil
  "When non-nil, `org-fold-core-region' will not remove existing overlays.")
(defvar org-fold-core--isearch-overlays) ; defined below
(defmacro org-fold-core--keep-overlays (&rest body)
  "Run BODY with `org-fold-core--keep-overlays' set to t."
  (declare (debug (body)))
  `(let ((org-fold-core--keep-overlays t))
     ,@body))

(defvar org-fold-core--isearch-active nil
  "When non-nil, `org-fold-core-region' records created overlays.
New overlays will be added to `org-fold-core--isearch-overlays'.")
(defmacro org-fold-core--with-isearch-active (&rest body)
  "Run BODY with `org-fold-core--isearch-active' set to t."
  (declare (debug (body)))
  `(let ((org-fold-core--isearch-active t))
     ,@body))

;; This is the core function performing actual folding/unfolding.  The
;; folding state is stored in text property (folding property)
;; returned by `org-fold-core--property-symbol-get-create'.  The value of the
;; folding property is folding spec symbol.
(defun org-fold-core-region (from to flag &optional spec-or-alias)
  "Hide or show lines from FROM to TO, according to FLAG.
SPEC-OR-ALIAS is the folding spec or foldable element, as a symbol.
If SPEC-OR-ALIAS is omitted and FLAG is nil, unfold everything in the region."
  (let ((spec (org-fold-core-get-folding-spec-from-alias spec-or-alias)))
    (when spec (org-fold-core--check-spec spec))
    (with-silent-modifications
      (org-with-wide-buffer
       ;; Arrange fontifying newlines after all the folds between FROM
       ;; and TO to match the first character before the fold; not the
       ;; last as per Emacs defaults.  This makes :extend faces span
       ;; past the ellipsis.  See bug#65896.  The face properties are
       ;; assigned via `org-activate-folds'.
       (when (or (not spec) (org-fold-core-get-folding-spec-property spec :font-lock))
         (when (equal ?\n (char-after from))
           (font-lock-flush from (1+ from)))
         (when (equal ?\n (char-after to))
           (font-lock-flush to (1+ to)))
         (dolist (region (org-fold-core-get-regions :from from :to to :specs spec))
           (when (equal ?\n (char-after (cadr region)))
             (font-lock-flush (cadr region) (1+ (cadr region))))
           ;; Re-fontify beginning of the fold - we may
           ;; unfold inside an existing fold, with FROM begin a newline
           ;; after spliced fold.
           (when (equal ?\n (char-after (car region)))
             (font-lock-flush (car region) (1+ (car region))))))
       (when (eq org-fold-core-style 'overlays)
         (if org-fold-core--keep-overlays
             (mapc
              (lambda (ov)
                (when (or (not spec)
                          (eq spec (overlay-get ov 'invisible)))
                  (when (and org-fold-core--isearch-active
                             (overlay-get ov 'invisible)
                             (org-fold-core-get-folding-spec-property
                              (overlay-get ov 'invisible) :isearch-open))
                    (when (overlay-get ov 'invisible)
                      (overlay-put ov 'org-invisible (overlay-get ov 'invisible)))
                    (overlay-put ov 'invisible nil)
                    (when org-fold-core--isearch-active
                      (cl-pushnew ov org-fold-core--isearch-overlays)))))
              (overlays-in from to))
           (when spec
             (remove-overlays from to 'org-invisible spec)
             (remove-overlays from to 'invisible spec))))
       (if flag
	   (if (not spec)
               (error "Calling `org-fold-core-region' with missing SPEC")
             (if (eq org-fold-core-style 'overlays)
                 ;; Use `front-advance' since text right before to the beginning of
                 ;; the overlay belongs to the visible line than to the contents.
                 (let ((o (make-overlay from to nil
                                        (org-fold-core-get-folding-spec-property spec :front-sticky)
                                        (org-fold-core-get-folding-spec-property spec :rear-sticky))))
                   (when org-fold-core--isearch-active
                     (push o org-fold-core--isearch-overlays))
                   (overlay-put o 'evaporate t)
                   (overlay-put o (org-fold-core--property-symbol-get-create spec) spec)
                   (overlay-put o 'invisible spec)
                   ;; Preserve priority.
                   (overlay-put o 'priority (length (member spec (org-fold-core-folding-spec-list))))
                   (overlay-put o 'isearch-open-invisible #'org-fold-core--isearch-show))
	       (put-text-property from to (org-fold-core--property-symbol-get-create spec) spec)
	       (put-text-property from to 'isearch-open-invisible #'org-fold-core--isearch-show)
	       (put-text-property from to 'isearch-open-invisible-temporary #'org-fold-core--isearch-show-temporary)
               (when (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
                 ;; If the SPEC has highest priority, assign it directly
                 ;; to 'invisible property as well.  This is done to speed
                 ;; up Emacs redisplay on huge (Mbs) folded regions where
                 ;; we don't even want Emacs to spend time cycling over
                 ;; `char-property-alias-alist'.
                 (when (eq spec (caar org-fold-core--specs)) (put-text-property from to 'invisible spec)))))
         (if (not spec)
             (mapc (lambda (spec) (org-fold-core-region from to nil spec)) (org-fold-core-folding-spec-list))
           (when (and (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
                      (eq org-fold-core-style 'text-properties))
             (when (eq spec (caar org-fold-core--specs))
               (let ((pos from))
                 (while (< pos to)
                   (if (eq spec (get-text-property pos 'invisible))
                       (let ((next (org-fold-core-next-folding-state-change spec pos to)))
                         (remove-text-properties pos next '(invisible t))
                         (setq pos next))
                     (setq pos (next-single-char-property-change pos 'invisible nil to)))))))
           (when (eq org-fold-core-style 'text-properties)
	     (remove-text-properties from to (list (org-fold-core--property-symbol-get-create spec) nil)))))
       ;; Re-calculate trailing faces for all the folds revealed
       ;; by unfolding or created by folding.
       (when (or (not spec) (org-fold-core-get-folding-spec-property spec :font-lock))
         (dolist (region (org-fold-core-get-regions :from from :to to :specs spec))
           (when (equal ?\n (char-after (cadr region)))
             (font-lock-flush (cadr region) (1+ (cadr region))))))))))

(cl-defmacro org-fold-core-regions (regions &key override clean-markers relative)
  "Fold every region in REGIONS list in current buffer.

Each region in the list is a list (BEG END SPEC-OR-ALIAS) describing
region and folding spec to be applied.

When optional argument OVERRIDE is non-nil, clear folding state in the
buffer first.

When optional argument CLEAN-MARKERS is non-nil, clear markers used to
mark region boundaries in REGIONS.

When optional argument RELATIVE is non-nil, it must be a buffer
position.  REGION boundaries are then treated as relative distances
from that position."
  `(org-with-wide-buffer
    (when ,override (org-fold-core-region (point-min) (point-max) nil))
    (pcase-dolist (`(,beg ,end ,spec) (delq nil ,regions))
      (let ((rel ,relative))
        (if rel
            (org-fold-core-region (+ rel beg) (+ rel end) t spec)
          (org-fold-core-region beg end t spec)))
      (when ,clean-markers
        (when (markerp beg) (set-marker beg nil))
        (when (markerp end) (set-marker end nil))))))

(defmacro org-fold-core-save-visibility (use-markers &rest body)
  "Save and restore folding state around BODY.
If USE-MARKERS is non-nil, use markers for the positions.  This
means that the buffer may change while running BODY, but it also
means that the buffer should stay alive during the operation,
because otherwise all these markers will point to nowhere."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (regions)
    `(let* ((,regions (org-fold-core-get-regions :with-markers ,use-markers)))
       (unwind-protect (progn ,@body)
         (org-fold-core-regions ,regions :override t :clean-markers t)))))

;;; Make isearch search in some text hidden via text propertoes

(defvar org-fold-core--isearch-overlays nil
  "List of overlays temporarily created during isearch.
This is used to allow searching in regions hidden via text properties.
As for [2020-05-09 Sat], Isearch only has special handling of hidden overlays.
Any text hidden via text properties is not revealed even if `search-invisible'
is set to `t'.")

(defvar-local org-fold-core--isearch-local-regions (make-hash-table :test 'equal)
  "Hash table storing temporarily shown folds from isearch matches.")

(defun org-fold-core--isearch-setup (type)
  "Initialize isearch in org buffer.
TYPE can be either `text-properties' or `overlays'."
  (pcase type
    (`text-properties
     (setq-local search-invisible 'open-all)
     (add-hook 'isearch-mode-end-hook #'org-fold-core--clear-isearch-state nil 'local)
     (add-hook 'isearch-mode-hook #'org-fold-core--clear-isearch-state nil 'local)
     (setq-local isearch-filter-predicate #'org-fold-core--isearch-filter-predicate-text-properties))
    (`overlays
     (when (eq org-fold-core-style 'text-properties)
       (add-function :before (local 'isearch-filter-predicate) #'org-fold-core--create-isearch-overlays)
       ;; When `isearch-filter-predicate' is called outside isearch,
       ;; it is common that `isearch-mode-end-hook' does not get
       ;; executed, but `isearch-clean-overlays' usually does.
       (advice-add
        'isearch-clean-overlays :after
        #'org-fold-core--clear-isearch-overlays
        '((name . isearch-clean-overlays@org-fold-core)))))
    (_ (error "%s: Unknown type of setup for `org-fold-core--isearch-setup'" type))))

(defun org-fold-core--isearch-reveal (pos)
  "Default function used to reveal hidden text at POS for isearch."
  (let ((region (org-fold-core-get-region-at-point nil pos)))
    (org-fold-core-region (car region) (cdr region) nil)))

(defun org-fold-core--isearch-filter-predicate-text-properties (beg end)
  "Make sure that folded text is searchable when user want so.
This function is intended to be used as `isearch-filter-predicate'."
  (and
   ;; Check folding specs that cannot be searched
   (not (memq nil (mapcar (lambda (spec) (not (org-fold-core-get-folding-spec-property spec :isearch-ignore)))
                      (org-fold-core-get-folding-specs-in-region beg end))))
   ;; Check 'invisible properties that are not folding specs.
   (or (eq search-invisible t) ; User wants to search anyway, allow it.
       (let ((pos beg)
	     unknown-invisible-property)
	 (while (and (< pos end)
		     (not unknown-invisible-property))
	   (when (and (get-text-property pos 'invisible)
                      (not (org-fold-core-folding-spec-p (get-text-property pos 'invisible))))
	     (setq unknown-invisible-property t))
	   (setq pos (next-single-char-property-change pos 'invisible)))
	 (not unknown-invisible-property)))
   (or (and (eq search-invisible t)
	    ;; FIXME: this opens regions permanenly for now.
            ;; I also tried to force search-invisible 'open-all around
            ;; `isearch-range-invisible', but that somehow causes
            ;; infinite loop in `isearch-lazy-highlight'.
            (prog1 t
	      ;; We still need to reveal the folded location
	      (org-fold-core--isearch-show-temporary (cons beg end) nil)))
       (not (isearch-range-invisible beg end)))))

(defun org-fold-core--clear-isearch-state ()
  "Clear `org-fold-core--isearch-local-regions'."
  (clrhash org-fold-core--isearch-local-regions))

(defun org-fold-core--isearch-show (overlay-or-region)
  "Reveal text at OVERLAY-OR-REGION found by isearch."
  (let (beg end)
    (if (overlayp overlay-or-region)
        (setq beg (overlay-start overlay-or-region)
              end (overlay-end overlay-or-region))
      (setq beg (car overlay-or-region)
            end (cdr overlay-or-region)))
    ;; FIXME: Reveal the match (usually point, but may sometimes go beyond the region).
    (when (< beg (point) end)
      (funcall org-fold-core-isearch-open-function (point)))
    (if (overlayp overlay-or-region)
        (delete-overlay overlay-or-region)
      (org-fold-core-region beg end nil))))

(defun org-fold-core--isearch-show-temporary (region hide-p)
  "Temporarily reveal text in REGION.
Hide text instead if HIDE-P is non-nil.
REGION can also be an overlay in current buffer."
  (save-match-data ; match data must not be modified.
    (let ((org-fold-core-style (if (overlayp region) 'overlays 'text-properties)))
      (if hide-p
          (if (not (overlayp region))
              nil ;; FIXME: after isearch supports text properties.
            (when (overlay-get region 'org-invisible)
              (overlay-put region 'invisible (overlay-get region 'org-invisible))))
        ;; isearch expects all the temporarily opened overlays to exist.
        ;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60399
        (org-fold-core--keep-overlays
         (org-fold-core--with-isearch-active
          (org-fold-core--isearch-show region)))))))

(defvar-local org-fold-core--isearch-special-specs nil
  "List of specs that can break visibility state when converted to overlays.
This is a hack, but I do not see a better way around until isearch
gets support of text properties.")
(defun org-fold-core--create-isearch-overlays (beg end)
  "Replace text property invisibility spec by overlays between BEG and END.
All the searchable folded regions will be changed to use overlays
instead of text properties.  The created overlays will be stored in
`org-fold-core--isearch-overlays'."
  (let ((pos beg))
    (while (< pos end)
      ;; We need loop below to make sure that we clean all invisible
      ;; properties, which may be nested.
      (catch :repeat
        (dolist (spec (org-fold-core-get-folding-spec 'all pos))
          (unless (org-fold-core-get-folding-spec-property spec :isearch-ignore)
	    (let* ((region (org-fold-core-get-region-at-point spec pos)))
              (when (memq spec org-fold-core--isearch-special-specs)
                (setq end (max end (cdr region)))
                (when (< (car region) beg)
                  (setq beg (car region))
                  (setq pos beg)
                  (throw :repeat t)))
	      ;; Changing text properties is considered buffer modification.
	      ;; We do not want it here.
	      (with-silent-modifications
                (org-fold-core-region (car region) (cdr region) nil spec)
                (let ((org-fold-core-style 'overlays))
                  (org-fold-core--with-isearch-active
                   (org-fold-core-region (car region) (cdr region) t spec)))))))
        (setq pos (org-fold-core-next-folding-state-change nil pos end))))))

(defun org-fold-core--clear-isearch-overlay (ov)
  "Convert OV region back into using text properties."
  (let ((spec (overlay-get ov 'invisible)))
    ;; Ignore deleted overlays.
    (when (and spec
	       (overlay-buffer ov))
      ;; Changing text properties is considered buffer modification.
      ;; We do not want it here.
      (with-silent-modifications
	(when (<= (overlay-end ov) (point-max))
	  (org-fold-core-region (overlay-start ov) (overlay-end ov) t spec)))))
  (delete-overlay ov))

(defun org-fold-core--clear-isearch-overlays ()
  "Convert overlays from `org-fold-core--isearch-overlays' back to text properties."
  (when org-fold-core--isearch-overlays
    (mapc #'org-fold-core--clear-isearch-overlay org-fold-core--isearch-overlays)
    (setq org-fold-core--isearch-overlays nil)))

;;; Handling changes in folded elements

(defvar org-fold-core--ignore-modifications nil
  "Non-nil: skip processing modifications in `org-fold-core--fix-folded-region'.")
(defvar org-fold-core--ignore-fragility-checks nil
  "Non-nil: skip fragility checks in `org-fold-core--fix-folded-region'.")
(defvar org-fold-core--suppress-folding-fix nil
  "Non-nil: skip folding fix in `org-fold-core--fix-folded-region'.")

(defmacro org-fold-core-ignore-modifications (&rest body)
  "Run BODY ignoring buffer modifications in `org-fold-core--fix-folded-region'."
  (declare (debug (form body)) (indent 0))
  `(let ((org-fold-core--ignore-modifications t))
     (unwind-protect (progn ,@body)
       (setq org-fold-core--last-buffer-chars-modified-tick (buffer-chars-modified-tick)))))

(defmacro org-fold-core-suppress-folding-fix (&rest body)
  "Run BODY skipping re-folding checks in `org-fold-core--fix-folded-region'."
  (declare (debug (form body)) (indent 0))
  `(let ((org-fold-core--suppress-folding-fix t))
     (progn ,@body)))

(defmacro org-fold-core-ignore-fragility-checks (&rest body)
  "Run BODY skipping :fragility checks in `org-fold-core--fix-folded-region'."
  (declare (debug (form body)) (indent 0))
  `(let ((org-fold-core--ignore-fragility-checks t))
     (progn ,@body)))

(defvar org-fold-core--region-delayed-list nil
  "List holding (MKFROM MKTO FLAG SPEC-OR-ALIAS) arguments to process.
The list is used by `org-fold-core--region-delayed'.")
(defun org-fold-core--region-delayed (from to flag &optional spec-or-alias)
  "Call `org-fold-core-region' after current command.
Pass the same FROM, TO, FLAG, and SPEC-OR-ALIAS."
  ;; Setup delayed folding.
  (add-hook 'post-command-hook #'org-fold-core--process-delayed)
  (let ((frommk (make-marker))
        (tomk (make-marker)))
    (set-marker frommk from (current-buffer))
    (set-marker tomk to (current-buffer))
    (push (list frommk tomk flag spec-or-alias) org-fold-core--region-delayed-list)))

(defun org-fold-core--process-delayed ()
  "Perform folding for `org-fold-core--region-delayed-list'."
  (when org-fold-core--region-delayed-list
    (mapc (lambda (args)
            (when (and (buffer-live-p (marker-buffer (nth 0 args)))
                       (buffer-live-p (marker-buffer (nth 1 args)))
                       (< (nth 0 args) (nth 1 args)))
              (org-with-point-at (car args)
                (apply #'org-fold-core-region args))))
          ;; Restore the initial folding order.
          (nreverse org-fold-core--region-delayed-list))
    ;; Cleanup `post-command-hook'.
    (remove-hook 'post-command-hook #'org-fold-core--process-delayed)
    (setq org-fold-core--region-delayed-list nil)))

(defvar-local org-fold-core--last-buffer-chars-modified-tick nil
  "Variable storing the last return value of `buffer-chars-modified-tick'.")

(defun org-fold-core--fix-folded-region (from to _)
  "Process modifications in folded elements within FROM . TO region.
This function intended to be used as one of `after-change-functions'.

This function does nothing if text the only modification was changing
text properties (for the sake of reducing overheads).

If a text was inserted into invisible region, hide the inserted text.
If a text was inserted in front/back of the region, hide it according
to :front-sticky/:rear-sticky folding spec property.

If the folded region is folded with a spec with non-nil :fragile
property, unfold the region if the :fragile function returns non-nil."
  ;; If no insertions or deletions in buffer, skip all the checks.
  (unless (or org-fold-core--ignore-modifications
              (eq org-fold-core--last-buffer-chars-modified-tick (buffer-chars-modified-tick))
              (memql 'ignore-modification-checks org-fold-core--optimise-for-huge-buffers))
    ;; Store the new buffer modification state.
    (setq org-fold-core--last-buffer-chars-modified-tick (buffer-chars-modified-tick))
    (save-match-data
      ;; Handle changes in all the indirect buffers and in the base
      ;; buffer.  Work around Emacs bug#46982.
      ;; Re-hide text inserted in the middle/front/back of a folded
      ;; region.
      (unless (or org-fold-core--suppress-folding-fix (equal from to)) ; Ignore deletions.
        (when (eq org-fold-core-style 'text-properties)
          (org-fold-core-cycle-over-indirect-buffers
	    (dolist (spec (org-fold-core-folding-spec-list))
              ;; Reveal fully invisible text inserted in the middle
              ;; of visible portion of the buffer.  This is needed,
              ;; for example, when there was a deletion in a folded
              ;; heading, the heading was unfolded, end `undo' was
              ;; called.  The `undo' would insert the folded text.
              (when (and (or (eq from (point-min))
                             (not (org-fold-core-folded-p (1- from) spec)))
                         (or (eq to (point-max))
                             (not (org-fold-core-folded-p to spec)))
                         (org-fold-core-region-folded-p from to spec))
                (org-fold-core-region from to nil spec))
              ;; Look around and fold the new text if the nearby folds are
              ;; sticky.
              (unless (org-fold-core-region-folded-p from to spec)
	        (let ((spec-to (org-fold-core-get-folding-spec spec (min to (1- (point-max)))))
		      (spec-from (org-fold-core-get-folding-spec spec (max (point-min) (1- from)))))
                  ;; Reveal folds around undone deletion.
                  (when undo-in-progress
                    (let ((lregion (org-fold-core-get-region-at-point spec (max (point-min) (1- from))))
                          (rregion (org-fold-core-get-region-at-point spec (min to (1- (point-max))))))
                      (if (and lregion rregion)
                          (org-fold-core-region (car lregion) (cdr rregion) nil spec)
                        (when lregion
                          (org-fold-core-region (car lregion) (cdr lregion) nil spec))
                        (when rregion
                          (org-fold-core-region (car rregion) (cdr rregion) nil spec)))))
                  ;; Hide text inserted in the middle of a fold.
	          (when (and (or spec-from (eq from (point-min)))
                             (or spec-to (eq to (point-max)))
                             (or spec-from spec-to)
                             (eq spec-to spec-from)
                             (or (org-fold-core-get-folding-spec-property spec :front-sticky)
                                 (org-fold-core-get-folding-spec-property spec :rear-sticky)))
                    (unless (and (eq from (point-min)) (eq to (point-max))) ; Buffer content replaced.
	              (org-fold-core-region from to t (or spec-from spec-to))))
                  ;; Hide text inserted at the end of a fold.
                  (when (and spec-from (org-fold-core-get-folding-spec-property spec-from :rear-sticky))
                    (org-fold-core-region from to t spec-from))
                  ;; Hide text inserted in front of a fold.
                  (when (and spec-to
                             (not (eq to (point-max))) ; Text inserted at the end of buffer is not prepended anywhere.
                             (org-fold-core-get-folding-spec-property spec-to :front-sticky))
                    (org-fold-core-region from to t spec-to))))))))
      ;; Process all the folded text between `from' and `to'.  Do it
      ;; only in current buffer to avoid verifying semantic structure
      ;; multiple times in indirect buffers that have exactly same
      ;; text anyway.
      (unless (or org-fold-core--ignore-fragility-checks
                  (memql 'ignore-fragility-checks org-fold-core--optimise-for-huge-buffers))
        (dolist (func org-fold-core-extend-changed-region-functions)
          (let ((new-region (funcall func from to)))
            (setq from (car new-region))
            (setq to (cdr new-region))))
        (org-fold-core-cycle-over-indirect-buffers
          (dolist (spec (org-fold-core-folding-spec-list))
            ;; No action is needed when :fragile is nil for the spec.
            (when (org-fold-core-get-folding-spec-property spec :fragile)
              (org-with-wide-buffer
               ;; Expand the considered region to include partially present fold.
               ;; Note: It is important to do this inside loop over all
               ;; specs.  Otherwise, the region may be expanded to huge
               ;; outline fold, potentially involving majority of the
               ;; buffer.  That would cause the below code to loop over
               ;; almost all the folds in buffer, which would be too slow.
               (let ((local-from from)
                     (local-to to)
                     (region-from (org-fold-core-get-region-at-point spec (max (point-min) (1- from))))
                     (region-to (org-fold-core-get-region-at-point spec (min to (1- (point-max))))))
                 (when region-from (setq local-from (car region-from)))
                 (when region-to (setq local-to (cdr region-to)))
                 (let ((pos local-from))
                   ;; Move to the first hidden region.
                   (unless (org-fold-core-get-folding-spec spec pos)
                     (setq pos (org-fold-core-next-folding-state-change spec pos local-to)))
                   ;; Cycle over all the folds.
                   (while (< pos local-to)
                     (save-match-data ; we should not clobber match-data in after-change-functions
                       (let ((fold-begin (and (org-fold-core-get-folding-spec spec pos)
                                              pos))
                             (fold-end (org-fold-core-next-folding-state-change spec pos local-to)))
                         (when (and fold-begin fold-end)
                           (when (save-excursion
                                   (funcall (org-fold-core-get-folding-spec-property spec :fragile)
                                            (cons fold-begin fold-end)
                                            spec))
                             ;; Reveal completely, not just from the SPEC.
                             ;; Do it only after command is finished -
                             ;; some Emacs commands assume that
                             ;; visibility is not altered by `after-change-functions'.
                             (org-fold-core--region-delayed fold-begin fold-end nil)))))
                     ;; Move to next fold.
                     (setq pos (org-fold-core-next-folding-state-change spec pos local-to)))))))))))))

;;; Handling killing/yanking of folded text

;; By default, all the text properties of the killed text are
;; preserved, including the folding text properties.  This can be
;; awkward when we copy a text from an indirect buffer to another
;; indirect buffer (or the base buffer).  The copied text might be
;; visible in the source buffer, but might disappear if we yank it in
;; another buffer.  This happens in the following situation:
;; ---- base buffer ----
;; * Headline<begin fold>
;; Some text hidden in the base buffer, but revealed in the indirect
;; buffer.<end fold>
;; * Another headline
;;
;; ---- end of base buffer ----
;; ---- indirect buffer ----
;; * Headline
;; Some text hidden in the base buffer, but revealed in the indirect
;; buffer.
;; * Another headline
;;
;; ---- end of indirect buffer ----
;; If we copy the text under "Headline" from the indirect buffer and
;; insert it under "Another headline" in the base buffer, the inserted
;; text will be hidden since it's folding text properties are copied.
;; Basically, the copied text would have two sets of folding text
;; properties: (1) Properties for base buffer telling that the text is
;; hidden; (2) Properties for the indirect buffer telling that the
;; text is visible.  The first set of the text properties in inactive
;; in the indirect buffer, but will become active once we yank the
;; text back into the base buffer.
;;
;; To avoid the above situation, we simply clear all the properties,
;; unrealated to current buffer when a text is copied.
;; FIXME: Ideally, we may want to carry the folding state of copied
;; text between buffer (probably via user customization).
(defun org-fold-core--buffer-substring-filter (beg end &optional delete)
  "Clear folding state in killed text.
This function is intended to be used as `filter-buffer-substring-function'.
The arguments and return value are as specified for `filter-buffer-substring'."
  (let ((return-string (buffer-substring--filter beg end delete))
	;; The list will be used as an argument to `remove-text-properties'.
	props-list)
    ;; There is no easy way to examine all the text properties of a
    ;; string, so we utilize the fact that printed string
    ;; representation lists all its properties.
    ;; Loop over the elements of string representation.
    (unless (or (string= "" return-string)
                (<= end beg)
                (eq org-fold-core-style 'overlays))
      ;; Collect all the text properties the string is completely
      ;; hidden with.
      (dolist (spec (org-fold-core-folding-spec-list))
        (when (and (org-fold-core-region-folded-p beg end spec)
                   (org-region-invisible-p beg end))
          (push (org-fold-core--property-symbol-get-create spec nil t) props-list)))
      (dolist (plist
               (if (fboundp 'object-intervals)
                   (object-intervals return-string)
                 ;; Backward compatibility with Emacs <28.
                 ;; FIXME: Is there any better way to do it?
                 ;; Yes, it is a hack.
                 ;; The below gives us string representation as a list.
                 ;; Note that we need to remove unreadable values, like markers (#<...>).
                 (seq-partition
                  (cdr (let ((data (read (replace-regexp-in-string
                                          "^#(" "("
                                          (replace-regexp-in-string
                                           " #(" " ("
                                           (replace-regexp-in-string
                                            "#<[^>]+>" "dummy"
                                            ;; Get text representation of the string object.
                                            ;; Make sure to print everything (see `prin1' docstring).
                                            ;; `prin1' is used to print "%S" format.
                                            (let (print-level print-length)
                                              (format "%S" return-string))))))))
                         (if (listp data) data (list data))))
                  3)))
        (let* ((start (car plist))
               (fin (cadr plist))
               (plist (car (cddr plist))))
          ;; Only lists contain text properties.
          (when (listp plist)
            ;; Collect all the relevant text properties.
            (while plist
              (let* ((prop (car plist))
                     (prop-name (symbol-name prop)))
                ;; Reveal hard-hidden text.  See
                ;; `org-fold-core--optimise-for-huge-buffers'.
                (when (and (eq prop 'invisible)
                           (member (cadr plist) (org-fold-core-folding-spec-list)))
                  (remove-text-properties start fin '(invisible t) return-string))
                ;; We do not care about values now.
                (setq plist (cddr plist))
                (when (string-match-p org-fold-core--spec-property-prefix prop-name)
                  ;; Leave folding specs from current buffer.  See
                  ;; comments in `org-fold-core--property-symbol-get-create' to
                  ;; understand why it works.
                  (unless (member prop (cdr (assq 'invisible char-property-alias-alist)))
                    (push prop props-list))))))))
      (remove-text-properties 0 (length return-string) props-list return-string))
    return-string))

(defun org-fold-core-update-optimisation (beg end)
  "Update huge buffer optimization between BEG and END.
See `org-fold-core--optimise-for-huge-buffers'."
  (when (and (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
             (eq org-fold-core-style 'text-properties))
    (let ((pos beg))
      (while (< pos end)
        (when (and (org-fold-core-folded-p pos (caar org-fold-core--specs))
                   (not (eq (caar org-fold-core--specs) (get-text-property pos 'invisible))))
          (put-text-property pos (org-fold-core-next-folding-state-change (caar org-fold-core--specs) pos end)
                             'invisible (caar org-fold-core--specs)))
        (setq pos (org-fold-core-next-folding-state-change (caar org-fold-core--specs) pos end))))))

(defun org-fold-core-remove-optimisation (beg end)
  "Remove huge buffer optimization between BEG and END.
See `org-fold-core--optimise-for-huge-buffers'."
  (when (and (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
             (eq org-fold-core-style 'text-properties))
    (let ((pos beg))
      (while (< pos end)
        (if (and (org-fold-core-folded-p pos (caar org-fold-core--specs))
                 (eq (caar org-fold-core--specs) (get-text-property pos 'invisible)))
            (remove-text-properties pos (org-fold-core-next-folding-state-change (caar org-fold-core--specs) pos end)
                                    '(invisible t)))
        (setq pos (org-fold-core-next-folding-state-change (caar org-fold-core--specs) pos end))))))

(provide 'org-fold-core)

;;; org-fold-core.el ends here
