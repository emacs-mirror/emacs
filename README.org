* which-key 
[[http://melpa.org/#/which-key][http://melpa.org/packages/which-key-badge.svg]] [[http://stable.melpa.org/#/which-key][file:http://stable.melpa.org/packages/which-key-badge.svg]] [[https://travis-ci.org/justbur/emacs-which-key][file:https://travis-ci.org/justbur/emacs-which-key.svg?branch=master]]

** Recent Changes
*** 2016-12-20: Added =which-key-max-display-columns=
Allows control over the number of columns in the popup. See [[#other-options][Other Options]].
*** 2016-11-21: Replacement list changes
The alists controlling the replacement of key binding descriptions was
simplified to use one centralized alist, =which-key-replacement-alist=. This
change also allows for some new features compared to the old method. The other
alists are deprecated. See [[#custom-string-replacement-options][Custom String Replacement Options]].

** Introduction
=which-key= is a minor mode for Emacs that displays the key bindings following
your currently entered incomplete command (a prefix) in a popup. For example,
after enabling the minor mode if you enter =C-x= and wait for the default of 1
second the minibuffer will expand with all of the available key bindings that
follow =C-x= (or as many as space allows given your settings).  This includes
prefixes like =C-x 8= which are shown in a different face. Screenshots of what
the popup will look like are included below. =which-key= started as a rewrite of
[[https://github.com/kai2nenobu/guide-key][guide-key-mode]], but the feature sets have diverged to a certain extent.

** Table of Contents                                                  :TOC_3:
 - [[#which-key][which-key]]
   - [[#recent-changes][Recent Changes]]
     - [[#2016-12-20-added-which-key-max-display-columns][2016-12-20: Added =which-key-max-display-columns=]]
     - [[#2016-11-21-replacement-list-changes][2016-11-21: Replacement list changes]]
   - [[#introduction][Introduction]]
   - [[#install][Install]]
     - [[#melpa][MELPA]]
     - [[#manually][Manually]]
   - [[#initial-setup][Initial Setup]]
     - [[#side-window-bottom-option][Side Window Bottom Option]]
     - [[#side-window-right-option][Side Window Right Option]]
     - [[#side-window-right-then-bottom][Side Window Right then Bottom]]
     - [[#minibuffer-option][Minibuffer Option]]
   - [[#additional-commands][Additional Commands]]
   - [[#special-features-and-configuration-options][Special Features and Configuration Options]]
     - [[#popup-type-options][Popup Type Options]]
     - [[#custom-string-replacement-options][Custom String Replacement Options]]
     - [[#sorting-options][Sorting Options]]
     - [[#paging-options][Paging Options]]
     - [[#face-customization-options][Face Customization Options]]
     - [[#other-options][Other Options]]
   - [[#support-for-third-party-libraries][Support for Third-Party Libraries]]
     - [[#key-chord][Key-chord]]
     - [[#evil-operators][Evil operators]]
     - [[#god-mode][God-mode]]
   - [[#more-examples][More Examples]]
     - [[#nice-display-with-split-frame][Nice Display with Split Frame]]
   - [[#thanks][Thanks]]

** Install
*** MELPA
After setting up [[http://melpa.org][MELPA]] as a repository, use =M-x package-install which-key= or
your preferred method. You will need to call =which-key-mode= to enable the
minor mode of course.

*** Manually
Add which-key.el to your =load-path= and require. Something like 
#+BEGIN_SRC emacs-lisp
(add-to-list 'load-path "path/to/which-key.el")
(require 'which-key)
(which-key-mode)
#+END_SRC

** Initial Setup
No further setup is required if you are happy with the default setup. To try
other options, there are 3 choices of default configs that are preconfigured
(then customize to your liking). The main choice is where you want the which-key
buffer to display. Screenshots of the default options are shown in the next
sections.

In each case, we show as many key bindings as we can fit in the buffer within
the constraints. The constraints are determined by several factors, including
your Emacs settings, the size of the current Emacs frame, and the which-key
settings, most of which are described below. 

There are many substitution abilities included, which are quite flexible
(ability to use regexp for example). This makes which-key very customizable.

*** Side Window Bottom Option
Popup side window on bottom. This is the current default. To restore this setup use

#+BEGIN_SRC emacs-lisp
(which-key-setup-side-window-bottom)
#+END_SRC

[[./img/which-key-bottom.png]]

*** Side Window Right Option
Popup side window on right. For defaults use

#+BEGIN_SRC emacs-lisp
(which-key-setup-side-window-right)
#+END_SRC

Note the defaults are fairly conservative and will tend to not display on
narrower frames. If you get a message saying which-key can't display the keys,
try making your frame wider or adjusting the defaults related to the maximum
width (see =M-x customize-group which-key=).

[[./img/which-key-right.png]]

*** Side Window Right then Bottom
This is a combination of the previous two choices. It will try to use the right
side, but if there is no room it will switch to using the bottom, which is
usually easier to fit keys into. This setting can be helpful if the size of 
the Emacs frame changes frequently, which might be the case if you are using
a dynamic/tiling window manager.

#+BEGIN_SRC emacs-lisp
(which-key-setup-side-window-right-bottom)
#+END_SRC

*** Minibuffer Option
Take over the minibuffer. For the recommended configuration use 

#+BEGIN_SRC emacs-lisp
(which-key-setup-minibuffer)
#+END_SRC

[[./img/which-key-minibuffer.png]]

Note the maximum height of the minibuffer is controlled through the built-in
variable =max-mini-window-height=.

** Additional Commands
- =which-key-show-top-level= will show most key bindings without a prefix. It is
  most and not all, because many are probably not interesting to most users.
- =which-key-show-next-page= is the command used for paging.
- =which-key-undo= can be used to undo the last keypress when in the middle of a
  key sequence.

** Special Features and Configuration Options
There are more options than the ones described here. All of the configurable
variables are available through =M-x customize-group which-key=.
*** Popup Type Options
There are three different popup types that which-key can use by default to
display the available keys. The variable =which-key-popup-type= decides which
one is used.
**** minibuffer
#+BEGIN_SRC emacs-lisp
(setq which-key-popup-type 'minibuffer)
#+END_SRC
Show keys in the minibuffer.
**** side window
#+BEGIN_SRC emacs-lisp
(setq which-key-popup-type 'side-window)
#+END_SRC
Show keys in a side window. This popup type has further options:
#+BEGIN_SRC emacs-lisp
;; location of which-key window. valid values: top, bottom, left, right, 
;; or a list of any of the two. If it's a list, which-key will always try
;; the first location first. It will go to the second location if there is
;; not enough room to display any keys in the first location
(setq which-key-side-window-location 'bottom)

;; max width of which-key window, when displayed at left or right.
;; valid values: number of columns (integer), or percentage out of current
;; frame's width (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-width 0.33)

;; max height of which-key window, when displayed at top or bottom.
;; valid values: number of lines (integer), or percentage out of current
;; frame's height (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-height 0.25)
#+END_SRC
**** frame

#+BEGIN_SRC emacs-lisp
(setq which-key-popup-type 'frame)
#+END_SRC
Show keys in a popup frame. This popup won't work very well in a terminal,
where only one frame can be shown at any given moment. This popup type has
further options:
#+BEGIN_SRC emacs-lisp
;; max width of which-key frame: number of columns (an integer)
(setq which-key-frame-max-width 60)

;; max height of which-key frame: number of lines (an integer)
(setq which-key-frame-max-height 20)
#+END_SRC

**** custom
Write your own display functions! This requires you to write three functions,
=which-key-custom-popup-max-dimensions-function=,
=which-key-custom-show-popup-function=, and
=which-key-custom-hide-popup-function=. Refer to the documentation for those
variables for more information, but here is a working example (this is the
current implementation of side-window bottom).


#+BEGIN_SRC emacs-lisp
(setq which-key-popup-type 'custom)
(defun which-key-custom-popup-max-dimensions-function (ignore)
  (cons
   (which-key-height-or-percentage-to-height which-key-side-window-max-height)
   (frame-width)))
(defun fit-horizonatally ()
  (let ((fit-window-to-buffer-horizontally t))
    (fit-window-to-buffer)))
(defun which-key-custom-show-popup-function (act-popup-dim)
  (let* ((alist '((window-width . fit-horizontally)
                  (window-height . fit-window-to-buffer))))
    (if (get-buffer-window which-key--buffer)
        (display-buffer-reuse-window which-key--buffer alist)
      (display-buffer-in-major-side-window which-key--buffer 'bottom 0 alist))))
(defun which-key-custom-hide-popup-function ()
  (when (buffer-live-p which-key--buffer)
    (quit-windows-on which-key--buffer)))
#+END_SRC

*** Custom String Replacement Options
    #+NAME: #custom-string-replacement-options
You can customize the way the keys show in the buffer using three different
replacement methods, each of which corresponds replacement alist. The basic idea
of behind each alist is that you specify a selection string in the =car= of each
cons cell and the replacement string in the =cdr=.

**** "Key-Based" replacement
Using this method, the description of a key is replaced using a string that you
provide. Here's an example

#+BEGIN_SRC emacs-lisp
(which-key-add-key-based-replacements
  "C-x C-f" "find files")
#+END_SRC

where the first string is the key combination whose description you want to
replace, in a form suitable for =kbd=. For that key combination, which-key
overwrites the description with the second string, "find files". In the second
type of entry you can restrict the replacements to a major-mode. For example,

#+BEGIN_SRC emacs-lisp
(which-key-add-major-mode-key-based-replacements 'org-mode
  "C-c C-c" "Org C-c C-c"
  "C-c C-a" "Org Attach")
#+END_SRC

Here the first entry is the major-mode followed by a list of the first type of
entries. In case the same key combination is listed under a major-mode and by
itself, the major-mode version takes precedence.

**** Key and Description replacement

The second and third methods target the text used for the keys and the
descriptions directly. The relevant variable is =which-key-replacement-alist=.
Here's an example of one of the default key replacements

#+BEGIN_SRC emacs-lisp
(push '(("<\\([[:alnum:]-]+\\)>" . nil) . ("\\1" . nil))
      which-key-replacement-alist)
#+END_SRC

Each element of the outer cons cell is a cons cell of the form =(KEY
. BINDING)=. The =car= of the outer cons determines how to match key bindings
while the =cdr= determines how those matches are replaced. See the docstring of
=which-key-replacement-alist= for more information.

The next example shows how to replace the description.

#+BEGIN_SRC emacs-lisp
(push '((nil . "left") . (nil . "lft")) which-key-replacement-alist)
#+END_SRC

Here is an example of using key replacement to include Unicode characters in the
results. Unfortunately, using Unicode characters may upset the alignment of the
which-key buffer, because Unicode characters can have different widths even in a
monospace font and alignment is based on character width.

#+BEGIN_SRC emacs-lisp
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))
#+END_SRC

The =cdr= may also be a function that receives a =cons= of the form =(KEY
. BINDING)= and produces a =cons= of the same form. This allows for interesting
ideas like this one suggested by [[https://github.com/pdcawley][@pdcawley]] in [[https://github.com/justbur/emacs-which-key/pull/147][PR #147]].

#+BEGIN_SRC emacs-lisp
(push (cons '(nil . "paredit-mode") 
            (lambda (kb)
              (cons (car kb)
                    (if paredit-mode
                        "[x] paredit-mode"
                      "[ ] paredit-mode"))))
      which-key-replacement-alist)
#+END_SRC

The box will be checked if =paredit-mode= is currently active. 

*** Sorting Options
By default the output is sorted by the key in a custom order. The default order
is to sort lexicographically within each "class" of key, where the classes and
their order are

=Special (SPC, TAB, ...) < Single Character (ASCII) (a, ...) < Modifier (C-, M-, ...) < Other=

You can control the order by setting this variable. This also shows the other
available options.

#+BEGIN_SRC emacs-lisp
;; default
(setq which-key-sort-order 'which-key-key-order)
;; same as default, except single characters are sorted alphabetically
;; (setq which-key-sort-order 'which-key-key-order-alpha)
;; same as default, except all prefix keys are grouped together at the end
;; (setq which-key-sort-order 'which-key-prefix-then-key-order)
;; same as default, except all keys from local maps shown first
;; (setq which-key-sort-order 'which-key-local-then-key-order)
;; sort based on the key description ignoring case
;; (setq which-key-sort-order 'which-key-description-order)
#+END_SRC

*** Paging Options

There are at least several prefixes that have many keys bound to them, like
=C-x=. which-key displays as many keys as it can given your settings, but for
these prefixes this may not be enough. The paging feature gives you the ability
to bind a key to the function =which-key-C-h-dispatch= which will allow you to
cycle through the pages without changing the key sequence you were in the middle
of typing. There are two slightly different ways of doing this.

**** Method 1 (default): Using C-h (or =help-char=)
This is the easiest way, and is turned on by default. Use
#+BEGIN_SRC emacs-lisp
(setq which-key-use-C-h-commands nil)
#+END_SRC
to disable the behavior (this will only take effect after toggling
which-key-mode if it is already enabled). =C-h= can be used with any prefix to
switch pages when there are multiple pages of keys. This changes the default
behavior of Emacs which is to show a list of key bindings that apply to a prefix.
For example, if you were to type =C-x C-h= you would get a list of commands that
follow =C-x=. This uses which-key instead to show those keys, and unlike the
Emacs default saves the incomplete prefix that you just entered so that the next
keystroke can complete the command. 

The commands are:
  - Cycle through the pages forward with =n= (or =C-n=)
  - Cycle backwards with =p= (or =C-p=)
  - Undo the last entered key (!) with =u= (or =C-u=)
  - Call the default command bound to =C-h=, usually =describe-prefix-bindings=,
    with =h= (or =C-h=)

This is especially useful for those who like =helm-descbinds= but also want to
use =C-h= as their which-key paging key.

Note =C-h= is by default equivalent to =?= in this context.

**** Method 2: Bind your own keys

Essentially, all you need to do for a prefix like =C-x= is the following which
will bind =<f5>= to the relevant command.

#+BEGIN_SRC emacs-lisp
(define-key which-key-mode-map (kbd "C-x <f5>") 'which-key-C-h-dispatch)
#+END_SRC

This is completely equivalent to 

#+BEGIN_SRC emacs-lisp
(setq which-key-paging-prefixes '("C-x"))
(setq which-key-paging-key "<f5>")
#+END_SRC

where the latter are provided for convenience if you have a lot of prefixes.

*** Face Customization Options
The faces that which-key uses are
| Face                                   | Applied To                    | Default Definition                                          |
|----------------------------------------+-------------------------------+-------------------------------------------------------------|
| =which-key-key-face=                   | Every key sequence            | =:inherit font-lock-constant-face=                          |
| =which-key-separator-face=             | The separator (→)             | =:inherit font-lock-comment-face=                           |
| =which-key-note-face=                  | Hints and notes               | =:inherit which-key-separator-face=                         |
| =which-key-special-key-face=           | User-defined special keys     | =:inherit which-key-key-face :inverse-video t :weight bold= |
| =which-key-group-description-face=     | Command groups (i.e, keymaps) | =:inherit font-lock-keyword-face=                           |
| =which-key-command-description-face=   | Commands not in local-map     | =:inherit font-lock-function-name-face=                     |
| =which-key-local-map-description-face= | Commands in local-map         | =:inherit which-key-command-description-face=               |

The last two deserve some explanation. A command lives in one of many possible
keymaps. You can distinguish between local maps, which depend on the buffer you
are in, which modes are active, etc., and the global map which applies
everywhere. It might be useful for you to distinguish between the two. One way
to do this is to remove the default face from
=which-key-command-description-face= like this

#+BEGIN_SRC emacs-lisp
  (set-face-attribute 'which-key-command-description-face nil :inherit nil)
#+END_SRC

another is to make the local map keys appear in bold

#+BEGIN_SRC emacs-lisp
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
#+END_SRC

You can also use =M-x customize-face= to customize any of the above faces to
your liking.

*** Other Options
    #+NAME: #other-options
The options below are also available through customize. Their defaults are
shown.

#+BEGIN_SRC emacs-lisp
  ;; Set the time delay (in seconds) for the which-key popup to appear. A value of
  ;; zero might cause issues so a non-zero value is recommended.
  (setq which-key-idle-delay 1.0)

  ;; Set the maximum length (in characters) for key descriptions (commands or
  ;; prefixes). Descriptions that are longer are truncated and have ".." added.
  (setq which-key-max-description-length 27)

  ;; Use additonal padding between columns of keys. This variable specifies the
  ;; number of spaces to add to the left of each column.
  (setq which-key-add-column-padding 0)

  ;; The maximum number of columns to display in the which-key buffer. nil means
  ;; don't impose a maximum.
  (setq which-key-max-display-columns nil)

  ;; Set the separator used between keys and descriptions. Change this setting to
  ;; an ASCII character if your font does not show the default arrow. The second
  ;; setting here allows for extra padding for Unicode characters. which-key uses
  ;; characters as a means of width measurement, so wide Unicode characters can
  ;; throw off the calculation.
  (setq which-key-separator " → " )
  (setq which-key-unicode-correction 3)

  ;; Set the prefix string that will be inserted in front of prefix commands
  ;; (i.e., commands that represent a sub-map).
  (setq which-key-prefix-prefix "+" )

  ;; Set the special keys. These are automatically truncated to one character and
  ;; have which-key-special-key-face applied. Disabled by default. An example
  ;; setting is
  ;; (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (setq which-key-special-keys nil)

  ;; Show the key prefix on the left, top, or bottom (nil means hide the prefix).
  ;; The prefix consists of the keys you have typed so far. which-key also shows
  ;; the page information along with the prefix.
  (setq which-key-show-prefix 'left)

  ;; Set to t to show the count of keys shown vs. total keys in the mode line.
  (setq which-key-show-remaining-keys nil)
#+END_SRC
** Support for Third-Party Libraries
   Some support is provided for third-party libraries which don't use standard
   methods of looking up commands. Some of these need to be enabled
   explicitly. This code includes some hacks, so please report any problems.
*** Key-chord
    Enabled by default.
*** Evil operators
    Evil motions and text objects following an operator like =d= are not all
    looked up in a standard way. Support is controlled through
    =which-key-allow-evil-operators= which should be non-nil if evil is loaded
    before which-key and through =which-key-show-operator-state-maps= which
    needs to be enabled explicitly because it is more of a hack. The former
    allows for the inner and outer text object maps to show, while the latter
    shows motions as well. 
*** God-mode
    Call =(which-key-enable-god-mode-support)= after loading god-mode to enable
    support for god-mode key sequences. This is new and experimental, so please
    report any issues.
** More Examples
*** Nice Display with Split Frame
Unlike guide-key, which-key looks good even if the frame is split into several
windows.
#+CAPTION: which-key in a frame with 3 horizontal splits
[[./img/which-key-right-split.png]]

#+CAPTION: which-key in a frame with 2 vertical splits
[[./img/which-key-bottom-split.png]]

** Thanks
Special thanks to
- [[https://github.com/bmag][@bmag]] for helping with the initial development and finding many bugs.
- [[https://github/iqbalansari][@iqbalansari]] who among other things adapted the code to make
  =which-key-show-top-level= possible.
