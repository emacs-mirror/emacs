;;; mh-xemacs-icons.el --- icons for the MH-E toolbars under XEmacs
;;
;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Various (See below)
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail toolbar
;; See: mh-e.el

;; This file is part of GNU Emacs.

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

;;; Commentary:

;;  This file contains the toolbar icons that MH-E uses under XEmacs. Some
;;  icons were created for MH-E and others were copied from other Emacs modes.
;;  The XPM files are copied into defconst's and the background colour is
;;  changed.

;;  The alist `mh-xemacs-icon-map' contains a map of the icon file names under
;;  GNU Emacs to the constant name under XEmacs. To add new icons for XEmacs
;;  this variable should be updated as well.

;;; Change Log:

;;; Code:

;; Avoid compiler warning
(eval-and-compile
  (require 'mh-utils)
  (defvar mh-xemacs-toolbar-folder-toolbar nil)
  (defvar mh-xemacs-toolbar-letter-toolbar nil))



;; Define the toolbar icons.

;; Derived From lisp/toolbar/mail.xpm
(defconst mh-xemacs-toolbar-inc-folder-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\"  c Gray0\",
\". c #673e666663d4\",
\"X c #a852a7bea3d2\",
\"o c #eb46ea1de471\",
\"O c Gray75 s backgroundToolBarColor\",
/* pixels */
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOO    OOOOOO\",
\"OOOOOOOOO     .ooX OOOOO\",
\"OOOO     .XooooooX OOOOO\",
\"OOO .Xoooooooooo.XX OOOO\",
\"OOO o..ooooooooX.Xo OOOO\",
\"OOO XoX..oooooo.Xoo OOOO\",
\"OOOO oooXX.Xoo...ooX OOO\",
\"OOOO oooooXX..XoX.Xo OOO\",
\"OOOO Xoooo.ooooooo.X OOO\",
\"OOOOO oooXXoooooooo.X OO\",
\"OOOOO ooo.oooooooooX  OO\",
\"OOOOO XoXXooooooX   OOOO\",
\"OOOOOO o.ooooX   OOOOOOO\",
\"OOOOOO .XoX   OOOOOOOOOO\",
\"OOOOOO ..  OOOOOOOOOOOOO\",
\"OOOOOOO  OOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\"};")
  "*MH inc folder icon.")

;; Derived from lisp/toolbar/attach.pbm
(defconst mh-xemacs-toolbar-mime-save-parts-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * file[] = {
\"24 24 2 1\",
\". c Gray75 s backgroundToolBarColor\",
\"  c black\",
/* pixels */
\"........................\",
\"........................\",
\"........................\",
\"...........  ...........\",
\".......... .. ..........\",
\"......... .... .........\",
\"......... .... .........\",
\"......... .... .........\",
\"......... . .. .........\",
\"......... . .. .........\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... . .. . .......\",
\"......... ..  .. .......\",
\".......... .... ........\",
\"........... .. .........\",
\"............  ..........\",
\"........................\",
\"........................\",
\"........................\"};")
  "*MH save MIME parts icon.")

;; Derived from lisp/toolbar/right_arrow.xpm
(defconst mh-xemacs-toolbar-next-undeleted-msg-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * right_arrow_xpm[] = {
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c #020202\",
\"+	c #1A1A1A\",
\"@	c #779D6D\",
\"#	c #88AE80\",
\"$	c #97B78B\",
\"%	c #9EBA92\",
\"&	c #E9EFE8\",
\"*	c #3C5936\",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"      ..                \",
\"      .&..              \",
\"      .&&&..            \",
\"      .&&&&&..          \",
\"      .&&&&&&&..        \",
\"      .&&&&&&&&&+.      \",
\"      +&&&&&&&&&&%..    \",
\"      .%#######@@*..    \",
\"      .%#####@@*..      \",
\"      .%###@@*..        \",
\"      .$#@@*..          \",
\"      .#@*..            \",
\"      .*..              \",
\"      ..                \",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \"};")
  "*MH previous message icon.")

;; Derived from mh-e/page-down.xpm
(defconst mh-xemacs-toolbar-page-msg-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_page_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #ea03ea03d271\",
\"o	c #a5d8a5d89550\",
\"O	c #d305d305bc3c\",
/* pixels */
\"                        \",
\"                        \",
\"   ..................   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XoooooooooooooXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .Xoooooooooo..oXX.   \",
\"   .XXXXXXXXXXX..XXX.   \",
\"   .XXXXXXXXXXX..XXX.   \",
\"   .XooooooXXXX..XXX.   \",
\"   .XXXXXXXXXXX..XXX.   \",
\"   .XXXXXXXXX.O..O.X.   \",
\"   .Xoooooooo.....XX.   \",
\"   .XXXXXXXXXX....XX.   \",
\"   .XXXXXXXXXXX..XXX.   \",
\"   .XXXXXXXXXXXooXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH page message icon.")

;; Derived from lisp/toolbar/left_arrow.xpm
(defconst mh-xemacs-toolbar-previous-undeleted-msg-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * left_arrow_xpm[] = {
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c #020202\",
\"+	c #121A12\",
\"@	c #78A16E\",
\"#	c #86AD7D\",
\"$	c #B2C6AE\",
\"%	c #263222\",
\"&	c #E7EDE6\",
\"*	c #497241\",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                ..      \",
\"              ..$.      \",
\"            ..&&$.      \",
\"          ..&&&&$.      \",
\"        ..&&&&&&$.      \",
\"      .+&&&&&&&&$.      \",
\"    ..$&&&&&&&&&$%      \",
\"    ..**@@@#####@.      \",
\"      ..**@#@###@.      \",
\"        ..**@#@#@.      \",
\"          ..**@@@.      \",
\"            ..*@*.      \",
\"              ..*.      \",
\"                ..      \",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH next message icon.")

;; Derived from lisp/toolbar/close.xpm
(defconst mh-xemacs-toolbar-delete-msg-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 2 1\",
\"  c Gray0\",
\". c Gray75 s backgroundToolBarColor\",
/* pixels */
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"....... ....  ..........\",
\".......  ..    .........\",
\"........  .   ..........\",
\"........     ...........\",
\".........   ............\",
\".........    ...........\",
\"........      ..........\",
\"........  .    .........\",
\".......  ...    ........\",
\"....... .....  .........\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\"};")
  "MH delete message icon.")

;; Derived from mh-e/refile.xpm
(defconst mh-xemacs-toolbar-refile-msg-icon
  (mh-funcall-if-exists toolbar-make-button-list
"/* XPM */
static char * refile_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 7 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #a5d8a5d89550\",
\"o	c #d305d305bc3c\",
\"O	c #ea03ea03d271\",
\"+	c #828282827474\",
\"@	c #61b761b7600a\",
/* pixels */
\"             .          \",
\"           ..X.         \",
\"         ..XoO....      \",
\"       ..XooooO.+.      \",
\"     ..XooooooOX..  ..  \",
\"    .@@ooooooOOO@. ...  \",
\"    .O@oooooOOOOO..@@.  \",
\"    .OO@oooOOOOOO..@@.  \",
\"  ...OO@XooOOOOO...@@.  \",
\" ..+.O@XooOOOO..@@@@@.  \",
\" .++..XooOOOO..@@@@@@.  \",
\" .++.@oooOO...@@@@@@@.  \",
\" ..+.XooOOO..@@@@@@@.   \",
\"  .++.OOOO.@@@@@@@@.    \",
\"   .+.oOO..@@@@@@@.     \",
\"   .++.OO.@@@@@@@.      \",
\"    .++.O.@@@@@..       \",
\"    ..+.O.@@@@@.        \",
\"     .++..@@@@.         \",
\"     ..++.@@@.          \",
\"       .+.@@.           \",
\"       ...@.            \",
\"        ...             \",
\"         .              \"};")
  "MH refile message icon.")

;; Derived from lisp/toolbar/undo.xpm
(defconst mh-xemacs-toolbar-undo-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\"  c Gray0\",
\". c #ae6e66e76a0a\",
\"X c #c6c67d7d8181\",
\"o c #e4e4e4e4dcdc\",
\"O c Gray75 s backgroundToolBarColor\",
/* pixels */
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOO OOOOOOOOOOOOOO\",
\"OOOOOOOO  OOOOOOOOOOOOOO\",
\"OOOOOOO oX   OOOOOOOOOOO\",
\"OOOOOO ooooX.  OOOOOOOOO\",
\"OOOOOOO oo   .. OOOOOOOO\",
\"OOOOOOOO  OOO . OOOOOOOO\",
\"OOOOOOOOO OOOO . OOOOOOO\",
\"OOOOOOOOOOOOOOO  OOOOOOO\",
\"OOOOOOOOOOOOOOO  OOOOOOO\",
\"OOOOOOOOOOOOOOO OOOOOOOO\",
\"OOOOOOOOOOOOOO OOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\"};")
  "MH undo icon.")

;; Derived from mh-e/execute.xpm
(defconst mh-xemacs-toolbar-execute-commands-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_exec_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 6 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #a5d8a5d89550\",
\"o	c #d305d305bc3c\",
\"O	c #ea03ea03d271\",
\"+	c white\",
/* pixels */
\"                        \",
\"                        \",
\"                        \",
\"                 ..     \",
\"             XX  ..     \",
\"         oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX  ..     \",
\"     OO  oo  XX         \",
\"     OO  oo             \",
\"     OO      +   ..     \",
\"             XX  ..     \",
\"         oo  XX         \",
\"     OO  oo             \",
\"     OO                 \",
\"                        \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH execute commands icon.")

;; Derived from mh-e/highlight.xpm
(defconst mh-xemacs-toolbar-toggle-tick-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * highlight_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 4 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #828282827474\",
\"o	c #dd00df007e00\",
/* pixels */
\"                 .....  \",
\"                ..XXX.. \",
\"                .XXXXX. \",
\"               .XXXXX.. \",
\"               .XXXXX.  \",
\"               .XXXXX.  \",
\"              .XXXXX.   \",
\"              .ooXX.    \",
\"              ..ooo.    \",
\"       oooo     ....    \",
\"oo.ooo....oo ...        \",
\"o.o.ooo.oo.o.ooo.o      \",
\".ooo.oo.oo.o.ooooo      \",
\".ooo.oo.oo.o.ooooo      \",
\".ooo.oo...oo.ooooo      \",
\".....oo.oo.o.ooooo      \",
\".ooo.oo.oo.o.ooooo      \",
\".ooo.oo.oo.o.ooo.o      \",
\". oo.o....ooo...o       \",
\"         oo oooo        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH toggle tick icon.")

;; Derived from mh-e/show.xpm
(defconst mh-xemacs-toolbar-toggle-showing-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_show_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 4 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #ea03ea03d271\",
\"o	c #a5d8a5d89550\",
/* pixels */
\"                        \",
\"                        \",
\"   ..................   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XoooooooooooooXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XoooooooooooooXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XooooooXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XoooooooooXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   .XXXXXXXXXXXXXXXX.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH toggle showing icon.")

;; Derived from mh-e/reply-all.xpm
(defconst mh-xemacs-toolbar-reply-all-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * reply_all_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #673e666663d4\",
\"o	c #eb46ea1de471\",
\"O	c #a852a7bea3d2\",
\"+	c #ae51c17b9b26\",
\"@	c #8d4d97577838\",
\"#	c #7c7c8b8b6e6e\",
\"$	c #5e0868be52d3\",
/* pixels */
\"                        \",
\"                        \",
\"              ....      \",
\"         .....XooO.     \",
\"    .....XOooooooO.     \",
\"   .XOooooooooooXOO.    \",
\"   .oXXooooooooOXOo.    \",
\"   .OoOXXooooooXOoo.    \",
\"    .oooOOXOooXXXooO.   \",
\"    ........XXOoOXOo.   \",
\"    ..++++@.ooooooXO.   \",
\"     ..+@@@.oooooooXO.  \",
\"    ..+@@@#.oooooooO..  \",
\"  ..++@@@#$.ooooO...    \",
\" .++++@@#.$    ..       \",
\"  .+@@@#.o  ..   .O .O  \",
\"   .+@#$.   .O.  .O .O  \",
\"    .#$.   .O .o .O .O  \",
\"     .$.   .  .O .O .O  \",
\"      .    ....O .O .O  \",
\"           .O .O .O .O  \",
\"           .O .O .O .O  \",
\"           .O .O .O .O  \",
\"                        \"};")
  "Reply to \"All\" icon.")

;; Derived from mh-e/reply-from.xpm
(defconst mh-xemacs-toolbar-reply-from-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * reply_from_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #673e666663d4\",
\"o	c #eb46ea1de471\",
\"O	c #a852a7bea3d2\",
\"+	c #ae51c17b9b26\",
\"@	c #8d4d97577838\",
\"#	c #7c7c8b8b6e6e\",
\"$	c #5e0868be52d3\",
/* pixels */
\"                        \",
\"                        \",
\"              ....      \",
\"         .....XooO.     \",
\"    .....XOooooooO.     \",
\"   .XOooooooooooXOO.    \",
\"   .oXXooooooooOXOo.    \",
\"   .OoOXXooooooXOoo.    \",
\"    .oooOOXOooXXXooO.   \",
\"    ........XXOoOXOo.   \",
\"    ..++++@.ooooooXO.   \",
\"     ..+@@@.oooooooXO.  \",
\"    ..+@@@#.oooooooO..  \",
\"  ..++@@@#$.ooooO...    \",
\"        #.$.oO...       \",
\"   ...O . ....          \",
\"   ...O                 \",
\"   .O                   \",
\"   ...O ..O .... .O O.  \",
\"   ...O ..O .OO. .....  \",
\"   .O   .O  .  . . . .  \",
\"   .O   .O  .OO. . . .  \",
\"   .O   .O  .... . O .  \",
\"                        \"};")
  "Reply to \"From\" icon..")

;; Derived from mh-e/reply-to.xpm
(defconst mh-xemacs-toolbar-reply-to-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * reply_to_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #673e666663d4\",
\"o	c #eb46ea1de471\",
\"O	c #a852a7bea3d2\",
\"+	c #ae51c17b9b26\",
\"@	c #8d4d97577838\",
\"#	c #7c7c8b8b6e6e\",
\"$	c #5e0868be52d3\",
/* pixels */
\"                        \",
\"                        \",
\"              ....      \",
\"         .....XooO.     \",
\"    .....XOooooooO.     \",
\"   .XOooooooooooXOO.    \",
\"   .oXXooooooooOXOo.    \",
\"   .OoOXXooooooXOoo.    \",
\"    .oooOOXOooXXXooO.   \",
\"    ........XXOoOXOo.   \",
\"    ..++++@.ooooooXO.   \",
\"     ..+@@@.oooooooXO.  \",
\"    ..+@@@#.oooooooO..  \",
\"  ..++@@@#$.ooooO...    \",
\" .++++@@#.$             \",
\"  .+@@@#.o  ......      \",
\"   .+@#$.   OO.OOO      \",
\"    .#$.      .O        \",
\"     .$.      .O  ....  \",
\"      .       .O  .OO.  \",
\"              .O  .  .  \",
\"              .O  .OO.  \",
\"              .O  ....  \",
\"                        \"};")
  "Reply to \"To\" icon..")

;; Derived from mh-e/mail/reply2.xpm
(defconst mh-xemacs-toolbar-reply-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_reply_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #673e666663d4\",
\"o	c #eb46ea1de471\",
\"O	c #a852a7bea3d2\",
\"+	c #ae51c17b9b26\",
\"@	c #8d4d97577838\",
\"#	c #7c7c8b8b6e6e\",
\"$	c #5e0868be52d3\",
/* pixels */
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"              ....      \",
\"         .....XooO.     \",
\"    .....XOooooooO.     \",
\"   .XOooooooooooXOO.    \",
\"   .oXXooooooooOXOo.    \",
\"   .OoOXXooooooXOoo.    \",
\"    .oooOOXOooXXXooO.   \",
\"    ........XXOoOXOo.   \",
\"    ..++++@.ooooooXO.   \",
\"     ..+@@@.oooooooXO.  \",
\"    ..+@@@#.oooooooO..  \",
\"  ..++@@@#$.ooooO...    \",
\" .++++@@#.$.oO...       \",
\"  .+@@@#.o....          \",
\"   .+@#$...             \",
\"    .#$.                \",
\"     .$.                \",
\"      .                 \",
\"                        \"};")
  "Reply to current message icon.")

;; Derived from mh-e/alias.xpm
(defconst mh-xemacs-toolbar-alias-grab-from-field-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * alias_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 4 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c #61b761b7600a\",
\"X	c #a5d8a5d89550\",
\"o	c black\",
/* pixels */
\"                        \",
\"                        \",
\"                        \",
\"     ......             \",
\"   ...XXXX..XX          \",
\"  o..ooooooo...         \",
\" ooo      oooo..X       \",
\" o.X        ooo...      \",
\" o.X          ooo.XX    \",
\" o.X            oo..    \",
\" o.X             oo.    \",
\" o...            oo..   \",
\"  o.X             o..   \",
\"  o.XX            oX.   \",
\"   o....          oo.   \",
\"    o..XX        oooo   \",
\"     o...XXX   XXoooo   \",
\"      ooo........ooooo  \",
\"        oooooXXooooo.oo \",
\"            ooo    o..oo\",
\"                    o...\",
\"                     ooo\",
\"                      oo\",
\"                        \"};")
  "MH alias grab from field icon.")

;; Derived from toolbar/mail_send.xpm
(defconst mh-xemacs-toolbar-send-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\"  c Gray0\",
\". c #757560602020\",
\"X c #6711662663d9\",
\"o c #8e8e7d7d4545\",
\"O c #adad8e8e3030\",
\"+ c #d8d8bebe6a6a\",
\"@ c #a8fba84da483\",
\"# c #eb79ea70e4f4\",
\"$ c Gray75 s backgroundToolBarColor\",
/* pixels */
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$    $$$$$$$\",
\"$$$$$$$$     X##@ $$$$$$\",
\"$$$     X@######@ $$$$$$\",
\"$$ X@##########X@@ $$$$$\",
\"$$ #XX########@X@# $$$$$\",
\"$$ @#@XX######X@## $$$$$\",
\"$$$ ###@@X@##XXX##@ $ $$\",
\"$$$ #####@@XX@#@X@#  + $\",
\"$$$ @####X#######X@ +o $\",
\"$$$$ ###@@######## +o $$\",
\"$$$$ ###X######## +o $$$\",
\"$$$$ @#@@######@ +o $$$$\",
\"$$$$$ #X####@   +o $$$$$\",
\"$$$$$ X@#@   $ +o $$$$$$\",
\"$$$$$ XX  $$$ +o $$$$$$$\",
\"$$$$$$  $$$$ +o $$$$$$$$\",
\"$$$$$$$$$$$O.  $$$$$$$$$\",
\"$$$$$$$$$$$  $$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\"};")
  "MH send icon.")

;; Derived from mh-e/rescan.xpm
(defconst mh-xemacs-toolbar-rescan-folder-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_rescan_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 6 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #a5d8a5d89550\",
\"o	c #d305d305bc3c\",
\"O	c #ea03ea03d271\",
\"+	c #828282827474\",
/* pixels */
\"                        \",
\"                        \",
\"       ..............   \",
\"      .XXXXXXXXXXXX..   \",
\"     .XXXXXXXXXXXX.X.   \",
\"    .XXXXXXXXXXXX.oo.   \",
\"   ..............ooo.   \",
\"   .OOOOOOOOOOOO.ooo.   \",
\"   .O++++++++++O.ooo.   \",
\"   .O+XXXXXXXX+O.ooo.   \",
\"   .O+XXXXXXXX+O.ooo.   \",
\"   .O+XXXXXXXX+O.ooo.   \",
\"   .O+XXXXXXXX+O.ooo.   \",
\"   .O++++++++++O.ooo.   \",
\"   .OOOOOOOOOOOO.ooo.   \",
\"   .O++++++++++O.ooo.   \",
\"   .O+XXXXXXXX+O.ooo.   \",
\"   .O+XXXXXXXX+O.ooX.   \",
\"   .O+XXXXXXXX+O.oo..   \",
\"   .O++++++++++O.o..    \",
\"  ..OOOOOOOOOOOO...     \",
\"  ................      \",
\"                        \",
\"                        \"};")
  "MH rescan folder icon.")

;; Derived from mh-e/repack.xpm
(defconst mh-xemacs-toolbar-pack-folder-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * mail_repack_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 6 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black\",
\"X	c #a5d8a5d89550\",
\"o	c #d305d305bc3c\",
\"O	c #ea03ea03d271\",
\"+	c #828282827474\",
/* pixels */
\"                        \",
\"                        \",
\"       ..............   \",
\"      .XXXXXXXXXXXX..   \",
\"     .XXXXXXXXXXXX.X.   \",
\"    .XXXXXXXXXXXX.oo.   \",
\"   ..............ooo.   \",
\"   .OOOOOOOOOOOO.oo.    \",
\"   .O++++++++++O.oo.    \",
\"   .O+XXXXXXXX+O.o.     \",
\"    .+XXXXXXXX+.o..     \",
\"    .+XX...XXX+....     \",
\"     ....o.......oo.    \",
\"     ....o.....Oooo.    \",
\"    .OOO...OOOO.oooo.   \",
\"    .++++++++++.oooo.   \",
\"    .+XXXXXXXX+.oooo.   \",
\"   .O+XXXXXXXX+O.ooX.   \",
\"   .O+XXXXXXXX+O.oo..   \",
\"   .O++++++++++O.o..    \",
\"  ..OOOOOOOOOOOO...     \",
\"  ................      \",
\"                        \",
\"                        \"};")
  "MH repack folder icon.")

;; Derived from lisp/toolbar/search.xpm
(defconst mh-xemacs-toolbar-search-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 8 1\",
\"  c #011801180102\",
\". c #464646463e3e\",
\"X c #5c5c5c5c57a0\",
\"o c #878787877979\",
\"O c #a910a91097af\",
\"+ c #ce5ace5ab851\",
\"@ c #e79de79dd134\",
\"# c Gray75 s backgroundToolBarColor\",
/* pixels */
\"########################\",
\"########################\",
\"############# ##########\",
\"###########  O #########\",
\"#########  O@@.#########\",
\"#######  O@@@@@ ########\",
\"#####  O+@@@@@@O #######\",
\"#### XX@++@@@@@@.#######\",
\"#### @.O+@@@@@@@@ ######\",
\"#### @@.++@@@@@@@O #####\",
\"#### @@.o+O.  .+@@ #####\",
\"#### @XO+O.O++o.+@@ ####\",
\"####  O+@.O@@+Oo.@@+ ###\",
\"#### X@@@ +#+OOO @@@@ ##\",
\"#### O@@@ +@OOOo @@@o ##\",
\"##### @@@.oOOOoX.@@  ###\",
\"##### O@@O.oOOX  @ #####\",
\"######X@@@O.  .X  ######\",
\"###### @@@@@@@+    #####\",
\"####### @@@@@O ##   ####\",
\"####### O@@+. ####   ###\",
\"######## @O #######  ###\",
\"#########  #############\",
\"########################\"};")
  "MH search icon.")

;; Derived from lisp/toolbar/fld_open.xpm
(defconst mh-xemacs-toolbar-visit-folder-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 4 1\",
\"  c Gray0\",
\". c #909090909090\",
\"X c #fefefefefefe\",
\"o c Gray75 s backgroundToolBarColor\",
/* pixels */
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooo  oooooooo\",
\"ooooooooooo   .. ooooooo\",
\"oooo  oo   ....XXo   ooo\",
\"ooo ..  ....XXXX  .. ooo\",
\"ooo .....XXXXX  .... ooo\",
\"oooo ..XXXXX  ...... ooo\",
\"oooo ..XXX  ........ ooo\",
\"ooooo .XX .......... ooo\",
\"ooooo ..X .......... ooo\",
\"oooooo .X .......... ooo\",
\"oooooo .. ........  oooo\",
\"ooooooo . ......  oooooo\",
\"ooooooo . ..... oooooooo\",
\"oooooooo  ...  ooooooooo\",
\"oooooooo  .  ooooooooooo\",
\"ooooooooo  ooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\",
\"oooooooooooooooooooooooo\"};")
  "MH visit folder icon.")

;; Derived from lisp/toolbar/help.xpm
(defconst mh-xemacs-toolbar-help-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 6 1\",
\"  c Gray0\",
\". c #65658b8b5e5e\",
\"X c #934ab2448dfb\",
\"o c #b35dc8c8afaf\",
\"O c #e0b2e944df83\",
\"+ c Gray75 s backgroundToolBarColor\",
/* pixels */
\"++++++++++++++++++++++++\",
\"++++++++++++++++++++++++\",
\"++++++++++++++++++++++++\",
\"++++++++++++++++++++++++\",
\"+++++++++     ++++++++++\",
\"++++++++ oOOOO +++++++++\",
\"+++++++ OOOOOOO ++++++++\",
\"++++++ oOo   oOo +++++++\",
\"+++++++ O +++ OO +++++++\",
\"+++++++O ++++ Oo +++++++\",
\"++++++++++++ OO. +++++++\",
\"+++++++++++ OOX ++++++++\",
\"++++++++++ OOX +++++++++\",
\"+++++++++ XOX ++++++++++\",
\"+++++++++ OX +++++++++++\",
\"+++++++++    +++++++++++\",
\"++++++++++++++++++++++++\",
\"++++++++++  ++++++++++++\",
\"+++++++++ Oo +++++++++++\",
\"+++++++++ oX +++++++++++\",
\"++++++++++  ++++++++++++\",
\"++++++++++++++++++++++++\",
\"++++++++++++++++++++++++\",
\"++++++++++++++++++++++++\"};")
  "MH help icon.")

;; Derived from lisp/toolbar/mail_send.xpm
(defconst mh-xemacs-toolbar-send-letter-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 9 1\",
\"  c Gray0\",
\". c #675e6580613e\",
\"X c #8c8c7c7c6969\",
\"o c #9b458d377822\",
\"O c #a941a6459f3e\",
\"+ c #c8c8b2b29898\",
\"@ c #dadac2c2a5a5\",
\"# c #eb4dea2fe4ad\",
\"$ c Gray75 s backgroundToolBarColor\",
/* pixels */
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\",
\"$$$$$$$$$$$$$    $$$$$$$\",
\"$$$$$$$$     .@#+ $$$$$$\",
\"$$$     .+#####@O $$$$$$\",
\"$$ .+##########.+O $$$$$\",
\"$$ @..########O.+# $$$$$\",
\"$$ O@O..@#####.+## $$$$$\",
\"$$$ ###+O.O##...##O $$$$\",
\"$$$ @####@+..O#O.+# $$$$\",
\"$$$ O####.#######.O $$$$\",
\"$$$$ ###+O########.O $$$\",
\"$$$$ ###.########@O  $$$\",
\"$$$$ +#+O#####@O   $$$$$\",
\"$$$$$ #.###@O     $$$$$$\",
\"$$$$$ .O@O   $$ .. $$$$$\",
\"$$$$$ ..  $$$$ .oo. $$$$\",
\"$$$$$$  $$$$$   oo   $$$\",
\"$$$$$$$$$$$$$$$ Oo $$$$$\",
\"$$$$$$$$$$$$$$ oOOX $$$$\",
\"$$$$$$$$$$$$$$ ++++ $$$$\",
\"$$$$$$$$$$$$$ O@@@@O $$$\",
\"$$$$$$$$$$$$$        $$$\",
\"$$$$$$$$$$$$$$$$$$$$$$$$\"};")
  "MH send letter icon.")

;; This is the same icon as `mh-xemacs-toolbar-mime-save-parts-icon',
;; so there is no point in duplicating it.
(defconst mh-xemacs-toolbar-compose-insertion-icon
  mh-xemacs-toolbar-mime-save-parts-icon
  "MH compose insertion icon.")

;; Derived from lisp/toolbar/spell.xpm
(defconst mh-xemacs-toolbar-ispell-message-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\"  c Gray0\",
\". c #41415b5b3939\",
\"X c #4c2f6b4e42d1\",
\"o c #5fe086865454\",
\"O c Gray75 s backgroundToolBarColor\",
/* pixels */
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOO  OO   OOO  OOOOOOOO\",
\"OOO OO O OO O OO OOOOOOO\",
\"OOO    O   OO OOOOOOOOOO\",
\"OOO OO O OO O OO OOOOOOO\",
\"OOO OO O   OOO  OOOO  OO\",
\"OOOOOOOOOOOOOOOOOOO  OOO\",
\"OOOOOOOOOOO  OOOOO  OOOO\",
\"OOOOOOOOOOO X OOO . OOOO\",
\"OOOOOOOOOOOO X O X OOOOO\",
\"OOOOOOOOOOOO Xo o. OOOOO\",
\"OOOOOOOOOOOOO XoX OOOOOO\",
\"OOOOOOOOOOOOO Xo. OOOOOO\",
\"OOOOOOOOOOOOOO X OOOOOOO\",
\"OOOOOOOOOOOOOO X OOOOOOO\",
\"OOOOOOOOOOOOOOO OOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\"};")
  "MH Ispell message icon.")

;; Derived from lisp/toolbar/save.xpm
(defconst mh-xemacs-toolbar-save-buffer-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\"  c #01be01be01be\",
\". c #62dd62dd62dd\",
\"X c Gray62\",
\"o c #e625e625e625\",
\"O c Gray75 s backgroundToolBarColor\",
/* pixels */
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOO  OOOOOOOOO\",
\"OOOOOOOOOOO  X. OOOOOOOO\",
\"OOOOOOOOO  oXoX OOOOOOOO\",
\"OOOOOOO  oXoooXX OOOOOOO\",
\"OOOOO  oXoooooo. OOOOOOO\",
\"OOO  XoooooooooX  OOOOOO\",
\"OO XooooooooooooX OOOOOO\",
\"OO .XoooooooooooX. OOOOO\",
\"OOO XooooooooooXXX OOOOO\",
\"OOO .XoooooooXX..X. OOOO\",
\"OOOO XoooooXX...X.X OOOO\",
\"OOOO .XooXX.Xoo.X.X. OOO\",
\"OOOOO XXX.oooooX.X.  OOO\",
\"OOOOO .XXoo.ooooXX   OOO\",
\"OOOOOO XX.o XooX.  OOOOO\",
\"OOOOOO .XXooXoX  OOOOOOO\",
\"OOOOOOO .X.oX  OOOOOOOOO\",
\"OOOOOOOO     OOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\"};")
  "MH save buffer icon.")

;; Derived from lisp/toolbar/cut.xpm
(defconst mh-xemacs-toolbar-kill-region-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 2 1\",
\"  c Gray0\",
\". c Gray75 s backgroundToolBarColor\",
/* pixels */
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\"........................\",
\".................. .....\",
\"................  ......\",
\"...............  .......\",
\"..............  ........\",
\".............  .........\",
\"....    ....  .....   ..\",
\"... ...  ..  ...    ....\",
\"... ...           ......\",
\"....    ...    .........\",
\"..........  ............\",
\".........   ............\",
\"........ .. ............\",
\"....... ... ............\",
\"....... .. .............\",
\".......   ..............\",
\"........................\",
\"........................\",
\"........................\",
\"........................\"};")
  "MH kill region icon.")

;; Derived from lisp/toolbar/copy.xpm
(defconst mh-xemacs-toolbar-kill-ring-save-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 7 1\",
\"  c Gray0\",
\". c #424242423a3a\",
\"X c #68e968e96363\",
\"o c #a8b1a8b1992b\",
\"O c #d3d3d3d3bdbd\",
\"+ c #e419e419cd6b\",
\"@ c Gray75 s backgroundToolBarColor\",
/* pixels */
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@  @@@@@@@@@@@@@@\",
\"@@@@@@  Oo @@@@@@@@@@@@@\",
\"@@@@ .ooOO @@@@  @@@@@@@\",
\"@@@@ +XoOOo @  Oo @@@@@@\",
\"@@@@ +.oO++ .ooOO @@@@@@\",
\"@@@@ XoO+++ +XoOOo @@@@@\",
\"@@@@ oOO+++ +.oO++ @@@@@\",
\"@@@@ oO++++ XoOO++o @@@@\",
\"@@@@@ +++++ oOO++++o @@@\",
\"@@@@@ o++++ oO++++++ @@@\",
\"@@@@@@ ++o   +++++++o @@\",
\"@@@@@@ o  @@ o++++o  @@@\",
\"@@@@@@@ @@@@@ ++o  @@@@@\",
\"@@@@@@@@@@ @@ o  @@@@@@@\",
\"@@@@@@@     @@ @@@@@@@@@\",
\"@@@@@@@     @@@@@@@@@@@@\",
\"@@@@@@@@@@ @@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\",
\"@@@@@@@@@@@@@@@@@@@@@@@@\"};")
  "MH kill ring save icon.")

;; Derived from lisp/toolbar/paste.xpm
(defconst mh-xemacs-toolbar-yank-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"24 24 5 1\",
\"  c Gray0\",
\". c #62ee62ee62ee\",
\"X c Gray68\",
\"o c Gray82\",
\"O c Gray75 s backgroundToolBarColor\",
/* pixels */
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOO   OOOOOOOOOOOO\",
\"OOOOOOO  ooo OOOOOO OOOO\",
\"OOOOO  ooooo OOO     OOO\",
\"OOO  oo. .Xoo OO     OOO\",
\"OO ooo.oX..oo OOOOO OOOO\",
\"OO ooo.X..oooo OOOOOOOOO\",
\"OOO oo..Xooooo O  OOOOOO\",
\"OOO oooooooooo  oX OOOOO\",
\"OOOO ooooooo  XXoo OOOOO\",
\"OOOO ooooooo o.XooX OOOO\",
\"OOOOO oooooo o.Xooo OOOO\",
\"OOOOO oooooo .XooooX OOO\",
\"OOOOOO ooooX XooooooX OO\",
\"OOOOOO XXOXX Xooooooo OO\",
\"OOOOOOO XXXX  oooooooX O\",
\"OOOOOOO XX  O XooooX  OO\",
\"OOOOOOOO  OOOO ooX  OOOO\",
\"OOOOOOOOOOOOOO X  OOOOOO\",
\"OOOOOOOOOOOOOOO OOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\",
\"OOOOOOOOOOOOOOOOOOOOOOOO\"};")
  "MH yank icon.")

;; This is the same icon as `mh-xemacs-toolbar-delete-msg-icon'
;; so there is no point in duplicating it here.
(defconst mh-xemacs-toolbar-fully-kill-draft-icon
  mh-xemacs-toolbar-delete-msg-icon
  "MH fully kill draft icon.")

;; Derived from lisp/toolbar/preferences.xpm
(defconst mh-xemacs-toolbar-preferences-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * preferences_xpm[] = {
\"24 24 8 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c #000000\",
\"+	c #E1E0E0\",
\"@	c #D7C99B\",
\"#	c #9A6C4E\",
\"$	c #A4A199\",
\"%	c #858579\",
\"&	c #AD8E30\",
\"                        \",
\"                        \",
\"                        \",
\"             ..         \",
\"           ..++.     .  \",
\"         ..++++.    .@. \",
\"      ...+++++++.  .@#. \",
\"    ..++++++++++. .@#.  \",
\"   .++++++#++++++.@#.   \",
\"   .+++++#++++++.@#.    \",
\"    .++#+#+++++.@#.     \",
\"    .++#$#++++.@#.+.    \",
\"     .++##+++.@#.++@.   \",
\"     .++++++.@#.+++@%.  \",
\"      .++++&+..@$$$$%.  \",
\"      .++++..$$$$$$@.   \",
\"       .+$%%$+++++..    \",
\"       .+++++++++.      \",
\"        .++++++..       \",
\"        .++++@.         \",
\"         .++..          \",
\"          ..            \",
\"                        \",
\"                        \"};")
  "MH preferences icon.")

;; This is the same icon as `mh-xemacs-toolbar-help-icon' so there is
;; no point in duplicating it here.
(defconst mh-xemacs-toolbar-letter-help-icon
  mh-xemacs-toolbar-help-icon
  "MH letter help icon.")

;; Derived from mh-e/widen.xpm
(defconst mh-xemacs-toolbar-widen-icon
  (mh-funcall-if-exists toolbar-make-button-list
   "/* XPM */
static char * widen_xpm[] = {
/* columns rows colors chars-per-pixel */
\"24 24 3 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c #8d4d97577838\",
\"X	c black\",
/* pixels */
\"                        \",
\"                        \",
\"                        \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .   XX        XX   .  \",
\"  .  XX          XX  .  \",
\"  . XX            XX .  \",
\"  .XXXXXXXX  XXXXXXXX.  \",
\"  .XXXXXXXX  XXXXXXXX.  \",
\"  . XX            XX .  \",
\"  .  XX          XX  .  \",
\"  .   XX        XX   .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"  .                  .  \",
\"                        \",
\"                        \",
\"                        \"};")
  "MH widen icon.")

(defvar mh-xemacs-icon-map
  '((mail . mh-xemacs-toolbar-inc-folder-icon)
    (attach . mh-xemacs-toolbar-mime-save-parts-icon)
    (right_arrow . mh-xemacs-toolbar-next-undeleted-msg-icon)
    (page-down . mh-xemacs-toolbar-page-msg-icon)
    (left_arrow . mh-xemacs-toolbar-previous-undeleted-msg-icon)
    (close . mh-xemacs-toolbar-delete-msg-icon)
    (refile . mh-xemacs-toolbar-refile-msg-icon)
    (undo . mh-xemacs-toolbar-undo-icon)
    (execute . mh-xemacs-toolbar-execute-commands-icon)
    (highlight . mh-xemacs-toolbar-toggle-tick-icon)
    (show . mh-xemacs-toolbar-toggle-showing-icon)
    (reply-from . mh-xemacs-toolbar-reply-from-icon)
    (reply-to . mh-xemacs-toolbar-reply-to-icon)
    (reply-all . mh-xemacs-toolbar-reply-all-icon)
    (mail/reply2 . mh-xemacs-toolbar-reply-icon)
    (alias . mh-xemacs-toolbar-alias-grab-from-field-icon)
    (mail_compose . mh-xemacs-toolbar-send-icon)
    (rescan . mh-xemacs-toolbar-rescan-folder-icon)
    (repack . mh-xemacs-toolbar-pack-folder-icon)
    (search . mh-xemacs-toolbar-search-icon)
    (fld_open . mh-xemacs-toolbar-visit-folder-icon)
    (mail_send . mh-xemacs-toolbar-send-letter-icon)
    (spell . mh-xemacs-toolbar-ispell-message-icon)
    (save . mh-xemacs-toolbar-save-buffer-icon)
    (cut . mh-xemacs-toolbar-kill-region-icon)
    (copy . mh-xemacs-toolbar-kill-ring-save-icon)
    (paste . mh-xemacs-toolbar-yank-icon)
    (preferences . mh-xemacs-toolbar-preferences-icon)
    (help . mh-xemacs-toolbar-help-icon)
    (widen . mh-xemacs-toolbar-widen-icon))
  "Map GNU Emacs icon file names to XEmacs image constants.")



(provide 'mh-xemacs-icons)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 5b06d860-a468-4a0f-a61b-255a148985e4
;;; mh-xemacs-icons.el ends here
