## Move
| Command                                                                                                       | Key bindings  | Documentation                                         |
|---------------------------------------------------------------------------------------------------------------|---------------|-------------------------------------------------------|
| [next-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L66)                          | C-n, Down     | Move the cursor to next line.                         |
| [next-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L73)                  |               | Move the cursor to the next logical line.             |
| [previous-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L80)                      | C-p, Up       | Move the cursor to the previous line.                 |
| [previous-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L84)              |               | Move the cursor to the previous logical line.         |
| [forward-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L88)                       | C-f, Right    | Move the cursor to the next character.                |
| [backward-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L94)                      | C-b, Left     | Move the cursor to the previous character.            |
| [move-to-beginning-of-buffer](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L99)        | M-<, C-Home   | Move the cursor to the beginning of the buffer.       |
| [move-to-end-of-buffer](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L104)             | M->, C-End    | Move the cursor to the end of the buffer.             |
| [move-to-beginning-of-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L109)         | C-a, Home     | Move the cursor to the beginning of the line.         |
| [move-to-beginning-of-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L125) |               | Move the cursor to the beginning of the logical line. |
| [move-to-end-of-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L129)               | C-e, End      | Move the cursor to the end of the line.               |
| [move-to-end-of-logical-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L135)       |               | Move the cursor to the end of the logical line.       |
| [next-page](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L139)                         | C-v, PageDown | Move the cursor to the next page by one page.         |
| [previous-page](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L147)                     | M-v, PageUp   | Move the cursor to the previous page by one page.     |
| [next-page-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L155)                    | C-x ]         | Move the cursor to the next page character (^L).      |
| [previous-page-char](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L165)                | C-x [         | Move the cursor to the previous page character (^L).  |
| [goto-line](https://github.com/lem-project/lem/blob/main/src/commands/move.lisp#L169)                         | M-g           | Move the cursor to the specified line number.         |

## Edit
| Command                                                                                                | Key bindings      | Documentation                                                                        |
|--------------------------------------------------------------------------------------------------------|-------------------|--------------------------------------------------------------------------------------|
| [self-insert](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L73)                 |                   | Processes the key entered.                                                           |
| [newline](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L91)                     | Return            | Insert a new line.                                                                   |
| [open-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L95)                   | C-o               | Insert a new line without moving the cursor position.                                |
| [quoted-insert](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L99)               | C-q               | Insert the next entered key (including control characters).                          |
| [delete-next-char](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L114)           | C-d, Delete       | Delete the next character.                                                           |
| [delete-previous-char](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L137)       | C-h, Backspace    | Delete the previous character.                                                       |
| [copy-region](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L151)                | M-w               | Copy the text of region.                                                             |
| [copy-region-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L157)   |                   | Copy the selected text to the clipboard.                                             |
| [kill-region](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L169)                | C-w               | Kill the text of region.                                                             |
| [kill-region-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L178)   |                   | Kill the text of region and copy to the clipboard.                                   |
| [kill-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L183)                  | C-k               | Kill from the current cursor position to the end of the line.                        |
| [kill-whole-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L215)            | C-Shift-Backspace | Kill the entire line and the remaining whitespace                                    |
| [yank](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L234)                       | C-y               | Paste the copied text.                                                               |
| [yank-pop](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L238)                   | M-y               | Replaces the immediately pasted text with the next text in the killring.             |
| [yank-pop-next](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L251)              |                   | Replaces the immediately preceding yank-pop text with the text before the kill ring. |
| [yank-to-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L264)          |                   | Copy the text of the killring to the clipboard.                                      |
| [paste-from-clipboard](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L271)       |                   | Inserts text from the clipboard.                                                     |
| [entab-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L289)                 |                   | Replaces the indent of the current line from space to tab.                           |
| [detab-line](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L295)                 |                   | Replaces the indent of the current line from tab to space.                           |
| [delete-blank-lines](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L302)         | C-x C-o           | Delete blank lines before and after the cursor.                                      |
| [just-one-space](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L326)             | M-Space           | Combines consecutive whitespace before and after the cursor into one.                |
| [delete-indentation](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L332)         | M-^               | Merge the current line with the previous line.                                       |
| [transpose-characters](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L352)       | C-t               | Swaps the characters before and after the cursor.                                    |
| [undo](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L369)                       | C-\               | Undo.                                                                                |
| [redo](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L376)                       | C-_, C-/          | Redo.                                                                                |
| [delete-trailing-whitespace](https://github.com/lem-project/lem/blob/main/src/commands/edit.lisp#L403) |                   | Removes all end-of-line and end-of-buffer whitespace from the current buffer.        |

## Mark
| Command                                                                                          | Key bindings | Documentation                                                  |
|--------------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------|
| [mark-set](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L15)              | C-@, C-Space | Sets a mark at the current cursor position.                    |
| [exchange-point-mark](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L21)   | C-x C-x      | Exchange the current cursor position with the marked position. |
| [mark-set-whole-buffer](https://github.com/lem-project/lem/blob/main/src/commands/mark.lisp#L29) | C-x h        | Select the whole buffer as a region.                           |

## Word
| Command                                                                                          | Key bindings                    | Documentation                                             |
|--------------------------------------------------------------------------------------------------|---------------------------------|-----------------------------------------------------------|
| [forward-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L84)          | M-f, C-Right                    | Move to cursor to next word.                              |
| [previous-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L88)         | M-b, C-Left                     | Move to cursor to previous word                           |
| [delete-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L92)           | M-d, C-Delete                   | Delete the next word.                                     |
| [backward-delete-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L106) | M-C-h, M-Backspace, C-Backspace | Delete the previous word.                                 |
| [downcase-region](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L137)      | C-x C-l                         | Replaces the selected region with a downcase.             |
| [uppercase-region](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L141)     | C-x C-u                         | Replaces the selected region with a uppercase.            |
| [capitalize-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L162)      | M-c                             | Replace the following word with capital-case.             |
| [lowercase-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L166)       | M-l                             | Replace the following word with lowercase.                |
| [uppercase-word](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L170)       | M-u                             | Replace the following word with uppercase.                |
| [forward-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L174)    | M-}                             | Move cursor to forward paragraph.                         |
| [backward-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L187)   | M-{                             | Move cursor to backward paragraph.                        |
| [kill-paragraph](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L191)       | M-k                             | Kill the forward paragraph.                               |
| [count-words](https://github.com/lem-project/lem/blob/main/src/commands/word.lisp#L207)          | M-=                             | Count the number of lines/words/characters in the buffer. |

## S-Expression
| Command                                                                                            | Key bindings     | Documentation                                     |
|----------------------------------------------------------------------------------------------------|------------------|---------------------------------------------------|
| [forward-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L30)    | M-C-f            | Move the cursor to the forward expression.        |
| [backward-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L41)   | M-C-b            | Move the cursor to the backward expression.       |
| [forward-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L45)    | M-C-n            | Move the cursor to the forward list.              |
| [backward-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L49)   | M-C-p            | Move the cursor to the backward list.             |
| [down-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L53)       | M-C-d            | Move the cursor to the inner expression.          |
| [up-list](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L57)         | M-C-u            | Move the cursor to the outer expression.          |
| [mark-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L62)       | M-C-@, M-C-Space | Select the forward expression as a region.        |
| [kill-sexp](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L72)       | M-C-k            | Kill the forward expression as a region.          |
| [transpose-sexps](https://github.com/lem-project/lem/blob/main/src/commands/s-expression.lisp#L81) | M-C-t            | Swaps the expression before and after the cursor. |

## File
| Command                                                                                           | Key bindings | Documentation                                                                                                                                                                                                       |
|---------------------------------------------------------------------------------------------------|--------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [find-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L69)              | C-x C-f      | Open the file.                                                                                                                                                                                                      |
| [find-file-recursively](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L212) |              | Open a file, from the list of all files present under the buffer's directory, recursively.                                                                                                                          |
| [read-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L228)             | C-x C-r      | Open the file as a read-only.                                                                                                                                                                                       |
| [save-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L266)   | C-x C-s      | Saves the current buffer text to a file                                                                                                                                                                             |
| [write-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L272)            | C-x C-w      | Saves the text in the current buffer to the specified file                                                                                                                                                          |
| [write-region-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L293)     |              | Saves the region of text to the specified file                                                                                                                                                                      |
| [insert-file](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L301)           | C-x Tab      | Inserts the contents of the file into the current buffer.                                                                                                                                                           |
| [save-some-buffers](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L307)     | C-x s        | Save some files in the open buffer.                                                                                                                                                                                 |
| [revert-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L339)         |              | Restores the buffer. Normally this command will cause the contents of the file to be reflected in the buffer.                                                                                                       |
| [change-directory](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L372)      |              | Change directories associated with the buffer.                                                                                                                                                                      |
| [current-directory](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L381)     |              | Display the directory of the active buffer.
With prefix argument INSERT, insert the directory of the active buffer at point.                                                                                                                                                                                                                                                         |
| [format-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/file.lisp#L389) |              | Save changes and try to format the current buffer.

Supported modes include: c-mode with clang-format, go-mode with gofmt, js-mode and json-mode with prettier, and lisp-mode. Additionally rust-mode uses rustfmt.                                                                                                                                                                          |

## Project
| Command                                                                                               | Key bindings | Documentation                                                                                                                                                                                                            |
|-------------------------------------------------------------------------------------------------------|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [project-find-file](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L137)      | C-x p f      | Open a file, from the list of all files in this project.                                                                                                                                                                 |
| [project-root](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L154)           |              | Display this buffer's project directory.                                                                                                                                                                                 |
| [project-root-directory](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L161) | C-x p d      | Open this project's root directory.                                                                                                                                                                                      |
| [project-kill-buffers](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L221)   | C-x p K      | Delete all this project's buffers, except:

  - if *delete-repl-buffer* is non t, we don't delete the REPL buffer.
  - if *delete-last-buffer* is non nil, we will delete the last buffer. This would cause Lem to exit.                                                                                                                                                                                                                                             |
| [project-save](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L269)           | C-x p s      | Remember the current project for later sessions.                                                                                                                                                                         |
| [project-unsave](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L274)         | C-x p u      | Prompt for a project and remove it from the list of saved projects.                                                                                                                                                      |
| [project-switch](https://github.com/lem-project/lem/blob/main/src/commands/project.lisp#L292)         | C-x p p      | Prompt for a saved project and find a file in this project.                                                                                                                                                              |

## Buffer
| Command                                                                                            | Key bindings | Documentation                                 |
|----------------------------------------------------------------------------------------------------|--------------|-----------------------------------------------|
| [indent-current-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L17) |              | Indent the current buffer.                    |
| [toggle-read-only](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L21)      | C-x C-q      | Toggle the buffer read-only.                  |
| [rename-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L29)         |              | Rename the buffer.                            |
| [unmark-buffer](https://github.com/lem-project/lem/blob/main/src/commands/buffer.lisp#L33)         | M-~          | Remove the mark where the buffer was changed. |

## Window
| Command                                                                                                        | Key bindings   | Documentation                                                         |
|----------------------------------------------------------------------------------------------------------------|----------------|-----------------------------------------------------------------------|
| [select-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L68)                     | C-x b          | Switches to the selected buffer.                                      |
| [kill-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L95)                       | C-x k          | Delete buffer.                                                        |
| [previous-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L104)                  | C-x Left       | Switches to the previous buffer.                                      |
| [next-buffer](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L114)                      | C-x Right      | Switches to the next buffer.                                          |
| [recenter](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L120)                         | C-l            | Scroll so that the cursor is in the middle.                           |
| [split-active-window-vertically](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L127)   | C-x 2          | Split the current window vertically.                                  |
| [split-active-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L133) | C-x 3          | Split the current window horizontally.                                |
| [next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L139)                      | C-x o, M-o     | Go to the next window.                                                |
| [previous-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L151)                  | M-O            |                                                                       |
| [switch-to-last-focused-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L154)    |                | Go to the window that was last in focus.                              |
| [window-move-down](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L162)                 |                | Go to the window below.                                         |
| [window-move-up](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L167)                   |                | Go to the window above.                                           |
| [window-move-right](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L172)                |                | Go to the window on the right.                                        |
| [window-move-left](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L177)                 |                | Go to the window on the left.                                         |
| [delete-other-windows](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L182)             | C-x 1          | Delete all other windows.                                             |
| [delete-active-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L195)             | C-x 0, M-q     | Delete the active window.                                             |
| [quit-active-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L200)               |                | Quit the active window. This is a command for a popped-up window.     |
| [grow-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L205)                      | C-x ^          | Grow the window's height.                                             |
| [shrink-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L213)                    | C-x C-z        | Shrink the window's height.                                           |
| [grow-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L221)         | C-x }          | Grow the window's width.                                              |
| [shrink-window-horizontally](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L229)       | C-x {          | Shrink the window's width.                                            |
| [scroll-down](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L240)                      | C-Down, M-Down | Scroll down.                                                          |
| [scroll-up](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L254)                        | C-Up, M-Up     | Scroll up.                                                            |
| [find-file-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L266)            | C-x 4 f        | Open a file in another window. Split the screen vertically if needed. |
| [read-file-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L267)            | C-x 4 r        | Read a file in another window.                                        |
| [select-buffer-next-window](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L268)        | C-x 4 b        | Select a buffer in another window.                                    |
| [compare-windows](https://github.com/lem-project/lem/blob/main/src/commands/window.lisp#L272)                  |                |                                                                       |

## Multiple-Cursors
| Command                                                                                                         | Key bindings | Documentation                                               |
|-----------------------------------------------------------------------------------------------------------------|--------------|-------------------------------------------------------------|
| [add-cursors-to-next-line](https://github.com/lem-project/lem/blob/main/src/commands/multiple-cursors.lisp#L10) | M-C          | Duplicates the cursor under the currently existing cursors. |

## Process
| Command                                                                                     | Key bindings | Documentation                                                                         |
|---------------------------------------------------------------------------------------------|--------------|---------------------------------------------------------------------------------------|
| [filter-buffer](https://github.com/lem-project/lem/blob/main/src/commands/process.lisp#L12) | C-x #        | Replaces the contents of the buffer with the result of executing the command entered. |
| [pipe-command](https://github.com/lem-project/lem/blob/main/src/commands/process.lisp#L45)  | C-x @        | Run a command and displays the output.                                                |

## Help
| Command                                                                                      | Key bindings | Documentation                                                                |
|----------------------------------------------------------------------------------------------|--------------|------------------------------------------------------------------------------|
| [describe-key](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L14)      | C-x ?        | Tell what is the command associated to a keybinding.                         |
| [describe-bindings](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L43) |              | Describe the bindings of the buffer's current major mode.                    |
| [list-modes](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L65)        |              | Output all available major and minor modes.                                  |
| [apropos-command](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L112)  |              | Find all symbols in the running Lisp image whose names match a given string. |
| [lem-version](https://github.com/lem-project/lem/blob/main/src/commands/help.lisp#L123)      |              | Display Lem's version.                                                       |

## Font
| Command                                                                                       | Key bindings | Documentation                                                        |
|-----------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------|
| [font-size-increase](https://github.com/lem-project/lem/blob/main/src/commands/font.lisp#L12) | C-+          | Make the font larger (this currently only works with SDL2 frontend)  |
| [font-size-decrease](https://github.com/lem-project/lem/blob/main/src/commands/font.lisp#L16) | C--          | Make the font smaller (this currently only works with SDL2 frontend) |

## Other
| Command                                                                                       | Key bindings   | Documentation                                                |
|-----------------------------------------------------------------------------------------------|----------------|--------------------------------------------------------------|
| [nop-command](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L24)       | NopKey         |                                                              |
| [undefined-key](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L27)     |                | Signal undefined key error.                                  |
| [keyboard-quit](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L31)     | C-g            | Signal a `quit` condition.                                   |
| [escape](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L35)            | Escape         | Signal a `quit` condition silently.                          |
| [exit-lem](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L39)          | C-x C-c        | Ask for modified buffers before exiting lem.                 |
| [quick-exit](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L52)        |                | Exit the lem job and kill it.                                |
| [execute-command](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L57)   | M-x            | Read a command name, then read the ARG and call the command. |
| [show-context-menu](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L76) | Shift-F10, M-h |                                                              |
| [load-library](https://github.com/lem-project/lem/blob/main/src/commands/other.lisp#L82)      |                | Load the Lisp library named NAME.                            |

## Frame
| Command                                                                                            | Key bindings | Documentation       |
|----------------------------------------------------------------------------------------------------|--------------|---------------------|
| [toggle-frame-fullscreen](https://github.com/lem-project/lem/blob/main/src/commands/frame.lisp#L8) |              | Toggles fullscreen. |

