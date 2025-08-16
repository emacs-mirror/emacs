;;; package-compile.el --- Byte-Compilation of Packages -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;;         Daniel Hackney <dan@haxney.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the code to generate the package menu, that gives
;; package.el a text-useer interface for browsing, installing, updating
;; and removing packages.  Use \\[list-packages] to display the menu.

;;; Code:

(require 'package-core)
(require 'package-install)
(require 'package-describe)
(require 'package-vc)

(require 'tabulated-list)
(require 'browse-url)
(require 'macroexp)
(require 'lisp-mnt)

(defgroup package-menu nil
  "A interface for package management."
  :group 'package
  :version "24.1")

(defcustom package-menu-async t
  "If non-nil, package-menu will use async operations when possible.
Currently, only the refreshing of archive contents supports
asynchronous operations.  Package transactions are still done
synchronously."
  :type 'boolean
  :version "25.1")

(defcustom package-menu-hide-low-priority 'archive
  "If non-nil, hide low priority packages from the packages menu.
A package is considered low priority if there's another version
of it available such that:
    (a) the archive of the other package is higher priority than
    this one, as per `package-archive-priorities';
  or
    (b) they both have the same archive priority but the other
    package has a higher version number.

This variable has three possible values:
    nil: no packages are hidden;
    `archive': only criterion (a) is used;
    t: both criteria are used.

This variable has no effect if `package-menu--hide-packages' is
nil, so it can be toggled with \\<package-menu-mode-map>\\[package-menu-toggle-hiding]."
  :type '(choice (const :tag "Don't hide anything" nil)
                 (const :tag "Hide per package-archive-priorities"
                        archive)
                 (const :tag "Hide per archive and version number" t))
  :version "25.1")

(defcustom package-hidden-regexps nil
  "List of regexps matching the name of packages to hide.
If the name of a package matches any of these regexps it is
omitted from the package menu.  To toggle this, type \\[package-menu-toggle-hiding].

Values can be interactively added to this list by typing
\\[package-menu-hide-package] on a package."
  :version "25.1"
  :type '(repeat (regexp :tag "Hide packages with name matching")))

(defcustom package-menu-use-current-if-no-marks t
  "Whether \\<package-menu-mode-map>\\[package-menu-execute] in package menu operates on current package if none are marked.

If non-nil, and no packages are marked for installation or
deletion, \\<package-menu-mode-map>\\[package-menu-execute] will operate on the current package at point,
see `package-menu-execute' for details.
The default is t.  Set to nil to get back the original behavior
of having `package-menu-execute' signal an error when no packages
are marked for installation or deletion."
  :version "29.1"
  :type 'boolean)

(defcustom package-name-column-width 30
  "Column width for the Package name in the package menu."
  :type 'natnum
  :version "28.1")

(defcustom package-version-column-width 14
  "Column width for the Package version in the package menu."
  :type 'natnum
  :version "28.1")

(defcustom package-status-column-width 12
  "Column width for the Package status in the package menu."
  :type 'natnum
  :version "28.1")

(defcustom package-archive-column-width 8
  "Column width for the Package archive in the package menu."
  :type 'natnum
  :version "28.1")

(defun package-browse-url (desc &optional secondary)
  "Open the website of the package under point in a browser.
`browse-url' is used to determine the browser to be used.  If
SECONDARY (interactively, the prefix), use the secondary browser.
DESC must be a `package-desc' object."
  (interactive (list (package--query-desc)
                     current-prefix-arg)
               package-menu-mode)
  (unless desc
    (user-error "No package here"))
  (let ((url (cdr (assoc :url (package-desc-extras desc)))))
    (unless url
      (user-error "No website for %s" (package-desc-name desc)))
    (if secondary
        (funcall browse-url-secondary-browser-function url)
      (browse-url url))))

(defun package--imenu-prev-index-position-function ()
  "Move point to previous line in package-menu buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

(defun package--imenu-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (let ((package-desc (tabulated-list-get-id)))
    (format "%s (%s): %s"
            (package-desc-name package-desc)
            (package-version-join (package-desc-version package-desc))
            (package-desc-summary package-desc))))

(defun package-menu--display (remember-pos suffix)
  "Display the Package Menu.
If REMEMBER-POS is non-nil, keep point on the same entry.

If SUFFIX is non-nil, append that to \"Package\" for the first
column in the header line."
  (setf (car (aref tabulated-list-format 0))
        (if suffix
            (concat "Package[" suffix "]")
          "Package"))
  (tabulated-list-init-header)
  (tabulated-list-print remember-pos))

(defun package-menu--generate (remember-pos &optional packages keywords)
  "Populate and display the Package Menu.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (package-menu--refresh packages keywords)
  (package-menu--display remember-pos
                  (when keywords
                    (let ((filters (mapconcat #'identity keywords ",")))
                      (concat "Package[" filters "]")))))

(defun package-menu--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (package-menu--print-info-simple (car pkg)))
(make-obsolete 'package-menu--print-info
               'package-menu--print-info-simple "25.1")


;;; Package menu faces

(defface package-name
  '((t :inherit link))
  "Face used on package names in the package menu."
  :version "25.1")

(defface package-description
  '((t :inherit default))
  "Face used on package description summaries in the package menu."
  :version "25.1")

;; Shame this hyphenates "built-in", when "font-lock-builtin-face" doesn't.
(defface package-status-built-in
  '((t :inherit font-lock-builtin-face))
  "Face used on the status and version of built-in packages."
  :version "25.1")

(defface package-status-external
  '((t :inherit package-status-built-in))
  "Face used on the status and version of external packages."
  :version "25.1")

(defface package-status-available
  '((t :inherit default))
  "Face used on the status and version of available packages."
  :version "25.1")

(defface package-status-new
  '((t :inherit (bold package-status-available)))
  "Face used on the status and version of new packages."
  :version "25.1")

(defface package-status-held
  '((t :inherit font-lock-constant-face))
  "Face used on the status and version of held packages."
  :version "25.1")

(defface package-status-disabled
  '((t :inherit font-lock-warning-face))
  "Face used on the status and version of disabled packages."
  :version "25.1")

(defface package-status-installed
  '((t :inherit font-lock-comment-face))
  "Face used on the status and version of installed packages."
  :version "25.1")

(defface package-status-from-source
  '((t :inherit font-lock-negation-char-face))
  "Face used on the status and version of installed packages."
  :version "29.1")

(defface package-status-dependency
  '((t :inherit package-status-installed))
  "Face used on the status and version of dependency packages."
  :version "25.1")

(defface package-status-unsigned
  '((t :inherit font-lock-warning-face))
  "Face used on the status and version of unsigned packages."
  :version "25.1")

(defface package-status-incompat
  '((t :inherit error))
  "Face used on the status and version of incompat packages."
  :version "25.1")

(defface package-status-avail-obso
  '((t :inherit package-status-incompat))
  "Face used on the status and version of avail-obso packages."
  :version "25.1")

(defface package-mark-install-line
  '((((class color) (background light))
     :background "darkolivegreen1" :extend t)
    (((class color) (background dark))
     :background "seagreen" :extend t)
    (t :inherit (highlight) :extend t))
  "Face used for highlighting in package-menu packages marked to be installed."
  :version "31.1")

(defface package-mark-delete-line
  '((((class color) (background light))
     :background "rosybrown1" :extend t)
    (((class color) (background dark))
     :background "indianred4" :extend t)
    (t :inherit (highlight) :extend t))
  "Face used for highlighting in package-menu packages marked to be deleted."
  :version "31.1")

(defface package-mode-line-total nil
  "Face for the total number of packages displayed on the mode line."
  :version "31.1")

(defface package-mode-line-installed '((t :inherit package-status-installed))
  "Face for the number of installed packages displayed on the mode line."
  :version "31.1")

(defface package-mode-line-to-upgrade '((t :inherit bold))
  "Face for the number of packages to upgrade displayed on the mode line."
  :version "31.1")

(defface package-mode-line-new '((t :inherit package-status-new))
  "Face for the number of new packages displayed on the mode line."
  :version "31.1")

;;; Package menu printing

(defun package-menu--print-info-simple (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG is a `package-desc' object.
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (let* ((status (package-desc-status pkg))
         (face (pcase status
                 ("built-in"  'package-status-built-in)
                 ("external"  'package-status-external)
                 ("available" 'package-status-available)
                 ("avail-obso" 'package-status-avail-obso)
                 ("new"       'package-status-new)
                 ("held"      'package-status-held)
                 ("disabled"  'package-status-disabled)
                 ("installed" 'package-status-installed)
                 ("source"    'package-status-from-source)
                 ("dependency" 'package-status-dependency)
                 ("unsigned"  'package-status-unsigned)
                 ("incompat"  'package-status-incompat)
                 (_            'font-lock-warning-face)))) ; obsolete.
    (list pkg
          `[(,(symbol-name (package-desc-name pkg))
             face package-name
             font-lock-face package-name
             follow-link t
             package-desc ,pkg
             action package-menu-describe-package)
            ,(propertize
              (if (package-vc-p pkg)
                  (package-vc-commit pkg)
                (package-version-join
                 (package-desc-version pkg)))
              'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,(propertize (or (package-desc-archive pkg) "")
                                    'font-lock-face face)
            ,(propertize (package-desc-summary pkg)
                         'font-lock-face 'package-description)])))

(defvar package-menu--old-archive-contents nil
  "`package-archive-contents' before the latest refresh.")

(defun package--ensure-package-menu-mode ()
  "Signal a user-error if major mode is not `package-menu-mode'."
  (unless (derived-mode-p 'package-menu-mode)
    (user-error "The current buffer is not a Package Menu")))

(defvar package-menu--new-package-list nil
  "List of newly-available packages since `list-packages' was last called.")

(defun package-menu--refresh-contents (&optional _arg _noconfirm)
  "In Package Menu, download the Emacs Lisp package archive.
Fetch the contents of each archive specified in
`package-archives', and then refresh the package menu.

`package-menu-mode' sets `revert-buffer-function' to this
function.  The args ARG and NOCONFIRM, passed from
`revert-buffer', are ignored."
  (package--ensure-package-menu-mode)
  (setq package-menu--old-archive-contents package-archive-contents)
  (setq package-menu--new-package-list nil)
  (package-refresh-contents package-menu-async))
(define-obsolete-function-alias 'package-menu-refresh 'revert-buffer "27.1")

(defun package-menu--overlay-line (face)
  "Highlight whole line with face FACE."
  (let ((ov (make-overlay (line-beginning-position)
                          (1+ (line-end-position)))))
    (overlay-put ov 'pkg-menu-ov t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'face face)))

(defun package-menu--remove-overlay ()
  "Remove all overlays done by `package-menu--overlay-line' in current line."
  (remove-overlays (line-beginning-position)
                   (1+ (line-end-position))
                   'pkg-menu-ov t))

(defun package-menu-hide-package ()
  "Hide in Package Menu packages that match a regexp.
Prompt for the regexp to match against package names.
The default regexp will hide only the package whose name is at point.

The regexp is added to the list in the user option
`package-hidden-regexps' and saved for future sessions.

To unhide a package, type
`\\[customize-variable] RET package-hidden-regexps', and then modify
the regexp such that it no longer matches the package's name.

Type \\[package-menu-toggle-hiding] to toggle package hiding."
  (declare (interactive-only "change `package-hidden-regexps' instead."))
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (let* ((name (when (derived-mode-p 'package-menu-mode)
                 (concat "\\`" (regexp-quote (symbol-name (package-desc-name
                                                           (tabulated-list-get-id))))
                         "\\'")))
         (re (read-string "Hide packages matching regexp: " name)))
    ;; Test if it is valid.
    (string-match re "")
    (push re package-hidden-regexps)
    (customize-save-variable 'package-hidden-regexps package-hidden-regexps)
    (package-menu--post-refresh)
    (let ((hidden
           (cl-remove-if-not (lambda (e) (string-match re (symbol-name (car e))))
                             package-archive-contents)))
      (message "Packages to hide: %d.  Type `%s' to toggle or `%s' to customize"
               (length hidden)
               (substitute-command-keys "\\[package-menu-toggle-hiding]")
               (substitute-command-keys "\\[customize-variable] RET package-hidden-regexps")))))


(defun package-menu-describe-package (&optional button)
  "Describe the current package.
The current package is the package at point.
If optional arg BUTTON is non-nil, describe its associated
package(s); this is always nil in interactive invocations."
  (interactive nil package-menu-mode)
  (let ((pkg-desc (if button (button-get button 'package-desc)
                    (tabulated-list-get-id))))
    (if pkg-desc
        (describe-package pkg-desc)
      (user-error "No package here"))))

;; fixme numeric argument
(defun package-menu-mark-delete (&optional _num)
  "Mark the current package for deletion and move to the next line.
The current package is the package at point."
  (interactive "p" package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (member (package-menu-get-status)
              '("installed" "source" "dependency" "obsolete" "unsigned"))
      (progn (package-menu--overlay-line 'package-mark-delete-line)
             (tabulated-list-put-tag "D" t))
    (forward-line)))

(defun package-menu-mark-install (&optional _num)
  "Mark the current package for installation and move to the next line.
The current package is the package at point."
  (interactive "p" package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (member (package-menu-get-status) '("available" "avail-obso" "new" "dependency"))
      (progn (package-menu--overlay-line 'package-mark-install-line)
             (tabulated-list-put-tag "I" t))
    (forward-line)))

(defun package-menu-mark-unmark (&optional _num)
  "Clear any marks on the current package and move to the next line.
The current package is the package at point."
  (interactive "p" package-menu-mode)
  (package--ensure-package-menu-mode)
  (package-menu--remove-overlay)
  (tabulated-list-put-tag " " t))

(defun package-menu-backup-unmark ()
  "Back up one line and clear any marks on that line's package."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (forward-line -1)
  (package-menu--remove-overlay)
  (tabulated-list-put-tag " "))

(defun package-menu-mark-obsolete-for-deletion ()
  "Mark all obsolete packages for deletion."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (equal (package-menu-get-status) "obsolete")
          (progn (package-menu--overlay-line 'package-mark-delete-line)
                 (tabulated-list-put-tag "D" t))
        (forward-line 1)))))

(defvar package--quick-help-keys
  '((("mark for installation," . 9)
     ("mark for deletion," . 9) "unmark," ("execute marked actions" . 1))
    ("next," "previous")
    ("Hide-package," "(-toggle-hidden")
    ("g-refresh-contents," "/-filter," "help")))

(defun package--prettify-quick-help-key (desc)
  "Prettify DESC to be displayed as a help menu."
  (if (listp desc)
      (if (listp (cdr desc))
          (mapconcat #'package--prettify-quick-help-key desc "   ")
        (let ((place (cdr desc))
              (out (copy-sequence (car desc))))
          (add-text-properties place (1+ place)
                               '(face help-key-binding)
                               out)
          out))
    (package--prettify-quick-help-key (cons desc 0))))

(defun package-menu-quick-help ()
  "Show short help for key bindings in `package-menu-mode'.
You can view the full list of keys with \\[describe-mode]."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (message (mapconcat #'package--prettify-quick-help-key
                      package--quick-help-keys "\n")))

(defun package-menu-get-status ()
  "Return status description of package at point in Package Menu."
  (package--ensure-package-menu-mode)
  (let* ((id (tabulated-list-get-id))
         (entry (and id (assoc id tabulated-list-entries))))
    (if entry
        (aref (cadr entry) 2)
      "")))

(defun package-menu--find-upgrades ()
  "In Package Menu, return an alist of packages that can be upgraded.
The alist has the same form as `package-alist', namely a list
of elements of the form (PKG . DESCS), but where DESCS is the `package-desc'
object corresponding to the newer version."
  (let (installed available upgrades)
    ;; Build list of installed/available packages in this buffer.
    (dolist (entry tabulated-list-entries)
      ;; ENTRY is (PKG-DESC [NAME VERSION STATUS DOC])
      (let ((pkg-desc (car entry))
            (status (aref (cadr entry) 2)))
        (cond ((member status '("installed" "dependency" "unsigned" "external" "built-in"))
               (push pkg-desc installed))
              ((member status '("available" "new"))
               (setq available (package--append-to-alist pkg-desc available))))))
    ;; Loop through list of installed packages, finding upgrades.
    (dolist (pkg-desc installed)
      (let* ((name (package-desc-name pkg-desc))
             (avail-pkg (cadr (assq name available))))
        (and avail-pkg
             (version-list-< (package-desc-priority-version pkg-desc)
                             (package-desc-priority-version avail-pkg))
             (or (not (package--active-built-in-p pkg-desc))
                 package-install-upgrade-built-in)
             (push (cons name avail-pkg) upgrades))))
    upgrades))

(defvar package-menu--mark-upgrades-pending nil
  "Whether mark-upgrades is waiting for a refresh to finish.")

(defun package-menu--mark-upgrades-1 ()
  "Mark all upgradable packages in the Package Menu.
Implementation of `package-menu-mark-upgrades'."
  (setq package-menu--mark-upgrades-pending nil)
  (let ((upgrades (package-menu--find-upgrades)))
    (if (null upgrades)
        (message "No packages to upgrade")
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((pkg-desc (tabulated-list-get-id))
                 (upgrade (cdr (assq (package-desc-name pkg-desc) upgrades))))
            (cond ((null upgrade)
                   (forward-line 1))
                  ((equal pkg-desc upgrade)
                   (package-menu-mark-install))
                  (t
                   (package-menu-mark-delete))))))
      (message "Packages marked for upgrading: %d"
               (length upgrades)))))


(defun package-menu-mark-upgrades ()
  "Mark all upgradable packages in the Package Menu.
For each installed package for which a newer version is available,
place an (I)nstall flag on the available version and a (D)elete flag
on the installed version.  A subsequent \\[package-menu-execute] command will upgrade
the marked packages.

If there's an async refresh operation in progress, the flags will
be placed as part of `package-menu--post-refresh' instead of
immediately."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (not package--downloads-in-progress)
      (package-menu--mark-upgrades-1)
    (setq package-menu--mark-upgrades-pending t)
    (message "Waiting for refresh to finish...")))

(defun package-menu--list-to-prompt (packages &optional include-dependencies)
  "Return a string listing PACKAGES that's usable in a prompt.
PACKAGES is a list of `package-desc' objects.
Formats the returned string to be usable in a minibuffer
prompt (see `package-menu--prompt-transaction-p').

If INCLUDE-DEPENDENCIES, also include the number of uninstalled
dependencies."
  ;; The case where `package' is empty is handled in
  ;; `package-menu--prompt-transaction-p' below.
  (format "%d (%s)%s"
          (length packages)
          (mapconcat #'package-desc-full-name packages " ")
          (let ((deps
                 (seq-remove
                  #'package-installed-p
                  (delete-dups
                   (apply
                    #'nconc
                    (mapcar (lambda (package)
                              (package--dependencies
                               (package-desc-name package)))
                            packages))))))
            (if (and include-dependencies deps)
                (if (length= deps 1)
                    (format " plus 1 dependency")
                  (format " plus %d dependencies" (length deps)))
              ""))))

(defun package-menu--prompt-transaction-p (delete install upgrade)
  "Prompt the user about DELETE, INSTALL, and UPGRADE.
DELETE, INSTALL, and UPGRADE are lists of `package-desc' objects.
Either may be nil, but not all."
  (y-or-n-p
   (concat
    (when delete
      (format "Packages to delete: %s.  "
              (package-menu--list-to-prompt delete)))
    (when install
      (format "Packages to install: %s.  "
              (package-menu--list-to-prompt install t)))
    (when upgrade
      (format "Packages to upgrade: %s.  "
              (package-menu--list-to-prompt upgrade)))
    "Proceed? ")))


(defun package-menu--partition-transaction (install delete)
  "Return an alist describing an INSTALL DELETE transaction.
Alist contains three entries, upgrade, delete, and install, each
with a list of package names.

The upgrade entry contains any `package-desc' objects in INSTALL
whose name coincides with an object in DELETE.  The delete and
the install entries are the same as DELETE and INSTALL with such
objects removed."
  (let* ((upg (cl-intersection install delete :key #'package-desc-name))
         (ins (cl-set-difference install upg :key #'package-desc-name))
         (del (cl-set-difference delete upg :key #'package-desc-name)))
    `((delete . ,del) (install . ,ins) (upgrade . ,upg))))

(defvar package-menu--transaction-status nil
  "Mode-line status of ongoing package transaction.")

(defun package-menu--perform-transaction (install-list delete-list)
  "Install packages in INSTALL-LIST and delete DELETE-LIST.
Return nil if there were no errors; non-nil otherwise."
  (let ((errors nil))
    (if install-list
        (let ((status-format (format ":Installing %%d/%d"
                                     (length install-list)))
              (i 0)
              (package-menu--transaction-status))
          (dolist (pkg install-list)
            (setq package-menu--transaction-status
                  (format status-format (incf i)))
            (force-mode-line-update)
            (redisplay 'force)
            ;; Don't mark as selected, `package-menu-execute' already
            ;; does that.
            (package-install pkg 'dont-select))))
    (let ((package-menu--transaction-status ":Deleting"))
      (force-mode-line-update)
      (redisplay 'force)
      (dolist (elt (package--sort-by-dependence delete-list))
        (condition-case-unless-debug err
            (let ((inhibit-message (or inhibit-message package-menu-async)))
              (package-delete elt nil 'nosave))
          (error
           (push (package-desc-full-name elt) errors)
           (message "Error trying to delete `%s': %s"
                    (package-desc-full-name elt)
                    (error-message-string err))))))
    errors))

(defun package--update-selected-packages (add remove)
  "Update the `package-selected-packages' list according to ADD and REMOVE.
ADD and REMOVE must be disjoint lists of package names (or
`package-desc' objects) to be added and removed to the selected
packages list, respectively."
  (dolist (p add)
    (cl-pushnew (if (package-desc-p p) (package-desc-name p) p)
                package-selected-packages))
  (dolist (p remove)
    (setq package-selected-packages
          (remove (if (package-desc-p p) (package-desc-name p) p)
                  package-selected-packages)))
  (when (or add remove)
    (package--save-selected-packages package-selected-packages)))

(defun package-menu-execute (&optional noquery)
  "Perform Package Menu actions on marked packages.
Packages marked for installation are downloaded and installed,
packages marked for deletion are removed, and packages marked for
upgrading are downloaded and upgraded.

If no packages are marked, the action taken depends on the state
of the current package, the one at point.  If it's not already
installed, this command will install the package; if it's installed,
the command will delete the package.

Optional argument NOQUERY non-nil means do not ask the user to
confirm the installations/deletions; this is always nil in interactive
invocations."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (let (install-list delete-list cmd pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          ;; This is the key PKG-DESC.
          (setq pkg-desc (tabulated-list-get-id))
          (cond ((eq cmd ?D)
                 (push pkg-desc delete-list))
                ((eq cmd ?I)
                 (push pkg-desc install-list))))
        (forward-line)))
    ;; Nothing marked.
    (unless (or delete-list install-list)
      ;; Not on a package line.
      (unless (and (tabulated-list-get-id)
                   package-menu-use-current-if-no-marks)
        (user-error "No operations specified"))
      (let* ((id (tabulated-list-get-id))
             (status (package-menu-get-status)))
        (cond
         ((member status '("installed"))
          (push id delete-list))
         ((member status '("available" "avail-obso" "new" "dependency"))
          (push id install-list))
         (t (user-error "No default action available for status: %s"
                        status)))))
    (let-alist (package-menu--partition-transaction install-list delete-list)
      (when (or noquery
                (package-menu--prompt-transaction-p .delete .install .upgrade))
        (let ((message-template
               (concat "[ "
                       (when .delete
                         (format "Delete %d " (length .delete)))
                       (when .install
                         (format "Install %d " (length .install)))
                       (when .upgrade
                         (format "Upgrade %d " (length .upgrade)))
                       "]")))
          (message "Operation %s started" message-template)
          ;; Packages being upgraded are not marked as selected.
          (package--update-selected-packages .install .delete)
          (unless (package-menu--perform-transaction install-list delete-list)
            ;; If there weren't errors, output data.
            (if-let* ((removable (package--removable-packages)))
                (message "Operation finished.  Packages that are no longer needed: %d.  Type `%s' to remove them"
                         (length removable)
                         (substitute-command-keys "\\[package-autoremove]"))
              (message "Operation %s finished" message-template))))))))

(defun package-menu--version-predicate (A B)
  "Predicate to sort \"*Packages*\" buffer by the version column.
This is used for `tabulated-list-format' in `package-menu-mode'."
  (let ((vA (or (ignore-error error (version-to-list (aref (cadr A) 1))) '(0)))
        (vB (or (ignore-error error (version-to-list (aref (cadr B) 1))) '(0))))
    (if (version-list-= vA vB)
        (package-menu--name-predicate A B)
      (version-list-< vA vB))))

(defun package-menu--status-predicate (A B)
  "Predicate to sort \"*Packages*\" buffer by the status column.
This is used for `tabulated-list-format' in `package-menu-mode'."
  (let ((sA (aref (cadr A) 2))
        (sB (aref (cadr B) 2)))
    (cond ((string= sA sB)
           (package-menu--name-predicate A B))
          ((string= sA "new") t)
          ((string= sB "new") nil)
          ((string-prefix-p "avail" sA)
           (if (string-prefix-p "avail" sB)
               (package-menu--name-predicate A B)
             t))
          ((string-prefix-p "avail" sB) nil)
          ((string= sA "installed") t)
          ((string= sB "installed") nil)
          ((string= sA "dependency") t)
          ((string= sB "dependency") nil)
          ((string= sA "source") t)
          ((string= sB "source") nil)
          ((string= sA "unsigned") t)
          ((string= sB "unsigned") nil)
          ((string= sA "held") t)
          ((string= sB "held") nil)
          ((string= sA "external") t)
          ((string= sB "external") nil)
          ((string= sA "built-in") t)
          ((string= sB "built-in") nil)
          ((string= sA "obsolete") t)
          ((string= sB "obsolete") nil)
          ((string= sA "incompat") t)
          ((string= sB "incompat") nil)
          (t (string< sA sB)))))

(defun package-menu--description-predicate (A B)
  "Predicate to sort \"*Packages*\" buffer by the description column.
This is used for `tabulated-list-format' in `package-menu-mode'."
  (let ((dA (aref (cadr A) (if (cdr package-archives) 4 3)))
        (dB (aref (cadr B) (if (cdr package-archives) 4 3))))
    (if (string= dA dB)
        (package-menu--name-predicate A B)
      (string< dA dB))))

(defun package-menu--name-predicate (A B)
  "Predicate to sort \"*Packages*\" buffer by the name column.
This is used for `tabulated-list-format' in `package-menu-mode'."
  (string< (symbol-name (package-desc-name (car A)))
           (symbol-name (package-desc-name (car B)))))

(defun package-menu--archive-predicate (A B)
  "Predicate to sort \"*Packages*\" buffer by the archive column.
This is used for `tabulated-list-format' in `package-menu-mode'."
  (let ((a (or (package-desc-archive (car A)) ""))
        (b (or (package-desc-archive (car B)) "")))
    (if (string= a b)
        (package-menu--name-predicate A B)
      (string< a b))))

(defun package-menu--populate-new-package-list ()
  "Decide which packages are new in `package-archive-contents'.
Store this list in `package-menu--new-package-list'."
  ;; Find which packages are new.
  (when package-menu--old-archive-contents
    (dolist (elt package-archive-contents)
      (unless (assq (car elt) package-menu--old-archive-contents)
        (push (car elt) package-menu--new-package-list)))
    (setq package-menu--old-archive-contents nil)))

(defun package-menu--find-and-notify-upgrades ()
  "Notify the user of upgradable packages."
  (when-let* ((upgrades (package-menu--find-upgrades)))
    (message "Packages that can be upgraded: %d; type `%s' to mark for upgrading."
             (length upgrades)
             (substitute-command-keys "\\[package-menu-mark-upgrades]"))))


(defun package-menu--post-refresh ()
  "Revert \"*Packages*\" buffer and check for new packages and upgrades.
Do nothing if there's no *Packages* buffer.

This function is called after `package-refresh-contents' and it
is added to `post-command-hook' by any function which alters the
package database (`package-install' and `package-delete').  When
run, it removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'package-menu--post-refresh)
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (package-menu--populate-new-package-list)
        (run-hooks 'tabulated-list-revert-hook)
        (tabulated-list-print 'remember 'update)))))

(defun package-menu--mark-or-notify-upgrades ()
  "If there's a *Packages* buffer, check for upgrades and possibly mark them.
Do nothing if there's no *Packages* buffer.  If there are
upgrades, mark them if `package-menu--mark-upgrades-pending' is
non-nil, otherwise just notify the user that there are upgrades.
This function is called after `package-refresh-contents'."
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if package-menu--mark-upgrades-pending
            (package-menu--mark-upgrades-1)
          (package-menu--find-and-notify-upgrades))))))

;;;###autoload
(defun list-packages (&optional no-fetch)
  "Display a list of packages.
This first fetches the updated list of packages before
displaying, unless a prefix argument NO-FETCH is specified.
The list is displayed in a buffer named `*Packages*', and
includes the package's version, availability status, and a
short description."
  (interactive "P")
  (require 'finder-inf nil t)
  ;; Initialize the package system if necessary.
  (unless package--initialized
    (package-initialize t))
  ;; Integrate the package-menu with updating the archives.
  (add-hook 'package--post-download-archives-hook
            #'package-menu--post-refresh)
  (add-hook 'package--post-download-archives-hook
            #'package-menu--mark-or-notify-upgrades 'append)
  (add-hook 'package--post-download-archives-hook
            #'package-menu--set-mode-line-format 'append)

  ;; Generate the Package Menu.
  (let ((buf (get-buffer-create "*Packages*")))
    (with-current-buffer buf
      ;; Since some packages have their descriptions include non-ASCII
      ;; characters...
      (setq buffer-file-coding-system 'utf-8)
      (package-menu-mode)

      ;; Fetch the remote list of packages.
      (unless no-fetch (package-menu--refresh-contents))

      ;; If we're not async, this would be redundant.
      (when package-menu-async
        (package-menu--generate nil t)))
    ;; The package menu buffer has keybindings.  If the user types
    ;; `M-x list-packages', that suggests it should become current.
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defalias 'package-list-packages 'list-packages)

;; Used in finder.el
;;;###autoload
(defun package-show-package-list (&optional packages keywords)
  "Display PACKAGES in a *Packages* buffer.
This is similar to `list-packages', but it does not fetch the
updated list of packages, and it only displays packages with
names in PACKAGES (which should be a list of symbols).

When KEYWORDS are given, only packages with those KEYWORDS are
shown."
  (interactive)
  (require 'finder-inf nil t)
  (let* ((buf (get-buffer-create "*Packages*"))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (package-menu-mode)
      (package-menu--generate nil packages keywords))
    (if win
        (select-window win)
      (switch-to-buffer buf))))

(defun package-menu--filter-by (predicate suffix)
  "Filter \"*Packages*\" buffer by PREDICATE and add SUFFIX to header.
PREDICATE is a function which will be called with one argument, a
`package-desc' object, and returns t if that object should be
listed in the Package Menu.

SUFFIX is passed on to `package-menu--display' and is added to
the header line of the first column."
  ;; Update `tabulated-list-entries' so that it contains all
  ;; packages before searching.
  (package-menu--refresh t nil)
  (let (found-entries)
    (dolist (entry tabulated-list-entries)
      (when (funcall predicate (car entry))
        (push entry found-entries)))
    (if found-entries
        (progn
          (setq tabulated-list-entries found-entries)
          (package-menu--display t suffix))
      (user-error "No packages found"))))

(defun package-menu-filter-by-archive (archive)
  "Filter the \"*Packages*\" buffer by ARCHIVE.
Display only packages from package archive ARCHIVE.
ARCHIVE can be the name of a single archive (a string), or
a list of archive names.  If ARCHIVE is nil or an empty
string, show all packages.

When called interactively, prompt for ARCHIVE.  To specify
several archives, type their names separated by commas."
  (interactive (list (completing-read-multiple
                      "Filter by archive: "
                      (mapcar #'car package-archives)))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (let ((archives (ensure-list archive)))
    (package-menu--filter-by
     (lambda (pkg-desc)
       (let ((pkg-archive (package-desc-archive pkg-desc)))
         (or (null archives)
             (and pkg-archive
                  (member pkg-archive archives)))))
     (concat "archive:" (string-join archives ",")))))

(defun package-menu-filter-by-description (description)
  "Filter the \"*Packages*\" buffer by the regexp DESCRIPTION.
Display only packages whose description matches the regexp
given as DESCRIPTION.

When called interactively, prompt for DESCRIPTION.

If DESCRIPTION is nil or the empty string, show all packages."
  (interactive (list (read-regexp "Filter by description (regexp)"))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (or (not description) (string-empty-p description))
      (package-menu--generate t t)
    (package-menu--filter-by (lambda (pkg-desc)
                        (string-match description
                                      (package-desc-summary pkg-desc)))
                      (format "desc:%s" description))))

(defun package--has-keyword-p (desc &optional keywords)
  "Test if package DESC has any of the given KEYWORDS.
When none are given, the package matches."
  (if keywords
      (let ((desc-keywords (and desc (package-desc--keywords desc)))
            found)
        (while (and (not found) keywords)
          (let ((k (pop keywords)))
            (setq found
                  (or (string= k (concat "arc:" (package-desc-archive desc)))
                      (string= k (concat "status:" (package-desc-status desc)))
                      (member k desc-keywords)))))
        found)
    t))

(defun package-all-keywords ()
  "Collect all package keywords."
  (let ((key-list))
    (package--mapc (lambda (desc)
                     (setq key-list (append (package-desc--keywords desc)
                                            key-list))))
    key-list))

(defun package-menu-filter-by-keyword (keyword)
  "Filter the \"*Packages*\" buffer by KEYWORD.
Display only packages whose keywords match the specified KEYWORD.
KEYWORD can be a string or a list of strings.  If KEYWORD is nil
or the empty string, show all packages.

In addition to package keywords, KEYWORD can include the name(s)
of archive(s) and the package status, such as \"available\"
or \"built-in\" or \"obsolete\".

When called interactively, prompt for KEYWORD.  To specify several
keywords, type them separated by commas."
  (interactive (list (completing-read-multiple
                      "Keywords: "
                      (package-all-keywords)))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (when (stringp keyword)
    (setq keyword (list keyword)))
  (if (not keyword)
      (package-menu--generate t t)
    (package-menu--filter-by (lambda (pkg-desc)
                        (package--has-keyword-p pkg-desc keyword))
                      (concat "keyword:" (string-join keyword ",")))))

(define-obsolete-function-alias
  'package-menu-filter #'package-menu-filter-by-keyword "27.1")

(defun package-menu-filter-by-name-or-description (name-or-description)
  "Filter the \"*Packages*\" buffer by the regexp NAME-OR-DESCRIPTION.
Display only packages whose name or description matches the regexp
NAME-OR-DESCRIPTION.

When called interactively, prompt for NAME-OR-DESCRIPTION.

If NAME-OR-DESCRIPTION is nil or the empty string, show all
packages."
  (interactive (list (read-regexp "Filter by name or description (regexp)"))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (or (not name-or-description) (string-empty-p name-or-description))
      (package-menu--generate t t)
    (package-menu--filter-by (lambda (pkg-desc)
                        (or (string-match name-or-description
                                          (package-desc-summary pkg-desc))
                            (string-match name-or-description
                                          (symbol-name
                                           (package-desc-name pkg-desc)))))
                      (format "name-or-desc:%s" name-or-description))))

(defun package-menu-filter-by-name (name)
  "Filter the \"*Packages*\" buffer by the regexp NAME.
Display only packages whose name matches the regexp NAME.

When called interactively, prompt for NAME.

If NAME is nil or the empty string, show all packages."
  (interactive (list (read-regexp "Filter by name (regexp)"))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (or (not name) (string-empty-p name))
      (package-menu--generate t t)
    (package-menu--filter-by (lambda (pkg-desc)
                        (string-match-p name (symbol-name
                                              (package-desc-name pkg-desc))))
                      (format "name:%s" name))))

(defun package-menu-filter-by-status (status)
  "Filter the \"*Packages*\" buffer by STATUS.
Display only packages with specified STATUS.
STATUS can be a single status, a string, or a list of strings.
If STATUS is nil or the empty string, show all packages.

When called interactively, prompt for STATUS.  To specify
several possible status values, type them separated by commas."
  (interactive (list (completing-read "Filter by status: "
                                      '("avail-obso"
                                        "available"
                                        "built-in"
                                        "dependency"
                                        "disabled"
                                        "external"
                                        "held"
                                        "incompat"
                                        "installed"
                                        "source"
                                        "new"
                                        "unsigned")))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (if (or (not status) (string-empty-p status))
      (package-menu--generate t t)
    (let ((status-list
           (if (listp status)
               status
             (split-string status ","))))
      (package-menu--filter-by
       (lambda (pkg-desc)
         (member (package-desc-status pkg-desc) status-list))
       (format "status:%s" (string-join status-list ","))))))

(defun package-menu-filter-by-version (version predicate)
  "Filter the \"*Packages*\" buffer by VERSION and PREDICATE.
Display only packages whose version satisfies the condition
defined by VERSION and PREDICATE.

When called interactively, prompt for one of the comparison operators
`<', `>' or `=', and for a version.  Show only packages whose version
is lower (`<'), equal (`=') or higher (`>') than the specified VERSION.

When called from Lisp, VERSION should be a version string and
PREDICATE should be the symbol `=', `<' or `>'.

If VERSION is nil or the empty string, show all packages."
  (interactive (let ((choice (intern
                              (char-to-string
                               (read-char-choice
                                "Filter by version? [Type =, <, > or q] "
                                '(?< ?> ?= ?q))))))
                 (if (eq choice 'q)
                     '(quit nil)
                   (list (read-from-minibuffer
                          (concat "Filter by version ("
                                  (pcase choice
                                    ('= "= equal to")
                                    ('< "< less than")
                                    ('> "> greater than"))
                                  "): "))
                         choice)))
               package-menu-mode)
  (package--ensure-package-menu-mode)
  (unless (equal predicate 'quit)
    (if (or (not version) (string-empty-p version))
        (package-menu--generate t t)
      (package-menu--filter-by
       (let ((fun (pcase predicate
                    ('= #'version-list-=)
                    ('< #'version-list-<)
                    ('> (lambda (a b) (not (version-list-<= a b))))
                    (_ (error "Unknown predicate: %s" predicate))))
             (ver (version-to-list version)))
         (lambda (pkg-desc)
           (funcall fun (package-desc-version pkg-desc) ver)))
       (format "versions:%s%s" predicate version)))))

(defun package-menu-filter-marked ()
  "Filter \"*Packages*\" buffer by non-empty mark.
Show only the packages that have been marked for installation or deletion.
Unlike other filters, this leaves the marks intact."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (widen)
  (let (found-entries mark pkg-id entry marks)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq mark (char-after))
        (unless (eq mark ?\s)
          (setq pkg-id (tabulated-list-get-id))
          (setq entry (package-menu--print-info-simple pkg-id))
          (push entry found-entries)
          ;; remember the mark
          (push (cons pkg-id mark) marks))
        (forward-line))
      (if found-entries
          (progn
            (setq tabulated-list-entries found-entries)
            (package-menu--display t nil)
            ;; redo the marks, but we must remember the marks!!
            (goto-char (point-min))
            (while (not (eobp))
              (setq mark (cdr (assq (tabulated-list-get-id) marks)))
              (tabulated-list-put-tag (char-to-string mark) t)))
        (user-error "No packages found")))))

(defun package-menu-filter-upgradable ()
  "Filter \"*Packages*\" buffer to show only upgradable packages."
  (interactive nil package-menu-mode)
  (let ((pkgs (mapcar #'car (package-menu--find-upgrades))))
    (package-menu--filter-by
     (lambda (pkg)
       (memql (package-desc-name pkg) pkgs))
     "upgradable")))

(defun package-menu-clear-filter ()
  "Clear any filter currently applied to the \"*Packages*\" buffer."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (package-menu--generate t t))

(defun package-list-packages-no-fetch ()
  "Display a list of packages.
Does not fetch the updated list of packages before displaying.
The list is displayed in a buffer named `*Packages*'."
  (interactive)
  (list-packages t))

;;;; Package menu mode.

(defvar-keymap package-menu-mode-map
  :doc "Local keymap for `package-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"   #'package-menu-describe-package
  "u"     #'package-menu-mark-unmark
  "DEL"   #'package-menu-backup-unmark
  "d"     #'package-menu-mark-delete
  "i"     #'package-menu-mark-install
  "U"     #'package-menu-mark-upgrades
  "r"     #'revert-buffer
  "~"     #'package-menu-mark-obsolete-for-deletion
  "w"     #'package-browse-url
  "b"     #'package-report-bug
  "x"     #'package-menu-execute
  "h"     #'package-menu-quick-help
  "H"     #'package-menu-hide-package
  "?"     #'package-menu-describe-package
  "("     #'package-menu-toggle-hiding
  "/ /"   #'package-menu-clear-filter
  "/ a"   #'package-menu-filter-by-archive
  "/ d"   #'package-menu-filter-by-description
  "/ k"   #'package-menu-filter-by-keyword
  "/ N"   #'package-menu-filter-by-name-or-description
  "/ n"   #'package-menu-filter-by-name
  "/ s"   #'package-menu-filter-by-status
  "/ v"   #'package-menu-filter-by-version
  "/ m"   #'package-menu-filter-marked
  "/ u"   #'package-menu-filter-upgradable)

(easy-menu-define package-menu-mode-menu package-menu-mode-map
  "Menu for `package-menu-mode'."
  '("Package"
    ["Describe Package" package-menu-describe-package :help "Display information about this package"]
    ["Open Package Website" package-browse-url
     :help "Open the website of this package"]
    ["Help" package-menu-quick-help :help "Show short key binding help for package-menu-mode"]
    "--"
    ["Refresh Package List" revert-buffer
     :help "Redownload the package archive(s)"
     :active (not package--downloads-in-progress)]
    ["Execute Marked Actions" package-menu-execute :help "Perform all the marked actions"]

    "--"
    ["Mark All Available Upgrades" package-menu-mark-upgrades
     :help "Mark packages that have a newer version for upgrading"
     :active (not package--downloads-in-progress)]
    ["Mark All Obsolete for Deletion" package-menu-mark-obsolete-for-deletion :help "Mark all obsolete packages for deletion"]
    ["Mark for Install" package-menu-mark-install :help "Mark a package for installation and move to the next line"]
    ["Mark for Deletion" package-menu-mark-delete :help "Mark a package for deletion and move to the next line"]
    ["Unmark" package-menu-mark-unmark :help "Clear any marks on a package and move to the next line"]

    "--"
    ("Filter Packages"
     ["Filter by Archive" package-menu-filter-by-archive
      :help
      "Prompt for archive(s), display only packages from those archives"]
     ["Filter by Description" package-menu-filter-by-description
      :help
      "Prompt for regexp, display only packages with matching description"]
     ["Filter by Keyword" package-menu-filter-by-keyword
      :help
      "Prompt for keyword(s), display only packages with matching keywords"]
     ["Filter by Name" package-menu-filter-by-name
      :help
      "Prompt for regexp, display only packages whose names match the regexp"]
     ["Filter by Name or Description" package-menu-filter-by-name-or-description
      :help
      "Prompt for regexp, display only packages whose name or description matches"]
     ["Filter by Status" package-menu-filter-by-status
      :help
      "Prompt for status(es), display only packages with those statuses"]
     ["Filter by Upgrades available" package-menu-filter-upgradable
      :help "Display only installed packages for which upgrades are available"]
     ["Filter by Version" package-menu-filter-by-version
      :help
      "Prompt for version and comparison operator, display only packages of matching versions"]
     ["Filter Marked" package-menu-filter-marked
      :help "Display only packages marked for installation or deletion"]
     ["Clear Filter" package-menu-clear-filter
      :help "Clear package list filtering, display the entire list again"])

    ["Hide by Regexp" package-menu-hide-package
     :help "Toggle visibility of obsolete and unwanted packages"]
    ["Display Older Versions" package-menu-toggle-hiding
     :style toggle :selected (not package-menu--hide-packages)
     :help "Display package even if a newer version is already installed"]

    "--"
    ["Quit" quit-window :help "Quit package selection"]
    ["Customize" (customize-group 'package)]))

(defconst package-menu-mode-line-format
  '((package-menu-mode-line-info
     (:eval (symbol-value 'package-menu-mode-line-info)))))

(defvar-local package-menu-mode-line-info nil
  "Variable which stores package-menu mode-line format.")

(defun package-menu--set-mode-line-format ()
  "Display package-menu mode-line."
  (when-let* ((buf (get-buffer "*Packages*"))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (setq package-menu-mode-line-info
            (let ((installed 0)
                  (new 0)
                  (total (length package-archive-contents))
                  (to-upgrade (length (package-menu--find-upgrades)))
                  (total-help "Total number of packages of all package archives")
                  (installed-help "Total number of packages installed")
                  (upgrade-help "Total number of packages to upgrade")
                  (new-help "Total number of packages added recently"))

              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((status (package-menu-get-status)))
                    (cond
                     ((member status
                              '("installed" "dependency" "unsigned"))
                      (setq installed (1+ installed)))
                     ((equal status "new")
                      (setq new (1+ new)))))
                  (forward-line)))

              (setq installed (number-to-string installed))
              (setq total (number-to-string total))
              (setq to-upgrade (number-to-string to-upgrade))

              (list
               " ["
               (propertize "Total: " 'help-echo total-help)
               (propertize total
                           'help-echo total-help
                           'face 'package-mode-line-total)
               " / "
               (propertize "Installed: " 'help-echo installed-help)
               (propertize installed
                           'help-echo installed-help
                           'face 'package-mode-line-installed)
               " / "
               (propertize "To Upgrade: " 'help-echo upgrade-help)
               (propertize to-upgrade
                           'help-echo upgrade-help
                           'face 'package-mode-line-to-upgrade)
               (when (> new 0)
                 (concat
                  " / "
                  (propertize "New: " 'help-echo new-help)
                  (propertize (number-to-string new)
                              'help-echo new-help
                              'face 'package-mode-line-new)))
               "] "))))))
(defvar package-menu--tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu
     #'package-menu-execute "package-menu/execute"
     map package-menu-mode-map)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu
     #'package-menu-mark-unmark "package-menu/unmark"
     map package-menu-mode-map)
    (tool-bar-local-item-from-menu
     #'package-menu-mark-install "package-menu/install"
     map package-menu-mode-map)
    (tool-bar-local-item-from-menu
     #'package-menu-mark-delete "package-menu/delete"
     map package-menu-mode-map)
    (tool-bar-local-item-from-menu
     #'package-menu-describe-package "package-menu/info"
     map package-menu-mode-map)
    (tool-bar-local-item-from-menu
     #'package-browse-url "package-menu/url"
     map package-menu-mode-map)
    (tool-bar-local-item
     "package-menu/upgrade" 'package-upgrade-all
    'package-upgrade-all
     map :help "Upgrade all the packages")
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item
     "search" 'isearch-forward 'search map
     :help "Search" :vert-only t)
    (tool-bar-local-item-from-menu
     #'revert-buffer "refresh"
     map package-menu-mode-map)
    (tool-bar-local-item-from-menu
     #'quit-window "close"
     map package-menu-mode-map)
    map))

(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of packages.
The most useful commands here are:

  `x': Install the package under point if it isn't already installed,
       and delete it if it's already installed,
  `i': mark a package for installation, and
  `d': mark a package for deletion.  Use the `x' command to perform the
       actions on the marked files.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  :interactive nil
  (setq mode-line-process '((package--downloads-in-progress ":Loading")
                            (package-menu--transaction-status
                             package-menu--transaction-status)))
  (setq-local mode-line-misc-info
              (append
               mode-line-misc-info
               package-menu-mode-line-format))
  (setq-local tool-bar-map package-menu--tool-bar-map)
  (setq tabulated-list-format
        `[("Package" ,package-name-column-width package-menu--name-predicate)
          ("Version" ,package-version-column-width package-menu--version-predicate)
          ("Status"  ,package-status-column-width  package-menu--status-predicate)
          ("Archive" ,package-archive-column-width package-menu--archive-predicate)
          ("Description" 0 package-menu--description-predicate)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook #'package-menu--refresh nil t)
  (tabulated-list-init-header)
  (setq revert-buffer-function 'package-menu--refresh-contents)
  (setf imenu-prev-index-position-function
        #'package--imenu-prev-index-position-function)
  (setf imenu-extract-index-name-function
        #'package--imenu-extract-index-name-function))

(defvar package-menu--hide-packages t
  "Whether available obsolete packages should be hidden.
Can be toggled with \\<package-menu-mode-map> \\[package-menu-toggle-hiding].
Installed obsolete packages are always displayed.")

(defun package-menu--refresh (&optional packages keywords)
  "Re-populate the `tabulated-list-entries'.
PACKAGES should be nil or t, which means to display all known packages.
KEYWORDS should be nil or a list of keywords."
  ;; Construct list of (PKG-DESC . STATUS).
  (unless packages (setq packages t))
  (let ((hidden-names (mapconcat #'identity package-hidden-regexps "\\|"))
        info-list)
    ;; Installed packages:
    (dolist (elt package-alist)
      (let ((name (car elt)))
        (when (or (eq packages t) (memq name packages))
          (dolist (pkg (cdr elt))
            (when (package--has-keyword-p pkg keywords)
              (push pkg info-list))))))

    ;; Built-in packages:
    (dolist (elt package--builtins)
      (let ((pkg  (package--from-builtin elt))
            (name (car elt)))
        (when (not (eq name 'emacs)) ; Hide the `emacs' package.
          (when (and (package--has-keyword-p pkg keywords)
                     (or package-list-unversioned
                         (package--bi-desc-version (cdr elt)))
                     (or (eq packages t) (memq name packages)))
            (push pkg info-list)))))

    ;; Available and disabled packages:
    (unless (equal package--old-archive-priorities package-archive-priorities)
      (package-read-all-archive-contents))
    (dolist (elt package-archive-contents)
      (let ((name (car elt)))
        ;; To be displayed it must be in PACKAGES;
        (when (and (or (eq packages t) (memq name packages))
                   ;; and we must either not be hiding anything,
                   (or (not package-menu--hide-packages)
                       (not package-hidden-regexps)
                       ;; or just not hiding this specific package.
                       (not (string-match hidden-names (symbol-name name)))))
          ;; Hide available-obsolete or low-priority packages.
          (dolist (pkg (package--remove-hidden (cdr elt)))
            (when (package--has-keyword-p pkg keywords)
              (push pkg info-list))))))

    ;; Print the result.
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar #'package-menu--print-info-simple info-list))))

(defun package--remove-hidden (pkg-list)
  "Filter PKG-LIST according to `package-archive-priorities'.
PKG-LIST must be a list of `package-desc' objects, all with the
same name, sorted by decreasing `package-desc-priority-version'.
Return a list of packages tied for the highest priority according
to their archives."
  (when pkg-list
    ;; Variable toggled with `package-menu-toggle-hiding'.
    (if (not package-menu--hide-packages)
        pkg-list
      (let ((installed (cadr (assq (package-desc-name (car pkg-list))
                                   package-alist))))
        (when installed
          (setq pkg-list
                (let ((ins-version (package-desc-version installed)))
                  (cl-remove-if (lambda (p) (version-list-< (package-desc-version p)
                                                       ins-version))
                                pkg-list))))
        (let ((filtered-by-priority
               (cond
                ((not package-menu-hide-low-priority)
                 pkg-list)
                ((eq package-menu-hide-low-priority 'archive)
                 (let (max-priority out)
                   (while pkg-list
                     (let ((p (pop pkg-list)))
                       (let ((priority (package-desc-priority p)))
                         (if (and max-priority (< priority max-priority))
                             (setq pkg-list nil)
                           (push p out)
                           (setq max-priority priority)))))
                   (nreverse out)))
                (pkg-list
                 (list (car pkg-list))))))
          (if (not installed)
              filtered-by-priority
            (let ((ins-version (package-desc-version installed)))
              (cl-remove-if (lambda (p) (or (version-list-= (package-desc-version p)
                                                            ins-version)
                                            (package-vc-p installed)))
                            filtered-by-priority))))))))

(defun package-menu-toggle-hiding ()
  "In Package Menu, toggle visibility of obsolete available packages.

Also hide packages whose name matches a regexp in user option
`package-hidden-regexps' (a list).  To add regexps to this list,
use `package-menu-hide-package'."
  (interactive nil package-menu-mode)
  (package--ensure-package-menu-mode)
  (setq package-menu--hide-packages
        (not package-menu--hide-packages))
  (if package-menu--hide-packages
      (message "Hiding obsolete or unwanted packages")
    (message "Displaying all packages"))
  (revert-buffer nil 'no-confirm))

(provide 'package-menu)
;;; package-menu.el ends here
