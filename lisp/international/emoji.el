;;; emoji.el --- Inserting emojis  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun

;; Package-Requires: ((emacs "28.0") (transient "0.3.7"))
;; Package-Version: 0.1

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

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'multisession)
(require 'generate-lisp-file)

(defgroup emoji nil
  "Inserting Emojis."
  :version "29.1"
  :group 'play)

(defface emoji-list-header
  '((default :weight bold :inherit variable-pitch))
  "Face for emoji list headers."
  :version "29.1")

(defface emoji
  '((t :height 2.0))
  "Face used when displaying an emoji."
  :version "29.1")

(defface emoji-with-derivations
  '((((background dark))
     (:background "#202020" :inherit emoji))
    (((background light))
     (:background "#e0e0e0" :inherit emoji)))
  "Face for emojis that have derivations."
  :version "29.1")

(defvar emoji-alternate-names nil
  "Alist of emojis and lists of alternate names for the emojis.
Each element in the alist should have the emoji (as a string) as
the first element, and the rest of the elements should be strings
representing names.  For instance:

  (\"🤗\" \"hug\" \"hugging\" \"kind\")")

(defvar emoji--labels nil)
(defvar emoji--all-bases nil)
(defvar emoji--derived nil)
(defvar emoji--names (make-hash-table :test #'equal))
(define-multisession-variable emoji--recent (list "😀" "😖"))
(defvar emoji--insert-buffer)

;;;###autoload (autoload 'emoji-insert "emoji" nil t)
(transient-define-prefix emoji-insert ()
  "Choose and insert an emoji glyph."
  :variable-pitch t
  [:class transient-columns
   :setup-children emoji--setup-suffixes
   :description emoji--group-description]
  (interactive "*")
  (emoji--init)
  (emoji--setup-prefix 'emoji-insert "Emoji" nil
                       `(("Recent" ,@(multisession-value emoji--recent))
                         ,@emoji--labels)))

;;;###autoload (autoload 'emoji-recent "emoji" nil t)
(transient-define-prefix emoji-recent ()
  "Choose and insert one of the recently-used emoji glyphs."
  :variable-pitch t
  [:class transient-columns
   :setup-children emoji--setup-suffixes
   :description emoji--group-description]
  (interactive "*")
  (emoji--init)
  (emoji--setup-prefix 'emoji-recent "Recent" t
                       (multisession-value emoji--recent)))

;;;###autoload (autoload 'emoji-search "emoji" nil t)
(transient-define-prefix emoji-search (glyph derived)
  "Choose and insert an emoji glyph by typing its Unicode name.
This command prompts for an emoji name, with completion, and
inserts it.  It recognizes the Unicode Standard names of emoji,
and also consults the `emoji-alternate-names' alist."
  :variable-pitch t
  [:class transient-columns
   :setup-children emoji--setup-suffixes
   :description emoji--group-description]
  (interactive
   (progn (barf-if-buffer-read-only)
          (emoji--init)
          (let ((cons (emoji--read-emoji)))
            (list (car cons) (cdr cons)))))
  (if derived
      (emoji--setup-prefix 'emoji-search "Choose Emoji"
                           (list glyph)
                           (cons glyph derived))
    (emoji--add-recent glyph)
    (insert glyph)))

(defclass emoji--narrow (transient-suffix)
  ((title :initarg :title)
   (done-derived :initarg :done-derived)
   (children :initarg :children)))

(defun emoji--setup-prefix (command title done-derived spec)
  (transient-setup
   command nil nil
   :scope (if (eq transient-current-command command)
              (cons (oref (transient-suffix-object) title)
                    (oref (transient-suffix-object) done-derived))
            (cons title done-derived))
   :value (if (eq transient-current-command command)
              (oref (transient-suffix-object) children)
            spec)))

(defun emoji--setup-suffixes (_)
  (transient-parse-suffixes
   (oref transient--prefix command)
   (pcase-let ((`(,title . ,done-derived) (oref transient--prefix scope)))
     (emoji--layout (oref transient--prefix command) title
                    (oref transient--prefix value) done-derived))))

(defun emoji--group-description ()
  (car (oref transient--prefix scope)))

(transient-define-suffix emoji-insert-glyph (glyph)
  "Insert the emoji you selected."
  (interactive
   (list (if (string-prefix-p "emoji-" (symbol-name transient-current-command))
             (oref (transient-suffix-object) description)
           (car (multisession-value emoji--recent))))
   not-a-mode)
  (emoji--add-recent glyph)
  (insert glyph))

;;;###autoload
(defun emoji-list ()
  "List emojis and allow selecting and inserting one of them.
Select the emoji by typing \\<emoji-list-mode-map>\\[emoji-list-select] on its picture.
The glyph will be inserted into the buffer that was current
when the command was invoked."
  (interactive)
  (let ((buf (current-buffer)))
    (emoji--init)
    (switch-to-buffer (get-buffer-create "*Emoji*"))
    (setq-local emoji--insert-buffer buf)
    ;; Don't regenerate the buffer if it already exists -- this will
    ;; leave point where it was the last time it was used.
    (when (zerop (buffer-size))
      (let ((inhibit-read-only t))
        (emoji-list-mode)
        (emoji--list-generate nil (cons nil emoji--labels))
        (goto-char (point-min))))))

;;;###autoload
(defun emoji-describe (glyph &optional interactive)
  "Display the name of the grapheme cluster composed from GLYPH.
GLYPH should be a string of one or more characters which together
produce an emoji.  Interactively, GLYPH is the emoji at point (it
could also be any character, not just emoji).

If called from Lisp, return the name as a string; return nil if
the name is not known."
  (interactive
   (list (if (eobp)
             (error "No glyph under point")
           (let ((comp (find-composition (point) (1+ (point)))))
             (if comp
                 (buffer-substring-no-properties (car comp) (cadr comp))
               (buffer-substring-no-properties (point) (1+ (point))))))
         t))
  (require 'emoji-labels)
  (if (not interactive)
      ;; Don't return a name for non-compositions when called
      ;; non-interactively.
      (gethash glyph emoji--names)
    ;; Give a name for (pretty much) any glyph, including non-emojis.
    (let ((name (emoji--name glyph)))
      (if (not name)
          (message "No known name for \"%s\"" glyph)
        (message "The name of \"%s\" is \"%s\"" glyph name)))))

(defun emoji--list-generate (name alist)
  (let ((width (/ (window-width) 5))
        (mname (pop alist)))
    (if (consp (car alist))
        ;; Recurse.
        (mapcar (lambda (elem)
                  (emoji--list-generate (if name
                                            (concat name " > " mname)
                                          mname)
                                        elem))
                alist)
      ;; Output this block of emojis.
      (insert (propertize
               (if (zerop (length name))
                   mname
                 (concat name " > " mname))
               'face 'emoji-list-header)
              "\n\n")
      (cl-loop for i from 0
               for glyph in alist
               do
               (when (and (plusp i)
                          (zerop (mod i width)))
                 (insert "\n"))
               (insert
                (propertize
                 (emoji--fontify-glyph glyph)
                 'emoji-glyph glyph
                 'help-echo (emoji--name glyph))))
      (insert "\n\n"))))

(defun emoji--fontify-glyph (glyph &optional done-derived)
  (propertize glyph 'face
              (if (and (not (or (eq done-derived t)
                                (member glyph done-derived)))
                       (gethash glyph emoji--derived))
                  ;; If this emoji has derivations, use a special face
                  ;; to tell the user.
                  'emoji-with-derivations
                ;; Normal emoji.
                'emoji)))

(defun emoji--name (glyph)
  (or (gethash glyph emoji--names)
      (char-to-name (aref glyph 0))))

(defvar-keymap emoji-list-mode-map
  "RET" #'emoji-list-select
  "<mouse-2>" #'emoji-list-select
  "h" #'emoji-list-help
  "<follow-link>" 'mouse-face)

(define-derived-mode emoji-list-mode special-mode "Emoji"
  "Mode to display emojis."
  :interactive nil
  (setq-local truncate-lines t))

;;;###autoload (autoload 'emoji-list-select "emoji" nil t)
(transient-define-prefix emoji-list-select (event)
  "Select the emoji under point."
  :variable-pitch t
  [:class transient-columns
   :setup-children emoji--setup-suffixes
   :description emoji--group-description]
  (interactive (list last-nonmenu-event) emoji-list-mode)
  (mouse-set-point event)
  (let ((glyph (get-text-property (point) 'emoji-glyph)))
    (unless glyph
      (error "No emoji under point"))
    (let ((buf emoji--insert-buffer))
      (quit-window)
      (if (buffer-live-p buf)
          (progn
            (switch-to-buffer buf)
            (barf-if-buffer-read-only))
        (error "Buffer disappeared")))
    (let ((derived (gethash glyph emoji--derived)))
      (if derived
          (emoji--setup-prefix 'emoji-list-select "Choose Emoji"
                               (list glyph)
                               (cons glyph derived))
        (emoji--add-recent glyph)
        (insert glyph)))))

(defun emoji-list-help ()
  "Display the name of the emoji at point."
  (interactive nil emoji-list-mode)
  (let ((glyph (get-text-property (point) 'emoji-glyph)))
    (unless glyph
      (error "No emoji here"))
    (let ((name (emoji--name glyph)))
      (if (not name)
          (error "Emoji name is unknown")
        (message "%s" name)))))

;;;###autoload
(defun emoji--init (&optional force inhibit-adjust)
  (when (or (not emoji--labels)
            force)
    (unless force
      (ignore-errors (require 'emoji-labels)))
    ;; The require should define the variable, but in case the .el
    ;; file doesn't exist (yet), parse the file now.
    (when (or force
              (not emoji--labels))
      (setq emoji--derived (make-hash-table :test #'equal))
      (emoji--parse-emoji-test)))
  (when (and (not inhibit-adjust)
             (not emoji--all-bases))
    (setq emoji--all-bases (make-hash-table :test #'equal))
    (emoji--adjust-displayable (cons "Emoji" emoji--labels))))

(defvar emoji--font nil)

(defun emoji--adjust-displayable (alist)
  "Remove glyphs we don't have fonts for."
  (let ((emoji--font nil))
    (emoji--adjust-displayable-1 alist)))

(defun emoji--adjust-displayable-1 (alist)
  (if (consp (caddr alist))
      (dolist (child (cdr alist))
        (emoji--adjust-displayable-1 child))
    (while (cdr alist)
      (let ((glyph (cadr alist)))
        ;; Store all the emojis for later retrieval by
        ;; the search feature.
        (when-let* ((name (emoji--name glyph)))
          (setf (gethash (downcase name) emoji--all-bases) glyph))
        (if (display-graphic-p)
            ;; Remove glyphs we don't have in graphical displays.
            (if (let ((char (elt glyph 0)))
                  (if emoji--font
                      (font-has-char-p emoji--font char)
                    (when-let* ((font (car (internal-char-font nil char))))
                      (setq emoji--font font))))
                (setq alist (cdr alist))
              ;; Remove the element.
              (setcdr alist (cddr alist)))
          ;; We don't have font info on non-graphical displays.
          (if (let ((char (elt glyph 0)))
                ;; FIXME.  Some grapheme clusters display more or less
                ;; correctly in the terminal, but we don't really know
                ;; which ones.  None of these display totally
                ;; correctly, though, so should they be filtered out?
                (char-displayable-p char))
              (setq alist (cdr alist))
            ;; Remove the element.
            (setcdr alist (cddr alist))))))))

(defun emoji--parse-emoji-test ()
  (setq emoji--labels nil)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "../admin/unidata/emoji-test.txt"
                                            data-directory))
    (unless (re-search-forward "^# +group:" nil t)
      (error "Can't find start of data"))
    (beginning-of-line)
    (setq emoji--names (make-hash-table :test #'equal))
    (let ((derivations (make-hash-table :test #'equal))
          (case-fold-search t)
          (glyphs nil)
          group subgroup)
      (while (not (eobp))
        (cond
         ((looking-at "# +group: \\(.*\\)")
          (setq group (match-string 1)
                subgroup nil))
         ((looking-at "# +subgroup: \\(.*\\)")
          (setq subgroup (match-string 1)))
         ((looking-at
           "\\([[:xdigit:] \t]+\\); *\\([^ \t]+\\)[ \t]+#.*?E[.0-9]+ +\\(.*\\)")
          (let* ((codes (match-string 1))
                 (qualification (match-string 2))
                 (name (match-string 3))
                 (glyph (mapconcat
                         (lambda (code)
                           (string (string-to-number code 16)))
                         (split-string codes))))
            (push (list name qualification group subgroup glyph) glyphs))))
        (forward-line 1))
      ;; We sort the data so that the "person foo" variant comes
      ;; first, so that that becomes the key.
      (setq glyphs
            (sort (nreverse glyphs)
                  (lambda (g1 g2)
                    (and (equal (nth 2 g1) (nth 2 g2))
                         (equal (nth 3 g1) (nth 3 g2))
                         (< (emoji--score (car g1))
                            (emoji--score (car g2)))))))
      ;; Get the derivations.
      (cl-loop for (name qualification group subgroup glyph) in glyphs
               for base = (emoji--base-name name derivations)
               do
               ;; Special-case flags.
               (when (equal base "flag")
                 (setq base name))
               ;; Register all glyphs to that we can look up their names
               ;; later.
               (setf (gethash glyph emoji--names) name)
               ;; For the interface, we only care about the fully qualified
               ;; emojis.
               (when (equal qualification "fully-qualified")
                 (when (equal base name)
                   (emoji--add-to-group group subgroup glyph))
                 ;; Create mapping from base glyph name to name of
                 ;; derived glyphs.
                 (setf (gethash base derivations)
                       (nconc (gethash base derivations) (list glyph)))))
      ;; Finally create the mapping from the base glyphs to derived ones.
      (setq emoji--derived (make-hash-table :test #'equal))
      (maphash (lambda (_k v)
                 (setf (gethash (car v) emoji--derived)
                       (cdr v)))
               derivations))))

(defun emoji--score (string)
  (if (string-match-p "person\\|people"
                      (replace-regexp-in-string ":.*" "" string))
      0
    1))

(defun emoji--add-to-group (group subgroup glyph)
  ;; "People & Body" is very large; split it up.
  (cond
   ((equal group "People & Body")
    (if (or (string-match "\\`person" subgroup)
            (equal subgroup "family"))
        (emoji--add-glyph glyph "People"
                          (if (equal subgroup "family")
                              (list subgroup)
                            ;; Avoid "Person person".
                            (cdr (emoji--split-subgroup subgroup))))
      (emoji--add-glyph glyph "Body" (emoji--split-subgroup subgroup))))
   ;; "Smileys & Emotion" also seems sub-optimal.
   ((equal group "Smileys & Emotion")
    (if (equal subgroup "emotion")
        (emoji--add-glyph glyph "Emotion" nil)
      (let ((subs (emoji--split-subgroup subgroup)))
        ;; Remove one level of menus in the face case.
        (when (equal (car subs) "face")
          (pop subs))
        (emoji--add-glyph glyph "Smileys" subs))))
   ;; Don't modify the rest.
   (t
    (emoji--add-glyph glyph group (emoji--split-subgroup subgroup)))))

(defun emoji--generate-file (&optional file)
  "Generate an .el file with emoji mapping data and write it to FILE."
  ;; Running from Makefile.
  (unless file
    (setq file (pop command-line-args-left)))
  (emoji--init t t)
  ;; Weed out the elements that are empty.
  (let ((glyphs nil))
    (maphash (lambda (k v)
               (unless v
                 (push k glyphs)))
             emoji--derived)
    (dolist (glyph glyphs)
      (remhash glyph emoji--derived)))
  (with-temp-buffer
    (generate-lisp-file-heading file 'emoji--generate-file)
    (insert ";; Copyright © 1991-2021 Unicode, Inc.
;; Generated from Unicode data files by emoji.el.
;; The source for this file is found in the admin/unidata/emoji-test.txt
;; file in the Emacs sources.  The Unicode data files are used under the
;; Unicode Terms of Use, as contained in the file copyright.html in that
;; same directory.\n\n")
    (dolist (var '(emoji--labels emoji--derived emoji--names))
      (insert (format "(defconst %s '" var))
      (pp (symbol-value var) (current-buffer))
      (insert (format "\n) ;; End %s\n\n" var)))
    (generate-lisp-file-trailer file)
    (write-region (point-min) (point-max) file)))

(defun emoji--base-name (name derivations)
  (let* ((base (replace-regexp-in-string ":.*" "" name)))
    (catch 'found
      ;; If we have (for instance) "person golfing", and we're adding
      ;; "man golfing", make the latter a derivation of the former.
      (let ((non-binary (replace-regexp-in-string
                         "\\`\\(m[ae]n\\|wom[ae]n\\) " "" base)))
        (dolist (prefix '("person " "people " ""))
          (let ((key (concat prefix non-binary)))
            (when (gethash key derivations)
              (throw 'found key)))))
      ;; We can also have the gender at the end of the string, like
      ;; "merman" and "pregnant woman".
      (let ((non-binary (replace-regexp-in-string
                         "\\(m[ae]n\\|wom[ae]n\\|maid\\)\\'" "" base)))
        (dolist (suffix '(" person" "person" ""))
          (let ((key (concat non-binary suffix)))
            (when (gethash key derivations)
              (throw 'found key)))))
      ;; Just return the base.
      base)))

(defun emoji--split-subgroup (subgroup)
  (let ((prefixes '("face" "hand" "person" "animal" "plant"
                    "food" "place")))
    (cond
     ((string-match (concat "\\`" (regexp-opt prefixes) "-") subgroup)
      ;; Split these subgroups into hierarchies.
      (list (substring subgroup 0 (1- (match-end 0)))
            (substring subgroup (match-end 0))))
     ((equal subgroup "person")
      (list "person" "age"))
     (t
      (list subgroup)))))

(defun emoji--add-glyph (glyph main subs)
  (let (parent elem)
    ;; Useless category.
    (unless (member main '("Component"))
      (unless (setq parent (assoc main emoji--labels))
        (setq emoji--labels (append emoji--labels
                                    (list (setq parent (list main))))))
      (setq elem parent)
      (while subs
        (unless (setq elem (assoc (car subs) parent))
          (nconc parent (list (setq elem (list (car subs))))))
        (pop subs)
        (setq parent elem))
      (nconc elem (list glyph)))))

(defun emoji--layout (command title spec done-derived)
  (let ((has-subs (consp (cadr spec))))
    (emoji--columnize
     (if has-subs
         (cl-loop for (key desc . glyphs) in (emoji--compute-prefix spec)
                  collect
                  (list key
                        (emoji--compute-name (cons desc glyphs))
                        command
                        :class 'emoji--narrow
                        :title (concat title " > " desc)
                        :done-derived (or (string-suffix-p "Recent" desc)
                                          done-derived)
                        :children glyphs))
       (cl-loop for glyph in spec
                for char in (emoji--char-sequence)
                for key = (string char)
                for derived = (and (not (or (eq done-derived t)
                                            (member glyph done-derived)))
                                   (gethash glyph emoji--derived))
                collect
                (if derived
                    (list key
                          (emoji--fontify-glyph glyph done-derived)
                          command
                          :class 'emoji--narrow
                          :title (concat title " " glyph)
                          :done-derived (or (eq done-derived t)
                                            (cons glyph done-derived))
                          :children (cons glyph derived))
                  (list key
                        (emoji--fontify-glyph glyph done-derived)
                        'emoji-insert-glyph))))
     (if has-subs 2 8))))

(defun emoji--char-sequence ()
  (append (number-sequence ?a ?z)
          (number-sequence ?A ?Z)
          (number-sequence ?0 ?9)
          (number-sequence ?! ?/)))

(defun emoji--add-recent (glyph)
  "Add GLYPH to the set of recently used emojis."
  (let ((recent (multisession-value emoji--recent)))
    (set-text-properties 0 (length glyph) nil glyph)
    (setq recent (delete glyph recent))
    (push glyph recent)
    ;; Shorten the list.
    (when-let* ((tail (nthcdr 30 recent)))
      (setcdr tail nil))
    (setf (multisession-value emoji--recent) recent)))

(defun emoji--columnize (list columns)
  "Split LIST into COLUMN columns."
  (cl-loop with length = (ceiling (/ (float (length list)) columns))
           for i upto columns
           for part on list by (lambda (l) (nthcdr length l))
           collect (apply #'vector (seq-take part length))))

(defun emoji--compute-prefix (alist)
  "Compute characters to use for entries in ALIST.
We prefer the earliest unique letter."
  (cl-loop with taken = (make-hash-table)
           for entry in alist
           for name = (car entry)
           collect (cons (cl-loop for char across (concat
                                                   (downcase name)
                                                   (upcase name))
                                  while (gethash char taken)
                                  finally (progn
                                            (setf (gethash char taken) t)
                                            (cl-return (string char))))
                         entry)))

(defun emoji--compute-name (entry)
  "Add example emojis to the name."
  (let* ((name (concat (car entry) " "))
         (children (emoji--flatten entry))
         (length (length name))
         (max 30))
    (cl-loop for i from 0 upto 20
             ;; Choose from all the children.
             while (< length max)
             do (cl-loop for child in children
                         for glyph = (elt child i)
                         while (< length max)
                         when glyph
                         do (setq name (concat name glyph)
                                  length (+ length 2))))
    (if (= (length name) max)
        ;; Make an ellipsis signal that we've not exhausted the
        ;; possibilities.
        (concat name "…")
      name)))

(defun emoji--flatten (alist)
  (pop alist)
  (if (consp (cadr alist))
      (cl-loop for child in alist
               append (emoji--flatten child))
    (list alist)))

(defun emoji--split-long-lists (alist)
  (let ((whole alist))
    (pop alist)
    (if (consp (cadr alist))
        ;; Descend.
        (cl-loop for child in alist
                 do (emoji--split-long-lists child))
      ;; We have a list.
      (when (length> alist 77)
        (setcdr whole
                (cl-loop for prefix from ?a
                         for bit on alist by (lambda (l) (nthcdr 77 l))
                         collect (cons (concat (string prefix) "-group")
                                       (seq-take bit 77))))))))

(defun emoji--read-emoji ()
  ;; Use the list of names.
  (let* ((table
          (if (not emoji-alternate-names)
              ;; If we don't have alternate names, do the efficient version.
              emoji--all-bases
            ;; Compute all the (possibly non-unique) names.
            (let ((table nil))
              (maphash
               (lambda (name glyph)
                 (push (concat name "\t" glyph) table))
               emoji--all-bases)
              (dolist (elem emoji-alternate-names)
                (dolist (name (cdr elem))
                  (push (concat name "\t" (car elem)) table)))
              (sort table #'string<))))
         (name
          (completing-read
           "Insert emoji: "
           (completion-table-with-metadata
            table
            `((affixation-function
               ;; Add the glyphs to the start of the displayed
               ;; strings when TAB-ing.
               . ,(lambda (strings)
                    (mapcar
                     (lambda (name)
                       (if emoji-alternate-names
                           (list name "" "")
                         (list name
                               (concat
                                (or (gethash name emoji--all-bases) " ")
                                "\t")
                               "")))
                     strings)))))
           nil t)))
    (if (plusp (length name))
        (let ((glyph (if emoji-alternate-names
                         (cadr (split-string name "\t"))
                       (gethash name emoji--all-bases))))
          (cons glyph (gethash glyph emoji--derived)))
      (user-error "You didn't specify an emoji"))))

(defvar-keymap emoji-zoom-map
  "+" #'emoji-zoom-increase
  "-" #'emoji-zoom-decrease
  "0" #'emoji-zoom-reset)

;;;###autoload
(defun emoji-zoom-increase (&optional factor)
  "Increase the size of the character under point.
FACTOR is the multiplication factor for the size."
  (interactive)
  (set-transient-map emoji-zoom-map t #'redisplay "Zoom with %k")
  (unless (eobp)
    (let* ((factor (or factor 1.1))
           (old (get-text-property (point) 'face))
           ;; The text property is either a named face, or a plist
           ;; with :height, or a list starting with such a plist,
           ;; followed by one or more faces.
           (newheight (* (or (and (consp old)
                                  (or (plist-get (car old) :height)
                                      (plist-get old :height)))
                             1.0)
                         factor))
           (inhibit-read-only t))
      (with-silent-modifications
        (if (consp old)
            (add-text-properties
             (point) (1+ (point))
             (list 'face
                   (cond
                    ((eq (car old) :height)
                     (plist-put (copy-sequence old) :height newheight))
                    ((plistp (car old))
                     (cons (plist-put (car old) :height newheight)
                           (cdr old)))
                    (t
                     (append (list (list :height newheight)) old)))
                   'rear-nonsticky t))
          (add-face-text-property (point) (1+ (point))
                                  (list :height newheight))
          (put-text-property (point) (1+ (point))
                             'rear-nonsticky t))))))

;;;###autoload
(defun emoji-zoom-decrease ()
  "Decrease the size of the character under point."
  (interactive)
  (emoji-zoom-increase 0.9))

;;;###autoload
(defun emoji-zoom-reset ()
  "Reset the size of the character under point."
  (interactive)
  (with-silent-modifications
    (let ((old (get-text-property (point) 'face)))
      (when (and (consp old)
                 (remove-text-properties (point) (1+ (point)) '(rear-nonsticky nil)))
        (if (eq (car old) :height)
            (remove-text-properties (point) (1+ (point)) '(face nil))
          (add-text-properties (point) (1+ (point)) (list 'face
                                                      (cdr old))))))))

(provide 'emoji)

;;; emoji.el ends here
