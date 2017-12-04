;;; bind-chord.el --- key-chord binding helper for use-package-chords

;; Copyright (C) 2015-2017 Justin Talbott

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-chords
;; Version: 0.2
;; Package-Requires: ((bind-key "1.0") (key-chord "0.6"))
;; Filename: bind-chord.el
;; License: GNU General Public License version 3, or (at your option) any later version
;;

;;; Commentary:
;;

;;; Code:

(require 'bind-key)
(require 'key-chord nil t)

;;;###autoload
(defmacro bind-chord (chord command &optional keymap)
  "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed)."
  (let ((key1 (logand 255 (aref chord 0)))
        (key2 (logand 255 (aref chord 1))))
    (if (eq key1 key2)
        `(bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)
      `(progn
         (bind-key (vector 'key-chord ,key1 ,key2) ,command ,keymap)
         (bind-key (vector 'key-chord ,key2 ,key1) ,command ,keymap)))))

;;;###autoload
(defmacro bind-chords (&rest args)
  "Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let* ((map (plist-get args :map))
         (maps (if (listp map) map (list map)))
         (key-bindings (progn
                         (while (keywordp (car args))
                           (pop args)
                           (pop args))
                         args)))
    (macroexp-progn
     (apply
      #'nconc
      (mapcar (lambda (form)
                (if maps
                    (mapcar
                     #'(lambda (m)
                         `(bind-chord ,(car form) ',(cdr form) ,m)) maps)
                  `((bind-chord ,(car form) ',(cdr form)))))
              key-bindings)))))

(provide 'bind-chord)

;;; bind-chord.el ends here
