(defpackage :lem/deepl
  (:use :cl :lem))
(in-package :lem/deepl)

(defun auth-key ()
  (or (config :deepl-auth-key)
      (setf (config :deepl-auth-key)
            (prompt-for-string "Auth key: "))))

(defun ja-char-p (char)
  (cond ((char<= (code-char 12354) ;#\HIRAGANA_LETTER_A
                 char
                 (code-char 12435) ;#\HIRAGANA_LETTER_N
                 ))
        ((char<= (code-char 12450) ;#\KATAKANA_LETTER_A
                 char
                 (code-char 12531) ;#\KATAKANA_LETTER_N
                 ))
        ((or (<= #x4E00
                 (char-code char)
                 #x9FFF)
             (find char "仝々〆〇ヶ")))))

(defun ja-text-p (text)
  (loop :for c :across text
        :when (ja-char-p c)
        :return t))

(defun translate (text &key (source-lang "EN") (target-lang "JA"))
  (let ((response
          (dex:post "https://api-free.deepl.com/v2/translate"
                    :headers `(("Authorization" . ,(format nil "DeepL-Auth-Key ~A" (auth-key))))
                    :content `(("text" . ,text)
                               ("target_lang" . ,target-lang)
                               ("source_lang" . ,source-lang)))))
    (with-output-to-string (out)
      (loop :for tr :in (gethash "translations" (yason:parse response))
            :do (fresh-line out)
                (write-string (gethash "text" tr) out)))))

(defun output-to-buffer (source-text translated-text buffer)
  (with-open-stream (stream (make-buffer-output-stream (buffer-end-point buffer)))
    (fresh-line stream)
    (format stream "---------- source ----------~%")
    (format stream "~A~%" source-text)
    (format stream "---------- target ----------~%")
    (format stream "~A~2%" translated-text))
  buffer)

(define-command deepl-translate-region (start end &optional is-replace) (:region :universal-nil)
  (let* ((source-text (points-to-string start end))
         (is-japanese (ja-text-p source-text))
         (translated-text (translate (ppcre:regex-replace-all "\\s+" source-text " ")
                                     :source-lang (if is-japanese "JA" "EN")
                                     :target-lang (if is-japanese "EN" "JA")))
         (buffer (output-to-buffer source-text
                                   translated-text
                                   (make-buffer "*deepl*"))))
    (cond (is-replace
           (delete-between-points start end)
           (insert-string start translated-text))
          (t
           (pop-to-buffer buffer)))))
