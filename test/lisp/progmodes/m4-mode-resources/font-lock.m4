# comment
# <- font-lock-comment-delimiter-face
# ^ font-lock-comment-face

  #comment
# ^ font-lock-comment-delimiter-face
#  ^ font-lock-comment-face

dnl discarded
# <- font-lock-comment-delimiter-face
#   ^ font-lock-comment-face

  dnl`'discarded
# ^^^ font-lock-comment-delimiter-face
#    ^^^ font-lock-comment-face

define(`a', `$#`$*'$@')
# <- font-lock-keyword-face
#            ^^ ^^ ^^ font-lock-variable-use-face

  m4_define(`b', ``$0$1$2$3$4`'$5$6$7$8$9'')
# ^^^^^^^^^ font-lock-keyword-face
#                  ^^^^^^^^^^  ^^^^^^^^^^ font-lock-variable-use-face

  m4`'_define(`c', `')
# ^^^^^^^^^^^ !font-lock-keyword-face

m4_`'define(`d', `')
# <- !font-lock-keyword-face
#    ^^^^^^ font-lock-keyword-face
