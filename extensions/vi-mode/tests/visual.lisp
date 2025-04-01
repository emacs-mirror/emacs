(defpackage :lem-vi-mode/tests/visual
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/visual)

(in-readtable :interpol-syntax)

(deftest visual-switch
  (with-fake-interface ()
    (with-vi-buffer (#?"[a]bc\ndef\n")
      (cmd "v")
      (ok (buf= #?"<[a]>bc\ndef\n"))
      (cmd "l")
      (ok (buf= #?"<a[b]>c\ndef\n"))
      (cmd "v")
      (ok (buf= #?"a[b]c\ndef\n"))
      (cmd "v")
      (ok (buf= #?"a<[b]>c\ndef\n"))
      (cmd "V")
      (ok (buf= #?"<a[b]c\n>def\n"))
      (cmd "j")
      (ok (buf= #?"<abc\nd[e]f\n>"))
      (cmd "l<C-v>")
      (ok (buf= #?"a<bc>\nd<e[f]>\n"))
      (cmd "v")
      (ok (buf= #?"a<bc\nde[f]>\n"))
      (cmd "v")
      (ok (buf= #?"abc\nde[f]\n")))))

(deftest text-objects
  (with-fake-interface ()
    (testing "vaw"
      (with-vi-buffer (#?"  [f]oo\n")
        (cmd "vaw")
        (ok (buf= #?"  <fo[o]>\n")))
      (with-vi-buffer (#?"  [ ]foo bar   baz  \n")
        (cmd "vaw")
        (ok (buf= #?"<   fo[o]> bar   baz  \n")))
      (with-vi-buffer (#?" < [ ]> foo bar   baz  \n")
        (cmd "aw")
        (ok (buf= #?" <   fo[o]> bar   baz  \n")))
      (with-vi-buffer (#?" < [ ]>foo bar   baz  \n")
        (cmd "aw")
        (ok (buf= #?" <  foo[ ]>bar   baz  \n")))
      (with-vi-buffer (#?" <[ ] > foo bar   baz  \n")
        (cmd "aw")
        (ok (buf= #?"<[ ]  > foo bar   baz  \n")))
      (with-vi-buffer (#?" f[o]o bar \n")
        (cmd "vaw")
        (ok (buf= #?" <foo[ ]>bar \n")))
      (with-vi-buffer (#?" f[o]o bar \n")
        (cmd "v2aw")
        (ok (buf= #?" <foo bar[ ]>\n")))
      (with-vi-buffer (#?" f[o]o bar \n")
        (cmd "v3aw")
        (ok (buf= #?" <foo bar [\n]>")))
      (with-vi-buffer (#?" foo b[a]r \n")
        (cmd "Vaw")
        (ok (buf= #?" foo <bar[ ]>\n")))
      (with-vi-buffer (#?" foo b[a]r\n")
        (cmd "vaw")
        (ok (buf= #?" foo< ba[r]>\n"))))
    (testing "viw"
      (with-vi-buffer (#?" [ ] foo bar\n")
        (cmd "viw")
        (ok (buf= #?"<  [ ]>foo bar\n")))
      (with-vi-buffer (#?"f[o]o bar\n")
        (cmd "viw")
        (ok (buf= #?"<fo[o]> bar\n")))
      (with-vi-buffer (#?"f[o]o bar\n")
        (cmd "v3iw")
        (ok (buf= #?"<foo ba[r]>\n"))))
    (testing "va\""
      (with-vi-buffer (#?' "f[o]o" "bar" ')
        (cmd "va\"")
        (ok (buf= #?' <"foo"[ ]>"bar" '))
        (cmd "a\"")
        (ok (buf= #?' <"foo" "bar"[ ]>')))
      (with-vi-buffer (#?' <"f[o]>o" ')
        (cmd "a\"")
        (ok (buf= #?' <"foo"[ ]>')))
      (with-vi-buffer (#?' "f<[o]o"> ')
        (cmd "a\"")
        (ok (buf= #?'<[ ]"foo"> '))))
    (testing "vi\""
      (with-vi-buffer (#?' "f[o]o" ')
        (cmd "vi\"")
        (ok (buf= #?' "<fo[o]>" '))))))

(deftest visual-block
  (with-fake-interface ()
    (testing "right-bottom"
      (with-vi-buffer (#?"apple\nor[a]nge\ngrape\n")
        (cmd "<C-v>jl")
        (ok (buf= #?"apple\nor<an>ge\ngr<a[p]>e\n"))))
    (testing "right-top"
      (with-vi-buffer (#?"apple\nor[a]nge\ngrape\n")
        (cmd "<C-v>kl")
        (ok (buf= #?"ap<p[l]>e\nor<an>ge\ngrape\n"))))
    (testing "left-top"
      (with-vi-buffer (#?"apple\nor[a]nge\ngrape\n")
        (cmd "<C-v>kh")
        (ok (buf= #?"a<[p]p>le\no<ra>nge\ngrape\n"))))
    (testing "left-bottom"
      (with-vi-buffer (#?"apple\nor[a]nge\ngrape\n")
        (cmd "<C-v>jh")
        (ok (buf= #?"apple\no<ra>nge\ng<[r]a>pe\n"))))
    (testing "delete"
      (with-vi-buffer (#?"[a]pple\norange\ngrape\n")
        (cmd "<C-v>jllld")
        (ok (buf= #?"[e]\nge\ngrape\n"))))))

(deftest visual-block-japanease
  (with-fake-interface ()
    (testing "移動"
      (testing "右下"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>jl")
          (ok (buf= #?"あいうえお\nか<きく>けこ\nさ<し[す]>せそ\n"))))

      (testing "右上"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>kl")
          (ok (buf= #?"あ<い[う]>えお\nか<きく>けこ\nさしすせそ\n"))))

      (testing "左上"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>kh")
          (ok (buf= #?"<[あ]い>うえお\n<かき>くけこ\nさしすせそ\n"))))

      (testing "左下"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>jh")
          (ok (buf= #?"あいうえお\n<かき>くけこ\n<[さ]し>すせそ\n")))))
    
    (testing "削除"
      (testing "右下"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>jlx")
          (ok (buf= #?"あいうえお\nか[け]こ\nさせそ\n"))))
      
      (testing "右上"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>lkx")
          (ok (buf= #?"あ[え]お\nかけこ\nさしすせそ\n"))))
      
      (testing "左上"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>hkx")
          (ok (buf= #?"[う]えお\nくけこ\nさしすせそ\n"))))
      
      (testing "左下"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>hjx")
          (ok (buf= #?"あいうえお\n[く]けこ\nすせそ\n")))))
    
    (testing "挿入"
      (testing "右"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>jlxlp")
          (ok (buf= #?"あいうえお\nかけこ[き]く\nさせそしす\n"))))

      (testing "左"
        (with-vi-buffer (#?"あいうえお\nか[き]くけこ\nさしすせそ\n")
          (cmd "<C-v>jlxhP")
          (ok (buf= #?"あいうえお\n[き]くかけこ\nしすさせそ\n")))))))

(deftest visual-swap-points
  (with-fake-interface ()
    (testing "visual-char"
      (with-vi-buffer (#?"a<[b]cd>e\nfghij\n")
        (cmd "o")
        (ok (buf= #?"a<bc[d]>e\nfghij\n"))
        (cmd "j")
        (ok (buf= #?"a<bcde\nfgh[i]>j\n"))
        (cmd "o")
        (ok (buf= #?"a<[b]cde\nfghi>j\n"))
        (cmd "O")
        (ok (buf= #?"a<bcde\nfgh[i]>j\n"))))
    (testing "visual-line"
      (with-vi-buffer (#?"ab[c]de\nfghij\n")
        (cmd "Vjl")
        (ok (buf= #?"<abcde\nfgh[i]j\n>"))
        (cmd "o")
        (ok (buf= #?"<ab[c]de\nfghij\n>"))
        (cmd "O")
        (ok (buf= #?"<abcde\nfgh[i]j\n>"))))
    (testing "visual-block"
      (with-vi-buffer (#?"a[b]cd\nefgh\n")
        (cmd "<C-v>jl")
        (ok (buf= #?"a<bc>d\ne<f[g]>h\n"))
        (cmd "o")
        (ok (buf= #?"a<[b]c>d\ne<fg>h\n"))
        (cmd "o")
        (ok (buf= #?"a<bc>d\ne<f[g]>h\n"))
        (cmd "O")
        (ok (buf= #?"a<bc>d\ne<[f]g>h\n"))
        (cmd "O")
        (ok (buf= #?"a<bc>d\ne<f[g]>h\n"))))))
