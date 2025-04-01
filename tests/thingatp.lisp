(defpackage :lem-tests/thingatp
  (:use :cl :rove :lem/thingatp :lem))


(in-package :lem-tests/thingatp)

(deftest url-type
  (let ((url-list-correct
	  '("https://mypage.com/directory"
	    "https://mypage.com/directory.txt"
	    "https://mypage.com/directory.php"
	    "https://mypage.com/my/dire/c/tory"
	    "https://mypage.com/my/dire/c/tory.txt"

	    "file://mypage/myfile.htm"
	    ))
	(url-list-wrong
	  '("not an url"
	    "3"
	    "this is a number"
	    "htttttttp hey"
	    "http:_+____"
	    "none")))
    (loop :for correct :in url-list-correct
	  :for wrong :in url-list-wrong
	  :do (progn
		(ok (typep correct 'url) )
		(ok (not (typep wrong 'url)))))))

(deftest path-type
  (let ((url-list-correct
	  '("/usr/bin/"
	    "/boot/initrd.img"
	    "/"
	    "/usr"
	    "/usr/sbin/mkfs"
	    ))
	(url-list-wrong
	  '("not an url"
	    "3"
	    "___ 23 path_"
	    "/ / / / / hey"
	    "none")))
    (loop :for correct :in url-list-correct
	  :for wrong :in url-list-wrong
	  :do (progn
		(ok (typep correct 'path) )
		(ok (not (typep wrong 'path)))))))
