;;; -*- mode: lisp-data; -*-

;; https://github.com/DanielOaks/irc-parser-tests
((mask-match
  (tests
   ((mask . "*@127.0.0.1")
    (matches "coolguy!ab@127.0.0.1" "cooldud3!~bc@127.0.0.1")
    (fails "coolguy!ab@127.0.0.5" "cooldud3!~d@124.0.0.1"))
   ((mask . "cool*@*")
    (matches "coolguy!ab@127.0.0.1" "cooldud3!~bc@127.0.0.1" "cool132!ab@example.com")
    (fails "koolguy!ab@127.0.0.5" "cooodud3!~d@124.0.0.1"))
   ((mask . "cool!*@*")
    (matches "cool!guyab@127.0.0.1" "cool!~dudebc@127.0.0.1" "cool!312ab@example.com")
    (fails "coolguy!ab@127.0.0.1" "cooldud3!~bc@127.0.0.1" "koolguy!ab@127.0.0.5" "cooodud3!~d@124.0.0.1"))
   ((mask . "cool!?username@*")
    (matches "cool!ausername@127.0.0.1" "cool!~username@127.0.0.1")
    (fails "cool!username@127.0.0.1"))
   ((mask . "cool!a?*@*")
    (matches "cool!ab@127.0.0.1" "cool!abc@127.0.0.1")
    (fails "cool!a@127.0.0.1"))
   ((mask . "cool[guy]!*@*")
    (matches "cool[guy]!guy@127.0.0.1" "cool[guy]!a@example.com")
    (fails "coolg!ab@127.0.0.1" "cool[!ac@127.0.1.1"))))
 (msg-join
  (tests
   ((desc . "Simple test with verb and params.")
    (atoms
     (verb . "foo")
     (params "bar" "baz" "asdf"))
    (matches "foo bar baz asdf" "foo bar baz :asdf"))
   ((desc . "Simple test with source and no params.")
    (atoms
     (source . "src")
     (verb . "AWAY"))
    (matches ":src AWAY"))
   ((desc . "Simple test with source and empty trailing param.")
    (atoms
     (source . "src")
     (verb . "AWAY")
     (params ""))
    (matches ":src AWAY :"))
   ((desc . "Simple test with source.")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "asdf"))
    (matches ":coolguy foo bar baz asdf" ":coolguy foo bar baz :asdf"))
   ((desc . "Simple test with trailing param.")
    (atoms
     (verb . "foo")
     (params "bar" "baz" "asdf quux"))
    (matches "foo bar baz :asdf quux"))
   ((desc . "Simple test with empty trailing param.")
    (atoms
     (verb . "foo")
     (params "bar" "baz" ""))
    (matches "foo bar baz :"))
   ((desc . "Simple test with trailing param containing colon.")
    (atoms
     (verb . "foo")
     (params "bar" "baz" ":asdf"))
    (matches "foo bar baz ::asdf"))
   ((desc . "Test with source and trailing param.")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "asdf quux"))
    (matches ":coolguy foo bar baz :asdf quux"))
   ((desc . "Test with trailing containing beginning+end whitespace.")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "  asdf quux "))
    (matches ":coolguy foo bar baz :  asdf quux "))
   ((desc . "Test with trailing containing what looks like another trailing param.")
    (atoms
     (source . "coolguy")
     (verb . "PRIVMSG")
     (params "bar" "lol :) "))
    (matches ":coolguy PRIVMSG bar :lol :) "))
   ((desc . "Simple test with source and empty trailing.")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" ""))
    (matches ":coolguy foo bar baz :"))
   ((desc . "Trailing contains only spaces.")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "  "))
    (matches ":coolguy foo bar baz :  "))
   ((desc . "Param containing tab (tab is not considered SPACE for message splitting).")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "b	ar" "baz"))
    (matches ":coolguy foo b	ar baz" ":coolguy foo b	ar :baz"))
   ((desc . "Tag with no value and space-filled trailing.")
    (atoms
     (tags
      (asd . ""))
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "  "))
    (matches "@asd :coolguy foo bar baz :  "))
   ((desc . "Tags with escaped values.")
    (atoms
     (verb . "foo")
     (tags
      (a . "b\\and\nk")
      (d . "gh;764")))
    (matches "@a=b\\\\and\\nk;d=gh\\:764 foo" "@d=gh\\:764;a=b\\\\and\\nk foo"))
   ((desc . "Tags with escaped values and params.")
    (atoms
     (verb . "foo")
     (tags
      (a . "b\\and\nk")
      (d . "gh;764"))
     (params "par1" "par2"))
    (matches "@a=b\\\\and\\nk;d=gh\\:764 foo par1 par2" "@a=b\\\\and\\nk;d=gh\\:764 foo par1 :par2" "@d=gh\\:764;a=b\\\\and\\nk foo par1 par2" "@d=gh\\:764;a=b\\\\and\\nk foo par1 :par2"))
   ((desc . "Tag with long, strange values (including LF and newline).")
    (atoms
     (tags
      (foo . "\\\\;\\s \n"))
     (verb . "COMMAND"))
    (matches "@foo=\\\\\\\\\\:\\\\s\\s\\r\\n COMMAND"))))
 (msg-split
  (tests
   ((input . "foo bar baz asdf")
    (atoms
     (verb . "foo")
     (params "bar" "baz" "asdf")))
   ((input . ":coolguy foo bar baz asdf")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "asdf")))
   ((input . "foo bar baz :asdf quux")
    (atoms
     (verb . "foo")
     (params "bar" "baz" "asdf quux")))
   ((input . "foo bar baz :")
    (atoms
     (verb . "foo")
     (params "bar" "baz" "")))
   ((input . "foo bar baz ::asdf")
    (atoms
     (verb . "foo")
     (params "bar" "baz" ":asdf")))
   ((input . ":coolguy foo bar baz :asdf quux")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "asdf quux")))
   ((input . ":coolguy foo bar baz :  asdf quux ")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "  asdf quux ")))
   ((input . ":coolguy PRIVMSG bar :lol :) ")
    (atoms
     (source . "coolguy")
     (verb . "PRIVMSG")
     (params "bar" "lol :) ")))
   ((input . ":coolguy foo bar baz :")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "")))
   ((input . ":coolguy foo bar baz :  ")
    (atoms
     (source . "coolguy")
     (verb . "foo")
     (params "bar" "baz" "  ")))
   ((input . "@a=b;c=32;k;rt=ql7 foo")
    (atoms
     (verb . "foo")
     (tags
      (a . "b")
      (c . "32")
      (k . "")
      (rt . "ql7"))))
   ((input . "@a=b\\\\and\\nk;c=72\\s45;d=gh\\:764 foo")
    (atoms
     (verb . "foo")
     (tags
      (a . "b\\and\nk")
      (c . "72 45")
      (d . "gh;764"))))
   ((input . "@c;h=;a=b :quux ab cd")
    (atoms
     (tags
      (c . "")
      (h . "")
      (a . "b"))
     (source . "quux")
     (verb . "ab")
     (params "cd")))
   ((input . ":src JOIN #chan")
    (atoms
     (source . "src")
     (verb . "JOIN")
     (params "#chan")))
   ((input . ":src JOIN :#chan")
    (atoms
     (source . "src")
     (verb . "JOIN")
     (params "#chan")))
   ((input . ":src AWAY")
    (atoms
     (source . "src")
     (verb . "AWAY")))
   ((input . ":src AWAY ")
    (atoms
     (source . "src")
     (verb . "AWAY")))
   ((input . ":cool	guy foo bar baz")
    (atoms
     (source . "cool	guy")
     (verb . "foo")
     (params "bar" "baz")))
   ((input . ":coolguy!ag@net5work.admin PRIVMSG foo :bar baz")
    (atoms
     (source . "coolguy!ag@net5work.admin")
     (verb . "PRIVMSG")
     (params "foo" "bar baz")))
   ((input . ":coolguy!~ag@net05work.admin PRIVMSG foo :bar baz")
    (atoms
     (source . "coolguy!~ag@net05work.admin")
     (verb . "PRIVMSG")
     (params "foo" "bar baz")))
   ((input . "@tag1=value1;tag2;vendor1/tag3=value2;vendor2/tag4= :irc.example.com COMMAND param1 param2 :param3 param3")
    (atoms
     (tags
      (tag1 . "value1")
      (tag2 . "")
      (vendor1/tag3 . "value2")
      (vendor2/tag4 . ""))
     (source . "irc.example.com")
     (verb . "COMMAND")
     (params "param1" "param2" "param3 param3")))
   ((input . ":irc.example.com COMMAND param1 param2 :param3 param3")
    (atoms
     (source . "irc.example.com")
     (verb . "COMMAND")
     (params "param1" "param2" "param3 param3")))
   ((input . "@tag1=value1;tag2;vendor1/tag3=value2;vendor2/tag4 COMMAND param1 param2 :param3 param3")
    (atoms
     (tags
      (tag1 . "value1")
      (tag2 . "")
      (vendor1/tag3 . "value2")
      (vendor2/tag4 . ""))
     (verb . "COMMAND")
     (params "param1" "param2" "param3 param3")))
   ((input . "COMMAND")
    (atoms
     (verb . "COMMAND")))
   ((input . "@foo=\\\\\\\\\\:\\\\s\\s\\r\\n COMMAND")
    (atoms
     (tags
      (foo . "\\\\;\\s \n"))
     (verb . "COMMAND")))
   ((input . ":gravel.mozilla.org 432  #momo :Erroneous Nickname: Illegal characters")
    (atoms
     (source . "gravel.mozilla.org")
     (verb . "432")
     (params "#momo" "Erroneous Nickname: Illegal characters")))
   ((input . ":gravel.mozilla.org MODE #tckk +n ")
    (atoms
     (source . "gravel.mozilla.org")
     (verb . "MODE")
     (params "#tckk" "+n")))
   ((input . ":services.esper.net MODE #foo-bar +o foobar  ")
    (atoms
     (source . "services.esper.net")
     (verb . "MODE")
     (params "#foo-bar" "+o" "foobar")))
   ((input . "@tag1=value\\\\ntest COMMAND")
    (atoms
     (tags
      (tag1 . "value\\ntest"))
     (verb . "COMMAND")))
   ((input . "@tag1=value\\1 COMMAND")
    (atoms
     (tags
      (tag1 . "value1"))
     (verb . "COMMAND")))
   ((input . "@tag1=value1\\ COMMAND")
    (atoms
     (tags
      (tag1 . "value1"))
     (verb . "COMMAND")))
   ((input . "@tag1=1;tag2=3;tag3=4;tag1=5 COMMAND")
    (atoms
     (tags
      (tag1 . "5")
      (tag2 . "3")
      (tag3 . "4"))
     (verb . "COMMAND")))
   ((input . "@tag1=1;tag2=3;tag3=4;tag1=5;vendor/tag2=8 COMMAND")
    (atoms
     (tags
      (tag1 . "5")
      (tag2 . "3")
      (tag3 . "4")
      (vendor/tag2 . "8"))
     (verb . "COMMAND")))
   ((input . ":SomeOp MODE #channel :+i")
    (atoms
     (source . "SomeOp")
     (verb . "MODE")
     (params "#channel" "+i")))
   ((input . ":SomeOp MODE #channel +oo SomeUser :AnotherUser")
    (atoms
     (source . "SomeOp")
     (verb . "MODE")
     (params "#channel" "+oo" "SomeUser" "AnotherUser")))))
 (userhost-split
  (tests
   ((source . "coolguy")
    (atoms
     (nick . "coolguy")))
   ((source . "coolguy!ag@127.0.0.1")
    (atoms
     (nick . "coolguy")
     (user . "ag")
     (host . "127.0.0.1")))
   ((source . "coolguy!~ag@localhost")
    (atoms
     (nick . "coolguy")
     (user . "~ag")
     (host . "localhost")))
   ((source . "coolguy@127.0.0.1")
    (atoms
     (nick . "coolguy")
     (host . "127.0.0.1")))
   ((source . "coolguy!ag")
    (atoms
     (nick . "coolguy")
     (user . "ag")))
   ((source . "coolguy!ag@net5work.admin")
    (atoms
     (nick . "coolguy")
     (user . "ag")
     (host . "net5work.admin")))
   ((source . "coolguy!~ag@net05work.admin")
    (atoms
     (nick . "coolguy")
     (user . "~ag")
     (host . "net05work.admin")))))
 (validate-hostname
  (tests
   ((host . "irc.example.com")
    (valid . t))
   ((host . "i.coolguy.net")
    (valid . t))
   ((host . "irc-srv.net.uk")
    (valid . t))
   ((host . "iRC.CooLguY.NeT")
    (valid . t))
   ((host . "gsf.ds342.co.uk")
    (valid . t))
   ((host . "324.net.uk")
    (valid . t))
   ((host . "xn--bcher-kva.ch")
    (valid . t))
   ((host . "-lol-.net.uk")
    (valid . :false))
   ((host . "-lol.net.uk")
    (valid . :false))
   ((host . "_irc._sctp.lol.net.uk")
    (valid . :false))
   ((host . "irc")
    (valid . :false))
   ((host . "com")
    (valid . :false))
   ((host . "")
    (valid . :false)))))
