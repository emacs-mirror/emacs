;;; -*- mode: lisp-data -*-
((pass 10.0 "PASS " (? ?:) "changeme"))
((nick 2.2 "NICK tester"))

((user 2.2 "USER " user " " (ignored digit "*") " :" realname)
 (0.0 ":" dom " 001 " nick " :Welcome to the Internet Relay Network tester")
 (0.0 ":" dom " 002 " nick " :Your host is " dom)
 (0.0 ":" dom " 003 " nick " :This server was created just now")
 (0.0 ":" dom " 004 " nick " " dom " BERios CEIRabehiklmnoqstv Iabehkloqv")
 (0.0 ":" dom " 005 " nick " MODES NETWORK=ExampleOrg NICKLEN=32 PREFIX=(qaohv)~&@%+ :are supported by this server")
 (0.0 ":" dom " 251 " nick " :There are 3 users and 0 invisible on 1 server(s)")
 (0.0 ":" dom " 252 " nick " 0 :IRC Operators online")
 (0.0 ":" dom " 253 " nick " 0 :unregistered connections")
 (0.0 ":" dom " 254 " nick " 1 :channels formed")
 (0.0 ":" dom " 255 " nick " :I have 3 clients and 0 servers")
 (0.0 ":" dom " 265 " nick " 3 3 :Current local users 3, max 3")
 (0.0 ":" dom " 266 " nick " 3 3 :Current global users 3, max 3")
 (0.0 ":" dom " 422 " nick " :MOTD File is missing"))

((mode-user 2.2 "MODE tester +i")
 (0.0 ":" dom " 221 " nick " +Zi")

 (0.0 ":" dom " 306 " nick " :You have been marked as being away")
 (0.0 ":" nick "!~" nick "@localhost JOIN #chan")
 (0.0 ":" dom " 353 alice = #chan :+alice @bob")
 (0.0 ":" dom " 366 alice #chan :End of NAMES list"))

((mode 2.2 "MODE #chan")
 (0.1 ":bob!~bob@example.org PRIVMSG #chan :" nick ": hey"))
