;;; -*- mode: lisp-data -*-

((pass 10.0 "PASS " (? ?:) "changeme"))
((nick 0.2 "NICK tester"))

((user 0.2 "USER user 0 * :tester")
 (0 ":irc.example.org 001 tester :Welcome to the Internet Relay Network tester")
 (0 ":irc.example.org 002 tester :Your host is irc.example.org")
 (0 ":irc.example.org 003 tester :This server was created just now")
 (0 ":irc.example.org 004 tester irc.example.org BERios CEIRabehiklmnoqstv Iabehkloqv")
 (0 ":irc.example.org 005 tester MODES NETWORK=ExampleOrg NICKLEN=32 PREFIX=(qaohv)~&@%+"
    " :are supported by this server")
 (0 ":irc.example.org 251 tester :There are 3 users and 0 invisible on 1 server(s)")
 ;; Just to mix thing's up (force handler to schedule timer)
 (0.1 ":irc.example.org 252 tester 0 :IRC Operators online")
 (0 ":irc.example.org 253 tester 0 :unregistered connections")
 (0 ":irc.example.org 254 tester 1 :channels formed")
 (0 ":irc.example.org 255 tester :I have 3 clients and 0 servers")
 (0.1 ":irc.example.org 265 tester 3 3 :Current local users 3, max 3")
 (0 ":irc.example.org 266 tester 3 3 :Current global users 3, max 3")
 (0 ":irc.example.org 422 tester :MOTD File is missing"))

((mode-user 5 "MODE tester +i")
 (0 ":irc.example.org 221 tester +Zi")
 (0 ":irc.example.org 306 tester :You have been marked as being away")
 (0 ":tester!~tester@localhost JOIN #chan")
 (0 ":irc.example.org 353 alice = #chan :+alice!~alice@example.com @%+bob!~bob@example.org")
 (0 ":irc.example.org 366 alice #chan :End of NAMES list"))

;; Some comment (to prevent regression)
((mode-chan 1.2 "MODE #chan")
 (0.1 ":bob!~bob@example.org PRIVMSG #chan :hey"))
