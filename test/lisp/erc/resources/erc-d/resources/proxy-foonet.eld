;;; -*- mode: lisp-data -*-

((pass 10.0 "PASS " (? ?:) network ":changeme"))
((nick 1.2 "NICK tester"))

((user 2.2 "USER user 0 * :tester")
 (0.015 ":" fqdn " 001 tester :Welcome to the FOO Network tester")
 (0.014 ":" fqdn " 002 tester :Your host is " fqdn)
 (0.013 ":" fqdn " 003 tester :This server was created just now")
 (0.012 ":" fqdn " 004 tester " fqdn " BERios CEIRabehiklmnoqstv Iabehkloqv")
 (0.011 ":" fqdn " 005 tester MODES NETWORK=" net " NICKLEN=32 PREFIX=(qaohv)~&@%+"
                 " :are supported by this server")
 (0.010 ":" fqdn " 251 tester :There are 3 users and 0 invisible on 1 server(s)")
 (0.009 ":" fqdn " 252 tester 0 :IRC Operators online")
 (0.008 ":" fqdn " 253 tester 0 :unregistered connections")
 (0.007 ":" fqdn " 254 tester 1 :channels formed")
 (0.006 ":" fqdn " 255 tester :I have 3 clients and 0 servers")
 (0.005 ":" fqdn " 265 tester 3 3 :Current local users 3, max 3")
 (0.004 ":" fqdn " 266 tester 3 3 :Current global users 3, max 3")
 (0.003 ":" fqdn " 422 tester :MOTD File is missing"))

((mode-user 1.2 "MODE tester +i")
 (0.002 ":" fqdn " 221 tester +Zi")
 (0.001 ":" fqdn " 306 tester :You have been marked as being away"))
