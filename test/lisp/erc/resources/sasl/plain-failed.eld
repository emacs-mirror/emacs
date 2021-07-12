;; -*- mode: lisp-data; -*-
((cap-req 10 "CAP REQ :sasl"))
((nick 1 "NICK tester"))
((user 1 "USER tester 0 * :tester")
 (0.0 ":irc.foonet.org NOTICE * :*** Looking up your hostname...")
 (0.0 ":irc.foonet.org NOTICE * :*** Found your hostname")
 (0.0 ":irc.foonet.org CAP * ACK :cap-notify sasl"))

((authenticate-plain 3.2 "AUTHENTICATE PLAIN")
 (0.0 ":irc.foonet.org AUTHENTICATE +"))

((authenticate-gimme 3.2 "AUTHENTICATE AHRlc3RlcgB3cm9uZw==")
 (0.0 ":irc.foonet.org 900 * * tester :You are now logged in as tester")
 (0.0 ":irc.foonet.org 904 * :SASL authentication failed: Invalid account credentials"))

((cap-end 3.2 "CAP END"))
