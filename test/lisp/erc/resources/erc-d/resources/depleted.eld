;;; -*- mode: lisp-data -*-

((pass 10.0 "PASS :changeme"))

((~fake 3.2 "FAKE ")
 (0.1 ":irc.example.org FAKE irc.example.com :ok"))

((nick 0.2 "NICK tester"))

((user 0.2 "USER user 0 * :tester")
 (0 ":irc.example.org 001 tester :Welcome to the Internet tester")
 (0 ":irc.example.org 422 tester :MOTD File is missing"))
