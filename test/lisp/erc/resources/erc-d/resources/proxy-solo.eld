;;; -*- mode: lisp-data -*-

((pass 10.0 "PASS " (? ?:) "changeme"))
((nick 0.2 "NICK tester"))

((user 0.2 "USER user 0 * :" (group (+ alpha)) eos)
 (0 ":*status!znc@znc.in NOTICE " nick " :You have no networks configured."
    " Use /znc AddNetwork <network> to add one.")
 (0 ":irc.znc.in 001 " nick " :Welcome " nick "!"))
