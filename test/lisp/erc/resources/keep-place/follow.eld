;; -*- mode: lisp-data; -*-
((nick 10 "NICK tester"))
((user 10 "USER tester 0 * :tester")
 (0.00 ":irc.foonet.org 001 tester :Welcome to the foonet IRC Network tester")
 (0.01 ":irc.foonet.org 002 tester :Your host is irc.foonet.org, running version ergo-v2.11.1")
 (0.01 ":irc.foonet.org 003 tester :This server was created Tue, 26 Dec 2023 08:36:35 UTC")
 (0.01 ":irc.foonet.org 004 tester irc.foonet.org ergo-v2.11.1 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0.00 ":irc.foonet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# CHATHISTORY=1000 ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX :are supported by this server")
 (0.01 ":irc.foonet.org 005 tester KICKLEN=390 MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=foonet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8ONLY WHOX :are supported by this server")
 (0.01 ":irc.foonet.org 005 tester draft/CHATHISTORY=1000 :are supported by this server")
 (0.01 ":irc.foonet.org 251 tester :There are 0 users and 4 invisible on 1 server(s)")
 (0.01 ":irc.foonet.org 252 tester 0 :IRC Operators online")
 (0.00 ":irc.foonet.org 253 tester 0 :unregistered connections")
 (0.00 ":irc.foonet.org 254 tester 2 :channels formed")
 (0.00 ":irc.foonet.org 255 tester :I have 4 clients and 0 servers")
 (0.00 ":irc.foonet.org 265 tester 4 4 :Current local users 4, max 4")
 (0.00 ":irc.foonet.org 266 tester 4 4 :Current global users 4, max 4")
 (0.03 ":irc.foonet.org 422 tester :MOTD File is missing")
 (0.01 ":irc.foonet.org NOTICE tester :This server is in debug mode and is logging all user I/O. If you do not wish for everything you send to be readable by the server owner(s), please disconnect."))

((mode 10 "MODE tester +i"))

((join 10 "JOIN #chan")
 (0.01 ":irc.foonet.org 221 tester +i")
 (0.01 ":tester!~u@p64eqfwvvbxrk.irc JOIN #chan")
 (0.03 ":irc.foonet.org 353 tester = #chan :@fsbot bob alice tester")
 (0.01 ":irc.foonet.org 366 tester #chan :End of NAMES list")
 (0.00 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :tester, welcome!")
 (0.01 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :tester, welcome!"))

((join 10 "JOIN #spam")
 (0.00 ":tester!~u@p64eqfwvvbxrk.irc JOIN #spam")
 (0.06 ":irc.foonet.org 353 tester = #spam :@fsbot bob alice tester")
 (0.01 ":irc.foonet.org 366 tester #spam :End of NAMES list")
 (0.03 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #spam :tester, welcome!")
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #spam :tester, welcome!"))

((mode 10 "MODE #chan")
 (0.00 ":irc.foonet.org 324 tester #chan +Cnt")
 (0.02 ":irc.foonet.org 329 tester #chan 1703579802")
 (0.02 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: Madam, my lord is gone, for ever gone.")
 (0.10 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :The kinder we, to give them thanks for nothing."))

((mode 10 "MODE #spam")
 (0.00 ":irc.foonet.org 324 tester #spam +Cnt")
 (0.02 ":irc.foonet.org 329 tester #spam 1703579805")
 (0.02 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :Most manifest, and not denied by himself.")
 (0.02 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: To bed, to bed: there's knocking at the gate. Come, come, come, come, give me your hand. What's done cannot be undone.")
 (0.02 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: And what I spake, I spake it to my face.")
 (0.08 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: Since you can cog, I'll play no more with you.")
 (0.06 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: The little casket bring me hither.")
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: Not to-night, good Iago: I have very poor and unhappy brains for drinking: I could well wish courtesy would invent some other custom of entertainment.")
 (0.02 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :Yes, faith will I, Fridays and Saturdays and all."))

((privmsg 10 "PRIVMSG #spam :one")
 (0.03 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: This is the first truth that e'er thine own tongue was guilty of.")
 (0.02 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: Drown the lamenting fool in sea-salt tears.")

 ;; Insert some lines ^ before rendezvous, so #chan can update scrolltobottom.
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #spam :Ay, the heads of the maids, or their maidenheads; take it in what sense thou wilt.")

 (0.05 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: And work confusion on his enemies.")
 (0.06 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: Truly, she must be given, or the marriage is not lawful."))

((privmsg 10 "PRIVMSG #spam :two")
 (0.02 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :To be whipped; and yet a better love than my master.")
 (0.06 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :And duty in his service perishing.")

 ;; Second check point.
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #spam :Cause they take vengeance of such kind of men.")

 (0.03 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: No egma, no riddle, no l'envoy; no salve in the mail, sir. O! sir, plantain, a plain plantain: no l'envoy, no l'envoy: no salve, sir, but a plantain.")
 (0.03 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :Signior Iachimo will not from it. Pray, let us follow 'em."))

((privmsg 10 "PRIVMSG #spam :three")
 ;; Third check point.
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #spam :Moved.")
 (0.01 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :Ready."))
