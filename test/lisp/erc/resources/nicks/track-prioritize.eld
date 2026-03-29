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
 (0.03 ":irc.foonet.org 353 tester = #chan :@fsbot alice tester")
 (0.01 ":irc.foonet.org 366 tester #chan :End of NAMES list")
 (0.01 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :tester, welcome!"))

((mode 10 "MODE #chan")
 (0.00 ":irc.foonet.org 324 tester #chan +Cnt")
 (0.01 ":irc.foonet.org 329 tester #chan 1703579802"))

((privmsg 10 "PRIVMSG control :1")
 (0.00 ":bob!~u@2q6ysndq32az6.irc JOIN #chan"))

((privmsg 10 "PRIVMSG control :2")
 (0.00 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :Most manifest, and not denied by himself."))

((privmsg 10 "PRIVMSG control :3")
 (0.00 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :To bed, to bed: there's knocking at the gate. Come, come, come, come, give me your hand. What's done cannot be undone."))

((privmsg 10 "PRIVMSG control :4")
 (0.00 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :Since you can cog, I'll play no more with you."))

((privmsg 10 "PRIVMSG control :5")
 (0.00 ":bob!~u@2q6ysndq32az6.irc PRIVMSG #chan :alice: Not to-night, good Iago: I have very poor and unhappy brains for drinking: I could well wish courtesy would invent some other custom of entertainment."))

((privmsg 10 "PRIVMSG control :6")
 (0.00 ":alice!~u@2q6ysndq32az6.irc PRIVMSG #chan :bob: Madam, my lord is gone, for ever gone."))

((privmsg 10 "PRIVMSG control :7")
 (0.00 ":bob!~u@2q6ysndq32az6.irc QUIT :later"))
