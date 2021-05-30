;; -*- mode: lisp-data; -*-
((pass 10 "PASS :foonet:changeme"))
((nick 1 "NICK tester"))

((user 1 "USER user 0 * :tester")
 (0 ":irc.foonet.org 001 tester :Welcome to the foonet IRC Network tester")
 (0 ":irc.foonet.org 002 tester :Your host is irc.foonet.org, running version oragono-2.6.0-7481bf0385b95b16")
 (0 ":irc.foonet.org 003 tester :This server was created Mon, 10 May 2021 00:58:22 UTC")
 (0 ":irc.foonet.org 004 tester irc.foonet.org oragono-2.6.0-7481bf0385b95b16 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0 ":irc.foonet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX KICKLEN=390 :are supported by this server")
 (0 ":irc.foonet.org 005 tester MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=foonet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8MAPPING=rfc8265 UTF8ONLY WHOX :are supported by this server")
 (0 ":irc.foonet.org 005 tester draft/CHATHISTORY=100 :are supported by this server")
 (0 ":irc.foonet.org 251 tester :There are 0 users and 3 invisible on 1 server(s)")
 (0 ":irc.foonet.org 252 tester 0 :IRC Operators online")
 (0 ":irc.foonet.org 254 tester 1 :channels formed")
 (0 ":irc.foonet.org 255 tester :I have 3 clients and 0 servers")
 (0 ":irc.foonet.org 265 tester 3 3 :Current local users 3, max 3")
 (0 ":irc.foonet.org 266 tester 3 3 :Current global users 3, max 3")
 (0 ":irc.foonet.org 422 tester :MOTD File is missing"))

((mode-user 10.2 "MODE tester +i"))
;; No mode answer ^

((join 3 "JOIN #chan")
 (0 ":tester!~u@q6ddatxcq6txy.irc JOIN #chan")
 (0 ":irc.foonet.org 353 tester = #chan :@alice bob tester")
 (0 ":irc.foonet.org 366 tester #chan :End of NAMES list")
 (0.1 ":bob!~u@q6ddatxcq6txy.irc PRIVMSG #chan :tester, welcome!")
 (0 ":alice!~u@q6ddatxcq6txy.irc PRIVMSG #chan :tester, welcome!"))

((mode 3 "MODE #chan")
 (0 ":irc.foonet.org 324 tester #chan +nt")
 (0 ":irc.foonet.org 329 tester #chan 1620608304")
 (0.1 ":bob!~u@q6ddatxcq6txy.irc PRIVMSG #chan :alice: Pray you, sir, deliver me this paper.")
 (0.1 ":alice!~u@q6ddatxcq6txy.irc PRIVMSG #chan :bob: Wake when some vile thing is near."))

((quit 3 "QUIT :\2ERC\2"))

((drop 0 DROP))
