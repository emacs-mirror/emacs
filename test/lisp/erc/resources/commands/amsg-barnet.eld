;; -*- mode: lisp-data; -*-
((nick 10 "NICK tester"))
((user 10 "USER user 0 * :unknown")
 (0 ":irc.barnet.org 001 tester :Welcome to the barnet IRC Network tester")
 (0 ":irc.barnet.org 002 tester :Your host is irc.barnet.org, running version oragono-2.6.0-7481bf0385b95b16")
 (0 ":irc.barnet.org 003 tester :This server was created Tue, 04 May 2021 05:06:19 UTC")
 (0 ":irc.barnet.org 004 tester irc.barnet.org oragono-2.6.0-7481bf0385b95b16 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0 ":irc.barnet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX KICKLEN=390 :are supported by this server")
 (0 ":irc.barnet.org 005 tester MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=barnet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8MAPPING=rfc8265 UTF8ONLY WHOX :are supported by this server")
 (0 ":irc.barnet.org 005 tester draft/CHATHISTORY=100 :are supported by this server")
 (0 ":irc.barnet.org 251 tester :There are 0 users and 3 invisible on 1 server(s)")
 (0 ":irc.barnet.org 252 tester 0 :IRC Operators online")
 (0 ":irc.barnet.org 253 tester 0 :unregistered connections")
 (0 ":irc.barnet.org 254 tester 1 :channels formed")
 (0 ":irc.barnet.org 255 tester :I have 3 clients and 0 servers")
 (0 ":irc.barnet.org 265 tester 3 3 :Current local users 3, max 3")
 (0 ":irc.barnet.org 266 tester 3 3 :Current global users 3, max 3")
 (0 ":irc.barnet.org 422 tester :MOTD File is missing"))

((mode-user 10 "MODE tester +i")
 (0 ":irc.barnet.org 221 tester +i")
 (0 ":irc.barnet.org NOTICE tester :This server is in debug mode and is logging all user I/O. If you do not wish for everything you send to be readable by the server owner(s), please disconnect."))

((join 10 "JOIN #bar")
 (0 ":tester!~u@jnu48g2wrycbw.irc JOIN #bar")
 (0 ":irc.barnet.org 353 tester = #bar :@mike joe tester")
 (0 ":irc.barnet.org 366 tester #bar :End of NAMES list"))

((mode-bar 10 "MODE #bar")
 (0 ":irc.barnet.org 324 tester #bar +nt")
 (0 ":irc.barnet.org 329 tester #bar 1620104779")
 (0.1 ":mike!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :tester, welcome!")
 (0.1 ":joe!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :tester, welcome!")
 (0.1 ":mike!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :joe: Whipp'd first, sir, and hang'd after.")
 (0.1 ":joe!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :mike: We have yet many among us can gripe as hard as Cassibelan; I do not say I am one, but I have a hand. Why tribute ? why should we pay tribute ? If C sar can hide the sun from us with a blanket, or put the moon in his pocket, we will pay him tribute for light; else, sir, no more tribute, pray you now."))

((privmsg-2 10 "PRIVMSG #bar :2 barnet only")
 (0.1 ":mike!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :joe: Double and treble admonition, and still forfeit in the same kind ? This would make mercy swear, and play the tyrant.")
 (0.1 ":joe!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :mike: And secretly to greet the empress' friends."))

((privmsg-4 10 "PRIVMSG #bar :\1ACTION 4 barnet only\1")
 (0.1 ":mike!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :joe: You have not been inquired after: I have sat here all day.")
 (0.1 ":joe!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :mike: That same Berowne I'll torture ere I go."))

((privmsg-5 10 "PRIVMSG #bar :5 all nets"))

((privmsg-6 10 "PRIVMSG #bar :\1ACTION 6 all nets\1")
 (0.1 ":mike!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :joe: For mine own part,no offence to the general, nor any man of quality,I hope to be saved.")
 (0.1 ":joe!~u@kd7gmjbnbkn8c.irc PRIVMSG #bar :mike: Mehercle! if their sons be ingenuous, they shall want no instruction; if their daughters be capable, I will put it to them. But, vir sapit qui pauca loquitur. A soul feminine saluteth us."))

((quit 5 "QUIT :\2ERC\2")
 (0 ":tester!~u@jnu48g2wrycbw.irc QUIT :Quit"))

((drop 0 DROP))
