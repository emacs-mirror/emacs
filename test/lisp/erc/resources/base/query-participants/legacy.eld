;; -*- mode: lisp-data; -*-
((nick 10 "NICK tester"))
((user 10 "USER tester 0 * :tester")
 (0.00 ":irc.foonet.org 001 tester :Welcome to the foonet IRC Network tester")
 (0.01 ":irc.foonet.org 002 tester :Your host is irc.foonet.org, running version ergo-v2.11.1")
 (0.01 ":irc.foonet.org 003 tester :This server was created Sun, 26 May 2024 09:32:55 UTC")
 (0.01 ":irc.foonet.org 004 tester irc.foonet.org ergo-v2.11.1 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0.00 ":irc.foonet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# CHATHISTORY=25 ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX :are supported by this server")
 (0.01 ":irc.foonet.org 005 tester KICKLEN=390 MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=foonet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8ONLY WHOX :are supported by this server")
 (0.01 ":irc.foonet.org 005 tester draft/CHATHISTORY=25 :are supported by this server")
 (0.02 ":irc.foonet.org 251 tester :There are 0 users and 4 invisible on 1 server(s)")
 (0.00 ":irc.foonet.org 252 tester 0 :IRC Operators online")
 (0.00 ":irc.foonet.org 253 tester 0 :unregistered connections")
 (0.00 ":irc.foonet.org 254 tester 2 :channels formed")
 (0.00 ":irc.foonet.org 255 tester :I have 4 clients and 0 servers")
 (0.03 ":irc.foonet.org 265 tester 4 4 :Current local users 4, max 4")
 (0.00 ":irc.foonet.org 266 tester 4 4 :Current global users 4, max 4")
 (0.00 ":irc.foonet.org 422 tester :MOTD File is missing")
 (0.00 ":irc.foonet.org 221 tester +Zi")
 (0.00 ":irc.foonet.org NOTICE tester :This server is in debug mode and is logging all user I/O. If you do not wish for everything you send to be readable by the server owner(s), please disconnect."))

((mode 10 "MODE tester +i")
 (0.00 ":irc.foonet.org 221 tester +Zi")
 (0.07 ":dummy!~u@psu3bp52z9f34.irc PRIVMSG tester :hi"))

((join 10 "JOIN #chan")
 (0.02 ":tester!~u@psu3bp52z9f34.irc JOIN #chan")
 (0.06 ":irc.foonet.org 353 tester = #chan :bob dummy tester @fsbot alice")
 (0.01 ":irc.foonet.org 366 tester #chan :End of NAMES list")
 (0.00 ":alice!~u@zmmipd3xfii2w.irc PRIVMSG #chan :tester, welcome!")
 (0.03 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :tester, welcome!"))

((mode-chan 10 "MODE #chan")
 (0.02 ":irc.foonet.org 324 tester #chan +Cnt")
 (0.01 ":irc.foonet.org 329 tester #chan 1716715981"))

((privmsg-chan-a 10 "PRIVMSG #chan :hi channel")
 (0.06 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :Perchance, Iago, I will ne'er go home.")

 ;; Bob (now known) sends us a DM
 (0.07 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG tester :hi")
 (0.02 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :alice: He is most in the company of the right noble Claudio.")
 (0.05 ":alice!~u@zmmipd3xfii2w.irc PRIVMSG #chan :bob: Such were our faults; or then we thought them none.")
 (0.03 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :You, sir, I entertain you for one of my hundred; only I do not like the fashion of your garments: you will say, they are Persian attire; but let them be changed.")

 ;; Dummy parts
 (0.01 ":dummy!~u@psu3bp52z9f34.irc PART #chan :bye")
 (0.08 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :alice: To lay a complot to betray thy foes.")

 ;; Bob quits
 (0.02 ":bob!~u@zmmipd3xfii2w.irc QUIT :later")
 (0.08 ":alice!~u@zmmipd3xfii2w.irc PRIVMSG #chan :bob: He was famous, sir, in his profession, and it was his great right to be so: Gerard de Narbon."))

;; Bob rejoins
((privmsg-chan-b 10 "PRIVMSG #chan :bob gone")

 (0.04 ":bob!~u@zmmipd3xfii2w.irc JOIN #chan")
 (0.01 ":alice!~u@zmmipd3xfii2w.irc PRIVMSG #chan :bob, welcome back!")
 (0.03 ":bob!~u@zmmipd3xfii2w.irc PRIVMSG #chan :Our states are forfeit: seek not to undo us."))

((part 10 "PART #chan :\2ERC\2")
 (0.02 ":tester!~u@psu3bp52z9f34.irc PART #chan :\2ERC\2"))
