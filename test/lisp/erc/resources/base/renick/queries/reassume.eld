;; -*- mode: lisp-data; -*-
((nick 10 "NICK tester"))
((user 10 "USER user 0 * :unknown")
 (0.00 ":irc.foonet.org 001 tester :Welcome to the foonet IRC Network tester")
 (0.00 ":irc.foonet.org 002 tester :Your host is irc.foonet.org, running version ergo-v2.11.1")
 (0.00 ":irc.foonet.org 003 tester :This server was created Thu, 09 May 2024 05:19:24 UTC")
 (0.00 ":irc.foonet.org 004 tester irc.foonet.org ergo-v2.11.1 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0.00 ":irc.foonet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# CHATHISTORY=25 ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX :are supported by this server")
 (0.00 ":irc.foonet.org 005 tester KICKLEN=390 MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=foonet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8ONLY WHOX :are supported by this server")
 (0.01 ":irc.foonet.org 005 tester draft/CHATHISTORY=25 :are supported by this server")
 (0.00 ":irc.foonet.org 251 tester :There are 0 users and 6 invisible on 1 server(s)")
 (0.00 ":irc.foonet.org 252 tester 0 :IRC Operators online")
 (0.00 ":irc.foonet.org 253 tester 0 :unregistered connections")
 (0.00 ":irc.foonet.org 254 tester 2 :channels formed")
 (0.00 ":irc.foonet.org 255 tester :I have 6 clients and 0 servers")
 (0.00 ":irc.foonet.org 265 tester 6 6 :Current local users 6, max 6")
 (0.00 ":irc.foonet.org 266 tester 6 6 :Current global users 6, max 6")
 (0.00 ":irc.foonet.org 422 tester :MOTD File is missing"))

((mode-user 10 "MODE tester +i")
 (0.00 ":irc.foonet.org 221 tester +i")
 (0.00 ":irc.foonet.org NOTICE tester :This server is in debug mode and is logging all user I/O. If you do not wish for everything you send to be readable by the server owner(s), please disconnect."))

((join 10 "JOIN #chan")
 (0.03 ":irc.foonet.org 221 tester +i") ; dupe
 (0.00 ":tester!~u@s8ceryiqkkcxk.irc JOIN #chan")
 (0.04 ":irc.foonet.org 353 tester = #chan :@fsbot bob alice dummy tester")
 (0.00 ":irc.foonet.org 366 tester #chan :End of NAMES list")
 (0.00 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :tester, welcome!")
 (0.00 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :tester, welcome!")
 (0.03 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :That eye that told you so look'd but a-squint."))

((mode-chan 10 "MODE #chan")
 (0.00 ":irc.foonet.org 324 tester #chan +Cnt")
 (0.01 ":irc.foonet.org 329 tester #chan 1715231970")

 ;; existing query with dummy
 (0.05 ":dummy!~u@s8ceryiqkkcxk.irc PRIVMSG tester :hi")
 (0.02 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :alice: Villains, forbear! we are the empress' sons.")
 (0.01 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :bob: This matter of marrying his king's daughter,wherein he must be weighed rather by her value than his own,words him, I doubt not, a great deal from the matter.")

 ;; dummy quits
 (0.07 ":dummy!~u@s8ceryiqkkcxk.irc QUIT :Quit: \2ERC\2 5.5.0.29.1 (IRC client for GNU Emacs 29.3.50)")
 (0.03 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :We will afflict the emperor in his pride.")
 (0.03 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :bob: Why, then, is my pump well flowered.")
 (0.05 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :Alas! sir, I know not Jupiter; I never drank with him in all my life.")

 ;; rejoins as warwick
 (0.03 ":warwick!~u@s8ceryiqkkcxk.irc JOIN #chan")
 (0.00 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :warwick, welcome!")
 (0.00 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :warwick, welcome!")
 (0.03 ":warwick!~u@s8ceryiqkkcxk.irc PRIVMSG #chan :hola")
 (0.03 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :bob: And stint thou too, I pray thee, nurse, say I.")

 ;; Makes contact in a query
 (0.02 ":warwick!~u@s8ceryiqkkcxk.irc PRIVMSG tester :howdy")
 (0.03 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :bob: Nor more willingly leaves winter; such summer-birds are men. Gentlemen, our dinner will not recompense this long stay: feast your ears with the music awhile, if they will fare so harshly o' the trumpet's sound; we shall to 't presently.")
 (0.03 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :If it please your honour, I know not well what they are; but precise villains they are, that I am sure of, and void of all profanation in the world that good Christians ought to have.")

 ;; warwick renicks back to dummy
 (0.08 ":warwick!~u@s8ceryiqkkcxk.irc NICK dummy")
 (0.04 ":bob!~u@68v4mpismdues.irc PRIVMSG #chan :Pleasure and action make the hours seem short.")
 (0.01 ":dummy!~u@s8ceryiqkkcxk.irc PRIVMSG tester :hey")
 (0.02 ":alice!~u@68v4mpismdues.irc PRIVMSG #chan :bob: Than those that have more cunning to be strange."))
