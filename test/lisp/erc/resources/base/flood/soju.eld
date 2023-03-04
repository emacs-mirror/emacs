;; -*- mode: lisp-data; -*-
((pass 1 "PASS :changeme"))
((nick 1 "NICK tester"))
((user 1 "USER user 0 * :tester")
 (0.13 ":soju.im 001 tester :Welcome to soju, tester")
 (0.0 ":soju.im 002 tester :Your host is soju.im")
 (0.0 ":soju.im 004 tester soju.im soju aiwroO OovaimnqpsrtklbeI")
 (0.0 ":soju.im 005 tester CHATHISTORY=1000 CASEMAPPING=ascii NETWORK=Soju :are supported")
 (0.0 ":soju.im 422 tester :No MOTD"))

((mode 1 "MODE tester +i")
 (0.0 ":tester!tester@10.0.2.100 JOIN #chan/foonet")
 (0.25 ":soju.im 331 tester #chan/foonet :No topic is set")
 (0.0 ":soju.im 353 tester = #chan/foonet :@bob/foonet alice/foonet tester")
 (0.01 ":soju.im 366 tester #chan/foonet :End of /NAMES list")
 (0.0 ":tester!tester@10.0.2.100 JOIN #chan/barnet")
 (0.04 ":soju.im 331 tester #chan/barnet :No topic is set")
 (0.0 ":soju.im 353 tester = #chan/barnet :tester @mike/barnet joe/barnet")
 (0.01 ":soju.im 366 tester #chan/barnet :End of /NAMES list")
 (0.01 ":bob/foonet PRIVMSG #chan/foonet :alice: Then this breaking of his has been but a try for his friends.")
 (0.16 ":alice/foonet PRIVMSG #chan/foonet :bob: By my troth, I take my young lord to be a very melancholy man.")
 (0.91 ":bob/foonet PRIVMSG #chan/foonet :alice: No, truly, for the truest poetry is the most feigning; and lovers are given to poetry, and what they swear in poetry may be said as lovers they do feign.")
 (0.01 ":alice/foonet PRIVMSG #chan/foonet :bob: Sir, his wife some two months since fled from his house: her pretence is a pilgrimage to Saint Jaques le Grand; which holy undertaking with most austere sanctimony she accomplished; and, there residing, the tenderness of her nature became as a prey to her grief; in fine, made a groan of her last breath, and now she sings in heaven.")
 (0.0 ":mike/barnet PRIVMSG #chan/barnet :joe: Who ? not the duke ? yes, your beggar of fifty, and his use was to put a ducat in her clack-dish; the duke had crotchets in him. He would be drunk too; that let me inform you.")
 (0.01 ":joe/barnet PRIVMSG #chan/barnet :mike: Prove it before these varlets here, thou honourable man, prove it.")
 (0.0 ":mike/barnet PRIVMSG #chan/barnet :joe: That my report is just and full of truth.")
 (0.0 ":joe/barnet PRIVMSG #chan/barnet :mike: It is impossible they bear it out.")
 ;; Expected, since we blindly send +i
 (0.0 ":soju.im 501 tester :Cannot change user mode in multi-upstream mode"))

((~mode-foonet 5 "MODE #chan/foonet")
 (0.0 ":soju.im 324 tester #chan/foonet +nt")
 (0.16 ":soju.im 329 tester #chan/foonet 1647158643")
 ;; Start frantic pinging
 (0.0 "PING :soju-msgid-1"))

((~mode-barnet 5 "MODE #chan/barnet")
 (0.0 ":soju.im 324 tester #chan/barnet +nt")
 (0.0 ":soju.im 329 tester #chan/barnet 1647158643"))

((pong-1 5 "PONG :soju-msgid-1")
 (0.0 ":bob/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :alice: The king's coming; I know by his trumpets. Sirrah, inquire further after me; I had talk of you last night: though you are a fool and a knave, you shall eat: go to, follow.")
 (0.0 ":mike/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :joe: Up: so. How is 't ? Feel you your legs ? You stand.")
 (0.0 ":alice/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :bob: Consider then we come but in despite.")
 (0.1 "PING :soju-msgid-2"))

((pong-2 2 "PONG :soju-msgid-2")
 (0.1 ":joe/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :mike: All hail, Macbeth! that shalt be king hereafter.")
 (0.1 "PING :soju-msgid-3"))

((pong-3 2 "PONG :soju-msgid-3")
 (0.1 ":bob/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :alice: And that at my bidding you could so stand up.")
 (0.1 "PING :soju-msgid-4"))

((pong-4 2 "PONG :soju-msgid-4")
 (0.03 ":mike/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :joe: Now he tells how she plucked him to my chamber. O! I see that nose of yours, but not the dog I shall throw it to.")
 (0.1 "PING :soju-msgid-5"))

((pong-5 2 "PONG :soju-msgid-5")
 (0.1 ":alice/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :bob: For policy sits above conscience.")
 (0.1 "PING :soju-msgid-6"))

((pong-6 2 "PONG :soju-msgid-6")
 (0.0 ":joe/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :mike: Take heed o' the foul fiend. Obey thy parents; keep thy word justly; swear not; commit not with man's sworn spouse; set not thy sweet heart on proud array. Tom's a-cold.")
 (0.1 "PING :soju-msgid-7"))

((pong-7 2 "PONG :soju-msgid-7")
 (0.08 ":mike/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :joe: To suffer with him. Good love, call him back.")
 (0.1 "PING :soju-msgid-8"))

((pong-9 2 "PONG :soju-msgid-8")
 (0.1 ":bob/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :alice: Be not obdurate, open thy deaf ears.")
 (0.0 "PING :soju-msgid-9"))

((pong-10 2 "PONG :soju-msgid-9")
 (0.04 ":joe/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :mike: To get good guard and go along with me.")
 (0.1 "PING :soju-msgid-10"))

((~privmsg 2 "PRIVMSG #chan/foonet :alice: hi")
 (0.1 ":alice/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :tester: Good, very good; it is so then: good, very good. Let it be concealed awhile."))

((pong-11 2 "PONG :soju-msgid-10")
 (0.1 ":alice/foonet!~u@g56t7uz8xjj4e.irc PRIVMSG #chan/foonet :bob: Some man or other must present Wall; and let him have some plaster, or some loam, or some rough-cast about him, to signify wall; and let him hold his fingers thus, and through that cranny shall Pyramus and Thisby whisper.")
 (0.0 "PING :soju-msgid-11"))

((pong-12 5 "PONG :soju-msgid-11")
 (0.1 ":mike/barnet!~u@qsidzk5cytcai.irc PRIVMSG #chan/barnet :joe: That's he that was Othello; here I am."))
