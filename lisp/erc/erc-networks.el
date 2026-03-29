;;; erc-networks.el --- IRC networks  -*- lexical-binding: t; -*-

;; Copyright (C) 2002, 2004-2026 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@lexx.delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file deals with IRC networks.
;;
;; Usage:
;;
;; This is the "networks" module.
;;
;; You can use (eq (erc-network) 'Network) if you'd like to set variables or do
;; certain actions according to which network you're connected to.
;; If a network you use is not listed in `erc-networks-alist', you can put
;; (add-to-list 'erc-networks-alist '(Network "irc.server-name.net")) in your
;; config file.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'erc-common)

(defvar erc--target)
(defvar erc-insert-marker)
(defvar erc-modules)
(defvar erc-rename-buffers)
(defvar erc-reuse-buffers)
(defvar erc-server-announced-name)
(defvar erc-server-connected)
(defvar erc-server-process)

(declare-function erc--get-isupport-entry "erc-backend" (key &optional single))
(declare-function erc--insert-admin-message "erc" (&rest args))
(declare-function erc-buffer-filter "erc" (predicate &optional proc))
(declare-function erc-current-nick "erc" nil)
(declare-function erc-display-error-notice "erc" (parsed string))
(declare-function erc-display-message "erc" (parsed type buffer msg &rest args))
(declare-function erc-get-buffer "erc" (target &optional proc))
(declare-function erc-server-process-alive "erc-backend" (&optional buffer))
(declare-function erc-set-active-buffer "erc" (buffer))

(declare-function erc-button--display-error-notice-with-keys
                  (maybe-buffer &rest strings))

;; Variables

(defgroup erc-networks nil
  "IRC Networks."
  :group 'erc)

(defcustom erc-server-alist
'(("4-irc: Random server" 4-irc "4-irc.com" 6667)
  ("A5KNet: Random server" A5KNet "irc.a5knet.com" ((6660 6669)))
  ("AbleNet: Random server" AbleNet "irc.ablenet.org" 6667)
  ("Accessirc: Random server" Accessirc "irc.accessirc.net" 6667)
  ("Acestar: Random server" Acestar "irc.acestar.org" 6667)
  ("Action-IRC: Random server" Action-IRC "irc.action-irc.net" ((6660 6669)))
  ("AfterNET: Random server" AfterNET "irc.afternet.org" 6667)
  ("Alternativenet: Random server" Alternativenet "irc.altnet.org" 6667)
  ("AmigaNet: Random server" AmigaNet "irc.amiganet.org" 6667)
  ("AngelEyez: Random server" AngelEyez "irc.angeleyez.net" ((6666 7000)))
  ("AnotherNet: Random server" Anothernet "irc.another.net" (6667 7000 ))
  ("ArabChat: Random server" ArabChat "irc.arabchat.org" ((6660 6667)))
  ("Ars-OpenIRC: Random server" Ars "irc.arstechnica.com" 6667)
  ("AsiaTalk: Random server" AsiaTalk "irc.asiatalk.org" ((6667 6669) 7000 ))
  ("AstroLink: Random server" AstroLink "irc.astrolink.org" ((6660 6667)))
  ("Asylumnet: Random server" Asylumnet "irc.asylum-net.org" ((6661 6669) 7000 7777 ))
  ("Austnet: Random AU server" Austnet "au.austnet.org" 6667)
  ("Austnet: Random NZ server" Austnet "nz.austnet.org" 6667)
  ("Austnet: Random SG server" Austnet "sg.austnet.org" 6667)
  ("Austnet: Random US server" Austnet "us.austnet.org" 6667)
  ("AwesomeChat: Random server" AwesomeChat "irc.awesomechat.net" ((6661 6669)))
  ("Awesomechristians: Random server" Awesomechristians "irc.awesomechristians.com" 7000)
  ("Axenet: Random server" Axenet "irc.axenet.org" ((6660 6667)))
  ("BeyondIRC: Random server" Beyondirc "irc.beyondirc.net" ((6660 6669)))
  ("BGIRC: Random server" BGIRC "irc.bulgaria.org" ((6666 6669) 7000 ))
  ("Blabbernet: Random server" Blabbernet "irc.blabber.net" (6667 7000 ))
  ("Blitzed: Random server" Blitzed "irc.blitzed.org" (6667 7000 ))
  ("Brasirc: Random server" Brasirc "irc.brasirc.net" ((6666 6667)))
  ("Brasirc: BR, PA, Belem" Brasirc "irc.libnet.com.br" ((6666 6668) 7777 8002 ))
  ("BRASnet: Random European server" BRASnet "eu.brasnet.org" ((6665 6669)))
  ("BRASnet: Random US server" BRASnet "us.brasnet.org" ((6665 6669)))
  ("BubbleNet: Random server" BubbleNet "irc.bubblenet.org" ((6667 6669)))
  ("CCnet: Random server" CCnet "irc.cchat.net" (6667 7000 ))
  ("CCnet: US, TX, Dallas" CCnet "irc2.cchat.net" (6667 7000 ))
  ("Chat-Net: Random server" Chat-Net "irc.chat-net.org" 6667)
  ("Chat-Solutions: Random server" Chat-Solutions "irc.chat-solutions.org" 6667)
  ("Chatcafe: Random server" Chatcafe "irc.chatcafe.net" 6667)
  ("Chatchannel: Random server" Chatchannel "irc.chatchannel.org" ((6666 6669) 7000 ))
  ("ChatCircuit: Random server" ChatCircuit "irc.chatcircuit.com" 6668)
  ("Chatlink: Random server" Chatlink "irc.chatlink.org" 6667)
  ("Chatnet: Random AU server" Chatnet "au.chatnet.org" 6667)
  ("Chatnet: Random EU server" Chatnet "eu.chatnet.org" 6667)
  ("Chatnet: Random US server" Chatnet "us.chatnet.org" 6667)
  ("ChatNut: Random server" ChatNut "irc.chatnut.net" (6667 7000 ))
  ("Chatpinoy: Random server" Chatpinoy "irc.chatpinoy.com" 6667)
  ("ChatPR: Random server" ChatPR "irc.chatpr.org" 6667)
  ("Chatroom: Random server" Chatroom "irc.chatroom.org" 6667)
  ("Chatster: Random server" Chatster "irc.chatster.org" 6667)
  ("ChatX: Random server" ChatX "irc.chatx.net" 6667)
  ("China263: Random server" China263 "irc.263.net" 6667)
  ("Cineplex1: Random server" Cineplex1 "irc.cineplex1.com" ((6666 6668)))
  ("CNN: CNN News discussions" CNN "chat.cnn.com" ((6667 6669) 7000 ))
  ("CobraNet: Random server" CobraNet "irc.cobra.net" 6667)
  ("Coolchat: Random server" Coolchat "irc.coolchat.net" 6667)
  ("Criten: Random server" Criten "irc.criten.net" 6667)
  ("Cyberchat: Random server" Cyberchat "irc.cyberchat.org" (6667 6668 ))
  ("CyGanet: Random server" CyGanet "irc.cyga.net" 6667)
  ("DALnet: AS, MY, Coins" DALnet "coins.dal.net" ((6663 6668) 7000 ))
  ("DALnet: CA, ON, Sodre" DALnet "sodre.on.ca.dal.net" ((6661 6669) 7000 ))
  ("DALnet: EU, DE, Nexgo" DALnet "nexgo.de.eu.dal.net" ((6664 6669) 7000 ))
  ("DALnet: EU, NO, Powertech" DALnet "powertech.no.eu.dal.net" ((6666 6667) 7000 ))
  ("DALnet: EU, SE, Borg" DALnet "borg.se.eu.dal.net" (6667 7000 ))
  ("DALnet: EU, SE, Ced" DALnet "ced.se.eu.dal.net" (6667 7000 ))
  ("DALnet: US, GA, Astro" DALnet "astro.ga.us.dal.net" ((6661 6669) 7000 ))
  ("DALnet: US, GA, Dragons" DALnet "dragons.ga.us.dal.net" ((6661 6669) 7000 ))
  ("DALnet: US, GA, Elysium" DALnet "elysium.ga.us.dal.net" ((6661 6669) 7000 ))
  ("DALnet: US, MA, Twisted" DALnet "twisted.ma.us.dal.net" ((6660 6669) 7001 7002 ))
  ("DALnet: US, MO, Global" DALnet "global.mo.us.dal.net" ((6661 6669) 7000 ))
  ("DALnet: US, NJ, Liberty" DALnet "liberty.nj.us.dal.net" ((6662 6669) 7000 ))
  ("DALnet: US, VA, Wombat" DALnet "wombat.va.us.dal.net" ((6661 6669) 7000 ))
  ("DALnet: Random EU server" DALnet "irc.eu.dal.net" 6667)
  ("DALnet: Random US server" DALnet "irc.dal.net" ((6660 6667)))
  ("Dark-Tou-Net: Random server" Dark-Tou-Net "irc.d-t-net.de" 6667)
  ("Darkfire: Random server" Darkfire "irc.darkfire.net" (6667 7000 8000 ))
  ("DarkMyst: Random server" DarkMyst "irc.darkmyst.org" 6667)
  ("Darkserv: Random server" Darkserv "irc.darkserv.net" 6667)
  ("Darksystem: Random server" Darksystem "irc.darksystem.com" 6667)
  ("Darktree: Random server" Darktree "irc.darktree.net" 6667)
  ("DayNet: Random server" DayNet "irc.daynet.org" 6667)
  ("Deepspace: Disability network" Deepspace "irc.deepspace.org" 6667)
  ("Different: Random server" Different "irc.different.net" 6667)
  ("Digarix: Random server" Digarix "irc.digarix.net" 6667)
  ("Digatech: Random server" Digatech "irc.digatech.net" 6667)
  ("Digital-Base: Random server" Digital-Base "irc.digital-base.net" ((6660 7000)))
  ("Digitalirc: Random server" Digitalirc "irc.digitalirc.net" 6667)
  ("Discussioni: Random server" Discussioni "irc.discussioni.org" ((6666 6669)))
  ("DorukNet: TR, Istanbul" DorukNet "irc.doruk.net.tr" ((6660 6669) 7000 8888 ))
  ("Dreamcast: Random server" Dreamcast "irc0.dreamcast.com" 6667)
  ("DWChat: Random server" DWChat "irc.dwchat.net" 6667)
  ("Dynastynet: Random server" Dynastynet "irc.dynastynet.net" 6667)
  ("EFnet: CA, AB, Edmonton (arcti)" EFnet "irc.arcti.ca" 6667)
  ("EFnet: CA, AB, Edmonton (mpls)" EFnet "irc.mpls.ca" ((6660 6669)))
  ("EFnet: CA, ON, Toronto" EFnet "irc2.magic.ca" 6667)
  ("EFnet: CA, QB, Montreal" EFnet "irc.qeast.net" 6667)
  ("EFnet: EU, DK, Aarhus" EFnet "irc.inet.tele.dk" 6667)
  ("EFnet: EU, FI, Helsinki" EFnet "efnet.cs.hut.fi" 6667)
  ("EFnet: EU, FR, Paris" EFnet "irc.isdnet.fr" ((6667 6669)))
  ("EFnet: EU, NL, Amsterdam" EFnet "efnet.vuurwerk.nl" 6667)
  ("EFnet: EU, NO, Homelien" EFnet "irc.homelien.no" (5190 (6666 6667) (7000 7001) ))
  ("EFnet: EU, NO, Oslo" EFnet "irc.daxnet.no" ((6666 7000)))
  ("EFnet: EU, PL, Warszawa" EFnet "irc.efnet.pl" 6667)
  ("EFnet: EU, RU, Moscow" EFnet "irc.rt.ru" ((6661 6669)))
  ("EFnet: EU, SE, Dalarna" EFnet "irc.du.se" ((6666 6669)))
  ("EFnet: EU, SE, Gothenburg" EFnet "irc.hemmet.chalmers.se" ((6666 7000)))
  ("EFnet: EU, SE, Sweden" EFnet "irc.light.se" 6667)
  ("EFnet: EU, UK, London (carrier)" EFnet "irc.carrier1.net.uk" ((6666 6669)))
  ("EFnet: EU, UK, London (demon)" EFnet "efnet.demon.co.uk" ((6665 6669)))
  ("EFnet: ME, IL, Inter" EFnet "irc.inter.net.il" ((6665 6669)))
  ("EFnet: US, AZ, Phoenix" EFnet "irc.easynews.com" (6660 (6665 6667) 7000 ))
  ("EFnet: US, CA, San Jose" EFnet "irc.concentric.net" ((6665 6668)))
  ("EFnet: US, CA, San Luis Obispo" EFnet "irc.prison.net" ((6666 6667)))
  ("EFnet: US, GA, Atlanta" EFnet "irc.mindspring.com" ((6660 6669)))
  ("EFnet: US, MI, Ann Arbor" EFnet "irc.umich.edu" 6667)
  ("EFnet: US, MN, Twin Cities" EFnet "irc.umn.edu" ((6665 6669)))
  ("EFnet: US, NY, Mineola" EFnet "irc.lightning.net" ((6665 7000)))
  ("EFnet: US, NY, New York (east)" EFnet "irc.east.gblx.net" 6667)
  ("EFnet: US, NY, New York (flamed)" EFnet "irc.flamed.net" ((6665 6669)))
  ("EFnet: US, TX, Houston" EFnet "ircd.lagged.org" ((6660 6669)))
  ("EFnet: US, VA, Ashburn" EFnet "irc.secsup.uu.net" ((6665 6669) 8080 ))
  ("EFnet: Random AU server" EFnet "au.rr.efnet.net" 6667)
  ("EFnet: Random CA server" EFnet "ca.rr.efnet.net" 6667)
  ("EFnet: Random EU server" EFnet "eu.rr.efnet.net" 6667)
  ("EFnet: Random US server" EFnet "us.rr.efnet.net" 6667)
  ("EgyptianIRC: Random server" EgyptianIRC "irc.egyptianirc.net" ((6667 6669)))
  ("Eircnet: Random server" Eircnet "irc.eircnet.org" ((6660 6669) 7000 ))
  ("Eleethal: Random server" Eleethal "irc.eleethal.com" ((6660 6669) 7000 ))
  ("EntertheGame: Random server" EntertheGame "irc.enterthegame.com" ((6667 6669)))
  ("EpiKnet: Random server" EpiKnet "irc.epiknet.org" ((6660 6669) 7000 7001 ))
  ("EsperNet: Random server" EsperNet "irc.esper.net" (5555 (6667 6669) ))
  ("Esprit: Random server" Esprit "irc.esprit.net" 6667)
  ("euIRC: Random server" euIRC "irc.euirc.net" ((6665 6669)))
  ("Evilzinc: Random server" Evilzinc "irc.evilzinc.net" ((6660 6669) 7000 8000 ))
  ("ExodusIRC: Random server" ExodusIRC "irc.exodusirc.net" ((6660 6669)))
  ("FDFnet: Random server" FDFnet "irc.fdfnet.net" ((6666 6668) 9999 ))
  ("FEFnet: Random server" FEFnet "irc.fef.net" 6667)
  ("Financialchat: Random server" Financialchat "irc.financialchat.com" ((6667 6669) 7000 ))
  ("Forestnet: Random server" Forestnet "irc.forestnet.org" (6667 7000 ))
  ("ForeverChat: Random server" ForeverChat "irc.foreverchat.net" ((6660 6669) 7000 ))
  ("Fraggers: Random server" Fraggers "irc.fraggers.co.uk" ((6661 6669) (7000 7001) ))
  ("FreedomChat: Random server" FreedomChat "chat.freedomchat.net" 6667)
  ("FreedomIRC: Random server" FreedomIRC "irc.freedomirc.net" 6667)
  ("Freenode: Random server" freenode "chat.freenode.net" 6667)
  ("Freenode: Random EU server" freenode "chat.eu.freenode.net" 6667)
  ("Freenode: Random US server" freenode "chat.us.freenode.net" 6667)
  ("FunNet: Random server" FunNet "irc.funnet.org" 6667)
  ("Galaxynet: Random server" GalaxyNet "irc.galaxynet.org" ((6662 6668) 7000 ))
  ("Galaxynet: AU, NZ, Auckland" GalaxyNet "auckland.nz.galaxynet.org" ((6661 6669)))
  ("Galaxynet: EU, BE, Online" GalaxyNet "online.be.galaxynet.org" ((6661 6669)))
  ("Galaxynet: US, FL, Florida" GalaxyNet "gymnet.us.galaxynet.org" ((6661 6669)))
  ("Gamesnet: Random east US server" Gamesnet "east.gamesnet.net" 6667)
  ("Gamesnet: Random west US server" Gamesnet "west.gamesnet.net" 6667)
  ("GammaForce: Random server" GammaForce "irc.gammaforce.org" ((6660 6669) 7000 ))
  ("GIKInet: Random server" GIKInet "irc.giki.edu.pk" 6667)
  ("GizNet: Random server" GizNet "irc.giznet.org" ((6666 6669) 7000 ))
  ("Globalchat: Random server" Globalchat "irc.globalchat.org" 6667)
  ("GlobIRC: Random server" GlobIRC "irc.globirc.net" ((6666 6668) 9999 ))
  ("Goldchat: Random server" Goldchat "irc.goldchat.nl" ((6660 6669) 7000 ))
  ("Goodchatting: Random server" Goodchatting "irc.goodchatting.com" ((6661 6669) 7000 ))
  ("GravityLords: Random server" GravityLords "irc.gravitylords.net" 6667)
  ("Grnet: Random EU server" GRnet "gr.irc.gr" (6667 7000 ))
  ("Grnet: Random server" GRnet "srv.irc.gr" (6667 7000 ))
  ("Grnet: Random US server" GRnet "us.irc.gr" (6667 7000 ))
  ("GulfChat: Random server" GulfChat "irc.gulfchat.net" ((6660 6669)))
  ("HabberNet: Random server" HabberNet "irc.habber.net" 6667)
  ("HanIRC: Random server" HanIRC "irc.hanirc.org" 6667)
  ("Hellenicnet: Random server" Hellenicnet "irc.mirc.gr" (6667 7000 ))
  ("IceNet: Random server" IceNet "irc.icenet.org.za" 6667)
  ("ICQnet: Random server" ICQnet "irc.icq.com" 6667)
  ("Infatech: Random server" Infatech "irc.infatech.net" ((6660 6669)))
  ("Infinity: Random server" Infinity "irc.infinity-irc.org" 6667)
  ("Infomatrix: Random server" Infomatrix "irc.infomatrix.net" 6667)
  ("Inside3D: Random server" Inside3D "irc.inside3d.net" ((6661 6669)))
  ("InterlinkChat: Random server" InterlinkChat "irc.interlinkchat.net" ((6660 6669) 7000 ))
  ("IRC-Chile: Random server" IRC-Chile "irc.cl" 6667)
  ("IRC-Hispano: Random server" IRC-Hispano "irc.irc-hispano.org" 6667)
  ("IRCchat: Random server" IRCchat "irc.ircchat.tk" 6667)
  ("IRCGate: Random server" IRCGate "irc.ircgate.net" ((6667 6669)))
  ("IRCGeeks: Random server" IRCGeeks "irc.ircgeeks.org" ((6660 6669)))
  ("IRChat: Random server" IRChat "irc.irchat.net" ((6660 6669)))
  ("IrcLordz: Random server" IrcLordz "irc.irclordz.com" 6667)
  ("IrcMalta: Random server" IrcMalta "irc.ircmalta.org" ((6660 6667)))
  ;; This one is dead but used in testing.  Please retain.
  ("IRCnet: EU, FR, Random" IRCnet "irc.fr.ircnet.net" 6667)
  ("IRCnet: EU, IT, Random" IRCnet "irc.ircd.it" ((6665 6669)))
  ("IRCnet: AS, IL, Haifa" IRCnet "ircnet.netvision.net.il" ((6661 6668)))
  ("IRCnet: AS, JP, Tokyo" IRCnet "irc.tokyo.wide.ad.jp" 6667)
  ("IRCnet: AS, TW, Seed" IRCnet "irc.seed.net.tw" 6667)
  ("IRCnet: EU, AT, Linz" IRCnet "linz.irc.at" ((6666 6668)))
  ("IRCnet: EU, AT, Wien" IRCnet "vienna.irc.at" ((6666 6669)))
  ("IRCnet: EU, BE, Brussels" IRCnet "irc.belnet.be" 6667)
  ("IRCnet: EU, BE, Zaventem" IRCnet "ircnet.wanadoo.be" ((6661 6669)))
  ("IRCnet: EU, CZ, Prague" IRCnet "irc.felk.cvut.cz" 6667)
  ("IRCnet: EU, DE, Berlin" IRCnet "irc.fu-berlin.de" ((6665 6669)))
  ("IRCnet: EU, DE, Dusseldorf" IRCnet "irc.freenet.de" ((6665 6669)))
  ("IRCnet: EU, DE, Stuttgart" IRCnet "irc.belwue.de" ((6665 6669)))
  ("IRCnet: EU, DK, Copenhagen" IRCnet "irc.ircnet.dk" 6667)
  ("IRCnet: EU, EE, Tallinn" IRCnet "irc.estpak.ee" ((6666 6668)))
  ("IRCnet: EU, FI, Helsinki" IRCnet "irc.cs.hut.fi" 6667)
  ("IRCnet: EU, GR, Thessaloniki" IRCnet "irc.ee.auth.gr" ((6666 6669)))
  ("IRCnet: EU, HU, Budapest" IRCnet "irc.elte.hu" 6667)
  ("IRCnet: EU, IS, Reykjavik (ircnet)" IRCnet "irc.ircnet.is" ((6661 6669)))
  ("IRCnet: EU, IS, Reykjavik (simnet)" IRCnet "irc.simnet.is" ((6661 6669)))
  ("IRCnet: EU, IT, Rome" IRCnet "irc.tin.it" ((6665 6669)))
  ("IRCnet: EU, NL, Amsterdam (nlnet)" IRCnet "irc.nl.uu.net" ((6660 6669)))
  ("IRCnet: EU, NL, Amsterdam (xs4all)" IRCnet "irc.xs4all.nl" ((6660 6669)))
  ("IRCnet: EU, NL, Enschede" IRCnet "irc.snt.utwente.nl" ((6660 6669)))
  ("IRCnet: EU, NL, Nijmegen" IRCnet "irc.sci.kun.nl" ((6660 6669)))
  ("IRCnet: EU, NO, Oslo" IRCnet "irc.ifi.uio.no" 6667)
  ("IRCnet: EU, NO, Trondheim" IRCnet "irc.pvv.ntnu.no" 6667)
  ("IRCnet: EU, PL, Lublin" IRCnet "lublin.irc.pl" ((6666 6668)))
  ("IRCnet: EU, PL, Warsaw" IRCnet "warszawa.irc.pl" ((6666 6668)))
  ("IRCnet: EU, RU, Moscow" IRCnet "irc.msu.ru" 6667)
  ("IRCnet: EU, SE, Lulea" IRCnet "irc.ludd.luth.se" ((6661 6669)))
  ("IRCnet: EU, UK, London (Demon)" IRCnet "ircnet.demon.co.uk" ((6665 6669)))
  ("IRCnet: EU, UK, London (Easynet)" IRCnet "ircnet.easynet.co.uk" ((6666 6669)))
  ("IRCnet: US, NY, New York" IRCnet "irc.stealth.net" ((6660 6669)))
  ("IRCnet: Random AU server" IRCnet "au.ircnet.org" 6667)
  ("IRCnet: Random EU server" IRCnet "eu.ircnet.org" ((6665 6668)))
  ("IRCnet: Random US server" IRCnet "us.ircnet.org" ((6665 6668)))
  ("IRCSoulZ: Random server" IRCSoulZ "irc.ircsoulz.net" 6667)
  ("IRCSul: BR, PR, Maringa" IRCSul "irc.wnet.com.br" 6667)
  ("IrcTalk: Random server" IrcTalk "irc.irctalk.net" ((6660 6669)))
  ("Irctoo: Random server" Irctoo "irc.irctoo.net" 6667)
  ("IRCtown: Random server" IRCtown "irc.irctown.net" ((6666 6669) 7000 ))
  ("IRCworld: Random server" IRCworld "irc.ircworld.org" 6667)
  ("ircXtreme: Random server" ircXtreme "irc.ircXtreme.net" ((6660 6669)))
  ("Israelnet: Random server" Israelnet "irc.israel.net" 6667)
  ("K0wNet: Random server" K0wNet "irc.k0w.net" ((6660 6669)))
  ("KDFSnet: Random server" KDFSnet "irc.kdfs.net" ((6667 6669)))
  ("Kemik: Random server" Kemik "irc.kemik.net" 6667)
  ("Kewl.Org: Random server" Kewl\.Org "irc.kewl.org" (6667 7000 ))
  ("Kickchat: Random server" Kickchat "irc.kickchat.com" ((6660 6669) 7000 ))
  ("Kidsworld: Random server" KidsWorld "irc.kidsworld.org" ((6666 6669)))
  ("Knightnet: AF, ZA, Durban" Knightnet "orc.dbn.za.knightnet.net" (6667 5555 ))
  ("Knightnet: US, CA, Goldengate" Knightnet "goldengate.ca.us.knightnet.net" (6667 5555 ))
  ("Konfido.Net: Random server" Konfido\.Net "irc.konfido.net" 6667)
  ("KreyNet: Random server" Kreynet "irc.krey.net" 6667)
  ("Krono: Random server" Krono "irc.krono.net" ((6660 6669) 7000 ))
  ("Krushnet: Random server" Krushnet "irc.krushnet.org" 6667)
  ("LagNet: Random server" LagNet "irc.lagnet.org.za" 6667)
  ("LagNet: AF, ZA, Cape Town" LagNet "reaper.lagnet.org.za" 6667)
  ("LagNet: AF, ZA, Johannesburg" LagNet "mystery.lagnet.org.za" 6667)
  ("Libera.Chat: Random server" Libera.Chat "irc.libera.chat"
   ((6665 6667) (8000 8002)) (6697 7000 7070))
  ;; If not deprecating this option, use ^ for the rest of these servers.
  ("Libera.Chat: Random Europe server" Libera.Chat "irc.eu.libera.chat" 6667 6697)
  ("Libera.Chat: Random US & Canada server" Libera.Chat "irc.us.libera.chat" 6667 6697)
  ("Libera.Chat: Random Australia & New Zealand server" Libera.Chat "irc.au.libera.chat" 6667 6697)
  ("Libera.Chat: Random East Asia server" Libera.Chat "irc.ea.libera.chat" 6667 6697)
  ("Libera.Chat: IPv4 only server" Libera.Chat "irc.ipv4.libera.chat" 6667 6697)
  ("Libera.Chat: IPv6 only server" Libera.Chat "irc.ipv6.libera.chat" 6667 6697)
  ("Librenet: Random server" Librenet "irc.librenet.net" 6667)
  ("LinkNet: Random server" LinkNet "irc.link-net.org" ((6667 6669)))
  ("LinuxChix: Random server" LinuxChix "irc.linuxchix.org" 6667)
  ("Liquidized: Random server" Liquidized "irc.liquidized.net" (6667 7000 ))
  ("M-IRC: Random server" M-IRC "irc.m-sys.org" ((6667 6669)))
  ("MagicStar: Random server" MagicStar "irc.magicstar.net" 6667)
  ("Mavra: Random server" Mavra "irc.mavra.net" 6667)
  ("MediaDriven: Random server" MediaDriven "irc.mediadriven.com" ((6667 6669)))
  ("mIRC-X: Random server" mIRC-X "irc.mircx.com" (6667 7000 ))
  ("Morat: Random server" Morat "irc.morat.net" 6667)
  ("MusicCity: Random server" MusicCity "chat.musiccity.com" 6667)
  ("Mysteria: Random server" Mysteria "irc.mysteria.net" (6667 7000 ))
  ("Mysterychat: Random server" Mysterychat "irc.mysterychat.net" ((6667 6669)))
  ("Mystical: Random server" Mystical "irc.mystical.net" (6667 7000 ))
  ("Narancs: Random server" Narancs "irc.narancs.com" ((6667 6669) 7000 ))
  ("Net-France: Random server" Net-France "irc.net-france.com" 6667)
  ("Nevernet: Random server" Nevernet "irc.nevernet.net" 6667)
  ("Newnet: Random server" Newnet "irc.newnet.net" ((6665 6667)))
  ("Nexusirc: Random server" Nexusirc "irc.nexusirc.org" 6667)
  ("Nightstar: Random server" NightStar "irc.nightstar.net" ((6665 6669)))
  ("NitrousNet: Random server" NitrousNet "irc.nitrousnet.net" 6667)
  ("Novernet: Random server" Novernet "irc.novernet.com" ((6665 6669) 7000 ))
  ("Nullrouted: Random server" Nullrouted "irc.nullrouted.org" ((6666 6669) 7000 ))
  ("NullusNet: Random server" NullusNet "irc.nullus.net" 6667)
  ("OFTC: Random server" OFTC "irc.oftc.net" ((6667 6670) 7000) (6697 9999))
  ("OpChat: Random server" OpChat "irc.opchat.org" ((6667 6669)))
  ("Othernet: Random server" Othernet "irc.othernet.org" 6667)
  ("Othernet: US, FL, Miami" Othernet "miami.fl.us.othernet.org" 6667)
  ("Othernet: US, MO, StLouis" Othernet "stlouis.mo.us.othernet.org" 6667)
  ("Otherside: Random server" OtherSide "irc.othersideirc.net" 6667)
  ("Outsiderz: Random server" Outsiderz "irc.outsiderz.com" 6667)
  ("OzOrg: AU, Perth" OzOrg "iinet.perth.oz.org" 6667)
  ("Peacefulhaven: Random server" Peacefulhaven "irc.peacefulhaven.net" ((6660 6669) 7000 ))
  ("PhazedIRC: Random server" PhazedIRC "irc.phazedirc.net" 6667)
  ("Philchat: Random server" Philchat "irc.philchat.net" 6667)
  ("phrozN: Random server" phrozN "irc.phrozn.net" 6667)
  ("PiNet: Random server" PiNet "irc.praetorians.org" ((6665 6669)))
  ("Pinoycentral: Random server" Pinoycentral "chat.abs-cbn.com" 6667)
  ("Planetarion: Random server" Planetarion "irc.planetarion.com" 6667)
  ("POLNet: Random server" POLNet "irc.ircnet.pl" 6667)
  ("Psionics: CA, PQ, Montreal" Psionics "chat.psionics.net" ((6660 6669)))
  ("PTirc: Random server" PTirc "irc.ptirc.com.pt" 6667)
  ("PTlink: Random server" PTlink "irc.ptlink.net" 6667)
  ("PTnet: Random server" PTnet "irc.ptnet.org" 6667)
  ("QChat: Random server" QChat "irc.qchat.net" 6667)
  ("QuakeNet: Random German server" QuakeNet "de.quakenet.org" ((6667 6669)))
  ("QuakeNet: Random server" QuakeNet "irc.quakenet.eu.org" ((6667 6669)))
  ("QuakeNet: Random Swedish server" QuakeNet "se.quakenet.org" ((6667 6669)))
  ("QuakeNet: Random UK server" QuakeNet "uk.quakenet.org" ((6667 6669)))
  ("QuakeNet: Random US server" QuakeNet "us.quakenet.org" ((6667 6669)))
  ("Realirc: Random server" Realirc "irc.realirc.org" 6667)
  ("RealmNET: Random server" RealmNET "irc.realmnet.com" 6667)
  ("Rebelchat: Random server" Rebelchat "irc.rebelchat.org" 6667)
  ("Red-Latina: Random server" Red-Latina "irc.red-latina.org" 6667)
  ("RedLatona: Random server" RedLatona "irc.redlatona.net" (6667 6668 ))
  ("Relicnet: Random server" Relicnet "irc.relic.net" 6667)
  ("Rezosup: Random server" Rezosup "irc.rezosup.org" 6667)
  ("Risanet: Random server" Risanet "irc.risanet.com" ((6667 6669)))
  ("Rizon: Random server" Rizon "irc.rizon.net" (6633 (6660 6669) 6697 7000 8080 9999))
  ("Rubiks: Random server" Rubiks "irc.rubiks.net" 6667)
  ("Rusnet: EU, RU, Tomsk" Rusnet "irc.tsk.ru" ((6667 6669) (7770 7775) ))
  ("Rusnet: EU, RU, Vladivostok" Rusnet "irc.vladivostok.ru" ((6667 6669) (7770 7775) ))
  ("Rusnet: EU, UA, Kiev" Rusnet "irc.kar.net" ((6667 6669) (7770 7775) ))
  ("Sandnet: Random server" Sandnet "irc.sandnet.net" ((6660 6669) 7000 ))
  ("Scunc: Random server" Scunc "irc.scunc.net" 6667)
  ("SerbianCafe: Random server" SerbianCafe "irc.serbiancafe.ws" ((6665 6669)))
  ("SexNet: Random server" SexNet "irc.sexnet.org" 6667)
  ("ShadowFire: Random server" ShadowFire "irc.shadowfire.org" 6667)
  ("ShadowWorld: Random server" ShadowWorld "irc.shadowworld.net" 6667)
  ("SkyNet: Random server" SkyNet "irc.bronowski.pl" ((6666 6668)))
  ("Slashnet: Random server" Slashnet "irc.slashnet.org" 6667)
  ("SolarStone: Random server" SolarStone "irc.solarstone.net" ((6660 6669)))
  ("Sorcerynet: Random server" Sorcery "irc.sorcery.net" (6667 7000 9000 ))
  ("Sorcerynet: EU, SE, Karlskrona" Sorcery "nexus.sorcery.net" (6667 7000 9000 ))
  ("Sorcerynet: US, CA, Palo Alto" Sorcery "kechara.sorcery.net" (6667 7000 9000 ))
  ("SourceIRC: Random server" SourceIRC "irc.sourceirc.net" ((6667 6669) 7000 ))
  ("SpaceTronix: Random server" SpaceTronix "irc.spacetronix.net" ((6660 6669) 7000 ))
  ("Spirit-Harmony: Random server" Spirit-Harmony "irc.spirit-harmony.com" ((6661 6669)))
  ("StarChat: Random server" StarChat "irc.starchat.net" ((6667 6669) 7000 ))
  ("StarEquinox: Random server" StarEquinox "irc.starequinox.net" ((6660 6669)))
  ("StarLink: Random server" Starlink "irc.starlink.net" ((6660 6669)))
  ("StarLink-irc: Random server" starlink-irc "irc.starlink-irc.org" 6667)
  ("StarWars-IRC: Random server" StarWars-IRC "irc.starwars-irc.net" ((6663 6667)))
  ("Stormdancing: Random server" Stormdancing "irc.stormdancing.net" ((6664 6669) 7000 9000 ))
  ("Superchat: Random server" Superchat "irc.superchat.org" ((6660 6668)))
  ("Sysopnet: Random server" Sysopnet "irc.sysopnet.org" ((6666 6668)))
  ("Telstra: Random server" Telstra "irc.telstra.com" ((6667 6669)))
  ("TR-net: EU, TR, Ankara" TR-net "irc.dominet.com.tr" 6667)
  ("TR-net: EU, Tr, Istanbul" TR-net "irc.teklan.com.tr" 6667)
  ("Tri-net: Random server" Tri-net "irc.tri-net.org" 6667)
  ("TriLink: Random server" TriLink "irc.ft4u.net" 6667)
  ("TurkishChat: Random server" TurkishChat "irc.turkishchat.org" ((6660 6669) 7000 ))
  ("UberNinja: Random server" UberNinja "irc.uberninja.net" ((6667 6669)))
  ("UICN: Random server" UICN "irc.uicn.net" 6667)
  ("UltraIRC: Random server" UltraIRC "irc.ultrairc.net" 6667)
  ("UnderChat: Random server" UnderChat "irc.underchat.it" ((6660 6669) 7000 ))
  ("Undernet: CA, ON, Toronto" Undernet "toronto.on.ca.undernet.org" ((6661 6669)))
  ("Undernet: CA, QC, Montreal" Undernet "montreal.qu.ca.undernet.org" ((6660 6669)))
  ("Undernet: EU, AT, Graz" Undernet "graz.at.eu.undernet.org" ((6661 6669)))
  ("Undernet: EU, BE, Antwerp" Undernet "flanders.be.eu.undernet.org" ((6660 6669)))
  ("Undernet: EU, BE, Brussels" Undernet "brussels.be.eu.undernet.org" 6667)
  ("Undernet: EU, CH, Geneva" Undernet "geneva.ch.eu.undernet.org" ((6660 6669) 7777 8000 ))
  ("Undernet: EU, FR, Caen" Undernet "caen.fr.eu.undernet.org" ((6666 6669)))
  ("Undernet: EU, NL, Diemen" Undernet "diemen.nl.eu.undernet.org" ((6660 6669)))
  ("Undernet: EU, NL, Haarlem" Undernet "haarlem.nl.eu.undernet.org" ((6660 6669)))
  ("Undernet: EU, NO, Oslo" Undernet "oslo.no.eu.undernet.org" ((6660 6669)))
  ("Undernet: EU, SE, Stockholm" Undernet "stockholm.se.eu.undernet.org" ((6666 6669)))
  ("Undernet: EU, UK, Surrey" Undernet "surrey.uk.eu.undernet.org" ((6660 6669)))
  ("Undernet: US, AZ, Mesa" Undernet "mesa.az.us.undernet.org" ((6665 6667)))
  ("Undernet: US, CA, San Diego" Undernet "sandiego.ca.us.undernet.org" ((6660 6670)))
  ("Undernet: US, DC, Washington" Undernet "washington.dc.us.undernet.org" ((6660 6669)))
  ("Undernet: US, KS, Manhattan" Undernet "manhattan.ks.us.undernet.org" ((6660 6669)))
  ("Undernet: US, NV, Las Vegas" Undernet "lasvegas.nv.us.undernet.org" ((6660 6669)))
  ("Undernet: US, TX, Austin" Undernet "austin.tx.us.undernet.org" ((6660 6669)))
  ("Undernet: US, UT, Saltlake" Undernet "saltlake.ut.us.undernet.org" ((6660 6669)))
  ("Undernet: US, VA, Arlington" Undernet "arlington.va.us.undernet.org" ((6660 6669)))
  ("Undernet: US, VA, McLean" Undernet "mclean.va.us.undernet.org" ((6666 6669)))
  ("Undernet: Random EU server" Undernet "eu.undernet.org" 6667)
  ("Undernet: Random US server" Undernet "us.undernet.org" 6667)
  ("UnderZ: Random server" UnderZ "irc.underz.org" ((6667 6668)))
  ("UniChat: Random server" UniChat "irc.uni-chat.net" 6667)
  ("UnionLatina: Random server" UnionLatina "irc.unionlatina.org" 6667)
  ("Univers: Random server" Univers "irc.univers.org" ((6665 6669)))
  ("UnixR: Random server" UnixR "irc.unixr.net" ((6667 6669)))
  ("Vidgamechat: Random server" Vidgamechat "irc.vidgamechat.com" 6667)
  ("VirtuaNet: Random server" VirtuaNet "irc.virtuanet.org" ((6660 6669) 7000 ))
  ("Vitamina: Random server" Vitamina "irc.vitamina.ca" 6667)
  ("Voila: Random server" Voila "irc.voila.fr" 6667)
  ("Wahou: Random server" Wahou "irc.wahou.org" ((6665 6669)))
  ("Warpednet: Random server" Warpednet "irc.warped.net" 6667)
  ("Weaklinks: Random server" Weaklinks "irc.weaklinks.net" ((6667 6669)))
  ("Webnet: Random server" Webnet "irc.webchat.org" ((6667 6669) 7000 ))
  ("Webnet: US, CA, Santa Clara" Webnet "webmaster.ca.us.webchat.org" ((6661 6669)))
  ("WinChat: Random server" WinChat "irc.winchat.net" ((6661 6669)))
  ("WinIRC: Random server" WinIRC "irc.winirc.org" ((6667 6669) 4400 ))
  ("WorldIRC: Random server" WorldIRC "irc.worldirc.org" ((6660 6667)))
  ("WyldRyde: Random server" WyldRyde "irc.wyldryde.net" ((6666 6669)))
  ("XentoniX: Random server" XentoniX "irc.xentonix.net" ((6661 6669)))
  ("Xevion: Random server" Xevion "irc.xevion.net" (6667 7000 ))
  ("XNet: Random server" XNet "irc.xnet.org" 6667)
  ("XWorld: Random server" XWorld "irc.xworld.org" 6667)
  ("ZAnet Net: Random server" ZAnetNet "irc.zanet.net" 6667)
  ("ZAnet Org: UK, London" ZAnetOrg "mystic.zanet.org.za" 6667)
  ("ZiRC: Random server" ZiRC "irc.zirc.org" ((6660 6669)))
  ("ZUHnet: Random server" ZUHnet "irc.zuh.net" 6667)
  ("Zurna: Random server" Zurna "irc.zurna.net" 6667))
  "Alist of irc servers.
Each server is a list (NAME NET HOST PORTS TLS-PORTS) where
NAME is a name for that server,
NET is a symbol indicating to which network from `erc-networks-alist'
  this server corresponds,
HOST is the server's hostname, and (TLS-)PORTS is either a
number, a list of numbers, or a list of port ranges."
  :package-version '(ERC . "5.6")
  :type '(alist :key-type (string :tag "Name")
		:value-type
		(group symbol (string :tag "Hostname")
		       (choice :tag "Ports"
			       (integer :tag "Port number")
			       (repeat :tag "List of ports or ranges"
				       (choice (integer :tag "Port number")
					       (list :tag "Port range"
						     integer integer))))
                       (choice :tag "TLS ports"
                               (integer :tag "TLS port number")
                               (repeat :tag "List of TLS ports or ranges"
                                       (choice (integer :tag "TLS port number")
                                               (list :tag "TLS port range"
                                                     integer integer)))))))
(make-obsolete-variable 'erc-server-alist
                        "specify `:server' with `erc-tls'." "30.1")

(defcustom erc-networks-alist
  '((4-irc "4-irc.com")
    (A5KNet "a5knet.com")
    (AbleNet "ablenet.org")
    (Accessirc "accessirc.net")
    (Acestar "acestar.org")
    (Action-IRC "action-irc.net")
    (AfterNET "afternet.org")
    (Alternativenet "altnet.org")
    (AmigaNet "amiganet.org")
    (AngelEyez "angeleyez.net")
    (Anothernet "another.net")
    (ArabChat "arabchat.org")
    (Ars "arstechnica.com")
    (AsiaTalk "asiatalk.org")
    (AstroLink "astrolink.org")
    (Asylumnet "asylumnet.org")
    (Austnet "austnet.org")
    (AwesomeChat "awesomechat.net")
    (Awesomechristians "awesomechristians.com")
    (Axenet "axenet.org")
    (Beyondirc "beyondirc.net")
    (BGIRC "bulgaria.org")
    (Blabbernet "blabber.net")
    (Blitzed "blitzed.org")
    (BrasIRC "brasirc.net")
    (BRASnet "brasnet.org")
    (BubbleNet "bubblenet.org")
    (CCnet "christian-chat.net")
    (Chat-Net "chat-net.org")
    (Chat-Solutions "chat-solutions.org")
    (Chatcafe "chatcafe.net")
    (Chatchannel "chatchannel.org")
    (ChatCircuit "chatcircuit.com")
    (Chatlink "chatlink.org")
    (Chatnet "chatnet.org")
    (ChatNut "chatnut.net")
    (Chatpinoy "chatpinoy.com")
    (ChatPR "chatpr.org")
    (Chatroom "chatroom.org")
    (Chatster "chatster.org")
    (ChatX "chatx.net")
    (China263 "263.net")
    (Cineplex1 "cineplex1.com")
    (CNN "cnn.com")
    (CobraNet "cobra.net")
    (Coolchat "coolchat.net")
    (Criten "criten.net")
    (Cyberchat "cyberchat.org")
    (CyGanet "cyga.net")
    (DALnet "dal.net")
    (Dark-Tou-Net "d-t-net.de")
    (Darkfire "darkfire.net")
    (DarkMyst "darkmyst.org")
    (Darkserv "darkserv.net")
    (Darksystem "darksystem.com")
    (Darktree "darktree.net")
    (DayNet "daynet.org")
    (Deepspace "deepspace.org")
    (Different "different.net")
    (Digarix "digarix.net")
    (Digatech "digatech.net")
    (Digital-Base "digital-base.net")
    (Digitalirc "digitalirc.net")
    (Discussioni "discussioni.org")
    (DorukNet "doruk.net.tr")
    (DWChat "dwchat.net")
    (Dynastynet "dynastynet.net")
    (EFnet nil)
    (EgyptianIRC "egyptianirc.net")
    (Eircnet "eircnet.org")
    (Eleethal "eleethal.com")
    (EntertheGame "enterthegame.com")
    (EpiKnet "epiknet.org")
    (EsperNet "esper.net")
    (Esprit "esprit.net")
    (euIRC "euirc.net")
    (Evilzinc "evilzinc.net")
    (ExodusIRC "exodusirc.net")
    (FDFnet "fdfnet.net")
    (FEFnet "fef.net")
    (Financialchat "financialchat.com")
    (Forestnet "forestnet.org")
    (ForeverChat "foreverchat.net")
    (Fraggers "fraggers.co.uk")
    (FreedomChat "freedomchat.net")
    (FreedomIRC "freedomirc.net")
    (freenode "freenode.net")
    (FunNet "funnet.org")
    (GalaxyNet "galaxynet.org")
    (Gamesnet "gamesnet.net")
    (GammaForce "gammaforce.org")
    (GIKInet "giki.edu.pk")
    (GizNet "giznet.org")
    (Globalchat "globalchat.org")
    (GlobIRC "globirc.net")
    (Goldchat "goldchat.nl")
    (Goodchatting "goodchatting.com")
    (GravityLords "gravitylords.net")
    (GRnet "irc.gr")
    (GulfChat "gulfchat.net")
    (HabberNet "habber.net")
    (HanIRC "hanirc.org")
    (Hellenicnet "mirc.gr")
    (IceNet "icenet.org.za")
    (ICQnet "icq.com")
    (iip "anon.iip")
    (Infatech "infatech.net")
    (Infinity "infinity-irc.org")
    (Infomatrix "infomatrix.net")
    (Inside3D "inside3d.net")
    (InterlinkChat "interlinkchat.net")
    (IRC-Chile "irc.cl")
    (IRC-Hispano "irc-hispano.org")
    (IRCchat "ircchat.tk")
    (IRCGate "ircgate.net")
    (IRCGeeks "ircgeeks.org")
    (IRChat "irchat.net")
    (IrcLordz "irclordz.com")
    (IrcMalta "ircmalta.org")
    (IRCnet nil)
    (IRCSoulZ "ircsoulz.net")
    (IRCSul "wnet.com.br")
    (IrcTalk "irctalk.net")
    (Irctoo "irctoo.net")
    (IRCtown "irc.irctown.net")
    (IRCworld "ircworld.org")
    (ircXtreme "ircXtreme.net")
    (Israelnet "israel.net")
    (K0wNet "k0w.net")
    (KDFSnet "kdfs.net")
    (Kemik "kemik.net")
    (Kewl\.Org "kewl.org")
    (Kickchat "kickchat.com")
    (KidsWorld "kidsworld.org")
    (Knightnet "knightnet.net")
    (Konfido\.Net "konfido.net")
    (Kreynet "krey.net")
    (Krono "krono.net")
    (Krushnet "krushnet.org")
    (LagNet "lagnet.org.za")
    (Libera.Chat "libera.chat")
    (Librenet "librenet.net")
    (LinkNet "link-net.org")
    (LinuxChix "cats\\.meow\\.at\\|linuxchix\\.org")
    (Liquidized "liquidized.net")
    (M-IRC "m-sys.org")
    (MagicStar "magicstar.net")
    (Mavra "mavra.net")
    (MediaDriven "mediadriven.com")
    (mIRC-X "mircx.com")
    (Morat "morat.net")
    (MusicCity "musiccity.com")
    (Mysteria "mysteria.net")
    (Mysterychat "mysterychat.net")
    (Mystical "mystical.net")
    (Narancs "narancs.com")
    (Net-France "net-france.com")
    (Nevernet "nevernet.net")
    (Newnet "newnet.net")
    (Nexusirc "nexusirc.org")
    (NightStar "nightstar.net")
    (NitrousNet "nitrousnet.net")
    (Novernet "novernet.com")
    (Nullrouted "nullrouted.org")
    (NullusNet "nullus.net")
    (OFTC "oftc.net")
    (OpChat "opchat.org")
    (Openprojects "openprojects.net")
    (Othernet "othernet.org")
    (OtherSide "othersideirc.net")
    (Outsiderz "outsiderz.com")
    (OzOrg "oz.org")
    (Peacefulhaven "peacefulhaven.net")
    (PhazedIRC "phazedirc.net")
    (Philchat "philchat.net")
    (phrozN "phrozn.net")
    (PiNet "praetorians.org")
    (Pinoycentral "abs-cbn.com")
    (Planetarion "planetarion.com")
    (POLNet "ircnet.pl")
    (Psionics "psionics.net")
    (PTirc "ptirc.com.pt")
    (PTlink "ptlink.net")
    (PTnet "ptnet.org")
    (QChat "qchat.net")
    (QuakeNet "quakenet.org")
    (Realirc "realirc.org")
    (RealmNET "realmnet.com")
    (Rebelchat "rebelchat.org")
    (Red-Latina "red-latina.org")
    (RedLatona "redlatona.net")
    (Relicnet "relic.net")
    (Rezosup "rezosup.org")
    (Risanet "risanet.com")
    (Rubiks "rubiks.net")
    (Rusnet "nil")
    (Sandnet "sandnet.net")
    (Scunc "scunc.net")
    (SerbianCafe "serbiancafe.ws")
    (SexNet "sexnet.org")
    (ShadowFire "shadowfire.org")
    (ShadowWorld "shadowworld.net")
    (SkyNet "bronowski.pl")
    (SlashNET "slashnet.org")
    (SolarStone "solarstone.net")
    (Sorcery "sorcery.net")
    (SourceIRC "sourceirc.net")
    (SpaceTronix "spacetronix.net")
    (Spirit-Harmony "spirit-harmony.com")
    (StarChat "starchat.net")
    (StarEquinox "starequinox.net")
    (Starlink "starlink.net")
    (starlink-irc "starlink-irc.org")
    (StarWars-IRC "starwars-irc.net")
    (Stormdancing "stormdancing.net")
    (Superchat "superchat.org")
    (Sysopnet "sysopnet.org")
    (Telstra "telstra.com")
    (TR-net "dominet.com.tr")
    (Tri-net "tri-net.org")
    (TriLink "ft4u.net")
    (TurkishChat "turkishchat.org")
    (UberNinja "uberninja.net")
    (UICN "uicn.net")
    (UltraIRC "ultrairc.net")
    (UnderChat "underchat.it")
    (Undernet "undernet.org")
    (UnderZ "underz.org")
    (UniChat "irc.uni-chat.net")
    (UnionLatina "unionlatina.org")
    (Univers "univers.org")
    (UnixR "unixr.net")
    (Vidgamechat "vidgamechat.com")
    (VirtuaNet "virtuanet.org")
    (Vitamina "vitamina.ca")
    (Voila "voila.fr")
    (Wahou "wf-net.org")
    (Warpednet "warped.net")
    (Weaklinks "weaklinks.net")
    (Webnet "webchat.org")
    (WinChat "winchat.net")
    (WinIRC "winirc.org")
    (WorldIRC "worldirc.org")
    (WyldRyde "wyldryde.net")
    (XentoniX "xentonix.net")
    (Xevion "xevion.net")
    (XNet "xnet.org")
    (XWorld "xworld.org")
    (ZAnetNet "zanet.net")
    (ZAnetOrg "zanet.org.za")
    (ZiRC "zirc.org")
    (ZUHnet "zuh.net")
    (Zurna "zurna.net"))
  "Alist of IRC networks.
Each network is a list (NET MATCHER) where
NET is a symbol naming that IRC network and
MATCHER is used to find a corresponding network to a server while
connected to it.  If it is a regexp, it's used to match against
`erc-server-announced-name'."
  :type '(repeat
	  (list :tag "Network"
		(symbol :tag "Network name")
		(choice :tag "Network's common server ending"
		 (regexp)
		 (const :tag "Network has no common server ending" nil)))))

(defvar-local erc-network nil
  "The name of the network you are connected to (a symbol).")


;;;; Identifying session context

;; This section is concerned with identifying and managing the
;; relationship between an IRC connection and its unique identity on a
;; given network (as seen by that network's nick-granting system).
;; This relationship is quasi-permanent and transcends IRC connections
;; and Emacs sessions.  As of mid 2022, only nicknames matter, and
;; whether a user is authenticated does not directly impact network
;; identity from a client's perspective.  However, ERC must be
;; equipped to adapt should this ever change.  And while a connection
;; is normally associated with exactly one nick, some networks (or
;; intermediaries) may allow multiple clients to control the same nick
;; by combining instance activity into a single logical client.  ERC
;; must be limber enough to handle such situations.

(defvar-local erc-networks--id nil
  "Server-local instance of its namesake struct.
Also shared among all target buffers for a given connection.  See
\\[describe-symbol] `erc-networks--id' for more.")

(cl-defstruct erc-networks--id
  "Persistent identifying info for a network presence.

Here, \"presence\" refers to some local state representing a
client's existence on a network.  Some clients refer to this as a
\"context\" or a \"net-id\".  The management of this state
involves tracking associated buffers and what they're displaying.
Since a presence can outlast physical connections and survive
changes in back-end transports (and even outlive Emacs sessions),
its identity must be resilient.

Essential to this notion of an enduring existence on a network is
ensuring recovery from the loss of a server buffer.  Thus, any
useful identifier must be shared among server and target buffers
to allow for reassociation.  Beyond that, it must ideally be
derivable from the same set of connection parameters.  See the
constructor `erc-networks--id-create' for more info."
  (ts nil :type float :read-only t :documentation "Creation timestamp.")
  (symbol nil :type symbol :documentation "ID as a symbol."))

(cl-defstruct (erc-networks--id-fixed
               (:include erc-networks--id)
               (:constructor erc-networks--id-fixed-create
                             (given &aux (ts (float-time)) (symbol given)))))

(cl-defstruct (erc-networks--id-qualifying
               (:include erc-networks--id)
               (:constructor erc-networks--id-qualifying-create
                             (&aux
                              (ts (float-time))
                              (parts (erc-networks--id-qualifying-init-parts))
                              (symbol (erc-networks--id-qualifying-init-symbol
                                       parts))
                              (len 1))))
  "A session context composed of hierarchical connection parameters.
Two identifiers are considered equivalent when their non-empty
`parts' slots compare equal.  Related identifiers share a common
prefix of `parts' taken from connection parameters (given or
discovered).  An identifier's unique `symbol', intended for
display purposes, is created by concatenating the shortest common
prefix among its relatives.  For example, related presences [b a
r d o] and [b a z a r] would have symbols b/a/r and b/a/z
respectively.  The separator is given by `erc-networks--id-sep'."
  (parts nil :type sequence ; a vector of atoms
         :documentation "Sequence of identifying components.")
  (len 0 :type integer
       :documentation "Length of active `parts' interval."))

(define-inline erc-networks--id-string (id)
  "Return the symbol for `erc-networks--id' ID as a string."
  (inline-quote (symbol-name (erc-networks--id-symbol ,id))))

;; For now, please use this instead of `erc-networks--id-fixed-p'.
(cl-defgeneric erc-networks--id-given (net-id)
  "Return the preassigned identifier for a network context, if any.
When non-nil, assume NET-ID originated from an `:id' argument to
entry-point commands `erc-tls' or `erc'.")

(cl-defmethod erc-networks--id-given (_) nil) ; _ may be nil

(cl-defmethod erc-networks--id-given ((nid erc-networks--id-fixed))
  (erc-networks--id-symbol nid))

(cl-generic-define-context-rewriter erc-obsolete-var (var spec)
  `((with-suppressed-warnings ((obsolete ,var) (free-vars ,var)) ,var) ,spec))

;; As a catch-all, derive the symbol from the unquoted printed repr.
(cl-defgeneric erc-networks--id-create (id)
  "Invoke an appropriate constructor for an `erc-networks--id' object."
  (erc-networks--id-fixed-create (intern (format "%s" id))))

;; When a given ID is a symbol, trust it unequivocally.
(cl-defmethod erc-networks--id-create ((id symbol))
  (erc-networks--id-fixed-create id))

;; Otherwise, use an adaptive name derived from network params.
(cl-defmethod erc-networks--id-create ((_ null))
  (erc-networks--id-qualifying-create))

;; But honor an explicitly set `erc-rename-buffers' (compat).
(cl-defmethod erc-networks--id-create
  ((_ null) &context (erc-obsolete-var erc-rename-buffers null))
  (erc-networks--id-fixed-create (intern (buffer-name))))

;; But honor an explicitly set `erc-reuse-buffers' (compat).
(cl-defmethod erc-networks--id-create
  ((_ null) &context (erc-obsolete-var erc-reuse-buffers null))
  (erc-networks--id-fixed-create (intern (buffer-name))))

(cl-defmethod erc-networks--id-create
  ((_ symbol) &context (erc-obsolete-var erc-reuse-buffers null))
  (erc-networks--id-fixed-create (intern (buffer-name))))

(cl-defgeneric erc-networks--id-equal-p (self other)
  "Return non-nil when two network IDs exhibit underlying equality.
Expect SELF and OTHER to be `erc-networks--id' struct instances
and that this will only be called for ID recovery or merging,
after which no two identities should be `equal' (timestamps
aside) that aren't also `eq'.")

(cl-defmethod erc-networks--id-equal-p ((_ null) (_ erc-networks--id)) nil)
(cl-defmethod erc-networks--id-equal-p ((_ erc-networks--id) (_ null)) nil)

(cl-defmethod erc-networks--id-equal-p ((self erc-networks--id)
                                        (other erc-networks--id))
  (eq self other))

(cl-defmethod erc-networks--id-equal-p ((a erc-networks--id-fixed)
                                        (b erc-networks--id-fixed))
  (or (eq a b) (eq (erc-networks--id-symbol a) (erc-networks--id-symbol b))))

(cl-defmethod erc-networks--id-equal-p ((a erc-networks--id-qualifying)
                                        (b erc-networks--id-qualifying))
  (or (eq a b) (equal (erc-networks--id-qualifying-parts a)
                      (erc-networks--id-qualifying-parts b))))

;; ERASE-ME: if some future extension were to come along offering
;; additional members, e.g., [Libera.Chat "bob" laptop], it'd likely
;; be cleaner to create a new struct type descending from
;; `erc-networks--id-qualifying' than to convert this function into a
;; generic.  However, the latter would be simpler because it'd just
;; require something like &context (erc-v3-device erc-v3--device-t).

(defun erc-networks--id-qualifying-init-parts ()
  "Return opaque list of atoms to serve as canonical identifier."
  (when-let* ((network (erc-network))
              (nick (erc-current-nick)))
    (vector network (erc-downcase nick))))

(defvar erc-networks--id-sep "/"
  "Separator for joining `erc-networks--id-qualifying-parts' into a net ID.")

(defun erc-networks--id-qualifying-init-symbol (elts &optional len)
  "Return symbol appropriate for network context identified by ELTS.
Use leading interval of length LEN as contributing components.
Combine them with string separator `erc-networks--id-sep'."
  (when elts
    (unless len
      (setq len 1))
    (intern (mapconcat (lambda (s) (prin1-to-string s t))
                       (seq-subseq elts 0 len)
                       erc-networks--id-sep))))

(defun erc-networks--id-qualifying-grow-id (nid)
  "Grow NID by one component or return nil when at capacity."
  (unless (= (length (erc-networks--id-qualifying-parts nid))
             (erc-networks--id-qualifying-len nid))
    (setf (erc-networks--id-symbol nid)
          (erc-networks--id-qualifying-init-symbol
           (erc-networks--id-qualifying-parts nid)
           (cl-incf (erc-networks--id-qualifying-len nid))))))

(defun erc-networks--id-qualifying-reset-id (nid)
  "Restore NID to its initial state."
  (setf (erc-networks--id-qualifying-len nid) 1
        (erc-networks--id-symbol nid)
        (erc-networks--id-qualifying-init-symbol
         (erc-networks--id-qualifying-parts nid))))

(defun erc-networks--id-qualifying-prefix-length (nid-a nid-b)
  "Return length of common initial prefix of NID-A and NID-B.
Return nil when no such sequence exists (instead of zero)."
  (when-let* ((a (erc-networks--id-qualifying-parts nid-a))
              (b (erc-networks--id-qualifying-parts nid-b))
              (n (min (length a) (length b)))
              ((> n 0))
              ((equal (elt a 0) (elt b 0)))
              (i 1))
    (while (and (< i n)
                (equal (elt a i)
                       (elt b i)))
      (cl-incf i))
    i))

(defun erc-networks--id-qualifying-update (dest source &rest overrides)
  "Update DEST from SOURCE in place.
Copy slots into DEST from SOURCE and recompute ID.  Both SOURCE
and DEST must be `erc-networks--id' objects.  OVERRIDES is an
optional plist of SLOT VAL pairs."
  (setf (erc-networks--id-qualifying-parts dest)
        (or (plist-get overrides :parts)
            (erc-networks--id-qualifying-parts source))
        (erc-networks--id-qualifying-len dest)
        (or (plist-get overrides :len)
            (erc-networks--id-qualifying-len source))
        (erc-networks--id-symbol dest)
        (or (plist-get overrides :symbol)
            (erc-networks--id-qualifying-init-symbol
             (erc-networks--id-qualifying-parts dest)
             (erc-networks--id-qualifying-len dest)))))

(cl-defgeneric erc-networks--id-reload (_nid &optional _proc _parsed)
  "Handle an update to the current network identity.
If provided, PROC should be the current `erc-server-process' and
PARSED the current `erc-response'.  NID is an `erc-networks--id'
object."
  nil)

(cl-defmethod erc-networks--id-reload ((nid erc-networks--id-qualifying)
                                       &optional proc parsed)
  "Refresh identity after an `erc-networks--id-qualifying-parts'update."
  (erc-networks--id-qualifying-update nid (erc-networks--id-qualifying-create)
                                      :len
                                      (erc-networks--id-qualifying-len nid))
  (erc-networks--rename-server-buffer (or proc erc-server-process) parsed)
  (erc-networks--shrink-ids-and-buffer-names-any)
  (erc-with-all-buffers-of-server erc-server-process #'erc-target
    (when-let*
        ((new-name (erc-networks--reconcile-buffer-names erc--target nid))
         ((not (equal (buffer-name) new-name))))
      (rename-buffer new-name 'unique))))

(cl-defgeneric erc-networks--id-ensure-comparable (self other)
  "Take measures to ensure two net identities are in comparable states.")

(cl-defmethod erc-networks--id-ensure-comparable ((_ erc-networks--id)
                                                  (_ erc-networks--id))
  nil)

(cl-defmethod erc-networks--id-ensure-comparable
  ((nid erc-networks--id-qualifying) (other erc-networks--id-qualifying))
  "Grow NID along with that of the current buffer.
Rename the current buffer if its NID has grown."
  (when-let* ((n (erc-networks--id-qualifying-prefix-length other nid)))
    (while (and (<= (erc-networks--id-qualifying-len nid) n)
                (erc-networks--id-qualifying-grow-id nid)))
    ;; Grow and rename a visited buffer and all its targets
    (when (and (> (erc-networks--id-qualifying-len nid)
                  (erc-networks--id-qualifying-len other))
               (erc-networks--id-qualifying-grow-id other))
      ;; Rename NID's buffers using current ID
      (erc-buffer-filter (lambda ()
                           (when (eq erc-networks--id other)
                             (erc-networks--maybe-update-buffer-name)))))))

(defun erc-networks--id-sort-buffers (buffers)
  "Return a list of target BUFFERS, newest to oldest."
  (sort buffers
        (lambda (a b)
          (> (erc-networks--id-ts (buffer-local-value 'erc-networks--id a))
             (erc-networks--id-ts (buffer-local-value 'erc-networks--id b))))))


;;;; Buffer association

(cl-defgeneric erc-networks--shrink-ids-and-buffer-names ()
  nil) ; concrete default implementation for non-eliding IDs

(defun erc-networks--refresh-buffer-names (identity &optional omit)
  "Ensure all colliding buffers for network IDENTITY have suffixes.
Then rename current buffer appropriately.  Don't consider buffer OMIT
when determining collisions."
  (if (erc-networks--examine-targets identity erc--target
        #'ignore
        (lambda ()
          (unless (or (not omit) (eq (current-buffer) omit))
            (erc-networks--ensure-unique-target-buffer-name)
            t)))
      (erc-networks--ensure-unique-target-buffer-name)
    (rename-buffer (erc--target-string erc--target) 'unique)))

;; This currently doesn't equalize related identities that may have
;; become mismatched because that shouldn't happen after a connection
;; is up (other than for a brief moment while renicking or similar,
;; when states are inconsistent).
(defun erc-networks--shrink-ids-and-buffer-names-any (&rest omit)
  (let (grown)
    ;; Gather all grown identities.
    (erc-buffer-filter
     (lambda ()
       (when (and erc-networks--id
                  (erc-networks--id-qualifying-p erc-networks--id)
                  (not (memq (current-buffer) omit))
                  (not (memq erc-networks--id grown))
                  (> (erc-networks--id-qualifying-len erc-networks--id) 1))
         (push erc-networks--id grown))))
    ;; Check for other identities with shared prefix.  If none exists,
    ;; and an identity is overlong, shrink it.
    (dolist (nid grown)
      (let ((skip (not (null omit))))
        (catch 'found
          (if (cdr grown)
              (dolist (other grown)
                (unless (eq nid other)
                  (setq skip nil)
                  (when (erc-networks--id-qualifying-prefix-length nid other)
                    (throw 'found (setq skip t)))))
            (setq skip nil)))
        (unless (or skip (< (erc-networks--id-qualifying-len nid) 2))
          (erc-networks--id-qualifying-reset-id nid)
          (erc-buffer-filter
           (lambda ()
             (when (and (eq erc-networks--id nid)
                        (not (memq (current-buffer) omit)))
               (if erc--target
                   (erc-networks--refresh-buffer-names nid omit)
                 (erc-networks--maybe-update-buffer-name))))))))))

(cl-defmethod erc-networks--shrink-ids-and-buffer-names
  (&context (erc-networks--id erc-networks--id-qualifying))
  (erc-networks--shrink-ids-and-buffer-names-any (current-buffer)))

(defun erc-networks-rename-surviving-target-buffer ()
  "Maybe drop qualifying suffix from fellow target-buffer's name.
But only do so when there's a single survivor with a target
matching that of the dying buffer."
  (when-let*
      (((with-suppressed-warnings ((obsolete erc-reuse-buffers))
          erc-reuse-buffers))
       (target erc--target)
       ;; Buffer name includes ID suffix
       ((not (string= (erc--target-symbol target) ; string= t "t" -> t
                      (erc-downcase (buffer-name)))))
       (buf (current-buffer))
       ;; All buffers, not just those belonging to same process
       (others (erc-buffer-filter
                (lambda ()
                  (and-let* ((erc--target)
                             ((not (eq buf (current-buffer))))
                             ((eq (erc--target-symbol target)
                                  (erc--target-symbol erc--target))))))))
       ((not (cdr others))))
    (with-current-buffer (car others)
      (unless (get-buffer (erc--target-string target))
        (rename-buffer (erc--target-string target))))))

(defun erc-networks-shrink-ids-and-buffer-names ()
  "Recompute network IDs and buffer names, ignoring the current buffer.
Only do so when an IRC connection's context supports qualified
naming.  Do not discriminate based on whether a buffer's
connection is active."
  (erc-networks--shrink-ids-and-buffer-names))

(defun erc-networks--examine-targets (identity target on-dupe on-collision)
  "Visit all ERC target buffers with the same TARGET.
Call ON-DUPE when a buffer's identity belongs to a network
IDENTITY or \"should\" after reconciliation.  Call ON-COLLISION
otherwise.  Neither function should accept any args. Expect
TARGET to be an `erc--target' object."
  (declare (indent 2))
  (let ((announced erc-server-announced-name))
    (erc-buffer-filter
     (lambda ()
       (when (and erc--target (eq (erc--target-symbol erc--target)
                                  (erc--target-symbol target)))
         ;; When a server sends administrative queries immediately
         ;; after connection registration and before the session has a
         ;; net-id, the buffer remains orphaned until reassociated
         ;; here retroactively.
         (unless erc-networks--id
           (let ((id (erc-with-server-buffer erc-networks--id))
                 (server-buffer (process-buffer erc-server-process)))
             (apply #'erc-button--display-error-notice-with-keys
                    server-buffer
                    (concat "Missing network session (ID) for %S. "
                            (if id "Using `%S' from %S." "Ignoring."))
                    (current-buffer)
                    (and id (list (erc-networks--id-symbol
                                   (setq erc-networks--id id))
                                  server-buffer)))))
         (when erc-networks--id
           (let ((oursp (if (erc--target-channel-local-p target)
                            (equal announced erc-server-announced-name)
                          (erc-networks--id-equal-p identity
                                                    erc-networks--id))))
             (funcall (if oursp on-dupe on-collision)))))))))

(defconst erc-networks--qualified-sep "@"
  "Separator used for naming a target buffer.")

(defun erc-networks--construct-target-buffer-name (target)
  "Return TARGET@suffix."
  (concat (erc--target-string target)
          (if (with-suppressed-warnings ((obsolete erc-reuse-buffers))
                erc-reuse-buffers)
              erc-networks--qualified-sep "/")
          (cond
           ((not (with-suppressed-warnings ((obsolete erc-reuse-buffers))
                   erc-reuse-buffers))
            (cadr (split-string
                   (erc-networks--id-string erc-networks--id)
                   "/")))
           ((erc--target-channel-local-p target) erc-server-announced-name)
           (t (erc-networks--id-string erc-networks--id)))))

(defun erc-networks--ensure-unique-target-buffer-name ()
  (when-let* ((new-name (erc-networks--construct-target-buffer-name
                         erc--target))
              ((not (equal (buffer-name) new-name))))
    (rename-buffer new-name 'unique)))

(defun erc-networks--ensure-unique-server-buffer-name ()
  (when-let* ((new-name (erc-networks--id-string erc-networks--id))
              ((not (equal (buffer-name) new-name))))
    (rename-buffer new-name 'unique)))

(defun erc-networks--maybe-update-buffer-name ()
  "Update current buffer name to reflect display ID if necessary."
  (if erc--target
      (erc-networks--ensure-unique-target-buffer-name)
    (erc-networks--ensure-unique-server-buffer-name)))

(defun erc-networks--reconcile-buffer-names (target nid)
  "Reserve preferred buffer name for TARGET and network identifier.
Expect TARGET to be an `erc--target' instance.  Guarantee that at
most one existing buffer has the same `erc-networks--id' and a
case-mapped target, i.e., `erc--target-symbol'.  If other buffers
with equivalent targets exist, rename them to TARGET@their-NID
and return TARGET@our-NID.  Otherwise return TARGET as a string.
When multiple buffers for TARGET exist for the current NID,
rename them with <n> suffixes going from newest to oldest."
  (let* (existing ; Former selves or unexpected dupes (for now allow > 1)
         ;; Renamed ERC buffers on other networks matching target
         (namesakes (erc-networks--examine-targets nid target
                      (lambda () (push (current-buffer) existing) nil)
                      ;; Append network ID as TARGET@NID,
                      ;; possibly qualifying to achieve uniqueness.
                      (lambda ()
                        (unless (erc--target-channel-local-p erc--target)
                          (erc-networks--id-ensure-comparable
                           nid erc-networks--id))
                        (erc-networks--ensure-unique-target-buffer-name)
                        t)))
         ;; Must follow ^ because NID may have been modified
         (name (if (or namesakes (not (with-suppressed-warnings
                                          ((obsolete erc-reuse-buffers))
                                        erc-reuse-buffers)))
                   (erc-networks--construct-target-buffer-name target)
                 (erc--target-string target)))
         placeholder)
    ;; If we don't exist, claim name temporarily while renaming others
    (when-let* ((ex (get-buffer name))
                ((not (memq ex existing))))
      (if namesakes ; if namesakes is nonempty, it contains ex
          (with-current-buffer ex
            (let ((temp-name (generate-new-buffer-name (format "*%s*" name))))
              (rename-buffer temp-name)
              (setq placeholder (get-buffer-create name))
              (rename-buffer name 'unique)))
        ;; Here, ex must be a server buffer or a non-ERC buffer
        (setq name (erc-networks--construct-target-buffer-name target))))
    (unless (with-suppressed-warnings ((obsolete erc-reuse-buffers))
              erc-reuse-buffers)
      (when (string-suffix-p ">" name)
        (setq name (string-trim-right name (rx "<" (+ digit) ">")))))
    (dolist (ex (erc-networks--id-sort-buffers existing))
      (with-current-buffer ex
        (rename-buffer name 'unique)))
    (when placeholder (kill-buffer placeholder))
    name))


;; Functions:

;;;###autoload
(defun erc-determine-network ()
  "Return the name of the network or \"Unknown\" as a symbol.
Use the server parameter NETWORK if provided, otherwise parse the
server name and search for a match in `erc-networks-alist'."
  ;; The server made it easy for us and told us the name of the NETWORK
  (declare (obsolete "maybe see `erc-networks--determine'" "29.1"))
  (defvar erc-server-parameters)
  (defvar erc-session-server)
  (let ((network-name (cdr (assoc "NETWORK" erc-server-parameters))))
    (if network-name
	(intern network-name)
      (or
       ;; Loop through `erc-networks-alist' looking for a match.
       (let ((server (or erc-server-announced-name erc-session-server)))
	 (cl-loop for (name matcher) in erc-networks-alist
		  when (and matcher
			    (string-match (concat matcher "\\'") server))
		  do (cl-return name)))
       'Unknown))))

(defun erc-network ()
  "Return the value of `erc-network' for the current server."
  (or erc-network (erc-with-server-buffer erc-network)))

(defun erc-network-name ()
  "Return the name of the current network as a string."
  (erc-with-server-buffer (symbol-name erc-network)))

(defun erc-set-network-name (_proc _parsed)
  "Set `erc-network' to the value returned by `erc-determine-network'."
  (declare (obsolete "maybe see `erc-networks--set-name'" "29.1"))
  (unless erc-server-connected
    (setq erc-network (with-suppressed-warnings
                          ((obsolete erc-determine-network))
                        (erc-determine-network))))
  nil)

(defconst erc-networks--name-missing-sentinel (gensym "Unknown ")
  "Value to cover rare case of a literal NETWORK=nil.")

(defun erc-networks--determine (&optional server)
  "Return the name of the network as a symbol.
Search `erc-networks-alist' for a known entity matching SERVER or
`erc-server-announced-name'.  If that fails, use the display name
given by the `RPL_ISUPPORT' NETWORK parameter."
  (or (cl-loop for (name matcher) in erc-networks-alist
               when (and matcher
                         (string-match (concat matcher "\\'")
                                       (or server erc-server-announced-name)))
               return name)
      (and-let* ((vanity (erc--get-isupport-entry 'NETWORK 'single))
                 ((intern vanity))))
      (erc-networks--id-given erc-networks--id)
      erc-networks--name-missing-sentinel))

(defvar erc-networks--allow-unknown-network nil
  "Whether to ignore a failure in identifying the network.
If you need this as a user option, please say so via \\[erc-bug].
Otherwise, expect it to vanish at any time.") ; Bug#59976

(defun erc-networks--set-name (proc parsed)
  "Set `erc-network' to the value returned by `erc-networks--determine'.
Print an error message when the network cannot be determined before
shutting down the connection."
  ;; Always update (possibly clobber) current value, if any.
  (pcase (setq erc-network (erc-networks--determine))
    ((and (pred (eq (erc-networks--id-given erc-networks--id)))
          (let m (format "Couldn't determine network. Using given ID `%s'."
                         erc-network)))
     (erc-display-message parsed 'notice nil m)
     nil)
    ((guard (eq erc-network erc-networks--name-missing-sentinel))
     ;; This can happen theoretically, e.g., when adjusting settings
     ;; on a proxy service that partially impersonates IRC but isn't
     ;; currently conveying anything through to a real network.  The
     ;; service may send a 422 but no NETWORK param (or *any* 005s).
     (erc-button--display-error-notice-with-keys
      "Failed to determine network. Please set entry for \""
      erc-server-announced-name "\" in `erc-networks-alist' or consider"
      " calling `erc-tls' with the keyword `:id'."
      " See Info:\"(erc) Network Identifier\" for more.")
     (if erc-networks--allow-unknown-network
         (progn
           (erc-display-error-notice
            parsed (format "Continuing anyway with network set to `%s'."
                           erc-network))
           nil)
       (delete-process proc)
       'error))))

;; This lives here in this file because all the other "on connect"
;; MOTD stuff ended up here (but perhaps that needs to change).

(defun erc-networks--ensure-announced (_ parsed)
  "Set a fallback `erc-server-announced-name' if still unset.
Copy source (prefix) from MOTD-ish message as a last resort."
  ;; The 004 handler never ran; see 2004-03-10 Diane Murray in change log
  (unless erc-server-announced-name
    (require 'erc-button)
    (erc-button--display-error-notice-with-keys
     "Failed to determine server name. Using \""
     (setq erc-server-announced-name (erc-response.sender parsed)) "\" instead"
     ". If this was unexpected, consider reporting it via \\[erc-bug]."))
  nil)

(defun erc-unset-network-name (_nick _ip _reason)
  "Set `erc-network' to nil."
  (declare (obsolete "`erc-network' is now effectively read-only" "29.1"))
  (setq erc-network nil)
  nil)

(defun erc-networks--transplant-buffer-content (src dest)
  "Insert buffer SRC's contents into DEST, above its contents."
  (with-silent-modifications
    (let ((content (with-current-buffer src
                     (cl-assert (not (buffer-narrowed-p)))
                     (erc--insert-admin-message 'graft ?n dest ?o src)
                     (buffer-substring (point-min) erc-insert-marker))))
      (with-current-buffer dest
        (save-excursion
          (save-restriction
            (cl-assert (not (buffer-narrowed-p)))
            (goto-char (point-min))
            (while (and (eql ?\n (char-after (point)))
                        (null (text-properties-at (point))))
              (delete-char 1))
            (insert-before-markers content)))))))

(defvar erc-networks--transplant-target-buffer-function
  #'erc-networks--transplant-buffer-content
  "Function to rename and merge the contents of two target buffers.
Called with the donating buffer to be killed and buffer to receive the
transplant.  Consuming modules can leave a marker at the beginning of
the latter buffer to access the insertion point, if needing to do things
like adjust invisibility properties, etc.")

(defvar erc-networks--target-transplant-in-progress-p nil
  "Non-nil when merging target buffers.")

;; This should run whenever a network identity is updated.
(defun erc-networks--reclaim-orphaned-target-buffers (new-proc nid announced)
  "Visit disowned buffers for same NID and associate with NEW-PROC.
Expect ANNOUNCED to be the server's reported host name."
  (erc-buffer-filter
   (lambda ()
     (when (and erc--target
                (not erc-server-connected)
                (erc-networks--id-equal-p erc-networks--id nid)
                (or (not (erc--target-channel-local-p erc--target))
                    (string= erc-server-announced-name announced)))
       ;; If a target buffer exists for the current process, kill this
       ;; stale one after transplanting its content; else reinstate.
       (if-let* ((actual (erc-get-buffer (erc--target-string erc--target)
                                         new-proc))
                 (erc-networks--target-transplant-in-progress-p t))
           (progn
             (funcall erc-networks--transplant-target-buffer-function
                      (current-buffer) actual)
             (kill-buffer (current-buffer))
             (with-current-buffer actual
               (erc-networks--ensure-unique-target-buffer-name)))
         (setq erc-server-process new-proc
               erc-server-connected t
               erc-networks--id nid))))))

;; For existing buffers, `erc-open' reinitializes a core set of local
;; variables in addition to some text, such as the prompt.  It expects
;; module activation functions to do the same for assets they manage.
;; However, "stateful" modules, whose functionality depends on the
;; evolution of a buffer's content, may need to reconcile state during
;; a merge.  An example might be a module that provides consistent
;; timestamps: it should ensure time values don't decrease.
(defvar erc-networks--copy-server-buffer-functions nil
  "Abnormal hook run in new server buffers when deduping.
Passed the existing buffer to be killed, whose contents have
already been copied over to the current, replacement buffer.")

(defun erc-networks--copy-over-server-buffer-contents (existing name)
  "Kill off existing server buffer after copying its contents.
Expect to be called from the replacement buffer."
  (defvar erc-kill-buffer-hook)
  (defvar erc-kill-server-hook)
  ;; The following observations from ERC 5.5 regarding the buffer
  ;; `existing' were thought at the time to be invariants:
  ;; - `erc-networks--id' is `erc-networks--id-equal-p' to the
  ;;    caller's network identity and older if not `eq'.
  ;; - `erc-server-process' should be set (local) but dead and `eq' to
  ;;    the result of `get-buffer-process' unless the latter is nil.
  (delete-process (buffer-local-value 'erc-server-process existing))
  (erc-networks--transplant-buffer-content existing (current-buffer))
  (let (erc-kill-server-hook erc-kill-buffer-hook)
    (run-hook-with-args 'erc-networks--copy-server-buffer-functions existing)
    (kill-buffer name)))

;; This stands alone for testing purposes

(defun erc-networks--update-server-identity ()
  "Maybe grow or replace the current network identity.
If a dupe is found, adopt its identity by overwriting ours.
Otherwise, take steps to ensure it can effectively be compared to
ours, now and into the future.  Note that target buffers are
considered as well because server buffers are often killed."
  (let* ((identity erc-networks--id)
         (buffer (current-buffer))
         (f (lambda ()
              (unless (or (not erc-networks--id)
                          (eq (current-buffer) buffer)
                          (eq erc-networks--id identity))
                (if (erc-networks--id-equal-p identity erc-networks--id)
                    (throw 'buffer erc-networks--id)
                  (erc-networks--id-ensure-comparable identity
                                                      erc-networks--id)
                  nil))))
         (found (catch 'buffer (erc-buffer-filter f))))
    (when found
      (setq erc-networks--id found))))

;; These steps should only run when initializing a newly connected
;; server buffer, whereas `erc-networks--rename-server-buffer' can run
;; mid-session, after an identity's core components have changed.

(defun erc-networks--init-identity (proc parsed)
  "Update identity with real network name."
  ;; Initialize identity for real now that we know the network
  (cl-assert erc-network)
  (if erc-networks--id
      (erc-networks--id-reload erc-networks--id proc parsed)
    (setq erc-networks--id (erc-networks--id-create nil))
    ;; Find duplicate identities or other conflicting ones and act
    ;; accordingly.
    (erc-networks--update-server-identity)
    (erc-networks--rename-server-buffer proc parsed))
  nil)

(defun erc-networks--rename-server-buffer (new-proc &optional _parsed)
  "Rename a server buffer based on its network identity.
Assume that the current buffer is a server buffer, either one
with a newly established connection whose identity has just been
fully fleshed out, or an existing one whose identity has just
been updated.  Either way, assume the current identity is ready
to serve as a canonical identifier.

When a server buffer already exists with the chosen name, copy
over its contents and kill it.  However, when its process is
still alive, kill off the current buffer.  This can happen, for
example, after a perceived loss in network connectivity turns out
to be a false alarm.  If `erc-reuse-buffers' is nil, let
`generate-new-buffer-name' do the actual renaming."
  (cl-assert (eq new-proc erc-server-process))
  (cl-assert (erc-networks--id-symbol erc-networks--id))
  ;; Always look for targets to reassociate because original server
  ;; buffer may have been deleted.
  (erc-networks--reclaim-orphaned-target-buffers new-proc erc-networks--id
                                                 erc-server-announced-name)
  (let* ((name (erc-networks--id-string erc-networks--id))
         ;; When this ends up being the current buffer, either we have
         ;; a "given" ID or the buffer was reused on reconnecting.
         (existing (get-buffer name)))
    (process-put new-proc 'erc-networks--id erc-networks--id)
    (cond ((or (not existing)
               (erc-networks--id-given erc-networks--id)
               (eq existing (current-buffer)))
           (rename-buffer name))
          ;; Abort on accidental reconnect or failure to pass :id param for
          ;; avoidable collisions.
          ((erc-server-process-alive existing)
           (kill-local-variable 'erc-network)
           (delete-process new-proc)
           (erc-display-error-notice nil (format "Buffer %s still connected"
                                                 name))
           (erc-set-active-buffer existing))
          ;; Copy over old buffer's contents and kill it
          ((with-suppressed-warnings ((obsolete erc-reuse-buffers))
             erc-reuse-buffers)
           (erc-networks--copy-over-server-buffer-contents existing name)
           (rename-buffer name))
          (t (rename-buffer (generate-new-buffer-name name)))))
  nil)

;; Soju v0.4.0 sends ISUPPORT and nothing else on upstream reconnect,
;; so this actually doesn't apply.  ZNC 1.8.2, however, still sends
;; the entire burst.
(defvar erc-networks--bouncer-targets '(*status bouncerserv)
  "Symbols matching proxy-bot targets.")

(defun erc-networks-on-MOTD-end (proc parsed)
  "Call on-connect functions with server PROC and PARSED message."
  ;; This should normally run before `erc-server-connected' is set.
  ;; However, bouncers and other proxies may interfere with that.
  (when erc-server-connected
    (unless (erc-buffer-filter (lambda ()
                                 (and erc--target
                                      (memq (erc--target-symbol erc--target)
                                            erc-networks--bouncer-targets)))
                               proc)
      (require 'erc-button)
      (erc-button--display-error-notice-with-keys
       "Unexpected state detected. Please report via \\[erc-bug].")))

  ;; For now, retain compatibility with erc-server-NNN-functions.
  (or (erc-networks--ensure-announced proc parsed)
      (erc-networks--set-name proc parsed)
      (erc-networks--init-identity proc parsed)))

(define-erc-module networks nil
  "Provide data about IRC networks."
  ((add-hook 'erc-server-376-functions #'erc-networks-on-MOTD-end)
   (add-hook 'erc-server-422-functions #'erc-networks-on-MOTD-end))
  ((remove-hook 'erc-server-376-functions #'erc-networks-on-MOTD-end)
   (remove-hook 'erc-server-422-functions #'erc-networks-on-MOTD-end)))

(defun erc-networks--warn-on-connect ()
  "Emit warning when the `networks' module hasn't been loaded.
Ideally, do so upon opening the network process."
  (unless (or erc--target erc-networks-mode)
    (let ((m (concat "Required module `networks' not loaded.  If this "
                     " was unexpected, please add it to `erc-modules'.")))
      ;; Assume the server buffer has been marked as active.
      (erc-display-error-notice
       nil (concat m "  See Info:\"(erc) Required Modules\" for more."))
      (lwarn 'erc :warning m))))

(defun erc-ports-list (ports)
  "Return a list of PORTS.

PORTS should be a list of either:
  A number, in which case it is returned a list.
  Or a pair of the form (LOW HIGH), in which case, a list of all the
  numbers between LOW and HIGH (inclusive) is returned.

As an example:
  (erc-ports-list \\='(1)) => (1)
  (erc-ports-list \\='((1 5))) => (1 2 3 4 5)
  (erc-ports-list \\='(1 (3 5))) => (1 3 4 5)"
  (let (result)
    (dolist (p (ensure-list ports))
      (cond ((numberp p)
	     (push p result))
	    ((listp p)
	     (setq result (nconc (cl-loop for i from (cadr p) downto (car p)
					  collect i)
				 result)))))
    (nreverse result)))

(defun erc-networks--server-select ()
  "Prompt for a server in `erc-server-alist' and return its irc(s):// URL.
Choose port at random if multiple candidates exist, but always
prefer TLS without asking.  When a port can't be determined,
return the host alone sans URL formatting (for compatibility)."
  (let* ((completion-ignore-case t)
	 (net (intern
	       (completing-read "Network: "
				(delete-dups
				 (mapcar (lambda (x)
                                           (list (nth 1 x)))
					 erc-server-alist)))))
         (s-choose (lambda (entry)
                     (and (equal (nth 1 entry) net)
                          (if-let* ((b (string-search ": " (car entry))))
                              (cons (format "%s (%s)" (nth 2 entry)
                                            (substring (car entry) (+ b 2)))
                                    (cdr entry))
                            entry))))
         (s-entries (delq nil (mapcar s-choose erc-server-alist)))
         (srv (assoc (completing-read "Server: " s-entries) s-entries))
	 (host (nth 2 srv))
         (pspec (nthcdr 3 srv))
         (ports (erc-ports-list (or (cadr pspec) (car pspec))))
         (scheme (if (cdr pspec) "ircs" "irc")))
    (if ports (format "%s://%s:%d" scheme host (seq-random-elt ports)) host)))

;;; The following experimental
;; It does not work yet, help me with it if you
;; think it is worth the effort.

(defvar erc-settings
  '((pals Libera.Chat ("kensanata" "shapr" "anti\\(fuchs\\|gone\\)"))
    (format-nick-function (Libera.Chat "#emacs") erc-format-@nick))
  "Experimental: Alist of configuration options.

WARNING: this variable is a vestige from a long-abandoned
experiment.  ERC may redefine it using the same name for any
purpose at any time.

The format is (VARNAME SCOPE VALUE) where
VARNAME is a symbol identifying the configuration option,
SCOPE is either a symbol which identifies an entry from
  `erc-networks-alist' or a list (NET TARGET) where NET is a network symbol and
  TARGET is a string identifying the channel/query target.
VALUE is the options value.")
(make-obsolete-variable 'erc-settings
                        "temporarily deprecated for later repurposing" "30.1")

(defun erc-get (var &optional net target)
  "Retrieve configuration values from `erc-settings'.

WARNING: this function is a non-functioning remnant from a
long-abandoned experiment.  ERC may redefine it using the same
name for any purpose at any time.

\(fn &rest UNKNOWN)"
  (declare (obsolete "temporarily deprecated for later repurposing" "30.1"))
  (let ((items erc-settings)
	elt val)
    (while items
      (setq elt (car items)
	    items (cdr items))
      (when (eq (car elt) var)
	(cond ((and net target (listp (nth 1 elt))
		    (eq net (car (nth 1 elt)))
		    (string-equal target (nth 1 (nth 1 elt))))
	       (setq val (nth 2 elt)
		     items nil))
	      ((and net (eq net (nth 1 elt)))
	       (setq val (nth 2 elt)
		     items nil))
	      ((and (not net) (not target) (not (nth 1 elt)))
	       (setq val (nth 2 elt)
		     items nil)))))
    val))

;; (erc-get 'pals 'Libera.Chat)

(provide 'erc-networks)

;;; erc-networks.el ends here
