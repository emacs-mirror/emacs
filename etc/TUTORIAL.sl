Copyright (c) 1985,96,97 Free Software Foundation, Inc;  See end for conditions.
Prvo berilo za Emacs

Ukazi v Emacsu v splo¹nem vkljuèujejo tipki CONTROL (vèasih oznaèeni
CTRL ali CTL) in META (vèasih oznaèena EDIT ali ALT). Namesto, da bi ju
vedno izpisali s celim imenom, bomo uporabili naslednji okraj¹avi:

 C-<znak> pomeni, da moramo dr¾ati pritisnjeno tipko CONTROL, ko
 	  vtipkamo <znak>. Oznaka C-f tako pomeni: dr¾imo pritisnjeno
 	  tipko CONTROL in pritisnemo tipko f.
 M-<znak> pomeni, da moramo dr¾ati pritisnjeno tipko META, EDIT ali
 	  ALT, ko vtipkamo <znak>. Èe na tipkovnici ni tipk META, EDIT
 	  ali ALT, pritisnemo tipko ESC, jo spustimo in zatem
 	  pritisnemo tipko <chr>. Tipko ESC bomo oznaèevali z <ESC>.

Pomembno: Emacs zapustimo z ukazom C-x C-c (dva znaka).
V uèbeniku so vaje, s katerimi preskusite nove ukaze. Oznaèujeta jih 
znaka ,>>` ob levem robu. Zgled:
<<Blank lines inserted here by startup of help-with-tutorial>>
[Sredina strani je iz didaktiènih razlogov prazna. Besedilo se nadaljuje spodaj]
>> Vtipkajte zdaj ukaz C-v (View next screen, Prika¾i naslednji zaslon),
	da se premaknete na naslednji zaslon (kar poskusite, pritisnite
	hkrati tipko CONTROL in V). Od zdaj naprej boste morali to
	napraviti sami vsakiè, ko pridete do konca zaslona.

Ste opazili, da sta se dve vrstici s prej¹njega zaslona ponovili? Ta
kontinuiteta olaj¹a branje pri skakanju s strani na stran.

Prva stvar, ki si jo morate zapomniti, je, kako se premikate po
datoteki. Zdaj ¾e veste, da se premaknete za cel zaslon naprej z
ukazom C-v. Za cel zaslon nazaj pa se premaknete z ukazom M-v
(pritisnite tipko META in jo dr¾ite ter pritisnite tipko v, ali pa
pritisnite in spustite <ESC> ter zatem pritisnite tipko v, èe tipke
META, EDIT ali ALT na va¹i tipkovnici ni).

>>  Nekajkrat pritisnite M-v in C-v, da vidite, kako ukaza delujeta.


* POVZETEK
----------

Za pregled celega zaslona besedila so uporabni naslednji ukazi:

	C-v	Premik se za cel zaslon naprej
	M-v	Premik se za cel zaslon nazaj
	C-l	Cel zaslon premaknemo tako, da je zdaj po vertikali
		 osredninjen okoli besedila, kjer se nahaja kazalèek
		 (znak v C-l je èrka L, ne ¹tevka 1)

>> Poi¹èite kazalèek na zaslonu in si zapomnite besedilo okoli njega.
   Vtipkajte C-l.
   Ponovno poi¹èite kazalèek. Besedilo okoli njega je ostalo isto.


* PREMIKANJE KAZALÈKA
---------------------

Premiki za celo stran naprej in nazaj so sicer uporabni, ampak kako pa
pridemo do izbranega mesta na zaslonu?

Naèinov je veè. Najosnovnej¹i je uporaba ukazov C-p, C-b, C-f in
C-n. Ti po vrsti premaknejo kazalèek v prej¹njo vrstico, znak nazaj,
znak naprej, in v naslednjo vrstico. Grafièno prikazano:

			  prej¹nja vrstica, C-p
				  :
				  :
       nazaj, C-b .... trenutni polo¾aj kazalèka .... naprej, C-f
				  :
				  :
			  naslednja vrstica, C-n

>> S pritiski na C-n ali C-p premaknite kazalèek v sredinsko vrstico
   na diagramu zgoraj. Zatem pritisnite C-l. S tem diagram postavite na
   sredino zaslona.

V angle¹èini ima izbor tipk nazoren pomen. P kot ,previous` (prej¹nji),
N kot ,next` (naslednji), B kot ,backward` (nazaj) in F kot ,forward`
(naprej). To so osnovni ukazi za premikanje kazalèka in uporabljali jih
boste VES ÈAS. Èim prej se jih nauèite, tem bolje.

>> Nekajkrat pritisnite C-n, da pride kazalèek do te vrstice.

>> Z nekaj C-f se pomaknite na desno na sredo vrstice, nato pa nekajkrat
   pritisnite C-p. Opazujte, kaj se dogaja s kazalèkom na sredini
   vrstice.

Vsaka vrstice v besedilu je zakljuèena z znakom za novo vrstico
(angl. Newline). Ta loèuje vrstico v besedilu od naslednje. Tudi
zadnja vrstica v datoteki mora biti zaljuèena z znakom za novo vrstico
(èeprav tega Emacs ne zahteva).

>> Poskusite ukaz C-b, ko je kazalèek na zaèetku vrstice. Kazalèek se
   mora premakniti na konec prej¹nje vrstice. To je zato, ker se je
   ravnokar premaknil prek znaka za konec vrstice.

Ukaz C-f premika kazalèek prek znaka za novo vrstico enako kot C-b.

>> Poskusite ¹e nekajkrat pritisniti C-b, da dobite obèutek za
   premikanje kazalèka. Potem nekajkrat poskusite C-f, da pridete do konca
   vrstice. ©e enkrat pritisnite C-f, da skoèite v naslednjo vrstico.

Ko s kazalèkom dose¾ete zgornji ali spodnji rob zaslona, se besedilo
toliko premakne, da kazalèek ostane na zaslonu. V angle¹èini se temu
pravi ,,scrolling``. To omogoèa, da lahko premaknemo kazalèek na
katerokoli mesto v besedilu, a vseeno ostanemo na zaslonu.

>> Poskusite kazalèek pripeljati s C-n èisto do dna zaslona in si oglejte,
   kaj se zgodi.

Èe se vam zdi premikanje po en znak prepoèasno, se lahko premikate za
celo besedo. M-f (META-f) premakne kazalèek za eno besedo naprej, M-b
pa za besedo nazaj.

>> Poskusite nekajkrat M-f in M-b.

Èe je kazalèek sredi besede, ga M-f prestavi na konec besede. Èe je v
belini med besedami, ga M-f premakne na konec naslednje besede. M-b
deluje podobno, a v nasprotni smeri.

>> Nekajkrat poskusite M-f in M-b, vmes pa ¹e nekaj C-f in
   C-b. Opazujte uèinke M-f in M-b, ko je kazalèek sredi besede ali
   med besedami.

Ste opazili paralelo med C-f in C-b na eni strani ter M-f in M-b na
drugi? V Emacsu se dostikrat ukazi Meta nana¹ajo na operacije nad
enotami jezika (besede, stavki, odstavki), medtem ko se ukazi Control 
nana¹ajo na operacije, neodvisne od zvrsti besedila (znaki, vrstice
ipd.).

Podobna zveza je tudi med vrsticami in stavki: ukaza C-a in C-e
premakneta kazalèek na zaèetek oz. konec vrstice, M-a in M-e pa na
zaèetek oz. konec stavka.

>> Poskusite nekaj ukazov C-a, potem pa nekaj ukazov C-e.
   Poskusite nekaj ukazov M-a, potem pa nekaj ukazov M-e.

Ste opazili, da ponovljeni C-a ne napravijo niè, ponovljeni M-a pa se
premikajo naprej? Èeprav se ne obna¹ata enako, pa je vendar obna¹anje
enega in drugega po svoje naravno.

Polo¾aju kazalèka na zaslonu pravimo tudi ,,point``, toèka.
Parafrazirano: kazalèek ka¾e na zaslonu, kje je toèka v besedilu.

Povzetek preprostih ukazov za premikanje kazalèka, vkljuèno s premiki
po besedo in stavek:

	C-f	Premik za znak naprej
	C-b	Premik za znak nazaj

	M-f	Premik za besedo naprej
	M-b	Premik za besedo nazaj

	C-n	Premik v naslednjo vrstico
	C-p	Premik v prej¹njo vrstico

	C-a	Premik na zaèetek vrstice
	C-e	Premik na konec vrstice

	M-a	Premik na zaèetek stavka
	M-e	Premik na konec stavka

>> Za vajo nekajkrat poskusite vsakega od teh ukazov.
   To so najpogosteje uporabljani ukazi.

©e dva pomembna ukaza za premikanje kazalèka sta M-< (META-manj¹i od),
ki ga premakne na zaèetek datoteke, in M-> (META-veèji od), ki ga
premakne na konec datoteke.

Na ameri¹kih tipkovnicah najdete znak < nad vejico in morate
pritisniti tipko Shift, da pridete do njega. Z ukazom M-< je enako -
prav tako morate pritisniti tipko Shift, sicer moste izvedli drug
ukaz, Meta-vejica. Na na¹ih tipkovnicah sta oba znaka na isti tipko,
in za ukaz M-> morate pritisniti ¹e tipko Shift.

>> Poskusite zdaj M-<, skok na zaèetek tega uèbenika.
   Potem se vrnite nazaj z zaporednimi C-v.

>> Poskusite zdaj M->, skok na konec tega uèbenika.
   Potem se vrnite nazaj z zaporednimi M-v.

Èe ima va¹a tipkovnica kurzorske tipke, lahko premikate kazalèek po
zaslonu tudi z njimi. Vseeno priporoèamo, da se privadite ukazov C-b,
C-f, C-n in C-p, in to iz treh razlogov. Prviè, delujejo na èisto vseh
terminalih. Drugiè, z nekaj prakse v Emacsu boste opazili, da je
tipkanje ukazov s CONTROL hitrej¹e od tipkanja s kurzorskimi tipkami, ker
ni treba ves èas premikati desnice s tipkovnice na kurzorske tipke in
nazaj. In tretjiè, ko se enkrat navadite teh ukazov s CONTROL, se boste
enostavneje nauèili tudi bolj zapletenih ukazov za premikanje kazalèka.

Veèini ukazov v Emacsu lahko podamo ¹tevilèni argument; najveèkrat ta
pove, kolikokrat zapovrstjo naj se ukaz izvede. Veèkratno ponovitev
ukaza izvedemo tako, da najprej vtipkamo C-u, zatem ¹tevilo,
kolikokrat naj se ukaz ponovi, in nazadnje ¾eljeni ukaz. Èe ima va¹a
tipkovnica tipko META (ali EDIT ali ALT), lahko izpustite ukaz C-u in
namesto tega vtipkate ¹tevilo ponovitev, medtem ko dr¾ite pritisnjeno
tipko META. Druga metoda je sicer kraj¹a, priporoèamo pa prvo, ker
deluje na vseh terminalih. Tak¹en ¹tevilèni argument je ,,prefiksni``
argument, ker vnesemo argument pred ukazom, na katerega se nana¹a.

Zgled: C-u 8 C-f premakne kazalèek za osem znakov naprej.

>> Poskusite s primernim argumentom za ¹tevilo ponovitev ukaza
   C-n ali C-p priti èim bli¾e tej vrstici v enem samem skoku.

Veèina ukazov, ne pa vsi, uporablja ¹tevilèni argument kot ¹tevilo
ponovitev ukaza. Nekateri ukazi - nobeden od tistih, ki smo si jih
ogledali do zdaj - ga uporabljajo kot stikalo: s podanim prefiksnim
argumentom napravi ukaz nekaj drugega kot obièajno.

Ukaza C-v in M-v sta tudi izjemi, a drugaèni. Èe jima podamo argument,
premakneta zaslon za navedeno ¹tevilo vrstic, ne pa zaslonov. Ukaz C-u
8 C-v, na primer, premakne zaslon navzgor za 8 vrstic.

>> Poskusite zdaj C-u 8 C-v

To bi moralo zaslon premakniti navzgor za osem vrstic. Èe bi ga radi
premaknili nazaj, poskusite M-v z istim argumentom.

Èe uporabljate grafièni vmesnik, denimo X11 ali MS Windows, imate
verjetno ob levem robu Emacsovega okna navpièno pravokotno ploskev,
imenovano drsnik. Pogled na besedilo lahko premikate tudi tako, da z
mi¹ko kliknete na drsnik.

>> Postavite kazalec na vrh oznaèenega obmoèja na drsniku in pritisnite
   srednji gumb na mi¹ki. To bi moralo premakniti besedilo na mesto,
   doloèeno s tem, kako visoko ali nizko na drsnik ste kliknili.

>> Medtem ko dr¾ite srednji gumb pritisnjen, premikajte mi¹ko gor in
   dol. Vidite, kako se premika besedilo v Emacsovem oknu, ko
   premikate mi¹ko?


* ÈE SE EMACS OBESI
-------------------

Èe se Emacs preneha odzivati na va¹e ukaze, ga lahko varno prekinete z
ukazom C-g. Z njim lahko prekinete ukaze, za katere bi trajalo
predolgo, da bi se izvedli.

Isti ukaz, C-g, lahko uporabite tudi, da preklièete ¹tevilèni
argument, ali pa zaèetek ukaza, ki ga ne ¾elite izvesti.

>> Vtipkajte C-u 100, s èimer ste izbrali ¹tevilèni argument 100,
   zatem pa vtipkajte C-g. Vtipkajte zdaj C-f. Kazalèek se je
   premaknil le za en znak, ker ste ¹tevilèni argument vmes preklicali
   s C-g.

Tudi èe ste po nesreèi vtipkali <ESC>, se ga lahko znebite s C-g.


* ONEMOGOÈENI UKAZI
-------------------

Nekaj ukazov v Emacsu je namenoma ,,onemogoèenih``, da bi jih
zaèetniki ne izvedli po nesreèi.

Èe vtipkate tak onemogoèen ukaz, se bo na zaslonu pojavilo novo okno z
obvestilom, kateri ukaz ste sku¹ali izvesti, in vas vpra¹alo, èe ga
res ¾elite izvesti.

Èe v resnici ¾elite poskusiti ukaz, pritisnite preslednico kot odgovor
na vpra¹anje. Normalno verjetno ukaza ne ¾elite izvesti, zato na
vpra¹anje odgovorite z ,n`.

>> Vtipkajte C-x C-l (ki je onemogoèen ukaz),
   zatem na vpra¹anje odgovorite n.


* OKNA
------

Emacs lahko prika¾e veè oken in v vsakem svoje besedilo. Kasneje bomo
razlo¾ili, kako uporabljamo veè oken hkrati. Zaenkrat bomo povedali
le, kako se znebite dodatnih oken, ki jih lahko odpre vgrajena pomoè ali
pa izpis kak¹nega drugega programa. Preprosto je:

	C-x 1   Eno okno (torej, zaprimo vsa ostala).

To je CONTROL-x, ki mu sledi ¹tevka 1. Ukaz C-x 1 raztegne èez cel
zaslon okno, v katerem se nahaja kazalèek, ostala pa zapre.

>> Premaknite kazalèek do te vrstice in vtipkajte C-u 0 C-l
>> Vtipkajte CONTROL-h k CONTROL-f.
   Vidite, kako se je to okno skrèilo in odstopilo prostor oknu,
   ki pojasnjuje ukaz CONTROL-f?

>> Vtipkajte C-x 1 in spodnje okno se bo zaprlo.

Za razliko od ukazov, ki smo se jih nauèili do zdaj, je ta ukaz
sestavljen iz dveh znakov. Zaène se z znakom CONTROL-x. Cela vrsta
ukazov se zaène enako, in mnogi od njih zadevajo delo z datotekami,
delovnimi podroèji in podobnim. Vsem tem ukazom je skupno, da se
zaènejo s CONTROL-x, ki mu sledi ¹e en, dva ali trije znaki.


* VRIVANJE IN BRISANJE
----------------------

Èe ¾elite v obstojeèe besedilo vriniti novo, preprosto premaknite
kazalèek na ¾eljeno mesto in zaènite tipkati. Znake, ki jih lahko
vidite, na primer A, 7, * in podobno, razume Emacs kot del besedila in
jih takoj vrine. S pritiskom na Return (ali Enter) vrinete znak za
skok v novo vrstico.

Zadnji vtipkani znak lahko izbri¹ete s pritiskom na tipko <Delete>. Na
nekaterih tipkovnicah je oznaèena z <Del>. Ponekod (ne pa povsod!)
slu¾i za brisanje tipka <Backspace>.

Splo¹no <Delete> pobri¹e znak neposredno pred trenutnim polo¾ajem
kazalèka.

>> Vtipkajte zdaj nekaj znakov in jih zatem s tipko <Delete> pobri¹ite.
   Niè naj vas ne skrbi, èe se je ta vrstica spremenila. Izvirnika
   tega uèbenika ne boste pokvarili -- tole je samo va¹a osebna kopija.

Ko vrstica postane predolga za zaslon, se ,,nadaljuje`` v naslednji
vrstici na zaslonu. Obrnjena po¹evnica (znak ,\`) ali v grafiènih
okoljih zavita pu¹èica ob desnem robu oznaèuje vrstico, ki se
nadaljuje v naslednji zaslonski vrstici.

>> Zdaj zaènite tipkati besedilo, dokler ne dose¾ete desnega roba, in
   ¹e naprej. Opazili boste, da se pojavi znak za nadaljevanje.

>> S tipko <Delete> pobri¹ite toliko znakov, da vrstica ne sega
   veè èez ¹irino zaslona. Znak za nadaljevanje v naslednji
   vrstici je izginil.

Znak za novo vrstico lahko pobri¹emo enako kot vsak drug znak. S tem,
ko pobri¹emo znak za novo vrstico, zdru¾imo vrstici v eno samo.  Èe bo
nova vrstica predolga, da bi cela pri¹la na zaslon, bo razdeljena v
veè zaslonskih vrstic.

>> Premaknite kazalèek na zaèetek vrstice in pritisnite <Delete>. To
   zdru¾i vrstico s prej¹njo.

>> Pritisnite <Return>. S tem ste ponovno vrinili znak za skok v novo
   vrstico, ki ste ga malo prej zbrisali.

Spomnimo se, da lahko za veèino ukazov v Emacsu doloèimo, naj se
izvedejo veèkrat zaporedoma; to vkljuèuje tudi vnos teksta. Ponovitev
obièajnega znaka ga veèkrat vrine v besedilo.

>> Poskusite zdaj tole: da vnesete osem zvezdic, vtipkajte C-u 8 *

Zdaj ste se nauèili najpreprostej¹i naèin, da v Emacsu nekaj natipkate
in popravite. Bri¹ete lahko tudi besede ali vrstice. Tu je povzetek
ukazov za brisanje:

	<Delete>     pobri¹e znak tik pred kazalèkom (levo od
	             oznake za kazalèek)
	C-d   	     pobri¹e znak tik za kazalèkom (,pod` oznako
		     za kazalèek)

	M-<Delete>   pobri¹e besedo tik pred kazalèkom
	M-d	     pobri¹e besedo tik za kazalèkom

	C-k          zavr¾e besedilo desno od kazalèka do konca vrstice
	M-k          zavr¾e besedilo od polo¾aja kazalèka do konca stavka

Èrka ,d` je iz angle¹ke besede ,delete` (pobrisati), èrka ,k` pa iz
besede ,kill` (pobiti). Ste opazili, da <Delete> in C-d na eni, ter
M-<Delete> in M-d na drugi strani nadaljujeta paralelo, ki sta jo zaèela
C-f in M-f (<Delete> pravzaprav ni kontrolni znak, kar pa naj nas ne 
moti).  C-k in M-k sta v enakem sorodu s C-e in M-e: prvi deluje na 
vrstice, drugi na stavke.

Obstaja tudi splo¹en postopek za brisanje kateregakoli dela delovnega
podroèja. Kazalèek postavimo na en konec podroèja, ki ga ¾elimo
izbrisati, in pritisnemo C-@ ali C-SPC (SPC je
preslednica). Katerikoli od obeh ukazov deluje. Premaknite kazalèek na
drug konec podroèja, ki ga ¾elite izbrisati, in pritisnite C-w. S tem
ste zavrgli vse besedilo med obema mejama.

>> Premaknite kazalèek na èrko O, s katero se zaèenja prej¹nji
   odstavek.
>> Vtipkajte C-SPC. Emacs prika¾e sporoèilo "Mark set" (slov. Oznaka
   postavljena) na dnu ekrana.
>> Premaknite kazalèek na èrko V v "postavimo" v drugi vrstici istega
   odstavka.
>> Vtipkajte C-w. S tem zavr¾emo vse besedilo zaèen¹i z O in vse do
   èrke V.

Razlika med tem, èe zavr¾ete cel odstavek besedila (angl. ,,kill``,
pobiti) ali pa èe pobri¹ete znak (angl. ,,delete``), je ta, da lahko
prvega vrnete nazaj z ukazom C-y, drugega pa ne. Na splo¹no ukazi, ki
lahko povzroèijo veliko ¹kode (pobri¹ejo veliko besedila), shranijo
pobrisano besedilo; tisti, ki pobri¹ejo samo posamezni znak, ali samo
prazne vrstice in presledke, pa ne.

>> Postavite kazalèek na zaèetek neprazne vrstice. Pritisnite C-k, da
   pobri¹ete vsebino vrstice.
>> ©e enkrat pritisnite C-k. To pobri¹e ¹e znak za novo vrstico.

Ste opazili, da prvi C-k pobri¹e vsebino vrstice, naslednji C-k pa ¹e
vrstici samo, s èimer se vse besedilo pod biv¹o vrstico premakne za
eno vrstico navzgor? Ukaz C-k obravnava ¹tevilèni argument malo
drugaèe: pobri¹e toliko in toliko vrstic z vsebinami vred. To ni zgolj
ponovitev. C-u 2 C-k pobri¹e dve polni vrstici besedila, kar je nekaj
drugega, kot èe dvakrat vtipkate C-k.

Besedilo, ki ste ga prej pobili, lahko povrnete (angl.  ,,yank`` --
potegniti). Predstavljajte si, kot da potegnete nazaj nekaj, kar vam
je nekdo odnesel. Pobito besedilo lahko potegnete nazaj na isti ali pa
na kak¹en drug kraj v besedilu, ali pa celo v kaki drugi
datoteki. Isto besedilo lahko veèkrat potegnete nazaj, tako da je v
delovnem podroèju poveèterjeno.

Ukaz za vraèanje pobitega besedila je C-y.

>> Poskusite z ukazom C-y povrniti pobrisano besedilo.

Èe ste uporabili veè zaporednih ukazov C-k, je vse pobrisano besedilo
shranjeno skupaj, in en sam C-y bo vrnil vse tako pobrisane vrstice.

>> Poskusite, nekajkrat vtipkajte C-k.

Zdaj pa vrnimo pobrisano besedilo:

>> Vtipkajte C-y. Zdaj pa premaknite kazalèek za nekaj vrstic navzdol
   in ¹e enkrat vtipkajte C-y. Vidite zdaj, kako se kopira dele
   besedila?

Kaj pa, èe ste pobrisali nekaj besedila, ki bi ga radi vrnili, vendar
ste za iskanim odlomkom pobrisali ¹e nekaj? C-y vrne samo nazadnje
pobrisan odlomek. Vendar tudi prej¹nje besedilo ni izgubljeno. Do
njega lahko pridete z ukazom M-y. Ko ste vrnili nazadnje zbrisano
besedilo s C-y, pritisnite M-y, ki ga zamenja s predzanje pobrisanim
besedilom. Vsak naslednji M-y prika¾e ¹e eno prej. Ko ste konèno
pri¹li do iskanega besedila, ni treba napraviti niè posebnega, da bi
ga obdr¾ali. Preprosto nadaljujte z urejanjem, in vrnjeno besedilo bo
ostalo, kamor ste ga odlo¾ili.

Èe pritisnete M-y dovolj velikokrat, se boste vrnili na zaèete, torej
spet na zadnje pobrisano besedilo.

>> Pobri¹ite vrstico, premaknite se nekam drugam, in pobri¹ite ¹e
   eno vrstico.
   Z ukazom C-y dobite nazaj to drugo vrstico.
   Z ukazom M-y pa jo zamenjate s prvo vrstico.
   Ponovite ukaz M-y ¹e nekajkrat in si oglejte, kaj dobite na
   zaslon. Ponavljajte ga, dokler se ne prika¾e ponovno nazadnje
   pobrisana vrstica, in ¹e naprej. Èe ¾elite, lahko tudi ukazu
   M-y podate pozitivno ali negativno ¹tevilo ponovitev.


* PREKLIC UKAZA (UNDO)
----------------------

Èe ste besedilo spremenili, a ste se kasneje premislili, lahko
besedilo vrnete v prvotno stanje z ukazom Undo, C-x u. Normalno vrne
C-x u zadnjo spremembo besedila; èe ukaz ponovimo, preklièemo ¹e
predzadnjo spremembo, in vsaka nadaljnja ponovitev se¾e ¹e eno
spremembo globlje v zgodovino.

Emacs hrani bolj ali manj celotno zgodovino na¹ih ukazov, z dvema
izjemama: ukazov, ki niso napravili nobene spremembe v besedilu
(npr. premik kazalèka), ne shranjuje, in zaporedje do 20 vrinjenih
znakov shrani kot en sam ukaz. Slednje prihrani nekaj ukazov C-x u, ki
bi jih morali vtipkati.

>> Pobri¹ite to vrstico z ukazom C-k, potem jo priklièite nazaj s C-x u.

C-_ je alternativni ukaz za preklic zadnjega ukaza.  Deluje enako kot
s C-x u, ga je pa la¾je odtipkati, èe morate ukaz ponoviti veèkrat
zaporedoma. Te¾ava z ukazom C-_ je, da na nekaterih tipkovnicah ni
povsem oèitno, kako ga vtipkati, zato je podvojen ¹e kot C-x u. Na
nekaterih terminalih moramo na primer vtipkati /, medtem ko dr¾imo
pritisnjeno tipko CONTROL.

Èe podamo ukazu C-_ ali C-x u numerièni argument, je to enako, kot èe
bi ukaz roèno ponovili tolikokrat, kot pravi argument.

Ukaz za brisanje besedila lahko preklièete in besedilo povrnete,
enako, kot èe bi besedilo pobili. Razlika med brisanjem in pobijanjem
besedila je le ta, da le slednje lahko potegnete nazaj z ukazom
C-y. Preklic ukaza pa velja za eno in drugo.


* DATOTEKE
----------

Da bi bile spremembe v besedilu trajne, morate besedilo shraniti v
datoteko. V nasprotnem primeru jih boste za vedno izgubili tisti hip,
ko boste zapustili Emacs. Besedilo postavimo v datoteko tako, da
na disku ,,poi¹èemo`` (angl. find) datoteko, preden zaènemo tipkati 
(pravimo tudi, da ,,obi¹èemo`` datoteko).

Poiskati datoteko pomeni, da v Emacsu vidimo vsebino datoteke. To je
bolj ali manj tako, kot da z Emacsom urejamo datoteko samo. Vendar pa
spremembe ne postanejo trajne, dokler datoteke ne shranimo
(angl. save) na disk. Tako imamo mo¾nost, da se izognemo temu, da bi
nam na pol spremenjene datoteke le¾ale po disku, kadar tega ne
¾elimo. Ker pa Emacs ohrani izvorno datoteko pod spremenjenim imenom,
lahko prvotno datoteko priklièemo nazaj celo ¹e potem, ko smo datoteko
¾e shranili na disk.

V predzadnji vrstici na dnu zaslona vidite vrstico, ki se zaène in
konèa z vezaji, in vsebuje niz znakov ,,--:-- TUTORIAL``. Ta del
zaslona navadno vsebuje ime datoteke, ki smo jo obiskali. Zdajle je to
,,TUTORIAL``, va¹a delovna kopija uèbenika Emacsa.  Ko boste poiskali
kak¹no drugo datoteko, bo na tem mestu pisalo njeno ime.

Posebnost ukaza za iskanje datoteke je, da moramo povedati, katero
datoteko i¹èemo. Pravimo, da ukaz ,,prebere argument s terminala`` (v
tem primeru je argument ime datoteke).  Ko vtipkate ukaz

	C-x C-f   (poi¹èi datoteko)

vas Emacs povpra¹a po imenu datoteke. Kar vtipkate, se sproti vidi v
vrstici na dnu zaslona. Temu delovnemu podroèju pravimo pogovorni
vmesnik (minibuffer), kadar se uporablja za tovrstni vnos. Znotraj
pogovornega vmesnika lahko uporabljate obièajne ukaze za urejanje, èe
ste se na primer pri tipkanju zmotili.

Sredi tipkanja imena datoteke (ali katerega koli drugega opravila v
pogovornem vmesniku) lahko ukaz preklièete s C-g.

>> Vtipkajte C-x C-f, zatem pa ¹e C-g. Zadnji ukaz od treh je
   zaprl pogovorni vmesnik in tudi preklical ukaz C-x C-f, ki je
   uporabljal pogovorni vmesnik. Konec z iskanjem datoteke.

Ko ste dokonèali ime, ga vnesete s pritiskom na <Return>. S tem se
po¾ene ukaz C-x C-f in poi¹èe iskano datoteko. Pogovorni vmesnik
izgine, ko je ukaz izveden.

Trenutek kasneje se vsebina datoteke pojavi na zaslonu. Zdaj lahko
dopolnjujete, urejate ali kako drugaèe spreminjate vsebino. Ko ¾elite,
da ostanejo spremembe trajne, izvedete ukaz:

	C-x C-s   (shrani datoteko)

Besedilo se s tem shrani iz pomnilnika raèunalnika na datoteko na
disk. Ko prviè izvedete ta ukaz, se izvorna datoteka preimenuje, tako
da ni izgubljena. Najdete jo pod novim imenom, ki se od starega
razlikuje po tem, da ima na koncu pripet znak ,,~``.

Ko je Emacs shranil datoteko, izpi¹e njeno ime. Shranjujte raje
pogosteje kot ne, da v primeru, èe gre z raèunalnikom kaj narobe, ne
izgubite veliko.

>> Vtipkajte C-x C-s, s èimer boste shranili svojo kopijo tega
   uèbenika. Emacs bo v vrstici na dnu zaslona izpisal ,,Wrote
   ...TUTORIAL``.

Opozorilo: na nekaterih sistemih bo ukaz C-x C-s zamrznil zaslon, in
tako ne boste videli, da Emacs ¹e kaj izpi¹e. To je znak, da je
operacijski sistem prestregel znak C-s in ga interpretiral kot znak za
prekinitev toka podatkov, namesto da bi ga posredoval Emacsu. Zaslon
,,odmrznete`` z ukazom C-q. Èe je va¹ sistem eden takih, si za nasvet,
kako re¹iti to nev¹eènost, oglejte razdelek ,,Spontaneous Entry to
Incremental Search`` v priroèniku za Emacs.

Poi¹èete lahko lahko ¾e obstojeèo datoteko, da si jo ogledate ali
popravite, ali pa tudi datoteko, ki ¹e ne obstaja. To je naèin, kako z
Emacsom ustvarimo novo datoteko: poi¹èite datoteko z izbranim imenom,
ki bo sprva prazna, in zaènite pisati. Ko jo boste prviè shranili, bo
Emacs ustvaril datoteko z vne¹enim besedilom. Od tod dalje delate na
¾e obstojeèi datoteki.


* DELOVNA PODROÈJA
------------------

Tudi èe ste z ukazom C-x C-f poiskali in odprli drugo datoteko, prva
ostane v Emacsu. Nanjo se vrnete tako, da jo ¹e enkrat ,,poi¹èete`` z
ukazom C-x C-f. Tako imate lahko v Emacsu hkrati kar precej datotek.

>> Ustvarite datoteko z imenom ,,foo`` tako, da vtipkate C-x C-f
   foo <Return>. Natipkajte nekaj besedila, ga po potrebi popravite, in
   shranite v datoteko ,,foo`` z ukazom C-x C-s. Ko ste konèali, se
   vrnite v uèbenik z ukazom C-x C-f TUTORIAL <Return>.

Emacs hrani besedilo vsake datoteke v takoimenovanem ,,delovnem
podroèju`` (angl. buffer). Ko poi¹èemo datoteko, Emacs ustvari zanjo
novo delovno podroèje. Vsa obstojeèa delovna podroèja v Emacsu vidimo
z ukazom:

	C-x C-b   Seznam delovnih podroèij.

>> Poskusite C-x C-b zdaj.

Vidite, da ima vsako delovno podroèje svoje ime, pri nekaterih pa pi¹e
tudi ime datoteke, katere vsebina se hrani v njem. Druga delovna
podroèja pa ne pripadajo nobeni datoteki. Podroèje ,,*Buffer List*``,
na primer, je ¾e eno takih. To delovno podroèje smo ustvarili
ravnokar, ko smo pognali ukaz C-x C-b. VSAKO besedilo, ki ga vidite v
katerem od Emacsovih oken, je vedno del kak¹nega delovnega podroèja.

>> Z ukazom C-x 1 se znebite seznama delovnih podroèij.

Èe ste spreminjali besedilo ene datoteke, potem pa poiskali drugo, to
ne shrani spremeb v prvo datoteko. Te ostanejo znotraj Emacsa, na
delovnem podroèju, ki pripada prvi datoteki. Ustvarjenje ali
spreminjanje delovnega podroèja druge datoteke nima nobenega vpliva na
podroèje prve. To je zelo uporabno, pomeni pa tudi, da potrebujemo
udobno pot, da shranimo delovno podroèje prve datoteke. Nerodno bi
bilo preklapljanje na prvo podroèje s C-x C-f, da bi shranili s C-x
C-s. Namesto tega imamo:

	C-x s     Shrani nekatera delovna podroèja

Ukaz C-x poi¹èe delovna podroèja, katerih vsebina je bila spremenjena,
odkar je bila zadnjiè shranjena na datoteko. Za vsako tako delovno
podroèje C-x s vpra¹a, èe ga ¾elite shraniti.


* RAZ©IRJEN NABOR UKAZOV
------------------------

©e mnogo, mnogo je ukazov Emacsa, ki bi zaslu¾ili, da jih obesimo na 
razne kontrolne in meta znake. Emacs se temu izogne z ukazom X (iz angl.
eXtend - raz¹iriti), ki uvede ukaz iz raz¹irjenega nabora. Dveh vrst je:

	C-x	Znakovna raz¹iritev (angl. Character eXtend).
		Sledi mu en sam znak.
	M-x	Raz¹iritev s poimenovanim ukazom. Sledi mu dolgo ime
		ukaza.

Tudi ti ukazi so na splo¹no uporabni, ne uporabljamo pa jih tako
pogosto kot tiste, ki ste se jih ¾e nauèili. Dva ukaza iz raz¹irjenega
nabora ¾e poznamo: C-x C-f, s katerim poi¹èemo datoteko, in C-x C-s, s
katerim datoteko shranimo. ©e en primer je ukaz, s katerim Emacsu
povemo, da ¾elimo konèati z delom iz iziti iz Emacsa. Ta ukaz je C-x
C-c (ne skrbite: preden konèa, Emacs ponudi, da shrani vse spremenjene
datoteke).

Z ukazom C-z Emacs zapustimo samo *zaèasno*, tako da lahko ob vrnitvi
nadaljujemo z delom, kjer smo ostali.

Na sistemih, ki to dopu¹èajo, ukaz C-z izide iz Emacsa v ukazno
lupino, a ga ne konèa - èe uporabljate ukazno lupino C, se lahko
vrnete z ukazom ,fg` ali splo¹neje z ukazom ,,%emacs``.

Drugod ukaz C-z po¾ene sekundarno ukazno lupino, tako da lahko
po¾enete kak¹en drug program in se kasneje vrnete v Emacs. V tem
primeru pravzaprav Emacsa ne zapustimo. Ukaz ,,exit`` v ukazni lupini
je navadno naèin, da zapremo sekundarno lupino in se vrnemo v Emacs.

Ukaz C-x C-c uporabimo, èe se nameravamo odjaviti s sistema. To je
tudi pravilen naèin za izhod iz Emacsa, èe je tega pognal program za
delo s po¹to ali kak drug program, saj ta verjetno ne ve, kaj
napraviti z zaèasno prekinjenim Emacsom. V vseh ostalih primerih pa,
èe se ne nameravate odjaviti s sistema, uporabite C-z, in se vrnite v
Emacs, ko bi radi spet urejali besedilo.

Ukazov C-x je veliko. Zaenkrat smo spoznali naslednje:

	C-x C-f		Poi¹èi datoteko.
	C-x C-s		Shrani datoteko.
	C-x C-b		Prika¾i seznam delovnih podroèij.
	C-x C-c		Konèaj Emacs.
	C-x 1		Zapri vsa okna razen enega.
	C-x u		Preklic zadnjega ukaza.

Poimenovani raz¹irjeni ukazi so ukazi, ki se uporabljajo ¹e bolj
poredko, ali pa se uporabljajo samo v nekaterih naèinih dela.  Eden
takih je na primer ukaz replace-string, ki po vsem besedilu zamenja en
niz znakov z drugim. Ko vtipkate M-x, se to izpi¹e v pogovornem
vmesniku na dnu zaslona, Emacs pa èaka, da vtipkate ime ukaza, ki ga
¾elite priklicati; v tem primeru je to ,,replace-string``. Vtipkajte
samo ,,repl s<TAB>`` in Emacs bo dopolnil ime (<TAB> je tabulatorska
tipka; navadno jo najdemo nad tipko Caps Lock ali Shift na levi strani
tipkovnice). Ukaz vnesete s pritiskom na <Return>.

Ukaz replace-string potrebuje dva argumenta -- niz, ki ga ¾elite
zamenjati, in niz, s katerim bi radi zamenjali prvega. Vsakega posebej
vnesete in zakljuèite s pritiskom na tipko Return.

>> Premaknite kazalèek na prazno vrstico dve vrstici pod to, zatem 
   vtipkajte M-x repl s<Return>zamenjala<Return>spremenila<Return>.

   Opazite, kako se je ta vrstica zamenjala? Vse besede
   z-a-m-e-n-j-a-l-a od tod do konca besedila ste nadomestili z besedo
   ,,spremenila``.


* AVTOMATIÈNO SHRANJEVANJE
--------------------------

Spremembe v datoteki, ki jih ¹e niste shranili na disk, so izgubljene,
èe medtem denimo zmanjka elektrike. Da bi vas zavaroval pred tem,
Emacs periodièno avtomatièno shrani vse datoteke, ki jih
urejate. Avtomatièno shranjena datoteka se od izvorne razlikuje po
znaku ,#` na zaèetku in koncu imena: èe se je va¹a datoteka imenovala
,,hello.c``, se avtomatièno shranjena datoteka imenuje
,,#hello.c#``. Ko normalno shranite datoteko, avtomatièno shranjena
datoteka ni veè potrebna, in Emacs jo pobri¹e.

Èe res pride do izgube podatkov v pomnilniku, lahko povrnete avtomatièno
shranjeno besedilo tako, da normalno poi¹èete datoteko (pravo ime
datoteke, ne ime avtomatièno shranjene datoteke), zatem pa vtipkate M-x
recover file<Return>. Ko vas vpra¹a za potrditev, vtipkajte yes<Return>
za nadaljevanje in povrnitev avtomatièno shranjenenih podatkov. 


* ODZIVNO PODROÈJE
------------------

Kadar Emacs opazi, da poèasi vtipkavate ukaz, odpre v zadnji vrstici
na dnu zaslona odzivno podroèje in v njem sproti prikazuje natipkano.


* STATUSNA VRSTICA
------------------

Vrstica nad odzivnim podroèjem je statusna vrstica. Ta ka¾e verjetno
nekaj podobnega kot:

--:** TUTORIAL          (Fundamental)--L670--58%----------------------

V njej so izpisani pomembni podatki o stanju Emacsa in besedilu, ki ga
urejate.

Zdaj ¾e veste, kaj pomeni ime datoteke -- to je datoteka, ki ste jo
poiskali. Oznaka --NN%-- pomeni, da je nad vrhom zaslona ¹e NN
odstotkov celotne datoteke. Èe je zaèetek datoteke na zaslonu, bo
namesto --00%-- pisalo --Top--. Podobno bo pisalo --Bot--, èe je
zadnja vrstica datoteke na zaslonu. Èe je datoteka, ki jo ogledujete,
tako kratka, da gre vsa na en zaslon, pa bo pisalo --All--.

Èrka L in ¹tevilke za njo ka¾ejo polo¾aj ¹e drugaèe, kot zaporedno
¹tevilko vrstice, v kateri je kazalèek.

Zvezdice na zaèetku vrstice pomenijo, da ste datoteko ¾e spreminjali.
Tik po tem, ko ste odprli ali shranili datoteko, ni nobenih zvezdic,
so samo èrtice.

Del statusne vrstice znotraj oklepajev vam pove, v kak¹nem naèinu dela
Emacs. Privzeti naèin je osnovni naèin (Fundamental), v katerem ste
sedaj. Fundamental je eden od glavnih naèinov (angl. major
mode). Emacs pozna veliko razliènih glavnih naèinov. Nekateri od njih
so namenjeni pisanju programov, kot na primer Lisp, ali pisanju
besedil, kot npr. Text. Naenkrat je lahko aktiven le en glavni naèin,
njegovo ime pa je vedno izpisano v statusni vrstici, kjer zdaj pi¹e
Fundamental.

Glavni naèini lahko spremenijo pomen nekaterim ukazom. Obstajajo,
denimo, ukazi za pisanje komentarjev v programu, in ker ima vsak
programski jezik svoje predstave o tem, kako mora komentar izgledati,
mora vsak glavni naèin vnesti komentarje drugaèe. Ker je vsak glavni
naèin ime raz¹irjenega ukaza, lahko tako tudi izbiramo glavni
naèin. Na primer, M-x fundamental-mode vas postavi v naèin
Fundamental.

Èe nameravate popravljati slovensko (ali angle¹ko) besedilo, kot je na
primer tole, boste verjetno izbrali tekstovni naèin (Text).
>> Vtipkajte M-x text mode<Return>.

Brez skrbi, noben od ukazov Emacsa, ki ste se jih nauèili, se s tem ne
spremeni kaj dosti. Lahko pa opazite, da Emacs zdaj jemlje opu¹èaje za
dele besed, ko se premikate z M-f ali M-b. V osnovnem naèinu jih je
obravnaval kot meje med besedami.

Glavni naèini navadno poèenjajo majhne spremembe, kot je ta: veèina
ukazov ,,opravi isti posel``, vendar pa to poènejo na razlièen naèin.

Dokumentacijo o trenutno aktivnem glavnem naèinu dobite z ukazom C-h m.

>> Uporabite C-u C-v enkrat ali veèkrat, toliko, da bo ta vrstica blizu
   vrha zaslona.
>> Vtipkajte C-h m, da vidite, v èem se tekstovni naèin (Text) razlikuje
   od osnovnega (Fundamental).
>> Vtipkajte C-x 1, da umaknete dokumentacijo z zaslona.

Glavnim naèinom pravimo glavni naèini zato, ker obstajajo tudi
podnaèini (angl. minor modes). Podnaèini ne nadome¹èajo glavnih
naèinom, ampak le spreminjajo njihovo obna¹anje. Podnaèine lahko
aktiviramo ali deaktiviramo neodvisno od glavnega naèina in neodvisno
od ostalih podnaèinov. Tako lahko ne uporabljate nobenega podnaèina,
en podnaèin, ali kombinacijo veèih podnaèinov.

Podnaèin, ki je zelo uporaben, posebno za pisanje besedil, je Auto
Fill. Ko je vklopljen, Emacs med pisanjem avtomatièno deli vrstice na
presledkih med besedami, tako da vrstice niso predolge.

Vklopite ga lahko z ukazom M-x auto fill mode<Return>. Ko je
vklopljen, ga lahko izklopite z istim ukazom, M-x
auto fill mode<Return>. Z istim ukazom torej preklapljamo
(angl. toggle) med vklopljenim in izklopljenim stanjem.

>> Vtipkajte zdaj M-x auto fill mode<Return>. Potem zaènite tipkati
   "asdf asdkl sdjf sdjkf"... dokler ne opazite, da je Emacs razbil
   vrstico na dve.  Med tipkanjem mora biti dovolj presledkov, saj
   Auto Fill prelamlja vrstice samo na presledkih.

©irina besedila je navadno postavljena na 70 znakov, kar pa lahko
spremenite z ukazom C-x f. Novo ¹irino morate podati kot ¹tevilèni
argument.

>> Vtipkajte C-x f in argument 20. (C-u 2 0 C-x f). Zatem vtipkajte
   nekaj besedila in poglejte, èe bo Emacs res delil vrstice pri 20
   znakih. Potem z ukazom C-x f postavite mejo nazaj na 70.

Auto Fill deluje le, kadar pi¹ete novo besedilo, ne pa,
kadar popravljate ¾e napisan odstavek.
Tak odstavek lahko poravnate tako, da kazalèek premaknete nekam
znotraj odstavka in uka¾ete M-q (META-q).

>> Premaknite kazalèek v prej¹nji odstavek in izvedite M-q.


* ISKANJE
---------

Emacs lahko v besedilu poi¹èe niz znakov (zaporedje znakov ali besed),
naprej ali nazaj po besedilu. Iskanje spada v skupino ukazov za
premikanje kazalèka, saj premakne kazalèek na kraj v besedilu, kjer je
na¹el iskani niz.

Iskanje v Emacsu je morda nekoliko drugaèno od tistega, ki ste ga
navajeni, in sicer je ,,inkrementalno``. To pomeni, da se iskanje
odvija hkrati s tem, ko tipkate iskani niz.

Ukaza za iskanje sta C-s za iskanje naprej po datoteki in C-r za
iskanje nazaj po datoteki. POÈAKAJTE! Ne preizku¹ajte jih ¹e ta hip!

Ko boste natipkali C-s, boste opazili niz ,,I-search`` kot pozivnik
v pogovornem vmesniku. To vam pove, da je Emacs v inkrementalnem iskanju
in vas èaka, da zaènete tipkati, kar i¹èete. <Return> zakljuèi iskanje.

>> Pritisnite zdaj C-s. POÈASI, èrko za èrko, vtipkajte besedo
   ,,kazalèek``. Za vsako vtipkano èrko se ustavite in si oglejte, kaj
   se je zgodilo s kazalèkom.
>> ©e enkrat pritisnite C-s, da poi¹èete naslednji ,,kazalèek``.
>> ©estkrat pritisnite <Delete> in opazujte, kako se premika kazalèek.
>> Konèajte iskanje s tipko <Return>.

Ste videli, kaj se je zgodilo? Emacs pri inkrementalnem iskanju sku¹a
poiskati niz, ki ste ga natipkali do tistega hipa. Da poi¹èete
naslednje mesto, kjer se pojavi ,,kazalèek``, samo ¹e enkrat
pritisnete C-s. Èe takega mesta ni, Emacs èivkne in vam sporoèi, da
iskanje ni uspelo. Tudi C-g prekine iskanje.

OPOZORILO: Na nekaterih sistemih bo s pritiskom na C-s ekran
zmrznil. To je znak, da je operacijski sistem prestregel znak C-s in
ga interpretiral kot znak za prekinitev toka podatkov, namesto da bi
ga posredoval programu Emacs. Ekran ,,odtajate`` s pritiskom na
C-q. Potem si oglejte razdelek ,,Spontaneous Entry to Incremental
Search`` v priroèniku za nasvet, kako se spopasti s to nev¹eènostjo.

Èe sredi inkrementalnega iskanja pritisnete <Delete>, boste opazili,
da to pobri¹e zadnji znak v iskanem nizu, kazalèek pa se premakne
nazaj na mesto v besedilu, kjer je na¹el kraj¹i niz. Na primer,
predpostavimo, da ste do zdaj natipkali ,,ka`` in je kazalèek na
mestu, kjer se prviè pojavi ,,ka``. Èe zdaj pritisnete <Delete>, boste
s tem v pogovornem vmesniku izbrisali ,a`, hkrati pa se bo kazalèek
postavil na mesto, kjer je prviè na¹el ,k`, preden ste natipkali ¹e
,a`.

Èe sredi iskanja vtipkate katerikoli kontrolni znaki ali metaznak
(razen tistih, ki imajo poseben pomen pri iskanju, to sta C-s in C-r),
se iskanje prekine.

C-s zaène iskati na mestu v datoteki, kjer trenutno stoji kazalèek, in
i¹èe do konca datoteke. Èe bi radi iskali proti zaèetku datoteke,
namesto C-s vtipkamo C-r.  Vse, kar smo povedali o ukazu C-s, velja
tudi za C-r, le smer iskanja je obrnjena.


* VEÈ OKEN NA ZASLONU
---------------------

Ena simpatiènih lastnosti Emacsa je, da zna hkrati prikazati veè oken
na ekranu, tudi èe ne delamo v grafiènem naèinu.

>> Premaknite kazalèek v to vrstico in vtipkajte C-u 0 C-l (zadnji
   znak je CONTROL-L, ne CONTROL-1)
>> Zdaj vtipkajte C-x 2, da razdelite zaslon na dve okni.
   V obeh oknih imate odprt ta priroènik. Kazalèek je ostal v zgornjem
   oknu.
>> Pritisnite C-M-v za listanje v spodnjem oknu.
   (Èe nimate tipke META, tipkajte ESC C-v).
>> Vtipkajte C-x o (o kot ,,other``, drugi), da preselite kazalèek v
   spodnje okno.
>> S C-v in M-v se v spodnjem oknu premikate po vsebini datoteke.
   Zgornje okno ¹e vedno ka¾e ta navodila.
>> Ponovni C-x o vas vrne v zgornje okno. Kazalèek se je vrnil na
   mesto, kjer je bil, preden smo skoèili v spodnje okno.

Z ukazom C-x o lahko preklapljamo med okni. Vsako okno si zapomni, kje
v oknu je ostal kazalèek, samo trenutno aktivno okno pa kazalèek tudi
v resnici prika¾e. Vsi obièajni ukazi za urejanje, ki smo se jih
nauèili, veljajo za aktivno okno.

Ukaz C-M-v je zelo uporaben, kadar urejamo besedilo v enem oknu,
drugega pa uporabljamo samo za pomoè. Kazalèek ostaja ves èas v oknu,
v katerem urejamo, po vsebini spodnjega okna pa se vseeno lahko
premikamo, ne da bi morali venomer skakati iz enega okna v drugega.

C-M-v je primer znaka CONTROL-META. Èe imate v resnici tipko META (na
PC navadno levi Alt), lahko vtipkate C-M-v tako, da dr¾ite pritisnjeni
tako CONTROL kot META, medtem ko vtipkate v. Ni pomembno, katero od
tipk, CONTROL ali META, pritisnete prvo, saj obe delujeta ¹ele, ko
pritisnete znak, ki sledi (v zgornjem primeru ,v`).

Nasprotno pa je vrstni red pritiskanja pomemben, èe nimate tipke META
in namesto nje uporabljate ESC. V tem primeru morate najprej
pritisniti ESC, potem pa Control-v. Obratna kombinacija, CONTROL-ESC v
ne deluje. To je zato, ker je ESC znak sam po sebi, ne pa modifikator,
kot sta CONTROL in META.

>> V zgornjem oknu vtipkajte C-x 1, da se znebite spodnjega okna.

(Èe bi vtipkali C-x 1 v spodnjem oknu, bi se znebili
zgornjega. Razmi¹ljajte o tem ukazu kot ,,Obdr¾i samo eno okno, in
sicer tisto, v katerem sem zdaj.``)

Seveda ni nujno, da obe okni ka¾eta isto delovno podroèje. Èe v enem
oknu izvedete C-x C-f in poi¹èete novo datoteko, se vsebina drugega
okna ne spremeni. V vsakem oknu lahko neodvisno obdelujete drugo
datoteko. 

Pa ¹e ena pot, kako v dveh oknih prika¾ete dve razlièni datoteki:

>> Vtipkajte C-x 4 C-f, in na pozivnik vtipkajte ime ene va¹ih
   datotek. Konèajte z <Return>. Odpre se ¹e eno okno in izbrana
   datoteka se pojavi v drugem oknu. Tudi kazalèek se preseli v drugo
   okno. 

>> Vtipkajte C-x o, da se vrnete nazaj v zgornje okno, in C-x 1, da
   zaprete spodnje okno.


* REKURZIVNI NIVOJI UREJANJA
----------------------------

Vèasih boste pri¹li v nekaj, èemur se pravi ,,rekurzivni nivo
urejanja``. To se vidi po tem, da v statusni vrstici oglati oklepaji
oklepajo ime glavnega naèina. V osnovnem naèinu bi, na primer, videli
[(Fundamental)] namesto (Fundamental).

Iz rekurzivnega nivoja urejanja se re¹ite, èe vtipkate ESC ESC ESC. To
zaporedje je vsenamenski ukaz ,,pojdi ven``. Uporabite ga lahko tudi
za ukinjanje odveènih oken, ali vrnitev iz pogovornega vmesnika.

>> Pritisnite M-x, da odprete pogovorni vmesnik, zatem pa vtipkajte
   ESC ESC ESC, da pridete ven iz njega.

Z ukazom C-g ne morete iz rekurzivnega nivoja urejanja, ker C-g
preklièe ukaze ali argumente ZNOTRAJ rekurzivnega nivoja.


* DODATNA POMOÈ
---------------

V tem uvodu smo posku¹ali zbrati dovolj informacij, da lahko zaènete
Emacs uporabljati. Emacs ponuja toliko, da bi bilo nemogoèe vse to
zbrati tukaj. Verjetno pa bi se vseeno radi nauèili kaj o ¹tevilnih
koristnih mo¾nostih, ki jih ¹e ne poznate. Emacs ima ¾e vgrajene
veliko dokumentacije, do katere lahko pridete s pritiskom na CONTROL-h
(h kot ,,help``, pomoè).

Za pomoè pritisnete C-h, potem pa vtipkate znak, ki pove, kak¹no pomoè
¾elite. Èe ste poplnoma izgubljeni, vtipkajte C-h ? in Emacs vam bo
povedal, kak¹na pomoè je sploh na voljo. Èe ste vtipkali C-h, pa ste
si premislili, lahko ukaz preklièete s C-g.

(Na nekaterih sistemih se znak C-h preslika v kaj drugega. To ni
dobro, in v takem primeru se prito¾ite sistemskemu vzdr¾evalcu. Medtem
pa, èe C-h ne prika¾e sporoèila o pomoèi na dnu zaslona, namesto tega
poskusite pritisniti tipko F1 ali pa vtipkajte M-x help <Return>.)

Najosnovnej¹i tip pomoèi prika¾e C-h c. Pritisnite C-h, tipko c, zatem
pa ukazni znak ali zaporedje ukaznih znakov, in Emacs bo izpisal
kratek opis ukaza.

>> Vtipkajte C-h c C-p.
   Izpi¹e se nekaj takega kot

	C-p runs the command previous-line

Ukaz je izpisal ime funkcije, ki izvede ukaz. Imena funkcij
uporabljamo, kadar pi¹emo prilagoditve in raz¹iritve Emacsa. Ker pa so
navadno imena funkcij izbrana tako, da kaj povedo o tem, kaj funkcija
poène, bo verjetno to tudi dovolj za kratko osve¾itev, èe ste se z
ukazom ¾e kdaj sreèali.

Ukazu C-h lahko sledi tudi zaporedje znakov, kot na primer C-x C-s,
ali, èe nimate tipke META, <Esc>v.

Za veè informacij o ukazu vtipkajte C-h k namesto C-h c.

>> Vtipkajte C-h k C-p.

To odpre novo okno in v njem prika¾e dokumentacijo o funkciji, obenem
z njenim imenom. Ko ste opravili, vtipkajte C-x 1, da se znebite okna
z pomoèjo. Tega seveda ni potrebno napraviti takoj, ampak lahko
urejate, medtem ko imate odprto okno s pomoèjo, in ga zaprete, ko ste
konèali. 

Sledi ¹e nekaj uporabnih mo¾nosti, ki jih ponuja pomoè:

   C-h f	Opi¹i funkcijo. Kot argument morate podati ime
		funkcije. 

>> Poskusite C-h f previous-line<Return>.
   To izpi¹e vse podatke, ki jih ima Emacs o funkciji, ki izvede ukaz C-p.

Podoben ukaz C-h v izpi¹e dokumentacijo za spremenljivke, s katerimi
lahko nastavite obna¹anje Emacsa. Ob pozivniku morate vpisati ime
spremenljivke.

   C-h a	 Apropos. Vtipkajte kljuèno besedo in Emacs bo izpisal
		 vse ukaze, ki vsebujejo to kljuèno besedo. Vse te
		 ukaze lahko priklièete z META-x. Pri nekaterih ukazih
		 bo Apropos izpisal tudi eno ali dvoznakovno
		 zaporedje, s katerim dose¾ete isti uèinek.

>> Vtipkajte C-h a file<Return>.

To odpre novo okno, v katerem so vsa dolga imena ukazov, ki vsebujejo
,,file`` v imenu. Izvedete jih lahko z M-x. Pri nekaterih se izpi¹e
tudi kratek ukaz, npr. C-x C-f ali C-x C-w pri ukazih find-file in
write-file. 

>> Pritisnite C-M-v, da se sprehajate po oknu s pomoèjo. Poskusite
   nekajkrat. 

>> Vtipkajte C-x 1, da zaprete okno s pomoèjo.

   C-h i         Priroèniki z navodili za uporabo (tkim. datoteke 
		 "info"). Ta ukaz vas prestavi v posebno delovno
		 podroèje, imenovano "info". V njem lahko prebirate
		 priroènike za programe, ki so name¹èeni v sistemu. Z
		 ukazom m emacs<Return> denimo dobite priroènik za
		 urejevalnik Emacs. Èe sistema Info ¹e niste
		 uporabljali, vtipkajte ? in Emacs vas bo popeljal na
		 vódeni izlet po naèinu Info in mo¾nostih, ki jih
		 ponuja. Ko boste zakljuèili z branjem tega prvega
		 berila, bo priroènik za Emacs v sistemu Info va¹
		 glavni vir dokumentacije.


* ZAKLJUÈEK
-----------

Zapomnite si, da Emacs zapustite z ukazom C-x C-c. Èe bi radi samo
zaèasno skoèili v ukazno lupino in se kasneje vrnili v Emacs, pa 
storite to z ukazom C-z. 

Ta uèbenik je napisan z namenom, da bi bil razumljiv vsem novincem v
Emacsu. Èe se vam kaj ne zdi jasno napisano, ne valite krivde nase -
prito¾ite se!


* RAZMNO®EVANJE IN RAZ©IRJANJE
------------------------------

Angle¹ki izvirnik tega uvoda v Emacs je naslednik dolge vrste tovrstnih
besedil, zaèen¹i s tistim, ki ga je Stuart Cracraft napisal za izvorni
Emacs. V sloven¹èino ga je prevedel Primo¾ Peterlin.

To besedilo, kot sam GNU Emacs, je avtorsko delo, in njegovo
razmno¾evanje in raz¹irjanje je dovoljeno pod naslednjimi pogoji:

Copyright (c) 1985, 1996, 1997, 2002 Free Software Foundation

   Dovoljeno je izdelovati in raz¹irjati neokrnjene kopije tega spisa
   v kakr¹nikoli obliki pod pogojem, da je ohranjena navedba o
   avtorstvu in to dovoljenje, ter da distributer dovoljuje prejemniku
   nadaljnje raz¹irjanje pod pogoji, navedenimi v tem dovoljenju.

   Pod pogoji iz prej¹njega odstavka je dovoljeno raz¹irjati 
   spremenjene verzije tega spisa ali njegovih delov, èe je jasno
   oznaèeno, kdo je nazadnje vnesel spremembe.

Pogoji za razmno¾evanje in raz¹irjanje samega Emacsa so malo drugaèni,
a v istem duhu. Prosimo, preberite datoteko COPYING in potem dajte
kopijo programa GNU Emacs svojim prijateljem. Pomagajte zatreti
obstrukcionizem (,,lastni¹tvo``) v programju tako, da uporabljate,
pi¹ete in delite prosto programje!

;;; Local Variables:
;;; coding: iso-latin-2
;;; End:
