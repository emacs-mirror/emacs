Copyright (c) 1985 Free Software Foundation, Inc;  Zie de voorwaarden onderaan.
Je leest nu de Emacs uitleg, zoals vertaald door Pieter Schoenmakers.

De meeste Emacs commando's gebruiken de CONTROL toets (soms CTRL of CTL
genaamd) en/of de META toets (soms genaamd EDIT of ALT).  In plaats van
steeds de volledige naam te noemen, gebruiken we de volgende afkortingen:

 C-<chr>  betekent: houd de CONTROL toets ingedrukt en type de toets <chr>
	  Dus C-f wordt: houd de CONTROL toets ingedrukt en type f.
 M-<chr>  betekent: houd de META, EDIT of ALT toets ingedrukt en type de
	  toets <chr>.  Als er geen toets META, EDIT of ALT is, kun je ook
	  eerst de ESC toets typen, gevolgd door <chr>.  We refereren aan
	  de ESC toets als <ESC>.

BELANGRIJK: om Emacs te verlaten, type C-x C-c (twee tekens).
De tekens ">>" tegen de linker kantlijn nodigen je uit om een
bepaald commando te proberen.  Bijvoorbeeld:
<<Blank lines inserted here by startup of help-with-tutorial>>
>>  Type nu C-v (volgend scherm) om naar het volgende scherm te gaan.
	(Geef nu het commando door de control toets ingedrukt te houden
	terwijl je de v typt.)
	Vanaf nu moet je dit steeds herhalen als je klaar bent met het
	lezen van een scherm.

Merk op dat er een tweeregelige overlap is als je van een scherm naar
het volgende scherm gaat; dit zorgt voor continuiteit bij het lezen van
de tekst.

Het eerste wat je moet weten is hoe je naar verschillende plaatsen in de
tekst kan bewegen.  Je weet al hoe je een scherm vooruit moet gaan: met
C-v.  Om een scherm terug te gaan, type M-v (houd de META toets ingedrukt
en type v, of type <ESC>v als je geen META, EDIT of ALT toets hebt).

>>  Probeer nu een paar keer M-v, steeds gevolgd door C-v.


* SAMENVATTING
--------------

De volgende commando's zijn handig volledige schermen te bekijken:

	C-v	ga een scherm vooruit
	M-v	ga een scherm terug
	C-l	maak het scherm schoon en teken alle tekst
		 opnieuw, waarbij de regel waarop de cursor
		 staat op het midden van het scherm terecht
		 komt.  (C-l is control-L, niet control-1.)

>> Kijk waar de cursor is en onthoud de tekst in zijn omgeving.
   Type C-l.
   Zoek de cursor en merk op dat 'ie nog steeds bij dezelfde tekst staat.


* BASISCOMMANDO'S CURSORBEWEGINGEN
----------------------------------

Het is handig om per scherm te bewegen, maar hoe beweeg je nu
naar een specifieke plaats op het scherm?

Er is een aantal manieren waarop je dit kan doen.  De basismanier is
m.b.v de commando's C-p, C-b, C-f en C-n.  Elk van deze commando's
verplaatst de cursor precies een rij of colomn in een bepaalde richting
op het scherm.  Hier volgt een figuur met de vier commando's en de
richting waarin ze de cursor bewegen:

			  vorige regel, C-p
				  :
				  :
    achteruit, C-b .... huidige cursorpositie .... vooruit, C-f
				  :
				  :
			 volgende regel, C-n

>> Verplaats, m.b.v. C-n of C-p, de cursor naar de middelste regel van
   de figuur.  Type dan C-l om de hele figuur in het midden van het
   centrum te plaatsen.

Met een beetje kennis van het engels zijn deze commando's gemakkelijk te
onthouden: de P komt van previous (vorige), de N van next (volgende), de
B van backward (achteruit) en de F van forward (vooruit).  Dit zijn de
basiscommando's om de cursor te bewegen, dus je zult ze CONTINUE
gebruiken: Het is slim als je ze nu leert te gebruiken.

>> Type een paar keer C-n om de cursor op deze regel te krijgen.

>> Beweeg binnen de regel met C-f (herhaaldelijk) en terug omhoog met C-p
   Let op wat C-P doet als de cursor midden in een regel staan.

Elke regel eindigt met een Newline teken (het engelse `new line' betekent
`nieuwe regel'); dit teken scheidt elke regel van de volgende.  De laatste
regel in een bestand moet eigenlijk ook met een Newline eindigen (maar dat
is niet noodzakelijk voor Emacs ).

>> Type een C-b terwijl de cursor aan het begin van een regel staat.
   De cursor zal naar het eind van de vorige regel bewegen, omdat je
   achteruit over het Newline teken gaat.

Net als C-b kan ook C-f over Newline tekens heen bewegen.

>> Type nog wat C-b's zodat je door krijgt waar de cursor is.
   Type dan C-f's om terug naar het einde van de regel te bewegen.
   Een C-f beweegt dan naar de volgende regel.

Wanneer je de cursor voorbij het begin of het einde van het scherm beweegt
zal de tekst over het scherm heen schuiven.  Dit heet `scrollen', of
`schuiven' in goed nederlands.  Door te scrollen zorgt Emacs ervoor dat
de cursor de gewenste beweging kan doen zonder dat de cursor van het
scherm af beweegt.

>> Probeer de cursor voorbij de onderkant van het scherm te bewegen met
   C-n en zie wat er gebeurt.

Als beweging op karakterbasis te langzaam gaat, kan je ook per woord
bewegen.  M-f (Meta-f) beweegt een woord vooruit en M-b een woord
achteruit.

>> Type een paar M-f's en M-b's.

Als je midden in een woord staan beweegt M-f naar het eind van het
woord.  Als je op witruimte tussen woorden staat beweegt M-f naar het
eind van het volgende woord.  M-b beweegt analoog, de andere kant op.

>> Type een paar M-f's en M-b's met tussendoor wat C-f's en C-b's zodat
   je ziet wat M-f en M-b doen vanaf bepaalde plaatsen in een woord en
   tussen twee woorden.

Merk op dat er een analogie zit tussen enerzijds C-f en C-b en
anderzijds M-f en M-b.  Het is bij veel commando's zo dat Meta tekens
gebruikt worden om iets te doen in eenheden van de taal (woorden,
zinnen, paragrafen) terwijl Control tekens te maken hebben met dingen
die los staan van wat je aan het editen bent (tekens, regels, etc).

Deze analogie gaat ook op voor regels en zinnen: C-a en C-e bewegen naar
het begin of eind van een regel, terwijl M-a en M-e naar het begin of
eind van een zin gaan.

>> Probeer een paar C-a's gevolgd door een paar C-e's.
   Probeer een paar M-a's gevolgd door een paar M-e's.

Zie hoe herhaalde C-a's niets doen, terwijl herhaalde M-a's steeds een
zin terug bewegen.  Alhoewel ze niet volledig overeenkomen is het gedrag
van allebei niet onnatuurlijk.

De plaats van de cursor in de tekst wordt `punt' genoemd (zonder
lidwoord, `point' in het engels).  Anders gezegd: de cursor laat op het
scherm de plek zien waarop punt in de tekst staat.

Nu volgt een samenvatting van eenvoudige cursorbewegingsoperaties,
inclusief die commando's die per woord of zin bewegen:

	C-f	ga een teken vooruit
	C-b	ga een teken achteruit

	M-f	ga een woord vooruit
	M-b	ga een woord achteruit

	C-n	ga naar de volgende regel
	C-p	ga naar de vorige regel

	C-a	ga naar het begin van de regel
	C-e	ga naar het eind van de regel

	M-a	ga terug naar het begin van de zin
	M-e	ga vooruit naar het eind van de zin

>> Probeer al deze commando's een paar keer als oefening.
   Deze commando's worden het frequentst gebruikt.

Er zijn nog twee belangrijk cursorbewegingsoperaties: M-<
(Meta kleiner-dan) beweegt naar het begin van het bestand,
en M-> (Meta groter-dan) beweegt naar het eind.

Op de meeste toetsenborden zit de "<" boven de comma, zodat je de Shift
toets (ook wel bekend als de hoofdlettertoets) moet gebruiken om het "<"
teken in te typen.  Op deze toetsenborden moet je ook de shift gebruiken
om M-< in te typen: zonder shift zou je M-, (Meta komma) typen.

>> Type nu M-< om naar het begin van dit bestand te gaan.
   Gebruik daarna C-v om hier weer terug te komen.

>> Type nu M-> om naar het eind van het bestand te springen.
   Gebruik daarna M-v om hier weer terug te komen.
   
Als je toetsenbord pijltjestoetsen heeft kan je ook die gebruiken om de
cursor te verplaatsen.  We raden je aan om C-b, C-f, C-n en C-p op zijn
minst te leren, om drie redenen.  Ten eerste werken ze op alle
toetsenborden, ook die zonder pijltjestoetsen.  Ten tweede zul je merken
dat, wanneer je eenmaal wat ervaring hebt opgedaan in omgaan met Emacs,
het gebruik van deze CTRL tekens sneller is dan de pijltjestoetsen (omdat
je handen in de typehouding kunnen blijven).  Ten derde, als je eenmaal
gewend bent aan deze commando's met CTRL tekens, kan je makkelijk andere
geavanceerde cursorbewegingscommandos leren.

De meeste Emacs commando's accepteren een numeriek argument.  Voor de
meeste commando's is dit argument het aantal keren dat het commando
herhaald moet worden.  Je geeft dit numerieke argument aan door voor het
commando, C-u gevolgd door de cijfers van het getal te typen.  Als je
toetsenbord een META (of EDIT of ALT) toets heeft, is er ook een andere
manier om het getal aan te geven: type de cijfers terwijl je de META toets
ingedrukt houdt.  We raden je aan de C-u manier te leren omdat die werkt
op elk willekeurig toetsenbord.

Bijvoorbeeld, C-u 8 C-f beweegt de cursor 8 plaatsen naar voren.

>> Probeer eens om met C-n of C-p en een numeriek argument de cursor
   met slechts een commando naar een regel in de buurt van deze zin
   te bewegen.

Voor de meeste commando's is het numerieke argument het aantal keren dat
het commando herhaald moet worden.  Voor sommige commando's betekent het
echter iets anders, en C-v en M-v vallen hier ook onder.  Met een numeriek
argument verschuiven deze commando's de tekst het aangegeven aantal regels
in plaats van (bijna) een heel scherm.  Bijvoorbeeld, C-u 4 C-v verschuift
de tekst 4 regels.

>> Probeer nu C-u 8 C-v.

Als het goed is is de tekst daarmee 8 regels opgeschoven.  Als je het weer
terug omlaag wil scrollen kan je een argument aan M-v geven.

Als je een windowing systeem gebruikt, zoals X Windows, dan zou je een
lange rechthoek moeten zien aan de linkerkant van het Emacs window.  Deze
rechthoek heet een scrollbar (misschien is `verschuifbalk' een goede
vertaling).  Je kan de tekst scrollen door met de muis in de scrollbar te
klikken.

>> Klik met de middelste muisknop bovenaan het donkere gebied in de
   scrollbar.  Dit zou de tekst moeten scrollen naar een positie die
   afhankelijk is van hoe hoog of laag je klikt.

>> Beweeg de muis heen en weer terwijl je de middelste muisknop ingedrukt
   houdt.  Je zal zien dat de tekst met de muis mee heen en weer scrollt.


* ALS EMACS HANGT
-----------------

Als Emacs niet meer op commando's reageert kan je haar veilig onderbreken
door C-g te typen.  Je kan C-g gebruiken om een commando te stoppen als
het te lang duurt om uit te voeren.

Je kan C-g ook gebruiken om een numeriek argument weg te gooien of
om het begin van een commando dat je niet wilt afmaken te vergeten.

>> Type nu C-u 100 om een numeriek argument te maken met de waarde 100, en
   type dan C-g.  Type vervolgens C-f.  Als het goed is is de cursor maar
   een positie verplaatst, omdat het argument weggegooid hebt met C-g.

Als je per ongeluk een <ESC> typt kan je daarvan komen met een C-g.


* ONMOGELIJKE COMMANDO'S
------------------------

Sommige Emacs commando's zijn onmogelijk gemaakt zodat beginnende
gebruikers ze niet per ongeluk kunnen uitvoeren.

Als je een van de onmogelijke commando's intypt laat Emacs uitleg zien
over het commando dat je gegeven hebt en vraagt of je het werkelijk uit
wilt voeren.

Wanneer je het commando echt wilt uitvoeren, type dan Spatie (de
spatiebalk) als antwoord op de vraag.  Normaliter wil je het commando niet
uitvoeren en beantwoord je de vraag met "n" (van `no' of `nee').

>> Type C-x C-l (een onmogelijk commando),
   en type dan n als antwoord op de vraag.


* VENSTERS
----------

Emacs kan meerdere vensters laten zien, elk venster met zijn eigen tekst.
We zullen later uitleggen hoe je met meerdere vensters om kan gaan.  Op
dit moment willen we slechts uitleggen hoe je van extra vensters af kunt
komen en terug kan keren naar simpelweg editen met 1 venster.  Het is
eenvoudig:

	C-x 1	een enkel venster (i.e. gooi alle andere vensters weg)

Het commando is Control-x gevolgd door het cijfer 1.  C-x 1 vergroot het
venster waar de cursor in staat tot het hele scherm.  Alle andere vensters
worden weggegooid.

>> Zet de cursor op deze regel en type C-u 0 C-l.
>> Type nu Control-h k Control-f.
   Zie hoe dit venster kleiner wordt, terwijl een nieuw venster verschijnt
   om de documentatie van het Control-f commando te laten zien.

>> Type nu C-x 1 en zie het documentatievenster verdwijnen.


* TOEVOEGEN EN WEGHALEN
-----------------------

Als je tekst toe wilt voegen type je die eenvoudigweg in.  Tekens die je
kan zien, zoals A, 7, *, en dergelijke, worden door Emacs als tekst
ge-interpreteerd en meteen aan de tekst toegevoegd.  Type <Return> (de
`volgende regel' toets) om een Newline toe te voegen en dus een nieuwe
regel te beginnen.

Je kan het laatste teken dat je intypte weghalen door <Delete> te typen.
<Delete> is een toets op het toetsenbord, die misschien ook wel "Del"
heet.  In sommige gevallen dient de "Backspace" toets als <Delete>, maar
niet altijd!

In het algemeen haalt <Delete> het teken dat juist voor de cursorpositie
staat weg.

>> Probeer dit nu: type een paar letters en haal ze weer weg door een paar
   keer op <Delete> te drukken.  Maak je niet druk over het feit dat dit
   bestand verandert; je zal niets veranderen aan de originele versie van
   deze uitleg.  Je zit slechts je eigen copie te wijzigen.

Als een regel tekst te lang wordt om op een regel op het scherm te laten
zien dan gaat de regel verder op een volgende schermregel.  Een backslash
("\") in de rechtermarge laat dan zien dat de regel op de volgende
schermregel verder gaat.

>> Voeg nu tekst toe totdat je de rechter kantlijn raakt, en blijf
   toevoegen.  Je zal zien dat er een vervolgregel verschijnt.

>> Type weer wat <Delete>s om zoveel tekens weg te halen tot de regel weer
   op een schermregel past.  De vervolgregel zal verdwijnen.

Je kan een Newline weggooien als elk ander teken.  Als je een Newline
weggooit voeg je de twee regels waar de Newline tussen staat samen tot een
enkele regel.  Als de regel die het resultaat is van deze operatie niet op
een schermregel past zal ze getoond worden met een vervolgregel.

>> Beweeg de cursor naar het begin van een regel en type <Delete>.  Dit
   voegt de huidige en vorige regel samen.

>> Type <Return> om de Newline die je net weggooide weer toe te voegen.

Je herinnert je dat je bij de meeste Emacs commando's het aantal keren dat
het herhaald moet worden op kan geven.  Dit geldt ook voor gewone tekens.
Als je een gewoon teken herhaalt wordt dat teken herhaaldelijk toegevoegd.

>> Probeer dat nu: type C-u 8 * om ******** toe te voegen.

Je hebt nu de basismanier geleerd om iets in Emacs te typen en fouten te
corrigeren.  Je kan tekst ook per woord of regel weggooien.  Hier volgt
een samenvatting van de commando's om tekst te verwijderen:

	<Delete>     haal het teken weg dat voor de cursor staat
	C-d   	     haal het teken weg dat achter de cursor staat

	M-<Delete>   gooi het woord weg dat voor de cursor staat
	M-d	     gooi het woord weg dat achter de cursor staat

	C-k	     gooi alles weg van de cursor tot het eind van de regel
	M-k	     gooi alles weg van de cursor tot het eind van de zin

Merk op dat <Delete> en C-d met M-<Delete> en M-d de analogie die begon
met C-f en M-f verder trekken (waarbij we voor het gemak even vergeten dat
<Delete> niet echt een control teken is).  C-k en M-k lijken enigzins op
C-e en M-e in hun relatie tot regels en zinnen.

Als je meer dan een enkel teken tegelijk weghaalt bewaart Emacs de tekst
die je weggooit zodat je haar weer terug kan halen.  Weggegooide tekst
terughalen heet "yanken".  Je kan weggegooide tekst terugbrengen op de
plaats waar je haar hebt weggegooid of op een andere plaats in de tekst.
Je kan ook meerdere keren yanken om er meedere copi-en van te maken.  Het
yank-commando is C-y.

Merk op dat er een verschil is tussen het weghalen en weggooien van iets:
iets dat je hebt weggooid kan je terugbrengen, maar iets dat je hebt
weggehaald niet.  (In het engels is het verschil tussen `killing' en
`deleting' duidelijker dan tussen de nederlandse vertaling `weggooien' en
`weghalen'.)  In het algemeen geldt dat de commando's die meer tekst dan
een enkel teken, Newline of spatie verwijderen deze tekst bewaren zodat ze
geyankt kan worden, terwijl dat niet geldt voor commando's die slechts een
enkel teken weghalen.

>> Zet de cursor op het begin van een regel die niet leef is.
   Type C-k om de tekst op die regl weg te gooien.
>> Type C-k een tweede keer.  Nu gooit dit commando het Newline teken
   weggooit.

Merk op hoe een enkele C-k de inhoud van een regel weggooit, een tweede
C-k de regel zelf zodat alle volgende regels een regel omhoog komen.  Het
numerieke argument is voor C-k bijzonder: het aangegeven aantal regels zal
worden weggegooid, inclusief de inhoud.  Dit is meer dan simpelweg
herhaling: C-u 2 C-k gooit twee regels weg, terwijl tweemaal C-k typen dat
niet doet.

Om de laatst weggegooide tekst terug te halen naar de plaats waar de
cursor nu op staat (te yanken), type C-y.

>> Probeer het nu: type C-y om de tekst te yanken.

Het is alsof je met C-y iets uit de prullenbak haalt wat je net had
weggegooid.  Merk op dat verschillende C-k's achter elkaar alle regels
die weggegooid worden bij elkaar bewaart zodat een enkele C-y die regels
in een keer terugbrengt.

>> Probeer het nu: type C-k een paar keer.

Om de weggegooide tekst terug te halen:

>> Type C-y.  Beweeg de cursor wat regels naar beneden en type weer C-y.
   Je ziet nu hoe je tekst kan copieren.

Wat nu te doen als je wat tekst terug wilt brengen, maar je hebt intussen
al iets anders weggegooid?  C-y zou datgene terugbrengen wat je het
recentst hebt weggegooid.  Gelukkig is de voorgaande tekst niet verloren
gegaan.  Je kunt die tekst terughalen met M-y.  Nadat je C-y hebt getypt
om de recentst weggegooide tekst terug te halen, vervangt M-y die tekst
met de tekst die je daarvoor had weggegooid.  Je kunt M-y herhalen om
tekst terug te halen die je steeds langer geleden hebt weggegooid.  Als je
de tekst te pakken hebt die je zocht hoe je niets te doen om die daar te
houden.  Je kan gewoon verder werken en de teruggehaalde tekst met rust
laten.

Als je M-y vaak genoeg typt kom je terug waar je begon, bij de recentst
weggegooide tekst.

>> Gooi een regel weg, beweeg de cursor wat, en gooi nog een regel weg.
   Type C-y om de tweede regel die je weggooide terug te halen.
   Type nog een M-y en die regel wordt vervangen door de eerste regel
   die je weggooide.
   Type nog wat M-y's en zie wat er langs komt.  Herhaal dit tot de
   tweede regel weer langs komt, en dan nog een paar keer.
   Je kan ook experimenteren met positieve en negatieve argumenten aan
   M-y.


* HERSTELLEN
------------

Als je de tekst veranderd hebt en je daar toch niet tevreden mee bent,
dan kan je de verandering ongedaan maken met het herstel commando, C-x u.

Normaal gesproken herstelt C-x u de veranderingen die het gevolg zijn van
een enkel commando; door herhaaldelijk C-x u te typen, worden steeds
eerdere commando's hersteld.

Er zijn echter twee uitzonderingen: commando's die de tekst niet wijzigen,
zoals cursorbewegingen, worden overgeslagen, en commando's die simpelweg
de ingetypte letter aan de tekst toevoegen worden meestal gegroepeerd
in groepjes van maximaal 20 tekens, zodat je minder C-x u's hoeft te
type om het toevoegen van teksts te herstellen.

>> Gooi deze regel weg met C-k; met C-x u zou ze weer moeten verschijnen.

C-_ is een alternatief voor C-x u.  Het levert exact hetzelfde resultaat
op, maar is makkelijker om een paar keer achter elkaar te typen.  Een
nadeel van C-_ is dat op sommige toetsenborden het intypen ervan niet
triviaal is.  Dat is ook de reden het alternatief, C-x u.  Op sommige
terminals kan je C-_ typen door te doen alsof je C-/ typt.

Een numeriek argument aan C-_ of C-x u duidt het aantal herhalingen aan.


* BESTANDEN
-----------

Om een tekst die je gemaakt of veranderd hebt op te slaan moet je de
tekst in een bestand bewaren (`to save a file' in het engels).  Als je
dat niet doet ben je die veranderingen kwijt op het moment dat je uit
Emacs gaat.  Je kan een bestand veranderen door het bestand `bezoeken'.
(Ook wel `vinden'; `finding' of `visiting' in het engels.)

Het bezoeken van een bestand betekent dat je de inhoud van dat bestand
in Emacs ziet.  Het lijkt er dan op alsof je het bestand aan het
veranderen bent.  Echter, deze veranderingen zijn slechts tijdelijk
zolang je het bestand niet bewaart.  Op deze manier kan je nooit per
ongeluk een half gewijzigd bestand op het systeem achterlaten.  Zelfs
als je het bestand bewaart, zorgt Emacs ervoor dat het originele
bestand onder een gewijzigde naam nog steeds beschikbaar is, voor het
geval je later besluit dat de veranderingen toch niet zo'n goed plan
waren.

In de buurt van de onderkant van het scherm zie je een regel die begint en
eindigt met streepjes, met aan het begin "--:-- TUTORIAL.nl" of iets
dergelijks.  Dit deel van het scherm laat normaal de naam van het bestand
zien dat je op dat moment bezoekt.  Op dit moment bezoek je een bestand
dat "TUTORIAL.nl" heet; het is je eigen copie van de nederlandstalige
Emacs uitleg (`tutorial' in het engels).  Als je in Emacs een bestand
bezoekt dan staat de naam van het bestand altijd op deze plaats.

De commando's om een bestand te bezoek of te bewaren zijn anders dan de
commando's die je tot nu toe geleerd hebt; ze bestaan namelijk uit twee
tekens.  Beide commando's beginnen met het teken Control-x.  Er zijn een
heleboel commando's die met Control-x beginnen.  Veel van die commando's
hebben te maken met bestanden, buffers, en daaraan gerelateerde zaken.
Dergelijke commando's bestaan uit twee, drie of vier tekens.

Nog iets bijzonders aan het commando om een bestand te bezoeken is dat
je aan moet geven welk bestand je wilt.  Dit heet dat het commando "een
argument van de gebruiker vraagt"; in dit geval de naam van het bestand.
Nadat je het commando

	C-x C-f		bezoek bestand (met de f van `find file')

hebt getypt vraagt Emacs om de naam van het bestand.  De naam die je
intypt verschijnt op de onderste regel van het scherm.  Wanneer die regel
voor dit soort invoer gebruikt wordt, heet ze de minibuffer.  Je kan de
gebruikelijke Emacs commando's gebruiken om de filename in te typen.

Tijdens het invoeren van de naam van het bestand (of willekeurig wat
voor minibuffer invoer) kan je het commando afbreken met C-g.

>> Type C-x C-f gevolgd door C-g.  Dit breekt de minibuffer af en
   ook het C-x C-f commando dat van de minibuffer gebruik maakte.
   Netto resultaat is dat je geen bestand bezoekt.

Als je de naam van een bestand hebt ingevoerd, type dan <Return> om het
af te sluiten.  Hierna gaat het C-x C-f commando aan het werk en bezoekt
het bestand dat je aangegeven hebt.  Als het C-x C-f commando klaar is,
verdwijnt de minibuffer.

Na korte tijd verschijnt de inhoud van het bestand op het scherm en kan
je de inhoud wijzigen.  Als je de wijzigingen op wilt slaan, type dan het
commando

	C-x C-s   bewaar bestand (met de s van `save file')

Dit bewaart de tekst zoals Emacs die nu heeft in het bestand.  De eerste
keer dat je dit doet bewaart Emacs het originele bestand onder een andere
naam zodat het nog niet verloren is.  De nieuwe naam bestaat uit de oude
naam gevolgd door een "~".

Als Emacs klaar is het bestand te bewaren laat ze de naam van het bestand
zien.  Het is een goede gewoonte een bestand redelijk vaak te bewaren
zodat er niet teveel werk verloren gaat als het systeem hangt of crasht.

>> Type C-x C-s, om je copie van deze uitleg te bewaren.  Als het goed is
   verschijnt "Wrote ...TUTORIAL" op de onderste schermregel.

OPMERKING: Op sommige systemen gebeurt er helemaal niets als je C-x C-s
typt, en daarna ook niets meer.  Dit komt door een eigenschap van de
machine waarop je werkt die te maken heeft met `flow control'.  Met C-s
stopt de `flow' en komt niets meer van wat je typt bij Emacs terecht.  Om
deze situatie te herstellen, type C-q.  Lees daarna de "Spontaneous Entry
to Incremental Search" sectie in het Emacs handboek over hoe om te gaan
met deze situatie.

Je kan een bestaand bestand bezoeken, om het te bekijken of het te
wijzigen.  Je kan ook een bestand bezoeken dat nog niet bestaat.  Dit is
de manier om met Emacs een nieuw bestand te maken: bezoek het bestand, dat
initieel leeg zal zijn, en voeg tekst toe.  Zodra je de tekst bewaart
wordt het bestand werkelijk gecreeerd, met de tekst als inhoud.  Vanaf dat
moment ben je dus bezig met een bestaand bestand.


* BUFFERS
---------

Als je een tweede bestand bezoekt met C-x C-f blijft het eerste bestand
gewoon in Emacs.  Je kan naar dat bestand terug door het gewoon nog een
keer te bezoeken met C-x C-f.  Op deze manier kan je een behoorlijk aantal
bestanden in Emacs krijgen.

>> Cre-eer een bestand dat "foo" heet door te typen: C-f C-f foo
   <Return>.  Voeg hieraan wat tekst toe, wijzig haar, en bewaar "foo"
   door C-x C-s te typen.  Type hierna C-x C-f TUTORIAL <Return> om
   weer hier, in de uitleg, terug te komen.

Emacs bewaart intern de tekst van elk bestand in een ding dat een "buffer"
genoemd wordt.  Als je een bestand bezoekt wordt er een nieuwe buffer
gemaakt.  Om een lijst van de huidige buffers te zien, type

	C-x C-b   laat de bufferlijst zien

>> Probeer C-x C-b nu.

Zie dat elke buffer een naam heeft en mogelijk ook een bestandsnaam; dit
is de naam van het bestand waarmee de buffer overeenkomt.  Sommige buffers
hebben niets met een bestand te maken.  Bijvoorbeeld, de buffer die
"*Buffer List*" heet heeft geen bestand.  Die buffer is de buffer die de
lijst bevat die door C-x C-b gemaakt wordt.  ALLE tekst die je in een
Emacs venster ziet is altijd onderdeel van een of andere buffer.

>> Type C-x 1 om de bufferlijst te verwijderen.

Als je de tekst van het ene bestand verandert en dan een ander bestand
bezoekt dan wordt het eerste bestand niet bewaard.  De wijzigingen blijven
in Emacs, in de buffer die bij het bestand hoort.  Het cre-eren of
modificeren van de buffer van het tweede bestand heeft geen effect op de
eerste buffer.  Dit is erg nuttig, maar betekent ook dat er een eenvoudige
manier nodig is om het eerste bestand te bewaren.  Het zou erg vervelend
zijn om er eerst naar terug te moeten gaan met C-x C-f om het dan te
kunnen bewaren met C-x C-s.  Dus hebben we

	C-x s	  bewaar een paar buffers

C-x s vraagt voor elke buffer die veranderingen heeft die nog niet
opgeslagen zijn, of je de buffer wilt bewaren.

>> Voeg een wat tekst toe en type C-x s.
   Emacs vraagt nu of je de buffer die TUTORIAL.nl heet wilt bewaren.
   Bewantwoord deze vraag positief door een "y" in te typen (de y van
   "yes", engels voor "ja").


* UITGEBREIDE COMMANDO'S
------------------------

Er zijn veel meer Emacs commando's dan er op de toetsen van het
toetsenbord passen, zelfs als we hun aantal kunnen vergroten door de
control of meta toets te gebruiken.  Emacs lost dit probleem op met het X
commando (met de X van eXtensie of uitbreiding).  Het X commando komt in
twee smaken:

	C-x	teken eXtensie; wordt gevolgd door een teken
	M-x	genaamd commando eXtensie; wordt gevolgd door een naam.

Deze commando's zijn in het algemeen nuttig, maar worden minder gebruikt
dan de commando's die tot nu toe uitgelegd zijn.  Je hebt al twee van deze
commando's gezien: C-x C-f om een bestand te bezoeken, en C-x C-s om het
te bewaren.  Een ander voorbeeld is het commando om Emacs te verlaten: dit
is C-x C-c.  (Maak je geen zorgen over het verloren gaan van veranderingen
die niet bewaard zijn; C-x C-c vraagt of je veranderde buffers wilt
bewaren voordat Emacs daadwerkelijk eindigt.)

C-z is het commando om Emacs *tijdelijk* te verlaten, zodat je daarna weer
terug kan keren in dezelfde Emacs sessie.

Op systemen die deze mogelijkheid bieden, zet C-z Emacs stil: je komt weer
terug in de shell, maar Emacs is nog aanwezig.  In de meeste shells kan je
Emacs weer activeren met het "fg" commando, of met "%emacs".

Op systemen die niet de mogelijkheid bieden om programma's stil te zetten
cre-eert C-z een subshell onder Emacs om je zo in de gelegenheid te
stellen andere programma's uit te voeren en daarna weer in Emacs terug te
keren; Emacs wordt dus niet werkelijk verlaten.  In dit geval is het
shellcommando "exit" de normale manier om de subshell te verlaten en in
Emacs terug te keren.

Het moment om C-x C-c te gebruiken is wanneer je uit gaat loggen.  Het is
ook het juiste commando om Emacs te be-eindigen wanneer Emacs opgestart
was door een mail programma of iets dergelijks, aangezien die misschien
niet met een stilgezette Emacs om kunnen gaan.  Normaal gezien is het
echter beter Emacs stil te zetten met C-z dan om Emacs te verlaten,
behalve als je uit wilt loggen natuurlijk.

Er bestaan vele C-x commando's.  Hier is een lijst van degene die je nu al
kent:

	C-x C-f		bezoek bestand
	C-x C-s		bewaar bestand
	C-x C-b		laat bufferlijst zien
	C-x C-c		verlaat Emacs
	C-x u		herstel

Genaamde uitgebreide commando's worden nog minder vaak gebruikt, of worden
alleen onder bepaalde omstandigheden gebruikt.  Een voorbeeld is het
commando replace-string, dat in de hele tekst een string vervangt door een
andere string (`to replace' betekent `vervangen').  Als je M-x typt echoot
Emacs onderaan het scherm `M-x' en moet je de naam van het commando
intypen, in dit geval "replace-string".  Als je gewoon "repl s<TAB>" typt
maakt Emacs de naam zelf af.  Be-eindig het commando met <Return>.

Het replace-string commando heeft twee argumenten nodig: de string die
vervangen moet worden en de string waarmee die vervangen moet worden.
Je sluit elk argument af met <Return>.

>> Plaats de cursor op de lege regel twee regels onder deze.
   Type dan M-x repl s<Return>gewijzigd<Return>veranderd<Return>.

   Zie hoe deze regel daardoor gewijzigd is.  Je hebt elk voorkomen van
   het woord g-e-w-i-j-z-i-g-d vervangen door "veranderd"; beginnend op
   de plek waar de cursor staat.


* AUTOMATISCH BEWAREN
---------------------

Als je een bestand veranderd hebt maar het nog niet bewaard hebt, zouden
de veranderinge verloren kunnen gaan als het systeem zou hangen of
herstarten.  Om je hiertegen te beschermen bewaart Emacs om de zoveel tijd
de veranderde tekst automatisch.  De naam van het bestand waarin de tekst
automatisch bewaard wordt begint en eindigt met een #.  Bijvoorbeeld, als
je het bestand "hello.c" aan het bewerken bent dan wordt de tekst
automatisch bewaard in een bestand dat "#hello.c#" heet.  Zodra je het
bestand werkelijk bewaart, wordt het automatisch bewaarde bestand weer
weggegooid.

Als de computer crasht kan je de automatisch bewaarde tekst terugkrijgen
door de file normal te bezoeken (de originele file, niet de automatisch
bewaarde), gevolgd door M-x recover file<Return>.  Als Emacs vraagt om
bevestiging, antwoord dan yes<Return> en de automatisch bewaarde
informatie wordt teruggehaald.


* ECHO GEBIED
-------------

Als je een commando langzaam intypt echoot Emacs de tekens aan de
onderkant van het scherm, in een deel dat het "echo gebied" genoemd wordt.
Dit gebied bevat de onderste regel van het scherm.


* MODE-REGEL
------------

De regel direct boven het echo gebied heet de "mode-regel".  De mode-regel
zier er ongeveer zo uit:

--**-Emacs: TUTORIAL.nl       (Fundamental)--68%------------------------

Deze regel geeft interessante informatie over Emacs en de tekst die
je aan het bewerken bent.

Je weet al wat de bestandsnaam betekent: het is de naam van het bestand
dat je bezoekt.  -NN%-- geeft je huidige positie in de tekst aan: NN
procent van de tekst bevindt zich boven het scherm.  Als het bestand vanaf
het begin op het scherm staat, staat er --Top-- in plaats van --00%--.
Als het laatste stuk tekst op het scherm staat, zal er --Bot-- staan (van
`bottom', `onderkant' in het nederlands).  Als de tekst zo klein is dat ze
volledig op het scherm past staat --All-- in de mode-regel.

De sterretjes aan het begin betekenen dat je de tekst gemodificeerd hebt.
Direct na het bezoeken of bewaren staan er gewoon streepjes.

In de mode-regel staat tussen haakjes in welke mode je aan het werken
bent.  Tenzij een andere mode gewenst is, zit je in de "Fundamental" mode
zoals nu (`fundamental' is `basis' in het nederlands).  Een dergelijke
mode heet een hoofdmode (`major mode' in het engels).

Emacs heeft verschillende hoofdmodes.  Sommige daarvan zijn bedoelt voor
het bewerken van verschillende talen of soorten tekst, zoals bijvoorbeeld
Lisp mode, Text mode, etc.  Op elk moment is er altijd precies een mode
actief, en de naam daarvan staat in de mode-regel, op de plaats waar nu
"Fundamental" staat.

Elke hoofdmode zorgt ervoor dat sommige commando's zich anders gedragen.
Zo bestaat er een commando om een commentaar in een programma te typen, en
aangezien elke programmeertaal een ander idee heeft over hoe commentaar
eruit moet zien, moet elke hoofdmode op een andere manier het commentaar
beginnen.  Elke hoofdmode is de naam van een uitgebreid commando, en met
dat commando schakel je om naar die hoofdmode.  Zo is bijvoorbeeld
M-x fundamental-mode het commando om naar de basismode om te schakelen.

Als je nederlandse of engelse tekst wil gaan bewerken, zoals bijvoorbeeld
dit bestand, kan je beter "text mode" gebruiken, de mode om tekst in een
gewone taal te bewerken:

>> Type M-x text-mode<Return>.

Wees gerust; geen van de commando's die je geleerd hebt zorgen voor
grondige veranderingen in Emacs.  Een van de dingen die je kan merken is
bijvoorbeeld dat M-f en M-b nu apostrophes als onderdeel van een woord
beschouwen.  In de vorige, Fundamental, mode behandelen M-f en M-b de
apostrophe als ruimte tussen twee woorden.

Het is gebruikelijk dat hoofdmodes dergelijke subtiele verschillen hebben.
De meeste commando's doen dus min of meer hetzelfde in elke hoofdmode.

Met het commando C-h m kan je de documentatie over de huidige hoofdmode
lezen.

>> Gebruik C-u C-v een paar keer om deze zin in de buurt van de bovenkant
   van het scherm te krijgen.
>> Type C-h m om te zien hoe Text mode verschilt van Fundamental mode.
>> Type C-x 1 om de documentatie van het scherm te verwijderen.

Hoofdmodes heten hoofdmodes omdat er ook bijmodes zijn.  Bijmodes zijn
geen alternatieven voor hoofdmodes; het zijn slechts kleine modificaties
daarvan.  Elke bijmode kan aan- of uitgezet worden, onafhankelijk van
andere bijmodes en onafhankelijk van de hoofdmode.  Je kan dus nul, een,
of willekeurig veel minor modes gebruiken.

Een nuttige bijmode voor het bewerken van tekst in een natuurlijke taal,
zoals nederlands, is Auto Fill mode (`auto fill' betekent automatisch
uitvullen).  Wanneer deze mode aanstaat breekt Emacs automatisch een regel
tussen twee woorden af als de regel anders te lang zou worden.

Je kan Auto Fill mode aanzetten met M-x auto-fill-mode<Return>.  Als deze
mode al aanstaat, kan je hem uitzetten met M-x auto-fill-mode<Return>.
Als de mode uitstaat zet dit commando de mode aan; als ze aanstaat zet dit
commando de mode uit.  Het commando zet de mode steeds aan en uit zet (`to
toggle' in het engels).

>> Type nu M-x auto-fill-mode<Return>.  Type nu vele malen asdf op een
   regel zodat je kan zien dat de regel in twee-en gesplitst wordt.  Er
   moeten wel spaties tussen de woorden staan, omdat de Auto Fill mode
   alleen op spaties de regel breekt.

De rechter kantlijn staat meestal op 70 tekens, maar die kan je veranderen
met het C-x f commando.  Dit commando accepteert een numeriek argument
dat de gewenste kantlijn is.

>> Type C-x f met 20 als argument (C-u 20 C-x f).
   Type wat tekst en zie dat Emacs de regels afbreekt bij 20 tekens.
   Zet de kantlijn nu terug op 70, weer met C-x f.

Als je de tekst midden in een regel verandert vult Auto Fill mode
de regel niet opnieuw.
Om een paragraaf opnieuw te vullen, type M-q (Meta-q) terwijl de
cursor in de paragraaf staat.

>> Plaats de cursor in de voorgaande paragraaf en type M-q.


* ZOEKEN
--------

Emacs kan strings zoeken (een string is een rij tekens), zowel volgend op
de cursorpositie, als eraan voorafgaand.  Het zoeken van een string
verplaatst de cursor naar de volgende plaats waar de gezochte string
voorkomt.

Het zoekcommando van Emacs is anders dan de zoekcommando's van de meeste
tekstverwerkers; het zoekt incrementeel.  Dit betekent dat het zoeken
gebeurt tijdens het intypen van de gezochte string.

Het commando om vooruit zoeken te starten is C-s (met de `s' van `to
search', i.e. zoeken); C-r start het zoeken achteruit (met de `r' van
`reverse' of achteruit).  WACHT nog even met ze te proberen.

Als je C-s typt verschijnt de string "I-search" in het echo gebied.  Dit
betekent dat Emacs bezig is met een `incremental search' (incrementele
zoekopdracht) en wacht op het intypen van de zoekstring.  <RET> be-eindigt
het zoeken.

>> Type nu C-s om het zoeken te start.  Type nu, LANGZAAM, een letter per
   keer, het woord `cursor', met een pauze na elke letter zodat je kan
   zien wat er met de cursor gebeurt.  Je hebt nu eenmaal naar het woord
   `cursor' gezocht.
>> Type nogmaals C-s, om naar het volgende voorkomen van `cursor' te
   zoeken.
>> Type nu viermaal <Delete> en let op de cursorbewegingen.
>> Type <RET> om het zoeken te be-eindigen.

Zag je wat er gebeurde?  Tijdens incrementeel zoeken probeert Emacs naar
de eerste plek te gaan waar de string staat die je tot dan toe getypt
hebt.  Om naar de volgende plek te gaan, type je C-s nog een keer.  Als er
geen volgende plek is piept Emacs en vertelt je dat de zoekopdracht faalt
(`failing' in het engels); met C-g kan je het zoeken afbreken.

OPMERKING: Op sommige systemen gebeurt er helemaal niets als je C-x C-s
typt, en daarna ook niets meer.  Dit komt door een eigenschap van de
machine waarop je werkt die te maken heeft met `flow control'.  Met C-s
stopt de `flow' en komt niets meer van wat je typt bij Emacs terecht.  Om
deze situatie te herstellen, type C-q.  Lees daarna de "Spontaneous Entry
to Incremental Search" sectie in het Emacs handboek over hoe om te gaan
met deze situatie.

Als je tijdens incrementeel zoeken <Delete> typt, zal je zien dat het
laatste teken dat je in de zoekstring typte weggehaald wordt en dat het
zoeken teruggaat naar de voorgaande plaats.  Als je bijvoorbeeld begint
met zoeken en je typt een "c" dan ga je naar het eerste voorkomen van een
"c".  Type je vervolgens een "u" dan gaat de cursor naar het eerste
voorkomen van de string "cu".  Als je nu <Delete> typt, dan wordt de "u"
van de zoekstring afgehaald en gaat de cursor terug naar de plaats waar
hij stond voordat je de "u" intypte, i.e. het eerste voorkomen van de "c".

Als je tijdens een zoekoperatie een control- of meta-teken intypt dan
wordt het zoeken be-eindigd.  Er zijn een paar uitzonderingen, namelijk
tekens die tijdens zoeken een speciale betekenis hebben, zoals C-s en C-r.

Met C-s begin je te zoeken naar het eerste voorkomen van de zoekstring NA
de huidige cursorpositie.  Als je iets wilt zoeken dat eerder in de tekst
moet voorkomen, gebruik dan C-r i.p.v. C-s.  Alles wat we nu weten over
C-s geldt ook voor C-r, alleen de zoekrichting is omgedraaid.


* MEERDERE VENSTERS
-------------------

Een van Emacs' aardige eigenschappen is dat je meerdere vensters op het
scherm kan laten zien.

>> Zet de cursor op deze regel en type C-u 0 C-l.

>> Type C-x 2 om het scherm in twee vensters op te splitsen.
   Beide vensters laten deze uitleg zien; de cursor blijft in het
   bovenste venster.

>> Type C-M-v om de tekst in het onderste venster te verschuiven.
   (Als je geen Meta toets hebt, type dan ESC C-v.)

>> Type C-x o (met de `o' van `other'; `ander' in het nederlands)
   om de cursor naar het andere venster te verplaatsen.

>> Verschuif de tekst in het onderste venster, m.b.v. C-v en M-v.
   Zorg ervoor dat je deze uitleg in het bovenste venster leest.  

>> Type weer C-x o om de cursor weer in het bovenste venster
   te zetten.  De cursor staat weer precies op de plaats waar
   hij stond toen je het venster verliet.

Je kan C-x o blijven gebruiken om van venster naar venster te gaan.  Elk
venster heeft zijn eigen cursorpositie; de cursor is altijd maar zichtbaar
in een daarvan.  Alle normale commando's hebben betrekking op het venster
waar de cursor in staat.  Dit venster is het `geselecteerde venster'
(`selected window' in het engels).

Het C-M-v commando is erg nuttig wanneer je tekst aan het bewerken bent in
het ene venster, terwijl je het andere venster als referentie gebruikt.
Je kan de cursor dan altijd in het venster houden waarin je bezig bent,
terwijl je met C-M-v door de tekst in het andere venster loopt.

C-M-v is een voorbeeld van een CONTROL-META teken.  Als je een echte META
toets hebt kan je C-M-v intypen door zowel CTRL als META ingedrukt te
houden terwijl je v typt.  Het maakt niet uit in welke volgorde je de CTRL
en META indrukt; het gaat erom welke ingedrukt zijn terwijl je typt.

Als je geen echte META toets hebt kan je ESC gebruiken; de volgorde maakt
dan wel uit.  Je moet dan ESC typen, gevolgd door CTRL-v; CTRL-ESC v zal
niet werken.  Dit komt doordat ESC zelf een teken is, terwijl CTRL en META
dat niet zijn.

>> Type C-x 1 (in het bovenste venster) om het onderste venster te laten
   verdwijnen.

(Als je C-x 1 typt in het onderste venster laat je het bovenste
verdwijnen.  C-x 1 betekent zoveel als `ik wil maar 1 venster,
en wel dat venster waar de cursor nu in staat.')

Hier is nog een manier om twee venster te krijgen die elk een andere tekst
laten zien:

>> Type C-x 4 C-f gevolgd door de naam van een van je bestanden, gevolgd
   door <Return>.  Het opgegeven bestand zal in het onderste venster
   verschijnen, en de cursor zal in dat venster staan.

>> Type C-x o om terug naar het bovenste venster te gaan, en C-x 1 om
   het onderste venster te laten verdwijnen.


* RECURSIEVE BEWERKINGSNIVEAUS
------------------------------

Soms kom je in Emacs in een recursief bewerkingsniveau terecht (engels:
`recursive editing level').  Dit is te zien in de moderegel aan de rechte
haken om de haakjes die om naam van de hoofdmode staan.  Dan staat er
bijvoorbeeld [(Fundamental)] in plaats van (Fundamental).

Type ESC ESC ESC Om uit een recursief bewerkingsniveau te komen.  Dit is
een generiek `ontsnappingscommando'.  Je kan het ook gebruiken om extra
vensters weg te gooien of om uit de minibuffer te komen.

>> Type M-x om in een minibuffer te komen, en type dan ESC ESC ESC
   om er weer uit te komen.

C-g is niet bruikbaar om uit een recursief bewerkingsniveau te komen.  De
reden hiervoor is dat C-g gebruikt wordt om commando's af te breken BINNEN
het recursieve bewerkingsniveau.


* MEER INFORMATIE
-----------------

We hebben geprobeerd je met deze uitleg precies genoeg informatie te geven
om met Emacs te beginnen.  De mogelijkheden van Emacs zijn zo legio dat
het onmogelijk is nu alles uit te leggen.  Emacs heeft zoveel nuttige
mogelijkheden dat je er meer over zou kunnen willen leren.  Emacs heeft
commando's om documentatie te laten zien over Emacs commando's.  Deze
`helpcommando's' beginnen allemaal met C-h: `het Hulpteken'.

Om hulp te krijgen, type C-h, gevolgd door een teken om aan te duiden
welke hulp je wilt.  Als je het echt niet meer weet, type C-h ? en Emacs
vertelt welke hulp het allemaal te bieden heeft.  Als je C-h hebt getypt
maar van gedachten veranderd bent, type je gewoon C-g om het af te breken.

(Op sommige computers is de betekenis van C-h veranderd.  Dat is een
slecht plan, zeker als die verandering op alle gebruikers invloed heeft,
en is dus een geldige reden om je beklag te doen bij de systeembeheerder
of helpdesk.  Als C-h intussen niet een bericht onderaan het scherm laat
zien over mogelijke hulp, probeer dan de F1 toets (functietoets 1) of
gebruik M-x help RET.)

De eenvoudigste hulp is C-h c.  Type C-h, het teken `c' en een teken of
uitgebreid commando, en Emacs laat een zeer korte beschrijving van het
commando zien.

>> Type C-h c Control-p.
   De beschrijving die getoond wordt zou zoiets moeten zijn als

	C-p runs the command previous-line

   (nederlands: C-p voert het commando previous-line uit.)

Dit commando vertelt je `de naam van de functie'.  Functies worden vooral
gebruikt om Emacs uit te breiden of aan de wensen van de gebruiker aan te
passen.  Aangezien functienamen gekozen zijn om aan te geven wat de
functie doet, zijn ze ook geschikt als erg korte documentatie; genoeg om
je te herinneren aan wat de commando's die je al geleerd hebt betekenen.

Uitgebreide commando's zoals C-x C-s en (als je geen META, EDIT or ALT
toets hebt) <ESC> v kunnen ook getypt worden na C-h c.

Om meer informatie over een commando te krijgen, type C-h k in plaats van
C-h c.

>> Type C-h k Control-p.

Dit laat de documentatie van de functie zien, inclusief de naam van de
functies, in een apart venster.  Als je klaar bent met lezen, type C-x 1
om van dat venster af te komen.  Je hoeft dat natuurlijk niet meteen te
doen.  Je kan ook eerst wat anders doen voordat je C-x 1 typt.

Hier zijn nog wat nuttige mogelijkheden van C-h:

   C-h f	Beschrijf een functie.  Je moet de naam van de functie
		intypen.

>> Type C-h f previous-line<Return>
   Dit laat alle informatie zien die Emacs heeft over de functie die het
   C-p commando implementeert.

   C-h a	Commando Apropos.  Type een woord in en Emacs zal een
		lijst van alle commando's laten zien waarin dat woord
		voorkomt.  Al deze commando's kunnen aangeroepen worden
		met M-x.  Bij sommige commando's staat met welke tekens
		dit commando direct uitgevoerd kan worden.

>> Type C-h a file<Return>.

Dit laat in een ander venster alle M-x commando's zien met `file' in hun
naam.  Je zal teken-commando's zien als C-x C-f naast de overeenkomende
commandonaam zoals find-file.

>> Type C-M-v herhaaldelijk om de tekst in het hulpvenster te verschuiven.

>> Type C-x 1 om het hulpvenster weg te gooien.


* CONCLUSIE
-----------

Denk eraan dat je met C-x C-c gebruikt om Emacs te verlaten.  Om tijdelijk
een shell te krijgen en daarna weer in Emacs terug te komen, type C-x.

De bedoeling van deze uitleg is dat ze begrijpelijk is voor alle nieuwe
Emacs gebruikers.  Als je dus iets onduidelijks bent tegengekomen blijf
dan niet zitten en maak jezelf geen verwijten.  Klaag erover!


* COPI-EREN
-----------

(De engelse versie van) deze uitleg is voorafgegaan door een lange reeks
van Emacs tutorials, die begon met de uitleg die Stuart Cracraft schreef
voor de originele Emacs.  Deze nederlandse vertaling is gemaakt door
Pieter Schoenmakers <tiggr@ics.ele.tue.nl> op basis van de GNU Emacs 20.2
TUTORIAL.

(Wat nu volgt is een vertaling naar het nederlands van de condities voor
gebruik en verspreiding van deze uitleg.  Deze vertaling is niet
gecontroleerd door een jurist.  Er kunnen derhalve geen rechten aan de
vertaling worden ontleend, en de vertaling wordt gevolgd door het engelse
origineel.)

Deze versie van de uitleg valt onder copyright, net als GNU Emacs.
Je mag deze uitleg distribu-eren onder bepaalde condities:

Copyright (c) 1985, 1996, 1997 Free Software Foundation

   Iedereen mag letterlijke copi-en van dit document, zoals ontvangen,
   verspreiden, op elke medium, vooropgesteld dat de copyrightmelding en
   toestemmingsmelding niet aangetast worden en dat de verspreider aan de
   ontvanger dezelfde distributierechten verleend als aan hem verleend
   door deze melding.

   Toestemming wordt verleend om gemodificeerde versies van dit document,
   of delen daarvan, te verspreiden, onder bovenstaande condities,
   vooropgesteld dat ze ook duidelijk melding maken van degene die als
   laatste modificaties doorgevoerd heeft.

De condities voor het copi-eren van Emacs zelf zijn complexer dan dit,
maar gebaseerd op dezelfde gedachte.  Lees het bestand COPYING en geef
vervolgens copi-en van Emacs aan al je vrienden.  Help bij het uitroeien
van softwarebeschermingspolitiek (`software eigendom') door vrije software
te gebruiken, schrijven en delen!

(Engels origineel van de copyrightmelding en condities:

This version of the tutorial, like GNU Emacs, is copyrighted, and
comes with permission to distribute copies on certain conditions:

Copyright (c) 1985, 1996 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

The conditions for copying Emacs itself are more complex, but in the
same spirit.  Please read the file COPYING and then do give copies of
GNU Emacs to your friends.  Help stamp out software obstructionism
("ownership") by using, writing, and sharing free software!)
