Emacs-inleiding.  De kopieervoorwaarden staan onderaan.

De meeste Emacs-commando's gebruiken de CONTROL-toets (soms CTRL of CTL
genaamd) en/of de META-toets (soms genaamd EDIT of ALT).  In plaats van
steeds de volledige naam te noemen, gebruiken we de volgende afkortingen:

 C-<ltr>  betekent: houd de CONTROL-toets ingedrukt en tik de toets <ltr>
	  Dus C-f wordt: houd de CONTROL-toets ingedrukt en tik f.
 M-<ltr>  betekent: houd de META-, EDIT- of ALT-toets ingedrukt en tik
	  de toets <ltr>.  Als er geen toets META, EDIT of ALT is, kun
	  je ook eerst de ESC-toets tikken, gevolgd door <ltr>.  We
	  verwijzen naar de ESC-toets als <ESC>.

BELANGRIJK: om Emacs te verlaten, tik C-x C-c (twee tekens).
De tekens ">>" tegen de linkerkantlijn nodigen je uit om een bepaald
commando te proberen.  Bijvoorbeeld:
<<Blank lines inserted around following line by help-with-tutorial>>
[Lege regels om didactische redenen.  Hieronder gaat het verder.]
>> Tik nu C-v (volgend scherm) om naar het volgende scherm te gaan.
	(Geef nu het commando door de CONTROL-toets ingedrukt te
	houden terwijl je de v tikt.)  Vanaf nu moet je dit steeds
	herhalen als je klaar bent met het lezen van een scherm.

Merk op dat er een overlapping van twee regels is als je van een
scherm naar het volgende gaat; dat zorgt voor continuïteit bij het
lezen van de tekst.

Het eerste wat je moet weten, is hoe je je naar verschillende plaatsen
in de tekst kan bewegen.  Je weet al hoe je een scherm vooruit moet
gaan: met C-v.  Om een scherm terug te gaan, tik je M-v (houd de
META-toets ingedrukt en tik v, of tik <ESC> v als je geen META-, EDIT-
of ALT-toets hebt).

>> Probeer nu een paar keer M-v, steeds gevolgd door C-v.


* SAMENVATTING
--------------

De volgende commando's zijn handig om volledige schermen te bekijken:

	C-v	Ga een scherm vooruit
	M-v	Ga een scherm terug
	C-l	Maak het scherm schoon en teken alle tekst opnieuw,
		 waarbij de regel waarop de cursor staat, op het
		 midden van het scherm terecht komt.  (C-l is
		 CONTROL-L, niet CONTROL-1.)

>> Kijk waar de cursor staat, en onthoud de tekst er omheen.  Tik C-l.
   Zoek de cursor en merk op dat hij nog steeds bij dezelfde tekst
   staat.

Als je toetsenbord PageUp- en PageDn-toetsen heeft dan kun je deze ook
gebruiken om een scherm terug dan wel vooruit te gaan, maar het werken
met C-v en M-v is efficiënter.


* BASISCOMMANDO'S CURSORBEWEGINGEN
----------------------------------

Het is handig om je per scherm te bewegen, maar hoe beweeg je je nu
naar een specifieke plaats op het scherm?

Er is een aantal manieren waarop je dit kan doen.  Je kan de
pijltjestoetsen gebruiken, maar het is efficiënter om je handen in de
standaardhouding te laten, en de commando's C-p, C-b, C-f en C-n te
gebruiken.  Elk van deze commando's verplaatst de cursor precies een
regel of teken in een bepaalde richting op het scherm.  Hier volgt een
figuur met de vier commando's en de richting waarin ze de cursor
bewegen:

			  vorige regel, C-p
				  :
				  :
    achteruit, C-b .... huidige cursorpositie .... vooruit, C-f
				  :
				  :
			 volgende regel, C-n

>> Verplaats, met C-n of C-p, de cursor naar de middelste regel van de
   figuur.  Tik dan C-l om de hele figuur in het midden van het
   centrum te plaatsen.

Met een beetje kennis van het Engels zijn deze commando's gemakkelijk
te onthouden: de p komt van "previous" (vorige), de n van "next"
(volgende), de b van "backward" (achteruit) en de f van "forward"
(vooruit).  Dit zijn de basiscommando's om de cursor te bewegen, dus
je zult ze VOORTDUREND gebruiken: het is vooruitziend als je ze nu
leert te gebruiken.

>> Tik een paar keer C-n om de cursor op deze regel te krijgen.

>> Beweeg je binnen de regel met C-f (herhaaldelijk) en terug omhoog
   met C-p.  Let op wat C-p doet als de cursor midden in een regel
   staat.

Elke regel eindigt met een Newline-teken (het Engelse "new line"
betekent "nieuwe regel"); dit teken scheidt elke regel van de
volgende.  De laatste regel in een bestand zou eigenlijk ook met een
Newline moeten eindigen (maar dat is niet noodzakelijk voor Emacs).

>> Probeer C-b aan het begin van een regel.  De cursor zal zich naar
   het eind van de vorige regel bewegen, omdat je achteruit over het
   Newline teken gaat.

Net als C-b kan ook C-f zich over Newline-tekens heen bewegen.

>> Tik nog een aantal keren het commando C-b, zodat je een gevoel
   krijgt waar de cursor is.  Tik dan enkele keren C-f om de cursor
   terug naar het einde van de regel te bewegen.  Een verder C-f
   commando beweegt de cursor dan naar de volgende regel.

Wanneer je de cursor voorbij het begin of het einde van het scherm
beweegt, zal de tekst over het scherm heen schuiven.  Dit heet
"scrollen", of "schuiven" in goed Nederlands.  Door te scrollen zorgt
Emacs ervoor dat de cursor de gewenste beweging kan maken zonder dat
de cursor van het scherm af beweegt.

>> Probeer de cursor voorbij de onderkant van het scherm te bewegen
   met C-n en zie wat er gebeurt.

Als de beweging per teken te langzaam gaat, kan je de cursor ook per
woord bewegen.  M-f (META-f) beweegt de cursor een woord vooruit en
M-b een woord achteruit.

>> Tik enkele keren M-f en M-b.

Als je midden in een woord staat, beweegt M-f de cursor naar het eind
van het woord.  Als je op een witte ruimte tussen twee woorden staat,
beweegt M-f de cursor naar het eind van het volgende woord.  Het
commando M-b beweegt de cursor analoog de andere kant op.

>> Tik enkele keren M-f en M-b en daar tussendoor een paar maal C-f en
   C-b, zodat je ziet wat M-f en M-b doen vanaf bepaalde plaatsen in
   een woord en tussen twee woorden.

Merk op dat er een analogie bestaat tussen enerzijds C-f en C-b en
anderzijds M-f en M-b.  Het is bij veel commando's zo dat META-tekens
gebruikt worden om iets te doen in eenheden van de taal (woorden,
zinnen, alinea's) terwijl CONTROL-tekens te maken hebben met dingen
die los staan van wat je aan het bewerken bent (tekens, regels, enz.).

Deze analogie gaat ook op voor regels en zinnen: C-a en C-e bewegen de
cursor naar het begin of eind van een regel, terwijl met M-a,
respectievelijk M-e, de cursor naar het begin, respectievelijk het
eind, van een zin gaat.

>> Tik enkele keren C-a, en dan een enkele keren C-e.
   Tik een paar maal M-a, en dan enkele keren M-e.

Bemerk hoe herhaalde C-a commando's niets doen, terwijl herhaalde M-a
commando's de cursor steeds een zin achteruit bewegen.  Alhoewel ze
niet volledig overeenkomen, is het gedrag van beide heel natuurlijk.

De plaats van de cursor in de tekst wordt "punt" genoemd (zonder
lidwoord, "point" in het Engels).  Anders gezegd: de cursor laat op
het scherm de plek zien waar punt in de tekst staat.

Nu volgt een samenvatting van eenvoudige cursorbewegingen, met
inbegrip van de commando's die de cursor per woord of zin bewegen:

	C-f	Ga een teken vooruit
	C-b	Ga een teken achteruit

	M-f	Ga een woord vooruit
	M-b	Ga een woord achteruit

	C-n	Ga naar de volgende regel
	C-p	Ga naar de vorige regel

	C-a	Ga naar het begin van de regel
	C-e	Ga naar het eind van de regel

	M-a	Ga terug naar het begin van de zin
	M-e	Ga vooruit naar het eind van de zin

>> Probeer al deze commando's een paar keer als oefening.  Deze
   commando's worden het vaakst gebruikt.

Er zijn nog twee belangrijke cursorbewegingen: M-< (META kleiner-dan)
beweegt de cursor naar het begin van het bestand, en M-> (META
groter-dan) beweegt hem naar het eind.

Op de meeste toetsenborden zit de '<' boven de komma, zodat je de
Shift-toets (ook wel bekend als de hoofdlettertoets) moet gebruiken om
het '<'-teken in te tikken.  Op deze toetsenborden moet je ook de
shift gebruiken om M-< in te tikken: zonder shift zou je M-, (META
komma) tikken.

>> Tik nu M-< om naar het begin van dit bestand te gaan.
   Gebruik daarna C-v om hier weer terug te komen.

>> Tik nu M-> om naar het eind van het bestand te springen.
   Gebruik daarna M-v om hier weer terug te komen.

Als je toetsenbord pijltjestoetsen heeft, kan je die ook gebruiken om
de cursor te verplaatsen.  We raden je aan om C-b, C-f, C-n en C-p te
leren, om drie redenen.  Ten eerste werken ze op alle toetsenborden,
ook die zonder pijltjestoetsen.  Ten tweede zul je merken dat wanneer
je eenmaal wat ervaring hebt opgedaan in de omgang met Emacs, het
gebruik van de CONTROL-tekens sneller is dan werken met de
pijltjestoetsen (omdat je handen in de normale tikpositie kunnen
blijven).  Ten derde, als je eenmaal gewend bent aan deze commando's
met CONTROL-tekens, kan je makkelijk andere gevorderde
cursorbewegingscommando's leren.

De meeste Emacs-commando's accepteren een numeriek argument.  Voor de
meeste commando's is dit argument het aantal keren dat het commando
herhaald moet worden.  Je geeft dit numerieke argument aan met C-u en
vervolgens de cijfers van het getal, vóór het commando.  Als je
toetsenbord een META- (of EDIT- of ALT-) toets heeft, is er ook een
andere manier om het getal aan te geven: tik de cijfers terwijl je de
META toets ingedrukt houdt.  We raden je aan de C-u manier te leren
omdat die beschikbaar is op elke terminal.

Bijvoorbeeld, C-u 8 C-f beweegt de cursor 8 plaatsen naar voren.

>> Probeer eens om met C-n of C-p en een numeriek argument de cursor
   met slechts een commando naar een regel in de buurt van deze zin te
   bewegen.

Voor de meeste commando's is het numerieke argument het aantal keren
dat het commando herhaald moet worden.  Voor sommige commando's
betekent het echter iets anders.  Verschillende commando's (die je
totnogtoe niet geleerd hebt) gebruiken het als een vlag -- de
aanwezigheid van een prefix-argument, ongeacht zijn waarde, maakt dat
het commando iets anders doet.

C-v en M-v vormen een andere uitzondering.  Met een numeriek argument
verschuiven deze commando's de tekst het aangegeven aantal regels in
plaats van (bijna) een heel scherm.  Bijvoorbeeld, C-u 4 C-v
verschuift de tekst 4 regels.

>> Probeer nu C-u 8 C-v.

Daarmee zou je tekst 8 regels opgeschoven moeten zijn.  Als je terug
omlaag wil scrollen, kan je M-v een argument geven.

Als je een scherm met vensters gebruikt, zoals X Windows of
MS-Windows, zou je een grote rechthoek moeten zien aan de linkerkant
van het Emacs-venster.  Deze rechthoek heet een schuifbalk
("scrollbar").  Je kan de tekst scrollen door met de muis in de
schuifbalk te klikken.

>> Klik met de middelste muisknop bovenaan het heldere gebied in de
   schuifbalk.  Dit zou de tekst moeten verschuiven naar een positie
   die afhankelijk is van hoe hoog of laag je klikt.

>> Beweeg de muis op en neer terwijl je de middelste muisknop
   ingedrukt houdt.  Je zal zien dat de tekst met de muis mee heen en
   weer scrollt.


* ALS EMACS HANGT
-----------------

Als Emacs niet meer op commando's reageert, kan je het veilig
onderbreken door C-g te tikken.  Je kan C-g gebruiken om een commando
te stoppen als het te lang duurt om uit te voeren.

Je kan C-g ook gebruiken om een numeriek argument te verwijderen of om
het begin van een commando dat je niet wilt afmaken, te verwijderen.

>> Tik nu C-u 100 om een numeriek argument te maken met de waarde 100,
   en tik dan C-g.  Tik vervolgens C-f.  Het zou de cursor maar één
   positie mogen verplaatsen, omdat je het argument verwijderd hebt
   met C-g.

Als je per ongeluk een <ESC> tikt, kan je dat ongedaan maken met het
commando C-g.


* UITGESCHAKELDE COMMANDO'S
---------------------------

Sommige Emacs-commando's zijn uitgeschakeld zodat beginnende
gebruikers ze niet per ongeluk kunnen uitvoeren.

Als je een van de uitgeschakelde commando's intikt, laat Emacs uitleg
zien over het commando dat je gegeven hebt, en vraagt of je het
werkelijk wil uitvoeren.

Wanneer je het commando echt wil uitvoeren, tik dan <SPC> (de
spatiebalk) als antwoord op de vraag.  Normaal wil je het commando
niet uitvoeren en beantwoord je de vraag met "n" (van "no" of "nee").

>> Tik C-x C-l (een uitgeschakeld commando), en tik dan n als antwoord
   op de vraag.


* VENSTERS
----------

Emacs kan meerdere vensters laten zien, elk venster met zijn eigen
tekst.  We zullen later uitleggen hoe je met meerdere vensters om kan
gaan.  Op dit moment willen we slechts uitleggen hoe je van extra
vensters af kunt komen en terug kan keren naar het werken met één
venster.  Het is eenvoudig:

	C-x 1	Een enkel venster (dat wil zeggen: verwijder alle
		andere vensters).

Het commando is CONTROL-x gevolgd door het cijfer 1.  C-x 1 vergroot
het venster waar de cursor in staat tot het hele scherm.  Alle andere
vensters worden verwijderd.

>> Zet de cursor op deze regel en tik C-u 0 C-l.
>> Tik nu C-h k C-f.
   Zie hoe dit venster kleiner is geworden, terwijl een nieuw venster
   verschijnt om de documentatie van het C-f commando te laten zien.

>> Tik nu C-x 1 en zie het documentatievenster verdwijnen.

Dit commando is anders dan de commando's die je tot nu toe geleerd
hebt aangezien het uit twee tekens bestaat.  Het begint met het teken
CONTROL-x.  Er zijn een heleboel commando's die met CONTROL-x
beginnen.  Veel van die commando's hebben te maken met vensters,
bestanden, buffers, en gelijkaardige dingen.  Dergelijke commando's
bestaan uit twee, drie of vier tekens.


* TOEVOEGEN EN WEGHALEN
-----------------------

Als je tekst toe wil voegen, tik je die eenvoudigweg in.  Tekens die
je kan zien, zoals A, 7, * en dergelijke, worden door Emacs als tekst
geïnterpreteerd en meteen aan de tekst toegevoegd.  Tik <Return> (de
"volgende regel"-toets) om een Newline toe te voegen en dus een nieuwe
regel te beginnen.

Je kan het laatste teken dat je hebt ingetikt weghalen door <Delback>
te tikken.  <Delback> is een toets op het toetsenbord -- dezelfde
toets die je normaal gesproken gebruikt, buiten Emacs, om het laatst
ingetikte teken te wissen.  Het is meestal een grote toets, een paar
rijen boven de <Return>-toets, waar "Delete", "Del" of "Backspace" op
staat.

Als er op die grote toets "Backspace" staat, dan is dat degene die je
gebruikt voor <Delback>.  Er kan op een andere plaats ook nog een
andere toets zijn waarop "Delete" staat, maar dat is niet <Delback>.

In het algemeen haalt <Delback> het teken weg dat juist voor de
cursorpositie staat.

>> Probeer dit nu: tik een paar letters en haal ze weer weg door een
   paar keer op <Delback> te drukken.  Maak je niet druk over het feit
   dat dit bestand verandert; je zal niets veranderen aan de originele
   versie van deze inleiding.  Je zit slechts je eigen kopie te
   wijzigen.

Als een regel tekst te lang wordt om helemaal op het scherm getoond te
worden, dan gaat hij verder op de volgende schermregel.  Een backslash
("\") in de rechtermarge (of, als je een scherm met vensters gebruikt,
een kleine gebogen pijl) laat dan zien dat de regel op de volgende
schermregel verder gaat.

>> Voeg nu tekst toe totdat je de rechter kantlijn raakt, en blijf
   toevoegen.  Je zal zien dat er een vervolgregel verschijnt.

>> Tik weer enkele keren <Delback> om zoveel tekens weg te halen tot
   de regel weer op een schermregel past.  De vervolgregel zal
   verdwijnen.

Je kan een Newline zoals elk ander teken verwijderen.  Als je een
Newline verwijdert, voeg je de twee regels waar de Newline tussen
staat samen tot een enkele regel.  Als de regel die het resultaat is
van deze operatie niet op een schermregel past, zal hij getoond worden
met een vervolgregel.

>> Beweeg de cursor naar het begin van een regel en tik <Delback>.
   Dit voegt de huidige en vorige regel samen.

>> Tik <Return> om de Newline die je net verwijderd hebt weer toe te
   voegen.

Je herinnert je dat je bij de meeste Emacs-commando's het aantal keren
op kan geven, dat ze herhaald moeten worden.  Dit geldt ook voor
gewone tekens.  Als je een gewoon teken herhaalt, wordt dat teken
herhaaldelijk toegevoegd.

>> Probeer dat nu: tik C-u 8 * om ******** toe te voegen.

Je hebt nu de eenvoudigste manier geleerd om iets in Emacs te tikken
en fouten te verbeteren.  Je kan tekst ook per woord of regel
verwijderen.  Hier volgt een samenvatting van de commando's om tekst
te verwijderen:

	<Delback>    Haal het teken weg dat voor de cursor staat
	C-d   	     Haal het teken weg dat achter de cursor staat

	M-<Delback>  Verwijder het woord dat voor de cursor staat
	M-d	     Verwijder het woord dat achter de cursor staat

	C-k	     Verwijder alles van de cursor tot het eind van de regel
	M-k	     Verwijder alles van de cursor tot het eind van de zin

Merk op dat <Delback> en C-d, met M-<Delback> en M-d de analogie
verder trekken, die begon met C-f en M-f (waarbij we voor het gemak
even vergeten dat <Delback> niet echt een CONTROL-teken is).  C-k en
M-k lijken enigzins op C-e en M-e in hun relatie tot regels en zinnen.

Je kunt ook op één uniforme manier een willekeurig deel van de tekst
verwijderen.  Beweeg daartoe naar één kant van het gedeelte dat je
wilt verwijderen en tik C-@ of C-<SPC>.  (<SPC> is de spatiebalk.)
Beweeg daarna naar de andere kant en tik C-w.  Dat verwijdert alle
tekst tussen de twee posities.

>> Beweeg de cursor naar de J aan het begin van de vorige alinea.
>> Tik C-<SPC>.  Emacs toont nu de mededeling "Mark set" ("Markering
   geplaatst") onderaan het scherm.
>> Plaats de cursor op de n van "kant" op de tweede regel van de
   alinea.
>> Tik C-w.  Dit zal de tekst vanaf de J tot vlak voor de n
   verwijderen.

Er is een verschil tussen iets weghalen en iets verwijderen: iets dat
je hebt verwijderd, kan je terugbrengen, maar iets dat je hebt
weggehaald niet.  (In het Engels is het verschil tussen "killing" en
"deleting" duidelijker dan tussen de Nederlandse vertalingen
"verwijderen" en "weghalen".)  Verwijderde tekst terughalen heet
"yanken".  In het algemeen geldt dat de commando's die meer tekst dan
een enkel teken, Newline of spatie verwijderen, deze tekst bewaren
zodat hij geyankt kan worden, terwijl dat niet geldt voor commando's
die slechts een enkel teken weghalen.

>> Zet de cursor op het begin van een regel die niet leeg is.
   Tik C-k om de tekst op die regel te verwijderen.
>> Tik C-k een tweede keer.  Nu verwijdert dit commando het
   Newline-teken.

Merk op hoe een enkel C-k commando de inhoud van een regel verwijdert,
een tweede C-k commando de regel zelf zodat alle volgende regels een
regel omhoog komen.  Het numerieke argument is voor C-k bijzonder: het
aangegeven aantal regels zal worden verwijderd, inclusief de inhoud.
Dit is meer dan simpelweg herhaling: C-u 2 C-k verwijdert twee regels,
terwijl tweemaal C-k tikken dat niet doet.

Om de laatst verwijderde tekst terug te halen naar de plaats waar de
cursor nu op staat (te yanken), tik C-y.

>> Probeer het nu: tik C-y om de tekst te yanken.

Het is alsof je met C-y iets uit de prullenbak haalt wat je net had
verwijderd.  Merk op dat verschillende C-k's achter elkaar alle regels
die verwijderd worden, bij elkaar bewaart zodat een enkele C-y die
regels in een keer terugbrengt.

>> Probeer het nu: tik C-k een paar keer.

Om de verwijderde tekst terug te halen:

>> Tik C-y.  Beweeg de cursor enkele regels naar beneden en tik weer
   C-y.  Je ziet nu hoe je tekst kan kopiëren.

Wat moet je doen als je wat tekst terug wilt brengen, maar je intussen
al iets anders verwijderd hebt?  C-y zou datgene terugbrengen wat je
het recentst hebt verwijderd.  Gelukkig is de voorgaande tekst niet
verloren gegaan.  Je kunt die tekst terughalen met M-y.  Nadat je C-y
hebt getikt om de recentst weggegooide tekst terug te halen, vervangt
M-y die tekst met de tekst die je daarvoor had weggegooid.  Je kunt
M-y herhalen om tekst terug te halen die je al langer geleden hebt
weggegooid.  Als je de tekst te pakken hebt die je zocht, hoef je
niets te doen om die daar te houden.  Je kan gewoon verder werken en
de teruggehaalde tekst met rust laten.

Als je M-y vaak genoeg tikt kom je terug waar je begon, bij de laatst
verwijderde tekst.

>> Verwijder een regel, beweeg de cursor wat, en verwijder nog een
   regel.  Tik C-y om de tweede regel die je verwijderde, terug te
   halen.  Tik nog een M-y en die regel wordt vervangen door de eerste
   regel die je verwijderde.  Tik nog enkele keren M-y en zie wat er
   langs komt.  Herhaal dit tot de tweede regel weer langs komt, en
   dan nog een paar keer.  Je kan ook experimenteren met positieve en
   negatieve argumenten bij M-y.


* HERSTELLEN
------------

Als je de tekst veranderd hebt en je daar toch niet tevreden mee bent,
dan kan je de verandering ongedaan maken met het herstelcommando, C-x
u.

Normaal gesproken herstelt C-x u de veranderingen die het gevolg zijn
van een enkel commando; door herhaaldelijk C-x u te tikken, worden
steeds eerdere commando's hersteld.

Er zijn echter twee uitzonderingen: commando's die de tekst niet
wijzigen, zoals cursorbewegingen, worden overgeslagen, en commando's
die simpelweg het ingetikte teken aan de tekst toevoegen, worden
meestal gegroepeerd in groepjes van maximaal 20 tekens, zodat je
minder vaak het commando C-x u hoeft te tikken om teksttoevoegingen te
herstellen.

>> Gooi deze regel weg met C-k; met C-x u zou hij weer moeten
   verschijnen.

C-_ is een alternatief voor C-x u.  Het levert exact hetzelfde
resultaat op, maar is makkelijker om een paar keer achter elkaar te
tikken.  Een nadeel van C-_ is dat op sommige toetsenborden het
intikken ervan niet gebruiksvriendelijk is.  Dat is ook de reden voor
het alternatief, C-x u.  Op sommige terminals kan je C-_ tikken door
"/" te tikken terwijl je de CONTROL-toets ingedrukt houdt.

Een numeriek argument bij C-_ of C-x u duidt het aantal herhalingen
aan.


* BESTANDEN
-----------

Om een tekst die je gemaakt of veranderd hebt op te slaan, moet je de
tekst in een bestand stoppen ("to save a file" in het Engels).  Als je
dat niet doet, ben je die veranderingen kwijt op het moment dat je
Emacs verlaat.  Je kan een bestand veranderen door het bestand te
"bezoeken".  (Ook wel "vinden"; "finding" of "visiting" in het
Engels.)

Een bestand bezoeken betekent dat je de inhoud van dat bestand in
Emacs ziet.  Het lijkt er dan op alsof je het bestand aan het
veranderen bent.  Deze veranderingen zijn echter slechts tijdelijk
zolang je het bestand niet opslaat.  Op deze manier kan je nooit per
ongeluk een half gewijzigd bestand op het systeem achterlaten.  Zelfs
als je het bestand opslaat, zorgt Emacs ervoor dat het originele
bestand onder een gewijzigde naam nog steeds beschikbaar is, voor het
geval je later besluit dat de veranderingen toch niet zo goed waren.

Bij de onderkant van het scherm zie je een regel die begint en eindigt
met streepjes, met aan het begin "-1:-- TUTORIAL.nl" of iets
dergelijks.  Dit deel van het scherm laat normaal de naam van het
bestand zien dat je op dat moment bezoekt.  Op dit moment bezoek je
een bestand dat "TUTORIAL.nl" heet; het is je eigen kopie van de
Nederlandstalige Emacs-inleiding ("tutorial" in het Engels).  Als je
in Emacs een bestand bezoekt dan staat de naam van het bestand altijd
op deze plaats.

Iets bijzonders aan het commando om een bestand te bezoeken, is dat je
aan moet geven welk bestand je wil.  Dit heet dat het commando "een
argument van de gebruiker vraagt"; in dit geval de naam van het
bestand.  Nadat je het commando

	C-x C-f		Bezoek bestand (met de f van "find file").

hebt getikt vraagt Emacs om de naam van het bestand.  De naam die je
intikt verschijnt op de onderste regel van het scherm.  Wanneer die
regel voor dit soort invoer gebruikt wordt, heet hij de minibuffer.
Je kan gewone Emacs commando's gebruiken om de bestandsnaam te
veranderen.

Tijdens het invoeren van de bestandsnaam (of om het even welke invoer
in de minibuffer) kan je het commando afbreken met C-g.

>> Tik C-x C-f gevolgd door C-g.  Dit commando breekt de minibuffer af
   en ook het C-x C-f commando dat van de minibuffer gebruik maakte.
   Het resultaat is dat je geen bestand bezoekt.

Als je de naam van een bestand hebt ingevoerd, tik dan <Return> om het
commando af te sluiten.  Hierna gaat het C-x C-f commando aan het werk
en haalt het bestand op dat je aangegeven hebt.  Als het C-x C-f
commando daarmee klaar is, verdwijnt de minibuffer.

Na korte tijd verschijnt de inhoud van het bestand op het scherm en
kan je de inhoud wijzigen.  Als je de wijzigingen op wilt slaan, tik
dan het commando

	C-x C-s   Sla bestand op (met de s van "save file").

Dit commando slaat de tekst zoals Emacs die nu heeft in het bestand
op.  De eerste keer dat je dit doet, slaat Emacs het originele bestand
onder een andere naam op, zodat het niet verloren gaat.  De nieuwe
naam bestaat uit de oude bestandsnaam gevolgd door een "~".

Als Emacs het bestand heeft opgeslagen, laat het de naam van het
bestand zien.  Het is een goede gewoonte een bestand regelmatig op te
slaan zodat er niet teveel werk verloren gaat als het systeem hangt of
crasht.

>> Tik C-x C-s, om je kopie van deze inleiding op te slaan.  Als het
   goed is verschijnt "Wrote ...TUTORIAL.nl" op de onderste
   schermregel.

OPMERKING: Op sommige systemen gebeurt er helemaal niets als je C-x
C-s tikt, en daarna ook niets meer.  Dit komt door een eigenschap van
de machine waarop je werkt die te maken heeft met "flow control".  Met
C-s stopt de "flow" en komt niets meer van wat je tikt bij Emacs
terecht.  Om deze situatie te herstellen, tik C-q.  Lees daarna het
hoofdstuk "Spontaneous Entry to Incremental Search" in het
Emacs-handboek over hoe je moet omgaan met deze situatie.

Je kan een bestaand bestand bezoeken om het te bekijken of het te
wijzigen.  Je kan ook een bestand bezoeken dat nog niet bestaat.  Dit
is de manier om met Emacs een nieuw bestand te maken: bezoek het
bestand, dat eerst leeg zal zijn, en voeg tekst toe.  Zodra je de
tekst opslaat, wordt het bestand werkelijk gecreëerd, met de tekst als
inhoud.  Vanaf dat moment ben je dus bezig met een bestaand bestand.


* BUFFERS
---------

Als je een tweede bestand bezoekt met C-x C-f, blijft het eerste
bestand gewoon in Emacs.  Je kan naar dat bestand terug door het
gewoon nog een keer te bezoeken met C-x C-f.  Op deze manier kan je
een behoorlijk aantal bestanden in Emacs krijgen.

>> Creëer een bestand dat "foo" heet door te tikken: C-x C-f foo
   <Return>.  Voeg hieraan wat tekst toe, wijzig hem, en sla "foo" op
   door C-x C-s te tikken.  Tik hierna C-x C-f TUTORIAL <Return> om
   weer hier, in de inleiding, terug te komen.

Emacs bewaart intern de tekst van elk bestand in een ding dat een
"buffer" genoemd wordt.  Als je een bestand bezoekt wordt er een
nieuwe buffer gemaakt.  Om een lijst van de huidige buffers te zien,
tik

	C-x C-b   Laat de bufferlijst zien

>> Probeer C-x C-b nu.

Bemerk dat elke buffer een naam heeft en mogelijk ook een
bestandsnaam; dit is de naam van het bestand waarmee de buffer
overeenkomt.  ALLE tekst die je in een Emacs venster ziet is altijd
onderdeel van een of andere buffer.

>> Tik C-x 1 om de bufferlijst te verwijderen.

Wanneer je met meerdere buffers werkt, dan is op elk moment slechts
één van die buffers "actueel".  De actuele buffer is degene die je aan
het bewerken bent.  Als je een andere buffer wilt bewerken, dan moet
je daarnaar "omschakelen".  Als je wilt omschakelen naar een buffer
die overeenkomt met een bestand, dan kun je dit doen door dat bestand
opnieuw te bezoeken met C-x C-f.  Er is ook een makkelijkere manier:
gebruik het commando C-x b.  Dit commando vraagt je naar de naam van
de buffer.

>> Tik C-x b foo <Return> om terug te gaan naar de buffer "foo" die de
   tekst van het bestand "foo" bevat.  Tik vervolgens C-x b TUTORIAL
   <Return> om terug te komen naar deze Emacs-inleiding.

Meestal is de naam van de buffer gelijk aan de naam van het bestand
(minus de naam van de directory).  Dit klopt echter niet altijd.  De
lijst met buffers die je maakt met C-x C-b laat je altijd de naam van
elke buffer zien.

ALLE tekst die je ziet in een venster van Emacs is altijd onderdeel
van een of andere buffer.  Sommige buffers komen niet overeen met een
bestand.  De buffer genaamd "*Buffer List*" heeft bijvoorbeeld geen
bijbehorend bestand.  Deze buffer bevat de lijst met buffers die je
gemaakt hebt met C-x C-b.  Ook de buffer "*Messages*" heeft geen
geassocieerd bestand; deze buffer bevat de mededelingen die Emacs je
op de onderste regel toonde.

>> Tik C-x b *Messages* <Return> om de buffer met mededelingen te
   bekijken.  Tik daarna weer C-x b TUTORIAL <Return> om terug te
   keren naar deze buffer met de Emacs-inleiding

Als je de tekst van het ene bestand verandert en dan een ander bestand
bezoekt, wordt het eerste bestand niet opgeslagen.  De wijzigingen
blijven in Emacs, in de buffer die bij het bestand hoort.  Het creëren
of veranderen van de buffer van het tweede bestand heeft geen effect
op de eerste buffer.  Dit is erg nuttig, maar betekent ook dat er een
eenvoudige manier nodig is om het eerste bestand te bewaren.  Het zou
erg vervelend zijn om er eerst naar terug te moeten gaan met C-x C-f
om het dan te kunnen bewaren met C-x C-s.  Dus hebben we het commando:

	C-x s	  Sla een paar buffers op

C-x s vraagt voor elke buffer die veranderingen heeft die nog niet
opgeslagen zijn, of je de buffer wilt bewaren.

>> Voeg wat tekst toe en tik C-x s.
   Emacs vraagt nu of je de buffer die TUTORIAL.nl heet wilt bewaren.
   Beantwoord deze vraag positief door een "y" in te tikken (de y van
   "yes", Engels voor "ja").


* UITGEBREIDE COMMANDO'S
------------------------

Er zijn veel meer Emacs commando's dan er op de toetsen van het
toetsenbord passen, zelfs als we hun aantal kunnen vergroten door de
CONTROL- of META-toets te gebruiken.  Emacs lost dit probleem op met
het X commando (met de X van eXtensie of uitbreiding).  Het X commando
komt voor in twee smaken:

	C-x	Tekenuitbreiding.  Gevolgd door een teken.
	M-x	Commando-naam-uitbreiding.  Wordt gevolgd door een naam.

Deze commando's zijn in het algemeen nuttig, maar worden minder
gebruikt dan de commando's die je tot nu toe al geleerd hebt.  Je hebt
al enkele van deze commando's gezien: C-x C-f om een bestand te
bezoeken en C-x C-s om het te bewaren, bijvoorbeeld.  Een ander
voorbeeld is het commando om Emacs te verlaten: dit is C-x C-c.  (Maak
je geen zorgen over het verloren gaan van veranderingen die niet
opgeslagen zijn; C-x C-c vraagt of je veranderde buffers wilt bewaren
voordat Emacs helemaal eindigt.)

C-z is het commando om Emacs *tijdelijk* te verlaten, zodat je daarna
weer terug kan keren in dezelfde Emacs-sessie.

Op systemen die deze mogelijkheid bieden, zet C-z Emacs stil: je komt
weer terug in de shell, maar Emacs is nog aanwezig.  In de meeste
shells kan je Emacs weer activeren met het "fg" commando, of met
"%emacs".

Op systemen die niet de mogelijkheid bieden om programma's stil te
zetten, creëert C-z een subshell onder Emacs om je zo in de
gelegenheid te stellen andere programma's uit te voeren en daarna weer
in Emacs terug te keren; Emacs wordt dus niet werkelijk verlaten.  In
dit geval is het shellcommando "exit" de normale manier om de subshell
te verlaten en in Emacs terug te keren.

Het moment om C-x C-c te gebruiken is wanneer je uit gaat loggen.  Het
is ook het juiste commando om Emacs te beëindigen wanneer Emacs
opgestart was door een mail-programma of iets dergelijks, aangezien
die misschien niet met een stilgezette Emacs om kunnen gaan.  Normaal
gezien is het echter beter Emacs stil te zetten met C-z dan om Emacs
te verlaten, behalve als je uit wilt loggen natuurlijk.

Er bestaan vele C-x commando's.  Hier is een lijst van degene die je
nu al kent:

	C-x C-f		Bezoek bestand
	C-x C-s		Sla bestand op
	C-x s		Sla een paar buffers op
	C-x C-b		Laat bufferlijst zien
	C-x b		Schakel naar een buffer
	C-x C-c		Verlaat Emacs
	C-x 1		Een enkel venster
	C-x u		Herstel

Commando-naam-bevelen worden nog minder vaak gebruikt, of alleen onder
bepaalde omstandigheden.  Een voorbeeld is het commando
replace-string, dat in de hele tekst een string vervangt door een
andere string ("to replace" betekent "vervangen").  Als je M-x tikt,
toont Emacs onderaan het scherm "M-x" en moet je de naam van het
commando intikken, in dit geval "replace-string".  Als je gewoon
"repl s<TAB>" tikt maakt Emacs de naam zelf af.  Beëindig het commando
met <Return>.

Het replace-string commando heeft twee argumenten nodig: de string die
vervangen moet worden en de string waarmee die vervangen moet worden.
Je sluit elk argument af met <Return>.

>> Plaats de cursor op de lege regel twee regels onder deze regel.
   Tik dan M-x repl s<Return>gewijzigd<Return>veranderd<Return>.

   Zie hoe deze regel daardoor gewijzigd is.  Je hebt elk voorkomen
   van het woord g-e-w-i-j-z-i-g-d vervangen door "veranderd"; te
   beginnen op de plek waar de cursor staat.


* AUTOMATISCH BEWAREN
---------------------

Als je een bestand veranderd hebt maar het nog niet opgeslagen hebt,
zouden de veranderingen verloren kunnen gaan als het systeem zou
hangen of herstarten.  Om je hiertegen te beschermen, slaat Emacs
regelmatig de veranderde tekst automatisch op.  De naam van het
bestand waarin de tekst automatisch wordt opgeslagen begint en eindigt
met een #.  Bijvoorbeeld, als je het bestand "hello.c" aan het
bewerken bent, wordt de tekst automatisch opgeslagen in een bestand
dat "#hello.c#" heet.  Zodra je het bestand werkelijk opslaat, wordt
het automatisch opgeslagen bestand verwijderd.

Als de computer crasht, kan je de automatisch opgeslagen tekst
terugkrijgen door het bestand gewoon te bezoeken (het originele
bestand, niet het automatisch opgeslagen), gevolgd door M-x
recover-file<Return>.  Als Emacs vraagt om bevestiging, antwoord dan
met yes<Return> en de automatisch opgeslagen informatie wordt
teruggehaald.


* ECHO-GEBIED
-------------

Als je een commando langzaam intikt, toont Emacs de tekens aan de
onderkant van het scherm in een deel dat het "echo-gebied" genoemd
wordt.  Dit gebied omvat de onderste regel van het scherm.


* MODUS-REGEL
-------------

De regel direct boven het echo gebied heet de "modusregel".  De
modusregel ziet er ongeveer zo uit:

-1:**  TUTORIAL.nl    62% L763    (Fundamental)-----------------------

Deze regel geeft interessante informatie over Emacs en de tekst die je
aan het bewerken bent.

Je weet al wat de bestandsnaam betekent: het is de naam van het
bestand dat je bezoekt.  NN% geeft je huidige positie in de tekst aan:
NN procent van de tekst bevindt zich boven het scherm.  Als het
bestand vanaf het begin op het scherm staat, staat er "Top" in plaats
van " 0%".  Als het laatste stuk tekst op het scherm staat, zal er
"Bot" staan (van "bottom", "onderkant" in het Nederlands).  Als de
tekst zo klein is dat hij volledig op het scherm past staat "All" in
de modus-regel.

De L gevolgd door een getal geeft het nummer van de regel waarin punt
zich bevindt.

De sterretjes aan het begin betekenen dat je de tekst veranderd hebt.
Direct na het bezoeken of opslaan staan er gewoon streepjes.

In de modusregel staat tussen haakjes in welke modus je aan het werken
bent.  De standaardmodus is de "Fundamental" modus, die je nu gebruikt
("fundamental" is "basis" in het Nederlands).  Een dergelijke modus
heet een hoofdmodus ("major mode" in het Engels).

Emacs heeft verschillende hoofdmodi.  Sommige daarvan zijn bedoeld
voor het bewerken van verschillende talen of soorten tekst, zoals
bijvoorbeeld Lisp modus, Text modus, etc.  Op elk moment is er altijd
precies een modus actief, en de naam daarvan staat in de modusregel,
op de plaats waar nu "Fundamental" staat.

Elke hoofdmodus zorgt ervoor dat sommige commando's zich anders
gedragen.  Zo bestaat er een commando om een commentaar in een
programma te tikken, en aangezien elke programmeertaal een ander idee
heeft over hoe commentaar eruit moet zien, moet elke hoofdmodus op een
andere manier het commentaar beginnen.  Elke hoofdmodus is de naam van
een uitgebreid commando, en met dat commando schakel je om naar die
hoofdmodus.  Zo is bijvoorbeeld M-x fundamental-mode het commando om
naar de basismodus om te schakelen.

Als je Nederlandse of Engelse tekst wil gaan bewerken, zoals
bijvoorbeeld dit bestand, kan je beter "Text mode" gebruiken, de modus
om tekst in een gewone taal te bewerken:

>> Tik M-x text-mode<Return>.

Wees gerust; geen van de commando's die je geleerd hebt zorgen voor
grondige veranderingen in Emacs.  Een van de dingen die je kan merken,
is bijvoorbeeld dat M-f en M-b nu apostrofs als onderdeel van een
woord beschouwen.  In de vorige modus (Fundamental) behandelen M-f en
M-b de apostrof als ruimte tussen twee woorden.

Het is gebruikelijk dat hoofdmodi dergelijke subtiele verschillen
hebben.  De meeste commando's doen dus min of meer hetzelfde in elke
hoofdmodus.

Met het commando C-h m kan je de documentatie over de huidige
hoofdmodus lezen.

>> Gebruik C-u C-v een paar keer om deze zin in de buurt van de
   bovenkant van het scherm te krijgen.
>> Tik C-h m om te zien hoe de tekstmodus verschilt van de basismodus.
>> Tik C-x 1 om de documentatie van het scherm te verwijderen.

Hoofdmodi heten zo omdat er ook bijmodi zijn.  Bijmodi zijn geen
alternatieven voor hoofdmodi; het zijn slechts kleine aanpassingen
daarvan.  Elke bijmodus kan aan- of uitgezet worden, onafhankelijk van
andere bijmodi en onafhankelijk van de hoofdmodus.  Het is dus
mogelijk geen bijmodi, één bijmodus of een willekeurige combinatie van
bijmodi te gebruiken.

Een nuttige bijmodus voor het bewerken van tekst in een natuurlijke
taal, zoals het Nederlands, is Auto Fill modus ("auto fill" betekent
automatisch uitvullen).  Wanneer deze modus aanstaat, breekt Emacs
automatisch een regel tussen twee woorden af als de regel te lang
wordt.

Je kan Auto Fill modus aanzetten met M-x auto-fill-mode<Return>.  Als
deze modus al aanstaat, kan je hem uitzetten met M-x
auto-fill-mode<Return>.  Als de modus uitstaat, zet dit commando de
modus aan; als ze aanstaat, zet dit commando de modus uit.  We zeggen
dat het commando de modus "schakelt" ("to toggle" in het Engels).

>> Tik nu M-x auto-fill-mode<Return>.  Tik nu vele malen "asdf " op
   een regel zodat je kan zien dat de regel in tweeën gesplitst wordt.
   Er moeten wel spaties tussen de woorden staan, omdat de Auto Fill
   modus de regel alleen op spaties breekt.

De rechterkantlijn staat meestal op 70 tekens, maar die kan je
veranderen met het C-x f commando.  Dit commando accepteert een
numeriek argument om de gewenste kantlijn te verkrijgen.

>> Tik C-x f met 20 als argument (C-u 20 C-x f).
   Tik wat tekst en zie dat Emacs de regels afbreekt bij 20 tekens.
   Zet de kantlijn nu terug op 70, dus met met C-u 70 C-x f.

Als je de tekst midden in een regel verandert vult Auto Fill modus de
regel niet opnieuw.
Om een alinea opnieuw te vullen, tik M-q (META-q) terwijl de cursor in
de alinea staat.

>> Plaats de cursor in de voorgaande alinea en tik M-q.


* ZOEKEN
--------

Emacs kan tekenreeksen ("strings") zoeken, zowel volgend op de
cursorpositie, als eraan voorafgaand.  Het zoeken naar een string
verplaatst de cursor naar de volgende plaats waar de gezochte string
voorkomt.

Het zoekcommando van Emacs is anders dan de zoekcommando's van de
meeste tekstverwerkers; het zoekt incrementeel.  Dit betekent dat het
zoeken gebeurt tijdens het intikken van de gezochte string.

Het commando om het voorwaarts zoeken te starten is C-s (met de "s"
van "to search", zoeken); C-r start het achterwaarts zoeken (met de
"r" van "reverse" of achteruit).  MAAR WACHT!  Probeer ze nu nog niet.

Als je C-s tikt verschijnt de string "I-search" in het echo-gebied.
Dit betekent dat Emacs bezig is met een "incremental search"
(incrementele zoekopdracht) en wacht op het intikken van de
zoekstring.  <Return> beëindigt het zoeken.

>> Tik nu C-s om het zoeken te starten.  Tik nu, LANGZAAM, één letter
   per keer, het woord "cursor", met een pauze na elke letter zodat je
   kan zien wat er met de cursor gebeurt.  Je hebt nu eenmaal naar het
   woord "cursor" gezocht.
>> Tik nogmaals C-s, om naar het volgende voorkomen van het woord
   "cursor" te zoeken.
>> Tik nu viermaal <Delback> en let op de cursorbewegingen.
>> Tik <Return> om het zoeken te beëindigen.

Zag je wat er gebeurde?  Tijdens incrementeel zoeken probeert Emacs
naar de eerste plek te gaan waar de string staat die je tot dan toe
getikt hebt.  Om naar de volgende plek te gaan, tik je C-s nog een
keer.  Als er geen volgende plek is gevonden, biept Emacs en vertelt
je dat de zoekopdracht niets gevonden heeft ("failing" in het Engels).
C-g zou het zoeken ook afbreken.

OPMERKING: Op sommige systemen gebeurt er helemaal niets als je C-s
tikt, en daarna ook niets meer.  Dit komt door een eigenschap van de
machine waarop je werkt die te maken heeft met "flow control".  Met
C-s stopt de "flow" en komt niets meer van wat je tikt bij Emacs
terecht.  Om deze situatie te herstellen, tik C-q.  Lees daarna het
hoofdstuk "Spontaneous Entry to Incremental Search" in het
Emacs-handboek over hoe je moet omgaan met deze situatie.

Als je tijdens incrementeel zoeken <Delback> tikt, zal je zien dat het
laatste teken dat je aan de zoekstring toegevoegd hebt, weggehaald
wordt en dat het zoeken teruggaat naar de voorgaande plaats.  Als je
bijvoorbeeld begint met zoeken en je tikt een "c", dan ga je naar de
plaats waar de "c" het eerst voorkomt.  Tik je vervolgens een "u", dan
gaat de cursor naar de plaats waar de string "cu" het eerst voorkomt.
Als je nu <Delback> tikt, dan wordt de "u" van de zoekstring
afgehaald, en gaat de cursor terug naar de plaats waar hij stond
voordat je de "u" intikte, namelijk daar waar "c" het eerst voorkwam.

Als je tijdens een zoekoperatie een CONTROL- of META-teken intikt, dan
wordt het zoeken beëindigd.  Er zijn een paar uitzonderingen, namelijk
tekens die tijdens zoeken een speciale betekenis hebben, zoals C-s en
C-r.

Met C-s begin je te zoeken naar de plaats waar de zoekstring voor het
eerst voorkomt NA de huidige cursorpositie.  Als je iets wilt zoeken
dat eerder in de tekst moet voorkomen, gebruik dan C-r in plaats van
C-s.  Alles wat we nu weten over C-s geldt ook voor C-r, alleen is de
zoekrichting omgedraaid.


* MEERDERE VENSTERS
-------------------

Een van Emacs' aardige eigenschappen is dat je meerdere vensters op
het scherm kan laten zien.

>> Zet de cursor op deze regel en tik C-u 0 C-l.

>> Tik C-x 2 om het scherm in twee vensters op te splitsen.
   Beide vensters laten deze inleiding zien; de cursor blijft in het
   bovenste venster.

>> Tik C-M-v om de tekst in het onderste venster te verschuiven.
   (Als je geen META-toets hebt, tik dan <ESC> C-v.)

>> Tik C-x o (met de o van "other"; "ander" in het Nederlands) om de
   cursor naar het andere venster te verplaatsen.

>> Verschuif de tekst in het onderste venster, met C-v en M-v.
   Zorg ervoor dat je deze inleiding in het bovenste venster leest.

>> Tik weer C-x o om de cursor weer in het bovenste venster te zetten.
   De cursor staat weer precies op de plaats waar hij stond toen je
   het venster verliet.

Je kan C-x o blijven gebruiken om van venster naar venster te gaan.
Elk venster heeft zijn eigen cursorpositie; de cursor is altijd enkel
zichtbaar in een daarvan.  Alle normale commando's hebben betrekking
op het venster waarin de cursor staat.  Dit venster is het
"geselecteerde venster" ("selected window" in het Engels).

Het C-M-v commando is erg nuttig wanneer je tekst aan het bewerken
bent in het ene venster, terwijl je het andere venster als referentie
gebruikt.  Je kan de cursor dan altijd in het venster houden waarin je
bezig bent, terwijl je met C-M-v door de tekst in het andere venster
loopt.

C-M-v is een voorbeeld van een CONTROL-META teken.  Als je een echte
META-toets hebt kan je C-M-v intikken door zowel CONTROL als META
ingedrukt te houden terwijl je v tikt.  Het maakt niet uit in welke
volgorde je CONTROL en META indrukt; het gaat erom welke toetsen
ingedrukt zijn terwijl je tikt.

Als je geen echte META-toets hebt kan je <ESC> gebruiken; de volgorde
is dan wel belangrijk.  Je moet dan eerst <ESC> tikken, gevolgd door
CONTROL-v; CONTROL-<ESC> v zal niet werken.  Dit komt doordat <ESC>
zelf een teken is, terwijl CONTROL en META dat niet zijn.

>> Tik C-x 1 (in het bovenste venster) om het onderste venster te
   laten verdwijnen.

(Als je C-x 1 tikt in het onderste venster laat je het bovenste
verdwijnen.  C-x 1 betekent zoveel als "ik wil maar 1 venster, en wel
dat venster waar de cursor nu in staat.")

Je hoeft niet dezelfde buffer in beide vensters te hebben.  Wanneer je
C-x C-f gebruikt om een bestand in één van de vensters te bezoeken,
zal het andere venster niet veranderen.  Je kunt de vensters
onafhankelijk van elkaar gebruiken om bestanden te bezoeken.

Hier is nog een manier om twee venster te krijgen die elk een andere
tekst laten zien:

>> Tik C-x 4 C-f gevolgd door de naam van een van je bestanden,
   gevolgd door <Return>.  Het opgegeven bestand zal in het onderste
   venster verschijnen, en de cursor zal in dat venster staan.

>> Tik C-x o om terug naar het bovenste venster te gaan, en C-x 1 om
   het onderste venster te laten verdwijnen.


* RECURSIEVE BEWERKINGSNIVEAUS
------------------------------

Soms kom je in Emacs in een recursief bewerkingsniveau terecht
(Engels: "recursive editing level").  Dit is te zien in de modusregel
aan de vierkante haken die om de haakjes van de naam van de hoofdmodus
staan.  Dan staat er bijvoorbeeld [(Fundamental)] in plaats van
(Fundamental).

Tik <ESC> <ESC> <ESC> Om uit een recursief bewerkingsniveau te komen.
Dit is een algemeen "ontsnappingscommando".  Je kan het ook gebruiken
om extra vensters te verwijderen of om uit de minibuffer te komen.

>> Tik M-x om in een minibuffer te komen, en tik dan <ESC> <ESC> <ESC>
   om er weer uit te komen.

C-g is niet bruikbaar om uit een recursief bewerkingsniveau te komen.
De reden hiervoor is dat C-g gebruikt wordt om commando's af te breken
BINNEN het recursieve bewerkingsniveau.


* MEER INFORMATIE
-----------------

We hebben geprobeerd je met deze inleiding precies genoeg informatie
te leveren om met Emacs te beginnen werken.  De mogelijkheden van
Emacs zijn zo groot dat het onmogelijk is nu alles uit te leggen.  Het
kan zijn dat je meer over Emacs wil leren omdat het zoveel nuttige
mogelijkheden heeft.  Emacs heeft commando's om documentatie te laten
zien over Emacs commando's.  Deze "helpcommando's" beginnen allemaal
met C-h: "het Hulpteken".

Om hulp te krijgen tik je C-h, gevolgd door een teken om aan te duiden
welke hulp je wilt.  Als je het echt niet meer weet, tik C-h ? en
Emacs vertelt welke hulp het allemaal te bieden heeft.  Als je C-h
hebt getikt maar van gedachten veranderd bent, tik je gewoon C-g om
het af te breken.

(In sommige installaties wordt de betekenis van C-h veranderd.  Dat is
geen goed idee, zeker als die verandering op alle gebruikers invloed
heeft, en is een geldige reden om je beklag te doen bij de
systeembeheerder of de helpdesk.  Als C-h intussen niet een bericht
onderaan het scherm laat zien over mogelijke hulp, probeer dan de F1
toets (functietoets 1) of gebruik M-x help <Return>.)

De eenvoudigste hulp is C-h c.  Tik C-h, het teken "c" en een teken of
uitgebreid commando, en Emacs laat een zeer korte beschrijving van het
commando zien.

>> Tik C-h c C-p.

De beschrijving die getoond wordt, zou zoiets moeten zijn als:

	C-p runs the command previous-line

   (Nederlands: C-p voert het commando previous-line uit.)

Dit commando vertelt je "de naam van de functie".  Functies worden
vooral gebruikt om Emacs uit te breiden of aan de wensen van de
gebruiker aan te passen.  Aangezien functienamen gekozen zijn om aan
te geven wat de functie doet, zijn ze ook geschikt als heel korte
documentatie; genoeg om je te herinneren aan wat de commando's die je
al geleerd hebt betekenen.

Uitgebreide commando's zoals C-x C-s en (als je geen META-, EDIT- of
ALT-toets hebt) <ESC> v kunnen ook getikt worden na C-h c.

Om meer informatie over een commando te krijgen, tik C-h k in plaats
van C-h c.

>> Tik C-h k C-p.

Dit laat de documentatie van de functie, inclusief de naam van de
functie, in een apart venster zien.  Als je klaar bent met lezen, tik
C-x 1 om van dat venster af te komen.  Je hoeft dat natuurlijk niet
meteen te doen.  Je kan ook eerst wat anders doen voordat je C-x 1
tikt.

Hier zijn nog wat nuttige mogelijkheden van C-h:

   C-h f	Beschrijf een functie.  Je moet de naam van de functie
		intikken.

>> Tik C-h f previous-line<Return>
   Dit laat alle informatie zien die Emacs heeft over de functie die
   het C-p commando implementeert.

Een vergelijkbaar commando C-h v toont de documentatie van variabelen
die je kunt instellen om het gedrag van Emacs naar wens aan te passen.
Het commando vraagt je om de naam van een variabele.

   C-h a	Commando Apropos.  Tik een woord in en Emacs zal een
		lijst van alle commando's laten zien waarin dat woord
		voorkomt.  Al deze commando's kunnen aangeroepen
		worden met M-x.  Bij sommige commando's staat met
		welke tekens dit commando direct uitgevoerd kan
		worden.

>> Tik C-h a file<Return>.

Dit laat in een ander venster alle M-x commando's zien met "file" in
hun naam.  Je zal teken-commando's zien als C-x C-f naast de
overeenkomende commandonaam zoals find-file.

>> Tik C-M-v herhaaldelijk om de tekst in het hulpvenster te
   verschuiven.

>> Tik C-x 1 om het hulpvenster te verwijderen.

   C-h i	Lees de online handleidingen (ook wel Info genoemd).
		Dit commando zet je in een speciale buffer genaamd
		"*info*" waar je online handleidingen kunt lezen van
		software die op je computer is geïnstalleerd.  Tik m
		Emacs <Return> om de handleiding van Emacs te lezen.
		Als je nog nooit Info hebt gebruikt dan kun je ?
		tikken zodat Emacs je een rondleiding geeft langs de
		mogelijkheden van het Info systeem.  Wanneer je klaar
		bent met deze Emacs-inleiding dan kun je de
		Emacs-Info-handleiding gebruiken als je primaire bron
		van informatie.


* MEER MOGELIJKHEDEN
--------------------

Je kunt meer over Emacs leren door haar handleiding te lezen.  Deze is
zowel als boek als in elektronische vorm via Info beschikbaar (gebruik
het Help menu of tik <F10> h r).  Kijk bijvoorbeeld eens naar
"completion", hetgeen minder tikwerk oplevert, of "dired" wat het
omgaan met bestanden vereenvoudigt.

"Completion" (of "afmaken", in het Nederlands) is een manier om
onnodig tikwerk te voorkomen.  Als je bijvoorbeeld naar de
"*Messages*" buffer wilt omschakelen, dan kun je C-x b *M<Tab> tikken
en dan zal Emacs de rest van de buffernaam invullen voor zover dit
mogelijk is.  Completion staat beschreven in de node "Completion" in
de Emacs-Info-handleiding.

"Dired" toont je een lijst van bestanden in een directory, waarmee je
gemakkelijk bestanden kunt bezoeken, van naam kunt veranderen, kunt
wissen, of andere acties op uit kunt voeren.  Informatie over Dired
kun je vinden in de node "Dired" van de Emacs-Info-handleiding.


* CONCLUSIE
-----------

Denk eraan dat je met C-x C-c Emacs permanent verlaat.  Om tijdelijk
een shell te krijgen en daarna weer in Emacs terug te komen, tik je
C-z.

De bedoeling van deze inleiding is dat ze begrijpelijk is voor alle
nieuwe Emacs-gebruikers.  Als je dus iets onduidelijks bent
tegengekomen, blijf dan niet zitten en maak jezelf geen verwijten.
Doe je beklag!


* KOPIËREN
-----------

(De Engelse versie van) deze inleiding is voorafgegaan door een lange
reeks van Emacs-inleidingen, die begon met de inleiding die Stuart
Cracraft schreef voor de originele Emacs.  Deze Nederlandse vertaling
is gemaakt door Pieter Schoenmakers <tiggr@ics.ele.tue.nl> op basis
van de GNU Emacs 20.2 TUTORIAL, en nagezien en verbeterd door Frederik
Fouvry en Lute Kamstra.

(Wat nu volgt is een vertaling naar het Nederlands van de condities
voor gebruik en verspreiding van deze inleiding.  Deze vertaling is
niet gecontroleerd door een jurist.  Er kunnen derhalve geen rechten
aan de vertaling worden ontleend, en de vertaling wordt gevolgd door
het Engelse origineel.)

Deze versie van de inleiding valt onder copyright, net als GNU Emacs.
Je mag deze inleiding verspreiden onder bepaalde voorwaarden:

Copyright (C) 1985, 1996, 1997, 2003, 2004,
   2005 Free Software Foundation, Inc.

   Iedereen mag letterlijke kopieën van dit document, zowel ontvangen
   als verspreiden, op elk medium, vooropgesteld dat de
   copyrightvermelding en de toestemmingsmelding niet veranderd worden
   en dat de verspreider aan de ontvanger dezelfde distributierechten
   verleent als aan hem verleend worden door deze melding.

   Toestemming wordt verleend om veranderde versies van dit document,
   of delen daarvan, te verspreiden, onder bovenstaande voorwaarden,
   vooropgesteld dat ze ook duidelijk vermelden wie als laatste
   veranderingen aangebracht heeft.

De condities voor het kopiëren van Emacs zelf zijn ingewikkelder dan
dit, maar gebaseerd op dezelfde gedachte.  Lees het bestand COPYING en
geef vervolgens kopieën van Emacs aan al je vrienden.  Help bij het
uitroeien van softwarebeschermingspolitiek ("eigendom") door vrije
software te gebruiken, te schrijven en te delen!

(Engels origineel van de copyrightmelding en condities:

This version of the tutorial, like GNU Emacs, is copyrighted, and
comes with permission to distribute copies on certain conditions:

Copyright (C) 1985, 1996, 1997, 2002, 2003, 2004,
   2005 Free Software Foundation, Inc.

   Permission is granted to anyone to make or distribute verbatim
   copies of this document as received, in any medium, provided that
   the copyright notice and permission notice are preserved, and that
   the distributor grants the recipient permission for further
   redistribution as permitted by this notice.

   Permission is granted to distribute modified versions of this
   document, or of portions of it, under the above conditions,
   provided also that they carry prominent notices stating who last
   altered them.

The conditions for copying Emacs itself are more complex, but in the
same spirit.  Please read the file COPYING and then do give copies of
GNU Emacs to your friends.  Help stamp out software obstructionism
("ownership") by using, writing, and sharing free software!)

;;; Local Variables:
;;;   coding: latin-1
;;; End:

;;; arch-tag: 3399e308-e605-4125-8fbb-b2fe91ac3149
