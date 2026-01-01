Tutoriál k Emacsu.  Podmínky viz na konci souboru.
Do češtiny přeložil Milan Zamazal <pdm@zamazal.org>.

Příkazy Emacsu obecně využívají klávesu CONTROL (často označovanou jako
CTRL) nebo klávesu META (obvykle označovanou jako ALT).  Abychom tyto
názvy nemuseli stále dokola psát v plném znění, budeme používat
následující zkratky:

 C-<chr>  znamená přidržet klávesu CONTROL a stisknout znak <chr>.
          Tedy C-f znamená: přidržte klávesu CONTROL a stiskněte f.
 M-<chr>  znamená přidržet klávesu META nebo ALT a stisknout <chr>.
          Pokud nemáte klávesu META ani ALT, tak místo toho stiskněte a
          pusťte klávesu ESC a poté <chr>.  Klávesu ESC budeme značit
          <ESC>.

Důležitá poznámka: práci s Emacsem ukončíte stiskem C-x C-c (dva znaky).
Částečně inicializovaný příkaz můžete ukončit stisknutím C-g.
Chcete-li tento tutoriál ukončit, stiskněte C-x k a poté na výzvu dále
stiskněte <Return>.
Znaky ">>" na levém okraji značí místa, kde si máte vyzkoušet příkaz.
Například:
<<Blank lines inserted here by startup of help-with-tutorial>>
[Prostřední část obrazovky je prázdná záměrně.  Text pokračuje níže.]
>> Nyní stiskněte C-v (View next screen) pro posun na další obrazovku.
        (Směle do toho, proveďte to přidržením klávesy CONTROL a stiskem v.)
	Od této chvíle byste toto měli provádět kdykoliv dočtete zobrazenou
        obrazovku.

Všimněte si, že při posuvu obrazovek vždy zůstávají zobrazeny dva řádky
z předchozí obrazovky; to poskytuje určitou návaznost při postupném
čtení textu.

Toto je kopie textu tutoriálu k Emacsu, mírně přizpůsobená pro vás.
Později vám ukážeme, jak použít různé příkazy k úpravě tohoto textu.
Nebojte se, pokud text změníte ještě předtím, než o to budete požádáni;
tomu se říká "úprava textu" a právě k tomu je Emacs určen.

První věc, kterou potřebujete vědět, je, jak se v textu pohybovat z
jednoho místa na druhé.  Už víte, jak se posunout o jednu obrazovku
vpřed, pomocí C-v.  K přechodu o obrazovku zpět použijte M-v (přidržte
klávesu META a stiskněte v nebo stiskněte <ESC>v, pakliže nemáte klávesu
META ani ALT).

>>  Zkuste stisknout M-v a pak C-v párkrát za sebou.

Je samozřejmě v pořádku posouvat tento text i jinými způsoby, pokud víte
jak.

* SHRNUTÍ
---------

K prohlížení celých stránek jsou užitečné následující příkazy:

	C-v	Posun o obrazovku vpřed
	M-v	Posun o obrazovku zpět
	C-l	Smazání obrazovky a znovu zobrazení celého textu,
		 přitom se text pod kurzorem přesune ke středu obrazovky.
		 (Jedná se o CONTROL-L a ne CONTROL-1.)

>> Najděte kurzor a zapamatujte si, jaký text se okolo něj nachází.  Pak
   stiskněte C-l.  Opět najděte kurzor a všimněte si, že je kolem něj
   tentýž text, ale nyní je ve středu obrazovky.  Pokud stisknete C-l
   znovu, tento kus textu se přesune na horní okraj
   obrazovky. Opakovaným stiskem tentýž klávesy se text přesune se na
   spodní okraj.

K posunu po celých obrazovkách můžete také použít klávesy PageUp a
PageDn, pokud je váš terminál má k dispozici.  Později ale uvidíte, že
efektivnější je používat C-v a M-v.

* ZÁKLADNÍ OVLÁDÁNÍ KURZORU
---------------------------

Posouvání po celých obrazovkách je užitečné, ale jak se přemístíte na
konkrétní místo v textu na obrazovce?

Existuje několik způsobů, jak toho dosáhnout.  Můžete buď použít
standardní kurzorové klávesy (tj. klávesy se šipkami), anebo příkazy
C-p, C-b, C-f a C-n.  Druhý zmíněný způsob je efektivnější, protože ruce
zůstávají ve výchozí poloze na klávesnici.  Zde je tabulka znázorňující
směr posuvu kurzoru vyvolaný příkazy C-p, C-b, C-f a C-n:

			  Předchozí řádek, C-p
				  :
				  :
      Dozadu, C-b .... Momentální pozice kurzoru .... Dopředu, C-f
				  :
				  :
			 Následující řádek, C-n

>> Přesuňte kurzor na prostřední řádek diagramu výše pomocí C-n nebo
   C-p.  Potom stiskněte C-l, abyste na obrazovce viděli celý diagram
   vycentrován.

Pravděpodobně se vám budou tyto příkazy snadněji pamatovat podle
počátečních písmen anglických názvů: P jako previous (předchozí), N jako
next (následující), B jako backward (zpět), F jako forward (vpřed).
Jsou to základní příkazy pro pohyb kurzoru a budete je používat
neustále, takže by bylo velmi vhodné, kdybyste se je teď naučili.

>> Proveďte několikrát C-n, abyste kurzor přesunuli na tento řádek.

>> Posuňte kurzor dovnitř řádku pomocí několika C-f a pak nahoru stiskem C-p.
   Pozorujte, co C-p dělá, když je kurzor uprostřed řádku.

Každý řádek textu končí znakem nového řádku, který jej odděluje od řádku
následujícího.  Znakem nového řádku by měl být ukončen i poslední řádek
souboru (přestože to Emacs nevyžaduje).

>> Vyzkoušejte C-b na začátku řádku.  Kurzor by se měl přesunout na
   konec předchozího řádku, neboť jej tím přesunete přes znak nového
   řádku.

C-f funguje analogicky jako C-b, tj. na konci řádku dojde k přesunu na
další řádek.

>> Proveďte několik C-b, tak abyste navnímali, kde se kurzor nachází.
   Pak provádějte C-f, abyste se vrátili na konec řádku.  Pak proveďte
   ještě jednou C-f, abyste se přesunuli na následující řádek.

Když kurzorem přejdete přes horní nebo dolní okraj obrazovky, posune se
text za příslušným okrajem na obrazovku.  Tato vlastnost se nazývá
"scrollování".  Scrollování umožňuje přemístit kurzor na libovolné místo
v textu, aniž by kurzor opustil obrazovku.

>> Zkuste posunout kurzor pod dolní okraj obrazovky pomocí C-n a
   pozorujte, co se stane.

Pokud je posun po znacích příliš pomalý, můžete se pohybovat po slovech.
M-f (META-f) provádí posun o slovo vpřed a M-b provádí posun o slovo
zpět.

>> Stiskněte několikrát M-f a M-b.

Pokud se kurzor nachází uprostřed slova, M-f provede přesun na konec
tohoto slova.  Nachází-li se kurzor v mezeře mezi slovy, M-f provede
přesun na konec následujícího slova.  M-b funguje totožně, akorát že v
opačném směru.

>> Stiskněte několikrát M-f a M-b a mezi tím C-f a C-b, abyste viděli
   výsledky příkazů M-f a M-b prováděných z různých míst uvnitř slov a
   mezi nimi.

Všimněte si analogie mezi C-f a C-b na jedné straně a M-f a M-b na
straně druhé.  Znaky s klávesou META jsou velmi často využívány pro
operace vztahující se k entitám definovaným jazykem (slova, věty,
odstavce), zatímco znaky s klávesou CONTROL pracují na základních
prvcích nezávislých na tom, co zrovna editujete (znaky, řádky apod.).

Tato analogie platí také pro řádky a věty: C-a a C-e provádí přesun
na začátek a konec řádku, M-a a M-e provádí přesun na začátek a konec
věty.

>> Zkuste několikrát C-a a poté několikrát C-e.
   Zkuste několikrát M-a a poté několikrát M-e.

Všimněte si, že opakované C-a nedělá nic, zatímco opakované M-a vždy
provádí posun o další větu.  Principu analogie to sice příliš
neodpovídá, ale přesto je toto chování možno považovat za přirozené.

Pozice kurzoru v textu se také nazývá "bod" ("point").  Abychom to
parafrázovali, kurzor je na obrazovce vidět v místě, kde je bod umístěn
v textu.

Zde je přehled jednoduchých operací pro pohyb kurzoru včetně příkazů pro
pohyb mezi slovy a větami:

	C-f	Přesun o znak vpřed
	C-b	Přesun o znak zpět

	M-f	Přesun o slovo vpřed
	M-b	Přesun o slovo zpět

	C-n	Přesun na následující řádek
	C-p	Přesun na předchozí řádek

	C-a	Přesun na začátek řádku
	C-e	Přesun na konec řádku

	M-a	Přesun zpět na začátek věty
	M-e	Přesun vpřed na konec věty

>> Procvičte si teď několikrát všechny tyto příkazy.
   Jsou to nejpoužívanější příkazy.

Další dva důležité příkazy pro pohyb kurzoru jsou M-< (META menší-než),
který provede přesun na začátek celého textu, a M-> (META větší-než),
který provede přesun na konec celého textu.

Na většině terminálů je "<" nad čárkou, takže pro vyvolání tohoto znaku
musíte použít klávesu Shift.  Na těchto terminálech je tedy nutno použít
klávesu shift i v případě příkazu M-<; bez klávesy shift byste provedli
M-čárka.

>> Zkuste teď M-< pro přesun na začátek tutoriálu.
   Použijte pak opakovaně C-v, abyste se opět vrátili sem.

>> Zkuste teď M-> pro přesun na konec tutoriálu.
   Použijte pak opakovaně M-v, abyste se opět vrátili sem.

Kurzor můžete přesouvat také pomocí kurzorových kláves (klávesy se
šipkami), pokud je váš terminál má.  My však doporučujeme naučit se C-b,
C-f, C-n a C-p, a to ze tří důvodů.  Za prvé, tyto klávesy fungují na
všech typech terminálů.  Za druhé, jakmile jednou získáte cvik v
používání Emacsu, zjistíte, že používání těchto Control znaků je
rychlejší než používání kurzorových kláves (protože nemusíte přesouvat
ruku z psací pozice).  Za třetí, zvyknete-li si používat tyto CTRL-znak
příkazy, snadno se naučíte používat jiné pokročilé příkazy pro pohyb
kurzoru.

Většina příkazů Emacsu akceptuje numerický argument, který pro většinu
příkazů slouží jako opakovač.  Počet opakování příkazu zadáte
prostřednictvím stisku C-u následovaného stiskem příslušných číslic před
vyvoláním příkazu.  Máte-li META (nebo ALT) klávesu, existuje
alternativní možnost zadání numerického argumentu: přidržte klávesu META
a stiskněte příslušné číslice.  Doporučujeme naučit se C-u metodu,
protože ta funguje na jakémkoliv terminálu.  Numerický argument se
taktéž nazývá "prefixový argument", protože jej zadáváte před příkazem,
na který se vztahuje.

Například C-u 8 C-f provede přesun o osm znaků vpřed.

>> Zkuste si procvičit použití C-n nebo C-p s numerickým argumentem tak,
   abyste kurzor posunuli někam poblíž tohoto řádku.

Většina příkazů používá numerický argument jako opakovač.  Jisté
výjimečné příkazy jej používají jiným způsobem.  Řada příkazů (žádný
však, který jste se dosud naučili) ho používá jako přepínač,
tzn. přítomnost prefixového argumentu způsobí, že příkaz udělá něco
jiného (tj. bez ohledu na jeho hodnotu).

Příkazy C-v a M-v patří do jiné kategorie výjimek.  Dostanou-li
numerický argument, posunou obrazovku nahoru nebo dolů o odpovídající
počet řádků.  Například C-u 8 C-v posune obrazovku o 8 řádků.

>> Zkuste nyní zadat C-u 8 C-v.

Tento příkaz by měl posunout text o 8 řádků nahoru.  Pokud chcete text
posunout zpět, použijte stejný prefixový argument s příkazem M-v.

Používáte-li grafické uživatelské prostředí, jako například X Window,
měli byste mít na jedné straně emacsového okna vysokou obdélníkovou
oblast, nazývanou jako scrollbar (posuvník).  Pakliže máte posuvník k
dispozici, text můžete posouvat jím.

Pokud máte k dispozici myš s rolovacím kolečkem, můžete používat i to.


* KDYŽ EMACS NEREAGUJE
----------------------

Jestliže Emacs přestane reagovat na vaše příkazy, můžete probíhající
činnost bezpečně zastavit pomocí C-g.  Pomocí C-g můžete zastavit
příkaz, jehož provádění trvá příliš dlouho.

C-g můžete použít také pro odstranění numerického argumentu příkazu,
který nechcete dokončit.

>> Stiskněte C-u 100 pro vytvoření numerického argumentu 100 a pak
   stiskněte C-g.  Nyní stiskněte C-f.  Měl by být proveden posun
   o právě jeden znak, protože jste argument zrušili prostřednictvím
   C-g.

Pokud jste omylem stiskli <ESC>, můžete se jej zbavit pomocí C-g.


* DEAKTIVOVANÉ PŘÍKAZY
----------------------

Některé příkazy Emacsu jsou "deaktivované" ("disabled"), aby je
začínající uživatelé nemohli vyvolat náhodně.

Pokud vyvoláte některý z deaktivovaných příkazů, Emacs zobrazí hlášení
oznamující, který příkaz to byl, s dotazem, zda chcete tento příkaz
provést.

Pokud opravdu chcete příkaz vyzkoušet, stiskněte mezerník jako odpověď
na tuto otázku.  Obyčejně, jestliže nechcete deaktivovaný příkaz
provést, odpovězte na tuto otázku pomocí "n".

>> Stiskněte C-x C-l (což je deaktivovaný příkaz),
   pak na otázku odpovězte n.


* OKNA
------

Emacs může mít několik oken (windows), z nichž každé zobrazuje svůj
vlastní text.  Způsob, jak více oken používat současně, objasníme
později.  Nyní bychom rádi objasnili, jak se zbavit nadbytečných oken a
vrátit se do základní jednookenní editace.  Je to jednoduché:

	C-x 1	Jedno okno (tj. zrušení všech ostatních oken)

Tedy vložení CONTROL-x následované číslicí 1.  Příkaz C-x 1 rozšíří okno
obsahující kurzor přes celou obrazovku.  Zruší to všechna ostatní okna.

>> Posuňte kurzor na tento řádek a stiskněte C-u 0 C-l.
>> Stiskněte C-h k C-f.
   Pozorujte, jak se aktuální okno zmenší a objeví se nové okno za
   účelem zobrazení dokumentace k příkazu C-f.

>> Stiskněte C-x 1 a pozorujte, jak okno s dokumentací zmizí.

Existuje celá řada příkazů, které začínají CONTROL-x.  Mnoho z těchto
příkazů se týká oken, souborů, bufferů a podobných věcí.  Tyto příkazy
mají dva, tři nebo čtyři znaky.


* VKLÁDÁNÍ A MAZÁNÍ
-------------------

Chcete-li vložit text, prostě jej napište.  Běžné znaky, které vidíte,
jako A, 7, *, atd., jsou Emacsem chápány jako prostý text a jsou
vkládány okamžitě.  Pro vložení znaku nového řádku stiskněte <Return>
(klávesu Enter).

Poslední znak, který jste napsali, můžete smazat stiskem <DEL>.  Jedná
se o stejnou klávesu, kterou mimo Emacs běžně používáte ke smazání
posledního znaku, který jste napsali.  Tato klávesa je většinou označena
jako "Backspace".

Na klávesnici je běžně k dispozici i klávesa označena jako <Delete>.
Nejedná se však o klávesu, kterou v prostředí Emacsu označujeme jako
<DEL>.

>> Proveďte to teď -- napište několik znaků a pak je smažte několika
   stisky <DEL>.  Nebojte se změn v tomto souboru; originální
   tutoriál se nezmění.  Toto je vaše osobní kopie.

Pokud se řádek textu zvětší natolik, že přesáhne jeden řádek obrazovky,
pak je zobrazen na dalším řádku obrazovky.  Pokud používáte grafické
uživatelské prostředí, po stranách se objeví malé zakřivené šipky, které
naznačují, kde řádek pokračuje.  Pakliže používáte textový terminál,
řádek textu, který pokračuje na dalším řádku obrazovky, je indikován
zpětným lomítkem („\“) na pravém okraji.

>> Vkládejte text, až dosáhnete pravého okraje, a pokračujte ve vkládání.
   Objeví se vám pokračovací řádek.

>> Použijte <DEL> pro smazání textu, až se řádek textu opět vejde na
   jeden řádek obrazovky.  Pokračovací řádek zmizí.

Znak nového řádku můžete smazat jako kterýkoliv jiný znak.  Smazání
znaku nového řádku mezi dvěma řádky způsobí jejich spojení do jediného
řádku.  Je-li výsledný řádek příliš dlouhý na to, aby se vešel na šířku
obrazovky, bude zobrazen pokračovacím řádkem.

>> Přesuňte kurzor na začátek řádku a stiskněte <DEL>.  To tento
   řádek spojí s řádkem předchozím.

>> Stiskněte <Return> pro znovu vložení smazaného znaku nového řádku.

Klávesa <Return> je speciální v tom, že její stisknutí může udělat více
než jen vložit znak nového řádku.  V závislosti na okolním textu může
také vložit prázdné mezery, čímž text zarovná s předchozím řádkem.
Tento způsob chování (kdy stisk klávesy udělá více než že jen vloží
příslušný znak) nazýváme "elektrický" (angl. "electric").

>> Zde je příklad, kdy je klávesa <Return> elektrická.
   Stiskněte <Return> na konci tohoto řádku.

Měli byste vidět, že po stisknutí klávesy <Return> Emacs vloží prázdné
mezery na začátek řádku tak, aby se kurzor dostal pod písmeno "S" ve
slově "Stiskněte".

Vzpomeňte si, že většina příkazů Emacsu může dostat počet opakování;
včetně textových znaků.  Opakování textových znaků je vloží několikrát.

>>  Vyzkoušejte si to teď -- stiskněte C-u 8 * pro vložení ********.

Teď už znáte nejzákladnější způsoby, jak něco v Emacsu napsat a jak
opravovat chyby.  Můžete ovšem také mazat po slovech nebo po řádcích.
Zde je shrnutí operací pro mazání textu:

	<DEL>        Smazání znaku bezprostředně před kurzorem
	C-d   	     Smazání znaku následujícího za kurzorem

	M-<DEL>      Zrušení slova bezprostředně před kurzorem
	M-d	     Zrušení slova následujícího za kurzorem

	C-k	     Zrušení textu od pozice kurzoru do konce řádku
	M-k	     Zrušení textu do konce aktuální věty

Všimněte si, že <DEL> a C-d, resp. M-<DEL> a M-d, rozšiřují paralelu
započatou C-f a M-f (pravda, <DEL> opravdu není CONTROL znak, ale
netrapme se tím).  C-k a M-k jsou jako C-e a M-e ve smyslu vztahu řádků
k větám.

Libovolnou část textu můžete rovněž zrušit následující metodou.
Přesuňte se na jeden konec této části a stiskněte C-<SPC> (<SPC>
označuje mezerník).  Dále se přesuňte se na druhý konec části, kterou
chcete zrušit.  Jakmile začnete tuto akci provádět, Emacs zvýrazní text
mezi kurzorem a pozicí, kde jste stiskli C-<SPC>.  Na závěr celé
sekvence stiskněte C-w, čímž se text mezi oběma pozicemi zruší.

>> Přesuňte kurzor na písmeno L na začátku předchozího odstavce.
>> Stiskněte C-<SPC>.  Emacs by měl ve spodní části obrazovky zobrazit
   zprávu "Mark set".
>> Přesuňte kurzor na písmeno c ve slově "konec" na druhém řádku
   odstavce.
>> Stiskněte C-w.  Text začínající písmenem L a končící před písmenem
   c bude zrušen.

Uvědomte si, že rozdíl mezi "rušením" ("killing") a "mazáním"
("deleting") je ten, že "zrušené" věci mohou být zpět vhozeny, zatímco
"smazané" nikoliv (můžete však smazané věci vrátit zpět, viz níže).
Zpětné vložení zrušeného textu se nazývá "vhazování" (angl. "yanking").
Představte si opětovné vhazování jako vracení dříve odstraněného textu
zpět.  Obecně platí, že příkazy, které mohou odstranit větší množství
textu provádějí "rušení" a mohou být tedy vhozeny zpět.  Naopak příkazy,
které mažou pouze jeden znak, mezery či řádek, provádějí mazání, a tudíž
nemohou být vhozeny zpět.  Příkazy <DEL> a C-d (bez jakéhokoliv
argumentu) provádějí mazání.  Pokud však dostanou prefixový argument,
pak provádějí rušení.

>> Přesuňte kurzor na začátek neprázdného řádku.
   Pak stiskněte C-k pro zrušení textu na tomto řádku.
>> Stiskněte C-k podruhé.  Uvidíte, že to zruší znak nového řádku, který
   za tímto řádkem následuje.

Všimněte si, že jedno C-k zruší obsah řádku a druhé C-k zruší řádek
samotný a posune všechny další řádky nahoru.  C-k pracuje s numerickým
argumentem speciálně: zruší odpovídající počet řádků VČETNĚ jejich
obsahu.  To už není opakování.  C-u 2 C-k zruší dva řádky a jejich
obsah; dvojitý stisk C-k by toto obvykle neudělal.

Zrušený text můžete vhodit buď na stejné místo, kde byl zrušen, nebo na
jiné místo v textu, který upravujete, ba dokonce i do jiného souboru.
Text můžete vhodit i vícekrát, vytváříte tak jeho další kopie.  Některé
jiné textové editory nazývají rušení jako "vyjmutí" a vhazování jako
"vkládání" (viz Glossary v Manuálu Emacs).

Příkazem pro vhazování je C-y.  Tento příkaz vloží poslední smazaný
text na pozici, na které se nachází kurzor.

>> Zkuste to; stiskněte C-y pro vhození textu zpět.

Stisknete-li několikrát C-k po sobě, všechen smazaný text je uložen
společně tak, aby bylo možné vhodit zpět všechny řádky najednou.

>> Stiskněte několikrát C-k.

Nyní obnovte posledně zrušený text:

>> Stiskněte C-y.  Následně posuňte kurzor o několik řádků níže a
   stiskněte C-y znova.  Nyní vidíte, jak lze text kopírovat.

Co když máte nějaký text, který byste rádi vhodili zpět a pak zrušíte
něco jiného?  Vzpomeňte, že C-y by vložilo poslední zrušený text.
Nebojte, předchozí text není ztracen.  Můžete jej získat zpět použitím
příkazu M-y.  Poté, co provedete C-y pro získání posledního zrušeného
textu, stisk M-y vymění tento vhozený text za předchozí zrušený text.
Dalšími a dalšími stisky M-y dostáváte předcházející a předcházející
zrušené texty.  Jakmile dosáhnete textu, který hledáte, nemusíte s ním
pro jeho uchování nic dalšího provádět.  Jednoduše vhozený text
ponechejte, kde je, a pokračujte v editaci.

Pokud opakujete M-y dostatečně dlouho, dostanete se zpátky k výchozímu
bodu (posledně zrušenému textu).

>> Zrušte řádek, přesuňte kurzor někam jinam a zrušte jiný řádek.
   Pak proveďte C-y pro vrácení druhého zrušeného řádku.
   Pak proveďte M-y a vhozený řádek bude nahrazen prvním zrušeným řádkem.
   Opakujte M-y a pozorujte, co dostáváte.  Pokračujte v tom, dokud se
   znovu neobjeví druhý zrušený řádek a pak několik dalších.
   Chcete-li, můžete zkusit předat M-y kladné a záporné argumenty.


* UNDO
------

Jestliže provedete v textu změnu a pak zjistíte, že to byl omyl, můžete
změnu vrátit příkazem undo, C-/.

C-/ obvykle vrátí změny provedené jedním příkazem; pokud C-/ zopakujete
několikrát za sebou, každé opakování vrátí jeden další příkaz.

Existují ale dvě výjimky: příkazy, které nemění text, se nepočítají (to
zahrnuje příkazy pro pohyb kurzoru a scrollování) a znaky vkládající
samy sebe jsou obvykle zpracovávány ve skupinách až po 20.  To je kvůli
tomu, aby se zredukoval počet C-/ nutných pro vrácení vkládaného textu.

>> Zrušte tento řádek pomocí C-k, stiskněte pak C-/ a řádek by se měl
   znovu objevit.

Alternativním příkazem undo je příkaz C-_.  Na některých terminálech
můžete při vkládání příkazu C-_ vynechat klávesu shift.  Pokud na
některých terminálech vložíte příkaz C-/, Emacs ve skutečnosti obdrží
příkaz C-_. Další alternativou je příkaz C-x u, který funguje totožně
jako C-/, avšak může být méně pohodlný.

Numerický argument pro C-_ a C-x u funguje jako počet opakování.

Pomocí příkazu undo můžete vrátit zrušený stejně jako smazaný text.
Rozdíl mezi mazáním a rušením textu ovlivňuje možnost vhození tohoto
textu pomocí C-y, neovlivňuje možnosti příkazu zpět.


* SOUBORY
---------

Aby text, který editujete, zůstal trvale uchován, musíte jej uložit do
souboru.  V opačném případě by byl po ukončení Emacsu ztracen.  Abyste
mohli vložit text do nějakého souboru, musíte ho nejprve "vyhledat"
(angl. "find").  (Také se to nazývá "navštívení" ("visiting") souboru.)

Vyhledání souboru znamená, že vidíte jeho obsah v Emacsu.  V mnoha
ohledech je to jako byste editovali přímo ten soubor.  Nicméně změny,
které prostřednictvím Emacsu činíte, se nestanou trvalými, dokud tyto
změny do souboru "neuložíte" ("save").  Tím se zamezí nechtěnému
ponechání částečně změněného souboru v systému.  Dokonce i když soubor
uložíte, Emacs uchová původní soubor pod změněným názvem pro případ, že
byste zjistili, že vaše úpravy byly chybné.

Když se podíváte do dolní části obrazovky, uvidíte řádek, který začíná
pomlčkami a na začátku má " -:--- TUTORIAL.cs" nebo něco podobného.
Tato část obrazovky obvykle obsahuje jméno souboru, který je právě
navštíven.  Zrovna teď máte navštíven soubor nazvaný "TUTORIAL.cs",
který je vaší osobní pracovní kopií tutoriálu Emacsu.  Když v Emacsu
vyhledáte soubor, jeho jméno se objeví přesně na tomto místě.

Příkaz pro vyhledání souboru je speciální v tom, že musíte Emacsu
nejprve sdělit jeho název.  V takovýchto případech hovoříme o tom, že
Emacs "čte argument" (argumentem je v tomto konkrétním případě název
daného souboru).  Poté co vyvoláte příkaz

	C-x C-f   Vyhledání souboru

se vás Emacs zeptá na jméno souboru.  Jméno souboru, které píšete, se
objevuje ve spodním řádku obrazovky.  Toto dialogové okno se nazývá
minibuffer.  Pro editaci jména souboru můžete používat obvyklé editační
příkazy Emacsu.

Zadávání jména souboru (obecně kterýkoliv vstup z minibufferu) můžete
zrušit příkazem C-g.

>> Stiskněte C-x C-f a pak C-g.  Tato klávesová kombinace zruší
   minibuffer a taktéž to zruší příkaz C-x C-f, který daný minibuffer
   vytvořil.  Jakmile příkaz zrušíte, tak soubor nebude vyhledán.

Pokud chcete příkaz dokončit, pak po napsání jména souboru stiskněte
<Return>.  Příkaz C-x C-f pak začne pracovat a vyhledá soubor, který
jste zvolili.

Po malé chvilce se obsah souboru objeví na obrazovce a můžete jej
editovat.  Když chcete změny trvale uložit, použijte příkaz

	C-x C-s   Uložení souboru

Na tomto místě Emacs zkopíruje text do příslušného souboru.  Když to
provedete poprvé, Emacs přejmenuje původní soubor na soubor s novým
jménem, aby nebyl ztracen.  Název nového souboru je vytvořeno přidáním
"~" na konec původního jména souboru.  Když je ukládání dokončeno, Emacs
zobrazí jméno zapsaného souboru.

>> Stiskněte C-x C-s TUTORIAL.cs <Return>.
   Mělo by se vám zobrazit "Wrote ...TUTORIAL.cs" ve spodní části
   obrazovky.

Existující soubor můžete vyhledat, abyste jej mohli prohlížet nebo
editovat.  Můžete ale také vyhledat soubor, který ještě neexistuje.  To
je způsob, jakým lze vytvořit soubor v Emacsu: vyhledejte soubor, který
bude na začátku prázdný a pak začněte vkládat text určený pro tento
soubor.  Když požádáte o uložení, Emacs skutečně vytvoří soubor s
textem, který jste vložili.  Od té chvíle se pak můžete cítit, jako
kdybyste editovali již existující soubor.


* BUFFERY
---------

Jestliže vyhledáte pomocí C-x C-f druhý soubor, první soubor v Emacsu
zůstává.  Můžete se do něj zpět přepnout jeho opětovným vyhledáním
pomocí C-x C-f.  Tímto způsobem můžete do Emacsu dostat poměrně hodně
souborů.

Emacs ukládá text každého souboru do objektu nazývaného "buffer".
Vyhledání souboru vytvoří v Emacsu nový buffer.  Chcete-li vidět seznam
bufferů, které momentálně existují ve vašem Emacsu, stiskněte:

	C-x C-b   Seznam bufferů

>> Zkuste teď C-x C-b.

Podívejte se, že každý buffer má v seznamu jméno a může tam mít také
jméno souboru, ke daný buffer kterému patří.  JAKÝKOLIV text, který
vidíte v emacsovém okně, je vždy součástí nějakého bufferu.

>> Stiskněte C-x 1, abyste se zbavili seznamu bufferů.

Pokud v Emacsu máte vícero aktivních bufferů, pouze jeden z nich může
být "aktuální".  Aktuální buffer je vždy ten, který právě editujete.
Pokud chcete provádět úpravy v jiném bufferu, pak se do něj musíte
"přepnout" (angl. "switch").  Pakliže se chcete přepnout do bufferu,
který odpovídá nějakému souboru na disku, můžete tak učinit příkazem C-x
C-f, který daný soubor navštíví.  Existuje ale lehčí způsob: použijte
příkaz C-x b.  Abyste tento příkaz dokončili, musíte vložit název
bufferu.

>> Vytvořte soubor nazvaný "foo" takto: C-x C-f foo <Return>.
   Dále proveďte C-x b TUTORIAL.cs <Return> tak, abyste se vrátili zpět
   do tohoto tutoriálu.

Ve většině případů název bufferu odpovídá názvu souboru (bez cesty k
němu).  Není tomu ale tak vždy.  Seznam bufferů, který se vám ukáže po
provedení příkazu C-x C-b, zobrazuje jak název bufferu, tak i souboru.

Nějaké buffery ovšem nepatří k žádnému souboru.  Například buffer
nazvaný "*Buffer List*" obsahuje seznam bufferů, který Emacs vytvořil po
provedení příkazu C-x C-b.  Tutoriál, který právě čtete, dříve nepatřil
k žádnému souboru.  Nyní ale ano, protože jste ho v předchozí části
pomocí C-x C-s uložili do souboru na disk.

Buffer nazvaný "*Messages*" taktéž nepatří k žádnému souboru.  Tento
buffer obsahuje seznam zpráv, které se vám zobrazují ve spodním řádku
vašeho Emacsu.

>> Proveďte C-x b *Messages* <Return> abyste se podívali, jaké zprávy
   tento buffer obsahuje.
   Dále proveďte C-x b TUTORIAL.cs <Return> abyste se vrátili zpět.

Pokud provedete změny textu jednoho souboru a pak vyhledáte jiný soubor,
nezpůsobí to uložení prvního souboru.  Jeho změny zůstávají v Emacsu
uchovány v jemu odpovídajícím bufferu.  Vytvoření a editace druhého
souboru nemá žádný vliv na buffer prvního souboru.  To je velmi
užitečné, ale také to znamená, že potřebujete vhodný způsob, jak uložit
buffer prvního souboru.  Nutnost přepnout se zpátky pomocí C-x C-f, aby
jej bylo možno uložit prostřednictvím C-x C-s, by byla nemístně
obtěžující.  Takže máme

	C-x s     Uložení některých bufferů

C-x s se vás zeptá na každý buffer, který obsahuje změny, které jste
neuložili.  Pro každý takový buffer se vás zeptá, zda jej má uložit.

>> Vložte řádek textu a pak stiskněte C-x s.
   Měli byste být dotázáni, zda má být uložen buffer nazvaný TUTORIAL.cs.
   Odpovězte na tuto otázku ano (yes) stiskem "y".


* ROZŠIŘOVÁNÍ SADY PŘÍKAZŮ
--------------------------

Existuje mnohem, mnohem více příkazů Emacsu, než které by vůbec mohly
být rozmístěny na všechny CONTROL a META znaky.  Emacs tento problém
obchází prostřednictvím X (eXtend) příkazu.  Ten vzniká dvěma způsoby:

	C-x	Znakový eXtend.  Následován jedním znakem.
	M-x	Pojmenovaný příkaz eXtend.  Následován dlouhým názvem.

To jsou příkazy, které jsou obecně užitečné, avšak méně často používané
než ty, které jste se již naučili.  Už jste viděli některé z nich, jako
například souborové příkazy C-x C-f pro vyhledání a C-x C-s pro uložení.
Jiný příklad je příkaz pro ukončení Emacsu -- tj. příkaz C-x C-c.
(Nemějte obavy o ztrátu změn, které jste provedli; C-x C-c nabídne
uložení každého změněného souboru, než Emacs ukončí.)

Pokud používáte grafické uživatelské prostředí, nemusíte používat žádný
speciální příkaz pro přepnutí do jiné aplikace.  Můžete tak učinit myší,
anebo pomocí příkazů vašeho správce oken.  Pakliže ale používáte
textový terminál, který může v jeden okamžik zobrazit pouze jednu
aplikaci, potřebujete příkaz na "pozastavení" (angl. "suspend") Emacsu,
abyste se mohli věnovat práci v jiné aplikaci.

C-z je příkaz na *dočasné* opuštění Emacsu -- můžete se po něm do
spuštěného Emacsu vrátit.  Pakliže je Emacs spuštěn na textovém
terminálu, C-z Emacs "pozastaví"; tzn. vrátí vás do shellu, avšak Emacs
neukončí.  V nejběžnějších shellech se můžete do Emacsu vrátit příkazem
"fg" nebo pomocí "%emacs".

Chvíle pro použití C-x C-c nastane, když se chystáte odhlásit ze
systému.  Správné je to také při ukončování Emacsu vyvolaného poštovním
programem a různými jinými utilitami.

Existuje mnoho C-x příkazů.  Zde je seznam těch, které jste se již
naučili:

	C-x C-f		Vyhledání souboru
	C-x C-s		Uložení soubor
	C-x s           Uložení některých bufferů
	C-x C-b		Seznam bufferů
	C-x b           Přepnutí bufferu
	C-x C-c		Ukončení Emacsu
	C-x 1           Jedno okno (tj. zrušení všech ostatních oken)
	C-x u		Undo (tj. zpět)

Pojmenované eXtended příkazy jsou příkazy, které jsou používány ještě
méně, nebo příkazy, které jsou používány jenom v jistých módech.
Příkladem je příkaz replace-string, který globálně nahradí jeden řetězec
jiným.  Když stisknete M-x, Emacs vypíše na spodním řádku obrazovky
prompt M-x a vy byste měli zadat název příkazu, v tomto případě
"replace-string".  Jednoduše napište "repl s<TAB>" a Emacs název doplní.
(<TAB> vyjadřuje klávesu tabulátor, která se běžně nachází nad klávesou
Caps Lock nebo Shift na levé straně klávesnice.)  Dokončete zadávání
názvu příkazu pomocí <Return>.

Příkaz replace-string vyžaduje dva argumenty -- řetězec, který má být
nahrazen, a řetězec, který jej má nahradit.  Každý argument musíte
ukončit pomocí <Return>.

>> Přesuňte kurzor na prázdný řádek, který se nachází dva řádky pod
   tímto.
   Pak napište M-x repl s<Return>změnil<Return>modifikoval<Return>.

   Všimněte si, jak se tento řádek změnil: nahradili jste slovo "změnil"
   slovem "modifikoval", kdekoliv se za aktuální pozicí kurzoru
   vyskytlo.


* AUTOMATICKÉ UKLÁDÁNÍ
----------------------

Jestliže jste provedli změny v souboru, ale nemáte je ještě uloženy,
mohou být v případě pádu systému ztraceny.  Aby vás Emacs od toho
uchránil, Emacs periodicky zapisuje "auto save" soubor pro každý soubor,
který editujete.  Jméno auto save souboru má na začátku a na konci #;
například jestliže se váš soubor jmenuje "hello.c", jeho auto save
soubor se jmenuje "#hello.c#".  Když soubor uložíte běžným způsobem,
Emacs auto save soubor smaže.

Jestliže dojde k pádu systému, můžete svoji editaci obnovit z auto-save
souboru, a to normálním vyhledáním souboru (toho, který jste editovali,
ne auto save souboru) a následnou aplikací M-x recover-this-file
<Return>.  Na žádost o potvrzení odpovězte zadáním yes<Return> pro
pokračování a obnovení auto-save dat.


* ECHO OBLAST
-------------

Když Emacs vidí, že píšete příkazy pomalu, ukazuje vám je ve spodní
části obrazovky v oblasti nazývané "echo oblast".  Echo oblast obsahuje
dolní řádek obrazovky.


* STAVOVÝ ŘÁDEK
---------------

Řádek bezprostředně nad echo oblastí se nazývá "stavový řádek" ("mode
line").  Stavový řádek ukazuje něco jako:

 -:**-  TUTORIAL.cs       63% L749    (Fundamental)

Tento řádek podává užitečnou informaci o stavu Emacsu a textu, který
editujete.

Už víte, co znamená jméno souboru -- je to soubor, který jste vyhledali.
NN% označuje vaši aktuální pozici v textu; říká, že NN procent textu je
nad horním okrajem obrazovky.  Je-li začátek souboru na obrazovce, Emacs
typicky ve stavovém řádku ukazuje "Top" a nikoliv " 0%".  Je-li konec
textu na obrazovce, ve stavovém řádku bude "Bot".  Jestliže se díváte na
tak malý text, že se celý vejde na obrazovku, stavový řádek říká "All".

Písmeno L a číslice za ním vyjadřují pozici kurzoru číslem řádku, na
kterém se nachází.

Hvězdičky poblíž začátku znamenají, že jste text změnili.  Těsně po
vyhledání nebo uložení souboru v této části stavového řádku nejsou žádné
hvězdičky, pouze pomlčky.

Část stavového řádku v závorkách říká, v jakých editačních módech se
nacházíte.  Výchozí mód je Fundamental, což je ten, který momentálně
používáte.  Jedná se o příklad hlavního módu ("major mode").

Emacs má celou řadu hlavních módů.  Některé z nich jsou určeny pro
editaci různých programovacích jazyků a/nebo textů jako třeba Lisp mód,
Text mód atd.  V libovolném okamžiku je aktivní právě jeden hlavní mód
a jeho jméno lze nalézt ve stavovém řádku na místě, kde se momentálně
nachází "Fundamental".

Každý hlavní mód mění chování některých příkazů.  Například existují
příkazy pro vytváření komentářů v programu, a protože každý programovací
jazyk má jinou představu o tom, jak má komentář vypadat, musí každý
hlavní mód vkládat komentáře jinak.  Každý hlavní mód je vlastně jméno
extended příkazu, kterým se do tohoto módu můžete přepnout.  Například
M-x fundamental-mode je příkaz pro přepnutí se do Fundamental módu.

Chystáte-li se editovat český text, jako třeba tento soubor,
pravděpodobně byste měli použít Text mód.

>> Napište M-x text-mode <Return>.

Nebojte se, žádný z příkazů, které jste se naučili, chování Emacsu nijak
významně nezmění.  Můžete si ale všimnout, že M-f a M-b nyní pracují s
apostrofy jako se součástmi slov.  Předtím, ve Fundamental módu, M-f a
M-b pracovaly s apostrofy coby oddělovači slov.

Hlavní módy obvykle dělají menší změny, jako byla tato: příkazy většinou
dělají "totéž", ale v každém hlavním módu pracují trochu jinak.

Dokumentaci k aktuálnímu hlavnímu módu si můžete zobrazit stiskem C-h m.

>> Posuňte kurzor na řádek pod tímto.
>> Stiskněte C-l C-l, abyste tento řádek posunuli úplně nahoru.
>> Stiskněte C-h m abyste viděli, jak se Text mód liší od Fundamental
   módu.
>> Stiskněte C-x 1 pro odstranění dokumentace z obrazovky.

Hlavní módy se nazývají hlavní proto, že také existují vedlejší módy
(minor modes).  Vedlejší módy nejsou alternativou k hlavním módům, nýbrž
jejich malé modifikace.  Každý vedlejší mód může být zapnut nebo vypnut
sám o sobě nezávisle na všech ostatních vedlejších módech a nezávisle na
hlavním módu.  Takže nemusíte používat žádný vedlejší mód nebo můžete
používat jeden vedlejší mód nebo libovolnou kombinaci několika
vedlejších módů.

Jedním z velmi užitečných vedlejších módů, zejména pro editaci prostého
textu, je Auto Fill mód.  Když je tento mód zapnutý, Emacs zalomí řádek
mezi dvěma slovy, kdykoliv vkládáte text a řádek se stane příliš
dlouhým.

Auto Fill mód můžete zapnout provedením M-x auto-fill-mode <Return>.
Je-li tento mód zapnutý, můžete jej vypnout provedením M-x
auto-fill-mode <Return>.  Je-li mód vypnut, tento příkaz jej zapíná, a
je-li mód zapnut, tak jej tento příkaz vypíná.  Říkáme, že tento příkaz
přepíná ("toggles") tento mód.

>> Napište teď M-x auto-fill-mode <Return>.  Pak vkládejte "asdf " stále
   dokola tak dlouho, až uvidíte, jak se vkládaný řádek rozdělí na dva
   řádky.  Do textu musíte vkládat mezery proto, že Auto Fill mód
   zalamuje řádky pouze v mezerách.

Okraj je obvykle nastaven na 70 znaků, ale můžete to změnit příkazem
C-x f.  Hodnotu okraje, kterou si přejete, byste měli předat jako
numerický argument.

>> Napište C-x f s argumentem 20.  (C-u 2 0 C-x f).
   Pak pište nějaký text a pozorujte, jak Emacs vyplňuje řádky po
   20 znacích.  Pak nastavte okraj zpátky na 70 opětovným použitím
   C-x f.

Jestliže provedete změny uprostřed odstavce, Auto Fill mód jej
nepřeformátuje.
Pro přeformátování odstavce stiskněte M-q (META-q) s kurzorem uvnitř
odstavce.

>> Přesuňte kurzor do předchozího odstavce a stiskněte M-q.


* VYHLEDÁVÁNÍ
-------------

Emacs umí v textu vyhledávat řetězce (tj. skupiny spojených znaků nebo
slov) směrem vpřed nebo vzad.  Hledání řetězce je příkaz přesunující
kurzor; přesune kurzor na nejbližší místo, kde se tento řetězec nachází.

Vyhledávací příkaz Emacsu je "inkrementální".  To znamená, že vyhledávání
se provádí už v okamžiku, kdy zadáváte vyhledávací řetězec.

Příkaz pro zahájení hledání vpřed je C-s a pro hledání vzad C-r.
ALE POZOR!  Nezkoušejte je ihned.

Když stisknete C-s, uvidíte v echo oblasti prompt "I-search".  To vám
říká, že Emacs se nachází ve stavu, který se nazývá inkrementální hledání,
a čeká, až mu zadáte, co chcete hledat.  <Return> hledání ukončí.

>> Nyní zahajte hledání stiskem C-s.  POMALU, písmeno po písmenu, pište
   slovo "kurzor".  Po každém písmenu si všimněte, co se děje s kurzorem.
   Teď jste vyhledali "kurzor" poprvé.
>> Stiskněte C-s znovu, abyste nalezli další výskyt slova "kurzor".
>> Nyní čtyřikrát stiskněte <DEL> a pozorujte, jak se kurzor přesouvá.
>> Stiskněte <Return> pro ukončení hledání.

Viděli jste, co se stalo?  Emacs se v inkrementálním hledání pokouší
přejít na další výskyt řetězce, který jste dosud napsali.  Chcete-li
přejít na další výskyt slova "kurzor", jednoduše stiskněte C-s znovu.
Jestliže už žádný takový výskyt není, Emacs pípne a řekne vám, že
hledání momentálně "selhává", C-g hledání ukončí.

Jestliže uprostřed inkrementálního hledání stisknete <DEL>, uvidíte, že
poslední znak v hledaném řetězci zmizí a hledání se vrací na poslední
místo hledání.  Pokud stisknete <DEL> ihned potom, co jste zmáčkli C-s,
abyste se přesunuli na další výskyt hledaného řetězce, <DEL> vrátí
kurzor zpět na poslední místo hledání.  Pokud neexistují žádné dřívější
výskyty, <DEL> vymaže poslední znak ve vyhledávacím řetězci.
Předpokládejme například, že jste napsali "c", abyste našli první výskyt
"c".  Jestliže nyní stisknete "u", kurzor se přesune na první výskyt
"cu".  Teď stiskněte <DEL>.  To vymaže "u" z hledaného řetězce a
kurzor se přesune zpět na první výskyt "c".

Jestliže uprostřed hledání stisknete CONTROL nebo znak META (s několika
výjimkami -- znaky, které jsou speciální v hledání, jako C-s a C-r),
hledání se ukončí.

C-s zahajuje hledání, které hledá jakýkoliv výskyt hledaného řetězce ZA
aktuální pozicí kurzoru.  Chcete-li něco hledat v předcházejícím textu,
stiskněte C-r místo C-s.  Vše, co jsme řekli o C-s, platí také o C-r
kromě toho, že směr hledání je opačný.


* VÍCE OKEN
-----------

Jednou z pěkných vlastností Emacsu je to, že může na obrazovce zobrazit
více oken současně.  Na tomto místě je důležité podotknout, že Emacs
používá termín "rámy" (angl. "frames") pro to, co ostatní aplikace
nazývají slovem okno (angl. "window").  Manuál k Emacsu obsahuje sekci
Glossary of Emacs terms, kde jsou tyto termíny popsány podrobněji.

>> Přesuňte kurzor na tento řádek a stiskněte C-l C-l.

>> Nyní stiskněte C-x 2, což rozdělí obrazovku na dvě okna.
   Obě okna zobrazují tento tutoriál.  Kurzor zůstává navrchu okna.

>> Tiskněte C-M-v pro scrollování spodního okna.
   (Nemáte-li skutečnou klávesu META, stiskněte <ESC> C-v.)

>> Stiskněte C-x o ("o" jako "other") pro přesun kurzoru do dolního
   okna.

>> Použijte C-v a M-v ve spodním okně pro jeho scrollování.
   Pokračujte ve čtení těchto instrukcí v horním okně.

>> Znovu stiskněte C-x o pro přesun kurzoru zpět do horního okna.
   Kurzor v horním okně je přesně na místě, kde byl původně.

Můžete dále používat C-x o pro přepínání mezi okny.  "Aktivní okno"
(tj. okno, kde provádíte většinu úprav) je to s výrazným kurzorem, který
bliká, když právě nepíšete.  Všechna ostatní okna mají svou vlastní
pozici kurzoru; pokud používáte Emacs v rámci grafického uživatelského
prostředí, tak je poznáte podle toho, že jsou vykresleny jako prázdné
rámečky, které neblikají.

Příkaz C-M-v je velmi užitečný, jestliže v jednom okně editujete text a
druhé okno používáte pouze pro přehled.  Můžete kurzor nechávat stále
v okně, kde editujete, a postupovat po druhém okně pomocí C-M-v.

C-M-v je příkladem CONTROL-META znaku.  Máte-li skutečnou META (nebo
Alt) klávesu, můžete vyvolat C-M-v přidržením obou kláves CONTROL a META
zatímco stisknete klávesu v.  Nezáleží na tom, zda je prvně stisknuta
CONTROL nebo META, protože obě tyto klávesy fungují jako modifikátory
kláves, které tisknete.

Pokud nemáte skutečnou META klávesu, můžete místo ní použít <ESC>, na
pořadí v tomto případě záleží: musíte nejprve stisknout <ESC> a následně
CONTROL-v; CONTROL-<ESC> v by nefungovalo.  To proto, že <ESC> je
samostatný znak, nikoliv modifikátor.

>> Stiskněte C-x 1 (v horním okně), abyste se zbavili dolního okna.

(Kdybyste C-x 1 stiskli v dolním okně, odstranilo by to horní okno.
Chápejte tento příkaz jako "ponechej právě jedno okno -- to, ve kterém
zrovna jste".)

Nemusíte v obou oknech zobrazovat tentýž buffer.  Jestliže použijete
C-x C-f pro vyhledání souboru v jednom z oken, druhé okno se nezmění.
Můžete vyhledávat soubory v obou oknech nezávisle.

Zde je další způsob, jak využít dvě okna ke zobrazení dvou různých věcí:

>> Stiskněte C-x 4 C-f následované jménem některého z vašich souborů.
   Dokončete to pomocí <Return>.  Vidíte zadaný soubor v dolním okně.
   Přesunul se tam i kurzor.

>> Stiskněte C-x o pro přesun zpět do horního okna a C-x 1 pro smazání
   dolního okna.


* VÍCE RÁMÚ
-----------

Emacs může kromě oken vytvořit také více rámů (angl. "frames").  Rám je
kolekce oken, jednotlivých položek v menu, posuvníku, echo oblasti
atd. V rámci grafického uživatelského prostředí "rámem" nazýváme to, co
většina ostatních aplikací nazývá "oknem".  V jeden okamžik můžeme
zobrazit klidně několik grafických rámů.  Na textových terminálech může
být zobrazen pouze jeden rám v jeden okamžik.

>> Stiskněte C-x 5 2.
   Pozorujte, jak se na obrazovce vytvoří nový rám.

V tomto novém (tj. druhém) rámu můžete provádět vše, co jste dělali v
rámu prvním.  V ničem se neliší od prvního rámu.

>> Stiskněte C-x 5 0.
   Tento příkaz odstraní právě zvolený rám.

Jakýkoliv rám je možné také odstranit běžnými metodami, které máte v
rámci grafického uživatelského prostředí k dispozici (např. kliknutím na
tlačítko "X" v horním rohu rámu).  Pokud tímto způsobem odstraníte
poslední rám dané Emacsové instance, dojde k jejímu ukončení.


* REKURZIVNÍ EDITAČNÍ ÚROVNĚ
----------------------------

Občas se dostanete do něčeho, co se nazývá "rekurzivní editační úroveň"
("recursive editing level").  To je indikováno hranatými závorkami ve
stavovém řádku obklopujícími závorky okolo jména hlavního módu.
Například můžete vidět [(Fundamental)] místo (Fundamental).

Abyste se dostali z rekurzivní editační úrovně, stiskněte <ESC> <ESC>
<ESC>.  To je obecný "vyskakovací" příkaz.  Můžete jej použít též pro
odstranění některých oken a vyskočení z minibufferu.

>> Stiskněte M-x, abyste se dostali do minibufferu; pak stiskněte
   <ESC> <ESC> <ESC>, abyste se z něj dostali ven.

Z rekurzivní editační úrovně nemůžete vyskočit pomocí C-g.  To proto, že
C-g je využíváno pro rušení příkazů a argumentů UVNITŘ rekurzivní
editační vrstvy.


* ZÍSKÁNÍ DALŠÍ NÁPOVĚDY
------------------------

V tomto tutoriálu jsme se pokusili poskytnout vám dostatek informací,
abyste mohli začít Emacs okamžitě používat.  V Emacsu je toho ale tolik,
že by bylo nemožné to zde všechno objasnit.  Nicméně se o Emacsu můžete
naučit více, protože má mnoho užitečných vlastností.  Emacs nabízí
příkazy pro čtení dokumentace svých příkazů.  Všechny tyto "help"
příkazy začínají znakem CONTROL-h, který se nazývá "help znak".

Pro použití vlastností nápovědy stiskněte znak C-h a pak znak říkající,
jaký druh nápovědy žádáte.  Jste-li OPRAVDU ztraceni, stiskněte C-h ? a
Emacs vám sdělí, jaké druhy nápovědy vám může poskytnout.  Jestliže
jste stiskli C-h a pak jste se rozhodli, že žádnou nápovědu nechcete,
jednoduše to zrušte stiskem C-g.

(Jestliže C-h nezobrazuje hlášení o nápovědě v dolní části obrazovky,
zkuste místo toho používat klávesu F1 nebo M-x help <Return>.)

Nejzákladnější help příkaz je C-h c.  Stiskněte C-h, znak c a klávesový
příkaz; Emacs pak zobrazí velmi stručný popis příkazu.

>> Stiskněte C-h c C-p.
   Hlášení by mělo vypadat asi takto

	C-p runs the command previous-line

To vám sděluje "jméno funkce".  Jelikož jména funkcí jsou volena tak,
aby naznačovala, co odpovídající příkaz dělá, mohou sloužit také jako
velmi stručná dokumentace -- dostatečná k tomu, aby vám připomenula
příkazy, které jste se již naučili.

Víceznakové příkazy jako C-x C-s a <ESC>v (pokud nemáte META ani ALT
klávesu) jsou po C-h c povoleny také.

K získání více informací o příkazu místo C-h c použijte C-h k.

>> Stiskněte C-h k C-p.

To zobrazí dokumentaci k funkci a její jméno v emacsovém okně.  Až
výstup přečtete, stiskněte C-x 1, abyste se textu nápovědy zbavili.
Nemusíte to dělat hned.  Můžete chvíli editovat a nahlížet do textu
nápovědy a teprve pak stisknout C-x 1.

Zde jsou další užitečné C-h volby:

   C-h x	Popis funkce.  Zadáváte jméno funkce.

>> Zkuste napsat C-h x previous-line <Return>.
   To vypíše veškeré informace, které Emacs má o funkci implementující
   příkaz C-p.

Podobný příkaz C-h v zobrazí dokumentaci proměnné, jejíž hodnotu
můžete nastavit a změnit tím chování Emacsu.  Jméno proměnné zadáte, až
se na ni Emacs zeptá.

   C-h a	Příkazové apropos.  Zadejte klíčové slovo a Emacs vypíše
		všechny příkazy, jejichž jména obsahují toto klíčové
		slovo.  Všechny tyto příkazy mohou být vyvolány pomocí
		META-x.  Pro některé příkazy příkazové apropos vypíše
		také jedno nebo dvouznakové sekvence, které provádějí
		tentýž příkaz.

>> Napište C-h a file <Return>.

To zobrazí v druhém okně seznam všech M-x příkazů obsahujících "file" ve
svém názvu.  Znakové příkazy jako C-x C-f uvidíte vypsané vedle
odpovídajících jmen příkazů jako find-file.

>> Stiskněte C-M-v pro posun okna s nápovědou.  Proveďte to několikrát.

>> Stiskněte C-x 1 pro smazání okna s nápovědou.

   C-h i	Čtení on-line manuálů (též Info).  Tento příkaz
		vás přepne do speciálního bufferu s názvem "*info*",
		ve kterém můžete číst on-line manuály pro balíky
		nainstalované na vašem systému.  Pokud stisknete
		m emacs <Return> můžete si například přečíst manuál
		k Emacsu.  Pokud jste dosud nikdy nepoužívali Info,
		stiskněte ? a Emacs vám představí hlavní možnosti
		módu pro Info.  Až si tyto možnosti prostudujete,
		měli byste používat Info manuál Emacsu jako svoji
		primární dokumentaci.


* VÍCE FUNKCÍ
-------------

O Emacsu se můžete dozvědět více přečtením jeho manuálu. Manuál k Emacsu
je dostupný jak v tištěné, tak i elektronické verzi (použijte menu
nápovědy nebo příkaz C-h r).  Mezi mimořádně užitečné funkce, o kterých
jsme se doposud nezmínili, patří našeptávač, který usnadňuje psaní, a
aplikace "Dired", která zjednodušuje práci s diskovými soubory.

Vestavěný našeptávač vám pomůže vyhnout se nadbytečnému psaní.  Pokud se
například chcete přepnout do bufferu s názvem "*Messages*", můžete
použít C-x b *M<Tab> a Emacs za vás doplní zbylý text kam až to půjde.
Našeptávač také funguje pro názvy souborů a příkazů.  Pokud se o
našeptávači chcete dozvědět více, navštivte sekci "Completion" v
emacsovém manuálu.

Aplikace "Dired" je nástroj, který vám pomůže rychle a snadno získat
seznam souborů v dané složce (a případně i podsložkách) na vašem
počítači, navštívit soubory na kterých vám záleží, přejmenovat je,
smazat či jinak manipulovat.  Aplikace "Dired" je popsána v manuálu ve
stejnojmenné sekci.

Emacs obsahuje mnoho dalších užitečných aplikací a funkcí.


* INSTALACE BALÍKŮ
------------------

Komunita okolo Emacsu vytvořila bohatou sadu balíků, které mohohou Emacs
rozšířit o nové funkce a dovednosti.  Tyto balíky mohou obsahovat
podporu nových jazyků, vzhledů, nástroje pro práci s externími
aplikacemi a mnoho, opravdu mnoho, dalšího.

Pakliže se chcete podívat na seznam všech dostupných balíků, použijte
M-x list-packages.  V tomto seznamu se můžete pohybovat, číst popisy
jednotlivých balíků a případně je i nainstalovat a odinstalovat.


* ZÁVĚR
-------

Pokud chcete Emacs opustit, použijte C-x C-c.

Záměrem tohoto tutoriálu je být srozumitelný všem novým uživatelům,
takže narazíte-li na něco nejasného, tak neusedejte a neklaďte to za
vinu sami sobě -- stěžujte si!


KOPÍROVÁNÍ
----------

Tento tutoriál vychází z dlouhé řady emacsových tutoriálů zahájené
tutoriálem napsaným Stuartem Cracraftem pro původní Emacs.

Tato verze tutoriálu je, podobně jako GNU Emacs, chráněna copyrightem a
je šířena se svolením distribuovat kopie za jistých podmínek
(pozn. překl. oficiální verze licence není v českém překladu k
dispozici):

  Copyright (C) 1985, 1996, 1998, 2001-2026 Free Software Foundation,
  Inc.

  This file is part of GNU Emacs.

  GNU Emacs is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GNU Emacs is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

Přečtěte si prosím soubor COPYING a pak předávejte kopie programu GNU
Emacs svým přátelům.  Pomáhejte potírat softwarovou obstrukci
("vlastnictví") používáním, psaním a sdílením svobodného softwaru!

;;; Local Variables:
;;; coding: utf-8
;;; mode: fundamental
;;; End:
