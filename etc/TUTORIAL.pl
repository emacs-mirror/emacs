Copyright (c) 1985, 2001 Free Software Foundation, Inc. 
Szczegó³y na koñcu pliku.  Czytasz w³a¶nie krótki podrêcznik Emacsa.

Polecenia Emacsa ogólnie wymagaj± wci¶niêcia klawisza CONTROL (czasami
oznaczanego CTRL lub CTL) lub klawisza META (czasami oznaczanego EDIT
lub ALT).  Zamiast pisaæ META czy CONTROL za ka¿dym razem, gdy masz
przycisn±æ ten klawisz, u¿ywaæ bêdziemy nastêpuj±cych skrótów:

 C-<znak> oznacza trzymanie klawisza CONTROL podczas wciskania klawisza <znak>.
          Na przyk³ad C-f bêdzie odpowiada³o naci¶niêciu f, podczas gdy
          klawisz CONTROL by³ wci¶niêty.
 M-<znak> oznacza trzymanie klawisza META wci¶niêtego podczas
          wciskania klawisza <znak>.  Je¶li nie masz klawisza META,
          naci¶nij i pu¶æ klawisz ESC, a potem naci¶nij klawisz <znak>.

Uwaga: by zakoñczyæ sesje Emacsa naci¶nij C-x C-c (dwa znaki).
Znaki ">>" na lewym marginesie w dalszej czê¶ci tego podrêcznika
oznaczaj± æwiczenia dla Ciebie.  Na przyk³ad: 
<<Blank lines inserted here by startup of help-with-tutorial>>
>>  Teraz naci¶nij C-v (nastêpny ekran), by przej¶æ na nastêpny ekran
    podrêcznika (zrób to naciskaj±c jednocze¶nie klawisz CONTROL i v).
    Od tego momentu powiniene¶ robiæ to zawsze, gdy dojdziesz
    do koñca ekranu.

Zwróæ uwagê na to, ze kilka linii powtarza siê, gdy przechodzisz z
ekranu na ekran; zachowanie to ma zapewniæ pewna ci±g³o¶æ podczas
przesuwania siê w obrêbie pliku.

Pierwsza umiejêtno¶ci±, która powiniene¶ opanowaæ, jest sposób
przesuwania siê z miejsca na miejsce.  Ju¿ wiesz, jak przesuwaæ siê o
jeden ekran do przodu.  By przesun±æ siê o jeden ekran do tylu,
wci¶nij M-v (wci¶nij META i naci¶nij v, lub naci¶nij <ESC>v je¶li nie
masz klawisza META lub EDIT).

>>  Spróbuj nacisn±æ M-v, a potem C-v by przesun±æ siê w przód i w ty³
    kilka razy.


PODSUMOWANIE
------------

Nastêpuj±ce polecenia s± u¿yteczne do przegl±dania po jednym ekranie:

	C-v	Przesuñ siê o jeden ekran do przodu
	M-v	Przesuñ siê o jeden ekran do tylu
	C-l	Wyczy¶æ ekran i wy¶wietl go na nowo, umieszczaj±c
                tekst z okolic kursora w ¶rodku ekranu.
                (Ta kombinacja to CONTROL-L, a nie CONTROL-1.)

>> Znajd¼ kursor i zapamiêtaj, jaki tekst jest w jego okolicy.
   Naci¶nij nastêpnie C-l.
   Znajd¼ kursor jeszcze raz i zwróæ uwagê, ¿e znajduje 
   siê on w okolicy tego samego tekstu.


PODSTAWY KIEROWANIA KURSOREM
----------------------------

Przesuwanie siê z ekranu na ekran jest u¿yteczne, ale jak przej¶æ do
okre¶lonego miejsca w obrêbie jednego ekranu?

Mo¿na to zrobiæ na kilka sposobów.  Najprostszym jest u¿ycie poleceñ
C-p, C-b, C-f oraz C-n.  Ka¿de z tych poleceñ przesuwa kursor o jeden
wiersz lub kolumnê w okre¶lonym kierunku.  Oto schemat, który to
obrazuje:

	           Poprzednia linia, C-p
                   (ang. previous line)
                            :
                            :
       Wstecz, C-b ....  Kursor ....  Do przodu, C-f
       (ang. back)          :         (ang. forward)    
                            :
                            :
                    Nastêpna linia, C-n
                     (ang. next line)

>> Przesuñ kursor na ¶rodek schematu za pomoc± C-n lub C-p.  Naci¶nij
   potem C-l, by zobaczyæ ca³y diagram na ¶rodku ekranu.

To s± podstawowe polecenia kieruj±ce po³o¿eniem kursora, których
bêdziesz u¿ywaæ nieustannnie, warto wiêc je zapamiêtaæ.

>> Naci¶nij kilka razy C-n, by przesun±æ kursor do tej linii.

>> Przesuñ siê w g³±b linii za pomoc± C-f, a potem do góry za pomoc±
   C-p.  Zwróæ uwagê na zachowanie siê C-p, gdy kursor jest w ¶rodku
   linii.

Ka¿da linia tekstu koñczy siê znakiem nowej linii, który oddziela ja
od nastêpnej.  Ka¿dy Twój plik powinien koñczyæ siê znakiem nowej
linii (ale Emacs nie zmusza Ciê do tego).

>> Spróbuj nacisn±æ C-b na pocz±tku linii.  Powinno to Ciê przenie¶æ
   na koniec poprzedniej linii.  Dzieje siê tak dlatego, ¿e kursor
   przechodzi nad znakiem nowej linii.

C-f przechodzi nad znakiem nowej linii tak samo jak C-b.

>> Naci¶nij kilka razy C-b, by¶ dostrzeg³, gdzie jest kursor.
   Naci¶nij potem C-f, by wróciæ na koniec linii.  W koñcu naci¶nij
   jeszcze raz C-f, by przej¶æ do nastêpnej linii.

Gdy przesuwasz kursor poza dolna krawêd¼ ekranu, tekst za krawêdzi±
przesuwa siê na ekran (ang. scrolling).  Dziêki temu Emacs mo¿e
przesun±æ kursor do okre¶lonego miejsca bez umieszczania go poza
ekranem.

>> Spróbuj przesun±æ kursor poza dolna granice ekranu za pomoc± C-n i
   zobacz co siê stanie.

Je¶li przesuwanie siê o jeden znak na raz jest dla Ciebie za wolne,
spróbuj przesuwaæ siê o s³owa.  M-f (Meta-f) przesuwa kursor do przodu
o s³owo, a M-b przesuwa go do tylu o jedno s³owo.

>> Spróbuj nacisn±æ kilka M-f i M-b.

Gdy jeste¶ w ¶rodku s³owa, M-f przesuwa kursor na koniec s³owa.  Je¶li
natomiast jeste¶ w przerwie miedzy s³owami, M-f przesuwa kursor na
koniec nastêpnego s³owa.  M-b zachowuje siê podobnie, jak chodzi o
ruch do ty³u.

>> Naci¶nij M-f i M-b kilka razy na przemian z C-f i C-b tak, by¶
   móg³ zauwa¿yæ dzia³anie M-f i M-b naci¶nietych w ró¿nych miejscach
   wewn±trz i pomiêdzy s³owami.

Zauwa¿ podobieñstwo pomiêdzy C-f i C-b oraz M-f i M-b.  Bardzo czêsto
kombinacje zawieraj±ce Meta opisuj± operacje zwi±zane z jednostkami
jêzykowymi (s³owa, zdania, akapity), podczas gdy kombinacje oparte o
Control dzia³aj± na podstawowych jednostkach niezale¿nych od tego, co
edytujesz (znaki, linie, itd.).

Ta zale¿no¶æ stosuje siê do linii i zdañ: C-a i C-e przesuwaj± kursor
na pocz±tek i koniec linii, a M-a i M-e przesuwaj± go na pocz±tek i
koniec zdania.

>> Naci¶nij kilka razy C-a, a potem kilka razy C-e.
   Powtórz to z M-a, a potem z M-e.

Czy zauwa¿y³e¶, ze powtarzanie C-a nic nie zmienia, natomiast powtórne
M-a przesuwa Ciê o jedno zdanie?  Chocia¿ nie jest to do koñca
analogiczne, wydaje siê jednak naturalne.

Po³o¿enie kursora w tek¶cie okre¶lane jest mianem "punktu".

Oto podsumowanie prostych poleceñ s³u¿±cych do przesuwania kursora,
w³±cznie z operacjami dotycz±cymi s³ów i zdañ:

	C-f	Do przodu o jeden znak
	C-b	W ty³ o jeden znak

	M-f	Do przodu o s³owo
	M-b	W ty³ o s³owo

	C-n	Nastêpna linia
	C-p	Poprzednia linia

	C-a	Pocz±tek linii
	C-e	Koniec linii

	M-a	W ty³ na pocz±tek zdania
	M-e	Do przodu na koniec zdania

>> Przeæwicz kilka razy wszystkie powy¿sze polecenia dla wprawy.
   S± one najczê¶ciej u¿ywanymi poleceniami.

Dwa inne wa¿ne polecenia przesuwaj±ce kursor to M-< (Meta i znak
mniejszo¶ci), które przesuwa kursor na pocz±tek ca³ego tekstu i M->
(Meta i znak wiêkszo¶ci), które przesuwa kursor na koniec ca³ego
tekstu.

Na wiêkszo¶ci terminali "<" jest nad przecinkiem, tak wiec musisz u¿yæ
klawisza Shift by nacisn±æ "<".  Musisz wiec tak¿e u¿yæ Shift by
nacisn±æ M-<.  Bez Shift by³oby to M-przecinek.

>> Naci¶nij M-< by przej¶æ na pocz±tek podrêcznika.  U¿yj potem C-v
   kilkukrotnie, by wróciæ tutaj.

>> Teraz naci¶nij M->, by przej¶æ na koniec podrêcznika.  Wróæ do tego
   miejsca za pomoc± kilkukrotnego M-v.

Je¶li Twój terminal ma klawisze strza³ek, to mo¿esz ich u¿yæ do
przesuwania kursora.  Zalecamy Ci nauczenie siê kombinacji C-b, C-f,
C-n i C-p z trzech powodów.  Po pierwsze, dzia³aj± one na wszystkich
typach terminali.  Po drugie, gdy ju¿ zdobêdziesz pewna praktykê w
pos³ugiwaniu siê Emacsem, bêdzie Ci szybciej nacisn±æ te kombinacje
ni¿ klawisze strza³ek (poniewa¿ nie wymaga to przenoszenia d³oni z
miejsca, które zajmuj± podczas szybkiego pisania za pomoc± 10 palców).
Po trzecie wreszcie, gdy ju¿ wyrobisz sobie zwyczaj pos³ugiwania siê
tymi poleceniami z klawiszem Control, bêdziesz móg³ ³atwo nauczyæ siê
innych zaawansowanych poleceñ przesuwaj±cych kursor.

Wiêkszo¶æ poleceñ Emacsa akceptuje argument liczbowy; dla wiêkszo¶ci
poleceñ s³u¿y on jako liczba powtórzeñ.  Sposób, w jaki okre¶lasz
liczbê powtórzeñ polecenia, to naci¶niecie C-u a potem cyfr, zanim
naci¶niesz polecenie.  Je¶li masz klawisz META (lub EDIT lub ALT),
alternatywnym sposobem jest wciskanie klawiszy cyfr podczas
wprowadzania argumentu liczbowego.  Zalecamy nauczenie siê metody
klawisza C-u, poniewa¿ dzia³a ona na wszystkich terminalach.

Na przyk³ad C-u 8 C-f przesuwa kursor do przodu o osiem znaków.
	
>> Spróbuj u¿yæ C-n i C-p z argumentem liczbowym, by przesun±æ kursor
   do jednej z linii w pobli¿u tego zdania za pomoc± tylko jednego
   polecenia.

Wiêkszo¶æ poleceñ u¿ywa argumentu liczbowego jako liczba powtórzeñ.
Jest kilka poleceñ, które u¿ywaj± go w inny sposób.  C-v i M-v s±
w¶ród tych wyj±tków.  Je¶li poda siê im argument, przesuwaj± zawarto¶æ
ekranu w gore lub w dó³ o podana liczbê linii zamiast o tyle¿ ekranów.
Na przyk³ad C-u 4 C-v przewija ekran o 4 linie.

>> Spróbuj nacisn±æ C-u 8 C-v.

To powinno by³o przewin±æ ekran do góry o 8 linii.  Je¶li chcia³by¶
przewin±æ go w dó³, mo¿esz podaæ argument przed poleceniem M-v.

Je¶li u¿ywasz systemu X-Windows, prawdopodobnie po lewej stronie okna
Emacsa znajduje siê prostok±tny obszar, nazywany po angielsku
"scrollbar".  Za jego pomoc± mo¿esz przewijaæ tekst, u¿ywaj±c do tego
celu myszy.

>> Spróbuj nacisn±æ ¶rodkowy klawisz myszy u góry pod¶wietlonego
   obszaru na scrollbarze.  To powinno przewin±æ tekst do miejsca
   okre¶lonego wysoko¶ci±, na której nacisn±³e¶ klawisz myszy.

>> Przesuñ mysz do miejsca oddalonego od górnego koñca scrollbaru
   mniej wiêcej o trzy linie i naci¶nij lewy klawisz myszy kilka razy.


* KIEROWANIE KURSOREM Z X TERMINALA
-----------------------------------

Je¶li masz X terminal, prawdopodobnie ³atwiej Ci bêdzie u¿ywaæ
klawiszy strza³ek po prawej stronie klawiatury do kierowania kursorem.
Klawisze strza³ek w lewo, w prawo, w górê i w dó³ dzia³aj± zgodnie z
oczekiwaniem; odpowiadaj± one dok³adnie C-b, C-f, C-p i C-n, ale s±
³atwiejsze do zapamiêtania.  Mo¿esz tak¿e u¿ywaæ C-lewo i C-prawo by
przesuwaæ siê o s³owa oraz C-góra i C-dó³, by przesuwaæ siê o bloki
(np. akapity, je¶li edytujesz tekst).  Je¶li masz klawisze oznaczone
HOME (lub BEGIN) oraz END, zanios± Ciê one na pocz±tek i koniec linii,
a C-home i C-end na pocz±tek i koniec pliku.  Je¶li Twoja klawiatura
ma klawisze PgUp i PgDn, mo¿esz ich u¿yæ do przesuwania siê o jeden
ekran za jednym razem, tak jak M-v i C-v.

Wszystkie te polecenia akceptuj± argument liczbowy, jak to jest
opisane powy¿ej.  Mo¿esz stosowaæ pewne skróty w celu wpisania tych
argumentów: naci¶nij i trzymaj CONTROL lub META i wpisz liczbê.  Na
przyk³ad, by przesun±æ kursor o 12 s³ów w prawo naci¶nij C-1 C-2
C-prawo.  Zwróæ uwagê, ze jest to ³atwe do wpisania, poniewa¿ nie
musisz puszczaæ klawisza CONTROL podczas wciskania klawiszy.


* GDY EMACS JEST ZABLOKOWANY
----------------------------

Je¶li Emacs przestaje odpowiadaæ na Twoje polecenia, mo¿esz go
bezpiecznie zatrzymaæ przyciskaj±c C-g.  Mo¿esz u¿yæ C-g do przerwania
polecenia, które zabiera zbyt wiele czasu.

Mo¿esz u¿yæ C-g tak¿e, by anulowaæ argument liczbowy lub pocz±tek
polecenia, którego nie chcesz dokañczaæ.

>> Napisz C-u 100 jako argument liczbowy, po czym naci¶nij C-g.  
   Teraz naci¶nij C-f.  Powinno przesun±æ to kursor o tylko jeden
   znak, poniewa¿ anulowa³e¶ argument za pomoc± C-g.

Je¶li nacisn±³e¶ <ESC> przez pomy³kê, mo¿esz tego siê pozbyæ za pomoc±
C-g.


* ZABLOKOWANE POLECENIA
-----------------------

Pewne polecenia Emacsa s± "zablokowane", tak by pocz±tkuj±cy
u¿ytkownicy nie mogli ich wywo³aæ przez przypadek.

Je¶li wywo³asz jedno z zablokowanych poleceñ, Emacs wypisze komunikat
informuj±cy o tym, co to za polecenie, i zapyta Ciê, czy chcesz je
wywo³aæ.

Je¶li naprawdê chcesz wywo³aæ to polecenie, naci¶nij spacje w
odpowiedzi na pytanie.  Je¶li nie chcesz wywo³aæ zablokowanego
polecenia, odpowiedz na pytanie naciskaj±c "n".

>> Napisz `C-x n p' (co jest zablokowanym poleceniem) i odpowiedz "n"
   na zadane pytanie.


* OKNA
------

Emacs mo¿e miêæ kilka okien, ka¿de wy¶wietlaj±ce w³asny tekst.  Zwróæ
uwagê, ze "okno" je¶li chodzi o Emacsa, nie odnosi siê do osobnego
okienka systemu okienkowego, ale do pojedynczego panelu wewn±trz
okienka systemu X-Windows.  (Emacs mo¿e miêæ kilka X-okien, lub
"ramek" w terminologii Emacsa.  Opisane jest to poni¿ej.)

Na tym etapie lepiej jest siê nie zag³êbiaæ w techniki wykorzystuj±ce
kilka okien.  Powiniene¶ jedynie wiedzieæ, w jaki sposób pozbyæ siê
nadmiaru okien, które mog± pojawiæ siê jako sk³adniki systemu pomocy
lub wynik pewnych poleceñ.  Robi siê to w prosty sposób:

	C-x 1	Jedno okno (tzn. zabij wszystkie inne okna).

Kombinacja ta to Control-x, po którym wystêpuje cyfra 1.  C-x 1
powiêksza okno, w którym jest kursor tak, by zajê³o ca³y ekran.
Kasuje to wszystkie inne okna Emacsa.

>> Przesuñ kursor do tej linii i naci¶nij C-u 0 C-l.

(C-l, jak pamiêtasz od¶wie¿a zawarto¶æ ekranu.  Je¶li poda siê temu
poleceniu argument liczbowy, bêdzie to oznacza³o "od¶wie¿ zawarto¶æ
ekranu i umie¶æ bie¿±ca linie o tyle linii od góry ekranu".  Tak wiec
C-u 0 C-1 oznacza "od¶wie¿ ekran, umieszczaj±c bie¿±ca linie na samej
górze".)

>> Naci¶nij Control-x 2
   Zauwa¿ jak okno siê kurczy, podczas gdy nowe okno pojawia siê,
   wy¶wietlaj±c zawarto¶æ tego bufora.

>> Naci¶nij C-x 1 i nowe okno zniknie.


* WSTAWIANIE I USUWANIE
-----------------------

Je¶li chcesz wstawiaæ tekst, po prostu go napisz.  Znaki, które da siê
wy¶wietliæ, takie jak A, 7, *, itd, Emacs traktuje jako tekst i
wstawia natychmiast do bufora.  Naci¶nij <Return> (znak powrotu
karetki), by wstawiæ znak nowej linii.

Ostatni znak, który napisa³e¶ mo¿esz skasowaæ przez naci¶niecie
klawisza <Delete>.  Klawisz ten mo¿e byæ oznaczony "Del".  W pewnych
wypadkach klawisz "Backspace" mo¿e s³u¿yæ za <Delete>, ale nie jest to
regu³±!

Ogólniej, <Delete> usuwa znak bezpo¶rednio przed bie¿±ca pozycj±
kursora.

>> Zrób to teraz: wstaw kilka znaków, po czym usuñ je za pomaca
   kilkukrotnego naci¶niêcia <Delete>.  Nie przejmuj siê tym, 
   ¿e zmieniasz ten plik; nie zmienisz w ten sposób g³ównego pliku
   podrêcznika.  To jest Twoja w³asna kopia.

Gdy linia tekstu staje siê zbyt d³uga, by zmie¶ciæ siê w jednym
wierszu na ekranie, jest ona "kontynuowana" w nastêpnym wierszu
ekranu.  Znak "backslash" (`\') na prawym marginesie pozwala Ci
rozpoznaæ takie linie.

>> Wpisuj jaki¶ tekst tak d³ugo, a¿ dojdziesz do prawego marginesu, i
   potem nie przestawaj.  Zauwa¿ysz, ze pojawi siê linia kontynuacji.

>> U¿yj <Delete> by usun±æ tekst tak, by linia znowu
   mie¶ci³a siê na ekranie.  Linia kontynuacji zniknie.

Znak nowej linii mo¿e byæ kasowany tak, jak ka¿dy inny znak.
Usuniecie znaku nowej linii ³±czy je w jedna.  Je¶li powsta³a w wyniku
tego linia jest zbyt d³uga, by zmie¶ciæ siê na szeroko¶æ ekranu,
zostanie ona wy¶wietlona z lini± kontynuacji.

>> Przesuñ kursor na pocz±tek linii i naci¶nij <Delete>.  Bie¿±ca
   linia zostanie po³±czona z poprzednia.

>> Naci¶nij <Return>, by z powrotem wstawiæ znak nowej linii, który
   skasowa³e¶.

Pamiêtaj, ze wiêkszo¶æ poleceñ Emacsa mo¿e zostaæ wywo³anych z
parametrem liczby powtórzeñ; dotyczy to tak¿e znaków tekstu.  Argument
liczbowy powoduje wstawienie znaku kilkukrotnie.

>>  Spróbuj zrobiæ to teraz -- naci¶nij C-u 8 * by uzyskaæ ********.

Nauczy³e¶ siê ju¿ wiêkszej czê¶ci podstawowych sposobów pisania oraz
poprawiania b³êdów.  W Emacsie mo¿esz usuwaæ równie¿ cale s³owa lub
cale linie.  Oto podsumowanie operacji usuwania znaków:

	<Delete>     usuñ znak bezpo¶rednio przed kursorem
	C-d          usuñ znak bezpo¶rednio za kursorem

	M-<Delete>   wytnij s³owo bezpo¶rednio przed kursorem
	M-d          wytnij nastêpne s³owo bezpo¶rednio za kursorem

	C-k          wytnij zawarto¶æ linii od kursora do jej koñca
	M-k          wytnij wszystkie znaki od kursora do koñca zdania

Zauwa¿, ze <Delete> i C-d w po³±czeniu z M-<Delete> i M-d rozszerzaj±
regule rozpoczêt± przez C-f i M-f (Có¿, <Delete> tak naprawdê nie
wymaga wci¶niêcia Control, ale pomiñmy to milczeniem).  C-k i M-k s±
podobne do C-e i M-e w sensie, ¿e linie s± odpowiednikami zdañ.

Gdy usuwasz wiêcej ni¿ jeden znak naraz, Emacs zachowuje usuniêty
tekst tak, by¶ móg³ go gdzie¶ wstawiæ z powrotem.  Wstawianie
usuniêtego tekstu to "wklejanie".  Mo¿esz wklejaæ usuniêty tekst b±d¼
to w to samo miejsce, z którego zosta³ usuniêty, b±d¼ to w inne
miejsca.  Ten sam tekst mo¿esz wklejaæ kilkukrotnie, w celu uzyskania
wielu kopii.  Poleceniem, które wkleja tekst jest C-y.

Zauwa¿ ró¿nicê pomiêdzy "wycinaniem" i "usuwaniem", polegaj±c± na tym,
ze wyciête rzeczy mog± byæ wklejone na nowo, natomiast usuniête nie.
W ogólno¶ci, polecenia, które kasuj± du¿o tekstu zachowuj± go, podczas
gdy polecenia, które usuwaj± po prostu jeden znak lub puste linie i
przerwy, nie zachowuj± usuniêtego tekstu.

>> Przesuñ kursor na pocz±tek linii, która nie jest pusta.  Naci¶nij
   C-k, by wyci±æ tekst z tej linii.

>> Naci¶nij C-k jeszcze raz.  Zauwa¿, ze wycina to znak nowej linii,
   który znajduje siê za ta linia.

Zwróæ uwagê, ze pojedyncze C-k wycina zawarto¶æ linii, a powtórne C-k
wycina sam± linie tak, ¿e pozosta³e linie przesuwaj± siê do góry.  C-k
traktuje argument liczbowy w specjalny sposób: wycina ono tyle linii
ORAZ ich zawarto¶æ.  To nie jest samo powtarzanie kilka razy C-k.  C-u
2 C-k wycina dwie linie i ich znaki nowej linii; dwukrotne naci¶niecie
C-k nie zrobi³oby tego.

By odzyskaæ ostatnio wyciêty tekst i wstawiæ go w miejsce kursora,
naci¶nij C-y.

>> Twoja kolej.  Naci¶nij C-y, by z powrotem wstawiæ tekst.

Zwróæ uwagê, ze je¶li naci¶niesz C-k kilka razy pod rz±d, ca³y wyciêty
tekst jest zachowywany w jednym kawa³ku tak, ¿e jedno C-y wklei
wszystkie linie.

>> Naci¶nij C-k kilka razy.

By odzyskaæ ten wyciêty tekst...

>> ...naci¶nij C-y.  Przesuñ potem kursor o kilka linii w dó³ i
   naci¶nij C-y jeszcze raz.  Widzisz, ze wstawia to ten sam tekst.

Co zrobiæ, je¶li chcesz wstawiæ tekst, który wcze¶niej wyci±³e¶, a
potem wycinasz cos innego?  C-y wstawia tekst ostatnio wyciêty.
Poprzedni fragment nie jest jednak stracony.  Mo¿esz wróciæ do niego,
u¿ywaj±c polecenia M-y.  Po tym, jak naci¶niesz C-y, by wstawiæ
ostatnio wyciêty tekst, naci¶niecie M-y zastêpuje wstawiony tekst
poprzednio wyciêtym.  Dalsze naciskanie M-y przywo³uje coraz
wcze¶niejsze fragmenty tekstu.  Gdy dojdziesz do tekstu, którego
szuka³e¶, nie musisz robiæ nic, by zosta³ on we w³a¶ciwym miejscu.  Po
prostu kontynuuj edycjê tekstu, pozostawiaj±c wklejony tekst tam,
gdzie siê znajduje.

Je¶li bêdziesz naciska³ M-y wystarczaj±co wiele razy, dojdziesz do
punktu, z którego wystartowa³e¶ (tekst ostatnio wyciêty).

>> Wytnij jak±¶ line, zmieñ pozycjê kursora i wytnij inna.  Naci¶nij
   potem C-y by wstawiæ druga z wyciêtych linii.  Potem naci¶nij M-y,
   i linia ta zostanie zast±piona przez ta pierwsza.  Naci¶nij M-y
   jeszcze kilka razy, by zobaczyæ co siê dzieje.  Powtarzaj to tak
   d³ugo, a¿ druga z linii pojawi siê z powrotem.  Je¶li chcesz,
   mo¿esz pod±æ M-y dodatnie i ujemne argumenty.


* COFNIJ
--------

Je¶li wprowadzisz zmiany do tekstu, a potem dojdziesz do wniosku, ¿e
to by³a pomy³ka, mo¿esz cofn±æ te zmiany za pomoc± polecenia "cofnij"
(ang. undo), C-x u.

C-x u cofa zmiany wprowadzone przez jedno polecenie; je¶li powtórzysz
C-x u kilka razy pod rz±d, ka¿de powtórzenie cofa koleje polecenie.

Od tej regu³y s± dwa wyj±tki: polecenia, które nie zmieniaj± tekstu
nie licz± siê jako polecenia, które mo¿na cofn±æ (zarówno przesuniêcia
kursora, jak i przewijanie tekstu), oraz znaki wstawiane do tekstu
(np.  litery) ³±czone s± w grupy do 20.  (Ma to na celu zredukowanie
liczby naci¶niêæ C-x u, które musia³by¶ wykonaæ, by cofn±æ wstawianie
tekstu.)

>> Wytnij te linie za pomoc± C-k, a potem naci¶nij C-x u i linia
   powinna pojawiæ siê tu z powrotem.

C-_ jest innym sposobem wywo³ania polecenia "cofnij"; dzia³a to
dok³adnie tak samo jak C-x u, jest jedynie ³atwiejsze do naci¶niêcia
kilka razy pod rz±d.  Wada C-_ jest to, ze nie jest to oczywiste w
jaki sposób nacisn±æ te kombinacje na niektórych klawiaturach.  To
w³a¶nie dlatego C-x u jest tak¿e dostêpne.  Na niektórych terminalach
mo¿esz nacisn±æ C-_ poprzez przytrzymanie CTRL i naci¶niecie /.

Argument liczbowy podany przed C-_ lub C-x u okre¶la liczbê powtórzeñ
tego polecenia.


* PLIKI
-------

Aby edytowny przez Ciebie tekst zosta³ nma trwa³e zachowany, musisz
umie¶ciæ go w pliku.  W przeciwnym wypadku zniknie on, gdy Emacs w
którym go edytowa³e¶ zostanie zamkniêty.  Zachowywanie Twojego tekstu
w pliku nazywane bywa "odwiedzaniem" lub "znajdywaniem" pliku (ang.
"visiting" lub "finding").

Odwiedzanie pliku oznacza, ¿e jego zawarto¶æ zostaje wy¶wietlona w
Emacsie.  Bardzo czêsto sprowadza siê to do edycji samego pliku.
Jednak¿e zmiany, które wprowadzasz nie s± trwa³e do momentu, w którym
"zachowasz" plik (ang. save).  Zapobiega to sytuacji, w której
zostawiasz w systemie plik, który zosta³ tylko w po³owie zmieniony, a
tego nie chcesz zrobiæ.  Nawet wtedy, gdy zachowujesz plik, Emacs
zostawia orygina³ zachowany pod inna nazwa na wypadek, gdyby¶ doszed³
do wniosku, ¿e wprowadzone zmiany by³y b³êdne.

Je¶li popatrzysz na dó³ ekranu, zauwa¿ysz linie, która zaczyna i
koñczy siê my¶lnikami i zawiera tekst "Emacs: TUTORIAL".  W tej
czê¶ci ekranu zawsze mo¿esz znale¼æ nazwê pliku, który w³a¶nie
odwiedzasz.  W tej chwili odwiedzasz plik o nazwie "TUTORIAL", który
jest Twoja w³asn± kopi± podrêcznika Emacsa.  Obojêtnie jaki plik
odwiedzisz, jego nazwa pojawi siê dok³adnie w tym miejscu.

Polecenia, które s³u¿± do odwiedzania i zachowywania plików ró¿ni± siê
od innych poleceñ, które pozna³e¶ tym, ¿e sk³adaj± siê one z dwóch
znaków.  Obydwa zaczynaj± siê od znaku Control-x.  Jest mnóstwo
poleceñ, które zaczynaj± siê od tego w³a¶nie znaku; wiele z nich
dotyczy plików, buforów i z tym zwi±zanych rzeczy.  Polecenia te maj±
d³ugo¶æ dwóch, trzech lub czterech znaków.

Kolejn± nowa rzecz± odno¶nie polecenia odwiedzania pliku jest to, ¿e
musisz mu pod±æ nazwê pliku, który chcesz znale¼æ.  Mówimy o tym, ¿e
polecenie "czyta argument z terminala" (w tym wypadku argument jest
nazwa pliku).  Po tym, gdy wpiszesz polecenie

	C-x C-f   znajd¼ plik (ang. find a file)

Emacs poprosi Ciê o wpisanie nazwy pliku.  Nazwa ta pojawia siê w
dolnej linii ekranu.  Liniê tê nazywa siê "minibuforem" (ang.
"minibuffer") wtedy, gdy jest u¿ywana do wprowadzania tego typu
danych.  Do edycji nazwy pliku u¿ywasz zwyk³ych poleceñ Emacsa.

Wprowadzanie nazwy pliku (lub jakichkolwiek innych danych w
minibuforze) mo¿e zostaæ anulowane za pomoc± C-g.

>> Naci¶nij C-x C-f, po czym naci¶nij C-g.  Na skutek tego zniknie
   minibufor oraz przerwane zostanie polecenie C-x C-f, które tego
   minibufora u¿ywa³o.  W rezultacie wiêc nie odwiedzisz ¿adnego
   pliku.

Gdy skoñczysz wpisywaæ nazwê pliku, naci¶nij <Return>, po czym
polecenie C-x C-f zabierze siê do roboty i znajdzie plik, który
wybra³e¶.  Minibufor znika z chwil± zakoñczenia wykonywania polecenia
C-x C-f.

Po chwili zawarto¶æ pliku pojawia siê na ekranie i mo¿esz j± edytowaæ.
Gdy chcesz zachowaæ zmiany, tak by je utrwaliæ, wydaj polecenie

	C-x C-s   zachowaj plik (ang. save).

Kopiuje to tekst z Emacsa do pliku.  Za pierwszym razem gdy to robisz
Emacs zmienia nazwê oryginalnego pliku poprzez dodanie "~" na koñcu
jego nazwy.

Gdy zachowywanie skoñczy siê, Emacs wypisuje nazwê zapisanego pliku.
Pliki powiniene¶ zachowywaæ stosunkowo czêsto, tak by nie straciæ za
du¿o w przypadku za³amania systemu.

>> Naci¶nij C-x C-s by zachowaæ swoja kopie podrêcznika.  Emacs
   powinien wypisaæ "Wrote ...TUTORIAL" na dole ekranu.

UWAGA: W niektórych systemach naci¶niecie C-x C-s zamrozi ekran i nie
zobaczysz ¿adnego tekstu z Emacsa.  Oznacza to, ¿e sk³adowa systemu
operacyjnego, zwana kontrol± przep³ywu (ang. flow control)
przechwyci³a C-s i nie pozwoli³a mu doj¶æ do Emacsa.  By odzyskaæ
kontrole nad ekranem, naci¶nij C-q.  Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podrêczniku
Emacsa.

Mo¿esz odwiedziæ istniej±ce pliki w celu ich edycji lub czytania.
Mo¿esz tak¿e odwiedziæ plik, który jeszcze nie istnieje.  W ten
w³a¶nie sposób tworzy siê w Emacsie nowe pliki: odwied¼ plik, co da Ci
nowe puste miejsce, a potem zacznij wstawiaæ tekst.  Gdy za¿±dasz
zachowania pliku, wtedy Emacs naprawdê utworzy plik z tekstem, który
wpisa³e¶.  Od tego momentu mo¿esz uwa¿aæ, ¿e edytujesz istniej±cy
plik.


* BUFORY
--------

Je¶li odwiedzisz inny plik za pomoc± C-x C-f, poprzedni plik pozostaje
w Emacsie.  Mo¿esz prze³±czyæ siê do niego, odwiedzaj±c go jeszcze raz
za pomoc± C-x C-f.  W ten sposób mo¿esz w Emacsie miêæ ca³kiem du¿o
plików.

>> Utwórz plik o nazwie "foo" za pomoc± C-x C-f foo <Return>.
   Wpisz w niego jaki¶ tekst i zachowaj "foo" za pomoc± C-x C-s.
   W koñcu napisz C-x C-f TUTORIAL <Return>, by wróciæ do podrêcznika.

Emacs przechowuje tekst ka¿dego pliku w obiekcie, zwanym "buforem".
Odwiedzenie pliku tworzy nowy bufor wewn±trz Emacsa.  By zobaczyæ
listê buforów, które istniej± w Twoim Emacsie, naci¶nij

	C-x C-b   lista buforów (ang. list buffers).

>> Naci¶nij C-x C-b.

Zwróæ uwagê, ze ka¿dy bufor ma w³asn± nazwê, mo¿e tak¿e mieæ
skojarzon± z sob± nazwê pliku, który zawiera.  Pewne bufory nie
odpowiadaj± ¿adnym plikom.  Na przyk³ad bufor "*Buffer List*" nie
odwiedza ¿adnego pliku.  Jest to bufor, który zawiera listê buforów
stworzona przez Twoje naci¶niecie C-x C-b.  DOWOLNY tekst, który
ogl±dasz w oknie Emacsa jest zawsze czê¶ci± jakiego¶ bufora.

>> Naci¶nij C-x 1 by pozbyæ siê listy buforów.

Je¶li dokonujesz zmian tekstu w jakim¶ pliku, a potem odwiedzisz inny
plik, zawarto¶æ tego pierwszego NIE jest automatycznie zachowywana.
Zmiany, które wprowadzi³e¶ pozostaj± w Emacsie, w buforze tego¿ pliku.
Tworzenie czy edytowanie innego bufora nie ma ¿adnego wp³ywu na ten
pierwszy.  Jest to bardzo przydatne, ale oznacza tak¿e, ¿e potrzebny
jest Ci wygodny sposób zachowywania zawarto¶ci Twoich buforów.
Prze³±czanie siê z powrotem do pierwszego bufora zawsze przy
wykonywaniu C-x C-f tylko po to, by nacisn±æ tam C-x C-s by³oby
niewygodne.  Dlatego istnieje polecenie:

	C-x s     Zachowaj bufory (ang. save some buffers)

C-x s pyta Ciê, czy chcesz zachowaæ ka¿dy z buforów, w którym
dokona³e¶ pewnych nie zachowanych jeszcze zmian.

>> Wstaw jak±¶ liniê tekstu, a potem naci¶nij C-x s.
   Powiniene¶ zostaæ zapytany o to, czy chcesz zachowaæ bufor
   TUTORIAL.  Odpowiedz na to pytanie twierdz±co naciskaj±c "y".

* U¯YWANIE MENU
---------------

Je¶li siedzisz przy X-terminalu zauwa¿ysz u góry okna Emacsa pasek
menu.  Mo¿esz u¿ywaæ menu by dotrzeæ do najpopularniejszych poleceñ
Emacsa, takich jak "find file".  Na pocz±tku bêdziesz s±dzi³, ze jest
to ³atwiejsze ni¿ klawiatura, poniewa¿ nie musisz uczyæ siê na pamiêæ
kombinacji klawiszy uruchamiaj±cych jakie¶ polecenie.  Gdy ju¿
zaznajomisz siê z Emacsem, bêdziesz móg³ zacz±æ uczyæ siê klawiszy ---
elementy menu pokazuj± kombinacje klawiszy, która wywo³uje dane
polecenie.

Zwróæ uwagê, ze pewne polecenia w menu nie maja jednoznacznych
odpowiedników klawiszowych.  Na przyk³ad menu "Buffers" zawiera listê
wszystkich dostêpnych buforów.  Mo¿esz prze³±czyæ siê do dowolnego z
nich wybieraj±c jego nazwê z menu Buffers.


* U¯YWANIE MYSZY
----------------

Emacs potrafi w pe³ni wykorzystywaæ mysz, je¶li tylko jest uruchomiony
pod X-Windows.  Mo¿esz zmieniaæ pozycje kursora poprzez naci¶niecie
lewego klawisza myszy w po¿±danym miejscu, mo¿esz tak¿e zaznaczaæ
tekst przez przesuniecie myszy z wci¶niêtym lewym klawiszem nad
tekstem, który chcesz zaznaczyæ.  (Innym sposobem jest klikniêcie na
jednym z koñców obszaru, przesuniêcie myszy na drugi koniec i
klikniêcie tam z jednoczesnym wci¶niêciem klawisza Shift.)

By wyci±æ zaznaczony tekst mo¿esz nacisn±æ C-w lub wybraæ Cut z menu
Edit.  Zwróæ uwagê na to, ze *nie* s± to równowa¿ne polecenia: C-w
zapamiêtuje zaznaczony tekst tylko wewn±trz Emacsa (podobnie jak
omówione powy¿ej C-k), natomiast Cut robi to oraz umieszcza ten tekst
w schowku systemu X, sk±d mo¿e on zostaæ pobrany przez inne programy.

By wkleiæ tekst ze schowka systemu X-Windows u¿yj polecenia Paste z
menu Edit.

¦rodkowy klawisz myszy jest czêsto u¿ywany do wybierania elementów,
które s± wy¶wietlone na ekranie.  Na przyk³ad, je¶li uruchomisz Info
(system dokumentacji Emacsa) naciskaj±c C-h i, lub wybieraj±c ten
element z menu Help, przej¶cie pod¶wietlonym po³±czeniem (ang. link)
odbywa siê poprzez naci¶niecie ¶rodkowego klawisza myszy.  Podobnie,
je¶li wpisujesz nazwê pliku (np. podczas wykonywania "Find File") i
naci¶niesz TAB, by zobaczyæ wszystkie mo¿liwe dokoñczenia nazwy,
mo¿esz wybraæ jedno z nich z wy¶wietlonej listy, w³a¶nie naciskaj±c
¶rodkowy klawisz myszy.

Prawy klawisz myszy pokazuje lokalne menu.  Zawarto¶æ tego menu zale¿y
od trybu pracy Emacsa, w którym aktualnie jeste¶, i zawiera kilka
czêsto u¿ywanych poleceñ, tak by by³y one ³atwiejsze w dostêpie.

>> Naci¶nij prawy klawisz myszy

Prawy klawisz myszy musi byæ trzymany, by menu nie znik³o
automatycznie.


* ROZSZERZANIE ZESTAWU POLECEN
------------------------------

Poleceñ Emacsa jest du¿o du¿o wiêcej, ni¿ mo¿na by skojarzyæ
kombinacjami zwyk³ych klawiszy oraz META czy CTRL.  Emacs radzi sobie
z tym za pomoc± polecenia X (ang. eXtend).  Istniej± jego dwa rodzaje:

	C-x	Rozszerzenie o znak.  Nastêpuje po nim jeden znak.
	M-x	Rozszerzenie o nazwane polecenie.  Nastêpuje po nim
                pe³na d³uga nazwa polecenia.  

Polecenia te w ogólno¶ci s± u¿yteczne, ale s± u¿ywane nie tak czêsto
jak polecenia, których ju¿ siê nauczy³e¶.  Mia³e¶ ju¿ okazje poznaæ
dwa z nich: C-x C-f s³u¿±ce do odwiedzania plików oraz C-x C-s do ich
zachowywania.  Innym przyk³adem mo¿e byæ polecenie, które koñczy sesje
Emacsa C-x C-c.  (Nie martw siê, ze mo¿esz w ten sposób straciæ
zmiany, które dokona³e¶; C-x C-c oferuje Ci mo¿liwo¶æ zachowania
ka¿dego ze zmodyfikowanych plików przed zamkniêciem Emacsa.)

C-z jest poleceniem, które wychodzi z Emacsa *na chwile*, tak by¶ móg³
wróciæ do tej samej sesji Emacsa po jakim¶ czasie.

W systemach, w których jest to mo¿liwe, C-z zawiesza proces Emacsa;
powoduje to powrót do pow³oki (ang.  shell), ale nie niszczy Emacsa.
W najpopularniejszych pow³okach mo¿esz wróciæ do Emacsa za pomoc±
polecenia `fg' lub `%emacs'.

W systemach, w których zawieszanie procesów nie dzia³a, C-z tworzy
proces podpow³oki (ang. "subshell"), który dzia³a pod Emacsem i daje
Ci szansê uruchamiania innych programów i powrotu do Emacsa po ich
skoñczeniu; w tych systemach C-z nie wychodzi naprawdê z Emacsa.  W
tych wypadkach normalnym poleceniem powrotu do Emacsa jest wyj¶cie z
podpow³oki za pomoc± "exit".

Polecenia C-x C-c powiniene¶ u¿ywaæ, gdy masz siê wylogowaæ.  Zalecane
jest tak¿e wychodzenie z Emacsa wystartowanego przez np. programy
obs³uguj±ce pocztê elektroniczna lub innego rodzaju narzêdzia,
poniewa¿ mog± one nie wiedzieæ jak sobie poradziæ z zawieszeniem
Emacsa.  Jednak¿e w zwyk³ych okoliczno¶ciach, je¶li nie musisz
wylogowywaæ siê z systemu, lepiej jest zawiesiæ Emacsa za pomoc± C-z
ni¿ z niego wyj¶æ.

Istnieje wiele poleceñ zaczynaj±cych siê od C-x.  Oto lista tych,
których siê ju¿ nauczy³e¶:

	C-x C-f           odwied¼ plik
	C-x C-s           zachowaj plik
	C-x C-b           wy¶wietl listê buforów
	C-x C-c           wyjd¼ z Emacsa
	C-x u             cofnij

Poleceñ podawanych za pomoc± nazwy u¿ywa siê jeszcze rzadziej lub
u¿ywa siê tylko w pewnych trybach.  Przyk³adem mo¿e byæ polecenie
replace-string, które globalnie zastêpuje jeden ³añcuch innym.  Gdy
naciskasz M-x, Emacs czeka na ci±g dalszy, wy¶wietlaj±c "M-x" na dole
ekranu.  Powiniene¶ po tym wpisaæ nazwê polecenia, w tym wypadku
"replace-string".  Napisz tylko "repl s<TAB>", a Emacs dokoñczy nazwê.
Zakoñcz wprowadzanie nazwy przez naci¶niecie klawisza <Return>.

Polecenie replace-string wymaga dwóch argumentów: ³añcucha, który ma
zostaæ zastêpowany i ³añcucha, który ma byæ wstawiony w miejsce tego¿.
Obydwa ³añcuchy musza byæ zakoñczone przyci¶niêciem <Return>.

>> Przesuñ kursor do czystej linii, dwie linie poni¿ej tej.
   Naci¶nij M-x repl s<Return>zmieni<Return>zmodyfikuje<Return>.

Zwróæ uwagê na to, jak ta linia siê zmieni: zast±pi³e¶ s³owem
"zmodyfikuje" ka¿de wyst±pienie s³owa z-m-i-e-n-i poni¿ej pocz±tkowej
pozycji kursora.


* AUTOMATYCZNE ZACHOWYWANIE
---------------------------

Gdy wprowadzisz zmiany do pliku i ich nie zachowasz, mog± one zostaæ
stracone, je¶li Twój komputer przestanie dzia³aæ.  By uchroniæ Ciê
przed tym, Emacs okresowo zapisuje specjalny plik z wprowadzonymi
zmianami.  Plik ten ma znak # na pocz±tku i na koñcu swojej nazwy.  Na
przyk³ad, za³ó¿my, ze Twój plik nazywa siê "hello.c".  Odpowiadaj±cy
mu plik automatycznie zachowywany bêdzie nosi³ nazwê "#hello.c#".  Gdy
zachowujesz plik w zwyk³y sposób, Emacs kasuje plik automatycznie
zachowany.

Je¶li Twój komputer przestanie dzia³aæ, mo¿esz odzyskaæ Twoje dane z
pliku automatycznie zachowanego przez zwykle odwiedzenie pliku (tego,
który edytowa³e¶, a nie pliku automatycznie zachowanego) i napisanie
M-x recover file<return>.  Gdy Emacs zapyta o potwierdzenie, napisz
yes<return> by odzyskaæ dane, które zosta³y automatycznie zachowane.


* OBSZAR ECHA
-------------

Je¶li polecenia dla Emacsa wpisujesz dostatecznie wolno, zostan± one
pokazywane w specjalnym obszarze na dole ekranu, zwanym obszarem echa
(ang. echo area).  Obszar echa zawiera ostatnia dolna linie ekranu.


* LINIA STANU
-------------

Linia, która znajduje siê bezpo¶rednio nad obszarem echa, zwana jest
"lini± trybu" (ang. modeline).  Pokazuje ona tekst podobny do
nastêpuj±cego:

--:**  TUTORIAL         (Fundamental)--L670--58%----------------

Linia ta podaje u¿yteczne informacje o stanie Emacsa i tekstu, który
edytujesz.  Wiesz ju¿, jakie jest znaczenie nazwy pliku: jest to plik,
który odwiedzi³e¶.  --NN%-- opisuje Twoja bie¿±c± pozycje wewn±trz
tekstu; oznacza to, ¿e NN procent tekstu znajduje siê ponad górnym
brzegiem ekranu.  Je¶li pocz±tek pliku znajduje siê na pocz±tku
ekranu, zamiast liczby --00%-- zobaczysz w tym miejscu --Top--.
Podobnie dla koñca tekstu pojawi siê tam napis --Bot-- (od
ang. bottom).  Je¶li wy¶wietlasz tekst na tyle krótki, ze mie¶ci siê w
ca³o¶ci na ekranie, linia stanu poka¿e --All--.

Gwiazdki blisko pocz±tku linii trybu oznaczaj±, ze wprowadzi³e¶ do
tekstu jakie¶ zmiany.  Bezpo¶rednio po odwiedzeniu lub po zachowaniu
pliku nie bêdzie w tym miejscu ¿adnych gwiazdek, a tylko my¶lniki.

Wewn±trz nawiasów znajdziesz informacje na temat trybu edycji, w
którym w³a¶nie jest Emacs.  Domy¶lnym trybem edycji jest tryb
podstawowy (ang. fundamental), który jest trybem (w³a¶nie w tej chwili
u¿ywanym--) u¿ywanym w³a¶nie w tej chwili.  Jest to przyk³ad "trybu
g³ównego" (ang. major mode).

Emacs mo¿e dzia³aæ w wielu trybach g³ównych.  Pewne z nich zosta³y
zaprojektowane do edycji rozmaitych jêzyków i/lub rodzajów tekstu,
takie jak tryb Lispu, tryb tekstowy, itd.  W danej chwili mo¿e byæ
aktywny tylko jeden g³ówny tryb pracy, i to jego nazwa jest
wy¶wietlana w linii trybu w miejscu, w którym teraz jest
"Fundamental".

Ka¿dy z g³ównych trybów edycyjnych mo¿e zmieniæ zachowanie niektórych
poleceñ.  Na przyk³ad, w Emacsie istniej± polecenia s³u¿±ce do
tworzenia komentarzy w programach.  Ka¿dy jêzyk programowania na swój
sposób okre¶la, jak powinien wygl±daæ komentarz, tak wiec ka¿dy z
g³ównych trybów edycyjnych musi wstawiaæ komentarze w specyficzny
sposób.  Ka¿dy tryb edycyjny jest nazw± polecenia, które mo¿esz
wykonaæ, by prze³±czyæ siê w ten tryb lub wy³±czyæ ten tryb.
Przyk³adem mo¿e byæ M-x fundamental-mode, które jest poleceniem
prze³±czaj±cym tryb podstawowy.

Je¶li zamierzasz edytowaæ tekst w jêzyku angielskim, taki jak na
przyk³ad oryginalna wersja tego podrêcznika, prawdopodobnie powiniene¶
u¿ywaæ trybu tekstowego (ang. text mode).

>> Napisz M-x text-mode<Return>.

Nie musisz siê martwiæ, ¿adne z poleceñ, które do tej pory pozna³e¶,
nie zmienia Emacsa w powa¿ny sposób.  Mo¿esz jednak zauwa¿yæ, ze teraz
M-f i M-b traktuj± apostrofy jako czê¶ci s³ów.  Poprzednio, w trybie
podstawowym, polecenia te traktowa³y apostrofy jako separatory s³ów.

G³ówne tryby edycji wprowadzaj± zwykle subtelne zmiany, takie jak
opisana powy¿ej: wiêkszo¶æ poleceñ robi dalej "to samo", robi to
jednak w sposób troszeczkê inny.

By zobaczyæ dokumentacjê na temat bie¿±cego g³ównego trybu edycji,
naci¶nij C-h m.

>> Naci¶nij C-u C-v raz lub wiêcej razy tak, by ta linia znalaz³a siê
   blisko góry ekranu.

>> Naci¶nij C-h m, by zobaczyæ jak tryb tekstowy ró¿ni siê od trybu
   podstawowego. 

>> Naci¶nij q, by usun±æ dokumentacje z ekranu.

G³ówne tryby edycji nazywaj± siê "g³ównymi", poniewa¿ s± tak¿e
podrzêdne tryby edycji (ang. minor modes).  Podrzêdne tryby edycji nie
s± alternatyw± dla g³ównych trybów edycji, a jedynie ich niewielk±
modyfikacj±.  Ka¿dy podrzêdny tryb edycji mo¿e zostaæ w³±czony lub
wy³±czony niezale¿nie od pozosta³ych podrzêdnych trybów edycji oraz
niezale¿nie od g³ównego trybu edycji.  Mo¿esz wiec u¿ywaæ jednego,
kombinacji dowolnych, lub nie u¿ywaæ ¿adnego trybu podrzêdnego.

Jednym z podrzêdnych trybów edycji, który jest bardzo u¿yteczny
szczególnie do edycji tekstu angielskiego, jest tryb automatycznego
wype³niania (ang. auto fill mode).  Je¶li ten tryb jest w³±czony,
Emacs lamie linie pomiêdzy s³owami automatycznie, gdy wstawiasz tekst
i linia robi siê za szeroka.

Tryb automatycznego wstawiania w³±cza siê na przyk³ad poprzez
wywo³anie polecenia M-x auto-fill-mode<Return>.  Je¶li ten tryb jest
w³±czony to samo polecenie wy³±cza go, i vice versa.  Mówimy, ze
polecenie to "prze³±cza ten tryb".

>> Napisz M-x auto-fill-mode<Return>.  Wstawiaj potem liniê pe³n±
   "asdf " tak d³ugo, a¿ zobaczysz, ¿e siê podzieli na dwie linie.
   Musisz wstawiæ spacje pomiêdzy znaki, poniewa¿ tryb automatycznego
   wype³niania ³amie linie tylko tam, gdzie s± spacje.

Margines jest zazwyczaj ustawiony na 70 znaków, ale mo¿esz go zmieniæ
za pomoc± polecenia C-x f.  Powiniene¶ podaæ mu argument liczbowy
mówi±cy, w której kolumnie ma zostaæ ustawiony margines.

>> Wywo³aj C-x f z argumentem równym 20. (C-u 2 0 C-x f).
   Napisz potem jaki¶ tekst i zauwa¿, ze Emacs wype³nia linie do
   d³ugo¶ci co najwy¿ej 20 znaków.  Ustaw margines z powrotem na
   70 znaków, wywo³uj±c jeszcze raz C-x f.

Je¶li dokonujesz zmian wewn±trz akapitu, tryb 
automatycznego wype³niania nie wyrówna marginesu
sam z siebie.  By wywo³aæ polecenie
wyrównania marginesu, naci¶nij M-q (Meta-q), 
podczas gdy kursor znajduje siê wewn±trz akapitu.

>> Przesuñ kursor do poprzedniego akapitu i naci¶nij M-q.


* SZUKANIE
----------

Emacs potrafi szukaæ ³añcuchów (zwartych ci±gów znaków lub s³ów)
zarówno wstecz jaki i do przodu.  Szukanie ³añcucha jest poleceniem,
które przesuwa kursor; przesuwa ono kursor do nastêpnego miejsca, w
którym dany ³añcuch wystêpuje.

Polecenie Emacsa "search" ró¿ni siê od podobnych poleceñ innych
edytorów w tym sensie, ze jest ono przyrostowe.  Znaczy to, ze
szukanie odbywa siê w trakcie, gdy Ty wpisujesz kolejne znaki
³añcucha, który ma zostaæ odnaleziony.

Poleceniami zapocz±tkowuj±cymi szukanie s±: C-s dla szukania w przód
oraz C-r dla szukania wstecz.  POCZEKAJ PROSZÊ!  Nie próbuj ich w tej
chwili.

Gdy naci¶niesz C-s zauwa¿ysz, ze tekst "I-search" pojawi siê w
obszarze echa.  Informuje Ciê to, ¿e Emacs znajduje siê w trybie
"incremental search", czekaj±c by¶ napisa³ tekst, który ma on znale¼æ.
Naci¶niecie <Return> koñczy proces szukania.

>> Teraz naci¶nij C-s, by rozpocz±æ szukanie.  POWOLI, litera po
   literze, napisz s³owo "kursor", zatrzymuj±c siê po ka¿dym znaku i
   obserwuj±c, gdzie zatrzymuje siê kursor.  Gdy naci¶niesz drugie
   "r", bêdzie mo¿na powiedzieæ, ¿e szuka³e¶ s³owa "kursor"
   jednokrotnie.  Naci¶nij C-s jeszcze raz, by znale¼æ nastêpne
   wyst±pienie s³owa "kursor".  Naci¶nij teraz <Delete> cztery
   razy i zobacz, co siê dzieje z kursorem.  Naci¶nij <RET>, by skoñczyæ
   szukanie.

Widzia³e¶, co siê dzia³o? Emacs podczas szukania przyrostowego próbuje
przej¶æ do miejsca wyst±pienia ³añcucha, który do tej pory wpisa³e¶,
pod¶wietlaj±c go dla Twojej wygody.  By znale¼æ nastêpne wyst±pienie
s³owa "kursor", po prostu naci¶nij C-s jeszcze raz.  Je¶li takiego nie
ma, Emacs zapiszczy i powie Ci, ze szukanie "skoñczy³o siê pora¿k±".
Naci¶niecie C-g tak¿e przerywa proces szukania.

UWAGA: W niektórych systemach naci¶niecie C-s zamrozi ekran i nie
zobaczysz ¿adnego tekstu z Emacsa.  Oznacza to, ¿e sk³adowa systemu
operacyjnego, zwana kontrol± przep³ywu (ang. "flow control")
przechwyci³a C-s i nie pozwoli³a mu dojsæ do Emacsa.  By odzyskaæ
kontrole nad ekranem, nacisnij C-q.  Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podrêczniku
Emacsa.

Jesli podczas szukania przyrostowego naci¶niesz <Delete> zauwa¿ysz, ze
ostatni znak, który wcisn±³es znika i kursor wraca do poprzedniego
miejsca.  Na przyk³ad, za³ó¿my, ze nacisn±³es "k" i znalaz³es pierwsze
wyst±pienie tej litery.  Jesli teraz naci¶niesz "u", kursor przesunie
siê do pierwszego wyst±pienia "ku".  Teraz nacisnij <Delete>.  Skasuje
to "u" z ³añcucha, którego poszukujesz, a kursor wróci do pierwszego
wyst±pienia "k".

Je¶li podczas szukania nacisniesz jaki¶ klawisz razem z META lub CTRL
(z nielicznymi wyj±tkami --- znakami, które maj± specjalne znaczenie
podczas szukania, takimi jak C-s i C-r) szukanie zostanie przerwane.

C-s rozpoczyna proces szukania, który poszukuje ³añcucha, który
znajduje siê ZA bie¿±c± pozycja kursora.  Je¶li chcesz szukaæ czego¶
wcze¶niej w tek¶cie, naci¶nij C-r.  Wszystko, co powiedzieli¶my o C-s
stosuje siê do C-r, oczywi¶cie ze zmian± kierunku szukania na wstecz.


* WIELE OKIEN
-------------

Jedn± z przyjemnych cech Emacsa jest mo¿liwo¶æ wy¶wietlania wiêcej ni¿
jednego okna na raz.

>> Przesuñ kursor do tej linii i naci¶nij C-u 0 C-l.

>> Naci¶nij teraz C-x 2, co podzieli ekran na dwa okna.  Obydwa okna
   wy¶wietlaj± ten podrêcznik.  Kursor pozostaje w górnym oknie.

>> Naci¶nij C-M-v by przewin±æ dolne okno.  (Je¶li nie masz
   prawdziwego klawisza Meta, naci¶nij ESC C-v.)

>> Naci¶nij C-x o ("o" jak angielskie "other") by przesun±æ kursor do
   dolnego okna.  U¿yj C-v i M-v w dolnym oknie by przewin±æ jego
   zawarto¶æ.  Polecenia, które masz wykonaæ czytaj w górnym oknie.

>> Naci¶nij C-x o jeszcze raz tak, by kursor wróci³ do górnego okna.
   Kursor w górnym oknie nie zmieni³ po³o¿enia.

Ka¿de okno pamiêta po³o¿enie swojego w³asnego kursora, lecz tylko
jedno okno w danej chwili wy¶wietla kursor.  Wszystkie polecenia
edycyjne stosuj± siê do okna, w którym jest kursor.  To okno nazywane
jest "wybranym oknem".

Polecenie C-M-v jest bardzo u¿yteczne gdy edytujesz tekst w jednym
oknie, a drugiego u¿ywasz tylko jako punkt odniesienia.  Dziêki temu
kursor mo¿e zawsze znajdowaæ siê w oknie, zawarto¶æ którego edytujesz,
a Ty mo¿esz przesuwaæ drugie okno za pomoc± C-M-v.

C-M-v to przyk³ad znaku, który uzyskuje siê za pomoc± CONTROL-META.
Je¶li masz prawdziwy klawisz META, C-M-v mo¿esz uzyskaæ przytrzymuj±c
jednocze¶nie CTRL oraz META i naciskaj±c v.  Nie jest wa¿ne, co
zosta³o naci¶niete wcze¶niej, CTRL czy META, poniewa¿ obydwa te
klawisze dzia³aj± jako modyfikatory znaczenia klawiszy, które
naciskasz.

Je¶li nie masz klawisza META i u¿ywasz w jego zastêpstwie ESC,
kolejno¶æ naciskania klawiszy jest znacz±ca: musisz najpierw nacisn±æ
i pu¶ciæ ESC, po czym nacisn±æ CTRL-v; CTRL-ESC v nie bêdzie dzia³aæ.
Dzieje siê tak dlatego, ze ESC jest znakiem, a nie modyfikatorem.

>> Naci¶nij C-x 1 (w górnym oknie), by pozbyæ siê dolnego okna.

(Je¶li nacisn±³by¶ C-x 1 w dolnym oknie, to znik³oby górne.  Mo¿esz
sobie t³umaczyæ to polecenie jako "zatrzymaj tylko jedno okno --- to w
którym w³a¶nie jestem".)

Nie musisz wy¶wietlaæ tego samego bufora w obydwu oknach.  Je¶li
u¿yjesz C-x C-f by wy¶wietliæ plik w jednym z okien, zawarto¶æ
drugiego nie zmieni siê.  W ka¿dym oknie mo¿esz wy¶wietlaæ ró¿ne pliki
niezale¿nie.

Oto inny sposób u¿ywania dwóch okien do wy¶wietlania dwóch ró¿nych
rzeczy:

>> Naci¶nij C-x 4 C-f i nazwê jednego z Twoich plików.  Zakoñcz
   wprowadzanie klawiszem <Return>.  Podany plik pojawi siê w dolnym
   oknie razem z kursorem, który tam przeskakuje.

>> Naci¶nij C-x o, by wróciæ do górnego okna, oraz C-x 1 by
   usun±æ dolne okno.


* REKURSYWNE POZIOMY EDYCJI
---------------------------

Czasami mo¿esz znale¼æ siê w czym¶, co nazywa siê "rekursywnym
poziomem edycji".  Mo¿esz to rozpoznaæ po nawiasach kwadratowych w
linii trybu, obejmuj±cych nawiasy okr±g³e zawieraj±ce nazwê g³ównego
trybu edycji.  Na przyk³ad, móg³by¶ widzieæ [(Fundamental)] zamiast
(Fundamental).

By wyj¶æ z rekursywnego poziomu edycji naci¶nij ESC ESC ESC.  Jest to
ogólnego przeznaczenia "wychodzimy".  Mo¿esz go u¿yæ tak¿e by pozbyæ
siê nadmiaru okien lub wyj¶æ z minibufora.

>> Naci¶nij M-x by wej¶æ do minibufora; naci¶nij potem ESC ESC ESC, by
   z niego wyj¶æ.

Nie mo¿esz u¿yæ C-g, by wyj¶æ z rekursywnego poziomu edycji.  Dzieje
siê tak dlatego, ze C-g jest u¿ywane do anulowania poleceñ i
argumentów WEWN¡TRZ rekursywnego poziomu edycji.


SZUKANIE DODATKOWEJ POMOCY
--------------------------

W tym podrêczniku spróbowali¶my dostarczyæ tylko tyle informacji, ile
jest niezbêdne, by¶ móg³ zacz±æ u¿ywaæ Emacsa.  Emacs jest istn±
kopalni± najró¿niejszych rzeczy, których nie sposób tutaj opisaæ.
Zapewne bêdziesz chcia³ dowiedzieæ siê wiêcej o Emacsie, poniewa¿
posiada on wiele po¿±danych cech, o których na razie nic nie wiesz.
Jest w nim zaszyte mnóstwo wewnêtrznej dokumentacji, która mo¿e byæ
osi±gniêta za pomoc± Control-h, które okre¶lamy mianem "znaku pomocy"
z powodu spe³nianej przez niego roli.

By uzyskaæ pomoc, naci¶nij C-h a potem znak, który okre¶la jakiego
typu pomocy oczekujesz.  Je¶li poczujesz siê NAPRAWDÊ zagubiony,
napisz C-h ? i Emacs spróbuje powiedzieæ Ci, jakiego typu pomocy mo¿e
Ci dostarczyæ.  Je¶li naci¶niesz C-h a potem zadecydujesz, ¿e pomoc
nie jest Ci jednak potrzebna, po prostu wci¶nij C-g by anulowaæ C-h.

Najprostsz± pomoc mo¿esz uzyskaæ naciskaj±c C-h c.  Naci¶nij C-h a
potem c, po czym kombinacje klawiszy polecenia, i Emacs wy¶wietli
bardzo krótki opis polecenia.

>> Naci¶nij C-h c Control-p.
   Powinno to przywo³aæ komunikat, o tre¶ci podobnej do

	C-p runs the command previous-line

W ten sposób mo¿esz uzyskaæ "nazwê funkcji".  Przydaje siê to podczas
pisania kodu w Lispie, który rozszerza Emacsa; wystarcza to tak¿e do
przypomnienia Ci, co dane polecenie robi, je¶li widzia³e¶ je ju¿
wcze¶niej, ale nie zapamiêta³e¶ go.

Polecenia wywo³ywane za pomoc± wieloznakowej kombinacji klawiszy, na
przyk³ad C-x C-s oraz (je¶li nie masz klawisza META lub EDIT) <ESC>v,
s± tak¿e dopuszczalne po C-h c.

By uzyskaæ wiêcej informacji na temat polecenia, naci¶nij C-h k
zamiast C-h c.

>> Naci¶nij C-h k Control-p.

To polecenie wy¶wietla dokumentacjê na temat danej funkcji oraz jej
nazwê w oknie Emacsa.  Gdy skoñczysz ¶ledziæ wynik tego polecenia
naci¶nij C-x 1, by pozbyæ siê tekstu pomocy.  Nie musisz tego robiæ od
razu.  Mo¿esz wykonaæ pewne operacje w oparciu o tekst pomocy zanim
naci¶niesz C-x 1.

Oto kilka innych u¿ytecznych wariantów C-h:

   C-h f	Opisz funkcje o podanej nazwie.

>> Napisz C-h f previous-line<Return>.  Wypisze to na ekranie ca³±
   informacje, jak± Emacs ma na temat funkcji, która implementuje
   polecenie C-p.

Podobnie komenda C-h v pokazuje na ekranie dokumentacjê zmiennych, których
wartosci mozesz zmieniæ, aby dostosowaæ Emacsa do swoich
preferencji. Wpisz nazwê zmiennej, gdy Emacs o ni± poprosi.


   C-h a	Apropos.   Wpisz s³owo kluczowe, a Emacs wypisze listê
                wszystkich poleceñ, których nazwa zawiera to s³owo.
                Polecenia te mog± zostaæ wywo³ane za pomoc± Meta-x.
                Dla niektórych poleceñ Apropos wypisze jedno- lub
                dwuznakowe sekwencje, które wywo³uj± dane polecenie.

>> Napisz C-h a file<Return>.  Zobaczysz listê wszystkich poleceñ,
   dostêpnych za pomoc± M-x, które maja s³owo "file" w swojej nazwie.
   Zauwa¿ysz tam tak¿e polecenia takie, jak C-x C-f oraz C-x C-w,
   umieszczone obok nazw poleceñ "find-file" i "write-file".

>> Napisz C-M-v, aby przewin±æ okno pomocy. Zrób to kilka razy.
>> Napisz C-x 1, aby usun±æ okno pomocy.

   C-h i 	Czytanie elektronicznych podrêczników (w formacie Info). To
         	polecenie prze³±czy ciê do specjalnego bufora o nazwie
         	*info*, gdzie bedziesz móg³ przeczytaæ podrêczniki
		dotycz±ce pakietów zainstalowanych w twoim
		systemie. Napisz m emacs <Return>, aby zapoznaæ siê z
		podrêcznikiem Emacsa.  Je¿eli nigdy wczesniej nie u¿ywa³es
		Info, napisz ? a Emacs przedstawi ci mo¿liwo¶ci
		tego trybu. Po tym, jak zapoznasz siê z tym krótkim
		podrêcznikiem w dalszej pracy podstawow± dokumentacjê
		stanowiæ bêdzie w³a¶nie Emacs Info.


PODSUMOWANIE
------------

Pamiêtaj, ¿e by wyj¶æ z Emacsa na sta³e, u¿ywaj C-x C-c.  By wyj¶æ do
pow³oki na chwilê tak, by¶ móg³ wróciæ, u¿yj C-z. (To nie dzia³a pod
X-Windows, poniewa¿ tam nie ma prawdziwego konceptu przej¶cia na
chwile do pow³oki.  Zamiast tego C-z ikonizuje okno Emacsa.)

Ten podrêcznik by³ pisany tak, by wszyscy nowi u¿ytkownicy mogli go
zrozumieæ.  Je¶li co¶ pozostawi³ niejasnym, nie sied¼ cicho i nie
obwiniaj siebie, tylko daj nam znaæ!


KOPIOWANIE
----------

Niniejszy podrêcznik jest potomkiem w d³ugiej linii podrêczników
Emacsa, która rozpoczyna siê od tego, który zosta³ napisany przez
Stuarta Cracrafta dla oryginalnego Emacsa.  Zosta³ on zmodyfikowany we
wrze¶niu 1994 przez Bena Winga, który zaktualizowa³ go, je¶li chodzi o
X-Windows.

T³umaczenia na jêzyk polski dokona³ Remek Trzaska z pomoc± Ryszarda
Kubiaka.  Jesli polskie znaki nie byly poprawnie wyswietlane w tym
buforze, oznacza to, ze nie masz zainstalowanych polskich fontow. 
Pomoc w tym zakresie mozesz znalezc pod adresem: 
               <URL:http://www.agh.edu.pl/ogonki>

T³umaczenie to zosta³o uaktualnione dla wersji GNU Emacs 21
przez Beatê Wierzcho³owsk± <beataw@orient.uw.edu.pl> z pomoc±
Janusza S. Bienia <jsbien@mail.uw.edu.pl>

Ta wersja podrêcznika, podobnie jak GNU Emacs, jest zastrze¿ona, a
pozwolenie na kopiowanie udzielone jest pod nastêpuj±cymi warunkami:

Copyright (c) 1985, 1994, 2001 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim
   copies of this document as received, in any medium, provided that
   the copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

Warunki kopiowania samego Emacsa s± w pewnym stopniu inne, aczkolwiek
zachowuj± te sama idee.  Proszê, przeczytaj plik COPYING, po czym
rozdaj swoim znajomym kopie Emacsa.  Pomó¿ zwalczyæ przeszkody w
rozpowszechnianiu oprogramowania przez tworzenie i dzielenie siê
oprogramowaniem.

;;; Local Variables:
;;;   mode: fundamental
;;;   coding: latin-2
;;; End:
