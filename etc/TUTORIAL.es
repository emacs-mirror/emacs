Copyright (c) 1985 Free Software Foundation, Inc;  Vea al final las 
condiciones. Usted está viendo el tutorial de Emacs.

Los comandos de Emacs generalmente ivolucran a la tecla CONTROL (algunas
veces llamada CTRL O CTL) o la tecla meta algunas veces llamada (EDIT o 
ALT). En lugar de escribir esto completamente en cada ocasión, usaremos 
las siguientes abreviaciones.

 C-<chr> significa mantener presionada la tecla CONTROL mientras digita el
         caracter <chr>. Por lo tanto C-f será: Mantenga presionada la 
	 tecla CONTROL y digite f.
 M-<chr> significa mantega presionada la tecla META o EDIT o ALT mientras
 	 digita <chr>. Si no hay teclas META, EDIT o ALT, en su lugar 
	 presione y libere la tecla <esc> y luego digite <CHR>. 
	 Escribimos <ESC> para la tecla ESC.

Nota importante: para finalizar la sesión de Emacs presione C-x C-c. 
(dos caracteres.) los caracteres ">>" al margen izquierdo indican 
instrucciones para que usted trate de usar un comando. Por ejemplo:  
<<Blank lines inserted here by startup of help-with-tutorial>>
>> Ahora digite C-v (ver la proxima pantalla) para desplazarse a la 
	siguiente pantalla  (hágalo manteniendo la tecla control oprimida
	mientras digita v). 
	Desde ahora usted deberia hacer esto cada vez que termine de 
	leer la pantalla

Note que se sobrelapan dos líneas cuando usted se mueve de pantalla en 
pantalla; esto proveé una continuidad para que pueda seguir leyendo el 
texto.

La primera cosa que necesita saber es como moverse de un lugar a otro en 
el texto. Usted ya sabe como adelantar una pantalla con C-v. Para mover 
atras una pantalla oprima M-v (mantega oprimida la tecla META y digite v,
o presione <ESC>v si no tiene las teclas META, EDIT o ALT).

>>  Intente oprimir M-v y luego C-v, varias veces.

* RESUMEN
---------
        
Los siguentes comandos son útiles para ver pantallas completas:

	C-v 	Avanzar una pantalla completa
	M-v 	Retroceder una pantalla completa
	C-l 	Limpiar la pantalla y mostrar todo el texto de nuevo, 
		moviendo el texto alrededor del cursor al centro de la 
		pantalla (Esto es control-L, no control-1.)

>> Encuentre el cursor,y note cual texto está cerca a este.
   Luego oprima C-l.
   Encuentre el cursor otra vez y note que el mismo texto está cerca al 
   cursor ahora.

* MOVIMIENTO BÁSICO DE CURSOR
-----------------------------

Moverse de una pantalla completa a otra es útil, pero cómo moverse a
un lugar específico dentro del texto en la pantalla?

Hay varias formas para que pueda hacerlo.  La forma más básica es usar los
comandos C-p, C-b, C-f, y C-n.  Cada uno de estos comandos mueve el cursor
una fila o columna en una dirección particular en la pantalla.
Aquí hay una tabla mostrando estos cuatro comandos y las direcciones en 
que ellos mueven
                               
		        Línea Anterior, C-P
				:
				:
   Atrás, C-b.... Posición actual del cursor .... Adelante, C-f 
				:
				:
			Línea siguiente, C-n

>> Mueva el cursor a la línea en la mitad del diagrama 
   usando C-n o C-p. Luego presione C-l para ver el 
   diagrama completo centrado en la pantalla.

Usted probablemente encontrará fácil recordarlos mediante las letras
P de Previous (anterior), N de Next (siguiente), B de Backward (atrás) 
y F de Forward (adelante).
Estos son los comandos básicos de posicionamiento del cursor y estará 
usándolos TODO el tiempo, serán de gran beneficio si los aprende a usar 
ahora.

>> Presione unas veces C-n para traer el cursor a esta línea.

>> Muévase en la línea con C-f y hacia arriba con C-p. 
   Observe lo que hace C-p cuando el cursor está en la mitad de la línea.

Cada línea de texto termina con un caracter de nueva línea, esto sirve 
para separarla de la siguiente línea. La última línea en su archivo debe 
tener una nueva línea al final (pero Emacs no requiere que esta lo tenga).

>> Intente usar C-b al comienzo de una línea. Debería moverse al 
   final de la línea previa. Esto es porque retrocede antes del caracter 
   de nueva línea.

C-f puede moverse por una nueva línea justo como C-b.

>> Presione algunas C-b más para que entienda donde se encuentra el cursor.
   Luego presione C-f para retornar al final de la línea.
   Al final presione una vez mas C-f para moverse a la siguiente línea.

Cuando pase el tope o el final de la pantalla, el texto más alla del borde
de la pantalla se mostrará. Esto es llamado "desplazamiento"  Esto le 
permite a Emacs mover el cursor al lugar especificado en el texto sin 
moverlo fuera de la pantalla.

>> Intente mover  el cursor fuera del  borde de la pantalla  con C-n y
   observe qué pasa.

Si moverse por caracteres es muy lento, se puede mover por palabras. M-f
(META-F) mueve adelante una palabra y M-b mueva atrás una palabra.

>> Oprima unas veces M-f y M-b

Cuando está en la mitad de una palabra, M-f mueve al final de la palabra.
Cuando está en un espacio entre dos palabras, M-f mueve al final de la
siguiente palabra.  M-b trabaja de la misma forma en la dirección opuesta.

>> Oprima unas veces M-f y M-b, intercale con C-f y C-b de tal forma que 
   observe la acción de M-f y M-b desde varios sitios dentro y entre 
   palabras.

Note el paralelo entre C-f y C-b de un lado y M-f y M-b del otro. 
Muy frecuentemente los caracteres Meta son usados para operaciones 
relacionadas, con las unidaes definidas por el lenguaje
(palabras, oraciones y parrafos), mientras los caracteres Control operan 
sobre unidades que son independientes de lo que usted está
editando (caracteres, líneas, etc).

Este paralelo se aplica entre líneas y oraciones: C-a y C-e para moverse
al comienzo o al final de la línea; y M-a y M-e para mover al comienzo o
al final de una oración.

>> Intente unas veces C-a, y a continuación C-e.
>> Intente unos M-a, y otros M-e.

Mire cómo la repeticion de C-a no hace nada, pero la repeticion de M-a 
sigue moviendo una oración adelante, aunque no son muy análogas, cada una
parece natural.

La localización del cursor en el texto es también llamada "punto", en 
otras palabras el cursor muestra sobre la pantalla donde esta el punto
localizado dentro del texto.

Aquí hay un resumen de operaciones simples del movimiento del cursor, 
incluyendo los comandos de movimiento en oracion y palabra:     
				  			  
	C-f 	Moverse adelante un caracter
	C-d 	Moverse atrás un caracter

	M-f 	Moverse adelante una palabra
	M-b 	Moverse atrás una palabra

	C-n 	Moverse a la línea siguiente
	C-p 	Moverse a la línea anterior

	C-a 	Moverse al comienzo de la línea
	C-e 	Moverse al final de la línea

	M-a 	Moverse al comienzo de la oración
	M-e 	Moverse al final de la oración

>> Pruebe todos estos comandos unas veces para practicar.
   Estos comandos son los usados más frecuentemente.

Otros dos comandos de movimiento del cursor importantes son M-< (Meta  
Menor que), el cual se mueve al comienzo del texto entero, y M->( Meta
Mayor que), el cual se mueve al final del texto entero.

En la mayoría de terminales, el "<" está sobre la coma, por lo tanto usted
puede usar la tecla shift para generarlo. En estas terminales usted podrá
usar la tecla shift también al teclear M-<; sin la tecla shift, usted 
estaría escribiendo M-coma.

>> Ahora pruebe M-<, para moverse al comienzo del tutorial.
   A continuación use C-v repetidamente para regresar aquí.

>> Ahora pruebe M->, para moverse el final del tutorial.
   Después use M-v repetidamente para regresar aquí.

También puede mover el cursor con las teclas de la flecha si su
terminal tiene teclas de flecha. Recomendamos aprender C-b, C-f,
C-n y C-p por tres razones. Primero, ellos funcionan en todo tipo de 
terminales. Segundo, una vez usted gane práctica usando Emacs, usted 
encontrará que teclear estos caracteres Control es más rápido que
usar teclas de flecha ( porque no tendrá que retirar sus manos de
la posición para teclear). Tercero, una vez tenga el hábito de usar
estos comandos Control, también puede aprender fácilmente a usar otros
comandos avanzados de movimiento de cursor.

La mayoría de comandos de Emacs aceptan un argumento númerico; para
la mayoría de comandos esto sirve como un factor de repetición. La manera que 
a un comando usted da un factor de repetición es tecleando C-u y luego los 
dígitos antes de introducir los comandos. Si usted tiene una tecla META 
( o EDIT o ALT), hay una manera alternativa para ingresar un argumento 
númerico:  teclear los dígitos mientras presiona la tecla META. 
Recomendamos aprender el método C-u porque este funciona en cualquier terminal.
El argumento  númerico es también llamado un "argumento prefijo", porque usted
teclea el argumento antes del comando al que se aplica.

Por ejemplo, C-u 8 C-f mueve hacia adelante ocho caracteres.
            
>> Pruebe usar C-n o C-p con un argumento númerico, para mover el cursor
   a una línea cerca a esta con solo un comando.

La mayoría de comandos usan argumentos numéricos como un factor de repetición, 
pero algunos comandos lo usan de otra forma. Varios comandos (pero ninguno 
de los que ha aprendido hasta ahora) usan esto como una marca -- la 
presencia de un argumento prefijo, sin tener en cuenta su valor, hace 
que el comando actúe de forma diferente.

C-v y M-v son otro tipo de excepción. Cuando se les da un argumento, 
ellos desplazan la pantalla arriba o abajo esa cantidad de líneas, en vez
de una pantalla completa. Por ejemplo, C-u 8 C-v desplaza la pantalla 8 
líneas.

>> Pruebe tecleando C-u 8 C-v ahora.

Este debió haber desplazado la pantalla hacia arriba 8 líneas. 
Si usted quisiera desplazarla hacia abajo de nuevo, usted puede dar un 
argumento a M-v.

Si usted esta usando X Window, debe haber una área rectangular larga
llamada una barra de desplazamiento al lado izquierdo de la ventana de 
Emacs. Usted puede desplazar el texto al oprimir el boton del mouse en la 
barra de desplazamiento.

>> Pruebe presionando el botón del medio en la parte superior del area 
   resaltada en la barra de desplazamiento. Este debe desplazar el texto a
   una posición determinada por cuán alto o bajo oprima el botón.

>> Intente mover el mouse arriba y abajo, mientras mantiene el botón el 
   medio presionado.  Usted verá que el texto se desplaza arriba y abajo 
   a medida que mueve el mouse.



* CUANDO EMACS ESTÁ BLOQUEADO
-----------------------------

Si Emacs deja de responder a sus comandos, usted puede detenerlo con 
seguridad al teclear C-g. Puede usar C-g para detener un comando 
que está tomando mucho tiempo para ejecutarse.

También puede usar C-g para descartar un argumento númerico o el comienzo
de un comando que usted no quiere finalizar.

>> Escriba C-u 100 para hacer un argumento númerico de 100, entonces
   pruebe C-g. Ahora pruebe C-f. Esto deberá mover sólo un caracter, ya
   que usted canceló el argumento con C-g.

Si usted ha digitado <ESC> por error, usted puede desecharlo con un C-g.


* COMANDOS DESACTIVADOS
-----------------------

Algunos comandos de Emacs son "desactivados" de manera que los usuarios 
principiantes no puedan usarlos por accidente.

Si usted prueba uno de los comandos desactivados, Emacs muestra un mensaje
informando cuál comando era, y preguntándole si usted quiere continuar y 
ejecutar el comando.

Si usted realmente quiere intentar el comando, oprima espacio como repuesta
a la pregunta. Normalmente, si usted no quiere ejecutar el comando 
desactivado, conteste la pregunta con "n".

>> Escriba C-x C-l (que es un comando desactivado), a continuación escriba
   n para responder la pregunta.


* VENTANAS
----------

Emacs puede tener varias ventanas, cada una mostrando su propio texto. 
Explicaremos después como usar múltiples ventanas. Ahora queremos 
explicar cómo deshacerse de ventanas adicionales y volver a la edición 
básica en una ventana. Es sencillo:

	C-x 1 	Una ventana (i.e.,  elimina todas las otras ventanas).

Esto es Control x seguido por el digito 1. C-x 1 expande la ventana que 
contiene el cursor, para ocupar toda la pantalla. Esto borra todas las 
otras ventanas.

>> Mueva el cursor a esta línea y escriba C-u 0 C-l.
>> Escriba Control-h k Control-f.
   Mire como esta ventana se encoge, mientras una nueva aparece y 
   muestra documentacion sobre el comando Control-f.

>> Escriba C-x 1 y vea la ventana de listado de documentación desaparecer.

Este comando es diferente a los otros comandos que usted ha aprendido en 
que este consiste de dos caracteres. Este comienza con el caracter Control-x. 
Hay toda una serie de comandos que comienzan con Control-x; muchos de 
ellos tienen que ver con ventanas, archivos, buffers y cosas 
relacionadas. Estos comandos son de una longitud de dos, tres o cuatro 
caracteres.

* INSERTANDO Y BORRANDO
-----------------------

Si usted quiere insertar un texto sólo escribalo. Los caracters que 
usted puede ver, tales como A, 7, *, etc. Son tomados por Emacs como texto
 e insertados inmediatamente. Oprima <Return> (la tecla Enter) para 
insertar un caracter de nueva línea.

Usted puede borrar el último caracter que escribió oprimiendo <Delete>. 
<Delete> es una tecla, que puede estar marcada como "Del". En algunos
 casos la tecla "Backspace" sirve como <Delete>, ¡pero no siempre!

Generalmente <Delete> borra el caracter inmediatamente anterior a la
posición actual del cursor.

>> Haga esto ahora -- Teclee unos pocos caracteres, y bórrelos con
   la tecla <Delete>. No se preocupe si este archivo cambia, no alterará 
   el tutorial principal. Esta es su copia personal.

Cuando una línea de texto se hace muy grande para una sola línea en la
pantalla, la línea de texto es "continuada" en una segunda línea. Un
 backslash ("\") en el margen derecho indica que la línea ha sido 
continuada.

>> Inserte texto hasta que llegue al margen derecho, y siga insertando.
   Verá a continuación aparecer una línea.

>> Use <Delete> para borrar el texto hasta que la línea de nuevo quepa
   en la pantalla. La línea  de continuación se pierde.

Puede borrar un caracter de Nueva línea justo como cualquier otro 
caracter. Al borrar un caracter de Nueva línea entre dos líneas 
las combina en una sola. Si el resultado de la combinación de líneas es
demasiado largo para caber en el ancho de la pantalla, será mostrada 
una línea de continuación.

>> Mueva el cursor al comienzo de una línea y oprima <Delete>. Esto 
   unirá esta línea con la anterior.

>> Oprima <Return> para reinsertar la nueva línea que borró.

Recuerde que a la mayoría de los comandos de Emacs se les puede dar un 
factor de repetición, esto incluye caracteres de texto. Repetir un 
caracter de texto lo inserta varias veces.

>> Ahora trate esto -- teclee C-u 8 * para insertar ********.

Usted ha aprendido la manera más básica de teclear algo en Emacs
y corregir errores. Puede borrar por palabras o por líneas. Este es un
resumen de las operaciones de borrado:

	<Delete> 	borra el caracter justo antes que el cursor
	C-d		borra el siguiente caracter después del cursor 

	M-<Delete> 	Elimina la palabra inmediatamente antes del cursor
	M-d        	Elimina la siguiente palabra antes del cursor

	C-k  		Elimina desde el cursor hasta el fin de la línea  
	M-k  		Elimina hasta el final de la oración actual.

Note que <Delete> y C-d contra M-<Delete> y M-d extienden el paralelo
iniciado por C-f y M-f (bien, <Delete> no es realmente una tecla de 
control, pero no nos preocuparemos ahora por eso). C-k y M-k son como
C-e y M-e, en ciertas forma, en que las líneas son oraciones opuestas.

También puede eliminar cualquier parte del buffer con un método uniforme.
Muevase a un extremo de esa parte, y digite C-@ o C-SPC (cualquiera de 
los dos). Muévase al extremo de esa parte y teclee
C-w. Eso elimina todo el texto entre las dos posiciones. 

>> Mueva el cursor a la letra T al inicio del párrafo anterior.
>> Teclee C-SPC. Emacs debe mostrar el mensaje "Mark set" en la parte de 
   abajo de la pantalla.
>> Mueva el cursor a la x en "extremo", en la segunda línea del parrafo.
>> Teclee C-w. Esto eliminará el texto que comienza desde la T, y 
   termina justo antes de la x.

La diferencia entre "eliminar" y "borrar" es que el texto "eliminado"
puede ser reinsertado, mientras que las cosas "borradas" no pueden
ser reinsertados.

La reinserción de texto eliminado es llamada "yanking".  Generalmente,
los comandos que pueden remover mucho texto lo eliminan (configurados de
esta forma para poder recuperarlo), mientras que los comandos que 
remueven un solo caracter, o solamente líneas en blanco o espacios, 
hacen un borrado (sin que usted pueda recuperar dicho texto).

>> Mueva el cursor al comienzo de una línea  que no esté desocupada. Luego
   digite C-k para eliminar el texto de esa línea.
>> Oprima C-k por segunda vez. Usted verá que elimina la nueva
   línea que sigue a esa.

Note que al oprimir C-k una sola vez elimina el contenido de la línea , y
un segundo C-k elimina la línea  misma, y hace que todas las otras líneas 
se muevan hacia arriba. C-k trata un argumento numérico especialmente: 
Elimina ese número de líneas y sus contenidos. Esto no es una simple 
repetición. C-u 2 C-k elimina dos líneas y sus nuevas líneas, tecleando 
C-k dos veces no hace esto.

Recuperar texto eliminado es llamado "yanking". (Piense en esto como si 
estuviera recuperando, reinsertando, algún texto que fue eliminado). 
Puede recuperar el texto eliminado o bien en el mismo sitio de dónde fue 
eliminado o en otro lugar en el buffer, o inclus en un archivo diferente.
Puede reinsertar el texto varias veces, lo que hace múltiples copias del
mismo.

El comando para recuperar es C-y. Este reinserta el  último texto 
eliminado, en la posición actual del cursor. 

>>Inténtelo; oprima C-y para recuperar el texto.

Si usted tecleea muchos C-k s seguidos, todo el texto eliminado es salvado
junto, de forma tal que un C-y lo recuperará todo de una sola vez.

>>Haga esto ahora, oprima C-k varias veces.

Ahora para recuperar ese texto eliminado:

>> Teclee C-y. Entonces baje el cursor unas pocas líneas y oprima C-y de 
nuevo. Ahora sabe como copiar un texto.

Qué hacer si usted tiene algún texto que quiere recuperar, pero elimina
otro antes de recuperarlo? C-y recuperaría la eliminación más reciente,
pero el texto previo no está perdido. Puede regresar a éste usando el 
comando M-y. Después de haber tecleado C-y para conseguir la eliminación
más reciente, escribiendo M-y reemplaza el texto recuperado con la 
eliminación previa. Tecleando M-y una y otra vez traerá las 
eliminaciones anteriores. Cuando usted ha encontrado el texto que 
buscaba, usted no tiene que hacer nada para conservarlo.Sólo siga con 
su edición, dejando el texto recuperado donde está.

Si usted digita M-y suficientes veces, usted regresa al punto inicial
(la eliminación más reciente)

>> Elimine una línea muévase, elimine otra línea.
   Luego oprima C-y para recuperar la segunda línea eliminada.
   Entonces oprima M-y, y será reemplazada por la primera línea eliminada.
   Oprima más veces M-y y vea lo que sucede. Siga haciéndolo hasta que la
   segunda línea eliminada regrese, y entonces unas pocas más. Si usted 
   quiere, puede tratar de darle a M-y argumentos positivos y negativos.


* DESHACER 
----------

Si hace un cambio al texto, y luego decide que fue un error,
puede deshacer el cambio con el comando deshacer C-x u.

Normalmente, C-x u deshace los cambios hechos por un comando; si repite
varias veces seguidas C-x u, cada repetición deshará un comando 
adicional.
 
Pero hay dos excepciones: comandos que no cambian el texto no cuentan 
(esto incluye los comandos de movimiento del cursor y el comando de 
desplazamiento), y los caracteres de autoinserción son usualmente  manejados
en grupos de hasta 20 (estos es para reducir el numero de C-x u que tiene que 
realizar para deshacer una inserción en el texto).

>> Elimine esta línea  con C-k, después C-x u debería reaparecerla.

C-_ es un comando alterno para deshacer, este trabaja igual que C-x u, 
pero es más facil de usar varias veces seguidas. La desventaja 
de C-_ es que en algunos teclados no es obvio como se genera. Por esto
proveemos C-x u también. En algunas terminales, usted puede oprimir C-_ 
al digitar / mientras oprime CONTROL.

Un argumento numérico para C-_ o C-x u actúa como un factor de repetición.

Usted puede deshacer el borrado de texto de la misma forma que si lo 
hubiera eliminado. La diferencia entre eliminar algo y borrarlo se hace
visible cuando lo recupera con C-y; no hay diferencia para deshacer.


* ARCHIVOS
----------

Para hacer permanente el texto que edite, lo debe colocar en un 
archivo. De otra manera, este se se perderá cuando cierre Emacs.
Para poder poner su texto en un archivo, usted debe "encontrar" el 
archivo antes de ingresar el texto (esto también es llamado "visitar" 
el archivo.) 

Encontrar un archivo significa que usted puede observar su contenido 
dentro de Emacs. Esto de cierta forma, es como si usted estuviera editando
el archivo. Aunque los cambios que usted haga usando Emacs no serán
permanentes hasta que "salve" el archivo. Esto es para no dejar un archivo
 a medio cambiar en el sistema cuando usted no quiere. Incluso cuando 
usted salve, Emacs dejará el archivo original bajo un nombre cambiado en 
caso de que luego usted decida que sus cambios fueron un error. 

Si mira cerca del final de la pantalla podrá ver una línea  que 
comienza y termina con guiones, y comienza con "--:-- TUTORIAL" o algo 
así. Esta parte de la pantalla normalmente muestra el nombre del archivo 
que está visitando. En este momento usted está visitando un archivo 
llamado "TUTORIAL" que es su borrador personal del tutorial de Emacs. Cuando
encuentre un archivo con Emacs, el nombre del archivo aparecerá en ese 
mismo sitio.

Una cosa especial acerca del comando para encontrar un archivo, es que 
usted tendrá que decir cual archivo desea. Decimos que el comando
"lee un argumento desde la terminal" (en este caso, el argumento es 
el nombre del archivo). Despues de generar el comando:

	C-x C-f   Encontrar un archivo

Emacs le pide que digite el nombre del archivo. El nombre de archivo que 
digite aparecerá en la línea final de la pantalla. A la línea final de la 
pantalla se le denomina minibuffer cuando es usada para este tipo de entradas.
Puede usar comandos de edición ordinarios de Emacs para editar el nombre 
del archivo.

Cuando está ingresando el nombre del archivo (o cualquier otra
información al minibuffer) usted puede cancelar el comando con C-g.

>> Oprima C-x C-f, luego oprima C-g. Esto cancela el minibuffer y 
   también cancela el comando C-x C-f que estaba usando el 
   minibuffer. Así que no encontrará archivo alguno.

Cuando usted haya finalizado de ingresar el nombre del archivo, oprima 
<Return> para terminarlo. Entonces el comando C-x C-f trabaja y encuentra
el archivo que usted escogió. El minibuffer desaparece cuando el comando 
C-x C-f termina. 

Después aparece el contenido del archivo en la pantalla y usted puede editarlo.
Cuando quiera hacer cambios permanentes, teclee el comando

	C-x C-s   Salvar el archivo

Este copia el texto de Emacs al archivo. La primera vez que usted haga 
esto, Emacs renombrará el archivo original con un nuevo nombre para que 
este no se pierda. El nuevo nombre se obtendrá adicionándo "~"
al final del nombre del archivo original.

Cuando la operación de salvar haya terminado, Emacs mostrará el 
nombre del archivo escrito. Usted debería salvar frecuentemente, para que
no pierda mucho trabajo si el sistema deja de funcionar.

>> Presione C-x C-s, salvando la copia del tutorial.  Esto debería 
   imprimir "Wrote...TUTORIAL" al final de la pantalla.

NOTA: En algunos sistemas, oprimir C-x C-s dejará inmóvil la pantalla y 
usted no podrá ver más respuesta de Emacs. Esto indica que una 
"característica" del sistema operativo llamada ''control de flujo'' 
está interceptando el comando C-s y no permitiéndole llegar a Emacs. 
Para descongelar la pantalla, presione C-q. Entonces consulte la sección 
"entrada espontánea para búsqueda incremental'' en el manual de 
Emacs para encontrar información de cómo tratar con esta "característica".

Usted puede encontrar un archivo existente para verlo o editarlo, 
también puede hacerlo con un archivo que no existe. Esta es la forma 
de crear un archivo en Emacs: encuentre el archivo, comenzará vacio, luego 
comience a introducir el texto al archivo. Cuando invoque "salvar" el archivo, 
Emacs creará realmente el archivo con el texto que introdujo. Desde ahí, 
usted puede considerarse estar editando un archivo existente.


* BUFFERS
---------

Si visita un segundo archivo con C-x C-f, el primer archivo permanece en 
Emacs. Usted puede volver al primer encontrándolo de nuevo con C-x C-f. De 
esta forma usted puede mantener un gran número de archivos en Emacs.

>> Cree un archivo llamado  "foo" presionando C-x C-f foo <Return>
   luego inserte algún texto, edítelo, y salve "foo" presionando C-x 
   C-s. Finalmente presione C-x C-f TUTORIAL <Return> para regresar al 
   tutorial.

Emacs almacena cada archivo de texto dentro de un objeto llamado 
"buffer". Al encontrar un archivo se crea un nuevo buffer dentro de 
Emacs. Para mirar la lista de los buffers que existen actualmente en 
su sesión de Emacs, presione:

	C-x C-b   Lista de Buffers

>> Intente ahora C-x C-b.

Mire como cada buffer tiene un nombre, y ademas puede tener un nombre de 
archivo para el archivo que contiene. Algunos Buffers no corresponden a 
archivos; por ejemplo, el buffer llamado "*Buffer List*" no tiene ningún 
archivo. Este es el buffer que tiene la lista de buffers que fue creado
por C-x C-b. CUALQUIER archivo de texto que vea en una ventana de Emacs, 
es siempre parte de algún Buffer.

>> Presione C-x 1 para deshacerse de la lista de buffers.

Si usted hace cambios al texto de un archivo, y luego encuentra otro 
archivo, esto no salva el primer archivo. Estos cambios permanecerán 
dentro de Emacs en el buffer del archivo. La creación o edición del 
segundo buffer de archivo no afecta al primero. Esto es muy útil pero 
significa que necesita una forma conveniente para salvar el archivo del 
primer buffer. Sería una molestia  tener que volver a este con  C-x 
C-f para salvarlo con C-x C-s así que tenemos
 
	C-x s 	Guardar algunos buffers
 
C-x s le pregunta sobre cada  buffer que contenga cambios que usted no
ha salvado.  Le pregunta por cada buffer si salvarlo o no.
 
>> Inserte una línea  de texto, y teclee C-x s.
   Esto debería preguntarle si  desea salvar el buffer llamado TUTORIAL.
   Conteste si a la pregunta presionando "y".
 
* EXTENDIENDO EL CONJUNTO DE COMANDOS
-------------------------------------
 
Hay muchísimos más comandos de Emacs que podrían ser posiblemente
puestos en todos los caracteres control y meta. Emacs puede lograrlo
usando el comando X (eXtendido).  Este viene de dos formas:

	C-x 	Caracter eXtendido seguido por un caracter.
        M-x 	Comando eXtendido por nombre. Seguido por un nombre largo.
 
Estos comandos son  generalmente útiles pero usados menos que los
comandos que usted hasta ahora ha aprendido. Hasta ahora ha visto dos
de estos: los comandos de archivo C-x C-f para encontrar y C-x C-s
para salvar. Otro ejemplo es el comando para terminar la sesión de
Emacs -- este es el comando C-x C-c ( no se preocupe  por perder los 
cambios que usted haya hecho; C-x C-c ofrece salvar cada archivo alterado
antes de que este elimine a Emacs.)
 
C-z es el comando para salir de Emacs *temporalmente*-- para que usted
pueda regresar a la misma sesión de Emacs después.
 	
En sistemas que lo permiten C-z "suspende" Emacs; esto es, se retorna
al shell pero no se destruye Emacs.  En los shells más comunes, usted
puede reanudar Emacs con el comando `fg' o con `%emacs'.
 		
En sistemas que no se implemente el suspendido, C-z crea un subshell
que corre bajo Emacs para darle la opción de correr otros programas y
regresar a Emacs después; esto en realidad no "sale" de Emacs. En este
caso, el comando shell `exit' es la vía usual para regresar a Emacs
desde el subshell.
 
El momento para usar C-x C-c es cuando usted está listo para salir. 
Es además el paso correcto para salir de Emacs llamado bajo programas de
manejo de correo y otra variedad de utilidades, puesto que ellos no
saben cómo lidiar con la suspensión de Emacs. En circunstancias normales,
si usted no va a  salir, es mejor suspender Emacs con C-z en lugar de 
salir de este.
 
 Existen varios comandos C-x. Aqui  hay una lista de algunos que usted
ha aprendido:

	C-x C-f 	Encontrar archivo.
        C-x C-s 	Salvar archivo.
        C-x C-b 	Lista de buffers.
        C-x C-c 	Salir de Emacs.
        C-x 1 		Borrar todo menos una ventana.
        C-x u 		Deshacer.
 
Los comandos llamados eXtendidos son comandos usados con menos frecuencia
o comandos que  son usados únicamente en ciertos modos. Un ejemplo es
el comando replace-string, el cual globalmente reemplaza una cadena
con otra. Cuando  usted teclea M-x, Emacs le pregunta al final de la
pantalla con M-x y usted debe escribir el nombre del comando; en este
caso "replace-string". Solo escriba "repl s<TAB>" y Emacs completará
el nombre. Finalice el nombre del comando con <Return>.
 
 El comando replace-string requiere dos argumentos -- la cadena a
reemplazar, y la cadena para reemplazarla.   Usted debe terminar cada 
argumento con <Return>.
 
>> Mueva el cursor hacia la línea en blanco dos líneas debajo de esta. A
   continuación escriba 
   M-x repl s<Return>cambiado<Return>alterado<Return>.
 
Note cómo esta línea ha cambiado: usted reemplaza la palabra
c-a-m-b-i-a-d-o  con "alterado" en cada ocurrencia, después de la
posición inicial del cursor.
 
* AUTO SALVADO
--------------
 
Cuando usted ha hecho cambios en un archivo, pero usted no ha salvado
estos todavía, estos  podrían perderse si su sistema deja de funcionar. Para
protegerlo de  esto, Emacs periódicamente escribe un archivo "auto salvado"
para cada  archivo que usted está editando. El nombre del
archivo auto salvado tiene un # al principio y al final; por ejemplo, si su 
archivo se llama "hola.c", su archivo auto salvado es "#hola.c#".  Cuando 
usted salva por la vía normal, Emacs borra su archivo de auto salvado.
 
Si el sistema deja de funcionar usted puede recuperar su edición de 
auto salvado encontrando el archivo normal (el  archivo que  estuvo editando, 
no el archivo  de auto salvar) y entonces presionando M-x recover file<Return>.
Cuando le pregunte por la confirmación, teclee yes<Return> para ir y 
recuperar la información del auto-salvado.
 
* AREA ECO
----------
 
Si Emacs ve que usted está presionando comandos de multicaracteres
lentamente, este le muestra estos al final de la pantalla en un área
llamada "área de eco". El área de eco contiene la línea final de la pantalla.
 
* LÍNEA DE MODO
---------------
 
 La línea inmediatamente encima del área de eco es llamada el "línea de modo"
dice algo así:

--:** TUTORIAL           (Fundamental)--l730--58%---------
 
Esta línea  da información útil acerca de el estado de Emacs y del
texto que usted está editando.
 
Usted ya sabe qué significa el nombre del archivo -- este es el
archivo que usted ha encontrado.  --NN%-- indica posición actual en el
texto; esto  significa que NN porciento del  texto está encima  de la
parte superior de la pantalla. Si el principio del archivo está en la
pantalla, este dirá --Top-- en vez de --00%--. Si el final del texto está
en la pantalla, este dirá --Bot--.   Si usted está mirando un texto tan
pequeño que cabe en la pantalla, el modo de línea dirá --All--.
 
La L y los dígitos indican la posición de otra forma: ellos dan el
número de línea actual del punto.
 
Las estrellas cerca del frente significan que usted ha hecho cambios
al texto. Exactamente despues de que usted visite o salve un archivo,
esa parte del modo de línea no muestra estrellas, solo los guiones.
 
La parte del modo de línea  dentro de los paréntesis es para indicarle
en qué modo de edición está usted. El modo por defecto es Fundamental,
el cual es el que usted está usando ahora. Este es un ejemplo de un
"modo mayor".
 
Emacs tiene diferentes modos mayores. Algunos de estos están
hechos para editar diferentes lenguajes y/o clases de texto, tales como
modo Lisp, modo de Texto, etc.   En cualquier  momento solamente un modo
mayor está activo, y su nombre se encuentra siempre en el modo
de línea donde "Fundamental" está ahora.
 
Cada modo mayor hace que algunos comandos actúen diferente
por ejemplo, hay comandos para crear comentarios en un programa, y como
cada lenguaje de programación tiene una idea diferente de cómo debe verse
un comentario, cada modo mayor tiene que insertar comentarios de
forma distinta. Cada modo mayor es el nombre de un comando 
extendido, que es como usted puede cambiar a ese modo. Por ejemplo, 
M-x fundamental-mode es un comando para cambiar al modo fundamental.

Si usted va a editar un texto en español, tal como este archivo, 
probablemente tendrá que usar el modo de texto.

>> Teclee M-x text-mode<Return>.  

No se preocupe, ninguno de los comandos de Emacs que ha aprendido cambia
de manera significativa.  Pero usted puede ver que M-f y M-b tratan a los
apóstrofes como parte de las palabras.  Previamente, en modo Fundamental,
M-f y M-b trataban los apóstrofes como separadores de palabras.

Los modos mayores usualmente hacen cambios sutiles como el 
anterior: La mayoría de comandos hacen "el mismo trabajo" en cada modo 
mayor, pero funcionan un poco diferente.

Para ver documentación en su modo mayor actual, teclee C-h m.

>> Use C-u C-v una vez o mas para llevar esta línea cerca de la 
   parte superior de la pantalla.

>> Teclee C-h m, para ver cómo el modo de texto difiere del modo 
   Fundamental.

>> Teclee C-x 1 para eliminar la documentación de la pantalla.

Los modos mayores son llamados así porque también hay modos menores.
Los modos menores no son alternativas para los modos mayores, apenas
modificaciones menores de estas.  Cada modo menor puede ser activado o
desactivado por si mismo, independiente de todos los otros modos 
menores, e independiente de su modo mayor.  Entonces usted puede no usar
modos menores, o un modo menor o alguna combinación de varios modos 
menores.

Un modo menor que es muy útil, especialmente para editar textos en
inglés, es el modo Auto Fill.  Cuando este modo es encendido, Emacs
rompe la línea entre palabras automáticamente cuando quiera que inserte
texto y haga una línea que es bastante ancha.

Usted puede activar el modo Auto Fill al hacer M-x auto fill mode<Return>.
Cuando el modo este activado, usted puede desactivarlo nuevamente usando
M-x autofill mode<Return>.
Si el modo está desactivado, este comando lo activa, y si el modo está
activado, este comando lo desactiva. Decimos que el comando "cambia el
modo".

>> Teclee M-x auto fill mode<Return> ahora.  Luego inserte una línea
   de "asdf " repetidas veces hasta que la vea dividida en dos líneas. 
   Usted debe intercalar espacios porque Auto Fill sólo rompe 
   líneas en espacios.

El margen es usualmente puesto en 70 caracteres, pero usted puede
cambiarlo con el comando C-x f. Usted debe dar el margen deseado
como un argumento numérico.

>> Teclee C-x f con un argumento de 20.  (C-u 2 0 C-x f).
   Entonces teclee algún texto y vea como Emacs llena con este líneas de
   20 caracteres.  Entonces ponga de nuevo el margen a 70 usando otra 
   vez C-x f.

Si usted hace cambios en el medio de un párrafo, el modo Auto Fill no
lo rellenará por usted.
Para rellenar el párrafo, teclee M-q (Meta-q) con el cursor dentro de
ese párrafo.

>>Mueva el cursor al párrafo anterior y teclee M-q.

* BUSCANDO
----------

Emacs puede hacer búsquedas de cadenas (estas son grupos de caracteres
contiguos o palabras) hacia adelante a través del texto o para atrás
en el mismo.  La busqueda de una cadena es un comando de movimiento de 
cursor; esto mueve el cursor al próximo lugar donde esa cadena aparece.

El comando de búsqueda de Emacs es diferente a los comandos de búsqueda
de los demás editores, en que este es "incremental".  Esto significa que
la búsqueda ocurre mientras usted teclea la cadena para buscarla.

El comando para iniciar una búsqueda es C-s para búsqueda hacia adelante,
y C-r para la búsqueda hacia atrás.  PERO ESPERE! no intente esto ahora.

Cuando teclee C-s usted verá que la cadena "I-search" aparece como un 
llamado en el área de eco.  Esto le dice que Emacs está en lo que se
conoce como búsqueda incremental, esperando que usted teclee la cosa que
quiere buscar.  <Return> termina una busqueda.

>> Ahora teclee C-s para comenzar la búsqueda.  LENTAMENTE, una letra a
   la vez, teclee la palabra 'cursor', haciendo pausa después de cada
   caracter para notar lo que pasa con el cursor.
   Ahora ha buscado "cursor", una vez.
>> Ahora teclee C-s de nuevo, para buscar la próxima aparición de
   "cursor".
>> Ahora presione <Delete> cuatro veces y mire como se mueve el cursor.
>> Teclee <Return> para terminar la búsqueda.

Vió lo que ocurrió? Emacs, en una búsqueda incremental, trata de ir a la 
ocurrencia de la cadena que usted ha tecleado hasta el momento.  Para ir
a la próxima ocurrencia de "cursor" solo teclee C-s de nuevo.  Si tal  
ocurrencia no existe, Emacs pita y le dice que la búsqueda actual está
fallando (failing).  C-g también termina la búsqueda.

NOTA: En algunos sistemas, oprimir C-x C-s dejará inmovil la pantalla y 
usted no podra ver más respuesta de Emacs. Esto indica que una 
"característica" del sistema operativo llamada ''control de flujo'' 
está interceptando el comando C-s y no permitiéndole llegar a Emacs. 
Para descongelar la pantalla, presione C-q. Entonces consulte la sección 
"entrada espontánea para búsqueda incremental'' en el manual de 
Emacs para encontrar información de cómo tratar con esta "característica".

Si usted está en medio de una búsqueda incremental y teclea <Delete>,
usted notara que el último caracter de la cadena buscada es borrado y
la búsqueda vuelve al último sitio de la búsqueda.  Por ejemplo, suponga 
que usted ha tecleado "c", para buscar la primera ocurrencia de "c".  
Ahora, si teclea "u", el cursor se moverá a la primera ocurrencia de 
"cu".  Ahora teclee <Delete>.  Esto borra la "u" de la cadena buscada,
y el cursor vuelve a la primera ocurrencia de "c".

Si usted está en medio de una búsqueda y teclea un caracter 
control o meta (con algunas pocas excepciones-- los caracteres que son 
especiales en una búsqueda, tales como C-s y C-r), la búsqueda es 
terminada.

El C-s inicia una exploración que busca alguna ocurrencia de la cadena 
buscada DESPUES de la posición actual del cursor.  Si usted quiere buscar
algo anterior en el texto, teclee en cambio C-r.  Todo lo que hemos 
dicho sobre C-s también se aplica a C-r, excepto que la dirección de la
búsqueda es invertida.

* MúLTIPLES VENTANAS 
--------------------

Una de las agradables características de Emacs es que usted puede mostrar más
de una ventana en la pantalla al mismo tiempo.

>> Mueva el cursor a esta línea y teclee C-u 0 C-l.

>> Ahora teclee C-x 2 que divide la pantalla en dos ventanas. Ambas 
   ventanas muestran este tutorial.  El cursor permanece en la ventana 
   superior.

>> Teclee C-M-v para desplazar la ventana inferior.  (si usted no 
   tiene una tecla Meta real, teclee ESC C-v) 

>> Teclee C-x o ("o" para "otro") para mover el cursor a la ventana 
   inferior.
>> Use C-v y M-v en la ventana inferior para desplazarla.  Siga 
   leyendo estas direcciones en la ventana superior.

>> Teclee C-x o de nuevo para mover el cursor de vuelta a la ventana 
   superior.  El cursor en la ventana superior está justo donde estaba
   antes.

Usted puede continuar usando C-x o Para cambiar entre las ventanas. Cada
 ventana tiene su propia posición del cursor, pero únicamente una 
ventana actual muestra el cursor. Todos los comandos de edición comunes
se aplican a la ventana en que está el cursor. Nosotros la llamamos 
"ventana seleccionada".

El comando C-M-v es muy útil cuando usted está editando un texto en una
 ventana y usando la otra ventana como referencia. Usted puede mantener
 el cursor siempre en la ventana donde está editando, y avanzar a la 
otra ventana secuencialmente con C-M-v.

C-M-v es un ejemplo de un carácter CONTROL-META. Si usted tiene una 
tecla Meta real, usted puede oprimir C-M-v Sosteniendo a la vez CONTROL
y META mientras oprime v. No importa cual tecla oprima primero CONTROL o META.
Porque las dos teclas actúan modificando los caracteres que usted digita.
Si usted no tiene una tecla META real, y en vez de esta usa ESC, el 
orden importa: usted debe digitar ESC seguido de Control-v, porque 
Control-ESC v no trabajará. Esto es porque ESC es un carácter que tiene
valor en sí mismo, no es una tecla modificadora.

>> digite C-x 1 (en la parte de arriba de la ventana) para deshacerse 
   de la ventana del final.

(Si usted hubiera digitado C-X 1 en la ventana inferior, esto eliminaría la 
superior. Piense en este comando como "mantener sólo una ventana -- la 
ventana en la cual estoy".)

Usted no tiene que mostrar el mismo buffer en ambas ventanas. Si usted 
usa C-x C-f para encontrar un archivo en una ventana, la otra ventana 
no cambia. Usted puede encontrar un archivo en cada ventana 
independientemente.

Aquí hay otra forma para usar dos ventanas para mostrar dos cosas 
diferentes:

>> Digite C-x 4 C-f seguido del nombre de uno de sus archivos. 
   Finalice con <Return>. Vea que el archivo especificado aparece en la 
   ventana inferior. El cursor vá allá también.

>> Digite C-x o para regresar a la ventana superior, y C-x 1 para borrar
   la ventana inferior.

* NIVELES RECURSIVOS DE EDICIÓN
--------------------------------

Algunas veces usted entrará a lo que es llamado un "nivel recursivo 
de edición". Esto está indicado por paréntesis cuadrados en la línea de modo
, rodeando los paréntesis del nombre del modo mayor. Por ejemplo, 
usted probablemente vea [(Fundamental)] en vez de  (Fundamental)

Para salir de los niveles recursivos de edición, presione ESC ESC ESC. 
Este es un comando de "salida" para todo propósito. Usted también lo 
puede usar para eliminar ventanas extras, y salir del minibuffer.

>> Digite M-x para entrar a un minibuffer; Entonces digite ESC ESC ESC 
   para salir.

Usted no puede usar C-g para salir de los "niveles recursivos de 
edición". Esto es porque C-g es usado para cancelar comandos y 
argumentos DENTRO del nivel recursivo de edición.

* CONSIGUIENDO MAS AYUDA
------------------------

En este tutorial hemos tratado de ofrecer suficiente información para 
que usted empiece a usar Emacs. Hay tanto disponible en Emacs que 
sería imposible explicar todo aquí, sin embargo, usted puede querer 
aprender más sobre Emacs, ya que este tiene muchas otras características 
útiles. Emacs provee comandos para leer documentación acerca de los 
comandos de Emacs. Estos comandos de "ayuda" todos comienza con el 
caracter Control-h, que es llamado el caracter de ayuda (help).

Para usar las características de ayuda, digite el caracter C-h, y 
entonces un carácter diciciendo qué tipo de ayuda quiere. Si usted está
REALMENTE perdido digite c-h ? y Emacs le dirá qué tipo de ayuda puede 
ofrecerle. Si usted ha digitado  C-h y decide que no quiere ninguna 
ayuda, digite C-g para cancelarlo.

(Algunos sitios cambian el significado del carácter C-h. Ellos realmente
no deberían hacer esto como una política para todos los 
usuarios, así que usted tiene argumentos para quejarse al administrador 
del sistema. Mientras tanto, sí C-h no muestra un mensaje de ayuda en
el final de la pantalla, intente digitar la tecla F1 o en su lugar M-x 
help <Return>).

La característica más básica en la AYUDA es C-h c. Digite C-h, el 
caracter c y un caracter de comando o una secuencia de comando; entonces 
Emacs muestra una muy breve descripción del comando.

>> Digite C-h c C-p.
   El mensaje debe ser algo como
	 C-p runs the command previous-line

Esto le dice el "nombre de la función". Los nombres de función son 
usados principalmente  para adecuar y extender Emacs. Pero ya que los 
nombres de las funciones son seleccionados para indicar lo que el comando
hace, ellos tambien pueden servir como una breve documentación --
suficiente para recordarle los comandos que ha aprendido.

Los comandos de múltiples caracteres tales como C-x C-s y (sí usted no 
tiene las teclas META o EDIT o ALT) <Esc>v, están permitidos también 
después de C-h c.

Para conseguir más información sobre un comando use C-h k en vez de 
C-h c.

>> Digite C-h k C-p.

Esto muestra la documentación de la función, al igual que el nombre, en
una ventana de Emacs. Cuando usted haya terminado de leer el resultado, 
digite C-x 1 para deshacerse del texto de ayuda. No tiene que hacer esto 
ahora. Usted puede  hacer algunas ediciones mientras se refiere al texto 
de ayuda, y entonces digitar C-x 1.

Aquí hay algunas otras opciones útiles de C-h:

   C-h f	Describe una función. Usted digita el nombre de la 
		función.

>> Intente digitar C-h f previous-line<Return>.
   Esto imprime toda la información que Emacs tiene sobre la función que 
   implementa el comando C-p

De forma similar el comando C-h v imprime la documentación de variables
cuyos valores pueden ser especificados para configurar el comportamiento
de Emacs. Necesita teclear el nombre de la variable cuando Emacs lo
pregunte.

   C-h a 	Comando Apropos. Digite una palabra y Emacs hará una 
		lista de todos los comandos que contengan la palabra 
		digitada. Todos Estos comandos pueden ser invocados con 
		Meta-x.  Para algunos comandos, el comando Apropos 
		también listará una secuencia de uno a dos caracteres 
		la cual correrá el mismo comando.

>> Digite C-h a file<Return>.

Esto muestra en otra ventana una lista de todos los comandos M-x con
la palabra "file" en sus nombres. Usted verá comandos de caracteres como
C-x C-f listados además de los nombres de los comandos correspondientes 
tales como find-file.

>> Digite C-M-v para desplazar la ventana de ayuda. Haga esto unas pocas 
   veces.

>> Digite C-x 1 para borrar la ventana de ayuda.

   C-h i        Leer manuales en línea (a.k.a. Info). Este comando lo
		lleva a un buffer especial llamado '*info*' en el cual
		puede leer manuales en línea de los paquetes instalados 
		en su sistema.  Teclee m emacs <Return> para leer el 
		manual de Emacs.  Si usted nunca ha usado Info antes, 
		teclee ? y Emacs lo llevara a un tour guiado de las 
		facilidades del modo Info.  Una vez que haya finalizado
		este tutorial, debería consultar el manual Info de Emacs
		como su primera fuente de documentación.


* CONCLUSIÓN
------------

Recuerde, Para salir permanentemente de Emacs use C-x C-c. Para salir a un 
Shell temporalmente, de forma que usted puede volver a Emacs después, use C-z.

Este tutorial intenta ser entendible para todos los usuarios 
nuevos, así que si encuentra algo confuso, no se siente y se culpe a sí
mismo - quéjese!

* COPIADO
---------

Este tutorial desciende de una larga línea de tutoriales de Emacs
comenzando con el escrito por Stuart Cracraft para el Emacs original.
La versión en español fue traducida por estudiantes del Gimnasio Fidel Cano 
(un colegio en Santafé de Bogotá, Colombia):

	Carlos Alberto López Troncoso
	Andrés Felipe Mancipe Galvis
	Lina Fernanda Pinto García
	Liliana Carolina Quitián Cedeño
	Leonardo Ramírez Vargas <leonardoramirez@latinmail.com>
	Juan David Vargas Botero <cyberbob1164@hotmail.com>
	Juan Pablo Yela Gallón
	Jorge Enrique Cárdenas Carrillo <platypus_life@hotmail.com>

además la versión en español ha sido revisada y corregida por:
	Pablo Reyes <reyes_pablo@hotmail.com>
	Igor Támara <ikks@bigfoot.com>
	Melissa Giraldo de Támara <melagira@yahoo.com>
	Vladimir Támara <vtamara@gnu.org>

Por favor, en caso de duda, sólo es válido el original en inglés de la
siguiente nota de derechos de  reproducción (que puede encontrar en el
archivo TUTORIAL).

Copyright (c) 1985, 1996 Free Software Foundation

   Se permite a cualquiera hacer o distribuir copias literales de este
   documento como se recibe, en cualquier medio, siempre que la nota de
   derechos de reproducción y la nota de permiso se preserven, y que el 
   distribuidor permita que el que la recibe hacer distribución posterior 
   como lo permite esta nota.

   Se permite distribuir versiones modificadas de este documento, o
   porciones de este, bajo las condiciones anteriores, siempre que
   ellas tengan nota visible especificando quién fue el último en
   alterarlas.

Las condiciones para copiar Emacs mismo son más complejas, pero con el 
mismo espíritu.  Por favor lea el archivo COPYING y entonces distribuya copias 
de GNU Emacs a sus amigos.  Ayude a erradicar el obstruccionismo del 
software ("propietariedad") usando, escribiendo, y compartiendo software
libre!


