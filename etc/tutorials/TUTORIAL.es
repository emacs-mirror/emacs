Tutorial de Emacs. Vea al final las condiciones de copiado.

Generalmente los comandos de Emacs involucran la tecla CONTROL
(algunas veces llamada CTRL O CTL) o la tecla meta (algunas veces
llamada EDIT o ALT).  En lugar de escribir completamente esto en cada
ocasión, usaremos las siguientes abreviaturas.

 C-<car> significa mantener presionada la tecla CONTROL mientras
         teclea el carácter <car>.  Por lo tanto C-f será: Mantenga
         presionada la tecla CONTROL y teclee f.
 M-<car> significa mantener presionada la tecla META o EDIT o ALT
 	 mientras teclea <car>.  Si no hay teclas META, EDIT o ALT, en
 	 su lugar presione y libere la tecla ESC y luego teclee
 	 <car>.  Escribimos <ESC> para referirnos a la tecla ESC.

Nota importante: para terminar la sesión de Emacs teclee C-x C-c (dos
caracteres).  Los caracteres ">>" en el margen izquierdo indican
instrucciones para que usted trate de usar un comando.  Por ejemplo:
<<Blank lines inserted around following line by help-with-tutorial>>
[Mitad de página en blanco para propósitos didácticos. El texto continúa abajo]
>> Ahora teclee C-v (ver la próxima pantalla) para desplazarse a la
	siguiente pantalla (hágalo manteniendo la tecla control
	oprimida mientras teclea v).  Desde ahora debería hacer esto
	cada vez que termine de leer la pantalla.

Note que se superponen dos líneas cuando se mueve de pantalla en
pantalla; esto provee una continuidad para que pueda seguir leyendo el
texto.

Lo primero que necesita saber es como moverse de un lugar a otro en el
texto.  Ya sabe como avanzar una pantalla, con C-v.  Para retroceder
una pantalla teclee M-v (mantenga oprimida la tecla META y teclee v, o
teclee <ESC>v si no tiene las teclas META, EDIT o ALT).

>> Intente teclear M-v y luego C-v, varias veces.


* RESUMEN
---------

Los siguientes comandos son útiles para ver pantallas completas:

	C-v 	Avanzar una pantalla completa
	M-v 	Retroceder una pantalla completa
	C-l 	Limpiar la pantalla y mostrar todo el texto de nuevo,
 		 moviendo el texto alrededor del cursor al centro de la
		 pantalla (Esto es CONTROL-L, no CONTROL-1.)

>> Encuentre el cursor, y fíjese qué texto hay cerca de éste.
   Luego teclee C-l.
   Encuentre el cursor otra vez y note que el mismo texto está cerca
   del cursor ahora.

Si su terminal las soporta, también puede usar las teclas AvPág o
RegPág para moverse por pantallas completas, pero puede editar más
eficientemente si usa C-v y M-v.

* MOVIMIENTO BÁSICO DE CURSOR
-----------------------------

Es útil moverse de una pantalla completa a otra pero, ¿cómo moverse a
un lugar específico dentro del texto en la pantalla?

Puede hacerlo de diversas formas.  Puede usar las teclas de flechas,
pero es mas eficiente mantener las manos en la posición estándar y
usar los comandos C-p, C-b, C-f, y C-n.  Estos caracteres son
equivalentes a las cuatro teclas de flechas, de esta manera:

		        Línea anterior, C-P
				:
				:
   Atrás, C-b.... Posición actual del cursor .... Adelante, C-f
				:
				:
			Línea siguiente, C-n

>> Mueva el cursor a la línea en la mitad del diagrama
   usando C-n o C-p. Luego teclee C-l para ver el
   diagrama completo centrado en la pantalla.

Le resultará fácil recordar estas letras por las palabras que
representan: P de Previous (anterior), N de Next (siguiente), B de
Backward (atrás) y F de Forward (adelante).  Estará usando estos
comandos de posicionamiento básico del cursor todo el tiempo.

>> Teclee algunos C-n para traer el cursor a esta línea.

>> Muévase a la línea con C-f y hacia arriba con algunos C-p.  Observe
   lo que hace C-p cuando el cursor está en medio de la línea.

Cada línea de texto termina con un carácter de nueva línea (Newline),
que sirve para separarla de la línea siguiente.  La última línea de su
archivo debe de tener un carácter de nueva línea al final (pero Emacs
no requiere que ésta lo tenga).

>> Intente usar C-b al comienzo de una línea.  Debería moverse al
   final de la línea previa.  Esto sucede porque retrocede a través
   del carácter de nueva línea.

C-f puede moverse a través de una nueva línea igual que C-b.

>> Teclee algunos C-b más para que sienta por donde se encuentra el
   cursor.
   Luego teclee C-f para regresar al final de la línea.
   Luego teclee otro C-f más para moverse a la línea siguiente.

Cuando pase el tope o el final de la pantalla, se mostrará el texto
más allá del borde de la pantalla.  Esto recibe el nombre de
"desplazamiento".  Esto le permite a Emacs mover el cursor al lugar
especificado en el texto sin moverlo fuera de la pantalla.

>> Intente mover el cursor fuera del borde de la pantalla con C-n, y
   observe lo que sucede.

Si moverse por caracteres es muy lento, puede moverse por palabras.
M-f (META-f) mueve adelante una palabra y M-b mueva atrás una palabra.

>> Teclee algunos M-f y M-b

Cuando está en la mitad de una palabra, M-f mueve al final de la
palabra.  Cuando está en un espacio entre dos palabras, M-f mueve al
final de la siguiente palabra.  M-b trabaja de la misma forma en la
dirección opuesta.

>> Teclee algunos M-f y M-b, intercaladas con C-f y C-b de tal forma
   que observe la acción de M-f y M-b desde varios sitios dentro y
   entre palabras.

Note el paralelo entre C-f y C-b de un lado y M-f y M-b del otro.  Muy
frecuentemente los caracteres Meta se usan para operaciones
relacionadas, con las unidades definidas por el lenguaje (palabras,
oraciones y párrafos), mientras los caracteres Control operan sobre
unidades básicas que son independientes de lo que está editando
(caracteres, líneas, etc).

Este paralelo se aplica entre líneas y oraciones: C-a y C-e para
moverse al comienzo o al final de la línea; y M-a y M-e para mover al
comienzo o al final de una oración.

>> Intente algunos C-a, y después unos cuantos C-e.
>> Intente algunos M-a, y después unos cuantos M-e.

Vea cómo la repetición de C-a no hace nada, pero la repetición de M-a
sigue moviendo una oración más.  Aunque no son muy análogas, cada una
parece natural.

La ubicación del cursor en el texto se llama también "punto".  En
otras palabras, el cursor muestra sobre la pantalla donde está
situado el punto dentro del texto.

Aquí hay un resumen de operaciones simples de movimiento del cursor,
incluyendo los comandos de movimiento por palabra y oración:

	C-f 	Avanzar un carácter
	C-d 	Retroceder un carácter

	M-f 	Avanzar una palabra
	M-b 	Retroceder una palabra

	C-n 	Avanzar a la línea siguiente
	C-p 	Retroceder a la línea anterior

	C-a 	Retroceder al comienzo de la línea
	C-e 	Avanzar al final de la línea

	M-a 	Retroceder al comienzo de la oración
	M-e 	Avanzar al final de la oración

>> Ahora pruebe todos estos comandos algunas veces para practicar.
   Estos comandos son los más frecuentemente usados.

Otros dos comandos importantes de movimiento del cursor son M-< (META
Menor que), el cual se mueve al comienzo del texto entero, y M-> (META
Mayor que), el cual se mueve al final del texto entero.

En la mayoría de las terminales, el "<" está sobre la coma, por lo
tanto tiene que usar la tecla shift para teclearlo.  En estas
terminales tendrá que usar la tecla shift también al teclear M-<; sin
la tecla shift, usted estaría escribiendo M-coma.

>> Ahora pruebe M-<, para moverse al comienzo del tutorial.
   Después use C-v repetidamente para regresar aquí.

>> Ahora pruebe M->, para moverse al final del tutorial.
   Después use M-v repetidamente para regresar aquí.

También puede mover el cursor con las teclas de flecha si su terminal
dispone de ellas.  Recomendamos aprender C-b, C-f, C-n y C-p por tres
razones.  Primero, funcionan en todo tipo de terminales.  Segundo, una
vez que gane práctica usando Emacs, encontrará que teclear estos
caracteres Control es más rápido que usar teclas de flecha (porque no
tendrá que mover las manos de la posición para mecanografiar).
Tercero, una vez tenga el hábito de usar estos comandos Control,
también puede aprender a usar otros comandos avanzados de movimiento
del cursor fácilmente.

La mayoría de comandos de Emacs aceptan un argumento numérico; para la
mayoría de comandos esto sirve como un factor de repetición.  La
manera de pasarle un factor de repetición a un comando es tecleando
C-u y luego los dígitos antes de introducir los comandos.  Si tiene
una tecla META (o EDIT o ALT), hay una manera alternativa para
ingresar un argumento numérico: teclear los dígitos mientras presiona
la tecla META.  Recomendamos aprender el método C-u porque este
funciona en cualquier terminal.  El argumento numérico es también
llamado un "argumento prefijo", porque usted teclea el argumento antes
del comando al que se aplica.

Por ejemplo, C-u 8 C-f mueve hacia adelante ocho caracteres.

>> Pruebe usar C-n o C-p con un argumento numérico, para mover el
   cursor a una línea cercana a ésta con un solo comando.

La mayoría de comandos usan el argumento numérico como un factor de
repetición, pero algunos comandos le dan otros usos.  Varios comandos
(pero ninguno de los que ha aprendido hasta ahora) lo usan como una
bandera: la presencia de un argumento prefijo, sin tener en cuenta su
valor, hace que el comando actúe de forma diferente.

C-v y M-v son otro tipo de excepción.  Cuando se les da un argumento,
desplazan la pantalla arriba o abajo esa cantidad de líneas, en vez de
una pantalla completa.  Por ejemplo, C-u 8 C-v desplaza la pantalla 8
líneas.

>> Pruebe tecleando C-u 8 C-v ahora.

Esto debió haber desplazado la pantalla hacia arriba 8 líneas.  Si
quisiera desplazarla hacia abajo de nuevo, puede dar un argumento a
M-v.

Si está usando un sistema de ventanas, como X11 o MS-Windows, debe
haber una larga área rectangular llamada una barra de desplazamiento
en el lado izquierdo de la ventana de Emacs.  Puede desplazar el texto
al oprimir el botón del ratón en la barra de desplazamiento.

>> Pruebe presionando el botón del medio en la parte superior del área
   resaltada en la barra de desplazamiento.  Éste debe desplazar el
   texto a una posición determinada según cuan alto o bajo oprima el
   botón.

>> Intente mover el ratón arriba y abajo, mientras mantiene el botón
   del medio presionado.  Verá que el texto se desplaza arriba y abajo
   a medida que mueve el ratón.



* CUANDO EMACS ESTÁ BLOQUEADO
-----------------------------

Si Emacs dejara de responder a sus comandos, puede detenerlo con
seguridad al teclear C-g.  Puede usar C-g para detener un comando que
esté tomando mucho tiempo para ejecutarse.

También puede usar C-g para descartar un argumento numérico o el
comienzo de un comando que no quiere finalizar.

>> Escriba C-u 100 para hacer un argumento numérico de 100, entonces
   pruebe C-g.
   Ahora pruebe C-f.  Esto deberá mover sólo un carácter, ya que
   canceló el argumento con C-g.

Si ha tecleado <ESC> por error, puede desecharlo con un C-g.


* COMANDOS DESACTIVADOS
-----------------------

Algunos comandos de Emacs están "desactivados" de manera que los
usuarios principiantes no puedan usarlos accidentalmente.

Si teclea uno de los comandos desactivados, Emacs muestra un mensaje
informando acerca de qué comando era, y preguntándole si quiere
continuar y ejecutar el comando.

Si realmente quiere intentar el comando, teclee Espacio como repuesta
a la pregunta.  Normalmente, si no quiere ejecutar el comando
desactivado, conteste la pregunta con "n".

>> Escriba C-x C-l (que es un comando desactivado), a continuación
   escriba n para responder la pregunta.


* VENTANAS
----------

Emacs puede tener varias ventanas, cada una mostrando su propio texto.
Explicaremos después como usar múltiples ventanas.  Ahora mismo
queremos explicar cómo deshacerse de ventanas adicionales y volver a
la edición básica en una ventana.  Es sencillo:

	C-x 1 	Una ventana (p.ej.,  elimina todas las otras ventanas).

Esto es CONTROL-x seguido por el dígito 1.  C-x 1 expande la ventana que
contiene el cursor, para ocupar toda la pantalla.  Esto borra todas las
demás ventanas.

>> Mueva el cursor a esta línea y escriba C-u 0 C-l.
>> Escriba Control-h k Control-f.
   Vea como esta ventana se encoge, mientras una nueva aparece y
   muestra documentación sobre el comando Control-f.

>> Escriba C-x 1 y vea que la ventana de listado de documentación
   desaparece.

Este comando es diferente a los otros que ha aprendido en que éste
consiste de dos caracteres.  Comienza con el carácter CONTROL-x.  Hay
toda una serie de comandos que comienzan con CONTROL-x; muchos de
ellos tienen que ver con ventanas, archivos, buffers y cosas
relacionadas.  Estos comandos son de una longitud de dos, tres o
cuatro caracteres.


* INSERTAR Y BORRAR
-------------------

Si quiere insertar un texto, basta con que lo teclee.  Emacs
interpreta los caracteres que usted puede ver, tales como A, 7, *,
etc. como texto y los inserta inmediatamente.  Teclee <Return> (la
tecla Enter) para insertar un carácter de nueva línea.

Puede borrar el último carácter que escribió oprimiendo <Delback>.
<Delback> es una tecla en el teclado--la misma que normalmente usa
fuera de emacs para borrar el último carácter que escribió.
Normalmente es una tecla una o dos filas arriba de la tecla <Return>,
y que esta usualmente rotulada como "Backspace", "Del" o simplemente
con una flecha en dirección izquierda que no es parte de las teclas de
flecha.

Si la tecla larga esta rotulada "Backspace", entonces esa es la que
debe de usar para <Delback>. Puede haber otra tecla llamada "Del" en
otra parte, pero esa no es <Delback>.

Generalmente, <Delback> borra el carácter inmediatamente anterior a la
posición actual del cursor.

>> Haga esto ahora: teclee unos pocos caracteres, después bórrelos
   tecleando <Delback> varias veces.  No se preocupe si este archivo
   cambia, no alterará el tutorial principal.  Ésta es su copia
   personal de él.

Cuando una línea de texto se hace muy grande para una sola línea en la
pantalla, la línea de texto "continúa" en una segunda línea en la
pantalla.  Un backslash ("\") (o, si está usando un sistema de
ventanas, una pequeña flecha curva) en el margen derecho indica que la
línea "continúa".

>> Inserte texto hasta que llegue al margen derecho, y siga
   insertando.  Verá aparecer una línea de continuación.

>> Use <Delback> para borrar el texto hasta que la línea de nuevo
   quepa en la pantalla.  La línea de continuación se pierde.

Puede borrar un carácter de nueva línea como cualquier otro carácter.
Al borrar el carácter de nueva línea entre dos líneas las junta en una
sola línea.  Si el resultado de la combinación de líneas es demasiado
largo para caber en el ancho de la pantalla, se mostrará con una línea
de continuación.

>> Mueva el cursor al comienzo de una línea y teclee <Delback>.  Esto
   juntará esa línea con la línea anterior.

>> Teclee <Return> para reinsertar la nueva línea que borró.

Recuerde que a la mayoría de los comandos de Emacs se les puede dar un
factor de repetición; esto incluye los caracteres de texto.  Repetir
un carácter de texto lo inserta varias veces.

>> Inténtelo ahora: teclee C-u 8 * para insertar ********.

Ya ha aprendido la manera más básica de teclear algo en Emacs y
corregir errores.  Puede borrar por palabras o por líneas.  He aquí un
resumen de las operaciones de borrado:

	<Delback> 	borra el carácter justo antes que el cursor
	C-d		borra el siguiente carácter después del cursor

	M-<Delback> 	Elimina la palabra inmediatamente antes del
                         cursor
	M-d        	Elimina la siguiente palabra después del cursor

	C-k  		Elimina desde el cursor hasta el fin de la línea
	M-k  		Elimina hasta el final de la oración actual

Note que <Delback> y C-d, comparados con M-<Delback> y M-d, extienden
el paralelismo iniciado por C-f y M-f (bien, <Delback> no es realmente
una tecla de control, pero no nos preocuparemos de eso ahora).  C-k y
M-k, en ciertas forma, son como C-e y M-e, en que las líneas son
oraciones opuestas.

También puede eliminar cualquier parte del buffer con un método
uniforme.  Muévase a un extremo de esa parte, y teclee C-@ o C-SPC
(cualquiera de los dos).  (SPC es la barra espaciadora.)  Muévase al
otro extremo de esa parte, y teclee C-w.  Eso elimina todo el texto
entre las dos posiciones.

>> Mueva el cursor a la letra T del inicio del párrafo anterior.
>> Teclee C-SPC. Emacs debe mostrar el mensaje "Mark set" en la parte
   de abajo de la pantalla.
>> Mueva el cursor a la x en "extremo", en la segunda línea del
   párrafo.
>> Teclee C-w.  Esto eliminará el texto que comienza desde la T, y
   termina justo antes de la x.

La diferencia entre "eliminar" y "borrar" es que el texto "eliminado"
puede ser reinsertado, mientras que las cosas "borradas" no pueden ser
reinsertadas.  La reinserción de texto eliminado se llama "yanking" o
"pegar".  Generalmente, los comandos que pueden quitar mucho texto lo
eliminan, mientras que los comandos que quitan solo un carácter, o
solo lineas en blanco y espacios, borran (para que no pueda pegar ese
texto).

>> Mueva el cursor al comienzo de una línea que no esté vacía.
   Luego teclee C-k para eliminar el texto de esa línea.
>> Teclee C-k por segunda vez.  Verá que elimina la nueva línea que
   sigue a esa línea.

Note que al teclear C-k una sola vez elimina el contenido de la línea,
y un segundo C-k elimina la línea misma, y hace que todas las otras
líneas se muevan hacia arriba.  C-k trata un argumento numérico
especialmente: Elimina ese número de líneas y TAMBIÉN sus
contenidos. Esto no es una simple repetición.  C-u 2 C-k elimina dos
líneas y sus nuevas líneas, tecleando C-k dos veces no hace esto.

Traer texto eliminado de regreso es llamado "yanking" o "pegar".
(Piense en ello como pegar de nuevo, o traer de vuelta, algún texto
que le fue quitado.)  Puede pegar el texto eliminado en, ya sea el
lugar en que fue eliminado, o en otra parte del buffer, o hasta en un
archivo diferente. Puede pegar el texto varias veces, lo que hace
varias copias de él.

El comando para pegar es C-y.  Reinserta el último texto eliminado, en
la posición actual del cursor.

>> Inténtelo; teclee C-y para pegar de nuevo el texto.

Si hace varios C-k seguidos, todo el texto eliminado se guarda junto,
de manera que un C-y pegará todas las líneas al mismo tiempo.

>> Haga esto ahora, teclee C-k varias veces.

Ahora para recuperar ese texto eliminado:

>> Teclee C-y.  Luego baje el cursor unas pocas líneas y teclee C-y de
   nuevo.  De esta forma puede copiar un texto.

¿Qué hacer si tiene algún texto que quiere pegar, y entonces elimina
otra cosa?  C-y pegaría la eliminación más reciente.  Pero el texto
previo no está perdido.  Puede regresar a éste usando el comando M-y.
Después de haber tecleado C-y para conseguir la eliminación más
reciente, tecleando M-y reemplaza el texto pegado con la eliminación
previa.  Tecleando M-y una y otra vez traerá las eliminaciones
anteriores.  Cuando haya encontrado el texto que buscaba, no tiene que
hacer nada para conservarlo.  Sólo siga con su edición, dejando el
texto pegado en donde está.

Si teclea M-y suficientes veces, regresa al punto inicial (la
eliminación más reciente).

>> Elimine una línea, muévase un poco, elimine otra línea.
   Luego teclee C-y para recuperar la segunda línea eliminada.
   Luego teclee M-y y será reemplazado por la primera línea eliminada.
   Teclee más veces M-y y vea lo que obtiene.  Siga haciéndolo hasta
   que la segunda línea eliminada regrese, y entonces unas pocas
   más. Si quiere, puede tratar de darle a M-y argumentos positivos y
   negativos.


* DESHACER
----------

Si hace un cambio al texto, y luego decide que fue un error,
puede deshacer el cambio con el comando deshacer, C-x u.

Normalmente, C-x u deshace los cambios hechos por un comando; si repite
varias veces seguidas C-x u, cada repetición deshará un comando
adicional.

Pero hay dos excepciones: los comandos que no cambian el texto no
cuentan (esto incluye los comandos de movimiento del cursor y el
comando de desplazamiento), y los caracteres de autoinserción se
manejan usualmente en grupos de hasta 20. (Esto es para reducir el
numero de C-x u que tenga que teclear para deshacer una inserción en
el texto.)

>> Elimine esta línea con C-k, después teclee C-x u y debería
   reaparecer.

C-_ es un comando alternativo para deshacer; funciona igual que C-x u,
pero es más fácil de teclear varias veces seguidas.  La desventaja de
C-_ es que en algunos teclados no es obvio cómo se teclea.  Por esto
existe también C-x u.  En algunas terminales, puede teclear C-_ al
teclear / mientras oprime CONTROL.

Un argumento numérico para C-_ o C-x u actúa como un factor de
repetición.

Uuede deshacer un texto borrado justo como puede deshacer el texto
eliminado.  La distinción entre eliminar algo y borrar algo afecta en
si puede pegarlo con C-y; no hay diferencia alguna para deshacer.


* ARCHIVOS
----------

Para que pueda hacer permanente el texto que edite, lo debe colocar en
un archivo.  De otra manera, éste se perderá cuando cierre Emacs.
Para poder poner su texto en un archivo, debe "encontrar" el archivo
antes de ingresar el texto. (Esto se llama también "visitar" el
archivo.)

Encontrar un archivo significa que puede ver su contenido dentro de
Emacs.  En cierta forma, es como si estuviera editando el archivo
mismo.  Sin embargo los cambios que haga mediante Emacs no serán
permanentes hasta que "guarde" el archivo.  Esto es para evitar dejar
un archivo a medio cambiar en el sistema cuando no quiera.  Incluso
cuando guarde, Emacs dejará el archivo original bajo un nombre
cambiado en caso de que luego decida que sus cambios fueron un error.

Si mira cerca del final de la pantalla podrá ver una línea que
comienza y termina con guiones, y comienza con "--:-- TUTORIAL.es" o
algo así.  Esta parte de la pantalla normalmente muestra el nombre del
archivo que está visitando.  En este momento está visitando un archivo
llamado "TUTORIAL.es" que es su borrador personal del tutorial de
Emacs.  Cuando encuentre un archivo con Emacs, el nombre de ese
archivo aparecerá en ese mismo punto.

Una cosa especial acerca del comando para encontrar un archivo, es que
tendrá que decir que nombre de archivo desea.  Decimos que el comando
"lee un argumento desde la terminal" (en este caso, el argumento es el
nombre del archivo).  Después de teclear el comando:

	C-x C-f   Encontrar un archivo

Emacs le pide que teclee el nombre del archivo.  El nombre de archivo
que teclee aparece en la línea final de la pantalla.  A la línea final
de la pantalla se la denomina minibuffer cuando se utiliza para este
tipo de entradas.  Puede usar comandos de edición ordinarios de Emacs
para editar el nombre del archivo.

Mientras está ingresando el nombre del archivo (o cualquier otra
entrada al minibuffer) puede cancelar el comando con C-g.

>> Teclee C-x C-f, luego teclee C-g.  Esto cancela el minibuffer, y
   también cancela el comando C-x C-f que estaba usando el minibuffer.
   Así que no encontrará archivo alguno.

Cuando haya finalizado de ingresar el nombre del archivo, teclee
<Return> para terminarlo.  Entonces el comando C-x C-f trabaja, y
encuentra el archivo que escogió.  El minibuffer desaparece cuando el
comando C-x C-f termina.

Poco tiempo después aparecerá el contenido del archivo en la pantalla,
y puede editarlo.  Cuando quiera que sus cambios sean permanentes,
teclee el comando

	C-x C-s   Guardar el archivo

Esto copia el texto dentro de Emacs al archivo.  La primera vez que
haga esto, Emacs renombrará el archivo original con un nuevo nombre
para que este no se pierda. El nuevo nombre se hace agregando "~" al
final del nombre del archivo original.

Cuando guardar haya terminado, Emacs mostrará el nombre del archivo
escrito.  Deberá guardar frecuentemente, para que no pierda mucho
trabajo si el sistema falla.

>> Teclee C-x C-s, guardando la copia del tutorial.
   Esto debería mostrar "Wrote ...TUTORIAL.es" al final de la
   pantalla.

NOTA: En algunos sistemas, teclear C-x C-s dejará inmóvil la pantalla
y no podrá ver más respuesta de Emacs.  Esto indica que una
"característica" del sistema operativo llamada "control de flujo" está
interceptando el C-s y no permitiéndole llegar hasta Emacs.  Para
descongelar la pantalla, teclee C-q.  Luego consulte la sección
"Entrada Espontánea para Búsqueda Incremental" en el manual de Emacs
para consejos de cómo tratar con esta "característica".

Puede encontrar un archivo existente, para verlo o editarlo.  También
puede hacerlo con un archivo que no exista.  Ésta es la forma de crear
un archivo en Emacs: encuentre el archivo, que comenzará vacío, luego
comience a insertar el texto para ese archivo.  Cuando invoque
"guardar" el archivo, Emacs creará realmente el archivo con el texto
que ha insertado.  De ahí en adelante, puede considerarse estar
editando un archivo existente.


* BUFFERS
---------

Si visita un segundo archivo con C-x C-f, el primer archivo permanece
dentro de Emacs.  Puede volver a el encontrándolo de nuevo con C-x
C-f.  De esta forma puede mantener un gran número de archivos dentro
de Emacs.

>> Cree un archivo llamado "foo" tecleando C-x C-f foo <Return>.
   Luego inserte algún texto, edítelo, y guarde "foo" tecleando C-x
   C-s.
   Finalmente teclee C-x C-f TUTORIAL.es <Return>
   para regresar al tutorial.

Emacs almacena cada texto del archivo dentro de un objeto llamado
"buffer".  Al encontrar un archivo se crea un nuevo buffer dentro de
Emacs.  Para mirar la lista de los buffers que existen actualmente en
su sesión de Emacs, teclee:

	C-x C-b   Lista de Buffers

>> Pruebe C-x C-b ahora.

Vea como cada buffer tiene un nombre, y además puede tener un nombre
de archivo para el archivo que contiene. CUALQUIER texto que vea en
una ventana de Emacs es siempre parte de algún Buffer.

>> Teclee C-x 1 para deshacerse de la lista de buffers.

Cuando tenga varios buffers, solo uno de ellos es "actual" en algún
momento.  Ese buffer es el que actualmente edita.  Si quiere editar
otro buffer, necesita "cambiar" a él.  Si quiere cambiar a un buffer
que corresponde a un archivo, puede hacerlo visitando el archivo de
nuevo con C-x C-f.  Pero existe una manera más rápida: use el comando
C-x b. En ese comando, necesita teclear el nombre de buffer.

>> Teclee C-x b foo <Return> para volver al buffer "foo" que contiene
   el texto del archivo "foo".  Después teclee C-x b TUTORIAL.es
   <Return> para regresar a este tutorial.

La mayoría del tiempo el nombre del buffer es el mismo que el nombre
del archivo (sin la parte del directorio del archivo). Sin embargo,
esto no es así siempre.  La lista de buffers que hace con C-x C-b
siempre muestra el nombre de todos los buffers.

CUALQUIER texto que vea en una ventana de Emacs siempre es parte de un
buffer.  Algunos buffers no corresponden a un archivo.  Por ejemplo,
el buffer llamado "*Buffer List*" no tiene ningún archivo.  Es el
buffer que contiene la lista de buffers que ha creado con C-x C-b.  El
buffer llamado "*Messages*" tampoco tiene un archivo correspondiente;
contiene los mensajes que han aparecido en la línea de abajo durante
su sesión de Emacs.

>> Teclee C-x b *Messages* <Return> para ver el buffer de mensajes.
   Luego teclee C-b TUTORIAL <Return> para regresar a este tutorial.

Si hace cambios al texto de un archivo, y luego encuentra otro
archivo, esto no guarda el primer archivo.  Sus cambios permanecerán
dentro de Emacs en ese buffer del archivo.  La creación o edición del
segundo buffer de archivo no afecta al primero.  Esto es muy útil,
pero también significa que necesita una forma conveniente para guardar
el archivo del primer buffer.  Sería una molestia tener que volver a
este con C-x C-f para guardarlo con C-x C-s. Así tenemos

	C-x s 	Guardar algunos buffers

C-x s le pregunta sobre cada buffer que contenga cambios que no haya
guardada.  Le pregunta, por cada buffer, si quiere guardarlo o no.

>> Inserte una línea de texto, luego teclee C-x s.
   Debería preguntarle si desea guardar el buffer llamado TUTORIAL.es.
   Conteste si a la pregunta tecleando "y".


* EXTENDER EL CONJUNTO DE COMANDOS
----------------------------------

Hay muchísimos más comandos de Emacs que los que podrían asignarse a
todos los caracteres control y meta.  Emacs puede darle la vuelta a
esto usando el comando X (eXtendido).  Este viene de dos formas:

	C-x 	Carácter eXtendido. Seguido por un carácter.
        M-x 	Comando eXtendido por nombre. Seguido por un nombre
                largo.

Estos comandos son generalmente útiles pero menos usados que los
comandos que ha aprendido hasta ahora.  Ya ha visto dos: los comandos
de archivo C-x C-f para Encontrar y C-x C-s para Guardar.  Otro
ejemplo es el comando para terminar la sesión de Emacs: se trata del
comando C-x C-c.  (No se preocupe por perder los cambios que haya
hecho; C-x C-c ofrece guardar cada archivo alterado antes de finalizar
Emacs.)

C-z es el comando para salir de Emacs *temporalmente*: para que pueda
regresar a la misma sesión de Emacs después.

En sistemas que lo permiten C-z "suspende" Emacs; esto es, se regresa
al intérprete de comandos pero no se destruye Emacs.  En los
intérpretes de comandos más comunes, puede reanudar Emacs con el
comando `fg' o con `%emacs'.

En sistemas que no implementen el suspendido, C-z crea un
subintérprete que corre bajo Emacs para darle la opción de correr
otros programas y regresar a Emacs después; esto en realidad no "sale"
de Emacs.  En este caso, el comando `exit' del intérprete es la vía
usual para regresar a Emacs desde éste.

El momento para usar C-x C-c es cuando está listo para salir del
sistema.  Es además el paso correcto para salir de un Emacs llamado
bajo programas de manejo de correo y diversas otras utilidades, puesto
que ellos no saben cómo lidiar con la suspensión de Emacs.  En
circunstancias normales, si no va a salir, es mejor suspender
Emacs con C-z en lugar de salir de él.

Existen varios comandos C-x.  Aquí hay una lista de los que ha
aprendido:

	C-x C-f 	Encontrar archivo.
        C-x C-s 	Guardar archivo.
        C-x C-b 	Lista de buffers.
        C-x C-c 	Salir de Emacs.
        C-x 1 		Borrar todo menos una ventana.
        C-x u 		Deshacer.

Los comandos eXtendidos por nombre son comandos que se utilizan aún
con menos frecuencia, o únicamente en ciertos modos.  Un ejemplo es el
comando replace-string, el cual globalmente substituye una cadena de
caracteres por otra.  Cuando teclea M-x, Emacs le pregunta al
final de la pantalla con M-x y debe escribir el nombre del
comando; en este caso "replace-string".  Solo teclee "repl s<TAB>" y
Emacs completará el nombre.  Finalice el nombre del comando con
<Return>.

El comando replace-string requiere dos argumentos: la cadena de
caracteres a reemplazar, y la cadena de caracteres para reemplazarla.
Debe terminar cada argumento con <Return>.

>> Mueva el cursor hacia la línea en blanco dos líneas abajo de esta.
   A continuación escriba
   M-x repl s<Return>cambiado<Return>alterado<Return>.

   Note cómo esta línea ha cambiado: ha substituido la palabra
   c-a-m-b-i-a-d-o por "alterado" en cada ocurrencia, después de la
   posición inicial del cursor.


* AUTO GUARDADO
---------------

Si ha hecho cambios en un archivo, pero no los ha guardado, éstos
podrían perderse si su computadora falla.  Para protegerlo de esto,
Emacs periódicamente escribe un archivo "auto guardado" para cada
archivo que está editando.  El nombre del archivo auto guardado tiene
un # al principio y al final; por ejemplo, si su archivo se llama
"hola.c", su archivo auto guardado es "#hola.c#".  Cuando guarda por
la vía normal, Emacs borra su archivo de auto guardado.

Si la computadora falla, puede recuperar su edición de auto
guardado encontrando el archivo normal (el archivo que estuvo
editando, no el archivo de auto guardar) y entonces tecleando M-x
recover file<Return>.  Cuando le pregunte por la confirmación, teclee
yes<Return> para ir y recuperar la información del auto guardado.


* ÁREA DE ECO
-------------

Si Emacs ve que usted está tecleando comandos de multicaracteres
lentamente, se los muestra al final de la pantalla en un área llamada
"área de eco".  El área de eco contiene la línea final de la pantalla.


* LÍNEA DE MODO
---------------

La línea inmediatamente encima del área de eco recibe el nombre de
"línea de modo" o "mode line".  La línea de modo dice algo así:

--:** TUTORIAL.es         (Fundamental)--l765--65%---------

Esta línea da información útil acerca del estado de Emacs y del texto
que está editando.

Ya sabe qué significa el nombre del archivo: es el archivo que usted
ha encontrado.  -NN%-- indica su posición actual en el texto; esto
significa que NN por ciento del texto está encima de la parte superior
de la pantalla.  Si el principio del archivo está en la pantalla, este
dirá --Top-- en vez de --00%--.  Si el final del texto está en la
pantalla, dirá --Bot--.  Si está mirando un texto tan pequeño que cabe
en la pantalla, el modo de línea dirá --All--.

La L y los dígitos indican la posición de otra forma: ellos dan el
número de línea actual del punto.

Los asteriscos cerca del frente significan que usted ha hecho cambios
al texto.  Inmediatamente después que visite o guarde un archivo, esa
parte de la línea de modo no muestra asteriscos, solo guiones.

La parte de la línea de modo dentro de los paréntesis es para
indicarle en qué modo de edición está.  El modo por omisión es
Fundamental, el cual está usando ahora.  Este es un ejemplo de un
"modo mayor".

Emacs tiene diferentes modos mayores.  Algunos están hechos para
editar diferentes lenguajes y/o clases de texto, tales como modo de
Lisp, modo de Texto, etc.  En cualquier momento uno y solo un modo
mayor está activo, y su nombre siempre se puede encontrar en la línea
de modo, justo en donde "Fundamental" está ahora.

Cada modo mayor hace que algunos comandos actúen diferente.  Por
ejemplo, hay comandos para crear comentarios en un programa, y como
cada lenguaje de programación tiene una idea diferente de cómo debe
verse un comentario, cada modo mayor tiene que insertar comentarios de
forma distinta.  Cada modo mayor es el nombre de un comando extendido,
que es como puede cambiar a ese modo.  Por ejemplo, M-x
fundamental-mode es un comando para cambiar al modo fundamental.

Si va a editar un texto de algún lenguaje humano, como este archivo,
debería usar el modo de texto.
>> Teclee M-x text mode<Return>.

No se preocupe, ninguno de los comandos de Emacs que ha aprendido
cambia de manera significativa.  Pero puede observar que M-f y M-b
tratan los apóstrofes como parte de las palabras.  Previamente, en
modo Fundamental, M-f y M-b trataban los apóstrofes como separadores
de palabras.

Los modos mayores normalmente hacen cambios sutiles como el anterior:
la mayoría de comandos hacen "el mismo trabajo" en cada modo mayor,
pero funcionan un poco diferente.

Para ver documentación en el modo mayor actual, teclee C-h m.

>> Use C-u C-v una o más veces para traer esta línea cerca de la
   parte superior de la pantalla.

>> Teclee C-h m, para ver como el modo de Texto difiere del modo
   Fundamental.

>> Teclee C-x 1 para eliminar la documentación de la pantalla.

Los modos mayores son llamados así porque también hay modos menores.
Los modos menores no son alternativas para los modos mayores, solo
modificaciones menores de éstos.  Cada modo menor puede ser activado o
desactivado por sí mismo, independiente de todos los otros modos
menores, e independiente de su modo mayor.  Por tanto, puede no usar
modos menores, o solamente uno, o cualquier combinación de varios
modos menores.

Un modo menor que es muy útil, especialmente para editar textos en
español, es el modo Auto Fill.  Cuando este modo está activado, Emacs
rompe la línea entre palabras automáticamente siempre que inserte
texto y la línea sea demasiado ancha.

Puede activar el modo Auto Fill al hacer M-x auto fill mode<Return>.
Cuando el modo esté activado, puede desactivarlo nuevamente usando M-x
auto fill mode<Return>.  Si el modo está desactivado, este comando lo
activa, y si el modo está activado, este comando lo desactiva.
Decimos que el comando "cambia el modo".

>> teclee M-x auto fill mode<Return> ahora.  Luego inserte una línea
   de "asdf " repetidas veces hasta que la vea dividida en dos líneas.
   Debe intercalar espacios porque Auto Fill sólo rompe líneas en los
   espacios.

El margen esta normalmente puesto en 70 caracteres, pero puede
cambiarlo con el comando C-x f.  Debe indicar el margen deseado como
un argumento numérico.

>> Teclee C-x f con un argumento de 20.  (C-u 2 0 C-x f).  Luego
   teclee algún texto y vea como Emacs lo parte en líneas de 20
   caracteres.  A continuación ponga de nuevo el margen a 70 usando
   otra vez C-x f.

Si hace cambios en el medio de un párrafo, el modo Auto Fill no lo
rellenará por usted.
Para rellenar el párrafo, teclee M-q (META-q) con el cursor dentro de
ese párrafo.

>> Mueva el cursor al párrafo anterior y teclee M-q.


* BUSCAR
--------

Emacs puede hacer búsquedas de cadenas (grupos de caracteres o
palabras contiguos) hacia adelante a través del texto o hacia atrás en
el mismo.  La búsqueda de una cadena es un comando de movimiento de
cursor; mueve el cursor al próximo lugar donde esa cadena aparece.

El comando de búsqueda de Emacs es diferente a los comandos de
búsqueda de los demás editores, en que es "incremental".  Esto
significa que la búsqueda ocurre mientras teclea la cadena para
buscarla.

El comando para iniciar una búsqueda es C-s para búsqueda hacia
adelante, y C-r para la búsqueda hacia atrás.  ¡PERO ESPERE!  No los
intente aún.

Cuando teclee C-s verá que la cadena "I-search" aparece como una
petición en el área de eco.  Esto le indica que Emacs está en lo que
se conoce como búsqueda incremental, esperando que teclee lo que
quiere buscar.  <Return> termina una búsqueda.

>> Ahora teclee C-s para comenzar la búsqueda.  LENTAMENTE, una letra
   a la vez, teclee la palabra 'cursor', haciendo pausa después de
   cada carácter para notar lo que pasa con el cursor.
   Ahora ha buscado "cursor", una vez.
>> Teclee C-s de nuevo, para buscar la siguiente ocurrencia de
   "cursor".
>> Ahora teclee <Delback> cuatro veces y vea como se mueve el cursor.
>> Teclee <Return> para terminar la búsqueda.

¿Vió lo que ocurrió?  Emacs, en una búsqueda incremental, trata de ir
a la ocurrencia de la cadena que ha tecleado hasta el momento.  Para
ir a la próxima ocurrencia de 'cursor' solo teclee C-s de nuevo.  Si
tal ocurrencia no existe, Emacs pita y le dice que la búsqueda actual
está fallando ("failing").  C-g también termina la búsqueda.

NOTA: En algunos sistemas, teclear C-s dejará inmóvil la pantalla y no
podrá ver más respuesta de Emacs.  Esto indica que una
"característica" del sistema operativo llamada "control de flujo" está
interceptando el C-s y no permitiéndole llegar hasta Emacs.  Para
descongelar la pantalla, teclee C-q.  Luego consulte la sección
"Entrada Espontánea para Búsqueda Incremental" en el manual de Emacs
para consejos de cómo tratar con esta "característica".

Si se encuentra en medio de una búsqueda incremental y teclea
<Delback>, notará que el último carácter de la cadena buscada se borra
y la búsqueda vuelve al sitio anterior de la búsqueda.  Por ejemplo,
suponga que ha tecleado "c", para buscar la primera ocurrencia de "c".
Ahora, si teclea "u", el cursor se moverá a la primera ocurrencia de
"cu".  Ahora teclee <Delback>.  Esto borra la "u" de la cadena
buscada, y el cursor vuelve a la primera ocurrencia de "c".

Si está en medio de una búsqueda y teclea un carácter control o meta
(con algunas pocas excepciones: los caracteres que son especiales en
una búsqueda, tales como C-s y C-r), la búsqueda termina.

El C-s inicia una exploración que busca alguna ocurrencia de la cadena
buscada DESPUÉS de la posición actual del cursor.  Si quiere buscar
algo anterior en el texto, teclee en cambio C-r.  Todo lo que hemos
dicho sobre C-s también se aplica a C-r, excepto que la dirección de
la búsqueda se invierte.


* MÚLTIPLES VENTANAS
--------------------

Una de las características agradables de Emacs es que se puede mostrar
más de una ventana en la pantalla al mismo tiempo.

>> Mueva el cursor a esta línea y teclee C-u 0 C-l (eso es CONTROL-L,
   no CONTROL-1).

>> Ahora teclee C-x 2 que divide la pantalla en dos ventanas.  Ambas
   ventanas muestran este tutorial.  El cursor permanece en la ventana
   superior.

>> Teclee C-M-v para desplazar la ventana inferior.
   (Si no tiene una tecla META real, teclee ESC C-v.)

>> Teclee C-x o ("o" para "otro") para mover el cursor a la ventana
   inferior.
>> Use C-v y M-v en la ventana inferior para desplazarla.
   Siga leyendo estas direcciones en la ventana superior.

>> Teclee C-x o de nuevo para mover el cursor de vuelta a la ventana
   superior.
   El cursor en la ventana superior está justo donde estaba antes.

Puede continuar usando C-x o para cambiar entre las ventanas.  Cada
ventana tiene su propia posición del cursor, pero únicamente una
ventana actual muestra el cursor.  Todos los comandos de edición
comunes se aplican a la ventana en que está el cursor.  Llamaremos
esto la "ventana seleccionada".

El comando C-M-v es muy útil cuando está editando un texto en una
ventana y usando la otra ventana como referencia.  Puede mantener el
cursor siempre en la ventana donde está editando, y avanzar a la otra
ventana secuencialmente con C-M-v.

C-M-v es un ejemplo de un carácter CONTROL-META.  Si tiene una tecla
META real, puede teclear C-M-v pulsando a la vez CONTROL y META
mientras teclea v.  No importa qué tecla "vaya primero", CONTROL o
META, porque las dos teclas actúan modificando los caracteres que
teclea.

Si no tiene una tecla META real, y en vez de eso usa ESC, el orden sí
importa: debe teclear ESC seguido de Control-v, porque Control-ESC v
no funcionará.  Esto es porque ESC es un carácter que tiene valor por
sí mismo, no es una tecla modificadora.

>> Teclee C-x 1 (en la ventana de arriba) para deshacerse de la
   ventana de abajo.

(Si hubiera tecleado C-x 1 en la ventana inferior, esto eliminaría la
superior.  Piense en este comando como "mantener sólo una
ventana--aquella en la cual estoy.")

No tiene por qué mostrarse el mismo buffer en ambas ventanas.  Si usa
C-x C-f para encontrar un archivo en una ventana, la otra ventana no
cambia.  Puede encontrar un archivo en cada ventana
independientemente.

Aquí hay otra forma para usar dos ventanas para mostrar dos cosas
diferentes:

>> Teclee C-x 4 C-f seguido del nombre de uno de sus archivos.
   Finalice con <Return>.  Vea que el archivo especificado aparece en
   la ventana inferior.  El cursor vá allá también.

>> Teclee C-x o para regresar a la ventana superior, y C-x 1 para
   borrar la ventana inferior.


* NIVELES RECURSIVOS DE EDICIÓN
--------------------------------

Algunas veces entrará a lo que es llamado un "nivel recursivo de
edición".  Esto se indica en la línea de modo mediante corchetes en la
línea de modo, rodeando los paréntesis del nombre del modo mayor.  Por
ejemplo, probablemente vea [(Fundamental)] en vez de (Fundamental).

Para salir de los niveles recursivos de edición, teclee ESC ESC ESC.
Éste es un comando de "salida" para todo propósito.  También lo puede
usar para eliminar ventanas extras, y salir del minibuffer.

>> Teclee M-x para entrar a un minibuffer; luego teclee ESC ESC ESC
   para salir.

No se puede usar C-g para salir de los "niveles recursivos de
edición".  Esto es porque C-g es usado para cancelar comandos y
argumentos DENTRO del nivel recursivo de edición.


* CONSEGUIR MAS AYUDA
---------------------

En este tutorial hemos tratado de ofrecer suficiente información para
que empiece a usar Emacs.  Hay tanto disponible en Emacs que sería
imposible explicar todo aquí.  Sin embargo, quizá desee aprender más
sobre Emacs, ya que tiene muchas otras características útiles.  Emacs
provee comandos para leer documentación acerca de los comandos de
Emacs.  Todos estos comandos de "ayuda" comienzan con el carácter
Control-h, que es llamado "el carácter de Ayuda (Help)".

Para usar las funciones de ayuda, teclee el carácter C-h, y luego un
carácter decidiendo qué tipo de ayuda quiere.  Si está REALMENTE
perdido teclee C-h ? y Emacs le dirá qué tipo de ayuda puede
ofrecerle.  Si ha tecleado C-h y decide que no quiere ninguna ayuda,
teclee C-g para cancelarlo.

(En algunas instalaciones cambian el significado del carácter C-h.
Realmente no deberían hacer esto como una política para todos los
usuarios, así que tiene argumentos para quejarse al administrador del
sistema.  Mientras tanto, si C-h no muestra un mensaje de ayuda en el
final de la pantalla, intente teclear la tecla F1 o, en su lugar, M-x
help <Return>).

La función de AYUDA más básica es C-h c.  Teclee C-h, el carácter c y
un carácter de comando o secuencia de comando; Emacs le mostrará
una descripción muy breve del comando.

>> Teclee C-h c C-p.
  El mensaje debe ser algo como

	  C-p runs the command previous-line

Esto le dice el "nombre de la función".  Los nombres de función se
usan principalmente para adecuar y extender Emacs.  Pero ya que los
nombres de las funciones se eligen para indicar lo que el comando
hace, también pueden servir como una breve documentación: suficiente
para recordarle los comandos que ha aprendido.

Los comandos de múltiples caracteres tales como C-x C-s y (sí no tiene
las teclas META o EDIT o ALT) <ESC>v también están permitidos después
de C-h c.

Para conseguir más información sobre un comando use C-h k en vez de
C-h c.

>> Teclee C-h k C-p.

Esto muestra la documentación de la función, al igual que el nombre,
en una ventana de Emacs.  Cuando haya terminado de leer el resultado,
teclee C-x 1 para deshacerse del texto de ayuda.  No tiene que hacer
esto ahora.  Puede hacer algunas ediciones mientras se refiere
al texto de ayuda, y entonces teclear C-x 1.

Aquí hay algunas otras opciones útiles de C-h:

   C-h f	Describe una función.  Usted teclea el nombre de la
		función.

>> Intente teclear C-h f previous-line<Return>.
   Esto muestra toda la información que Emacs tiene sobre la función
   que implementa el comando C-p

Un comando similar, C-h v, muestra la documentación de variables cuyos
valores pueda poner para adecuar el comportamiento de Emacs.  Necesita
teclear el nombre de la variable cuando Emacs pregunte por ella.

   C-h a 	Comando Apropos. Teclee una palabra y Emacs hará una
		lista de todos los comandos que contengan esa palabra.
		Todos estos comandos pueden ser invocados con META-x.
		Para algunos comandos, el Comando Apropos también
		listará una secuencia de uno o dos caracteres la cual
		ejecutará el mismo comando.

>> Teclee C-h a file<Return>.

Esto muestra en otra ventana una lista de todos los comandos M-x con
la palabra "file" en sus nombres.  Verá comandos de caracteres como
C-x C-f listados además de los nombres de los comandos
correspondientes tales como find-file.

>> Teclee C-M-v para desplazar la ventana de ayuda.  Haga esto unas
   cuantas veces.

>> Teclee C-x 1 para borrar la ventana de ayuda.

   C-h i        Leer los Manuales En-Línea (alias Info).  Este comando
                lo pone en un buffer especial llamado `*info*' donde
                puede leer manuales en línea de los paquetes
                instalados en su sistema.  Teclee m Emacs <Return>
                para leer el manual de Emacs.  Sí nunca ha usado Info
                antes, teclee ? y Emacs lo llevará en una visita
                guiada de los servicios del modo de Info.  Una vez que
                haya terminado este tutorial, debería considerar el
                manual Info de Emacs como su documentación primaria.


* MÁS CARACTERÍSTICAS
---------------------

Puede aprender más de Emacs leyendo su manual, ya sea como libro o en
línea en el Info (use el menú Ayuda--"Help"--o teclee F10 h r). Dos
características que pueden gustarle son la completación, que ahorra
teclear, y dired, que simplifica el manejo de archivos.

La completación es una manera de ahorrar teclear innecesariamente.
Por ejemplo, si quiere cambiarse al buffer "*Messages*", puede teclear
C-x b *M<Tab> y emacs encontrará el resto del nombre del buffer tan
lejos como pueda determinar de lo que ya haya tecleado.  La
completación es descrita en el Info del manual de Emacs en el nodo
llamado "Completation".

Dired le permite listar los archivos en un directorio (y opcionalmente
sus subdirectorios), moverse alrededor de esa lista, visitar,
renombrar, borrar y aparte de eso operar en los archivos.  Dired esta
descrito en el Info en el manual de Emacs en el nodo llamado "Dired".

El manual también describe otras características de Emacs.


* CONCLUSIÓN
------------

Recuerde, para salir permanentemente de Emacs use C-x C-c.  Para salir
temporalmente a un intérprete de comandos, de forma que puede volver a
Emacs después, use C-z.

Este tutorial intenta ser comprensible para todos los usuarios nuevos,
así que si encuentra algo que no esté claro, no se siente y se culpe a
sí mismo: ¡Quéjese!


* COPIA
-------

Este tutorial desciende de una larga línea de tutoriales de Emacs
comenzando con el escrito por Stuart Cracraft para el Emacs original.

La versión en español fue originalmente traducida por estudiantes del
Gimnasio Fidel Cano (un colegio en Santafé de Bogotá, Colombia):

	Carlos Alberto López Troncoso
	Andrés Felipe Mancipe Galvis
	Lina Fernanda Pinto García
	Liliana Carolina Quitián Cedeño
	Leonardo Ramírez Vargas <leonardoramirez@latinmail.com>
	Juan David Vargas Botero <cyberbob1164@hotmail.com>
	Juan Pablo Yela Gallón
	Jorge Enrique Cárdenas Carrillo <platypus_life@hotmail.com>

La versión en español ha sido revisada y corregida por:

	Pablo Reyes <reyes_pablo@hotmail.com>
	Igor Támara <ikks@bigfoot.com>
	Melissa Giraldo de Támara <melagira@yahoo.com>
	Vladimir Támara <vtamara@gnu.org>
        Rafael Sepúlveda <drs@gnulinux.org.mx>
        Juanma Barranquero <lektu@terra.es>

La versión en español ha sido actualizada por:

	Rafael Sepúlveda <drs@gnulinux.org.mx>

Por favor, en caso de duda, sólo es válido el original en inglés de la
siguiente nota de derechos de reproducción (que puede encontrar en el
archivo TUTORIAL).

Copyright (C) 1985, 1996, 1998, 2001, 2002, 2003, 2004,
   2005, 2006, 2007  Free Software Foundation, Inc.

   Se permite a cualquiera hacer o distribuir copias literales de este
   documento como se recibe, en cualquier medio, siempre que la nota
   de derechos de reproducción y la nota de permiso se preserven, y
   que el distribuidor permita que el que la recibe hacer distribución
   posterior como lo permite esta nota.

   Se permite distribuir versiones modificadas de este documento, o
   porciones de este, bajo las condiciones anteriores, siempre que
   ellas tengan nota visible especificando quién fue el último en
   alterarlas.

Las condiciones para copiar Emacs mismo son más complejas, pero con el
mismo espíritu.  Por favor lea el archivo COPYING y luego distribuya
copias de GNU Emacs a sus amigos.  ¡Ayude a erradicar el
obstruccionismo del software ("propietariedad") usando, escribiendo, y
compartiendo software libre!

--- end of TUTORIAL.es ---

;;; Local Variables:
;;;   coding: latin-1
;;; End:

;;; arch-tag: 66aae86e-6f86-4a3e-b82a-44a783f774fd
