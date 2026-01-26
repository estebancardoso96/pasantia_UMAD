# Pasantia UMAD

## Objetivos al inicio de la Pasantía

Desarrollar un visualizador de datos interactivo para analizar trayectorias políticas en el Uruguay y que responda preguntas sustantivas sobre representación de género, edad, secuencias de cargos y diferencias partidarias, dirigido a investigadores, periodistas y ciudadanía en general.

## Relato del proceso de Pasantía

En el apartado descriptivo del informe se relata el trabajo realizado en la pasantía que se estructura en dos etapas interconectadas y dependientes entre sí, con estos objetivos:

1) Limpieza, estandarización, modelado, agregación de variables e integración de nuevas filas a la base de datos de candidaturas y cargos ocupados por políticos y políticas de la UMAD.
2) Construcción y diseño del visualizador de políticos y políticas del Uruguay.

Si bien los procesos se relatan como dos etapas distintas, en el curso de la pasantía ambas tareas estuvieron interconectadas, pivotando de una a otra dada la dependencia de la base de datos con el visualizador y viceversa.

### Primera etapa: Base de datos

La base de datos utilizada es la contenida en el paquete puy del programa R, mantenido y desarrollado por Nicolás Schmidt, Daniel Chasquetti, Antonio Cardarello, entre otros investigadores de la UMAD (Unidad de Métodos y Acceso a Datos de la Facultad de Ciencias Sociales). La base contiene datos de candidaturas y cargos que ocuparon políticos desde 1830 hasta la actualidad. Cada fila en la base representa un cargo o una candidatura de un político en un período de tiempo con un inicio y un final dentro de una legislatura. Las variables originales de la base son: el nombre y apellido del político (en un solo campo), la candidatura o el cargo ocupado, el partido político, el inicio y fin, la legislatura, el estatus (si fue suplente o titular) y la circunscripción del cargo (en caso que corresponda).

La primera decisión a tomar fue la de que corte temporal elegir para el visualizador. Como se mencionó la base contiene datos desde 1830 hasta 2025, un período muy extenso y con mayor cantidad de valores nulos cuanto más cerca del origen. Por tanto, se decidió tomar con fecha inicial a la primera Legislatura que da inicio en el siglo XX: la Legislatura número 21. Luego del corte temporal, se procedió a realizar una limpieza inicial básica de la base como la eliminación de los espacios a los costados y la corrección de algunos valores, por ejemplo:

|**partido**  |
|-------------|
|Frente Ampilio|
|Frente Amplio|

Otros casos eran de correcciones más puntuales y que se fueron dando a medida que entendía mejor la base y los datos allí expuestos. Un ejemplo de ello es el siguiente caso en que las fechas de inicio no coinciden con el número legislatura:

| político             | partido          | fecha_inicio | fecha_fin  | legislatura |
| -------------------- | ---------------- | ------------ | ---------- | ----------- |
| CZARBIEVICZ, Abraham | Partido Colorado | 1987-10-01   | 1987-11-01 | 43          |
| CZARBIEVICZ, Abraham | Partido Colorado | 1987-10-01   | 1987-11-01 | 38          |
| CZARBIEVICZ, Abraham | Partido Colorado | 1987-10-01   | 1987-11-01 | 34          |

Con el corte temporal definido y con una estandarización y limpieza básica, el primer desafío fue generar un identificador único de político, una variable indispensable para poder realizar análisis de trayectorias individuales o de frecuencias. La variable “político” (que indica el nombre completo del político) por sí misma no podía ser un identificador único dado que un mismo político podía tener su nombre escrito de diversas formas en cada uno de los cargos o candidaturas. Por ejemplo, el caso de José Mujica:

| político             | partido       | fecha_inicio | fecha_fin  | legislatura | cargo                      | condición |
| -------------------- | ------------- | ------------ | ---------- | ----------- | -------------------------- | --------- |
| MUJICA CORDANO, José | Frente Amplio | 2010-03-01   | 2015-03-01 | 47          | Presidente de la República | Titular   |
| MUJICA, José         | Frente Amplio | 2009-10-25   | 2009-10-25 | 46          | Candidato Presidente       |           |

Para generar el identificador único de político se pensaron varias estrategias y se aplicaron para ver cual era la más efectiva. En principio la idea fue generar un matcheo probabilístico por la variable político, agrupando por nombres parecidos, pero los resultados no fueron los mejores dado que no identificaba al político cuando en una fila estaba y en la otra no el segundo apellido. Aunque si era exitosos en los pocos casos que el apellido del político se diferenciaba por una o dos letras en ambas filas, por ejemplo en casos de apellidos largos o no simples de escribir:

| político             | partido       | fecha_inicio | legislatura | cargo    | condición |
| -------------------- | ------------- | ------------ | ----------- | -------- | --------- |
| LUSTEMBERG, Cristina | Frente Amplio | 2015-02-15   | 48          | Diputado | Titular   |
| LUSTENBERG, Cristina | Frente Amplio | 2020-02-15   | 49          | Diputado | Titular   |

La segunda estrategia fue separar la variable político en 4 campos: Primer Nombre, Segundo Nombre, Primer Apellido y Segundo Apellido. Antes de ello, hubo que indicarle al programa R que algunos apellidos separados por espacios debía de colocarlos en un campo solo: los apellidos compuestos (Da Rosa, Da Silva, De Herrera, etc). El matcheo en este paso fue determinístico, buscando la coincidencia total de dos o más filas en las columnas primer nombre, primer apellido y partido político. Las filas matcheadas correctamente fueron mayores que utilizando la primera estrategia, pero aún así había muchos cambios que ajustar. Surgieron casos matcheados erróneamente de dos tipos: los falsos positivos y los falsos negativos. Los falsos positivos son casos de políticos distintos pero unidos con el mismo id, estos casos se dan dentro del mismo partido cuando políticos distintos comparten el mismo nombre y apellido, por ejemplo: Luis Hierro López y Luis Hierro Gambardella del partido Colorado. Por otra parte, los falsos negativos eran casos que siendo la misma persona, el código les asignaba diferente id. Esto se daba dado que en la base existen políticos que ocuparon cargos o candidaturas en más de un partido, por lo que el código les asignaba un id distinto al no matchear filas distintas por partido. Estos casos fueron solucionados mediante distintos matcheos probabilísticos no tomando en cuenta al partido político y dando más peso al primer apellido. La minoría de los casos fueron solucionados con una revisión manual para identificar la correcta asignación.
La segunda variable que se agregó a la base fue la fecha de nacimiento para poder calcular edades a la hora de asumir distintos cargos o candidaturas. Este proceso en un principio fue manual, teniendo que realizar búsquedas en sitios confiables como Wikipedia o en la base de https://autores.uy/ que fue muy útil para los legisladores de principio del Siglo XX, dado que muchos eran académicos o autores y por tanto, se encontraban en la base con sus respectivas publicaciones. En el proceso de la construcción de esta variable encontré la página https://biblioteca.parlamento.gub.uy:8008/biografias/busqueda que fue de gran ayuda para llenar fechas de nacimiento faltantes. Pero fue de mayor ayuda aún cuando identifiqué que la base de la UMAD tenía errores en los legisladores de las legislaturas más recientes. Uno de esos errores es el de colocar a todos los legisladores como titulares en la legislatura 47. 

| legislatura | cantidad |
| ----------- | -------- |
| 49          | 128      |
| 48          | 136      |
| 47          | 159      |
| 46          | 129      |

Otro error fue el de encontrar legisladores (como Manuela Mutti) en legislaturas que no actuaron como la 47.

| político                 | partido       | fecha_inicio | fecha_fin | legislatura | cargo    | status  |
| ------------------------ | ------------- | ------------ | --------- | ----------- | -------- | ------- |
| MUTTI FORNAROLI, Manuela | Frente Amplio | 2015-02-15   |           | 48          | Diputado | Titular |
| MUTTI FORNAROLI, Manuela | Frente Amplio |              |           | 47          | Diputado | Titular |

Luego de estas constataciones, decidí aprovechar la completitud de los datos de la página de biografías del parlamento y armé un script para obtener los datos mediante web scraping. Agregando esta nueva fuente se pudieron corregir los errores y además obtener muchos legisladores suplentes que no aparecían en la base de la UMAD. 

### Segunda etapa: Diseño y construcción del visualizador

El visualizador está estructurado en 5 pestañas donde se muestran los datos de la base en diferentes formatos. La primera pestaña le permite al usuario realizar una búsqueda dentro de la base y con diferentes filtros desplegables: por legislatura, por partido y por cargo (o candidatura). Colocar esta pestaña al principio me pareció la mejor forma para el primer acercamiento del usuario, para que pueda buscar libremente y mediante filtros que le permitan conocer los principales aspectos de los datos.
La segunda pestaña llamada Tablas intenta dar cuenta uno de los objetivos que se explicitan en la pasantía: la representación de las mujeres en la política, desde aspectos cuantitativos, como saber en qué legislaturas ha habido más mujeres en cargos, hasta cualitativos, como conocer en qué cargos la distribución ha sido más pareja o dispareja. 
La tercera pestaña Métricas también da cuenta de otro objetivo del visualizador: una aproximación sociodemográfica de los políticos, intentando ver si existe una relación entre el cargo y la edad al asumirlo. También ver la “juventud” o “vejez” de los legisladores de los partidos en las distintas legislaturas.
La cuarta pestaña despliega datos curiosos como los políticos de menor y mayor edad en asumir cada cargo y también, en una aproximación del análisis de trayectorias, muestra los legisladores más exitosos, es decir, aquellos que ocuparon el cargo (tanto de Senador como de Diputado) más veces en legislaturas diferentes.
Por último, la quinta pestaña intenta mostrar los mismos datos de las anteriores pestañas pero gráficamente y de forma interactiva, para que el usuario pueda aplicar los filtros que quiera y realizar clicks para sobre las barras o puntos para obtener información absoluta y relativa del fenómeno graficado.
El visualizador ofrece un plus al usuario, ya que le permite descargar varias de las tablas mostradas en formato csv e incluso la posibilidad de descargar la base completa con todos los microdatos.
Modelado de los datos

Es importante contar que todo este proceso que implicó la estandarización, la corrección pero también la agregación e integración de tablas está organizado y modelado en SQL, lo que permitió una mejor organización de los datos y una forma eficiente de realizar consultas. Este modelado, a su vez, está pensado para futuras actualizaciones de datos, tanto de siguientes legislaturas como agregados retroactivos. Se pensó en un modelado “estrella”, con una tabla de hechos principal y varias tablas pequeñas que la rodean.

