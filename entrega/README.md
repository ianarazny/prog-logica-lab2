El laboratorio contiene dos archivos:
* modelo_problog.pl, que contiene el modelo probabilístico con los cálculos para distintos tipos de jugadas y finaliza con una query
* lab2.pl, que contiene la invocación al modelo de problog y la lógica del juego


Para la correcta ejecución de este programa, es necesario tener en el mismo directorio los archivos lab2.pl y modelo_problog.pl.
El predicado para jugar es yahtzeelog(Estrategia, Seed), donde Estrategia es un valor entre humano, ia_det e ia_prob, y Seed es un número.


A la hora de jugar con la Estrategia humano, se deberá luego del primer y segundo lanzamiento de dados dar como input un patrón de relanzamiento de dados con el formato [P1, P2, P3, P4, P5], donde cada valor corresponde con el dado de su respectivo lugar, y estos deben ser 0 o 1, 0 si se quiere mantener el dado y 1 si se quiere volver a lanzar.
Luego del tercer lanzamiento, el jugador se deberá decidir por una categoría para asignar los dados resultantes, por lo que deberá ingresar como input una de las siguientes categorías:
yahtzee, large_straight, small_straight, full_house, four_of_a_kind, three_of_a_kind, chance, aces, twos, threes, fourths, fives, sixes.