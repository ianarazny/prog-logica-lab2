:- use_module(library(random)).
:- use_module(library(filesex)).


:- dynamic(probs/1). % Declara que variable_global puede ser dinámica.


% Predicado para inicializar la variable global con un valor inicial.
inicializar_variable_global() :-
    retractall(probs(_)), % Borra cualquier valor anterior.
    consultar_probabilidades(ListaValores), % Consulto las probs y la cargo en ListaValores
    assertz(probs(ListaValores)). % Establece el nuevo valor.


%%%%%%%%%%%%%%%%%%%%%%%%%%% INSTANCIACION PROBLOG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Invoco a Problog a partir de un modelo 
% Y consulto el resultado para obtener 
% las consultas y su probabilidad


consultar_probabilidades(ListaValores):-
    % Problog debe estar en el path!
    absolute_file_name(path(problog),Problog,[access(exist),extensions([exe])]),
    % Nombre del modelo, que se supone está en el mismo directorio que el fuente
    absolute_file_name(modelo_problog,Modelo,[file_type(prolog)]),
    % Invoca a problog con el modelo como argumento, y envía la salida a un pipe
    process_create(Problog,[Modelo],[stdout(pipe(In))]),
    % Convierte la salida a un string
    read_string(In,_,Result),
    % Divide la salida
    split_string(Result,"\n\t","\r ",L),
    % Quito último elemento de la lista
    append(L1,[_],L),
    lista_valores(L1,ListaValores).


% Predicado auxiliar para transformar a términos y a números, como se espera
lista_valores([X,Y|T],[TermValor|T1]):-
    % Saco los dos puntos del final
    split_string(X,"",":",[X1|_]),
    term_string(TermX,X1),
    parse_term(TermX, TermValor, Y),
    lista_valores(T,T1).
lista_valores([],[]).


% Un caso de separador de términos para cada caso:
% Caso 1: un parámetro (casos normales)
% Caso 2: dos parámetros (los de huecos que llevan dos)
% Caso 3: sin parámetros (los de yahtzee que no llevan)
parse_term(TermX, TermValor, Y):-
    TermX =.. [Cat,Valor],
    number_string(NumberY,Y),
    TermValor =.. [p,Cat,Valor,NumberY].
parse_term(TermX, TermValor, Y):-
    TermX =.. [Cat,Valor1, Valor2],
    number_string(NumberY,Y),
    TermValor =.. [p,Cat,Valor1, Valor2, NumberY].
parse_term(TermX, TermValor, Y):-
    TermX =.. [Cat],
    number_string(NumberY,Y),
    TermValor =.. [p,Cat,NumberY].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tablero_test([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0),
    s(large_straight,40), s(yahtzee,50),s(chance,10)]).


% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).


% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).


% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que habí­a; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).


% Lanza un dado
tiro_dado(X):-
    random(1,7,X).


% Inicializo el tablero a partir de la lista de categorías
tablero_inicial([],[]).
tablero_inicial([Cat|Cats],[s(Cat,nil)|T1]):-
        tablero_inicial(Cats,T1).


% Jugador yahtzee
% Jugador puede ser humano o ia
yahtzeelog(Estrategia,Seed):-
    inicializar_variable_global(),
    set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal).


% Esto es simplemente para no utilizar ronda1 como sinónimo de juego
partida(IA,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,IA,Tablero,TableroFinal).




% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.


ronda(NumRonda,IA,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,IA,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,IA,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,IA,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1,
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,IA,Tablero2,TableroSalida).


% Asigna un puntaje a una categoría y actualiza el tablero
ajustar_tablero([], _, _, []).
ajustar_tablero([s(C,_)|L], C, P, [s(C, P)|Ts]):-
    ajustar_tablero(L, C, P, Ts).
ajustar_tablero([s(C1,X)|L], C, P, [s(C1,X)|Ts]):-
    ajustar_tablero(L, C, P, Ts).


% Suma todos los puntos del tablero y los retorna en la variable Puntaje
puntaje_tablero([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13], Puntaje) :-
    suma([C1,C2,C3,C4,C5,C6], P1, 0),
    llevaBonus(P1, Bonus),
    suma([C7,C8,C9,C10,C11,C12,C13], P2, 0),
    Puntaje is P1 + Bonus + P2.


% Decide si asignar el bono de categorías superiores
llevaBonus(P, 35) :- P>=63, !.
llevaBonus(_, 0).


% Dada una lista suma sus valores
suma([], P, P).
suma([s(_, X)|Y], P, Ac) :-
    Ac1 is Ac+ X,
    suma(Y, P, Ac1).


buscar([], _, 0).
buscar([N|Y], N, C) :-
    buscar(Y, N, C1),
    C is C1 + 1.
buscar([_|Y], N, C) :-
    buscar(Y, N, C).


% Puntajes para cada categoría dada una configuración de dados
puntaje(Dados, aces, P):-
    buscar(Dados, 1, P1),
    P is P1*1.


puntaje(Dados, twos, P):-
    buscar(Dados, 2, P1),
    P is P1*2.


puntaje(Dados, threes, P):-
    buscar(Dados, 3, P1),
    P is P1*3.


puntaje(Dados, fours, P):-
 buscar(Dados, 4, P1),
 P is P1*4.


puntaje(Dados, fives, P):-
 buscar(Dados, 5, P1),
 P is P1*5.


puntaje(Dados, sixes, P):-
 buscar(Dados, 6, P1),
 P is P1*6.


puntaje(Dies, three_of_a_kind, P):-
    permutation(Dies, [X, X, X, _, _]),
 !,
 suma(Dies, P).


puntaje(_, three_of_a_kind, 0).


puntaje(Dies, four_of_a_kind, P):-
 permutation(Dies, [X, X, X, X, _]),
 !,
 suma(Dies, P).


puntaje(_, four_of_a_kind, 0).


puntaje(Dies, full_house, 25):-
    permutation(Dies, [X, X, X, Y, Y]),
    !,
    X \= Y.


puntaje(_, full_house, 0).


puntaje(Dies, small_straight, 30):-
    permutation(Dies, [1, 2, 3, 4, _]), !.


puntaje(Dies, small_straight, 30):-
    permutation(Dies, [2, 3, 4, 5, _]), !.


puntaje(Dies, small_straight, 30):-
    permutation(Dies, [3, 4, 5, 6, _]), !.


puntaje(_, small_straight, 0).


puntaje(Dies, large_straight, 40):-
    permutation(Dies, [1, 2, 3, 4, 5]), !.


puntaje(Dies, large_straight, 40):-
    permutation(Dies, [2, 3, 4, 5, 6]), !.


puntaje(_, large_straight, 0).


puntaje(Dies, yahtzee, 50):-
    permutation(Dies, [X, X, X, X, X]), !.


puntaje(_, yahtzee, 0).


puntaje(Dies, chance, Points):-
    suma(Dies, Points).


suma([], 0).
suma(L, P) :- sumaAux(L, 0, P).


sumaAux([], P, P).
sumaAux([X|Y], Ac, P) :-
    Ac1 is Ac + X,
    sumaAux(Y, Ac1, P).


%Busca una lista el valor que se repite más veces, y cuantas veces se repite
maximas_repeticiones(L, MC, MVP) :-
    findall(C-V, (between(1, 6, V), contar_ocurrencias(L, V, C)), Cs),
    miembro_mayor(Cs, MC-MVP).


% Función para contar el número de ocurrencias de un valor en una lista
contar_ocurrencias([], _, 0).
contar_ocurrencias([X|T], X, C) :-
    contar_ocurrencias(T, X, C1),
    C is C1 + 1.
contar_ocurrencias([Y|T], X, C) :-
    X \= Y,
    contar_ocurrencias(T, X, C).


% Predicado auxiliar para encontrar el máximo miembro en una lista de pares (Count-Value)
miembro_mayor([H|T], M) :-
    miembro_mayor(T, H, M).
miembro_mayor([], M, M).
miembro_mayor([C-V|T], C1-V1, M) :-
    cuenta_mayor(C-V, C1-V1, G),
    miembro_mayor(T, G, M).


% Predicado auxiliar para determinar cuál de los dos pares tiene el mayor Count
cuenta_mayor(C1-V1, C2-_, C1-V1) :-
    C1 > C2.
cuenta_mayor(C1-_, C2-V2, C2-V2) :-
    C1 =< C2.


% Función principal para reemplazar los elementos en la lista
reemplazar(_, [], []).
reemplazar(Value, [H|T], [0|T2]) :-
    H = Value,
    reemplazar(Value, T, T2).
reemplazar(Value, [H|T], [1|T2]) :-
    H \= Value,
    reemplazar(Value, T, T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cambio_dados(_, _, humano, Patron):-
    read(Patron).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]):-
    puntaje(Dados, yahtzee, 50),
 ocupada_categoria(yahtzee, Tablero, 0).


cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]):-
    puntaje(Dados, large_straight, 40),
 ocupada_categoria(large_straight, Tablero, 0).


cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]):-
    puntaje(Dados, small_straight, 30),
 ocupada_categoria(small_straight, Tablero, 0).


cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]):-
    puntaje(Dados, full_house, 25),
 ocupada_categoria(full_house, Tablero, 0).


cambio_dados(Dados, _, ia_det, [1,1,1,1,1]):-
    sin_repetidos(Dados).


cambio_dados(Dados, _, ia_det, Patron):-
    maximas_repeticiones(Dados, _, N),
    reemplazar(N, Dados, Patron).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cambio_dados(Dados, Tablero, ia_prob, [0,0,0,0,0]):-
    puntaje(Dados, yahtzee, 50),
 ocupada_categoria(yahtzee, Tablero, 0).


cambio_dados(Dados, Tablero, ia_prob, [0,0,0,0,0]):-
    puntaje(Dados, large_straight, 40),
 ocupada_categoria(large_straight, Tablero, 0).


cambio_dados(Dados, Tablero, ia_prob, [0,0,0,0,0]):-
    puntaje(Dados, small_straight, 30),
 ocupada_categoria(small_straight, Tablero, 0).


cambio_dados(Dados, Tablero, ia_prob, [0,0,0,0,0]):-
    puntaje(Dados, full_house, 25),
 ocupada_categoria(full_house, Tablero, 0).


cambio_dados(Dados, Tablero, ia_prob, Patron) :-
        prob(Dados, yahtzee, Tablero, Y1, P1),
        prob(Dados, large_straight, Tablero, Y2, P2),
        prob(Dados, small_straight, Tablero, Y3, P3),
        prob(Dados, full_house, Tablero, Y4, P4),
        max([d(yahtzee,Y1, P1), d(large_straight, Y2, P2), d(small_straight, Y3, P3), d(full_house, Y4, P4)], d(_, _, Patron)).


cambio_dados(Dados, _, ia_prob, [1,1,1,1,1]):-
    sin_repetidos(Dados).


cambio_dados(Dados, _, ia_prob, Patron):-
    maximas_repeticiones(Dados, _, N),
    reemplazar(N, Dados, Patron).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Predicado para determinar si una categoría ya tiene un puntaje asignado
%ocupada_categoria(_, [], 0) .
ocupada_categoria(C, [s(C,nil)|_], 0) :- !.
ocupada_categoria(C, [s(C,P)|_], 1):- P \= nil, !.
ocupada_categoria(C, [s(_,_)|Ts], X):-
    ocupada_categoria(C, Ts, X).


% Función para contar la cantidad de cada número en los dados
contar([], _, 0).
contar([X|Xs], X, N) :-
    contar(Xs, X, N1),
    N is N1 + 1.
contar([Y|Xs], X, N) :-
    X \= Y,
    contar(Xs, X, N).


% Función principal para encontrar la mejor categorí­a superior
mejor_categoria_superior(Dados, Tablero, Categoria) :-
    findall(N-C, (member(N, [1,2,3,4,5,6]), contar(Dados, N, C), C > 1), Repetidos),
    sort(2, @>=, Repetidos, Ordenados),
    manejar_ordenados(Ordenados, Tablero, Categoria).


% Manejar la lista de números repetidos ordenados
manejar_ordenados([Num-_|_], Tablero, Categoria) :-
    categoria_superior(Num, Categoria),
    ocupada_categoria(Categoria, Tablero, 0),
    !.
manejar_ordenados(_, Tablero, Categoria) :-
    findall(C, (member(N, [1,2,3,4,5,6]), categoria_superior(N, C), ocupada_categoria(C, Tablero, 0)), Categorias),
    min_member(Categoria, Categorias).


% Mapeo de números a categorías superiores
categoria_superior(1, aces).
categoria_superior(2, twos).
categoria_superior(3, threes).
categoria_superior(4, fours).
categoria_superior(5, fives).
categoria_superior(6, sixes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eleccion_slot(_, _, humano, Slot):-
    read(Slot).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Eleccion de categoría para la ia_det, dada la configuración de los dados y el tablero, en orden de prioridad
eleccion_slot(Dados, Tablero, ia_det, yahtzee):-
    puntaje(Dados, yahtzee, 50),
    ocupada_categoria(yahtzee, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_det, large_straight):-
    puntaje(Dados, large_straight, 40),
    ocupada_categoria(large_straight, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_det, small_straight):-
    puntaje(Dados, small_straight, 30),
    ocupada_categoria(small_straight, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_det, full_house):-
    puntaje(Dados, full_house, 25),
    ocupada_categoria(full_house, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_det, Categoria):-
    mejor_categoria_superior(Dados, Tablero, Categoria).


eleccion_slot(Dados, Tablero, ia_det, four_of_a_kind) :-
    puntaje(Dados, four_of_a_kind, X),
    X > 0,
    ocupada_categoria(four_of_a_kind, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_det, three_of_a_kind) :-
    puntaje(Dados, three_of_a_kind, X),
    X > 0,
    ocupada_categoria(three_of_a_kind, Tablero, 0).


eleccion_slot(_, Tablero, ia_det, chance) :-
    ocupada_categoria(chance, Tablero, 0).


eleccion_slot(_, Tablero, ia_det, X) :-
    tomar_random_no_ocupada([chance, three_of_a_kind, four_of_a_kind, aces, twos, threes, fours, fives, sixes, full_house, small_straight, large_straight, yahtzee], Tablero, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Eleccion de categoría para la ia_prob, dada la configuración de los dados y el tablero, en orden de prioridad
eleccion_slot(Dados, Tablero, ia_prob, yahtzee):-
    puntaje(Dados, yahtzee, 50),
    ocupada_categoria(yahtzee, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_prob, large_straight):-
    puntaje(Dados, large_straight, 40),
    ocupada_categoria(large_straight, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_prob, small_straight):-
    puntaje(Dados, small_straight, 30),
    ocupada_categoria(small_straight, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_prob, full_house):-
    puntaje(Dados, full_house, 25),
    ocupada_categoria(full_house, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_prob, Categoria):-
    mejor_categoria_superior(Dados, Tablero, Categoria).


eleccion_slot(Dados, Tablero, ia_prob, four_of_a_kind) :-
    puntaje(Dados, four_of_a_kind, X),
    X > 0,
    ocupada_categoria(four_of_a_kind, Tablero, 0).


eleccion_slot(Dados, Tablero, ia_prob, three_of_a_kind) :-
    puntaje(Dados, three_of_a_kind, X),
    X > 0,
    ocupada_categoria(three_of_a_kind, Tablero, 0).


eleccion_slot(_, Tablero, ia_prob, chance) :-
    ocupada_categoria(chance, Tablero, 0).


eleccion_slot(_, Tablero, ia_prob, X) :-
    tomar_random_no_ocupada([chance, three_of_a_kind, four_of_a_kind, aces, twos, threes, fours, fives, sixes, full_house, small_straight, large_straight, yahtzee], Tablero, X).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Toma una categoría aleatoria sin ocupar
tomar_random_no_ocupada([], _, _).
tomar_random_no_ocupada([X|_], Tablero, X) :-
    ocupada_categoria(X, Tablero, 0), !.
tomar_random_no_ocupada([_|Xs], Tablero, X) :-
    tomar_random_no_ocupada(Xs, Tablero, X), !.


% Predicado que determina si una lista no tiene dados repetidos
sin_repetidos([], _).
sin_repetidos([X|Xs], Y) :-
    \+ member(X, Y),
    sin_repetidos(Xs, [X|Y]).
sin_repetidos(L) :-
    sin_repetidos(L, []).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Retorna la mayor sublista dentro de una lista
mayor_sublista(Lista, Resultado) :-
    sort(Lista, ListaOrdenada),
    mayor_sublista_aux(ListaOrdenada, Resultado).
mayor_sublista_aux(Lista, Sublista) :-
    append(_, Sublista, Lista),   % Sublista es una parte de Lista
    length(Sublista, N),          % N es la longitud de Sublista
    not((append(_, OtraSublista, Lista),  % OtraSublista es otra parte de Lista
         length(OtraSublista, M),         % M es la longitud de OtraSublista
         M > N)).                        % La longitud de OtraSublista es mayor que la de Sublista


% Busca un hueco en una escalera y su tamaño
encontrar_tamano_hueco([X|Y], R, N) :-
    mayor_sublista([X|Y], R),
    espacio_mas_grande(R,0, N).

% Encuentra el espacio más grande entre dos números consecutivos
espacio_mas_grande([_|[]], Ac, Ac) :- !.
espacio_mas_grande([X,Y|Resto],Ac, Max) :-
    Ac1 is Y - X - 1,
    Ac1 > Ac,
    espacio_mas_grande([Y|Resto], Ac1, Max),!.
espacio_mas_grande([_,Y|Resto],Ac, Max) :-
    espacio_mas_grande([Y|Resto], Ac, Max),!.


% Encuentra una escalera en una lista y devuelve esa escalera, su tamaño y su último valor en orden de menor a mayor
encontrar_escalera(Lista, Sublista, Tamano, Ultimo) :-
    sort(Lista, Lis),
    findall(A, (append(A, _, Lis), consecutivos(A)), Listas1),
    findall(B, (append(_, B, Lis), consecutivos(B)), Listas2),
    append(Listas1, Listas2, Listas),
    lista_mas_larga(Listas, Sublista),
    length(Sublista, Tamano),
    append(_, Ul, Lis), 
    length(Ul, 1), 
    primero(Ul, Ultimo), !.       


primero([], []).
primero([X|_], X).


% Dada una lista devuelve su subsecuencia más larga
lista_mas_larga([L|Ls], Subsecuencia) :-
    lista_mas_larga_ac([L|Ls], 0, [], Subsecuencia).


lista_mas_larga_ac([], _, AcL, AcL).
lista_mas_larga_ac([L|Ls], Ac, _, S) :-
    length(L, Ac1),
    Ac1 > Ac,
    lista_mas_larga_ac(Ls, Ac1, L, S).
lista_mas_larga_ac([L|Ls], Ac, AcL, S):-
    length(L, Ac),
    suma(L, Sl),
    suma(AcL, Sacl),
    Sl > Sacl,
    lista_mas_larga_ac(Ls, Ac, L, S).
lista_mas_larga_ac([_|Ls], Ac, AcL, S) :-
    lista_mas_larga_ac(Ls, Ac, AcL, S).


% Determina si los números de una lista son consecutivos
consecutivos([_]).
consecutivos([X,Y|Resto]) :-          
    Y is X + 1,
    consecutivos([Y|Resto]).


% Calcula el patrón de relanzamiento para una escalera
calculoPatron([], _, []).
calculoPatron([L|Ls], Escalera, [0|Z]):-
    find_in_list(L, Escalera),
    remove(Escalera, L, Escalera2),
    calculoPatron(Ls, Escalera2, Z), !.
calculoPatron([_|Ls], Escalera, [1|Z]):-
    calculoPatron(Ls, Escalera, Z), !.


% Predicado auxiliar para buscar un valor en una lista
find_in_list(X, [X|_]).
find_in_list(X, [_|T]) :-
    find_in_list(X, T).


% Remueve un valor de una lista
remove([], _, []).
remove([X|T], X, R) :-
    remove(T, 0, R).
remove([H|T], X, [H|R]) :-
    remove(T, X, R).

hacer_lista(0, _, []).
hacer_lista(Cant, Num, [Num|Xs]) :-
    Cant > 0,
    Cant1 is Cant - 1,
    hacer_lista(Cant1, Num, Xs).


% Funciones auxiliares para la búsqueda de probabilidades dependiendo de la cantidad de parámetros necesarios
find_aux(Cat, P) :-
    probs(Probs),
    find2(Probs, Cat, P).
find_aux(Cat, X, P) :-
    probs(Probs),
    find3(Probs, Cat, X, P).
find_aux(Cat, X, Y, P) :-
    probs(Probs),
    find4(Probs, Cat, X, Y, P).


find2([p(Cat,P) | _], Cat, P).
find2([_|Probs], Cat, P) :- find2(Probs, Cat, P).


find3([p(Cat, X, P)|_], Cat, X, P).
find3([_ | Probs], Cat, X, P) :- find3(Probs, Cat, X, P).


find4([p(Cat, X, Y, P)|_], Cat, X, Y, P).
find4([_ | Probs], Cat, X, Y, P) :- find4(Probs, Cat, X, Y, P).

% Devuelve el máximo de una lista de elementos d(_,_,_) en el tercer argumento
max(L, R) :-
    max_ac(L, d(_,0,_), R), !.


max_ac([], Ac, Ac).
max_ac([d(C, P, Pat)|Xs], d(_,Ac,_), R) :-
    P > Ac,
    max_ac(Xs, d(C,P,Pat), R).
max_ac([_|Xs], Ac, R) :-
    max_ac(Xs, Ac, R).

% Dado que ninguna escalera si puede formar del 1 al 6, en caso que esten ambos se saca el 1.
sacarCosas(L, L) :-
    not(member(1, L)).
sacarCosas(L, L) :-
    not(member(6, L)).
sacarCosas(L, R) :-
    member(1, L),
    member(6, L),
    select(L, 1, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%prob(_, yahtzee, Tablero, 0, [1,1,1,1,1]) :- ocupada_categoria(yahtzee, Tablero, 1), !.    // Esta opcion la comentamos para que por mas que ya se haya tomado el yahtzee aún sea posible utilizar este cambio de dados para las upper categories
prob(Dados, yahtzee, _, P, Patron) :- maximas_repeticiones(Dados, 4, Elem), !, find_aux(yahtzee_with_4, P), hacer_lista(4, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).
prob(Dados, yahtzee, _, P, Patron) :- maximas_repeticiones(Dados, 3, Elem), !, find_aux(yahtzee_with_3, P), hacer_lista(3, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).
prob(Dados, yahtzee, _, P, Patron) :- maximas_repeticiones(Dados, 2, Elem), !, find_aux(yahtzee_with_2, P), hacer_lista(2, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).
prob(Dados, yahtzee, _, P, Patron) :- maximas_repeticiones(Dados, 1, Elem), !, find_aux(yahtzee_with_1, P), hacer_lista(1, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).


prob(_, full_house, Tablero, 0, [1,1,1,1,1]) :- ocupada_categoria(full_house, Tablero, 1), !.
prob(Dados, full_house, _, P, Patron) :- maximas_repeticiones(Dados, 3, Elem), !, find_aux(full_house_with_3,1, P), hacer_lista(3, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).
prob(Dados, full_house, _, P, Patron) :- maximas_repeticiones(Dados, 2, Elem), !, find_aux(full_house_with_2,1, P), hacer_lista(2, Elem, Sublista), calculoPatron(Dados, Sublista, Patron).
prob(_, full_house, _, 0, [1,1,1,1,1]).


prob(_, large_straight, Tablero, 0, [1,1,1,1,1]) :- ocupada_categoria(large_straight, Tablero, 1), !.
prob(Dados, large_straight, _, P, Patron) :- encontrar_tamano_hueco(Dados, Sublista, 2), find_aux(large_straight_h_2,1,5, P), sacarCosas(Sublista, Sublista1), calculoPatron(Dados, Sublista1, Patron).
prob(Dados, large_straight, _, P, Patron) :- encontrar_tamano_hueco(Dados, Sublista, 1), find_aux(large_straight_h_1,1,5, P), sacarCosas(Sublista, Sublista1), calculoPatron(Dados, Sublista1, Patron).
prob(Dados, large_straight, _, P, Patron) :- encontrar_escalera(Dados, Sublista, X, Y), diccionario(X, Y, P), calculoPatron(Dados, Sublista, Patron).


prob(_, small_straight, Tablero, 0, [1,1,1,1,1]) :- ocupada_categoria(small_straight, Tablero, 1), !.
prob(Dados, small_straight, _, P, Patron) :- encontrar_tamano_hueco(Dados, Sublista, 2), find_aux(small_straight_h_2,1,4, P), sacarCosas(Sublista, Sublista1), calculoPatron(Dados, Sublista1, Patron).
prob(Dados, small_straight, _, P, Patron) :- encontrar_tamano_hueco(Dados, Sublista, 1), find_aux(small_straight_h_1,1,4, P), sacarCosas(Sublista, Sublista1), calculoPatron(Dados, Sublista1, Patron).
prob(Dados, small_straight, _, P, Patron) :- encontrar_escalera(Dados, Sublista, X, Y), diccionario1(X, Y, P), calculoPatron(Dados, Sublista, Patron).

%diccionario para las probabilidades de las escaleras

diccionario(2, 2, P):- find_aux(large_straight_2,2, P).
diccionario(2, 3, P):- find_aux(large_straight_2,3, P).
diccionario(2, 4, P):- find_aux(large_straight_2,4, P).
diccionario(2, 5, P):- find_aux(large_straight_2,5, P).
diccionario(2, 6, P):- find_aux(large_straight_2,6, P).
diccionario(3, 2, P):- find_aux(large_straight_3,2, P).
diccionario(3, 3, P):- find_aux(large_straight_3,3, P).
diccionario(3, 4, P):- find_aux(large_straight_3,4, P).
diccionario(3, 5, P):- find_aux(large_straight_3,5, P).
diccionario(3, 6, P):- find_aux(large_straight_3,6, P).
diccionario(4, 2, P):- find_aux(large_straight_4,2, P).
diccionario(4, 3, P):- find_aux(large_straight_4,3, P).
diccionario(4, 4, P):- find_aux(large_straight_4,4, P).
diccionario(4, 5, P):- find_aux(large_straight_4,5, P).
diccionario(4, 6, P):- find_aux(large_straight_4,6, P).


diccionario1(2, 2, P):- find_aux(small_straight_2,2, P).
diccionario1(2, 3, P):- find_aux(small_straight_2,3, P).
diccionario1(2, 4, P):- find_aux(small_straight_2,4, P).
diccionario1(2, 5, P):- find_aux(small_straight_2,5, P).
diccionario1(2, 6, P):- find_aux(small_straight_2,6, P).
diccionario1(3, 3, P):- find_aux(small_straight_3,3, P).
diccionario1(3, 4, P):- find_aux(small_straight_3,4, P).
diccionario1(3, 5, P):- find_aux(small_straight_3,5, P).
diccionario1(3, 6, P):- find_aux(small_straight_3,6, P).