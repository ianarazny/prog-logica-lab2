:- use_module(library(random)).
:- use_module(library(filesex)).

tablero_test([s(aces,4), s(twos,2),s(threes,15), s(fours,16), s(fives,10), s(sixes,18),
    s(three_of_a_kind,20), s(four_of_a_kind,22), s(full_house,0), s(small_straight,0),
    s(large_straight,40), s(yahtzee,50),s(chance,10)]).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
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
yahtzeelog(Estrategia,_):-
    %set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal).

% Esto es simplemente para no utilizar ronda1 como sinónimo de juego
partida(ia_det,TableroFinal):-
    categorias(C),
    tablero_inicial(C,Tablero),
    ronda(1,ia_det,Tablero,TableroFinal).

% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.

ronda(NumRonda,ia_det,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,ia_det,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,ia_det,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,ia_det,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1,
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,ia_det,Tablero2,TableroSalida).

%cambio_dados(_, _, humano, Patron):-
% read(Patron).

%eleccion_slot(_, _, humano, Slot):-
% read(Slot).

ajustar_tablero([], _, _, []).
ajustar_tablero([s(C,_)|L], C, P, [s(C, P)|Ts]):-
    ajustar_tablero(L, C, P, Ts).
ajustar_tablero([s(C1,X)|L], C, P, [s(C1,X)|Ts]):-
    ajustar_tablero(L, C, P, Ts).

puntaje_tablero([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13], Puntaje) :-
suma([C1,C2,C3,C4,C5,C6], P1, 0),
llevaBonus(P1, Bonus),
suma([C7,C8,C9,C10,C11,C12,C13], P2, 0),
Puntaje is P1 + Bonus + P2.

llevaBonus(P, 35) :- P>=63, !.
llevaBonus(_, 0).

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

%-----------------------------------------------------------------------

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

%ocupada_categoria(_, [], 0) .
ocupada_categoria(C, [s(C,nil)|_], 0) :- !.
ocupada_categoria(C, [s(C,P)|_], 1):- P \= nil, !.
ocupada_categoria(C, [s(_,_)|Ts], X):-
    ocupada_categoria(C, Ts, X).

uppercateando(_, _, [], []).
uppercateando(D, T, [X|Xs], [p(X, nil)|P]) :-
ocupada_categoria(X, T, 1),
 uppercateando(D, T, Xs, P),!.
uppercateando(D, T, [X|Xs], [p(X, Y)|P]) :-
puntaje(D, X, Y),
uppercateando(D, T, Xs, P),!.

seleccion_upper(L, Y) :-
seleccion_upper_acc(L, _, 0, Y).

seleccion_upper_acc([], Ac, _, Ac).
seleccion_upper_acc([p(_, nil)|Xs], Ac, Acc, Y) :-
seleccion_upper_acc(Xs, Ac, Acc, Y), !.
seleccion_upper_acc([p(C, Cp)|Xs], _, Acc, Y) :-
Cp >= Acc,
seleccion_upper_acc(Xs, C, Cp, Y).
seleccion_upper_acc([p(_, _)|Xs], Ac, Acc, Y) :-
seleccion_upper_acc(Xs, Ac, Acc, Y).

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

eleccion_slot(Dados, Tablero, ia_det, X):-
uppercateando(Dados, Tablero, [sixes, fives, fours, threes, twos, aces], L),
seleccion_upper(L, X),
nonvar(X).

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

tomar_random_no_ocupada([], _, _).
tomar_random_no_ocupada([X|_], Tablero, X) :-
ocupada_categoria(X, Tablero, 0), !.
tomar_random_no_ocupada([_|Xs], Tablero, X) :-
tomar_random_no_ocupada(Xs, Tablero, X), !.

sin_repetidos([], _).
sin_repetidos([X|Xs], Y) :-
    \+ member(X, Y),
    sin_repetidos(Xs, [X|Y]).
sin_repetidos(L) :-
    sin_repetidos(L, []).