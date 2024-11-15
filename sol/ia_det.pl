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

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    puntaje(Dados, yahtzee, 50).

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    puntaje(Dados, large_straight, 40).

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    puntaje(Dados, small_straight, 30).

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    puntaje(Dados, full_house, 25).

cambio_dados(Dados, Tablero, ia_det, [1,1,1,1,1]) :-
    sin_repetidos(Dados).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    maximas_repeticiones(Dados, _, N),
    reemplazar(N, Dados, Patron).

prioridades([yahtzee, large_straight, small_straight, full_house, sixes, fives, fours, threes, twos, aces, four_of_a_kind, three_of_a_kind, chance]).

prioridades_rev([chance, three_of_a_kind, four_of_a_kind, aces, twos, threes, fours, fives, sixes, full_house, small_straight, large_straight, yahtzee]).

ocupada_categoria(_, [], 1).
ocupada_categoria(C, [s(C,nil)|_], 0).
ocupada_categoria(C, [s(C,P)|_], 1) :- P \= nil.
ocupada_categoria(C, [s(_,_)|Ts], X) :-
    ocupada_categoria(C, Ts, X).

eleccion_slot(Dados, Tablero, ia_det, yahtzee) :-
    puntaje(Dados, yahtzee, 50),
    ocupada_categoria(yahtzee, Tablero, 0).

eleccion_slot(Dados, Tablero, ia_det, large_straight) :-
    puntaje(Dados, large_straight, 40),
    ocupada_categoria(large_straight, Tablero, 0).

eleccion_slot(Dados, Tablero, ia_det, small_straight) :-
    puntaje(Dados, small_straight, 30),
    ocupada_categoria(small_straight, Tablero, 0).

eleccion_slot(Dados, Tablero, ia_det, full_house) :-
    puntaje(Dados, full_house, 25),
    ocupada_categoria(full_house, Tablero, 0).

uc([sixes, fives, fours, threes, twos, aces]).

uppercateando(_, _, [], []).
uppercateando(D, T, [X|Xs], [p(X, nil)|P]) :-
    ocupada_categoria(X, T, 1),
    uppercateando(D, T, Xs, P).
uppercateando(D, T, [X|Xs], [p(X, Y)|P]) :-
    puntaje(D, X, Y),
    uppercateando(D, T, Xs, P).

seleccion_upper(L, Y) :-
    seleccion_upper_acc(L, _, 0, Y).

seleccion_upper_acc([], Ac, _, Ac).
seleccion_upper_acc([p(_, nil)|Xs], Ac, Acc, Y) :-
    seleccion_upper_acc(Xs, Ac, Acc, Y).
seleccion_upper_acc([p(C, Cp)|Xs], _, Acc, Y) :-
    Cp >= Acc,
    seleccion_upper_acc(Xs, C, Cp, Y).
seleccion_upper_acc([p(C, Cp)|Xs], Ac, Acc, Y) :-
    seleccion_upper_acc(Xs, Ac, Acc, Y).

eleccion_slot(Dados, Tablero, ia_det, X) :-
    uc(UC),
    uppercateando(Dados, Tablero, UC, L),
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

eleccion_slot(Dados, Tablero, ia_det, chance) :-
    ocupada_categoria(chance, Tablero, 0).

eleccion_slot(Dados, Tablero, ia_det, X) :-
    prioridades_rev(Prioridades_rev),
    tomar_random_no_ocupada(Prioridades_rev, Tablero, X).

tomar_random_no_ocupada([], _, _).
tomar_random_no_ocupada([X|_], Tablero, X) :-
    ocupada_categoria(X, Tablero, 0).
tomar_random_no_ocupada([_|Xs], Tablero, X) :-
    tomar_random_no_ocupada(Xs, Tablero, X).

sin_repetidos([], _).
sin_repetidos([X|Xs], Y) :-
    \+ member(X, Y),
    sin_repetidos(Xs, [X|Y]).
sin_repetidos(L) :-
    sin_repetidos(L, []).
