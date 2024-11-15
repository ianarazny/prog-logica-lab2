mayor_sublista(Lista, Resultado) :-
    sort(Lista, ListaOrdenada),
    mayor_sublista_aux(ListaOrdenada, Resultado).
mayor_sublista_aux(Lista, Sublista) :-
    append(_, Sublista, Lista),   % Sublista es una parte de Lista
    length(Sublista, N),          % N es la longitud de Sublista
    not((append(_, OtraSublista, Lista),  % OtraSublista es otra parte de Lista
         length(OtraSublista, M),         % M es la longitud de OtraSublista
         M > N)).                        % La longitud de OtraSublista es mayor que la de Sublista

encontrar_tamano_hueco([X|Y], N) :-
    mayor_sublista([X|Y], R),
    espacio_mas_grande(R,0, N).

espacio_mas_grande([_|[]], Ac, Ac) :- !.
espacio_mas_grande([X,Y|Resto],Ac, Max) :-
    Ac1 is Y - X - 1,
    Ac1 > Ac,
    espacio_mas_grande([Y|Resto], Ac1, Max),!.
espacio_mas_grande([_,Y|Resto],Ac, Max) :-
    espacio_mas_grande([Y|Resto], Ac, Max),!.