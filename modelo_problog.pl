1/6::dado1(X) :- between(1, 6, X).
1/6::dado2(X) :- between(1, 6, X).
1/6::dado3(X) :- between(1, 6, X).
1/6::dado4(X) :- between(1, 6, X).
1/6::dado5(X) :- between(1, 6, X).

yahtzee_with_4 :- dado5(X).
yahtzee_with_3 :- dado4(X),dado5(X).
yahtzee_with_2 :- dado3(X),dado4(X),dado5(X).
yahtzee_with_1 :- dado2(X),dado3(X),dado4(X),dado5(X).

full_house_with_3(X) :- dado1(X), dado2(X), dado3(X), dado4(Y), dado5(Y), X \= Y.
full_house_with_2(X) :- dado1(X), dado2(X), dado3(Y), dado4(Y), dado5(Y), X \= Y.
full_house_with_2(X) :- dado1(X), dado2(X), dado3(X), dado4(Y), dado5(Y), X \= Y. % Caso en el que uno de los dados coincide con los (2) que ya tengo, tambien messi.

% para no hacerlo mas complicado decidimos rollear siempre el 5to dado, sabemos que tendriamos mas probs a favor.
% Consideramos el caso de que small_straight_1 poco interesante.
%small_straight_1(X) :- dado3(Y), dado4(W), dado5(Z), X is Y + 1, Y is W + 1, W is Z + 1, X \= 0, X \= 7.
%small_straight_1(X) :- dado3(Y), dado4(W), dado5(Z), X is Y - 1, Y is W - 1, W is Z - 1, X \= 0, X \= 7. 

small_straight_2(X) :- dado3(Y), dado4(W), X is Y + 2, Y is W + 1, X > 3, X < 7.
small_straight_2(X) :- dado3(Y), dado4(W), X is Y + 2, Y is W - 3, X > 2, X < 7.
small_straight_2(X) :- dado3(Y), dado4(W), X is Y - 1, Y is W - 1, X > 1, X < 7.

small_straight_3(X) :- dado3(Y), X is Y + 3, X > 3, X < 7.
small_straight_3(X) :- dado3(Y), X is Y - 1, X > 2, X < 7.

small_straight_h_1(X,Y) :- dado3(Z), X is Z - 1, Y is Z + 2, X > 0, X < 7, Y > X + 2, Y < 7.
small_straight_h_1(X,Y) :- dado3(Z), X is Z - 1, Y is Z + 1, X > 1, X < 7, Y > X + 1, Y < 7.
small_straight_h_2(X,Y) :- dado3(Z), dado4(W), X is Z - 1, Z is W - 1, Y is W + 1, X > 0, X < 7, Y > X + 2, Y < 7.

%large_straight_1(X) :- dado2(Y), dado3(W), dado4(Z), dado5(R), X is Y + 1, Y is W + 1, W is Z + 1, Z is R + 1, X \= 0, X \= 7.
%large_straight_1(X) :- dado2(Y), dado3(W), dado4(Z), dado5(R), X is Y - 1, Y is W - 1, W is Z - 1, Z is R - 1, X \= 0, X \= 7.
%large_straight_1(X) :- dado2(Y), dado3(W), dado4(Z), dado5(R), X is W + 1, W is Y + 1, X is Z - 1, Z is R - 1, X \= 0, X \= 7.
% Consideramos que antes de buscar una escalera teniendo solamente un numero que pertencería podria buscarse otra cosa mas interesante.

large_straight_2(X) :- dado3(W), dado4(Z), dado5(R), X is W + 4, W is Z - 1, Z is R - 1, X > 4, X < 7.
large_straight_2(X) :- dado3(W), dado4(Z), dado5(R), X is W - 1, W is Z - 1, Z is R - 1, X > 1, X < 7.
large_straight_2(X) :- dado3(W), dado4(Z), dado5(R), X is W + 2, W is Z - 3, Z is R - 1, X > 2, X < 7.
large_straight_2(X) :- dado3(W), dado4(Z), dado5(R), X is W + 3, W is Z - 1, Z is R - 3, X > 3, X < 7.

large_straight_3(X) :- dado4(Z), dado5(R), X is Z + 4, Z is R - 1, X > 0, X < 7, Z < X - 2, R < X - 2.
large_straight_3(X) :- dado4(Z), dado5(R), X is Z - 1, Z is R - 1, X \= 0, X \= 7, X > 2.
large_straight_3(X) :- dado4(Z), dado5(R), X is Z + 3, Z is R - 4, X \= 0, X \= 7.

large_straight_4(X) :- dado5(R), X is R - 1, X > 0, X < 7, X > 3.
large_straight_4(X) :- dado5(R), X is R + 4, X > 0, X < 7, X > 4.

large_straight_h_1(X,Y) :- dado3(W), X is W - 1, Y is W + 3, X > 0, X < 7, Y > X + 3, Y < 7.
large_straight_h_1(X,Y) :- dado3(W), X is W - 1, Y is W + 2, X > 1, X < 7, Y > X + 2, Y < 7.
large_straight_h_1(X,Y) :- dado3(W), X is W - 1, Y is W + 1, X > 2, X < 7, Y > X + 1, Y < 7.
large_straight_h_2(X,Y) :- dado3(W), dado4(Z), X is W - 1, W is Z - 1, Y is Z + 2, X > 0, X < 7, Y > X + 3, Y < 7.
large_straight_h_2(X,Y) :- dado2(W), dado3(Z), X is W - 1, W is Z - 1, Y is Z + 1, X > 1, X < 7, Y > X + 2, Y < 7.
%large_straight_h_3(X,Y) :- dado2(W), dado3(Z), dado4(R), X is W - 1, W is Z - 1, Z is R - 1, Y is R + 1, X > 0, X < 7, Y > X + 3, Y < 7.
% 
query(yahtzee_with_4).
query(yahtzee_with_3).
query(yahtzee_with_2).
query(yahtzee_with_1).
query(full_house_with_2(1)). % todos iguales por eso tomo solo el 1
query(full_house_with_3(1)). % todos iguales por eso tomo solo el 1
query(small_straight_2(_)).
query(small_straight_3(_)).
query(small_straight_h_1(1,4)). % todos iguales por eso tomo solo el 1,4
query(small_straight_h_2(1,4)). % todos iguales por eso tomo solo el 1,4
query(large_straight_2(_)).
query(large_straight_3(_)).
query(large_straight_4(_)).
query(large_straight_h_1(1,5)). % todos iguales por eso tomo solo el 1,5
query(large_straight_h_2(1,5)). % todos iguales por eso tomo solo el 1,5
%query(large_straight_h_3(1,5)). % todos iguales por eso tomo solo el 1,5