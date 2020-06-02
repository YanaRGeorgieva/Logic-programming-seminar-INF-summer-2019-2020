/*
split, flatten, binaryTreeGenerator.

Списък от три числа [X, Y, R] ще интерпрети-
раме като окръжност с център hX, Y i и радиус R. Да
се дефинира генератор circles(X, Y, R, Z, T, S), който по
дадена окръжност [X, Y, R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които съдържат ок-
ръжността [X, Y, R].

*/
    
% split(L, R) -> L = [1,2,3] -> R = [[1], [2], [3]], R = [[1, 2], [3]], R = [[1], [2, 3]], R = [[1, 2, 3]].
split([], []).
split(L, [Prefix|R]):-  append(Prefix, Suffix, L), 
                        Prefix \= [], 
                        split(Suffix, R).


% flatten([[1], a, [[2,3],6]], [1, a, 2 , 3 ,6]).
isList([]).
isList([_|_]).

$flatten([], []). 
$flatten(X, [X]):- not(isList(X)).
$flatten([H|T], R):- $flatten(H, FH), $flatten(T, FT), append(FH, FT, R).

flatten(X, R):- isList(X), $flatten(X, R).


% []
% [A, B]

% [[[], []], []] /

binaryTreeGenerator(0, []).
binaryTreeGenerator(N, [A, B]):- N > 0, N1 is N - 1, between(0, N1, NA), NB is N1 - NA,
    binaryTreeGenerator(NA, A), binaryTreeGenerator(NB, B).

nat(0).
nat(N):-nat(M), N is M + 1.

binaryTreeGenerator(T):-nat(N), binaryTreeGenerator(N, T).

% NOT binaryTreeGenerator([A, B]):-binaryTreeGenerator(A), binaryTreeGenerator(B).

% HW make binaryTreeGenerator  -> generateTree many children

/*
Списък от три числа [X, Y, R] ще интерпрети-
раме като окръжност с център hX, Y i и радиус R. Да
се дефинира генератор circles(X, Y, R, Z, T, S), който по
дадена окръжност [X, Y, R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които съдържат ок-
ръжността [X, Y, R].
*/

inCircle(X, Y, R, A, B):- X1 is X - R, X2 is X + R, Y1 is Y - R, Y2 is Y + R,
    between(X1, X2, A), between(Y1, Y2, B),
    (A-X)*(A-X) + (B-Y)*(B-Y) =< R*R.

/*
Фенски списък е краен списък, всеки елемeнт, на който е някоя от буквите 1,2, или фенски списък, като
броя елементи, които са буквата 1 е равен на броят елементи, които са буквата 2. Да се дефинира предикат,
който чрез преудовлетворяване генерира всички фенски списъци с краен брой '['.
*/

/*
1, 2, []

*/
generateTree(0, []).
generateTree(N, [H|T]):- N > 0, N1 is N - 1, between(0, N1, R), M is N1 - R, generateSubTree(R, H), generateTree(M, T).

generateSubTree(1, X):- member(X, [1, 2]).
generateSubTree(N, X):- generateTree(N, X).

oneTwoTree(T):- nat(N), generateTree(N, T).

fenlist(L):- oneTwoTree(L), condition(L).

% count(X, L, N).
count(_, [], 0).
count(H, [H|T], N):- count(H, T, N1), N is N1 + 1.
count(X, [H|T], N):- H \= X, count(X, T, N).

condition(X):- not(isList(X)).
condition(L):- isList(L), count(1, L, N), count(2, L, N), not(( member(X, L), not(condition(X)) )).

/* 
L = [L1, L2, ..., Ln] - list of lists |L| = n -> R = [a1, a2, ..., an] - all n-tuples, where ai in Li  
*/

cartesianTuple([], []).
cartesianTuple([LI|T], [X|R]):- member(X, LI), cartesianTuple(T, R).