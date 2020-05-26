/* 
Arithmetics in Prolog:

length(L, N).
element_At(L, Elem, Idx).
n_th_element(L, Elem, Idx).

Generators (finite and infinite):

natural(N).
pairs(N, M).
between(A, B, C).

HW:

quickSort

*/
length1([], 0).
length1([_|T], N):- length1(T, M), N is M + 1.

% X is Y+Z

sum([], 0).
sum([H|T], N):- sum(T, M), N is M + H.

element_At([H|_], H, 0).
element_At([_|L], E, N):- N > 0, M is N - 1, element_At(L, E, M).

n_th_element([H|_], H, 0).
n_th_element([_|L], E, N):- n_th_element(L, E, M), N is M + 1.


% 0, 1, 2, ...
% 0, 2, 4...
% 0 1 1 2 3 5 8 13
% ?-p(X).

natGenerator(0).
natGenerator(N):- natGenerator(M), N is M + 1.

natRecogniser(0).
natRecogniser(N) :- N > 0, M is N - 1, natRecogniser(M).

evenGenerator(0).
evenGenerator(N):- evenGenerator(M), N is M + 2.


evenGenerator2(N):- natGenerator(M), N is M * 2.

% p(X):- natGenerator(N), decode(X).

isEven(N):- N mod 2 =:= 0.

evenGenerator3(N):- natGenerator(N), isEven(N).

% pairs(N, M):- natGenerator(N), natGenerator(M).

% between(A, B, C). C in [A; B]
between1(L, R, L):- L =< R.
between1(L, R, X):- L < R, L1 is L + 1, between1(L1, R, X).

range(L, L, [L]).
range(L, R, [L|T]):- L < R, L1 is L + 1, range(L1, R, T).

pairs(N, M):- natGenerator(S), between(0, S, N), M is S - N.

% x1 + x2 + ... + xk = S
s_vectors([H], 1, H).
s_vectors([H|T], K, S):- K > 1, between(0, S, H), K1 is K - 1, S1 is S - H, s_vectors(T, K1, S1). 

generateAllFiniteSequencesOfNaturalNumbers([]).
generateAllFiniteSequencesOfNaturalNumbers(L):- pairs(K, S), K > 0, s_vectors(L, K, S).


% 0, 1, 2, 3, ...., -1, -2, 
% 0, 1, -1, 2, -2, 3, ...
switchSign(X, X).
switchSign(X, Y):- X > 0, Y is -X.

integer1(X):- natGenerator(N), switchSign(N, X).

integer2(X):- natGenerator(N), member(S, [1, -1]), X is N * S.
% fib(N, X).
fib(0, 0).
fib(1, 1).
fib(N, X):- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, A), fib(N2, B), X is A + B. 

fib(X):- natGenerator(N), fib(N, X).

fibonacci(0, 0, 1).
fibonacci(N, L, R):- fibonacci(N1, L1, L), N is N1 + 1, R is L1 + L.

% A_{n+2} A_{n+1} A_n

/* 
A_0=A_1=2
A_{n+2}=2*A_{n+1} - A_n + n 
*/

someSequence(0, 2, 2).
someSequence(N2, L, R):- someSequence(N1, L1, L), N2 is N1 + 1, R is 2 * L - L1 + N1 - 1.

fibonacci(X):- fibonacci(_, X, _).