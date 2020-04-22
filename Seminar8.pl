% S|=f

% 1. Факти
% p(T1, T2, ..., Tn).
% <T1, T2, ... Tn> e ot p
% p(ivan, peter).
% p(X, peter).

% 2. Правила
% p(...):-p1(...), p2(...), ..., pn(..).
% p(X):-q(X, X).

% 3.Цели
% ?-p1(), p2(), ..., pn().

parent(katja, maria).
parent(ivan, maria).
parent(maria, peter).
parent(peter, hristo).

v(X, Y):-parent(Y, Z), parent(Z, X).

% X --------------...Z Y

ancestor(X, Y):- parent(X, Y).
% ancestor(X, Y):- parent(X, Z), ancestor(Z, Y).
ancestor(X, Y):- ancestor(X, Z), parent(Z, Y).


member(X, [X|_]).
member(X, [_|T]):-member(X, T).

first(F, [F|_]).

second(S, [_, S|_]).

last(X, [_|T]):-last(X, T).
last(X, [X]).

% append(A, B, AB)
% X|A1 B 
% X|C
% C = A1.B
append([], B, B).
append([X|A], B, [X|C]):-append(A, B, C).

last2(X, L):-append(_, [X], L).
member2(X, L):-append(_, [X|_], L).

% HW
remove(X, L, NewL).
removeAllX(X, L, NewL).
insert(X, L, NewL).

% f(f(1))
% empty
% list(Head, Tail)
% [1]
% list(1, empty).
% [1, 2]

%  list(1, list(2, empty))

% add(X, L, NewL).
% add(X, L, list(X, L)).
% % member(X, L).
% member(X, list(X, L)).
% member(X, list(Y, L)):-member(X, L).
% % 2, [1, 2]
% % 2 [2]
% % [[], 1]
% []
% [H|T]

% [H, T]

% [A, [B|[C |T]]]
% [A, [B, C]| T]
% [1, [2, 3], 4, 5, []]

% [1,2]
% [1|[2|[]]]
% % .(1,.(2,[]))

% [1,2,3,[[a]], amalalska]

