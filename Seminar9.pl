/* HW
remove(X, L, NewL).
removeAllX(X, L, NewL).
insert(X, L, NewL). 
*/

%remove(X, L, NewL).
remove(X, [X|T], T).
remove(X, [H|T], [H|Z]) :-
    remove(X, T, Z).

remove1(X, List, Result) :-
    append(A, [X|B], List),
    append(A, B, Result).

% removeAllX(X, L, NewL).
% removeAllX(X, [X], []).
% p():-removeAll(), q().
removeAllX(_, [], []).
removeAllX(X, [X|T], Z) :-
    removeAllX(X, T, Z).
removeAllX(X, [H|T], [H|Z]) :-
    % X \= H,
    removeAllX(X, T, Z).

insert(X, L, NewL) :-
    remove(X, NewL, L).
insert1(X, List, Result) :-
    append(A, B, List),
    append(A, [X|B], Result).

%insert(X, L, NewL).
insertFront(X, L, [X|L]).

insertBack(X, [], [X]).
insertBack(X, [H|T], [H|Z]) :-
    insertBack(X, T, Z).

% member1(X, List).
member1(X, [X|_]).
member1(X, [_|T]) :-
    member1(X, T).

member2(X, L) :-
    append(_, [X|_], L).

member3(X, L) :-
    remove(X, L, _).

member4(X, L) :-
    insert(X, _, L).

% permutation(L, P).
% [X|P] List

permutation([], []).
permutation(List, [X|P]) :-
    remove(X, List, Q),
    % write(X),
    % nl,
    % write(Q),
    % nl,
    permutation(Q, P).

permutation1([], []).
permutation1([H|T], R) :-
    permutation1(T, Q),
    insert(H, Q, R).

% less(X, Y).
less(X, Y):- X =< Y.
% isSorted(List). 
isSorted([]).
isSorted([_]).
isSorted([X, Y|T]) :-
    less(X, Y),
    isSorted([Y|T]).
% X1 < X2 < X3 <.....<XM

% simpleSort(List, SortedList).
simpleSort(List, SortedList) :-
    permutation(List, SortedList),
    isSorted(SortedList).

% prefix(List, Prefix).
prefix(List, Prefix) :-
    append(Prefix, _, List).

% suffix(List, Suffix).
suffix(List, Suffix) :-
    append(_, Suffix, List).

% [1,2,5,a] -> [2,5]
% infix(List, Infix).
infix(List, Infix) :-
    prefix(List, Prefix),
    suffix(Prefix, Infix).

infix1(List, Infix) :-
    suffix(List, Suffix),
    prefix(Suffix, Infix).

% Prefix Infix Suffix
% List 
infix2(List, Infix) :-
    append(A, _, List),
    append(_, Infix, A).

% HW reverse(List, ReversedList).
/*
    We have a binary relation p. Write these formulas on Prolog.
        a) \forall X \forall Y p(X,Y)
        b) \exists X \forall Y p(X,Y)
        c) \forall X \exists Y p(X,Y)
        d) \exists X \exists Y p(X,Y)
*/
% subset(List, Subset).
