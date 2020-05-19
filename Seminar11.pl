% inUnion(X, A, B).
inUnion1(X, A, B):- append(A, B, L), member(X, L).

inUnion2(X, A, B):- member(X, A); member(X, B).

inUnion3(X, A, _):- member(X, A).
inUnion3(X, _, B):- member(X, B).

% inIntersection(X, A, B).
inIntersection1(X, A, B):- member(X, A), member(X, B).
/*
    If X is not instatiated then we ask Prolog if A \cap B != empty_set.
*/

% inDifference(X, A, B).
inDifference1(X, A, B):- member(X, A), not( member(X, B) ).

% isSubsetOf(A, B). \A x in A (x in B) |=| not \E x not (x in B)
isSubsetOf(A, B):- not(( member(X, A), not( member(X, B) ) )).

% areEqualSets(A, B).
areEqualSets(A, B):- isSubsetOf(A, B), isSubsetOf(B, A).

% toSet(A, B). // removeDuplicates(A,B).
toSet2([], S, S).
toSet2([H|T], S, R):-member(H, S), toSet2(T, S, R).
toSet2([H|T], S, R):-not(member(H, S)), toSet2(T, [H|S], R).

toSet(A, B):-toSet2(A, [], B).

toSetA([], []).
toSetA([H|T], S):- member(H, T), toSetA(T, S). % може и да ги резменим конюнктите
toSetA([H|T], [H|S]):- not(member(H, T)), toSetA(T, S).

toSetB([], []).
toSetB([H|T], [H|S]):- toSetB(T, S), not(member(H, S)).
toSetB([H|T], S):- toSetB(T, S), member(H, S). % тук не можем

% palindrome(L).
% palindrome(L):- reverse(L, L1), L = L1.
palindrome(L):- reverse(L, L).

palindrome1([]).
palindrome1([_]).
% palindrome1(L):- append([H|L1], [Last], L), H = Last, palindrome1(L1).
palindrome1(L):- append([H|L1], [H], L), palindrome1(L1).
/*
    aaabbbaaa -> aabbbaa -> abbba -> bbb -> b
    abaklba -> baklb -> akl -> fail
*/

% selectionSort(L, S).
less(X, Y):- X < Y.
lessOrEqual(X, Y):- X =< Y.
% not(lessOrEqual(X, Y)) -> X > Y
% minElement(L, X):- member(X, L), not(( member(Y, L), less(Y, X) )).
%                                            not(lessOrEqual(X, Y)) -> X > Y
minElement(L, X):- member(X, L), not(( member(Y, L), not( lessOrEqual(X, Y) ) )).

minTwoElements(A, B, A):- less(A, B).
minTwoElements(A, B, B):- not( less(A, B) ).

minElement2([], Min, Min).
minElement2([H|T], CurrentMin, Min):- minTwoElements(H, CurrentMin, NewCurrentMin), minElement2(T, NewCurrentMin, Min).
minElement2(L, Min):- L=[H|T], minElement2(T, H, Min).

minElement3([M], M).
minElement3([H|T], M):- minElement3(T, N), minTwoElements(N, H, M).

remove(L, X, Rest):- append(A, [X|B], L), append(A, B, Rest).

selectionSort([], []).
selectionSort(L, [Min|R]):- minElement(L, Min), remove(L, Min, Rest), selectionSort(Rest, R).


% insertCorrect(X, SortedList, Res).
insertCorrect(X, [], [X]).
insertCorrect(X, [H | T], [X | [H | T]]) :- X < H.
insertCorrect(X, [H | T], [H | Res]) :- X >= H, insertCorrect(X, T, Res).

% insertionSort(List, SortedList).
insertionSort(List, SortedList) :- insertionSortHelper(List, [], SortedList).

insertionSortHelper([], X, X).
insertionSortHelper([H | T], Curr, SortedList) :- insertCorrect(H, Curr, SortedListHelper),
                                                  insertionSortHelper(T, SortedListHelper, SortedList).

mergeSort([], []).
mergeSort([X], [X]).
mergeSort([H|T], R):-  T \= [], 
                        split([H|T], LeftList, RightList), 
                        mergeSort(LeftList, R1), 
                        mergeSort(RightList, R2),  
                        merge(R1, R2, R).

split([], [], []).
split([X], [X], []).
split([A, B|T], [A|LR], [B|RR]):- split(T, LR, RR).

merge([], [], []).
merge([], B, B):- B \= [].
merge(A, [], A):- A \= [].
merge([H1|LL], [H2|RL], [H1|R]):- less(H1, H2), merge(LL, [H2|RL], R).
merge([H1|LL], [H2|RL], [H2|R]):- not( less(H1, H2) ), merge([H1|LL], RL, R). 


% L T R

add(X, [], [[], X, []]).
add(X, [L, Root, R], [L1, Root, R]):- less(X, Root), add(X, L, L1).
add(X, [L, Root, R], [L, Root, R1]):- not(less(X, Root)), add(X, R, R1).

makeTree([], []).
makeTree([H|L], T):- makeTree(L, T1), add(H, T1, T).

ltr([], []).
ltr([L, Root, R], List):- ltr(L, LList), ltr(R, RList), append(LList, [Root|RList], List).

treeSort(L, S):- makeTree(L, T), ltr(T, S).


/* 
HW:

quickSort

Да се дефинират на пролог едноместни предикати q1, q2 , q3 и q4 .
такива че даден списък X:
(1) q1 разпознава дали празният списък е елемент на X,
(2) q2 разпознава дали X съдържа елементи Y и Z, които нямат общи
елементи,
(3) q3 разпознава дали X съдържа елемент Y, чиито елементи са
еленти на всички елемeнти на X,
(4) q4 разпознава дали за всеки елемент Y на X съществува такъв
елемент Z на X, че Y и Z нямат общи елемeнти.

 Да се дефинира на пролог предикат p(X, Y ),
който по даден списък от числа X и списък от списъци
от числа Y проверява дали са изпълнени следните три
условия: 1) X може да се представи като конкатенация
на два елемента на Y ; 2) X има четен брой елементи
и 3) сумата от елементите на X е последен елемент на
елемент на Y .

Да се дефинира на пролог предикат p(X, Y ),
който по даден списък X от списъци от числа намира та-
къв елемент Y на X, че Y не съдържа по-голям елемент от
най-големите елементи на елементите на X, и никой еле-
мент на X, притежаващ същото свойство, не е с повече
елементи от Y .

Да се дефинира на пролог предикат p(L, M ),
който по даден списък от числа L при преудовлетворяване
генерира в M всички списъци, такива че:
∙ множеството от елементите на M е подмножество на
множеството от елементите на L;
∙ за всеки елемент X на M съществува такъв елемент
Y на M , че множеството { X − Y, X ∗ Y, X + Y } е
подмножество на множеството от елементите на L.

Да се дефинира на пролог предикат p(L), който
по даден списък от различни списъци L проверява дали
всеки два различни елемента на L имат общ елемент, който
не принадлежи на някой елемент на L.

Да се дефинира на пролог предикат p(L), който
по даден списък от различни списъци L проверява дали
в L съществуват два различни елемента, които имат общ
елемент, който не принадлежи на никой друг елемент на L.

Казваме, че списък X мажорира списък Y , ако
всички елементи на X са елементи на Y . Да се дефинира
на пролог предикат p(L, M ), който по даден списък от спи-
съци L намира списък M , който съдържа всички елементи
на L и в който никой елемент не се мажорира от елемент,
намиращ се след него в списъка.

Да се дефинират предикати p(X), q(X) и r(X), та-
кива че ако X е списък от списъци, то
p(X) ⇐⇒ 5 е елемент на поне два елемента на X
q(X) ⇐⇒ всеки три елемента на X имат поне два различ-
ни общи елемента.
r(X) ⇐⇒ съществува такова ненулево естествено число n,
че X съдържа 2n различни по между си елемен-
ти, всеки от които има не повече от n елемента.

За произволен списък [a1 , a2, . . . , ak ] ще казваме, че
списъците от вида [am , am+1, . . . , am+i ], където 1 5 m 5 k и
0 5 m 5 k − m, са негови подспицъци. Да се дефинира пре-
дикат p(X, Y, Z), който по дадени списъци X и Y генерира
всички подсписъци Z на Y , такива че дължината на Z е
колкото дължината на X, всеки елемент на Z е по-голям от
елемента, намиращ се на същата позиция в X и последните
елементи на Z и Y са равни.

*/