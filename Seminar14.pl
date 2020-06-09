/*
Представяне на точка (p, q) ∈ Q × Q с рационални координати в равнината
наричаме всяка четворка (ap , bp , aq , bq ) ∈ Z4 , за която a bp p = p и bq aq = q. (в частност
bp 6= 0 6= bq .)
Да се дефинира на пролог предикат:
1. max_independent(S, M ), който по даден краен списък S от представяния
на точки с рационални координати в равнината генерира в M максимално
по размер подмножество на S, така че никои две различни окръжностицентрове в точки, представени от елементи на M , и радиуси 1 нямат общи


I.2 Дървото на Раней, R, е пълно двоично дърво, във върховете на което
така са поставени двойките естествени числа, че:
• в корена на R е поставена двойката (1, 1);
• ако в един връх v на R е поставена двойката (a, b), то в левия наслед-
ник на v е поставена двойката (a, a + b), а в десния - (a + b, b).
Да се дефинира на пролог едноместен предикат raney(L), който при пре-
удовлетворяване генерира в L всички етажи от дървото на Раней.

*/

% -----------------------------------------------------------------------------------------------------
/*
Да се дефинират на пролог едноместни предикати q1, q2 , q3 и q4 .
такива че даден списък X:
(1) q1 разпознава дали празният списък е елемент на X,
(2) q2 разпознава дали X съдържа елементи Y и Z, които нямат общи
елементи,
(3) q3 разпознава дали X съдържа елемент Y, чиито елементи са
елементи на всички елемeнти на X,
(4) q4 разпознава дали за всеки елемент Y на X съществува такъв
елементи Z на X, че Y и Z нямат общи елемeнти.
*/

q1(X):- member(Y, X), Y = [].

q2(X):- append(_, [Y|B], X), append(_, [Z|_], B), not(commonElement(Y, Z)).

commonElement(Y, Z):- member(X, Y), member(X, Z).

/* (E Y in X)(A Z in X)[Y subset Z] |=| (E Y in X)-(E Z in X)[Y not subset Z]  
    Y subset Z := (A W in Y)[W in Z] |=| -(E W in Y)[W not in Z] */
isSubsetOf(X, Y):- not(( member(W, X), not(member(W, Y)) )).

q3(X):- member(Y, X), not(( member(Z, X), not(isSubsetOf(Y, Z)) )).

/* (A Y in X)(E Z in X)[Y int Z = empty set] |=| -(E Y in X)-(E Z in X)[Y int Z = empty set]
    Y int Z = empty set |=| -(E X in Y)[X in Z]
    commonElement(Y, Z) := (E X in Y)[X in Z]
*/
q4(X):- not((member(Y, X), not((member(Z, X), not(commonElement(Y,Z)) )) )).

/*
Да се дефинира на Пролог двуместен предикат,
който по дадени две цели числа A, B in Z разпознава дали те имат
едни и същи прости делители.
*/

areEqual(A, B):- isSubsetOf(A, B), isSubsetOf(B, A).

makeAbsolute(A, A):- A >= 0.
makeAbsolute(A, A1):- A < 0, A1 is -A.

samePrimeDivs(A, B):- makeAbsolute(A, A1), 
                    makeAbsolute(B, B1), 
                    primeDivs(A1, PrimeDivsA), 
                    primeDivs(B1, PrimeDivsB), 
                    areEqual(PrimeDivsA, PrimeDivsB).

d(X, Y):- Y mod X =:= 0.

leastPrimeDivisor(X, Y, X):- d(X, Y).
leastPrimeDivisor(X, Y, R):- not(d(X, Y)), X1 is X + 1, leastPrimeDivisor(X1, Y, R).

primeDivs(1, []).
primeDivs(X, [H|T]):- X > 1, leastPrimeDivisor(2, X, H), Y is X // H, primeDivs(Y, T).

/*
I.1 Редиците на Фарей, Fn , са редици от двойки естествени числа, които
се дефинират рекурсивно за n ≥ 1 по следния начин:
• F1 = [[0, 1], [1, 1]];
• Fn+1 се получава от Fn , като между всеки два последователни члена
[a, b] и [c, d] на Fn, за които b+d = n+1, се добавя двойката [a+c, n+1].
Да се дефинира на пролог едноместен предикат farey(F), който при пре-
удовлетворяване генерира в F всички редици на Фарей.
*/

farey(F):- farey(F, _).

farey([[0, 1], [1, 1]], 1).
farey(F, NPlus1):- farey(F1, N), NPlus1 is N + 1, addNewPairs(F1, NPlus1, F).

addNewPairs([], _, []).
addNewPairs([X], _, [X]).
addNewPairs([[A, B], [C, D]|T], NPlus1, [[A, B], [APlusC, NPlus1]| R]):- 
    B + D =:= NPlus1, 
    APlusC is A + C, 
    addNewPairs([[C, D]|T], NPlus1, R).
addNewPairs([[A, B], [C, D]|T], NPlus1, [[A, B]| R]):- 
    B + D =\= NPlus1, 
    addNewPairs([[C, D]|T], NPlus1, R).

/*
Казваме, че списък X мажорира списък Y , ако
всички елементи на X са елементи на Y . Да се дефинира
на пролог предикат p(L, M ), който по даден списък от спи-
съци L намира списък M , който съдържа всички елементи
на L и в който никой елемент не се мажорира от елемент,
намиращ се след него в списъка.
*/
maj(X, Y):- isSubsetOf(X, Y).

/* -append(_, [Y|B], L)(E Z in B)[Z subset Y] */
p(L, M):- permutation(L, M), condition(M).

condition(L):- not(( append(_, [Y|B], L), member(Z, B), maj(Z, Y) )).

/*
Ще казваме, че един списък е анаграма на друг, ако е съставен от същите елементи, но
в евентуално различен ред.
Да се дефинира предикат на пролог maxAnagrams(L, M ) на пролог, който по даден
списък от списъци L, генерира в M най-голямото число, за което същ K >= M на брой
M-елементни списъка от L, които са анаграми един на друг.
*/

isAnagram(X, Y):- permutation(X, Y).

maxAnagrams(L, M):- 
    length(L, N), 
    between(0, N, M), 
    generateAnagramList(L, M),
    M1 is M + 1,
    not(( between(M1, N, K), generateAnagramList(L, K) )).
   
generateAnagramList(L, M):-  
    subset(S, L), 
    length(S, V),
    V >= M,
    conditionLengths(S, M),
    areAllAnagrams(M).

conditionLengths([], _).
conditionLengths([H|T], M):- length(H, M), conditionLengths(T, M).

areAllAnagrams([]).
areAllAnagrams(L):- L = [H|T], areAllAnagrams(H, T).

areAllAnagrams(_, []).
areAllAnagrams(X, [H|T]):- isAnagram(X, H), areAllAnagrams(X, T).

/*
Нека за всяко положително число i с ξ(i) означим съответно броя
на простите числа от вида 6k + 1, които са по-малки от i.
Да се дефинират на пролог еднометсни предикати su(X), които
по дадено цяло число X разпознават дали за някое положително цяло
число i е в сила равенството X = i+ξ(i) за su(X).
*/

isPrime(P):- not(( between(2, P, K), d(K, P) )).

ksi(I, NumPrimes):- ksi(I, 2, NumPrimes).
ksi(I, I, 0).
ksi(I, J, NumPrimesJ):- 
    J < I, 
    isPrime(J), 
    J mod 6 =:= 1, 
    J1 is J + 1, 
    ksi(I, J1, NumPrimesJ1), 
    NumPrimesJ is NumPrimesJ1 + 1.
ksi(I, J, NumPrimesJ1):- 
    J < I, 
    not(( isPrime(J), 
    J mod 6 =:= 1 )), 
    J1 is J + 1, 
    ksi(I, J1, NumPrimesJ1).

su(X):- X > 0, between(1, X, I), ksi(I, NumPrimes), X =:= I + NumPrimes.
