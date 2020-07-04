/* 22 август 2017
Зад. 1. Ще използваме списъци от символи, за да представяме думи. 
    Да се дефинира на Пролог предикат р(А, В, С, К), който по дадени думи A, В и С 
    проверява дали думата К може да се представи и като конкатенации от вида wС, 
    където думата w е или равна на С, или е конкатенация на четен брой думи, всяка от които е равна на A или В. 
Забележка: Условието за К може да се запише като следния и регулярен израз:
    (C union ((A union B)(A union B))*)С.
*/
append1([], B, B).
append1([H|T], B, [H|R]):- 
    append1(T, B, R).

p(A, B, C, K):- 
    append1(W, C, K), 
    ((W = C) ; (condition(A, B, W, N), N mod 2 =:= 0)).

condition(_, _, [], 0).
condition(A, B, CurrentW, N):-             
    append1(A, NextW, CurrentW), 
    condition(A, B, NextW, M), N is M + 1.
condition(A, B, CurrentW, N):- 
    append1(B, NextW, CurrentW), 
    condition(A, B, NextW, M), N is M + 1.


/* 22 август 2017
Зaд. 2. Записваme съждителни формули посредством списъци, 
    в които всяка подформула се записва с отделен списък, който се състои от 
    основната логическа операции в подформулата и нейните аргументи. 
    Списъците се записват в инфиксен стил, като атомарните формули се записват с номера. 
    Например формулата (p1 or ~p2) and (p2 -> p4) се записва като списъка 
    [[[p, 1], or, [neg, [p, 2]]], and, [[p, 2], ->, [p, 4]]]. 
    Да се дефинира на Пролог предикат s(F, N), където F е формула с 
    атомарни формули p1, p2, ... pN, записана като списък, 
    а N е списък от N стойности t или f (като t е съкращение на истина, f съкращение па лъжа). 
    Предикатът трябва да проверява дали записаната с F формула е вярна, 
    ако първият елемент ка N е стойността па p1, вторият елемент на N е стойността на p2 и т.н. */

s(F, N):- replaceAllAtomicFormulaeWithValueFromN(F, N, VF), evaluate(VF).
% F=[[[p, 1], or, [neg, [p, 2]]], and, [[p, 2], ->, [p, 4]]], N = [t,f,t,t] -> VF=[[t, or, [neg, f]], and, [f, ->, t]]  
% s([[[p, 3], <->, [p, 4]], or, [p, 2]], [t,f,t,f]).
% s([[[p, 1], or, [neg, [p, 2]]], and, [[p, 2], ->, [p, 4]]], [f, t, f ,t]).
evaluate(t).
evaluate([neg, A]):- not(evaluate(A)).
evaluate([A, or, B]):- evaluate(A) ; evaluate(B).
evaluate([A, and, B]):- evaluate(A) , evaluate(B).
evaluate([A, ->, B]):- not(evaluate(A)) ; evaluate(B).
evaluate([A, <->, B]):- evaluate([A, ->, B]) , evaluate([B, ->, A]).

% n_th_element(List, Index, Element)
n_th_element([X|_], 1, X).
n_th_element([_|T], N, X):- n_th_element(T, M, X), N is M + 1.

replaceAllAtomicFormulaeWithValueFromN(F, N, VF):- value(F, N, VF).

value([_, Index], N, Value):- n_th_element(N, Index, Value).
value([A, Op, B], N, [VA, Op, VB]):- value(A, N, VA), value(B, N, VB).
value([neg, A], N, [neg, VA]):- value(A, N, VA).
