% ------------------------------
% Coprime Check via GCD
% ------------------------------

% Find the greatest common divisor (GCD) of two numbers
gcd_val(0, B, B) :- 
    B > 0.
gcd_val(A, B, G) :- 
    A > 0, B > 0,
    Rem is B mod A,
    gcd_val(Rem, A, G).

% True if two numbers share no common divisors except 1
is_coprime(M, N) :-
    M > 0, N > 0,
    gcd_val(M, N, G),
    G =:= 1.

% ------------------------------
% Generate List from Range
% ------------------------------

% Construct a list containing all integers between Low and High
make_range(Low, High, []) :-
    Low > High.
make_range(Low, High, [Low|Rest]) :-
    Low =< High,
    Next is Low + 1,
    make_range(Next, High, Rest).

% ------------------------------
% Insertion Sort Implementation
% ------------------------------

% Insert an element into the correct place in a sorted list
insert_sorted(Elem, [], [Elem]).
insert_sorted(Elem, [Head|Tail], [Elem,Head|Tail]) :-
    Elem =< Head, !.
insert_sorted(Elem, [Head|Tail], [Head|NewTail]) :-
    Elem > Head,
    insert_sorted(Elem, Tail, NewTail).

% Sort a list using insertion sort
insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, TempSorted),
    insert_sorted(H, TempSorted, Sorted).

% ------------------------------
% Binary Logic Constraint Solver
% ------------------------------

% Validate binary variable assignments under specific logic constraints
valid_solution([P, Q, R, S, T, U]) :-
    member(P, [0, 1]), member(Q, [0, 1]), member(R, [0, 1]),
    member(S, [0, 1]), member(T, [0, 1]), member(U, [0, 1]),

    P is Q * R * S * T * U,
    Q is (1 - R) * (1 - S) * (1 - T) * (1 - U),
    R is min(P + Q, 1),
    S is min(T + U, 1),
    T is (1 - P) * (1 - Q) * (1 - R),
    U is (1 - P) * (1 - Q) * (1 - R) * (1 - S) * (1 - T).
