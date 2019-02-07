% tower/3
tower(N, T, C) :-
    length(T, N),
    % T part
    lengthN(N, T),
    maplist(lengthN(N), T),
    transpose(T, TT),
    maplist(map_n(N), T),
    maplist(map_n(N), TT),
    maplist(fd_labeling, TT),
    % C part
    transpose(T, T1),
    reverse(T, T2),
    transpose(T2, T22),
    transpose(T, T4),
    reverse(T4, T44),
    transpose(T44, T444),
    tower_side(N, T1, V1),
    tower_side(N, T22, V2),
    tower_side(N, T, V3),
    tower_side(N, T444, V4),
    C = counts(V1,V2,V3,V4).

map_domain(N, L) :-
    fd_domain(L, 1, N).

map_n(N, T) :-
    fd_all_different(T),
    maplist(map_domain(N), T).

tower_side(N, [H|T], [Value|Next]) :-
    view_tower( H, 0, Value),
    tower_side(N, T, Next).
tower_side(N, [], Value) :-
    Value = [].

view_tower([], Max, View) :- 
    View = 0.
view_tower([H|T], M, V1) :- 
    H > M,  
    view_tower(T, H, View), V1 is View + 1.
view_tower([H|T], M, View) :- 
    H =< M, 
    view_tower(T, M, View).

counts([], [], [], []).
counts([H1|T1],[H2|T2],[H3|T3],[H4|T4]) :-
    counts(T1,T2,T3,T4).



% plain_tower/3
plain_tower(N, T, C) :-
    % T part
    lengthN(N, T),
    maplist(lengthN(N), T),

    plain_map_domain(N, L),
    transpose(T, TT),
    % idea from TA course
    plain_map_tower(T, TT, L, N),

    % C part
    transpose(T, T1),
    reverse(T, T2),
    transpose(T2, T22),
    transpose(T, T4),
    reverse(T4, T44),
    transpose(T44, T444),
    tower_side(N, T1, V1),
    tower_side(N, T22, V2),
    tower_side(N, T, V3),
    tower_side(N, T444, V4),
    C = counts(V1,V2,V3,V4).

plain_map_tower([], [], _, N).
plain_map_tower([H|T], [HT|TT], L, N) :-
    % permutation for row and column
    permutation(L, H),
    permutation(L, HT),
    plain_map_tower(T, TT, L, N).

plain_map_domain(0, []).
plain_map_domain(N, Value) :-
    findall(N1, between(1, N, N1), Value).



append_element(L, I, V) :-
    append(L, [I], V).

reverse([], L).
reverse([H|T], L) :- reverse(T, [H|L]).

speedup(Ratio) :-
    speed_tower(A, B),
    speed_plain(C, D),
    Ratio1 is B - A,
    Ratio2 is D - C,
    Ratio is Ratio2 / Ratio1.

speed_tower(Start, End) :-    
    statistics(cpu_time, [Start|T1]),
    tower(5, T, counts([3,2,1,2,3],[1,2,3,4,2],[3,2,2,3,1],[3,2,3,1,2])),
    statistics(cpu_time, [End|T2]).

speed_plain(Start, End) :-    
    statistics(cpu_time, [Start|T1]),
    plain_tower(5, T, counts([3,2,1,2,3],[1,2,3,4,2],[3,2,2,3,1],[3,2,3,1,2])),
    statistics(cpu_time, [End|T2]).

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    diff_matrix(T1, T2, CC),
    CC > 0.

diff_matrix([], [], TF) :-
    TF = 0.
diff_matrix([H1|T1], [H2|T2], TF) :-
    diff_list(H1, H2, TF1),
    diff_matrix(T1, T2, TF2),
    TF is TF1 + TF2.

diff_list([], [], TF) :-
    TF = 0.
diff_list([H1|T1], [H2|T2], TF) :-
    H1 == H2, diff_list(T1, T2, TF).
diff_list([H1|T1], [H2|T2], TF) :-
    H1 \= H2, TF = 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transpose matrix
% reference: clpfd.pl in SWI-Prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% code in TA course

apply(_,[]).
apply(Func, [H|T]) :- call(Func, H), apply(Func, T).

lengthN(N, T) :- length(T, N).

create_2d_array(T, N) :- length(T, N), apply(lengthN(N), T).


% find position I, J in matrix L
nth(1, [H], H).
nth(I, [_|T], Value) :- I1 is I-1, nth(I1, T, Value).
at(I, J, L, Value) :- nth(I, L, Row), nth(J, Row, Value).

append([], List1, List1).
append([Head|Tail], List2, [Head|Result] ):-
    append(Tail, List2, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create 1d array
create_array(0, []).
create_array(N, [0|A]) :- 
    N1 is N-1, create_array(N1, A).
