% Part 1
% tower
tower(N, T, counts(U,D,L,R)) :- 
    fair(N,T,L),
    transpose(T,Ttrans),
    fair(N,Ttrans,U),
    fairR(N,T,R),
    fairR(N,Ttrans,D),
    flatten(T,Flat),
    fd_labeling(Flat).

% fair used to check a direction of T
fair(Length, T, C) :-
    fair(Length, Length, T, C).
fair(_, 0, [], []).
fair(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    increasing(Thead,CountHead,Length),
    RemainingDec is Remaining - 1,
    RemainingDec >= 0,
    fair(Length, RemainingDec, Ttail, CountTail).

% fairR used to check a reverse direction of T
fairR(Length, T, C) :-
    fairR(Length, Length, T, C).
fairR(_, 0, [], []).
fairR(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    reverse(Thead, TheadReverse),
    length(Thead, L),
    L == Length, !,
    increasing(TheadReverse,CountHead,Length),
    RemainingDec is Remaining - 1,
    RemainingDec >= 0,
    fairR(Length, RemainingDec, Ttail, CountTail).

% increasing used to check if row has strictly increasing sublist of size count
increasing([Head|Tail],Count,Length) :-
    Head #> 0,
    Head #=< Length,
    CountDec #= Count - 1,
    CountDec #>= 0,
    LengthDec #= Length - 1,
    LengthDec #>= 0,
    increasing(Tail,Head,CountDec,LengthDec,Length),
    fd_all_different([Head|Tail]).
increasing([],_,0,0,_).
increasing([Head|Tail],V,Count,Length,N) :- 
    Head #> V,
    Head #=< N,
    CountDec #= Count - 1,
    CountDec #>= 0,
    LengthDec #= Length - 1,
    LengthDec #>= 0,
    increasing(Tail,Head,CountDec,LengthDec,N).
increasing([Head|Tail],V,Count,Length,N) :- 
    Head #> 0,
    Head #< V,
    LengthDec #= Length - 1,
    LengthDec #>= 0,
    increasing(Tail,V,Count,LengthDec,N).

% Modified implementation of transpose taken from SWI Prolog
transpose(Ms, Ts) :-
    (   Ms = [] -> Ts = []
    ;   Ms = [F|_],
        transpose(F, Ms, Ts)
    ).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% Part 2.a
plain_tower(N, T, counts(U,D,L,R)) :- 
    plain_fair(N,T,L),
    transpose(T,Ttrans),
    plain_fair(N,Ttrans,U),
    plain_fairR(N,T,R),
    plain_fairR(N,Ttrans,D).

% plain_fair used to check a direction of T
plain_fair(Length, T, C) :-
    plain_fair(Length, Length, T, C).
plain_fair(_, 0, [], []).
plain_fair(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    plain_increasing(Thead,CountHead,Length),
    RemainingDec is Remaining - 1,
    RemainingDec >= 0,
    plain_fair(Length, RemainingDec, Ttail, CountTail).

% plain_fairR used to check a reverse direction of T
plain_fairR(Length, T, C) :-
    plain_fairR(Length, Length, T, C).
plain_fairR(_, 0, [], []).
plain_fairR(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    reverse(Thead, TheadReverse),
    length(Thead, L),
    L == Length, !,
    plain_increasing(TheadReverse,CountHead,Length),
    RemainingDec is Remaining - 1,
    RemainingDec >= 0,
    plain_fairR(Length, RemainingDec, Ttail, CountTail).

plain_increasing(P,Count,Length) :-
    range_inc(1,Length,R),
    permutation(R,P),
    member(Count,R),
    plain_increasing_helper(P,Count,Length).

plain_increasing_helper([Head|Tail],Count,Length) :-
    Head > 0,
    Head =< Length,
    CountDec is Count - 1,
    CountDec >= 0,
    LengthDec is Length - 1,
    LengthDec >= 0,
    plain_increasing_helper(Tail,Head,CountDec,LengthDec,Length).
plain_increasing_helper([],_,0,0,_).
plain_increasing_helper([Head|Tail],V,Count,Length,N) :- 
    Head > V,
    Head =< N,
    CountDec is Count - 1,
    CountDec >= 0,
    LengthDec is Length - 1,
    LengthDec >= 0,
    plain_increasing_helper(Tail,Head,CountDec,LengthDec,N).
plain_increasing_helper([Head|Tail],V,Count,Length,N) :- 
    Head > 0,
    Head < V,
    LengthDec is Length - 1,
    LengthDec >= 0,
    plain_increasing_helper(Tail,V,Count,LengthDec,N).

range_inc(N,N,[N]) :- !.
range_inc(N,M,[N|T]) :- 
    N1 is N + 1,
    range_inc(N1, M, T).

% Part 2.b
speedup(Ratio) :-
    stat_plain_tower(A),
    stat_tower(B),
    !,
    Ratio is A / B.

stat_plain_tower(TimeLength) :-
    statistics(cpu_time, [Start|_]), plain_tower(4,_,_), statistics(cpu_time, [End|_]),
    TimeLength is (End - Start).

stat_tower(TimeLength) :-
    statistics(cpu_time, [Start|_]), tower(4,_,_), statistics(cpu_time, [End|_]),
    TimeLength is (End - Start).

% Part 3
ambiguous(N,C,T1,T2) :-
    call(tower(N,T1,C)),
    call(tower(N,T2,C)),
    T1\==T2.
