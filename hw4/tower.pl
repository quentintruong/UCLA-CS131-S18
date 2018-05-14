tower(N, T, counts(U,D,L,R)) :- 
    fair(N,T,L),
    transpose(T,Ttrans),
    fair(N,Ttrans,U),
    fairR(N,T,R),
    fairR(N,Ttrans,D).

fairR(Length, T, C) :-
    fairR(Length, Length, T, C).
fairR(Length, 0, [], []).
fairR(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    reverse(Thead, TheadReverse),
    length(Thead, L),
    L == Length, !,
    increasing(TheadReverse,CountHead,Length),
    RemainingDec #= Remaining - 1,
    fairR(Length, RemainingDec, Ttail, CountTail).

fair(Length, T, C) :-
    fair(Length, Length, T, C).
fair(Length, 0, [], []).
fair(Length, Remaining, [Thead|Ttail], [CountHead|CountTail]) :-
    increasing(Thead,CountHead,Length),
    RemainingDec #= Remaining - 1,
    fair(Length, RemainingDec, Ttail, CountTail).

increasing([Head|Tail],Count,Length) :-
    Head #> 0,
    Head #=< Length,
    CountDec #= Count - 1,
    LengthDec #= Length - 1,
    increasing(Tail,Head,CountDec,LengthDec,Length),
    fd_all_different([Head|Tail]).

increasing([],V,0,0,N).

increasing([Head|Tail],V,Count,Length,N) :- 
    Head #> V,
    Head #=< N,
    CountDec #= Count - 1,
    LengthDec #= Length - 1,
    increasing(Tail,Head,CountDec,LengthDec,N).

increasing([Head|Tail],V,Count,Length,N) :- 
    Head #> 0,
    Head #< V,
    LengthDec #= Length - 1,
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