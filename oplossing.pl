% Ramses De Norre
% Andere

% Bewaar dit bestand als oplossing.pl en implementeer het predicaat arukone/3.
% Documenteer de oplossingsstrategie van je oplossing op hoog niveau.
% Documenteer de heuristieken en optimalisaties die je toepast.
% Besteed voldoende aandacht aan de leesbaarheid en indentatie van je code.
% Splits lange predicaten in korte predicaten die elk een eigen taak vervullen.
% Documenteer de beoogde functionaliteit van de afzonderlijke predicaten.
% Documenteer eventuele gekende problemen en leg uit hoe je deze op kan lossen.
% Verwijder deze lijst met aandachtspunten alvorens je oplossing in te dienen.
% 
% ^^^^^^^^^^^^^^^ TODO TODO TODO ^^^^^^^^^^^^^^^
%

:- ensure_loaded(puzzels).
:- ensure_loaded(visualisatie).

% solve/2
solve(PuzzleId, Solution) :-
    puzzle(PuzzleId, Grid, Links),
    arukone(Grid, Links, Solution).

% arukone/3 
%
% Cut at the end after finding a path because we could assume that every puzzle
% has a unique solution. Backtracking is thus guaranteed to be useless.
arukone(Grid, Links, Solution) :-
    init_occupied(Links, Grid, Occupied),
    sort_links(Links, Grid, OrderedLinks),
    find_paths(Grid, OrderedLinks, Occupied, Solution),
    !.

find_paths(_, [], _, []).
find_paths(Grid, [link(Type, From, To)|Links], Occupied, [connects(Type, Path)|Paths]) :-
    find_path(Grid, From, To, Occupied, Path),
    find_paths(Grid, Links, Occupied, Paths).

find_path(Grid, From, To, _, [From, To]) :-
    neighbours(From, To, Grid).
find_path(Grid, From, To, Occupied, [From|SubPath]) :-
    \+ neighbours(From, To, Grid),
    best_neighbour(From, To, Grid, Occupied, Node),
    set_occupied(Node, Grid, Occupied),
    find_path(Grid, Node, To, Occupied, SubPath).

best_neighbour(From, To, Grid, Occupied, Neighbour) :-
    sorted_neighbours(From, To, Grid, Occupied, Neighbours),
    member((From, To, Grid, Neighbour), Neighbours).

sorted_neighbours(From, To, Grid, Occupied, Neighbours) :-
    findall((From, To, Grid, Neighbour),
            (neighbours(From, Neighbour, Grid), \+ is_occupied(Neighbour, Grid, Occupied)),
            UnsortedNeighbours),
    order_neighbours(UnsortedNeighbours, Neighbours).


init_occupied(Links, Grid, Occupied) :-
    create_occupied(Grid, Occupied),
    fill_occupied(Links, Grid, Occupied).

create_occupied(grid(N, M), Occupied) :-
    Size is N * M,
    functor(Occupied, occupied, Size).

fill_occupied([], _, _).
fill_occupied([link(_, First, Second) | Links], Grid, Occupied) :-
    set_occupied(First, Grid, Occupied),
    set_occupied(Second, Grid, Occupied),
    fill_occupied(Links, Grid, Occupied).

set_occupied(pos(R, C), grid(N, _), Occupied) :-
    Idx is ((R - 1) * N) + C,
    arg(Idx, Occupied, true).

is_occupied(pos(R, C), grid(_, N), Occupied) :-
    Idx is ((R - 1) * N) + C,
    arg(Idx, Occupied, Result),
    Result == true.


neighbours(pos(A, B), pos(C, B), _) :-
    A > 1,
    C is A - 1.
neighbours(pos(A, B), pos(C, B), grid(N, _)) :-
    A < N,
    C is A + 1.
neighbours(pos(A, B), pos(A, C), _) :-
    B > 1,
    C is B - 1.
neighbours(pos(A, B), pos(A, C), grid(_, N)) :-
    B < N,
    C is B + 1.


%%%%%%%%% Orderings %%%%%%%%%

sort_links(Links, Grid, OrderedLinks) :-
    findall((Link, Grid), member(Link, Links), NewLinks),
    predsort(link_ordering, NewLinks, TmpOrderedLinks),
    findall(Link, member((Link, Grid), TmpOrderedLinks), OrderedLinks).

link_ordering(Delta, Tuple1, Tuple2) :-
        Preds = [all_on_border_link_ordering, longest_paths_first_ordering],
            combine(Preds, Tuple1, Tuple2, Delta).

combine([Pred|Preds], First, Second, Delta) :-
    Term =.. [Pred, NewDelta, First, Second],
    call(Term),
    ( NewDelta == (=) ->
        combine(Preds, First, Second, Delta)
    ;
        Delta = NewDelta
    ).

all_on_border_link_ordering(<, (Link, Grid), _) :-
    all_on_border(Link, Grid).
all_on_border_link_ordering(>, (Link1, Grid), (Link2, Grid)) :-
    \+ all_on_border(Link1, Grid),
    all_on_border(Link2, Grid).
all_on_border_link_ordering(=, (Link1, Grid), (Link2, Grid)) :-
    \+ all_on_border(Link1, Grid),
    \+ all_on_border(Link2, Grid).

all_on_border(link(_, From, To), Grid) :-
    on_border(From, Grid),
    on_border(To, Grid).

longest_paths_first_ordering(Delta,
                             (link(_, pos(R1, C1), pos(R2, C2)), _),
                             (link(_, pos(R3, C3), pos(R4, C4)), _)) :-
    Diff1 is abs(R1 - R2) + abs(C1 - C2),
    Diff2 is abs(R3 - R4) + abs(C3 - C4),
    compare_no_equals(Delta, Diff2, Diff1).



along_border_ordering(Delta, (From, To, Grid, _), (From, To, Grid, Second)) :-
    ( on_border(Second, Grid) ->
        Delta = >
    ;
        Delta = <
    ).

on_border(pos(1, _), _).
on_border(pos(R, 1), _) :- 
    R \== 1.
on_border(pos(N, C), grid(N, _)) :-
    C \== 1.
on_border(pos(R, M), grid(N, M)):- 
    R \== 1,
    R \== N.
   
compare_no_equals(Delta, A, B) :-
    ( A < B ->
        Delta = <
    ;
        Delta = >
    ).

order_neighbours(Neighbours, OrderedNeighbours) :-
    predsort(along_border_ordering, Neighbours, OrderedNeighbours).





%%%%%%%%% Unused %%%%%%%%%%
/*
shortest_paths_first(Delta, link(_, pos(R1, C1), pos(R2, C2)), link(_, pos(R3, C3), pos(R4, C4))) :-
    Diff1 is abs(R1 - R2) + abs(C1 - C2),
    Diff2 is abs(R3 - R4) + abs(C3 - C4),
    compare_no_equals(Delta, Diff1, Diff2).

current_ordering(Delta, A, B) :-
    %orig_ordering(Delta, A, B).
    along_border_ordering(Delta, A, B).
    %random_ordering(Delta, A, B).
    %towards_goal_ordering(Delta, A, B).
    %towards_goal2(Delta, A, B).
    %combined(Delta, A, B).
    

orig_ordering(<, _, _).

% towards_goal_ordering(Delta, (From, To, Grid, Node1), (From, To, Grid, Node2))
% is an ordering key to be used with predsort, it returns < if Node1 is closer
% to To than Node2.
towards_goal_ordering(Delta, (_, Goal, _, pos(R1, C1)), (_, Goal, _, pos(R2, C2))) :-
    Goal = pos(RG, CG),
    Diff1 = abs(RG - R1) + abs(CG - C1),
    Diff2 = abs(RG - R2) + abs(CG - C2),
    compare_no_equals(Delta, Diff1, Diff2).

towards_goal2(Delta, (From, To, _, pos(_, C1)), (From, To, _, pos(_, C2))) :-
    From = pos(R, _),
    To = pos(R, C),
    Length1 is abs(C - C1),
    Length2 is abs(C - C2),
    compare_no_equals(Delta, Length1, Length2).
towards_goal2(Delta, (From, To, _, pos(R1, _)), (From, To, _, pos(R2, _))) :-
    From = pos(_, C),
    To = pos(R, C),
    Length1 is abs(R - R1),
    Length2 is abs(R - R2),
    compare_no_equals(Delta, Length1, Length2).
towards_goal2(Delta, (From, To, _, pos(R1, C1)), (From, To, _, pos(R2, C2))) :-
    From = pos(FromR, FromC),
    To = pos(R, C),
    R =\= FromR,
    C =\= FromC,
    Length1 is abs(R - R1) + abs(C - C1),
    Length2 is abs(R - R2) + abs(C - C2),
    compare_no_equals(Delta, Length1, Length2).


random_ordering(Delta, _, _) :-
    ( 0 =:= random(2) ->
        Delta = <
    ;
        Delta = >
    ).

combined(<, (_, _, Grid, First), _) :-
    on_border(First, Grid).
combined(>, (From, To, Grid, First), (From, To, Grid, Second)) :-
    \+ on_border(First, Grid),
    on_border(Second, Grid).
combined(Delta, Node1, Node2) :-
    Node1 = (_, _, Grid, First),
    Node2 = (_, _, Grid, Second),
    \+ on_border(First, Grid),
    \+ on_border(Second, Grid),
    towards_goal_ordering(Delta, Node1, Node2).
*/

