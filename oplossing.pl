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
    init_occupied(Links, Occupied),
    order_links(Links, OrderedLinks),
    find_paths(Grid, OrderedLinks, Occupied, Solution),
    !.

find_paths(_, [], _, []).
find_paths(Grid, [link(Type, From, To)|Links], Occupied, [connects(Type, Path)|Paths]) :-
    find_path(Grid, From, To, Occupied, Path, NewOccupied),
    find_paths(Grid, Links, NewOccupied, Paths).

find_path(Grid, From, To, Occupied, [From, To], Occupied) :-
    neighbours(From, To, Grid).
find_path(Grid, From, To, Occupied, [From|SubPath], NewOccupied) :-
    \+ neighbours(From, To, Grid),
    best_neighbour(From, To, Grid, Occupied, Node),
    set_occupied(Node, Occupied, Occupied2),
    find_path(Grid, Node, To, Occupied2, SubPath, NewOccupied).


best_neighbour(From, To, Grid, Occupied, Neighbour) :-
    sorted_neighbours(From, To, Grid, Occupied, Neighbours),
    member((From, To, Grid, Neighbour), Neighbours).

sorted_neighbours(From, To, Grid, Occupied, Neighbours) :-
    findall((From, To, Grid, Neighbour),
            (neighbours(From, Neighbour, Grid), \+ is_occupied(Neighbour, Occupied)),
            UnsortedNeighbours),
    order_neighbours(UnsortedNeighbours, Neighbours).

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



init_occupied(Links, Occupied) :-
    fill_occupied(Links, [], Occupied).

fill_occupied([], Occupied, Occupied).
fill_occupied([link(_, First, Second) | Links], Occupied, NewOccupied) :-
    set_occupied(First, Occupied, Occupied2),
    set_occupied(Second, Occupied2, Occupied3),
    fill_occupied(Links, Occupied3, NewOccupied).

is_occupied(pos(R, C), [(Row, Columns) | Tail]) :-
    ( R == Row ->
        memberchk(C, Columns)
    ;
        is_occupied(pos(R, C), Tail)
    ).

set_occupied(Pos, Occupied, NewOccupied) :-
    set_occupied(Pos, Occupied-[], A-A, NewOccupied).

set_occupied(pos(R, C), A-A, Acc-ATail, NewOccupied) :-
    [(R, [C])|Acc]-ATail = NewOccupied-[].
set_occupied(pos(R, C), [(Row, Columns)|Occupied]-OTail, Acc-ATail, NewOccupied) :-
    ( R == Row ->
        NewColumns = [C|Columns],
        ATail = [(R, NewColumns)|Occupied],
        Acc-OTail = NewOccupied-[]
    ;
        set_occupied(pos(R, C), Occupied-OTail, [(Row, Columns)|Acc]-ATail, NewOccupied)
    ).


%%%%%%%%% Orderings %%%%%%%%%

order_links(Links, OrderedLinks) :-
    predsort(longest_paths_first, Links, OrderedLinks).

longest_paths_first(Delta, link(_, pos(R1, C1), pos(R2, C2)), link(_, pos(R3, C3), pos(R4, C4))) :-
    Diff1 is abs(R1 - R2) + abs(C1 - C2),
    Diff2 is abs(R3 - R4) + abs(C3 - C4),
    compare_no_equals(Delta, Diff2, Diff1).

order_neighbours(Neighbours, OrderedNeighbours) :-
    predsort(along_border_ordering, Neighbours, OrderedNeighbours).

along_border_ordering(Delta, (From, To, Grid, _), (From, To, Grid, Second)) :-
    ( on_border(Second, Grid) ->
        Delta = >
    ;
        Delta = <
    ).

on_border(pos(1, _), _).
on_border(pos(R, 1), _) :- 
    R =\= 1.
on_border(pos(N, C), grid(N, _)) :-
    C =\= 1.
on_border(pos(R, M), grid(N, M)):- 
    R =\= 1,
    R =\= N.

compare_no_equals(Delta, A, B) :-
    ( A < B ->
        Delta = <
    ;
        Delta = >
    ).

