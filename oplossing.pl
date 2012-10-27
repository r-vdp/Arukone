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
    to_occupied(Links, Occupied),
    find_paths(Grid, Links, Occupied, Solution),
    !.

% to_occupied(Links, Occupied) succeeds if Occupied is a list of pos terms
% consisting of the two positions in each link term that is contained in Links.
to_occupied([], []).
to_occupied([link(_, Pos1, Pos2) | Links], [Pos1, Pos2 | Tail]) :-
    to_occupied(Links, Tail).

find_paths(_, [], _, []).
find_paths(Grid, [link(Type, From, To)|Links], Occupied, [connects(Type, Path)|Paths]) :-
    find_path(Grid, From, To, Occupied, Path, NewOccupied),
    find_paths(Grid, Links, NewOccupied, Paths).

find_path(Grid, From, To, Occupied, [From, To], Occupied) :-
    neighbours(From, To, Grid).
find_path(Grid, From, To, Occupied, [From|SubPath], NewOccupied) :-
    best_neighbour(From, To, Grid, Node),
    \+ member(Node, Occupied),
    find_path(Grid, Node, To, [Node|Occupied], SubPath, NewOccupied).

best_neighbour(From, To, Grid, Neighbour) :-
    sorted_neighbours(From, To, Grid, Neighbours),
    member((To, Neighbour), Neighbours).

sorted_neighbours(From, To, Grid, Neighbours) :-
    findall((To, Neighbour), neighbours(From, Neighbour, Grid), UnsortedNeighbours),
    predsort(towards_goal_ordering, UnsortedNeighbours, Neighbours).

towards_goal_ordering(Delta, (To, pos(TR1, TC1)), (To, pos(TR2, TC2))) :-
    To = pos(TR, TC),
    (abs(TR - TR1) + abs(TC - TC1) < abs(TR - TR2) + abs(TC - TC2) ->
        Delta = <
    ;
        Delta = >
    ).

orig_ordering(<, _, _).

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

    
