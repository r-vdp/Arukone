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

% arukone/3 - zelf te implementeren!  
arukone(grid(N, N), Links, Solution) :-
    occupy_initial_values(Links, Occupied),
    find_paths(N, Links, Occupied, Solution).

occupy_initial_values([], []).
occupy_initial_values([link(_, Pos1, Pos2) | Links], [Pos1, Pos2 | Tail]) :-
    occupy_initial_values(Links, Tail).

find_path(From, To, Occupied, N, [From, To], Occupied) :-
    neighbours(From, To, N).
find_path(From, To, Occupied, N, [From|SubPath], NewOccupied) :-
    neighbours(From, Node, N),
    \+ member(Node, Occupied),
    find_path(Node, To, [Node|Occupied], N, SubPath, NewOccupied).
    
find_paths(_, [], _, []).
find_paths(N, [link(Type, From, To)|Links], Occupied, [connects(Type, Path)|Paths]) :-
    find_path(From, To, Occupied, N, Path, NewOccupied),
    find_paths(N, Links, NewOccupied, Paths).

neighbours(pos(A, B), pos(C, B), _) :-
    A > 1,
    C is A - 1.
neighbours(pos(A, B), pos(C, B), N) :-
    A < N,
    C is A + 1.
neighbours(pos(A, B), pos(A, C), _) :-
    B > 1,
    C is B - 1.
neighbours(pos(A, B), pos(A, C), N) :-
    B < N,
    C is B + 1.

    
