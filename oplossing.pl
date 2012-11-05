% Ramses De Norre
% Andere

% Oplossingsstrategie: de basisstrategie bestaat eruit om voor elk getal een pad
%                      te zoeken tussen de twee voorkomens van dat getal zonder
%                      gebruik te maken van "occupied" posities, i.e. posities
%                      die reeds zijn ingenomen door andere paden, inclusief de
%                      gegeven getallen.
%                      Wanneer zo'n pad niet kan gevonden worden, worden via
%                      backtracking alternatieve paden uitgeprobeerd totdat voor
%                      elk getal een pad gevonden is.
%
% Heuristieken:        Heuristieken kunnen op twee plaatsen toegepast worden,
%                      bij het ordenen van de links in het begin om te beslissen
%                      in welke volgorde er voor de verschillende getallen paden
%                      worden gezocht en bij het beslissen in welke volgorde de
%                      verschillende mogelijke richtingen om een pad te
%                      verlengen, geprobeerd worden.
%
%                      Tijdens het experimenteren bleek dat de overhead van een
%                      te complexe heuristiek voor het verlengen van een pad al
%                      snel nadelige gevolgen had op de performantie, dus ik heb
%                      een redelijk eenvoudige heuristiek gebruikt (eerst langs
%                      de kanten proberen paden op te bouwen) die goed
%                      samenwerkt met de heuristiek om de links te ordenen.
%
%                      Voor de links kon een duurdere heuristiek toegepast
%                      worden omdat deze heursitiek slechts eenmaal gebruikt
%                      wordt om een lijst te ordenen. Hier bleek uit
%                      experimenten dat een gecombineerd criterium het beste
%                      resultaat gaf: ik neem eerst links waarvan beide posities
%                      op de rand van de puzzel liggen, vervolgens sorteer ik de
%                      andere links volgens de afstand tussen hun posities
%                      waarbij links met een grotere afstand eerst worden bekeken.
%
% Optimalisaties:      Naast de heuristieken heb ik aandacht besteed aan de
%                      datastructuur voor de "occupied" posities, daar profiling
%                      aanwees dat een simpele lineaire lijst van bezette
%                      posities als gevolg had dat bijna 50% van de
%                      uitvoeringstijd aan occupied-checks besteed werd.
%                      De alternatieven waren:
%                        * een array structuur met directed access simuleren met
%                          functor/arg predicaten (zie Bratko sectie 8.5.4).
%                          Dit was de snelste structuur maar omdat dit niet zo'n
%                          idiomatische prolog code is, heb ik hier toch niet
%                          voor geopteerd.
%                        * de assoc library:
%                            http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%27A.3%27,swi%28%27/doc/Manual/assoc.html%27%29%29
%                          deze gaf, ondanks log(n) complexiteit, echter zeer
%                          veel overhead.
%                        * een zelf geimplementeerde structuur bestaande uit een
%                          lijst van paren waarvan de eerste component een
%                          rijnummer is en de tweede een lijst van de
%                          kolomnummers die bezet zijn in die rij.
%                          Deze datastructuur heeft lineaire complexiteit voor
%                          zowel updates als insertions.

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
    sort_links(Links, Grid, OrderedLinks),
    find_paths(Grid, OrderedLinks, Occupied, Solution),
    !.

% find_paths(Grid, Links, Occupied, Paths) succeeds if Paths is a list of
% connects terms which represent the paths needed to connect all links from
% Links in a puzzle where the positions contained in Occupied are already
% occupied and which has a size described by Grid.
find_paths(_, [], _, []).
find_paths(Grid, [link(Type, From, To)|Links], Occupied, [connects(Type, Path)|Paths]) :-
    find_path(Grid, From, To, Occupied, Path, NewOccupied),
    find_paths(Grid, Links, NewOccupied, Paths).

% find_path(Grid, From, To, Occupied, Path, NewOccupied) is a wrapper around the
% do_find_path predicate which checks whether To lies on the border of the
% puzzle and if so, swaps From and To before calling do_find_path.
find_path(Grid, From, To, Occupied, Path, NewOccupied) :-
    ( on_border(To, Grid) ->
        do_find_path(Grid, To, From, Occupied, [], Path, NewOccupied)
    ;
        do_find_path(Grid, From, To, Occupied, [], Path, NewOccupied)
    ).

% do_find_path(Grid, From, To, Occupied, PathAcc, Path, NewOccupied) succeeds
% if Path is the concatenation of the path contained in PathAcc and a path
% from From to To in a puzzle described by Grid and if that path does not use
% any position included in Occupied.
% NewOccupied is the new list of occupied positions which is the concatenation
% of Occupied and the positions used by Path.
do_find_path(Grid, From, To, Occupied, PathAcc, [To,From|PathAcc], Occupied) :-
    neighbours(From, To, Grid).
do_find_path(Grid, From, To, Occupied, PathAcc, Path, NewOccupied) :-
    \+ neighbours(From, To, Grid),
    best_neighbour(From, Grid, Occupied, Node),
    set_occupied(Node, Occupied, Occupied2),
    do_find_path_acc(Grid, Node, To, Occupied2, [From|PathAcc], Path, NewOccupied).

/*
do_find_path(Grid, From, To, Occupied, [From, To], Occupied) :-
    neighbours(From, To, Grid).
do_find_path(Grid, From, To, Occupied, [From|SubPath], NewOccupied) :-
    \+ neighbours(From, To, Grid),
    best_neighbour(From, Grid, Occupied, Node),
    set_occupied(Node, Occupied, Occupied2),
    do_find_path(Grid, Node, To, Occupied2, SubPath, NewOccupied).
*/

%%%%%%%%% Neighbours %%%%%%%%%

% best_neighbour(From, Grid, Occupied, Neighbour) succeeds if Neighbours is the
% best neighbour, i.e. the head of the result of sorted_neighbours, in the
% puzzle described by Grid which is not contained in Occupied.
best_neighbour(From, Grid, Occupied, Neighbour) :-
    sorted_neighbours(From, Grid, Occupied, Neighbours),
    member((Grid, Neighbour), Neighbours).

% sorted_neighbours(From, Grid, Occupied, Neighbours) succeeds if Neighbours is
% a list of all neighbours of From in the puzzle described by Grid and which are
% not a member of Occupied, sorted by the sort_neighbours predicate.
sorted_neighbours(From, Grid, Occupied, Neighbours) :-
    findall((Grid, Neighbour),
            (neighbours(From, Neighbour, Grid), \+ is_occupied(Neighbour, Occupied)),
            UnsortedNeighbours),
    sort_neighbours(UnsortedNeighbours, Neighbours).

% neighbours(Pos1, Pos2, Grid) succeeds if Pos1 is a neighbour of Pos2 in the
% puzzle described by Grid.
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


%%%%%%%%% Occupied Positions %%%%%%%%%
% The occupied positions are kept as a list of pairs for which the first
% component is a row number and the second is a list of the columns which
% are occupied in that row.
% This structure enables linear time look-ups and insertions.

% init_occupied(Links, Occupied) succeeds if Occupied contains all positions
% which are contained in the elements of Links.
init_occupied(Links, Occupied) :-
    fill_occupied(Links, [], Occupied).

% fill_occupied(Links, Occupied, NewOccupied) succeeds if NewOccupied contains
% all positions included in either Occupied or an element of Links.
fill_occupied([], Occupied, Occupied).
fill_occupied([link(_, First, Second) | Links], Occupied, NewOccupied) :-
    set_occupied(First, Occupied, Occupied2),
    set_occupied(Second, Occupied2, Occupied3),
    fill_occupied(Links, Occupied3, NewOccupied).

% is_occupied(Pos, Occupied) succeeds if Pos in contained in Occupied.
is_occupied(pos(R, C), [(Row, Columns) | Tail]) :-
    ( R == Row ->
        memberchk(C, Columns)
    ;
        is_occupied(pos(R, C), Tail)
    ).

% set_occupied(Pos, Occupied, NewOccupied) succeeds if NewOccupied contains all
% elements of Occupied and the additional element Pos.
set_occupied(Pos, Occupied, NewOccupied) :-
    set_occupied(Pos, Occupied-[], A-A, NewOccupied).

% set_occupied(Pos, Occupied, Accumulator, NewOccupied) succeeds if NewOccupied
% contains all elements of Occupied and Accumulator and the position Pos.
% The base case is only reached if the row of Pos was not yet contained in
% Occupied, a new row tuple is added then.
% If the row was already present, the recursion can be stopped when the tuple
% containing the row was found and the remaining part of Occupied can then be
% added to Accumulator in constant time.
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

% sort_links(Links, Grid, OrderedLinks) succeeds if OrderedLinks contains all
% elements of Links and they are ordered with respect to the ordering defined
% by link_ordering.
% The findall clauses are used to add and remove the Grid variable from the link
% lists such that the Grid variable is available during sorting.
sort_links(Links, Grid, OrderedLinks) :-
    findall((Link, Grid), member(Link, Links), NewLinks),
    predsort(link_ordering, NewLinks, TmpOrderedLinks),
    findall(Link, member((Link, Grid), TmpOrderedLinks), OrderedLinks).

% link_ordering(Delta, Tupl1, Tuple2) is used as the ordening predicate for
% predsort, it succeeds if Delta represents the ordering of Tuple1 and Tuple2.
% This predicate should never unify Delta with (=) because predsort removes
% duplicates.
link_ordering(Delta, Tuple1, Tuple2) :-
    Preds = [all_on_border_link_ordering, longest_paths_first_ordering],
    combine(Preds, Tuple1, Tuple2, Delta).

% combine(Preds, First, Second, Delta) unifies Delta with the resulting Delta
% of the first element of Preds which does not give (=) when called with First
% and Second as arguments.
% It is mandatory that the last element of Preds guarantees to never unify Delta
% to (=), this guarantees that the result of combine can be safely used with
% predsort without losing duplicates and errors against this requirement are
% easily detected because the predicate will fail.
combine([Pred|Preds], First, Second, Delta) :-
    Term =.. [Pred, NewDelta, First, Second],
    call(Term),
    ( NewDelta == (=) ->
        combine(Preds, First, Second, Delta)
    ;
        Delta = NewDelta
    ).
% all_on_border_link_ordering(Delta, Link1, Link2) succeeds if Delta labels a
% link of which both positions lie on the border of the puzzle described by Grid
% as smaller then another link.
% If both links don't have both their positions lying on the border, the links
% are considered to be equal.
all_on_border_link_ordering(<, (Link, Grid), _) :-
    all_on_border(Link, Grid).
all_on_border_link_ordering(>, (Link1, Grid), (Link2, Grid)) :-
    \+ all_on_border(Link1, Grid),
    all_on_border(Link2, Grid).
all_on_border_link_ordering(=, (Link1, Grid), (Link2, Grid)) :-
    \+ all_on_border(Link1, Grid),
    \+ all_on_border(Link2, Grid).

% all_on_border(Link, Grid) succeeds if both positions of Link are lying on the
% border of the puzzle described by Grid.
all_on_border(link(_, From, To), Grid) :-
    on_border(From, Grid),
    on_border(To, Grid).

% longest_paths_first_ordering(Delta, Link1, Link2) succeeds if Delta labels the
% link with the longest distance between its two positions as the smallest of
% the two supplied links.
% This ordering guarantees to never unify Delta with (=) such that it can be
% safely used with predsort without losing duplicates.
longest_paths_first_ordering(Delta,
                             (link(_, pos(R1, C1), pos(R2, C2)), _),
                             (link(_, pos(R3, C3), pos(R4, C4)), _)) :-
    Diff1 is abs(R1 - R2) + abs(C1 - C2),
    Diff2 is abs(R3 - R4) + abs(C3 - C4),
    compare_no_equals(Delta, Diff2, Diff1).

% compare_no_equals(Delta, A, B) succeeds if Delta gives the correct ordering of
% A and B and is never unified with (=).
compare_no_equals(Delta, A, B) :-
    ( A < B ->
        Delta = <
    ;
        Delta = >
    ).

% sort_neighbours(Neighbours, OrderedNeighbours) succeeds if OrderedNeighbours
% contains all elements of Neighbours, ordered by the along_border_ordering
% predicate.
sort_neighbours(Neighbours, OrderedNeighbours) :-
    predsort(along_border_ordering, Neighbours, OrderedNeighbours).

% along_border_ordering(Delta, (Grid, First), (Grid, Second)) succeeds if Delta
% is (>) if Second is a position on the border of the puzzle described by Grid,
% (<) otherwise.
along_border_ordering(Delta, (Grid, _), (Grid, Second)) :-
    ( on_border(Second, Grid) ->
        Delta = >
    ;
        Delta = <
    ).

% on_border(Pos, Grid) succeeds if Pos lies on the border of the puzzle
% described by Grid.
on_border(pos(1, _), _).
on_border(pos(R, 1), _) :- 
    R =\= 1.
on_border(pos(N, C), grid(N, _)) :-
    C =\= 1.
on_border(pos(R, M), grid(N, M)):- 
    R =\= 1,
    R =\= N.

