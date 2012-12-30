% Deze code visualiseert de puzzels en oplossingen.

% show_puzzle/1
% Dit predicaat toont de opgave van de gegeven puzzel.
show_puzzle(PuzzleId) :-
    puzzle(PuzzleId,Grid,Links),
    generateEmptyGrid(Grid,Array),
    addLinksToGrid(Links,Array),
    showGrid(Array).

% show_solution/1
% Dit predicaat lost de gegeven puzzel op en toont de oplossing.
show_solution(PuzzleId) :-
    puzzle(PuzzleId,Grid,_),
    generateEmptyGrid(Grid,Array),
    solve(PuzzleId,Solution),
    addSolutionToGrid(Solution,Array),
    showGrid(Array).

% generateEmptyGrid/2
generateEmptyGrid(grid(H,W),Array) :-
    length(Array,H),
    maplist(lengthReverse(W),Array).

% lengthReverse/2
lengthReverse(Length,List) :-
    length(List,Length).

% addLinksToGrid/2
addLinksToGrid([],_).

% addLinksToGrid/2
addLinksToGrid([link(Label,pos(R1,C1),pos(R2,C2))|RestOfLinks],Rows) :-
    nth1(R1,Rows,Row1),
    nth1(C1,Row1,Label),
    nth1(R2,Rows,Row2), 
    nth1(C2,Row2,Label),
    addLinksToGrid(RestOfLinks,Rows).

% addSolutionToGrid/2
addSolutionToGrid([],_).

% addSolutionToGrid/2
addSolutionToGrid([connects(_,[])|RestOfSolution],Rows) :-
    addSolutionToGrid(RestOfSolution,Rows).

% addSolutionToGrid/2
addSolutionToGrid([connects(Label,[pos(R,C)|RestOfPath])|RestOfSolution],Rows) :-
    nth1(R,Rows,Row),
    nth1(C,Row,Label),
    addSolutionToGrid([connects(Label,RestOfPath)|RestOfSolution],Rows).

% showCell/1
showCell(Cell) :-
    ( var(Cell) -> 
        write(' _')
    ; 
        writef(' %w',[Cell])
    ).

% showRow/1
showRow(Row) :-
    maplist(showCell,Row), 
    nl.

% showGrid/1
showGrid(Rows) :-
    nl, 
    maplist(showRow,Rows), 
    nl.

