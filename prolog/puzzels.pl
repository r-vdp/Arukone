% Voorstelling van de puzzels:
% puzzle(PuzzleId,grid(NumberOfRows,NumberOfColumns), link(Label,pos(Row1,Column1),pos(Row2,Column2)), ...])
 
% Voorstelling van de oplossingen:
% [connects(Label1,[pos(Row1,Column1), ..., pos(RowN,ColumnN)]), ...]


puzzle(-1, grid(3,3), [
    link(1, pos(2,1), pos(3,2)),
    link(2, pos(1,1), pos(3,3))
]).

%  0: 5x5 met 4 paden - voorbeeld uit opgave (afkomstig van http://www.menneske.no/arukone/eng/)
puzzle(0,grid(5,5),[
    link(1, pos(2,1), pos(5,5)),
    link(2, pos(1,5), pos(3,2)),
    link(3, pos(1,3), pos(3,3)),
    link(4, pos(1,1), pos(2,3))
]).

%  1: 5x5 met 4 paden (afkomstig van http://www.menneske.no/arukone/eng/)
puzzle(1,grid(5,5),[
    link(1,pos(2,1),pos(4,4)),
    link(2,pos(1,1),pos(2,2)),
    link(3,pos(2,4),pos(5,4)),
    link(4,pos(4,2),pos(2,5))
]).

%  2: 5x5 met 3 paden (afkomstig van http://www.menneske.no/arukone/eng/)
puzzle(2,grid(5,5),[
    link(1,pos(5,1),pos(5,4)),
    link(2,pos(2,4),pos(5,5)),
    link(3,pos(2,2),pos(1,4))
]).

%  3: 6x6 met 5 paden (afkomstig van Flow Free)
puzzle(3,grid(6,6),[
    link(1,pos(1,3),pos(3,1)),
    link(2,pos(2,2),pos(5,5)),
    link(3,pos(2,3),pos(3,6)),
    link(4,pos(2,5),pos(5,1)),
    link(5,pos(5,2),pos(6,1))
]).

%  4: 7x7 met 7 paden (afkomstig van Flow Free)
puzzle(4,grid(7,7),[
    link(1,pos(1,1),pos(5,1)),
    link(2,pos(1,3),pos(1,5)),
    link(3,pos(1,6),pos(4,7)),
    link(4,pos(2,1),pos(5,3)),
    link(5,pos(2,3),pos(3,5)),
    link(6,pos(2,6),pos(4,5)),
    link(7,pos(5,2),pos(6,6))
]).

%  5: 7x7 met 6 paden (afkomstig van Flow Free)
puzzle(5,grid(7,7),[
    link(1,pos(1,7),pos(7,6)),
    link(2,pos(2,6),pos(3,2)),
    link(3,pos(2,7),pos(6,5)),
    link(4,pos(4,4),pos(5,3)),
    link(5,pos(4,5),pos(7,7)),
    link(6,pos(5,5),pos(6,6))
]).

%  6: 7x7 met 5 paden (afkomstig van Flow Free)
puzzle(6,grid(7,7),[
    link(1,pos(2,1),pos(7,1)),
    link(2,pos(3,3),pos(4,5)),
    link(3,pos(5,3),pos(6,5)),
    link(4,pos(5,5),pos(6,2)),
    link(5,pos(6,1),pos(6,6))
]).

%  7: 7x7 met 5 paden (afkomstig van Flow Free)
puzzle(7,grid(7,7),[
    link(1,pos(1,4),pos(7,7)),
    link(2,pos(1,5),pos(4,2)),
    link(3,pos(2,5),pos(3,3)),
    link(4,pos(2,6),pos(5,6)),
    link(5,pos(3,2),pos(4,4))
]).

%  8: 7x7 met 5 paden (afkomstig van Flow Free)
puzzle(8,grid(7,7),[
    link(1,pos(2,1),pos(3,2)),
    link(2,pos(2,2),pos(2,6)),
    link(3,pos(3,1),pos(5,3)),
    link(4,pos(4,2),pos(4,4)),
    link(5,pos(5,2),pos(5,6))
]).

%  9: 8x8 met 7 paden (afkomstig van Flow Free)
puzzle(9,grid(8,8),[
    link(1,pos(1,5),pos(1,7)),
    link(2,pos(1,6),pos(4,6)),
    link(3,pos(2,5),pos(7,4)),
    link(4,pos(2,7),pos(4,7)),
    link(5,pos(3,3),pos(5,3)),
    link(6,pos(5,4),pos(7,2)),
    link(7,pos(6,6),pos(7,3))
]).

% 10: 8x8 met 7 paden (afkomstig van Flow Free)
puzzle(10,grid(8,8),[
    link(1,pos(1,1),pos(7,8)),
    link(2,pos(2,7),pos(4,7)),
    link(3,pos(2,8),pos(4,2)),
    link(4,pos(3,4),pos(4,8)),
    link(5,pos(5,5),pos(7,3)),
    link(6,pos(5,8),pos(6,3)),
    link(7,pos(6,8),pos(7,4))
]).

% 11: 9x9 met 9 paden (afkomstig van http://www.menneske.no/arukone/eng/)
puzzle(11,grid(9,9),[
    link(1,pos(8,6),pos(5,8)),
    link(2,pos(2,8),pos(9,9)),
    link(3,pos(8,2),pos(3,5)),
    link(4,pos(1,5),pos(8,7)),
    link(5,pos(6,2),pos(2,3)),
    link(6,pos(1,8),pos(3,9)),
    link(7,pos(3,1),pos(2,2)),
    link(8,pos(4,3),pos(7,4)),
    link(9,pos(2,1),pos(9,4))
]). 

