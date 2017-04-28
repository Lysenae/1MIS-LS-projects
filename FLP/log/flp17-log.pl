% Projekt: FLP #2 - Turingov Stroj
% Autor:   Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

main(_Argv) :-
  prompt(_, ''),
  read_lines(LL),
  split_lines(LL,S),
  write(S),
  halt.

% ##############################################################################
% Tato cast je prebrana z input2.pl
% ##############################################################################

%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
  get_char(C),
  (isEOFEOL(C), L = [], !;
    read_line(LL,_),% atom_codes(C,[Cd]),
    [C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
  C == end_of_file;
  (char_code(C,Code), Code==10).

read_lines(Ls) :-
  read_line(L,C),
  ( C == end_of_file, Ls = [] ;
    read_lines(LLs), Ls = [L|LLs]
  ).

% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
% aby to fungovalo i s retezcem na miste seznamu
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).
% G je prvni seznam ze seznamu seznamu G|S1
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]).

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

% ##############################################################################
