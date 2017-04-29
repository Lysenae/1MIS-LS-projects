% Projekt: FLP #2 - Turingov Stroj
% Autor:   Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

:- dynamic rule/4.

main :-
  parse_stdin(Tape),
  write("Tape: "),
  writeln(Tape),
  halt.

parse_stdin(Tape) :-
  prompt(_, ''),
  read_lines(Lines),
  maplist(remove_spaces, Lines, CleanLines),
  get_tape(CleanLines, Tp),
  create_rules(CleanLines),
  Tape = Tp.

remove_spaces([], []).
remove_spaces([HL|TL], R) :-
  (HL == ' ', remove_spaces(TL, R));
  (HL \== ' ', remove_spaces(TL, Rs), R = [HL|Rs]).

get_tape(In, Tape) :-
  last(In, Tp),
  Tape = Tp.

create_rules(In) :-
  last(In, Last),         % Odstran pasku
  delete(In, Last, In2),
  create_rules2(In2).

create_rules2([]).
create_rules2([H|T]) :-
  write("Creating rule for: "),
  writeln(H),
  nth0(0, H, SttP), % StatePresent  - sucasny stav
  nth0(1, H, SymP), % SymbolPresent - sucasny synbol
  nth0(2, H, SttN), % StateNew      - novy stav
  nth0(3, H, SymN), % SymbolNew     - novy symbol, resp. posun
  assert(rule(SttP, SymP, SttN, SymN)),
  create_rules2(T).

% ##############################################################################
% Tato cast je prebrana z input2.pl
% ##############################################################################

%Reads line from stdin, terminates on LF or EOF.
read_line(L, C) :-
  get_char(C),
  (isEOFEOL(C), L = [], !;
    read_line(LL, _),% atom_codes(C,[Cd]),
    [C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
  C == end_of_file;
  (char_code(C, Code), Code==10).

read_lines(Ls) :-
  read_line(L, C),
  ( C == end_of_file, Ls = [] ;
    read_lines(LLs), Ls = [L|LLs]
  ).

% ##############################################################################
