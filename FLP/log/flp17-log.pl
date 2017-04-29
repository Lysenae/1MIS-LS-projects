% Projekt: FLP #2 - Turingov Stroj
% Autor:   Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

:- dynamic rule/4.

% Vstupny bod programu
main :-
  parse_stdin(Config),
  print_config(Config),
  config_props(Config, S, L, R, N),
  write("Position: "),writeln(N),
  write("L: "),writeln(L),
  write("R: "),writeln(R),
  write("State: "),writeln(S),
  halt.

% Spracovanie nacitanych udajov - zostavenie pasky a pravidiel TS
parse_stdin(Config) :-
  prompt(_, ''),
  read_lines(Lines),
  maplist(remove_spaces, Lines, CleanLines),
  get_config(Lines, C),
  create_rules(CleanLines),
  Config = C.

% Odstrani medzery zo zoznamu
remove_spaces([], []).
remove_spaces([HL|TL], R) :-
  (HL == ' ', remove_spaces(TL, R));
  (HL \== ' ', remove_spaces(TL, Rs), R = [HL|Rs]).

% Ziska pociatocnu konfiguraciu TS
get_config(In, Config) :-
  last(In, C),
  Config = ['S'|C]. % Nastavi pociatocny stav

% Zostavi pravidla
create_rules(In) :-
  last(In, Last),
  delete(In, Last, In2), % Odstran pasku
  create_rules2(In2).

create_rules2([]).
create_rules2([H|T]) :-
  nth0(0, H, SttP), % StatePresent  - sucasny stav
  nth0(1, H, SymP), % SymbolPresent - sucasny synbol
  nth0(2, H, SttN), % StateNew      - novy stav
  nth0(3, H, SymN), % SymbolNew     - novy symbol, resp. posun
  assert(rule(SttP, SymP, SttN, SymN)),
  create_rules2(T).

% Vypise aktualnu konfiguraciu
print_config(Config) :-
  atomic_list_concat(Config, '', S),
  atom_string(S, R),
  writeln(R).

% Najde aktualnu poziciu hlavy TS
pos(Config, Pos) :-
  length(Config, Len),
  L is Len - 1,
  pos2(Config, L, P),
  Pos is P.

% Vyhlada velke pismenu, ktore znaci poziciu hlavy, ak ho najde ulozi jeho
% poziciu do OutPos.
pos2(Config, CurPos, OutPos) :-
  nth0(CurPos, Config, N),
  ( (char_type(N, upper), OutPos is CurPos);
    ( NewPos is CurPos - 1,
      pos2(Config, NewPos, OutPos)
    )
  ).

state(Config, State) :-
  pos(Config, Pos),
  nth0(Pos, Config, State).

% Ziska retazec pred (L) a za (R) hlavou TS
split_config([],[],[]).
split_config([H|T],L,R) :- char_type(H, upper),R = T, L = [].
split_config([H|T],L,R) :-
  \+ char_type(H, upper),
  split_config(T,X,R),
  append([H],X,L).

% Ziska vlastnosti konfiguracie - stav, retazec pred a za hlavou, poziciu
config_props(Config, State, Left, Right, N) :-
  pos(Config, N),
  state(Config, State),
  split_config(Config, Left, Right).

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
