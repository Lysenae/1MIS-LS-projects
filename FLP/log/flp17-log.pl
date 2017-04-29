% Projekt: FLP #2 - Turingov Stroj
% Autor:   Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

:- dynamic rule/4.

% Vstupny bod programu
main :-
  parse_stdin(Config),
  print_config(Config),
  config(Config, S, L, R, N),
  write("Position: "),writeln(N),
  write("L: "),writeln(L),
  write("R: "),writeln(R),
  write("State: "),writeln(S),
  tm_perform(Config),
  halt.

% Spracovanie nacitanych udajov - zostavenie pasky a pravidiel TS.
% Config - vystpuny parameter pre konfiguraciu TS
parse_stdin(Config) :-
  prompt(_, ''),
  read_lines(Lines),
  maplist(remove_spaces, Lines, CleanLines),
  get_config(Lines, Config),
  create_rules(CleanLines).

% Odstrani medzery zo zoznamu.
% L - zoznam
% R - vysupny parameter pre vystupny zoznam
remove_spaces([], []).
remove_spaces([HL|TL], R) :-
  (HL == ' ', remove_spaces(TL, R));
  (HL \== ' ', remove_spaces(TL, Rs), R = [HL|Rs]).

% Ziska pociatocnu konfiguraciu TS.
% In     - zoznam
% Config - vystupny parameter pre konfiguraciu
get_config(In, Config) :-
  last(In, C),
  Config = ['S'|C]. % Nastavi pociatocny stav

% Zostavi pravidla.
create_rules(In) :-
  last(In, Last),
  delete(In, Last, In2), % Odstran pasku
  create_rules2(In2).

% Zostavi pravidla zo stvorprvkoveho zoznamu.
create_rules2([]).
create_rules2([H|T]) :-
  nth0(0, H, SttP), % StatePresent  - sucasny stav
  nth0(1, H, SymP), % SymbolPresent - sucasny synbol
  nth0(2, H, SttN), % StateNew      - novy stav
  nth0(3, H, SymN), % SymbolNew     - novy symbol, resp. posun
  assert(rule(SttP, SymP, SttN, SymN)),
  create_rules2(T).

% Vypise aktualnu konfiguraciu.
% Config - konfiguracia TS
print_config(Config) :-
  atomic_list_concat(Config, '', S),
  atom_string(S, R),
  writeln(R).

% Najde aktualnu poziciu hlavy TS.
% Config - konfiguracia TS
% Pos    - vystupny parameter pre poziciu hlavy na paske
pos(Config, Pos) :-
  length(Config, Len),
  L is Len - 1,
  pos2(Config, L, P),
  Pos is P.

% Vyhlada velke pismeno, ktore znaci poziciu hlavy a vrati jeho poziciu v retazci.
% Config - konfiguracia TS
% CurPos - aktualne kontrolovana pozicia
% OutPos - vystupny parameter pre poziciu hlavy
pos2(Config, CurPos, OutPos) :-
  nth0(CurPos, Config, N),
  ( (char_type(N, upper), OutPos is CurPos);
    ( NewPos is CurPos - 1,
      pos2(Config, NewPos, OutPos)
    )
  ).

% Ziska aktualny stav TS.
% Config - konfiguracia TS
% State  - vystupny parameter pre stav
state(Config, State) :-
  pos(Config, Pos),
  nth0(Pos, Config, State).

% Ziska retazec pred a za hlavou TS.
% Config - prvy parameter, konfiguracia TS
% L      - vystupny parameter pre retazec za (nalavo) hlavou
% R      - vystupny parameter pre retazec pred (napravo) hlavou
split_config([],[],[]).
split_config([H|T],L,R) :- char_type(H, upper),R = T, L = [].
split_config([H|T],L,R) :-
  \+ char_type(H, upper),
  split_config(T,X,R),
  append([H],X,L).

% Ziska vlastnosti konfiguracie - stav, retazec pred a za hlavou, poziciu.
% Config - konfiguracia TS
% State  - vystupny parameter pre stav TS
% Left   - vystupny parameter pre retazec za (nalavo) hlavou
% Right  - vystupny parameter pre retazec pred (napravo) hlavou
% N      - vystupny parameter pre poziciu hlavy na paske
config(Config, State, Left, Right, N) :-
  pos(Config, N),
  state(Config, State),
  split_config(Config, Left, Right).

% Zisti, ci je zoznam prazdny.
% L - zoznam
is_empty(L) :-
  length(L, Len),
  Len == 0.

% Odstrani N-ty prvok zoznamu
% InList  - vstupny zoznam
% N       - pozicia odstranovaneho prvku
% OutList - vystupny parameter pre vystupny zoznam
delete_nth(InList, N, OutList) :- delete_nth2(InList, N, 0, [], OutList).

% Vid. delete_nth.
% CN      - aktualny prvok
% TmpList - docasny zoznam
delete_nth2([HI|TI], N, CN, TmpList, OutList) :-
  ( N == CN,
    append(TmpList, TI, OutList)
  );
  ( N \== CN,
    CNx is CN + 1,
    append(TmpList, [HI], TL),
    delete_nth2(TI, N, CNx, TL, OutList)
  ).

% Odstrani posledny prvok zoznamu.
% InList  - vstupny zoznam
% OutList - vystupny zoznam
delete_last(InList, OutList) :-
  \+ is_empty(InList),
  length(InList, Len),
  L is Len-1,
  delete_nth(InList, L, OutList).

% Vid. delete_last.
delete_first(InList, OutList) :-
  \+ is_empty(InList),
  delete_nth(InList, 0, OutList).

tm_perform(Config) :-
  tm_step(Config, OutConfig),
  print_config(OutConfig).

tm_step(InConfig, OutConfig) :-
  OutConfig = InConfig.

%tm_action('L', InConfig, OutConfig) :-
%  split_config(InConfig, LC, RC),
%  \+ is_empty(LC).

%tm_action('R', InConfig, OutConfig) :-
%tm_action(Sym, InConfig, OutConfig) :-

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
