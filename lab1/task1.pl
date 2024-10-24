%% Первая часть задания - предикаты работы со списками

% Длинна списка
my_length([], 0).
my_length([_|T], L) :- my_length(T, L1), L is L1 + 1.

% Элемент в списке
my_member(El, [El|_]).
my_member(El, [_|T]) :- my_member(El, T).

% Вставка
my_append([], L, L).
my_append([H|T], L2, [H|L2T]) :- my_append(T, L2, L2T).

% Удаление элемента из списка
my_remove(El, [El|T], T).
my_remove(El, [H|T], [H|T2]) :- my_remove(El, T, T2).

% Перестановки
my_permute([], []).
my_permute(L, [H|T]) :- my_remove(H, L, R), my_permute(R, T).

% Подсписок списка 
my_sublist(S, L) :- my_append(_, L1, L), my_append(S, _, L1). 

% Вариант 10 - вставка элемента в список на указанную позицию
insert_at_std(El, Pos, L, R) :-
    Pos > 0,
    length(Prefix, PosMinus1),
    PosMinus1 is Pos - 1,
    append(Prefix, Suffix, L),
    append(Prefix, [El|Suffix], R).

% Без испроьзования стандартных предикатов
insert_at(El, 1, T, [El|T]) :- !.
insert_at(El, Pos, [H|T], [H|R]) :-
    Pos > 1,
    Pos1 is Pos - 1,
    insert_at(El, Pos1, T, R).

% Вариант 15 - Позиция первого отричательного элемента в списке
first_negative_std(L, Pos) :-
    nth1(Pos, L, El),
    El < 0, !.

% Без стандартных предикатов
first_negative_position(L, Pos) :-
    first_negative_position(L, Pos, 1).
first_negative_position([], _, _) :- 
    fail.
first_negative_position([El|_], Pos, Pos) :-
    El < 0.
first_negative_position([_|T], Pos, CurPos) :-
    Pos1 is CurPos + 1,
    first_negative_position(T, Pos, Pos1).

% Примеры использования
?- insert_at(x, 3, [a, b, c, d ,e], Res), writeln(Res).
?- first_negative_position([-1, 2, 1, 1, 2, 3, 4], Res), writeln(Res).