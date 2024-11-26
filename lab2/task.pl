% Инициализация списка друзей для каждого человека
initialize_friends([], _).
initialize_friends([FriendList | Tail], People) :-
    sublist(FriendList, People), 
    initialize_friends(Tail, People).

% Построение списка друзей по фамилии
get_friends_by_surname(Surname, [Surname | _], [FriendList | _], FriendList).
get_friends_by_surname(Surname, [_ | RestSurnames], [_ | RestFriendLists], FriendList) :-
    get_friends_by_surname(Surname, RestSurnames, RestFriendLists, FriendList).

% Основной предикат решения задачи
solve([[LeonidSurname, leonid, LeonidFriends], 
       [MichaelSurname, michael, MichaelFriends], 
       [NickolaySurname, nickolay, NickolayFriends], 
       [OlegSurname, oleg, OlegFriends], 
       [PeterSurname, peter, PeterFriends]]) :-
    
    % Перестановка фамилий
    permutation([atarov, bartenev, klenov, danilin, ivanov],
                [LeonidSurname, MichaelSurname, NickolaySurname, OlegSurname, PeterSurname]),
    
    % Проверка всех условий
    validate([
        [LeonidSurname, leonid, LeonidFriends],
        [MichaelSurname, michael, MichaelFriends],
        [NickolaySurname, nickolay, NickolayFriends],
        [OlegSurname, oleg, OlegFriends],
        [PeterSurname, peter, PeterFriends]
    ]), !.

% Проверка условий задачи
validate([
    [LeonidSurname, leonid, LeonidFriends],
    [MichaelSurname, michael, MichaelFriends],
    [NickolaySurname, nickolay, NickolayFriends],
    [OlegSurname, oleg, OlegFriends],
    [PeterSurname, peter, PeterFriends]
]) :- 

    % Инициализация списков друзей
    initialize_friends([LeonidFriends, MichaelFriends, NickolayFriends, OlegFriends, PeterFriends],
                       [[LeonidSurname, leonid], [MichaelSurname, michael], [NickolaySurname, nickolay], [OlegSurname, oleg], [PeterSurname, peter]]),

    % Условия по задаче

    % 1. Петр знает троих людей
    PeterFriends = [_, _, _],

    % 2. Леонид знает одного человека
    LeonidFriends = [_],

    % 3. Леонида знает только один человек
    (
        member([LeonidSurname, leonid], MichaelFriends),
        \+ (member([LeonidSurname, leonid], NickolayFriends); 
            member([LeonidSurname, leonid], OlegFriends); 
            member([LeonidSurname, leonid], PeterFriends))
    ;
        member([LeonidSurname, leonid], NickolayFriends),
        \+ (member([LeonidSurname, leonid], MichaelFriends); 
            member([LeonidSurname, leonid], OlegFriends); 
            member([LeonidSurname, leonid], PeterFriends))
    ;
        member([LeonidSurname, leonid], OlegFriends),
        \+ (member([LeonidSurname, leonid], MichaelFriends); 
            member([LeonidSurname, leonid], NickolayFriends); 
            member([LeonidSurname, leonid], PeterFriends))
    ;
        member([LeonidSurname, leonid], PeterFriends),
        \+ (member([LeonidSurname, leonid], MichaelFriends); 
            member([LeonidSurname, leonid], NickolayFriends); 
            member([LeonidSurname, leonid], OlegFriends))
    ),

    % 4. Михаил, Николай и Олег знают друг друга
    member([_, nickolay], MichaelFriends),
    member([_, oleg], MichaelFriends),
    member([_, michael], NickolayFriends),
    member([_, oleg], NickolayFriends),
    member([_, michael], OlegFriends),
    member([_, nickolay], OlegFriends),

    % 5. Николай знает Иванова
    member([ivanov, _], NickolayFriends),

    % 6. Михаил не знает Данилина
    \+ member([danilin, _], MichaelFriends),

    % 7. Бартенев знает двух людей
    get_friends_by_surname(bartenev, [LeonidSurname, MichaelSurname, NickolaySurname, OlegSurname, PeterSurname],
                           [LeonidFriends, MichaelFriends, NickolayFriends, OlegFriends, PeterFriends], BartenevFriends),
    BartenevFriends = [_, _],

    % 8. Атаров знает троих людей
    get_friends_by_surname(atarov, [LeonidSurname, MichaelSurname, NickolaySurname, OlegSurname, PeterSurname],
                           [LeonidFriends, MichaelFriends, NickolayFriends, OlegFriends, PeterFriends], AtarovFriends),
    AtarovFriends = [_, _, _].

?- solve(Result), writeln(Result).