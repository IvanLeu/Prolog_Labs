name(X) :- (X = leonid; X = michael; X = nickolay; X = oleg; X = petr).
surname(X) :- (X = atarov; X = bartenev; X = klenov; X = danilin; X = ivanov).
person(N, S) :- name(N), surname(S).

less(leonid, michael).
less(leonid, nickolay).
less(leonid, oleg).
less(leonid, petr).
less(michael, nickolay).
less(michael, oleg).
less(michael, petr).
less(nickolay, oleg).
less(nickolay, petr).
less(oleg, petr).

uniq([]).
uniq([H | T]) :- 
   member(H, T), !, fail;
   uniq(T).

relation([]) :- fail.
relation([[N, S], [N1, S1]]) :- 
   person(N, S), person(N1, S1), 
   uniq([N, N1]),
   uniq([S, S1]).
relation([[N, S], [N1, S1], [N2, S2]]) :- 
   person(N, S), person(N1, S1), person(N2, S2),
   uniq([N, N1, N2]), less(N1, N2),
   uniq([S, S1, S2]).
relation([[N, S], [N1, S1], [N2, S2], [N3, S3]]) :- 
   person(N, S), person(N1, S1), person(N2, S2), person(N3, S3),
   uniq([N, N1, N2, N3]), less(N1, N2), less(N1, N3), less(N2, N3),
   uniq([S, S1, S2, S3]).

z([[_, _]]).
z([[N, S] | T]) :- 
   member([N, AS], T), not(AS = S), !, fail;
   member([AN, S], T), not(AN = N), !, fail;
   z(T).

solution(V, W, X, Y, Z) :- 
   V = [[leonid, S1] | T1],
   relation([[leonid, S1] | T1]),
   append([[leonid, S1]], T1, L1),
   %------------------------------------------------------------------------------1111111
   (
      S1 = bartenev,
      length(T1, 2);
      not(S1 = bartenev)
   ),

   length(T1, 1),

   (
      S1 = danilin,
      not(member([michael, _], T1));
      not(S1 = danilin)
   ),
   
   not(member([michael, danilin], T1)),

   (
      S1 = ivanov,
      member([nickolay, _], T1);
      not(S1 = ivanov)
   ),
   not(member([nickolay, ivanov], T1)),

   (
      S1 = atarov,
      length(T1, 3);
      not(S1 = atarov)
   ),
   
   (
      S1 = klenov,
      length(T1, 1);
      not(S1 = klenov)
   ),
   %------------------------------------------------------------------------------2222222

   W = [[michael, S2] | T2],
   relation([[michael, S2] | T2]),
   append([[michael, S2]], T2, L2),
   append(L1, L2, L6),

   (
      S2 = bartenev,
      length(T2, 2);
      not(S2 = bartenev)
   ),

   not(member([_, danilin], T2)),

   not(S2 = danilin),

   (
      S2 = ivanov,
      member([nickolay, _], T2);
      not(S2 = ivanov)
   ),

   not(member([nickolay, ivanov], T2)),

   member([nickolay, _], T2),
   member([oleg, _], T2),

   (
      S2 = atarov,
      length(T2, 3);
      not(S2 = atarov)
   ),
   
   (
      S2 = klenov,
      length(T2, 1);
      not(S2 = klenov)
   ),
   %------------------------------------------------------------------------------3333333
   
   X = [[nickolay, S3] | T3],
   relation([[nickolay, S3] | T3]),
   append([[nickolay, S3]], T3, L3),
   append(L6, L3, L7),

   (
      S3 = bartenev,
      length(T3, 2);
      not(S3 = bartenev)
   ),

   (
      S3 = danilin,
      not(member([michael, S2], T3));
      not(S3 = danilin)
   ),
   not(member([michael, danilin], T3)),

   member([_, ivanov], T3),
   
   not(S3 = ivanov),

   member([michael, S2], T3),

   member([oleg, _], T3),

   (
      S3 = atarov,
      length(T3, 3);
      not(S3 = atarov)
   ),
   
   (
      S3 = klenov,
      length(T3, 1);
      not(S3 = klenov)
   ),
   %------------------------------------------------------------------------------4444444

   Y = [[oleg, S4] | T4],
   relation([[oleg, S4] | T4]),
   append([[oleg, S4]], T4, L4),
   append(L7, L4, L8),

   (
      S4 = bartenev,
      length(T4, 2);
      not(S4 = bartenev)
   ),

   (
      S4 = danilin,
      not(member([michael, S2], T4));
      not(S4 = danilin)
   ),
   
   not(member([michael, danilin], T4)),

   (
      S4 = ivanov,
      member([nickolay, S3], T4);
      not(S4 = ivanov)
   ),
   not(member([nickolay, ivanov], T4)),

   member([michael, S2], T4),

   member([nickolay, S3], T4),

   (
      S4 = atarov,
      length(T4, 3);
      not(S4 = atarov)
   ),
   
   (
      S4 = klenov,
      length(T4, 1);
      not(S4 = klenov)
   ),

   (
      member([petr, S5], T1), member([petr, S5], T2), member([petr, S5], T3), not(member([petr, S5], T4));
      member([petr, S5], T1), member([petr, S5], T2), member([petr, S5], T4), not(member([petr, S5], T3));
      member([petr, S5], T1), member([petr, S5], T3), member([petr, S5], T4), not(member([petr, S5], T2));
      member([petr, S5], T2), member([petr, S5], T3), member([petr, S5], T4), not(member([petr, S5], T1))
   ),


   %------------------------------------------------------------------------------5555555

   Z = [[petr, S5] | T5],
   relation([[petr, S5] | T5]),
   uniq([S1, S2, S3, S4, S5]),
   append([[petr, S5]], T5, L5),
   append(L8, L5, L),
   z(L),

   (
      S5 = bartenev,
      length(T5, 2);
      not(S5 = bartenev)
   ),

   length(T5, 3),

   (
      member([leonid, S1], T2), not(member([leonid, S1], T3)), not(member([leonid, S1], T4)), not(member([leonid, S1], T5));
      member([leonid, S1], T3), not(member([leonid, S1], T2)), not(member([leonid, S1], T4)), not(member([leonid, S1], T5));
      member([leonid, S1], T4), not(member([leonid, S1], T2)), not(member([leonid, S1], T3)), not(member([leonid, S1], T5));
      member([leonid, S1], T5), not(member([leonid, S1], T2)), not(member([leonid, S1], T3)), not(member([leonid, S1], T4))
   ),

   (
      S5 = danilin,
      not(member([michael, S2], T5));
      not(S5 = danilin)
   ),

   not(member([michael, danilin], T5)),

   (
      S5 = ivanov,
      member([nickolay, S3], T5);
      not(S5 = ivanov)
   ),
   not(member([nickolay, ivanov], T5)),

   (
      S5 = atarov,
      length(T5, 3);
      not(S5 = atarov)
   ),
   
   (
      S5 = klenov,
      length(T5, 1);
      not(S5 = klenov)
   ).

?- solution(V, W, X, Y, Z), writeln(V), writeln(W), writeln(X), writeln(Y), writeln(Z).