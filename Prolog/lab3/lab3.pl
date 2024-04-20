test(Arr, K) :-
    printItems(Arr),
    writeln(K).

printItems([]).
printItems([Item|Rest]) :-
     writeln(Item),
     printItems(Rest).

