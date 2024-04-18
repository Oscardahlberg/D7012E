start() :- calcKSmallest(state(Arr, k)).

calcKSmallest(state(Arr, k)) :-
    write('Sum        List    First Index  Second Index'),
    kSmallestSubArrays(state(Arr, k), KSmallestSorted),
    printItems(KSmallestSorted).

printItems([]).
printItems([Item|Rest]) :-
     writeln(Item),
     printItems(Rest).

kSmallestSubArrays(state(Arr, k), KSmallestSorted) :-
    length(Arr, ArrSize),
    fullSubLists(state(Arr, 1, ArrSize), SubLists),
    length(SubLists, Size),
    smallestSubset(SubLists, Size, k, KSmallestSorted).

smallestSubset(SubLists, Size, _, Sorted) :-
    Size =:= 1,
    Sorted = SubLists.

smallestSubset(SubLists, Size, k, Sorted) :-
    Size =:= k,
    insertionSort(SubLists, Sorted).

smallestSubset(SubLists, Size, k, KSmallest) :-
    Size > k,
    insertionSort(SubLists, Sorted),
    kSmallest(Sorted, k, KSmallest).

kSmallest(_, 0, []).
kSmallest([First|RestArr], k, KSmallest) :-
    Rest is k - 1,
    Rest > 0,
    kSmallest(RestArr, Rest, Larger),
    append(First, Larger, KSmallest).

insertionSort([], []).
insertionSort([X|Rest], Sorted) :-
    insertionSort(Rest, SortedRest),
    insert(X, SortedRest, Sorted).

insert((Sum, Arr, Idx), [], [(Sum, Arr, Idx)]).
insert((Sum, Arr, Idx), [(SumY, ArrY, IdxY)|Rest], [(Sum, Arr, Idx),(SumY, ArrY, IdxY)|Rest]) :-
    Sum =< SumY.
insert((Sum, Arr, Idx), [(SumY, ArrY, IdxY)|Rest], [(SumY, ArrY, IdxY)|Sorted]) :-
    Sum > SumY,
    insert((Sum, Arr, Idx), Rest, Sorted).

fullSubLists(state(Arr, FirstIndex, LastIndex), [(Sum, Arr, (FirstIndex, LastIndex))| NewState]) :-
    sumList(Arr, Sum),
    length(Arr, Size),
    Size > 1,
    removeLast(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(state(Head, FirstIndex, NewLast), PrefixSubLists),
    tail(Arr, Tail),
    NewFirst is FirstIndex + 1,
    fullSubLists(state(Tail, NewFirst, LastIndex), SuffixSubLists),
    append(PrefixSubLists, SuffixSubLists, NewState).

prefixIndexSubLists(state([LastItem], Idx), [(Sum, LastItem, Idx)]) :-
    sumList(Arr, Sum).
prefixIndexSubLists(state(Arr, FirstIndex, LastIndex), [(Sum, Arr, (FirstIndex, LastIndex)) | PrefixSubLists]) :-
    sumList(Arr, Sum),
    length(Arr, Size),
    Size > 1,
    removeLast(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(state(Head, FirstIndex, NewLast)), PrefixSubLists).

removeLast([_], []).
removeLast([X|Xs], [X|Rest]) :-
    removeLast(Xs, Rest).

sumList([], 0).
sumList([Head|Tail], Sum) :-
    sumList(Tail, TailSum),
    Sum is Head + TailSum.


