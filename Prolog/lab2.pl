start(Arr, K) :- calcKSmallest(Arr, K).

calcKSmallest(Arr, K) :-
    writeln('Sum   List  First Index  Second Index'),
    kSmallestSubArrays(Arr, K, KSmallestSorted),
    printItems(KSmallestSorted).

printItems([]).
printItems([(Sum, Arr, FirstIndex, LastIndex)|Rest]) :-
    write(Sum),
    write("     "),
    write(Arr),
    write("     "),
    write(FirstIndex),
    write("             "),
    writeln(LastIndex),
    printItems(Rest).

kSmallestSubArrays(Arr, K, KSmallestSorted) :-
    length(Arr, ArrSize),
    fullSubLists(Arr, 1, ArrSize, SubLists),
    length(SubLists, Size),
    smallestSubset(SubLists, Size, K, KSmallestSorted).

smallestSubset(SubLists, Size, _, Sorted) :-
    Size =:= 1,
    Sorted = SubLists.

smallestSubset(SubLists, Size, K, Sorted) :-
    Size =:= K,
    insertionSort(SubLists, Sorted).

smallestSubset(SubLists, Size, K, KSmallest) :-
    Size > K,
    insertionSort(SubLists, Sorted),
    kSmallest(Sorted, K, KSmallest).

kSmallest([First|_], 1, [First]).
kSmallest([First|RestArr], K, [First|Larger]) :-
    Rest is K - 1,
    kSmallest(RestArr, Rest, Larger).

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

fullSubLists([LastItem], FIdx, LIdx, [(LastItem, [LastItem], (FIdx, LIdx))]).
fullSubLists(Arr, FirstIndex, LastIndex, [(Sum, Arr, (FirstIndex, LastIndex))| NewState]) :-
    sumList(Arr, Sum),
    removeLast(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(Head, FirstIndex, NewLast, PrefixSubLists),
    removeFirst(Arr, Tail),
    NewFirst is FirstIndex + 1,
    fullSubLists(Tail, NewFirst, LastIndex, SuffixSubLists),
    append(PrefixSubLists, SuffixSubLists, NewState).

prefixIndexSubLists([LastItem], FIdx, LIdx, [(LastItem, [LastItem], (FIdx, LIdx))]).
prefixIndexSubLists(Arr, FirstIndex, LastIndex, [(Sum, Arr, (FirstIndex, LastIndex)) | PrefixSubLists]) :-
    sumList(Arr, Sum),
    removeLast(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(Head, FirstIndex, NewLast, PrefixSubLists).

removeLast([_], []).
removeLast([X|Xs], [X|Rest]) :-
    removeLast(Xs, Rest).

removeFirst([_|Xs], Xs).

sumList([], 0).
sumList([Head|Tail], Sum) :-
    sumList(Tail, TailSum),
    Sum is Head + TailSum.


