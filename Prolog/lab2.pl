% Oscar Dahlberg, oscdah-9

start(Arr, K) :- calcKSmallest(Arr, K).

calcKSmallest(Arr, K) :-
    writeln('Sum   i     j     List'),
    writeln('----------------------'),
    kSmallestSubArrays(Arr, K, KSmallestSorted),
    printItems(KSmallestSorted).

printItems([]).
printItems([(Sum, Arr, FirstIndex, LastIndex)|Rest]) :-
    write(Sum),
    write("     "),
    write(FirstIndex),
    write("     "),
    write(LastIndex),
    write("    "),
    writeln(Arr),
    printItems(Rest).

kSmallestSubArrays(Arr, K, KSmallestSorted) :-
    length(Arr, ArrSize),
    fullSubLists(Arr, 1, ArrSize, SubLists),
    length(SubLists, Size),
    smallestSubset(SubLists, Size, K, KSmallestSorted).

% Different predicates for the size of the sublist
% After if the list is larger than 1 it is sorted
smallestSubset(SubLists, Size, _, Sorted) :-
    Size =:= 1,
    Sorted = SubLists.

smallestSubset(SubLists, Size, K, Sorted) :-
    Size =:= K,
    insertionSort(SubLists, Sorted).

% Sorts the list and then only returns the k smallest
smallestSubset(SubLists, Size, K, KSmallest) :-
    Size > K,
    insertionSort(SubLists, Sorted),
    kSmallest(Sorted, K, KSmallest).

% Adds the k first elements in the list to an new list
% through recursion, then returns it.
kSmallest([First|_], 1, [First]).
kSmallest([First|RestArr], K, [First|Larger]) :-
    Rest is K - 1,
    kSmallest(RestArr, Rest, Larger).

% Standard insertion sort
% Sorts based on the size of Sum
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

% The elements in the array looks like this: (Sum, List, (First index, Last index))
% The objective of this function is to get all sub lists from the list
% It does this by calling PrefixIndexSubLists with -1 to the LastIndex
% so it removes the last element.
% It then recursively applies -1 to it until it is of size 1 and returns all
% Then fullSubLists calls itself with +1 to FirstIndex and then prefixIndexSublists again
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

% Removes the last element (index List size element)
removeLast([_], []).
removeLast([X|Xs], [X|Rest]) :-
    removeLast(Xs, Rest).

% Removes the First element, (index 0 element)
removeFirst([_|Xs], Xs).

% Finds the sum of a list
sumList([], 0).
sumList([Head|Tail], Sum) :-
    sumList(Tail, TailSum),
    Sum is Head + TailSum.


