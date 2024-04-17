start() :- solve(state([], k)).

solve


kSmallestSubArrays(state(Arr, k), KSmallestSorted) :-
    length(Arr, ArrSize),
    fullSubLists(state(Arr, 1, ArrSize), SubLists),
    length(Sublists, Size)
    sortedSubset(SubLists, Size, k, KSmallestSorted).

sortedSubSet(SubLists, Size, _, Sorted) :-
    Size == 1,
    Sorted is Sublists.

sortedSubSet(Sublists, Size, k, Sorted) :-
    Size == k,
    insertionSort(Sublists, Sorted).

sortedSubSet(Sublists, Size, k, KSmallest) :-
    Size > k,
    insertionSort(Sublists, Sorted),
    kSmallest(Sorted, k, KSmallest).

kSmallest(Sorted, k, KSmallest)

insertionSort() :-


fullSubLists(state(Arr, FirstIndex, LastIndex), [(ArrSum, Arr, (FirstIndex, LastIndex))| NewState]) :-
    sum_list(Arr, ArrSum),
    length(Arr, Size),
    Size > 1,
    remove_last(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(state(Head, FirstIndex, NewLast), PrefixSubLists),
    tail(Arr, Tail),
    NewFirst is FirstIndex + 1,
    fullSubLists(state(Tail, NewFirst, LastIndex), SuffixSubLists),
    append(PrefixSubLists, SuffixSubLists, NewState).

prefixIndexSubLists(state(Arr, FirstIndex, LastIndex), [(ArrSum, Arr, (FirstIndex, LastIndex)) | PrefixSubLists]) :-
    sum_list(Arr, ArrSum),
    length(Arr, Size),
    Size > 1,
    remove_last(Arr, Head),
    NewLast is LastIndex - 1,
    prefixIndexSubLists(state(Head, FirstIndex, NewLast)), PrefixSubLists),

remove_last([_], []).
remove_last([X|Xs], [X|Rest]) :-
    remove_last(Xs, Rest).

sum_list([], 0).
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, TailSum),
    Sum is Head + TailSum.

