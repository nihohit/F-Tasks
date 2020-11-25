// assumes that `comparer` takes 2 values of type 'a and returns 1 if the first is larger than the 
// second, -1 if the second is larger than the first, and 0 if they're equal.
// This uses quicksort, as stated bin task1.fsi, has a high worst-case complexity, but is usually good enough.
// while performance can be improved by not sorting the larger array once the lowest k items were found, but it's late
// and I'm too tired to optimize. ¯\_(ツ)_/¯
// It wasn't clear from the task what's the signature of the comparer, so I wasn't sure whether I can use List.sortBy.
let rec sorter (ls :'a list) comparer =
    let rec auxiliarySorter pivot internalList smaller larger equal =
        match internalList with
        | [] -> (smaller, equal, larger)
        | head :: tail -> 
            let comparison = comparer head pivot 
            if comparison > 0 then
              auxiliarySorter pivot tail smaller (head::larger) equal
            else if comparison < 0 then
              auxiliarySorter pivot tail (head::smaller) larger equal
            else
              auxiliarySorter pivot tail smaller larger (head::equal)
    match ls with
    | [] -> []
    | head :: _ ->
        let (smaller, equal, larger) = auxiliarySorter head ls [] [] []
        (sorter smaller comparer)@equal@(sorter larger comparer)

let findKthSmallest comparer k ls = 
    let sortedList = sorter ls comparer
    sortedList.Item(k)

let intComparer first second = 
    if first > second then 1
    else if second > first then -1
    else 0

let res1 = findKthSmallest intComparer 2 [2; 5; 3]
let res2 = findKthSmallest intComparer 1 [2; 5; 3;1]

printfn  "result1: %A" res1
printfn  "result2: %A" res2

    