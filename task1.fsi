type Size = int
type Bolt = Bolt of Size
type Nut = Nut of Size

let comp (nut: Nut) (bolt: Bolt) : int = 
    let val1 = match nut with Nut value -> value
    let val2 =  match bolt with Bolt value -> value
    if val1 < val2 then
        -1
    else if val1 > val2 then
        1
    else 
        0

// Spent too long trying to understand how to combine generics with a discriminated union, so decided to just write twice. SAD :(\
// This uses quicksort which has an average complexity of O(nlogn), but worst case of O(n^2). I was too busy trying to handle the new language
// to implement mergesort. sorry ¯\_(ツ)_/¯
let nutSorter (ls :Nut list) (pivot :Bolt) =
    let rec auxiliarySorter internalList smaller larger equal =
        match internalList with
        | [] -> (smaller, equal, larger)
        | head :: tail -> 
            let comparison = comp head pivot
            if comparison > 0 then
              auxiliarySorter tail smaller (head::larger) equal
            else if comparison < 0 then
              auxiliarySorter tail (head::smaller) larger equal
            else
              auxiliarySorter tail smaller larger (head::equal)
    auxiliarySorter ls [] [] []

let boltSorter (ls :Bolt list) (pivot :Nut) =
    let rec auxiliarySorter internalList smaller larger equal =
        match internalList with
        | [] -> (smaller, equal, larger)
        | head :: tail -> 
            let comparison = comp pivot head
            if comparison > 0 then
              auxiliarySorter tail (head::smaller) larger equal
            else if comparison < 0 then
              auxiliarySorter tail smaller (head::larger) equal
            else
              auxiliarySorter tail smaller larger (head::equal)
    auxiliarySorter ls [] [] []

let rec fit (nuts: Nut list) (bolts: Bolt list)  =
    match nuts with 
    | [] -> []
    | nut :: _ ->
        let (smallerBolts, equalBolts, largerBolts) = boltSorter bolts nut
        let (smallerNuts, equalNuts, largerNuts) = nutSorter nuts equalBolts.Head
        let zipped = List.zip equalNuts equalBolts 
        (fit smallerNuts smallerBolts)@zipped@(fit largerNuts largerBolts)

        
let res = fit [Nut(1);Nut(3);Nut(2);Nut(4);Nut(0)] [Bolt(4);Bolt(2);Bolt(1);Bolt(0);Bolt(3)] 
printfn  "result: %A" res
let res1 = fit [Nut(1);Nut(1);Nut(3);Nut(2);Nut(4);Nut(0);Nut(3)] [Bolt(4);Bolt(3);Bolt(2);Bolt(1);Bolt(0);Bolt(1);Bolt(3)] 
printfn  "result: %A" res1