namespace OddOccurance
module Program =

    // Remove an index from an array
    let splice index (data:int[]) =
        if data.Length = 1 then
            [| |]
        else if data.Length = index + 1 then
            data.[..(index-1)]
        else if index = 0 then
            data.[1..]
        else
            Array.concat[ data.[..(index - 1)] ; data.[(index + 1)..] ]

    // Count the number of occurances of a value in an array and then return an array without that value
    let rec countOccurances find count index (data:int[]) =
        if  index = data.Length then
            count, data
        else if data.[index] = find then
            countOccurances find (count + 1) index (splice index data)
        else
            countOccurances find count (index + 1) data

    // Find the first element in an array that occurs an odd number of times
    let rec findOddElement (data:int[]) =
        let item = data.[0]
        let count, nextSet = countOccurances item 0 0 data
        if count % 2 = 1 then
            item
        else
            findOddElement nextSet

    [<EntryPoint>]
    let main argv =
        let printPair = printfn "%d,%d"
        printPair 7 (findOddElement [| 9;3;9;3;9;7;9 |])
        printPair 2 (findOddElement [| 90;90;3;9;3;7;11;9;9;7;9;11;90;2;90;2;2 |])
        0