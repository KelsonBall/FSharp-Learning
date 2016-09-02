// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let rec merge (left:int[]) (right:int[]) : int[] =
    let mutable lefti = 0
    let mutable righti = 0
    let mutable result = Array.concat[ left ; right ]
    for i = 0 to left.Length + right.Length - 1 do
        if left.[lefti] < right.[righti] then
            result.[i] <- left.[lefti]
            lefti <- lefti + 1
            if lefti >= left.Length then
                lefti <- lefti - 1
                left.[lefti] <- 2147483647
        else
            result.[i] <- right.[righti]
            righti <- righti + 1
            if righti >= right.Length then
                righti <- righti - 1
                right.[righti] <- 2147483647
    result


let rec mergesort fromIndex toIndex (data:int[]) : int[] =
    if fromIndex = toIndex then
        [| data.[fromIndex] |]
    else
        let left = mergesort fromIndex ((toIndex + fromIndex) / 2) data
        let right = mergesort ((toIndex + fromIndex) / 2 + 1) toIndex data
        merge left right

[<EntryPoint>]
let main argv =
    let rnd = System.Random()
    let unsortedData = (Array.init (rnd.Next(5, 30)) (fun _ -> rnd.Next(100)))
    let sortedData = mergesort 0 (unsortedData.Length - 1) unsortedData
    0
