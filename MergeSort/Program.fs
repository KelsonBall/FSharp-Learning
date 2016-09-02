let rec merge (left:int[]) (right:int[]) : int[] =
    if left.Length = 0 then
        right
    else if right.Length = 0 then
        left
    else if left.[0] < right.[0] then
        Array.concat[ [| left.[0] |] ; merge left.[1..] right]
    else
        Array.concat[ [| right.[0] |] ; merge left right.[1..]]


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
