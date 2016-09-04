namespace RotateArray
module Program =

    let rotate count (data:int[]) =
        let index = data.Length - count % data.Length
        if index = 0 then
            data
        else
            Array.concat[ data.[index..] ; data.[..(index-1)] ]


    [<EntryPoint>]
    let main argv =
        let printPair = printfn "%A,%A"
        printPair [| 6;3;8;9;7 |] (rotate 1 [| 3;8;9;7;6 |])
        printPair [| 9;7;6;3;8 |] (rotate 3 [| 3;8;9;7;6 |])
        0
