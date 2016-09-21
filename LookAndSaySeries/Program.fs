namespace LookAndSaySeries
module Program =

    // given a sequence of numbers, return the sequence that would be the input 'read out loud'
    let rec next (data:int[]) =
        let rec count n (data:int[]) =
            if data.Length = 0 || (not (data.[0] = n)) then
                0
            else
                1 + (count n data.[1..])
        if data.Length = 0 then
            [||]
        else
            let n = data.[0]
            let num = count n data
            Array.concat[ [| num ; n |] ; (next data.[num..])]

    // find the "Nth" look and say sequence
    let rec lookAndSay n =
        if n = 0 then
            [| 1 |]
        else
            next (lookAndSay (n - 1))

    [<EntryPoint>]
    let main argv =
        let zeroth = lookAndSay 0
        let first = lookAndSay 1
        let second = lookAndSay 2
        let tenth = lookAndSay 10
        printfn "%A" tenth
        0 // return an integer exit code
