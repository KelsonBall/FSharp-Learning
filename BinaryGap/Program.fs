// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace BinaryGap
module Program =

    // Return the length of the longest run of consecutives '0' between two '1' in the binary representation of the input data
    let gap data =
        let rec countMaxZeros count max data =
            if data = 0 then
                max
            else
                if (data &&& 1) = 0 then
                    if (count + 1) > max then
                        countMaxZeros (count + 1) (count + 1) (data >>> 1)
                    else
                        countMaxZeros (count + 1) max (data >>> 1)
                else
                    countMaxZeros 0 max (data >>> 1)
        let rec normalize data =
            if (data &&& 1) = 1 then
                data
            else
                normalize (data >>> 1)

        countMaxZeros 0 0 (normalize data)

    [<EntryPoint>]
    let main argv =
        let printPair = printfn "%d,%d"
        printPair 5 (gap 1041)
        printPair 1 (gap 20)
        printPair 2 (gap 9)
        printPair 4 (gap 529)
        0 // return an integer exit code
