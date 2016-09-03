namespace Main
module Program =

    // Utilizes the property (definition) of the lucas numbers that the sum of
    // two consecutive seeds is in the lucas set of those seed numbers
    let rec lucas a b n =
        if n = 0 then
            a
        else
            lucas b (a + b) (n - 1)

    // Special case of the lucas numbers seeded with 0 and 1
    let fib = lucas 0 1

    [<EntryPoint>]
    let main argv =
        let rng = System.Random()
        let value = rng.Next(10, 30)
        printfn "%d, %d" value (fib value)
        0 // return an integer exit code
