namespace Main
module Program =

    let isLetter (letter:char) = (letter >= 'a' && letter <= 'z') || (letter >= 'A' && letter <= 'Z')

    let rec shiftCharUp (letter:char) shift : char =
        if shift = 0 then
            letter
        else
            if isLetter letter then
                match letter with
                | 'z' -> shiftCharUp 'a' (shift - 1)
                | 'Z' -> shiftCharUp 'A' (shift - 1)
                | _ -> shiftCharUp (letter + (char)1) (shift - 1)
            else
                letter

    let rec encode (mutator: int -> int) (key:int) (content:string)  : string =
        if content.Length = 0 then
            ""
        else
            ((string)(shiftCharUp ((char)content.[0]) key)) + (encode mutator (mutator key) (content.[1..])  )

    let simpleEncode = encode (fun value -> value + 1 ) 7
    let simpleDecode = encode (fun value -> value - 1 ) (26 - 7)

    [<EntryPoint>]
    let main argv =
        let code = simpleEncode "Hello, World!"
        printfn "%s" code
        let result = simpleDecode code
        printfn "%s" result
        0 // return an integer exit code
