namespace MergeSort

module DataDisplay =
    let printArray (data:int[]) : unit =
    let rec printElement (data:int[]) : unit =
        if data.Length = 0 then
            System.Console.WriteLine(System.Environment.NewLine)
        else
            System.Console.Write(", ")
            System.Console.Write(data.[0])
            printElement data.[1..]
    System.Console.Write(data.[0])
    printElement data.[1..]

