open System
open System.Collections
open Program.Library


[<EntryPoint>]
let main argv = 
    Console.WriteLine("Connect four!")
        
    let game = Program.Game.start

    let action = fun _ ->
        Console.Write "\nLägg sten: "
        Console.ReadLine()


    let readlines = Seq.initInfinite (fun _ -> action())
    let run item = 
        if item = "q" 
        then Some(item) 
        else 
            //parse item 
            None

    Seq.pick run readlines |> ignore

    Console.WriteLine "Later!"
    0