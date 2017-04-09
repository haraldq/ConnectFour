open System
open System.Collections
open Program.Library
open Program.Game


[<EntryPoint>]
let main argv = 
    let rec play() =    
        Console.Clear()    
        Console.WriteLine("Connect four!")    
        roll init Player.White
        Console.WriteLine "Play again? (y/n)"         
        match Console.ReadLine() with 
        | "y" | "Y" -> play()
        | "n" |  "N" | _ -> ()
    play()
    0