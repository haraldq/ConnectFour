open System
open System.Collections
open Program.Library
open Program.Game


[<EntryPoint>]
let main argv = 
    Console.WriteLine("Connect four!")
    roll init Player.White

    Console.WriteLine "Later!"
    0