namespace Neasden

module Console =

    type Key = string

    let getKey (): Key =
        System.Console.ReadKey().Key.ToString()
    
    let isArrowKey (key: Key): bool =
        match key with 
            | "UpArrow"
            | "DownArrow"
            | "LeftArrow"
            | "RightArrow" -> true
            | _ -> false
    
    let bell (): unit =
        System.Console.Beep()