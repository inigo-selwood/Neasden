namespace Neasden

module Console =

    let getCursorXY (): int * int =
        let x = System.Console.CursorLeft
        let y = System.Console.CursorTop
        (x, y)
    
    let setCursorXY ((x, y) : int * int): unit =
        System.Console.SetCursorPosition(x, y)
    