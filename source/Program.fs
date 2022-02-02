namespace Neasden

module Program =

    [<EntryPoint>]
    let main _ =
        let outcome = Game.play ()

        Console.clearToEnd ()
        0