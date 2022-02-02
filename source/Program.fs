namespace Neasden

module Program =

    [<EntryPoint>]
    let main _ =
        let outcome = Game.play ()
        0