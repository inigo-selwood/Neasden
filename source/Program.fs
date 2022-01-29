namespace Neasden

module Program =

    [<EntryPoint>]
    let main _ =
        let startState = Game.stateCreate ()
        let outcome = Game.play startState

        0



