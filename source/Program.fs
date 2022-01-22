namespace Balham

module Program =

    let loop (): int =
        let board = Game.Board.empty ()
        Game.Board.display board

        0

    [<EntryPoint>]
    let main _ =
        loop ()