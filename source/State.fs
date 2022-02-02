namespace Neasden

type State =
    { 
        board: Board
        turn: int
        player: Player
    }

module State =

    // Create an empty board
    let create (): State =
        {
            board = Board.create ()
            turn = 0
            player = Player.USER
        }
    
    // Advance the state with a move
    let advance (state: State) (move: Move): State =
        let newBoard = Board.makeMove state.board move state.player
        let newTurn =
            if move.confirmed then state.turn + 1
            else state.turn
        let newPlayer =
            if not move.confirmed then state.player
            else Player.other state.player

        {
            board = newBoard
            turn = newTurn
            player = newPlayer
        }
    
    let display (state: State): unit =
        Board.display state.board