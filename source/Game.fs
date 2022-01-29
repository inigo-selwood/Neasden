namespace Neasden

module Game =

    type Outcome =
        | WIN
        | LOSS
        | DRAW
    
    // ----------------------------------------------------------------- Player

    type Player =
        | USER
        | COMPUTER
    
    let playerOther (player: Player): Player =
        match player with 
            | Player.USER -> Player.COMPUTER
            | Player.COMPUTER -> Player.USER
    
    let playerValue (player: Player): int =
        match player with 
            | Player.USER -> 1
            | Player.COMPUTER -> -1

    type Move = int

    // ------------------------------------------------------------------ Board

    type Board = 
        {
            cells: list<int>
            cursor: option<Geometry.Point>
        }

    let boardCreate (): Board =
        {
            cells = [for _ in 0..8 -> 0]
            cursor = None
        }
    
    let boardSetCursor (board: Board) (cursor: Geometry.Point): Board =
        {
            cells = board.cells
            cursor = Some cursor
        }
    
    let boardGetCursor (board: Board): Geometry.Point =
        if board.cursor.IsSome then 
            board.cursor.Value
        else
            {
                x = 0
                y = 0
            }

    let boardFull (board: Board): bool =
        not <| List.contains 0 board.cells
    
    let boardFirstEmptyIndex (board: Board): option<int> =
        if boardFull board 
            then None 
        else 
            let isZero (element: int): bool =
                element = 0
            Some (List.findIndex isZero board.cells)

    let boardAxes (): list<list<int>> = 
        [
            [0; 1; 2]
            [3; 4; 5]
            [6; 7; 8]

            [0; 3; 6]
            [1; 4; 7]
            [2; 5; 8]

            [0; 4; 8]
            [2; 4; 6]
        ]
    
    let boardEmptyIndices (board: Board): list<int> =
        let listIndices = [0..8]
        let indexEmpty (index: int): option<int> =
            if board.cells[index] = 0 then Some index
            else None
        List.choose indexEmpty listIndices
    
    let boardAxesSums (board: Board): list<int> =
        let axisSum (axis: list<int>): int =
            let elementValue (index: int): int =
                board.cells[index]
            List.sumBy elementValue axis
        
        List.map axisSum (boardAxes ())
    
    let boardOutcome (board: Board) (player: Player): option<Outcome> =
        let ourValue = playerValue player
        let theirValue = playerValue (playerOther player)

        let axesSums = boardAxesSums board
        if List.contains (3 * ourValue) axesSums then
            Some Outcome.WIN
        elif List.contains (3 * theirValue) axesSums then 
            Some Outcome.LOSS
        elif boardFull board then
            Some Outcome.DRAW
        else
            None
    
    let boardDisplay (board: Board): unit =

        for row in 0..2 do
            System.Console.Write("| ")
            for column in 0..2 do 
                let icon ((column, row): int * int): char =
                    let index = row * 3 + column

                    if board.cursor.IsSome && board.cursor.Value.x = column && board.cursor.Value.y = row then '@'
                    elif board.cells[index] = playerValue Player.USER then 'x'
                    elif board.cells[index] = playerValue Player.COMPUTER then 'o'
                    else ' '
                
                System.Console.Write(icon (column, row))
            System.Console.Write('\n')
        System.Console.Write('\n')
        
    let boardHasAxisSum (board: Board) (axisSum: int): bool =
        let axesSums = boardAxesSums board
        List.contains axisSum axesSums
    
    let boardAxisHasSumCount (board: Board) (axisSum: int): int =
        let axesSums = boardAxesSums board
        let axisHasSum (index: int): option<int> =
            if axesSums[index] = axisSum then Some index
            else None
        let axesWithSum = List.choose axisHasSum [0..8]
        axesWithSum.Length
        
    let boardMakeMove (board: Board) (move: Move) (player: Player): Board =
        let value = playerValue player
        let newCells = List.updateAt move value board.cells

        {
            cells = newCells
            cursor = board.cursor
        }

    // ------------------------------------------------------------------ State

    type State = 
        {
            board: Board
            playerTurn: Player
            turn: int
        }

    let stateCreate (): State = 
        {
            board = boardCreate ()
            playerTurn = Player.USER
            turn = 0
        }
    
    let stateAdvance (state: State) (move: Move): State =
        {
            board = state.board
            playerTurn = playerOther state.playerTurn
            turn = state.turn + 1
        }
    
    // ------------------------------------------------------------------- Game

    let rec getUserMove (board: Board): Move =

        boardDisplay board

        let cursor = boardGetCursor board

        // If an arrow key was pressed, move the cursor 
        let key = Console.getKey ()
        if Console.isArrowKey key then
            let keyDirection (key: Console.Key): Geometry.Direction =
                match key with
                    | "UpArrow" -> Geometry.Direction.UP
                    | "DownArrow" -> Geometry.Direction.DOWN
                    | "LeftArrow" -> Geometry.Direction.LEFT
                    | "RightArrow" -> Geometry.Direction.RIGHT
                    | _ -> failwith "key does not correspond to an arrow key"
                
            let direction = keyDirection key
            let newCursor = Geometry.move cursor direction
            
            // Play a sound and don't move if the cursor isn't in bounds
            let inBounds (cursor: Geometry.Point): bool =
                cursor.x >= 0 && cursor.x < 3 && cursor.y >= 0 && cursor.y < 3
            if not (inBounds newCursor) then
                Console.bell ()
                getUserMove board
            
            // Otherwise, update the board with the new cursor
            else
                let newBoard = boardSetCursor board newCursor
                getUserMove newBoard
        
        // If enter was pressed, resolve the current position of the cursor 
        // as the intended move
        elif key = "Enter" then 
            let cursorIndex (cursor: Geometry.Point): int =
                cursor.y * 3 + cursor.x      
            cursorIndex cursor

        // Otherwise, alert the user that a wrong key was pressed
        else 
            Console.bell ()
            getUserMove board
    
    let getComputerMove (board: Board): Move =
        
        // Values for the user and the computer
        let userValue = playerValue Player.USER
        let computerValue = playerValue Player.COMPUTER

        // Makes the given move, returning a fresh board
        let makeMove (move: Move): Board =
            boardMakeMove board move Player.COMPUTER

        // True if a move will win the game
        let moveWins (move: Move): bool =
            let newBoard = boardMakeMove board move Player.COMPUTER

            let winSum = 3 * computerValue
            boardHasAxisSum newBoard winSum
        
        // True if a move blocks the user from getting 3 in a row
        let moveBlocks (move: Move): bool =
            let blockSum = (2 * userValue)
            let blockAxesStart = boardAxisHasSumCount board blockSum

            let newBoard = makeMove move
            let blockAxesEnd = boardAxisHasSumCount newBoard blockSum
            blockAxesEnd < blockAxesStart
        
        // True if the move increases the number of potential winning moves
        let moveForks (move: Move): bool =
            let forkSum = (2 * computerValue)
            let forkAxesStart = boardAxisHasSumCount board forkSum

            let newBoard = makeMove move
            let forkAxesEnd = boardAxisHasSumCount newBoard forkSum
            forkAxesEnd > forkAxesStart
        
        // True if the move decreases the number of potential winning moves
        // for the opponent
        let moveBlocksFork (move: Move): bool = 
            let forkBlockSum = (2 * userValue)
            let forkBlockAxesStart = boardAxisHasSumCount board forkBlockSum

            let newBoard = makeMove move
            let forkBlockAxesEnd = boardAxisHasSumCount newBoard forkBlockSum
            forkBlockAxesStart < forkBlockAxesEnd
        
        // True if the move would take the centre square
        let moveTakesCentre (move: Move): bool =
            move = 4
        
        // True if the move takes the index of a corner, the opposite of which
        // is occupied by the player
        let moveTakesOppositeCorner (move: Move): bool =
            let isCorner = 
                let cornerIndices = [0; 2; 6; 8]
                List.contains move cornerIndices
            let userHasDimetricCorner =
                let dimetricIndex = abs (move - 8)
                board.cells[dimetricIndex] = userValue
            
            isCorner && userHasDimetricCorner
        
        // True for any empty corner
        let moveTakesEmptyCorner (move: Move): bool =
            let cornerIndices = [0; 2; 6; 8]
            List.contains move cornerIndices
        
        // True for any empty side
        let moveTakesEmptySide (move: Move): bool =
            let sideIndices = [1; 3; 5; 7]
            List.contains move sideIndices

        // List of move functors, in order of importance
        let moveFunctors: list<Move -> bool> = 
            [
                moveWins
                moveBlocks
                moveForks
                moveBlocksFork
                moveTakesCentre
                moveTakesOppositeCorner
                moveTakesEmptyCorner
                moveTakesEmptySide
            ]

        // Checks whether the given functor can succeed
        let possibleMoves = boardEmptyIndices board
        let functorSucceeds (functor: Move -> bool): option<Move> =
            List.tryFind functor possibleMoves
        
        // Takes the move from the highest-priority funcor which succeeded
        let successfulFunctors = List.choose functorSucceeds moveFunctors
        successfulFunctors.Head
    
    let getMove (state: State): Move =
        let moveFunctor =
            match state.playerTurn with 
                | Player.USER -> getUserMove
                | Player.COMPUTER -> getComputerMove
        moveFunctor state.board

    let turn (state: State): State =
        boardDisplay state.board
        let move = getMove state
        stateAdvance state move

    let rec play (state: State): Outcome = 
        let newState = turn state

        let outcome = boardOutcome newState.board state.playerTurn
        if outcome.IsSome then outcome.Value
        else play newState
