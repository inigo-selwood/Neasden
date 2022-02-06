namespace Neasden

module Game =
    
    type Key = string

    type Direction =
        | UP 
        | DOWN 
        | LEFT 
        | RIGHT

    // Gets a move, made by the player
    let getUserMove (board: Board): Move =
    
        
        // If the board doesn't have a cursor yet, then create one
        if board.cursor.IsNone then 
            {
                index = (Board.firstEmptyIndex board).Value
                confirmed = false
            }
        
        // Handle user input
        else

            // Gets a key from the console
            let getKey (): Key =
                System.Console.ReadKey(true).Key.ToString()
            
            // True if the given key is an arrow key
            let isArrowKey (key: Key): bool =
                match key with 
                    | "UpArrow"
                    | "DownArrow"
                    | "LeftArrow"
                    | "RightArrow" -> true
                    | _ -> false

            // Get the cursor position, and wait for key input from the user
            let cursor = board.cursor.Value
            let key = getKey ()
            if isArrowKey key then 
            
                // Get the direction corresponding to a given key
                let keyDirection (key: Key): option<Direction> =
                    match key with 
                        | "UpArrow" -> Some Direction.UP
                        | "DownArrow" -> Some Direction.DOWN
                        | "LeftArrow" -> Some Direction.LEFT
                        | "RightArrow" -> Some Direction.RIGHT
                        | _ -> None
                
                // Create a new cursor by moving the current one in a given
                // direction -- or None if the new index would be out-of-bounds
                let moveCursor (direction: Direction): option<int> =
                    let newX, newY =
                        let x, y = (cursor % 3, cursor / 3)
                        match direction with 
                            | Direction.UP -> (x, y - 1)
                            | Direction.DOWN -> (x, y + 1)
                            | Direction.LEFT -> (x - 1, y)
                            | Direction.RIGHT -> (x + 1, y)

                    let inBounds: bool =
                        newX >= 0 && newX < 3 && newY >= 0 && newY < 3
                    
                    if not inBounds then None 
                    else Some (newX + newY * 3)
                
                // Try to move the cursor in the desired direction, sound the
                // console bell if the movement requested would take the cursor
                // out of bounds
                let newCursor = moveCursor (keyDirection key).Value
                if newCursor.IsNone then 
                    System.Console.Beep ()
                    {
                        index = cursor
                        confirmed = false
                    }
                else
                    {
                        index = newCursor.Value
                        confirmed = false
                    }
            
            // When the enter key is hit, confirm the current position of the
            // cursor
            elif key = "Enter" then 

                // Check the cell selected isn't already occupied
                if board.cells[cursor] = 0 then
                    {
                        index = cursor
                        confirmed = true
                    }
                
                // Otherwise, send a console beep and don't confirm the move
                else 
                    System.Console.Beep ()
                    {
                        index = cursor 
                        confirmed = false
                    }
            
            // For any other key, sound the console bell, and don't move the 
            // cursor
            else 
                System.Console.Beep ()
                {
                    index = cursor
                    confirmed = false
                }
    
    // Evaluate the best possible move for the computer to make
    // Noughts-and-crosses is a solved game, so there's an algorithm for this
    let getComputerMove (board: Board): Move =
        
        // Makes the given move, returning a fresh board
        let getUpdatedBoard (moveIndex: int): Board =
            let move = {
                    index = moveIndex
                    confirmed = true
                }
            Board.makeMove board move Player.COMPUTER
        
        // List of moves actionable by the computer player
        let moveFunctors: list<int -> bool> =

            let computerValue = Player.value Player.COMPUTER
            let userValue = Player.value Player.USER

            // True if a move will win the game
            let moveWins (moveIndex: int): bool =
                let newBoard = getUpdatedBoard moveIndex
                let winSum = 3 * computerValue
                Board.hasAxisSum newBoard winSum
            
            // True if a move blocks the user from getting 3 in a row
            let moveBlocks (moveIndex: int): bool =
                let blockSum = (2 * userValue)
                let blockAxesStart = Board.axesWithSum board blockSum

                let newBoard = getUpdatedBoard moveIndex
                let blockAxesEnd = Board.axesWithSum newBoard blockSum
                blockAxesEnd < blockAxesStart
            
            // True if the move increases the number of potential winning moves
            let moveForks (moveIndex: int): bool =
                let forkSum = (2 * computerValue)
                let forkAxesStart = Board.axesWithSum board forkSum

                let newBoard = getUpdatedBoard moveIndex
                let forkAxesEnd = Board.axesWithSum newBoard forkSum
                forkAxesEnd > forkAxesStart
            
            // True if the move decreases the number of potential winning moves
            // for the opponent
            let moveBlocksFork (moveIndex: int): bool = 
                let forkBlockSum = (2 * userValue)
                let forkBlockAxesStart = Board.axesWithSum board forkBlockSum

                let newBoard = getUpdatedBoard moveIndex
                let forkBlockAxesEnd = Board.axesWithSum newBoard forkBlockSum
                forkBlockAxesStart < forkBlockAxesEnd
            
            // True if the move would take the centre square
            let moveTakesCentre (moveIndex: int): bool =
                moveIndex = 4
            
            // True if the move takes the index of a corner, the opposite of 
            // which is occupied by the player
            let moveTakesOppositeCorner (moveIndex: int): bool =
                let isCorner = 
                    let cornerIndices = [0; 2; 6; 8]
                    List.contains moveIndex cornerIndices
                let userHasDimetricCorner =
                    let dimetricIndex = abs (moveIndex - 8)
                    board.cells[dimetricIndex] = userValue
                
                isCorner && userHasDimetricCorner
            
            // True for any empty corner
            let moveTakesEmptyCorner (moveIndex: int): bool =
                let cornerIndices = [0; 2; 6; 8]
                List.contains moveIndex cornerIndices
            
            // True for any empty side
            let moveTakesEmptySide (moveIndex: int): bool =
                let sideIndices = [1; 3; 5; 7]
                List.contains moveIndex sideIndices

            // List the move functors in order of which ones are most important
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
        
        // Gets a list of moves, ranked in order of those which do the most to 
        // advantage the computer
        let viableMovesIndices = 
            let availableCells = Board.emptyIndices board
            let moveSucceeds (functor: int -> bool): option<int> =
                List.tryFind functor availableCells
            List.choose moveSucceeds moveFunctors
        
        // Take the most advantageous of all the viable move indices, and wrap
        // it up in a move record
        {
            index = viableMovesIndices.Head
            confirmed = true
        }

    // Take a game-turn for a given player
    let turn (state: State): State =
        let moveFunctor = 
            match state.player with 
                | Player.USER -> getUserMove
                | Player.COMPUTER -> getComputerMove
        let move = moveFunctor state.board
        State.advance state move

    // Play the game
    let play (): unit = 

        let consoleOrigin = Console.getCursorXY ()

        let rec loop (state: State): Outcome =
            Console.setCursorXY consoleOrigin
            Board.display state.board

            let newState = turn state
            let outcome = Board.outcomeForPlayer newState.board Player.USER
    
            if outcome.IsNone then loop newState
            else outcome.Value
        
        let startState = State.create ()
        let outcome = loop startState
        
        // Let the player know how the game ended
        let outcomeMessage =
            match outcome with 
                | Outcome.WIN -> "you won, congratulations!\n"
                | Outcome.LOSS -> "you lost, comiserations.\n"
                | Outcome.DRAW -> "the game was a draw, better luck next time\n"
        
        System.Console.Write(outcomeMessage)
        ()

