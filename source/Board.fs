namespace Neasden

type Outcome =
    | WIN
    | LOSS
    | DRAW

type Board =
    {
        cells: list<int>
        cursor: option<int>
    }

module Board =

    // List of lists of indices corresponding to the axes on the board 
    // (ie: columns, rows, and diagonals)
    let private axes (): list<list<int>> =
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
    
    // Gets the sum of the cells along each axis
    let private axesSums (board: Board): list<int> =
        let axisSum (axisIndices: list<int>): int =
            List.sumBy (fun index -> board.cells[index]) axisIndices
        List.map axisSum (axes ())
    
    // True if the board doesn't have any empty cells
    let private full (board: Board): bool =
        not <| List.contains 0 board.cells

    // Creates an empty board
    let create (): Board =
        {
            cells = [for _ in 0..8 -> 0]
            cursor = None
        }
    
    // Makes a move on the board, updating the cells & cursor as required
    let makeMove (board: Board) (move: Move) (player: Player): Board =
        
        let playerValue = Player.value player
        if move.confirmed then 
            {
                cells = List.updateAt move.index playerValue board.cells
                cursor = None
            }
        else 
            {
                cells = board.cells
                cursor = Some move.index
            }
    
    // Displays a given board
    let display (board: Board): unit =

        let columns = 3
        let rows = 3
        let cellWidth = 3
        let cellHeight = 1

        let realWidth = cellWidth + 1
        let realHeight = cellHeight + 1

        let getCharacter ((x, y): int * int): char =

            // Gets the relevant box-drawing character for the given (x, y) 
            // position
            let getBoxCharacter: char =

                // '─' '│'
                // '┌' '┬' '┐'
                // '├' '┼' '┤'
                // '└' '┴' '┘'

                // Whether the (x, y) position corresponds to a grid line
                let onGridX = (x % realWidth = 0)
                let onGridY = (y % realHeight = 0)

                // Whether the grid join lies at the start, middle, or end of 
                // its line
                let firstX = (x = 0)
                let firstY = (y = 0)
                let lastX = (x = columns * realWidth)
                let lastY = (y = rows * realHeight)
                let middleX = not firstX && not lastX
                let middleY = not firstY && not lastY

                // Filler character
                if not onGridX && not onGridY then ' '
                
                // Sides
                elif onGridX && not onGridY then '│'
                elif not onGridX && onGridY then '─'

                // Corners
                elif firstX && firstY then '┌'
                elif firstX && lastY then '└'
                elif lastX && firstY then '┐'
                elif lastX && lastY then '┘'

                // Joins
                elif firstX && middleY then '├'
                elif lastX && middleY then '┤'
                elif firstY && middleX then '┬'
                elif lastY && middleX then '┴'
                else '┼'
            
            // True if the (x, y) *drawing* position corresponds to the centre of 
            // a cell
            let inCell = 
                let inCellX = (((x - realWidth / 2) % realWidth) = 0)
                let inCellY = (((y - realHeight / 2) % realHeight) = 0)
                inCellX && inCellY
            
            // If in a cell, draw its value (ie: a nought or a cross)
            if inCell then 

                // Get the index in the board list from the current drawing 
                // position
                let cellIndex = 
                    let cellX = int ((x - realWidth / 2) / realWidth)
                    let cellY = int ((y - realHeight / 2) / realHeight)
                    cellY * columns + cellX

                // Get the index of the cursor, if there is one
                let cursorIndex =
                    if board.cursor.IsNone then -1
                    else board.cursor.Value
                
                if cellIndex = cursorIndex then 
                    '@'
                else 
                    match board.cells[cellIndex] with 
                        | 0 -> ' '
                        | 1 -> 'x' 
                        | -1 -> 'o'
                        | _ -> '?'
                
            // Otherwise, get the box drawing character for this position
            else 
                getBoxCharacter
        
        // Save cursor position
        Console.saveCursor ()
        
        // Driver for drawing each x, y position in the grid
        let width = columns * realWidth
        let height = rows * realHeight
        for y in 0..(height) do
            for x in 0..(width) do
                let character = getCharacter (x, y)
                System.Console.Write(character)
            System.Console.Write('\n')

        // Restore cursor position
        Console.restoreCursor ()
    
    // Determine the outcome of a game for a given player, or None if an 
    // outcome hasn't been reached yet
    let outcomeForPlayer (board: Board) (player: Player): option<Outcome> =

        // Values for the player whose outcome we're evaluating to either win
        // or lose
        let playerValue = Player.value player
        let playerWinValue = 3 * playerValue
        let playerLossValue = -3 * playerValue

        // Check if the values are present, or if the board is full. Failing 
        // those, the game hasn't reached an outcome yet
        let sums = axesSums board
        if List.contains playerWinValue sums then Some Outcome.WIN
        elif List.contains playerLossValue sums then Some Outcome.LOSS
        elif full board then Some Outcome.DRAW
        else None
    
    // Checks whether the board has one (or more) axes with a given sum
    let hasAxisSum (board: Board) (sum: int): bool =
        List.contains sum (axesSums board)
    
    // Counts the number of axes with a given sum
    let axesWithSum (board: Board) (sum: int): int =
        let axesSums = axesSums board
        let axisHasSum (index: int): option<int> =
            if axesSums[index] = sum then Some index
            else None
        (List.choose axisHasSum [0..7]).Length
    
    // Gets the index of the first empty cell available on the board
    let firstEmptyIndex (board: Board): option<int> =
        if full board then 
            None 
        else 
            Some (List.findIndex (fun index -> board.cells[index] = 0) [0..8])

    // Gets a list of the indices with no value assigned to them
    let emptyIndices (board: Board): list<int> =
        let indexEmpty (index: int): option<int> =
            if board.cells[index] = 0 then Some index
            else None
        List.choose indexEmpty [0..8]