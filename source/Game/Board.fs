namespace Balham.Game

module Board =

    type Type = {
        ships: list<Ship>
    }

    // An empty board
    let empty (): Type = 
        {ships = []}
    
    // Display a board
    let display (board: Type): unit =
        for ship in board.ships:
            Ship.display ship
    
    // Gets user input to create a board loadout
    let setup (): Type =

        let placementValid (board: Type) (ship: Ship.Type): bool =


        let rec addShips (board: Type) (ships: list<Ship.Type>): Type =

            let newBoard = board
            let newShips = ships.Tail
            if not <| List.isEmpty newShips then
                addShips newBoard newShips
            else
                newBoard
            
        addShips
        <| empty ()
        <| Ship.loadout ()