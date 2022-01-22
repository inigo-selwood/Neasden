namespace Balham.Game

open Coordinates

module Ship =

    type Type = {
        name: string
        pivot: Position
        offset: Vector
        shape: Vector
    }

    // Creates a ship
    let create (name: string) (length: int): Type =
        let shape = vectorCreatePolar Direction.UP length

        {
            name = name; 
            pivot = positionCreate (0, 0);
            offset = vectorDivide shape 2;
            shape = shape;
        }
    
    let display 
            (ship: Type) 
            (shots: list<Position>) 
            (boardOffset: Vector): unit =

        

    // List of all ships typically used in a game of Battleship
    let loadout (): list<Type> =
        [
            create "carrier"      5;
            create "battleship"   4;
            create "destroyer"    3;
            create "submarine"    3;
            create "patrol board" 2;
        ]
    
    // Performs a Tetris-like wall-kick to make sure the ship is in the playing
    // field.
    let wallKick (ship: Type): Type =
        let bow = positionAddVector ship.pivot ship.offset
        let stern = positionAddVector bow ship.shape

        let xMinimum = min bow.x stern.x
        let yMinimum = min bow.y stern.y
        let xMaximum = max bow.x stern.x
        let yMaximum = max bow.y stern.y

        let lowerBound, upperBound = (0, 9)
        let boundedDelta (minimum: int) (maximum: int): int =
            
            if minimum < lowerBound then
                lowerBound - minimum
            elif maximum > upperBound then
                upperBound - maximum
            else
                0

        let deltaX = boundedDelta xMinimum xMaximum
        let deltaY = boundedDelta yMinimum yMaximum

        let delta = vectorCreateCartesian (deltaX, deltaY)
        if vectorLength delta <> 0 then
            System.Console.Beep()
        
        {
            name = ship.name;
            pivot = positionAddVector ship.pivot delta;
            offset = ship.offset;
            shape = ship.shape;
        }
    
    // Moves the ship one unit in a given direction
    let move (ship: Type) (direction: Direction): Type =
        let delta = vectorCreatePolar direction 1
        
        let movedShip = 
            {
                name = ship.name;
                pivot = positionAddVector ship.pivot delta;
                offset = ship.offset;
                shape = ship.shape;
            }
        
        wallKick movedShip

    // Rotates the ship to face a given direction
    let rotate (ship: Type) (direction: Direction): Type =
        let newOffset = 
            vectorCreatePolar 
            <| direction 
            <| vectorLength ship.offset
        let newShape =
            vectorCreatePolar
            <| directionOpposite direction
            <| vectorLength ship.shape
        
        let rotatedShip = {
            name = ship.name;
            pivot = ship.pivot;
            offset = newOffset;
            shape = newShape;
        }

        wallKick rotatedShip