namespace Balham.Game

module Coordinates =

    type Direction = 
        | UP
        | DOWN
        | LEFT
        | RIGHT
    
    type Orientation = 
        | NORTH
        | EAST
        | SOUTH
        | WEST

    type Position = {
        x: int
        y: int
    }

    type Vector = {
        x: int
        y: int
    }

    let directionOpposite (direction: Direction): Direction =
        match direction with 
            | Direction.UP    -> Direction.DOWN
            | Direction.DOWN  -> Direction.UP
            | Direction.LEFT  -> Direction.RIGHT
            | Direction.RIGHT -> Direction.LEFT
    
    let positionCreate ((x, y): int * int): Position =
        {x = x; y = y}
    
    let positionCreateVector (vector: Vector): Position =
        {
            x = vector.x;
            y = vector.y;
        }
    
    let positionIntersectsVector (position: Position) (vector: Vector): bool = 
        false
    
    let vectorsIntersect (vectorOne: Vector) (vectorTwo: Vector): bool =
        false
    
    let positionAddVector (position: Position) (vector: Vector): Position =
        {
            x = position.x + vector.x;
            y = position.y + vector.y;
        }
    
    let vectorDirection (vector: Vector): option<Direction> =
        if vector.x > 0 && vector.y = 0 then
            some Direction.RIGHT
        elif vector.x < 0 && vector.y = 0 then
            some Direction.LEFT
        elif vector.x = 0 && vector.y > 0 then
            some Direction.DOWN
        elif vector.x = 0 && vector.y < 0 then
            some Direction.UP
        else
            None
    
    let vectorCreateCartesian ((x, y): int * int): Vector = 
        {x = x; y = y}
    
    let vectorCreatePolar (direction: Direction) (length: int): Vector =
        match direction with 
            | Direction.UP    -> {x = 0; y = -length}
            | Direction.DOWN  -> {x = 0; y = length}
            | Direction.LEFT  -> {x = -length; y = 0}
            | Direction.RIGHT -> {x = length; y = 0}
    
    let vectorCreatePositionDelta 
            (positionOne: Position) 
            (positionTwo: Position): Vector =
        
        {
            x = positionOne.x - positionTwo.x;
            y = positionOne.y - positionTwo.y;
        }
        
    let vectorAdd (vectorOne: Vector) (vectorTwo: Vector): Vector =
        {
            x = vectorOne.x + vectorTwo.x
            y = vectorOne.y + vectorTwo.y
        }
        
    let vectorDivide (vector: Vector) (divisor: int): Vector =
        let x = floor <| float (vector.x / divisor)
        let y = floor <| float (vector.y / divisor)

        {
            x = int x;
            y = int y;
        }
    
    let vectorLength (vector: Vector): int =
        let xSquared = (float vector.x) ** 2.0
        let ySquared = (float vector.y) ** 2.0
        sqrt (xSquared + ySquared) |> int
    
