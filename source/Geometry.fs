namespace Neasden

module Geometry =

    type Direction = 
        | UP
        | DOWN
        | LEFT
        | RIGHT

    type Point =
        {
            x: int
            y: int
        }
    
    let move (point: Point) (direction: Direction): Point =
        let x, y =
            match direction with 
                | Direction.UP -> (point.x, point.y - 1)
                | Direction.DOWN -> (point.x, point.y + 1)
                | Direction.LEFT -> (point.x - 1, point.y)
                | Direction.RIGHT -> (point.x + 1, point.y)
        
        {
            x = x
            y = y
        }