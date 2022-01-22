namespace Balham.Game

open Balham

module Cell =

    type Type = 
        | EMPTY
        | SHIP
        | SHOT_HIT
        | SHOT_MISS
    
    let icon (state: Type): string =

        let text, colour = 
            match state with
                | Type.EMPTY     -> ("•", (0, 0, 255))
                | Type.SHIP      -> ("■", (255, 255, 255))
                | Type.SHOT_HIT  -> ("■", (255, 0, 0))
                | Type.SHOT_MISS -> ("•", (255, 128, 128))
        
        Console.embedColour text <| Console.colourCreateRGB colour