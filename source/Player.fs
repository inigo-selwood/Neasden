namespace Neasden

type Player =
    | USER
    | COMPUTER

module Player =

    // Gets the value assigned to a given player
    let value (player: Player): int =
        match player with 
            | Player.USER -> 1
            | Player.COMPUTER -> -1

    // Gets whichever player is not the one provided
    // Like a bitwise NOT operator, but for the Player type
    let other (player: Player): Player =
        match player with 
            | Player.USER -> Player.COMPUTER
            | Player.COMPUTER -> Player.USER