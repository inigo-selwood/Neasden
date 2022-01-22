namespace Balham

module Console =

    type Colour = {
        red: int
        green: int
        blue: int
    }

    let colourCreateRGB ((red, green, blue): int * int * int): Colour =

        let lowerBound, upperBound = (0, 255)
        let boundValue (value: int): int =
            if value < lowerBound then
                lowerBound
            elif value > upperBound then
                upperBound
            else
                value
        
        {
            red = boundValue red;
            green = boundValue green;
            blue = boundValue blue;
        }

    let embedColour (text: string) (colour: Colour): string =
        let escape = string (char 0x1B)
        let setColour =
            sprintf "[38;2;%d;%d;%dm" colour.red colour.green colour.blue
        let resetColour = "[0m"
        escape + setColour + text + escape + resetColour
    
    let writeXY (text: string) ((x, y): int * int): unit = 
        System.Console.SetCursorPosition(x, y)
        System.Console.Write(text)