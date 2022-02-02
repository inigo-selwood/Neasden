namespace Neasden

module Console =

    let private sendCommand (command: string): unit =
        System.Console.Write((string (char 0x1b)) + command)

    let saveCursor (): unit =
        sendCommand "[s"
    
    let restoreCursor (): unit =
        sendCommand "[u"
    
    let clearToEnd (): unit =
        sendCommand "[0J"