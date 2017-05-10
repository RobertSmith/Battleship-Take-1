namespace Battleship

open System

module Models =

    type Coordinates = {
        Row: int
        Column: int
    }
    
    type OccupationType = 
        | Empty
        | Carrier
        | Battleship
        | Cruiser
        | Destroyer
        | Submarine
        | Hit
        | Miss

    type Panel(coordinates) = 
        let mutable occupationType = OccupationType.Empty
        
        member this.Coordinates = coordinates
        member this.OccupationType = occupationType
        member this.setOccupationType occType =
            occupationType <- occType
        member this.isRandomAvailable =
            (this.Coordinates.Row % 2 = 0 && this.Coordinates.Column % 2 = 0) || (this.Coordinates.Row % 2 = 1 && this.Coordinates.Column % 2 = 1)
        member this.getStatus = 
            match occupationType with 
                | Empty -> "_" 
                | Carrier -> "A"
                | Battleship -> "B"
                | Cruiser -> "C"
                | Destroyer -> "D"
                | Submarine -> "S"
                | Hit -> "H"
                | Miss -> "M"
        member this.isOccupied = 
            match occupationType with
                | Empty -> false
                | Miss -> false
                | _ -> true
  
    type GameBoard() = 
        let mutable board = Seq.empty

        do
            for x in 1 .. 10 do
                for y in 1 .. 10 do
                    let p = new Panel({ Row = x; Column = y })
                    board <- Seq.append board [p]

        member this.Board = board

    type FiringBoard() = 
        let mutable board = Seq.empty

        do
            for x in 1 .. 10 do
                for y in 1 .. 10 do
                    let p = new Panel({ Row = x; Column = y })
                    board <- Seq.append board [p]

        member this.Board = board
        member this.getOpenRandomPanels = 
            Seq.filter (fun (x:Panel) -> x.OccupationType = OccupationType.Empty && x.isRandomAvailable) board
        member this.getNeighbors coordinates =
            Seq.filter (fun (x:Panel) -> x.Coordinates.Column = coordinates.Column && x.Coordinates.Row = coordinates.Row - 1 || 
                                         x.Coordinates.Column = coordinates.Column && x.Coordinates.Row = coordinates.Row + 1 ||
                                         x.Coordinates.Column = coordinates.Column - 1 && x.Coordinates.Row = coordinates.Row ||
                                         x.Coordinates.Column = coordinates.Column + 1 && x.Coordinates.Row = coordinates.Row) board
        member this.getHitNeighbors =
            let mutable neighbors = Seq.empty
            let hits = Seq.filter (fun (x:Panel) -> x.OccupationType = OccupationType.Hit) board
            for hit in hits do
                neighbors <- Seq.append neighbors (this.getNeighbors hit.Coordinates)
        
            neighbors <- Seq.filter (fun (x:Panel) -> x.OccupationType = OccupationType.Empty) neighbors
            Seq.distinct neighbors

    type Ship(occupationType, width) =
        let mutable hits = 0
        let setShipType occupationType =
            match occupationType with
                | Miss -> OccupationType.Empty
                | Hit -> OccupationType.Empty
                | _ -> occupationType
        let mutable shipType = setShipType occupationType

        member this.Width = width
        member this.isSunk = 
            hits >= this.Width
        member this.recordHit =
            hits <- hits + 1
        member this.OccupationType = 
            shipType

    type Player(name: string) = 
        let mutable gameBoard = new GameBoard()
        let mutable firingBoard = new FiringBoard()
 
        let ships = [
            Ship(OccupationType.Carrier, 5)
            Ship(OccupationType.Battleship, 4) 
            Ship(OccupationType.Cruiser, 3)
            Ship(OccupationType.Submarine, 3) 
            Ship(OccupationType.Destroyer, 2)]

        member this.randomShot =
            let random = new Random(Guid.NewGuid().GetHashCode())
            let openPanels = firingBoard.getOpenRandomPanels
            let panelId = random.Next(Seq.length openPanels)
            let shot = Seq.item panelId openPanels
            printfn """%s says: "%i, %i" """ this.Name shot.Coordinates.Row shot.Coordinates.Column
            shot

        member this.searchingShot =
            let random = new Random(Guid.NewGuid().GetHashCode())
            let openPanels = firingBoard.getHitNeighbors
            let panelId = random.Next(Seq.length openPanels)
            let shot = Seq.item panelId openPanels
            printfn """%s says: "%i, %i" """ this.Name shot.Coordinates.Row shot.Coordinates.Column
            shot

        member this.Name = name
        member this.GameBoard = gameBoard
        member this.FiringBoard = firingBoard
        member this.Ships = ships
        member this.hasLost =
            let validShips = List.filter (fun (x:Ship) -> x.isSunk = false) ships
            validShips.Length = 0
        
        member this.fireShot =
            let openPanels = firingBoard.getHitNeighbors
            match Seq.length openPanels with
                | 0 -> this.randomShot
                | _ -> this.searchingShot

        member this.processShot (panel:Panel) =
            let ourPanel = Seq.find (fun (x:Panel) -> x.Coordinates.Row = panel.Coordinates.Row && x.Coordinates.Column = panel.Coordinates.Column) gameBoard.Board
            
            if (ourPanel.isOccupied) then
                let ship = List.find (fun (x:Ship) -> x.OccupationType = ourPanel.OccupationType) ships
                ship.recordHit
                printfn """%s says: "Hit!" """ this.Name
                
                if (ship.isSunk) then
                    printfn """%s says: "You sunk my %A!" """ this.Name ship.OccupationType

                OccupationType.Hit
            else
                printfn """%s says: "Miss!" """ this.Name
                OccupationType.Miss

        member this.processShotResult (panel:Panel) occupationType =
            let ourPanel = Seq.find (fun (x:Panel) -> x.Coordinates.Row = panel.Coordinates.Row && x.Coordinates.Column = panel.Coordinates.Column) firingBoard.Board
            match occupationType with
                | Hit -> ourPanel.setOccupationType OccupationType.Hit
                | Miss -> ourPanel.setOccupationType OccupationType.Miss
                | _ -> ourPanel.setOccupationType OccupationType.Empty

    type Game(player1: Player, player2: Player) =
        let player1 = player1
        let player2 = player2

        member this.Player1 = player1
        member this.Player2 = player2

        member this.placeShips (player: Player) =
            let random = new Random(Guid.NewGuid().GetHashCode())

            for ship in player.Ships do
                let mutable isFinished = false;

                while isFinished = false do
                    let startColumn = random.Next(1, 11);
                    let startRow = random.Next(1, 11);
                    let mutable endColumn = startColumn
                    let mutable endRow = startRow

                    let orientation = random.Next(1, 101) % 2;

                    if orientation = 0 then
                        endRow <- startRow + ship.Width - 1
                    else
                        endColumn <- endColumn + ship.Width - 1

                    if (endRow <= 10 && endColumn <= 10) then
                        let affectedPanels = Seq.filter (fun (p:Panel) -> (p.Coordinates.Row >= startRow 
                                                                        && p.Coordinates.Column >= startColumn 
                                                                        && p.Coordinates.Row <= endRow 
                                                                        && p.Coordinates.Column <= endColumn)) player.GameBoard.Board
                        let isFree = Seq.filter (fun (x:Panel) -> (x.isOccupied)) affectedPanels

                        if Seq.length isFree = 0 then
                            for panel in affectedPanels do
                                panel.setOccupationType ship.OccupationType
                            isFinished <- true

 
        member this.printBoards (player:Player) =
            printfn "%s" (player.Name)

            for x in 1 .. 10 do
                for y in 1 .. 10 do
                    let p = Seq.find (fun (z:Panel) -> z.Coordinates.Row = x && z.Coordinates.Column = y) player.GameBoard.Board
                    Console.Write((p.getStatus) + " ");
        
                Console.Write("                 ");

                for y in 1 .. 10 do
                    let p = Seq.find (fun (z:Panel) -> z.Coordinates.Row = x && z.Coordinates.Column = y) player.FiringBoard.Board
                    Console.Write((p.getStatus) + " ");

                Console.WriteLine()
            Console.WriteLine()

        member this.playRound =
            let coordinates = player1.fireShot
            let result = player2.processShot coordinates
            player1.processShotResult coordinates result

            if (player2.hasLost = false) then
                let coordinates = player2.fireShot
                let result = player1.processShot coordinates
                player2.processShotResult coordinates result
        
        member this.playToEnd =
            while (player1.hasLost = false && player2.hasLost = false) do
                this.playRound

            if (player1.hasLost) then
                printfn "%s has won the game!" player2.Name
            else
                printfn "%s has won the game!" player1.Name
