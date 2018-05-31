open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Position = int * int
type Rotated = bool
type Length = int
type ShipType =
    | Carrier
    | Battleship
    | Cruiser
    | Destroyer
    | Submarine
type Ship = {pos: Position; rot: Rotated; ship: ShipType}
type GamePhase =
    | PlayerPlaceShip
    | EnemyPlaceShip
    | PlayerShoot 
    | EnemyShoot
    | PlayerWin
    | EnemyWin
type ShipGame = {
    playerShips: Ship list; enemyShips: Ship list;
    shotsOnPlayer: Set<Position>; shotsOnEnemy: Set<Position>;
    phase: GamePhase 
} 
type Stateful<'T> = 
    {state: 'T}
    member this.Map f =
        { this with state = f this.state}
    member this.Bind f = f this.state

type MouseClickPosition = int * int
type BoardParameters = {tileWidth: int; leftMargin: int; topMargin: int;}

type Shot =
    | HitOnPlayer
    | HitOnEnemy
    | MissOnPlayer
    | MissOnEnemy

let shipLength ship = 
    match ship.ship with
    | Carrier -> 5
    | Battleship -> 4
    | Cruiser -> 3
    | Destroyer -> 2
    | Submarine -> 1

let shipTiles (ship: Ship): Position list =
    let t = [1..shipLength ship]
    let shipOrigin = ship.pos
    if ship.rot 
        then
            t |> List.map (fun i -> i - 1 + (fst ship.pos), snd shipOrigin)
        else 
            t |> List.map (fun i -> fst shipOrigin, i - 1 + snd shipOrigin)

let shipTilesWithAdjacent (ship: Ship): Position list =
    shipTiles ship |> List.collect (fun (x,y) -> [x-1,y; x+1,y; x,y+1; x,y-1]) |> List.append (shipTiles ship) |>  List.distinct

let setShipPosition (tile: Option<Position>) (ship: Ship): Ship =
    match tile with
    | Option.Some tile -> { ship with pos = tile }
    | None -> ship

let flipShipRotation (ship: Ship): Ship =
    { ship with rot = not ship.rot }


let board = [ for x in 0..9 do
              for y in 0..9 -> (x, y) ]

let getShip = { pos = (0,0); rot = false; ship = Cruiser }

let addPlayerShip (ship: Ship) (game: ShipGame): ShipGame =
    if shipTiles ship |> List.forall 
        (fun tile -> 
            List.contains tile board &&
            not (List.contains tile (game.playerShips |> List.collect shipTilesWithAdjacent)))
    then
       { game with playerShips = ship :: game.playerShips }
    else
        game

let placeEnemyShips (game: ShipGame): ShipGame =
    let enemyShips =[
        {pos = (0,0); rot = false; ship = Battleship};
        {pos = (2,0); rot = false; ship = Cruiser};
        {pos = (4,0); rot = false; ship = Cruiser};
        {pos = (0,6); rot = false; ship = Destroyer};
        {pos = (2,6); rot = false; ship = Destroyer};
        {pos = (4,6); rot = false; ship = Destroyer};
        {pos = (0,9); rot = false; ship = Submarine};
        {pos = (2,9); rot = false; ship = Submarine};
        {pos = (4,9); rot = false; ship = Submarine};
        {pos = (6,9); rot = false; ship = Submarine}
    ]
    { game with enemyShips = enemyShips; phase = PlayerShoot }


let toStateful shipgame = { state = shipgame }
let applyToState f (stateful: Stateful<'T>) = stateful.Map(f)

let gridNumToLetter x =
    match x with
    | 1 -> "A"
    | 2 -> "B"
    | 3 -> "C"
    | 4 -> "D"
    | 5 -> "E"
    | 6 -> "F"
    | 7 -> "G"
    | 8 -> "H"
    | 9 -> "I"
    | 10 -> "J"
    | _ -> "?"

let boardParam = {tileWidth = 60; leftMargin = 60; topMargin = 80}

let coordinateToTile (c: MouseClickPosition): Option<Position> =
    let mx, my = c
    let x = (mx - 60) / 60
    let y = (my - 80) / 60
    let tile = (x,y)
    match List.contains tile board with
    | true -> Option.Some tile
    | false -> Option.None

let coordinateToEnemyTile c =
    let x, y = c
    coordinateToTile (x - boardParam.tileWidth * 11, y)

let nextShipToPlace (game: ShipGame) =
    let shipsToPlace = [(Battleship, 1); (Cruiser, 2); (Destroyer, 3); (Submarine, 4)]
    let placedShips = game.playerShips
    shipsToPlace |> List.map (fun (a, b) -> (a, b - (placedShips |> List.filter (fun x -> x.ship = a) |> List.length))) |>
    List.filter (fun (a,b) -> b > 0) |> function | [] -> Option.None 
                                                 | l -> List.head l |> fun y -> { pos = (0,0); rot = false; ship = fst y } |> Option.Some

let shootAtEnemy (p: Position) (game: ShipGame): ShipGame =
    { game with shotsOnEnemy = Set.add p game.shotsOnEnemy; phase = EnemyShoot }

let shootAtPlayer (game: ShipGame): ShipGame =
    let nonShotTiles = Set.difference (Set.ofList board) game.shotsOnPlayer
    let randomShuffle (r: Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())
    let nextTileToBeShot = nonShotTiles |> randomShuffle (Random()) |> Seq.head
    { game with
        shotsOnPlayer = Set.add nextTileToBeShot game.shotsOnPlayer;
        phase = PlayerShoot }

let sunkShips tilesShot ships =
    ships |> List.filter (fun s -> Set.isSubset (shipTiles s |> Set.ofList) tilesShot)

let isGameOver (game: ShipGame): ShipGame = 
    let shipTilesRemaining ships shotTiles = Set.difference (ships |> List.collect shipTiles |> Set.ofList) shotTiles
    let playerSTR = shipTilesRemaining game.playerShips game.shotsOnPlayer
    let enemySTR = shipTilesRemaining game.enemyShips game.shotsOnEnemy
    let hasWon = (Set.isEmpty playerSTR, Set.isEmpty enemySTR)
    match hasWon with
    | (true, false)  -> { game with phase = EnemyWin  }
    | (false, true)  -> { game with phase = PlayerWin }
    | _              ->   game

let vector32 a =
    let x,y = a
    Vector2(float32 x, float32 y)

let vector a =
    let x,y = a
    Vector2(x, y)

// IMPURE ZONE BEGINS HERE

type Game1 () as x =
    inherit Game()
    do x.Content.RootDirectory <- "Content"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch : SpriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable gridTexture = Unchecked.defaultof<Texture2D>

    let mutable carrierTexture    = Unchecked.defaultof<Texture2D>
    let mutable battleshipTexture = Unchecked.defaultof<Texture2D>
    let mutable cruiserTexture    = Unchecked.defaultof<Texture2D>
    let mutable destroyerTexture  = Unchecked.defaultof<Texture2D>
    let mutable subTexture        = Unchecked.defaultof<Texture2D>
    let mutable shotTexture       = Unchecked.defaultof<Texture2D>
    let mutable missTexture       = Unchecked.defaultof<Texture2D>

    let mutable font: SpriteFont  = Unchecked.defaultof<SpriteFont>
    let mutable lastMouseState: MouseState = Mouse.GetState()
    let mutable lastKeyboardState: KeyboardState = Keyboard.GetState()
    
    let mutable shipgame = 
        toStateful {
            playerShips = []; enemyShips = [];
            shotsOnPlayer = Set.ofList []; shotsOnEnemy = Set.ofList [];
            phase = PlayerPlaceShip
        }

    let mutable currentShipToPlace =
        toStateful (shipgame.state |> nextShipToPlace |> Option.get)

    let shipTexture (ship: Ship) =
        match ship.ship with
        | Carrier -> carrierTexture
        | Battleship -> battleshipTexture
        | Cruiser -> cruiserTexture
        | Destroyer -> destroyerTexture
        | Submarine -> subTexture

    let drawShip (ship: Ship) (enemyShip: bool) (spritebatch: SpriteBatch) =
        let drawPos = 
            match enemyShip with
            | false -> vector32 ( fst ship.pos *  boardParam.tileWidth + boardParam.leftMargin,
                                  snd ship.pos *  boardParam.tileWidth + boardParam.topMargin)
            | true -> vector32 ( fst ship.pos *  boardParam.tileWidth + boardParam.leftMargin + boardParam.tileWidth * 11,
                                  snd ship.pos *  boardParam.tileWidth + boardParam.topMargin)
        let texture = shipTexture ship
        if ship.rot then
            spritebatch.Draw(
                texture, drawPos,
                System.Nullable(Rectangle(0,0,texture.Width, texture.Height)),
                Color.White, float32 -(Math.PI / 2.0),
                vector32 (120, 0), 
                vector (0.5f, 0.5f), SpriteEffects.None, 0.0f)
        else
            spritebatch.Draw(
                texture, drawPos,
                System.Nullable(Rectangle(0,0,texture.Width, texture.Height)),
                Color.White, 0.0f,
                Vector2.Zero,
                vector (0.5f, 0.5f), SpriteEffects.None, 0.0f)
    
    let drawPlayerShip ship spritebatch = drawShip ship false spritebatch
    let drawEnemyShip ship spritebatch = drawShip ship true spritebatch

    let drawShotOrMiss pos shottype (spritebatch: SpriteBatch) =
        let isEnemy = match shottype with
                      | HitOnEnemy  | MissOnEnemy  -> true
                      | HitOnPlayer | MissOnPlayer -> false

        let drawPos = pos |> function | x,y when isEnemy -> vector32 ( x * boardParam.tileWidth + boardParam.leftMargin + boardParam.tileWidth * 11,
                                                                       y * boardParam.tileWidth + boardParam.topMargin )
                                      | x,y              -> vector32 ( x * boardParam.tileWidth + boardParam.leftMargin,
                                                                       y * boardParam.tileWidth + boardParam.topMargin )

        let texture = 
            match shottype with
            | HitOnPlayer  | HitOnEnemy  -> shotTexture
            | MissOnPlayer | MissOnEnemy -> missTexture

        spritebatch.Draw(
            texture, drawPos,
            System.Nullable(Rectangle(0,0,shotTexture.Width, shotTexture.Height)),
            Color.White, 0.0f,
            Vector2.Zero,
            vector (0.5f, 0.5f), SpriteEffects.None, 1.0f)
        
    let drawHitOnEnemy  pos = drawShotOrMiss pos HitOnEnemy
    let drawHitOnPlayer pos = drawShotOrMiss pos HitOnPlayer
    let drawMissOnEnemy   pos = drawShotOrMiss pos MissOnEnemy
    let drawMissOnPlayer  pos = drawShotOrMiss pos MissOnPlayer

    override x.Initialize() =
        x.IsMouseVisible <- true
        graphics.PreferredBackBufferHeight <- 720
        graphics.PreferredBackBufferWidth <- 1340
        graphics.ApplyChanges()
        do spriteBatch |> ignore
        do base.Initialize()
        gridTexture <- new Texture2D(graphics.GraphicsDevice, 1, 1)
        gridTexture.SetData([| Color.Black |])

    override x.LoadContent() = 
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        font <- x.Content.Load<SpriteFont>("Font")
        carrierTexture <- x.Content.Load<Texture2D>("carrier")
        battleshipTexture <- x.Content.Load<Texture2D>("battleship")
        cruiserTexture <- x.Content.Load<Texture2D>("cruiser")
        destroyerTexture <- x.Content.Load<Texture2D>("destroyer")
        subTexture <- x.Content.Load<Texture2D>("sub")
        shotTexture <- x.Content.Load<Texture2D>("shot")
        missTexture <- x.Content.Load<Texture2D>("miss")


    override x.Update(gameTime) = 
        let mouseState = Mouse.GetState()
        let keyboardState = Keyboard.GetState()
        let mousePos = (mouseState.X, mouseState.Y)
        let mouseClicked = mouseState.LeftButton = ButtonState.Pressed && lastMouseState.LeftButton = ButtonState.Released
        match shipgame.state.phase with
        | PlayerPlaceShip ->
            let rotation = keyboardState.IsKeyDown(Keys.Space) && lastKeyboardState.IsKeyUp(Keys.Space)
            currentShipToPlace <- applyToState (setShipPosition (mousePos |> coordinateToTile)) currentShipToPlace
            if rotation then
                currentShipToPlace <- applyToState flipShipRotation currentShipToPlace
            if mouseClicked then
                shipgame <- applyToState (addPlayerShip currentShipToPlace.state) shipgame
                let nextShip = nextShipToPlace shipgame.state
                match nextShip with
                | Some nextShip -> currentShipToPlace <- toStateful nextShip
                | None -> shipgame <- applyToState (fun game -> {game with phase = EnemyPlaceShip }) shipgame
        | EnemyPlaceShip -> shipgame <- shipgame.Map(placeEnemyShips)
        | PlayerShoot ->
            mousePos |> coordinateToEnemyTile |> function | Some s when mouseClicked -> shipgame <- shipgame.Map(shootAtEnemy s >> isGameOver)
                                                          | _ -> ()
            shipgame <- shipgame.Map(isGameOver)
        | EnemyShoot ->
            shipgame <- shipgame.Map(shootAtPlayer >> isGameOver)
        | _ -> ()
        lastMouseState <- mouseState
        lastKeyboardState <- keyboardState

    override x.Draw(gameTime) =
        x.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin()
        x.DrawGrid
        shipgame.state.playerShips   |> List.iter (fun s -> drawPlayerShip   s spriteBatch)
        shipgame.state.enemyShips    |> sunkShips shipgame.state.shotsOnEnemy |> List.iter (fun s -> drawEnemyShip    s spriteBatch)
        let hitsOnEnemy  = Set.intersect shipgame.state.shotsOnEnemy (shipgame.state.enemyShips |> List.collect shipTiles |> Set.ofList)
        let missesOnEnemy = Set.difference shipgame.state.shotsOnEnemy hitsOnEnemy
        let hitsOnPlayer = Set.intersect shipgame.state.shotsOnPlayer (shipgame.state.playerShips |> List.collect shipTiles |> Set.ofList)
        let missesOnPlayer = Set.difference shipgame.state.shotsOnPlayer hitsOnPlayer
        hitsOnEnemy   |> Set.iter (fun s -> drawHitOnEnemy s spriteBatch)
        missesOnEnemy |> Set.iter (fun s -> drawMissOnEnemy s spriteBatch)
        hitsOnPlayer  |> Set.iter (fun s -> drawHitOnPlayer s spriteBatch)
        missesOnPlayer|> Set.iter (fun s -> drawMissOnPlayer s spriteBatch)
        shipgame.state.shotsOnPlayer |> Set.iter  (fun s -> drawHitOnPlayer s spriteBatch)
        match shipgame.state.phase with
        | PlayerPlaceShip -> drawPlayerShip currentShipToPlace.state spriteBatch
        | PlayerWin -> spriteBatch.DrawString(font, "You Win!", vector32 (5, 5), Color.Red)
        | EnemyWin -> spriteBatch.DrawString(font, "You Lost!", vector32 (5, 5), Color.Red)
        | _ -> ()
        spriteBatch.End()

    member x.DrawGrid =
        let vert_lines = [for x in 0..10 -> Rectangle(x*boardParam.tileWidth + boardParam.leftMargin, boardParam.topMargin, 1, 10*boardParam.tileWidth)]
        let hor_lines = [for y in 0..10 -> Rectangle(boardParam.leftMargin, y*boardParam.tileWidth + boardParam.topMargin, 10*boardParam.tileWidth, 1)]
        let grid1 = vert_lines @ hor_lines
        let grid2 = grid1 |> List.map (fun line -> Rectangle(line.X + boardParam.tileWidth * 11, line.Y, line.Width, line.Height))
        grid1 @ grid2 |> List.iter (fun line -> spriteBatch.Draw(gridTexture, line, Color.White))
        [1..10] |> List.iter (fun i -> spriteBatch.DrawString(font, gridNumToLetter i, vector32 (boardParam.tileWidth* i + 20, boardParam.topMargin/2), Color.Black))
        [1..10] |> List.iter (fun i -> spriteBatch.DrawString(font, gridNumToLetter i, vector32 (boardParam.tileWidth* i + 680, boardParam.topMargin/2), Color.Black))
        [1..10] |> List.iter (fun i -> spriteBatch.DrawString(font, string i, vector32 (30, boardParam.tileWidth * i + boardParam.topMargin/2), Color.Black))
        [1..10] |> List.iter (fun i -> spriteBatch.DrawString(font, string i, vector32 (690, boardParam.tileWidth*i + boardParam.topMargin/2), Color.Black))
        

[<EntryPoint>]
let main argv = 
    use g = new Game1()
    g.Run()
    0