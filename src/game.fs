module Game
open System
open System.Collections.Generic

type Canvas(rows:int, cols:int) =
    do 
        Console.SetWindowSize(1, 1)
        Console.SetWindowSize(rows, cols)
    let mutable grid = Array2D.init rows cols (fun i j -> ('.', System.ConsoleColor.White, System.ConsoleColor.Black))
    
    static member bgToAnsi(color:System.ConsoleColor) : int =
            match color with
            | System.ConsoleColor.Black -> 40
            | System.ConsoleColor.Blue -> 104
            | System.ConsoleColor.Gray -> 100
            | System.ConsoleColor.Green -> 102
            | System.ConsoleColor.Red -> 101
            | System.ConsoleColor.White -> 107
            | System.ConsoleColor.Yellow -> 103
            | _ -> 41
    static member fgToAnsi(color:System.ConsoleColor) : int =
            match color with
            | System.ConsoleColor.Black -> 30
            | System.ConsoleColor.Blue -> 94
            | System.ConsoleColor.Gray -> 90
            | System.ConsoleColor.Green -> 92
            | System.ConsoleColor.Red -> 91
            | System.ConsoleColor.White -> 97
            | System.ConsoleColor.Yellow -> 93
            | _ -> 31
    
    member this.Clear() = grid <- Array2D.init rows cols (fun i j -> ('.', System.ConsoleColor.White, System.ConsoleColor.Black))
    member this.Set(x:int, y:int, c:char, fg:System.ConsoleColor, bg:System.ConsoleColor) =
        Array2D.set grid y x (c, fg, bg)
    member this.Show() =
        System.Console.Clear()
        System.Console.SetCursorPosition(0,0)
        let stringBuilder = Text.StringBuilder()
        Array2D.iteri (fun y x item -> 
            let c, fg, bg = item
            stringBuilder.Append (sprintf "\x1B[%d;%dm%c" (Canvas.fgToAnsi(fg)) (Canvas.bgToAnsi(bg)) c) |> ignore
            if (x = cols-1) then stringBuilder.AppendLine() |> ignore
        ) grid
        System.Console.Write(stringBuilder)
        System.Console.ResetColor()
    member this.GetHeight() = rows
    member this.GetWidth() = cols
    member this.Get(x:int, y:int) = Array2D.get grid y x

[<AbstractClass;>]
type Entity(_x:int, _y:int) =
    let mutable position = (_x, _y)
    abstract member Character: unit -> char
    abstract member Color: unit -> System.ConsoleColor
    member this.GetPosition() = position
    member this.Render(canvas:Canvas) =
        canvas.Set(fst position, snd position, this.Character(), this.Color(), System.ConsoleColor.Black)
    
    member this.SetPosition(_position:int*int) =
        position <- _position
    
    override this.Equals(other) =
        if (other :? Entity) then false
        else
        let otherEntity : Entity = other |> unbox
        LanguagePrimitives.PhysicalEquality this otherEntity
    
    override this.GetHashCode() = 
        System.Runtime.InteropServices.GCHandle.Alloc(this, System.Runtime.InteropServices.GCHandleType.WeakTrackResurrection).GetHashCode()
    
    interface IComparable with
        member x.CompareTo(obj) = (x.GetHashCode()).CompareTo(obj.GetHashCode())


[<AbstractClass>]
type LivingEntity(_x:int, _y:int, _hitpoints:int) =
    inherit Entity(_x, _y)
    let mutable hitpoints = _hitpoints
    member this.UpdateHitPoints(change:int) =
        hitpoints <- hitpoints + change

    member this.GetHitPoints() = hitpoints
    
    member this.IsDead() = hitpoints <= 0

type Player(_x:int, _y:int) =
    inherit LivingEntity(_x, _y, 10)
    override this.Character() = '@'
    override this.Color() = System.ConsoleColor.Green

[<AbstractClass>]
type Item(_x:int, _y:int) =
    inherit Entity(_x, _y)
    abstract member InteractWith: Player -> bool
    abstract member FullyOccupy: unit -> bool

type Wall(_x:int, _y:int) =
    inherit Item(_x, _y)
    override this.InteractWith(player:Player) = true
    override this.FullyOccupy() = true
    override this.Character() = '#'
    override this.Color() = System.ConsoleColor.White

type Fire(_x:int, _y:int) =
    inherit Item(_x, _y)
    let mutable charges = 5
    override this.InteractWith(player:Player) = 
        player.UpdateHitPoints(-1)
        charges <- charges-1
        charges > 0
    override this.FullyOccupy() = false
    override this.Character() = '☲'
    override this.Color() = System.ConsoleColor.Red

type Water(_x:int, _y:int) =
    inherit Item(_x, _y)
    let mutable charges = 1
    override this.InteractWith(player:Player) = 
        player.UpdateHitPoints(2)
        charges <- charges-1
        charges > 0
    override this.FullyOccupy() = false
    override this.Character() = '☵'
    override this.Color() = System.ConsoleColor.Blue


type FleshEatingPlant(_x:int, _y:int) =
    inherit Item(_x, _y)
    override this.InteractWith(player:Player) = 
        player.UpdateHitPoints(-5)
        true
    override this.FullyOccupy() = true
    override this.Character() = 'X'
    override this.Color() = System.ConsoleColor.Red


type World(_difficulity:int) =
    let mutable difficulity = _difficulity
    let mutable items : Map<int*int, Item list> = Map.empty
    let mutable entities : Map<Entity, bool> = Map.empty
    let canvas = Canvas(Console.BufferHeight-2, Console.BufferWidth)

    member this.GetDifficulity() = difficulity
    member this.SetDifficulity(_difficulity:int) = difficulity <- _difficulity

    member this.Clear() =
        items <- Map.empty
        entities <- Map.empty

    member this.GetItem(position:int*int) : Item list =
        match items.TryFind(position) with
            | None -> []
            | Some items -> items

    member this.AddEntity(entity:Entity) =
        entities <- entities.Add(entity, true)

    member this.RemoveEntity(entity:Entity) =
        entities <- entities.Remove(entity)

    member this.RemoveItem(item:Item) =
        let position = item.GetPosition()
        items <- items.Add(position, this.GetItem(position) |> List.filter (fun other -> not (LanguagePrimitives.PhysicalEquality item other)))
    
    member this.RemoveAllItems(position:int*int) =
        let x, y = position
        items <- items.Remove(position)

    member this.AddItem(item:Item) =
        let position = item.GetPosition()
        items <- items.Add(position, (this.GetItem(position) @ [item]))

    member this.Play() =
        canvas.Clear()
        items |> Map.iter (fun key value ->
            value |> List.map (fun item -> (item.Render(canvas))) |> ignore
            )
        entities |> Map.iter (fun key value ->
            key.Render(canvas) |> ignore
            )
        canvas.Show()
    member this.GetCanvas() = canvas

    member this.IsOutOfBounds(position:int*int) =
        let x, y = position
        canvas.GetWidth() < x || canvas.GetHeight() < y+1 || x < 0 || y < 0

    member this.IsPositionOcupied(position) =
        this.GetItem(position) |> List.exists (fun item -> item.FullyOccupy())
    member this.IsWall(position:int*int): bool =
        this.GetItem(position) |> List.exists (fun item -> item :? Wall)

    member this.GetLocationNeighbors(position:int*int): (int*int) list =
        let x, y = position
        [(x+1,y);(x-1,y);(x,y+1);(x,y-1)] 
    member this.GetEmptySpot() =
        let rec emptySpot(): int*int =
            let spawn = System.Random().Next(1, canvas.GetWidth()-1), System.Random().Next(1, canvas.GetHeight()-1)
            if(this.IsWall(spawn)) then emptySpot()
            else
            spawn
        emptySpot()

    member this.GenerateRandomWorld() =
        let canvas = this.GetCanvas()
        for y = 0 to canvas.GetHeight()-1 do
            for x = 0 to canvas.GetWidth()-1 do
                this.AddItem(Wall(x,y))
        let startPosition = System.Random().Next(1, canvas.GetWidth()-1), System.Random().Next(1, canvas.GetHeight()-1)
        let tilesToTurn = System.Random().Next(int((((canvas.GetWidth()-1) * (canvas.GetHeight()-1)))/10), ((canvas.GetWidth()-1) * (canvas.GetHeight()-1)))
        let rec drunkardsWalk(world:World, position:int*int, tilesLeft:int): World =
            if(tilesLeft <= 0) then world
            else
            let neighbors = world.GetLocationNeighbors(position)
            let nextPosition = neighbors.[System.Random().Next(0, neighbors.Length)]
            if (world.IsOutOfBounds(nextPosition)) then
                drunkardsWalk(world, position, tilesLeft)
            else
            if (not (world.IsWall(nextPosition))) then 
                drunkardsWalk(world, nextPosition, tilesLeft)
            else
            world.RemoveAllItems(nextPosition)
            drunkardsWalk(world, nextPosition, (tilesLeft-1))
        drunkardsWalk(this, startPosition, tilesToTurn)
    
    member this.Populate() = 
        let rec spawn(left:int) =
            if (left<=0) then ()
            else
            let rng = System.Random().Next(1,5)
            if(rng=0) then 
                this.AddItem(FleshEatingPlant(this.GetEmptySpot()))
                spawn(left-1)
            else 
                this.AddItem(Fire(this.GetEmptySpot()))
                spawn(left-1)
        this.AddItem(Water(this.GetEmptySpot()))
        spawn(difficulity)


        

type Exit(_x:int, _y:int, world:World) =
    inherit Item(_x, _y)

    override this.FullyOccupy() = false

    override this.InteractWith(player:Player) = 
        world.Clear() |> ignore
        world.GenerateRandomWorld() |> ignore
        player.SetPosition(world.GetEmptySpot())
        this.SetPosition(world.GetEmptySpot())
        world.AddEntity(player)
        world.AddItem(this)
        world.SetDifficulity(world.GetDifficulity() + System.Random().Next(2,12))
        world.Populate()
        true
    override this.Character() = 'Æ'
    override this.Color() = System.ConsoleColor.Yellow

let world = World(5).GenerateRandomWorld()
let player = Player(world.GetEmptySpot())
let exitSpot = world.GetEmptySpot()
let exit = Exit(fst exitSpot, snd exitSpot, world)
world.AddEntity(player)
world.AddItem(exit)
world.Populate()

world.AddItem(FleshEatingPlant(world.GetEmptySpot()))

let mutable gameloop = true
while not (player.IsDead()) do
    world.Play()
    printfn "Player HP: %i" (player.GetHitPoints()) 
    let x, y = player.GetPosition()
    let mutable newPosition = x,y
    let key_info = Console.ReadKey()
    let key_char = key_info.KeyChar
    if(key_char = 'w') then newPosition<-(x, y-1)
    if(key_char = 's') then newPosition<-(x, y+1)
    if(key_char = 'a') then newPosition<-(x-1, y)
    if(key_char = 'd') then newPosition<-(x+1, y)
    if((not (world.IsOutOfBounds(newPosition)) && not (world.IsPositionOcupied(newPosition)))) then player.SetPosition(newPosition)
    let items = world.GetItem(newPosition)
    items |> List.iter (fun item -> 
        if (not (item.InteractWith(player))) then world.RemoveItem(item) 
        )
Console.Clear()
printfn "GAME OVER"