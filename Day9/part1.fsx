module part1

open System.IO

type FloorSpace = LowPoint of int | Point of int
type Floor = FloorSpace List List


let listTofloorSpace = List.map (List.map Point)


let getValue = function 
| (LowPoint n) -> n
| (Point n) -> n


let getFloorSpace (floor: Floor) rowIndex elemIndex  = 
    let (>>=) m f = Option.bind f m

    if rowIndex < 0 || elemIndex < 0 then None 
    else List.tryItem rowIndex floor >>= List.tryItem elemIndex


let getAdjacent (floor: Floor) floorRow index =
    let floorItem = getFloorSpace floor

    [ floorItem floorRow (index + 1) // Look right
    ; floorItem floorRow (index - 1) // Look left
    ; floorItem (floorRow + 1) index // Look above
    ; floorItem (floorRow - 1) index ] // Look below
    |> List.choose id


let classifyPoint (floor: Floor) rowIndex elemIndex = 
    match getFloorSpace floor rowIndex elemIndex with 
    | None -> None 
    | Some floorSpace ->
        let value = getValue floorSpace
        let isLowest =     
            getAdjacent floor rowIndex elemIndex 
            |> List.forall(fun fs -> getValue fs > value)
        
        if isLowest then Some (LowPoint value)
        else Some (Point value)


let isLowPoint = function 
| LowPoint _ -> true
| _ -> false


let parseInput path = 
    File.ReadAllLines(path)
    |> Array.toList
    |> List.map 
        (fun str -> 
            Array.map (string >> int) (str.ToCharArray()) 
            |> Array.toList)


let classifyPoints (floor: Floor)  = 
    let height = floor.Length
    let length = floor |> List.head |> List.length

    [ for y in [0..height - 1] ->
        [ for x in [0..length - 1] ->
            Option.get (classifyPoint floor y x) ] ]


let threatLevel (floor: Floor) =
    floor 
    |> List.collect (List.filter (isLowPoint))
    |> List.map (getValue >> (+) 1)
    |> List.reduce (+)


listTofloorSpace >> classifyPoints >> threatLevel <| parseInput "input.txt" |> printfn "%i"

