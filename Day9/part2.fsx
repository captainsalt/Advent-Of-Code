open System.IO

type FloorType = LowPoint | Basin | High | Undefined

type FloorSpace = {
    row: int
    col: int 
    value: int
    floorType: FloorType
}

type Floor = FloorSpace seq seq

let (>>=) m f = Option.bind f m

let listTofloorSpace = 
    Seq.mapi (fun rowIndex row -> 
        row 
        |> Seq.mapi (fun colIndex value  ->  
            { row = rowIndex; col = colIndex; value = value; floorType = Undefined} ))

let getFloorSpace (floor: Floor) floorSpace =
    Seq.tryItem floorSpace.row floor
    >>= Seq.tryItem floorSpace.col

let getAdjacent (floor: Floor) floorSpace =
    let floorItem = getFloorSpace floor

    [ floorItem <|  { floorSpace with col = floorSpace.col + 1 } // Look right
      floorItem <|  { floorSpace with col = floorSpace.col - 1 } // Look left
      floorItem <|  { floorSpace with row = floorSpace.row + 1 }  // Look above
      floorItem <|  { floorSpace with row = floorSpace.row - 1 } ] // Look below
    |> Seq.choose id

let classifyPoint (floor: Floor) floorSpace =
    match getFloorSpace floor floorSpace with
    | None -> None
    | Some floorSpace ->
        let isLowest =
            getAdjacent floor floorSpace
            |> Seq.forall (fun fs -> fs.value > floorSpace.value)

        if isLowest then
            Some { floorSpace with floorType = LowPoint}
        else
            Some floorSpace

let isLowPoint =
    function
    | { floorType = LowPoint } -> true
    | _ -> false

let parseInput path =
    File.ReadAllLines(path)
    |> Seq.map (fun str -> Seq.map (string >> int) (str.ToCharArray()))

let classifyPoints (floor: Floor) =
    let classifyPoint = classifyPoint floor

    floor
    |> Seq.map (fun row ->
        row 
        |> Seq.map (fun fs -> classifyPoint fs))
    |> Seq.map (Seq.choose id)

let threatLevel (floor: Floor) =
    floor
    |> classifyPoints
    |> Seq.collect (Seq.filter isLowPoint)
    |> Seq.map (fun fs -> fs.value + 1)
    |> Seq.reduce (+)

listTofloorSpace >> threatLevel
<| parseInput "input.txt"
|> printfn "%i"
