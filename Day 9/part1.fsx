open System.IO

type FloorSpace =
    | LowPoint of int
    | Point of int

type Floor = FloorSpace seq seq

let listTofloorSpace = Seq.map (Seq.map Point)

let getValue =
    function
    | (LowPoint n) -> n
    | (Point n) -> n

let getFloorSpace (floor: Floor) rowIndex elemIndex =
    let (>>=) m f = Option.bind f m

    if rowIndex < 0 || elemIndex < 0 then
        None
    else
        Seq.tryItem rowIndex floor
        >>= Seq.tryItem elemIndex

let getAdjacent (floor: Floor) floorRow index =
    let floorItem = getFloorSpace floor

    [ floorItem floorRow (index + 1) // Look right
      floorItem floorRow (index - 1) // Look left
      floorItem (floorRow + 1) index // Look above
      floorItem (floorRow - 1) index ] // Look below
    |> Seq.choose id

let classifyPoint (floor: Floor) rowIndex elemIndex =
    match getFloorSpace floor rowIndex elemIndex with
    | None -> None
    | Some floorSpace ->
        let value = getValue floorSpace

        let isLowest =
            getAdjacent floor rowIndex elemIndex
            |> Seq.forall (fun fs -> getValue fs > value)

        if isLowest then
            LowPoint value |> Some
        else
            Point value |> Some

let isLowPoint =
    function
    | LowPoint _ -> true
    | _ -> false

let parseInput path =
    File.ReadAllLines(path)
    |> Seq.map (fun str -> Seq.map (string >> int) (str.ToCharArray()))

let classifyPoints (floor: Floor) =
    let classifyPoint = classifyPoint floor

    floor
    |> Seq.mapi (fun rowIndex row ->
        row 
        |> Seq.mapi (fun colIndex _ -> classifyPoint rowIndex colIndex))
    |> Seq.map (Seq.choose id)

let threatLevel (floor: Floor) =
    floor
    |> classifyPoints
    |> Seq.collect (Seq.filter (isLowPoint))
    |> Seq.map (getValue >> (+) 1)
    |> Seq.reduce (+)

listTofloorSpace >> threatLevel
<| parseInput "input.txt"
|> printfn "%i"
