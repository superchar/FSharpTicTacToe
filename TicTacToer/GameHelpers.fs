module TicTacToer.GameHelpers

let emptySign = " "

let playerSign = "X"

let aiSign = "O"

let private areCellsEqual (cell1: string) (cell2: string) = cell1 <> emptySign && cell1 = cell2

let checkWinner board =

    let rowWinner =
        board
        |> List.exists (fun row -> row |> List.forall (fun cell -> areCellsEqual cell row[0]))

    let colWinner =
        [ 0..2 ]
        |> List.exists (fun col -> board |> List.forall (fun row -> areCellsEqual row[col] board[0].[col]))

    let diagWinner =
        [ 0..2 ] |> List.forall (fun i -> areCellsEqual board[i].[i] board[0].[0])
        || [ 0..2 ] |> List.forall (fun i -> areCellsEqual board[i].[2 - i] board[0].[2])

    rowWinner || colWinner || diagWinner

let updateBoard row col value board =
    board
    |> List.mapi (fun i r ->
        if i = row then
            r |> List.mapi (fun j c -> if j = col then value else c)
        else
            r)

let getAvailablePositions board =
    board
    |> List.mapi (fun i r -> r |> List.mapi (fun j c -> if c = emptySign then (i, j) else (-1, -1)))
    |> List.collect id
    |> List.filter (fun (i, j) -> i <> -1 && j <> -1)

let checkTie = getAvailablePositions >> List.isEmpty