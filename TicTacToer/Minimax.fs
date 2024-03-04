module TicTacToer.Minimax

open TicTacToer.GameHelpers

let private depthPenalty = 10

let rec minimax board depth isMax : int =
    let winner = checkWinner board

    if winner then
        if isMax then -depthPenalty + depth else depthPenalty - depth
    else
        let positions = getAvailablePositions board

        if positions |> List.isEmpty then
            0
        else if isMax then
            positions
            |> List.map (fun (i, j) ->
                let newBoard = updateBoard i j aiSign board
                minimax newBoard (depth + 1) false)
            |> List.max
        else
            positions
            |> List.map (fun (i, j) ->
                let newBoard = updateBoard i j playerSign board
                minimax newBoard (depth + 1) true)
            |> List.min

let findBestMove board =
    getAvailablePositions board
    |> List.map (fun (i, j) ->
        let newBoard = updateBoard i j aiSign board
        let score = minimax newBoard 0 false
        (score, (i, j)))
    |> List.maxBy fst
    |> snd