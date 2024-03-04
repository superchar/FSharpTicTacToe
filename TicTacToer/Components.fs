module TicTacToer.Components

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open TicTacToer.GameHelpers
open TicTacToer.Minimax

type private RowType =
    | Top
    | Middle
    | Bottom

let private defaultBoard =
    [ [ emptySign; emptySign; emptySign ]
      [ emptySign; emptySign; emptySign ]
      [ emptySign; emptySign; emptySign ] ]

let private startNewGame (values: IWritable<string> list list) =
    values |> List.iter (fun row -> row |> List.iter (fun col -> col.Set emptySign))

let private handleBoardClick (values: IWritable<string> list list) (lastGameResult: IWritable<string>) () =
    let currentBoard = values |> List.map (fun row -> row |> List.map (_.Current))
    let isTie = checkTie currentBoard
    let isWinner = checkWinner currentBoard

    if isTie || isWinner then
        lastGameResult.Set(if isTie then "It's a tie!" else "You won!")
        startNewGame values
    else
        let row, col = currentBoard |> findBestMove
        values[row].[col].Set aiSign
        let updatedBoard = currentBoard |> updateBoard row col aiSign
        let isAiTie = checkTie updatedBoard
        let isAiWinner = checkWinner updatedBoard

        if isAiTie || isAiWinner then
            lastGameResult.Set(if isAiTie then "It's a tie!" else "AI won!")
            startNewGame values

let boardCell
    ((left, top, right, bottom): float * float * float * float)
    (value: IWritable<string>)
    (onClick: unit -> unit)
    =
    Component.create (
        "cell",
        (fun ctx ->
            let value = ctx.usePassed value

            let onPointerPressed =
                fun _ ->
                    if value.Current = emptySign then
                        value.Set playerSign
                        onClick ()

            Border.create
                [ Border.borderBrush "yellow"
                  Border.borderThickness (left, top, right, bottom)
                  Border.width 200
                  Border.height 200
                  Border.child (
                      Button.create
                          [ TextBlock.onPointerPressed onPointerPressed
                            Button.content value.Current
                            Button.foreground "blue"
                            Button.fontSize 100.0
                            Button.verticalAlignment VerticalAlignment.Stretch
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                            Button.background "white" ]
                  ) ])
    )

let private boardRow (rowType: RowType) (onClick: unit -> unit) (values: IWritable<string> list) =
    let rowPositions =
        match rowType with
        | Top -> [ (0, 0, 4, 4); (4, 0, 4, 4); (4, 0, 0, 4) ]
        | Middle -> [ (0, 4, 4, 4); (4, 4, 4, 4); (4, 4, 0, 4) ]
        | Bottom -> [ (0, 4, 4, 0); (4, 4, 4, 0); (4, 4, 0, 0) ]

    let children: IView list =
        (rowPositions
         |> List.zip values
         |> List.map (fun (state, (left, top, right, bottom)) -> boardCell (left, top, right, bottom) state onClick))

    StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ]

let lastGameStatus (lastGameResult: IWritable<string>) =
    TextBlock.create
        [ TextBlock.text lastGameResult.Current
          TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.verticalAlignment VerticalAlignment.Center
          TextBlock.fontSize 50 ]

let gameBoard () =
    Component(fun ctx ->
        let states =
            defaultBoard
            |> List.map (fun row -> row |> List.map (fun col -> ctx.useState (col, false)))

        let lastGameResult = ctx.useState ""
        startNewGame states

        let onClick = handleBoardClick states lastGameResult

        StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.horizontalAlignment HorizontalAlignment.Center
              StackPanel.verticalAlignment VerticalAlignment.Center
              StackPanel.children
                  [ boardRow Top onClick states[0]
                    boardRow Middle onClick states[1]
                    boardRow Bottom onClick states[2]
                    lastGameStatus lastGameResult ] ])