module TicTacToe exposing
    ( Cell
    , Model
    , Player(..)
    , hasFilledAColumn
    , hasFilledADiagonal
    , hasFilledALine
    , initialModel
    , isGameFinished
    , main
    , nextPlayer
    , playCell
    )

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Html exposing (Html)


type Msg
    = Played Cell


type Player
    = X
    | O
    | Nobody


type alias Row =
    List Cell


type alias Model =
    { board : List Cell --TODO replace with an Array of Cells
    , currentPlayer : Player
    , isGameFinished : Bool
    }


type alias Cell =
    { player : Player
    , isPlayed : Bool
    , index : Int
    }


boardWidth : Int
boardWidth =
    3


indexes =
    List.range 0 <| boardWidth * boardWidth - 1


initialModel : Model
initialModel =
    let
        cells =
            List.map initCell indexes
    in
    { board = cells
    , currentPlayer = X
    , isGameFinished = False
    }


initCell : Int -> Cell
initCell index =
    { index = index
    , player = Nobody
    , isPlayed = False
    }


view : Model -> Html Msg
view model =
    Element.layout [] <|
        column
            [ centerX
            , centerY
            , spacing 10
            ]
            [ column [ spacing 2 ] <|
                viewBoard model.board
            , el [ centerX ] <|
                text <|
                    viewStatusMessage model
            ]


viewStatusMessage : Model -> String
viewStatusMessage model =
    if model.isGameFinished then
        
            if hasWon X model then
                "X won!"

            else if hasWon O model then
                "O won!"

            else
                "Game over."

        else
            viewPlayer model.currentPlayer ++ " should play."


viewBoard : List Cell -> List (Element Msg)
viewBoard board =
    if List.isEmpty board then
        [ el [] none ]

    else
        (viewRow <| List.take boardWidth board)
            :: (viewBoard <| List.drop boardWidth board)


viewRow : List Cell -> Element Msg
viewRow cellRow =
    row [ spacing 2 ] <|
        List.map viewCell cellRow


viewCell : Cell -> Element Msg
viewCell cell =
    el
        [ onClick <| Played cell
        , Background.color (rgb 0.5 0.5 0.5)
        , height <| px 50
        , width <| px 50
        , Border.rounded 5
        ]
    <|
        el [ centerX, centerY ] <|
            text <|
                viewPlayer cell.player


viewPlayer : Player -> String
viewPlayer player =
    case player of
        X ->
            "X"

        O ->
            "O "

        Nobody ->
            "•"


update : Msg -> Model -> Model
update message model =
    case message of
        Played cell ->
            let
                newModel =
                    playCell model cell
                
            in
            { newModel | isGameFinished = isGameFinished newModel }


playCell : Model -> Cell -> Model
playCell model cell =
    if cell.isPlayed then
        model

    else
        let
            playedCell =
                { cell | isPlayed = True, player = model.currentPlayer }

            splitIndex =
                cell.index + 1

            newCellList =
                List.take cell.index model.board
                    ++ [ playedCell ]
                    ++ List.drop splitIndex model.board

            newModel =
                { model | board = newCellList }
        in
        { newModel | currentPlayer = nextPlayer newModel }


nextPlayer : Model -> Player
nextPlayer model =
    if isGameFinished model then
        Nobody

    else if model.currentPlayer == X then
        O

    else
        X


isGameFinished : Model -> Bool
isGameFinished model =
    List.all .isPlayed model.board
        || hasWon X model
        || hasWon O model


hasWon : Player -> Model -> Bool
hasWon player model =
    hasFilledALine player model.board
        || hasFilledAColumn player model.board
        || hasFilledADiagonal player model.board


hasFilledALine : Player -> List Cell -> Bool
hasFilledALine player board =
    if List.isEmpty board then
        False

    else
        let
            row =
                List.take boardWidth board
        in
        List.all (\cell -> cell.player == player) row
            || (hasFilledALine player <| List.drop boardWidth board)


columnIndexes =
    List.range 0 (boardWidth - 1)


hasFilledAColumn : Player -> List Cell -> Bool
hasFilledAColumn player board =
    List.map (\index -> isColumnBelongingTo player board index) columnIndexes
        |> List.any (\result -> result)


isColumnBelongingTo : Player -> List Cell -> Int -> Bool
isColumnBelongingTo player board columnIndex =
    List.filter (\cell -> isCellInColumn cell columnIndex) board
        |> List.all (\cell -> cell.player == player)


isCellInColumn : Cell -> Int -> Bool
isCellInColumn cell columnIndex =
    columnIndex == cell.index || columnIndex == modBy boardWidth cell.index


hasFilledADiagonal : Player -> List Cell -> Bool
hasFilledADiagonal player board =
    let
        hasFilledFirstDiagonal =
            topLeftDownRightDiagonalCells board 0
                |> List.all (\cell -> cell.player == player)

        topRightCellIndex =
            boardWidth - 1

        hasFilledSecondDiagonal =
            topRightDownLeftDiagonalCells (List.drop topRightCellIndex board) topRightCellIndex
                |> List.all (\cell -> cell.player == player)
    in
    hasFilledFirstDiagonal || hasFilledSecondDiagonal


topLeftDownRightDiagonalCells : List Cell -> Int -> List Cell
topLeftDownRightDiagonalCells board cellToKeepIndex =
    let
        head =
            List.head board
    in
    case head of
        Just cell ->
            let
                nextCellsInDiagonal =
                    topLeftDownRightDiagonalCells (List.drop (boardWidth + 1) board) (cellToKeepIndex + boardWidth + 1)
            in
            if cell.index == cellToKeepIndex then
                cell :: nextCellsInDiagonal

            else
                nextCellsInDiagonal

        Nothing ->
            []


topRightDownLeftDiagonalCells : List Cell -> Int -> List Cell
topRightDownLeftDiagonalCells board cellToKeepIndex =
    let
        head =
            List.head board
    in
    case head of
        Just cell ->
            let
                remainingCells =
                    --List.drop boardWidth board
                    List.drop 1 board

                nextDiagonalCellIndex =
                    if isNotLastCellOfBoard && cell.index == cellToKeepIndex then
                        cellToKeepIndex + boardWidth - 1

                    else
                        cellToKeepIndex

                nextCellsInDiagonal =
                    topRightDownLeftDiagonalCells remainingCells nextDiagonalCellIndex

                isNotLastCellOfBoard =
                    cellToKeepIndex /= List.length indexes - 1
            in
            if isNotLastCellOfBoard && cell.index == cellToKeepIndex then
                cell :: nextCellsInDiagonal

            else
                nextCellsInDiagonal

        Nothing ->
            []



{--
0 1 2
3 4 5 + boardWidth + 1
6 7 8

 0  1  2  3
 4  5  6  7
 8  9 10 11
12 13 14 15
 
+  boardWidth - 1

--}


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
