module TicTacToe exposing
    ( Cell
    , Model
    , Player(..)
    , allCellPlayed
    , hasFilledAColumn
    , hasFilledADiagonal
    , hasFilledALine
    , initialModel
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
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type Msg
    = Played Cell
    | PlayAgain


type Player
    = X
    | O
    | Nobody


type alias Model =
    { board : List Cell --TODO replace with an Array of Cells
    , currentPlayer : Player
    , isGameFinished : Bool
    , winner : Player
    , firstPlayer : Player
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
    , winner = Nobody
    , firstPlayer = X
    }


initCell : Int -> Cell
initCell index =
    { index = index
    , player = Nobody
    , isPlayed = False
    }


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color <| rgb 0.5 0.5 0.5
        ]
    <|
        column
            [ centerX
            , centerY
            ]
            [ column
                [ spacing 2
                , centerX
                ]
              <|
                viewBoard model.board
            , el
                [ padding 20
                , centerX
                , below <| el [ centerX ] <| viewPlayAgainButton model
                ]
              <|
                text <|
                    viewStatusMessage model
            ]


viewStatusMessage : Model -> String
viewStatusMessage model =
    if model.isGameFinished then
        if model.winner == X then
            "X won!"

        else if model.winner == O then
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
    let
        backgroundColor =
            case cell.player of
                X ->
                    rgb 0.2 0.2 0.2

                O ->
                    rgb 0.8 0.8 0.8

                _ ->
                    rgb 0.5 0.5 0.5

        frontColor =
            case cell.player of
                X ->
                    rgb 0.8 0.8 0.8

                O ->
                    rgb 0.2 0.2 0.2

                _ ->
                    rgb 0 0 0
    in
    el
        [ onClick <| Played cell
        , Background.color backgroundColor
        , Font.color frontColor
        , height <| px 50
        , width <| px 50
        , Border.rounded 5
        , Border.color <| rgb 1 1 1
        , Border.width 1
        , pointer
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


viewPlayAgainButton : Model -> Element Msg
viewPlayAgainButton model =
    if model.isGameFinished then
        Input.button
            [ Background.color <| rgb 0 0 0
            , Font.color <| rgb 1 1 1
            , padding 10
            , Border.rounded 5
            , Border.color <| rgb 0 0 0
            , Border.width 1
            ]
            { onPress = Just PlayAgain
            , label = text "Play again"
            }

    else
        none


update : Msg -> Model -> Model
update message model =
    case message of
        Played cell ->
            let
                newModel =
                    playCell model cell

                isXWinner =
                    hasWon X newModel

                isOWinner =
                    hasWon O newModel

                winner =
                    if isXWinner then
                        X

                    else if isOWinner then
                        O

                    else
                        Nobody

                isGameFinished =
                    allCellPlayed newModel
                        || isXWinner
                        || isOWinner
            in
            { newModel
                | isGameFinished = isGameFinished
                , winner = winner
            }

        PlayAgain ->
            case model.winner of
                X ->
                    { initialModel | currentPlayer = O }

                O ->
                    { initialModel | currentPlayer = X }

                Nobody ->
                    let
                        firstPlayer =
                            if model.firstPlayer == X then
                                O

                            else
                                X
                    in
                    { initialModel | currentPlayer = firstPlayer, firstPlayer = firstPlayer }


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
    if allCellPlayed model then
        Nobody

    else if model.currentPlayer == X then
        O

    else
        X


allCellPlayed : Model -> Bool
allCellPlayed model =
    List.all .isPlayed model.board


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
3 4 5 
6 7 8
Diagonal1 = 0/4/8 ==> + boardWidth + 1 to get next cell Id
Diagonal2 = 2/4/6 ==> +  boardWidth - 1 to get next cell Id, as long as you exclude the last cell of the grid (bottom right cell)
It works on a bigger grid
 0  1  2  3
 4  5  6  7
 8  9 10 11
12 13 14 15
 
--}


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
