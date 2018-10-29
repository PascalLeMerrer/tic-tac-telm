module TicTacToe exposing
    ( Cell
    , Model
    , Player(..)
    , hasFilledALine
    , initialModel
    , isGameFinished
    , main
    , nextPlayer
    , playCell
    )

import Browser
import Debug
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
    { board : List Cell
    , currentPlayer : Player
    }


type alias Cell =
    { player : Player
    , isPlayed : Bool
    , index : Int
    }


boardWidth : Int
boardWidth =
    3


initialModel : Model
initialModel =
    let
        indexes =
            List.range 0 <| boardWidth * boardWidth - 1

        cells =
            List.map initCell indexes
    in
    { board = cells
    , currentPlayer = X
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
                    viewHelpMessage model.currentPlayer
            ]


viewHelpMessage : Player -> String
viewHelpMessage player =
    if player == Nobody then
        "Game over."

    else
        viewPlayer player ++ " should play."


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
        el [ centerX, centerY ] 
            <| text 
            <| viewPlayer cell.player


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
            playCell model cell


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


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
