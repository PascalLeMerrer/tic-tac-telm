module LiveCoding exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Played Cell


type Player
    = X
    | O
    | Nobody


type alias Row =
    List Cell


type alias Model =
    { grid : List Cell
    , currentPlayer : Player
    }


type alias Cell =
    { player : Player
    , isPlayed : Bool
    , index : Int
    }


gridSize =
    3


initialModel : Model
initialModel =
    let
        indexes =
            List.range 0 <| gridSize * gridSize - 1

        cells =
            List.map initCell indexes
    in
    { grid = cells
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
    div []
        [ div [] <| viewGrid model.grid
        , p [] [ text <| "Next player: " ++ viewPlayer model.currentPlayer ]
        ]


viewGrid : List Cell -> List (Html Msg)
viewGrid grid =
    if List.isEmpty grid then
        [ br [] [] ]

    else
        (viewRow <| List.take gridSize grid)
            :: (viewGrid <| List.drop gridSize grid)


viewRow : List Cell -> Html Msg
viewRow row =
    div [ class "row" ] <|
        List.map viewCell row


viewCell : Cell -> Html Msg
viewCell cell =
    div [ class "cell", onClick <| Played cell ] [ text <| viewPlayer cell.player ]


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
                playedCell =
                    { cell | isPlayed = True, player = model.currentPlayer }

                splitIndex = cell.index + 1   
                newCellList = List.take cell.index model.grid 
                            ++ [playedCell] 
                            ++ List.drop splitIndex model.grid

                nextPlayer =
                    if model.currentPlayer == X then
                        O
                    else
                        X
            in
            { model | currentPlayer = nextPlayer, grid = newCellList }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
