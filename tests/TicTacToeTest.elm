module TicTacToeTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import TicTacToe exposing (..)


suite : Test
suite =
    describe "The tic tac toe game"
        [ describe "allCellPlayed"
            [ test "should detect when the game is not finished" <|
                \_ ->
                    Expect.equal False <| allCellPlayed unfinishedGameModel
            , test "should detect end of the game when all cells are played" <|
                \_ ->
                    Expect.equal True <| allCellPlayed finishedGameModel
            ]
        , describe "hasFilledALine"
            [ test "should return true for X when all cells in a row were played by X" <|
                \_ ->
                    Expect.equal True <| hasFilledALine X boardWithLineOfX
            , test "should return false for X when all cells in a row were played by  O" <|
                \_ ->
                    Expect.equal False <| hasFilledALine X boardWithLineOfO
            , test "should return true for O when all cells in a row were played by O" <|
                \_ ->
                    Expect.equal True <| hasFilledALine O boardWithLineOfO
            , test "should return false for O when all cells in a row were played by X" <|
                \_ ->
                    Expect.equal False <| hasFilledALine O boardWithLineOfX
            , test "should return false for O when no player filled a row" <|
                \_ ->
                    Expect.equal False <| hasFilledALine O unfinishedGameModel.board
            , test "should return false for X when no player filled a row" <|
                \_ ->
                    Expect.equal False <| hasFilledALine X unfinishedGameModel.board
            ]
        , describe "hasFilledAColumn"
            [ test "should return true for X when all cells in a column were played by X" <|
                \_ ->
                    Expect.equal True <| hasFilledAColumn X boardWithColumnOfX
            , test "should return false for X when all cells in a column were played by  O" <|
                \_ ->
                    Expect.equal False <| hasFilledAColumn X boardWithColumnOfO
            , test "should return true for O when all cells in a column were played by O" <|
                \_ ->
                    Expect.equal True <| hasFilledAColumn O boardWithColumnOfO
            , test "should return false for O when all cells in a column were played by X" <|
                \_ ->
                    Expect.equal False <| hasFilledAColumn O boardWithColumnOfX
            , test "should return false for O when no player filled a column" <|
                \_ ->
                    Expect.equal False <| hasFilledAColumn O unfinishedGameModel.board
            , test "should return false for X when no player filled a column" <|
                \_ ->
                    Expect.equal False <| hasFilledAColumn X unfinishedGameModel.board
            ]
        , describe "hasFilledADiagonal"
            [ test "should return true for X when all cells in a diagonal were played by X" <|
                \_ ->
                    Expect.equal True <| hasFilledADiagonal X boardWithDiagonalOfX
            , test "should return false for X when all cells in a diagonal were played by  O" <|
                \_ ->
                    Expect.equal False <| hasFilledADiagonal X boardWithDiagonalOfO
            , test "should return true for O when all cells in a diagonal were played by O" <|
                \_ ->
                    Expect.equal True <| hasFilledADiagonal O boardWithDiagonalOfO
            , test "should return false for O when all cells in a diagonal were played by X" <|
                \_ ->
                    Expect.equal False <| hasFilledADiagonal O boardWithDiagonalOfX
            , test "should return false for O when O did not fill a diagonal" <|
                \_ ->
                    Expect.equal False <| hasFilledADiagonal O unfinishedGameModel.board
            , test "should return false for X when X did not fill a diagonal" <|
                \_ ->
                    Expect.equal False <| hasFilledADiagonal X unfinishedGameModel.board
            ]
        , describe "nextPlayer"
            [ test "should say the next player is O when X is the current one" <|
                \_ ->
                    Expect.equal O <| nextPlayer unfinishedGameModel
            , test "should say the next player is X when O is the current one" <|
                \_ ->
                    let
                        model =
                            { unfinishedGameModel | currentPlayer = O }
                    in
                    Expect.equal X <| nextPlayer model
            , test "should say the next player is Nobody when the game is finished" <|
                \_ ->
                    Expect.equal Nobody <| nextPlayer finishedGameModel
            ]
        , describe "playCell"
            [ test "should not allow to play when the game is finished" <|
                \_ ->
                    let
                        playedCell =
                            { isPlayed = True, player = X, index = 0 }
                    in
                    Expect.equal finishedGameModel <| playCell finishedGameModel playedCell
            , test "should allow to play when the game is not finished" <|
                \_ ->
                    let
                        playedCell =
                            { isPlayed = False, player = Nobody, index = 2 }

                        newGameModel =
                            playCell unfinishedGameModel playedCell

                        expectedGrid =
                            [ { isPlayed = True, player = X, index = 0 }
                            , { isPlayed = True, player = O, index = 1 }
                            , { isPlayed = True, player = X, index = 2 }
                            , { isPlayed = True, player = O, index = 3 }
                            , { isPlayed = True, player = X, index = 4 }
                            , { isPlayed = False, player = Nobody, index = 5 }
                            , { isPlayed = False, player = Nobody, index = 6 }
                            , { isPlayed = False, player = Nobody, index = 7 }
                            , { isPlayed = False, player = Nobody, index = 8 }
                            ]
                    in
                    Expect.equal expectedGrid <| (playCell unfinishedGameModel playedCell).board
            , test "should define the next player as nobody when the game is finished" <|
                \_ ->
                    -- TODO if playCell args were cell model This function could use pipes
                    let
                        model2 =
                            playCell unfinishedGameModel { isPlayed = False, player = Nobody, index = 2 }

                        model5 =
                            playCell model2 { isPlayed = False, player = Nobody, index = 5 }

                        model6 =
                            playCell model5 { isPlayed = False, player = Nobody, index = 6 }

                        model7 =
                            playCell model6 { isPlayed = False, player = Nobody, index = 7 }

                        finalGameModel =
                            playCell model7 { isPlayed = False, player = Nobody, index = 8 }
                    in
                    Expect.equal Nobody <| finalGameModel.currentPlayer
            ]
        ]


unfinishedGameModel : Model
unfinishedGameModel =
    let
        board =
            [ { isPlayed = True, player = X, index = 0 }
            , { isPlayed = True, player = O, index = 1 }
            , { isPlayed = False, player = Nobody, index = 2 }
            , { isPlayed = True, player = O, index = 3 }
            , { isPlayed = True, player = X, index = 4 }
            , { isPlayed = False, player = Nobody, index = 5 }
            , { isPlayed = False, player = Nobody, index = 6 }
            , { isPlayed = False, player = Nobody, index = 7 }
            , { isPlayed = False, player = Nobody, index = 8 }
            ]
    in
    { board = board
    , currentPlayer = X
    , isGameFinished = False
    , winner = Nobody
    , firstPlayer = X
    }


finishedGameModel : Model
finishedGameModel =
    let
        board =
            [ { isPlayed = True, player = X, index = 0 }
            , { isPlayed = True, player = O, index = 1 }
            , { isPlayed = True, player = X, index = 2 }
            , { isPlayed = True, player = O, index = 3 }
            , { isPlayed = True, player = X, index = 4 }
            , { isPlayed = True, player = O, index = 5 }
            , { isPlayed = True, player = X, index = 6 }
            , { isPlayed = True, player = O, index = 7 }
            , { isPlayed = True, player = X, index = 8 }
            ]
    in
    { board = board
    , currentPlayer = O
    , isGameFinished = True
    , winner = X
    , firstPlayer = X
    }


boardWithLineOfX : List Cell
boardWithLineOfX =
    [ { isPlayed = True, player = X, index = 0 }
    , { isPlayed = True, player = O, index = 1 }
    , { isPlayed = True, player = O, index = 2 }
    , { isPlayed = True, player = X, index = 3 }
    , { isPlayed = True, player = X, index = 4 }
    , { isPlayed = True, player = X, index = 5 }
    , { isPlayed = True, player = O, index = 6 }
    , { isPlayed = False, player = Nobody, index = 7 }
    , { isPlayed = True, player = O, index = 8 }
    ]


boardWithLineOfO : List Cell
boardWithLineOfO =
    [ { isPlayed = True, player = X, index = 0 }
    , { isPlayed = True, player = O, index = 1 }
    , { isPlayed = True, player = O, index = 2 }
    , { isPlayed = True, player = X, index = 3 }
    , { isPlayed = False, player = Nobody, index = 4 }
    , { isPlayed = True, player = X, index = 5 }
    , { isPlayed = True, player = O, index = 6 }
    , { isPlayed = True, player = O, index = 7 }
    , { isPlayed = True, player = O, index = 8 }
    ]


boardWithColumnOfO : List Cell
boardWithColumnOfO =
    [ { isPlayed = True, player = X, index = 0 }
    , { isPlayed = True, player = O, index = 1 }
    , { isPlayed = True, player = O, index = 2 }
    , { isPlayed = False, player = Nobody, index = 3 }
    , { isPlayed = True, player = O, index = 4 }
    , { isPlayed = False, player = Nobody, index = 5 }
    , { isPlayed = True, player = X, index = 6 }
    , { isPlayed = True, player = O, index = 7 }
    , { isPlayed = True, player = X, index = 8 }
    ]


boardWithColumnOfX : List Cell
boardWithColumnOfX =
    [ { isPlayed = True, player = X, index = 0 }
    , { isPlayed = True, player = O, index = 1 }
    , { isPlayed = True, player = O, index = 2 }
    , { isPlayed = True, player = X, index = 3 }
    , { isPlayed = False, player = Nobody, index = 4 }
    , { isPlayed = False, player = Nobody, index = 5 }
    , { isPlayed = True, player = X, index = 6 }
    , { isPlayed = True, player = O, index = 7 }
    , { isPlayed = True, player = X, index = 8 }
    ]


boardWithDiagonalOfO : List Cell
boardWithDiagonalOfO =
    [ { isPlayed = True, player = O, index = 0 }
    , { isPlayed = False, player = Nobody, index = 1 }
    , { isPlayed = True, player = X, index = 2 }
    , { isPlayed = False, player = Nobody, index = 3 }
    , { isPlayed = True, player = O, index = 4 }
    , { isPlayed = False, player = Nobody, index = 5 }
    , { isPlayed = True, player = X, index = 6 }
    , { isPlayed = False, player = Nobody, index = 7 }
    , { isPlayed = True, player = O, index = 8 }
    ]


boardWithDiagonalOfX : List Cell
boardWithDiagonalOfX =
    [ { isPlayed = True, player = O, index = 0 }
    , { isPlayed = False, player = Nobody, index = 1 }
    , { isPlayed = True, player = X, index = 2 }
    , { isPlayed = False, player = Nobody, index = 3 }
    , { isPlayed = True, player = X, index = 4 }
    , { isPlayed = False, player = Nobody, index = 5 }
    , { isPlayed = True, player = X, index = 6 }
    , { isPlayed = False, player = Nobody, index = 7 }
    , { isPlayed = True, player = O, index = 8 }
    ]
