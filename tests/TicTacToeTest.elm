module TicTacToeTest exposing (suite)

import Debug
import Expect exposing (Expectation)
import Test exposing (..)
import TicTacToe
    exposing
        ( Cell
        , Model
        , Player(..)
        , initialModel
        , isGameFinished
        , main
        , nextPlayer
        , playCell
        )


suite : Test
suite =
    describe "The tic tac toe game"
        [ describe "is game finished"
            [ test "should detect when the game is not finished" <|
                \_ ->
                    Expect.equal False <| isGameFinished unfinishedGameModel
            , test "should detect end of the game" <|
                \_ ->
                    Expect.equal True <| isGameFinished finishedGameModel
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
                            ]
                    in
                    Expect.equal expectedGrid <| (playCell unfinishedGameModel playedCell).board
            , test "should define the next player as nobody when the game is finished" <|
                \_ ->
                    let
                        playedCell1 =
                            { isPlayed = False, player = Nobody, index = 2 }

                        playedCell2 =
                            { isPlayed = False, player = Nobody, index = 5 }

                        intermediateGameModel =
                            playCell unfinishedGameModel playedCell1

                        finalGameModel =
                            playCell intermediateGameModel playedCell2
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
            ]
    in
    { board = board
    , currentPlayer = X
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
            ]
    in
    { board = board
    , currentPlayer = O
    }
