module Game exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import List exposing (length)
import List.Extra exposing (getAt)
import List.Split exposing (chunksOfLeft)
import Random
import Random.List exposing (shuffle)
import Task
import Time exposing (..)


mystring n =
    if n == 0 then
        ""

    else
        String.fromInt n


shuffleBoard board seed =
    let
        flat =
            List.concat board

        s =
            Random.initialSeed seed

        g =
            shuffle flat

        shuffledList =
            Tuple.first (Random.step g s)

        newBoard =
            chunksOfLeft 4 shuffledList
    in
    newBoard


view model =
    div []
        [ div [ class "title" ]
            [ h2 [] [ text "15 Game" ]
            , button [ class "button", onClick Shuffle ] [ text "Shuffle" ]
            ]
        , div
            [ class "game" ]
            (List.indexedMap
                (\i d ->
                    div [ class "grid-container" ]
                        (List.indexedMap
                            (\j n ->
                                div
                                    [ if n == 0 then
                                        class "emptysquare"

                                      else
                                        class "square"
                                    , attribute "data" (String.fromInt (i * 4 + j))
                                    , onClick (State { description = "Clicked", x = j, y = i })
                                    ]
                                    [ text (mystring n) ]
                            )
                            d
                        )
                )
                model.board
            )
        ]


type alias Model =
    { time : Int, board : List (List Int) }


neighbors x y =
    List.filter (\( a, b ) -> a >= 0 && a < 4 && b >= 0 && b < 4) [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]


getValue i j board =
    Maybe.withDefault -1 (getAt i (Maybe.withDefault [] (getAt j board)))


type Msg
    = Tick Time.Posix
    | Shuffle
    | State { x : Int, y : Int, description : String }


initialModel : Model
initialModel =
    { time = 0
    , board =
        [ [ 1, 2, 3, 4 ]
        , [ 5, 6, 7, 8 ]
        , [ 9, 10, 11, 12 ]
        , [ 13, 14, 15, 0 ]
        ]
    }


findZero msg model =
    let
        coords =
            neighbors msg.x msg.y

        outlist =
            List.filter (\( i, j ) -> getValue i j model.board == 0) coords
    in
    if length outlist == 0 then
        Nothing

    else
        getAt 0 outlist


update msg model =
    case msg of
        Shuffle ->
            ( { model | board = shuffleBoard model.board model.time }, Cmd.none )

        Tick newTime ->
            ( { model | time = posixToMillis newTime }, Cmd.none )

        State state ->
            let
                zeroCoords =
                    findZero state model
            in
            case zeroCoords of
                Just ( x, y ) ->
                    ( { model
                        | board =
                            List.indexedMap
                                (\j row ->
                                    List.indexedMap
                                        (\i cell ->
                                            if i == x && j == y then
                                                getValue state.x state.y model.board

                                            else if i == state.x && j == state.y then
                                                0

                                            else
                                                cell
                                        )
                                        row
                                )
                                model.board
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


initialCmd =
    Task.perform Tick Time.now


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
