module Game exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import Keyboard
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
    | KeyMsg Keyboard.RawKey


initialModel : Model
initialModel =
    { time = 0
    , board =
        [ [ 3, 2, 4, 7 ]
        , [ 12, 5, 14, 0 ]
        , [ 13, 8, 9, 1 ]
        , [ 10, 6, 15, 11 ]
        ]
    }


indexOfZeroHelper lst i =
    case lst of
        0 :: xs ->
            i

        t :: xs ->
            indexOfZeroHelper xs (i + 1)

        [] ->
            -1


indexOfZero lst =
    indexOfZeroHelper lst 0


findEmptyCell model =
    let
        fl =
            List.concat model.board

        pos =
            indexOfZero fl

        x =
            modBy 4 pos

        y =
            pos // 4
    in
    ( x, y )


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


findUpperCell x y =
    if y == 0 then
        Nothing

    else
        Just ( x, y - 1 )


findLowerCell x y =
    if y == 3 then
        Nothing

    else
        Just ( x, y + 1 )


findLeftCell x y =
    if x == 0 then
        Nothing

    else
        Just ( x - 1, y )


findRightCell x y =
    if x == 3 then
        Nothing

    else
        Just ( x + 1, y )


newModel x y cx cy model =
    { model
        | board =
            List.indexedMap
                (\j row ->
                    List.indexedMap
                        (\i cell ->
                            if i == x && j == y then
                                getValue cx cy model.board

                            else if i == cx && j == cy then
                                0

                            else
                                cell
                        )
                        row
                )
                model.board
    }


update msg model =
    case msg of
        KeyMsg code ->
            case Keyboard.whitespaceKey code of
                Just key ->
                    ( { model | board = shuffleBoard model.board model.time }, Cmd.none )

                Nothing ->
                    case Keyboard.navigationKey code of
                        Just Keyboard.ArrowLeft ->
                            let
                                ( x, y ) =
                                    findEmptyCell model

                                nm =
                                    case findRightCell x y of
                                        Just ( x1, y1 ) ->
                                            newModel x y x1 y1 model

                                        Nothing ->
                                            model
                            in
                            ( nm, Cmd.none )

                        Just Keyboard.ArrowRight ->
                            let
                                ( x, y ) =
                                    findEmptyCell model

                                nm =
                                    case findLeftCell x y of
                                        Just ( x1, y1 ) ->
                                            newModel x y x1 y1 model

                                        Nothing ->
                                            model
                            in
                            ( nm, Cmd.none )

                        Just Keyboard.ArrowDown ->
                            let
                                ( x, y ) =
                                    findEmptyCell model

                                nm =
                                    case findUpperCell x y of
                                        Just ( x1, y1 ) ->
                                            newModel x y x1 y1 model

                                        Nothing ->
                                            model
                            in
                            ( nm, Cmd.none )

                        Just Keyboard.ArrowUp ->
                            let
                                ( x, y ) =
                                    findEmptyCell model

                                nm =
                                    case findLowerCell x y of
                                        Just ( x1, y1 ) ->
                                            newModel x y x1 y1 model

                                        Nothing ->
                                            model
                            in
                            ( nm, Cmd.none )

                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

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
                    ( newModel x y state.x state.y model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


initialCmd =
    Task.perform Tick Time.now


subscriptions model =
    Keyboard.downs KeyMsg


init =
    \flags -> ( initialModel, initialCmd )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
