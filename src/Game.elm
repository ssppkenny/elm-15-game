module Game exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import List exposing (length)
import List.Extra exposing (getAt)


mystring n =
    if n == 0 then
        ""

    else
        String.fromInt n


view model =
    div []
        [ div [ class "title" ]
            [ h2 [] [ text "15 Game" ] ]
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
                                    , onClick { description = "Clicked", x = j, y = i }
                                    ]
                                    [ text (mystring n) ]
                            )
                            d
                        )
                )
                model
            )
        ]


type alias Model =
    List (List Int)


neighbors x y =
    List.filter (\( a, b ) -> a >= 0 && a < 4 && b >= 0 && b < 4) [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]


getValue i j mod =
    Maybe.withDefault -1 (getAt i (Maybe.withDefault [] (getAt j mod)))


type alias Msg =
    { x : Int, y : Int, description : String }


initialModel : Model
initialModel =
    [ [ 1, 2, 3, 4 ]
    , [ 5, 6, 7, 8 ]
    , [ 9, 10, 11, 12 ]
    , [ 13, 14, 15, 0 ]
    ]


findZero msg model =
    let
        coords =
            neighbors msg.x msg.y

        outlist =
            List.filter (\( i, j ) -> getValue i j model == 0) coords
    in
    if length outlist == 0 then
        Nothing

    else
        getAt 0 outlist


update msg model =
    let
        zeroCoords =
            findZero msg model
    in
    case zeroCoords of
        Just ( x, y ) ->
            ( List.indexedMap
                (\j row ->
                    List.indexedMap
                        (\i cell ->
                            if i == x && j == y then
                                getValue msg.x msg.y model

                            else if i == msg.x && j == msg.y then
                                0

                            else
                                cell
                        )
                        row
                )
                model
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


initialCmd =
    Cmd.none


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
