module App exposing (main)

import Html exposing (program, div, button, text)
import Html.Events exposing (onClick)
import Random
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Svg exposing (point2d)
import Svg
import Svg.Attributes as Attributes
import Dict


randomPoint : Random.Generator ( Float, Float )
randomPoint =
    Random.pair (Random.float 0 600) (Random.float 0 600)


randomPoints : Int -> Random.Generator (List ( Float, Float ))
randomPoints cnt =
    Random.list cnt randomPoint


main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Buds =
    List Point2d.Point2d


type alias Hormone =
    Point2d.Point2d


type alias Bud =
    Point2d.Point2d


type alias Hormones =
    List Hormone


type alias Model =
    { hormons : Hormones
    , buds : Buds
    }


init : ( Model, Cmd Msg )
init =
    ( { hormons = []
      , buds =
            [ Point2d.fromCoordinates ( 0, 0 )
            ]
      }
    , Cmd.none
    )


view model =
    div []
        [ button [ onClick CreateHormons ] [ text "Generate" ]
        , button [ onClick Step ] [ text "Step" ]
        , Svg.svg
            [ Attributes.width "600", Attributes.height "600", Attributes.viewBox "0 0 600 600" ]
            [ Svg.g
                [ Attributes.stroke "black"
                , Attributes.fill "white"
                ]
                (List.map (point2d []) model.hormons)
            , Svg.g
                [ Attributes.stroke "red"
                , Attributes.fill "white"
                ]
                (List.map (point2d []) model.buds)
            ]
        , div [] [ text (toString model) ]
        ]


type Msg
    = CreateHormons
    | NewHormons (List ( Float, Float ))
    | Step


update msg model =
    case msg of
        CreateHormons ->
            ( model, (Random.generate NewHormons (randomPoints 1000)) )

        NewHormons hormons ->
            ( { model | hormons = List.map Point2d.fromCoordinates hormons }, Cmd.none )

        Step ->
            ( step model, Cmd.none )


findNearestBud : List Bud -> Hormone -> Maybe ( Hormone, Bud )
findNearestBud buds h =
    (List.map (\b -> ( b, (Point2d.distanceFrom h b) )) buds)
        |> (List.filter (\( _, d ) -> d < 40))
        |> (List.sortBy Tuple.second)
        |> List.head
        |> Maybe.map (\b -> ( h, Tuple.first b ))


updateBud : Hormone -> Maybe { pos : Bud, vec : Vector2d.Vector2d } -> Maybe { pos : Bud, vec : Vector2d.Vector2d }
updateBud h b =
    case b of
        Nothing ->
            Nothing

        Just bud ->
            Just { bud | vec = Vector2d.sum bud.vec (Vector2d.from bud.pos h) }


calcNewPos ( _, { pos, vec } ) =
    let
        a =
            Debug.log "" (Vector2d.normalize vec |> Vector2d.scaleBy 10)
    in
        Point2d.translateBy (Vector2d.normalize vec |> Vector2d.scaleBy 40) pos


isNotNearBud : Buds -> Hormone -> Bool
isNotNearBud buds h =
    (List.any (\b -> (Point2d.distanceFrom h b) > 20) buds)


step : Model -> Model
step { hormons, buds } =
    let
        bs =
            List.map (\b -> ( (Point2d.coordinates b), { pos = b, vec = Vector2d.fromComponents ( 0, 0 ) } )) buds
                |> Dict.fromList

        nearestBuds =
            List.map (findNearestBud buds) hormons
                |> List.filterMap identity
                |> List.foldl (\( h, b ) r -> Dict.update (Point2d.coordinates b) (updateBud h) r) bs
                |> Dict.toList
                |> List.map calcNewPos
    in
        { hormons = List.filter (isNotNearBud nearestBuds) hormons, buds = nearestBuds }
