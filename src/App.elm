module App exposing (main)

import Html exposing (program, div, button, text)
import Html.Events exposing (onClick)
import Random
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Svg exposing (point2dWith, lineSegment2d)
import Svg
import Svg.Attributes as Attributes
import Dict
import OpenSolid.LineSegment2d as Line
import List.Extra exposing (lift2)
import Task
import Time exposing (Time, millisecond)


hormoneRange =
    60


branchLength =
    1


budRange =
    4


numberPoints =
    2000


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
    if (List.isEmpty model.hormons) then
        Sub.none
    else
        Time.every millisecond (\_ -> Step)


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
    , lines : List Line.LineSegment2d
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    ( { hormons = []
      , buds =
            [ Point2d.fromCoordinates ( 300, 300 )
            ]
      , lines = []
      , seed = Random.initialSeed 1
      }
    , Cmd.none
    )


view model =
    div []
        [ button [ onClick CreateHormons ] [ text "Generate" ]
        , button [ onClick Grid ] [ text "Grid" ]
        , button [ onClick Step ] [ text "Step" ]
        , Svg.svg
            [ Attributes.width "600", Attributes.height "600", Attributes.viewBox "0 0 600 600" ]
            [ Svg.g
                [ Attributes.stroke "none"
                , Attributes.fill "HSLA(68, 63%, 37%, 1.00)"
                ]
                (List.map (point2dWith { radius = 1 } []) model.hormons)
            , Svg.g
                [ Attributes.stroke "#F7C92E"
                , Attributes.fill "none"
                ]
                (List.map (point2dWith { radius = budRange } []) model.buds)
            , Svg.g
                [ Attributes.stroke "HSLA(312, 69%, 17%, 1.00)"
                ]
                (List.map (lineSegment2d []) model.lines)
            ]
        ]


type Msg
    = CreateHormons
    | NewHormons (List ( Float, Float ))
    | Step
    | Grid


update msg model =
    case msg of
        CreateHormons ->
            ( model, (Random.generate NewHormons (randomPoints numberPoints)) )

        NewHormons hormons ->
            ( { model
                | hormons = List.map Point2d.fromCoordinates hormons
                , buds =
                    [ Point2d.fromCoordinates ( 300.01, 300.04 )
                    ]
                , lines = []
              }
            , Cmd.none
            )

        Grid ->
            let
                range =
                    List.range 0 10 |> List.map (toFloat >> ((*) 60))
            in
                ( { model
                    | hormons = List.map Point2d.fromCoordinates (lift2 (,) range range)
                    , buds =
                        [ Point2d.fromCoordinates ( 300, 300 )
                        ]
                    , lines = []
                  }
                , Cmd.none
                )

        Step ->
            ( step model, Cmd.none )


findNearestBud : List Bud -> Hormone -> Maybe ( Hormone, Bud )
findNearestBud buds h =
    (List.map (\b -> ( b, (Point2d.distanceFrom h b) )) buds)
        |> (List.filter (\( _, d ) -> d < hormoneRange))
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


calcNewPos ( _, { pos, vec } ) ( r, seed ) =
    let
        ( scaleFactor, newSeed ) =
            Debug.log ""
                (Random.step (Random.float 1 branchLength) seed)
    in
        ( ( pos
          , Point2d.translateBy
                (Vector2d.normalize vec
                    |> Vector2d.scaleBy
                        scaleFactor
                )
                pos
          )
            :: r
        , newSeed
        )


isNotNearBud seed buds h =
    (List.all (\( _, b ) -> (Point2d.distanceFrom h b) > Tuple.first (Random.step (Random.float 0 budRange) seed)) buds)


canMove ( _, { pos, vec } ) =
    vec /= Vector2d.fromComponents ( 0, 0 )


createLines b =
    List.map (\l -> Line.fromEndpoints l) b


branchBud : Bud -> ( Random.Seed, Buds ) -> ( Random.Seed, Buds )
branchBud bud ( seed, buds ) =
    let
        ( v, newSeed ) =
            Random.step (Random.int 0 10) seed
    in
        if (v > 2) then
            ( newSeed, bud :: buds )
        else
            ( newSeed, buds )


branchBuds : Buds -> Random.Seed -> ( Random.Seed, Buds )
branchBuds buds seed =
    (List.foldl branchBud ( seed, [] ) buds)


step : Model -> Model
step { hormons, buds, lines, seed } =
    let
        bs =
            List.map (\b -> ( (Point2d.coordinates b), { pos = b, vec = Vector2d.fromComponents ( 0, 0 ) } )) buds
                |> Dict.fromList

        ( nearestBuds, newSeed ) =
            List.map (findNearestBud buds) hormons
                |> List.filterMap identity
                |> List.foldl (\( h, b ) r -> Dict.update (Point2d.coordinates b) (updateBud h) r) bs
                |> Dict.toList
                |> List.filter canMove
                |> List.foldl calcNewPos ( [], seed )

        ( _, newBuds ) =
            branchBuds buds seed
    in
        { hormons = List.filter (isNotNearBud seed nearestBuds) hormons
        , buds = List.append newBuds (List.map Tuple.second nearestBuds)
        , lines = List.append lines (createLines nearestBuds)
        , seed = newSeed
        }
