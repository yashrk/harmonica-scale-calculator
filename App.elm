import Dict exposing (..)
import Html exposing (..)
import List.Extra exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type Accidental
  = Sharp
  | Flat

type alias Shift =
  { value: Int
  , accidental: Accidental
  }

type alias Model =
  ()

chromaticSharp : List String
chromaticSharp = [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "h" ]

chromaticFlat : List String
chromaticFlat =  [ "c", "d♭", "d", "e♭", "e", "f", "g♭", "g", "a♭", "a", "b", "h" ]

tonalitySharp : Int -> Dict Int String
tonalitySharp shift =
  List.Extra.zip (List.range 1 13) (cycle 24 chromaticSharp)
    |> List.drop shift
    |> List.take 12
    |> Dict.fromList

tonalityFlat : Int -> Dict Int String
tonalityFlat shift =
  List.Extra.zip (List.range 1 13) (cycle 24 chromaticFlat)
    |> List.drop shift
    |> List.take 12
    |> Dict.fromList

tonality : Shift -> Dict Int String
tonality shift =
  case shift.accidental of
    Sharp -> tonalitySharp shift.value
    Flat ->  tonalityFlat shift.value

degreeToNote : Int -> Dict Int String -> String
degreeToNote degree curTonality =
  Maybe.withDefault "" (Dict.get degree curTonality)

init : (Model, Cmd Msg)
init =
  ((), Cmd.none)

-- VIEW

layout : List (List Int)
layout =
  [ [ 0, 0, 0,  0, 0, 0,  0,  0, 0, 11 ]
  , [ 0, 0, 0,  0, 0, 0,  0,  4, 7, 12 ]
  , [ 1, 5, 8,  1, 5, 8,  1,  5, 8, 1  ]
  , [ 3, 8, 12, 3, 6, 10, 12, 3, 6, 10 ]
  , [ 2, 7, 11, 2, 0, 9,  0,  0, 0, 0  ]
  , [ 0, 6, 10, 0, 0, 0,  0,  0, 0, 0  ]
  , [ 0, 0, 9,  0, 0, 0,  0,  0, 0, 0  ]
  ]

drawHole : String -> Int -> Int -> Svg Msg
drawHole note hx hy =
  g []
    [ rect [ x (toString (hx * 50))
           , y (toString (hy * 50))
           , width "50"
           , height "50"
           , fill "none"
           , stroke "black"
           , strokeWidth "3px"
           ] []
    , text_ [ x (toString ((hx * 50) + 10))
            , y (toString ((hy * 50) + 30))
            , fontSize "25"
            , fontFamily "Courier"
            , fontWeight "Bold"
            ] [ Html.text note ]
    ]

drawRow : List Int -> Int -> Dict Int String -> Svg Msg
drawRow row rowNumber curTonality =
  g []
    (List.Extra.zip row (List.range 0 10)
       |> List.filter (\ (degree, _) ->
                         degree /= 0)
       |> List.map (\ (degree, column) ->
                      drawHole (degreeToNote degree curTonality) column rowNumber))

drawLayout : List (List Int) -> Dict Int String -> Svg Msg
drawLayout layout curTonality =
  g []
    (List.Extra.zip layout (List.range 0 7)
       |> List.map (\ (row, rowNumber) ->
                      drawRow row rowNumber curTonality))

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ svg [ viewBox "0 0 500 500", width "500px" ]
            [ (drawLayout layout (tonality (Shift 0 Flat)))
            ]
        ]
    , div []
        [
        ]
    ]

-- UPDATE

type Msg =
  None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    None ->
      (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
