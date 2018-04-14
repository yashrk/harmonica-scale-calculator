module App exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing ( value, selected )
import Html.Events exposing ( onInput )
import List.Extra exposing ( cycle, zip )
import String exposing ( toInt )
import Svg exposing ( Svg, g, rect, svg, text_ )
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
  | Natural
  | Flat

type alias Shift =
  { value: Int
  , accidental: Accidental
  }

type alias Model =
  { tonality: Shift
  , harp: Shift
  }

chromaticSharp : List String
chromaticSharp = [ "c", "c♯", "d", "d♯", "e", "f", "f♯", "g", "g♯", "a", "a♯", "h" ]

chromaticFlat : List String
chromaticFlat =  [ "c", "d♭", "d", "e♭", "e", "f", "g♭", "g", "a♭", "a", "b", "h" ]

sharpTonalities : List Int
sharpTonalities = [ 2, 4, 7, 9, 11 ] -- Shift to D, E, G, A, H

flatTonalities : List Int
flatTonalities = [ 0, 1, 3, 5, 6, 8, 10 ] -- Shift to C, D♭, E♭, F, G♭, A♭, B

tonalityType : Int -> Accidental
tonalityType shift =
  if List.any (\x -> x == shift) sharpTonalities
  then Sharp
  else Flat

accidentalType : Shift -> Accidental
accidentalType shift =
  if shift.accidental /= Natural
  then shift.accidental
  else tonalityType shift.value

tonalitySharp : Int -> Dict Int String
tonalitySharp shiftValue =
  cycle 24 chromaticSharp
    |> List.drop shiftValue
    |> List.take 12
    |> List.Extra.zip (List.range 1 12)
    |> Dict.fromList

tonalityFlat : Int -> Dict Int String
tonalityFlat shiftValue =
  cycle 24 chromaticFlat
    |> List.drop shiftValue
    |> List.take 12
    |> List.Extra.zip (List.range 1 12)
    |> Dict.fromList

tonality : Shift -> Dict Int String
tonality shift =
  let accidental = (accidentalType shift) in
  case accidental of
    Sharp -> tonalitySharp shift.value
    _ ->  tonalityFlat shift.value

degreeToNote : Int -> Dict Int String -> String
degreeToNote degree curTonality =
  Maybe.withDefault "" (Dict.get degree curTonality)

init : (Model, Cmd Msg)
init =
  ((Model (Shift 0 Natural) (Shift 0 Natural)), Cmd.none)

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
        [ svg [ viewBox "0 0 500 380", width "500px" ]
            [ (drawLayout layout (tonality model.harp))
            ]
        ]
    , div []
        [ p [] [ text "Tonality" ]
        , select [ onInput TonicUpdate ]
            [ option [value "0"]  [Html.text "C"]
            , option [value "2"]  [Html.text "D"]
            , option [value "4"]  [Html.text "E"]
            , option [value "5"]  [Html.text "F"]
            , option [value "7"]  [Html.text "G"]
            , option [value "9"]  [Html.text "A"]
            , option [value "10"] [Html.text "B"]
            , option [value "11"] [Html.text "H"]
            ]
        , select [ onInput TonicAccUpdate ]
            [ option [value "Flat", selected (model.tonality.accidental == Flat)]
                     [Html.text "♭"]
            , option [value "Natural", selected (model.tonality.accidental == Natural)]
                     [Html.text "♮"]
            , option [value "Sharp", selected (model.tonality.accidental == Sharp)]
                     [Html.text "♯"]
            ]
        ]
    , div []
        [ p [] [ text "Harp" ]
        , select [ onInput HarpUpdate ]
            [ option [value "0"]  [Html.text "C"]
            , option [value "2"]  [Html.text "D"]
            , option [value "4"]  [Html.text "E"]
            , option [value "5"]  [Html.text "F"]
            , option [value "7"]  [Html.text "G"]
            , option [value "9"]  [Html.text "A"]
            , option [value "10"] [Html.text "B"]
            , option [value "11"] [Html.text "H"]
            ]
        , select [ onInput HarpAccUpdate ]
            [ option [value "Flat", selected (model.harp.accidental == Flat)]
                     [Html.text "♭"]
            , option [value "Natural", selected (model.harp.accidental == Natural)]
                     [Html.text "♮"]
            , option [value "Sharp", selected (model.harp.accidental == Sharp)]
                     [Html.text "♯"]
            ]
        ]
    ]

-- UPDATE

type Msg
  = HarpUpdate String
  | HarpAccUpdate String
  | TonicUpdate String
  | TonicAccUpdate String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HarpUpdate newHarp ->
      Result.withDefault 1 (toInt newHarp)
      |> (\t -> ( { model | harp = (Shift t model.harp.accidental) }, Cmd.none))
    HarpAccUpdate newAccStr ->
      let newAcc = case newAccStr of
                     "Flat" -> Flat
                     "Sharp" -> Sharp
                     _ -> Natural
      in
        ({ model | harp = (Shift model.harp.value newAcc)}, Cmd.none)
    TonicUpdate newTonic ->
      (model, Cmd.none)
    TonicAccUpdate _ ->
      (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
