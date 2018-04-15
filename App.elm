module App exposing (..)

import Dict exposing ( Dict, fromList, get, toList )
import Html exposing ( Html, a, div, img, option, p, select, text)
import Html.Attributes exposing ( alt, href, selected, src, value )
import Html.Events exposing ( onInput )
import List.Extra exposing ( cycle, zip )
import Set exposing ( Set, fromList, member)
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

type alias Tonality =
  Dict Int String

type alias Mode =
  List Int

type alias Scale =
  Set String

type alias Model =
  { tonality: Shift
  , harp: Shift
  , mode: Mode
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
  if List.member shift sharpTonalities
  then Sharp
  else Flat

accidentalType : Shift -> Accidental
accidentalType shift =
  if shift.accidental /= Natural
  then shift.accidental
  else tonalityType shift.value

tonalitySharp : Int -> Tonality
tonalitySharp shiftValue =
  cycle 24 chromaticSharp
    |> List.drop shiftValue
    |> List.take 12
    |> List.Extra.zip (List.range 1 12)
    |> Dict.fromList

tonalityFlat : Int -> Tonality
tonalityFlat shiftValue =
  cycle 24 chromaticFlat
    |> List.drop shiftValue
    |> List.take 12
    |> List.Extra.zip (List.range 1 12)
    |> Dict.fromList

tonality : Shift -> Tonality
tonality shift =
  let accidental = (accidentalType shift) in
  case accidental of
    Sharp -> tonalitySharp shift.value
    _ -> tonalityFlat shift.value

degreeToNote : Int -> Tonality -> String
degreeToNote degree curTonality =
  Maybe.withDefault "" (Dict.get degree curTonality)

naturalMajorMode : Mode
naturalMajorMode = [ 1, 3, 5, 6, 8, 10, 12 ]

pentatonicMode : Mode
pentatonicMode = [ 1, 3, 5, 8, 10 ]

bluesMode : Mode
bluesMode = [ 1, 4, 6, 7, 8, 11 ]

majorBluesMode : Mode
majorBluesMode = [ 1, 3, 4, 5, 8, 10 ]

enharmonicEquivalents : Dict String String
enharmonicEquivalents =
  zip chromaticFlat chromaticSharp ++ zip chromaticSharp chromaticFlat
    |> List.filter (\ (a, b) -> a /= b)
    |> Dict.fromList

addEquivalents : List String -> List String
addEquivalents notes =
  let eqv = List.foldl (\note acc -> case (Dict.get note enharmonicEquivalents) of
                                       Just e -> e :: acc
                                       Nothing -> acc)
            []
            notes
  in
    notes ++ eqv

scale : Shift -> Mode -> Scale
scale shift mode =
  tonality shift
    |> Dict.toList
    |> List.filter (\ (n, _) -> List.member n mode)
    |> List.map (\ (_, x) -> x)
    |> addEquivalents
    |> Set.fromList

init : (Model, Cmd Msg)
init =
  ((Model (Shift 0 Natural) (Shift 0 Natural) naturalMajorMode), Cmd.none)

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

drawHole : String -> Int -> Int -> Scale -> Svg Msg
drawHole note hx hy scale =
  g []
    [ rect [ x (toString (hx * 50))
           , y (toString (hy * 50))
           , width "50"
           , height "50"
           , fill (if Set.member note scale then "#00ff00" else "none")
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

drawRow : List Int -> Int -> Tonality -> Scale -> Svg Msg
drawRow row rowNumber curHarpTonality curScale =
  g []
    (List.Extra.zip row (List.range 0 10)
       |> List.filter (\ (degree, _) ->
                         degree /= 0)
       |> List.map (\ (degree, column) ->
                      drawHole (degreeToNote degree curHarpTonality) column rowNumber curScale))

drawLayout : List (List Int) -> Tonality -> Scale -> Svg Msg
drawLayout layout curHarpTonality curScale =
  g []
    (List.Extra.zip layout (List.range 0 7)
       |> List.map (\ (row, rowNumber) ->
                      drawRow row rowNumber curHarpTonality curScale))

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ svg [ viewBox "0 0 500 380", width "500px" ]
            [ (drawLayout layout
                          (tonality model.harp)
                          (scale model.tonality model.mode))
            ]
        ]
    , div []
        [ p [] [ text "Scale" ]
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
        , select [ onInput ModeUpdate ]
            [ option [value "Natural major"]
                     [Html.text "Natural major"]
            , option [value "Pentatonic"]
                     [Html.text "Pentatonic"]
            , option [value "Blues"]
                     [Html.text "Blues"]
            , option [value "Major blues"]
                     [Html.text "Major blues"]

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
      , div []
          [ p [] [ text "ⓒYuriy Shirokov ("
                 , a [ href "https://github.com/yashrk" ] [ Html.text "github.com/yashrk" ]
                 , text "), 2018"
                 ]
          , p [] [ text "Source code: "
                 , a [ href "https://github.com/yashrk/harmonica-scale-calculator" ]
                     [ Html.text "github.com/yashrk/harmonica-scale-calculator" ]
                 ]
          , p [] [ text "License: "
                 , a [ href "https://www.gnu.org/licenses/agpl.html" ]
                     [ Html.text "AGPLv3" ]
                 ]
          , p [] [ a [ href "https://www.gnu.org/licenses/agpl.html" ]
                     [img [ src "https://yashrk.github.io/images/agplv3.png", alt "AGPLv3" ] []]
                 ]
          ]
    ]

-- UPDATE

type Msg
  = HarpUpdate String
  | HarpAccUpdate String
  | ModeUpdate String
  | TonicUpdate String
  | TonicAccUpdate String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HarpUpdate newHarp ->
      Result.withDefault 0 (toInt newHarp)
      |> (\t -> ({ model | harp = (Shift t model.harp.accidental) }, Cmd.none))
    HarpAccUpdate newAccStr ->
      let newAcc = case newAccStr of
                     "Flat" -> Flat
                     "Sharp" -> Sharp
                     _ -> Natural
      in
        ({ model | harp = (Shift model.harp.value newAcc)}, Cmd.none)
    ModeUpdate newModeStr ->
      let newMode = case newModeStr of
                      "Pentatonic" -> pentatonicMode
                      "Blues" -> bluesMode
                      "Major blues" -> majorBluesMode
                      _ -> naturalMajorMode

      in
      ({model | mode = newMode }, Cmd.none)
    TonicUpdate newTonic ->
      Result.withDefault 0 (toInt newTonic)
      |> (\t -> ({ model | tonality = (Shift t model.tonality.accidental) }, Cmd.none))
    TonicAccUpdate newAccStr ->
      let newAcc = case newAccStr of
                     "Flat" -> Flat
                     "Sharp" -> Sharp
                     _ -> Natural
      in
        ({ model | tonality = (Shift model.tonality.value newAcc)}, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
