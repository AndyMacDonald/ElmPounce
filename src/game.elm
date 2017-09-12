import Html exposing (Html, div, h2, text, button, map, fieldset, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Board exposing (..)
import Set exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type Opponent
  = Human
  | Robot

type alias Model =
  { board : Board.Model
  ,  opponent : Opponent
  }

type Msg
  = Board Board.Msg
  | Reset
  | Opponent Opponent

init : (Model, Cmd Msg)
init =
  (Model Board.init Robot, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Pounce"]
    , Html.p [] [text (statusText model.board)]
    , Html.map (\a -> Board a) (Board.view model.board)
    , button [ onClick Reset ] [ text "Reset" ]
    , fieldset []
      [ radio "One player" (model.opponent == Robot) (Opponent Robot)
      , radio "Two player" (model.opponent == Human) (Opponent Human)
      ]
    ]

radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "radio", onClick msg, checked isChecked ] []
    , text value
    ]

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Board boardMsg ->
      case model.opponent of
        Human -> ({model | board = Board.update boardMsg model.board}, Cmd.none)

        Robot ->
          let
            humanMove = Board.update boardMsg model.board
            (score, next) = negamax scorer humanMove 2 1
            robotMove
              = if next == -1 then
                  humanMove
                else
                  Board.update (Clicked next) humanMove
          in
            ({model | board = robotMove}, Cmd.none)

    Reset ->
      (Model Board.init model.opponent, Cmd.none)

    Opponent opponent ->
      (Model Board.init opponent, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



----------------------
-- NEGAMAX code for a robot to play against

-- scoreFn returns board score from O's viewpoint
-- board is current node in the tree
-- depth is how many more layers to evaluate below this
-- color is 1 for O and -1 for X (robot is always O)
-- returns pair of (best score, number of square for that score)
negamax : (Board.Model -> Int) -> Board.Model -> Int -> Int -> (Int, Int)
negamax scoreFn board depth color =
  if depth == 0 || Set.isEmpty board.moves then
    (color * scoreFn board, -1)
  else
    let
      candidates = List.map 
                    (\x -> (-(Tuple.first (negamax scoreFn (Board.update (Clicked x) board) (depth - 1) -color)), x))
                    (Set.toList board.moves)
      best = case (List.sortBy (\x -> -(Tuple.first x)) candidates |> List.head) of
        Just x -> x
        Nothing -> (-win, 0)
    in
      best

scorer : Board.Model -> Int
scorer board =
  case board.next of
    XMove -> -(Set.size board.moves)
    OMove -> Set.size board.moves
    XPounced -> -win
    OPounced -> win
    XBlocked -> win
    OBlocked -> -win

win : Int
win =
  1000000