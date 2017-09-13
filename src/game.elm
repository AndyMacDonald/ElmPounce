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
            next = nextMove humanMove
            robotMove
                = case next of
                  Just idx -> Board.update (Clicked idx) humanMove
                  Nothing -> humanMove
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

type alias Index = Int
type alias Score = Int
type alias Color = Int
type alias Depth = Int

nextMove : Board.Model -> Maybe Index
nextMove board =
    negamax scorer board 2 1 |> Tuple.second

-- scoreFn returns board score from O's viewpoint
-- board is current node in the tree
-- depth is how many more layers to evaluate below this
-- color is 1 for O and -1 for X (robot is always O)
-- returns pair of (best score, number of square for that score)
negamax : (Board.Model -> Score) -> Board.Model -> Depth -> Color -> (Score, Maybe Index)
negamax scoreFn board depth color =
  if depth == 0 || Set.isEmpty board.moves then
    (color * scoreFn board, Nothing)
  else
    let
      candidates = List.map 
                    (\x -> (-(Tuple.first (negamax scoreFn (Board.update (Clicked x) board) (depth - 1) -color)), Just x))
                    (Set.toList board.moves)
    in
        case (List.sortBy (\x -> -(Tuple.first x)) candidates |> List.head) of
            Just x -> x
            Nothing -> (-win, Nothing)

scorer : Board.Model -> Score
scorer board =
  case board.next of
    XMove -> -(Set.size board.moves)
    OMove -> Set.size board.moves
    XPounced -> -win
    OPounced -> win
    XBlocked -> win
    OBlocked -> -win

win : Score
win =
  1000000
