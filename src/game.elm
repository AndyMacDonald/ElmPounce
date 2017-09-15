import Html exposing (Html, div, h2, text, button, map, fieldset, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Board exposing (..)
import Set exposing (..)
import Task exposing (..)
import Process exposing (..)
import Time exposing (..)

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
  , opponent : Opponent
  }

type Msg
  = Board Board.Msg
  | Reset
  | Opponent Opponent
  | RobotMove Index
  | Hold

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
                in
                    ({model | board = humanMove}, runRobot humanMove)

    Reset ->
      (Model Board.init model.opponent, Cmd.none)

    Opponent opponent ->
      (Model Board.init opponent, Cmd.none)

    RobotMove idx ->
        ({model | board = Board.update (Clicked idx) model.board}, Cmd.none)

    Hold ->
        (model, Cmd.none)

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

runRobot : Board.Model -> Cmd Msg
runRobot board =
    Task.perform
        (\a -> case a of
            Just idx -> RobotMove idx
            Nothing -> Hold)
        (Task.andThen
            (\_ -> Task.succeed (nextMove board))
            (Process.sleep (50 * Time.millisecond)))

nextMove : Board.Model -> Maybe Index
nextMove board =
    negamax scorer board 3 (win * -2) (win * 2) 1 |> Tuple.second

-- scoreFn returns board score from O's viewpoint
-- board is current node in the tree
-- depth is how many more layers to evaluate below this
-- alpha is the lower bound of child node values at this depth
-- beta is the uppor bound of child node values at this depth
-- color is 1 for O and -1 for X (robot is always O)
-- returns pair of (best score, number of square for that score)
negamax : (Board.Model -> Score) -> Board.Model -> Depth -> Score -> Score -> Color -> (Score, Maybe Index)
negamax scoreFn board depth alpha beta color =
  if depth == 0 || Set.isEmpty board.moves then
    (color * scoreFn board, Nothing)
  else
    let
        candidates = List.map (\x -> (x, (Board.update (Clicked x) board))) (Set.toList board.moves)
    in
        alphaBetaPruner scoreFn candidates depth alpha beta color (-1000 * win, Nothing)


alphaBetaPruner : (Board.Model -> Score) -> List (Index, Board.Model) -> Depth -> Score -> Score -> Color -> (Score, Maybe Index) -> (Score, Maybe Index)
alphaBetaPruner scoreFn boards depth alpha beta color best =
    case boards of
        (idx, board) :: rest ->
            let
                (negScore, i2) = negamax scorer board (depth - 1) -beta -alpha -color
                newBest = 
                    if Tuple.first best >= -negScore then
                        best
                    else
                        (-negScore, Just idx)
                newAlpha = Basics.max alpha -negScore
            in
                if newAlpha < beta then
                    alphaBetaPruner scoreFn rest depth alpha beta color newBest
                else
                    newBest

        [] -> best

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
