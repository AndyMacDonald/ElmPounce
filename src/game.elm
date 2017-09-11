import Html exposing (Html, div, h2, text, button, map)
import Html.Events exposing (onClick)
import Board exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model =
  { board : Board.Model
  }

type Msg
  = Board Board.Msg
  | Reset

init : (Model, Cmd Msg)
init =
  (Model Board.init, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Pounce"]
    , button [ onClick Reset ] [ text "Reset" ]
    , map (\a -> Board a) (Board.view model.board)
    , Html.p [] [text (statusText model.board)]
    ]

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Board boardMsg -> 
      ({model | board = Board.update boardMsg model.board}, Cmd.none)
    Reset ->
      (Model Board.init, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
