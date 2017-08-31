import Html exposing (Html, div, h2, text)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type Player
  = X
  | O

type alias Model =
  { turn : Player
  , xwins : Int
  , owins : Int
  }


init : (Model, Cmd Msg)
init =
  (Model X 0 0, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Pounce"]
    ]

-- UPDATE

type Msg
  = Click Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
