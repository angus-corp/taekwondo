module Admin exposing (..)

--TODO: Automatic infinite scrolling on scroll-to-bottom.
--TODO: STYLING.
--TODO: DEBOUNCE.

import Auth
import Dict exposing (Dict)
import Date exposing (Date)
import Json.Encode as E
import Json.Decode as D
import Messages
import Navbar exposing (navbar)
import LocalStorage
import Task exposing (Task)
import Html exposing (..)
import Http
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onClick)



pageLength = 30



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL



type alias Model =
  { query   : String
  , results : List Student
  , state   : State
  , token   : Maybe Auth.Token
  }



type State
  = Loading
  | Error Auth.Error
  | Ready



type alias Student =
  { id : Int
  , username  : String
  , firstName : String
  , lastName  : String
  }



init : (Model, Cmd Msg)
init =
  ( Model "" [] Loading Nothing
  , Task.attempt DefrostAuth Auth.defrost
  )



subscriptions : Model -> Sub Msg
subscriptions model = Auth.reauth Login



-- UPDATE



type Msg
  = DefrostAuth (Result LocalStorage.Error (Maybe Auth.Token))
  | Login (Maybe Auth.Token)
  | FetchNext
  | Logout
  | UpdateQuery String
  | Found String Int (Result Auth.Error (List Student))
  | Void



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateQuery query ->
      ( { model
        | query = query
        , results = []
        , state = Loading
        }
      , fetch Found model.token query 0
      )

    FetchNext ->
      ( {model | state = Loading}
      , fetch Found model.token model.query (List.length model.results)
      )

    Found query offset result ->
      if Loading == model.state &&
        query == model.query &&
        offset == (List.length model.results) then
        case result of
          Ok results ->
            ( { model
              | state = Ready
              , results = model.results ++ results
              }
            , Cmd.none
            )
          Err error ->
            ( { model
              | state = Error error
              }
            , Cmd.none
            )
      else
        (model, Cmd.none)

    DefrostAuth (Ok (Just t)) ->
      ( {model | token = (Just t)} -- Results already nothing.
      , fetch Found (Just t) "" 0
      )

    DefrostAuth _ ->
      ( model -- Results already nothing.
      , fetch Found model.token "" 0
      )

    Login t ->
      ( {model | token = t}
      , Cmd.none
      )

    Logout ->
      ( {model | token = Nothing}
      , Task.attempt (\_ -> Void) Auth.clear
      )

    _ -> (model, Cmd.none)



-- QUERY



serverQuery offset = """query ($q: String) {
  users(query: $q, offset: """ ++ (toString offset) ++ """, limit: """ ++ (toString pageLength) ++ """) {
    id,
    firstName,
    lastName,
    username
  }
}"""



fetch msg token query offset =
  D.map4 Student
    (D.field "id" D.int)
    (D.field "username" D.string)
    (D.field "firstName" D.string)
    (D.field "lastName" D.string)
  |>  D.list
  |>  D.at ["data", "users"]
  |>  Auth.graphql
        token
        "https://ttkkdd.herokuapp.com" --LONG: Move out into config.
        (serverQuery offset)
        [("q", E.string query), ("i", E.int offset)]
  |>  Task.attempt (msg query offset)



-- VIEW



view : Model -> Html Msg
view model =
    div []
      [ navbar Logout model.token "Admin"
      , section [class "content"]
          [ input
              [ type_ "text"
              , class "search"
              , value model.query
              , autofocus True
              , placeholder "Filter Users…"
              , onInput UpdateQuery
              ] []
          , ul [class "users"] (List.map showResult model.results)
          , case model.state of
              Loading ->
                Messages.loader
              Error err ->
                p [class "error toast"] [Messages.authError FetchNext err]
              Ready ->
                button [onClick FetchNext, class "more"] [text "More"]
          ]
      ]

showResult res =
  li []
    [ a [href ("user?id=" ++ (toString res.id)), class "user"]
        [ span [class "first-name"] [text res.firstName]
        , text " "
        , span [class "last-name"] [text res.lastName]
        , text " "
        , span [class "username"] [text res.username]
        ]
    ]
