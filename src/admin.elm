module Admin exposing (..)

import Auth
import Dict exposing (Dict)
import Date exposing (Date)
import Json.Encode as E
import Json.Decode as D
import Navbar exposing (navbar)
import Loader
import LocalStorage
import Task exposing (Task)
import Html exposing (..)
import Http
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onClick)



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
  , results : Maybe (Result Auth.Error (List Student))
  , token   : Maybe Auth.Token
  }



type alias Student =
  { id : Float
  , username  : String
  , firstName : String
  , lastName  : String
  }



init : (Model, Cmd Msg)
init =
  ( Model "" (Just (Ok [])) Nothing
  , Task.attempt DefrostAuth Auth.defrost
  )



subscriptions : Model -> Sub Msg
subscriptions model = Auth.reauth Login



-- UPDATE



type Msg
  = DefrostAuth (Result LocalStorage.Error (Maybe Auth.Token))
  | Login (Maybe Auth.Token)
  | Retry
  | Logout
  | UpdateQuery String
  | UpdateResults (Result Auth.Error (List Student))
  | Void



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateQuery query ->
      ( { model
        | query = query
        , results = Nothing
        }
      , fetch UpdateResults model.token query
      )

    UpdateResults res ->
      ({model | results = Just res}, Cmd.none)

    Retry ->
      ({model | results = Nothing}, fetch UpdateResults model.token model.query)

    DefrostAuth (Ok (Just t)) ->
      ({model | token = Just t}, Cmd.none)

    Login t ->
      ({model | token = t}, Cmd.none)

    Logout ->
      ({model | token = Nothing}, Task.attempt (\_ -> Void) Auth.clear)

    _ -> (model, Cmd.none)



-- QUERY



serverQuery = """query ($q: String) {
  users(query: $q) {
    id,
    firstName,
    lastName,
    username
  }
}"""



fetch msg token query =
  D.map4 Student
    (D.field "id" D.float)
    (D.field "username" D.string)
    (D.field "firstName" D.string)
    (D.field "lastName" D.string)
  |>  D.list
  |>  D.at ["data", "users"]
  |>  Auth.graphql
        token
        "https://ttkkdd.herokuapp.com" --LONG: Move out into config.
        serverQuery
        [("q", E.string query)]
  |>  Task.attempt msg

-- VIEW



view : Model -> Html Msg
view model =
    div []
      [ navbar Logout model.token "Admin"
      , section [class "content"]
          [ input
              [ type_ "text"
              , value model.query
              , placeholder "User Query…"
              , onInput UpdateQuery
              ] []
          , showResults model.results
          ]
      ]

showResults results =
  case results of
    Nothing ->
      Loader.loader

    Just (Err err) ->
      p [class "error toast"] [errorText err]

    Just (Ok results) ->
      ul []
        (List.map showResult results)

showResult res = --TODO: This, onclick and separate fields.
  li [] [text res.username, text res.firstName, text res.lastName]

--TODO: Abstract. These appear in several pages.
errorText err =
  case err of
      Auth.HttpError err ->
        case err of
          Http.BadUrl _ -> --TODO: Move to config.
            a [href "http://www.mspaintadventures.com/?s=6&p=003552"]
              [text "I'm sorry, but…"]
          Http.Timeout ->
            tryAgain "The Internet took too long."
          Http.NetworkError ->
            tryAgain "An unknown network error occurred."
          Http.BadPayload _ _ ->
            pleaseSignIn -- GraphQL Error.
          Http.BadStatus _ ->
            tryAgain "The server encountered an error."
      Auth.ExpiredToken ->
        pleaseSignInAgain
      Auth.Unauthorized ->
        pleaseSignIn
      Auth.Forbidden ->
        tryAgain "You aren't privileged enough."

pleaseSignInAgain =
  span []
    [ text "Please "
    , a [href "login", target "login"] [text "sign in again"]
    , text "."
    ]

pleaseSignIn =
  span []
    [ text "Please "
    , a [href "login", target "login"] [text "sign in"]
    , text "."
    ]

tryAgain msg =
  span []
    [ text (msg ++ " ")
    , a [href "#", onClick Retry] [text "Try again?"]
    ]
