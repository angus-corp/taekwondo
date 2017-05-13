module Admin exposing (..)

--TODO: Automatic infinite scrolling on scroll-to-bottom.
--TODO: STYLING.
--TODO: DEBOUNCE.

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
              , value model.query
              , placeholder "Filter Users…"
              , onInput UpdateQuery
              ] []
          , showResults model.results model.state
          ]
      ]

showResults results state =
  div [] <|
    [ ul [] (List.map showResult results)
    , case state of
        Loading -> Loader.loader
        Error err -> p [class "error toast"] [errorText err]
        Ready -> button [onClick FetchNext] [text "More"]
    ]

showResult res =
  li []
    [ a [href ("user?id=" ++ (toString res.id))]
        [ span [] [text res.username]
        , span [] [text res.firstName]
        , span [] [text res.lastName]]
    ]

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
    , a [onClick FetchNext] [text "Try again?"]
    ]
