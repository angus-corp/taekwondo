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
  , results : Maybe (Result Http.Error (List Student))
  , token   : Maybe Auth.Token
  }



type alias Student =
  { id : Float
  , firstName : String
  , lastName  : String
  , username  : String
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
  | Logout
  | Void



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DefrostAuth (Ok (Just t)) ->
      ({model | token = Just t}, Cmd.none)

    Login t ->
      ({model | token = t}, Cmd.none)

    Logout ->
      ({model | token = Nothing}, Task.attempt (\_ -> Void) Auth.clear)

    _ -> (model, Cmd.none)



-- VIEW



view : Model -> Html Msg
view model =
    div []
      [ navbar Logout model.token "Admin"
      , section [class "content"] [text "TODO"]
      ]
