module Login exposing (..)

import Auth
import LocalStorage
import Task
import Html exposing (..)
import Http
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Window



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL



type State
  = Listening
  | Loading
  | Done



type alias Model =
  { state : State
  , username : String
  , password : String
  , error : Maybe String
  }



init : (Model, Cmd Msg)
init =
  ( Model Listening "" "" Nothing
  , Cmd.none
  )



-- UPDATE



type Msg
  = Username String
  | Password String
  | Submit
  | CompleteLogin (Result Http.Error Auth.Token)
  | Finish (Result LocalStorage.Error ())
  | Void



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Username username ->
      ({ model | username = username }, Cmd.none)
    Password password ->
      ({ model | password = password }, Cmd.none)
    Submit ->
      if model.state /= Loading then
        ({ model | state = Loading, error = Nothing }, login model)
      else
        (model, Cmd.none)
    CompleteLogin (Ok t) ->
      (model, Auth.frost t |> Task.attempt Finish)
    CompleteLogin (Err e) ->
      ( { model
        | state = Listening
        , error = Just (errorMessage e)
        }
      , Cmd.none)
    Finish _ ->
      ( {model | state = Done }
      , Window.close |> Task.attempt (\_ -> Void))
    Void -> ({model | state = Done}, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model = Sub.none



login model = Auth.login CompleteLogin model.username model.password



errorMessage e = case e of
  Http.BadUrl _ -> "Bad URL."
  Http.Timeout -> "The connection timed out."
  Http.NetworkError -> "Unknown network error."
  Http.BadStatus res ->
    case res.status.code of
      422 -> "Incorrect username or password."
      _ -> "Unknown error."
  Http.BadPayload _ _ -> "The server's acting up."



-- VIEW



view : Model -> Html Msg
view model =
  div [class "container"] <|
    if model.state == Done then
      [p [class "success toast"]
        [text "Success! You can close this window now."]]
    else
      case model.error of
        Just msg -> [error msg, loginForm model]
        Nothing -> [loginForm model]



loginForm model =
  Html.form [onSubmit Submit] <|
    List.reverse
      [ label [for "username"] [text "Email / Username"]
        , input
            [ type_ "text"
            , name "username"
            , required True
            , onInput Username
            , autofocus True
            , tabindex 1
            , value model.username
            ] []
      , label [for "password"] [text "Password"]
      , input
          [ type_ "password"
          , name "password"
          , required True
          , onInput Password
          , tabindex 2
          , value model.password
          ] []
      , loginButton model
      ]



loginButton model =
  button
    [ type_ "submit"
    , disabled (model.state == Loading)
    , tabindex -1
    ]
    [text (if model.state /= Loading then "Sign In" else "Loading…")]



error error =
  p [class "error toast"] [text error]
