module Auth exposing (..)

import Json.Encode as E
import Json.Decode as D
import Html exposing (..)
import Http
import LocalStorage
import Task
import Time



-- TYPES



type alias Token =
  { username: String
  , token : String
  , created : Time.Time
  }



type Error
  = HttpError Http.Error
  | ExpiredToken
  | Unauthorized
  | Forbidden



-- CONSTANTS



url = "https://ttkkdd.herokuapp.com/auth/login"
expiry = 43800 * 60 * 1000 -- one month



-- FUNCTIONS



login m username password =
  Task.attempt m (Task.map2
    (Token username)
    (Http.toTask
      (Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.stringBody "application/json" (
            E.encode 0 (E.object [
              ("username", E.string username),
              ("password", E.string password)]))
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }))
    Time.now)



username token = token.username

expired token now = token.created + expiry < now



get token url decoder =
  request "GET" token url Http.emptyBody decoder

post token url body decoder =
  request "POST" token url (Http.stringBody "application/json" body) decoder

graphql token url query variables decoder =
  post
    token url

    (E.encode 0 (E.object
      [ ("query", E.string query)
      , ("variables", E.object variables)
      ]))

    decoder

request method token url body decoder =
  Task.mapError (mapError token) <|
    Http.toTask <|
      Http.request
        { method = method
        , headers = case token of
            Just token ->
              [Http.header "Authorization" ("HELLO " ++ token.token)]
            Nothing -> []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }



mapError token error =
  case error of
    Http.BadStatus res ->
      case res.status.code of
        401 ->
          case token of
            Just _ -> ExpiredToken
            Nothing -> Unauthorized
        403 -> Forbidden
        _ -> HttpError (Http.BadStatus res)
    x -> HttpError x



toString t =
  E.encode 0 <| E.object
    [ ("username", E.string t.username)
    , ("token", E.string t.token)
    , ("created", E.float t.created)
    ]



tokenModel =
  D.map3 Token
    (D.at ["username"] D.string)
    (D.at ["token"] D.string)
    (D.at ["created"] D.float)



fromString s = D.decodeString tokenModel s



-- PERSISTENCE



--LONG: Use localForage or something.

reauth msg =
  LocalStorage.changes
    <| (\e -> msg <|
      if e.key == "auth" then tryParse e.newValue
      else Nothing)

beginWriting = -- Make sure the event's fired.
  LocalStorage.get "auth" |>
    Task.andThen (\m -> case m of
      Just _ -> Task.succeed ()
      Nothing -> LocalStorage.set "auth" "null")

clear =
  LocalStorage.set "auth" "null"



frost t =
  beginWriting |>
    Task.andThen (\_ -> LocalStorage.set "auth" (toString t))
defrost = LocalStorage.get "auth" |> Task.map (Maybe.andThen tryParse)



tryParse s = Result.toMaybe (fromString s)
