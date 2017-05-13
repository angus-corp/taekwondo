module Messages exposing (..)

import Auth
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http

--TODO: Better loader.
loader = p [] [text "Loading…"]

authError retryMsg err =
  case err of
      Auth.HttpError err ->
        case err of
          Http.BadUrl _ ->
            a [href "http://www.mspaintadventures.com/?s=6&p=003552"]
              [text "I'm sorry, but…"]
          Http.Timeout ->
            tryAgain retryMsg "The Internet took too long."
          Http.NetworkError ->
            tryAgain retryMsg "An unknown network error occurred."
          Http.BadPayload _ _ ->
            pleaseSignIn -- Assuming it's a GraphQL Error.
          Http.BadStatus _ ->
            tryAgain retryMsg "The server encountered an error."
      Auth.ExpiredToken ->
        pleaseSignInAgain
      Auth.Unauthorized ->
        pleaseSignIn
      Auth.Forbidden ->
        tryAgain retryMsg "You aren't privileged enough."

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

tryAgain msg info =
  span []
    [ text (info ++ " ")
    , a [onClick msg] [text "Try again?"]
    ]
