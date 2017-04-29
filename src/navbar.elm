module Navbar exposing (navbar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



--LONG: Fill in links.
navbar logout token page =
  nav []
    [ h1 [] [text "TTKKDD"]
    , ul []
        [ navBtn page "Match" "match"
        , navBtn page "Roll" "roll"
        , navBtn page "Payments" "payments"
        , navBtn page "Admin" "admin"
        ]
    , div [class "divider"] []
    , loginBtn logout token
    ]



loginBtn logout token =
  case token of
    Just token ->
      div [class "dropdown"]
        [ button
            [ class "button dropdown-button"
            --LONG: Horrible hack for Safari, should consider alternatives.
            , attribute "onclick" "this.focus()"
            ]
            [text token.username]
        , div [class "dropdown-menu"]
            [ a [href "profile"]
                [text "Profile"]
            , a [href "login", target "login"]
                [text "Switch User"]
            , a [href "#", onClick logout]
                [text "Sign Out"]
            ]
        ]
    Nothing ->
      a [class "button", href "login", target "login"]
        [text "Sign In"]



navBtn page name addr =
  let c =
    if page == name then
      "active nav-link"
    else
      "nav-link"
  in
    li [class c] [a [href addr] [text name]]
