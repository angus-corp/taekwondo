module Roll exposing (..)

import Auth
import Dict exposing (Dict)
import Date exposing (Date)
import Json.Encode as E
import Json.Decode as D
import Navbar exposing (navbar)
import Messages
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



type Model
  = Ready
    { token: Maybe Auth.Token
    , meta: Meta
    , roll: RollModel
    , date: Date
    , class: Class
    }
  | Loading
    { token: Maybe Auth.Token
    }
  | Error
    { token: Maybe Auth.Token
    , error: Auth.Error
    }
  | Empty
    { token: Maybe Auth.Token
    }

token model =
  case model of
    Ready x -> x.token
    Loading x -> x.token
    Error x -> x.token
    Empty x -> x.token

setToken token model =
  case model of
    Ready x -> Ready { x | token = token }
    Loading x -> Loading { x | token = token }
    Error x -> Error { x | token = token }
    Empty x -> Empty { x | token = token }



type RollModel
  = Waiting
  | Bad LocalStorage.Error
  | Good Roll



init : (Model, Cmd Msg)
init =
  ( Loading { token = Nothing }
  , Task.attempt DefrostMeta <| tackOnTheDate getMeta
  )



tackOnTheDate task =
  task |> Task.andThen (\res -> Task.map ((,) res) Date.now)



type alias Roll = Dict ClassID (Dict UserID Attendance)
type alias ClassID = Int
type alias UserID = Int
type Attendance
  = Present
  | Absent
  | Late
  | Unknown

type alias StudentNames =
  { firstName : String
  , lastName : String
  , username : String
  }

type alias Meta =
  { students : Dict UserID StudentNames
  , classes : List Class
  }

type alias Class =
  { id : ClassID
  , name : String
  , students : List UserID
  }



-- PERSISTENCE



getRoll : Date -> Task LocalStorage.Error Roll
getRoll date =
  let
    key = "roll-" ++ (dateString date)
    attendify n =
      case n of
        1 -> Present
        2 -> Absent
        3 -> Late
        _ -> Unknown
    decodeRoll =
      dictFromList D.int (dictFromList D.int <| D.map attendify D.int)
  in
    LocalStorage.get key
    |> Task.map
      ( ( D.decodeString decodeRoll
          >> Result.toMaybe
          |> Maybe.andThen
        ) >> Maybe.withDefault Dict.empty
      )

setRoll : Date -> Roll -> Task LocalStorage.Error ()
setRoll date roll =
  let
    key = "roll-" ++ (dateString date)
    serializeAttendance att =
      case att of
        Unknown -> 0
        Present -> 1
        Absent -> 2
        Late -> 3
    encodeClass (id, students) =
      E.list
        [ E.int id
        , students
          |> Dict.toList
          |> List.filter (\(a, b) -> b /= Unknown)
          |> List.map
            (\(a, b) -> E.list [E.int a, b |> serializeAttendance |> E.int])
          |> E.list
        ]
  in
    roll
    |> Dict.toList
    |> List.map encodeClass
    |> E.list
    |> E.encode 0
    |> LocalStorage.set key

getMeta : Task LocalStorage.Error (Maybe Meta)
getMeta =
  let
    decodeMeta =
      D.map2 Meta
        ( D.field "students" <|
            D.map Dict.fromList <| D.list <| D.map2 (,)
              (D.index 0 D.int)
              (D.index 1 <| D.map3 StudentNames
                (D.index 0 D.string)
                (D.index 1 D.string)
                (D.index 2 D.string))
        )
        ( D.field "classes" <|
            D.list <| D.map3
              (\x y z -> { id = x, name = y, students = z })
              (D.field "id" D.int)
              (D.field "name" D.string)
              (D.field "students" (D.list D.int))
        )
  in
    LocalStorage.get "roll-meta"
    |> Task.map
      ( D.decodeString decodeMeta
        >> Result.toMaybe
        |> Maybe.andThen
      )

setMeta : Meta -> Task LocalStorage.Error ()
setMeta meta =
  let
    encodeStudent (id, stu) =
      E.list
        [ E.int id
        , E.list
            [ E.string stu.firstName
            , E.string stu.lastName
            , E.string stu.username
            ]
        ]
    encodeClass class =
      E.object
        [ ("id", E.int class.id)
        , ("name", E.string class.name)
        , ("students", class.students |> List.map E.int |> E.list)
        ]
  in
    E.object
      [ ( "students"
        , meta.students
          |> Dict.toList
          |> List.map encodeStudent
          |> E.list
        )
      , ("classes", meta.classes |> List.map encodeClass |> E.list)
      ]
      |> E.encode 0
      |> LocalStorage.set "roll-meta"



dictFromList a b =
  D.map Dict.fromList (D.list (D.map2 (,) (D.index 0 a) (D.index 1 b)))



-- SERVER



type alias Location =
  { id : Int
  , name : String
  , students : List Student
  }
type alias Student =
  { id : Int
  , firstName : String
  , lastName : String
  , username : String
  }



fetch msg token =
  Task.attempt msg <|
    tackOnTheDate <|
      Auth.graphql
        token
        "https://ttkkdd.herokuapp.com" --LONG: Move out into config.
        query
        []
        responseModel

responseModel =
  D.list locationModel
  |> D.at ["data", "user", "instructingLocations"]
  |> D.map metafy

locationModel =
  D.map3 Location
    (D.field "id" D.int)
    (D.field "name" D.string)
    (D.field "students" (D.list studentModel))

studentModel =
  D.map4 Student
    (D.field "id" D.int)
    (D.field "firstName" D.string)
    (D.field "lastName" D.string)
    (D.field "username" D.string)

query = """{
  user {
    instructingLocations {
      id,
      name,
      students {
        id,
        firstName,
        lastName,
        username
      }
    }
  }
}"""

metafy locs =
  { students =
      locs
      |> List.map
        (\loc -> List.map
          (\stu ->
            ( stu.id
            , StudentNames stu.firstName stu.lastName stu.username
            )
          )
          loc.students
        )
      |> List.map Dict.fromList
      |> List.foldl Dict.union Dict.empty
  , classes =
      List.map
        (\loc ->
          { id = loc.id
          , name = loc.name
          , students = List.map (\x -> x.id) loc.students
          }
        )
        locs
  }



-- UPDATE



type Msg
  = DefrostAuth (Result LocalStorage.Error (Maybe Auth.Token))
  | DefrostMeta (Result LocalStorage.Error ((Maybe Meta), Date))
  | Login (Maybe Auth.Token)
  | FetchMeta
  | FetchedMeta (Result Auth.Error (Meta, Date))
  | SetDate Date
  | SetClass Class
  | Mark Attendance UserID
  | FetchRoll
  | FetchedRoll (Result LocalStorage.Error Roll)
  | Void
  | Logout



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DefrostAuth (Ok (Just t)) ->
      updateAndRefresh model (Just t)

    DefrostAuth _ ->
      (model, Cmd.none)



    DefrostMeta (Ok (Just t, today)) ->
      case List.head t.classes of
        Just a ->
          ( Ready
              { token = token model
              , meta = t
              , roll = Waiting
              , date = today
              , class = a
              }
          , Cmd.batch
              [ getRoll today |> Task.attempt FetchedRoll
              , defrostAuth
              ]
          )
        Nothing ->
          (Empty {token = token model}, defrostAuth)

    DefrostMeta e ->
      (Empty { token = token model }, defrostAuth)



    FetchRoll ->
      case model of
        Ready x ->
          ( Ready {x | roll = Waiting}
          , getRoll x.date |> Task.attempt FetchedRoll
          )
        _ ->
          (model, Cmd.none)



    Mark val userid ->
      case model of
        Ready model ->
          case model.roll of
            Good roll ->
              let
                new =
                  Dict.update
                    model.class.id
                    ( Maybe.withDefault Dict.empty
                      >> Dict.insert userid val
                      >> Just
                    )
                    roll
              in
                ( Ready {model | roll = Good new}
                , Task.attempt (\_ -> Void) (setRoll model.date new))
            _ ->
              (Ready model, Cmd.none)
        _ ->
          (model, Cmd.none)



    SetDate date ->
      case model of
        Ready x ->
          ( Ready {x | roll = Waiting, date = date}
          , getRoll date |> Task.attempt FetchedRoll
          )
        _ ->
          (model, Cmd.none)



    SetClass class ->
      case model of
        Ready x ->
          (Ready {x | class = class}, Cmd.none)
        _ ->
          (model, Cmd.none)



    FetchMeta ->
      case model of
        Loading _ ->
          (model, Cmd.none)
        _ ->
          let
            t = token model
          in
            ( Loading { token = t }
            , fetch FetchedMeta t
            )



    Login token ->
      updateAndRefresh model token



    FetchedMeta (Ok (t, today)) ->
      case List.head t.classes of
        Just a ->
          ( Ready
              { token = token model
              , meta = t
              , roll = Waiting
              , date = today
              , class = a
              }
          , Cmd.batch
              [ getRoll today |> Task.attempt FetchedRoll
              , setMeta t |> Task.attempt (\x -> Void)
              ]
          )
        Nothing ->
          (Empty {token = token model}, Cmd.none)

    FetchedMeta (Err err) ->
      ( Error
          { token = token model
          , error = err
          }
      , Cmd.none)



    FetchedRoll (Ok roll) ->
      case model of
        Ready model ->
          (Ready { model | roll = Good roll }, Cmd.none)
        _ ->
          (model, Cmd.none)

    FetchedRoll (Err err) ->
      case model of
        Ready model ->
          (Ready { model | roll = Bad err }, Cmd.none)
        _ ->
          (model, Cmd.none)



    Logout ->
      (setToken Nothing model, Task.attempt (\_ -> Void) Auth.clear)



    Void -> (model, Cmd.none)



defrostAuth = Task.attempt DefrostAuth Auth.defrost



updateAndRefresh : Model -> Maybe Auth.Token -> (Model, Cmd Msg)
updateAndRefresh model t =
  let
    refresh = (Loading { token = t }, fetch FetchedMeta t)
  in
    case model of
      Error _ -> refresh
      Empty _ -> refresh
      _ -> (setToken t model, Cmd.none)
      


subscriptions : Model -> Sub Msg
subscriptions model = Auth.reauth Login



-- VIEW



view : Model -> Html Msg
view model =
    div []
      [ navbar Logout (token model) "Roll"
      , section [class "content"] [content model]
      ]

content model =
  (
    case model of
      Loading model -> [Messages.loader]
      Empty model ->
        [ p [class "error toast"] <|
            if model.token == Nothing then
              [Messages.pleaseSignIn]
            else
              [Messages.tryAgain FetchMeta "You have no classes to track!"]
        ]
      Error {error} ->
        [ p [class "error toast"]
            [Messages.authError FetchMeta error]
        ]
      Ready model ->
        [ div [class "input-group"]
            [ button [onClick FetchMeta] [text "⟳"]
            , classSelect model
            , datePicker model
            ]
        , case model.roll of
            Good roll ->
              rollTable model roll
            Waiting ->
              Messages.loader
            Bad error ->
              p [class "error toast"]
                [Messages.tryAgain FetchRoll "There was a storage error."]
        ]
  ) |> div [class "container"]



datePicker model =
  input
    [ type_ "date"
    , value (dateString model.date)
    , required True
    , placeholder "YYYY-MM-DD"
    , onInput (\x -> case (Date.fromString x) of
        Ok date -> SetDate date
        Err _ -> Void)
    ]
    []



classSelect model =
  model.meta.classes
  |> List.map
      (\c ->
        option
          [ value (toString c.id)
          , selected (model.class.id == c.id)
          ]
          [text c.name]
      )
  |> select
      [ onInput
        (\x ->
          let
            class =
              find (\c -> toString c.id == x) model.meta.classes
          in
            case class of
              Just class -> SetClass class
              Nothing -> Void
        )
      ]



rollTable model roll =
  model.class.students
  |> List.map
      (\id ->
        let
          active = Dict.get model.class.id roll
          |> Maybe.andThen (Dict.get id)
          |> Maybe.withDefault Unknown
        in
          tr []
            [ td []
                [ a [href ("user?id=" ++ toString id), class "name"]
                    [text (nameOf model id)]
                ]
            , rollCell id active Present "present"
            , rollCell id active Absent "absent"
            , rollCell id active Late "late"
            ]
      )
  |> table [class "att"]



rollCell id active kind name =
  td
    [ onClick <|
        if active == kind then
          Mark Unknown id
        else
          Mark kind id
    , class <|
        if active == kind then
          name ++ " active button"
        else
          name ++ " button"
    ]
    []



-- HELPERS



find f list =
  List.foldr (\x acc -> if (f x) then (Just x) else acc) Nothing list

nameOf model student =
  Dict.get student model.meta.students
  |> Maybe.map (\s -> s.firstName ++ " " ++ s.lastName)
  |> Maybe.withDefault "???"

dateString date =
  (toString (Date.year date)) ++ "-" ++
  (monthToString (Date.month date)) ++ "-" ++
  (String.padLeft 2 '0' (toString (Date.day date)))

monthToString month =
  case month of
    Date.Jan -> "01"
    Date.Feb -> "02"
    Date.Mar -> "03"
    Date.Apr -> "04"
    Date.May -> "05"
    Date.Jun -> "06"
    Date.Jul -> "07"
    Date.Aug -> "08"
    Date.Sep -> "09"
    Date.Oct -> "10"
    Date.Nov -> "11"
    Date.Dec -> "12"
