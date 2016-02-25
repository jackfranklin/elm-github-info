module MyApp (..) where

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Never, Effects)
import Html.Attributes exposing (disabled, value)
import String
import Http
import Json.Decode as JD exposing ((:=), Decoder)
import Json.Decode.Extra exposing ((|:))
import Graphics.Element exposing (..)
import Task exposing (Task, andThen, onError)
import Debug
import StartApp


type alias Model =
  { person : Person
  , input : String
  , error : String
  }


type Action
  = NewGithubPerson (Maybe Person)
  | GetGithubData
  | UpdateInput String


initialModel : Model
initialModel =
  { person = nullPerson
  , input = "jackfranklin"
  , error = ""
  }


type alias Person =
  { login : String, reposCount : Int }


nullPerson : Person
nullPerson =
  { login = "", reposCount = 0 }


getGithubData : String -> Effects Action
getGithubData name =
  Http.get jsonToPerson ("https://api.github.com/users/" ++ name)
    |> Task.toMaybe
    |> Task.map NewGithubPerson
    |> Effects.task


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdateInput string ->
      ( { model | input = string }, Effects.none )

    GetGithubData ->
      ( model, getGithubData model.input )

    NewGithubPerson maybePerson ->
      case maybePerson of
        Just person ->
          ( { model | person = person, error = "" }, Effects.none )

        Nothing ->
          ( { model | error = "something went wrong getting data" }, Effects.none )


jsonToPerson : Decoder Person
jsonToPerson =
  JD.succeed Person
    |: ("login" := JD.string)
    |: ("public_repos" := JD.int)


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ button [ onClick address GetGithubData, disabled (String.isEmpty model.input) ] [ text "Go!" ]
    , input [ on "input" targetValue (\str -> Signal.message address (UpdateInput str)), value model.input ] []
    , div [] [ text model.person.login ]
    , div [] [ text ("Repo Count: " ++ (toString model.person.reposCount)) ]
    , div [] [ text model.error ]
    ]


init : ( Model, Effects Action )
init =
  ( initialModel, getGithubData initialModel.input )


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


main =
  app.html
