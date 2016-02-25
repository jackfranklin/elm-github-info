module MyApp (..) where

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (disabled, value)
import String
import Http
import Json.Decode as JD exposing ((:=), Decoder)
import Json.Decode.Extra exposing ((|:))
import Graphics.Element exposing (..)
import Task exposing (Task, andThen, onError)
import Debug


type alias Model =
  { person : Person
  , input : String
  , error : String
  }


type Action
  = NewGithubPerson Person
  | GetGithubData
  | UpdateInput String
  | HttpError String
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


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


updateData : Person -> Task x ()
updateData person =
  Signal.send actions.address (NewGithubPerson person)


errorMessage : Http.Error -> String
errorMessage error =
  case error of
    Http.Timeout ->
      "Timeout"

    Http.NetworkError ->
      "Network error"

    Http.UnexpectedPayload _ ->
      "Error decoding JSON"

    Http.BadResponse code str ->
      "Error: " ++ (toString code) ++ " " ++ str


httpError : Http.Error -> Task () ()
httpError error =
  Signal.send actions.address (HttpError (errorMessage error))


getGithubData : String -> Task () ()
getGithubData name =
  (Http.get jsonToPerson ("https://api.github.com/users/" ++ name)) `andThen` updateData `onError` httpError


update : Action -> ( Model, Task () () ) -> ( Model, Task () () )
update action ( model, _ ) =
  case action of
    NoOp ->
      ( model, Task.succeed () )

    HttpError str ->
      ( { model | error = str }, Task.succeed () )

    UpdateInput string ->
      ( { model | input = string }, Task.succeed () )

    GetGithubData ->
      ( model, getGithubData model.input )

    NewGithubPerson person ->
      ( { model | person = person }, Task.succeed () )


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



-- SIGNALS AND PORTS


modelAndTask : Signal ( Model, Task () () )
modelAndTask =
  Signal.foldp update ( initialModel, getGithubData initialModel.input ) actions.signal


taskSignal : Signal (Task () ())
taskSignal =
  Signal.map snd modelAndTask


modelSignal : Signal Model
modelSignal =
  Signal.map fst modelAndTask


port tasks : Signal (Task () ())
port tasks =
  taskSignal


main : Signal Html
main =
  Signal.map (view actions.address) modelSignal
