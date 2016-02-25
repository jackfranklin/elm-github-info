module MyApp (..) where

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (disabled, value)
import String
import Http
import Json.Decode as JD exposing ((:=), Decoder)
import Json.Decode.Extra exposing ((|:))
import Graphics.Element exposing (..)
import Task exposing (Task, andThen)


type alias Model =
  { person : Person
  , input : String
  }


type Action
  = NewGithubPerson Person
  | GetGithubData
  | UpdateInput String
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


initialModel : Model
initialModel =
  { person = nullPerson
  , input = "jackfranklin"
  }


type alias Person =
  { login : String, reposCount : Int }


nullPerson : Person
nullPerson =
  { login = "", reposCount = 0 }


updateData : Person -> Task x ()
updateData person =
  Signal.send actions.address (NewGithubPerson person)


getGithubData : String -> Task () ()
getGithubData name =
  (silenceTask (Http.get jsonToPerson ("https://api.github.com/users/" ++ name))) `andThen` updateData


update : Action -> ( Model, Task () () ) -> ( Model, Task () () )
update action ( model, _ ) =
  case action of
    NoOp ->
      ( model, Task.succeed () )

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
    ]



-- SIGNALS AND PORTS


modelAndTask : Signal ( Model, Task () () )
modelAndTask =
  Signal.foldp update ( initialModel, getGithubData initialModel.input ) actions.signal


taskSignal : Signal (Task () ())
taskSignal =
  Signal.map snd modelAndTask


silenceTask : Task x a -> Task () a
silenceTask task =
  task
    |> Task.mapError (\_ -> ())


modelSignal : Signal Model
modelSignal =
  Signal.map fst modelAndTask


port tasks : Signal (Task () ())
port tasks =
  taskSignal


main : Signal Html
main =
  Signal.map (view actions.address) modelSignal



-- main : Element
-- main =
--   myJson
--     |> JD.decodeString jsonToPerson
--     |> Result.toMaybe
--     |> Maybe.withDefault nullPerson
--     |> .name
--     |> show
