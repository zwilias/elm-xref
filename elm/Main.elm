port module Main exposing (main, toElm, toJS)

import Dict exposing (Dict)
import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Base as Syntax
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Import)
import Elm.Syntax.Ranged exposing (Ranged)
import Json.Encode as Json
import Set exposing (Set)


type alias Model =
    { todo :
        List
            { blockedBy : Set Syntax.ModuleName
            , data : Module
            }
    , packageData : Dict Syntax.ModuleName InterfaceData
    , done : Dict Syntax.ModuleName Module
    }


type alias InFile =
    { package : Maybe PackageIdentifier
    , fileName : String
    , content : String
    }


type alias PackageIdentifier =
    { name : String
    , version : String
    }


type Msg
    = Parse InFile
    | Send


init : ( Model, Cmd msg )
init =
    ( { todo = []
      , packageData = Dict.empty
      , done = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse file ->
            case parse file of
                Ok mod ->
                    ( add mod model
                    , toJS <| Json.string <| "parsed " ++ file.fileName
                    )

                Err e ->
                    ( model
                    , toJS <| Json.string <| file.fileName ++ ": " ++ toString e
                    )

        Send ->
            ( model, toJS <| todoToJson model.todo )


todoToJson : List { blockedBy : Set Syntax.ModuleName, data : Module } -> Json.Value
todoToJson todo =
    List.map
        (\{ blockedBy, data } ->
            Json.object
                [ ( "module"
                  , Json.string <| String.join "." data.name
                  )
                , ( "blockedBy"
                  , blockedBy
                        |> Set.toList
                        |> List.map (String.join "." >> Json.string)
                        |> Json.list
                  )
                ]
        )
        todo
        |> Json.list


type alias Errors =
    List String


add : InModule -> Model -> Model
add mod model =
    case mod of
        Package package ->
            { model | packageData = Dict.insert package.name package model.packageData }
                |> checkTodo (Set.singleton package.name)

        Own modul ->
            { model | todo = todoItem modul model :: model.todo }
                |> checkTodo Set.empty


todoItem : Module -> Model -> { blockedBy : Set Syntax.ModuleName, data : Module }
todoItem modul model =
    { blockedBy = requiredModules modul model
    , data = modul
    }


checkTodo : Set Syntax.ModuleName -> Model -> Model
checkTodo nowDone model =
    let
        ( newlyDone, newTodo ) =
            List.foldl
                (\{ blockedBy, data } ( done, todo ) ->
                    let
                        newBlockers =
                            Set.diff blockedBy nowDone
                    in
                    if Set.isEmpty newBlockers then
                        ( finalize model data :: done, todo )
                    else
                        ( done, { blockedBy = newBlockers, data = data } :: todo )
                )
                ( [], [] )
                model.todo

        newDone =
            List.map (\m -> ( m.name, m )) newlyDone |> Dict.fromList

        newModel =
            { model | todo = newTodo, done = Dict.union model.done newDone }
    in
    case newlyDone of
        [] ->
            newModel

        _ ->
            checkTodo (Dict.keys newDone |> Set.fromList) newModel


finalize : Model -> Module -> Module
finalize _ m =
    -- TODO: Gather declarations and uses
    m


requiredModules : Module -> Model -> Set Syntax.ModuleName
requiredModules modul model =
    let
        needed =
            List.map .moduleName modul.imports
                |> List.filter (not << isNative)
                |> Set.fromList
    in
    Set.diff needed (availableModules model)


isNative : Syntax.ModuleName -> Bool
isNative =
    List.head >> Maybe.map ((==) "Native") >> Maybe.withDefault False


availableModules : Model -> Set Syntax.ModuleName
availableModules model =
    List.concat
        [ Dict.keys model.packageData
        , Dict.keys model.done
        ]
        |> Set.fromList


parse : InFile -> Result Errors InModule
parse file =
    Parser.parse file.content
        |> Result.map (toModule file.package file.fileName)


toModule : Maybe PackageIdentifier -> String -> RawFile -> InModule
toModule package fileName rawFile =
    case package of
        Just packageInfo ->
            toPackageModule packageInfo fileName rawFile

        Nothing ->
            toInternalModule fileName rawFile


toPackageModule : PackageIdentifier -> String -> RawFile -> InModule
toPackageModule package fileName rawFile =
    Package
        { name = RawFile.moduleName rawFile |> Maybe.withDefault []
        , fileName = fileName
        , interface = Interface.build rawFile
        , package = package
        }


toInternalModule : String -> RawFile -> InModule
toInternalModule fileName rawFile =
    let
        file =
            Processing.process Processing.init rawFile
    in
    Own
        { name = RawFile.moduleName rawFile |> Maybe.withDefault []
        , fileName = fileName
        , interface = Interface.build rawFile
        , imports = file.imports
        , declarations = file.declarations
        }


type InModule
    = Package InterfaceData
    | Own Module


type alias InterfaceData =
    { name : Syntax.ModuleName
    , fileName : String
    , interface : Interface
    , package : PackageIdentifier
    }


type alias Module =
    { name : Syntax.ModuleName
    , fileName : String
    , interface : Interface
    , imports : List Import
    , declarations : List (Ranged Declaration)
    }


port toElm : (InFile -> msg) -> Sub msg


port toJS : Json.Value -> Cmd msg


port fetch : (() -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ toElm Parse
        , fetch (always Send)
        ]


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = always subscriptions
        }
