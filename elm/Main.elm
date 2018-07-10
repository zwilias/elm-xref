port module Main exposing (main, toElm, toJS)

import CallGraph exposing (CallGraph, Caller, Function, Usage)
import Dict exposing (Dict)
import Elm.Interface as Interface exposing (Interface)
import Elm.Json.Decode as RawFile
import Elm.Json.Encode as RawFile
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Base as Syntax
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Import)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)
import Gather
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)
import Util


type alias InFile =
    { package : Maybe PackageIdentifier
    , fileName : String
    , content : String
    }


type alias CachedFile =
    { package : Maybe PackageIdentifier
    , fileName : String
    , data : Encode.Value
    }


type alias PackageIdentifier =
    { name : String
    , version : String
    }


type Module
    = Package PackageData
    | Own ModuleData


type alias PackageData =
    { name : Syntax.ModuleName
    , fileName : String
    , interface : Interface
    , package : PackageIdentifier
    }


type alias ModuleData =
    { name : Syntax.ModuleName
    , fileName : String
    , interface : Interface
    , imports : List Import
    , declarations : List (Ranged Declaration)
    }


findUnused : Model -> Set Function
findUnused model =
    let
        usedFunctions : Set Function
        usedFunctions =
            Set.foldl (CallGraph.walkGraph model.callGraph)
                Set.empty
                model.entryPoints

        allOwnFunctions : Set Function
        allOwnFunctions =
            Dict.foldl findOwnFunctions Set.empty model.done
    in
    Set.diff allOwnFunctions usedFunctions


findOwnFunctions : Syntax.ModuleName -> Module -> Set Function -> Set Function
findOwnFunctions name modul acc =
    case modul of
        Package _ ->
            acc

        Own m ->
            List.foldl
                (\f fs ->
                    Set.insert ( name, f ) fs
                )
                acc
                (List.concatMap (Ranged.value >> Util.resolve) m.declarations)


type alias Errors =
    List String


add : Module -> Model -> Model
add mod model =
    case mod of
        Package package ->
            { model | done = Dict.insert package.name mod model.done }
                |> checkTodo (Set.singleton package.name)

        Own modul ->
            { model | todo = todoItem modul model :: model.todo }
                |> findEntryPoints modul
                |> checkTodo Set.empty


todoItem : ModuleData -> Model -> { blockedBy : Set Syntax.ModuleName, data : ModuleData }
todoItem modul model =
    { blockedBy = requiredModules modul model
    , data = modul
    }


findEntryPoints : ModuleData -> Model -> Model
findEntryPoints modul model =
    case Interface.exposesFunction "main" modul.interface of
        True ->
            { model
                | entryPoints =
                    Set.insert
                        ( modul.name, "main" )
                        model.entryPoints
            }

        False ->
            model


checkTodo : Set Syntax.ModuleName -> Model -> Model
checkTodo nowDone model =
    let
        ( newlyDone, newTodo, usages ) =
            List.foldl
                (\{ blockedBy, data } ( done, todo, usages ) ->
                    let
                        newBlockers =
                            Set.diff blockedBy nowDone
                    in
                    if Set.isEmpty newBlockers then
                        ( data :: done
                        , todo
                        , finalize model data :: usages
                        )
                    else
                        ( done
                        , { blockedBy = newBlockers, data = data } :: todo
                        , usages
                        )
                )
                ( [], [], [] )
                model.todo

        newDone =
            List.map (\m -> ( m.name, Own m )) newlyDone |> Dict.fromList

        newModel =
            { model | todo = newTodo, done = Dict.union model.done newDone }
    in
    case newlyDone of
        [] ->
            newModel

        _ ->
            List.foldl updateCallGraph newModel usages
                |> checkTodo (Dict.keys newDone |> Set.fromList)


updateCallGraph : List Usage -> Model -> Model
updateCallGraph usages model =
    { model | callGraph = CallGraph.addCalls usages model.callGraph }


finalize : Model -> ModuleData -> List Usage
finalize m modul =
    let
        importedDecls : List Gather.DeclDict
        importedDecls =
            List.map (declImportDict m.done) modul.imports

        scope : List Gather.DeclDict
        scope =
            importedDecls ++ Gather.defaultImports
    in
    Gather.calls modul.name scope modul.declarations


interface : Module -> Interface
interface mod =
    case mod of
        Package p ->
            p.interface

        Own m ->
            m.interface


declImportDict : Dict Syntax.ModuleName Module -> Module.Import -> Gather.DeclDict
declImportDict modules imports =
    let
        iface : Interface
        iface =
            Dict.get imports.moduleName modules
                |> Maybe.map interface
                |> Maybe.withDefault []

        allExports : List String
        allExports =
            resolveInterface iface
    in
    case imports.exposingList of
        Nothing ->
            { unqualified = []
            , qualified = allExports
            , qualifier =
                imports.moduleAlias
                    |> Maybe.withDefault imports.moduleName
            , name = imports.moduleName
            }

        Just (Exposing.All _) ->
            { unqualified = allExports
            , qualified = allExports
            , qualifier =
                imports.moduleAlias
                    |> Maybe.withDefault imports.moduleName
            , name = imports.moduleName
            }

        Just (Exposing.Explicit list) ->
            { unqualified =
                List.concatMap
                    (Ranged.value >> resolveImport iface)
                    list
            , qualified = allExports
            , qualifier =
                imports.moduleAlias
                    |> Maybe.withDefault imports.moduleName
            , name = imports.moduleName
            }


resolveImport :
    Interface
    -> Exposing.TopLevelExpose
    -> List String
resolveImport iface expose =
    case expose of
        Exposing.InfixExpose i ->
            [ i ]

        Exposing.FunctionExpose f ->
            [ f ]

        Exposing.TypeExpose t ->
            case t.constructors of
                Nothing ->
                    []

                Just (Exposing.All _) ->
                    resolveConstructors iface t.name

                Just (Exposing.Explicit ctors) ->
                    List.map Ranged.value ctors

        _ ->
            []


resolveConstructors : Interface -> String -> List String
resolveConstructors iface t =
    case iface of
        [] ->
            []

        (Interface.Type ( name, ctors )) :: rest ->
            if name == t then
                ctors
            else
                resolveConstructors rest t

        _ :: rest ->
            resolveConstructors rest t


resolveInterface : Interface -> List String
resolveInterface =
    List.concatMap resolveExposed


resolveExposed : Interface.Exposed -> List String
resolveExposed exp =
    case exp of
        Interface.Function s ->
            [ s ]

        Interface.Operator i ->
            [ i.operator ]

        Interface.Type ( _, ctors ) ->
            ctors

        _ ->
            []


requiredModules : ModuleData -> Model -> Set Syntax.ModuleName
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
    Dict.keys model.done
        |> Set.fromList


parse : InFile -> Result Errors ( Module, Encode.Value )
parse file =
    Parser.parse file.content
        |> Result.map
            (\rawFile ->
                ( toModule file.package file.fileName rawFile
                , RawFile.encode rawFile
                )
            )


parseCached : CachedFile -> Result String Module
parseCached cached =
    Decode.decodeValue RawFile.decode cached.data
        |> Result.map (toModule cached.package cached.fileName)


toModule : Maybe PackageIdentifier -> String -> RawFile -> Module
toModule package fileName rawFile =
    case package of
        Just packageInfo ->
            toPackageModule packageInfo fileName rawFile

        Nothing ->
            toInternalModule fileName rawFile


toPackageModule : PackageIdentifier -> String -> RawFile -> Module
toPackageModule package fileName rawFile =
    Package
        { name = RawFile.moduleName rawFile |> Maybe.withDefault []
        , fileName = fileName
        , interface = Interface.build rawFile
        , package = package
        }


toInternalModule : String -> RawFile -> Module
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



-- Program


port toElm : (InFile -> msg) -> Sub msg


port restore : (CachedFile -> msg) -> Sub msg


port toJS : Encode.Value -> Cmd msg


port allUnused : List Function -> Cmd msg


port fetch : (() -> msg) -> Sub msg


port check : (Function -> msg) -> Sub msg


port storeFile :
    { content : String
    , data : Encode.Value
    }
    -> Cmd msg


port showUsages : List ( Function, List Int ) -> Cmd msg


store : InFile -> Encode.Value -> Cmd msg
store file data =
    storeFile { content = file.content, data = data }


type alias Model =
    { todo :
        List
            { blockedBy : Set Syntax.ModuleName
            , data : ModuleData
            }
    , done : Dict Syntax.ModuleName Module
    , entryPoints : Set Function
    , callGraph : CallGraph
    }


type Msg
    = Parse InFile
    | Restore CachedFile
    | Send
    | Check Function


init : ( Model, Cmd msg )
init =
    ( { todo = []
      , done = Dict.empty
      , entryPoints = Set.empty
      , callGraph = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse file ->
            case parse file of
                Ok ( mod, data ) ->
                    ( add mod model
                    , store file data
                    )

                Err e ->
                    ( model
                    , toJS <| Encode.string <| file.fileName ++ ": " ++ toString e
                    )

        Restore cached ->
            case parseCached cached of
                Ok mod ->
                    ( add mod model
                    , Cmd.none
                    )

                Err e ->
                    ( model
                    , Cmd.batch
                        [ toJS <| Encode.string <| toString e
                        , toJS <| Encode.string cached.fileName
                        ]
                    )

        Send ->
            ( model
            , allUnused <| Set.toList <| findUnused model
            )

        Check fun ->
            ( model
            , showUsages <| CallGraph.findUsages fun model.callGraph
            )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ toElm Parse
        , fetch (always Send)
        , restore Restore
        , check Check
        ]


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = always subscriptions
        }



-- Static helperdata
