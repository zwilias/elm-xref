port module Main exposing (main, toElm, toJS)

import Dict exposing (Dict)
import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Base as Syntax
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Import)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)
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


type alias DeclDict =
    { unqualified : List String
    , qualified : List String
    , qualifier : List String
    , name : List String
    }


finalize : Model -> Module -> Module
finalize m modul =
    let
        localDecls : DeclDict
        localDecls =
            { unqualified = List.concatMap (Ranged.value >> resolve) modul.declarations
            , qualified = []
            , qualifier = []
            , name = modul.name
            }

        importedDecls : List DeclDict
        importedDecls =
            List.map (declImportDict m.packageData) modul.imports

        _ =
            Debug.log "local" localDecls

        _ =
            Debug.log "imported" importedDecls
    in
    -- TODO: Gather declarations and uses
    modul


declImportDict : Dict Syntax.ModuleName InterfaceData -> Module.Import -> DeclDict
declImportDict modules imports =
    let
        iface : Interface
        iface =
            Dict.get imports.moduleName modules
                |> Maybe.map .interface
                |> Maybe.withDefault []

        ctors : Dict String (List String)
        ctors =
            iface
                |> List.filterMap exposedType
                |> Dict.fromList

        allExports : List String
        allExports =
            Dict.get imports.moduleName modules
                |> Maybe.map (\m -> resolveInterface m.interface)
                |> Maybe.withDefault []
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
                    (Ranged.value >> resolveImport iface ctors)
                    list
            , qualified = allExports
            , qualifier =
                imports.moduleAlias
                    |> Maybe.withDefault imports.moduleName
            , name = imports.moduleName
            }


defaultImports : List DeclDict
defaultImports =
    [ { unqualified = basicFunctions
      , qualified = basicFunctions
      , qualifier = [ "Basics" ]
      , name = [ "Basics" ]
      }
    , { unqualified = [ "(::)", "[]" ]
      , qualified =
            [ "isEmpty"
            , "length"
            , "reverse"
            , "member"
            , "head"
            , "tail"
            , "filter"
            , "take"
            , "drop"
            , "singleton"
            , "repeat"
            , "range"
            , "(::)"
            , "append"
            , "concat"
            , "intersperce"
            , "partition"
            , "unzip"
            , "map"
            , "map2"
            , "map3"
            , "map4"
            , "map5"
            , "filterMap"
            , "concatMap"
            , "indexedMap"
            , "foldr"
            , "foldl"
            , "sum"
            , "product"
            , "maximum"
            , "minimum"
            , "all"
            , "any"
            , "scanl"
            , "sort"
            , "sortBy"
            , "sortWith"
            ]
      , qualifier = [ "List" ]
      , name = [ "List" ]
      }
    , { unqualified = [ "Just", "Nothing" ]
      , qualified =
            [ "Just"
            , "Nothing"
            , "withDefault"
            , "map"
            , "map2"
            , "map3"
            , "map4"
            , "map5"
            , "andThen"
            ]
      , qualifier = [ "Maybe" ]
      , name = [ "Maybe" ]
      }
    , { unqualified = [ "Ok", "Err" ]
      , qualified =
            [ "Ok"
            , "Err"
            , "map"
            , "map2"
            , "map3"
            , "map4"
            , "map5"
            , "andThen"
            , "withDefault"
            , "toMaybe"
            , "fromMaybe"
            , "mapError"
            ]
      , qualifier = [ "Result" ]
      , name = [ "Result" ]
      }
    , { unqualified = []
      , qualified =
            [ "isEmpty"
            , "length"
            , "reverse"
            , "repeat"
            , "cons"
            , "uncons"
            , "fromChar"
            , "append"
            , "concat"
            , "split"
            , "join"
            , "words"
            , "lines"
            , "slice"
            , "left"
            , "right"
            , "dropLeft"
            , "dropRight"
            , "contains"
            , "startsWith"
            , "endsWith"
            , "indexes"
            , "indices"
            , "toInt"
            , "toFloat"
            , "toList"
            , "fromList"
            , "toUpper"
            , "toLower"
            , "pad"
            , "padLeft"
            , "padRight"
            , "trim"
            , "trimLeft"
            , "trimRight"
            , "map"
            , "filter"
            , "foldr"
            , "foldr"
            , "any"
            , "all"
            ]
      , qualifier = [ "String" ]
      , name = [ "String" ]
      }
    , { unqualified = []
      , qualified = [ "first", "second", "mapFirst", "mapSecond" ]
      , qualifier = [ "Tuple" ]
      , name = [ "Tuple" ]
      }
    , { unqualified = []
      , qualified = [ "log", "crash" ]
      , qualifier = [ "Debug" ]
      , name = [ "Debug" ]
      }
    , { unqualified = []
      , qualified = [ "program", "programWithFlags", "sendToApp", "sendToSelf" ]
      , qualifier = [ "Platform" ]
      , name = [ "Platform" ]
      }
    , { unqualified = [ "(!)" ]
      , qualified = [ "(!)", "map", "batch", "none" ]
      , qualifier = [ "Cmd" ]
      , name = [ "Platform", "Cmd" ]
      }
    , { unqualified = []
      , qualified = [ "map", "batch", "none" ]
      , qualifier = [ "Sub" ]
      , name = [ "Platform", "Sub" ]
      }
    ]


basicFunctions : List String
basicFunctions =
    [ "(==)"
    , "(/=)"
    , "(<)"
    , "(>)"
    , "(<=)"
    , "(>=)"
    , "max"
    , "min"
    , "LT"
    , "GT"
    , "EQ"
    , "compare"
    , "not"
    , "(&&)"
    , "(||)"
    , "xor"
    , "(+)"
    , "(-)"
    , "(*)"
    , "(/)"
    , "(^)"
    , "(//)"
    , "rem"
    , "(%)"
    , "negate"
    , "abs"
    , "sqrt"
    , "clamp"
    , "logBase"
    , "e"
    , "pi"
    , "cos"
    , "sin"
    , "tan"
    , "acos"
    , "asin"
    , "atan"
    , "atan2"
    , "round"
    , "floor"
    , "ceiling"
    , "truncate"
    , "toFloat"
    , "degrees"
    , "radians"
    , "turns"
    , "toPolar"
    , "fromPolar"
    , "isNaN"
    , "isInfinite"
    , "toString"
    , "(++)"
    , "identity"
    , "always"
    , "(<|)"
    , "(|>)"
    , "(<<)"
    , "(>>)"
    , "flip"
    , "curry"
    , "uncurry"
    , "never"
    ]


resolveImport :
    Interface
    -> Dict String (List String)
    -> Exposing.TopLevelExpose
    -> List String
resolveImport iface ctors expose =
    case expose of
        Exposing.InfixExpose i ->
            [ i ]

        Exposing.FunctionExpose f ->
            [ f ]

        Exposing.TypeOrAliasExpose t ->
            [ t ]

        Exposing.TypeExpose t ->
            case t.constructors of
                Nothing ->
                    []

                Just (Exposing.All _) ->
                    Dict.get t.name ctors |> Maybe.withDefault []

                Just (Exposing.Explicit cs) ->
                    List.map Ranged.value cs


exposedType : Interface.Exposed -> Maybe ( String, List String )
exposedType exp =
    case exp of
        Interface.Function s ->
            Nothing

        Interface.Type ( t, c ) ->
            Just ( t, c )

        Interface.Alias a ->
            Nothing

        Interface.Operator i ->
            Nothing


resolveInterface : Interface -> List String
resolveInterface =
    List.concatMap resolveExposed


resolveExposed : Interface.Exposed -> List String
resolveExposed exp =
    case exp of
        Interface.Function s ->
            [ s ]

        Interface.Type ( _, c ) ->
            c

        Interface.Alias a ->
            [ a ]

        Interface.Operator i ->
            [ i.operator ]


resolve : Declaration -> List String
resolve decl =
    case decl of
        Declaration.FuncDecl f ->
            [ f.declaration.name.value ]

        Declaration.AliasDecl a ->
            [ a.name ]

        Declaration.TypeDecl t ->
            List.map .name t.constructors

        Declaration.PortDeclaration p ->
            [ p.name.value ]

        Declaration.InfixDeclaration i ->
            [ i.operator ]

        Declaration.Destructuring _ _ ->
            -- TODO: what patterns are actually valid as LHS in assignment?
            []


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
