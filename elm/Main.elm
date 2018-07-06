port module Main exposing (main, toElm, toJS)

import Dict exposing (Dict)
import Elm.Interface as Interface exposing (Interface)
import Elm.Parser as Parser
import Elm.Processing as Processing
import Elm.RawFile as RawFile exposing (RawFile)
import Elm.Syntax.Base as Syntax
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Import)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)
import Json.Encode as Json
import Set exposing (Set)


type alias Model =
    { todo :
        List
            { blockedBy : Set Syntax.ModuleName
            , data : Module
            }
    , done : Dict Syntax.ModuleName InModule
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
    , usages : Dict String (List Caller)
    }


type alias Module =
    { name : Syntax.ModuleName
    , fileName : String
    , interface : Interface
    , imports : List Import
    , declarations : List (Ranged Declaration)
    , usages : Dict String (List Caller)
    }


type Msg
    = Parse InFile
    | Send


init : ( Model, Cmd msg )
init =
    ( { todo = []
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
            ( model
            , allUnused <| findUnused model.done
            )


type alias Unused =
    { modul : Syntax.ModuleName
    , fun : String
    }


findUnused : Dict Syntax.ModuleName InModule -> List Unused
findUnused allCalls =
    Dict.foldl
        (\k mod acc ->
            findUnusedInModule k (usages mod) acc
        )
        []
        allCalls


findUnusedInModule :
    Syntax.ModuleName
    -> Dict String (List Caller)
    -> List Unused
    -> List Unused
findUnusedInModule mod calls callsAcc =
    Dict.foldl
        (\f callers acc ->
            case callers of
                [] ->
                    { modul = mod, fun = f } :: acc

                _ ->
                    acc
        )
        callsAcc
        calls


usages : InModule -> Dict String (List Caller)
usages mod =
    case mod of
        Own m ->
            m.usages

        Package p ->
            p.usages


type alias Errors =
    List String


add : InModule -> Model -> Model
add mod model =
    case mod of
        Package package ->
            { model | done = Dict.insert package.name mod model.done }
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
        ( newlyDone, newTodo, usages ) =
            List.foldl
                (\{ blockedBy, data } ( done, todo, usages ) ->
                    let
                        newBlockers =
                            Set.diff blockedBy nowDone
                    in
                    if Set.isEmpty newBlockers then
                        let
                            ( modul, usg ) =
                                finalize model data
                        in
                        ( modul :: done
                        , todo
                        , usg :: usages
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
            List.foldl registerUsages newModel usages
                |> checkTodo (Dict.keys newDone |> Set.fromList)


registerUsages : List Usage -> Model -> Model
registerUsages usages model =
    { model | done = List.foldl registerUsage model.done usages }


registerUsage : Usage -> Dict Syntax.ModuleName InModule -> Dict Syntax.ModuleName InModule
registerUsage usage mods =
    Dict.update usage.callee.modul
        (\modul ->
            case modul of
                Nothing ->
                    Nothing

                Just (Own mod) ->
                    Just <|
                        Own
                            { mod
                                | usages =
                                    registerCaller
                                        usage.callee.fun
                                        usage.caller
                                        mod.usages
                            }

                Just (Package pkg) ->
                    Just <|
                        Package
                            { pkg
                                | usages =
                                    registerCaller
                                        usage.callee.fun
                                        usage.caller
                                        pkg.usages
                            }
        )
        mods


registerCaller : String -> Caller -> Dict String (List Caller) -> Dict String (List Caller)
registerCaller fun caller usages =
    Dict.update fun
        (\u ->
            case u of
                Nothing ->
                    Just [ caller ]

                Just calls ->
                    Just (caller :: calls)
        )
        usages


type alias DeclDict =
    { unqualified : List String
    , qualified : List String
    , qualifier : List String
    , name : List String
    }


type alias Caller =
    { modul : Syntax.ModuleName
    , fun : String
    , line : Int
    }


type alias Usage =
    { caller : Caller
    , callee :
        { modul : Syntax.ModuleName
        , fun : String
        }
    }


finalize : Model -> Module -> ( Module, List Usage )
finalize m modul =
    let
        importedDecls : List DeclDict
        importedDecls =
            List.map (declImportDict m.done) modul.imports

        scope : List DeclDict
        scope =
            importedDecls ++ defaultImports

        initialUsages : Dict String (List Caller)
        initialUsages =
            List.foldl
                (\f ->
                    Dict.insert f []
                )
                Dict.empty
                (List.concatMap (Ranged.value >> resolve) modul.declarations)
    in
    ( { modul | usages = initialUsages }
    , gatherCalls modul.name scope modul.declarations
    )


gatherCalls : Syntax.ModuleName -> List DeclDict -> List (Ranged Declaration) -> List Usage
gatherCalls name scope decls =
    let
        localScope : DeclDict
        localScope =
            { unqualified = List.concatMap (Ranged.value >> resolve) decls
            , qualified = []
            , qualifier = []
            , name = name
            }

        currentScope : List DeclDict
        currentScope =
            localScope :: scope
    in
    List.foldl (gatherDeclarationCalls name currentScope) [] decls


type alias Scope =
    { modul : Syntax.ModuleName
    , function : String
    , extra : List (List String)
    , available : List DeclDict
    }


gatherDeclarationCalls :
    Syntax.ModuleName
    -> List DeclDict
    -> Ranged Declaration
    -> List Usage
    -> List Usage
gatherDeclarationCalls name scope decl acc =
    case Ranged.value decl of
        Declaration.Destructuring pat expression ->
            -- TODO: enter the `i`
            acc

        Declaration.FuncDecl { declaration } ->
            gatherExpression
                { modul = name
                , function = declaration.name.value
                , extra = []
                , available =
                    { qualified = []
                    , unqualified = List.concatMap patternToNames declaration.arguments
                    , qualifier = []
                    , name = name
                    }
                        :: scope
                }
                declaration.expression
                acc

        _ ->
            acc


gatherExpression : Scope -> Ranged Expression -> List Usage -> List Usage
gatherExpression scope ( range, expr ) acc =
    case expr of
        Expression.UnitExpr ->
            acc

        Expression.Application es ->
            List.foldl (gatherExpression scope) acc es

        Expression.OperatorApplication op _ expr1 expr2 ->
            register range.start.row op scope acc
                |> gatherExpression scope expr1
                |> gatherExpression scope expr2

        Expression.FunctionOrValue v ->
            register range.start.row v scope acc

        Expression.IfBlock cond ifExpr elseExpr ->
            acc
                |> gatherExpression scope cond
                |> gatherExpression scope ifExpr
                |> gatherExpression scope elseExpr

        Expression.PrefixOperator p ->
            register range.start.row p scope acc

        Expression.Operator o ->
            register range.start.row o scope acc

        Expression.Negation n ->
            gatherExpression scope n acc

        Expression.TupledExpression p ->
            List.foldl (gatherExpression scope) acc p

        Expression.ParenthesizedExpression p ->
            gatherExpression scope p acc

        Expression.LetExpression { declarations, expression } ->
            let
                letScope : Scope
                letScope =
                    { scope
                        | extra =
                            List.concatMap gatherLetNames
                                declarations
                                :: scope.extra
                    }
            in
            List.foldl (gatherLetDeclaration letScope) acc declarations
                |> gatherExpression letScope expression

        Expression.CaseExpression { expression, cases } ->
            acc
                |> gatherExpression scope expression
                |> (\c -> List.foldl (gatherCase scope) c cases)

        Expression.LambdaExpression { args, expression } ->
            let
                lambdaScope : Scope
                lambdaScope =
                    { scope
                        | extra = List.concatMap patternToNames args :: scope.extra
                    }
            in
            gatherExpression lambdaScope expression acc

        Expression.RecordExpr setters ->
            List.foldl (\( _, e ) -> gatherExpression scope e) acc setters

        Expression.ListExpr items ->
            List.foldl (gatherExpression scope) acc items

        Expression.QualifiedExpr qualifier f ->
            registerQualified range.start.row qualifier f scope acc

        Expression.RecordAccess record _ ->
            gatherExpression scope record acc

        Expression.RecordUpdateExpression { updates } ->
            List.foldl (\( _, e ) -> gatherExpression scope e) acc updates

        _ ->
            acc


register : Int -> String -> Scope -> List Usage -> List Usage
register line f scope acc =
    let
        caller : Caller
        caller =
            { modul = scope.modul
            , fun = scope.function
            , line = line
            }
    in
    if List.any (List.member f) scope.extra then
        acc
    else
        case find (exposed f) scope.available of
            Just decl ->
                { caller = caller
                , callee = { modul = decl.name, fun = f }
                }
                    :: acc

            Nothing ->
                acc


exposed : String -> DeclDict -> Bool
exposed f dict =
    List.member f dict.unqualified


find : (a -> Bool) -> List a -> Maybe a
find pred l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x
            else
                find pred xs


registerQualified : Int -> Syntax.ModuleName -> String -> Scope -> List Usage -> List Usage
registerQualified line qualifier f scope acc =
    case find (qualified qualifier f) scope.available of
        Just decl ->
            { caller =
                { modul = scope.modul
                , fun = scope.function
                , line = line
                }
            , callee =
                { modul = decl.name
                , fun = f
                }
            }
                :: acc

        Nothing ->
            acc


qualified : Syntax.ModuleName -> String -> DeclDict -> Bool
qualified qualifier f dict =
    dict.qualifier == qualifier && List.member f dict.qualified


gatherCase : Scope -> ( Ranged Pattern, Ranged Expression ) -> List Usage -> List Usage
gatherCase scope ( pat, exp ) acc =
    gatherExpression { scope | extra = patternToNames pat :: scope.extra } exp acc


gatherLetDeclaration : Scope -> Ranged Expression.LetDeclaration -> List Usage -> List Usage
gatherLetDeclaration scope ( _, decl ) acc =
    case decl of
        Expression.LetFunction { declaration } ->
            gatherExpression
                { scope
                    | extra =
                        List.concatMap patternToNames
                            declaration.arguments
                            :: scope.extra
                }
                declaration.expression
                acc

        Expression.LetDestructuring _ e ->
            gatherExpression scope e acc


gatherLetNames : Ranged Expression.LetDeclaration -> List String
gatherLetNames ( _, decl ) =
    case decl of
        Expression.LetFunction f ->
            [ f.declaration.name.value ]

        Expression.LetDestructuring p _ ->
            patternToNames p


patternToNames : Ranged Pattern -> List String
patternToNames pat =
    case Ranged.value pat of
        Pattern.TuplePattern p ->
            List.concatMap patternToNames p

        Pattern.RecordPattern p ->
            List.map .value p

        Pattern.UnConsPattern h t ->
            patternToNames h ++ patternToNames t

        Pattern.ListPattern l ->
            List.concatMap patternToNames l

        Pattern.VarPattern s ->
            [ s ]

        Pattern.NamedPattern _ p ->
            List.concatMap patternToNames p

        Pattern.AsPattern p n ->
            n.value :: patternToNames p

        Pattern.ParenthesizedPattern p ->
            patternToNames p

        _ ->
            []


interface : InModule -> Interface
interface mod =
    case mod of
        Package p ->
            p.interface

        Own m ->
            m.interface


declImportDict : Dict Syntax.ModuleName InModule -> Module.Import -> DeclDict
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
    , { unqualified = []
      , qualified =
            [ "withDefault"
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
    , { unqualified = []
      , qualified =
            [ "map"
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
    -> Exposing.TopLevelExpose
    -> List String
resolveImport iface expose =
    case expose of
        Exposing.InfixExpose i ->
            [ i ]

        Exposing.FunctionExpose f ->
            [ f ]

        _ ->
            []


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

        _ ->
            []


resolve : Declaration -> List String
resolve decl =
    case decl of
        Declaration.FuncDecl f ->
            [ f.declaration.name.value ]

        Declaration.PortDeclaration p ->
            [ p.name.value ]

        Declaration.Destructuring p _ ->
            patternToNames p

        _ ->
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
    Dict.keys model.done
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
        , usages = Dict.empty
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
        , usages = Dict.empty
        }


port toElm : (InFile -> msg) -> Sub msg


port toJS : Json.Value -> Cmd msg


port allUnused : List Unused -> Cmd msg


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
