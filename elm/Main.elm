port module Main exposing (main, toElm, toJS)

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
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Import)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)


type alias Function =
    ( Syntax.ModuleName, String )


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


type alias Scope =
    { modul : Syntax.ModuleName
    , function : String
    , extra : List (List String)
    , available : List DeclDict
    }


findUnused : Model -> Set Function
findUnused model =
    let
        usedFunctions : Set Function
        usedFunctions =
            Set.foldl (walkGraph model.callGraph)
                Set.empty
                model.entryPoints

        allOwnFunctions : Set Function
        allOwnFunctions =
            Dict.foldl findOwnFunctions Set.empty model.done
    in
    Set.diff allOwnFunctions usedFunctions


walkGraph : CallGraph -> Function -> Set Function -> Set Function
walkGraph graph func acc =
    case Set.member func acc of
        True ->
            acc

        False ->
            Dict.get func graph
                |> Maybe.withDefault []
                |> List.foldl (Tuple.second >> walkGraph graph) (Set.insert func acc)


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
                (List.concatMap (Ranged.value >> resolve) m.declarations)


findUsages : Function -> CallGraph -> List ( Function, List Int )
findUsages fun =
    Dict.foldr
        (\f calls acc ->
            let
                filteredCalls =
                    List.filterMap
                        (\( line, calledF ) ->
                            if calledF == fun then
                                Just line
                            else
                                Nothing
                        )
                        calls
            in
            case filteredCalls of
                [] ->
                    acc

                c ->
                    ( f, c ) :: acc
        )
        []


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
    { model | callGraph = List.foldl addCall model.callGraph usages }


addCall : Usage -> CallGraph -> CallGraph
addCall { caller, callee } graph =
    Dict.update ( caller.modul, caller.fun )
        (\calls ->
            case calls of
                Nothing ->
                    Just <| [ ( caller.line + 1, ( callee.modul, callee.fun ) ) ]

                Just c ->
                    Just <| ( caller.line + 1, ( callee.modul, callee.fun ) ) :: c
        )
        graph


finalize : Model -> ModuleData -> List Usage
finalize m modul =
    let
        importedDecls : List DeclDict
        importedDecls =
            List.map (declImportDict m.done) modul.imports

        scope : List DeclDict
        scope =
            importedDecls ++ defaultImports
    in
    gatherCalls modul.name scope modul.declarations


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


gatherDeclarationCalls :
    Syntax.ModuleName
    -> List DeclDict
    -> Ranged Declaration
    -> List Usage
    -> List Usage
gatherDeclarationCalls name scope decl acc =
    case Ranged.value decl of
        Declaration.Destructuring pat expression ->
            -- NOTE: in 0.18, you can still destructure a complex expression,
            -- meaning that each usage would correspond to the definition of
            -- (possibly) many variables. This is annoying, an 0.19 doesn't
            -- allow it anymore, so ignoring that use-case for now.
            acc

        Declaration.FuncDecl { declaration } ->
            gatherExpression
                { modul = name
                , function = declaration.name.value
                , extra = [ List.concatMap patternToNames declaration.arguments ]
                , available = scope
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

        Expression.RecordUpdateExpression { name, updates } ->
            List.foldl (\( _, e ) -> gatherExpression scope e)
                (register range.start.row name scope acc)
                updates

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


interface : Module -> Interface
interface mod =
    case mod of
        Package p ->
            p.interface

        Own m ->
            m.interface


declImportDict : Dict Syntax.ModuleName Module -> Module.Import -> DeclDict
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


resolve : Declaration -> List String
resolve decl =
    case decl of
        Declaration.FuncDecl f ->
            [ f.declaration.name.value ]

        Declaration.PortDeclaration p ->
            [ p.name.value ]

        Declaration.Destructuring p _ ->
            patternToNames p

        Declaration.TypeDecl { constructors } ->
            List.map .name constructors

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


type alias CallGraph =
    Dict Function (List ( Int, Function ))


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
            , showUsages <| findUsages fun model.callGraph
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
    , { unqualified = [ "Just", "Nothing" ]
      , qualified =
            [ "Just"
            , "Nothing"
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
    , { unqualified = [ "Ok", "Err" ]
      , qualified =
            [ "Ok"
            , "Err"
            , "isEmpty"
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
