module Gather exposing (DeclDict, Scope, calls, defaultImports)

import CallGraph exposing (Usage)
import Elm.Syntax.Base as Base
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)
import Util


type alias DeclDict =
    { unqualified : List String
    , qualified : List String
    , qualifier : List String
    , name : List String
    }


type alias Scope =
    { modul : Base.ModuleName
    , function : String
    , extra : List (List String)
    , available : List DeclDict
    }


calls : Base.ModuleName -> List DeclDict -> List (Ranged Declaration) -> List Usage
calls name scope decls =
    let
        localScope : DeclDict
        localScope =
            { unqualified = List.concatMap (Ranged.value >> Util.resolve) decls
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
    Base.ModuleName
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
                , extra = [ List.concatMap Util.patternToNames declaration.arguments ]
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
                        | extra = List.concatMap Util.patternToNames args :: scope.extra
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
        caller : CallGraph.Caller
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


registerQualified : Int -> Base.ModuleName -> String -> Scope -> List Usage -> List Usage
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


qualified : Base.ModuleName -> String -> DeclDict -> Bool
qualified qualifier f dict =
    dict.qualifier == qualifier && List.member f dict.qualified


gatherCase : Scope -> ( Ranged Pattern, Ranged Expression ) -> List Usage -> List Usage
gatherCase scope ( pat, exp ) acc =
    gatherExpression { scope | extra = Util.patternToNames pat :: scope.extra } exp acc


gatherLetDeclaration : Scope -> Ranged Expression.LetDeclaration -> List Usage -> List Usage
gatherLetDeclaration scope ( _, decl ) acc =
    case decl of
        Expression.LetFunction { declaration } ->
            gatherExpression
                { scope
                    | extra =
                        List.concatMap Util.patternToNames
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
            Util.patternToNames p


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