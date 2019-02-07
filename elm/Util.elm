module Util exposing
    ( patternToFunctions
    , patternToNames
    , patternToNodes
    , resolveNames
    , resolveNodes
    )

import CallGraph exposing (Function)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)


resolveNames : Declaration -> List String
resolveNames =
    resolveNodes >> List.map Node.value


resolveNodes : Declaration -> List (Node String)
resolveNodes decl =
    case decl of
        Declaration.FunctionDeclaration f ->
            [ f.declaration
                |> Node.value
                |> .name
            ]

        Declaration.PortDeclaration p ->
            [ p.name ]

        Declaration.Destructuring p _ ->
            patternToNodes p

        Declaration.CustomTypeDeclaration { constructors } ->
            List.map (Node.value >> .name) constructors

        _ ->
            []


patternToFunctions : Node Pattern -> List Function
patternToFunctions pat =
    case Node.value pat of
        Pattern.TuplePattern p ->
            List.concatMap patternToFunctions p

        Pattern.UnConsPattern h t ->
            patternToFunctions h ++ patternToFunctions t

        Pattern.ListPattern l ->
            List.concatMap patternToFunctions l

        Pattern.ParenthesizedPattern p ->
            patternToFunctions p

        Pattern.AsPattern p _ ->
            patternToFunctions p

        Pattern.NamedPattern ref ps ->
            ( ref.moduleName, ref.name ) :: List.concatMap patternToFunctions ps

        _ ->
            []


patternToNames : Node Pattern -> List String
patternToNames =
    patternToNodes >> List.map Node.value


patternToNodes : Node Pattern -> List (Node String)
patternToNodes pat =
    case Node.value pat of
        Pattern.TuplePattern p ->
            List.concatMap patternToNodes p

        Pattern.RecordPattern p ->
            p

        Pattern.UnConsPattern h t ->
            patternToNodes h ++ patternToNodes t

        Pattern.ListPattern l ->
            List.concatMap patternToNodes l

        Pattern.VarPattern s ->
            [ Node.Node (Node.range pat) s ]

        Pattern.NamedPattern _ p ->
            List.concatMap patternToNodes p

        Pattern.AsPattern p n ->
            n :: patternToNodes p

        Pattern.ParenthesizedPattern p ->
            patternToNodes p

        _ ->
            []
