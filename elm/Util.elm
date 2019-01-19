module Util exposing (patternToNames, resolve)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)


resolve : Declaration -> List String
resolve decl =
    case decl of
        Declaration.FunctionDeclaration f ->
            [ f.declaration
                |> Node.value
                |> .name
                |> Node.value
            ]

        Declaration.PortDeclaration p ->
            [ Node.value p.name ]

        Declaration.Destructuring p _ ->
            patternToNames p

        Declaration.CustomTypeDeclaration { constructors } ->
            List.map (Node.value >> .name >> Node.value) constructors

        _ ->
            []


patternToNames : Node Pattern -> List String
patternToNames pat =
    case Node.value pat of
        Pattern.TuplePattern p ->
            List.concatMap patternToNames p

        Pattern.RecordPattern p ->
            List.map Node.value p

        Pattern.UnConsPattern h t ->
            patternToNames h ++ patternToNames t

        Pattern.ListPattern l ->
            List.concatMap patternToNames l

        Pattern.VarPattern s ->
            [ s ]

        Pattern.NamedPattern _ p ->
            List.concatMap patternToNames p

        Pattern.AsPattern p n ->
            Node.value n :: patternToNames p

        Pattern.ParenthesizedPattern p ->
            patternToNames p

        _ ->
            []
