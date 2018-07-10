module Util exposing (patternToNames, resolve)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Ranged as Ranged exposing (Ranged)


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
