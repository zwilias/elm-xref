module CallGraph exposing
    ( CallGraph
    , Caller
    , Function
    , Usage
    , addCalls
    , findUsages
    , walkGraph
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Set exposing (Set)


type alias CallGraph =
    Dict Function (List ( Int, Function ))


type alias Function =
    ( ModuleName, String )


type alias Caller =
    { modul : ModuleName
    , fun : String
    , line : Int
    }


type alias Usage =
    { caller : Caller
    , callee :
        { modul : ModuleName
        , fun : String
        }
    }


addCalls : List Usage -> CallGraph -> CallGraph
addCalls usages graph =
    List.foldl addCall graph usages


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


walkGraph : CallGraph -> Function -> Set Function -> Set Function
walkGraph graph func acc =
    case Set.member func acc of
        True ->
            acc

        False ->
            Dict.get func graph
                |> Maybe.withDefault []
                |> List.foldl (Tuple.second >> walkGraph graph) (Set.insert func acc)


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
