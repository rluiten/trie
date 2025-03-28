module TrieModel exposing
    ( Trie(..)
    , empty
    , add
    , remove
    , has
    , get
    , getNode
    , valueCount
    , expand
    , isEmpty
    , getValues
    )

{-| A Trie data structure.

A trie is an ordered tree data structure that is used to store a dynamic
set or associative array where the keys are usually strings.

In this implementation they key is a String.

In this implementation unique reference stored in the value
dictionary for a given key is a String.


## Data Model

@docs Trie


## Create

@docs empty


## Modify

@docs add
@docs remove


## Query

@docs has
@docs get
@docs getNode
@docs valueCount
@docs expand
@docs isEmpty


## Get data values from node

@docs getValues

Copyright (c) 2015 Robin Luiten

-}

import Dict exposing (Dict)
import List
import Maybe
import MaybeExtra
import String


{-| Trie data model.

    type Trie a
        = EmptyTrie
        | ValNode (Dict String a)
        | TrieNode (Dict String (Trie a))
        | ValTrieNode ( Dict String a, Dict String (Trie a) )

-}
type Trie a
    = EmptyTrie
    | ValNode (Dict String a)
    | TrieNode (Dict String (Trie a))
    | ValTrieNode ( Dict String a, Dict String (Trie a) )


{-| An empty Trie
-}
empty : Trie a
empty =
    EmptyTrie


{-| Returns True if Trie is empty
-}
isEmpty : Trie a -> Bool
isEmpty trie =
    trie == empty


{-| Add reference and values with key to Trie.
-}
add : ( String, a ) -> String -> Trie a -> Trie a
add refValues key trie =
    addByStr refValues (toChars key) trie


{-| break string up into list of single Char strings
-}
toChars : String -> List String
toChars str =
    List.map
        (\c -> String.fromChar c)
        (String.toList str)


{-| see add
-}
addByStr : ( String, a ) -> List String -> Trie a -> Trie a
addByStr ( ref, value ) key trie =
    case key of
        [] ->
            case trie of
                EmptyTrie ->
                    ValNode (Dict.singleton ref value)

                ValNode refValues ->
                    ValNode (Dict.insert ref value refValues)

                TrieNode trieDict ->
                    ValTrieNode ( Dict.singleton ref value, trieDict )

                ValTrieNode ( refValues, trieDict ) ->
                    ValTrieNode ( Dict.insert ref value refValues, trieDict )

        keyHead :: keyTail ->
            case trie of
                EmptyTrie ->
                    TrieNode (createTrieDict keyHead keyTail ref value)

                ValNode refValues ->
                    ValTrieNode ( refValues, createTrieDict keyHead keyTail ref value )

                TrieNode trieDict ->
                    TrieNode (updateAddTrieDict keyHead keyTail ref value trieDict)

                ValTrieNode ( refValues, trieDict ) ->
                    ValTrieNode ( refValues, updateAddTrieDict keyHead keyTail ref value trieDict )


createTrieDict : comparable -> List String -> String -> d -> Dict comparable (Trie d)
createTrieDict keyHead keyTail ref value =
    EmptyTrie
        |> addByStr ( ref, value ) keyTail
        |> Dict.singleton keyHead


updateAddTrieDict : comparable -> List String -> String -> d -> Dict comparable (Trie d) -> Dict comparable (Trie d)
updateAddTrieDict keyHead keyTail ref value trieDict =
    Dict.insert keyHead
        (Dict.get keyHead trieDict
            |> Maybe.withDefault EmptyTrie
            |> addByStr ( ref, value ) keyTail
        )
        trieDict


{-| Remove values for key and reference from Trie.

Return the same trie if no changes required.
Idempotent if a key or ref is not in tree no change happens same trie returned
You can check trie instance value against original if you want to know if trie changed.

An example but does not do anything.

    updatedTrie =
        Trie.remove "for" "refid125" Trie.empty

Add something then remove it.

    trie1 =
        Trie.add ( "refid123", ( "ValueStored", 42.34 ) ) "someword" Trie.empty

    trie2 =
        Trie.remove "someword" "refid123" Trie.trie1

-}
remove : String -> String -> Trie a -> Trie a
remove key ref trie =
    remove_ (toChars key) ref trie


{-| Remove the ref at key in trie.

This is recursive.
Return the same trie if no changes were made.
Reasons for this might be key not found or ref not found.

-}
remove_ : List String -> String -> Trie a -> Trie a
remove_ key ref trie =
    let
        --| remove and update only happen if newChildTrie /= childTrie
        updateRemoveTrieDict :
            Dict comparable (Trie a)
            -> comparable
            -> List String
            -> Trie a
            -> (Dict comparable (Trie a) -> Trie a)
            -> (Trie a -> Dict comparable (Trie a) -> Trie a)
            -> Trie a
        updateRemoveTrieDict =
            \trieDict head tail noDictNode removeChildFunc replaceChildFunc ->
                case Dict.get head trieDict of
                    -- can't find head the next part of key nothing changes
                    Nothing ->
                        trie

                    Just childTrie ->
                        let
                            newChildTrie =
                                remove_ tail ref childTrie
                        in
                        if newChildTrie == childTrie then
                            -- as i didnt find key match nothing changes
                            trie

                        else if newChildTrie == EmptyTrie then
                            if Dict.size trieDict == 1 then
                                noDictNode

                            else
                                removeChildFunc trieDict

                        else
                            replaceChildFunc newChildTrie trieDict
    in
    case key of
        [] ->
            -- end of tree only have ref in vals to care about
            let
                removeValForKey : Dict String a -> Trie a -> (Dict String a -> Trie a) -> Trie a
                removeValForKey =
                    \vals noValsNode valsFunc ->
                        let
                            newVals =
                                Dict.remove ref vals
                        in
                        if newVals == vals then
                            trie

                        else if Dict.size newVals == 0 then
                            noValsNode

                        else
                            valsFunc newVals
            in
            case trie of
                EmptyTrie ->
                    EmptyTrie

                ValNode vals ->
                    removeValForKey vals EmptyTrie (\newVals -> ValNode newVals)

                TrieNode _ ->
                    trie

                ValTrieNode ( vals, trieDict ) ->
                    removeValForKey vals (TrieNode trieDict) (\newVals -> ValTrieNode ( newVals, trieDict ))

        head :: [] ->
            -- the childTrie if found by head may contain ref in vals
            case trie of
                EmptyTrie ->
                    EmptyTrie

                ValNode _ ->
                    trie

                TrieNode trieDict ->
                    -- in this case childTrie may contain the reference
                    updateRemoveTrieDict
                        trieDict
                        head
                        []
                        EmptyTrie
                        (\trieDict2 -> TrieNode (Dict.remove head trieDict2))
                        (\newChildTrie trieDict2 -> TrieNode (Dict.insert head newChildTrie trieDict2))

                ValTrieNode ( vals, trieDict ) ->
                    updateRemoveTrieDict
                        trieDict
                        head
                        []
                        (ValNode vals)
                        (\trieDict2 -> ValTrieNode ( vals, Dict.remove head trieDict2 ))
                        (\newChildTrie trieDict2 -> ValTrieNode ( vals, Dict.insert head newChildTrie trieDict2 ))

        head :: tail ->
            -- If childTrie does not change then current childTrie reference should not change
            case trie of
                EmptyTrie ->
                    EmptyTrie

                ValNode _ ->
                    trie

                TrieNode trieDict ->
                    updateRemoveTrieDict
                        trieDict
                        head
                        tail
                        EmptyTrie
                        (\trieDict2 -> TrieNode (Dict.remove head trieDict2))
                        (\newChildTrie trieDict2 -> TrieNode (Dict.insert head newChildTrie trieDict2))

                ValTrieNode ( vals, trieDict ) ->
                    updateRemoveTrieDict
                        trieDict
                        head
                        tail
                        (ValNode vals)
                        (\trieDict2 -> ValTrieNode ( vals, Dict.remove head trieDict2 ))
                        (\newChildTrie trieDict2 -> ValTrieNode ( vals, Dict.insert head newChildTrie trieDict2 ))


{-| Return Trie node if found.
-}
getNode : String -> Trie a -> Maybe (Trie a)
getNode key trie =
    getNodeByStr (toChars key) trie


{-| see getNode, better version?
-}
getNodeByStr : List String -> Trie a -> Maybe (Trie a)
getNodeByStr key trie =
    MaybeExtra.filtered (List.isEmpty >> not) key
        |> Maybe.andThen (\k -> getNodeCore k trie)


getNodeCore : List String -> Trie a -> Maybe (Trie a)
getNodeCore key trie =
    case key of
        [] ->
            Just trie

        keyHead :: keyTail ->
            getTrieDict trie
                |> Maybe.andThen (Dict.get keyHead)
                |> Maybe.andThen (getNodeCore keyTail)


{-| Checks whether key is contained within a Trie.
This means tha a "ref" exists at that key location.
We don't check which ref just that there is one.
-}
has : String -> Trie a -> Bool
has key trie =
    hasByStr (toChars key) trie


{-| see has
-}
hasByStr : List String -> Trie a -> Bool
hasByStr key trie =
    getNodeByStr key trie
        |> Maybe.andThen getValues
        |> Maybe.withDefault Dict.empty
        |> (Dict.isEmpty >> not)


{-| Return values for a key if found.
-}
get : String -> Trie a -> Maybe (Dict String a)
get key trie =
    getByStr (toChars key) trie


{-| see get
-}
getByStr : List String -> Trie a -> Maybe (Dict String a)
getByStr key trie =
    getNodeByStr key trie
        |> Maybe.andThen getValues


{-| Return the tree dict in this Trie node if there is one
-}
getTrieDict : Trie a -> Maybe (Dict String (Trie a))
getTrieDict trie =
    case trie of
        EmptyTrie ->
            Nothing

        ValNode _ ->
            Nothing

        TrieNode trieDict ->
            Just trieDict

        ValTrieNode ( _, trieDict ) ->
            Just trieDict


{-| Return the values in this Trie node if there is one
-}
getValues : Trie a -> Maybe (Dict String a)
getValues trie =
    case trie of
        EmptyTrie ->
            Nothing

        ValNode refValues ->
            Just refValues

        TrieNode _ ->
            Nothing

        ValTrieNode ( refValues, _ ) ->
            Just refValues


{-| Return number of values stored at Trie location.
-}
valueCount : String -> Trie a -> Int
valueCount key trie =
    Dict.size (Maybe.withDefault Dict.empty (get key trie))


{-| Find all the possible suffixes of the passed key using keys
currently in the store.
-}
expand : String -> Trie a -> List String
expand key trie =
    expandByStr (toChars key) trie


{-| see expand
-}
expandByStr : List String -> Trie a -> List String
expandByStr key trie =
    case getNodeByStr key trie of
        Nothing ->
            []

        Just keyTrie ->
            expandCore key keyTrie []


expandCore : List String -> Trie a -> List String -> List String
expandCore key trie keyList =
    let
        addRefKey refValues =
            if not (Dict.isEmpty refValues) then
                String.concat key :: keyList

            else
                keyList

        expandSub char trie1 foldList =
            expandCore (key ++ [ char ]) trie1 foldList
    in
    case trie of
        EmptyTrie ->
            keyList

        ValNode refValues ->
            addRefKey refValues

        TrieNode trieDict ->
            Dict.foldr expandSub keyList trieDict

        ValTrieNode ( refValues, trieDict ) ->
            let
                dirtyList =
                    addRefKey refValues
            in
            Dict.foldr expandSub dirtyList trieDict
