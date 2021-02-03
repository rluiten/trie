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
                    TrieNode (updateTrieDict keyHead keyTail ref value trieDict)

                ValTrieNode ( refValues, trieDict ) ->
                    ValTrieNode ( refValues, updateTrieDict keyHead keyTail ref value trieDict )


createTrieDict : comparable -> List String -> String -> d -> Dict comparable (Trie d)
createTrieDict keyHead keyTail ref value =
    EmptyTrie
        |> addByStr ( ref, value ) keyTail
        |> Dict.singleton keyHead


updateTrieDict : comparable -> List String -> String -> d -> Dict comparable (Trie d) -> Dict comparable (Trie d)
updateTrieDict keyHead keyTail ref value trieDict =
    Dict.insert keyHead
        (Dict.get keyHead trieDict
            |> Maybe.withDefault EmptyTrie
            |> addByStr ( ref, value ) keyTail
        )
        trieDict


{-| Remove values for key and reference from Trie.

This removes the reference from the correct values list.
If the key does not exist nothing changes.
If the ref is not found in the values for the key nothing changes.

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
    removeByChars (toChars key) ref (Just trie)
        |> Maybe.withDefault EmptyTrie


{-| Create Maybe of Dictionary returning Nothing if empty dictionary.
-}
filterNonEmptyDict : Dict String a -> Maybe (Dict String a)
filterNonEmptyDict dict =
    if Dict.isEmpty dict then
        Nothing

    else
        Just dict


{-| New variant that returns Nothing if removed value leaves empty.
-}
removeByChars : List String -> String -> Maybe (Trie a) -> Maybe (Trie a)
removeByChars key ref trie =
    case key of
        [] ->
            Maybe.andThen (removeFromTrieLeaf ref) trie

        keyHead :: keyTail ->
            Maybe.andThen (removeFromTrie keyHead keyTail ref) trie


{-| Remove the ref document from any values containers.
This function is for use when you have found the node.

Returns Nothing if trie becomes empty from removal.

Can't easilly use getNodeCore because it has to update
parent nodes if a sub tree becomes empty

-}
removeFromTrieLeaf : String -> Trie a -> Maybe (Trie a)
removeFromTrieLeaf ref aTrie =
    case aTrie of
        EmptyTrie ->
            Nothing

        ValNode valuesDict ->
            -- optimisation return same aTrie if not in dictValues
            if Dict.member ref valuesDict then
                Dict.remove ref valuesDict
                    |> filterNonEmptyDict
                    |> Maybe.map (\r -> ValNode r)

            else
                Just aTrie

        TrieNode _ ->
            Just aTrie

        ValTrieNode ( valuesDict, trieDict ) ->
            -- optimisation return same aTrie if not in dictValues
            if Dict.member ref valuesDict then
                Dict.remove ref valuesDict
                    |> filterNonEmptyDict
                    |> Maybe.map (\values -> ValTrieNode ( values, trieDict ))
                    |> Maybe.withDefault (TrieNode trieDict)
                    |> Just

            else
                Just aTrie


{-| Remove the reference from any values in trie.
Returns Nothing if the trie becomes empty from removal.
-}
removeFromTrie : String -> List String -> String -> Trie a -> Maybe (Trie a)
removeFromTrie keyHead keyTail ref aTrie =
    case aTrie of
        EmptyTrie ->
            Nothing

        ValNode _ ->
            Just aTrie

        TrieNode trieDict ->
            removeFromTrieDict keyHead keyTail ref trieDict
                |> Maybe.map (\d2 -> TrieNode d2)

        ValTrieNode ( refValues, trieDict ) ->
            removeFromTrieDict keyHead keyTail ref trieDict
                |> Maybe.map (\d2 -> ValTrieNode ( refValues, d2 ))
                |> Maybe.withDefault (ValNode refValues)
                |> Just


{-| Recursive remove the ref document from trie on given keyHead keyTail sequence.
-}
removeFromTrieDict : String -> List String -> String -> Dict String (Trie a) -> Maybe (Dict String (Trie a))
removeFromTrieDict keyHead keyTail ref trieDict =
    case Dict.get keyHead trieDict of
        Nothing ->
            Just trieDict

        justTrie ->
            removeByChars keyTail ref justTrie
                |> MaybeExtra.filter (isEmpty >> not)
                |> Maybe.map (\a -> Dict.insert keyHead a trieDict)


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
