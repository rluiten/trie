module Trie.Json.Encoder exposing (encoder)

{-| Json Encoder for Trie

@docs encoder

Copyright (c) 2015 Robin Luiten

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import Set
import String
import TrieModel exposing (Trie(..))


{-| Encoder for Trie.
-}
encoder : (f -> Encode.Value) -> Trie f -> Encode.Value
encoder valEnc trie =
    case trie of
        EmptyTrie ->
            Encode.null

        ValNode refValues ->
            encoderValDict valEnc refValues

        TrieNode trieDict ->
            encoderTrieDict valEnc trieDict

        ValTrieNode ( refValues, trieDict ) ->
            let
                encodedValues =
                    encoderValDict valEnc refValues

                encodedDict =
                    encoderTrieDict valEnc trieDict
            in
            Encode.list identity [ encodedValues, encodedDict ]


{-| Encode the Dict of Value references.
-}
encoderValDict : (f -> Encode.Value) -> Dict String f -> Encode.Value
encoderValDict valEnc refValues =
    Dict.toList refValues
        |> List.map (\( key, val ) -> ( key, valEnc val ))
        |> Encode.object


{-| Encode the core Trie structure Dict.
-}
encoderTrieDict : (f -> Encode.Value) -> Dict String (Trie f) -> Encode.Value
encoderTrieDict valEnc trieDict =
    Dict.toList trieDict
        |> List.map (\( key, val ) -> ( key, encoder valEnc val ))
        |> Encode.object
