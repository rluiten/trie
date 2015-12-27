module TrieFloatEncode where

{-| Json Encoder for Trie

If someone knows of a more compact json encoded form that does not
make the decoder more complex please let me know.

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing ( Dict )
import Json.Encode exposing (..)
import Set
import String
import Trie exposing ( Trie(..) )


encodeTrie : Trie f -> (f -> Value) -> Value
encodeTrie trie valEnc =
    case trie of
      EmptyTrie ->
        null -- string "null" -- Think this is ok

      ValNode refValues -> -- refValues is dict.
        list [encodeRefValues refValues valEnc, (int 0)]

      TrieNode trieDict ->
        list [(int 0), encodeTrieDict trieDict valEnc]

      ValTrieNode refValues trieDict ->
        let
          encodedValues = encodeRefValues refValues valEnc
          encodedDict = encodeTrieDict trieDict valEnc
        in
          list [encodedValues, encodedDict]


{-| Encode the Char trie dict. -}
encodeTrieDict : Dict Char (Trie f) -> (f -> Value) -> Value
encodeTrieDict trieDict valEnc =
    object <|
      List.map
        (\( key, val ) -> ( String.fromChar key, encodeTrie val valEnc ))
        (Dict.toList trieDict)


{-| Encode the Dict of document references and tf score. -}
encodeRefValues : Dict String f -> (f -> Value) -> Value
encodeRefValues refValues valEnc =
    object <|
      List.map
        (\( key, val ) -> ( key, valEnc val ))
        (Dict.toList refValues)
