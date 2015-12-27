module Trie.Json.Decoder (decoder) where

{-| Json Decoder for Trie

@docs decoder

Copyright (c) 2016 Robin Luiten
-}

import Dict exposing ( Dict )
import Json.Decode as Decode
import String

import TrieModel exposing ( Trie(..) )


{-  Work around a bug in Decoder with decoding recursive structures.
See https://github.com/elm-lang/elm-compiler/issues/873

This method gratefully received from from @gdotdesign on
elmlang.slack.com it was originally called "lazy".
-}
lazyDecoder : (() -> Decode.Decoder a) -> Decode.Decoder a
lazyDecoder thunk =
    Decode.customDecoder Decode.value
      (\js -> Decode.decodeValue (thunk ()) js)


{-| A Trie Decoder -}
decoder : Decode.Decoder a -> Decode.Decoder (Trie a)
decoder valDec =
    Decode.oneOf
    [ Decode.null EmptyTrie
    , decoderTrie valDec
    ]


decoderTrie : Decode.Decoder a -> Decode.Decoder (Trie a)
decoderTrie valDec =
    Decode.oneOf
    [ Decode.map ValNode (decoderValDict valDec)
    , Decode.map TrieNode (lazyDecoder (\_ -> decoderTrieDict valDec))
    , Decode.map ValTrieNode (lazyDecoder (\_ -> decoderValTrieNode valDec))
    , Decode.fail "Invalid Trie Structure found."
    ]


decoderValDict : Decode.Decoder a -> Decode.Decoder (Dict String a)
decoderValDict = Decode.dict


decoderTrieDict : Decode.Decoder a -> Decode.Decoder (Dict String (Trie a))
decoderTrieDict valDec = Decode.dict (decoder valDec)


decoderValTrieNode : Decode.Decoder a -> Decode.Decoder (Dict String a, Dict String (Trie a))
decoderValTrieNode valDec =
    Decode.tuple2 (,)
      (decoderValDict valDec)
      (decoderTrieDict valDec)
