module TrieCodecTests exposing (decodeEmptyTrieTest, decodeTrieTest1, encodeEmptyTrieTest, encodeTrieTest, exampleEncodedTrie3, tests, trie1, trie2, trie3)

{-| Test Trie encoder and decoder
-}

import Dict exposing (Dict)
import Expect
import Json.Decode exposing (..)
import Json.Encode as Encode
import Set
import String
import Test exposing (..)
import Trie
import Trie.Json.Decoder as TrieDecoder
import Trie.Json.Encoder as TrieEncoder
import TrieModel exposing (Trie(..))


tests : Test
tests =
    describe "Trie encode tests"
        [ encodeEmptyTrieTest ()
        , encodeTrieTest ()
        , decodeEmptyTrieTest ()
        , decodeTrieTest1 ()
        ]


trie1 =
    Trie.add ( "id001", 23.4 ) "Hello" Trie.empty


trie2 =
    Trie.add ( "id002", 21.4 ) "Hello" trie1


trie3 =
    Trie.add ( "id003", 28.4 ) "Hell" trie2


exampleEncodedTrie3 =
    "{\"H\":{\"e\":{\"l\":{\"l\":[{\"id003\":28.4},{\"o\":{\"id001\":23.4,\"id002\":21.4}}]}}}}"


encodeTrieTest _ =
    let
        encodedTrie =
            Encode.encode 0 (TrieEncoder.encoder Encode.float trie3)

        _ =
            Debug.log "readable2 " encodedTrie

        _ =
            Debug.log "readable2 " trie3
    in
    test "encode a trie" <|
        \() ->
            Expect.equal
                exampleEncodedTrie3
                encodedTrie


encodeEmptyTrieTest _ =
    let
        str : String
        str =
            Encode.encode 0 (TrieEncoder.encoder Encode.float EmptyTrie)
    in
    test "encode am EmptyTrie 1" <|
        \() -> Expect.equal "null" str


decodeEmptyTrieTest _ =
    let
        a =
            1

        result =
            decodeString (TrieDecoder.decoder float) "null"
    in
    test "encode am EmptyTrie 2" <|
        \() -> Expect.equal (Ok EmptyTrie) result



{-
   From http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Dict
   QUOTE: "Dictionary equality with (==) is unreliable and should not be used."

   Therefore decode then encode back to string to check its same.
-}


decodeTrieTest1 _ =
    let
        resultTrie =
            Debug.log "Moo1" <|
                decodeString (TrieDecoder.decoder float) exampleEncodedTrie3

        resultStr =
            Result.map
                (\decodedTrie ->
                    Encode.encode 0 (TrieEncoder.encoder Encode.float decodedTrie)
                )
                resultTrie
    in
    test "decode back to example3" <|
        \() -> Expect.equal (Ok exampleEncodedTrie3) resultStr
