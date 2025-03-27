module TrieTests exposing
    ( addTest2
    , emptyTrieTest1
    , expandDoesNotOutputAKeyForRemovedReference
    , expandTest1
    , getTest1
    , getTest2
    , hasTest1
    , hasTest2
    , hasTest3
    , isEmptyAfterRemovingTheOnlyRef
    , removeOnAnEmptyTrieReturnsEmpty
    , removeTest1
    )

import Expect
import Test exposing (..)
import TestHelper exposing (expectEqualListMembers)
import Trie
import TrieModel exposing (Trie(..))


emptyTrieTest1 : Test
emptyTrieTest1 =
    test "empty trie test 1" <|
        \() -> Expect.equal Trie.empty EmptyTrie


type alias MyDoc =
    { cid : String
    , title : String
    , author : String
    , body : String
    }


doc1 : MyDoc
doc1 =
    { cid = "3"
    , title = "t1"
    , author = "a1"
    , body = "b1"
    }


{-| a bit odd test.
-}
addTest2 : Test
addTest2 =
    let
        trie2a =
            Trie.add ( "refid123", doc1 ) "ab" Trie.empty

        -- _ = Debug.log("addtest2 1") (trie2a)
        tarie2b =
            Trie.add ( "refid123", doc1 ) "ac" trie2a

        -- _ = Debug.log("addtest2 2") (tarie2b)
    in
    describe "add test 2"
        [ test "a" <|
            \() ->
                trie2a
                    |> Expect.notEqual EmptyTrie
        , test "b" <|
            \() ->
                tarie2b
                    |> Expect.notEqual EmptyTrie
        , test "c" <|
            \() ->
                tarie2b
                    |> Expect.notEqual trie2a
        ]


hasTest1 : Test
hasTest1 =
    test "EmptyTrie does not have \"ab\"" <|
        \() ->
            EmptyTrie
                |> Trie.has "ab"
                |> Expect.equal False
                |> Expect.onFail "EmptyTree should not contain any token."


hasTest2 : () -> Test
hasTest2 _ =
    test "Created trie has \"ab\"" <|
        \() ->
            Trie.empty
                |> Trie.add ( "refid123", doc1 ) "ab"
                |> Trie.has "ab"
                |> Expect.equal True
                |> Expect.onFail "Trie created with token should contain it"


hasTest3 : () -> Test
hasTest3 _ =
    test "EmptyTrie does not have \"\"" <|
        \() ->
            EmptyTrie
                |> Trie.has ""
                |> Expect.equal False
                |> Expect.onFail "EmptyTree does not even contain empty string."


getTest1 : Test
getTest1 =
    describe "get test 1"
        [ test "get \"ab\" from EmptyTree is Nothing" <|
            \() ->
                EmptyTrie
                    |> Trie.getNode "ab"
                    |> Expect.equal Nothing
        , test "get \"\" from EmptyTree is Nothing" <|
            \() ->
                EmptyTrie
                    |> Trie.getNode ""
                    |> Expect.equal Nothing
        ]


getTest2 : Test
getTest2 =
    let
        trie2 =
            Trie.add ( "refid123", doc1 ) "ab" Trie.empty
    in
    describe "get test 2"
        [ test "get \"a\" from trie1 is not Nothing" <|
            \() ->
                Trie.getNode "a" trie2
                    |> Expect.notEqual Nothing
        , test "get \"ab\" from trie2 is not Nothing" <|
            \() ->
                Trie.getNode "ab" trie2
                    |> Expect.notEqual Nothing
        , test "get \"abc\" from trie3 is Nothing" <|
            \() ->
                Trie.getNode "abc" trie2
                    |> Expect.equal Nothing
        ]


expandTest1 : Test
expandTest1 =
    let
        testTrieA =
            Trie.empty
                |> Trie.add ( "refid121", 1 ) "ab"
                |> Trie.add ( "refid122", 2 ) "ac"
                |> Trie.add ( "refid123", 3 ) "acd"
                |> Trie.add ( "refid124", 4 ) "for"
                |> Trie.add ( "refid125", 5 ) "forward"
    in
    describe "expand test 1"
        [ test "expand \"a\"" <|
            \() ->
                testTrieA
                    |> Trie.expand "a"
                    |> expectEqualListMembers [ "ab", "acd", "ac" ]
        , test "expand \"ac\"" <|
            \() ->
                testTrieA
                    |> Trie.expand "ac"
                    |> expectEqualListMembers [ "acd", "ac" ]
        , test "expand \"\"" <|
            \() ->
                testTrieA
                    |> Trie.expand ""
                    |> expectEqualListMembers []
        , test "expand \"b\"" <|
            \() ->
                testTrieA
                    |> Trie.expand "b"
                    |> expectEqualListMembers []
        , test "expand \"f\"" <|
            \() ->
                testTrieA
                    |> Trie.expand "f"
                    |> expectEqualListMembers [ "for", "forward" ]
        , test "expand \"for\"" <|
            \() ->
                testTrieA
                    |> Trie.expand "for"
                    |> expectEqualListMembers [ "for", "forward" ]
        ]


removeTest1 : Test
removeTest1 =
    let
        testTrie =
            Trie.empty
                |> Trie.add ( "refid121", 1 ) "ab"
                |> Trie.add ( "refid122", 2 ) "ac"
                |> Trie.add ( "refid123", 3 ) "acd"
                |> Trie.add ( "refid124", 4 ) "for"
                |> Trie.add ( "refid125", 5 ) "forward"

        -- _ = Debug.log "removeTest1 a" (trieU5)
        -- _ = Debug.log("removeTest1 b get") (Trie.get "for" trieU5)
        -- _ = Debug.log("removeTest1 b1 rem") (Trie.remove "for" "refid125" trieU5)
        -- _ = Debug.log("removeTest1 b2 rem") (Trie.remove "for" "refid124" trieU5)
        -- _ = Debug.log("removeTest1 b3 rem") (Trie.remove "forward" "refid125" trieU5)
        -- _ = Debug.log("removeTest1 c has") (Trie.has "for" trieU5)
        -- _ = Debug.log("removeTest1 c1 has") (Trie.has "for" (Trie.remove "for" "refid125" trieU5))
        -- _ = Debug.log("removeTest1 c2 has") (Trie.has "for" (Trie.remove "for" "refid124" trieU5))
        -- _ = Debug.log("removeTest1 d rem") (Trie.remove "for" "refid124" trieU5)
    in
    describe "remove test 1"
        [ test "remove token but doc reference wrong, so does not change trie" <|
            \() ->
                testTrie
                    |> Trie.remove "for" "refid125"
                    |> Trie.has "for"
                    |> Expect.equal True
                    |> Expect.onFail "Removing token with wrong document reference does not remove it."
        , test "remove token with right doc reference 1" <|
            \() ->
                testTrie
                    |> Trie.remove "for" "refid124"
                    |> Trie.has "for"
                    |> Expect.equal False
                    |> Expect.onFail "Removing token with correct doc reference does remove it."
        , test "remove token with right doc reference 2" <|
            \() ->
                testTrie
                    |> Trie.remove "forward" "refid125"
                    |> Trie.has "forward"
                    |> Expect.equal False
                    |> Expect.onFail "Removing token with correct doc reference does remove it."
        ]


removeOnAnEmptyTrieReturnsEmpty : Test
removeOnAnEmptyTrieReturnsEmpty =
    test "remove on empty returns empty" <|
        \() ->
            Trie.empty
                |> Trie.remove "anything" "anything2"
                |> Trie.isEmpty
                |> Expect.equal True
                |> Expect.onFail "Removing from empty returns empty"


isEmptyAfterRemovingTheOnlyRef : Test
isEmptyAfterRemovingTheOnlyRef =
    test "add then remove a trie value check it reports empty" <|
        \() ->
            Trie.empty
                |> Trie.add ( "refid121", ( 1, 2 ) ) "ab"
                |> Trie.remove "ab" "refid121"
                |> Trie.isEmpty
                |> Expect.equal True
                |> Expect.onFail "Should be an empty tree."


expandDoesNotOutputAKeyForRemovedReference : Test
expandDoesNotOutputAKeyForRemovedReference =
    test "expand a removed keyword expands to empty list" <|
        \() ->
            Trie.empty
                |> Trie.add ( "refid124", 4 ) "for"
                |> Trie.remove "for" "refid124"
                |> Trie.expand "for"
                |> Expect.equal []
