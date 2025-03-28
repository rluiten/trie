module TrieTests exposing
    ( aValTrieNodeTest
    , addTest2
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
    , removeTest2
    , removingRefDoesNotClearTrie
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


{-| bit larger copied from the document keys in elm-text-search case
see test removingRefDoesNotClearTrie for the boiled down problem.
-}
removeTest2 : Test
removeTest2 =
    let
        testTrie =
            Trie.empty
                |> Trie.add ( "doc1", "1" ) "exampl"
                |> Trie.add ( "doc1", "1" ) "banana"
                |> Trie.add ( "doc1", "1" ) "salli"
                |> Trie.add ( "doc1", "1" ) "appl"
                |> Trie.add ( "doc1", "1" ) "write"
                |> Trie.add ( "doc1", "1" ) "word"
                |> Trie.add ( "doc1", "1" ) "grown"
                |> Trie.add ( "doc2", "1" ) "grown"
                |> Trie.add ( "doc2", "1" ) "banana"
                |> Trie.add ( "doc2", "1" ) "appeal"
                |> Trie.add ( "doc2", "1" ) "exampl"
                |> Trie.add ( "doc2", "1" ) "appl"
                |> Trie.add ( "doc2", "1" ) "engin"

        testTrie2 =
            testTrie
                |> Trie.remove "grown" "doc2"
                |> Trie.remove "banana" "doc2"
                |> Trie.remove "appeal" "doc2"
                |> Trie.remove "exampl" "doc2"
                |> Trie.remove "appl" "doc2"
                |> Trie.remove "engin" "doc2"

        --_ = Debug.log ">>>>>>>>>>" testTrie
    in
    describe "remove test 2"
        [ test "\"exampl\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "exampl"
                    |> Expect.equal True
                    |> Expect.onFail "exampl missing"
        , test "\"banana\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "banana"
                    |> Expect.equal True
                    |> Expect.onFail "banana missing"
        , test "\"salli\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "salli"
                    |> Expect.equal True
                    |> Expect.onFail "salli missing"
        , test "\"appl\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "appl"
                    |> Expect.equal True
                    |> Expect.onFail "appl missing"
        , test "\"write\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "write"
                    |> Expect.equal True
                    |> Expect.onFail "write missing"
        , test "\"word\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "word"
                    |> Expect.equal True
                    |> Expect.onFail "word missing"
        , test "\"grown\" check keys in doc1 still there" <|
            \() ->
                testTrie2
                    |> Trie.has "grown"
                    |> Expect.equal True
                    |> Expect.onFail "grown missing"
        , test "\"appeal\" check uniq keys in doc2 gone" <|
            \() ->
                testTrie2
                    |> Trie.has "appeal"
                    |> Expect.equal False
                    |> Expect.onFail "appeal missing"
        , test "\"engin\" check uniq keys in doc2 gone" <|
            \() ->
                testTrie2
                    |> Trie.has "engin"
                    |> Expect.equal False
                    |> Expect.onFail "engin missing"
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


{-| From a bug report in elm-text-search

Reference: <https://github.com/rluiten/elm-text-search/pull/30>
Boiled down the example somewhat.
Entire trie was getting cleared by a single remove.

-}
removingRefDoesNotClearTrie : Test
removingRefDoesNotClearTrie =
    test "removing key from trie should eave behind other key" <|
        \() ->
            Trie.empty
                |> Trie.add ( "doc1", 0 ) "pl"
                |> Trie.add ( "doc2", 1 ) "pal"
                |> Trie.remove "pal" "doc2"
                |> Trie.isEmpty
                |> Expect.equal False
                |> Expect.onFail "The trie should not be empty"


{-| want a test with ValTrieNodes with a remove on them.
likely not an issue but...

This has a ValTrieNode in it.
Going to remove one word a time check other are ok

-}
aValTrieNodeTest =
    let
        testTrie =
            Trie.empty
                |> Trie.add ( "doc1", () ) "exampl"
                |> Trie.add ( "doc1", () ) "exb"
                |> Trie.add ( "doc1", () ) "exdm"
                |> Trie.add ( "doc1", () ) "ex"

        -- _ =
        --     Debug.log "aValTrieNodeTest testTrie" testTrie
    in
    describe "check removes on trie with ValTrieNode in it"
        [ test "\"exampl\" check" <|
            \() ->
                testTrie
                    |> Trie.remove "exampl" "doc1"
                    |> Trie.has "exampl"
                    |> Expect.equal False
                    |> Expect.onFail "exampl found oops"
        , test "\"exb\" check" <|
            \() ->
                testTrie
                    |> Trie.remove "exb" "doc1"
                    |> Trie.has "exb"
                    |> Expect.equal False
                    |> Expect.onFail "exb missing"
        , test "\"exdm\" check" <|
            \() ->
                testTrie
                    |> Trie.remove "exdm" "doc1"
                    |> Trie.has "exdm"
                    |> Expect.equal False
                    |> Expect.onFail "exdm missing"
        , test "\"ex\" check" <|
            \() ->
                testTrie
                    |> Trie.remove "ex" "doc1"
                    |> Trie.has "ex"
                    |> Expect.equal False
                    |> Expect.onFail "exdm missing"
        ]
