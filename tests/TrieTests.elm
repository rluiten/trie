module TrieTests where

import Dict
import ElmTest exposing (..)
import Set

import Trie exposing (Trie(..))


tests : Test
tests =
    suite "DocTrie tests"
      [ addTest1
      , addTest2
      , hasTest1
      , hasTest2
      , hasTest3
      , getTest1
      , getTest2
      , expandTest1
      , removeTest1
      ]


addTest1 : Test
addTest1 =
    test "add test 1"
      <| assertEqual Trie.empty EmptyTrie


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


addTest2 : Test
addTest2 =
    let
      trieU1 = Trie.add ("refid123", doc1) "ab" Trie.empty
      -- _ = Debug.log("addtest2 1") (trieU1)
      trieU2 = Trie.add ("refid123", doc1) "ac" trieU1
      -- _ = Debug.log("addtest2 2") (trieU2)
    in
      suite "add test 2"
        [ test "a"
          <| assertNotEqual EmptyTrie trieU1
        , test "b"
          <| assertNotEqual EmptyTrie trieU2
        , test "c"
          <| assertNotEqual trieU1 trieU2
        ]


hasTest1 : Test
hasTest1 =
    test "has test 1"
      <| assert
      <| not
      <| Trie.has "ab" EmptyTrie


hasTest2 : Test
hasTest2 =
    let
      trieU1 = Trie.add ("refid123", doc1) "ab" Trie.empty
    in
      test "has test 2"
        <| assert
        <| Trie.has "ab" trieU1


hasTest3 : Test
hasTest3 =
    test "has test 3"
      <| assert
      <| not
      <| Trie.has "" EmptyTrie


getTest1 : Test
getTest1 =
    let
      trie1 = Trie.getNode "ab" EmptyTrie
      trie2 = Trie.getNode "" EmptyTrie
    in
      suite "get test 1"
        [ test "a"
            <| assert
            <| trie1 == Nothing
        , test "b"
            <| assert
            <| trie2 == Nothing
        ]


getTest2 : Test
getTest2 =
    let
      trieU1 = Trie.add ("refid123", doc1) "ab" Trie.empty
      trie1 = Trie.getNode "a" trieU1
      trie2 = Trie.getNode "ab" trieU1
      trie3 = Trie.getNode "abc" trieU1
    in
      suite "get test 2"
        [ test "a"
            <| assert
            <| trie1 /= Nothing
        , test "b"
            <| assert
            <| trie2 /= Nothing
        , test "c"
            <| assert
            <| trie3 == Nothing
        ]

expandTest1 : Test
expandTest1 =
    let
        trieU1 = Trie.add ("refid121", 1) "ab" Trie.empty
        trieU2 = Trie.add ("refid122", 2) "ac" trieU1
        trieU3 = Trie.add ("refid123", 3) "acd" trieU2
        trieU4 = Trie.add ("refid124", 4) "for" trieU3
        trieU5 = Trie.add ("refid125", 5) "forward" trieU4
        tokens1 = Trie.expand "a" trieU5
        tokens2 = Trie.expand "ac" trieU5
        tokens3 = Trie.expand "" trieU5
        tokens4 = Trie.expand "b" trieU5
        tokens5 = Trie.expand "f" trieU5
        tokens6 = Trie.expand "for" trieU5
        -- _ = Debug.log("expandTest1") (tokens1,tokens2,tokens3,tokens4,tokens5,tokens6)
        setBounce list = Set.toList (Set.fromList list)
    in
      suite "expand test 1"
        [ test "expand \"a\""
            (assertEqual ["ab","acd","ac"] tokens1)
        , test "expand \"ac\""
            (assertEqual ["acd","ac"] tokens2)
        , test "expand \"\""
            (assertEqual [] tokens3)
        , test "expand \"b\""
            (assertEqual [] tokens4)
        , test "expand \"f\""
            (assertEqual (setBounce ["for","forward"]) (setBounce tokens5))
        , test "expand \"for\""
            (assertEqual (setBounce ["for","forward"]) (setBounce tokens5))
        ]


removeTest1 : Test
removeTest1 =
    let
        trieU1 = Trie.add ("refid121", 1) "ab" Trie.empty
        trieU2 = Trie.add ("refid122", 2) "ac" trieU1
        trieU3 = Trie.add ("refid123", 3) "acd" trieU2
        trieU4 = Trie.add ("refid124", 4) "for" trieU3
        trieU5 = Trie.add ("refid125", 5) "forward" trieU4

        -- _ = Debug.log("removeTest1 a") (trieU5)
        -- _ = Debug.log("removeTest1 b get") (Trie.get "for" trieU5)
        -- _ = Debug.log("removeTest1 b1 rem") (Trie.remove "for" "refid125" trieU5)
        -- _ = Debug.log("removeTest1 b2 rem") (Trie.remove "for" "refid124" trieU5)
        -- _ = Debug.log("removeTest1 b3 rem") (Trie.remove "forward" "refid125" trieU5)
        -- _ = Debug.log("removeTest1 c has") (Trie.has "for" trieU5)
        -- _ = Debug.log("removeTest1 c1 has") (Trie.has "for" (Trie.remove "for" "refid125" trieU5))
        -- _ = Debug.log("removeTest1 c2 has") (Trie.has "for" (Trie.remove "for" "refid124" trieU5))
        -- _ = Debug.log("removeTest1 d rem") (Trie.remove "for" "refid124" trieU5)
        setBounce list = Set.toList (Set.fromList list)
    in
      suite "remove test 1"
        [ test "remove token but doc reference wrong, so does not change trie"
            <| assert
            <| Trie.has "for" (Trie.remove "for" "refid125" trieU5)
        ,  test "remove token with right doc reference"
            <| assert
            <| not
            <| Trie.has "for" (Trie.remove "for" "refid124" trieU5)
        ,  test "remove token with right doc reference"
            <| assert
            <| not
            <| Trie.has "forward" (Trie.remove "forward" "refid125" trieU5)
        ]
