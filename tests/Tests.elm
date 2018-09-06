module Tests exposing (all)

import Test exposing (..)
import TrieCodecTests
import TrieTests


all : Test
all =
    describe "TrieTests, TrieCodecTests"
        [ TrieTests.tests
        , TrieCodecTests.tests
        ]
