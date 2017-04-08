module Tests exposing (..)

import Test exposing (..)
import TrieTests
import TrieCodecTests


all : Test
all =
    describe "TrieTests, TrieCodecTests"
        [ TrieTests.tests
        , TrieCodecTests.tests
        ]
