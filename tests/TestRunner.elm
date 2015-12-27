module TestRunner where

import ElmTest exposing (..)
import Graphics.Element exposing (Element)
import String

import TrieTests
import TrieCodecTests


main : Element
main =
    elementRunner
      ( suite "Element Test Runner Tests"
        [ TrieTests.tests
        , TrieCodecTests.tests
        ]
      )
