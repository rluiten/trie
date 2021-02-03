module TestHelper exposing (..)

import Expect exposing (Expectation)
import Set


expectEqualListMembers : List String -> List String -> Expectation
expectEqualListMembers l1 l2 =
    let
        normaliseListThroughSet : List comparable -> List comparable
        normaliseListThroughSet list =
            Set.toList (Set.fromList list)
    in
    Expect.equal (normaliseListThroughSet l1) (normaliseListThroughSet l2)
