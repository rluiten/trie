module MaybeExtra exposing (..)

{-| Some helpers for Maybe stuff.


## Functions

@docs filter

-}


{-| Keep a maybe value if it satisfies the predicate, else return Nothing.

Maybe extension.

-}
filter : (a -> Bool) -> Maybe a -> Maybe a
filter predicate =
    Maybe.andThen
        (\trie ->
            if predicate trie then
                Just trie

            else
                Nothing
        )


{-| Keep a value if it passes a predicate.
-}
filtered : (a -> Bool) -> a -> Maybe a
filtered f x =
    if f x then
        Just x

    else
        Nothing
