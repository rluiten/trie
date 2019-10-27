## Trie implementation

Copyright (c) 2015 Robin Luiten

An implementation of a Trie data structure.

A Trie is an ordered tree data structure that is used to store a dynamic
set or associative array where the keys are usually strings.

In this implementation the keys are strings.

As of Sunday 2015/12/27 Json encoder and decoder have been added to this package.

To improve the Encoder and the Decoder, parts of data model have changed.
The release also removed the exposed Constructors of Trie as they were accidently exposed
previously and the Trie was intended to be
an opaque type. Hope this is not a big problem for anyone!

## Testing

This uses elm-test for testing so install it if you dont have it.

To see tests look at TrieTest.elm and TrieCodecTests.elm in tests folder.

* npm install -g elm-test

To run Tests

* elm-test

Copyright (c) 2015 Robin Luiten
