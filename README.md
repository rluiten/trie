## Trie implementation

Copyright (c) 2025 Robin Luiten

An implementation of a Trie data structure.

A Trie is an ordered tree data structure that is used to store a dynamic
set or associative array where the keys are usually strings.

In this implementation the keys are strings.

## History

### 2025/03/25

Seoius bug in Trie.remove it would throw away other nodes in some cases when removing a key in the Trie.
Flaw uncovered by a bug report in elm-text-search.

### 2021/02/03

The Trie.remove cleans up left over nodes with no held values. This did not affect my usage but it surprised me so I fixed it.

Added "isEmpty" to api of Trie.

### 2015/12/27

As of Sunday Json encoder and decoder have been added to this package.

To improve Encoder and Decoder parts of data model have changed.
Have also removed exposing the Constructors of Trie, they were accidentally exposed
previously, hope this is not a big problem for anyone, it was intended to be
an opaque type.

Also updated the tests to use a more common test pattern.

## Testing

This uses elm-test for testing so install it if you don't have it.

To see tests look at TrieTest.elm and TrieCodecTests.elm in tests folder.

- npm install -g elm-test

To run Tests

- elm-test

Copyright (c) 2025 Robin Luiten
